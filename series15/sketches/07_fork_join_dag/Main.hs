-- ============================================
-- 07 フォーク・ジョイン並列と計算 DAG（並列フィボナッチ）
-- 並列フィボナッチ fib(n)：fib(n−1) を spawn（別の人に任せる）しながら、自分は fib(n−2) を計算し、sync で待ち合わせる
-- 丸 = ストランド（1 単位時間の仕事）、矢印 = 「これが終わってから」。ワーク T₁ = 丸の数、スパン T∞ = いちばん長い道
-- 欲ばりスケジューラ：毎ステップ、準備のできた丸を P 個まで実行。T_P ≤ T₁/P + T∞（ブレントの定理）
-- スペース = スケジュール再生・停止 / 上下 = n / 左右 = プロセッサ数 P / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Array (Array, accumArray, (!))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

data DepKind = Spawn | Cont | Call | Ret deriving Eq

data World = World
  { wNum :: !Int
  , wProcs :: !Int
  , wWork :: !Int
  , wSpan :: !Int
  , wCritical :: !IS.IntSet
  , wDeps :: [(Int, Int, DepKind)]
  , wPx :: !(U.Vector Float)
  , wPy :: !(U.Vector Float)
  , wPreds :: Array Int [Int]
  , wDoneAt :: !(U.Vector Int)
  , wProc :: !(U.Vector Int)
  , wTick :: !Int
  , wRunning :: !Bool
  , wTimer :: !Int
  , wBusy :: [Int]            -- ステップごとの稼働数
  }

main :: IO ()
main = runSketch (sketch "15-07 Fork-Join DAG" (build 1280 800 6 3 0))
  { skBack  = hsb 200 35 9
  , skStart = \env -> pure (build (envW env) (envH env) 6 3 0)   -- ★ 最初の n と P
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

-- DAG を作る途中経過：ストランドの横位置と矢印の列
data B = B { bXs :: !(IM.IntMap Float), bNext :: !Int, bCounter :: !Float, bDeps :: [(Int, Int, DepKind)] }

-- 各呼び出しは 3 つのストランド：A（spawn まで）、B（呼び出しまで）、C（sync の後）
recFib :: Int -> B -> ((Int, Int), B)
recFib k b0
  | k <= 1 =
      let i = bNext b0
      in ((i, i), b0 { bXs = IM.insert i (bCounter b0) (bXs b0), bNext = i + 1, bCounter = bCounter b0 + 1 })
  | otherwise =
      let a = bNext b0
          ((f1, l1), b1) = recFib (k - 1) b0 { bNext = a + 1 }
          b = bNext b1
          ((f2, l2), b2) = recFib (k - 2) b1 { bNext = b + 1 }
          c = bNext b2
          x i = IM.findWithDefault 0 i (bXs b2)
          xs' = IM.insert a (x f1 - 0.6) (IM.insert b ((x l1 + x f2) / 2) (IM.insert c (x l2 + 0.6) (bXs b2)))
          deps' = (l2, c, Ret) : (l1, c, Ret) : (b, f2, Call) : (a, b, Cont) : (a, f1, Spawn) : bDeps b2
      in ((a, c), b2 { bXs = xs', bNext = c + 1, bDeps = deps' })

-- 並列フィボナッチ fib(n) の DAG をつくり、ワーク・スパン・位置を計算する
build :: Float -> Float -> Int -> Int -> Int -> World
build width height n procs timer =
  let (_, bf) = recFib n (B IM.empty 0 0 [])
      deps = reverse (bDeps bf)
      work = bNext bf
      xsL = IM.elems (bXs bf)
      minX = foldr min 1e9 xsL
      maxX = foldr max (-1e9) xsL
      px = U.generate work (\i -> remap (IM.findWithDefault 0 i (bXs bf)) minX (if maxX == 0 then 1 else maxX) 60 (width - 60))
      preds = accumArray (flip (:)) [] (0, work - 1) [(b, a) | (a, b, _) <- reverse deps] :: Array Int [Int]
      succs = accumArray (flip (:)) [] (0, work - 1) [(a, b) | (a, b, _) <- reverse deps] :: Array Int [Int]
      -- トポロジカル順（依存の順）に並べる
      indeg0 = IM.fromList [(i, length (preds ! i)) | i <- [0 .. work - 1]]
      kahn [] _ acc = reverse acc
      kahn (v : q) deg acc =
        let (deg', new) = foldl' dec (deg, []) (succs ! v)
            dec (d, ns) y =
              let c = IM.findWithDefault 0 y d - 1
              in (IM.insert y c d, if c == 0 then ns ++ [y] else ns)
        in kahn (q ++ new) deg' (v : acc)
      topo = kahn [i | i <- [0 .. work - 1], null (preds ! i)] indeg0 []
      -- スパン：最長路
      relax (lg, fr) v = foldl' (relaxOne v) (lg, fr) (preds ! v)
      relaxOne v (lg, fr) a =
        let la = IM.findWithDefault 1 a lg + 1
        in if la > IM.findWithDefault 1 v lg then (IM.insert v la lg, IM.insert v a fr) else (lg, fr)
      (longest, from) = foldl' relax (IM.empty, IM.empty) topo
      lg i = IM.findWithDefault 1 i longest
      spanV = foldr (max . lg) 1 [0 .. work - 1]
      lastCrit = case [i | i <- [0 .. work - 1], lg i == spanV] of
        (i : _) -> i
        [] -> 0
      walk v acc = case IM.lookup v from of
        Just a -> walk a (IS.insert a acc)
        Nothing -> acc
      critical = walk lastCrit (IS.singleton lastCrit)
      -- 時間（層）で縦の位置：最長路の長さ
      py = U.generate work (\i -> remap (fromIntegral (lg i)) 1 (fromIntegral spanV) 120 (height - 170))
  in resetSchedule World
       { wNum = n, wProcs = procs, wWork = work, wSpan = spanV, wCritical = critical
       , wDeps = deps, wPx = px, wPy = py, wPreds = preds
       , wDoneAt = U.empty, wProc = U.replicate work 0
       , wTick = 0, wRunning = False, wTimer = timer, wBusy = [] }

resetSchedule :: World -> World
resetSchedule w = w { wDoneAt = U.replicate (wWork w) (-1), wTick = 0, wRunning = False, wBusy = [] }

allDone :: World -> Bool
allDone w = U.all (> 0) (wDoneAt w)

-- 欲ばりスケジューラ：準備のできた丸を P 個まで実行する
scheduleTick :: World -> World
scheduleTick w =
  let done = wDoneAt w
      tick = wTick w
      isReady i = done U.! i < 0 && all (\a -> done U.! a >= 0 && done U.! a < tick + 1) (wPreds w ! i)
      ready = filter isReady [0 .. wWork w - 1]
      run = take (wProcs w) ready      -- ★ ここの選び方をランダムにしても上限は守られる
  in if null ready
       then w { wRunning = False }
       else w { wTick = tick + 1
              , wDoneAt = done U.// [(i, tick + 1) | i <- run]
              , wProc = wProc w U.// zip run [0 ..]
              , wBusy = wBusy w ++ [length run] }

stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wRunning w =
      let t = wTimer w + 1
          w' = w { wTimer = t }
      in pure (if t `mod` 12 == 0 then scheduleTick w' else w')
  | otherwise = pure w

drawW :: Env -> World -> Picture
drawW env w =
  let work = wWork w
      spanV = wSpan w
      procs = wProcs w
      h = envH env
      r = clampF 6 22 (260 / sqrt (fromIntegral work))
      posOf i = (wPx w U.! i, wPy w U.! i)
      crit i = IS.member i (wCritical w)
      depPic (a, b, kind) =
        let isCrit = crit a && crit b
            c | isCrit = hsb 0 75 100
              | kind == Spawn = hsb 140 50 70
              | kind == Ret = hsb 200 20 45
              | otherwise = hsb 45 50 70
        in seg env c (if isCrit then 3 else 1.2) (posOf a) (posOf b)
      strandPic i =
        let c | wDoneAt w U.! i > 0 = hsb (fromIntegral (wProc w U.! i) * 360 / fromIntegral procs) 70 95
              | crit i = hsb 0 40 80
              | otherwise = hsb 200 10 75
        in dot env c (posOf i) r
      y = h - 120
      ceilDiv a b = (a + b - 1) `div` b
      lower = max (ceilDiv work procs) spanV
      upper = ceilDiv work procs + spanV
      parallelism = fromIntegral work / fromIntegral spanV :: Float
      busy = wBusy w
      complete = length (filter (== procs) busy)
      gx' = envW env - 330
      gy' = 96
      gw = 300
      gh = 70
      bw = gw / fromIntegral (max 1 (length busy))
      barPic (k, b) =
        let bh = fromIntegral b / fromIntegral procs * gh
        in box env (hsb 140 60 90) (gx' + fromIntegral k * bw) (gy' + gh - bh) (max 1 (bw - 1)) bh
  in Pictures
       [ Pictures (map depPic (wDeps w))
       , Pictures (map strandPic [0 .. work - 1])
       , txt env 20 36 18 (lum 100)
           ("Space: schedule " ++ (if wRunning w then "stop" else "play") ++ "  Up/Down: n = " ++ show (wNum w)
             ++ "  Left/Right: P = " ++ show procs ++ "  R: restart")
       , txt env 20 62 15 (lum 65) "Green = spawn   Yellow = continue, call   Gray = return -> sync   Red = longest path (span)"
       , txt env 20 y 19 (lum 100)
           ("Work T1 = " ++ show work ++ "   Span Tinf = " ++ show spanV ++ "   Parallelism T1/Tinf = " ++ f1 parallelism)
       , txt env 20 (y + 32) 19 (hsb 45 80 100)
           ("P = " ++ show procs ++ ":  lower max(T1/P, Tinf) = " ++ show lower ++ "  <=  T_P = "
             ++ show (wTick w) ++ (if allDone w then "" else "...") ++ "  <=  upper T1/P + Tinf = " ++ show upper)
       , txt env 20 (y + 62) 15 (lum 75)
           ("Steps with everyone busy (complete) " ++ show complete ++ "   Steps with idle workers (incomplete) "
             ++ show (length busy - complete) ++ "  -> complete <= T1/P, incomplete <= Tinf")
       , box env (hsb 200 30 15) gx' gy' gw gh
       , Pictures (map barPic (zip [0 :: Int ..] busy))
       , txt env gx' (gy' + gh + 20) 14 (lum 70) "Busy workers per step (top = P)"
       ]

f1 :: Float -> String
f1 = printf "%.1f"

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ ->
    let w1 = if allDone w then resetSchedule w else w
    in w1 { wRunning = not (wRunning w1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild (min 10 (wNum w + 1))
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild (max 2 (wNum w - 1))
  EventKey (SpecialKey KeyRight) Down _ _ -> resetSchedule w { wProcs = min 16 (wProcs w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetSchedule w { wProcs = max 1 (wProcs w - 1) }
  EventKey (Char c) Down _ _ | c `elem` "rR" -> resetSchedule w
  _ -> w
  where rebuild n = build (envW env) (envH env) n (wProcs w) (wTimer w)

-- ---------- やってみよう ----------
-- 1. P = 1 なら T_P = T₁。P を増やすと、どこで速くならなくなる？（並列度 T₁/T∞ あたり）
-- 2. n を 1 増やすとワークは約 1.6 倍、スパンは +2 しか増えない → 大きい n ほど並列向き
-- 3. scheduleTick の take の選び方をランダムにしても、ブレントの上限は必ず守られる

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 中心 (x, y)、直径 d の円（p5.js の circle）
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

-- 文字：txt は左下が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = label (gx env x - fromIntegral (length str) * s * 0.72) (gy env y) s c str

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0
