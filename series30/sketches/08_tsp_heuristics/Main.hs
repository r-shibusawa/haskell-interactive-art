-- ============================================
-- 08 巡回セールスマン問題 ─ ヒューリスティクス
-- 全部の町を 1 回ずつ回って戻る最短の道は？ 町が 20 で約 6 京通り。全部は試せないので「そこそこ良い道」を速くつくる
--   つくる：最近傍法（一番近い町へ）→ 直す：2-opt（交差する 2 本をつなぎかえる）、Or-opt（1〜3 個の町を別の場所へ移す）
-- クリック = 町を足す / N = 最近傍法 / 2 = 2-opt / O = Or-opt / スペース = 全部を順に / X = でたらめな順 / ↑↓ = 町の数 / R = 新しい町 / C = 全部消す
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Numeric (showFFloat)
import Data.Ord (comparing)
import Data.List (sortBy)
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U

data Mode = Idle | NN | TwoOpt | OrOpt deriving Eq

data World = World
  { wNCities :: !Int                   -- ★ 町の数
  , wCities :: ![(Float, Float)]
  , wCount :: !Int                     -- 今の町の数（クリックで増える）
  , wDM :: !(U.Vector Double)          -- 町どうしの距離の表（町の数が変わったら作り直す）
  , wTour :: !(U.Vector Int)
  , wVisited :: !(U.Vector Bool)
  , wMode :: !Mode
  , wChain :: !Bool                    -- スペース：NN → 2-opt → Or-opt を続けて
  , wMoves :: !Int
  , wLenLog :: !(Seq.Seq (Double, Mode))   -- 道の長さの記録（600 個まで）
  }

mapX, mapY, mapW, mapH :: Float
mapX = 30
mapY = 100
mapW = 1280 * 0.62
mapH = 800 - 130

main :: IO ()
main = runSketch (sketch "30-08 TSP Heuristics" (error "skStart makes the world"))
  { skBack  = hsb 225 30 12
  , skStart = \_ -> newCities 60
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

newCities :: Int -> IO World
newCities n = do
  cs <- replicateM n ((\a b -> (mapX + a, mapY + b)) <$> randomRIO (20, mapW - 20) <*> randomRIO (20, mapH - 20))
  pure (resetTour (World n cs n (distTable cs) U.empty U.empty Idle False 0 Seq.empty))

resetTour :: World -> World
resetTour w = logLen w { wTour = U.fromList [0 .. wCount w - 1], wMode = Idle, wChain = False, wLenLog = Seq.empty, wMoves = 0 }

distTable :: [(Float, Float)] -> U.Vector Double
distTable cs = U.fromList
  [ sqrt ((realToFrac a - realToFrac c) ^ (2 :: Int) + (realToFrac b - realToFrac d) ^ (2 :: Int)) | (a, b) <- cs, (c, d) <- cs ]

-- 町どうしの距離
dist :: World -> Int -> Int -> Double
dist w a b = U.unsafeIndex (wDM w) (a * wCount w + b)
{-# INLINE dist #-}

tourLen :: World -> U.Vector Int -> Double
tourLen w t
  | U.null t = 0
  | otherwise = U.sum (U.zipWith (dist w) t (U.snoc (U.drop 1 t) (U.head t)))

logLen :: World -> World
logLen w =
  let l = wLenLog w Seq.|> (tourLen w (wTour w), wMode w)
  in w { wLenLog = if Seq.length l > 600 then Seq.drop 1 l else l }

-- ★ 最近傍法：今いる町から、まだ行っていない一番近い町へ
startNN :: Int -> World -> World
startNN s w = w { wTour = U.singleton s, wVisited = U.generate (wCount w) (== s), wMode = NN, wMoves = 0 }

-- 戻り値の Bool は「まだ続く」
stepNN :: World -> (World, Bool)
stepNN w =
  let n = wCount w
      cur = U.last (wTour w)
      cands = [(dist w cur j, j) | j <- [0 .. n - 1], not (U.unsafeIndex (wVisited w) j)]
  in case cands of
       [] -> (w, False)
       _ ->
         let (_, best) = minimum cands
             t = U.snoc (wTour w) best
         in (w { wTour = t, wVisited = wVisited w U.// [(best, True)] }, U.length t < n)

-- ★ 2-opt：辺 (a,b) と (c,e) を (a,c) と (b,e) につなぎかえて短くなるなら、間を逆向きにする
stepTwoOpt :: World -> Maybe (U.Vector Int)
stepTwoOpt w = goI 0
  where
    t = wTour w
    n = U.length t
    at' = U.unsafeIndex t
    goI !i
      | i >= n - 1 = Nothing
      | otherwise = goJ i (i + 2)
    goJ !i !j
      | j >= n = goI (i + 1)
      | i == 0 && j == n - 1 = goJ i (j + 1)
      | otherwise =
          let a = at' i
              b = at' (i + 1)
              c = at' j
              e = at' ((j + 1) `mod` n)
          in if dist w a c + dist w b e < dist w a b + dist w c e - 1e-9
               then Just (U.concat [U.take (i + 1) t, U.reverse (U.slice (i + 1) (j - i) t), U.drop (j + 1) t])
               else goJ i (j + 1)

-- ★ Or-opt：連続する 1〜3 個の町をぬき出して、別の辺の間に（向きも選んで）入れる
stepOrOpt :: World -> Maybe (U.Vector Int)
stepOrOpt w = goL 1
  where
    t = wTour w
    n = U.length t
    at' = U.unsafeIndex t
    goL :: Int -> Maybe (U.Vector Int)
    goL !l
      | l > 3 = Nothing
      | otherwise = goI l 0
    goI !l !i
      | i + l > n = goL (l + 1)
      | prev == sL || next == s0 = goI l (i + 1)
      | otherwise = goK l i s0 sL gain 0
      where
        s0 = at' i
        sL = at' (i + l - 1)
        prev = at' ((i - 1 + n) `mod` n)
        next = at' ((i + l) `mod` n)
        gain = dist w prev s0 + dist w sL next - dist w prev next
    goK !l !i !s0 !sL !gain !k
      | k >= n = goI l (i + 1)
      | k >= i - 1 && k <= i + l - 1 = goK l i s0 sL gain (k + 1)
      | i == 0 && k == n - 1 = goK l i s0 sL gain (k + 1)
      | otherwise =
          let p = at' k
              q = at' ((k + 1) `mod` n)
              add1 = dist w p s0 + dist w sL q - dist w p q
              add2 = dist w p sL + dist w s0 q - dist w p q
          in if min add1 add2 < gain - 1e-9
               then
                 let seg0 = U.slice i l t
                     sg = if add2 < add1 then U.reverse seg0 else seg0
                     rest = U.take i t U.++ U.drop (i + l) t
                     pos = maybe 0 (+ 1) (U.elemIndex p rest)
                 in Just (U.concat [U.take pos rest, sg, U.drop pos rest])
               else goK l i s0 sL gain (k + 1)

-- 1 フレーム分進める
stepWorld :: World -> World
stepWorld w = case wMode w of
  NN ->
    let (w1, more1) = stepNN w
        (w2, more2) = if more1 then stepNN w1 else (w1, False)
    in logLen (if more2 then w2 else w2 { wMode = if wChain w2 then TwoOpt else Idle })
  TwoOpt -> logLen $ case stepTwoOpt w of
    Just t -> w { wTour = t, wMoves = wMoves w + 1 }
    Nothing -> w { wMode = if wChain w then OrOpt else Idle }
  OrOpt -> logLen $ case stepOrOpt w of
    Just t -> w { wTour = t, wMoves = wMoves w + 1 }
    Nothing -> w { wMode = Idle, wChain = False }
  Idle -> w

modeColor :: Mode -> Color
modeColor m = case m of
  NN -> hsb 30 80 100
  TwoOpt -> hsb 190 80 100
  OrOpt -> hsb 300 60 100
  Idle -> hsb 50 80 100

drawAll :: Env -> World -> Picture
drawAll env w@World{..} = screen env $ Pictures [mapPic, graph, info, hud]
  where
    wd = envW env
    ht = envH env
    cityV = U.fromList wCities
    pos i = cityV U.! i
    closed = wMode /= NN
    tourPts = map pos (U.toList wTour)

    -- 地図
    mapPic = Pictures $
      [ boxL 1 (hsba 0 0 100 0.15) mapX mapY mapW mapH
      , if U.length wTour >= 2
          then (if closed then loopPath else path) 2.5 (modeColor wMode) tourPts
          else Blank
      ]
      ++ [dot white x y 9 | (x, y) <- wCities]
      ++ [ case tourPts of
             (x, y) : _ -> dot (hsb 0 80 100) x y 14
             [] -> Blank ]

    -- 右：長さのグラフ
    gx = mapX + mapW + 40
    gw = wd - gx - 30
    gy = mapY + 30
    gh = ht * 0.38
    logL = toList wLenLog
    closedLens = [l | (l, m) <- logL, m /= NN]
    (hi0, lo0) = case closedLens of
      [] -> (case reverse logL of
               (l, _) : _ -> l
               [] -> 0, 0)
      ls -> (maximum ls, minimum ls)
    lo = lo0 * 0.95
    nLog = length logL
    graph = Pictures $
      txt gx (gy - 12) 15 (gr 85) "Tour length over time"
      : if nLog <= 1 then [] else
        [ dot (modeColor m) (gx + fromIntegral k / fromIntegral (nLog - 1) * gw)
            (gy + gh - clampF 0 1 (fl ((l - lo) / max 1 (hi0 - lo))) * gh) 4
        | (k, (l, m)) <- zip [0 :: Int ..] logL, m /= NN ]

    ty = gy + gh + 50
    stage = case wMode of
      Idle -> "stopped"
      NN -> "nearest neighbor (orange)"
      TwoOpt -> "2-opt repair (cyan)"
      OrOpt -> "Or-opt repair (purple)"
    infoLines =
      [ "Cities " ++ show wCount ++ "  Moves " ++ show wMoves
      , "Stage: " ++ stage
      , ""
      , "NN: fast, but leaves a long return edge"
      , "2-opt: uncrossing 2 edges always shortens"
      , "  so a 2-opt tour has no crossings"
      , "Or-opt: moves detours 2-opt can't fix"
      , ""
      , "Even a random order (X) + 2-opt gets good,"
      , "but not optimal (a local optimum)"
      ]
    info = Pictures $
      txt gx ty 24 white (if closed then "Length " ++ nf 0 (tourLen w wTour) else "Building...")
      : [txt gx (ty + 30 + i * 22) 14 (gr 75) s | (i, s) <- zip [0 ..] infoLines]

    hud = Pictures
      [ txt 20 40 16 white ("Click: add  N: nearest  2: 2-opt  O: Or-opt  Space: all  X: random  U/D cities " ++ show wNCities ++ "  R: new  C: clear")
      , txt 20 68 14 (gr 60) "Build, then repair: the basis of heuristics. The red city is the start."
      ]

-- キーとマウス
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
    in if mx > mapX && mx < mapX + mapW && my > mapY && my < mapY + mapH
         then pure (addCity (mx, my) w)
         else pure w
  EventKey (SpecialKey KeyUp) Down _ _ -> newCities (min 300 (wNCities w + 20))
  EventKey (SpecialKey KeyDown) Down _ _ -> newCities (max 10 (wNCities w - 20))
  EventKey (Char 'r') Down _ _ -> newCities (wNCities w)
  EventKey (Char 'R') Down _ _ -> newCities (wNCities w)
  _ | wCount w < 4 -> pure w
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    w1 <- beginNN w
    pure w1 { wChain = True }
  EventKey (Char c) Down _ _ -> case toLower c of
    'n' -> do
      w1 <- beginNN w
      pure w1 { wChain = False }
    '2' -> pure (if wMode w == NN then w else w { wChain = False, wMode = TwoOpt })
    'o' -> pure (if wMode w == NN then w else w { wChain = False, wMode = OrOpt })
    'x' -> do
      s <- shuffleIO [0 .. wCount w - 1]
      pure (logLen w { wTour = U.fromList s, wMode = Idle, wChain = False, wMoves = 0 })
    'c' -> pure w { wCities = [], wCount = 0, wDM = U.empty, wTour = U.empty, wMode = Idle, wLenLog = Seq.empty }
    _ -> pure w
  _ -> pure w

beginNN :: World -> IO World
beginNN w = do
  s <- randomRIO (0, wCount w - 1)
  pure (startNN s w)

addCity :: (Float, Float) -> World -> World
addCity p w =
  let cs = wCities w ++ [p]
      w1 = w { wCities = cs, wCount = length cs, wDM = distTable cs }
  in if wMode w == NN
       then resetTour w1
       else logLen w1 { wTour = U.snoc (wTour w) (length cs - 1) }

-- ---------- やってみよう ----------
-- 1. 同じ町で「最近傍法 → 2-opt」と「でたらめ（X）→ 2-opt」の最後の長さを比べよう。どちらが良い？ 何回改善した？
-- 2. 町を円の形に並べてクリックしてみよう。最適な道は明らか。ヒューリスティクスは見つけられる？
-- 3. 最近傍法のスタートを変えると結果が変わる。全部の町をスタートにして一番良いものを選ぶ（反復最近傍法）を書こう
-- 4. 2-opt が止まった道は「局所最適」。そこから抜け出すには？（→ 09 焼きなまし法）


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標で描くための変換
screen :: Env -> Picture -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2)

-- マウスの位置（左上が原点、y は下向き）
mouseP :: Env -> Point
mouseP = toP5 <*> envMouse

-- gloss の座標 → 左上が原点の座標
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字：(x, y) は左端のベースライン
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label x (negate y)

-- 枠だけの長方形（線の太さ t）
boxL :: Float -> Color -> Float -> Float -> Float -> Float -> Picture
boxL t c x y w h = loopPath t c [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- 折れ線（太さ t）
path :: Float -> Color -> [Point] -> Picture
path t c ps = Color c (thickPath t [(x, negate y) | (x, y) <- ps])

-- 閉じた折れ線（太さ t）
loopPath :: Float -> Color -> [Point] -> Picture
loopPath t c ps = path t c (ps ++ take 1 ps)

-- 塗りつぶした円：中心 (x, y)、直径 d
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (at (x, negate y) (disc (d / 2)))

-- 灰色（明るさ 0〜100）
gr :: Float -> Color
gr = hsb 0 0

-- 小数点以下 d 桁の文字列（p5.js の nf）
nf :: RealFloat a => Int -> a -> String
nf d x
  | d <= 0 = show (round x :: Int)
  | otherwise = showFFloat (Just d) x ""

-- Double → Float（計算は Double、描くのは Float）
fl :: Double -> Float
fl = realToFrac

-- 並びをランダムに混ぜる（p5.js の shuffle）
shuffleIO :: [a] -> IO [a]
shuffleIO xs = do
  keys <- mapM (const (randomRIO (0, 1 :: Float))) xs
  pure (map snd (sortBy (comparing fst) (zip keys xs)))

