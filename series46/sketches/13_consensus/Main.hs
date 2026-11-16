-- ============================================
-- 46-13 合意と多数決 ─ ネットワークの上の意見
-- A 平均化（合意）：となりの意見との差の ε 倍だけ近づく。x ← x ＋ ε Σ(xj − x)。速さはグラフの「代数的連結度 λ2」で決まる
-- V 投票者モデル：ランダムに選ばれた人が、となりの 1 人の意見をまねる / M 多数決：となりの多数派に合わせる
-- 操作：1〜4 = グラフ（輪・スモールワールド・近い者どうし・スケールフリー）/ A・V・M = モデル / クリック = 頑固者（意見を変えない）
--       ↑↓ = ε（平均化の強さ）/ R = 意見をバラバラに / スペース = 一時停止
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftL, shiftR, xor)
import Data.List (foldl')
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)
import Numeric (showEFloat, showFFloat)

nn :: Int
nn = 60                       -- ★ 人（ノード）の数

rewire :: Float
rewire = 0.15                 -- ★ スモールワールドのつなぎ替え確率

data World = World
  { wGraph   :: !Int                       -- グラフの番号 1〜4
  , wPos     :: !(U.Vector (Float, Float)) -- 人の位置（p5.js と同じ座標）
  , wAdj     :: !(V.Vector (U.Vector Int)) -- となりのリスト
  , wEdges   :: ![(Int, Int)]
  , wV       :: !(U.Vector Double)         -- 意見 0〜1
  , wStub    :: !(U.Vector Bool)           -- 頑固者か
  , wRule    :: !Char                      -- 'A' 平均化 / 'V' 投票者 / 'M' 多数決
  , wEps     :: !Double                    -- ε の倍率（実際は ε = この値 / 最大次数）
  , wEpsNow  :: !Double
  , wLambda2 :: !Double
  , wVar0    :: !Double
  , wInit    :: !Double                    -- 最初の平均
  , wStep    :: !Int
  , wDoneAt  :: !Int
  , wHist    :: ![Double]                  -- 記録（新しい順、400 個まで）
  , wPaused  :: !Bool
  , wSeed    :: !Seed
  }

graphName :: Int -> String
graphName 1 = "ring (2 each side)"
graphName 2 = "small world (shortcuts)"
graphName 3 = "random geometric"
graphName _ = "scale-free (hubs)"

main :: IO ()
main = runSketch (sketch "46-13 Consensus and majority" (build0 1))
  { skBack  = hsb 255 30 11
  , skStart = \env -> buildGraph env 2 . build0 <$> randomRIO (1, 2 ^ (62 :: Int))
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

build0 :: Seed -> World
build0 s = World 2 U.empty V.empty [] U.empty (U.replicate nn False) 'A' 0.5 0 0 0 0 0 (-1) [] False s

-- ---------- グラフを作る ----------
-- つくりかけのグラフ：となり（IntMap）と線（つないだ順）
data GB = GB !(IM.IntMap [Int]) !(Seq.Seq (Int, Int))

addEdge :: Int -> Int -> GB -> GB
addEdge a b g@(GB adj es)
  | a == b || b `elem` IM.findWithDefault [] a adj = g
  | otherwise = GB (IM.insertWith (flip (++)) b [a] (IM.insertWith (flip (++)) a [b] adj)) (es Seq.|> (a, b))

buildGraph :: Env -> Int -> World -> World
buildGraph env gid w = computeLambda2 (resetOpinions w { wGraph = gid, wPos = pos, wAdj = adjV, wEdges = foldr (:) [] es
                                                       , wStub = U.replicate nn False, wSeed = s3 })
  where
    cx = envW env * 0.33
    cy = envH env * 0.55
    rr = min (envW env * 0.28) (envH env * 0.38)
    ringPos = U.generate nn (\i -> let a = fromIntegral i / fromIntegral nn * tau in (cx + rr * cos a, cy + rr * sin a))
    empty = GB IM.empty Seq.empty
    (g1, pos1, s1)
      | gid == 1 || gid == 2 =
          let one (g, s) (i, d) =
                let (u, sA) = rand01 s
                    (u2, sB) = rand01 sA
                    j = if gid == 2 && u < rewire then min (nn - 1) (floor (u2 * fromIntegral nn)) else (i + d) `mod` nn   -- 近道につなぎ替え
                in (addEdge i j g, if gid == 2 then sB else s)
              (g, s) = foldl' one (empty, wSeed w) [ (i, d) | i <- [0 .. nn - 1], d <- [1, 2] ]
          in (g, ringPos, s)
      | gid == 3 =
          let p = U.generate nn (\i -> (cx + (hashRand (wSeed w) (2 * i) - 0.5) * 2 * rr, cy + (hashRand (wSeed w) (2 * i + 1) - 0.5) * 2 * rr))
              g = foldl' (\acc (i, j) -> if dist2 (p U.! i) (p U.! j) < rr * 0.36 then addEdge i j acc else acc)
                         empty [ (i, j) | i <- [0 .. nn - 1], j <- [i + 1 .. nn - 1] ]
          in (g, p, nextSeed (wSeed w))
      | otherwise =
          -- バラバシ・アルバートのつながり方：線の端を 1 つ選ぶ = 次数に比例して選ぶ
          let g0 = addEdge 2 0 (addEdge 1 2 (addEdge 0 1 empty))
              one (g@(GB _ es), s) i =
                let (u, s') = rand01 s
                    r = min (2 * Seq.length es - 1) (floor (u * fromIntegral (2 * Seq.length es)))
                    (ea, eb) = Seq.index es (r `div` 2)
                in (addEdge i (if even r then ea else eb) g, s')
              (g, s) = foldl' one (g0, wSeed w) [ i | i <- [3 .. nn - 1], _ <- [0, 1 :: Int] ]
              GB adj0 _ = g
              p = springLayout cx cy rr (adjVec adj0) s
          in (g, p, nextSeed s)
    GB adjM es = connectAll pos1 g1
    pos = pos1
    adjV = adjVec adjM
    s3 = nextSeed s1

adjVec :: IM.IntMap [Int] -> V.Vector (U.Vector Int)
adjVec m = V.generate nn (\i -> U.fromList (IM.findWithDefault [] i m))

-- ばらばらの島があれば、一番近い 2 人をつないで 1 つにする
connectAll :: U.Vector (Float, Float) -> GB -> GB
connectAll pos = go (0 :: Int)
  where
    go guard g@(GB adj _)
      | guard >= nn = g
      | otherwise =
          let reach = bfs [0] (IM.singleton 0 ())
              bfs [] seen = seen
              bfs (a : q) seen =
                let new = [ b | b <- IM.findWithDefault [] a adj, not (IM.member b seen) ]
                in bfs (new ++ q) (foldl' (\m b -> IM.insert b () m) seen new)
          in case [ i | i <- [0 .. nn - 1], not (IM.member i reach) ] of
               [] -> g
               (out : _) ->
                 let best = snd (foldl' (\(bd, bi) i -> let d = dist2 (pos U.! i) (pos U.! out) in if d < bd then (d, i) else (bd, bi))
                                        (1e9, 0) (IM.keys reach))
                 in go (guard + 1) (addEdge best out g)

-- ★ ラプラシアン L = D − A の 2 番目に小さい固有値 λ2 を べき乗法で求める（合意の速さを決める）
computeLambda2 :: World -> World
computeLambda2 w = w { wLambda2 = max 0 (c - mu), wEpsNow = wEps w / fromIntegral maxDeg, wSeed = nextSeed (wSeed w) }
  where
    adj = wAdj w
    maxDeg = max 1 (V.maximum (V.cons 0 (V.map U.length adj)))
    c = 2 * fromIntegral maxDeg + 1 :: Double
    v0 = U.generate nn (\i -> realToFrac (hashRand (wSeed w) i) - 0.5)
    (mu, _) = iterate it (0, v0) !! 3000
    it (_, v) =
      let m = U.sum v / fromIntegral nn
          vc = U.map (subtract m) v                                   -- 全員同じ向き（固有値 0）を取り除く
          wv = U.generate nn (\i -> let a = adj V.! i                  -- (cI − L) v
                                        x = vc U.! i
                                    in c * x - (fromIntegral (U.length a) * x - U.sum (U.map (vc U.!) a)))
          nw = let n = sqrt (U.sum (U.map (^ (2 :: Int)) wv)) in if n == 0 then 1 else n
          nv = let n = U.sum (U.map (^ (2 :: Int)) vc) in if n == 0 then 1 else n
      in (U.sum (U.zipWith (*) wv vc) / nv, U.map (/ nw) wv)

resetOpinions :: World -> World
resetOpinions w = w { wV = v, wStep = 0, wDoneAt = -1, wHist = [], wInit = mean v, wVar0 = disagreement v, wSeed = nextSeed s }
  where
    s = wSeed w
    v = U.generate nn (\i -> let u = realToFrac (hashRand s i)
                             in if wStub w U.! i then 1 else if wRule w == 'A' then u else if u < 0.5 then 1 else 0)

mean :: U.Vector Double -> Double
mean v = U.sum v / fromIntegral nn

disagreement :: U.Vector Double -> Double
disagreement v = let m = mean v in U.sum (U.map (\x -> (x - m) * (x - m)) v) / fromIntegral nn

-- ★ 1 ステップ
updateOnce :: World -> World
updateOnce w
  | wRule w == 'A' =
      let v = wV w
          nv i = if wStub w U.! i then v U.! i
                 else let x = v U.! i in clampD (x + wEpsNow w * U.sum (U.map (\j -> v U.! j - x) (wAdj w V.! i)))
      in w { wV = U.generate nn nv, wStep = wStep w + 1 }
  | otherwise =
      let one (v, s) _ =                                           -- 1 ステップ = 全員が平均 1 回ずつ
            let (u, sA) = rand01 s
                i = min (nn - 1) (floor (u * fromIntegral nn))
                a = wAdj w V.! i
            in if wStub w U.! i || U.null a then (v, sA)
               else if wRule w == 'V'
                 then let (u2, sB) = rand01 sA                     -- となりの 1 人をまねる
                          j = a U.! min (U.length a - 1) (floor (u2 * fromIntegral (U.length a)))
                      in (v U.// [(i, v U.! j)], sB)
                 else let sm = U.sum (U.map (v U.!) a) * 2 - fromIntegral (U.length a)   -- 多数派に合わせる（同数なら変えない）
                      in (if sm > 0 then v U.// [(i, 1)] else if sm < 0 then v U.// [(i, 0)] else v, sA)
          (v', s') = foldl' one (wV w, wSeed w) [1 .. nn]
      in w { wV = v', wSeed = s', wStep = wStep w + 1 }

redShare :: World -> Double
redShare w = fromIntegral (U.length (U.filter (> 0.5) (wV w))) / fromIntegral nn

step :: World -> World
step w0 = w1 { wDoneAt = done, wHist = take 400 (h : wHist w1) }
  where
    w1 = if wPaused w0 then w0 else iterate updateOnce w0 !! (if wRule w0 == 'A' then 1 else 3)
    dis = disagreement (wV w1)
    red = redShare w1
    fin = if wRule w1 == 'A' then dis < 1e-6 else red == 0 || red == 1
    done = if wDoneAt w1 < 0 && fin then wStep w1 else wDoneAt w1
    h = if wRule w1 == 'A' then logBase 10 (max dis 1e-12) else red

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- グラフを描く
  [ Color (hsbA 0 0 100 22) (Pictures [ Line [pos U.! a, pos U.! b] | (a, b) <- wEdges w ]) ]
  ++ concat [ [ dot (hsb (wrapF 360 (lerpF 220 360 (realToFrac v))) 70 95) x y (d * 2) ]
              ++ [ Color (hsb 0 0 100) (Translate x y (ring (d + 4) 3)) | st ]
            | i <- [0 .. U.length pos - 1], let (x, y) = pos U.! i, let v = wV w U.! i, let st = wStub w U.! i
            , let d = 4 + 1.6 * sqrt (fromIntegral (U.length (wAdj w V.! i))) ]
  -- ---- 右：数字とグラフ
  ++ [ Color (hsbA 0 0 0 35) (rect (gx + gw / 2) (gy + gh / 2) gw gh) ]
  ++ plot
  ++ [ txt gx (gy + gh + 40 + fromIntegral i * 26) 15 (hsb 0 0 100) s | (i, s) <- zip [0 :: Int ..] info ]
  ++ [ txt gx (gy + gh + 40 + 6 * 26 + 10) 14 (hsb 0 0 65) "A stubborn node (white ring) breaks"
     , txt gx (gy + gh + 40 + 6 * 26 + 30) 14 (hsb 0 0 65) "'mean is conserved': all pulled to it"
     , txt 20 36 18 (hsb 0 0 100) ("1-4: graph   A: averaging   V: voter   M: majority   Click: stubborn   Up/Down: eps   R: restart   Space: "
                                   ++ (if wPaused w then "paused" else "running"))
     , txt 20 60 14 (hsb 0 0 70) "Colour = opinion (blue 0 to red 1). Dot size = number of links (degree)" ]
  where
    pos = wPos w
    gx = envW env * 0.66
    gw = envW env - gx - 30
    gy = 120
    gh = 160
    hist = reverse (wHist w)
    dis = disagreement (wV w)
    red = redShare w
    maxDeg = V.maximum (V.cons 0 (V.map U.length (wAdj w)))
    plot
      | wRule w == 'A' =
          -- 理論：ばらつきは毎ステップ (1 − ε λ2)² 倍ずつ小さくなる（一番遅いモード）
          let rate = 2 * logBase 10 (max (abs (1 - wEpsNow w * wLambda2 w)) 1e-12)
              yv v = gy + gh - clampF 0 1 ((realToFrac v + 12) / 12) * gh
              start = fromIntegral (wStep w - length hist) :: Double
              l0 = logBase 10 (max (wVar0 w) 1e-30)
          in [ seg 1.5 (hsb 180 60 100) (gx, yv (l0 + rate * max start 0)) (gx + gw, yv (l0 + rate * (start + 400)))
             , path 2 (hsb 330 70 100) [ (gx + fromIntegral i / 399 * gw, yv v) | (i, v) <- zip [0 :: Int ..] hist ]
             , txt gx (gy - 8) 14 (hsb 0 0 85) "log10 spread (pink = measured, cyan = theory)"
             , txt gx (gy + gh + 16) 14 (hsb 180 40 90) "theory: x (1 - eps l2)^2 per step" ]
      | otherwise =
          [ seg 1 (hsbA 0 0 100 30) (gx, gy + gh / 2) (gx + gw, gy + gh / 2)
          , path 2 (hsb 0 80 100) [ (gx + fromIntegral i / 399 * gw, gy + gh - realToFrac v * gh) | (i, v) <- zip [0 :: Int ..] hist ]
          , txt gx (gy - 8) 14 (hsb 0 0 85) "Share of red opinions" ]
    info =
      [ "Graph " ++ show (wGraph w) ++ ": " ++ graphName (wGraph w)
      , show nn ++ " people, " ++ show (length (wEdges w)) ++ " links, max deg " ++ show maxDeg
      , "l2 = " ++ fmtD 4 (wLambda2 w) ++ " (bigger = faster)"
      , "Model: " ++ (case wRule w of
                        'A' -> "averaging (eps = " ++ fmtD 3 (wEpsNow w) ++ ")"
                        'V' -> "voter model"
                        _ -> "majority rule")
      , if wRule w == 'A' then "spread " ++ showEFloat (Just 2) dis "" ++ " (goal: mean " ++ fmtD 3 (wInit w) ++ ")"
        else "red " ++ fmtD 0 (red * 100) ++ " %, blue " ++ fmtD 0 ((1 - red) * 100) ++ " %"
      , "Step " ++ show (wStep w) ++ (if wDoneAt w >= 0 then "  -> " ++ (if wRule w == 'A' then "agreed" else "unanimous")
                                                           ++ " at " ++ show (wDoneAt w) else "") ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let m = toP5 env p
    in case [ i | i <- [0 .. U.length (wPos w) - 1], dist2 (wPos w U.! i) m < 14 ] of
         (i : _) -> let st = not (wStub w U.! i)
                    in w { wStub = wStub w U.// [(i, st)], wV = if st then wV w U.// [(i, 1)] else wV w }
         [] -> w
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> buildGraph env (fromEnum c - fromEnum '0') w
    | c `elem` "aA" -> resetOpinions w { wRule = 'A' }
    | c `elem` "vV" -> resetOpinions w { wRule = 'V' }
    | c `elem` "mM" -> resetOpinions w { wRule = 'M' }
    | c `elem` "rR" -> resetOpinions w
  EventKey (SpecialKey KeyUp) Down _ _ -> computeLambda2 w { wEps = min 0.9 (wEps w + 0.1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> computeLambda2 w { wEps = max 0.1 (wEps w - 0.1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. A（平均化）で 1（輪）と 2（スモールワールド）を比べる。近道が少しあるだけで λ2 が何倍になり、合意は何倍速くなる？
-- 2. 合意したときの値は「最初の平均」と同じ？ 頑固者を 1 人クリックしてから R すると、合意値はどうなる？
-- 3. V（投票者）では、全員一致までに何ステップかかる？ 4（スケールフリー）だと人気者の意見が広まりやすい？
-- 4. M（多数決）で 1（輪）にすると、色のかたまりが残って止まる。多数決は「局所的な合意」で固まりやすい
-- 5. 投票者モデルで頑固者を 1 人置くと、いつかは全員がその意見になる。少数派でも折れない人の力

-- ---------- 道具 ----------
-- グラフをばねで並べる：みんな反発、つながりはばね、中心へ少し引く
springLayout :: Float -> Float -> Float -> V.Vector (U.Vector Int) -> Seed -> U.Vector (Float, Float)
springLayout cx cy rr adj s = iterate it p0 !! 300
  where
    p0 = U.generate nn (\i -> (cx + (hashRand s (2 * i) - 0.5) * rr, cy + (hashRand s (2 * i + 1) - 0.5) * rr))
    it p = U.generate nn $ \i ->
      let (xi, yi) = p U.! i
          rep (ax, ay) j
            | j == i = (ax, ay)
            | otherwise = let (xj, yj) = p U.! j
                              dx = xi - xj
                              dy = yi - yj
                              d2 = dx * dx + dy * dy + 1
                              f = 3000 / d2 / sqrt d2
                          in (ax + dx * f, ay + dy * f)
          (rx, ry) = foldl' rep (0, 0) [0 .. nn - 1]
          (sx, sy) = U.foldl' (\(ax, ay) j -> let (xj, yj) = p U.! j in (ax + (xj - xi) * 0.02, ay + (yj - yi) * 0.02)) (0, 0) (adj V.! i)
          fx = rx + sx + (cx - xi) * 0.01
          fy = ry + sy + (cy - yi) * 0.01
      in (clampF (cx - rr) (cx + rr) (xi + clampF (-10) 10 fx), clampF (cy - rr) (cy + rr) (yi + clampF (-10) 10 fy))

clampD :: Double -> Double
clampD = max 0 . min 1

-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- gloss の座標（中央が原点、y は上向き）を p5.js の座標に直す
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = if w <= 1 then Color c (Line ps) else Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmtD :: Int -> Double -> String
fmtD n v = showFFloat (Just n) v ""

-- 乱数（xorshift64）。種を World に入れておけば skStep は純粋なまま
type Seed = Word64

nextSeed :: Seed -> Seed
nextSeed s0 = let s1 = s0 `xor` (s0 `shiftL` 13)
                  s2 = s1 `xor` (s1 `shiftR` 7)
              in s2 `xor` (s2 `shiftL` 17)

-- 0〜1 の乱数と次の種
rand01 :: Seed -> (Float, Seed)
rand01 s = let s' = nextSeed s in (fromIntegral (s' `shiftR` 40) / 16777216, s')

-- 種 s と番号 i から決まる 0〜1 の乱数（たくさんの人に一度に配るとき用）
hashRand :: Seed -> Int -> Float
hashRand s i =
  let z0 = s + fromIntegral i * 0x9E3779B97F4A7C15
      z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
      z3 = z2 `xor` (z2 `shiftR` 31)
  in fromIntegral (z3 `shiftR` 40) / 16777216
