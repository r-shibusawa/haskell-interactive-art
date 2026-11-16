-- ============================================
-- 46-05 粒子群最適化（PSO）─ 等高線の谷底を群れでさがす
-- 粒子はそれぞれ「いまの勢い（慣性）」「自分が見つけた一番いい場所 p」「群れ全体の一番いい場所 g」に引かれて動く
-- v ← w v ＋ c1 r1 (p − x) ＋ c2 r2 (g − x)、x ← x ＋ v。関数の値が小さいほどよい（最小値はどれも 0）
-- 操作：1〜4 = 関数（球・Rastrigin・Ackley・Himmelblau）/ ←→ = 慣性 w / ↑↓ = c1（自分のベスト）/ C = c2（群れのベスト）を切替
--       クリック = その場所に粒子を集めてやり直す / R = ランダムにやり直す / F = 速さ / スペース = 一時停止
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftL, shiftR, xor)
import Data.List (elemIndex, foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)
import Numeric (showEFloat, showFFloat)

numP :: Int
numP = 30                    -- ★ 粒子の数

c2List :: [Double]
c2List = [0, 0.5, 1.0, 1.49, 2.0]   -- ★ C で切り替える c2

-- 関数：名前、範囲、式、最小値の場所
data Fn = Fn { fnName :: String, fnLo, fnHi :: !Double, fnF :: Double -> Double -> Double, fnOpt :: [(Double, Double)] }

funcs :: [Fn]
funcs =
  [ Fn "sphere x^2 + y^2" (-5.12) 5.12 (\x y -> x * x + y * y) [(0, 0)]
  , Fn "Rastrigin (many holes)" (-5.12) 5.12 rastrigin [(0, 0)]
  , Fn "Ackley (flat plateau, deep hole)" (-5) 5 ackley [(0, 0)]
  , Fn "Himmelblau (4 valleys)" (-5) 5 (\x y -> (x * x + y - 11) ^ (2 :: Int) + (x + y * y - 7) ^ (2 :: Int))
       [(3, 2), (-2.805118, 3.131312), (-3.77931, -3.283186), (3.584428, -1.848126)] ]
  where
    rastrigin x y = 20 + x * x - 10 * cos (2 * pi * x) + y * y - 10 * cos (2 * pi * y)
    ackley x y = -20 * exp (-0.2 * sqrt (0.5 * (x * x + y * y))) - exp (0.5 * (cos (2 * pi * x) + cos (2 * pi * y))) + exp 1 + 20

-- 粒子：位置 x y、速度 vx vy、自分のベストの場所 bx by と値 bv
data P = P { px, py, pvx, pvy, pbx, pby, pbv :: !Double }

-- 粒子 0 の速度の 3 成分（矢印で表示）：位置、慣性、自分のベストへ、群れのベストへ
data Forces = Forces !Double !Double !Double !Double !Double !Double !Double !Double

data World = World
  { wFnId    :: !Int
  , wField   :: !Picture          -- 等高線の絵（関数を変えたときだけ作り直す）
  , wIn      :: !Double           -- 慣性 w
  , wC1      :: !Double           -- 自分のベストへの引き
  , wC2      :: !Double           -- 群れのベストへの引き
  , wFpi     :: !Int              -- 何フレームに 1 回更新するか（F で切替）
  , wParts   :: ![P]
  , wGx, wGy :: !Double           -- 群れのベストの場所 g
  , wGVal    :: !Double
  , wIter    :: !Int
  , wHist    :: ![Float]          -- log10 f(g) の記録（新しい順、300 個まで）
  , wLast    :: !(Maybe Forces)
  , wPaused  :: !Bool
  , wSeed    :: !Seed
  }

world0 :: World
world0 = World 2 Blank 0.72 1.49 1.49 4 [] 0 0 inf 0 [] Nothing False 1

inf :: Double
inf = 1 / 0

main :: IO ()
main = runSketch (sketch "46-05 Particle swarm optimisation" world0)
  { skBack  = hsb 262 40 12
  , skStart = \_ -> do
      s <- randomRIO (1, 2 ^ (62 :: Int))
      pure (chooseFn 1 world0 { wSeed = s })
  , skStep  = \env _ w -> pure (if not (wPaused w) && envFrame env `mod` wFpi w == 0 then psoIteration w else w)
  , skDraw  = draw
  , skEvent = event
  }

curFn :: World -> Fn
curFn w = case drop (wFnId w - 1) funcs of
  (f : _) -> f
  [] -> Fn "sphere" (-5) 5 (\x y -> x * x + y * y) [(0, 0)]

evalF :: Fn -> Double -> Double -> Double
evalF fn x y = let v = fnF fn x y in if isNaN v || isInfinite v then 1e9 else v

chooseFn :: Int -> World -> World
chooseFn i w = let w' = w { wFnId = i } in restart Nothing w' { wField = makeField (curFn w') }

-- やり直す（at = そこに粒子を集める）
restart :: Maybe (Double, Double) -> World -> World
restart at w = w { wParts = ps, wGx = gx, wGy = gy, wGVal = gv, wIter = 0, wHist = [], wSeed = nextSeed s }
  where
    fn = curFn w
    sp = fnHi fn - fnLo fn
    s = wSeed w
    r i k = realToFrac (hashRand s (4 * i + k)) :: Double
    one i =
      let (x, y) = case at of
            Just (ax, ay) -> (ax + (r i 0 - 0.5) * sp * 0.08, ay + (r i 1 - 0.5) * sp * 0.08)
            Nothing -> (fnLo fn + r i 0 * sp, fnLo fn + r i 1 * sp)
      in P x y ((r i 2 - 0.5) * sp * 0.05) ((r i 3 - 0.5) * sp * 0.05) x y (evalF fn x y)
    ps = map one [0 .. numP - 1]
    (gv, gx, gy) = foldl' (\(bv, bx, by) p -> if pbv p < bv then (pbv p, px p, py p) else (bv, bx, by)) (inf, 0, 0) ps

-- ★ PSO の 1 回分
psoIteration :: World -> World
psoIteration w = w { wParts = ps', wGx = gx', wGy = gy', wGVal = gv', wIter = wIter w + 1, wSeed = nextSeed s
                   , wHist = take 300 (realToFrac (logBase 10 (max gv' 1e-12)) : wHist w)
                   , wLast = case ps of { [] -> wLast w; (p0 : _) -> Just (forces p0 0) } }
  where
    fn = curFn w
    vmax = (fnHi fn - fnLo fn) * 0.2
    s = wSeed w
    ps = wParts w
    forces p i =
      let r1 = realToFrac (hashRand s (2 * i))
          r2 = realToFrac (hashRand s (2 * i + 1))
      in Forces (px p) (py p) (wIn w * pvx p) (wIn w * pvy p)                       -- 慣性
                (wC1 w * r1 * (pbx p - px p)) (wC1 w * r1 * (pby p - py p))          -- 自分のベストへ
                (wC2 w * r2 * (wGx w - px p)) (wC2 w * r2 * (wGy w - py p))          -- 群れのベストへ
    move p i =
      let Forces _ _ ix iy ppx ppy gx gy = forces p i
          vx = clampD (-vmax) vmax (ix + ppx + gx)
          vy = clampD (-vmax) vmax (iy + ppy + gy)
          x = clampD (fnLo fn) (fnHi fn) (px p + vx)
          y = clampD (fnLo fn) (fnHi fn) (py p + vy)
          v = evalF fn x y
      in if v < pbv p then P x y vx vy x y v else p { px = x, py = y, pvx = vx, pvy = vy }
    ps' = zipWith move ps [0 ..]
    (gv', gx', gy') = foldl' (\(bv, bx, by) p -> if pbv p < bv then (pbv p, pbx p, pby p) else (bv, bx, by)) (wGVal w, wGx w, wGy w) ps'

draw :: Env -> World -> Picture
draw env w = view env $
  [ Translate (ox + sz / 2) (oy + sz / 2) (Scale (sz / 220) (-(sz / 220)) (wField w)) ]
  -- 最適解の場所
  ++ concat [ [ seg 2 (hsb 120 70 100) (sx a - 7, sy b) (sx a + 7, sy b), seg 2 (hsb 120 70 100) (sx a, sy b - 7) (sx a, sy b + 7) ] | (a, b) <- fnOpt fn ]
  -- 粒子：線 = 自分のベストへの線、点 = いまの位置
  ++ [ Color (hsbA 0 0 100 25) (Pictures [ Line [(sx (px p), sy (py p)), (sx (pbx p), sy (pby p))] | p <- wParts w ]) ]
  ++ [ dot (hsb 55 80 100) (sx (px p)) (sy (py p)) 8 | p <- wParts w ]
  ++ [ dot (hsb 0 90 100) (sx (wGx w)) (sy (wGy w)) 14 ]
  -- 粒子 0 の速度の 3 成分（矢印）
  ++ (case wLast w of
        Nothing -> []
        Just (Forces x y ix iy ppx ppy gx gy) ->
          let k = sz / realToFrac (fnHi fn - fnLo fn)
              x0 = sx x
              y0 = sy y
              d v = realToFrac v * k
          in [ arrowTo x0 y0 (d ix) (-(d iy)) (hsb 200 70 100)
             , arrowTo x0 y0 (d ppx) (-(d ppy)) (hsb 120 70 100)
             , arrowTo x0 y0 (d gx) (-(d gy)) (hsb 0 80 100)
             , Color (gray 1) (Translate x0 y0 (ring 8 1.5)) ])
  -- ---- 右：数字とグラフ
  ++ [ txt gx' (110 + fromIntegral i * 26) 16 (gray 1) l | (i, l) <- zip [0 :: Int ..] info ]
  ++ [ drawPlot (wHist w) gx' 280 gw 160 (-12) 3 (hsb 0 85 100) "Best value (vertical: log10 f, lower = closer to 0)"
     , txt (gx' - 36) 292 14 (hsb 0 0 70) "1e3"
     , txt (gx' - 52) 444 14 (hsb 0 0 70) "1e-12"
     , txt gx' 490 14 (hsb 200 70 100) "blue = inertia w v"
     , txt gx' 512 14 (hsb 120 70 100) "green = c1 r1 (p - x)  toward own best"
     , txt gx' 534 14 (hsb 0 80 100) "red = c2 r2 (g - x)  toward swarm best"
     , txt gx' 556 14 (hsb 0 0 70) "(3 parts for circled particle 0; sum = next velocity)"
     , txt 20 36 17 (gray 1) ("1-4: function   Left/Right: inertia w = " ++ fmtD 2 (wIn w) ++ "   Up/Down: c1 = " ++ fmtD 2 (wC1 w)
                              ++ "   C: c2 = " ++ fmtD 2 (wC2 w) ++ "   F: speed   Space: " ++ (if wPaused w then "paused" else "running"))
     , txt 20 60 14 (hsb 0 0 70) "Click: gather the swarm there and restart (start from a bad place on purpose)   R: random restart" ]
  where
    fn = curFn w
    sz = min (envH env - 110) (envW env * 0.58)
    ox = 30
    oy = 90
    sx x = ox + realToFrac ((x - fnLo fn) / (fnHi fn - fnLo fn)) * sz
    sy y = oy + sz - realToFrac ((y - fnLo fn) / (fnHi fn - fnLo fn)) * sz
    gx' = ox + sz + 40
    gw = envW env - gx' - 30
    info = [ "Function " ++ show (wFnId w) ++ ": " ++ fnName fn
           , "Iteration " ++ show (wIter w) ++ " (" ++ show numP ++ " particles)"
           , "Swarm best f(g) = " ++ fmtV (wGVal w)
           , "g = (" ++ fmtD 3 (wGx w) ++ ", " ++ fmtD 3 (wGy w) ++ ")"
           , "True minimum = 0 (green +)" ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ (mx, my) ->
    let fn = curFn w
        sz = min (envH env - 110) (envW env * 0.58)
        x = mx + envW env / 2
        y = envH env / 2 - my
        sp = fnHi fn - fnLo fn
    in if x < 30 || x > 30 + sz || y < 90 || y > 90 + sz then w
       else restart (Just (fnLo fn + realToFrac ((x - 30) / sz) * sp, fnLo fn + realToFrac ((90 + sz - y) / sz) * sp)) w
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> chooseFn (fromEnum c - fromEnum '0') w
    | c `elem` "cC" -> w { wC2 = case elemIndex (wC2 w) c2List of
                                  Just i -> c2List !! ((i + 1) `mod` length c2List)
                                  Nothing -> 0 }
    | c `elem` "fF" -> w { wFpi = if wFpi w == 4 then 1 else 4 }
    | c `elem` "rR" -> restart Nothing w
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wIn = min 1.2 (wIn w + 0.05) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wIn = max 0 (wIn w - 0.05) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wC1 = min 3 (wC1 w + 0.1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wC1 = max 0 (wC1 w - 0.1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1（球）は 100 回もあれば 1e-10 以下まで下がる。2（Rastrigin）だと途中で止まることがある。それはどんな場所？
-- 2. w = 1.0 以上にすると粒子が止まらず暴れる。w = 0.3 だとすぐ止まる。ちょうどいい w は？
-- 3. C で c2 = 0 にすると、みんな自分のベストしか見ない（群れでない）。結果はどうなる？
-- 4. 4（Himmelblau）では谷底が 4 つ。何回か R をして、群れはどの谷に行く？ 1 回で 4 つ全部は見つからない（07 で解決）
-- 5. 画面の隅をクリックして、わざと悪い場所から始める。それでも群れは谷底を見つけられる？（numP を 5 にすると？）

-- ---------- 道具 ----------
-- 等高線の絵（220 × 220）：値の log をとって色にし、14 段ごとに暗い線を入れる
makeField :: Fn -> Picture
makeField fn = pixelImage nf nf 1 pix
  where
    nf = 220
    sp = fnHi fn - fnLo fn
    vals = U.generate (nf * nf) (\k -> let (j, i) = k `divMod` nf
                                           x = fnLo fn + (fromIntegral i + 0.5) / fromIntegral nf * sp
                                           y = fnHi fn - (fromIntegral j + 0.5) / fromIntegral nf * sp
                                       in realToFrac (log (1 + evalF fn x y)) :: Float)
    vmax = max 1e-12 (U.maximum vals)
    pix i j =
      let t = vals U.! (j * nf + i) / vmax
          band = if fract (t * 14) < 0.08 then 0.75 else 1
      in (q ((40 + 200 * t) * band), q ((25 + 90 * (1 - t)) * band), q ((70 + 60 * (1 - t)) * band))
    q v = fromIntegral (max 0 (min 255 (round v :: Int)))

-- (x, y) から (dx, dy) だけ伸びる矢印（短すぎるときは描かない）
arrowTo :: Float -> Float -> Float -> Float -> Color -> Picture
arrowTo x y dx dy c
  | m < 2 = Blank
  | otherwise = Pictures [ seg 2.5 c (x, y) (ex, ey)
                         , seg 2.5 c (ex, ey) (ex - ux * 8 - uy * 5, ey - uy * 8 + ux * 5)
                         , seg 2.5 c (ex, ey) (ex - ux * 8 + uy * 5, ey - uy * 8 - ux * 5) ]
  where
    m = sqrt (dx * dx + dy * dy)
    ux = dx / m
    uy = dy / m
    ex = x + dx
    ey = y + dy

clampD :: Double -> Double -> Double -> Double
clampD lo hi = max lo . min hi

-- 小さい値は 1.23e-5 のように書く
fmtV :: Double -> String
fmtV v
  | isInfinite v = "-"
  | v < 1e-4 = showEFloat (Just 2) v ""
  | otherwise = fmtD 4 v

-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

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

-- グラフ：記録 hist（新しい順、300 個まで）を左上 (x, y)、幅 w、高さ h の枠に lo〜hi で描く
drawPlot :: [Float] -> Float -> Float -> Float -> Float -> Float -> Float -> Color -> String -> Picture
drawPlot hist x y w h lo hi c title = Pictures
  [ Color (hsbA 0 0 0 35) (rect (x + w / 2) (y + h / 2) w h)
  , txt x (y - 8) 14 (hsb 0 0 85) title
  , if length hist < 2 then Blank
    else path 2 c [ (x + fromIntegral i / 299 * w, y + h - clampF 0 1 ((v - lo) / (hi - lo)) * h)
                  | (i, v) <- zip [0 :: Int ..] (reverse hist) ] ]

-- 乱数（xorshift64）。種を World に入れておけば skStep は純粋なまま
type Seed = Word64

nextSeed :: Seed -> Seed
nextSeed s0 = let s1 = s0 `xor` (s0 `shiftL` 13)
                  s2 = s1 `xor` (s1 `shiftR` 7)
              in s2 `xor` (s2 `shiftL` 17)

-- 種 s と番号 i から決まる 0〜1 の乱数（たくさんの粒子に一度に配るとき用）
hashRand :: Seed -> Int -> Float
hashRand s i =
  let z0 = s + fromIntegral i * 0x9E3779B97F4A7C15
      z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
      z3 = z2 `xor` (z2 `shiftR` 31)
  in fromIntegral (z3 `shiftR` 40) / 16777216
