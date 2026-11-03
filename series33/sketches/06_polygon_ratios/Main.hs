-- ============================================
-- 06 正多角形の中の貴金属比
-- 正 m 角形で、k 個とばしの対角線と辺の比は  d(k) / 辺 = sin(kπ/m) / sin(π/m)
--   正五角形 → φ（黄金比） / 正八角形 → 1+√2（白銀比） / 正十二角形 → 1+√3, 2+√3 …
-- ←→ = 角の数 m / マウスを頂点に近づける = その頂点からの対角線 / クリック = 固定 / D = 全対角線 / スペース = 回転
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showFFloat)

data World = World
  { wM      :: !Int    -- ★ 正 m 角形
  , wPicked :: !Int    -- クリックで固定した頂点
  , wAll    :: !Bool
  , wSpin   :: !Bool
  , wRot    :: !Float
  }

initialWorld :: World
initialWorld = World 5 (-1) True False (-pi / 2)

-- 名前つきの比（見つけたら名前を出す）
named :: [(Double, String)]
named =
  [ (sqrt 2, "sqrt2 (silver, Yamato)")
  , ((1 + sqrt 5) / 2, "phi (golden)")
  , (sqrt 3, "sqrt3")
  , (2, "2")
  , (1 + sqrt 2, "1+sqrt2 (silver)")
  , (1 + sqrt 3, "1+sqrt3 (2nd silver)")
  , (2 + sqrt 3, "2+sqrt3")
  , ((1 + sqrt 5) / 2 + 1, "phi^2 = phi+1")
  , (sqrt (4 + 2 * sqrt 2), "sqrt(4+2sqrt2)")
  , (sqrt (2 + sqrt 2), "sqrt(2+sqrt2)")
  ]

main :: IO ()
main = runSketch (sketch "33-06 Polygon Ratios" initialWorld)
  { skBack  = hsb 250 25 10
  , skStep  = \_ _ w -> pure (if wSpin w then w { wRot = wRot w + 0.004 } else w)
  , skDraw  = drawWorld
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

-- 多角形の中心と半径
layout :: Env -> (Float, Float, Float)
layout env = (envW env * 0.36, envH env * 0.55, min (envW env * 0.3) (envH env * 0.38))

-- 頂点の座標（p5.js と同じ座標系）
vertices :: Env -> World -> [Point]
vertices env w =
  let (cx, cy, r) = layout env
      m = wM w
  in [ (cx + r * cos a, cy + r * sin a)
     | i <- [0 .. m - 1], let a = wRot w + tau * fromIntegral i / fromIntegral m ]

-- マウスにいちばん近い頂点（固定していなければ）
selected :: Env -> World -> [Point] -> Int
selected env w ps
  | wPicked w >= 0 = wPicked w
  | best > r * 0.6 = -1
  | otherwise = sel
  where
    (_, _, r) = layout env
    mp = mouseP env
    (best, sel) = foldl' pick (1e9, -1) (zip [0 ..] ps)
    pick (b, s) (i, p) = let d = dist2 mp p in if d < b then (d, i) else (b, s)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (diagonals ++ [edges] ++ picked ++ dots ++ hud ++ panel)
  where
    m = wM w
    ps = vertices env w
    gp = map (uncurry (pt env)) ps
    sel = selected env w ps
    half = m `div` 2
    -- 全部の対角線（長さの種類ごとに色）
    indexed = zip [0 :: Int ..] gp
    diagonals
      | wAll w = [ Color (hsba (hueOf k) 50 80 0.25) (Line [a, b])
                 | (i, a) <- indexed, (j, b) <- indexed, j >= i + 2
                 , let k = min (j - i) (m - (j - i)) ]
      | otherwise = []
    -- 辺
    edges = Color white (thickPath 3 (gp ++ take 1 gp))
    -- 選んだ頂点からの対角線
    from k = take 1 (drop ((sel + k) `mod` m) gp)
    picked
      | sel < 0 = []
      | otherwise =
          let origin = take 1 (drop sel gp)
          in [ Pictures [ Color (hsb (hueOf k) 80 100) (thickLine 4 a q)
                        , Color (hsb (hueOf k) 80 100) (at q (disc 6)) ]
             | k <- [2 .. half], a <- origin, q <- from k ]
             ++ [ Color white (at a (disc 9)) | a <- origin ]
    dots = [ Color white (at p (disc 3.5)) | p <- gp ]
    hud =
      [ txt env 20 28 15 white ("Regular " ++ show m ++ "-gon (L/R)   Mouse to vertex, click to fix " ++ onOff (wPicked w >= 0) ++ "   D all " ++ onOff (wAll w) ++ "   Space spin")
      , txt env 20 58 14 (gray 0.75) "Diagonal length with side = 1: sin(k pi/m) / sin(pi/m) skips k vertices."
      ]
    -- 右：比の表
    px = envW env * 0.63
    py = 130
    panel =
      [ txt env px py 16 white "Side = 1" ]
      ++ [ txt env px (py + 34 + fromIntegral (k - 1) * 28) 15 (if k == 1 then white else hsb (hueOf k) 70 100)
             ((if k == 1 then "side  " else "d" ++ show k ++ "  ") ++ nfD 5 v ++ (if null nm then "" else " = " ++ nm))
         | k <- [1 .. half]
         , let v = sin (fromIntegral k * pi / fromIntegral m) / sin (pi / fromIntegral m) :: Double
         , let nm = nameOfValue v ]
      ++ zipWith (\i s -> txt env px (yy + i * 22) 14 (gray 0.7) s) [0 ..]
           [ "Examples"
           , "Pentagon: diagonal / side = phi"
           , "Octagon: d3 / side = 1 + sqrt2"
           , "         d2 / side = sqrt(2+sqrt2)"
           , "Dodecagon: d3 = 1 + sqrt3, d5 = 2 + sqrt3"
           , "Square: diagonal / side = sqrt2 (A paper)"
           ]
    yy = py + 60 + fromIntegral half * 28

nameOfValue :: Double -> String
nameOfValue v = case [ s | (t, s) <- named, abs (t - v) < 1e-6 ] of
  (s : _) -> s
  [] -> ""

hueOf :: Int -> Float
hueOf k = fromIntegral ((k * 67 + 20) `mod` 360)

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | wPicked w >= 0 -> w { wPicked = -1 }
    | otherwise ->
        let mp = mouseP env
            hits = [ i | (i, p) <- zip [0 ..] (vertices env w), dist2 mp p < 40 ]
        in case reverse hits of
             (i : _) -> w { wPicked = i }
             [] -> w
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wM = min (wM w + 1) 24, wPicked = -1 }
  EventKey (SpecialKey KeyLeft) Down _ _  -> w { wM = max (wM w - 1) 3, wPicked = -1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wSpin = not (wSpin w) }
  EventKey (Char c) Down _ _
    | c == 'd' || c == 'D' -> w { wAll = not (wAll w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 正五角形の対角線を全部ひくと星形の中に小さな五角形ができる。大きさの比は？（φ² 倍）
-- 2. 正十角形・正十二角形で、表に名前が出る対角線を探そう
-- 3. 正 m 角形の d2 / 辺 = 2cos(π/m)。m を大きくすると 2 に近づくのはなぜ？
-- 4. named に「(√6+√2)/2」（正十二角形の d2）など自分で見つけた値を足してみよう

-- ---------- 道具 ----------
onOff :: Bool -> String
onOff b = if b then "ON" else "off"

nfD :: Int -> Double -> String
nfD d v = showFFloat (Just d) v ""

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str
