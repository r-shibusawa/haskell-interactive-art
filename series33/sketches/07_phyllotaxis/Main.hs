-- ============================================
-- 07 フィロタキシス（葉序）─ ひまわりの種の並び
-- i 番目の種を「角度 i·θ、半径 c√i」に置く。θ = 360° × φ（の小数部分）= 360°/φ が黄金角
--   黄金比なら 222.5°（逆回りに見ると 137.5°）。有理数の角度だとスポーク（放射状のすき間）が出る
-- ←→ = 角度のプリセット / ↑↓ = 0.05° ずつ微調整 / C = らせんの腕を色分け（本数を切りかえ）/ スペース = 成長をやり直す
-- ============================================
module Main (main) where

import Art
import Data.List (intercalate, nub)
import Numeric (showFFloat)

data World = World
  { wGrown  :: !Int      -- いま何個まで育ったか
  , wPreset :: !Int
  , wTheta  :: !Double   -- 角度（度）
  , wArm    :: !Int      -- 色分けに使う分母の番号（-1 = しない）
  }

seedCount :: Int
seedCount = 1400         -- ★ 種の数

presets :: [(String, Double)]
presets =
  [ ("Golden angle (n=1)", 360 / metallic 1)
  , ("Silver angle (n=2)", 360 / metallic 2)
  , ("Bronze angle (n=3)", 360 / metallic 3)
  , ("Rational 2/5 turn", 144)
  , ("Rational 3/8 turn", 135)
  , ("Rational 1/3 turn", 120)
  , ("Near golden 137.0", 137.0)
  , ("Closer 137.5", 137.5)
  ]

presetAt :: Int -> (String, Double)
presetAt i = case drop (i `mod` length presets) presets of
  (p : _) -> p
  [] -> ("", 137.5)

initialWorld :: World
initialWorld = World 0 0 (snd (presetAt 0)) (-1)

main :: IO ()
main = runSketch (sketch "33-07 Phyllotaxis" initialWorld)
  { skBack  = hsb 100 25 10
  , skStep  = \_ _ w -> pure w { wGrown = min seedCount (wGrown w + 12) }
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (seeds ++ hud ++ panel)
  where
    n = fromIntegral seedCount :: Float
    c = min (envW env * 0.7) (envH env - 110) * 0.45 / sqrt n    -- 種の間隔
    cx = envW env * 0.38
    cy = (envH env + 90) / 2
    theta = wTheta w
    dens = convergentDens (theta / 360)
    q | wArm w >= 0 && not (null dens) = case drop (wArm w `mod` length dens) dens of
          (d : _) -> d
          [] -> 0
      | otherwise = 0
    th = theta * pi / 180
    seeds =
      [ Color col (at (pt env (cx + r * realToFrac (cos a)) (cy + r * realToFrac (sin a))) (disc (c * 0.625)))
      | i <- [1 .. wGrown w]
      , let fi = fromIntegral i :: Float
      , let a = fromIntegral i * th :: Double
      , let r = c * sqrt fi
      , let col | q > 0 = hsb (fromIntegral (i `mod` q) * 360 / fromIntegral q) 70 95
                | otherwise = hsb (40 + 20 * sin (fi * 0.002)) (80 - 40 * fi / n) (100 - 30 * fi / n)
      ]
    hud =
      [ txt env 20 28 15 white ("theta = " ++ nfD 3 theta ++ " (L/R preset, Up/Dn fine)   " ++ fst (presetAt (wPreset w)) ++ "   C arms " ++ (if q > 0 then show q else "off") ++ "   Space regrow")
      , txt env 20 58 14 (gray 0.75) "A rational angle (p/q turn) makes q spokes with gaps. The golden angle packs without gaps."
      ]
    -- 右：θ/360 の連分数と、見えるらせんの本数
    px = envW env * 0.64
    py = 130
    cf = contFrac (theta / 360) 10
    (cf1, cf2) = splitAt 5 cf
    showCf xs = intercalate ", " (map show xs)
    panel =
      [ txt env px py 15 white ("theta / 360 = " ++ nfD 6 (theta / 360))
      , txt env px (py + 26) 15 white ("CF [" ++ showCf cf1 ++ (if null cf2 then "]" else ","))
      , txt env px (py + 48) 15 white (if null cf2 then "" else "   " ++ showCf cf2 ++ "]")
      , txt env px (py + 80) 14 (gray 0.8) "Convergent denominators = arm counts"
      ]
      ++ [ txt env (px + fromIntegral (j `mod` 4) * 105) (py + 106 + fromIntegral (j `div` 4) * 22) 14
             (if d == q then hsb 50 90 100 else gray 0.75) (show d ++ " arms")
         | (j, d) <- zip [0 :: Int ..] dens ]
      ++ zipWith (\i s -> txt env px (py + 186 + i * 22) 14 (gray 0.7) s) [0 ..]
           [ "Golden: 1, 2, 3, 5, 8, 13, 21, 34, 55 ..."
           , " -> Fibonacci! (sunflowers: 34 and 55)"
           , "Silver: 1, 2, 5, 12, 29 ... (Pell numbers)"
           , "Press C again and again and look for"
           , "the count where colors form straight arms."
           ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> choose ((wPreset w + 1) `mod` length presets)
  EventKey (SpecialKey KeyLeft) Down _ _  -> choose ((wPreset w + length presets - 1) `mod` length presets)
  EventKey (SpecialKey KeyUp) Down _ _    -> w { wTheta = wTheta w + 0.05 }
  EventKey (SpecialKey KeyDown) Down _ _  -> w { wTheta = wTheta w - 0.05 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wGrown = 0 }
  EventKey (Char c) Down _ _
    | c == 'c' || c == 'C' -> w { wArm = if wArm w >= 9 then -1 else wArm w + 1 }
  _ -> w
  where choose p = w { wPreset = p, wTheta = snd (presetAt p), wGrown = 0 }

-- ---------- やってみよう ----------
-- 1. 137.0° と 137.5° と黄金角を見くらべよう。ほんの 0.5° でどこにすき間が出る？
-- 2. C で 8・13・21・34 本の腕を順に色分けしよう。中心近くと外側で目立つ本数が変わる
-- 3. 種の大きさ（disc の半径）を i によって変えると、もっとひまわりらしくなる
-- 4. 半径を c·√i ではなく c·i にすると？（アルキメデスらせんになり、密度がそろわない）

-- ---------- 道具 ----------
metallic :: Double -> Double
metallic k = (k + sqrt (k * k + 4)) / 2

contFrac :: Double -> Int -> [Integer]
contFrac _ 0 = []
contFrac x m =
  let f = floor x :: Integer
      rest = x - fromIntegral f
  in if rest < 1e-9 then [f] else f : contFrac (1 / rest) (m - 1)

-- 近似分数の分母（2 以上 200 以下）
convergentDens :: Double -> [Int]
convergentDens x =
  let as = drop 1 (contFrac x 12)
      qs = go as 0 1
      go [] _ _ = []
      go (a : r) q0 q1 = let q2 = a * q1 + q0 in q2 : go r q1 q2
  in nub [ fromInteger q | q <- qs, q >= 2, q <= 200 ]

nfD :: Int -> Double -> String
nfD d v = showFFloat (Just d) v ""

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str
