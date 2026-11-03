-- ============================================
-- 08 最も無理な数 ─ 分数で近似しにくい数
-- 分母 q の分数 p/q で x を近似したときの「誤差 × q²」= q²·|x − p/q| を、q = 1, 2, 3 … について点で打つ
-- フルヴィッツの定理：どんな無理数も q²|x − p/q| < 1/√5 を無限回みたす。φ は 1/√5 より良くならない = いちばん近似しにくい
-- 貴金属比 φn では下限が 1/√(n²+4) に近づく。π は 355/113 で急に 0 近くまで落ちる
-- マウスを点に重ねる = その分数 / スペース = 一時停止 / ↑↓ = 分母の上限 / R = やり直す
-- ============================================
-- p5.js 版は点を別の層に描きためる。e < 0.5 の点はごく少ないので、ここでは点をリストに覚えて毎回描く。
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showFFloat)

-- 調べる数
data Band = Band { bName :: String, bV :: !Double, bHue :: !Float, bBound :: !Double }

-- 打った点：帯の番号、分母 q、値 e
data Dot = Dot !Int !Int !Double

data World = World
  { wQmax   :: !Int      -- ★ 分母の上限
  , wQ      :: !Int      -- いま何番目の分母まで調べたか
  , wPaused :: !Bool
  , wDots   :: ![Dot]
  , wBest   :: ![Maybe (Int, Int)]  -- 帯ごとの、それまでで一番よい分数 p/q
  }

bands :: [Band]
bands =
  [ Band "phi1 golden" (metallic 1) 45 (1 / sqrt 5)
  , Band "phi2 = 1+sqrt2" (metallic 2) 200 (1 / sqrt 8)
  , Band "phi3 bronze" (metallic 3) 25 (1 / sqrt 13)
  , Band "e" (exp 1) 120 (-1)
  , Band "pi" pi 330 (-1)
  ]

restart :: Int -> World -> World
restart qmax w = w { wQmax = qmax, wQ = 1, wDots = [], wBest = map (const Nothing) bands }

initialWorld :: World
initialWorld = restart 3000 (World 3000 1 False [] [])

main :: IO ()
main = runSketch (sketch "33-08 Most Irrational" initialWorld)
  { skBack  = hsb 260 25 9
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- 分母 q の最良の p
bestP :: Double -> Int -> Int
bestP v q = floor (fromIntegral q * v + 0.5)

errOf :: Double -> Int -> Int -> Double
errOf v p q = abs (v - fromIntegral p / fromIntegral q)

-- 1 フレームに 40 個ずつ分母を増やす
stepWorld :: World -> World
stepWorld w
  | wPaused w || wQ w > wQmax w = w
  | otherwise =
      let qs = [wQ w .. min (wQ w + 39) (wQmax w)]
      in foldl' oneQ w qs

oneQ :: World -> Int -> World
oneQ w q =
  let results = zipWith one (zip [0 ..] bands) (wBest w)
      one (i, b) best =
        let p = bestP (bV b) q
            e = fromIntegral (q * q) * errOf (bV b) p q
            better = case best of
              Nothing -> True
              Just (lp, lq) -> errOf (bV b) p q < errOf (bV b) lp lq
        in ( [ Dot i q e | e < 0.5 ], if better then Just (p, q) else best )
  in w { wQ = q + 1
       , wDots = concatMap fst results ++ wDots w
       , wBest = map snd results }

-- 帯 i の位置 (x, y, 幅, 高さ)
bandRect :: Env -> Int -> (Float, Float, Float, Float)
bandRect env i =
  let top = 100
      h = (envH env - top - 30) / fromIntegral (length bands)
  in (250, top + fromIntegral i * h, envW env - 280, h - 12)

qToX :: World -> (Float, Float, Float, Float) -> Int -> Float
qToX w (x, _, rw, _) q = x + realToFrac (log (fromIntegral q) / log (fromIntegral (wQmax w)) :: Double) * rw

vToY :: (Float, Float, Float, Float) -> Double -> Float
vToY (_, y, _, rh) v = y + rh - clampF 0 1 (realToFrac (v / 0.5)) * rh

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (concat (zipWith3 drawBand [0 ..] bands (wBest w)) ++ dots ++ hover ++ hud)
  where
    drawBand :: Int -> Band -> Maybe (Int, Int) -> [Picture]
    drawBand i b best =
      let r@(rx, ry, rw, rh) = bandRect env i
          hasBound = bBound b > 0
          limitY = vToY r (1 / sqrt 5)
      in [ Color (withA 0.15 white) (Line (map (uncurry (pt env)) [(rx, ry), (rx + rw, ry), (rx + rw, ry + rh), (rx, ry + rh), (rx, ry)]))
         , Color (withA 0.4 white) (dashed (pt env rx limitY) (pt env (rx + rw) limitY))
         , txt env 20 (ry + 24) 15 (hsb (bHue b) 60 100) (bName b)
         , txt env 20 (ry + 44) 14 (gray 0.7) ("x = " ++ nfD 6 (bV b))
         ]
         ++ [ Color (hsba (bHue b) 80 100 0.8) (dashed (pt env rx (vToY r (bBound b))) (pt env (rx + rw) (vToY r (bBound b)))) | hasBound ]
         ++ [ txt env 20 (ry + 64) 14 (gray 0.7) ("bound = " ++ nfD 3 (bBound b)) | hasBound ]
         ++ [ txt env 20 (ry + (if hasBound then 84 else 64)) 14 (gray 0.7) ("best " ++ show p ++ "/" ++ show q) | Just (p, q) <- [best] ]
    dots =
      [ Color (hsba (bHue b) 70 100 0.9) (at (pt env (qToX w r q) (vToY r e)) (disc 3))
      | Dot i q e <- wDots w
      , b <- take 1 (drop i bands)
      , let r = bandRect env i ]
    -- マウスの下の点を調べる
    hover = concat (zipWith hoverBand [0 ..] bands)
    (mx0, my0) = envMouse env
    mx = mx0 + envW env / 2
    my = envH env / 2 - my0
    hoverBand :: Int -> Band -> [Picture]
    hoverBand i b =
      let r@(rx, ry, rw, rh) = bandRect env i
          inside = mx > rx && mx < rx + rw && my > ry && my < ry + rh
          raw = exp (realToFrac ((mx - rx) / rw) * log (fromIntegral (wQmax w))) :: Double
          qq = max 1 (min (wQmax w) (round raw))
          p = bestP (bV b) qq
          e = fromIntegral (qq * qq) * errOf (bV b) p qq
          lx = qToX w r qq
      in if not inside then [] else
           [ Color (withA 0.6 white) (Line [pt env lx ry, pt env lx (ry + rh)])
           , txt env (min (mx + 10) (envW env - 420)) (ry + 18) 14 white
               (show p ++ "/" ++ show qq ++ "  q^2|x - p/q| = " ++ nfD 4 e)
           ]
    hud =
      [ txt env 20 24 15 white ("q = 1 to " ++ show (min (wQ w) (wQmax w)) ++ " (limit " ++ show (wQmax w) ++ ", Up/Dn)   x: q (log)   y: q^2|x - p/q| (0-0.5)   Space pause   R redo")
      , txt env 20 46 14 (gray 0.75) "White dashes = 1/sqrt5 = 0.447 (Hurwitz limit). Dots far below = easy to approximate by fractions."
      , txt env 20 68 14 (gray 0.75) "Dots of phi stick to the 0.447 line and can hardly go below it. Bound = 1/sqrt(n^2+4)."
      ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyUp) Down _ _    -> restart (min 20000 (wQmax w * 2)) w
  EventKey (SpecialKey KeyDown) Down _ _  -> restart (max 200 (wQmax w `div` 2)) w
  EventKey (Char c) Down _ _
    | c == 'r' || c == 'R' -> restart (wQmax w) w
  _ -> w

-- ---------- やってみよう ----------
-- 1. π の帯で 0 近くまで落ちる点を探し、マウスで分数を読もう（22/7、355/113）
-- 2. e の帯は点がだんだん 0 に近づいていく。e の連分数 [2; 1, 2, 1, 1, 4, 1, 1, 6, …] と関係がある
-- 3. bands に √3 や (1+√5)/2 + 1 を足すと、φ と同じ下限になるのはどれ？（連分数がいつか 1 ばかりになる数）
-- 4. 07 のひまわり：角度が「近似しにくい」ほど種がすき間なく並ぶ理由を説明してみよう

-- ---------- 道具 ----------
metallic :: Double -> Double
metallic k = (k + sqrt (k * k + 4)) / 2

nfD :: Int -> Double -> String
nfD d v = showFFloat (Just d) v ""

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

-- 点線
dashed :: Point -> Point -> Picture
dashed a@(ax, ay) b@(bx, by) =
  let len = dist2 a b
      n = floor (len / 10) :: Int
      f t = (ax + (bx - ax) * t, ay + (by - ay) * t)
  in Pictures [ Line [f (fromIntegral i * 10 / len), f ((fromIntegral i * 10 + 4) / len)] | i <- [0 .. n], len > 0 ]
