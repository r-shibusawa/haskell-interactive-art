-- ============================================
-- 07 1 の n 乗根：FFT を速くする魔法の点
-- ωₙ = e^{2πi/n} は円を n 等分する点。n 乗すると 1 に戻る
-- 1：累乗（掛け算 = 角度の足し算）  2：半分補題（2 乗すると n/2 乗根になる → 問題が半分に）
-- 3：相殺補題（全部足すと 0）  4：DFT 行列（y = V a の V の中身）
-- ←→：n を変える / ↑↓：k を変える / スペース：アニメの一時停止
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt)
import Numeric (showFFloat)

data World = World
  { wN      :: !Int     -- ★ 最初の n（2 の累乗にすると半分補題がきれい）
  , wMode   :: !Int
  , wK      :: !Int
  , wT      :: !Float
  , wPaused :: !Bool
  }

main :: IO ()
main = runSketch (sketch "16-07 Roots of Unity" (World 8 1 1 0 False))
  { skBack  = hsb 265 45 10
  , skStep  = \_ _ w -> pure (if wPaused w then w else w { wT = wT w + 0.008 })
  , skDraw  = drawW
  , skEvent = onEvent
  }

-- HSB の灰色（p5.js の fill(0, 0, v)）
gv :: Float -> Color
gv = hsb 0 0

drawW :: Env -> World -> Picture
drawW env w =
  let n = wN w
      hud = Pictures
        [ txt env 30 50 26 (gv 92) "Roots of unity: the magic points behind FFT"
        , txt env 30 80 15 (gv 60) ("1: powers  2: halving  3: cancellation  4: DFT matrix  L/R: n = " ++ show n
                                     ++ "  Up/Down: k = " ++ show (wK w) ++ "  Space: " ++ (if wPaused w then "resume" else "pause"))
        ]
  in Pictures (hud : (if wMode w == 4 then [dftMatrix env n] else [circleMode env w]))

circleMode :: Env -> World -> Picture
circleMode env w =
  let n = wN w
      nf = fromIntegral n :: Float
      t = wT w
      k = wK w
      wd = envW env
      hg = envH env
      cx = wd * 0.32
      cy = hg * 0.56
      rad = min (wd * 0.25) (hg * 0.36)
      pAt r ang = (cx + cos ang * r, cy - sin ang * r)
      axes = Pictures
        [ seg env (gv 30) 1 (cx - rad * 1.2, cy) (cx + rad * 1.2, cy)
        , seg env (gv 30) 1 (cx, cy - rad * 1.2) (cx, cy + rad * 1.2)
        , hoop env (gv 45) 2 cx cy (rad * 2)
        , txt env (cx + rad * 1.2 + 4) (cy + 5) 14 (gv 60) "Re"
        , txt env (cx + 6) (cy - rad * 1.2 + 14) 14 (gv 60) "Im"
        ]
      side = wd * 0.62
      sw = wd - side - 30
      body = case wMode w of
        1 -> mode1
        2 -> mode2
        _ -> mode3
      -- 1：累乗
      mode1 =
        let kk = floor (t * 3) `mod` (n + 1) :: Int
            mark j =
              let (x, y) = pAt rad (tau * fromIntegral j / nf)
                  c = hsb (fromIntegral j * 360 / nf) 70 95
              in Pictures
                   [ dot env c x y (if j == kk `mod` n then 26 else 16)
                   , txt env (x + (if x > cx then 12 else -44)) (y + (if y > cy then 22 else -12)) 14 (gv 90) ("w^" ++ show j) ]
            spiral = [ pAt (rad * (0.55 + 0.4 * (fromIntegral j / nf))) (tau * fromIntegral j / nf) | j <- [0 .. kk] ]
            yel = hsb 45 90 100
            deg = 360 / nf
            f1 v = showFFloat (Just 1) (v :: Float) ""
        in Pictures
             [ Pictures (map mark [0 .. n - 1])
             , Color yel (thickPath 3 (map (pt env) spiral))
             , seg env yel 3 (cx, cy) (pAt rad (tau * fromIntegral kk / nf))
             , stack env side 130 sw
                 [ (18, gv 92, 18, "w_n = e^(2 pi i/" ++ show n ++ ") = cos(2 pi/" ++ show n ++ ") + i sin(2 pi/" ++ show n ++ ")")
                 , (18, gv 92, 18, "w^" ++ show kk ++ " has angle " ++ show kk ++ " x " ++ f1 deg ++ " = " ++ f1 (fromIntegral kk * deg) ++ " deg")
                 , (18, yel, 30, if kk == n then "w^" ++ show n ++ " = 1: back after one full turn!" else "Each multiplication adds 360/n degrees")
                 , (15, gv 65, 0, "Complex product: multiply lengths, add angles. Multiplying by a number of length 1 only rotates.\nAll n roots become 1 when raised to n: the n solutions of x^n = 1.")
                 ]
             ]
      -- 2：半分補題
      mode2 =
        let s = clampF 0 1 ((sin (t * 2) + 1) / 2 * 1.3 - 0.15) -- 0：もとの根 → 1：2 乗した根
            hn = max 1 (n `div` 2)
            mark j =
              let a0 = tau * fromIntegral j / nf
                  (x, y) = pAt rad (a0 * (1 + s))
              in dot env (hsba (fromIntegral (j `mod` hn) * 360 / fromIntegral hn) 70 95 0.85) x y (if j >= n `div` 2 then 14 else 20)
            h2 = n `div` 2
        in Pictures
             [ Pictures (map mark [0 .. n - 1])
             , stack env side 130 sw
                 [ (18, gv 92, 18, "Square the " ++ show n ++ " roots...")
                 , (18, hsb 45 90 100, 18, "They land on " ++ show h2 ++ " " ++ show h2 ++ "-th roots (same colours merge)")
                 , (18, gv 92, 18, "(w_n)^2 has double the angle: same as w_(n/2)")
                 , (18, gv 92, 30, "w^1 and w^" ++ show (1 + h2) ++ " are 180 deg apart: their squares coincide")
                 , (15, gv 65, 0, "Halving lemma: (w_n^(k+n/2))^2 = (w_n^k)^2.\nFFT splits even and odd coefficients: A(x) = A0(x^2) + x*A1(x^2). Only n/2 values of x^2 remain, so solve 2 problems of half size: T(n) = 2T(n/2) + Theta(n) = Theta(n log n)")
                 ]
             ]
      -- 3：相殺補題（Σ_j (ω^k)^j を矢印の継ぎ足しで描く）
      mode3 =
        let lenA = rad * 0.9 / max 2 (nf / 3)
            start = (wd * 0.32 - lenA, cy + rad * 0.3)
            shown = min n (floor (t * 4) `mod` (n + 6))
            angOf j = tau * fromIntegral (k * j `mod` n) / nf
            walk p [] = (p, [])
            walk p@(x, y) (j : js) =
              let ang = angOf j
                  q = (x + cos ang * lenA, y - sin ang * lenA)
                  c = hsb (fromIntegral j * 360 / nf) 70 95
                  (e, ps) = walk q js
              in (e, seg env c 3 p q : uncurry (dot env c) q 8 : ps)
            (_, arrows) = walk start [0 .. shown - 1]
            sumX = sum [cos (angOf j) | j <- [0 .. shown - 1]]
            sumY = sum [sin (angOf j) | j <- [0 .. shown - 1]]
            isZero = k `mod` n /= 0
            f2 v = showFFloat (Just 2) (v :: Float) ""
        in Pictures
             [ Pictures arrows
             , stack env side 130 sw
                 [ (18, gv 92, 18, "Sum of (w_n^" ++ show k ++ ")^j  (j = 0 .. " ++ show (n - 1) ++ ")")
                 , (18, if isZero then hsb 150 70 90 else hsb 45 90 100, 18,
                     if isZero then "k = " ++ show k ++ " is not a multiple of n: the arrows close, sum 0"
                               else "k = " ++ show k ++ " is a multiple of n: all 1, sum " ++ show n)
                 , (18, gv 80, 30, "Sum so far = " ++ f2 sumX ++ " + " ++ f2 sumY ++ "i")
                 , (15, gv 65, 0, "Cancellation (summation) lemma: if k is not a multiple of n, the sum of w_n^(jk) is 0.\nThis is why the inverse DFT is just w -> w^-1, divide by n.\nUp/Down changes k: look for closed polygons and stars.")
                 ]
             ]
  in Pictures [axes, body]

dftMatrix :: Env -> Int -> Picture
dftMatrix env n =
  let nN = min n 32
      nf = fromIntegral nN :: Float
      cs = min ((envH env - 170) / nf) (envW env * 0.55 / nf)
      ox = 40
      oy = 110
      cell j kk =
        let e = (j * kk) `mod` nN
            ang = tau * fromIntegral e / nf
            x = ox + fromIntegral kk * cs
            y = oy + fromIntegral j * cs
            cxx = x + cs / 2
            cyy = y + cs / 2
        in Pictures
             [ box env (hsb (fromIntegral e * 360 / nf) 65 85) x y (cs - 1) (cs - 1)
             , if cs > 14
                 then seg env (gv 10) (max 1 (cs / 14)) (cxx, cyy) (cxx + cos ang * cs * 0.38, cyy - sin ang * cs * 0.38)
                 else Blank ]
      side = ox + nf * cs + 40
      sw = envW env - side - 30
      nlogn = floor (nf * logBase 2 nf + 0.5) :: Int
  in Pictures
       [ Pictures [cell j kk | j <- [0 .. nN - 1], kk <- [0 .. nN - 1]]
       , stack env side 120 sw
           [ (18, gv 92, 18, "DFT matrix V (" ++ show nN ++ "x" ++ show nN ++ ")")
           , (15, gv 70, 0, "Row j, column k = w_n^(jk). Colour and needle show the angle.\nComputing y = V a directly takes n^2 = " ++ show (nN * nN)
                             ++ " multiplications.\n\nEach row is a needle turning at speed k: the same DFT as in series 5.\n\nSplit even and odd columns: the top and bottom halves repeat the same pattern (halving lemma). FFT uses this to cut the work to n log n = "
                             ++ show nlogn ++ ".\n\nV^-1 uses w^-1 (turning backward), divided by n.")
           ]
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> fixN w { wMode = digitToInt c }
    | otherwise -> fixN w
  EventKey (SpecialKey KeyRight) Down _ _ ->
    let n = wN w in fixN w { wN = min 32 (n + (if wMode w == 2 then n else 1)) }
  EventKey (SpecialKey KeyLeft) Down _ _ ->
    let n = wN w in fixN w { wN = max 2 (if wMode w == 2 then n `div` 2 else n - 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> fixN w { wK = wK w + 1 }
  EventKey (SpecialKey KeyDown) Down _ _ -> fixN w { wK = max 0 (wK w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> fixN w { wPaused = not (wPaused w) }
  _ -> w

-- モード 2 では n を 2 の累乗にそろえる
fixN :: World -> World
fixN w
  | wMode w == 2 = w { wN = 2 ^ (round (logBase 2 (fromIntegral (wN w) :: Float)) :: Int) }
  | otherwise = w

-- ---------- やってみよう ----------
-- 1. 3 で n = 8, k = 2 → 4 角形を 2 周。k = 3 → 星形。k = 8 → まっすぐ伸びる（合計 8）
-- 2. 2 で n = 16 → 8 → 4 … と半分にしていく。何回で 1 点になる？（log₂ n 回 = FFT の段数）
-- 3. 4 の DFT 行列で、0 行目と 0 列目が全部同じ色（= 1）になる理由は？

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> (Float, Float) -> (Float, Float)
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> (Float, Float) -> (Float, Float) -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 長方形の枠線（太さ t）
frame :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
frame env c t x y w h = Pictures
  [ seg env c t (x, y) (x + w, y), seg env c t (x + w, y) (x + w, y + h)
  , seg env c t (x + w, y + h) (x, y + h), seg env c t (x, y + h) (x, y) ]

-- 直径 d の円（p5.js の circle）
dot :: Env -> Color -> Float -> Float -> Float -> Picture
dot env c x y d = Color c (at (pt env (x, y)) (disc (d / 2)))

-- 直径 d の円の輪郭（太さ t）
hoop :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
hoop env c t x y d = Color c (at (pt env (x, y)) (ring (d / 2) t))

-- 文字：txt は左下が (x, y)、txtT は左上が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtT, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtT env x y s c str = txt env x (y + s) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = txt env (x - fromIntegral (length str) * s * 0.78) y s c str

-- ---------- 道具：文字の折り返し（文字の幅はおよそ 0.85 × 大きさ） ----------
wrapText :: Float -> Float -> String -> [String]
wrapText s w = concatMap one . lines
  where
    limit = max 8 (floor (w / (s * 0.85))) :: Int
    one l = case words l of
      [] -> [""]
      (x : xs) -> go x xs
    go cur [] = [cur]
    go cur (x : xs)
      | length cur + 1 + length x <= limit = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs

lineH :: Float -> Float
lineH s = s * 1.35

-- 左上 (x, y) から幅 w で折り返して書く
para :: Env -> Float -> Float -> Float -> Color -> Float -> String -> Picture
para env x y s c w str = Pictures
  [ txtT env x (y + fromIntegral i * lineH s) s c l | (i, l) <- zip [0 :: Int ..] (wrapText s w str) ]

-- 折り返したときの高さ
paraH :: Float -> Float -> String -> Float
paraH s w str = fromIntegral (length (wrapText s w str)) * lineH s

-- 上から順に積む：(大きさ, 色, 下のすきま, 文字)
stack :: Env -> Float -> Float -> Float -> [(Float, Color, Float, String)] -> Picture
stack env x y0 w = Pictures . go y0
  where
    go _ [] = []
    go y ((s, c, gap, str) : rest) = para env x y s c w str : go (y + paraH s w str + gap) rest

-- 画面の下の端にそろえて書く
footer :: Env -> Float -> Float -> Float -> Color -> String -> Picture
footer env x w s c str = para env x (envH env - 14 - paraH s w str) s c w str
