-- ============================================
-- 03 極と安定性（s 平面）
-- システムの「性格」は極（伝達関数の分母が 0 になる s）で決まる
--   極 p = σ ± jω　→　応答は e^(σt) cos(ωt)
--   σ < 0（左半面）なら消えていく = 安定、σ > 0（右半面）なら育つ = 不安定
-- 極をマウスでドラッグ / 1〜3 で極の数（実数 1 つ・複素対・対 + 実数）
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

tMax :: Double
tMax = 10       -- ★ インパルス応答を計算する時間 [秒]

sigLo, sigHi, omLo, omHi :: Float
sigLo = -5      -- s 平面の範囲
sigHi = 2
omLo = -5
omHi = 5

type Pole = (Float, Float)   -- (実部 σ, 虚部 ω)

data World = World
  { wMode  :: !Int            -- ★ 1 = 実数極 1 つ、2 = 複素共役の対、3 = 対 + 実数極
  , wPoles :: ![Pole]
  , wDrag  :: !Int            -- ドラッグ中の極（なければ -1）
  , wResp  :: !(U.Vector Float)  -- インパルス応答（極が変わったときだけ計算する）
  }

makePoles :: Int -> [Pole]
makePoles 1 = [(-1, 0)]
makePoles 2 = [(-0.5, 2.5), (-0.5, -2.5)]
makePoles _ = [(-0.5, 2.5), (-0.5, -2.5), (-2, 0)]

setPoles :: [Pole] -> World -> World
setPoles ps w = w { wPoles = ps, wResp = impulseResponse ps tMax }

world0 :: World
world0 = setPoles (makePoles 2) (World 2 [] (-1) U.empty)

main :: IO ()
main = runSketch (sketch "20-03 Poles and Stability" world0)
  { skBack  = hsb 230 30 10
  , skDraw  = drawAll
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

-- 画面の s 平面の枠
data Plane = Plane !Float !Float !Float !Float

planeOf :: Env -> Plane
planeOf env = Plane (envW env * 0.05) (envH env * 0.18) (envW env * 0.4) (envH env * 0.66)

-- 画面座標 ↔ s 平面
sx, sy :: Plane -> Float -> Float
sx (Plane x _ w _) re = remap re sigLo sigHi x (x + w)
sy (Plane _ y _ h) im = remap im omLo omHi (y + h) y

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures $
  [ box planeX planeY (x0 - planeX) planeH (hsba 120 50 30 0.4)                -- 左半面 = 安定
  , box x0 planeY (planeX + planeW - x0) planeH (hsba 0 60 40 0.4)             -- 右半面 = 不安定
  ]
  ++ [ Color (hsb 0 0 40) (Pictures (circleInPlane pl x0 y0 (sx pl r - x0))) | r <- [1 .. 5] ]
  ++
  [ seg 2 (hsb 0 0 70) (x0, planeY) (x0, planeY + planeH)
  , seg 2 (hsb 0 0 70) (planeX, y0) (planeX + planeW, y0)
  , txt (planeX + planeW - 140) (y0 - 8) 14 c70 "sigma (real) ->"
  , txt (x0 + 8) (planeY + 16) 14 c70 "jw (imag)"
  , txt (planeX + 10) (planeY + planeH - 10) 14 c70 "Stable (left half-plane)"
  , txt (x0 + 10) (planeY + planeH - 10) 14 c70 "Unstable"
  ]
  ++ map drawPole wPoles
  ++
  [ -- インパルス応答（右）
    box gx gy gw gh (hsb 0 0 14)
  , seg 1 (hsb 0 0 30) (gx, gy + gh / 2) (gx + gw, gy + gh / 2)
  , Color (hsb (if maxRe < 0 then 120 else if maxRe > 0 then 0 else 45) 80 100) (thickPath 2 respPts)
  , txt (gx + 8) (gy + 20) 14 c70 ("Impulse response (0-" ++ nf 0 (realToFrac tMax) ++ " s)  vertical +/-" ++ nf 1 peakAbs)
    -- 説明
  , txt 20 40 18 white ("Drag a pole   1-3: number of poles   Now: " ++ verdict)
  , txt 20 68 15 c60 ("p = sigma +/- jw -> e^(sigma t) cos(wt). Left: fades faster. Larger w: shakes faster."
        ++ (if wMode >= 2 then "  wn = " ++ nf 2 wn ++ "  zeta = " ++ nf 2 zeta else ""))
  ]
  ++ zipWith (\i s -> txt gx (gy + gh + 30 + i * 24) 15 c60 s) [0 ..]
     [ "Distance from origin = speed (circles), angle = damping."
     , "On the imaginary axis: keeps shaking (the limit)."
     , "On the real axis: no shaking."
     , "G(s) = 1/((s-p1)(s-p2)...). A pole is an s where the"
     , "denominator is 0. Control means moving poles to a good"
     , "place in the left half-plane (see 08 and 10)." ]
  where
    wd = envW env
    ht = envH env
    pl@(Plane planeX planeY planeW planeH) = planeOf env
    x0 = sx pl 0
    y0 = sy pl 0
    c70 = hsb 0 0 70
    c60 = hsb 0 0 60
    gx = wd * 0.5
    gy = ht * 0.2
    gw = wd * 0.46
    gh = ht * 0.45
    peakAbs = min 20 (U.foldl' (\m v -> max m (abs v)) 1 wResp)
    n = U.length wResp
    respPts = [ (remap (fromIntegral i) 0 (fromIntegral (max 1 (n - 1))) gx (gx + gw)
                , remap (clampF (-peakAbs) peakAbs v) (-peakAbs) peakAbs (gy + gh) gy)
              | (i, v) <- zip [0 :: Int ..] (U.toList wResp) ]
    maxRe = foldr (max . fst) (-1 / 0) wPoles
    verdict | maxRe < -1e-6 = "stable (fades away)"
            | maxRe > 1e-6 = "unstable (grows)"
            | otherwise = "limit (keeps shaking)"
    (mainRe, mainIm) = case wPoles of { (p : _) -> p; [] -> (0, 0) }
    wn = sqrt (mainRe * mainRe + mainIm * mainIm)
    zeta = if wn > 0 then -mainRe / wn else 0
    drawPole (re, im) =
      let c = hsb (if re < 0 then 120 else 0) 80 100
          x = sx pl re
          y = sy pl im
      in Pictures
           [ seg 4 c (x - 10, y - 10) (x + 10, y + 10)
           , seg 4 c (x - 10, y + 10) (x + 10, y - 10)
           , txt (x + 14) (y - 10) 14 white (nf 2 re ++ (if im >= 0 then " + " else " - ") ++ nf 2 (abs im) ++ "j") ]

-- 原点を中心とする円のうち、s 平面の枠の中にある部分だけを描く
circleInPlane :: Plane -> Float -> Float -> Float -> [Picture]
circleInPlane (Plane x y w h) cx cy r =
  let n = 96 :: Int
      pts = [ (cx + r * cos a, cy + r * sin a) | i <- [0 .. n], let a = tau * fromIntegral i / fromIntegral n ]
      inside (a, b) = a >= x && a <= x + w && b >= y && b <= y + h
  in [ Line [p, q] | (p, q) <- zip pts (drop 1 pts), inside p, inside q ]

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        pl = planeOf env
        hits = [ i | (i, (re, im)) <- zip [0 ..] (wPoles w), dist2 (mx, my) (sx pl re, sy pl im) < 25 ]
    in w { wDrag = foldl (\_ i -> i) (-1) hits }   -- 重なっていたら最後の極
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion p | wDrag w >= 0 -> dragTo (toP5 env p) (planeOf env) w
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' ->
    let m = fromEnum c - fromEnum '0'
    in setPoles (makePoles m) w { wMode = m, wDrag = -1 }
  _ -> w

-- ドラッグ：複素対は鏡写し、実数極は実軸の上
dragTo :: Point -> Plane -> World -> World
dragTo (mx, my) (Plane x y pw ph) w =
  let i = wDrag w
      ps = wPoles w
      re = clampF sigLo sigHi (remap mx x (x + pw) sigLo sigHi)
      im0 = clampF omLo omHi (remap my (y + ph) y omLo omHi)
      pIm = case drop i ps of { ((_, v) : _) -> v; [] -> 0 }
      im1 = if pIm == 0 && abs im0 < 0.3 then 0 else im0      -- 実軸の極は実軸上に留まる
      im2 = if abs im1 < 0.15 then 0.15 * (if pIm >= 0 then 1 else -1) else im1
      new | wMode w >= 2 && i < 2 = (re, abs im2) : (re, negate (abs im2)) : drop 2 ps
          | otherwise = take i ps ++ (re, 0) : drop (i + 1) ps
  in setPoles new w

-- ---------- やってみよう ----------
-- 1. 複素対を虚軸（σ = 0）の上に置く → 永遠にゆれる。右へ出すと？
-- 2. 対を原点から遠ざける（同じ角度のまま）→ 形は同じで速くなる
-- 3. 3 で実数極を −0.2 に置く → ゆっくりの極が「支配的」になり、ゆれが見えにくくなる
-- 4. 実数極を右半面に置く → ゆれずに発散する（指数関数的に育つ）

-- ==================================================
-- ここから下は「道具」です
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を gloss の座標に直す
screen :: Env -> Picture -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- マウスの位置を p5.js の座標に直す
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字：(x, y) は文字の下端。txt = 左揃え、txtC = 中央揃え
txt, txtC :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = Translate x y (Scale 1 (-1) (label 0 0 s c str))
txtC x y s c str = txt (x - textW s str / 2) y s c str

-- 文字の幅のおよその値
textW :: Float -> String -> Float
textW s str = fromIntegral (length str) * s * 0.72

-- 左上 (x, y)、幅 w、高さ h の塗りつぶした長方形
box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2) (y + h / 2) w h)

-- 太さ wt の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg wt c a b = Color c (thickLine wt a b)

-- 太さ wt の枠
frameBox :: Float -> Color -> Float -> Float -> Float -> Float -> Picture
frameBox wt c x y w h = Color c (thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 直径 d の円
dot :: Float -> Float -> Float -> Color -> Picture
dot x y d c = Color c (at (x, y) (disc (d / 2)))

-- 小数点以下 d 桁の文字列（p5.js の nf）
nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

-- ---------- 応答を計算する ----------
-- 極のリスト → 分母多項式の係数（昇べき：a0 + a1 s + … + s^n）
polyFromPoles :: [Pole] -> [Double]
polyFromPoles = go [1]
  where
    go a [] = a
    go a ((re0, im0) : ps)
      | abs im0 < 1e-9 = go (polyMul a [negate re, 1]) ps      -- (s − p)
      | otherwise = go (polyMul a [re * re + im * im, -2 * re, 1]) (dropConj ps)  -- (s − p)(s − p̄)
      where
        re = realToFrac re0 :: Double
        im = realToFrac im0 :: Double
        isConj (r, i) = abs (r - re0) < 1e-9 && abs (i + im0) < 1e-9
        dropConj qs = let (x, y) = break isConj qs in x ++ drop 1 y

polyMul :: [Double] -> [Double] -> [Double]
polyMul a b = foldr (\ai acc -> addL (map (* ai) b) (0 : acc)) [] a
  where
    addL (x : xs) (y : ys) = (x + y) : addL xs ys
    addL xs [] = xs
    addL [] ys = ys

-- 1/分母 のインパルス応答：y^(n) = −Σ a_i y^(i)、初期値 y^(n−1)(0) = 1
impulseResponse :: [Pole] -> Double -> U.Vector Float
impulseResponse ps t =
  let a = polyFromPoles ps
      n = length a - 1
      h = 0.004
      steps = round (t / h) :: Int
      every = max 1 (steps `div` 500)
      lim = max (-1e6) . min 1e6
      forceL xs = foldr seq xs xs
      loop !k zs acc
        | k >= steps = U.fromList (reverse acc)
        | otherwise =
            let top = negate (sum (zipWith (*) a zs))
                zs' = forceL (map lim (zipWith (+) zs (map (* h) (drop 1 zs ++ [top]))))
                v = case zs' of { (z : _) -> realToFrac z; [] -> 0 } :: Float
            in if k `mod` every == 0 then v `seq` loop (k + 1) zs' (v : acc)
               else loop (k + 1) zs' acc
  in loop (0 :: Int) (replicate (n - 1) 0 ++ [1]) []
