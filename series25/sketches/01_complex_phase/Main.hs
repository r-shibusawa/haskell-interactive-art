-- ============================================
-- 01 複素数と位相
-- 波動関数 ψ は複素数。大きさ |ψ| と向き（位相 θ）をもつ「回る矢印」 ψ = |ψ| e^{iθ} = |ψ|(cos θ + i sin θ)
--   平面波 ψ(x, t) = e^{i(kx − ωt)} は x 方向にねじれたらせん。色 = 位相、明るさ = |ψ| で 1 本の帯にも描ける
-- 1 = 波 1 本 / 2 = 2 本の重ね合わせ（うなり） / ←→ = 2 本目の波数 k2 / ↑↓ = 1 本目の波数 k1 / D = 分散 ω = k²/2 ⇔ ω = k / マウス = 見る場所 x / スペース = 停止
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Numeric (showFFloat)

xs :: Int
xs = 360                   -- ★ 描く点の数

xRange :: Float
xRange = 60                -- 画面の幅に入れる x の範囲

data World = World
  { wK1   :: !Float        -- ★ 波数（1 あたりの位相の進み）
  , wK2   :: !Float
  , wMode :: !Int
  , wDisp :: !Bool         -- True：ω = k²/2（自由粒子、ħ = m = 1） / False：ω = k（光や音のように形が崩れない）
  , wT    :: !Float
  , wRun  :: !Bool
  }

main :: IO ()
main = runSketch (sketch "25-01 Complex Phase" (World 0.6 0.75 1 True 0 True))
  { skBack  = hsb 250 30 10
  , skStep  = \_ dt w -> pure (if wRun w then w { wT = wT w + dt * 2 } else w)
  , skDraw  = drawW
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

omega :: World -> Float -> Float
omega w k = if wDisp w then k * k / 2 else k

-- 位置 x、時刻 t での ψ（複素数を (re, im) で返す）
psi :: World -> Float -> (Float, Float)
psi w x =
  let th1 = wK1 w * x - omega w (wK1 w) * wT w
      th2 = wK2 w * x - omega w (wK2 w) * wT w
  in if wMode w == 2
       then ((cos th1 + cos th2) / 2, (sin th1 + sin th2) / 2)
       else (cos th1, sin th1)

phaseHue :: (Float, Float) -> Float
phaseHue (re, im) = (atan2 im re + pi) / tau * 360

drawW :: Env -> World -> Picture
drawW env w =
  let width = envW env
      height = envH env
      left = 360
      right = width - 40
      wd = right - left
      cyH = height * 0.36        -- らせんの中心と大きさ
      amp = height * 0.14
      n1 = fromIntegral (xs - 1) :: Float
      xOf i = left + fromIntegral i / n1 * wd
      xVal i = fromIntegral i / n1 * xRange
      idx = [0 .. xs - 1]
      vals = map (psi w . xVal) idx
      pairs = zip3 [1 :: Int ..] vals (drop 1 vals)
      -- らせん（斜めから見た 3D：横 = x、縦 = Re ψ、奥行き = Im ψ）
      helixPt i (re, im) = (xOf i + im * 10, cyH - re * amp * 0.85 + im * amp * 0.35)
      helix = [ seg env 3 (hsba (phaseHue b) 80 100 (if snd b > 0 then 1 else 0.45)) (helixPt (i - 1) a) (helixPt i b)
              | (i, a, b) <- pairs ]        -- 奥（Im < 0）は暗く
      shadow = curve env 1.5 (hsba 200 70 90 0.4) [ (xOf i, cyH - re * amp) | (i, (re, _)) <- zip idx vals ]
      -- 色の帯：色 = 位相、明るさ = |ψ|
      by = height * 0.62
      bh = 50
      band = [ box env (hsb (phaseHue p) 80 (norm2 p * 100)) (xOf i) by (wd / fromIntegral xs + 1) bh | (i, p) <- zip idx vals ]
      -- |ψ|² のグラフ（2 本ならうなり）
      gy = height * 0.9
      gh = height * 0.14
      probCurve = curve env 2 (hsb 55 80 100) [ (xOf i, gy - (re * re + im * im) * gh) | (i, (re, im)) <- zip idx vals ]
      -- 左：複素平面の回る矢印（マウスの位置 x）
      (mx, _) = mouseP env
      mi = max 0 (min (xs - 1) (floor ((mx - left) / wd * n1)))
      p@(pr, pi') = psi w (xVal mi)
      cx = 180
      cyc = height * 0.42
      bigR = 120
      hue = phaseHue p
      grid = hsb 0 0 35
      deg = atan2 pi' pr * 180 / pi
      vph = omega w (wK1 w) / wK1 w
      vg = if wDisp w then (wK1 w + wK2 w) / 2 else 1
      ringDots = [ let r = bigR + 8
                       aa = fromIntegral a * pi / 180 - pi
                   in dotAt env (hsb (fromIntegral a) 80 100) (cx + r * cos aa) (cyc - r * sin aa) 5
                 | a <- [0, 6 .. 354 :: Int] ]
  in Pictures $
       [ seg env 1 (hsb 0 0 40) (left, cyH) (right, cyH), shadow ]
       ++ helix ++ band
       ++ [ seg env 1 (hsb 0 0 40) (left, gy) (right, gy), probCurve
          , txt env left (by - 8) 14 (gray 0.7) "Color = phase theta   Brightness = |psi|"
          , txt env left (gy - gh - 6) 14 (gray 0.7) "|psi|^2 = density of finding it (flat for one plane wave)"
          , seg env 1 grid (cx - bigR - 20, cyc) (cx + bigR + 20, cyc)
          , seg env 1 grid (cx, cyc - bigR - 20) (cx, cyc + bigR + 20)
          , Color grid (at (pt env cx cyc) (ring bigR 1))
          , seg env 4 (hsb hue 80 100) (cx, cyc) (cx + pr * bigR, cyc - pi' * bigR)
          , dotAt env (hsb hue 80 100) (cx + pr * bigR) (cyc - pi' * bigR) 14
          -- Re と Im の成分
          , seg env 2 (hsb 200 70 90) (cx, cyc + bigR + 34) (cx + pr * bigR, cyc + bigR + 34)
          , seg env 2 (hsb 30 70 95) (cx - bigR - 34, cyc) (cx - bigR - 34, cyc - pi' * bigR)
          , seg env 1 (hsba 0 0 100 0.4) (xOf mi, 96) (xOf mi, height - 20)
          ]
       ++ ringDots   -- 位相の色相環
       ++ [ txt env (cx - 50) (cyc + bigR + 58) 15 white ("Re psi = " ++ nf 2 pr)
          , txt env 20 (cyc - bigR - 30) 15 white ("Im psi = " ++ nf 2 pi')
          , txt env (cx - 100) (cyc + bigR + 84) 15 white ("|psi| = " ++ nf 2 (norm2 p) ++ "  theta = " ++ nf 0 deg)
          , txt env (cx - 30) (cyc + bigR + 108) 15 white ("x = " ++ nf 1 (xVal mi))
          , txt env 20 26 16 white ("1/2: waves (" ++ show (wMode w) ++ ")   Up/Down: k1 = " ++ nf 2 (wK1 w) ++ "   Left/Right: k2 = " ++ nf 2 (wK2 w))
          , txt env 20 48 16 white ("D: " ++ (if wDisp w then "w = k^2/2 (matter wave)" else "w = k (keeps its shape)") ++ "   Space: " ++ (if wRun w then "running" else "paused") ++ "   Mouse: choose x")
          , txt env 20 70 14 (gray 0.6) ("Phase speed w/k = " ++ nf 2 vph ++ "   Beat envelope (group) speed = " ++ nf 2 vg ++ "   Matter wave: crest and lump differ")
          ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _ -> case toLower c of
    '1' -> w { wMode = 1 }
    '2' -> w { wMode = 2 }
    'd' -> w { wDisp = not (wDisp w) }
    _ -> w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wK1 = min 2 (wK1 w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wK1 = max 0.05 (wK1 w - 0.05) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wK2 = min 2 (wK2 w + 0.05) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wK2 = max 0.05 (wK2 w - 0.05) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wRun = not (wRun w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. マウスを右へ動かすと矢印はどう回る？ 時間を止めて（スペース）x だけ変えると、位相が k ずつ進むのがわかる
-- 2. 2 にして k1 と k2 を近づけると、うなりの間隔（2π / |k1 − k2|）が広がる
-- 3. D で ω = k に切り替えると、山とうなりが同じ速さで進む。ω = k²/2 だと山の速さは包絡線（群速度）のちょうど半分
-- 4. psi に 3 本目の波（k3）を足すと、うなりはどうなる？ 波をたくさん足すと「1 つのかたまり」= 波束（02）になる

-- ==================================================
-- ここから下は「道具」
-- ==================================================
nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = pt env x y in label gx gy size c s

seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c (x1, y1) (x2, y2) = Color c (thickLine w (pt env x1 y1) (pt env x2 y2))

curve :: Env -> Float -> Color -> [Point] -> Picture
curve env w c ps = Color c (thickPath w (map (uncurry (pt env)) ps))

box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

dotAt :: Env -> Color -> Float -> Float -> Float -> Picture
dotAt env c x y d = Color c (at (pt env x y) (disc (d / 2)))
