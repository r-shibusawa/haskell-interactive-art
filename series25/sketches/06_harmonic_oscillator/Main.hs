-- ============================================
-- 06 調和振動子
-- V = x²/2（ばね、ω = 1, ħ = m = 1）。エネルギーは等間隔のはしご En = n + 1/2、固有関数はエルミート関数
--   ψn(x) = Hn(x) e^{−x²/2} / √(2ⁿ n! √π)。いくつも重ねた「コヒーレント状態」は古典的なボールのように往復する
-- 1 = 固有状態 / 2 = 2 つの重ね合わせ / 3 = コヒーレント状態 / ↑↓ = n / ←→ = 振幅 α / K = 古典の確率分布 / スペース = 停止
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

nH :: Int
nH = 40              -- ★ 使う固有関数の数

mM :: Int
mM = 300

xR :: Float
xR = 6               -- x の範囲 −6 〜 6

type Field = U.Vector Float

data World = World
  { wMode    :: !Int
  , wN       :: !Int     -- 選んでいる n
  , wAlpha   :: !Float   -- コヒーレント状態の α
  , wT       :: !Float
  , wRun     :: !Bool
  , wClassic :: !Bool
  , wC       :: !Field   -- 係数 cn
  , wPRe     :: !Field   -- いまの ψ(x, t)
  , wPIm     :: !Field
  , wEavg    :: !Float   -- ⟨E⟩
  }

main :: IO ()
main = runSketch (sketch "25-06 Harmonic Oscillator" (compute (setCoeffs (World 1 0 2 0 True True U.empty U.empty U.empty 0))))
  { skBack  = hsb 170 30 9
  , skStep  = \_ dt w -> pure (compute (if wRun w then w { wT = wT w + dt } else w))
  , skDraw  = drawW
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

xAt :: Int -> Float
xAt i = -xR + 2 * xR * fromIntegral i / fromIntegral (mM - 1)

-- ★ 正規化されたエルミート関数を漸化式でつくる（階乗の大きな数を使わずにすむ）
--   ψ0 = π^{−1/4} e^{−x²/2}、ψ1 = √2 x ψ0、ψ(n+1) = √(2/(n+1)) x ψn − √(n/(n+1)) ψ(n−1)
hermiteColumn :: Float -> [Float]
hermiteColumn x =
  let p0 = pi ** (-0.25) * exp (-x * x / 2)
      p1 = sqrt 2 * x * p0
      go :: Int -> Float -> Float -> [Float]
      go n a b
        | n >= nH - 1 = []
        | otherwise =
            let fn = fromIntegral n
                c = sqrt (2 / (fn + 1)) * x * b - sqrt (fn / (fn + 1)) * a
            in c : go (n + 1) b c
  in p0 : p1 : go 1 p0 p1

-- table の並び：点 i ごとに nH 個
table :: Field
table = U.fromList (concatMap (hermiteColumn . xAt) [0 .. mM - 1])

-- 係数 cn を決める
setCoeffs :: World -> World
setCoeffs w =
  let zero = U.replicate nH 0
      r2 = sqrt 0.5
      a = wAlpha w
      -- ★ コヒーレント状態：cn = e^{−α²/2} αⁿ / √(n!)（漸化式 cn = c(n−1) · α / √n）
      coherent = U.fromList (take nH (scanl (\c n -> c * a / sqrt n) (exp (-a * a / 2)) [1 ..]))
      c = case wMode w of
        1 -> zero U.// [(wN w, 1)]
        2 -> zero U.// [(wN w, r2), (min (wN w + 1) (nH - 1), r2)]
        _ -> coherent
  in w { wC = c }

-- ψ(x, t) = Σ cn e^{−i(n + 1/2)t} ψn(x)
compute :: World -> World
compute w =
  let t = wT w
      active = [ (n, cr, ci, a)
               | n <- [0 .. nH - 1]
               , let a = wC w U.! n
               , abs a >= 1e-9
               , let th = -(fromIntegral n + 0.5) * t
               , let cr = a * cos th
               , let ci = a * sin th ]
      eavg = sum [ a * a * (fromIntegral n + 0.5) | (n, _, _, a) <- active ]
      at' f i = foldl' (\acc tr@(n, _, _, _) -> acc + f tr * table `U.unsafeIndex` (i * nH + n)) 0 active
  in w { wPRe = U.generate mM (at' (\(_, r, _, _) -> r))
       , wPIm = U.generate mM (at' (\(_, _, q, _) -> q))
       , wEavg = eavg }

drawW :: Env -> World -> Picture
drawW env w =
  let width = envW env
      height = envH env
      left = 60
      wd = width - 380
      base = height - 60
      eU = (height - 200) / 13
      sx x = left + (x + xR) / (2 * xR) * wd
      mode = wMode w
      nSel = wN w
      -- 放物線 V = x²/2 とはしご
      parab = curve env 2 (hsb 0 0 45) [ (sx x, base - min (x * x / 2) 13 * eU) | k <- [0 .. 240 :: Int], let x = -xR + fromIntegral k * 0.05 ]
      ladder = [ seg env 1 (hsba 0 0 100 (if n == nSel && mode /= 3 then 0.6 else 0.15)) (sx (-aa), base - e * eU) (sx aa, base - e * eU)
               | n <- [0 .. 12]
               , let e = fromIntegral n + 0.5
               , let aa = sqrt (2 * e) ]
      -- |ψ|²（⟨E⟩ の高さから）色 = 位相
      eavg = wEavg w
      yE = base - eavg * eU
      hs = eU * 3.2
      bars = [ seg env (wd / fromIntegral mM + 0.8) (hsb ((atan2 b a + pi) / tau * 360) 75 100) (x, yE) (x, yE - p * hs)
             | i <- [0 .. mM - 1]
             , let a = wPRe w U.! i
             , let b = wPIm w U.! i
             , let p = a * a + b * b
             , p * hs >= 0.4
             , let x = sx (xAt i) ]
      -- 古典的な確率分布 P(x) = 1 / (π √(A² − x²))：ボールが長くいる場所（折り返し点）ほど濃い
      classic =
        let aa = sqrt (2 * eavg)
            ks = takeWhile (< aa) (iterate (+ 0.02) (-aa + 0.02))
        in curve env 2 (hsb 55 90 100) [ (sx x, yE - min (1 / (pi * sqrt (max 1e-6 (aa * aa - x * x)))) 1.2 * hs) | x <- ks ]
      -- 古典的なボール（コヒーレント状態と比べる）x = √2 α cos t
      xc = sqrt 2 * wAlpha w * cos (wT w)
      ball = [ seg env 1 (hsba 0 0 100 0.4) (sx xc, base) (sx xc, yE - 2.4 * hs)
             , dotAt env white (sx xc) (base - xc * xc / 2 * eU) 16 ]
      -- 右：係数 |cn|² のはしご
      rx = width - 290
      ry = base
      coefRow n =
        let y = ry - fromIntegral n * ((ry - 140) / 24)
            a2 = sq (wC w U.! n)
        in [ box env (hsb 0 0 25) (rx + 40) (y - 8) 200 12
           , box env (hsb (170 + fromIntegral n * 6) 70 100) (rx + 40) (y - 8) (a2 * 200) 12
           , txt env (rx + 10) (y + 3) 14 (gray 0.6) (show n) ]
      mname = case mode of
        1 -> "eigenstate n = " ++ show nSel
        2 -> "superposition n = " ++ show nSel ++ ", " ++ show (nSel + 1)
        _ -> "coherent state alpha = " ++ nf 2 (wAlpha w)
  in Pictures $
       [ parab ] ++ ladder ++ bars
       ++ [ classic | wClassic w && mode == 1 ]
       ++ (if mode == 3 then ball else [])
       ++ [ txt env rx 112 14 (gray 0.7) "|cn|^2 of each level" ]
       ++ concatMap coefRow [0 .. 23]
       ++ [ txt env 20 26 16 white ("1/2/3: " ++ mname ++ "   Up/Down: n   Left/Right: alpha")
          , txt env 20 48 16 white ("K: classical " ++ (if wClassic w then "ON" else "off") ++ "   Space: " ++ (if wRun w then "running" else "paused") ++ "   <E> = " ++ nf 2 eavg)
          , txt env 20 70 14 (gray 0.6) (case mode of
              1 -> "An eigenstate's |psi|^2 never changes; only its phase turns. Large n nears the yellow classical curve."
              2 -> "Neighboring levels slosh left and right at w = E(n+1) - En = 1."
              _ -> "White ball = classical. A coherent state keeps its shape and swings with it. <E> = alpha^2 + 1/2.")
          ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "123" -> setCoeffs w { wMode = fromEnum c - fromEnum '0' }
    | toLower c == 'k' -> w { wClassic = not (wClassic w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> setCoeffs w { wN = min 12 (wN w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> setCoeffs w { wN = max 0 (wN w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> setCoeffs w { wAlpha = min 3 (wAlpha w + 0.25) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> setCoeffs w { wAlpha = max 0 (wAlpha w - 0.25) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wRun = not (wRun w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. n = 0 の「ゼロ点エネルギー」1/2。いちばん低い状態でも粒子は止まっていない。なぜ？（02 の不確定性）
-- 2. n = 12 にして、|ψ|² の山の数と黄色い古典分布を比べよう。折り返し点の外にも少ししみ出している
-- 3. 3 で α = 0 にすると？（基底状態そのもの）。α を大きくすると |cn|² の山はどこへ動く？
-- 4. setCoeffs で cn の符号を交互（(−1)ⁿ）にすると、ボールはどこから出発する？

-- ==================================================
-- ここから下は「道具」
-- ==================================================
sq :: Float -> Float
sq x = x * x

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

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
