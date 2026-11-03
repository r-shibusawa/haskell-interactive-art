-- ============================================
-- 05 無限井戸の固有状態
-- 幅 L の箱に閉じこめた粒子：固有関数 ψn(x) = √(2/L) sin(nπx/L)、エネルギー En = n²π²/(2L²) ∝ n²（ħ = m = 1）
--   重ね合わせ ψ(x,t) = Σ cn ψn(x) e^{−iEn t} は、エネルギーの差の速さで左右に揺れる（sloshing）
-- 左の準位をクリック = 係数を 0 → 0.5 → 1 / 1〜6 = その準位だけ / G = 局在した波束（30 準位の重ね合わせ） / ↑↓ = 時間の速さ / スペース = 停止 / R = 時刻 0
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

nM :: Int
nM = 30              -- ★ 使う準位の数

mM :: Int
mM = 300             -- 描く点の数

w1 :: Float
w1 = 0.5             -- ★ E1 を「1 秒あたりの角度」で（画面の時間の速さ）

traceMax :: Int
traceMax = 600

type Field = U.Vector Float

data World = World
  { wCRe   :: !Field     -- 係数 cn の実部（添字 0 は使わない）
  , wCIm   :: !Field
  , wSpeed :: !Float
  , wT     :: !Float
  , wRun   :: !Bool
  , wTrace :: ![Float]   -- ⟨x⟩ の記録（新しいものが先頭）
  , wPRe   :: !Field     -- いまの ψ(x, t)
  , wPIm   :: !Field
  , wMx    :: !Float     -- ⟨x⟩
  }

main :: IO ()
main = runSketch (sketch "25-05 Infinite Well" world0)
  { skBack  = hsb 200 30 10
  , skStep  = \_ dt w -> pure (stepW dt w)
  , skDraw  = drawW
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

world0 :: World
world0 = compute (normalizeC (World c0 (U.replicate (nM + 1) 0) 1 0 True [] U.empty U.empty 0))
  where c0 = U.replicate (nM + 1) 0 U.// [(1, 1), (2, 1)]

-- basis n i = ψn(xi)（L = 1）
basisTable :: Field
basisTable = U.generate ((nM + 1) * mM) $ \k ->
  let (n, i) = k `divMod` mM
  in if n == 0 then 0 else sqrt 2 * sin (fromIntegral n * pi * fromIntegral i / fromIntegral (mM - 1))

basis :: Int -> Int -> Float
basis n i = basisTable `U.unsafeIndex` (n * mM + i)

normalizeC :: World -> World
normalizeC w =
  let s0 = U.sum (U.drop 1 (U.map sq (wCRe w))) + U.sum (U.drop 1 (U.map sq (wCIm w)))
      (re, s) = if s0 < 1e-12 then (wCRe w U.// [(1, 1)], 1) else (wCRe w, s0)
      k = 1 / sqrt s
  in w { wCRe = U.map (* k) re, wCIm = U.map (* k) (wCIm w), wT = 0, wTrace = [] }

-- ★ ガウス波束を固有関数で展開：cn = ∫ ψn(x) g(x) dx
gaussPacket :: World -> World
gaussPacket w =
  let x0 = 0.3
      s = 0.05
      kk = 20
      xOf i = fromIntegral i / fromIntegral (mM - 1) :: Float
      g i = exp (-sq (xOf i - x0) / (4 * s * s))
      coef f n | n == 0 = 0
               | otherwise = foldl' (\acc i -> acc + basis n i * g i * f (kk * xOf i)) 0 [0 .. mM - 1]
  in normalizeC w { wCRe = U.generate (nM + 1) (coef cos), wCIm = U.generate (nM + 1) (coef sin) }

-- ψ(x, t) = Σ cn ψn(x) e^{−i n² w1 t}
compute :: World -> World
compute w =
  let t = wT w
      active = [ (n * mM, ar, ai)
               | n <- [1 .. nM]
               , let cr = wCRe w U.! n
               , let ci = wCIm w U.! n
               , cr /= 0 || ci /= 0
               , let th = -fromIntegral (n * n) * w1 * t
               , let c = cos th
               , let s = sin th
               , let ar = cr * c - ci * s
               , let ai = cr * s + ci * c ]
      sumAt f i = foldl' (\acc tr@(o, _, _) -> acc + f tr * basisTable `U.unsafeIndex` (o + i)) 0 active
      pr = U.generate mM (sumAt (\(_, a, _) -> a))
      pim = U.generate mM (sumAt (\(_, _, b) -> b))
      m1 = fromIntegral (mM - 1) :: Float
      mx = U.sum (U.imap (\i (a, b) -> fromIntegral i / m1 * (a * a + b * b) / m1) (U.zip pr pim))
  in w { wPRe = pr, wPIm = pim, wMx = mx }

stepW :: Float -> World -> World
stepW dt w0 =
  let w1' = compute (if wRun w0 then w0 { wT = wT w0 + dt * wSpeed w0 } else w0)
  in if wRun w1' then w1' { wTrace = take traceMax (wMx w1' : wTrace w1') } else w1'

drawW :: Env -> World -> Picture
drawW env w =
  let width = envW env
      height = envH env
      -- 左：エネルギー準位
      lx = 40
      lw = 240
      lb = height - 60
      eU = (height - 240) / 36
      m1 = fromIntegral (mM - 1) :: Float
      levelPic n =
        let fn = fromIntegral n :: Float
            y = lb - fn * fn * eU
            a2 = sq (wCRe w U.! n) + sq (wCIm w U.! n)
            hue = fn * 50
        in [ seg env 1 (hsba 0 0 100 0.3) (lx, y) (lx + lw, y)
           , curve env 2 (hsba hue 70 100 ((40 + a2 * 60) / 100))
               [ (lx + fromIntegral i / m1 * lw, y - basis n i * 12) | i <- [0, 6 .. mM - 1] ]
           , dotAt env (hsb hue 70 100) (lx + lw + 26) y (6 + a2 * 30)
           , txt env (lx + lw + 46) (y + 5) 14 (gray 0.8) ("n=" ++ show n ++ " E=" ++ show (n * n) ++ "E1") ]
      -- 右：|ψ(x,t)|²
      mx0 = 480
      mw = width - mx0 - 40
      mb = height * 0.62
      mh = height * 0.4
      probs = U.zipWith (\a b -> a * a + b * b) (wPRe w) (wPIm w)
      pmax = U.maximum probs
      hs = mh / max pmax 1
      bars = [ seg env (mw / fromIntegral mM + 0.8) (hsb ((atan2 (wPIm w U.! i) (wPRe w U.! i) + pi) / tau * 360) 75 100) (x, mb) (x, mb - p * hs)
             | i <- [0 .. mM - 1]
             , let p = probs U.! i
             , let x = mx0 + fromIntegral i / m1 * mw ]
      mx = wMx w
      -- ⟨x⟩(t) のグラフ（揺れの速さ = エネルギー差）
      gy = height - 40
      gh = height * 0.22
      traceAsc = reverse (wTrace w)
      tRev = tau / w1
  in Pictures $
       [ seg env 2 (hsb 0 0 60) (lx, lb) (lx, 120)
       , seg env 2 (hsb 0 0 60) (lx + lw, lb) (lx + lw, 120)
       , seg env 2 (hsb 0 0 60) (lx, lb) (lx + lw, lb) ]
       ++ concatMap levelPic [1 .. 6]
       ++ [ txt env lx 112 14 (gray 0.7) "Click a level (circle = |cn|^2)"
          , seg env 3 (hsb 0 0 60) (mx0, mb) (mx0, mb - mh)
          , seg env 3 (hsb 0 0 60) (mx0 + mw, mb) (mx0 + mw, mb - mh) ]
       ++ bars
       ++ [ seg env 1 (hsb 0 0 50) (mx0, mb) (mx0 + mw, mb)
          , seg env 2 (hsb 55 90 100) (mx0 + mx * mw, mb + 4) (mx0 + mx * mw, mb + 18)
          , frameBox env (hsb 0 0 35) mx0 (gy - gh) mw gh
          , curve env 2 (hsb 55 90 100) [ (mx0 + fromIntegral i / fromIntegral traceMax * mw, gy - v * gh) | (i, v) <- zip [0 :: Int ..] traceAsc ]
          , txt env mx0 (gy - gh - 8) 14 (gray 0.7) "<x> over time (top = right wall)"
          , txt env 20 26 16 white ("Click / 1-6: levels   G: packet   Up/Down: speed x " ++ nf 2 (wSpeed w) ++ "   Space: " ++ (if wRun w then "running" else "paused"))
          , txt env 20 48 16 white ("R: t = 0   t = " ++ nf 1 (wT w) ++ "   (revival period " ++ nf 1 tRev ++ ")")
          , txt env 20 70 14 (gray 0.6) "One level alone: |psi|^2 stands still. Two levels slosh at E2 - E1 = 3 E1."
          , txt env 20 90 14 (gray 0.6) "A packet falls apart, then returns after 2 pi / E1 (quantum revival)."
          ]

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> clickLevels env w
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '6' ->
        let n = fromEnum c - fromEnum '0'
        in normalizeC w { wCRe = U.replicate (nM + 1) 0 U.// [(n, 1)], wCIm = U.replicate (nM + 1) 0 }
    | toLower c == 'g' -> gaussPacket w
    | toLower c == 'r' -> w { wT = 0, wTrace = [] }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wSpeed = min 8 (wSpeed w * 1.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wSpeed = max 0.1 (wSpeed w / 1.5) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wRun = not (wRun w) }
  _ -> w

clickLevels :: Env -> World -> World
clickLevels env w =
  let (mx, my) = mouseP env
      lx = 40
      lw = 240
      lb = envH env - 60
      eU = (envH env - 240) / 36
      hit n = abs (my - (lb - fromIntegral (n * n) * eU)) < max 8 (eU * 1.2)
  in if mx < lx - 10 || mx > lx + lw + 120 then w else
     case filter hit [1 .. 6 :: Int] of
       [] -> w
       (n : _) ->
         -- いまの比率を保ったまま n 番目だけ 0 → 0.5 → 1 → 0（その後で正規化）
         let a = sqrt (sq (wCRe w U.! n) + sq (wCIm w U.! n))
             next | a < 0.25 = 0.5
                  | a < 0.75 = 1
                  | otherwise = 0
             high = [ (m, 0) | m <- [7 .. nM] ]
         in normalizeC w { wCRe = wCRe w U.// ((n, next) : high), wCIm = wCIm w U.// ((n, 0) : high) }

-- ---------- やってみよう ----------
-- 1. n = 1 と 2 を混ぜたときと、1 と 3 を混ぜたときで ⟨x⟩ は揺れる？（偶奇が同じだと ⟨x⟩ は真ん中のまま）
-- 2. 揺れの周期を測って 2π / ((m² − n²) w1) と比べよう
-- 3. G の波束は 復活の周期の半分で鏡に映した位置に、1/4 で 2 つに分かれて現れる（分数リバイバル）。止めて確かめよう
-- 4. gaussPacket の kk を 0 や 40 にすると、使われる準位（丸の大きさ）はどう変わる？

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

mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = pt env x y in label gx gy size c s

seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c (x1, y1) (x2, y2) = Color c (thickLine w (pt env x1 y1) (pt env x2 y2))

curve :: Env -> Float -> Color -> [Point] -> Picture
curve env w c ps = Color c (thickPath w (map (uncurry (pt env)) ps))

frameBox :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
frameBox env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rectLine gx gy w h)

dotAt :: Env -> Color -> Float -> Float -> Float -> Picture
dotAt env c x y d = Color c (at (pt env x y) (disc (d / 2)))
