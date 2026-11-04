-- ============================================
-- 43-11 時間と周波数の双対（フーリエ変換）
-- 時間で狭い信号は周波数で広く、時間で広い信号は周波数で狭い（不確定性）
--   ガウス関数はガウス関数に写る。矩形 ↔ sinc、くし形 ↔ くし形（間隔が逆数）
--   幅 Δt と Δf を標準偏差で測ると Δt·Δf ≥ 1/(4π)：ガウスで等号
-- 操作：↑↓ = 幅 / ←→ = 周波数（変調）・間隔 / 1〜4 = 信号 / 簡易 DFT をリアルタイム計算
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

nS :: Int
nS = 256                  -- ★ 標本の数（DFT は N² なので増やしすぎ注意）

data World = World
  { wSig   :: !Int
  , wWidth :: !Float      -- ★ 信号の幅（時間 −1〜1 の中で）
  , wF0    :: !Float      -- ★ 変調の周波数（1〜4 の時）
  }

main :: IO ()
main = runSketch (sketch "43-11 Time-frequency duality" (World 1 0.12 0))
  { skBack  = hsb 250 40 10
  , skDraw  = draw
  , skEvent = event
  }

-- 三角関数の表
cosT, sinT :: U.Vector Float
cosT = U.generate nS (\k -> cos (tau * fromIntegral k / fromIntegral nS))
sinT = U.generate nS (\k -> sin (tau * fromIntegral k / fromIntegral nS))

sigName :: Int -> String
sigName k = case k of
  1 -> "Gaussian"
  2 -> "Rectangle"
  3 -> "Gaussian x cos (modulated)"
  _ -> "Comb (pulse train)"

-- ★ 信号 x(t)、t ∈ [−1, 1)
signal :: World -> Float -> Float
signal w t = case wSig w of
  1 -> gauss
  2 -> if abs t < wp then 1 else 0
  3 -> gauss * cos (tau * (8 + wF0 w * 4) * t)
  _ -> let d = max 0.04 wp                   -- 間隔
           r = wrapF d (t + 1)
       in if r < 2 / fromIntegral nS then 1 else 0
  where
    wp = wWidth w
    gauss = exp (-t * t / (2 * wp * wp))

-- 簡易 DFT（|X_k| を返す。k = −N/2 … N/2−1 の順に並べ替え）
dft :: U.Vector Float -> U.Vector Float
dft x =
  let bin k =
        let go !n !re !im
              | n >= nS = sqrt (re * re + im * im) / fromIntegral nS
              | otherwise =
                  let i = (k * n) `mod` nS
                      v = U.unsafeIndex x n
                  in go (n + 1) (re + v * U.unsafeIndex cosT i) (im - v * U.unsafeIndex sinT i)
        in go 0 0 0
      mag = U.generate nS bin
  in U.drop (nS `div` 2) mag U.++ U.take (nS `div` 2) mag

-- 幅（2 乗を重みにした標準偏差）
spread :: U.Vector Float -> U.Vector Float -> Float
spread vals pos =
  let wgt = U.map (\v -> v * v) vals
      tot0 = U.sum wgt
      tot = if tot0 == 0 then 1e-9 else tot0
      m = U.sum (U.zipWith (*) wgt pos) / tot
      v = U.sum (U.zipWith (\a p -> a * (p - m) * (p - m)) wgt pos) / tot
  in sqrt (max v 0)

draw :: Env -> World -> Picture
draw env w = view env $
  -- 時間領域
  [ box (hsb 0 0 35) lx ly lw lh
  , seg 1 (hsb 0 0 30) (lx, ly + lh * 0.8) (lx + lw, ly + lh * 0.8)
  , seg 1 (hsb 0 0 30) (lx + lw / 2, ly) (lx + lw / 2, ly + lh)
  , path 2.5 (hsb 10 80 100)
      [ (lx + (t + 1) / 2 * lw, ly + lh * 0.8 - v * lh * 0.7) | (t, v) <- U.toList (U.zip ts sig) ]
  , txt (lx + 10) (ly + 22) 14 (hsb 0 0 70) ("time x(t)   " ++ sigName (wSig w) ++ "   width dt = " ++ fmt 3 dt)
  -- 幅の帯
  , solid (hsbA 10 80 100 15) (lx + lw / 2 - dt * lw / 2) ly (dt * lw) lh
  -- 周波数領域
  , box (hsb 0 0 35) lx ry lw lh
  , seg 1 (hsb 0 0 30) (lx + lw / 2, ry) (lx + lw / 2, ry + lh)
  , path 2.5 (hsb 180 80 100)
      [ (lx + fromIntegral k / fromIntegral nS * lw, ry + lh * 0.9 - v / mmax * lh * 0.8) | (k, v) <- zip [0 :: Int ..] (U.toList mag) ] ]
  ++ concat [ [ seg 1 (hsb 0 0 30) (sx, ry + lh - 6) (sx, ry + lh)
              , txt (sx - 8) (ry + lh + 17) 14 (hsb 0 0 50) (show f) ]
            | f <- [-60, -40 .. 60 :: Int]
            , let sx = lx + (fromIntegral f / 0.5 + fromIntegral nS / 2) / fromIntegral nS * lw ]
  ++ [ txt (lx + 10) (ry + 22) 14 (hsb 0 0 70) ("frequency |X(f)|   width df = " ++ fmt 3 df)
     , solid (hsbA 180 80 100 15) (lx + lw / 2 - df / 128 * lw) ry (df / 64 * lw) lh
     , txt 20 32 16 (hsb 0 0 100)
         (sigName (wSig w) ++ " (1-4)  width " ++ fmt 2 (wWidth w) ++ " (Up/Down)"
          ++ (if wSig w == 3 then "  mod " ++ show (round (8 + wF0 w * 4) :: Int) ++ " Hz (L/R)" else "")
          ++ "  dt*df = " ++ fmt 4 (dt * df) ++ " (min 1/4pi = " ++ fmt 4 (1 / (4 * pi) :: Float) ++ ")")
     , txt 20 58 14 (hsb 0 0 60) "Up/Down changes the time width; the frequency width moves the opposite way. Gaussian (1) has the smallest product."
     , txt 20 78 14 (hsb 0 0 60) "Rectangle (2) -> sinc. Modulation (3) moves the peaks to +-f0. Comb (4) -> comb with the inverse spacing." ]
  where
    ts = U.generate nS (\n -> -1 + 2 * fromIntegral n / fromIntegral nS)
    sig = U.map (signal w) ts
    mag = dft sig
    fs = U.generate nS (\k -> (fromIntegral k - fromIntegral nS / 2) / 2)   -- 周波数：区間の長さが 2 なので刻みは 0.5
    dt = spread sig ts
    df = spread mag fs
    lx = 60
    ly = 110
    lw = envW env - 120
    lh = (envH env - 200) / 2 - 20
    ry = ly + lh + 60
    mmax0 = U.maximum mag
    mmax = if mmax0 == 0 then 1 else mmax0

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char k) Down _ _ | k `elem` "1234" -> w { wSig = fromEnum k - fromEnum '0' }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wWidth = min 0.6 (wWidth w * 1.25) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wWidth = max 0.01 (wWidth w / 1.25) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wF0 = min 8 (wF0 w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wF0 = max (-1) (wF0 w - 1) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. ガウスの幅を半分にすると Δf は何倍？ 積 Δt·Δf が変わらないことを確認
-- 2. 矩形の幅をとても狭くすると（インパルスに近づく）スペクトルは平らになる。広くすると？
-- 3. くし形の間隔を 2 倍にするとスペクトルの間隔は半分。「周期 ↔ 離散」の双対
-- 4. 自分の信号を signal に足す（三角波、ノコギリ波）。音の高さと倍音の関係が見える

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 左上が (x, y) の長方形の枠
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 左上が (x, y) の塗りつぶした長方形
solid :: Color -> Float -> Float -> Float -> Float -> Picture
solid c x y w h = Color c (Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)])

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: RealFloat a => Int -> a -> String
fmt n v = showFFloat (Just n) v ""
