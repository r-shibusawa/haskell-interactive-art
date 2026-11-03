-- ============================================
-- 14 波動関数ブラシ
-- 上の段にポテンシャル V(x) と最初の波をマウスで描くと、シュレーディンガー方程式（03 と同じ分割演算子法）で動き出す
--   下の段は「時空図」：横 = x、縦 = 時間（上が今）。色 = 位相、明るさ = |ψ|。波の動きがそのまま絵になる。自由制作のベース
-- クリックで音をスタート / ドラッグ = 描く / V = ポテンシャルの筆 / W = 波の筆 / ←→ = 波の運動量 k0 / スペース = 停止・再生 / 1〜3 = V の例 / S = 保存 / C = 全部消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, replicateM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.&.))
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)
import Numeric (showFFloat)

nN :: Int
nN = 256              -- ★ 格子点の数

lL :: Float
lL = 100

dt :: Float
dt = 0.05

subSteps :: Int
subSteps = 4          -- ★ 1 フレームの歩数

rows :: Int
rows = 300            -- 時空図の行数（リングバッファ）

dx :: Float
dx = lL / fromIntegral nN

type Field = U.Vector Float

data World = World
  { wRe      :: !Field
  , wIm      :: !Field
  , wAmp     :: !Field            -- 描いた波の振幅
  , wV       :: !Field
  , wVc      :: !Field
  , wVs      :: !Field
  , wBrush   :: !Char             -- 'V' か 'W'
  , wK0      :: !Float
  , wRun     :: !Bool
  , wDrawing :: !Bool
  , wLastJ   :: !Int
  , wHead    :: !Int              -- 時空図のいちばん新しい行
  , wPg      :: !(U.Vector Word8) -- 時空図の画素（RGB）
  , wE       :: !Float            -- ⟨E⟩
  , wPmax    :: !Float
  , wAudio   :: !Bool
  , wNext    :: !Float            -- 次に音を鳴らす時刻
  }

main :: IO ()
main = runSketch (sketch "25-14 Psi Brush" world0)
  { skBack  = hsb 250 20 8
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

world0 :: World
world0 = clearAll (World U.empty U.empty U.empty U.empty U.empty U.empty 'W' 2 True False (-1) 0 U.empty 0 0 False 0)

ks, kc, kS :: Field
ks = U.generate nN (\j -> 2 * pi / lL * fromIntegral (if j < nN `div` 2 then j else j - nN))
kc = U.map (\k -> cos (-(k * k * dt) / 2)) ks
kS = U.map (\k -> sin (-(k * k * dt) / 2)) ks

clearAll :: World -> World
clearAll w =
  let n = fromIntegral nN :: Float
      amp = U.generate nN (\j -> exp (-sq (fromIntegral j - n * 0.3) / (2 * 64)))   -- 最初はガウス波束
  in makePsi (makeV w { wV = U.replicate nN 0, wAmp = amp, wPg = U.replicate (nN * rows * 3) 0 })

-- ★ V の例
presetV :: Int -> World -> World
presetV k w =
  let v j = let x = fromIntegral j / fromIntegral nN * lL - lL / 2
            in case k of
                 1 -> if abs x < 1.5 then 3 else 0
                 2 -> 0.004 * x * x
                 _ -> 1.5 * (1 + cos (x * 1.2)) * (if abs x < 25 then 1 else 0)
  in makeV w { wV = U.generate nN v }

-- V を位相の配列に。両端に高い壁（波が反対側から出てこないように）
makeV :: World -> World
makeV w =
  let v j a = if j < 3 || j > nN - 4 then 30 else a
      vs = U.imap v (wV w)
  in w { wVc = U.map (\a -> cos (-a * dt / 2)) vs, wVs = U.map (\a -> sin (-a * dt / 2)) vs }

-- 描いた振幅 amp に e^{ik0x} をかけて正規化
makePsi :: World -> World
makePsi w =
  let s = U.sum (U.map (\a -> a * a * dx) (wAmp w))
  in if s < 1e-9
       then makePsi w { wAmp = wAmp w U.// [(nN `div` 2, 1)] }
       else let nrm = 1 / sqrt s
                x j = fromIntegral j * dx
            in w { wRe = U.imap (\j a -> nrm * a * cos (wK0 w * x j)) (wAmp w)
                 , wIm = U.imap (\j a -> nrm * a * sin (wK0 w * x j)) (wAmp w) }

advance :: World -> World
advance w = runST $ do
  re <- U.thaw (wRe w)
  im <- U.thaw (wIm w)
  replicateM_ subSteps $ do
    rotate2 re im (wVc w) (wVs w)
    fft False re im
    rotate2 re im kc kS
    fft True re im
    rotate2 re im (wVc w) (wVs w)
  r <- U.unsafeFreeze re
  i <- U.unsafeFreeze im
  pure w { wRe = r, wIm = i }

-- 画面の配置
topY, leftX :: Float
topY = 100
leftX = 30

panelH :: Env -> Float
panelH env = envH env * 0.36

-- マウスで描く
paint :: Env -> World -> World
paint env w =
  let (mx, my) = mouseP env
      ph = panelH env
      base = topY + ph
      wd = envW env - 60
      j = max 0 (min (nN - 1) (round ((mx - leftX) / wd * fromIntegral (nN - 1))))
      v = clampF 0 1 ((base - my) / ph)
      a = if wLastJ w < 0 then j else wLastJ w
      spots = [min a j .. max a j]
  in if wBrush w == 'V'
       then makeV w { wV = wV w U.// [ (k, v * 8) | k <- spots ], wLastJ = j }
       else makePsi w { wAmp = wAmp w U.// [ (k, v) | k <- spots ], wLastJ = j }

-- 期待値 ⟨E⟩（音に使う）
measure :: World -> World
measure w =
  let probs = U.zipWith (\a b -> a * a + b * b) (wRe w) (wIm w)
      sp = U.sum probs
      sv = U.sum (U.zipWith (*) (wV w) probs)
      (tr, ti) = fftPure False (wRe w) (wIm w)
      kp = U.zipWith (\a b -> a * a + b * b) tr ti
      st = U.sum (U.zipWith (\k p -> k * k / 2 * p) ks kp)
      sk = U.sum kp
  in w { wPmax = U.foldl' max 0 probs, wE = st / max sk 1e-12 + sv / max sp 1e-12 }

-- 時空図に 1 行書く（新しい行が上）
addRow :: World -> World
addRow w =
  let hd = (wHead w - 1 + rows) `mod` rows
      pm = max (wPmax w) 1e-9
      pixel j =
        let a = wRe w `U.unsafeIndex` j
            b = wIm w `U.unsafeIndex` j
            p = a * a + b * b
        in hsv ((atan2 b a + pi) / tau) 0.8 (min 1 (sqrt (p / pm) * 1.1))
      pg = U.modify (\mv -> forM_ [0 .. nN - 1] $ \j -> do
             let (r, g, bl) = pixel j
                 k = (hd * nN + j) * 3
             MV.unsafeWrite mv k r
             MV.unsafeWrite mv (k + 1) g
             MV.unsafeWrite mv (k + 2) bl) (wPg w)
  in w { wHead = hd, wPg = pg }

stepW :: Env -> Float -> World -> IO World
stepW env _ w0 = do
  let w1 = if wDrawing w0 then paint env w0 else w0
      w2 = if wRun w1 && not (wDrawing w1 && wBrush w1 == 'W') then advance w1 else w1
      w3 = measure w2
      time = envTime env
      -- ★ 音の高さ = エネルギー。連続音の代わりに短い音をくり返す
      due = wAudio w3 && wRun w3 && time >= wNext w3
  when due (playTone (clampF 40 3000 (110 + 90 * clampF 0 12 (wE w3))) 0.3 0.24 Triangle)
  let w4 = if due then w3 { wNext = time + 0.25 } else w3
  pure (if wRun w4 then addRow w4 else w4)

drawW :: Env -> World -> Picture
drawW env w =
  let width = envW env
      height = envH env
      ph = panelH env
      base = topY + ph
      left = leftX
      wd = width - 60
      fN = fromIntegral nN :: Float
      px j = left + fromIntegral j / (fN - 1) * wd
      -- 時空図（いちばん新しい行が上に来るよう、読む行をずらす）
      sy = base + 20
      sh = height - sy - 20
      hd = wHead w
      spaceTime = pixelImage nN rows 1 $ \x y ->
        let r0 = y + hd
            r = if r0 >= rows then r0 - rows else r0
            k = (r * nN + x) * 3
        in (wPg w `U.unsafeIndex` k, wPg w `U.unsafeIndex` (k + 1), wPg w `U.unsafeIndex` (k + 2))
      stPic = at (pt env (left + wd / 2) (sy + sh / 2)) (Scale (wd / fN) (sh / fromIntegral rows) spaceTime)
      -- 上の段：V と |ψ|²
      vTop j = base - min (wV w U.! j / 8) 1 * ph
      potPic = Color (hsb 0 0 25) $ Pictures
        [ Polygon (map (uncurry (pt env)) [(px j, base), (px (j + 1), base), (px (j + 1), vTop (j + 1)), (px j, vTop j)])
        | j <- [0 .. nN - 2], vTop j < base - 0.3 || vTop (j + 1) < base - 0.3 ]
      pm = max (wPmax w) 1e-9
      bars = [ seg env (wd / fN + 0.8) (hsb ((atan2 b a + pi) / tau * 360) 75 100) (px j, base) (px j, base - p / pm * ph * 0.9)
             | j <- [0 .. nN - 1]
             , let a = wRe w U.! j
             , let b = wIm w U.! j
             , let p = a * a + b * b ]
      ampCurve = [ curve env 1 (hsba 0 0 100 0.5) [ (px j, base - wAmp w U.! j * ph) | j <- [0 .. nN - 1] ]
                 | wBrush w == 'W' && not (wRun w) ]
  in Pictures $
       [ stPic, potPic ]
       ++ bars ++ ampCurve
       ++ [ seg env 1 (hsb 0 0 50) (left, base) (left + wd, base)
          , txt env 20 26 16 white ((if wAudio w then "Sound on" else "Click for sound") ++ "   Brush: " ++ (if wBrush w == 'V' then "V potential" else "W wave") ++ " (V/W)   Left/Right: k0 = " ++ nf 1 (wK0 w))
          , txt env 20 48 16 white ("Space: " ++ (if wRun w then "running" else "paused") ++ "   1-3: V examples   S: save   C: clear   <E> = " ++ nf 2 (wE w))
          , txt env 20 70 14 (gray 0.6) "Draw on top (gray = V, colored hill = |psi|^2). Below is time piled up: top = now, bottom = past. Pitch = energy."
          ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w0 = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (_, my) = mouseP env
    in pure $ if my > topY && my < topY + panelH env
         then w { wDrawing = True, wLastJ = -1 }
         else w
  EventKey (MouseButton LeftButton) Up _ _ -> pure w0 { wDrawing = False }
  EventKey (Char c) Down _ _
    | toLower c == 's' -> saveSvg "psi_brush" env (drawW env w) >> pure w
    | otherwise -> pure (onChar c w)
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (makePsi w { wK0 = min 5 (wK0 w + 0.5) })
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (makePsi w { wK0 = max (-5) (wK0 w - 0.5) })
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wRun = not (wRun w) }
  _ -> pure w0
  where w = w0 { wAudio = True }

onChar :: Char -> World -> World
onChar c w
  | c >= '1' && c <= '3' = presetV (fromEnum c - fromEnum '0') w
  | otherwise = case toLower c of
      'v' -> w { wBrush = 'V' }
      'w' -> w { wBrush = 'W' }
      'c' -> clearAll w
      _ -> w

-- ---------- やってみよう ----------
-- 1. 2（調和ポテンシャル）で波束を投げると、時空図にジグザグの模様（往復）が描かれる
-- 2. 波の筆で離れた 2 つの山を描くと、ぶつかった所に干渉の縞が出る。k0 を変えると縞の細かさは？
-- 3. 3（周期的な山）は結晶のモデル。k0 を 0.5 ずつ変えて、通り抜ける k0 と はね返る k0 を探そう（バンド）
-- 4. 時空図の色の付け方（hsv の引数）や subSteps を変えて、自分だけの「量子の絵」を作り S で保存

-- ==================================================
-- ここから下は「道具」（FFT・色）
-- ==================================================
-- 複素数の配列に位相 (c + is) をかける
rotate2 :: MV.STVector s Float -> MV.STVector s Float -> Field -> Field -> ST s ()
rotate2 re im c s = go 0
  where
    go !j
      | j >= nN = pure ()
      | otherwise = do
          a <- MV.unsafeRead re j
          b <- MV.unsafeRead im j
          let cj = c `U.unsafeIndex` j
              sj = s `U.unsafeIndex` j
          MV.unsafeWrite re j (a * cj - b * sj)
          MV.unsafeWrite im j (a * sj + b * cj)
          go (j + 1)

twC, twS :: Field
twC = U.generate (nN `div` 2) (\k -> cos (2 * pi * fromIntegral k / fromIntegral nN))
twS = U.generate (nN `div` 2) (\k -> sin (2 * pi * fromIntegral k / fromIntegral nN))

-- re, im：長さ nN（2 のべき）の配列をその場で変換する。inv = True で逆変換（1/n で割る）
fft :: Bool -> MV.STVector s Float -> MV.STVector s Float -> ST s ()
fft inv re im = swaps 1 0 >> stage 2 >> when inv (divide 0)
  where
    n = nN
    next !j !bit | j .&. bit /= 0 = next (j `xor` bit) (bit `shiftR` 1)
                 | otherwise = j `xor` bit
    swaps !i !j                                     -- ビット反転の並べ替え
      | i >= n = pure ()
      | otherwise = do
          let j' = next j (n `shiftR` 1)
          when (i < j') (MV.unsafeSwap re i j' >> MV.unsafeSwap im i j')
          swaps (i + 1) j'
    stage !len                                      -- バタフライ演算
      | len > n = pure ()
      | otherwise = block len 0 >> stage (len * 2)
    block !len !i
      | i >= n = pure ()
      | otherwise = wing len i 0 >> block len (i + len)
    wing !len !i !k
      | k >= len `shiftR` 1 = pure ()
      | otherwise = do
          let a = i + k
              b = a + len `shiftR` 1
              t = k * (n `div` len)
              wr = twC `U.unsafeIndex` t
              wi = if inv then twS `U.unsafeIndex` t else negate (twS `U.unsafeIndex` t)
          rb <- MV.unsafeRead re b
          ib <- MV.unsafeRead im b
          ra <- MV.unsafeRead re a
          ia <- MV.unsafeRead im a
          let xr = rb * wr - ib * wi
              xi = rb * wi + ib * wr
          MV.unsafeWrite re b (ra - xr)
          MV.unsafeWrite im b (ia - xi)
          MV.unsafeWrite re a (ra + xr)
          MV.unsafeWrite im a (ia + xi)
          wing len i (k + 1)
    scaleBy = 1 / fromIntegral n :: Float
    divide !i
      | i >= n = pure ()
      | otherwise = MV.unsafeModify re (* scaleBy) i >> MV.unsafeModify im (* scaleBy) i >> divide (i + 1)

fftPure :: Bool -> Field -> Field -> (Field, Field)
fftPure inv re im = runST $ do
  r <- U.thaw re
  i <- U.thaw im
  fft inv r i
  (,) <$> U.unsafeFreeze r <*> U.unsafeFreeze i

-- 色相 h（0〜1）・彩度 s・明るさ v → (r, g, b)（0〜255）
hsv :: Float -> Float -> Float -> (Word8, Word8, Word8)
hsv h s v =
  let h6 = h * 6
      fl = floor h6 :: Int
      f = h6 - fromIntegral fl
      p = v * (1 - s)
      q = v * (1 - f * s)
      u = v * (1 - (1 - f) * s)
      b8 x = fromIntegral (max 0 (min 255 (round (x * 255) :: Int))) :: Word8
      tri a b c = (b8 a, b8 b, b8 c)
  in case fl `mod` 6 of
       0 -> tri v u p
       1 -> tri q v p
       2 -> tri p v u
       3 -> tri p q v
       4 -> tri u p v
       _ -> tri v p q

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
