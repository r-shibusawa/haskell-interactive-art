-- ============================================
-- 07 スペクトログラム（声の指紋）
-- 横 = 時間、縦 = 周波数、色 = 強さ。声や音楽を「周波数の地図」として見る
-- 母音ごとにちがう縞（フォルマント）、口笛は細い 1 本線、拍手は縦の帯
-- クリックでマイク ON / 1・2 で縦軸の範囲（マイクが使えないときはデモの信号）
-- ============================================
module Main (main) where

import Art
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as U

data World = World
  { wMic    :: !(Maybe Mic)        -- まだ ON にしていなければ Nothing
  , wLive   :: !Bool               -- 本物のマイクの音が来ているか
  , wMaxBin :: !Int                -- 表示する周波数の範囲（256 まで）
  , wCol    :: !Int                -- 次に描く列
  , wSmooth :: !(U.Vector Float)   -- なめらかにしたスペクトル
  , wImage  :: !(U.Vector Word8)   -- 描きためた地図（1 マス = 強さ 0〜255）
  }

cols, rows :: Int
cols = 427               -- 地図の横のマス数（1 マス = 画面の 3 ピクセル）
rows = 185               -- 地図の縦のマス数

topBar :: Float
topBar = 60              -- 上の文字の帯の高さ

binHz :: Float
binHz = 22050 / 512      -- 1 bin の周波数の幅（Art.Device は 22050 Hz で 512 点）

main :: IO ()
main = runSketch (sketch "05-07 Spectrogram" (World Nothing False 256 0 (U.replicate 256 0) (U.replicate (cols * rows) 0)))
  { skBack  = hsb 0 0 5
  , skStep  = \_ _ w -> step w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

step :: World -> IO World
step w = case wMic w of
  Nothing -> pure w
  Just mic -> do
    live <- micLive mic
    spectrum <- micSpectrum mic      -- 低い音〜高い音の強さ（256 個）
    let new = U.fromListN 256 (spectrum ++ repeat 0)
        -- 前のフレームと混ぜて、なめらかにする（p5.js の smoothingTimeConstant = 0.5）
        smooth = U.zipWith (\old v -> 0.5 * old + 0.5 * v) (wSmooth w) new
        column = [ ((r * cols + wCol w), toByte (cell (wMaxBin w) smooth r)) | r <- [0 .. rows - 1] ]
    pure w
      { wLive = live
      , wSmooth = smooth
      , wImage = wImage w U.// column
      , wCol = (wCol w + 1) `mod` cols       -- 時間が右へ進む
      }

-- 地図の r 行目（0 が上）に入る強さ。そのマスに入る bin のうち最大のものを使う
cell :: Int -> U.Vector Float -> Int -> Float
cell maxBin smooth r =
  let lo = (rows - 1 - r) * maxBin `div` rows
      hi = max lo (((rows - r) * maxBin `div` rows) - 1)
  in U.maximum (U.slice lo (hi - lo + 1) smooth)

-- ★ 強さをデシベルにして 0〜255 に直す（p5.js の analyser の -100〜-30 dB と同じ範囲）
toByte :: Float -> Word8
toByte m =
  let db = 20 * logBase 10 (max 1e-6 (m * 40 / 512))
  in round (255 * clampF 0 1 ((db + 100) / 70))

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures ([image, cursor] ++ ticks ++ [info] ++ demo)
  where
    ww = envW env
    h = envH env
    gh = h - topBar
    sx = ww / fromIntegral cols
    sy = gh / fromIntegral rows
    image = at (0, -topBar / 2) $ Scale sx sy $ pixelImage cols rows 1 $ \x y ->
      let v = fromIntegral (wImage w U.! (y * cols + x))
      in (lutR U.! v, lutG U.! v, lutB U.! v)
    -- 再生位置の線
    curX = fromIntegral ((wCol w + 1) `mod` cols) * sx
    cursor | null (wMic w) = Blank
           | otherwise = Pictures
               [ Color (hsb 0 0 5) (box env (curX + 1) topBar 6 gh)
               , Color (hsb 0 0 100) (Line [pt env curX topBar, pt env curX h])
               ]
    -- 周波数の目盛（1 bin ≈ 43 Hz）
    maxBin = fromIntegral (wMaxBin w) :: Float
    ticks =
      [ text' env 6 (topBar + remap (fromIntegral hz / binHz) 0 maxBin gh 0 - 4) 14 (hsb 0 0 100) (show hz ++ " Hz")
      | hz <- [0, 1000 .. floor (maxBin * binHz) :: Int] ]
    info = text' env 20 24 16 (hsb 0 0 100) $
      if null (wMic w)
        then "Click to turn the mic ON"
        else "Talk, sing, whistle, clap: each draws its own pattern   1: to 11 kHz   2: to 5.5 kHz"
    demo | not (null (wMic w)) && not (wLive w) = [text' env 20 48 14 (hsb 40 80 100) "No mic: demo signal"]
         | otherwise = []

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> case wMic w of
    Nothing -> do
      mic <- startMic
      pure w { wMic = Just mic }
    Just _ -> pure w
  EventKey (Char '1') Down _ _ -> pure w { wMaxBin = 256 }
  EventKey (Char '2') Down _ _ -> pure w { wMaxBin = 128 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 「あ・い・う・え・お」を順に言って、縞の位置（フォルマント）の違いを見よう
-- 2. 口笛を吹くと細い線。高くすると線が上に動く
-- 3. colorOf の式を変えて、好きな配色のスペクトログラムに

-- ---------- 道具 ----------

-- ★ 強さ（0〜255）→ 色：青 → 赤。0 は背景の色
colorOf :: Int -> (Word8, Word8, Word8)
colorOf 0 = (13, 13, 13)
colorOf i =
  let v = fromIntegral i / 255
      (r, g, b, _) = rgbaOfColor (hsb (240 * (1 - v)) 80 (v * 100))
      q c = round (c * 255)
  in (q r, q g, q b)

-- 毎回色を計算しないように、先に表にしておく
lutR, lutG, lutB :: U.Vector Word8
lutR = U.generate 256 (\i -> let (r, _, _) = colorOf i in r)
lutG = U.generate 256 (\i -> let (_, g, _) = colorOf i in g)
lutB = U.generate 256 (\i -> let (_, _, b) = colorOf i in b)

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標（中央が原点、y は上向き）に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- p5.js の map(v, a, b, c, d, true)：範囲からはみ出さない
mapC :: Float -> Float -> Float -> Float -> Float -> Float
mapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)

-- p5.js の座標で文字を書く
text' :: Env -> Float -> Float -> Float -> Color -> String -> Picture
text' env x y size col str = let (gx, gy) = pt env x y in label gx gy size col str

-- p5.js の rect(x, y, w, h)：左上の角と幅・高さで指定する
box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y bw bh = let (gx, gy) = pt env (x + bw / 2) (y + bh / 2) in rect gx gy bw bh
