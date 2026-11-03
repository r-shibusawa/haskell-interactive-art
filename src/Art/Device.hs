-- | マイクとカメラ。ffmpeg をパイプで呼び出して読む。
--   ffmpeg やデバイスが無いときは合成信号に切りかえ、スケッチは止まらない。
--   micLive / camLive で本物かどうかを調べられる。
module Art.Device
  ( Mic, startMic, micLevel, micSpectrum, micSamples, micLive
  , Camera, startCamera, cameraFrame, cameraSize, camLive, camGray, camImage
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Bits (shiftL, (.|.))
import Data.Complex
import Data.IORef
import Data.Int (Int16)
import Data.Word (Word8)
import GHC.Clock (getMonotonicTime)
import Graphics.Gloss (Picture)
import System.Directory (findExecutable)
import System.Info (os)
import System.IO (Handle)
import System.Process (createProcess, proc, std_err, std_out, StdStream(CreatePipe, NoStream))

import Art.Draw (pixelImage)
import Art.Runtime (isTestMode)

spawn :: [String] -> IO (Maybe Handle)
spawn args
  | isTestMode = pure Nothing
  | otherwise = do
      ff <- findExecutable "ffmpeg"
      case ff of
        Nothing -> pure Nothing
        Just p -> do
          r <- try (do (_, mh, _, _) <- createProcess (proc p (["-hide_banner", "-loglevel", "quiet"] ++ args)) { std_out = CreatePipe, std_err = NoStream }
                       pure mh)
          pure $ case r :: Either SomeException (Maybe Handle) of
            Right mh -> mh
            Left _ -> Nothing

---------------------------------------------------------------- マイク

data Mic = Mic { micBuf :: IORef [Float], micOn :: IORef Bool }

micN :: Int
micN = 512

-- | マイクを開く（失敗してもよい）
startMic :: IO Mic
startMic = do
  buf <- newIORef (replicate micN 0)
  on <- newIORef False
  let inp = if os == "darwin" then ["-f", "avfoundation", "-i", ":0"] else ["-f", "alsa", "-i", "default"]
  mh <- spawn (inp ++ ["-ac", "1", "-ar", "22050", "-f", "s16le", "-"])
  case mh of
    Nothing -> pure ()
    Just h -> void $ forkIO $ do
      let loop = do
            b <- B.hGet h (micN * 2)
            if B.length b < micN * 2 then writeIORef on False else do
              writeIORef on True
              writeIORef buf [ fromIntegral (s16 b i) / 32768 | i <- [0 .. micN - 1] ]
              loop
      void (try loop :: IO (Either SomeException ()))
  pure (Mic buf on)
  where s16 :: B.ByteString -> Int -> Int16
        s16 b i = fromIntegral (fromIntegral (BU.unsafeIndex b (2 * i)) .|. (fromIntegral (BU.unsafeIndex b (2 * i + 1)) `shiftL` 8 :: Int))

-- | 本物のマイクから音が来ているか
micLive :: Mic -> IO Bool
micLive = readIORef . micOn

-- | 直近の波形（512 サンプル、22050 Hz、-1〜1）
micSamples :: Mic -> IO [Float]
micSamples m = do
  live <- readIORef (micOn m)
  if live then readIORef (micBuf m) else do
    t <- realToFrac <$> getMonotonicTime
    let amp = 0.08 + 0.07 * sin (t * 1.3) :: Float
    pure [ amp * (sin (fromIntegral i * 0.09 + t * 5) + 0.5 * sin (fromIntegral i * 0.31)) | i <- [0 .. micN - 1 :: Int] ]

-- | 音量（RMS、0〜1）
micLevel :: Mic -> IO Float
micLevel m = do
  xs <- micSamples m
  pure (sqrt (sum (map (\x -> x * x) xs) / fromIntegral micN))

-- | 低い音〜高い音の強さ（256 個、0〜1）
micSpectrum :: Mic -> IO [Float]
micSpectrum m = do
  xs <- micSamples m
  let win = zipWith (\i x -> x * (0.5 - 0.5 * cos (2 * pi * fromIntegral i / fromIntegral micN))) [0 :: Int ..] xs
  pure (map (\z -> min 1 (magnitude z / 40)) (take 256 (fft (map (:+ 0) win))))

fft :: [Complex Float] -> [Complex Float]
fft [] = []
fft [x] = [x]
fft xs =
  let n = length xs
      split (a : b : r) = let (p, q) = split r in (a : p, b : q)
      split r = (r, [])
      (ev, od) = split xs
      e = fft ev
      o = fft od
      tw = [cis (-2 * pi * fromIntegral k / fromIntegral n) * v | (k, v) <- zip [0 :: Int ..] o]
  in zipWith (+) e tw ++ zipWith (-) e tw

---------------------------------------------------------------- カメラ

data Camera = Camera { camW :: Int, camH :: Int, camBuf :: IORef B.ByteString, camOn :: IORef Bool }

-- | カメラを開く。w × h に縮小し左右反転した RGB を受け取る（例：160 × 100）。
startCamera :: Int -> Int -> IO Camera
startCamera w h = do
  buf <- newIORef B.empty
  on <- newIORef False
  let inp = if os == "darwin"
              then ["-f", "avfoundation", "-framerate", "30", "-video_size", "640x480", "-i", "0"]
              else ["-f", "v4l2", "-i", "/dev/video0"]
      vf = "scale=" ++ show w ++ ":" ++ show h ++ ",hflip"
  mh <- spawn (inp ++ ["-vf", vf, "-pix_fmt", "rgb24", "-f", "rawvideo", "-"])
  case mh of
    Nothing -> pure ()
    Just hd -> void $ forkIO $ do
      let n = w * h * 3
          loop = do
            b <- B.hGet hd n
            if B.length b < n then writeIORef on False else do
              writeIORef buf b
              writeIORef on True
              loop
      void (try loop :: IO (Either SomeException ()))
  pure (Camera w h buf on)

cameraSize :: Camera -> (Int, Int)
cameraSize c = (camW c, camH c)

-- | 本物のカメラ映像が来ているか
camLive :: Camera -> IO Bool
camLive = readIORef . camOn

-- | 最新のフレーム（RGB、左上から順、長さ w × h × 3）。
--   カメラが使えないときは動く合成映像を返す。
cameraFrame :: Camera -> IO B.ByteString
cameraFrame c = do
  live <- readIORef (camOn c)
  if live then readIORef (camBuf c) else do
    t <- realToFrac <$> getMonotonicTime
    pure (synthetic (camW c) (camH c) t)

synthetic :: Int -> Int -> Float -> B.ByteString
synthetic w h t = fst (B.unfoldrN (w * h * 3) (\i -> Just (px i, i + 1)) 0)
  where
    fw = fromIntegral w :: Float
    fh = fromIntegral h :: Float
    px :: Int -> Word8
    px i =
      let (j, ch) = i `divMod` 3
          (y, x) = j `divMod` w
          u = fromIntegral x / fw - 0.5
          v = fromIntegral y / fh - 0.5
          bx = 0.28 * cos (t * 0.7)
          by = 0.22 * sin (t * 0.9)
          d = (u - bx) * (u - bx) + (v - by) * (v - by)
          base = 0.25 + 0.2 * sin (u * 9 + t) * cos (v * 7 - t * 0.6) + 0.7 * exp (-d * 40)
          k = [1.0, 0.85, 0.7] !! ch
      in fromIntegral (max 0 (min 255 (round (base * k * 255) :: Int)))

-- | (x, y) の明るさ（0〜1）
camGray :: Camera -> B.ByteString -> Int -> Int -> Float
camGray c f x0 y0 =
  let w = camW c
      x = max 0 (min (w - 1) x0)
      y = max 0 (min (camH c - 1) y0)
      i = (y * w + x) * 3
  in if B.length f < (w * camH c * 3) then 0 else
     (0.299 * g i + 0.587 * g (i + 1) + 0.114 * g (i + 2)) / 255
  where g k = fromIntegral (BU.unsafeIndex f k) :: Float

-- | フレームを k 倍で画面中央に置く
camImage :: Camera -> B.ByteString -> Float -> Picture
camImage c f k =
  let w = camW c
  in pixelImage w (camH c) k (\x y -> let i = (y * w + x) * 3 in (B.index f i, B.index f (i + 1), B.index f (i + 2)))
