-- | 音を鳴らす道具。波形を WAV ファイルに書き、OS の再生コマンドで鳴らす。
--   macOS は afplay、Linux は aplay を使う。無ければ音なしで動き続ける。
--   p5.js 版の Web Audio と違い、発音までに少し遅れがあり、連続音は短い音の連なりになる。
module Art.Sound
  ( Wave(..), playTone, playNote, playChord, noteFreq, midiFreq, scaleNote
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, getTemporaryDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (createProcess, proc, waitForProcess, std_err, std_out, StdStream(NoStream))

import Art.Runtime (isTestMode)

data Wave = Sine | Triangle | Square | Sawtooth deriving (Eq, Show)

running :: MVar Int
running = unsafePerformIO (newMVar 0)
{-# NOINLINE running #-}

player :: Maybe (FilePath, [String])
player = unsafePerformIO $ do
  a <- findExecutable "afplay"
  b <- findExecutable "aplay"
  pure $ case (a, b) of
    (Just p, _) -> Just (p, [])
    (_, Just p) -> Just (p, ["-q"])
    _ -> Nothing
{-# NOINLINE player #-}

-- | 音を 1 つ鳴らす：周波数 [Hz]、音量 0〜1、長さ [秒]、波形。同時に 8 音まで。
playTone :: Float -> Float -> Float -> Wave -> IO ()
playTone freq vol dur wave = unless (isTestMode || bad) $ forM_ player $ \(cmd, args) -> do
  ok <- modifyMVar running (\n -> pure (if n < 8 then (n + 1, True) else (n, False)))
  when ok $ void $ forkIO $ do
    _ <- try (do path <- waveFile freq vol dur wave
                 (_, _, _, h) <- createProcess (proc cmd (args ++ [path])) { std_out = NoStream, std_err = NoStream }
                 void (waitForProcess h)) :: IO (Either SomeException ())
    modifyMVar_ running (pure . subtract 1)
  where bad = isNaN freq || isInfinite freq || freq <= 0 || dur <= 0

-- | 音名で鳴らす： playNote "C4" 0.3 0.3 Triangle
playNote :: String -> Float -> Float -> Wave -> IO ()
playNote n = playTone (noteFreq n)

playChord :: [Float] -> Float -> Float -> Wave -> IO ()
playChord fs vol dur wave = mapM_ (\f -> playTone f vol dur wave) fs

-- | 'C4' や 'F#3' を周波数に変える
noteFreq :: String -> Float
noteFreq s = case s of
  (c : '#' : o) -> m c 1 o
  (c : 'b' : o) -> m c (-1) o
  (c : o) -> m c 0 o
  [] -> 440
  where
    m c d o = midiFreq (base c + d + (read (if null o then "4" else o) + 1) * 12)
    base :: Char -> Int
    base c = case c of { 'C' -> 0; 'D' -> 2; 'E' -> 4; 'F' -> 5; 'G' -> 7; 'A' -> 9; _ -> 11 }

midiFreq :: Int -> Float
midiFreq n = 440 * 2 ** (fromIntegral (n - 69) / 12)

-- | ペンタトニックの i 番目の音（根音 MIDI 番号から）
scaleNote :: Int -> Int -> Float
scaleNote root i =
  let (o, k) = i `divMod` 5
  in midiFreq (root + 12 * o + [0, 2, 4, 7, 9] !! k)

waveFile :: Float -> Float -> Float -> Wave -> IO FilePath
waveFile freq vol dur wave = do
  tmp <- getTemporaryDirectory
  let dir = tmp ++ "/haskell-art-sound"
      q :: Float -> Int
      q x = round (x * 100)
      path = dir ++ "/" ++ show wave ++ "_" ++ show (q freq) ++ "_" ++ show (q vol) ++ "_" ++ show (q dur) ++ ".wav"
  createDirectoryIfMissing True dir
  e <- doesFileExist path
  unless e (BL.writeFile path (BB.toLazyByteString (wav freq vol dur wave)))
  pure path

wav :: Float -> Float -> Float -> Wave -> BB.Builder
wav freq vol dur wave =
  let rate = 22050 :: Int
      n = ceiling (dur * fromIntegral rate) :: Int
      a = max 0 (min 1 vol) * 0.8
      phase i = fractional (freq * fromIntegral i / fromIntegral rate)
      fractional x = x - fromIntegral (floor x :: Int)
      osc p = case wave of
        Sine -> sin (2 * pi * p)
        Square -> if p < 0.5 then 1 else -1
        Sawtooth -> 2 * p - 1
        Triangle -> 4 * abs (p - 0.5) - 1
      env i = let t = fromIntegral i / fromIntegral rate :: Float
              in min 1 (t / 0.008) * max 0 (1 - t / dur) ** 1.2
      sample i = round (32000 * a * env i * osc (phase i)) :: Int16
      bytes = fromIntegral (n * 2)
  in mconcat
       [ BB.string7 "RIFF", BB.word32LE (36 + bytes), BB.string7 "WAVEfmt ", BB.word32LE 16
       , BB.word16LE 1, BB.word16LE 1, BB.word32LE (fromIntegral rate), BB.word32LE (fromIntegral rate * 2)
       , BB.word16LE 2, BB.word16LE 16, BB.string7 "data", BB.word32LE bytes
       , foldMap (BB.int16LE . sample) [0 .. n - 1] ]
