module Art.Runtime (isTestMode, rememberBitmap, recallBitmap) where
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Map.Strict as M
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- | ART_TEST が設定されているか（テストでは音・マイク・カメラを本物に繋がない）
isTestMode :: Bool
isTestMode = unsafePerformIO (maybe False (const True) <$> lookupEnv "ART_TEST")
{-# NOINLINE isTestMode #-}

-- SVG 保存のために、大きさごとに直近のピクセル画像を覚えておく（BitmapData の中身は取り出せないため）
bitmaps :: IORef (M.Map (Int, Int) B.ByteString)
bitmaps = unsafePerformIO (newIORef M.empty)
{-# NOINLINE bitmaps #-}

rememberBitmap :: Int -> Int -> B.ByteString -> B.ByteString
rememberBitmap w h bs = unsafePerformIO (atomicModifyIORef' bitmaps (\m -> (M.insert (w, h) bs m, bs)))
{-# NOINLINE rememberBitmap #-}

recallBitmap :: Int -> Int -> Maybe B.ByteString
recallBitmap w h = unsafePerformIO (M.lookup (w, h) <$> readIORef bitmaps)
{-# NOINLINE recallBitmap #-}
