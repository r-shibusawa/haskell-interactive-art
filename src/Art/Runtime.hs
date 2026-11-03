module Art.Runtime (isTestMode, rememberBitmap, recallBitmap) where
import qualified Data.ByteString as B
import Data.IORef
import Graphics.Gloss.Data.Bitmap (BitmapData)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- | ART_TEST が設定されているか（テストでは音・マイク・カメラを本物に繋がない）
isTestMode :: Bool
isTestMode = unsafePerformIO (maybe False (const True) <$> lookupEnv "ART_TEST")
{-# NOINLINE isTestMode #-}

-- SVG 保存のために、直近のピクセル画像の中身を覚えておく（BitmapData の中身は取り出せないため）
bitmaps :: IORef [(BitmapData, B.ByteString)]
bitmaps = unsafePerformIO (newIORef [])
{-# NOINLINE bitmaps #-}

rememberBitmap :: BitmapData -> B.ByteString -> BitmapData
rememberBitmap bd bs = unsafePerformIO (atomicModifyIORef' bitmaps (\m -> (take 32 ((bd, bs) : m), bd)))
{-# NOINLINE rememberBitmap #-}

recallBitmap :: BitmapData -> Maybe B.ByteString
recallBitmap bd = unsafePerformIO (lookup bd <$> readIORef bitmaps)
{-# NOINLINE recallBitmap #-}
