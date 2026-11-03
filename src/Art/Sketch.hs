-- | スケッチの骨組み。p5.js の setup / draw / イベント関数に対応する。
--   座標は gloss のまま：画面中央が原点、x は右、y は上が正。
--
--   環境変数 ART_TEST=1 で起動すると、ウィンドウを開かずに決まった操作を
--   流して全フレームを評価し、例外・NaN・処理時間を報告する（tools/check.sh が使う）。
module Art.Sketch
  ( Env(..), Sketch(..), sketch, runSketch, isDown, saveSvg
  ) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (foldM, forM_, when)
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Clock (getMonotonicTime)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

import Art.Runtime (isTestMode)
import Art.Svg (pictureToSvg, pictureStats, png)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import qualified Graphics.Rendering.OpenGL as GL

-- | 毎フレーム渡される環境（p5.js の width, height, mouseX, frameCount などに相当）
data Env = Env
  { envW      :: !Float            -- ^ 画面の幅
  , envH      :: !Float            -- ^ 画面の高さ
  , envMouse  :: !(Float, Float)   -- ^ マウスの位置（中央が原点、y は上向き）
  , envDown   :: !Bool             -- ^ 左ボタンを押しているか
  , envFrame  :: !Int              -- ^ フレーム番号
  , envTime   :: !Float            -- ^ 起動してからの秒数
  , envKeys   :: !(Set.Set Key)    -- ^ いま押されているキー
  }

-- | キーが押されているか（keyIsDown に相当）
isDown :: Env -> Key -> Bool
isDown env k = Set.member k (envKeys env)

data Sketch w = Sketch
  { skTitle :: String
  , skBack  :: Color
  , skStart :: Env -> IO w                  -- ^ setup
  , skStep  :: Env -> Float -> w -> IO w    -- ^ 時間を進める（dt は秒）
  , skDraw  :: Env -> w -> Picture          -- ^ draw
  , skEvent :: Env -> Event -> w -> IO w    -- ^ キー・マウス
  }

-- | 何もしないスケッチ。必要なところだけレコード更新で書きかえて使う。
sketch :: String -> w -> Sketch w
sketch title w0 = Sketch
  { skTitle = title
  , skBack  = makeColorI 16 18 28 255
  , skStart = \_ -> pure w0
  , skStep  = \_ _ w -> pure w
  , skDraw  = \_ _ -> Blank
  , skEvent = \_ _ w -> pure w
  }

env0 :: Env
env0 = Env 1280 800 (0, 0) False 0 0 Set.empty

trackEvent :: Event -> Env -> Env
trackEvent ev env = case ev of
  EventMotion p -> env { envMouse = p }
  EventResize (w, h) -> env { envW = fromIntegral w, envH = fromIntegral h }
  EventKey (MouseButton LeftButton) st _ p ->
    env { envDown = st == Down, envMouse = p
        , envKeys = upd st (MouseButton LeftButton) (envKeys env) }
  EventKey k st _ _ -> env { envKeys = upd st k (envKeys env) }
  where upd Down = Set.insert
        upd Up   = Set.delete

-- | スケッチを実行する（テストモードならウィンドウを開かない）
runSketch :: Sketch w -> IO ()
runSketch sk
  | isTestMode = runHeadless sk
  | otherwise  = do
      w0 <- skStart sk env0
      grab <- lookupEnv "ART_GRAB"   -- 開発者用：90 フレーム目の実画面を PNG に保存して終了する
      playIO (InWindow (skTitle sk) (1280, 800) (60, 40)) (skBack sk) 60 (env0, w0)
        (\(e, w) -> pure (skDraw sk e w))
        (\ev (e, w) -> let e' = trackEvent ev e in (,) e' <$> skEvent sk e' ev w)
        (\dt (e, w) -> do
            let e' = e { envFrame = envFrame e + 1, envTime = envTime e + dt }
            forM_ grab (\path -> when (envFrame e' == 90) (grabWindow path (envW e') (envH e')))
            (,) e' <$> skStep sk e' dt w)

-- | 画面を SVG に保存する（p5.js の saveCanvas の代わり）
saveSvg :: String -> Env -> Picture -> IO ()
saveSvg name env pic
  | isTestMode = pure ()
  | otherwise = do
      t <- getCurrentTime
      let path = name ++ "-" ++ formatTime defaultTimeLocale "%H%M%S" t ++ ".svg"
      writeFile path (pictureToSvg (envW env) (envH env) black pic)
      putStrLn ("saved " ++ path)

foreign import ccall "exit" c_exit :: CInt -> IO ()

grabWindow :: FilePath -> Float -> Float -> IO ()
grabWindow path wf hf = do
  let w = round wf :: Int
      h = round hf :: Int
      n = w * h * 4
  allocaBytes n $ \ptr -> do
    GL.readBuffer GL.$= GL.FrontBuffers
    GL.readPixels (GL.Position 0 0) (GL.Size (fromIntegral w) (fromIntegral h)) (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
    bs <- B.packCStringLen (castPtr (ptr :: Ptr Word8), n)
    let flipped = B.concat [B.take (w * 4) (B.drop (y * w * 4) bs) | y <- [h - 1, h - 2 .. 0]]
    let opaque = snd (B.mapAccumL (\i v -> (i + 1 :: Int, if i `mod` 4 == 3 then 255 else v)) 0 flipped)
    B.writeFile path (png w h opaque)
  hFlush stdout
  c_exit 0

---------------------------------------------------------------- テストモード

-- | check.sh が流す操作。p5.js 版のテストと同じ順序のマウスとキー。
script :: [(Int, Event)]
script = zip [10, 20 ..] (concat [moves, click (0, 0), click (-140, 100), drag, keys, arrows, [motion (260, -100)]])
  where
    motion p = EventMotion p
    mouse st p = EventKey (MouseButton LeftButton) st (Modifiers Up Up Up) p
    moves = map motion [(-540, 300), (0, 0), (560, -300), (-340, -200), (360, 200)]
    click p = [motion p, mouse Down p, mouse Up p]
    drag = mouse Down (-340, 100) : map (\k -> motion (-340 + k * 40, 100 - k * 20)) [0 .. 7] ++ [mouse Up (-20, -60)]
    tap k = [EventKey k Down (Modifiers Up Up Up) (0, 0), EventKey k Up (Modifiers Up Up Up) (0, 0)]
    keys = concatMap (tap . Char) "123456zrctasnfg" ++ tap (SpecialKey KeySpace)
    arrows = concatMap (tap . SpecialKey) [KeyUp, KeyUp, KeyDown, KeyRight, KeyLeft]

runHeadless :: Sketch w -> IO ()
runHeadless sk = do
  svgDir <- lookupEnv "ART_SVG"
  let total = maximum (map fst script) + 120
      dt = 1 / 60 :: Float
      frame (e, w, !tot, !mx, !prims, !nans) n = do
        t0 <- getMonotonicTime
        let evs = [ev | (f, ev) <- script, f == n]
        (e1, w1) <- foldM (\(ea, wa) ev -> let eb = trackEvent ev ea in (,) eb <$> skEvent sk eb ev wa) (e, w) evs
        let e2 = e1 { envFrame = n, envTime = fromIntegral n * dt }
        w2 <- skStep sk e2 dt w1
        let pic = skDraw sk e2 w2
        (p, b) <- evaluate (pictureStats pic)
        t1 <- getMonotonicTime
        let ms = (t1 - t0) * 1000 :: Double
        forM_ svgDir $ \d -> when (n `elem` [60, total `div` 2, total]) $
          writeFile (d ++ "/f" ++ show n ++ ".svg") (pictureToSvg (envW e2) (envH e2) (skBack sk) pic)
        pure (e2, w2, tot + ms, max mx ms, max prims p, nans + b)
  r <- try (do w0 <- skStart sk env0
               foldM frame (env0, w0, 0 :: Double, 0 :: Double, 0 :: Int, 0 :: Int) [1 .. total])
  case r of
    Left (ex :: SomeException) -> do
      putStrLn ("== " ++ skTitle sk ++ "  EXC: " ++ take 300 (show ex))
      hFlush stdout
      exitWith (ExitFailure 1)
    Right (_, _, tot, mx, prims, nans) -> do
      printf "== %s  frames=%d avg=%.2fms max=%.1fms prims=%d nan=%d\n"
        (skTitle sk) total (tot / fromIntegral total) mx prims nans
      hFlush stdout
