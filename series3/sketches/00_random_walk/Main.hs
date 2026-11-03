-- ============================================
-- 00 ランダムウォーク（いちばん簡単な「生き物」）
-- 毎フレーム、ランダムな方向に1歩進むだけ。でも足あとは生き物らしく見える
-- クリックで歩く人を追加 / C で消す / マウス左右 = 歩はば / S で保存
-- ============================================
-- gloss は毎フレーム描きなおすので、足あとを覚えておく必要がある。
-- 新しい足あとは線のまま描き、古い足あとはピクセル画像（wCanvas）に焼きつけて残す。
module Main (main) where

import Art
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

data Walker = Walker
  { wkX   :: !Float
  , wkY   :: !Float
  , wkHue :: !Float
  }

-- 足あと 1 本（座標は p5.js と同じ：左上が原点、y は下向き）
data Seg = Seg
  { sgFrom :: !Point
  , sgTo   :: !Point
  , sgHue  :: !Float
  , sgPic  :: Picture       -- この線の絵（一度つくれば使いまわせる）
  }

type Rgb = (Word8, Word8, Word8)

data World = World
  { wWalkers :: ![Walker]
  , wSegs    :: ![Seg]             -- 線のまま描く足あと（新しいものが先頭）
  , wCount   :: !Int               -- wSegs の長さ
  , wCanvas  :: !(U.Vector Rgb)    -- 焼きつけた古い足あと
  , wBaked   :: Picture            -- wCanvas の絵（焼きつけたときだけ作りなおす）
  }

maxSegs :: Int
maxSegs = 6000             -- ★ 線のまま覚えておく足あとの数

canvasW, canvasH :: Int
canvasW = 356              -- 焼きつけ用の画像の大きさ
canvasH = 222

background :: Color
background = hsb 0 0 8

main :: IO ()
main = runSketch (sketch "03-00 Random Walk" (emptyWorld []))
  { skBack  = background
  , skStart = \env -> do
      w <- newWalker (envW env / 2) (envH env / 2)   -- 最初の1人は画面の真ん中から
      pure (emptyWorld [w])
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

emptyWorld :: [Walker] -> World
emptyWorld ws = World ws [] 0 blank' (canvasPicture blank')
  where blank' = U.replicate (canvasW * canvasH) (toRgb background)

newWalker :: Float -> Float -> IO Walker
newWalker x y = Walker x y <$> randomRIO (0, 360)

-- 歩はば：マウスの左右で 1〜8
stepSize :: Env -> Float
stepSize env = clampF 1 8 (remap (fst (mouseP5 env)) 0 (envW env) 1 8)

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = do
  moved <- mapM (walk env (stepSize env)) (wWalkers w)
  let segs = map snd moved
  pure (bakeOld w
    { wWalkers = map fst moved
    , wSegs = reverse segs ++ wSegs w
    , wCount = wCount w + length segs
    })

-- ★ ここが「ルール」：ランダムな方向に step だけ進む
walk :: Env -> Float -> Walker -> IO (Walker, Seg)
walk env step (Walker x y hue) = do
  angle <- randomRIO (0, tau)
  let nx = x + cos angle * step
      ny = y + sin angle * step
      seg = Seg (x, y) (nx, ny) hue (Color (hsba hue 60 100 0.6) (thickLine 2 (x, y) (nx, ny)))
  pure (Walker (clampF 0 (envW env) nx) (clampF 0 (envH env) ny) hue, seg)  -- 画面の外に出ないように

-- 足あとが増えすぎたら、古いものをピクセル画像に焼きつける
bakeOld :: World -> World
bakeOld w
  | wCount w <= maxSegs = w
  | otherwise =
      let (keep, old) = splitAt (maxSegs - 1000) (wSegs w)
          canvas = U.accum blend (wCanvas w) (concatMap stamps (reverse old))
      in w { wSegs = keep, wCount = length keep, wCanvas = canvas, wBaked = canvasPicture canvas }

-- 線の両端を、焼きつけ用の画像のマスに置く
stamps :: Seg -> [(Int, Rgb)]
stamps (Seg a b hue _) = [(pixelIndex p, toRgb (hsb hue 60 100)) | p <- [a, b]]

pixelIndex :: Point -> Int
pixelIndex (x, y) =
  let px = max 0 (min (canvasW - 1) (floor (x / 1280 * fromIntegral canvasW)))
      py = max 0 (min (canvasH - 1) (floor (y / 800 * fromIntegral canvasH)))
  in px + py * canvasW

blend :: Rgb -> Rgb -> Rgb
blend (r, g, b) (r2, g2, b2) = (mixByte r r2, mixByte g g2, mixByte b b2)
  where mixByte p q = fromIntegral ((fromIntegral p * 2 + fromIntegral q * 3) `div` (5 :: Int))

canvasPicture :: U.Vector Rgb -> Picture
canvasPicture c = pixelImage canvasW canvasH 1 (\x y -> c U.! (x + y * canvasW))

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Scale (envW env / fromIntegral canvasW) (envH env / fromIntegral canvasH) (wBaked w)
  , p5Space env (Pictures (map sgPic (reverse (wSegs w))))
  -- 説明（帯を描いて文字を見やすく）
  , Color background (rect 0 (envH env / 2 - 22) (envW env) 44)
  , hud env 28 16 white
      ("Walkers " ++ show (length (wWalkers w)) ++ "   Click: add   C: clear   Mouse X: step "
        ++ printf "%.1f" (stepSize env))
  ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | my < 44 -> pure w
    | otherwise -> do
        n <- newWalker mx my
        pure w { wWalkers = n : wWalkers w }
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> pure (emptyWorld [])
    | c `elem` "sS" -> saveSvg "walk" env (drawWorld env w) >> pure w
  _ -> pure w
  where (mx, my) = mouseP5 env

-- ---------- やってみよう ----------
-- 1. randomRIO (0, tau) を「前の角度 + randomRIO (-0.5, 0.5)」にすると「くねくね」歩く
--    （Walker に角度のフィールドを足す必要がある）
-- 2. 上下左右の4方向だけにすると？  fromIntegral k * pi / 2 （k は 0〜3 の乱数）
-- 3. 100人クリックで置いて、しばらく待つとどんな模様になる？

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）で描いた絵を、gloss の座標に置く
p5Space :: Env -> Picture -> Picture
p5Space env = Translate (-envW env / 2) (envH env / 2) . Scale 1 (-1)

-- マウスの位置を p5.js の座標で返す
mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左上から y の高さに文字を書く
hud :: Env -> Float -> Float -> Color -> String -> Picture
hud env y size = label (16 - envW env / 2) (envH env / 2 - y) size

toRgb :: Color -> Rgb
toRgb c = let (r, g, b, _) = rgbaOfColor c in (byte r, byte g, byte b)
  where byte v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))
