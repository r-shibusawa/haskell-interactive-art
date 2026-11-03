-- ============================================
-- 04 コンスタレーション（星座図）と雑音
-- 位相と大きさの組み合わせで「1 回に何ビット送るか」を決める
-- QPSK = 4 点（2 ビット）、16QAM = 16 点（4 ビット）。点が多いほど速いが、雑音で隣と間違えやすい
-- 1 = QPSK / 2 = 16QAM / 3 = 64QAM / マウス左右 = 雑音の強さ
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Char (digitToInt)
import Data.List (foldl')
import Numeric (showFFloat)

data Rx = Rx !Float !Float !Bool   -- 受信した点 x, y と、正しく判定できたか

data World = World
  { wMode     :: !Int
  , wReceived :: ![Rx]    -- 受信した点（新しいものが先頭）
  , wErrors   :: !Int
  , wTotal    :: !Int
  }

perFrame, maxPoints :: Int
perFrame = 20             -- ★ 毎フレーム送る数
maxPoints = 600           -- ★ 画面に残す点の数

main :: IO ()
main = runSketch (sketch "06-04 Constellation" (World 1 [] 0 0))
  { skBack  = hsb 230 40 10
  , skStep  = \env _ w -> step env w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ 星座の点を作る：M 点
constellation :: Int -> [Point]
constellation 1 = [(cos (a * pi / 180), sin (a * pi / 180)) | a <- [45, 135, 225, 315]]
constellation m =
  [(grid i, grid j) | i <- [0 .. n - 1], j <- [0 .. n - 1]]
  where
    n = if m == 2 then 4 else 8 :: Int
    grid i = remap (fromIntegral i) 0 (fromIntegral (n - 1)) (-1) 1

modeName :: Int -> String
modeName 1 = "QPSK"
modeName 2 = "16QAM"
modeName _ = "64QAM"

noiseOf :: Env -> Float
noiseOf env = clampF 0 0.5 (remap (fst (mouseXY env)) 0 (envW env) 0 0.5)

-- 受信側：いちばん近い星座の点を選ぶ
nearest :: [Point] -> Point -> Int
nearest pts p = snd (foldl' closer (1 / 0, 0) (zip [0 ..] pts))
  where
    closer (bd, best) (i, q) =
      let d = dist2 q p in if d < bd then (d, i) else (bd, best)

-- 毎フレーム 20 個送る：ランダムな点 ＋ 雑音 → いちばん近い点に判定
step :: Env -> World -> IO World
step env w = do
  w' <- foldM (\acc _ -> sendOne acc) w [1 .. perFrame]
  pure w' { wReceived = take maxPoints (wReceived w') }
  where
    pts = constellation (wMode w)
    level = noiseOf env
    sendOne acc = do
      sent <- randomRIO (0, length pts - 1)
      gx <- gaussian
      gy <- gaussian
      let (x, y) = pts !! sent
          r = (x + gx * level, y + gy * level)
          ok = nearest pts r == sent
      pure acc
        { wReceived = Rx (fst r) (snd r) ok : wReceived acc
        , wTotal = wTotal acc + 1
        , wErrors = wErrors acc + (if ok then 0 else 1)
        }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Color (hsb 0 0 25) (Pictures
      [ Line [toG env (cx - s * 1.3) cy, toG env (cx + s * 1.3) cy]
      , Line [toG env cx (cy - s * 1.3), toG env cx (cy + s * 1.3)] ])
  , Pictures (map drawRx (reverse (wReceived w)))
  , Color (hsb 45 90 100) (Pictures [at (toG env (cx + x * s) (cy - y * s)) (disc 7) | (x, y) <- pts])
  , txt env (cx + s * 1.3 - 120) (cy + 22) 14 dim "I (cos part)"
  , txt env (cx + 10) (cy - s * 1.3 + 10) 14 dim "Q (sin part)"
    -- 右：成績
  , txt env rx0 ry 22 white' (modeName (wMode w) ++ ": " ++ show (length pts) ++ " points")
  , txt env rx0 (ry + 30) 18 white' ("= " ++ show bitsPerSymbol ++ " bits per symbol")
  , txt env rx0 (ry + 70) 18 white' ("Noise " ++ nf 2 (noiseOf env) ++ " (mouse X)")
  , txt env rx0 (ry + 110) 16 white'
      ("Errors " ++ show (wErrors w) ++ " / " ++ show (wTotal w) ++ " = "
       ++ nf 2 (100 * fromIntegral (wErrors w) / fromIntegral (max 1 (wTotal w))) ++ " %")
  , txt env rx0 (ry + 155) 15 dim "Yellow = sent points"
  , txt env rx0 (ry + 177) 15 dim "Cyan = received correctly"
  , txt env rx0 (ry + 199) 15 dim "Red = mistaken for a neighbor"
  , txt env 20 40 18 white' "1: QPSK  2: 16QAM  3: 64QAM   More points = faster, but weaker to noise"
  ]
  where
    pts = constellation (wMode w)
    bitsPerSymbol = round (logBase 2 (fromIntegral (length pts) :: Float)) :: Int
    cx = envW env * 0.4
    cy = envH env * 0.53
    s = min (envW env) (envH env) * 0.3
    rx0 = envW env * 0.67
    ry = envH env * 0.3
    white' = hsb 0 0 100
    dim = hsb 0 0 60
    drawRx (Rx x y ok) =
      Color (if ok then hsba 170 80 100 0.5 else hsba 0 80 100 0.5) (at (toG env (cx + x * s) (cy - y * s)) (disc 3))

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> (reset w) { wMode = digitToInt c }
  EventKey (MouseButton LeftButton) Down _ _ -> reset w
  _ -> w

reset :: World -> World
reset w = w { wReceived = [], wErrors = 0, wTotal = 0 }

-- ---------- やってみよう ----------
-- 1. 同じ雑音で QPSK と 64QAM の誤り率を比べよう。Wi-Fi が電波の弱い場所で遅くなる理由
-- 2. 雑音 0.5 の QPSK と、雑音 0.1 の 64QAM、どちらが速く正しく送れる？
-- 3. 256QAM（16×16）を追加してみよう（constellation の n を増やす）

-- ---------- 道具 ----------

-- 正規分布の乱数（p5.js の randomGaussian。ボックス・ミュラー法）
gaussian :: IO Float
gaussian = do
  u <- randomRIO (1e-6, 1)
  v <- randomRIO (0, 1)
  pure (sqrt (-2 * log u) * cos (tau * v))

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 小数を d 桁で文字にする（p5.js の nf）
nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
