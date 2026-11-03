-- ============================================
-- 03 逆回転する車輪（エイリアシングを目で見る）
-- 映画で車輪が逆回転して見えるのは、カメラが 1 秒に 24 回しか「サンプリング」しないから
-- 左 = 本当の回転 / 右 = 1 秒 12 回だけ見た回転
-- マウス左右 = 回転の速さ / 上下 = スポークの数
-- ============================================
module Main (main) where

import Art
import Text.Printf (printf)

data World = World
  { wAngle      :: !Float   -- 本当の角度
  , wShown      :: !Float   -- 右の車輪の「見えている」角度
  , wLastSample :: !Float   -- 最後に見た時刻 [秒]
  }

sampleFps :: Float
sampleFps = 12              -- ★ 1 秒に何回見るか

main :: IO ()
main = runSketch (sketch "05-03 Aliasing Wheel" (World 0 0 0))
  { skBack = hsb 220 40 10
  , skStep = \env _ w -> pure (step env w)
  , skDraw = drawWorld
  }

rpmOf :: Env -> Float
rpmOf env = mapC (fst (mouseP env)) 0 (envW env) 0 200

spokesOf :: Env -> Int
spokesOf env = floor (mapC (snd (mouseP env)) 0 (envH env) 12 3)

step :: Env -> World -> World
step env w =
  let -- 本当の回転（1 フレーム = 1/60 秒）
      angle = wrapF tau (wAngle w + (rpmOf env / 60) * tau / 60)
  in -- 右側は 1 秒に sampleFps 回だけ角度を「見る」
     if envTime env - wLastSample w > 1 / sampleFps
       then w { wAngle = angle, wShown = angle, wLastSample = envTime env }
       else w { wAngle = angle }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ drawWheel env (ww * 0.28) (h * 0.5) (wAngle w) spokes "True rotation (continuous)"
  , drawWheel env (ww * 0.72) (h * 0.5) (wShown w) spokes
      ("Seen only " ++ show (round sampleFps :: Int) ++ " times a second")
  , text' env 20 40 16 (hsb 0 0 100)
      ("Speed " ++ show (round rpm :: Int) ++ " rpm (left/right)   Spokes " ++ show spokes
       ++ " (up/down)   Right wheel: " ++ dir)
  , text' env 20 68 15 (hsb 0 0 60)
      ("It turns " ++ nf1 (perSample * 180 / pi) ++ " deg per sample. Spoke gap is "
       ++ nf1 (360 / fromIntegral spokes) ++ " deg: past half of that it looks reversed")
  ]
  where
    ww = envW env
    h = envH env
    rpm = rpmOf env
    spokes = spokesOf env
    -- 見かけの回転方向を計算：スポークの対称性で mod をとる
    perSample = (rpm / 60) * tau / sampleFps   -- 1 サンプルあたりの本当の回転角
    symmetry = tau / fromIntegral spokes
    apparent = wrapF symmetry (wrapF symmetry perSample + symmetry * 1.5) - symmetry / 2
    dir | abs apparent < 0.01 = "looks stopped"
        | apparent > 0 = "looks forward"
        | otherwise = "looks REVERSED!"

drawWheel :: Env -> Float -> Float -> Float -> Int -> String -> Picture
drawWheel env cx cy a spokes name = Pictures
  [ at center (Color (hsb 0 0 70) (ring r 10))
  , Color (hsb 40 90 100) (Pictures
      [ spoke 5 (a + tau * fromIntegral i / fromIntegral spokes) | i <- [1 .. spokes] ])
  , Color (hsb 0 90 100) (spoke 8 a)          -- 1 本だけ目印
  , labelC (fst (pt env cx 0)) (snd (pt env 0 (cy + r + 40))) 18 (hsb 0 0 100) name
  ]
  where
    r = min (envW env) (envH env) * 0.2
    center@(gx, gy) = pt env cx cy
    -- p5.js と同じく画面で時計回りに回す（y が上向きなので sin の符号が反対）
    spoke weight ang = thickLine weight center (gx + r * cos ang, gy - r * sin ang)

-- ---------- やってみよう ----------
-- 1. ゆっくり速くしていき、右が「止まる」速さ、「逆転する」速さを見つけよう
-- 2. sampleFps を 24（映画）や 30（テレビ）にすると？
-- 3. 02 の波の話とまったく同じ現象。「2 倍より速く見ないと、本当の動きはわからない」

-- ---------- 道具 ----------

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

-- 小数 1 桁の文字列
nf1 :: Float -> String
nf1 = printf "%.1f"
