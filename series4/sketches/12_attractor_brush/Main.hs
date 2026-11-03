-- ============================================
-- 12 アトラクター・ブラシ（自由制作のベース）
-- 粒たちがアトラクターの式に従って流れ、光の筋を描く
-- クリックした場所に粒を放つ。パラメータが変わると流れの形も変わり、音が鳴る
-- クリック = 粒を放つ / 1〜4 キー = パラメータを変える / C で消す / S で保存
-- ============================================
-- ★ のついた数字を変えて、自分だけの流れをつくろう！
module Main (main) where

import Art
import Control.Monad (replicateM)
import Numeric (showFFloat)

data Particle = Particle
  { pX, pY  :: !Float        -- 数学の座標
  , pPrev   :: !Point        -- 前のフレームの画面の位置
  , pAge    :: !Int
  }

-- 残っている線分（色相、形）
data Stroke = Stroke !Float Picture

data World = World
  { wA, wB, wC, wD :: !Float
  , wHueBase :: !Float
  , wParts   :: ![Particle]  -- 新しいものが先頭
  , wFrames  :: ![[Stroke]]  -- フレームごとの線分（新しいものが先頭）
  }

initial :: World
initial = World (-1.4) 1.6 1.0 0.7 200 [] []     -- ★ アトラクターのパラメータと色

speed :: Float
speed = 0.08                 -- ★ 流れる速さ

lifeSpan :: Int
lifeSpan = 600               -- ★ 寿命（フレーム）

strokeWidth :: Float
strokeWidth = 2              -- ★ 線の太さ

fade :: Float
fade = 0.97                  -- ★ 残像の消え方（1 に近いほど長く残る。p5.js の透明度 3 に相当）

trailFrames, strokeBudget, maxParticles :: Int
trailFrames = 90             -- ★ 残像を何フレーム覚えておくか
strokeBudget = 6500          -- ★ 残像の線分の上限（超えると古い線から消える）
maxParticles = 600           -- ★ 粒の数の上限（超えると古い粒から消える）

main :: IO ()
main = runSketch (sketch "04-12 Attractor Brush" initial)
  { skBack  = hsb 0 0 4
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

scaleOf :: Env -> Float
scaleOf env = min (envW env) (envH env) * 0.22

step :: Env -> World -> World
step env w = w
  { wParts = [p | (p, _) <- moved, pAge p <= lifeSpan]
  , wFrames = keepFrames strokeBudget (take trailFrames (map snd moved : wFrames w)) }
  where
    s = scaleOf env
    moved = map one (wParts w)
    one p =
      -- ★ 式：クリフォード・アトラクター。sin / cos を入れかえると別の流れに
      let nx = sin (wA w * pY p) + wC w * cos (wA w * pX p)
          ny = sin (wB w * pX p) + wD w * cos (wB w * pY p)
          -- いきなり飛ばず、少しずつ目標へ（なめらかな軌跡になる）
          x = lerpF (pX p) nx speed
          y = lerpF (pY p) ny speed
          pos = (x * s, -y * s)
          hue = wHueBase w + fromIntegral (pAge p) * 0.5
      in ( p { pX = x, pY = y, pPrev = pos, pAge = pAge p + 1 }
         , Stroke hue (thickLine strokeWidth (pPrev p) pos) )

-- 線分の合計が n 本を超えるところで古いフレームを捨てる
keepFrames :: Int -> [[Stroke]] -> [[Stroke]]
keepFrames n frames = case frames of
  (f : rest) | n > 0 -> f : keepFrames (n - length f) rest
  _ -> []

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  ( reverse (zipWith drawFrame (iterate (* fade) 0.7) (wFrames w)) ++
    -- 粒の先端（止まった粒も点として見える。p5.js の線の丸い端に相当）
    [ Color (hsba (wHueBase w + fromIntegral (pAge p) * 0.5) 70 100 0.7) (at (pPrev p) (disc (strokeWidth / 2)))
    | p <- wParts w ] ++
  [ txt env 16 28 16 white
      ("Click: particles   1-4: change   C: clear   S: save   N = " ++ show (length (wParts w))
        ++ "   a=" ++ fmt 2 (wA w) ++ " b=" ++ fmt 2 (wB w))
  ])
  where
    drawFrame alpha strokes = Pictures [Color (hsba hue 70 100 alpha) shape | Stroke hue shape <- strokes]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ (mx, my)
    | snd (mouseP5 env) >= 44 -> do
        let s = scaleOf env
            spawn = do
              -- クリック位置を数学の座標に戻して、少し散らして放つ
              dx <- randomRIO (-0.1, 0.1)
              dy <- randomRIO (-0.1, 0.1)
              pure (Particle (mx / s + dx) (-my / s + dy) (mx, my) 0)
        new <- replicateM 40 spawn
        note <- pick ["C4", "E4", "G4", "B4"]
        playNote note 0.2 0.6 Sine
        pure w { wParts = take maxParticles (new ++ wParts w) }
  EventKey (Char c) Down _ _
    | c == '1' -> randomParam (\v -> w { wA = v })
    | c == '2' -> randomParam (\v -> w { wB = v })
    | c == '3' -> randomParam (\v -> w { wC = v })
    | c == '4' -> randomParam (\v -> w { wD = v })
    | c == 'c' || c == 'C' -> pure w { wFrames = [] }
    | c == 's' || c == 'S' -> saveSvg "attractor-brush" env (drawWorld env w) >> pure w
  _ -> pure w
  where
    randomParam set = do
      v <- randomRIO (-2, 2)
      hue <- randomRIO (0, 360)
      note <- pick ["C3", "G3", "D4"]
      playNote note 0.3 1.0 Triangle
      pure (set v) { wHueBase = hue }

-- ---------- 自由制作のアイデア ----------
-- ・EventMotion でドラッグしながら粒を放つ（筆のように）
-- ・11 のマイクと合体：声の大きさで a を変える
-- ・04 のローレンツや 06 のスタンダード写像の式に入れかえる
-- ・粒の音：pY の値を音の高さにして、たまに鳴らす

-- ---------- 道具 ----------

-- リストからランダムに 1 つ選ぶ（音名のリスト用）
pick :: [String] -> IO String
pick xs = do
  i <- randomRIO (0, length xs - 1)
  pure (case drop i xs of
          (x : _) -> x
          [] -> "C4")

-- p5.js 風の座標（左上が原点、y は下向き）を gloss の座標に直す
sc :: Env -> Float -> Float -> Point
sc env x y = (x - envW env / 2, envH env / 2 - y)

mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size col s = let (gx, gy) = sc env x y in label gx gy size col s

fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
