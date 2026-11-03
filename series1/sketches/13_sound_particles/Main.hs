-- ============================================
-- 13 音に反応するパーティクル（自由制作のベース）
-- マウスの位置から粒が湧き出る
-- 声が大きいほど、たくさん・大きく・速く出る
-- クリックでマイク ON / C で消す / S で保存
-- ============================================
-- ★ のついた数字を変えて、自分だけの作品にしよう！
module Main (main) where

import Art
import Control.Monad (replicateM)

data Particle = Particle
  { pX, pY   :: !Float
  , pVx, pVy :: !Float
  , pSize    :: !Float
  , pHue     :: !Float
  , pLife    :: !Float
  , pTrail   :: ![Point]    -- 通ってきた場所（残像のため）
  }

data World = World
  { wMic   :: !(Maybe Mic)
  , wParts :: ![Particle]
  , wHue   :: !Float
  }

trailLength :: Int
trailLength = 8             -- ★ 残像の長さ。長いほど重い

trailFade :: Float
trailFade = 0.85            -- ★ 1 に近いほど残像が長く見える（p5.js の background の透明度 15 に相当）

background :: Color
background = hsb 0 0 5

main :: IO ()
main = runSketch (sketch "01-13 Sound Particles" (World Nothing [] 0))
  { skBack  = background
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

step :: Env -> Float -> World -> IO World
step env _ w = do
  -- 音量（マイク OFF のときは 0）
  vol <- maybe (pure 0) micLevel (wMic w)
  -- 音量に応じて、粒を生み出す数を決める
  let amount = floor (clampF 1 30 (remap vol 0 0.3 1 30)) :: Int     -- ★ 最大 30 個
  new <- replicateM amount (spawn (envMouse env) vol (wHue w))
  pure w
    { wParts = take 2200 (new ++ concatMap move (wParts w))
    , wHue = wrapF 360 (wHue w + 1)                                  -- ★ 色の変わる速さ
    }

spawn :: Point -> Float -> Float -> IO Particle
spawn (x, y) vol hue = do
  angle <- randF 0 tau
  speed0 <- randF 0.5 2
  size0 <- randF 4 10
  dh <- randF (-30) 30
  let speed = speed0 + vol * 30          -- ★ 音量で速くなる
  pure Particle
    { pX = x, pY = y
    , pVx = cos angle * speed
    , pVy = sin angle * speed
    , pSize = size0 + vol * 80           -- ★ 音量で大きくなる
    , pHue = wrapF 360 (hue + dh)
    , pLife = 100
    , pTrail = []
    }

move :: Particle -> [Particle]
move p
  | life <= 0 = []
  | otherwise = [p { pX = pX p + pVx p
                   , pY = pY p + pVy p
                   , pVy = pVy p - 0.03          -- ★ 重力（y は上向きが正。プラスにすると上へ）
                   , pLife = life
                   , pSize = pSize p * 0.98
                   , pTrail = take trailLength ((pX p, pY p) : pTrail p) }]
  where life = pLife p - 1.5                      -- ★ 寿命の減る速さ

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (map (drawParticle tailN) (reverse (wParts w)) ++ [hint])
  where
    -- 粒が多いときは残像を短くして、描く数を一定以下におさえる
    tailN = max 0 (min trailLength (7000 `div` max 1 (length (wParts w)) - 1))
    hint = case wMic w of
      Nothing -> labelC 0 (envH env / 2 - 40) 20 white "Click: mic ON, then speak and move the mouse"
      Just _ -> Blank

drawParticle :: Int -> Particle -> Picture
drawParticle tailN p = Pictures (reverse (zipWith dot fades (pos : take tailN (pTrail p))))
  where
    pos = (pX p, pY p)
    fades = iterate (* trailFade) 1
    dot fade q = Color (hsba (pHue p) 70 100 (fade * pLife p / 100))
                       (at q (disc (pSize p / 2)))         -- ★ 図形を変えてみよう

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | Nothing <- wMic w -> do                  -- マイクを ON
        mic <- startMic
        pure w { wMic = Just mic }
  EventKey (Char c) Down _ _
    | c == 'c' || c == 'C' -> pure w { wParts = map (\p -> p { pTrail = [] }) (wParts w) }  -- 残像を消す
    | c == 's' || c == 'S' -> do
        saveSvg "sound-particles" env (Pictures [Color background (rectangleSolid (envW env) (envH env)), drawWorld env w])
        pure w
  _ -> pure w

-- ---------- 自由制作のアイデア ----------
-- ・粒を星や四角や文字にする
-- ・マウスではなく画面の中心や、ランダムな場所から出す
-- ・09 のキーボードピアノと組み合わせて、鍵盤ごとに色を変える
-- ・12 のカメラと組み合わせて、明るいところから粒を出す

-- ---------- 道具 ----------

randF :: Float -> Float -> IO Float
randF a b = randomRIO (a, b)
