-- ============================================
-- 06 花火（パーティクル）
-- クリックした場所で花火が爆発する
-- たくさんの「粒（パーティクル）」をリストで管理する
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)

-- 粒 1 つ
data Particle = Particle
  { pX, pY   :: !Float
  , pVx, pVy :: !Float
  , pHue     :: !Float
  , pSize    :: !Float     -- 直径
  , pLife    :: !Float     -- 寿命（100 → 0）
  , pTrail   :: ![Point]   -- 通ってきた場所（残像のため。新しいものが先頭）
  }

newtype World = World [Particle]

maxParticles :: Int
maxParticles = 850         -- ★ 同時に出せる粒の数（超えると古い粒から消える）

trailLength :: Int
trailLength = 8            -- ★ 残像の長さ（フレーム数）

trailFade :: Float
trailFade = 0.8            -- ★ 残像が 1 フレームごとに何倍に薄くなるか（p5.js の background の透明度 20 に相当）

main :: IO ()
main = runSketch (sketch "01-06 Fireworks" (World []))
  { skBack  = hsb 0 0 5
  , skStep  = \_ _ (World ps) -> pure (World (concatMap move ps))
  , skDraw  = \_ (World ps) -> Pictures (map drawParticle (reverse ps))
  , skEvent = onEvent
  }

-- 粒を 1 フレームぶん動かす。寿命が尽きたら消える（空のリストを返す）
move :: Particle -> [Particle]
move p
  | life <= 0 = []
  | otherwise = [p { pX = x, pY = y, pVx = vx, pVy = vy, pLife = life
                   , pTrail = take trailLength ((pX p, pY p) : pTrail p) }]
  where
    vy = pVy p - 0.05        -- ★ 重力で少しずつ下向きに加速（y は上向きが正）
    vx = pVx p * 0.99        -- ★ 空気抵抗で少しずつ減速
    x = pX p + vx            -- 速さの分だけ移動
    y = pY p + vy
    life = pLife p - 1.2     -- ★ 寿命を減らす

-- 寿命が減るほど透明になる。残像は古いほど薄く描く
drawParticle :: Particle -> Picture
drawParticle p = Pictures (zipWith dot (reverse (take trailLength fades)) (reverse (pTrail p)) ++ [dot 1 (pX p, pY p)])
  where
    fades = iterate (* trailFade) trailFade
    dot fade pos = Color (hsba (pHue p) 80 100 (fade * pLife p / 100)) (at pos (disc (pSize p / 2)))

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev (World ps) = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> do
    new <- explode pos
    pure (World (take maxParticles (new ++ ps)))
  _ -> pure (World ps)

-- 花火を 1 発爆発させる関数
explode :: Point -> IO [Particle]
explode (x, y) = do
  hue <- randF 0 360
  replicateM 150 $ do                  -- ★ 粒の数
    angle <- randF 0 tau               -- 飛んでいく方向（0〜360°）
    speed <- randF 1 7                 -- ★ 飛んでいく速さ
    dh <- randF (-20) 20
    size <- randF 3 8
    pure Particle
      { pX = x, pY = y
      , pVx = cos angle * speed        -- 横方向の速さ
      , pVy = sin angle * speed        -- 縦方向の速さ
      , pHue = wrapF 360 (hue + dh)
      , pSize = size
      , pLife = 100
      , pTrail = []
      }

-- ---------- やってみよう ----------
-- 1. 粒の数 150 を 300 にすると？（maxParticles も増やそう）
-- 2. 重力 pVy p - 0.05 を pVy p + 0.05 にすると？（上に落ちる！）
-- 3. EventMotion を受けて envDown env のときも explode すると、ドラッグ中も爆発する

-- ---------- 道具 ----------

randF :: Float -> Float -> IO Float
randF a b = randomRIO (a, b)
