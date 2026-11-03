-- ============================================
-- 10 声で育つ木
-- 声の大きさで枝が開き、色が変わり、大声で花が咲く
-- クリックでマイク ON（マイクが使えないときはデモの信号で動く）
-- ============================================
module Main (main) where

import Art

data World = World
  { wMic   :: !(Maybe Mic)   -- まだ ON にしていなければ Nothing
  , wLive  :: !Bool          -- 本物のマイクの音が来ているか
  , wVol   :: !Float         -- なめらかにした音量
  , wBloom :: !Float         -- 花の咲き具合（0〜1）
  }

sensitivity :: Float
sensitivity = 0.25           -- ★ どのくらいの声で最大になるか

shrink :: Float
shrink = 0.7                 -- ★ 枝の縮む割合

main :: IO ()
main = runSketch (sketch "02-10 Voice Tree" (World Nothing False 0 0))
  { skBack  = hsb 220 40 10
  , skStep  = \_ _ w -> step w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

step :: World -> IO World
step w = do
  -- 音量を取って、なめらかにする
  (level, live) <- case wMic w of
    Nothing -> pure (0, False)
    Just mic -> (,) <$> micLevel mic <*> micLive mic
  let vol = lerpF (wVol w) level 0.2
      -- 大声が続くと花が咲き、静かになるとしぼむ
      bloom = lerpF (wBloom w) (if openOf vol > 0.5 then 1 else 0) 0.05
  pure w { wVol = vol, wBloom = bloom, wLive = live }

-- 音量 → 0〜1
openOf :: Float -> Float
openOf vol = clampF 0 1 (remap vol 0 sensitivity 0 1)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (tree : hud)
  where
    ww = envW env
    wh = envH env
    open = openOf (wVol w)
    angle = remap open 0 1 0.15 0.9   -- 枝の開く角度
    t = fromIntegral (envFrame env) * 0.01
    sway = (noise2 t 0.3 - 0.5) * 0.2 * open   -- 大声でゆれる
    tree = Pictures (branch w angle 0 (-wh / 2) sway (wh * 0.26) 0)
    left = -ww / 2 + 20
    hud = case wMic w of
      Nothing -> [label left (wh / 2 - 30) 18 white "Click to turn the mic ON"]
      Just _ ->
        label left (wh / 2 - 30) 18 white "Speak and the branches open. Keep it loud and flowers bloom!"
          : [label left (wh / 2 - 56) 16 (gray 0.7) "No mic found: demo signal" | not (wLive w)]

-- 枝を 1 本描いて、その先に小さい枝を描く
branch :: World -> Float -> Float -> Float -> Float -> Float -> Int -> [Picture]
branch w angle x y h len depth
  | len < 8 =
      [ Color body stem ]
        ++ [ at tip (Color (hsba (340 + d * 5) 60 100 (wBloom w * 0.9))
                       (disc ((6 + wBloom w * 14 + wVol w * 60) / 2)))
           | wBloom w > 0.05 ]
  | otherwise =
      Color body stem
        : branch w angle tx ty (h + angle) (len * shrink) (depth + 1)
        ++ branch w angle tx ty (h - angle * 0.8) (len * shrink) (depth + 1)
  where
    d = fromIntegral depth
    tip@(tx, ty) = (x + len * sin h, y + len * cos h)
    wd = clampF 1 14 (remap d 0 9 14 1)
    -- 静か = 青っぽい、うるさい = 暖かい色
    body = hsb (lerpF 200 20 (openOf (wVol w))) 60 (remap d 0 9 50 100)
    line' = thickLine wd (x, y) tip
    stem = if depth <= 5 then Pictures [line', at tip (disc (wd / 2))] else line'

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case (ev, wMic w) of
  (EventKey (MouseButton LeftButton) Down _ _, Nothing) -> do
    mic <- startMic
    pure w { wMic = Just mic }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. sensitivity（どのくらいの声で最大になるか）を変えて感度を調整しよう
-- 2. 枝の縮む割合 shrink を、音量で変えてみよう
-- 3. 高い声と低い声で色を変えたい → micSpectrum を使ってみよう
