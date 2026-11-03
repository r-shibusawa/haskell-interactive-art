-- ============================================
-- 07 音の出るボール
-- クリックするとボールが落ちて、床で跳ねるたびに音が鳴る
-- 左ほど低い音、右ほど高い音
-- ============================================
module Main (main) where

import Art

data Ball = Ball
  { bX, bY  :: !Float
  , bVy     :: !Float
  , bR      :: !Float
  , bNote   :: !String
  , bHue    :: !Float
  , bFlash  :: !Float     -- 光の輪（100 → 0）
  , bTrail  :: ![Float]   -- 通ってきた高さ（残像のため）
  }

newtype World = World [Ball]

-- ペンタトニック・スケール（どの音を組み合わせてもきれいに聞こえる音階）
notes :: [String]
notes = ["C3", "D3", "E3", "G3", "A3", "C4", "D4", "E4", "G4", "A4", "C5", "D5"]  -- ★

noteCount :: Int
noteCount = length notes

floorH :: Float
floorH = 30

main :: IO ()
main = runSketch (sketch "01-07 Sound Balls" (World []))
  { skBack  = hsb 230 40 12
  , skStep  = \env _ (World bs) -> World . limit <$> mapM (moveBall env) bs
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ボールが多すぎたら古いものから消す
limit :: [Ball] -> [Ball]
limit bs = if length bs > 25 then drop 1 bs else bs

-- ---- ボールを動かす（音を鳴らすので IO）----
moveBall :: Env -> Ball -> IO Ball
moveBall env b = do
  let vy0 = bVy b - 0.5                  -- ★ 重力（y は上向きが正）
      y0 = bY b + vy0
      flash0 = max 0 (bFlash b - 5)
      floorY = -envH env / 2 + floorH + bR b
      moved = b { bVy = vy0, bY = y0, bFlash = flash0, bTrail = take 4 (bY b : bTrail b) }
  if y0 >= floorY then pure moved else do
    -- 床にぶつかったら跳ね返る
    let hit = abs vy0 > 2
        -- ぶつかった強さで音の大きさを決める
        volume = clampF 0.05 0.5 (remap (abs vy0) 0 25 0.05 0.5)
    if hit then playNote (bNote b) volume 0.4 Triangle else pure ()   -- 音を鳴らす（音名, 大きさ, 長さ, 音色）
    pure moved
      { bY = floorY
      , bVy = vy0 * (-0.85)              -- ★ 跳ね返り（少しずつ弱くなる）
      , bFlash = if hit then 100 else flash0
      }

drawWorld :: Env -> World -> Picture
drawWorld env (World bs) = Pictures (map drawBall bs ++ zones ++ [help])
  where
    w = envW env
    h = envH env
    zoneW = w / fromIntegral noteCount
    -- ---- 床（音のゾーン）を描く ----
    zones = concat
      [ [ Color (hsb (fromIntegral i * 30) 60 50)
                (rect (-w / 2 + fromIntegral i * zoneW + (zoneW - 2) / 2) (-h / 2 + floorH / 2) (zoneW - 2) floorH)
        , labelC (-w / 2 + (fromIntegral i + 0.5) * zoneW) (-h / 2 + floorH / 2) 14 white n ]
      | (i, n) <- zip [0 :: Int ..] notes ]
    -- ---- 説明 ----
    help = label (-w / 2 + 20) (h / 2 - 36) 16 white "Click to drop a ball!"

drawBall :: Ball -> Picture
drawBall b = Pictures (trail ++ [body, flashRing])
  where
    pos y = at (bX b, y)
    -- 残像（p5.js の半透明の背景に相当）
    trail = reverse (zipWith (\fade y -> Color (hsba (bHue b) 70 100 fade) (pos y (disc (bR b))))
                             (iterate (* 0.6) 0.6) (bTrail b))
    body = Color (hsb (bHue b) 70 100) (pos (bY b) (disc (bR b)))
    -- ぶつかった瞬間に光の輪を出す
    flashRing
      | bFlash b > 0 = Color (hsba (bHue b) 50 100 (bFlash b / 100))
                             (pos (bY b) (ring (bR b + (100 - bFlash b) / 2) 4))
      | otherwise = Blank

onEvent :: Env -> Event -> World -> IO World
onEvent env ev (World bs) = case ev of
  EventKey (MouseButton LeftButton) Down _ (mx, my) -> do
    -- マウスの x 位置で音の高さを決める
    let i = max 0 (min (noteCount - 1) (floor (remap mx (-envW env / 2) (envW env / 2) 0 (fromIntegral noteCount))))
    r <- randomRIO (15, 35)
    let ball = Ball
          { bX = mx, bY = my, bVy = 0, bR = r
          , bNote = concat (take 1 (drop i notes))
          , bHue = fromIntegral i * 30
          , bFlash = 0
          , bTrail = []
          }
    pure (World (bs ++ [ball]))
  _ -> pure (World bs)

-- ---------- やってみよう ----------
-- 1. 跳ね返り -0.85 を -1 にすると？（ずっと跳ね続ける）
-- 2. 重力 0.5 を 0.2 にすると、ふんわり跳ねる
-- 3. notes の中身を変えて、別の音階にしてみよう
--    例） ["C4", "E4", "G4", "B4", "C5"] など
