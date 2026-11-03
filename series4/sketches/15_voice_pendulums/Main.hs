-- ============================================
-- 15 声でゆらす振り子たち（おまけ）
-- 13 の振り子に、声の大きさで「風」を吹かせる
-- 静かなら同じ動き。声でキックすると、ほんのわずかな差が広がってばらばらに
-- クリックでマイク ON・やり直し（マイクが使えないときはデモの信号で動く）
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

-- わずかな差を追うので、角度と速さは Double で計算する
data Pend = Pend
  { pA1, pA2, pV1, pV2 :: !Double
  , pHue  :: !Float
  , pPrev :: !(Maybe Point)      -- 前のフレームの先端の位置
  }

-- 残っている線分（色相、形）
data Stroke = Stroke !Float Picture

data World = World
  { wPends  :: ![Pend]
  , wVol    :: !Float
  , wMic    :: !(Maybe Mic)      -- まだ ON にしていなければ Nothing
  , wLive   :: !Bool             -- 本物のマイクの音が来ているか
  , wFrames :: ![[Stroke]]       -- フレームごとの線分（新しいものが先頭）
  }

count :: Int
count = 60                       -- ★ 本数

windPower :: Float
windPower = 0.004                -- ★ 声 → 風の強さ

fade :: Float
fade = 0.94                      -- ★ 残像の消え方（p5.js の透明度 6 に相当）

trailFrames :: Int
trailFrames = 50                 -- ★ 残像を何フレーム覚えておくか

main :: IO ()
main = runSketch (sketch "04-15 Voice Pendulums" (World startPends 0 Nothing False []))
  { skBack  = hsb 240 40 8
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

startPends :: [Pend]
startPends =
  [ Pend (pi * 0.5 + fromIntegral i * 0.00005) (pi * 0.5) 0 0 (fromIntegral i / fromIntegral count * 300) Nothing
  | i <- [0 .. count - 1] ]

step :: Env -> Float -> World -> IO World
step env _ w = do
  (level, live) <- case wMic w of
    Nothing -> pure (0, False)
    Just mic -> (,) <$> micLevel mic <*> micLive mic
  let vol = lerpF (wVol w) level 0.2
      kick = realToFrac (remapC vol 0 0.2 0 windPower)    -- ★ 声 → 風の強さ
      one p0 =
        let f = stepPend (1 / 6)
            p1 = f (f (f (f (f (f p0)))))
            p = p1 { pV1 = pV1 p1 + kick }    -- 風（全員に同じ力。でも結果はばらばらになる）
            (_, tip) = arms env p
            strokes = case pPrev p of
              Just old -> [Stroke (pHue p) (thickLine 1.5 old tip)]
              Nothing -> []
        in (p { pPrev = Just tip }, strokes)
      moved = map one (wPends w)
  pure w
    { wVol = vol, wLive = live
    , wPends = map fst moved
    , wFrames = take trailFrames (concatMap snd moved : wFrames w) }

-- 振り子の位置：(1つ目のおもり, 先端)
arms :: Env -> Pend -> (Point, Point)
arms env p = ((x1, y1), (x2, y2))
  where
    (ox, oy) = origin env
    l = min (envW env) (envH env) * 0.2
    x1 = ox + l * toF (sin (pA1 p))
    y1 = oy - l * toF (cos (pA1 p))
    x2 = x1 + l * toF (sin (pA2 p))
    y2 = y1 - l * toF (cos (pA2 p))

origin :: Env -> Point
origin env = sc env (envW env / 2) (envH env * 0.38)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  ( reverse (zipWith drawFrame (iterate (* fade) 0.5) (wFrames w)) ++
    -- 本体（うすく）
    map drawBody (wPends w) ++
  [ txt env 20 40 18 white
      (case wMic w of
         Nothing -> "Click to turn the mic ON"
         Just _ -> "Your voice makes the wind   Level " ++ fmt 3 (wVol w) ++ "   Click: restart")
  , if wLive w || null (wMic w) then Blank
    else txt env 20 66 14 (hsb 0 0 70) "No mic found: demo signal"
  ])
  where
    drawFrame alpha strokes = Pictures [Color (hsba hue 70 100 alpha) shape | Stroke hue shape <- strokes]
    drawBody p =
      let (a, b) = arms env p
      in Color (hsba (pHue p) 50 100 0.4) (Pictures [thickLine 1 (origin env) a, thickLine 1 a b])

-- 二重振り子の運動方程式（02 と同じ）に、ほんの少しの摩擦を足したもの
stepPend :: Double -> Pend -> Pend
stepPend dt p = p
  { pA1 = a1 + v1' * dt, pA2 = a2 + v2' * dt
  , pV1 = v1' * 0.9995, pV2 = v2' * 0.9995 }
  where
    g = 1; m1 = 1; m2 = 1; l1 = 1; l2 = 1
    a1 = pA1 p; a2 = pA2 p; v1 = pV1 p; v2 = pV2 p
    num1 = -g * (2 * m1 + m2) * sin a1 - m2 * g * sin (a1 - 2 * a2)
           - 2 * sin (a1 - a2) * m2 * (v2 * v2 * l2 + v1 * v1 * l1 * cos (a1 - a2))
    den1 = l1 * (2 * m1 + m2 - m2 * cos (2 * a1 - 2 * a2))
    num2 = 2 * sin (a1 - a2) * (v1 * v1 * l1 * (m1 + m2) + g * (m1 + m2) * cos a1 + v2 * v2 * l2 * m2 * cos (a1 - a2))
    den2 = l2 * (2 * m1 + m2 - m2 * cos (2 * a1 - 2 * a2))
    v1' = v1 + num1 / den1 * dt * 0.05
    v2' = v2 + num2 / den2 * dt * 0.05

onEvent :: Env -> Event -> World -> IO World
onEvent _ (EventKey (MouseButton LeftButton) Down _ _) w = case wMic w of
  Nothing -> do                 -- 1回目のクリック：マイクを ON
    mic <- startMic
    pure w { wMic = Just mic }
  Just _ -> pure w { wPends = startPends, wFrames = [] }   -- 2回目から：やり直し
onEvent _ _ w = pure w

-- ---------- やってみよう ----------
-- 1. 黙って見る → ずっとそろって動く。ひと声かける → ばらばらに。もう元には戻らない
-- 2. 風の強さ windPower を変える。小さくても結局ばらばらになる？
-- 3. 本数 count を 200 にして、残像を長く（fade を 0.98 に）すると絵画のようになる

-- ---------- 道具 ----------

toF :: Double -> Float
toF = realToFrac

-- p5.js 風の座標（左上が原点、y は下向き）を gloss の座標に直す
sc :: Env -> Float -> Float -> Point
sc env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size col s = let (gx, gy) = sc env x y in label gx gy size col s

remapC :: Float -> Float -> Float -> Float -> Float -> Float
remapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)

fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
