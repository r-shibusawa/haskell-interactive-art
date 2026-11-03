-- ============================================
-- 11 声でカオスを操る
-- 声の大きさが r になる。静かなら規則的な点滅、大きな声でカオスの嵐
-- 画面には「いまの x の値」の粒が飛び、下には最近の値のヒストグラム
-- クリックでマイク ON（マイクが使えないときはデモの信号で動く）
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

data Particle = Particle
  { pX, pY, pVY :: !Float         -- 位置は p5.js と同じく y 下向き
  , pHue        :: !Float
  , pLife       :: !Float
  }

data World = World
  { wMic   :: !(Maybe Mic)        -- まだ ON にしていなければ Nothing
  , wLive  :: !Bool               -- 本物のマイクの音が来ているか
  , wR     :: !Float
  , wX     :: !Double             -- 長く動かしても 0 に落ちないよう Double で計算する
  , wVol   :: !Float
  , wParts :: ![Particle]
  , wHist  :: !(U.Vector Float)   -- どの値によく来るか（50 区間）
  , wTrace :: ![Float]            -- 最近の x の画面位置（残像用。新しいものが先頭）
  }

bins :: Int
bins = 50

sensitivity :: Float
sensitivity = 0.25                -- ★ どのくらいの声で r = 4 になるか

ghostFade :: Float
ghostFade = 0.6                   -- ★ 残像の薄くなり方（p5.js の background の透明度 40 に相当）

main :: IO ()
main = runSketch (sketch "04-11 Voice Chaos" (World Nothing False 2.8 0.3 0 [] (U.replicate bins 0) []))
  { skBack  = hsb 230 40 10
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

step :: Env -> Float -> World -> IO World
step env _ w = do
  (level, live) <- case wMic w of
    Nothing -> pure (0, False)
    Just mic -> (,) <$> micLevel mic <*> micLive mic
  vy <- randomRIO (-3, -1)
  let vol = lerpF (wVol w) level 0.15
      r = remapC vol 0 sensitivity 2.8 4.0     -- ★ 声の大きさ → r
      -- 毎フレーム x を更新して粒を出す
      x = realToFrac r * wX w * (1 - wX w)
      fx = realToFrac x :: Float
      px = remap fx 0 1 80 (envW env - 80)
      new = Particle px (envH env * 0.45) vy (remap fx 0 1 180 360) 100
      idx = max 0 (min (bins - 1) (floor (fx * 49.99)))
      moved = [ p { pY = pY p + pVY p, pLife = pLife p - 2 } | p <- new : wParts w ]
  pure w
    { wLive = live, wVol = vol, wR = r, wX = x
    , wParts = filter (\p -> pLife p > 0) moved
    , wHist = U.map (* 0.99) (U.accum (+) (wHist w) [(idx, 1)])   -- 古い記録はゆっくり忘れる
    , wTrace = take 5 (px : wTrace w)
    }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  ( concatMap drawPart (reverse (wParts w)) ++
    -- いまの x（残像つき）
    reverse (zipWith drawDot (iterate (* ghostFade) 1) (wTrace w)) ++
    -- ヒストグラム（どの値によく来るか）
    [ drawBar i v | (i, v) <- zip [0 ..] (U.toList (wHist w)) ] ++
  [ txt env 20 40 18 white
      (case wMic w of
         Nothing -> "Click to turn the mic ON"
         Just _ -> "Voice level -> r = " ++ fmt 3 r ++ "   " ++ stateText r)
  , if wLive w || null (wMic w) then Blank
    else txt env 20 66 14 (hsb 0 0 70) "No mic found: demo signal"
  ])
  where
    r = wR w
    w0 = envW env
    h0 = envH env
    size = (10 + (r - 2.8) * 8) / 2
    -- 粒（p5.js の半透明の背景で残る跡を、数個前の位置を薄く重ねて再現）
    drawPart p =
      [ Color (hsba (pHue p) 70 100 (pLife p / 100 * fade))
          (at (sc env (pX p) (pY p - pVY p * k)) (disc size))
      | (k, fade) <- zip [4, 3, 2, 1, 0] (map (ghostFade ^) [4, 3, 2, 1, 0 :: Int]) ]
    drawDot fade px = Color (withA fade white) (at (sc env px (h0 * 0.45)) (disc 12))
    m = max 1e-6 (U.maximum (wHist w))
    drawBar :: Int -> Float -> Picture
    drawBar i v =
      let fi = fromIntegral i
          hx = remap fi 0 50 80 (w0 - 80)
          bw = (w0 - 160) / 50 - 2
          hh = v / m * (h0 * 0.3)
          (cx, cy) = sc env (hx + bw / 2) (h0 - 60 - hh / 2)
      in Color (hsb (remap fi 0 50 180 360) 60 80) (rect cx cy bw hh)

stateText :: Float -> String
stateText r
  | r < 3.0   = "Regular (one value)"
  | r < 3.449 = "Period 2"
  | r < 3.57  = "Period 4 / 8"
  | otherwise = "Chaos!"

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case (ev, wMic w) of
  (EventKey (MouseButton LeftButton) Down _ _, Nothing) -> do   -- マイクを ON
    mic <- startMic
    pure w { wMic = Just mic }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 黙る → 粒は1列。小声 → 2列。大声 → 散らばる。声でカオスを ON/OFF できる
-- 2. sensitivity（どのくらいの声で r=4 になるか）を変えて感度を調整
-- 3. x を音の高さにして鳴らす（10 と合体）

-- ---------- 道具 ----------

-- p5.js 風の座標（左上が原点、y は下向き）を gloss の座標に直す
sc :: Env -> Float -> Float -> Point
sc env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size col s = let (gx, gy) = sc env x y in label gx gy size col s

remapC :: Float -> Float -> Float -> Float -> Float -> Float
remapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)

fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
