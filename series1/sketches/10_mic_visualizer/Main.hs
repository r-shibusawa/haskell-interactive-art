-- ============================================
-- 10 マイク・ビジュアライザー
-- 声や音楽に反応して、光の輪が動く
-- クリックでマイクを ON（マイクが使えないときはデモの信号で動く）
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U

-- 1 フレームぶんの記録（残像のために少し覚えておく）
data Snap = Snap
  { sHue  :: !Float
  , sVol  :: !Float
  , sBars :: !(U.Vector Float)    -- 線の長さのもと（0〜1）
  }

data World = World
  { wMic    :: !(Maybe Mic)       -- まだ ON にしていなければ Nothing
  , wLive   :: !Bool              -- 本物のマイクの音が来ているか
  , wHue    :: !Float
  , wSmooth :: !(U.Vector Float)  -- なめらかにしたスペクトル
  , wSnaps  :: ![Snap]            -- 新しいものが先頭
  }

bins :: Int
bins = 100                 -- ★ 使う数（高すぎる音は使わない）

trailFade :: Float
trailFade = 0.75           -- ★ 残像の薄くなり方（p5.js の background の透明度 25 に相当）

main :: IO ()
main = runSketch (sketch "01-10 Mic Visualizer" (World Nothing False 0 (U.replicate bins 0) []))
  { skBack  = hsb 0 0 5
  , skStep  = \_ _ w -> step w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

step :: World -> IO World
step w = case wMic w of
  Nothing -> pure w
  Just mic -> do
    live <- micLive mic
    spectrum <- micSpectrum mic         -- 低い音〜高い音の強さ（256 個）
    vol <- micLevel mic                 -- 全体の音量（0〜1）
    let new = U.fromListN bins (spectrum ++ repeat 0)
        -- 前のフレームと混ぜて、なめらかにする（p5.js の smoothingTimeConstant）
        smooth = U.zipWith (\old v -> 0.8 * old + 0.2 * v) (wSmooth w) new
        snap = Snap (wHue w) vol (U.map toDecibelScale smooth)
    pure w
      { wLive = live
      , wSmooth = smooth
      , wSnaps = take 5 (snap : wSnaps w)
      , wHue = wrapF 360 (wHue w + 0.5)
      }

drawWorld :: Env -> World -> Picture
drawWorld env w = case wMic w of
  Nothing -> labelC 0 0 24 white "Click to turn the mic ON"
  Just _ -> Pictures (reverse (zipWith (drawSnap env) (iterate (* trailFade) 1) (wSnaps w)) ++ [hint])
  where
    hint | wLive w = Blank
         | otherwise = label (-envW env / 2 + 20) (envH env / 2 - 36) 16 (gray 0.7) "No mic found: demo signal"

drawSnap :: Env -> Float -> Snap -> Picture
drawSnap env fade s = Pictures (lines_ ++ [center])
  where
    size = min (envW env) (envH env)
    inner = size * 0.15                  -- 内側の円の半径
    -- ---- 放射状の線（スペクトラム）----
    lines_ =
      [ Color (hsba (remap fi 0 fb 180 360 + sHue s) 80 100 (0.9 * fade)) $
          thickLine 4 (cos a * inner, sin a * inner) (cos a * (inner + len), sin a * (inner + len))
      | i <- [0 .. bins - 1]
      , let fi = fromIntegral i
            fb = fromIntegral bins
            angle = remap fi 0 fb 0 pi        -- 半周分
            len = sBars s U.! i * size * 0.35
      , side <- [1, -1]                       -- 左右対称に描く
      , let a = pi / 2 - angle * side         -- 真上から左右に開く
      ]
    -- ---- 中心の円（音量で大きくなる）----
    center = Color (hsba (sHue s + 200) 60 100 (0.8 * fade))
                   (disc ((inner * 1.6 + sVol s * 800) / 2))

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case (ev, wMic w) of
  (EventKey (MouseButton LeftButton) Down _ _, Nothing) -> do   -- マイクを ON
    mic <- startMic
    pure w { wMic = Just mic }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 「あー」と高い声・低い声を出して、線の動き方を比べよう
-- 2. スマホで音楽を流してマイクに近づけてみよう
-- 3. thickLine を disc に変えると、どんな模様になる？

-- ---------- 道具 ----------

-- 強さ（0〜1）を、p5.js の getByteFrequencyData と同じ -100〜-30 dB の目盛りで 0〜1 にする
toDecibelScale :: Float -> Float
toDecibelScale v =
  let magnitude = v * 40 / 512
      db = 20 * logBase 10 (max 1e-6 magnitude)
  in clampF 0 1 ((db + 100) / 70)
