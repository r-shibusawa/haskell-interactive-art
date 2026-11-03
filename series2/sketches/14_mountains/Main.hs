-- ============================================
-- 14 フラクタル山脈（おまけ）
-- 「真ん中を上下にずらす」を再帰でくり返すと、山の稜線になる（中点変位法）
-- マウス左右 = 見る位置（手前の山が速く動く）/ クリックで新しい山 / ↑↓ = ギザギザ度
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)
import System.Random (StdGen, mkStdGen, randomR)

data World = World
  { wLayers :: ![U.Vector Float]   -- 奥から手前へ、何枚かの山
  , wRough  :: !Float              -- ギザギザ度
  , wGen    :: !StdGen
  }

layerCount, lastIndex :: Int
layerCount = 5     -- ★ 山の枚数
lastIndex = 256    -- 2^8 + 1 個の高さ

main :: IO ()
main = runSketch (sketch "02-14 Fractal Mountains" (makeMountains (World [] 0.5 (mkStdGen 1))))
  { skBack  = hsb 250 50 25
  , skStart = \_ -> do
      s <- randomRIO (0, 1000000)
      pure (makeMountains (World [] 0.5 (mkStdGen s)))
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

makeMountains :: World -> World
makeMountains w = w { wLayers = layers, wGen = g }
  where
    (layers, g) = go layerCount (wGen w)
    go :: Int -> StdGen -> ([U.Vector Float], StdGen)
    go 0 g0 = ([], g0)
    go k g0 =
      let (a, g1) = randomR (0.3, 0.7) g0
          (b, g2) = randomR (0.3, 0.7) g1
          (mid, g3) = midpoint (wRough w) 0 a lastIndex b 0.5 g2    -- ★ 再帰で間を埋める
          (more, g4) = go (k - 1) g3
      in (U.fromList (a : mid ++ [b]) : more, g4)

-- 区間 [a, b] の真ん中の高さを「両端の平均 ＋ ランダム」で決めて、左右に再帰
-- 間にある高さを左から順に返す
midpoint :: Float -> Int -> Float -> Int -> Float -> Float -> StdGen -> ([Float], StdGen)
midpoint rough a ha b hb amp g0
  | m == a = ([], g0)     -- 間がなくなったら終わり
  | otherwise =
      let (d, g1) = randomR (-amp, amp) g0
          hm = (ha + hb) / 2 + d
          (l, g2) = midpoint rough a ha m hm (amp * rough) g1   -- 左半分（ずらす量は小さく）
          (r, g3) = midpoint rough m hm b hb (amp * rough) g2   -- 右半分
      in (l ++ hm : r, g3)
  where m = (a + b) `div` 2

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (sky ++ [sun] ++ zipWith layer [0 ..] (wLayers w) ++ [info])
  where
    ww = envW env
    wh = envH env
    -- 空
    sky =
      [ Color (hsb (remap y 0 wh 250 20) 50 (remap y 0 wh 25 80)) (rect 0 (wh / 2 - y - 2) ww 4.5)
      | y <- [0, 4 .. wh] ]
    -- 太陽
    sun = at (ww * 0.25, wh * 0.2) (Color (hsb 40 40 100) (disc 45))
    -- 山を奥から描く
    mouseT = remap (fst (envMouse env)) (-ww / 2) (ww / 2) (-1) 1
    layer :: Int -> U.Vector Float -> Picture
    layer i pts =
      let depth = fromIntegral i / fromIntegral (layerCount - 1)  -- 0 = 奥, 1 = 手前
          shift = mouseT * depth * 200       -- 手前ほど大きく動く（視差）
          base = wh * (0.45 + depth * 0.5)
          amp = wh * (0.15 + depth * 0.2)
          c = hsb (remap depth 0 1 230 140) (remap depth 0 1 25 50) (remap depth 0 1 70 15)
          bottom = -wh / 2 - 10
          xOf k = remap (fromIntegral k) 0 (fromIntegral lastIndex) (-ww / 2 - 300) (ww / 2 + 300) + shift
          yOf k = wh / 2 - (base - (pts U.! k) * amp)
          -- 山の形はへこみがあるので、細長い四角に分けて塗る
          quad k = Polygon [(xOf k, bottom), (xOf (k + 1) + 0.5, bottom), (xOf (k + 1) + 0.5, yOf (k + 1)), (xOf k, yOf k)]
      in Color c (Pictures (map quad [0 .. lastIndex - 1]))
    info = label (-ww / 2 + 20) (wh / 2 - 30) 16 white
      ("Mouse left/right: view   Click: new mountains   Up/Down: roughness (" ++ showFFloat (Just 2) (wRough w) "" ++ ")")

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (makeMountains w)
  EventKey (MouseButton _) _ _ _ -> pure w
  -- どのキーでも新しい山になる（p5.js 版と同じ）
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (makeMountains w { wRough = min 0.9 (wRough w + 0.05) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (makeMountains w { wRough = max 0.1 (wRough w - 0.05) })
  EventKey (Char c) Down _ _ | c == 's' || c == 'S' -> do
    saveSvg "mountains" env (drawWorld env w)
    pure (makeMountains w)
  EventKey _ Down _ _ -> pure (makeMountains w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. ギザギザ度 0.1（なめらかな丘）〜 0.9（険しい岩山）を比べよう
-- 2. 山の色を、夕方や夜の色にしよう
-- 3. 2D の線を 2 回使うと、2D の地形（高さの地図）もつくれる。調べてみよう：ダイヤモンド・スクエア法
