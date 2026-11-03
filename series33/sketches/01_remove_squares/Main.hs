-- ============================================
-- 01 正方形を取り除く ─ 相似な長方形が残る
-- 1 : φn の長方形から正方形を n 枚取ると、残りはまた 1 : φn（向きが 90° 回るだけ）
--   φn − n = 1/φn  ⇔  φn² = n·φn + 1
-- ←→ = n（1 黄金・2 白銀・3 青銅…）/ ↑↓ = 深さ / スペース = 自動で 1 段ずつ / F = 比をマウスで自由に / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.List (unfoldr)
import Numeric (showFFloat)

data World = World
  { wN     :: !Int    -- ★ 何枚ずつ正方形を取るか
  , wDepth :: !Int    -- ★ 何段まで分けるか
  , wShown :: !Int    -- いま見えている段数（アニメーション）
  , wAuto  :: !Bool
  , wFree  :: !Bool   -- F：比を自由に変える（相似にならない例）
  , wTimer :: !Int
  }

initialWorld :: World
initialWorld = World 1 9 0 True False 0

main :: IO ()
main = runSketch (sketch "33-01 Remove Squares" initialWorld)
  { skBack  = hsb 225 30 10
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- 時間とともに 1 段ずつ増やす
stepWorld :: World -> World
stepWorld w
  | not (wAuto w) = w
  | wTimer w + 1 > 40 = w { wTimer = 0, wShown = nextShown w }
  | otherwise = w { wTimer = wTimer w + 1 }

nextShown :: World -> Int
nextShown w = if wShown w >= wDepth w then 0 else wShown w + 1

-- 1 段分の結果：その段の正方形と、取り除いたあとの残り
data Level = Level
  { lvD     :: !Int
  , lvK     :: !Int        -- 何枚取ったか
  , lvS     :: !Double     -- 正方形の一辺
  , lvSq    :: [(Double, Double)]  -- 正方形の左上
  , lvRest  :: !(Double, Double, Double, Double)  -- 残りの長方形 (x, y, w, h)
  , lvRatio :: !Double
  }

-- 分割を順にたどる：向き dir は 0 左から 1 上から 2 右から 3 下から
-- 誤差が段ごとに φ² 倍にふくらむので、ここだけ Double で計算する
levels :: World -> Double -> (Double, Double, Double, Double) -> [Level]
levels w _ start = unfoldr next (0, start)
  where
    next (d, (x, y, rw, rh))
      | d >= wDepth w || rw <= 0.5 || rh <= 0.5 = Nothing
      | otherwise =
          let dir = d `mod` 4
              s = min rw rh                                  -- 正方形の一辺 = 短い辺
              k0 = floor (max rw rh / s + 1e-9) :: Int       -- 何枚取れるか
              k = if wFree w then k0 else min k0 (wN w)
              fk = fromIntegral k
              place j = let fj = fromIntegral j in case dir of
                0 -> (x + fj * s, y)
                1 -> (x, y + fj * s)
                2 -> (x + rw - (fj + 1) * s, y)
                _ -> (x, y + rh - (fj + 1) * s)
              rest = case dir of
                0 -> (x + fk * s, y, rw - fk * s, rh)
                1 -> (x, y + fk * s, rw, rh - fk * s)
                2 -> (x, y, rw - fk * s, rh)
                _ -> (x, y, rw, rh - fk * s)
              (_, _, w2, h2) = rest
              lv = Level d k s (map place [0 .. min k 200 - 1]) rest (max w2 h2 / max 0.0001 (min w2 h2))
          in Just (lv, (d + 1, rest))

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (concatMap drawLevel lvs ++ [outer] ++ hud ++ panel)
  where
    n = wN w
    (mx, _) = envMouse env
    mouseX = clampD 0 (realToFrac (envW env)) (realToFrac mx + realToFrac (envW env) / 2)
    r | wFree w = 1.05 + mouseX / realToFrac (envW env) * (fromIntegral n + 1.95 - 1.05)
      | otherwise = metallicD (fromIntegral n)
    -- 画面に入る大きさの長方形
    bigH = min (realToFrac (envH env) - 220) ((realToFrac (envW env) - 460) / r)
    bigW = bigH * r
    x0 = 40 :: Double
    y0 = 130 :: Double
    lvs = levels w r (x0, y0, bigW, bigH)
    f = realToFrac :: Double -> Float
    drawLevel lv =
      let visible = lvD lv < wShown w
          sqPic j (sx, sy) = Pictures
            [ Color (hsba (wrapF 360 (fromIntegral (lvD lv * 40 + j * 12))) 55 (if visible then 85 else 25) (if visible then 0.8 else 0.3))
                (box env (f sx) (f sy) (f (lvS lv)) (f (lvS lv)))
            , Color (hsba 0 0 100 0.7) (boxLine env 1.5 (f sx) (f sy) (f (lvS lv)) (f (lvS lv)))
            ]
          (rx, ry, rw, rh) = lvRest lv
          -- いま増えた段の残りを太枠で示す
          mark = [ Color (hsb 55 90 100) (boxLine env 3 (f rx) (f ry) (f rw) (f rh)) | lvD lv == wShown w - 1 ]
      in zipWith sqPic [0 :: Int ..] (lvSq lv) ++ mark
    outer = Color white (boxLine env 2 (f x0) (f y0) (f bigW) (f bigH))
    hud =
      [ txt env 20 26 15 white ("n = " ++ show n ++ " (L/R) " ++ nameOf n ++ "   depth " ++ show (wDepth w) ++ " (Up/Dn)   Space auto " ++ onOff (wAuto w) ++ "   F free " ++ onOff (wFree w) ++ "   R restart")
      , txt env 20 52 14 (gray 0.75)
          (if wFree w
             then "Free ratio (mouse L/R): take all squares that fit. The rest changes each time: not similar."
             else "Remove " ++ show n ++ " square(s) from 1 : phi and the rest is again 1 : phi, so the shape repeats forever.")
      ]
    px = f x0 + f bigW + 40
    py = 140
    rows = length lvs
    panel =
      [ txt env px py 15 white ("First ratio 1 : " ++ nfD 5 r) ]
      ++ [ txt env px (py + 30 + fromIntegral i * 22) 14 (if i < wShown w then hsb 55 70 100 else hsb 0 0 50)
             ("Step " ++ show (i + 1) ++ ": " ++ show (lvK lv) ++ " sq -> 1 : " ++ nfD 5 (lvRatio lv))
         | (i, lv) <- zip [0 ..] lvs ]
      ++ (if wFree w then [] else
            let v = metallicD (fromIntegral n)
                ry = py + 60 + fromIntegral rows * 22
            in [ txt env px ry 14 (gray 0.8) ("phi - " ++ show n ++ " = " ++ nfD 5 (v - fromIntegral n))
               , txt env px (ry + 22) 14 (gray 0.8) ("1 / phi = " ++ nfD 5 (1 / v) ++ "  same!")
               , txt env px (ry + 44) 14 (gray 0.8) ("phi^2 = " ++ show n ++ "*phi + 1, divide by phi:")
               , txt env px (ry + 66) 14 (gray 0.8) ("phi = " ++ show n ++ " + 1/phi")
               ])

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wN = min (wN w + 1) 6, wShown = 0 }
  EventKey (SpecialKey KeyLeft) Down _ _  -> w { wN = max (wN w - 1) 1, wShown = 0 }
  EventKey (SpecialKey KeyUp) Down _ _    -> w { wDepth = min (wDepth w + 1) 16 }
  EventKey (SpecialKey KeyDown) Down _ _  -> w { wDepth = max (wDepth w - 1) 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  EventKey (Char c) Down _ _
    | c == 'f' || c == 'F' -> w { wFree = not (wFree w) }
    | c == 'r' || c == 'R' -> w { wShown = 0, wTimer = 0 }
  EventKey (MouseButton LeftButton) Down _ _
    | not (wAuto w) -> w { wShown = nextShown w }
  _ -> w

-- ---------- やってみよう ----------
-- 1. n = 1, 2, 3 で、何段目まで目で見分けられるか数えよう。小さくなる速さは 1/φ 倍ずつ
-- 2. F で自由な比にして、マウスを φ の近くでゆっくり動かそう。少しずれると何段目で崩れる？
-- 3. A 判の紙 √2 を F で試そう（正方形 1 枚取ると 1 : 2.414 になる → 05 につづく）
-- 4. 正方形の色を「段ごと」ではなく「大きさ」で決めるように変えてみよう

-- ---------- 道具 ----------
metallicD :: Double -> Double
metallicD k = (k + sqrt (k * k + 4)) / 2

nameOf :: Int -> String
nameOf 1 = "golden"
nameOf 2 = "silver"
nameOf 3 = "bronze"
nameOf k = "metallic #" ++ show k

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

nfD :: Int -> Double -> String
nfD d v = showFFloat (Just d) v ""

clampD :: Double -> Double -> Double -> Double
clampD lo hi = max lo . min hi

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = Polygon [pt env x y, pt env (x + w) y, pt env (x + w) (y + h), pt env x (y + h)]

boxLine :: Env -> Float -> Float -> Float -> Float -> Float -> Picture
boxLine env t x y w h =
  thickPath t [pt env x y, pt env (x + w) y, pt env (x + w) (y + h), pt env x (y + h), pt env x y]
