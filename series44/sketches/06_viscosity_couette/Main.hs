-- ============================================
-- 44-06 粘性 ─ クエット流とポアズイユ流
-- 粘性 = となりの層を「引きずる」性質。速さの差をならして、運動量を横に伝える
-- 2 枚の板のあいだの流れは、ナヴィエ・ストークス方程式がとても簡単になる：
--   ∂u/∂t = ν ∂²u/∂y² + G　　（ν = 動粘性係数、G = 圧力こう配による押す力）
-- これは熱の拡散と同じ「拡散方程式」。止まった状態から始めて、定常解に近づくようすを見る
--   1 = クエット流（上の板を動かす → 直線）　2 = ポアズイユ流（圧力で押す → 放物線）　3 = 両方
-- 操作：1〜3 = 流れ / 上下キー = 粘性 ν / R = 止めた状態からやり直し / スペース = 一時停止
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

utop :: Float
utop = 1.0          -- ★ 上の板の速さ

gPush :: Float
gPush = 0.8         -- ★ 圧力こう配で押す力（ρ で割ったもの）

n :: Int
n = 61              -- 格子点の数（壁から壁まで）

data World = World
  { wNu     :: !Float           -- ★ 動粘性係数 ν（すき間 H = 1 としたときの値）
  , wScene  :: !Int
  , wU      :: !(U.Vector Float)
  , wT      :: !Float
  , wPaused :: !Bool
  , wCols   :: ![U.Vector Float] -- 色水のすじ（最初は縦一直線）
  }

main :: IO ()
main = runSketch (sketch "44-06 Viscosity: Couette and Poiseuille flow" (restart (World 0.1 1 U.empty 0 False [])))
  { skBack  = hsb 215 60 10
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

restart :: World -> World
restart w = w { wU = U.replicate n 0, wT = 0, wCols = [ U.replicate n (fromIntegral k / 6) | k <- [0 .. 5 :: Int] ] }

topSpeed, push1 :: World -> Float
topSpeed w = if wScene w == 2 then 0 else utop
push1 w = if wScene w == 1 then 0 else gPush

-- 定常解（時間がたったあとの形）：直線 + 放物線
steady :: World -> Float -> Float
steady w y = topSpeed w * y + push1 w / (2 * wNu w) * y * (1 - y)

dy :: Float
dy = 1 / fromIntegral (n - 1)

-- ★ 拡散方程式を 1 歩進める：u[i] の変化 = ν × (上 − 2 × 自分 + 下) / Δy² + G
stepFlow :: World -> Float -> U.Vector Float -> U.Vector Float
stepFlow w dt old =
  let a = wNu w * dt / (dy * dy)
      g = push1 w
  in U.generate n $ \i ->
       if i == 0 then 0                       -- 壁では流体は壁と同じ速さ（すべりなし条件）
       else if i == n - 1 then topSpeed w
       else old U.! i + a * (old U.! (i - 1) - 2 * old U.! i + old U.! (i + 1)) + g * dt

step :: Env -> Float -> World -> IO World
step _ _ w = pure $
  let frameDt = if wPaused w then 0 else 1 / 60
      -- 陽解法は ν Δt / Δy² ≤ 0.5 でないと発散するので、1 フレームを細かく分ける
      sub = max 1 (ceiling (frameDt * wNu w / (0.4 * dy * dy))) :: Int
      u = iterate (stepFlow w (frameDt / fromIntegral sub)) (wU w) !! sub
      -- 色水のすじ：それぞれの高さの速さで右へ流される
      moveCol c = U.zipWith (\ci ui -> fract (ci + ui * frameDt * 0.25)) c u
  in w { wU = u, wT = wT w + frameDt, wCols = map moveCol (wCols w) }

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (hsbA 185 60 100 80) (thickLine 3 (x0 + a * wd, ypx (i - 1)) (x0 + b * wd, ypx i))
  | c <- wCols w, i <- [1 .. n - 1], let a = c U.! (i - 1), let b = c U.! i, abs (b - a) < 0.5 ]
  -- 板
  ++ [ rectTL (hsb 42 40 85) x0 yB wd 14, rectTL (hsb 42 40 85) x0 (yT - 14) wd 14 ]
  ++ [ Color (hsb 215 60 10) (thickLine 3 (x, yT - 14) (x, yT)) | x <- takeWhile (< x1) [x0 + slide, x0 + slide + 40 ..] ]
  ++ [ Color (hsb 215 60 10) (thickLine 3 (x, yB) (x, yB + 14)) | x <- takeWhile (< x1) [x0, x0 + 40 ..] ]
  ++ [ Color (hsb 12 70 100) (arrowLine 4 (x0 - 30, (yB + yT) / 2) (x0 - 4, (yB + yT) / 2)) | wScene w /= 1 ]
  -- 右：速度分布のグラフ（横 = 速さ u、縦 = 高さ y）
  ++ [ Color (hsb 0 0 40) (Line [(gx, yT), (gx, yB)]), Color (hsb 0 0 40) (Line [(gx, yB), (gx + gw, yB)])
     , Color (hsb 0 0 40) (Line [(gx, yT), (gx + gw, yT)]) ]
  ++ [ Color (hsbA 42 80 100 80) (thickLine 2 (gx + steady w (yf i) * sc, ypx i) (gx + steady w (yf (i + 1)) * sc, ypx (i + 1)))
     | i <- [0, 2 .. n - 2] ]   -- 定常解（点線）
  ++ [ path 3 (hsb 185 70 100) [ (gx + u U.! i * sc, ypx i) | i <- [0 .. n - 1] ] ]
  ++ [ Color (hsb 185 70 100) (arrowLine 1.5 (gx, ypx i) (gx + u U.! i * sc, ypx i)) | i <- [5, 10 .. n - 2], abs (u U.! i * sc) > 6 ]
  -- 数字
  ++ [ txt 20 40 18 (hsb 0 0 100)
         ((["", "1 Couette flow (top plate moves)", "2 Poiseuille flow (pressure pushes)", "3 Couette + Poiseuille"] !! wScene w)
          ++ "   nu = " ++ fx 3 (wNu w) ++ " (Up/Down)   R = restart   space = " ++ (if wPaused w then "play" else "pause"))
     , txt 20 68 15 (hsb 0 0 80)
         ("du/dt = nu d2u/dy2 + G    t = " ++ fx 2 (wT w) ++ "    off from steady " ++ fx 1 (100 * err / big)
          ++ "%    time to settle ~ H^2/nu = " ++ fx 1 (1 / wNu w))
     , txt 20 94 15 (hsb 0 0 65)
         ("cyan = velocity now   yellow dashes = steady solution   drag on the bottom wall nu du/dy = " ++ fx 3 shear)
     , txt (gx + 6) (yT - 22) 14 (hsb 185 70 100) "speed u(y)"
     , txt x0 (yT - 22) 14 (hsb 0 0 65) "dye lines (start straight up)" ]
  where
    u = wU w
    x0 = envW env * 0.05
    x1 = envW env * 0.6
    yB = envH env * 0.82
    yT = envH env * 0.3
    wd = x1 - x0
    hh = yB - yT
    ypx i = yB - hh * fromIntegral i / fromIntegral (n - 1)
    yf i = fromIntegral i / fromIntegral (n - 1)
    slide = wrapF 40 (wT w * topSpeed w * 0.25 * wd)   -- 上の板のしま模様（動きが見える）
    gx = envW env * 0.68
    gw = envW env * 0.27
    top = maximum (1.1 : [ steady w (yf i) * 1.08 | i <- [0 .. n - 1] ])
    sc = gw / top
    err = sum [ abs (u U.! i - steady w (yf i)) | i <- [0 .. n - 1] ]
    big = 1e-6 + sum [ abs (steady w (yf i)) | i <- [0 .. n - 1] ]
    shear = wNu w * (u U.! 1 - u U.! 0) / dy   -- 下の壁を引きずる力（せん断応力 ÷ ρ）= ν du/dy

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char k) Down _ _
    | k `elem` "123" -> restart w { wScene = fromEnum k - fromEnum '0' }
    | k `elem` "rR" -> restart w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wNu = min 0.6 (wNu w * 1.3) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wNu = max 0.04 (wNu w / 1.3) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1 のクエット流で R。板の動きが下の層へ「しみこむ」ようすを見る。直線になるまで何秒？
-- 2. ν を上げると早くなじむ（時間は H²/ν に比例）。はちみつと水、どちらが早い？
-- 3. 2 のポアズイユ流：ν を上げると最大の速さは下がる（u_max = G H² / 8ν）。確かめよう
-- 4. 3 にして gPush を 4 にすると、下のほうが板より速くなる。−4 にすると逆流する場所ができる
-- 5. step の中の 0.4 を 0.6 に変えると計算がこわれる（数値不安定）。どんなふうにこわれる？

-- ---------- 道具 ----------
fx :: Int -> Float -> String
fx d v = showFFloat (Just d) v ""

-- 横向きの矢印（矢じりは 2 本の短い線）
arrowLine :: Float -> Point -> Point -> Picture
arrowLine wdt (x1, y1) (x2, y2) =
  let d = if x2 > x1 then -7 else 7
  in Pictures [ thickLine wdt (x1, y1) (x2, y2), thickLine wdt (x2, y2) (x2 + d, y2 - 4), thickLine wdt (x2, y2) (x2 + d, y2 + 4) ]

rectTL :: Color -> Float -> Float -> Float -> Float -> Picture
rectTL c x y w h = Color c (Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)])

view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

path :: Float -> Color -> [Point] -> Picture
path w c ps = Color c (Pictures (zipWith (thickLine w) ps (drop 1 ps)))
