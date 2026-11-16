-- ============================================
-- 44-02 ベルヌーイの定理　p + ½ρv² + ρgh = 一定
-- 流れに沿って「圧力・運動・高さ」のエネルギーの合計は変わらない（粘性なし・定常）
--   1 = ベンチュリ管：細い所は速い → 圧力が下がる（立てた管の水位でわかる）
--   2 = トリチェリの定理：タンクの穴から出る速さ v = √(2gh)
-- 操作：1/2 = 場面 / ドラッグ = しぼる・穴の高さ / 上下キー = 流速・穴の高さ / R = 元に戻す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Numeric (showFFloat)

v00, throat0 :: Float
v00 = 120           -- ★ 太い所の流速 [px/秒]
throat0 = 0.5       -- ★ しぼり（細い所の太さ ÷ 元の太さ）

rho, p0, pipeH :: Float
rho = 1.0           -- ★ 密度
p0 = 60000          -- ★ 太い所の圧力（立てた管の水位のもと）
pipeH = 90

-- トリチェリ用
grav, drain, holeY0 :: Float
grav = 600          -- ★ 重力加速度 [px/秒²]
drain = 0.02        -- ★ 穴の大きさ（大きいほど水位が早く下がる）
holeY0 = 0.3        -- ★ 穴の高さ（タンクの高さに対する割合）

data Drop = Drop { dX :: !Float, dY :: !Float, dVx :: !Float, dVy :: !Float }

data World = World
  { wScene  :: !Int
  , wV0     :: !Float
  , wThroat :: !Float
  , wParts  :: ![(Float, Float)]   -- 粒の (x, 管の中の高さの割合)
  , wLevel  :: !Float              -- 水位（タンクの高さに対する割合）
  , wHoleY  :: !Float
  , wDrops  :: ![Drop]
  }

main :: IO ()
main = runSketch (sketch "44-02 Bernoulli's theorem" (World 1 v00 throat0 [] 0.9 holeY0 []))
  { skBack  = hsb 215 60 10
  , skStart = \env -> do
      ps <- replicateM 500 ((,) <$> randomRIO (0, envW env) <*> randomRIO (-1, 1))
      pure (World 1 v00 throat0 ps 0.9 holeY0 [])
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

cy :: Env -> Float
cy env = envH env * 0.6

halfHeight :: Env -> World -> Float -> Float
halfHeight env w x =
  let s = clampF (-1) 1 ((x - envW env / 2) / (envW env * 0.2))
  in pipeH * (1 - (1 - wThroat w) * 0.5 * (1 + cos (pi * s)))

-- 連続の式
speedAt :: Env -> World -> Float -> Float
speedAt env w x = wV0 w * pipeH / halfHeight env w x

-- ★ ベルヌーイの定理：p = p0 + ½ρ(v0² − v²)
pressureAt :: Env -> World -> Float -> Float
pressureAt env w x = let v = speedAt env w x in p0 + 0.5 * rho * (wV0 w * wV0 w - v * v)

-- タンクの寸法：(左端, 幅, 地面, 高さ)
tank :: Env -> (Float, Float, Float, Float)
tank env = (envW env * 0.08, envW env * 0.2, envH env * 0.88, envH env * 0.62)

step :: Env -> Float -> World -> IO World
step env dt0 w = pure $
  let dt = min dt0 0.033
      (_, my) = mouseP env
  in if wScene w == 1
       then
         let w1 = if envDown env && my > 90 then w { wThroat = clampF 0.3 1 (abs (my - cy env) / pipeH) } else w
             mv (x, e) = let x' = x + speedAt env w1 x * dt in (if x' > envW env then x' - envW env else x', e)
         in w1 { wParts = map mv (wParts w1) }
       else
         let (tx, tw, ground, th) = tank env
             top = ground - th
             w1 = if envDown env && my > top then w { wHoleY = clampF 0.03 0.85 ((ground - my) / th) } else w
             depth = max (wLevel w1 - wHoleY w1) 0 * th   -- 穴の上にある水の深さ h
             v = sqrt (2 * grav * depth)                   -- ★ トリチェリの定理
             level = max (wLevel w1 - drain * v / th * dt) 0   -- 出たぶんだけ水位が下がる
             hy = ground - wHoleY w1 * th
             new = [Drop (tx + tw) hy v 0 | v > 1, even (envFrame env)]
             fall (Drop x y vx vy) = let vy' = vy + grav * dt in Drop (x + vx * dt) (y + vy' * dt) vx vy'
             drops = filter (\d -> dY d < ground && dX d < envW env) (map fall (wDrops w1 ++ new))
         in w1 { wLevel = level, wDrops = drops }

draw :: Env -> World -> Picture
draw env w = if wScene w == 1 then venturi env w else tankPic env w

venturi :: Env -> World -> Picture
venturi env w = view env $
  [ path 5 (hsb 42 40 85) [ (x, c + sgn * (halfHeight env w x + 4)) | x <- [0, 8 .. wd + 8] ] | sgn <- [-1, 1] ]
  ++ [ dot (hsb (remap (clampF 80 500 v) 80 500 200 10) 70 100) x (c + e * halfHeight env w x) 5
     | (x, e) <- wParts w, let v = speedAt env w x ]
  -- 立てた管（マノメータ）と、エネルギーの棒グラフ
  ++ concat
     [ [ Color (hsb 0 0 70) (thickLine 2 (x - 9, top) (x - 9, 90))
       , Color (hsb 0 0 70) (thickLine 2 (x + 9, top) (x + 9, 90))
       , rectTL (hsbA 200 70 95 80) (x - 8) (top - hp) 16 hp           -- 水位 = 圧力 p
       , rectTL (hsbA 12 70 100 80) (x + 22) (top - hp - dyn * unit) 10 (dyn * unit) -- 動圧をその上に積む
       , txtC x (c + pipeH + 34) 14 (hsb 200 50 100) ("p = " ++ f1 (p / 1000))
       , txtC x (c + pipeH + 54) 14 (hsb 12 60 100) ("rho v^2/2 = " ++ f1 (dyn / 1000))
       , txtC x (c + pipeH + 74) 14 (hsb 0 0 100) ("total " ++ f1 ((p + dyn) / 1000)) ]
       ++ [ txtC x (c + pipeH + 96) 14 (hsb 350 80 100) "NEGATIVE! (bubbles)" | p < 0 ]
     | k <- [0 .. 4 :: Int]
     , let x = wd * (0.1 + 0.2 * fromIntegral k)
     , let top = c - halfHeight env w x - 4
     , let p = pressureAt env w x
     , let v = speedAt env w x
     , let dyn = 0.5 * rho * v * v
     , let hp = max p 0 * unit ]
  -- 合計の高さ（全部の場所で同じ）
  ++ [ Color (hsbA 42 80 100 70) (Line [(x, total), (x + 8, total)]) | x <- [0, 16 .. wd - 1] ]
  ++ [ txt 20 (total - 8) 14 (hsb 42 80 100) "p + rho v^2/2 = const (total pressure)"
     , txt 20 40 18 (hsb 0 0 100)
         ("1 Venturi tube   v0 = " ++ f0 (wV0 w) ++ " (Up/Down)   throat " ++ f0 (wThroat w * 100) ++ "% (drag up/down)   2 = Torricelli")
     , txt 20 68 15 (hsb 0 0 65)
         "Blue = pressure p (water column), red = dynamic pressure rho v^2/2. Faster -> lower p. The sum is always the same." ]
  where
    wd = envW env
    c = cy env
    v0 = wV0 w
    unit = (c - pipeH - 110) / (p0 + 0.5 * rho * v0 * v0) * 0.9   -- 高さへの換算
    total = c - pipeH - 4 - (p0 + 0.5 * rho * v0 * v0) * unit

tankPic :: Env -> World -> Picture
tankPic env w = view env $
  [ rectTL (hsbA 200 70 90 70) tx (ground - wLevel w * th) tw (wLevel w * th)
  , wall (tx, top) (tx, ground), wall (tx, ground) (envW env, ground)
  , wall (tx + tw, top) (tx + tw, hy - 8), wall (tx + tw, hy + 8) (tx + tw, ground) ]
  -- 噴流：水平に速さ v で飛び出し、あとは放物線
  ++ [ path 2 (hsbA 200 50 100 60) jet | v > 1 ]
  ++ [ dot (hsb 200 60 100) (dX d) (dY d) 6 | d <- wDrops w ]
  ++ [ Color (hsb 42 80 100) (thickLine 2 (tx + tw + reach, ground - 14) (tx + tw + reach, ground + 14))
     , txtC (tx + tw + reach) (ground + 32) 14 (hsb 42 80 100) ("reach " ++ f0 reach)
     , txt (tx + 10) (hy - 12) 15 (hsb 0 0 100) ("depth h = " ++ f0 depth)
     , txt (tx + tw + 14) (hy - 12) 15 (hsb 0 0 100) ("v = sqrt(2gh) = " ++ f0 v)
     , txt 20 40 18 (hsb 0 0 100) "2 Torricelli   hole height (Up/Down or drag on the tank)   R = refill   1 = Venturi"
     , txt 20 68 15 (hsb 0 0 65)
         "Bernoulli between surface (v ~ 0, height h) and hole (v, height 0): rho g h = rho v^2/2, like free fall from h." ]
  where
    (tx, tw, ground, th) = tank env
    top = ground - th
    depth = max (wLevel w - wHoleY w) 0 * th
    v = sqrt (2 * grav * depth)
    hy = ground - wHoleY w * th
    wall a b = Color (hsb 42 40 85) (thickLine 5 a b)
    jet = takeUntilGround [ (tx + tw + v * s, hy + 0.5 * grav * s * s) | s <- [0, 0.03 .. 3] ]
    takeUntilGround ps = case span (\(_, y) -> y <= ground) ps of
      (a, (x, _) : _) -> a ++ [(x, ground)]
      (a, []) -> a
    -- 飛ぶ距離の理論値：L = v × 落下時間 = 2 √(h × 穴の高さ)
    reach = 2 * sqrt (depth * wHoleY w * th)

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char '1') Down _ _ -> w { wScene = 1 }
  EventKey (Char '2') Down _ _ -> w { wScene = 2, wLevel = 0.9, wDrops = [] }
  EventKey (SpecialKey KeyUp) Down _ _
    | wScene w == 1 -> w { wV0 = min 220 (wV0 w + 10) }
    | otherwise -> w { wHoleY = min 0.85 (wHoleY w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _
    | wScene w == 1 -> w { wV0 = max 40 (wV0 w - 10) }
    | otherwise -> w { wHoleY = max 0.03 (wHoleY w - 0.05) }
  EventKey (Char k) Down _ _ | k `elem` "rR" -> w { wLevel = 0.9, wThroat = throat0, wV0 = v00, wDrops = [] }
  _ -> w

-- ---------- やってみよう ----------
-- 1. しぼりを 50% にして v0 を上げていく。細い所の水位が 0 になる（負圧）のは v0 がいくつのとき？
-- 2. 「合計」の数字が 5 か所で同じことを確かめる。p0 を変えるとどうなる？
-- 3. 2 のタンクで、いちばん遠くまで届く穴の高さは？（水位のちょうど半分になるはず）
-- 4. grav を 100（月）にする。v と届く距離はどう変わる？
-- 5. drain を 0.1 にすると水位が早く下がる。水位の下がり方はだんだん遅くなる。なぜ？

-- ---------- 道具 ----------
f0, f1 :: Float -> String
f0 v = show (round v :: Int)
f1 v = showFFloat (Just 1) v ""

-- 左上が (x, y) の塗りつぶした長方形（高さが負なら描かない）
rectTL :: Color -> Float -> Float -> Float -> Float -> Picture
rectTL c x y w h
  | w <= 0 || h <= 0 = Blank
  | otherwise = Color c (Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)])

view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y size c s = txt (x - fromIntegral (length s) * size * 0.36) y size c s

hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

path :: Float -> Color -> [Point] -> Picture
path w c ps = Color c (Pictures (zipWith (thickLine w) ps (drop 1 ps)))

dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))
