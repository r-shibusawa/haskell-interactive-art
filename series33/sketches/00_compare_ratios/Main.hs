-- ============================================
-- 00 比をくらべる ─ 黄金比・白銀比・青銅比
-- 短い辺を 1 にそろえて、長い辺が「何倍か」を並べてくらべる
--   貴金属比 φn = (n + √(n² + 4)) / 2   n=1 黄金比 / n=2 白銀比 / n=3 青銅比
-- 日本で「白銀比」と呼ばれる √2（A 判の紙・大和比）は別もの。名前の混乱に注意！
-- 右下の自由な長方形の右端をドラッグ / 1〜5 = その比を強調 / O = 重ねて比べる / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (minimumBy)
import Data.Ord (comparing)
import Numeric (showFFloat)

-- 比 1 つ分のデータ
data Named = Named
  { rName :: String
  , rExpr :: String
  , rV    :: !Float
  , rHue  :: !Float
  , rEx   :: String
  }

ratios :: [Named]
ratios =
  [ Named "Silver (Yamato)" "sqrt2" (sqrt 2) 200 "A4 paper 210x297 mm, Horyu-ji, anime faces"
  , Named "Golden (n=1)" "(1+sqrt5)/2" ((1 + sqrt 5) / 2) 45 "Credit card 54x85.6 mm, name cards"
  , Named "Silver (n=2)" "1+sqrt2" (1 + sqrt 2) 210 "Regular octagon: diagonal / side"
  , Named "2nd silver" "1+sqrt3" (1 + sqrt 3) 280 "Regular dodecagon: diagonal / side"
  , Named "Bronze (n=3)" "(3+sqrt13)/2" ((3 + sqrt 13) / 2) 25 "Remove 3 squares: a similar one remains"
  ]

data World = World
  { wFree    :: !Float   -- 自由な長方形の比（ドラッグで変わる）
  , wDrag    :: !Bool
  , wPicked  :: !Int     -- 強調している比（-1 = なし）
  , wOverlay :: !Bool    -- 重ねて比べるモード
  }

initialWorld :: World
initialWorld = World 1.3 False (-1) False

main :: IO ()
main = runSketch (sketch "33-00 Compare Ratios" initialWorld)
  { skBack  = hsb 230 25 12
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = drawWorld
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

-- 短い辺の長さ（画面に合わせる）★
unitOf :: Env -> Float
unitOf env = clampF 40 140 (min (envW env / 9) ((envH env - 260) / 7.5))

x0, y0 :: Float
x0 = 40
y0 = 120

-- 自由な長方形の上端と高さ
freeTop, freeH :: Env -> Float
freeTop env = envH env - unitOf env * 1.3 - 40
freeH env = unitOf env * 1.1

stepWorld :: Env -> World -> World
stepWorld env w
  | wDrag w = let (mx, _) = mouseP env
              in w { wFree = clampF 1 4 ((mx - x0) / freeH env) }
  | otherwise = w

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  ( [ txt env 20 26 15 white ("Short side = 1: compare the long side   1-5 pick   O overlay " ++ onOff (wOverlay w) ++ "   R reset")
    , txt env 20 48 14 (gray 0.75) "Two \"silver ratios\": Japanese sqrt2 = 1.414 and metallic 1+sqrt2 = 2.414."
    , txt env 20 70 14 (gray 0.75) "Both names are right; use the context. Drag the handle of the lower rectangle."
    ]
    ++ zipWith (drawRatio env w) [0 ..] ratios
    ++ [drawFree env w, drawFormulaPanel env])

-- 名前つきの長方形を 1 つ描く
drawRatio :: Env -> World -> Int -> Named -> Picture
drawRatio env w i r = Pictures (body : extra)
  where
    u = unitOf env
    fi = fromIntegral i :: Float
    y = if wOverlay w then y0 else y0 + fi * (u * 0.62 + 20)
    h = if wOverlay w then u * 2 else u * 0.62
    rw = h * rV r
    on = wPicked w < 0 || wPicked w == i
    body = Pictures
      [ Color (hsba (rHue r) 60 80 (if wOverlay w then 0.08 else if on then 0.3 else 0.08)) (box env x0 y rw h)
      , Color (hsba (rHue r) 70 100 (if on then 1 else 0.25)) (boxLine env (if wPicked w == i then 4 else 2) x0 y rw h)
      ]
    extra
      | wOverlay w =
          [ txt env (x0 + rw - 60) (y + h + 18 + fi * 18) 14 (hsba (rHue r) 70 100 (if on then 1 else 0.4)) (rExpr r) ]
      | otherwise =
          -- 正方形の区切りを薄く（何枚の正方形が入るか）
          [ Color (hsba (rHue r) 40 100 (if on then 0.4 else 0.1))
              (Line [pt env (x0 + k * h) y, pt env (x0 + k * h) (y + h)])
          | k <- takeWhile (< rV r) [1, 2, 3, 4] ]
          ++
          [ txt env (x0 + rw + 16) (y + h * 0.42) 15 (withA (if on then 1 else 0.4) white)
              (show (i + 1) ++ " " ++ rName r ++ " 1 : " ++ rExpr r ++ " = " ++ nf 4 (rV r))
          , txt env (x0 + rw + 16) (y + h * 0.42 + 20) 14 (hsba 0 0 70 (if on then 1 else 0.4)) (rEx r)
          ]

-- 自由な長方形（ドラッグで比を変える）
drawFree :: Env -> World -> Picture
drawFree env w = Pictures
  [ Color (withA 0.1 white) (box env x0 fy fw fh)
  , Color white (boxLine env 2 x0 fy fw fh)
  , Color (if wDrag w then hsb 50 90 100 else white) (at (pt env (x0 + fw) (fy + fh / 2)) (disc 9))
  -- いちばん近い比の長方形を点線で重ねる
  , Color (hsb (rHue near) 70 100) (dashedBox env x0 fy (fh * rV near) fh)
  , txt env tx (fy + fh * 0.4) 16 white ("Your rectangle 1 : " ++ nf 3 (wFree w))
  , txt env tx (fy + fh * 0.4 + 26) 15 (hsb (rHue near) 60 100)
      ("Nearest: " ++ rName near ++ " (diff " ++ nf 3 (abs (wFree w - rV near)) ++ ")")
  ]
  where
    fy = freeTop env
    fh = freeH env
    fw = fh * wFree w
    near = nearest (wFree w)
    tx = x0 + max fw (fh * rV near) + 20

-- 貴金属比の式と数値の表（右上）
drawFormulaPanel :: Env -> Picture
drawFormulaPanel env
  | px < unitOf env * 5 = Blank      -- 画面がせまいときは省略
  | otherwise = Pictures
      ( [ Color (withA 0.07 white) (box env (px - 16) (py - 30) 476 230)
        , txt env px py 15 white "phi_n = (n + sqrt(n^2 + 4)) / 2"
        ]
        ++ [ txt env px (py + 26 + fromIntegral n * 22) 14 (rowColor n)
               ("n=" ++ show n ++ "  phi=" ++ nf 5 v ++ "  1/phi=" ++ nf 5 (1 / v))
           | n <- [1 .. 6 :: Int], let v = metallic (fromIntegral n) ]
        ++ [ txt env px (py + 190) 14 (gray 0.75) "phi - 1/phi = n (same decimals!)" ])
  where
    px = envW env - 470
    py = 120
    rowColor :: Int -> Color
    rowColor 1 = hsb 45 60 100
    rowColor 2 = hsb 210 60 100
    rowColor 3 = hsb 25 60 100
    rowColor _ = hsb 0 0 80

nearest :: Float -> Named
nearest v = minimumBy (comparing (\r -> abs (rV r - v))) ratios

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
        fy = freeTop env
        fh = freeH env
        hit = my > fy - 20 && my < fy + fh + 20 && abs (mx - (x0 + fh * wFree w)) < 40
    in if hit then w { wDrag = True } else w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = False }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '5' ->
        let i = fromEnum c - fromEnum '1'
        in w { wPicked = if wPicked w == i then -1 else i }
    | c == 'o' || c == 'O' -> w { wOverlay = not (wOverlay w) }
    | c == 'r' || c == 'R' -> w { wFree = 1.3, wPicked = -1, wOverlay = False }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 定規で手元の紙・カード・スマホの縦横を測って、どの比に近いか自由な長方形で確かめよう
-- 2. 表の 1/φ の小数部分と φ の小数部分を見くらべよう。なぜ同じになる？（φ² = nφ + 1 を φ で割る）
-- 3. ratios に「1 : 1.5（3:2 の写真）」「1 : 16/9（テレビ）」を足してみよう
-- 4. n = 4, 5 の貴金属比の長方形も並べてみよう（metallic 4 を使う）

-- ---------- 道具 ----------
-- n 番目の貴金属比
metallic :: Float -> Float
metallic n = (n + sqrt (n * n + 4)) / 2

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし
box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = Polygon [pt env x y, pt env (x + w) y, pt env (x + w) (y + h), pt env x (y + h)]

boxLine :: Env -> Float -> Float -> Float -> Float -> Float -> Picture
boxLine env t x y w h =
  thickPath t [pt env x y, pt env (x + w) y, pt env (x + w) (y + h), pt env x (y + h), pt env x y]

-- 点線の長方形
dashedBox :: Env -> Float -> Float -> Float -> Float -> Picture
dashedBox env x y w h = Pictures
  [ dashed (pt env x y) (pt env (x + w) y), dashed (pt env (x + w) y) (pt env (x + w) (y + h))
  , dashed (pt env (x + w) (y + h)) (pt env x (y + h)), dashed (pt env x (y + h)) (pt env x y) ]

dashed :: Point -> Point -> Picture
dashed a@(ax, ay) b@(bx, by) =
  let len = dist2 a b
      n = floor (len / 12) :: Int
      f t = (ax + (bx - ax) * t, ay + (by - ay) * t)
  in Pictures [ thickLine 2 (f (fromIntegral i * 12 / len)) (f ((fromIntegral i * 12 + 6) / len))
              | i <- [0 .. n], len > 0 ]
