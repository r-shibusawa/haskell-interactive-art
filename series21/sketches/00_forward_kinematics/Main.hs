-- ============================================
-- 00 2 リンクアームの順運動学（FK）
-- 関節の角度 (q1, q2) を決めると、手先の位置 (x, y) は三角関数で決まる
--   x = L1 cos q1 + L2 cos(q1 + q2),  y = L1 sin q1 + L2 sin(q1 + q2)
-- ←→ = 第 1 関節 q1 / ↑↓ = 第 2 関節 q2 / W = 届く範囲（作業空間）を表示 / T = 手先の軌跡を消す / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

l1, l2 :: Float
l1 = 180                  -- ★ リンクの長さ（px）
l2 = 140

turnSpeed :: Float
turnSpeed = 0.025         -- ★ キーを押したときの回る速さ

data World = World
  { wQ1     :: !Float     -- 関節角（ラジアン）
  , wQ2     :: !Float
  , wTrail  :: ![Point]   -- 手先の軌跡（新しい順、600 点まで）
  , wShowWs :: !Bool      -- 作業空間を表示するか
  }

main :: IO ()
main = runSketch (sketch "21-00 Forward Kinematics" world0)
  { skBack  = hsb 215 35 12
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = World 0.6 0.9 [] False

-- 台座の位置（画面の下のほう、少し左）
basePos :: Point
basePos = (width * 0.38, height * 0.62)

-- ★ 順運動学：角度 → ひじと手先の位置（画面は y が下向きなので sin にマイナス）
forward :: Float -> Float -> (Point, Point)
forward a1 a2 =
  let (bx, by) = basePos
      ex = bx + l1 * cos a1
      ey = by - l1 * sin a1
      hx = ex + l2 * cos (a1 + a2)
      hy = ey - l2 * sin (a1 + a2)
  in ((ex, ey), (hx, hy))

-- キーを押している間、角度が変わる
step :: Env -> World -> World
step env w =
  let key k d = if isDown env (SpecialKey k) then d else 0
      q1 = wQ1 w + key KeyLeft turnSpeed + key KeyRight (-turnSpeed)
      q2 = wQ2 w + key KeyUp turnSpeed + key KeyDown (-turnSpeed)
      (_, hand) = forward q1 q2
  in w { wQ1 = q1, wQ2 = q2, wTrail = take 600 (hand : wTrail w) }

drawAll :: World -> Picture
drawAll w =
  let q1 = wQ1 w
      q2 = wQ2 w
      b@(bx, by) = basePos
      (e@(ex, ey), h@(hx, hy)) = forward q1 q2
      bigX = hx - bx      -- 台座を原点、y は上向き
      bigY = by - hy
      px = width - 440
      py = 110
  in Pictures
  [ if wShowWs w then wsImage else Blank
  , view
      [ Color (hsbA 170 70 100 60) (thickPath 2 (wTrail w))     -- 軌跡
      , Color (hsb 0 0 40) (boxAt (bx - 30) by 60 14)           -- 台座
      , Color (hsb 200 60 95) (thickLine 14 b e)                -- リンク 1
      , Color (hsb 320 60 95) (thickLine 14 e h)                -- リンク 2
      , Color (hsb 0 0 100) (Pictures [at b (disc 11), at e (disc 9)])   -- 関節
      , Color (hsb 45 90 100) (at h (disc 8))
      -- 角度の弧
      , Color (hsbA 200 60 95 70) (thickPath 2 (arcPts b 35 (-q1) 0))
      , Color (hsbA 320 60 95 70) (thickPath 2 (arcPts e 30 (-(q1 + q2)) (-q1)))
      , Color (hsbA 0 0 50 50) (thickLine 2 b (bx + 60, by))
      , Color (hsbA 0 0 50 50) (thickLine 2 e (ex + 50 * cos q1, ey - 50 * sin q1))
      -- 式のパネル
      , Color (hsbA 0 0 100 8) (boxAt (px - 16) (py - 30) 430 150)
      ]
  , txt 20 40 16 (hsb 0 0 100) ("q1 = " ++ degStr q1 ++ " deg (L/R)   q2 = " ++ degStr q2 ++ " deg (Up/Down)   W: reach "
                                ++ onOff (wShowWs w) ++ "   T: clear trail   R: reset")
  , txt 20 66 14 (hsb 0 0 60) "Angles fix the hand position exactly (forward kinematics). Sketch 01 goes the other way."
  , txt px py 16 (hsb 0 0 100) "x = L1 cos q1 + L2 cos(q1 + q2)"
  , txt px (py + 26) 16 (hsb 0 0 100) "y = L1 sin q1 + L2 sin(q1 + q2)"
  , txt px (py + 62) 16 (hsb 45 90 100) ("x = " ++ nf 0 l1 ++ "*" ++ nf 2 (cos q1) ++ " + " ++ nf 0 l2 ++ "*" ++ nf 2 (cos (q1 + q2)) ++ " = " ++ nf 0 bigX)
  , txt px (py + 88) 16 (hsb 45 90 100) ("y = " ++ nf 0 l1 ++ "*" ++ nf 2 (sin q1) ++ " + " ++ nf 0 l2 ++ "*" ++ nf 2 (sin (q1 + q2)) ++ " = " ++ nf 0 bigY)
  , txt px (py + 112) 14 (hsb 0 0 60) ("Reach: a donut of radius " ++ nf 0 (abs (l1 - l2)) ++ " to " ++ nf 0 (l1 + l2))
  ]

-- 作業空間：q1, q2 をすべて試して手先の点を打つ（ドーナツ形になる）
-- 毎フレーム計算しないように、1 枚の画像に数えて作っておく（p5 の wsLayer の代わり）
wsImage :: Picture
wsImage = at (bx - width / 2, height / 2 - by) (pixelImageF n n k pixel)
  where
    (bx, by) = basePos
    n = 260 :: Int        -- ★ 画像の大きさ（マス）
    k = 2.5 :: Float      -- 1 マスの大きさ（px）
    half = fromIntegral n * k / 2
    angles = takeWhile (< tau) (map (* 0.05) [0 ..])
    cellOf (x, y) = floor ((x - bx + half) / k) + floor ((y - by + half) / k) * n
    counts :: U.Vector Int
    counts = U.accum (+) (U.replicate (n * n) 0)
      [ (cellOf (snd (forward a1 a2)), 1) | a1 <- angles, a2 <- angles ]
    (br, bg, bb, _) = rgbaOfColor (hsb 215 35 12)
    (cr, cg, cb, _) = rgbaOfColor (hsb 45 80 100)
    pixel x y =
      -- 透明度 18% の点を c 個重ねたときの濃さ
      let c = counts U.! (x + y * n)
          a = 1 - 0.82 ^ c
      in (lerpF br cr a, lerpF bg cg a, lerpF bb cb a)

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _ -> case toLower c of
    'w' -> w { wShowWs = not (wShowWs w) }
    't' -> w { wTrail = [] }
    'r' -> w { wQ1 = 0.6, wQ2 = 0.9, wTrail = [] }
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. q2 = 0 のときと q2 = 180° のとき、手先はどこ？ 届く範囲の外側と内側の円の半径を確かめよう
-- 2. l1 = l2 にすると内側の穴が消える。l1 と l2 を変えて W で作業空間を見比べよう
-- 3. q1 だけ回すと手先は円を描く。q1 と q2 を同時に回すと？（軌跡を観察）
-- 4. 3 本目のリンク l3 と q3 を足して、forward の式を 3 項にしてみよう


-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は p5 の text と同じ左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 左上の角が (x, y) の長方形（p5 の rect）
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y bw bh = rect (x + bw / 2) (y + bh / 2) bw bh

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 小数点以下 d 桁の文字列（p5 の nf）
nf :: Int -> Float -> String
nf d = printf ("%." ++ show d ++ "f")

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- 中心 c、半径 r、角度 a から b まで時計回りの弧（p5 の arc と同じ向き）
arcPts :: Point -> Float -> Float -> Float -> [Point]
arcPts (cx, cy) r a0 b0 =
  let a = wrapF tau a0
      b1 = wrapF tau b0
      b = if b1 < a then b1 + tau else b1
      n = max 2 (ceiling ((b - a) / 0.1)) :: Int
  in [ (cx + r * cos t, cy + r * sin t)
     | i <- [0 .. n], let t = a + (b - a) * fromIntegral i / fromIntegral n ]

-- 角度を度にして -360〜360 の範囲で表示（JavaScript の % と同じく符号を残す）
degStr :: Float -> String
degStr q =
  let d = q * 180 / pi
      k = fromIntegral (truncate (d / 360) :: Int)
  in nf 0 (d - 360 * k)
