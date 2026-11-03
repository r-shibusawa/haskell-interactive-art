-- ============================================
-- 04 差動二輪ロボットとオドメトリ
-- 左右の車輪の速さ vL, vR だけで動くロボット（お掃除ロボットと同じ）
--   前進速度 v = (vR + vL) / 2,  回転速度 ω = (vR − vL) / W（W = 車輪の間隔）
-- オドメトリ = 車輪の回転から自分の位置を推定する。でも車輪は少しすべる → 推定（薄い）と本当（濃い）がずれていく
-- ↑↓ = 両輪を速く／遅く / ←→ = 左右の差 / スペース = 停止 / N = すべりの大きさ / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Text.Printf (printf)

wheelBase :: Float
wheelBase = 40                 -- ★ 車輪の間隔（px）

slipLevels :: [Float]
slipLevels = [0, 0.03, 0.1]    -- ★ すべりの大きさ（車輪ごとの速度のばらつき）

data Pose = Pose { px :: !Float, py :: !Float, th :: !Float }

data World = World
  { wVL      :: !Float    -- 指令した車輪速度（px/フレーム）
  , wVR      :: !Float
  , wSlipIdx :: !Int
  , wTrue    :: !Pose     -- 本当の姿勢
  , wOdom    :: !Pose     -- オドメトリの推定
  , wTrueTr  :: ![Point]  -- 軌跡（新しい順、1200 点まで）
  , wOdomTr  :: ![Point]
  }

main :: IO ()
main = runSketch (sketch "21-04 Differential Drive" world0)
  { skBack  = hsb 215 35 12
  , skStep  = \env _ w -> step env w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = resetAll (World 0 0 1 start start [] [])
  where start = Pose (width / 2) (height / 2) 0

resetAll :: World -> World
resetAll w = w { wTrue = start, wOdom = start, wTrueTr = [], wOdomTr = [], wVL = 0, wVR = 0 }
  where start = Pose (width / 2) (height / 2) 0

slipOf :: World -> Float
slipOf w = case drop (wSlipIdx w) slipLevels of
  (s : _) -> s
  [] -> 0

-- ★ 差動二輪の運動学：車輪速度 → 姿勢の変化（1 フレーム分）
advance :: Float -> Float -> Pose -> Pose
advance l r p =
  let v = (r + l) / 2
      om = (r - l) / wheelBase
  in p { px = px p + v * cos (th p), py = py p + v * sin (th p), th = th p + om }

step :: Env -> World -> IO World
step env w = do
  g1 <- gauss
  g2 <- gauss
  let slip = slipOf w
      vL = wVL w
      vR = wVR w
      -- 本当の動き：車輪ごとに少しすべる（指令より速かったり遅かったり）
      t0 = advance (vL * (1 + g1 * slip)) (vR * (1 + g2 * slip)) (wTrue w)
      -- オドメトリ：指令どおり動いたと信じて積分
      o0 = advance vL vR (wOdom w)
      -- 画面の端で折り返す（両方同じだけ向きを変える）
      hit = px t0 < 10 || px t0 > width - 10 || py t0 < 90 || py t0 > height - 10
      t1 | hit = t0 { th = th t0 + pi, px = clampF 10 (width - 10) (px t0), py = clampF 90 (height - 10) (py t0) }
         | otherwise = t0
      o1 | hit = o0 { th = th o0 + pi }
         | otherwise = o0
      record = even (envFrame env) && (vL /= 0 || vR /= 0)
      push p tr = if record then take 1200 ((px p, py p) : tr) else tr
  pure w { wTrue = t1, wOdom = o1, wTrueTr = push t1 (wTrueTr w), wOdomTr = push o1 (wOdomTr w) }

drawRobot :: Pose -> Float -> Float -> Picture
drawRobot p hue alpha =
  let c = (px p, py p)
      t = th p
  in Pictures
     [ Color (hsbA hue 60 90 alpha) (bodyRect c t (-22) (-18) 44 36)
     , Color (hsbA 0 0 20 alpha) (Pictures
         [ bodyRect c t (-10) (-wheelBase / 2 - 4) 20 8     -- 左の車輪
         , bodyRect c t (-10) (wheelBase / 2 - 4) 20 8 ])   -- 右の車輪
     , Color (hsbA 0 0 100 alpha) (bodyTri c t (22, 0) (10, -8) (10, 8))   -- 前
     ]

drawAll :: World -> Picture
drawAll w =
  let tp = wTrue w
      op = wOdom w
      vL = wVL w
      vR = wVR w
      v = (vR + vL) / 2
      om = (vR - vL) / wheelBase
      err = dist2 (px tp, py tp) (px op, py op)
      pX = width - 360
      pY = 110
  in Pictures
  [ view
      [ Color (hsbA 45 60 100 40) (thickPath 2 (wOdomTr w))
      , Color (hsbA 170 60 100 90) (thickPath 2 (wTrueTr w))
      , drawRobot op 45 35
      , drawRobot tp 170 100
      -- ずれを線で
      , Color (hsbA 0 80 100 60) (Line [(px tp, py tp), (px op, py op)])
      , Color (hsbA 0 0 100 8) (boxAt (pX - 16) (pY - 30) 350 110)
      ]
  , txt 20 34 16 (hsb 0 0 100) ("vL = " ++ nf 1 vL ++ "   vR = " ++ nf 1 vR ++ " (Up/Down: both, L/R: diff)   Space: stop   v = " ++ nf 1 v
                                ++ "   w = " ++ nf 3 om)
  , txt 20 58 14 (hsb 0 0 100) ("Slip " ++ nf 2 (slipOf w) ++ " (N)   Position error " ++ nf 0 err ++ " px   R: reset")
  , txt 20 78 14 (hsb 0 0 60) "Solid = true pose. Faint = pose estimated from the wheels (odometry). The error keeps growing; sketch 10 corrects it."
  , txt pX pY 15 (hsb 45 90 100) "v = (vR + vL) / 2"
  , txt pX (pY + 24) 15 (hsb 45 90 100) "w = (vR - vL) / W"
  , txt pX (pY + 48) 15 (hsb 45 90 100) "x += v cos t, y += v sin t, t += w"
  , txt pX (pY + 72) 14 (hsb 0 0 60) "vL = vR: straight   vL = -vR: spin"
  ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey k) Down _ _ -> case k of
    KeyUp -> wheels dv dv
    KeyDown -> wheels (-dv) (-dv)
    KeyLeft -> wheels (-dv) dv
    KeyRight -> wheels dv (-dv)
    KeySpace -> w { wVL = 0, wVR = 0 }
    _ -> w
  EventKey (Char c) Down _ _ -> case toLower c of
    'n' -> w { wSlipIdx = (wSlipIdx w + 1) `mod` length slipLevels }
    'r' -> resetAll w
    _ -> w
  _ -> w
  where
    dv = 0.3     -- ★ キー 1 回で変わる速さ
    wheels a b = w { wVL = clampF (-4) 4 (wVL w + a), wVR = clampF (-4) 4 (wVR w + b) }

-- ---------- やってみよう ----------
-- 1. ↑ を 3 回押してまっすぐ走らせる。すべりがあると少しずつ曲がる。なぜ横ではなく向きがずれる？
-- 2. その場回転（← だけ）を 10 秒続けて、本当の向きと推定の向きがどれだけ開くか
-- 3. wheelBase を 10 や 100 にすると曲がりやすさはどう変わる？（ω の式を見る）
-- 4. 直進と回転を交互に繰り返して「四角」を走らせる。元の場所に戻れるか（ロボット競技でよくある課題）


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

-- (px, py) に向き th で置いた機体の中の点を、画面の座標に変える（p5 の translate + rotate）
place :: Point -> Float -> Point -> Point
place (px, py) th (x, y) = (px + x * cos th - y * sin th, py + x * sin th + y * cos th)

-- 機体の中の長方形（左上 (x, y)、幅 bw、高さ bh）
bodyRect :: Point -> Float -> Float -> Float -> Float -> Float -> Picture
bodyRect p th x y bw bh = Polygon (map (place p th) [(x, y), (x + bw, y), (x + bw, y + bh), (x, y + bh)])

-- 機体の中の三角形
bodyTri :: Point -> Float -> Point -> Point -> Point -> Picture
bodyTri p th a b c = Polygon (map (place p th) [a, b, c])

-- 平均 0、標準偏差 1 の正規乱数（p5 の randomGaussian。ボックス・ミュラー法）
gauss :: IO Float
gauss = do
  u <- randomRIO (1e-6, 1)
  v <- randomRIO (0, 1)
  pure (sqrt (-2 * log u) * cos (tau * v))
