-- ============================================
-- 44-00 流線・流跡線・流脈線
-- 流れを「線」で見る 3 つの方法
--   流線   = いまこの瞬間の速度ベクトルをつないだ線（スナップ写真）
--   流跡線 = 1 個の粒子がたどってきた道（長時間露光）
--   流脈線 = 同じ点から次々に出した粒子を、いま結んだ線（煙突のけむり）
-- 定常（時間で変わらない流れ）では 3 本は重なる。非定常にするとばらばらになる
-- 操作：T = 定常／非定常 / 1〜3 = 流れの種類 / クリック = 放出点を置く / R = やり直し
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)

u0 :: Float
u0 = 140            -- ★ 一様流の速さ [px/秒]

swing, swingRate :: Float
swing = 0.6         -- ★ 非定常のとき、流れの向きがゆれる角度 [rad]
swingRate = 1.2     -- ★ ゆれる速さ [rad/秒]

strength :: Float
strength = 15000    -- ★ 渦・湧き出しの強さ（速さ = strength / 距離）

data World = World
  { wT        :: !Float
  , wField    :: !Int           -- 1 = 一様流, 2 = 一様流 + 渦, 3 = 一様流 + 湧き出し
  , wUnsteady :: !Bool
  , wEmit     :: !Point         -- 放出点（p5 の座標）
  , wTracer   :: !Point         -- 流跡線の粒子
  , wPath     :: ![Point]       -- 流跡線（新しい順）
  , wStreak   :: ![Point]       -- 流脈線の粒子（新しい順）
  , wDots     :: ![Point]       -- 背景の粒
  }

fieldNames :: [String]
fieldNames = ["", "uniform flow", "uniform + vortex", "uniform + source"]

main :: IO ()
main = runSketch (sketch "44-00 Streamlines, pathlines, streaklines" (world0 1280 800 []))
  { skBack  = hsb 215 60 10
  , skStart = \env -> do
      ds <- replicateM 260 (randomPoint env)
      pure (world0 (envW env) (envH env) ds)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: Float -> Float -> [Point] -> World
world0 w h ds = let e = (w * 0.22, h * 0.42) in World 0 1 False e e [] [] ds

restart :: World -> World
restart w = w { wStreak = [], wPath = [], wTracer = wEmit w }

-- ★ 速度場：場所 (x, y) と時刻 tt を入れると、そこでの流れの速度 (vx, vy) が返る
velocity :: Env -> World -> Float -> Float -> Float -> Point
velocity env w tt x y =
  let th = if wUnsteady w then swing * sin (swingRate * tt) else 0
      vx = u0 * cos th
      vy = u0 * sin th
      dx = x - envW env / 2
      dy = y - envH env / 2
      r2 = dx * dx + dy * dy + 900
  in case wField w of
       2 -> (vx - strength * dy / r2, vy + strength * dx / r2)   -- 渦：中心のまわりを回る
       3 -> (vx + strength * dx / r2, vy + strength * dy / r2)   -- 湧き出し：中心から外へ
       _ -> (vx, vy)

step :: Env -> Float -> World -> IO World
step env dt0 w = do
  let dt = min dt0 0.033
      t = wT w + dt
      w1 = w { wT = t }
      mv = moveBy env w1 dt
  -- 背景の粒：外に出たらランダムな場所へ
  ds <- mapM (\d -> let d' = mv d in if inside env d' then pure d' else randomPoint env) (wDots w)
  -- 流跡線：1 個の粒子を追いかけ、通った場所を全部おぼえる
  let tr = mv (wTracer w)
      path = tr : wPath w
      (tr', path') = if not (inside env tr) || length path > 700 then (wEmit w, []) else (tr, path)
  -- 流脈線：放出点から毎フレーム粒子を出し、出た順に結ぶ
      streak = take 500 (filter (inside env) (map mv (wEmit w : wStreak w)))
  pure w1 { wDots = ds, wTracer = tr', wPath = path', wStreak = streak }

draw :: Env -> World -> Picture
draw env w = view env $
  [ dot (hsbA 200 30 80 45) x y 3 | (x, y) <- wDots w ]
  -- 流線：いまの速度場を少しずつたどる（時間は止めたまま）
  ++ [ Color (hsbA 200 40 70 35) (Line (streamline env w 2 ((k + 0.5) * h / 13) 260)) | k <- [0 .. 12] ]
  ++ (if wField w > 1
        then [ Color (hsbA 200 40 70 35) (Line (streamline env w (envW env / 2 + 45 * cos (k * 0.785)) (h / 2 + 45 * sin (k * 0.785)) 200))
             | k <- [0 .. 7] ]
        else [])
  ++ [ path 3 (hsb 180 80 100) (uncurry (streamline env w) (wEmit w) 260)
     , path 3 (hsb 42 80 100) (wPath w)
     , dot (hsb 42 80 100) tx ty 12
     , path 3 (hsb 350 60 100) (wStreak w)
     , Color (hsb 0 0 100) (Translate ex ey (ring 9 2))
     , txt 20 40 18 (hsb 0 0 100)
         (name ++ " (1-3)   " ++ (if wUnsteady w then "UNSTEADY: direction swings in time" else "STEADY: flow does not change")
          ++ " (T)   click = emitter   R = reset")
     , txt 20 68 15 (hsb 0 0 65)
         (if wUnsteady w
            then "The 3 lines split: streamline = now, pathline = one particle's past, streakline = all particles from one point, now"
            else "Steady flow, so all 3 lines lie on top of each other. Press T to make the flow swing.")
     , txt lx ly 16 (hsb 180 80 100) "--- streamline"
     , txt lx (ly + 26) 16 (hsb 42 80 100) "--- pathline"
     , txt lx (ly + 52) 16 (hsb 350 60 100) "--- streakline"
     ]
  where
    h = envH env
    (tx, ty) = wTracer w
    (ex, ey) = wEmit w
    lx = envW env - 250
    ly = h - 100
    name = case drop (wField w) fieldNames of
      (s : _) -> s
      _ -> ""

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
    in restart w { wEmit = (clampF 10 (envW env - 10) mx, clampF 90 (envH env - 10) my) }
  EventKey (Char k) Down _ _
    | k `elem` "123" -> restart w { wField = fromEnum k - fromEnum '0' }
    | k `elem` "tT" -> restart w { wUnsteady = not (wUnsteady w) }
    | k `elem` "rR" -> restart w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 定常のまま 1〜3 を切り替える。3 本が重なったままなのを確かめる
-- 2. T で非定常に。流脈線（ピンク）は「旗」のようにうねる。流線（水色）はまっすぐなのに、なぜ？
-- 3. swingRate を 3 にする／swing を 1.2 にする。うねりの波長はどう変わる？
-- 4. 2 の渦のすぐ近くに放出点を置く。粒子は渦につかまる？ strength を 2 倍にすると？
-- 5. velocity の case に自分の流れを足してみよう（例：vy に 60 * sin (x / 80) を足す）

-- ---------- 道具（線をたどる） ----------
-- 粒子を dt 秒だけ流す（中点法：半歩先の速度で進めるとずれが小さい）
moveBy :: Env -> World -> Float -> Point -> Point
moveBy env w dt (x, y) =
  let t = wT w
      (ax, ay) = velocity env w t x y
      (bx, by) = velocity env w (t + dt / 2) (x + ax * dt / 2) (y + ay * dt / 2)
  in (x + bx * dt, y + by * dt)

-- 流線：(x, y) から、いまの速度の向きへ 8 px ずつ n 回進んだ点の列
streamline :: Env -> World -> Float -> Float -> Int -> [Point]
streamline env w x0 y0 n0 = (x0, y0) : go x0 y0 n0
  where
    t = wT w
    go _ _ 0 = []
    go x y n =
      let (ax, ay) = velocity env w t x y
          ma = sqrt (ax * ax + ay * ay) + 1e-6
          (bx, by) = velocity env w t (x + ax / ma * 4) (y + ay / ma * 4)
          mb = sqrt (bx * bx + by * by) + 1e-6
          x' = x + bx / mb * 8
          y' = y + by / mb * 8
      in if x' < -20 || x' > envW env + 20 || y' < -20 || y' > envH env + 20 then []
         else (x', y') : go x' y' (n - 1 :: Int)

inside :: Env -> Point -> Bool
inside env (x, y) = x > -20 && x < envW env + 20 && y > -20 && y < envH env + 20

randomPoint :: Env -> IO Point
randomPoint env = do
  x <- randomRIO (0, envW env)
  y <- randomRIO (0, envH env)
  pure (x, y)

-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = Color c (Pictures (zipWith (thickLine w) ps (drop 1 ps)))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))
