-- ============================================
-- 44-04 円柱まわりの流れと循環 ─ クッタ・ジューコフスキーの定理
-- 一様流 U の中の円柱（半径 a）に、循環 Γ（まわりを回る流れ）を足す
--   ψ = U (r − a²/r) sin θ + (Γ / 2π) ln(r / a)
-- Γ = 0 なら上下対称で力はゼロ。Γ を足すと上が速く・下が遅くなり、圧力差で持ち上がる
--   揚力 L = ρ U Γ（形によらない！）　　回るボールが曲がる「マグヌス効果」と同じ
-- 操作：上下キー = 循環 Γ / 左右キー = 流速 U / R = 元に戻す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import qualified Data.IntSet as IS
import Data.Array (listArray, (!))
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Numeric (showFFloat)

radius0 :: Float
radius0 = 90        -- ★ 円柱の半径 [px]

rho :: Float
rho = 1             -- ★ 密度

spacing :: Float
spacing = 22        -- ★ 流線の間かく [px]

gx :: Int
gx = 129            -- 圧力の色と流線を計算する格子の横の点の数

data World = World
  { wU     :: !Float
  , wGam   :: !Float        -- 循環 Γ ÷ (4πUa)。1 をこえると淀み点が円柱からはなれる
  , wDots  :: ![Point]
  , wLayer :: !Picture      -- 圧力の色（描き直したときだけ作る）
  , wLines :: ![[Point]]    -- 流線
  }

main :: IO ()
main = runSketch (sketch "44-04 Flow past a cylinder with circulation" (World 120 0 [] Blank []))
  { skBack  = hsb 215 60 10
  , skStart = \env -> do
      ds <- replicateM 320 ((,) <$> randomRIO (0, envW env) <*> randomRIO (0, envH env))
      pure (rebuild env (World 120 0 ds Blank []))
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

radius :: Env -> Float
radius env = min radius0 (envH env * 0.14)

center :: Env -> Point
center env = (envW env * 0.45, envH env * 0.54)

circulation :: Env -> World -> Float
circulation env w = wGam w * 4 * pi * wU w * radius env

-- ★ 流れ関数 ψ と速度（x 右向き・y 上向きの座標。円柱の中心が原点）
flowAt :: Env -> World -> Float -> Float -> (Float, Float, Float)
flowAt env w x y =
  let a = radius env
      u = wU w
      r2 = max (x * x + y * y) (a * a)
      r = sqrt r2
      sn = y / r
      cs = x / r
      g = circulation env w / (2 * pi)
      psi = u * (r - a * a / r) * sn + g * log (r / a)
      vr = u * (1 - a * a / r2) * cs           -- 半径方向
      vt = -u * (1 + a * a / r2) * sn - g / r  -- 周方向（反時計回りが正）
  in (psi, vr * cs - vt * sn, vr * sn + vt * cs)

-- 圧力の色と流線を計算し直す（Γ か U を変えたときだけ）
rebuild :: Env -> World -> World
rebuild env w =
  let cell = envW env / fromIntegral (gx - 1)
      gy = ceiling (envH env / cell) + 1
      (cx, cy) = center env
      u = wU w
      vals = U.generate (gx * gy) (\n -> let (j, i) = n `divMod` gx
                                         in flowAt env w (fromIntegral i * cell - cx) (cy - fromIntegral j * cell))
      psiG = U.map (\(p, _, _) -> p / (u * spacing)) vals
      -- ベルヌーイの定理：圧力係数 Cp = (p − p∞) / (½ρU²) = 1 − (v/U)²
      colorAt i j =
        let (_, vx, vy) = vals U.! (i + j * gx)
            cp = clampF (-3) 1 (1 - (vx * vx + vy * vy) / (u * u))
            hot = max cp 0          -- 赤 = 高圧
            cold = max (-cp / 3) 0  -- 青 = 低圧
        in (b8 (14 + 200 * hot), b8 (26 + 40 * hot + 90 * cold), b8 (46 + 170 * cold))
      iw = fromIntegral gx * cell
      ih = fromIntegral gy * cell
      img = Translate (iw / 2 - envW env / 2) (envH env / 2 - ih / 2) (pixelImage gx gy cell colorAt)
  in w { wLayer = img
       , wLines = contourChains gx gy (\i j -> (fromIntegral i * cell, fromIntegral j * cell)) psiG 0 }

step :: Env -> Float -> World -> IO World
step env dt0 w = do
  let dt = min dt0 0.033
      (cx, cy) = center env
      a = radius env
  -- 流れに乗る粒
  ds <- mapM (\(x0, y0) -> do
      let (_, vx, vy) = flowAt env w (x0 - cx) (cy - y0)
          x = x0 + vx * dt
          y = y0 - vy * dt
      if x > envW env || y < 0 || y > envH env || dist2 (x, y) (cx, cy) < a || isNaN x || isNaN y
        then (,) 0 <$> randomRIO (0, envH env)
        else pure (x, y)) (wDots w)
  pure w { wDots = ds }

draw :: Env -> World -> Picture
draw env w = Pictures
  [ wLayer w
  , view env $
      [ Color (hsbA 0 0 100 55) (Pictures (map Line (wLines w))) ]
      ++ [ dot (hsbA 0 0 100 80) x y 3.5 | (x, y) <- wDots w ]
      -- 円柱の表面：圧力が押す力（矢印）。外向き = 高圧、内向き = 低圧（吸われる）
      ++ [ Color (hsb (if cp > 0 then 8 else 205) 80 100) (thickLine 2 (px, py) (px + len * cos th, py - len * sin th))
         | (th, cp) <- surface
         , let px = cx + a * cos th
         , let py = cy - a * sin th
         , let len = clampF (-6) 1 cp * 14 ]
      ++ [ dot (hsb 215 40 22) cx cy (2 * a)
         , Color (hsb 0 0 100) (Translate cx cy (ring a 2)) ]
      -- 回転の向き
      ++ (if abs gam > 0.01
            then let s = if gam > 0 then 5.2 else 0.4
                     d = if gam > 0 then 1 else -1
                     ex = cx + a / 2 * cos s
                     ey = cy + a / 2 * sin s
                 in [ path 3 (hsb 42 80 100) [ (cx + a / 2 * cos t, cy + a / 2 * sin t) | t <- [0.4, 0.5 .. 5.2] ]
                    , Color (hsb 42 80 100) (thickLine 3 (ex, ey) (ex - 12 * d, ey - 10)) ]
            else [])
      -- 淀み点（速さ 0 の点）：sin θ = −Γ / (4πUa)
      ++ (if abs gam <= 1
            then let ths = asin (-gam)
                 in [ dot (hsb 42 90 100) (cx + a * cos ths) (cy - a * sin ths) 12
                    , dot (hsb 42 90 100) (cx - a * cos ths) (cy - a * sin ths) 12 ]
            else let rs = a * (abs gam + sqrt (gam * gam - 1))   -- 円柱をはなれた淀み点
                 in [ dot (hsb 42 90 100) cx (cy + (if gam > 0 then rs else -rs)) 12 ])
      -- 揚力の矢印
      ++ (if abs len > 2
            then let hy = cy - len + (if len > 0 then 12 else -12)
                 in [ Color (hsb 130 70 100) (Pictures [ thickLine 5 (cx, cy) (cx, cy - len)
                                                       , thickLine 5 (cx, cy - len) (cx - 9, hy)
                                                       , thickLine 5 (cx, cy - len) (cx + 9, hy) ]) ]
            else [])
      ++ [ txt 20 40 18 (hsb 0 0 100)
             ("Circulation Gamma = " ++ f2 gam ++ " x 4 pi U a (Up/Down)   U = " ++ f0 u ++ " (Left/Right)   R = reset")
         , txt 20 68 15 (hsb 0 0 80)
             ("Lift L = rho U Gamma = " ++ f3 (lift / 1e6) ++ "  /  sum of surface pressure = " ++ f3 (liftSum / 1e6)
              ++ "  (x 1e6)   yellow = stagnation points   green = lift")
         , txt 20 (envH env - 24) 15 (hsb 0 0 80)
             (if abs gam < 0.01 then "Symmetric: pressures balance and the force is zero (d'Alembert's paradox)"
              else if abs gam <= 1 then "Faster on top -> lower pressure on top (blue) -> pulled upward"
              else "The stagnation point leaves the cylinder: fluid keeps circling around it") ]
  ]
  where
    (cx, cy) = center env
    a = radius env
    u = wU w
    gam = wGam w
    g = circulation env w
    n = 72 :: Int
    -- 表面の点の角度と圧力係数
    surface = [ (th, 1 - vt * vt / (u * u))
              | k <- [0 .. n - 1]
              , let th = (fromIntegral k + 0.5) * 2 * pi / fromIntegral n
              , let vt = -2 * u * sin th - g / (2 * pi * a) ]   -- 表面の速さ
    -- 圧力 × 面積 × 上向き成分 を足し合わせる
    liftSum = sum [ -(0.5 * rho * u * u * cp) * sin th * a * (2 * pi / fromIntegral n) | (th, cp) <- surface ]
    lift = rho * u * g        -- ★ クッタ・ジューコフスキーの定理
    len = clampF (-220) 220 (lift / (rho * u * u * a) * 12)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> upd w { wGam = min 1.6 (wGam w + 0.1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> upd w { wGam = max (-1.6) (wGam w - 0.1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> upd w { wU = min 240 (wU w + 20) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> upd w { wU = max 40 (wU w - 20) }
  EventKey (Char k) Down _ _ | k `elem` "rR" -> upd w { wGam = 0, wU = 120 }
  _ -> w
  where
    upd v = rebuild env v { wGam = if abs (wGam v) < 0.001 then 0 else wGam v }

-- ---------- やってみよう ----------
-- 1. Γ = 0 で淀み点（黄）が前後にあるのを確認。上キーで Γ を増やすと 2 つの淀み点はどう動く？
-- 2. Γ = 1.00 で 2 つが真下で 1 つに。それより増やすと？
-- 3. 「ρUΓ」と「圧力を足し合わせた値」がいつも一致することを確かめる（定理の中身）
-- 4. U を 2 倍にすると揚力は何倍？（Γ も 4πUa に比例させているので…）
-- 5. 下キーで Γ を負に。野球のカーブ・サッカーの無回転でないシュートを思い出そう

-- ---------- 道具（等高線を描く） ----------
-- 等高線（マーチングスクエア法）：nx × ny の格子の値 f が「整数」になる所を線で結ぶ。
-- pos i j = 格子点の画面座標。wrap > 0 なら、値が wrap だけ飛ぶ「切れ目」をまたいでつなぐ。
-- 小さな線分をたくさん描くと遅いので、端がつながる線分を 1 本の折れ線にまとめて返す
contourChains :: Int -> Int -> (Int -> Int -> Point) -> U.Vector Float -> Float -> [[Point]]
contourChains nx ny pos f wrap = chainSegments segs
  where
    segs = concat [ cellSegs i j | j <- [0 .. ny - 2], i <- [0 .. nx - 2] ]
    -- 格子の辺の番号（横の辺は偶数、縦の辺は奇数）
    hEdge i j = 2 * (i + j * nx)
    vEdge i j = 2 * (i + j * nx) + 1
    cellSegs i j =
      let raw = [f U.! (i + j * nx), f U.! (i + 1 + j * nx), f U.! (i + 1 + (j + 1) * nx), f U.! (i + (j + 1) * nx)]
          c0 = head raw
          c = c0 : [ if wrap > 0 then v - wrap * fromIntegral (round ((v - c0) / wrap) :: Int) else v | v <- drop 1 raw ]
          lo = minimum c
          hi = maximum c
          ps = [pos i j, pos (i + 1) j, pos (i + 1) (j + 1), pos i (j + 1)]
          es = [hEdge i j, vEdge (i + 1) j, hEdge i (j + 1), vEdge i j]
          corner = zip3 c ps es
          sides = zip corner (drop 1 corner ++ take 1 corner)
          crossings lv = [ ((e, lv), (ax + s * (bx - ax), ay + s * (by - ay)))
                         | ((a, (ax, ay), e), (b, (bx, by), _)) <- sides
                         , (a < fromIntegral lv) /= (b < fromIntegral lv)
                         , let s = (fromIntegral lv - a) / (b - a) ]
          pairUp (p : q : rest) = (p, q) : pairUp rest
          pairUp _ = []
      in if not (hi - lo < 5) then []   -- 特異点のすぐそば（値が急変）は描かない
         else concat [ pairUp (crossings lv) | lv <- [ceiling lo .. floor hi :: Int] ]

-- 端点（辺の番号と高さの組）を共有する線分をつないで折れ線にする
chainSegments :: [(((Int, Int), Point), ((Int, Int), Point))] -> [[Point]]
chainSegments segList = go 0 IS.empty
  where
    n = length segList
    segV = listArray (0, max 0 (n - 1)) segList
    key (e, lv) = e * 65536 + (lv `mod` 65536)   -- 辺と高さの組を 1 つの整数に
    adj = IM.fromListWith (++) (concat [ [(key ka, [s]), (key kb, [s])] | (s, ((ka, _), (kb, _))) <- zip [0 ..] segList ])
    seg s = segV ! s
    go i used
      | i >= n = []
      | IS.member i used = go (i + 1) used
      | otherwise =
          let ((ka, pa), (kb, pb)) = seg i
              (fwd, u1) = extend kb (IS.insert i used)
              (back, u2) = extend ka u1
          in (reverse back ++ [pa, pb] ++ fwd) : go (i + 1) u2
    -- 端点 k から、まだ使っていない線分をたどれるだけたどる
    extend k used = case [ t | t <- IM.findWithDefault [] (key k) adj, not (IS.member t used) ] of
      (t : _) ->
        let ((ka, pa), (kb, pb)) = seg t
            (nk, np) = if ka == k then (kb, pb) else (ka, pa)
            (rest, u') = extend nk (IS.insert t used)
        in (np : rest, u')
      [] -> ([], used)

-- ---------- 道具 ----------
f0, f2, f3 :: Float -> String
f0 v = show (round v :: Int)
f2 v = showFFloat (Just 2) v ""
f3 v = showFFloat (Just 3) v ""

b8 :: Float -> Word8
b8 v = fromIntegral (max 0 (min 255 (round v :: Int)))

view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

path :: Float -> Color -> [Point] -> Picture
path w c ps = Color c (Pictures (zipWith (thickLine w) ps (drop 1 ps)))

dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))
