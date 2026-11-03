-- ============================================
-- 11 ボロノイで動く群れと領域（なわばり）
-- 動き回る生き物のなわばり = 毎フレーム作り直すボロノイ図。下のグラフは各自のなわばりの広さの移り変わり
--   クリックで「捕食者」を放す：いちばん近い獲物を追い、近くの獲物は逃げる → なわばりの形が大きく変わる
-- クリック = 捕食者 / A = 獲物を追加 / スペース = 一時停止 / ↑↓ = 速さ / C = 捕食者を消す / R = 最初から
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (foldl', sortOn)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Text.Printf (printf)

nPrey :: Int
nPrey = 28                -- ★ 獲物の数

fear :: Float
fear = 160                -- ★ 捕食者から逃げはじめる距離

-- 動ける範囲
bx0, by0, bx1, by1 :: Float
bx0 = 0
by0 = 90
bx1 = width
by1 = height - 150

-- 面積のグラフの大きさ。画面では 2 倍に拡大する（2 フレームに 1 列ずつ描き足すと 1 px / フレームで流れる）
stripW, stripH :: Int
stripW = 640
stripH = 70

data Prey = Prey { pX, pY, pVx, pVy, pHue :: !Float }

data World = World
  { wPrey  :: ![Prey]
  , wPreds :: ![Point]
  , wSpeed :: !Float      -- ★ 獲物の速さ
  , wPause :: !Bool
  , wCells :: ![[Point]]
  , wStrip :: !(U.Vector Word8)   -- 面積のグラフ（列ごとに RGB を並べたもの）
  , wTick  :: !Int
  , wEaten :: !Int
  }

main :: IO ()
main = runSketch (sketch "22-11 Moving Territories" world0)
  { skBack  = hsb 230 25 12
  , skStart = \_ -> reset world0
  , skStep  = \_ _ w -> step w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World [] [] 1.5 False [] (U.concat (replicate (stripW * stripH) stripBack)) 0 0

stripBack :: U.Vector Word8
stripBack = let (r, g, b) = hsbToRgb 230 25 8 in U.fromList [w8 r, w8 g, w8 b]

randomPrey :: IO Prey
randomPrey = do
  x <- randomRIO (bx0 + 20, bx1 - 20)
  y <- randomRIO (by0 + 20, by1 - 20)
  a <- randomRIO (0, tau)
  h <- randomRIO (0, 360)
  pure (Prey x y (cos a) (sin a) h)

reset :: World -> IO World
reset w = do
  ps <- replicateM nPrey randomPrey
  pure (rebuild w { wPrey = ps, wPreds = [], wEaten = 0 })

rebuild :: World -> World
rebuild w = w { wCells = map (\p -> voronoiCell sites p bx0 by0 bx1 by1) sites }
  where sites = map (\p -> (pX p, pY p)) (wPrey w)

movePrey :: Float -> [Point] -> Prey -> IO Prey
movePrey speed preds p0 = do
  -- 捕食者から逃げる
  let flee p (qx, qy) =
        let dx = pX p - qx
            dy = pY p - qy
            d = sqrt (dx * dx + dy * dy) + 0.01
        in if d < fear then p { pVx = pVx p + dx / d * 0.3, pVy = pVy p + dy / d * 0.3 } else p
      p1 = foldl' flee p0 preds
  ax <- randomRIO (-0.1, 0.1)
  ay <- randomRIO (-0.1, 0.1)
  let vx0 = pVx p1 + ax
      vy0 = pVy p1 + ay
      v0 = sqrt (vx0 * vx0 + vy0 * vy0)
      v = if v0 == 0 then 1 else v0
      vmax = if null preds then 1 else 1.6
      vx = vx0 / v * min v vmax
      vy = vy0 / v * min v vmax
      x = pX p1 + vx * speed
      y = pY p1 + vy * speed
      -- 壁ではね返る
      (x', vx') | x < bx0 + 5 = (bx0 + 5, abs vx)
                | x > bx1 - 5 = (bx1 - 5, negate (abs vx))
                | otherwise = (x, vx)
      (y', vy') | y < by0 + 5 = (by0 + 5, abs vy)
                | y > by1 - 5 = (by1 - 5, negate (abs vy))
                | otherwise = (y, vy)
  pure p1 { pX = x', pY = y', pVx = vx', pVy = vy' }

-- 捕食者：いちばん近い獲物を追う。つかまえたら食べる
movePred :: Float -> ([Point], [Prey], Int) -> Point -> ([Point], [Prey], Int)
movePred speed (done, prey, eaten) q@(qx, qy) =
  let pick (bi, bd) (i, p) = let d = dist2 (pX p, pY p) q in if d < bd then (i, d) else (bi, bd)
      (best, _) = foldl' pick (-1 :: Int, 1 / 0 :: Float) (zip [0 ..] prey)
  in case drop best prey of
       (b : _) | best >= 0 ->
         let dx = pX b - qx
             dy = pY b - qy
             d = sqrt (dx * dx + dy * dy) + 0.01
             q' = (qx + dx / d * speed * 1.25, qy + dy / d * speed * 1.25)
         in if d < 10 && length prey > 3
              then (q' : done, [p | (i, p) <- zip [0 ..] prey, i /= best], eaten + 1)
              else (q' : done, prey, eaten)
       _ -> (q : done, prey, eaten)

step :: World -> IO World
step w
  | wPause w = pure (rebuild w)
  | otherwise = do
      prey1 <- mapM (movePrey (wSpeed w) (wPreds w)) (wPrey w)
      let (preds', prey2, eaten) = foldl' (movePred (wSpeed w)) ([], prey1, wEaten w) (wPreds w)
          w1 = rebuild w { wPrey = prey2, wPreds = reverse preds', wEaten = eaten, wTick = wTick w + 1 }
      pure (if even (wTick w1) then addColumn w1 else w1)

-- 面積のグラフ：獲物ごとの割合を積み上げて 1 列描く（左へ流れる）
addColumn :: World -> World
addColumn w = w { wStrip = U.drop (stripH * 3) (wStrip w) U.++ column }
  where
    total = (bx1 - bx0) * (by1 - by0)
    order = sortOn (pHue . fst) (zip (wPrey w) (wCells w))
    -- 上から順に (下の端の y, 色) を並べる
    bands = drop 1 (scanl (\(y, _) (p, c) -> (y + polyArea c / total * 130, pHue p)) (5, 0) order)
    column = U.concat (map rowColor [0 .. stripH - 1])
    rowColor r =
      let y = (fromIntegral r + 0.5) * 2
      in if y < 5 || y >= 135 then stripBack else
         case dropWhile (\(e, _) -> e <= y) bands of
           ((_, h) : _) -> let (a, b, c) = hsbToRgb h 45 75 in U.fromList [w8 a, w8 b, w8 c]
           [] -> stripBack

drawAll :: World -> Picture
drawAll w = Pictures
  [ view (cellPics ++ [dotAt (gray 1) (pX p, pY p) 9 | p <- wPrey w] ++ concatMap predPic (wPreds w))
  , Translate 0 (height / 2 - (height - 145 + 70)) stripImg
  , txt 20 30 16 (gray 1) ("Click: release predator  A: add prey  Space: " ++ (if wPause w then "resume" else "pause") ++ "  Up/Down: speed " ++ printf "%.1f" (wSpeed w)
      ++ "  C: clear predators  R: reset")
  , txt 20 52 14 (hsb 0 0 65) ("Prey " ++ show (length (wPrey w)) ++ "  Eaten " ++ show (wEaten w) ++ "  Territory: largest " ++ printf "%.1f" (mx / total * 100) ++ "%, smallest " ++ printf "%.1f" (mn / total * 100)
      ++ "% (mean " ++ printf "%.1f" (100 / fromIntegral (max 1 (length (wPrey w))) :: Float) ++ "%)")
  , txt 20 74 14 (hsb 0 0 65) "The band below shows area shares over time. Predators make prey huddle, so territories become uneven."
  ]
  where
    total = (bx1 - bx0) * (by1 - by0)
    areas = map polyArea (wCells w)
    mx = foldl' max 0 areas
    mn = if null areas then 0 else foldl' min (1 / 0) areas
    cellPics =
      [ Pictures [Color (hsb (pHue p) 45 55) (Polygon c), Color (hsbA 0 0 100 70) (outline 1.5 c)]
      | (p, c) <- zip (wPrey w) (wCells w), length c > 2 ]
    predPic q@(x, y) =
      [ Color (hsb 0 90 100) (Polygon [(x, y - 12), (x - 10, y + 8), (x + 10, y + 8)])
      , Color (hsbA 0 90 100 30) (at q (ring fear 1.5)) ]
    strip = wStrip w
    stripImg = pixelImage stripW stripH 2 $ \x y ->
      let i = (x * stripH + y) * 3
      in (U.unsafeIndex strip i, U.unsafeIndex strip (i + 1), U.unsafeIndex strip (i + 2))

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let m@(_, y) = toP5 pos
    in pure (if y > by0 && y < by1 && length (wPreds w) < 5 then w { wPreds = wPreds w ++ [m] } else w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPause = not (wPause w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 5 (wSpeed w + 0.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 0 (wSpeed w - 0.5) }
  EventKey (Char c) Down _ _ -> case toLower c of
    'a' -> do ps <- replicateM 5 randomPrey
              pure w { wPrey = wPrey w ++ ps }
    'c' -> pure w { wPreds = [] }
    'r' -> reset w
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 捕食者なしで、なわばりの広さの差（最大と最小）はどれくらい？ 捕食者を 3 匹にすると？
-- 2. 獲物に「自分のセルの重心へ少し向かう」力（06 のロイド）を足すと、なわばりは公平になる？
-- 3. 自分のなわばりが平均より狭い獲物は弱って遅くなる、というルールを足してみよう
-- 4. 獲物を「お店」、捕食者を「大型店」と思って、商圏の奪い合いの物語にしてみよう

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- マウスの位置（gloss の座標：中央が原点、y は上向き）を p5 と同じ座標に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は p5 の text と同じ左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 直径 d の円（p5 の circle と同じく直径で書く）
dotAt :: Color -> Point -> Float -> Picture
dotAt c p d = Color c (at p (disc (d / 2)))

-- 閉じた多角形の太い枠
outline :: Float -> [Point] -> Picture
outline t ps = thickPath t (ps ++ take 1 ps)

-- 色の変換（HSB → RGB 0〜255）
hsbToRgb :: Float -> Float -> Float -> (Float, Float, Float)
hsbToRgb h s0 b0 = (f 5, f 3, f 1)
  where
    s = s0 / 100
    b = b0 / 100
    f :: Float -> Float
    f n = let k = wrapF 6 (n + h / 60) in 255 * b * (1 - s * max 0 (min k (min (4 - k) 1)))

w8 :: Float -> Word8
w8 v = fromIntegral (max 0 (min 255 (truncate (v + 0.5) :: Int)))

-- ==================================================
-- ボロノイ図の道具（ほかのスケッチにもそのままコピーして使える）
--   voronoiCell sites a x0 y0 x1 y1 … サイト a の領域（多角形）。長方形を垂直二等分線で切っていく（O(n²)）
-- ==================================================
voronoiCell :: [Point] -> Point -> Float -> Float -> Float -> Float -> [Point]
voronoiCell sites a@(ax, ay) x0 y0 x1 y1 = foldl' cut [(x0, y0), (x1, y0), (x1, y1), (x0, y1)] sites
  where
    -- a の側 ＝ (b − a)・p ≦ (|b|² − |a|²) / 2　（a と b の垂直二等分線で切る）
    cut poly b@(bx, by)
      | b == a || length poly <= 2 = poly
      | otherwise = clipHalfPlane poly (bx - ax) (by - ay) ((bx * bx + by * by - ax * ax - ay * ay) / 2)

-- nx·x + ny·y ≦ c の側だけ残す（Sutherland–Hodgman の 1 本分）
clipHalfPlane :: [Point] -> Float -> Float -> Float -> [Point]
clipHalfPlane poly nx ny c = concat (zipWith edge poly (drop 1 poly ++ take 1 poly))
  where
    side (x, y) = nx * x + ny * y - c
    edge p@(px, py) q@(qx, qy) =
      let dp = side p
          dq = side q
          t = dp / (dp - dq)
      in [p | dp <= 0] ++ [(px + (qx - px) * t, py + (qy - py) * t) | (dp < 0 && dq > 0) || (dp > 0 && dq < 0)]

-- 多角形の面積（Float の桁落ちを防ぐため、最初の頂点からの相対座標で計算する）
polyArea :: [Point] -> Float
polyArea [] = 0
polyArea poly@((ox, oy) : _) = abs (sum (zipWith cr poly (drop 1 poly ++ take 1 poly))) / 2
  where cr (px, py) (qx, qy) = (px - ox) * (qy - oy) - (qx - ox) * (py - oy)
