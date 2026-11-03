-- ============================================
-- 00 正多角形で平面を埋める
-- 1 種類の正多角形だけで、すき間なく・重ならずに平面を埋められるのはどれ？
--   1 つの頂点に集まる角の合計がちょうど 360° のときだけ埋まる → 正三角形・正方形・正六角形の 3 つだけ
-- 3〜8 = 正 n 角形 / スペース = 頂点のまわりに置き直す / ←→ = 回転 / ↑↓ = 大きさ / S = 保存
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Set as S
import Numeric (showFFloat)

data World = World
  { wN      :: !Int        -- ★ 正 n 角形（3〜8 キーで変える）
  , wSide   :: !Float      -- ★ 1 辺の長さ（ピクセル）
  , wPlaced :: !Int        -- 左：頂点のまわりに置いた枚数
  , wTimer  :: !Int
  , wSpin   :: !Float      -- 全体の回転（ラジアン）
  , wSpread :: [[Point]]   -- 右：となりに次々と並べた多角形
  }

world0 :: World
world0 = World 5 60 0 0 0 []

main :: IO ()
main = runSketch (sketch "32-00 Regular Polygons" world0)
  { skBack  = backCol
  , skStart = \env -> pure (makeSpread env world0)
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

backCol :: Color
backCol = rgb255 18 20 28

-- ★ 色（n ごと）
palette :: Int -> (Int, Int, Int)
palette n = case n of
  3 -> (255, 140, 105)
  4 -> (120, 200, 255)
  5 -> (255, 210, 90)
  6 -> (140, 230, 160)
  7 -> (200, 150, 255)
  _ -> (255, 130, 190)

-- 頂点のまわりに何枚入るか
fitOf :: Int -> Int
fitOf n = floor (tau / interiorAngle n + 1e-4)

-- 1 つの角（ラジアン）
interiorAngle :: Int -> Float
interiorAngle k = pi - tau / fromIntegral k

stepWorld :: World -> World
stepWorld w =
  let t = wTimer w + 1
      more = t `mod` 40 == 0 && wPlaced w < fitOf (wN w) + 1
  in w { wTimer = t, wPlaced = if more then wPlaced w + 1 else wPlaced w }

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      n = wN w
      a = interiorAngle n
      fit = fitOf n
      deg = 180 * fromIntegral (n - 2) / fromIntegral n :: Float
      total = deg * fromIntegral fit
      msg | abs (total - 360) < 0.01 = "exactly 360 deg! The plane is filled"
          | otherwise = nf1 (360 - total) ++ " deg gap (one more would overlap)"
  in Pictures
    [ -- 右：となりへ、となりへと並べていく（重なると濃くなる）。枠の外は上から塗って隠す
      onP5 env (Pictures (map (\p -> drawPoly p (palette n) 70) (wSpread w)))
    , onP5 env (Color backCol (Pictures [box 0 0 (wd * 0.5) h, box 0 0 wd 90, box 0 (h - 90) wd 90]))
    , onP5 env (Color (rgb255 60 64 80) (rectLine (wd * 0.75) (h / 2) (wd * 0.5 - 1) (h - 180)))
    , -- 左：1 つの頂点のまわりに 1 枚ずつ置く
      drawVertexDemo env w (wd * 0.25) (h * 0.56) a fit
    , txt env 20 36 20 (gray 0.94) ("Regular " ++ show n ++ "-gon (keys 3-8)   one angle = 180 x (n-2) / n = " ++ nf1 deg ++ " deg")
    , txt env 20 62 15 (gray 0.7) ("Left: " ++ show fit ++ " around a vertex -> total " ++ nf1 total ++ " deg   " ++ msg)
    , txt env (wd * 0.5 + 10) (h - 60) 15 (gray 0.7) "Right: reflect across edges. Overlaps look darker"
    , drawTable env n 20 (h - 150)
    ]

-- 頂点 (vx, vy) のまわりに placed 枚並べる
drawVertexDemo :: Env -> World -> Float -> Float -> Float -> Int -> Picture
drawVertexDemo env w vx vy a fit =
  let n = wN w
      spin = wSpin w
      polys = [ drawPoly (polyFromVertex vx vy (spin + fromIntegral k * a) (wSide w * 1.1) n)
                         (if k >= fit then (255, 70, 70) else palette n)
                         (if k >= fit then 120 else 200)
              | k <- [0 .. wPlaced w - 1] ]
      rest = tau - fromIntegral fit * a
      showGap = wPlaced w >= fit && rest > 0.01
      a0 = spin + fromIntegral fit * a
      mid = a0 + rest / 2
      gapPie = Color (rgba255 255 230 0 90) (Polygon (pie vx vy 60 a0 (spin + tau)))
  in Pictures
    [ onP5 env (Pictures polys)
    , if showGap then onP5 env gapPie else Blank
    , if showGap then txt env (vx + cos mid * 80 - 18) (vy + sin mid * 80 + 6) 16 (rgb255 255 230 0) (nf1 (rest * 180 / pi) ++ " deg") else Blank
    , onP5 env (Color white (at (vx, vy) (disc 4)))
    ]

-- 頂点 (vx, vy) から角度 th の向きに 1 辺を伸ばし、左回りに正 k 角形を作る
polyFromVertex :: Float -> Float -> Float -> Float -> Int -> [Point]
polyFromVertex vx vy th s k = go k vx vy th
  where
    go 0 _ _ _ = []
    go i x y d = (x, y) : go (i - 1 :: Int) (x + cos d * s) (y + sin d * s) (d + tau / fromIntegral k)

-- 真ん中の多角形から、辺で「折り返して」となりを次々に作る（幅優先）
makeSpread :: Env -> World -> World
makeSpread env w =
  let n = wN w
      depth = case n of { 3 -> 12; 4 -> 8; 6 -> 6; 5 -> 2; _ -> 1 } :: Int   -- ★ 埋まらない形は 1〜2 周まで
      cx = envW env * 0.75
      cy = envH env * 0.5
      r = wSide w / (2 * sin (pi / fromIntegral n))
      ang i = wSpin w + tau * fromIntegral i / fromIntegral n
      first = [ (cx + r * cos (ang i), cy + r * sin (ang i)) | i <- [0 .. n - 1 :: Int] ]
      grow :: Int -> [[Point]] -> [[Point]] -> S.Set (Int, Int) -> [[Point]]
      grow d front acc seen
        | d >= depth || length acc > 900 = acc
        | otherwise =
            let cands = [ map (\v -> reflectPt v a b) p | p <- front, (a, b) <- edgesOf p ]
                (seen', next) = foldl' visit (seen, []) cands
            in grow (d + 1) (reverse next) (acc ++ reverse next) seen'
      visit (seen, out) q =
        let k = keyOf q
        in if S.member k seen then (seen, out) else (S.insert k seen, q : out)
  in w { wSpread = grow 0 [first] [first] (S.singleton (keyOf first)) }

keyOf :: [Point] -> (Int, Int)
keyOf p =
  let m = fromIntegral (length p)
      (x, y) = foldl' (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) p
  in (round (x / m), round (y / m))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char c) Down _ _
    | c >= '3' && c <= '8' -> pure (makeSpread env w { wN = fromEnum c - 48, wPlaced = 0, wTimer = 0 })
    | c == 's' || c == 'S' -> saveSvg "regular_tiling" env (drawAll env w) >> pure w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaced = 0, wTimer = 0 }
  EventKey (SpecialKey KeyLeft) Down _ _  -> pure (makeSpread env w { wSpin = wSpin w - 0.1 })
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (makeSpread env w { wSpin = wSpin w + 0.1 })
  EventKey (SpecialKey KeyUp) Down _ _    -> pure (makeSpread env w { wSide = min 120 (wSide w + 8) })
  EventKey (SpecialKey KeyDown) Down _ _  -> pure (makeSpread env w { wSide = max 24 (wSide w - 8) })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 3・4・6 だけが「ぴったり 360°」。5・7・8 で残る角度を記録して表にしよう
-- 2. 8（正八角形）のすき間は何の形？ → 正方形を足すと 01 の「4.8.8」になる
-- 3. 角の合計の式 180 × (n − 2) / n × 枚数 = 360 を解くと、なぜ 3 つしかないか分かる
-- 4. makeSpread の depth を変えて、広げる周の数を増やしてみよう


-- ==================================================
-- ここから下は「道具」（座標の変換・多角形を描く・折り返す・表を描く）
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を画面に合わせる
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- p5.js の text(s, x, y) と同じ位置に文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

-- 左上が (x, y) の長方形（p5.js の rect）
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

nf1, nf2 :: Float -> String
nf1 x = showFFloat (Just 1) x ""
nf2 x = showFFloat (Just 2) x ""

edgesOf :: [a] -> [(a, a)]
edgesOf p = zip p (drop 1 p ++ take 1 p)

drawPoly :: [Point] -> (Int, Int, Int) -> Int -> Picture
drawPoly pts (r, g, b) a = Pictures
  ( Color (rgba255 r g b a) (Polygon pts)
  : [ Color (rgb255 20 22 30) (thickLine 2 p q) | (p, q) <- edgesOf pts ] )

-- 中心 (x, y)、半径 r の扇形（角度 a0 から a1 まで）
pie :: Float -> Float -> Float -> Float -> Float -> [Point]
pie x y r a0 a1 =
  (x, y) : [ (x + r * cos t, y + r * sin t) | i <- [0 .. 16 :: Int], let t = a0 + (a1 - a0) * fromIntegral i / 16 ]

-- 点 v を直線 p-q で折り返す
reflectPt :: Point -> Point -> Point -> Point
reflectPt (vx, vy) (px, py) (qx, qy) =
  let dx = qx - px
      dy = qy - py
      t = ((vx - px) * dx + (vy - py) * dy) / max 1e-6 (dx * dx + dy * dy)
      fx = px + t * dx
      fy = py + t * dy
  in (2 * fx - vx, 2 * fy - vy)

drawTable :: Env -> Int -> Float -> Float -> Picture
drawTable env n x y = Pictures
  [ txt env x (y + fromIntegral (k - 3) * 20) 14 (if k == n then white else gray 0.6)
      (show k ++ "-gon  " ++ nf1 deg ++ " deg   360 / angle = " ++ nf2 q ++ (if good then "  OK" else "  x"))
  | k <- [3 .. 8 :: Int]
  , let deg = 180 * fromIntegral (k - 2) / fromIntegral k :: Float
        q = 360 / deg
        good = abs (q - fromIntegral (round q :: Int)) < 1e-4
  ]
