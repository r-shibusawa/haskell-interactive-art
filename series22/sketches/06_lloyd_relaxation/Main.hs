-- ============================================
-- 06 ロイドの緩和（重心ボロノイ）
-- 「サイトを自分のセルの重心へ動かす → ボロノイ図を作り直す」をくり返すと、セルの大きさがそろっていく
--   最後は サイト = セルの重心 になる（重心ボロノイ図）。内側のセルはほとんど六角形 = ハチの巣
-- スペース = 緩和 ON/OFF / N = 1 回だけ / ←→ = サイトの数 / M = 重心の矢印 / R = ばらばらに戻す / クリック = 追加
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.IntMap.Strict as IM
import Text.Printf (printf)

rate :: Float
rate = 1.0                -- ★ 重心へどれだけ動くか（1 = 一気に、0.3 = ゆっくり）

-- 計算する範囲
bx0, by0, bx1, by1 :: Float
bx0 = 20
by0 = 100
bx1 = width - 340
by1 = height - 20

data World = World
  { wSites  :: ![Point]    -- サイト（座標は p5 と同じ：左上が原点、y は下向き）
  , wN      :: !Int        -- ★ サイトの数
  , wRun    :: !Bool
  , wCells  :: ![[Point]]
  , wEnergy :: ![Float]    -- エネルギー（点から最寄りのサイトまでの距離² の平均）の履歴
  , wIter   :: !Int
  , wArrow  :: !Bool
  }

main :: IO ()
main = runSketch (sketch "22-06 Lloyd Relaxation" world0)
  { skBack  = hsb 230 25 12
  , skStart = \_ -> scatter world0
  , skStep  = \_ _ w -> pure (if wRun w then lloydStep w else w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World [] 80 False [] [] 0 True

scatter :: World -> IO World
scatter w = do
  -- わざと偏らせて置く（左下に寄せる）と、広がっていく様子がよく見える
  ss <- replicateM (wN w) $ do
    u <- randomRIO (0, 1)
    v <- randomRIO (0, 1)
    pure (lerpF (bx0 + 5) (bx1 - 5) (u ** 1.6), lerpF (by0 + 5) (by1 - 5) (v ** 0.7))
  pure (rebuild w { wSites = ss, wEnergy = [], wIter = 0 })

rebuild :: World -> World
rebuild w = w { wCells = map (\s -> voronoiCell (wSites w) s bx0 by0 bx1 by1) (wSites w) }

-- ★ セル内の点からサイトまでの距離² の積分（多角形の公式）。これが小さいほど「サイトがセルの真ん中」
cellEnergy :: [Point] -> Point -> Float
cellEnergy ps (sx, sy) = abs (sum (zipWith term ps (drop 1 ps ++ take 1 ps))) / 12
  where
    term (px, py) (qx, qy) =
      let ax = px - sx
          ay = py - sy
          bx = qx - sx
          by = qy - sy
          cr = ax * by - bx * ay
      in cr * (ax * ax + ax * bx + bx * bx + ay * ay + ay * by + by * by)

-- ロイドの 1 ステップ
lloydStep :: World -> World
lloydStep w = rebuild w
  { wSites = zipWith move (wSites w) (wCells w)
  , wEnergy = drop (length hist - 200) hist
  , wIter = wIter w + 1 }
  where
    good = [(c, s) | (c, s) <- zip (wCells w) (wSites w), length c >= 3]
    total = sum [cellEnergy c s | (c, s) <- good]
    area = sum [polyArea c | (c, _) <- good]
    hist = wEnergy w ++ [if area > 0 then total / area else 0]
    move s@(x, y) c
      | length c < 3 = s
      | otherwise = let (cx, cy) = polyCentroid c in (lerpF x cx rate, lerpF y cy rate)

-- セルの辺の数（画面のふちに触れるセルは数えない）
sides :: [Point] -> Int
sides ps
  | any (\(x, y) -> x < bx0 + 0.5 || x > bx1 - 0.5 || y < by0 + 0.5 || y > by1 - 0.5) ps = 0
  | otherwise = length (filter (> 0.5) (zipWith dist2 ps (drop 1 ps ++ take 1 ps)))

-- 辺の数の色（5 = 青, 6 = 緑, 7 = 赤）
sideHue :: Int -> Float
sideHue n
  | n == 6 = 130
  | n == 5 = 210
  | n == 7 = 0
  | n == 4 = 270
  | n >= 8 = 30
  | otherwise = 0

drawAll :: World -> Picture
drawAll w = Pictures
  [ view (map cellPic (wCells w) ++ concat (zipWith sitePic (wSites w) (wCells w)) ++ panels ++ graph ++ bars)
  , txt (px + 12) 122 14 (gray 1) "Energy (smaller = neater)"
  , case reverse (wEnergy w) of
      (e : _ : _) -> txt (px + pw - 80) 122 14 (gray 0.8) (printf "%.1f" e)
      _ -> Blank
  , txt (px + 12) 367 14 (gray 1) "Sides of inner cells"
  , Pictures [txt (px + 18 + fromIntegral (n - 3) * 40 + 9) 570 14 (gray 0.8) (show n) | n <- [3 .. 9]]
  , txt (px + 12) 395 15 (hsb 130 60 90) ("Hexagons " ++ (if inner > 0 then printf "%.0f" (100 * fromIntegral (count 6) / fromIntegral inner :: Float) else "0") ++ "%")
  , txt 20 30 16 (gray 1) ("Space: relax " ++ onOff (wRun w) ++ "  N: once  Left/Right: sites " ++ show (wN w) ++ "  M: arrows  R: scatter  Click: add  Steps " ++ show (wIter w))
  , txt 20 52 14 (hsb 0 0 65) "Yellow arrows go from each site to its cell centroid. Everyone moves there, then rebuild."
  , txt 20 74 14 (hsb 0 0 65) "Repeat: the energy falls and green cells (hexagons) increase."
  ]
  where
    px = width - 320
    pw = 300
    counts = [sides c | c <- wCells w]
    hist = IM.fromListWith (+) [(n, 1 :: Int) | n <- counts, n > 0]
    count n = IM.findWithDefault 0 n hist
    inner = length (filter (> 0) counts)
    -- セル：辺の数で色分け
    cellPic ps =
      let n = sides ps
          c = if n > 0 then hsb (sideHue n) 50 60 else hsb 0 0 30
      in Pictures [Color c (Polygon ps), Color (hsb 0 0 90) (outline 1.2 ps)]
    -- サイトと重心への矢印
    sitePic s c =
      (if wArrow w && length c > 2
         then let g = polyCentroid c in [Color (hsb 50 90 100) (thickLine 1.5 s g), dotAt (hsb 50 90 100) g 4]
         else [])
      ++ [dotAt (gray 1) s 6]
    -- 右のパネル：エネルギーのグラフと辺の数のヒストグラム
    panels = [Color (hsbA 0 0 0 40) (boxAt px 100 pw 230), Color (hsbA 0 0 0 40) (boxAt px 345 pw 230)]
    graph = case wEnergy w of
      es@(_ : _ : _) ->
        let mx = maximum es
            mn = minimum es
        in [Color (hsb 50 90 100) (thickPath 2
              [ (px + 12 + k / 199 * (pw - 24), 315 - (e - mn) / (mx - mn + 1e-9) * 170) | (k, e) <- zip [0 ..] es ])]
      _ -> []
    bars =
      [ Color (hsb h 50 70) (boxAt (px + 18 + fromIntegral (n - 3) * 40) (555 - bh) 28 bh)
      | n <- [3 .. 9]
      , let bh = if inner > 0 then fromIntegral (count n) / fromIntegral inner * 160 else 0
            h = if n >= 8 then 30 else sideHue n ]

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let m@(x, y) = toP5 pos
    in if x > bx0 && x < bx1 && y > by0 && y < by1
         then pure (rebuild w { wSites = wSites w ++ [m], wN = length (wSites w) + 1 })
         else pure w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wRun = not (wRun w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> scatter w { wN = min 250 (wN w + 20) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> scatter w { wN = max 10 (wN w - 20) }
  EventKey (Char c) Down _ _ -> case toLower c of
    'n' -> pure (lloydStep w)
    'm' -> pure w { wArrow = not (wArrow w) }
    'r' -> scatter w
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 何回くらいでエネルギーのグラフが平らになる？ サイトを 20 個と 200 個で比べよう
-- 2. rate を 0.3 にするとゆっくり、1.8 にすると行き過ぎる。グラフはどうなる？
-- 3. 最後まで六角形にならないセルはどこにある？（ふちの近く・5 角形と 7 角形のペア = 「欠陥」）
-- 4. 計算範囲 bx0 〜 by1 を円形に切り取る（clipHalfPlane を何回か使う）と、どんな模様に落ち着く？

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

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y bw bh = rect (x + bw / 2) (y + bh / 2) bw bh

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 直径 d の円（p5 の circle と同じく直径で書く）
dotAt :: Color -> Point -> Float -> Picture
dotAt c p d = Color c (at p (disc (d / 2)))

-- 閉じた多角形の太い枠
outline :: Float -> [Point] -> Picture
outline t ps = thickPath t (ps ++ take 1 ps)

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- ==================================================
-- ボロノイ図の道具
--   voronoiCell sites a x0 y0 x1 y1 … サイト a の領域（多角形）。長方形を垂直二等分線で切っていく
--   サイトが多いときのために、セルのどの頂点よりも遠い二等分線（セルに届かない）は切らずに飛ばす
-- ==================================================
voronoiCell :: [Point] -> Point -> Float -> Float -> Float -> Float -> [Point]
voronoiCell sites a@(ax, ay) x0 y0 x1 y1 = fst (foldl' cut (start, reach start) sites)
  where
    start = [(x0, y0), (x1, y0), (x1, y1), (x0, y1)]
    -- a からいちばん遠い頂点までの距離² × 4（これより遠いサイトの二等分線はセルに届かない）
    reach ps = 4 * foldl' (\m (x, y) -> max m ((x - ax) * (x - ax) + (y - ay) * (y - ay))) 0 ps
    cut acc@(poly, r) b@(bx, by)
      | b == a || null (drop 2 poly) = acc
      | (bx - ax) * (bx - ax) + (by - ay) * (by - ay) > r = acc
      | otherwise =
          -- a の側 ＝ (b − a)・p ≦ (|b|² − |a|²) / 2　（a と b の垂直二等分線で切る）
          let poly' = clipHalfPlane poly (bx - ax) (by - ay) ((bx * bx + by * by - ax * ax - ay * ay) / 2)
          in (poly', reach poly')

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

-- 多角形の重心（Float の桁落ちを防ぐため、最初の頂点からの相対座標で計算する）
polyCentroid :: [Point] -> Point
polyCentroid [] = (0, 0)
polyCentroid poly@((ox, oy) : _)
  | abs a < 1e-6 = (sum (map fst poly) / n, sum (map snd poly) / n)
  | otherwise = (ox + cx / (3 * a), oy + cy / (3 * a))
  where
    n = fromIntegral (length poly)
    (a, cx, cy) = foldl' add (0, 0, 0) (zip poly (drop 1 poly ++ take 1 poly))
    add (!s, !sx, !sy) ((px0, py0), (qx0, qy0)) =
      let px = px0 - ox
          py = py0 - oy
          qx = qx0 - ox
          qy = qy0 - oy
          cr = px * qy - qx * py
      in (s + cr, sx + (px + qx) * cr, sy + (py + qy) * cr)
