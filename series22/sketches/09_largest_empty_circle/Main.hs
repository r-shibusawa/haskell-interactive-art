-- ============================================
-- 09 最近点探索と最大空円
-- サイト = 町のコンビニ。① マウスの場所からいちばん近い店は？（自分がどのセルにいるか）
--   ② 新しい店はどこに建てる？ → どの店からもいちばん遠い場所 = 最大の「空の円」の中心。候補はボロノイの頂点（外心）
-- マウス = 現在地 / スペース = 最大空円 / N = いちばん良い場所に出店 / クリック = 出店 / R = ランダム / C = 3 店に戻す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (foldl', partition, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import GHC.Float (double2Float, float2Double)
import Text.Printf (printf)

topN :: Int
topN = 3                  -- ★ 表示する候補の数

-- 町の範囲（この中に中心がある円だけ考える）
tx0, ty0, tx1, ty1 :: Float
tx0 = 40
ty0 = 110
tx1 = width - 300
ty1 = height - 30

data World = World
  { wShops :: ![Point]    -- 店（座標は p5 と同じ：左上が原点、y は下向き）
  , wCells :: ![[Point]]
  , wEC    :: ![(Float, Float, Float)]   -- 空の円の候補 (x, y, 半径)。大きい順
  , wShow  :: !Bool
  , wDirty :: !Bool
  , wHist  :: ![Float]    -- 出店したときの最大空円の半径
  }

main :: IO ()
main = runSketch (sketch "22-09 Largest Empty Circle" world0)
  { skBack  = hsb 230 25 12
  , skStart = \_ -> randomShops 12 world0
  , skStep  = \_ _ w -> pure (if wDirty w then rebuild w else w)
  , skDraw  = \env w -> drawAll (toP5 (envMouse env)) w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World [] [] [] True True []

randomShops :: Int -> World -> IO World
randomShops n w = do
  ss <- replicateM n ((,) <$> randomRIO (tx0 + 20, tx1 - 20) <*> randomRIO (ty0 + 20, ty1 - 20))
  pure w { wShops = ss, wHist = [], wDirty = True }

rebuild :: World -> World
rebuild w = w { wCells = map (\s -> voronoiCell ss s tx0 ty0 tx1 ty1) ss, wEC = emptyCircles ss, wDirty = False }
  where ss = wShops w

-- ★ 最大空円の候補：町の中にあるボロノイの頂点（外心）。半径 = 3 店までの距離（それより近い店はない）
--   （本当は町の境界とボロノイの辺の交点・町の角も候補になる。ここでは内側の頂点だけ）
emptyCircles :: [Point] -> [(Float, Float, Float)]
emptyCircles ss = sortOn (\(_, _, r) -> negate r) (foldl' add [] centers)
  where
    sv = U.fromList ss
    centers = [ (double2Float x, double2Float y, double2Float (sqrt r2))
              | (a, b, c) <- delaunay ss
              , let (x, y, r2) = circumcircle (toPD (sv U.! a)) (toPD (sv U.! b)) (toPD (sv U.! c))
              , r2 < 1e12 ]
    add list c@(x, y, _)
      | x < tx0 || x > tx1 || y < ty0 || y > ty1 = list
      | any (\(qx, qy, _) -> abs (qx - x) < 2 && abs (qy - y) < 2) list = list   -- 4 店が同じ円の上 → 同じ中心が 2 回出るので 1 つに
      | otherwise = c : list

drawAll :: Point -> World -> Picture
drawAll (mx0, my0) w = Pictures
  [ view (Color (hsb 230 20 18) (boxAt tx0 ty0 (tx1 - tx0) (ty1 - ty0)) : cellPics ++ ecPics)
  , ecTexts
  , view (shopPics ++ mePics ++ panel)
  , txt (px + 12) 132 14 (gray 1) "Largest empty radius"
  , txt (px + 12) 150 14 (gray 1) "at each new shop"
  , txt (px + 12) 395 14 (hsb 0 0 70) "N builds at the farthest"
  , txt (px + 12) 415 14 (hsb 0 0 70) "place. How does r shrink?"
  , txt 20 30 16 (gray 1) ("Mouse: you  Space: empty circles " ++ onOff (wShow w) ++ "  N: best new shop  Click: build  R: random  C: back to 3  Shops " ++ show (length ss))
  , txt 20 52 14 (hsb 0 0 65) ("Nearest shop = the site of the cell you are in (distance " ++ printf "%.0f" (if best < 0 then 0 else bd) ++ " px). Yellow dots = Voronoi vertices.")
  , txt 20 74 14 (hsb 0 0 65) "Their circumcircles hold no shop (empty circle). The center of the largest is the best new site."
  ]
  where
    ss = wShops w
    px = width - 280
    -- 最寄りの店（全部と比べる：n 回の比較）
    me = (clampF tx0 tx1 mx0, clampF ty0 ty1 my0)
    (best, bd) = foldl' pick (-1 :: Int, 1 / 0 :: Float) (zip [0 ..] ss)
    pick (bi, bv) (i, s) = let d = dist2 me s in if d < bv then (i, d) else (bi, bv)
    cellPics =
      [ Pictures [Color (if i == best then hsb 190 60 55 else hsb 230 15 22) (Polygon c), Color (hsb 0 0 60) (poly c)]
      | (i, c) <- zip [0 ..] (wCells w), length c > 2 ]
    -- 最大空円
    ec = wEC w
    top = reverse (zip [0 :: Int ..] (take topN ec))
    ecPics
      | wShow w =
          [dotAt (hsbA 50 90 100 60) (x, y) 5 | (x, y, _) <- ec]
          ++ concat [ [ Color (if k == 0 then hsb 50 90 100 else hsbA 50 50 80 60) (at (x, y) (ring r (if k == 0 then 3 else 1.5)))
                      , dotAt (if k == 0 then hsb 50 90 100 else hsb 50 40 80) (x, y) 10 ]
                    | (k, (x, y, r)) <- top ]
      | otherwise = []
    ecTexts
      | wShow w = Pictures [ txt (x + 8) (y - 8) 14 (if k == 0 then hsb 50 90 100 else hsb 50 40 80) ("#" ++ show (k + 1) ++ " r=" ++ printf "%.0f" r)
                           | (k, (x, y, r)) <- top ]
      | otherwise = Blank
    -- 店と現在地
    shopPics = concat
      [ [Color (hsb 0 0 10) (rect x y 16 16), Color (if i == best then hsb 190 80 100 else gray 1) (rect x y 12 12)]
      | (i, (x, y)) <- zip [0 ..] ss ]
    mePics = case nth ss best of
      Just s -> [Color (hsb 190 80 100) (thickLine 2 me s), dotAt (hsb 0 80 100) me 12]
      Nothing -> []
    -- 右のパネル：出店するたびの最大空円の半径
    hist = wHist w
    panel = Color (hsbA 0 0 0 40) (boxAt px 110 260 260) : case hist of
      [] -> []
      _ ->
        let mx = max 1e-3 (maximum hist)
            bw = min 20 (236 / fromIntegral (length hist))
        in [ Color (hsb 50 80 90) (boxAt (px + 12 + k * bw) (360 - bh) (max 1 (bw - 2)) bh)
           | (k, h) <- zip [0 ..] hist, let bh = h / mx * 190 ]

addShop :: Point -> World -> World
addShop p w = w { wHist = wHist w ++ [case wEC w of { ((_, _, r) : _) -> r; [] -> 0 }], wShops = wShops w ++ [p], wDirty = True }

onEvent :: Event -> World -> IO World
onEvent ev w0' = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let m@(x, y) = toP5 pos
    in pure (if x > tx0 && x < tx1 && y > ty0 && y < ty1 then addShop m w else w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wShow = not (wShow w) }
  EventKey (Char c) Down _ _ -> case toLower c of
    'n' -> pure (case wEC w of
                   ((x, y, _) : _) -> addShop (x, y) w
                   [] -> w)
    'r' -> do n <- randomRIO (6, 19)
              randomShops n w
    'c' -> randomShops 3 w
    _ -> pure w
  _ -> pure w
  where w = if wDirty w0' then rebuild w0' else w0'

-- ---------- やってみよう ----------
-- 1. N を 10 回押して、店がどんな並びになるか見よう。半径のグラフはどう減る？
-- 2. 町の角は候補に入れていない。角に住む人がいちばん遠いこともある。角（4 点）を候補に足してみよう
-- 3. 逆に「ゴミ処理場はどこに？」= 家（サイト）からいちばん遠い場所。同じ計算で答えが出る
-- 4. 最寄り探しは今「全部と比べる」。点が 100 万個だったら？ ボロノイのセルをたどる方法・格子に分ける方法を考えよう

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

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- 安全な添字アクセス
nth :: [a] -> Int -> Maybe a
nth xs i | i < 0 = Nothing
         | otherwise = case drop i xs of
             (x : _) -> Just x
             [] -> Nothing

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

-- 外接円 (x, y, r2) は桁が大きくなるので Double で計算する（r2 = 半径の 2 乗。3 点が一直線なら無限大）
type PD = (Double, Double)

circumcircle :: PD -> PD -> PD -> (Double, Double, Double)
circumcircle (ax, ay) (bx, by) (cx, cy)
  | abs d < 1e-9 = ((ax + bx + cx) / 3, (ay + by + cy) / 3, 1 / 0)
  | otherwise = (x, y, (ax - x) * (ax - x) + (ay - y) * (ay - y))
  where
    d = 2 * (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by))
    a2 = ax * ax + ay * ay
    b2 = bx * bx + by * by
    c2 = cx * cx + cy * cy
    x = (a2 * (by - cy) + b2 * (cy - ay) + c2 * (ay - by)) / d
    y = (a2 * (cx - bx) + b2 * (ax - cx) + c2 * (bx - ax)) / d

-- ==================================================
-- ドロネーの道具（外接円の計算は桁が大きいので Double で行う）
--   delaunay pts … ドロネー三角形分割（Bowyer–Watson 法）。頂点番号の組を返す
-- ==================================================

toPD :: Point -> PD
toPD (x, y) = (float2Double x, float2Double y)

data Tri = Tri !Int !Int !Int !Double !Double !Double

-- Bowyer–Watson：点を 1 つずつ入れ、「外接円の中に新しい点がある三角形」を消して穴を新しい点とつなぐ
delaunay :: [Point] -> [(Int, Int, Int)]
delaunay pts
  | n < 3 = []
  | otherwise = [(a, b, c) | Tri a b c _ _ _ <- foldl' insert [mk n (n + 1) (n + 2)] [0 .. n - 1], a < n, b < n, c < n]
  where
    n = length pts
    ds = map toPD pts
    x0 = minimum (map fst ds)
    x1 = maximum (map fst ds)
    y0 = minimum (map snd ds)
    y1 = maximum (map snd ds)
    m = maximum [x1 - x0, y1 - y0, 1] * 20
    mx = (x0 + x1) / 2
    my = (y0 + y1) / 2
    -- 全部を囲む大きな三角形を後ろに足す
    pv = U.fromList (ds ++ [(mx - m, my - m), (mx + m, my - m), (mx, my + m)]) :: U.Vector PD
    mk i j k = let (x, y, r2) = circumcircle (pv U.! i) (pv U.! j) (pv U.! k) in Tri i j k x y r2
    insert tris i =
      let (px, py) = pv U.! i
          inside (Tri _ _ _ x y r2) = (px - x) * (px - x) + (py - y) * (py - y) < r2
          (bad, keep) = partition inside tris
          -- 2 回出た辺 = 穴の内側なので消す。1 回だけの辺が穴のふち
          toggle e (a, b) = let k = (min a b, max a b)
                            in if M.member k e then M.delete k e else M.insert k (a, b) e
          rim = foldl' toggle M.empty (concat [[(a, b), (b, c), (c, a)] | Tri a b c _ _ _ <- bad])
      in [mk a b i | (a, b) <- M.elems rim] ++ keep
