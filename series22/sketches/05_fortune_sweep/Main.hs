-- ============================================
-- 05 フォーチュンのアルゴリズム（掃引線）
-- 水平な線（掃引線）を上から下へ動かしながらボロノイ図を作る、速い方法 O(n log n) のしくみを見る
--   掃引線より上のサイトごとに「サイトと掃引線から等距離」の放物線ができる。その下側の包み = 汀線（ビーチライン）
--   ビーチラインより上は、もう答えが決まった場所。放物線どうしの交点がボロノイの辺をなぞっていく
-- スペース = 一時停止 / ↑↓ = 速さ / ←→ = 手で動かす / P = 放物線 / クリック = サイトを追加 / R = ランダム
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (foldl', partition)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import GHC.Float (double2Float, float2Double)
import Text.Printf (printf)

res :: Float
res = 5

gw, gh :: Int
gw = ceiling (width / res)
gh = ceiling (height / res)

-- イベントの光
data Flash = Flash { fX, fY, fT :: !Float, fSite :: !Bool, fR :: !Float }

data World = World
  { wSites :: ![Point]     -- サイト（座標は p5 と同じ：左上が原点、y は下向き）
  , wLy    :: !Float       -- 掃引線の y
  , wSpeed :: !Float       -- ★ 掃引線の速さ（px / フレーム）
  , wPause :: !Bool
  , wPar   :: !Bool
  , wCells :: ![[Point]]
  , wCirc  :: ![(Float, Float, Float)]   -- 外接円 (x, y, 半径)
  , wLab   :: !(U.Vector Int)            -- 各画素のいちばん近いサイト（サイトが変わったときだけ計算）
  , wBeach :: !(U.Vector Float)          -- 各 x のビーチラインの y
  , wFlash :: ![Flash]
  , wDirty :: !Bool
  }

main :: IO ()
main = runSketch (sketch "22-05 Fortune Sweep" world0)
  { skBack  = hsb 230 25 12
  , skStart = \_ -> randomSites 18 world0
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World [] 90 1.5 False True [] [] U.empty U.empty [] True

randomSites :: Int -> World -> IO World
randomSites n w = do
  ss <- replicateM n ((,) <$> randomRIO (60, width - 60) <*> randomRIO (130, height - 60))
  pure w { wSites = ss, wLy = 90, wDirty = True }

rebuild :: World -> World
rebuild w = w { wCells = cells, wCirc = circ, wLab = lab, wDirty = False }
  where
    ss = wSites w
    sv = U.fromList ss
    n = U.length sv
    cells = map (\s -> voronoiCell ss s 0 90 width height) ss
    circ = [ (double2Float x, double2Float y, double2Float (sqrt r2))
           | (a, b, c) <- delaunay ss
           , let (x, y, r2) = circumcircle (toPD (sv U.! a)) (toPD (sv U.! b)) (toPD (sv U.! c))
           , r2 < 1e12 ]
    lab = U.generate (gw * gh) nearest
    nearest i =
      let (gy, gx) = i `divMod` gw
          x = (fromIntegral gx + 0.5) * res
          y = (fromIntegral gy + 0.5) * res
          go !k !best !bd
            | k >= n = best
            | otherwise =
                let (sx, sy) = U.unsafeIndex sv k
                    d = (x - sx) * (x - sx) + (y - sy) * (y - sy)
                in if d < bd then go (k + 1) k d else go (k + 1) best bd
      in go 0 0 (1 / 0 :: Float) :: Int

-- ★ サイト s と掃引線 y = L から等距離の点の y（放物線）
parabolaY :: Point -> Float -> Float -> Float
parabolaY (sx, sy) x l = (l * l - sy * sy - (x - sx) * (x - sx)) / (2 * (l - sy))

computeBeach :: [Point] -> Float -> U.Vector Float
computeBeach ss ly = U.generate (round width + 1) column
  where
    active = filter (\(_, sy) -> ly - sy > 0.5) ss
    column i = let x = fromIntegral i in foldl' (\y s -> max y (parabolaY s x ly)) (-1 / 0) active

beachAt :: U.Vector Float -> Float -> Float
beachAt b x = U.unsafeIndex b (max 0 (min (U.length b - 1) (round x)))

step :: World -> World
step w0' =
  let w = if wDirty w0' then rebuild w0' else w0'
      prev = wLy w
      moved = if wPause w then prev else prev + wSpeed w
      ly = if moved > height * 1.8 then 90 else moved         -- 下まで行ったら最初から
      -- イベント：サイトイベント（掃引線がサイトを通過）と円イベント（外接円のいちばん下を通過 = ボロノイの頂点が決まる）
      siteEv = [Flash x y 30 True 0 | (x, y) <- wSites w, prev < y, ly >= y]
      circEv = [Flash x y 40 False r | (x, y, r) <- wCirc w, prev < y + r, ly >= y + r]
      -- 光は毎フレーム少しずつ消える（一時停止中も）
      old = [f { fT = fT f - 1 } | f <- wFlash w, fT f > 1]
  in w { wLy = ly, wFlash = old ++ siteEv ++ circEv, wBeach = computeBeach (wSites w) ly }

-- ビーチラインより上だけ、いちばん近いサイトの色で塗る（ほかは背景の色）
paintBuffer :: World -> Picture
paintBuffer w = pixelImage gw gh res pixel
  where
    n = length (wSites w)
    cols = U.generate n (\i -> let (r, g, b) = hsbToRgb (wrapF 360 (fromIntegral i * 137.5)) 40 70 in (w8 r, w8 g, w8 b)) :: U.Vector (Word8, Word8, Word8)
    back = let (r, g, b) = hsbToRgb 230 25 12 in (w8 r, w8 g, w8 b)
    lab = wLab w
    beach = wBeach w
    pixel gx gy =
      let x = (fromIntegral gx + 0.5) * res
          y = (fromIntegral gy + 0.5) * res
      in if y >= beachAt beach x || y < 90 || n == 0 then back
         else U.unsafeIndex cols (U.unsafeIndex lab (gx + gy * gw))

-- 完成図のボロノイの辺のうち、ビーチラインより上の部分だけかく
fixedEdges :: World -> [Picture]
fixedEdges w = concatMap cellEdges (wCells w)
  where
    beach = wBeach w
    cellEdges ps = concat (zipWith edgeRuns ps (drop 1 ps ++ take 1 ps))
    edgeRuns p@(px, py) q@(qx, qy)
      | (py <= 90.5 && qy <= 90.5) || (px <= 0.5 && qx <= 0.5) || (px >= width - 0.5 && qx >= width - 0.5)
        || (py >= height - 0.5 && qy >= height - 0.5) = []     -- 画面のふち
      | otherwise = go 0 (-1)
      where
        n = max 1 (ceiling (dist2 p q / 5)) :: Int
        pos t = (px + (qx - px) * t, py + (qy - py) * t)
        tOf j = fromIntegral j / fromIntegral n
        vis j = let (x, y) = pos (tOf j) in y < beachAt beach x
        go :: Int -> Float -> [Picture]
        go j run
          | j > n = []
          | otherwise =
              let v = vis j
                  run' = if v && run < 0 then tOf j else run
              in if (not v || j == n) && run' >= 0
                   then thickLine 2.5 (pos run') (pos (if v then tOf j else tOf (j - 1))) : go (j + 1) (-1)
                   else go (j + 1) run'

-- 放物線 1 本。画面の上に出る所で切る
parabola :: Float -> Point -> [Picture]
parabola ly s = map Line (runs [ (x, y) | x <- [0, 8 .. width], let y = parabolaY s x ly ])
  where
    runs ps = case dropWhile (\(_, y) -> y <= 90) ps of
      [] -> []
      rest -> let (a, b) = span (\(_, y) -> y > 90) rest in a : runs b

drawAll :: World -> Picture
drawAll w
  | U.null (wBeach w) || U.null (wLab w) = Blank
  | otherwise = Pictures
  [ paintBuffer w
  , view (fixedEdges w ++ vertices ++ parabolas ++ [beachLine] ++ flashes ++ sweepAndSites
          ++ [Color (hsbA 0 0 0 55) (boxAt 0 0 width 84)])
  , txt 20 30 16 (gray 1) ("Space: " ++ (if wPause w then "resume" else "pause") ++ "  Up/Down: speed " ++ printf "%.1f" (wSpeed w)
      ++ "  Left/Right: move by hand  P: parabolas " ++ onOff (wPar w) ++ "  Click: add  R: random")
  , txt 20 52 14 (gray 0.8) "Red = sweep line, cyan = beach line (lower envelope of parabolas). Above it the answer is settled."
  , txt 20 74 14 (gray 0.8) "Green flash = site event (a new parabola). Yellow circle = circle event (a vertex is fixed)."
  ]
  where
    ly = wLy w
    -- 決まったボロノイの頂点
    vertices = [dotAt (hsb 50 90 100) (x, y) 8 | (x, y, r) <- wCirc w, ly >= y + r]
    -- 放物線（1 つずつ）とビーチライン
    parabolas
      | wPar w = [Color (hsbA 190 60 100 30) (Pictures (concatMap (parabola ly) (filter (\(_, sy) -> ly - sy > 0.5) (wSites w))))]
      | otherwise = []
    beachLine = Color (hsb 190 90 100)
      (thickPath 3 [ (x, max 90 (beachAt (wBeach w) x)) | x <- [0, 4 .. width] ])
    -- 光るイベント
    flashes = map flashPic (wFlash w)
    flashPic f
      | fSite f = Color (hsbA 130 80 100 (fT f * 3)) (thickLine 2 (fX f, fY f) (fX f, ly))
      | otherwise = Color (hsbA 50 90 100 (fT f * 2.5)) (at (fX f, fY f) (ring (fR f) 2))
    -- 掃引線とサイト
    sweepAndSites = Color (hsb 0 80 100) (thickLine 2 (0, ly) (width, ly))
      : concat [ [dotAt (hsb 0 0 10) s 13, dotAt (if sy < ly then gray 1 else gray 0.45) s 9] | s@(_, sy) <- wSites w ]

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos
    | snd (toP5 pos) > 90 -> pure w { wSites = wSites w ++ [toP5 pos], wDirty = True }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPause = not (wPause w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 8 (wSpeed w + 0.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 0.5 (wSpeed w - 0.5) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wPause = True, wLy = min (height * 1.8) (wLy w + 12) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wPause = True, wLy = max 90 (wLy w - 12) }
  EventKey (Char c) Down _ _ -> case toLower c of
    'p' -> pure w { wPar = not (wPar w) }
    'r' -> do n <- randomRIO (8, 29)
              randomSites n w
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. ←→ で少しずつ動かし、2 本の放物線の交点が、ちょうどボロノイの辺の上を動くことを確かめよう
-- 2. 放物線が 1 本消える瞬間（円イベント）を見つけよう。そのとき掃引線は外接円のいちばん下に触れている
-- 3. 掃引線の真上にあるサイトの放物線はとても細い（縦線）。掃引線から離れるほど広がるのはなぜ？
-- 4. 本物のフォーチュン法は「イベントの順番待ちの列（優先度つきキュー）」と「ビーチラインの木」で O(n log n)。
--    このスケッチはすべての x で放物線を全部調べている（遅いが見やすい）。どこが速くできるか考えよう

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
