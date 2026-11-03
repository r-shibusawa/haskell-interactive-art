-- ============================================
-- 03 ドロネー三角形分割（Bowyer–Watson 法）
-- 点を結んで三角形に分ける方法のうち「どの三角形の外接円の中にも、ほかの点が入らない」もの = ドロネー三角形分割
--   Bowyer–Watson：点を 1 つ入れる → 外接円にその点が入る三角形（赤）を消す → できた穴のふちを新しい点とつなぐ（緑）
-- スペース = 1 ステップ / A = 自動 / ↑↓ = 速さ / F = 一気に完成 / クリック = 点を追加 / マウスを三角形に乗せる = 空の円 / R = 作り直し
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (find, foldl', partition)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import GHC.Float (double2Float, float2Double)

nPoints :: Int
nPoints = 40              -- ★ 最初の点の数

-- 三角形：頂点の番号 3 つと、外接円（中心と半径の 2 乗）
data Tri = Tri { tA, tB, tC :: !Int, tX, tY, tR2 :: !Double }

data World = World
  { wP     :: !(IM.IntMap PD)  -- 点（0〜2 は全体を囲む大きな三角形の頂点）
  , wTris  :: ![Tri]
  , wNext  :: !Int        -- 次に入れる点
  , wPhase :: !Int        -- 0 = 次の点へ, 1 = 赤い三角形を見せる, 2 = 緑の三角形を見せる
  , wBad   :: ![Tri]
  , wFresh :: ![Tri]
  , wAuto  :: !Bool
  , wWait  :: !Int        -- ★ 自動のとき 1 ステップに何フレームかけるか
  , wTimer :: !Int
  }

main :: IO ()
main = runSketch (sketch "22-03 Delaunay (Bowyer-Watson)" (world0 (IM.fromList [])))
  { skBack  = hsb 230 25 12
  , skStart = \_ -> reset True 20
  , skStep  = \_ _ w -> pure (autoStep w)
  , skDraw  = \env w -> drawAll (toP5 (envMouse env)) w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: IM.IntMap PD -> World
world0 p = World p [] 3 0 [] [] True 20 0

reset :: Bool -> Int -> IO World
reset auto wait = do
  let m = float2Double (max width height) * 10
      cx = float2Double width / 2
      cy = float2Double height / 2
      super = [(cx - m, cy - m), (cx + m, cy - m), (cx, cy + m)]
  ps <- replicateM nPoints ((,) <$> randomRIO (50, float2Double width - 50) <*> randomRIO (130, float2Double height - 40))
  let p = IM.fromList (zip [0 ..] (super ++ ps))
  pure (world0 p) { wTris = [mk p 0 1 2], wAuto = auto, wWait = wait }

pt :: IM.IntMap PD -> Int -> PD
pt p i = IM.findWithDefault (0, 0) i p

mk :: IM.IntMap PD -> Int -> Int -> Int -> Tri
mk p i j k = let (x, y, r2) = circumcircle (pt p i) (pt p j) (pt p k) in Tri i j k x y r2

inCircle :: PD -> Tri -> Bool
inCircle (x, y) t = (x - tX t) * (x - tX t) + (y - tY t) * (y - tY t) < tR2 t

-- ★ Bowyer–Watson の 1 ステップ
stepBW :: World -> World
stepBW w
  | wNext w >= IM.size p = w
  | wPhase w == 0 =
      -- 外接円の中に新しい点がある三角形
      w { wBad = filter (inCircle q) (wTris w), wFresh = [], wPhase = 1 }
  | wPhase w == 1 =
      -- 赤い三角形の辺のうち、1 回だけ出てくる辺 = 穴のふち
      let (bad, keep) = partition (inCircle q) (wTris w)
          toggle e (a, b) = let k = (min a b, max a b)
                            in if M.member k e then M.delete k e else M.insert k (a, b) e
          rim = foldl' toggle M.empty (concat [[(tA t, tB t), (tB t, tC t), (tC t, tA t)] | t <- bad])
          fresh = [mk p a b (wNext w) | (a, b) <- M.elems rim]
      in w { wTris = keep ++ fresh, wFresh = fresh, wBad = [], wPhase = 2 }
  | otherwise = w { wNext = wNext w + 1, wPhase = 0, wFresh = [] }
  where
    p = wP w
    q = pt p (wNext w)

autoStep :: World -> World
autoStep w
  | wAuto w && wNext w < IM.size (wP w) =
      if wTimer w + 1 >= wWait w then stepBW w { wTimer = 0 } else w { wTimer = wTimer w + 1 }
  | otherwise = w

isReal :: Tri -> Bool
isReal t = tA t > 2 && tB t > 2 && tC t > 2

triPts :: World -> Tri -> [Point]
triPts w t = map (fromPD . pt (wP w)) [tA t, tB t, tC t]

insideTri :: World -> Point -> Tri -> Bool
insideTri w (x, y) t = case triPts w t of
  [(ax, ay), (bx, by), (cx, cy)] ->
    let s1 = (bx - ax) * (y - ay) - (by - ay) * (x - ax)
        s2 = (cx - bx) * (y - by) - (cy - by) * (x - bx)
        s3 = (ax - cx) * (y - cy) - (ay - cy) * (x - cx)
    in (s1 >= 0 && s2 >= 0 && s3 >= 0) || (s1 <= 0 && s2 <= 0 && s3 <= 0)
  _ -> False

-- 外接円の輪（大きすぎる円・一直線の場合はかかない）
circleOf :: Float -> Tri -> Picture
circleOf t tri
  | tR2 tri < 1e10 = at (fromPD (tX tri, tY tri)) (ring (double2Float (sqrt (tR2 tri))) t)
  | otherwise = Blank

drawAll :: Point -> World -> Picture
drawAll m w = Pictures
  [ view (map triPic (wTris w) ++ concatMap badPic (wBad w) ++ concatMap freshPic (wFresh w) ++ map pointPic (IM.toList (wP w)) ++ hoverPic)
  , txt 20 30 16 (gray 1) ("Space: step  A: auto " ++ onOff (wAuto w) ++ "  Up/Down: speed  F: finish  Click: add  R: redo  Points "
      ++ show (min (next - 3) (total - 3)) ++ " / " ++ show (total - 3) ++ "  Triangles " ++ show (length (filter isReal (wTris w))))
  , txt 20 52 14 (hsb 0 0 65) msg
  , txt 20 74 14 (hsb 0 0 65) hoverMsg
  ]
  where
    total = IM.size (wP w)
    next = wNext w
    done = next >= total
    -- 三角形（大きな三角形につながるものは薄く）
    triPic t
      | isReal t = Pictures [Color (hsbA 200 40 40 40) (Polygon ps), Color (hsb 200 40 90) (outline 1.5 ps)]
      | otherwise = Color (hsbA 0 0 100 10) (poly ps)
      where ps = triPts w t
    -- 赤：消される三角形とその外接円
    badPic t = let ps = triPts w t in
      [ Color (hsbA 0 80 90 45) (Polygon ps), Color (hsb 0 80 100) (outline 2 ps)
      , if tR2 t < 4e6 then Color (hsbA 0 80 100 60) (circleOf 2 t) else Blank ]
    -- 緑：新しくできた三角形
    freshPic t = let ps = triPts w t in [Color (hsbA 130 70 80 45) (Polygon ps), Color (hsb 130 70 100) (outline 2 ps)]
    -- 点
    pointPic (i, q)
      | i < 3 = Blank
      | i == next = dotAt (hsb 50 90 100) (fromPD q) 16
      | i < next = dotAt (gray 1) (fromPD q) 8
      | otherwise = dotAt (gray 0.5) (fromPD q) 6
    -- マウスの下の三角形：外接円の中に点がない（空の円）ことを確かめる
    hover = find (\t -> isReal t && insideTri w m t) (wTris w)
    insideCount t = length [ () | i <- [3 .. next - 1], i /= tA t, i /= tB t, i /= tC t, inCircle (pt (wP w) i) t ]
    hoverPic = case hover of
      Nothing -> []
      Just t ->
        [ Color (if insideCount t > 0 then hsb 0 80 100 else hsb 50 90 100) (circleOf 2.5 t)
        , Color (hsbA 50 90 100 40) (Polygon (triPts w t))
        , dotAt (hsb 50 90 100) (fromPD (tX t, tY t)) 7 ]
    msg | done = "Finished. Hover a triangle to see its circumcircle (yellow). No circle holds another point = Delaunay."
        | wPhase w == 1 = "Red = triangles whose circumcircle contains the new yellow point. They are removed."
        | wPhase w == 2 = "Green = rim edges of the hole joined to the new point. The empty-circle rule holds again."
        | otherwise = "Insert the next point (yellow)."
    hoverMsg = case hover of
      Nothing -> ""
      Just t -> let c = insideCount t in
        "Points inside this circumcircle: " ++ show c ++ (if c > 0 then " (still in progress)" else " - empty circle OK")

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> do
    let (x, y) = toP5 pos
    if y > 90
      then do
        e <- randomRIO (-0.01, 0.01)
        pure w { wP = IM.insert (IM.size (wP w)) (float2Double x + e, float2Double y) (wP w) }
      else pure w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (stepBW w { wAuto = False })
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wWait = max 1 (wWait w - 4) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wWait = min 80 (wWait w + 4) }
  EventKey (Char c) Down _ _ -> case toLower c of
    'a' -> pure w { wAuto = not (wAuto w) }
    'f' -> pure (finish w) { wPhase = 0, wBad = [], wFresh = [] }
    'r' -> reset (wAuto w) (wWait w)
    _ -> pure w
  _ -> pure w

-- 全部の点を入れ終わるまでくり返す
finish :: World -> World
finish w | wNext w >= IM.size (wP w) = w
         | otherwise = finish (stepBW w)

-- ---------- やってみよう ----------
-- 1. 1 点入れるたびに赤い三角形は何個くらい？ 点が増えても平均はあまり増えない（だいたい一定）ことを確かめよう
-- 2. 点を正方形の格子にきっちり並べると、4 点が同じ円の上に乗る。どちらの対角線で切られる？（ドロネーは 1 通りに決まらない）
-- 3. 完成した図の「いちばん小さい角」を見てみよう。ドロネーは細長い三角形をなるべく作らない分割
-- 4. 大きな三角形の頂点 P[0〜2] を画面の中に置くと何が起きる？ 外側の三角形が残ってしまう理由を考えよう

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

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- 外接円 (x, y, r2) は桁が大きくなるので Double で計算する（r2 = 半径の 2 乗。3 点が一直線なら無限大）
type PD = (Double, Double)

fromPD :: PD -> Point
fromPD (x, y) = (double2Float x, double2Float y)

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
