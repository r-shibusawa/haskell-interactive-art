-- ============================================
-- 00 意思決定をモデルにする ─ 生産計画
-- 工房で椅子を x 脚、テーブルを y 台つくる。木材・作業時間・塗装ラインには上限がある。利益がいちばん大きい計画は？
--   OR（オペレーションズ・リサーチ）の第一歩は「決めること（変数）」「目的（利益）」「制約（資源）」に分けて式にすること
-- ドラッグ = 生産計画（点）を動かす / P = 最適解を見せる / ←→ = 椅子の利益 / ↑↓ = テーブルの利益 / I = 整数だけ / R = 元に戻す
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Numeric (showFFloat)
import Data.Ord (comparing)
import Data.List (foldl', sortBy)
import Data.Char (toLower)

-- | 資源の制約：a·x + b·y ≤ cap
data Res = Res { rName :: String, rA, rB, rCap :: !Double, rUnit :: String, rHue :: !Float }

resources :: [Res]
resources =                            -- ★ 資源の制約
  [ Res "Wood" 2 3 120 "kg" 30
  , Res "Hours" 2 1 80 "h" 200
  , Res "Paint line" 0 1 35 "units" 300
  ]

xMax, yMax :: Double
xMax = 70                              -- グラフの範囲
yMax = 50

data World = World
  { wProfitA, wProfitB :: !Double      -- ★ 1 個あたりの利益（千円）：椅子・テーブル
  , wPlanX, wPlanY :: !Double          -- 今の生産計画（椅子 x、テーブル y）
  , wShowOpt, wIntOnly :: !Bool
  }

world0 :: World
world0 = World 30 40 10 10 False False

main :: IO ()
main = runSketch (sketch "30-00 Production Planning" world0)
  { skBack  = hsb 225 30 12
  , skDraw  = drawAll
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

-- グラフの位置と大きさ
data Layout = Layout { pl, pt, pw, ph :: !Float }

layout :: Env -> Layout
layout env =
  let w = min (envW env * 0.55) ((envH env - 190) * fl (xMax / yMax))
  in Layout 80 110 w (w * fl (yMax / xMax))

sx, sy :: Layout -> Double -> Float
sx Layout{..} x = pl + fl (x / xMax) * pw
sy Layout{..} y = pt + ph - fl (y / yMax) * ph

-- 制約をすべて満たすか（x ≥ 0, y ≥ 0 も）
isFeasible :: Double -> Double -> Bool
isFeasible x y = x >= -1e-9 && y >= -1e-9 && all (\r -> rA r * x + rB r * y <= rCap r + 1e-7) resources

allCons :: [(Double, Double, Double)]
allCons = [(rA r, rB r, rCap r) | r <- resources] ++ [(-1, 0, 0), (0, -1, 0)]

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures
  [ grid, region, ints, limits, dashed z (hsba 50 80 100 0.7), optimum, corners, plan, panel, hud ]
  where
    l@Layout{..} = layout env
    wd = envW env
    poly = feasibleVertices allCons
    z = wProfitA * wPlanX + wProfitB * wPlanY
    best = bestVertex wProfitA wProfitB poly
    ok = isFeasible wPlanX wPlanY
    dg = if wIntOnly then 0 else 1 :: Int

    -- 目盛り
    grid = Pictures $
      [seg 1 (hsba 0 0 100 0.1) (sx l x, pt) (sx l x, pt + ph) | x <- [0, 10 .. xMax]]
      ++ [seg 1 (hsba 0 0 100 0.1) (pl, sy l y) (pl + pw, sy l y) | y <- [0, 10 .. yMax]]
      ++ [txtC (sx l x) (pt + ph + 14) 14 (gr 60) (nf 0 x) | x <- [0, 10 .. xMax]]
      ++ [txtR (pl - 8) (sy l y + 5) 14 (gr 60) (nf 0 y) | y <- [0, 10 .. yMax]]
      ++ [ txtR (pl + pw) (pt + ph + 62) 14 (gr 80) "Chairs x ->"
         , txt (pl - 60) (pt - 12) 14 (gr 80) "^ Tables y" ]

    -- 実行可能領域（すべての制約を満たす多角形）
    region = fillPoly (hsba 120 60 70 0.22) [(sx l x, sy l y) | (x, y) <- poly]
    ints
      | wIntOnly = Pictures
          [ dot (hsba 0 0 100 (if isFeasible x y then 0.4 else 0.08)) (sx l x) (sy l y) 3
          | x <- [0, 2 .. xMax], y <- [0, 2 .. yMax] ]
      | otherwise = Blank

    -- 制約の境界線
    limits = Pictures (map limit resources)
    limit r = case clipLine (rA r) (rB r) (rCap r) of
      Nothing -> Blank
      Just ((x1, y1), (x2, y2)) -> Pictures
        [ seg 2 (hsb (rHue r) 70 100) (sx l x1, sy l y1) (sx l x2, sy l y2)
        , let s = rName r ++ " " ++ nf 0 (rA r) ++ "x+" ++ nf 0 (rB r) ++ "y<=" ++ nf 0 (rCap r)
          in if rA r == 0
               then txtR (sx l x2) (sy l y2 - 6) 14 (hsb (rHue r) 60 100) s
               else txt (sx l x2 + 6) (sy l y2 + 40) 14 (hsb (rHue r) 60 100) s
        ]

    -- 利益が zz になる直線（等利益線）を点線で描く
    dashed zz c = case clipLine wProfitA wProfitB zz of
      Nothing -> Blank
      Just ((x1, y1), (x2, y2)) -> dash 2 c (sx l x1, sy l y1) (sx l x2, sy l y2)

    -- 最適解：多角形の頂点のどれか
    optimum = case best of
      Just (bx, by, bz) | wShowOpt ->
        let ax = sx l bx
            ay = sy l by
            n = sqrt (wProfitA * wProfitA + wProfitB * wProfitB)
            len = 50
        in Pictures
          [ dashed bz (hsb 50 90 100)
          , dot (hsb 50 90 100) ax ay 18
          , seg 2 (hsb 50 90 100) (ax, ay) (ax + fl (wProfitA / n) * len, ay - fl (wProfitB / n) * len)
          , txt (ax + 14) (ay - 12) 15 white ("Best (" ++ nf 1 bx ++ ", " ++ nf 1 by ++ ") profit " ++ nf 0 bz)
          ]
      _ -> Blank
    corners = Pictures [dot (hsba 0 0 100 0.7) (sx l x) (sy l y) 7 | (x, y) <- poly]

    -- 今の計画（点）
    plan = Pictures
      [ dot (if ok then hsb 140 80 100 else hsb 0 80 100) (sx l wPlanX) (sy l wPlanY) 20
      , txtC (sx l wPlanX) (sy l wPlanY) 14 (gr 10) (if ok then "o" else "x")
      ]

    -- 右側：資源の使用量
    rx = pl + pw + 60
    ry = pt + 10
    bw = wd - rx - 40
    ty = ry + 320
    nRes = fromIntegral (length resources) :: Float
    panel = Pictures $
      [ txt rx ry 20 white ("Chairs " ++ nf dg wPlanX ++ " + Tables " ++ nf dg wPlanY)
      , txt rx (ry + 44) 26 (if ok then hsb 140 70 100 else hsb 0 70 100) ("Profit " ++ nf 0 z ++ "k yen")
      , if ok then Blank else txt (rx + 240) (ry + 44) 16 (hsb 0 70 100) "INFEASIBLE"
      ]
      ++ zipWith resBar [0 :: Int ..] resources
      ++ [ txt rx ty 14 (gr 70) "Model (linear programming)"
         , txt rx (ty + 26) 15 (gr 90) ("Maximize " ++ nf 0 wProfitA ++ "x + " ++ nf 0 wProfitB ++ "y") ]
      ++ [ txt rx (ty + 50 + fromIntegral i * 22) 15 (gr 90)
             (nf 0 (rA r) ++ "x + " ++ nf 0 (rB r) ++ "y <= " ++ nf 0 (rCap r) ++ " (" ++ rName r ++ ")")
         | (i, r) <- zip [0 :: Int ..] resources ]
      ++ [ txt rx (ty + 50 + nRes * 22) 15 (gr 90) "x >= 0, y >= 0" ]
      ++ [ case best of
             Just (_, _, bz) | bz > 0 -> txt rx (ty + 80 + nRes * 22) 15 (gr 60) ("Now at " ++ nf 1 (100 * z / bz) ++ "% of the best")
             _ -> Blank ]
    resBar i r =
      let used = rA r * wPlanX + rB r * wPlanY
          y = ry + 90 + fromIntegral i * 70
      in Pictures
        [ txt rx y 14 (gr 85) (rName r ++ " " ++ nf 1 used ++ " / " ++ nf 0 (rCap r) ++ " " ++ rUnit r ++ "  left " ++ nf 1 (rCap r - used))
        , box (gr 25) rx (y + 10) bw 18
        , box (if used > rCap r + 1e-7 then hsb 0 80 100 else hsb (rHue r) 60 90) rx (y + 10) (bw * fl (min 1.3 (used / rCap r)) / 1.3) 18
        , seg 2 white (rx + bw / 1.3, y + 6) (rx + bw / 1.3, y + 32)
        ]

    hud = Pictures
      [ txt 20 40 16 white ("Drag: plan  P: best " ++ onOff wShowOpt ++ "  L/R chair profit " ++ nf 0 wProfitA ++ "  U/D table profit " ++ nf 0 wProfitB ++ "  I: integer " ++ onOff wIntOnly ++ "  R: reset")
      , txt 20 68 14 (gr 60) "Green = feasible plans. Yellow dashes = equal profit: slide them up-right; the last corner touched is best."
      ]

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- キーとマウス
onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> movePlan env w
  EventMotion _ | envDown env -> movePlan env w
  EventKey (Char c) Down _ _ -> onChar (toLower c) w
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wProfitA = min 100 (wProfitA w + 5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wProfitA = max 5 (wProfitA w - 5) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wProfitB = min 100 (wProfitB w + 5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wProfitB = max 5 (wProfitB w - 5) }
  _ -> w

onChar :: Char -> World -> World
onChar 'p' w = w { wShowOpt = not (wShowOpt w) }
onChar 'i' w
  | wIntOnly w = w { wIntOnly = False }
  | otherwise = w { wIntOnly = True, wPlanX = roundD (wPlanX w), wPlanY = roundD (wPlanY w) }
onChar 'r' w = w { wProfitA = 30, wProfitB = 40, wPlanX = 10, wPlanY = 10, wShowOpt = False }
onChar _ w = w

movePlan :: Env -> World -> World
movePlan env w
  | mx < pl - 20 || mx > pl + pw + 20 || my < pt - 20 || my > pt + ph + 20 = w
  | wIntOnly w = w { wPlanX = roundD x, wPlanY = roundD y }
  | otherwise = w { wPlanX = x, wPlanY = y }
  where
    Layout{..} = layout env
    (mx, my) = mouseP env
    x = max 0 (min xMax (realToFrac ((mx - pl) / pw) * xMax))
    y = max 0 (min yMax (realToFrac ((pt + ph - my) / ph) * yMax))

roundD :: Double -> Double
roundD v = fromIntegral (round v :: Int)

-- ---------- やってみよう ----------
-- 1. まず P を押さずに、ドラッグだけで利益が最大の計画を探そう。最適解（P）とどれくらい違った？
-- 2. ←→ で椅子の利益を上げていくと、最適な角が別の角へジャンプする。いくつで変わる？
-- 3. 4 つ目の制約（例：「椅子は 25 脚まで」→ Res "Chair limit" 1 0 25 "pcs" 90）を resources に足してみよう
-- 4. I で整数だけにすると、最適な角が整数でないとき何が起こる？（→ 02 整数計画）


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標で描くための変換
screen :: Env -> Picture -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2)

-- マウスの位置（左上が原点、y は下向き）
mouseP :: Env -> Point
mouseP = toP5 <*> envMouse

-- gloss の座標 → 左上が原点の座標
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字：(x, y) は左端のベースライン
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label x (negate y)

-- 文字：(x, y) が文字の中心
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y = labelC x (negate y)

-- 文字：右端を x にそろえる
txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtR x y size = \c s -> label (x - fromIntegral (length s) * size * 0.72) (negate y) size c s

-- 塗りつぶした長方形：左上 (x, y)、幅 w、高さ h
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (rect (x + w / 2) (negate (y + h / 2)) w h)

-- 線分（太さ t）
seg :: Float -> Color -> Point -> Point -> Picture
seg t c (x1, y1) (x2, y2) = Color c (thickLine t (x1, negate y1) (x2, negate y2))

-- 塗りつぶした凸多角形
fillPoly :: Color -> [Point] -> Picture
fillPoly c ps = Color c (Polygon [(x, negate y) | (x, y) <- ps])

-- 点線（8 描いて 6 空ける）
dash :: Float -> Color -> Point -> Point -> Picture
dash t c (x1, y1) (x2, y2) =
  let l = dist2 (x1, y1) (x2, y2)
      pt d = (x1 + (x2 - x1) * d / l, y1 + (y2 - y1) * d / l)
      ds = takeWhile (< l) [0, 14 ..]
  in if l < 1 then Blank else Pictures [seg t c (pt d) (pt (min l (d + 8))) | d <- ds]

-- 塗りつぶした円：中心 (x, y)、直径 d
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (at (x, negate y) (disc (d / 2)))

-- 灰色（明るさ 0〜100）
gr :: Float -> Color
gr = hsb 0 0

-- 小数点以下 d 桁の文字列（p5.js の nf）
nf :: RealFloat a => Int -> a -> String
nf d x
  | d <= 0 = show (round x :: Int)
  | otherwise = showFFloat (Just d) x ""

-- Double → Float（計算は Double、描くのは Float）
fl :: Double -> Float
fl = realToFrac

-- 制約 (a, b, c)（a·x + b·y ≤ c）の境界線どうしの交点のうち、全部の制約を満たすもの = 多角形の頂点
feasibleVertices :: [(Double, Double, Double)] -> [(Double, Double)]
feasibleVertices cons =
  let ic = zip [0 :: Int ..] cons
      cands = [ (x, y)
              | (i, (a1, b1, c1)) <- ic, (j, (a2, b2, c2)) <- ic, i < j
              , let det = a1 * b2 - a2 * b1
              , abs det >= 1e-9
              , let x = (c1 * b2 - c2 * b1) / det
              , let y = (a1 * c2 - a2 * c1) / det
              , all (\(a, b, c) -> a * x + b * y <= c + 1e-7) cons ]
      addNew ps p@(x, y) = if any (\(u, v) -> abs (u - x) + abs (v - y) < 1e-6) ps then ps else ps ++ [p]
      pts = foldl' addNew [] cands
      n = fromIntegral (length pts)
      cx = sum (map fst pts) / max 1 n
      cy = sum (map snd pts) / max 1 n
  in sortBy (comparing (\(x, y) -> atan2 (y - cy) (x - cx))) pts

-- 頂点の中で目的関数 p·x + q·y が一番大きいもの (x, y, z)
bestVertex :: Double -> Double -> [(Double, Double)] -> Maybe (Double, Double, Double)
bestVertex p q = foldl' pick Nothing
  where
    pick b (x, y) =
      let z = p * x + q * y
      in case b of
           Just (_, _, bz) | z <= bz + 1e-9 -> b
           _ -> Just (x, y, z)

-- 直線 a·x + b·y = c のうち、グラフの枠の中に入る部分
clipLine :: Double -> Double -> Double -> Maybe ((Double, Double), (Double, Double))
clipLine a b c =
  let fromX = [(x, y) | abs b > 1e-9, x <- [0, xMax], let y = (c - a * x) / b, y >= 0, y <= yMax]
      fromY = [(x, y) | abs a > 1e-9, y <- [0, yMax], let x = (c - b * y) / a, x >= 0, x <= xMax]
      pts = sortBy (comparing fst <> comparing (negate . snd)) (fromX ++ fromY)
  in case pts of
       (p : _ : _) -> Just (p, last pts)
       _ -> Nothing

