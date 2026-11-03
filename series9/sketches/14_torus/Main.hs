-- ============================================
-- 14 3 次元の形：球とトーラス（おまけ）
-- 点が 3 次元でも計算は同じ。球面の点は穴なし、ドーナツ（トーラス）の点は輪が 2 つ
-- 1 = 球 / 2 = トーラス / 3 = 2 つの球 / マウスで回転 / スペースで計算
-- ============================================
module Main (main) where

import Art
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', sortOn)
import qualified Data.Vector.Unboxed as U

data World = World
  { wPts      :: ![V3]
  , wEdges    :: ![(Int, Int)]   -- 見た目用の辺（r = 0.35 のリップス）
  , wKind     :: !Int
  , wBars     :: ![Bar]
  , wComputed :: !Bool
  }

maxR :: Float
maxR = 1.1          -- ★ 計算する最大の半径

numPoints :: Int
numPoints = 48      -- ★ 点の数（増やすと三角形の数が急増する）

main :: IO ()
main = runSketch (sketch "09-14 Sphere and Torus" (makePoints 2))
  { skBack  = hsb 230 40 10
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

kindName :: Int -> String
kindName k = case k of
  1 -> "Sphere"
  2 -> "Torus (donut)"
  _ -> "2 spheres"

makePoints :: Int -> World
makePoints kind = World pts edges kind [] False
  where
    n = fromIntegral numPoints :: Float
    pts = map point [0 .. numPoints - 1]
    golden = pi * (1 + sqrt 5)
    point i =
      let fi = fromIntegral i
          u = tau * fromIntegral (i `mod` 8) / 8 + fromIntegral ((i `div` 8) `mod` 2) * 0.39
          v = tau * fromIntegral (i `div` 8) / 6
      in case kind of
           1 -> let a = acos (1 - 2 * ((fi + 0.5) / n))
                    b = golden * fi
                in V3 (sin a * cos b) (sin a * sin b) (cos a)
           2 -> let bigR = 1
                    tube = 0.45
                in V3 ((bigR + tube * cos v) * cos u) ((bigR + tube * cos v) * sin u) (tube * sin v)
           _ -> let a = acos (1 - 2 * ((fromIntegral (i `mod` 24) + 0.5) / 24))
                    b = golden * fi
                    s = if i < 24 then -1.3 else 1.3
                in V3 (0.6 * sin a * cos b + s) (0.6 * sin a * sin b) (0.6 * cos a)
    indexed = zip [0 ..] pts
    edges = [(i, j) | (i, p) <- indexed, (j, q) <- indexed, i < j, ptDist p q < 0.7]

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ -- 辺（r = 0.35 のリップス、見た目用）
    Color (hsba 170 60 100 0.4) (Pictures [ thickLine 1 (screenOf i) (screenOf j) | (i, j) <- wEdges w ])
  -- 点：奥から順に描く（手前ほど明るく大きい）
  , Pictures [ Color (hsb 170 70 (mapDepth depth 50 100)) (at p (disc (mapDepth depth 5 11 / 2)))
             | (p, depth) <- depthSort snd projected ]
  , if wComputed w
      then Pictures
        [ drawBarcode env (wBars w) bx 120 bw (h - 220) maxR Nothing
        , txt env bx (h - 40) 28 (hsb 0 0 100) ("Long H1 bars = " ++ show holes) ]
      else txt env bx 200 20 (hsb 0 0 60) "Press Space to compute"
  , txt env 20 40 18 (hsb 0 0 100)
      ("Shape: " ++ kindName (wKind w) ++ " (1-3)  Mouse: rotate  Space: compute H1")
  , txt env 20 68 15 (hsb 0 0 60)
      "Expect: sphere 0 holes, torus 2 loops (around the hole and the tube), 2 spheres 2 blobs and 0 holes."
  , txt env 20 90 15 (hsb 0 0 60)
      "In 3D, all we need is distance: the calculation is the same."
  ]
  where
    (mx, my) = mouseXY env
    h = envH env
    ry = remap mx 0 (envW env) (-pi) pi
    rx = remap my 0 h (-1) 1
    cx = envW env * 0.3
    cy = h * 0.55
    s = min (envW env) h * 0.18
    bx = envW env * 0.58
    bw = envW env - bx - 50
    -- 回転して画面に写す：V3 の x が横、z が縦、y が奥行き
    rotated = [rotX rx (rotZ ry p) | p <- wPts w]
    projected = [(toG env (cx + x * s) (cy - z * s), y) | V3 x y z <- rotated]
    table = IM.fromList (zip [0 ..] (map fst projected))
    screenOf i = IM.findWithDefault (0, 0) i table
    mapDepth d = remap d (-1.5) 1.5
    holes = length [b | b <- wBars w, barDim b == 1, barLife maxR b > maxR * 0.15]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> pure (makePoints (fromEnum c - fromEnum '0'))
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wBars = persistence (wPts w) maxR, wComputed = True }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. トーラスの 2 本の H1 の棒の長さは同じ？（短い方 = 筒の太さ、長い方 = ドーナツの穴）
-- 2. 球には H2（空洞）があるはず。この道具は H1 までしか計算しない。どう拡張する？
-- 3. 点を 48 → 100 にすると、計算時間はどれくらい増える？（三角形の数が急増）

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 範囲からはみ出さない remap（p5.js の map(v, a, b, c, d, true)）
mapC :: Float -> Float -> Float -> Float -> Float -> Float
mapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)

-- ここでは点は 3 次元
type Pt = V3

ptDist :: Pt -> Pt -> Float
ptDist a b = norm3 (sub3 a b)

-- ==================================================
-- ここから下は「TDA の道具」です（リップス複体とパーシステント・ホモロジーの計算）
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- リップス複体：edges は (i, j, 長さ)、triangles は (i, j, k, 最長辺の長さ)
data Complex = Complex
  { cxEdges     :: [(Int, Int, Float)]
  , cxTriangles :: [(Int, Int, Int, Float)]
  }

-- 半径 r（直径 2r）以内の点どうしをつなぐ
ripsComplex :: [Pt] -> Float -> Complex
ripsComplex pts r = Complex edges triangles
  where
    n = length pts
    lim = 2 * r
    table = U.fromList [ptDist a b | a <- pts, b <- pts]
    d i j = table U.! (i * n + j)
    edges = [(i, j, d i j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1], d i j <= lim]
    triangles =
      [ (i, j, k, max dij (max (d j k) (d i k)))
      | (i, j, dij) <- edges, k <- [j + 1 .. n - 1], d i k <= lim, d j k <= lim ]

-- バー：dim は 0（塊）か 1（穴）、death は半径 r の値（無限なら 1/0）
data Bar = Bar { barDim :: !Int, barBirth :: !Float, barDeath :: !Float }

-- 棒の長さ（無限に生きる棒は maxR まで）
barLife :: Float -> Bar -> Float
barLife maxR b = (if isInfinite (barDeath b) then maxR else barDeath b) - barBirth b

-- 単体：頂点の番号の並びと、生まれる半径
data Simplex = Simplex { sDim :: !Int, sBirth :: !Float, sVerts :: [Int] }

-- パーシステント・ホモロジー（Z2 係数、列簡約）。maxR：この半径まで計算（大きいほど重い）
persistence :: [Pt] -> Float -> [Bar]
persistence pts maxR = reverse finiteBars ++ foreverBars
  where
    n = length pts
    Complex es ts = ripsComplex pts maxR
    -- 単体のリスト：頂点（誕生 0）、辺（長さ/2）、三角形（最長辺/2）を、生まれる順に並べる
    simplices = sortOn (\s -> (sBirth s, sDim s)) $
         [Simplex 0 0 [i] | i <- [0 .. n - 1]]
      ++ [Simplex 1 (e / 2) [i, j] | (i, j, e) <- es]
      ++ [Simplex 2 (e / 2) [i, j, k] | (i, j, k, e) <- ts]
    births = U.fromList (map sBirth simplices)
    dims = U.fromList (map sDim simplices)
    -- (i, j) の単体が何番目か（頂点は i = j のところに入れる）
    position = U.replicate (n * n) 0 U.// concat (zipWith place [0 ..] simplices)
    place p s = case sVerts s of
      [i]    -> [(i * n + i, p)]
      [i, j] -> [(i * n + j, p)]
      _      -> []
    pos i j = position U.! (i * n + j)
    -- 境界行列の列（Z2 なので「含まれる行の番号」のリスト。大きい順）
    boundary s = case sVerts s of
      [i, j]    -> sortDesc [pos i i, pos j j]
      [i, j, k] -> sortDesc [pos i j, pos j k, pos i k]
      _         -> []
    -- 列簡約：同じ「いちばん下の 1」を持つ列があれば足し合わせる（XOR）
    (pivots, nonZero, finiteBars) = foldl' reduceColumn (IM.empty, IS.empty, []) (zip [0 ..] (map boundary simplices))
    reduceColumn (!piv, !nz, !out) (j, col) = case reduced piv col of
      [] -> (piv, nz, out)
      c@(low : _) ->
        let birth = births U.! low
            death = births U.! j
            out' = if death > birth + 1e-9 then Bar (dims U.! low) birth death : out else out
        in (IM.insert low c piv, IS.insert j nz, out')
    reduced piv col = case col of
      low : _ | Just c <- IM.lookup low piv -> reduced piv (xorDesc col c)
      _ -> col
    -- 誰にも殺されなかった「生む」単体 → 無限に生きるバー
    foreverBars =
      [ Bar (sDim s) (sBirth s) (1 / 0)
      | (j, s) <- zip [0 ..] simplices, sDim s < 2, not (IS.member j nonZero), not (IM.member j pivots) ]

sortDesc :: [Int] -> [Int]
sortDesc = sortOn negate

-- 大きい順に並んだリストの対称差（Z2 の足し算）
xorDesc :: [Int] -> [Int] -> [Int]
xorDesc [] bs = bs
xorDesc as [] = as
xorDesc as@(a : as') bs@(b : bs')
  | a > b     = a : xorDesc as' bs
  | b > a     = b : xorDesc as bs'
  | otherwise = xorDesc as' bs'

-- 半径 r のときのベッチ数 (β0, β1)（バーの中で r をまたぐものを数える）
bettiAt :: [Bar] -> Float -> (Int, Int)
bettiAt bars r = (count 0, count 1)
  where count k = length [b | b <- bars, barDim b == k, barBirth b <= r, r < barDeath b]

-- バーコードを描く（x, y, w, h の枠に、半径 0〜maxR）
drawBarcode :: Env -> [Bar] -> Float -> Float -> Float -> Float -> Float -> Maybe Float -> Picture
drawBarcode env bars x y w h maxR currentR = Pictures
  ( Color (hsb 0 0 30) (Line [toG env x (y + h), toG env (x + w) (y + h)])
  : zipWith oneBar [0 ..] sorted
  ++ [ Color (hsb 45 90 100) (thickLine 2 (toG env (sx r) y) (toG env (sx r) (y + h))) | Just r <- [currentR] ]
  ++ [ txt env x (y + h + 18) 14 (hsb 0 0 60) "r = 0"
     , txt env (x + w - 70) (y + h + 18) 14 (hsb 0 0 60) ("r = " ++ showMax) ] )
  where
    sorted = sortOn (\b -> (barDim b, barBirth b))
      [b | b <- bars, barDeath b - barBirth b > maxR * 0.01 || isInfinite (barDeath b)]
    rowH = min 14 (h / fromIntegral (max 1 (length sorted)))
    sx r = x + r / maxR * w
    showMax = show (round maxR :: Int)
    oneBar :: Float -> Bar -> Picture
    oneBar i b =
      let by = y + i * rowH + rowH / 2
      in Color (hsb (if barDim b == 0 then 200 else 20) 80 100)
           (thickLine (max 2 (rowH - 4)) (toG env (sx (barBirth b)) by) (toG env (sx (min (barDeath b) maxR)) by))
