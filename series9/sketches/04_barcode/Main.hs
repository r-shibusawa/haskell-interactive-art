-- ============================================
-- 04 パーシステント・バーコード（穴の一生を記録する）
-- r を 0 から大きくしていき、塊や穴が「生まれた r」と「消えた r」を棒で表す
-- 長い棒 = ずっと存在する本物の特徴。短い棒 = ノイズ
-- 自動で r が動く / スペースで一時停止 / 1〜4 で形 / クリックで作り直す
-- ============================================
module Main (main) where

import Art
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', sortOn)
import qualified Data.Vector.Unboxed as U
import System.Random (mkStdGen, randomRs)

data World = World
  { wPts    :: ![Point]
  , wBars   :: ![Bar]
  , wR      :: !Float   -- いまの半径
  , wShape  :: !Int
  , wSeed   :: !Int
  , wPaused :: !Bool
  }

maxR, rSpeed :: Float
maxR = 170              -- ★ 計算する最大の半径
rSpeed = 36             -- ★ r が 1 秒に増える量（p5.js の 1 フレーム 0.6）

initialWorld :: World
initialWorld = World [] [] 0 1 5 False

main :: IO ()
main = runSketch (sketch "09-04 Barcode" initialWorld)
  { skBack  = hsb 230 40 10
  , skStart = \env -> pure (makePoints env initialWorld)
  , skStep  = \_ dt w -> pure (if wPaused w then w else w { wR = wrapF maxR (wR w + rSpeed * dt) })
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

shapeName :: Int -> String
shapeName k = case k of
  1 -> "Circle"
  2 -> "2 circles"
  3 -> "Blob"
  _ -> "Figure 8"

makePoints :: Env -> World -> World
makePoints env w = w { wPts = pts, wBars = persistence pts maxR, wR = 0 }   -- ★ バーコードを計算
  where
    pts = map sample (take 28 (randomChunks (wSeed w) 7))
    cx = envW env * 0.3
    cy = envH env / 2 + 30
    bigR = min (envW env) (envH env) * 0.22
    sample (ua : us : ur : g1 : g2 : g3 : g4 : _) =
      let a = ua * tau
          s = if us < 0.5 then -1 else 1
          (x, y) = case wShape w of
            1 -> (cx + cos a * bigR, cy + sin a * bigR)
            2 -> (cx + s * bigR * 1.1 + cos a * bigR * 0.6, cy + sin a * bigR * 0.6)
            3 -> let rr = bigR * sqrt ur in (cx + cos a * rr, cy + sin a * rr)
            _ -> (cx + bigR * 1.2 * sin a, cy + bigR * 0.8 * sin a * cos a)
      in (x + gaussian g1 g2 * 6, y + gaussian g3 g4 * 6)
    sample _ = (cx, cy)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ -- 左：複体
    drawComplex env pts r 0.25 0.7
  , Color (hsba 0 0 100 0.06) (Pictures [at (toG env x y) (disc r) | (x, y) <- pts])
  , Color (hsb 0 0 100) (Pictures [at (toG env x y) (disc 3) | (x, y) <- pts])
  -- 右：バーコード
  , drawBarcode env (wBars w) bx by bw bh maxR (Just r)   -- ★ 道具
  , txt env bx (by - 30) 14 (hsb 200 80 100) "# H0 (blobs)"
  , txt env (bx + 170) (by - 30) 14 (hsb 20 80 100) "# H1 (holes)"
  , txt env bx (by + bh + 44) 14 (hsb 0 0 60) "Left end = born r, right end = died r"
  , txt env bx (by + bh + 66) 14 (hsb 0 0 60) "(reaches the right edge = lives forever)"
  , txt env 20 40 18 (hsb 0 0 100)
      ("r = " ++ show (round r :: Int) ++ "  b0 = " ++ show b0 ++ ", b1 = " ++ show b1
        ++ "  Space: " ++ (if wPaused w then "resume" else "pause") ++ "  Shape: " ++ shapeName (wShape w) ++ " (1-4)  Click: new")
  , txt env 20 68 15 (hsb 0 0 60)
      "Yellow line = current r. One H0 bar per point; one ends at each merge. A long H1 bar = a real hole."
  ]
  where
    pts = wPts w
    r = wR w
    bx = envW env * 0.58
    bw = envW env - bx - 50
    by = 120
    bh = envH env - 220
    (b0, b1) = bettiAt (wBars w) r

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _ | c >= '1' && c <= '4' -> pure (makePoints env w { wShape = fromEnum c - fromEnum '0' })
  EventKey (MouseButton LeftButton) Down _ _ -> do
    s <- randomRIO (0, 9999)
    pure (makePoints env w { wSeed = s })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 4 つの形で、H1 の長い棒の数を比べる（円 1、2 つの円 2、塊 0、8 の字 2）
-- 2. 短い H1 の棒はどこから来る？（点が偶然 4 つ並んだところ。r を止めて見てみよう）
-- 3. 長い H0 の棒が 2 本残る形は？（2 つの塊が離れている）

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

-- 種から決まる 0〜1 の乱数を k 個ずつ区切った列（p5.js の randomSeed + random）。
-- 同じ種なら毎回同じ点群になる
randomChunks :: Int -> Int -> [[Float]]
randomChunks seed k = go (randomRs (0, 1) (mkStdGen seed))
  where go us = let (a, b) = splitAt k us in a : go b

-- 0〜1 の乱数 2 つから正規分布の乱数を作る（p5.js の randomGaussian）
gaussian :: Float -> Float -> Float
gaussian u v = sqrt (-2 * log (max 1e-6 u)) * cos (tau * v)


-- ここでは点は 2 次元
type Pt = Point

ptDist :: Pt -> Pt -> Float
ptDist = dist2
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

-- 複体を描く：三角形（透明度 triA）と辺（透明度 edgeA）
drawComplex :: Env -> [Point] -> Float -> Float -> Float -> Picture
drawComplex env pts r triA edgeA = Pictures
  [ Color (hsba 170 60 60 triA) (Pictures [Polygon [p i, p j, p k] | (i, j, k, _) <- cxTriangles cx])
  , Color (hsba 170 60 100 edgeA) (Pictures [thickLine 1.5 (p i) (p j) | (i, j, _) <- cxEdges cx])
  ]
  where
    cx = ripsComplex pts r
    table = U.fromList (map (uncurry (toG env)) pts)
    p i = table U.! i
