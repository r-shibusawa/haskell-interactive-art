-- ============================================
-- 05 パーシステンス図とノイズへの強さ
-- バーを点にする：横 = 生まれた r、縦 = 消えた r。対角線から遠い点ほど長生き = 本物の特徴
-- ノイズを増やしても、本物の穴の点は対角線から離れたまま（安定性定理）
-- マウス上下 = ノイズの大きさ / 1〜4 で形 / クリックで作り直す
-- ============================================
module Main (main) where

import Art
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', sortOn)
import qualified Data.Vector.Unboxed as U
import System.Random (mkStdGen, randomRs)

data World = World
  { wPts   :: ![Point]
  , wBars  :: ![Bar]
  , wShape :: !Int
  , wSeed  :: !Int
  , wNoise :: !Int
  }

maxR :: Float
maxR = 170                 -- ★ 計算する最大の半径

realLife :: Float
realLife = maxR * 0.2      -- ★ 「本物の穴」とみなす寿命

initialWorld :: World
initialWorld = World [] [] 1 5 5

main :: IO ()
main = runSketch (sketch "09-05 Diagram and Noise" initialWorld)
  { skBack  = hsb 230 40 10
  , skStart = \env -> pure (makePoints env initialWorld)
  , skStep  = stepWorld
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
makePoints env w = w { wPts = pts, wBars = persistence pts maxR }
  where
    pts = map sample (take 28 (randomChunks (wSeed w) 7))
    noiseAmt = fromIntegral (wNoise w)
    cx = envW env * 0.28
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
      in (x + gaussian g1 g2 * noiseAmt, y + gaussian g3 g4 * noiseAmt)
    sample _ = (cx, cy)

-- マウス上下でノイズが変わったときだけ作り直す
stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w =
  let na = round (mapC (snd (mouseXY env)) 0 (envH env) 40 0)
  in pure (if na /= wNoise w then makePoints env w { wNoise = na } else w)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ -- 左：点群
    Color (hsb 170 70 100) (Pictures [at (toG env x y) (disc 3.5) | (x, y) <- wPts w])
  -- 右：パーシステンス図
  , Color (hsb 0 0 30) (Line [g dx dy, g (dx + ds) dy, g (dx + ds) (dy + ds), g dx (dy + ds), g dx dy])
  , Color (hsb 0 0 45) (Line [g dx (dy + ds), g (dx + ds) dy])     -- 対角線
  -- 対角線に近い帯（ノイズ領域の目安）
  , Color (hsba 0 0 100 0.05) (Polygon [g dx (dy + ds), g (dx + ds) dy, g (dx + ds) (dy + 30), g (dx + 30) (dy + ds)])
  , Pictures (map barDot (wBars w))
  , txt env (dx + ds - 110) (dy + ds + 20) 14 (hsb 0 0 60) "born r ->"
  , txt env (dx - 10) (dy - 10) 14 (hsb 0 0 60) "died r ^"
  , txt env dx (dy + ds + 40) 14 (hsb 200 80 100) "o H0"
  , txt env (dx + 60) (dy + ds + 40) 14 (hsb 20 80 100) "o H1"
  , txt env 20 40 18 (hsb 0 0 100)
      ("Noise " ++ show (wNoise w) ++ " (mouse Y)  Shape: " ++ shapeName (wShape w) ++ " (1-4)  Long-lived holes = " ++ show realHoles ++ "  Click: new")
  , txt env 20 68 15 (hsb 0 0 60)
      "Near the diagonal (pale band) = dies fast = noise. Far away = long life = the true shape."
  , txt env 20 90 15 (hsb 0 0 60)
      "Add noise: the big H1 dot barely moves (stability)."
  ]
  where
    g = toG env
    dx = envW env * 0.55
    ds = min (envW env * 0.4) (envH env - 200)
    dy = 110
    barDot b =
      let px = dx + barBirth b / maxR * ds
          py = dy + ds - min (barDeath b) maxR / maxR * ds
          forever = isInfinite (barDeath b)
      in Pictures
           ( Color (hsba (if barDim b == 0 then 200 else 20) 80 100 (if forever then 1 else 0.8))
               (at (g px py) (disc (if barDim b == 1 then 7 else 4)))
           : [Color (hsb 200 80 100) (Line [g px py, g px dy]) | forever] )
    -- 本物の穴の数（寿命が長い H1）
    realHoles = length [b | b <- wBars w, barDim b == 1, barLife maxR b > realLife]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '4' -> pure (makePoints env w { wShape = fromEnum c - fromEnum '0' })
  EventKey (MouseButton LeftButton) Down _ _ -> do
    s <- randomRIO (0, 9999)
    pure (makePoints env w { wSeed = s })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. ノイズ 0 → 20 → 40 と増やして、H1 の大きな点がどこまで残るか見る
-- 2. 「本物」の判定（realLife = maxR × 0.2）のしきい値を変えると、答えは変わる？
-- 3. 図の上に「点の数」が多いほど対角線近くが混む。それでも遠い点は目立つ

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
