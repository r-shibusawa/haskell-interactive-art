-- ============================================
-- 10 バーコードを聞く
-- r を 0 から大きくしながら、生きているバーを音にする
--   H0（塊）= 低い音がだんだん減っていく　H1（穴）= 高い音が生まれて消える
-- 形のちがいが「曲」のちがいとして聞こえる
-- クリックで再生 / 1〜4 で形 / マウス左右 = 再生の速さ
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', nub, sortOn)
import qualified Data.Vector.Unboxed as U
import System.Random (mkStdGen, randomRs)

data World = World
  { wPts      :: ![Point]
  , wBars     :: ![Bar]
  , wShape    :: !Int
  , wR        :: !Float
  , wPlaying  :: !Bool
  , wSounding :: !IS.IntSet   -- いま鳴っているバーの番号
  , wPulse    :: !Float       -- 次に鳴らし直すまでの秒数
  }

seed :: Int
seed = 7

maxR, pulseGap :: Float
maxR = 160         -- ★ 計算する最大の半径
pulseGap = 0.45    -- ★ 音を鳴らし直す間隔（秒）。連続音の代わりに短い音をくり返す

initialWorld :: World
initialWorld = World [] [] 1 0 False IS.empty 0

main :: IO ()
main = runSketch (sketch "09-10 Barcode Sound" initialWorld)
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
makePoints env w = w { wPts = pts, wBars = bars, wR = 0, wSounding = IS.empty }
  where
    pts = map sample (take 26 (randomChunks seed 7))
    bars = [b | b <- persistence pts maxR, isInfinite (barDeath b) || barDeath b - barBirth b > 2]
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

stepWorld :: Env -> Float -> World -> IO World
stepWorld env dt w
  | not (wPlaying w) = pure w
  | otherwise = updateSound dt (if r' > maxR then w { wR = 0, wSounding = IS.empty } else w { wR = r' })
  where
    speed = mapC (fst (mouseXY env)) 0 (envW env) 0.2 1.5
    r' = wR w + speed * 60 * dt     -- p5.js は 1 フレームに speed ずつ進む

-- ★ 生きているバーを鳴らす。生まれたバーはすぐ鳴らし、生きている間はくり返し鳴らす
updateSound :: Float -> World -> IO World
updateSound dt w = do
  let r = wR w
      alive = [(i, b) | (i, b) <- zip [0 ..] (wBars w), barBirth b <= r, r < barDeath b]
      born = [ib | ib@(i, _) <- alive, not (IS.member i (wSounding w))]
      pulse = wPulse w - dt
      due = pulse <= 0
      voices = chooseVoices (if due then alive else born)
  forM_ voices $ \(f, vol, wave) -> playTone f vol (pulseGap + 0.05) wave
  pure w { wSounding = IS.fromList (map fst alive), wPulse = if due then pulseGap else pulse }

-- 同時に鳴らせる数には限りがあるので、高い音 4 つ、低い音 4 つまでにする
chooseVoices :: [(Int, Bar)] -> [(Float, Float, Wave)]
chooseVoices ibs =
  take 4 (nub [ (660 + fromIntegral (i `mod` 8) * 60, 0.12, Triangle) | (i, b) <- ibs, barDim b == 1 ])  -- ★ 穴の音
  ++ take 4 (nub [ (110 + fromIntegral (i `mod` 12) * 15, 0.1, Sine) | (i, b) <- ibs, barDim b == 0 ])  -- ★ 塊の音

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ drawComplex env pts r 0.25 0.7
  , Color (hsb 0 0 100) (Pictures [at (toG env x y) (disc 3) | (x, y) <- pts])
  , drawBarcode env (wBars w) bx 120 bw (envH env - 220) maxR (Just r)
  , txt env 20 40 18 (hsb 0 0 100)
      ((if wPlaying w then "Playing r = " ++ show (round r :: Int) else "Click to play")
        ++ "  Speed (mouse X)  Shape: " ++ shapeName (wShape w) ++ " (1-4)  Sounding: low " ++ show b0 ++ ", high " ++ show b1)
  , txt env 20 68 15 (hsb 0 0 60)
      "H0 bar = a low note (gone after a merge). H1 bar = a high note (rings while the hole is open)."
  , txt env 20 90 15 (hsb 0 0 60)
      "A circle is a tune where one high note lasts long."
  ]
  where
    pts = wPts w
    r = wR w
    bx = envW env * 0.58
    bw = envW env - bx - 50
    (b0, b1) = bettiAt (wBars w) r

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    pure w { wPlaying = not (wPlaying w), wSounding = IS.empty, wPulse = 0 }
  EventKey (Char c) Down _ _ | c >= '1' && c <= '4' -> pure (makePoints env w { wShape = fromEnum c - fromEnum '0' })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 円と塊を聞き比べる。高い音が「長く」鳴るのはどちら？
-- 2. 8 の字（4）は高い音が 2 つ重なる。和音に聞こえる？
-- 3. 音の高さを「棒の長さ」で決めると、本物の穴ほど目立つ音になる

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
