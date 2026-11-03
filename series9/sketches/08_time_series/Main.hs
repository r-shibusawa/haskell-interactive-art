-- ============================================
-- 08 時系列の形（周期があると輪になる）
-- 信号 x(t) を、(x(t), x(t+τ)) の点に変換する（遅延埋め込み）
-- 周期的な信号 → 点が輪を描く → H1 の長い棒。ノイズやカオス → 輪にならない
-- 1 = 正弦波 / 2 = 2 つの周期 / 3 = カオス（ロジスティック） / 4 = ノイズ / マウス左右 = 遅れ τ
-- クリックで信号を作り直す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', sortOn)
import qualified Data.Vector.Unboxed as U

data World = World
  { wKind   :: !Int
  , wSignal :: !(U.Vector Float)
  , wPts    :: ![Point]
  , wBars   :: ![Bar]
  , wDelay  :: !Int        -- 遅れ τ（Art の tau = 2π と区別する）
  }

maxR :: Float
maxR = 120                 -- ★ 計算する最大の半径

samples, cloudSize :: Int
samples = 300              -- ★ 信号の長さ
cloudSize = 40             -- ★ 点群に使う点の数

main :: IO ()
main = runSketch (sketch "09-08 Time Series" (World 1 U.empty [] [] 10))
  { skBack  = hsb 230 40 10
  , skStart = \env -> makeSignal env (World 1 U.empty [] [] 10)
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

kindName :: Int -> String
kindName k = case k of
  1 -> "Sine wave (periodic)"
  2 -> "Sum of 2 periods"
  3 -> "Chaos (logistic map)"
  _ -> "Noise"

makeSignal :: Env -> World -> IO World
makeSignal env w = do
  values <- case wKind w of
    1 -> pure [sin (tau * 5 * t i) | i <- is]
    2 -> pure [0.6 * sin (tau * 5 * t i) + 0.4 * sin (tau * 11 * t i) | i <- is]
    3 -> pure (map (\x -> x * 2 - 1) (take samples (drop 1 (iterate (\x -> 3.9 * x * (1 - x)) 0.3))))
    _ -> replicateM samples (randomRIO (-1, 1))
  pure (embed env w { wSignal = U.fromList values })
  where
    is = [0 .. samples - 1]
    t i = fromIntegral i / fromIntegral samples

-- ★ 遅延埋め込み：(x(t), x(t+τ)) を点にする（40 点だけ使う）
embed :: Env -> World -> World
embed env w = w { wPts = pts, wBars = persistence pts maxR }
  where
    cx = envW env * 0.3
    cy = envH env * 0.6
    s = min (envW env) (envH env) * 0.2
    n = U.length (wSignal w)
    delay = wDelay w
    pts = [ (cx + wSignal w U.! t * s, cy - wSignal w U.! (t + delay) * s)
          | i <- [0 .. cloudSize - 1]
          , let t = floor (remap (fromIntegral i) 0 (fromIntegral cloudSize) 0 (fromIntegral (n - delay - 1))) ]

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w =
  let nt = floor (mapC (fst (mouseXY env)) 0 (envW env) 1 40)
  in pure (if nt /= wDelay w then embed env w { wDelay = nt } else w)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ -- 上：信号
    Color (hsb 0 0 30) (Line [g left mid, g right mid])
  , Color (hsb 170 70 100) (thickPath 2
      [ g (remap (fromIntegral i) 0 (fromIntegral n) left right) (mid - v * amp)
      | (i, v) <- zip [0 :: Int ..] (U.toList (wSignal w)) ])
  , txt env left (mid - amp - 8) 14 (hsb 0 0 60) "Signal x(t)"
  -- 左下：埋め込んだ点群
  , Color (hsb 0 0 40) (Line (map (uncurry g) (wPts w)))
  , Pictures [ Color (hsb (remap (fromIntegral i) 0 40 180 330) 70 100) (at (g x y) (disc 4))
             | (i, (x, y)) <- zip [0 :: Int ..] (wPts w) ]
  , txt env (envW env * 0.3 - 80) (envH env * 0.6 + s + 30) 14 (hsb 0 0 60) "Horizontal x(t), vertical x(t + tau)"
  -- 右下：バーコード
  , drawBarcode env (wBars w) bx 260 bw (envH env - 360) maxR Nothing
  , txt env 20 40 18 (hsb 0 0 100)
      (kindName (wKind w) ++ " (1-4)  Delay tau = " ++ show (wDelay w) ++ " (mouse X)  -> long H1 bars = " ++ show holes)
  , txt env 20 68 15 (hsb 0 0 60)
      "A periodic signal draws a loop (nearly a circle when tau = 1/4 period). Used on ECG, EEG and stock prices."
  ]
  where
    g = toG env
    n = U.length (wSignal w)
    left = 60
    right = envW env - 60
    mid = 150
    amp = 45
    s = min (envW env) (envH env) * 0.2
    bx = envW env * 0.58
    bw = envW env - bx - 50
    holes = length [b | b <- wBars w, barDim b == 1, barLife maxR b > maxR * 0.2]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '4' -> makeSignal env w { wKind = fromEnum c - fromEnum '0' }
  EventKey (MouseButton LeftButton) Down _ _ -> makeSignal env w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 正弦波で τ を動かす。輪がいちばん丸くなる τ は？（周期 60 サンプルの 1/4 = 15）
-- 2. 2 つの周期だと、輪が「ねじれた」形になり H1 が複雑に
-- 3. カオスでも点は「決まった形」の上に乗る（シリーズ4のアトラクター）。ノイズとどう違う？

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
