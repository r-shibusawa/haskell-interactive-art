-- ============================================
-- 11 声の輪（マイクの波形を点群にする）
-- 08 と同じ遅延埋め込みを、マイクの波形にリアルタイムで
-- 「あー」と一定の声を出すと、波形は周期的 → 点は輪を描く → H1 の長い棒が立つ
-- 息や「す〜」（ノイズ）だと輪にならない。クリックでマイク ON / マウス左右 = 遅れ τ
-- ============================================
module Main (main) where

import Art
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', sortOn)
import qualified Data.Vector.Unboxed as U

data World = World
  { wMic   :: !(Maybe Mic)
  , wLive  :: !Bool              -- 本物のマイクの音か
  , wWave  :: !(U.Vector Float)  -- 最新の波形
  , wPts   :: ![Point]
  , wBars  :: ![Bar]
  , wHoles :: !Int
  , wSkip  :: !Int               -- 計算を間引くカウンタ
  }

maxR :: Float
maxR = 120          -- ★ 計算する最大の半径

cloudSize :: Int
cloudSize = 36      -- ★ 点群に使う点の数

main :: IO ()
main = runSketch (sketch "09-11 Mic Cloud" (World Nothing False U.empty [] [] 0 0))
  { skBack  = hsb 230 40 10
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 遅れ τ（サンプル数）。マイクは 22050 Hz なので、p5.js 版（2〜80）の半分の数で同じ時間になる
delayOf :: Env -> Int
delayOf env = floor (mapC (fst (mouseXY env)) 0 (envW env) 1 40)

stepWorld :: Env -> Float -> World -> IO World
stepWorld _ _ w@World { wMic = Nothing } = pure w
stepWorld env _ w@World { wMic = Just mic } = do
  samples <- micSamples mic
  live <- micLive mic
  let wave = U.fromList samples
      lvl = rms wave
      w1 = w { wWave = wave, wLive = live }
  pure (if lvl > 0.01 then updateCloud env lvl w1 else w1)

-- 音量（RMS）
rms :: U.Vector Float -> Float
rms v
  | U.null v = 0
  | otherwise = sqrt (U.sum (U.map (\x -> x * x) v) / fromIntegral (U.length v))

-- ★ 遅延埋め込み：(x(t), x(t+τ)) を 36 点
updateCloud :: Env -> Float -> World -> World
updateCloud env lvl w
  -- 計算は重いので 6 フレームに 1 回
  | wSkip w `mod` 6 == 0 =
      let bars = persistence pts maxR
      in w' { wBars = bars, wHoles = length [b | b <- bars, barDim b == 1, barLife maxR b > maxR * 0.15] }
  | otherwise = w'
  where
    w' = w { wPts = pts, wSkip = wSkip w + 1 }
    wave = wWave w
    n = U.length wave
    delay = delayOf env
    cx = envW env * 0.3
    cy = envH env * 0.6
    s = min (envW env) (envH env) * 0.2 / max (lvl * 3) 0.1
    pts = [ (cx + wave U.! t * s, cy - wave U.! (t + delay) * s)
          | i <- [0 .. cloudSize - 1]
          , let t = floor (remap (fromIntegral i) 0 (fromIntegral cloudSize) 0 (fromIntegral (n - delay - 1))) ]

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ -- 波形（上）
    Color (hsba 170 60 100 0.6) (thickPath 1.5
      [ g (remap (fromIntegral i) 0 (fromIntegral n) 60 (envW env - 60)) (150 - v * 120)
      | (i, v) <- zip [0 :: Int ..] (U.toList (wWave w)), even i ])
  -- 点群
  , Color (hsb 0 0 35) (Line (map (uncurry g) (wPts w)))
  , Pictures [ Color (hsb (remap (fromIntegral i) 0 36 180 330) 70 100) (at (g x y) (disc 4.5))
             | (i, (x, y)) <- zip [0 :: Int ..] (wPts w) ]
  , drawBarcode env (wBars w) bx 260 bw (envH env - 360) maxR Nothing
  , if wHoles w > 0
      then boldTxt env bx 230 34 (hsb 45 90 100) "Loop! (periodic)"
      else boldTxt env bx 230 34 (hsb 0 0 60) "No loop"
  , txt env 20 40 18 (hsb 0 0 100) status
  , txt env 20 68 15 (hsb 0 0 60)
      "Vocal cords vibrate periodically, so (x(t), x(t+tau)) draws a loop. Its shape changes with pitch and timbre."
  ]
  where
    g = toG env
    n = U.length (wWave w)
    bx = envW env * 0.58
    bw = envW env - bx - 50
    delayText = "  Delay tau = " ++ show (delayOf env) ++ " (mouse X)"
    status = case wMic w of
      Nothing -> "Click to turn the mic ON"
      Just _
        | wLive w   -> "Steady \"aaah\" -> loop. \"sss\" or breath -> no loop." ++ delayText
        | otherwise -> "No mic: demo signal." ++ delayText

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case (ev, wMic w) of
  (EventKey (MouseButton LeftButton) Down _ _, Nothing) -> do
    mic <- startMic
    pure w { wMic = Just mic }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 低い声と高い声で、輪が丸く見える τ は変わる？（周期の 1/4 ＝ 高い声ほど小さい τ）
-- 2. 2 人で同時に「あー」→ 輪は 2 つ？ ねじれる？
-- 3. 口笛（ほぼ正弦波）はきれいな円。「あ」と「い」で輪の形（音色）が変わる

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

-- 大きな太い文字（少しずらして重ね書きする）
boldTxt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
boldTxt env x y size c s = Pictures
  [txt env (x + dx) (y + dy) size c s | dx <- [-w, 0, w], dy <- [-w, 0, w]]
  where w = size / 45


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
