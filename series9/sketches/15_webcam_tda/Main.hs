-- ============================================
-- 15 カメラに映った形の穴を数える（おまけ）
-- カメラの映像を 2 値化し、暗い（または明るい）部分から点をサンプリングして TDA
-- 指で輪を作る、紙に描いた図形を見せる → 穴の数が出る
-- マウス左右 = しきい値 / I で白黒反転
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', sortOn)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)

data World = World
  { wCam    :: !(Maybe Camera)
  , wLive   :: !Bool
  , wOn     :: !(U.Vector Bool)   -- 2 値化した画像
  , wInvert :: !Bool
  , wPts    :: ![Point]
  , wBars   :: ![Bar]
  , wHoles  :: !Int
  , wComps  :: !Int
  , wTimer  :: !Float             -- 次の計算までの秒数
  }

camW, camH :: Int
camW = 96       -- ★ カメラの横の画素数
camH = 72       -- ★ カメラの縦の画素数

maxR, computeGap :: Float
maxR = 90           -- ★ 計算する最大の半径
computeGap = 0.7    -- ★ 計算の間隔（秒）

ox, oy :: Float
ox = 30
oy = 110

main :: IO ()
main = runSketch (sketch "09-15 Webcam TDA" (World Nothing False U.empty False [] [] 0 0 computeGap))
  { skBack  = hsb 230 40 10
  , skStart = \_ -> do
      cam <- startCamera camW camH
      pure (World (Just cam) False U.empty False [] [] 0 0 computeGap)
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

scaleOf :: Env -> Float
scaleOf env = min (envW env * 0.55 / fromIntegral camW) ((envH env - 160) / fromIntegral camH)

thresholdOf :: Env -> Float
thresholdOf env = mapC (fst (mouseXY env)) 0 (envW env) 30 220

stepWorld :: Env -> Float -> World -> IO World
stepWorld _ _ w@World { wCam = Nothing } = pure w
stepWorld env dt w@World { wCam = Just cam } = do
  frame <- cameraFrame cam
  live <- camLive cam
  let th = thresholdOf env
      on = binarize frame th (wInvert w)
      timer = wTimer w - dt
      w1 = w { wLive = live, wOn = on, wTimer = timer }
  pure (if timer <= 0 then samplePoints env w1 { wTimer = computeGap } else w1)

-- 2 値化：明るさ（RGB の平均）としきい値をくらべる
binarize :: B.ByteString -> Float -> Bool -> U.Vector Bool
binarize frame th inv
  | B.length frame < camW * camH * 3 = U.replicate (camW * camH) False
  | otherwise = U.generate (camW * camH) $ \i ->
      let byte k = fromIntegral (B.index frame (i * 3 + k)) :: Float
          b = (byte 0 + byte 1 + byte 2) / 3
      in if inv then b > th else b < th

-- ★ ON の画素から 40 点をサンプリング（間引く）して計算する
samplePoints :: Env -> World -> World
samplePoints env w
  | n > 10 = w { wPts = pts, wBars = bars
               , wHoles = length [b | b <- bars, barDim b == 1, barLife maxR b > maxR * 0.15]
               , wComps = length [b | b <- bars, barDim b == 0, barLife maxR b > maxR * 0.3] }
  | otherwise = w { wPts = [] }
  where
    cand = U.toList (U.findIndices id (wOn w))
    n = length cand
    stepN = max 1 (n `div` 40)
    sc = scaleOf env
    pts = take 40
      [ (ox + fromIntegral (i `mod` camW) * sc, oy + fromIntegral (i `div` camW) * sc)
      | (k, i) <- zip [0 :: Int ..] cand, k `mod` stepN == 0 ]
    bars = persistence pts maxR

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ -- 2 値化して表示（startCamera の映像はもう左右反転してあるので鏡になる）
    at (toG env (ox + fromIntegral camW * sc / 2) (oy + fromIntegral camH * sc / 2)) image
  , Color (hsb 170 80 100) (Pictures [at (toG env x y) (disc 3.5) | (x, y) <- wPts w])
  , if null (wBars w) then Blank else drawBarcode env (wBars w) bx 120 bw (h * 0.45) maxR Nothing
  , boldTxt env bx (h * 0.75) 60 (hsb 0 0 100) ("Holes " ++ show (wHoles w))
  , txt env bx (h * 0.75 + 44) 28 (hsb 0 0 100) ("Blobs " ++ show (wComps w))
  , txt env 20 40 18 (hsb 0 0 100)
      ("Threshold " ++ show (round (thresholdOf env) :: Int) ++ " (mouse X)  I: invert ("
        ++ (if wInvert w then "bright" else "dark") ++ " parts are ON)  Points " ++ show (length (wPts w))
        ++ (if wLive w then "" else "  No camera: demo video"))
  , txt env 20 68 15 (hsb 0 0 60)
      "Make a ring with your fingers, or show paper with \"O\" or \"8\". Use a plain background; adjust the threshold."
  ]
  where
    h = envH env
    sc = scaleOf env
    bx = envW env * 0.62
    bw = envW env - bx - 40
    on = wOn w
    image
      | U.null on = Blank
      | otherwise = pixelImage camW camH sc $ \x y ->
          if on U.! (x + y * camW) then onColor else offColor

onColor, offColor :: (Word8, Word8, Word8)
onColor = (255, 204, 51)     -- hsb(45, 80, 100)
offColor = (64, 64, 64)      -- hsb(0, 0, 25)

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char c) Down _ _ | c == 'i' || c == 'I' -> pure w { wInvert = not (wInvert w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 指で「OK」の輪を作って、穴 1 が出るか。両手で 2 つの輪なら？
-- 2. メガネ、ドーナツ、数字の 8 と 0 と 1。穴の数で区別できる
-- 3. サンプリングする点を 40 → 60 にすると正確になるが遅くなる。限界を探そう

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
