-- ============================================
-- 12 点を置いて穴をつくる（自由制作のベース）
-- クリックで点を置くと、半径 r の複体とバーコードがその場で更新される
-- 穴が生まれた瞬間に音が鳴る。「穴を 3 つ作る」「できるだけ長生きの穴を作る」などのゲームに
-- クリックで点を置く / マウス左右（上の帯）= r / C で消す / S で保存
-- ============================================
-- ★ のついた数字を変えて、自分の TDA アートに！
module Main (main) where

import Art
import Control.Monad (when)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', sortOn)
import qualified Data.Vector.Unboxed as U

data World = World
  { wPts       :: ![Point]
  , wBars      :: ![Bar]
  , wR         :: !Float
  , wLastHoles :: !Int
  }

maxR :: Float
maxR = 160         -- ★ 計算する最大の半径

maxPoints :: Int
maxPoints = 40     -- ★ 最大 40 点（多いと重い）

main :: IO ()
main = runSketch (sketch "09-12 TDA Brush" (World [] [] 50 0))
  { skBack  = hsb 230 40 8
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- バーコードを計算し直す。穴が増えたら音を鳴らす
recompute :: World -> IO World
recompute w = do
  let bars = if length (wPts w) > 1 then persistence (wPts w) maxR else []
      holes = length [b | b <- bars, barDim b == 1, barLife maxR b > 8]
  when (holes > wLastHoles w) $ playTone (440 + fromIntegral holes * 110) 0.3 0.6 Triangle  -- ★ 穴が増えたら音
  pure w { wBars = bars, wLastHoles = holes }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ -- 帯（r）
    Color (hsb 0 0 20) (rectTL 20 80 (envW env * 0.5) 14)
  , Color (hsb 45 90 100) (rectTL 20 80 (envW env * 0.5 * r / maxR) 14)
  , drawComplex env pts r 0.2 0.6                                              -- ★ 三角形と線の色は drawComplex の中
  , Color (hsba 0 0 100 0.05) (Pictures [at (toG env x y) (disc r) | (x, y) <- pts])
  , Color (hsb 45 80 100) (Pictures [at (toG env x y) (disc 4) | (x, y) <- pts])  -- ★ 点の見た目
  , if null (wBars w) then Blank else drawBarcode env (wBars w) bx 120 bw (envH env - 220) maxR (Just r)
  , txt env 20 40 18 (hsb 0 0 100)
      ("Click: add point (" ++ show (length pts) ++ ")  r = " ++ show (round r :: Int) ++ " (top bar)  Blobs " ++ show b0 ++ ", holes " ++ show b1 ++ "  C: clear  S: save")
  , txt env 20 64 15 (hsb 0 0 60)
      "Goals: make 3 holes / a hole that survives as r changes / 2 holes born at the same moment"
  ]
  where
    pts = wPts w
    r = wR w
    bx = envW env * 0.58
    bw = envW env - bx - 50
    (b0, b1) = if null (wBars w) then (length pts, 0) else bettiAt (wBars w) r
    rectTL x y rw rh = let (cx, cy) = toG env (x + rw / 2) (y + rh / 2) in rect cx cy rw rh

-- 帯の上か（帯は y が 74〜100、x は帯の右端まで）
onBand :: Env -> Bool
onBand env = let (mx, my) = mouseXY env in my > 74 && my < 100 && mx < envW env * 0.5 + 20

bandValue :: Env -> Float
bandValue env = clampF 1 maxR ((fst (mouseXY env) - 20) / (envW env * 0.5) * maxR)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | onBand env -> pure w { wR = bandValue env }
    | snd (mouseXY env) < 100 || length (wPts w) >= maxPoints -> pure w
    | otherwise -> recompute w { wPts = wPts w ++ [mouseXY env] }
  EventMotion _ | envDown env && onBand env -> pure w { wR = bandValue env }
  EventKey (Char c) Down _ _
    | c == 'c' || c == 'C' -> pure w { wPts = [], wBars = [], wLastHoles = 0 }
    | c == 's' || c == 'S' -> saveSvg "tda" env (drawWorld env w) >> pure w
  _ -> pure w

-- ---------- 自由制作のアイデア ----------
-- ・点を置くたびに、全部のバーを和音として鳴らす（10 を参考に playChord で）
-- ・点を少しずつ動かして（ノイズ）、バーコードが「ゆれる」アニメーション（skStep を足す）
-- ・11 のマイクの点群と合体：声で点が置かれる
-- ・三角形の色を「生まれた r」で変える（古い三角形ほど濃く）

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
