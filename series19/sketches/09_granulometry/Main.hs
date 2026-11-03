-- ============================================
-- 09 粒度分析（granulometry）
-- 半径 k の円でオープニングすると、半径 k より小さい粒は消える。k を 0, 1, 2, … と増やしながら面積を測る
-- 「k で消えた面積」のグラフ = パターンスペクトル。粒の大きさの分布が、粒を 1 つも数えずにわかる
-- 左右で k / 上下で粒の数 / 1〜3 で粒の大きさの分布 / スペースで新しい粒
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Control.Monad.ST (runST)
import Data.Char (toLower)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import System.Random (mkStdGen, randomRs)

cols, rows :: Int
cols = 96
rows = 64

ox, oy :: Float
ox = 20
oy = 110

kMax :: Int
kMax = 12    -- ★ 調べる最大半径

data World = World
  { wImg    :: !Grid
  , wGrains :: !Int     -- ★ 粒の数
  , wDist   :: !Int     -- ★ 1 = いろいろ 2 = 2 種類 3 = 1 種類
  , wK      :: !Int
  , wSeed   :: !Int
  , wOpened :: !(V.Vector Grid)    -- 半径 0..kMax でオープニングした画像
  , wAreas  :: !(U.Vector Int)     -- その面積
  , wRadii  :: ![Int]              -- 置いた粒の半径（答え合わせ用）
  }

main :: IO ()
main = runSketch (sketch "19-09 Granulometry" (makeGrains world0))
  { skBack  = hsb 170 45 9
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = World e 16 1 0 1 (V.singleton e) (U.singleton 0) []
  where e = U.replicate (cols * rows) False

-- 乱数の列の先頭を取り出す
next :: [Float] -> (Float, [Float])
next (u : us) = (u, us)
next [] = (0.5, [])

-- random(a, b)
range :: Float -> Float -> [Float] -> (Float, [Float])
range a b us = let (u, rest) = next us in (a + (b - a) * u, rest)

-- ★ 粒（円）をランダムに置く（重ならない場所を探す）
makeGrains :: World -> World
makeGrains w = analyze w { wImg = img, wRadii = map (\(_, _, r) -> r) placed }
  where
    placed = go (wGrains w) (randomRs (0, 1) (mkStdGen (wSeed w))) []
    go :: Int -> [Float] -> [(Float, Float, Int)] -> [(Float, Float, Int)]
    go 0 _ acc = reverse acc
    go n us acc =
      let (r, us1) = pickRadius (wDist w) us
          (found, us2) = tryPlace (40 :: Int) r us1 acc
      in go (n - 1) us2 (maybe acc (: acc) found)
    tryPlace 0 _ us _ = (Nothing, us)
    tryPlace t r us acc =
      let rf = fi r
          (cx, us1) = range (rf + 1) (fi cols - rf - 1) us
          (cy, us2) = range (rf + 1) (fi rows - rf - 1) us1
          clash = any (\(px, py, pr) -> dist2 (px, py) (cx, cy) < fi pr + rf + 2) acc
      in if clash then tryPlace (t - 1) r us2 acc else (Just (cx, cy, r), us2)
    img = gridFrom (\x y -> any (\(cx, cy, r) -> distI x y cx cy <= fi r + 0.3) placed)

pickRadius :: Int -> [Float] -> (Int, [Float])
pickRadius kind us = case kind of
  1 -> let (v, rest) = range 2 9 us in (floor v, rest)
  2 -> let (u, rest) = next us in (if u < 0.5 then 3 else 7, rest)
  _ -> (5, us)

-- ★ 半径 0..kMax の円でオープニングして面積を記録
analyze :: World -> World
analyze w = w { wOpened = opened, wAreas = U.fromList (map areaOf (V.toList opened)), wK = min (wK w) kMax }
  where opened = V.generate (kMax + 1) (openDisk (wImg w))

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures $
  [ cellsPic env c ox oy panelC cellColor
  -- 右：パターンスペクトル（k で消えた面積）
  , txt env px (py - 30) 15 (lum 100) "Pattern spectrum: area lost at k"
  , txt env px (py - 12) 14 (lum 60) "PS(k) = area(k-1) - area(k)"
  , Color (lum 30) (Line [pt env px (py + ph), pt env (px + pw) (py + ph)])
  ]
  ++ concat
     [ [ box env (if r == k then hsb 350 85 100 else hsb 150 60 80) (px + fi r * bw + 2) (py + ph - h) (bw - 4) h
       , txt env (px + fi r * bw + bw / 2 - (if r >= 10 then 9 else 4)) (py + ph + 18) 14 (lum 60) (show r) ]
       ++ [ txt env (px + fi r * bw + bw / 2 - 4) (py + ph - h - 6) 14 (lum 100) (show cnt) | cnt > 0 ]
     | r <- [1 .. kMax], let h = barH r, let cnt = hist r ]
  ++ [ txt env (px + pw - 80) (py + ph + 38) 14 (lum 60) "radius k ->"
     -- 面積の曲線
     , txt env px (qy - 10) 14 (lum 100)
         ("Area " ++ show (areaAt k) ++ " / " ++ show (areaAt 0) ++ "   Grains: " ++ show nPlaced)
     , Color (lum 30) (Line [pt env px qy, pt env (px + pw) qy, pt env (px + pw) (qy + qh), pt env px (qy + qh), pt env px qy])
     , Color (hsb 45 90 100) (thickPath 2.5 [uncurry (pt env) (curve r) | r <- [0 .. kMax]])
     , Color (hsb 350 85 100) (at (uncurry (pt env) (curve k)) (disc 5))
     , txt env px (qy + qh + 20) 14 (lum 60) "White numbers: real grain counts"
     , txt env 20 40 16 (lum 100)
         ("Left/Right: k = " ++ show k ++ " (red = lost at this k)   Up/Down: " ++ show nPlaced ++ " grains   1-3: sizes (" ++ distName (wDist w) ++ ")   Space: new grains")
     , txt env 20 68 14 (lum 60)
         "A sieve of size k drops small grains first. Graphing what falls gives the size distribution (powders, cells, bubbles)."
     ]
  where
    c = cellOf env
    k = wK w
    img = wImg w
    opened = wOpened w
    cur = opened V.! min k (V.length opened - 1)
    prev = if k > 0 then opened V.! min (k - 1) (V.length opened - 1) else img
    cellColor x y
      | not (img U.! i) = offRGB
      | cur U.! i = onRGB
      | prev U.! i = goneRGB      -- この k で消えた粒
      | otherwise = oldRGB        -- もっと前に消えた粒
      where i = idx x y
    areaAt r = wAreas w U.! max 0 (min r (U.length (wAreas w) - 1))
    px = ox + fi cols * c + 50
    pw = envW env - px - 30
    py = oy + 60
    ph = (envH env - 200) * 0.5
    bw = pw / fi (kMax + 1)
    -- デジタルの円は厳密には「ふるい」の条件を満たさないので、ごくまれに負になる → 0 とみなす
    spectrum r = max 0 (areaAt (r - 1) - areaAt r)
    maxPS = maximum (1 : map spectrum [1 .. kMax])
    barH r = (fi (spectrum r) / fi maxPS) * (ph - 10)
    qy = py + ph + 70
    qh = envH env - qy - 40
    curve r = (px + fi r * bw + bw / 2, qy + qh - (fi (areaAt r) / fi (max (areaAt 0) 1)) * (qh - 6) - 3)
    -- 答え合わせ：本当にその半径の粒がいくつあるか
    hist r = length (filter (\v -> min v kMax == r) (wRadii w))
    nPlaced = length (wRadii w)

distName :: Int -> String
distName d = case d of
  1 -> "various, r 2-8"
  2 -> "two, r 3 and 7"
  _ -> "one, r 5"

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wK = max 0 (wK w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wK = min kMax (wK w + 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> makeGrains w { wGrains = min 60 (wGrains w + 4) }
  EventKey (SpecialKey KeyDown) Down _ _ -> makeGrains w { wGrains = max 4 (wGrains w - 4) }
  EventKey (SpecialKey KeySpace) Down _ _ -> makeGrains w { wSeed = wSeed w + 1 }
  EventKey (Char ch) Down _ _ -> case toLower ch of
    '1' -> makeGrains w { wDist = 1 }
    '2' -> makeGrains w { wDist = 2 }
    '3' -> makeGrains w { wDist = 3 }
    _   -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 分布 2（半径 3 と 7）で、スペクトルの山が 2 つ出るか。山の位置は半径と合っている？
-- 2. 粒を増やして重なりを作る。重なった粒は「大きな粒」に見える → スペクトルはどうずれる？
-- 3. 正方形 SE 版を作る（openDisk のかわりに正方形の SE で収縮→膨張）。円の粒でも同じ結果になる？

-- ==================================================
-- ここから下は「道具」（座標、描画、距離変換で円のオープニングを速く計算）
-- （中身は読まなくても OK）
-- ==================================================
cellOf :: Env -> Float
cellOf env = min ((envW env - 420) / fi cols) ((envH env - 140) / fi rows)

panelC :: Color
panelC = hsb 170 30 16

onRGB, offRGB, goneRGB, oldRGB :: (Float, Float, Float)
onRGB = rgbOf (hsb 150 60 95)
offRGB = rgbOf panelC
goneRGB = rgbOf (hsb 350 85 100)
oldRGB = rgbOf (hsb 150 30 28)

-- 半径 r の円でオープニング：収縮 = 「背景までの距離 > r」、膨張 = 「収縮後の形までの距離 ≤ r」
openDisk :: Grid -> Int -> Grid
openDisk a r
  | r == 0 = a
  | otherwise = U.map (<= lim) dout
  where
    lim = fromIntegral (r * r) + fromIntegral r * 0.5 :: Double  -- SE は makeSE の円と同じ
    din = edtSquared (\i -> not (a U.! i))
    er = U.zipWith (\v d -> v && d > lim) a din                  -- 収縮
    dout = edtSquared (\i -> er U.! i)                           -- 膨張

-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- 文字（左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

-- 塗りつぶした長方形（左上が (x, y)。p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0

fi :: Int -> Float
fi = fromIntegral

-- 色を pixelImageF 用の (赤, 緑, 青) にする
rgbOf :: Color -> (Float, Float, Float)
rgbOf c = let (r, g, b, _) = rgbaOfColor c in (r, g, b)

-- マス目全体を 1 枚の画像にする。f x y がマスの色。1 マスずつ長方形を描くより速い
imagePic :: Env -> Float -> Float -> Float -> (Int -> Int -> (Float, Float, Float)) -> Picture
imagePic env c x0 y0 f =
  at (pt env (x0 + fi cols * c / 2) (y0 + fi rows * c / 2)) (pixelImageF cols rows c f)

-- 1 ピクセルのすき間のあるマス目（p5.js の rect(x, y, cell - 1, cell - 1)）。
-- 画像の上に、すき間の色の線を重ねて描く
cellsPic :: Env -> Float -> Float -> Float -> Color -> (Int -> Int -> (Float, Float, Float)) -> Picture
cellsPic env c x0 y0 gapC f = Pictures
  [ imagePic env c x0 y0 f
  , Color gapC (Pictures
      ( [Line [pt env (x0 + fi i * c - 0.5) y0, pt env (x0 + fi i * c - 0.5) (y0 + fi rows * c)] | i <- [1 .. cols - 1]]
     ++ [Line [pt env x0 (y0 + fi j * c - 0.5), pt env (x0 + fi cols * c) (y0 + fi j * c - 0.5)] | j <- [1 .. rows - 1]]))
  ]

-- 二値画像：長さ cols × rows の Bool の並び（True = 前景）
type Grid = U.Vector Bool

idx :: Int -> Int -> Int
idx x y = x + y * cols

gridFrom :: (Int -> Int -> Bool) -> Grid
gridFrom f = U.generate (cols * rows) (\i -> f (i `rem` cols) (i `quot` cols))

-- 前景のマスの数
areaOf :: Grid -> Int
areaOf = U.length . U.filter id

-- (x, y) と (cx, cy) の距離（p5.js の dist）
distI :: Int -> Int -> Float -> Float -> Float
distI x y cx cy = dist2 (fi x, fi y) (cx, cy)

-- 2 乗ユークリッド距離変換（Felzenszwalb & Huttenlocher）：
-- isSite i が True のマスまでの距離²。縦に 1 回、横に 1 回、1 次元の処理をする
edtSquared :: (Int -> Bool) -> U.Vector Double
edtSquared isSite =
  U.concat [ edt1d (U.generate cols (\x -> byCol U.! (x * rows + y))) | y <- [0 .. rows - 1] ]
  where
    big = 1e9
    f = U.generate (cols * rows) (\i -> if isSite i then 0 else big)
    byCol = U.concat [ edt1d (U.generate rows (\y -> f U.! (x + y * cols))) | x <- [0 .. cols - 1] ]

-- 1 次元：放物線の下側包絡線
edt1d :: U.Vector Double -> U.Vector Double
edt1d f = runST $ do
  let n = U.length f
      sq :: Int -> Double
      sq q = fromIntegral (q * q)
      cross q p = ((f U.! q + sq q) - (f U.! p + sq p)) / fromIntegral (2 * q - 2 * p)
  v <- MU.replicate n (0 :: Int)
  z <- MU.replicate (n + 1) (0 :: Double)
  MU.write z 0 (-1e20)
  MU.write z 1 1e20
  let insert k q = do
        p <- MU.read v k
        zk <- MU.read z k
        let s = cross q p
        if s <= zk then insert (k - 1) q else do
          MU.write v (k + 1) q
          MU.write z (k + 1) s
          MU.write z (k + 2) 1e20
          pure (k + 1)
  _ <- foldM insert 0 [1 .. n - 1]
  d <- MU.replicate n (0 :: Double)
  let query k q
        | q >= n = pure ()
        | otherwise = do
            zn <- MU.read z (k + 1)
            if zn < fromIntegral q then query (k + 1) q else do
              p <- MU.read v k
              MU.write d q (sq (q - p) + f U.! p)
              query k (q + 1)
  query 0 0
  U.freeze d
