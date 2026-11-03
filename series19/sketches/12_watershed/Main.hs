-- ============================================
-- 12 ウォーターシェッド（分水嶺）
-- 画像を「地形」とみなし、谷底（極小）から水をためていく。水面が上がって 2 つの湖がぶつかる所に「堤防」を作る = 分水嶺
-- くっついた塊は、距離変換を裏返した地形（中心が谷）で分けられる
-- スペースで再生／一時停止 / 右で 1 段上げる / 1〜2 で地形（ノイズ・くっついた丸） / ドラッグで丸を足す（2） / R でやり直し / N で別の地形
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_)
import Control.Monad.ST (ST, runST)
import Data.Char (toLower)
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, ViewL(..), viewl, (|>))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import System.Random (mkStdGen, randomRs)

cols, rows :: Int
cols = 80
rows = 56

ox, oy :: Float
ox = 20
oy = 110

levels :: Int
levels = 48    -- ★ 水面の段数

data Minimum = Minimum !Int ![Int]  -- 谷底：高さとマスの一覧

-- 高さごとの順番待ち（高さ → マスの番号の列）
type Buckets = IM.IntMap (Seq Int)

data World = World
  { wH       :: !(U.Vector Int)
  , wLabels  :: !(U.Vector Int)  -- 0 = まだ, -1 = 分水嶺, 1〜 = 湖の番号
  , wBlocked :: !Grid
  , wMinima  :: ![Minimum]
  , wBuckets :: !Buckets
  , wImg     :: !Grid            -- 地形 2 用の二値画像
  , wTerrain :: !Int             -- ★ 1 = ノイズの地形 2 = くっついた丸（距離変換）
  , wLevel   :: !Int
  , wNLabels :: !Int
  , wPlaying :: !Bool
  , wSeed    :: !Int
  }

main :: IO ()
main = runSketch (sketch "19-12 Watershed" (makeTerrain world0))
  { skBack  = hsb 170 45 9
  , skStep  = \env _ w -> pure (if wPlaying w && envFrame env `mod` 4 == 0 then floodStep w else w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = World zeros zeros none [] IM.empty none 2 (-1) 0 True 1
  where zeros = U.replicate (cols * rows) 0
        none = U.replicate (cols * rows) False

makeTerrain :: World -> World
makeTerrain w
  | wTerrain w == 1 =
      let sd = fi (wSeed w) * 23.7
          -- ライブラリのノイズは振れ幅が小さいので、p5.js 版と同じくらいに広げる
          n x y = clampF 0 1 (0.5 + 2 * (fbm2 4 (fi x * 0.06 + sd) (fi y * 0.06) - 0.5))
          hs = U.generate (cols * rows) (\i -> floor (n (i `rem` cols) (i `quot` cols) * fi (levels - 1)))
      in resetFlood w { wH = hs, wBlocked = U.map (const False) (wImg w) }
  | otherwise =
      let us = randomRs (0, 1 :: Float) (mkStdGen (wSeed w))
          circle3 k = case take 3 (drop (3 * k) us) of
            [a, b, c] -> (12 + a * (fi cols - 24), 10 + b * (fi rows - 20), 7 + c * 5)
            _ -> (0, 0, 0)
          cs = map circle3 [0 .. 5]
      in terrainFromImage w { wImg = gridFrom (\x y -> any (\(cx, cy, r) -> distI x y cx cy <= r) cs) }

-- ★ 二値画像 → 地形：距離変換を裏返す（中心ほど深い谷）。背景は水が入らない「岩」
terrainFromImage :: World -> World
terrainFromImage w = resetFlood w { wBlocked = U.map not img, wH = U.imap height d }
  where
    img = wImg w
    d = U.map sqrt (edtSquared (\i -> not (img U.! i)))
    maxD = U.foldl' max 1 (U.zipWith (\v x -> if v then x else 0) img d)
    height i x
      | img U.! i = floor ((1 - x / maxD) * fromIntegral (levels - 1))
      | otherwise = levels

-- ★ 谷底（極小）を探す：まわり（8 近傍）に自分より低いマスがない「平らな所」
findMinima :: U.Vector Int -> Grid -> [Minimum]
findMinima h blocked = runST $ do
  seen <- MU.replicate (cols * rows) False
  let walk [] cells lower = pure (reverse cells, lower)
      walk (i : stack) cells lower = do
        (stack', cells', lower') <- foldM (visit i) (stack, cells, lower) (neighbors8 i)
        walk stack' cells' lower'
      visit i (stack, cells, lower) j
        | blocked U.! j = pure (stack, cells, lower)
        | h U.! j < h U.! i = pure (stack, cells, True)
        | h U.! j == h U.! i = do
            s <- MU.read seen j
            if s then pure (stack, cells, lower) else do
              MU.write seen j True
              pure (j : stack, j : cells, lower)
        | otherwise = pure (stack, cells, lower)
      start acc s0 = do
        s <- MU.read seen s0
        if s || blocked U.! s0 then pure acc else do
          MU.write seen s0 True
          (cells, lower) <- walk [s0] [s0] False
          pure (if lower then acc else Minimum (h U.! s0) cells : acc)
  reverse <$> foldM start [] [0 .. cols * rows - 1]

resetFlood :: World -> World
resetFlood w = w
  { wLabels = U.replicate (cols * rows) 0, wLevel = -1, wNLabels = 0
  , wBuckets = IM.empty, wMinima = findMinima (wH w) (wBlocked w) }

-- となりの未処理のマスを「高さの順番待ち」に入れる（水面より低ければ今すぐ）
pushNeighbors :: World -> Int -> MU.MVector s Int -> Buckets -> Int -> ST s Buckets
pushNeighbors w level labels b i = foldM push b (neighbors4 i)
  where
    push acc j = do
      l <- MU.read labels j
      pure $ if l == 0 && not (wBlocked w U.! j)
        then IM.insertWith (\_ old -> old |> j) (max (wH w U.! j) level) (Seq.singleton j) acc
        else acc

-- ★ 水面を 1 段上げる（Meyer の洪水アルゴリズム：低いマスから順に、となりの湖の色をもらう）
floodStep :: World -> World
floodStep w
  | wLevel w >= levels - 1 = w
  | otherwise = runST $ do
      labels <- U.thaw (wLabels w)
      let level = wLevel w + 1
          -- (1) 水面の高さに谷底があれば、新しい湖が生まれる
          birth (n, b) (Minimum mh cells)
            | mh /= level = pure (n, b)
            | otherwise = do
                forM_ cells $ \i -> MU.write labels i (n + 1)
                b' <- foldM (pushNeighbors w level labels) b cells
                pure (n + 1, b')
          -- (2) 水面以下のマスを順番に処理：となりが 1 色なら同じ湖、2 色以上なら分水嶺
          drain b = case viewl (IM.findWithDefault Seq.empty level b) of
            EmptyL -> pure b
            i :< rest -> do
              let b1 = IM.insert level rest b
              l0 <- MU.read labels i
              if l0 /= 0 then drain b1 else do
                (found, conflict, wshed) <- foldM (look labels) (0, False, False) (neighbors4 i)
                let l | conflict = -1
                      | found /= 0 = found
                      | wshed = -1
                      | otherwise = 0
                MU.write labels i l
                b2 <- if l /= 0 then pushNeighbors w level labels b1 i else pure b1
                drain b2
      (n, b1) <- foldM birth (wNLabels w, wBuckets w) (wMinima w)
      b2 <- drain b1
      final <- U.freeze labels
      pure w { wLabels = final, wLevel = level, wNLabels = n, wBuckets = IM.delete level b2 }
  where
    look labels (found, conflict, wshed) j = do
      l <- MU.read labels j
      pure $ if l > 0 then (l, conflict || (found /= 0 && l /= found), wshed)
             else if l == -1 then (found, conflict, True)
             else (found, conflict, wshed)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures $
  [ imagePic env c ox oy cellColor
  , txt env px (ty + 20) 34 (hsb 45 90 100) ("Level " ++ show lv ++ " / " ++ show (levels - 1))
  , txt env px (ty + 54) 15 (lum 100) ("Lakes: " ++ show (wNLabels w) ++ " / minima " ++ show (length (wMinima w)))
  , txt env px (ty + 78) 15 (lum 100) ("Watershed cells: " ++ show nWshed)
  -- 水面のゲージ
  , box env (lum 25) px (ty + 96) 240 12
  , box env (hsb 200 80 100) px (ty + 96) (240 * (fi lv / fi (levels - 1))) 12
  , note 140 "Gray = height (white is high)"
  , note 164 "Color = lake (which minimum)"
  , note 188 "Yellow = watershed (lakes meet)"
  ]
  ++ (if wTerrain w == 2
        then [ note 228 "Terrain = inverted distance map:"
             , note 252 "centers = valleys, necks = passes"
             , note 276 "-> touching disks split at necks"
             , note 300 "(the classic way to split cells)" ]
        else [ note 228 "One lake per minimum"
             , note 252 "Noise: many valleys, over-split"
             , note 276 "-> pick valleys with markers (11)"
             , note 300 "   or use the gradient (08)" ])
  ++ [ txt env 20 40 16 (lum 100)
         ("Space: " ++ (if wPlaying w then "pause" else "play") ++ "   Right: raise 1 level   1-2: terrain ("
          ++ (if wTerrain w == 1 then "noise" else "touching disks") ++ ")   Drag: add disk (2)   R: restart   N: new terrain")
     , txt env 20 68 14 (lum 60)
         "The water rises step by step. A valley fills: a new lake. Two lakes meet: a dam (watershed). The dams left are the borders."
     ]
  where
    c = cellOf env
    lv = max (wLevel w) 0
    labels = wLabels w
    h = wH w
    nWshed = U.length (U.filter (== (-1)) labels)
    cellColor x y
      | wBlocked w U.! i = rockRGB
      | l == -1 = damRGB
      | l > 0 = rgbOf (hsb (fi ((l * 53) `mod` 360)) 70 (55 + hl * 40))
      | otherwise = rgbOf (lum (20 + hl * 70))
      where i = idx x y
            l = labels U.! i
            hl = fi (h U.! i) / fi levels
    px = ox + fi cols * c + 40
    ty = oy + 10
    note dy s = txt env px (ty + dy) 14 (lum 60) s

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> paint env w
  EventMotion _ | envDown env && envFrame env `mod` 4 == 0 -> paint env w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> floodStep w { wPlaying = False }
  EventKey (Char ch) Down _ _ -> case toLower ch of
    '1' -> makeTerrain w { wTerrain = 1 }
    '2' -> makeTerrain w { wTerrain = 2 }
    'r' -> resetFlood w
    'n' -> makeTerrain w { wSeed = wSeed w + 1 }
    _   -> w
  _ -> w

paint :: Env -> World -> World
paint env w
  | wTerrain w /= 2 || not (inGrid gx gy) = w
  | otherwise = terrainFromImage w { wImg = U.imap addDisk (wImg w) }
  where
    (gx, gy) = cellAt env (cellOf env) ox oy
    addDisk i v = v || distI (i `rem` cols) (i `quot` cols) (fi gx) (fi gy) <= 6  -- ★ 足す丸の半径

-- ---------- やってみよう ----------
-- 1. 地形 2 で、丸を 2 つ重ねて描く。分水嶺はくびれの位置に出る？ 重なりが大きいと分かれない？
-- 2. 地形 1 で湖の数を数える。ノイズの 0.06 を 0.12 にすると湖はどうなる？（過分割）
-- 3. 08 の勾配画像を地形にしてみる（h = 勾配 × (levels−1)）。物体の輪郭で堤防ができる

-- ==================================================
-- ここから下は「道具」（座標、描画、近傍、ユークリッド距離変換）
-- （中身は読まなくても OK）
-- ==================================================
cellOf :: Env -> Float
cellOf env = min ((envW env - 400) / fi cols) ((envH env - 140) / fi rows)

rockRGB, damRGB :: (Float, Float, Float)
rockRGB = rgbOf (hsb 170 30 14)
damRGB = rgbOf (hsb 45 95 100)

-- マス i のとなり（画面の外は除く）
neighbors4, neighbors8 :: Int -> [Int]
neighbors4 = neighborsOf [(1, 0), (-1, 0), (0, 1), (0, -1)]
neighbors8 = neighborsOf [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)]

neighborsOf :: [(Int, Int)] -> Int -> [Int]
neighborsOf ds i =
  [ idx nx ny | (dx, dy) <- ds
  , let nx = i `rem` cols + dx, let ny = i `quot` cols + dy, inGrid nx ny ]

-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

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

-- 二値画像：長さ cols × rows の Bool の並び（True = 前景）
type Grid = U.Vector Bool

idx :: Int -> Int -> Int
idx x y = x + y * cols

gridFrom :: (Int -> Int -> Bool) -> Grid
gridFrom f = U.generate (cols * rows) (\i -> f (i `rem` cols) (i `quot` cols))

-- (x, y) と (cx, cy) の距離（p5.js の dist）
distI :: Int -> Int -> Float -> Float -> Float
distI x y cx cy = dist2 (fi x, fi y) (cx, cy)

-- マウスのいるマスの番号
cellAt :: Env -> Float -> Float -> Float -> (Int, Int)
cellAt env c x0 y0 =
  let (mx, my) = mouseXY env in (floor ((mx - x0) / c), floor ((my - y0) / c))

inGrid :: Int -> Int -> Bool
inGrid x y = x >= 0 && y >= 0 && x < cols && y < rows

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
