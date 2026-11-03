-- ============================================
-- 09 画像のフィルトレーション（白黒の「しきい値」を動かす）
-- 絵を描いて、しきい値を動かすと、暗い部分だけが「ON」になる
-- しきい値を変えながら、ON の塊の数（β0）と穴の数（β1）を数える = 画像の TDA
-- ドラッグで描く（ぼんやりした筆） / マウス左右（上の帯）= しきい値 / C で消す / 1〜2 でお手本
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newListArray, readArray, writeArray)
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

data World = World
  { wImg       :: !(U.Vector Float)   -- 0〜1 の濃さ
  , wThreshold :: !Float
  , wCurve0    :: ![Int]              -- しきい値ごとの β0
  , wCurve1    :: ![Int]              -- しきい値ごとの β1
  }

cols, rows :: Int
cols = 48           -- ★ 画像の横のマス数
rows = 30           -- ★ 画像の縦のマス数

initialWorld :: World
initialWorld = World (preset 1) 0.5 [] []

main :: IO ()
main = runSketch (sketch "09-09 Image Filtration" initialWorld)
  { skBack  = hsb 230 40 10
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- お手本の画像
preset :: Int -> U.Vector Float
preset k = U.generate (cols * rows) $ \i ->
  let (y, x) = i `divMod` cols
      fx = fromIntegral x
      fy = fromIntegral y
      ring cx cy = clampF 0 1 (1 - abs (dist2 (fx, fy) (cx, cy) - 8) / 4)
  in if k == 1
       then max (ring 15 15) (ring 33 15)     -- 2 つのリング（穴 2 つ）
       else fbm2 4 (fx * 0.12) (fy * 0.12)    -- ノイズ画像

-- ★ しきい値で 2 値化して、β0（8 近傍の塊）と β1（オイラー標数から）を数える
betti :: U.Vector Float -> Float -> (Int, Int, Int)
betti img th = (b0, b0 - chi, f)
  where
    on x y = x >= 0 && y >= 0 && x < cols && y < rows && img U.! (x + y * cols) >= th
    -- ON の画素を正方形とみなして、面・辺・角を数える（chi = V − E + F）
    f = count [on x y | y <- [0 .. rows - 1], x <- [0 .. cols - 1]]
    v = count [on (x - 1) (y - 1) || on x (y - 1) || on (x - 1) y || on x y | y <- [0 .. rows], x <- [0 .. cols]]
    eh = count [on x (y - 1) || on x y | y <- [0 .. rows], x <- [0 .. cols - 1]]
    ev = count [on (x - 1) y || on x y | y <- [0 .. rows - 1], x <- [0 .. cols]]
    chi = v - (eh + ev) + f
    b0 = countBlobs on
    count = length . filter id

-- つながった ON の塊の数（union-find）
countBlobs :: (Int -> Int -> Bool) -> Int
countBlobs on = runST $ do
  parent <- newListArray (0, cols * rows - 1) [0 .. cols * rows - 1] :: ST s (STUArray s Int Int)
  let find a = do
        p <- readArray parent a
        if p == a then pure a else do
          r <- find p
          writeArray parent a r
          pure r
  forM_ [(x, y) | y <- [0 .. rows - 1], x <- [0 .. cols - 1], on x y] $ \(x, y) ->
    forM_ [(1, 0), (0, 1), (1, 1), (-1, 1)] $ \(dx, dy) ->
      when (on (x + dx) (y + dy)) $ do
        a <- find (x + y * cols)
        b <- find (x + dx + (y + dy) * cols)
        writeArray parent a b
  roots <- mapM find [x + y * cols | y <- [0 .. rows - 1], x <- [0 .. cols - 1], on x y]
  pure (IS.size (IS.fromList roots))

-- 全部のしきい値での β0, β1（20 フレームに 1 回）
stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | envFrame env `mod` 20 == 1 =
      let bs = [betti (wImg w) (fromIntegral t * 0.05) | t <- [0 .. 20 :: Int]]
      in pure w { wCurve0 = [b | (b, _, _) <- bs], wCurve1 = [b | (_, b, _) <- bs] }
  | otherwise = pure w

cellSize :: Env -> Float
cellSize env = min ((envW env - 300) / fromIntegral cols) ((envH env - 160) / fromIntegral rows)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ -- 帯（しきい値）
    Color (hsb 0 0 20) (rectTL ox 80 iw 14)
  , Color (hsb 45 90 100) (rectTL ox 80 (iw * th) 14)
  , Pictures [ cellPic (i `mod` cols) (i `div` cols) v | (i, v) <- zip [0 ..] (U.toList (wImg w)) ]
  -- 右：ベッチ数の曲線（しきい値を 0〜1 で走査）
  , Color (hsb 0 0 30) (Line [g rx (ry + rh), g (rx + rw) (ry + rh)])
  , curvePic (wCurve0 w) 200
  , curvePic (wCurve1 w) 20
  , Color (hsb 45 90 100) (thickLine 2 (g (rx + th * rw) ry) (g (rx + th * rw) (ry + rh)))
  , txt env rx (ry - 30) 14 (hsb 200 80 100) "# b0 blobs"
  , txt env rx (ry - 10) 14 (hsb 20 80 100) "# b1 holes"
  , txt env rx (ry + rh + 22) 14 (hsb 0 0 60) "Threshold 0 -> 1"
  , txt env 20 40 18 (hsb 0 0 100)
      ("Threshold " ++ nf 2 th ++ " (top bar)  ON " ++ show area ++ "  b0 = " ++ show b0 ++ "  b1 = " ++ show b1 ++ "  Drag: paint  C: clear  1-2: presets")
  , txt env 20 68 15 (hsb 0 0 60)
      "ON = darkness >= threshold. Blobs appear and merge, holes open and fill. Right: b0, b1 at all thresholds."
  ]
  where
    g = toG env
    th = wThreshold w
    (b0, b1, area) = betti (wImg w) th
    cell = cellSize env
    ox = 20
    oy = 110
    iw = fromIntegral cols * cell
    rx = ox + iw + 40
    rw = envW env - rx - 30
    ry = 130
    rh = envH env - 240
    rectTL x y rectW rectH = let (cx, cy) = g (x + rectW / 2) (y + rectH / 2) in rect cx cy rectW rectH
    cellPic x y v = Color
      (if v >= th then hsb 45 90 100 else hsb 230 30 (15 + v * 40))
      (rectTL (ox + fromIntegral x * cell) (oy + fromIntegral y * cell) (cell - 1) (cell - 1))
    curvePic curve hue =
      let lastK = fromIntegral (max 1 (length curve - 1)) :: Float
      in Color (hsb hue 80 100) (thickPath 2.5
           [ g (rx + k / lastK * rw) (ry + rh - fromIntegral (min c 20) * (rh / 20))
           | (k, c) <- zip [0 ..] curve ])

-- 帯の上ならしきい値を変える。それ以外は、ぼんやりした筆で描く
paint :: Env -> World -> World
paint env w
  | my > 74 && my < 100 = w { wThreshold = clampF 0 1 ((mx - 20) / (fromIntegral cols * cell)) }
  | otherwise = w { wImg = U.imap brush (wImg w) }
  where
    (mx, my) = mouseXY env
    cell = cellSize env
    gx = (mx - 20) / cell
    gy = (my - 110) / cell
    brush i v =
      let (y, x) = i `divMod` cols
          d = dist2 (fromIntegral x + 0.5, fromIntegral y + 0.5) (gx, gy)
      in if d < 3 then min 1 (v + (1 - d / 3) * 0.25) else v

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (paint env w)
  EventMotion _ | envDown env -> pure (paint env w)
  EventKey (Char c) Down _ _
    | c == 'c' || c == 'C' -> pure w { wImg = U.replicate (cols * rows) 0 }
    | c == '1' -> pure w { wImg = preset 1 }
    | c == '2' -> pure w { wImg = preset 2 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. お手本 1 で、しきい値を動かす。穴が 2 つある範囲は？ 高いしきい値で輪が切れると穴は消える
-- 2. 自分で「ドーナツ」を描いて穴を作る。穴が消えるのはしきい値がどのくらいのとき？
-- 3. お手本 2（ノイズ画像）は、しきい値 0.5 付近で塊と穴が激しく変わる（パーコレーション）

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

-- 小数を d 桁で文字にする（p5.js の nf）
nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
