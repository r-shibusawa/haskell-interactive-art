-- ============================================
-- 07 点描画（スティップリング）─ 重みつき重心ボロノイ
-- 06 のロイド緩和で、重心を「暗さで重みをつけて」計算する → 暗い所に点が集まり、点の間隔がそろった点描画になる
--   各画素を最寄りの点に割り当て（ボロノイ）、暗さ w を重みにした重心 Σ(w·x) / Σw へ点を動かす
-- スペース = 緩和 ON/OFF / 1〜3 = 絵 / ←→ = 点の数 / V = 元の絵 / D = 点の大きさを暗さで変える / R = やり直し
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import System.Random (StdGen, newStdGen, randomR)
import Text.Printf (printf)

res :: Int
res = 3                   -- ★ 計算に使う画素のまとめ数

-- 元の絵の大きさ（画面の px）と、計算のマス目の数
picW, picH :: Int
picW = round width - 40
picH = round height - 120

gw, gh :: Int
gw = picW `div` res
gh = picH `div` res

-- 絵の左上の位置
ox, oy :: Float
ox = 20
oy = 100

data World = World
  { wN       :: !Int      -- ★ 点の数
  , wXs      :: !(U.Vector Float)   -- 点の位置（マス目の単位）
  , wYs      :: !(U.Vector Float)
  , wDens    :: !(U.Vector Float)   -- 各画素の暗さ 0〜1
  , wTotal   :: !Float
  , wRun     :: !Bool
  , wPicture :: !Int
  , wShowSrc :: !Bool
  , wSize    :: !Bool     -- 点の大きさを暗さで変える
  , wIter    :: !Int
  , wMoved   :: !Float
  , wSrcImg  :: !Picture  -- V で見せる元の絵
  }

main :: IO ()
main = runSketch (sketch "22-07 Stippling" world0)
  { skBack  = hsb 40 8 96
  , skStart = \_ -> makePicture world0
  , skStep  = \_ _ w -> pure (if wRun w then stippleStep w else w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World 2500 U.empty U.empty U.empty 1 True 1 False True 0 0 Blank

-- ★ 元の絵の明るさ 0〜255（画像ファイルなしでグラデーション・図形）
srcGray :: Int -> Float -> Float -> Float
srcGray picture x y
  | picture == 1 =
      -- 左から右へ暗くなる。その上に白い丸と黒い丸
      if inCircle (w * 0.7) (h * 0.5) (h * 0.275) then 255
      else if inCircle (w * 0.25) (h * 0.5) (h * 0.2) then 40
      else 255 - 230 * (fromIntegral (floor (x / 4) :: Int) * 4) / w
  | inCircle (w * 0.3) (h * 0.5) rMax =
      -- 球のような影：外から 3 px ずつ小さな円を重ねた色
      let r = rMax - 3 * fromIntegral (floor ((rMax - dist2 (x, y) (w * 0.3, h * 0.5)) / 3) :: Int)
      in 255 * r / rMax
  | x >= w * 0.58 && x < w * 0.73 && y >= h * 0.15 && y < h * 0.85 = 60
  | inTriangle = 150
  | otherwise = 255
  where
    w = fromIntegral picW
    h = fromIntegral picH
    rMax = h * 0.45
    inCircle cx cy r = (x - cx) * (x - cx) + (y - cy) * (y - cy) < r * r
    -- 頂点 (0.88w, 0.15h)、底辺 y = 0.85h（0.78w 〜 0.98w）の三角形
    inTriangle = y >= h * 0.15 && y <= h * 0.85 && abs (x - w * 0.88) <= w * 0.1 * (y - h * 0.15) / (h * 0.7)

-- 絵 3 は文字。折れ線で書いた文字を太くして、暗い画素を塗る
strokeDens :: U.Vector Float
strokeDens = runST $ do
  d <- MU.replicate (gw * gh) (0 :: Float)
  let fw = fromIntegral picW :: Float
      fh = fromIntegral picH :: Float
      big = fh * 0.62
      small = fh * 0.12
      gray20 = 1 - 20 / 255
  forM_ (textStrokes "p5" (fw * 0.5) (fh * 0.45) big) (\(a, b) -> paintSeg d gray20 (big * 0.15) a b)
  forM_ (textStrokes "VORONOI" (fw * 0.5) (fh * 0.88) small) (\(a, b) -> paintSeg d gray20 (small * 0.16) a b)
  U.freeze d

-- 線分のまわり（太さ t）の画素に暗さ v を書き込む
paintSeg :: MU.MVector s Float -> Float -> Float -> Point -> Point -> ST s ()
paintSeg d v t a@(ax, ay) b@(bx, by) = do
  let r = fromIntegral res :: Float
      g lo = max 0 (floor ((lo - t) / r)) :: Int
      x0 = g (min ax bx)
      y0 = g (min ay by)
      x1 = min (gw - 1) (ceiling ((max ax bx + t) / r))
      y1 = min (gh - 1) (ceiling ((max ay by + t) / r))
      l2 = max 1e-6 ((bx - ax) * (bx - ax) + (by - ay) * (by - ay))
  forM_ [y0 .. y1] $ \gy -> forM_ [x0 .. x1] $ \gx -> do
    let px = fromIntegral (gx * res)
        py = fromIntegral (gy * res)
        s = clampF 0 1 (((px - ax) * (bx - ax) + (py - ay) * (by - ay)) / l2)
    when (dist2 (px, py) (ax + s * (bx - ax), ay + s * (by - ay)) <= t / 2) $
      MU.write d (gx + gy * gw) v

-- 文字列を、中心 (cx, cy)・大きさ size の線分のリストにする
textStrokes :: String -> Float -> Float -> Float -> [(Point, Point)]
textStrokes s cx cy size =
  [ (p a, p b)
  | (i, ch) <- zip [0 :: Int ..] s
  , ln <- glyph ch
  , (a, b) <- zip ln (drop 1 ln)
  , let p (u, v) = (x0 i + u * cellW, cy + (v - 0.5) * size) ]
  where
    cellW = size * 0.72
    x0 i = cx - fromIntegral (length s) * cellW / 2 + fromIntegral i * cellW + cellW * 0.12

-- 文字の形（幅 0.76・高さ 1 の箱に折れ線で書く。y は下向き）
glyph :: Char -> [[Point]]
glyph ch = case ch of
  'p' -> [[(0, 0.32), (0, 1.15)], arc 0.3 0.52 0.3 0.2 (-90) 270]
  '5' -> [[(0.58, 0), (0.08, 0), (0.04, 0.42)], arc 0.3 0.68 0.32 0.32 (-125) 150]
  'V' -> [[(0, 0), (0.33, 1), (0.66, 0)]]
  'O' -> [arc 0.33 0.5 0.33 0.5 0 360]
  'R' -> [[(0, 1), (0, 0), (0.42, 0)], arc 0.42 0.27 0.22 0.27 (-90) 90, [(0.42, 0.54), (0, 0.54)], [(0.3, 0.54), (0.64, 1)]]
  'N' -> [[(0, 1), (0, 0), (0.62, 1), (0.62, 0)]]
  'I' -> [[(0.2, 0), (0.2, 1)]]
  _ -> []
  where
    arc cx cy rx ry a0 a1 =
      [ (cx + rx * cos t, cy + ry * sin t)
      | k <- [0 .. 40 :: Int]
      , let t = (a0 + (a1 - a0) * fromIntegral k / 40) * pi / 180 ]

-- 絵を決めて、暗さの表を作り、点をまき直す
makePicture :: World -> IO World
makePicture w = initDots w { wDens = dens, wTotal = 1 + U.sum dens, wSrcImg = preview }
  where
    pic = wPicture w
    dens
      | pic == 3 = strokeDens
      | otherwise = U.generate (gw * gh) $ \i ->
          let (gy, gx) = i `divMod` gw
          in 1 - srcGray pic (fromIntegral (gx * res)) (fromIntegral (gy * res)) / 255        -- 暗いほど 1
    -- 元の絵を背景と混ぜて見せる（8 万画素に収まるよう 4 倍に拡大）
    (br, bg, bb) = hsbToRgb 40 8 96
    alpha = 120 / 255
    previewW = picW `div` 4
    previewH = picH `div` 4
    preview = Translate 0 (height / 2 - (oy + fromIntegral picH / 2)) $
      pixelImage previewW previewH 4 $ \x y ->
        let d = dens U.! (min (gw - 1) (x * 4 `div` res) + min (gh - 1) (y * 4 `div` res) * gw)
            v = 255 * (1 - d)
            mix c = w8 (c * (1 - alpha) + v * alpha)
        in (mix br, mix bg, mix bb)

-- 最初の点：暗い所ほど置かれやすい（棄却サンプリング）
initDots :: World -> IO World
initDots w = do
  g0 <- newStdGen
  let dens = wDens w
      n = wN w
      go :: Int -> Int -> StdGen -> [Point] -> [Point]
      go !got !guard g acc
        | got >= n || guard >= n * 200 = acc
        | otherwise =
            let (gx, g1) = randomR (0, gw - 1) g
                (gy, g2) = randomR (0, gh - 1) g1
                (t, g3) = randomR (0, 1 :: Float) g2
                (fx, g4) = randomR (0, 1 :: Float) g3
                (fy, g5) = randomR (0, 1 :: Float) g4
            in if t < U.unsafeIndex dens (gx + gy * gw) * 0.95 + 0.002
                 then go (got + 1) (guard + 1) g5 ((fromIntegral gx + fx, fromIntegral gy + fy) : acc)
                 else go got (guard + 1) g3 acc
      ds = go 0 0 g0 []
  pure w { wXs = U.fromList (map fst ds), wYs = U.fromList (map snd ds), wIter = 0 }

data Best = Best !Int !Float

-- ★ 重みつきロイドの 1 回
stippleStep :: World -> World
stippleStep w
  | n == 0 = w
  | otherwise = w { wXs = xs', wYs = ys', wMoved = moved, wIter = wIter w + 1 }
  where
    xs = wXs w
    ys = wYs w
    dens = wDens w
    n = U.length xs
    -- 近くの点をすばやく探すためのマス目。暗い所で 1 マスに点が 1 個くらい
    cs = max 2 (sqrt (wTotal w / fromIntegral (max 1 n)))
    bw = ceiling (fromIntegral gw / cs) :: Int
    bh = ceiling (fromIntegral gh / cs) :: Int
    bucketOf i =
      min (bw - 1) (max 0 (floor (U.unsafeIndex xs i / cs)))
        + min (bh - 1) (max 0 (floor (U.unsafeIndex ys i / cs))) * bw
    cells = U.generate n bucketOf :: U.Vector Int
    counts = U.accumulate (+) (U.replicate (bw * bh) (0 :: Int)) (U.map (\c -> (c, 1)) cells)
    starts = U.scanl' (+) 0 counts
    items = U.create $ do
      pos <- U.thaw starts
      out <- MU.new n
      forM_ [0 .. n - 1] $ \i -> do
        let c = U.unsafeIndex cells i
        p <- MU.unsafeRead pos c
        MU.unsafeWrite out p i
        MU.unsafeWrite pos c (p + 1)
      pure out

    -- 近くのマスだけ調べて最寄りの点を探す（近くになければ範囲を広げる）
    nearestDot :: Float -> Float -> Int
    nearestDot x y = search 1 (Best (-1) (1 / 0))
      where
        cx = floor (x / cs) :: Int
        cy = floor (y / cs) :: Int
        search :: Int -> Best -> Int
        search !r !b0 =
          let !b@(Best bi bd) = rows (cy - r) r b0
              rr = fromIntegral r * cs
          in if r >= 6 || (bi >= 0 && bd <= rr * rr) then bi else search (r + 1) b
        rows !j !r !b
          | j > cy + r = b
          | j < 0 || j >= bh = rows (j + 1) r b
          | otherwise = rows (j + 1) r (cols j (max 0 (cx - r)) (min (bw - 1) (cx + r)) b)
        cols !j !i !iEnd !b
          | i > iEnd = b
          | otherwise =
              let c = i + j * bw
              in cols j (i + 1) iEnd (scan (U.unsafeIndex starts c) (U.unsafeIndex starts (c + 1)) b)
        scan !k !kEnd b@(Best _ bd)
          | k >= kEnd = b
          | otherwise =
              let q = U.unsafeIndex items k
                  dx = U.unsafeIndex xs q - x
                  dy = U.unsafeIndex ys q - y
                  d = dx * dx + dy * dy
              in scan (k + 1) kEnd (if d < bd then Best q d else b)

    (xs', ys', moved) = runST $ do
      sx <- MU.replicate n (0 :: Float)
      sy <- MU.replicate n (0 :: Float)
      sw <- MU.replicate n (0 :: Float)
      let loop !gi
            | gi >= gw * gh = pure ()
            | otherwise = do
                let v = U.unsafeIndex dens gi
                when (v >= 0.01) $ do                 -- 真っ白な所は重心に効かない
                  let gy = gi `quot` gw
                      gx = gi - gy * gw
                      x = fromIntegral gx + 0.5
                      y = fromIntegral gy + 0.5
                      k = nearestDot x y
                  when (k >= 0) $ do
                    MU.unsafeModify sx (+ v * x) k
                    MU.unsafeModify sy (+ v * y) k
                    MU.unsafeModify sw (+ v) k
                loop (gi + 1)
      loop 0
      fx <- U.unsafeFreeze sx
      fy <- U.unsafeFreeze sy
      fw <- U.unsafeFreeze sw
      let move old s = U.izipWith (\i o a -> let m = U.unsafeIndex fw i in if m <= 0 then o else a / m) old s
          nx = move xs fx
          ny = move ys fy
          total = U.sum (U.zipWith (+) (U.map abs (U.zipWith (-) nx xs)) (U.map abs (U.zipWith (-) ny ys)))
      pure (nx, ny, total / fromIntegral n)

drawAll :: World -> Picture
drawAll w = Pictures
  [ if wShowSrc w then wSrcImg w else Blank
  , view [Color (hsb 230 40 15) (Pictures (zipWith dotPic (U.toList (wXs w)) (U.toList (wYs w))))]
  , txt 20 30 16 (hsb 0 0 15) ("Space: relax " ++ onOff (wRun w) ++ "  1-3: picture  Left/Right: dots " ++ show (U.length (wXs w))
      ++ "  V: source  D: size by dark " ++ onOff (wSize w) ++ "  R: redo  Steps " ++ show (wIter w))
  , txt 20 52 14 (hsb 0 0 35) "Each dot moves to the darkness-weighted centroid of its cell (the pixels nearest to it)."
  , txt 20 74 14 (hsb 0 0 35) ("Dots gather in dark areas with even spacing. Mean move " ++ printf "%.2f" (wMoved w * fromIntegral res) ++ " px")
  ]
  where
    dens = wDens w
    s = fromIntegral res :: Float
    dotPic x y =
      let i = max 0 (min (gw - 1) (floor x)) + max 0 (min (gh - 1) (floor y)) * gw
          dk = if U.null dens then 0 else U.unsafeIndex dens i
          d = if wSize w then 1.5 + 3.5 * dk else 3            -- ★ 点の大きさ（直径）
      in at (ox + x * s, oy + y * s) (disc (d / 2))

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wRun = not (wRun w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> initDots w { wN = min 5000 (wN w + 500) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> initDots w { wN = max 500 (wN w - 500) }
  EventKey (Char c) Down _ _ -> case toLower c of
    '1' -> makePicture w { wPicture = 1 }
    '2' -> makePicture w { wPicture = 2 }
    '3' -> makePicture w { wPicture = 3 }
    'v' -> pure w { wShowSrc = not (wShowSrc w) }
    'd' -> pure w { wSize = not (wSize w) }
    'r' -> initDots w
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 緩和を止めた最初の状態（R の直後）と、30 回後を比べよう。ざらざら → なめらかになる理由は？
-- 2. 重みを dens の 2 乗にすると、暗い所への集まり方はどう変わる？（コントラストが強くなる）
-- 3. srcGray に自分の図形を足して点描画にしよう。S キーで saveSvg を足すと保存できる
-- 4. 点の代わりに短い線や小さな ♥ を描くと、違うタッチの絵になる

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は p5 の text と同じ左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- 色の変換（HSB → RGB 0〜255）
hsbToRgb :: Float -> Float -> Float -> (Float, Float, Float)
hsbToRgb h s0 b0 = (f 5, f 3, f 1)
  where
    s = s0 / 100
    b = b0 / 100
    f :: Float -> Float
    f n = let k = wrapF 6 (n + h / 60) in 255 * b * (1 - s * max 0 (min k (min (4 - k) 1)))

w8 :: Float -> Word8
w8 v = fromIntegral (max 0 (min 255 (truncate (v + 0.5) :: Int)))
