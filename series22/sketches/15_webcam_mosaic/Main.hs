-- ============================================
-- 15 カメラでボロノイ（おまけ）
-- カメラの映像にサイトをまき、セルの中の色の平均で塗る → ボロノイのモザイク（ステンドグラス風の自分）
--   1 = サイトをランダムに / 2 = 輪郭（明るさが急に変わる所）に多くまく → 少ない数でも顔の形が残る
-- 1・2 = まき方 / ←→ = サイトの数 / A = 平均の色 ⇄ サイトの点の色 / E = 境界線 / スペース = 止める（カメラがなければ作った絵で動く）
-- ============================================
module Main (main) where

import Art
import Control.Monad (when)
import Control.Monad.ST (runST)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import System.Random (StdGen, newStdGen, randomR)

-- ★ カメラ画像の大きさ
vw, vh :: Int
vw = 160
vh = 120

-- ★ セルを計算する細かさ（カメラ 1 画素を lv×lv に分ける）
lv, lw, lh :: Int
lv = 2
lw = vw * lv
lh = vh * lv

data World = World
  { wCam    :: !(Maybe Camera)
  , wXs     :: !(U.Vector Float)   -- サイトの位置（カメラの画素の単位）
  , wYs     :: !(U.Vector Float)
  , wN      :: !Int       -- ★ サイトの数
  , wMode   :: !Int
  , wAvg    :: !Bool      -- 平均の色 ⇄ サイトの点の色
  , wEdges  :: !Bool
  , wFrozen :: !Bool
  , wLab    :: !(U.Vector Int)     -- 各画素がどのサイトのセルか
  , wNeed   :: !Bool      -- セルを作り直す必要がある
  , wLive   :: !Bool      -- 本物のカメラか
  , wTick   :: !Int
  , wImg    :: !Picture
  }

main :: IO ()
main = runSketch (sketch "22-15 Webcam Mosaic" world0)
  { skBack  = hsb 230 25 12
  , skStart = \_ -> do
      cam <- startCamera vw vh
      scatter Nothing world0 { wCam = Just cam }
  , skStep  = \_ _ w -> step w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World Nothing U.empty U.empty 400 1 True True False (U.replicate (lw * lh) 0) True False 0 Blank

-- 映像の画素（RGB）。カメラが使えなければ Art.Device が作る動く絵になる
source :: World -> IO (B.ByteString, Bool)
source w = case wCam w of
  Nothing -> pure (B.replicate (vw * vh * 3) 0, False)
  Just cam -> do
    f <- cameraFrame cam
    live <- camLive cam
    pure (if B.length f >= vw * vh * 3 then f else B.replicate (vw * vh * 3) 0, live)

byteAt :: B.ByteString -> Int -> Float
byteAt f i = fromIntegral (BU.unsafeIndex f i)

-- ★ サイトをまく：1 = ランダム、2 = 輪郭ほど多く（明るさの差が大きい所）
scatter :: Maybe B.ByteString -> World -> IO World
scatter mpx w = do
  g0 <- newStdGen
  let ps = case mpx of
        Just px | wMode w == 2 -> edgeSites px (wN w) g0
        _ -> uniformSites (wN w) g0
  pure w { wXs = U.fromList (map fst ps), wYs = U.fromList (map snd ps), wNeed = True }

uniformSites :: Int -> StdGen -> [Point]
uniformSites n = go n []
  where
    go :: Int -> [Point] -> StdGen -> [Point]
    go 0 acc _ = acc
    go k acc g =
      let (x, g1) = randomR (0, fromIntegral vw) g
          (y, g2) = randomR (0, fromIntegral vh) g1
      in go (k - 1) ((min x 159.99, min y 119.99) : acc) g2

edgeSites :: B.ByteString -> Int -> StdGen -> [Point]
edgeSites px n = go 0 0 []
  where
    gray' i = (byteAt px (3 * i) + byteAt px (3 * i + 1) + byteAt px (3 * i + 2)) / 3
    go :: Int -> Int -> [Point] -> StdGen -> [Point]
    go !got !guard acc g
      | got >= n || guard >= n * 100 = acc
      | otherwise =
          let (x, g1) = randomR (1, vw - 2) g
              (y, g2) = randomR (1, vh - 2) g1
              (t, g3) = randomR (0, 1 :: Float) g2
              i = x + y * vw
              e = abs (gray' (i + 1) - gray' (i - 1)) + abs (gray' (i + vw) - gray' (i - vw))
          in if t < 0.04 + e / 60
               then let (fx, g4) = randomR (0, 1 :: Float) g3
                        (fy, g5) = randomR (0, 1 :: Float) g4
                    in go (got + 1) (guard + 1) ((fromIntegral x + fx, fromIntegral y + fy) : acc) g5
               else go got (guard + 1) acc g3

data Best = Best !Int !Float

-- 各画素の最寄りのサイトを、マス目で近くだけ探して決める
computeLabels :: U.Vector Float -> U.Vector Float -> U.Vector Int
computeLabels xs ys = U.generate (lw * lh) labelAt
  where
    n = U.length xs
    fvw = fromIntegral vw :: Float
    fvh = fromIntegral vh :: Float
    cs = max 2 (sqrt (fvw * fvh / fromIntegral (max 1 n)) * 1.2)
    bw = ceiling (fvw / cs) :: Int
    bh = ceiling (fvh / cs) :: Int
    bucketOf i = min (bw - 1) (floor (U.unsafeIndex xs i / cs)) + min (bh - 1) (floor (U.unsafeIndex ys i / cs)) * bw
    cells = U.generate n bucketOf :: U.Vector Int
    counts = U.accumulate (+) (U.replicate (bw * bh) (0 :: Int)) (U.map (\c -> (c, 1)) cells)
    starts = U.scanl' (+) 0 counts
    items = U.create $ do
      pos <- U.thaw starts
      out <- MU.new n
      let fill !i
            | i >= n = pure ()
            | otherwise = do
                let c = U.unsafeIndex cells i
                p <- MU.unsafeRead pos c
                MU.unsafeWrite out p i
                MU.unsafeWrite pos c (p + 1)
                fill (i + 1)
      fill 0
      pure out
    labelAt li =
      let ly = li `quot` lw
          lx = li - ly * lw
          x = (fromIntegral lx + 0.5) / fromIntegral lv
          y = (fromIntegral ly + 0.5) / fromIntegral lv
          cx = floor (x / cs) :: Int
          cy = floor (y / cs) :: Int
          search :: Int -> Best -> Int
          search !r !b0 =
            let !b@(Best bi bd) = rows (cy - r) r b0
                rr = fromIntegral r * cs
            in if r >= 8 || bd <= rr * rr then bi else search (r + 1) b
          rows !j !r !b
            | j > cy + r = b
            | j < 0 || j >= bh = rows (j + 1) r b
            | otherwise = rows (j + 1) r (colsLoop j (max 0 (cx - r)) (min (bw - 1) (cx + r)) b)
          colsLoop !j !i !iEnd !b
            | i > iEnd = b
            | otherwise =
                let c = i + j * bw
                in colsLoop j (i + 1) iEnd (scan (U.unsafeIndex starts c) (U.unsafeIndex starts (c + 1)) b)
          scan !k !kEnd b@(Best _ bd)
            | k >= kEnd = b
            | otherwise =
                let q = U.unsafeIndex items k
                    dx = U.unsafeIndex xs q - x
                    dy = U.unsafeIndex ys q - y
                    d = dx * dx + dy * dy
                in scan (k + 1) kEnd (if d < bd then Best q d else b)
      in search 1 (Best 0 (1 / 0))

-- セルごとの色：平均（A）かサイトの点の色。(赤の和, 緑の和, 青の和, 数) を返す
cellColors :: World -> B.ByteString -> U.Vector Int -> (U.Vector Float, U.Vector Float, U.Vector Float, U.Vector Float)
cellColors w px lab
  | wAvg w = runST $ do
      sr <- MU.replicate n 0
      sg <- MU.replicate n 0
      sb <- MU.replicate n 0
      sc <- MU.replicate n 0
      let loop !li
            | li >= lw * lh = pure ()
            | otherwise = do
                let l = U.unsafeIndex lab li
                    ly = li `quot` lw
                    lx = li - ly * lw
                    k = 3 * (lx `quot` lv + (ly `quot` lv) * vw)
                when (l < n) $ do
                  MU.unsafeModify sr (+ byteAt px k) l
                  MU.unsafeModify sg (+ byteAt px (k + 1)) l
                  MU.unsafeModify sb (+ byteAt px (k + 2)) l
                  MU.unsafeModify sc (+ 1) l
                loop (li + 1)
      loop 0
      (,,,) <$> U.unsafeFreeze sr <*> U.unsafeFreeze sg <*> U.unsafeFreeze sb <*> U.unsafeFreeze sc
  | otherwise =
      let at' off = U.generate n $ \i ->
            let x = min (vw - 1) (floor (U.unsafeIndex (wXs w) i))
                y = min (vh - 1) (floor (U.unsafeIndex (wYs w) i))
            in byteAt px (3 * (x + y * vw) + off)
      in (at' 0, at' 1, at' 2, U.replicate n 1)
  where n = U.length (wXs w)

-- 画面いっぱいに 4:3 で表示
scaleK :: Float
scaleK = min ((width - 40) / fromIntegral lw) ((height - 110) / fromIntegral lh)

render :: World -> B.ByteString -> Picture
render w px = Translate 0 (height / 2 - (95 + fromIntegral lh * scaleK / 2)) (pixelImage lw lh scaleK pixel)
  where
    lab = wLab w
    (sr, sg, sb, sc) = cellColors w px lab
    n = U.length sr
    edges = wEdges w
    pixel lx ly =
      let i = lx + ly * lw
          l = U.unsafeIndex lab i
          e = edges && ((lx + 1 < lw && U.unsafeIndex lab (i + 1) /= l) || (ly + 1 < lh && U.unsafeIndex lab (i + lw) /= l))
          f = if e then 0.25 else 1
      in if l >= n then (0, 0, 0) else
         let c = max 1 (U.unsafeIndex sc l)
         in ( w8 (U.unsafeIndex sr l / c * f), w8 (U.unsafeIndex sg l / c * f), w8 (U.unsafeIndex sb l / c * f) )

step :: World -> IO World
step w0'
  | wFrozen w0' = pure w0'
  | otherwise = do
      (px, live) <- source w0'
      let tick = wTick w0' + 1
      -- 輪郭は動くので時々まき直す
      w1 <- if wMode w0' == 2 && tick `mod` 15 == 0 then scatter (Just px) w0' else pure w0'
      let w2 = if wNeed w1 && U.length (wXs w1) > 0
                 then w1 { wLab = computeLabels (wXs w1) (wYs w1), wNeed = False }
                 else w1
      pure w2 { wTick = tick, wLive = live, wImg = render w2 px }

drawAll :: World -> Picture
drawAll w = Pictures
  [ wImg w
  , txt 20 30 16 (gray 1) ("1,2: scatter = " ++ (if wMode w == 1 then "random" else "more on edges") ++ "  Left/Right: sites " ++ show (wN w)
      ++ "  A: " ++ (if wAvg w then "cell average" else "site color") ++ "  E: edges " ++ onOff (wEdges w) ++ "  Space: " ++ (if wFrozen w then "resume" else "freeze"))
  , txt 20 52 14 (hsb 0 0 65) ((if wLive w then "Camera on. " else "No camera: using a generated picture. ") ++ "Averaging a cell removes fine detail and keeps only shapes.")
  , txt 20 74 14 (hsb 0 0 65) "With sites gathered on edges, outlines stay sharp with few sites (small cells = high resolution)."
  ]

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> rescatter w { wN = min 3000 (floor (fromIntegral (wN w) * 1.5 :: Float)) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rescatter w { wN = max 30 (floor (fromIntegral (wN w) / 1.5 :: Float)) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wFrozen = not (wFrozen w) }
  EventKey (Char c) Down _ _ -> case toLower c of
    '1' -> scatter Nothing w { wMode = 1 }
    '2' -> rescatter w { wMode = 2 }
    'a' -> pure w { wAvg = not (wAvg w) }
    'e' -> pure w { wEdges = not (wEdges w) }
    _ -> pure w
  _ -> pure w
  where
    rescatter v
      | wMode v == 2 = do
          (px, _) <- source v
          scatter (Just px) v
      | otherwise = scatter Nothing v

-- ---------- やってみよう ----------
-- 1. サイトを 30 個と 3000 個で比べよう。何個あれば自分だとわかる？
-- 2. まき方 2 で、輪郭の重み（e / 60 の 60）を変えてみよう。小さくすると輪郭にもっと集まる
-- 3. セルを平均の色でなく「いちばん明るい色」や「白黒 2 値」で塗ると、ポスター風になる
-- 4. 06 のロイド緩和を 1 回かけてからモザイクにすると、セルの大きさがそろってタイルのようになる

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- 文字：(x, y) は p5 の text と同じ左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

w8 :: Float -> Word8
w8 v = fromIntegral (max 0 (min 255 (truncate (v + 0.5) :: Int)))
