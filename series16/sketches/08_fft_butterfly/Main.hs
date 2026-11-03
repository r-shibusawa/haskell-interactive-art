-- ============================================
-- 08 FFT：分けて、まとめる（再帰とバタフライ）
-- 1：再帰の木　偶数番目と奇数番目に分けて半分の DFT を 2 つ → 「バタフライ」でまとめる（再帰版 FFT）
-- 2：バタフライ回路　葉の並び（ビット反転順）から始めて、下から順にまとめる（反復版 FFT）
-- スペース：1 段進む / A：自動 / R：最初から / ←→：n = 4, 8, 16 / 入力の丸をクリック：値を変える
-- 3：インパルス  4：定数  5：方形波  6：ランダム（M でモード切りかえ）
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (shiftL, testBit, (.|.))
import Data.Char (digitToInt, toLower)
import Data.List (foldl')
import Numeric (showFFloat, showIntAtBase)

speed :: Int
speed = 60   -- ★ 自動再生の間隔（フレーム）

type C = (Double, Double)

-- 再帰の木の 1 ノード：係数の番号のリストと DFT の結果
data Node = Node { nIdx :: ![Int], nY :: [C] }

data World = World
  { wN      :: !Int      -- ★ 点の数（2 の累乗）
  , wMode   :: !Int
  , wA      :: ![Int]
  , wStages :: [[C]]     -- 反復版の各段の値
  , wTree   :: [[Node]]  -- 再帰版の木（深さごと）
  , wMax    :: !Double   -- 針の長さのものさし
  , wStage  :: !Int
  , wAuto   :: !Bool
  , wTimer  :: !Int
  , wFlow   :: !Float
  }

main :: IO ()
main = runSketch (sketch "16-08 FFT Butterfly" (compute (World 8 2 (square 8) [] [] 1 0 False 0 0)))
  { skBack  = bgC
  , skStep  = \_ _ w -> pure (stepW w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

square :: Int -> [Int]
square n = [if j < n `div` 2 then 1 else -1 | j <- [0 .. n - 1]]

input :: Int -> World -> IO World
input k w = do
  let n = wN w
  a <- case k of
    3 -> pure [if j == 0 then 1 else 0 | j <- [0 .. n - 1]]
    4 -> pure (replicate n 1)
    5 -> pure (square n)
    _ -> replicateM n (randomRIO (-2, 2))
  pure (compute w { wA = a, wStage = 0 })

cmul, cadd, csub :: C -> C -> C
cmul (a, b) (c, d) = (a * c - b * d, a * d + b * c)
cadd (a, b) (c, d) = (a + c, b + d)
csub (a, b) (c, d) = (a - c, b - d)

-- ω_m^j
omega :: Int -> Int -> C
omega m j = let t = 2 * pi * fromIntegral j / fromIntegral m in (cos t, sin t)

mag :: C -> Double
mag (a, b) = sqrt (a * a + b * b)

rev :: Int -> Int -> Int
rev bits i = foldl' (\r b -> if testBit i b then r .|. shiftL 1 (bits - 1 - b) else r) 0 [0 .. bits - 1]

log2n :: Int -> Int
log2n n = round (logBase 2 (fromIntegral n :: Double))

-- 反復版の 1 段：長さ m のバタフライ
stageOf :: Int -> [C] -> [C]
stageOf s xs = concatMap block (chunks m xs)
  where
    m = shiftL 1 s
    block ys =
      let (u, v) = splitAt (m `div` 2) ys
          t = zipWith (\j y -> cmul (omega m j) y) [0 ..] v
      in zipWith cadd u t ++ zipWith csub u t

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks m xs = let (a, b) = splitAt m xs in a : chunks m b

-- 再帰版 FFT：番号のリストを受け取って DFT を返す
fftRec :: [Int] -> [Int] -> [C]
fftRec a [i] = [(fromIntegral (nthI a i), 0)]
fftRec a idx =
  let (ev, od) = evenOdd idx
      y0 = fftRec a ev
      y1 = fftRec a od
      m = length idx
      t = zipWith (\k y -> cmul (omega m k) y) [0 ..] y1
  in zipWith cadd y0 t ++ zipWith csub y0 t

evenOdd :: [a] -> ([a], [a])
evenOdd xs = ([x | (i, x) <- zip [0 :: Int ..] xs, even i], [x | (i, x) <- zip [0 :: Int ..] xs, odd i])

compute :: World -> World
compute w =
  let n = wN w
      a = wA w
      l = log2n n
      start = [(fromIntegral (nthI a (rev l i)), 0) | i <- [0 .. n - 1]]
      stages = scanl (\xs s -> stageOf s xs) start [1 .. l]
      levels = take (l + 1) (iterate (concatMap (\ix -> let (e, o) = evenOdd ix in [e, o])) [[0 .. n - 1]])
      tree = map (map (\ix -> Node ix (fftRec a ix))) levels
      mx = foldl' max 1e-9 (map mag (concat stages))
  in w { wStages = stages, wTree = tree, wMax = mx }

maxStage :: World -> Int
maxStage w = let l = log2n (wN w) in if wMode w == 1 then 2 * l else l

nextStage :: World -> World
nextStage w = w { wStage = if wStage w >= maxStage w then 0 else wStage w + 1 }

stepW :: World -> World
stepW w0 =
  let timer = if wAuto w0 then wTimer w0 + 1 else wTimer w0
      w1 = w0 { wTimer = timer, wFlow = wrapF 1 (wFlow w0 + 0.02) }
  in if wAuto w0 && timer `mod` speed == 0 then nextStage w1 else w1

drawW :: Env -> World -> Picture
drawW env w = Pictures
  [ txt env 30 50 26 (g8 235) (if wMode w == 1 then "FFT: recursion tree (split, then combine)" else "FFT: butterfly circuit (iterative)")
  , txt env 30 72 15 (g8 150) ("M: mode (1: tree, 2: circuit)  Space: 1 stage  A: auto" ++ (if wAuto w then " (on)" else "") ++ "  R: redo  L/R: n = " ++ show (wN w))
  , txt env 30 94 15 (g8 150) "Click an input circle: change value  3: impulse  4: constant  5: square  6: random"
  , if wMode w == 1 then drawTree env w else drawButterfly env w
  ]

-- 複素数を針で描く：長さ = 大きさ、向き = 位相
phasor :: Env -> Double -> Float -> Float -> C -> Float -> Color -> Picture
phasor env mx x y v r col =
  let mg = mag v
      s = realToFrac (min 1 (mg / mx)) * r
      ux = realToFrac (fst v / mg) :: Float
      uy = realToFrac (snd v / mg) :: Float
  in Pictures
       [ hoop env (rgb255 60 52 80) 1 x y (r * 2)
       , if mg > 1e-9 then seg env col 2.5 (x, y) (x + ux * s, y - uy * s) else Blank
       , dot env col x y 4 ]

drawTree :: Env -> World -> Picture
drawTree env w =
  let n = wN w
      l = log2n n
      stage = wStage w
      top = 125
      rowH = (envH env - 280) / (fromIntegral l + 0.7)
      -- stage 0..L：下へ分ける / L..2L：上へまとめる
      downTo = min stage l
      upFrom = if stage > l then l - (stage - l) else l + 1
      levelPic (d, nodes) =
        let bw = (envW env - 80) / fromIntegral (length nodes)
            nodePic (i, nd) =
              let x = 40 + fromIntegral (i :: Int) * bw
                  y = top + fromIntegral d * rowH
                  done = d >= upFrom || (d == l && stage >= l)
                  ids = nIdx nd
                  ys = nY nd
                  len = fromIntegral (length ids) :: Float
                  pr = min 22 (min (bw / (len * 2.6)) (rowH * 0.18))
                  cy = y + rowH * 0.62
                  ny = top + fromIntegral (d + 1) * rowH
              in Pictures
                   [ box env (if done then rgb255 30 70 66 else cellC) (x + 4) y (bw - 8) (rowH * 0.62)
                   , frame env (if done then mint else rgb255 90 80 120) 1.5 (x + 4) y (bw - 8) (rowH * 0.62)
                   , txtC env (x + bw / 2) (y + 15) 14 (g8 230) (unwords (map (('a' :) . show) ids))
                   , if done
                       then Pictures [ phasor env (wMax w) (x + bw / 2 + (fromIntegral k - (len - 1) / 2) * pr * 2.4) (y + rowH * 0.42) v pr amber
                                     | (k, v) <- zip [0 :: Int ..] ys ]
                       else Blank
                   , if d < l && d < downTo
                       then Pictures [ seg env (rgba255 255 95 109 150) 2 (x + bw * 0.5, cy) (x + bw * 0.25, ny)
                                     , seg env (rgba255 255 95 109 150) 2 (x + bw * 0.5, cy) (x + bw * 0.75, ny) ]
                       else Blank
                   ]
        in if d > downTo then Blank else Pictures (map nodePic (zip [0 ..] nodes))
      my = envH env - 120
      msg | stage <= l = "Split: even (0, 2, 4..) and odd (1, 3, 5..) indices. A(x) = A0(x^2) + x*A1(x^2). Down to level " ++ show downTo
          | otherwise = "Combine (butterfly): y_k = y0_k + w^k*y1_k, y_(k+m/2) = y0_k - w^k*y1_k. Yellow needles are DFT values (length = size, direction = phase)"
  in Pictures
       [ Pictures (map levelPic (zip [0 :: Int ..] (wTree w)))
       , box env panelC 30 my (envW env - 60) 100
       , para env 46 (my + 10) 15 (g8 235) (envW env - 100) msg
       , para env 46 (my + 54) 14 (g8 150) (envW env - 100)
           "Leaves a0 a4 a2 a6 a1 a5 a3 a7: the index read backward in binary (bit reversal), so the iterative version sorts that way first. Each level Theta(n), log n levels: Theta(n log n)"
       ]

-- バタフライ回路の左の余白
leftX :: Float
leftX = 190

topY :: Float
topY = 130

rowHOf :: Env -> Int -> Float
rowHOf env n = (envH env - 270) / fromIntegral (n - 1)

drawButterfly :: Env -> World -> Picture
drawButterfly env w =
  let n = wN w
      l = log2n n
      stage = wStage w
      rowH = rowHOf env n
      colW = (envW env - leftX - 360) / fromIntegral l
      xAt s = leftX + fromIntegral (s :: Int) * colW
      yAt i = topY + fromIntegral (i :: Int) * rowH
      flow = wFlow w
      stagesL = wStages w
      valueAt s i = case drop s stagesL of
        (st : _) -> case drop i st of
          (v : _) -> v
          [] -> (0, 0)
        [] -> (0, 0)
      -- 線
      wing s k0 j =
        let m = shiftL 1 s :: Int
            u = k0 + j
            v = k0 + j + m `div` 2
            active = s == stage
            past = s < stage
            col | active = amber
                | past = rgba255 78 225 190 140
                | otherwise = rgb255 70 60 95
            th = if active then 2.5 else 1.5
            ln p q = seg env col th (xAt (s - 1), yAt p) (xAt s, yAt q)
            beads = if active
              then [ dot env amber (lerpF (xAt (s - 1)) (xAt s) flow) (lerpF (yAt p) (yAt q) flow) 7
                   | (p, q) <- [(u, u), (v, v), (u, v), (v, u)] ]
              else []
            lbl = if rowH > 22 || j == 0
              then txtC env (lerpF (xAt (s - 1)) (xAt s) 0.25) (yAt v - 11) 14 (rgb255 255 150 160) ("w" ++ show m ++ "^" ++ show j)
              else Blank
        in Pictures ([ln u u, ln v v, ln u v, ln v u] ++ beads ++ [lbl])
      wings = [ wing s k0 j
              | s <- [1 .. l], let m = shiftL 1 s :: Int
              , k0 <- [0, m .. n - 1], j <- [0 .. m `div` 2 - 1] ]
      -- ノード（値は針で表示）
      r = min 16 (rowH * 0.42)
      nodes = [ Pictures
                  [ dot env plotC (xAt s) (yAt i) (r * 2.2)
                  , if s <= stage
                      then phasor env (wMax w) (xAt s) (yAt i) (valueAt s i) r (if s == l then mint else amber)
                      else Blank ]
              | s <- [0 .. l], i <- [0 .. n - 1] ]
      -- 入力ラベル（ビット反転）
      inputs = [ let j = rev l i
                 in Pictures
                      [ txtR env (xAt 0 - 24) (yAt i - 4) 14 (g8 230) ("a" ++ show j ++ " = " ++ show (nthI (wA w) j))
                      , txtR env (xAt 0 - 24) (yAt i + 13) 14 (g8 150) (bin l i ++ " -> " ++ bin l j) ]
               | i <- [0 .. n - 1] ]
      -- 出力の大きさ
      ox = xAt l + 40
      outs = [ let v = valueAt l i
                   mg = mag v
               in Pictures
                    [ box env (if stage >= l then mint else rgb255 60 52 80) ox (yAt i - min 10 (rowH * 0.3))
                          (realToFrac (mg / fromIntegral n) * 160) (min 20 (rowH * 0.6))
                    , txt env (ox + 170) (yAt i + 6) 14 (g8 200) ("y" ++ show i ++ "  |" ++ showFFloat (Just 2) mg "" ++ "|") ]
             | i <- [0 .. n - 1] ]
      my = envH env - 120
      msg | stage == 0 = "Level 0: arrange the input in bit-reversed order (read the binary index backward). See the numbers on the left"
          | stage < l = "Level " ++ show stage ++ ": butterflies of length " ++ show (shiftL 1 stage :: Int)
                        ++ ". Upper = u + w*t, lower = u - w*t (one multiplication gives two values)"
          | otherwise = "Done: " ++ show l ++ " levels x " ++ show (n `div` 2) ++ " butterflies = " ++ show (l * n `div` 2)
                        ++ " complex multiplications (the plain DFT needs " ++ show (n * n) ++ ")"
  in Pictures
       [ Pictures wings
       , Pictures nodes
       , Pictures inputs
       , Pictures outs
       , box env panelC 30 my (envW env - 60) 100
       , para env 46 (my + 10) 15 (g8 235) (envW env - 100) msg
       , para env 46 (my + 54) 14 (g8 150) (envW env - 100)
           "Needle length = size, direction = phase. Square (5): only odd frequencies. Impulse (3): all equal. Constant (4): only y0. All of a level can run at once (parallel hardware)"
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | wMode w == 2 ->
        let n = wN w
            l = log2n n
            rowH = rowHOf env n
            m = mouseP env
            hits = [rev l i | i <- [0 .. n - 1], dist2 m (leftX, topY + fromIntegral i * rowH) < 16]
            bump a j = [if i == j then (if v >= 2 then -1 else v + 1) else v | (i, v) <- zip [0 ..] a]
        in pure (if null hits then w else compute w { wA = foldl' bump (wA w) hits })
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (nextStage w)
  EventKey (SpecialKey KeyRight) Down _ _
    | wN w < 16 -> input 5 w { wN = wN w * 2 }
  EventKey (SpecialKey KeyLeft) Down _ _
    | wN w > 4 -> input 5 w { wN = wN w `div` 2 }
  EventKey (Char c0) Down _ _ -> case toLower c0 of
    'a' -> pure w { wAuto = not (wAuto w) }
    'r' -> pure w { wStage = 0 }
    'm' -> pure w { wMode = 3 - wMode w, wStage = 0 }
    c | c `elem` "3456" -> input (digitToInt c) w
      | c `elem` "12" -> pure w { wMode = digitToInt c, wStage = 0 }
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 再帰の木（1）の葉の並びと、バタフライ回路（2）の入力の並びが同じことを確かめる
-- 2. n = 16 で掛け算の回数を数える。DFT のままの何分の 1？ n = 1024 なら？
-- 3. 入力を 1 つだけ 1 にする位置を変えると、出力の大きさは変わらず、針の向きだけが変わる

-- ---------- 道具 ----------
nthI :: [Int] -> Int -> Int
nthI xs i = case drop i xs of
  (x : _) -> x
  [] -> 0

-- 2 進数の文字列（L けたになるまで 0 を足す）
bin :: Int -> Int -> String
bin l v =
  let s = showIntAtBase 2 (\d -> if d == 0 then '0' else '1') v ""
  in replicate (l - length s) '0' ++ s

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> (Float, Float) -> (Float, Float)
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> (Float, Float) -> (Float, Float) -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 長方形の枠線（太さ t）
frame :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
frame env c t x y w h = Pictures
  [ seg env c t (x, y) (x + w, y), seg env c t (x + w, y) (x + w, y + h)
  , seg env c t (x + w, y + h) (x, y + h), seg env c t (x, y + h) (x, y) ]

-- 直径 d の円（p5.js の circle）
dot :: Env -> Color -> Float -> Float -> Float -> Picture
dot env c x y d = Color c (at (pt env (x, y)) (disc (d / 2)))

-- 直径 d の円の輪郭（太さ t）
hoop :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
hoop env c t x y d = Color c (at (pt env (x, y)) (ring (d / 2) t))

-- 文字：txt は左下が (x, y)、txtT は左上が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtT, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtT env x y s c str = txt env x (y + s) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = txt env (x - fromIntegral (length str) * s * 0.78) y s c str

-- ---------- 道具：文字の折り返し（文字の幅はおよそ 0.85 × 大きさ） ----------
wrapText :: Float -> Float -> String -> [String]
wrapText s w = concatMap one . lines
  where
    limit = max 8 (floor (w / (s * 0.85))) :: Int
    one l = case words l of
      [] -> [""]
      (x : xs) -> go x xs
    go cur [] = [cur]
    go cur (x : xs)
      | length cur + 1 + length x <= limit = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs

lineH :: Float -> Float
lineH s = s * 1.35

-- 左上 (x, y) から幅 w で折り返して書く
para :: Env -> Float -> Float -> Float -> Color -> Float -> String -> Picture
para env x y s c w str = Pictures
  [ txtT env x (y + fromIntegral i * lineH s) s c l | (i, l) <- zip [0 :: Int ..] (wrapText s w str) ]

-- 折り返したときの高さ
paraH :: Float -> Float -> String -> Float
paraH s w str = fromIntegral (length (wrapText s w str)) * lineH s

-- 上から順に積む：(大きさ, 色, 下のすきま, 文字)
stack :: Env -> Float -> Float -> Float -> [(Float, Color, Float, String)] -> Picture
stack env x y0 w = Pictures . go y0
  where
    go _ [] = []
    go y ((s, c, gap, str) : rest) = para env x y s c w str : go (y + paraH s w str + gap) rest

-- 画面の下の端にそろえて書く
footer :: Env -> Float -> Float -> Float -> Color -> String -> Picture
footer env x w s c str = para env x (envH env - 14 - paraH s w str) s c w str

-- ---------- 道具：色 ----------
bgC, panelC, plotC, cellC, mint, coral, amber :: Color
bgC    = rgb255 18 14 26
panelC = rgb255 32 26 44
plotC  = rgb255 28 22 38
cellC  = rgb255 40 34 56
mint   = rgb255 78 225 190
coral  = rgb255 255 95 109
amber  = rgb255 255 200 90

-- 0〜255 の灰色
g8 :: Int -> Color
g8 v = rgb255 v v v
