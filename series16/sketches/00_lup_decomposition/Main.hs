-- ============================================
-- 00 LUP 分解で連立方程式を解く
-- 行列 A を「入れかえ P」「下三角 L」「上三角 U」に分けて（PA = LU）、
-- 前進代入 → 後退代入の 2 回の簡単な計算で Ax = b を解く
-- スペース：1 ステップ進む / A：自動再生 / R：やり直し（2 は新しい乱数）
-- 1：お手本の 3×3  2：ランダム 4×4  3：特異行列（解けない）  4：ピボットが必要な 4×4
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (digitToInt, toLower)
import Numeric (showEFloat, showFFloat)

speed :: Int
speed = 45 -- ★ 自動再生の間隔（フレーム）。小さいほど速い

-- いま光らせている場所
data Hi
  = HNone
  | HCol !Int !Bool     -- 列を調べている（True なら全部 0）
  | HPivot !Int !Int    -- 列 k のピボットは行 p
  | HSwap !Int !Int
  | HL !Int
  | HSchur !Int
  | HFwd !Int
  | HBack !Int
  deriving Eq

-- 計算の状態（1 ステップごとに 1 つできる）
data S = S
  { sM     :: ![[Double]]      -- 作業行列（L と U を 1 枚に重ねて書く）
  , sPi    :: ![Int]           -- 置換 π
  , sY     :: ![Maybe Double]
  , sX     :: ![Maybe Double]
  , sDoneK :: !Int
  , sHi    :: !Hi
  , sMsg   :: !String
  , sFin   :: !Bool
  }

data World = World
  { wB      :: ![Double]
  , wS      :: !S
  , wRest   :: [S]                     -- これから来る状態
  , wAuto   :: !Bool
  , wTimer  :: !Int
  , wSwap   :: !(Maybe (Int, Int, Float)) -- 行の入れかえアニメ
  , wPreset :: !Int
  }

main :: IO ()
main = runSketch (sketch "16-00 LUP Decomposition" (makeWorld 1 a1 b1))
  { skBack  = bgC
  , skStart = \_ -> load 1
  , skStep  = \_ _ w -> pure (stepW w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

a1, a3, a4 :: [[Double]]
a1 = [[2, 4, -2], [1, 3, 0], [3, 1, 4]]                                -- 答えは x = (1, 2, -1)
a3 = [[1, 2, 3], [2, 4, 6], [1, 0, 1]]
a4 = [[0, 2, 1, 3], [4, 1, -2, 1], [2, 3, 1, -1], [-2, 1, 3, 2]]       -- 左上が 0。答えは x = (1, -1, 2, 1)

b1, b3, b4 :: [Double]
b1 = [12, 7, 1]
b3 = [1, 2, 3]
b4 = [3, 0, 0, 5]

load :: Int -> IO World
load k = case k of
  1 -> pure (makeWorld 1 a1 b1)
  3 -> pure (makeWorld 3 a3 b3)
  4 -> pure (makeWorld 4 a4 b4)
  _ -> do
    a <- replicateM 4 (replicateM 4 (fromIntegral <$> randomRIO (-6, 6 :: Int)))
    b <- replicateM 4 (fromIntegral <$> randomRIO (-9, 9 :: Int))
    pure (makeWorld 2 a b)

makeWorld :: Int -> [[Double]] -> [Double] -> World
makeWorld k a b =
  let n = length a
      s0 = S a [0 .. n - 1] (replicate n Nothing) (replicate n Nothing) 0 HNone
             "Space: one step.  A: auto play." False
  in World b s0 (steps (lupSolve a b) s0) False 0 Nothing k

-- LUP 分解 → 前進代入 → 後退代入 を 1 ステップずつ止まりながら実行する
lupSolve :: [[Double]] -> [Double] -> Gen S ()
lupSolve a0 b0 = decomp 0
  where
    n = length a0
    decomp k
      | k >= n = forward 0
      | otherwise = do
          m <- gets sM
          let (best, p) = foldl (\(b, q) i -> let v = abs (m !! i !! k) in if v > b then (v, i) else (b, q))
                                (0, k) [k .. n - 1]
          if best < 1e-9
            then do
              modify (\s -> s { sHi = HCol k True, sFin = True
                              , sMsg = "Column " ++ show (k + 1) ++ " is all 0 below the diagonal: singular. No inverse, no unique solution." })
              yield
            else do
              modify (\s -> s { sHi = HPivot k p
                              , sMsg = "Pick the largest absolute value " ++ fmt (m !! p !! k) ++ " in column " ++ show (k + 1)
                                       ++ " as pivot (never divide by 0, less rounding error)." })
              yield
              if p /= k
                then do
                  modify (\s -> s { sM = swapL k p (sM s), sPi = swapL k p (sPi s), sHi = HSwap k p
                                  , sMsg = "Swap rows " ++ show (k + 1) ++ " and " ++ show (p + 1) ++ ". The record of swaps is pi (the permutation)." })
                  yield
                else pure ()
              modify (\s -> s { sM = divideCol k (sM s), sHi = HL k
                              , sMsg = "Divide the numbers below the pivot by the pivot: this is a column of L (how many times to subtract)." })
              yield
              modify (\s -> s { sM = schur k (sM s), sDoneK = k + 1, sHi = HSchur k
                              , sMsg = "Subtract l x pivot row from the lower right block (Schur complement). Row "
                                       ++ show (k + 1) ++ " is now fixed as a row of U." })
              yield
              decomp (k + 1)
    forward i
      | i >= n = backward (n - 1)
      | otherwise = do
          s0 <- gets id
          let row = sM s0 !! i
              pii = sPi s0 !! i
              v = b0 !! pii - sum [row !! j * val (sY s0 !! j) | j <- [0 .. i - 1]]
          modify (\s -> s { sY = setAt i (Just v) (sY s), sHi = HFwd i
                          , sMsg = "Forward: solve Ly = Pb from the top. y" ++ show (i + 1) ++ " = b" ++ show (pii + 1)
                                   ++ " - sum(l*y) = " ++ fmt v })
          yield
          forward (i + 1)
    backward i
      | i < 0 = finish
      | otherwise = do
          s0 <- gets id
          let row = sM s0 !! i
              d = row !! i
              v = (val (sY s0 !! i) - sum [row !! j * val (sX s0 !! j) | j <- [i + 1 .. n - 1]]) / d
          modify (\s -> s { sX = setAt i (Just v) (sX s), sHi = HBack i
                          , sMsg = "Backward: solve Ux = y from the bottom. x" ++ show (i + 1) ++ " = (y" ++ show (i + 1)
                                   ++ " - sum(u*x)) / " ++ fmt d ++ " = " ++ fmt v })
          yield
          backward (i - 1)
    finish = do
      xs <- gets (map val . sX)
      let err = foldl max 0 [abs (sum (zipWith (*) r xs) - b) | (r, b) <- zip a0 b0]
      modify (\s -> s { sHi = HNone, sFin = True
                      , sMsg = "Done! Check: max |Ax - b| = " ++ showEFloat (Just 1) err ""
                               ++ ". Factoring is Theta(n^3), solving Theta(n^2): a new b is fast." })
      yield

val :: Maybe Double -> Double
val = maybe 0 id

swapL :: Int -> Int -> [a] -> [a]
swapL a b xs = [pick i x | (i, x) <- zip [0 ..] xs]
  where pick i x | i == a = xs !! b
                 | i == b = xs !! a
                 | otherwise = x

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if j == i then v else x | (j, x) <- zip [0 ..] xs]

divideCol :: Int -> [[Double]] -> [[Double]]
divideCol k m =
  let pv = m !! k !! k
  in [ if i > k then setAt k (r !! k / pv) r else r | (i, r) <- zip [0 ..] m ]

schur :: Int -> [[Double]] -> [[Double]]
schur k m =
  let top = m !! k
  in [ if i > k then [ if j > k then v - (r !! k) * (top !! j) else v | (j, v) <- zip [0 ..] r ] else r
     | (i, r) <- zip [0 ..] m ]

advance :: World -> World
advance w
  | sFin (wS w) = w
  | otherwise = case wRest w of
      [] -> w { wS = (wS w) { sFin = True } }
      (s : r) -> w { wS = s, wRest = r
                   , wSwap = case sHi s of
                       HSwap a b -> Just (a, b, 0)
                       _ -> wSwap w }

stepW :: World -> World
stepW w0 =
  let timer = if wAuto w0 && not (sFin (wS w0)) then wTimer w0 + 1 else wTimer w0
      w1 = w0 { wTimer = timer }
      w2 = if wAuto w0 && not (sFin (wS w0)) && timer `mod` speed == 0 then advance w1 else w1
      sw = case wSwap w2 of
        Just (a, b, t) | t + 0.06 < 1 -> Just (a, b, t + 0.06)
        _ -> Nothing
  in w2 { wSwap = sw }

drawW :: Env -> World -> Picture
drawW env w =
  let s = wS w
      n = length (sM s)
      nf = fromIntegral n :: Float
      cs = min 88 (min ((envH env - 330) / nf) (envW env * 0.5 / (nf + 1)))
      ox = 70
      oy = 150
      doneK = sDoneK s
      hi = sHi s
      -- 入れかえアニメ中の行の位置
      rowY i = case wSwap w of
        Just (a, b, t)
          | i == a -> lerpF (fromIntegral b) (fromIntegral a) (ease t)
          | i == b -> lerpF (fromIntegral a) (fromIntegral b) (ease t)
        _ -> fromIntegral i
      cellPic i j v =
        let px = ox + fromIntegral j * cs
            py = oy + rowY i * cs
            base | j < i && j < doneK = rgb255 30 90 80
                 | j >= i && i < doneK = rgb255 110 40 55
                 | otherwise = cellC
            c = case hi of
              HL k | j == k && i > k -> mint
              HSchur k | i > k && j > k -> rgba255 255 200 90 160
              _ -> base
            colMark = case hi of
              HCol k bad | j == k && i >= k -> frame env (if bad then rgb255 255 60 60 else amber) 3 (px + 3) (py + 3) (cs - 6) (cs - 6)
              HPivot k _ | j == k && i >= k -> frame env amber 3 (px + 3) (py + 3) (cs - 6) (cs - 6)
              _ -> Blank
            pivotMark = case hi of
              HPivot k p | i == p && j == k -> frame env white 5 (px + 2) (py + 2) (cs - 4) (cs - 4)
              _ -> Blank
        in Pictures [ box env c (px + 1.5) (py + 1.5) (cs - 3) (cs - 3), colMark, pivotMark
                    , txtC env (px + cs / 2) (py + cs / 2) (max 14 (cs * 0.26)) (g8 240) (fmt v) ]
      vx = ox + nf * cs + 40
      cols = [ ("pi", map (Just . fromIntegral . (+ 1)) (sPi s), const False)
             , ("b", map Just (wB w), const False)
             , ("y", sY s, \i -> hi == HFwd i)
             , ("x", sX s, \i -> hi == HBack i) ] :: [(String, [Maybe Double], Int -> Bool)]
      colPic (c, (name, arr, on)) =
        let px = vx + fromIntegral (c :: Int) * (cs * 0.95)
            one (i, v) =
              let py = oy + fromIntegral (i :: Int) * cs
              in Pictures [ box env (if on i then amber else panelC) (px + 1.5) (py + 1.5) (cs * 0.8 - 3) (cs - 3)
                          , txtC env (px + cs * 0.4) (py + cs / 2) (max 14 (cs * 0.24)) (if on i then g8 20 else g8 230) (maybe "" fmt v) ]
        in Pictures (txtC env (px + cs * 0.4) (oy - 18) 20 (g8 200) name : map one (zip [0 ..] arr))
      my = oy + nf * cs + 40
  in Pictures
       [ txt env 30 50 26 (g8 235) "LUP decomposition: solve Ax = b"
       , txt env 30 80 15 (g8 150) ("Space: 1 step  A: auto" ++ (if wAuto w then " (on)" else "")
                                     ++ "  R: redo  1: 3x3 sample  2: random 4x4  3: singular  4: needs pivot")
       , txt env 30 115 15 mint "L (lower triangle, diagonal 1)"
       , txt env 390 115 15 coral "U (upper triangle)"
       , txt env 590 115 15 (g8 150) "Still being computed"
       , Pictures [cellPic i j v | (i, r) <- zip [0 ..] (sM s), (j, v) <- zip [0 ..] r]
       , Pictures (map colPic (zip [0 ..] cols))
       , box env panelC 30 my (envW env - 60) 70
       , para env 50 (my + 10) 16 (if sFin s then mint else g8 240) (envW env - 100) (sMsg s)
       , para env 30 (my + 90) 15 (g8 150) (envW env - 60)
           "Why factor? L and U are triangles, so plain substitution solves them row by row. The heavy factoring (Theta(n^3)) is done once; each new right side b costs only Theta(n^2).\nWithout partial pivoting you may divide by 0, or by a tiny number that blows up the error."
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (advance w)
  EventKey (Char c0) Down _ _ -> case toLower c0 of
    'a' -> pure w { wAuto = not (wAuto w) }
    'r' -> keepAuto <$> load (wPreset w)
    c | c `elem` "1234" -> keepAuto <$> load (digitToInt c)
    _ -> pure w
  _ -> pure w
  where keepAuto w' = w' { wAuto = wAuto w, wTimer = wTimer w }

-- ---------- やってみよう ----------
-- 1. 4 のお手本で、ピボットを「絶対値最大」ではなく「その行のまま」にすると何が起きる？（0 で割る）
-- 2. 2 のランダムを何回か出して、π（入れかえの記録）がどう変わるか見る
-- 3. b4 だけ変えて R。分解のステップは同じで、代入だけが変わることを確かめる

-- ---------- 道具 ----------
-- 整数なら小数点なしで書く
fmt :: Double -> String
fmt v
  | abs (v - fromIntegral (rnd v)) < 1e-9 = show (rnd v)
  | otherwise = fixed 2 v

ease :: Float -> Float
ease t = t * t * (3 - 2 * t)

-- ---------- 道具：数の表示 ----------
-- JavaScript の toFixed
fixed :: Int -> Double -> String
fixed d v = showFFloat (Just d) v ""

-- 四捨五入（JavaScript の Math.round）
rnd :: Double -> Int
rnd v = floor (v + 0.5)

f2 :: Double -> Float
f2 = realToFrac

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

-- ---------- 道具：1 手ずつ止まる計算（p5.js の function* の代わり） ----------
-- yield のたびに状態を 1 つ流す計算。結果は「状態の遅延リスト」になり、
-- 画面側はリストを 1 つずつ取り出して進める。
newtype Gen s a = Gen { unGen :: s -> (a -> s -> [s]) -> [s] }

instance Functor (Gen s) where
  fmap f (Gen g) = Gen (\s k -> g s (k . f))

instance Applicative (Gen s) where
  pure x = Gen (\s k -> k x s)
  Gen f <*> Gen g = Gen (\s k -> f s (\h s' -> g s' (k . h)))

instance Monad (Gen s) where
  Gen g >>= f = Gen (\s k -> g s (\x s' -> unGen (f x) s' k))

yield :: Gen s ()
yield = Gen (\s k -> s : k () s)

gets :: (s -> a) -> Gen s a
gets f = Gen (\s k -> k (f s) s)

modify :: (s -> s) -> Gen s ()
modify f = Gen (\s k -> k () $! f s)

steps :: Gen s () -> s -> [s]
steps (Gen g) s0 = g s0 (\_ _ -> [])
