-- ============================================
-- 63-05 Q 学習 ─ 迷路で「マス × 向き」ごとの価値 Q(s, a) を表に覚える
-- 1 歩ごとに Q(s,a) ← Q(s,a) + α (r + γ max_a′ Q(s′,a′) − Q(s,a))。次のマスでは「いちばん良い行動をとる」と仮定して更新するので、
-- 探索のためにでたらめに動いていても、最短の道（最適方策）の価値を学べる（方策オフ）
-- 各マスの 4 つの三角形 = 4 方向の Q の値（明るいほど良い）
-- 操作：F 速さ / スペース 一時停止 / ↑↓ ε / ← → α / R 表を消す / クリック 壁
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad.ST (runST)
import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.Bits (shiftR, xor, (.|.))
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word32)
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

mapRows :: [String]
mapRows =                                -- ★ 迷路（S スタート、G ゴール、# 壁）
  [ "S.....#....."
  , ".####.#.###."
  , ".#....#...#."
  , ".#.####.#.#."
  , ".#......#.#."
  , ".######.#.#."
  , "........#..G"
  , ".######....." ]

gammaQ, stepR :: Double
gammaQ = 0.95                            -- ★ 割引率
stepR = -1                               -- ★ 1 歩の報酬

maxLen :: Int
maxLen = 2000                            -- ★ 1 エピソードの上限

speedNames :: [String]
speedNames = ["slow", "normal", "fast"]

-- Q 学習のエージェント：Q は (マス × 4 + 向き) の表
data QAgent = QAgent
  { qQ     :: !(U.Vector Double)
  , qS     :: !Int
  , qLen   :: !Int
  , qCount :: !Int                       -- 終わったエピソードの数
  , qLens  :: !(Seq.Seq Double)          -- エピソードごとの歩数
  , qLogs  :: !(Seq.Seq Double) }        -- その log10（グラフ用）

data World = World
  { wGW     :: !GW
  , wAgent  :: !QAgent
  , wEps    :: !Double                   -- ★ 探索の割合 ε
  , wLr     :: !Double                   -- ★ 学習率 α
  , wSpeed  :: !Int
  , wPaused :: !Bool
  , wRng    :: !Rng
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "63-05 Q-learning" (world0 []))
  { skBack  = bgC
  , skStart = \_ -> do
      let rs = selfTest
      printSelfTest rs
      pure (world0 rs)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: [(String, Bool)] -> World
world0 rs = World gw0 (makeQAgent gw0) 0.1 0.5 1 False (seedRng 5) rs
  where gw0 = gridParse mapRows

startOf, goalOf :: GW -> Int
startOf gw = maybe 0 id (U.elemIndex 'S' (gwCell gw))
goalOf gw = maybe 0 id (U.elemIndex 'G' (gwCell gw))

makeQAgent :: GW -> QAgent
makeQAgent g = QAgent (U.replicate (gwW g * gwH g * 4) 0) (startOf g) 0 0 Seq.empty Seq.empty

qRow :: U.Vector Double -> Int -> [Double]
qRow q s = U.toList (U.slice (s * 4) 4 q)

-- ★ Q 学習の 1 歩
qStep :: GW -> Double -> Double -> QAgent -> R QAgent
qStep g ep al ag = do
  let s = qS ag
      q = qQ ag
  a <- epsGreedy (qRow q s) ep
  let s2 = gridMove g s a
      done = cellOf g s2 == 'G'
      target = if done then stepR else stepR + gammaQ * maxOf (qRow q s2)   -- ゴールに着いたら先の価値はない
      i = s * 4 + a
      old = q U.! i
      q' = U.modify (\m -> UM.write m i (old + al * (target - old))) q
      len = qLen ag + 1
  pure $ if done || len >= maxLen
    then let l = fromIntegral len
         in ag { qQ = q', qS = startOf g, qLen = 0, qCount = qCount ag + 1
               , qLens = pushCap 100000 l (qLens ag), qLogs = pushCap 100000 (logBase 10 l) (qLogs ag) }
    else ag { qQ = q', qS = s2, qLen = len }

-- 学習した表で、いちばん良い向きだけを選んで進む
greedyPath :: QAgent -> GW -> [Int]
greedyPath ag g = go (0 :: Int) (startOf g) [startOf g]
  where go t s p | t >= 200 || cellOf g s == 'G' = reverse p
                 | otherwise = let s' = gridMove g s (argmaxFirst (qRow (qQ ag) s)) in go (t + 1) s' (s' : p)

learnOnce :: World -> World
learnOnce w = let (ag, r) = runWith (wRng w) (qStep (wGW w) (wEps w) (wLr w) (wAgent w)) in w { wAgent = ag, wRng = r }

step :: Env -> Float -> World -> IO World
step env _ w
  | wPaused w = pure w
  | wSpeed w == 0 = pure (if envFrame env `mod` 4 == 0 then learnOnce w else w)
  | wSpeed w == 1 = pure (learnOnce w)
  | otherwise = sliced 6 (const True) learnOnce w

gx0, gy0 :: Float
gx0 = 40
gy0 = 110

cellSize :: Env -> GW -> Float
cellSize env gw = minimum [56, (envW env * 0.55 - 60) / fromIntegral (gwW gw), (envH env - 220) / fromIntegral (gwH gw)]

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Q-learning: episode " ++ show (qCount ag) ++ (if wPaused w then " (paused)" else "")) 40 42 22 (grayC 240)
  , say ("F speed (" ++ idx "" speedNames (wSpeed w) ++ ")   Space pause   Up/Down eps = " ++ fmtN 2 (wEps w) ++ "   Left/Right alpha = " ++ fmtN 2 (wLr w) ++ "   R clear   Click wall") 40 68 15 (rgbC 170 176 210) ]
  -- ---- Q の三角形
  ++ concatMap cellPic [0 .. gwW gw * gwH gw - 1]
  ++ [ Color pinkC (polyline 3 [ ctr s | s <- gp ])
     , Color cyanC (uncurry circleAt (ctr (qS ag)) (cs * 0.5)) ]
  -- ---- エピソードごとの歩数
  ++ [ plotAxes g "episode" "log10 (steps to goal)" [0, jsRound (x1 g / 2), x1 g] [0, 1, 2, 3] ]
  ++ [ Color (withA (160 / 255) limeC) (lineW 1 (pX g, gY g (logBase 10 shortest)) (pX g + pW g, gY g (logBase 10 shortest))) | not (isInfinite shortest) ]
  ++ [ drawCurve g (qLogs ag) cyanC 2 0
     , say ("Shortest path = " ++ (if isInfinite shortest then "none" else show (round shortest :: Int) ++ " steps (green line)")) (pX g) py 16 (grayC 230)
     , say ("Pink path (greedy policy) = " ++ (if reach then show (length gp - 1) ++ " steps" else "does not reach the goal yet")) (pX g) (py + 26) 16 (grayC 230)
     , say ("Last episode = " ++ (case Seq.viewr (qLens ag) of Seq.EmptyR -> "-"; _ Seq.:> l -> show (round l :: Int) ++ " steps (detours because of eps)")) (pX g) (py + 52) 16 (grayC 230)
     , sayWrap "Q starts at 0. Every step gives -1, so visited cells get darker -> untried directions look bright, and the agent explores them by itself." (pX g) (py + 72) (pW g) 14 (rgbC 170 176 210)
     , drawSelfTest env (wTest w) ]
  where
    gw = wGW w
    ag = wAgent w
    q = qQ ag
    cs = cellSize env gw
    ctr s = let (x, y) = cellXY gw s gx0 gy0 cs in (x + cs / 2, y + cs / 2)
    lo = min (-1) (U.foldl' min 0 q)
    cellPic s =
      let (x, y) = cellXY gw s gx0 gy0 cs
          c = cellOf gw s
          (cx, cy) = (x + cs / 2, y + cs / 2)
          corners = [(x, y), (x + cs, y), (x + cs, y + cs), (x, y + cs)]
          tri a = let p1 = idx (0, 0) corners a
                      p2 = idx (0, 0) corners ((a + 1) `mod` 4)
                      t = 1 - clampv ((q U.! (s * 4 + a)) / lo) 0 1
                  in [ Color (rgbD (30 + 60 * t + 160 * t * t) (34 + 150 * t) (80 + 40 * t)) (Polygon [p1, p2, (cx, cy)])
                     , Color bgC (Line [p1, p2, (cx, cy), p1]) ]
      in case c of
           '#' -> [Color (rgbC 70 76 120) (rectTL (x + 1) (y + 1) (cs - 2) (cs - 2))]
           'G' -> [Color goldC (rectTL (x + 1) (y + 1) (cs - 2) (cs - 2)), sayM "G" cx cy (max 14 (cs * 0.4)) (rgbC 40 30 0)]
           _ -> concatMap tri [0 .. 3] ++ [ sayM "S" cx cy (max 14 (cs * 0.35)) white | c == 'S' ]
    gp = greedyPath ag gw
    reach = cellOf gw (last' gp) == 'G'
    last' xs = idx 0 xs (length xs - 1)
    shortest = bfsDist gw (goalOf gw) U.! startOf gw
    n = qCount ag
    gL = gx0 + fromIntegral (gwW gw) * cs + 60
    g = G gL 120 (envW env - gL - 40) 260 0 (fromIntegral (max 50 n)) 0 3.4
    py = pY g + pH g + 60

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        gw = wGW w
        s = cellAt gw gx0 gy0 (cellSize env gw) mx my
        c = cellOf gw s
    in if s < 0 || c == 'S' || c == 'G' || s == qS (wAgent w) then w
       else w { wGW = gw { gwCell = gwCell gw U.// [(s, if c == '#' then '.' else '#')] } }
  EventKey (Char ch) Down _ _
    | ch `elem` "fF" -> w { wSpeed = (wSpeed w + 1) `mod` 3 }
    | ch `elem` "rR" -> w { wAgent = makeQAgent (wGW w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wEps = min 1 (jsRound ((wEps w + 0.05) * 100) / 100) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wEps = max 0 (jsRound ((wEps w - 0.05) * 100) / 100) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wLr = min 1 (jsRound ((wLr w + 0.1) * 10) / 10) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wLr = max 0.1 (jsRound ((wLr w - 0.1) * 10) / 10) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("maze reaches the goal (shortest " ++ showL ++ " steps, breadth-first search)", not (isInfinite l) && l > 0)
  , ("after 300 episodes the greedy path = the shortest " ++ showL ++ " steps", cellOf g (idx 0 gp (length gp - 1)) == 'G' && fromIntegral (length gp - 1) == l)
  , ("steps go down: mean of the last 20 = " ++ fmtN 1 lastM ++ " < 1/5 of the mean of the first 20", lastM < firstM / 5)
  , ("one update follows the formula: Q(S, right) = 0 + 0.5 x (-1 + 0.95 x 0 - 0) = -0.5", near (qQ toy U.! (startOf g * 4 + 1)) (-0.5) 1e-12)
  , ("Q values are finite (no NaN)", noNaN (U.toList (qQ ag))) ]
  where
    g = gridParse mapRows
    l = bfsDist g (goalOf g) U.! startOf g
    showL = show (round l :: Int)
    ag = withSeed 12 (train (makeQAgent g))
    train a | qCount a >= 300 = pure a
            | otherwise = qStep g 0.1 0.5 a >>= train
    gp = greedyPath ag g
    lens = toList (qLens ag)
    lastM = meanOf (drop (length lens - 20) lens)
    firstM = meanOf (take 20 lens)
    -- 手計算との照合：Q = 0 のマスで右に動いたら Q ← 0 + 0.5 (−1 + 0.95·0 − 0) = −0.5
    toy0 = makeQAgent g
    toy = withSeed 1 (qStep g 0 0.5 toy0 { qQ = qQ toy0 U.// zip [startOf g * 4 ..] [-9, 0, -9, -9] })

-- ---------- やってみよう ----------
-- 1. ε を 0 にすると？ 0.5 にすると？（学習の速さと、直近のエピソードの歩数をくらべる）
-- 2. 学習のあとで壁をクリックして道をふさごう。ピンクの道はどう変わる？ どのくらいで新しい道を見つける？
-- 3. α を 0.1 と 1.0 でくらべよう（迷路は決定的なので α = 1 でもよい。06 の崖では？）
-- 4. stepR を 0 にして、ゴールで +1 だけもらう形にすると？（07 の探索の話につながる）
-- 5. 迷路を自分で作ろう（mapRows を書きかえる）

-- ==================================================
-- ここから下は「強化学習の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。乱数は p5.js 版と同じ mulberry32 で、同じ順に引く
-- ==================================================
-- ---- 色と小さな道具
bgC, goldC, cyanC, pinkC, limeC, violetC :: Color
bgC = rgbC 14 16 34                     -- 背景（アーケードの紺）
goldC = rgbC 255 196 60
cyanC = rgbC 90 215 245
pinkC = rgbC 245 110 170
limeC = rgbC 160 230 110
violetC = rgbC 175 140 255

negInf, posInf :: Double
negInf = -1 / 0
posInf = 1 / 0

-- v を a〜b に（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

-- いちばん大きい値の番号（同点はランダムに 1 つ。乱数を 1 つ使う）
argmaxR :: [Double] -> R Int
argmaxR q = case reverse best of
    [] -> pure 0
    bs -> do u <- rnd; pure (idx 0 bs (floor (u * fromIntegral (length bs))))
  where
    (_, best) = foldl' step (negInf, []) (zip [0 ..] q)
    step (m, b) (i, v)
      | v > m + 1e-12 = (v, [i])
      | abs (v - m) <= 1e-12 = (m, i : b)
      | otherwise = (m, b)

-- 同点は若い番号
argmaxFirst :: [Double] -> Int
argmaxFirst q = fst (foldl' (\(b, bv) (i, v) -> if v > bv then (i, v) else (b, bv)) (0, idx 0 q 0) (zip [0 ..] q))

maxOf :: [Double] -> Double
maxOf = foldl' (\m v -> if v > m then v else m) negInf

-- ε の確率でランダム、ほかは貪欲
epsGreedy :: [Double] -> Double -> R Int
epsGreedy q eps = do
  u <- rnd
  if u < eps then (\v -> floor (v * fromIntegral (length q))) <$> rnd else argmaxR q

-- ボルツマン分布 exp(h/τ) / Σ（大きい数でもあふれないように最大値を引く）
softmaxArr :: [Double] -> Double -> [Double]
softmaxArr h tau = map (/ s) e
  where
    m = maxOf h
    t = if tau == 0 || isNaN tau then 1 else tau
    e = map (\v -> exp ((v - m) / t)) h
    s = foldl' (+) 0 e

sampleFrom :: [Double] -> R Int
sampleFrom p = do
  u <- rnd
  let go _ _ [] = length p - 1
      go i c (x : xs) = let c' = c + x in if u < c' then i else go (i + 1) c' xs
  pure (go 0 0 p)

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else foldl' (+) 0 a / fromIntegral (length a)

-- 小数点以下 d 桁（toFixed）。−0.00 は 0.00 に、NaN は ?
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let s = showFFloat (Just d) v ""
                in if all (`elem` "-0.") s && take 1 s == "-" then drop 1 s else s

-- リストの i 番目（なければ d）
idx :: a -> [a] -> Int -> a
idx d xs i = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d
-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態はすぐに計算する（正格）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- 種 s で始めて結果だけ取り出す（seedRng(s) してから計算）
withSeed :: Word32 -> R a -> a
withSeed s m = fst (runR m (seedRng s))

-- 今の乱数の状態で計算して、(結果, 次の状態)
runWith :: Rng -> R a -> (a, Rng)
runWith s m = runR m s

rnd :: R Double
rnd = R (\(Rng s) -> case rndStep (Rng s) of (u, s') -> (u, s'))

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

randn :: R Double                             -- 標準正規分布（ボックス-ミュラー）
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))
-- ---- グラフの道具（左上が原点、y は下向き）：G 左 上 幅 高さ x0 x1 y0 y1
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv ((v - x0 g) / (x1 g - x0 g)) (-0.02) 1.02) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv ((v - y0 g) / (y1 g - y0 g)) (-0.02) 1.02) * pH g

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgbC 70 76 110) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 18) 14 (rgbC 150 156 190) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (rgbC 150 156 190) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 38) 14 (rgbC 205 210 235), say yl (pX g) (pY g - 8) 14 (rgbC 205 210 235) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- 直前 k 個の平均（ギザギザの学習曲線をならす）
smoothArr :: Seq.Seq Double -> Int -> Seq.Seq Double
smoothArr a k = Seq.fromList (go 0 0 (toList a))
  where
    arr = a
    go _ _ [] = []
    go !i !s (v : vs) = let s1 = s + v - (if i >= k then Seq.index arr (i - k) else 0)
                        in s1 / fromIntegral (min (i + 1) k) : go (i + 1) s1 vs

-- arr の i 番目を x = i + xOff に。長いときは間引いて 600 点まで（色 col、太さ sw）
drawCurve :: G -> Seq.Seq Double -> Color -> Float -> Double -> Picture
drawCurve g arr col sw xOff = Color col (polyline sw pts)
  where
    n = Seq.length arr
    stride = max 1 ((n + 599) `div` 600)
    pt i = (gX g (fromIntegral i + xOff), gY g (Seq.index arr i))
    pts = [ pt i | i <- [0, stride .. n - 1] ] ++ [ pt (n - 1) | n > 1, (n - 1) `mod` stride /= 0 ]

-- 色の見本と名前（items = [(色, 名前)]）
legend :: Float -> Float -> [(Color, String)] -> Picture
legend x y items = Pictures (concat [ [Color c (rectTL x (yy - 5) 16 10), say s (x + 22) (yy + 5) 14 (rgbC 220 225 245)]
                                    | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * fromIntegral k ])

-- 長さ cap までの列に追加（古いものから捨てる。メモリを増やし続けない）
pushCap :: Int -> Double -> Seq.Seq Double -> Seq.Seq Double
pushCap cap !v s = let s' = s Seq.|> v in if Seq.length s' > cap then Seq.drop 1 s' else s'
-- ---- グリッドワールド：文字の地図 '.' 床、'#' 壁、'S' スタート、'G' 宝（+1 で終わり）、'P' 落とし穴（−1 で終わり）
dirs :: [(Int, Int)]
dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)]       -- 行動 0 上、1 右、2 下、3 左

dirNames :: [String]
dirNames = ["up", "right", "down", "left"]

data GW = GW { gwW, gwH :: !Int, gwCell :: !(U.Vector Char) }

gridParse :: [String] -> GW
gridParse rows = GW (length (headOr "" rows)) (length rows) (U.fromList (concat rows))

cellOf :: GW -> Int -> Char
cellOf gw s = if s >= 0 && s < U.length (gwCell gw) then gwCell gw U.! s else '#'

-- 決まった向きに 1 マス。外や壁ならその場にとどまる
gridMove :: GW -> Int -> Int -> Int
gridMove gw s a =
  let (dx, dy) = idx (0, 0) dirs a
      x = s `mod` gwW gw + dx
      y = s `div` gwW gw + dy
  in if x < 0 || y < 0 || x >= gwW gw || y >= gwH gw || cellOf gw (y * gwW gw + x) == '#' then s else y * gwW gw + x

isTerminalCell :: Char -> Bool
isTerminalCell c = c == 'G' || c == 'P'

enterReward :: Char -> Double -> Double
enterReward c stepR = if c == 'G' then 1 else if c == 'P' then -1 else stepR

-- 遷移 (次の状態 s2, 確率 p, 報酬 r)
data Tr = Tr { trS2 :: !Int, trP :: !Double, trR :: !Double }

-- マルコフ決定過程：mP V.! s V.! a = [遷移]。意図した向きに 1 − slip、左右の直角の向きに slip/2 ずつ
data Mdp = Mdp { mdpNS :: !Int, mdpP :: !(V.Vector (V.Vector [Tr])), mdpTerm :: !(U.Vector Bool), mdpGW :: !GW }

buildMdp :: GW -> Double -> Double -> Mdp
buildMdp gw slip stepR = Mdp nS (V.generate nS row) term gw
  where
    nS = gwW gw * gwH gw
    term = U.generate nS (\s -> let c = cellOf gw s in isTerminalCell c || c == '#')
    row s = V.generate 4 (\a -> if term U.! s then [Tr s 1 0] else outs s a)   -- 終わりのマス（と壁）はそこに留まり、報酬 0
    outs s a = foldl' add [] [(a, 1 - slip), ((a + 1) `mod` 4, slip / 2), ((a + 3) `mod` 4, slip / 2)]
      where add list (d, p)
              | p <= 0 = list
              | otherwise = let s2 = gridMove gw s d
                            in if any ((== s2) . trS2) list then [ if trS2 o == s2 then o { trP = trP o + p } else o | o <- list ]
                               else list ++ [Tr s2 p (enterReward (cellOf gw s2) stepR)]

trans :: Mdp -> Int -> Int -> [Tr]
trans mdp s a = (mdpP mdp V.! s) V.! a

isTerm :: Mdp -> Int -> Bool
isTerm mdp s = mdpTerm mdp U.! s

qValue :: Mdp -> U.Vector Double -> Double -> Int -> Int -> Double
qValue mdp v gamma s a = foldl' (\q o -> q + trP o * (trR o + gamma * (v U.! trS2 o))) 0 (trans mdp s a)

-- 価値反復の 1 回：V(s) ← max_a Σ p (r + γ V(s'))。(新しい V, 最大の変化量)
viSweep :: Mdp -> U.Vector Double -> Double -> (U.Vector Double, Double)
viSweep mdp v gamma = (nv, U.maximum (U.cons 0 (U.zipWith (\a b -> abs (a - b)) nv v)))
  where nv = U.generate (mdpNS mdp) (\s -> if isTerm mdp s then v U.! s else maximum' [ qValue mdp v gamma s a | a <- [0 .. 3] ])
        maximum' = foldl' max negInf

-- (V, 回数, 毎回の変化量)
valueIteration :: Mdp -> Double -> Double -> Int -> (U.Vector Double, Int, [Double])
valueIteration mdp gamma tol maxIt = go (U.replicate (mdpNS mdp) 0) 0 posInf []
  where
    go v its d ds
      | d >= tol && its < maxIt = let (v', d') = viSweep mdp v gamma in go v' (its + 1) d' (d' : ds)
      | otherwise = (v, its, reverse ds)

-- 最適な行動の集合（同点は全部）
optimalActs :: Mdp -> U.Vector Double -> Double -> Int -> Double -> [Int]
optimalActs mdp v gamma s tol = [ a | (a, q) <- zip [0 ..] qs, q >= m - tol ]
  where qs = [ qValue mdp v gamma s a | a <- [0 .. 3] ]
        m = maxOf qs

-- 方策評価：V(s) ← Σ p (r + γ V(s')) を、変化が tol 未満になるまで（その場で書きかえる）。(V, 何回まわしたか)
evalPolicy :: Mdp -> U.Vector Int -> Double -> Double -> U.Vector Double -> (U.Vector Double, Int)
evalPolicy mdp pol gamma tol v0 = runST $ do
  v <- U.thaw v0
  let qv s a = foldM (\q o -> do x <- UM.read v (trS2 o); pure (q + trP o * (trR o + gamma * x))) 0 (trans mdp s a)
      sweep = foldM (\d s -> if isTerm mdp s then pure d else do
                        x <- qv s (pol U.! s)
                        old <- UM.read v s
                        UM.write v s x
                        pure (max d (abs (x - old)))) 0 [0 .. mdpNS mdp - 1]
      loop !n d | d >= tol && n < 100000 = do d' <- sweep; loop (n + 1) d'
                | otherwise = pure n
  n <- loop (0 :: Int) posInf
  v' <- U.freeze v
  pure (v', n)

-- マスの左上（p5 の座標）
cellXY :: GW -> Int -> Float -> Float -> Float -> (Float, Float)
cellXY gw s gx gy cs = (gx + fromIntegral (s `mod` gwW gw) * cs, gy + fromIntegral (s `div` gwW gw) * cs)

-- マスの中心から向き a に矢印（太さ wt）
drawArrowIn :: Float -> Float -> Int -> Float -> Float -> Picture
drawArrowIn cx cy a len wt = Pictures
  [ thickLine wt (cx - dx * len * 0.4, cy - dy * len * 0.4) (ex, ey)
  , Polygon [(ex + dx * 5, ey + dy * 5), (ex - dy * 5 - dx * 2, ey + dx * 5 - dy * 2), (ex + dy * 5 - dx * 2, ey - dx * 5 - dy * 2)] ]
  where
    (ix', iy') = idx (0, 0) dirs a
    dx = fromIntegral ix'
    dy = fromIntegral iy'
    ex = cx + dx * len
    ey = cy + dy * len

-- 壁・宝（G）・落とし穴（P）・スタート（S）を描く
drawCells :: GW -> Float -> Float -> Float -> Bool -> Picture
drawCells gw gx gy cs skipFloor = Pictures (concatMap one [0 .. gwW gw * gwH gw - 1])
  where
    ts = max 14 (cs * 0.4)
    one s =
      let c = cellOf gw s
          (x, y) = cellXY gw s gx gy cs
          bgc | c == '#' = rgbC 70 76 120
              | c == 'G' = goldC
              | c == 'P' = rgbC 120 30 70
              | otherwise = rgbC 26 30 60
          mid col str = [sayM str (x + cs / 2) (y + cs / 2) ts col]
      in if c == '.' && skipFloor then [] else
           [Color bgc (rectTL (x + 1) (y + 1) (cs - 2) (cs - 2))]
           ++ (if c == 'G' then mid (rgbC 40 30 0) "G" else [])
           ++ (if c == 'P' then mid (rgbC 255 150 190) "P" else [])
           ++ (if c == 'S' then mid cyanC "S" else [])

-- マウスの下のマス（なければ −1）
cellAt :: GW -> Float -> Float -> Float -> Float -> Float -> Int
cellAt gw gx gy cs mx my =
  let x = floor ((mx - gx) / cs)
      y = floor ((my - gy) / cs)
  in if x >= 0 && y >= 0 && x < gwW gw && y < gwH gw then y * gwW gw + x else -1

-- 決定的に動くときの、各マスからゴールまでの最短の歩数（届かなければ posInf）
bfsDist :: GW -> Int -> U.Vector Double
bfsDist gw goal = go (U.replicate n posInf U.// [(goal, 0)]) (Seq.singleton goal)
  where
    n = gwW gw * gwH gw
    go d q = case Seq.viewl q of
      Seq.EmptyL -> d
      s Seq.:< rest ->
        let step (dd, qq) a =
              let (ddx, ddy) = idx (0, 0) dirs a
                  x = s `mod` gwW gw - ddx
                  y = s `div` gwW gw - ddy
                  p = y * gwW gw + x            -- p から向き a で s に来られる？
              in if x < 0 || y < 0 || x >= gwW gw || y >= gwH gw || cellOf gw p == '#' || dd U.! p < posInf || isTerminalCell (cellOf gw p)
                   then (dd, qq)
                   else if gridMove gw p a == s then (dd U.// [(p, dd U.! s + 1)], qq Seq.|> p) else (dd, qq)
            (d', q') = foldl' step (d, rest) [0 .. 3]
        in go d' q'

-- 低い = 紫、0 = 紺、高い = 金
valueFill :: Double -> Double -> Double -> Color
valueFill v lo hi =
  let rng' = if hi - lo == 0 then 1 else hi - lo
      t = clampv ((v - lo) / rng') 0 1
  in if t < 0.5 then let u = t / 0.5 in rgbD (110 - 80 * u) (60 - 30 * u) (150 - 90 * u)
     else let u = (t - 0.5) / 0.5 in rgbD (30 + 220 * u) (30 + 160 * u) (60 - 20 * u)

rgbD :: Double -> Double -> Double -> Color
rgbD r g b = makeColor (f2 r / 255) (f2 g / 255) (f2 b / 255) 1
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 maxW で折り返す文字（左上 = (x, y)）
sayWrap :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayWrap s x y maxW size c = Pictures [ say l x (y + size + fromIntegral k * size * 1.3) size c | (k, l) <- zip [0 :: Int ..] (wrapText maxW size s) ]

wrapText :: Float -> Float -> String -> [String]
wrapText maxW size s = go (words s) ""
  where
    fits t = textW t size <= maxW
    go [] cur = [cur | not (null cur)]
    go (w : ws) "" = go ws w
    go (w : ws) cur = let t = cur ++ " " ++ w in if fits t then go ws t else cur : go (w : ws) ""

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が (x, y) の長方形の中にあるか
inBox :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
inBox (mx, my) (x, y, w, h) = mx > x && mx < x + w && my > y && my < y + h
-- ---- 1 フレームの計算を budget ミリ秒までにする（p5 の do { … } while (performance.now() − t0 < budget && more)）
--      step は必ず 1 回は動く。速さ F は budget を変える
sliced :: Double -> (w -> Bool) -> (w -> w) -> w -> IO w
sliced budget more step w0 = do
  t0 <- getMonotonicTime
  let go w = do
        w' <- evaluate (step w)
        t <- getMonotonicTime
        if (t - t0) * 1000 < budget && more w' then go w' else pure w'
  go w0
-- ---- 数の道具
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt = fmtN

-- 有効数字 n 桁に丸めた数（toPrecision）
roundSig :: Int -> Double -> Double
roundSig n v0'
  | v == 0 = 0
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (n - 1 - e) :: Double
                in fromIntegral (round (v * k) :: Integer) / k
  where v = fin 0 v0'

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v = let r = roundSig 3 v
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (jsRoundI v)

jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e9) 1e9)

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d
-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- NaN も ∞ もない
noNaN :: [Double] -> Bool
noNaN = all (\v -> not (isNaN v || isInfinite v))

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 画面の右下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 225 200 else rgbC 255 90 120)
