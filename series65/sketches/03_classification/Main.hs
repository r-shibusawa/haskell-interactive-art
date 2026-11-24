-- ============================================
-- 65-03 状態の分類 ─ 自分で遷移図を作ると、状態を自動で分類して色分けする
-- 通じ合うクラス：i から j へも、j から i へも行ける状態のまとまり（強連結成分、タージャンの方法で求める）
-- 閉じたクラス：外へ出る矢印がない（一度入ると出られない）。1 つだけで自分に戻るなら吸収状態
-- 周期：戻ってこられる歩数の最大公約数（gcd）。周期 2 だと分布が行ったり来たりして収束しない
-- 既約（クラスが 1 つ）で周期 1 なら、どこから始めても定常分布へ（01・02 の場合）
-- 操作：空いている所をクリック 状態を追加 / 状態から状態へドラッグ 矢印をつける・消す / 状態をクリック 自分への矢印
--       1 周期 2 の例　2 吸収状態のある例　3 既約で周期 1　4 周期 3　C 全部消す　（矢印の確率は、出る矢印に均等）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (find, foldl')
import qualified Data.Set as S
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

maxStates :: Int
maxStates = 8                                 -- ★ 状態の数の上限

nodeR :: Float
nodeR = 26

data World = World
  { wPos   :: [(Float, Float)]                -- 状態の場所
  , wEdges :: Mat                             -- 矢印があれば 1
  , wP     :: Mat                             -- 遷移行列
  , wInfo  :: ChainInfo                       -- 分類の結果
  , wDragFrom :: !Int                         -- ドラッグを始めた状態（なければ −1）
  , wPress :: !(Float, Float)
  , wHist  :: [Vec]                           -- 状態 0 から始めた分布 μ_0 … μ_40
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "65-03 Classifying states" world0)
  { skBack  = col bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (preset env 1 world0 { wTest = selfTest })
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World [] [] [] (ChainInfo [] [] False) (-1) (0, 0) [] []

-- ★ 例の遷移図（状態の数と、矢印のリスト）
presetList :: Int -> (Int, [(Int, Int)])
presetList k = case k of
  1 -> (4, [(0, 1), (1, 2), (2, 3), (3, 0), (1, 0), (3, 2)])                          -- 偶数歩でしか戻れない → 周期 2
  2 -> (5, [(1, 0), (1, 2), (2, 1), (2, 3), (3, 2), (3, 4), (0, 0), (4, 4)])          -- 0 と 4 が吸収状態（ギャンブラーの破産、04）
  3 -> (5, [(0, 1), (1, 2), (2, 3), (3, 4), (4, 0), (2, 0), (4, 4)])                  -- 長さ 5 と 3 の輪 + 自分への矢印 → 周期 1
  _ -> (6, [(0, 1), (1, 2), (2, 0), (2, 3), (3, 4), (4, 5), (5, 3), (5, 0)])          -- どの輪も長さ 3 の倍数 → 周期 3

preset :: Env -> Int -> World -> World
preset env k w =
  let ww = envW env * 0.55
      hh = envH env - 200
      cx = 40 + ww / 2
      cy = 110 + hh / 2
      (n, list) = presetList k
      r = min ww hh * 0.36
  in rebuild w { wPos = [ (cx + r * cos t, cy + r * sin t) | i <- [0 .. n - 1], let t = -pi / 2 + i2f i * 2 * pi / i2f n ]
               , wEdges = fromList n list }

fromList :: Int -> [(Int, Int)] -> Mat
fromList n list = [ [ if (i, j) `elem` list then 1 else 0 | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]

-- ★ 矢印から遷移行列：出る矢印に均等な確率。矢印がない状態は「その場にとどまる」（吸収状態）
edgesToP :: Mat -> Mat
edgesToP = fixRows

rebuild :: World -> World
rebuild w =
  let p = edgesToP (wEdges w)
      n = length (wPos w)
      mu = [ if i == 0 then 1 else 0 | i <- [0 .. n - 1] ]
  in w { wP = p, wInfo = classifyChain p, wHist = take 41 (iterate (`vecMat` p) mu) }

nodeAt :: World -> (Float, Float) -> Int
nodeAt w (x, y) = maybe (-1) fst (find (\(_, (px, py)) -> hyp (x - px) (y - py) < nodeR + 6) (zip [0 ..] (wPos w)))

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Classifying states: " ++ show n ++ " states, " ++ show (length classes) ++ (if length classes == 1 then " class" else " classes") ++ (if ciIrreducible info then " (irreducible)" else ""))
             "Click empty space: add state   drag state -> state: arrow   click a state: self arrow   1-4 examples   C clear"
  , Color (col (60, 54, 50)) (frameTL 1 40 96 ww (envH env - 150)) ]
  ++ (if n > 0 then
        [ fst (drawChainGraph (wPos w) (wP w) nodeR (\i -> stateCol (at' (ciComp info) i 0)) defOpts { goLabels = n <= 6 })
        , Pictures [ sayT tag x (y + nodeR + 7) 14 (col sub) True
                   | (i, (x, y)) <- zip [0 ..] (wPos w)
                   , let c = at' classes (at' (ciComp info) i 0) (ChainClass [] False 0 False)
                   , let tag = if ccAbsorbing c then "absorbing" else if ccClosed c then "closed" else "transient" ] ]
      else [])
  ++ [ if wDragFrom w >= 0 then Color (col mustard) (lineW 2 (at' (wPos w) (wDragFrom w) (0, 0)) (mouseP env)) else Blank
  -- ---- 分類の結果
  , say "Classes (states that reach each other)" x0 y0 16 (col cream)
  , Pictures [ Pictures [ Color (col (stateCol k)) (rectTL x0 (yy - 12) 16 16)
                        , say ("{" ++ commaList (map show (ccMembers c)) ++ "}  " ++ kind c ++ "  " ++ per c) (x0 + 24) yy 14 (col cream) ]
             | (k, c) <- zip [0 :: Int ..] classes, let yy = y0 + 26 + i2f k * 26 ]
  , if n > 0
      then say (if ciIrreducible info
                  then (if ccPeriod (headOr (ChainClass [] False 0 False) classes) == 1 then "Irreducible, period 1: converges to pi"
                        else "Irreducible but period " ++ show (ccPeriod (headOr (ChainClass [] False 0 False) classes)) ++ ": keeps cycling")
                  else "Not irreducible: the end depends on the start") x0 vy 15 (col mustard)
      else Blank
  -- ---- 状態 0 から始めた分布の変化（周期があると振動）
  , plotAxes g "n (steps)" "P(each state) n steps after starting at 0" [0, 10, 20, 30, 40] [0, 0.5, 1]
  , Pictures [ drawCurve g (map (\d -> at' d i 0) (wHist w)) (stateCol i) 2 0 | i <- [0 .. n - 1] ]
  , Pictures [ sayLM (show i) (grX g + grW g + 4) (gY g (at' (at' (wHist w) 40 []) i 0)) 14 (col (stateCol i)) | i <- [0 .. min n 8 - 1] ]
  , sayT "line colour = state (node colour = class)" (grX g) (grY g + grH g + 42) 14 (col sub) False
  , drawSelfTest env (wTest w) ]
  where
    n = length (wPos w)
    info = wInfo w
    classes = ciClasses info
    ww = envW env * 0.55
    x0 = ww + 80
    y0 = 120
    vy = y0 + 40 + i2f (length classes) * 26
    g = Graph x0 (vy + 50) (min 460 (envW env - x0 - 40)) (envH env - vy - 140) 0 40 0 1
    kind c = if ccAbsorbing c then "absorbing" else if ccClosed c then "closed (recurrent)" else "leaves (transient)"
    per c = if ccPeriod c == 0 then "no return" else "period " ++ show (ccPeriod c)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let p@(mx, my) = toP5 env pos
        w1 = w { wPress = p }
        i = nodeAt w p
        n = length (wPos w)
    in if mx > envW env * 0.55 + 40 || my < 96 then w1
       else if i >= 0 then w1 { wDragFrom = i }
       else if n < maxStates
         then rebuild w1 { wPos = wPos w ++ [p], wEdges = map (++ [0]) (wEdges w) ++ [replicate (n + 1) 0] }
         else w1
  EventKey (MouseButton LeftButton) Up _ pos | wDragFrom w >= 0 ->
    let p@(mx, my) = toP5 env pos
        j = nodeAt w p
        (px, py) = wPress w
        moved = hyp (mx - px) (my - py) > 8
        i = wDragFrom w
        w1 = w { wDragFrom = -1 }
    in if j >= 0 && (j /= i || not moved) then rebuild w1 { wEdges = modifyMat i j (\v -> if v /= 0 then 0 else 1) (wEdges w) } else w1
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> preset env (fromEnum c - fromEnum '0') w
    | c == 'c' || c == 'C' -> w { wPos = [], wEdges = [], wP = [], wInfo = ChainInfo [] [] False, wHist = replicate 41 [], wDragFrom = -1 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 例 1（周期 2）に、どこか 1 つ「自分への矢印」を足すと、周期は？ グラフの振動は？
-- 2. 例 2 で、状態 0 から始めずに真ん中から始めたい → rebuild の mu を「i == 2 なら 1」に
-- 3. 例 4 の輪のどこかに長さ 4 の輪を作ると、gcd(3, 4) = 1 で周期 1 になる
-- 4. 吸収状態が 2 つあるとき、どちらに吸収されるかの確率を 04・05 で計算する

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("2 つの間を行き来するだけ：既約、周期 2", ciIrreducible c2 && per0 c2 == 2)
  , ("長さ 3 の輪：周期 3", ciIrreducible c3 && per0 c3 == 3)
  , ("輪に自分への矢印を足す：周期 1", ciIrreducible c1 && per0 c1 == 1)
  , ("ギャンブラーの連鎖：クラス {0} {1,2,3} {4}、0 と 4 は吸収、{1,2,3} は一時的"
    , length (ciClasses gr) == 3 && ccAbsorbing (byM 0) && ccAbsorbing (byM 4) && not (ccClosed (byM 2)) && length (ccMembers (byM 2)) == 3)
  , ("でたらめなグラフ 40 個：周期 = gcd{n : Pⁿ(i,i) > 0}（総当たり）", all fst randomRes)
  , ("40 個：既約 ⇔ どこからどこへも行ける（Pⁿ で確認）", all snd randomRes)
  , ("周期 2 の連鎖では分布が (0,1) ⇔ (1,0) を行き来して収束しない", osc) ]
  where
    cls n list = classifyChain (edgesToP (fromList n list))
    per0 ci = ccPeriod (headOr (ChainClass [] False 0 False) (ciClasses ci))
    c2 = cls 2 [(0, 1), (1, 0)]
    c3 = cls 3 [(0, 1), (1, 2), (2, 0)]
    c1 = cls 3 [(0, 1), (1, 2), (2, 0), (1, 1)]
    gr = cls 5 [(1, 0), (1, 2), (2, 1), (2, 3), (3, 2), (3, 4)]   -- 0 と 4 は矢印なし → 吸収
    byM m = maybe (ChainClass [] False 0 False) id (find (\c -> m `elem` ccMembers c) (ciClasses gr))
    -- 周期 = gcd{ n : P^n(i,i) > 0 } を総当たりで、でたらめなグラフと比べる
    graphs = withSeed 3 (mapM (\t -> do let n = 3 + t `mod` 5
                                            pr = if odd t then 0.25 else 0.4
                                        vs <- mapM (\_ -> rnd) [1 .. n * n]
                                        pure [ [ if v < pr then 1 else 0 | v <- take n (drop (i * n) vs) ] | i <- [0 .. n - 1] ]) [0 .. 39 :: Int])
    randomRes = map checkOne graphs
    checkOne e =
      let n = length e
          a = edgesToP e
          info = classifyChain a
          pws = take (3 * n) (drop 1 (iterate (`matMul` a) (eyeMat n)))
          g = [ foldl' (\gg (k, pw) -> if (pw !! i !! i) > 1e-15 then gcdInt gg k else gg) 0 (zip [1 ..] pws) | i <- [0 .. n - 1] ]
          reach = [ [ i == j || any (\pw -> (pw !! i !! j) > 1e-15) pws | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]
          okP = and [ ccPeriod c == g !! i | c <- ciClasses info, i <- ccMembers c ]
          okI = all and reach == ciIrreducible info
      in (okP, okI)
    p2 = edgesToP (fromList 2 [(0, 1), (1, 0)])
    osc = and [ (mu !! (k `mod` 2)) == 1 | (k, mu) <- zip [1 .. 10 :: Int] (drop 1 (iterate (`vecMat` p2) [1, 0])) ]

commaList :: [String] -> String
commaList xs = case xs of
  [] -> ""
  _ -> foldr1 (\a b -> a ++ ", " ++ b) xs

-- ==================================================
-- ここから下は「マルコフ連鎖の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。行列 = 行のリスト、分布 = 横ベクトル（リスト）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具
type Vec = [Double]                           -- 分布・ベクトル
type Mat = [[Double]]                         -- 行列（transP !! i !! j = i の次に j へ行く確率、行の和 = 1）
type RGB = (Float, Float, Float)

bgCol, cream, dice, teal, mustard, plum, sub :: RGB
bgCol = (32, 28, 26)                          -- 背景（あたたかい炭色）
cream = (246, 236, 214)
dice = (236, 96, 58)
teal = (44, 178, 166)
mustard = (236, 190, 82)
plum = (190, 130, 200)
sub = (190, 178, 160)

stateCols :: [RGB]                            -- ★ 状態の色（8 色をくり返す）
stateCols = [(236, 96, 58), (44, 178, 166), (236, 190, 82), (190, 130, 200), (120, 190, 240), (150, 210, 110), (240, 140, 170), (200, 170, 130)]

stateCol :: Int -> RGB
stateCol i = stateCols !! (i `mod` 8)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

grayC :: Float -> Color
grayC v = col (v, v, v)

scaleRGB :: Float -> RGB -> RGB
scaleRGB k (r, g, b) = (r * k, g * k, b * k)

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

meanOf :: [Double] -> Double
meanOf xs = if null xs then 0 else sumOf xs / fromIntegral (length xs)

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float                         -- 描くときの Float（NaN は 0）
f2 = realToFrac . fin 0

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- リストの i 番目を f で変える
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

setAt :: Int -> a -> [a] -> [a]
setAt i v = modifyAt i (const v)

-- 行列の (i, j) を f で変える
modifyMat :: Int -> Int -> (Double -> Double) -> Mat -> Mat
modifyMat i j f = modifyAt i (modifyAt j f)

-- 長さ √(a² + b²)（Math.hypot）
hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Integer
jsRound v = let r = floor v in if v - fromIntegral r >= 0.5 then r + 1 else r

jsRoundI :: Double -> Int
jsRoundI v = if isNaN v || isInfinite v then 0 else fromIntegral (jsRound (clampv v (-1e15) 1e15))

-- Math.round(v * 100) / 100
round2 :: Double -> Double
round2 v = fromIntegral (jsRound (v * 100)) / 100

round1 :: Double -> Double
round1 v = fromIntegral (jsRound (v * 10)) / 10

-- 確率 p !! i で i を返す（rnd() を 1 回だけ引く。p5.js 版の sampleFrom）
sampleFrom :: [Double] -> R Int
sampleFrom p = do
  u0 <- rnd
  let u = u0 * sumOf p
      go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- ---- 数の書き方
-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）。−0.00 は 0.00
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = if d > 0 then ip ++ "." ++ fp else ip
                in if n == 0 || v > 0 then body else '-' : body

-- v.toExponential(1)：1.2e-15 の形
fmtE :: Double -> String
fmtE v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0.0e+0"
  | otherwise =
      let r = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          e1 = head ([e | e <- [e0 - 1, e0, e0 + 1], 10 ^^ e <= r, r < 10 ^^ (e + 1)] ++ [e0])
          n0 = floor (r / 10 ^^ (e1 - 1) + 1 / 2) :: Integer
          (n, e) = if n0 >= 100 then (10, e1 + 1) else (n0, e1)
      in (if v < 0 then "-" else "") ++ show (n `div` 10) ++ "." ++ show (n `mod` 10) ++ "e" ++ (if e >= 0 then "+" else "-") ++ show (abs e)

-- 整数を 3 けたごとのコンマで（toLocaleString）
commas :: Integer -> String
commas n | n < 0 = '-' : commas (negate n)
         | otherwise = reverse (go (reverse (show n)))
  where go s = case splitAt 3 s of
          (a, []) -> a
          (a, rest) -> a ++ "," ++ go rest

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v0 = let v = fin 0 v0 in
  if v == 0 then "0" else
    let e = floor (logBase 10 (abs v)) :: Int
        k = 10 ^^ (2 - e) :: Double
        r = fromIntegral (jsRound (v * k)) / k
    in if r == fromIntegral (jsRound r) then show (jsRound r) else showFFloat Nothing r ""

showD :: Double -> String                     -- JavaScript の String(v) に近い書き方（0.05、2、0.1）
showD v = if v == fromIntegral (jsRound v) && abs v < 1e15 then show (jsRound v) else showFFloat Nothing v ""

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：種 s の列（0 なら 1）
rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 計算を種 s から実行して、結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

randn :: R Double                             -- 標準正規分布（ボックス-ミュラー、u = 1 − rnd()）
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- n 回くり返して結果を順に集める（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- ---- 行列の道具（行列 = 行のリスト。遷移行列 transP !! i !! j = i の次に j へ行く確率、行の和 = 1）
zerosMat :: Int -> Mat
zerosMat n = replicate n (replicate n 0)

zerosMat2 :: Int -> Int -> Mat
zerosMat2 n m = replicate n (replicate m 0)

eyeMat :: Int -> Mat
eyeMat n = [[if i == j then 1 else 0 | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]

nCols :: Mat -> Int
nCols a = length (headOr [] a)

-- 行列の積 (a b)[i][j] = Σ_k a[i][k] b[k][j]（k の順に足す）
matMul :: Mat -> Mat -> Mat
matMul a b = [ foldl' (\acc (v, brow) -> if v == 0 then acc else zipWith (\c x -> c + v * x) acc brow) (replicate (nCols b) 0) (zip arow b) | arow <- a ]

-- 横ベクトル × 行列：次の時刻の分布 = いまの分布 × P
vecMat :: Vec -> Mat -> Vec
vecMat v a = foldl' (\acc (x, row) -> if x == 0 then acc else forceL (zipWith (\c y -> c + x * y) acc row)) (replicate (nCols a) 0) (zip v a)

forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

matPow :: Mat -> Int -> Mat                   -- P^k（k 回かけるだけ）
matPow a k = iterate (`matMul` a) (eyeMat (length a)) !! max 0 k

-- 2 つの行列（またはベクトル）のいちばん大きなずれ
maxAbsDiff :: Mat -> Mat -> Double
maxAbsDiff a b = foldl' maxN 0 (zipWith maxAbsDiffV a b)

maxAbsDiffV :: Vec -> Vec -> Double
maxAbsDiffV a b = foldl' (\d (x, y) -> maxN d (abs (x - y))) 0 (zip a b)

-- Math.max と同じく NaN が混じったら NaN
maxN :: Double -> Double -> Double
maxN a b = if isNaN a || isNaN b then 0 / 0 else max a b

rowSumErr :: Mat -> Double -> Double          -- 行の和と target のいちばん大きなずれ
rowSumErr a target = foldl' (\d r -> max d (abs (sumOf r - target))) 0 a

-- 行の和を 1 に。全部 0 の行は「その場にとどまる」にする（NaN を出さない）
fixRows :: Mat -> Mat
fixRows a = [ let s = sumOf r in if not (s > 1e-12) then [if j == i then 1 else 0 | j <- [0 .. length r - 1]] else map (/ s) r
            | (i, r) <- zip [0 :: Int ..] a ]

-- ガウスの消去法（部分ピボット）で a x = b。解けなければ Nothing
solveLinear :: Mat -> Vec -> Maybe Vec
solveLinear a b = fmap (map (headOr 0)) (solveMany a (map (: []) b))

-- 右辺が何本あっても同じ手順で消去する（右辺 1 本ずつ解くのとまったく同じ計算になる）
solveMany :: Mat -> Mat -> Maybe Mat
solveMany a bs = go 0 (zipWith (++) a bs)
  where
    n = length a
    go c m
      | c >= n = Just [ map (/ at' r i 1) (drop n r) | (i, r) <- zip [0 ..] m ]
      | otherwise =
          let colC = [ abs (at' r c 0) | r <- m ]
              p = foldl' (\pb r -> if at' colC r 0 > at' colC pb 0 then r else pb) c [c + 1 .. n - 1]
          in if at' colC p 0 < 1e-13 then Nothing else
               let rc = at' m p []
                   rp = at' m c []
                   m1 = [ if r == c then rc else if r == p then rp else row | (r, row) <- zip [0 ..] m ]
                   piv = at' rc c 1
                   m2 = [ if r == c then row else
                            let f = at' row c 0 / piv
                            in if f == 0 then row else forceL (take c row ++ zipWith (\x y -> x - f * y) (drop c row) (drop c rc))
                        | (r, row) <- zip [0 ..] m1 ]
               in go (c + 1) m2

-- 逆行列（a x = e_j を列ごとに解いたもの）。逆がなければ Nothing
matInv :: Mat -> Maybe Mat
matInv a = solveMany a (eyeMat (length a))

transposeM :: Mat -> Mat
transposeM a = if null a then [] else [ [ at' r j 0 | r <- a ] | j <- [0 .. nCols a - 1] ]

-- 定常分布：π P = π と Σ π = 1 を連立方程式として解く（(P^T − I) π = 0 の最後の式を Σ π = 1 に置きかえる）
stationaryExact :: Mat -> Maybe Vec
stationaryExact a =
  let n = length a
      m = [ if i == n - 1 then replicate n 1 else [ at' (at' a j []) i 0 - (if i == j then 1 else 0) | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]
      b = [ if i == n - 1 then 1 else 0 | i <- [0 .. n - 1] ]
  in if n == 0 then Nothing else solveLinear m b

-- べき乗法：π ← π P を変化が tol 未満になるまで。戻り値 (π, くり返した回数)
stationaryPower :: Mat -> Double -> Int -> Maybe Vec -> (Vec, Int)
stationaryPower a tol maxIt start = go v0 0 posInf
  where
    n = length a
    v0 = maybe (replicate n (1 / fromIntegral n)) id start
    go v its d
      | d > tol && its < maxIt = let w = vecMat v a in go w (its + 1) (maxAbsDiffV w v)
      | otherwise = (v, its)

tvDist :: Vec -> Vec -> Double                -- 全変動距離
tvDist p q = foldl' (+) 0 (zipWith (\x y -> abs (x - y)) p q) / 2

-- エントロピー率 H = −Σ_i π_i Σ_j P_ij log₂ P_ij（1 歩あたりの情報量、ビット）
entropyRate :: Mat -> Vec -> Double
entropyRate a pv = foldl' (\h (row, p) -> foldl' (\h2 v -> if v > 0 then h2 - p * v * logBase 2 v else h2) h row) 0 (zip a pv)

posInf :: Double
posInf = 1 / 0

-- ---- 状態の分類：強連結成分（タージャンの方法）と周期
adjOf :: Mat -> [[Int]]                       -- 確率 > 0 の矢印
adjOf a = [ [ j | (j, v) <- zip [0 ..] r, v > 1e-12 ] | r <- a ]

-- 戻り値：comp !! i = 状態 i のクラスの番号（行ったり来たりできる状態どうしが同じ番号）
sccTarjan :: [[Int]] -> [Int]
sccTarjan adj = IM.elems (tComp (foldl' (\s v -> if IM.member v (tIdx s) then s else visit v s) st0 [0 .. n - 1]))
  where
    n = length adj
    st0 = TS IM.empty IM.empty [] S.empty (IM.fromList [(i, -1) | i <- [0 .. n - 1]]) 0 0
    visit v s0 =
      let s1 = s0 { tIdx = IM.insert v (tCnt s0) (tIdx s0), tLow = IM.insert v (tCnt s0) (tLow s0), tStack = v : tStack s0, tOn = S.insert v (tOn s0), tCnt = tCnt s0 + 1 }
          s2 = foldl' (\s w -> if not (IM.member w (tIdx s)) then let s' = visit w s in s' { tLow = IM.insert v (min (low s' v) (low s' w)) (tLow s') }
                               else if S.member w (tOn s) then s { tLow = IM.insert v (min (low s v) (IM.findWithDefault 0 w (tIdx s))) (tLow s) }
                               else s) s1 (at' adj v [])
      in if low s2 v == IM.findWithDefault 0 v (tIdx s2) then popTo v s2 else s2
    low s v = IM.findWithDefault 0 v (tLow s)
    popTo v s = case tStack s of
      (w : rest) -> let s' = s { tStack = rest, tOn = S.delete w (tOn s), tComp = IM.insert w (tNc s) (tComp s) }
                    in if w == v then s' { tNc = tNc s + 1 } else popTo v s'
      [] -> s

data TS = TS { tIdx :: IM.IntMap Int, tLow :: IM.IntMap Int, tStack :: [Int], tOn :: S.Set Int, tComp :: IM.IntMap Int, tCnt :: !Int, tNc :: !Int }

gcdInt :: Int -> Int -> Int
gcdInt a b = gcd (abs a) (abs b)

-- クラスの周期：BFS の深さ lev で、クラスの中の矢印 u→v ごとに gcd(lev u + 1 − lev v)。0 = クラスの中に矢印がない
classPeriod :: [[Int]] -> [Int] -> Int
classPeriod adj members = case members of
  [] -> 0
  (m0 : _) ->
    let inC = S.fromList members
        bfs lev [] = lev
        bfs lev (u : q) = let new = [ v | v <- at' adj u [], S.member v inC ]
                              (lev', q') = foldl' (\(l, qq) v -> if IM.member v l then (l, qq) else (IM.insert v (IM.findWithDefault 0 u l + 1) l, qq ++ [v])) (lev, q) new
                          in bfs lev' q'
        lv = bfs (IM.singleton m0 0) [m0]
    in foldl' (\g (u, v) -> gcdInt g (IM.findWithDefault 0 u lv + 1 - IM.findWithDefault 0 v lv)) 0
         [ (u, v) | u <- members, v <- at' adj u [], S.member v inC ]

data ChainClass = ChainClass { ccMembers :: [Int], ccClosed :: !Bool, ccPeriod :: !Int, ccAbsorbing :: !Bool }
data ChainInfo = ChainInfo { ciComp :: [Int], ciClasses :: [ChainClass], ciIrreducible :: !Bool }

-- クラスごとに：メンバー、閉じている（外へ出ない）か、周期
classifyChain :: Mat -> ChainInfo
classifyChain a =
  let adj = adjOf a
      comp = sccTarjan adj
      nc = maxOr (-1) comp + 1
      cls = [ ChainClass mem closed (classPeriod adj mem) (length mem == 1 && at' (at' a (headOr 0 mem) []) (headOr 0 mem) 0 > 1 - 1e-12)
            | c <- [0 .. nc - 1]
            , let mem = [ i | (i, k) <- zip [0 ..] comp, k == c ]
                  closed = and [ at' comp v (-1) == c | u <- mem, v <- at' adj u [] ] ]
  in ChainInfo comp cls (nc == 1)

-- ---- 遷移図を描く道具：矢印の太さ = 確率
-- 少しふくらんだ曲線の点（両はしの円 r の中は除く）
bezPts :: (Float, Float) -> (Float, Float) -> Float -> Float -> [(Float, Float)]
bezPts (ax, ay) (bx, by) bend r =
  let mx = (ax + bx) / 2
      my = (ay + by) / 2
      dx = bx - ax
      dy = by - ay
      l0 = sqrt (dx * dx + dy * dy)
      l = if l0 == 0 then 1 else l0
      cx = mx - (dy / l) * bend * l
      cy = my + (dx / l) * bend * l
  in [ (x, y) | k <- [0 .. 30 :: Int], let t = i2f k / 30
                                           x = (1 - t) * (1 - t) * ax + 2 * t * (1 - t) * cx + t * t * bx
                                           y = (1 - t) * (1 - t) * ay + 2 * t * (1 - t) * cy + t * t * by
     , hyp (x - ax) (y - ay) > r + 2, hyp (x - bx) (y - by) > r + 6 ]

-- 折れ線の最後に三角の矢じり
arrowHeadAt :: [(Float, Float)] -> Float -> Picture
arrowHeadAt pts sz = case reverse pts of
  ((x2, y2) : (x1, y1) : _) ->
    let l0 = hyp (x2 - x1) (y2 - y1)
        l = if l0 == 0 then 1 else l0
        ux = (x2 - x1) / l
        uy = (y2 - y1) / l
    in Polygon [(x2 + ux * sz, y2 + uy * sz), (x2 - uy * sz * 0.6, y2 + ux * sz * 0.6), (x2 + uy * sz * 0.6, y2 - ux * sz * 0.6)]
  _ -> Blank

-- 矢印のラベルやクリック判定に使う点：i → j の矢印の真ん中 (hx, hy)
data Hit = Hit { hI :: !Int, hJ :: !Int, hX :: !Float, hY :: !Float }

data GraphOpts = GraphOpts { goNames :: Maybe [String], goHl :: !Int, goNameSize :: !Float, goLabels :: !Bool, goAlpha :: Maybe Float }

defOpts :: GraphOpts
defOpts = GraphOpts Nothing (-1) 18 True Nothing

-- nodes = [(x, y), …]、a = 遷移行列、r = 円の半径、cols i = 状態 i の色。戻り値：絵と、矢印ごとの Hit
drawChainGraph :: [(Float, Float)] -> Mat -> Float -> (Int -> RGB) -> GraphOpts -> (Picture, [Hit])
drawChainGraph nodes a r cols opts = (Pictures (edgePics ++ nodePics), hits)
  where
    nn = length nodes
    cx = realToFrac (meanOf (map (realToFrac . fst) nodes)) :: Float
    cy = realToFrac (meanOf (map (realToFrac . snd) nodes)) :: Float
    edges = [ (i, j, p, c, w, pts, h)
            | i <- [0 .. nn - 1], j <- [0 .. nn - 1]
            , let p = at' (at' a i []) j 0
            , p > 1e-9
            , let c = cols i
                  w = 1 + 11 * f2 p
                  (pts, h) = edgeShape i j ]
    hits = [ h | (_, _, _, _, _, _, Just h) <- edges ]
    edgeShape i j
      | i == j = let (nx, ny) = at' nodes i (0, 0)
                     ox = nx - cx
                     oy = ny - cy
                     l0 = hyp ox oy
                     l = if l0 == 0 then 1 else l0
                     ux = if l <= 1 then 0 else ox / l
                     uy = if l > 1 then oy / l else -1
                     lx = nx + ux * r * 1.55
                     ly = ny + uy * r * 1.55
                     rr = r * 0.62
                     base = atan2 (negate uy) (negate ux)
                     pts = [ (lx + rr * cos t, ly + rr * sin t) | k <- [0 .. 32 :: Int], let t = base + 0.9 + (i2f k / 32) * (2 * pi - 1.8) ]
                 in (pts, Just (Hit i j (lx + ux * rr) (ly + uy * rr)))
      | otherwise = let pts = bezPts (at' nodes i (0, 0)) (at' nodes j (0, 0)) 0.14 r
                        (hx, hy) = at' pts (length pts `div` 2) (0, 0)
                    in (pts, if null pts then Nothing else Just (Hit i j hx hy))
    aE = maybe 210 id (goAlpha opts)
    aH = maybe 230 id (goAlpha opts)
    edgePics = [ Pictures [ Color (colA aE c) (polyline w pts), Color (colA aH c) (arrowHeadAt pts (8 + w * 0.8))
                          , case h of
                              Just hh | goLabels opts -> sayM (fmtN 2 p) (hX hh) (hY hh - 12) 14 (col cream)
                              _ -> Blank ]
               | (_, _, p, c, w, pts, h) <- edges ]
    nodePics = [ Pictures [ Color (col cream) (circleAt x y (2 * r + sw)), Color (col (scaleRGB 0.55 c)) (circleAt x y (2 * r - sw))
                          , sayM (maybe (show i) (\ns -> at' ns i (show i)) (goNames opts)) x y (goNameSize opts) (col cream) ]
               | (i, (x, y)) <- zip [0 ..] nodes, let c = cols i, let sw = if goHl opts == i then 4 else 1.5 ]

-- 行列を色つきの表で（濃い = 大きい）。左上 (x, y)、マスの大きさ cw × ch、d = 小数の桁、hi = いちばん濃い値
drawMatrix :: Mat -> Float -> Float -> Float -> Float -> Maybe [String] -> Float -> Int -> Double -> Picture
drawMatrix a x y cw ch names ts d hi = Pictures $
  [ Pictures [ Color (col (48 + t * (tr - 48), 42 + t * (tg - 42), 40 + t * (tb - 40))) (rectTL (x + i2f j * cw) (y + i2f i * ch) (cw - 2) (ch - 2))
             , sayM (fmtN d v) (x + i2f j * cw + cw / 2) (y + i2f i * ch + ch / 2) ts (col cream) ]
  | (i, row) <- zip [0 ..] a, (j, v) <- zip [0 ..] row, let t = f2 (clampv (abs v / hi) 0 1) ]
  ++ case names of
       Nothing -> []
       Just ns -> [ sayRM nm (x - 6) (y + i2f i * ch + ch / 2) ts (col sub) | (i, nm) <- zip [0 .. length a - 1] ns ]
                  ++ [ sayC nm (x + i2f j * cw + cw / 2) (y - 5) ts (col sub) | (j, nm) <- zip [0 .. nCols a - 1] ns ]
  where (tr, tg, tb) = teal

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

gPt :: Graph -> Double -> Double -> (Float, Float)
gPt g a b = (gX g a, gY g b)

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (col (84, 76, 70)) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (col sub) | v <- xt ]
  ++ [ sayRM (prec3 v) (grX g - 5) (gY g v) 14 (col sub) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 36) 14 (col cream), say yl (grX g) (grY g - 8) 14 (col cream) ]

-- arr !! i を x = i (+ xOff) に。長いときは間引いて 600 点まで
drawCurve :: Graph -> [Double] -> RGB -> Float -> Double -> Picture
drawCurve g arr c sw xOff =
  let n = length arr
      stride = max 1 (ceiling (i2d n / 600)) :: Int
      pts = [ (gX g (i2d i + xOff), gY g v) | (i, v) <- zip [0 ..] arr, i `mod` stride == 0 ]
            ++ [ (gX g (i2d (n - 1) + xOff), gY g (lastOr 0 arr)) | n > 1, (n - 1) `mod` stride /= 0 ]
  in Color (col c) (polyline sw pts)

legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures [ Pictures [Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col cream)]
                            | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- 棒グラフ（top = 縦軸の上限）
drawBars :: Float -> Float -> Float -> Float -> [Double] -> (Int -> RGB) -> Double -> Maybe [String] -> Picture
drawBars x y w h vals cols top names =
  let n = length vals
      bw = w / i2f (max 1 n)
  in Pictures $ Color (col (84, 76, 70)) (lineW 1 (x, y + h) (x + w, y + h))
       : [ Pictures [ Color (col (cols i)) (rectTL (x + i2f i * bw + bw * 0.12) (y + h - hh) (bw * 0.76) hh)
                    , case names of
                        Just ns -> sayT (at' ns i "") (x + i2f i * bw + bw / 2) (y + h + 4) 14 (col sub) True
                        Nothing -> Blank ]
         | (i, v) <- zip [0 ..] vals, let hh = f2 (clampv (v / (if top == 0 then 1 else top)) 0 1) * h ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 左揃え・右揃えで上下は中央（textAlign(LEFT, CENTER)、(RIGHT, CENTER)）
sayLM, sayRM :: String -> Float -> Float -> Float -> Color -> Picture
sayLM s x y size c = say s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c

-- 上が y の文字（textAlign(…, TOP)）。center = True なら中央揃え
sayT :: String -> Float -> Float -> Float -> Color -> Bool -> Picture
sayT s x y size c center = (if center then sayC else say) s x (y + size * 0.85) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (max 0 d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (max 0 d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> thickPath w pts
  _ -> Blank

-- 点線（長さ on の線と off のすき間をくり返す）
dashLine :: Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine w on off (x1, y1) (x2, y2) =
  let l = hyp (x2 - x1) (y2 - y1)
      n = floor (l / (on + off)) :: Int
      ux = if l == 0 then 0 else (x2 - x1) / l
      uy = if l == 0 then 0 else (y2 - y1) / l
  in Pictures [ lineW w (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + on), y1 + uy * min l (s + on)) | k <- [0 .. n], let s = i2f k * (on + off), s < l ]

-- 見出しと操作の説明（左上）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures [ say t 40 42 22 (col cream), say s 40 68 15 (col sub) ]

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（iw × ih の画像）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

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

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col (120, 220, 190) else col (255, 90, 90))
