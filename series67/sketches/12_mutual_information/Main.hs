-- ============================================
-- 67-12 相互情報量と独立 ─ 同時分布の表 p(x, y) から「独立な表の集まり」M への KL が相互情報量 I(X; Y)
-- m-射影（06）の足は周辺分布の積 q = p(x)p(y)、I = D(p‖q) = H(X) + H(Y) − H(X, Y)。独立なら 0
-- 3 変数（2 値 × 3）：全体の関係 D(p‖p1) = 「3 つ全体の関係」D(p‖p2) + 「2 つずつの関係」D(p2‖p1)
--   p1 = 3 つの周辺分布の積、p2 = 2 つずつの周辺分布を守るエントロピー最大の分布（反復比例あてはめ IPF）。XOR ではペアは独立なのに全体は 1 ビット
-- 操作：1 2×2　2 3×3　3 3 変数 / マス目を上下にドラッグ = 確率を増やす・減らす / P お手本を順に / R でたらめ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (orange, violet, rose)
import Data.Bits (shiftR, testBit, xor, (.&.), (.|.))
import Data.List (foldl', intercalate)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8, Word32)
import Numeric (showEFloat)
import System.Environment (lookupEnv)

presets2 :: [String]
presets2 = ["independent", "copy (x = y)", "inverse (x != y)", "noisy copy", "random"]   -- ★ 2 変数のお手本

presets3 :: [String]
presets3 = ["XOR (z = x xor y)", "all three equal", "x = y, z independent", "random"]    -- ★ 3 変数のお手本

data World = World
  { wMode  :: !Int                            -- 0 = 2×2、1 = 3×3、2 = 3 変数
  , wTab   :: [Double]                        -- 表（2 変数は tab[x*k + y]、3 変数は tab[4x + 2y + z]）
  , wP2    :: [Double]                        -- 3 変数のときの IPF の答え（表が変わったときだけ計算）
  , wPre   :: !Int
  , wGrab  :: !Int                            -- ドラッグしているマス（−1 = なし）
  , wLastY :: !Double
  , wRng   :: !Rng
  , wTest  :: [Check] }

main :: IO ()
main = runSketch (sketch "67-12 Mutual information" world0)
  { skBack  = col bgC
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure w
  , skDraw  = draw
  , skEvent = event }

-- p5.js 版は自己テストのあと seedRng(12) してお手本を作る
world0 :: World
world0 = applyPreset (World 0 [] [] 0 (-1) 0 (rngOf 12) [])

dimK :: Int -> Int
dimK m = if m == 1 then 3 else 2

-- ---- 2 変数：tab[x*k + y]
marg2 :: [Double] -> Int -> ([Double], [Double])
marg2 t k = ( [ sumOf [ at' t (x * k + y) 0 | y <- [0 .. k - 1] ] | x <- [0 .. k - 1] ]
            , [ sumOf [ at' t (x * k + y) 0 | x <- [0 .. k - 1] ] | y <- [0 .. k - 1] ] )

prod2 :: [Double] -> Int -> [Double]
prod2 t k = let (px, py) = marg2 t k in [ a * b | a <- px, b <- py ]

mutualInfo :: [Double] -> Int -> Double
mutualInfo t k = divKL t (prod2 t k)

-- ---- 3 変数（2 値）：tab[4x + 2y + z]
idx3 :: Int -> Int -> Int -> Int
idx3 x y z = 4 * x + 2 * y + z

bitOf :: Int -> Int -> Int                    -- i の j 番目の変数（0 = x、1 = y、2 = z）
bitOf i j = (i `shiftR` (2 - j)) .&. 1

prod3 :: [Double] -> [Double]
prod3 t = [ foldl' (\s j -> s * (if bitOf i j == 1 then at' m j 0 else 1 - at' m j 0)) 1 [0, 1, 2] | i <- [0 .. 7] ]
  where m = [ foldl' (\s (i, v) -> if bitOf i j == 1 then s + v else s) 0 (zip [0 ..] t) | j <- [0, 1, 2] ]

pairMarg :: [Double] -> Int -> Int -> [Double]
pairMarg t a b = foldl' (\out (i, v) -> modifyAt (2 * bitOf i a + bitOf i b) (+ v) out) [0, 0, 0, 0] (zip [0 ..] t)

-- ★ 反復比例あてはめ：一様から始めて、3 つのペアの周辺分布を順に合わせる
ipfPairwise :: [Double] -> [Double]
ipfPairwise t = go (0 :: Int) (replicate 8 (1 / 8))
  where
    pairs = [(0, 1), (1, 2), (0, 2)]
    go it r
      | it >= 5000 = r
      | otherwise =
          let (r', err) = foldl' onePair (r, 0) pairs
          in if err < 1e-15 then r' else go (it + 1) r'
    onePair (r, err) (a, b) =
      let want = pairMarg t a b
          have = pairMarg r a b
          r' = forceL [ let c = 2 * bitOf i a + bitOf i b
                            h = at' have c 0
                        in if h > 0 then (ri * at' want c 0) / h else 0
                      | (i, ri) <- zip [0 ..] r ]
      in (r', foldl' max err (zipWith (\wv hv -> abs (wv - hv)) want have))

-- お手本の表（p5.js 版と同じく、どのお手本でもマスごとに rnd() を 1 回引く）
applyPreset :: World -> World
applyPreset w =
  let (t, g) = runR (if wMode w < 2 then two else three) (wRng w)
  in withTab t w { wRng = g }
  where
    k = dimK (wMode w)
    two = do
      vs <- mapM (\(x, y) -> do
                    u <- rnd
                    let v = at' [ i2d (((x + 1) * (y + 2)) `mod` 5 + 1), if x == y then 1 else 0, if x /= y then 1 else 0
                                , if x == y then 0.85 else 0.15 / i2d (k - 1), u ] (wPre w) 0
                    pure (if wPre w == 0 then i2d ((x + 1) * (k - y + 1)) else v))   -- 独立：行と列の積
                 [ (x, y) | x <- [0 .. k - 1], y <- [0 .. k - 1] ]
      pure (mixFloor (normP vs) 1e-4)
    three = do
      vs <- mapM (\i -> do
                    u <- rnd
                    let x = bitOf i 0
                        y = bitOf i 1
                        z = bitOf i 2
                    pure (at' [ if z == x `xor` y then 1 else 0, if x == y && y == z then 1 else 0, if x == y then 1 else 0, u ] (wPre w) 0)) [0 .. 7]
      pure (normP vs)

withTab :: [Double] -> World -> World
withTab t w = w { wTab = forceL t, wP2 = if wMode w == 2 then ipfPairwise t else [] }

-- ドラッグできるマス目 (x, y, 一辺, 番号)
cellsOf :: Env -> World -> [(Double, Double, Double, Int)]
cellsOf env w
  | wMode w < 2 = [ (70 + i2d b * cs, 140 + i2d a * cs, cs - 4, a * k + b) | a <- [0 .. k - 1], b <- [0 .. k - 1] ]
  | otherwise = [ (70 + i2d z * (2 * cs3 + 40) + i2d y * cs3, 140 + i2d x * cs3, cs3 - 4, idx3 x y z) | z <- [0, 1], x <- [0, 1], y <- [0, 1] ]
  where k = dimK (wMode w)
        cs = cs2 env k
        cs3 = cs3of env

cs2 :: Env -> Int -> Double
cs2 env k = min 110 ((d2 (envH env) - 300) / i2d k)

cs3of :: Env -> Double
cs3of env = min 90 ((d2 (envH env) - 380) / 2)

-- 表を色の濃さで
drawTable :: [Double] -> Int -> Int -> Double -> Double -> Double -> String -> RGB -> Picture
drawTable t k1 k2 x0 y0 cs title c = Pictures $
  say title (f2 x0) (f2 y0 - 10) 15 (col c)
  : [ Pictures [ Color (col (sh c)) (rectTL x y (f2 cs - 4) (f2 cs - 4)), Color (col wire) (frameTL 1 x y (f2 cs - 4) (f2 cs - 4))
               , sayM (fmtN 3 v) (x + f2 cs / 2 - 2) (y + f2 cs / 2 - 2) 14 (col ink) ]
    | a <- [0 .. k1 - 1], b <- [0 .. k2 - 1]
    , let v = at' t (a * k2 + b) 0, let x = f2 (x0 + i2d b * cs), let y = f2 (y0 + i2d a * cs)
    , let sh (r, g, bb) = let s = f2 (sqrt (v / max mx 1e-9)) in (30 + (r - 30) * s, 30 + (g - 30) * s, 30 + (bb - 30) * s) ]
  where mx = maxOr 0 t

-- v.toExponential(1)：1.2e-16、0.0e+0 の形
expo1 :: Double -> String
expo1 v
  | isNaN v || isInfinite v = fmtN 1 v
  | otherwise = let (mant, ex) = break (== 'e') (showEFloat (Just 1) v "")
                in case ex of
                     ('e' : '-' : d) -> mant ++ "e-" ++ d
                     ('e' : d) -> mant ++ "e+" ++ d
                     _ -> mant

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle "Mutual information and independence: KL to the independent tables"
             ("1 2x2   2 3x3   3 three variables   Drag cells up/down   P preset (" ++ at' names (wPre w `mod` length names) "" ++ ")   R random")
  , if wMode w < 2 then draw2 env w else draw3 env w
  , drawSelfTest env (wTest w) ]
  where names = if wMode w < 2 then presets2 else presets3

draw2 :: Env -> World -> Picture
draw2 env w = Pictures
  [ drawTable t k k 70 140 cs "joint p(x,y), row x, col y" orange
  , drawTable q k k (120 + i2d k * cs) 140 cs "product of marginals q = p(x)p(y)" violet
  , say ("p(x) = " ++ fmtP 3 px ++ "   p(y) = " ++ fmtP 3 py) 70 (f2 (170 + i2d k * cs)) 14 (col dimc)
  , say ("Mutual information I(X;Y) = D(p||q) = " ++ fmtN 5 (bitsOf mi) ++ " bits") 70 y0 17 (col ink)
  , say ("H(X) + H(Y) - H(X,Y) = " ++ fmtN 4 (bitsOf hx) ++ " + " ++ fmtN 4 (bitsOf hy) ++ " - " ++ fmtN 4 (bitsOf hxy) ++ " = "
         ++ fmtN 5 (max 0 (bitsOf (hx + hy - hxy))) ++ " bits") 70 (y0 + 28) 15 (col dimc)
  , say ("Pythagoras: D(p||uniform) = " ++ fmtN 5 (divKL t u) ++ " = I + D(q||uniform) = " ++ fmtN 5 mi ++ " + " ++ fmtN 5 (divKL q u) ++ " (nats)") 70 (y0 + 56) 15 (col lime)
  , say "I = 0 <=> the table is independent (same as q). Knowing x tells I bits about y on average" 70 (y0 + 84) 14 (col dimc)
  , say "q is the ML fit of the independence model (m-projection, 06)." 70 (y0 + 106) 14 (col dimc)
  , say "For any independent r: D(p||r) = I + D(q||r)" 70 (y0 + 125) 14 (col dimc) ]
  where
    k = dimK (wMode w)
    cs = cs2 env k
    t = wTab w
    q = prod2 t k
    (px, py) = marg2 t k
    mi = mutualInfo t k
    u = replicate (k * k) (1 / i2d (k * k))
    y0 = f2 (220 + i2d k * cs)
    hx = entropyH px
    hy = entropyH py
    hxy = entropyH t

draw3 :: Env -> World -> Picture
draw3 env w = Pictures $
  [ drawTable (half t z) 2 2 (70 + i2d z * (2 * cs + 40)) 140 cs ("p (z = " ++ show z ++ ")") orange | z <- [0, 1] ]
  ++ [ drawTable (half p2 z) 2 2 (d2 (envW env) * 0.55 + i2d z * (2 * cs + 40)) 140 cs ("p2, pairs only (z = " ++ show z ++ ")") violet | z <- [0, 1] ]
  ++ [ say ("Pairwise MI: I(X;Y) = " ++ fmtN 4 (bitsOf (mi2 0 1)) ++ "   I(Y;Z) = " ++ fmtN 4 (bitsOf (mi2 1 2)) ++ "   I(X;Z) = " ++ fmtN 4 (bitsOf (mi2 0 2)) ++ " bits") 70 y0 16 (col dimc)
     , say ("Total relation D(p||p1) = " ++ fmtN 6 (bitsOf tot) ++ " bits (p1 = product of the 3 marginals)") 70 (y0 + 36) 17 (col ink)
     , say ("= three-way relation D(p||p2) = " ++ fmtN 6 (bitsOf three)) 70 (y0 + 64) 17 (col orange)
     , say ("+ pairwise relations D(p2||p1) = " ++ fmtN 6 (bitsOf two)) 70 (y0 + 92) 17 (col violet)
     , say ("difference = " ++ expo1 (tot - three - two) ++ " (Pythagoras: it splits exactly in two)") 70 (y0 + 120) 15 (col lime)
     , say "XOR: every pair is independent (pair MI = 0), yet all three share 1 bit" 70 (y0 + 150) 14 (col dimc)
     , say "(z cannot be known without seeing both x and y)" 70 (y0 + 169) 14 (col dimc) ]
  where
    cs = cs3of env
    t = wTab w
    p2 = if null (wP2 w) then ipfPairwise t else wP2 w
    p1 = prod3 t
    tot = divKL t p1
    three = divKL t p2
    two = divKL p2 p1
    half v z = [ at' v (idx3 0 0 z) 0, at' v (idx3 0 1 z) 0, at' v (idx3 1 0 z) 0, at' v (idx3 1 1 z) 0 ]
    mi2 a b = mutualInfo (pairMarg t a b) 2
    y0 = f2 (200 + 2 * cs)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    w { wGrab = foldl' (\g (x, y, s, i) -> if inRect mx my (Box x y s s) then i else g) (-1) (cellsOf env w), wLastY = my }
  EventMotion _ | wGrab w >= 0 ->
    let f = exp (-(my - wLastY w) * 0.02)       -- 上へドラッグ = 増やす
        t = modifyAt (wGrab w) (\v -> max v 1e-4 * f) (wTab w)
    in withTab (mixFloor (normP t) (if wMode w < 2 then 1e-4 else 0)) w { wLastY = my }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wGrab = -1 }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> applyPreset w { wMode = fromEnum c - fromEnum '1', wPre = 0, wGrab = -1 }
    | c == 'p' || c == 'P' -> applyPreset w { wPre = (wPre w + 1) `mod` (if wMode w < 2 then length presets2 else length presets3) }
    | c == 'r' || c == 'R' -> applyPreset w { wPre = if wMode w < 2 then 4 else 3 }
  _ -> w
  where (mx, my) = mouseD env

-- ---------- やってみよう ----------
-- 1. 2×2 で「コピー」の表は何ビット？ ノイズを増やす（対角を下げる）と I はどう減る？
-- 2. 3×3 でコピーの表は log₂3 ≈ 1.585 ビット。なぜ 2×2 より大きい？
-- 3. 3 変数で「3 つとも同じ」の表は、全体の関係のうち何割が「2 つずつ」で説明できる？
-- 4. XOR の表のマスを 1 つ少しだけ変えると、D(p2‖p1) は 0 からどう動く？
-- 5. 暗号の「秘密分散」は XOR と同じ形：2 人だけでは何も分からず、3 人そろうと分かる

-- ---- 自己テスト
selfTest :: [Check]
selfTest =
  [ ("I(X;Y) = D(p‖p(x)p(y)) = H(X) + H(Y) − H(X,Y)（200 個の表、誤差 1e-12）", all (\(a, _, _, _) -> a) r1)
  , ("I ≥ 0、独立な表なら I = 0", all (\(_, b, c, _) -> b && c) r1)
  , ("どの独立な r でも D(p‖r) = I + D(q‖r)（ピタゴラス、誤差 1e-10）", all (\(_, _, _, d) -> d) r1)
  , ("3 変数：D(p‖p1) = D(p‖p2) + D(p2‖p1)（50 個、誤差 1e-10）", all fst r2)
  , ("IPF の p2 は 3 つのペアの周辺分布をすべて守る（誤差 1e-12）", all snd r2)
  , ("XOR：ペアの相互情報量はすべて 0、3 つ全体の関係 D(p‖p2) = 1 ビット"
    , all (\(a, b) -> mutualInfo (pairMarg xorT a b) 2 < 1e-15) [(0, 1), (1, 2), (0, 2)] && abs (bitsOf (divKL xorT (ipfPairwise xorT)) - 1) < 1e-12) ]
  where
    (r1, r2) = withSeed 1212 $ do
      a1 <- mapM (\n -> do
                    let k = 2 + n `mod` 3
                    t <- rndDist (k * k) 1e-4
                    a <- rndDist k 0
                    b <- rndDist k 0
                    let (px, py) = marg2 t k
                        i = mutualInfo t k
                        ind = prod2 t k
                        r = [ x * y | x <- a, y <- b ]
                    pure ( not (abs (i - (entropyH px + entropyH py - entropyH t)) > 1e-12)
                         , i >= 0
                         , not (mutualInfo ind k > 1e-14)
                         , not (abs (divKL t r - i - divKL ind r) > 1e-10) )) [0 .. 199 :: Int]
      a2 <- mapM (\_ -> do
                    t <- rndDist 8 1e-3
                    let p1 = prod3 t
                        p2 = ipfPairwise t
                    pure ( not (abs (divKL t p1 - divKL t p2 - divKL p2 p1) > 1e-10)
                         , and [ not (abs (v - m) > 1e-12) | (a, b) <- [(0, 1), (1, 2), (0, 2)], (v, m) <- zip (pairMarg p2 a b) (pairMarg t a b) ] )) [1 .. 50 :: Int]
      pure (a1, a2)
    xorT = normP [ if (i .&. 1) == (bitOf i 0 `xor` bitOf i 1) then 1 else 0 | i <- [0 .. 7 :: Int] ]

-- ==================================================
-- ここから下は「情報幾何の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。確率分布 p = [p1, p2, ...]（合計 1）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（等高線地図の色）
type RGB = (Float, Float, Float)

bgC, ink, orange, violet, teal, rose, lime, sand, dimc, panel, wire :: RGB
bgC = (22, 33, 52)                            -- 背景（深いスレートブルー）
ink = (240, 236, 226)                         -- 文字（紙の白）
orange = (255, 150, 75)
violet = (170, 140, 255)
teal = (95, 205, 190)
rose = (255, 110, 125)
lime = (150, 225, 120)
sand = (235, 210, 150)
dimc = (140, 152, 175)
panel = (32, 46, 70)
wire = (62, 80, 110)

-- 結果ごとの色（8 色をくり返す）
outC :: Int -> RGB
outC i = at' [(255, 150, 75), (95, 205, 190), (170, 140, 255), (235, 210, 150), (255, 110, 125), (150, 225, 120), (120, 175, 255), (230, 140, 210)] (i `mod` 8) ink

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

fin :: Double -> Double -> Double             -- NaN や ∞ なら d
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float                         -- 描くときの Float（NaN・∞ は 0）
f2 = realToFrac . fin 0

d2 :: Float -> Double                         -- マウスの位置などを計算用の Double に
d2 = realToFrac

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

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

-- 厳密にリストを評価する（遅延評価の「つけ」をためない）
forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Int
jsRound v0 = let v = clampv v0 (-1e15) 1e15
                 r = floor v
             in if v - fromIntegral r >= 0.5 then r + 1 else r

-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）。∞ は inf
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

-- fmtP(p, d)：(0.50, 0.30, 0.20) の形
fmtP :: Int -> [Double] -> String
fmtP d p = "(" ++ intercalate ", " (map (fmtN d) p) ++ ")"

-- v.toExponential(1)：1.2e-15 の形
fmtE :: Double -> String
fmtE v = if isNaN v || isInfinite v then fmtN 2 v else plusE (showEFloat (Just 1) v "")
  where plusE s = case break (== 'e') s of
          (m, 'e' : '-' : ex) -> m ++ "e-" ++ ex
          (m, 'e' : ex) -> m ++ "e+" ++ ex
          _ -> s

-- 左上のタイトルと操作の説明（説明は \n で改行、行の間 19）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- 長方形 r = {x, y, w, h}
data Box = Box { bxX :: !Double, bxY :: !Double, bxW :: !Double, bxH :: !Double }

inRect :: Double -> Double -> Box -> Bool
inRect x y r = x >= bxX r && x <= bxX r + bxW r && y >= bxY r && y <= bxY r + bxH r

-- 等高線の地図（作っておいた画像と、置く場所）
data Field = Field { fdPic :: Picture, fdX :: !Float, fdY :: !Float, fdW :: !Float, fdH :: !Float, fdIW :: !Int, fdIH :: !Int }

drawField :: Maybe Field -> Picture           -- 等高線の地図（画像）を描く
drawField mf = case mf of
  Just f -> imageBox (fdX f) (fdY f) (fdW f) (fdH f) (fdIW f) (fdIH f) (fdPic f)
  Nothing -> Blank

-- 地図の作り方（p5.js 版の opt）
data FieldOpt = FieldOpt { foLevel :: !Double, foVmin :: !Double, foVmax :: !Double, foC0 :: RGB, foC1 :: RGB, foCap :: !Double, foRes :: !Int }

fieldOpt :: FieldOpt                          -- p5.js 版の既定の値（res は 0 = 道具ごとの既定）
fieldOpt = FieldOpt 0.1 0 1 (40, 60, 95) orange (1 / 0) 0

-- 色の帯と線の画像を作る（vals = 各画素の値、NaN は外 = 背景の色。edge の線は背景の上に重ねた色）
bandImage :: Int -> Int -> U.Vector Double -> (Double -> Int) -> (Double -> Double) -> FieldOpt -> Float -> Float -> Picture
bandImage gw gh vals bandOf tOf opt edgeV edgeA = pixelImage gw gh 1 px
  where
    (b0, b1, b2) = bgC
    blend a c bgc = a / 255 * c + (1 - a / 255) * bgc
    (c00, c01, c02) = foC0 opt
    (c10, c11, c12) = foC1 opt
    w8 :: Float -> Word8
    w8 v = fromIntegral (max 0 (min 255 (round v :: Int)))
    px i j =
      let k = j * gw + i
          v = vals U.! k
          band = bandOf v
          okN q = not (isNaN q)
          edge = v < foCap opt && ((i + 1 < gw && okN (vals U.! (k + 1)) && bandOf (vals U.! (k + 1)) /= band)
                                   || (j + 1 < gh && okN (vals U.! (k + gw)) && bandOf (vals U.! (k + gw)) /= band))
          t = realToFrac (clampv (tOf v) 0 1)
          sh = if odd band then 0.9 else 1
          mixc a b = (a + (b - a) * t) * sh
      in if isNaN v then (w8 b0, w8 b1, w8 b2)
         else if edge then (w8 (blend edgeA edgeV b0), w8 (blend edgeA edgeV b1), w8 (blend edgeA edgeV b2))
         else (w8 (mixc c00 c10), w8 (mixc c01 c11), w8 (mixc c02 c12))

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

-- n 回くり返す（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- 標準正規分布（ボックス-ミュラー法、u = 1 − rnd() を先に引く）
gaussRnd :: R Double
gaussRnd = do
  u0 <- rnd
  v <- rnd
  pure (sqrt (-2 * log (1 - u0)) * cos (2 * pi * v))

-- でたらめな確率分布（k 個、どれも floor 以上）
rndDist :: Int -> Double -> R [Double]
rndDist k fl = do
  es <- mapM (\_ -> fmap (\u -> -log (1 - u) + 1e-9) rnd) [1 .. k]
  pure (mixFloor (normP es) fl)

-- p にしたがって番号を 1 つ（rnd() を 1 回）
sampleIndex :: [Double] -> R Int
sampleIndex p = do
  u <- rnd
  let go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- ---- 情報の道具：確率分布 p = [p1, p2, ...]（合計 1）。log(0) は 0·log 0 = 0 として扱う
tiny :: Double
tiny = 1e-300

posInf :: Double
posInf = 1 / 0

normP :: [Double] -> [Double]
normP p = let s = sumOf (map (max 0) p)
          in if not (s > 0) then map (const (1 / i2d (length p))) p else map (\v -> max 0 v / s) p

-- どの確率も f 以上に（0 を避ける）
mixFloor :: [Double] -> Double -> [Double]
mixFloor p f = if f == 0 then p else normP (map (max f) p)

safeLog :: Double -> Double
safeLog v = log (max v tiny)

-- エントロピー（ナット）
entropyH :: [Double] -> Double
entropyH = foldl' (\h v -> if v > 0 then h - v * log v else h) 0

-- KL ダイバージェンス D(p‖q)（ナット）。q が 0 で p > 0 なら ∞
divKL :: [Double] -> [Double] -> Double
divKL p q = go 0 (zip p (q ++ repeat 0))
  where go !d [] = max 0 d
        go !d ((a, b) : rest)
          | a <= 0 = go d rest
          | b <= 0 = posInf
          | otherwise = go (d + a * log (a / b)) rest

-- 交差エントロピー H(p, q)
crossH :: [Double] -> [Double] -> Double
crossH p q = foldl' (\h (a, b) -> if a > 0 then h - a * safeLog b else h) 0 (zip p q)

bhatta :: [Double] -> [Double] -> Double     -- Σ √(p q)
bhatta p q = min 1 (foldl' (\s (a, b) -> s + sqrt (max 0 (a * b))) 0 (zip p q))

-- フィッシャー-ラオ距離 = 球の大円の長さ
fisherRao :: [Double] -> [Double] -> Double
fisherRao p q = 2 * acos (clampv (bhatta p q) (-1) 1)

-- Σ(√p − √q)²
hellingerSq :: [Double] -> [Double] -> Double
hellingerSq p q = foldl' (\s (a, b) -> let d = sqrt a - sqrt b in s + d * d) 0 (zip p q)

tvDist :: [Double] -> [Double] -> Double     -- 全変動距離
tvDist p q = foldl' (\s (a, b) -> s + abs (a - b)) 0 (zip p q) / 2

-- α-ダイバージェンス（甘利の流儀）。α → −1 で D(p‖q)、α → +1 で D(q‖p)
alphaDiv :: [Double] -> [Double] -> Double -> Double
alphaDiv p q a
  | abs (a + 1) < 1e-12 = divKL p q
  | abs (a - 1) < 1e-12 = divKL q p
  | otherwise = let ea = (1 - a) / 2
                    eb = (1 + a) / 2
                    s = foldl' (\acc (x, y) -> acc + max x 0 ** ea * max y 0 ** eb) 0 (zip p q)
                in max 0 ((4 / (1 - a * a)) * (1 - s))

-- m-測地線：確率をそのまま混ぜる
mixM :: [Double] -> [Double] -> Double -> [Double]
mixM p q t = zipWith (\v w -> (1 - t) * v + t * w) p q

-- e-測地線：対数で混ぜて正規化
mixE :: [Double] -> [Double] -> Double -> [Double]
mixE p q t = normP (zipWith (\v w -> exp ((1 - t) * safeLog v + t * safeLog w)) p q)

-- α-測地線：u^((1−α)/2) で混ぜて正規化（α = −1 は m、α = +1 は e）
mixAlpha :: [Double] -> [Double] -> Double -> Double -> [Double]
mixAlpha p q t a
  | abs (a - 1) < 1e-9 = mixE p q t
  | otherwise = let ex = (1 - a) / 2
                in normP (zipWith (\v w -> ((1 - t) * v ** ex + t * w ** ex) ** (1 / ex)) p q)

bitsOf :: Double -> Double
bitsOf nat = nat / log 2

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

mouseD :: Env -> (Double, Double)             -- 計算用の Double で
mouseD env = let (x, y) = mouseP env in (d2 x, d2 y)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

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

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（iw × ih の画像）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral (max 1 iw)) (negate h / fromIntegral (max 1 ih)) pic)

-- ---- 自己テストの道具
type Check = (String, Bool)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [Check] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestOk :: [Check] -> Bool
selfTestOk = all snd

selfTestLine :: [Check] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [Check] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col lime else col rose)
