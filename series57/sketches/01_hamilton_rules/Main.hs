-- ============================================
-- 57-01 ハミルトンの発見 ─ i² = j² = k² = ijk = −1
-- 1, i, j, k の掛け算表を qMul（4 次元ベクトルの積）で計算して並べる。ij = k なのに ji = −k
-- 一般の四元数の積は、16 個の「基本の積」を表から足し合わせたもの（分配法則）。それが qMul の式と一致する
-- 操作：表のマスをクリック a·b と b·a を比べる / R 新しい p, q / スペース i → j → k の輪を回す / ← → 列を変える
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate, isPrefixOf)
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

names :: [String]
names = ["1", "i", "j", "k"]

-- 1843 年の表（行 × 列）
hamilton :: [[String]]
hamilton = [["1", "i", "j", "k"], ["i", "-1", "k", "-j"], ["j", "-k", "-1", "i"], ["k", "j", "-i", "-1"]]

-- ★ 一般の四元数 p, q
pq0 :: (Q, Q)
pq0 = (Q 1 2 (-1) 0.5, Q 0.5 (-1) 3 2)

data World = World
  { wSelA, wSelB :: !Int               -- 選んだマス（行 a、列 b）
  , wPQ          :: !(Q, Q)
  , wSpin        :: !Double
  , wSpinning    :: !Bool
  , wTest        :: [(String, Bool)] }

world0 :: World
world0 = World 1 2 pq0 0 False []

main :: IO ()
main = runSketch (sketch "57-01 Hamilton's discovery" world0)
  { skBack  = rgbC 18 20 36
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (if wSpinning w then w { wSpin = wSpin w + 0.01 } else w)
  , skDraw  = draw
  , skEvent = event
  }

basis :: Int -> Q
basis a = Q (b 0) (b 1) (b 2) (b 3) where b t = if t == a then 1 else 0

nameOf :: Int -> String
nameOf = getAt "?" names

-- ±1, ±i, ±j, ±k なら名前に
qName :: Q -> String
qName v = case [ (if s < 0 then "-" else "") ++ nameOf a | a <- [0 .. 3], s <- [1, -1], qDist v (qScl (basis a) s) < 1e-12 ] of
  (nm : _) -> nm
  [] -> qStr 2 v

-- 分配法則：p q = Σ p_a q_b (e_a e_b)、e_a e_b は表から
expandMul :: Q -> Q -> (Q, [(Int, Int, Q)])
expandMul p q = (foldl' (\r (a, b, e) -> qAdd r (qScl e (qAt p a * qAt q b))) (Q 0 0 0 0) terms, terms)
  where terms = [ (a, b, qMul (basis a) (basis b)) | a <- [0 .. 3], b <- [0 .. 3] ]

-- 表の位置（左上 x0, y0、マスの大きさ cs）
tx0, ty0, cs :: Float
tx0 = 60; ty0 = 150; cs = 86

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 掛け算表
  concat [ cell r c | r <- [-1 .. 3], c <- [-1 .. 3] ]
  ++ [ say (nameOf (wSelA w) ++ " * " ++ nameOf (wSelB w) ++ " = " ++ qName ab) tx0 (ty0 + 5 * cs + 40) 22 (grayC 240)
     , say (nameOf (wSelB w) ++ " * " ++ nameOf (wSelA w) ++ " = " ++ qName ba ++ (if qDist ab ba < 1e-12 then "   (the same)" else "   (opposite sign!)"))
           tx0 (ty0 + 5 * cs + 72) 22 (if qAt ab 0 == qAt ba 0 && qDist ab ba < 1e-12 then rgbC 130 230 150 else rgbC 255 150 150)
     , say ("as 4D vectors: " ++ qStr 0 a ++ " x " ++ qStr 0 b ++ " = " ++ qStr 0 ab) tx0 (ty0 + 5 * cs + 100) 14 (grayC 180) ]
  -- ---- i → j → k の輪
  ++ [ Color (rgbaC 255 200 80 120) (circleLine 2 cx cy rr) ]
  ++ concat [ [ Color (rgbC 255 200 80) (Translate px py (Rotate (negate (f2 (am + pi / 2)) * 180 / pi) (Polygon [(8, 0), (-6, -6), (-6, 6)])))
              , Color (rgbC 30 40 66) (at (nx, ny) (disc 23)), Color (rgbC 255 200 80) (circleLine 1 nx ny 23)
              , sayCC (nameOf (n + 1)) nx ny 24 white ]
            | n <- [0 .. 2 :: Int]
            , let an = -pi / 2 + wSpin w + (fromIntegral n * 2 * pi) / 3
                  am = an + pi / 3
                  px = cx + rr * f2 (cos am); py = cy + rr * f2 (sin am)
                  nx = cx + rr * f2 (cos an); ny = cy + rr * f2 (sin an) ]
  ++ [ say "Along the arrows: +   ij = k, jk = i, ki = j" (cx - 160) (cy + rr + 50) 14 (grayC 200)
     , say "Against them:     -   ji = -k, kj = -i, ik = -j" (cx - 160) (cy + rr + 72) 14 (grayC 200) ]
  -- ---- 一般の積を表から組み立てる
  ++ [ say ("p = " ++ qStr 2 p) tx ty 17 (grayC 240), say ("q = " ++ qStr 2 q) tx (ty + 24) 17 (grayC 240)
     , say "pq = sum p_a q_b (e_a e_b), 16 terms" tx (ty + 52) 14 (grayC 175) ]
  ++ [ say ((if c >= 0 then "+" else "-") ++ fmt 2 (abs c) ++ " (" ++ nameOf ia ++ nameOf ib ++ " = " ++ qName e ++ ")")
           (tx + fromIntegral (n `mod` 2) * 170) (ty + 76 + fromIntegral (n `div` 2) * 18) 14 (if nm0 e then rgbC 255 150 150 else grayC 200)
     | (n, (ia, ib, e)) <- zip [0 :: Int ..] terms, let c = qAt p ia * qAt q ib ]
  ++ [ say ("table: " ++ qStr 3 exR) tx (ty + 240) 15 (rgbC 130 230 150)
     , say ("qMul : " ++ qStr 3 (qMul p q)) tx (ty + 264) 15 (rgbC 130 230 150)
     , say ("q p  : " ++ qStr 3 (qMul q p) ++ " (differs)") tx (ty + 288) 15 (rgbC 255 150 150) ]
  -- ---- ブルーム橋
  ++ [ say "16 October 1843, Broom Bridge, Dublin" 640 (hh - 120) 16 (rgbC 255 210 110)
     , sayBox ("On a walk, Hamilton realised that division works with 'quadruples' instead of 'triples' and carved i^2 = j^2 = k^2 = ijk = -1 "
               ++ "into the stone of the bridge. A plaque marks the spot today.") 640 (hh - 110) (ww - 660) 14 (grayC 200)
     , say "Hamilton's discovery   i^2 = j^2 = k^2 = ijk = -1" 20 36 20 (grayC 240)
     , say "Click a cell of the table: compare a*b with b*a (red cell)   R: new p, q   Space: turn the i -> j -> k ring   Left/Right: column" 20 62 15 (grayC 175)
     , say "Green = chosen a*b, red = swapped b*a. 1 changes nothing. Among i, j, k the order changes the sign" 20 86 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    ww = envW env; hh = envH env
    a = basis (wSelA w); b = basis (wSelB w)
    ab = qMul a b; ba = qMul b a
    cx = 640; cy = 300; rr = 90
    (p, q) = wPQ w
    (exR, terms) = expandMul p q
    tx = 900; ty = 130
    cell r c
      | r < 0 && c < 0 = [ sayCC "a \\ b" (x + cs / 2) (y + cs / 2) 15 (grayC 150) ]
      | r < 0 || c < 0 = [ sayCC (nameOf (if r < 0 then c else r)) (x + cs / 2) (y + cs / 2) 30 (rgbC 255 210 110) ]
      | otherwise =
          let nm = qName (qMul (basis r) (basis c))
              on = r == wSelA w && c == wSelB w
              tr = r == wSelB w && c == wSelA w
              neg = "-" `isPrefixOf` nm
              bg | on = rgbC 60 120 90
                 | tr = rgbC 120 70 70
                 | neg = rgbC 40 36 60
                 | otherwise = rgbC 30 40 66
          in [ Color bg (rectTL x y cs cs), Color (rgbC 70 80 120) (frameTL 1 x y cs cs)
             , sayCC nm (x + cs / 2) (y + cs / 2) 30 (if neg then rgbC 255 150 150 else grayC 235) ]
      where x = tx0 + fromIntegral (c + 1) * cs
            y = ty0 + fromIntegral (r + 1) * cs

nm0 :: Q -> Bool
nm0 e = any (< 0) (qList e)

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP5 env
        c = floor ((mx - realToFrac tx0) / realToFrac cs) - 1 :: Int
        r = floor ((my - realToFrac ty0) / realToFrac cs) - 1 :: Int
    in pure (if r >= 0 && r < 4 && c >= 0 && c < 4 then w { wSelA = r, wSelB = c } else w)
  EventKey (Char ch) Down _ _
    | ch `elem` "rR" -> do
        let one = (\v -> jsRound (v * 8 - 4) / 2) <$> randomRIO (0, 1 :: Double)
        [a0, a1, a2, a3, b0, b1, b2, b3] <- replicateM 8 one
        pure w { wPQ = (Q a0 a1 a2 a3, Q b0 b1 b2 b3) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wSpinning = not (wSpinning w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wSelB = (wSelB w + 1) `mod` 4 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wSelB = (wSelB w + 3) `mod` 4 }
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 8 項目・同じ種・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("i^2 = j^2 = k^2 = -1", all (\e -> qDist (qMul e e) m1 == 0) [ii, jj, kk])
  , ("ijk = -1", qDist (qMul (qMul ii jj) kk) m1 == 0)
  , ("ij = k, jk = i, ki = j", qDist (qMul ii jj) kk == 0 && qDist (qMul jj kk) ii == 0 && qDist (qMul kk ii) jj == 0)
  , ("ji = -k, kj = -i, ik = -j (not commutative)"
    , qDist (qMul jj ii) (qScl kk (-1)) == 0 && qDist (qMul kk jj) (qScl ii (-1)) == 0 && qDist (qMul ii kk) (qScl jj (-1)) == 0)
  , ("the 4 x 4 table made with qMul = Hamilton's table"
    , and [ qName (qMul (basis r) (basis c)) == getAt "" (getAt [] hamilton r) c | r <- [0 .. 3], c <- [0 .. 3] ])
  , ("product summed from the table (distributive law) = qMul", e1 < 1e-12)
  , ("associative (pq)s = p(qs) (200 triples)", e2 < 1e-12)
  , ("1 is the identity, but in general pq /= qp (" ++ show nc ++ " / 200 pairs)", e3 == 0 && nc == 200) ]
  where
    one = Q 1 0 0 0; m1 = Q (-1) 0 0 0
    ii = basis 1; jj = basis 2; kk = basis 3
    trials = evalGen 2 $ replicateM 200 $ do
      p0 <- qRandom; sp <- rndG
      q0 <- qRandom; sq <- rndG
      s <- qRandom
      pure (qScl p0 (3 * sp), qScl q0 (3 * sq), s)
    e1 = maximum (0 : [ qDist (fst (expandMul p q)) (qMul p q) | (p, q, _) <- trials ])
    e2 = maximum (0 : [ qDist (qMul (qMul p q) s) (qMul p (qMul q s)) | (p, q, s) <- trials ])
    e3 = maximum (0 : concat [ [qDist (qMul one p) p, qDist (qMul p one) p] | (p, _, _) <- trials ])
    nc = length [ () | (p, q, _) <- trials, qDist (qMul p q) (qMul q p) > 1e-6 ]

-- ---------- やってみよう ----------
-- 1. i·j と j·i、j·k と k·j をクリックで比べよう。符号はどうなる？ 輪の矢印と見比べよう
-- 2. 表の対角線（i², j², k²）はなぜ全部 −1？ 1 の行と列はなぜ「そのまま」？
-- 3. ijk = −1 を、(ij)k = k·k と i(jk) = i·i の 2 通りで計算しよう（結合法則）
-- 4. R で p, q を変えよう。p q と q p で、ちがうのはどの成分？（実部は同じ → 02 へ）
-- 5. ハミルトンが 3 つ組（1, i, j）で割り算を作れなかった理由を考えよう。i·j はどこに行く？

-- ==================================================
-- ここから下は「四元数の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）。自己テストで使う
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rnd :: Rng -> (Double, Rng)
rnd (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 標準正規分布（ボックス＝ミュラー法）
rndNormal :: Rng -> (Double, Rng)
rndNormal r0 = let (a, r1) = rnd r0
                   (v, r2) = rnd r1
                   u = 1 - a
               in (sqrt (-2 * log u) * cos (2 * pi * v), r2)

-- 乱数を引きながら計算する小さなモナド（JavaScript と同じく左から順に引く）
newtype Gen a = Gen { runGen :: Rng -> (a, Rng) }

instance Functor Gen where
  fmap f (Gen g) = Gen (\r -> case g r of (a, r1) -> (f a, r1))

instance Applicative Gen where
  pure a = Gen (\r -> (a, r))
  Gen f <*> Gen g = Gen (\r -> case f r of
                                 (h, !r1) -> case g r1 of (a, !r2) -> (h a, r2))

instance Monad Gen where
  Gen g >>= k = Gen (\r -> case g r of (a, !r1) -> runGen (k a) r1)

rndG, rndNormalG :: Gen Double
rndG = Gen rnd
rndNormalG = Gen rndNormal

-- 種 s から始めて m を計算する
evalGen :: Word32 -> Gen a -> a
evalGen s m = fst (runGen m (seedRng s))

clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else v

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標に
mouseP5 :: Env -> (Double, Double)
mouseP5 env = let (x, y) = envMouse env in (realToFrac (x + envW env / 2), realToFrac (envH env / 2 - y))

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC r g b a = makeColorI r g b (max 0 (min 255 a))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 0〜255 の小数で色を作る（明るさを掛けた色など）
rgbD :: Double -> Double -> Double -> Double -> Color
rgbD r g b a = makeColor (c r) (c g) (c b) (c a)
  where c v = f2 (clampv (v / 255) 0 1)

-- 左下が (x, y) の文字（y は文字の下の線 = p5.js の BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央（textAlign(CENTER, CENTER)）
sayCC :: String -> Float -> Float -> Float -> Color -> Picture
sayCC s x y size c = sayC s x (y + size * 0.36) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 w の箱に折り返して書く（p5.js の text(s, x, y, w, h)。(x, y) は箱の左上）
sayBox :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayBox s x y w size c = Pictures [ say ln x (y + size + fromIntegral k * size * 1.25) size c | (k, ln) <- zip [0 :: Int ..] (wrapWords (words s)) ]
  where
    wrapWords [] = []
    wrapWords (h : t) = go h t
    go cur [] = [cur]
    go cur (x1 : xs)
      | textW (cur ++ " " ++ x1) size <= w = go (cur ++ " " ++ x1) xs
      | otherwise = cur : go x1 xs

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 中心 (x, y)、半径 r の円の線（太さ wt）
circleLine :: Float -> Float -> Float -> Float -> Picture
circleLine wt x y r = Translate x y (ThickCircle r wt)

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- ---- 数の道具
-- Math.hypot（JavaScript と同じ計算：最大値で割ってカハンの足し算）
hypotL :: [Double] -> Double
hypotL xs =
  let as = map abs xs
      m = maximum (0 : as)
      go (!s, !c) v = let n = v / m
                          summand = n * n - c
                          pre = s + summand
                      in (pre, (pre - s) - summand)
  in if any isInfinite as then 1 / 0 else if any isNaN as then 0 / 0 else if m == 0 then 0
     else sqrt (fst (foldl' go (0, 0) as)) * m

hypot3 :: Double -> Double -> Double -> Double
hypot3 a b c = hypotL [a, b, c]

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- JavaScript の % （0 の方へ切り捨てた余り）
jsMod :: Double -> Double -> Double
jsMod a b = a - b * fromIntegral (truncate (a / b) :: Integer)

-- Math.round（.5 は上へ）
jsRound :: Double -> Double
jsRound v = fromIntegral (floor (fin 0 v + 0.5) :: Integer)

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v0 = let v = fin 0 v0
               s = showFFloat (Just n) v ""
           in if all (`elem` "-0.") s then dropWhile (== '-') s else s

-- 指数で書く（toExponential(n)、例 1.2e-16）
expo :: Int -> Double -> String
expo n v0
  | v == 0 = fmt n 0 ++ "e+0"
  | otherwise = let s = showEFloat (Just n) v ""
                    (m, e) = break (== 'e') s
                    e' = drop 1 e
                in m ++ "e" ++ (if take 1 e' == "-" then e' else '+' : e')
  where v = fin 0 v0
-- ---- 四元数 Q w x y z = w + xi + yj + zk
data Q = Q !Double !Double !Double !Double deriving (Eq, Show)

qMul :: Q -> Q -> Q                    -- ハミルトン積（i² = j² = k² = ijk = −1）
qMul (Q a0 a1 a2 a3) (Q b0 b1 b2 b3) =
  Q (a0 * b0 - a1 * b1 - a2 * b2 - a3 * b3)
    (a0 * b1 + a1 * b0 + a2 * b3 - a3 * b2)
    (a0 * b2 - a1 * b3 + a2 * b0 + a3 * b1)
    (a0 * b3 + a1 * b2 - a2 * b1 + a3 * b0)

qConj :: Q -> Q                        -- 共役 q*
qConj (Q w x y z) = Q w (-x) (-y) (-z)

qAdd, qSub :: Q -> Q -> Q
qAdd (Q a b c d) (Q e f g h) = Q (a + e) (b + f) (c + g) (d + h)
qSub (Q a b c d) (Q e f g h) = Q (a - e) (b - f) (c - g) (d - h)

qScl :: Q -> Double -> Q
qScl (Q a b c d) s = Q (a * s) (b * s) (c * s) (d * s)

qDot :: Q -> Q -> Double
qDot (Q a b c d) (Q e f g h) = a * e + b * f + c * g + d * h

qNorm :: Q -> Double                   -- |q|
qNorm a = sqrt (qDot a a)

qUnit :: Q -> Q
qUnit a = let n = qNorm a in if n > 1e-300 then qScl a (1 / n) else Q 1 0 0 0

qInv :: Q -> Q                         -- q⁻¹ = q*/|q|²
qInv a = let n2 = qDot a a in if n2 > 1e-300 then qScl (qConj a) (1 / n2) else Q 0 0 0 0

qDist :: Q -> Q -> Double
qDist a b = qNorm (qSub a b)

qList :: Q -> [Double]
qList (Q w x y z) = [w, x, y, z]

qAt :: Q -> Int -> Double              -- 成分 0〜3
qAt q i = getAt 0 (qList q) i

qAxisAngle :: Vec -> Double -> Q       -- 軸 n まわりに角度 th の回転：cos(th/2) + sin(th/2) n
qAxisAngle n th = let Vec ux uy uz = v3Unit n
                      s = sin (th / 2)
                  in Q (cos (th / 2)) (ux * s) (uy * s) (uz * s)

qRot :: Q -> Vec -> Vec                -- v を q v q* で回す（v は 3 次元ベクトル → 純四元数）
qRot q (Vec x y z) = let Q _ a b c = qMul (qMul q (Q 0 x y z)) (qConj q) in Vec a b c

qToMat :: Q -> Mat                     -- 同じ q から作った 3×3 回転行列（単位四元数のとき）
qToMat (Q w x y z) =
  Mat (Vec (1 - 2 * (y * y + z * z)) (2 * (x * y - w * z)) (2 * (x * z + w * y)))
      (Vec (2 * (x * y + w * z)) (1 - 2 * (x * x + z * z)) (2 * (y * z - w * x)))
      (Vec (2 * (x * z - w * y)) (2 * (y * z + w * x)) (1 - 2 * (x * x + y * y)))

qExp :: Q -> Q                         -- exp(w + v) = e^w (cos|v| + sin|v| v/|v|)
qExp (Q a0 a1 a2 a3) =
  let vn = hypot3 a1 a2 a3
      ew = exp a0
      s = if vn > 1e-12 then sin vn / vn else 1 - (vn * vn) / 6
  in Q (ew * cos vn) (ew * s * a1) (ew * s * a2) (ew * s * a3)

qLog :: Q -> Q                         -- log q = ln|q| + acos(w/|q|) v/|v|
qLog a@(Q a0 a1 a2 a3) =
  let n = qNorm a
      vn = hypot3 a1 a2 a3
      th = atan2 vn a0
      s = if vn > 1e-12 then th / vn else 1 / max n 1e-300
  in Q (log (max n 1e-300)) (s * a1) (s * a2) (s * a3)

qSlerp :: Q -> Q -> Double -> Bool -> Q   -- 球面線形補間。shortest なら −b と近い方の道
qSlerp a b0 t shortest =
  let d0 = qDot a b0
      (b, d1) = if shortest && d0 < 0 then (qScl b0 (-1), -d0) else (b0, d0)
      d = clampv d1 (-1) 1
      om = acos d
      so = sin om
  in if so < 1e-9 then qUnit (qAdd (qScl a (1 - t)) (qScl b t))       -- ほぼ同じ向き → lerp で十分
     else qAdd (qScl a (sin ((1 - t) * om) / so)) (qScl b (sin (t * om) / so))

qNlerp :: Q -> Q -> Double -> Bool -> Q   -- 正規化した線形補間
qNlerp a b0 t shortest =
  let b = if shortest && qDot a b0 < 0 then qScl b0 (-1) else b0
  in qUnit (qAdd (qScl a (1 - t)) (qScl b t))

qAngle :: Q -> Q -> Double             -- 2 つの姿勢の間の回転角（0〜π）
qAngle a b = let Q d0 d1 d2 d3 = qMul (qConj (qUnit a)) (qUnit b)
             in 2 * atan2 (hypot3 d1 d2 d3) (abs d0)

qRandom :: Gen Q                       -- 一様にランダムな単位四元数（S³ の上の点）
qRandom = do
  a <- rndNormalG; b <- rndNormalG; c <- rndNormalG; d <- rndNormalG
  pure (qUnit (Q a b c d))

qStr :: Int -> Q -> String             -- (w, x, y, z) を小数点以下 dg 桁で
qStr dg q = "(" ++ intercalate ", " [ fmt dg (if abs v < 0.5 * 10 ** negate (fromIntegral dg) then 0 else v) | v <- qList q ] ++ ")"

-- ---- 3 次元ベクトルと 3×3 行列（行列は 3 つの行）
data Vec = Vec !Double !Double !Double deriving (Eq, Show)
data Mat = Mat !Vec !Vec !Vec deriving (Eq, Show)

v3Dot :: Vec -> Vec -> Double
v3Dot (Vec a b c) (Vec d e f) = a * d + b * e + c * f

v3Cross :: Vec -> Vec -> Vec
v3Cross (Vec a0 a1 a2) (Vec b0 b1 b2) = Vec (a1 * b2 - a2 * b1) (a2 * b0 - a0 * b2) (a0 * b1 - a1 * b0)

v3Len :: Vec -> Double
v3Len (Vec a b c) = hypot3 a b c

v3Unit :: Vec -> Vec
v3Unit a@(Vec x y z) = let n = v3Len a in if n > 1e-300 then Vec (x / n) (y / n) (z / n) else Vec 0 0 1

v3Add, v3Sub :: Vec -> Vec -> Vec
v3Add (Vec a b c) (Vec d e f) = Vec (a + d) (b + e) (c + f)
v3Sub (Vec a b c) (Vec d e f) = Vec (a - d) (b - e) (c - f)

v3Scl :: Vec -> Double -> Vec
v3Scl (Vec a b c) s = Vec (a * s) (b * s) (c * s)

v3Dist :: Vec -> Vec -> Double
v3Dist (Vec a b c) (Vec d e f) = hypot3 (a - d) (b - e) (c - f)

v3List :: Vec -> [Double]
v3List (Vec a b c) = [a, b, c]

matVec :: Mat -> Vec -> Vec
matVec (Mat r0 r1 r2) v = Vec (v3Dot r0 v) (v3Dot r1 v) (v3Dot r2 v)

matT :: Mat -> Mat
matT (Mat (Vec a b c) (Vec d e f) (Vec g h i)) = Mat (Vec a d g) (Vec b e h) (Vec c f i)

matMul :: Mat -> Mat -> Mat
matMul (Mat r0 r1 r2) b = let Mat c0 c1 c2 = matT b
                              row r = Vec (v3Dot r c0) (v3Dot r c1) (v3Dot r c2)
                          in Mat (row r0) (row r1) (row r2)

matDet :: Mat -> Double
matDet (Mat r0 r1 r2) = v3Dot r0 (v3Cross r1 r2)

matList :: Mat -> [[Double]]
matList (Mat a b c) = map v3List [a, b, c]

matDiff :: Mat -> Mat -> Double        -- 成分の差の最大
matDiff a b = maximum (0 : zipWith (\x y -> abs (x - y)) (concat (matList a)) (concat (matList b)))

matOrthoErr :: Mat -> Double           -- |MᵀM − I| の最大成分（回転行列なら 0）
matOrthoErr a = matDiff (matMul (matT a) a) matI

matI :: Mat
matI = Mat (Vec 1 0 0) (Vec 0 1 0) (Vec 0 0 1)
