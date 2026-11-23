-- ============================================
-- 58-10 符号 (p, q) ─ e_i² を +1 にするか −1 にするかで、いろいろな「数」が生まれる
-- クリフォード代数 Cl(p, q)：2 乗して +1 の基底が p 個、−1 の基底が q 個。掛け算のしくみ（並べかえの符号）は全部同じ
-- Cl(0,1) = 複素数、Cl(1,0) = 双曲数（分解型複素数）、Cl(0,2) = 四元数、Cl(1,3) = 時空の代数（11）、Cl(3,1) = 共形幾何（12）
-- 操作：← → 次元 n（1〜4）/ 1〜4 e1〜e4 の 2 乗を ± 切りかえ / 表のマスにマウス 計算のしかた / Q W E 例
-- ============================================
module Main (main) where

import Art
import Data.Bits (popCount, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (foldl', transpose)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

data World = World
  { wDim  :: !Int
  , wSig  :: [Double]          -- e1〜e4 の 2 乗
  , wAlg  :: Alg
  , wTest :: [(String, Bool)] }

vio, cya, sil, gold :: (Int, Int, Int)
vio = (180, 130, 255)
cya = (90, 220, 240)
sil = (215, 220, 230)
gold = (255, 210, 120)

main :: IO ()
main = runSketch (sketch "58-10 Signatures (p, q)" world0)
  { skBack  = rgbC 14 14 18
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = rebuild (World 2 [-1, -1, 1, 1] (makeAlgebra [1]) [])   -- ★ 最初は Cl(0,2) = 四元数

rebuild :: World -> World
rebuild w = w { wAlg = makeAlgebra (take (wDim w) (wSig w)) }

pq :: World -> (Int, Int)
pq w = let m = take (wDim w) (wSig w) in (length (filter (> 0) m), length (filter (< 0) m))

knownName :: World -> String
knownName w = case pq w of
  (1, 0) -> "split-complex numbers: j^2 = +1. (1 + j)(1 - j) = 0 -> nonzero numbers whose product is 0"
  (0, 1) -> "complex numbers C: e1^2 = -1 is the imaginary unit i"
  (2, 0) -> "plane GA G(2): same as real 2x2 matrices. Its even part (1, e12) is the complex numbers (03)"
  (1, 1) -> "same as real 2x2 matrices (the 2D version of spacetime, 11)"
  (0, 2) -> "quaternions H: i = e1, j = e2, k = e12 with i^2 = j^2 = k^2 = ijk = -1"
  (3, 0) -> "space GA G(3): same as complex 2x2 matrices (Pauli). Its even part is the quaternions (07)"
  (0, 3) -> "two copies of the quaternions, H + H"
  (1, 3) -> "spacetime algebra (STA): g0^2 = +1, g1^2 = g2^2 = g3^2 = -1 (Dirac matrices)"
  (3, 1) -> "conformal GA for the plane: e1, e2, e+ square to +1, e- to -1 (12)"
  (4, 0) -> "4D geometric algebra G(4)"
  (2, 2) -> "Cl(2,2): same as real 4x4 matrices"
  (p, q) -> "Cl(" ++ show p ++ "," ++ show q ++ "): an algebra of dimension " ++ show (2 ^ wDim w :: Int)

draw :: Env -> World -> Picture
draw env w = view env $
  -- 掛け算の表
  concat [ [ sayC (nameOf al i) (tx + (fromIntegral i + 1.5) * cw) (ty + ch / 2 + fs * 0.4) fsz (grayC 150)
           , sayC (nameOf al i) (tx + cw / 2) (ty + (fromIntegral i + 1.5) * ch + fs * 0.4) fsz (grayC 150) ]
         | i <- [0 .. nb - 1] ]
  ++ concat [ let s = sgn i j
                  x = tx + fromIntegral (j + 1) * cw
                  y = ty + fromIntegral (i + 1) * ch
                  inside = inCell i j
                  c = if s > 0 then vio else cya
              in [ Color (colA c (if inside then 230 else if i == j then 150 else 70)) (rectTL (x + 1) (y + 1) (cw - 2) (ch - 2))
                 , sayC ((if s < 0 then "-" else "") ++ nameOf al (i `xor` j)) (x + cw / 2) (y + ch / 2 + fs * 0.4) fsz (grayC 240) ]
            | i <- [0 .. nb - 1], j <- [0 .. nb - 1] ]
  -- 右：基底の 2 乗（−1 になるもの = 「虚数単位」の候補）
  ++ [ say (if nb > 8 then "squares" else "squares of the basis") rx ry 16 (col3 sil) ]
  ++ [ let sq = sgn k k in say (nameOf al k ++ "^2 = " ++ (if sq > 0 then "+1" else "-1")) rx (ry + fromIntegral (k + 1) * rowH) (realToFrac (max 14 (min 15 (rowH - 4)))) (col3 (if sq > 0 then vio else cya))
     | k <- [0 .. nb - 1] ]
  -- 文字
  ++ [ say ("Left/Right: n = " ++ show (wDim w) ++ "   1-" ++ show (wDim w) ++ ": flip the sign of e_i^2   Q complex   W quaternions   E spacetime   mouse: table") 20 34 17 (col3 sil)
     , say ("Cl(" ++ show p ++ ", " ++ show q ++ ")  =  " ++ intercalateS ", " [ "e" ++ show (i + 1) ++ "^2 = " ++ (if s > 0 then "+1" else "-1") | (i, s) <- zip [0 :: Int ..] (take (wDim w) (wSig w)) ]) 20 72 22 (col3 gold)
     , say (knownName w) 20 102 16 (col3 sil)
     , say "violet = +, cyan = -. Same table shape for every (p, q), only the signs change (bitmask XOR -> basis; swaps, e_i^2 -> sign)" 20 126 14 (grayC 150) ]
  ++ [ say hovTxt 20 (h - 40) 16 (col3 gold) | not (null hovTxt) ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    al = wAlg w
    nb = aNb al
    (p, q) = pq w
    w0 = realToFrac (envW env) :: Double
    h = realToFrac (envH env) :: Double
    -- マスの大きさ（gloss の文字は幅が広いので、横長のマスにする）
    cw = min 64 (fromIntegral (floor ((w0 - 230) / fromIntegral (nb + 1)) :: Int))
    ch = min 42 (fromIntegral (floor ((h - 230) / fromIntegral (nb + 1)) :: Int))
    fsz = if nb > 8 then 12 else 14 :: Float
    fs = realToFrac fsz :: Double
    tx = 30
    ty = 150
    sgn i j = aTbl al U.! (i * nb + j)
    (mx, my) = mouseP env
    inCell i j = let x = tx + fromIntegral (j + 1) * cw; y = ty + fromIntegral (i + 1) * ch in mx > x && mx < x + cw && my > y && my < y + ch
    hovTxt = case [ (i, j) | i <- [0 .. nb - 1], j <- [0 .. nb - 1], inCell i j ] of
      ((i, j) : _) ->
        let s = sgn i j
            sw = reorderSign i j
            common = [ (t, getAt 1 (wSig w) t) | t <- [0 .. wDim w - 1], testBit (i .&. j) t ]
            ms = product (map snd common)
        in nameOf al i ++ " x " ++ nameOf al j ++ " = " ++ (if s < 0 then "-" else "") ++ nameOf al (i `xor` j)
           ++ "  : " ++ (if sw > 0 then "even number of swaps (+)" else "odd number of swaps (-)")
           ++ concat [ ", e" ++ show (t + 1) ++ "^2 = " ++ (if m > 0 then "+1" else "-1") | (t, m) <- common ]
           ++ "  -> sign " ++ (if sw * ms > 0 then "+" else "-")
      [] -> ""
    rx = tx + (fromIntegral nb + 1.6) * cw
    ry = ty + 10
    rowH = min 26 ((h - ry - 60) / fromIntegral nb)

intercalateS :: String -> [String] -> String
intercalateS sep xs = case xs of
  [] -> ""
  (x : rest) -> x ++ concatMap (sep ++) rest

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey k Down _ _ -> rebuild (key k)
  _ -> w
  where
    key k = case k of
      SpecialKey KeyRight -> w { wDim = min 4 (wDim w + 1) }
      SpecialKey KeyLeft -> w { wDim = max 1 (wDim w - 1) }
      Char c | c >= '1' && c <= '4' && fromEnum c - fromEnum '0' <= wDim w ->
                 let i = fromEnum c - fromEnum '1' in w { wSig = [ if t == i then -s else s | (t, s) <- zip [0 ..] (wSig w) ] }
             | c `elem` "qQ" -> w { wDim = 1, wSig = [-1, 1, 1, 1] }
             | c `elem` "wW" -> w { wDim = 2, wSig = [-1, -1, 1, 1] }
             | c `elem` "eE" -> w { wDim = 4, wSig = [1, -1, -1, -1] }
      _ -> w

-- ---- 自己テスト（p5.js 版と同じ 7 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest = evalRng 10 $ do
  let algs = [ (m, makeAlgebra m) | n <- [1 .. 5], pp <- [0 .. n], let m = [ if i < pp then 1 else -1 | i <- [0 .. n - 1] ] ]
      e al i = blade al (1 `shiftL` i) 1
      ok1 = and [ nearM (gp al (e al i) (e al j)) (smul (gp al (e al j) (e al i)) (-1)) 1e-9 | (m, al) <- algs, i <- [0 .. length m - 1], j <- [0 .. length m - 1], i /= j ]
      ok2 = and [ nearM (gp al (e al i) (e al i)) (scalar al s) 1e-9 | (m, al) <- algs, (i, s) <- zip [0 ..] m ]
      assoc al = do
        rs <- mapM (\_ -> do x <- randM al; y <- randM al; z <- randM al; pure (nearM (gp al (gp al x y) z) (gp al x (gp al y z)) 1e-11)) [1 .. 10 :: Int]
        pure (and rs)
  a1 <- assoc (makeAlgebra [1, 1, 1])
  a2 <- assoc (makeAlgebra [1, -1, -1, -1])
  a3 <- assoc (makeAlgebra [1, 1, 1, 1, -1])
  let c1 = makeAlgebra [-1]
  okC <- and <$> mapM (\_ -> do
            a <- nextR; b <- nextR; c <- nextR; d <- nextR
            let z = gp c1 (U.fromList [a, b]) (U.fromList [c, d])
            pure (near (z U.! 0) (a * c - b * d) 1e-9 && near (z U.! 1) (a * d + b * c) 1e-9)) [1 .. 20 :: Int]
  let hq = makeAlgebra [-1, -1]
      qi = blade hq 1 1
      qj = blade hq 2 1
      qk = blade hq 3 1
      m1 = scalar hq (-1)
      dd = makeAlgebra [1]
      one = scalar dd 1
      jj = blade dd 1 1
      g2 = makeAlgebra [1, 1]
      e12 = blade g2 3 1
  pure
    [ ("every (p, q) with n <= 5: e_i e_j = -e_j e_i (i /= j)", ok1)
    , ("every (p, q) with n <= 5: e_i^2 = its sign (+1 or -1)", ok2)
    , ("associativity: random checks in G(3), Cl(1,3), Cl(4,1) (n = 5, 32 components)", a1 && a2 && a3)
    , ("Cl(0,1) product = complex product (a + bi)(c + di)", okC)
    , ("Cl(0,2): i = e1, j = e2, k = e12 give i^2 = j^2 = k^2 = ijk = -1, ij = k = -ji (quaternions)"
      , all (\x -> nearM x m1 1e-9) [gp hq qi qi, gp hq qj qj, gp hq qk qk, gps hq [qi, qj, qk]] && nearM (gp hq qi qj) qk 1e-9 && nearM (gp hq qj qi) (smul qk (-1)) 1e-9)
    , ("Cl(1,0): j^2 = +1, (1 + j)(1 - j) = 0 (zero divisors exist)", nearM (gp dd jj jj) one 1e-9 && isZeroM (gp dd (addM one jj) (subM one jj)) 1e-9)
    , ("Cl(2,0) and Cl(0,2) share e12^2 = -1 but differ in the sign of e1^2 (both have 2^n = 4 dimensions)"
      , nearM (gp g2 e12 e12) (scalar g2 (-1)) 1e-9 && aNb g2 == 4 && aNb hq == 4 && not (nearM (gp g2 (blade g2 1 1) (blade g2 1 1)) (gp hq (blade hq 1 1) (blade hq 1 1)) 1e-9)) ]

-- ---------- やってみよう ----------
-- 1. Q（複素数）から 1 を押して e1² = +1 にすると、何が変わる？（双曲数）
-- 2. W（四元数）の表で、i·j と j·i のマスを見比べよう
-- 3. n = 3 で全部 +1（G(3)）にして、2 乗が −1 になる基底を数えよう。それは何個？
-- 4. E（時空の代数）で γ0 = e1 だけ +1。e1e2 の 2 乗は？（11 のローレンツブーストで使う）
-- 5. Cl(1,1) と Cl(2,0) はどちらも 2×2 の実行列と同じになる。表の符号の数を比べてみよう

-- ==================================================
-- ここから下は「幾何代数の道具」（中身は読まなくても OK。消さないでね）
--   マルチベクトル = 2ⁿ 個の Double の配列（unboxed vector）。基底ブレードは「ビットマスク」で番号をつける
--   例（n = 3）：0 = 1、1 = e1、2 = e2、3 = e12、4 = e3、5 = e13、6 = e23、7 = e123（e31 = −e13）
--   幾何積：ブレード a と b を並べかえる回数（入れかえ 1 回で −1）× 同じ e_i が 2 つ並んだら e_i² を掛ける
--   計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
type MV = U.Vector Double

-- 代数：次元 n、基底の数 2ⁿ、各 e_i の 2 乗、次数、掛け算表（符号）、基底の名前
data Alg = Alg
  { aN      :: !Int
  , aNb     :: !Int
  , aMetric :: [Double]
  , aGrade  :: !(U.Vector Int)
  , aTbl    :: !(U.Vector Double)
  , aNames  :: [String] }

-- 基底 a のうしろに基底 b を並べ、標準の順（番号の小さい順）にするまでの入れかえ回数 → 奇数なら −1
reorderSign :: Int -> Int -> Double
reorderSign a0 b = go (a0 `shiftR` 1) 0
  where go 0 s = if odd s then -1 else 1
        go a s = go (a `shiftR` 1) (s + popCount (a .&. b))

-- 代数をつくる：metric = 各 e_i の 2 乗（+1 か −1）。[1,1,1] = G(3)、[-1,-1] = Cl(0,2)、[1,-1,-1,-1] = 時空の代数
makeAlgebra :: [Double] -> Alg
makeAlgebra metric = Alg n nb metric grade tbl names
  where
    n = length metric
    nb = 1 `shiftL` n
    grade = U.generate nb popCount
    tbl = U.generate (nb * nb) $ \k ->
      let (a, b) = k `divMod` nb
          c = a .&. b
      in foldl' (\s (i, m) -> if testBit c i then s * m else s) (reorderSign a b) (zip [0 ..] metric)   -- 同じ e_i どうし → e_i² = ±1
    names = [ if a == 0 then "1" else 'e' : concat [ show (i + 1) | i <- [0 .. n - 1], testBit a i ] | a <- [0 .. nb - 1] ]

-- 基底の名前を取りかえる
withNames :: [String] -> Alg -> Alg
withNames ns al = al { aNames = ns }

nameOf :: Alg -> Int -> String
nameOf al i = getAt "?" (aNames al) i

zeroM :: Alg -> MV
zeroM al = U.replicate (aNb al) 0

scalar :: Alg -> Double -> MV
scalar al s = zeroM al U.// [(0, s)]

-- 基底ブレード（係数 c）
blade :: Alg -> Int -> Double -> MV
blade al mask c = zeroM al U.// [(mask, c)]

-- ベクトル（足りない成分は 0）
vec :: Alg -> [Double] -> MV
vec al vs = zeroM al U.// [ (1 `shiftL` i, v) | (i, v) <- zip [0 .. aN al - 1] vs ]

vecOf :: Alg -> MV -> [Double]
vecOf al m = [ m U.! (1 `shiftL` i) | i <- [0 .. aN al - 1] ]

addM, subM :: MV -> MV -> MV
addM = U.zipWith (+)
subM = U.zipWith (-)

-- いくつも足す
addMs :: Alg -> [MV] -> MV
addMs al = foldl' addM (zeroM al)

smul :: MV -> Double -> MV
smul a s = U.map (* s) a

-- 積の共通部分：kind 0 = 幾何積、1 = 外積 ∧（共通の e_i がないときだけ）、2 = 左縮約 ⌋（a のブレードが b に含まれるときだけ）
prodK :: Alg -> Int -> MV -> MV -> MV
prodK al kind a b = U.accum (+) (zeroM al)
  [ (i `xor` j, U.unsafeIndex tbl (i * nb + j) * x * y)
  | i <- [0 .. nb - 1], let x = U.unsafeIndex a i, x /= 0
  , j <- [0 .. nb - 1], let y = U.unsafeIndex b j, y /= 0
  , kind /= 1 || i .&. j == 0
  , kind /= 2 || i .&. j == i ]
  where nb = aNb al
        tbl = aTbl al

gp, wedge, lc :: Alg -> MV -> MV -> MV
gp al = prodK al 0
wedge al = prodK al 1
lc al = prodK al 2

-- 3 つ以上の積（左から順に）
gps, wedges :: Alg -> [MV] -> MV
gps al ms = case ms of
  (m : rest) -> foldl' (gp al) m rest
  [] -> scalar al 1
wedges al ms = case ms of
  (m : rest) -> foldl' (wedge al) m rest
  [] -> scalar al 1

-- ベクトルどうしなら a·b
inner :: Alg -> MV -> MV -> MV
inner al a b = gradePart al (gp al a b) 0

-- スカラー部分
sp :: Alg -> MV -> MV -> Double
sp al a b = gp al a b U.! 0

gradePart :: Alg -> MV -> Int -> MV
gradePart al a k = U.imap (\i x -> if aGrade al U.! i == k then x else 0) a

gradeSize :: Alg -> MV -> Int -> Double
gradeSize al a k = sqrt (U.ifoldl' (\s i x -> if aGrade al U.! i == k then s + x * x else s) 0 a)

-- 逆順 ã（掛ける順を逆に）
rev :: Alg -> MV -> MV
rev al = U.imap (\i x -> let g = aGrade al U.! i in if odd ((g * (g - 1)) `div` 2) then -x else x)

-- 次数の反転（奇数次に −）
ginv :: Alg -> MV -> MV
ginv al = U.imap (\i x -> if odd (aGrade al U.! i) then -x else x)

normSq, norm :: Alg -> MV -> Double
normSq al a = gp al a (rev al a) U.! 0
norm al a = sqrt (abs (normSq al a))

unit :: Alg -> MV -> MV
unit al a = let n = norm al a in smul a (1 / (if n == 0 || isNaN n then 1 else n))

-- 逆元（ベクトル・ブレード・ベルソル用：a ã がスカラー）
inv :: Alg -> MV -> MV
inv al a = let r = rev al a in smul r (1 / (gp al a r U.! 0))

-- 擬スカラー（一番上のブレード）
pss :: Alg -> MV
pss al = blade al (aNb al - 1) 1

-- 双対 a* = a I⁻¹
dualOf, undual :: Alg -> MV -> MV
dualOf al a = gp al a (inv al (pss al))
undual al a = gp al a (pss al)

-- 交わり（向きと大きさは決めかた次第）
meet :: Alg -> MV -> MV -> MV
meet al a b = undual al (wedge al (dualOf al a) (dualOf al b))

-- R x R̃（回転）
sandwich :: Alg -> MV -> MV -> MV
sandwich al r x = gps al [r, x, rev al r]

-- −n x n⁻¹（法線 n の鏡で反射）
reflect :: Alg -> MV -> MV -> MV
reflect al nv x = smul (gps al [nv, x, inv al nv]) (-1)

isOdd :: Alg -> MV -> Bool
isOdd al v = o > e
  where (o, e) = U.ifoldl' (\(so, se) i x -> if odd (aGrade al U.! i) then (so + x * x, se) else (so, se + x * x)) (0, 0) v

-- ±V x V⁻¹（鏡 k 枚の積なら符号 (−1)^k）
applyVersor :: Alg -> MV -> MV -> MV
applyVersor al v x = smul (gps al [v, x, inv al v]) (if isOdd al v then -1 else 1)

-- 二重ベクトル B の指数関数 e^B：B² がスカラーなら閉じた式、そうでなければ級数（割って 2 乗をくり返す）
expB :: Alg -> MV -> Bool -> MV
expB al b forceSeries
  | not forceSeries && rest < 1e-12 =
      let s = U.head b2
          (c, k) | s < -1e-15 = let t = sqrt (-s) in (cos t, sin t / t)
                 | s > 1e-15 = let t = sqrt s in (cosh t, sinh t / t)
                 | otherwise = (1, 1)
          r = smul b k
      in r U.// [(0, U.head r + c)]
  | otherwise =
      let sz0 = U.foldl' (\m x -> max m (abs x)) 0 b
          halve (sz, q) = if sz > 0.25 && q < 40 then halve (sz / 2, q + 1) else q
          sq = halve (sz0, 0 :: Int)
          x = smul b (1 / (2 ^^ sq))
          series = snd (foldl' (\(term, sm) k -> let t' = smul (gp al term x) (1 / fromIntegral k) in (t', addM sm t')) (scalar al 1, scalar al 1) [1 .. 29 :: Int])
      in iterate (\m -> gp al m m) series !! sq
  where
    b2 = gp al b b
    rest = U.sum (U.map abs (U.drop 1 b2))

near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 2 つのマルチベクトルが同じか（tol の中）
nearM :: MV -> MV -> Double -> Bool
nearM a b tol = U.length a == U.length b && U.and (U.zipWith (\x y -> abs (x - y) <= tol) a b)

isZeroM :: MV -> Double -> Bool
isZeroM a tol = U.all (\x -> abs x <= tol) a

-- ランダムなマルチベクトル・ベクトル（成分は −1〜1）
randM, randVec :: Alg -> R MV
randM al = U.fromList <$> mapM (\_ -> (\r -> r * 2 - 1) <$> nextR) [1 .. aNb al]
randVec al = vec al <$> mapM (\_ -> (\r -> r * 2 - 1) <$> nextR) [1 .. aN al]

-- 文字にする：「1.20 + 0.50 e12」（記号は ASCII）
str :: Alg -> MV -> Int -> String
str al a d = if null s then "0" else s
  where
    s = foldl' piece "" (zip [0 ..] (U.toList a))
    piece acc (i, x)
      | abs x < 10 ^^ negate d / 2 = acc
      | otherwise =
          let v = fmt d (abs x)
              sg = if null acc then (if x < 0 then "-" else "") else (if x < 0 then " - " else " + ")
              body = if i > 0 then (if v == fmt d 1 then "" else v ++ " ") ++ nameOf al i else v
          in acc ++ sg ++ body

-- ---- 比べるための「ふつうの」計算（行列・外積・行列式）。ベクトルは [x, y, z] のリスト
dotv :: [Double] -> [Double] -> Double
dotv a b = foldl' (+) 0 (zipWith (*) a b)

crossv :: [Double] -> [Double] -> [Double]
crossv a b = [a1 * b2 - a2 * b1, a2 * b0 - a0 * b2, a0 * b1 - a1 * b0]
  where [a0, a1, a2] = take 3 (a ++ repeat 0)
        [b0, b1, b2] = take 3 (b ++ repeat 0)

lenv :: [Double] -> Double
lenv a = sqrt (dotv a a)

det3 :: [Double] -> [Double] -> [Double] -> Double
det3 a b c = dotv a (crossv b c)

matVec :: [[Double]] -> [Double] -> [Double]
matVec m v = map (\r -> foldl' (+) 0 (zipWith (*) r v)) m

matMul :: [[Double]] -> [[Double]] -> [[Double]]
matMul p q = [ [ foldl' (+) 0 (zipWith (*) r col) | col <- transpose q ] | r <- p ]

rotMat2 :: Double -> [[Double]]
rotMat2 t = [[cos t, -(sin t)], [sin t, cos t]]

-- 軸 ax（単位ベクトル）まわりに角 t 回す行列
rodrigues :: [Double] -> Double -> [[Double]]
rodrigues ax t =
  [ [c + x * x * k, x * y * k - z * s, x * z * k + y * s]
  , [y * x * k + z * s, c + y * y * k, y * z * k - x * s]
  , [z * x * k - y * s, z * y * k + x * s, c + z * z * k] ]
  where [x, y, z] = take 3 (ax ++ repeat 0)
        c = cos t
        s = sin t
        k = 1 - c

nearArr :: [Double] -> [Double] -> Double -> Bool
nearArr a b tol = length a == length b && and (zipWith (\x y -> abs (x - y) <= tol) a b)

-- ---- 種つきの乱数（自己テストで毎回同じ数を使う。p5.js 版の rnd と同じ mulberry32）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

nextRng :: Rng -> (Double, Rng)
nextRng (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に使う計算（状態を手で受けわたさなくてよいように）
newtype R a = R (Rng -> (a, Rng))

instance Functor R where fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where R g >>= k = R (\s -> let (a, s1) = g s; R h = k a in h s1)

nextR :: R Double
nextR = R nextRng

runRng :: R a -> Rng -> (a, Rng)
runRng (R g) = g

evalRng :: Word32 -> R a -> a
evalRng sd m = fst (runRng m (seedRng sd))

-- ---- 数の道具
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- JavaScript の % と同じ（符号は割られる数と同じ）
jsMod :: Double -> Double -> Double
jsMod a m = a - m * fromIntegral (truncate (a / m) :: Integer)

-- Math.round と同じ（.5 は大きい方へ）
jsRound :: Double -> Double
jsRound v = if isNaN v || isInfinite v then 0 else fromIntegral (floor (v + 0.5) :: Integer)

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . clampv (-1e6) 1e6 . fin 0

hypot :: Double -> Double -> Double
hypot a b = sqrt (a * a + b * b)

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 1.2e-5 のような書き方（toExponential(1)）
fmtE :: Double -> String
fmtE v = showEFloat (Just 1) (fin 0 v) ""

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点、y は下向き）で
mouseP :: Env -> (Double, Double)
mouseP env = let (mx, my) = envMouse env in (realToFrac (mx + envW env / 2), realToFrac (envH env / 2 - my))

-- gloss のイベントの位置 → p5.js の座標
evP :: Env -> (Float, Float) -> (Double, Double)
evP env (mx, my) = (realToFrac (mx + envW env / 2), realToFrac (envH env / 2 - my))

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC r g b a = makeColorI r g b (max 0 (min 255 a))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- [r, g, b] の色（透明度 a）
colA :: (Int, Int, Int) -> Int -> Color
colA (r, g, b) a = rgbaC r g b a

col3 :: (Int, Int, Int) -> Color
col3 (r, g, b) = rgbC r g b

-- 左下が (x, y) の文字
say :: String -> Double -> Double -> Float -> Color -> Picture
say s x y size c = Translate (f2 x) (f2 y) (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Double -> Double -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

textW :: String -> Float -> Double
textW s size = fromIntegral (length s) * realToFrac size * 0.72

pt2 :: (Double, Double) -> (Float, Float)
pt2 (x, y) = (f2 x, f2 y)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Double -> Double -> Double -> Double -> Picture
rectTL x y w h = Polygon [pt2 (x, y), pt2 (x + w, y), pt2 (x + w, y + h), pt2 (x, y + h)]

-- 線（太さ wt）
lineS :: Float -> (Double, Double) -> (Double, Double) -> Picture
lineS wt a b = if wt <= 1.2 then Line [pt2 a, pt2 b] else thickLine wt (pt2 a) (pt2 b)

-- 点線（dash：線の長さ、gap：すき間）
dashed :: Float -> Double -> Double -> (Double, Double) -> (Double, Double) -> Picture
dashed wt dash gap (x1, y1) (x2, y2) = Pictures
  [ lineS wt (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + dash), y1 + uy * min l (s + dash)) | s <- takeWhile (< l) [0, dash + gap ..] ]
  where l = min 5000 (fin 0 (hypot (x2 - x1) (y2 - y1)))
        ux = if l > 0 then (x2 - x1) / l else 0
        uy = if l > 0 then (y2 - y1) / l else 0

-- 点が 2 つ以上あるときだけ折れ線を描く（closed なら閉じる）
polyline :: Float -> Bool -> [(Double, Double)] -> Picture
polyline wt closed pts = case pts of
  (p0 : _ : _) -> let ps = map pt2 (if closed then pts ++ [p0] else pts) in if wt <= 1.2 then Line ps else thickPath wt ps
  _ -> Blank

-- 閉じた形を塗る（へこんだ形でも大丈夫なように、重心からの三角形で塗る）
fillPoly :: [(Double, Double)] -> Picture
fillPoly pts = case pts of
  (_ : _ : _ : _) -> Pictures [ Polygon [pt2 c, pt2 a, pt2 b] | (a, b) <- zip pts (drop 1 pts ++ take 1 pts) ]
  _ -> Blank
  where n = fromIntegral (length pts)
        c = (sum (map fst pts) / n, sum (map snd pts) / n)

-- 最初の点から扇のように塗る（beginShape の扇形・星形の形）
fanFill :: [(Double, Double)] -> Picture
fanFill pts = case pts of
  (p0 : rest@(_ : _ : _)) -> Pictures [ Polygon [pt2 p0, pt2 a, pt2 b] | (a, b) <- zip rest (drop 1 rest) ]
  _ -> Blank

-- 円（中心・半径）の枠と塗り
circleS :: Float -> (Double, Double) -> Double -> Picture
circleS wt (x, y) r = Translate (f2 x) (f2 y) (if wt <= 1.2 then Circle (f2 r) else ThickCircle (f2 r) wt)

discS :: (Double, Double) -> Double -> Picture
discS (x, y) r = Translate (f2 x) (f2 y) (circleSolid (f2 r))

-- 矢印（画面の座標）：線の太さ sw
arrowS :: Color -> Float -> (Double, Double) -> (Double, Double) -> Picture
arrowS c sw (x1, y1) (x2, y2) = Color c $ Pictures $
  lineS sw (x1, y1) (x2, y2) :
  [ Polygon [pt2 (x2, y2), pt2 (x2 - h * cos (a - 0.4), y2 - h * sin (a - 0.4)), pt2 (x2 - h * cos (a + 0.4), y2 - h * sin (a + 0.4))]
  | hypot (x2 - x1) (y2 - y1) >= 2 ]
  where a = atan2 (y2 - y1) (x2 - x1)
        h = 8 + realToFrac sw * 2

-- ---- 自己テストの道具
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
drawSelfTest env rs = say (selfTestLine rs) (realToFrac (envW env) - 200) (realToFrac (envH env) - 12) 14 (if all snd rs then rgbC 150 220 200 else rgbC 255 100 110)
