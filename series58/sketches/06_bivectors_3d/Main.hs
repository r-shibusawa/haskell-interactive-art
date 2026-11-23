-- ============================================
-- 58-06 3D の二重ベクトル ─ 回転の「面」B = a∧b と、その双対の「軸」
-- 3 次元では面 B に垂直な向きが 1 つだけあるので、双対 B* = B I⁻¹ が軸ベクトルになる：a × b = −I(a∧b)
-- でも 4 次元では面に垂直な向きが 2 次元分もある → 軸は決まらない。面 B ならどの次元でも同じ式で使える
-- 操作：ドラッグ 視点を回す（ホイール 拡大）/ 1 2 3 例 / ← → b を回す / ↑↓ a を傾ける / スペース 自動 / 4 4 次元の例
-- ============================================
module Main (main) where

import Art
import Data.Bits (popCount, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (foldl', intercalate, transpose)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

g3, g4 :: Alg
g3 = makeAlgebra [1, 1, 1]
g4 = makeAlgebra [1, 1, 1, 1]

scl :: Double
scl = 150                         -- ★ 長さ 1 のピクセル数

data World = World
  { wAngB   :: !Double
  , wTiltA  :: !Double
  , wPreset :: !Int
  , wAuto   :: !Bool
  , wShow4  :: !Bool
  , wCam    :: !Cam
  , wTest   :: [(String, Bool)] }

vio, cya, gold, grn :: (Int, Int, Int)
vio = (180, 130, 255)
cya = (90, 220, 240)
gold = (255, 210, 120)
grn = (150, 220, 170)

main :: IO ()
main = runSketch (sketch "58-06 Bivectors in 3D" world0)
  { skBack  = rgbC 14 14 18
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (if wAuto w then w { wAngB = wAngB w + 0.008 } else w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1.1 0.25 1 True False (camFromGL 520 (-380) 720) []

-- 数学の座標 → ピクセル
toW :: [Double] -> (Double, Double, Double)
toW v = sc3d scl (t3 v)

vecs :: World -> ([Double], [Double])
vecs w = (map (* 1.6) a, map (* 1.6) b)
  where
    ta = wTiltA w
    ab = wAngB w
    a0 = [cos ta, 0, sin ta]
    (a, b) = case wPreset w of
      2 -> ([1, 0, 0], [cos ab, sin ab, 0])
      3 -> (a0, [cos ta + 0.15 * cos ab, 0.15 * sin ab, sin ta])
      _ -> (a0, [cos ab * 0.9, sin ab * 0.9, 0.5])

draw :: Env -> World -> Picture
draw env w = Pictures
  [ view env $
      [ drawItems items ]
      ++ zipWith (\k (s, c) -> hudLine k s c) [0 ..] hud
      ++ [ drawSelfTest env (wTest w) ] ]
  where
    cam = wCam w
    (av, bv) = vecs w
    a = vec g3 av
    b = vec g3 bv
    bb = wedge g3 a b
    ax = dualOf g3 bb
    axv = vecOf g3 ax
    cr = crossv av bv
    o = (0, 0, 0)
    pab = zipWith (+) av bv
    -- 面の中の正規直交基底 u, w（a から b へ回る向き）
    u = map (/ lenv av) av
    w0 = zipWith (\x uk -> x - dotv bv u * uk) bv u
    wl = let l = lenv w0 in if l == 0 then 1 else l
    wv = map (/ wl) w0
    c = map (/ 2) pab
    ring = [ toW (zipWith3 (\ck uk wk -> ck + 0.3 * (cos t * uk + sin t * wk)) c u wv) | i <- [0 .. 30 :: Int], let t = fromIntegral i / 30 * 1.6 * pi ]
    items =
      [ line3 env cam (grayC 70) 1 (toW (map negate e)) (toW e) | k <- [0 .. 2 :: Int], let e = [ if j == k then 2.4 else 0 | j <- [0 .. 2] ] ]
      ++ [ poly3 env cam (colA vio 90) q | q <- quadGrid 3 o (toW av) (toW bv) ]
      ++ [ line3 env cam (col3 vio) 1.5 p q | (p, q) <- zip ring (drop 1 ring) ]
      ++ arrow3 env cam (col3 vio) o (toW av)
      ++ arrow3 env cam (col3 cya) o (toW bv)
      ++ arrow3 env cam (col3 gold) o (toW axv)        -- 双対 = 軸
    b4 = addM (blade g4 3 1) (blade g4 12 1)
    showV v = "(" ++ intercalate ", " (map (fmt 2) v) ++ ")"
    hud =
      [ ("drag: view   1 2 3: examples (now " ++ show (wPreset w) ++ ")   Left/Right: turn b   Up/Down: tilt a   Space: auto   4: 4D example " ++ (if wShow4 w then "ON" else "off"), (215, 220, 230))
      , ("a = " ++ showV av ++ "    b = " ++ showV bv, vio)
      , ("B = a^b = " ++ str g3 bb 3 ++ "   (area |B| = " ++ fmt 3 (gradeSize g3 bb 2) ++ ")", vio)
      , ("dual B* = B I^-1 = " ++ str g3 ax 3 ++ "  <- the axis perpendicular to the plane (yellow arrow)", gold)
      , ("cross product a x b = (" ++ intercalate ", " (map (fmt 3) cr) ++ ")  -> the same as -I(a^b)", (215, 220, 230))
      , ("the parts e23, e13, e12 of B say how much the plane leans towards the yz, xz and xy planes", (215, 220, 230)) ]
      ++ (if wShow4 w
            then [ ("4D: B = e12 + e34 has B^B = " ++ str g4 (wedge g4 b4 b4) 2 ++ " /= 0 -> not a single plane (it turns in 2 planes at once).", grn)
                 , ("  its dual B I^-1 = " ++ str g4 (dualOf g4 b4) 2 ++ " is a bivector -> no 'axis vector' exists", grn) ]
            else [ ("press 4 for the 4D example", (136, 136, 136)) ])

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char ch) Down _ _
    | ch >= '1' && ch <= '3' -> w { wPreset = fromEnum ch - fromEnum '0' }
    | ch == '4' -> w { wShow4 = not (wShow4 w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wAngB = wAngB w + 0.1 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wAngB = wAngB w - 0.1 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wTiltA = min 1.4 (wTiltA w + 0.1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wTiltA = max (-1.4) (wTiltA w - 0.1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  _ -> w { wCam = camEvent env ev (wCam w) }

-- ---- 自己テスト（p5.js 版と同じ 7 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest = evalRng 6 $ do
  rs <- mapM (\_ -> do
          a <- randVec g3; b <- randVec g3
          let bb = wedge g3 a b
              ax = vecOf g3 (dualOf g3 bb)
              ok1 = nearArr (vecOf g3 (smul (gp g3 (pss g3) bb) (-1))) (crossv (vecOf g3 a) (vecOf g3 b)) 1e-12
              ok2 = near (dotv ax (vecOf g3 a)) 0 1e-12 && near (dotv ax (vecOf g3 b)) 0 1e-12 && near (lenv ax) (gradeSize g3 bb 2) 1e-12
          br <- (\m -> gradePart g3 m 2) <$> randM g3
          let ok3 = isZeroM (wedge g3 br br) 1e-12
              ok4 = nearM (undual g3 (dualOf g3 bb)) bb 1e-12
          pure [ok1, ok2, ok3, ok4]) [1 .. 40 :: Int]
  let col k = all (\r -> getAt False r k) rs
      b4 = addM (blade g4 3 1) (blade g4 12 1)
      s = 0.7
      t = 1.3
      bst = addM (blade g4 3 s) (blade g4 12 t)
      ex = expB g4 bst False
      prd = gp g4 (expB g4 (blade g4 3 s) False) (expB g4 (blade g4 12 t) False)
      r4 = expB g4 (smul bst (-0.5)) False
      y = vecOf g4 (sandwich g4 r4 (vec g4 [1, 0, 1, 0]))
  pure
    [ ("a x b = -I(a^b) (40 pairs)", col 0)
    , ("the dual axis B I^-1 is perpendicular to a and b, its length = area |a^b|", col 1)
    , ("a 3D bivector is always a single plane (B^B = 0)", col 2)
    , ("undoing the dual gives B back ((B I^-1) I = B)", col 3)
    , ("4D: e12 + e34 has B^B = 2 e1234 /= 0 (not a single plane)", nearM (wedge g4 b4 b4) (blade g4 15 2) 1e-9)
    , ("4D: e^(s e12 + t e34) (by the series) = e^(s e12) e^(t e34) (product of 2 plane rotations)", nearM ex prd 1e-10)
    , ("that rotor turns e1 by s in the e12 plane and e3 by t in the e34 plane (a 4D rotation with 2 angles)", nearArr y [cos s, sin s, cos t, sin t] 1e-10) ]

-- ---------- やってみよう ----------
-- 1. 2 を押すと a と b は xy 平面の中。B のどの成分だけが残る？ 軸はどちら向き？
-- 2. 3 を押すと a と b がほぼ平行。面積と軸の長さはどうなる？
-- 3. ← → で b を a のまわりに回し、軸の向きと B の符号が一緒に変わることを見よう
-- 4. 4 を押して 4 次元の例を読もう。「軸のまわりの回転」より「面の中の回転」が一般的な理由は？
-- 5. scl を変えて大きさを調整しよう

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

-- ---- 3D の道具（p5.js の WEBGL と orbitControl の代わり）
--   数学の座標（右手系、z が上）をカメラの向きに回して透視投影し、奥のものから順に描く（画家のアルゴリズム）
--   カメラ：方位角 az（xy 平面の中の向き）、仰角 el、原点からの距離 dist（ピクセル）
data Cam = Cam { cAz, cEl, cDist :: !Double, cLast :: !(Maybe (Double, Double)) }

-- p5.js の camera(x, y, z, 0, 0, 0, 0, 1, 0) と同じ位置から見るカメラ（WEBGL の座標で渡す）
camFromGL :: Double -> Double -> Double -> Cam
camFromGL gx gy gz = Cam (atan2 my mx) (atan2 mz (hypot mx my)) (sqrt (mx * mx + my * my + mz * mz)) Nothing
  where (mx, my, mz) = (gx, -gz, -gy)        -- WEBGL (x, y 下, z 手前) → 数学 (x, y, z 上)

-- ドラッグで回す・ホイールで近づく（orbitControl の代わり）
camEvent :: Env -> Event -> Cam -> Cam
camEvent env ev c = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> c { cLast = Just (evP env pos) }
  EventKey (MouseButton LeftButton) Up _ _ -> c { cLast = Nothing }
  EventMotion pos | Just (lx, ly) <- cLast c ->
    let (mx, my) = evP env pos
    in c { cAz = cAz c - (mx - lx) * 0.01, cEl = clampv (-1.5) 1.5 (cEl c + (my - ly) * 0.01), cLast = Just (mx, my) }
  EventKey (MouseButton WheelUp) Down _ _ -> c { cDist = max 300 (cDist c * 0.9) }
  EventKey (MouseButton WheelDown) Down _ _ -> c { cDist = min 4000 (cDist c / 0.9) }
  _ -> c

-- 3D の点（ピクセル単位の数学の座標）→ 画面の点（p5.js の座標）と、手前への深さ
camProj :: Env -> Cam -> (Double, Double, Double) -> ((Double, Double), Double)
camProj env c (x, y, z) = ((realToFrac (envW env) / 2 + k * sx, realToFrac (envH env) / 2 - k * sy), sz)
  where
    ca = cos (cAz c); sa = sin (cAz c); ce = cos (cEl c); se = sin (cEl c)
    sx = -sa * x + ca * y                              -- 画面の右
    sy = -se * ca * x - se * sa * y + ce * z           -- 画面の上
    sz = ce * ca * x + ce * sa * y + se * z            -- カメラの方（手前）
    focal = realToFrac (envH env) / 2 / tan (pi / 6)   -- p5.js の標準の画角（60°）
    k = focal / max 50 (cDist c - sz)

-- 描くもの：深さと絵（あとで奥から順に並べる）
type Item = (Double, Picture)

-- 3D の線（a → b）
line3 :: Env -> Cam -> Color -> Float -> (Double, Double, Double) -> (Double, Double, Double) -> Item
line3 env c col wt a b = let (pa, da) = camProj env c a; (pb, db) = camProj env c b in ((da + db) / 2, Color col (lineS wt pa pb))

-- 3D の多角形（塗り）
poly3 :: Env -> Cam -> Color -> [(Double, Double, Double)] -> Item
poly3 env c col ps = (sum (map snd prs) / fromIntegral (max 1 (length prs)), Color col (fillPoly (map fst prs)))
  where prs = map (camProj env c) ps

-- 3D の点（画面での半径 r）
dot3D :: Env -> Cam -> Color -> Double -> (Double, Double, Double) -> Item
dot3D env c col r p = let (q, d) = camProj env c p in (d + 0.5, Color col (discS q r))

-- 四角形の板を n × n の小さな四角に分ける（奥から順に塗るときの重なりをへらす）
quadGrid :: Int -> (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double) -> [[(Double, Double, Double)]]
quadGrid n o u v = [ [pt i j, pt (i + 1) j, pt (i + 1) (j + 1), pt i (j + 1)] | i <- [0 .. n - 1], j <- [0 .. n - 1] ]
  where pt i j = let s = fromIntegral i / fromIntegral n; t = fromIntegral j / fromIntegral n in add3d o (add3d (sc3d s u) (sc3d t v))

-- 奥から順に描く
drawItems :: [Item] -> Picture
drawItems its = Pictures (map snd (depthSort (realToFrac . fst) its))

add3d, sub3d :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
add3d (a, b, c) (d, e, f) = (a + d, b + e, c + f)
sub3d (a, b, c) (d, e, f) = (a - d, b - e, c - f)

sc3d :: Double -> (Double, Double, Double) -> (Double, Double, Double)
sc3d k (a, b, c) = (k * a, k * b, k * c)

-- リスト [x, y, z] → 組
t3 :: [Double] -> (Double, Double, Double)
t3 v = case v ++ repeat 0 of
  (x : y : z : _) -> (x, y, z)
  _ -> (0, 0, 0)

-- 3D の矢印（数学の座標で from → to、ピクセル単位）。矢じりは 4 本の短い線
arrow3 :: Env -> Cam -> Color -> (Double, Double, Double) -> (Double, Double, Double) -> [Item]
arrow3 env c col p q
  | l < 1 = [line3 env c col 3 p q]
  | otherwise = line3 env c col 3 p q : [ line3 env c col 3 q (add3d q (add3d (sc3d (-hl) d) (sc3d (hl * 0.4) s))) | s <- [u, w, sc3d (-1) u, sc3d (-1) w] ]
  where
    dv = sub3d q p
    l = let (a, b, e) = dv in sqrt (a * a + b * b + e * e)
    d = sc3d (1 / l) dv
    (_, _, dz) = d
    h = if abs dz < 0.9 then [0, 0, 1] else [1, 0, 0]
    dl = let (a, b, e) = d in [a, b, e]
    u0 = crossv dl h
    u = t3 (map (/ lenv u0) u0)
    w = t3 (crossv dl (let (a, b, e) = u in [a, b, e]))
    hl = min 18 (l * 0.3)

-- HUD の 1 行（左上から、WEBGL の画面の文字の代わり）
hudLine :: Int -> String -> (Int, Int, Int) -> Picture
hudLine k s c = say s 16 (26 + 24 * fromIntegral k) 15 (col3 c)
