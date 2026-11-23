-- ============================================
-- 58-02 ブレードと次数 ─ 3 次元の幾何代数は 8 つの基底でできている
-- 1（スカラー・次数 0）、e1 e2 e3（ベクトル・次数 1）、e12 e23 e13（二重ベクトル = 面・次数 2）、e123（三重ベクトル = 体積・次数 3）
-- コンピュータの中では基底を「ビットマスク」で表す：e1 = 001、e2 = 010、e3 = 100、e13 = 101 …（どの e_i を含むか）
-- 操作：下の 8 本のスライダ マルチベクトル A の成分 / 1 2 3 v = e1, e2, e3 / A：A、W：v∧A、L：v⌋A、G：vA を見る / スペース ランダム / C 0 に
-- ============================================
module Main (main) where

import Art
import Data.Bits (popCount, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.Char (toUpper)
import Data.List (foldl', transpose)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

g3 :: Alg
g3 = makeAlgebra [1, 1, 1]

comps0 :: [Double]
comps0 = [0.5, 1, 0, 0, 0, 0, 0.8, 0]   -- ★ 最初の A の成分（ビットマスク順：1, e1, e2, e12, e3, e13, e23, e123）

data World = World
  { wComps :: [Double]
  , wV     :: !Int          -- v = e1, e2, e3
  , wView  :: !Char         -- 'A' 'W' 'L' 'G'
  , wDragS :: !Int
  , wTest  :: [(String, Bool)] }

-- 次数 0〜3 の色
gcol :: Int -> (Int, Int, Int)
gcol g = getAt (220, 220, 230) [(220, 220, 230), (90, 220, 240), (180, 130, 255), (255, 190, 110)] g

sil :: (Int, Int, Int)
sil = (215, 220, 230)

main :: IO ()
main = runSketch (sketch "58-02 Blades and grades" world0)
  { skBack  = rgbC 14 14 18
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World comps0 1 'W' (-1) []

-- 斜めから見た 3D → 画面（右手系：x 左下、y 右下、z 上）
pr :: Double -> Double -> Double -> [Double] -> (Double, Double)
pr cx cy s v = let (x, y, z) = t3 v in (cx + (y - x) * 0.87 * s, cy + (x + y) * 0.5 * s - z * s)

t3 :: [Double] -> (Double, Double, Double)
t3 v = case v ++ repeat 0 of
  (x : y : z : _) -> (x, y, z)
  _ -> (0, 0, 0)

bw :: Env -> Double
bw env = min 150 ((realToFrac (envW env) - 40) / 8)

draw :: Env -> World -> Picture
draw env w = view env $
  -- 上：8 つの基底の絵とビットマスク
  concat [ let cx = 20 + wd * fromIntegral m + wd / 2; g = aGrade g3 U.! m; col = gcol g
           in [ Color (rgbaC 255 255 255 8) (rectTL (cx - wd / 2 + 4) 56 (wd - 8) 150) ]
              ++ bladeIcon m cx 140 26 col
              ++ [ sayC (nameOf g3 m) cx 186 17 (col3 col)
                 , sayC (bits3 m ++ "  grade " ++ show g) cx 202 14 (grayC 150) ]
         | m <- [0 .. 7] ]
  -- 中：スライダ（A の成分）
  ++ concat [ let cx = 20 + wd * fromIntegral m + wd / 2; c = getAt 0 (wComps w) m; yy = sy + sh / 2 - c * (sh / 4)
              in [ Color (grayC 60) (Pictures [lineS 1 (cx, sy) (cx, sy + sh), lineS 1 (cx - 14, sy + sh / 2) (cx + 14, sy + sh / 2)])
                 , Color (col3 (gcol (aGrade g3 U.! m))) (discS (cx, yy) 9)
                 , sayC (fmt 2 c) cx (sy + sh + 18) 14 (col3 sil) ]
            | m <- [0 .. 7] ]
  -- 下左：次数ごとの大きさの棒（A と結果）
  ++ gradeBars 30 (h - 40) "A" a ++ gradeBars 360 (h - 40) rname res
  -- 下右：結果を絵で（ベクトル = 矢印、二重ベクトル = 面、三重ベクトル = 立方体）
  ++ drawMV (ww * 0.8) (h - 150) res
  -- 文字
  ++ [ say ("sliders: parts of A   1 2 3: v = e1 / e2 / e3 (now " ++ vname ++ ")   A W L G: show " ++ rname ++ "   Space: random   C: zero") 20 34 18 (col3 sil)
     , say ("A = " ++ str g3 a 2) 20 452 16 (col3 (gcol 1))
     , say (rname ++ " = " ++ str g3 res 2) 20 478 16 (col3 (gcol 3))
     , say expl 20 504 14 (grayC 150)
     , drawSelfTest env (wTest w) ]
  where
    ww = realToFrac (envW env) :: Double
    h = realToFrac (envH env) :: Double
    wd = bw env
    sy = 250
    sh = 150
    a = U.fromList (take 8 (wComps w ++ repeat 0))
    v = blade g3 (1 `shiftL` (wV w - 1)) 1
    res = case wView w of
      'A' -> a
      'W' -> wedge g3 v a
      'L' -> lc g3 v a
      _ -> gp g3 v a
    vname = 'e' : show (wV w)
    rname = case wView w of
      'A' -> "A"
      'W' -> vname ++ "^A"
      'L' -> vname ++ "_|A"
      _ -> vname ++ "A"
    expl = case wView w of
      'W' -> "^ raises the grade by 1 (blades that already contain the same e_i vanish: the bits overlap)"
      'L' -> "_| (left contraction) lowers the grade by 1 (only blades that contain " ++ vname ++ " remain)"
      'G' -> "geometric product vA = v_|A + v^A (the lowered part plus the raised part)"
      _ -> "A is a set of 8 numbers, coloured by grade"

bits3 :: Int -> String
bits3 m = [ if testBit m i then '1' else '0' | i <- [2, 1, 0] ]

-- 基底ブレードの小さな絵
bladeIcon :: Int -> Double -> Double -> Double -> (Int, Int, Int) -> [Picture]
bladeIcon m cx cy s col =
  [ Color (grayC 70) (Pictures [ lineS 1 o (p (map (* 1.3) e)) | e <- ax ]) ]
  ++ case idx of
       [] -> [Color (col3 col) (discS o 7)]
       [i] -> [arrowS (col3 col) 3 o (p (getAt [] ax i))]
       _ -> [ Color (colA col (if length idx == 2 then 110 else 60)) (fillPoly [o, p u, p (zipWith (+) u v), p v])
            | (i, j) <- faces, let u = getAt [] ax i, let v = getAt [] ax j ]
            ++ [ Color (col3 col) (lineS 1.5 (p [1, 1, 0]) (p [1, 1, 1])) | length idx == 3 ]
  where
    p = pr cx cy s
    ax = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
    idx = [ i | i <- [0 .. 2], testBit m i ]
    o = p [0, 0, 0]
    faces = case idx of
      [i, j] -> [(i, j)]
      _ -> [(0, 1), (1, 2), (0, 2)]

gradeBars :: Double -> Double -> String -> MV -> [Picture]
gradeBars x y nm m = say (nm ++ ": size of each grade") x (y - 118) 15 (col3 sil) : concat
  [ [ Color (grayC 40) (rectTL (x + fromIntegral g * 70) (y - 90) 50 90)
    , Color (col3 (gcol g)) (rectTL (x + fromIntegral g * 70) y 50 (-hh))
    , sayC ("grade " ++ show g) (x + fromIntegral g * 70 + 25) (y + 18) 14 (grayC 170) ]
  | g <- [0 .. 3], let hh = min 90 (gradeSize g3 m g * 50) ]

-- マルチベクトルの絵：ベクトル部分は矢印、二重ベクトル部分は法線（双対）に垂直な正方形、三重ベクトルは立方体
drawMV :: Double -> Double -> MV -> [Picture]
drawMV cx cy m =
  [ Color (grayC 55) (Pictures [ lineS 1 o (p e) | e <- [[1.6, 0, 0], [0, 1.6, 0], [0, 0, 1.6]] ]) ]
  ++ (if abs t > 1e-3
        then let c = abs t ** (1 / 3)
             in [ Color (colA (gcol 3) (if t > 0 then 50 else 25)) (fillPoly [o, p u, p (zipWith (+) u v), p v])
                | (u, v) <- [([c, 0, 0], [0, c, 0]), ([0, c, 0], [0, 0, c]), ([c, 0, 0], [0, 0, c])] ]
        else [])
  ++ (if bl > 1e-3
        then let nn = map (/ bl) nrm
                 hv = if abs (getAt 0 nn 0) < 0.9 then [1, 0, 0] else [0, 1, 0]
                 u0 = crossv nn hv
                 u1 = map (/ lenv u0) u0
                 w1 = crossv nn u1
                 r = sqrt bl
                 u = map (* r) u1
                 v = map (* r) w1
             in [Color (colA (gcol 2) 110) (fillPoly [o, p u, p (zipWith (+) u v), p v])]
        else [])
  ++ [ arrowS (col3 (gcol 1)) 3 o (p vv) | lenv vv > 1e-3 ]
  ++ [ Color (col3 (gcol 0)) (discS o ((8 + 10 * min 3 (abs (m U.! 0))) / 2)) | abs (m U.! 0) > 1e-3 ]
  where
    s = 60
    p = pr cx cy s
    o = p [0, 0, 0]
    t = m U.! 7
    nrm = [m U.! 6, -(m U.! 5), m U.! 3]       -- 二重ベクトル → 法線（e23 → x、e31 = −e13 → y、e12 → z）
    bl = lenv nrm
    vv = [m U.! 1, m U.! 2, m U.! 4]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = evP env pos
        hit = [ m | m <- [0 .. 7], let cx = 20 + bw env * fromIntegral m + bw env / 2, abs (mx - cx) < 22 && my > 230 && my < 420 ]
        w' = w { wDragS = if null hit then -1 else last hit }
    in pure (dragTo my w')
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDragS = -1 }
  EventMotion pos | wDragS w >= 0 -> pure (dragTo (snd (evP env pos)) w)
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> pure w { wV = fromEnum c - fromEnum '0' }
    | toUpper c `elem` "AWLG" -> pure w { wView = toUpper c }
    | c `elem` "cC" -> pure w { wComps = map (const 0) (wComps w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    cs <- mapM (\_ -> do r <- randomRIO (0, 1 :: Double); pure (jsRound ((r * 4 - 2) * 4) / 4)) (wComps w)
    pure w { wComps = cs }
  _ -> pure w
  where
    dragTo my w' = let k = wDragS w' in
      if k < 0 then w' else w' { wComps = [ if i == k then jsRound (clampv (-2) 2 ((250 + 75 - my) / 37.5) * 20) / 20 else c | (i, c) <- zip [0 ..] (wComps w') ] }

-- ---- 自己テスト（p5.js 版と同じ 8 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest = evalRng 2 $ do
  let g5 = makeAlgebra [1, 1, 1, 1, 1]
      cnt al k = U.length (U.filter (== k) (aGrade al))
      b k = blade g3 k 1
  rs <- mapM (\_ -> do
          v <- randVec g3
          am <- randM g3
          let parts = [ (g, gradePart g3 am g) | g <- [0 .. 3] ]
              okW = and [ let up = wedge g3 v ag in nearM up (gradePart g3 up (g + 1)) 1e-9 | (g, ag) <- parts ]
              okL = and [ let dn = lc g3 v ag in nearM dn (if g > 0 then gradePart g3 dn (g - 1) else zeroM g3) 1e-9 | (g, ag) <- parts ]
              okG = nearM (gp g3 v am) (addM (lc g3 v am) (wedge g3 v am)) 1e-9
          pure [okW, okL, okG]) [1 .. 30 :: Int]
  let col k = all (\r -> getAt False r k) rs
  pure
    [ ("grade of a blade = number of bits (e13 = 101 -> 2, e123 = 111 -> 3)", aGrade g3 U.! 5 == 2 && aGrade g3 U.! 7 == 3 && nameOf g3 5 == "e13")
    , ("basis count per grade is binomial: n = 3 -> 1, 3, 3, 1; n = 5 -> 1, 5, 10, 10, 5, 1 (total 2^n)"
      , and (zipWith (\k c -> cnt g3 k == c) [0 ..] [1, 3, 3, 1]) && and (zipWith (\k c -> cnt g5 k == c) [0 ..] [1, 5, 10, 10, 5, 1]) && aNb g5 == 32)
    , ("reordering signs: e2e1 = -e12, e3e1 = -e13, e3e2e1 = -e123", reorderSign 2 1 == -1 && reorderSign 4 1 == -1 && nearM (gps g3 [b 4, b 2, b 1]) (blade g3 7 (-1)) 1e-9)
    , ("^ raises the grade by 1 (v^<A>_k has grade k + 1 only)", col 0)
    , ("_| lowers the grade by 1 (v_|<A>_k has grade k - 1 only, 0 on scalars)", col 1)
    , ("for a vector v: vA = v_|A + v^A", col 2)
    , ("e1^e2^e3 = e123, e1^e1 = 0, e1_|e12 = e2", nearM (wedges g3 [b 1, b 2, b 4]) (b 7) 1e-9 && isZeroM (wedge g3 (b 1) (b 1)) 1e-9 && nearM (lc g3 (b 1) (b 3)) (b 2) 1e-9)
    , ("the reverse A~ has signs +, +, -, - for grades 0-3"
      , nearM (rev g3 (addMs g3 [scalar g3 1, b 1, b 3, b 7])) (addMs g3 [scalar g3 1, b 1, blade g3 3 (-1), blade g3 7 (-1)]) 1e-9) ]

-- ---------- やってみよう ----------
-- 1. A = e12 だけにして、1 2 3 で v を変え W（∧）を見よう。e3∧e12 は？ e1∧e12 は？（ビットが重なると 0）
-- 2. L（⌋）で e1⌋e12 と e3⌋e12 を比べよう。どちらが 0？
-- 3. G（幾何積）の結果が W と L の和になっていることを、次数の棒で確かめよう
-- 4. ビットマスクで e2 と e13 の積の番号は？（2 XOR 5 = 7 → e123、符号は並べかえの回数）
-- 5. 4 次元（n = 4）なら基底は何個？ 次数ごとの数は？（makeAlgebra [1, 1, 1, 1] で確かめられる）

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
