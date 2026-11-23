-- ============================================
-- 58-00 内積と外積のその先 ─ ベクトルの「掛け算」を 3 つ並べる
-- 内積 a·b（スカラー：どれだけ同じ向きか）、外積 a×b（3 次元でだけ決まる法線ベクトル）、
-- ウェッジ積 a∧b（向き付きの平行四辺形 = 二重ベクトル）。a∧b = −b∧a、a∧a = 0、|a∧b| = |a||b| sin θ
-- 操作：ドラッグ a・b の先を動かす / S a と b を入れかえる（向きが逆になる）/ P 射影の表示 / R 元にもどす
-- ============================================
module Main (main) where

import Art
import Data.Bits (popCount, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (foldl', intercalate, transpose)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

type P2 = (Double, Double)

-- 3 次元の幾何代数 G(3)（平面のベクトルは z = 0 として入れる）
g3 :: Alg
g3 = makeAlgebra [1, 1, 1]

va0, vb0 :: P2
va0 = (3, 1)                     -- ★ 最初のベクトル（単位 = マス目 1 つ）
vb0 = (1.2, 2.5)

unitPx :: Double
unitPx = 70                      -- ★ 長さ 1 のピクセル数

data World = World
  { wA, wB     :: !P2
  , wDrag      :: !Int
  , wSwapped   :: !Bool
  , wProj      :: !Bool
  , wTest      :: [(String, Bool)] }

vio, cya, sil, yel, grn :: (Int, Int, Int)
vio = (180, 130, 255)
cya = (90, 220, 240)
sil = (215, 220, 230)
yel = (255, 210, 120)
grn = (150, 200, 160)

main :: IO ()
main = runSketch (sketch "58-00 Beyond the dot and cross products" world0)
  { skBack  = rgbC 14 14 18
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World va0 vb0 (-1) False True []

ox, oy :: Env -> Double
ox env = realToFrac (envW env) * 0.36
oy env = realToFrac (envH env) * 0.6

toPx :: Env -> P2 -> P2
toPx env (x, y) = (ox env + x * unitPx, oy env - y * unitPx)

draw :: Env -> World -> Picture
draw env w = view env $
  drawGrid env
  -- a∧b：平行四辺形（反時計回り = +、紫 / 時計回り = −、水色）
  ++ [ Color (colA col 60) (fillPoly [o, pa, pab, pb])
     , Color (colA col 160) (dashed 1 5 5 pa pab), Color (colA col 160) (dashed 1 5 5 pb pab)
     , circulation ((fst o + fst pab) / 2, (snd o + snd pab) / 2) (atan2 (-py) px) th col ]
  -- a·b：b を a の向きに射影した長さ × |a|
  ++ (if wProj w && la > 1e-6
        then let t = ab / (la * la); pr = toPx env (px * t, py * t)
             in [ Color (colA yel 180) (dashed 1 3 4 pb pr), Color (col3 yel) (lineS 5 o pr) ]
        else [])
  ++ [ arrowS (col3 vio) 3 o pa, arrowS (col3 cya) 3 o pb
     , say (if wSwapped w then "b" else "a") (fst pa + 10) (snd pa - 8) 22 (col3 vio)
     , say (if wSwapped w then "a" else "b") (fst pb + 10) (snd pb - 8) 22 (col3 cya) ]
  ++ inset3d env (px, py) (qx, qy) cr w12
  -- 文字
  ++ [ say ("drag: vector tips   S: swap the order (now " ++ first ++ "^" ++ second ++ ")   P: projection " ++ (if wProj w then "ON" else "off") ++ "   R: reset") 20 34 18 (col3 sil) ]
  ++ [ say s 20 (76 + 28 * fromIntegral k) 16 (col3 c) | (k, (s, c)) <- zip [0 :: Int ..] rows ]
  ++ [ say "the cross product x has a single normal only in 3D, but ^ is 'the plane itself', so the same formula works in 2D and 4D" 20 (h - 40) 14 (grayC 150)
     , drawSelfTest env (wTest w) ]
  where
    h = realToFrac (envH env) :: Double
    (p, q) = if wSwapped w then (wB w, wA w) else (wA w, wB w)
    (px, py) = p
    (qx, qy) = q
    a = vec g3 [px, py, 0]
    b = vec g3 [qx, qy, 0]
    ab = inner g3 a b U.! 0
    wd = wedge g3 a b
    w12 = wd U.! 3
    cr = crossv [px, py, 0] [qx, qy, 0]
    la = hypot px py
    lb = hypot qx qy
    th = atan2 (px * qy - py * qx) (px * qx + py * qy)
    o = toPx env (0, 0)
    pa = toPx env p
    pb = toPx env q
    pab = toPx env (px + qx, py + qy)
    col = if w12 >= 0 then vio else cya
    first = if wSwapped w then "b" else "a"
    second = if wSwapped w then "a" else "b"
    (vax, vay) = wA w
    (vbx, vby) = wB w
    rows =
      [ ("a = (" ++ fmt 2 vax ++ ", " ++ fmt 2 vay ++ ")   b = (" ++ fmt 2 vbx ++ ", " ++ fmt 2 vby ++ ")   theta = " ++ fmt 1 (th * 180 / pi) ++ " deg (1st to 2nd)", sil)
      , ("dot " ++ first ++ "." ++ second ++ " = " ++ fmt 3 ab ++ " = |a||b| cos(theta) = " ++ fmt 3 (la * lb * cos th) ++ "  (scalar, grade 0)", yel)
      , ("   yellow line = projection x |a|", yel)
      , ("wedge " ++ first ++ "^" ++ second ++ " = " ++ str g3 wd 3 ++ "  (bivector, grade 2 = oriented area)", col)
      , ("area |" ++ first ++ "^" ++ second ++ "| = " ++ fmt 3 (abs w12) ++ " = |a||b| |sin(theta)| = " ++ fmt 3 (la * lb * abs (sin th)), col)
      , (second ++ "^" ++ first ++ " = " ++ str g3 (wedge g3 b a) 3 ++ " (opposite sign)   a^a = " ++ str g3 (wedge g3 a a) 2, sil)
      , ("cross " ++ first ++ " x " ++ second ++ " = (" ++ intercalate ", " (map (fmt 3) cr) ++ ")  <- the 3D normal", grn)
      , ("   same number as a^b (top right picture)", grn) ]

-- 1 つ目から 2 つ目へ回る向きの小さな矢印
circulation :: P2 -> Double -> Double -> (Int, Int, Int) -> Picture
circulation (cx, cy) a0 th col = case reverse pts of
  (e : d : _) -> Pictures [Color (col3 col) (polyline 2 False pts), arrowS (col3 col) 2 d e]
  _ -> Blank
  where r = 34
        n = 24 :: Int
        pts = [ (cx + r * cos t, cy + r * sin t) | i <- [0 .. n], let t = a0 - (th * fromIntegral i) / fromIntegral n ]

-- 右上の小窓：3 次元で見た a、b、a∧b（xy 平面の面）と a×b（法線）
inset3d :: Env -> P2 -> P2 -> [Double] -> Double -> [Picture]
inset3d env (px, py) (qx, qy) cr w12 =
  [ Color (rgbaC 255 255 255 8) (rectTL (cx - 200) (cy - 190) 400 330)
  , Color (grayC 70) (Pictures (concat [ [lineS 1 (pr k (-3) 0) (pr k 3 0), lineS 1 (pr (-3) k 0) (pr 3 k 0)] | k <- [-3 .. 3] ]))
  , Color (colA col 70) (fillPoly [o, pa, pab, pb])
  , arrowS (col3 vio) 2 o pa, arrowS (col3 cya) 2 o pb
  , arrowS (col3 grn) 3 o (pr 0 0 (clampv (-4) 4 (getAt 0 cr 2 * sc * sc)))
  , sayC "in 3D: a^b is a plane, a x b is its normal" cx (cy + 128) 14 (grayC 170) ]
  where
    cx = realToFrac (envW env) - 230
    cy = 230
    s = 34
    pr x y z = (cx + (y - x) * 0.87 * s, cy + (x + y) * 0.5 * s - z * s)
    sc = 0.6
    o = pr 0 0 0
    pa = pr (px * sc) (py * sc) 0
    pb = pr (qx * sc) (qy * sc) 0
    pab = pr ((px + qx) * sc) ((py + qy) * sc) 0
    col = if w12 >= 0 then vio else cya

drawGrid :: Env -> [Picture]
drawGrid env =
  [ Color (grayC 32) (Pictures ([ lineS 1 (x, 0) (x, h) | x <- takeWhile (< ww) [jsMod (ox env) unitPx, jsMod (ox env) unitPx + unitPx ..] ]
                              ++ [ lineS 1 (0, y) (ww, y) | y <- takeWhile (< h) [jsMod (oy env) unitPx, jsMod (oy env) unitPx + unitPx ..] ]))
  , Color (grayC 70) (Pictures [lineS 1 (0, oy env) (ww, oy env), lineS 1 (ox env, 0) (ox env, h)]) ]
  where ww = realToFrac (envW env)
        h = realToFrac (envH env)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = evP env pos
        pick (best, k) (i, v) = let (sx, sy) = toPx env v; d = hypot (mx - sx) (my - sy) in if d < best then (d, i) else (best, k)
    in w { wDrag = snd (foldl' pick (30, -1) (zip [0 ..] [wA w, wB w])) }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion pos | wDrag w >= 0 ->
    let (mx, my) = evP env pos
        v = (clampv (-8) 8 (jsRound (((mx - ox env) / unitPx) * 10) / 10), clampv (-6) 6 (jsRound (((oy env - my) / unitPx) * 10) / 10))
    in if wDrag w == 0 then w { wA = v } else w { wB = v }
  EventKey (Char c) Down _ _
    | c `elem` "sS" -> w { wSwapped = not (wSwapped w) }
    | c `elem` "pP" -> w { wProj = not (wProj w) }
    | c `elem` "rR" -> w { wA = va0, wB = vb0, wSwapped = False }
  _ -> w

-- ---- 自己テスト：ランダムなベクトルで性質を確かめる（p5.js 版と同じ 7 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest = evalRng 58 $ do
  rs <- mapM (\_ -> do
          a <- randVec g3; b <- randVec g3; c <- randVec g3
          let av = vecOf g3 a
              bv = vecOf g3 b
              la = lenv av
              lb = lenv bv
              cosv = dotv av bv / (la * lb)
              sn = sqrt (max 0 (1 - cosv * cosv))
              w = wedge g3 a b
              cr = crossv av bv
          pure
            [ nearM (wedge g3 a b) (smul (wedge g3 b a) (-1)) 1e-9
            , isZeroM (wedge g3 a a) 1e-9
            , near (gradeSize g3 (wedge g3 a b) 2) (la * lb * sn) 1e-9
            , nearArr [w U.! 6, -(w U.! 5), w U.! 3] cr 1e-9
            , nearM (inner g3 a b) (smul (addM (gp g3 a b) (gp g3 b a)) 0.5) 1e-9 && nearM w (smul (subM (gp g3 a b) (gp g3 b a)) 0.5) 1e-9
            , near (dotv av bv * dotv av bv + gradeSize g3 w 2 * gradeSize g3 w 2) (la * la * lb * lb) 1e-9
            , nearM (wedge g3 (addM a c) b) (addM (wedge g3 a b) (wedge g3 c b)) 1e-9 ]) [1 .. 50 :: Int]
  let col k = all (\r -> getAt False r k) rs
  pure
    [ ("a^b = -b^a (50 random pairs)", col 0)
    , ("a^a = 0", col 1)
    , ("|a^b| = |a||b| sin(theta)", col 2)
    , ("the parts (e23, -e13, e12) of a^b = the parts of a x b", col 3)
    , ("a.b = (ab + ba)/2, a^b = (ab - ba)/2", col 4)
    , ("(a.b)^2 + |a^b|^2 = |a|^2 |b|^2 (Lagrange's identity)", col 5)
    , ("^ is linear: (a + c)^b = a^b + c^b", col 6) ]

-- ---------- やってみよう ----------
-- 1. b を a と同じ向きにすると、a·b と a∧b はそれぞれどうなる？ 直角にすると？
-- 2. S で順番を入れかえて、平行四辺形の色（向き）と a∧b の符号が変わることを確かめよう
-- 3. 面積 |a∧b| が最大になるのは θ が何度のとき？ |a||b| と比べよう
-- 4. 右上の図の緑の法線 a×b の長さは、平行四辺形の面積と同じ。4 次元だと法線はどうなる？（06 へ）
-- 5. unitPx を変えて、画面を広く使ってみよう

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
