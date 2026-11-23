-- ============================================
-- 58-07 3D のローター ─ R = e^{−Bθ/2}、v ↦ R v R̃ で 3 次元の回転
-- 05 の 2D とまったく同じ式。I を「回転の面」B（単位の二重ベクトル）に取りかえるだけ
-- ローターの成分 (1, e23, e13, e12) は四元数と同じ掛け算をする（i = −e23, j = e13, k = −e12）→ シリーズ 57 へ
-- 操作：ドラッグ 視点（ホイール 拡大）/ 1 2 3 4 回転の面 / ← → 角度 θ / スペース 自動 / C 2 つの回転の順番（R2R1 と R1R2）
-- ============================================
module Main (main) where

import Art
import Data.Bits (popCount, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (foldl', transpose)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

g3 :: Alg
g3 = makeAlgebra [1, 1, 1]

scl :: Double
scl = 140                        -- ★ 長さ 1 のピクセル数

-- ★ 回す物体（紙飛行機）：頂点と辺
pts :: [[Double]]
pts = [[1.4, 0, 0], [-0.8, 0.9, 0], [-0.8, -0.9, 0], [-0.6, 0, 0], [-0.8, 0, -0.45], [-0.8, 0, 0.35]]

edges :: [(Int, Int)]
edges = [(0, 1), (1, 3), (3, 2), (2, 0), (0, 3), (0, 4), (4, 3), (3, 5), (5, 0)]

-- 回転の面 1〜3：基底ブレードの番号と名前
planes :: [(Int, String)]
planes = [(3, "e12 (around the z axis)"), (6, "e23 (around the x axis)"), (5, "e13 (around the y axis, direction -y)")]

data World = World
  { wPlane :: !Int
  , wTheta :: !Double
  , wAuto  :: !Bool
  , wOrder :: !Int               -- 0 = なし、1 = R2R1、2 = R1R2
  , wCam   :: !Cam
  , wTest  :: [(String, Bool)] }

vio, cya, gold, sil :: (Int, Int, Int)
vio = (180, 130, 255)
cya = (90, 220, 240)
gold = (255, 210, 120)
sil = (215, 220, 230)

main :: IO ()
main = runSketch (sketch "58-07 Rotors in 3D" world0)
  { skBack  = rgbC 14 14 18
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (if wAuto w then w { wTheta = jsMod (wTheta w + 0.012) (4 * pi) } else w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 4 0.6 True 0 (camFromGL 560 (-400) 760) []

toW :: [Double] -> (Double, Double, Double)
toW v = sc3d scl (t3 v)

-- 回転の面 B（単位の二重ベクトル）
planeB :: Int -> MV
planeB k
  | k <= 3 = blade g3 (fst (getAt (3, "") planes (k - 1))) 1
  | otherwise = unit g3 (wedge g3 (vec g3 [1, 0.3, 0.2]) (vec g3 [0.1, 0.7, 0.9]))   -- ★ 4：自由な面 a∧b

rotorOf :: MV -> Double -> MV
rotorOf b th = expB g3 (smul b (-th / 2)) False

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawItems items ]
  ++ zipWith (\k (s, c) -> hudLine k s c) [0 ..] hud
  ++ [ say s (16 + 56 * fromIntegral j) (26 + 24 * fromIntegral (tableRow + i)) 15 (col3 gold) | (i, row) <- zip [0 :: Int ..] qTable, (j, s) <- zip [0 :: Int ..] row ]
  ++ [ hudLine (tableRow + 4) "quaternions and 3D rotations in more detail: series 57" (136, 136, 136)
     , drawSelfTest env (wTest w) ]
  where
    cam = wCam w
    th = wTheta w
    b = planeB (wPlane w)
    axis = vecOf g3 (dualOf g3 b)            -- B = n I → 軸 n = B I⁻¹（双対）
    r | wOrder w == 0 = rotorOf b th
      | otherwise = let r1 = rotorOf (blade g3 3 1) th; r2 = rotorOf (blade g3 6 1) (pi / 2)   -- C：2 つの回転を合成
                    in if wOrder w == 1 then gp g3 r2 r1 else gp g3 r1 r2
    rot = map (\p -> vecOf g3 (sandwich g3 r (vec g3 p))) pts
    pAt xs i = toW (getAt [0, 0, 0] xs i)
    -- 回転の面（円板）
    bv = [b U.! 6, -(b U.! 5), b U.! 3]
    u0 = crossv bv (if abs (getAt 0 bv 0) < 0.9 then [1, 0, 0] else [0, 1, 0])
    u = map (/ lenv u0) u0
    wv = crossv bv u
    disc = [ toW (zipWith (\x wk -> 1.8 * (cos t * x + sin t * wk)) u wv) | i <- [0 .. 47 :: Int], let t = fromIntegral i / 48 * 2 * pi ]
    mrot = rodrigues axis th
    items =
      [ line3 env cam (grayC 60) 1 (toW (map negate e)) (toW e) | k <- [0 .. 2 :: Int], let e = [ if j == k then 2.2 else 0 | j <- [0 .. 2] ] ]
      ++ (if wOrder w == 0
            then [ poly3 env cam (colA vio 40) [(0, 0, 0), p, q] | (p, q) <- zip disc (drop 1 disc ++ take 1 disc) ]
                 ++ [ line3 env cam (col3 gold) 2 (toW (map (* (-2)) axis)) (toW (map (* 2) axis)) ]
            else [])
      -- 元の物体（うすく）と、ローターで回した物体
      ++ [ line3 env cam (grayC 90) 1 (pAt pts i) (pAt pts j) | (i, j) <- edges ]
      ++ [ line3 env cam (col3 cya) 3 (pAt rot i) (pAt rot j) | (i, j) <- edges ]
      ++ [ poly3 env cam (col3 vio) [pAt rot i, pAt rot j, pAt rot 3] | (i, j) <- [(0, 1), (0, 2)] ]
      -- 回転行列（ロドリゲスの式）で回した頂点 = 白い点（ローターの結果と重なる）
      ++ (if wOrder w == 0 then [ dot3D env cam white 4.5 (toW (matVec mrot p)) | p <- pts ] else [])
    q = [r U.! 0, -(r U.! 6), r U.! 5, -(r U.! 3)]        -- 四元数 w + xi + yj + zk
    sg x = (if x < 0 then " - " else " + ") ++ fmt 3 (abs x)
    orderName = getAt "" ["off", "R2R1 (z axis, then x axis)", "R1R2 (the other order)"] (wOrder w)
    hud =
      [ ("drag: view   1 2 3 4: rotation plane   Left/Right: theta   Space: auto " ++ (if wAuto w then "ON" else "off") ++ "   C: compose " ++ orderName, sil)
      , if wOrder w /= 0
          then ("R1 = theta in the e12 plane, R2 = 90 deg in the e23 plane. R2R1 /= R1R2 -> order matters in 3D", cya)
          else ("rotation plane B = " ++ (if wPlane w <= 3 then snd (getAt (3, "") planes (wPlane w - 1)) else str g3 b 3 ++ " (free plane)") ++ "   theta = " ++ fmt 0 (th * 180 / pi) ++ " deg", sil)
      , ("R = " ++ (if wOrder w /= 0 then "" else "e^(-B theta/2) = ") ++ str g3 r 3 ++ "   R R~ = " ++ str g3 (gp g3 r (rev g3 r)) 3, vio)
      , ("as a quaternion q = " ++ fmt 3 (getAt 0 q 0) ++ sg (getAt 0 q 1) ++ "i" ++ sg (getAt 0 q 2) ++ "j" ++ sg (getAt 0 q 3) ++ "k", sil)
      , ("white dots = vertices turned by the rotation matrix (Rodrigues) -> exactly on the rotor result (cyan)", sil)
      , ("table of i = -e23, j = e13, k = -e12 (computed with the geometric product):", gold) ]
    tableRow = length hud

-- 四元数の掛け算表を幾何積で作る（見出しの行 + 3 行、1 行は 5 つのます）
qTable :: [[String]]
qTable = ["", "1", "i", "j", "k"] : [ getAt "" nm a : [ name (gp g3 (getAt one u a) (getAt one u b)) | b <- [0 .. 3] ] | a <- [1 .. 3] ]
  where
    one = scalar g3 1
    u = [one, blade g3 6 (-1), blade g3 5 1, blade g3 3 (-1)]
    nm = ["1", "i", "j", "k"]
    name m = case [ (if s < 0 then "-" else "+") ++ getAt "" nm k | s <- [1, -1], k <- [0 .. 3], nearM m (smul (getAt one u k) s) 1e-9 ] of
      (x : _) -> x
      [] -> "?"

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> w { wPlane = fromEnum c - fromEnum '0', wOrder = 0 }
    | c `elem` "cC" -> w { wOrder = (wOrder w + 1) `mod` 3 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wTheta = wTheta w + 0.1 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wTheta = wTheta w - 0.1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  _ -> w { wCam = camEvent env ev (wCam w) }

-- ---- 自己テスト（p5.js 版と同じ 7 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest = evalRng 7 $ do
  rs <- mapM (\_ -> do
          n <- unit g3 <$> randVec g3
          th <- (\x -> x * 12 - 6) <$> nextR
          v <- randVec g3
          let nv = vecOf g3 n
              r = rotorOf (gp g3 n (pss g3)) th
              ok1 = nearArr (vecOf g3 (sandwich g3 r v)) (matVec (rodrigues nv th) (vecOf g3 v)) 1e-12
              ok2 = nearM (gp g3 r (rev g3 r)) (scalar g3 1) 1e-12
          m <- unit g3 <$> randVec g3
          th2 <- (* 6) <$> nextR
          let r2 = rotorOf (gp g3 m (pss g3)) th2
              rm = matMul (rodrigues (vecOf g3 m) th2) (rodrigues nv th)
              ok3 = nearArr (vecOf g3 (sandwich g3 (gp g3 r2 r) v)) (matVec rm (vecOf g3 v)) 1e-12
          a <- unit g3 <$> randVec g3
          b <- unit g3 <$> randVec g3
          let rab = unit g3 (addM (scalar g3 1) (gp g3 b a))
              ok4 = nearM (sandwich g3 rab a) b 1e-10
          pure [ok1, ok2, ok3, ok4]) [1 .. 30 :: Int]
  let col k = all (\x -> getAt False x k) rs
      qi = blade g3 6 (-1)
      qj = blade g3 5 1
      qk = blade g3 3 (-1)
      m1 = scalar g3 (-1)
      rz = rotorOf (blade g3 3 1) 1
      rx = rotorOf (blade g3 6 1) 1
  pure
    [ ("R v R~ = Rodrigues rotation matrix x v (30 cases, B = n I)", col 0)
    , ("R R~ = 1", col 1)
    , ("product of rotors R2R1 = product of matrices M2M1", col 2)
    , ("R = (1 + ba)/|1 + ba| turns a onto b (a rotor from 2 vectors)", col 3)
    , ("i^2 = j^2 = k^2 = ijk = -1 (i = -e23, j = e13, k = -e12)", all (\x -> nearM x m1 1e-9) [gp g3 qi qi, gp g3 qj qj, gp g3 qk qk, gps g3 [qi, qj, qk]])
    , ("ij = k, jk = i, ki = j, ji = -k (the even part = quaternions)", nearM (gp g3 qi qj) qk 1e-9 && nearM (gp g3 qj qk) qi 1e-9 && nearM (gp g3 qk qi) qj 1e-9 && nearM (gp g3 qj qi) (smul qk (-1)) 1e-9)
    , ("rotations in the e12 and e23 planes depend on the order (R1R2 /= R2R1), R(360 deg) = -1"
      , not (nearM (gp g3 rz rx) (gp g3 rx rz) 1e-3) && nearM (rotorOf (blade g3 3 1) (2 * pi)) m1 1e-12) ]

-- ---------- やってみよう ----------
-- 1. 1 2 3 で面を変え、黄色の軸と紫の円板（回転の面）の関係を見よう
-- 2. θ を 360° 回すと物体は元どおり。でも R はいくつ？（画面の R を見る）720° では？
-- 3. C で順番を変えて、R2R1 と R1R2 で飛行機の向きが違うことを確かめよう
-- 4. 画面の掛け算表を、i = e23, j = e13, k = e12（マイナスなし）で作るとどこが変わる？ planeB と qTable を書きかえて試そう
-- 5. 4 の自由な面の a、b を変えてみよう。軸を一度も使わずに回転が決まっている

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
