-- ============================================
-- 58-13 幾何代数で物理 ─ 角運動量は「面」L = r∧p、回転する物体の向きはローター
-- 3 次元の教科書では角運動量は軸ベクトル r × p。でも本当は「r と p が掃く面」= 二重ベクトル r∧p（r × p = −I(r∧p)）
-- 太陽のまわりの惑星（リープフロッグ法）で L が一定 = 面積速度一定（ケプラーの第 2 法則）。惑星の自転はローター R ← e^{−Iωdt/2} R
-- 操作：↑↓ 最初の速さ / ← → 自転の速さ ω / A 同じ時間に掃く面積 / スペース 一時停止 / R やり直す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (popCount, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (foldl', transpose)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

type P2 = (Double, Double)

g2 :: Alg
g2 = makeAlgebra [1, 1]

ps :: MV
ps = blade g2 3 1                -- I = e12

gm, dt :: Double
gm = 1                           -- ★ 重力の強さ
dt = 0.002                       -- ★ 時間の刻み

sub :: Int
sub = 10                         -- ★ 1 フレームの刻みの数

data World = World
  { wV0     :: !Double           -- ★ 最初の速さ（1 なら円軌道）
  , wOmega  :: !Double           -- ★ 自転の角速度
  , wR      :: !P2
  , wV      :: !P2
  , wBody   :: !MV               -- 惑星の向き（ローター）
  , wT      :: !Double
  , wHist   :: [(Double, Double)]   -- (L, E) の記録（新しい順、400 個まで）
  , wPaused :: !Bool
  , wArea   :: !Bool
  , wTest   :: [(String, Bool)] }

vio, cya, sil, gold :: (Int, Int, Int)
vio = (180, 130, 255)
cya = (90, 220, 240)
sil = (215, 220, 230)
gold = (255, 210, 120)

main :: IO ()
main = runSketch (sketch "58-13 Physics with geometric algebra" world0)
  { skBack  = rgbC 14 14 18
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = restart (World 0.8 2.5 (1, 0) (0, 0.8) (scalar g2 1) 0 [] False True [])

restart :: World -> World
restart w = w { wR = (1, 0), wV = (0, wV0 w), wBody = scalar g2 1, wT = 0, wHist = [] }

-- リープフロッグ法（速度ベルレ）：中心力なら r∧p がぴったり保存される
stepOrbit :: P2 -> P2 -> Double -> (P2, P2)
stepOrbit (rx, ry) (vx, vy) h = (rn, (vhx + 0.5 * h * anx, vhy + 0.5 * h * any'))
  where
    acc (px, py) = let d = hypot px py in ((-gm * px) / d ** 3, (-gm * py) / d ** 3)
    (ax, ay) = acc (rx, ry)
    vhx = vx + 0.5 * h * ax
    vhy = vy + 0.5 * h * ay
    rn = (rx + h * vhx, ry + h * vhy)
    (anx, any') = acc rn

-- L = r∧p（質量 1）
angMom :: P2 -> P2 -> MV
angMom (a, b) (c, d) = wedge g2 (vec g2 [a, b]) (vec g2 [c, d])

energy :: P2 -> P2 -> Double
energy (rx, ry) (vx, vy) = 0.5 * (vx ^ (2 :: Int) + vy ^ (2 :: Int)) - gm / hypot rx ry

step :: Env -> World -> World
step env w0 = w1 { wHist = hist' }
  where
    w1 | wPaused w0 = w0
       | otherwise = foldl' one w0 [1 .. sub]
    one w _ = let (r, v) = stepOrbit (wR w) (wV w) dt
              in w { wR = r, wV = v, wT = wT w + dt
                   , wBody = gp g2 (expB g2 (smul ps ((-(wOmega w) * dt) / 2)) False) (wBody w) }   -- 自転：ローターを少しずつ掛ける
    hist' | envFrame env `mod` 2 == 0 = take 400 ((angMom (wR w1) (wV w1) U.! 3, energy (wR w1) (wV w1)) : wHist w1)
          | otherwise = wHist w1

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (grayC 60) (polyline 1 False orbit) ]
  ++ [ Color (colA gold 40) (fanFill f) | wArea w, f <- fans ]
  ++ [ Color (colA vio 60) (fillPoly [o, pr, pv, ph])                -- r∧(v/2) の平行四辺形
     , Color (rgbC 255 200 90) (discS o 13)
     , Color (col3 vio) (lineS 1.5 o pr)
     , arrowS (col3 cya) 2.5 pr pv
     , Color white (lineS 3 (fst pr - axx * rl * u, snd pr + axy * rl * u) (fst pr + axx * rl * u, snd pr - axy * rl * u))
     , Color (col3 cya) (discS pr 7) ]
  ++ plotHist (gx, gy, gw, gh) True fst "e12 part of L = r^v (constant!)" vio
  ++ plotHist (gx, gy + gh + 70, gw, gh) False snd "energy E = 1/2 v^2 - GM/r" gold
  ++ [ say ("Up/Down: start speed " ++ fmt 2 (wV0 w) ++ "   Left/Right: spin omega = " ++ fmt 1 (wOmega w) ++ "   A: areas " ++ (if wArea w then "ON" else "off") ++ "   Space: pause   R: restart") 20 34 17 (col3 sil) ]
  ++ [ say s 20 (64 + 25 * fromIntegral k) 15 (col3 c) | (k, (s, c)) <- zip [0 :: Int ..] rows ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    ww = realToFrac (envW env) :: Double
    hh = realToFrac (envH env) :: Double
    u = min ww hh * 0.27
    toPx (x, y) = (ww * 0.36 + x * u, hh * 0.5 - y * u)
    (rx, ry) = wR w
    (vx, vy) = wV w
    -- 軌道を先まで計算して描く（同じ方法で 1 周ぶん）
    orbit = go (0 :: Int) (wR w) (wV w) []
      where go !k r v acc
              | k >= 4000 = reverse acc
              | otherwise = let (r', v') = stepOrbit r v 0.004 in go (k + 1) r' v' (if k `mod` 8 == 0 then toPx r' : acc else acc)
    -- 同じ時間に掃く扇形（面積 = |L|/2 × 時間）
    fans = goF (0 :: Int) (wR w) (wV w)
      where goF s r v
              | s >= 6 = []
              | otherwise =
                  let path = take 41 (iterate (\(a, b) -> stepOrbit a b 0.006) (r, v))
                      (r2, v2) = foldl' (\(a, b) _ -> stepOrbit a b 0.006) (getAt (r, v) path 40) [1 .. 100 :: Int]
                  in (toPx (0, 0) : map (toPx . fst) path) : goF (s + 1) r2 v2
    o = toPx (0, 0)
    pr = toPx (rx, ry)
    pv = toPx (rx + vx * 0.5, ry + vy * 0.5)
    ph = toPx (vx * 0.5, vy * 0.5)
    l = angMom (wR w) (wV w)
    -- 自転する惑星：棒の向き = ローター body で e1 を回したもの
    (axx, axy) = case vecOf g2 (sandwich g2 (wBody w) (blade g2 1 1)) of
      (a : b : _) -> (a, b)
      _ -> (1, 0)
    rl = 0.12
    gx = ww * 0.66
    gy = 150
    gw = ww * 0.3
    gh = 130
    cr = crossv [rx, ry, 0] [vx, vy, 0]
    rows =
      [ ("L = r^v = " ++ str g2 l 6 ++ "   (a bivector: the area of the parallelogram of r and v)", vio)
      , ("as a 3D axis vector: r x v = (0, 0, " ++ fmt 6 (getAt 0 cr 2) ++ ") = -I(r^v)  -> the same number", sil)
      , ("orientation of the planet: rotor R = " ++ str g2 (wBody w) 3 ++ "   R R~ = " ++ fmt 12 (gp g2 (wBody w) (rev g2 (wBody w)) U.! 0), (255, 255, 255))
      , ("yellow fans = area swept in equal times (each is |L|/2 x time)", gold) ]
    plotHist (x, y, gwd, ght) isL sel title c =
      [ say title x (y - 8) 14 (col3 sil)
      , Color (grayC 60) (polyline 1 True [(x, y), (x + gwd, y), (x + gwd, y + ght), (x, y + ght)]) ]
      ++ case wHist w of
           (newest : _ : _) ->
             let vals = reverse (map sel (wHist w))
                 mid = sel newest
                 dev = foldl' (\m v -> max m (abs (v - mid))) 0 vals
                 span0 = max 1e-6 dev * 1.3
                 sp' = if isL then max span0 (abs mid * 0.05) else span0
             in [ Color (col3 c) (polyline 2 False [ (x + (fromIntegral i / 399) * gwd, y + ght / 2 - ((v - mid) / sp') * (ght / 2)) | (i, v) <- zip [0 :: Int ..] vals ])
                , say ("now " ++ fmt 8 mid ++ "   spread " ++ fmtE dev) x (y + ght + 18) 14 (grayC 170) ]
           _ -> []

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> restart w { wV0 = min 1.35 (wV0 w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _ -> restart w { wV0 = max 0.4 (wV0 w - 0.05) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wOmega = min 12 (wOmega w + 0.5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wOmega = max (-12) (wOmega w - 0.5) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wArea = not (wArea w) }
    | c `elem` "rR" -> restart w
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest = evalRng 13 $ do
  let r0 = (1, 0)
      v0 = (0, 0.8)
      l0 = angMom r0 v0 U.! 3
      e0 = energy r0 v0
      -- 20000 歩：L とエネルギーのずれの最大、1000 歩ごとの掃いた面積、y = 0 を下から上へ横切った時刻
      go !k r v !maxL !maxE areas prevY crossT
        | k > 20000 = (maxL, maxE, reverse areas, reverse crossT)
        | otherwise =
            let (r', v') = stepOrbit r v dt
                mL = max maxL (abs (angMom r' v' U.! 3 - l0))
                mE = max maxE (abs (energy r' v' - e0))
                areas1 = if k `mod` 1000 == 1 then 0 : areas else areas
                da = 0.5 * (wedge g2 (vec g2 [fst r, snd r]) (vec g2 [fst r', snd r']) U.! 3)
                areas2 = case areas1 of
                  (a : rest) -> (a + da) : rest
                  [] -> []
                ct = if prevY < 0 && snd r' >= 0 then fromIntegral k * dt : crossT else crossT
            in go (k + 1 :: Int) r' v' mL mE areas2 (snd r') ct
      (maxL', maxE', areas', crossT') = go 1 r0 v0 0 0 [] (snd r0) []
      aSemi = -gm / (2 * e0)
      tk = 2 * pi * aSemi ** 1.5 / sqrt gm
      okA = case areas' of
        (a0 : _) -> all (\a -> near a a0 1e-12) (take (length areas' - 1) areas')
        [] -> False
      okT = case crossT' of
        (t1 : t2 : _) -> near ((t2 - t1) / tk) 1 1e-3
        _ -> False
  okX <- and <$> mapM (\_ -> do
            a1 <- nextR; a2 <- nextR; b1 <- nextR; b2 <- nextR
            let a = (a1 - 0.5, a2 - 0.5)
                b = (b1 - 0.5, b2 - 0.5)
            pure (near (angMom a b U.! 3) (getAt 0 (crossv [fst a, snd a, 0] [fst b, snd b, 0]) 2) 1e-15)) [1 .. 20 :: Int]
  let wv = 2.5
      n = round' ((2 * pi) / (wv * dt))
      dtw = (2 * pi) / (wv * fromIntegral n)
      rr = foldl' (\r _ -> gp g2 (expB g2 (smul ps ((-wv * dtw) / 2)) False) r) (scalar g2 1) [1 .. n]
      round' x = floor (x + 0.5) :: Int
  pure
    [ ("leapfrog, 20000 steps: drift of L = r^v < 1e-12 (angular momentum is conserved)", maxL' < 1e-12)
    , ("energy drift < 1e-4 (symplectic method)", maxE' < 1e-4)
    , ("equal areas in equal times (Kepler's 2nd law, 1e-12)", okA)
    , ("period = 2 pi a^(3/2) / sqrt(GM) (Kepler's 3rd law, 0.1%)", okT)
    , ("the e12 part of the 2D r^v = the z part of the 3D r x v", okX)
    , ("multiplying the rotor " ++ show n ++ " times gives one turn: R = -1, R R~ = 1 (1e-10)", nearM rr (scalar g2 (-1)) 1e-10 && near (gp g2 rr (rev g2 rr) U.! 0) 1 1e-10) ]

-- ---------- やってみよう ----------
-- 1. ↑↓ で最初の速さを変え、楕円の形が変わっても L のグラフが平らなままか確かめよう
-- 2. 黄色の扇形の面積は同じ。太陽に近いときと遠いときで形はどう違う？
-- 3. 速さ 1 で円軌道。√2 ≈ 1.41 をこえると？（↑ の上限を変えてみよう）
-- 4. stepOrbit をオイラー法（r += v dt; v += a dt）に変えると、L とエネルギーはどうなる？
-- 5. ← → で自転を速くしても公転は変わらない。向きをローターで持つと、何回まわっても R R̃ = 1 のまま

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
