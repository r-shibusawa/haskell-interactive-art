-- ============================================
-- 58-11 時空の代数 ─ Cl(1,1)：γ0² = +1（時間）、γ1² = −1（空間）
-- 時空のベクトル v = tγ0 + xγ1 の「長さ」v² = t² − x²。光（t = ±x）は v² = 0
-- B = γ0γ1 は B² = +1 なので e^{−φB/2} = cosh(φ/2) − B sinh(φ/2)。R v R̃ が回転ではなく「双曲回転」= ローレンツブースト
-- 操作：← → ブースト φ1 / ↑↓ φ2 / C 2 つのブーストの合成 / マウス 出来事（点）の座標を両方の観測者で / R 元にもどす
-- 画面の基底の名前は ASCII：γ0 → g0、γ1 → g1
-- ============================================
module Main (main) where

import Art
import Data.Bits (popCount, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (foldl', transpose)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

-- 時空の代数（1 + 1 次元）
sta :: Alg
sta = withNames ["1", "g0", "g1", "g0g1"] (makeAlgebra [1, -1])

gam0, gam1, bb :: MV
gam0 = blade sta 1 1
gam1 = blade sta 2 1
bb = blade sta 3 1

unitPx :: Double
unitPx = 95

data World = World
  { wPhi1, wPhi2 :: !Double      -- ★ ラピディティ（速さ v = tanh φ、光速 c = 1）
  , wCompose     :: !Bool
  , wTest        :: [(String, Bool)] }

vio, cya, sil, gold :: (Int, Int, Int)
vio = (180, 130, 255)
cya = (90, 220, 240)
sil = (215, 220, 230)
gold = (255, 210, 120)

main :: IO ()
main = runSketch (sketch "58-11 Spacetime algebra" world0)
  { skBack  = rgbC 14 14 18
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 0.55 0.4 False []

boost :: Double -> MV
boost phi = expB sta (smul bb (-phi / 2)) False

toPx :: Env -> Double -> Double -> (Double, Double)
toPx env t x = (realToFrac (envW env) * 0.4 + x * unitPx, realToFrac (envH env) * 0.56 - t * unitPx)

-- マルチベクトル → (t, x)
tx :: MV -> (Double, Double)
tx v = (v U.! 1, v U.! 2)

ev :: Double -> Double -> MV
ev t x = addM (smul gam0 t) (smul gam1 x)

draw :: Env -> World -> Picture
draw env w = view env $
  -- 双曲線 t² − x² = ±1, ±4（ブーストしても動かない）
  [ Color (rgbC 80 80 100) (polyline 1 False [ if sg > 0 then tp (side * s2 * cosh u) (s2 * sinh u) else tp (s2 * sinh u) (side * s2 * cosh u) | u <- us ])
  | s2 <- [1, 2], sg <- [1, -1 :: Int], side <- [1, -1] ]
  -- 静止している観測者の格子（うすい）
  ++ [ Color (grayC 45) (Pictures (concat [ [lineS 1 (tp (-ext) k) (tp ext k), lineS 1 (tp k (-ext)) (tp k ext)] | k <- [-3 .. 3] ])) ]
  -- 光円錐 t = ±x
  ++ [ Color (col3 gold) (Pictures [lineS 2 (tp (-ext) (-ext)) (tp ext ext), lineS 2 (tp (-ext) ext) (tp ext (-ext))]) ]
  -- ブーストした観測者の格子：R γ0 R̃（時間の軸）と R γ1 R̃（空間の軸）
  ++ concat [ [ Color (colA vio (if k == 0 then 255 else 70)) (lineS (if k == 0 then 3 else 1) (tp (k * e1t - ext * e0t) (k * e1x - ext * e0x)) (tp (k * e1t + ext * e0t) (k * e1x + ext * e0x)))
              , Color (colA cya (if k == 0 then 255 else 70)) (lineS (if k == 0 then 3 else 1) (tp (k * e0t - ext * e1t) (k * e0x - ext * e1x)) (tp (k * e0t + ext * e1t) (k * e0x + ext * e1x))) ]
            | k <- [-3 .. 3] ]
  ++ [ arrowS (col3 vio) 3 o p0, arrowS (col3 cya) 3 o p1
     , say "R g0 R~ (time of the moving observer)" (fst p0 + 8) (snd p0) 16 (col3 vio)
     , say "R g1 R~ (their 'same time')" (fst p1 + 8) (snd p1 + 18) 16 (col3 cya) ]
  ++ [ Color white (discS (mx, my) 5) | inside ]
  -- 文字
  ++ [ say ("Left/Right: phi1 = " ++ fmt 2 (wPhi1 w) ++ "   Up/Down: phi2 = " ++ fmt 2 (wPhi2 w) ++ "   C: compose " ++ (if wCompose w then "ON" else "off") ++ "   mouse: an event   R: reset") 20 34 18 (col3 sil) ]
  ++ [ say s 20 (64 + 25 * fromIntegral k) 16 (col3 c) | (k, (s, c)) <- zip [0 :: Int ..] rows ]
  ++ concat [ [say i1 20 (h - 62) 15 white, say i2 20 (h - 40) 15 white] | Just (i1, i2) <- [info] ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    tp = toPx env
    h = realToFrac (envH env) :: Double
    r = if wCompose w then gp sta (boost (wPhi2 w)) (boost (wPhi1 w)) else boost (wPhi1 w)
    phi = if wCompose w then wPhi1 w + wPhi2 w else wPhi1 w
    ext = 3.6
    us = take 88 (iterate (+ 0.05) (-2.2))   -- p5.js の for (u = −2.2; u <= 2.2; u += 0.05) と同じ 88 点
    (e0t, e0x) = tx (sandwich sta r gam0)
    (e1t, e1x) = tx (sandwich sta r gam1)
    o = tp 0 0
    p0 = tp e0t e0x
    p1 = tp e1t e1x
    -- マウスの出来事：静止系の (t, x) と、動く観測者の (t', x')（R̃ X R で座標を読む）
    (mx, my) = mouseP env
    mt = (realToFrac (envH env) * 0.56 - my) / unitPx
    mxx = (mx - realToFrac (envW env) * 0.4) / unitPx
    inside = abs mt < ext && abs mxx < ext
    info
      | inside =
          let x = ev mt mxx
              xp = sandwich sta (rev sta r) x
              s2 = gp sta x x U.! 0
          in Just ( "event: rest frame (t, x) = (" ++ fmt 2 mt ++ ", " ++ fmt 2 mxx ++ ")   moving observer (t', x') = (" ++ fmt 2 (xp U.! 1) ++ ", " ++ fmt 2 (xp U.! 2) ++ ")"
                  , "X^2 = t^2 - x^2 = " ++ fmt 3 s2 ++ " = t'^2 - x'^2 = " ++ fmt 3 ((xp U.! 1) ^ (2 :: Int) - (xp U.! 2) ^ (2 :: Int))
                    ++ (if abs s2 < 0.05 then "  (on the light cone)" else if s2 > 0 then "  (timelike: cause and effect can connect)" else "  (spacelike)") )
      | otherwise = Nothing
    v1 = tanh (wPhi1 w)
    v2 = tanh (wPhi2 w)
    rows =
      [ ("B = g0g1, B^2 = " ++ str sta (gp sta bb bb) 2 ++ " (+1, unlike I^2 = -1 for rotations)", vio)
      , ("R = e^(-phi B/2) = " ++ str sta r 3, vio)
      , ("speed of the moving observer v = tanh(phi) = " ++ fmt 4 (tanh phi) ++ " c   (R g0 R~ = " ++ str sta (sandwich sta r gam0) 3 ++ ")", vio) ]
      ++ (if wCompose w
            then [ ("composed R2R1 = e^(-(phi1+phi2)B/2): rapidities add " ++ fmt 2 (wPhi1 w) ++ " + " ++ fmt 2 (wPhi2 w) ++ " = " ++ fmt 2 phi, cya)
                 , ("speed (v1 + v2)/(1 + v1v2) = (" ++ fmt 3 v1 ++ " + " ++ fmt 3 v2 ++ ")/(1 + " ++ fmt 3 (v1 * v2) ++ ") = " ++ fmt 4 ((v1 + v2) / (1 + v1 * v2)) ++ "  (not a sum, never above c)", cya) ]
            else [])
      ++ [ ("yellow = light cone (the same for every observer), grey = curves of constant t^2 - x^2 (boosts keep them)", gold) ]

event :: Env -> Event -> World -> IO World
event _ ev' w = pure $ case ev' of
  EventKey k Down _ _ -> let w' = key k in w' { wPhi1 = clampv (-2.5) 2.5 (wPhi1 w'), wPhi2 = clampv (-2.5) 2.5 (wPhi2 w') }
  _ -> w
  where
    key k = case k of
      SpecialKey KeyRight -> w { wPhi1 = wPhi1 w + 0.05 }
      SpecialKey KeyLeft -> w { wPhi1 = wPhi1 w - 0.05 }
      SpecialKey KeyUp -> w { wPhi2 = wPhi2 w + 0.05 }
      SpecialKey KeyDown -> w { wPhi2 = wPhi2 w - 0.05 }
      Char c | c `elem` "cC" -> w { wCompose = not (wCompose w) }
             | c `elem` "rR" -> w { wPhi1 = 0.55, wPhi2 = 0.4, wCompose = False }
      _ -> w

-- ---- 自己テスト（p5.js 版と同じ 8 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest = evalRng 11 $ do
  rs <- mapM (\_ -> do
          a <- (\x -> x * 4 - 2) <$> nextR
          b <- (\x -> x * 4 - 2) <$> nextR
          v <- randVec sta
          let r = boost a
              g = sandwich sta r gam0
              va = tanh a
              vb = tanh b
              ww = sandwich sta (gp sta (boost b) r) gam0
              rv = sandwich sta r v
          pure
            [ nearM (gp sta r (rev sta r)) (scalar sta 1) 1e-12
            , near (g U.! 1) (cosh a) 1e-12 && near (g U.! 2) (sinh a) 1e-12
            , nearM (gp sta (boost b) r) (boost (a + b)) 1e-10
            , near ((ww U.! 2) / (ww U.! 1)) ((va + vb) / (1 + va * vb)) 1e-12
            , near (gp sta rv rv U.! 0) (gp sta v v U.! 0) 1e-10 ]) [1 .. 40 :: Int]
  let col k = all (\x -> getAt False x k) rs
      light = addM gam0 gam1
      lb = sandwich sta (boost 1.3) light
      st4 = makeAlgebra [1, -1, -1, -1]
      rx = expB st4 (smul (blade st4 3 1) (-0.7)) False
  pure
    [ ("g0^2 = +1, g1^2 = -1, g0g1 = -g1g0, B^2 = (g0g1)^2 = +1"
      , nearM (gp sta gam0 gam0) (scalar sta 1) 1e-9 && nearM (gp sta gam1 gam1) (scalar sta (-1)) 1e-9 && nearM (gp sta gam0 gam1) (smul (gp sta gam1 gam0) (-1)) 1e-9 && nearM (gp sta bb bb) (scalar sta 1) 1e-9)
    , ("R R~ = 1 (the rotor has size 1)", col 0)
    , ("R g0 R~ = cosh(phi) g0 + sinh(phi) g1 -> speed v = tanh(phi)", col 1)
    , ("R(phi2) R(phi1) = R(phi1 + phi2) (rapidities add)", col 2)
    , ("composed speed = (v1 + v2)/(1 + v1v2) (relativistic velocity addition)", col 3)
    , ("t^2 - x^2 does not change under boosts (invariant interval)", col 4)
    , ("the light vector g0 + g1 keeps v^2 = 0 after a boost (same light speed for everyone), its length grows by e^phi (Doppler)"
      , near (gp sta lb lb U.! 0) 0 1e-12 && near (lb U.! 1) (exp 1.3) 1e-12)
    , ("Cl(1,3): a boost along x leaves g2, g3 alone and sends g0 -> cosh g0 + sinh g1"
      , nearM (sandwich st4 rx (blade st4 4 1)) (blade st4 4 1) 1e-12 && nearM (sandwich st4 rx (blade st4 8 1)) (blade st4 8 1) 1e-12
        && nearM (sandwich st4 rx (blade st4 1 1)) (addM (blade st4 1 (cosh 1.4)) (blade st4 2 (sinh 1.4))) 1e-12) ]

-- ---------- やってみよう ----------
-- 1. → を押し続けて φ1 を大きくしよう。紫と水色の軸は光円錐（黄色）に近づくが、こえる？
-- 2. マウスを光円錐の上に置くと、両方の観測者で t² − x² はいくつ？
-- 3. C で合成。φ1 = φ2 = 1（v ≈ 0.76c）を 2 回合成した速さは？ 1.52c になる？
-- 4. 静止系で同時（t = 0）の 2 つの出来事は、動く観測者でも同時？（水色の線と比べる）
-- 5. bb の符号をまちがえて回転にすると（sta を makeAlgebra [1, 1] にすると）何が変わる？

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
