-- ============================================
-- 58-14 幾何代数の音 🔊 ─ マルチベクトルの 4 つの次数を 4 つの声部に
-- M = (1 + a)(1 + b)(1 + c) を幾何積で計算し、スカラー・ベクトル・二重ベクトル・擬スカラー（三重ベクトル）の大きさで 4 つの音の音量を決める
-- 二重ベクトル a∧b の向き（e12 の符号）で 3 度の音が長調 ⇔ 短調に変わる。音量は小さめから（全体の音量 master は 0.2 以下）
-- 操作：クリック 音 ON / ドラッグ a・b の先 / ↑↓ c の傾き / スペース 自動で回す / M 消音 / R 元にもどす
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.Bits (popCount, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (foldl', transpose)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

type P2 = (Double, Double)

g3 :: Alg
g3 = makeAlgebra [1, 1, 1]

master :: Double
master = 0.12                    -- ★ 全体の音量（0.2 以下にしておく）

base :: [Double]
base = [110, 220, 329.63, 440]   -- ★ 4 つの声部の音の高さ（スカラー・ベクトル・二重ベクトル・擬スカラー）

va0, vb0 :: P2
va0 = (1.2, 0.3)                 -- ★ 平面のベクトル a, b（z = 0）
vb0 = (0.2, 1.1)

unitPx :: Double
unitPx = 120

data World = World
  { wA, wB   :: !P2
  , wTiltC   :: !Double
  , wAuto    :: !Bool
  , wT       :: !Double
  , wMuted   :: !Bool
  , wAudio   :: !Bool            -- 音を始めたか（最初のクリックかキーで ON）
  , wDrag    :: !Int
  , wTest    :: [(String, Bool)] }

-- 次数 0〜3 の色と名前
gcol :: Int -> (Int, Int, Int)
gcol k = getAt (220, 220, 230) [(220, 220, 230), (90, 220, 240), (180, 130, 255), (255, 190, 110)] k

gname :: [String]
gname = ["scalar", "vector", "bivector", "trivector"]

sil :: (Int, Int, Int)
sil = (215, 220, 230)

main :: IO ()
main = runSketch (sketch "58-14 Sound of geometric algebra" world0)
  { skBack  = rgbC 14 14 18
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World va0 vb0 0.6 False 0 False False (-1) []

cVec :: World -> [Double]
cVec w = [0.5 * cos (wT w * 0.7), 0.5 * sin (wT w * 0.7), sin (wTiltC w)]

multi :: MV -> MV -> MV -> MV
multi a b c = let one = scalar g3 1 in gps g3 [addM one a, addM one b, addM one c]

-- 次数ごとの大きさ → 0〜1 の音量（合計 1）
levels :: MV -> [Double]
levels m = if s > 1e-12 && not (isNaN s || isInfinite s) then map (/ s) g else [0, 0, 0, 0]
  where g = [ gradeSize g3 m k | k <- [0 .. 3] ]
        s = foldl' (+) 0 g

-- 二重ベクトルの e12 成分が負なら 3 度を半音下げる（長調 → 短調）
freqs :: MV -> [Double]
freqs m = [ if k == 2 && m U.! 3 < 0 then f * 2 ** (-1 / 12) else f | (k, f) <- zip [0 :: Int ..] base ]

current :: World -> (MV, MV, MV, MV)
current w = (a, b, c, multi a b c)
  where a = vec g3 [fst (wA w), snd (wA w), 0]
        b = vec g3 [fst (wB w), snd (wB w), 0]
        c = vec g3 (cVec w)

toPx :: Env -> P2 -> P2
toPx env (x, y) = (realToFrac (envW env) * 0.3 + x * unitPx, realToFrac (envH env) * 0.55 - y * unitPx)

-- 1 コマ進める。音は 0.3 秒ごとに 4 つの声部を短く鳴らす（Web Audio の持続音の代わり）
step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let w | wAuto w0 = let (r1, r2) = (uncurry hypot (wA w0), uncurry hypot (wB w0))
                         a1 = uncurry (flip atan2) (wA w0) + 0.006
                         a2 = uncurry (flip atan2) (wB w0) - 0.011
                     in w0 { wT = wT w0 + 0.01, wA = (r1 * cos a1, r1 * sin a1), wB = (r2 * cos a2, r2 * sin a2) }
        | otherwise = w0
      (_, _, _, m) = current w
      lv = levels m
      fq = freqs m
  when (wAudio w && not (wMuted w) && envFrame env `mod` 18 == 0) $
    forM_ (zip3 [0 :: Int ..] lv fq) $ \(k, l, f) ->
      when (l > 0.01) $ playTone (realToFrac (clampv 50 2000 f)) (realToFrac (min 0.2 master * clampv 0 1 l)) 0.32 (if k == 0 then Sine else Triangle)
  pure w

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (grayC 35) (Pictures [ circleS 1 o (r * unitPx) | r <- [1, 2] ])
  , Color (colA (gcol 2) (if w12 >= 0 then 70 else 30)) (fillPoly [o, pa, pab, pb])
  , arrowS (col3 (gcol 1)) 3 o pa, arrowS (rgbC 90 170 255) 3 o pb, arrowS (col3 (gcol 3)) 2 o pc
  , say "a" (fst pa + 8) (snd pa) 18 (col3 (gcol 1)), say "b" (fst pb + 8) (snd pb) 18 (rgbC 90 170 255)
  , say ("c (z = " ++ fmt 2 (getAt 0 cv 2) ++ ")") (fst pc + 8) (snd pc) 18 (col3 (gcol 3)) ]
  -- 右：4 つの声部（次数ごとの大きさ = 音量）
  ++ concat [ let x = bx + fromIntegral k * (bw + 24); hk = l * hh * 0.45
                  al = floor (200 + 55 * sin (fromIntegral (envFrame env) * 0.02 * f / 110)) :: Int
              in [ Color (grayC 35) (rectTL x (by - hh * 0.45) bw (hh * 0.45))
                 , Color (colA (gcol k) al) (rectTL x by bw (-hk))
                 , sayC (getAt "" gname k) (x + bw / 2) (by + 20) 14 (col3 sil)
                 , sayC (fmt 1 f ++ " Hz") (x + bw / 2) (by + 38) 14 (col3 sil)
                 , sayC (fmt 0 (l * 100) ++ "%") (x + bw / 2) (by - hk - 6) 14 (col3 sil) ]
            | (k, l, f) <- zip3 [0 ..] lv fq ]
  -- 文字
  ++ [ say ((if wAudio w then (if wMuted w then "muted (M)" else "sound ON") else "click for sound") ++ "   drag: a, b   Up/Down: tilt of c   Space: auto " ++ (if wAuto w then "ON" else "off") ++ "   M: mute   R: reset") 20 34 18 (col3 sil) ]
  ++ [ say s 20 (62 + 23 * fromIntegral k) 15 (col3 c) | (k, (s, c)) <- zip [0 :: Int ..] rows ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    hh = realToFrac (envH env) :: Double
    ww = realToFrac (envW env) :: Double
    cv = cVec w
    (_, _, _, m) = current w
    lv = levels m
    fq = freqs m
    o = toPx env (0, 0)
    pa = toPx env (wA w)
    pb = toPx env (wB w)
    pab = toPx env (fst (wA w) + fst (wB w), snd (wA w) + snd (wB w))
    pc = toPx env (getAt 0 cv 0 - getAt 0 cv 2 * 0.35, getAt 0 cv 1 + getAt 0 cv 2 * 0.75)   -- c は z を上向きに少しずらして描く
    w12 = m U.! 3
    bx = ww * 0.58
    by = hh * 0.72
    bw = min 110 (ww * 0.08)
    rows =
      [ ("M = (1 + a)(1 + b)(1 + c) = " ++ str g3 m 2, sil) ]
      ++ [ ("<M>" ++ show k ++ " = " ++ str g3 (gradePart g3 m k) 2, gcol k) | k <- [0 .. 3] ]
      ++ [ ("a^b orientation: " ++ (if w12 >= 0 then "counterclockwise -> major (3rd = " ++ show (getAt 0 base 2) ++ " Hz)" else "clockwise -> minor (3rd one semitone lower)") ++ "   trivector part = a^b^c (volume)", (255, 210, 120)) ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = evP env pos
        pick (best, k) (i, v) = let (sx, sy) = toPx env v; d = hypot (mx - sx) (my - sy) in if d < best then (d, i) else (best, k)
    in w { wAudio = True, wDrag = snd (foldl' pick (28, -1) (zip [0 ..] [wA w, wB w])) }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion pos | wDrag w >= 0 ->
    let (mx, my) = evP env pos
        v = (clampv (-2.2) 2.2 ((mx - realToFrac (envW env) * 0.3) / unitPx), clampv (-2.2) 2.2 ((realToFrac (envH env) * 0.55 - my) / unitPx))
    in if wDrag w == 0 then w { wA = v } else w { wB = v }
  EventKey k Down _ _ -> let w' = w { wAudio = True } in case k of
    SpecialKey KeyUp -> w' { wTiltC = min 1.5 (wTiltC w + 0.1) }
    SpecialKey KeyDown -> w' { wTiltC = max (-1.5) (wTiltC w - 0.1) }
    SpecialKey KeySpace -> w' { wAuto = not (wAuto w) }
    Char c | c `elem` "mM" -> w' { wMuted = not (wMuted w) }
           | c `elem` "rR" -> w' { wA = va0, wB = vb0, wTiltC = 0.6, wT = 0, wAuto = False }
    _ -> w'
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest = evalRng 14 $ do
  rs <- mapM (\_ -> do
          a <- randVec g3; b <- randVec g3; c <- randVec g3
          let one = scalar g3 1
              m = multi a b c
              lv = levels m
              fq = freqs m
          pure
            [ nearM (addMs g3 [ gradePart g3 m g | g <- [0 .. 3] ]) m 1e-12
            , nearM (gp g3 (addM one a) (addM one b)) (addMs g3 [addM one (inner g3 a b), addM a b, wedge g3 a b]) 1e-12
            , nearM (gradePart g3 m 3) (gradePart g3 (gps g3 [a, b, c]) 3) 1e-12 && nearM (gradePart g3 (gps g3 [a, b, c]) 3) (wedges g3 [a, b, c]) 1e-12
            , all (\x -> x >= 0 && x <= 1 && not (isNaN x || isInfinite x)) lv && near (foldl' (+) 0 lv) 1 1e-12 && all (\f -> f >= 100 && f <= 1000) fq ]) [1 .. 40 :: Int]
  let col k = all (\x -> getAt False x k) rs
      z = zeroM g3
      lz = levels (multi z z z)
  pure
    [ ("M = <M>0 + <M>1 + <M>2 + <M>3 (the grades add back to M)", col 0)
    , ("(1 + a)(1 + b) = (1 + a.b) + (a + b) + a^b", col 1)
    , ("trivector part of M = <abc>3 = a^b^c", col 2)
    , ("volumes in 0-1 (sum 1), frequencies in 100-1000 Hz (40 random cases)", col 3)
    , ("all vectors 0 gives no NaN (M = 1 -> only the scalar voice sounds)", all (\x -> not (isNaN x || isInfinite x)) lz && near (getAt 0 lz 0) 1 1e-9)
    , ("volume limit: master <= 0.2", master <= 0.2) ]

-- ---------- やってみよう ----------
-- 1. a と b を平行にすると二重ベクトルの声部はどうなる？ 直角にすると？
-- 2. b を a の右側から左側へ回して、長調 ⇔ 短調が入れかわる瞬間を聞こう
-- 3. ↑↓ で c を平面に寝かせる（z = 0）と、擬スカラーの声部は？（a, b, c が同じ平面 → 体積 0）
-- 4. base の音の高さを変えて、自分の和音を作ろう（例：[130.8, 196, 261.6, 329.6]）
-- 5. multi を a b c（1 + を取る）に変えると、どの声部が鳴らなくなる？ それはなぜ？

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
