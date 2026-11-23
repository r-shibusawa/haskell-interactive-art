-- ============================================
-- 57-00 複素数と平面の回転 ─ 掛け算 = 回転と拡大
-- 複素数 z = x + iy を平面の点とみなす。w を掛けると、z は arg w だけ回って |w| 倍にのびる
-- e^{iθ} = cos θ + i sin θ（級数を本当に足して確かめる）。掛ける順番を変えても同じ（交換法則）
-- 操作：z と w をドラッグ / スペース w を何回も掛ける（らせん）/ ← → θ（E のとき）/ E e^{iθ} を級数で / R 最初から
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

type C = (Double, Double)              -- 複素数 (実部, 虚部)

z0, w0 :: C
z0 = (1.6, 0.6)                        -- ★ 最初の z
w0 = (0.9, 0.75)                       -- ★ 最初の w

unitPx :: Double
unitPx = 110                           -- ★ 1 の長さ（ピクセル）

nTerms :: Int
nTerms = 30                            -- ★ e^{iθ} の級数を何項まで足すか

data World = World
  { wZ, wW     :: !C
  , wPowers    :: !Int
  , wShowExp   :: !Bool
  , wTheta     :: !Double
  , wDragging  :: !(Maybe Char)        -- 'z' か 'w' をつかんでいる
  , wTest      :: [(String, Bool)] }

world0 :: World
world0 = World z0 w0 0 False 1.0 Nothing []

main :: IO ()
main = runSketch (sketch "57-00 Complex numbers and plane rotation" world0)
  { skBack  = rgbC 16 20 38
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

cMul :: C -> C -> C                    -- (a + bi)(c + di)
cMul (a, b) (c, d) = (a * c - b * d, a * d + b * c)

cAbs, cArg :: C -> Double
cAbs (a, b) = hypotL [a, b]
cArg (a, b) = atan2 b a

cExpSeries :: Double -> Int -> C       -- e^{iθ} = Σ (iθ)ⁿ / n! を本当に足す
cExpSeries th n = go 0 (0, 0) (1, 0)
  where go k (sr, si) term@(tr, ti)
          | k >= n = (sr, si)
          | otherwise = go (k + 1) (sr + tr, si + ti) (cMul term (0, th / fromIntegral (k + 1)))

-- 原点の位置（画面の左寄り）
origin :: Env -> (Double, Double)
origin env = (realToFrac (envW env) * 0.36, realToFrac (envH env) * 0.56)

toScr :: Env -> C -> (Float, Float)
toScr env (a, b) = let (ox, oy) = origin env in (f2 (ox + a * unitPx), f2 (oy - b * unitPx))

fromScr :: Env -> (Double, Double) -> C
fromScr env (x, y) = let (ox, oy) = origin env in ((x - ox) / unitPx, (oy - y) / unitPx)

step :: Env -> World -> World
step env w0' =
  let w1 = case wDragging w0' of
        Just k -> let (a, b) = fromScr env (mouseP5 env)
                      c = (clampv a (-4) 4, clampv b (-3) 3)
                  in if k == 'z' then w0' { wZ = c } else w0' { wW = c }
        Nothing -> w0'
  in if wShowExp w1 then w1 { wW = cExpSeries (wTheta w1) nTerms } else w1

draw :: Env -> World -> Picture
draw env w = view env $
  -- 方眼と単位円
  [ Color (rgbC 40 50 80) (Line [(ox + k * u, 0), (ox + k * u, hh)]) | k <- [-6 .. 6] ]
  ++ [ Color (rgbC 40 50 80) (Line [(0, oy + k * u), (ww * 0.68, oy + k * u)]) | k <- [-5 .. 5] ]
  ++ [ Color (rgbC 120 130 170) (Line [(0, oy), (ww * 0.68, oy)]), Color (rgbC 120 130 170) (Line [(ox, 0), (ox, hh)])
     , Color (rgbaC 255 255 255 70) (circleLine 1 ox oy u)
     , say "Real" (ox + 4.4 * u) (oy - 6) 14 (grayC 150), say "Imag i" (ox + 6) (oy - 2.9 * u) 14 (grayC 150) ]
  -- スペース：z, zw, zw², ... のらせん
  ++ (if wPowers w > 0
        then let pts = map (toScr env) (spiral (wZ w) (wW w) (wPowers w))
             in [ Color (rgbaC 255 200 80 160) (polyline 2 pts) ] ++ [ Color (rgbC 255 200 80) (at p (disc 3)) | p <- pts ]
        else [])
  -- 角の弧：arg z（青）と arg w（橙）、arg zw = arg z + arg w
  ++ [ Color (rgbC 110 170 255) (arcDir 0 az 0.55), Color (rgbC 255 150 90) (arcDir az (az + aw) 0.75)
     , arrowTo (wZ w) (rgbC 110 170 255) "z", arrowTo (wW w) (rgbC 255 150 90) "w", arrowTo zw (rgbC 130 230 150) "zw"
     , let (px, py) = toScr env wz in Color white (circleLine 1.5 px py 10) ]
  ++ (if wShowExp w
        then let (ex, ey) = toScr env (cExpSeries (wTheta w) nTerms)
             in [ Color (rgbaC 255 150 90 120) (Line [(ox, oy), (ex, oy)]), Color (rgbaC 255 150 90 120) (Line [(ex, oy), (ex, ey)]) ]
        else [])
  -- 右の表
  ++ [ say (nm ++ " = " ++ cStr c) tx (ty + fromIntegral n * 26) 17 (grayC 240)
     | (n, (nm, c)) <- zip [0 :: Int ..] [("z", wZ w), ("w", wW w), ("z*w", zw), ("w*z", wz)] ]
  ++ [ say ("|z| |w| = " ++ fmt 4 (cAbs (wZ w) * cAbs (wW w)) ++ "   |zw| = " ++ fmt 4 (cAbs zw)) tx (ty + 120) 15 (grayC 200)
     , say ("arg z + arg w = " ++ deg (az + aw) ++ " deg") tx (ty + 144) 15 (grayC 200)
     , say ("arg zw = " ++ deg (cArg zw) ++ " deg") tx (ty + 166) 15 (grayC 200)
     , sayBox ("zw - wz = " ++ expo 1 (hypotL [fst zw - fst wz, snd zw - snd wz]) ++ "  (commutative: the order does not matter)")
              tx (ty + 176) (ww - tx - 10) 15 (rgbC 130 230 150)
     , say ("e^(i theta) = sum (i theta)^n / n!  (" ++ show nTerms ++ " terms)") tx (ty + 240) 16 (rgbC 255 200 80)
     , say ("theta = " ++ fmt 3 (wTheta w)) tx (ty + 262) 15 (grayC 220)
     , say ("series         = " ++ fmt 6 er ++ " + " ++ fmt 6 ei ++ "i") tx (ty + 282) 15 (grayC 220)
     , say ("cos + i sin = " ++ fmt 6 (cos (wTheta w)) ++ " + " ++ fmt 6 (sin (wTheta w)) ++ "i") tx (ty + 302) 15 (grayC 220)
     , sayBox ("Multiplying by i turns 90 deg, i*i = -1 turns 180 deg. A 2D rotation is just one complex multiplication. "
               ++ "And 3D rotations? Hamilton searched more than 10 years for 'triples' -> 01")
              tx (ty + 330) (ww - tx - 20) 14 (grayC 170)
     , say "Complex numbers and plane rotation: multiplying by w turns by arg w and scales by |w|" 20 36 20 (grayC 240)
     , say "Drag z (blue) and w (orange)   Space: multiply by w again and again   E: w = e^(i theta) by the series   Left/Right theta   R reset"
           20 62 15 (grayC 175)
     , say "White circle = w*z (the same point as z*w)" 20 86 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    ww = envW env; hh = envH env
    (oxD, oyD) = origin env
    ox = f2 oxD; oy = f2 oyD; u = f2 unitPx
    zw = cMul (wZ w) (wW w)
    wz = cMul (wW w) (wZ w)
    az = cArg (wZ w); aw = cArg (wW w)
    tx = ww * 0.7; ty = 120
    (er, ei) = cExpSeries (wTheta w) nTerms
    -- 角 a0 → a1 の弧（数学の向き：反時計回り）
    arcDir a0 a1 r = thickPath 3 [ (f2 (oxD + r * unitPx * cos a), f2 (oyD - r * unitPx * sin a))
                                 | s <- [0 .. 40 :: Int], let a = a0 + ((a1 - a0) * fromIntegral s) / 40 ]
    arrowTo c col lab =
      let p@(px, py) = toScr env c
          a = atan2 (py - oy) (px - ox)
          hd d = (px - 14 * cos (a + d), py - 14 * sin (a + d))
      in Color col (Pictures [ thickLine 3 (ox, oy) p, Polygon [p, hd (-0.35), hd 0.35], say lab (px + 10) (py - 8) 18 col ])

-- z, zw, zw², ...（大きくなりすぎたら止める）
spiral :: C -> C -> Int -> [C]
spiral z0' w n = z0' : go n z0'
  where go 0 _ = []
        go k c = let c' = cMul c w in if cAbs c' > 30 then [] else c' : go (k - 1) c'

cStr :: C -> String
cStr (a, b) = fmt 3 a ++ (if b >= 0 then " + " else " - ") ++ fmt 3 (abs b) ++ "i"

deg :: Double -> String                -- 角度を −180〜180 度で
deg a = fmt 1 (jsMod (jsMod (a * 180 / pi) 360 + 540) 360 - 180)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP5 env
        d c = let (px, py) = toScr env c in hypotL [mx - realToFrac px, my - realToFrac py]
    in if d (wZ w) < 30 then w { wDragging = Just 'z' }
       else if d (wW w) < 30 then w { wDragging = Just 'w', wShowExp = False }
       else w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDragging = Nothing }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPowers = if wPowers w > 0 then 0 else 24 }
  EventKey (Char c) Down _ _
    | c `elem` "eE" -> let s = not (wShowExp w) in if s then w { wShowExp = True } else w { wShowExp = False, wW = w0 }
    | c `elem` "rR" -> w { wZ = z0, wW = w0, wPowers = 0, wShowExp = False, wTheta = 1 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wTheta = wTheta w + pi / 12, wShowExp = True }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wTheta = wTheta w - pi / 12, wShowExp = True }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 7 項目・同じ種・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("commutative zw = wz (200 pairs)", e1 < 1e-12)
  , ("|zw| = |z||w|", e2 < 1e-12)
  , ("arg zw = arg z + arg w (mod 2 pi)", e3 < 1e-12)
  , ("associative (ab)c = a(bc)", e4 < 1e-12)
  , ("i*i = -1, multiplying by i sends (x, y) -> (-y, x) (90 deg turn)", ii == (-1, 0) && iv == (-2, 3))
  , ("series of e^(i theta) = cos theta + i sin theta (|theta| <= 6, 40 terms, error " ++ expo 1 e5 ++ ")", e5 < 1e-10)
  , ("multiplying by e^(i theta) = multiplying by the rotation matrix [[cos, -sin], [sin, cos]]", hypotL [fst r - fst m, snd r - snd m] < 1e-12) ]
  where
    trials = evalGen 1 $ replicateM 200 $ do
      a0 <- rndNormalG; a1 <- rndNormalG; b0 <- rndNormalG; b1 <- rndNormalG; c0 <- rndNormalG; c1 <- rndNormalG
      pure ((a0, a1), (b0, b1), (c0, c1))
    errs = [ ( hypotL [fst ab - fst ba, snd ab - snd ba]
             , abs (cAbs ab - cAbs a * cAbs b)
             , let d = cArg ab - cArg a - cArg b in abs (atan2 (sin d) (cos d))
             , let l = cMul (cMul a b) c; r' = cMul a (cMul b c) in hypotL [fst l - fst r', snd l - snd r'] )
           | (a, b, c) <- trials, let ab = cMul a b, let ba = cMul b a ]
    e1 = maximum (0 : [ x | (x, _, _, _) <- errs ])
    e2 = maximum (0 : [ x | (_, x, _, _) <- errs ])
    e3 = maximum (0 : [ x | (_, _, x, _) <- errs ])
    e4 = maximum (0 : [ x | (_, _, _, x) <- errs ])
    ii = cMul (0, 1) (0, 1)
    iv = cMul (0, 1) (3, 2)
    e5 = maximum (0 : concat [ [abs (sr - cos th), abs (si - sin th)]
                             | th <- takeWhile (<= 6) (iterate (+ 0.37) (-6)), let (sr, si) = cExpSeries th 40 ])
    w' = cExpSeries 0.7 40
    z = (2, -1)
    r = cMul w' z
    m = (cos 0.7 * 2 - sin 0.7 * (-1), sin 0.7 * 2 + cos 0.7 * (-1))

-- ---------- やってみよう ----------
-- 1. w を単位円の上（|w| = 1）に置くと、スペースのらせんはどうなる？ 1 の外・内に置くと？
-- 2. w = i（0 + 1i）にしてスペース。何回掛けると z にもどる？ w = e^{2πi/5} なら？
-- 3. E を押して ← → で θ を変えよう。級数の項数 nTerms を 5 にすると、θ が大きいときに何が起きる？
-- 4. 2 次元の回転は掛け算 1 回。では 3 次元の回転は「3 つ組の数」でできる？ ハミルトンの答え → 01

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
