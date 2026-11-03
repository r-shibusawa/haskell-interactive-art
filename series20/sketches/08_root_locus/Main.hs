-- ============================================
-- 08 根軌跡（ゲイン K を変えると極はどう動くか）
-- フィードバック u = K (r − y) をかけると、閉ループの極は 1 + K P(s) = 0 の解
--   K を 0 から大きくしていくと、極は開ループの極から出発して動いていく（根軌跡）
--   3 次のプラントでは、K が大きいと極が虚軸を越えて右半面へ = 不安定
-- 左右キー = K / 1〜3 でプラント
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

sigLo, sigHi, omLo, omHi :: Float
sigLo = -8     -- s 平面の範囲
sigHi = 3
omLo = -8
omHi = 8

data World = World
  { wPlant  :: !Int
  , wK      :: !Double                -- ★ フィードバックゲイン
  , wNum    :: ![Double]              -- 開ループ P(s) の分子（昇べき）
  , wDen    :: ![Double]              -- 開ループ P(s) の分母（昇べき）
  , wLocus  :: !(U.Vector (Float, Float))  -- 根軌跡の点（K の小さい順に 3 個ずつ）
  , wKcrit  :: !(Maybe Double)        -- 虚軸を越える K
  , wOlPoles, wOlZeros, wClPoles :: ![Cx]
  , wStep   :: !(U.Vector Float)      -- ステップ応答
  , wStable :: !Bool
  }

plantName :: Int -> String
plantName 1 = "P = 1/((s+1)(s+2)(s+3))"
plantName 2 = "P = 1/(s(s+1)(s+2)) (integrator)"
plantName _ = "P = (s+4)/((s+1)(s+2)(s+3)) (zero)"

makePlant :: Int -> Double -> World
makePlant p k =
  let (num, den)
        | p == 1 = ([1], polyMul [1, 1] (polyMul [2, 1] [3, 1]))
        | p == 2 = ([1], polyMul [0, 1] (polyMul [1, 1] [2, 1]))
        | otherwise = ([4, 1], polyMul [1, 1] (polyMul [2, 1] [3, 1]))
      -- ★ 根軌跡：K を対数で 0.01 〜 500 まで動かして、1 + K P(s) = 0 の根を集める
      kAt i = 10 ** (fromIntegral i / 300 * 4.7 - 2) :: Double
      rootsAt i = polyRoots (polyAdd den (polyScale num (kAt i)))
      allRoots = [ (kAt i, rootsAt i) | i <- [0 .. 300 :: Int] ]
      crit = case [ ki | (ki, rs) <- allRoots, maxRe rs > 0 ] of
        (ki : _) -> Just ki
        [] -> Nothing
      locus = U.fromList [ (realToFrac re, realToFrac im) | (_, rs) <- allRoots, (re, im) <- rs ]
  in updateK World
       { wPlant = p, wK = k, wNum = num, wDen = den, wLocus = locus, wKcrit = crit
       , wOlPoles = polyRoots den, wOlZeros = if length num > 1 then polyRoots num else []
       , wClPoles = [], wStep = U.empty, wStable = True }

maxRe :: [Cx] -> Double
maxRe = foldr (max . fst) (-1 / 0)

updateK :: World -> World
updateK w@World{..} =
  let charPoly = polyAdd wDen (polyScale wNum wK)
      cl = polyRoots charPoly
  in w { wClPoles = cl, wStable = maxRe cl < 0, wStep = stepResponse (polyScale wNum wK) charPoly 10 }

main :: IO ()
main = runSketch (sketch "20-08 Root Locus" (makePlant 1 5))
  { skBack  = hsb 230 30 10
  , skDraw  = drawAll
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures $
  [ box planeX planeY (x0 - planeX) planeH (hsba 120 50 30 0.3)
  , box x0 planeY (planeX + planeW - x0) planeH (hsba 0 60 40 0.3)
  , seg 2 (hsb 0 0 70) (x0, planeY) (x0, planeY + planeH)
  , seg 2 (hsb 0 0 70) (planeX, y0) (planeX + planeW, y0)
  ]
  -- 軌跡（K が小さい = 暗い、大きい = 明るい）
  ++ [ dot (sx re) (sy im) 4 (hsb 200 60 (remap (fromIntegral i) 0 (fromIntegral nPts) 40 100))
     | (i, (re, im)) <- zip [0 :: Int ..] (U.toList wLocus), inPlane re im ]
  -- 開ループの極（×）と零点（○）
  ++ [ cross (sx (f re)) (sy (f im)) | (re, im) <- wOlPoles ]
  ++ [ Color white (at (sx (f re), sy (f im)) (ring 8 3)) | (re, im) <- wOlZeros ]
  -- いまの閉ループ極
  ++ concat [ [ dot (sx (f re)) (sy (f im)) 14 (hsb (if re < 0 then 120 else 0) 80 100)
              , txt (sx (f re) + 10) (sy (f im) - 8) 14 white
                  (nf 2 (f re) ++ (if abs im > 1e-6 then " +/- " ++ nf 2 (f (abs im)) ++ "j" else "")) ]
            | (re, im) <- wClPoles ]
  ++
  [ txt (planeX + 10) (planeY + planeH - 10) 14 (hsb 0 0 70) "x open-loop poles   o zeros   dot: closed-loop poles"
    -- ステップ応答
  , drawStep gx gy gw gh wStep wStable (\p -> "Closed-loop step response (0-10 s), yellow = 1, +/-" ++ p)
  , txt 20 40 18 white ("K = " ++ nf 2 (f wK) ++ " (L/R)   " ++ plantName wPlant ++ " (1-3)   " ++ (if wStable then "Stable" else "UNSTABLE!"))
  , txt 20 68 15 c60 "Closed loop T = KP/(1 + KP); poles solve 1 + KP(s) = 0. Raising K: faster, then shaky, then diverges."
  , txt 20 90 15 c60 (case wKcrit of
      Just kc -> "K crossing the imaginary axis ~ " ++ nf 1 (f kc)
      Nothing -> "No instability in this range of K")
  ]
  ++ zipWith (\i s -> txt gx (gy + gh + 30 + i * 24) 15 c60 s) [0 ..]
     [ "The locus starts at the poles on the real axis, meets,"
     , "then splits up and down. A zero pulls the locus and"
     , "helps stability (3). An integrator (2) reaches the"
     , "axis even at low K."
     , "Final value = KP(0)/(1 + KP(0)): plant 1 gives K/(6+K)"
     , "(offset remains); plant 2 gives 1 (no offset)." ]
  where
    wd = envW env
    ht = envH env
    planeX = wd * 0.05
    planeY = ht * 0.18
    planeW = wd * 0.42
    planeH = ht * 0.66
    sx re = remap re sigLo sigHi planeX (planeX + planeW)
    sy im = remap im omLo omHi (planeY + planeH) planeY
    x0 = sx 0
    y0 = sy 0
    inPlane re im = re > sigLo && re < sigHi && im > omLo && im < omHi
    nPts = U.length wLocus
    f = realToFrac :: Double -> Float
    cross x y = Pictures [ seg 3 white (x - 8, y - 8) (x + 8, y + 8), seg 3 white (x - 8, y + 8) (x + 8, y - 8) ]
    gx = wd * 0.52
    gy = ht * 0.2
    gw = wd * 0.44
    gh = ht * 0.4
    c60 = hsb 0 0 60

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> updateK w { wK = min (wK w * 1.15) 500 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> updateK w { wK = max (wK w / 1.15) 0.01 }
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> makePlant (fromEnum c - fromEnum '0') (wK w)
  _ -> w

-- ---------- やってみよう ----------
-- 1. プラント 1 で K を上げていき、極が虚軸を越える瞬間（K ≈ 60）のステップ応答を見る
-- 2. 同じ K で 1 と 3 を比べる。零点がある 3 は不安定にならない。なぜ？（軌跡が零点に吸い寄せられる）
-- 3. 2（積分器つき）は K = 6 で限界。でも定常値は 1（偏差なし）。04・05 の話とつながる
-- 4. 極が実軸で合流して分かれる点（分岐点）を探す。そこが「ゆれ始める K」

-- ==================================================
-- ここから下は「道具」です
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を gloss の座標に直す
screen :: Env -> Picture -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- マウスの位置を p5.js の座標に直す
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字：(x, y) は文字の下端。txt = 左揃え、txtC = 中央揃え
txt, txtC :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = Translate x y (Scale 1 (-1) (label 0 0 s c str))
txtC x y s c str = txt (x - textW s str / 2) y s c str

-- 文字の幅のおよその値
textW :: Float -> String -> Float
textW s str = fromIntegral (length str) * s * 0.72

-- 左上 (x, y)、幅 w、高さ h の塗りつぶした長方形
box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2) (y + h / 2) w h)

-- 太さ wt の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg wt c a b = Color c (thickLine wt a b)

-- 太さ wt の枠
frameBox :: Float -> Color -> Float -> Float -> Float -> Float -> Picture
frameBox wt c x y w h = Color c (thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 直径 d の円
dot :: Float -> Float -> Float -> Color -> Picture
dot x y d c = Color c (at (x, y) (disc (d / 2)))

-- 小数点以下 d 桁の文字列（p5.js の nf）
nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

-- ---------- 複素数と多項式 ----------
-- 複素数は (実部, 虚部)。根の計算は桁が必要なので Double を使う
type Cx = (Double, Double)

cmul, cdiv, csub :: Cx -> Cx -> Cx
cmul (a, b) (c, d) = (a * c - b * d, a * d + b * c)
cdiv (a, b) (c, d) = let e = c * c + d * d + 1e-30 in ((a * c + b * d) / e, (b * c - a * d) / e)
csub (a, b) (c, d) = (a - c, b - d)

cabs :: Cx -> Double
cabs (a, b) = sqrt (a * a + b * b)

-- 多項式は昇べきの係数のリスト：c0 + c1 s + c2 s² …
polyMul :: [Double] -> [Double] -> [Double]
polyMul a b = foldr (\ai acc -> zipLong (map (* ai) b) (0 : acc)) [] a

zipLong :: [Double] -> [Double] -> [Double]
zipLong (x : xs) (y : ys) = (x + y) : zipLong xs ys
zipLong xs [] = xs
zipLong [] ys = ys

polyAdd :: [Double] -> [Double] -> [Double]
polyAdd = zipLong

polyScale :: [Double] -> Double -> [Double]
polyScale a k = map (* k) a

-- 複素数 s で評価（ホーナー法）
polyEvalC :: [Double] -> Cx -> Cx
polyEvalC c s = foldr (\ci acc -> let (re, im) = cmul acc s in (re + ci, im)) (0, 0) c

-- 多項式の根（Durand-Kerner 法）：昇べきの係数 → 根のリスト
polyRoots :: [Double] -> [Cx]
polyRoots c0 =
  let c = reverse (dropWhile (\v -> abs v < 1e-12) (reverse c0))
      n = length c - 1
  in case reverse c of
       (lead : _) | n >= 1 ->
         let a = map (/ lead) c
             th i = 2 * pi * fromIntegral i / fromIntegral n + 0.3
             start = [(0.4 * cos (th i), 0.9 * sin (th i)) | i <- [0 .. n - 1]]
             clean (re, im) = (re, if abs im < 1e-7 then 0 else im)
         in map clean (iterRoots a n (200 :: Int) start)
       _ -> []

iterRoots :: [Double] -> Int -> Int -> [Cx] -> [Cx]
iterRoots _ _ 0 rs = rs
iterRoots a n it rs =
  let (rs', mv) = foldl' (sweep a) (rs, 0) [0 .. n - 1]
  in if mv < 1e-10 then rs' else iterRoots a n (it - 1) rs'

-- i 番目の根を 1 つ動かす。動いた量の最大も覚えておく
sweep :: [Double] -> ([Cx], Double) -> Int -> ([Cx], Double)
sweep a (rs, mv) i =
  let ri = rs !! i
      denom = foldl' cmul (1, 0) [csub ri r | (j, r) <- zip [0 ..] rs, j /= i]
      d@(dr, di) = cdiv (polyEvalC a ri) denom
      new = csub ri d
      (before, after) = splitAt i rs
  in (before ++ new : drop 1 after, max mv (abs dr + abs di))

-- ステップ応答：伝達関数 b(s)/a(s)（可制御正準形で積分、T 秒）
stepResponse :: [Double] -> [Double] -> Double -> U.Vector Float
stepResponse b a t =
  let n = length a - 1
      an = case reverse a of { (v : _) -> v; [] -> 1 }
      h = 0.002
      steps = round (t / h) :: Int
      every = max 1 (steps `div` 500)
      lim = max (-1e6) . min 1e6
      forceL xs = foldr seq xs xs
      loop !k zs acc
        | k >= steps = U.fromList (reverse acc)
        | otherwise =
            let top = (1 - sum (zipWith (*) a zs)) / an
                zs1 = zipWith (+) zs (map (* h) (drop 1 zs ++ [top]))
                y = sum (zipWith (*) b zs1)
                zs2 = forceL (map lim zs1)
                v = realToFrac (lim y) :: Float
            in if k `mod` every == 0 then v `seq` loop (k + 1) zs2 (v : acc)
               else loop (k + 1) zs2 acc
  in loop (0 :: Int) (replicate n 0) []

-- ステップ応答のグラフ（縦の範囲はデータに合わせて ±2〜±20）
drawStep :: Float -> Float -> Float -> Float -> U.Vector Float -> Bool -> (String -> String) -> Picture
drawStep gx gy gw gh resp stable title = Pictures
  [ box gx gy gw gh (hsb 0 0 14)
  , seg 1 (hsb 0 0 30) (gx, yOf 0) (gx + gw, yOf 0)
  , seg 1 (hsba 45 90 100 0.6) (gx, yOf 1) (gx + gw, yOf 1)
  , Color (hsb (if stable then 120 else 0) 80 100) (thickPath 2.5 pts)
  , txt (gx + 8) (gy + 20) 14 (hsb 0 0 70) (title (nf 1 peak)) ]
  where
    peak = min 20 (U.foldl' (\m v -> max m (abs v)) 2 resp)
    yOf v = remap v (-peak) peak (gy + gh) gy
    n = U.length resp
    pts = [ (remap (fromIntegral i) 0 (fromIntegral (max 1 (n - 1))) gx (gx + gw), yOf (clampF (-peak) peak v))
          | (i, v) <- zip [0 :: Int ..] (U.toList resp) ]
