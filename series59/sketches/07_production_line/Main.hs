-- ============================================
-- 59-07 生産ライン ─ 機械 1 → 2 → … → m の順に部品を加工。部品はパレット（台）にのって流れ、最後の機械のあと r 分で入口に戻る
-- 機械 i が部品 k を終える時刻 d_i(k) = max( d_{i−1}(k), d_i(k−1), [i = 1 なら] d_m(k−P) + r ) + p_i
--   = 「前の機械が終わる」「自分が前の部品を終える」「パレットが戻る」の遅いほう ⊗ 加工時間。これがマックスプラスの漸化式
--   周期 λ = max( いちばん遅い機械 p_i、 1 周の時間 (Σp + r) / P ) → スループット = 1/λ（個/分）
--   （シリーズ 38 の 10 は工程の固有値。ここはパレットの数 P という「トークン」とボトルネックの入れかわりを見る）
-- 操作：3・4・5 機械の数 / ← → 機械を選ぶ / ↑↓ 加工時間 / P・O パレットを増やす・減らす / R 元にもどす
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Array (Array, listArray, (!))
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

returnT :: Double
returnT = 6                -- ★ パレットが入口に戻るまでの時間 r（分）

kParts :: Int
kParts = 18                -- ガントチャートに描く部品の数

proc0 :: [Int]
proc0 = [3, 5, 4, 2, 3]    -- ★ 加工時間 p_i（分）

-- キーを押したときだけ計算し直すもの
data Calc = Calc { cD :: Array Int Vec, cLam :: !Double, cNodes :: [Int], cSlope :: !Double, cByP :: [Double] }

data World = World
  { wM    :: !Int             -- ★ 機械の数
  , wProc :: [Int]
  , wP    :: !Int             -- ★ パレットの数 P
  , wSel  :: !Int
  , wCalc :: Calc
  , wTest :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "59-07 Production line" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = recalc (World 4 proc0 3 1 (Calc (listArray (0, 0) [[]]) 0 [] 0 []) [])

-- ★ マックスプラスの漸化式で、部品 k = 1 … K の完了時刻 d ! k !! i（d ! 0 は全部 ε）
simulateLine :: [Double] -> Int -> Double -> Int -> Array Int Vec
simulateLine p np r kk = d
  where
    m = length p
    d = listArray (0, kk) (replicate m negInf : [ row k | k <- [1 .. kk] ])
    row k = let go _ [] = []
                go prev ((i, pi') : rest) =
                  let ready0 = mpAdd prev (vix (d ! (k - 1)) i)
                      ready = if i == 0 then mpAdd ready0 (if k - np >= 1 then mpMul (vix (d ! (k - np)) (m - 1)) r else 0) else ready0   -- はじめの P 台は時刻 0 に入口にある
                      v = mpMul ready pi'
                  in v : go v rest
            in go negInf (zip [0 ..] p)

-- ★ イベントグラフの辺（始点、終点、時間、トークン = パレットの数）
data EArc = EArc { eA, eB :: !Int, eW :: !Double, eTok :: !Int }

eventArcs :: [Double] -> Int -> Double -> [EArc]
eventArcs p np r = concat [ EArc i i pi' 1 : [ EArc i (i + 1) (vix p (i + 1)) 0 | i + 1 < m ] | (i, pi') <- zip [0 ..] p ]
                   ++ [EArc (m - 1) 0 (r + vix p 0) np]
  where m = length p

-- 回路を全部調べて λ = max (回路の時間の和) / (回路のトークン)。(λ, 回路のノード)
cycleRatioBrute :: Int -> [EArc] -> (Double, [Int])
cycleRatioBrute m arcs = foldl' (\best s -> dfs s s [s] 0 0 best) (negInf, []) [0 .. m - 1]
  where
    dfs s v path w tok best0 = foldl' visit best0 [ e | e <- arcs, eA e == v ]
      where visit best e
              | eB e == s = let t = tok + eTok e; ratio = (w + eW e) / fromIntegral t
                            in if t > 0 && ratio > fst best then (ratio, path) else best
              | eB e > s && eB e `notElem` path = dfs s (eB e) (path ++ [eB e]) (w + eW e) (tok + eTok e) best
              | otherwise = best

-- 漸化式を 3000 個回した傾き（最後の 840 個）
slopeSim :: [Double] -> Int -> Double -> Double
slopeSim p np r = let kk = 3000; ww = 840; d = simulateLine p np r kk; m = length p
                  in (vix (d ! kk) (m - 1) - vix (d ! (kk - ww)) (m - 1)) / fromIntegral ww

procD :: World -> [Double]
procD w = map fromIntegral (take (wM w) (wProc w))

recalc :: World -> World
recalc w = w { wCalc = Calc d lam nodes (slopeSim p (wP w) returnT) byP }
  where
    p = procD w
    d = simulateLine p (wP w) returnT kParts
    (lam, nodes) = cycleRatioBrute (wM w) (eventArcs p (wP w) returnT)
    byP = [ fst (cycleRatioBrute (wM w) (eventArcs p np returnT)) | np <- [1 .. 10] ]

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Production line: " ++ show m ++ " machines, P = " ++ show (wP w) ++ " pallets, return r = " ++ mpFmt 0 returnT
         ++ " min -> period lambda = " ++ fmt 2 lam ++ " min") 40 40 20 (grayC 240)
  , say ("3 4 5 machines   <- -> select machine (now " ++ show (wSel w + 1) ++ ")   Up/Down process time   P / O pallets +-   R reset") 40 66 15 (grayC 170) ]
  -- ---- ガントチャート
  ++ concat [ [ Color (if bott then rgbC 70 45 30 else grayC 34) (rectTL (pX g) y (pW g) 38)
              , sayR ("M" ++ show (i + 1) ++ " (" ++ mpFmt 0 pi' ++ " min)") (pX g - 10) (y + 24) 15 (if i == wSel w then rgbC 255 220 120 else grayC 220) ]
              ++ concat [ [ Color (hsb ((30 + fromIntegral ((k - 1) `mod` np) * (300 / fromIntegral np)) `fmod` 360) 55 95)
                                 (rectTL (sx s + 1) (y + 4) (max 1 (sx (min e tMax) - sx s - 2)) 30) ]
                          ++ [ sayM (show k) ((sx s + sx e) / 2) (y + 19) 14 (grayC 20) | sx e - sx s > 22 ]
                        | k <- [1 .. kParts], let e = vix (d ! k) i, let s = e - pi', s <= tMax ]
            | (i, pi') <- zip [0 ..] p, let y = pY g + fromIntegral i * 46, let bott = not loopLimited && headOr (-1) nodes == i ]
  ++ [ sayC (mpFmt 0 t ++ " min") (sx t) (pY g + pH g + 18) 14 (grayC 140) | t <- takeWhile (<= tMax) [0, stepT ..] ]
  -- ---- P を変えたときのスループット
  ++ [ plotAxes gp "number of pallets P" "throughput 1/lambda (parts/min)" [1, 2, 4, 6, 8, 10] [0, read (fmt 3 (1 / maxP))] ]
  ++ [ Color (if np == wP w then rgbC 255 170 70 else rgbaC 110 190 255 170) (rectTL (gX gp (fromIntegral np) - 12) (gY gp (1 / l)) 24 (pY gp + pH gp - gY gp (1 / l)))
     | (np, l) <- zip [1 ..] (cByP c) ]
  ++ [ Color (rgbC 255 220 120) (lineW 1.5 (pX gp, gY gp (1 / maxP)) (pX gp + pW gp, gY gp (1 / maxP))) ]
  -- ---- 説明
  ++ [ say (if loopLimited then "Bottleneck: not enough pallets (the loop circuit)"
            else "Bottleneck: machine " ++ show (headOr 0 nodes + 1) ++ " (process " ++ mpFmt 0 (vix p (headOr 0 nodes)) ++ " min)") tx ty 18
           (if loopLimited then rgbC 255 170 70 else rgbC 150 230 160)
     , say ("Machines alone: lambda >= max p_i = " ++ mpFmt 0 maxP ++ " min") tx (ty + 30) 15 (grayC 220)
     , say ("Pallet loop: (sum p + r) / P = (" ++ mpFmt 0 sump ++ " + " ++ mpFmt 0 returnT ++ ") / " ++ show (wP w) ++ " = " ++ fmt 2 ((sump + returnT) / fromIntegral (wP w)) ++ " min") tx (ty + 54) 15 (grayC 220)
     , say ("lambda = the larger = " ++ fmt 2 lam ++ " min (slope of 3000 parts " ++ fmt 4 (cSlope c) ++ ")") tx (ty + 78) 15 (grayC 220)
     , say ("Throughput 1/lambda = " ++ fmt 3 (1 / lam) ++ " parts/min") tx (ty + 102) 15 (grayC 220)
     , sayWrap "Left: more pallets raise the throughput, but it levels off at the yellow line (the slowest machine)" tx (ty + 116) (envW env - tx - 30) 15 (grayC 170)
     , drawSelfTest env (wTest w) ]
  where
    m = wM w
    np = wP w
    p = procD w
    c = wCalc w
    d = cD c
    lam = cLam c
    nodes = cNodes c
    loopLimited = length nodes > 1
    tMax = vix (d ! kParts) (m - 1)
    g = G 130 120 (envW env - 180) (46 * fromIntegral m) 0 1 0 1
    sx t = pX g + f2 (t / tMax) * pW g
    stepT = if tMax > 120 then 20 else 10
    maxP = foldl' max 1 p
    sump = sum p
    gp = G 130 (pY g + pH g + 80) (min 520 (envW env * 0.4)) (envH env - pY g - pH g - 170) 0.5 10.5 0 (1 / maxP * 1.2)
    tx = pX gp + pW gp + 60
    ty = pY gp + 10
    fmod a b = a - b * fromIntegral (floor (a / b) :: Int)

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char ch) Down _ _
    | ch `elem` "345" -> let k = read [ch] in recalc w { wM = k, wSel = min (wSel w) (k - 1) }
    | ch `elem` "pP" -> recalc w { wP = min 10 (wP w + 1) }
    | ch `elem` "oO" -> recalc w { wP = max 1 (wP w - 1) }
    | ch `elem` "rR" -> recalc w { wProc = proc0, wP = 3, wM = 4, wSel = 1 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSel = (wSel w + wM w - 1) `mod` wM w }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSel = (wSel w + 1) `mod` wM w }
  EventKey (SpecialKey KeyUp) Down _ _ -> recalc (setP (\v -> min 12 (v + 1)))
  EventKey (SpecialKey KeyDown) Down _ _ -> recalc (setP (\v -> max 1 (v - 1)))
  _ -> w
  where setP f = w { wProc = [ if i == wSel w then f v else v | (i, v) <- zip [0 ..] (wProc w) ] }

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 7・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("finish times keep all 3 conditions (previous machine, previous part, pallet)", all (\(a, _, _, _, _) -> a) rs)
  , ("no needless waiting: every finish time equals one condition + process time exactly", all (\(_, a, _, _, _) -> a) rs)
  , ("brute-force circuits lambda = max(max p_i, (sum p + r)/P) (30 random lines)", all (\(_, _, a, _, _) -> a) rs)
  , ("slope of the recursion over 3000 parts (last 840) = lambda (exact once periodic)", all (\(_, _, _, a, _) -> a) rs)
  , ("P = 1 gives lambda = sum p + r (one at a time); large P gives the slowest machine"
    , fst (cycleRatioBrute 4 (eventArcs [3, 5, 4, 2] 1 6)) == 20 && fst (cycleRatioBrute 4 (eventArcs [3, 5, 4, 2] 10 6)) == 5)
  , ("no NaN even with eps (nothing done yet)", all (\(_, _, _, _, a) -> a) rs) ]
  where
    rs = withSeed 7 (mapM one [0 .. 29 :: Int])
    one t = do
      let m = 3 + t `mod` 3
      p <- map fromIntegral <$> replicateM m (rndInt 1 9)
      np <- rndInt 1 8
      r <- fromIntegral <$> rndInt 0 10
      let kk = 60
          d = simulateLine p np r kk
          pres k i = [ if i > 0 then vix (d ! k) (i - 1) else negInf, vix (d ! (k - 1)) i
                     , if i == 0 then (if k - np >= 1 then vix (d ! (k - np)) (m - 1) + r else 0) else negInf ]
          cells = [ (vix (d ! k) i, pres k i, vix p i) | k <- [1 .. kk], i <- [0 .. m - 1] ]
          okC = and [ not (any (\v -> not (isEps v) && dk < v + pi') pre) | (dk, pre, pi') <- cells ]
          okT = and [ any (\v -> not (isEps v) && dk == v + pi') pre | (dk, pre, pi') <- cells ]
          br = fst (cycleRatioBrute m (eventArcs p np r))
          okF = abs (br - max (maximum p) ((sum p + r) / fromIntegral np)) <= 1e-9
          okS = abs (slopeSim p np r - br) <= 1e-9
          okN = noNaN (concat [ d ! k | k <- [0 .. kk] ])
      pure (okC, okT, okF, okS, okN)

-- ---------- やってみよう ----------
-- 1. O でパレットを 1 台にすると、ガントチャートはどうなる？ λ = Σp + r を確かめよう
-- 2. P を 1 ずつ増やして、スループットが頭打ちになる P を探そう。それより多く買うのはむだ
-- 3. いちばん遅い機械の加工時間を ↓ で短くすると、ボトルネックはどこへ移る？
-- 4. returnT（パレットの戻り時間）を 0 にすると、必要なパレットの数は？
-- 5. 14 では、このガントチャートの「完了」を音にして、周期 λ をリズムとして聞く

-- ==================================================
-- ここから下は「マックスプラスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。ε = −∞ は Double の −Infinity で表し、
-- ⊗ は必ず mpMul を使う（ε があれば ε。−∞ + ∞ = NaN を作らない）
-- ==================================================
-- ---- マックスプラスの数：ε = −∞（「ゼロ」= 起きない・制約なし）、e = 0（「いち」= 待ち時間なし）
negInf, posInf :: Double
negInf = -1 / 0
posInf = 1 / 0

isEps :: Double -> Bool
isEps v = v == negInf

mpAdd :: Double -> Double -> Double          -- a ⊕ b = max(a, b)
mpAdd a b = if a >= b then a else b

mpMul :: Double -> Double -> Double          -- a ⊗ b = a + b（ε があれば ε）
mpMul a b = if a == negInf || b == negInf then negInf else a + b

-- ε を「eps」と表示（−Infinity を見せない）。d = 小数点以下の桁（toFixed のあと余分な 0 を消す）
mpFmt :: Int -> Double -> String
mpFmt d v
  | v == negInf = "eps"
  | v == posInf = "+inf"
  | isNaN v = "?"
  | otherwise = let s = stripZeros (showFFloat (Just d) v "")
                in if s == "-0" then "0" else s
  where stripZeros s | '.' `elem` s = let t = reverse (dropWhile (== '0') (reverse s))
                                      in if last' t == '.' then init t else t
                     | otherwise = s
        last' t = if null t then ' ' else last t

-- 小数 1 桁（p5 の mpFmt(v) と同じ）
mpF :: Double -> String
mpF = mpFmt 1
-- ---- マックスプラス行列（リストのリスト）。(A ⊗ B)_ij = max_k (a_ik + b_kj)
type Mat = [[Double]]
type Vec = [Double]

-- (i, j) の成分（範囲の外は ε）
ix :: Mat -> Int -> Int -> Double
ix a i j = vix (rowOf a i) j

rowOf :: Mat -> Int -> Vec
rowOf a i = case drop i a of
  (r : _) | i >= 0 -> r
  _ -> []

vix :: Vec -> Int -> Double
vix v j = case drop j v of
  (x : _) | j >= 0 -> x
  _ -> negInf

setM :: Int -> Int -> Double -> Mat -> Mat
setM i j x = zipWith (\r row -> if r == i then zipWith (\c v -> if c == j then x else v) [0 ..] row else row) [0 ..]

mpMat :: Int -> Int -> Double -> Mat
mpMat n m v = replicate n (replicate m v)

mpEye :: Int -> Mat                           -- 単位行列 E（対角 e = 0、他は ε）
mpEye n = [ [ if i == j then 0 else negInf | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]

transposeM :: Mat -> Mat
transposeM a = case a of
  [] -> []
  (r : _) -> [ map (`vix` j) a | j <- [0 .. length r - 1] ]

mpMatMul :: Mat -> Mat -> Mat
mpMatMul a b = let bt = transposeM b
               in [ [ foldl' mpAdd negInf (zipWith mpMul r c) | c <- bt ] | r <- a ]

mpMatAdd :: Mat -> Mat -> Mat
mpMatAdd = zipWith (zipWith mpAdd)

mpMatVec :: Mat -> Vec -> Vec
mpMatVec a x = [ foldl' mpAdd negInf (zipWith mpMul r x) | r <- a ]

mpPow :: Mat -> Int -> Mat
mpPow a k = foldl' (\p _ -> mpMatMul p a) (mpEye (length a)) [1 .. k]

-- A* = E ⊕ A ⊕ A² ⊕ … ⊕ A^(n−1)。正の回路があると A* は発散する → 2 つ目が True（tol は計算誤差のゆとり）
mpStar :: Double -> Mat -> (Mat, Bool)
mpStar tol a = (st, positive)
  where
    n = length a
    pows = take (max 1 (n - 1)) (iterate (`mpMatMul` a) a)      -- A, A², …, A^(n−1)（n = 1 なら A だけ）
    st = foldl' mpMatAdd (mpEye n) pows
    plus = foldl' mpMatAdd a (take (max 0 (n - 1)) (drop 1 (iterate (`mpMatMul` a) a)))   -- A ⊕ … ⊕ A^n
    positive = or [ ix plus i i > tol | i <- [0 .. n - 1] ]

-- 残余（いちばん大きい劣解）：A ⊗ x ≤ b を満たす最大の x。x_j = min_i (b_i − a_ij)（a_ij = ε の行は制約なし）
mpResid :: Mat -> Vec -> Vec
mpResid a b = [ foldl' min posInf [ if isEps bi then negInf else bi - aij
                                  | (r, bi) <- zip a b, let aij = vix r j, not (isEps aij) ]
              | j <- [0 .. m - 1] ]
  where m = case a of { (r : _) -> length r; [] -> 0 }

mpVecLeq :: Double -> Vec -> Vec -> Bool
mpVecLeq tol u v = and (zipWith (\a b -> isEps a || a <= b + tol) u v) && length u <= length v

mpVecEq :: Double -> Vec -> Vec -> Bool
mpVecEq tol u v = length u <= length v && and (zipWith (\a b -> (isEps a && isEps b) || (not (isEps a) && not (isEps b) && abs (a - b) <= tol)) u v)
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> let (a, s1) = g s in runR (k a) s1)

-- 種 s で始めて結果だけ取り出す（seedRng(s) してから計算）
withSeed :: Word32 -> R a -> a
withSeed s m = fst (runR m (seedRng s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))
-- ---- グラフの道具（左上が原点、y は下向き）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv (-0.02) 1.02 ((v - x0 g) / (x1 g - x0 g))) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv (-0.02) 1.02 ((v - y0 g) / (y1 g - y0 g))) * pH g

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (grayC 95) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 18) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 38) 14 (grayC 205), say yl (pX g) (pY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 maxW で折り返す文字（左上 = (x, y)）
sayWrap :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayWrap s x y maxW size c = Pictures [ say l x (y + size + fromIntegral k * size * 1.3) size c | (k, l) <- zip [0 :: Int ..] (wrapText maxW size s) ]

wrapText :: Float -> Float -> String -> [String]
wrapText maxW size s = go (words s) ""
  where
    fits t = textW t size <= maxW
    go [] cur = [cur | not (null cur)]
    go (w : ws) "" = go ws w
    go (w : ws) cur = let t = cur ++ " " ++ w in if fits t then go ws t else cur : go (w : ws) ""

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine
-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 n 桁に丸めた数（toPrecision）
roundSig :: Int -> Double -> Double
roundSig n v0'
  | v == 0 = 0
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (n - 1 - e) :: Double
                in fromIntegral (round (v * k) :: Integer) / k
  where v = fin 0 v0'

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v = let r = roundSig 3 v
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (jsRoundI v)

jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d
-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = not . any isNaN

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
