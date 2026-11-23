-- ============================================
-- 59-13 ビタビと脱量子化 ─ 確率の掛け算は対数にすると足し算、「いちばん大きい」は max → 最尤の道さがしはマックスプラスの行列計算
-- 隠れマルコフモデル：天気（晴れ・くもり・雨）は見えず、友だちの行動（散歩・買い物・掃除）だけが見える
--   ビタビ：δ_t(j) = max_i (δ_{t−1}(i) + log a_ij) + log b_j(o_t)。確率 0 の遷移は log 0 = −∞ = ε
-- 温度 h の log-sum-exp：h·log(e^{a/h} + e^{b/h}) は h → 0 で max(a, b)（マスロフの脱量子化）。h = 1 なら前向きアルゴリズム（確率の和）
-- 操作：上の行動をクリックで変える / ↑↓ 温度 h / R ランダムな行動 / M ランダムなモデル
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

stNames, obNames :: [String]
stNames = ["Sunny", "Cloudy", "Rain"]
obNames = ["Walk", "Shop", "Clean"]

-- 隠れマルコフモデル：(はじめの確率, 遷移 a_ij, 出力 b_j(o))
data Model = Model { mPi :: [Double], mA :: [[Double]], mB :: [[Double]] }

model0 :: Model
model0 = Model [0.5, 0.3, 0.2]                                       -- ★ はじめの天気の確率
               [[0.6, 0.3, 0.1], [0.3, 0.4, 0.3], [0, 0.4, 0.6]]       -- ★ a_ij = 今日 i → 明日 j（雨 → 晴れ は 0）
               [[0.6, 0.3, 0.1], [0.3, 0.4, 0.3], [0.1, 0.3, 0.6]]     -- ★ b_j(o) = 天気 j のとき行動 o

data Vit = Vit { vLogp :: !Double, vPath :: [Int], vDelta :: [Double] }
data Brute = Brute { bLp :: !Double, bPath :: [Int], bLogSum :: !Double }

data World = World
  { wModel :: Model
  , wObs   :: [Int]          -- ★ 見えた行動の列
  , wH     :: !Double        -- ★ 温度 h
  , wVit   :: Vit
  , wBf    :: Brute
  , wCurve :: [(Double, Double)]   -- (log10 h, F_h) の曲線（モデルか行動が変わったときだけ計算）
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "59-13 Viterbi and dequantization" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = recompute (World model0 [0, 0, 1, 2, 2, 1, 0] 0.5 (Vit 0 [] []) (Brute 0 [] 0) [] [])

lg :: Double -> Double
lg p = if p > 0 then log p else negInf

at1 :: [Double] -> Int -> Double
at1 v k = case drop k v of { (x : _) | k >= 0 -> x; _ -> 0 }

-- ★ 温度 h の log-sum-exp（大きい数でもあふれないように max を引いてから）。h → 0 で max
lseH :: [Double] -> Double -> Double
lseH arr h = let m = foldl' mpAdd negInf arr
             in if isEps m then negInf
                else m + h * log (foldl' (\s v -> if isEps v then s else s + exp ((v - m) / h)) 0 arr)

-- ★ ビタビ（max-plus の行列とベクトルの積をくり返す）＋ 道のもどり
viterbi :: Model -> [Int] -> Vit
viterbi (Model p0 a b) o = Vit (at1 dN lastJ) path dN
  where
    n = length p0
    l = map (map lg) a
    em j t = lg (ix b j t)
    d0 = [ mpMul (lg p) (em j (headOr 0 o)) | (j, p) <- zip [0 ..] p0 ]
    (dN, backs) = foldl' stepT (d0, []) (drop 1 o)
    stepT (d, bs) ot =
      let res = [ (mpMul best (em j ot), arg)
                | j <- [0 .. n - 1]
                , let (best, arg) = foldl' (\(bv, ba) i -> let v = mpMul (at1 d i) (ix l i j) in if v > bv then (v, i) else (bv, ba)) (negInf, 0) [0 .. n - 1] ]
      in (map fst res, map snd res : bs)          -- bs は新しい順
    lastJ = foldl' (\lj j -> if at1 dN j > at1 dN lj then j else lj) 0 [0 .. n - 1]
    path = foldl' (\p bp -> vixI bp (headOr 0 p) : p) [lastJ] backs

-- ★ 温度 h の前向きアルゴリズム：max を h·log-sum-exp に替えただけ（h = 1 で本当の確率の和）
forwardH :: Model -> [Int] -> Double -> Double
forwardH (Model p0 a b) o h = lseH fN h
  where
    n = length p0
    l = map (map lg) a
    f0 = [ mpMul (lg p) (lg (ix b j (headOr 0 o))) | (j, p) <- zip [0 ..] p0 ]
    fN = foldl' (\f ot -> [ mpMul (lseH [ mpMul v (ix l i j) | (i, v) <- zip [0 ..] f ] h) (lg (ix b j ot)) | j <- [0 .. n - 1] ]) f0 (drop 1 o)

-- 総当たり：3^T 通りの天気の列すべて
bruteAll :: Model -> [Int] -> Brute
bruteAll (Model p0 a b) o = Brute lp bp (lg total)
  where
    n = length p0
    tt = length o
    seqOf code = take tt (map (`mod` n) (iterate (`div` n) code))
    prob s = foldl' (\p (t, (sp, st)) -> p * (ix a sp st * ix b st (vixI o t)))
                    (at1 p0 (headOr 0 s) * ix b (headOr 0 s) (headOr 0 o)) (zip [1 ..] (zip s (drop 1 s)))
    (lp, bp, total) = foldl' (\(blp, bpath, sm) code -> let s = seqOf code; p = prob s
                                                       in if lg p > blp then (lg p, s, sm + p) else (blp, bpath, sm + p))
                             (negInf, [], 0) [0 .. n ^ tt - 1]

recompute :: World -> World
recompute w = w { wVit = viterbi m o, wBf = bruteAll m o
                , wCurve = [ (lh, forwardH m o (10 ** lh)) | q <- [0 .. 80 :: Int], let lh = logBase 10 0.01 + (0 - logBase 10 0.01) * fromIntegral q / 80 ] }
  where m = wModel w
        o = wObs w

colsW :: [(Int, Int, Int)]
colsW = [(255, 200, 90), (170, 180, 200), (110, 170, 255)]

cellW :: Env -> World -> Float
cellW env w = min 110 (envW env * 0.55 / fromIntegral (length (wObs w)))

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 見えた行動とトレリス
  concat [ [ Color (grayC 50) (rectTL (ox + ft * cw - cw * 0.4) (oy - 64) (cw * 0.8) 30)
           , sayM (stName obNames ot) (ox + ft * cw) (oy - 49) 14 (grayC 240) ]
           ++ [ Color (rgbaC 255 255 255 25) (Line [(ox + (ft - 1) * cw, oy + fromIntegral i * rh), (ox + ft * cw, oy + fromIntegral j * rh)])
              | t > 0, j <- [0 .. 2], i <- [0 .. 2], ix (mA m) i j > 0 ]
         | (t, ot) <- zip [0 :: Int ..] obs, let ft = fromIntegral t ]
  ++ [ Color (rgbC 255 170 70) (lineW 4 (ox + fromIntegral (t - 1) * cw, oy + fromIntegral pj * rh) (ox + fromIntegral t * cw, oy + fromIntegral j * rh))
     | (t, (pj, j)) <- zip [1 :: Int ..] (zip path (drop 1 path)) ]
  ++ concat [ [ Color (let (r, g, b) = vixC j in rgbaC r g b (if on then 255 else 90)) (circleAt x y (if on then 30 else 22)) ]
              ++ [ Color white (ringAt x y 40 1.5) | vixI (bPath bf) t == j, t < length (bPath bf) ]
            | t <- [0 .. length obs - 1], j <- [0 .. 2]
            , let on = vixI path t == j && t < length path
                  x = ox + fromIntegral t * cw
                  y = oy + fromIntegral j * rh ]
  ++ [ sayR (stName stNames j) (ox - 30) (oy + fromIntegral j * rh + 6) 15 (grayC 220) | j <- [0 .. 2] ]
  ++ [ say l 40 (y0 + fromIntegral k * 20) 15 c | (k, (l, c)) <- zip [0 :: Int ..] answerLines ]
  -- ---- 温度 h の前向き：h → 0 でビタビ
  ++ [ plotAxes g "log10 h (left = colder)" "forward value F_h at temperature h" [-2, -1, 0] [read2 (vLogp vit), read2 (bLogSum bf)]
     , Color (rgbC 255 170 70) (polyline 2.5 [ (gX g lh, gY g f) | (lh, f) <- wCurve w ])
     , Color (rgbaC 255 220 120 150) (Line [(pX g, gY g (vLogp vit)), (pX g + pW g, gY g (vLogp vit))])
     , Color (rgbaC 255 220 120 150) (Line [(gX g 0, pY g), (gX g 0, pY g + pH g)])
     , Color white (circleAt (gX g (logBase 10 h)) (gY g fh) 10)
     , say ("h = " ++ fmt 3 h ++ ": F_h = " ++ fmt 4 fh ++ "   (h = 1 -> log(sum of probabilities) = " ++ fmt 4 (bLogSum bf) ++ ", h -> 0 -> Viterbi " ++ fmt 4 (vLogp vit) ++ ")")
           (pX g) (pY g + pH g + 58) 14 (grayC 200) ]
  -- ---- 2 つの数の log-sum-exp
  ++ [ plotAxes g2 "b (a = 0 fixed)" "h log(e^(0/h) + e^(b/h))" [-4, 0, 4] [0, 2, 4] ]
  ++ [ Color (if cur then rgbC 255 170 70 else rgbaC 110 190 255 120)
             (polyline (if cur then 3 else 1.5) [ (gX g2 b, gY g2 (lseH [0, b] hh)) | s <- [0 .. 80 :: Int], let b = -4 + 8 * fromIntegral s / 80 ])
     | (hh, cur) <- [(2, h == 2), (1, h == 1), (0.5, h == 0.5), (h, True)] ]
  ++ [ Color white (polyline 1.5 [(gX g2 (-4), gY g2 0), (gX g2 0, gY g2 0), (gX g2 4, gY g2 4)]) ]
  ++ [ say l (pX g2) (pY g2 + pH g2 + 50 + fromIntegral k * 19) 14 c | (k, (l, c)) <- zip [0 :: Int ..] notes ]
  ++ [ say ("Viterbi and dequantization   temperature h = " ++ fmt 3 h) 40 40 20 (grayC 240)
     , say "Click an action above to change it   Up/Down temperature h   R random actions   M random model" 40 66 15 (grayC 170)
     , drawSelfTest env (wTest w) ]
  where
    m = wModel w
    obs = wObs w
    tt = length obs
    vit = wVit w
    bf = wBf w
    path = vPath vit
    h = wH w
    cw = cellW env w
    ox = 120
    oy = 150
    rh = 70
    y0 = oy + 3 * rh + 10
    vixC j = case drop j colsW of { (c : _) -> c; [] -> (255, 255, 255) }
    stName names k = headOr "?" (drop k names)
    pathStr p = joinWith " > " (map (stName stNames) p)
    answerLines = [ (l, rgbC 255 220 120) | l <- wrapText (envW env * 0.58) 15 ("Viterbi: " ++ pathStr path ++ "   log P = " ++ mpFmt 4 (vLogp vit)) ]
               ++ [ (l, grayC 200) | l <- wrapText (envW env * 0.58) 15 ("Brute force (3^" ++ show tt ++ " = " ++ show ((3 :: Int) ^ tt) ++ "): " ++ pathStr (bPath bf) ++ "   log P = " ++ mpFmt 4 (bLp bf)
                                                                          ++ (if near (bLp bf) (vLogp vit) 1e-9 then "   OK same (white rings)" else "")) ]
    read2 v = fromIntegral (round (v * 100) :: Integer) / 100
    g = G 70 (y0 + 100) (envW env * 0.5) (envH env - y0 - 190) (logBase 10 0.01) 0 (vLogp vit - 1) (bLogSum bf + 1)
    fh = forwardH m obs h
    g2 = G (envW env * 0.64) 130 (envW env * 0.32) (envH env * 0.4) (-4) 4 (-1) 5
    wrapN s c = [ (l, c) | l <- wrapText (pW g2) 14 s ]
    notes = wrapN "White = max(0, b). Blue = h = 2, 1, 0.5, orange = current h" (grayC 220)
            ++ wrapN ("The gap is at most h ln 2 = " ++ fmt 4 (h * log 2) ++ " (at b = 0). As h -> 0, 'sum' becomes 'max': log(e^a + e^b) -> max(a, b)") (grayC 220)
            ++ wrapN "In the max-plus world the Legendre transform plays the role of the Fourier transform (series 43, sketch 06)" (grayC 170)

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  (x : rest) -> x ++ concatMap (sep ++) rest

-- (i, j) の成分（範囲の外は 0）
ix :: [[Double]] -> Int -> Int -> Double
ix a i j = at1 (case drop i a of { (r : _) | i >= 0 -> r; _ -> [] }) j

vixI :: [Int] -> Int -> Int
vixI xs k = case drop k xs of { (x : _) | k >= 0 -> x; _ -> 0 }

randomModel :: R Model
randomModel = do
  p0 <- rowP
  a <- mapM (const rowP) [1 .. 3 :: Int]
  b <- mapM (const rowP) [1 .. 3 :: Int]
  pure (Model p0 a b)
  where rowP = do r0 <- rnd; r1 <- rnd; r2 <- rnd
                  let r = [r0 + 0.05, r1 + 0.05, r2 + 0.05]
                      s = sum r
                  pure (map (/ s) r)

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        cw = cellW env w
        obs' = [ if abs (mx - (120 + fromIntegral t * cw)) < cw * 0.4 && abs (my - (150 - 49)) < 16 then (o + 1) `mod` 3 else o
               | (t, o) <- zip [0 :: Int ..] (wObs w) ]
    in pure (if obs' /= wObs w then recompute w { wObs = obs' } else w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wH = min 1 (wH w * 1.25) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wH = max 0.01 (wH w / 1.25) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> do
        o <- mapM (const (randomRIO (0, 2 :: Int))) (wObs w)
        pure (recompute w { wObs = o })
    | c `elem` "mM" -> do
        s <- randomRIO (0, 999999 :: Int)
        pure (recompute w { wModel = withSeed (fromIntegral s) randomModel })
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 13・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Viterbi path = brute-force most likely path (30 random models, length 6)", all (\(a, _, _, _) -> a) runs)
  , ("Viterbi log P = brute-force maximum (to 1e-9)", all (\(_, b, _, _) -> b) runs)
  , ("forward at h = 1 = log(sum of all path probabilities) (forward algorithm)", all (\(_, _, c, _) -> c) runs)
  , ("h = 0.01: Viterbi <= F_h <= Viterbi + h T ln 3 (dequantization error bound)", all (\(_, _, _, d) -> d) runs)
  , ("two numbers: max(a, b) <= h log(e^(a/h) + e^(b/h)) <= max(a, b) + h ln 2, equal to the plain formula", and pairs)
  , ("no overflow, no NaN: a = b = 1000, h = 0.001 -> 1000 + 0.001 ln 2; all eps -> eps"
    , near (lseH [1000, 1000] 0.001) (1000 + 0.001 * log 2) 1e-9 && isEps (lseH [negInf, negInf] 0.5) && noNaN (vDelta (viterbi model0 [2, 2, 0, 0]))) ]
  where
    (runs, pairs) = withSeed 13 $ do
      a <- mapM (const one) [1 .. 30 :: Int]
      b <- mapM (const pair) [1 .. 200 :: Int]
      pure (a, b)
    one = do
      md <- randomModel
      o <- mapM (const (rndInt 0 2)) [1 .. 6 :: Int]
      let v = viterbi md o
          b = bruteAll md o
          f = forwardH md o 0.01
      pure ( vPath v == bPath b, near (vLogp v) (bLp b) 1e-9, near (forwardH md o 1) (bLogSum b) 1e-9
           , f >= vLogp v - 1e-9 && f <= vLogp v + 0.01 * 6 * log 3 + 1e-9 )
    pair = do
      a <- (\u -> u * 20 - 10) <$> rnd
      b <- (\u -> u * 20 - 10) <$> rnd
      h <- (\u -> 0.01 + u * 3) <$> rnd
      let l = lseH [a, b] h
      pure (l >= max a b - 1e-12 && l <= max a b + h * log 2 + 1e-12 && near l (h * log (exp (a / h) + exp (b / h))) 1e-9)

-- ---------- やってみよう ----------
-- 1. 行動をクリックで変えて、ビタビの道（オレンジ）と総当たり（白い輪）がいつも一致するか確かめよう
-- 2. ↓ で h を小さくすると、F_h はビタビの値に近づく。誤差 ≤ h·T·ln 3 を確かめよう
-- 3. model0 の雨 → 晴れ（0）を 0.2 にすると、どの道が変わる？（行の和が 1 になるように）
-- 4. 長さ 7 の総当たりは 3^7 = 2187 通り。長さ 20 なら？ ビタビは 20 × 9 回の ⊗ ですむ
-- 5. h·log(e^{a/h} + e^{b/h}) を、a = b のとき手で計算しよう（max + h·ln 2）

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
