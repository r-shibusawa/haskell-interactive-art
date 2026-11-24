-- ============================================
-- 65-13 エーレンフェストの壺 ─ 2 つの壺 A・B に玉が全部で N 個。毎回、N 個の中から 1 個をでたらめに選んで、反対の壺へ移す
-- 状態 k = A の玉の数。A が多いほど A から出やすいので、k は半分（N/2）に引き戻される。平均は (1 − 2/N) 倍ずつ N/2 に近づく。
-- 定常分布は 2 項分布 π_k = C(N, k) / 2^N（玉 1 個ずつが半々でどちらかにいる）。
-- カックの公式：状態 k に戻るまでの平均の時間 = 1/π_k。全部が A に集まった状態（k = N）に戻るには平均 2^N 回！
-- k は毎回 ±1 なので偶奇が交互（周期 2）。n 回後の分布は収束せず振動するが、時間平均の割合は 2 項分布に近づく
-- 操作：↑↓ 玉の数 N / F 速さ / スペース 一時停止 / R 全部 A から
-- （熱の逆戻りしない向き・エントロピーの話はシリーズ 55 へ。混ざった気体がひとりでに分かれないのは、戻る時間が天文学的だから）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Ratio ((%))
import qualified Data.Sequence as Q
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

speeds :: [String]
speeds = ["slow (see the moves)", "1 move/frame", "100 moves/frame", "full speed"]

anim :: Int
anim = 16                                     -- ★ ゆっくりのとき、玉が移る時間（フレーム）

fullMoves :: Int
fullMoves = 3000                              -- ★ 「全力」のときの 1 フレームの回数（p5.js 版は 6 ミリ秒ぶん）

data Moving = Moving { mvId :: !Int, mvFrom :: !(Float, Float), mvT :: !Int }

data World = World
  { wN       :: !Int                          -- ★ 玉の数 N（2〜60、偶数）
  , wSpeed   :: !Int
  , wPaused  :: !Bool
  , wUrnA    :: !(U.Vector Int)               -- A に入っている玉の番号（入った順）
  , wUrnB    :: !(U.Vector Int)
  , wK       :: !Int                          -- A の玉の数
  , wT       :: !Int                          -- 何回目か
  , wTrace   :: !(Q.Seq Int)                  -- 最近 2000 回の k
  , wOcc     :: !(U.Vector Int)               -- k にいた回数
  , wPExact  :: Vec                           -- n 回後の分布（全部 A から、行列で正確に）
  , wMoving  :: Maybe Moving
  , wRetSum  :: !Double                       -- k = N/2 に戻るまでの時間の合計と回数
  , wRetN    :: !Int
  , wLastMid :: !Int
  , wEhrP    :: Mat
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "65-13 Ehrenfest urn" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = restart (World 20 0 False U.empty U.empty 0 0 Q.empty U.empty [] Nothing 0 0 (-1) [] (rngOf 13) [])

restart :: World -> World
restart w = let n = wN w in
  w { wUrnA = U.enumFromN 0 n, wUrnB = U.empty, wK = n, wT = 0, wTrace = Q.singleton n, wOcc = U.replicate (n + 1) 0
    , wMoving = Nothing, wRetSum = 0, wRetN = 0, wLastMid = -1, wEhrP = ehrenfestMatrix n
    , wPExact = [ if k == n then 1 else 0 | k <- [0 .. n] ] }

-- ★ 遷移行列：k → k − 1 は確率 k/N（A の玉が選ばれる）、k → k + 1 は (N − k)/N
ehrenfestMatrix :: Int -> Mat
ehrenfestMatrix n = [ [ if j == k - 1 then i2d k / i2d n else if j == k + 1 then i2d (n - k) / i2d n else 0 | j <- [0 .. n] ] | k <- [0 .. n] ]

-- C(n, k) / 2^n。Haskell の Integer はあふれないので、分数でぴったり計算してから小数にする
-- （p5.js 版は対数で計算。こちらは 2^N の逆数がちょうど 2^N になる）
binomPmf :: Int -> Vec
binomPmf n = [ fromRational (choose n k % (2 ^ n)) | k <- [0 .. n] ]

choose :: Int -> Int -> Integer
choose n k = product [ fromIntegral (n - k + 1) .. fromIntegral n ] `div` product [1 .. fromIntegral k]

ehrStep :: Int -> Int -> R Int                -- 1 回（玉の番号は気にしない版）
ehrStep k n = do u <- rnd; pure (if u < i2d k / i2d n then k - 1 else k + 1)

-- 分布 × 遷移行列（vecMat と同じ計算。行列は 3 重対角なので、0 の所を飛ばして速くした版）
ehrDist :: Int -> Vec -> Vec
ehrDist n v = let vv = U.fromList v
                  get i = if i < 0 || i > n then 0 else vv U.! i
                  out j = (0 + get (j - 1) * (if j >= 1 then i2d (n - (j - 1)) / i2d n else 0)) + get (j + 1) * (if j + 1 <= n then i2d (j + 1) / i2d n else 0)
              in forceL [ out j | j <- [0 .. n] ]

-- ---- 壺の場所（draw と玉の場所で同じものを使う）
urnBoxes :: Env -> [(Float, Float, Float, Float)]
urnBoxes env = [(60, 100, uw, uh), (60 + uw + 90, 100, uw, uh)]
  where lw = envW env * 0.5
        uw = min 200 (lw * 0.36)
        uh = (uw / 6) * 10 + 10

-- 玉の場所（壺の中の何番目か）
ballPos :: Env -> World -> Int -> (Float, Float)
ballPos env w i =
  let (lst, slot) = case U.elemIndex i (wUrnA w) of
        Just s -> (0, s)
        Nothing -> (1, maybe 0 id (U.elemIndex i (wUrnB w)))
      (ux, uy, uw, uh) = at' (urnBoxes env) lst (0, 0, 0, 0)
      cell = uw / 6
  in (ux + i2f (slot `mod` 6) * cell + cell / 2, uy + uh - cell / 2 - i2f (slot `div` 6) * cell)

-- 玉を 1 個選んで反対へ
moveOne :: Env -> World -> World
moveOne env w =
  let n = wN w
      (bid, g) = runR (rndInt 0 (n - 1)) (wRng w)
      fromA = U.elem bid (wUrnA w)
      before = ballPos env w bid
      (ua, ub, k) = if fromA then (U.filter (/= bid) (wUrnA w), U.snoc (wUrnB w) bid, wK w - 1)
                             else (U.snoc (wUrnA w) bid, U.filter (/= bid) (wUrnB w), wK w + 1)
      t = wT w + 1
      tr0 = wTrace w Q.|> k
      tr = if Q.length tr0 > 2000 then Q.drop 1 tr0 else tr0
      mid = k * 2 == n
      (rs, rn) = if mid && wLastMid w >= 0 then (wRetSum w + i2d (t - wLastMid w), wRetN w + 1) else (wRetSum w, wRetN w)
  in w { wUrnA = ua, wUrnB = ub, wK = k, wT = t, wTrace = tr, wOcc = U.accum (+) (wOcc w) [(k, 1)]
       , wRetSum = rs, wRetN = rn, wLastMid = if mid then t else wLastMid w
       , wPExact = if t <= 3000 then ehrDist n (wPExact w) else wPExact w
       , wMoving = if wSpeed w == 0 then Just (Moving bid before 0) else wMoving w, wRng = g }

step :: Env -> World -> World
step env w
  | wPaused w = w
  | otherwise = case wSpeed w of
      0 -> case wMoving w of
             Nothing -> moveOne env w
             Just m -> w { wMoving = if mvT m + 1 >= anim then Nothing else Just m { mvT = mvT m + 1 } }
      1 -> moveOne env w
      2 -> iterate' 100 (moveOne env) w
      _ -> iterate' fullMoves (moveOne env) w

iterate' :: Int -> (a -> a) -> a -> a
iterate' k f a = if k <= 0 then a else let !b = f a in iterate' (k - 1) f b

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Ehrenfest urn: N = " ++ show n ++ "   move " ++ show (wT w) ++ "   balls in A k = " ++ show (wK w))
             ("Up/Down N   F speed (" ++ at' speeds (wSpeed w) "" ++ ")   Space " ++ (if wPaused w then "resume" else "pause") ++ "   R all in A")
  -- ---- 2 つの壺と玉
  , Pictures [ Pictures [ Color (col cream) (polyline 3 [(ux - 6, uy), (ux - 6, uy + uh + 6), (ux + uw + 6, uy + uh + 6), (ux + uw + 6, uy)])
                        , sayC ((if s == 1 then "B: " else "A: ") ++ show (if s == 1 then n - wK w else wK w)) (ux + uw / 2) (uy + uh + 32) 18 (col cream) ]
             | (s, (ux, uy, uw, uh)) <- zip [0 :: Int ..] boxes ]
  , Pictures [ Color (col (if odd i then teal else dice)) (circleAt px py rb)
             | i <- [0 .. n - 1]
             , let p0 = ballPos env w i
             , let (px, py) = case wMoving w of
                     Just m | mvId m == i -> let t = i2f (mvT m) / i2f anim
                                                 (fx, fy) = mvFrom m
                                             in (fx + (fst p0 - fx) * t, fy + (snd p0 - fy) * t - sin (pi * t) * 80)
                     _ -> p0 ]
  -- ---- k の時間変化
  , plotAxes g "last 600 moves" "balls in A, k (dashed = N/2, yellow = mean in theory)" [0, 300, 600] [0, i2d n / 2, i2d n]
  , Color (col sub) (dashLine 1 4 4 (grX g, gY g (i2d n / 2)) (grX g + grW g, gY g (i2d n / 2)))
  , if t0 == 0 then drawCurve g [ i2d n / 2 + (i2d n / 2) * (1 - 2 / i2d n) ^^ t | t <- [0 .. length tl - 1] ] mustard 2 0 else Blank
  , drawCurve g (map i2d tl) teal 1.5 0
  -- ---- 右：時間平均の割合 と 2 項分布、n 回後の分布（正確）
  , say ("Time average: share of time at k (" ++ show (wT w) ++ " moves)") rx 92 15 (col cream)
  , say "line = binomial C(N, k)/2^N" rx 112 15 (col cream)
  , drawBars rx 120 rw 150 [ i2d c / tot | c <- U.toList (wOcc w) ] (const dice) top Nothing
  , Color (col cream) (polyline 2 [ (rx + (i2f k + 0.5) * bw, 120 + 150 - f2 (clampv (v / top) 0 1) * 150) | (k, v) <- zip [0 :: Int ..] piv ])
  , say ("Distribution after n = " ++ show tShow ++ " moves (exact, from all in A):") rx 296 15 (col cream)
  , say "even and odd k take turns" rx 314 15 (col cream)
  , drawBars rx 320 rw 110 (wPExact w) (const teal) (top * 2) Nothing
  , sayT "k = 0" rx 434 14 (col sub) False
  , sayR "k = N" (rx + rw) (434 + 14 * 0.85) 14 (col sub)
  -- ---- カックの公式
  , say ("Back to all in A (k = N): mean 1/pi_N = 2^" ++ show n) rx ky 16 (col mustard)
  , say ("  = " ++ (if back < 10 ^ (9 :: Int) then commas back else fmtExpR 2 (fromIntegral back)) ++ " moves") rx (ky + 22) 16 (col mustard)
  , say ("At 1 move per second: " ++ humanTime (fromIntegral back)) rx (ky + 48) 15 (col cream)
  , say ("Back to half (k = N/2): mean 1/pi_(N/2) = " ++ fmtN 2 mid ++ " moves") rx (ky + 78) 15 (col cream)
  , say ("  simulation: " ++ (if wRetN w > 0 then fmtN 2 (wRetSum w / i2d (wRetN w)) ++ " (mean of " ++ show (wRetN w) ++ ")" else "-")) rx (ky + 102) 15 (col cream)
  , sayParas ["A return always happens some day, but for N = 60 the universe would end many times first."] rx (ky + 116) rw 14 (col sub)
  , drawSelfTest env (wTest w) ]
  where
    n = wN w
    boxes = urnBoxes env
    (_, _, uw0, uh0) = at' boxes 0 (0, 0, 0, 0)
    rb = (uw0 / 6) * 0.8
    lw = envW env * 0.5
    gy = 100 + uh0 + 80
    g = Graph 60 gy (lw - 80) (envH env - gy - 60) 0 600 0 (i2d n)
    trl = foldr (:) [] (wTrace w)
    tl = drop (length trl - 600) trl
    t0 = wT w - (length tl - 1)
    rx = lw + 30
    rw = envW env - rx - 40
    piv = binomPmf n
    top = maxOr 0 piv * 1.25
    tot = i2d (max 1 (U.sum (wOcc w)))
    bw = rw / i2f (n + 1)
    tShow = min (wT w) 3000
    ky = 480
    back = 2 ^ n :: Integer
    mid = 1 / at' piv (n `div` 2) 1

-- 段落を折り返して書く（左上 = (x, y)）
sayParas :: [String] -> Float -> Float -> Float -> Float -> Color -> Picture
sayParas ps x y maxW size c = Pictures [ say l x (y + size + i2f k * size * 1.3) size c | (k, l) <- zip [0 :: Int ..] (concatMap (wrapWords maxW size) ps) ]

wrapWords :: Float -> Float -> String -> [String]
wrapWords maxW size s = go (words s) ""
  where
    go [] cur = [cur | not (null cur)]
    go (x : xs) "" = go xs x
    go (x : xs) cur = let t = cur ++ " " ++ x in if textW t size <= maxW then go xs t else cur : go (x : xs) ""

-- v.toExponential(d)（v > 0 の分数でぴったり）
fmtExpR :: Int -> Rational -> String
fmtExpR d r
  | r <= 0 = "0e+0"
  | otherwise =
      let e1 = findE 0
          findE e | 10 ^^ e > r = findE (e - 1)
                  | 10 ^^ (e + 1) <= r = findE (e + 1)
                  | otherwise = e
          n0 = floor (r / 10 ^^ (e1 - d) + 1 / 2) :: Integer
          (m, e) = if n0 >= 10 ^ (d + 1) then (n0 `div` 10, e1 + 1) else (n0, e1)
          ds = show m
      in take 1 ds ++ "." ++ drop 1 ds ++ "e" ++ (if e >= 0 then "+" else "-") ++ show (abs (e :: Int))

-- 秒をわかりやすく
humanTime :: Double -> String
humanTime sec
  | sec < 120 = show (jsRound sec) ++ " s"
  | sec < 7200 = fmtN 1 (sec / 60) ++ " min"
  | sec < 172800 = fmtN 1 (sec / 3600) ++ " hours"
  | yr < 1 = fmtN 1 (sec / 86400) ++ " days"
  | otherwise = (if yr < 1e6 then fmtN 1 yr else fmtExpR 2 (toRational yr)) ++ " years" ++ (if yr > 1.38e10 then " (> age of universe)" else "")
  where yr = sec / (365.25 * 86400)

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> restart w { wN = min 60 (wN w + 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> restart w { wN = max 2 (wN w - 2) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c == 'f' || c == 'F' -> w { wSpeed = (wSpeed w + 1) `mod` length speeds, wMoving = Nothing }
    | c == 'r' || c == 'R' -> restart w
  _ -> w

-- ---------- やってみよう ----------
-- ・N = 4 にして全力で回す。全部 A に戻るのは平均 16 回に 1 回。N = 10 なら 1024 回、N = 20 なら？
-- ・「n 回後の分布」の棒が 1 つおきになるのはなぜ？（k の偶奇は毎回入れかわる）
-- ・時間平均の棒と 2 項分布の線、N が大きいほど山が細くなる（幅は √N、割合 k/N で見ると 1/√N）
-- ・ehrStep を「半分の確率で何もしない」（怠惰な連鎖）に変えると、n 回後の分布も収束する。試してみよう

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("遷移行列の各行の合計 = 1（N = 20）", rowSumErr m 1 < 1e-12)
  , ("定常分布 = 2 項分布 C(20, k)/2^20：π P = π、連立方程式の解と一致", maxAbsDiffV (vecMat piv m) piv < 1e-15 && maybe False (\s -> maxAbsDiffV s piv < 1e-10) (stationaryExact m))
  , ("既約で周期 2（タージャン + gcd）", ciIrreducible cl && maybe False ((== 2) . ccPeriod) (headOr Nothing (map Just (ciClasses cl))))
  , ("カック：全部 A に戻るまでの平均 1/π_N = 2^20 = 1,048,576（ぴったり）", 1 / at' piv n 0 == 1048576 && 1 / at' piv 0 0 == 1048576)
  , ("平均 E[k_t] = N/2 + (N/2)(1 − 2/N)^t（60 回まで 1e-9）", okMean)
  , ("n 回後の分布は 1 つおき（全変動距離 " ++ fmtN 2 (tvDist pT piv) ++ " のまま）、時間平均は 2 項分布へ（" ++ fmtE (tvDist avg piv) ++ "）", tvDist pT piv > 0.4 && tvDist avg piv < 0.01)
  , ("N = 6：空の壺 A（k = 0）に戻るまでの平均 " ++ fmtN 2 mr ++ " ≈ 64（" ++ show (length rets) ++ " 回、許容 4 SE）", abs (mr - 64) < 4 * se)
  , ("N = 10：時間平均の割合 ≈ 2 項分布（全変動距離 < 0.02）", tvDist (map (\v -> i2d v / 200000) oc) (binomPmf 10) < 0.02)
  , ("N = 60 でも π に NaN なし、合計 = 1", noNaN (binomPmf 60) && near (sumOf (binomPmf 60)) 1 1e-12) ]
  where
    n = 20
    m = ehrenfestMatrix n
    piv = binomPmf n
    cl = classifyChain m
    bigT = 4000 :: Int
    -- n 回後の分布 p と、時間平均 avg（チェザロ平均）
    (pT, avg, okMean) = foldl' stepT ([ if k == n then 1 else 0 | k <- [0 .. n] ], replicate (n + 1) 0, True) [1 .. bigT]
    stepT (p, a, ok) t =
      let p' = vecMat p m
          a' = forceL (zipWith (\s v -> s + v / i2d bigT) a p')
          mean = foldl' (\s (k, v) -> s + v * i2d k) 0 (zip [0 :: Int ..] p')
          ok' = ok && (t > 60 || abs (mean - (i2d n / 2 + (i2d n / 2) * (1 - 2 / i2d n) ^^ t)) <= 1e-9)
      in (p', a', ok')
    -- シミュレーション：N = 6 で k = 0 に戻るまで、続けて N = 10 の時間平均（種 1301 の 1 本の列）
    (rets, oc) = withSeed 1301 $ do
      rs <- simRet 1 0 0 []
      o <- simOcc 200000 10 (U.replicate 11 0)
      pure (rs, U.toList o)
    simRet :: Int -> Int -> Int -> [Double] -> R [Double]
    simRet t !k !lastT acc
      | t > 400000 = pure (reverse acc)
      | otherwise = do
          k' <- ehrStep k 6
          if k' == 0 then simRet (t + 1) k' t (i2d (t - lastT) : acc) else simRet (t + 1) k' lastT acc
    simOcc :: Int -> Int -> U.Vector Int -> R (U.Vector Int)
    simOcc 0 _ o = pure o
    simOcc c !k !o = do
      k' <- ehrStep k 10
      simOcc (c - 1) k' (U.accum (+) o [(k', 1)])
    mr = meanOf rets
    sd = sqrt (meanOf (map (\v -> (v - mr) ^ (2 :: Int)) rets))
    se = sd / sqrt (i2d (length rets))

-- ==================================================
-- ここから下は「マルコフ連鎖の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。行列 = 行のリスト、分布 = 横ベクトル（リスト）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具
type Vec = [Double]                           -- 分布・ベクトル
type Mat = [[Double]]                         -- 行列（transP !! i !! j = i の次に j へ行く確率、行の和 = 1）
type RGB = (Float, Float, Float)

bgCol, cream, dice, teal, mustard, plum, sub :: RGB
bgCol = (32, 28, 26)                          -- 背景（あたたかい炭色）
cream = (246, 236, 214)
dice = (236, 96, 58)
teal = (44, 178, 166)
mustard = (236, 190, 82)
plum = (190, 130, 200)
sub = (190, 178, 160)

stateCols :: [RGB]                            -- ★ 状態の色（8 色をくり返す）
stateCols = [(236, 96, 58), (44, 178, 166), (236, 190, 82), (190, 130, 200), (120, 190, 240), (150, 210, 110), (240, 140, 170), (200, 170, 130)]

stateCol :: Int -> RGB
stateCol i = stateCols !! (i `mod` 8)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

grayC :: Float -> Color
grayC v = col (v, v, v)

scaleRGB :: Float -> RGB -> RGB
scaleRGB k (r, g, b) = (r * k, g * k, b * k)

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

meanOf :: [Double] -> Double
meanOf xs = if null xs then 0 else sumOf xs / fromIntegral (length xs)

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float                         -- 描くときの Float（NaN は 0）
f2 = realToFrac . fin 0

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- リストの i 番目を f で変える
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

setAt :: Int -> a -> [a] -> [a]
setAt i v = modifyAt i (const v)

-- 行列の (i, j) を f で変える
modifyMat :: Int -> Int -> (Double -> Double) -> Mat -> Mat
modifyMat i j f = modifyAt i (modifyAt j f)

-- 長さ √(a² + b²)（Math.hypot）
hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Integer
jsRound v = let r = floor v in if v - fromIntegral r >= 0.5 then r + 1 else r

jsRoundI :: Double -> Int
jsRoundI v = if isNaN v || isInfinite v then 0 else fromIntegral (jsRound (clampv v (-1e15) 1e15))

-- Math.round(v * 100) / 100
round2 :: Double -> Double
round2 v = fromIntegral (jsRound (v * 100)) / 100

round1 :: Double -> Double
round1 v = fromIntegral (jsRound (v * 10)) / 10

-- 確率 p !! i で i を返す（rnd() を 1 回だけ引く。p5.js 版の sampleFrom）
sampleFrom :: [Double] -> R Int
sampleFrom p = do
  u0 <- rnd
  let u = u0 * sumOf p
      go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- ---- 数の書き方
-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）。−0.00 は 0.00
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = if d > 0 then ip ++ "." ++ fp else ip
                in if n == 0 || v > 0 then body else '-' : body

-- v.toExponential(1)：1.2e-15 の形
fmtE :: Double -> String
fmtE v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0.0e+0"
  | otherwise =
      let r = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          e1 = head ([e | e <- [e0 - 1, e0, e0 + 1], 10 ^^ e <= r, r < 10 ^^ (e + 1)] ++ [e0])
          n0 = floor (r / 10 ^^ (e1 - 1) + 1 / 2) :: Integer
          (n, e) = if n0 >= 100 then (10, e1 + 1) else (n0, e1)
      in (if v < 0 then "-" else "") ++ show (n `div` 10) ++ "." ++ show (n `mod` 10) ++ "e" ++ (if e >= 0 then "+" else "-") ++ show (abs e)

-- 整数を 3 けたごとのコンマで（toLocaleString）
commas :: Integer -> String
commas n | n < 0 = '-' : commas (negate n)
         | otherwise = reverse (go (reverse (show n)))
  where go s = case splitAt 3 s of
          (a, []) -> a
          (a, rest) -> a ++ "," ++ go rest

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v0 = let v = fin 0 v0 in
  if v == 0 then "0" else
    let e = floor (logBase 10 (abs v)) :: Int
        k = 10 ^^ (2 - e) :: Double
        r = fromIntegral (jsRound (v * k)) / k
    in if r == fromIntegral (jsRound r) then show (jsRound r) else showFFloat Nothing r ""

showD :: Double -> String                     -- JavaScript の String(v) に近い書き方（0.05、2、0.1）
showD v = if v == fromIntegral (jsRound v) && abs v < 1e15 then show (jsRound v) else showFFloat Nothing v ""

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：種 s の列（0 なら 1）
rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 計算を種 s から実行して、結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

randn :: R Double                             -- 標準正規分布（ボックス-ミュラー、u = 1 − rnd()）
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- n 回くり返して結果を順に集める（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- ---- 行列の道具（行列 = 行のリスト。遷移行列 transP !! i !! j = i の次に j へ行く確率、行の和 = 1）
zerosMat :: Int -> Mat
zerosMat n = replicate n (replicate n 0)

zerosMat2 :: Int -> Int -> Mat
zerosMat2 n m = replicate n (replicate m 0)

eyeMat :: Int -> Mat
eyeMat n = [[if i == j then 1 else 0 | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]

nCols :: Mat -> Int
nCols a = length (headOr [] a)

-- 行列の積 (a b)[i][j] = Σ_k a[i][k] b[k][j]（k の順に足す）
matMul :: Mat -> Mat -> Mat
matMul a b = [ foldl' (\acc (v, brow) -> if v == 0 then acc else zipWith (\c x -> c + v * x) acc brow) (replicate (nCols b) 0) (zip arow b) | arow <- a ]

-- 横ベクトル × 行列：次の時刻の分布 = いまの分布 × P
vecMat :: Vec -> Mat -> Vec
vecMat v a = foldl' (\acc (x, row) -> if x == 0 then acc else forceL (zipWith (\c y -> c + x * y) acc row)) (replicate (nCols a) 0) (zip v a)

forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

matPow :: Mat -> Int -> Mat                   -- P^k（k 回かけるだけ）
matPow a k = iterate (`matMul` a) (eyeMat (length a)) !! max 0 k

-- 2 つの行列（またはベクトル）のいちばん大きなずれ
maxAbsDiff :: Mat -> Mat -> Double
maxAbsDiff a b = foldl' maxN 0 (zipWith maxAbsDiffV a b)

maxAbsDiffV :: Vec -> Vec -> Double
maxAbsDiffV a b = foldl' (\d (x, y) -> maxN d (abs (x - y))) 0 (zip a b)

-- Math.max と同じく NaN が混じったら NaN
maxN :: Double -> Double -> Double
maxN a b = if isNaN a || isNaN b then 0 / 0 else max a b

rowSumErr :: Mat -> Double -> Double          -- 行の和と target のいちばん大きなずれ
rowSumErr a target = foldl' (\d r -> max d (abs (sumOf r - target))) 0 a

-- 行の和を 1 に。全部 0 の行は「その場にとどまる」にする（NaN を出さない）
fixRows :: Mat -> Mat
fixRows a = [ let s = sumOf r in if not (s > 1e-12) then [if j == i then 1 else 0 | j <- [0 .. length r - 1]] else map (/ s) r
            | (i, r) <- zip [0 :: Int ..] a ]

-- ガウスの消去法（部分ピボット）で a x = b。解けなければ Nothing
solveLinear :: Mat -> Vec -> Maybe Vec
solveLinear a b = fmap (map (headOr 0)) (solveMany a (map (: []) b))

-- 右辺が何本あっても同じ手順で消去する（右辺 1 本ずつ解くのとまったく同じ計算になる）
solveMany :: Mat -> Mat -> Maybe Mat
solveMany a bs = go 0 (zipWith (++) a bs)
  where
    n = length a
    go c m
      | c >= n = Just [ map (/ at' r i 1) (drop n r) | (i, r) <- zip [0 ..] m ]
      | otherwise =
          let colC = [ abs (at' r c 0) | r <- m ]
              p = foldl' (\pb r -> if at' colC r 0 > at' colC pb 0 then r else pb) c [c + 1 .. n - 1]
          in if at' colC p 0 < 1e-13 then Nothing else
               let rc = at' m p []
                   rp = at' m c []
                   m1 = [ if r == c then rc else if r == p then rp else row | (r, row) <- zip [0 ..] m ]
                   piv = at' rc c 1
                   m2 = [ if r == c then row else
                            let f = at' row c 0 / piv
                            in if f == 0 then row else forceL (take c row ++ zipWith (\x y -> x - f * y) (drop c row) (drop c rc))
                        | (r, row) <- zip [0 ..] m1 ]
               in go (c + 1) m2

-- 逆行列（a x = e_j を列ごとに解いたもの）。逆がなければ Nothing
matInv :: Mat -> Maybe Mat
matInv a = solveMany a (eyeMat (length a))

transposeM :: Mat -> Mat
transposeM a = if null a then [] else [ [ at' r j 0 | r <- a ] | j <- [0 .. nCols a - 1] ]

-- 定常分布：π P = π と Σ π = 1 を連立方程式として解く（(P^T − I) π = 0 の最後の式を Σ π = 1 に置きかえる）
stationaryExact :: Mat -> Maybe Vec
stationaryExact a =
  let n = length a
      m = [ if i == n - 1 then replicate n 1 else [ at' (at' a j []) i 0 - (if i == j then 1 else 0) | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]
      b = [ if i == n - 1 then 1 else 0 | i <- [0 .. n - 1] ]
  in if n == 0 then Nothing else solveLinear m b

-- べき乗法：π ← π P を変化が tol 未満になるまで。戻り値 (π, くり返した回数)
stationaryPower :: Mat -> Double -> Int -> Maybe Vec -> (Vec, Int)
stationaryPower a tol maxIt start = go v0 0 posInf
  where
    n = length a
    v0 = maybe (replicate n (1 / fromIntegral n)) id start
    go v its d
      | d > tol && its < maxIt = let w = vecMat v a in go w (its + 1) (maxAbsDiffV w v)
      | otherwise = (v, its)

tvDist :: Vec -> Vec -> Double                -- 全変動距離
tvDist p q = foldl' (+) 0 (zipWith (\x y -> abs (x - y)) p q) / 2

-- エントロピー率 H = −Σ_i π_i Σ_j P_ij log₂ P_ij（1 歩あたりの情報量、ビット）
entropyRate :: Mat -> Vec -> Double
entropyRate a pv = foldl' (\h (row, p) -> foldl' (\h2 v -> if v > 0 then h2 - p * v * logBase 2 v else h2) h row) 0 (zip a pv)

posInf :: Double
posInf = 1 / 0

-- ---- 状態の分類：強連結成分（タージャンの方法）と周期
adjOf :: Mat -> [[Int]]                       -- 確率 > 0 の矢印
adjOf a = [ [ j | (j, v) <- zip [0 ..] r, v > 1e-12 ] | r <- a ]

-- 戻り値：comp !! i = 状態 i のクラスの番号（行ったり来たりできる状態どうしが同じ番号）
sccTarjan :: [[Int]] -> [Int]
sccTarjan adj = IM.elems (tComp (foldl' (\s v -> if IM.member v (tIdx s) then s else visit v s) st0 [0 .. n - 1]))
  where
    n = length adj
    st0 = TS IM.empty IM.empty [] S.empty (IM.fromList [(i, -1) | i <- [0 .. n - 1]]) 0 0
    visit v s0 =
      let s1 = s0 { tIdx = IM.insert v (tCnt s0) (tIdx s0), tLow = IM.insert v (tCnt s0) (tLow s0), tStack = v : tStack s0, tOn = S.insert v (tOn s0), tCnt = tCnt s0 + 1 }
          s2 = foldl' (\s w -> if not (IM.member w (tIdx s)) then let s' = visit w s in s' { tLow = IM.insert v (min (low s' v) (low s' w)) (tLow s') }
                               else if S.member w (tOn s) then s { tLow = IM.insert v (min (low s v) (IM.findWithDefault 0 w (tIdx s))) (tLow s) }
                               else s) s1 (at' adj v [])
      in if low s2 v == IM.findWithDefault 0 v (tIdx s2) then popTo v s2 else s2
    low s v = IM.findWithDefault 0 v (tLow s)
    popTo v s = case tStack s of
      (w : rest) -> let s' = s { tStack = rest, tOn = S.delete w (tOn s), tComp = IM.insert w (tNc s) (tComp s) }
                    in if w == v then s' { tNc = tNc s + 1 } else popTo v s'
      [] -> s

data TS = TS { tIdx :: IM.IntMap Int, tLow :: IM.IntMap Int, tStack :: [Int], tOn :: S.Set Int, tComp :: IM.IntMap Int, tCnt :: !Int, tNc :: !Int }

gcdInt :: Int -> Int -> Int
gcdInt a b = gcd (abs a) (abs b)

-- クラスの周期：BFS の深さ lev で、クラスの中の矢印 u→v ごとに gcd(lev u + 1 − lev v)。0 = クラスの中に矢印がない
classPeriod :: [[Int]] -> [Int] -> Int
classPeriod adj members = case members of
  [] -> 0
  (m0 : _) ->
    let inC = S.fromList members
        bfs lev [] = lev
        bfs lev (u : q) = let new = [ v | v <- at' adj u [], S.member v inC ]
                              (lev', q') = foldl' (\(l, qq) v -> if IM.member v l then (l, qq) else (IM.insert v (IM.findWithDefault 0 u l + 1) l, qq ++ [v])) (lev, q) new
                          in bfs lev' q'
        lv = bfs (IM.singleton m0 0) [m0]
    in foldl' (\g (u, v) -> gcdInt g (IM.findWithDefault 0 u lv + 1 - IM.findWithDefault 0 v lv)) 0
         [ (u, v) | u <- members, v <- at' adj u [], S.member v inC ]

data ChainClass = ChainClass { ccMembers :: [Int], ccClosed :: !Bool, ccPeriod :: !Int, ccAbsorbing :: !Bool }
data ChainInfo = ChainInfo { ciComp :: [Int], ciClasses :: [ChainClass], ciIrreducible :: !Bool }

-- クラスごとに：メンバー、閉じている（外へ出ない）か、周期
classifyChain :: Mat -> ChainInfo
classifyChain a =
  let adj = adjOf a
      comp = sccTarjan adj
      nc = maxOr (-1) comp + 1
      cls = [ ChainClass mem closed (classPeriod adj mem) (length mem == 1 && at' (at' a (headOr 0 mem) []) (headOr 0 mem) 0 > 1 - 1e-12)
            | c <- [0 .. nc - 1]
            , let mem = [ i | (i, k) <- zip [0 ..] comp, k == c ]
                  closed = and [ at' comp v (-1) == c | u <- mem, v <- at' adj u [] ] ]
  in ChainInfo comp cls (nc == 1)

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

gPt :: Graph -> Double -> Double -> (Float, Float)
gPt g a b = (gX g a, gY g b)

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (col (84, 76, 70)) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (col sub) | v <- xt ]
  ++ [ sayRM (prec3 v) (grX g - 5) (gY g v) 14 (col sub) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 36) 14 (col cream), say yl (grX g) (grY g - 8) 14 (col cream) ]

-- arr !! i を x = i (+ xOff) に。長いときは間引いて 600 点まで
drawCurve :: Graph -> [Double] -> RGB -> Float -> Double -> Picture
drawCurve g arr c sw xOff =
  let n = length arr
      stride = max 1 (ceiling (i2d n / 600)) :: Int
      pts = [ (gX g (i2d i + xOff), gY g v) | (i, v) <- zip [0 ..] arr, i `mod` stride == 0 ]
            ++ [ (gX g (i2d (n - 1) + xOff), gY g (lastOr 0 arr)) | n > 1, (n - 1) `mod` stride /= 0 ]
  in Color (col c) (polyline sw pts)

legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures [ Pictures [Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col cream)]
                            | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- 棒グラフ（top = 縦軸の上限）
drawBars :: Float -> Float -> Float -> Float -> [Double] -> (Int -> RGB) -> Double -> Maybe [String] -> Picture
drawBars x y w h vals cols top names =
  let n = length vals
      bw = w / i2f (max 1 n)
  in Pictures $ Color (col (84, 76, 70)) (lineW 1 (x, y + h) (x + w, y + h))
       : [ Pictures [ Color (col (cols i)) (rectTL (x + i2f i * bw + bw * 0.12) (y + h - hh) (bw * 0.76) hh)
                    , case names of
                        Just ns -> sayT (at' ns i "") (x + i2f i * bw + bw / 2) (y + h + 4) 14 (col sub) True
                        Nothing -> Blank ]
         | (i, v) <- zip [0 ..] vals, let hh = f2 (clampv (v / (if top == 0 then 1 else top)) 0 1) * h ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 左揃え・右揃えで上下は中央（textAlign(LEFT, CENTER)、(RIGHT, CENTER)）
sayLM, sayRM :: String -> Float -> Float -> Float -> Color -> Picture
sayLM s x y size c = say s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c

-- 上が y の文字（textAlign(…, TOP)）。center = True なら中央揃え
sayT :: String -> Float -> Float -> Float -> Color -> Bool -> Picture
sayT s x y size c center = (if center then sayC else say) s x (y + size * 0.85) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (max 0 d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (max 0 d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> thickPath w pts
  _ -> Blank

-- 点線（長さ on の線と off のすき間をくり返す）
dashLine :: Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine w on off (x1, y1) (x2, y2) =
  let l = hyp (x2 - x1) (y2 - y1)
      n = floor (l / (on + off)) :: Int
      ux = if l == 0 then 0 else (x2 - x1) / l
      uy = if l == 0 then 0 else (y2 - y1) / l
  in Pictures [ lineW w (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + on), y1 + uy * min l (s + on)) | k <- [0 .. n], let s = i2f k * (on + off), s < l ]

-- 見出しと操作の説明（左上）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures [ say t 40 42 22 (col cream), say s 40 68 15 (col sub) ]

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（iw × ih の画像）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = all (\v -> not (isNaN v || isInfinite v))

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col (120, 220, 190) else col (255, 90, 90))
