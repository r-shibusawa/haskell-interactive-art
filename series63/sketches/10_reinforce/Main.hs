-- ============================================
-- 63-10 方策勾配 REINFORCE ─ 価値の表を作らずに「行動の確率」そのものを学ぶ
-- 短い廊下：3 マス + ゴール。1 歩ごとに −1。どのマスも見た目が同じ（エージェントには区別できない）のに、2 マス目だけ左右が逆になる。
-- 「いつも右」も「いつも左」もゴールに着けない → 確率で選ぶ方策（右 59% くらい）がいちばん良い。決定的な方策では解けない例
-- 方策 π(右) = e^θR / (e^θR + e^θL)（ソフトマックス）。エピソードが終わったら θ ← θ + α (G_t − b) ∇ln π(a_t)
-- ベースライン b（平均的な結果の予想）を引くと、勾配の向きは同じまま、ゆれ（分散）が小さくなる
-- 操作：F 速さ / スペース 一時停止 / R やり直し / ← → グラフの p を動かして J(p) を見る
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Exception (evaluate)
import Control.Monad (foldM, replicateM)
import Data.Bits (shiftR, xor, (.|.))
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word32)
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

p0 :: Double
p0 = 0.05                                -- ★ はじめの π(右)

alphaNB, alphaB, alphaW :: Double
alphaNB = 2 ** (-14)                     -- ★ ベースラインなしの学習率
alphaB = 2 ** (-10)                      -- ★ ベースラインありの学習率（方策）
alphaW = 2 ** (-6)                       -- ★ ベースラインありの学習率（ベースライン）

nRuns, nEpis :: Int
nRuns = 30                               -- ★ 平均をとる回数
nEpis = 1000                             -- ★ エピソード数

-- 学ぶ人：ベースラインを使うか、θ = (θR, θL)、ベースライン w、エピソード数
data Learner = Learner { lBase :: !Bool, lThR, lThL :: !Double, lW :: !Double, lEp :: !Int }

-- 画面の廊下を歩くデモ
data Demo = Demo { dS :: !Int, dPathLen :: !Int, dEp :: !Int }

data World = World
  { wLearners :: ![Learner]              -- [なし, あり]
  , wCurves   :: ![U.Vector Double]      -- エピソードごとの J の合計（なし, あり）
  , wRunsDone :: !Int
  , wSpeed    :: !Int
  , wPaused   :: !Bool
  , wProbe    :: !Double
  , wDemo     :: !Demo
  , wRng      :: !Rng
  , wTest     :: [(String, Bool)] }

speedNames :: [String]
speedNames = ["slow", "normal", "fast"]

main :: IO ()
main = runSketch (sketch "63-10 REINFORCE" (resetAll (World [] [] 0 1 False 0.5 (Demo 0 1 0) (seedRng 10) [])))
  { skBack  = bgC
  , skStart = \_ -> do
      let rs = selfTest
      printSelfTest rs
      pure (resetAll (World [] [] 0 1 False 0.5 (Demo 0 1 0) (seedRng 10) rs))
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

resetAll :: World -> World
resetAll w = w { wCurves = [U.replicate nEpis 0, U.replicate nEpis 0], wRunsDone = 0
               , wLearners = [makeLearner False, makeLearner True], wDemo = Demo 0 1 0 }

makeLearner :: Bool -> Learner
makeLearner base = Learner base (log p0) (log (1 - p0)) 0 0

-- ソフトマックス（softmaxArr [θR, θL] 1 の 1 つ目と同じ計算）
pRight :: Learner -> Double
pRight l = let a = lThR l
               b = lThL l
               m = if b > a then b else a
               ea = exp (a - m)
               eb = exp (b - m)
           in ea / (0 + ea + eb)

-- ---- 環境：マス 0, 1, 2（1 だけ左右が逆）、3 = ゴール。左のはしでは左に行けない
corridorMove :: Int -> Bool -> Int
corridorMove s right = let goRight = if s == 1 then not right else right
                       in if goRight then s + 1 else max 0 (s - 1)

-- 1 エピソードの行動の列（True = 右）。cap 歩で打ち切り
runEpisode :: Double -> Int -> R [Bool]
runEpisode p cap = go 0 0 []
  where go s n acc | s < 3 && n < cap = do u <- rnd
                                           let right = u < p
                                           go (corridorMove s right) (n + 1) (right : acc)
                   | otherwise = pure (reverse acc)

-- ★ 真の J(p)：v0 = −1 + p v1 + (1−p) v0、v1 = −1 + (1−p) v0 + p v2 ... を解いた式（v1 = (p − 3)/(p(1 − p))、v0 = v1 − 1/p）
trueJ :: Double -> Double
trueJ p | p <= 0 || p >= 1 = negInf
        | otherwise = let v1' = (p - 3) / (p * (1 - p)) in v1' - 1 / p

-- ★ REINFORCE の 1 エピソード
reinforceEpisode :: Learner -> R Learner
reinforceEpisode l0 = do
  acts <- runEpisode (pRight l0) 5000
  let tT = length acts
      upd l (t, a) =
        let g = negate (fromIntegral (tT - t))                 -- G_t = それから先の報酬の合計（γ = 1）
            pr = pRight l
            delta = g - (if lBase l then lW l else 0)
            w' = if lBase l then lW l + alphaW * delta else lW l   -- ベースライン b = w も平均の結果に近づける
            gR = (if a then 1 else 0) - pr                      -- ∇ln π(a) = (選んだら 1) − π
            gL = (if a then 0 else 1) - (1 - pr)
            al = if lBase l then alphaB else alphaNB
        in l { lW = w', lThR = clampv (lThR l + al * delta * gR) (-20) 20, lThL = clampv (lThL l + al * delta * gL) (-20) 20 }
      l1 = foldl' upd l0 (zip [0 :: Int ..] acts)
  pure l1 { lEp = lEp l1 + 1 }

-- 1 エピソードぶんの勾配の推定（θR の向き）
gradSample :: Double -> Bool -> R Double
gradSample p useBase = do
  acts <- runEpisode p 5000
  let tT = length acts
      b = if useBase then trueJ p else 0
  pure (foldl' (\g (t, a) -> g + (negate (fromIntegral (tT - t)) - b) * ((if a then 1 else 0) - p)) 0 (zip [0 :: Int ..] acts))

-- 30 回 × 1000 エピソードの平均をとる計算の 1 回ぶん
learnOnce :: World -> World
learnOnce w
  | wRunsDone w >= nRuns = w
  | otherwise =
      let (ls, g) = runWith (wRng w) (mapM reinforceEpisode (wLearners w))
          curves = [ U.modify (\mv -> UM.modify mv (+ trueJ (pRight l)) (lEp l - 1)) c | (c, l) <- zip (wCurves w) ls ]
          w1 = forceAll ls `seq` forceAll curves `seq` w { wLearners = ls, wCurves = curves, wRng = g }
      in if lEp (headOr (makeLearner False) ls) >= nEpis
           then w1 { wRunsDone = wRunsDone w1 + 1, wLearners = [makeLearner False, makeLearner True] }
           else w1

step :: Env -> Float -> World -> IO World
step env _ w0
  | wPaused w0 = pure w0
  | otherwise = do
      let budget = if wSpeed w0 == 0 then 0.5 else if wSpeed w0 == 1 then 3 else 6   -- ★ 1 フレームの計算時間（ミリ秒）
      w1 <- if wRunsDone w0 >= nRuns then pure w0
            else if wSpeed w0 == 0 then pure (learnOnce w0)
            else sliced budget (\v -> wRunsDone v < nRuns) learnOnce w0
      let fr = envFrame env
          -- 平均がそろったら、画面の 2 つをゆっくり学習
          w2 = if wRunsDone w1 >= nRuns && lEp (headOr (makeLearner False) (wLearners w1)) < nEpis && fr `mod` 2 == 0
                 then let (ls, g) = runWith (wRng w1) (mapM reinforceEpisode (wLearners w1)) in forceAll ls `seq` w1 { wLearners = ls, wRng = g }
                 else w1
          w3 = if fr `mod` 8 == 0 then demoStep w2 else w2
      pure w3
  where
    demoStep w =
      let (u, g) = runWith (wRng w) rnd
          r = u < pRight (idx (makeLearner True) (wLearners w) 1)
          d = wDemo w
          s' = corridorMove (dS d) r
          d' = if s' == 3 || dPathLen d + 1 > 60 then Demo 0 1 (dEp d + 1) else d { dS = s', dPathLen = dPathLen d + 1 }
      in w { wDemo = d', wRng = g }

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Policy gradient REINFORCE: a corridor that all looks the same" 40 42 22 (grayC 240)
  , say ("F speed (" ++ idx "" speedNames (wSpeed w) ++ ")   Space pause   R restart   <- -> move p on the graph") 40 68 15 (rgbC 170 176 210) ]
  -- ---- 廊下
  ++ concat [ [ Color (if i == 3 then goldC else rgbC 30 34 70) (rectTL x cy cw 60)
              , Color (withA (150 / 255) cyanC) (frameTL 2 x cy cw 60)
              , sayM (if i == 3 then "GOAL" else if i == 1 then "flipped" else "normal") (x + cw / 2) (cy + 30) 15 (if i == 3 then grayC 30 else grayC 230) ]
            | i <- [0 .. 3 :: Int], let x = cx0 + fromIntegral i * (cw + 10) ]
  ++ [ Color pinkC (circleAt (cx0 + fromIntegral (dS (wDemo w)) * (cw + 10) + cw / 2) (cy - 14) 18)
     , say ("Now: pi(right) with baseline = " ++ fmtN 3 pB ++ ", without = " ++ fmtN 3 pN) tx (cy + 14) 15 (grayC 230)
     , say "Best: pi(right) = 2 - sqrt 2 ~ 0.586" tx (cy + 36) 15 (grayC 230)
     , say ("thetaR = " ++ fmtN 2 (lThR lb) ++ ", thetaL = " ++ fmtN 2 (lThL lb) ++ "   episode " ++ show (lEp lb) ++ "   (" ++ show (wRunsDone w) ++ " / " ++ show nRuns ++ " runs done)") tx (cy + 58) 15 (grayC 230)
  -- ---- J(p) の曲線
     , plotAxes g "pi(right) = p" "expected return J(p)" [0, 0.25, 0.5, 0.75, 1] [-100, -50, -12, 0]
     , Color limeC (polyline 2.5 [ (gX g p, gY g (max (-100) (trueJ p))) | i <- [1 .. 199 :: Int], let p = fromIntegral i / 200 ])
     , Color goldC (circleAt (gX g pB) (gY g (max (-100) (trueJ pB))) 14)
     , Color pinkC (circleAt (gX g pN) (gY g (max (-100) (trueJ pN))) 10)
     , Color (rgbaC 255 255 255 120) (lineW 1 (gX g (wProbe w), pY g) (gX g (wProbe w), pY g + pH g))
     , say ("p = " ++ fmtN 2 (wProbe w) ++ ": J = " ++ fmtN 2 (trueJ (wProbe w))) (pX g) (pY g + pH g + 50) 14 (grayC 230)
     , say "(p = 0 or 1 gives -inf: never reaches the goal)" (pX g) (pY g + pH g + 70) 14 (grayC 230)
  -- ---- 学習曲線（平均）
     , plotAxes h "episode" ("J (mean of " ++ show (max 1 (wRunsDone w)) ++ " runs)") [0, 250, 500, 750, 1000] [-90, -50, -11.66]
     , Color (withA (120 / 255) limeC) (lineW 1 (pX h, gY h (-11.66)) (pX h + pW h, gY h (-11.66))) ]
  ++ (if wRunsDone w > 0 then [ drawCurve h (curveOf 0) pinkC 2.5 0, drawCurve h (curveOf 1) goldC 2.5 0 ] else [])
  ++ [ legend (pX h + pW h - 200) (pY h + pH h - 50) [(goldC, "with baseline"), (pinkC, "no baseline")]
     , sayWrap "Without a baseline the learning rate must be small or it goes wild -> slow. Gradient methods: series 49" (pX h) (pY h + pH h + 36) (pW h) 14 (rgbC 170 176 210)
     , drawSelfTest env (wTest w) ]
  where
    cw = 90
    cx0 = 60
    cy = 110
    tx = cx0 + 4 * (cw + 10) + 20
    ln = idx (makeLearner False) (wLearners w) 0
    lb = idx (makeLearner True) (wLearners w) 1
    pB = pRight lb
    pN = pRight ln
    g = G 80 250 (envW env * 0.36) (envH env - 360) 0 1 (-100) 0
    h = G (envW env * 0.5) 250 (envW env * 0.45) (envH env - 360) 0 (fromIntegral nEpis) (-90) (-10)
    nDone = fromIntegral (max 1 (wRunsDone w))
    curveOf k = Seq.fromList (map (/ nDone) (U.toList (idx U.empty (wCurves w) k)))

-- リストの中身まで計算しておく（計算をためこまない）
forceAll :: [a] -> ()
forceAll = foldr seq ()

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char ch) Down _ _
    | ch `elem` "fF" -> w { wSpeed = (wSpeed w + 1) `mod` 3 }
    | ch `elem` "rR" -> resetAll w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wProbe = min 0.99 (wProbe w + 0.01) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wProbe = max 0.01 (wProbe w - 0.01) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("formula J(0.3) = " ++ fmtN 2 (trueJ 0.3) ++ " ~ mean of 5000 episodes " ++ fmtN 2 mu ++ " (within 3 standard errors)", abs (mu - trueJ 0.3) < 3 * se)
  , ("J(p) is largest at p = " ++ prec3 best ++ " ~ 2 - sqrt 2 (right 59%, J ~ -11.66)", near best (2 - sqrt 2) 0.002 && near (trueJ best) (-11.657) 0.01)
  , ("REINFORCE raises the expected return: J(0.05) = " ++ fmtN 1 (trueJ p0) ++ " -> after 400 episodes with baseline " ++ fmtN 1 jB ++ ", without " ++ fmtN 1 jN
    , jB > -14 && jN > trueJ p0 + 20)
  , ("mean of gradient estimates ~ true gradient " ++ fmtN 1 exact ++ " (without " ++ fmtN 1 (meanOf gA) ++ ", with " ++ fmtN 1 (meanOf gB) ++ ")"
    , abs (meanOf gA - exact) < 3 * sqrt (va / 4000) && abs (meanOf gB - exact) < 3 * sqrt (vb / 4000))
  , ("the baseline makes the variance smaller: " ++ show (jsRoundI vb) ++ " < " ++ show (jsRoundI va), vb < va) ]
  where
    gs = withSeed 1 (replicateM 5000 (negate . fromIntegral . length <$> runEpisode 0.3 100000))
    mu = meanOf gs
    se = sqrt (meanOf (map (\v -> (v - mu) ^ (2 :: Int)) gs) / fromIntegral (length gs))
    best = foldl' (\b i -> let p = fromIntegral i / 1000 in if trueJ p > trueJ b then p else b) 0.01 [1 .. 999 :: Int]
    train base seed = withSeed seed (foldM (\l _ -> reinforceEpisode l) (makeLearner base) [1 .. 400 :: Int])
    jB = sum [ trueJ (pRight (train True (100 + r))) / 6 | r <- [0 .. 5] ]
    jN = sum [ trueJ (pRight (train False (100 + r))) / 6 | r <- [0 .. 5] ]
    gA = withSeed 7 (replicateM 4000 (gradSample 0.3 False))
    gB = withSeed 8 (replicateM 4000 (gradSample 0.3 True))
    exact = (trueJ (0.3 + 1e-6) - trueJ (0.3 - 1e-6)) / 2e-6 * 0.3 * 0.7
    va = meanOf (map (\v -> (v - meanOf gA) ^ (2 :: Int)) gA)
    vb = meanOf (map (\v -> (v - meanOf gB) ^ (2 :: Int)) gB)

-- ---------- やってみよう ----------
-- 1. alphaNB を 2 倍、4 倍にすると、ベースラインなしの学習はどうなる？（大きすぎると p が 0 か 1 に張りついて戻れない）
-- 2. p0 を 0.95（右ばかり）から始めると、どちらに向かって学ぶ？
-- 3. ← → で p を動かして、J(p) の山の形を確かめよう。なぜ p = 0.5 より右寄りがよい？
-- 4. 「決定的な方策」（いつも右・いつも左）で解ける廊下にするには、どこを変える？
-- 5. 勾配を使って山を登る方法は、シリーズ 49 の勾配法と同じ考え方。ちがいは「勾配をサンプルで推定する」こと

-- ==================================================
-- ここから下は「強化学習の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。乱数は p5.js 版と同じ mulberry32 で、同じ順に引く
-- ==================================================
-- ---- 色と小さな道具
bgC, goldC, cyanC, pinkC, limeC, violetC :: Color
bgC = rgbC 14 16 34                     -- 背景（アーケードの紺）
goldC = rgbC 255 196 60
cyanC = rgbC 90 215 245
pinkC = rgbC 245 110 170
limeC = rgbC 160 230 110
violetC = rgbC 175 140 255

negInf, posInf :: Double
negInf = -1 / 0
posInf = 1 / 0

-- v を a〜b に（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

-- いちばん大きい値の番号（同点はランダムに 1 つ。乱数を 1 つ使う）
argmaxR :: [Double] -> R Int
argmaxR q = case reverse best of
    [] -> pure 0
    bs -> do u <- rnd; pure (idx 0 bs (floor (u * fromIntegral (length bs))))
  where
    (_, best) = foldl' step (negInf, []) (zip [0 ..] q)
    step (m, b) (i, v)
      | v > m + 1e-12 = (v, [i])
      | abs (v - m) <= 1e-12 = (m, i : b)
      | otherwise = (m, b)

-- 同点は若い番号
argmaxFirst :: [Double] -> Int
argmaxFirst q = fst (foldl' (\(b, bv) (i, v) -> if v > bv then (i, v) else (b, bv)) (0, idx 0 q 0) (zip [0 ..] q))

maxOf :: [Double] -> Double
maxOf = foldl' (\m v -> if v > m then v else m) negInf

-- ε の確率でランダム、ほかは貪欲
epsGreedy :: [Double] -> Double -> R Int
epsGreedy q eps = do
  u <- rnd
  if u < eps then (\v -> floor (v * fromIntegral (length q))) <$> rnd else argmaxR q

-- ボルツマン分布 exp(h/τ) / Σ（大きい数でもあふれないように最大値を引く）
softmaxArr :: [Double] -> Double -> [Double]
softmaxArr h tau = map (/ s) e
  where
    m = maxOf h
    t = if tau == 0 || isNaN tau then 1 else tau
    e = map (\v -> exp ((v - m) / t)) h
    s = foldl' (+) 0 e

sampleFrom :: [Double] -> R Int
sampleFrom p = do
  u <- rnd
  let go _ _ [] = length p - 1
      go i c (x : xs) = let c' = c + x in if u < c' then i else go (i + 1) c' xs
  pure (go 0 0 p)

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else foldl' (+) 0 a / fromIntegral (length a)

-- 小数点以下 d 桁（toFixed）。−0.00 は 0.00 に、NaN は ?
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let s = showFFloat (Just d) v ""
                in if all (`elem` "-0.") s && take 1 s == "-" then drop 1 s else s

-- リストの i 番目（なければ d）
idx :: a -> [a] -> Int -> a
idx d xs i = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d
-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
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

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態はすぐに計算する（正格）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- 種 s で始めて結果だけ取り出す（seedRng(s) してから計算）
withSeed :: Word32 -> R a -> a
withSeed s m = fst (runR m (seedRng s))

-- 今の乱数の状態で計算して、(結果, 次の状態)
runWith :: Rng -> R a -> (a, Rng)
runWith s m = runR m s

rnd :: R Double
rnd = R (\(Rng s) -> case rndStep (Rng s) of (u, s') -> (u, s'))

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

randn :: R Double                             -- 標準正規分布（ボックス-ミュラー）
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))
-- ---- グラフの道具（左上が原点、y は下向き）：G 左 上 幅 高さ x0 x1 y0 y1
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv ((v - x0 g) / (x1 g - x0 g)) (-0.02) 1.02) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv ((v - y0 g) / (y1 g - y0 g)) (-0.02) 1.02) * pH g

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgbC 70 76 110) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 18) 14 (rgbC 150 156 190) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (rgbC 150 156 190) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 38) 14 (rgbC 205 210 235), say yl (pX g) (pY g - 8) 14 (rgbC 205 210 235) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- 直前 k 個の平均（ギザギザの学習曲線をならす）
smoothArr :: Seq.Seq Double -> Int -> Seq.Seq Double
smoothArr a k = Seq.fromList (go 0 0 (toList a))
  where
    arr = a
    go _ _ [] = []
    go !i !s (v : vs) = let s1 = s + v - (if i >= k then Seq.index arr (i - k) else 0)
                        in s1 / fromIntegral (min (i + 1) k) : go (i + 1) s1 vs

-- arr の i 番目を x = i + xOff に。長いときは間引いて 600 点まで（色 col、太さ sw）
drawCurve :: G -> Seq.Seq Double -> Color -> Float -> Double -> Picture
drawCurve g arr col sw xOff = Color col (polyline sw pts)
  where
    n = Seq.length arr
    stride = max 1 ((n + 599) `div` 600)
    pt i = (gX g (fromIntegral i + xOff), gY g (Seq.index arr i))
    pts = [ pt i | i <- [0, stride .. n - 1] ] ++ [ pt (n - 1) | n > 1, (n - 1) `mod` stride /= 0 ]

-- 色の見本と名前（items = [(色, 名前)]）
legend :: Float -> Float -> [(Color, String)] -> Picture
legend x y items = Pictures (concat [ [Color c (rectTL x (yy - 5) 16 10), say s (x + 22) (yy + 5) 14 (rgbC 220 225 245)]
                                    | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * fromIntegral k ])

-- 長さ cap までの列に追加（古いものから捨てる。メモリを増やし続けない）
pushCap :: Int -> Double -> Seq.Seq Double -> Seq.Seq Double
pushCap cap !v s = let s' = s Seq.|> v in if Seq.length s' > cap then Seq.drop 1 s' else s'
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

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

-- 点が (x, y) の長方形の中にあるか
inBox :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
inBox (mx, my) (x, y, w, h) = mx > x && mx < x + w && my > y && my < y + h
-- ---- 1 フレームの計算を budget ミリ秒までにする（p5 の do { … } while (performance.now() − t0 < budget && more)）
--      step は必ず 1 回は動く。速さ F は budget を変える
sliced :: Double -> (w -> Bool) -> (w -> w) -> w -> IO w
sliced budget more step w0 = do
  t0 <- getMonotonicTime
  let go w = do
        w' <- evaluate (step w)
        t <- getMonotonicTime
        if (t - t0) * 1000 < budget && more w' then go w' else pure w'
  go w0
-- ---- 数の道具
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt = fmtN

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
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e9) 1e9)

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d
-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- NaN も ∞ もない
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

-- 画面の右下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 225 200 else rgbC 255 90 120)
