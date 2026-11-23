-- ============================================
-- 63-11 小さなニューラルネットで Q 学習 ─ 落ちてくる玉を、幅 3 のかごで受ける「キャッチ」（10 × 10）
-- 状態（玉の列・玉の行・かごの列）を 30 個の 0/1 にして、隠れ層 1 層（16 個、tanh）のネットで Q(s, 左/そのまま/右) を出す。
-- 表のかわりにネットを使う Q 学習の 2 つの工夫：経験再生（昔の経験をためて、ランダムに 16 個ずつ学ぶ）と
-- ターゲットネットワーク（目標 r + γ max Q′ は、100 歩ごとにコピーした古いネットで計算 → 目標が逃げない）
-- 誤差逆伝播は手書き、最適化は Adam。重みは 1 本の unboxed ベクトル（W1 | b1 | W2 | b2）に並べる
-- 操作：F 速さ / スペース 一時停止 / R やり直し / G 見るだけ（ε = 0、学習しない）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.Bits (shiftR, xor, (.|.))
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

boardN, nIn, nH, nA :: Int
boardN = 10                -- ★ 盤の大きさ
nIn = 30                   -- 入力の数（玉の列 10 + 玉の行 10 + かごの列 10）
nH = 16                    -- ★ 隠れ層の数
nA = 3                     -- 出力（行動）の数

gammaQ, lr :: Double
gammaQ = 0.9               -- ★ 割引率
lr = 0.01                  -- ★ Adam の学習率

batchN, bufSize, targetEvery, epsSteps :: Int
batchN = 16                -- ★ 1 回に学ぶ経験の数
bufSize = 3000             -- ★ 経験再生のバッファの大きさ
targetEvery = 100          -- ★ ターゲットネットワークをコピーする間隔（歩）
epsSteps = 2400            -- ★ ε を 1 → 0.1 に下げる歩数

-- 重みの並び：W1（nH × nIn）、b1（nH）、W2（nA × nH）、b2（nA）
offB1, offW2, offB2, nParams :: Int
offB1 = nH * nIn
offW2 = offB1 + nH
offB2 = offW2 + nA * nH
nParams = offB2 + nA

speedNames :: [String]
speedNames = ["slow", "normal", "fast"]

-- ---- ゲーム（玉の列 bx、玉の行 by、かごの列 px）
data Game = Game { gBx, gBy, gPx :: !Int }

-- 1 つの経験（状態, 行動, 報酬, 次の状態, 終わったか）
data Exp = Exp !Game !Int !Int !Game !Bool

data Dqn = Dqn
  { dNet, dTgt  :: !(U.Vector Double)   -- 学習するネットとターゲットネットワーク
  , dM, dV      :: !(U.Vector Double)   -- Adam の勾配の平均と 2 乗の平均
  , dOptT       :: !Int
  , dBuf        :: !(Seq.Seq Exp)       -- 経験再生のバッファ
  , dT          :: !Int                 -- 学習した歩数
  , dGame       :: !Game
  , dGames      :: !Int                 -- 終わったゲームの数
  , dLast50     :: !(Seq.Seq Double)    -- 直近 50 回の結果（1 とれた / 0 落とした）
  , dRate       :: !(Seq.Seq Double)    -- 直近 50 回の成功率の曲線
  , dLastQ      :: ![Double]
  , dLastH      :: !(U.Vector Double) }

data World = World
  { wDqn    :: !Dqn
  , wSpeed  :: !Int
  , wPaused :: !Bool
  , wWatch  :: !Bool
  , wRng    :: !Rng
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "63-11 Tiny neural net Q-learning" (setup []))
  { skBack  = bgC
  , skStart = \_ -> do
      let rs = selfTest
      printSelfTest rs
      pure (setup rs)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

setup :: [(String, Bool)] -> World
setup rs = World d 1 False False g rs
  where (d, g) = runWith (seedRng 2) makeDqn

newGame :: R Game
newGame = do
  bx <- rnd
  px <- rnd
  pure (Game (floor (bx * fromIntegral boardN)) 0 (floor (px * fromIntegral boardN)))

-- a = 0 左、1 そのまま、2 右。(0 続く / +1 とれた / −1 落とした, 新しいゲーム)
gameStep :: Game -> Int -> (Int, Game)
gameStep (Game bx by px) a =
  let px' = max 0 (min (boardN - 1) (px + a - 1))
      by' = by + 1
      r = if by' == boardN - 1 then (if abs (bx - px') <= 1 then 1 else -1) else 0
  in (r, Game bx by' px')

-- 30 個の入力のうち 1 になる 3 つの番号
encode :: Game -> [Int]
encode (Game bx by px) = [bx, boardN + by, 2 * boardN + px]

-- ---- ニューラルネット（30 → 16 tanh → 3）
newNet :: R (U.Vector Double)
newNet = do
  w1 <- replicateM (nH * nIn) ((* sqrt (1 / 3)) <$> randn)
  w2 <- replicateM (nA * nH) ((* sqrt (1 / fromIntegral nH)) <$> randn)
  pure (U.fromList (w1 ++ replicate nH 0 ++ w2 ++ replicate nA 0))

-- x = 1 になる入力の番号（ほかは 0 なので足すだけ）。(隠れ層, Q)
forward :: U.Vector Double -> [Int] -> (U.Vector Double, [Double])
forward net x = (hid, q)
  where
    hid = U.generate nH (\j -> tanh (foldl' (\s i -> s + net U.! (j * nIn + i)) (net U.! (offB1 + j)) x))
    q = [ foldl' (\s j -> s + net U.! (offW2 + a * nH + j) * hid U.! j) (net U.! (offB2 + a)) [0 .. nH - 1] | a <- [0 .. nA - 1] ]

-- ★ 誤差逆伝播：損失 ½ (Q(s,a) − y)² の各重みでの微分を (番号, 値) の列で返す
backprop :: U.Vector Double -> [Int] -> Int -> Double -> [(Int, Double)]
backprop net x a y = (offB2 + a, d) : concat [ grad j | j <- [0 .. nH - 1] ]
  where
    (hid, q) = forward net x
    d = idx 0 q a - y
    grad j = let h = hid U.! j
                 dh = d * net U.! (offW2 + a * nH + j) * (1 - h * h)     -- tanh′ = 1 − tanh²
             in (offW2 + a * nH + j, d * h) : (offB1 + j, dh) : [ (j * nIn + i, dh) | i <- x ]

-- Adam：勾配の平均と 2 乗の平均で、重みごとに歩幅を調整。(新しい重み, m, v)
adamStep :: U.Vector Double -> U.Vector Double -> U.Vector Double -> U.Vector Double -> Int -> Int
         -> (U.Vector Double, U.Vector Double, U.Vector Double)
adamStep p gr m v t n = (U.zipWith3 (\pi' mi vi -> pi' - (lr * (mi / c1)) / (sqrt (vi / c2) + 1e-8)) p m' v', m', v')
  where
    c1 = 1 - 0.9 ** fromIntegral t
    c2 = 1 - 0.999 ** fromIntegral t
    gi = U.map (/ fromIntegral n) gr
    m' = U.zipWith (\mi g -> 0.9 * mi + 0.1 * g) m gi
    v' = U.zipWith (\vi g -> 0.999 * vi + 0.001 * g * g) v gi

-- ---- DQN エージェント
makeDqn :: R Dqn
makeDqn = do
  net <- newNet
  g <- newGame
  let z = U.replicate nParams 0
  pure (Dqn net net z z 0 Seq.empty 0 g 0 Seq.empty Seq.empty [0, 0, 0] U.empty)

epsAt :: Int -> Double
epsAt t = max 0.1 (1 - fromIntegral t / fromIntegral epsSteps)

-- 1 歩動かす（learn = True なら ε-greedy で動いて学習もする）
dqnStep :: Bool -> Dqn -> R Dqn
dqnStep learn d0 = do
  let x = encode (dGame d0)
      (hid, q) = forward (dNet d0) x
  u <- if learn then rnd else pure 1
  a <- if learn && u < epsAt (dT d0) then (\v -> floor (v * fromIntegral nA)) <$> rnd else pure (argmaxFirst q)
  let (r, g1) = gameStep (dGame d0) a
      done = r /= 0
  d1 <- if done then do
          g2 <- newGame
          let res = if r > 0 then 1 else 0
              l50 = let s = dLast50 d0 Seq.|> res in if Seq.length s > 50 then Seq.drop 1 s else s
          pure d0 { dGame = g2, dGames = dGames d0 + 1, dLast50 = l50, dRate = pushCap 20000 (meanOf (toList l50)) (dRate d0) }
        else pure d0 { dGame = g1 }
  let d2 = d1 { dLastQ = q, dLastH = hid }
  if not learn then pure d2 else do
    let e = Exp (dGame d0) a r g1 done
        buf | Seq.length (dBuf d2) < bufSize = dBuf d2 Seq.|> e
            | otherwise = Seq.update (dT d2 `mod` bufSize) e (dBuf d2)   -- 経験再生のバッファ（古いものから上書き）
        nb = Seq.length buf
    d3 <- if nb >= batchN then do
            picks <- replicateM batchN ((\v -> Seq.index buf (floor (v * fromIntegral nb))) <$> rnd)
            let contrib (Exp s aa rr s2 dd) =
                  let y = if dd then fromIntegral rr else fromIntegral rr + gammaQ * maxOf (snd (forward (dTgt d2) (encode s2)))   -- 目標はターゲットネットワークで
                  in backprop (dNet d2) (encode s) aa y
                gr = U.accum (+) (U.replicate nParams 0) (concatMap contrib picks)
                t' = dOptT d2 + 1
                (p', m', v') = adamStep (dNet d2) gr (dM d2) (dV d2) t' batchN
            pure d2 { dNet = p', dM = m', dV = v', dOptT = t', dBuf = buf }
          else pure d2 { dBuf = buf }
    let t1 = dT d3 + 1
    pure d3 { dT = t1, dTgt = if t1 `mod` targetEvery == 0 then dNet d3 else dTgt d3 }

-- 100 通りの始まり方すべてで、貪欲に動いたときにとれる割合
catchRate :: U.Vector Double -> Double
catchRate net = fromIntegral (length (filter id [ play (Game bx 0 px) | bx <- [0 .. boardN - 1], px <- [0 .. boardN - 1] ])) / fromIntegral (boardN * boardN)
  where play g = let (r, g') = gameStep g (argmaxFirst (snd (forward net (encode g))))
                 in if r == 0 then play g' else r > 0

step :: Env -> Float -> World -> IO World
step env _ w
  | wPaused w = pure w
  | wWatch w || wSpeed w == 0 = pure (if envFrame env `mod` 6 == 0 then one (not (wWatch w)) w else w)
  | wSpeed w == 1 = pure (if envFrame env `mod` 2 == 0 then one True w else w)
  | otherwise = sliced 7 (const True) (one True) w
  where one learn v = let (d, g) = runWith (wRng v) (dqnStep learn (wDqn v)) in v { wDqn = d, wRng = g }

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Tiny neural net Q-learning: Catch   " ++ show (dT d) ++ " steps   eps = " ++ (if wWatch w then "0 (watch only)" else fmtN 2 (epsAt (dT d)))) 40 42 22 (grayC 240)
  , say ("F speed (" ++ idx "" speedNames (wSpeed w) ++ ")   Space pause   R restart   G watch only (" ++ (if wWatch w then "ON" else "OFF") ++ ")") 40 68 15 (rgbC 170 176 210) ]
  -- ---- 盤
  ++ [ Color (rgbC 24 28 58) (rectTL (gx + fromIntegral x * cs) (gy + fromIntegral y * cs) (cs - 2) (cs - 2)) | y <- [0 .. boardN - 1], x <- [0 .. boardN - 1] ]
  ++ [ Color goldC (circleAt (gx + fromIntegral (gBx g) * cs + cs / 2 - 1) (gy + fromIntegral (gBy g) * cs + cs / 2 - 1) (cs * 0.7))
     , Color cyanC (rectTL (gx + fromIntegral (gPx g - 1) * cs) (gy + fromIntegral (boardN - 1) * cs + cs * 0.3) (cs * 3 - 2) (cs * 0.6)) ]
  -- Q の値
  ++ [ say "Q now (left / stay / right)" qx qy 15 (grayC 230) ]
  ++ concat [ [ Color (if v >= 0 then limeC else pinkC) (rectTL (qx + fromIntegral a * 130) (qy + 12) (f2 (clampv (abs v * 80) 0 120)) 16)
              , say (fmtN 2 v) (qx + fromIntegral a * 130) (qy + 48) 15 (grayC 230) ]
            | (a, v) <- zip [0 :: Int ..] (dLastQ d) ]
  -- ---- 隠れ層の活性（16 個）
  ++ [ say "16 hidden units (tanh)" nx (ny - 10) 15 (grayC 230) ]
  ++ [ Color (if v > 0 then withA (f2 (clampv ((60 + 195 * v) / 255) 0 1)) goldC else withA (f2 (clampv ((60 - 195 * v) / 255) 0 1)) violetC)
             (circleAt (nx + 16 + fromIntegral (j `mod` 8) * 34) (ny + 20 + fromIntegral (j `div` 8) * 34) 26)
     | (j, v) <- zip [0 :: Int ..] (U.toList (dLastH d)) ]
  -- ---- 成功率
  ++ [ plotAxes pg "games" "catch rate (last 50 games)" [0, jsRound (x1 pg / 2), x1 pg] [0, 0.3, 0.5, 0.8, 1]
     , Color (withA (120 / 255) limeC) (lineW 1 (pX pg, gY pg 0.8) (pX pg + pW pg, gY pg 0.8))
     , drawCurve pg (dRate d) cyanC 2.5 (fromIntegral (n - Seq.length (dRate d)))
     , say "Random moves catch about 30%. Green line = 80%. While learning, eps makes it miss on purpose" (pX pg) (pY pg + pH pg + 50) 14 (rgbC 170 176 210)
     , drawSelfTest env (wTest w) ]
  where
    d = wDqn w
    g = dGame d
    cs = min 40 ((envH env - 220) / fromIntegral boardN)
    gx = 40
    gy = 100
    qx = gx
    qy = gy + fromIntegral boardN * cs + 30
    nx = gx + fromIntegral boardN * cs + 50
    ny = 120
    n = dGames d
    pg = G (nx + 10) 230 (envW env - nx - 60) (envH env - 360) 0 (fromIntegral (max 100 n)) 0 1

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char ch) Down _ _
    | ch `elem` "fF" -> w { wSpeed = (wSpeed w + 1) `mod` 3 }
    | ch `elem` "rR" -> let (d, g) = runWith (wRng w) makeDqn
                        in w { wDqn = d { dLastQ = dLastQ (wDqn w), dLastH = dLastH (wDqn w) }, wRng = g }
    | ch `elem` "gG" -> w { wWatch = not (wWatch w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("backprop = numerical derivative (all weights, max relative error " ++ fmtExp1 worst ++ " < 1e-5)", worst < 1e-5)
  , ("the untrained net catches little (" ++ show (jsRoundI (100 * max before r0)) ++ "%)", max before r0 < 0.6)
  , ("after 6000 learning steps it catches " ++ show (jsRoundI (100 * r1)) ++ "% of the 100 starts (> 80%)", r1 > 0.8)
  , ("the target network is a copy of the values (same values, kept apart from the learning net)", copyOk)
  , ("no NaN or Infinity in the weights", noNaN (U.toList (dNet dEnd))) ]
  where
    -- 数値微分との照合（中心差分）
    net = withSeed 3 newNet
    x = [3, boardN + 4, 2 * boardN + 6]
    gr = U.accum (+) (U.replicate nParams 0) (backprop net x 1 0.7)
    lossAt nt = 0.5 * (idx 0 (snd (forward nt x)) 1 - 0.7) ^ (2 :: Int)
    worst = foldl' max 0 [ abs (num - an) / (abs num + abs an)
                         | i <- [0 .. nParams - 1]
                         , let o = net U.! i
                               lp = lossAt (net U.// [(i, o + 1e-5)])
                               lm = lossAt (net U.// [(i, o - 1e-5)])
                               num = (lp - lm) / 2e-5
                               an = gr U.! i
                         , abs num + abs an > 1e-9 ]
    before = catchRate net
    (d0, dEnd, mid) = withSeed 4 $ do
      dd <- makeDqn
      let go !t v acc | t >= (6000 :: Int) = pure (v, acc)
                      | otherwise = do v' <- dqnStep True v; go (t + 1) v' (if t + 1 == 50 then Just v' else acc)
      (e, m) <- go 0 dd Nothing
      pure (dd, e, m)
    r0 = catchRate (dNet d0)
    r1 = catchRate (dNet dEnd)
    -- はじめは同じ値。50 歩学習したあと、学習するネットは変わってもターゲットはコピーした時のまま
    copyOk = dTgt d0 == dNet d0 && maybe False (\m -> dTgt m == dNet d0 && dNet m /= dNet d0) mid

-- 1.2e-8 のような書き方（toExponential(1)）
fmtExp1 :: Double -> String
fmtExp1 v
  | v == 0 || isNaN v || isInfinite v = fmtN 1 v ++ "e+0"
  | otherwise = let e0 = floor (logBase 10 (abs v)) :: Int
                    m0 = v / 10 ^^ e0
                    (m, e) = if abs (fromIntegral (round (m0 * 10) :: Int) / 10) >= (10 :: Double) then (m0 / 10, e0 + 1) else (m0, e0)
                in fmtN 1 m ++ "e" ++ (if e >= 0 then "+" else "") ++ show e

-- ---------- やってみよう ----------
-- 1. targetEvery を 1 にすると（ターゲットネットワークなし）、学習はどうなる？
-- 2. bufSize を 16 にすると（ほとんど経験再生なし）？ 直前の経験ばかり学ぶとどうなる？
-- 3. nH を 4 や 64 にしてみよう。小さすぎると覚えられない、大きいと遅い
-- 4. 入力を 100 マスの絵（0/1）にすると、本物の「画面から学ぶ」に近づく（nIn = 100 に）
-- 5. G で見るだけにして、学習したかごの動きを観察しよう。どこで動き出す？

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
