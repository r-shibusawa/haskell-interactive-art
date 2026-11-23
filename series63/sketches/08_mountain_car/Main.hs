-- ============================================
-- 63-08 山登り車とタイルコーディング ─ 力の弱い車は、そのままでは右の坂を登れない
-- 反対側の坂に登って勢いをつける必要がある。状態は「位置 x と速度 v」の連続値なので、表には書けない。
-- そこで 8 枚の少しずつずらした格子（タイリング）で状態を表し（タイルコーディング）、Q(s,a) = 当たったタイルの重みの合計 で近似。
-- 1 歩ごとに −1 の報酬、SARSA で重みを更新。右上の地形 = 学習した「ゴールまでのコスト」−max_a Q
-- 操作：F 速さ / スペース 一時停止 / R やり直し / M 人が運転（← → で押す）
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
import Data.Word (Word32)
import qualified Data.Word as W
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nTilings, nGrid :: Int
nTilings = 8                             -- ★ タイリングの枚数
nGrid = 8                                -- ★ 1 枚の格子の細かさ（8 × 8）

alphaT :: Double
alphaT = 0.5 / fromIntegral nTilings     -- ★ 学習率（タイル 1 枚あたり）

maxLen :: Int
maxLen = 5000                            -- ★ 1 エピソードの上限

per :: Int
per = (nGrid + 1) * (nGrid + 1)          -- ずらすので 1 枚あたり 9 × 9 のタイル

-- 軌跡の 1 点（位置, 速度）
data P2 = P2 !Double !Double

-- 学習する車
data Car = Car
  { cW      :: !(U.Vector Double)        -- 重み（タイル × 3 行動）
  , cX, cV  :: !Double
  , cA      :: !Int                      -- 次に選ぶ行動
  , cT      :: !(U.Vector Int)           -- いまの状態のタイル
  , cLen    :: !Int
  , cEpLens :: !(Seq.Seq Double)         -- エピソードごとの歩数
  , cTraj   :: ![P2] }                   -- いまのエピソードの軌跡（新しい順）

data World = World
  { wCar     :: !Car
  , wSpeed   :: !Int                     -- 0 ゆっくり、1 ふつう、2 速い
  , wPaused  :: !Bool
  , wManual  :: !Bool
  , wPush    :: !Int
  , wCost    :: !(U.Vector Double)       -- 40 × 40 のコスト −max_a Q
  , wCostMax :: !Double
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

speedNames :: [String]
speedNames = ["slow", "normal", "fast"]

main :: IO ()
main = runSketch (sketch "63-08 Mountain car" (setup []))
  { skBack  = bgC
  , skStart = \_ -> do
      let rs = selfTest
      printSelfTest rs
      pure (setup rs)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- seedRng(8) してから車を作る
setup :: [(String, Bool)] -> World
setup rs = updateCost (World c 1 False False 1 U.empty 1 g rs)
  where (c, g) = runWith (seedRng 8) makeCar

-- ---- 山登り車の物理（よく使われる決まった式）：v += 0.001·a − 0.0025·cos(3x)、x += v
carPhysics :: Double -> Double -> Int -> (Double, Double)   -- a = 0 左に押す、1 押さない、2 右に押す
carPhysics x v a =
  let v1 = clampv (v + 0.001 * fromIntegral (a - 1) - 0.0025 * cos (3 * x)) (-0.07) 0.07
      x1 = x + v1
  in if x1 < -1.2 then (-1.2, 0)          -- 左の壁にぶつかったら止まる
     else if x1 > 0.5 then (0.5, v1) else (x1, v1)

-- ★ タイルコーディング：8 枚の格子それぞれで (x, v) が入るタイルの番号（× 3 行動ぶんの場所の先頭）
tilesOf :: Double -> Double -> U.Vector Int
tilesOf x v = U.generate nTilings tile
  where
    xs = (x + 1.2) / 1.7 * fromIntegral nGrid
    vs = (v + 0.07) / 0.14 * fromIntegral nGrid
    tile t =
      let ix = clampI (floorI (xs + fromIntegral t / fromIntegral nTilings))                       -- x は 1、v は 3 ずつずらす
          iv = clampI (floorI (vs + fromIntegral ((3 * t) `mod` nTilings) / fromIntegral nTilings))
      in (t * per + ix * (nGrid + 1) + iv) * 3
    clampI i = max 0 (min nGrid i)

qOf :: U.Vector Double -> U.Vector Int -> Int -> Double
qOf w tt a = U.foldl' (\s i -> s + w U.! (i + a)) 0 tt

greedyAct :: U.Vector Double -> U.Vector Int -> R Int
greedyAct w tt = argmaxR [ qOf w tt a | a <- [0 .. 2] ]

makeCar :: R Car
makeCar = startEpisode (Car (U.replicate (nTilings * per * 3) 0) 0 0 0 U.empty 0 Seq.empty [])

startEpisode :: Car -> R Car
startEpisode c = do
  u <- rnd
  let x = -0.6 + 0.2 * u
      tt = tilesOf x 0
  a <- greedyAct (cW c) tt
  pure c { cX = x, cV = 0, cLen = 0, cTraj = [], cT = tt, cA = a }

-- ★ SARSA の 1 歩（Q の初期値 0 は −1 の報酬にくらべて楽観的なので、ε = 0 でも探索する）
carStep :: Car -> R Car
carStep c = do
  let (x2, v2) = carPhysics (cX c) (cV c) (cA c)
      r = -1
      goal = x2 >= 0.5
      len = cLen c + 1
      traj = if len `mod` 4 == 0 then take 2000 (P2 x2 v2 : cTraj c) else cTraj c
      t2 = tilesOf x2 v2
  a2 <- if goal then pure 0 else greedyAct (cW c) t2
  let target = if goal then r else r + qOf (cW c) t2 a2       -- ゴールに着いたら先の価値はない
      d = target - qOf (cW c) (cT c) (cA c)
      w' = U.accum (+) (cW c) [ (i + cA c, alphaT * d) | i <- U.toList (cT c) ]   -- 当たったタイルの重みだけを動かす
      c' = c { cW = w', cLen = len, cTraj = traj }
  if goal || len >= maxLen
    then startEpisode c' { cEpLens = pushCap 20000 (fromIntegral len) (cEpLens c') }
    else pure c' { cX = x2, cV = v2, cA = a2, cT = t2 }

-- 40 × 40 の点で −max_a Q
updateCost :: World -> World
updateCost w = w { wCost = vals, wCostMax = max 1 (U.maximum (U.cons negInf vals)) }
  where
    wt = cW (wCar w)
    vals = U.generate 1600 (\k -> let i = k `mod` 40; j = k `div` 40
                                      tt = tilesOf (-1.2 + 1.7 * (fromIntegral i + 0.5) / 40) (0.07 - 0.14 * (fromIntegral j + 0.5) / 40)
                                  in negate (maxOf [ qOf wt tt a | a <- [0 .. 2] ]))

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let fr = envFrame env
  w1 <- if wManual w0
          then pure (if fr `mod` 2 == 0 then manualStep w0 else w0)
          else if wPaused w0 then pure w0
          else case wSpeed w0 of
            0 -> pure (learn 1 w0)
            1 -> pure (learn 8 w0)
            _ -> sliced 6 (const True) (learn 1) w0
  pure (if fr `mod` 30 == 1 then updateCost w1 else w1)
  where
    learn :: Int -> World -> World
    learn n w = let (c, g) = runWith (wRng w) (foldM (\cc _ -> carStep cc) (wCar w) [1 .. n]) in w { wCar = c, wRng = g }
    manualStep w =
      let c = wCar w
          (x, v) = carPhysics (cX c) (cV c) (wPush w)
          c1 = c { cX = x, cV = v, cTraj = take 2000 (P2 x v : cTraj c) }
      in w { wCar = if x >= 0.5 then c1 { cX = -0.5, cV = 0, cTraj = [] } else c1 }

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Mountain car: tile coding + SARSA   episode " ++ show (Seq.length (cEpLens c)) ++ (if wManual w then " (you are driving)" else "")) 40 42 22 (grayC 240)
  , say ("F speed (" ++ idx "" speedNames (wSpeed w) ++ ")   Space pause   R restart   M drive yourself (<- -> push)") 40 68 15 (rgbC 170 176 210)
  -- ---- 坂と車
  , Color (rgbC 110 120 180) (polyline 3 [ (toX x, toY x) | i <- [0 .. 100 :: Int], let x = -1.2 + 1.7 * fromIntegral i / 100 ])
  , Color goldC (lineW 2 (toX 0.5, toY 0.5) (toX 0.5, toY 0.5 - 40))
  , Color goldC (Polygon [(toX 0.5, toY 0.5 - 40), (toX 0.5 - 22, toY 0.5 - 33), (toX 0.5, toY 0.5 - 26)])
  , Translate cx cy (Rotate (negate (ang * 180 / pi)) (Pictures
      [ Color cyanC (rectTL (-22) (-24) 44 18), Color (grayC 240) (circleAt (-12) (-5) 11), Color (grayC 240) (circleAt 12 (-5) 11) ]))
  , sayC (idx "" ["<- push left", "no push", "push right ->"] act) cx (cy - 40) 15 (grayC 230)
  , say ("position x = " ++ fmtN 3 (cX c) ++ "   velocity v = " ++ fmtN 4 (cV c) ++ "   this episode " ++ show (cLen c) ++ " steps") hx (hy + hh + 20) 15 (rgbC 200 205 235)
  -- ---- コストの地形（位置 × 速度）
  , Translate (ix + iw / 2) (iy + ih / 2) (Scale (iw / 40) (negate (ih / 40)) (pixelImage 40 40 1 costPx))
  , Color (rgbC 80 86 130) (frameTL 1 ix iy iw ih)
  , Color (rgbaC 255 255 255 220) (polyline 1.5 [ (ix + f2 ((x + 1.2) / 1.7) * iw, iy + f2 ((0.07 - v) / 0.14) * ih) | P2 x v <- cTraj c ])
  , say "position x -> (-1.2 to 0.5)" ix (iy + ih + 18) 14 (rgbC 200 205 235)
  , say "^ velocity v (-0.07 to 0.07)" ix (iy - 8) 14 (rgbC 200 205 235)
  , say ("bright = far from goal (max cost " ++ show (jsRoundI (wCostMax w)) ++ " steps). White = this episode") ix (iy + ih + 38) 14 (rgbC 200 205 235)
  -- ---- エピソードごとの歩数
  , plotAxes g "episode" "log10 (steps to goal)" [0, jsRound (x1 g / 2), x1 g] [2, 3]
  , Color (withA (120 / 255) limeC) (lineW 1 (pX g, gY g (logBase 10 200)) (pX g + pW g, gY g (logBase 10 200)))
  , drawCurve g (fmap (logBase 10) (cEpLens c)) cyanC 2 0
  , say "Green line = 200 steps. First episodes take thousands of steps; after some dozens it is 100-200" (pX g + 10) (pY g + 18) 14 (rgbC 170 176 210)
  , drawSelfTest env (wTest w) ]
  where
    c = wCar w
    hx = 40
    hy = 110
    hw = envW env * 0.5
    hh = envH env * 0.42
    toX x = hx + f2 ((x + 1.2) / 1.7) * hw
    toY x = hy + hh * 0.5 - f2 (sin (3 * x)) * hh * 0.42
    cx = toX (cX c)
    cy = toY (cX c)
    ang = atan (negate (f2 (cos (3 * cX c))) * 3 * hh * 0.42 / (hw / 1.7))
    act = if wManual w then wPush w else cA c
    ix = envW env * 0.58
    iy = 110
    iw = min (envW env * 0.38) (hh * 1.25)
    ih = hh
    costPx i j = let t = clampv (idxU (wCost w) (j * 40 + i) / wCostMax w) 0 1
                 in (byteOf (30 + 220 * t), byteOf (40 + 150 * t * t), byteOf (120 - 80 * t))
    n = Seq.length (cEpLens c)
    gy0 = hy + hh + 80
    g = G 70 gy0 (envW env - 140) (envH env - gy0 - 70) 0 (fromIntegral (max 50 n)) 2 (logBase 10 (fromIntegral maxLen))

idxU :: U.Vector Double -> Int -> Double
idxU v i = if i >= 0 && i < U.length v then v U.! i else 0

byteOf :: Double -> W.Word8
byteOf v = fromIntegral (max 0 (min 255 (floorI v)))

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char ch) Down _ _
    | ch `elem` "fF" -> w { wSpeed = (wSpeed w + 1) `mod` 3 }
    | ch `elem` "rR" -> let (c, g) = runWith (wRng w) makeCar in w { wCar = c, wRng = g }
    | ch `elem` "mM" -> let (c, g) = runWith (wRng w) (startEpisode (wCar w)) in w { wManual = not (wManual w), wCar = c, wRng = g }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wPush = 0 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wPush = 2 }
  EventKey _ Up _ _ -> w { wPush = 1 }          -- どのキーを離しても「押さない」に
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("physics as the formula: from x = -0.5, v = 0 pushing right gives v = 0.001 - 0.0025 cos(-1.5); stays in range"
    , okB && near v1 (0.001 - 0.0025 * cos (-1.5)) 1e-15 && near x1' (-0.5 + v1) 1e-15)
  , ("each of the 8 tilings has one tile. Near states share many tiles (" ++ show (share ta tb) ++ "), far states none (" ++ show (share ta tc) ++ ")"
    , length (uniq (U.toList ta)) == 8 && share ta tb >= 6 && share ta tc == 0)
  , ("under 200 steps to the goal within 100 episodes (episode " ++ show firstOK ++ "), mean of last 10 = " ++ fmtN 0 last10 ++ " steps < 200"
    , firstOK > 0 && firstOK <= 40 && last10 < 200)
  , ("learned greedy policy: " ++ show ok10 ++ " of 10 starts reach the goal in under 200 steps", ok10 >= 9)
  , ("weights are finite (no NaN)", noNaN (U.toList (cW trained))) ]
  where
    okB = withSeed 1 (fmap and (replicateM 2000 (do
            u1 <- rnd; u2 <- rnd; a <- rndInt 0 2
            let (x, v) = carPhysics (-1.2 + 1.7 * u1) (-0.07 + 0.14 * u2) a
            pure (not (x < -1.2 || x > 0.5 || v < -0.07 || v > 0.07 || isNaN (x + v) || isInfinite (x + v))))))
    (x1', v1) = carPhysics (-0.5) 0 2
    ta = tilesOf (-0.3) 0.01
    tb = tilesOf (-0.29) 0.01
    tc = tilesOf 0.4 (-0.06)
    share p q = length (filter id (zipWith (==) (U.toList p) (U.toList q)))
    uniq = foldr (\v acc -> if v `elem` acc then acc else v : acc) []
    (trained, firstOK) = withSeed 3 (makeCar >>= train (-1))
    train :: Int -> Car -> R (Car, Int)
    train fo c
      | Seq.length (cEpLens c) >= 100 = pure (c, fo)
      | otherwise = do
          c' <- carStep c
          let n = Seq.length (cEpLens c')
              fo' = if fo < 0 && n > 0 && Seq.index (cEpLens c') (n - 1) < 200 then n else fo
          fo' `seq` train fo' c'
    last10 = meanOf (toList (Seq.drop (Seq.length (cEpLens trained) - 10) (cEpLens trained)))
    ok10 = length (filter (< 200) [ run (-0.6 + 0.2 * fromIntegral k / 9) 0 0 | k <- [0 .. 9 :: Int] ])
    run :: Double -> Double -> Int -> Int
    run x v t | t >= 500 || x >= 0.5 = t
              | otherwise = let (x', v') = carPhysics x v (argmaxFirst [ qOf (cW trained) (tilesOf x v) a | a <- [0 .. 2] ]) in run x' v' (t + 1)

-- ---------- やってみよう ----------
-- 1. M で自分で運転してみよう。何歩でゴールできる？ どう押すのがよい？
-- 2. nTilings を 1 にすると（ずらさない 1 枚の格子）学習はどう変わる？
-- 3. nGrid を 4 や 16 にすると？（粗いと区別できない、細かいと覚えることが多い）
-- 4. コストの地形のうずまきの形は、何を表している？（勢いをつけてから登る道）
-- 5. 同じ「棒を立てる・車を動かす」問題を、人が設計した制御で解くのがシリーズ 20・50。ここでは設計せず学ぶ

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
