-- ============================================
-- 63-04 モンテカルロと TD 学習 ─ 5 つの状態 A〜E のランダムウォーク（真ん中 C から出発、左右に 1/2 ずつ）
-- 右はしに出たら報酬 1、左はしなら 0。真の価値 = 右に出る確率 = 1/6, 2/6, …, 5/6。
-- モンテカルロ（MC）はエピソードの最後まで待って「実際の結果」に近づける。TD(0) は 1 歩ごとに「次の状態の推定」に近づける
-- 操作：↑↓ TD の学習率 α / ← → MC の学習率 α / F 速さ / スペース やり直し
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.Bits (shiftR, xor, (.|.))
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nEpis, nRuns :: Int
nEpis = 100                            -- ★ 誤差の曲線：100 エピソード ×
nRuns = 100                            -- ★ 100 回の平均

trueV :: [Double]
trueV = [ i / 6 | i <- [1 .. 5] ]

-- 状態の番号：0 = 左はし（終わり）、1〜5 = A〜E、6 = 右はし（終わり）
v0 :: U.Vector Double
v0 = U.fromList [0, 0.5, 0.5, 0.5, 0.5, 0.5, 0]

data World = World
  { wAlphaTD :: !Double                -- ★ TD の学習率
  , wAlphaMC :: !Double                -- ★ MC の学習率
  , wVTD     :: !(U.Vector Double)
  , wVMC     :: !(U.Vector Double)
  , wWalker  :: !Int
  , wPath    :: ![Int]                 -- いまのエピソードの道（新しいものが先頭）
  , wEpisodes :: !Int
  , wFast    :: !Bool
  , wRmsTD   :: !(U.Vector Double)     -- 誤差の曲線（足しこみ中）
  , wRmsMC   :: !(U.Vector Double)
  , wRunsDone :: !Int
  , wLastR   :: !(Maybe Int)
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "63-04 Monte Carlo vs TD" (setup []))
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
setup rs = resetAll (World 0.1 0.03 v0 v0 3 [3] 0 False U.empty U.empty 0 Nothing (seedRng 9) rs)

resetAll :: World -> World
resetAll w = resetCurves w { wVTD = v0, wVMC = v0, wWalker = 3, wPath = [3], wEpisodes = 0 }

resetCurves :: World -> World
resetCurves w = w { wRmsTD = U.replicate (nEpis + 1) 0, wRmsMC = U.replicate (nEpis + 1) 0, wRunsDone = 0 }

rms :: U.Vector Double -> Double
rms v = sqrt (foldl' (\e i -> e + (v U.! i - idx 0 trueV (i - 1)) ^ (2 :: Int)) 0 [1 .. 5] / 5)

walkStep :: Int -> R Int
walkStep s = do u <- rnd; pure (s + (if u < 0.5 then -1 else 1))

-- ★ TD(0)：V(s) ← V(s) + α (r + V(s′) − V(s))。終わりの状態の価値は 0
tdUpdate :: U.Vector Double -> Int -> Int -> Double -> U.Vector Double
tdUpdate v s s2 alpha = v U.// [(s, vs + alpha * (r + v2 - vs))]
  where r = if s2 == 6 then 1 else 0
        v2 = if s2 == 0 || s2 == 6 then 0 else v U.! s2
        vs = v U.! s

-- ★ 初回訪問モンテカルロ：エピソードで最初に来た状態ごとに V(s) ← V(s) + α (G − V(s))（G = 最後の報酬）
--   counts があれば α = 1 / 来た回数（標本平均）
mcUpdate :: U.Vector Double -> [Int] -> Double -> Double -> Maybe (U.Vector Int) -> (U.Vector Double, Maybe (U.Vector Int))
mcUpdate v states g alpha counts = foldl' one (v, counts) (firstVisits states)
  where
    one (vv, Nothing) s = (vv U.// [(s, vv U.! s + alpha * (g - vv U.! s))], Nothing)
    one (vv, Just c) s = let n = c U.! s + 1
                         in (vv U.// [(s, vv U.! s + (g - vv U.! s) / fromIntegral n)], Just (c U.// [(s, n)]))
    firstVisits = reverse . foldl' (\seen s -> if s `elem` seen then seen else s : seen) []

runEpisodeTD :: U.Vector Double -> Double -> R (U.Vector Double)
runEpisodeTD v0' alpha = go 3 v0'
  where go s v = do
          s2 <- walkStep s
          let v' = tdUpdate v s s2 alpha
          if s2 == 0 || s2 == 6 then pure v' else go s2 v'

runEpisodeMC :: U.Vector Double -> Double -> Maybe (U.Vector Int) -> R (U.Vector Double, Maybe (U.Vector Int))
runEpisodeMC v alpha counts = go 3 [3]
  where go s states = do
          s' <- walkStep s
          if s' == 0 || s' == 6 then pure (mcUpdate v (reverse states) (if s' == 6 then 1 else 0) alpha counts)
          else go s' (s' : states)

-- 1 回分の誤差の曲線を足しこむ
curveRun :: U.Vector Double -> Bool -> Double -> Int -> R (U.Vector Double)
curveRun acc isTD alpha runs = do
  let ep v = if isTD then runEpisodeTD v alpha else fst <$> runEpisodeMC v alpha Nothing
      go !e v errs | e > nEpis = pure (reverse errs)
                   | otherwise = do v' <- ep v; go (e + 1) v' (rms v' : errs)
  errs <- go (1 :: Int) v0 [rms v0]
  pure (U.zipWith (\a x -> a + x / fromIntegral runs) acc (U.fromList errs))

-- 画面のエージェントを 1 歩（TD はすぐ更新、MC は最後に）
demoStep :: World -> World
demoStep w
  | s2 == 0 || s2 == 6 = w1 { wVMC = fst (mcUpdate (wVMC w) (reverse (wPath w)) rr (wAlphaMC w) Nothing)
                            , wLastR = Just (if s2 == 6 then 1 else 0), wEpisodes = wEpisodes w + 1, wWalker = 3, wPath = [3] }
  | otherwise = w1 { wWalker = s2, wPath = s2 : wPath w }
  where
    (s2, g) = runWith (wRng w) (walkStep (wWalker w))
    w1 = w { wRng = g, wVTD = tdUpdate (wVTD w) (wWalker w) s2 (wAlphaTD w) }
    rr = if s2 == 6 then 1 else 0

curveStep :: World -> World
curveStep w = w { wRmsTD = a, wRmsMC = b, wRunsDone = wRunsDone w + 1, wRng = g }
  where ((a, b), g) = runWith (wRng w) $ do
          x <- curveRun (wRmsTD w) True (wAlphaTD w) nRuns
          y <- curveRun (wRmsMC w) False (wAlphaMC w) nRuns
          pure (x, y)

-- 時刻 t0 から budget ミリ秒になるまで、cond が成り立つ間 f をくり返す（p5 の while）
loopUntil :: Double -> Double -> (w -> Bool) -> (w -> w) -> w -> IO w
loopUntil t0 budget cond f = go
  where go w = do
          t <- getMonotonicTime
          if cond w && (t - t0) * 1000 < budget then evaluate (f w) >>= go else pure w

step :: Env -> Float -> World -> IO World
step env _ w = do
  t0 <- getMonotonicTime
  w1 <- if wFast w then loopUntil t0 3 (const True) demoStep w
        else pure (if envFrame env `mod` 10 == 0 then demoStep w else w)
  loopUntil t0 6 (\v -> wRunsDone v < nRuns) curveStep w1

stateName :: Int -> String
stateName s = [idx '?' "ABCDE" (s - 1)]

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Monte Carlo and TD(0): random walk   episode " ++ show (wEpisodes w)) 40 42 22 (grayC 240)
  , say ("Up/Down TD alpha = " ++ fmtN 2 (wAlphaTD w) ++ "   <- -> MC alpha = " ++ fmtN 2 (wAlphaMC w) ++ "   F speed (" ++ (if wFast w then "fast" else "normal") ++ ")   Space restart") 40 68 15 (rgbC 170 176 210) ]
  -- ---- 一列の状態
  ++ concat [ let x = cx0 + fromIntegral i * gapX in
              if i == 0 || i == 6
                then [ Color (rgbC 60 66 110) (rectTL (x - 22) (cy - 22) 44 44), sayM (if i == 6 then "+1" else "0") x cy 15 (grayC 230) ]
                else [ Color (rgbC 30 34 70) (circleAt x cy 52), Color (withA (160 / 255) cyanC) (ringAt x cy 52 2)
                     , sayM (stateName i) x cy 18 (grayC 240)
                     , Color (rgbC 90 96 140) (lineW 1 (x + 28, cy) (x + gapX - 28, cy)) ]
            | i <- [0 .. 6 :: Int] ]
  ++ [ Color goldC (circleAt (cx0 + fromIntegral (wWalker w) * gapX) (cy - 42) 20)
     , say ("Path this episode: " ++ pathStr ++ maybe "" (\r -> "   (last result " ++ show r ++ ")") (wLastR w)) 40 (cy + 55) 14 (rgbC 170 176 210) ]
  -- ---- 推定値（画面の 1 回の学習）
  ++ [ plotAxes g "state (A-E)" "value estimate" [1, 2, 3, 4, 5] [0, 0.5, 1]
     , Color limeC (polyline 2 [ (gX g i, gY g v) | (i, v) <- zip [1 ..] trueV ])
     , Color cyanC (polyline 3 [ (gX g (fromIntegral i), gY g (wVTD w U.! i)) | i <- [1 .. 5] ])
     , Color pinkC (polyline 3 [ (gX g (fromIntegral i), gY g (wVMC w U.! i)) | i <- [1 .. 5] ])
     , legend (pX g + 12) (pY g + 16) [(limeC, "true value 1/6 - 5/6"), (cyanC, "TD(0)"), (pinkC, "Monte Carlo")]
     , say ("Error (RMS)   TD " ++ fmtN 3 (rms (wVTD w)) ++ "   MC " ++ fmtN 3 (rms (wVMC w))) (pX g) (pY g + pH g + 50) 14 (grayC 220) ]
  -- ---- 誤差の曲線（100 回の平均）
  ++ [ plotAxes h "episodes" ("RMS error (mean of " ++ show (wRunsDone w) ++ " runs)") [0, 25, 50, 75, 100] [0, 0.05, 0.1, 0.15, 0.2, 0.25] ]
  ++ (if wRunsDone w > 0
        then [ drawCurve h (curveOf (wRmsTD w)) cyanC 2.5 0, drawCurve h (curveOf (wRmsMC w)) pinkC 2.5 0 ] else [])
  ++ [ legend (pX h + pW h - 200) (pY h + 16) [(cyanC, "TD(0) alpha = " ++ fmtN 2 (wAlphaTD w)), (pinkC, "MC alpha = " ++ fmtN 2 (wAlphaMC w))]
     , say "Large alpha drops fast but keeps wobbling and levels off." (pX h) (pY h + pH h + 50) 14 (rgbC 170 176 210)
     , say "TD updates a guess from a guess, so it does well with little data" (pX h) (pY h + pH h + 70) 14 (rgbC 170 176 210)
     , drawSelfTest env (wTest w) ]
  where
    cx0 = 120
    gapX = min 130 ((envW env - 240) / 6)
    cy = 140
    pathAll = map stateName (reverse (wPath w))
    pathStr = let full = joinA pathAll
              in if length full > 70 then "... " ++ joinA (dropWhileLong pathAll) else full
    joinA = foldr1' (\a b -> a ++ " -> " ++ b)
    foldr1' f xs = if null xs then "" else foldr1 f xs
    dropWhileLong xs = if length (joinA xs) > 66 then dropWhileLong (drop 1 xs) else xs
    g = G 70 250 (envW env * 0.38) (envH env - 360) 0.5 5.5 0 1
    h = G (envW env * 0.53) 250 (envW env * 0.42) (envH env - 360) 0 (fromIntegral nEpis) 0 0.25
    curveOf v = Seq.fromList (map (\x -> x * fromIntegral nRuns / fromIntegral (wRunsDone w)) (U.toList v))

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> setA (min 0.5 (jsRound ((wAlphaTD w + 0.01) * 100) / 100)) (wAlphaMC w)
  EventKey (SpecialKey KeyDown) Down _ _ -> setA (max 0.01 (jsRound ((wAlphaTD w - 0.01) * 100) / 100)) (wAlphaMC w)
  EventKey (SpecialKey KeyRight) Down _ _ -> setA (wAlphaTD w) (min 0.5 (jsRound ((wAlphaMC w + 0.01) * 100) / 100))
  EventKey (SpecialKey KeyLeft) Down _ _ -> setA (wAlphaTD w) (max 0.01 (jsRound ((wAlphaMC w - 0.01) * 100) / 100))
  EventKey (Char ch) Down _ _ | ch `elem` "fF" -> w { wFast = not (wFast w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> resetAll w
  _ -> w
  where setA a b = let w' = w { wAlphaTD = a, wAlphaMC = b }
                   in if a /= wAlphaTD w || b /= wAlphaMC w then resetCurves w' else w'   -- α が変わったら曲線をやり直す

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ種）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Monte Carlo (sample mean) 3000 episodes: RMS " ++ fmtN 4 (rms vMC) ++ " < 0.05", rms vMC < 0.05)
  , ("TD(0) (alpha = 0.01) 3000 episodes: RMS " ++ fmtN 4 (rms vTD) ++ " < 0.05", rms vTD < 0.05)
  , ("at the true values i/6 the expected TD update is exactly 0 (solves the Bellman equation)", okFix)
  , ("mean of 100 runs, error over episodes 50-100: TD(alpha = 0.05) " ++ fmtN 3 errTD ++ " < best MC alpha " ++ fmtN 3 bestMC, errTD < bestMC)
  , ("error at episode 100 is smaller than at episode 1 (TD)", a U.! 100 < a U.! 1 && noNaN (U.toList a)) ]
  where
    vMC = withSeed 1 (loopN 3000 (v0, Just (U.replicate 7 0)) (\(v, c) -> runEpisodeMC v 0 c)) `seq1` fst
    vTD = withSeed 1 (loopN 3000 v0 (\v -> runEpisodeTD v 0.01))
    seq1 x f = f x
    tt = [0] ++ trueV ++ [0]
    okFix = and [ near (0.5 * ((if s - 1 == 0 then 0 else idx 0 tt (s - 1)) - idx 0 tt s) + 0.5 * ((if s + 1 == 6 then 1 else idx 0 tt (s + 1)) - idx 0 tt s)) 0 1e-12 | s <- [1 .. 5] ]
    (a, bestMC) = withSeed 2 $ do
      aa <- loopN 100 (U.replicate (nEpis + 1) 0) (\acc -> curveRun acc True 0.05 100)
      bs <- forM [0.01, 0.02, 0.03, 0.04] $ \al -> do
        b <- loopN 100 (U.replicate (nEpis + 1) 0) (\acc -> curveRun acc False al 100)
        pure (meanOf (drop 50 (U.toList b)))
      pure (aa, foldl' min posInf bs)
    errTD = meanOf (drop 50 (U.toList a))

-- f を n 回くり返す
loopN :: Int -> a -> (a -> R a) -> R a
loopN n x f = if n <= 0 then pure x else f x >>= \y -> loopN (n - 1) y f

-- ---------- やってみよう ----------
-- 1. TD の α を 0.15 → 0.05 にすると、曲線はどう変わる？（速さと最後の誤差のかけひき）
-- 2. MC の α をいろいろ変えて、TD より良くなる α はある？
-- 3. F で速くして、画面の推定値（左のグラフ）が真の値 1/6〜5/6 にどう近づくか見よう
-- 4. 状態を 19 個にふやすと？（trueV も変える）
-- 5. α を 1/（その状態に来た回数）にすると、MC はただの平均になる。これが標本平均

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
