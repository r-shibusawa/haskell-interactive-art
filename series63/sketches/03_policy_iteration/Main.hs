-- ============================================
-- 63-03 方策反復 ─ 「方策評価（いまの方策の価値を計算）」→「方策改善（価値を見て貪欲に向きを変える）」のくり返し
-- 方策を改善すると価値は下がらない（方策改善定理）。方策の種類は有限なので、数回で最適方策にたどり着いて止まる。
-- 価値反復（02）と、くり返しの回数・計算量をくらべる
-- 操作：スペース 1 段階すすめる / A 自動 / R 方策をでたらめに / U 全部「上」 / ↑↓ γ / ← → すべり / クリック 床→壁→宝→穴
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad.ST (runST)
import Control.Monad (foldM, forM, replicateM)
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

mapRows :: [String]
mapRows =                              -- ★ 地図（02 と同じ）
  [ ".........."
  , ".....#...."
  , ".........."
  , "S........G"
  , ".PPPPPPPP."
  , ".........." ]

stepR, tol :: Double
stepR = -0.04                          -- ★ 1 歩の報酬
tol = 1e-8                             -- ★ 方策評価の収束の判定

data Phase = PEval | PImprove | PDone deriving (Eq)

-- 改善の記録：(評価の回数の累計, 向きが変わったマスの数, V(S))
data LogRow = LogRow !Int !Int !Double

data World = World
  { wGW      :: !GW
  , wGamma   :: !Double                -- ★ 割引率
  , wSlip    :: !Double                -- ★ すべりやすさ
  , wMdp     :: !Mdp
  , wPolicy  :: !(U.Vector Int)        -- 方策 π(s)（向き 0〜3）
  , wV       :: !(U.Vector Double)
  , wPhase   :: !Phase
  , wLog     :: ![LogRow]
  , wAuto    :: !Bool
  , wSweeps  :: !Int
  , wVI      :: !(U.Vector Double, Int, [Double])   -- くらべる相手：価値反復
  , wChanged :: ![Int]                 -- 直前の改善で向きが変わったマス
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "63-03 Policy iteration" (setup ([], seedRng 1)))
  { skBack  = bgC
  , skStart = \_ -> do
      let st = selfTest
      printSelfTest (fst st)
      pure (setup st)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- p5 は setup で種をまき直さないので、乱数は自己テストのあとの状態から続く
setup :: ([(String, Bool)], Rng) -> World
setup (rs, g) = rebuild (World gw 0.95 0.2 (buildMdp gw 0.2 stepR) U.empty U.empty PEval [] True 0 (U.empty, 0, []) [] g rs)
  where gw = gridParse mapRows

rebuild :: World -> World
rebuild w = resetPolicy False w { wMdp = m, wVI = valueIteration m (wGamma w) tol 20000 }
  where m = buildMdp (wGW w) (wSlip w) stepR

resetPolicy :: Bool -> World -> World
resetPolicy randomize w = w { wPolicy = U.fromList pol, wRng = g, wV = U.replicate n 0, wPhase = PEval, wLog = [], wSweeps = 0, wChanged = [] }
  where n = mdpNS (wMdp w)
        (pol, g) = if randomize then runWith (wRng w) (replicateM n (do u <- rnd; pure (floor (u * 4))))
                   else (replicate n 0, wRng w)

-- ★ 方策評価の 1 回：V(s) ← Σ_s' P(s′|s,π(s)) (r + γ V(s′))（行動は方策が決めた 1 つだけ、その場で書きかえる）。(新しい V, 最大の変化量)
evalSweep :: Mdp -> U.Vector Int -> U.Vector Double -> Double -> (U.Vector Double, Double)
evalSweep m pol v0 gam = runST $ do
  v <- U.thaw v0
  d <- foldM (\d s -> if isTerm m s then pure d else do
                 x <- foldM (\q o -> do y <- UM.read v (trS2 o); pure (q + trP o * (trR o + gam * y))) 0 (trans m s (pol U.! s))
                 old <- UM.read v s
                 UM.write v s x
                 pure (max d (abs (x - old)))) 0 [0 .. mdpNS m - 1]
  v' <- U.freeze v
  pure (v', d)

-- ★ 方策改善：各マスで Q(s, a) がいちばん大きい向きへ。同点ならいまの向きのまま（行ったり来たりを防ぐ）。(新しい方策, 変わったマス)
improve :: Mdp -> U.Vector Int -> U.Vector Double -> Double -> (U.Vector Int, [Int])
improve m pol v gam = (pol U.// [ (s, a) | (s, a) <- ch ], map fst ch)
  where
    ch = [ (s, best) | s <- [0 .. mdpNS m - 1], not (isTerm m s)
                     , let q = [ qValue m v gam s a | a <- [0 .. 3] ], let best = argmaxFirst q
                     , idx 0 q best > idx 0 q (pol U.! s) + 1e-12 ]

-- 全部まとめて（自己テスト用）。(方策, V, 評価の回数, 改善の回数, 改善ごとの V)
policyIteration :: Mdp -> U.Vector Int -> Double -> (U.Vector Int, U.Vector Double, Int, Int, [U.Vector Double])
policyIteration m pi0 gam = go 0 pi0 (U.replicate (mdpNS m) 0) 0 []
  where
    go rounds pol v sweeps vs
      | rounds >= 100 = (pol, v, sweeps, rounds + 1, reverse vs)
      | otherwise =
          let (v', sweeps') = evalLoop pol v sweeps
              (pol', ch) = improve m pol v' gam
          in if null ch then (pol', v', sweeps', rounds + 1, reverse (v' : vs)) else go (rounds + 1) pol' v' sweeps' (v' : vs)
    evalLoop pol v !sw = let (v', d) = evalSweep m pol v gam
                         in if d >= tol && sw + 1 < 1000000 then evalLoop pol v' (sw + 1) else (v', sw + 1)

startOf :: GW -> Int
startOf gw = maybe 0 id (U.elemIndex 'S' (gwCell gw))

-- 評価を 1 回すすめる／評価が終わっていたら改善
advance :: World -> World
advance w = case wPhase w of
  PDone -> w
  PEval -> let (v', d) = evalSweep (wMdp w) (wPolicy w) (wV w) (wGamma w)
           in w { wV = v', wSweeps = wSweeps w + 1, wPhase = if d < tol then PImprove else PEval }
  PImprove -> let (pol', ch) = improve (wMdp w) (wPolicy w) (wV w) (wGamma w)
              in w { wPolicy = pol', wChanged = ch, wLog = wLog w ++ [LogRow (wSweeps w) (length ch) (wV w U.! startOf (wGW w))]
                   , wPhase = if null ch then PDone else PEval }

step :: Env -> Float -> World -> IO World
step env _ w
  | not (wAuto w) = pure w
  | wPhase w == PEval = pure (foldl' (\v _ -> if wPhase v == PEval then advance v else v) w [1 .. 4 :: Int])
  | envFrame env `mod` 45 == 0 = pure (advance w)
  | otherwise = pure w

gx0, gy0 :: Float
gx0 = 40
gy0 = 110

cellSize :: Env -> GW -> Float
cellSize env gw = minimum [64, (envW env * 0.55 - 60) / fromIntegral (gwW gw), (envH env - 230) / fromIntegral (gwH gw)]

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Policy iteration: improvement " ++ show (length (wLog w)) ++ "   " ++ phaseLabel) 40 42 22 (grayC 240)
  , say ("Space one stage   A auto (" ++ (if wAuto w then "ON" else "OFF") ++ ")   R random policy   U all 'up'   Up/Down gamma = " ++ fmtN 2 (wGamma w) ++ "   <- -> slip = " ++ fmtN 1 (wSlip w)) 40 68 15 (rgbC 170 176 210) ]
  ++ concat [ let (x, y) = cellXY gw s gx0 gy0 cs
              in Color (valueFill (wV w U.! s) (-1) 1) (rectTL (x + 1) (y + 1) (cs - 2) (cs - 2))
                 : [ Color pinkC (frameTL 2 (x + 3) (y + 3) (cs - 6) (cs - 6)) | s `elem` wChanged w ]
            | s <- [0 .. mdpNS m - 1], not (isTerm m s) ]
  ++ [ drawCells gw gx0 gy0 cs True ]
  ++ concat [ let (x, y) = cellXY gw s gx0 gy0 cs
              in [ sayC (fmtN 2 (wV w U.! s)) (x + cs / 2) (y + 17) 14 (grayC 235)
                 , Color white (drawArrowIn (x + cs / 2) (y + cs * 0.6) (wPolicy w U.! s) (cs * 0.22) 2) ]
            | s <- [0 .. mdpNS m - 1], not (isTerm m s) ]
  ++ [ sayWrap "Arrow = current policy pi(s) (one per cell). Pink frame = cells whose arrow changed in the last improvement" gx0 (gy0 + fromIntegral (gwH gw) * cs + 6) (fromIntegral (gwW gw) * cs) 14 (rgbC 150 156 190) ]
  -- ---- 記録の表と、価値反復との比較
  ++ [ say h (px + dx) py 16 (grayC 230) | (h, dx) <- [("#", 0), ("eval sweeps", 60), ("changed", 200), ("V(S)", 340)] ]
  ++ [ Color c (Pictures [ say (show k) px yy 16 c, say (show sw) (px + 60) yy 16 c, say (show ch) (px + 200) yy 16 c, say (fmtN 4 vS) (px + 340) yy 16 c ])
     | (i, LogRow sw ch vS) <- zip [0 :: Int ..] shown
     , let k = length (wLog w) - length shown + i + 1, let yy = py + 26 + fromIntegral i * 22
     , let c = if ch > 0 then grayC 210 else goldC ]
  ++ [ say ("Policy iter: " ++ show (length (wLog w)) ++ " improvements, " ++ show (wSweeps w) ++ " sweeps (~" ++ show actsPI ++ " Q evals)") px ty 15 cyanC
     , say ("Value iter (02): " ++ show viIts ++ " sweeps (~" ++ show actsVI ++ " Q evals)") px (ty + 26) 15 goldC
     , say "Policy iteration needs very few improvements," px (ty + 58) 14 (rgbC 170 176 210)
     , say "but each evaluation takes dozens of sweeps" px (ty + 78) 14 (rgbC 170 176 210) ]
  ++ [ say ("Same as value iteration's best: " ++ show same ++ " / " ++ show tot ++ " cells") px (ty + 112) 16 limeC | wPhase w == PDone ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    gw = wGW w
    m = wMdp w
    cs = cellSize env gw
    phaseLabel = case wPhase w of
      PEval -> "evaluating the policy"
      PImprove -> "evaluation converged -> improve next"
      PDone -> "policy did not change -> optimal policy"
    px = gx0 + fromIntegral (gwW gw) * cs + 50
    py = 120
    shown = let l = wLog w in drop (length l - 10) l
    ty = py + 270
    (viV, viIts, _) = wVI w
    actsPI = wSweeps w * mdpNS m + length (wLog w) * mdpNS m * 4
    actsVI = viIts * mdpNS m * 4
    nonTerm = [ s | s <- [0 .. mdpNS m - 1], not (isTerm m s) ]
    tot = length nonTerm
    same = length [ s | s <- nonTerm, (wPolicy w U.! s) `elem` optimalActs m viV (wGamma w) s 1e-6 ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let gw = wGW w
        (mx, my) = toP5 env pos
        s = cellAt gw gx0 gy0 (cellSize env gw) mx my
        order = ".#GP"
        c = cellOf gw s
        next = idx '.' order ((maybe (-1) id (lookup c (zip order [0 ..])) + 1) `mod` 4)
    in if s < 0 || c == 'S' then w else rebuild w { wGW = gw { gwCell = gwCell gw U.// [(s, next)] } }
  EventKey (SpecialKey KeySpace) Down _ _
    | wPhase w == PEval -> until (\v -> wPhase v /= PEval) advance w
    | otherwise -> advance w
  EventKey (Char ch) Down _ _
    | ch `elem` "aA" -> w { wAuto = not (wAuto w) }
    | ch `elem` "rR" -> resetPolicy True w
    | ch `elem` "uU" -> resetPolicy False w
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild w { wGamma = min 0.99 (jsRound ((wGamma w + 0.05) * 100) / 100) }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild w { wGamma = max 0.05 (jsRound ((wGamma w - 0.05) * 100) / 100) }
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild w { wSlip = min 0.8 (jsRound ((wSlip w + 0.1) * 10) / 10) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild w { wSlip = max 0 (jsRound ((wSlip w - 0.1) * 10) / 10) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ種）。乱数の最後の状態も返す（p5 はそこから続く）
selfTest :: ([(String, Bool)], Rng)
selfTest =
  ( [ ("policy iteration stops within 15 improvements (at most " ++ show maxRounds ++ ", slip 0 and 0.2, 4 initial policies)", all (\(r, _, _, _) -> r <= 15) results)
    , ("final values = value iteration's V* (within 1e-6)", all (\(_, a, _, _) -> a) results)
    , ("final policy picks one of value iteration's best arrows in every cell", all (\(_, _, a, _) -> a) results)
    , ("policy improvement theorem: no cell's value goes down after an improvement (up to 1e-6 evaluation cutoff)", all (\(_, _, _, a) -> a) results)
    , ("improving the final policy again changes nothing", null (snd (improve m2 pol2 v2 0.95))) ]
  , gEnd )
  where
    gwT = gridParse mapRows
    perSlip sl seed = runWith (seedRng seed) $ do
      let m = buildMdp gwT sl stepR
          (viV, _, _) = valueIteration m 0.95 1e-12 20000
      forM [0 .. 3 :: Int] $ \trial -> do
        pi0 <- if trial == 0 then pure (U.replicate (mdpNS m) 0) else U.fromList <$> replicateM (mdpNS m) (do u <- rnd; pure (floor (u * 4)))
        let (pol, v, _, rounds, vs) = policyIteration m pi0 0.95
            nt = [ s | s <- [0 .. mdpNS m - 1], not (isTerm m s) ]
            okV = and [ near (v U.! s) (viV U.! s) 1e-6 | s <- nt ]
            okPi = and [ (pol U.! s) `elem` optimalActs m viV 0.95 s 1e-6 | s <- nt ]
            okMono = and [ U.and (U.zipWith (\b a -> b >= a - 1e-6) vb va) | (va, vb) <- zip vs (drop 1 vs) ]
        pure (rounds, okV, okPi, okMono)
    (r0, _) = perSlip 0 21
    (r1, gEnd) = perSlip 0.2 23                 -- seedRng(21 + sl × 10)
    results = r0 ++ r1
    maxRounds = maximum (0 : [ r | (r, _, _, _) <- results ])
    -- 改善しても変わらない = 貪欲方策が自分自身
    m2 = buildMdp gwT 0.2 stepR
    (pol2, v2, _, _, _) = policyIteration m2 (U.replicate (mdpNS m2) 0) 0.95

-- ---------- やってみよう ----------
-- 1. U（全部「上」）と R（でたらめ）で始めて、改善の回数をくらべよう
-- 2. 評価のたびに V を 0 からやり直すと、評価の回数はどれくらい増える？（いまは前の V から続けている）
-- 3. γ を 0.99 にすると、価値反復と方策反復の回数はそれぞれどう変わる？
-- 4. 評価を最後まで待たずに数回で打ち切って改善すると？（価値反復との中間）
-- 5. 壁を置いて迷路にすると、改善の回数はどう変わる？

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
-- ---- グリッドワールド：文字の地図 '.' 床、'#' 壁、'S' スタート、'G' 宝（+1 で終わり）、'P' 落とし穴（−1 で終わり）
dirs :: [(Int, Int)]
dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)]       -- 行動 0 上、1 右、2 下、3 左

dirNames :: [String]
dirNames = ["up", "right", "down", "left"]

data GW = GW { gwW, gwH :: !Int, gwCell :: !(U.Vector Char) }

gridParse :: [String] -> GW
gridParse rows = GW (length (headOr "" rows)) (length rows) (U.fromList (concat rows))

cellOf :: GW -> Int -> Char
cellOf gw s = if s >= 0 && s < U.length (gwCell gw) then gwCell gw U.! s else '#'

-- 決まった向きに 1 マス。外や壁ならその場にとどまる
gridMove :: GW -> Int -> Int -> Int
gridMove gw s a =
  let (dx, dy) = idx (0, 0) dirs a
      x = s `mod` gwW gw + dx
      y = s `div` gwW gw + dy
  in if x < 0 || y < 0 || x >= gwW gw || y >= gwH gw || cellOf gw (y * gwW gw + x) == '#' then s else y * gwW gw + x

isTerminalCell :: Char -> Bool
isTerminalCell c = c == 'G' || c == 'P'

enterReward :: Char -> Double -> Double
enterReward c stepR = if c == 'G' then 1 else if c == 'P' then -1 else stepR

-- 遷移 (次の状態 s2, 確率 p, 報酬 r)
data Tr = Tr { trS2 :: !Int, trP :: !Double, trR :: !Double }

-- マルコフ決定過程：mP V.! s V.! a = [遷移]。意図した向きに 1 − slip、左右の直角の向きに slip/2 ずつ
data Mdp = Mdp { mdpNS :: !Int, mdpP :: !(V.Vector (V.Vector [Tr])), mdpTerm :: !(U.Vector Bool), mdpGW :: !GW }

buildMdp :: GW -> Double -> Double -> Mdp
buildMdp gw slip stepR = Mdp nS (V.generate nS row) term gw
  where
    nS = gwW gw * gwH gw
    term = U.generate nS (\s -> let c = cellOf gw s in isTerminalCell c || c == '#')
    row s = V.generate 4 (\a -> if term U.! s then [Tr s 1 0] else outs s a)   -- 終わりのマス（と壁）はそこに留まり、報酬 0
    outs s a = foldl' add [] [(a, 1 - slip), ((a + 1) `mod` 4, slip / 2), ((a + 3) `mod` 4, slip / 2)]
      where add list (d, p)
              | p <= 0 = list
              | otherwise = let s2 = gridMove gw s d
                            in if any ((== s2) . trS2) list then [ if trS2 o == s2 then o { trP = trP o + p } else o | o <- list ]
                               else list ++ [Tr s2 p (enterReward (cellOf gw s2) stepR)]

trans :: Mdp -> Int -> Int -> [Tr]
trans mdp s a = (mdpP mdp V.! s) V.! a

isTerm :: Mdp -> Int -> Bool
isTerm mdp s = mdpTerm mdp U.! s

qValue :: Mdp -> U.Vector Double -> Double -> Int -> Int -> Double
qValue mdp v gamma s a = foldl' (\q o -> q + trP o * (trR o + gamma * (v U.! trS2 o))) 0 (trans mdp s a)

-- 価値反復の 1 回：V(s) ← max_a Σ p (r + γ V(s'))。(新しい V, 最大の変化量)
viSweep :: Mdp -> U.Vector Double -> Double -> (U.Vector Double, Double)
viSweep mdp v gamma = (nv, U.maximum (U.cons 0 (U.zipWith (\a b -> abs (a - b)) nv v)))
  where nv = U.generate (mdpNS mdp) (\s -> if isTerm mdp s then v U.! s else maximum' [ qValue mdp v gamma s a | a <- [0 .. 3] ])
        maximum' = foldl' max negInf

-- (V, 回数, 毎回の変化量)
valueIteration :: Mdp -> Double -> Double -> Int -> (U.Vector Double, Int, [Double])
valueIteration mdp gamma tol maxIt = go (U.replicate (mdpNS mdp) 0) 0 posInf []
  where
    go v its d ds
      | d >= tol && its < maxIt = let (v', d') = viSweep mdp v gamma in go v' (its + 1) d' (d' : ds)
      | otherwise = (v, its, reverse ds)

-- 最適な行動の集合（同点は全部）
optimalActs :: Mdp -> U.Vector Double -> Double -> Int -> Double -> [Int]
optimalActs mdp v gamma s tol = [ a | (a, q) <- zip [0 ..] qs, q >= m - tol ]
  where qs = [ qValue mdp v gamma s a | a <- [0 .. 3] ]
        m = maxOf qs

-- 方策評価：V(s) ← Σ p (r + γ V(s')) を、変化が tol 未満になるまで（その場で書きかえる）。(V, 何回まわしたか)
evalPolicy :: Mdp -> U.Vector Int -> Double -> Double -> U.Vector Double -> (U.Vector Double, Int)
evalPolicy mdp pol gamma tol v0 = runST $ do
  v <- U.thaw v0
  let qv s a = foldM (\q o -> do x <- UM.read v (trS2 o); pure (q + trP o * (trR o + gamma * x))) 0 (trans mdp s a)
      sweep = foldM (\d s -> if isTerm mdp s then pure d else do
                        x <- qv s (pol U.! s)
                        old <- UM.read v s
                        UM.write v s x
                        pure (max d (abs (x - old)))) 0 [0 .. mdpNS mdp - 1]
      loop !n d | d >= tol && n < 100000 = do d' <- sweep; loop (n + 1) d'
                | otherwise = pure n
  n <- loop (0 :: Int) posInf
  v' <- U.freeze v
  pure (v', n)

-- マスの左上（p5 の座標）
cellXY :: GW -> Int -> Float -> Float -> Float -> (Float, Float)
cellXY gw s gx gy cs = (gx + fromIntegral (s `mod` gwW gw) * cs, gy + fromIntegral (s `div` gwW gw) * cs)

-- マスの中心から向き a に矢印（太さ wt）
drawArrowIn :: Float -> Float -> Int -> Float -> Float -> Picture
drawArrowIn cx cy a len wt = Pictures
  [ thickLine wt (cx - dx * len * 0.4, cy - dy * len * 0.4) (ex, ey)
  , Polygon [(ex + dx * 5, ey + dy * 5), (ex - dy * 5 - dx * 2, ey + dx * 5 - dy * 2), (ex + dy * 5 - dx * 2, ey - dx * 5 - dy * 2)] ]
  where
    (ix', iy') = idx (0, 0) dirs a
    dx = fromIntegral ix'
    dy = fromIntegral iy'
    ex = cx + dx * len
    ey = cy + dy * len

-- 壁・宝（G）・落とし穴（P）・スタート（S）を描く
drawCells :: GW -> Float -> Float -> Float -> Bool -> Picture
drawCells gw gx gy cs skipFloor = Pictures (concatMap one [0 .. gwW gw * gwH gw - 1])
  where
    ts = max 14 (cs * 0.4)
    one s =
      let c = cellOf gw s
          (x, y) = cellXY gw s gx gy cs
          bgc | c == '#' = rgbC 70 76 120
              | c == 'G' = goldC
              | c == 'P' = rgbC 120 30 70
              | otherwise = rgbC 26 30 60
          mid col str = [sayM str (x + cs / 2) (y + cs / 2) ts col]
      in if c == '.' && skipFloor then [] else
           [Color bgc (rectTL (x + 1) (y + 1) (cs - 2) (cs - 2))]
           ++ (if c == 'G' then mid (rgbC 40 30 0) "G" else [])
           ++ (if c == 'P' then mid (rgbC 255 150 190) "P" else [])
           ++ (if c == 'S' then mid cyanC "S" else [])

-- マウスの下のマス（なければ −1）
cellAt :: GW -> Float -> Float -> Float -> Float -> Float -> Int
cellAt gw gx gy cs mx my =
  let x = floor ((mx - gx) / cs)
      y = floor ((my - gy) / cs)
  in if x >= 0 && y >= 0 && x < gwW gw && y < gwH gw then y * gwW gw + x else -1

-- 決定的に動くときの、各マスからゴールまでの最短の歩数（届かなければ posInf）
bfsDist :: GW -> Int -> U.Vector Double
bfsDist gw goal = go (U.replicate n posInf U.// [(goal, 0)]) (Seq.singleton goal)
  where
    n = gwW gw * gwH gw
    go d q = case Seq.viewl q of
      Seq.EmptyL -> d
      s Seq.:< rest ->
        let step (dd, qq) a =
              let (ddx, ddy) = idx (0, 0) dirs a
                  x = s `mod` gwW gw - ddx
                  y = s `div` gwW gw - ddy
                  p = y * gwW gw + x            -- p から向き a で s に来られる？
              in if x < 0 || y < 0 || x >= gwW gw || y >= gwH gw || cellOf gw p == '#' || dd U.! p < posInf || isTerminalCell (cellOf gw p)
                   then (dd, qq)
                   else if gridMove gw p a == s then (dd U.// [(p, dd U.! s + 1)], qq Seq.|> p) else (dd, qq)
            (d', q') = foldl' step (d, rest) [0 .. 3]
        in go d' q'

-- 低い = 紫、0 = 紺、高い = 金
valueFill :: Double -> Double -> Double -> Color
valueFill v lo hi =
  let rng' = if hi - lo == 0 then 1 else hi - lo
      t = clampv ((v - lo) / rng') 0 1
  in if t < 0.5 then let u = t / 0.5 in rgbD (110 - 80 * u) (60 - 30 * u) (150 - 90 * u)
     else let u = (t - 0.5) / 0.5 in rgbD (30 + 220 * u) (30 + 160 * u) (60 - 20 * u)

rgbD :: Double -> Double -> Double -> Color
rgbD r g b = makeColor (f2 r / 255) (f2 g / 255) (f2 b / 255) 1
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
