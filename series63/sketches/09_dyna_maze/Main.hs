-- ============================================
-- 63-09 モデルを使う Dyna-Q ─ 実際の 1 歩で Q 学習をしたあと、「(s, a) → (r, s′)」の記録（モデル）から
-- 思い出した経験で n 回の Q 学習をくり返す（頭の中の計画 = プランニング）。同じ経験を何度も使えるので、少ない歩数で学べる。
-- 1：計画回数 n = 0・5・50 のくらべっこ　2：途中で道がふさがる迷路　3：途中で近道ができる迷路
-- 2・3 では Dyna-Q と Dyna-Q+（長く試していない行動に小さなボーナス κ√τ）をくらべる
-- 操作：1 2 3 実験をえらぶ / F 速さ / スペース 一時停止 / R やり直し
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.))
import Data.Foldable (toList)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word32)
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- ★ 迷路（S スタート、G ゴール）
mazeBasic, mazeBlockA, mazeBlockB, mazeShortA, mazeShortB :: [String]
mazeBasic = [".......#G", "..#....#.", "S.#....#.", "..#......", ".....#...", "........."]
mazeBlockA = ["........G", ".........", ".........", "########.", ".........", "...S....."]
mazeBlockB = ["........G", ".........", ".........", ".########", ".........", "...S....."]
mazeShortA = ["........G", ".........", ".........", ".########", ".........", "...S....."]
mazeShortB = ["........G", ".........", ".........", ".#######.", ".........", "...S....."]

gammaD, alphaD, epsD, kappa :: Double
gammaD = 0.95                            -- ★ 割引率
alphaD = 0.1                             -- ★ 学習率
epsD = 0.1                               -- ★ ε
kappa = 0.001                            -- ★ Dyna-Q+ のボーナスの強さ

changeAt, totalOf :: Int -> Int
changeAt m = if m == 2 then 1000 else 3000   -- ★ 迷路が変わる歩数
totalOf m = if m == 2 then 3000 else 6000    -- ★ 全体の歩数

-- 実験ごとのエージェント：(計画回数 n, Dyna-Q+ か, 名前, 色)
kindsOf :: Int -> [(Int, Bool, String, Color)]
kindsOf 1 = [(0, False, "n = 0 (plain Q-learning)", pinkC), (5, False, "n = 5", cyanC), (50, False, "n = 50", goldC)]
kindsOf _ = [(50, False, "Dyna-Q (n = 50)", cyanC), (50, True, "Dyna-Q+ (n = 50)", goldC)]

-- Dyna-Q のエージェント。モデルは (s, a) の番号 k = 4s + a ごとの (報酬, 次の状態, 終わり, 時刻) と、記録した順の番号の列
data Agent = Agent
  { agN      :: !Int
  , agPlus   :: !Bool
  , agG      :: !GW
  , agQ      :: !(U.Vector Double)       -- Q(s, a) = agQ ! (4s + a)
  , agHas    :: !(U.Vector Bool)
  , agMR     :: !(U.Vector Double)
  , agMS     :: !(U.Vector Int)
  , agMD     :: !(U.Vector Bool)
  , agMT     :: !(U.Vector Int)
  , agKeys   :: !(U.Vector Int)          -- 記録した順（ランダムに思い出すときに使う）
  , agSeen   :: !(U.Vector Bool)
  , agS      :: !Int
  , agLen    :: !Int
  , agEpLens :: !(Seq.Seq Double)
  , agCum    :: !Int
  , agCumHist :: !(Seq.Seq Double) }

data World = World
  { wMode   :: !Int
  , wAgents :: ![Agent]
  , wTick   :: !Int
  , wSpeed  :: !Int
  , wPaused :: !Bool
  , wRng    :: !Rng
  , wTest   :: [(String, Bool)] }

speedNames :: [String]
speedNames = ["slow", "normal", "fast"]

main :: IO ()
main = runSketch (sketch "63-09 Dyna-Q maze" (resetMode (World 1 [] 0 1 False (seedRng 6) [])))
  { skBack  = bgC
  , skStart = \_ -> do
      let rs = selfTest
      printSelfTest rs
      pure (resetMode (World 1 [] 0 1 False (seedRng 6) rs))
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

mazeFor :: Int -> Int -> [String]
mazeFor m t
  | m == 1 = mazeBasic
  | m == 2 = if t < changeAt 2 then mazeBlockA else mazeBlockB
  | otherwise = if t < changeAt 3 then mazeShortA else mazeShortB

resetMode :: World -> World
resetMode w = w { wTick = 0, wAgents = [ makeAgent n plus (gridParse (mazeFor (wMode w) 0)) | (n, plus, _, _) <- kindsOf (wMode w) ] }

startOf :: GW -> Int
startOf g = maybe 0 id (U.elemIndex 'S' (gwCell g))

makeAgent :: Int -> Bool -> GW -> Agent
makeAgent n plus g = Agent n plus g (U.replicate (4 * ns) 0) (U.replicate (4 * ns) False) (U.replicate (4 * ns) 0) (U.replicate (4 * ns) 0)
                       (U.replicate (4 * ns) False) (U.replicate (4 * ns) 0) U.empty (U.replicate ns False) (startOf g) 0 Seq.empty 0 (Seq.singleton 0)
  where ns = gwW g * gwH g

-- Q 学習の更新：Q(s,a) ← Q(s,a) + α (目標 − Q(s,a))
qUpdate :: UM.MVector s Double -> Int -> Int -> Double -> Int -> Bool -> ST s ()
qUpdate q s a r s2 done = do
  t <- if done then pure r else do
         qs <- mapM (\b -> UM.read q (4 * s2 + b)) [0 .. 3]
         pure (r + gammaD * maxOf qs)
  old <- UM.read q (4 * s + a)
  UM.write q (4 * s + a) (old + alphaD * (t - old))

-- ★ Dyna-Q の 1 歩：(1) 実際に動いて Q 学習 (2) モデルに記録 (3) モデルからランダムに n 回思い出して Q 学習
dynaStep :: GW -> Int -> Agent -> R Agent
dynaStep g now ag = R $ \g0 -> runST $ do
  ref <- newSTRef g0
  q <- U.thaw (agQ ag)
  has <- U.thaw (agHas ag)
  mr <- U.thaw (agMR ag)
  ms <- U.thaw (agMS ag)
  md <- U.thaw (agMD ag)
  mt <- U.thaw (agMT ag)
  let s = agS ag
  qs <- mapM (\b -> UM.read q (4 * s + b)) [0 .. 3]
  a <- liftR ref (epsGreedy qs epsD)
  let s2 = gridMove g s a
      done = cellOf g s2 == 'G'
      r = if done then 1 else 0
  qUpdate q s a r s2 done
  -- Dyna-Q+：はじめて来た状態の、まだ試していない行動も「その場にとどまる・報酬 0」としてモデルに入れる
  newKeys1 <- if agPlus ag && not (agSeen ag U.! s)
    then fmap concat $ mapM (\b -> do
           let k = s * 4 + b
           h <- UM.read has k
           if h then pure [] else do
             UM.write has k True; UM.write mr k 0; UM.write ms k s; UM.write md k False; UM.write mt k 0
             pure [k]) [0 .. 3]
    else pure []
  let k = s * 4 + a
  hk <- UM.read has k
  UM.write has k True; UM.write mr k r; UM.write ms k s2; UM.write md k done; UM.write mt k now
  let keys = agKeys ag U.++ U.fromList (newKeys1 ++ [k | not hk])
      nk = U.length keys
  forM_ [1 .. agN ag] $ \_ -> do
    u <- rndS ref
    let kk = keys U.! floor (u * fromIntegral nk)
    rr <- UM.read mr kk
    ss <- UM.read ms kk
    dd <- UM.read md kk
    tt <- UM.read mt kk
    qUpdate q (kk `div` 4) (kk `mod` 4) (rr + (if agPlus ag then kappa * sqrt (fromIntegral (now - tt)) else 0)) ss dd
  q' <- U.freeze q
  has' <- U.freeze has
  mr' <- U.freeze mr
  ms' <- U.freeze ms
  md' <- U.freeze md
  mt' <- U.freeze mt
  gEnd <- readSTRef ref
  let len = agLen ag + 1
      cum = agCum ag + (if done then 1 else 0)
      ag1 = ag { agQ = q', agHas = has', agMR = mr', agMS = ms', agMD = md', agMT = mt', agKeys = keys
               , agSeen = if agPlus ag then agSeen ag U.// [(s, True)] else agSeen ag
               , agCum = cum, agCumHist = pushCap 20000 (fromIntegral cum) (agCumHist ag) }
      ag2 = if done then ag1 { agEpLens = pushCap 20000 (fromIntegral len) (agEpLens ag), agLen = 0, agS = startOf g }
            else ag1 { agLen = len, agS = s2 }
  pure (ag2, gEnd)

stepAll :: World -> World
stepAll w
  | wMode w == 1 = let (ags, g) = runWith (wRng w) (mapM (\ag -> if Seq.length (agEpLens ag) < 50 then dynaStep (agG ag) (wTick w) ag else pure ag) (wAgents w))
                   in w { wAgents = ags, wRng = g, wTick = wTick w + 1 }
  | wTick w >= totalOf (wMode w) = w
  | otherwise =
      let ags0 = if wTick w == changeAt (wMode w) then [ ag { agG = gridParse (mazeFor (wMode w) (wTick w)) } | ag <- wAgents w ] else wAgents w   -- ここで迷路が変わる（エージェントには知らせない）
          (ags, g) = runWith (wRng w) (mapM (\ag -> dynaStep (agG ag) (wTick w) ag) ags0)
      in w { wAgents = ags, wRng = g, wTick = wTick w + 1 }

step :: Env -> Float -> World -> IO World
step env _ w
  | wPaused w = pure w
  | otherwise = case wSpeed w of
      0 -> pure (if envFrame env `mod` 3 == 0 then stepAll w else w)
      1 -> pure (iterate stepAll w !! 5)
      _ -> sliced 6 (const True) stepAll w

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Dyna-Q: " ++ title ++ "   " ++ show (wTick w) ++ " steps") 40 42 22 (grayC 240)
  , say ("1 2 3 experiment   F speed (" ++ idx "" speedNames (wSpeed w) ++ ")   Space pause   R restart") 40 68 15 (rgbC 170 176 210) ]
  -- ---- 迷路（エージェントごと、色 = max Q）
  ++ concat [ let gx = 40 + fromIntegral k * (fromIntegral (gwW (agG ag)) * cs + 24)
                  (ax, ay) = cellXY (agG ag) (agS ag) gx gy cs
              in [ Color (cellCol ag s) (rectTL x y (cs - 1) (cs - 1))
                 | s <- [0 .. gwW (agG ag) * gwH (agG ag) - 1], let (x, y) = cellXY (agG ag) s gx gy cs ]
                 ++ [ Color col (circleAt (ax + cs / 2) (ay + cs / 2) (cs * 0.6)), say nm gx (gy - 8) 14 col ]
            | (k, ag, (_, _, nm, col)) <- zip3 [0 :: Int ..] (wAgents w) kinds ]
  ++ graph
  ++ [ legend (pX g + pW g - 260) (pY g + pH g - 20 * fromIntegral (length kinds) - 6) [ (col, nm) | (_, _, nm, col) <- kinds ]
     , drawSelfTest env (wTest w) ]
  where
    mode = wMode w
    kinds = kindsOf mode
    title | mode == 1 = "compare planning steps n"
          | mode == 2 = "the path gets blocked (changes at " ++ show (changeAt 2) ++ " steps)"
          | otherwise = "a shortcut opens (changes at " ++ show (changeAt 3) ++ " steps)"
    g0 = maybe (gridParse mazeBasic) agG (safeHead (wAgents w))
    nA = fromIntegral (length (wAgents w))
    cs = minimum [40, (envW env - 120) / (fromIntegral (gwW g0) * nA + nA), envH env * 0.3 / fromIntegral (gwH g0)]
    gy = 120
    gh = fromIntegral (gwH g0) * cs
    cellCol ag s = let c = cellOf (agG ag) s in
      if c == '#' then rgbC 70 76 120 else if c == 'G' then goldC
      else valueFill (maxOf [ agQ ag U.! (4 * s + b) | b <- [0 .. 3] ]) (-1) 1
    gTop = gy + gh + 70
    g | mode == 1 = G 70 gTop (envW env - 140) (envH env - gTop - 70) 1 50 0 800
      | otherwise = G 70 gTop (envW env - 140) (envH env - gTop - 70) 0 (fromIntegral (totalOf mode)) 0 (if mode == 2 then 150 else 400)
    graph
      | mode == 1 =
          [ plotAxes g "episode" "steps to goal (shown up to 800)" [1, 10, 20, 30, 40, 50] [0, 200, 400, 600, 800]
          , Color (withA (120 / 255) limeC) (lineW 1 (pX g, gY g 14) (pX g + pW g, gY g 14)) ]
          ++ [ drawCurve g (agEpLens ag) col 2.5 1 | (ag, (_, _, _, col)) <- zip (wAgents w) kinds ]
          ++ [ say "Green line = shortest 14 steps. Bigger n gets near the shortest in fewer episodes" (pX g + 10) (pY g + 18) 14 (rgbC 170 176 210) ]
      | otherwise =
          [ plotAxes g "steps" "goals reached (total)" [0, fromIntegral (changeAt mode), fromIntegral (totalOf mode)] [0, y1 g / 2, y1 g]
          , Color (withA (150 / 255) pinkC) (lineW 1 (gX g (fromIntegral (changeAt mode)), pY g) (gX g (fromIntegral (changeAt mode)), pY g + pH g)) ]
          ++ [ drawCurve g (agCumHist ag) col 2.5 0 | (ag, (_, _, _, col)) <- zip (wAgents w) kinds ]
          ++ [ say (if mode == 2 then "At the pink line the right path is blocked. Flat graph = searching for a new path"
                    else "At the pink line a shortcut opens on the right. Dyna-Q is happy with the old path and rarely notices")
                   (pX g + 10) (pY g + 18) 14 (rgbC 170 176 210) ]

safeHead :: [a] -> Maybe a
safeHead xs = case xs of
  (x : _) -> Just x
  [] -> Nothing

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char ch) Down _ _
    | ch `elem` "123" -> resetMode w { wMode = fromEnum ch - fromEnum '0' }
    | ch `elem` "fF" -> w { wSpeed = (wSpeed w + 1) `mod` 3 }
    | ch `elem` "rR" -> resetMode w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 4 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("steps of episodes 2-20 summed: n = 50 (" ++ show (jsRoundI (tot 50)) ++ ") < n = 5 (" ++ show (jsRoundI (tot 5)) ++ ") < n = 0 (" ++ show (jsRoundI (tot 0)) ++ ")"
    , tot 50 < tot 5 && tot 5 < tot 0)
  , ("planning reaches <= 20 steps to the goal in fewer episodes (n = 50: " ++ fmtN 1 (epsTo 50) ++ ", n = 0: " ++ fmtN 1 (epsTo 0) ++ ")", epsTo 50 < epsTo 0)
  , ("goals in the 3000 steps after the shortcut opens: Dyna-Q+ " ++ show (after True) ++ " > Dyna-Q " ++ show (after False), after True > after False)
  , ("one real step records one (s, a) -> (r, s') in the model (s' = the cell actually reached)"
    , U.length (agKeys toy) == 1 && U.length (U.filter id (agHas toy)) == 1 && agMS toy U.! k0 == agS toy && agMR toy U.! k0 == 0) ]
  where
    g = gridParse mazeBasic
    runUntil stop ag t = if stop ag then pure ag else dynaStep g t ag >>= \ag' -> runUntil stop ag' (t + 1)
    tot n = sum [ sum (drop 1 (toList (agEpLens ag))) / 3
                | seed <- [1 .. 3], let ag = withSeed seed (runUntil (\a -> Seq.length (agEpLens a) >= 20) (makeAgent n False g) 0) ]
    epsTo n = sum [ fromIntegral (Seq.length (agEpLens ag)) / 3
                  | seed <- [1 .. 3]
                  , let stop a = let m = Seq.length (agEpLens a) in m >= 60 || (m > 0 && Seq.index (agEpLens a) (m - 1) <= 20)
                  , let ag = withSeed seed (runUntil stop (makeAgent n False g) 0) ] :: Double
    gA = gridParse mazeShortA
    gB = gridParse mazeShortB
    after plus = sum [ withSeed seed (do
                         let go t ag | t >= 3000 = pure ag
                                     | otherwise = dynaStep gA t ag >>= go (t + 1)
                             go2 t ag | t >= 6000 = pure ag
                                      | otherwise = dynaStep gB t ag >>= go2 (t + 1)
                         a1 <- go 0 (makeAgent 50 plus gA)
                         a2 <- go2 3000 a1
                         pure (agCum a2 - agCum a1)) | seed <- [1, 2] ]
    toy = withSeed 1 (dynaStep g 0 (makeAgent 3 False g))
    k0 = agKeys toy U.! 0

-- ---------- やってみよう ----------
-- 1. 実験 1 で n を 500 にすると？（考えるほど速く学べるが、1 歩の計算は重くなる）
-- 2. 実験 2：ふさがったあと、Dyna-Q は何歩くらいで新しい道を見つける？
-- 3. 実験 3：kappa を 0.01 にすると、近道に気づくのは早くなる？ 遅くなる？
-- 4. モデルがまちがっている（世界が変わった）とき、計画はむしろ害になる。どんなときか考えよう
-- 5. 自分の迷路を mazeBasic のように足してみよう

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
-- ---- 書きかえられる表（ST）の中で使う乱数（重い学習用。R と同じ列になる）
type RngRef s = STRef s Rng

rndS :: RngRef s -> ST s Double
rndS ref = do
  s <- readSTRef ref
  let (u, s'@(Rng w)) = rndStep s
  w `seq` writeSTRef ref s'
  pure u

-- R の計算を ST の中で（同じ乱数の状態を続けて使う）
liftR :: RngRef s -> R a -> ST s a
liftR ref m = do
  s <- readSTRef ref
  let (a, s'@(Rng w)) = runR m s
  w `seq` writeSTRef ref s'
  pure a
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
