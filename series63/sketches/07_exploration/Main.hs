-- ============================================
-- 63-07 探索の工夫 ─ 報酬がゴール（+1）にしかない迷路。1 歩ごとの報酬は 0 なので、ゴールを一度見つけるまで何も学べない
-- 5 つのエージェントが同じ迷路で同時に Q 学習する。ちがうのは「どう行動を選ぶか」だけ：
--   ε 一定 / ε を減らす / 楽観的な初期値（Q を 1 から始め、ε = 0）/ ボルツマン選択（ソフトマックス）/ 貪欲だけ（Q = 0、ε = 0）
-- 小さな迷路の色 = そのマスに来た回数（明るいほど多い）
-- 操作：F 速さ / スペース 一時停止 / R やり直し / ↑↓ 温度 τ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad.ST (runST)
import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.Bits (shiftR, xor, (.|.))
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word32)
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

mapRows :: [String]
mapRows =                                -- ★ 迷路（S スタート、G ゴール）
  [ "S...#......."
  , ".##.#.####.."
  , ".#..#....#.."
  , ".#.####..#.."
  , ".#....#..#.."
  , ".####.#.##.."
  , "......#....#"
  , "#####...##.G" ]

gammaE, alphaE, epsE, qOpt :: Double
gammaE = 0.95                            -- ★ 割引率
alphaE = 0.5                             -- ★ 学習率
epsE = 0.1                               -- ★ ε 一定の ε
qOpt = 1                                 -- ★ 楽観的な初期値

maxLen :: Int
maxLen = 3000                            -- ★ 1 エピソードの上限

speedNames :: [String]
speedNames = ["slow", "normal", "fast"]

data Kind = KEps | KDecay | KOpt | KBoltz | KGreedy deriving (Eq)

-- [種類, 名前, 色]
kinds :: [(Kind, String, (Double, Double, Double))]
kinds = [ (KEps, "eps fixed (0.1)", (90, 215, 245)), (KDecay, "eps decays (1 -> 0.02)", (160, 230, 110))
        , (KOpt, "optimistic init (Q = 1, eps = 0)", (255, 196, 60)), (KBoltz, "Boltzmann (softmax)", (175, 140, 255))
        , (KGreedy, "greedy only (Q = 0, eps = 0)", (245, 110, 170)) ]

kindCol :: (Double, Double, Double) -> Color
kindCol (r, g, b) = rgbD r g b

data Agent = Agent
  { agKind   :: !Kind
  , agQ      :: !(U.Vector Double)
  , agS      :: !Int
  , agLen    :: !Int
  , agCount  :: !Int                     -- 終わったエピソードの数
  , agFirst  :: !(Seq.Seq Double)        -- 最初の 41 エピソードの歩数（グラフと自己テスト用）
  , agVisits :: !(U.Vector Double)
  , agFails  :: !Int }

data World = World
  { wAgents :: ![Agent]
  , wTau    :: !Double                   -- ★ ボルツマン選択の温度 τ（小さいほど貪欲）
  , wSpeed  :: !Int
  , wPaused :: !Bool
  , wRng    :: !Rng
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "63-07 Exploration" (world0 []))
  { skBack  = bgC
  , skStart = \_ -> do
      let rs = selfTest
      printSelfTest rs
      pure (world0 rs)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

gw0 :: GW
gw0 = gridParse mapRows

startS, goalS :: Int
startS = maybe 0 id (U.elemIndex 'S' (gwCell gw0))
goalS = maybe 0 id (U.elemIndex 'G' (gwCell gw0))

world0 :: [(String, Bool)] -> World
world0 rs = World (newAgents gw0) 0.02 1 False (seedRng 4) rs

newAgents :: GW -> [Agent]
newAgents g = [ makeAgent g k | (k, _, _) <- kinds ]

makeAgent :: GW -> Kind -> Agent
makeAgent g kind = Agent kind (U.replicate (n * 4) q0) startS 0 0 Seq.empty (U.replicate n 0) 0
  where q0 = if kind == KOpt then qOpt else 0
        n = gwW g * gwH g

qRow :: U.Vector Double -> Int -> [Double]
qRow q s = U.toList (U.slice (s * 4) 4 q)

-- ★ 行動の選び方（ここだけがちがう）
chooseAct :: Double -> Agent -> R Int
chooseAct tau ag = case agKind ag of
  KEps -> epsGreedy q epsE
  KDecay -> epsGreedy q (max 0.02 (1 / (1 + fromIntegral (agCount ag) / 3)))
  KBoltz -> sampleFrom (softmaxArr q tau)
  _ -> pure (argmaxFirst q)              -- 楽観的と貪欲だけ：いちばん良い向き（同点なら上→右→下→左の順）
  where q = qRow (agQ ag) (agS ag)

-- Q 学習の 1 歩（5 つとも同じ）
agentStep :: GW -> Double -> Agent -> R Agent
agentStep g tau ag = do
  a <- chooseAct tau ag
  let s = agS ag
      q = agQ ag
      s2 = gridMove g s a
      done = cellOf g s2 == 'G'
      r = if done then 1 else 0
      i = s * 4 + a
      old = q U.! i
      q' = U.modify (\m -> UM.write m i (old + alphaE * ((if done then r else r + gammaE * maxOf (qRow q s2)) - old))) q
      vis = U.modify (\m -> UM.modify m (+ 1) s2) (agVisits ag)
      len = agLen ag + 1
  pure $ if done || len >= maxLen
    then ag { agQ = q', agVisits = vis, agLen = 0, agS = startS, agCount = agCount ag + 1
            , agFails = agFails ag + (if done then 0 else 1)
            , agFirst = if Seq.length (agFirst ag) < 41 then agFirst ag Seq.|> fromIntegral len else agFirst ag }
    else ag { agQ = q', agVisits = vis, agLen = len, agS = s2 }

-- 貪欲にたどってゴールまでの歩数（200 歩で着かなければ −1）
greedyLen :: Agent -> GW -> Int
greedyLen ag g = go 1 startS
  where go t s | t > 200 = -1
               | otherwise = let s' = gridMove g s (argmaxFirst (qRow (agQ ag) s))
                             in if cellOf g s' == 'G' then t else go (t + 1) s'

learnOnce :: World -> World
learnOnce w = let (ags, r) = runWith (wRng w) (mapM (agentStep gw0 (wTau w)) (wAgents w)) in w { wAgents = ags, wRng = r }

step :: Env -> Float -> World -> IO World
step env _ w
  | wPaused w = pure w
  | wSpeed w == 0 = pure (if envFrame env `mod` 3 == 0 then learnOnce w else w)
  | wSpeed w == 1 = pure (iterate learnOnce w `idxW` 10)
  | otherwise = sliced 6 (const True) learnOnce w
  where idxW xs k = idx w xs k

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Exploration: the only reward is far away" 40 42 22 (grayC 240)
  , say ("F speed (" ++ idx "" speedNames (wSpeed w) ++ ")   Space pause   R restart   Up/Down Boltzmann temperature tau = " ++ fmtN 3 (wTau w)) 40 68 15 (rgbC 170 176 210) ]
  -- ---- 5 つの小さな迷路（来た回数のヒートマップ）
  ++ concat (zipWith agentPic [0 :: Int ..] (wAgents w))
  -- ---- エピソードごとの歩数
  ++ [ plotAxes g "episode" "log10 (steps)" [0, 20, 40] [1, 2, 3]
     , Color (withA (120 / 255) limeC) (lineW 1 (pX g, gY g (logBase 10 l)) (pX g + pW g, gY g (logBase 10 l))) ]
  ++ [ drawCurve g (fmap (\v -> logBase 10 (max 1 v)) (agFirst ag)) (kindCol c) 2 0 | (ag, (_, _, c)) <- zip (wAgents w) kinds ]
  ++ [ sayWrap ("First 40 episodes. Green = shortest " ++ show (round l :: Int) ++ ", top = cut off at " ++ show maxLen) (pX g - 20) (pY g + pH g + 40) (envW env - pX g) 14 (rgbC 170 176 210)
     , drawSelfTest env (wTest w) ]
  where
    gw = gw0
    ncol = 3 :: Int
    bw = (envW env - 80) / fromIntegral ncol
    cs = min (bw / fromIntegral (gwW gw + 1)) (envH env * 0.5 / 2 / fromIntegral (gwH gw + 2))
    l = bfsDist gw goalS U.! startS
    agentPic k ag =
      let gx = 40 + fromIntegral (k `mod` ncol) * bw
          gy = 120 + fromIntegral (k `div` ncol) * (fromIntegral (gwH gw) * cs + 60)
          mx = log (1 + U.maximum (agVisits ag))
          (_, nm, kc@(cr, cg, cb)) = idx (KEps, "", (0, 0, 0)) kinds k
          cellP s = let (x, y) = cellXY gw s gx gy cs
                        c = cellOf gw s
                        t = if mx > 0 then log (1 + agVisits ag U.! s) / mx else 0
                        col | c == '#' = rgbC 60 66 110
                            | c == 'G' = goldC
                            | otherwise = rgbD (20 + t * cr * 0.9) (22 + t * cg * 0.9) (50 + t * cb * 0.7)
                    in Color col (rectTL x y (cs - 1) (cs - 1))
          (ax, ay) = cellXY gw (agS ag) gx gy cs
          gl = greedyLen ag gw
      in map cellP [0 .. gwW gw * gwH gw - 1]
         ++ [ Color white (circleAt (ax + cs / 2) (ay + cs / 2) (cs * 0.6))
            , say nm gx (gy - 8) 14 (kindCol kc)
            , say ("goal " ++ show (agCount ag - agFails ag) ++ " times   greedy path " ++ (if gl > 0 then show gl ++ " steps" ++ (if fromIntegral gl == l then " OK" else "") else "not yet"))
                  gx (gy + fromIntegral (gwH gw) * cs + 18) 14 (rgbC 200 205 235) ]
    px = 40 + 2 * bw
    gTop = 120 + fromIntegral (gwH gw) * cs + 80
    g = G (px + 30) gTop (bw - 50) (max 80 (envH env - gTop - 110)) 0 40 1 3.6

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char ch) Down _ _
    | ch `elem` "fF" -> w { wSpeed = (wSpeed w + 1) `mod` 3 }
    | ch `elem` "rR" -> w { wAgents = newAgents gw0 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wTau = min 1 (wTau w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wTau = max 0.005 (wTau w / 2) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("greedy only (Q = 0, ties in a fixed order) does not reach the goal in " ++ show maxLen ++ " steps (goes back and forth)", agFails gr == 1)
  , ("optimistic init (eps = 0) reaches the goal from the 1st episode (" ++ joinW "/" (map show firsts) ++ " steps); after 60 the greedy path = shortest " ++ showL ++ " steps (3 seeds)", okOpt)
  , ("eps fixed, eps decay and Boltzmann also learn a path to the goal in 300 episodes (" ++ showL ++ "-" ++ show (round l + 2 :: Int) ++ " steps: " ++ joinW "," (map show lens) ++ ")", okAll)
  , ("total steps of the first 20 episodes: optimistic " ++ show (jsRoundI (sOpt / 3)) ++ " < eps fixed " ++ show (jsRoundI (sEps / 3)) ++ " (mean of 3 seeds)", sOpt < sEps)
  , ("softmax probabilities sum to 1 and do not overflow for large values", near (sum p) 1 1e-12 && noNaN p) ]
  where
    g = gw0
    l = bfsDist g goalS U.! startS
    showL = show (round l :: Int)
    run kind seed n = withSeed seed (train (makeAgent g kind))
      where train a | agCount a >= n = pure a
                    | otherwise = agentStep g 0.02 a >>= train
    gr = run KGreedy 1 1
    ops = [ run KOpt seed 60 | seed <- [1 .. 3] ]
    firsts = [ round (Seq.index (agFirst op) 0) :: Int | op <- ops ]
    okOpt = and [ Seq.index (agFirst op) 0 < fromIntegral maxLen && fromIntegral (greedyLen op g) == l | op <- ops ]
    lens = [ greedyLen (run kind seed 300) g | kind <- [KEps, KDecay, KBoltz], seed <- [1 .. 3] ]
    okAll = and [ fromIntegral gl >= l && fromIntegral gl <= l + 2 | gl <- lens ]
    tot a = sum (take 20 (toList (agFirst a)))
    sOpt = sum [ tot (run KOpt seed 20) | seed <- [1 .. 3] ]
    sEps = sum [ tot (run KEps seed 20) | seed <- [1 .. 3] ]
    p = softmaxArr [1000, 0, -5, 2] 0.01
    joinW sep xs = case xs of [] -> ""; _ -> foldr1 (\a b -> a ++ sep ++ b) xs

-- ---------- やってみよう ----------
-- 1. どのエージェントがいちばん早く最短の道を見つける？ ヒートマップで「どこを探したか」をくらべよう
-- 2. ↑↓ で τ を変えると、ボルツマン選択はどう変わる？（大きい = でたらめ、小さい = 貪欲）
-- 3. qOpt を 0.1 にすると、楽観的な初期値の探索はどう変わる？
-- 4. 貪欲だけのエージェントがどこで行き来しているか、ヒートマップで探そう
-- 5. ε 一定のエージェントが 22 歩の道に落ち着いたら、左の縦の道（20 歩）を試すには何が要る？
-- 6. 1 歩ごとに −0.01 の報酬にすると、貪欲だけでも探索するようになる。なぜ？（05 と同じしくみ）

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
