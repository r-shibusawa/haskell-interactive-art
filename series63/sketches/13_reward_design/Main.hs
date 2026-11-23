-- ============================================
-- 63-13 報酬の設計と「ずる」─ 宝（+1）に早く着いてほしいので「宝に近づいたら +0.1」のごほうびを足すと…？
-- 近づく → 離れる → 近づく…と行ったり来たりすれば、宝に着かなくても +0.1 をもらい続けられる（報酬ハッキング）。
-- ポテンシャルに基づく整形 F = γΦ(s′) − Φ(s)（Φ = −0.1 × 宝までの距離）なら、1 周してもどれば合計がほぼ 0 になり、ずるができない。
-- しかも最適方策は元の報酬とまったく同じになる（価値は V − Φ にずれるだけ）
-- 3 つのエージェントが同時に Q 学習。矢印 = 価値反復で求めた最適方策
-- 操作：F 速さ / スペース 一時停止 / R やり直し / ↑↓ ごほうびの大きさ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad.ST (runST)
import Control.Exception (evaluate)
import Control.Monad (foldM, unless, when)
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
mapRows =                                -- ★ 部屋（S スタート、G 宝）
  [ "........."
  , "........."
  , "S...#...."
  , "....#...G"
  , "....#...."
  , "........." ]

gammaR, alphaR, epsR :: Double
gammaR = 0.99                            -- ★ 割引率
alphaR = 0.5                             -- ★ 学習率
epsR = 0.1                               -- ★ ε

maxLen :: Int
maxLen = 200                             -- ★ 1 エピソードの上限

speedNames :: [String]
speedNames = ["slow", "normal", "fast"]

data Kind = KOrig | KNaive | KPot deriving (Eq)

-- 価値反復で求めた最適な価値と、マスごとの最適な行動の集合
data Plan = Plan { plV :: !(U.Vector Double), plActs :: !(V.Vector [Int]) }

data Agent = Agent
  { agKind   :: !Kind
  , agQ      :: !(U.Vector Double)
  , agS      :: !Int
  , agLen    :: !Int
  , agGoals  :: !Int
  , agShaped :: !Double                   -- もらった報酬の合計
  , agHist   :: !(Seq.Seq Double) }       -- 20 歩ごとの「宝に着いた回数」

data World = World
  { wBonus  :: !Double                   -- ★ 素朴な整形のごほうび（近づいたら +bonus）。ポテンシャルは Φ = −bonus × 距離
  , wDist   :: !(U.Vector Double)
  , wPlans  :: ![Plan]
  , wAgents :: ![Agent]
  , wTick   :: !Int
  , wSpeed  :: !Int
  , wPaused :: !Bool
  , wRng    :: !Rng
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "63-13 Reward design and hacking" (world0 []))
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

allKinds :: [Kind]
allKinds = [KOrig, KNaive, KPot]

kindName :: Double -> Kind -> String
kindName b k = case k of
  KOrig -> "original reward (+1 at the goal only)"
  KNaive -> "naive shaping (+" ++ fmtN 2 b ++ " when closer)"
  KPot -> "potential shaping g*Phi(s') - Phi(s)"

kindCol :: Kind -> Color
kindCol k = case k of
  KOrig -> cyanC
  KNaive -> pinkC
  KPot -> goldC

world0 :: [(String, Bool)] -> World
world0 rs = rebuild (World 0.1 (U.replicate 0 0) [] [] 0 1 False (seedRng 13) rs)

rebuild :: World -> World
rebuild w = w { wDist = dd, wPlans = [ solve gw0 dd k (wBonus w) | k <- allKinds ], wAgents = map newAgent allKinds, wTick = 0 }
  where dd = bfsDist gw0 goalS

newAgent :: Kind -> Agent
newAgent k = Agent k (U.replicate (gwW gw0 * gwH gw0 * 4) 0) startS 0 0 0 (Seq.singleton 0)

-- ポテンシャル Φ(s)（宝では 0）
phi :: U.Vector Double -> Int -> Double -> Double
phi dd s b = if isInfinite (dd U.! s) then 0 else -b * (dd U.! s)

-- ★ 報酬の 3 つの作り方
rewardOf :: GW -> Kind -> Int -> Int -> U.Vector Double -> Double -> Double
rewardOf g kind s s2 dd b = r0 + naive + pot
  where
    r0 = if cellOf g s2 == 'G' then 1 else 0
    naive = if kind == KNaive && dd U.! s2 < dd U.! s then b else 0                    -- 近づいたらごほうび（離れても罰はない）
    pot = if kind == KPot then (if cellOf g s2 == 'G' then 0 else gammaR * phi dd s2 b) - phi dd s b else 0   -- 終わりの状態の Φ は 0

-- 価値反復（決定的な迷路。その場で書きかえる）で、その報酬での最適な価値と方策
solve :: GW -> U.Vector Double -> Kind -> Double -> Plan
solve g dd kind b = Plan v acts
  where
    n = gwW g * gwH g
    skip s = cellOf g s == '#' || cellOf g s == 'G'
    nxt = U.generate (n * 4) (\i -> gridMove g (i `div` 4) (i `mod` 4))
    rew = U.generate (n * 4) (\i -> rewardOf g kind (i `div` 4) (nxt U.! i) dd b)
    isG = U.generate n (\s -> cellOf g s == 'G')
    qOf vv s a = let i = s * 4 + a; s2 = nxt U.! i in rew U.! i + (if isG U.! s2 then 0 else gammaR * vv s2)
    v = runST $ do
      mv <- UM.replicate n 0
      let sweep = foldM (\delta s -> if skip s then pure delta else do
                            qs <- mapM (\a -> let i = s * 4 + a; s2 = nxt U.! i in
                                          if isG U.! s2 then pure (rew U.! i) else do x <- UM.read mv s2; pure (rew U.! i + gammaR * x)) [0 .. 3]
                            let best = foldl' max negInf qs
                            old <- UM.read mv s
                            UM.write mv s best
                            pure (max delta (abs (best - old)))) 0 [0 .. n - 1]
          loop it = when (it < 5000) $ do
            d <- sweep
            unless (d < 1e-12) (loop (it + 1))
      loop 0
      U.freeze mv
    acts = V.generate n (\s -> if skip s then [] else
             let qs = [ qOf (v U.!) s a | a <- [0 .. 3] ]
                 m = maxOf qs
             in [ a | (a, q) <- zip [0 ..] qs, q >= m - 1e-9 ])

-- S から最適な向き（最初のもの）に進む。ループしたら止める
rollout :: GW -> Plan -> [Int]
rollout g plan = go (0 :: Int) startS [startS]
  where go t s p | t >= 100 || cellOf g s == 'G' = reverse p
                 | otherwise = let s' = gridMove g s (headOr 0 (plActs plan V.! s)) in go (t + 1) s' (s' : p)

agentStep :: GW -> U.Vector Double -> Double -> Agent -> R Agent
agentStep g dd b ag = do
  let s = agS ag
      q = agQ ag
  a <- epsGreedy (U.toList (U.slice (s * 4) 4 q)) epsR
  let s2 = gridMove g s a
      done = cellOf g s2 == 'G'
      r = rewardOf g (agKind ag) s s2 dd b
      i = s * 4 + a
      old = q U.! i
      q' = U.modify (\m -> UM.write m i (old + alphaR * ((if done then r else r + gammaR * maxOf (U.toList (U.slice (s2 * 4) 4 q))) - old))) q
      len = agLen ag + 1
      ag' = ag { agQ = q', agShaped = agShaped ag + r, agGoals = agGoals ag + (if done then 1 else 0) }
  pure $ if done || len >= maxLen then ag' { agLen = 0, agS = startS } else ag' { agLen = len, agS = s2 }

learnOnce :: World -> World
learnOnce w = w { wAgents = ags', wRng = r, wTick = t }
  where
    (ags, r) = runWith (wRng w) (mapM (agentStep gw0 (wDist w) (wBonus w)) (wAgents w))
    t = wTick w + 1
    ags' = if t `mod` 20 == 0 then [ a { agHist = pushCap 100000 (fromIntegral (agGoals a)) (agHist a) } | a <- ags ] else ags

step :: Env -> Float -> World -> IO World
step env _ w
  | wPaused w = pure w
  | wSpeed w == 0 = pure (if envFrame env `mod` 4 == 0 then learnOnce w else w)
  | wSpeed w == 1 = pure (idx w (iterate learnOnce w) 5)
  | otherwise = sliced 6 (const True) learnOnce w

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Reward design and hacking   " ++ show (wTick w) ++ " steps") 40 42 22 (grayC 240)
  , say ("F speed (" ++ idx "" speedNames (wSpeed w) ++ ")   Space pause   R restart   Up/Down bonus " ++ fmtN 2 (wBonus w) ++ "   (gamma = " ++ prec3 gammaR ++ ")") 40 68 15 (rgbC 170 176 210) ]
  ++ concat (zipWith3 agentPic [0 :: Int ..] (wAgents w) (wPlans w))
  ++ [ plotAxes g "steps (x 20)" "goals reached (total)" [0, x1 g] [0, jsRound (y1 g)] ]
  ++ [ drawCurve g (agHist ag) (kindCol (agKind ag)) 2.5 0 | ag <- wAgents w ]
  ++ [ sayWrap "Pink (naive shaping) collects lots of reward but hardly goes to the goal = it learned something the designer did not intend" (pX g) (pY g + pH g + 30) (pW g) 14 (rgbC 170 176 210)
     , drawSelfTest env (wTest w) ]
  where
    gw = gw0
    bw = (envW env - 80) / 3
    cs = min (bw / fromIntegral (gwW gw + 1)) (envH env * 0.42 / fromIntegral (gwH gw))
    gy = 120
    agentPic k ag plan =
      let gx = 40 + fromIntegral k * bw
          off = fromIntegral (k - 1) * 3
          ctr s = let (x, y) = cellXY gw s gx gy cs in (x + cs / 2 + off, y + cs / 2 + off)
          path = rollout gw plan
          ok = cellOf gw (idx 0 path (length path - 1)) == 'G'
          (ax, ay) = cellXY gw (agS ag) gx gy cs
          col = kindCol (agKind ag)
      in [ say (kindName (wBonus w) (agKind ag)) gx (gy - 10) 14 col
         , drawCells gw gx gy cs False ]
         ++ [ Color (rgbaC 255 255 255 110) (drawArrowIn (x + cs / 2) (y + cs / 2) a (cs * 0.25) 1.5)
            | s <- [0 .. gwW gw * gwH gw - 1], let (x, y) = cellXY gw s gx gy cs, a <- plActs plan V.! s ]
         ++ [ Color col (polyline 4 (map ctr path))
            , Color white (circleAt (ax + cs / 2) (ay + cs / 2) (cs * 0.45))
            , say ("Optimal policy path: " ++ (if ok then show (length path - 1) ++ " steps to the goal" else "never reaches the goal")) gx (gy + fromIntegral (gwH gw) * cs + 20) 14 (grayC 220)
            , say ("Learning: goal " ++ show (agGoals ag) ++ " times, reward " ++ fmtN 1 (agShaped ag)) gx (gy + fromIntegral (gwH gw) * cs + 40) 14 (grayC 220) ]
    gTop = gy + fromIntegral (gwH gw) * cs + 110
    a0 = headOr (newAgent KOrig) (wAgents w)
    g = G 70 gTop (envW env - 140) (envH env - gTop - 80) 0 (fromIntegral (max 100 (Seq.length (agHist a0))))
          0 (fromIntegral (maximum (10 : map agGoals (wAgents w))))

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char ch) Down _ _
    | ch `elem` "fF" -> w { wSpeed = (wSpeed w + 1) `mod` 3 }
    | ch `elem` "rR" -> rebuild w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild w { wBonus = min 0.5 (jsRound ((wBonus w + 0.01) * 100) / 100) }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild w { wBonus = max 0 (jsRound ((wBonus w - 0.01) * 100) / 100) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("potential shaping: the sets of optimal actions = the original reward (all " ++ show (length cells) ++ " cells)", all (\s -> actsOf pa s == actsOf pp s) cells)
  , ("potential shaping values = original values - Phi (only shifted)", all (\s -> near (plV pp U.! s) (plV pa U.! s - phi dd s 0.1) 1e-8) cells)
  , ("naive shaping optimal policy never reaches the goal (even in 100 steps); original reward takes the shortest " ++ show (round lS :: Int) ++ " steps"
    , cellOf g (idx 0 pathN (length pathN - 1)) /= 'G' && fromIntegral (length pathA - 1) == lS)
  , ("naive shaping changes the optimal actions in some cells (" ++ show (length changed) ++ " cells)", not (null changed))
  , ("Q-learning 20000 steps: naive shaping reaches the goal only " ++ show (agGoals an) ++ " times and most of its reward " ++ show (jsRoundI (agShaped an))
     ++ " is the 'closer' bonus. Potential shaping reaches it " ++ show (agGoals ap) ++ " times"
    , agGoals an * 3 < agGoals ap && agShaped an - fromIntegral (agGoals an) > 10 * fromIntegral (agGoals an)) ]
  where
    g = gw0
    dd = bfsDist g goalS
    pa = solve g dd KOrig 0.1
    pn = solve g dd KNaive 0.1
    pp = solve g dd KPot 0.1
    actsOf p s = plActs p V.! s
    cells = [ s | s <- [0 .. gwW g * gwH g - 1], not (null (actsOf pa s)) ]
    pathN = rollout g pn
    pathA = rollout g pa
    lS = dd U.! startS
    changed = [ s | s <- cells, actsOf pa s /= actsOf pn s ]
    -- Q 学習でも：素朴な整形は報酬を多くもらうのに宝に行かない
    (an, ap) = withSeed 5 (go (0 :: Int) (newAgent KNaive) (newAgent KPot))
    go t a b | t >= 20000 = pure (a, b)
             | otherwise = do a' <- agentStep g dd 0.1 a; b' <- agentStep g dd 0.1 b; go (t + 1) a' b'

-- ---------- やってみよう ----------
-- 1. ↓ でごほうびを小さくしていくと、素朴な整形でも宝に向かうようになるのはどこから？（γ も関係する）
-- 2. 素朴な整形で「離れたら −0.1」も足すと？ それはポテンシャル整形とどうちがう？
-- 3. Φ をでたらめな値にしても、ポテンシャル整形なら最適方策は変わらない。試してみよう
-- 4. ゲームや掃除ロボットで「報酬のずる」が起きそうな例を考えよう
-- 5. 人がほめる・しかる報酬（14・15）でも、同じことが起きないかな？

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
