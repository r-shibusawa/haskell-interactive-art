-- ============================================
-- 63-00 多腕バンディット ─ 当たりの確率が隠れた 5 台のスロットマシン
-- まず自分で引いて「どれが当たりやすいか」を探す。次に ε-greedy・UCB1・トンプソン抽出に任せて、
-- 後悔（regret = いちばん良い台を引き続けた場合との差の合計）がどう増えるかを比べる。探索と活用のジレンマ
-- 操作：クリック or 1〜5 自分で引く / スペース 3 つのアルゴリズムの競争をやり直す / F 速さ / H 確率を見せる / R 台を入れかえ
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

probs0 :: [Double]
probs0 = [0.1, 0.2, 0.3, 0.4, 0.9]     -- ★ 当たりの確率（はじめはシャッフルして隠す）

horizon :: Int
horizon = 2000                         -- ★ 競争で引く回数

epsB :: Double
epsB = 0.1                             -- ★ ε-greedy の ε（10% はでたらめに引く）

data Kind = KEps | KUcb | KTs | KMe deriving (Eq)

-- [種類, 名前, 色]
kinds :: [(Kind, String, Color)]
kinds = [(KEps, "eps-greedy (eps = 0.1)", cyanC), (KUcb, "UCB1", goldC), (KTs, "Thompson sampling", pinkC)]

-- 1 人のプレイヤー：台ごとの引いた回数 n と当たりの数 s、合計の後悔、後悔の曲線
data Agent = Agent
  { agKind   :: !Kind
  , agN      :: !(U.Vector Int)
  , agS      :: !(U.Vector Int)
  , agT      :: !Int
  , agRegret :: !Double
  , agReward :: !Int
  , agCurve  :: !(Seq.Seq Double) }

data World = World
  { wProbs   :: ![Double]
  , wAgents  :: ![Agent]
  , wMe      :: !Agent
  , wLastWin :: !Bool
  , wFast    :: !Bool
  , wReveal  :: !Bool
  , wFlash   :: ![Double]
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "63-00 Multi-armed bandit" (setup []))
  { skBack  = bgC
  , skStart = \_ -> do
      let rs = selfTest
      printSelfTest rs
      pure (setup rs)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- seedRng(7) してから台をシャッフル
setup :: [(String, Bool)] -> World
setup rs = restart (World ps [] (makeAgent KMe 5) False False False [] g rs)
  where (ps, g) = runWith (seedRng 7) (shuffle probs0)

-- フィッシャー-イェーツのシャッフル（p5 と同じ順に乱数を引く）
shuffle :: [Double] -> R [Double]
shuffle xs0 = go (length xs0 - 1) xs0
  where
    go i xs | i <= 0 = pure xs
            | otherwise = do
                u <- rnd
                let j = floor (u * fromIntegral (i + 1))
                    a = idx 0 xs i
                    b = idx 0 xs j
                go (i - 1) [ if k == i then b else if k == j then a else v | (k, v) <- zip [0 ..] xs ]

restart :: World -> World
restart w = w { wAgents = [ makeAgent k (length (wProbs w)) | (k, _, _) <- kinds ], wMe = makeAgent KMe (length (wProbs w))
              , wFlash = map (const 0) (wProbs w) }

makeAgent :: Kind -> Int -> Agent
makeAgent kind k = Agent kind (U.replicate k 0) (U.replicate k 0) 0 0 0 (Seq.singleton 0)

-- ★ どの台を引くか（ここがアルゴリズムの中身）
chooseArm :: Agent -> R Int
chooseArm ag = case agKind ag of
  KUcb                                   -- 平均 + 自信のなさ √(2 ln t / n)。まだ引いていない台が先
    | Just i <- U.findIndex (== 0) (agN ag) -> pure i
    | otherwise -> argmaxR [ s / n + sqrt (2 * log (fromIntegral (agT ag) + 1) / n) | (n, s) <- ns ]
  KTs ->                                 -- トンプソン抽出：信念 Beta(1 + 当たり, 1 + はずれ) から 1 つずつ引いて、いちばん大きい台
    mapM (\(n, s) -> betaSample (1 + s) (1 + n - s)) ns >>= argmaxR
  _ -> do                                -- ε の確率で探索、それ以外は平均がいちばん高い台（活用）
    u <- rnd
    if u < epsB then (\v -> floor (v * fromIntegral k)) <$> rnd
    else argmaxR [ if n > 0 then s / n else 0 | (n, s) <- ns ]
  where
    k = U.length (agN ag)
    ns = zip (map fromIntegral (U.toList (agN ag))) (map fromIntegral (U.toList (agS ag))) :: [(Double, Double)]

-- 台 arm を引く。(当たり 1 / はずれ 0, 新しいプレイヤー)
pull :: [Double] -> Int -> Agent -> R (Int, Agent)
pull pr arm ag = do
  u <- rnd
  let r = if u < idx 0 pr arm then 1 else 0
      reg = agRegret ag + maxOf pr - idx 0 pr arm   -- 後悔は「期待値の差」で数える（運の良し悪しを除く）
      bump v = U.imap (\i x -> if i == arm then x + 1 else x) v
      addS v = U.imap (\i x -> if i == arm then x + r else x) v
  pure (r, ag { agN = bump (agN ag), agS = addS (agS ag), agT = agT ag + 1, agReward = agReward ag + r
              , agRegret = reg, agCurve = pushCap 200000 reg (agCurve ag) })

-- 3 つのアルゴリズムを 1 回ずつ引かせる
round1 :: World -> World
round1 w = w { wAgents = ags, wRng = g }
  where
    (ags, g) = runWith (wRng w) (mapM one (wAgents w))
    one ag | agT ag < horizon = do a <- chooseArm ag; snd <$> pull (wProbs w) a ag
           | otherwise = pure ag

-- 競争を進める（1 フレームの計算は数ミリ秒まで → 60fps のまま）
step :: Env -> Float -> World -> IO World
step _ _ w = do
  w' <- sliced (if wFast w then 6 else 0) (\v -> agT (headOr (makeAgent KMe 1) (wAgents v)) < horizon) round1 w
  pure w' { wFlash = map (* 0.9) (wFlash w') }

machineW :: Env -> Float
machineW env = min 120 ((envW env * 0.5 - 60) / 5 - 14)

machineBox :: Env -> Int -> (Float, Float, Float, Float)
machineBox env i = (40 + fromIntegral i * (machineW env + 14), 100, machineW env, 170)

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Multi-armed bandit: explore vs exploit" 40 42 22 (grayC 240)
  , say ("Click or 1-5 pull yourself   Space restart race   F speed (" ++ (if wFast w then "fast" else "normal") ++ ")   H show p   R new machines") 40 68 15 (rgbC 170 176 210) ]
  -- ---- スロットマシン（自分で引く）
  ++ concat [ let (x, y0, mw, mh) = machineBox env i
                  cx = x + mw / 2
              in [ Color (rgbC 30 34 70) (rectTL x y0 mw mh)
                 , Color (withA ((120 + 135 * f2 fl) / 255) goldC) (frameTL 2 x y0 mw mh)
                 , Color (if fl > 0 && wLastWin w then goldC else rgbC 60 66 110) (rectTL (x + 12) (y0 + 20) (mw - 24) 50)
                 , sayM (show (i + 1)) cx (y0 + 45) 26 white
                 , sayM ("pulled " ++ show (agN me U.! i)) cx (y0 + 92) 14 (rgbC 200 205 235)
                 , sayM ("wins " ++ show (agS me U.! i)) cx (y0 + 112) 14 (rgbC 200 205 235)
                 , sayM ("mean " ++ (if agN me U.! i > 0 then fmtN 2 (fromIntegral (agS me U.! i) / fromIntegral (agN me U.! i)) else "?")) cx (y0 + 132) 14 (rgbC 200 205 235)
                 , sayM (if wReveal w then "p = " ++ prec3 p else "p = ?") cx (y0 + 154) 14 limeC ]
            | (i, p, fl) <- zip3 [0 ..] (wProbs w) (wFlash w) ]
  ++ [ say ("You: " ++ show (agT me) ++ " pulls, " ++ show (agReward me) ++ " wins   regret " ++ fmtN 1 (agRegret me)
            ++ " (best machine would expect " ++ fmtN 1 (fromIntegral (agT me) * maxOf (wProbs w)) ++ ")") 40 300 16 (grayC 230) ]
  -- ---- 後悔のグラフ（3 つのアルゴリズム + あなた）
  ++ [ plotAxes g "pulls t" "cumulative regret" [0, 500, 1000, 1500, 2000] [0, jsRound (y1 g / 2), jsRound (y1 g)] ]
  ++ [ drawCurve g (agCurve ag) c 2.5 0 | (ag, (_, _, c)) <- zip (wAgents w) kinds ]
  ++ [ drawCurve g (agCurve me) (grayC 235) 2 0 | agT me > 1 ]
  ++ [ legend (pX g + 12) (pY g + 16) ([ (c, nm) | (_, nm, c) <- kinds ] ++ [(grayC 235, "you")]) ]
  -- ---- それぞれが各台を何回引いたか
  ++ concat [ let x = 40 + fromIntegral k * (bw + 20)
                  cw = bw / fromIntegral nArms
              in say (nm ++ "  regret " ++ fmtN 1 (agRegret ag) ++ "  wins " ++ show (agReward ag) ++ " / " ++ show (agT ag)) x (by - 10) 15 c
                 : concat [ [ Color (withA (200 / 255) c) (rectTL (x + fromIntegral i * cw + 4) (by + bh - h) (cw - 8) h)
                            , sayC (show (i + 1) ++ (if wReveal w then " (" ++ prec3 (idx 0 (wProbs w) i) ++ ")" else "")) (x + fromIntegral i * cw + cw / 2) (by + bh + 18) 14 (rgbC 190 195 225) ]
                          | i <- [0 .. nArms - 1], let h = fromIntegral (agN ag U.! i) / fromIntegral (max 1 (agT ag)) * bh ]
            | (k, ag, (_, nm, c)) <- zip3 [0 :: Int ..] (wAgents w) kinds ]
  ++ [ say "Bar = share of pulls per machine. Focusing on the good machine makes regret grow more slowly" 40 (envH env - 40) 14 (rgbC 150 156 190)
     , drawSelfTest env (wTest w) ]
  where
    me = wMe w
    nArms = length (wProbs w)
    g0 = G (envW env * 0.55) 110 (envW env * 0.4) (envH env * 0.36) 0 (fromIntegral horizon) 0 0
    g = g0 { y1 = maximum (20 : map agRegret (wAgents w)) * 1.1 }
    by = pY g + pH g + 90
    bh = max 60 (envH env - by - 80)
    bw = (envW env - 120) / 3

-- 自分で台 i を引く
pullMine :: Int -> World -> World
pullMine i w
  | i < 0 || i >= length (wProbs w) = w
  | otherwise = let ((r, me'), g) = runWith (wRng w) (pull (wProbs w) i (wMe w))
                in w { wMe = me', wLastWin = r == 1, wRng = g, wFlash = [ if k == i then 1 else f | (k, f) <- zip [0 ..] (wFlash w) ] }

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    foldl' (\v i -> if inBox (toP5 env pos) (machineBox env i) then pullMine i v else v) w [0 .. length (wProbs w) - 1]
  EventKey (Char ch) Down _ _
    | ch >= '1' && ch <= '5' -> pullMine (fromEnum ch - fromEnum '1') w
    | ch `elem` "fF" -> w { wFast = not (wFast w) }
    | ch `elem` "hH" -> w { wReveal = not (wReveal w) }
    | ch `elem` "rR" -> let (ps, g) = runWith (wRng w) (mapM (const (do u <- rnd; pure (jsRound ((0.05 + u * 0.9) * 100) / 100))) (wProbs w))
                        in restart w { wProbs = ps, wRng = g }
  EventKey (SpecialKey KeySpace) Down _ _ -> restart w
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("UCB1 regret < eps-greedy (eps = 0.1): mean of 20 x 2000 pulls " ++ fmtN 1 rUcb ++ " < " ++ fmtN 1 rEps, rUcb < rEps)
  , ("Thompson sampling regret < eps-greedy: " ++ fmtN 1 rTs ++ " < " ++ fmtN 1 rEps, rTs < rEps)
  , ("pulls add up to 2000, regret >= 0, UCB1 tries every machine once in the first 5 pulls", and [ a | (_, _, a, _) <- runs ] && and [ b | (_, _, _, b) <- runs ])
  , ("Beta(3, 7) samples: mean " ++ fmtN 3 m ++ " ~ 0.3, variance " ++ fmtN 4 v ++ " ~ 0.0191", near m 0.3 0.01 && near v (21 / 1100) 0.0015 && all (\x -> x > 0 && x < 1) xs)
  , ("pulling machine 5 (p = 0.9) 4000 times: win rate ~ 0.9, regret 0", near (fromIntegral hits / 4000) 0.9 0.02 && agRegret ag5 == 0) ]
  where
    base = [0.1, 0.2, 0.3, 0.4, 0.9]
    nRuns = 20 :: Int
    runs = [ withSeed (1000 + 7 * fromIntegral r) (race kind) | r <- [0 .. nRuns - 1], kind <- [KEps, KUcb, KTs] ]   -- 3 つとも同じ種で比べる
    race kind = do
      let go !t ag okF | t >= horizon = pure (ag, okF)
                       | otherwise = do
                           a <- chooseArm ag
                           (_, ag') <- pull base a ag
                           go (t + 1) ag' (okF && not (kind == KUcb && t < 5 && a /= t))
      (ag, okF) <- go 0 (makeAgent kind 5) True
      pure (kind, agRegret ag, U.sum (agN ag) == horizon && agRegret ag >= 0, okF)
    regOf k = sum [ r / fromIntegral nRuns | (k', r, _, _) <- runs, k' == k ]
    rEps = regOf KEps
    rUcb = regOf KUcb
    rTs = regOf KTs
    xs = withSeed 3 (replicateM 20000 (betaSample 3 7))
    m = meanOf xs
    v = meanOf (map (\x -> (x - m) * (x - m)) xs)
    (hits, ag5) = withSeed 5 (go5 0 (0 :: Int) (makeAgent KEps 5))
    go5 !t !h ag | t >= (4000 :: Int) = pure (h, ag)
                 | otherwise = do (r, ag') <- pull base 4 ag; go5 (t + 1) (h + r) ag'

-- ---------- やってみよう ----------
-- 1. H を押さずに 30 回だけ引いて、どの台がいちばん良いか当てよう。何回引けば自信がもてる？
-- 2. epsB を 0.01 や 0.3 にすると、ε-greedy の後悔のグラフはどう変わる？（探索しすぎ・しなさすぎ）
-- 3. probs0 を [0.5, 0.55, 0.6, 0.62, 0.65] のように近くすると、UCB1 と ε-greedy の順位は？（差が小さいと見分けに時間がかかる）
-- 4. トンプソン抽出の Beta(1 + 当たり, 1 + はずれ) は「信念」。台ごとの山の形を描いてみよう
-- 5. 当たりの確率が途中で入れかわったら？（14 で、好みが変わる人に合わせる工夫をする）

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
-- ---- ベータ分布からのサンプル（トンプソン抽出用）：ガンマ分布 2 つの比
gammaSample :: Double -> R Double
gammaSample k
  | k < 1 = do g <- gammaSample (k + 1); u <- rnd; pure (g * (1 - u) ** (1 / k))
  | otherwise = loop
  where
    d = k - 1 / 3
    c = 1 / sqrt (9 * d)
    loop = do                                 -- マルサグリア-ツァンの方法
      x <- randn
      let v0 = 1 + c * x
      if v0 <= 0 then loop else do
        let v = v0 * v0 * v0
        u <- rnd
        if log (1 - u) < 0.5 * x * x + d - d * v + d * log v then pure (d * v) else loop

betaSample :: Double -> Double -> R Double
betaSample a b = do x <- gammaSample a; y <- gammaSample b; pure (x / (x + y))
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
