-- ============================================
-- 63-14 耳で報酬を与える 🔊 ─ エージェントが 6 つのフレーズ（4〜8 音）から 1 つ選んで鳴らす。
-- 聞いたあなたが「好き」（+1）か「きらい」（−1）を返すと、それが報酬。多腕バンディット（00）と同じしくみで好みを学ぶ。
-- 人の好みは途中で変わる → 平均（1/n）ではなく、最近の報酬を重く見る「一定の学習率 α」で更新すると、変化についていける
-- 操作：クリック 音を出す・好き/きらい（↑ 好き ↓ きらい でも）/ スペース もう一度聞く / A 自動の聞き手（好みが 150 回ごとに変わる）/ N フレーズを作り直す
-- 音量は小さめから。全体の音量 master は 0.2 以下
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, void, when)
import Data.Bits (shiftR, xor, (.|.))
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nPhrases :: Int
nPhrases = 6                           -- ★ フレーズの数

alphaC, epsP :: Double
alphaC = 0.3                           -- ★ 一定の学習率（最近を重く見る）
epsP = 0.15                            -- ★ 探索の割合

scaleNotes :: [Int]
scaleNotes = [0, 2, 4, 7, 9, 12, 14, 16]   -- ★ 音階（ペンタトニック、ド = 0）

baseMidi :: Int
baseMidi = 60                          -- ★ いちばん低い音（60 = ド）

period :: Int
period = 150                           -- 自動の聞き手の好みが変わる間隔

-- フレーズ：音（midi 番号, 長さ 秒）の列と波形
data Phrase = Phrase { phNotes :: [(Int, Double)], phWave :: Wave }

-- エージェント：一定の α か標本平均か、推定値 q、回数 n
data Ag = Ag { agConst :: !Bool, agQ :: !(U.Vector Double), agN :: !(U.Vector Int) }

-- 自動の聞き手：好きなフレーズ fav、回数 t、2 つのエージェントと「好きなものを選べたか」の記録
data Sim = Sim { sFav :: !Int, sT :: !Int, sAgents :: ![Ag], sPicks :: ![Seq.Seq Double] }

data World = World
  { wPhrases :: ![Phrase]
  , wAgent   :: !Ag
  , wCurrent :: !Int
  , wWaiting :: !Bool
  , wLog     :: !(Seq.Seq (Int, Int))   -- (フレーズ, 報酬) の最近 60 回
  , wAuto    :: !Bool
  , wSim     :: !Sim
  , wPlayAt  :: !Float                  -- 鳴らしはじめた時刻（秒）
  , wNextAt  :: !(Maybe Float)          -- 次のフレーズを鳴らす時刻（setTimeout の代わり）
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "63-14 Reward by ear" (setup []))
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
setup rs = World ps (makeAgent True) (-1) False Seq.empty False makeSim (-100) Nothing g rs
  where (ps, g) = runWith (seedRng 14) makePhrases

-- 4〜8 音のフレーズ：音の高さ（音階の番号）と長さ
makePhrases :: R [Phrase]
makePhrases = mapM one [0 .. nPhrases - 1]
  where
    one k = do
      n <- rndInt 4 8
      deg0 <- rndInt 0 4
      let go 0 _ acc = pure (reverse acc)
          go i deg acc = do
            dd <- rndInt (-2) 2
            let deg' = max 0 (min (length scaleNotes - 1) (deg + dd))
            di <- rndInt 0 3
            go (i - 1 :: Int) deg' ((baseMidi + idx 0 scaleNotes deg', idx 0.3 [0.15, 0.3, 0.3, 0.45] di) : acc)
      notes <- go n deg0 []
      pure (Phrase notes (idx Triangle [Triangle, Sine, Square] (k `mod` 3)))

makeAgent :: Bool -> Ag
makeAgent c = Ag c (U.replicate nPhrases 0) (U.replicate nPhrases 0)

choosePhrase :: Ag -> R Int
choosePhrase ag = epsGreedy (U.toList (agQ ag)) epsP

-- ★ 更新：一定の α なら Q ← Q + α (r − Q)（最近の報酬ほど重い）。標本平均なら α = 1/n（全部を同じ重さで平均）
learn :: Ag -> Int -> Double -> Ag
learn ag k r = ag { agN = n', agQ = U.imap (\i q -> if i == k then q + al * (r - q) else q) (agQ ag) }
  where n' = U.imap (\i v -> if i == k then v + 1 else v) (agN ag)
        al = if agConst ag then alphaC else 1 / fromIntegral (n' U.! k)

-- 自動の聞き手：好きなフレーズは 90%、ほかは 20% で「好き」。period 回ごとに好みが変わる
makeSim :: Sim
makeSim = Sim 0 0 [makeAgent True, makeAgent False] [Seq.empty, Seq.empty]

simListen :: Sim -> Int -> R Double
simListen s k = do u <- rnd; pure (if u < (if k == sFav s then 0.9 else 0.2) then 1 else -1)

simStep :: Sim -> R Sim
simStep s0 = do
  fav <- if sT s0 > 0 && sT s0 `mod` period == 0
           then (\u -> (sFav s0 + 1 + floor (u * fromIntegral (nPhrases - 1))) `mod` nPhrases) <$> rnd
           else pure (sFav s0)
  let s = s0 { sFav = fav }
      one ag = do k <- choosePhrase ag; r <- simListen s k; pure (learn ag k r, if k == fav then 1 else 0)
  res <- mapM one (sAgents s)
  pure s { sAgents = map fst res, sPicks = zipWith (\p (_, v) -> p Seq.|> v) (sPicks s) res, sT = sT s + 1 }

-- フレーズ k を鳴らす（音を順に予約する）
playPhrase :: Env -> Int -> World -> IO World
playPhrase env k w = case drop k (wPhrases w) of
  (ph : _) | k >= 0 -> do
    let starts = scanl (+) 0.05 (map snd (phNotes ph))
    forM_ (zip starts (phNotes ph)) $ \(t, (m, d)) -> playNoteAt t (midiHz (fromIntegral m)) (d * 0.95) (phWave ph)
    pure w { wPlayAt = envTime env }
  _ -> pure w

nextTrial :: Env -> World -> IO World
nextTrial env w = do
  let (k, g) = runWith (wRng w) (choosePhrase (wAgent w))
  playPhrase env k w { wCurrent = k, wWaiting = True, wRng = g }

feedback :: Env -> Double -> World -> World
feedback env r w
  | not (wWaiting w) = w
  | otherwise = w { wAgent = learn (wAgent w) (wCurrent w) r, wWaiting = False
                  , wLog = let s = wLog w Seq.|> (wCurrent w, round r) in if Seq.length s > 60 then Seq.drop 1 s else s
                  , wNextAt = Just (envTime env + 0.4) }

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  w1 <- case wNextAt w0 of
    Just t | envTime env >= t -> nextTrial env w0 { wNextAt = Nothing }
    _ -> pure w0
  if wAuto w1
    then sliced 2 (\v -> sT (wSim v) < 3000) (\v -> if sT (wSim v) >= 3000 then v else let (s, g) = runWith (wRng v) (simStep (wSim v)) in v { wSim = s, wRng = g }) w1
    else pure w1

cardW :: Env -> Float
cardW env = min 180 ((envW env - 80) / fromIntegral nPhrases - 12)

cardBox :: Env -> Int -> (Float, Float, Float, Float)
cardBox env k = (40 + fromIntegral k * (cardW env + 12), 100, cardW env, 150)

-- (左上 x, y, 幅, 高さ, 報酬)
buttons :: [(Float, Float, Float, Float, Int)]
buttons = [(40, 290, 200, 60, 1), (260, 290, 200, 60, -1), (480, 290, 200, 60, 0)]

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Reward by ear: a bandit that picks phrases" ++ (if wAuto w then " (auto listener)" else "")) 40 42 22 (grayC 240)
  , say "Click: play / like / dislike (Up like, Down dislike)   Space again   A auto listener   N new phrases" 40 68 15 (rgbC 170 176 210) ]
  -- ---- フレーズのカード（ピアノロール）と推定値
  ++ concat [ let (x, y0, cw, ch) = cardBox env k
                  on = k == wCurrent w && not (wAuto w)
                  playing = on && envTime env - wPlayAt w < 2.5
                  tot = sum (map snd (phNotes ph))
                  xs = scanl (+) (x + 10) [ (cw - 20) * f2 (d / tot) | (_, d) <- phNotes ph ]
                  q = agQ ag U.! k
              in [ Color (if on then rgbC 50 56 100 else rgbC 28 32 64) (rectTL x y0 cw ch)
                 , Color (if on then goldC else rgbC 70 76 120) (frameTL (if on then 3 else 1) x y0 cw ch) ]
                 ++ [ Color (if playing then goldC else cyanC) (rectTL tx (y0 + 100 - fromIntegral (m - baseMidi) * 4) ((cw - 20) * f2 (d / tot) - 2) 8)
                    | (tx, (m, d)) <- zip xs (phNotes ph) ]
                 ++ [ say ("Phrase " ++ show (k + 1) ++ " (" ++ show (length (phNotes ph)) ++ " notes)") (x + 8) (y0 + 22) 14 (grayC 230)
                    , Color (if q >= 0 then limeC else pinkC) (rectTL (x + cw / 2) (y0 + 120) (f2 q * (cw / 2 - 10)) 10)
                    , say ("Q=" ++ fmtN 2 q ++ "  n=" ++ show (agN ag U.! k)) (x + 8) (y0 + 146) 14 (rgbC 200 205 235) ]
            | (k, ph) <- zip [0 ..] (wPhrases w) ]
  ++ (if not (wAuto w) then manual else auto)
  ++ [ drawSelfTest env (wTest w) ]
  where
    ag = if wAuto w then headOr (makeAgent True) (sAgents (wSim w)) else wAgent w
    -- ---- 好き / きらい ボタンと報酬の履歴
    manual =
      concat [ [ Color (if r > 0 then withA (if wWaiting w then 1 else 90 / 255) limeC else if r < 0 then withA (if wWaiting w then 1 else 90 / 255) pinkC else cyanC) (rectTL x y bw bh)
               , sayM s (x + bw / 2) (y + bh / 2) 20 (grayC 20) ]
             | (x, y, bw, bh, r) <- buttons
             , let s = if r > 0 then "Like (Up)" else if r < 0 then "Dislike (Down)" else if wCurrent w < 0 then "Start >" else "Again" ]
      ++ [ say (if wCurrent w < 0 then "Click Start first (it plays sound - keep volume low)"
                else if wWaiting w then "How was phrase " ++ show (wCurrent w + 1) ++ "?" else "Choosing the next phrase...") 700 326 16 (grayC 230)
         , say "Rewards you gave (newest on the right)" 40 400 16 (grayC 230) ]
      ++ concat [ [ Color (if r > 0 then limeC else pinkC) (rectTL (40 + fromIntegral i * 20) 414 18 18)
                  , sayM (show (k + 1)) (49 + fromIntegral i * 20) 423 14 (grayC 20) ]
                | (i, (k, r)) <- zip [0 :: Int ..] (toList (wLog w)) ]
      ++ [ sayWrap "Try changing your taste midway (dislike the phrase you liked). With a constant alpha, Q drops after a few tries and the agent starts trying others" 40 445 (envW env - 80) 14 (rgbC 170 176 210) ]
    -- ---- 自動の聞き手：好きなフレーズを選べた割合（直近 50 回）
    g = G 70 310 (envW env - 140) (envH env - 310 - 80) 0 3000 0 1
    auto =
      [ plotAxes g "plays (the taste changes every 150)" "share of picks that were the favourite (last 50)" [0, 750, 1500, 2250, 3000] [0, 0.5, 1] ]
      ++ [ Color (rgbC 90 96 140) (lineW 1 (gX g t, pY g) (gX g t, pY g + pH g)) | t <- [150, 300 .. 2850] ]
      ++ [ drawCurve g (smoothArr p 50) c 2.5 0 | (p, c) <- zip (sPicks (wSim w)) [goldC, violetC] ]
      ++ [ legend (pX g + 12) (pY g + 16) [(goldC, "constant alpha = 0.3 (recent counts more)"), (violetC, "sample mean 1/n (old counts the same)")] ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> do
    let p = toP5 env pos
    case [ r | not (wAuto w), (x, y, bw, bh, r) <- buttons, inBox p (x, y, bw, bh) ] of
      (r : _) | r == 0 -> if wCurrent w < 0 then nextTrial env w else playPhrase env (wCurrent w) w
              | otherwise -> pure (feedback env (fromIntegral r) w)
      [] -> foldM (\v k -> if inBox p (cardBox env k) then playPhrase env k v else pure v) w [0 .. nPhrases - 1]
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (feedback env 1 w)
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (feedback env (-1) w)
  EventKey (SpecialKey KeySpace) Down _ _ -> if wCurrent w < 0 then nextTrial env w else playPhrase env (wCurrent w) w
  EventKey (Char ch) Down _ _
    | ch `elem` "aA" -> pure (if wAuto w then w { wAuto = False } else w { wAuto = True, wRng = seedRng 140, wSim = makeSim })
    | ch `elem` "nN" -> let (ps, g) = runWith (wRng w) makePhrases
                        in pure w { wPhrases = ps, wRng = g, wAgent = makeAgent True, wLog = Seq.empty, wCurrent = -1, wWaiting = False }
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("overall volume master <= 0.2", master <= 0.2)
  , ("there are " ++ show nPhrases ++ " phrases, each with 4-8 notes, all pitches in the scale"
    , length ps == nPhrases && all (\p -> length (phNotes p) >= 4 && length (phNotes p) <= 8 && all (\(m, _) -> (m - baseMidi) `elem` scaleNotes) (phNotes p)) ps)
  , ("update rule: constant alpha = 0.3 gives 0 -> 0.3 -> 0.3 + 0.3(-1 - 0.3) = -0.09, sample mean gives (1 - 1)/2 = 0"
    , near (agQ a1 U.! 2) (-0.09) 1e-12 && near (agQ a2 U.! 2) 0 1e-12)
  , ("before the taste changes both pick the favourite (" ++ pct (idx 0 before 0) ++ "%, " ++ pct (idx 0 before 1) ++ "%)", idx 0 before 0 > 0.6 && idx 0 before 1 > 0.6)
  , ("after it changes, constant alpha (" ++ pct (idx 0 after 0) ++ "%) follows better than the sample mean (" ++ pct (idx 0 after 1) ++ "%)", idx 0 after 0 > idx 0 after 1 + 0.1) ]
  where
    ps = withSeed 1 makePhrases
    a1 = learn (learn (makeAgent True) 2 1) 2 (-1)
    a2 = learn (learn (makeAgent False) 2 1) 2 (-1)
    sims = [ withSeed seed (foldM (\s _ -> simStep s) makeSim [1 .. 600 :: Int]) | seed <- [1 .. 6] ]
    sliceMean a b sq = meanOf (toList (Seq.take (b - a) (Seq.drop a sq)))
    before = [ sum [ sliceMean 100 150 (idx Seq.empty (sPicks s) i) / 6 | s <- sims ] | i <- [0, 1] ]
    after = [ sum [ sliceMean 150 600 (idx Seq.empty (sPicks s) i) / 6 | s <- sims ] | i <- [0, 1] ]
    pct v = show (jsRoundI (100 * v))

-- ---------- やってみよう ----------
-- 1. 好きなフレーズを決めて、エージェントが何回でそれを選ぶようになるか数えよう
-- 2. alphaC を 0.05 にすると、好みの変化についていくのはどうなる？ 0.8 では？
-- 3. epsP を 0 にすると、ほかのフレーズを試さなくなる。好みが変わったらどうなる？
-- 4. 「好き」の報酬を +1、「きらい」を 0 にすると？（−1 との違い）
-- 5. scaleNotes を変えて、音階をかえてみよう（例：[0, 3, 5, 6, 7, 10, 12]）

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
-- ---- 音の道具（Art.Sound）。全体の音量 master は 0.2 以下
master :: Float
master = 0.12                          -- ★ 全体の音量（0.2 以下にしておく）

-- delay 秒あとに 1 音（周波数 freq、長さ dur 秒）
playNoteAt :: Double -> Double -> Double -> Wave -> IO ()
playNoteAt delay freq dur wave = void $ forkIO $ do
  when (delay > 0) (threadDelay (round (delay * 1e6)))
  playTone (realToFrac freq) (0.6 * min 0.2 master) (realToFrac dur) wave

midiHz :: Double -> Double
midiHz m = 440 * 2 ** ((m - 69) / 12)
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
