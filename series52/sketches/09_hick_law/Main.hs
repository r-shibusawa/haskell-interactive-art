-- ============================================
-- 52-09 反応時間とヒックの法則 ─ 選択肢が 2 倍になると、反応は一定時間ずつ遅くなる
-- 箱のどれかが光ったら、その下のキーをできるだけ速く押す。選択肢の数 N = 1, 2, 4, 8 の順にブロックで測る
-- 平均反応時間 RT と log₂N（選ぶのに必要な情報量 [bit]）に直線 RT = a + b·log₂N を当てはめる。1/b = 情報を処理する速さ [bit/秒]
-- 操作：スペースでブロック開始 / A S D F J K L ; で答える / O = シミュレーション観察者 / R = やり直し
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.Char (toLower)
import Data.List (elemIndex, foldl')
import qualified Data.Vector as V
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

keysH :: String
keysH = "asdfjkl;"                   -- 8 つのキー（左手 ASDF、右手 JKL;）

-- N ごとに使う箱
sets :: Int -> [Int]
sets 1 = [3]
sets 2 = [3, 4]
sets 4 = [2, 3, 4, 5]
sets _ = [0 .. 7]

ns :: [Int]
ns = [1, 2, 4, 8]

per :: Int
per = 8                              -- ★ 1 ブロックの試行（4 × 8 = 32 試行）

simA, simB, simSd, simErr :: Double
simA = 220                           -- ★ シミュレーション観察者：RT = a + b·log₂N（ms）
simB = 140
simSd = 40
simErr = 0.03

data Phase = Ready | Fore | Stim | Iti | Done deriving Eq

-- 1 試行の記録：N、反応時間 [ms]（早すぎなら Nothing）、正しいキーか、早すぎか
data TrialR = TrialR { tN :: !Int, tRt :: Maybe Double, tOk :: !Bool, tEarly :: !Bool }

data World = World
  { wBi      :: !Int             -- いまのブロック（0〜3）
  , wTrials  :: [TrialR]         -- 古い順
  , wPhase   :: !Phase
  , wTGo     :: !Float           -- 次に進む時刻 [秒]
  , wOnset   :: !Float           -- 光った時刻 [秒]
  , wTarget  :: !Int
  , wMsg     :: String
  , wSim     :: !Bool
  , wSimSeed :: !Word32
  , wRng     :: !Rng
  , wFit     :: Maybe Lin
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "52-09 Hick's law" world0)
  { skBack  = rgb255 30 32 40
  , skStart = \_ -> do
      printSelfTest selfTest
      r <- freshSeed
      pure world0 { wRng = r, wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = resetData False (seedRng 1) (World 0 [] Ready 0 0 (-1) "" False 1 (seedRng 1) Nothing [])

resetData :: Bool -> Rng -> World -> World
resetData sim r w = w { wSim = sim, wBi = 0, wTrials = [], wPhase = Ready, wMsg = "", wFit = Nothing, wRng = r }

log2i :: Int -> Double
log2i n = logBase 2 (fromIntegral n)

curN :: World -> Int
curN w = getAt 8 ns (min (wBi w) (length ns - 1))

blockCount :: World -> Int
blockCount w = length [ () | t <- wTrials w, tN t == getAt 0 ns (wBi w) ]

-- 次の試行：0.8〜1.6 秒待ってから光る（予測させない）
nextTrial :: Env -> World -> World
nextTrial env w
  | blockCount w >= per = let bi = wBi w + 1 in w { wBi = bi, wPhase = if bi >= length ns then Done else Ready }
  | otherwise = let (u, r') = runRand rnd (wRng w)
                in w { wPhase = Fore, wTGo = envTime env + 0.8 + f2 u * 0.8, wTarget = -1, wRng = r' }

-- キーを押した（t = 押した時刻）
respond :: Env -> Char -> World -> World
respond env k w = case elemIndex k keysH of
  Nothing -> w
  Just idx
    | wSim w -> w
    | wPhase w == Fore -> nextTrial env w { wMsg = "Too early! (pressed before the light)"
                                          , wTrials = wTrials w ++ [TrialR n Nothing False True] }
    | wPhase w /= Stim -> w
    | otherwise ->
        let rt = realToFrac (envTime env - wOnset w) * 1000 :: Double
            ok = idx == wTarget w
            w' = w { wMsg = if ok then fmt 0 (roundJ rt) ++ " ms" else "Wrong key (" ++ fmt 0 (roundJ rt) ++ " ms)"
                   , wTrials = wTrials w ++ [TrialR n (Just rt) ok False], wPhase = Iti, wTGo = envTime env + 0.5 }
        in refit w'
  where n = getAt 8 ns (wBi w)

-- N ごとの平均 RT（正解だけ）
means :: [TrialR] -> [Maybe Double]
means tr = [ let r = [ rt | t <- tr, tN t == n, tOk t, Just rt <- [tRt t] ]
             in if null r then Nothing else Just (sum r / fromIntegral (length r))
           | n <- ns ]

-- RT = a + b·log₂N
refit :: World -> World
refit w = w { wFit = linFit (map fst ps) (map snd ps) }
  where ps = [ (log2i n, m) | (n, Just m) <- zip ns (means (wTrials w)) ]

simTrials :: Int -> Rand [TrialR]
simTrials p = sequence [ one n | n <- ns, _ <- [1 .. p] ]
  where one n = do
          e <- rndNormal
          u <- rnd
          pure (TrialR n (Just (max 120 (simA + simB * log2i n + simSd * e))) (u > simErr * log2i (2 * n)) False)

runSim :: Word32 -> World -> World
runSim seed w0 = refit w { wTrials = tr, wRng = r', wBi = length ns, wPhase = Done }
  where
    w = resetData True (seedRng seed) w0
    (tr, r') = runRand (simTrials 30) (wRng w)

step :: Env -> Float -> World -> IO World
step env _ w
  | wPhase w == Fore && envTime env >= wTGo w =
      let n = getAt 8 ns (wBi w)
          (u, r') = runRand rnd (wRng w)
      in pure w { wTarget = getAt 0 (sets n) (floor (u * fromIntegral n)), wPhase = Stim, wOnset = envTime env, wRng = r' }
  | wPhase w == Iti && envTime env >= wTGo w = pure (nextTrial env w)
  | otherwise = pure w

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Space starts a block   press the key under the lit box (A S D F J K L ;) at once" 20 30 18 (gray255 235)
  , say "O simulated observer   R reset" 20 54 16 (gray255 235)
  , say "Index fingers on F and J, the other fingers on the keys next to them. Fast, but try not to make errors." 20 78 14 (gray255 170) ]
    -- ---- 8 つの箱
  ++ concat [ let x = boxX i
                  use = i `elem` sets n && wPhase w /= Done
              in [ Color (if wPhase w == Stim && i == wTarget w then rgb255 255 220 120 else gray255 (if use then 45 else 32)) (rectTL x by bw bw)
                 , Color (gray255 (if use then 150 else 60)) (frameTL 2 x by bw bw)
                 , sayC [toUpperA (getAt ' ' keysH i)] (x + bw / 2) (by + bw + 26) 18 (gray255 (if use then 220 else 80)) ]
            | i <- [0 .. 7] ]
  ++ [ say info bx (by + bw + 70) 16 (gray255 220)
     , say (wMsg w) bx (by + bw + 110) 22 (rgb255 255 210 120) ]
    -- ---- 表
  ++ [ say h (bx + cx) ty 14 (gray255 160) | (h, cx) <- zip ["N", "bit", "mean RT", "errors / early"] cols ]
  ++ concat [ [ say s (bx + cx) (ty + 22 + fromIntegral k * 20) 14 (gray255 210)
              | (s, cx) <- zip [ show n, show (round (log2i n) :: Int), maybe "-" (\v -> fmt 0 (roundJ v) ++ " ms") mi
                               , show (length [ () | t <- wTrials w, tN t == n, not (tOk t) ]) ++ " / " ++ show (length [ () | t <- wTrials w, tN t == n ]) ] cols ]
            | (k, n, mi) <- zip3 [0 :: Int ..] ns ms ]
    -- ---- RT と log₂N のグラフ
  ++ [ plotAxes g "log2 N [bit]" "Reaction time RT [ms]" [0, 1, 2, 3] [0, 250, 500, 750, 1000] ]
  ++ [ Color (rgba255 120 190 255 110) (Pictures [ circleD (gX g (log2i (tN t) + (fract' (rt * 7) - 0.5) * 0.12)) (gY g (min rt 1000)) 5
                                               | t <- wTrials w, tOk t, Just rt <- [tRt t] ]) ]
  ++ [ Color (rgb255 120 190 255) (circleD (gX g (log2i n)) (gY g m) 13) | (n, Just m) <- zip ns ms ]
  ++ [ Color (rgb255 120 200 140) (thickLine 1.5 (gX g 0, gY g simA) (gX g 3, gY g (simA + 3 * simB))) | wSim w ]
  ++ fitPart
  ++ [ say ("True values: a = " ++ fmt 0 simA ++ " ms, b = " ++ fmt 0 simB ++ " ms/bit") (gx g) (ry + 52) 14 (rgb255 120 200 140) | wSim w ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    n = curN w
    bw = min 70 ((envW env * 0.5 - 60) / 8)
    bx = 20
    by = 120
    boxX i = bx + fromIntegral i * (bw + 8) + (if i >= 4 then 20 else 0)
    info | wPhase w == Done = if wSim w then "Simulated observer (30 trials for each N)" else "Well done! R to try again"
         | otherwise = "Block " ++ show (wBi w + 1) ++ " / 4: N = " ++ show n ++ " (" ++ show (round (log2i n) :: Int) ++ " bit)   "
                       ++ show (blockCount w) ++ " / " ++ show per ++ (if wPhase w == Ready then "   -> press space" else "")
    ms = means (wTrials w)
    ty = by + bw + 160
    cols = [0, 50, 100, 200]
    g = Graph (envW env * 0.6) 110 (envW env * 0.35) (envH env * 0.5) (-0.3) 3.3 0 1000 False False
    ry = gy g + gh g + 64
    fract' v = v - fromIntegral (floor v :: Int)
    fitPart = case wFit w of
      Just f -> [ Color (rgb255 255 170 90) (thickLine 3 (gX g 0, gY g (linA f)) (gX g 3, gY g (linA f + 3 * linB f)))
                , say ("RT = " ++ fmt 0 (linA f) ++ " + " ++ fmt 0 (linB f) ++ " * log2 N [ms]   r2 = " ++ fmt 2 (linR2 f)) (gx g) ry 17 (gray255 235)
                , say ("+1 bit -> " ++ fmt 0 (linB f) ++ " ms slower -> speed ~ " ++ (if linB f > 1 then fmt 1 (1000 / linB f) ++ " bit/s" else "-")) (gx g) (ry + 26) 15 (gray255 235) ]
      Nothing -> [ say "Measure 2 or more N to fit the line" (gx g) ry 16 (gray255 235) ]

toUpperA :: Char -> Char
toUpperA c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ | wPhase w == Ready -> pure (nextTrial env w { wMsg = "" })
  EventKey (Char c) Down _ _
    | c `elem` "oO" -> let s = wSimSeed w + 1 in pure (runSim s w { wSimSeed = s })
    | c `elem` "rR" -> do r <- freshSeed
                          pure (resetData False r w)
    | otherwise -> pure (respond env (toLower c) w)
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("log2 1 = 0, log2 2 = 1, log2 4 = 2, log2 8 = 3 (bit)", and [ log2i n == fromIntegral i | (i, n) <- zip [0 :: Int ..] ns ])
  , ("number of boxes used for each N = N", all (\n -> length (sets n) == n) ns)
  , ("noise-free RT 200, 350, 500, 650 -> a = 200, b = 150 exactly, r2 = 1"
    , case linFit [0, 1, 2, 3] [200, 350, 500, 650] of
        Just f -> near (linA f) 200 1e-9 && near (linB f) 150 1e-9 && near (linR2 f) 1 1e-12
        Nothing -> False)
  , ("only one kind of N -> no line is fitted", case linFit [1, 1, 1] [300, 320, 310] of { Nothing -> True; Just _ -> False })
  , ("simulated observer (80 trials per N): b ~ 140 ms/bit (+-15)", maybe False (\f -> near (linB f) simB 15) f2')
  , ("intercept a ~ 220 ms (+-25)", maybe False (\f -> near (linA f) simA 25) f2') ]
  where
    tr = fst (runRand (simTrials 80) (seedRng 52))
    f2' = linFit [0, 1, 2, 3] [ maybe 0 id m | m <- means tr ]

-- ---------- やってみよう ----------
-- 1. 32 試行やって、自分の a と b を出そう。1/b は何 bit/秒？
-- 2. 平均 RT は log₂N の直線に乗った？ N = 1 だけ特別に速くない？
-- 3. まちがいの数は N とともに増えた？ 速さと正確さのトレードオフを考えよう
-- 4. 片方の箱だけよく光るようにすると（確率をかたよらせる）、ヒックの法則ではどうなると予想できる？
-- 5. メニューの項目数と選ぶ時間：アプリのメニューを設計するときに使えるか考えよう

-- ==================================================
-- ここから下は「心理物理の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 種つきの乱数（同じ種なら毎回同じ列）。p5.js 版の rnd()（mulberry32）と同じ数が出る
newtype Rng = Rng Word32

-- 種 0 は 1 にする（p5.js 版の (s >>> 0) || 1 と同じ）
seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 乱数を「順番に」使う小さなしくみ：Rand a は「乱数の状態を受け取って、値と次の状態を返す」計算
newtype Rand a = Rand { runRand :: Rng -> (a, Rng) }

instance Functor Rand where
  fmap f (Rand g) = Rand (\r -> let (a, r') = g r in (f a, r'))
instance Applicative Rand where
  pure a = Rand (\r -> (a, r))
  Rand f <*> Rand g = Rand (\r -> let (h, r1) = f r; (a, r2) = g r1 in (h a, r2))
instance Monad Rand where
  Rand g >>= k = Rand (\r -> let (a, r1) = g r in runRand (k a) r1)

-- 0〜1 の一様乱数
rnd :: Rand Double
rnd = Rand $ \(Rng s0) ->
  let a = s0 + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 標準正規分布（ボックス＝ミュラー法。u を先、v を後に引く）
rndNormal :: Rand Double
rndNormal = do
  u <- fmap (1 -) rnd
  v <- rnd
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- フィッシャー＝イェーツのシャッフル（後ろから順に入れかえる）
shuffleArr :: [a] -> Rand [a]
shuffleArr xs = go (V.fromList xs) (length xs - 1)
  where
    go v i | i <= 0 = pure (V.toList v)
           | otherwise = do
               u <- rnd
               let j = floor (u * fromIntegral (i + 1))
               go (v V.// [(i, v V.! j), (j, v V.! i)]) (i - 1)

-- 起動のたびに変わる種（p5.js 版の Date.now() の代わり）
freshSeed :: IO Rng
freshSeed = seedRng <$> randomRIO (1, maxBound)

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- JavaScript の Math.round（.5 は大きい方へ）
roundJ :: Double -> Double
roundJ x = fromIntegral (floor (x + 0.5) :: Int)

-- ---- 正規分布：Φ（累積分布）と z（その逆関数。二分法で数値的に解く）
-- 誤差関数（Abramowitz & Stegun 7.1.26、誤差 1.5e-7 以下）
erfA :: Double -> Double
erfA x0 = s * y
  where
    s = if x0 < 0 then -1 else 1
    x = abs x0
    t = 1 / (1 + 0.3275911 * x)
    y = 1 - ((((1.061405429 * t - 1.453152027) * t + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * exp (-x * x)

phi :: Double -> Double
phi x = 0.5 * (1 + erfA (x / sqrt 2))

-- Φ(z) = p となる z
zOf :: Double -> Double
zOf p0 = go (-9) 9 (70 :: Int)
  where
    p = clampv 1e-9 (1 - 1e-9) p0
    go lo hi 0 = (lo + hi) / 2
    go lo hi n = let m = (lo + hi) / 2 in if phi m < p then go m hi (n - 1) else go lo m (n - 1)

-- 割合を 1/(2N) で補正（0 や 1 だと z が無限大になるので）
rateFix :: Double -> Double -> Double
rateFix k n = if n > 0 then clampv (1 / (2 * n)) (1 - 1 / (2 * n)) (k / n) else 0.5

-- ---- 上下法（階段法）：nDown 回続けて正答で 1 段下げ（難しく）、1 回まちがえたら 1 段上げ（やさしく）
-- 値は掛け算で動かす（factor 倍）。1-up-2-down は正答率 70.7%（p² = 0.5）、1-up-3-down は 79.4% に集まる
-- fast = True なら、最初にまちがえるまでは 1 回正解ごとに factor² ずつ下げて、すばやくしきい値の近くへ
data Stair = Stair
  { stV, stFactor :: !Double, stNDown :: !Int, stLo, stHi :: !Double, stFast :: !Bool
  , stRun, stDir :: !Int
  , stTrack :: [(Double, Bool)]     -- 試行の記録（値, 正答か）。新しい順
  , stRevs  :: [(Int, Double)] }    -- 反転点（試行番号, 値）。新しい順

makeStair :: Double -> Double -> Int -> Double -> Double -> Bool -> Stair
makeStair start f nd lo hi fast = Stair start f nd lo hi fast 0 0 [] []

-- これまでの試行数
stN :: Stair -> Int
stN = length . stTrack

stairUpdate :: Stair -> Bool -> Stair
stairUpdate st0 correct
  | stFast st = st { stDir = -1, stV = clampv (stLo st) (stHi st) (stV st / (stFactor st * stFactor st)) }
  | move == 0 = st { stRun = run' }
  | otherwise = st { stRun = run', stDir = move
                   , stRevs = if stDir st /= 0 && move /= stDir st then (stN st - 1, stV st) : stRevs st else stRevs st   -- 向きが変わった = 反転点
                   , stV = clampv (stLo st) (stHi st) (if move < 0 then stV st / stFactor st else stV st * stFactor st) }
  where
    st1 = st0 { stTrack = (stV st0, correct) : stTrack st0 }
    st = if stFast st1 && not correct then st1 { stFast = False } else st1
    (run', move) | correct = if stRun st + 1 >= stNDown st then (0, -1) else (stRun st + 1, 0 :: Int)
                 | otherwise = (0, 1)

-- しきい値 = 反転点の幾何平均（最初の skip 個は練習とみなして捨てる）。足りなければ最後の 6 試行の幾何平均
stairThreshold :: Stair -> Int -> Maybe Double
stairThreshold st skip
  | null r = Nothing
  | otherwise = Just (exp (foldl' (\s v -> s + log v) 0 r / fromIntegral (length r)))
  where
    r0 = map snd (drop skip (reverse (stRevs st)))
    r = if length r0 < 2 then map fst (reverse (take 6 (stTrack st))) else r0

-- ---- 直線 y = a + b x を最小二乗法で当てはめる（r² = 当てはまりのよさ）
data Lin = Lin { linA, linB, linR2 :: !Double }

linFit :: [Double] -> [Double] -> Maybe Lin
linFit xs ys
  | n < 2 || sxx < 1e-12 = Nothing
  | otherwise = Just (Lin (my - b * mx) b (if syy > 1e-12 then sxy * sxy / (sxx * syy) else 1))
  where
    n = length xs
    nf = fromIntegral n
    mx = sum xs / nf
    my = sum ys / nf
    sxx = sum [ (x - mx) ^ (2 :: Int) | x <- xs ]
    sxy = sum [ (x - mx) * (y - my) | (x, y) <- zip xs ys ]
    syy = sum [ (y - my) ^ (2 :: Int) | y <- ys ]
    b = sxy / sxx

-- ---- グラフの道具：枠 (x, y, w, h) と値の範囲。対数の軸も
data Graph = Graph { gx, gy, gw, gh :: !Float, gx0, gx1, gy0, gy1 :: !Double, glogx, glogy :: !Bool }

gX :: Graph -> Double -> Float
gX g v = gx g + f2 (clampv (-0.02) 1.02 t) * gw g
  where t = if glogx g then (log v - log (gx0 g)) / (log (gx1 g) - log (gx0 g)) else (v - gx0 g) / (gx1 g - gx0 g)

gY :: Graph -> Double -> Float
gY g v = gy g + gh g - f2 (clampv (-0.02) 1.02 t) * gh g
  where t = if glogy g then (log v - log (gy0 g)) / (log (gy1 g) - log (gy0 g)) else (v - gy0 g) / (gy1 g - gy0 g)

-- 枠・目盛り・軸の名前
plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (gray255 95) (frameTL 1 (gx g) (gy g) (gw g) (gh g)) ]
  ++ [ sayC (prec3 v) (gX g v) (gy g + gh g + 20) 14 (gray255 165) | v <- xt ]
  ++ [ sayR (prec3 v) (gx g - 6) (gY g v + 6) 14 (gray255 165) | v <- yt ]
  ++ [ sayR xl (gx g + gw g) (gy g + gh g + 44) 14 (gray255 205)
     , say yl (gx g) (gy g - 10) 14 (gray255 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ w）
polyline :: Float -> [Point] -> Picture
polyline w pts = if length pts < 2 then Blank else if w <= 1 then Line pts else thickPath w pts

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

gray255 :: Float -> Color
gray255 v = gray (v / 255)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 文字のおよその幅
textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中央揃え・右揃えの文字（y は文字の下）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt。1 以下なら細い線）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h
  | wt <= 1 = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
  | otherwise = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 円（直径 d、p5.js の circle と同じ）
circleD :: Float -> Float -> Float -> Picture
circleD x y d = at (x, y) (disc (d / 2))

-- ---- 数の道具
-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 3 桁で、余分な 0 を書かない（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v
  | v == 0 || isNaN v || isInfinite v = "0"
  | e > 2 = show (round (v / 10 ^^ (e - 2)) * 10 ^ (e - 2) :: Integer)
  | otherwise = strip (showFFloat (Just (2 - e)) v "")
  where
    e = floor (logBase 10 (abs v) + 1e-12) :: Int
    strip s = if '.' `elem` s then let t = reverse (dropWhile (== '0') (reverse s)) in if last' t == '.' then init t else t else s
    last' t = if null t then ' ' else last t

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- Maybe の値（null なら不合格）
nearM :: Maybe Double -> Double -> Double -> Bool
nearM m b tol = maybe False (\a -> near a b tol) m

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 画面の左下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 12) 14 (if all snd rs then rgb255 120 210 150 else rgb255 255 90 90)
