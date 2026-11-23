-- ============================================
-- 56-14 束の音 🔊 ─ 約数の束の各要素を音の高さにして、鎖をたどって鳴らす
-- 素因数ごとに音程を決める：2 → 長 3 度（×5/4）、3 → 完全 5 度（×3/2）、5 → 長 2 度（×9/8）、7 → ×7/4
-- 約数 d = 2^a·3^b·5^c… の高さ = 基音 × (5/4)^a·(3/2)^b·(9/8)^c…。かけ算なので 上限（lcm）と下限（gcd）の 2 音の積 = もとの 2 音の積
-- 操作：クリックで音 ON / 要素をクリック → その音、2 つ目をクリック → 2 音のあと上限と下限 / スペース ランダムな極大鎖を鳴らす
--       ← → n を変える / ↑↓ テンポ / M 消音 / 音量は小さめから（全体の音量 master は 0.2 以下）
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_, replicateM, when)
import qualified Data.Array as A
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.Bits (shiftR, xor, (.&.), (.|.))
import Data.List (elemIndex, intercalate, partition, sortBy, foldl')
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

ns :: [Int]
ns = [60, 72, 120, 180, 210, 360, 30]   -- ★ 選べる n

base :: Double
base = 130.81                           -- ★ 基音（ド、C3）Hz

-- ★ 素因数ごとの音程（ないものは 1）
ratio :: Int -> Double
ratio p = case p of { 2 -> 5 / 4; 3 -> 3 / 2; 5 -> 9 / 8; 7 -> 7 / 4; 11 -> 11 / 8; _ -> 1 }

master :: Double
master = 0.12                           -- ★ 全体の音量（0.2 以下にしておく）

-- 鳴らす予定の音（時刻 [秒]、周波数、長さ）と、要素のまわりの波紋
data Tone = Tone { tAt :: !Double, tFreq :: !Double, tDur :: !Double }
data RingW = RingW { rI :: !Int, rT :: !Double, rLife :: !Double }

data World = World
  { wNi     :: !Int
  , wLat    :: Poset
  , wSel    :: [Int]
  , wSeq    :: [Int]
  , wTempo  :: !Double
  , wMuted  :: !Bool
  , wAudio  :: !Bool               -- 音を始めたか（クリックかキーで ON）
  , wRings  :: [RingW]
  , wTones  :: [Tone]
  , wNow    :: !Double             -- いまの時刻 [秒]（millis() / 1000）
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "56-14 Lattice sounds" (rebuild env1 w0))
  { skBack  = bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (rebuild env w0 { wTest = selfTest })
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }
  where env1 = Env 1280 800 (0, 0) False 0 0 mempty
        w0 = World 0 (divisorPoset 60) [] [] 0.32 False False [] [] 0 []

freqOf :: Int -> Double
freqOf d = foldl' (\f (p, e) -> f * ratio p ^ e) base (primeFactors d)

-- 周波数 → 音名（C4 など）とセント（c）
noteName :: Double -> String
noteName f = getAt "?" names (((k `mod` 12) + 12) `mod` 12) ++ show (floor (fromIntegral k / 12 - 1 :: Double) :: Int)
             ++ (if c /= 0 then (if c > 0 then "+" else "") ++ show c ++ "c" else "")
  where m = 12 * logBase 2 (f / 440) + 69
        k = jsRound m
        c = jsRound ((m - fromIntegral k) * 100)
        names = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

-- Math.round と同じ（.5 は大きい方へ）
jsRound :: Double -> Int
jsRound v = floor (v + 0.5)

rebuild :: Env -> World -> World
rebuild env w = w { wLat = layoutHasse (divisorPoset (getAt 60 ns (wNi w))) 100 170 (envW env * 0.6 - 100) (envH env - 290) True, wSel = [], wSeq = [] }

-- ⊥ から、すぐ上の元（被覆）をランダムに選んで ⊤ まで = 極大鎖。us は 0〜1 の乱数の列
randomChain :: Poset -> [Double] -> [Int]
randomChain lat us0 = go (botOf lat) us0 (0 :: Int)
  where
    top = topOf lat
    go x us k
      | x == top || k > pN lat = [x]
      | otherwise = case ([ b | (a, b) <- pCov lat, a == x ], us) of
          ([], _) -> [x]
          (up, u : rest) -> x : go (getAt x up (floor (u * fromIntegral (length up)))) rest (k + 1)
          (up, []) -> x : go (getAt x up 0) [] (k + 1)

-- 極大鎖の数：下から「すぐ下の元の数の合計」を順に足す
countChains :: Poset -> Integer
countChains lat
  | botOf lat < 0 || topOf lat < 0 = 0
  | otherwise = cs A.! topOf lat
  where
    ord = sortBy (comparing (rankOf lat)) (allOf lat)
    c0 = A.listArray (0, pN lat - 1) [ if i == botOf lat then 1 else 0 | i <- allOf lat ] :: A.Array Int Integer
    cs = foldl' (\c y -> foldl' (\c' (a, b) -> if b == y then c' A.// [(y, c' A.! y + c' A.! a)] else c') c (pCov lat)) c0 ord

multinomial :: Int -> Integer
multinomial n = fact (sum es) `div` product (map fact es)
  where es = map snd (primeFactors n)
        fact k = product [1 .. fromIntegral k]

-- 要素 i の音を delay 秒後から dur 秒
sound :: Int -> Double -> Double -> World -> World
sound i delay dur w = w { wTones = (if wMuted w then id else (Tone t0 (freqOf (getAt 1 (pVal (wLat w)) i)) dur :)) (wTones w)
                        , wRings = RingW i t0 dur : wRings w }
  where t0 = wNow w + delay

-- やわらかい音（三角波）。音量は master × vol × 0.4
tone :: Double -> Double -> Double -> IO ()
tone freq dur vol = playTone (realToFrac (clampv freq 40 4000)) (realToFrac (min 0.2 master * clampv vol 0 1 * 0.4)) (realToFrac (max 0.1 dur)) Triangle

step :: Env -> Float -> World -> IO World
step env _ w = do
  let now = realToFrac (envTime env)
      (due, later) = partition (\t -> tAt t <= now) (wTones w)
  when (wAudio w) $ forM_ (take 6 due) $ \t -> tone (tFreq t) (tDur t) 0.5
  pure w { wNow = now, wTones = later, wRings = filter (\r -> now < rT r + rLife r) (wRings w) }

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 鳴っている元のまわりに波紋
  [ let a = (now - rT r) / rLife r in Color (rgbaC 255 220 140 (round (200 * (1 - a)))) (at (posOf lat (rI r)) (ring (f2 (20 + 45 * a)) 2))
  | r <- wRings w, now >= rT r ]
  ++ [ drawHasse lat hopt { oRad = 22, oFs = 14, oCol = Just . colOf, oEcol = ecol } ]
  -- ---- 右のパネル
  ++ [ say ("n = " ++ show n ++ " = " ++ intercalate "*" [ show p ++ (if e > 1 then "^" ++ show e else "") | (p, e) <- primeFactors n ] ++ "   " ++ show (pN lat) ++ " divisors") px y 17 (grayC 230)
     , say "Intervals: 2 -> x5/4, 3 -> x3/2, 5 -> x9/8, 7 -> x7/4" px (y + 28) 14 (grayC 190)
     , say ("Maximal chains (1 to n, one step at a time): " ++ show (countChains lat)) px (y + 52) 14 (grayC 190) ]
  ++ [ say (show (vOf hv) ++ ": " ++ fmt 1 (freqOf (vOf hv)) ++ " Hz (" ++ noteName (freqOf (vOf hv)) ++ ")") px (y + 86) 16 (rgbC 255 220 140) | hv >= 0 ]
  ++ (case wSel w of
        [i, j] -> let a = vOf i
                      b = vOf j
                      l = lcmOf a b
                      g = gcd a b
                  in [ say ("Two notes " ++ show a ++ " and " ++ show b ++ " -> join lcm = " ++ show l ++ ", meet gcd = " ++ show g) px (y + 124) 15 (grayC 220)
                     , say ("f(lcm)*f(gcd) = " ++ fmt 1 (freqOf l * freqOf g) ++ "   f(a)*f(b) = " ++ fmt 1 (freqOf a * freqOf b)) px (y + 148) 15 (grayC 220)
                     , say "-> join and meet have the same 'mean pitch' as a and b" px (y + 172) 15 (grayC 220) ]
        _ -> [])
  ++ (if null (wSeq w) then [] else
        [ say ("Chain: " ++ intercalate " -> " (map (show . vOf) (wSeq w))) px (y + 214) 15 (rgbC 255 220 140)
        , say ("       " ++ intercalate " " (map (noteName . freqOf . vOf) (wSeq w))) px (y + 238) 14 (rgbC 255 220 140) ])
  ++ [ say "Lattice sounds   pitch of divisor d = base x product of the prime intervals" 20 36 18 (grayC 240)
     , say ((if wAudio w then (if wMuted w then "muted (M)" else "sound ON") else "click for sound") ++ "   Click note, two = join & meet   Space chain   Left/Right n   Up/Down tempo "
            ++ fmt 2 (wTempo w) ++ " s   M mute") 20 62 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    lat = wLat w
    now = wNow w
    n = getAt 60 ns (wNi w)
    vOf i = getAt 1 (pVal lat) i
    px = envW env * 0.63
    y = 160
    hv = nodeAt lat (toP5 env (envMouse env)) 24
    fmin = freqOf 1
    fmax = freqOf n
    act = [ rI r | r <- wRings w, now >= rT r, now < rT r + rLife r ]
    colOf i | i `elem` act = rgbC 255 255 255
            | i `elem` wSel w = rgbC 255 220 140
            | otherwise = let u = f2 (log (freqOf (vOf i) / fmin) / max 1e-9 (log (fmax / fmin)))
                          in makeColor ((70 + 140 * u) / 255) ((150 + 60 * (1 - u)) / 255) ((230 - 90 * u) / 255) 1
    ecol a b = case (elemIndex a (wSeq w), elemIndex b (wSeq w)) of
      (Just ia, Just ib) | abs (ia - ib) == 1 -> Just (rgbC 255 220 140)
      _ -> Nothing

event :: Env -> Event -> World -> IO World
event env ev w0 = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let i = nodeAt (wLat w) (toP5 env pos) 26
        sel = (if length (wSel w) >= 2 then [] else wSel w) ++ [i]
        w1 = sound i 0 0.9 w { wSel = sel }
    in pure $ if i < 0 then w else case sel of
         [a, b] -> sound (meetOf (wLat w) a b) 1.5 1.4 (sound (joinOf (wLat w) a b) 1.5 1.4 (sound b 0.6 1.2 (sound a 0.6 1.2 w1)))
         _ -> w1
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    us <- replicateM 32 (randomRIO (0, 1))
    let sq = randomChain (wLat w) us
        len = length sq
        t = wTempo w
    pure (foldl' (\acc (k, x) -> sound x (fromIntegral k * t) (t * fromIntegral (len - k) + 0.6) acc) w { wSeq = sq } (zip [0 :: Int ..] sq))   -- 音を重ねていくと和音に
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (rebuild env w { wNi = (wNi w + 1) `mod` length ns })
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (rebuild env w { wNi = (wNi w + length ns - 1) `mod` length ns })
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wTempo = max 0.12 (wTempo w - 0.04) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wTempo = min 0.8 (wTempo w + 0.04) }
  EventKey (Char c) Down _ _ | c `elem` "mM" -> pure w { wMuted = not (wMuted w) }
  _ -> pure w0
  where w = case ev of
          EventKey _ Down _ _ -> w0 { wAudio = True, wNow = realToFrac (envTime env) }   -- startAudio()
          _ -> w0

-- ---- 自己テスト（p5.js 版と同じ 5 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("d | e => f(d) < f(e) (pitch rises along 'divides' = monotone)", mono)
  , ("for every pair f(lcm)*f(gcd) = f(a)*f(b) (max + min of exponents = sum)", mult)
  , ("number of maximal chains = multinomial (a+b+c)!/(a!b!c!) (60: 4!/(2!1!1!) = 12)", cnt && countChains (divisorPoset 60) == 12)
  , ("random chains always walk covers from 1 to n (maximal chains)", okCh)
  , ("pitches within 100..2000 Hz, master volume <= 0.2", range && master <= 0.2) ]
  where
    qs = [ (n, divisorPoset n) | n <- ns ]
    mono = and [ freqOf (v q a) < freqOf (v q b) | (_, q) <- qs, (a, b) <- pCov q ]
    mult = and [ abs (freqOf (lcmOf a b) * freqOf (gcd a b) - freqOf a * freqOf b) <= 1e-6 * freqOf a * freqOf b | (_, q) <- qs, a <- pVal q, b <- pVal q ]
    range = and [ freqOf n <= 2000 && freqOf 1 >= 100 | (n, _) <- qs ]
    cnt = and [ countChains q == multinomial n | (n, q) <- qs ]
    v q i = getAt 1 (pVal q) i
    q360 = divisorPoset 360
    -- p5.js 版は Math.random。ここでは種つきの乱数で 30 本
    (okCh, _) = foldl' oneCh (True, seedRng 14) [1 .. 30 :: Int]
    oneCh (ok, r0) _ =
      let (us, r1) = foldl' (\(acc, r) _ -> let (u, r') = rnd r in (u : acc, r')) ([], r0) [1 .. 16 :: Int]
          ch = randomChain q360 us
          good = getAt (-1) ch 0 == 0 && last' ch == pN q360 - 1
                 && and [ (a, b) `elem` pCov q360 | (a, b) <- zip ch (drop 1 ch) ]
      in (ok && good, r1)
    last' xs = if null xs then -1 else last xs

-- ---------- やってみよう ----------
-- 1. スペースを何回か押す。同じ n でも鎖ごとに音の進み方がちがう。でも最後の和音は？
-- 2. 2 つクリックして、上限と下限の 2 音を聞きくらべよう
-- 3. ratio を 2 → 2（オクターブ）、3 → 3/2 にすると？ 音の並びはどう変わる？
-- 4. n = 210 = 2·3·5·7 は 4 次元の立方体。7 の音程 ×7/4 は「自然 7 度」と呼ばれる少し低い音
-- 5. 自由制作：自分の n と音程で、束をたどる曲を作ろう


-- ==================================================
-- ここから下は「束論の道具」（中身は読まなくても OK。消さないでね）
-- ==================================================
-- ---- 順序の表：R ! (i, j) = True は「i ≤ j」（n × n の表）
type Rel = UArray (Int, Int) Bool

-- 順序集合 P。pJ・pM は上限・下限の表（tables で作る）、pPos はハッセ図の位置
data Poset = Poset
  { pN    :: !Int
  , pR    :: !Rel
  , pCov  :: [(Int, Int)]
  , pRk   :: !(UArray Int Int)
  , pLab  :: A.Array Int String
  , pPos  :: A.Array Int (Float, Float)
  , pVal  :: [Int]
  , pHint :: Maybe (Int -> Double)
  , pJ, pM :: !(UArray (Int, Int) Int) }

relSize :: Rel -> Int
relSize r = let ((_, _), (n1, _)) = bounds r in n1 + 1

elemsU :: Rel -> [Bool]
elemsU r = [ r ! (i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1] ] where n = relSize r

assocsU :: Rel -> [((Int, Int), Bool)]
assocsU r = [ ((i, j), r ! (i, j)) | i <- [0 .. n - 1], j <- [0 .. n - 1] ] where n = relSize r

leq :: Poset -> Int -> Int -> Bool
leq p i j = pR p ! (i, j)

rankOf :: Poset -> Int -> Int
rankOf p i = if i >= 0 && i < pN p then pRk p ! i else 0

labOf :: Poset -> Int -> String
labOf p i = if i >= 0 && i < pN p then pLab p A.! i else "-"

posOf :: Poset -> Int -> (Float, Float)
posOf p i = let (lo, hi) = A.bounds (pPos p) in if i >= lo && i <= hi then pPos p A.! i else (0, 0)

makePoset :: Int -> (Int -> Int -> Bool) -> [String] -> Poset
makePoset n leqFn labs = posetFromRel (listArray ((0, 0), (n - 1, n - 1)) [ i == j || leqFn i j | i <- [0 .. n - 1], j <- [0 .. n - 1] ]) labs

-- labs が空なら 0, 1, 2, ... を名前にする
posetFromRel :: Rel -> [String] -> Poset
posetFromRel r labs = Poset n r cov (ranksOf n cov) (A.listArray (0, n - 1) (take n (labs ++ map show [length labs ..])))
                            (A.listArray (0, -1) []) [] Nothing emptyT emptyT
  where n = relSize r
        cov = coversOf r

emptyT :: UArray (Int, Int) Int
emptyT = listArray ((0, 0), (-1, -1)) []

-- 被覆の組から
fromCovers :: [String] -> [(Int, Int)] -> Poset
fromCovers labs cv = posetFromRel (closeRel (runSTUArray (do
  r <- newArray ((0, 0), (n - 1, n - 1)) False
  forM_ cv $ \(a, b) -> writeArray r (a, b) True
  pure r))) labs
  where n = length labs

-- 反射的・推移的閉包（ワーシャル法：k を経由して行けるなら i ≤ j）
closeRel :: Rel -> Rel
closeRel r0 = runSTUArray $ do
  let n = relSize r0
  c <- newArray ((0, 0), (n - 1, n - 1)) False
  forM_ [0 .. n - 1] $ \i -> forM_ [0 .. n - 1] $ \j -> writeArray c (i, j) (r0 ! (i, j) || i == j)
  forM_ [0 .. n - 1] $ \k -> forM_ [0 .. n - 1] $ \i -> do
    ik <- readArray c (i, k)
    when ik $ forM_ [0 .. n - 1] $ \j -> do
      kj <- readArray c (k, j)
      when kj (writeArray c (i, j) True)
  pure c

isReflexive, isAntisym, isTransitive, isPoset :: Rel -> Bool
isReflexive r = and [ r ! (i, i) | i <- [0 .. relSize r - 1] ]
isAntisym r = and [ not (r ! (i, j) && r ! (j, i)) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ] where n = relSize r
isTransitive r = and [ r ! (i, k) | i <- [0 .. n - 1], j <- [0 .. n - 1], r ! (i, j), k <- [0 .. n - 1], r ! (j, k) ] where n = relSize r
isPoset r = isReflexive r && isAntisym r && isTransitive r

-- 被覆 a ⋖ b：a < b で、間にはさまる c がない（推移的簡約 = ハッセ図の辺）
coversOf :: Rel -> [(Int, Int)]
coversOf r = [ (a, b) | a <- [0 .. n - 1], b <- [0 .. n - 1], lt a b, not (any (\c -> lt a c && lt c b) [0 .. n - 1]) ]
  where n = relSize r
        lt a b = a /= b && r ! (a, b) && not (r ! (b, a))

-- ランク = 下から c までの最長の鎖の長さ（最大 n 回くり返せば止まる）
ranksOf :: Int -> [(Int, Int)] -> UArray Int Int
ranksOf n cov = runSTUArray $ do
  rk <- newArray (0, n - 1) 0
  let pass t = when (t <= n) $ do
        ch <- foldM (\c (a, b) -> do
                       ra <- readArray rk a
                       rb <- readArray rk b
                       if rb < ra + 1 && ra < n then writeArray rk b (ra + 1) >> pure True else pure c) False cov
        when ch (pass (t + 1))
  pass 0
  pure rk

allOf :: Poset -> [Int]
allOf p = [0 .. pN p - 1]

upperOf, lowerOf :: Poset -> Int -> Int -> [Int]
upperOf p a b = [ c | c <- allOf p, leq p a c, leq p b c ]     -- 上界：a と b の両方より上
lowerOf p a b = [ c | c <- allOf p, leq p c a, leq p c b ]     -- 下界

-- 最小元・最大元（なければ −1）
leastOf, greatestOf :: Poset -> [Int] -> Int
leastOf p sx = case [ x | x <- sx, all (leq p x) sx ] of { (x : _) -> x; [] -> -1 }
greatestOf p sx = case [ x | x <- sx, all (\y -> leq p y x) sx ] of { (x : _) -> x; [] -> -1 }

-- 上限 a ∨ b = 最小上界、下限 a ∧ b = 最大下界（なければ −1）
joinOf, meetOf :: Poset -> Int -> Int -> Int
joinOf p a b = leastOf p (upperOf p a b)
meetOf p a b = greatestOf p (lowerOf p a b)

botOf, topOf :: Poset -> Int
botOf p = leastOf p (allOf p)
topOf p = greatestOf p (allOf p)

isLattice :: Poset -> Bool
isLattice p = pN p > 0 && and [ joinOf p a b >= 0 && meetOf p a b >= 0 | a <- allOf p, b <- [a + 1 .. pN p - 1] ]

-- 上限・下限の表（何度も使うので先に計算）
tables :: Poset -> Poset
tables p = p { pJ = mk joinOf, pM = mk meetOf }
  where n = pN p
        mk f = listArray ((0, 0), (n - 1, n - 1)) [ f p a b | a <- [0 .. n - 1], b <- [0 .. n - 1] ]

-- 表を引く（tables の前や範囲の外なら −1）
tJ, tM :: Poset -> Int -> Int -> Int
tJ p a b = tableAt (pJ p) a b
tM p a b = tableAt (pM p) a b

tableAt :: UArray (Int, Int) Int -> Int -> Int -> Int
tableAt t a b = let ((_, _), (n1, _)) = bounds t in if a >= 0 && b >= 0 && a <= n1 && b <= n1 then t ! (a, b) else -1

-- ---- よく使う束：約数の束（割り切る）と部分集合の束（含まれる）
lcmOf :: Int -> Int -> Int
lcmOf a b = (a `div` gcd a b) * b

divisorsOf :: Int -> [Int]
divisorsOf n = [ k | k <- [1 .. n], n `mod` k == 0 ]

primeFactors :: Int -> [(Int, Int)]
primeFactors = go 2
  where
    go p n | p * p > n = if n > 1 then [(n, 1)] else []
           | n `mod` p == 0 = let (e, m) = strip p n 0 in (p, e) : go (p + 1) m
           | otherwise = go (p + 1) n
    strip p n e = if n `mod` p == 0 then strip p (n `div` p) (e + 1) else (e, n)

expOf :: Int -> Int -> Int
expOf p v = if v > 0 && v `mod` p == 0 then 1 + expOf p (v `div` p) else 0

divisorPoset :: Int -> Poset
divisorPoset n = (makePoset (length ds) (\i j -> dv j `mod` dv i == 0) (map show ds)) { pVal = ds, pHint = Just hint }
  where
    ds = divisorsOf n
    da = listArray (0, length ds - 1) ds :: UArray Int Int
    dv i = da ! i
    ps = map fst (primeFactors n)
    dir = [-1, 1, -0.3, 0.55, -0.62] :: [Double]
    hint i = sum [ fromIntegral (expOf p (dv i)) * getAt 0 dir (t `mod` 5) | (t, p) <- zip [0 ..] ps ]

popc :: Int -> Int
popc m = if m <= 0 then 0 else (m .&. 1) + popc (m `shiftR` 1)

-- 部分集合の名前（空集合は {}）
setName :: Int -> Int -> String
setName m k = case [ c | (t, c) <- zip [0 .. k - 1] "abcdefg", (m `shiftR` t) .&. 1 == 1 ] of
  "" -> "{}"
  s -> s

powersetPoset :: Int -> Poset
powersetPoset k = (makePoset (2 ^ k) (\i j -> i .&. j == i) [ setName m k | m <- [0 .. 2 ^ k - 1] ])
                    { pVal = [0 .. 2 ^ k - 1], pHint = if k <= 4 then Just hint else Nothing }
  where dir = [-1, 1, -0.3, 0.55] :: [Double]
        hint i = sum [ fromIntegral ((i `shiftR` t) .&. 1) * d | (t, d) <- zip [0 ..] dir ]

-- ---- ハッセ図を描く：高さ = ランク、横 = hint（なければ下の被覆の平均の順）
layoutHasse :: Poset -> Float -> Float -> Float -> Float -> Bool -> Poset
layoutHasse p x y w h exact = p { pPos = A.listArray (0, n - 1) [ (x + w * f2 (pxF i), y + h - (if mr > 0 then fromIntegral (rankOf p i) / fromIntegral mr else 0.5) * h) | i <- [0 .. n - 1] ] }
  where
    n = pN p
    mr = maximum (0 : [ rankOf p i | i <- [0 .. n - 1] ])
    useHint = exact && isJust (pHint p)
    lvl r = [ i | i <- [0 .. n - 1], rankOf p i == r ]
    step :: A.Array Int Double -> Int -> A.Array Int Double
    step px r =
      let ids = lvl r
          v i = case pHint p of
            Just f -> f i
            Nothing -> let ss = [ px A.! a | (a, b) <- pCov p, b == i ]
                       in (if null ss then 0.5 else sum ss / fromIntegral (length ss)) + fromIntegral i * 1e-6
          px1 = px A.// [ (i, v i) | i <- ids ]
          sorted = sortBy (comparing (px1 A.!)) ids
          m = length sorted
      in if useHint then px1 else px1 A.// [ (i, (fromIntegral k + 0.5) / fromIntegral m) | (k, i) <- zip [0 :: Int ..] sorted ]
    pxRaw = foldl' step (A.listArray (0, n - 1) (replicate n 0.5)) [0 .. mr]
    vs = A.elems pxRaw
    lo = if null vs then 0 else minimum vs
    hi = if null vs then 0 else maximum vs
    pxF i | useHint = if hi > lo then 0.04 + 0.92 * (pxRaw A.! i - lo) / (hi - lo) else 0.5
          | otherwise = pxRaw A.! i

-- 描き方の指定：要素の色・辺の色（Nothing なら標準の色）、丸の半径、文字の大きさ
data HOpt = HOpt { oCol :: Int -> Maybe Color, oEcol :: Int -> Int -> Maybe Color, oRad :: Float, oFs :: Float }

hopt :: HOpt
hopt = HOpt (const Nothing) (\_ _ -> Nothing) 16 14

drawHasse :: Poset -> HOpt -> Picture
drawHasse p o = Pictures $
  [ Color (maybe (rgbC 96 108 168) id (oEcol o a b)) (thickLine 2 (posOf p a) (posOf p b)) | (a, b) <- pCov p ]
  ++ concat [ [ Color bgCol (at (posOf p i) (disc (oRad o + 1)))
              , Color (maybe (rgbC 226 232 248) id (oCol o i)) (at (posOf p i) (disc (oRad o - 1)))
              , let (px, py) = posOf p i in sayM (labOf p i) px (py + 1) (max 14 (oFs o)) bgCol ]
            | i <- allOf p ]

-- (mx, my) にいちばん近い要素（r より遠ければ −1）
nodeAt :: Poset -> (Float, Float) -> Float -> Int
nodeAt p (mx, my) r = fst (foldl' f (-1, r) (allOf p))
  where f (best, bd) i = let (x, y) = posOf p i
                             d = sqrt ((x - mx) * (x - mx) + (y - my) * (y - my))
                         in if d < bd then (i, d) else (best, bd)

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rnd :: Rng -> (Double, Rng)
rnd (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- ランダムな順番 o で、前 → 後ろに確率 p で辺 → 閉包
randomPoset :: Int -> Double -> Rng -> (Poset, Rng)
randomPoset n p r0 = (posetFromRel (closeRel rel) [], r2)
  where
    shuffle (o, r) i = let (u, r') = rnd r
                           j = floor (u * fromIntegral (i + 1))
                           oi = o A.! i
                           oj = o A.! j
                       in (o A.// [(i, oj), (j, oi)], r')
    (ord, r1) = foldl' shuffle (A.listArray (0, n - 1) [0 .. n - 1] :: A.Array Int Int, r0) [n - 1, n - 2 .. 1]
    pick (acc, r) (a, b) = let (u, r') = rnd r in (if u < p then (ord A.! a, ord A.! b) : acc else acc, r')
    (edges, r2) = foldl' pick ([], r1) [ (a, b) | a <- [0 .. n - 1], b <- [a + 1 .. n - 1] ]
    rel = runSTUArray $ do
      m <- newArray ((0, 0), (n - 1, n - 1)) False
      forM_ edges $ \e -> writeArray m e True
      pure m

-- p5.js 版と同じ順番：clampv v a b（v を a〜b に収める）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else v

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

f2 :: Double -> Float
f2 v = if isNaN v || isInfinite v then 0 else realToFrac v

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (if isNaN v || isInfinite v then 0 else v) ""

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- gloss のマウス座標 → 左上が原点の座標
toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

bgCol :: Color
bgCol = rgbC 18 20 44

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃え（y は文字の下の線）、sayM は (x, y) が文字の真ん中
sayC, sayR, sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c
sayM s x y size c = say s (x - textW s size / 2) (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- ---- 自己テストの道具
-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 220 170 else rgbC 255 90 90)
