-- ============================================
-- 61-14 クラスタの音 ─ 点が「音符」。横 = 時間（左から右へ再生）、縦 = 音の高さ
--   クラスタごとに楽器（音色）と和音が決まっている：0 サイン波・C の和音 / 1 三角波・Am / 2 矩形波・F / 3 のこぎり波・G
--   点の高さは、そのクラスタの和音の音（2 オクターブ分）に合わせる → 同じクラスタの点はいつも和音になる
--   スペースで k-means を半ステップ進める。割り当てで点が別のクラスタに移ると、その点の音色と和音が変わる（光る）
--   音は Art.Sound で鳴らす（少し遅れて鳴る）。音量は小さめ（全体の音量 master は 0.2 以下）
-- 操作：クリック 点を置く（音が鳴る）/ スペース 半ステップ / A 1 小節ごとに自動 / ↑↓ k / M 消音 / R やり直し
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (forM, forM_, replicateM, when)
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.List (foldl', sort, sortBy)
import Data.Bits (shiftR, xor, (.|.))
import Data.Word (Word32)
import Control.Monad (forM, forM_, replicateM, when, foldM)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed.Mutable as UM
import Numeric (showFFloat)
import System.Environment (lookupEnv)

bar :: Double
bar = 4                    -- ★ 1 小節の秒数

waves :: [Wave]
waves = [Sine, Triangle, Square, Sawtooth]

waveName :: Int -> String
waveName c = headOr "" (drop (c `mod` 4) ["sine", "triangle", "square", "sawtooth"])

chords :: [[Int]]
chords = [[0, 4, 7], [9, 12, 16], [5, 9, 12], [7, 11, 14]]   -- ★ C・Am・F・G（ド = 0 からの半音の数）

chordName :: Int -> String
chordName c = headOr "" (drop (c `mod` 4) ["C", "Am", "F", "G"])

waveVol :: [Double]
waveVol = [0.5, 0.45, 0.18, 0.15]     -- ★ 音色ごとの音量（矩形波・のこぎり波はうるさいので小さく）

data World = World
  { wPts    :: !Pts
  , wCs     :: !Pts                  -- 中心
  , wLab    :: !(U.Vector Int)
  , wK      :: !Int
  , wNextAssign :: !Bool             -- 次の半ステップは ① 割り当てか
  , wAuto   :: !Bool
  , wMuted  :: !Bool
  , wFlash  :: !(U.Vector Int)       -- 光る残りフレーム
  , wT0     :: !(Maybe Float)        -- 音を ON にした時刻（Nothing = まだ）
  , wLastPhase :: !Double
  , wQueue  :: ![(Float, Int)]       -- これから鳴らす点（時刻, 点の番号）
  , wMsg    :: !String
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "61-14 Cluster sounds" world0)
  { skBack  = rgbC 22 24 34
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = reset (World V.empty V.empty U.empty 3 False False False U.empty Nothing 0 [] "" [])

reset :: World -> World
reset w = w { wPts = pts, wCs = cs, wLab = assignLabels pts cs, wNextAssign = False, wFlash = U.replicate (V.length pts) 0, wMsg = "" }
  where
    pts = withSeed 141 $ V.fromList . concat <$> forM [0 .. 2] (\g -> replicateM 7 $ do
            a <- gauss
            b <- gauss
            pure (pt2 (clampv 0.02 0.98 (0.15 + 0.33 * g + 0.09 * a)) (clampv 0.03 0.97 (0.3 + 0.2 * g + 0.12 * b))))
    cs = withSeed 7 (initRandomCenters pts (wK w))

midiToFreq :: Int -> Double
midiToFreq m = 440 * 2 ** (fromIntegral (m - 69) / 12)

-- ★ 点の高さ y（0 = 下、1 = 上）→ クラスタ c の和音の音（C4 = 60 から 2 オクターブ）
noteOf :: Double -> Int -> Int
noteOf y c = headOr 60 (drop i tones)
  where
    tones = [ 60 + 12 * o + s - 12 | o <- [0, 1], s <- headOr [] (drop (c `mod` 4) chords) ]
    i = max 0 (min (length tones - 1) (floorI (y * fromIntegral (length tones))))

noteName :: Int -> String
noteName m = headOr "" (drop (m `mod` 12) ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]) ++ show (m `div` 12 - 1)

playPoint :: World -> Int -> IO ()
playPoint w i = when (not (wMuted w) && wT0 w /= Nothing && i < V.length (wPts w)) $ do
  let c = wLab w U.! i
  tone (midiToFreq (noteOf (ptY (wPts w V.! i)) c)) (headOr Sine (drop (c `mod` 4) waves)) 0.35 (headOr 0 (drop (c `mod` 4) waveVol))

-- ★ 半ステップ。割り当てで別のクラスタに移った点の番号も返す
halfStep :: World -> (World, [Int])
halfStep w
  | wNextAssign w = let nl = assignLabels (wPts w) (wCs w)
                        moved = [ i | i <- [0 .. U.length nl - 1], nl U.! i /= wLab w U.! i ]
                    in (w { wLab = nl, wNextAssign = False }, moved)
  | otherwise = (w { wCs = updateCenters (wPts w) (wLab w) (wCs w), wNextAssign = True }, [])

doStep :: Float -> World -> World
doStep now w0 =
  let (w, moved) = halfStep w0
  in w { wFlash = wFlash w U.// [ (i, 60) | i <- moved ]
       , wQueue = wQueue w ++ [ (now + 0.05 + 0.12 * fromIntegral r, i) | (r, i) <- zip [0 :: Int ..] moved ]
       , wMsg = (if wNextAssign w then "(2) update: centres move to the mean" else "(1) assignment: " ++ show (length moved) ++ " points moved to another cluster")
                ++ " (J = " ++ fmt 4 (inertiaOf (wPts w) (wCs w) (wLab w)) ++ ")" }

-- 音を ON にする（p5.js 版の startAudio と、再生の開始時刻）
startAudio :: Env -> World -> World
startAudio env w = case wT0 w of
  Nothing -> w { wT0 = Just (envTime env) }
  Just _ -> w

-- 再生：縦の線が点を通りすぎたら鳴らす。予約した音も時刻が来たら鳴らす
step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let now = envTime env
      phase = case wT0 w0 of
        Just t0 -> let u = realToFrac (now - t0) / bar in u - fromIntegral (floor u :: Int)
        Nothing -> 0
      lp = wLastPhase w0
      crossed = [ i | Just _ <- [wT0 w0], (i, p) <- zip [0 ..] (V.toList (wPts w0))
                    , let x = ptX p, if lp <= phase then x > lp && x <= phase else x > lp || x <= phase ]
      (due, later) = L.partition ((<= now) . fst) (wQueue w0)
  forM_ crossed (playPoint w0)
  forM_ (map snd due) (playPoint w0)
  let w1 = w0 { wFlash = U.map (\f -> max 0 (f - 1)) (wFlash w0 U.// [ (i, max (wFlash w0 U.! i) 12) | i <- crossed ])
              , wQueue = later, wLastPhase = phase }
  pure (if wT0 w0 /= Nothing && phase < lp && wAuto w0 then doStep now w1 else w1)

viewOf :: Env -> G
viewOf env = G 40 100 (envW env - 380) (envH env - 190) 0 1 0 1

draw :: Env -> World -> Picture
draw env w = view env $
  -- ボロノイの領域（粗いマス）
  [ Color (palCA (nearestCenter (pt2 ((fromIntegral gx + 0.5) / 40) (1 - (fromIntegral gy + 0.5) / 24)) cs) 28)
          (rectTL (pX g + fromIntegral gx * pW g / 40) (pY g + fromIntegral gy * pH g / 24) (pW g / 40 + 1) (pH g / 24 + 1))
  | V.length cs > 0, gy <- [0 .. 23 :: Int], gx <- [0 .. 39 :: Int] ]
  ++ [ Color (grayC 80) (frameTL 1 (pX g) (pY g) (pW g) (pH g))
     , Color (rgbaC 255 255 255 120) (lineW 2 (gX g phase, pY g) (gX g phase, pY g + pH g)) ]
  -- 点と中心
  ++ concat [ [ Color (rgbaC 255 255 255 (min 255 (4 * fl))) (circleAt x y (18 + fromIntegral fl * 0.6)) | fl > 0 ]
              ++ [ Color (grayC 20) (circleAt x y 18), Color (palC c) (circleAt x y 16)
                 , say (noteName (noteOf (ptY p) c)) (x + 10) (y - 6) 14 (grayC 200) ]
            | (i, p) <- zip [0 ..] (V.toList (wPts w)), let c = wLab w U.! i, let x = gX g (ptX p), let y = gY g (ptY p), let fl = wFlash w U.! i ]
  ++ concat [ [ Color white (rectTL (gX g (ptX cn) - 11) (gY g (ptY cn) - 11) 22 22), Color (palC j) (rectTL (gX g (ptX cn) - 9) (gY g (ptY cn) - 9) 18 18) ]
            | (j, cn) <- zip [0 ..] (V.toList cs) ]
  -- ---- 右：クラスタの楽器と和音
  ++ concat [ [ Color (palC j) (rectTL bx (120 + fromIntegral j * 52) 22 22)
              , say ("cluster " ++ show j ++ ": " ++ waveName j) (bx + 34) (136 + fromIntegral j * 52) 15 (grayC 220)
              , say ("chord " ++ chordName j ++ "   " ++ show (U.length (U.filter (== j) (wLab w))) ++ " points") (bx + 34) (155 + fromIntegral j * 52) 15 (grayC 220) ]
            | j <- [0 .. wK w - 1] ]
  ++ [ sayWrap (wMsg w) bx 346 280 14 (rgbC 255 220 140)
     , sayWrap "Letters next to the points = note names. Even at the same height, a point that changes cluster is snapped to the new chord, so its pitch may change a little." bx 436 280 14 (grayC 150)
  -- ---- 文字
     , say ("Cluster sounds   k = " ++ show (wK w) ++ "   next: " ++ (if wNextAssign w then "(1) assignment (points may move)" else "(2) update")
            ++ (if wT0 w /= Nothing then (if wMuted w then "   muted" else "") else "   [click: sound ON]")) 40 40 20 (grayC 240)
     , say ("Click: add point   Space half step   A auto every bar" ++ (if wAuto w then " (ON)" else "") ++ "   Up/Down k   M mute   R reset") 40 66 15 (grayC 170)
     , drawSelfTest env (wTest w) ]
  where
    g = viewOf env
    cs = wCs w
    phase = wLastPhase w
    bx = envW env - 310

event :: Env -> Event -> World -> IO World
event env ev w0 = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> do
    let (mx, my) = toP5 env pos
        g = viewOf env
    if inBox g mx my && V.length (wPts w) < 80
      then do
        let p = pt2 (clampv 0 1 (invX g mx)) (clampv 0 1 (invY g my))
            w' = w { wPts = V.snoc (wPts w) p, wLab = U.snoc (wLab w) (nearestCenter p (wCs w)), wFlash = U.snoc (wFlash w) 40 }
        playPoint w' (V.length (wPts w))
        pure w'
      else pure w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (doStep (envTime env) w)
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "mM" -> pure w { wMuted = not (wMuted w) }
    | c `elem` "rR" -> pure (reset w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (changeK 1)
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (changeK (-1))
  _ -> pure w0
  where
    w = case ev of
      EventKey _ Down _ _ -> startAudio env w0
      _ -> w0
    changeK d = let k = max 2 (min 4 (wK w + d))
                    cs = withSeed (7 + fromIntegral k) (initRandomCenters (wPts w) k)
                in w { wK = k, wCs = cs, wLab = assignLabels (wPts w) cs, wNextAssign = False }

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 14・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("J never increases at any half step (20 cases)", all fst runs)
  , ("points change cluster only at the assignment step, and move to the nearest centre", all snd runs)
  , ("a point at any height plays a note of its cluster's chord (C, Am, F, G)", inChord)
  , ("pitches stay in 100..1000 Hz, A4 = 440 Hz, A5 = 880 Hz", range && near (midiToFreq 69) 440 1e-12 && near (midiToFreq 81) 880 1e-12)
  , ("master volume = " ++ show master ++ " <= 0.2, each note volume <= 0.5", master <= 0.2 && all (<= 0.5) waveVol)
  , ("clusters 0..3 have different timbres and chords", length (L.nub (map waveName [0 .. 3])) == 4 && length (L.nub (map chordName [0 .. 3])) == 4) ]
  where
    runs = withSeed 14 $ forM [0 .. 19 :: Int] $ \t -> do
      pts <- V.fromList <$> replicateM 25 (do { x <- rnd; y <- rnd; pure (pt2 x y) })
      cs <- initRandomCenters pts (2 + t `mod` 3)
      let w0 = world0 { wPts = pts, wCs = cs, wLab = assignLabels pts cs, wNextAssign = False }
          go :: Int -> World -> Double -> (Bool, Bool)
          go s w prev
            | s >= 12 = (True, True)
            | otherwise =
                let wasAssign = wNextAssign w
                    (w', mv) = halfStep w
                    j = inertiaOf (wPts w') (wCs w') (wLab w')
                    mono = not (j > prev + 1e-12)
                    only = not (not wasAssign && not (null mv))
                           && not (wasAssign && or [ wLab w' U.! i /= nearestCenter (wPts w' V.! i) (wCs w') | i <- [0 .. V.length pts - 1] ])
                    (a, b) = go (s + 1) w' j
                in (mono && a, only && b)
      pure (go 0 w0 (inertiaOf pts cs (wLab w0)))
    ys = takeWhile (<= 1) (iterate (+ 0.01) 0)
    notes = [ (c, noteOf y c) | c <- [0 .. 3], y <- ys ]
    inChord = and [ any (\s -> (m - 60 - s) `mod` 12 == 0) (headOr [] (drop c chords)) | (c, m) <- notes ]
    range = and [ f >= 100 && f <= 1000 | (_, m) <- notes, let f = midiToFreq m ]

-- ---------- やってみよう ----------
-- 1. まずクリックで音 ON。スペースを押していき、割り当てのときに光った点の音が変わるのを聞こう
-- 2. A で自動にすると、1 小節ごとに半ステップ進む。止まったら（J が変わらない）音楽も同じくり返しになる
-- 3. 2 つのクラスタのあいだに点を置くと？ どちらの楽器になるかは、近い中心しだい
-- 4. ↑↓ で k を 4 にすると G の和音（のこぎり波）が加わる。chords を変えて自分のコード進行にしよう
-- 5. 横 = 時間なので、クラスタが「左のほう」に集まると曲の前半、右なら後半に鳴る

-- ==================================================
-- ここから下は「クラスタリングの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。点は U.Vector Double（[x, y] や [r, g, b]）、
-- 点の集まりは V.Vector（p5.js 版の配列の配列）、ラベルは U.Vector Int（−1 = ノイズ）
-- ==================================================
-- ---- 共通：クラスタの色・距離（点は [x, y] や [r, g, b] のような U.Vector Double）
type P = U.Vector Double
type Pts = V.Vector P

pt2 :: Double -> Double -> P                -- 2 次元の点 [x, y]
pt2 x y = U.fromList [x, y]

ptX, ptY :: P -> Double                     -- x 座標・y 座標（なければ 0）
ptX p = if U.length p > 0 then U.unsafeIndex p 0 else 0
ptY p = if U.length p > 1 then U.unsafeIndex p 1 else 0

palette :: [(Int, Int, Int)]
palette = [(176, 140, 255), (110, 230, 190), (255, 190, 80), (110, 180, 255), (255, 120, 130), (200, 240, 110), (255, 150, 220), (90, 210, 220), (255, 150, 70), (190, 190, 255)]

palRGB :: Int -> (Int, Int, Int)            -- クラスタ k の色（k < 0 = ノイズ = 灰色）
palRGB k | k < 0 = (120, 120, 135)
         | otherwise = headOr (0, 0, 0) (drop (k `mod` length palette) palette)

palC :: Int -> Color
palC k = palCA k 255

palCA :: Int -> Int -> Color                -- 不透明度 a（0〜255）つき
palCA k a = let (r, g, b) = palRGB k in rgbaC r g b a

sqd :: P -> P -> Double                     -- 距離の 2 乗
sqd a b = go 0 0
  where
    n = min (U.length a) (U.length b)
    go !d !s | d >= n = s
             | otherwise = let e = U.unsafeIndex a d - U.unsafeIndex b d in go (d + 1) (s + e * e)
{-# INLINE sqd #-}

edist :: P -> P -> Double                   -- ユークリッド距離
edist a b = sqrt (sqd a b)

meanOf :: [Double] -> Double
meanOf [] = 0
meanOf xs = foldl' (+) 0 xs / fromIntegral (length xs)

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ。gauss の 2 つ目の値もとっておく）
data Rng = Rng !Word32 !(Maybe Double)

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s) Nothing

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s sp) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (fromIntegral (t2 `xor` (t2 `shiftR` 14)) :: Int) / 4294967296, Rng a sp)   -- Int を通すと速い

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> let (a, s1) = g s in runR (k a) s1)

-- 種 s で始めて結果だけ取り出す（seedRng(s) してから計算）
withSeed :: Word32 -> R a -> a
withSeed s m = fst (runR m (seedRng s))

seedR :: Word32 -> R ()                     -- 計算の途中で seedRng(s)
seedR s = R (\_ -> ((), seedRng s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int               -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

gauss :: R Double                           -- 標準正規分布（ボックス-ミュラー法）
gauss = R $ \(Rng s sp) -> case sp of
  Just g -> (g, Rng s Nothing)
  Nothing ->
    let (u0, r1) = rndStep (Rng s Nothing)
        (v, Rng s2 _) = rndStep r1
        u = 1 - u0
        r = sqrt (-2 * log u)
    in (r * cos (2 * pi * v), Rng s2 (Just (r * sin (2 * pi * v))))

-- ---- k-means（ロイドのアルゴリズム）。目的関数 = 慣性 = Σ |x_i − 中心|²
nearestCenter :: P -> Pts -> Int
nearestCenter p cs = go 0 0 (1 / 0)
  where
    k = V.length cs
    go !j !b !bd
      | j >= k = b
      | otherwise = let d = sqd p (V.unsafeIndex cs j) in if d < bd then go (j + 1) j d else go (j + 1) b bd

assignLabels :: Pts -> Pts -> U.Vector Int  -- ① 割り当て
assignLabels pts cs = U.generate (V.length pts) (\i -> nearestCenter (V.unsafeIndex pts i) cs)

inertiaOf :: Pts -> Pts -> U.Vector Int -> Double
inertiaOf pts cs labels = U.ifoldl' (\s i l -> s + sqd (pts V.! i) (cs V.! l)) 0 (U.take (V.length pts) labels)

-- ② 更新（仲間の平均。仲間 0 なら動かさない）
updateCenters :: Pts -> U.Vector Int -> Pts -> Pts
updateCenters pts labels cs
  | V.null cs = cs
  | otherwise = V.generate k (\j -> let c = cnt U.! j in if c > 0 then U.generate dim (\d -> sums U.! (j * dim + d) / fromIntegral c) else cs V.! j)
  where
    k = V.length cs
    dim = U.length (V.head cs)
    cnt = U.accumulate (+) (U.replicate k (0 :: Int)) (U.map (\l -> (l, 1)) labels)
    sums = runST $ do
      s <- UM.replicate (k * dim) 0
      forM_ [0 .. V.length pts - 1] $ \i -> do
        let l = labels U.! i
            p = pts V.! i
        forM_ [0 .. min dim (U.length p) - 1] $ \d -> UM.unsafeModify s (+ U.unsafeIndex p d) (l * dim + d)
      U.freeze s

initRandomCenters :: Pts -> Int -> R Pts    -- ランダムに k 点えらぶ（重ならないように）
initRandomCenters pts k
  | V.null pts = pure (V.replicate k (pt2 0 0))
  | otherwise = go 0 (U.enumFromN 0 n) []
  where
    n = V.length pts
    go j idx out
      | j >= min k n = pure (V.fromList (reverse out ++ replicate (k - length out) (V.head pts)))
      | otherwise = do
          u <- rnd
          let r = j + floor (u * fromIntegral (n - j))
              idx' = idx U.// [(j, idx U.! r), (r, idx U.! j)]
          go (j + 1) idx' (pts V.! (idx' U.! j) : out)

-- k-means++：今ある中心から遠い点ほど（距離² に比例して）えらばれやすい
initPlusPlus :: Pts -> Int -> R Pts
initPlusPlus pts k
  | V.null pts = pure (V.replicate k (pt2 0 0))
  | otherwise = do
      u <- rnd
      let c0 = pts V.! min (n - 1) (floor (u * fromIntegral n))
      loop [c0] (U.generate n (\i -> sqd (pts V.! i) c0))
  where
    n = V.length pts
    loop out d2
      | length out >= k = pure (V.fromList (reverse out))
      | otherwise = do
          let tot = U.foldl' (+) 0 d2
          u <- rnd
          pick <- if tot > 0 then pure (scan (u * tot) 0) else (\v -> min (n - 1) (floor (v * fromIntegral n))) <$> rnd
          let c = pts V.! pick
          loop (c : out) (U.imap (\i v -> min v (sqd (pts V.! i) c)) d2)
      where scan r i | i >= n = n - 1
                     | r - d2 U.! i <= 0 = i
                     | otherwise = scan (r - d2 U.! i) (i + 1)

-- k-means の結果。hist = 半ステップ（割り当て・更新）ごとの慣性、all = やり直しごとの慣性
data KM = KM { kmCenters :: !Pts, kmLabels :: !(U.Vector Int), kmInertia :: !Double, kmHist :: ![Double], kmIters :: !Int, kmAll :: ![Double] }

-- 最後まで回す（maxIter 回まで。0 なら 100 回）
lloyd :: Pts -> Pts -> Int -> KM
lloyd pts c0 maxIter = go 0 c0 l0 [inertiaOf pts c0 l0]
  where
    mi = if maxIter <= 0 then 100 else maxIter
    l0 = assignLabels pts c0
    go it cs ls hist
      | it >= mi = finish it cs ls hist
      | otherwise =
          let nc = updateCenters pts ls cs
              h1 = inertiaOf pts nc ls
              nl = assignLabels pts nc
              h2 = inertiaOf pts nc nl
              hist' = h2 : h1 : hist
          in if nl == ls then finish (it + 1) nc nl hist' else go (it + 1) nc nl hist'
    finish it cs ls hist = KM cs ls (headOr 0 hist) (reverse hist) it []

-- restarts 回やり直して慣性がいちばん小さいものを選ぶ（usePP = k-means++ で始める）
kmeansBest :: Pts -> Int -> Int -> Bool -> Int -> R KM
kmeansBest pts k restarts usePP maxIter = do
  rs <- forM [1 .. max 1 restarts] $ \_ -> do
    c0 <- if usePP then initPlusPlus pts k else initRandomCenters pts k
    pure (lloyd pts c0 maxIter)
  let best = foldl' (\b r -> if kmInertia r < kmInertia b then r else b) (headOr (KM V.empty U.empty 0 [] 0 []) rs) (drop 1 rs)
  pure best { kmAll = map kmInertia rs }

-- ---- グラフと図の道具：G = 画面の四角（左上 x, y、幅、高さ）とデータの範囲（p5.js と同じ、左上が原点）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv (-0.02) 1.02 ((v - x0 g) / (x1 g - x0 g))) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv (-0.02) 1.02 ((v - y0 g) / (y1 g - y0 g))) * pH g

invX :: G -> Float -> Double                -- 画面の x → データの x
invX g sx = x0 g + realToFrac ((sx - pX g) / pW g) * (x1 g - x0 g)

invY :: G -> Float -> Double
invY g sy = y0 g + realToFrac ((pY g + pH g - sy) / pH g) * (y1 g - y0 g)

inBox :: G -> Float -> Float -> Bool
inBox g sx sy = sx >= pX g && sx <= pX g + pW g && sy >= pY g && sy <= pY g + pH g

-- 縦横の縮尺をそろえる（span = 短い辺に入るデータの幅、(cx, cy) = まん中）
fitView :: Float -> Float -> Float -> Float -> Double -> Double -> Double -> G
fitView x y w h cx cy sp = G x y w h (cx - hw) (cx + hw) (cy - hh) (cy + hh)
  where
    s = realToFrac (min w h) / sp
    hw = realToFrac w / 2 / s
    hh = realToFrac h / 2 / s

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (grayC 95) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 18) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 38) 14 (grayC 205), say yl (pX g) (pY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- クラスタの色で点を描く（ラベルがなければ白）。r = 直径、al = 不透明度（0〜255）
drawPts :: G -> Pts -> Maybe (U.Vector Int) -> Float -> Int -> Picture
drawPts g pts labels r al = Pictures
  [ Color (maybe (rgbaC 230 230 240 al) (\ls -> palCA (if i < U.length ls then ls U.! i else 0) al) labels) (circleAt (gX g (ptX p)) (gY g (ptY p)) r)
  | (i, p) <- zip [0 ..] (V.toList pts) ]

-- ---- 音の道具（Art.Sound）。音量の上限：全体の音量 master は 0.2 以下
master :: Float
master = 0.1                                -- ★ 全体の音量（0.2 以下にしておく）

-- 1 音（周波数 Hz、波形、長さ 秒、音量 0〜1）
tone :: Double -> Wave -> Double -> Double -> IO ()
tone freq wave dur vol = playTone (realToFrac (clampv 40 4000 freq)) (min 0.2 master * realToFrac (clampv 0 1 vol)) (realToFrac dur) wave

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

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

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double   -- clampv 下限 上限 値
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = let s = showFFloat (Just n) (fin 0 v) "" in if all (`elem` "-0.") s && take 1 s == "-" then drop 1 s else s

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
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = not . any isNaN

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下に小さく（view の中で使う）
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
