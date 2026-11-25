-- ============================================
-- 67-14 分布を聞く 🔊 ─ 8 つの音（ド〜高いド）の確率分布から、拍ごとに 1 音をくじ引きして鳴らす
-- 目標の分布（白い枠）へなめらかに近づける道を 4 通り：1 m-測地線（確率を混ぜる）2 e-測地線（対数で混ぜる）
-- 3 自然勾配（KL(目標‖p) をフィッシャー行列 7×7 の連立方程式で）4 ふつうの勾配（ソフトマックスの対数のまま）。道によってメロディの変わり方がちがう
-- 自然勾配の道は m-測地線とほぼ同じ（η 座標でまっすぐ目標へ）。いまの KL を表示
-- 操作：クリック 音 ON / 棒をドラッグ = いまの分布 / スペース 目標へ向かう・止まる / 1〜4 道 / T 次の目標 / ↑↓ テンポ
-- 音は Art.Sound の playTone（全体の音量 master は 0.2 以下）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (orange, violet, rose)
import Control.Monad (foldM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8, Word32)
import Numeric (showEFloat)
import System.Environment (lookupEnv)

notes :: [Double]
notes = [60, 62, 64, 65, 67, 69, 71, 72]      -- ★ 鳴らす音（MIDI：ド レ ミ ファ ソ ラ シ ド）

noteNames :: [String]
noteNames = ["Do", "Re", "Mi", "Fa", "Sol", "La", "Ti", "HiDo"]

-- ★ 目標の分布
targets :: [(String, [Double])]
targets =
  [ ("Do Mi Sol (chord)", [0.3, 0.02, 0.25, 0.02, 0.3, 0.02, 0.02, 0.07])
  , ("mostly high notes", [0.01, 0.01, 0.02, 0.04, 0.1, 0.2, 0.3, 0.32])
  , ("Re and La only", [0.01, 0.45, 0.01, 0.01, 0.01, 0.48, 0.01, 0.02])
  , ("uniform", [0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125]) ]

pathNames :: [String]
pathNames = ["m-geodesic", "e-geodesic", "natural gradient", "plain gradient"]

data World = World
  { wP       :: [Double]                      -- いまの分布
  , wStart   :: [Double]                      -- 動き始めの分布
  , wTgK     :: !Int
  , wPathK   :: !Int
  , wMoving  :: !Bool
  , wSMix    :: !Double
  , wTheta   :: [Double]                      -- 勾配の道の θ（logits）
  , wBpm     :: !Int
  , wNextBeat :: !Double                      -- 次の拍の時刻（秒、envTime）
  , wRoll    :: [Int]                         -- 鳴った音（新しい順、32 個まで）
  , wKlHist  :: [(Double, Double)]            -- (KL(p‖目標), KL(目標‖p))、古い順、300 個まで
  , wDragging :: !Bool
  , wAudio   :: !Bool                         -- 音 ON（クリックかキーで）
  , wRng     :: !Rng
  , wTest    :: [Check] }

main :: IO ()
main = runSketch (sketch "67-14 Listening to distributions" world0)
  { skBack  = col bgC
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World [0.05, 0.05, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2] [] 0 0 False 0 [] 150 0 [] [] False False (rngOf 14) []

barROf :: Env -> Box
barROf env = Box 70 130 (d2 (envW env) * 0.5) (d2 (envH env) * 0.36)

tgt :: World -> [Double]
tgt w = normP (snd (at' targets (wTgK w) ("", [])))

-- θ_i = log(p_i / p_8)（最後は 0）
logits :: [Double] -> [Double]
logits p = let l = safeLog (lastOr 1 p) in map (\v -> safeLog v - l) p

softmax :: [Double] -> [Double]
softmax th = let m = maxOr 0 th
                 e = map (\v -> exp (v - m)) th
                 s = sumOf e
             in map (/ s) e

-- フィッシャー行列 F = diag(p) − p pᵀ（最後の成分を除いた n × n）と勾配 p − t
fisherAndGrad :: [Double] -> [Double] -> ([[Double]], [Double])
fisherAndGrad p t = let n = length p - 1
                        pp = take n p
                    in ([ [ (if i == j then pi' else 0) - pi' * pj | (j, pj) <- zip [0 :: Int ..] pp ] | (i, pi') <- zip [0 ..] pp ]
                       , zipWith (-) pp (take n t))

-- ★ 自然勾配の 1 歩：∇_θ KL(t‖p) = p − t（7 成分）、F = diag(p) − p pᵀ（7×7）を解いて Δθ = −lr F⁻¹ ∇
naturalStep :: [Double] -> [Double] -> Double -> [Double]
naturalStep th t lr =
  let p = softmax th
      n = length p - 1
      (f, g) = fisherAndGrad p t
      d = solveLin f g
  in forceL [ if i < n then v - lr * clampv (at' d i 0) (-50) 50 else 0 | (i, v) <- zip [0 ..] th ]

plainStep :: [Double] -> [Double] -> Double -> [Double]
plainStep th t lr = let p = softmax th
                    in forceL [ if i < length th - 1 then v - lr * (at' p i 0 - at' t i 0) else 0 | (i, v) <- zip [0 ..] th ]

startMove :: World -> World
startMove w = w { wStart = wP w, wSMix = 0, wTheta = logits (wP w), wMoving = True, wKlHist = [] }

-- 1 フレーム分だけ目標へ近づける
advance :: World -> World
advance w =
  let t = tgt w
      w1 = if wPathK w <= 1
             then let s = min 1 (wSMix w + 1 / 360)
                      p = (if wPathK w == 0 then mixM else mixE) (wStart w) t s
                  in w { wSMix = s, wP = p, wMoving = s < 1 }
             else let th = (if wPathK w == 2 then naturalStep (wTheta w) t 0.012 else plainStep (wTheta w) t 0.25)
                      p = softmax th
                  in w { wTheta = th, wP = p, wMoving = not (divKL t p < 1e-5) }
  in w1 { wP = forceL (mixFloor (wP w1) 1e-6) }

pushRoll :: Int -> World -> World
pushRoll k w = w { wRoll = take 32 (k : wRoll w) }

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let w1 = if wMoving w0 then advance w0 else w0
      t = tgt w1
      fc = envFrame env
      w2 = if fc `mod` 3 == 0 then w1 { wKlHist = let h = wKlHist w1 ++ [(divKL (wP w1) t, divKL t (wP w1))] in if length h > 300 then drop 1 h else h } else w1
  -- ---- 拍ごとに 1 音
  if wAudio w2
    then do
      let now = d2 (envTime env)
          nb0 = if wNextBeat w2 < now then now + 0.05 else wNextBeat w2
          go w nb
            | nb < now + 0.1 = do
                let (k, g) = runR (sampleIndex (wP w)) (wRng w)
                tone (midiHz (at' notes k 60)) 0.35 Triangle 0.35
                go (pushRoll k w { wRng = g }) (nb + 60 / i2d (wBpm w))
            | otherwise = pure w { wNextBeat = nb }
      go w2 nb0
    else if fc `mod` max 1 (jsRound (3600 / i2d (wBpm w2))) == 0
      then let (k, g) = runR (sampleIndex (wP w2)) (wRng w2) in pure (pushRoll k w2 { wRng = g })
      else pure w2

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Hear it: " ++ (if wAudio w then "sound ON" else "click for sound") ++ " | path: " ++ at' pathNames (wPathK w) "" ++ " | target: " ++ fst (at' targets (wTgK w) ("", [])))
             ("Click sound ON  Drag bars  Space to target (" ++ (if wMoving w then "moving" else "stopped") ++ ")  1 m  2 e  3 natural  4 plain  T next target  Up/Down tempo " ++ show (wBpm w))
  , drawBars (wP w) barR barOpt { boNames = noteNames, boGhost = t, boVmax = 0.6 }
  , say "color = current distribution, white frame = target" (f2 (bxX barR)) (f2 (bxY barR) - 14) 14 (col dimc)
  -- ---- 右：KL の変化
  , drawAxes b "time" "KL(p||target) (orange), KL(target||p) (violet)"
  , plotLine b [ (i2d i, min (pbY1 b) a) | (i, (a, _)) <- zip [0 :: Int ..] (wKlHist w) ] (col orange) 2
  , plotLine b [ (i2d i, min (pbY1 b) c) | (i, (_, c)) <- zip [0 :: Int ..] (wKlHist w) ] (col violet) 2
  , say ("KL(p||target) = " ++ fmtN 4 (bitsOf kl) ++ " bits") (f2 rx) (by + 44) 16 (col ink)
  , say ("KL(target||p) = " ++ fmtN 4 (bitsOf klr) ++ " bits") (f2 rx) (by + 68) 16 (col ink)
  , say ("Entropy H(p) = " ++ fmtN 3 (bitsOf (entropyH (wP w))) ++ " bits") (f2 rx) (by + 96) 14 (col dimc)
  , say "  (max 3 = all 8 notes equally likely)" (f2 rx) (by + 114) 14 (col dimc)
  , say "m: notes that were 0 grow little by little" (f2 rx) (by + 138) 14 (col dimc)
  , say "e: notes near 0 hardly ever appear" (f2 rx) (by + 156) 14 (col dimc)
  -- ---- 下：鳴った音（新しいほど左）
  , say "Notes played (newest on the left, height = pitch)" 70 (ry - 10) 14 (col dimc) ]
  ++ [ Color (colA (255 - i2f i * 6) (outC k)) (rectTL (70 + i2f i * cw) (ry + i2f (7 - k) * rh) (cw - 3) (rh - 2)) | (i, k) <- zip [0 :: Int ..] (wRoll w) ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    barR = barROf env
    t = tgt w
    kl = divKL (wP w) t
    klr = divKL t (wP w)
    rx = bxX barR + bxW barR + 50
    b = plotBox rx 130 (d2 (envW env) - rx - 40) (d2 (envH env) * 0.3) 0 300 0 (maxOr 0.5 (0.5 : [ min 4 a | (a, _) <- wKlHist w ]))
    by = f2 (pbY b + pbH b)
    ry = f2 (bxY barR + bxH barR) + 70
    cw = min 34 ((envW env - 140) / 32)
    rh = 16

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let dr = inRect mx my (Box (bxX barR) (bxY barR - 20) (bxW barR) (bxH barR + 40))
    in dragged w { wAudio = True, wDragging = dr }
  EventMotion _ -> dragged w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDragging = False }
  EventKey k Down _ _ -> key k w { wAudio = True }
  _ -> w
  where
    barR = barROf env
    (mx, my) = mouseD env
    dragged w'
      | wDragging w' = w' { wMoving = False, wP = barsDrag (wP w') (Box (bxX barR) (bxY barR + bxH barR - bxH barR / 0.6) (bxW barR) (bxH barR / 0.6)) mx my 0.002 }
      | otherwise = w'
    restartIf w' = if wMoving w' then startMove w' else w'
    key k w' = case k of
      SpecialKey KeySpace -> if wMoving w' then w' { wMoving = False } else startMove w'
      SpecialKey KeyUp -> w' { wBpm = min 300 (wBpm w' + 10) }
      SpecialKey KeyDown -> w' { wBpm = max 60 (wBpm w' - 10) }
      Char c
        | c >= '1' && c <= '4' -> restartIf w' { wPathK = fromEnum c - fromEnum '1' }
        | c == 't' || c == 'T' -> restartIf w' { wTgK = (wTgK w' + 1) `mod` length targets }
      _ -> w'

-- ---------- やってみよう ----------
-- 1. 同じ目標に 1（m）と 2（e）で近づけて、メロディの変わり方を聞き比べよう。どちらが先に「和音らしく」なる？
-- 2. 3（自然勾配）と 1（m）の棒グラフの動きはほとんど同じ。4（ふつうの勾配）はどこがちがう？
-- 3. targets に自分の目標（たとえば「ドとソだけ」）を足そう
-- 4. notes を別の音階（ペンタトニック：60, 62, 64, 67, 69, 72, 74, 76）にしてみよう
-- 5. KL(p‖目標) と KL(目標‖p) はどちらが先に小さくなる？ 01 の「向き」を耳で感じよう

-- ---- 自己テスト
selfTest :: [Check]
selfTest =
  [ ("m と e の道は t = 0 で出発点、t = 1 で目標、途中も合計 1", okMix)
  , ("フィッシャー行列 7×7 の連立方程式 F x = ∇ の残差 < 1e-12、logits → softmax でもとにもどる"
    , and (zipWith (\v gv -> abs (v - gv) < 1e-12) r g) && and (zipWith (\v pv -> abs (v - pv) < 1e-12) q p0))
  , ("自然勾配の道は m-測地線から " ++ fmtN 4 devN ++ " しか離れない（ふつうの勾配は " ++ fmtN 3 devP ++ "）", devN < 0.01 && devP > 3 * devN)
  , ("自然勾配で KL(目標‖p) は毎歩減り、600 歩で " ++ fmtN 6 prevKL ++ " まで", mono && prevKL < 1e-3)
  , ("自然勾配の 1 歩は η（= 確率）で見ると −lr (p − 目標) の向き（まっすぐ目標へ）", okEta)
  , ("くじ引き 4 万回の割合は分布から 0.01 以内", and [ abs (i2d (cnt U.! i) / i2d nS - pv) < 0.01 | (i, pv) <- zip [0 ..] p0 ]) ]
  where
    p0 = normP [0.05, 0.05, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2]
    t = normP (snd (at' targets 0 ("", [])))
    near a b = and (zipWith (\x y -> abs (x - y) <= 1e-12) a b)
    okMix = and [ near (fn p0 t 0) p0 && near (fn p0 t 1) t && abs (sumOf (fn p0 t 0.4) - 1) <= 1e-12 | fn <- [mixM, mixE] ]
    th = logits p0
    q = softmax th
    (f, g) = fisherAndGrad q t
    r = matVec f (solveLin f g)
    distToM p = foldl' min posInf [ tvDist p (mixM p0 t (i2d k / 400)) | k <- [0 .. 400 :: Int] ]
    loop (!thN, !thP, !dN, !dP, !mo, !pv) s =
      let thN' = naturalStep thN t 0.012
          thP' = plainStep thP t 0.25
          pn = softmax thN'
          k = divKL t pn
          mo' = mo && not (k > pv + 1e-12)
          (dN', dP') = if s `mod` 20 == 0 then (max dN (distToM pn), max dP (distToM (softmax thP'))) else (dN, dP)
      in (thN', thP', dN', dP', mo', k)
    (_, _, devN, devP, mono, prevKL) = foldl' loop (th, th, 0, 0, True, posInf) [0 .. 599 :: Int]
    e0 = q
    e1 = softmax (naturalStep th t 1e-3)
    okEta = and [ not (abs ((a - b) - (-1e-3) * (b - tv)) > 1e-5) | (a, b, tv) <- zip3 e1 e0 t ]
    nS = 40000 :: Int
    cnt = withSeed 140 (loopR nS (\c -> do i <- sampleIndex p0
                                           pure (U.accum (+) c [(i, 1 :: Int)])) (U.replicate 8 0))

-- ==================================================
-- ここから下は「情報幾何の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。確率分布 p = [p1, p2, ...]（合計 1）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（等高線地図の色）
type RGB = (Float, Float, Float)

bgC, ink, orange, violet, teal, rose, lime, sand, dimc, panel, wire :: RGB
bgC = (22, 33, 52)                            -- 背景（深いスレートブルー）
ink = (240, 236, 226)                         -- 文字（紙の白）
orange = (255, 150, 75)
violet = (170, 140, 255)
teal = (95, 205, 190)
rose = (255, 110, 125)
lime = (150, 225, 120)
sand = (235, 210, 150)
dimc = (140, 152, 175)
panel = (32, 46, 70)
wire = (62, 80, 110)

-- 結果ごとの色（8 色をくり返す）
outC :: Int -> RGB
outC i = at' [(255, 150, 75), (95, 205, 190), (170, 140, 255), (235, 210, 150), (255, 110, 125), (150, 225, 120), (120, 175, 255), (230, 140, 210)] (i `mod` 8) ink

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

fin :: Double -> Double -> Double             -- NaN や ∞ なら d
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float                         -- 描くときの Float（NaN・∞ は 0）
f2 = realToFrac . fin 0

d2 :: Float -> Double                         -- マウスの位置などを計算用の Double に
d2 = realToFrac

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- リストの i 番目を f で変える
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

setAt :: Int -> a -> [a] -> [a]
setAt i v = modifyAt i (const v)

-- 厳密にリストを評価する（遅延評価の「つけ」をためない）
forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Int
jsRound v0 = let v = clampv v0 (-1e15) 1e15
                 r = floor v
             in if v - fromIntegral r >= 0.5 then r + 1 else r

-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）。∞ は inf
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = if d > 0 then ip ++ "." ++ fp else ip
                in if n == 0 || v > 0 then body else '-' : body

-- fmtP(p, d)：(0.50, 0.30, 0.20) の形
fmtP :: Int -> [Double] -> String
fmtP d p = "(" ++ intercalate ", " (map (fmtN d) p) ++ ")"

-- v.toExponential(1)：1.2e-15 の形
fmtE :: Double -> String
fmtE v = if isNaN v || isInfinite v then fmtN 2 v else plusE (showEFloat (Just 1) v "")
  where plusE s = case break (== 'e') s of
          (m, 'e' : '-' : ex) -> m ++ "e-" ++ ex
          (m, 'e' : ex) -> m ++ "e+" ++ ex
          _ -> s

-- 左上のタイトルと操作の説明（説明は \n で改行、行の間 19）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- 長方形 r = {x, y, w, h}
data Box = Box { bxX :: !Double, bxY :: !Double, bxW :: !Double, bxH :: !Double }

inRect :: Double -> Double -> Box -> Bool
inRect x y r = x >= bxX r && x <= bxX r + bxW r && y >= bxY r && y <= bxY r + bxH r

-- 等高線の地図（作っておいた画像と、置く場所）
data Field = Field { fdPic :: Picture, fdX :: !Float, fdY :: !Float, fdW :: !Float, fdH :: !Float, fdIW :: !Int, fdIH :: !Int }

drawField :: Maybe Field -> Picture           -- 等高線の地図（画像）を描く
drawField mf = case mf of
  Just f -> imageBox (fdX f) (fdY f) (fdW f) (fdH f) (fdIW f) (fdIH f) (fdPic f)
  Nothing -> Blank

-- 地図の作り方（p5.js 版の opt）
data FieldOpt = FieldOpt { foLevel :: !Double, foVmin :: !Double, foVmax :: !Double, foC0 :: RGB, foC1 :: RGB, foCap :: !Double, foRes :: !Int }

fieldOpt :: FieldOpt                          -- p5.js 版の既定の値（res は 0 = 道具ごとの既定）
fieldOpt = FieldOpt 0.1 0 1 (40, 60, 95) orange (1 / 0) 0

-- 色の帯と線の画像を作る（vals = 各画素の値、NaN は外 = 背景の色。edge の線は背景の上に重ねた色）
bandImage :: Int -> Int -> U.Vector Double -> (Double -> Int) -> (Double -> Double) -> FieldOpt -> Float -> Float -> Picture
bandImage gw gh vals bandOf tOf opt edgeV edgeA = pixelImage gw gh 1 px
  where
    (b0, b1, b2) = bgC
    blend a c bgc = a / 255 * c + (1 - a / 255) * bgc
    (c00, c01, c02) = foC0 opt
    (c10, c11, c12) = foC1 opt
    w8 :: Float -> Word8
    w8 v = fromIntegral (max 0 (min 255 (round v :: Int)))
    px i j =
      let k = j * gw + i
          v = vals U.! k
          band = bandOf v
          okN q = not (isNaN q)
          edge = v < foCap opt && ((i + 1 < gw && okN (vals U.! (k + 1)) && bandOf (vals U.! (k + 1)) /= band)
                                   || (j + 1 < gh && okN (vals U.! (k + gw)) && bandOf (vals U.! (k + gw)) /= band))
          t = realToFrac (clampv (tOf v) 0 1)
          sh = if odd band then 0.9 else 1
          mixc a b = (a + (b - a) * t) * sh
      in if isNaN v then (w8 b0, w8 b1, w8 b2)
         else if edge then (w8 (blend edgeA edgeV b0), w8 (blend edgeA edgeV b1), w8 (blend edgeA edgeV b2))
         else (w8 (mixc c00 c10), w8 (mixc c01 c11), w8 (mixc c02 c12))

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：種 s の列（0 なら 1）
rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 計算を種 s から実行して、結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

-- n 回くり返す（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- 標準正規分布（ボックス-ミュラー法、u = 1 − rnd() を先に引く）
gaussRnd :: R Double
gaussRnd = do
  u0 <- rnd
  v <- rnd
  pure (sqrt (-2 * log (1 - u0)) * cos (2 * pi * v))

-- でたらめな確率分布（k 個、どれも floor 以上）
rndDist :: Int -> Double -> R [Double]
rndDist k fl = do
  es <- mapM (\_ -> fmap (\u -> -log (1 - u) + 1e-9) rnd) [1 .. k]
  pure (mixFloor (normP es) fl)

-- p にしたがって番号を 1 つ（rnd() を 1 回）
sampleIndex :: [Double] -> R Int
sampleIndex p = do
  u <- rnd
  let go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- ---- 情報の道具：確率分布 p = [p1, p2, ...]（合計 1）。log(0) は 0·log 0 = 0 として扱う
tiny :: Double
tiny = 1e-300

posInf :: Double
posInf = 1 / 0

normP :: [Double] -> [Double]
normP p = let s = sumOf (map (max 0) p)
          in if not (s > 0) then map (const (1 / i2d (length p))) p else map (\v -> max 0 v / s) p

-- どの確率も f 以上に（0 を避ける）
mixFloor :: [Double] -> Double -> [Double]
mixFloor p f = if f == 0 then p else normP (map (max f) p)

safeLog :: Double -> Double
safeLog v = log (max v tiny)

-- エントロピー（ナット）
entropyH :: [Double] -> Double
entropyH = foldl' (\h v -> if v > 0 then h - v * log v else h) 0

-- KL ダイバージェンス D(p‖q)（ナット）。q が 0 で p > 0 なら ∞
divKL :: [Double] -> [Double] -> Double
divKL p q = go 0 (zip p (q ++ repeat 0))
  where go !d [] = max 0 d
        go !d ((a, b) : rest)
          | a <= 0 = go d rest
          | b <= 0 = posInf
          | otherwise = go (d + a * log (a / b)) rest

-- 交差エントロピー H(p, q)
crossH :: [Double] -> [Double] -> Double
crossH p q = foldl' (\h (a, b) -> if a > 0 then h - a * safeLog b else h) 0 (zip p q)

bhatta :: [Double] -> [Double] -> Double     -- Σ √(p q)
bhatta p q = min 1 (foldl' (\s (a, b) -> s + sqrt (max 0 (a * b))) 0 (zip p q))

-- フィッシャー-ラオ距離 = 球の大円の長さ
fisherRao :: [Double] -> [Double] -> Double
fisherRao p q = 2 * acos (clampv (bhatta p q) (-1) 1)

-- Σ(√p − √q)²
hellingerSq :: [Double] -> [Double] -> Double
hellingerSq p q = foldl' (\s (a, b) -> let d = sqrt a - sqrt b in s + d * d) 0 (zip p q)

tvDist :: [Double] -> [Double] -> Double     -- 全変動距離
tvDist p q = foldl' (\s (a, b) -> s + abs (a - b)) 0 (zip p q) / 2

-- α-ダイバージェンス（甘利の流儀）。α → −1 で D(p‖q)、α → +1 で D(q‖p)
alphaDiv :: [Double] -> [Double] -> Double -> Double
alphaDiv p q a
  | abs (a + 1) < 1e-12 = divKL p q
  | abs (a - 1) < 1e-12 = divKL q p
  | otherwise = let ea = (1 - a) / 2
                    eb = (1 + a) / 2
                    s = foldl' (\acc (x, y) -> acc + max x 0 ** ea * max y 0 ** eb) 0 (zip p q)
                in max 0 ((4 / (1 - a * a)) * (1 - s))

-- m-測地線：確率をそのまま混ぜる
mixM :: [Double] -> [Double] -> Double -> [Double]
mixM p q t = zipWith (\v w -> (1 - t) * v + t * w) p q

-- e-測地線：対数で混ぜて正規化
mixE :: [Double] -> [Double] -> Double -> [Double]
mixE p q t = normP (zipWith (\v w -> exp ((1 - t) * safeLog v + t * safeLog w)) p q)

-- α-測地線：u^((1−α)/2) で混ぜて正規化（α = −1 は m、α = +1 は e）
mixAlpha :: [Double] -> [Double] -> Double -> Double -> [Double]
mixAlpha p q t a
  | abs (a - 1) < 1e-9 = mixE p q t
  | otherwise = let ex = (1 - a) / 2
                in normP (zipWith (\v w -> ((1 - t) * v ** ex + t * w ** ex) ** (1 / ex)) p q)

bitsOf :: Double -> Double
bitsOf nat = nat / log 2

-- ---- 棒グラフ（ドラッグで高さを変えられる）
data BarOpt = BarOpt { boCols :: Int -> RGB, boNames :: [String], boVmax :: !Double, boGhost :: [Double] }

barOpt :: BarOpt                              -- 既定：結果の色、名前は 1 2 3 …、上限 1、重ねる分布なし
barOpt = BarOpt outC [] 1 []

drawBars :: [Double] -> Box -> BarOpt -> Picture
drawBars p r opt = Pictures $
  Color (col wire) (lineW 1 (f2 (bxX r), f2 (bxY r + bxH r)) (f2 (bxX r + bxW r), f2 (bxY r + bxH r)))
  : concat [ [ Color (colA 210 (boCols opt i)) (rectTL (x + i2f i * bw + 3) (y + h - hh) (bw - 6) hh)
             , if null (boGhost opt) then Blank
               else let gh = f2 (clampv (at' (boGhost opt) i 0) 0 vmax / vmax) * h
                    in Color (col ink) (frameTL 1.5 (x + i2f i * bw + 3) (y + h - gh) (bw - 6) gh)
             , if bw >= 34 then sayC (fmtN 2 v) (x + (i2f i + 0.5) * bw) (y + h - hh - 4) 14 (col ink) else Blank   -- 細い棒では数字を省く
             , sayC (if null (boNames opt) then show (i + 1) else at' (boNames opt) i "") (x + (i2f i + 0.5) * bw) (y + h + 17) 14 (col dimc) ]
           | (i, v) <- zip [0 ..] p, let hh = f2 (clampv v 0 vmax / vmax) * h ]
  where k = length p
        x = f2 (bxX r)
        y = f2 (bxY r)
        h = f2 (bxH r)
        bw = f2 (bxW r) / i2f (max 1 k)
        vmax = boVmax opt

-- 棒をドラッグ：i 番目の高さを変えて、残りを比例で縮める（合計 1 のまま）
barsDrag :: [Double] -> Box -> Double -> Double -> Double -> [Double]
barsDrag p r mx my fl0 =
  let k = length p
      fl = if fl0 == 0 then 0.001 else fl0
      i = floor ((mx - bxX r) / (bxW r / i2d k)) :: Int
      v = clampv ((bxY r + bxH r - my) / bxH r) fl (1 - fl * i2d (k - 1))
      rest = 1 - at' p i 0
      q = [ if j == i then v else if rest > 1e-9 then (pj / rest) * (1 - v) else (1 - v) / i2d (k - 1) | (j, pj) <- zip [0 ..] p ]
  in if i < 0 || i >= k || my < bxY r - 30 || my > bxY r + bxH r + 20 then p else mixFloor q fl

-- ---- 小さな連立一次方程式 A x = b（ガウスの消去法、部分ピボット）
solveLin :: [[Double]] -> [Double] -> [Double]
solveLin a b = backSub (elim 0 (zipWith (\r v -> r ++ [v]) a b))
  where
    n = length b
    safeD d = if abs d < 1e-300 then 1e-300 else d
    elim c m
      | c >= n = m
      | otherwise =
          let col' = [ abs (at' (at' m r []) c 0) | r <- [0 .. n - 1] ]
              piv = foldl' (\pv r -> if at' col' r 0 > at' col' pv 0 then r else pv) c [c + 1 .. n - 1]
              rc = at' m piv []
              rp = at' m c []
              m1 = [ if r == c then rc else if r == piv then rp else row | (r, row) <- zip [0 ..] m ]
              d = safeD (at' rc c 0)
              m2 = [ if r <= c then row else let f = at' row c 0 / d in [ if k >= c then v - f * at' rc k 0 else v | (k, v) <- zip [0 ..] row ]
                   | (r, row) <- zip [0 :: Int ..] m1 ]
          in elim (c + 1) (map forceL m2)
    backSub m = foldl' (\x r -> let row = at' m r []
                                    s = foldl' (\acc k -> acc - at' row k 0 * at' x k 0) (at' row n 0) [r + 1 .. n - 1]
                                in setAt r (s / safeD (at' row r 0)) x) (replicate n 0) [n - 1, n - 2 .. 0]

matVec :: [[Double]] -> [Double] -> [Double]
matVec a x = [ foldl' (+) 0 (zipWith (*) r x) | r <- a ]

-- ---- 2 次元の図の枠（データ座標 ↔ 画面）
data PBox = PBox { pbX :: !Double, pbY :: !Double, pbW :: !Double, pbH :: !Double, pbX0 :: !Double, pbX1 :: !Double, pbY0 :: !Double, pbY1 :: !Double }

plotBox :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> PBox
plotBox = PBox

pbBox :: PBox -> Box                          -- 枠の長方形（inRect 用）
pbBox b = Box (pbX b) (pbY b) (pbW b) (pbH b)

toScr :: PBox -> Double -> Double -> (Double, Double)
toScr b u v = (pbX b + (u - pbX0 b) / (pbX1 b - pbX0 b) * pbW b, pbY b + pbH b - (v - pbY0 b) / (pbY1 b - pbY0 b) * pbH b)

toScrF :: PBox -> Double -> Double -> (Float, Float)
toScrF b u v = let (x, y) = toScr b u v in (f2 x, f2 y)

fromScr :: PBox -> Double -> Double -> (Double, Double)
fromScr b sx sy = (pbX0 b + (sx - pbX b) / pbW b * (pbX1 b - pbX0 b), pbY0 b + (pbY b + pbH b - sy) / pbH b * (pbY1 b - pbY0 b))

drawAxes :: PBox -> String -> String -> Picture
drawAxes b xl yl = Pictures
  [ Color (col wire) (frameTL 1 x y w h)
  , sayT xl (x + w / 2) (y + h + 16) 14 (col dimc) True
  , sayT (fmtN 1 (pbX0 b)) x (y + h + 2) 14 (col dimc) False
  , sayR (fmtN 1 (pbX1 b)) (x + w) (y + h + 14) 14 (col dimc)
  , sayRM (fmtN 1 (pbY0 b)) (x - 4) (y + h) 14 (col dimc)
  , sayRM (fmtN 1 (pbY1 b)) (x - 4) y 14 (col dimc)
  , say yl x (y - 5) 14 (col dimc) ]
  where x = f2 (pbX b)
        y = f2 (pbY b)
        w = f2 (pbW b)
        h = f2 (pbH b)

-- データ座標の点列を折れ線に（枠の外の部分は枠のふちで切る）
plotLine :: PBox -> [(Double, Double)] -> Color -> Float -> Picture
plotLine b pts c w = Color c (Pictures (map (polyline w . map (\(x, y) -> (f2 x, f2 y))) (clipRuns b pts)))

-- 枠の中に入る部分の折れ線の列（リャン-バースキー法で線分を切る）
clipRuns :: PBox -> [(Double, Double)] -> [[(Double, Double)]]
clipRuns b pts = finish (foldl' stepP ([], [], Nothing) pts)
  where
    x0 = pbX b - 1
    x1 = pbX b + pbW b + 1
    y0 = pbY b - 1
    y1 = pbY b + pbH b + 1
    flush run done = if length run >= 2 then reverse run : done else done
    finish (run, done, _) = reverse (flush run done)
    clip (ax, ay) (cx, cy) =
      let dx = cx - ax
          dy = cy - ay
          go (t0, t1) (p, q)
            | p == 0 = if q < 0 then Nothing else Just (t0, t1)
            | otherwise = let r = q / p in Just (if p < 0 then (max t0 r, t1) else (t0, min t1 r))
          res = foldl' (\acc pq -> acc >>= \tt -> go tt pq) (Just (0, 1)) [(-dx, ax - x0), (dx, x1 - ax), (-dy, ay - y0), (dy, y1 - ay)]
      in case res of
           Just (t0, t1) | t0 <= t1 -> Just (t0, t1)
           _ -> Nothing
    stepP (run, done, prev) (u, v) =
      let s@(sx, sy) = toScr b u v
          good = not (isNaN sx || isNaN sy || isInfinite sx || isInfinite sy)
      in if not good then ([], flush run done, Nothing) else
         case prev of
           Nothing -> (run, done, Just s)
           Just pv@(px, py) -> case clip pv s of
             Nothing -> ([], flush run done, Just s)
             Just (t0, t1) ->
               let pa = (px + t0 * (sx - px), py + t0 * (sy - py))
                   pb = (px + t1 * (sx - px), py + t1 * (sy - py))
                   (run1, done1) = if t0 > 0 then ([pa], flush run done) else if null run then ([pa], done) else (run, done)
                   run2 = pb : run1
               in if t1 < 1 then ([], flush run2 done1, Just s) else (run2, done1, Just s)

-- 長方形の上の等高線の地図（fn(u, v)）
gridField :: PBox -> (Double -> Double -> Double) -> FieldOpt -> Field
gridField b fn opt =
  let res = if foRes opt > 0 then foRes opt else 3
      gw = ceiling (pbW b / i2d res) :: Int
      gh = ceiling (pbH b / i2d res) :: Int
      vals = U.generate (gw * gh) $ \k ->
        let (j, i) = k `divMod` gw
            (u, v) = fromScr b (pbX b + (i2d i + 0.5) * i2d res) (pbY b + (i2d j + 0.5) * i2d res)
        in fn u v
      lv = foLevel opt
      vmin = foVmin opt
      vmax = foVmax opt
      bandOf v = floor (clampv ((v - vmin) / lv) (-1e6) 1e6) :: Int
      pic = bandImage gw gh (U.map (\v -> if isNaN v then -1e300 else v) vals) bandOf (\v -> (v - vmin) / (vmax - vmin)) opt 235 170
  in vals `seq` Field pic (f2 (pbX b)) (f2 (pbY b)) (i2f (gw * res)) (i2f (gh * res)) gw gh

-- ---- 音の道具（Art.Sound）。全体の音量 master は 0.2 以下
master :: Double
master = 0.12                                 -- ★ 全体の音量（0.2 以下にしておく）

-- 1 音：周波数 [Hz]、長さ [秒]、波形、音量（p5.js 版の playNote と同じ音量 × master）
tone :: Double -> Double -> Wave -> Double -> IO ()
tone freq dur wave vol = playTone (f2 freq) (f2 (min 0.2 master * vol)) (f2 dur) wave

midiHz :: Double -> Double
midiHz m = 440 * 2 ** ((m - 69) / 12)

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

mouseD :: Env -> (Double, Double)             -- 計算用の Double で
mouseD env = let (x, y) = mouseP env in (d2 x, d2 y)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 左揃え・右揃えで上下は中央（textAlign(LEFT, CENTER)、(RIGHT, CENTER)）
sayLM, sayRM :: String -> Float -> Float -> Float -> Color -> Picture
sayLM s x y size c = say s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c

-- 上が y の文字（textAlign(…, TOP)）。center = True なら中央揃え
sayT :: String -> Float -> Float -> Float -> Color -> Bool -> Picture
sayT s x y size c center = (if center then sayC else say) s x (y + size * 0.85) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (max 0 d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (max 0 d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> thickPath w pts
  _ -> Blank

-- 点線（長さ on の線と off のすき間をくり返す）
dashLine :: Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine w on off (x1, y1) (x2, y2) =
  let l = hyp (x2 - x1) (y2 - y1)
      n = floor (l / (on + off)) :: Int
      ux = if l == 0 then 0 else (x2 - x1) / l
      uy = if l == 0 then 0 else (y2 - y1) / l
  in Pictures [ lineW w (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + on), y1 + uy * min l (s + on)) | k <- [0 .. n], let s = i2f k * (on + off), s < l ]

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（iw × ih の画像）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral (max 1 iw)) (negate h / fromIntegral (max 1 ih)) pic)

-- ---- 自己テストの道具
type Check = (String, Bool)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [Check] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestOk :: [Check] -> Bool
selfTestOk = all snd

selfTestLine :: [Check] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [Check] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col lime else col rose)
