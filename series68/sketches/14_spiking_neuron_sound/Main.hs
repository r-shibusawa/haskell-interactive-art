-- ============================================
-- 68-14 スパイクするニューロンの音 🔊 ─ 漏れ積分発火（LIF）ニューロン：τ dV/dt = −V + R I
-- 膜電位 V はたまっては漏れる。しきい値 θ をこえたら「発火」（スパイク）して 0 に戻る。発火のたびに音を 1 つ鳴らす
-- 一定の入力なら発火の間隔は T = τ ln(RI / (RI − θ))（RI ≤ θ なら永遠に発火しない）→ 頻度 f = 1/T（F-I 曲線）
-- 計算は「正確な指数の式」：1 ステップの間に V = RI + (V − RI) e^(−dt/τ)。しきい値をこえる時刻もステップの中で正確に求める
-- 5 個をつなぐ：興奮（マゼンタ、相手の V を上げる）と抑制（ライム、下げる）のつなぎ方でリズムや「勝ち残り」が変わる
-- 操作：クリック 音を出す / ↑↓ 入力の強さ / 1〜4 つなぎ方 / スペース 一時停止 / M 消音
-- 時間は 1/8 のスローモーション（ニューロンの 1 秒 = 画面の 8 秒）。音量は小さめ（master は 0.2 以下）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (violet)
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when, unless)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.), (.&.), shiftL, testBit)
import Data.List (foldl', intercalate, sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (floatToDigits)
import System.Environment (lookupEnv)

tauM, theta, dtS :: Double
tauM = 0.02                -- ★ 時定数 20 ms
theta = 1                  -- ★ しきい値
dtS = 0.0001               -- ★ 計算の刻み 0.1 ms

timeScale :: Double
timeScale = 1 / 8          -- ★ スローモーション

numN :: Int
numN = 5                   -- ★ ニューロンの数

noteMidi :: [Int]
noteMidi = [60, 64, 67, 71, 74]   -- ★ 音の高さ（ド ミ ソ シ レ）

presets :: [String]
presets = ["separate (no links)", "excitatory ring 1->2->3->4->5->1", "inhibition: winner takes all (pair 1,2 vs pair 3,4)", "mixed (random)"]

-- 膜電位とつながり（wSyn i j = j が発火したとき i の V に足す量）
type Syn = U.Vector Double

data World = World
  { wPreset  :: !Int
  , wDrive   :: !Double
  , wV       :: !Vec
  , wSyn     :: !Syn
  , wT       :: !Double                 -- シミュレーションの時刻 [秒]
  , wSpikes  :: ![(Double, Int)]        -- 最近 1.5 秒の発火（古い順）
  , wTrace   :: ![Vec]                  -- 膜電位の記録（新しい順、400 個まで）
  , wFlash   :: ![Int]
  , wPaused  :: !Bool
  , wMuted   :: !Bool
  , wAudio   :: !Bool                   -- クリックかキーで音が使えるようになる（p5.js の startAudio）
  , wFI      :: ![(Double, Double)]     -- F-I 曲線のシミュレーション
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "68-14 Spiking neuron sound" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      _ <- evaluate (sum (map snd (wFI world0)))
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World 0 1.25 (U.fromList [0.1, 0.3, 0.5, 0.7, 0.9]) (presetSyn 0) 0 [] [] (replicate numN 0) False False False
               [ (ri, simRate ri 1.5) | ri <- [1.05, 1.1, 1.2, 1.35, 1.5, 1.75, 2, 2.5, 3] ] []

-- ★ F-I の公式：f = 1 / (τ ln(RI / (RI − θ)))、RI ≤ θ なら 0
rateFormula :: Double -> Double
rateFormula ri = if ri > theta then 1 / (tauM * log (ri / (ri - theta))) else 0

-- ★ 1 ステップ（長さ dt）を正確に進める。しきい値をこえる時刻 t* = τ ln((RI − V)/(RI − θ)) を求め、0 に戻して残りを進める
-- 戻り値 = 新しい V と、このステップの中の発火の時刻（ステップの始めからの秒）
lifStep :: Double -> Double -> Double -> (Double, [Double])
lifStep v0 ri dt = go v0 dt 0 []
  where
    go v left used out
      | left > 0 =
          if ri > theta && v < theta
            then let tc = tauM * log ((ri - v) / (ri - theta))
                 in if tc <= left then go 0 (left - tc) (used + tc) (used + tc : out) else finish v left out
            else if v >= theta then go 0 left used (used : out) else finish v left out
      | otherwise = (v, reverse out)
    finish v left out = (ri + (v - ri) * exp (-left / tauM), reverse out)

-- 1 個だけで dur 秒。発火の間隔の平均から頻度を
simRate :: Double -> Double -> Double
simRate ri dur =
  let n = jsRound (dur / dtS)
      go !k !v ts | k >= n = ts
                  | otherwise = let (v', sp) = lifStep v ri dtS in go (k + 1) v' (foldl' (\acc t -> i2d k * dtS + t : acc) ts sp)
      times = reverse (go 0 0 [])
  in if length times < 2 then 0 else i2d (length times - 1) / (lastOr 0 times - headOr 0 times)

presetSyn :: Int -> Syn
presetSyn k = U.replicate (numN * numN) 0 U.// links
  where
    ix i j = i * numN + j
    links = case k of
      1 -> [ (ix ((j + 1) `mod` numN) j, 0.45) | j <- [0 .. numN - 1] ]
      2 -> [ (ix i j, -0.8) | (i, j) <- [(2, 0), (3, 0), (2, 1), (3, 1), (0, 2), (1, 2), (0, 3), (1, 3)] ]
           ++ [ (ix i j, 0.25) | (i, j) <- [(1, 0), (0, 1), (3, 2), (2, 3)] ]
      3 -> withSeed 1414 (fmap concat (mapM (\(i, j) -> if i == j then pure [] else do
                                                 u <- rnd
                                                 if u < 0.5 then do u2 <- rnd
                                                                    pure [(ix i j, if u2 < 0.6 then 0.3 else -0.5)]
                                                 else pure []) [ (i, j) | i <- [0 .. numN - 1], j <- [0 .. numN - 1] ]))
      _ -> []

driveOf :: Double -> Int -> Double      -- 少しずつちがう入力（全部同じだとそろってしまう）
driveOf drive i = drive * (1 + 0.04 * i2d i)

-- ネットワークを dur 秒ぶん進める（発火は次のステップで相手に届く = 遅れ 0.1 ms）
-- 戻り値：新しい V、記録に足す膜電位（古い順）、発火（時刻, 番号）
simNetwork :: Double -> Syn -> Double -> Double -> Vec -> (Vec, [Vec], [(Double, Int)])
simNetwork drive syn t0 dur v0 = go 0 v0 [] []
  where
    n = jsRound (dur / dtS)
    go !k v tr fired
      | k >= n = (v, reverse tr, reverse fired)
      | otherwise =
          let res = [ lifStep (v U.! i) (driveOf drive i) dtS | i <- [0 .. numN - 1] ]
              v1 = U.fromList (map fst res)
              now = [ i | (i, (_, sp)) <- zip [0 ..] res, not (null sp) ]
              v2 = foldl' (\vv j -> U.imap (\i x -> let s = syn U.! (i * numN + j) in if s /= 0 then clampv (x + s) (-2) (theta + 0.5) else x) vv) v1 now
              fired' = foldl' (\acc j -> (t0 + i2d k * dtS, j) : acc) fired now
              tr' = if k `mod` 20 == 0 then v2 : tr else tr
          in go (k + 1) v2 tr' fired'

step :: Env -> Float -> World -> IO World
step _ _ w0
  | wPaused w0 = pure w0 { wFlash = map (\f -> max 0 (f - 1)) (wFlash w0) }
  | otherwise = do
      let (v, tr, fired) = simNetwork (wDrive w0) (wSyn w0) (wT w0) (timeScale / 60) (wV w0)
          n = jsRound ((timeScale / 60) / dtS)
          t' = wT w0 + i2d n * dtS
      when (wAudio w0 && not (wMuted w0)) $
        forM_ (take 4 fired) $ \(_, j) -> playTone (midiHz (at' noteMidi j 60)) (0.35 * master) 0.25 Triangle
      let fl0 = foldl' (\fl (_, j) -> [ if q == j then 10 else f | (q, f) <- zip [0 ..] fl ]) (wFlash w0) fired
      pure w0 { wV = v, wT = t', wTrace = take 400 (reverse tr ++ wTrace w0)
              , wSpikes = dropWhile (\(t, _) -> t < t' - 1.5) (wSpikes w0 ++ fired)
              , wFlash = map (\f -> max 0 (f - 1)) fl0 }

cols5 :: [RGB]
cols5 = [mag, lime, sky, amber, Main.violet]

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Spiking neuron sound: " ++ at' presets (wPreset w) "" ++ "   input RI = " ++ fmtN (wDrive w) 2 ++ " (theta = 1)   "
              ++ (if wAudio w then (if wMuted w then "muted" else "sound ON") else "click for sound"))
             ("click sound on   Up/Down input strength   1-4 wiring   Space " ++ (if wPaused w then "resume" else "pause") ++ "   M mute   (1/8 slow motion)")
  -- ---- 左上：つながり
  , Pictures [ let (ax, ay) = pos j
                   (bx, by) = pos i
                   dx = bx - ax
                   dy = by - ay
                   l = sqrt (dx * dx + dy * dy)
                   ux = dx / l
                   uy = dy / l
                   ox = -uy * 6
                   oy = ux * 6
                   s = wSyn w U.! (i * numN + j)
                   c = if s > 0 then mag else lime
               in Pictures [ Color (colA 200 c) (lineW (1 + 4 * f2 (abs s)) (ax + ux * 24 + ox, ay + uy * 24 + oy) (bx - ux * 30 + ox, by - uy * 30 + oy))
                           , Color (col c) (circleAt (bx - ux * 28 + ox) (by - uy * 28 + oy) 8) ]
             | i <- [0 .. numN - 1], j <- [0 .. numN - 1], wSyn w U.! (i * numN + j) /= 0 ]
  , Pictures [ let (px, py) = pos i
                   f = at' (wFlash w) i 0
                   d = 40 + 2 * i2f f
               in Pictures [ Color (col ink) (circleAt px py (d + 3)), Color (col (if f > 0 then amber else panel)) (circleAt px py d)
                           , sayM (show (i + 1)) px py 15 (col (if f > 0 then bgCol else ink)) ]
             | i <- [0 .. numN - 1] ]
  , Pictures [ say s 40 (250 + 110 + 50 + 18 * i2f q) 14 (col sub) | (q, s) <- zip [0 :: Int ..] ["magenta = excitatory (+)", "lime = inhibitory (-)", "dot = arrow head"] ]
  -- ---- 右上：膜電位
  , plotAxes g1 ("time -> (last " ++ show (jsRound (400 * 20 * dtS * 1000)) ++ " ms)") "membrane potential V (neurons 1-5 from the bottom, dashed = threshold)" [] []
  , Pictures [ Color (col wire) (dashed 4 4 (grX g1, gY g1 thr) (grX g1 + grW g1, gY g1 thr)) | i <- [0 .. numN - 1], let thr = i2d i * 1.2 + theta - 0.6 ]
  , Pictures [ plotXY g1 (map i2d [0 .. length trs - 1]) [ i2d i * 1.2 + clampv (r U.! i) (-0.6) 1.1 - 0.6 | r <- trs ] (col (at' cols5 i ink)) 1.5 | i <- [0 .. numN - 1] ]
  -- ---- 右中：ラスター
  , plotAxes g2 "time (last 1.5 s)" "raster: spike times (one row per neuron)" [] []
  , Pictures [ let x = gX g2 t
                   y = gY g2 (i2d j)
               in Color (col (at' cols5 j ink)) (lineW 2 (x, y - 7) (x, y + 7)) | (t, j) <- wSpikes w ]
  -- ---- 下：F-I 曲線
  , plotAxes g3 "input RI" "F-I curve: line = formula 1/(tau ln(RI/(RI - theta))), dots = simulation [Hz]" [1, 2, 3] [0, 50, 100]
  , plotXY g3 ris (map rateFormula ris) (col amber) 2.5
  , Pictures [ Color (col ink) (circleAt (gX g3 ri) (gY g3 f) 8) | (ri, f) <- wFI w ]
  , Color (col mag) (Line [(gX g3 (wDrive w), grY g3), (gX g3 (wDrive w), grY g3 + grH g3)])
  , say ("rate now (last 1.5 s): " ++ intercalate " / " [ fmtN (i2d c / 1.5) 0 | c <- cnt ] ++ " Hz") tx (grY g3 + 20) 14 (col ink)
  , say ("neuron 1 alone, by the formula: " ++ fmtN (rateFormula (driveOf (wDrive w) 0)) 1 ++ " Hz") tx (grY g3 + 44) 14 (col ink)
  , Pictures [ say s tx (grY g3 + 76 + 20 * i2f q) 14 (col sub)
             | (q, s) <- zip [0 :: Int ..] [ "RI <= theta = 1: V only rises to RI, no spikes (rate 0)", "large RI: f ~ RI/(tau theta), almost proportional"
                                         , "excitatory ring: a neighbour's spike fires you sooner", "inhibition: the pair that fires first silences the other" ] ]
  , drawSelfTest env (wTest w) ]
  where
    pos i = let a = -pi / 2 + 2 * pi * i2f i / i2f numN in (190 + 110 * cos a, 250 + 110 * sin a)
    rx = 400
    g1 = Graph rx 105 (envW env - rx - 40) ((envH env - 220) * 0.38) 0 400 (-1.2) (i2d numN * 1.2)
    g2 = Graph rx (grY g1 + grH g1 + 50) (grW g1) ((envH env - 220) * 0.22) (wT w - 1.5) (wT w) (-0.5) (i2d numN - 0.5)
    g3y = grY g2 + grH g2 + 60
    g3 = Graph 70 g3y (envW env * 0.45) (envH env - g3y - 70) 0.8 3 0 120
    trs = reverse (wTrace w)
    ris = [ 0.8 + i2d k * 0.011 | k <- [0 .. 200 :: Int] ]
    tx = grX g3 + grW g3 + 40
    cnt = [ length [ () | (_, j) <- wSpikes w, j == i ] | i <- [0 .. numN - 1] ]

event :: Env -> Event -> World -> IO World
event _ ev w0 = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wDrive = min 3 (i2d (jsRound ((wDrive w + 0.05) * 100)) / 100) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wDrive = max 0.8 (i2d (jsRound ((wDrive w - 0.05) * 100)) / 100) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> let k = fromEnum c - fromEnum '1' in w { wPreset = k, wSyn = presetSyn k }
    | c `elem` "mM" -> w { wMuted = not (wMuted w) }
  EventKey _ Down _ _ -> w
  _ -> w0
  where w = w0 { wAudio = True }               -- クリックかキーで音を使えるようにする

-- ---------- やってみよう ----------
-- 1. ↓ で入力を 1 以下にすると音が止まる。ぎりぎり 1.05 だと？（公式で何 Hz？）
-- 2. 2 の興奮の輪で、入力を少しずつ上げる。リズムはどう変わる？
-- 3. 3 の抑制で、presetSyn の −0.8 を −0.2 にすると、負けた組も鳴るようになる？
-- 4. noteMidi を変えて、自分の音階にする。timeScale を 1/4 にすると？

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("シミュレーションの頻度 = 公式 1/(τ ln(RI/(RI − θ)))（RI = 1.1〜3、ずれ " ++ fmtN (100 * worst) 4 ++ "% < 2%）", worst < 0.02)
  , ("最初の発火の時刻 = τ ln(RI/(RI − θ))（ずれ " ++ fmtE (abs (first - tauM * log 3)) ++ " 秒）", near first (tauM * log 3) 1e-12)
  , ("RI = 0.9 ≤ θ：2 秒たっても発火しない、公式も 0", simRate 0.9 2 == 0 && rateFormula 0.9 == 0 && rateFormula 1 == 0)
  , ("入力が大きいと f ≈ RI/(τθ) − 1/(2τ)（RI = 20 で " ++ fmtN (rateFormula big) 1 ++ " Hz）", abs (rateFormula big / (big / (tauM * theta) - 1 / (2 * tauM)) - 1) < 0.02)
  , ("F-I 曲線は単調に増える", mono)
  , ("興奮の輪：となりからの入力で 3 番の発火が増える（" ++ show alone ++ " → " ++ show ring ++ " 回/秒）", ring > alone) ]
  where
    worst = foldl' (\m ri -> max m (abs (simRate ri 1 / rateFormula ri - 1))) 0 [1.1, 1.5, 2, 3]
    first = let go k v | k >= (2000 :: Int) = 0 / 0
                       | otherwise = let (v', sp) = lifStep v 1.5 dtS in case sp of
                                       (t : _) -> i2d k * dtS + t
                                       [] -> go (k + 1) v'
            in go 0 0
    big = 20
    ris = takeWhile (< 4) (iterate (+ 0.05) 1.01)
    mono = and (zipWith (<) (0 : map rateFormula ris) (map rateFormula ris))
    count pr = let (_, _, f) = simNetwork 1.2 (presetSyn pr) 0 1 (U.replicate numN 0) in length (filter ((== 2) . snd) f)
    alone = count 0
    ring = count 1

-- ==================================================
-- ここから下は「神経回路網の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。座標は p5.js 版と同じ（左上が原点、y は下向き）で、view で gloss の座標に直す
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（神経細胞：深い青緑の黒、あたたかい白、シナプスのマゼンタ、軸索のライム）
type RGB = (Double, Double, Double)
type Vec = U.Vector Double                    -- ベクトル（unboxed）

bgCol, ink, mag, lime, amber, sky, violet, sub, panel, wire :: RGB
bgCol = (8, 22, 24)                           -- 背景（深い青緑の黒）
ink = (246, 238, 224)                         -- 文字（あたたかい白）
mag = (232, 72, 160)
lime = (184, 232, 76)
amber = (250, 186, 72)
sky = (96, 196, 236)
violet = (168, 140, 250)
sub = (150, 178, 172)
panel = (16, 38, 40)
wire = (44, 76, 76)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Double -> RGB -> Color                -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Double -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

clampI :: Int -> Int -> Int -> Int
clampI v a b = max a (min b v)

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

f2 :: Double -> Float                         -- 描くときの Float（NaN と無限大は 0）
f2 v = if isNaN v || isInfinite v then 0 else realToFrac v

d2 :: Float -> Double
d2 = realToFrac

isFin :: Double -> Bool
isFin v = not (isNaN v || isInfinite v)

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

sumOf :: [Double] -> Double
sumOf = foldl' (+) 0

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else sumOf a / i2d (length a)

sigm :: Double -> Double
sigm z = 1 / (1 + exp (-(clampv z (-40) 40)))

-- JavaScript の Math.round（.5 は大きいほうへ）と Math.floor
jsRound :: Double -> Int
jsRound v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e15) 1e15)

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

-- 最初にいちばん大きい値の番号（indexOf(Math.max(...))）
argMax :: [Double] -> Int
argMax xs = fst (foldl' (\(bi, bv) (i, v) -> if v > bv then (i, v) else (bi, bv)) (0, headOr 0 xs) (zip [0 ..] xs))

-- 数を書く（p5.js 版の fmtN：toFixed(d)、"-0.00" は "0.00" に）
fmtN :: Double -> Int -> String
fmtN v d
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = (if v < 0 && n > 0 then "-" else "") ++ body
  where
    n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
    ds = show n
    padded = replicate (d + 1 - length ds) '0' ++ ds
    (ip, fp) = splitAt (length padded - d) padded
    body = if d > 0 then ip ++ "." ++ fp else ip

-- p5.js 版の fmtE：toExponential(1)（例 6.7e-9）。0 ちょうどは "0"
fmtE :: Double -> String
fmtE v
  | isNaN v || isInfinite v = "?"
  | v == 0 = "0"
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          pick e = (floor (a / (10 ^^ (e - 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= 100 then pick (e0 + 1) else if n < 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= 100 then (n1 `div` 10, e1 + 1) else (n1, e1)
          ds = show n2
      in (if v < 0 then "-" else "") ++ take 1 ds ++ "." ++ drop 1 ds ++ "e" ++ (if e2 >= 0 then "+" else "-") ++ show (abs e2)

-- JavaScript の数の書き方（String(v)：いちばん短い 10 進数）
jsNum :: Double -> String
jsNum v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0"
  | v < 0 = '-' : jsNum (negate v)
  | otherwise =
      let (ds0, e) = floatToDigits 10 v
          ds = concatMap show ds0
          k = length ds
      in if e >= k && e <= 21 then ds ++ replicate (e - k) '0'
         else if e > 0 && e <= 21 then take e ds ++ "." ++ drop e ds
         else if e > -6 && e <= 0 then "0." ++ replicate (negate e) '0' ++ ds
         else take 1 ds ++ (if k > 1 then "." ++ drop 1 ds else "") ++ "e" ++ (if e - 1 >= 0 then "+" else "-") ++ show (abs (e - 1))

-- +v.toPrecision(k)（有効数字 k 桁に丸めた数）と、その書き方（目盛りの数）
roundPrec :: Int -> Double -> Double
roundPrec k v
  | not (isFin v) || v == 0 = 0
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          top = 10 ^ k :: Integer
          pick e = (floor (a / (10 ^^ (e - k + 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= top then pick (e0 + 1) else if n < top `div` 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= top then (n1 `div` 10, e1 + 1) else (n1, e1)
          r = fromRational (fromIntegral n2 * 10 ^^ (e2 - k + 1)) :: Double
      in if v < 0 then negate r else r

precN :: Int -> Double -> String
precN k = jsNum . roundPrec k

prec3 :: Double -> String
prec3 = precN 3

-- 左上のタイトルと操作の説明（説明は "\n" で 2 行にできる）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col sub) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- mulberry32 は状態に毎回同じ数 0x6d2b79f5 を足すだけ → k 回目の乱数は「状態 + k × その数」から直接作れる
rndOut :: Word32 -> Double
rndOut a =
  let t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296

rngStep :: Word32
rngStep = 0x6d2b79f5

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：列を種 s から始め直す（0 なら 1）
seedRng :: Int -> R ()
seedRng s = R (\_ -> let w = fromIntegral s :: Word32 in ((), Rng (if w == 0 then 1 else w)))

rngOf :: Int -> Rng
rngOf s = snd (runR (seedRng s) (Rng 1))

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double                               -- 0〜1 の一様乱数
rnd = R (\(Rng s) -> let a = s + rngStep in (rndOut a, Rng a))

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * i2d (b - a + 1)))

-- 標準正規分布（ボックス-ミュラー、u = 1 − rnd() なので log(0) にならない）
randn :: R Double
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- rnd() を n 回、まとめて（unboxed のベクトルに）
rndVec :: Int -> R (U.Vector Double)
rndVec n = R (\(Rng s) -> (U.generate n (\k -> rndOut (s + fromIntegral (k + 1) * rngStep)), Rng (s + fromIntegral n * rngStep)))

-- randn() を n 回、まとめて
randnVec :: Int -> R (U.Vector Double)
randnVec n = R (\(Rng s) -> (U.generate n (gauss s), Rng (s + fromIntegral (2 * n) * rngStep)))
  where gauss s k = let u = 1 - rndOut (s + fromIntegral (2 * k + 1) * rngStep)
                        v = rndOut (s + fromIntegral (2 * k + 2) * rngStep)
                    in sqrt (-2 * log u) * cos (2 * pi * v)

-- フィッシャー-イェーツのシャッフル（後ろから i と 0〜i のどこかを入れかえる）
shuffleU :: U.Unbox a => U.Vector a -> R (U.Vector a)
shuffleU v = do
  let n = U.length v
  js <- mapM (\i -> do u <- rnd; pure (i, floor (u * i2d (i + 1)))) [n - 1, n - 2 .. 1]
  pure (U.modify (\mv -> mapM_ (\(i, j) -> MU.swap mv i j) js) v)

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (col wire) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (col sub) | v <- xt ]
  ++ [ sayRM (prec3 v) (grX g - 5) (gY g v) 14 (col sub) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 36) 14 (col ink), say yl (grX g) (grY g - 8) 14 (col ink) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ 1.5 までは gloss の細い線）
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1.5 then Line pts else thickPath w pts
  _ -> Blank

-- (xs[i], ys[i]) を折れ線で（有限の値だけ）
plotXY :: Graph -> [Double] -> [Double] -> Color -> Float -> Picture
plotXY g xs ys c w = Color c (polyline w [ (gX g x, gY g y) | (x, y) <- zip xs ys, isFin y ])

-- 凡例：items = [(色, 名前)]
legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures
  [ Pictures [Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col ink)]
  | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- ---- 音の道具（Art.Sound）。全体の音量 master は 0.2 以下
master :: Float
master = 0.1                                  -- ★ 全体の音量（0.2 以下にしておく）

midiHz :: Int -> Float
midiHz m = 440 * 2 ** ((i2f m - 69) / 12)

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）、右揃え・左揃えで上下は中央
sayM, sayRM, sayLM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c
sayLM s x y size c = say s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = i2f (length s) * size * 0.72

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
frameTL wt x y w h = if wt <= 1.2 then Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
                     else thickPath wt [(x - wt / 2, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分（細いときは gloss の線）
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW wt a b = if wt <= 1.5 then Line [a, b] else thickLine wt a b

-- 線分を箱（左上 x, y、幅 w、高さ h）の中だけに切る（p5.js 版の clip の代わり）
clipSeg :: Float -> Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Maybe ((Float, Float), (Float, Float))
clipSeg bx by bw bh (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
      edges = [(-dx, x1 - bx), (dx, bx + bw - x1), (-dy, y1 - by), (dy, by + bh - y1)]
      step (t0, t1) (p, q)
        | p == 0 = if q < 0 then (1, 0) else (t0, t1)
        | p < 0 = (max t0 (q / p), t1)
        | otherwise = (t0, min t1 (q / p))
      (a, b) = foldl' step (0, 1) edges
  in if a > b then Nothing else Just ((x1 + a * dx, y1 + a * dy), (x1 + b * dx, y1 + b * dy))

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashed on off (x1, y1) (x2, y2) =
  let l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
      k = max 1 (ceiling (l / (on + off))) :: Int
      p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
      fr = on / (on + off)
  in Pictures [ Line [p (i2f i / i2f k), p ((i2f i + fr) / i2f k)] | i <- [0 .. k - 1] ]

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = isFin a && abs (a - b) <= tol

relErr :: Double -> Double -> Double
relErr a b = abs (a - b) / max 1e-12 (abs a + abs b)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then makeColorI 150 230 170 255 else makeColorI 255 90 110 255)
