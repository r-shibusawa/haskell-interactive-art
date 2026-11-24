-- ============================================
-- 66-14 オートマトンの作曲 🔊 ─ 状態ごとに音を決めておき、入力の 0 と 1 の列に沿って 1 拍に 1 歩ずつ歩かせる
-- 同じ入力でも機械（矢印）を変えると曲が変わる。入力を読み終えたとき受理状態（二重丸）にいれば「終わりの和音」、
-- そうでなければ「まだ続く」音で次の小節へ。矢印のない記号を読むと行き止まり（その小節は休み）
-- 操作：スペース 再生・停止 / 0 1 入力に足す / Backspace 消す / R でたらめな入力 / ↑↓ テンポ（音量は小さめから）
-- 編集：空いた所をクリック = 状態 / 状態から状態へドラッグ → 0 か 1 のキー = 矢印 / ダブルクリック = 受理 / Shift + ドラッグ = 動かす /
--       K 選んだ状態の音を変える / X 選んだ状態を消す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (cyan, rose, violet)
import Control.Monad (when)
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- ★ 状態に割り当てる音（名前, 音の種類）
data Snd = Kick | Snare | Hat | Note Int | Rest deriving (Eq)

sounds :: [(String, Snd)]
sounds = [("Kick", Kick), ("Snare", Snare), ("Hat", Hat), ("Do", Note 60), ("Mi", Note 64), ("So", Note 67), ("La", Note 69), ("HiDo", Note 72), ("Rest", Rest)]

master :: Double
master = 0.12                                 -- ★ 全体の音量（0.2 以下にしておく）

r0 :: Float
r0 = 30                                       -- 状態の円の半径

data World = World
  { wMach    :: EdM                           -- 編集できる DFA
  , wEd      :: Ed                            -- 編集の途中の様子
  , wSnd     :: [Int]                         -- 状態ごとの音（sounds の番号）
  , wWord    :: String                        -- 入力（1 小節）
  , wBeat    :: !Int                          -- いま何拍目（−1 = まだ）
  , wPlaying :: !Bool
  , wLastBeat :: !Float                       -- 最後に拍を打った時刻（ミリ秒）
  , wCurQ    :: !Int
  , wRoll    :: [Int]                         -- 鳴った音（ピアノロール）
  , wFlash   :: !Float
  , wCadence :: String
  , wBpm     :: !Int                          -- ★ テンポ（1 拍 = 8 分音符）
  , wRng     :: Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "66-14 Automaton composer" (world0 1280 800))
  { skBack  = col bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      pure (world0 (envW env) (envH env)) { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: Float -> Float -> World
world0 w h =
  let (m, sn) = defaultMachine (w * 0.32) (h * 0.48) (min (w * 0.2) (h * 0.26))
  in World m ed0 sn "0110100110010110" (-1) True 0 (dStart (emD m)) [] 0 "" 120 (rngOf 1) []

-- ★ はじめの機械：キック・スネア・ハイハット・ミ・ソ
defaultMachine :: Float -> Float -> Float -> (EdM, [Int])
defaultMachine cx cy rad =
  let m0 = (edNew "01") { emPos = circleLayout 5 cx cy rad pi }
      m1 = m0 { emD = (emD m0) { dN = 5, dTr = replicate 5 M.empty, dAcc = [True, False, False, False, False] } }
      m2 = foldl' (\m (a, c, b) -> edSetEdge a b c m) m1
             [(0, '0', 2), (0, '1', 1), (1, '0', 2), (1, '1', 3), (2, '0', 0), (2, '1', 4), (3, '0', 2), (3, '1', 0), (4, '0', 3), (4, '1', 0)]
  in (m2, [0, 1, 2, 4, 5])

-- ★ 歩く：入力を 1 文字読むごとの状態（-1 = 行き止まり）。音はその状態の音
walk :: DFA -> String -> [Int]
walk m s = drop 1 (dfaTrace m s)

beatSec :: World -> Float
beatSec w = 60 / i2f (wBpm w) / 2

vol :: Double -> Float
vol v = realToFrac (v * master)

-- 音を鳴らす（キック・スネア・ハイハットは短い音で代用）
playSound :: World -> Int -> IO ()
playSound w k = case fmap snd (lookupIdx k) of
  Just Kick -> playTone 60 (vol 0.9) 0.25 Sine
  Just Snare -> playTone 190 (vol 0.5) 0.15 Sawtooth
  Just Hat -> playTone 6000 (vol 0.3) 0.05 Square
  Just (Note n) -> playTone (midiFreq n) (vol 0.5) (beatSec w * 1.6) Triangle
  _ -> pure ()
  where lookupIdx i = if i >= 0 && i < length sounds then Just (at' sounds i ("", Rest)) else Nothing

-- 終わりの和音（ド・ミ・ソ）か、まだ続く感じ（レ・ファ・シ）
playCadence :: Bool -> IO ()
playCadence ok = playChord (map midiFreq (if ok then [60, 64, 67, 72] else [62, 65, 71])) (vol 0.35) 0.9 Sine

onBeat :: World -> IO World
onBeat w0
  | null (wWord w0) = pure w0
  | otherwise = do
      let d = emD (wMach w0)
          b1 = wBeat w0 + 1
          endBar = b1 >= length (wWord w0)                     -- 小節の終わり
          ok = dfaAccepts d (wWord w0)
      when endBar (playCadence ok)
      let b = if endBar then 0 else b1
          q = at' (walk d (wWord w0)) b (-1)
          k = at' (wSnd w0) q 0
      when (q >= 0) (playSound w0 k)
      pure w0 { wBeat = b, wCurQ = q, wFlash = 1
              , wCadence = if endBar then (if ok then "accepted -> final chord" else "not accepted -> 'to be continued' chord") else wCadence w0
              , wRoll = let r = wRoll w0 ++ [if q >= 0 then k else -1] in if length r > 48 then drop 1 r else r }

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let now = envTime env * 1000
  w1 <- if wPlaying w0 && now - wLastBeat w0 >= beatSec w0 * 1000 then onBeat w0 { wLastBeat = now } else pure w0
  pure w1 { wFlash = wFlash w1 * 0.9 }

inArea :: Env -> Float -> Float -> Bool
inArea env x y = x < envW env * 0.62 && y > 100 && y < envH env - 170

draw :: Env -> World -> Picture
draw env w = view env
  [ hudTitle "Automaton composer: state = sound, input = rhythm path"
             ("Space play / stop   0 1 input   Backspace   R random input   Up/Down tempo " ++ show (wBpm w) ++ "   (keep the volume low)\n"
              ++ "Click: state   Drag -> 0/1 key: arrow   Double-click: accept   Shift+drag: move   K change sound   X delete")
  -- ---- 機械
  , drawDfa defOpt { oR = r0 + wFlash w * 6, oNames = Just [ fst (at' sounds (at' (wSnd w) q 0) ("", Rest)) | q <- [0 .. dN d - 1] ]
                   , oHi = hi, oNameSize = Just 14, oLabelSize = 15, oEdgeHi = edgeHi } d (emPos (wMach w))
  , edDrawRubber r0 (wMach w, wEd w)
  -- ---- 入力（リズムの道順）
  , say ("Input (1 bar = " ++ show (length word) ++ " beats)   " ++ wCadence w) 30 (y - 12) 15 (col ink)
  , Pictures [ Pictures [ Color (col (if i == wBeat w then (80, 60, 20) else panel)) (rectTL (30 + i2f i * cw) y (cw - 4) 36)
                        , Color (col (if i == wBeat w then amber else wire)) (frameTL (if i == wBeat w then 3 else 1) (30 + i2f i * cw) y (cw - 4) 36)
                        , sayM [c] (30 + i2f i * cw + cw / 2 - 2) (y + 18) 18 (col ink) ]
             | (i, c) <- zip [0 ..] word ]
  -- ---- ピアノロール（右）：行 = 音、横 = 時間
  , say "Sounds played (right = now)" px (py - 8) 15 (col ink)
  , Pictures [ Pictures [ sayRM s (px + 56) (py + 14 + i2f k * rh) 14 (col dimc), Color (col wire) (lineW 1 (px + 62, py + 14 + i2f k * rh) (envW env - 40, py + 14 + i2f k * rh)) ]
             | (k, (s, _)) <- zip [0 :: Int ..] sounds ]
  , Pictures [ Color (col (if k < 3 then cyan else amber)) (rectTL (px + 64 + i2f i * rw) (py + 4 + i2f k * rh) (max 2 (rw - 2)) (rh - 8))
             | (i, k) <- zip [0 :: Int ..] (wRoll w), k >= 0 ]
  , Pictures [ say t px (ry + i2f i * 22) 14 (col dimc)
             | (i, t) <- zip [0 :: Int ..] [ "Same input, change one arrow:", "the music changes.", "The arrows decide which sound", "follows which (Kick -1-> Snare ...).", "Ending in a double circle", "plays the final chord." ] ]
  , drawSelfTest env (wTest w) ]
  where
    d = emD (wMach w)
    word = wWord w
    q = wCurQ w
    sel = edSel (wEd w)
    hi0 = if q >= 0 && q < dN d then IM.singleton q amber else IM.empty
    hi = if sel >= 0 && sel < dN d && not (IM.member sel hi0) then IM.insert sel violet hi0 else hi0
    prevQ = if wBeat w > 0 then at' (walk d word) (wBeat w - 1) (-1) else dStart d
    edgeHi e = if wBeat w >= 0 && eFrom e == prevQ && eTo e == q && [at' word (wBeat w) ' '] `elem` eSyms e then Just amber else Nothing
    y = envH env - 150
    cw = min 40 (envW env * 0.6 / i2f (max 1 (length word)))
    px = envW env * 0.66
    py = 110
    rh = 26
    rw = (envW env - px - 40) / 48
    ry = py + i2f (length sounds) * rh + 30

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down mods p ->
    let (x, y) = toP5 env p
        sh = shiftDown mods || isDown env (SpecialKey KeyShiftL) || isDown env (SpecialKey KeyShiftR)
    in pure $ if inArea env x y then let (_, (m, e)) = edPress x y r0 (envTime env * 1000) sh (wMach w, wEd w) in w { wMach = m, wEd = e } else w
  EventMotion p | envDown env ->
    let (x, y) = toP5 env p
        (m, e) = edDragTo x y (wMach w, wEd w)
    in pure w { wMach = m, wEd = e }
  EventKey (MouseButton LeftButton) Up _ p ->
    let (x, y) = toP5 env p
        (act, (m, e)) = edRelease x y r0 (inArea env x y) (wMach w, wEd w)
        n = dN (emD m)
        sn = if act == ActAdd then take (n - 1) (wSnd w ++ repeat 0) ++ [3 + n `mod` 5] else wSnd w
    in pure w { wMach = m, wEd = e, wSnd = sn }
  EventKey k Down _ _ -> pure $ case (typedKey k, k) of
    (TChar c, _) | (c == '0' || c == '1') && edPending (wEd w) /= Nothing ->
      let (_, (m, e)) = edLabel c (wMach w, wEd w) in w { wMach = m, wEd = e }     -- 名前待ちの矢印に記号をつける
    (TChar c, _) | c == '0' || c == '1' -> w { wWord = typeInto (wWord w) c "01" 16 }
    (TBack, _) -> let wd = dropLast (wWord w) in w { wWord = wd, wBeat = min (wBeat w) (length wd - 1) }
    (_, SpecialKey KeySpace) -> w { wPlaying = not (wPlaying w) }
    (_, SpecialKey KeyUp) -> w { wBpm = min 220 (wBpm w + 10) }
    (_, SpecialKey KeyDown) -> w { wBpm = max 60 (wBpm w - 10) }
    (TChar c, _)
      | isKey 'r' c -> let (wd, g) = runR (mapM (const (fmap (\u -> if u < 0.5 then '0' else '1') rnd)) [1 .. 16 :: Int]) (wRng w)
                       in w { wWord = wd, wRng = g, wBeat = -1 }
      | isKey 'k' c && sel >= 0 && sel < n -> w { wSnd = take n (modifyAt sel (\v -> (v + 1) `mod` length sounds) (wSnd w ++ replicate (n - length (wSnd w)) 0)) }
      | isKey 'x' c && sel >= 0 && sel < n && n > 1 ->
          let m = edDelete sel (wMach w) in w { wSnd = removeAt sel (wSnd w), wMach = m, wEd = (wEd w) { edSel = -1 }, wCurQ = dStart (emD m) }
    _ -> w
  _ -> pure w
  where
    sel = edSel (wEd w)
    n = dN (emD (wMach w))
    shiftDown mods = shift mods == Down

-- ---------- やってみよう ----------
-- 1. 入力を 0000000000000000 にすると、キック → ハイハット → キック …の 2 拍のループ。1111 … だと？
-- 2. 矢印を 1 本だけ（ドラッグで）変えて、同じ入力の曲がどう変わるか聞きくらべよう
-- 3. K で状態の音をメロディ（ド・ミ・ソ・ラ）に変えて、0 と 1 で上がったり下がったりする機械を作ろう
-- 4. 「最後がキックで終わるリズム」だけが受理になる。受理状態を変えて、終わりの和音が鳴る入力をさがそう
-- 5. 02 の NFA のように「同時に 2 つの状態」にいられたら？ 和音が鳴る機械になる（ムーア型の出力 = 00）

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("はじめの機械で 0110 を歩く：ハイハット → ソ → キック → ハイハット（状態 2 → 4 → 0 → 2）", walk m "0110" == [2, 4, 0, 2])
  , ("入力が 16 拍なら音も 16 個、状態はいつも 0〜4（行き止まりなし）", length (walk m "0110100110010110") == 16 && all (all (\q -> q >= 0 && q < 5) . walk m) (allStrings "01" 8))
  , ("終わりの和音が鳴る ⇔ 入力を読み終えて受理状態（キック）にいる（長さ 10 まで全部）", all (\s -> null s || (lastOr (-1) (walk m s) == 0) == dfaAccepts m s) (allStrings "01" 10))
  , ("矢印を消すと行き止まり（-1）になり、そのあとは休み", walk m2 "0011" == [2, 0, -1, -1])
  , ("全体の音量 MASTER = " ++ showD master ++ " ≤ 0.2、テンポ 120 で 1 拍 = 0.25 秒", master <= 0.2 && abs (60 / 120 / 2 - 0.25) < (1e-12 :: Double)) ]
  where
    m = emD (fst (defaultMachine 0 0 100))
    m2 = m { dTr = modifyAt 0 (M.delete '1') (dTr m) }

-- ==================================================
-- ここから下は「オートマトンの道具」（中身は読まなくても OK。消さないでね）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具
type RGB = (Float, Float, Float)

bgCol, ink, amber, cyan, lime, rose, violet, dimc, panel, wire :: RGB
bgCol = (9, 20, 15)                           -- 背景（回路基板の黒緑）
ink = (226, 240, 228)                         -- 文字（蛍光の白）
amber = (255, 170, 50)
cyan = (70, 215, 230)
lime = (140, 230, 120)
rose = (255, 100, 120)
violet = (185, 150, 255)
dimc = (125, 155, 138)
panel = (18, 36, 28)
wire = (44, 74, 58)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

scaleRGB :: Float -> RGB -> RGB
scaleRGB k (r, g, b) = (r * k, g * k, b * k)

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

clampI :: Int -> Int -> Int -> Int
clampI v a b = max a (min b v)

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

f2 :: Double -> Float                         -- 描くときの Float（NaN は 0）
f2 v = if isNaN v || isInfinite v then 0 else realToFrac v

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

-- i 番目を取りのぞく（splice(i, 1)）
removeAt :: Int -> [a] -> [a]
removeAt i xs = [x | (k, x) <- zip [0 ..] xs, k /= i]

-- 長さ √(a² + b²)（Math.hypot）
hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- 文字の入力：allowed に入っている 1 文字だけ足す（長さの上限つき。p5.js 版の typeInto）
typeInto :: String -> Char -> String -> Int -> String
typeInto s ch allowed maxLen = if ch `elem` allowed && length s < maxLen then s ++ [ch] else s

dropLast :: [a] -> [a]                        -- s.slice(0, -1)
dropLast xs = if null xs then xs else init xs

-- キーの名前をそろえる（gloss では スペース・Enter・Backspace・Tab が SpecialKey で来る）
data TypedKey = TChar Char | TBack | TEnter | TTab | TOther

typedKey :: Key -> TypedKey
typedKey k = case k of
  Char '\b' -> TBack
  Char '\DEL' -> TBack
  Char '\t' -> TTab
  Char '\r' -> TEnter
  Char '\n' -> TEnter
  Char c -> TChar c
  SpecialKey KeySpace -> TChar ' '
  SpecialKey KeyBackspace -> TBack
  SpecialKey KeyDelete -> TBack               -- Mac の delete キー
  SpecialKey KeyEnter -> TEnter
  SpecialKey KeyPadEnter -> TEnter
  SpecialKey KeyTab -> TTab
  _ -> TOther

-- 文字（大文字・小文字のどちらでも）か
isKey :: Char -> Char -> Bool
isKey k c = c == k || c == toUpperC k

toUpperC :: Char -> Char
toUpperC c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

nubOrd :: Ord a => [a] -> [a]                 -- 重なりを除く（最初の順を保つ）
nubOrd = go S.empty
  where go _ [] = []
        go seen (x : xs) = if S.member x seen then go seen xs else x : go (S.insert x seen) xs

-- 厳密にリストを評価する（遅延評価の「つけ」をためない）
forceL :: [a] -> [a]
forceL xs = foldl' (flip seq) () xs `seq` xs

-- ---- 数の書き方
-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）
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

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Integer
jsRound v = let r = floor v in if v - fromIntegral r >= 0.5 then r + 1 else r

showD :: Double -> String                     -- JavaScript の String(v) に近い書き方（0.12、2）
showD v = if v == fromIntegral (jsRound v) && abs v < 1e15 then show (jsRound v) else showFFloat Nothing v ""

-- n を 2 進数の文字列に（n.toString(2)）
toBin :: Int -> String
toBin n = if n <= 0 then "0" else reverse (go n)
  where go 0 = ""
        go k = (if odd k then '1' else '0') : go (k `div` 2)

-- 2 進数の文字列を数に（parseInt(s, 2)。0 と 1 でない文字で止まる）
fromBin :: String -> Int
fromBin = foldl' (\a c -> 2 * a + (if c == '1' then 1 else 0)) 0 . takeWhile (`elem` "01")

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

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

rndPick :: String -> R Char                   -- 文字の中から 1 つ
rndPick cs = do u <- rnd; pure (at' cs (floor (u * fromIntegral (length cs))) ' ')

rndString :: String -> Int -> R String        -- alpha の文字で長さ len の文字列
rndString alpha len = mapM (const (rndPick alpha)) [1 .. len]

-- n 回くり返して結果を順に集める（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- ---- 有限オートマトンの道具
-- DFA = 状態の数・記号・遷移（dTr !! q で、記号 → 次の状態）・開始・受理
--   遷移がない記号 = 「行き止まり」（そこで不受理が決まる）
-- NFA = 遷移が「次の状態のリスト」。記号 epsSym が ε 遷移
type Sym = Char

data DFA = DFA { dN :: !Int, dAlpha :: [Sym], dTr :: [M.Map Sym Int], dStart :: !Int, dAcc :: [Bool] }
data NFA = NFA { nN :: !Int, nAlpha :: [Sym], nTr :: [M.Map Sym [Int]], nStart :: !Int, nAcc :: [Bool] }

epsSym :: Sym                                 -- ε（文字を読まずに進む）
epsSym = '\0'

mkDfa :: [Sym] -> [[(Sym, Int)]] -> Int -> [Bool] -> DFA
mkDfa alpha rows st acc = DFA (length rows) alpha (map M.fromList rows) st acc

mkNfa :: [Sym] -> [[(Sym, [Int])]] -> Int -> [Bool] -> NFA
mkNfa alpha rows st acc = NFA (length rows) alpha (map M.fromList rows) st acc

dfaNext :: DFA -> Int -> Sym -> Int
dfaNext d q c
  | q < 0 = -1
  | otherwise = case M.lookup c (at' (dTr d) q M.empty) of
      Just t | t >= 0 && t < dN d -> t
      _ -> -1

-- たどった状態の列（-1 = 行き止まり）
dfaTrace :: DFA -> String -> [Int]
dfaTrace d = scanl (dfaNext d) (dStart d)

dfaAccepts :: DFA -> String -> Bool
dfaAccepts d s = go (dStart d) s
  where go q [] = q >= 0 && at' (dAcc d) q False
        go q (c : cs) = let t = dfaNext d q c in t >= 0 && go t cs

-- 行き止まりを「ごみ箱」状態にまとめて、全部の遷移をそろえる（返り値の 2 つ目 = ごみ箱の番号、なければ −1）
completeDfaS :: DFA -> (DFA, Int)
completeDfaS d =
  let missing = or [ dfaNext d q c < 0 | q <- [0 .. dN d - 1], c <- dAlpha d ]
      sink = dN d
      fillRow q = foldl' (\m c -> if dfaNext d q c < 0 then M.insert c sink m else m) (at' (dTr d) q M.empty) (dAlpha d)
  in if not missing then (d, -1) else
       ( d { dN = dN d + 1
           , dTr = [ fillRow q | q <- [0 .. dN d - 1] ] ++ [M.fromList [ (c, sink) | c <- dAlpha d ]]
           , dAcc = take (dN d) (dAcc d ++ repeat False) ++ [False] }
       , sink )

completeDfa :: DFA -> DFA
completeDfa = fst . completeDfaS

complementDfa :: DFA -> DFA                   -- 受理と不受理を入れかえる
complementDfa d = let e = completeDfa d in e { dAcc = map not (dAcc e) }

data Op = OpAnd | OpOr | OpDiff | OpXor deriving (Eq, Show)

opFun :: Op -> Bool -> Bool -> Bool
opFun o x y = case o of
  OpAnd -> x && y
  OpOr -> x || y
  OpDiff -> x && not y
  OpXor -> x /= y

data Product = Product { prD :: DFA, prPairs :: [(Int, Int)], prLeft :: DFA, prRight :: DFA }

-- 積の構成：状態 = (A の状態, B の状態) の組。たどれる組だけ、見つけた順に番号をつける
productDfa :: DFA -> DFA -> Op -> Product
productDfa a0 b0 o =
  let a = completeDfa a0
      b = completeDfa b0
      alpha = filter (`elem` dAlpha b) (dAlpha a)
      start = (dStart a, dStart b)
      loop h ids prs n rows
        | h >= n = (prs, reverse rows)
        | otherwise =
            let (p, q) = IM.findWithDefault (0, 0) h prs
                addSym (ids1, prs1, n1, row) c =
                  let t = (dfaNext a p c, dfaNext b q c)
                  in case M.lookup t ids1 of
                       Just k -> (ids1, prs1, n1, M.insert c k row)
                       Nothing -> (M.insert t n1 ids1, IM.insert n1 t prs1, n1 + 1, M.insert c n1 row)
                (ids', prs', n', row') = foldl' addSym (ids, prs, n, M.empty) alpha
            in loop (h + 1) ids' prs' n' (row' : rows)
      (pairsM, trs) = loop 0 (M.singleton start 0) (IM.singleton 0 start) 1 []
      pairs = IM.elems pairsM
      accOf (p, q) = opFun o (at' (dAcc a) p False) (at' (dAcc b) q False)
  in Product (DFA (length pairs) alpha trs 0 (map accOf pairs)) pairs a b

-- 幅優先探索：受理状態までの一番短い文字列（Nothing = 空言語）
dfaShortestAccepted :: DFA -> Maybe String
dfaShortestAccepted d
  | dStart d < 0 || dStart d >= dN d = Nothing
  | otherwise = go [dStart d] [] (IM.singleton (dStart d) "")
  where
    go [] [] _ = Nothing
    go [] next seen = go (reverse next) [] seen
    go (q : qs) next seen
      | at' (dAcc d) q False = Just (reverse (IM.findWithDefault "" q seen))
      | otherwise =
          let path = IM.findWithDefault "" q seen
              (next', seen') = foldl' (\(nx, sn) c -> let t = dfaNext d q c in
                                          if t >= 0 && not (IM.member t sn) then (t : nx, IM.insert t (c : path) sn) else (nx, sn))
                                      (next, seen) (dAlpha d)
          in go qs next' seen'

-- Nothing なら同じ言語、そうでなければ反例
dfaEquivalent :: DFA -> DFA -> Maybe String
dfaEquivalent a b = dfaShortestAccepted (prD (productDfa a b OpXor))

-- スタートからたどれない状態を消す（2 つ目 = 新しい番号 → 元の番号）
reachableDfa :: DFA -> (DFA, [Int])
reachableDfa d =
  let go h order idx
        | h >= length order = (order, idx)
        | otherwise =
            let q = at' order h 0
                (order', idx') = foldl' (\(o, ix) c -> let t = dfaNext d q c in
                                            if t >= 0 && not (IM.member t ix) then (o ++ [t], IM.insert t (length o) ix) else (o, ix))
                                        (order, idx) (dAlpha d)
            in go (h + 1) order' idx'
      (ord', idxM) = go 0 [dStart d] (IM.singleton (dStart d) 0)
      row q = M.fromList [ (c, IM.findWithDefault 0 t idxM) | c <- dAlpha d, let t = dfaNext d q c, t >= 0 ]
  in (DFA (length ord') (dAlpha d) (map row ord') 0 (map (\q -> at' (dAcc d) q False) ord'), ord')

data Mini = Mini { mWork :: DFA, mOld :: [Int], mRounds :: [[Int]], mBlockOf :: [Int], mMin :: DFA }

-- 見つけた順に 0, 1, 2, … と番号をつけなおす
renum :: Ord k => [k] -> [Int]
renum keys = reverse (snd (foldl' (\(m, out) k -> case M.lookup k m of
                                     Just v -> (m, v : out)
                                     Nothing -> let v = M.size m in (M.insert k v m, v : out)) (M.empty, []) keys))

-- ムーアの分割の精密化：「区別できない状態」を同じブロックに
minimizeMoore :: DFA -> Mini
minimizeMoore d =
  let (e, old) = reachableDfa (completeDfa d)
      blk0 = renum (map (\v -> if v then 1 else 0 :: Int) (dAcc e))
      nDistinct = S.size . S.fromList
      refine blk rounds =
        let keys = [ (b, [ at' blk (dfaNext e q c) 0 | c <- dAlpha e ]) | (q, b) <- zip [0 ..] blk ]
            nb = renum keys
        in if nDistinct nb == nDistinct blk then (blk, reverse rounds) else refine nb (nb : rounds)
      (blk, rounds) = refine blk0 [blk0]
      k = nDistinct blk
      accs = IM.fromList [ (b, at' (dAcc e) q False) | (q, b) <- zip [0 ..] blk ]
      trs = IM.fromListWith M.union [ (b, M.fromList [ (c, at' blk (dfaNext e q c) 0) | c <- dAlpha e ]) | (q, b) <- zip [0 ..] blk ]
      mn = DFA k (dAlpha e) [ IM.findWithDefault M.empty b trs | b <- [0 .. k - 1] ] (at' blk (dStart e) 0) [ IM.findWithDefault False b accs | b <- [0 .. k - 1] ]
  in Mini e old rounds blk mn

-- 長さ 0〜maxLen の文字列を全部（短い順）
allStrings :: [Sym] -> Int -> [String]
allStrings alpha maxLen = concat (take (maxLen + 1) (iterate (\layer -> [ s ++ [c] | s <- layer, c <- alpha ]) [""]))

nfaRow :: NFA -> Int -> Sym -> [Int]
nfaRow nfa q c = M.findWithDefault [] c (at' (nTr nfa) q M.empty)

-- ε 遷移だけでたどれる状態を全部足す（小さい順）
epsClosure :: NFA -> [Int] -> [Int]
epsClosure nfa states = S.toAscList (go (S.fromList states) states)
  where go seen [] = seen
        go seen (q : todo) = let new = [ t | t <- nfaRow nfa q epsSym, not (S.member t seen) ]
                             in go (foldl' (flip S.insert) seen new) (new ++ todo)

nfaMove :: NFA -> [Int] -> Sym -> [Int]
nfaMove nfa set c = epsClosure nfa (S.toList (S.fromList (concatMap (\q -> nfaRow nfa q c) set)))

nfaTrace :: NFA -> String -> [[Int]]
nfaTrace nfa = scanl (nfaMove nfa) (epsClosure nfa [nStart nfa])

nfaAccepts :: NFA -> String -> Bool
nfaAccepts nfa s = go (epsClosure nfa [nStart nfa]) s
  where go cur [] = any (\q -> at' (nAcc nfa) q False) cur
        go cur (c : cs) = let nx = nfaMove nfa cur c in not (null nx) && go nx cs

data Subset = Subset { sSets :: [[Int]], sRows :: [(Int, M.Map Sym Int)], sDfa :: DFA }

-- 部分集合構成：DFA の 1 状態 = NFA の状態の集合（たどれる集合だけ、見つけた順に番号）
subsetConstruct :: NFA -> Int -> Subset
subsetConstruct nfa maxStates =
  let first = epsClosure nfa [nStart nfa]
      loop h sets ids n rows
        | h >= n || n > maxStates = (sets, reverse rows)
        | otherwise =
            let cur = IM.findWithDefault [] h sets
                addSym (sets1, ids1, n1, row) c =
                  let t = nfaMove nfa cur c
                  in case M.lookup t ids1 of
                       Just k -> (sets1, ids1, n1, M.insert c k row)
                       Nothing -> (IM.insert n1 t sets1, M.insert t n1 ids1, n1 + 1, M.insert c n1 row)
                (sets', ids', n', row') = foldl' addSym (sets, ids, n, M.empty) (nAlpha nfa)
            in loop (h + 1) sets' ids' n' ((h, row') : rows)
      (setsM, rows) = loop 0 (IM.singleton 0 first) (M.singleton first 0) 1 []
      sets = IM.elems setsM
      dfa = DFA (length sets) (nAlpha nfa) (map snd rows) 0 [ any (\q -> at' (nAcc nfa) q False) st | st <- sets ]
  in Subset sets rows dfa

setName :: [Int] -> String
setName st = "{" ++ joinWith "," (map show st) ++ "}"

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  _ -> foldr1 (\a b -> a ++ sep ++ b) xs

-- ---- 状態遷移図を描く道具（円 = 状態、二重丸 = 受理、矢印 = 遷移。曲線は折れ線で描く）
data Pos = Pos { pX :: !Float, pY :: !Float, pLoop :: Maybe Float }   -- 位置と自己ループの向き

-- 辺 = 出る状態・入る状態・ラベル・記号のリスト
data Edge = Edge { eFrom :: !Int, eTo :: !Int, eLabel :: String, eSyms :: [String] }

epsLabel :: String                            -- ε の矢印のラベル（画面は ASCII だけなので "eps"）
epsLabel = "eps"

-- 2 次ベジェ曲線 → 折れ線の点
bezPts :: Float -> Float -> Float -> Float -> Float -> Float -> Int -> [(Float, Float)]
bezPts x0 y0 cx cy x1 y1 n =
  [ (u * u * x0 + 2 * u * t * cx + t * t * x1, u * u * y0 + 2 * u * t * cy + t * t * y1) | i <- [0 .. n], let t = i2f i / i2f n, let u = 1 - t ]

-- 3 次ベジェ曲線 → 折れ線の点（自己ループ用）
cubicPts :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Int -> [(Float, Float)]
cubicPts (p0x, p0y) (p1x, p1y) (p2x, p2y) (p3x, p3y) n =
  [ (a * p0x + b * p1x + c * p2x + d * p3x, a * p0y + b * p1y + c * p2y + d * p3y)
  | i <- [0 .. n], let t = i2f i / i2f n, let u = 1 - t, let a = u * u * u, let b = 3 * u * u * t, let c = 3 * u * t * t, let d = t * t * t ]

-- 折れ線 + 先の三角（dashed = 点線：1 つおきの区間だけ描く）
arrowLine :: [(Float, Float)] -> RGB -> Float -> Bool -> Picture
arrowLine pts c w dashed = case reverse pts of
  ((x1, y1) : (x0, y0) : _) ->
    let l0 = hyp (x1 - x0) (y1 - y0)
        l = if l0 == 0 then 1 else l0
        ux = (x1 - x0) / l
        uy = (y1 - y0) / l
        s = 7 + w * 1.5
        body = if dashed then Pictures [ lineW w p q | (k, p, q) <- zip3 [0 :: Int ..] pts (drop 1 pts), even k ] else polyline w pts
        tip = Polygon [(x1, y1), (x1 - ux * s - uy * s * 0.5, y1 - uy * s + ux * s * 0.5), (x1 - ux * s + uy * s * 0.5, y1 - uy * s - ux * s * 0.5)]
    in Color (col c) (Pictures [body, tip])
  _ -> Blank

-- 状態ごとの「ラベル → 行き先のリスト」から辺のリスト（同じ向きの辺はラベルをまとめる。p5.js 版の edgeList）
edgesFrom :: [[(String, [Int])]] -> [Edge]
edgesFrom rows =
  let n = length rows
      items = [ ((q, t), lab) | (q, row) <- zip [0 ..] rows, (lab, ts) <- row, t <- ts, t >= 0, t < n ]
      keys = nubOrd (map fst items)
      symsOf k = [ lab | (k', lab) <- items, k' == k ]
      sortSyms = sortOn (\s -> (s == epsLabel, s))
  in [ Edge q t (joinWith "," (sortSyms (symsOf (q, t)))) (symsOf (q, t)) | (q, t) <- keys ]

symLabel :: Sym -> String
symLabel c = if c == epsSym then epsLabel else [c]

dfaEdges :: DFA -> [Edge]
dfaEdges d = edgesFrom [ [ (symLabel c, [t]) | (c, t) <- M.toList row ] | row <- take (dN d) (dTr d ++ repeat M.empty) ]

nfaEdges :: NFA -> [Edge]
nfaEdges m = edgesFrom [ [ (symLabel c, ts) | (c, ts) <- M.toList row ] | row <- take (nN m) (nTr m ++ repeat M.empty) ]

-- 1 本の辺の折れ線の点とラベルの位置
edgeGeometry :: Edge -> Pos -> Pos -> Float -> Bool -> ([(Float, Float)], (Float, Float))
edgeGeometry e a b r both
  | eFrom e == eTo e =                         -- 自己ループ：中心から外向き（または上）に輪
      let ang = maybe (-pi / 2) id (pLoop a)
          ux = cos ang
          uy = sin ang
          px = -uy
          py = ux
          s0 = (pX a + (ux * 0.6 + px * 0.8) * r, pY a + (uy * 0.6 + py * 0.8) * r)
          s1 = (pX a + (ux * 0.6 - px * 0.8) * r, pY a + (uy * 0.6 - py * 0.8) * r)
          pts = cubicPts s0 (fst s0 + (ux * 2.2 + px * 0.9) * r, snd s0 + (uy * 2.2 + py * 0.9) * r)
                            (fst s1 + (ux * 2.2 - px * 0.9) * r, snd s1 + (uy * 2.2 - py * 0.9) * r) s1 18
      in (pts, (pX a + ux * 2.35 * r, pY a + uy * 2.35 * r))
  | otherwise =
      let dx = pX b - pX a
          dy = pY b - pY a
          l0 = hyp dx dy
          l = if l0 == 0 then 1 else l0
          ux = dx / l
          uy = dy / l
          bend = if both then 0.22 * l else 0
          mx = (pX a + pX b) / 2 - uy * bend
          my = (pY a + pY b) / 2 + ux * bend
          d0x = mx - pX a
          d0y = my - pY a
          l0' = nz (hyp d0x d0y)
          d1x = mx - pX b
          d1y = my - pY b
          l1 = nz (hyp d1x d1y)
          pts = bezPts (pX a + (d0x / l0') * r) (pY a + (d0y / l0') * r) mx my (pX b + (d1x / l1) * (r + 2)) (pY b + (d1y / l1) * (r + 2)) 16
          (midX, midY) = at' pts 8 (mx, my)
      in (pts, (midX - uy * 13, midY + ux * 13))
  where nz v = if v == 0 then 1 else v

-- 図の描き方の選択（p5.js 版の opt）
data MOpt = MOpt
  { oR :: Float                               -- 円の半径
  , oNames :: Maybe [String]                  -- 状態の名前（Nothing なら q0, q1, …）
  , oHi :: IM.IntMap RGB                      -- 色をつける状態
  , oEdgeHi :: Edge -> Maybe RGB              -- 色をつける辺
  , oStart :: Bool                            -- 開始の矢印を描くか
  , oLabels :: Bool                           -- 辺のラベルを描くか
  , oLabelSize :: Float
  , oNameSize :: Maybe Float
  , oEdgeCol :: RGB
  , oLabelCol :: RGB
  , oStartCol :: RGB
  , oStartAng :: Maybe Float }

defOpt :: MOpt
defOpt = MOpt 22 Nothing IM.empty (const Nothing) True True 14 Nothing (120, 160, 140) (200, 225, 210) ink Nothing

-- 状態の数 n・開始・受理・辺・位置から図を描く
drawMachine :: MOpt -> Int -> Int -> [Bool] -> [Edge] -> [Pos] -> Picture
drawMachine opt n start acc edges pos = Pictures (edgePics ++ [startPic] ++ statePics)
  where
    r = oR opt
    has = S.fromList [ (eFrom e, eTo e) | e <- edges ]
    posOf q = if q >= 0 then at' (map Just pos) q Nothing else Nothing
    edgePics =
      [ Pictures [ arrowLine pts (maybe (oEdgeCol opt) id hc) (if hc == Nothing then 1.8 else 3.5) (eLabel e == epsLabel && hc == Nothing)
                 , if oLabels opt || hc /= Nothing then sayM (eLabel e) lx ly (max 14 (oLabelSize opt)) (col (maybe (oLabelCol opt) id hc)) else Blank ]
      | e <- edges, Just a <- [posOf (eFrom e)], Just b <- [posOf (eTo e)]
      , let (pts, (lx, ly)) = edgeGeometry e a b r (eFrom e /= eTo e && S.member (eTo e, eFrom e) has)
      , let hc = oEdgeHi opt e ]
    startPic = case posOf start of
      Just p | oStart opt && start < n ->
        let sa = case oStartAng opt of
              Just v -> v
              Nothing -> case pLoop p of
                Just lp | cos lp < -0.5 -> -2.2
                _ -> pi
            ux = cos sa
            uy = sin sa
        in arrowLine [(pX p + ux * (r + 34), pY p + uy * (r + 34)), (pX p + ux * (r + 2), pY p + uy * (r + 2))] (oStartCol opt) 2 False
      _ -> Blank
    statePics =
      [ Pictures [ Color (col (maybe panel (scaleRGB 0.35) hc)) (circleAt (pX p) (pY p) (2 * r))
                 , Color (col (maybe (150, 190, 170) id hc)) (ringAt (pX p) (pY p) (2 * r) sw)
                 , if at' acc q False then Color (col (maybe (150, 190, 170) id hc)) (ringAt (pX p) (pY p) (2 * r - 9) sw) else Blank
                 , sayM nm (pX p) (pY p + 1) (max 14 (maybe (min 16 (r * 0.75)) id (oNameSize opt))) (col (maybe ink (const (255, 255, 255)) hc)) ]
      | q <- [0 .. n - 1], Just p <- [posOf q]
      , let hc = IM.lookup q (oHi opt)
      , let sw = if hc == Nothing then 2 else 3.5
      , let nm = maybe ("q" ++ show q) (\ns -> at' ns q "") (oNames opt) ]

drawDfa :: MOpt -> DFA -> [Pos] -> Picture
drawDfa opt d = drawMachine opt (dN d) (dStart d) (dAcc d) (dfaEdges d)

drawNfa :: MOpt -> NFA -> [Pos] -> Picture
drawNfa opt m = drawMachine opt (nN m) (nStart m) (nAcc m) (nfaEdges m)

-- 円の上に並べる（自己ループは外向き）
circleLayout :: Int -> Float -> Float -> Float -> Float -> [Pos]
circleLayout n cx cy rad a0 =
  [ Pos (cx + rad * cos a) (cy + rad * sin a) (Just a) | i <- [0 .. n - 1], let a = a0 + 2 * pi * i2f i / i2f (max 1 n) ]

-- スタートからの距離（幅優先）で左 → 右に並べる
layerLayout :: Int -> Int -> [Edge] -> Float -> Float -> Float -> Float -> [Pos]
layerLayout n start edges x y w h
  | n <= 0 = []
  | otherwise =
      let st = clampI start 0 (n - 1)
          bfs i order depth
            | i >= length order = (order, depth)
            | otherwise =
                let q = at' order i 0
                    dq = IM.findWithDefault 0 q depth
                    (order', depth') = foldl' (\(o, dp) e -> if eFrom e == q && not (IM.member (eTo e) dp) && eTo e >= 0 && eTo e < n
                                                               then (o ++ [eTo e], IM.insert (eTo e) (dq + 1) dp) else (o, dp)) (order, depth) edges
                in bfs (i + 1) order' depth'
          (order0, depth0) = bfs 0 [st] (IM.singleton st 0)
          maxD0 = maxOr 0 (IM.elems depth0)
          rest = [ q | q <- [0 .. n - 1], not (IM.member q depth0) ]
          depth = foldl' (\dp q -> IM.insert q (maxD0 + 1) dp) depth0 rest
          order = order0 ++ rest
          maxD = maxOr 0 (IM.elems depth)
          cols = IM.fromListWith (flip (++)) [ (IM.findWithDefault 0 q depth, [q]) | q <- order ]
          posM = IM.fromList [ (q, Pos (x + (if maxD > 0 then i2f d / i2f maxD * w else w / 2)) (y + i2f (k + 1) / i2f (length cs + 1) * h) (Just (-pi / 2)))
                             | (d, cs) <- IM.toList cols, (k, q) <- zip [0 :: Int ..] cs ]
      in [ IM.findWithDefault (Pos x y Nothing) q posM | q <- [0 .. n - 1] ]

-- (x, y) の上にある状態（後ろから調べる。なければ −1）
hitState :: [Pos] -> Float -> Float -> Float -> Int
hitState pos x y r = headOr (-1) [ q | (q, p) <- reverse (zip [0 ..] pos), hyp (pX p - x) (pY p - y) < r ]

-- ---- 図を編集する道具：空いた所をクリック = 状態を足す、状態から状態へドラッグ = 遷移（すぐ記号のキーで名前）、
--      ダブルクリック = 受理 ⇔ 不受理、状態をドラッグ（Shift を押しながら）= 動かす、X = 選んだ状態を消す
data EdM = EdM { emD :: DFA, emPos :: [Pos] }            -- 編集できる DFA（位置つき）
data Drag = Drag { dgFrom :: !Int, dgX :: !Float, dgY :: !Float, dgMove :: !Bool, dgFar :: !Float }
data Ed = Ed { edDrag :: Maybe Drag, edPending :: Maybe (Int, Int), edLastT :: !Float, edLastQ :: !Int, edSel :: !Int }
data EdAct = ActAcc | ActDrag | ActMove | ActAdd | ActSelect | ActEdge | ActNone deriving (Eq, Show)

ed0 :: Ed
ed0 = Ed Nothing Nothing (-1e9) (-1) (-1)

edNew :: [Sym] -> EdM
edNew alpha = EdM (DFA 0 alpha [] 0 []) []

edAddState :: Float -> Float -> EdM -> (Int, EdM)
edAddState x y (EdM d ps) =
  (dN d, EdM d { dN = dN d + 1, dTr = dTr d ++ [M.empty], dAcc = dAcc d ++ [False] } (ps ++ [Pos x y (Just (-pi / 2))]))

-- DFA なので同じ記号の古い遷移は上書き
edSetEdge :: Int -> Int -> Sym -> EdM -> EdM
edSetEdge from to c m@(EdM d ps)
  | from >= 0 && from < dN d && to >= 0 && to < dN d && c `elem` dAlpha d = EdM d { dTr = modifyAt from (M.insert c to) (dTr d) } ps
  | otherwise = m

edSetAcc :: Int -> Bool -> EdM -> EdM
edSetAcc q v (EdM d ps) = EdM d { dAcc = setAt q v (dAcc d) } ps

edDelete :: Int -> EdM -> EdM
edDelete q m@(EdM d ps)
  | q < 0 || q >= dN d || dN d <= 1 = m
  | otherwise =
      let fixRow = M.mapMaybe (\t -> if t == q then Nothing else Just (if t > q then t - 1 else t))
          st = dStart d
      in EdM d { dN = dN d - 1, dTr = map fixRow (removeAt q (dTr d)), dAcc = removeAt q (dAcc d)
               , dStart = if st == q then 0 else if st > q then st - 1 else st } (removeAt q ps)

-- 押した（now = ミリ秒、shift = Shift を押しているか）
edPress :: Float -> Float -> Float -> Float -> Bool -> (EdM, Ed) -> (EdAct, (EdM, Ed))
edPress x y r now shift (m, e) =
  let q = hitState (emPos m) x y r
  in if q >= 0 && edLastQ e == q && now - edLastT e < 350
       then (ActAcc, (edSetAcc q (not (at' (dAcc (emD m)) q False)) m, e { edLastT = -1e9, edLastQ = -1, edDrag = Nothing }))
       else let e1 = e { edLastT = now, edLastQ = q }
            in if q >= 0 then let dg = Drag q x y shift 0 in (if shift then ActMove else ActDrag, (m, e1 { edSel = q, edDrag = Just dg }))
               else (ActNone, (m, e1 { edDrag = Nothing }))

edDragTo :: Float -> Float -> (EdM, Ed) -> (EdM, Ed)
edDragTo x y (m, e) = case edDrag e of
  Nothing -> (m, e)
  Just dg ->
    let p = at' (emPos m) (dgFrom dg) (Pos x y Nothing)
        dg' = dg { dgX = x, dgY = y, dgFar = max (dgFar dg) (hyp (x - pX p) (y - pY p)) }   -- どこまで離れたか
        m' = if dgMove dg then m { emPos = setAt (dgFrom dg) p { pX = x, pY = y } (emPos m) } else m
    in (m', e { edDrag = Just dg' })

edRelease :: Float -> Float -> Float -> Bool -> (EdM, Ed) -> (EdAct, (EdM, Ed))
edRelease x y r inArea (m, e) = case edDrag e of
  Nothing -> if inArea && hitState (emPos m) x y (r * 2.2) < 0
               then let (q, m') = edAddState x y m in (ActAdd, (m', e { edSel = q }))
               else (ActNone, (m, e))
  Just dg ->
    let e1 = e { edDrag = Nothing }
        to = hitState (emPos m) x y r
    in if dgMove dg then (ActMove, (m, e1))
       else if to < 0 then (ActNone, (m, e1))
       else if to == dgFrom dg && not (dgFar dg > r * 1.3) then (ActSelect, (m, e1))   -- その場でクリックしただけ（外に出て戻ると自己ループ）
       else (ActEdge, (m, e1 { edPending = Just (dgFrom dg, to) }))

-- 名前待ちの矢印に記号 c をつける
edLabel :: Sym -> (EdM, Ed) -> (Bool, (EdM, Ed))
edLabel c (m, e) = case edPending e of
  Just (a, b) | c `elem` dAlpha (emD m) -> (True, (edSetEdge a b c m, e { edPending = Nothing }))
  _ -> (False, (m, e))

-- ドラッグ中の線と、名前待ちの辺
edDrawRubber :: Float -> (EdM, Ed) -> Picture
edDrawRubber r (m, e) = Pictures [rubber, pending]
  where
    rubber = case edDrag e of
      Just dg | not (dgMove dg) -> let p = at' (emPos m) (dgFrom dg) (Pos 0 0 Nothing) in Color (col amber) (lineW 2 (pX p, pY p) (dgX dg, dgY dg))
      _ -> Blank
    pending = case edPending e of
      Just (fa, fb) | fa < length (emPos m) && fb < length (emPos m) ->
        let a = at' (emPos m) fa (Pos 0 0 Nothing)
            b = at' (emPos m) fb (Pos 0 0 Nothing)
        in Pictures
             [ sayM ("press a symbol key (" ++ unwords (map (: []) (dAlpha (emD m))) ++ ") to label it") ((pX a + pX b) / 2) ((pY a + pY b) / 2 - r - 22) 14 (col amber)
             , Color (col amber) (if fa == fb then dashCircle 2 (pX a) (pY a - r * 1.6) (r * 0.8) else dashLine 2 6 6 (pX a, pY a) (pX b, pY b)) ]
      _ -> Blank

-- 点線の円（中心 (x, y)、半径 rad）
dashCircle :: Float -> Float -> Float -> Float -> Picture
dashCircle w x y rad =
  let k = max 6 (floor (2 * pi * rad / 12)) :: Int
      pt i = (x + rad * cos (2 * pi * i2f i / i2f (2 * k)), y + rad * sin (2 * pi * i2f i / i2f (2 * k)))
  in Pictures [ lineW w (pt (2 * i)) (pt (2 * i + 1)) | i <- [0 .. k - 1] ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

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

-- 左上のタイトルと操作の説明（説明は \n で改行、行の間 19）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

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

-- 右下の小さな表示
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col lime else col rose)
