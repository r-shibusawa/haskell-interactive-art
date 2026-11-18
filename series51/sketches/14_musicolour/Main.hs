-- ============================================
-- 51-14 Musicolour 風（パスク 1953〜） 🔊
-- キーボードで音を鳴らすと、8 つのランプが光で応える。でも同じ音・同じ流れをくり返すと機械は「慣れて（飽きて）」光が弱まる
-- 新しい音やつながりを弾くと、また明るく応える。飽きが続くと、機械は自分の色の割り当てを変える（気分を変える）
-- 操作：A S D F G H J K = ド〜ド / ランプをクリック = その音 / P = 同じフレーズを自動演奏 / Z = でたらめ自動演奏 / R = やり直し
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (when)
import Data.Bits (shiftR, xor, (.|.))
import Data.Char (toLower)
import Data.List (elemIndex, foldl')
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

keysStr :: String
keysStr = "asdfghjk"

freqs :: [Double]
freqs = [261.6, 293.7, 329.6, 349.2, 392.0, 440.0, 493.9, 523.3]

names :: [String]
names = ["Do", "Re", "Mi", "Fa", "So", "La", "Ti", "Do"]

alpha, tauH, forgetT, boredLim :: Double
alpha = 0.25         -- ★ 1 回鳴るごとの慣れの増え方
tauH = 6             -- ★ 慣れが消える時間 [秒]
forgetT = 0.97       -- ★ 音のつながりの記録を、1 音ごとに少しずつ忘れる
boredLim = 0.65      -- ★ 飽きがこの値を 4 秒こえたら気分を変える

phrase :: [Int]
phrase = [0, 2, 4, 2]

-- ---- 機械の状態：慣れ h（チャンネルごと）、ランプの明るさ、つながりの回数 T[前][今]、飽き
data Machine = Machine
  { mH       :: [Double]
  , mLamp    :: [Double]
  , mT       :: [[Double]]
  , mPrev    :: !Int
  , mBored   :: !Double
  , mBoredT  :: !Double
  , mMoods   :: !Int
  , mHue     :: !Double
  , mLast    :: !(Double, Double, Double) }   -- 前の音の (1 − 慣れ, 新しさ, 応答)

data World = World
  { wM      :: !Machine
  , wAuto   :: !Int            -- 0 = 止める、1 = 同じフレーズ、2 = でたらめ
  , wAutoT  :: !Double
  , wAutoI  :: !Int
  , wRng    :: [Double]        -- でたらめ演奏の乱数（種 14）
  , wHist   :: [Double]        -- 1 音ごとの応答（新しい順、120 個まで）
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "51-14 Musicolour" world0)
  { skBack  = col (24, 35, 10) 100
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World makeMachine 0 0 0 (makeRng 14) [] []

makeMachine :: Machine
makeMachine = Machine (replicate 8 0) (replicate 8 0) (replicate 8 (replicate 8 0)) (-1) 0 0 0 0 (0, 1, 0)

-- 時間がたつ：慣れは e^(−t/τ) で消え、ランプは暗くなり、飽きはゆっくり下がる
advance :: Double -> Machine -> Machine
advance dt m
  | b > boredLim = let bt = mBoredT m + dt
                   in if bt > 4
                        then m1 { mMoods = mMoods m + 1, mHue = fromIntegral ((round (mHue m) + 137) `mod` 360 :: Int)
                                , mBoredT = 0, mBored = 0, mT = replicate 8 (replicate 8 0) }
                        else m1 { mBoredT = bt }
  | otherwise = m1 { mBoredT = 0 }
  where
    f = exp (-dt / tauH)
    b = mBored m * exp (-dt / 8)
    m1 = m { mH = map (* f) (mH m), mLamp = map (* exp (-dt / 0.6)) (mLamp m), mBored = b }

-- 音 c が鳴った：応答 = (1 − 慣れ) × (0.4 + 0.6 × 新しさ)。新しさ = 1 ÷ (1 + このつながりを聞いた回数)
hear :: Int -> Machine -> (Double, Machine)
hear c m = (out, m')
  where
    hc = getAt 0 (mH m) c
    r = 1 - hc
    nov = if mPrev m < 0 then 1 else 1 / (1 + getAt 0 (getAt [] (mT m) (mPrev m)) c)
    out = r * (0.4 + 0.6 * nov)
    tForgot = map (map (* forgetT)) (mT m)
    t' = [ [ if i == mPrev m && j == c then v + 1 else v | (j, v) <- zip [0 ..] row ] | (i, row) <- zip [0 ..] tForgot ]
    m' = m { mH = setAt c (clampv 0 1 (hc + alpha * (1 - hc))) (mH m)
           , mT = t', mPrev = c
           , mLamp = setAt c (max (getAt 0 (mLamp m) c) out) (mLamp m)
           , mBored = clampv 0 1 (mBored m + 0.15 * (1 - out - mBored m))
           , mLast = (r, nov, out) }

-- 音を鳴らして機械に聞かせる
playChannel :: Int -> World -> IO World
playChannel c w = do
  let (o, m') = hear c (wM w)
  playNoteK (getAt 440 freqs c) 0.35 (0.15 + 0.15 * o) Triangle
  pure w { wM = m', wHist = take 120 (o : wHist w) }

step :: Env -> Float -> World -> IO World
step _ dt0 w0 = do
  let dt = if dt0 > 0 then min 0.1 (realToFrac dt0) else 1 / 60
      w = w0 { wM = advance dt (wM w0) }
  if wAuto w == 0 then pure w else do
    let t = wAutoT w + dt
    if t <= 0.3 then pure w { wAutoT = t } else
      if wAuto w == 1
        then playChannel (getAt 0 phrase (wAutoI w `mod` length phrase)) w { wAutoT = 0, wAutoI = wAutoI w + 1 }
        else case wRng w of
          (r : rs) -> playChannel (floor (r * 8)) w { wAutoT = 0, wRng = rs }
          [] -> pure w { wAutoT = 0 }

-- ランプの並び
lampGeom :: Env -> (Float, Float, Float)
lampGeom env = ((envW env - 80) / 8, 110, envH env * 0.38)

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Musicolour - repeat yourself and the machine gets bored" 20 36 20 cTxt
  , say ("A S D F G H J K play   Click a lamp   P same phrase " ++ onOff (wAuto w == 1) ++ "   Z random " ++ onOff (wAuto w == 2) ++ "   R reset") 20 62 15 cSub ]
  ++ concatMap lamp [0 .. 7]
  ++ [ say ("Red bar = habituation h of the channel (repeating pushes it to 1, rest fades it as e^(-t/" ++ num tauH ++ "))") 40 (ly + lh + 70) 14 cSub
     , say (if mPrev m < 0 then "Play with A S D F ... (or click a lamp)"
            else "Response to last note = (1 - habit " ++ fmt 2 (1 - lr) ++ ") x (0.4 + 0.6 x novelty " ++ fmt 2 lnov ++ ") = " ++ fmt 3 lout)
           40 my 16 cTube
     , say ("Boredom " ++ fmt 2 (mBored m) ++ (if mBored m > boredLim then " (" ++ fmt 1 (mBoredT m) ++ " s ... mood changes at 4 s)" else "") ++ "   Mood changes " ++ show (mMoods m))
           40 (my + 26) 16 (if mBored m > boredLim then cAlarm else cTxt)
     , plotFrame 40 gy gw gh "Response per note (0 to 1)"
     , plotLine (reverse (wHist w)) 40 gy gw gh 0 1 cTube 120 2
     , say "Transitions heard (row: prev, col: now)" tx (gy + 12) 14 cSub ]
  ++ [ Color (hsb 42 60 (15 + 20 * f2 (min 4 v))) (rectTL (tx + fromIntegral j * cs) (gy + 22 + fromIntegral i * cs) (cs - 2) (cs - 2))
     | (i, row) <- zip [0 :: Int ..] (mT m), (j, v) <- zip [0 :: Int ..] row ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    m = wM w
    (lw, ly, lh) = lampGeom env
    (lr, lnov, lout) = mLast m
    my = ly + lh + 100
    gy = my + 44
    gh = envH env - gy - 40
    gw = envW env * 0.55
    tx = 70 + gw
    cs = min 22 ((gh - 20) / 9)
    lamp c =
      let x = 40 + fromIntegral c * lw
          hue = wrapF 360 (fromIntegral c * 45 + f2 (mHue m))
          b = f2 (getAt 0 (mLamp m) c)
      in [ Color (hsb hue 70 (15 + 85 * b)) (rectTL (x + 6) ly (lw - 12) lh) ]
         ++ [ Color (hsba hue 60 100 (0.12 * b)) (at (x + lw / 2, ly + lh / 2) (disc ((lw - 12) * (0.5 + 0.35 * k) * b / 2))) | k <- [3, 2, 1], b > 0.01 ]
         ++ [ say (getAt "" names c ++ " (" ++ [toUpperA (getAt 'a' keysStr c)] ++ ")") (x + 16) (ly + lh + 26) 15 cTxt
            , Color (hsba 0 0 100 0.08) (rectTL (x + 10) (ly + lh + 38) (lw - 20) 10)
            , Color (col cAlarm 100) (rectTL (x + 10) (ly + lh + 38) ((lw - 20) * f2 (getAt 0 (mH m) c)) 10) ]
    toUpperA ch = if ch >= 'a' && ch <= 'z' then toEnum (fromEnum ch - 32) else ch
    onOff b = if b then "ON" else "OFF"

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (Char ch) Down _ _
    | Just c <- elemIndex (toLower ch) keysStr -> playChannel c w
    | ch `elem` "pP" -> pure w { wAuto = if wAuto w == 1 then 0 else 1 }
    | ch `elem` "zZ" -> pure w { wAuto = if wAuto w == 2 then 0 else 2 }
    | ch `elem` "rR" -> pure w { wM = makeMachine, wHist = [] }
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = p5Mouse env
        (lw, ly, lh) = lampGeom env
        c = floor ((mx - 40) / lw) :: Int
    in if c >= 0 && c < 8 && my > ly && my < ly + lh then playChannel c w else pure w
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("same note (Do) 20 times: response falls every time (" ++ fmt 2 (getAt 0 rs 0) ++ " -> " ++ fmt 2 (getAt 0 rs 19) ++ ")"
    , and (zipWith (<) (drop 1 rs) rs))
  , ("after tiring of Do, Re still gives habit 0 x new transition (novelty 1) = response 1", near fresh 1 1e-12)
  , ("15 s of rest clears habituation: 1 - h = " ++ fmt 3 back ++ " > 0.9 (decays as e^(-15/6))", back > 0.9)
  , ("Do Mi So 10 times: novelty " ++ fmt 2 (getAt 0 nov 4) ++ " -> " ++ fmt 2 (last' 0 nov) ++ " (1/3 or less)", last' 0 nov < getAt 0 nov 4 / 3)
  , ("1000 random notes: habituation and boredom stay in 0..1", ok5)
  , ("same note for 20 s -> long boredom changes the mood (colours) (" ++ show (mMoods m5) ++ " times)", mMoods m5 >= 1) ]
  where
    run20 = foldl' (\(acc, m) _ -> let (r, m') = hear 0 m in (r : acc, advance 0.2 m')) ([], makeMachine) [1 .. 20 :: Int]
    rs = reverse (fst run20)
    fresh = fst (hear 1 (snd run20))
    back = 1 - getAt 0 (mH (advance 15 (snd run20))) 0
    nov = reverse (fst (foldl' novStep ([], makeMachine) (concat (replicate 10 [0, 2, 4]))))
    novStep (acc, m) c = let m' = snd (hear c m)
                             (_, nv, _) = mLast m'
                         in (nv : acc, advance 0.3 m')
    ok5 = go5 (1000 :: Int) makeMachine (makeRng 1) True
    go5 0 _ _ ok = ok
    go5 k m (a : b : rest) ok =
      let m' = advance (b * 0.5) (snd (hear (floor (a * 8)) m))
          good = all (\v -> v >= 0 && v <= 1) (mH m') && mBored m' >= 0 && mBored m' <= 1
      in go5 (k - 1) m' rest (ok && good)
    go5 _ _ _ ok = ok
    m5 = foldl' (\m _ -> advance 0.1 (snd (hear 0 m))) makeMachine [1 .. 200 :: Int]

-- ---------- やってみよう ----------
-- 1. ド（A）だけを連打しよう。光はどうなる？ ほかの音に変えると？
-- 2. P で同じフレーズを流すと、機械はいつ「気分を変える」？ そのあと同じフレーズへの反応は？
-- 3. Z（でたらめ）と P（くり返し）で、応答のグラフを比べよう。機械が一番喜ぶのはどんな演奏？
-- 4. alpha や tauH を変えて、飽きっぽい機械・がまん強い機械を作ろう
-- 5. パスクは「演奏者と機械が互いに学び合う会話」と考えた。人が機械に合わせて弾き方を変えるのは、どちらが制御している？

-- ==================================================
-- ここから下は「サイバネティクスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で
p5Mouse :: Env -> (Float, Float)
p5Mouse env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 色は (色相, 彩度, 明るさ)。透明度は 0〜100
type HSB = (Float, Float, Float)

cTxt, cSub, cScope, cTube, cAlarm :: HSB
cTxt = (40, 12, 96)
cSub = (35, 18, 72)
cScope = (135, 70, 92)
cTube = (24, 85, 100)
cAlarm = (0, 80, 100)

col :: HSB -> Float -> Color
col (h, s, b) a = hsba h s b (a / 100)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> HSB -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size (col c 100) s))

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- オシロスコープ風の枠
plotFrame :: Float -> Float -> Float -> Float -> String -> Picture
plotFrame x y w h title = Pictures $
  [ Color (hsba 130 40 6 0.9) (rectTL x y w h) ]
  ++ [ Color (hsba 130 40 40 0.4) (Line [(x, y + h * k / 4), (x + w, y + h * k / 4)]) | k <- [1, 2, 3] ]
  ++ [ Color (hsb 42 55 60) (frameTL 1 x y w h) ]
  ++ [ say title (x + 8) (y + 18) 14 cSub | not (null title) ]

-- arr の値を lo〜hi の範囲で、枠 (x, y, w, h) に折れ線で描く（n = 横に並べる点の数）
plotLine :: [Double] -> Float -> Float -> Float -> Float -> Double -> Double -> HSB -> Int -> Float -> Picture
plotLine arr x y w h lo hi c n wt
  | length arr < 2 = Blank
  | otherwise = Color (col c 100) (thickPath wt [ (x + fromIntegral i / fromIntegral (n - 1) * w, y + h - f2 (clampv 0 1 ((fin lo v - lo) / (hi - lo))) * h)
                                              | (i, v) <- zip [0 :: Int ..] arr ])

-- ---- 音：周波数 f [Hz]、長さ dur 秒（0.05〜3）、大きさ vol（0.5 まで）、音色 wave
playNoteK :: Double -> Double -> Double -> Wave -> IO ()
playNoteK fr dur vol wave =
  when (not (isNaN fr) && not (isInfinite fr) && fr > 0) $
    playTone (realToFrac (min 4000 fr)) (realToFrac (max 0.0002 (min 0.5 vol))) (realToFrac (max 0.05 (min 3 dur))) wave

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- JavaScript の数の書き方に近い形（整数なら小数点なし）
num :: Double -> String
num v | v == fromIntegral (round v :: Int) = show (round v :: Int)
      | otherwise = showFFloat Nothing v ""

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- リストの i 番目を v にする
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [ if k == i then v else x | (k, x) <- zip [0 ..] xs ]

-- 最後の要素（なければ d）
last' :: a -> [a] -> a
last' d [] = d
last' _ xs = last xs

-- 決まった種（seed）から毎回同じ乱数列（0〜1）を出す（p5.js 版の makeRng と同じ mulberry32）
makeRng :: Word32 -> [Double]
makeRng = go
  where
    go s0 =
      let s = s0 + 0x6d2b79f5
          t1 = (s `xor` (s `shiftR` 15)) * (1 .|. s)
          t2 = (t1 + ((t1 `xor` (t1 `shiftR` 7)) * (61 .|. t1))) `xor` t1
          r = t2 `xor` (t2 `shiftR` 14)
      in fromIntegral r / 4294967296 : go s

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

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
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 16) 14 (if all snd rs then cScope else cAlarm)
