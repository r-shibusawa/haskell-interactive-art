-- ============================================
-- 69-14 真理値表のシーケンサー 🔊 ─ 16 拍のカウンタ（0〜15 の 2 進数）の各桁を変数にする：p = 1 の位、q = 2 の位、r = 4 の位、s = 8 の位
-- 16 拍 = 4 変数の真理値表の 16 行。段ごとの論理式が真（1）の拍で音が鳴る。式を変えるとリズムが変わる。
-- 恒真の段はいつも鳴り、矛盾の段は一度も鳴らない。p が一番速く変わるので、p だけの式は 1 拍おきのリズム
-- 操作：まずクリックかキーで音を ON（音量は小さめから）/ スペース 再生・停止 / ↑↓ 段を選ぶ / ← → テンポ / M 選んだ段を消音 / 1〜3 お手本
--       Tab 選んだ段の式を編集（編集中は文字キーが全部入力、~ & | -> <-> も可。Enter か Tab で終わる。まちがった式なら前の式のまま）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.Bits (shiftR, testBit, xor, (.&.), (.|.))
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)

data Snd = Kick | Snare | Hat | Bell | Bass deriving (Eq, Show)

banks :: [[(String, String, Snd)]]            -- ★ お手本：(段の名前, 式, 音)
banks =
  [ [("Kick", "~p & ~q", Kick), ("Snare", "q & ~p", Snare), ("Hi-hat", "p | ~p", Hat), ("Bell", "p & (r <-> s)", Bell), ("Bass", "~p & ~r", Bass), ("Contradiction", "p & ~p", Bell)]
  , [("Kick", "~p & ~q & ~r | (p & q & r)", Kick), ("Snare", "q & r & ~p", Snare), ("Hi-hat", "p | q", Hat), ("Bell", "p -> (q <-> s)", Bell), ("Bass", "s & ~p", Bass), ("Tautology", "(p -> q) | (q -> p)", Hat)]
  , [("Kick", "~(p | q)", Kick), ("Snare", "~(p | ~q)", Snare), ("Hi-hat", "p", Hat), ("Bell", "(p & q) | (r & s)", Bell), ("Bass", "~q & ~r & ~p", Bass), ("De Morgan", "~(p & q) <-> ~p | ~q", Hat)] ]

scaleMidi :: [Int]
scaleMidi = [60, 62, 64, 67, 69, 72, 74, 76]  -- ★ ベルの音階（ペンタトニック）。拍の q r s で選ぶ

master :: Float
master = 0.12                                 -- ★ 全体の音量（0.2 以下にしておく）

data Row = Row { rNm :: String, rSrc :: String, rF :: F, rSnd :: Snd, rMute :: !Bool }

data World = World
  { wRows    :: [Row]
  , wSel     :: !Int
  , wPlaying :: !Bool
  , wStep    :: !Int                          -- 次に鳴らす拍
  , wLastMs  :: !Float                        -- 最後に拍を進めた時刻（ミリ秒）
  , wBpm     :: !Int                          -- ★ テンポ（1 拍 = 16 分音符）
  , wEditing :: !Bool
  , wEditSrc :: String
  , wEditErr :: String
  , wSound   :: !Bool                         -- 音が ON か（最初のクリックかキーで ON）
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "69-14 Truth-table Sequencer" (loadBank 0 world0))
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (loadBank 0 world0) { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World [] 0 True 0 0 112 False "" "" False []

loadBank :: Int -> World -> World
loadBank k w = w { wRows = [ Row nm s (either (const FBot) id (parseFormula s)) snd' False | (nm, s, snd') <- at' banks k [] ], wSel = 0, wEditing = False }

-- ★ 拍 k → 変数（p = 1 の位、q = 2 の位、r = 4 の位、s = 8 の位）
bits :: Int -> Asg
bits k = M.fromList [("p", testBit k 0), ("q", testBit k 1), ("r", testBit k 2), ("s", testBit k 3)]

pattern :: F -> [Bool]
pattern f = [ evalFml f (bits k) | k <- [0 .. 15] ]

-- 式を読み、正しければ入れかえる。返り値：(まちがいの説明（"" = OK）, 段)
setRowFormula :: Row -> String -> (String, Row)
setRowFormula r s = case parseFormula s of
  Left (e, _) -> (e, r)
  Right f ->
    let extra = filter (`notElem` ["p", "q", "r", "s"]) (fmlVars f)
    in if not (null extra) then ("variables are p q r s only (" ++ unwords extra ++ " not allowed)", r)
       else ("", r { rSrc = fmlStr f, rF = f })

midiHz :: Int -> Float
midiHz m = 440 * 2 ** ((fromIntegral m - 69) / 12)

vol :: Float -> Float
vol v = v * min 0.2 master

-- 拍 k で、式が真の段の音を鳴らす（雑音は作れないので、スネアとハイハットは短い音で代用）
playStep :: World -> Int -> IO ()
playStep w k = forM_ (wRows w) $ \r -> when (not (rMute r) && evalFml (rF r) (bits k)) $ case rSnd r of
  Kick -> playTone 60 (vol 0.9) 0.25 Sine
  Snare -> playTone 190 (vol 0.45) 0.14 Sawtooth
  Hat -> playTone 6000 (vol 0.22) 0.04 Square
  Bass -> playTone (midiHz (36 + ((k `shiftR` 2) .&. 3) * 2)) (vol 0.35) 0.22 Sawtooth
  Bell -> playTone (midiHz (at' scaleMidi ((k `shiftR` 1) `mod` length scaleMidi) 60)) (vol 0.4) 0.25 Triangle

stepSec :: World -> Float
stepSec w = 60 / i2f (wBpm w) / 4

-- ---- 時間を進める（経過時間で拍を数える）
step :: Env -> Float -> World -> IO World
step env _ w
  | wPlaying w && now - wLastMs w > stepSec w * 1000 = do
      when (wSound w) (playStep w (wStep w))
      pure w { wLastMs = now, wStep = (wStep w + 1) `mod` 16 }
  | otherwise = pure w
  where now = envTime env * 1000

-- 格子の位置と大きさ（描くときとクリックで共通）：(x0, y0, gy, 列の幅, 段の高さ)
geom :: Env -> World -> (Float, Float, Float, Float, Float)
geom env w =
  let x0 = 220
      y0 = 150
      gy = y0 + 4 * 22 + 16
      n = max 1 (length (wRows w))
  in (x0, y0, gy, min 52 ((envW env - x0 - 160) / 16), max 30 (min 64 ((envH env - gy - 140) / i2f n)))

draw :: Env -> World -> Picture
draw env w =
  let (x0, y0, gy, cw, rh) = geom env w
      cur = (wStep w + 15) `mod` 16
      rows = wRows w
      nr = length rows
      vars = ["p", "q", "r", "s"]
      header = concat
        [ sayM v (x0 - 20) (y0 + i2f j * 22) 14 (col sky)
          : [ sayM (if b then "1" else "0") (x0 + i2f k * cw + cw / 2) (y0 + i2f j * 22) 14 (col (if k == cur then yel else if b then ink else linec))
            | k <- [0 .. 15], let b = testBit k j ]
        | (j, v) <- zip [0 ..] vars ]
        ++ [ sayM (show k) (x0 + i2f k * cw + cw / 2) (y0 - 24) 14 (col dimc) | k <- [0 .. 15 :: Int] ]
      rowPic i r =
        let y = gy + i2f i * rh
            pat = pattern (rF r)
            n = length (filter id pat)
            kc = if n == 16 then grn else if n == 0 then pink else yel
            fs0 = if wEditing w && i == wSel w then "editing" else rSrc r
            fs = if textW fs0 14 > x0 - 40 then take 15 fs0 ++ "..." else fs0
        in Pictures $
             [ Color (col linec) (rectTL 20 (y + 2) (envW env - 40) (rh - 4)) | i == wSel w ]
             ++ [ sayLM (rNm r ++ (if rMute r then " (muted)" else "")) 30 (y + rh * 0.32) 15 (col (if rMute r then dimc else ink))
                , sayLM fs 30 (y + rh * 0.68) 14 (col kc) ]
             ++ [ Color (col (if on && k == cur && wPlaying w then yel else if on then (if rMute r then dimc else (120, 170, 120)) else panelc))
                        (rectTL (x0 + i2f k * cw + 2) (y + 6) (cw - 4) (rh - 12))
                | (k, on) <- zip [0 :: Int ..] pat ]
             ++ [ sayLM (show n ++ " / 16" ++ (if n == 16 then " taut" else if n == 0 then " contra" else "")) (x0 + 16 * cw + 10) (y + rh / 2) 14 (col dimc) ]
      ey = gy + i2f nr * rh + 30
      selRow = at' rows (wSel w) (Row "" "" FBot Bell False)
  in view env $
       [ hudTitle "Truth-table sequencer: 16 beats = the 16 rows of a 4-variable truth table"
           ((if wSound w then "" else "Click (or press a key) first to turn sound on   ") ++ "Space play/stop   Up/Down row   Left/Right tempo " ++ show (wBpm w)
            ++ "\nM mute   1-3 examples   Tab edit formula (Enter or Tab to finish)") ]
       ++ header
       ++ zipWith rowPic [0 ..] rows
       ++ [ Color (col yel) (frameTL 2 (x0 + i2f cur * cw) (y0 - 36) cw (gy + i2f nr * rh - y0 + 40))
          -- ---- 編集欄
          , if wEditing w then drawInputLine (envFrame env) 30 ey (rNm selRow ++ " formula:") (wEditSrc w) (wEditErr w) True
            else say ("Selected row: " ++ rNm selRow ++ " \"" ++ clip 30 (rSrc selRow) ++ "\"   Tab to edit.  Beat k: p q r s = binary digits of k (p = 1s)") 30 ey 14 (col dimc)
          , say "Beats that sound = true assignments x 2 for each variable not in the formula. ~p & ~q: every 4th, p: every 2nd" 30 (ey + 50) 14 (col dimc)
          , drawSelfTest env (envW env - 180) (envH env - 14) (wTest w) ]

event :: Env -> Event -> World -> IO World
event env ev w0 = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (_, _, gy, _, rh) = geom env w
        (_, my) = toP5 env pos
        i = floor ((my - gy) / rh)
    in if i >= 0 && i < length (wRows w) && not (wEditing w) then w { wSel = i } else w
  EventKey k Down mods _
    | wEditing w -> editKey (typedKey k mods)       -- 編集モード：文字キーは全部入力
    | otherwise -> case typedKey k mods of
        TTab -> w { wEditing = True, wEditSrc = rSrc (curRow w), wEditErr = "" }
        TChar ' ' -> w { wPlaying = not (wPlaying w) }
        TUp -> w { wSel = wrapI (wSel w - 1) n }
        TDown -> w { wSel = wrapI (wSel w + 1) n }
        TLeft -> w { wBpm = max 60 (wBpm w - 8) }
        TRight -> w { wBpm = min 200 (wBpm w + 8) }
        TChar c
          | isKey 'm' c -> w { wRows = modifyAt (wSel w) (\r -> r { rMute = not (rMute r) }) (wRows w) }
          | c >= '1' && c <= '3' -> loadBank (fromEnum c - fromEnum '1') w
        _ -> w
  _ -> w0
  where
    w = w0 { wSound = True }                        -- クリックかキーで音を ON
    n = max 1 (length (wRows w))
    curRow x = at' (wRows x) (wSel x) (Row "" "" FBot Bell False)
    editKey tk = case tk of
      t | t == TEnter || t == TTab ->
        let (e, r') = setRowFormula (curRow w) (wEditSrc w)
            w1 = w { wRows = setAt (wSel w) r' (wRows w), wEditErr = e }
        in if null e || t /= TEnter then w1 { wEditing = False } else w1
      TBack -> reparse w { wEditSrc = dropLast (wEditSrc w) }
      TClear -> reparse w { wEditSrc = "" }
      TChar c -> reparse w { wEditSrc = typeInto (wEditSrc w) c fmlKeys 60 }
      _ -> reparse w
    reparse x = x { wEditErr = either fst (const "") (parseFormula (wEditSrc x)) }

-- ---------- やってみよう ----------
-- 1. キックを ~p & ~q & ~r にすると何拍に 1 回？ ~p だけなら？（変数が 1 つ増えるごとに半分）
-- 2. スネアの式を「2 拍目と 4 拍目」（拍 4 と 12）に鳴るように作ろう。ヒント：~p & ~q & r
-- 3. 恒真の段と矛盾の段を作ろう。真理値表（00）で「恒真」と出た式は、ここではいつも鳴る
-- 4. 同値な式（例：ド・モルガンの両辺）を 2 つの段に入れると、まったく同じリズムになる
-- 5. banks に自分のリズムを足そう。scaleMidi を変えるとベルの音階が変わる

-- ---- 自己テスト（p5.js 版と同じ項目。名前も同じ）
selfTest :: [(String, Bool)]
selfTest =
  [ ("お手本 3 組の式 " ++ show (length allF) ++ " 個が全部読めて、変数は p q r s だけ", all (\s -> fst (setRowFormula row0 s) == "") allF)
  , ("恒真の段（p ∨ ¬p）は 16 拍全部、矛盾の段（p ∧ ¬p）は 0 拍", and (pat "p | ~p") && not (or (pat "p & ~p")))
  , ("キック ¬p ∧ ¬q は 0, 4, 8, 12 拍、p だけなら 1 拍おき（奇数の拍）", on "~p & ~q" == [0, 4, 8, 12] && on "p" == [1, 3 .. 15])
  , ("どの段も「鳴る拍の数 = 真になる割り当ての数 × 2^(4 − 変数の数)」", all okCount allF)
  , ("拍 11 = 2 進数 1011 → p = 1, q = 1, r = 0, s = 1", b11 "p" && b11 "q" && not (b11 "r") && b11 "s")
  , ("まちがった式（p & や、変数 x）は説明を出して、前の式 p のまま"
    , not (null e1) && not (null e2) && rSrc r2 == "p" && e3 == "" && rSrc r3 == "q")
  , ("音量：全体 MASTER ≤ 0.2", master <= 0.2) ]
  where
    allF = [ s | b <- banks, (_, s, _) <- b ]
    row0 = Row "" "p" (FVar "p") Bell False
    parsed s = either (const FBot) id (parseFormula s)
    pat = pattern . parsed
    on s = [ k | (k, b) <- zip [0 :: Int ..] (pat s), b ]
    okCount s = let f = parsed s
                    (_, nTrue, _) = classifyFml f
                in length (filter id (pattern f)) == nTrue * 2 ^ (4 - length (fmlVars f))
    b11 v = M.findWithDefault False v (bits 11)
    (e1, r1) = setRowFormula row0 "p &"
    (e2, r2) = setRowFormula r1 "p & x"
    (e3, r3) = setRowFormula r2 "q"

-- ==================================================
-- ここから下は「論理の道具」（中身は読まなくても OK。消さないでね）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 画面の記号は ASCII：~ = ¬、& = ∧、| = ∨、-> = →、<-> = ↔、T = ⊤、_|_ = ⊥、[] = □、<> = ◇、\ = λ
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（黒板とチョーク）
type RGB = (Float, Float, Float)

bgc, ink, yel, sky, pink, grn, lilac, dimc, panelc, linec :: RGB
bgc = (40, 54, 47)                            -- 背景（黒板の灰緑）
ink = (238, 238, 228)                         -- 文字（白いチョーク）
yel = (246, 218, 120)
sky = (128, 186, 222)
pink = (238, 146, 150)
grn = (164, 214, 146)
lilac = (196, 170, 230)
dimc = (150, 166, 156)
panelc = (50, 66, 58)
linec = (92, 112, 102)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

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

-- (k + n) % n（負の数でも 0〜n−1）
wrapI :: Int -> Int -> Int
wrapI k n = if n <= 0 then 0 else k `mod` n

hyp :: Float -> Float -> Float                -- 長さ √(a² + b²)（Math.hypot）
hyp a b = sqrt (a * a + b * b)

nubOrd :: Ord a => [a] -> [a]                 -- 重なりを除く（最初の順を保つ。[...new Set(xs)]）
nubOrd = go S.empty
  where go _ [] = []
        go seen (x : xs) = if S.member x seen then go seen xs else x : go (S.insert x seen) xs

-- 厳密にリストを評価する（遅延評価の「つけ」をためない）
forceL :: [a] -> [a]
forceL xs = foldl' (flip seq) () xs `seq` xs

-- 長い文字列を n 文字で切る（p5.js 版の s.slice(0, n - 1) + '…'）
clip :: Int -> String -> String
clip n s = if length s > n then take (n - 1) s ++ "..." else s

-- 画面に出せない文字（ASCII 以外）を ? に
asciiOnly :: String -> String
asciiOnly = map (\c -> if c >= ' ' && c <= '~' then c else '?')

-- 文字の入力：allowed に入っている 1 文字だけ足す（長さの上限つき。p5.js 版の typeInto）
typeInto :: String -> Char -> String -> Int -> String
typeInto s ch allowed maxLen = if ch `elem` allowed && length s < maxLen then s ++ [ch] else s

dropLast :: [a] -> [a]                        -- s.slice(0, -1)
dropLast xs = if null xs then xs else init xs

-- キーの名前をそろえる（gloss では スペース・Enter・Backspace・Tab・Esc が SpecialKey で来る）
-- TClear = 全部消す（p5.js 版の Esc。gloss では Esc でウィンドウが閉じるので Shift + Backspace でも）
data TypedKey = TChar Char | TBack | TClear | TEnter | TTab | TUp | TDown | TLeft | TRight | TOther
  deriving (Eq, Show)

typedKey :: Key -> Modifiers -> TypedKey
typedKey k m = case k of
  Char '\b' -> back
  Char '\DEL' -> back
  Char '\t' -> TTab
  Char '\r' -> TEnter
  Char '\n' -> TEnter
  Char '\ESC' -> TClear
  Char c -> TChar c
  SpecialKey KeySpace -> TChar ' '
  SpecialKey KeyBackspace -> back
  SpecialKey KeyDelete -> back                -- Mac の delete キー
  SpecialKey KeyEnter -> TEnter
  SpecialKey KeyPadEnter -> TEnter
  SpecialKey KeyTab -> TTab
  SpecialKey KeyEsc -> TClear
  SpecialKey KeyUp -> TUp
  SpecialKey KeyDown -> TDown
  SpecialKey KeyLeft -> TLeft
  SpecialKey KeyRight -> TRight
  _ -> TOther
  where back = if shift m == Down then TClear else TBack

-- 文字（大文字・小文字のどちらでも）か
isKey :: Char -> Char -> Bool
isKey k c = c == k || c == toUpperC k

toUpperC :: Char -> Char
toUpperC c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

type PErr = (String, Int)                     -- (まちがいの説明, 何文字目)

-- まちがいの説明を 1 行に（p5.js 版の「（n 文字目あたり）」）
errLine :: PErr -> String
errLine (m, a) = m ++ " (near character " ++ show (a + 1) ++ ")"

-- 数の書き方：小数点以下 d 桁（toFixed と同じく、半分なら大きいほうへ）
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

rndPick :: [a] -> R a                         -- リストから 1 つ（空のリストには使わない）
rndPick xs = do
  u <- rnd
  let i = floor (u * fromIntegral (length xs))
  pure (case drop i xs of
          (x : _) -> x
          [] -> case xs of
            (x : _) -> x
            [] -> errorWithoutStackTrace "rndPick: empty list")

-- n 回くり返す（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- ---- 論理式の道具：字句 → 構文木（再帰下降）→ 文字列・真理値
-- 構文木 = FVar 名前 / FTop ⊤ / FBot ⊥ / FNot a / FBin 演算子 a b（様相論理だけ FBox a □ と FDia a ◇）
-- 強さの順：~ [] <> ＞ & ＞ | ＞ ->（右から結合：p->q->r = p->(q->r)）＞ <->
-- ASCII で書く：~ ! = ¬、& ^ = ∧、| = ∨、-> > = →、<-> = = ↔、1 T = ⊤、0 _|_ = ⊥、[] = □、<> = ◇（記号のままでも読める）
data Op = OAnd | OOr | OImp | OIff deriving (Eq, Ord, Show)

data F = FVar String | FTop | FBot | FNot F | FBin Op F F | FBox F | FDia F
  deriving (Eq, Ord, Show)

fmlKeys :: String                             -- 入力できる文字
fmlKeys = ['a' .. 'z'] ++ ['0' .. '9'] ++ "~!&|^-<>=()[] T_"

fmlMaxVars :: Int
fmlMaxVars = 6                                -- ★ 変数の数の上限（真理値表は 2⁶ = 64 行まで）

opSym :: Op -> String                         -- 画面の記号（ASCII）
opSym o = case o of
  OAnd -> "&"
  OOr -> "|"
  OImp -> "->"
  OIff -> "<->"

fVar :: String -> F
fVar = FVar

fNot :: F -> F
fNot = FNot

fBin :: Op -> F -> F -> F
fBin = FBin

fKids :: F -> [F]                             -- 子（構文木の下の段）
fKids f = case f of
  FNot a -> [a]
  FBox a -> [a]
  FDia a -> [a]
  FBin _ a b -> [a, b]
  _ -> []

data Tk = TkVar String | TkNot | TkAnd | TkOr | TkImp | TkIff | TkL | TkR | TkTop | TkBot | TkBox | TkDia
  deriving (Eq, Show)

tkSym :: Tk -> String
tkSym t = case t of
  TkNot -> "~"
  TkAnd -> "&"
  TkOr -> "|"
  TkImp -> "->"
  TkIff -> "<->"
  TkL -> "("
  TkR -> ")"
  TkTop -> "T"
  TkBot -> "_|_"
  TkBox -> "[]"
  TkDia -> "<>"
  TkVar n -> n

lexFormula :: Bool -> String -> Either PErr [(Tk, Int)]
lexFormula modal src = go 0 src
  where
    go _ [] = Right []
    go i s@(c : rest)
      | c == ' ' = go (i + 1) rest
      | pre "<->" = tok TkIff 3
      | pre "->" = tok TkImp 2
      | pre "_|_" = tok TkBot 3
      | pre "[]" || pre "<>" = if modal then tok (if c == '[' then TkBox else TkDia) 2
                               else Left ("[] <> (box, diamond) only in modal logic (12)", i)
      | c >= 'a' && c <= 'z' =
          let tl = takeWhile (\d -> (d >= '0' && d <= '9') || d == '\'') rest
              k = 1 + length tl
          in ((TkVar (c : tl), i) :) <$> go (i + k) (drop (k - 1) rest)
      | Just t <- lookup c one =
          if (t == TkBox || t == TkDia) && not modal then Left ("box and diamond only in modal logic (12)", i)
          else tok t 1
      | c `elem` "-<[]_" = Left ("'" ++ [c] ++ "' alone is not a symbol (write -> <-> [] <>)", i)
      | otherwise = Left ("character '" ++ asciiOnly [c] ++ "' not allowed", i)
      where pre p = take (length p) s == p
            tok t k = ((t, i) :) <$> go (i + k) (drop k s)
    one = [ ('¬', TkNot), ('~', TkNot), ('!', TkNot), ('∧', TkAnd), ('&', TkAnd), ('^', TkAnd), ('∨', TkOr), ('|', TkOr)
          , ('→', TkImp), ('>', TkImp), ('↔', TkIff), ('=', TkIff), ('(', TkL), (')', TkR)
          , ('⊤', TkTop), ('1', TkTop), ('T', TkTop), ('⊥', TkBot), ('0', TkBot), ('□', TkBox), ('◇', TkDia) ]

-- 返り値 Right 式 / Left (説明, 何文字目)（例外は出さない）。modal = 様相論理の □ ◇ を使えるか
parseFormulaM :: Bool -> String -> Either PErr F
parseFormulaM modal src
  | length src > 160 = Left ("too long (160 characters max)", 160)
  | otherwise = do
      toks <- lexFormula modal src
      if null toks then Left ("empty formula", 0) else do
        (f, rest) <- pIff 0 toks
        case rest of
          ((t, a) : _) -> Left (leftover t, a)
          [] -> let nv = length (fmlVars f)
                in if nv > fmlMaxVars then Left (show nv ++ " variables (" ++ show fmlMaxVars ++ " max)", 0) else Right f
  where
    len = length src
    atOf ts = case ts of
      ((_, a) : _) -> a
      [] -> len
    failAt ts m = Left (m, atOf ts)
    leftover t = case t of
      TkR -> "extra ')'"
      TkVar _ -> needOp
      TkL -> needOp
      TkTop -> needOp
      TkBot -> needOp
      TkNot -> needOp
      _ -> "'" ++ tkSym t ++ "' in a strange place"
    needOp = "an operator (& | -> ...) is needed here"
    -- 2 項演算の 1 段：左結合（right なら右結合）
    binL sub op d ts = do
      (a, r) <- sub d ts
      loop a r
      where loop a r = case r of
              ((t, _) : r') | t == op -> do (b, r2) <- sub d r'; loop (FBin (tkOp op) a b) r2
              _ -> Right (a, r)
    binR sub op d ts = do
      (a, r) <- sub d ts
      case r of
        ((t, _) : r') | t == op -> do (b, r2) <- binR sub op d r'; Right (FBin (tkOp op) a b, r2)
        _ -> Right (a, r)
    tkOp t = case t of
      TkAnd -> OAnd
      TkOr -> OOr
      TkImp -> OImp
      _ -> OIff
    pIff = binL pImp TkIff
    pImp = binR pOr TkImp
    pOr = binL pAnd TkOr
    pAnd = binL pUn TkAnd
    pUn d ts
      | d + 1 > 80 = failAt ts "nesting too deep"
      | otherwise = case ts of
          [] -> failAt ts "the formula ends too early"
          ((t, _) : r) -> case t of
            TkNot -> do (a, r2) <- pUn (d + 1) r; Right (FNot a, r2)
            TkBox -> do (a, r2) <- pUn (d + 1) r; Right (FBox a, r2)
            TkDia -> do (a, r2) <- pUn (d + 1) r; Right (FDia a, r2)
            TkVar nm -> Right (FVar nm, r)
            TkTop -> Right (FTop, r)
            TkBot -> Right (FBot, r)
            TkL -> do
              (a, r2) <- pIff (d + 1) r
              case r2 of
                ((TkR, _) : r3) -> Right (a, r3)
                _ -> failAt r2 "')' missing"
            TkR -> failAt ts "nothing before ')'"
            _ -> failAt ts ("nothing before '" ++ tkSym t ++ "'")

parseFormula :: String -> Either PErr F
parseFormula = parseFormulaM False

fmlPrec :: F -> Int                           -- 2 項演算の強さ（2 項演算でなければ 0）
fmlPrec f = case f of
  FBin OIff _ _ -> 1
  FBin OImp _ _ -> 2
  FBin OOr _ _ -> 3
  FBin OAnd _ _ -> 4
  _ -> 0

-- 構文木 → 文字列（かっこは必要な所と、ちがう演算子が混ざる所に）
fmlStr :: F -> String
fmlStr f = case f of
  FVar n -> n
  FTop -> "T"
  FBot -> "_|_"
  FNot a -> "~" ++ un a
  FBox a -> "[]" ++ un a
  FDia a -> "<>" ++ un a
  FBin op a b ->
    let side c isLeft = case c of
          FBin op2 _ _ -> let assocOk = op2 == op && (if op == OImp then not isLeft else isLeft)
                          in if assocOk then fmlStr c else "(" ++ fmlStr c ++ ")"
          _ -> fmlStr c
    in side a True ++ " " ++ opSym op ++ " " ++ side b False
  where un a = if fmlPrec a > 0 then "(" ++ fmlStr a ++ ")" else fmlStr a

-- 出てくる変数（名前の順）
fmlVars :: F -> [String]
fmlVars f = S.toList (go f S.empty)
  where go g s = case g of
          FVar n -> S.insert n s
          _ -> foldr go s (fKids g)

type Asg = M.Map String Bool                  -- 割り当て（変数 → 真偽）

-- 真理値（□ ◇ は真理値表では評価しない → 偽。12 を見よ）
evalFml :: F -> Asg -> Bool
evalFml f e = case f of
  FVar n -> M.findWithDefault False n e
  FTop -> True
  FBot -> False
  FNot a -> not (evalFml a e)
  FBin OAnd a b -> evalFml a e && evalFml b e
  FBin OOr a b -> evalFml a e || evalFml b e
  FBin OImp a b -> not (evalFml a e) || evalFml b e
  FBin OIff a b -> evalFml a e == evalFml b e
  _ -> False

-- k 番目の割り当て（上の桁が最初の変数）
envOf :: [String] -> Int -> Asg
envOf vars k = M.fromList [ (v, testBit k (length vars - 1 - i)) | (i, v) <- zip [0 ..] vars ]

-- 真理値表の行 (割り当て, 値)
truthRowsV :: [String] -> F -> [(Asg, Bool)]
truthRowsV vars f = [ (e, evalFml f e) | k <- [0 .. 2 ^ length vars - 1 :: Int], let e = envOf vars k ]

truthRows :: F -> [(Asg, Bool)]
truthRows f = truthRowsV (fmlVars f) f

data Kind = Taut | Sat | Contra deriving (Eq, Show)

-- 恒真 / 充足可能（恒真ではない）/ 矛盾、真の行の数、行の数
classifyFml :: F -> (Kind, Int, Int)
classifyFml f =
  let rows = truthRows f
      n = length (filter snd rows)
      m = length rows
  in (if n == m then Taut else if n == 0 then Contra else Sat, n, m)

kindOf :: F -> Kind
kindOf f = let (k, _, _) = classifyFml f in k

kindName :: Kind -> String
kindName k = case k of
  Taut -> "TAUTOLOGY (valid)"
  Sat -> "SATISFIABLE (not valid)"
  Contra -> "CONTRADICTION (unsatisfiable)"

fmlEq :: F -> F -> Bool
fmlEq = (==)

fmlSize :: F -> Int
fmlSize f = 1 + sum (map fmlSize (fKids f))

-- 全部の割り当てで同じ値か（同値）
sameTable :: F -> F -> Bool
sameTable f g =
  let vars = S.toList (S.fromList (fmlVars f ++ fmlVars g))
  in and [ evalFml f e == evalFml g e | k <- [0 .. 2 ^ length vars - 1 :: Int], let e = envOf vars k ]

-- でたらめな式（自己テスト用、種つき乱数。p5.js 版と同じ順に引く）
randomFml :: Int -> [String] -> Bool -> R F
randomFml d vars consts = do
  r <- rnd
  if d <= 0 || r < 0.22 then do
    c <- if consts then (< 0.12) <$> rnd else pure False
    if c then do u <- rnd; pure (if u < 0.5 then FTop else FBot)
    else FVar <$> rndPick vars
  else if r < 0.38 then FNot <$> randomFml (d - 1) vars consts
  else do
    op <- rndPick [OAnd, OOr, OImp, OIff, OAnd, OOr]
    a <- randomFml (d - 1) vars consts
    b <- randomFml (d - 1) vars consts
    pure (FBin op a b)

fmlTreeLabel :: F -> String                   -- 木の丸の中の文字
fmlTreeLabel f = case f of
  FVar n -> n
  FTop -> "T"
  FBot -> "_|_"
  FNot _ -> "~"
  FBox _ -> "[]"
  FDia _ -> "<>"
  FBin op _ _ -> opSym op

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

textW :: String -> Float -> Float             -- 文字の幅（だいたい）
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

-- 点 (x, y) が長方形 (bx, by, bw, bh) の中か
inBox :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
inBox (mx, my) (bx, by, bw, bh) = mx > bx && mx < bx + bw && my > by && my < by + bh

-- 左上のタイトルと操作の説明（説明は \n で改行、行の間 19）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- 黒板のパネル（p5.js 版の角の丸い長方形）
chalkPanel :: Float -> Float -> Float -> Float -> Picture
chalkPanel x y w h = Color (col panelc) (rectTL x y w h)

-- 入力欄：ラベル、文字列（点滅するカーソル）、まちがいの説明。active = False なら灰色でカーソルなし
drawInputLine :: Int -> Float -> Float -> String -> String -> String -> Bool -> Picture
drawInputLine frame x y lab s err active =
  let lx = x + textW lab 15 + 8
      blink = active && frame `mod` 60 < 30
  in Pictures
       [ say lab x y 15 (col (if active then yel else dimc))
       , say (asciiOnly s ++ (if blink then "_" else "")) lx (y + 1) 20 (col ink)
       , if null err then Blank else say ("! " ++ asciiOnly err) lx (y + 22) 14 (col pink) ]

-- ---- 自己テストの道具
-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  hSetEncoding stdout utf8
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 小さな表示（x, y は文字の左下。右端からはみ出さないように左へ寄せる）
drawSelfTest :: Env -> Float -> Float -> [(String, Bool)] -> Picture
drawSelfTest env x y rs = say (selfTestLine rs) (min x (envW env - 240)) y 14 (if all snd rs then col grn else col pink)
