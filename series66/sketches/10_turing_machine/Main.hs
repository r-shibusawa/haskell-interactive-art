-- ============================================
-- 66-10 チューリング機械 ─ 無限に長いテープと、1 マスだけ読み書きできるヘッドと、状態表
-- 1 手 = 「いまの状態」と「ヘッドの下の記号」で表を引き、記号を書いて、左右に 1 マス動き、次の状態へ。
-- 規則がない組み合わせに来たら止まる。こんな単純な機械で、どんな計算もできる（と考えられている）。
-- 右の時空図は、テープの 1 手ごとの様子を上から下へ並べたもの
-- 操作：1 2 進数に 1 を足す / 2 単項の足し算 / 3 回文の判定 / スペース 1 手 / A 自動 / ↑↓ 速さ / R 最初から / I 新しい入力
--       表のマスをクリックで選んで W 書く記号 / M 動く向き / N 次の状態 を順に変える / Backspace その規則を消す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (cyan, rose, violet)
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (elemIndex, foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32, Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- プログラム：名前・記号・状態・開始・新しい入力の作り方（乱数）・規則
data Prog = Prog { pName :: String, pSyms :: [Char], pStates :: [String], pStart :: String, pInputs :: R String
                 , pCode :: [((String, Char), (Char, Char, String))] }

programs :: [Prog]
programs =
  [ Prog "add 1 to a binary number" "01_" ["right", "carry"] "right" (toBin <$> rndInt 0 63)
      [ (("right", '0'), ('0', 'R', "right")), (("right", '1'), ('1', 'R', "right")), (("right", '_'), ('_', 'L', "carry"))
      , (("carry", '1'), ('0', 'L', "carry")), (("carry", '0'), ('1', 'L', "H")), (("carry", '_'), ('1', 'L', "H")) ]
  , Prog "unary addition (111+11 -> 11111)" "1+_" ["A", "B", "C"] "A"
      (do a <- rndInt 0 5; b <- rndInt 0 5; pure (replicate a '1' ++ "+" ++ replicate b '1'))
      [ (("A", '1'), ('1', 'R', "A")), (("A", '+'), ('1', 'R', "B")), (("B", '1'), ('1', 'R', "B")), (("B", '_'), ('_', 'L', "C")), (("C", '1'), ('_', 'L', "H")) ]
  , Prog "palindrome check (Y = accept, N = reject)" "ab_" ["read", "aR", "bR", "aChk", "bChk", "back"] "read"
      (do n <- rndInt 1 4
          h <- rndString "ab" n
          u <- rnd
          if u < 0.5 then pure (h ++ reverse h) else do m <- rndInt 2 7; rndString "ab" m)
      [ (("read", 'a'), ('_', 'R', "aR")), (("read", 'b'), ('_', 'R', "bR")), (("read", '_'), ('_', 'R', "Y"))
      , (("aR", 'a'), ('a', 'R', "aR")), (("aR", 'b'), ('b', 'R', "aR")), (("aR", '_'), ('_', 'L', "aChk"))
      , (("bR", 'a'), ('a', 'R', "bR")), (("bR", 'b'), ('b', 'R', "bR")), (("bR", '_'), ('_', 'L', "bChk"))
      , (("aChk", 'a'), ('_', 'L', "back")), (("aChk", 'b'), ('b', 'L', "N")), (("aChk", '_'), ('_', 'L', "Y"))
      , (("bChk", 'b'), ('_', 'L', "back")), (("bChk", 'a'), ('a', 'L', "N")), (("bChk", '_'), ('_', 'L', "Y"))
      , (("back", 'a'), ('a', 'L', "back")), (("back", 'b'), ('b', 'L', "back")), (("back", '_'), ('_', 'R', "read")) ] ]

prog :: Int -> Prog
prog k = at' programs k (Prog "" "" [] "" (pure "") [])

symCol :: Char -> Maybe RGB                   -- テープの記号の色
symCol c = lookup c [('1', (255, 170, 50)), ('0', (70, 150, 160)), ('a', (255, 170, 50)), ('b', (70, 215, 230)), ('+', (185, 150, 255))]

histLo, histCells, histRows :: Int
histLo = -6                                   -- 時空図のテープの左端
histCells = 40
histRows = 110

makeMachine :: Int -> TM
makeMachine p = let pr = prog p in TM (pStates pr) (pSyms pr) '_' (M.fromList (pCode pr)) (pStart pr) "H"

data World = World
  { wProg   :: !Int
  , wMach   :: TM
  , wCfg    :: Cfg
  , wInput  :: String
  , wHist   :: [(String, Int)]                -- 時空図の行（テープと、ヘッドの位置）
  , wHistPic :: Picture                       -- 時空図の絵（変わったときだけ作りなおす）
  , wAuto   :: !Bool
  , wSpeed  :: !Int
  , wSel    :: Maybe (String, Char)           -- 選んだ表のマス
  , wRng    :: Rng                            -- 乱数の列（p5.js 版と同じく、はじめは種 1）
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "66-10 Turing machine" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = loadProgram 0 "1011" (World 0 (makeMachine 0) (tmStart (makeMachine 0) "" 0) "" [] Blank False 4 Nothing (Rng 1) [])

loadProgram :: Int -> String -> World -> World
loadProgram p inp w = resetRun w { wProg = p, wMach = makeMachine p, wInput = inp, wSel = Nothing }

resetRun :: World -> World
resetRun w = let c = tmStart (wMach w) (wInput w) 0 in redraw w { wCfg = c, wHist = [(histRow c, cHead c)] }

histRow :: Cfg -> String
histRow c = tmTape c '_' histLo (histLo + histCells - 1)

doStep :: World -> World
doStep w = case tmStep (wMach w) (wCfg w) of
  (True, c) -> w { wCfg = c, wHist = let h = wHist w ++ [(histRow c, cHead c)] in drop (length h - histRows) h }
  (False, c) -> w { wCfg = c }                -- 規則がない → 止まった印だけつける

-- 時空図（1 行 = 1 手）を絵にしておく。1 マス = 横 8 × 縦 1 ピクセル（縦は 3 倍に引きのばす）
redraw :: World -> World
redraw w =
  let rows = IM.fromList (zip [0 ..] (wHist w))
      px x y = case IM.lookup y rows of
        Nothing -> (14, 26, 20)
        Just (row, hd) ->
          let k = x `div` 8
              sub = x `mod` 8
          in if hd - histLo == k && sub >= 2 && sub < 5 then (255, 255, 255)
             else case symCol (at' row k '_') of
                    Just (r, g, b) | sub < 7 -> (w8 r, w8 g, w8 b)
                    _ -> (14, 26, 20)
      w8 v = fromIntegral (round v :: Int) :: Word8
  in w { wHistPic = pixelImage (histCells * 8) histRows 1 px }

step :: Env -> World -> World
step env w
  | wAuto w && not (cHalted (wCfg w)) && envFrame env `mod` max 1 (6 - wSpeed w) == 0 =
      let w1 = iterate doStep w !! wSpeed w in if cSteps (wCfg w1) /= cSteps (wCfg w) then redraw w1 else w1
  | otherwise = w

-- 状態表のマス：(x, y, 幅, 高さ, 状態, 記号)
cellBoxes :: World -> [(Float, Float, Float, Float, String, Char)]
cellBoxes w = [ (tx + 80 + i2f j * cw, ty + 26 + i2f i * rh + 2, cw - 6, rh - 4, q, s)
              | (i, q) <- zip [0 :: Int ..] (tmStates (wMach w)), (j, s) <- zip [0 :: Int ..] (tmSyms (wMach w)) ]
  where tx = 30; ty = 270; cw = 110; rh = 30

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Turing machine: " ++ pName pr)
             ("1 2 3 program   Space 1 step   A auto (" ++ (if wAuto w then "ON" else "OFF") ++ ")   Up/Down speed " ++ show (wSpeed w) ++ "   R restart   I new input\nclick a table cell -> W symbol to write / M direction / N next state / Backspace delete the rule")
  -- ---- テープ
  , say ("input \"" ++ wInput w ++ "\"   steps " ++ show (cSteps c) ++ "   state " ++ cQ c ++ (if cHalted c then "   -> halted" else "")) 30 110 16 (col ink)
  , drawTape c '_' 30 140 40 (floor ((envW env - 60) / 40)) (max (cHead c) 8) symCol
  , if cHalted c then say haltMsg 30 230 20 (col lime) else Blank
  -- ---- 状態表
  , say "State table (row = state, column = symbol read, cell = write move next)" 30 260 15 (col ink)
  , Pictures [ sayM (if s == '_' then "blank _" else [s]) (110 + i2f j * 110 + 55) 282 14 (col dimc) | (j, s) <- zip [0 :: Int ..] (tmSyms m) ]
  , Pictures [ sayLM q 30 (296 + i2f i * 30 + 15) 14 (col (if q == cQ c then amber else dimc)) | (i, q) <- zip [0 :: Int ..] (tmStates m) ]
  , Pictures [ Pictures [ Color (col (if on then (70, 55, 20) else panel)) (rectTL x y bw bh)
                        , Color (col (if isSel then violet else if on then amber else wire)) (frameTL (if isSel || on then 2.5 else 1) x y bw bh)
                        , sayM (maybe "(halts)" (\(a, d, nq) -> [a, ' ', d, ' '] ++ nq) r) (x + bw / 2) (y + bh / 2) 14 (col (if r == Nothing then dimc else ink)) ]
             | (x, y, bw, bh, q, s) <- cellBoxes w
             , let r = M.lookup (q, s) (tmRules m)
             , let on = q == cQ c && s == tmRead c (cHead c) '_' && not (cHalted c)
             , let isSel = wSel w == Just (q, s) ]
  , say "H / Y / N = halting states. Violet = selected cell (change it with W M N)" 30 (310 + i2f (length (tmStates m)) * 30) 14 (col dimc)
  -- ---- 時空図
  , say "Space-time diagram (across = tape, down = time)" hx (hy - 10) 15 (col ink)
  , Translate (hx + iw * sc / 2) (hy + ih * sc / 2) (Scale sc (-3 * sc) (wHistPic w))
  , Color (col wire) (frameTL 1 hx hy (iw * sc) (ih * sc))
  , drawSelfTest env (wTest w) ]
  where
    pr = prog (wProg w)
    m = wMach w
    c = wCfg w
    out = tmContent c '_'
    haltMsg = case wProg w of
      0 -> "result " ++ out ++ "   (in decimal " ++ show (fromBin (if null (wInput w) then "0" else wInput w)) ++ " + 1 = " ++ show (fromBin (if null out then "0" else out)) ++ ")"
      1 -> "result " ++ show (length out) ++ " ones   (" ++ joinPlus (map (show . length) (splitOn '+' (wInput w))) ++ ")"
      _ -> if cQ c == "Y" then "accept (a palindrome)" else "reject (not a palindrome)"
    hx = envW env * 0.6
    hy = 270
    iw = i2f (histCells * 8)
    ih = i2f (histRows * 3)
    sc = min ((envW env - hx - 30) / iw) ((envH env - hy - 60) / ih)

splitOn :: Char -> String -> [String]
splitOn d s = case break (== d) s of
  (a, _ : rest) -> a : splitOn d rest
  (a, []) -> [a]

joinPlus :: [String] -> String
joinPlus xs = case xs of
  [] -> ""
  _ -> foldr1 (\a b -> a ++ " + " ++ b) xs

-- 配列の中で v の次（なければ先頭）
cycleNext :: Eq a => [a] -> a -> a -> a
cycleNext arr v d = at' arr (maybe 0 (+ 1) (elemIndex v arr) `mod` max 1 (length arr)) d

editRule :: Char -> World -> World
editRule what w = case wSel w of
  Nothing -> w
  Just (q, s) ->
    let m = wMach w
        (a, d, nq) = M.findWithDefault (s, 'R', q) (q, s) (tmRules m)
        r' = case what of
          'w' -> (cycleNext (tmSyms m) a a, d, nq)
          'm' -> (a, cycleNext "LR" d d, nq)
          _ -> (a, d, cycleNext (tmStates m ++ (if wProg w == 2 then ["Y", "N"] else ["H"])) nq nq)
    in resetRun w { wMach = m { tmRules = M.insert (q, s) r' (tmRules m) } }

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (x, y) = toP5 env p
    in foldl' (\wa (bx, by, bw, bh, q, s) -> if x > bx && x < bx + bw && y > by && y < by + bh then wa { wSel = Just (q, s) } else wa) w (cellBoxes w)
  EventKey k Down _ _ -> case typedKey k of
    TBack | Just sel <- wSel w -> resetRun w { wMach = (wMach w) { tmRules = M.delete sel (tmRules (wMach w)) } }
    _ -> case k of
      Char c | c >= '1' && c <= '3' ->
                 let p = fromEnum c - fromEnum '1'
                     (inp, g) = runR (pInputs (prog p)) (rngOf ((p + 1) * 11))
                 in loadProgram p inp w { wRng = g }
             | isKey 'a' c -> w { wAuto = not (wAuto w) }
             | isKey 'r' c -> resetRun w
             | isKey 'i' c -> let (inp, g) = runR (pInputs (prog (wProg w))) (wRng w) in resetRun w { wInput = inp, wRng = g }
             | isKey 'w' c -> editRule 'w' w
             | isKey 'm' c -> editRule 'm' w
             | isKey 'n' c -> editRule 'n' w
      SpecialKey KeySpace -> redraw (doStep w)
      SpecialKey KeyUp -> w { wSpeed = min 8 (wSpeed w + 1) }
      SpecialKey KeyDown -> w { wSpeed = max 1 (wSpeed w - 1) }
      _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 「2 進数に 1 を足す」で 1111 を入れると、carry が左へ走っていく様子を時空図で見よう
-- 2. 状態表を 1 マスこわして（W や N）、どこで答えがまちがうか確かめよう。R で最初から
-- 3. 「2 進数から 1 を引く」機械を作ろう：carry の代わりに「繰り下がり」（0 を 1 にして左へ、1 を 0 にして止まる）
-- 4. 単項の足し算を「かけ算」にするには状態がたくさん要る。どんな手順なら作れそう？
-- 5. 止まらない機械も作れる（右へ右へ進み続ける）。止まるかどうかを前もって判定できるか → 11 の忙しいビーバーと停止問題

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("2 進数に 1 を足す：n = 0〜255 で結果のテープ = n + 1（最大 " ++ show maxSteps ++ " 手で止まる）", all (\(n, c) -> cHalted c && tmContent c '_' == toBin (n + 1)) runs0)
  , ("単項の足し算：a, b = 0〜8 の 81 通りで 1 の数 = a + b"
    , and [ cHalted c && tmContent c '_' == replicate (a + b) '1' | a <- [0 .. 8], b <- [0 .. 8], let c = tmRun m1 (replicate a '1' ++ "+" ++ replicate b '1') 1000 0 ])
  , ("回文の判定：長さ 8 までの全部 511 個で、Y で止まる = 回文", and [ cHalted c && (cQ c == "Y") == (s == reverse s) | s <- allStrings "ab" 8, let c = tmRun m2 s 5000 0 ])
  , ("1011 + 1 = 1100 は 8 手で止まり、止まったあとは 1 手も進まない", tmContent c4' '_' == "1100" && cSteps c4 == 8 && cSteps c4' == 8) ]
  where
    m0 = makeMachine 0
    m1 = makeMachine 1
    m2 = makeMachine 2
    runs0 = [ (n, tmRun m0 (toBin n) 1000 0) | n <- [0 .. 255] ]
    maxSteps = maximum (0 : map (cSteps . snd) runs0)
    c4 = tmRun m0 "1011" 100 0
    c4' = snd (tmStep m0 c4)

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

-- ---- チューリング機械の道具
-- 機械 = 状態・記号・空白・規則（(状態, 読んだ記号) → (書く記号, 向き 'L' 'R', 次の状態)）・開始・止まる状態
--   規則がないときも止まる（「止まる」= 規則がない or 状態 H）
data TM = TM { tmStates :: [String], tmSyms :: [Char], tmBlank :: Char, tmRules :: M.Map (String, Char) (Char, Char, String), tmStartQ :: String, tmHalt :: String }

data Cfg = Cfg { cTape :: M.Map Int Char, cHead :: !Int, cQ :: String, cSteps :: !Int, cHalted :: !Bool, cLo :: !Int, cHi :: !Int }

tmStart :: TM -> String -> Int -> Cfg
tmStart m s h = Cfg (M.fromList [ (i, c) | (i, c) <- zip [0 ..] s, c /= tmBlank m ]) h (tmStartQ m) 0 False 0 (max 0 (length s - 1))

tmRead :: Cfg -> Int -> Char -> Char
tmRead cfg i blank = M.findWithDefault blank i (cTape cfg)

-- 1 手：読む → 書く → 動く → 状態を変える（動いたら True）
tmStep :: TM -> Cfg -> (Bool, Cfg)
tmStep m cfg
  | cHalted cfg = (False, cfg)
  | otherwise = case M.lookup (cQ cfg, tmRead cfg (cHead cfg) (tmBlank m)) (tmRules m) of
      Just (w, d, nq) | cQ cfg /= tmHalt m ->
        let tape = if w == tmBlank m then M.delete (cHead cfg) (cTape cfg) else M.insert (cHead cfg) w (cTape cfg)
            h = cHead cfg + (if d == 'R' then 1 else if d == 'L' then -1 else 0)
        in (True, cfg { cTape = tape, cHead = h, cQ = nq, cSteps = cSteps cfg + 1, cLo = min (cLo cfg) h, cHi = max (cHi cfg) h, cHalted = nq == tmHalt m })
      _ -> (False, cfg { cHalted = True })

tmRun :: TM -> String -> Int -> Int -> Cfg
tmRun m s maxSteps h = go (tmStart m s h)
  where go c = if cHalted c || cSteps c >= maxSteps then c else go (snd (tmStep m c))

tmTape :: Cfg -> Char -> Int -> Int -> String
tmTape cfg blank lo hi = [ tmRead cfg i blank | i <- [lo .. hi] ]

-- 空白でない部分だけの文字列
tmContent :: Cfg -> Char -> String
tmContent cfg blank = case (M.lookupMin (cTape cfg), M.lookupMax (cTape cfg)) of
  (Just (lo, _), Just (hi, _)) -> tmTape cfg blank lo hi
  _ -> ""

tmCount :: Cfg -> Char -> Int
tmCount cfg sym = M.size (M.filter (== sym) (cTape cfg))

-- 'A0:1RB A1:1LB B0:1LA B1:1RH' のような短い書き方から
tmFromCode :: String -> TM
tmFromCode code =
  let parts = words code
      rule p = case break (== ':') p of
        (k, ':' : (w : d : nq)) | not (null k) -> Just ((init k, last k), (w, d, nq))
        _ -> Nothing
      rs = [ r | Just r <- map rule parts ]
  in TM (nubOrd [ q | ((q, _), _) <- rs ]) "01" '0' (M.fromList rs) "A" "H"

-- テープのマスとヘッド（x, y = 左上、cw = マスの幅、ncell マス、center = 真ん中のマスの番号、cols = 記号の色）
drawTape :: Cfg -> Char -> Float -> Float -> Float -> Int -> Int -> (Char -> Maybe RGB) -> Picture
drawTape cfg blank x y cw ncell center cols =
  let lo = center - ncell `div` 2
      ts = min 22 (cw * 0.6)
  in Pictures
       [ Pictures [ Color (col (maybe panel id (cols v))) (rectTL cx y (cw - 2) (cw * 1.2))
                  , Color (col wire) (frameTL 1 cx y (cw - 2) (cw * 1.2))
                  , if v == blank then Blank else sayM [v] (cx + cw / 2 - 1) (y + cw * 0.6) (max 14 ts) (col ink)
                  , if i == cHead cfg
                      then Pictures [ Color (col amber) (Polygon [(cx + cw / 2 - 1, y - 4), (cx + cw / 2 - 10, y - 18), (cx + cw / 2 + 8, y - 18)])
                                    , Color (col amber) (frameTL 3 cx y (cw - 2) (cw * 1.2)) ]
                      else Blank ]
       | k <- [0 .. ncell - 1], let i = lo + k, let v = tmRead cfg i blank, let cx = x + i2f k * cw ]

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
