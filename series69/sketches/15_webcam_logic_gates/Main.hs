-- ============================================
-- 69-15 カメラで論理 📷 ─ 画面を 4 つに分け、それぞれの「動きがある」（または「明るい」）を変数 p, q, r, s にする
-- 左上 = p、右上 = q、左下 = r、右下 = s。入力した論理式の値がリアルタイムで変わり、真なら枠が光る。
-- 手で 4 つの領域をおおったり振ったりして、真理値表の 16 行を全部ためすゲーム。カメラが使えないときは領域をクリックで切りかえ
-- 操作：Tab 式を編集（編集中は文字キーが全部入力、Enter か Tab で終わる）/ ↑↓ お手本の式 / ← → しきい値 / M 動き ⇔ 明るさ
--       C カメラ ⇔ マウス / R ためした行を消す / S 画面を保存（SVG）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, testBit, xor, (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)

presets :: [String]                           -- ★ お手本の式
presets = ["(p & q) -> (r | s)", "p & q", "~(p <-> q)", "p | q | r | s", "~(p & q & r & s)", "(p & s) | (q & r)"]

camW, camH :: Int
camW = 64
camH = 48

data World = World
  { wSrc     :: String                        -- いまの式（文字列）
  , wF       :: F
  , wP       :: !Int                          -- お手本の番号
  , wEditing :: !Bool
  , wEditSrc :: String
  , wEditErr :: String
  , wThr     :: !Int                          -- ★ しきい値（動き：前のフレームとの差の平均、明るさ：0〜255 の平均を 8 で割った値）
  , wMotion  :: !Bool                         -- True = 動き、False = 明るさ
  , wCam     :: Maybe Camera
  , wUseCam  :: !Bool
  , wLive    :: !Bool                         -- 本物のカメラの映像が来ているか
  , wFrame   :: B.ByteString
  , wPrev    :: Maybe (U.Vector Float)        -- 前のフレームの明るさ
  , wLevels  :: [Float]                       -- 4 つの領域の量
  , wOn      :: [Bool]                        -- 4 つの領域の真偽（p q r s）
  , wVisited :: S.Set Int                     -- ためした真理値表の行
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "69-15 Logic with a Camera" world0)
  { skBack  = col bgc
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera camW camH            -- 左右反転した映像（p5.js の flipped: true）
      pure world0 { wCam = Just cam, wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World src0 (either (const FBot) id (parseFormula src0)) 0 False "" "" 18 True Nothing True False B.empty Nothing [0, 0, 0, 0] [False, False, False, False] S.empty []
  where src0 = headOr "p" presets

-- ★ 4 つの領域の量：動き = 前のフレームとの明るさの差の平均、明るさ = 明るさの平均 / 8
regionLevels :: U.Vector Float -> Maybe (U.Vector Float) -> Int -> Int -> Bool -> [Float]
regionLevels gray prev w h motion =
  let idx x y = (if i2f y < i2f h / 2 then 0 else 2) + (if i2f x < i2f w / 2 then 0 else 1) :: Int
      val i = if motion then (case prev of
                                Just p | U.length p == U.length gray -> abs (gray U.! i - p U.! i)
                                _ -> 0)
              else gray U.! i / 8
      add (s, c) (k, v) = (modifyAt k (+ v) s, modifyAt k (+ 1) c)
      (sums, cnts) = foldl' add ([0, 0, 0, 0], [0, 0, 0, 0 :: Int])
                       [ (idx x y, val i) | y <- [0 .. h - 1], x <- [0 .. w - 1], let i = y * w + x, i < U.length gray ]
  in zipWith (\s c -> s / i2f (max 1 c)) sums cnts

-- ちらつかないように：消えるのは 0.7 倍を下回ったとき
hysteresis :: Bool -> Float -> Float -> Bool
hysteresis on v t = if on then v > t * 0.7 else v > t

envNow :: [Bool] -> Asg
envNow st = M.fromList (zip ["p", "q", "r", "s"] st)

-- 真理値表の行（p が上の桁）
rowIndex :: [Bool] -> Int
rowIndex st = sum [ if b then v else 0 | (b, v) <- zip st [8, 4, 2, 1] ]

-- 式を読む：まちがいなら説明を返して前の式のまま。正しければ入れかえて、ためした行を消す
setFormula :: String -> World -> (String, World)
setFormula s w = case parseFormula s of
  Left (e, _) -> (e, w)
  Right f -> if any (`notElem` ["p", "q", "r", "s"]) (fmlVars f) then ("variables are p q r s only", w)
             else ("", w { wSrc = fmlStr f, wF = f, wVisited = S.empty })

-- 1 フレームの明るさ（(赤 + 緑 + 青) / 3）
grayOf :: B.ByteString -> U.Vector Float
grayOf f = U.generate (camW * camH) (\i -> (b (3 * i) + b (3 * i + 1) + b (3 * i + 2)) / 3)
  where b k = fromIntegral (BU.unsafeIndex f k)

step :: Env -> Float -> World -> IO World
step _ _ w0 = do
  (live, frame) <- case wCam w0 of
    Just cam | wUseCam w0 -> do
      ok <- camLive cam
      if ok then do f <- cameraFrame cam; pure (B.length f >= camW * camH * 3, f) else pure (False, B.empty)
    _ -> pure (False, B.empty)
  let w1 = if not live then w0 { wLive = False } else
             let g = grayOf frame
                 lv = regionLevels g (wPrev w0) camW camH (wMotion w0)
                 same = wMotion w0 && wPrev w0 /= Nothing && all (== 0) lv
                 levels = if same then wLevels w0 else zipWith (\o v -> o * 0.5 + v * 0.5) (wLevels w0) lv   -- 新しいコマが来たときだけ、なめらかに更新
             in w0 { wLive = True, wFrame = frame, wPrev = Just g, wLevels = levels
                   , wOn = zipWith (\on v -> hysteresis on v (i2f (wThr w0))) (wOn w0) levels }
  pure w1 { wVisited = S.insert (rowIndex (wOn w1)) (wVisited w1) }

videoBox :: Env -> (Float, Float, Float, Float)
videoBox env = let vw = min (envW env * 0.5) ((envH env - 230) * 4 / 3) in (30, 150, vw, vw * 0.75)

imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

draw :: Env -> World -> Picture
draw env w =
  let (vx, vy, vw, vh) = videoBox env
      val = evalFml (wF w) (envNow (wOn w))
      vc = if val then grn else pink
      region k v =
        let x = vx + i2f (k `mod` 2) * vw / 2
            y = vy + i2f (k `div` 2) * vh / 2
            on = at' (wOn w) k False
        in Pictures $
             [ Color (col (if on then yel else linec)) (frameTL (if on then 5 else 1.5) (x + 4) (y + 4) (vw / 2 - 8) (vh / 2 - 8))
             , say (v ++ " = " ++ (if on then "1" else "0")) (x + 16) (y + 40) 34 (col (if on then yel else ink)) ]
             ++ [ say ("level " ++ fmtN 1 (realToFrac (at' (wLevels w) k 0))) (x + 18) (y + 68) 14 (col dimc) | wLive w ]
      -- ---- 右：真理値表とためした行
      tx = vx + vw + 50
      ty = 150
      rh = min 30 ((envH env - ty - 120) / 17)
      cur = rowIndex (wOn w)
      ts = max 14 (min 15 (rh * 0.6))
      rowsT = [ (k, e, evalFml (wF w) e) | k <- [0 .. 15], let e = M.fromList (zip ["p", "q", "r", "s"] [testBit k 3, testBit k 2, testBit k 1, testBit k 0]) ]
      nTrue = length [ () | (_, _, True) <- rowsT ]
      nVis = S.size (wVisited w)
      table = [ sayM h (tx + i2f j * 44 + (if j > 4 then 20 else 0)) ty 14 (col sky) | (j, h) <- zip [0 :: Int ..] ["p", "q", "r", "s", "val", "tried"] ]
        ++ concat
          [ [ Color (col linec) (rectTL (tx - 20) (y - rh / 2) 300 (rh - 2)) | k == cur ]
            ++ [ sayM (if b then "1" else "0") (tx + i2f j * 44) y ts (col (if b then ink else dimc)) | (j, u) <- zip [0 :: Int ..] ["p", "q", "r", "s"], let b = M.findWithDefault False u e ]
            ++ [ sayM (if v then "1" else "0") (tx + 4 * 44) y ts (col (if v then grn else pink))
               , sayM (if S.member k (wVisited w) then "OK" else ".") (tx + 5 * 44 + 20) y ts (col (if S.member k (wVisited w) then yel else linec)) ]
          | (k, e, v) <- rowsT, let y = ty + i2f (k + 1) * rh ]
      formulaLine = if wEditing w then drawInputLine (envFrame env) 30 116 "Formula:" (wEditSrc w) (wEditErr w) True
                    else let s1 = "Formula: " ++ wSrc w ++ "  =  "
                         in Pictures [ say s1 30 116 22 (col ink), say (if val then "1 (true)" else "0 (false)") (40 + textW s1 22) 116 22 (col vc) ]
  in view env $
       [ hudTitle ("Logic with a camera: " ++ (if wLive w then (if wMotion w then "move your hands in the 4 regions" else "make the 4 regions bright / dark")
                                                else "click a region to toggle it (no camera)"))
           ("Tab edit formula   Up/Down examples   Left/Right threshold " ++ show (wThr w) ++ "   M motion <-> brightness (now: " ++ (if wMotion w then "motion" else "brightness") ++ ")"
            ++ "\nC camera <-> mouse   R reset tried rows   S save (SVG)")
       , case wCam w of
           Just cam | wLive w -> imageBox vx vy vw vh camW camH (camImage cam (wFrame w) 1)
           _ -> chalkPanel vx vy vw vh ]
       ++ zipWith region [0 ..] ["p", "q", "r", "s"]
       ++ [ Color (col vc) (frameTL (if val then 8 else 3) (vx - 6) (vy - 6) (vw + 12) (vh + 12))
          , formulaLine ]
       ++ table
       ++ [ say (if nVis == 16 then "All 16 rows tried! True rows: " ++ show nTrue else "Rows tried " ++ show nVis ++ " / 16")
                (tx - 20) (ty + 18 * rh + 10) 17 (col (if nVis == 16 then grn else ink))
          , drawSelfTest env (envW env - 180) (envH env - 14) (wTest w) ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos
    | wLive w || wEditing w -> pure w
    | otherwise ->
        let (vx, vy, vw, vh) = videoBox env
            (mx, my) = toP5 env pos
            k = (if my < vy + vh / 2 then 0 else 2) + (if mx < vx + vw / 2 then 0 else 1)
        in pure (if mx < vx || mx > vx + vw || my < vy || my > vy + vh then w else w { wOn = modifyAt k not (wOn w) })
  EventKey k Down mods _
    | wEditing w -> pure (editKey (typedKey k mods))     -- 編集モード：文字キーは全部入力
    | otherwise -> case typedKey k mods of
        TTab -> pure w { wEditing = True, wEditSrc = wSrc w, wEditErr = "" }
        TUp -> pure (preset (wP w - 1))
        TDown -> pure (preset (wP w + 1))
        TLeft -> pure w { wThr = max 2 (wThr w - 2) }
        TRight -> pure w { wThr = min 60 (wThr w + 2) }
        TChar c
          | isKey 'm' c -> pure w { wMotion = not (wMotion w), wPrev = Nothing }
          | isKey 'c' c -> pure w { wUseCam = not (wUseCam w), wPrev = Nothing }
          | isKey 'r' c -> pure w { wVisited = S.empty }
          | isKey 's' c -> do saveSvg "logic_camera" env (draw env w); pure w
        _ -> pure w
  _ -> pure w
  where
    preset i = let k = wrapI i (length presets) in snd (setFormula (at' presets k "p") w { wP = k })
    editKey tk = case tk of
      t | t == TEnter || t == TTab ->
        let (e, w1) = setFormula (wEditSrc w) w
        in if null e || t /= TEnter then w1 { wEditing = False, wEditErr = "" } else w1 { wEditErr = e }
      TBack -> reparse w { wEditSrc = dropLast (wEditSrc w) }
      TClear -> reparse w { wEditSrc = "" }
      TChar c -> reparse w { wEditSrc = typeInto (wEditSrc w) c fmlKeys 60 }
      _ -> reparse w
    reparse x = x { wEditErr = either fst (const "") (parseFormula (wEditSrc x)) }

-- ---------- やってみよう ----------
-- 1. お手本 1 (p & q) -> (r | s) を偽にするには、どの領域で手を動かせばいい？ 真理値表の 1 行だけ
-- 2. 16 行全部をためそう（両手と頭を使う！）。カメラがないときはクリックで
-- 3. M で「明るさ」にして、手で領域をおおう（暗くする）と 0 になる式を作ろう
-- 4. ~(p <-> q)（どちらか一方だけ）は排他的論理和 XOR。左上と右上で手を動かしてためそう
-- 5. 「4 つのうち 3 つ以上」で真になる式（多数決）を作ろう。00 の真理値表でも確かめよう

-- ---- 自己テスト（p5.js 版と同じ項目。名前も同じ）
selfTest :: [(String, Bool)]
selfTest =
  [ ("お手本の式 " ++ show (length presets) ++ " 個が読めて、変数は p q r s だけ"
    , all (\s -> either (const False) (all (`elem` ["p", "q", "r", "s"]) . fmlVars) (parseFormula s)) presets)
  , ("明るさ：右上だけ明るい画像 → q だけが 1（しきい値 18）", map (> 18) lv == [False, True, False, False])
  , ("動き：左下だけ変わった 2 枚 → r だけが 1、前のフレームがなければ全部 0"
    , map (> 18) lm == [False, False, True, False] && all (== 0) (regionLevels moved Nothing w h True))
  , ("ちらつき防止：18 をこえたら 1、12.6 を下回るまで 1 のまま（0,20,15,13,11,25,5 → 0,1,1,1,0,1,0）", hyst == "0111010")
  , ("(p ∧ q) → (r ∨ s) が偽になるのは 16 行中 1 行だけ（p = q = 1, r = s = 0 の 12 行目）、行の番号の付け方も一致", rowsFalse == ["12"])
  , ("変数 x を含む式はまちがいとして説明し、前の式のまま", not (null e) && wSrc w' == wSrc world0) ]
  where
    w = 8
    h = 6
    dark = U.replicate (w * h) 0
    bright = U.generate (w * h) (\i -> let (y, x) = i `divMod` w in if x >= 4 && y < 3 then 240 else 20)
    lv = regionLevels bright Nothing w h False
    moved = U.generate (w * h) (\i -> let (y, x) = i `divMod` w in if y >= 3 && x < 4 then 120 else 0)
    lm = regionLevels moved (Just dark) w h True
    hyst = concatMap (\b -> if b then "1" else "0") (drop 1 (scanl (\on v -> hysteresis on v 18) False [0, 20, 15, 13, 11, 25, 5]))
    f0 = either (const FBot) id (parseFormula (headOr "" presets))
    rowsFalse = concat [ [ "idx" | rowIndex st /= k ] ++ [ show k | not (evalFml f0 (envNow st)) ]
                       | k <- [0 .. 15 :: Int], let st = [testBit k 3, testBit k 2, testBit k 1, testBit k 0] ]
    (e, w') = setFormula "p & x" world0

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
