-- ============================================
-- 66-15 身ぶりをオートマトンで認識 📷 ─ カメラの「動いた所」（前のフレームとの差）の重心がどちらへ動いたかを
-- 左 L・右 R・上 U・下 D の記号に変え、その記号の列を DFA で読む。身ぶりは正規表現で書き、
-- (L|R|U|D)*(身ぶり) を トンプソン構成 → 部分集合構成 → 最小化 して「いつ終わってもよい」認識の DFA にする（04 → 02 → 03）。
-- 受理状態に着いたら認識！ その DFA は最初にもどる。カメラが使えないときはマウスを大きく動かす
-- 操作：1 2 3 図に出す身ぶり / ↑↓ 感度 / C カメラ ⇔ マウス / X 記号の列を消す / S 画面を保存（SVG）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (cyan, rose, violet)
import qualified Data.ByteString as B
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', isSuffixOf, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- ★ (名前, 正規表現（記号 L R U D）)
gestures :: [(String, String)]
gestures =
  [ ("Wave (left-right-left-right)", "LRLR|RLRL")
  , ("Nod (up-down-up-down)", "UDUD|DUDU")
  , ("Circle (clockwise)", "RDLU|DLUR|LURD|URDL") ]

shortName :: Int -> String                    -- 認識したときに出す名前
shortName k = takeWhile (/= ' ') (fst (at' gestures k ("", "")))

camW, camH :: Int
camW = 64                                     -- ★ 動きを調べる映像の大きさ
camH = 48

-- 認識機：NFA → 部分集合構成の DFA → 最小 DFA、いまの状態と認識した回数
data Rec = Rec { rNfa :: NFA, rBig :: DFA, rDfa :: DFA, rQ :: !Int, rCount :: !Int }

data World = World
  { wRecs    :: [Rec]
  , wShowG   :: !Int
  , wSens    :: !Double                       -- ★ 感度（大きいほど小さな動きでも記号が出る）
  , wCam     :: Maybe Camera
  , wUseCam  :: !Bool
  , wLive    :: !Bool                         -- 本物のカメラ映像を使っているか
  , wFrame   :: B.ByteString                  -- いまの映像
  , wPrev    :: Maybe (U.Vector Double)       -- 前のフレームの明るさ
  , wCen     :: Maybe (Double, Double)        -- 動いた所の重心
  , wTrack   :: [(Double, Double)]
  , wAcc     :: (Double, Double)              -- たまった移動量
  , wCool    :: !Int
  , wIdle    :: !Int
  , wLastSym :: Maybe Char
  , wSince   :: !Int
  , wStream  :: String                        -- 記号の列
  , wLastMouse :: Maybe (Float, Float)
  , wFlashG  :: !Int
  , wFlashT  :: !Int
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "66-15 Gesture DFA" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera camW camH            -- 左右反転した映像（p5.js の flipped: true）
      pure world0 { wCam = Just cam, wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = World recs0 0 1.0 Nothing True False B.empty Nothing Nothing [] (0, 0) 0 0 Nothing 0 "" Nothing (-1) 0 []

recs0 :: [Rec]
recs0 = map (buildRecognizer . snd) gestures

-- ★ 身ぶりの認識 DFA：(L|R|U|D)*(身ぶり) → NFA → DFA → 最小 DFA
buildRecognizer :: String -> Rec
buildRecognizer re =
  let nfa = case parseRegex ("(L|R|U|D)*(" ++ re ++ ")") of
        Right ast -> (thompson ast) { nAlpha = "DLRU" }
        Left _ -> NFA 1 "DLRU" [M.empty] 0 [False]
      big = sDfa (subsetConstruct nfa 4096)
      mn = mMin (minimizeMoore big)
  in Rec nfa big mn (dStart mn) 0

-- 記号を 1 つ全部の DFA に読ませる
feed :: Char -> World -> World
feed sym w =
  let one (k, r) (fg, ft, acc) =
        let q = dfaNext (rDfa r) (rQ r) sym
            hit = q >= 0 && at' (dAcc (rDfa r)) q False             -- 認識！ 最初にもどる
            r' = r { rQ = if hit || q < 0 then dStart (rDfa r) else q, rCount = rCount r + (if hit then 1 else 0) }
        in (if hit then k else fg, if hit then 60 else ft, acc ++ [r'])
      (g, t, rs) = foldl' (flip one) (wFlashG w, wFlashT w, []) (zip [0 ..] (wRecs w))
      st = wStream w ++ [sym]
  in w { wStream = drop (length st - 28) st, wRecs = rs, wFlashG = g, wFlashT = t }

-- ★ 動きの向き → 記号：たまった移動量が しきい値 をこえ、縦横の片方が 1.5 倍以上大きいとき
symbolFrom :: (Double, Double) -> Double -> Maybe Char
symbolFrom (ax, ay) thresh
  | abs ax > thresh && abs ax > 1.5 * abs ay = Just (if ax > 0 then 'R' else 'L')
  | abs ay > thresh && abs ay > 1.5 * abs ax = Just (if ay > 0 then 'D' else 'U')
  | otherwise = Nothing

-- フレーム差分の重心（0〜1 の座標）。動きが少なければ Nothing
motionCentroid :: B.ByteString -> Maybe (U.Vector Double) -> (Maybe (Double, Double), U.Vector Double)
motionCentroid f prev =
  let n = camW * camH
      g = U.generate n (\i -> (fromIntegral (B.index f (3 * i)) + fromIntegral (B.index f (3 * i + 1)) + fromIntegral (B.index f (3 * i + 2))) / 3)
      (sx, sy, sw) = case prev of
        Just p | U.length p == n ->
          U.ifoldl' (\(!a, !b, !c) i v -> let d = abs (v - p U.! i) in
                                            if d > 30 then (a + i2d (i `mod` camW) * d, b + i2d (i `div` camW) * d, c + d) else (a, b, c)) (0, 0, 0) g
        _ -> (0, 0, 0)
  in (if sw > 2000 then Just (sx / sw / i2d camW, sy / sw / i2d camH) else Nothing, g)

videoBox :: Env -> (Float, Float, Float, Float)
videoBox env = let vw = envW env * 0.42 in (30, 90, vw, vw * 0.75)

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  (live, frame) <- case wCam w0 of
    Just cam | wUseCam w0 -> do
      ok <- camLive cam
      if ok then do f <- cameraFrame cam; pure (B.length f >= camW * camH * 3, f) else pure (False, B.empty)
    _ -> pure (False, B.empty)
  let (vx, vy, vw, vh) = videoBox env
      (mx, my) = mouseP env
      -- ---- 動き → 記号
      (p, prev) =
        if live then let (c, g) = motionCentroid frame (wPrev w0) in (fmap (\(cx, cy) -> (cx * realToFrac vw, cy * realToFrac vh)) c, Just g)
        else case wLastMouse w0 of
          Just (lx, ly) | hyp (mx - lx) (my - ly) > 2 -> (Just (clampv (realToFrac (mx - vx)) 0 (realToFrac vw), clampv (realToFrac (my - vy)) 0 (realToFrac vh)), wPrev w0)
          _ -> (Nothing, wPrev w0)
      w1 = w0 { wLive = live, wFrame = frame, wPrev = prev, wLastMouse = Just (mx, my) }
      w2 = case p of
        Just pt@(px, py) ->
          let (ax, ay) = wAcc w1
              acc' = case wCen w1 of
                Just (cx, cy) -> (ax + px - cx, ay + py - cy)
                Nothing -> (ax, ay)
              tr = wTrack w1 ++ [pt]
          in w1 { wAcc = acc', wCen = Just pt, wIdle = 0, wTrack = if length tr > 30 then drop 1 tr else tr }
        Nothing ->                            -- 動きが止まったら：少し待ってから移動量を減らし、0.5 秒で重心を忘れる
          let idle = wIdle w1 + 1
              (ax, ay) = wAcc w1
              w' = if idle > 8 then w1 { wAcc = (ax * 0.85, ay * 0.85), wTrack = drop 1 (wTrack w1) } else w1
          in w' { wIdle = idle, wCen = if idle > 30 then Nothing else wCen w' }
      w3 = w2 { wCool = if wCool w2 > 0 then wCool w2 - 1 else 0 }
      sym = if wCool w3 == 0 then symbolFrom (wAcc w3) (realToFrac (vw * 0.18) / wSens w3) else Nothing
      w4 = w3 { wSince = wSince w3 + 1 }
      -- 同じ向きへの 1 回の大きな動きは 1 記号だけ（向きが変わるか、少し止まるまで同じ記号は出さない）
      w5 = case sym of
        Just s -> let wf = if not (Just s == wLastSym w4 && wSince w4 < 40) then (feed s w4) { wLastSym = Just s } else w4
                  in wf { wSince = 0, wAcc = (0, 0), wCool = 10 }
        Nothing -> w4
  pure w5 { wFlashT = if wFlashT w5 > 0 then wFlashT w5 - 1 else 0 }

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（iw × ih の画像）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

arrowName :: Char -> String                   -- 画面の矢印（ASCII）
arrowName c = case c of
  'L' -> "<"
  'R' -> ">"
  'U' -> "^"
  _ -> "v"

draw :: Env -> World -> Picture
draw env w = view env
  [ hudTitle ("Gesture recognition with automata: " ++ (if wLive w then "move your hand in front of the camera" else "move the mouse in big strokes (no camera)"))
             ("1 2 3 gesture to show   Up/Down sensitivity " ++ fmtN 1 (wSens w) ++ "   C camera <-> mouse   X clear the symbols   S save (SVG)")
  -- ---- 映像
  , case wCam w of
      Just cam | wLive w -> imageBox vx vy vw vh camW camH (camImage cam (wFrame w) 1)
      _ -> Pictures [ Color (col panel) (rectTL vx vy vw vh), sayM "Move the mouse a lot inside this box" (vx + vw / 2) (vy + vh / 2) 14 (col dimc) ]
  , Color (col amber) (polyline 3 [ (vx + f2 x, vy + f2 y) | (x, y) <- wTrack w ])
  , case wCen w of
      Just (cx, cy) -> Color (col amber) (circleAt (vx + f2 cx) (vy + f2 cy) 14)
      Nothing -> Blank
  -- ---- 記号の列
  , say "Symbols (newest on the right)" vx sy0 15 (col ink)
  , Pictures [ Pictures [ Color (col panel) (rectTL (vx + i2f i * 26) (sy0 + 10) 23 30)
                        , sayM (arrowName c) (vx + i2f i * 26 + 11) (sy0 + 25) 16 (col (if i == length (wStream w) - 1 then amber else ink)) ]
             | (i, c) <- zip [0 ..] (wStream w) ]
  , Pictures [ say (show (k + 1) ++ ". " ++ nm ++ "   " ++ joinWith " | " (splitBar re) ++ "   seen " ++ show (rCount r) ++ "x")
                   vx (sy0 + 76 + i2f k * 26) 15 (col (if wFlashG w == k && wFlashT w > 0 then lime else if k == wShowG w then ink else dimc))
             | (k, (r, (nm, re))) <- zip [0 :: Int ..] (zip (wRecs w) gestures) ]
  , if wFlashT w > 0 then sayM (shortName (wFlashG w) ++ "!") (vx + vw / 2) (vy + 40) 34 (colA (255 * i2f (wFlashT w) / 60) lime) else Blank
  -- ---- 選んだ身ぶりの DFA
  , say ("Recognizer DFA for \"" ++ fst (at' gestures (wShowG w) ("", "")) ++ "\"") rx 100 15 (col ink)
  , say ("NFA " ++ show (nN (rNfa r)) ++ " states -> subset construction " ++ show (dN (rBig r)) ++ " -> minimized " ++ show (dN (rDfa r)) ++ " states.") rx 122 14 (col dimc)
  , say "Arrows = symbols (D L R U)" rx 142 14 (col dimc)
  , drawDfa defOpt { oR = 22, oHi = IM.singleton (rQ r) amber, oLabelSize = 14, oNameSize = Just 14 } (rDfa r)
            (circleLayout (dN (rDfa r)) (envW env * 0.72) (envH env * 0.5) (min (envW env * 0.14) (envH env * 0.22)) pi)
  , drawSelfTest env (wTest w) ]
  where
    (vx, vy, vw, vh) = videoBox env
    sy0 = vy + vh + 30
    rx = envW env * 0.52
    r = at' (wRecs w) (wShowG w) (headOr (buildRecognizer "L") (wRecs w))

splitBar :: String -> [String]                -- "LRLR|RLRL" → ["LRLR", "RLRL"]
splitBar s = case break (== '|') s of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitBar rest

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> pure w { wShowG = fromEnum c - fromEnum '1' }
    | isKey 'c' c -> pure w { wUseCam = not (wUseCam w), wPrev = Nothing, wCen = Nothing, wTrack = [] }
    | isKey 'x' c -> pure w { wStream = "", wLastSym = Nothing, wRecs = [ r { rQ = dStart (rDfa r) } | r <- wRecs w ] }
    | isKey 's' c -> do saveSvg "gesture_dfa" env (draw env w); pure w
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSens = min 3 (wSens w + 0.2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSens = max 0.4 (wSens w - 0.2) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. カメラの前で手を左右に 2 往復。記号の列に < > < > が並んで「Wave!」になるか確かめよう（↑↓ で感度）
-- 2. gestures に新しい身ぶりを足そう：「LLL」（左へ 3 回スワイプ）、「(LR)+U」（振ってから上げる）など
-- 3. 右の DFA の状態の数は、身ぶりの長さとどう関係する？ 03 の最小化がなければ何状態？
-- 4. 円を左回りでも認識させるには？ 正規表現に | で足すだけ（DFA は自動で作り直される）
-- 5. ゲームのコマンド入力（↓↘→ + パンチ）や、スマホのジェスチャーも同じしくみで作れる

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("3 つの身ぶり：NFA = 最小 DFA = 「その身ぶりで終わる」（記号 4 種、長さ 6 までの全部 5461 個）", ok)
  , ("最小 DFA は部分集合構成の DFA と同じ言語（積の BFS）で、状態は同じか少ない（" ++ joinWith "、" [ show (dN (rBig r)) ++ "→" ++ show (dN (rDfa r)) | r <- recs0 ] ++ "）", okMin)
  , ("記号の列 LRLR UDUD LRUD を読むと：手を振る 1 回・うなずく 1 回（LRUD では何も認識しない）", map rCount (wRecs fed) == [1, 1, 0])
  , ("動き → 記号：右へ 40 → R、上へ 40 → U、ななめ (30, 25) と小さな動きは記号なし"
    , symbolFrom (40, 3) 20 == Just 'R' && symbolFrom (-2, -40) 20 == Just 'U' && symbolFrom (30, 25) 20 == Nothing && symbolFrom (5, 0) 20 == Nothing) ]
  where
    ws = allStrings "DLRU" 6
    -- くらべる相手：「その身ぶりのどれかで終わる」（p5.js 版の RegExp('(…)$') と同じ意味）
    endsWithGesture re s = any (`isSuffixOf` s) (splitBar re)
    ok = and [ nfaAccepts (rNfa r) s == v && dfaAccepts (rDfa r) s == v | (r, (_, re)) <- zip recs0 gestures, s <- ws, let v = endsWithGesture re s ]
    okMin = all (\r -> dfaEquivalent (rBig r) (rDfa r) == Nothing && dN (rDfa r) <= dN (rBig r)) recs0
    fed = foldl' (flip feed) world0 "LRLRUDUDLRUD"

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

-- ---- 正規表現の道具：再帰下降の構文解析 → 構文木 → トンプソン構成の NFA
-- 使える記号：文字（英字・数字）、| （または）、つなげる、* （0 回以上）、+ （1 回以上）、? （0 か 1 回）、( )
data Rx = RChr Char | REps | RCat Rx Rx | RAlt Rx Rx | RStar Rx | RPlus Rx | ROpt Rx deriving (Eq, Show)

-- 返り値 Right 構文木 または Left (まちがいの説明, 位置)（例外は投げない）
parseRegex :: String -> Either (String, Int) Rx
parseRegex src = case alt 0 of
    Left e -> Left e
    Right (a, i) -> if i < n then Left ("missing (", i) else Right a
  where
    n = length src
    peek i = if i < n then Just (at' src i ' ') else Nothing
    isChr c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
    isOp c = c == '*' || c == '+' || c == '?'
    alt i = do
      (a, j) <- cat i
      altMore a j
    altMore a j = if peek j == Just '|'
      then do (b, k) <- cat (j + 1); altMore (RAlt a b) k
      else Right (a, j)
    cat i = go Nothing i
      where go acc j = case peek j of
              Just c | c /= '|' && c /= ')' -> do
                (r, k) <- rep j
                go (Just (maybe r (\a -> RCat a r) acc)) k
              _ -> Right (maybe REps id acc, j)
    rep i = do
      (a, j) <- atom i
      let (a', k) = case peek j of
            Just '*' -> (RStar a, j + 1)
            Just '+' -> (RPlus a, j + 1)
            Just '?' -> (ROpt a, j + 1)
            _ -> (a, j)
      case peek k of
        Just c | isOp c -> Left ("two operators in a row (use parentheses)", k)
        _ -> Right (a', k)
    atom i = case peek i of
      Just '(' -> do
        (a, j) <- alt (i + 1)
        if peek j /= Just ')' then Left ("missing )", j) else Right (a, j + 1)
      Just c | isChr c -> Right (RChr c, i + 1)
      Nothing -> Left ("the expression ends too early", i)
      Just c -> Left ("'" ++ [c] ++ "' cannot be here", i)

-- 構文木 → 文字列（かっこを補って）。ε は "e" と書く
astString :: Rx -> String
astString a = case a of
  RChr c -> [c]
  REps -> "e"
  RCat x y -> astString x ++ astString y
  RAlt x y -> "(" ++ astString x ++ "|" ++ astString y ++ ")"
  RStar x -> "(" ++ astString x ++ ")*"
  RPlus x -> "(" ++ astString x ++ ")+"
  ROpt x -> "(" ++ astString x ++ ")?"

-- トンプソン構成：部品ごとに「入口 1 つ・出口 1 つ」の NFA を作ってつなぐ（状態の番号は p5.js 版と同じ順）
data TB = TB { tbN :: !Int, tbTr :: IM.IntMap (M.Map Sym [Int]), tbAlpha :: S.Set Sym }

thompson :: Rx -> NFA
thompson ast =
  let ns tb = (tbN tb, tb { tbN = tbN tb + 1 })
      add p c q tb = tb { tbTr = IM.insertWith (\_ old -> M.insertWith (\_ o -> o ++ [q]) c [q] old) p (M.singleton c [q]) (tbTr tb) }
      build a tb0 =
        let (s, tb1) = ns tb0 in
        case a of
          RChr c -> let (f, tb2) = ns tb1 in ((s, f), add s c f tb2 { tbAlpha = S.insert c (tbAlpha tb2) })
          REps -> let (f, tb2) = ns tb1 in ((s, f), add s epsSym f tb2)
          RCat x y -> let ((x0, x1), tb2) = build x tb1
                          ((y0, y1), tb3) = build y tb2
                          tb4 = add x1 epsSym y0 (add s epsSym x0 tb3)
                          (f, tb5) = ns tb4
                      in ((s, f), add y1 epsSym f tb5)
          RAlt x y -> let ((x0, x1), tb2) = build x tb1
                          ((y0, y1), tb3) = build y tb2
                          (f, tb4) = ns tb3
                      in ((s, f), add y1 epsSym f (add x1 epsSym f (add s epsSym y0 (add s epsSym x0 tb4))))
          _ -> let x = case a of { RStar u -> u; RPlus u -> u; ROpt u -> u; _ -> a }
                   ((x0, x1), tb2) = build x tb1
                   (f, tb3) = ns tb2
                   tb4 = add x1 epsSym f (add s epsSym x0 tb3)
                   tb5 = case a of { RPlus _ -> tb4; _ -> add s epsSym f tb4 }       -- * と ? は 0 回でも OK
                   tb6 = case a of { ROpt _ -> tb5; _ -> add x1 epsSym x0 tb5 }      -- * と + はもう一度
               in ((s, f), tb6)
      ((s0, f0), tbE) = build ast (TB 0 IM.empty S.empty)
      n = tbN tbE
  in NFA n (S.toAscList (tbAlpha tbE)) [ IM.findWithDefault M.empty q (tbTr tbE) | q <- [0 .. n - 1] ] s0 [ q == f0 | q <- [0 .. n - 1] ]

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
