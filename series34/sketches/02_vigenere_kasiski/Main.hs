-- ============================================
-- 02 ヴィジュネル暗号とカシスキー法
-- 鍵の単語をくり返し並べ、1 文字ごとに違う数だけずらす暗号。頻度がならされて 01 の方法が効かない。
-- でも鍵の長さ L が分かれば「L 個おきの文字」はただのシーザー暗号 → 00 の方法で解ける
--   カシスキー法：同じ 3 文字のくり返しの間隔は L の倍数になりやすい
--   一致指数（IC）：L 個おきに取り出した文字の偏り。正しい L で英語の値（約 0.066）に近づく
-- 文字を打つ = 鍵の単語を入力 / Backspace = 1 文字消す / 1 = 鍵を例に戻す
-- ※ 教育用。実際の安全な暗号には使わない
-- ============================================
module Main (main) where

import Art
import Data.Char (chr, isAsciiLower, isAsciiUpper, ord, toUpper)
import Data.List (foldl', isPrefixOf, minimumBy)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as U

plainText :: String
plainText = filter isAsciiUpper
  ( "THE TRAIN LEFT THE STATION AT DAWN AND THE CITY WAS STILL ASLEEP. "
  ++ "THE DRIVER KNEW THE ROUTE BY HEART AND THE PASSENGERS KNEW THE DRIVER. "
  ++ "AT THE THIRD STOP A WOMAN WITH A RED UMBRELLA STEPPED ONTO THE PLATFORM AND "
  ++ "LOOKED AT THE CLOCK. SHE WAS WAITING FOR A LETTER THAT WOULD CHANGE EVERYTHING, "
  ++ "AND THE LETTER WAS IN THE POCKET OF THE MAN SITTING NEXT TO THE WINDOW. "
  ++ "NEITHER OF THEM KNEW THAT THE OTHER WAS THERE, AND THE TRAIN MOVED ON "
  ++ "THROUGH THE FIELDS AND THE RIVERS UNTIL THE SUN WAS HIGH ABOVE THE HILLS." )

enFreq :: [Float]
enFreq = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.15, 0.77, 4.0, 2.4,
          6.7, 7.5, 1.9, 0.095, 6.0, 6.3, 9.1, 2.8, 0.98, 2.4, 0.15, 2.0, 0.074]

startKey :: String
startKey = "LEMON"      -- ★ 鍵の単語（A〜Z、最大 10 文字）

maxL :: Int
maxL = 14               -- ★ 調べる鍵の長さの上限

data World = World
  { keyword    :: !String
  , cipherText :: !String
  , ic         :: ![Float]          -- L = 1..maxL の一致指数
  , kasiski    :: ![Int]            -- L = 1..maxL が間隔を割り切った回数
  , repeats    :: ![(Int, Int)]     -- くり返しの 3 文字の位置（前, 後）
  , guessL     :: !Int
  , recovered  :: !String
  }

main :: IO ()
main = runSketch (sketch "34-02 Vigenere Cipher and Kasiski" (analyze startKey))
  { skBack  = rgb255 14 18 28
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- ★ ヴィジュネル暗号：i 文字目を 鍵[i % 鍵の長さ] だけずらす
vigenere :: String -> String -> String
vigenere s kw = zipWith (\ch k -> shiftBy (ord k - 65) ch) s (cycle kw)

vigenereBack :: String -> String -> String
vigenereBack s kw = zipWith (\ch k -> shiftBy (negate (ord k - 65)) ch) s (cycle kw)

shiftBy :: Int -> Char -> Char
shiftBy k ch = chr (65 + (ord ch - 65 + k) `mod` 26)

analyze :: String -> World
analyze kw = World kw ctext ics kas (reverse revRepeats) gl rec'
  where
    ctext = if null kw then plainText else vigenere plainText kw
    -- 一致指数：L 個おきの列ごとに「同じ文字を 2 つ選ぶ確率」を出して平均
    ics = [ sum [indexOfCoincidence (column ctext l c) | c <- [0 .. l - 1]] / fromIntegral l | l <- [1 .. maxL] ]
    -- カシスキー法：同じ 3 文字の並びが出てくる間隔を集めて、2〜maxL の約数を数える
    (_, revRepeats) = foldl' scanTri (M.empty, []) (zip [0 ..] (triples ctext))
    scanTri (seen, rs) (i, tri) =
      let rs' = case M.lookup tri seen of
                  Just j -> (j, i) : rs
                  Nothing -> rs
      in (M.insert tri i seen, rs')
    kas = [ if l == 1 then 0 else length [() | (a, b) <- revRepeats, (b - a) `mod` l == 0] | l <- [1 .. maxL] ]
    -- IC が英語（0.066）に近づく最小の L を鍵の長さと推測
    gl = case [l | (l, v) <- zip [1 ..] ics, v > 0.058] of
           (l : _) -> l
           [] -> 1
    -- 列ごとにシーザーの総当たり（00 と同じカイ二乗）で鍵の文字を復元
    rec' = [chr (65 + bestShift (column ctext gl c)) | c <- [0 .. gl - 1]]

triples :: String -> [String]
triples s = case s of
  (a : b : c : _) -> [a, b, c] : triples (drop 1 s)
  _ -> []

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (header ++ cipherLetters ++ graphs ++ results)
  where
    sw = envW env
    sh = envH env
    cursor = if envFrame env `mod` 60 < 30 then "_" else " "
    header =
      [ txt env 24 34 18 (gray 0.94) ("Vigenere cipher   key = " ++ keyword w ++ cursor ++ "  (type letters, Backspace, 1 example)")
      , txt env 24 56 14 (gray 0.6) "Educational: once L is known, every L-th letter is just a Caesar cipher"
      ]

    -- ---- 暗号文（くり返しの 3 文字を色づけ）
    ctext = cipherText w
    pitch = 12 :: Float
    lineH = 20 :: Float
    perLine = max 1 (floor ((sw - 48) / pitch)) :: Int
    nChars = length ctext
    marks = U.replicate nChars False U.//
      [ (p, True) | (a, b) <- take 40 (repeats w), j <- [0, 1, 2], p <- [a + j, b + j], p < nChars ]
    cipherLetters =
      [ txt env (24 + fromIntegral c * pitch) (86 + fromIntegral l * lineH) 14
          (if marks U.! i then rgb255 255 200 90 else rgb255 130 150 190) [ch]
      | (i, ch) <- zip [0 ..] ctext
      , let (l, c) = i `divMod` perLine ]
    nLines = (nChars + perLine - 1) `div` perLine
    gy = 86 + fromIntegral nLines * lineH + 20

    -- ---- グラフ 2 つ
    gh = sh - gy - 124
    half = (sw - 72) / 2
    graphs =
      drawBars env (guessL w) 24 gy half gh (ic w) 0.08 "Index of coincidence (per key length L)" True
      ++ drawBars env (guessL w) (48 + half) gy half gh (map fromIntegral (kasiski w))
           (fromIntegral (max 1 (maximum (0 : kasiski w)))) "Kasiski: how often L divides a repeat gap" False

    -- ---- 結果
    kw = keyword w
    rc = recovered w
    ok = rc == kw
      || (not (null kw) && length rc `mod` length kw == 0 && kw `isPrefixOf` rc && rc == take (length rc) (cycle kw))
    results =
      [ txt env 24 (sh - 64) 18 (gray 0.94) ("Guessed length L = " ++ show (guessL w) ++ "  -> key solved column by column: " ++ rc)
      , if ok
          then txt env 24 (sh - 34) 16 (rgb255 120 230 150) ("Key broken! Plain: " ++ take 60 (vigenereBack ctext rc) ++ "...")
          else txt env 24 (sh - 34) 16 (rgb255 255 150 120) "Not yet (a long key or short text makes it hard)"
      ]

drawBars :: Env -> Int -> Float -> Float -> Float -> Float -> [Float] -> Float -> String -> Bool -> [Picture]
drawBars env gl x y w h vals top title withRefs =
  txt env x y 14 (gray 0.78) title : concat (zipWith bar [1 ..] vals) ++ refLines   -- 目安の線は棒の上に描く
  where
    bw = w / fromIntegral (max 1 (length vals))
    refLines = if withRefs then ref 0.066 "English 0.066" (rgb255 120 230 150) ++ ref 0.038 "Random 0.038" (rgb255 255 120 120) else []
    ref v s c =
      let ry = y + 10 + h - v / top * h
          (a, b) = toS env (x, ry)
          (d, e) = toS env (x + w, ry)
      in [ Color (withA 0.63 c) (thickLine 1 (a, b) (d, e)), txt env (x + w - 150) (ry - 5) 14 c s ]
    bar :: Int -> Float -> [Picture]
    bar l v =
      let bh = clampF 0 1 (v / top) * h
          bx = x + fromIntegral (l - 1) * bw
      in [ box env (bx + 2) (y + 10 + h - bh) (bw - 4) bh (if l == gl then rgb255 255 200 90 else rgb255 90 130 200)
         , txtC env (bx + bw / 2) (y + h + 30) 14 (gray 0.78) (show l)
         ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyBackspace) Down _ _ -> backspace
  EventKey (SpecialKey KeyDelete) Down _ _    -> backspace   -- Mac の delete キー
  EventKey (Char '\b') Down _ _   -> backspace
  EventKey (Char '\DEL') Down _ _ -> backspace
  EventKey (Char '1') Down _ _    -> analyze startKey
  EventKey (Char c) Down _ _
    | (isAsciiUpper c || isAsciiLower c) && length kw < 10 -> analyze (kw ++ [toUpper c])
  _ -> w
  where
    kw = keyword w
    backspace = analyze (take (length kw - 1) kw)

-- ---------- やってみよう ----------
-- 1. 鍵を A 1 文字にすると？（ずらさない）。B 1 文字なら 00 のシーザー暗号と同じ
-- 2. 鍵を長くしていくと、何文字から破れなくなる？ plainText を長くすると？
-- 3. IC のグラフで、正しい L の倍数（2L, 3L）も高くなるのはなぜ？
-- 4. 鍵を plainText と同じ長さのランダムな文字にすると…それが 03 のワンタイムパッド

-- ==================================================
-- ここから下は「道具」（列の取り出し・一致指数・シーザーの総当たり）
-- ==================================================
column :: String -> Int -> Int -> String
column s l c = go (drop c s)
  where go [] = []
        go (x : r) = x : go (drop (l - 1) r)

counts :: String -> U.Vector Int
counts s = U.accum (+) (U.replicate 26 0) [(ord ch - 65, 1) | ch <- s]

indexOfCoincidence :: String -> Float
indexOfCoincidence s
  | n < 2 = 0
  | otherwise = fromIntegral (U.sum (U.map (\v -> v * (v - 1)) (counts s))) / fromIntegral (n * (n - 1))
  where n = length s

bestShift :: String -> Int
bestShift s = fst (minimumBy (comparing snd) [(k, chiFor k) | k <- [0 .. 25]])
  where
    n = fromIntegral (length s) :: Float
    f = counts s
    chiFor k = sum (zipWith (\i fr ->
      let e = n * fr / 100
          d = fromIntegral (f U.! ((i + k) `mod` 26)) - e
      in d * d / e) [0 .. 25] enFreq)

-- p5.js と同じ「左上が原点・y は下向き」の座標を gloss の座標に直す
toS :: Env -> Point -> Point
toS e (x, y) = (x - envW e / 2, envH e / 2 - y)

-- 文字（(x, y) は左端の下）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt e x y s c str = let (a, b) = toS e (x, y) in label a b s c str

-- 中央揃えの文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC e x y s c str = txt e (x - fromIntegral (length str) * s * 0.36) y s c str

-- 左上が (x, y) の長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box e x y bw bh c = let (a, b) = toS e (x + bw / 2, y + bh / 2) in Color c (rect a b bw bh)
