-- ============================================
-- 00 シーザー暗号と総当たり
-- 文字を k 個ずらすだけの暗号。鍵は 26 通りしかないので、全部試せば必ず読める（総当たり攻撃）
-- 文字を打つ = 平文を入力 / Backspace = 1 文字消す / ←→ = ずらす数 k / 1 = 例文 / 2 = 全部消す
-- ※ 教育用。実際の安全な暗号には使わない
-- ============================================
module Main (main) where

import Art
import Data.Char (chr, isAlpha, isAscii, ord, toUpper)
import qualified Data.Vector.Unboxed as U

examplePlain :: String
examplePlain = "MEET ME AT THE OLD LIBRARY AFTER CLASS"  -- 平文（大文字に直して使う）

startShift :: Int
startShift = 3                -- ★ 鍵 = ずらす数（0〜25）

maxLen :: Int
maxLen = 60                   -- ★ 入力できる最大の文字数

-- 英語の文字の出現頻度（%）A〜Z。「英語らしさ」の点数に使う
enFreq :: [Float]
enFreq = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.15, 0.77, 4.0, 2.4,
          6.7, 7.5, 1.9, 0.095, 6.0, 6.3, 9.1, 2.8, 0.98, 2.4, 0.15, 2.0, 0.074]

data World = World
  { plain  :: !String
  , shiftK :: !Int
  }

main :: IO ()
main = runSketch (sketch "34-00 Caesar Cipher and Brute Force" (World examplePlain startShift))
  { skBack  = rgb255 16 20 26
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- ★ シーザー暗号の本体：A〜Z だけを k 個ずらす（Z の次は A に戻る = mod 26）
caesar :: Int -> String -> String
caesar k = map shiftCh
  where
    shiftCh ch
      | ch >= 'A' && ch <= 'Z' = chr (65 + (ord ch - 65 + k) `mod` 26)
      | otherwise = ch

-- ★ 英語らしさ：文字の頻度と英語の頻度の「ずれ」（カイ二乗）。小さいほど英語っぽい
englishScore :: String -> Float
englishScore s
  | total == 0 = 0
  | otherwise = sum (zipWith chi (U.toList counts) enFreq)
  where
    letters = [ord ch - 65 | ch <- s, ch >= 'A', ch <= 'Z']
    total = length letters
    counts = U.accum (+) (U.replicate 26 (0 :: Int)) [(c, 1) | c <- letters]
    chi c f =
      let expect = fromIntegral total * f / 100
          d = fromIntegral c - expect
      in d * d / expect

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (header ++ table ++ rows ++ [footer])
  where
    sw = envW env
    sh = envH env
    k = shiftK w
    cipherText = caesar k (plain w)
    cursor = if envFrame env `mod` 60 < 30 then "_" else ""

    -- ---- 上：暗号化のようす
    header =
      [ txt env 24 36 18 (gray 0.94) ("Caesar cipher  k = " ++ show k ++ "  (Left/Right)   type letters, Backspace, 1 example, 2 clear")
      , txt env 24 60 14 (gray 0.6) "Educational only: there are just 26 keys, so it breaks right away"
      , txt env 24 104 20 skyBlue "Plain"
      , mono env 120 104 20 16 skyBlue (plain w ++ cursor)
      , txt env 24 140 20 amber "Cipher"
      , mono env 120 140 20 16 amber cipherText
      ]

    -- アルファベットの対応表（上の段 → 下の段）
    cw = min 34 ((sw - 48) / 26)
    table = concat
      [ [ box env x 164 (cw - 3) 56 (rgb255 40 50 64)
        , txtC env (x + cw / 2 - 1) 186 16 skyBlue [chr (65 + i)]
        , txtC env (x + cw / 2 - 1) 212 16 amber [chr (65 + (i + k) `mod` 26)]
        ]
      | i <- [0 .. 25]
      , let x = 24 + fromIntegral i * cw ]

    -- ---- 下：総当たり（暗号文を 26 通りの k で戻してみる）
    top = 252
    rowH = max 14 ((sh - top - 30) / 26)
    trials = [(t, caesar (negate t) cipherText) | t <- [0 .. 25]]
    scores = map (englishScore . snd) trials
    best = snd (minimum (zip scores [0 :: Int ..]))
    barX = sw - 260
    rows =
      txt env 24 (top - 8) 16 (gray 0.94) "Brute force: undo with k = 0..25  (bar = chi-square, shorter = more English)"
      : concat (zipWith row trials scores)
    row (t, s) score =
      let y = top + fromIntegral t * rowH
          isBest = t == best
          bar = clampF 2 220 (score / 8)   -- χ² の棒（小さいほど英語の頻度に近い）
      in [ if isBest then box env 18 (y + 2) (sw - 36) (rowH - 1) (rgba255 60 160 100 90) else Blank
         , mono env 24 (y + rowH * 0.8) 14 12 (if isBest then white else gray 0.63) (two t ++ "  " ++ take 70 s)
         , box env barX (y + rowH * 0.2) bar (rowH * 0.6) (if isBest then rgb255 90 230 140 else rgb255 90 110 140)
         ]
    footer = txt env (sw - 610) (sh - 8) 15 (rgb255 90 230 140)
      ("Most English-like: k = " ++ show best
        ++ (if best == k then "  -> key found!" else "  (short text can fool it)"))

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyLeft) Down _ _  -> w { shiftK = (shiftK w + 25) `mod` 26 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { shiftK = (shiftK w + 1) `mod` 26 }
  EventKey (SpecialKey KeyBackspace) Down _ _ -> backspace
  EventKey (SpecialKey KeyDelete) Down _ _    -> backspace   -- Mac の delete キー
  EventKey (Char '\b') Down _ _   -> backspace
  EventKey (Char '\DEL') Down _ _ -> backspace
  EventKey (Char '1') Down _ _    -> w { plain = examplePlain }
  EventKey (Char '2') Down _ _    -> w { plain = "" }
  EventKey (SpecialKey KeySpace) Down _ _ -> typed ' '
  EventKey (Char c) Down _ _      -> typed c
  _ -> w
  where
    backspace = w { plain = take (length (plain w) - 1) (plain w) }
    typed c
      | length (plain w) >= maxLen = w
      | isAscii c && isAlpha c || c == ' ' || c == '.' || c == ',' = w { plain = plain w ++ [toUpper c] }
      | otherwise = w

-- ---------- やってみよう ----------
-- 1. 自分の名前を打って、k = 13（ROT13）で 2 回暗号化すると元に戻るのを確かめよう
-- 2. 「HI」のような短い文だと、英語らしさの判定が外れることがある。何文字あれば当たる？
-- 3. caesar を「偶数番目は +k、奇数番目は −k」に変えると、総当たりは何通りになる？
-- 4. enFreq を日本語のローマ字の頻度に置きかえて、ローマ字の文で試そう

-- ==================================================
-- ここから下は「道具」（caesar と englishScore が主役）
-- ==================================================
skyBlue, amber :: Color
skyBlue = rgb255 120 200 255
amber = rgb255 255 200 90

two :: Int -> String
two n = if n < 10 then '0' : show n else show n

-- p5.js と同じ「左上が原点・y は下向き」の座標を gloss の座標に直す
toS :: Env -> Point -> Point
toS e (x, y) = (x - envW e / 2, envH e / 2 - y)

-- 文字（(x, y) は左端の下）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt e x y s c str = let (a, b) = toS e (x, y) in label a b s c str

-- 中央揃えの文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC e x y s = \c str -> txt e (x - fromIntegral (length str) * s * 0.36) y s c str

-- 1 文字ずつ等間隔に並べる（総当たりの表で桁がそろう）
mono :: Env -> Float -> Float -> Float -> Float -> Color -> String -> Picture
mono e x y s pitch c str =
  Pictures [txt e (x + fromIntegral i * pitch) y s c [ch] | (i, ch) <- zip [0 :: Int ..] str, ch /= ' ']

-- 左上が (x, y) の長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box e x y bw bh c = let (a, b) = toS e (x + bw / 2, y + bh / 2) in Color c (rect a b bw bh)
