-- ============================================
-- 01 換字式暗号と頻度分析
-- A〜Z をバラバラに入れかえる暗号。鍵は 26! ≒ 4×10^26 通りで総当たりは無理。
-- でも「E がいちばん多い」など英語の文字の偏りは暗号文にも残る → 頻度で解ける
-- 上の段（暗号文の文字）をクリック → 下の段（英語の文字）をクリックで「この文字はこれだ」と推理
-- F = 頻度の順位でまとめて推理 / C = 推理を消す / R = 新しい鍵 / H = 1 文字だけ正解を教えてもらう
-- ※ 教育用。実際の安全な暗号には使わない
-- ============================================
module Main (main) where

import Art
import Data.Char (chr, ord)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as U

-- 平文（このスケッチ用に書いた英文）。長いほど頻度分析が当たりやすい
plainText :: String
plainText =
  "EVERY SECRET MESSAGE LEAVES A SHADOW. WHEN THE LETTERS ARE ONLY SWAPPED, THE "
  ++ "COMMON ONES STAY COMMON. THE LETTER E STILL APPEARS MORE THAN ANY OTHER, AND THE WORD THE "
  ++ "SHOWS UP AGAIN AND AGAIN. A PATIENT READER CAN COUNT THE SYMBOLS, COMPARE THEM WITH ORDINARY "
  ++ "ENGLISH TEXT, AND SLOWLY TURN THE NOISE BACK INTO WORDS. THIS IS WHY SIMPLE SUBSTITUTION "
  ++ "CIPHERS WERE BROKEN MORE THAN A THOUSAND YEARS AGO BY SCHOLARS WHO STUDIED LANGUAGE."

enFreq :: U.Vector Float
enFreq = U.fromList
  [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.15, 0.77, 4.0, 2.4,
   6.7, 7.5, 1.9, 0.095, 6.0, 6.3, 9.1, 2.8, 0.98, 2.4, 0.15, 2.0, 0.074]

data World = World
  { perm       :: !(U.Vector Int)  -- 鍵：平文の文字 i → 暗号の文字 perm[i]
  , cipherText :: !String
  , guess      :: !(U.Vector Int)  -- 推理：暗号の文字 c → 平文の文字 guess[c]（-1 = 未定）
  , cnt        :: !(U.Vector Int)  -- 暗号文の文字の出現回数
  , picked     :: !Int             -- クリックで選んだ暗号の文字
  }

main :: IO ()
main = do
  w0 <- newKey
  runSketch (sketch "34-01 Substitution and Frequency Analysis" w0)
    { skBack  = rgb255 18 16 24
    , skDraw  = drawWorld
    , skEvent = onEvent
    }

-- ★ ランダムな入れかえ = 鍵
newKey :: IO World
newKey = do
  p <- U.fromList <$> shuffle [0 .. 25]
  let code ch | ch >= 'A' && ch <= 'Z' = chr (65 + p U.! (ord ch - 65))
              | otherwise = ch
      ctext = map code plainText
      counts = U.accum (+) (U.replicate 26 0) [(ord ch - 65, 1) | ch <- ctext, ch >= 'A', ch <= 'Z']
  pure (World p ctext (U.replicate 26 (-1)) counts (-1))

-- 画面の割りつけ：(棒の左端, 棒 1 本の幅, 2 段の境目)
layout :: Env -> (Float, Float, Float)
layout env = (24, (envW env - 48) / 26, envH env * 0.62)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (header ++ letters ++ bars ++ footer)
  where
    sw = envW env
    sh = envH env
    (gx, gw, midY) = layout env
    h = sh * 0.22
    violet' = rgb255 150 120 230
    good = rgb255 120 230 150

    header =
      [ txt env 24 34 18 (gray 0.94) "Substitution cipher  click a top bar (cipher letter), then a bottom bar (English)"
      , txt env 24 56 14 (gray 0.6) "F guess by frequency rank   C clear   R new key   H hint   | educational: frequency breaks it fast"
      ]

    -- ---- 文章（推理を当てはめた結果）
    pitch = 12 :: Float
    perLine = max 1 (floor ((sw - 48) / pitch)) :: Int
    letters = zipWith letter [0 :: Int ..] (cipherText w)
    letter i ch =
      let (ln, col) = i `divMod` perLine
          x = 24 + fromIntegral col * pitch
          y = 86 + fromIntegral ln * 21
          c = ord ch - 65
      in if c >= 0 && c < 26
           then let g = guess w U.! c
                in if g >= 0
                     then txt env x y 14 (if perm w U.! g == c then good else rgb255 255 120 120) [chr (97 + g)]
                     else txt env x y 14 (if c == picked w then rgb255 255 220 90 else rgb255 110 110 140) [ch]
           else if ch == ' ' then Blank else txt env x y 14 (gray 0.43) [ch]
    correct = length [() | c <- [0 .. 25], let g = guess w U.! c, g >= 0, perm w U.! g == c]

    -- ---- ヒストグラム 2 段
    maxC = max 1 (U.maximum (cnt w))
    bars = concatMap bar [0 .. 25]
    bar i =
      let x = gx + fromIntegral i * gw
          cx = x + gw / 2
          -- 上：暗号文の文字の回数
          bh = fromIntegral (cnt w U.! i) / fromIntegral maxC * h
          topCol | i == picked w = rgb255 255 220 90
                 | guess w U.! i >= 0 = violet'
                 | otherwise = rgb255 90 90 130
          -- 下：英語の頻度
          eh = enFreq U.! i / 12.7 * h
          used = U.elem i (guess w)
          g = guess w U.! i
      in [ box env (x + 2) (midY - bh) (gw - 4) bh topCol
         , txtC env cx (midY + 18) 14 (gray 0.9) [chr (65 + i)]
         , if g >= 0 then txtC env cx (midY + 38) 14 violet' [chr (97 + g)] else Blank
         , box env (x + 2) (midY + 50) (gw - 4) eh (if used then rgb255 70 140 100 else rgb255 70 170 220)
         , txtC env cx (midY + 50 + eh + 18) 14 (gray 0.9) [chr (97 + i)]
         ]
    footer =
      [ txt env gx (midY - h - 8) 14 (gray 0.78) "Cipher letter counts (click to pick)"
      , txt env gx (sh - 8) 14 (gray 0.78) "English letter frequency (click to assign)"
      , txt env (sw - 300) (midY - h - 8) 16 good ("Correct letters: " ++ show correct ++ " / 26")
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (click env w)
  EventKey (Char c) Down _ _
    | c `elem` "fF" -> pure (autoGuess w)
    | c `elem` "cC" -> pure w { guess = U.replicate 26 (-1) }
    | c `elem` "rR" -> newKey
    | c `elem` "hH" -> pure (hint w)
  _ -> pure w

click :: Env -> World -> World
click env w
  | i < 0 || i >= 26 = w
  | my < midY + 40 && my > midY - envH env * 0.3 = w { picked = i }
  | my > midY + 40 && picked w >= 0 = (assign (picked w) i w) { picked = -1 }
  | otherwise = w
  where
    (gx, gw, midY) = layout env
    (mx0, my0) = envMouse env
    mx = mx0 + envW env / 2
    my = envH env / 2 - my0
    i = floor ((mx - gx) / gw) :: Int

-- 暗号の文字 c に英字 t を当てはめる（すでに使われている英字なら入れかえる）
assign :: Int -> Int -> World -> World
assign c t w =
  let g0 = guess w
      g1 = case U.elemIndex t g0 of
             Just other -> g0 U.// [(other, g0 U.! c)]
             Nothing -> g0
  in w { guess = g1 U.// [(c, t)] }

-- ★ 頻度分析：暗号文で多い順と、英語で多い順をそのまま対応させる
autoGuess :: World -> World
autoGuess w = w { guess = U.replicate 26 (-1) U.// zip cOrder eOrder }
  where
    cOrder = sortBy (\a b -> compare (cnt w U.! b) (cnt w U.! a)) [0 .. 25]
    eOrder = sortBy (\a b -> compare (enFreq U.! b) (enFreq U.! a)) [0 .. 25]

-- まだ正しくない文字のうち、いちばん多い文字の正解を 1 つ教える
hint :: World -> World
hint w = case wrong of
  [] -> w
  (c0 : cs) ->
    let best = foldl (\b c -> if cnt w U.! c > cnt w U.! b then c else b) c0 cs
        truth = fromMaybe 0 (U.elemIndex best (perm w))
    in assign best truth w
  where
    isRight c = let g = guess w U.! c in g >= 0 && perm w U.! g == c
    wrong = [c | c <- [0 .. 25], not (isRight c), cnt w U.! c > 0]

-- ---------- やってみよう ----------
-- 1. F を押すと何文字当たる？ E と T はたいてい当たる。残りは単語の形（THE, AND）から推理しよう
-- 2. plainText を短く（1 文）すると、F で当たる文字はどう変わる？
-- 3. 暗号文の中の「3 文字の単語」でいちばん多いものは何？ それは THE の可能性が高い
-- 4. 日本語のローマ字の文に変えると、どの文字がいちばん多くなる？

-- ==================================================
-- ここから下は「道具」（シャッフルと、座標の変換）
-- ==================================================
-- リストをランダムに並べかえる（p5.js の shuffle）
shuffle :: [a] -> IO [a]
shuffle [] = pure []
shuffle xs = do
  i <- randomRIO (0, length xs - 1)
  let (a, rest) = splitAt i xs
  case rest of
    (x : b) -> (x :) <$> shuffle (a ++ b)
    [] -> pure a

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
