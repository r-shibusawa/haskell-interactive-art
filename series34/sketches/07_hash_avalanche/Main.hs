-- ============================================
-- 07 ハッシュ関数となだれ効果
-- どんな長さの文も 32 ビットの「指紋」にする。1 文字変えるだけで約半分（16 ビット）が変わる。
-- 下：誕生日のパラドックス。k ビットに切り詰めたハッシュは、約 √(2^k) 個で「同じ指紋」が見つかる
-- 文字を打つ = 文を入力 / Backspace = 消す / 1 = 例文 / ←→ = 切り詰めるビット数 k
-- ※ 教育用（FNV-1a と自作のミックス関数。安全なハッシュは SHA-256 など）
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (popCount, rotateL, shiftL, shiftR, testBit, xor, (.&.))
import Data.Char (chr, ord, toUpper)
import qualified Data.IntSet as IS
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showHex)
import Text.Printf (printf)

startMsg :: String
startMsg = "hello world"   -- ★ ハッシュする文

perFrame :: Int
perFrame = 1               -- ★ 1 フレームに何回実験するか

data World = World
  { msg    :: !String
  , kBits  :: !Int         -- ★ 誕生日の実験で使うビット数（8〜24）
  , trials :: ![Int]       -- 衝突までに試した個数の記録（新しいものが先頭、最大 400 個）
  }

main :: IO ()
main = runSketch (sketch "34-07 Hash and Avalanche" (World startMsg 16 []))
  { skBack  = rgb255 14 18 20
  , skStep  = \_ _ w -> do
      new <- replicateM perFrame (birthday (kBits w))
      pure w { trials = take 400 (new ++ trials w) }
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (header ++ section 96 "FNV-1a 32" fnv1a ++ section 226 "Mini mix hash" mixHash ++ birthdayPart)
  where
    sw = envW env
    sh = envH env
    m = msg w
    k = kBits w
    cursor = if envFrame env `mod` 60 < 30 then "_" else ""
    header =
      [ txt env 24 34 18 (gray 0.94) ("Hash and avalanche  type, Backspace, 1 example  |  Left/Right birthday test k = " ++ show k ++ " bits")
      , txt env 24 56 14 (gray 0.6) "Same text -> always the same value. One changed letter -> a totally different value | educational"
      ]
    -- 最後の 1 文字を「次の文字」に変えた文と比べる
    other = case reverse m of
      (c : r) -> reverse (chr (ord c + 1) : r)
      [] -> "a"
    section :: Float -> String -> (String -> Word32) -> [Picture]
    section y name f =
      let h1 = f m
          h2 = f other
      in [ txt env 24 y 16 (gray 0.78) name
         , drawRow env 24 (y + 14) ("\"" ++ m ++ cursor ++ "\"") h1 0
         , drawRow env 24 (y + 54) ("\"" ++ other ++ "\"") h2 (h1 `xor` h2)
         , txt env (sw - 300) y 16 (rgb255 255 200 90) (show (popCount (h1 `xor` h2)) ++ " / 32 bits changed")
         ]

    -- ---- 誕生日のパラドックス
    gy = 96 + 2 * 130 + 20
    gh = sh - gy - 70
    gw = sw - 48
    theory = sqrt (pi / 2 * 2 ^^ k) :: Float
    nT = length (trials w)
    mean = fromIntegral (sum (trials w)) / fromIntegral (max 1 nT) :: Float
    -- 個数のヒストグラム（横軸 0〜4×理論値）
    bins = U.accum (+) (U.replicate 40 (0 :: Int))
      [(min 39 (floor (fromIntegral t / (theory * 4) * 40)), 1) | t <- trials w]
    mb = max 1 (U.maximum bins)
    bw = gw / 40
    tx = 24 + gw / 4
    birthdayPart =
      txt env 24 gy 16 (gray 0.86) ("Birthday paradox: hash random strings to " ++ show k ++ " bits and count until two match")
      : [ box env (24 + fromIntegral i * bw + 1) (gy + gh - h) (bw - 2) h (rgb255 110 170 230)
        | i <- [0 .. 39]
        , let h = fromIntegral (bins U.! i) / fromIntegral mb * (gh - 30) ]
      ++ [ Color (rgb255 255 200 90) (thickLine 2 (toS env (tx, gy + 20)) (toS env (tx, gy + gh)))
         , txt env (tx + 8) (gy + 36) 15 (rgb255 255 200 90) ("Theory sqrt(pi/2 * 2^k) = " ++ printf "%.0f" theory)
         , txt env 24 (sh - 40) 17 (gray 0.94)
             ("Mean " ++ printf "%.0f" mean ++ " strings to collide (out of 2^" ++ show k ++ " = " ++ commas (2 ^ k) ++ " values!)")
         , txt env 24 (sh - 14) 14 (gray 0.6) "256 bits need about 2^128 strings -> longer than the age of the universe. So secure hashes are long"
         ]

-- 32 ビットを 32 個の四角で（diff の立っているビットは赤）
drawRow :: Env -> Float -> Float -> String -> Word32 -> Word32 -> Picture
drawRow env x y str h diff = Pictures
  ( [ box env (x + fromIntegral b * bw) y (bw - 2) 26
        (if testBit diff (31 - b) then rgb255 255 90 90
         else if testBit h (31 - b) then rgb255 220 235 230 else rgb255 40 52 56)
    | b <- [0 .. 31] ]
  ++ [ txt env (x + 32 * bw + 12) (y + 19) 14 (gray 0.7) (hex8 h ++ "  " ++ take 36 str) ] )
  where bw = min 22 ((envW env - 520) / 32)

-- ★ 誕生日の実験：ランダムな文字列を k ビットのハッシュにして、2 回目の値が出たら終わり
birthday :: Int -> IO Int
birthday k = go 1 IS.empty
  where
    mask = (1 `shiftL` k) - 1 :: Word32
    limit = 1 `shiftL` 22 :: Int
    go :: Int -> IS.IntSet -> IO Int
    go n seen
      | n >= limit = pure limit
      | otherwise = do
          r <- randomRIO (0, 999999999 :: Int)
          let s = 'm' : show r ++ ":" ++ show n
              h = fromIntegral (mixHash s .&. mask) :: Int
          if IS.member h seen then pure n else go (n + 1) (IS.insert h seen)

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyLeft) Down _ _  -> w { kBits = max 8 (kBits w - 2), trials = [] }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { kBits = min 24 (kBits w + 2), trials = [] }
  EventKey (SpecialKey KeyBackspace) Down _ _ -> backspace
  EventKey (SpecialKey KeyDelete) Down _ _    -> backspace   -- Mac の delete キー
  EventKey (Char '\b') Down _ _   -> backspace
  EventKey (Char '\DEL') Down _ _ -> backspace
  EventKey (Char '1') Down _ _    -> w { msg = startMsg }
  EventKey (SpecialKey KeySpace) Down _ _ -> typed ' '
  EventKey (Char c) Down _ _      -> typed c
  _ -> w
  where
    backspace = w { msg = take (length (msg w) - 1) (msg w) }
    typed c | length (msg w) < 40 && c >= ' ' && c <= '~' = w { msg = msg w ++ [c] }
            | otherwise = w

-- ---------- やってみよう ----------
-- 1. "hello world" と "hello worle" の違いは 1 文字。何ビット変わる？ 何回か試して平均は 16 に近い？
-- 2. k を 2 増やすと、衝突までの個数は約何倍になる？（√2 倍 × √2 倍 = 2 倍）
-- 3. mixHash の中の掛け算の数（0x85EBCA6B）を 1 にすると、なだれ効果はどうなる？
-- 4. パスワードはそのまま保存せず、ハッシュ値を保存する。なぜ？ 同じパスワードの人がいたら？

-- ==================================================
-- ここから下は「道具」（2 つのハッシュ関数と表示の道具）
-- Word32 は 32 ビットであふれた分が自動で捨てられる（JavaScript の Math.imul と同じ）
-- ==================================================
code :: Char -> Word32
code = fromIntegral . ord

-- FNV-1a：1 文字ずつ XOR して素数を掛ける。単純で速い（安全用ではない）
fnv1a :: String -> Word32
fnv1a = foldl' (\h c -> (h `xor` code c) * 0x01000193) 0x811C9DC5

-- ミニ混ぜハッシュ：1 文字ごとに回転・掛け算し、最後に「混ぜる」処理を 3 回（なだれ効果を強くする）
mixHash :: String -> Word32
mixHash s =
  let h0 = 0x9E3779B9 `xor` fromIntegral (length s)
      step h c =
        let a = (h `xor` code c) * 0xCC9E2D51
            b = a `rotateL` 13
        in b * 5 + 0xE6546B64
      h1 = foldl' step h0 s
      h2 = (h1 `xor` (h1 `shiftR` 16)) * 0x85EBCA6B
      h3 = (h2 `xor` (h2 `shiftR` 13)) * 0xC2B2AE35
  in h3 `xor` (h3 `shiftR` 16)

hex8 :: Word32 -> String
hex8 v = let s = map toUpper (showHex v "") in replicate (8 - length s) '0' ++ s

-- 3 桁ごとにカンマを入れる
commas :: Int -> String
commas n = reverse (go (reverse (show n)))
  where go s = case splitAt 3 s of
          (a, []) -> a
          (a, b) -> a ++ "," ++ go b

-- p5.js と同じ「左上が原点・y は下向き」の座標を gloss の座標に直す
toS :: Env -> Point -> Point
toS e (x, y) = (x - envW e / 2, envH e / 2 - y)

-- 文字（(x, y) は左端の下）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt e x y s c str = let (a, b) = toS e (x, y) in label a b s c str

-- 左上が (x, y) の長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box e x y bw bh c = let (a, b) = toS e (x + bw / 2, y + bh / 2) in Color c (rect a b bw bh)
