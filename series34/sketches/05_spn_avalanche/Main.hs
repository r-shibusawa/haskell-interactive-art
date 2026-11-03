-- ============================================
-- 05 ブロック暗号の考え方（置換と転置）
-- 16 ビットのおもちゃのブロック暗号（SPN）。1 ラウンド = 鍵を XOR → S ボックス（4 ビットずつ入れかえ）→ P ボックス（ビットの並べかえ）
-- 入力を 1 ビットだけ変えると、ラウンドを重ねるごとに違いが広がり、最後は約半分（8 ビット）が変わる = なだれ効果
-- 入力のビットをクリック = 変えるビットを選ぶ / ↑↓ = ラウンド数 / ←→ = 鍵を変える / スペース = 入力をランダムに
-- ※ 教育用（16 ビットは小さすぎて総当たりできる）。実際の安全な暗号には使わない
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (popCount, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.Char (toUpper)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Numeric (showHex)
import Text.Printf (printf)

sbox :: U.Vector Int
sbox = U.fromList [0xC, 0x5, 0x6, 0xB, 0x9, 0x0, 0xA, 0xD, 0x3, 0xE, 0xF, 0x8, 0x4, 0x7, 0x1, 0x2]  -- ★ 4 ビットの入れかえ表

samples :: Int
samples = 3000             -- ★ なだれ効果のグラフに使う試行回数

data World = World
  { rounds    :: !Int      -- ★ ラウンド数（1〜8）
  , cipherKey :: !Int      -- ★ 16 ビットの鍵
  , inputX    :: !Int
  , flipBit   :: !Int      -- 変えるビット
  , hist      :: !(U.Vector Int)
  , meanFlip  :: !Float
  }

main :: IO ()
main = runSketch (sketch "34-05 SPN Block Cipher Avalanche" (World 4 0x3A94 0x1234 0 (U.replicate 17 0) 0))
  { skBack  = rgb255 14 16 24
  , skStart = \_ -> measure (World 4 0x3A94 0x1234 0 (U.replicate 17 0) 0)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 途中の状態 1 つ
data Stage = Stage { stName :: String, stV :: !Int, stPerm :: !Bool }

-- ★ 暗号化。途中の状態を全部記録して返す（図に使う）
encryptTrace :: Int -> Int -> Int -> [Stage]
encryptTrace x0 k r = Stage "Input" x0 False : go 0 x0
  where
    go i x
      | i >= r = [Stage "Output" (x `xor` roundKey k r) False]
      | otherwise =
          let a = x `xor` roundKey k i
              b = subst a
              nm = 'R' : show (i + 1)
              rest | i < r - 1 = let c = permute b in Stage (nm ++ " P") c True : go (i + 1) c
                   | otherwise = go (i + 1) b
          in Stage (nm ++ " KeyXor") a False : Stage (nm ++ " S") b False : rest

-- 出力だけを速く求める（encryptTrace の最後の値と同じ）
encrypt :: Int -> Int -> Int -> Int
encrypt x0 k r = foldl' oneRound x0 [0 .. r - 1] `xor` roundKey k r
  where
    oneRound x i =
      let b = subst (x `xor` roundKey k i)
      in if i < r - 1 then permute b else b

-- なだれ効果：ランダムな入力の 1 ビットを変えて、出力が何ビット変わるかを数える
measure :: World -> IO World
measure w = do
  ds <- replicateM samples $ do
    x <- randomRIO (0, 65535)
    b <- randomRIO (0, 15)
    let d = encrypt x (cipherKey w) (rounds w) `xor` encrypt (x `xor` (1 `shiftL` b)) (cipherKey w) (rounds w)
    pure (popCount d)
  let h = U.accum (+) (U.replicate 17 0) [(d, 1) | d <- ds]
  pure w { hist = h, meanFlip = fromIntegral (sum ds) / fromIntegral samples }

bitSize :: Env -> Float
bitSize env = min 26 ((envW env * 0.6 - 200) / 16)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (header ++ rows ++ graph)
  where
    sw = envW env
    sh = envH env
    header =
      [ txt env 24 34 18 (gray 0.94)
          ("Toy SPN block cipher (16 bit)  rounds " ++ show (rounds w) ++ " (Up/Down)  key " ++ hex4 (cipherKey w) ++ " (Left/Right)  Space input")
      , txt env 24 56 14 (gray 0.6) "Click a bit in the top row: B = input with that bit flipped. Red = bits where A and B differ | educational"
      ]
    tA = encryptTrace (inputX w) (cipherKey w) (rounds w)
    tB = encryptTrace (inputX w `xor` (1 `shiftL` flipBit w)) (cipherKey w) (rounds w)
    n = length tA
    bit = bitSize env
    rowH = min 46 ((sh - 120) / fromIntegral n)
    x0 = 150
    y0 = 84
    diffs = zipWith (\a b -> stV a `xor` stV b) tA tB
    rows = concat (zipWith4' row [0 :: Int ..] tA diffs (0 : diffs))
    row i st diff prevDiff =
      let y = y0 + fromIntegral i * rowH
          -- P ボックスの配線（前の行 → この行）
          wires = if not (stPerm st) then [] else
            [ Color (if testBit prevDiff (15 - b) then rgba255 255 100 100 200 else rgba255 80 100 140 90)
                (thickLine 1 (toS env (x0 + fromIntegral b * bit + bit / 2, y - rowH + bit))
                             (toS env (x0 + fromIntegral (pboxIndex b) * bit + bit / 2, y)))
            | b <- [0 .. 15] ]
          cells =
            [ box env bx y (bit - 2) (bit - 2)
                (if testBit diff (15 - b) then rgb255 255 90 90
                 else if testBit (stV st) (15 - b) then rgb255 220 225 240 else rgb255 44 50 66)
            | b <- [0 .. 15], let bx = x0 + fromIntegral b * bit ]
          marker = [ frame env (x0 + fromIntegral b * bit - 2) (y - 2) (bit + 2) (bit + 2) 2 (rgb255 255 210 90)
                   | i == 0, b <- [15 - flipBit w] ]
      in [ txt env 20 (y + bit * 0.7) 14 (gray 0.67) (stName st) ]
         ++ wires ++ cells ++ marker
         ++ [ txt env (x0 + 16 * bit + 12) (y + bit * 0.7) 14 (gray 0.78) (show (popCount diff) ++ (if popCount diff == 1 then " bit differs" else " bits differ")) ]

    -- ---- 右：なだれ効果のヒストグラム
    gx = sw * 0.64
    gy = 110
    gw = sw * 0.33
    gh = sh * 0.5
    mx = max 1 (U.maximum (hist w))
    bw = gw / 17
    graph =
      txt env gx (gy - 20) 14 (gray 0.86) ("Output bits changed per 1-bit flip (" ++ show samples ++ " runs)")
      : concat
        [ [ box env (gx + fromIntegral d * bw + 1) (gy + gh - h) (bw - 2) h (if d == 8 then rgb255 255 210 90 else rgb255 110 140 230)
          , txtC env (gx + fromIntegral d * bw + bw / 2) (gy + gh + 18) 14 (gray 0.63) (show d) ]
        | d <- [0 .. 16 :: Int]
        , let h = fromIntegral (hist w U.! d) / fromIntegral mx * gh ]
      ++ [ txt env gx (gy + gh + 50) 18 (gray 0.94) ("Mean " ++ printf "%.2f" (meanFlip w) ++ " bits (ideal 8 = half)")
         , txt env gx (gy + gh + 80) 14 (gray 0.6) "Round 1: the change stays inside one S-box"
         , txt env gx (gy + gh + 102) 14 (gray 0.6) "More rounds: the P-box spreads it everywhere"
         , txt env gx (gy + gh + 124) 14 (gray 0.6) "-> Real AES: 128 bits, 10 rounds or more"
         ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let bit = bitSize env
        (mx0, my0) = envMouse env
        mx = mx0 + envW env / 2
        my = envH env / 2 - my0
        b = floor ((mx - 150) / bit) :: Int
    in pure (if my > 84 && my < 84 + bit && b >= 0 && b < 16 then w { flipBit = 15 - b } else w)
  EventKey (SpecialKey KeyUp) Down _ _    -> measure w { rounds = min 8 (rounds w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _  -> measure w { rounds = max 1 (rounds w - 1) }
  EventKey (SpecialKey KeyLeft) Down _ _  -> newKey
  EventKey (SpecialKey KeyRight) Down _ _ -> newKey
  EventKey (SpecialKey KeySpace) Down _ _ -> (\x -> w { inputX = x }) <$> randomRIO (0, 65535)
  _ -> pure w
  where newKey = do
          k <- randomRIO (0, 65535)
          measure w { cipherKey = k }

-- ---------- やってみよう ----------
-- 1. ラウンド 1 → 2 → 3 で平均は何ビットになる？ 何ラウンドで 8 に近づく？
-- 2. sbox を [0,1,2,...,15]（何もしない）にすると？ 線形な処理だけでは混ざらない
-- 3. pboxIndex を「そのまま b を返す」にすると、違いが 4 ビットの中に閉じこめられる
-- 4. 鍵は 65536 通り。1 秒に 1000 万回試せるコンピュータなら何秒で総当たりできる？

-- ==================================================
-- ここから下は「道具」（S ボックス・P ボックス・ラウンド鍵・図の部品）
-- ==================================================
subst :: Int -> Int
subst x = foldl' (\y i -> y .|. ((sbox U.! ((x `shiftR` (4 * i)) .&. 0xF)) `shiftL` (4 * i))) 0 [0 .. 3]

-- P ボックス：4×4 の表の行と列を入れかえる（左から b 番目のビット → pboxIndex b 番目へ）
pboxIndex :: Int -> Int
pboxIndex b = (b `mod` 4) * 4 + (b `shiftR` 2)

permute :: Int -> Int
permute x = foldl' (\y b -> if testBit x (15 - b) then y .|. (1 `shiftL` (15 - pboxIndex b)) else y) 0 [0 .. 15]

roundKey :: Int -> Int -> Int
roundKey k r =
  let s = (3 * r) `mod` 16
  in (((k `shiftL` s) .|. (k `shiftR` (16 - s))) `xor` (0x1357 * (r + 1))) .&. 0xFFFF

hex4 :: Int -> String
hex4 v = let s = map toUpper (showHex v "") in "0x" ++ replicate (4 - length s) '0' ++ s

zipWith4' :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4' f (a : as) (b : bs) (c : cs) (d : ds) = f a b c d : zipWith4' f as bs cs ds
zipWith4' _ _ _ _ _ = []

-- 太さのある枠
frame :: Env -> Float -> Float -> Float -> Float -> Float -> Color -> Picture
frame env x y w h t c = Color c (thickPath t (map (toS env) [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]))

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
