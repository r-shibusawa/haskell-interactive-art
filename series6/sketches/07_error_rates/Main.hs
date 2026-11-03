-- ============================================
-- 07 誤り率くらべ（どの符号が強い？）
-- 雑音の強さを変えながら、3 つの方法で何万ビットも送って、誤り率をグラフにする
--   そのまま / 3 回くり返し / ハミング(7,4)
-- 自動で雑音を変えながら測定 / クリックでやり直し
-- ============================================
module Main (main) where

import Art
import Data.Bits (xor)
import System.Random (StdGen, newStdGen, randoms)

data Result = Result { rP :: !Float, rRaw :: !Float, rRep :: !Float, rHam :: !Float }

data World = World
  { wResults :: ![Result]
  , wP       :: !Float     -- 次に測る雑音（ビット反転確率）
  }

startP, endP, growth :: Float
startP = 0.005             -- ★ 測り始めの雑音
endP = 0.5                 -- ★ 測り終わりの雑音
growth = 1.15              -- ★ 1 回ごとに雑音を何倍にするか

blocks :: Int
blocks = 1000              -- ★ 4 ビットずつ送る回数（合計 4000 ビット）

main :: IO ()
main = runSketch (sketch "06-07 Error Rates" (World [] startP))
  { skBack  = hsb 230 40 10
  , skStep  = \_ _ w -> step w
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- 自動で p を増やしながら測る
step :: World -> IO World
step w
  | wP w > endP = pure w
  | otherwise = do
      g <- newStdGen
      let !r = measure g (wP w)
      pure w { wResults = wResults w ++ [r], wP = wP w * growth }

-- 確率 prob で乱数がビットをひっくり返す
flipBit :: Float -> Float -> Int -> Int
flipBit prob r b = if r < prob then 1 - b else b

-- ★ ハミング(7,4)：符号化して雑音を通し、1 ビット訂正して 4 ビットを取り出す
hamming :: Float -> [Float] -> [Int] -> [Int]
hamming prob rs [d1, d2, d3, d4] =
  let code = zipWith (flipBit prob) rs
               [d1 `xor` d2 `xor` d4, d1 `xor` d3 `xor` d4, d1, d2 `xor` d3 `xor` d4, d2, d3, d4]
  in case code of
       [c0, c1, c2, c3, c4, c5, c6] ->
         let pos = (c0 `xor` c2 `xor` c4 `xor` c6)
                 + 2 * (c1 `xor` c2 `xor` c5 `xor` c6)
                 + 4 * (c3 `xor` c4 `xor` c5 `xor` c6)
             fixed = zipWith (\i b -> if i == pos then 1 - b else b) [1 ..] code
         in case fixed of
              [_, _, a, _, b, c, d] -> [a, b, c, d]
              _ -> []
       _ -> []
hamming _ _ _ = []

-- 3 回くり返し：3 つの乱数で 1 ビットを送って多数決
repeat3 :: Float -> [Float] -> Int -> Int
repeat3 prob rs b = if sum (map (\r -> flipBit prob r b) rs) >= 2 then 1 else 0

chunks3 :: [Float] -> [[Float]]
chunks3 [] = []
chunks3 xs = let (a, b) = splitAt 3 xs in a : chunks3 b

-- 1 回の測定：4000 ビットのデータを各方式で送って誤り率を返す
measure :: StdGen -> Float -> Result
measure g prob = go blocks (randoms g) 0 0 0
  where
    n = fromIntegral (blocks * 4) :: Float
    go :: Int -> [Float] -> Int -> Int -> Int -> Result
    go 0 _ !eRaw !eRep !eHam =
      Result prob (fromIntegral eRaw / n) (fromIntegral eRep / n) (fromIntegral eHam / n)
    go k rs !eRaw !eRep !eHam =
      let (r0, r1) = splitAt 4 rs     -- データ
          (r2, r3) = splitAt 4 r1     -- そのまま
          (r4, r5) = splitAt 12 r3    -- 3 回くり返し
          (r6, r7) = splitAt 7 r5     -- ハミング
          d = map (\r -> if r < 0.5 then 1 else 0) r0
          count xs = length (filter id (zipWith (/=) xs d))
          raw = zipWith (flipBit prob) r2 d
          rep = zipWith (repeat3 prob) (chunks3 r4) d
          ham = hamming prob r6 d
      in go (k - 1) r7 (eRaw + count raw) (eRep + count rep) (eHam + count ham)

log10 :: Float -> Float
log10 = logBase 10

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ -- 軸（両対数）
    Color (hsb 0 0 30) (Pictures [Line [toG env left base, toG env right base], Line [toG env left base, toG env left top]])
  , Pictures [ Pictures [ Color (hsb 0 0 22) (Line [toG env left (yOf e), toG env right (yOf e)])
                        , txt env (left - 60) (yOf e + 5) 14 dim ("10^" ++ show (round e :: Int)) ]
             | e <- [-3, -2, -1, 0] ]
  , Pictures [ txt env (xOf e - 30) (base + 22) 14 dim ("p=10^" ++ show (round e :: Int)) | e <- [-2, -1] ]
  , txt env (right - 300) (base + 44) 14 dim "Noise (bit flip probability) ->"
  , txt env left (top - 14) 14 dim "Error rate after decoding"
  , Pictures (map curve series)
  , Pictures (zipWith legend [0 ..] series)
  , txt env 20 40 18 (hsb 0 0 100)
      ("Raising the noise step by step...  Measurements: " ++ show (length (wResults w)) ++ "   Click to restart")
  , txt env 20 68 15 dim "Log-log plot. Low noise: Hamming ~100x fewer errors than raw. High noise: all about the same."
  ]
  where
    dim = hsb 0 0 60
    left = 100
    right = envW env - 80
    base = envH env - 100
    top = 110
    yOf e = remap e (-4) 0 base top
    xOf e = remap e (log10 startP) (log10 endP) left right
    series =
      [ (rRaw, 0, "Raw (no coding)")
      , (rRep, 40, "Repeat x3 (speed 1/3)")
      , (rHam, 120, "Hamming(7,4) (speed 4/7)") ]
    colorOf :: Float -> Color
    colorOf hue = if hue == 0 then hsb 0 0 80 else hsb hue 80 100
    curve (field, hue, _) =
      Color (colorOf hue) (thickPath 3
        [ toG env (xOf (log10 (rP r))) (yOf (log10 (max 0.0001 (field r)))) | r <- wResults w ])
    legend :: Float -> (Result -> Float, Float, String) -> Picture
    legend i (_, hue, name) =
      let y = base - 120 + i * 30     -- 線と重ならない右下に置く
          (gx, gy) = toG env (envW env - 405) (y + 6)
      in Pictures
           [ Color (colorOf hue) (rect gx gy 30 12)
           , txt env (envW env - 380) (y + 12) 15 (colorOf hue) name ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> World [] startP
  _ -> w

-- ---------- やってみよう ----------
-- 1. p が小さいとき、くり返し符号の誤り率は p の何乗で減る？（傾きを読む）
-- 2. 3 回くり返しとハミングはどちらが強い？ 速さも考えると？
-- 3. 7 回くり返しを追加してみよう（repeat3 と chunks3 をまねする）

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s
