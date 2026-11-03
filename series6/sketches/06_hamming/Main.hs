-- ============================================
-- 06 ハミング符号（1 ビットの誤りを自動で直す）
-- 4 ビットのデータに 3 ビットの「検査ビット」を足して 7 ビットにする
-- どこか 1 ビットが反転しても、検査ビットの組み合わせで「どこが壊れたか」がわかる
-- クリックで送信中のビットをひっくり返す / スペースで新しいデータ
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (shiftR, xor, (.&.))

data World = World
  { wData     :: ![Int]   -- 送りたい 4 ビット
  , wSent     :: ![Int]   -- 符号化した 7 ビット
  , wReceived :: ![Int]   -- 受信した 7 ビット
  }

main :: IO ()
main = runSketch (sketch "06-06 Hamming Code" (fromData [1, 0, 1, 1]))
  { skBack  = hsb 230 40 10
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ 符号化：検査ビット p1, p2, p3 は、決まったデータビットの偶奇（XOR）
encode :: [Int] -> [Int]
encode [d1, d2, d3, d4] =
  let p1 = d1 `xor` d2 `xor` d4
      p2 = d1 `xor` d3 `xor` d4
      p3 = d2 `xor` d3 `xor` d4
  in [p1, p2, d1, p3, d2, d3, d4]   -- 位置 1〜7（検査ビットは 1, 2, 4 番目）
encode other = other

fromData :: [Int] -> World
fromData d = let s = encode d in World d s s

-- ★ 復号：受信側で検査を計算し直す。3 つの検査結果を 2 進数で読むと、壊れた位置になる！
--   戻り値は（壊れた位置（0 なら誤りなし）、取り出した 4 ビット）
decode :: [Int] -> (Int, [Int])
decode r@[r0, r1, r2, r3, r4, r5, r6] =
  let c1 = r0 `xor` r2 `xor` r4 `xor` r6
      c2 = r1 `xor` r2 `xor` r5 `xor` r6
      c3 = r3 `xor` r4 `xor` r5 `xor` r6
      pos = c1 * 1 + c2 * 2 + c3 * 4
      fixed = flipAt (pos - 1) r
  in (pos, pick fixed)
  where
    pick [_, _, a, _, b, c, d] = [a, b, c, d]
    pick xs = xs
decode r = (0, r)

-- i 番目（0 から）のビットを反転する。範囲外なら何もしない
flipAt :: Int -> [Int] -> [Int]
flipAt i = zipWith (\k b -> if k == i then 1 - b else b) [0 ..]

cellOf :: Env -> Float
cellOf env = min 90 ((envW env - 200) / 8)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ drawBits env d 100 (h * 0.2) cell "Data to send (4 bits)" (map (const 170) d)
  , drawBits env s 100 (h * 0.38) cell "Encoded (7 bits): p1 p2 d1 p3 d2 d3 d4 - parity bits added" [40, 40, 170, 40, 170, 170, 170]
  , drawBits env r 100 (h * 0.56) cell "Received (click to break a bit)" (zipWith (\a b -> if a /= b then 0 else 230) r s)
  , drawBits env out 100 (h * 0.76) cell "Decoded data" (zipWith (\a b -> if a == b then 120 else 0) out d)
  , Pictures (zipWith (\k line' -> txt env msgX (h * 0.56 + cell / 2 - 6 + k * 30) 20 (hsb 0 0 100) line') [0 ..] msg)
  , txt env 20 40 18 (hsb 0 0 100) "Click: flip a received bit (fixes 1 only. Break 2 and...?)  Space: new data"
  , txt env 20 68 15 (hsb 0 0 60) "p1 = d1^d2^d4   p2 = d1^d3^d4   p3 = d2^d3^d4   (^ = XOR: 1 if the count of 1s is odd)"
  ]
  where
    h = envH env
    d = wData w
    s = wSent w
    r = wReceived w
    (pos, out) = decode r
    cell = cellOf env
    msgX = 100 + 7 * (cell + 8) + 10        -- 受信の行の右に出す
    bin = concatMap (show . (\k -> (pos `shiftR` k) .&. 1)) [2, 1, 0 :: Int]
    msg | pos == 0 = ["Check = 000", "-> no errors"]
        | otherwise = ["Check = " ++ bin ++ " = " ++ show pos, "-> fixed bit " ++ show pos ++ "!"]

drawBits :: Env -> [Int] -> Float -> Float -> Float -> String -> [Float] -> Picture
drawBits env bits x0 y0 cell title hues = Pictures
  ( txt env x0 (y0 - 12) 15 (hsb 0 0 100) title
  : zipWith3 one [0 :: Int ..] bits hues )
  where
    one i b hue =
      let x = x0 + fromIntegral i * (cell + 8)
          quiet = hue == 230
          fillC = if quiet then hsb hue 25 35 else hsb hue 70 90
          textC = hsb 0 0 (if quiet then 90 else 10)
      in Pictures
           [ roundBox env x y0 cell cell 10 fillC
           , txtC env (x + cell / 2) (y0 + cell / 2) 30 textC (show b)
           , txtC env (x + cell / 2) (y0 + cell + 14) 14 textC' (show (i + 1))
           ]
    textC' = hsb 0 0 70

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let cell = cellOf env
        y0 = envH env * 0.56
        (mx, my) = mouseXY env
        i = floor ((mx - 100) / (cell + 8)) :: Int
    in pure $ if my >= y0 && my <= y0 + cell && i >= 0 && i < 7
         then w { wReceived = flipAt i (wReceived w) }
         else w
  EventKey (SpecialKey KeySpace) Down _ _ -> fromData <$> replicateM 4 (randomRIO (0, 1))
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 7 個のどれを壊しても直ることを確かめよう（検査ビット自身が壊れても OK）
-- 2. 2 個壊すと「間違った場所」を直してしまう。ハミング(7,4) は 1 ビット訂正まで
-- 3. 05 の 3 回くり返し（12 ビットで 4 ビット送る）と比べて、7 ビットで済む。効率 4/7

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 文字（中央が x, y）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = let (gx, gy) = toG env x y in labelC gx gy size c s

-- 角の丸い長方形（左上が x, y、角の半径 r）
roundBox :: Env -> Float -> Float -> Float -> Float -> Float -> Color -> Picture
roundBox env x y w h r c = Color c (Polygon (concat
  [ corner (x + w - r) (y + r) (-90), corner (x + w - r) (y + h - r) 0
  , corner (x + r) (y + h - r) 90, corner (x + r) (y + r) 180 ]))
  where
    corner cx cy a0 = [ toG env (cx + r * cos a) (cy + r * sin a)
                      | i <- [0 .. 4 :: Int], let a = (a0 + 22.5 * fromIntegral i) * pi / 180 ]
