-- ============================================
-- 14 符号で分ける（CDMA）（おまけ）
-- 同じ周波数・同じ時間に、何人もが同時に送っているのに、ちゃんと取り出せる
-- 各ユーザーに「直交する符号」を割り当て、ビットを符号で広げて足し算して送る
-- 受信側は自分の符号をかけて足す（相関）と、自分のビットだけ出てくる
-- クリックでビットをランダムに / 1〜4 で取り出すユーザー
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (digitToInt)
import Data.List (foldl')

data World = World
  { wUserBits :: ![[Int]]   -- 4 人のビット（1 人 3 ビット）
  , wListenTo :: !Int       -- 取り出すユーザー（0〜3）
  }

-- ★ ウォルシュ符号（たがいに直交：かけて足すと 0）
codes :: [[Int]]
codes =
  [ [1, 1, 1, 1, 1, 1, 1, 1]
  , [1, -1, 1, -1, 1, -1, 1, -1]
  , [1, 1, -1, -1, 1, 1, -1, -1]
  , [1, -1, -1, 1, 1, -1, -1, 1]
  ]

chipsPerBit, bitsPerUser :: Int
chipsPerBit = 8
bitsPerUser = 3

main :: IO ()
main = runSketch (sketch "06-14 CDMA" (World [[1, 0, 1], [0, 1, 1], [1, 1, 0], [0, 0, 1]] 0))
  { skBack  = hsb 230 40 10
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 送信信号 = ビット（±1）× 符号
spread :: [Int] -> [Int] -> [Int]
spread code bits = concat [map (* (if b == 1 then 1 else -1)) code | b <- bits]

-- 電波の上では全部足し算される
addAll :: [[Int]] -> [Int]
addAll = foldl' (zipWith (+)) (replicate (chipsPerBit * bitsPerUser) 0)

-- ★ 受信：聞きたいユーザーの符号をかけて、8 個ずつ足す
despread :: [Int] -> [Int] -> [Int]
despread _ [] = []
despread code chips =
  let (now, rest) = splitAt chipsPerBit chips
  in sum (zipWith (*) now code) : despread code rest

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Pictures (zipWith3 user [0 :: Int ..] signals (zip (wUserBits w) codes))
  , drawSteps env total left right 440 10 0 "On the air (everyone added together)" True
    -- ---- 受信 ----
  , txt env left (recvY - 14) 15 (hsb 0 0 100)
      ("Receiver: multiply by user " ++ show (wListenTo w + 1) ++ "'s code, add 8 at a time ->")
  , Pictures (zipWith decodedBox [0 :: Float ..] decoded)
  , txt env 20 40 18 (hsb 0 0 100) "1-4: pick a user   Click: random bits   4 users share one frequency"
  , txt env 20 68 15 (hsb 0 0 60) "Other users' codes multiply-and-add to 0 (orthogonal), so they vanish. Used in 3G phones and GPS"
  ]
  where
    left = 60
    right = envW env - 60
    chipW = (right - left) / fromIntegral (chipsPerBit * bitsPerUser)
    recvY = 560
    signals = zipWith spread codes (wUserBits w)
    total = addAll signals
    listenCode = case drop (wListenTo w) codes of
      (c : _) -> c
      [] -> []
    decoded = despread listenCode total

    -- 描く：4 ユーザー
    user u sig (bits, code) =
      drawSteps env sig left right (120 + fromIntegral u * 70) 20 (if u == wListenTo w then 45 else 170)
        ("User " ++ show (u + 1) ++ "   Bits " ++ concatMap show bits ++ "   Code " ++ map (\c -> if c > 0 then '+' else '-') code)
        False

    decodedBox :: Float -> Int -> Picture
    decodedBox b v =
      let bw = fromIntegral chipsPerBit * chipW
          x = left + b * bw
          pos = v > 0
      in Pictures
           [ roundBox env (x + 4) recvY (bw - 8) 50 8 (if pos then hsb 45 90 100 else hsb 230 30 35)
           , txtC env (x + bw / 2) (recvY + 25) 20 (hsb 0 0 (if pos then 10 else 90))
               ((if pos then "+" else "") ++ show v ++ " -> " ++ (if pos then "1" else "0"))
           ]

drawSteps :: Env -> [Int] -> Float -> Float -> Float -> Float -> Float -> String -> Bool -> Picture
drawSteps env arr left right mid h hue name thick = Pictures
  [ Color (hsb 0 0 25) (Line [toG env left mid, toG env right mid])
  , Color col (thickPath (if thick then 3 else 2) (concat (zipWith edge [0 ..] arr)))
  , txt env left (mid - h * (if thick then 4.5 else 1) - 8) 14 col name
  ]
  where
    w = (right - left) / fromIntegral (length arr)
    col = if hue == 0 then hsb 0 0 90 else hsb hue 80 100
    edge :: Float -> Int -> [Point]
    edge i v =
      let y = mid - fromIntegral v * h
      in [toG env (left + i * w) y, toG env (left + (i + 1) * w) y]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '4' -> pure w { wListenTo = digitToInt c - 1 }
  EventKey (MouseButton LeftButton) Down _ _ -> do
    new <- replicateM 4 (replicateM bitsPerUser (randomRIO (0, 1)))
    pure w { wUserBits = new }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 1〜4 を押して、合成波は同じなのに取り出されるビットが変わるのを確認
-- 2. 符号を 2 つ同じにすると（直交でなくなる）、取り出せなくなる
-- 3. 合成波に雑音を足しても取り出せる？（total に乱数を足してみよう）

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

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
