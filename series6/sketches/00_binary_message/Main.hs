-- ============================================
-- 00 文字をビットにする（情報は 0 と 1）
-- キーボードで文字を打つと、1文字が 8 個の電球（ビット）になって並ぶ
-- コンピュータも通信も、すべてはこの 0/1 の列を送っている
-- キーで入力 / Backspace で消す / クリックで送信アニメーション
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR, (.&.))
import Data.Char (ord, toUpper)

data World = World
  { wMessage :: !String   -- 打った文字
  , wSending :: !Bool     -- 送信アニメーション中か
  , wSendPos :: !Float    -- ビット列が流れた距離
  }

maxChars :: Int
maxChars = 18             -- ★ 打てる文字数

bitSpacing :: Float
bitSpacing = 14           -- ★ 流れるビットの間隔

main :: IO ()
main = runSketch (sketch "06-00 Binary Message" (World "HELLO" False 0))
  { skBack  = hsb 230 40 10
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- ★ 文字 → 文字コード（ASCII）→ 2進数 8 桁
toBits :: Char -> String
toBits ch = [if (ord ch `shiftR` k) .&. 1 == 1 then '1' else '0' | k <- [7, 6 .. 0]]

allBits :: String -> String
allBits = concatMap toBits

step :: Env -> World -> World
step env w
  | not (wSending w) = w
  | pos > fromIntegral (length (allBits (wMessage w))) * bitSpacing + envW env = w { wSending = False, wSendPos = pos }
  | otherwise = w { wSendPos = pos }
  where pos = wSendPos w + 4

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Pictures (zipWith (drawRow env) [0 ..] (takeWhile visible (zip rowYs msg)))
  , Color (hsb 0 0 30) (Line [toG env 40 lineY, toG env (envW env - 40) lineY])
  , if wSending w then drawStream env lineY (wSendPos w) bits else Blank
  , txt env 20 40 18 (hsb 0 0 100)
      ("Type letters or digits  \"" ++ msg ++ "\" = " ++ show (length msg) ++ " chars = " ++ show (length bits) ++ " bits  Click to send")
  , txt env 20 68 15 (hsb 0 0 60) "Character code (ASCII) as 8 binary digits. 1 byte = 8 bits = 8 light bulbs"
  ]
  where
    msg = wMessage w
    bits = allBits msg
    lineY = envH env - 90
    rowYs = [110 + i * 44 | i <- [0 ..]]
    visible (y, _) = y <= envH env - 160

-- 1 文字ぶんの行：文字、文字コード、8 個の電球
drawRow :: Env -> Int -> (Float, Char) -> Picture
drawRow env _ (y, ch) = Pictures
  ( txt env 40 (y + 8) 18 (hsb 0 0 100) [if ch == ' ' then '_' else ch]
  : txt env 90 (y + 8) 18 (hsb 0 0 60) (show (ord ch))
  : zipWith bulb [0 ..] (toBits ch) )
  where
    bulb :: Float -> Char -> Picture
    bulb k b =
      let on = b == '1'
          x = 160 + k * 40
      in Pictures
           [ Color (if on then hsb 45 90 100 else hsb 230 30 25) (at (toG env x y) (disc 14))
           , txtC env x y 14 (if on then hsb 230 40 20 else hsb 0 40 60) [b]
           ]

-- 送信アニメーション：ビット列が右から流れる。1 = 高い、0 = 低い（電圧のイメージ）
drawStream :: Env -> Float -> Float -> String -> Picture
drawStream env lineY pos bits = Pictures (zipWith one [0 ..] bits)
  where
    one :: Float -> Char -> Picture
    one i b
      | x < 40 || x > envW env - 40 = Blank
      | b == '1' = box env x (lineY - 30) 12 30 (hsb 45 90 100)
      | otherwise = box env x (lineY - 4) 12 4 (hsb 230 30 40)
      where x = 40 + i * bitSpacing - pos + envW env

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char '\b') Down _ _ -> backspace w
  EventKey (SpecialKey KeyBackspace) Down _ _ -> backspace w
  EventKey (SpecialKey KeyDelete) Down _ _ -> backspace w   -- Mac の delete キー
  EventKey (SpecialKey KeySpace) Down _ _ -> typed ' ' w
  EventKey (Char c) Down _ _ | c >= ' ' && c <= '~' -> typed c w
  EventKey (MouseButton LeftButton) Down _ _ -> w { wSending = True, wSendPos = 0 }
  _ -> w

typed :: Char -> World -> World
typed c w
  | length (wMessage w) < maxChars = w { wMessage = wMessage w ++ [toUpper c] }
  | otherwise = w

backspace :: World -> World
backspace w = w { wMessage = take (length (wMessage w) - 1) (wMessage w) }

-- ---------- やってみよう ----------
-- 1. 'A' と 'a' の 2進数を比べよう。1 ビットだけちがう（32 の位）。
--    toUpper を外すと小文字も打てる
-- 2. 数字の '0' は 48。'0'〜'9' の 2 進数の下 4 桁を見ると？
-- 3. 日本語 1 文字は何ビット？（UTF-8 では 24 ビット）
--    ghci で ord 'あ' を調べて、toBits の桁数を増やしてみよう

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

-- 長方形（左上が x, y）
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)
