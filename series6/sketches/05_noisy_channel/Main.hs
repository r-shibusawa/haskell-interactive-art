-- ============================================
-- 05 雑音のある通信路（画像を送る）
-- 送ったビットが確率 p でひっくり返る「二元対称通信路」
-- 絵をそのまま送ると… / 3 回ずつ送って多数決すると…（くり返し符号）
-- マウス左右 = ビットが反転する確率 / 1 = そのまま / 2 = 3 回くり返し / 3 = 5 回
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftR)
import Numeric (showFFloat)
import System.Random (mkStdGen, randomRs)

newtype World = World { wRepeat :: Int }   -- 何回くり返して送るか

imgW, imgH :: Int
imgW = 48
imgH = 32

main :: IO ()
main = runSketch (sketch "06-05 Noisy Channel" (World 1))
  { skBack  = hsb 230 40 10
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- 元の絵：スマイルマーク（0/1）
original :: [Int]
original =
  [ if face || eye || mouth then 1 else 0
  | y <- [0 .. imgH - 1], x <- [0 .. imgW - 1]
  , let fx = fromIntegral x :: Float
        fy = fromIntegral y :: Float
        cx = fromIntegral imgW / 2
        cy = fromIntegral imgH / 2
        d = dist2 (fx, fy) (cx, cy)
        face = d < 14 && d > 12
        eye = dist2 (fx, fy) (cx - 5, cy - 4) < 2 || dist2 (fx, fy) (cx + 5, cy - 4) < 2
        mouth = d > 7 && d < 9 && fy > cy + 2
  ]

-- ★ 通信路：各ビットが確率 p で反転。くり返し送って多数決で復元
--   seed が同じなら同じ雑音になる（p5.js の randomSeed と同じ考え方）
transmit :: Int -> Float -> Int -> [Int] -> [Int]
transmit seed p rep = go (randomRs (0, 1) (mkStdGen seed))
  where
    go _ [] = []
    go rs (b : bs) =
      let (now, rest) = splitAt rep rs
          ones = sum [if r < p then 1 - b else b | r <- now]   -- 反転！
          decoded = if 2 * ones > rep then 1 else 0            -- 多数決
      in decoded : go rest bs

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ drawImage env original 60 120 cell "Sent picture"
  , drawImage env received (envW env / 2 + 40) 120 cell ("Received picture (sent " ++ show rep ++ "x, majority vote)")
  , txt env 20 40 18 (hsb 0 0 100)
      ("Flip p = " ++ nf 2 p ++ " (mouse X)  1: x1  2: x3  3: x5   Errors " ++ show errs ++ " / " ++ show total
       ++ " (" ++ nf 1 (100 * fromIntegral errs / fromIntegral total) ++ " %)")
  , txt env 20 68 15 (hsb 0 0 60)
      ("Sends " ++ show (total * rep) ++ " bits (x" ++ show rep ++ "). More repeats: more accurate, but slower. Smarter: 06 Hamming")
  ]
  where
    p = clampF 0 0.5 (remap (fst (mouseXY env)) 0 (envW env) 0 0.5)
    rep = wRepeat w
    seed = envFrame env `shiftR` 4              -- 少しずつ変わる
    received = transmit seed p rep original
    errs = length (filter id (zipWith (/=) received original))
    total = length original
    cell = min ((envW env / 2 - 100) / fromIntegral imgW) ((envH env - 200) / fromIntegral imgH)

drawImage :: Env -> [Int] -> Float -> Float -> Float -> String -> Picture
drawImage env img ox oy cell title = Pictures
  ( txt env ox (oy - 10) 16 (hsb 0 0 100) title
  : zipWith pixel [0 ..] img )
  where
    pixel :: Int -> Int -> Picture
    pixel i b =
      let x = ox + fromIntegral (i `mod` imgW) * cell
          y = oy + fromIntegral (i `div` imgW) * cell
          (gx, gy) = toG env (x + (cell - 1) / 2) (y + (cell - 1) / 2)
      in Color (if b == 1 then onColor else offColor) (rect gx gy (cell - 1) (cell - 1))

onColor, offColor :: Color
onColor = hsb 45 90 100
offColor = hsb 230 30 20

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char '1') Down _ _ -> World 1
  EventKey (Char '2') Down _ _ -> World 3
  EventKey (Char '3') Down _ _ -> World 5
  _ -> w

-- ---------- やってみよう ----------
-- 1. p = 0.1 で 1 回 → 3 回 → 5 回 と比べよう。誤りがどのくらい減る？
-- 2. p = 0.5 だと何回くり返しても直らない（情報がゼロ）。p = 0.9 だと…？（反転すれば読める！）
-- 3. 自分の絵を描いて送ろう（original を変える）

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

-- 小数を d 桁で文字にする（p5.js の nf）
nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
