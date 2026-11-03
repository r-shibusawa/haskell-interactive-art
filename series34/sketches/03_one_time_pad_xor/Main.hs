-- ============================================
-- 03 ワンタイムパッドと XOR
-- 1 ビットずつ「鍵」と XOR（同じなら 0、違えば 1）。鍵が完全にランダムで 1 回きりなら、理論上ぜったいに破れない。
-- でも同じ鍵を 2 回使うと：C1 ⊕ C2 = (A ⊕ K) ⊕ (B ⊕ K) = A ⊕ B　→ 鍵が消えて 2 枚の絵が重なって見える！
-- ドラッグ = 絵 A に描く（右ドラッグ/Shift で消す）/ スペース = 鍵の使い回し ON/OFF / K = 新しい鍵 / C = 絵 A を消す / 1 = 絵 A を戻す
-- ※ 教育用。実際の安全な暗号には使わない
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (xor)
import qualified Data.Vector.Unboxed as U

cols, rows :: Int
cols = 40    -- ★ ビットの数（横）
rows = 28    -- ★ ビットの数（縦）

type Bits = U.Vector Int

data World = World
  { imgA    :: !Bits
  , imgB    :: !Bits
  , key1    :: !Bits
  , key2    :: !Bits
  , reuse   :: !Bool   -- True = 同じ鍵を 2 回使う（ダメな使い方）
  , drawing :: !Bool   -- ドラッグ中か
  , erasing :: !Bool   -- 消す描き方か
  }

main :: IO ()
main = do
  (k1, k2) <- newKeys
  runSketch (sketch "34-03 One-Time Pad and XOR" (World pictureA pictureB k1 k2 True False False))
    { skBack  = rgb255 12 16 22
    , skDraw  = drawWorld
    , skEvent = onEvent
    }

-- 絵 A：にこにこ顔
pictureA :: Bits
pictureA = gridOf $ \x y ->
  let dx = x - fromIntegral cols * 0.5
      dy = y - fromIntegral rows * 0.5
      r = hypot dx (dy * 1.1)
      face = r > 10 && r < 12.5
      eye = hypot (dx + 4) (dy + 3) < 1.8 || hypot (dx - 4) (dy + 3) < 1.8
      mouth = dy > 1 && dy < 4.5 && abs (hypot dx dy - 5.5) < 1
  in face || eye || mouth

-- 絵 B：大きな星
pictureB :: Bits
pictureB = gridOf $ \x y ->
  let dx = x - fromIntegral cols * 0.5
      dy = y - fromIntegral rows * 0.52
      a = atan2 dy dx
      edge = 7 + 5 * cos (5 * a + 1.57)
  in hypot dx dy < edge

gridOf :: (Float -> Float -> Bool) -> Bits
gridOf f = U.generate (cols * rows) $ \i ->
  let (y, x) = i `divMod` cols
  in if f (fromIntegral x) (fromIntegral y) then 1 else 0

newKeys :: IO (Bits, Bits)
newKeys = (,) <$> randomBits <*> randomBits

-- ★ 完全にランダムな鍵
randomBits :: IO Bits
randomBits = U.fromList <$> replicateM (cols * rows) (coin <$> randomRIO (0, 1 :: Float))
  where coin v = if v < 0.5 then 1 else 0

-- パネルの大きさ（p5.js 版より少しだけ低くして、下の段と重ならないようにした）
-- (横の間隔, 縦の間隔, 1 ビットの大きさ)
layout :: Env -> (Float, Float, Float)
layout env =
  let pw = (envW env - 80) / 3
      ph = (envH env - 210) / 2
  in (pw, ph, min (pw / fromIntegral cols) ((ph - 30) / fromIntegral rows))

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (panels ++ header ++ eavesdropper)
  where
    sw = envW env
    sh = envH env
    (pw, ph, cell) = layout env
    kB = if reuse w then key1 w else key2 w
    c1 = U.zipWith xor (imgA w) (key1 w)       -- ★ XOR で暗号化
    c2 = U.zipWith xor (imgB w) kB
    x12 = U.zipWith xor c1 c2                  -- 盗聴者は C1 と C2 だけを XOR する
    blue' = rgb255 120 200 255
    orange' = rgb255 255 190 90
    red' = rgb255 255 110 110
    items =
      [ ("Picture A (plain, drag to draw)", imgA w, blue')
      , ("Key K1 (random)", key1 w, gray 0.78)
      , ("C1 = A xor K1 (sent)", c1, orange')
      , ("Picture B (another plain)", imgB w, blue')
      , (if reuse w then "Key K1 used again (bad)" else "Key K2 (fresh random)", kB, if reuse w then red' else gray 0.78)
      , ("C2 = B xor " ++ (if reuse w then "K1" else "K2"), c2, orange')
      ]
    panels =
      [ drawBits env cell (20 + fromIntegral (i `mod` 3) * (pw + 20)) (80 + fromIntegral (i `div` 3) * (ph + 10)) t b c
      | (i, (t, b, c)) <- zip [0 :: Int ..] items ]

    -- ヘッダー
    header =
      [ txt env 20 32 18 (gray 0.94)
          ("One-time pad (XOR)  Space: reuse key " ++ (if reuse w then "ON (bad)" else "OFF (good)") ++ "   K new key   C clear   1 restore")
      , txt env 20 56 14 (gray 0.6)
          "0 xor 0 = 0, 0 xor 1 = 1, 1 xor 0 = 1, 1 xor 1 = 0.  XOR with K twice restores: A xor K xor K = A  | educational"
      ]

    -- 下：盗聴者の視点
    by = sh - 106
    small = min 2.6 ((sh - by - 20) / fromIntegral rows) * 1.5
    eavesdropper =
      txt env 20 by 17 (if reuse w then red' else rgb255 120 230 150)
        ("Eavesdropper: C1 xor C2 = "
          ++ (if reuse w then "A xor B (key gone! pictures show through)" else "pure noise (separate keys reveal nothing)"))
      : [ box env (sw - 30 - fromIntegral cols * small + fromIntegral x * small) (by - 8 + fromIntegral y * small) small small
            (if x12 U.! (y * cols + x) == 1 then (if reuse w then rgb255 255 120 120 else gray 0.78) else gray 0.12)
        | y <- [0 .. rows - 1], x <- [0 .. cols - 1] ]
      ++ binaryRows
    -- 1 行ぶんの XOR を 0/1 で
    midRow bits = concatMap show [bits U.! (rows `div` 2 * cols + x) | x <- [0 .. 19]]
    binaryRows =
      [ row 26 "A middle row" (midRow (imgA w))
      , row 46 "K1" (midRow (key1 w))
      , row 66 "A xor K1" (midRow c1)
      ]
    row dy name s =
      Pictures [ txt env 20 (by + dy) 14 (gray 0.67) name, txt env 150 (by + dy) 14 (gray 0.67) s ]

drawBits :: Env -> Float -> Float -> Float -> String -> Bits -> Color -> Picture
drawBits env cell x y title bits col = Pictures
  ( txt env x (y + 14) 14 (gray 0.86) title
  : box env x (y + 24) (fromIntegral cols * cell) (fromIntegral rows * cell) (rgb255 28 34 44)
  : [ box env (x + fromIntegral i * cell) (y + 24 + fromIntegral j * cell) (cell - 0.5) (cell - 0.5) col
    | j <- [0 .. rows - 1], i <- [0 .. cols - 1], bits U.! (j * cols + i) == 1 ] )

-- マウスの下のビットに描く（絵 A のパネルは左上）
paint :: Env -> World -> World
paint env w
  | i < 0 || j < 0 || i >= cols || j >= rows = w
  | otherwise = w { imgA = imgA w U.// [(j * cols + i, if erasing w then 0 else 1)] }
  where
    (_, _, cell) = layout env
    (mx, my) = envMouse env
    i = floor ((mx + envW env / 2 - 20) / cell) :: Int
    j = floor ((envH env / 2 - my - (80 + 24)) / cell) :: Int

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down mods _ ->
    pure (paint env w { drawing = True, erasing = shift mods == Down })
  EventKey (MouseButton RightButton) Down _ _ ->
    pure (paint env w { drawing = True, erasing = True })
  EventKey (MouseButton _) Up _ _ -> pure w { drawing = False }
  EventMotion _ | drawing w -> pure (paint env w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { reuse = not (reuse w) }
  EventKey (Char c) Down _ _
    | c `elem` "kK" -> do
        (k1, k2) <- newKeys
        pure w { key1 = k1, key2 = k2 }
    | c `elem` "cC" -> pure w { imgA = U.replicate (cols * rows) 0 }
    | c == '1' -> pure w { imgA = pictureA }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 使い回し ON で絵 A に描くと、下の C1 xor C2 にもそのまま現れる。鍵は毎回ランダムなのに！
-- 2. randomBits の v < 0.5 を v < 0.1 にすると C1 から絵が透けて見える。「ランダムさ」が大事
-- 3. 絵 B を全部 0（白紙）にすると C2 は鍵そのもの。「知っている平文」から鍵がバレる
-- 4. ワンタイムパッドの弱点は「平文と同じ長さの鍵を、事前に安全に渡す」こと。どうやって渡す？（→ 08）

-- ==================================================
-- ここから下は「道具」（xor は Data.Bits の関数）
-- ==================================================
hypot :: Float -> Float -> Float
hypot a b = sqrt (a * a + b * b)

-- p5.js と同じ「左上が原点・y は下向き」の座標を gloss の座標に直す
toS :: Env -> Point -> Point
toS e (x, y) = (x - envW e / 2, envH e / 2 - y)

-- 文字（(x, y) は左端の下）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt e x y s c str = let (a, b) = toS e (x, y) in label a b s c str

-- 左上が (x, y) の長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box e x y bw bh c = let (a, b) = toS e (x + bw / 2, y + bh / 2) in Color c (rect a b bw bh)
