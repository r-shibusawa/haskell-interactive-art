-- ============================================
-- 12 ドラゴン曲線
-- 細長い紙を同じ向きに n 回半分に折り、広げて、すべての折り目を 90° にすると「ドラゴン曲線」になる
--   折り目の向き（右・左）の並びは「紙折り数列」：1 1 0 1 1 0 0 1 1 1 0 0 1 0 0 …
-- ←→ = 折る回数 n / スペース = 折りたたんだ状態から広げるアニメーション / 4 = 4 匹で平面を埋める / C = 色
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl')
import Numeric (showFFloat)

data World = World
  { wN       :: !Int       -- ★ 折る回数（1〜16）
  , wAng     :: !Float     -- 折り目の角度（180 = たたんだまま、90 = ドラゴン）
  , wPlaying :: !Bool
  , wPlayT   :: !Float
  , wFour    :: !Bool
  , wHue     :: !Int
  , wDirty   :: !Bool      -- 曲線を描き直す必要があるか
  , wCurve   :: Picture    -- 描いておいた曲線（変わるまで使い回す）
  }

main :: IO ()
main = runSketch (sketch "26-12 Dragon Curve" (World 10 90 False 0 False 0 True Blank))
  { skBack  = rgb255 24 22 30
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ 紙折り数列：k = 2^m × 奇数 o と書いたとき、o を 4 で割った余りが 1 なら 1（右）、3 なら 0（左）
turnAt :: Int -> Int
turnAt k
  | even k = turnAt (k `div` 2)
  | k `mod` 4 == 1 = 1
  | otherwise = 0

-- 折り目ごとに ±foldAng 曲がりながら進んだ点の列
curvePoints :: Int -> Float -> [(Double, Double)]
curvePoints k a =
  let nn = 2 ^ k :: Int
      ar = realToFrac (180 - a) * pi / 180 :: Double
      go (!x, !y, !c, acc) i =
        let hd = fromIntegral (c :: Int) * ar
            x' = x + cos hd
            y' = y + sin hd
            c' | i < nn = if turnAt i == 1 then c + 1 else c - 1
               | otherwise = c
        in (x', y', c', (x', y') : acc)
      (_, _, _, rev) = foldl' go (0, 0, 0, [(0, 0)]) [1 .. nn]
  in reverse rev

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 =
  let w1 | wPlaying w0 =
             let t = wPlayT w0 + 1 / 180
             in w0 { wPlayT = t, wAng = lerpF 180 90 (easeInOut (min 1 t)), wDirty = True, wPlaying = t < 1 }
         | otherwise = w0
  in pure (if wDirty w1 then w1 { wCurve = renderCurve env w1, wDirty = False } else w1)

easeInOut :: Float -> Float
easeInOut x = if x < 0.5 then 2 * x * x else 1 - (-2 * x + 2) * (-2 * x + 2) / 2

renderCurve :: Env -> World -> Picture
renderCurve env w =
  let pts = curvePoints (wN w) (wAng w)
      -- 4 匹のときは原点のまわりに 90° ずつ回す
      copies = if wFour w then 4 else 1 :: Int
      rot :: Int -> (Double, Double) -> (Double, Double)
      rot c (x, y) = case c of
        0 -> (x, y)
        1 -> (-y, x)
        2 -> (-x, -y)
        _ -> (y, -x)
      -- 画面に収まるように拡大・移動
      ext (!a, !b, !c, !d) (x, y) = (min a x, max b x, min c y, max d y)
      (x0, x1, y0, y1) = foldl' ext (0, 0, 0, 0)
        (if wFour w then concat [[(x, y), (-y, x), (-x, -y), (y, -x)] | (x, y) <- pts] else pts)
      wd = realToFrac (envW env) :: Double
      ht = realToFrac (envH env) :: Double
      s = min ((wd - 80) / max 1 (x1 - x0)) ((ht - 220) / max 1 (y1 - y0))
      ox = wd / 2 - (x0 + x1) / 2 * s
      oy = 100 + (ht - 220) / 2 - (y0 + y1) / 2 * s
      toS (x, y) = sc env (realToFrac (ox + x * s), realToFrac (oy + y * s))
      sw = clampF 1 4 (realToFrac s * 0.35)
      segs = length pts - 1
      total = segs * copies
      groups = 64 :: Int
      per = max 1 ((segs + groups - 1) `div` groups)
      -- 線分が多いときは細い線 1 本（Line）で描いて、描画の要素を減らす
      thickOK = total <= 6000 && sw >= 1.5
      joints = total <= 3000
      draw qs
        | thickOK = Pictures (thickPath sw qs : [at q (disc (sw / 2)) | joints, q <- qs])
        | otherwise = Line qs
      hueOf c g
        | wHue w == 0 = hsb (wrapF 360 (fromIntegral c * 90 + fromIntegral g / fromIntegral groups * 80 + 10)) 70 100
        | otherwise = hsb (fromIntegral g / fromIntegral groups * 360) 80 100
      chunks g xs
        | length (take 2 xs) < 2 = []
        | otherwise = (g, take (per + 1) xs) : chunks (g + 1) (drop per xs)
      oneCopy c =
        let arr = map (toS . rot c) pts
        in [Color (hueOf c g) (draw part) | (g, part) <- chunks (0 :: Int) arr]
  in Pictures (concatMap oneCopy [0 .. copies - 1])

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let n = wN w
      nn = 2 ^ n :: Int
      -- 紙折り数列（最初の 63 個）
      seqStr = concat [show (turnAt k) ++ (if k `mod` 8 == 7 then "  " else " ") | k <- [1 .. min 64 nn - 1]]
      seqLines = wrapW (envW env - 40) 15 ("Paper-folding sequence (1 = right, 0 = left): " ++ seqStr)
      seqPics = [ txt env 20 (envH env - 96 + fromIntegral i * 18) 15 (rgb255 255 190 40) l | (i, l) <- zip [0 :: Int ..] seqLines ]
      hud =
        [ txt env 20 40 18 white ("Left/Right: n = " ++ show n ++ " (" ++ show nn ++ " segments)   Space: unfold animation   4: four dragons " ++ (if wFour w then "ON" else "off") ++ "   C: color")
        , txt env 20 68 15 (gray 0.67) ("Fold angle " ++ showFFloat (Just 0) (wAng w) "" ++ " deg (180 = still folded, 90 = dragon curve). The curve never crosses itself.")
        ]
  in Pictures ([wCurve w] ++ seqPics ++ hud ++ [drawStrip env w])

-- 細長い紙と折り目（n ≤ 7 のとき）
drawStrip :: Env -> World -> Picture
drawStrip env w =
  let k = min (wN w) 7
      nn = 2 ^ k :: Int
      x0 = 20
      y0 = envH env - 36
      sw = envW env - 40
      (cx, cy) = sc env (x0 + sw / 2, y0 + 9)
  in Pictures $
       [ Color (rgb255 246 240 226) (rect cx cy sw 18) ]
       ++ [ Color (if turnAt i == 1 then rgb255 60 110 230 else rgb255 220 70 50)
              (thickLine 2 (sc env (x, y0)) (sc env (x, y0 + 18)))
          | i <- [1 .. nn - 1], let x = x0 + sw * fromIntegral i / fromIntegral nn ]
       ++ [ txt env x0 (y0 - 6) 14 (gray 0.63) ("Creases on the unfolded strip (blue = valley = 1, red = mountain = 0)" ++ (if wN w > 7 then "   showing up to n = 7" else "")) ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wN = min 16 (wN w + 1), wDirty = True }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wN = max 1 (wN w - 1), wDirty = True }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = True, wPlayT = 0, wN = min 13 (wN w) }
  EventKey (Char '4') Down _ _ -> w { wFour = not (wFour w), wDirty = True }
  EventKey (Char c) Down _ _ | toLower c == 'c' -> w { wHue = 1 - wHue w, wDirty = True }
  EventResize _ -> w { wDirty = True }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 細長い紙を 3〜4 回折って広げ、折り目を全部 90° にして立ててみよう。画面と同じ形になる？
-- 2. 紙折り数列の「1 つおき」（奇数番目）だけを見ると 1 0 1 0 … のくり返し。偶数番目を見ると？
-- 3. wAng の初期値を 90 以外（例：120、60）に固定すると、どんな曲線になる？
-- 4. 4 匹のドラゴンは重ならずに平面をすき間なく埋める。n を大きくして確かめよう

-- ==================================================
-- ここから下は「画面の道具」
-- ==================================================

sc :: Env -> Point -> Point
sc env (x, y) = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sc env (x, y) in label a b size c s

wrapW :: Float -> Float -> String -> [String]
wrapW w size = go "" . words
  where
    maxC = floor (w / (size * 0.72)) :: Int
    go cur [] = [cur | not (null cur)]
    go "" (x : xs) = go x xs
    go cur (x : xs)
      | length cur + 1 + length x <= maxC = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs
