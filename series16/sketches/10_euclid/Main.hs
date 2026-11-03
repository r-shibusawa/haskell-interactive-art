-- ============================================
-- 10 ユークリッドの互除法：長方形を正方形で切り取る
-- a × b の長方形から、いちばん大きい正方形を切り取り続けると、最後の正方形の 1 辺が最大公約数 gcd(a, b)
-- 拡張版は帰り道で ax + by = d となる整数 x, y も求める（拡張ユークリッドの互除法）
-- スペース：1 ステップ / A：自動 / ドラッグ：長方形の右下の角で a, b を変える
-- 1：gcd(45, 33)  2：gcd(161, 119)  3：フィボナッチ（最悪の場合）  4：ランダム
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)

speed :: Int
speed = 40 -- ★ 自動再生の間隔

-- 表の 1 行
data Row = Row { rA, rB :: !Int, rQ :: !(Maybe Int), rD, rX, rY :: !Int }

data World = World
  { wA     :: !Int
  , wB     :: !Int
  , wRows  :: [Row]
  , wS     :: !Int
  , wAuto  :: !Bool
  , wTimer :: !Int
  , wDrag  :: !Bool
  }

main :: IO ()
main = runSketch (sketch "16-10 Euclid" (prepare (World 161 119 [] 0 False 0 False)))
  { skBack  = hsb 265 45 10
  , skStep  = \_ _ w -> pure (stepW w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

-- 行き：a を b で割った余りで次の行へ。帰り：x, y を下の行から計算する
rowsOf :: Int -> Int -> [Row]
rowsOf x y
  | y <= 0 = [Row x 0 Nothing x 1 0]
  | otherwise =
      let q = x `div` y
          below = rowsOf y (x `mod` y)
          (d, nx, ny) = case below of
            (r : _) -> (rD r, rX r, rY r)
            [] -> (x, 1, 0)
      in Row x y (Just q) d ny (nx - q * ny) : below

prepare :: World -> World
prepare w = w { wRows = rowsOf (wA w) (wB w), wS = 0 }

total :: World -> Int
total w = 2 * length (wRows w) - 1

nextS :: World -> World
nextS w = w { wS = if wS w >= total w then 0 else wS w + 1 }

stepW :: World -> World
stepW w0 =
  let timer = if wAuto w0 then wTimer w0 + 1 else wTimer w0
      w1 = w0 { wTimer = timer }
  in if wAuto w0 && timer `mod` speed == 0 then nextS w1 else w1

scaleOf :: Env -> World -> Float
scaleOf env w = min (envW env * 0.5 / fromIntegral (wA w)) ((envH env - 170) / fromIntegral (wB w))

-- 切り取りを 1 行ずつ進める：(x, y, 向き, 図形)
cutSquares :: Env -> Float -> Float -> Float -> Int -> [Row] -> (Int, Int, Bool, [Picture])
cutSquares env sc ox oy shown rows = go 0 (take shown rows) 0 0 True
  where
    go :: Int -> [Row] -> Int -> Int -> Bool -> (Int, Int, Bool, [Picture])
    go _ [] x y h = (x, y, h, [])
    go i (r : rs) x y h =
      let side = rB r
          q = maybe 0 id (rQ r)
          sf = fromIntegral side * sc
          c = hsba (fromIntegral ((i * 47) `mod` 360)) 60 85 0.8
          sq k =
            let (cx, cy) = if h then (x + k * side, y) else (x, y + k * side)
                fx = ox + fromIntegral cx * sc
                fy = oy + fromIntegral cy * sc
            in Pictures [ box env c (fx + 0.75) (fy + 0.75) (sf - 1.5) (sf - 1.5) ]
          lbl = if sf > 40
            then txtC env (ox + (fromIntegral x + fromIntegral side / 2) * sc)
                          (oy + (fromIntegral y + fromIntegral side / 2) * sc)
                          (max 14 (min 22 (sf * 0.3))) (hsb 0 0 10) (show side)
            else Blank
          (x', y') = if h then (x + q * side, y) else (x, y + q * side)
          (xe, ye, he, ps) = go (i + 1) rs x' y' (not h)
      in (xe, ye, he, map sq [0 .. q - 1] ++ [lbl] ++ ps)

drawW :: Env -> World -> Picture
drawW env w =
  let a = wA w
      b = wB w
      s = wS w
      rows = wRows w
      nr = length rows
      sc = scaleOf env w
      ox = 40
      oy = 110
      shown = min s (nr - 1)
      (xe, ye, horizontal, squares) = cutSquares env sc ox oy shown rows
      lastRow = case drop (nr - 1) rows of
        (r : _) -> r
        [] -> Row a b Nothing 1 1 0
      g = rD lastRow
      gcdBox
        | s >= nr - 1 =
            let bx = if not horizontal then xe - g else xe
                by = if not horizontal then ye else ye - g
                gf = fromIntegral g * sc
            in frame env (hsb 45 90 100) 4 (ox + fromIntegral bx * sc) (oy + fromIntegral by * sc) gf gf
        | otherwise = Blank
      -- 表
      px = envW env * 0.55
      py = 120
      cw = (envW env - px - 30) / 6
      rh = min 34 ((envH env - 330) / fromIntegral nr)
      heads = ["a", "b", "a/b", "d", "x", "y"]
      headP = [ txtC env (px + fromIntegral j * cw + cw / 2) py 16 (hsb 0 0 70) h | (j, h) <- zip [0 :: Int ..] heads ]
      rowP (i, r) =
        let yy = py + 26 + fromIntegral (i :: Int) * rh
            down = i < s
            up = s >= nr - 1 && i >= total w - s
            bg | down && not up && i == s - 1 = hsb 45 70 40
               | up && i == total w - s = hsb 160 60 35
               | otherwise = hsb 265 30 18
            col j = px + cw * (fromIntegral (j :: Int) + 0.5)
        in if not down && not up then Blank else Pictures
             [ box env bg px (yy - rh / 2 + 2) (cw * 6) (rh - 4)
             , txtC env (col 0) yy 16 (hsb 0 0 92) (show (rA r))
             , txtC env (col 1) yy 16 (hsb 0 0 92) (show (rB r))
             , txtC env (col 2) yy 16 (hsb 0 0 92) (maybe "-" show (rQ r))
             , if up
                 then Pictures [ txtC env (col 3) yy 16 (hsb 45 80 100) (show (rD r))
                               , txtC env (col 4) yy 16 (hsb 160 60 95) (show (rX r))
                               , txtC env (col 5) yy 16 (hsb 160 60 95) (show (rY r)) ]
                 else Blank
             ]
      my = py + 40 + fromIntegral nr * rh
      pw = envW env - px - 30
      prevRow = case drop (s - 1) rows of
        (r : _) | s >= 1 -> r
        _ -> lastRow
      msg | s == 0 = "Space to start. Going down: next row uses the remainder of a / b (gcd(a, b) = gcd(b, a mod b))"
          | s < nr =
              if s == nr - 1
                then "b = 0: gcd = " ++ show g ++ ". The way back starts from d = a*1 + 0*0"
                else show (rA prevRow) ++ " = " ++ show (maybe 0 id (rQ prevRow)) ++ " x " ++ show (rB prevRow) ++ " + "
                       ++ show (rA prevRow `mod` max 1 (rB prevRow)) ++ " : cut " ++ show (maybe 0 id (rQ prevRow)) ++ " squares"
          | otherwise = "Way back: x <- y of the row below, y <- x below - (a/b) * y below"
      first = case rows of
        (r : _) -> r
        [] -> lastRow
  in Pictures
       [ txt env 30 50 26 (hsb 0 0 92) ("Euclid's algorithm: gcd(" ++ show a ++ ", " ++ show b ++ ")")
       , txt env 30 80 15 (hsb 0 0 60) ("Space: 1 step  A: auto" ++ (if wAuto w then " (on)" else "")
                                         ++ "  Drag corner: a, b  1: (45, 33)  2: (161, 119)  3: Fibonacci  4: random")
       , frame env (hsb 0 0 70) 2 ox oy (fromIntegral a * sc) (fromIntegral b * sc)
       , Pictures squares
       , gcdBox
       , dot env (hsb 0 0 100) (ox + fromIntegral a * sc) (oy + fromIntegral b * sc) 16
       , Pictures headP
       , Pictures (map rowP (zip [0 ..] rows))
       , para env px my 16 (hsb 0 0 92) pw msg
       , if s >= total w
           then txtT env px (my + 50) 18 (hsb 45 80 100)
                  (show (rA first) ++ "*(" ++ show (rX first) ++ ") + " ++ show (rB first) ++ "*(" ++ show (rY first) ++ ") = " ++ show (rD first))
           else Blank
       , footer env px pw 14 (hsb 0 0 60)
           ("Steps: " ++ show (nr - 1) ++ ". Fibonacci pairs are the worst case (Lame's theorem): O(log b) steps.\nx, y are Bezout coefficients. If d = 1, x is the inverse of a mod b: used for RSA keys (13).")
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let sc = scaleOf env w
    in pure (if dist2 (mouseP env) (40 + fromIntegral (wA w) * sc, 110 + fromIntegral (wB w) * sc) < 20
               then w { wDrag = True } else w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = False }
  EventMotion _
    | wDrag w ->
        let sc = scaleOf env w
            (mx, my) = mouseP env
            a = max 2 (min 400 (round ((mx - 40) / sc)))
            b = max 1 (min 400 (round ((my - 110) / sc)))
            w1 = prepare w { wA = a, wB = b }
        in pure w1 { wS = total w1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (nextS w)
  EventKey (Char c0) Down _ _ -> case toLower c0 of
    'a' -> pure w { wAuto = not (wAuto w) }
    '1' -> pure (prepare w { wA = 45, wB = 33 })
    '2' -> pure (prepare w { wA = 161, wB = 119 })
    '3' -> pure (prepare w { wA = 89, wB = 55 })
    '4' -> do
      a <- randomRIO (20, 299)
      b <- randomRIO (10, a - 1)
      pure (prepare w { wA = a, wB = b })
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 3 のフィボナッチ（89, 55）は毎回正方形が 1 枚だけ → いちばんステップが多い
-- 2. a と b が互いに素（gcd = 1）だと、最後の正方形は 1 × 1
-- 3. 帰り道の計算を手でやって、表の x, y と比べる

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> (Float, Float) -> (Float, Float)
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> (Float, Float) -> (Float, Float) -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 長方形の枠線（太さ t）
frame :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
frame env c t x y w h = Pictures
  [ seg env c t (x, y) (x + w, y), seg env c t (x + w, y) (x + w, y + h)
  , seg env c t (x + w, y + h) (x, y + h), seg env c t (x, y + h) (x, y) ]

-- 直径 d の円（p5.js の circle）
dot :: Env -> Color -> Float -> Float -> Float -> Picture
dot env c x y d = Color c (at (pt env (x, y)) (disc (d / 2)))

-- 直径 d の円の輪郭（太さ t）
hoop :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
hoop env c t x y d = Color c (at (pt env (x, y)) (ring (d / 2) t))

-- 文字：txt は左下が (x, y)、txtT は左上が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtT, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtT env x y s c str = txt env x (y + s) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = txt env (x - fromIntegral (length str) * s * 0.78) y s c str

-- ---------- 道具：文字の折り返し（文字の幅はおよそ 0.85 × 大きさ） ----------
wrapText :: Float -> Float -> String -> [String]
wrapText s w = concatMap one . lines
  where
    limit = max 8 (floor (w / (s * 0.85))) :: Int
    one l = case words l of
      [] -> [""]
      (x : xs) -> go x xs
    go cur [] = [cur]
    go cur (x : xs)
      | length cur + 1 + length x <= limit = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs

lineH :: Float -> Float
lineH s = s * 1.35

-- 左上 (x, y) から幅 w で折り返して書く
para :: Env -> Float -> Float -> Float -> Color -> Float -> String -> Picture
para env x y s c w str = Pictures
  [ txtT env x (y + fromIntegral i * lineH s) s c l | (i, l) <- zip [0 :: Int ..] (wrapText s w str) ]

-- 折り返したときの高さ
paraH :: Float -> Float -> String -> Float
paraH s w str = fromIntegral (length (wrapText s w str)) * lineH s

-- 上から順に積む：(大きさ, 色, 下のすきま, 文字)
stack :: Env -> Float -> Float -> Float -> [(Float, Color, Float, String)] -> Picture
stack env x y0 w = Pictures . go y0
  where
    go _ [] = []
    go y ((s, c, gap, str) : rest) = para env x y s c w str : go (y + paraH s w str + gap) rest

-- 画面の下の端にそろえて書く
footer :: Env -> Float -> Float -> Float -> Color -> String -> Picture
footer env x w s c str = para env x (envH env - 14 - paraH s w str) s c w str
