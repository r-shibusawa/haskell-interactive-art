-- ============================================
-- 12 中国剰余定理：2 つの余りから 1 つの数へ
-- 「3 で割ると 2 余り、5 で割ると 3 余る数は？」n₁ と n₂ が互いに素なら、0 〜 n₁n₂−1 にちょうど 1 つある
-- 数 k をマス（k mod n₁, k mod n₂）に置いていくと、斜めに進んで端で反対側へワープし、全部のマスを 1 回ずつ通る
-- マスをクリック：その余りの組になる数を計算 / スペース：一時停止 / R：最初から
-- ←→：n₂ / ↑↓：n₁ / 1：3 と 5　2：4 と 9　3：7 と 11　4：4 と 6（互いに素でない）
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import qualified Data.Map.Strict as M

rate :: Float
rate = 0.15      -- ★ 1 フレームに進む数の量

data World = World
  { wN1     :: !Int    -- ★ 2 つの割る数
  , wN2     :: !Int
  , wK      :: !Float
  , wPaused :: !Bool
  , wPick   :: !(Maybe (Int, Int))
  }

main :: IO ()
main = runSketch (sketch "16-12 Chinese Remainder" (World 3 5 0 False Nothing))
  { skBack  = hsb 265 45 10
  , skStep  = \_ _ w -> pure (stepW w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

stepW :: World -> World
stepW w
  | wPaused w = w
  | otherwise = w { wK = min (fromIntegral (wN1 w * wN2 w + 30)) (wK w + rate) }

-- x の mod m での逆数（なければ Nothing）
modInv :: Int -> Int -> Maybe Int
modInv x m = go (x `mod` m) m 1 0
  where
    go r0 r1 s0 s1
      | r1 == 0 = if r0 == 1 then Just (s0 `mod` m) else Nothing
      | otherwise = let q = r0 `div` r1 in go r1 (r0 - q * r1) s1 (s0 - q * s1)

cellSize :: Env -> World -> Float
cellSize env w = minimum' [envW env * 0.55 / fromIntegral (wN2 w), (envH env - 180) / fromIntegral (wN1 w), 70]
  where minimum' = foldr min 1e9

drawW :: Env -> World -> Picture
drawW env w =
  let n1 = wN1 w
      n2 = wN2 w
      nn = n1 * n2
      g = gcd n1 n2
      cs = cellSize env w
      ox = 70
      oy = 130
      placed = min nn (floor (wK w))
      -- 同じマスに何度も来たら、あとの数で上書きする
      cellMap = M.fromList [((v `mod` n1, v `mod` n2), v) | v <- [0 .. placed - 1]]
      gridCell i j =
        let x = ox + fromIntegral j * cs
            y = oy + fromIntegral i * cs
            val = M.lookup (i, j) cellMap
            c = case val of
              Nothing -> hsb 265 30 16
              Just v -> hsb (fromIntegral v * 360 / fromIntegral nn) 55 80
        in Pictures
             [ box env c (x + 1) (y + 1) (cs - 2) (cs - 2)
             , case val of
                 Just v -> txtC env (x + cs / 2) (y + cs / 2) (max 14 (cs * 0.3)) (hsb 0 0 8) (show v)
                 Nothing -> Blank
             , if wPick w == Just (i, j) then frame env (hsb 45 90 100) 4 (x + 2) (y + 2) (cs - 4) (cs - 4) else Blank
             ]
      colHead = [ txtC env (ox + fromIntegral j * cs + cs / 2) (oy - 12) 14 (hsb 0 0 60) (show j) | j <- [0 .. n2 - 1] ]
      rowHead = [ txtR env (ox - 8) (oy + fromIntegral i * cs + cs / 2 + 6) 14 (hsb 0 0 60) (show i) | i <- [0 .. n1 - 1] ]
      -- いま置いた数の動き（斜めに進む）
      cur = floor (wK w) - 1 :: Int
      curMark
        | cur >= 0 && cur < nn =
            hoop env (hsb 0 0 100) 3 (ox + fromIntegral (cur `mod` n2) * cs + cs / 2)
                 (oy + fromIntegral (cur `mod` n1) * cs + cs / 2) (cs * 0.9)
        | otherwise = Blank
      px = ox + fromIntegral n2 * cs + 50
      pw = envW env - px - 30
      s1 = show
      headLine
        | g == 1 = (17, hsb 150 60 95, 24 :: Float,
                    "gcd(" ++ s1 n1 ++ ", " ++ s1 n2 ++ ") = 1: " ++ s1 nn ++ " numbers fit exactly one each in " ++ s1 nn ++ " cells (1-to-1)")
        | otherwise = (17, hsb 0 70 95, 24,
                       "gcd(" ++ s1 n1 ++ ", " ++ s1 n2 ++ ") = " ++ s1 g ++ " is not 1: numbers repeat with period " ++ s1 (nn `div` g)
                         ++ " and fill only 1/" ++ s1 g ++ " of the cells. Some pairs have no answer, some have several")
      detail = case wPick w of
        Nothing -> [(15, hsb 0 0 65, 0, "Click a cell to compute the number with that pair of remainders")]
        Just (r1, r2) ->
          (17, hsb 0 0 92, 12, "Solve x = " ++ s1 r1 ++ " (mod " ++ s1 n1 ++ "),  x = " ++ s1 r2 ++ " (mod " ++ s1 n2 ++ ")")
            : if g == 1
                then
                  let m1 = n2
                      m2 = n1
                      i1 = maybe 0 id (modInv m1 n1)
                      i2 = maybe 0 id (modInv m2 n2)
                      c1 = m1 * i1
                      c2 = m2 * i2
                      x = (r1 * c1 + r2 * c2) `mod` nn
                  in [ (15, hsb 0 0 80, 8, "m1 = n/n1 = " ++ s1 m1 ++ ",  m2 = n/n2 = " ++ s1 m2)
                     , (15, hsb 0 0 80, 8, "c1 = m1*(m1^-1 mod n1) = " ++ s1 m1 ++ "*" ++ s1 i1 ++ " = " ++ s1 c1 ++ "  (1 mod " ++ s1 n1 ++ ", 0 mod " ++ s1 n2 ++ ")")
                     , (15, hsb 0 0 80, 12, "c2 = m2*(m2^-1 mod n2) = " ++ s1 m2 ++ "*" ++ s1 i2 ++ " = " ++ s1 c2 ++ "  (0 mod " ++ s1 n1 ++ ", 1 mod " ++ s1 n2 ++ ")")
                     , (18, hsb 45 90 100, 0, "x = " ++ s1 r1 ++ "*" ++ s1 c1 ++ " + " ++ s1 r2 ++ "*" ++ s1 c2 ++ " mod " ++ s1 nn ++ " = " ++ s1 x)
                     ]
                else
                  [ (15, hsb 0 70 95, 0, case M.lookup (r1, r2) cellMap of
                      Nothing -> "No number lands in this cell: no solution"
                      Just v -> "Solutions: " ++ s1 v ++ " and shifts by " ++ s1 (nn `div` g) ++ " (not unique)") ]
  in Pictures
       [ txt env 30 50 26 (hsb 0 0 92) ("Chinese remainder theorem: mod " ++ show n1 ++ " and mod " ++ show n2 ++ " to mod " ++ show nn)
       , txt env 30 80 15 (hsb 0 0 60) ("Click cell: solve  Space: " ++ (if wPaused w then "resume" else "pause")
                                         ++ "  R: redo  Up/Down: n1  L/R: n2  1: 3,5  2: 4,9  3: 7,11  4: 4,6")
       , Pictures colHead
       , Pictures rowHead
       , txt env ox (oy - 28) 14 (hsb 0 0 75) ("k mod " ++ show n2 ++ " ->")
       , at (pt env (ox - 40, oy)) (Rotate 90 (label 0 0 14 (hsb 0 0 75) ("k mod " ++ show n1 ++ " ->")))
       , Pictures [gridCell i j | i <- [0 .. n1 - 1], j <- [0 .. n2 - 1]]
       , curMark
       , stack env px 130 pw (headLine : detail)
       , footer env px pw 14 (hsb 0 0 55)
           "c1, c2 are parts that are 1 on one side and 0 on the other. Add them per remainder to get the answer (same idea as Lagrange interpolation).\nBig mod n work can be split into small n1, n2 and rebuilt at the end (fast RSA decryption)."
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let cs = cellSize env w
        (mx, my) = mouseP env
        j = floor ((mx - 70) / cs) :: Int
        i = floor ((my - 130) / cs) :: Int
    in if i >= 0 && i < wN1 w && j >= 0 && j < wN2 w then w { wPick = Just (i, j) } else w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> set2 (wN1 w) (min 16 (wN2 w + 1))
  EventKey (SpecialKey KeyLeft) Down _ _ -> set2 (wN1 w) (max 2 (wN2 w - 1))
  EventKey (SpecialKey KeyUp) Down _ _ -> set2 (min 12 (wN1 w + 1)) (wN2 w)
  EventKey (SpecialKey KeyDown) Down _ _ -> set2 (max 2 (wN1 w - 1)) (wN2 w)
  EventKey (Char c0) Down _ _ -> case toLower c0 of
    '1' -> set2 3 5
    '2' -> set2 4 9
    '3' -> set2 7 11
    '4' -> set2 4 6
    'r' -> w { wK = 0, wPick = Nothing }
    _ -> w
  _ -> w
  where set2 p q = w { wN1 = p, wN2 = q, wK = 0, wPick = Nothing }

-- ---------- やってみよう ----------
-- 1. 「3 で割ると 2、5 で割ると 3 余る数」を表から探して、公式の答えと比べる
-- 2. 4 と 6 で、埋まらないマスの余りの組はどんな組？（偶奇が合わない）
-- 3. 3 つの数（3, 5, 7）に広げるには？ 立方体のマスで考える

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
