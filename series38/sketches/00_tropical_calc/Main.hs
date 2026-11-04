-- ============================================
-- 38-00 トロピカル電卓
-- トロピカル半環：「足し算 ⊕」は min、「掛け算 ⊗」はふつうの足し算
--   a ⊕ b = min(a, b)，a ⊗ b = a + b。引き算（⊕ の逆）はない。1 ⊕ 1 = 1，0 が ⊗ の単位元
-- 操作：←→ = a を変える / ↑↓ = b を変える / M = max 流儀（⊕ = max）に切替 / マウス = 表のマスを読む
-- 画面では ⊕ を (+)、⊗ を (x)、⊘ を (/) と書く
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

nn :: Int
nn = 10                    -- ★ 表の大きさ（0 〜 nn-1）

cell :: Float
cell = 30                  -- マスの大きさ（px）

data World = World
  { wA, wB :: !Int         -- ★ 2 つの数
  , wMax :: !Bool }        -- ★ True なら max 流儀（max-plus 代数）

world0 :: World
world0 = World 3 5 False

main :: IO ()
main = runSketch (sketch "38-00 Tropical Calculator" world0)
  { skBack  = bgCol
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- ★ トロピカルの 2 つの演算
tadd :: World -> Int -> Int -> Int
tadd w x y = if wMax w then max x y else min x y       -- ⊕

tmul :: Int -> Int -> Int
tmul x y = x + y                                       -- ⊗

sym :: World -> String
sym w = if wMax w then "max" else "min"

draw :: Env -> World -> Picture
draw env w = Pictures
  ( [ txt env 20 36 16 white' ("<- ->: a   Up/Down: b   M: " ++ sym w ++ " style (now)   Mouse: read a table cell")
    , txt env 20 62 14 (gray 0.65) ("Tropical semiring: (+) = " ++ sym w ++ ", (x) = +. So 1 (+) 1 = 1 (idempotent). No subtraction exists.")
    , box env (x0 - 10) (y0 - 10) 490 300 (hsba 0 0 100 0.08)
    , txt env x0 (y0 + 40) 28 (hsb 35 90 100) ("a = " ++ show a)
    , txt env (x0 + 160) (y0 + 40) 28 (hsb 330 70 100) ("b = " ++ show b)
    , txt env x0 (y0 + 100) 16 white' ("a (+) b = " ++ sym w ++ "(" ++ show a ++ ", " ++ show b ++ ") = " ++ show s)
    , txt env x0 (y0 + 140) 16 white' ("a (x) b = " ++ show a ++ " + " ++ show b ++ " = " ++ show p)
    , txt env x0 (y0 + 180) 15 white' ("a (/) b = " ++ show a ++ " - " ++ show b ++ " = " ++ show (a - b) ++ " (divide = minus)")
    , txt env x0 (y0 + 220) 15 white' ("a^3 = 3a = " ++ show (3 * a) ++ " (power = ordinary times)")
    , txt env x0 (y0 + 256) 14 (gray 0.6) "No inverse of (+): the equation"
    , txt env x0 (y0 + 278) 14 (gray 0.6) (sym w ++ "(x, " ++ show b ++ ") = " ++ show a ++ " has no unique solution x")
    , txt env x0 ly 16 white' "Check the laws with these numbers"
    ]
    ++ zipWith (\k str -> txt env x0 (ly + 28 + k * 24) 14 (hsb 90 60 95) str) [0 ..] lawLines
    ++ [ drawTable env w tx ty ("(+) table (" ++ sym w ++ ")") (tadd w) 35
       , drawTable env w (tx + fromIntegral nn * cell + 100) ty "(x) table (+)" tmul 330 ] )
  where
    a = wA w
    b = wB w
    s = tadd w a b
    p = tmul a b
    x0 = 30
    y0 = 110
    ly = envH env - 150
    c = 2 :: Int
    sw = sym w
    lawLines =
      [ "Idempotent  a (+) a = a:  " ++ sw ++ "(" ++ show a ++ ", " ++ show a ++ ") = " ++ show (tadd w a a)
      , "Identity  a (+) " ++ (if wMax w then "-inf" else "inf") ++ " = a,  a (x) 0 = " ++ show a ++ " + 0 = " ++ show a
      , "Distributive  a (x) (b (+) c) = (a (x) b) (+) (a (x) c):  " ++ show a ++ " + " ++ sw ++ "(" ++ show b ++ ", " ++ show c ++ ") = "
          ++ show (tmul a (tadd w b c)) ++ " = " ++ sw ++ "(" ++ show (tmul a b) ++ ", " ++ show (tmul a c) ++ ") = " ++ show (tadd w (tmul a b) (tmul a c)) ++ "  (c = 2)"
      , "Binomial  (a (+) b)^2 = a^2 (+) b^2:  2 " ++ sw ++ "(" ++ show a ++ ", " ++ show b ++ ") = " ++ show (2 * tadd w a b)
          ++ " = " ++ sw ++ "(" ++ show (2 * a) ++ ", " ++ show (2 * b) ++ ") = " ++ show (tadd w (2 * a) (2 * b)) ++ "  (freshman's dream)"
      ]
    -- 右：⊕ の表と ⊗ の表（ヒートマップ）
    tx = max 560 (envW env - (2 * fromIntegral nn * cell + 140))
    ty = 130

white' :: Color
white' = gray 1

-- 表を 1 つ描く
drawTable :: Env -> World -> Float -> Float -> String -> (Int -> Int -> Int) -> Float -> Picture
drawTable env w tx ty title f hue = Pictures (
  [ txt env tx (ty - 34) 16 white' title ]
  ++ concat [ cellPic i j | i <- idx, j <- idx ]
  ++ concat [ [ txt env (tx + fromIntegral i * cell + cell / 2 - 5) (ty - 6) 14 (gray 0.7) (show i)
              , txt env (tx - 16) (ty + fromIntegral i * cell + cell / 2 + 6) 14 (gray 0.7) (show i) ] | i <- idx ]
  ++ [ txt env (tx + fromIntegral nn * cell - 40) (ty - 34) 14 (gray 0.7) "a ->"
     , txt env (tx - 30) (ty + fromIntegral nn * cell + 18) 14 (gray 0.7) "b v" ]
  ++ hover )
  where
    idx = [0 .. nn - 1]
    vals = [f i j | i <- idx, j <- idx]
    lo = minimum vals                  -- vals は空にならない（nn >= 1）
    hi = maximum vals
    (mx, my) = mouseP env
    cellOf i j = (tx + fromIntegral i * cell, ty + fromIntegral j * cell)
    cellPic i j =
      let v = f i j
          (cx, cy) = cellOf i j
          vf = fromIntegral v :: Float
          mid = (fromIntegral lo + fromIntegral hi) / 2 :: Float
          bright = remap vf (fromIntegral lo) (fromIntegral hi) 25 100
      in [ box env cx cy (cell - 2) (cell - 2) (hsb hue 70 bright) ]
         ++ [ frameP env (cx - 1) (cy - 1) cell cell 3 white' | i == wA w && j == wB w ]
         ++ [ txtC env (cx + cell / 2) (cy + cell / 2) 14 (if vf > mid then gray 0.1 else white') (show v) ]
    hoverCells = [ (i, j) | i <- idx, j <- idx
                 , let (cx, cy) = cellOf i j
                 , mx >= cx && mx < cx + cell && my >= cy && my < cy + cell ]
    hover = case hoverCells of
      ((i, j) : _) ->
        [ txt env tx (ty + fromIntegral nn * cell + 40) 16 white' ("(" ++ show i ++ ", " ++ show j ++ ") -> " ++ show (f i j))
        , txt env tx (ty + fromIntegral nn * cell + 62) 14 (gray 0.7) "(white frame = current a, b)" ]
      [] -> []

-- 枠線（左上が (x, y)、太さ t）
frameP :: Env -> Float -> Float -> Float -> Float -> Float -> Color -> Picture
frameP env x y w h t c = Pictures
  [ seg env t c (x, y) (x + w, y), seg env t c (x + w, y) (x + w, y + h)
  , seg env t c (x + w, y + h) (x, y + h), seg env t c (x, y + h) (x, y) ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wA = max 0 (wA w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wA = min (nn - 1) (wA w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wB = max 0 (wB w - 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wB = min (nn - 1) (wB w + 1) }
  EventKey (Char ch) Down _ _ | ch `elem` "mM" -> w { wMax = not (wMax w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. ⊕ の表はなぜ「階段」になる？ ⊗ の表はなぜ「斜めのしま」？ 色を見て説明しよう
-- 2. M で max 流儀にすると何が変わる？ 変わらない法則は？（分配法則・べき等性）
-- 3. 表を 0 〜 nn-1 でなく −5 〜 5 にして、負の数でも法則が成り立つか確かめよう
-- 4. (a ⊕ b)^{⊗3} = a^{⊗3} ⊕ b^{⊗3} も成り立つ？ lawLines に行を 1 つ足して確かめよう


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）→ gloss の座標（中央が原点、y は上向き）
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js と同じ座標で返す
mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

-- 中央揃えの文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = let (gx, gy) = pt env x y in labelC gx gy s c str

-- 左上が (x, y) の塗りつぶした長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 線分（太さ wd）。太さ 1 以下は細い線で描く
seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env wd c (x1', y1') (x2', y2')
  | wd <= 1 = Color c (Line [a, b])
  | otherwise = Color c (thickLine wd a b)
  where a = pt env x1' y1'
        b = pt env x2' y2'

-- 塗りつぶした円（直径 d）
dotP :: Env -> Float -> Float -> Float -> Color -> Picture
dotP env x y d c = Color c (at (pt env x y) (disc (d / 2)))

-- 暗いふちのある円（直径 d）
dotO :: Env -> Float -> Float -> Float -> Color -> Picture
dotO env x y d c = Pictures [dotP env x y (d + 2) (hsb 0 0 10), dotP env x y (d - 2) c]

bgCol :: Color
bgCol = hsb 190 55 10

-- 小数 n 桁の文字列（p5.js の nf）
nf :: Int -> Float -> String
nf n' v = noMinusZero (showFFloat (Just n') v "")

nfd :: Int -> Double -> String
nfd n' v = noMinusZero (showFFloat (Just n') v "")

-- "-0.00" は "0.00" と書く
noMinusZero :: String -> String
noMinusZero str = if all (`elem` "-0.") str then dropWhile (== '-') str else str
