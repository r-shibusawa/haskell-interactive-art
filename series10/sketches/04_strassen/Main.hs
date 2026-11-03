-- ============================================
-- 04 行列のかけ算とシュトラッセン法
-- n×n の行列の積は、ふつうに計算すると n^3 回のかけ算。
-- 半分ずつ 4 つのブロックに分けると 8 回の小さな積。シュトラッセン（1969）は工夫して 7 回にした
-- 1 回へらすだけで、くり返すと n^2.81 に。グラフで差が広がっていくのを見よう
-- ←→：行列の大きさ n（2 のべき乗）　1：ふつう　2：分割統治（8 回）　3：シュトラッセン（7 回）
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt)
import Data.List (intercalate)

data World = World
  { wK      :: !Int     -- ★ n = 2^k
  , wMethod :: !Int
  }

main :: IO ()
main = runSketch (sketch "10-04 Matrix Multiplication" (World 3 3))
  { skBack  = hsb 270 30 10
  , skDraw  = drawW
  , skEvent = onEvent
  }

-- かけ算の回数を再帰で数える（T(n) = 8T(n/2) または 7T(n/2)、ブロックが 1×1 になったら 1 回）
mults :: Int -> Int -> Int
mults sz m
  | m == 1 = sz * sz * sz
  | sz == 1 = 1
  | otherwise = (if m == 2 then 8 else 7) * mults (sz `div` 2) m

log2F :: Int -> Float
log2F v = logBase 2 (fromIntegral v)

drawW :: Env -> World -> Picture
drawW env w =
  let k = wK w
      method = wMethod w
      sz = 2 ^ k :: Int
      wd = envW env
      hg = envH env
      -- 左：再帰の木。深さ d には (分かれ方)^d 個の小さな積がある
      s = min (hg - 220) (wd * 0.42)
      x0 = 60
      y0 = 150
      br = case method of
        1 -> 1
        2 -> 8
        _ -> 7 :: Int
      levels = if method == 1 then 1 else k + 1
      rowH = min 70 ((hg - y0 - 60) / fromIntegral (max 1 levels))
      level d =
        let count = br ^ d :: Int
            size = sz `div` (2 ^ d)
            y = y0 + fromIntegral d * rowH
            cell = max 2 (min 14 ((s - 10) / fromIntegral (min count 60)))
            shown = min count (60 * max 1 (floor ((rowH - 22) / cell)))
            more = if count > shown then "   (" ++ comma shown ++ " shown)" else ""
            title = "depth " ++ show d ++ ": " ++ comma count ++ " products of "
                      ++ show size ++ "x" ++ show size ++ more
            square' i = box env (hsb (fromIntegral ((d * 70 + (i `mod` br) * 30) `mod` 360)) 55 90)
                          (x0 + fromIntegral (i `mod` 60) * cell)
                          (y + 18 + fromIntegral (i `div` 60) * cell)
                          (cell - 1) (cell - 1)
        in Pictures (txt env x0 (y + 14) 14 (hsb 0 0 80) title : map square' [0 .. shown - 1])
      note = if method == 1
               then "Three nested loops: n^3 multiplications"
               else "The last row is 1x1 products = 1 multiplication each. Their count is the answer"
      -- 右上：7 つの積（シュトラッセン）
      rx = x0 + s + 60
      eqs = case method of
        3 -> [ "M1 = (A11 + A22)(B11 + B22)", "M2 = (A21 + A22) B11", "M3 = A11 (B12 - B22)", "M4 = A22 (B21 - B11)"
             , "M5 = (A11 + A12) B22", "M6 = (A21 - A11)(B11 + B12)", "M7 = (A12 - A22)(B21 + B22)"
             , "C11 = M1 + M4 - M5 + M7", "C12 = M3 + M5", "C21 = M2 + M4", "C22 = M1 - M2 + M3 + M6" ]
        2 -> [ "C11 = A11 B11 + A12 B21", "C12 = A11 B12 + A12 B22", "C21 = A21 B11 + A22 B21", "C22 = A21 B12 + A22 B22"
             , "-> 8 smaller products" ]
        _ -> [ "C[i][j] = sum of A[i][k] B[k][j]", "-> three loops i, j, k: n^3 times" ]
      eqLine (i, str) =
        let c = if i < 7 && method == 3 then hsb (fromIntegral i * 45) 60 100 else hsb 0 0 85
        in txt env rx (y0 + fromIntegral i * 24 + 15) 15 c str
      -- 右下：回数のグラフ（対数）
      gx0 = rx
      gy0 = y0 + 300
      gw = wd - rx - 60
      gh = hg - gy0 - 70
      maxV = log2F (mults 1024 1)
      curve m =
        let hue = case m of
              1 -> 0
              2 -> 200
              _ -> 140
            ptsOf :: Int -> (Float, Float)
            ptsOf e = pt env (gx0 + fromIntegral e / 10 * gw, gy0 + gh - log2F (mults (2 ^ e) m) / maxV * gh)
        in Color (hsb hue 70 100) (thickPath (if m == method then 4 else 2) (map ptsOf [0 .. 10]))
      graph
        | gh > 80 = Pictures
            [ seg env (hsb 0 0 35) 1 (gx0, gy0 + gh) (gx0 + gw, gy0 + gh)
            , seg env (hsb 0 0 35) 1 (gx0, gy0) (gx0, gy0 + gh)
            , Pictures (map curve [1, 2, 3 :: Int])
            , seg env (hsb 0 0 70) 1 (gx0 + fromIntegral k / 10 * gw, gy0) (gx0 + fromIntegral k / 10 * gw, gy0 + gh)
            , txt env gx0 (gy0 + gh + 22) 14 (hsb 0 0 60) "y: multiplications (log scale)  x: n = 1, 2, 4 .. 1024"
            , txt env (gx0 + 10) (gy0 + 18) 14 (hsb 0 70 100) "Red: n^3"
            , txt env (gx0 + 10) (gy0 + 36) 14 (hsb 200 70 100) "Blue: 8 splits (still n^3)"
            , txt env (gx0 + 10) (gy0 + 54) 14 (hsb 140 70 100) "Green: Strassen n^2.81"
            ]
        | otherwise = Blank
      name = case method of
        1 -> "standard"
        2 -> "8 splits"
        _ -> "Strassen"
      help = hsb 0 0 65
  in Pictures
       [ Pictures (map level [0 .. levels - 1])
       , txt env x0 (hg - 26) 14 (hsb 0 0 70) note
       , Pictures (map eqLine (zip [0 :: Int ..] eqs))
       , graph
       , txt env 20 40 22 (hsb 0 0 100)
           ("Matrix product  n = " ++ show sz ++ "  " ++ name ++ ": " ++ comma (mults sz method)
             ++ " mults  (n^3 = " ++ comma (sz * sz * sz) ++ ")")
       , txt env 20 68 15 help "Left/Right: change n   1 standard   2 divide & conquer (8)   3 Strassen (7)"
       , txt env 20 92 15 help "T(n) = 7T(n/2) + Theta(n^2) -> Theta(n^log2(7)). More additions, but multiplications dominate"
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wK = min 7 (wK w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wK = max 0 (wK w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> pure w { wMethod = digitToInt c }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. n = 128 で、ふつうとシュトラッセンのかけ算の回数は何倍ちがう？
-- 2. 「8 回」の分割統治が n^3 から速くならない理由を、T(n) = 8T(n/2) から考えよう
-- 3. 実際の計算機では小さいブロックはふつうに計算したほうが速い。mults を「n ≤ 8 ならふつう」に変えよう

-- ---------- 道具：3 けた区切り ----------
comma :: Int -> String
comma v = intercalate "," (reverse (map reverse (chunks (reverse (show v)))))
  where
    chunks [] = []
    chunks s = let (a, b) = splitAt 3 s in a : chunks b

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> (Float, Float) -> (Float, Float)
pt env (x, y) = (gx env x, gy env y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> (Float, Float) -> (Float, Float) -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 文字：txt は左下が (x, y)、txtC は中心が (x, y)
txt, txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
