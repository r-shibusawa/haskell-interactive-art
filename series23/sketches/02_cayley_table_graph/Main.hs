-- ============================================
-- 02 ケイリー表とケイリーグラフ
-- 群を「表」と「グラフ」で見る。グラフの点 = 元、矢印 = 生成元を掛ける（x → x·g）
--   生成元の矢印だけで全部の点に行ける = 生成元で群がつくれる
-- 1 = Zn（時計の足し算）/ 2 = Dn（二面体群）/ 3 = Z2×Z2（クラインの四元群）/ 4 = S3（3 個の並べかえ）
-- ↑↓ = n / 表のマスや点にマウスを乗せる = 掛け算を表示 / G = 生成元の矢印を切りかえ
-- ============================================
module Main (main) where

import Art
import Data.Array
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

-- 群：元の名前の配列と、掛け算の表 mul (i, j) = 「i をしてから j」の番号
data Grp = Grp
  { gName :: String
  , gEls  :: !(Array Int String)
  , gMul  :: !(Array (Int, Int) Int)
  , gGens :: ![Int]
  , gPos  :: !(Array Int Point)
  , gComm :: !Bool
  }

data World = World
  { wNo   :: !Int           -- 群の番号
  , wN    :: !Int           -- ★ Zn, Dn の n
  , wG    :: !Grp           -- 今の群
  , wShow :: !(Bool, Bool)  -- 生成元の矢印を表示するか
  }

genColors :: [(Int, Int, Int)]
genColors = [(230, 70, 90), (40, 130, 230)]

main :: IO ()
main = runSketch (sketch "23-02 Cayley Table and Graph" (World 2 4 (build 2 4) (True, True)))
  { skBack  = rgb255 18 20 28
  , skDraw  = \env w -> drawWorld (toP5 (envMouse env)) w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- 元の数・名前・掛け算・生成元・位置から群をつくる
mkGroup :: String -> [String] -> (Int -> Int -> Int) -> [Int] -> [Point] -> Grp
mkGroup nm els mul gens pos =
  let nn = length els
      r = [0 .. nn - 1]
      tbl = array ((0, 0), (nn - 1, nn - 1)) [((i, j), mul i j) | i <- r, j <- r]
  in Grp nm (listArray (0, nn - 1) els) tbl gens (listArray (0, nn - 1) pos)
       (and [tbl ! (i, j) == tbl ! (j, i) | i <- r, j <- r])

build :: Int -> Int -> Grp
build 1 n = mkGroup ("Z" ++ show n ++ " (" ++ show n ++ "-hour clock)") (map show [0 .. n - 1])
              (\i j -> (i + j) `mod` n) [1] [ringPos i n 1 False | i <- [0 .. n - 1]]
build 2 n =
  let els = [(k, f) | f <- [0, 1 :: Int], k <- [0 .. n - 1]]
      arr = listArray (0, 2 * n - 1) els :: Array Int (Int, Int)
      nm (k, f) | k == 0 && f == 0 = "e"
                | otherwise = (if k /= 0 then "r" ++ (if k > 1 then show k else "") else "") ++ (if f /= 0 then "s" else "")
      mul i j = let (xk, xf) = arr ! i
                    (yk, yf) = arr ! j
                in (yk + (if yf /= 0 then -xk else xk)) `mod` n + ((xf + yf) `mod` 2) * n
  in mkGroup ("D" ++ show n ++ " (symmetries of a regular " ++ show n ++ "-gon)") (map nm els) mul [1, n]
       [ringPos k n (if f /= 0 then 0.5 else 1) (f /= 0) | (k, f) <- els]
build 3 _ =
  let els = [(0, 0), (1, 0), (0, 1), (1, 1)] :: [(Int, Int)]
      arr = listArray (0, 3) els :: Array Int (Int, Int)
      mul i j = let (a, b) = arr ! i
                    (c, d) = arr ! j
                in (a + c) `mod` 2 + ((b + d) `mod` 2) * 2
  in mkGroup "Z2xZ2 (Klein four-group)" [ "(" ++ show a ++ "," ++ show b ++ ")" | (a, b) <- els ] mul [1, 2]
       [(-0.6, -0.6), (0.6, -0.6), (-0.6, 0.6), (0.6, 0.6)]
build _ _ =
  let perms = [[0, 1, 2], [1, 2, 0], [2, 0, 1], [1, 0, 2], [0, 2, 1], [2, 1, 0]] :: [[Int]]
      arr = listArray (0, 5) perms :: Array Int [Int]
      findP p = fromMaybe 0 (elemIndex p perms)
      mul i j = findP [arr ! j !! (arr ! i !! x) | x <- [0, 1, 2]]    -- i をしてから j
  in mkGroup "S3 (arrangements of 3 things)" (map cycleName perms) mul [1, 3]
       [ringPos (i `mod` 3) 3 (if i < 3 then 1 else 0.5) (i >= 3) | i <- [0 .. 5]]

ringPos :: Int -> Int -> Float -> Bool -> Point
ringPos k m r inner =
  let a = -pi / 2 + (if inner then -1 else 1) * fromIntegral k * tau / fromIntegral m
  in (r * cos a, r * sin a)

-- 3 個の並べかえを巡回表記に
cycleName :: [Int] -> String
cycleName p =
  let go seen i
        | i >= 3 = []
        | i `elem` seen || p !! i == i = go seen (i + 1)
        | otherwise =
            let c = takeCycle i i True
            in ("(" ++ unwords (map (show . (+ 1)) c) ++ ")") ++ go (seen ++ c) (i + 1)
      takeCycle s j first
        | not first && j == s = []
        | otherwise = j : takeCycle s (p !! j) False
      out = go [] 0
  in if null out then "e" else out

drawWorld :: Point -> World -> Picture
drawWorld mouse@(mx, my) w = Pictures (table ++ arrows ++ path ++ nodes ++ hud)
  where
    g = wG w
    nn = snd (bounds (gEls g)) + 1
    r = [0 .. nn - 1]
    mul i j = gMul g ! (i, j)
    -- 表
    x0 = 30 :: Float
    y0 = 110 :: Float
    cs = min (width * 0.5 / fromIntegral (nn + 1)) ((height - 130) / fromIntegral (nn + 1))
    cj = floor ((mx - x0) / cs) - 1 :: Int
    ci = floor ((my - y0) / cs) - 1 :: Int
    hoverCell = if ci >= 0 && ci < nn && cj >= 0 && cj < nn then Just (ci, cj) else Nothing
    fit s = max 14 (min (min 18 (cs * 0.34)) (cs * 0.92 / (0.72 * fromIntegral (length s))))
    table = concat
      [ cell i j (x0 + fromIntegral (j + 1) * cs) (y0 + fromIntegral (i + 1) * cs)
      | i <- [-1 .. nn - 1], j <- [-1 .. nn - 1], i >= 0 || j >= 0 ]
    cell i j x y
      | i < 0 || j < 0 =
          let s = gEls g ! (if i < 0 then j else i)
          in [txtC (x + cs / 2) (y + cs / 2) (fit s) (gray (200 / 255)) s]
      | otherwise =
          let p = mul i j
              over = hoverCell == Just (i, j)
              s = gEls g ! p
          in [ box (x + 0.75) (y + 0.75) (cs - 1.5) (cs - 1.5) (hsb (fromIntegral p * 360 / fromIntegral nn) 55 (if over then 100 else 80))
             , txtC (x + cs / 2) (y + cs / 2) (fit s) (gray (15 / 255)) s ]
    -- グラフ
    gx = width * 0.74
    gy = height * 0.52
    gr = min (width * 0.2) (height * 0.32)
    nodeP i = let (a, b) = gPos g ! i in (gx + a * gr, gy + b * gr)
    hoverNode = foldl (\acc i -> if dist2 mouse (nodeP i) < 24 then i else acc) (-1) r
    focus = maybe hoverNode fst hoverCell
    arrows = concat
      [ [ genArrow (nodeP i) (nodeP j) col (if focus < 0 || focus == i then 230 else 50) (mul j gi == i)
        | i <- r, let j = mul i gi ]
      | (c, gi, col) <- zip3 [0 :: Int ..] (gGens g) genColors, shown c ]
    shown c = (if c == 0 then fst else snd) (wShow w)
    -- 掛け算の経路（表のマスに乗せたとき：i → i·j）
    path = case hoverCell of
      Just (i, j) -> [Color (rgb255 255 220 90) (thickLine 5 (pt (nodeP i)) (pt (nodeP (mul i j))))]
      Nothing -> []
    nodes = concat
      [ [ Color (case hoverCell of
                   Just (a, b) | a == i || mul a b == i -> white
                   _ -> gray (18 / 255)) (at (pt p) (disc 21.5))
        , Color (hsb (fromIntegral i * 360 / fromIntegral nn) 55 85) (at (pt p) (disc 19))
        , txtC (fst p) (snd p) 14 (if length s > 3 then white else gray (15 / 255)) s ]
      | i <- r, let p = nodeP i, let s = gEls g ! i ]
    -- 文字
    tx = width * 0.56
    hud =
      [ txt 20 36 18 white ("1: Zn   2: Dn   3: Z2xZ2   4: S3   Up/Down: n = " ++ show (wN w) ++ "   G: arrows   Mouse: hover a cell or point")
      , txt 20 62 15 (gray (170 / 255)) (gName g ++ "   order " ++ show nn ++ "  |  Row a, column b = 'do a, then b'. Same color = same element")
      , txt tx 136 14 (gray (170 / 255)) (if gComm g then "Commutative (table is symmetric across the diagonal)" else "Non-commutative (table is not symmetric)")
      ] ++
      [ let (a, b, c') = col
        in txt (tx + fromIntegral c * 170) (height - 60) 16 (rgb255 a b c')
             ("--> *" ++ (gEls g ! gi) ++ (if shown c then "" else " (hidden)"))
      | (c, gi, col) <- zip3 [0 :: Int ..] (gGens g) genColors ] ++
      (case hoverCell of
         Just (a, b) -> [txt tx 110 20 white ((gEls g ! a) ++ " * " ++ (gEls g ! b) ++ " = " ++ (gEls g ! mul a b))]
         Nothing | hoverNode >= 0 ->
                     [txt tx 110 20 white ("Arrows from " ++ (gEls g ! hoverNode) ++ " = " ++ (gEls g ! hoverNode) ++ " * generator")]
                 | otherwise -> [])

-- 矢印（両向きなら線だけ）
genArrow :: Point -> Point -> (Int, Int, Int) -> Int -> Bool -> Picture
genArrow a@(ax, ay) b@(bx, by) (cr, cg, cb) al both
  | dist2 a b < 1 = Blank
  | both = Color col (thickLine 3 (pt s) (pt e))
  | otherwise = Color col (Pictures
      [ thickLine 3 (pt s) (pt e)
      , Polygon (map pt [ e
                        , (ex - 13 * cos (ang - 0.4), ey - 13 * sin (ang - 0.4))
                        , (ex - 13 * cos (ang + 0.4), ey - 13 * sin (ang + 0.4)) ]) ])
  where
    col = rgba255 cr cg cb al
    ang = atan2 (by - ay) (bx - ax)
    off = if both then 0 else 6         -- 行きと帰りが重ならないように少しずらす
    ox = -sin ang * off
    oy = cos ang * off
    s = (ax + cos ang * 22 + ox, ay + sin ang * 22 + oy)
    e@(ex, ey) = (bx - cos ang * 22 + ox, by - sin ang * 22 + oy)

onEvent :: Event -> World -> World
onEvent (EventKey k Down _ _) w = case k of
  Char c | c >= '1' && c <= '4' -> rebuild w { wNo = fromEnum c - fromEnum '0' }
  Char c | c == 'g' || c == 'G' -> w { wShow = case wShow w of
                                                 (True, True) -> (True, False)
                                                 (True, _) -> (False, True)
                                                 _ -> (True, True) }
  SpecialKey KeyUp -> rebuild w { wN = min 10 (wN w + 1) }
  SpecialKey KeyDown -> rebuild w { wN = max 2 (wN w - 1) }
  _ -> w
  where rebuild v = v { wG = build (wNo v) (wN v) }
onEvent _ w = w

-- ---------- やってみよう ----------
-- 1. Z6 と S3 はどちらも位数 6。表を比べると？（Z6 は可換、S3 は非可換 → 違う群）
-- 2. D3 と S3 のグラフの形を比べよう。同じ形 = 同型（三角形の対称性 = 3 頂点の並べかえ）
-- 3. Z4 と Z2×Z2 はどちらも位数 4。何が違う？（表の対角線：a·a）
-- 4. G で生成元を 1 つだけにすると、Dn では全部の点に行けない（回転だけの輪）
-- 5. Zn の生成元を 1 から 2 に変えると？（n が偶数だと半分にしか行けない）build の [1] を書きかえよう

-- ---------- 道具 ----------
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Point -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

-- gloss の座標 → p5 の座標（マウス用）
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y = labelC (x - width / 2) (height / 2 - y)

box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)
