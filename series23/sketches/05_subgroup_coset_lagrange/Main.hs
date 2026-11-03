-- ============================================
-- 05 部分群・剰余類・ラグランジュの定理
-- 部分群 H（群 G の中で閉じている部分）をずらした gH を剰余類という。剰余類は G を同じ大きさに分ける
--   → |G| = |H| × （剰余類の数）。だから |H| はいつも |G| を割り切る（ラグランジュの定理）
-- 1 = Zn / 2 = Dn / ↑↓ = n / ←→ = 部分群をえらぶ / 点をクリック = その元 g の剰余類 gH を強調 / C = 左剰余類・右剰余類
-- ============================================
module Main (main) where

import Art
import Data.Array
import Data.List (intercalate, nub, sort, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data World = World
  { wNo   :: !Int                        -- 1 = Zn, 2 = Dn
  , wN    :: !Int                        -- ★ n
  , wEls  :: !(Array Int String)         -- 元の名前
  , wMul  :: !(Array (Int, Int) Int)     -- 掛け算（i をしてから j）
  , wPos  :: !(Array Int Point)
  , wSubs :: ![[Int]]                    -- すべての部分群（元の番号の列）
  , wSi   :: !Int                        -- えらんでいる部分群
  , wPick :: !Int                        -- クリックした元（なければ -1）
  , wLeft :: !Bool                       -- 左剰余類か右剰余類か
  }

main :: IO ()
main = runSketch (sketch "23-05 Subgroups, Cosets, Lagrange" (build 2 6 True))
  { skBack  = rgb255 16 22 30
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \env ev w -> pure (onEvent (toP5 (envMouse env)) ev w)
  }

build :: Int -> Int -> Bool -> World
build no n left =
  let (names, mulF, posL)
        | no == 1 =
            ( map show [0 .. n - 1]
            , \i j -> (i + j) `mod` n
            , [ (cos a, sin a) | i <- [0 .. n - 1], let a = -pi / 2 + fromIntegral i * tau / fromIntegral n ] )
        | otherwise =
            let es = [(k, f) | f <- [0, 1 :: Int], k <- [0 .. n - 1]]
                ea = listArray (0, 2 * n - 1) es :: Array Int (Int, Int)
                nm (k, f) | k == 0 && f == 0 = "e"
                          | otherwise = (if k /= 0 then "r" ++ (if k > 1 then show k else "") else "") ++ (if f /= 0 then "s" else "")
                mu i j = let (xk, xf) = ea ! i
                             (yk, yf) = ea ! j
                         in (yk + (if yf /= 0 then -xk else xk)) `mod` n + ((xf + yf) `mod` 2) * n
                po (k, f) = let a = -pi / 2 + fromIntegral k * tau / fromIntegral n
                                r = if f /= 0 then 0.55 else 1
                            in (r * cos a, r * sin a)
            in (map nm es, mu, map po es)
      nn = length names
      r = [0 .. nn - 1]
      tbl = array ((0, 0), (nn - 1, nn - 1)) [((i, j), mulF i j) | i <- r, j <- r]
      -- 部分群をすべて探す：2 つの元 a, b から生成される集合（Zn・Dn の部分群はこれで全部）
      allH = [closure tbl [a, b] | a <- r, b <- [a .. nn - 1]]
      (_, foundRev) = foldl' (\(seen, acc) h -> if Set.member h seen then (seen, acc) else (Set.insert h seen, h : acc))
                             (Set.empty, []) allH
      second h = case h of
        (_ : x : _) -> x
        _ -> -1
      subs = sortOn (\h -> (length h, second h)) (reverse foundRev)
  in World no n (listArray (0, nn - 1) names) tbl (listArray (0, nn - 1) posL) subs (min 1 (length subs - 1)) (-1) left

-- 生成：掛け算で閉じるまで足していく
closure :: Array (Int, Int) Int -> [Int] -> [Int]
closure tbl gens = go (Set.fromList (0 : gens))
  where
    go h =
      let xs = Set.toList h
          h' = foldl' (\s x -> foldl' (\s2 y -> Set.insert (tbl ! (x, y)) s2) s xs) h xs
      in if Set.size h' == Set.size h then xs else go h'

currentH :: World -> [Int]
currentH w = case drop (wSi w) (wSubs w) of
  (h : _) -> h
  [] -> [0]

-- 剰余類：左 gH = { g∘h }。mul(i, j) は「i をしてから j」= j∘i なので g∘h = mul(h, g)
cosetOf :: World -> Bool -> Int -> [Int] -> [Int]
cosetOf w left g h = sort [if left then wMul w ! (x, g) else wMul w ! (g, x) | x <- h]

-- 円の中心と半径
cx, cy, bigR :: Float
cx = width * 0.33
cy = height * 0.55
bigR = min (width * 0.25) (height * 0.34)

nodeP :: World -> Int -> Point
nodeP w i = let (a, b) = wPos w ! i in (cx + a * bigR, cy + b * bigR)

drawWorld :: World -> Picture
drawWorld w = Pictures (links ++ picks ++ nodes ++ hud)
  where
    nn = snd (bounds (wEls w)) + 1
    r = [0 .. nn - 1]
    h = currentH w
    -- 剰余類に分ける
    (cosets, which) = foldl' addCoset ([], Map.empty) r
    addCoset (cs, m) g
      | Map.member g m = (cs, m)
      | otherwise =
          let c = cosetOf w (wLeft w) g h
              idx = length cs
          in (cs ++ [c], foldl' (\mm x -> Map.insert x idx mm) m c)
    count = length cosets
    normal = and [cosetOf w True g h == cosetOf w False g h | g <- r]
    hueC c = fromIntegral c * 360 / fromIntegral count :: Float
    -- 剰余類ごとに点を線でつなぐ
    links =
      [ Color (hsba (hueC c) 60 90 0.5) (thickPath 6 (map (pt . nodeP w) (cc ++ take 1 cc)))
      | (c, cc) <- zip [0 :: Int ..] cosets ]
    -- 選んだ g の剰余類 gH：H の各元 h から gh への矢印
    picks
      | wPick w < 0 = []
      | otherwise =
          [ Color (rgb255 255 220 90) (thickLine 2 (pt (nodeP w x)) (pt (nodeP w t)))
          | x <- h, let t = if wLeft w then wMul w ! (x, wPick w) else wMul w ! (wPick w, x) ]
    nodes = concat
      [ [ discAt x y (23 + (if inH then 2 else 1)) (if inH then white else rgb255 16 22 30)
        , discAt x y (23 - (if inH then 2 else 1)) (hsb (hueC (Map.findWithDefault 0 i which)) 60 90)
        , txtC x y 15 (gray (15 / 255)) (wEls w ! i) ]
      | i <- r, let (x, y) = nodeP w i, let inH = i `elem` h ]
    -- 文字
    px = width * 0.6
    py = 130 :: Float
    joinEls xs = intercalate ", " [wEls w ! i | i <- xs]
    hud =
      [ txt 20 36 18 white ("1: Zn   2: Dn   Up/Down: n = " ++ show (wN w) ++ "   Left/Right: subgroup (" ++ show (wSi w + 1) ++ " / " ++ show (length (wSubs w))
          ++ ")   Click: coset of g   C: " ++ (if wLeft w then "left coset gH" else "right coset Hg"))
      , txt 20 62 15 (gray (170 / 255)) "White rim = subgroup H. Same color = same coset. Cosets never overlap and are all the same size"
      , txt px py 20 white ((if wNo w == 1 then "Z" else "D") ++ show (wN w) ++ "   |G| = " ++ show nn)
      ] ++ linesAt px (py + 36) 21 17 (gray 0.78) (wrapAt 42 ("H = { " ++ joinEls h ++ " }")) ++
      [ txt px (py + 120) 36 (rgb255 255 210 90) (show nn ++ " = " ++ show (length h) ++ " x " ++ show count)
      , txt px (py + 150) 16 (gray 0.78) "|G| = |H| x number of cosets (index [G : H])"
      ] ++ cosetLines ++
      linesAt px (height - 80) 20 16 (if normal then rgb255 120 230 160 else rgb255 255 160 120)
        (wrapAt 42 (if normal then "Normal subgroup: left cosets = right cosets (-> quotient group in 06)"
                              else "Not normal: left and right cosets differ (compare with C)")) ++
      [ txt px (height - 30) 14 (gray (170 / 255))
          ("Subgroup sizes: " ++ intercalate ", " (map show (nub (map length (wSubs w)))) ++ " (all divide " ++ show nn ++ ")") ]
    cosetLines =
      let rows = concat [ map ((,) c) (wrapAt 48 ("{ " ++ joinEls cc ++ " }")) | (c, cc) <- zip [0 :: Int ..] cosets ]
      in [ txt px y 14 (hsb (hueC c) 60 90) s
         | (y, (c, s)) <- zip (takeWhile (< height - 110) [py + 200, py + 222 ..]) rows ]

linesAt :: Float -> Float -> Float -> Float -> Color -> [String] -> [Picture]
linesAt x y dy size c ss = [txt x (y + fromIntegral i * dy) size c s | (i, s) <- zip [0 :: Int ..] ss]

onEvent :: Point -> Event -> World -> World
onEvent mouse ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let nn = snd (bounds (wEls w)) + 1
    in w { wPick = foldl' (\acc i -> if dist2 mouse (nodeP w i) < 24 then i else acc) (-1) [0 .. nn - 1] }
  EventKey (Char '1') Down _ _ -> build 1 (wN w) (wLeft w)
  EventKey (Char '2') Down _ _ -> build 2 (max 3 (min 10 (wN w))) (wLeft w)
  EventKey (Char c) Down _ _ | c == 'c' || c == 'C' -> w { wLeft = not (wLeft w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> build (wNo w) (min (if wNo w == 1 then 24 else 10) (wN w + 1)) (wLeft w)
  EventKey (SpecialKey KeyDown) Down _ _ -> build (wNo w) (max (if wNo w == 1 then 2 else 3) (wN w - 1)) (wLeft w)
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSi = (wSi w + 1) `mod` subCount }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSi = (wSi w + subCount - 1) `mod` subCount }
  _ -> w
  where subCount = max 1 (length (wSubs w))

-- ---------- やってみよう ----------
-- 1. Z12 の部分群を全部見よう。大きさは 1, 2, 3, 4, 6, 12 → 12 の約数だけ。5 の部分群はない
-- 2. D3 で H = { e, s } を選び、C で左と右を比べる。剰余類がずれる = 正規でない
-- 3. Dn の回転だけの部分群 { e, r, r², … } は正規。剰余類は「回転」と「鏡映」の 2 つ
-- 4. 素数 p の Zp には部分群が 2 つ（{0} と全体）しかない。なぜ？

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

discAt :: Float -> Float -> Float -> Color -> Picture
discAt x y r c = Color c (at (pt (x, y)) (disc r))

-- 長い文字列を、1 行 m 文字くらいで折り返す
wrapAt :: Int -> String -> [String]
wrapAt m = go "" . words
  where
    go cur [] = [cur | not (null cur)]
    go "" (x : r) = go x r
    go cur (x : r)
      | length cur + 1 + length x > m = cur : go x r
      | otherwise = go (cur ++ " " ++ x) r
