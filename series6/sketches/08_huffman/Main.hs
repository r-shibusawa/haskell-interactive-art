-- ============================================
-- 08 ハフマン符号（よく出る文字は短く）
-- 文章の中でよく出る文字に短いビット列、めったに出ない文字に長いビット列を割り当てる
-- 木を下から組み立てると、自動的にいちばん短い符号ができる（ZIP や JPEG の中で使われている）
-- キーボードで文章を打つ / 1〜3 でお手本の文章 / Backspace で消す
-- ============================================
module Main (main) where

import Art
import Data.Char (isDigit, toUpper)
import Data.List (intercalate, nub, sort, sortOn)
import qualified Data.Map.Strict as M
import Numeric (showFFloat)

-- ハフマン木：葉は文字と回数、枝は合計の回数と左右の子
data Tree = Leaf !Char !Int | Node !Int Tree Tree

data World = World
  { wText  :: !String
  , wTree  :: !(Maybe Tree)
  , wCodes :: !(M.Map Char String)
  }

maxText :: Int
maxText = 60              -- ★ 打てる文字数

main :: IO ()
main = runSketch (sketch "06-08 Huffman Code" (withText "HELLO WORLD THIS IS A TEST OF HUFFMAN CODING"))
  { skBack  = hsb 230 40 10
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

withText :: String -> World
withText s =
  let t = buildTree s
  in World s t (maybe M.empty assign t)

countOf :: Tree -> Int
countOf (Leaf _ c) = c
countOf (Node c _ _) = c

-- ★ ハフマン木をつくる
buildTree :: String -> Maybe Tree
buildTree s = merge leaves
  where
    -- 1. 文字ごとの出現回数
    count ch = length (filter (== ch) s)
    -- 2. 葉のリスト（p5.js の Object.keys と同じく、数字を先に、あとは出てきた順）
    keys = sort (filter isDigit (nub s)) ++ filter (not . isDigit) (nub s)
    leaves = [Leaf ch (count ch) | ch <- keys]
    -- 3. いちばん少ない 2 つを取り出してまとめる、をくり返す
    merge nodes = case sortOn countOf nodes of
      (a : b : rest) -> merge (rest ++ [Node (countOf a + countOf b) a b])
      [t] -> Just t
      [] -> Nothing

-- 4. 木をたどって符号を決める（左 = 0、右 = 1）
assign :: Tree -> M.Map Char String
assign = go ""
  where
    go code (Leaf ch _) = M.singleton ch (if null code then "0" else code)
    go code (Node _ l r) = M.union (go (code ++ "0") l) (go (code ++ "1") r)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ maybe Blank (drawTree env) (wTree w)
  , txt env 40 92 15 dim "Char"
  , txt env 104 92 15 dim "Count"
  , txt env 180 92 15 dim "Code"
  , Pictures (zipWith row [114, 136 ..] (takeWhile' entries))
  , txt env 20 40 18 (hsb 0 0 100) ("Type a sentence (1-3: samples)   Length " ++ show (length s))
  , txt env 20 68 15 dim
      ("ASCII: " ++ show fixed ++ " bits -> Huffman: " ++ show totalBits ++ " bits ("
       ++ nf 0 (100 * fromIntegral totalBits / fromIntegral (max 1 fixed)) ++ " %)   Common chars get short codes")
  , txt env 40 (envH env - 64) 14 (hsba 45 90 100 0.8) (take 80 shown)
  , txt env 40 (envH env - 42) 14 (hsba 45 90 100 0.8) (drop 80 shown ++ (if length encoded > 160 then " ..." else ""))
  ]
  where
    dim = hsb 0 0 60
    s = wText w
    codes = wCodes w
    freq = M.fromListWith (+) [(c, 1 :: Int) | c <- s]
    entries = sortOn (\(ch, code) -> (length code, ch)) (M.toList codes)
    takeWhile' = take (max 0 (floor ((envH env - 120 - 114) / 22) + 1))
    totalBits = sum [M.findWithDefault 0 ch freq * length code | (ch, code) <- entries]
    fixed = length s * 8
    row :: Float -> (Char, String) -> Picture
    row y (ch, code) = Pictures
      [ txt env 40 y 15 (hsb 0 0 100) [showChar' ch]
      , txt env 112 y 15 (hsb 0 0 100) (pad2 (M.findWithDefault 0 ch freq))
      , txt env 180 y 15 (hsb 45 90 100) code
      ]
    -- 符号化した文章
    encoded = intercalate " " [M.findWithDefault "" c codes | c <- s]
    shown = take 160 encoded

-- 木を描く。葉を左から順に並べ、枝は 2 つの子のまん中に置く
drawTree :: Env -> Tree -> Picture
drawTree env tree = pic
  where
    (_, _, pic) = go 0 0 tree
    nLeaves = leaves tree
    span' = envW env * 0.66
    dx = if nLeaves <= 1 then 0 else min 70 (span' / fromIntegral (nLeaves - 1))
    x0 = envW env * 0.635 - dx * fromIntegral (nLeaves - 1) / 2
    stepY = min 70 ((envH env - 110 - 100) / fromIntegral (max 1 (depth tree)))
    yOf :: Int -> Float
    yOf d = 110 + fromIntegral d * stepY

    -- 戻り値は（次の葉の番号、自分の x、絵）
    go :: Int -> Int -> Tree -> (Int, Float, Picture)
    go d i (Leaf ch _) =
      let x = x0 + fromIntegral i * dx
      in (i + 1, x, node x (yOf d) 15 (hsb 45 80 100) [showChar' ch])
    go d i (Node c l r) =
      let (i1, xl, pl) = go (d + 1) i l
          (i2, xr, pr) = go (d + 1) i1 r
          x = (xl + xr) / 2
          y = yOf d
          yc = yOf (d + 1)
          edges = Color (hsb 0 0 50) (Pictures
            [ thickLine 1.5 (toG env x y) (toG env xl yc)
            , thickLine 1.5 (toG env x y) (toG env xr yc) ])
          marks = Pictures
            [ txt env ((x + xl) / 2 - 14) ((y + yc) / 2) 14 (hsb 0 0 60) "0"
            , txt env ((x + xr) / 2 + 6) ((y + yc) / 2) 14 (hsb 0 0 60) "1" ]
      in (i2, x, Pictures [edges, marks, pl, pr, node x y 13 (hsb 170 80 100) (show c)])

    node x y r c str = Pictures
      [ Color c (at (toG env x y) (disc r))
      , txtC env x y 14 (hsb 0 0 10) str ]

leaves :: Tree -> Int
leaves (Leaf _ _) = 1
leaves (Node _ l r) = leaves l + leaves r

depth :: Tree -> Int
depth (Leaf _ _) = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char '1') Down _ _ -> withText "AAAAAAAABBBBCCD"
  EventKey (Char '2') Down _ _ -> withText "ABCDEFGH"
  EventKey (Char '3') Down _ _ -> withText "MISSISSIPPI RIVER"
  EventKey (Char '\b') Down _ _ -> backspace w
  EventKey (SpecialKey KeyBackspace) Down _ _ -> backspace w
  EventKey (SpecialKey KeyDelete) Down _ _ -> backspace w   -- Mac の delete キー
  EventKey (SpecialKey KeySpace) Down _ _ -> typed ' ' w
  EventKey (Char c) Down _ _ | c >= ' ' && c <= '~' -> typed c w
  _ -> w

typed :: Char -> World -> World
typed c w
  | length (wText w) < maxText = withText (wText w ++ [toUpper c])
  | otherwise = w

backspace :: World -> World
backspace w
  | length (wText w) > 2 = withText (take (length (wText w) - 1) (wText w))
  | otherwise = w

-- ---------- やってみよう ----------
-- 1. お手本 1（A が多い）と 2（全部同じ回数）で圧縮率を比べよう。偏りがあるほど縮む
-- 2. 符号を見ると「どの符号も他の符号の先頭にならない」（だから区切りなしで読める）
-- 3. 01 のエントロピーと比べてみよう。ハフマンの平均ビット数はエントロピーに近づく

-- ---------- 道具 ----------

-- 空白は見えないので _ で表す
showChar' :: Char -> Char
showChar' ch = if ch == ' ' then '_' else ch

-- 2 桁にそろえる
pad2 :: Int -> String
pad2 n = if n < 10 then '0' : show n else show n

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 文字（中央が x, y）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = let (gx, gy) = toG env x y in labelC gx gy size c s

-- 小数を d 桁で文字にする（p5.js の nf）
nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
