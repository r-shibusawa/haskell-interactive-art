-- ============================================
-- 09 決定木と比較ソートの下界
-- 「比べるだけ」で並べる方法は、どれも「a ≤ b ? の質問を重ねる木」として描ける（決定木）
-- 葉は答え（並べ方）。n 個の並べ方は n! 通りあるので、葉は n! 枚以上必要
-- 高さ h の二分木の葉は最大 2^h 枚 → h ≥ log2(n!) ≈ n log2 n。どんな比較ソートも Ω(n log n) 回は比べる
-- スペース：ランダムな入力で木をたどる　1：挿入ソートの木　2：マージソートの木　3・4：n = 3 / 4
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt)
import qualified Data.IntSet as IS
import Data.List (delete, foldl', intercalate, sortOn)
import Numeric (showFFloat)

-- 組み立て中の木：質問、「≤」の枝、「>」の枝、葉の答え
data Tree = Tree
  { tQ    :: !(Maybe (Int, Int))
  , tLe   :: !(Maybe Tree)
  , tGt   :: !(Maybe Tree)
  , tLeaf :: !String
  }

-- 座標を決めた木。nId は根が 1、子が 2i・2i+1 の番号
data Node = Node
  { nId   :: !Int
  , nQ    :: !(Maybe (Int, Int))
  , nLeaf :: !String
  , nKids :: ![(Bool, Node)]   -- True = 「≤」の枝
  , nDep  :: !Int
  , nSlot :: !Float
  }

data World = World
  { wN     :: !Int
  , wAlgo  :: !Int
  , wTree  :: !Node
  , wLeafs :: !Int
  , wPath  :: ![Int]      -- たどるノードの番号
  , wInput :: ![Int]
  , wT     :: !Float
  }

main :: IO ()
main = runSketch (sketch "10-09 Decision Tree" (build 3 1))
  { skBack  = hsb 200 25 9
  , skStep  = \_ _ w -> pure w { wT = min 1000 (wT w + 0.04) }
  , skDraw  = drawW
  , skEvent = onEvent
  }

perms :: [Int] -> [[Int]]
perms [] = [[]]
perms xs = [x : p | x <- xs, p <- perms (delete x xs)]

-- 比べた記録：(i, j, vals[i] ≤ vals[j])
type Cmp = (Int, Int, Bool)

-- 比べた記録を残しながら並べる。結果は「もとの何番目か」の列
sortWithLog :: Int -> [Int] -> ([Cmp], [Int])
sortWithLog algo vals
  | algo == 1 = insertionLog
  | otherwise = mergeLog [0 .. length vals - 1]
  where
    val i = case drop i vals of
      (v : _) -> v
      [] -> 0
    less i j = (i, j, val i <= val j)
    -- 挿入ソート：並んだ山（大きい順に持つ）へ key を後ろから差し込む
    insertionLog =
      let step (logs, sortedRev) key =
            let (l, s') = insertKey key sortedRev
            in (logs ++ l, s')
          (cs, rev) = foldl' step ([], []) [0 .. length vals - 1]
      in (cs, reverse rev)
    insertKey key [] = ([], [key])
    insertKey key (x : xs) =
      let c@(_, _, r) = less x key
      in if r
           then ([c], key : x : xs)
           else let (l, s') = insertKey key xs in (c : l, x : s')
    -- マージソート
    mergeLog arr
      | length arr <= 1 = ([], arr)
      | otherwise =
          let (a, b) = splitAt (length arr `div` 2) arr
              (l1, ls) = mergeLog a
              (l2, rs) = mergeLog b
              (l3, out) = mergeTwo ls rs
          in (l1 ++ l2 ++ l3, out)
    mergeTwo [] rs = ([], rs)
    mergeTwo ls [] = ([], ls)
    mergeTwo (x : xs) (y : ys) =
      let c@(_, _, r) = less x y
      in if r
           then let (l, o) = mergeTwo xs (y : ys) in (c : l, x : o)
           else let (l, o) = mergeTwo (x : xs) ys in (c : l, y : o)

leafName :: [Int] -> String
leafName order = intercalate "<=" (map (\i -> [letter i]) order)

letter :: Int -> Char
letter i = case drop i "abcd" of
  (c : _) -> c
  [] -> '?'

emptyTree :: Tree
emptyTree = Tree Nothing Nothing Nothing ""

-- 1 つの入力の記録を木に書きこむ
addPath :: [Cmp] -> String -> Tree -> Tree
addPath [] leaf t = t { tLeaf = leaf }
addPath ((i, j, r) : rest) leaf t =
  let child c = addPath rest leaf (maybe emptyTree id c)
  in if r
       then t { tQ = Just (i, j), tLe = Just (child (tLe t)) }
       else t { tQ = Just (i, j), tGt = Just (child (tGt t)) }

-- 葉に左から番号をふり、親は子の平均の位置に置く
place :: Int -> Int -> Int -> Tree -> (Node, Int)
place ident depth cnt t =
  let kids0 = [(True, 2 * ident, k) | Just k <- [tLe t]] ++ [(False, 2 * ident + 1, k) | Just k <- [tGt t]]
  in if null kids0
       then (Node ident (tQ t) (tLeaf t) [] depth (fromIntegral cnt), cnt + 1)
       else
         let go (acc, c) (isLe, i, k) =
               let (nd, c') = place i (depth + 1) c k
               in (acc ++ [(isLe, nd)], c')
             (kids, cnt') = foldl' go ([], cnt) kids0
             slot = sum (map (nSlot . snd) kids) / fromIntegral (length kids)
         in (Node ident (tQ t) "" kids depth slot, cnt')

build :: Int -> Int -> World
build n algo =
  let base = [1 .. n]
      tree = foldl' (\t p -> let (cs, order) = sortWithLog algo p in addPath cs (leafName order) t)
               emptyTree (perms base)
      (root, total) = place 1 0 0 tree
  in World n algo root total [] [] 0

treeHeight :: Node -> Int
treeHeight nd = case nKids nd of
  [] -> 0
  ks -> 1 + foldl' max 0 (map (treeHeight . snd) ks)

-- 入力をランダムに並べて、根から葉までたどる
runRandom :: World -> IO World
runRandom w = do
  keys <- mapM (const (randomRIO (0, 1 :: Float))) [1 .. wN w]
  let input = map snd (sortOn fst (zip keys [1 .. wN w]))
      val i = case drop i input of
        (v : _) -> v
        [] -> 0
      walk nd = nId nd : case nQ nd of
        Just (i, j) ->
          let want = val i <= val j
          in case [k | (isLe, k) <- nKids nd, isLe == want] of
               (k : _) -> walk k
               [] -> []
        Nothing -> []
  pure w { wInput = input, wPath = walk (wTree w), wT = 0 }

drawW :: Env -> World -> Picture
drawW env w =
  let n = wN w
      root = wTree w
      h = treeHeight root
      t = 150
      b = envH env - (if n == 4 then 165 else 110)
      l = 50
      r = envW env - 50
      xOf nd = l + (nSlot nd + 0.5) / fromIntegral (max 1 (wLeafs w)) * (r - l)
      yOf nd = t + fromIntegral (nDep nd) * (b - t) / fromIntegral (max 1 h)
      shown = min (length (wPath w)) (floor (wT w) + 1)
      onPath = IS.fromList (take shown (wPath w))
      isOn nd = IS.member (nId nd) onPath
      edges nd = concatMap (edge nd) (nKids nd)
      edge nd (isLe, c) =
        let hot = isOn c
            colr = if isOn nd && hot then hsb 45 90 100 else hsb 0 0 35
            xm = (xOf nd + xOf c) / 2 + (if isLe then -14 else 12)
            ym = (yOf nd + yOf c) / 2
        in seg env colr (if hot then 3 else 1.5) (xOf nd, yOf nd) (xOf c, yOf c)
             : txtC env xm ym 14 (hsb 0 0 55) (if isLe then "<=" else ">")
             : edges c
      nodes nd = node nd : concatMap (nodes . snd) (nKids nd)
      node nd =
        let x = xOf nd
            y = yOf nd
            on = isOn nd
        in case nQ nd of
             Just (i, j) ->
               let s = if n == 3 then 46 else 36
               in Pictures
                    [ box env (if on then hsb 45 90 100 else hsb 210 40 45) (x - s / 2) (y - 13) s 26
                    , txtC env x y 14 (hsb 0 0 (if on then 5 else 95)) [letter i, ':', letter j] ]
             Nothing
               | n == 3 -> Pictures
                   [ box env (if on then hsb 140 80 95 else hsb 140 40 45) (x - 44) (y - 13) 88 26
                   , txtC env x y 14 (hsb 0 0 (if on then 5 else 95)) (nLeaf nd) ]
               | otherwise ->     -- n = 4 の葉は多いので、文字を縦にする
                   at (pt env (x, y))
                     (Rotate 90 (label 6 (-7) 14 (if on then hsb 140 80 95 else hsb 140 40 60) (nLeaf nd)))
      lg = sum (map (logBase 2 . fromIntegral) [2 .. n]) :: Float
      help = hsb 0 0 65
      inputLine
        | null (wInput w) = "Worst case for any comparison sort = compares to the deepest leaf (= height)"
        | otherwise = "Input " ++ (if n == 4 then "a,b,c,d" else "a,b,c") ++ " = "
                        ++ intercalate ", " (map show (wInput w))
                        ++ "   compares " ++ show (length (wPath w) - 1)
  in Pictures
       [ Pictures (edges root)
       , Pictures (nodes root)
       , txt env 20 40 22 (hsb 0 0 100)
           ("Decision tree (" ++ (if wAlgo w == 1 then "insertion" else "merge") ++ ", n = " ++ show n
             ++ ")  leaves " ++ show (wLeafs w) ++ " = " ++ show n ++ "!  height " ++ show h
             ++ " >= log2(" ++ show n ++ "!) = " ++ showFFloat (Just 2) lg "")
       , txt env 20 68 15 help "Space: follow a random input   1 insertion sort   2 merge sort   3 4: change n"
       , txt env 20 92 15 help ("a:b asks \"a <= b ?\".  " ++ inputLine)
       , txt env 20 (envH env - 16) 14 (hsb 0 0 55)
           "n = 10 needs log2(10!) = 21.8 compares, n = 100 about 525. Merge sort and heapsort nearly reach this limit"
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> runRandom w
  EventKey (Char c) Down _ _
    | c `elem` "12" -> pure (build (wN w) (digitToInt c))
    | c `elem` "34" -> pure (build (digitToInt c) (wAlgo w))
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. n = 3 の挿入ソートで、比較が 2 回で済む入力と 3 回かかる入力を見つける
-- 2. n = 4 でマージソートと挿入ソートの木の高さ（最悪の比較回数）を比べる
-- 3. 4! = 24 枚の葉を持つ二分木の高さは最低いくつ？ 実際の木と比べよう

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
