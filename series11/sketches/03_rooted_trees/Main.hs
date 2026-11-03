-- ============================================
-- 03 根つき木の表し方
-- 子の数が決まっていない木でも、各節点に「いちばん左の子」と「すぐ右の兄弟」の 2 本の矢印だけで表せる
-- （左子・右兄弟表現 left-child, right-sibling）。左が普通の絵、右が同じ木を 2 本の矢印で描いたもの
-- 操作：左の木の節点をクリック = 子を追加 / スペース = ランダムな木 / 1 = 二分木 / 2 = 子が何個でもよい木 / R = 小さな木
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')

maxChildren :: Int
maxChildren = 4   -- ★ ランダムな木の子の数の最大

maxNodes :: Int
maxNodes = 22     -- ★ 節点の数の上限

-- | 節点の番号 → 子の番号のリスト（根は 0 番）
type Kids = IM.IntMap [Int]

-- | 節点の番号 → (横の位置, 深さ)
type Layout = IM.IntMap (Float, Int)

data World = World
  { kids   :: !Kids
  , nextId :: !Int
  , mode   :: !Int
  , msg    :: String
  }

initialWorld :: World
initialWorld = smallTree (World IM.empty 0 2 "Click a node on the left to add a child")

-- | 節点の子のリスト
childrenOf :: Kids -> Int -> [Int]
childrenOf ks x = IM.findWithDefault [] x ks

smallTree :: World -> World
smallTree w =
  w { kids = IM.fromList [(0, [1, 2, 3]), (1, [4, 5]), (3, [6])], nextId = 7 }

-- | 幅優先で節点を増やす
randomTree :: Int -> World -> IO World
randomTree maxK w = go [0] IM.empty 1
  where
    go :: [Int] -> Kids -> Int -> IO World
    go (x : q) ks next | next < maxNodes - maxK = do
      k <- randomRIO (if x == 0 then 2 else 0, maxK)
      let cs = take (min k (maxNodes - next)) [next ..]
      go (q ++ cs) (IM.insert x cs ks) (next + length cs)
    go _ ks next = pure w { kids = ks, nextId = next }

countAll :: Kids -> Int -> Int
countAll ks x = 1 + sum (map (countAll ks) (childrenOf ks x))

-- | 普通の木の位置：葉を左から順に並べ、親は子の真ん中
layoutGeneral :: Kids -> Int -> Int -> (Int, Layout) -> (Float, (Int, Layout))
layoutGeneral ks x d st = case childrenOf ks x of
  [] ->
    let (ctr, out) = st
        v = fi ctr
    in (v, (ctr + 1, IM.insert x (v, d) out))
  cs ->
    let visit (acc, s) c = let (v, s') = layoutGeneral ks c (d + 1) s in (v : acc, s')
        (xs, (c2, o2)) = foldl' visit ([], st) cs
        v = case (xs, reverse xs) of
              (a : _, b : _) -> (a + b) / 2
              _ -> 0
    in (v, (c2, IM.insert x (v, d) o2))

-- | 左子・右兄弟の二分木：left = 最初の子、right = 次の兄弟
data Bin = BNil | Bin !Int Bin Bin

toLCRS :: Kids -> Int -> [Int] -> Bin
toLCRS ks x sib =
  let l = case childrenOf ks x of
            (c : cs) -> toLCRS ks c cs
            [] -> BNil
      r = case sib of
            (s : ss) -> toLCRS ks s ss
            [] -> BNil
  in Bin x l r

-- | 中間順で横位置を決める
layoutBinary :: Bin -> Int -> (Int, Layout) -> (Int, Layout)
layoutBinary BNil _ st = st
layoutBinary (Bin x l r) d st =
  let (c1, o1) = layoutBinary l (d + 1) st
  in layoutBinary r (d + 1) (c1 + 1, IM.insert x (fi c1, d) o1)

-- | 2 つの図の座標
data Scene = Scene
  { genLay :: Layout
  , binLay :: Layout
  , lcrs   :: Bin
  , posG   :: (Float, Int) -> (Float, Float)
  , posB   :: (Float, Int) -> (Float, Float)
  }

makeScene :: Env -> Kids -> Scene
makeScene env ks =
  let (_, (leaves, gen)) = layoutGeneral ks 0 0 (0, IM.empty)
      lc = toLCRS ks 0 []
      (n, bin) = layoutBinary lc 0 (0, IM.empty)
      maxDepth lay = maximum (0 : map snd (IM.elems lay))
      half = envW env / 2
      top0 = 150
      gapG = min 80 ((envH env - 260) / fi (maxDepth gen + 1))
      gapB = min 60 ((envH env - 260) / fi (maxDepth bin + 1))
      pg (x, d) = (30 + ((x + 0.5) / max 1 (fi leaves)) * (half - 60), top0 + fi d * gapG)
      pb (x, d) = (half + 30 + ((x + 0.5) / max 1 (fi n)) * (half - 60), top0 + fi d * gapB)
  in Scene gen bin lc pg pb

-- | マウスの下の節点（左の図でも右の図でもよい）
hoverOf :: Env -> Scene -> Int
hoverOf env sc =
  let m = toP5 env (envMouse env)
      near f lay = [k | (k, p) <- IM.toList lay, dist2 m (f p) < 18]
  in case reverse (near (posG sc) (genLay sc) ++ near (posB sc) (binLay sc)) of
       (k : _) -> k
       [] -> -1

draw :: Env -> World -> Picture
draw env w =
  let sc = makeScene env (kids w)
      half = envW env / 2
      hover = hoverOf env sc
      blue' = rgb255 120 210 255
      orange' = rgb255 255 170 110
      at' lay f k = f (IM.findWithDefault (0, 0) k lay)
      gEdges = [ let (x1, y1) = at' (genLay sc) (posG sc) x
                     (x2, y2) = at' (genLay sc) (posG sc) c
                 in seg env 2 (rgb255 110 120 150) x1 y1 x2 y2
               | (x, cs) <- IM.toList (kids w), c <- cs ]
      bEdges = binEdges (lcrs sc)
      binEdges BNil = []
      binEdges (Bin x l r) =
        let from = at' (binLay sc) (posB sc) x
            edge c sub = case sub of
              BNil -> []
              Bin y _ _ -> arrowL env from (at' (binLay sc) (posB sc) y) c : binEdges sub
        in edge blue' l ++ edge orange' r
      nodes f lay = [let (x, y) = f p in drawNode env x y k hover | (k, p) <- IM.toList lay]
  in Pictures
       ( [ txt env 30 24 26 (gray 0.94) "Rooted Trees: Left-Child, Right-Sibling"
         , txt env 30 60 14 (rgb255 160 170 190) "Click a left node = add a child  Space = random  1 = binary tree  2 = any number of children  R = small tree"
         , txtC env (half / 2) 114 18 (gray 0.78) (if mode w == 1 then "Binary tree (at most left / right)" else "Any number of children")
         , txtC env (half + half / 2) 114 18 (gray 0.78) "Same tree with 2 pointers"
         ]
         ++ gEdges ++ nodes (posG sc) (genLay sc)
         ++ bEdges ++ nodes (posB sc) (binLay sc)
         ++ [ seg env 1 (gray 0.2) half 100 half (envH env - 110)
            , txt env (half + 30) (envH env - 100) 15 blue' "Blue = left-child"
            , txt env (half + 30) (envH env - 76) 15 orange' "Orange = right-sibling"
            , txt env 30 (envH env - 54) 16 (rgb255 255 225 140) (msg w)
            , txt env 30 (envH env - 30) 15 (rgb255 255 225 140)
                (show (countAll (kids w) 0) ++ " nodes: 2 pointers each in either form -> O(n) memory")
            ] )

drawNode :: Env -> Float -> Float -> Int -> Int -> Picture
drawNode env x y k hover =
  let h = k == hover
  in Pictures
       [ circ env x y 15 (if k == 0 then rgb255 150 90 200 else rgb255 50 130 120)
       , circEdge env x y 15 (if h then 4 else 1.5) (if h then rgb255 255 214 90 else gray 0.78)
       , txtC env x y 14 (gray 1) (show k)
       ]

arrowL :: Env -> (Float, Float) -> (Float, Float) -> Color -> Picture
arrowL env (ax, ay) (bx, by) c =
  let ang = atan2 (by - ay) (bx - ax)
      ex = bx - cos ang * 16
      ey = by - sin ang * 16
  in Pictures
       [ seg env 2 c ax ay ex ey
       , tri env c (ex, ey) (ex - cos (ang - 0.4) * 9, ey - sin (ang - 0.4) * 9) (ex - cos (ang + 0.4) * 9, ey - sin (ang + 0.4) * 9)
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (onClick env w)
  EventKey k Down _ _ -> onKey (normKey k) w
  _ -> pure w

onClick :: Env -> World -> World
onClick env w =
  let x = hoverOf env (makeScene env (kids w))
      cs = childrenOf (kids w) x
  in if x < 0 then w
     else if countAll (kids w) 0 >= maxNodes then w { msg = "Cannot add more" }
     else if mode w == 1 && length cs >= 2 then w { msg = "Binary tree: at most 2 children" }
     else w { kids = IM.insert x (cs ++ [nextId w]) (kids w), nextId = nextId w + 1
            , msg = "Added a child to node " ++ show x ++ ": on the right it joins as the last child's right sibling" }

onKey :: Key -> World -> IO World
onKey k w = case k of
  SpecialKey KeySpace -> do
    w1 <- randomTree (if mode w == 1 then 2 else maxChildren) w
    pure w1 { msg = "Random tree" }
  Char '1' -> do
    w1 <- randomTree 2 w
    pure w1 { mode = 1, msg = "Binary tree: left and right children differ (p, left, right: 3 pointers)" }
  Char '2' -> do
    w1 <- randomTree maxChildren w
    pure w1 { mode = 2, msg = "A tree with any number of children" }
  Char 'r' -> pure (smallTree w) { msg = "Small tree" }
  _ -> pure w

main :: IO ()
main = runSketch (sketch "11-03 Rooted Trees" initialWorld)
  { skBack  = rgb255 20 22 26
  , skDraw  = draw
  , skEvent = onEvent
  }

-- ---------- やってみよう ----------
-- 1. 根に子を 6 個つける。左は横に広がるが、右は「橙の矢印が 1 列に並ぶ」だけ
-- 2. 右の図で、ある節点の子を全部たどるには？（青で 1 回下りて、橙で右へ進み続ける）
-- 3. 二分木の配列表現（ヒープ）：節点 i の子は 2i, 2i+1。シリーズ10 のヒープソートとくらべよう


-- ==================================================
-- ここから下は「画面の道具」です
-- p5.js と同じ「左上が原点、y は下向き」の座標で考えて、ここで gloss の座標に直します
-- ==================================================
fi :: Int -> Float
fi = fromIntegral

scr :: Env -> Float -> Float -> Point
scr env x y = (x - envW env / 2, envH env / 2 - y)

-- | gloss の座標を p5.js と同じ座標へ
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- | 左上をそろえた文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y sz c s = let (px, py) = scr env x (y + sz) in label px py sz c s

-- | 中央をそろえた文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y sz c s = let (px, py) = scr env x y in labelC px py sz c s

-- | 塗りつぶした円（中心と半径）
circ :: Env -> Float -> Float -> Float -> Color -> Picture
circ env x y r c = at (scr env x y) (Color c (disc r))

-- | 円の枠（中心、半径、太さ）
circEdge :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
circEdge env x y r sw c = at (scr env x y) (Color c (ring r sw))

-- | 太さのある線分
seg :: Env -> Float -> Color -> Float -> Float -> Float -> Float -> Picture
seg env sw c x1 y1 x2 y2 = Color c (thickLine sw (scr env x1 y1) (scr env x2 y2))

-- | 三角形（矢じりなど）
tri :: Env -> Color -> Point -> Point -> Point -> Picture
tri env c (a, b) (d, e) (f, g) = Color c (Polygon [scr env a b, scr env d e, scr env f g])

-- | キーをそろえる：英字は小文字に。Enter・Backspace・スペースは環境によって来かたが違うので 1 つにまとめる
normKey :: Key -> Key
normKey k = case k of
  Char ' ' -> SpecialKey KeySpace
  Char '\r' -> SpecialKey KeyEnter
  Char '\n' -> SpecialKey KeyEnter
  Char '\b' -> SpecialKey KeyBackspace
  Char '\DEL' -> SpecialKey KeyBackspace
  SpecialKey KeyDelete -> SpecialKey KeyBackspace
  SpecialKey KeyPadEnter -> SpecialKey KeyEnter
  Char c -> Char (toLower c)
  _ -> k
