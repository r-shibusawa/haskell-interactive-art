-- ============================================
-- 13 順序統計木：「k 番目に小さいキー」を O(log n) で
-- 赤黒木の各節点に size（自分の部分木の節点の数）を足しておく。x.size = x.left.size + x.right.size + 1
-- SELECT(i)：r = 左の size + 1 が自分の順位。i = r なら自分、i < r なら左へ、i > r なら右へ（i から r を引く）
-- RANK(x)：x から根まで上がりながら、右の子として上がるたびに「左の兄弟の size + 1」を足す
-- size は回転のときに 2 つの節点だけ直せばよい → データ構造の拡張（augmenting）の典型例
-- 操作：数字 + S = i 番目を探す（SELECT）/ 節点をクリック = 順位を求める（RANK）
--       数字 + Enter = 挿入 / 数字 + D = 削除 / スペース = ランダムに挿入 / R = 作り直す / P・← → = 再生操作
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt, isDigit, toLower)
import Data.Function ((&))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe)

sampleKeys :: [Int]
sampleKeys = [33, 18, 52, 9, 25, 44, 61, 4, 13, 21, 29, 40, 48, 57, 66, 2, 11, 36, 50, 70]  -- ★ 見本の 20 個

animSpeed :: Float
animSpeed = 0.07    -- コマの切りかえの速さ

holdFrames :: Int
holdFrames = 35     -- 1 コマ止まる長さ（フレーム）

treeTop :: Float
treeTop = 140       -- 木を描く範囲（上の余白）

treeBottom :: Env -> Float
treeBottom _ = 170  -- 木を描く範囲（下の余白）

data World = World
  { tree :: !RB
  , anim :: !Anim
  , buf  :: String
  }

initialWorld :: World
initialWorld = rebuild (World (emptyRB False) emptyAnim "")

-- | 木に操作をして、記録したコマを再生の列へ移す
withTree :: (RB -> RB) -> World -> World
withTree f w = let (t, a) = flushLog (f (tree w)) (anim w) in w { tree = t, anim = a }

rebuild :: World -> World
rebuild w =
  let quiet = foldl' (\t k -> fromMaybe t (rbInsertKey k t)) (emptyRB False) sampleKeys
  in withTree (rbShow "The number under each node is its size (nodes in its subtree)" [])
       w { tree = quiet, anim = clearFrames (anim w) }

sizeOf :: RB -> Int -> Int
sizeOf t x = nSize (node t x)

osSelect :: Int -> RB -> RB
osSelect i t
  | i < 1 || i > n = rbShow ("Please give a number from 1 to " ++ show n) [] t
  | otherwise = go (rbRoot t) i (rbShow ("SELECT(" ++ show i ++ "): find the key of rank " ++ show i) [] t)
  where
    n = sizeOf t (rbRoot t)
    go x want acc
      | x == nil = acc
      | want == r = rbShow ("r = " ++ show ls ++ " + 1 = " ++ show r ++ " = i -> " ++ show x ++ " has rank " ++ show i ++ "!") [x] acc
      | want < r = go (leftOf acc x) want (rbShow (show x ++ " has rank r = " ++ show r ++ " in this subtree. i = " ++ show want ++ " < r -> go left") [x] acc)
      | otherwise = go (rightOf acc x) (want - r) (rbShow ("r = " ++ show r ++ ", i = " ++ show want ++ " > r -> go right, and look for rank " ++ show want ++ " - " ++ show r ++ " = " ++ show (want - r) ++ " there") [x] acc)
      where ls = sizeOf acc (leftOf acc x)
            r = ls + 1

osRank :: Int -> RB -> RB
osRank x t =
  let r0 = sizeOf t (leftOf t x) + 1
      go y r acc
        | y == rbRoot acc = rbShow (show x ++ " has rank " ++ show r) [x] acc
        | y == rightOf acc p =
            let ls = sizeOf acc (leftOf acc p)
                r' = r + ls + 1
            in go p r' (rbShow ("Up to " ++ show p ++ " as a right child -> add left sibling's " ++ show ls ++ " + 1 for " ++ show p ++ ": r = " ++ show r') [p] acc)
        | otherwise = go p r (rbShow ("Up to " ++ show p ++ " as a left child -> add nothing (r = " ++ show r ++ ")") [p] acc)
        where p = parentOf acc y
  in go x r0 (rbShow ("RANK(" ++ show x ++ "): first r = left size + 1 = " ++ show r0) [x] t)

step :: Env -> Float -> World -> IO World
step _ _ w = pure w { anim = updateFrames (anim w) }

draw :: Env -> World -> Picture
draw env w =
  let t = tree w
      style = defaultStyle { sSub = Just (\_ nd -> "size " ++ show (iSize (fnInfo nd))) }
      ranks = unwords (zipWith (\i k -> show (i :: Int) ++ ":" ++ show k ++ " ") [1 ..] (allKeys t (rbRoot t)))
      rows = take 3 (wrapText 120 ranks)
      y0 = envH env - 100 - fi (length rows) * 18
  in Pictures
       ( [ txt env 30 24 26 (gray 0.94) "Order-Statistic Tree (a red-black tree with size)"
         , txt env 30 58 14 (rgb255 160 200 180) "Number + S = rank i (SELECT)  Click = rank of a node (RANK)  Number + Enter = insert"
         , txt env 30 76 14 (rgb255 160 200 180) "Number + D = delete  Space = random  R = rebuild  P / <- -> = playback"
         , txt env 30 96 18 (gray 1) ("Input: " ++ buf w ++ (if envFrame env `mod` 60 < 30 then "|" else ""))
         , drawTreeFrames env style (anim w)
         , txt env 30 (y0 - 20) 14 (rgb255 160 230 200) "Sorted (rank:key):"
         ]
         ++ zipWith (\i s -> txt env 30 (y0 + fi (i :: Int) * 18) 14 (rgb255 160 230 200) s) [0 ..] rows
         ++ [drawFrameMessage env (anim w) (envH env - 80)] )

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    pure $ case nodeAt env (anim w) (toP5 env pos) of
      Just k ->
        let w1 = w { anim = skipToEnd (anim w) }
        in if rbSearch k (tree w1) /= nil then withTree (osRank k) w1 else w1
      Nothing -> w
  EventKey k Down _ _ -> onKey (normKey k) w
  _ -> pure w

onKey :: Key -> World -> IO World
onKey key w = case key of
  Char c | isDigit c -> pure w { buf = pushDigit 3 c (buf w) }
  SpecialKey KeyBackspace -> pure w { buf = popDigit (buf w) }
  SpecialKey KeyRight -> pure w { anim = stepForward (anim w) }
  SpecialKey KeyLeft -> pure w { anim = stepBack (anim w) }
  Char 'p' -> pure w { anim = togglePlay (anim w) }
  _ -> command key w { anim = skipToEnd (anim w) }

command :: Key -> World -> IO World
command key w = case key of
  Char 's' | not (null (buf w)) -> pure (withTree (osSelect (readNum (buf w))) w) { buf = "" }
  SpecialKey KeyEnter | not (null (buf w)) ->
    let k = readNum (buf w)
    in pure (withTree (\t -> case rbInsertKey k t of
                 Just t' -> rbShow (buf w ++ " inserted (size is updated only along the path and at rotations)") [k] t'
                 Nothing -> rbShow (buf w ++ " is already there") [k] t) w) { buf = "" }
  Char 'd' | not (null (buf w)) ->
    let k = readNum (buf w)
    in pure (withTree (\t -> case rbDelete k t of
                 Just t' -> rbShow (buf w ++ " deleted") [] t'
                 Nothing -> rbShow (buf w ++ " is not there") [] t) w) { buf = "" }
  SpecialKey KeySpace -> do
    v <- randomRIO (1, 99)
    let t = tree w
    pure $ if countNodes t (rbRoot t) < 40
      then case rbInsertKey v t of
             Just t' -> withTree (const (rbShow (show v ++ " inserted") [v] t')) w
             Nothing -> w
      else w
  Char 'r' -> pure (rebuild w)
  _ -> pure w

main :: IO ()
main = runSketch (sketch "11-13 Order-Statistic Tree" initialWorld)
  { skBack  = rgb255 14 20 18
  , skStep  = step
  , skDraw  = draw
  , skEvent = onEvent
  }

-- ---------- やってみよう ----------
-- 1. 17 + S：17 番目に小さいキーは？ 何回の比較で着いた？（高さくらい）
-- 2. 40 をクリック：順位の計算で「右の子として上がる」ときだけ足している
-- 3. size の代わりに「部分木のキーの合計」を持たせると、範囲の合計も O(log n) で求められる


-- ==================================================
-- ここから下は「木を描く道具」です（アニメーション用）
-- 操作の途中経過を「コマ」として記録し、順番になめらかに再生します
-- ==================================================
-- | NIL（どこも指さない）を表す番号
nil :: Int
nil = -1

data Paint = NoPaint | Red | Black deriving (Eq)

-- | コマに写しておく節点の中身
data Info = Info { iPaint :: !Paint, iSize :: !Int, iMax :: !Int, iLabel :: String }

-- | コマの中の節点：中間順の番号 fnI、深さ fnD
data FNode = FNode { fnI :: !Int, fnD :: !Int, fnInfo :: !Info }

-- | 1 コマ（その瞬間の木の形とメッセージ）
data Frame = Frame
  { fNodes :: !(IM.IntMap FNode)
  , fEdges :: [(Int, Int)]
  , fCount :: !Int
  , fMaxD  :: !Int
  , fMsg   :: String
  , fHl    :: [Int]
  , fOut   :: Maybe [Int]
  }

-- | 再生の状態
data Anim = Anim
  { frames  :: !(Seq.Seq Frame)
  , cur     :: !Int
  , tAnim   :: !Float
  , hold    :: !Int
  , playing :: !Bool
  }

emptyAnim :: Anim
emptyAnim = Anim Seq.empty 0 1 0 True

-- | 木をなぞって 1 コマを作る。kids は左右の子、info は節点の中身を返す関数
makeFrame :: Int -> (Int -> (Int, Int)) -> (Int -> Info) -> String -> [Int] -> Frame
makeFrame rt kids info msg hl =
  let walk x d pk st@(ns, es, i, md)
        | x == nil = st
        | otherwise =
            let (l, r) = kids x
                (ns1, es1, i1, md1) = walk l (d + 1 :: Int) x st
                st2 = (IM.insert x (FNode i1 d (info x)) ns1, es1, i1 + 1, max md1 d)
                (ns3, es3, i3, md3) = walk r (d + 1) x st2
            in (ns3, if pk == nil then es3 else (pk, x) : es3, i3, md3)
      (nodes, edges, n, maxD) = walk rt 0 nil (IM.empty, [], 0 :: Int, 0)
  in Frame nodes edges n maxD msg (filter (/= nil) hl) Nothing

-- | コマを足す（多くても 400 コマ）
recordFrame :: Frame -> Anim -> Anim
recordFrame f a =
  let fs = frames a Seq.|> f
  in if Seq.length fs > 400 then a { frames = Seq.drop 1 fs, cur = max 0 (cur a - 1) } else a { frames = fs }

clearFrames :: Anim -> Anim
clearFrames a = a { frames = Seq.empty, cur = 0, tAnim = 1, hold = 0 }

updateFrames :: Anim -> Anim
updateFrames a
  | Seq.null (frames a) = a
  | tAnim a < 1 = a { tAnim = min 1 (tAnim a + animSpeed) }
  | cur a < Seq.length (frames a) - 1 && playing a =
      if hold a < holdFrames then a { hold = hold a + 1 } else a { hold = 0, cur = cur a + 1, tAnim = 0 }
  | otherwise = a

stepForward :: Anim -> Anim
stepForward a = if cur a < Seq.length (frames a) - 1 then a { cur = cur a + 1, tAnim = 0, hold = 0 } else a

stepBack :: Anim -> Anim
stepBack a = if cur a > 0 then a { cur = cur a - 1, tAnim = 1 } else a

skipToEnd :: Anim -> Anim
skipToEnd a = a { cur = max 0 (Seq.length (frames a) - 1), tAnim = 1 }

togglePlay :: Anim -> Anim
togglePlay a = a { playing = not (playing a) }

currentFrame :: Anim -> Maybe Frame
currentFrame a = Seq.lookup (cur a) (frames a)

layoutPos :: Env -> Frame -> FNode -> (Float, Float)
layoutPos env f nd =
  let gap = min 78 ((envH env - treeTop - treeBottom env) / max 1 (fi (fMaxD f + 1)))
  in (50 + ((fi (fnI nd) + 0.5) / max 1 (fi (fCount f))) * (envW env - 100), treeTop + fi (fnD nd) * gap)

nodeRadius :: Env -> Frame -> Float
nodeRadius env f = clampF 9 24 (((envW env - 100) / max 1 (fi (fCount f))) * 0.42)

easeT :: Float -> Float
easeT t = if t < 0.5 then 2 * t * t else 1 - ((-2 * t + 2) ** 2) / 2

-- | 今のコマ B と、動き始めのコマ A
scenes :: Anim -> Maybe (Frame, Frame)
scenes a = do
  b <- currentFrame a
  let prev = if cur a > 0 && tAnim a < 1 then Seq.lookup (cur a - 1) (frames a) else Nothing
  pure (maybe b id prev, b)

-- | 各節点の今の位置 (x, y, 不透明度 0〜1)
nodePositions :: Env -> Anim -> IM.IntMap (Float, Float, Float)
nodePositions env a = case scenes a of
  Nothing -> IM.empty
  Just (fa, fb) ->
    let t = easeT (tAnim a)
        place k nd =
          let (bx, by) = layoutPos env fb nd
          in case IM.lookup k (fNodes fa) of
               Just ndA -> let (ax, ay) = layoutPos env fa ndA in (lerpF ax bx t, lerpF ay by t, 1)
               Nothing -> (bx, by, t)
    in IM.mapWithKey place (fNodes fb)

-- | (mx, my) の近くにある節点（p5.js の lastPos を使った判定と同じ）
nodeAt :: Env -> Anim -> (Float, Float) -> Maybe Int
nodeAt env a m =
  case [k | (k, (x, y, _)) <- IM.toAscList (nodePositions env a), dist2 m (x, y) < 22] of
    (k : _) -> Just k
    [] -> Nothing

-- | 節点の見た目：色、文字、下の小さい文字
data Style = Style
  { sFill  :: Int -> FNode -> Color
  , sLabel :: Int -> FNode -> String
  , sSub   :: Maybe (Int -> FNode -> String)
  }

defaultStyle :: Style
defaultStyle = Style (\_ nd -> defaultFill nd) (\_ nd -> iLabel (fnInfo nd)) Nothing

defaultFill :: FNode -> Color
defaultFill nd = case iPaint (fnInfo nd) of
  Red -> rgb255 222 60 75
  Black -> rgb255 28 30 40
  NoPaint -> rgb255 40 120 150

mixColor :: Color -> Color -> Float -> Float -> Color
mixColor c0 c1 t a =
  let (r0, g0, b0, _) = rgbaOfColor c0
      (r1, g1, b1, _) = rgbaOfColor c1
  in makeColor (lerpF r0 r1 t) (lerpF g0 g1 t) (lerpF b0 b1 t) a

drawTreeFrames :: Env -> Style -> Anim -> Picture
drawTreeFrames env style a = case scenes a of
  Nothing -> Blank
  Just (fa, fb) ->
    let t = easeT (tAnim a)
        pos = nodePositions env a
        r = lerpF (nodeRadius env fa) (nodeRadius env fb) t
        edgePic (p, q) = case (IM.lookup p pos, IM.lookup q pos) of
          (Just (x1, y1, a1), Just (x2, y2, a2)) ->
            seg env 2 (makeColorI 120 130 165 (round (255 * min a1 a2))) x1 y1 x2 y2
          _ -> Blank
        gone = [ let (x, y) = layoutPos env fa nd
                 in circ env x y (r * (1 + t)) (makeColorI 240 90 90 (round (255 * (1 - t))))
               | (k, nd) <- IM.toList (fNodes fa), not (IM.member k (fNodes fb)) ]
        nodePic (k, nd) = case IM.lookup k pos of
          Nothing -> Blank
          Just (x, y, al) ->
            let ndA = IM.findWithDefault nd k (fNodes fa)
                hl = k `elem` fHl fb
                lab = sLabel style k nd
                sz = max 14 (min 18 (r * (if length lab > 2 then 0.62 else 0.85)))
            in Pictures
                 [ if hl then circ env x y (r * 1.55) (makeColorI 255 214 90 (round (70 * al))) else Blank
                 , circ env x y r (mixColor (sFill style k ndA) (sFill style k nd) t al)
                 , if hl then circEdge env x y r 4 (makeColor 1 0.84 0.35 al)
                         else circEdge env x y r 1.5 (makeColor 0.9 0.9 0.94 (al * 0.6))
                 , txtC env x y sz (makeColor 1 1 1 al) lab
                 , case sSub style of
                     Just f -> txtC env x (y + r + 10) 14 (makeColor 0.75 0.78 0.9 al) (f k nd)
                     Nothing -> Blank
                 ]
    in Pictures (map edgePic (fEdges fb) ++ gone ++ map nodePic (IM.toList (fNodes fb)))

-- | メッセージ（長ければ 2 行に折り返す）とコマの番号
drawFrameMessage :: Env -> Anim -> Float -> Picture
drawFrameMessage env a y = case currentFrame a of
  Nothing -> Blank
  Just f ->
    Pictures
      [ Pictures (zipWith (\i s -> txt env 30 (y + i * 20) 16 (rgb255 255 225 140) s) [0 ..] (take 2 (wrapText 105 (fMsg f))))
      , txt env 30 (y + 42) 14 (rgb255 140 150 180)
          ("Frame " ++ show (cur a + 1) ++ " / " ++ show (Seq.length (frames a)) ++ "   "
           ++ (if playing a then "> playing" else "|| paused") ++ "  (P play/pause, <- -> step)")
      ]


-- ==================================================
-- ここから下は「赤黒木の道具」です（挿入・削除・回転を Haskell で書いたもの）
-- 節点は「キー → Node」の表に入れて、left / right / parent をキーの番号で指す
-- NIL は「番兵」：葉の代わりに置く黒い節点。すべての葉がこの 1 個（番号 -1）を指す
-- ==================================================
data Node = Node
  { nLeft   :: !Int
  , nRight  :: !Int
  , nParent :: !Int
  , nColor  :: !Paint
  , nSize   :: !Int      -- 部分木の節点の数（順序統計木で使う）
  , nHigh   :: !Int      -- 区間の右端（ふつうの木ではキーと同じ）
  , nMax    :: !Int      -- 部分木の nHigh の最大（区間木で使う）
  , nLabel  :: String
  }

data RB = RB
  { rbRoot   :: !Int
  , rbNodes  :: !(IM.IntMap Node)
  , rbLogOn  :: !Bool      -- False にすると途中経過を記録しない
  , rbFrames :: [Frame]    -- 記録したコマ（新しいものが先頭）
  }

nilNode :: Node
nilNode = Node nil nil nil Black 0 minBound minBound "NIL"

emptyRB :: Bool -> RB
emptyRB logOn = RB nil (IM.singleton nil nilNode) logOn []

node :: RB -> Int -> Node
node t x = IM.findWithDefault nilNode x (rbNodes t)

leftOf :: RB -> Int -> Int
leftOf t = nLeft . node t

rightOf :: RB -> Int -> Int
rightOf t = nRight . node t

parentOf :: RB -> Int -> Int
parentOf t = nParent . node t

colorOf :: RB -> Int -> Paint
colorOf t = nColor . node t

lbl :: RB -> Int -> String
lbl t = nLabel . node t

modNode :: Int -> (Node -> Node) -> RB -> RB
modNode x f t = t { rbNodes = IM.adjust f x (rbNodes t) }

setLeft :: Int -> Int -> RB -> RB
setLeft x v = modNode x (\n -> n { nLeft = v })

setRight :: Int -> Int -> RB -> RB
setRight x v = modNode x (\n -> n { nRight = v })

setParent :: Int -> Int -> RB -> RB
setParent x v = modNode x (\n -> n { nParent = v })

setColor :: Int -> Paint -> RB -> RB
setColor x c = modNode x (\n -> n { nColor = c })

-- | 左右をまとめて書くための道具（左右反対のケースを 1 つの関数で書ける）
data Side = L | R deriving (Eq)

other :: Side -> Side
other L = R
other R = L

childOf :: Side -> RB -> Int -> Int
childOf L = leftOf
childOf R = rightOf

setChild :: Side -> Int -> Int -> RB -> RB
setChild L = setLeft
setChild R = setRight

-- | 拡張データ構造：size と max を子から計算
fixNode :: Int -> RB -> RB
fixNode x t
  | x == nil = t
  | otherwise =
      let l = node t (leftOf t x)
          r = node t (rightOf t x)
      in modNode x (\n -> n { nSize = nSize l + nSize r + 1, nMax = maximum [nHigh n, nMax l, nMax r] }) t

fixAll :: RB -> RB
fixAll t = go (rbRoot t) t
  where go x acc = if x == nil then acc else fixNode x (go (rightOf acc x) (go (leftOf acc x) acc))

-- | 今の木をコマにして記録する（rbLogOn を見ない）
rbShow :: String -> [Int] -> RB -> RB
rbShow msg hl t0 =
  let t = fixAll t0
      info x = let n = node t x in Info (nColor n) (nSize n) (nMax n) (nLabel n)
      f = makeFrame (rbRoot t) (\x -> (leftOf t x, rightOf t x)) info msg hl
  in t { rbFrames = f : rbFrames t }

-- | 途中経過を記録する
rbLog :: String -> [Int] -> RB -> RB
rbLog msg hl t = if rbLogOn t then rbShow msg hl t else t

-- | 記録したコマを再生の列へ移す
flushLog :: RB -> Anim -> (RB, Anim)
flushLog t a = (t { rbFrames = [] }, foldl' (flip recordFrame) a (reverse (rbFrames t)))

-- | 回転。rotateTo L x が左回転、rotateTo R x が右回転
rotateTo :: Side -> Int -> RB -> RB
rotateTo s x t =
  let o = other s
      y = childOf o t x      -- 左回転なら y = x.right
      b = childOf s t y      -- y.left は x の右へ移る
      p = parentOf t x
      t1 = t & setChild o x b
             & (if b /= nil then setParent b x else id)
             & setParent y p
      t2 | p == nil = t1 { rbRoot = y }
         | x == leftOf t1 p = setLeft p y t1
         | otherwise = setRight p y t1
  in t2 & setChild s y x & setParent x y
        & fixNode x & fixNode y   -- 回転で変わるのは x と y の 2 つだけ → O(1) で更新できる

rbSearch :: Int -> RB -> Int
rbSearch k t = go (rbRoot t)
  where go x | x == nil || x == k = x
             | k < x = go (leftOf t x)
             | otherwise = go (rightOf t x)

treeMinimum :: RB -> Int -> Int
treeMinimum t x = if leftOf t x == nil then x else treeMinimum t (leftOf t x)

-- | 挿入（同じキーがあれば Nothing）。high は区間の右端、lab は節点に書く文字
rbInsert :: Int -> Int -> String -> RB -> Maybe RB
rbInsert k high lab t
  | rbSearch k t /= nil = Nothing
  | otherwise =
      let descend y x | x == nil = y
                      | k < x = descend x (leftOf t x)
                      | otherwise = descend x (rightOf t x)
          y = descend nil (rbRoot t)
          z = Node nil nil y Red 1 high high lab
          t1 = t { rbNodes = IM.insert k z (rbNodes t) }
          t2 | y == nil = t1 { rbRoot = k }
             | k < y = setLeft y k t1
             | otherwise = setRight y k t1
      in Just (insertFixup k (rbLog (lab ++ " inserted as in a plain BST and painted red") [k] t2))

-- | ふつうのキーを入れるとき
rbInsertKey :: Int -> RB -> Maybe RB
rbInsertKey k = rbInsert k k (show k)

-- | 赤が 2 つ続く所を「ぬりかえ」と「回転」で直す（ケース 1〜3）
insertFixup :: Int -> RB -> RB
insertFixup z t
  | colorOf t (parentOf t z) /= Red = finish
  | colorOf t uncle == Red =
      t & setColor p Black & setColor uncle Black & setColor g Red
        & rbLog ("Case 1" ++ tag ++ ": uncle is red -> parent, uncle black, grandparent red; go on from grandparent") [z, p, uncle, g]
        & insertFixup g
  | otherwise =
      let (z1, t1)
            | z == childOf (other side) t p =     -- z が「内側」の孫
                let t' = rotateTo side p t
                in (p, rbLog ("Case 2" ++ tag ++ ": z is an inner grandchild -> rotate at parent to get the case 3 shape") [p, parentOf t' p] t')
            | otherwise = (z, t)
          p1 = parentOf t1 z1
          g1 = parentOf t1 p1
          t2 = t1 & setColor p1 Black & setColor g1 Red
                  & rbLog ("Case 3" ++ tag ++ ": paint parent black, grandparent red") [p1, g1]
                  & rotateTo (other side) g1
      in t2 & rbLog ("Case 3" ++ tag ++ ": rotate at grandparent -> no more two reds in a row") [parentOf t2 z1]
            & insertFixup z1
  where
    p = parentOf t z
    g = parentOf t p
    side = if p == leftOf t g then L else R
    uncle = childOf (other side) t g
    tag = if side == L then "" else " (mirrored)"
    finish
      | colorOf t (rbRoot t) == Red =
          t & setColor (rbRoot t) Black & rbLog "Paint the root black (property 2). Done" [rbRoot t]
      | otherwise = rbLog "All red-black properties hold again. Done" [] t

-- | u の場所に v をつなぎかえる（v は NIL でもよい。NIL の親も書きかえる）
rbTransplant :: Int -> Int -> RB -> RB
rbTransplant u v t =
  let p = parentOf t u
      t1 | p == nil = t { rbRoot = v }
         | u == leftOf t p = setLeft p v t
         | otherwise = setRight p v t
  in setParent v p t1

-- | 削除（キーがなければ Nothing）
rbDelete :: Int -> RB -> Maybe RB
rbDelete z t0
  | rbSearch z t0 == nil = Nothing
  | otherwise = Just result
  where
    t = rbLog (lbl t0 z ++ " will be deleted") [z] t0
    zl = leftOf t z
    zr = rightOf t z
    finish yColor x t' =
      let t2 = t' { rbNodes = IM.delete z (rbNodes t') }
      in if yColor == Black then deleteFixup x t2
         else rbLog "The removed node was red -> properties are not broken. Done" [] t2
    result
      | zl == nil =
          finish (colorOf t z) zr (t & rbTransplant z zr & rbLog "No left child -> replace it by the right child" [zr])
      | zr == nil =
          finish (colorOf t z) zl (t & rbTransplant z zl & rbLog "No right child -> replace it by the left child" [zl])
      | otherwise =
          let y = treeMinimum t zr
              x = rightOf t y
              t1 = rbLog ("Two children -> use the next larger " ++ lbl t y ++ " (minimum of the right subtree)") [z, y] t
              t2 | parentOf t1 y == z = setParent x y t1
                 | otherwise = t1 & rbTransplant y x & setRight y zr & setParent zr y
              t3 = t2 & rbTransplant z y & setLeft y zl & setParent zl y & setColor y (colorOf t z)
                      & rbLog (lbl t y ++ " moves to the place of " ++ lbl t z ++ " (and takes its color)") [y]
          in finish (colorOf t y) x t3

-- | 黒が 1 つ足りない所 x を、兄弟 w の色で分けて直す（ケース 1〜4）
deleteFixup :: Int -> RB -> RB
deleteFixup x0 t0 =
  loop x0 (rbLog ("One black is missing (x = " ++ lbl t0 x0 ++ " is 'doubly black')") [x0] t0)
  where
    loop x t
      | x /= rbRoot t && colorOf t x == Black =
          let p = parentOf t x
              s = if x == leftOf t p then L else R
              o = other s
              tag = if s == L then "" else " (mirrored)"
              w0 = childOf o t p
              t1 | colorOf t w0 == Red =
                     let tr = t & setColor w0 Black & setColor p Red & rotateTo s p
                     in rbLog ("Case 1" ++ tag ++ ": sibling w is red -> recolor and rotate to get a black sibling") [childOf o tr p, p] tr
                 | otherwise = t
              w1 = childOf o t1 p
          in if colorOf t1 (leftOf t1 w1) == Black && colorOf t1 (rightOf t1 w1) == Black
               then loop p (t1 & setColor w1 Red & rbLog ("Case 2" ++ tag ++ ": both children of w are black -> paint w red, pass the problem to the parent") [w1, p])
               else
                 let t2 | colorOf t1 (childOf o t1 w1) == Black =
                            let tr = t1 & setColor (childOf s t1 w1) Black & setColor w1 Red & rotateTo o w1
                            in rbLog ("Case 3" ++ tag ++ ": only the inner child of w is red -> rotate at w to get the case 4 shape") [childOf o tr p] tr
                        | otherwise = t1
                     w2 = childOf o t2 p
                     t3 = t2 & setColor w2 (colorOf t2 p) & setColor p Black
                             & setColor (childOf o t2 w2) Black & rotateTo s p
                             & rbLog ("Case 4" ++ tag ++ ": outer child of w is red -> recolor and rotate at parent. Black restored") [w2]
                 in loop (rbRoot t3) t3
      | otherwise = t & setColor x Black & rbLog "Paint x black. Done" [x]

countNodes :: RB -> Int -> Int
countNodes t x = if x == nil then 0 else 1 + countNodes t (leftOf t x) + countNodes t (rightOf t x)

allKeys :: RB -> Int -> [Int]
allKeys t x = if x == nil then [] else allKeys t (leftOf t x) ++ [x] ++ allKeys t (rightOf t x)


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

-- | 長い文を n 文字ごとに折り返す
wrapText :: Int -> String -> [String]
wrapText n s = go (words s) ""
  where
    go [] cur = [cur | not (null cur)]
    go (x : xs) "" = go xs x
    go (x : xs) cur
      | length cur + 1 + length x <= n = go xs (cur ++ " " ++ x)
      | otherwise = cur : go xs x

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

-- | 打ち込んだ数字の文字列を数に（空なら 0）
readNum :: String -> Int
readNum = foldl' (\acc c -> acc * 10 + digitToInt c) 0

-- | 入力欄：数字を 1 つ足す（lim 桁まで）
pushDigit :: Int -> Char -> String -> String
pushDigit lim c s = if length s < lim then s ++ [c] else s

-- | 入力欄：最後の 1 文字を消す
popDigit :: String -> String
popDigit s = take (length s - 1) s
