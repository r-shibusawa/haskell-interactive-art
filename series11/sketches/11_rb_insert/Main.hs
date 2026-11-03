-- ============================================
-- 11 赤黒木：挿入と回転
-- 各節点を赤か黒にぬり、5 つの性質を守ると、高さが必ず 2 log₂(n+1) 以下になる
--   1. 節点は赤か黒　2. 根は黒　3. 葉（NIL）は黒　4. 赤の子は黒（赤が 2 つ続かない）　5. どの道も黒の数が同じ
-- 挿入：二分探索木として入れて赤にぬる → 性質 4 がくずれたら「ぬりかえ」と「回転」で直す（ケース 1〜3）
-- 操作：数字 + Enter = 挿入 / スペース = ランダムに挿入 / A = 1, 2, 3, … と小さい順に入れる / F = 見本の例
--       R = 空に / P = 再生・一時停止 / ← → = コマ送り
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt, isDigit, toLower)
import Data.Function ((&))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Numeric (showFFloat)

animSpeed :: Float
animSpeed = 0.07    -- コマの切りかえの速さ

holdFrames :: Int
holdFrames = 35     -- ★ 1 コマ止まる長さ（フレーム）

treeTop :: Float
treeTop = 140       -- 木を描く範囲（上の余白）

treeBottom :: Env -> Float
treeBottom _ = 160  -- 木を描く範囲（下の余白）

data World = World
  { tree    :: !RB
  , anim    :: !Anim
  , buf     :: String
  , pending :: [Int]   -- これから順に入れるキー
  , order   :: [Int]   -- 入れた順番（くらべる用）
  }

initialWorld :: World
initialWorld = resetTree (World (emptyRB True) emptyAnim "" [] [])

-- | 木に操作をして、記録したコマを再生の列へ移す
withTree :: (RB -> RB) -> World -> World
withTree f w = let (t, a) = flushLog (f (tree w)) (anim w) in w { tree = t, anim = a }

resetTree :: World -> World
resetTree w =
  withTree (rbLog "An empty red-black tree. Try number + Enter, A or F" [])
    w { tree = emptyRB True, order = [], pending = [], anim = clearFrames (anim w) }

insertKey :: Int -> World -> World
insertKey k w = case rbInsertKey k (tree w) of
  Just t -> withTree (const t) w { order = order w ++ [k] }
  Nothing -> withTree (rbLog (show k ++ " is already there") [k]) w

-- | 同じ順番で普通の二分探索木に入れたときの高さ（くらべる用）
data Plain = PE | PN !Int Plain Plain

plainHeight :: [Int] -> Int
plainHeight keys = snd (foldl' add (PE, 0) keys)
  where
    add (t, h) k = let (t', d) = ins k 1 t in (t', max h d)
    ins :: Int -> Int -> Plain -> (Plain, Int)
    ins k d PE = (PN k PE PE, d)
    ins k d (PN x l r)
      | k < x = let (l', h) = ins k (d + 1) l in (PN x l' r, h)
      | otherwise = let (r', h) = ins k (d + 1) r in (PN x l r', h)

step :: Env -> Float -> World -> IO World
step _ _ w0 =
  let w1 = case pending w0 of
             (k : rest) | not (framesBusy (anim w0)) -> insertKey k w0 { pending = rest }
             _ -> w0
  in pure w1 { anim = updateFrames (anim w1) }

draw :: Env -> World -> Picture
draw env w =
  let t = tree w
      n = countNodes t (rbRoot t)
      bh = blackHeight t (rbRoot t)
      rx = envW env - 30
      bound = 2 * logBase 2 (fi n + 1) :: Float
  in Pictures
       [ txt env 30 24 26 (gray 0.94) "Red-Black Tree: Insertion and Rotation"
       , txt env 30 58 14 (rgb255 210 160 170) "Number + Enter = insert  Space = random  A = 1-20 in order"
       , txt env 30 76 14 (rgb255 210 160 170) "F = sample  R = clear  P play/pause  <- -> step"
       , txt env 30 96 18 (gray 1) ("Input: " ++ buf w ++ (if envFrame env `mod` 60 < 30 then "|" else ""))
       , drawTreeFrames env defaultStyle (anim w)
       , txtR env rx 60 15 (gray 0.86) (show n ++ " nodes  height " ++ show (treeHeight t (rbRoot t)) ++ " (bound 2 log2(n+1) = " ++ fmt 1 bound ++ ")")
       , txtR env rx 82 15 (gray 0.86) ("Plain BST in the same order: height " ++ show (plainHeight (order w)))
       , if framesBusy (anim w) then Blank
         else txtR env rx 104 15 (if bh > 0 then rgb255 140 230 160 else rgb255 255 120 120)
                (if bh > 0 then "OK: all 5 properties hold, black height = " ++ show (bh - 1) else "In progress...")
       , drawFrameMessage env (anim w) (envH env - 80)
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey k Down _ _ -> onKey (normKey k) w
  _ -> pure w

onKey :: Key -> World -> IO World
onKey key w = case key of
  Char c | isDigit c -> pure w { buf = pushDigit 3 c (buf w) }
  SpecialKey KeyBackspace -> pure w { buf = popDigit (buf w) }
  SpecialKey KeyRight -> pure w { anim = stepForward (anim w) }
  SpecialKey KeyLeft -> pure w { anim = stepBack (anim w) }
  Char 'p' -> pure w { anim = togglePlay (anim w) }
  SpecialKey KeyEnter | not (null (buf w)) ->
    pure (insertKey (readNum (buf w)) w { anim = skipToEnd (anim w) }) { buf = "" }
  SpecialKey KeySpace -> do
    let w1 = w { anim = skipToEnd (anim w) }
    if countNodes (tree w1) (rbRoot (tree w1)) < 40
      then do
        k <- randomRIO (1, 99)
        pure (insertKey k w1)
      else pure w1
  Char 'a' -> pure (resetTree w) { pending = [1 .. 20] }
  Char 'f' -> pure (resetTree w) { pending = [26, 30, 6, 22, 5, 1, 16, 29, 34, 13, 8] }
  Char 'r' -> pure (resetTree w)
  _ -> pure w

main :: IO ()
main = runSketch (sketch "11-11 Red-Black Tree Insert" initialWorld)
  { skBack  = rgb255 24 14 18
  , skStep  = step
  , skDraw  = draw
  , skEvent = onEvent
  }

-- ---------- やってみよう ----------
-- 1. A：小さい順に 20 個。普通の二分探索木なら高さ 20 だが、赤黒木は 6〜8 におさまる
-- 2. F：見本の例。最後の 8 でケース 1 → 2 → 3 が全部出てくる
-- 3. 1 回の挿入で回転は多くても 2 回。ぬりかえ（ケース 1）は根まで上がることがある


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

framesBusy :: Anim -> Bool
framesBusy a = not (Seq.null (frames a)) && (cur a < Seq.length (frames a) - 1 || tAnim a < 1)

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

-- | 性質のチェック：赤の子は黒か？ どの道も黒の数が同じか？ → 黒高さを返す（こわれていたら -1）
blackHeight :: RB -> Int -> Int
blackHeight t x
  | x == nil = 1
  | colorOf t x == Red && (colorOf t (leftOf t x) == Red || colorOf t (rightOf t x) == Red) = -1
  | otherwise =
      let a = blackHeight t (leftOf t x)
          b = blackHeight t (rightOf t x)
      in if a < 0 || b < 0 || a /= b then -1 else a + (if colorOf t x == Black then 1 else 0)

treeHeight :: RB -> Int -> Int
treeHeight t x = if x == nil then 0 else 1 + max (treeHeight t (leftOf t x)) (treeHeight t (rightOf t x))

countNodes :: RB -> Int -> Int
countNodes t x = if x == nil then 0 else 1 + countNodes t (leftOf t x) + countNodes t (rightOf t x)


-- ==================================================
-- ここから下は「画面の道具」です
-- p5.js と同じ「左上が原点、y は下向き」の座標で考えて、ここで gloss の座標に直します
-- ==================================================
fi :: Int -> Float
fi = fromIntegral

scr :: Env -> Float -> Float -> Point
scr env x y = (x - envW env / 2, envH env / 2 - y)

-- | 左上をそろえた文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y sz c s = let (px, py) = scr env x (y + sz) in label px py sz c s

-- | 中央をそろえた文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y sz c s = let (px, py) = scr env x y in labelC px py sz c s

-- | 右上をそろえた文字
txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtR env x y sz c s = txt env (x - fi (length s) * sz * 0.72) y sz c s

-- | 塗りつぶした円（中心と半径）
circ :: Env -> Float -> Float -> Float -> Color -> Picture
circ env x y r c = at (scr env x y) (Color c (disc r))

-- | 円の枠（中心、半径、太さ）
circEdge :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
circEdge env x y r sw c = at (scr env x y) (Color c (ring r sw))

-- | 太さのある線分
seg :: Env -> Float -> Color -> Float -> Float -> Float -> Float -> Picture
seg env sw c x1 y1 x2 y2 = Color c (thickLine sw (scr env x1 y1) (scr env x2 y2))

-- | 小数 n 桁の文字列（p5.js の nf に相当）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""

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
