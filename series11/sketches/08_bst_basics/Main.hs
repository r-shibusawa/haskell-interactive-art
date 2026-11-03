-- ============================================
-- 08 二分探索木：探索・最小・後者・なぞり方
-- どの節点でも「左の部分木のキー ≤ 自分 ≤ 右の部分木のキー」。だから大小を比べて左右に進むだけで探せる
-- かかる時間は木の高さ h に比例：O(h)
-- 操作：数字 + Enter = 挿入 / 数字 + S = 探索 / N = 最小 / X = 最大 / 節点をクリック = 後者（次に大きいキー）
--       W = なぞり方（中間順 → 先行順 → 後行順）/ スペース = ランダムに挿入 / R = 最初の木 / C = 空に
--       P = 再生・一時停止 / ← → = コマ送り
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt, isDigit, toLower)
import Data.Function ((&))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Sequence as Seq

firstKeys :: [Int]
firstKeys = [21, 10, 30, 5, 14, 26, 35, 3, 8, 12, 17]  -- ★ 最初の木

animSpeed :: Float
animSpeed = 0.07    -- ★ コマの切りかえの速さ

holdFrames :: Int
holdFrames = 35     -- ★ 1 コマ止まる長さ（フレーム）

treeTop :: Float
treeTop = 140       -- 木を描く範囲（上の余白）

treeBottom :: Env -> Float
treeBottom _ = 160  -- 木を描く範囲（下の余白）

walkNames :: [String]
walkNames = ["Inorder (left -> self -> right)", "Preorder (self -> left -> right)", "Postorder (left -> right -> self)"]

-- | 節点：キーを番号にして、left / right / parent をキーで指す（NIL は nil）
data Node = Node { nLeft :: !Int, nRight :: !Int, nParent :: !Int }

data World = World
  { nodes    :: !(IM.IntMap Node)
  , rootB    :: !Int
  , buf      :: String
  , walkType :: !Int
  , anim     :: !Anim
  }

initialWorld :: World
initialWorld = buildFirst (World IM.empty nil "" 0 emptyAnim)

node :: World -> Int -> Node
node w x = IM.findWithDefault (Node nil nil nil) x (nodes w)

leftOf, rightOf, parentOf :: World -> Int -> Int
leftOf w = nLeft . node w
rightOf w = nRight . node w
parentOf w = nParent . node w

-- | 途中経過を 1 コマ記録する
rec :: String -> [Int] -> World -> World
rec msg hl = recOut msg hl Nothing

recOut :: String -> [Int] -> Maybe [Int] -> World -> World
recOut msg hl out w =
  let info x = Info NoPaint 0 0 (show x)
      f = makeFrame (rootB w) (\x -> (leftOf w x, rightOf w x)) info msg hl
  in w { anim = recordFrame f { fOut = out } (anim w) }

buildFirst :: World -> World
buildFirst w0 =
  let w1 = foldl' (\acc k -> treeInsert False k acc) w0 { nodes = IM.empty, rootB = nil } firstKeys
  in rec "A binary search tree: left < self < right holds everywhere" [] w1 { anim = clearFrames (anim w1) }

-- | 比べて左右どちらへ行くかのメッセージ
compareMsg :: Int -> Int -> String
compareMsg k x
  | k < x = show k ++ " < " ++ show x ++ " -> go left"
  | otherwise = show k ++ " > " ++ show x ++ " -> go right"

treeInsert :: Bool -> Int -> World -> World
treeInsert showIt k = go nil 0 Nothing
  where
    go :: Int -> Int -> Maybe Int -> World -> World
    go y c start w =
      let x = maybe (rootB w) id start
      in if x == nil then attach y c w
         else if k == x then (if showIt then rec (show k ++ " is already there") [k] w else w)
         else
           let w1 = if showIt then rec (compareMsg k x) [x] w else w
           in go x (c + 1) (Just (if k < x then leftOf w1 x else rightOf w1 x)) w1
    attach y c w =
      let w1 = w { nodes = IM.insert k (Node nil nil y) (nodes w) }
          w2 | y == nil = w1 { rootB = k }
             | k < y = w1 { nodes = IM.adjust (\n -> n { nLeft = k }) y (nodes w1) }
             | otherwise = w1 { nodes = IM.adjust (\n -> n { nRight = k }) y (nodes w1) }
      in if showIt then rec ("Attach " ++ show k ++ " at the empty spot (" ++ show c ++ " comparisons)") [k] w2 else w2

treeSearch :: Int -> World -> World
treeSearch k w0 = go (rootB w0) (0 :: Int) w0
  where
    go x c w
      | x == nil = rec ("Reached NIL -> " ++ show k ++ " is not there (" ++ show c ++ " comparisons)") [] w
      | x == k = rec ("Found " ++ show k ++ "! (" ++ show (c + 1) ++ " comparisons)") [k] w
      | otherwise = go (if k < x then leftOf w x else rightOf w x) (c + 1) (rec (compareMsg k x) [x] w)

-- | いちばん左へ（途中を記録する）。着いた節点とともに返す
treeMin :: Int -> World -> (Int, World)
treeMin x w
  | leftOf w x == nil = (x, w)
  | otherwise = treeMin (leftOf w x) (rec "Left, and left again..." [x] w)

-- | いちばん右へ
treeMax :: Int -> World -> (Int, World)
treeMax x w
  | rightOf w x == nil = (x, w)
  | otherwise = treeMax (rightOf w x) (rec "Right, and right again..." [x] w)

successor :: Int -> World -> World
successor x0 w0 =
  let w1 = rec ("Find the successor (next larger key) of " ++ show x0) [x0] w0
      r = rightOf w1 x0
      climb x y w
        | y /= nil && x == rightOf w y = climb y (parentOf w y) (rec "Came up as a right child -> keep going up" [y] w)
        | y /= nil = rec ("Came up as a left child at " ++ show y ++ ": that is the successor") [y] w
        | otherwise = rec "It is the largest, so there is no successor" [] w
  in if r /= nil
       then
         let w2 = rec "There is a right subtree -> take its minimum" [r] w1
             (s, w3) = treeMin r w2
         in rec ("The successor is " ++ show s) [s] w3
       else climb x0 (parentOf w1 x0) w1

-- | なぞる：type 0 = 中間順、1 = 先行順、2 = 後行順。出力をためながらコマを記録する
walk :: Int -> Int -> ([Int], World) -> ([Int], World)
walk typ x st
  | x == nil = st
  | otherwise =
      let visit (out, w) =
            let out' = out ++ [x]
            in (out', recOut ("Visit: " ++ show x) [x] (Just out') w)
          w0 = snd st
          l = leftOf w0 x
          r = rightOf w0 x
      in st & (if typ == 1 then visit else id)
            & walk typ l
            & (if typ == 0 then visit else id)
            & walk typ r
            & (if typ == 2 then visit else id)

step :: Env -> Float -> World -> IO World
step _ _ w = pure w { anim = updateFrames (anim w) }

draw :: Env -> World -> Picture
draw env w =
  let style = defaultStyle { sFill = \_ _ -> rgb255 40 120 150 }
  in Pictures
       [ txt env 30 24 26 (gray 0.94) "Binary Search Tree: Search, Minimum, Successor, Walks"
       , txt env 30 58 14 (rgb255 150 170 200) "Number + Enter insert  Number + S search  N min  X max  Click successor  W walk"
       , txt env 30 76 14 (rgb255 150 170 200) "Space random  R first tree  C clear  P play/pause  <- -> step"
       , txt env 30 96 18 (gray 1) ("Input: " ++ buf w ++ (if envFrame env `mod` 60 < 30 then "|" else ""))
       , drawTreeFrames env style (anim w)
       , case currentFrame (anim w) >>= fOut of
           Just out -> txt env 30 (envH env - 120) 20 (rgb255 140 230 190) ("Output: " ++ unwords (map show out))
           Nothing -> Blank
       , drawFrameMessage env (anim w) (envH env - 80)
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    pure $ case nodeAt env (anim w) (toP5 env pos) of
      Just k -> successor k w { anim = skipToEnd (anim w) }
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
  SpecialKey KeyEnter | not (null (buf w)) -> pure (treeInsert True (readNum (buf w)) w) { buf = "" }
  Char 's' | not (null (buf w)) -> pure (treeSearch (readNum (buf w)) w) { buf = "" }
  Char 'n' | rootB w /= nil ->
    let (x, w1) = treeMin (rootB w) w
    in pure (rec ("The minimum is " ++ show x ++ " (the leftmost)") [x] w1)
  Char 'x' | rootB w /= nil ->
    let (x, w1) = treeMax (rootB w) w
    in pure (rec ("The maximum is " ++ show x ++ " (the rightmost)") [x] w1)
  Char 'w' ->
    let name = case drop (walkType w) walkNames of
                 (s : _) -> s
                 [] -> ""
        w1 = recOut ("Walk: " ++ name) [] (Just []) w
        (_, w2) = walk (walkType w) (rootB w1) ([], w1)
    in pure w2 { walkType = (walkType w + 1) `mod` 3 }
  SpecialKey KeySpace -> do
    k <- randomRIO (1, 99)
    pure (treeInsert True k w)
  Char 'r' -> pure (buildFirst w)
  Char 'c' -> pure (rec "An empty tree. Number + Enter to insert" [] w { nodes = IM.empty, rootB = nil, anim = clearFrames (anim w) })
  _ -> pure w

main :: IO ()
main = runSketch (sketch "11-08 Binary Search Tree" initialWorld)
  { skBack  = rgb255 16 22 30
  , skStep  = step
  , skDraw  = draw
  , skEvent = onEvent
  }

-- ---------- やってみよう ----------
-- 1. W で中間順にすると、出力が小さい順に並ぶ（二分探索木のいちばん大事な性質）
-- 2. 17 の後者は？（右の部分木がないので上がっていく → 21）
-- 3. 1, 2, 3, … と順番に入れると木がまっすぐ 1 本に。探索が O(n) になってしまう → 10 と 11 へ


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
