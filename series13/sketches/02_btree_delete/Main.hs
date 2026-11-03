-- ============================================
-- 02 B木からの削除 ─ 6 つのケース
-- 下りる前に「子のキーが t 個以上ある」ようにしておくのがコツ
-- ケース 1 葉から消す / 2a 先行キーで置換 / 2b 後続キーで置換 / 2c 併合 / 3a 兄弟から借りる（回転）/ 3b 兄弟と併合
-- キーをクリックで削除 / 数字 + Enter で削除 / スペースでランダムに削除 / I でランダムに挿入 / ←→ で t / R で新しい木
-- ============================================
module Main (main) where

import Art
import Data.Char (isDigit)
import Data.List (foldl', mapAccumL, sortOn)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

startT :: Int
startT = 2            -- ★ 最小次数 t（3 にすると太い木になる）

startN :: Int
startN = 28           -- ★ 最初のキーの数

speed :: Float
speed = 0.85          -- ★ 1 コマの秒数

-- ケースの一覧（名前と説明）
cases :: [(String, String)]
cases =
  [ ("1", "In a leaf: just remove")
  , ("2a", "Left >= t: use predecessor")
  , ("2b", "Right >= t: use successor")
  , ("2c", "Both t-1: merge children")
  , ("3a", "Sibling >= t: borrow a key")
  , ("3b", "Sibling t-1: merge with it")
  , ("shrink", "Root empty: tree shrinks")
  ]

data Node = Node { nKeys :: ![Int], nKids :: ![Int] }
type Nodes = IM.IntMap Node

data Hi = Hi { hNode :: !(Maybe Int), hTarget :: !(Maybe Int), hKey :: !(Maybe Int) }

noHi :: Hi
noHi = Hi Nothing Nothing Nothing

data Frame = Frame
  { fNodes :: !Nodes, fRoot :: !Int, fMsg :: !String, fHi :: !Hi
  , fTally :: !(M.Map String Int), fLast :: !String }

data Alg = Alg
  { aT :: !Int, aUid :: !Int, aNodes :: !Nodes, aRoot :: !Int
  , aTally :: !(M.Map String Int), aLast :: !String
  , aFrames :: ![Frame] }

data Op = OpDel !Int | OpIns !Int

data World = World
  { wAlg     :: !Alg
  , wCur     :: !Frame
  , wNext    :: ![Frame]
  , wTimer   :: !Float
  , wPending :: ![Op]
  , wKeyPos  :: !(M.Map Int Point)
  , wBoxPos  :: !(M.Map Int (Float, Float, Float))
  , wKw      :: !Float
  , wTyped   :: !String
  }

emptyAlg :: Int -> Alg
emptyAlg t = Alg t 1 (IM.singleton 0 (Node [] [])) 0 M.empty "" []

initWorld :: World
initWorld =
  let a = emptyAlg startT
  in World a (Frame (aNodes a) 0 "" noHi M.empty "") [] 0 [] M.empty M.empty 40 ""

main :: IO ()
main = runSketch (sketch "13-02 B-tree Delete" initWorld)
  { skBack  = hsb 200 35 9
  , skStart = \_ -> resetWorld startT
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

resetWorld :: Int -> IO World
resetWorld t = do
  ks <- take startN <$> shuffle [1 .. 99]
  let a1 = execM (mapM_ btInsert ks >> snap "Click a key to delete it" noHi) (emptyAlg t)
      f = case aFrames a1 of
            (x : _) -> x
            [] -> wCur initWorld
  pure initWorld { wAlg = a1 { aFrames = [] }, wCur = f }

-- ---------- 状態つきの計算（状態を次々に渡していくだけの小さなモナド）----------
newtype M a = M { runM :: Alg -> (a, Alg) }
instance Functor M where fmap f (M g) = M (\s -> let (a, s1) = g s in (f a, s1))
instance Applicative M where
  pure a = M (\s -> (a, s))
  M f <*> M g = M (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad M where M g >>= f = M (\s -> let (a, s1) = g s in runM (f a) s1)

execM :: M a -> Alg -> Alg
execM m s = snd (runM m s)

gets :: (Alg -> a) -> M a
gets f = M (\s -> (f s, s))

modify :: (Alg -> Alg) -> M ()
modify f = M (\s -> ((), f s))

snap :: String -> Hi -> M ()
snap msg hi = modify (\a -> a { aFrames = Frame (aNodes a) (aRoot a) msg hi (aTally a) (aLast a) : aFrames a })

node :: Int -> M Node
node i = gets (getNode i . aNodes)

getNode :: Int -> Nodes -> Node
getNode = IM.findWithDefault (Node [] [])

putNode :: Int -> Node -> M ()
putNode i n = modify (\a -> a { aNodes = IM.insert i n (aNodes a) })

newNode :: Node -> M Int
newNode n = do
  i <- gets aUid
  modify (\a -> a { aUid = i + 1 })
  putNode i n
  pure i

hasKey :: Nodes -> Int -> Bool
hasKey ns k = any (elem k . nKeys) (IM.elems ns)

allKeys :: Nodes -> Int -> [Int]
allKeys ns i = let n = getNode i ns in nKeys n ++ concatMap (allKeys ns) (nKids n)

kidAt :: Node -> Int -> Int
kidAt n i = maybe 0 id (safeAt (nKids n) i)

keyAt :: Node -> Int -> Int
keyAt n i = maybe 0 id (safeAt (nKeys n) i)

treeH :: Nodes -> Int -> Int
treeH ns x = case nKids (getNode x ns) of
  (c : _) -> 1 + treeH ns c
  [] -> 0

-- ---------- 挿入（木を用意するため）----------
btInsert :: Int -> M ()
btInsert k = do
  ns <- gets aNodes
  if hasKey ns k then pure () else do
    t <- gets aT
    r <- gets aRoot
    rn <- node r
    when' (length (nKeys rn) == 2 * t - 1) $ do
      s <- newNode (Node [] [r])
      modify (\a -> a { aRoot = s })
      splitChild s 0
    r2 <- gets aRoot
    insertNonfull r2 k

when' :: Bool -> M () -> M ()
when' c m = if c then m else pure ()

splitChild :: Int -> Int -> M ()
splitChild x i = do
  t <- gets aT
  xn <- node x
  let yi = kidAt xn i
  y <- node yi
  let (keep, rest) = splitAt (t - 1) (nKeys y)
      mid = case rest of { (m : _) -> m; [] -> 0 }
  z <- newNode (Node (drop 1 rest) (drop t (nKids y)))
  putNode yi y { nKeys = keep, nKids = take t (nKids y) }
  putNode x xn { nKeys = insertAt i mid (nKeys xn), nKids = insertAt (i + 1) z (nKids xn) }

insertNonfull :: Int -> Int -> M ()
insertNonfull x k = do
  t <- gets aT
  xn <- node x
  let i = length (takeWhile (k >) (nKeys xn))
  if null (nKids xn)
    then putNode x xn { nKeys = insertAt i k (nKeys xn) }
    else do
      cn <- node (kidAt xn i)
      if length (nKeys cn) == 2 * t - 1
        then do
          splitChild x i
          xn2 <- node x
          insertNonfull (kidAt xn2 (if keyAt xn2 i < k then i + 1 else i)) k
        else insertNonfull (kidAt xn i) k

-- ---------- 削除（下りる前に子のキーを t 個以上にしておく）----------
mark :: String -> M ()
mark c = modify (\a -> a { aTally = M.insertWith (+) c 1 (aTally a), aLast = c })

-- 部分木の一番大きいキー・一番小さいキー
maxKeyOf, minKeyOf :: Int -> M Int
maxKeyOf x = do
  n <- node x
  case nKids n of
    [] -> pure (if null (nKeys n) then 0 else last (nKeys n))
    ks -> maxKeyOf (last ks)
minKeyOf x = do
  n <- node x
  case nKids n of
    [] -> pure (keyAt n 0)
    (c : _) -> minKeyOf c

-- x の i 番目のキーをはさんで、i 番目と i+1 番目の子を 1 つにする。できた節点を返す
merge :: Int -> Int -> M Int
merge x i = do
  xn <- node x
  let yi = kidAt xn i
      zi = kidAt xn (i + 1)
  y <- node yi
  z <- node zi
  putNode yi y { nKeys = nKeys y ++ keyAt xn i : nKeys z, nKids = nKids y ++ nKids z }
  putNode x xn { nKeys = removeAt i (nKeys xn), nKids = removeAt (i + 1) (nKids xn) }
  modify (\a -> a { aNodes = IM.delete zi (aNodes a) })
  pure yi

deleteKey :: Int -> M ()
deleteKey k = do
  ns <- gets aNodes
  if not (hasKey ns k)
    then snap (show k ++ " is not in the tree") noHi
    else do
      modify (\a -> a { aLast = "" })
      r <- gets aRoot
      snap ("Delete " ++ show k ++ ": go down from the root") noHi { hNode = Just r, hTarget = Just k }
      del r k
      r2 <- gets aRoot
      rn <- node r2
      case (nKeys rn, nKids rn) of
        ([], c : _) -> do
          modify (\a -> a { aRoot = c, aNodes = IM.delete r2 (aNodes a) })
          mark "shrink"
          snap "Root became empty: its only child is the new root (one level shorter)" noHi { hNode = Just c }
        _ -> pure ()

del :: Int -> Int -> M ()
del x k = do
  t <- gets aT
  xn <- node x
  let i = length (takeWhile (k >) (nKeys xn))
      isLeaf = null (nKids xn)
  if safeAt (nKeys xn) i == Just k
    then
      if isLeaf
        then do
          putNode x xn { nKeys = removeAt i (nKeys xn) }
          mark "1"
          snap ("Case 1: " ++ show k ++ " is in a leaf, so just remove it") noHi { hNode = Just x }
        else do
          let yi = kidAt xn i
              zi = kidAt xn (i + 1)
          y <- node yi
          z <- node zi
          if length (nKeys y) >= t
            then do
              kp <- maxKeyOf yi
              mark "2a"
              snap ("Case 2a: left child has >= " ++ show t ++ " keys: find predecessor " ++ show kp) noHi { hNode = Just x, hTarget = Just k, hKey = Just kp }
              putNode x xn { nKeys = setAt i kp (nKeys xn) }
              snap ("Replace " ++ show k ++ " with " ++ show kp ++ ", then delete " ++ show kp ++ " from the left subtree") noHi { hNode = Just yi, hKey = Just kp }
              del yi kp
            else if length (nKeys z) >= t
              then do
                ks <- minKeyOf zi
                mark "2b"
                snap ("Case 2b: right child has >= " ++ show t ++ " keys: find successor " ++ show ks) noHi { hNode = Just x, hTarget = Just k, hKey = Just ks }
                putNode x xn { nKeys = setAt i ks (nKeys xn) }
                snap ("Replace " ++ show k ++ " with " ++ show ks ++ ", then delete " ++ show ks ++ " from the right subtree") noHi { hNode = Just zi, hKey = Just ks }
                del zi ks
              else do
                mark "2c"
                snap ("Case 2c: both children have only " ++ show (t - 1) ++ " keys: merge them") noHi { hNode = Just x, hTarget = Just k }
                _ <- merge x i
                snap ("Merged 2 children with " ++ show k ++ " in the middle (" ++ show (2 * t - 1) ++ " keys)") noHi { hNode = Just yi, hTarget = Just k }
                del yi k
    else if isLeaf
      then pure ()
      else do
        let ci = kidAt xn i
        c <- node ci
        target <- if length (nKeys c) == t - 1 then fixChild x i ci k else pure ci
        snap "Go down to the child" noHi { hNode = Just target, hTarget = Just k }
        del target k

-- 下りる子のキーが t−1 個しかないとき：借りるか併合する（ケース 3）。下りる節点を返す
fixChild :: Int -> Int -> Int -> Int -> M Int
fixChild x i ci k = do
  t <- gets aT
  xn <- node x
  c <- node ci
  let lsI = if i > 0 then safeAt (nKids xn) (i - 1) else Nothing
      rsI = safeAt (nKids xn) (i + 1)
  ls <- maybe (pure Nothing) (fmap Just . node) lsI
  rs <- maybe (pure Nothing) (fmap Just . node) rsI
  let rich = maybe False (\s -> length (nKeys s) >= t)
      done = snap "A parent key moved down, a sibling key moved up (rotation)" noHi { hNode = Just ci, hTarget = Just k }
  case () of
    _ | rich ls, Just li <- lsI, Just l <- ls -> do
          mark "3a"
          snap ("Case 3a: child has " ++ show (t - 1) ++ " keys: borrow from the left sibling") noHi { hNode = Just ci, hTarget = Just k }
          let borrowed = last (nKeys l)
              moved = [last (nKids l) | not (null (nKids l))]
          putNode ci c { nKeys = keyAt xn (i - 1) : nKeys c, nKids = moved ++ nKids c }
          putNode x xn { nKeys = setAt (i - 1) borrowed (nKeys xn) }
          putNode li l { nKeys = init (nKeys l), nKids = if null (nKids l) then [] else init (nKids l) }
          done
          pure ci
      | rich rs, Just ri <- rsI, Just r <- rs -> do
          mark "3a"
          snap ("Case 3a: child has " ++ show (t - 1) ++ " keys: borrow from the right sibling") noHi { hNode = Just ci, hTarget = Just k }
          putNode ci c { nKeys = nKeys c ++ [keyAt xn i], nKids = nKids c ++ take 1 (nKids r) }
          putNode x xn { nKeys = setAt i (keyAt r 0) (nKeys xn) }
          putNode ri r { nKeys = drop 1 (nKeys r), nKids = drop 1 (nKids r) }
          done
          pure ci
      | otherwise -> do
          mark "3b"
          snap ("Case 3b: child and sibling have " ++ show (t - 1) ++ " keys: merge using a parent key") noHi { hNode = Just ci, hTarget = Just k }
          m <- case rsI of
            Just _ -> merge x i
            Nothing -> merge x (i - 1)
          snap "Merged. Now it is safe to go down" noHi { hNode = Just m, hTarget = Just k }
          pure m

-- ---------- 配置 ----------
data Box = Box { bId :: !Int, bX, bY, bW :: !Float, bN :: !Int }

data Layout = Layout
  { lBoxes :: [Box]
  , lEdges :: [(Int, Int, Int)]
  , lKeys  :: M.Map Int Point
  , lById  :: M.Map Int Box
  , lKw    :: !Float
  }

data Placed = Placed { pId :: !Int, pC :: !Float, pD :: !Int, pKids :: [Placed] }

layoutOf :: Env -> Nodes -> Int -> Layout
layoutOf env ns root = Layout boxes edges (M.fromList keys) (M.fromList [(bId b, b) | b <- boxes]) (kw0 * s)
  where
    kw0 = 42
    gap = 20
    n i = getNode i ns
    wid i =
      let own = fromIntegral (max 1 (length (nKeys (n i)))) * kw0 + gap
      in if null (nKids (n i)) then own else max own (sum (map wid (nKids (n i))))
    bigW = wid root
    s = min 1.3 ((envW env - 380) / bigW)
    lh = min 120 ((envH env - 300) / fromIntegral (max 1 (treeH ns root)))
    place i left d
      | null (nKids (n i)) = Placed i (left + w / 2) d []
      | otherwise =
          let total = sum (map wid (nKids (n i)))
              (_, kids) = mapAccumL (\l c -> (l + wid c, place c l (d + 1))) (left + (w - total) / 2) (nKids (n i))
          in Placed i (midOf (left + w / 2) (map pC kids)) d kids
      where w = wid i
    every p = p : concatMap every (pKids p)
    nodes = every (place root 0 (0 :: Int))
    sx c = (envW env - 340) / 2 + (c - bigW / 2) * s
    sy d = 190 + fromIntegral d * lh
    boxW p = fromIntegral (max 1 (length (nKeys (n (pId p))))) * kw0 * s
    boxes = [ Box (pId p) (sx (pC p)) (sy (pD p)) (boxW p) (length (nKeys (n (pId p)))) | p <- nodes ]
    keys = [ (k, (sx (pC p) - boxW p / 2 + (fromIntegral j + 0.5) * kw0 * s, sy (pD p)))
           | p <- nodes, (j, k) <- zip [0 :: Int ..] (nKeys (n (pId p))) ]
    edges = [ (pId p, j, pId c) | p <- nodes, (j, c) <- zip [0 ..] (pKids p) ]

-- ---------- 時間を進める ----------
stepWorld :: Env -> Float -> World -> IO World
stepWorld env dt w0 = do
  let t1 = wTimer w0 + dt
      w1 = case wNext w0 of
             (f : rest) | t1 > speed -> w0 { wCur = f, wNext = rest, wTimer = 0 }
             _ -> w0 { wTimer = t1 }
      w2 = case wPending w1 of
             (p : ps) | null (wNext w1) && wTimer w1 > speed * 0.5 -> runOp p w1 { wPending = ps, wTimer = 0 }
             _ -> w1
  pure (moveAll env w2)

runOp :: Op -> World -> World
runOp op w =
  let m = case op of
        OpDel k -> deleteKey k
        OpIns k -> btInsert k >> snap ("Inserted " ++ show k) noHi { hKey = Just k }
      a = execM m (wAlg w)
  in w { wAlg = a { aFrames = [] }, wNext = wNext w ++ reverse (aFrames a) }

moveAll :: Env -> World -> World
moveAll env w =
  let f = wCur w
      lay = layoutOf env (fNodes f) (fRoot f)
      boxes = M.fromList
        [ (bId b, (lerpF ox (bX b) 0.15, lerpF oy (bY b) 0.15, lerpF ow (bW b) 0.15))
        | b <- lBoxes lay, let (ox, oy, ow) = M.findWithDefault (bX b, bY b, bW b) (bId b) (wBoxPos w) ]
      keys = M.mapWithKey (\k t -> lerpP 0.15 (M.findWithDefault t k (wKeyPos w)) t) (lKeys lay)
  in w { wBoxPos = boxes, wKeyPos = keys, wKw = lKw lay }

-- ---------- 描画 ----------
drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (edgePics ++ concatMap boxPic (lBoxes lay) ++ keyPics ++ table ++ panel)
  where
    f = wCur w
    hi = fHi f
    t = aT (wAlg w)
    lay = layoutOf env (fNodes f) (fRoot f)
    kw = lKw lay
    mouse = mouseXY env
    bp i = M.findWithDefault (0, 0, 0) i (wBoxPos w)
    edgePics =
      [ lineP env 2 (hsb 200 25 45) (px - pw / 2 + fromIntegral j * (pw / fromIntegral n), py + 18) (cx, cy - 18)
      | (pid, j, cid) <- lEdges lay
      , let (px, py, pw) = bp pid
            (cx, cy, _) = bp cid
            n = max 1 (maybe 1 bN (M.lookup pid (lById lay))) ]
    boxPic b =
      let (x, y, bw) = bp (bId b)
          on = hNode hi == Just (bId b)
          thin = bN b == t - 1 && bId b /= fRoot f
          edge | on = hsb 50 90 100
               | thin = hsb 280 50 75
               | otherwise = hsb 200 30 55
      in [ fillBox env (if on then hsb 200 40 30 else hsb 200 30 18) (x - bw / 2) (y - 18) bw 36
         , frameBox env edge (if on then 3.5 else 1.5) (x - bw / 2) (y - 18) bw 36 ]
         ++ [ lineP env 1 (hsb 200 25 35) (sepX, y - 14) (sepX, y + 14)
            | j <- [1 .. bN b - 1], let sepX = x - bw / 2 + fromIntegral j * (bw / fromIntegral (bN b)) ]
    ts = clampF 14 18 (kw * 0.42)
    keyPics = concat
      [ [ discP env (if tg then hsb 355 75 90 else if isHi then hsb 50 85 100 else hsba 0 0 100 0.25) p 15 | tg || isHi || hover ]
        ++ [ txtFit env (fst p) (snd p) ts (kw - 2) (if isHi then hsb 0 0 10 else hsb 0 0 100) (show k) ]
      | k <- M.keys (lKeys lay)
      , let p = M.findWithDefault (0, 0) k (wKeyPos w)
            tg = hTarget hi == Just k
            isHi = hKey hi == Just k
            hover = dist2 mouse p < kw / 2 ]
    px = envW env - 330
    table =
      txt env px 130 16 (hsb 0 0 100) "Cases (times used)" : concat
      [ [ fillBox env (if on then hsb 50 85 100 else hsb 200 25 22) px y 310 46
        , txt env (px + 10) (y + 4) 15 (if on then hsb 0 0 10 else hsb 0 0 100) ((if c == "shrink" then "root" else c) ++ "  x " ++ show (M.findWithDefault 0 c (fTally f)))
        , txt env (px + 10) (y + 25) 14 (if on then hsb 0 0 20 else hsb 0 0 75) d ]
      | (j, (c, d)) <- zip [0 :: Int ..] cases
      , let y = 160 + fromIntegral j * 52
            on = fLast f == c ]
    panel =
      [ txt env 24 18 26 (hsb 0 0 100) "B-tree delete"
      , txt env 24 56 15 (hsb 0 0 80)
          ("t = " ++ show t ++ ": non-root nodes hold " ++ show (t - 1) ++ ".." ++ show (2 * t - 1) ++ " keys   keys " ++ show (length (allKeys (fNodes f) (fRoot f)))
           ++ "   height " ++ show (treeH (fNodes f) (fRoot f)) ++ "   purple border = minimum (t-1 keys)")
      , txt env 24 86 18 (hsb 50 80 100) (fMsg f)
      , txt env 24 (envH env - 74) 14 (hsb 0 0 60) ("Typing: " ++ (if null (wTyped w) then "-" else wTyped w))
      , txt env 24 (envH env - 52) 14 (hsb 0 0 60) "Click a key = delete / Digits + Enter = delete / Space = random delete"
      , txt env 24 (envH env - 30) 14 (hsb 0 0 60) "I = random insert / Left, Right = t / R = new tree"
      ]

-- ---------- マウスとキー ----------
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseXY env
        near = [k | (k, p) <- M.toList (wKeyPos w), dist2 m p < max 12 (wKw w / 2)]
    in pure $ case near of
         (k : _) -> w { wPending = wPending w ++ [OpDel k] }
         [] -> w
  EventKey (Char c) Down _ _ | isDigit c -> pure w { wTyped = lastN 3 (wTyped w ++ [c]) }
  EventKey k Down _ _ | isBackKey k -> pure w { wTyped = take (length (wTyped w) - 1) (wTyped w) }
  EventKey (SpecialKey KeyEnter) Down _ _ | not (null (wTyped w)) ->
    pure w { wPending = wPending w ++ [OpDel (digitsToInt (wTyped w))], wTyped = "" }
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    k <- pick (allKeys (aNodes (wAlg w)) (aRoot (wAlg w)))
    pure (maybe w (\x -> w { wPending = wPending w ++ [OpDel x] }) k)
  EventKey (SpecialKey KeyRight) Down _ _ -> resetWorld (min 4 (aT (wAlg w) + 1))
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetWorld (max 2 (aT (wAlg w) - 1))
  EventKey (Char c) Down _ _
    | c `elem` "iI" -> do
        k <- randomRIO (1, 99)
        pure w { wPending = wPending w ++ [OpIns k] }
    | c `elem` "rR" -> resetWorld (aT (wAlg w))
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. ケース 2c と 3b が起きるのはどんなとき？ 紫の枠（ぎりぎりの節点）に注目して狙ってみよう
-- 2. 全部のキーを消していくと、木はどこで低くなる？ 表の root の回数を見よう
-- 3. startT を 3 にして、ケースの回数がどう変わるか比べよう

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左上が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx (gy - size) size c s

-- 中央揃えの文字（中心が x, y）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = let (gx, gy) = toG env x y in labelC gx gy size c s

-- 太さのある線分
lineP :: Env -> Float -> Color -> Point -> Point -> Picture
lineP env w c (x1, y1) (x2, y2) = Color c (thickLine w (toG env x1 y1) (toG env x2 y2))

-- 太さのある折れ線
pathP :: Env -> Float -> Color -> [Point] -> Picture
pathP env w c ps = Color c (thickPath w [toG env x y | (x, y) <- ps])

-- 塗りつぶした長方形（x, y は左上）
fillBox :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
fillBox env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 長方形の枠（x, y は左上、sw は線の太さ）
frameBox :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
frameBox env c sw x y w h = pathP env sw c [(x - sw / 2, y), (x + w + sw / 2, y), (x + w, y), (x + w, y + h), (x + w + sw / 2, y + h), (x - sw / 2, y + h), (x, y + h), (x, y)]

-- 塗りつぶした円（p は中心、r は半径）
discP :: Env -> Color -> Point -> Float -> Picture
discP env c (x, y) r = Color c (at (toG env x y) (disc (max 0.1 r)))

-- 2 点の間を t の割合で進む（p5.js の lerp を x, y に）
lerpP :: Float -> Point -> Point -> Point
lerpP t (a, b) (c, d) = (lerpF a c t, lerpF b d t)

-- 数字の文字列を整数にする（空なら 0）
digitsToInt :: String -> Int
digitsToInt = foldl (\a c -> a * 10 + (fromEnum c - fromEnum '0')) 0

-- 文字列の後ろ n 文字
lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

-- 消すキーか（Mac の delete キーは KeyDelete になる）
isBackKey :: Key -> Bool
isBackKey k = k `elem` [SpecialKey KeyBackspace, SpecialKey KeyDelete, Char '\b', Char '\DEL']

-- 列の i 番目（なければ Nothing）
safeAt :: [a] -> Int -> Maybe a
safeAt xs i = if i < 0 then Nothing else case drop i xs of
  (x : _) -> Just x
  [] -> Nothing

-- 列の i 番目に v を入れる
insertAt :: Int -> a -> [a] -> [a]
insertAt i v xs = let (a, b) = splitAt i xs in a ++ v : b

-- 列の i 番目を除く
removeAt :: Int -> [a] -> [a]
removeAt i xs = let (a, b) = splitAt i xs in a ++ drop 1 b

-- 列の i 番目を v に置きかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = let (a, b) = splitAt i xs in a ++ v : drop 1 b

-- 列をランダムに並べ直す
shuffle :: [a] -> IO [a]
shuffle xs = do
  rs <- mapM (const (randomRIO (0, 1 :: Double))) xs
  pure (map snd (sortOn fst (zip rs xs)))

-- 列からランダムに 1 つ選ぶ
pick :: [a] -> IO (Maybe a)
pick [] = pure Nothing
pick xs = do
  i <- randomRIO (0, length xs - 1)
  pure (safeAt xs i)

-- 子の中心の真ん中（子がなければ d）
midOf :: Float -> [Float] -> Float
midOf d [] = d
midOf _ xs@(x : _) = (x + last xs) / 2

-- マスの中に入る文字（幅 maxW を超えるときは横に縮める）
txtFit :: Env -> Float -> Float -> Float -> Float -> Color -> String -> Picture
txtFit env x y size maxW c s =
  let (gx, gy) = toG env x y
      w = fromIntegral (length s) * size * 0.9
      k = min 1 (maxW / max 1 w)
  in Translate gx gy (Scale k 1 (labelC 0 0 size c s))
