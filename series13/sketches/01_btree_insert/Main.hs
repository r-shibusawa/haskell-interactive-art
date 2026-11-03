-- ============================================
-- 01 B木への挿入 ─ 下りながら先回りして分割する
-- 満杯（2t−1 個）の節点に出会ったら、下りる前に真ん中のキーを親へ上げて 2 つに割る
-- だから葉に着いたとき必ず空きがある。木は「根が割れたとき」だけ 1 段高くなる ─ 上に伸びる木
-- 数字 + Enter で挿入 / スペースでランダムに挿入 / A で自動挿入 / ←→ で t / P で一時停止・N で 1 コマ進む / R でやり直し
-- ============================================
module Main (main) where

import Art
import Data.Char (isDigit)
import Data.List (mapAccumL)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

startT :: Int
startT = 2            -- ★ 最小次数 t（2 なら 2-3-4 木）

speed :: Float
speed = 0.7           -- ★ 1 コマの秒数

maxKey :: Int
maxKey = 99           -- ★ ランダムなキーの最大値

-- 節点は番号で管理する（IntMap が「ディスク」の代わり）。子がなければ葉
data Node = Node { nKeys :: ![Int], nKids :: ![Int] }
type Nodes = IM.IntMap Node

-- コマの強調（どの節点、どのキー、運んでいるキー、満杯か）
data Hi = Hi { hNode :: !(Maybe Int), hKey :: !(Maybe Int), hCarry :: !(Maybe Int), hFull :: !Bool }

noHi :: Hi
noHi = Hi Nothing Nothing Nothing False

-- アニメーションの 1 コマ。IntMap は変更しても昔の版が残るので、そのまま保存できる
data Frame = Frame
  { fNodes :: !Nodes, fRoot :: !Int, fMsg :: !String, fHi :: !Hi
  , fDiskR, fDiskW, fSplits :: !Int }

-- 挿入アルゴリズムの状態
data Alg = Alg
  { aT :: !Int, aUid :: !Int, aNodes :: !Nodes, aRoot :: !Int
  , aDiskR, aDiskW, aSplits :: !Int
  , aFrames :: ![Frame] }   -- 新しいコマが先頭

data World = World
  { wAlg     :: !Alg
  , wCur     :: !Frame      -- いま見せているコマ
  , wNext    :: ![Frame]    -- これから見せるコマ
  , wTimer   :: !Float
  , wPaused, wAuto :: !Bool
  , wPending :: ![Int]
  , wKeyPos  :: !(M.Map Int Point)
  , wBoxPos  :: !(M.Map Int (Float, Float, Float))
  , wTyped   :: !String
  }

main :: IO ()
main = runSketch (sketch "13-01 B-tree Insert" (resetWorld startT))
  { skBack  = hsb 250 30 10
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

resetWorld :: Int -> World
resetWorld t =
  let a0 = Alg t 1 (IM.singleton 0 (Node [] [])) 0 0 0 0 []
      a1 = execM (snap "Empty B-tree. Insert some keys!" noHi) a0
      f = case aFrames a1 of
            (x : _) -> x
            [] -> Frame (aNodes a1) 0 "" noHi 0 0 0
  in World a1 { aFrames = [] } f [] 0 False False [] M.empty M.empty ""

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
snap msg hi = modify (\a -> a { aFrames = Frame (aNodes a) (aRoot a) msg hi (aDiskR a) (aDiskW a) (aSplits a) : aFrames a })

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

-- ---------- 挿入（各段階をコマとして記録）----------
insertKey :: Int -> M ()
insertKey k = do
  ns <- gets aNodes
  if hasKey ns k
    then snap (show k ++ " is already in the tree (no duplicate keys)") noHi
    else do
      r <- gets aRoot
      t <- gets aT
      snap ("Insert " ++ show k ++ ": go down from the root") noHi { hNode = Just r, hCarry = Just k }
      rn <- node r
      if length (nKeys rn) == 2 * t - 1
        then do
          snap ("Root is full (" ++ show (2 * t - 1) ++ " keys): split it before going down") noHi { hNode = Just r, hCarry = Just k, hFull = True }
          s <- newNode (Node [] [r])
          modify (\a -> a { aRoot = s })
          mid <- splitChild s 0
          snap ("Root split: the middle key " ++ show mid ++ " is the new root. The tree grew one level") noHi { hNode = Just s, hKey = Just mid, hCarry = Just k }
          insertNonfull s k
        else insertNonfull r k

-- x の i 番目の子を 2 つに割り、上がったキーを返す
splitChild :: Int -> Int -> M Int
splitChild x i = do
  t <- gets aT
  xn <- node x
  let yi = maybe 0 id (safeAt (nKids xn) i)
  y <- node yi
  let (keep, rest) = splitAt (t - 1) (nKeys y)
      mid = case rest of { (m : _) -> m; [] -> 0 }
  z <- newNode (Node (drop 1 rest) (drop t (nKids y)))
  putNode yi y { nKeys = keep, nKids = take t (nKids y) }
  putNode x xn { nKeys = insertAt i mid (nKeys xn), nKids = insertAt (i + 1) z (nKids xn) }
  modify (\a -> a { aDiskW = aDiskW a + 3, aSplits = aSplits a + 1 })   -- y, z, x を書き戻す
  pure mid

insertNonfull :: Int -> Int -> M ()
insertNonfull x k = do
  t <- gets aT
  xn <- node x
  let i = length (takeWhile (k >) (nKeys xn))
  if null (nKids xn)
    then do
      putNode x xn { nKeys = insertAt i k (nKeys xn) }
      modify (\a -> a { aDiskW = aDiskW a + 1 })
      snap ("Put " ++ show k ++ " in the leaf (it is guaranteed to have room)") noHi { hNode = Just x, hKey = Just k }
    else do
      let c = maybe 0 id (safeAt (nKids xn) i)
      cn <- node c
      modify (\a -> a { aDiskR = aDiskR a + 1 })
      snap (show k ++ " goes to child number " ++ show i ++ " (DISK-READ)") noHi { hNode = Just c, hCarry = Just k }
      if length (nKeys cn) == 2 * t - 1
        then do
          snap "This child is full: split before going down" noHi { hNode = Just c, hCarry = Just k, hFull = True }
          mid <- splitChild x i
          snap ("Split: " ++ show mid ++ " moved up to the parent; now there are 2 children") noHi { hNode = Just x, hKey = Just mid, hCarry = Just k }
          xn2 <- node x
          let i2 = if maybe False (k >) (safeAt (nKeys xn2) i) then i + 1 else i
              c2 = maybe 0 id (safeAt (nKids xn2) i2)
          snap (show k ++ " goes to child number " ++ show i2) noHi { hNode = Just c2, hCarry = Just k }
          insertNonfull c2 k
        else insertNonfull c k

treeH :: Nodes -> Int -> Int
treeH ns x = case nKids (getNode x ns) of
  (c : _) -> 1 + treeH ns c
  [] -> 0

-- ---------- 配置 ----------
data Box = Box { bId :: !Int, bX, bY, bW :: !Float, bN :: !Int }

data Layout = Layout
  { lBoxes :: [Box]
  , lEdges :: [(Int, Int, Int)]     -- 親、何番目、子
  , lKeys  :: M.Map Int Point
  , lById  :: M.Map Int Box
  , lKw    :: !Float
  }

data Placed = Placed { pId :: !Int, pC :: !Float, pD :: !Int, pKids :: [Placed] }

layoutOf :: Env -> Nodes -> Int -> Layout
layoutOf env ns root = Layout boxes edges (M.fromList keys) (M.fromList [(bId b, b) | b <- boxes]) (kw0 * s)
  where
    kw0 = 44
    gap = 20
    n i = getNode i ns
    wid i =
      let own = fromIntegral (length (nKeys (n i))) * kw0 + gap
      in if null (nKids (n i)) then own else max own (sum (map wid (nKids (n i))))
    bigW = wid root
    s = min 1.3 ((envW env - 60) / bigW)
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
    sx c = envW env / 2 + (c - bigW / 2) * s
    sy d = 190 + fromIntegral d * lh
    boxW p = fromIntegral (max 1 (length (nKeys (n (pId p))))) * kw0 * s
    boxes = [ Box (pId p) (sx (pC p)) (sy (pD p)) (boxW p) (length (nKeys (n (pId p)))) | p <- nodes ]
    keys = [ (k, (sx (pC p) - boxW p / 2 + (fromIntegral j + 0.5) * kw0 * s, sy (pD p)))
           | p <- nodes, (j, k) <- zip [0 :: Int ..] (nKeys (n (pId p))) ]
    edges = [ (pId p, j, pId c) | p <- nodes, (j, c) <- zip [0 ..] (pKids p) ]

-- ---------- 時間を進める ----------
stepWorld :: Env -> Float -> World -> IO World
stepWorld env dt w0 = do
  let t1 = if wPaused w0 then wTimer w0 else wTimer w0 + dt
      w1 = case wNext w0 of
             (f : rest) | not (wPaused w0) && t1 > speed -> w0 { wCur = f, wNext = rest, wTimer = 0 }
             _ -> w0 { wTimer = t1 }
  -- 最後のコマまで見せたら、次の挿入を始める
  w2 <- if null (wNext w1) && wTimer w1 > speed * 0.6
          then case wPending w1 of
                 (k : ks) -> pure (runInsert k w1 { wPending = ks, wTimer = 0 })
                 [] | wAuto w1 -> do
                        k <- randomRIO (1, maxKey)
                        pure (runInsert k w1 { wTimer = 0 })
                    | otherwise -> pure w1
          else pure w1
  pure (moveAll env w2)

runInsert :: Int -> World -> World
runInsert k w =
  let a = execM (insertKey k) (wAlg w)
  in w { wAlg = a { aFrames = [] }, wNext = wNext w ++ reverse (aFrames a) }

-- キーと箱を目標の位置へ少しずつ動かす
moveAll :: Env -> World -> World
moveAll env w =
  let f = wCur w
      lay = layoutOf env (fNodes f) (fRoot f)
      boxes = M.fromList
        [ (bId b, (lerpF ox (bX b) 0.15, lerpF oy (bY b) 0.15, lerpF ow (bW b) 0.15))
        | b <- lBoxes lay, let (ox, oy, ow) = M.findWithDefault (bX b, bY b, bW b) (bId b) (wBoxPos w) ]
      keys0 = M.mapWithKey (\k t -> lerpP 0.15 (M.findWithDefault (fst t, snd t - 60) k (wKeyPos w)) t) (lKeys lay)
      -- 運んでいるキー（節点の右上についていく）
      carried = case (hCarry (fHi f), hNode (fHi f) >>= (`M.lookup` boxes)) of
        (Just k, Just (bx, by, bw)) | M.notMember k keys0 ->
          let t = (bx + bw / 2 + 30, by - 34)
          in M.insert k (lerpP 0.15 (M.findWithDefault (envW env / 2, 110) k (wKeyPos w)) t) keys0
        _ -> keys0
  in w { wBoxPos = boxes, wKeyPos = carried }

-- ---------- 描画 ----------
drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (edgePics ++ concatMap boxPic (lBoxes lay) ++ keyPics ++ carryPic ++ panel)
  where
    f = wCur w
    hi = fHi f
    t = aT (wAlg w)
    lay = layoutOf env (fNodes f) (fRoot f)
    kw = lKw lay
    bp i = M.findWithDefault (0, 0, 0) i (wBoxPos w)
    edgePics =
      [ lineP env 2 (hsb 230 25 45) (px - pw / 2 + fromIntegral j * kw * (pw / max 1 bw), py + 18) (cx, cy - 18)
      | (pid, j, cid) <- lEdges lay
      , let (px, py, pw) = bp pid
            (cx, cy, _) = bp cid
            bw = maybe 1 bW (M.lookup pid (lById lay)) ]
    boxPic b =
      let (x, y, bw) = bp (bId b)
          on = hNode hi == Just (bId b)
          full = bN b == 2 * t - 1
          edge | on = if hFull hi then hsb 15 90 100 else hsb 50 90 100
               | full = hsb 25 70 80
               | otherwise = hsb 230 30 55
      in [ fillBox env (if on then hsb 250 35 30 else hsb 250 25 20) (x - bw / 2) (y - 18) bw 36
         , frameBox env edge (if on then 3.5 else 1.5) (x - bw / 2) (y - 18) bw 36 ]
         ++ [ lineP env 1 (hsb 230 25 35) (sepX, y - 14) (sepX, y + 14)
            | j <- [1 .. bN b - 1], let sepX = x - bw / 2 + fromIntegral j * (bw / fromIntegral (bN b)) ]
    ts = clampF 14 18 (kw * 0.42)
    keyPics = concat
      [ [ discP env (hsb 50 85 100) p 15 | isHi ] ++ [ txtFit env (fst p) (snd p) ts (kw - 2) (if isHi then hsb 0 0 10 else hsb 0 0 100) (show k) ]
      | k <- M.keys (lKeys lay)
      , let p = M.findWithDefault (0, 0) k (wKeyPos w)
            isHi = hKey hi == Just k ]
    carryPic = case hCarry hi of
      Just k | M.notMember k (lKeys lay), Just p <- M.lookup k (wKeyPos w) ->
        [ discP env (hsb 50 85 100) p 17, txtC env (fst p) (snd p) 16 (hsb 0 0 10) (show k) ]
      _ -> []
    countKeys = sum (map (length . nKeys) (reachable (fNodes f) (fRoot f)))
    panel =
      [ txt env 24 18 26 (hsb 0 0 100) "B-tree insert (split on the way down)"
      , txt env 24 56 14 (hsb 0 0 80)
          ("t = " ++ show t ++ ": max 2t-1 = " ++ show (2 * t - 1) ++ " keys" ++ (if t == 2 then " (2-3-4 tree)" else "")
           ++ "   n = " ++ show countKeys ++ "   height " ++ show (treeH (fNodes f) (fRoot f))
           ++ "   splits " ++ show (fSplits f) ++ "   DISK-READ " ++ show (fDiskR f) ++ "   DISK-WRITE " ++ show (fDiskW f))
      , txt env 24 86 18 (hsb 50 80 100) (fMsg f)
      , txt env 24 (envH env - 74) 14 (hsb 0 0 60)
          ("Typing: " ++ (if null (wTyped w) then "-" else wTyped w) ++ "   " ++ (if wAuto w then "Auto insert  " else "") ++ (if wPaused w then "Paused  " else "") ++ "  Orange border = full node")
      , txt env 24 (envH env - 52) 14 (hsb 0 0 60) "Digits + Enter = insert / Space = random / A = auto / Left, Right = t"
      , txt env 24 (envH env - 30) 14 (hsb 0 0 60) "P = pause / N = next frame / R = reset"
      ]

reachable :: Nodes -> Int -> [Node]
reachable ns i = let n = getNode i ns in n : concatMap (reachable ns) (nKids n)

-- ---------- キー ----------
onEvent :: Env -> Event -> World -> IO World
onEvent _ (EventKey (SpecialKey KeySpace) Down _ _) w = do
  k <- randomRIO (1, maxKey)
  pure w { wPending = wPending w ++ [k] }
onEvent _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _ | isDigit c -> w { wTyped = lastN 3 (wTyped w ++ [c]) }
  EventKey k Down _ _ | isBackKey k -> w { wTyped = take (length (wTyped w) - 1) (wTyped w) }
  EventKey (SpecialKey KeyEnter) Down _ _ | not (null (wTyped w)) ->
    w { wPending = wPending w ++ [digitsToInt (wTyped w)], wTyped = "" }
  EventKey (SpecialKey KeyRight) Down _ _ -> resetWorld (min 4 (aT (wAlg w) + 1))
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetWorld (max 2 (aT (wAlg w) - 1))
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "pP" -> w { wPaused = not (wPaused w) }
    | c `elem` "nN" -> case wNext w of
        (f : rest) -> w { wCur = f, wNext = rest }
        [] -> w
    | c `elem` "rR" -> resetWorld (aT (wAlg w))
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1, 2, 3, 4 ... と小さい順に入れてみよう。分割はどこで起きる？ ランダムな順と比べよう
-- 2. startT を 3 や 4 にして A で自動挿入。高さの伸びはどう変わる？
-- 3. fDiskR と fDiskW の増え方を、キーの数 n の対数と比べてみよう

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

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
