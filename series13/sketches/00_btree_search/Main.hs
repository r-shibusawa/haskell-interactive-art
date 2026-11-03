-- ============================================
-- 00 B木の探索 ─ ディスクにやさしい「太い」木
-- 1 つの節点にキーを最大 2t−1 個つめこみ、どの葉も同じ深さにそろえた木
-- 高さはおよそ log_t n。ディスクを読む回数（= 訪れる節点の数）がとても少ない
-- 数字を打って Enter で探索 / スペースでランダムに探索 / ←→ で最小次数 t / V で赤黒木に変身（t = 2 のとき）/ R で作り直す
-- ============================================
module Main (main) where

import Art
import Data.Char (isDigit)
import Data.List (foldl', mapAccumL, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Printf (printf)

startT :: Int
startT = 3            -- ★ 最小次数 t（根以外の節点のキーは t−1〜2t−1 個）

keyCount :: Int
keyCount = 40         -- ★ キーの数

speed :: Float
speed = 0.45          -- ★ 探索アニメーションの 1 コマの秒数

-- B木の節点。子がなければ葉
data Node = Node { nId :: !Int, nKeys :: ![Int], nKids :: ![Node] }

-- 探索の 1 コマ（どの節点の、どのキーと比べて、結果はどうだったか）
data Step = Step { stId :: !Int, stKey :: !(Maybe Int), stCmp :: !String }

data World = World
  { wT      :: !Int
  , wRoot   :: !Node
  , wTyped  :: !String
  , wPath   :: ![Step]
  , wPi     :: !Int
  , wTimer  :: !Float
  , wTarget :: !(Maybe Int)
  , wResult :: !String
  , wRb     :: !Bool
  , wKeyPos :: !(M.Map Int Point)                -- キーの今の位置
  , wBoxPos :: !(M.Map Int (Float, Float, Float)) -- 箱の今の位置と幅
  }

main :: IO ()
main = runSketch (sketch "13-00 B-tree Search" (World startT (Node 0 [] []) "" [] 0 0 Nothing "" False M.empty M.empty))
  { skBack  = hsb 225 35 10
  , skStart = \_ -> rebuild (World startT (Node 0 [] []) "" [] 0 0 Nothing "" False M.empty M.empty)
  , skStep  = \env dt w -> pure (stepWorld env dt w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 1〜3N の数から N 個選んで、順に挿入して木を作る
rebuild :: World -> IO World
rebuild w = do
  ks <- take keyCount <$> shuffle [1 .. keyCount * 3]
  let t = wT w
      (_, root) = foldl' (btInsert t) (1, Node 0 [] []) ks
  pure w { wRoot = root, wKeyPos = M.empty, wBoxPos = M.empty, wPath = [], wPi = 0
         , wResult = "", wTarget = Nothing, wRb = wRb w && t == 2 }

-- ---------- B木への挿入（満杯の節点は下りる前に分割）----------
-- uid は次に使う節点の番号。(uid, 節点) を受け取って更新したものを返す
btInsert :: Int -> (Int, Node) -> Int -> (Int, Node)
btInsert t (uid, r) k
  | length (nKeys r) == 2 * t - 1 =
      let (uid1, s) = splitChild t (uid + 1) (Node uid [] [r]) 0
      in insertNonfull t uid1 s k
  | otherwise = insertNonfull t uid r k

-- x の i 番目の子を 2 つに割り、真ん中のキーを x へ上げる
splitChild :: Int -> Int -> Node -> Int -> (Int, Node)
splitChild t uid x i = case splitAt i (nKids x) of
  (before, y : after) ->
    let (keep, rest) = splitAt (t - 1) (nKeys y)
        mid = take 1 rest
        z = Node uid (drop 1 rest) (drop t (nKids y))   -- 後ろ半分 t−1 個
        y' = y { nKeys = keep, nKids = take t (nKids y) }
        (kl, kr) = splitAt i (nKeys x)
    in (uid + 1, x { nKeys = kl ++ mid ++ kr, nKids = before ++ y' : z : after })
  _ -> (uid, x)

insertNonfull :: Int -> Int -> Node -> Int -> (Int, Node)
insertNonfull t uid x k
  | null (nKids x) = (uid, x { nKeys = insertAt i k (nKeys x) })
  | otherwise =
      let full = maybe False (\c -> length (nKeys c) == 2 * t - 1) (safeAt (nKids x) i)
          (uid1, x1) = if full then splitChild t uid x i else (uid, x)
          i1 = if full && maybe False (k >) (safeAt (nKeys x1) i) then i + 1 else i
      in case splitAt i1 (nKids x1) of
           (before, c : after) ->
             let (uid2, c') = insertNonfull t uid1 c k
             in (uid2, x1 { nKids = before ++ c' : after })
           _ -> (uid1, x1)
  where i = length (takeWhile (k >) (nKeys x))

treeH :: Node -> Int
treeH x = case nKids x of
  (c : _) -> 1 + treeH c
  [] -> 0

countKeys :: Node -> Int
countKeys x = length (nKeys x) + sum (map countKeys (nKids x))

-- ---------- 探索を 1 コマずつ記録 ----------
searchPath :: Node -> Int -> [Step]
searchPath x k =
  let smaller = takeWhile (k >) (nKeys x)
      i = length smaller
      passed = [Step (nId x) (Just c) ">" | c <- smaller]
  in passed ++ case safeAt (nKeys x) i of
       Just c | c == k -> [Step (nId x) (Just c) "="]
       other ->
         [Step (nId x) (Just c) "<" | Just c <- [other]] ++
         case safeAt (nKids x) i of
           Nothing -> [Step (nId x) Nothing "none"]
           Just c -> searchPath c k

startSearch :: Int -> World -> World
startSearch k w = w { wTarget = Just k, wPath = searchPath (wRoot w) k, wResult = "", wPi = 0, wTimer = 0 }

-- ---------- 配置 ----------
data Box = Box { bId :: !Int, bX, bY, bW, bH :: !Float, bN :: !Int }

data Layout = Layout
  { lBoxes :: [Box]
  , lEdges :: [(Point, Point)]
  , lKeys  :: M.Map Int Point
  , lKw    :: !Float
  }

keyW, gapW :: Float
keyW = 44
gapW = 18

-- 部分木の幅
wid :: Node -> Float
wid n =
  let own = fromIntegral (length (nKeys n)) * keyW + gapW
  in if null (nKids n) then own else max own (sum (map wid (nKids n)))

-- 節点に場所をつけた木（pC は拡大前の中心 x）
data Placed = Placed { pNode :: Node, pC :: !Float, pD :: !Int, pKids :: [Placed] }

place :: Node -> Float -> Int -> Placed
place n left d
  | null (nKids n) = Placed n (left + w / 2) d []
  | otherwise =
      let total = sum (map wid (nKids n))
          (_, kids) = mapAccumL (\l c -> (l + wid c, place c l (d + 1))) (left + (w - total) / 2) (nKids n)
      in Placed n (midOf (left + w / 2) (map pC kids)) d kids
  where w = wid n

layoutBT :: Env -> Node -> Layout
layoutBT env root = Layout boxes edges (M.fromList keys) (keyW * s)
  where
    bigW = wid root
    s = min 1.2 ((envW env - 60) / bigW)
    lh = min 120 ((envH env - 260) / fromIntegral (max 1 (treeH root)))
    sx c = envW env / 2 + (c - bigW / 2) * s
    sy d = 150 + fromIntegral d * lh
    top = place root 0 0
    every p = p : concatMap every (pKids p)
    nodes = every top
    boxes = [ Box (nId (pNode p)) (sx (pC p)) (sy (pD p)) (boxW p) (36 * min 1 (s + 0.3)) (length (nKeys (pNode p))) | p <- nodes ]
    boxW p = fromIntegral (length (nKeys (pNode p))) * keyW * s
    leftOf p = sx (pC p) - boxW p / 2
    keys = [ (k, (leftOf p + (fromIntegral j + 0.5) * keyW * s, sy (pD p)))
           | p <- nodes, (j, k) <- zip [0 :: Int ..] (nKeys (pNode p)) ]
    edges = [ ((leftOf p + fromIntegral j * keyW * s, sy (pD p) + 18), (sx (pC c), sy (pD c) - 18))
            | p <- nodes, (j, c) <- zip [0 :: Int ..] (pKids p) ]

-- 2-3-4 木（t = 2）→ 赤黒木：真ん中のキーが黒、両わきのキーが赤い子になる
data RB = RB { rbKey :: !Int, rbRed :: !Bool, rbL, rbR :: !(Maybe RB) }

conv :: Node -> Maybe RB
conv n =
  let kid i = safeAt (nKids n) i >>= conv
  in case nKeys n of
       [a] -> Just (RB a False (kid 0) (kid 1))
       [a, b] -> Just (RB b False (Just (RB a True (kid 0) (kid 1))) (kid 2))
       [a, b, c] -> Just (RB b False (Just (RB a True (kid 0) (kid 1))) (Just (RB c True (kid 2) (kid 3))))
       _ -> Nothing

data RBLayout = RBLayout
  { rlKeys  :: M.Map Int (Point, Bool)
  , rlEdges :: [(Int, Int, Bool)]
  , rlBlack :: !Int
  }

layoutRB :: Env -> Node -> RBLayout
layoutRB env root = RBLayout (M.fromList placed) edges (blackHeight tree)
  where
    tree = conv root
    walk _ Nothing = []
    walk d (Just n) = walk (d + 1) (rbL n) ++ [(n, d :: Int)] ++ walk (d + 1) (rbR n)
    order = walk 0 tree
    edgesOf Nothing = []
    edgesOf (Just n) = [ (rbKey n, rbKey c, rbRed c) | Just c <- [rbL n, rbR n] ] ++ edgesOf (rbL n) ++ edgesOf (rbR n)
    edges = edgesOf tree
    maxD = foldl' max 0 (map snd order)
    dx = (envW env - 80) / fromIntegral (max 1 (length order))
    lh = min 80 ((envH env - 260) / fromIntegral (max 1 maxD))
    placed = [ (rbKey n, ((40 + (fromIntegral i + 0.5) * dx, 150 + fromIntegral d * lh), rbRed n))
             | (i, (n, d)) <- zip [0 :: Int ..] order ]

blackHeight :: Maybe RB -> Int
blackHeight Nothing = 1
blackHeight (Just n) = blackHeight (rbL n) + (if rbRed n then 0 else 1)

-- ---------- 時間を進める ----------
stepWorld :: Env -> Float -> World -> World
stepWorld env dt w0 =
  let n = length (wPath w0)
      animating = n > 0 && wPi w0 < n - 1
      t1 = if animating then wTimer w0 + dt else wTimer w0
      go = animating && t1 > speed
      pi1 = if go then wPi w0 + 1 else wPi w0
      res = if n > 0 && pi1 == n - 1 && null (wResult w0)
              then (case safeAt (wPath w0) pi1 of
                      Just s | stCmp s == "=" -> "Found!"
                      _ -> "Not found (reached a leaf)")
              else wResult w0
      w1 = w0 { wTimer = if go then 0 else t1, wPi = pi1, wResult = res }
      lay = layoutBT env (wRoot w1)
      kt = if wRb w1 then M.map fst (rlKeys (layoutRB env (wRoot w1))) else lKeys lay
      movedKeys = M.mapWithKey (\k t ->
                    let old = M.findWithDefault (fst t, snd t - 40) k (wKeyPos w1)
                    in lerpP 0.12 old t) kt
      movedBoxes = M.fromList
        [ (bId b, (lerpF ox (bX b) 0.12, lerpF oy (bY b) 0.12, lerpF ow (bW b) 0.12))
        | b <- lBoxes lay
        , let (ox, oy, ow) = M.findWithDefault (bX b, bY b, bW b) (bId b) (wBoxPos w1) ]
  in w1 { wKeyPos = movedKeys, wBoxPos = movedBoxes }

-- ---------- 描画 ----------
drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (treePics ++ keyPics ++ panel)
  where
    cur = safeAt (wPath w) (wPi w)
    visited = S.fromList (map stId (take (wPi w + 1) (wPath w)))
    lay = layoutBT env (wRoot w)
    rb = layoutRB env (wRoot w)
    posOf k = M.findWithDefault (0, 0) k (wKeyPos w)
    kw = lKw lay
    t = wT w
    treePics
      | wRb w = [ lineP env 3 (if red then hsb 0 75 90 else hsb 0 0 55) (posOf a) (posOf b) | (a, b, red) <- rlEdges rb ]
      | otherwise =
          [ lineP env 2 (hsb 210 25 45) a b | (a, b) <- lEdges lay ] ++ concatMap boxPic (lBoxes lay)
    boxPic b =
      let (x, y, bw) = M.findWithDefault (bX b, bY b, bW b) (bId b) (wBoxPos w)
          on = maybe False ((== bId b) . stId) cur
          was = S.member (bId b) visited
          h = bH b
          left = x - bw / 2
      in [ fillBox env (if was then hsb 220 35 26 else hsb 222 30 18) left (y - h / 2) bw h
         , frameBox env (if on then hsb 45 90 100 else if was then hsb 45 50 70 else hsb 210 30 55) (if on then 3 else 1.5) left (y - h / 2) bw h ]
         ++ [ lineP env 1 (hsb 210 25 35) (left + fromIntegral j * kw, y - h / 2 + 4) (left + fromIntegral j * kw, y + h / 2 - 4) | j <- [1 .. bN b - 1] ]
    ts = clampF 14 18 (kw * 0.5)
    keyPics = concat
      [ keyPic k p | (k, p) <- M.toList (wKeyPos w) ]
    keyPic k p =
      let hi = maybe False ((== Just k) . stKey) cur
          isEq = maybe False ((== "=") . stCmp) cur
          bg | wRb w =
                 let red = maybe False snd (M.lookup k (rlKeys rb))
                 in [ discP env (if red then hsb 0 75 85 else hsb 0 0 18) p 15
                    , ringP env (if hi then hsb 45 90 100 else hsb 0 0 70) (if hi then 3 else 1) p 15 ]
             | hi = [ fillBox env (if isEq then hsb 140 70 80 else hsba 45 80 90 0.7) (fst p - (kw - 4) / 2) (snd p - 15) (kw - 4) 30 ]
             | otherwise = []
      in bg ++ [ txtFit env (fst p) (snd p) ts (if wRb w then 28 else kw - 2) (hsb 0 0 100) (show k) ]
    n = countKeys (wRoot w)
    h = treeH (wRoot w)
    bound = log (fromIntegral (n + 1) / 2) / log (fromIntegral t) :: Float
    reads' = S.size visited
    cmpStr = case cur of
      Just (Step _ (Just c) op) -> "Compare: " ++ maybe "" show (wTarget w) ++ " " ++ op ++ " " ++ show c
      _ -> ""
    panel =
      [ txt env 24 18 26 (hsb 0 0 100) (if wRb w then "2-3-4 tree = red-black tree" else "B-tree search")
      , txt env 24 56 15 (hsb 0 0 80)
          ("t = " ++ show t ++ " (node holds " ++ show (t - 1) ++ ".." ++ show (2 * t - 1) ++ " keys" ++ (if t == 2 then ", 2-3-4 tree" else "")
           ++ ")   keys n = " ++ show n ++ "   height h = " ++ show h ++ "   bound log_t((n+1)/2) = " ++ fmtF 2 bound)
      ] ++
      [ txt env 24 80 15 (hsb 0 0 80) ("As a red-black tree: black = B-tree node, red = same node. Black height " ++ show (rlBlack rb) ++ " = height " ++ show h ++ " + 2 (with NIL)") | wRb w ] ++
      [ txt env 24 (if wRb w then 104 else 80) 15 (hsb 45 80 100)
          ("Target " ++ show k ++ "   Disk reads " ++ show reads' ++ "   " ++ cmpStr ++ "   " ++ wResult w) | Just k <- [wTarget w] ] ++
      [ txt env 24 (envH env - 52) 14 (hsb 0 0 60) ("Typing: " ++ (if null (wTyped w) then "-" else wTyped w))
      , txt env 24 (envH env - 30) 14 (hsb 0 0 60) "Digits + Enter = search / Space = random search / Left, Right = t / V = red-black view (t=2) / R = rebuild"
      ]

-- ---------- キー ----------
onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char c) Down _ _ | isDigit c -> pure w { wTyped = lastN 4 (wTyped w ++ [c]) }
  EventKey k Down _ _ | isBackKey k -> pure w { wTyped = take (length (wTyped w) - 1) (wTyped w) }
  EventKey (SpecialKey KeyEnter) Down _ _ | not (null (wTyped w)) ->
    pure (startSearch (digitsToInt (wTyped w)) w) { wTyped = "" }
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    k <- randomRIO (1, keyCount * 3 - 1)
    pure (startSearch k w)
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild w { wT = min 5 (wT w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild w { wT = max 2 (wT w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "vV" -> pure (if wT w == 2 then w { wRb = not (wRb w) } else w)
    | c `elem` "rR" -> rebuild w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. startT を 2 にして V を押そう。2-3-4 木と赤黒木が同じものだと見える
-- 2. keyCount を 200 に増やしても、木の高さはほとんど変わらない。上限の式と比べよう
-- 3. 探索の「Disk reads」は高さ + 1 以下。t を変えて、読み出し回数がどう変わるか調べよう

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

-- 円の輪郭
ringP :: Env -> Color -> Float -> Point -> Float -> Picture
ringP env c w (x, y) r = Color c (at (toG env x y) (ring (max 0.1 r) w))

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

-- 列をランダムに並べ直す
shuffle :: [a] -> IO [a]
shuffle xs = do
  rs <- mapM (const (randomRIO (0, 1 :: Double))) xs
  pure (map snd (sortOn fst (zip rs xs)))

-- 子の中心の真ん中（子がなければ d）
midOf :: Float -> [Float] -> Float
midOf d [] = d
midOf _ xs@(x : _) = (x + last xs) / 2

-- 小数を桁数を指定して文字にする
fmtF :: Int -> Float -> String
fmtF n v = printf ("%." ++ show n ++ "f") v

-- マスの中に入る文字（幅 maxW を超えるときは横に縮める）
txtFit :: Env -> Float -> Float -> Float -> Float -> Color -> String -> Picture
txtFit env x y size maxW c s =
  let (gx, gy) = toG env x y
      w = fromIntegral (length s) * size * 0.9
      k = min 1 (maxW / max 1 w)
  in Translate gx gy (Scale k 1 (labelC 0 0 size c s))
