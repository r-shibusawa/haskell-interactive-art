-- ============================================
-- 03 二項ヒープ ─ 2 進数の足し算でできたヒープ
-- 二項木 B_k は B_{k−1} を 2 本つないだ木（節点 2^k 個）。n 個のヒープは n の 2 進数の 1 の桁の B_k を 1 本ずつ持つ
-- 2 つのヒープの和（UNION）は、2 進数の足し算の「繰り上がり」と同じ動き。同じ次数の木が 2 本 → つないで 1 本上へ
-- スペース / 数字 + Enter で挿入 / X で最小値を取り出す / 節点クリックでキーを減らす / U で別のヒープと合併 / R でやり直し
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Char (isDigit)
import Data.List (foldl', intercalate, mapAccumL, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Numeric (showIntAtBase)

speed :: Float
speed = 0.7           -- ★ 1 コマの秒数

maxKey :: Int
maxKey = 99           -- ★ キーの最大値

-- 二項木。キーはどれも違うので、キーが節点の名前になる
data T = T { tKey :: !Int, tDeg :: !Int, tKids :: ![T] }

data Hi = Hi { hKey :: !(Maybe Int), hOther :: ![T] }

noHi :: Hi
noHi = Hi Nothing []

data Frame = Frame { fRoots :: ![T], fMsg :: !String, fHi :: !Hi }

-- アルゴリズムの状態
data Alg = Alg { aRoots :: ![T], aUsed :: !(S.Set Int), aLastOp :: !String, aFrames :: ![Frame] }

data Op = OpIns !Int | OpExt | OpDec !Int | OpUni

data World = World
  { wAlg     :: !Alg
  , wCur     :: !Frame
  , wNext    :: ![Frame]
  , wTimer   :: !Float
  , wPending :: ![Op]
  , wKeyPos  :: !(M.Map Int Point)
  , wTyped   :: !String
  }

initWorld :: World
initWorld = World (Alg [] S.empty "" []) (Frame [] "" noHi) [] 0 [] M.empty ""

main :: IO ()
main = runSketch (sketch "13-03 Binomial Heap" initWorld)
  { skBack  = hsb 170 40 9
  , skStart = \_ -> resetWorld
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

resetWorld :: IO World
resetWorld = do
  (rs, used) <- foldM addOne ([], S.empty) [1 .. 11 :: Int]
  let a = Alg rs used "" []
  pure initWorld { wAlg = a, wCur = Frame rs "11 = 1011 in binary: trees B3, B1, B0 (3 trees)" noHi }
  where
    addOne (rs, used) _ = do
      mk <- freshKey used
      pure $ case mk of
        Just k -> (quietUnion rs [mk0 k], S.insert k used)
        Nothing -> (rs, used)

-- まだ使っていないキーをランダムに選ぶ
freshKey :: S.Set Int -> IO (Maybe Int)
freshKey used
  | S.size used >= maxKey = pure Nothing
  | otherwise = do
      k <- randomRIO (1, maxKey)
      if S.member k used then freshKey used else pure (Just k)

mk0 :: Int -> T
mk0 k = T k 0 []

treeSize :: T -> Int
treeSize n = 1 + sum (map treeSize (tKids n))

total :: [T] -> Int
total = sum . map treeSize

binary :: Int -> String
binary n = showIntAtBase 2 (\d -> toEnum (d + 48)) n ""

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
snap msg hi = modify (\a -> a { aFrames = Frame (aRoots a) msg hi : aFrames a })

setRoots :: [T] -> M ()
setRoots rs = modify (\a -> a { aRoots = rs })

-- ---------- 二項ヒープの操作 ----------
-- 2 本の B_k をつなぐ：y を z の子にする（z のキーが小さい）
link :: T -> T -> T
link y z = z { tKids = y : tKids z, tDeg = tDeg z + 1 }

-- 同じ次数の木をつなぐ 1 手。つなぐものがなければ次の位置へ
unionStep :: Int -> [T] -> Maybe (Int, [T], Maybe (Int, Int))
unionStep i rs = case drop i rs of
  (x : nx : rest)
    | tDeg x /= tDeg nx || threeInARow -> Just (i + 1, rs, Nothing)
    | otherwise ->
        let merged = if tKey x <= tKey nx then link nx x else link x nx
        in Just (i, take i rs ++ merged : rest, Just (tDeg x, min (tKey x) (tKey nx)))
    where threeInARow = case rest of
            (r : _) -> tDeg r == tDeg x
            [] -> False
  _ -> Nothing

-- 記録なしの合併
quietUnion :: [T] -> [T] -> [T]
quietUnion a b = go 0 (sortOn tDeg (a ++ b))
  where go i rs = case unionStep i rs of
          Just (i', rs', _) -> go i' rs'
          Nothing -> rs

-- 2 つのヒープの合併（コマを記録する）
unionRoots :: [T] -> [T] -> M ()
unionRoots a b = do
  let rs0 = sortOn tDeg (a ++ b)
  setRoots rs0
  snap "Merge the root lists by degree (same degrees may remain)" noHi
  go 0 rs0
  where
    go i rs = case unionStep i rs of
      Nothing -> setRoots rs
      Just (i', rs', Nothing) -> go i' rs'
      Just (i', rs', Just (k, m)) -> do
        setRoots rs'
        snap ("B" ++ show k ++ " + B" ++ show k ++ " = B" ++ show (k + 1) ++ " (carry): smaller key " ++ show m ++ " is the root") noHi { hKey = Just m }
        go i' rs'

insertKey :: Int -> M ()
insertKey k = do
  used <- gets aUsed
  if S.member k used
    then snap (show k ++ " is already there") noHi
    else do
      modify (\a -> a { aUsed = S.insert k used, aLastOp = "insert" })
      snap ("Insert " ++ show k ++ ": make a heap with one B0 and merge (add 1 to n)") noHi { hKey = Just k }
      rs <- gets aRoots
      unionRoots rs [mk0 k]
      rs2 <- gets aRoots
      snap ("Done: n = " ++ show (total rs2) ++ " = " ++ binary (total rs2) ++ " in binary") noHi { hKey = Just k }

extractMin :: M ()
extractMin = do
  rs <- gets aRoots
  case sortOn tKey rs of
    [] -> snap "The heap is empty" noHi
    (m : _) -> do
      modify (\a -> a { aLastOp = "extract-min" })
      snap ("The minimum is one of the roots: " ++ show (tKey m) ++ " (look at roots: O(log n))") noHi { hKey = Just (tKey m) }
      let others = filter ((/= tKey m) . tKey) rs
          kids = reverse (tKids m)
          names = if null kids then "(none)" else intercalate ", " ["B" ++ show (tDeg c) | c <- kids]
      setRoots others
      modify (\a -> a { aUsed = S.delete (tKey m) (aUsed a) })
      snap ("Remove " ++ show (tKey m) ++ ": its children " ++ names ++ " form another heap") noHi
      unionRoots others kids
      rs2 <- gets aRoots
      snap ("The 2 heaps are merged: n = " ++ show (total rs2)) noHi

-- 根から節点 k までのキーの列（根が先頭）
pathTo :: Int -> [T] -> Maybe [Int]
pathTo _ [] = Nothing
pathTo k (r : rs)
  | tKey r == k = Just [k]
  | otherwise = case pathTo k (tKids r) of
      Just p -> Just (tKey r : p)
      Nothing -> pathTo k rs

-- 森の中のキーを取りかえる
mapKeys :: (Int -> Int) -> [T] -> [T]
mapKeys f = map (\n -> n { tKey = f (tKey n), tKids = mapKeys f (tKids n) })

-- BINOMIAL-HEAP-DECREASE-KEY：キーを減らして親と入れかえながら上へ（d はランダムな減らす量）
decreaseKey :: Int -> Int -> M ()
decreaseKey k d = do
  rs <- gets aRoots
  case pathTo k rs of
    Nothing -> pure ()
    Just p -> do
      used <- gets aUsed
      let free n = if S.member n used then free (n - 1) else n
          nk = free (k - d)
      modify (\a -> a { aUsed = S.insert nk (S.delete k (aUsed a)), aLastOp = "decrease-key" })
      setRoots (mapKeys (\x -> if x == k then nk else x) rs)
      snap ("Decrease " ++ show k ++ " to " ++ show nk) noHi { hKey = Just nk }
      bubble nk (reverse (take (length p - 1) p))
      snap "Heap property restored" noHi { hKey = Just nk }
  where
    bubble nk (par : up) | par > nk = do
      rs <- gets aRoots
      setRoots (mapKeys (\x -> if x == nk then par else if x == par then nk else x) rs)
      snap ("Smaller than parent " ++ show par ++ ": swap and move up") noHi { hKey = Just nk }
      bubble nk up
    bubble _ _ = pure ()

unionWith :: [Int] -> M ()
unionWith ks = do
  let other = foldl' (\o k -> quietUnion o [mk0 k]) [] ks
  rs <- gets aRoots
  let a = total rs
      b = total other
  modify (\s -> s { aUsed = foldr S.insert (aUsed s) ks, aLastOp = "union" })
  snap ("Union with a heap of " ++ show b ++ ": " ++ binary a ++ " + " ++ binary b ++ " = " ++ binary (a + b)) noHi { hOther = other }
  unionRoots rs other
  snap ("Union complete: n = " ++ show (a + b)) noHi

-- ---------- 配置 ----------
data Tg = Tg { tgP :: !Point, tgRoot :: !Bool, tgDeg :: !Int, tgOther :: !Bool }

leaves :: T -> Int
leaves n = if null (tKids n) then 1 else sum (map leaves (tKids n))

depth :: T -> Int
depth n = if null (tKids n) then 0 else 1 + foldl' max 0 (map depth (tKids n))

layoutForest :: Env -> Bool -> [T] -> Float -> ([(Int, Tg)], [(Int, Int)], Float)
layoutForest env isOther rs y0 = (concat placed, concatMap edgesOf rs, u)
  where
    units = sum [fromIntegral (leaves r) + 0.8 | r <- rs] :: Float
    u = min 52 ((envW env - 80) / max 1 units)
    deepest = foldl' max 0 (map depth rs)
    lh = min 80 ((envH env - y0 - 120) / fromIntegral (max 1 deepest))
    x0 = envW env / 2 - units * u / 2
    (_, placed) = mapAccumL (\x r -> (x + (fromIntegral (leaves r) + 0.8) * u, snd (place r x 0))) x0 rs
    place n left d
      | null (tKids n) = emit n (left + fromIntegral (leaves n) * u / 2) d []
      | otherwise =
          let (_, res) = mapAccumL (\l c -> (l + fromIntegral (leaves c) * u, place c l (d + 1))) left (tKids n)
          in emit n (midOf left (map fst res)) d (concatMap snd res)
    emit n cx d sub = (cx, sub ++ [(tKey n, Tg (cx, y0 + fromIntegral (d :: Int) * lh) (d == 0) (tDeg n) isOther)])
    edgesOf n = concatMap edgesOf (tKids n) ++ [(tKey n, tKey c) | c <- tKids n]

-- ---------- 時間を進める ----------
stepWorld :: Env -> Float -> World -> IO World
stepWorld env dt w0 = do
  let t1 = wTimer w0 + dt
      w1 = case wNext w0 of
             (f : rest) | t1 > speed -> w0 { wCur = f, wNext = rest, wTimer = 0 }
             _ -> w0 { wTimer = t1 }
  w2 <- case wPending w1 of
          (p : ps) | null (wNext w1) && wTimer w1 > speed * 0.5 -> runOp p w1 { wPending = ps, wTimer = 0 }
          _ -> pure w1
  pure (moveKeys env w2)

runOp :: Op -> World -> IO World
runOp op w = do
  m <- case op of
    OpIns k -> pure (insertKey k)
    OpExt -> pure extractMin
    OpDec k -> do
      d <- randomRIO (20, 59)
      pure (decreaseKey k d)
    OpUni -> do
      n <- randomRIO (3, 7 :: Int)
      (ks, _) <- foldM (\(acc, u) _ -> do
                          mk <- freshKey u
                          pure (maybe (acc, u) (\k -> (acc ++ [k], S.insert k u)) mk))
                       ([], aUsed (wAlg w)) [1 .. n]
      pure (unionWith ks)
  let a = execM m (wAlg w)
  pure w { wAlg = a { aFrames = [] }, wNext = wNext w ++ reverse (aFrames a) }

targets :: Env -> Frame -> M.Map Int Tg
targets env f =
  let (a, _, _) = layoutForest env False (fRoots f) 220
      (b, _, _) = layoutForest env True (hOther (fHi f)) (envH env - 230)
  in M.fromList (a ++ b)

moveKeys :: Env -> World -> World
moveKeys env w =
  let tg = targets env (wCur w)
  in w { wKeyPos = M.mapWithKey (\k t -> let p = tgP t
                                         in lerpP 0.14 (M.findWithDefault (fst p, snd p - 50) k (wKeyPos w)) p) tg }

-- ---------- 描画 ----------
drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (rootLinks ++ edgePics ++ nodePics ++ panel)
  where
    f = wCur w
    hi = fHi f
    (_, edges, u) = layoutForest env False (fRoots f) 220
    (_, otherEdges, _) = layoutForest env True (hOther hi) (envH env - 230)
    tg = targets env f
    posOf k = M.findWithDefault (0, 0) k (wKeyPos w)
    mouse = mouseXY env
    rk = map tKey (fRoots f)
    rootLinks =
      [ dashP env 2 (hsb 170 40 60) 6 6 [(ax + 18, ay), (bx - 18, by)]
      | (a, b) <- zip rk (drop 1 rk), let (ax, ay) = posOf a, let (bx, by) = posOf b ]
    edgePics = [ lineP env 2 (hsb 170 20 50) (posOf a) (posOf b) | (a, b) <- edges ++ otherEdges ]
    d = clampF 18 36 (u * 0.7)
    nodePics = concat
      [ [ discP env (if isHi then hsb 50 90 100 else if tgOther t then hsb 300 40 60 else if tgRoot t then hsb 170 55 65 else hsb 185 35 40) p (d / 2) ]
        ++ [ ringP env (hsb 0 0 100) 2 p (d / 2) | dist2 mouse p < d / 2 ]
        ++ [ txtFit env (fst p) (snd p) (max 14 (d * 0.45)) (d - 2) (if isHi then hsb 0 0 10 else hsb 0 0 100) (show k) ]
        ++ [ txtC env (fst p) (snd p - d / 2 - 12) 14 (hsb 170 30 80) ("B" ++ show (tgDeg t)) | tgRoot t && not (tgOther t) ]
      | (k, t) <- M.toList tg
      , let p = posOf k
            isHi = hKey hi == Just k ]
    n = total (fRoots f)
    bits = binary n
    lastOp = aLastOp (wAlg w)
    bitPics = concat
      [ [ fillBox env (if on then hsb 170 55 65 else hsb 170 15 22) x 82 48 40
        , txtC env (x + 24) 94 18 (hsb 0 0 100) [b]
        , txtC env (x + 24) 113 14 (hsb 0 0 100) ("B" ++ show deg) ]
      | (i, b) <- zip [0 :: Int ..] bits
      , let x = 24 + fromIntegral i * 54
            deg = length bits - 1 - i
            on = b == '1' ]
    panel =
      [ txt env 24 18 26 (hsb 0 0 100) "Binomial heap"
      , txt env 24 56 15 (hsb 0 0 80)
          ("n = " ++ show n ++ "   binary = " ++ bits ++ "   roots = " ++ show (length (fRoots f)) ++ " (= number of 1 bits <= floor(log2 n) + 1)   "
           ++ (if null lastOp then "" else "Op: " ++ lastOp))
      ] ++ bitPics ++
      [ txt env 24 136 18 (hsb 50 80 100) (fMsg f) ] ++
      [ txt env 24 (envH env - 270) 14 (hsb 300 40 85) "The other heap to merge" | not (null (hOther hi)) ] ++
      [ txt env 24 (envH env - 52) 14 (hsb 0 0 60) ("Typing: " ++ (if null (wTyped w) then "-" else wTyped w))
      , txt env 24 (envH env - 30) 14 (hsb 0 0 60)
          "Space, digits + Enter = insert / X = extract-min / click node = decrease key / U = union / R = reset"
      ]

-- ---------- マウスとキー ----------
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseXY env
        inFrame = map tKey (everyNode (fRoots (wCur w)))
        near = [k | (k, p) <- M.toList (wKeyPos w), dist2 m p < 18, k `elem` inFrame]
    in pure $ case near of
         (k : _) -> push (OpDec k)
         [] -> w
  EventKey (Char c) Down _ _ | isDigit c -> pure w { wTyped = lastN 3 (wTyped w ++ [c]) }
  EventKey k Down _ _ | isBackKey k -> pure w { wTyped = take (length (wTyped w) - 1) (wTyped w) }
  EventKey (SpecialKey KeyEnter) Down _ _ | not (null (wTyped w)) ->
    pure (push (OpIns (digitsToInt (wTyped w)))) { wTyped = "" }
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    mk <- freshKey (aUsed (wAlg w))
    pure (maybe w (push . OpIns) mk)
  EventKey (Char c) Down _ _
    | c `elem` "xX" -> pure (push OpExt)
    | c `elem` "uU" -> pure (push OpUni)
    | c `elem` "rR" -> resetWorld
  _ -> pure w
  where push op = w { wPending = wPending w ++ [op] }

everyNode :: [T] -> [T]
everyNode = concatMap (\n -> n : everyNode (tKids n))

-- ---------- やってみよう ----------
-- 1. n = 15 のあとに 1 個入れると、繰り上がりは何回起きる？ 2 進数の足し算と比べよう
-- 2. U で合併するとき、上の 2 進数の足し算を自分で先に計算してみよう
-- 3. X で取り出したとき、なぜ子が B0, B1, B2 ... の順に並ぶの？ reverse を消して試そう

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

-- 点線
dashP :: Env -> Float -> Color -> Float -> Float -> [Point] -> Picture
dashP env w c on off ps = Pictures (go on True ps)
  where
    go _ _ [] = []
    go _ _ [_] = []
    go left drawing (a : b : rest) =
      let l = dist2 a b
      in if l <= left
           then [lineP env w c a b | drawing] ++ go (left - l) drawing (b : rest)
           else let t = left / max 1e-6 l
                    m = (lerpF (fst a) (fst b) t, lerpF (snd a) (snd b) t)
                in [lineP env w c a m | drawing] ++ go (if drawing then off else on) (not drawing) (m : b : rest)

-- 塗りつぶした長方形（x, y は左上）
fillBox :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
fillBox env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

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
