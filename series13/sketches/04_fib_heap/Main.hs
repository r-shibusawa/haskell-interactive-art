-- ============================================
-- 04 フィボナッチヒープ ─ 「あとで片づける」ヒープ
-- INSERT と UNION は根のリストに置くだけ（O(1)）。片づけは EXTRACT-MIN の CONSOLIDATE でまとめて行う
-- ポテンシャル Φ = 根の数 t + 2 × 印の数 m。散らかすと Φ が貯まり、片づけで Φ を使う ─ 償却解析（シリーズ12）
-- スペース / 数字 + Enter で挿入 / X で最小値を取り出す / U で別のヒープと合併 / P で一時停止・N で 1 コマ / R でやり直し
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Char (isDigit)
import Data.List (find, foldl', mapAccumL)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Printf (printf)

speed :: Float
speed = 0.6           -- ★ 1 コマの秒数

maxKey :: Int
maxKey = 99           -- ★ キーの最大値

start :: Int
start = 14            -- ★ 最初に入れるキーの数

data T = T { tKey :: !Int, tDeg :: !Int, tKids :: ![T], tMark :: !Bool }

-- コマの強調（キー、次数の箱 A、もう 1 つのヒープ）
data Hi = Hi { hKey :: !(Maybe Int), hA :: !(Maybe [Maybe Int]), hOther :: ![T] }

noHi :: Hi
noHi = Hi Nothing Nothing []

-- 操作の記録：名前、実コスト、ΔΦ
type LogRow = (String, Int, Int)

data Frame = Frame
  { fRoots :: ![T], fMin :: !(Maybe Int), fMsg :: !String, fHi :: !Hi
  , fN, fPhi, fT, fM :: !Int, fHist :: ![LogRow] }

data Alg = Alg
  { aRoots :: ![T], aMin :: !(Maybe Int), aUsed :: !(S.Set Int)
  , aLog :: ![LogRow]     -- 新しい順、6 個まで
  , aWork, aPhi0 :: !Int
  , aFrames :: ![Frame] }

data Op = OpIns !Int | OpExt | OpUni

data World = World
  { wAlg     :: !Alg
  , wCur     :: !Frame
  , wNext    :: ![Frame]
  , wTimer   :: !Float
  , wPending :: ![Op]
  , wPaused  :: !Bool
  , wKeyPos  :: !(M.Map Int Point)
  , wTyped   :: !String
  }

emptyAlg :: Alg
emptyAlg = Alg [] Nothing S.empty [] 0 0 []

initWorld :: World
initWorld = World emptyAlg (Frame [] Nothing "" noHi 0 0 0 0 []) [] 0 [] False M.empty ""

main :: IO ()
main = runSketch (sketch "13-04 Fibonacci Heap" initWorld)
  { skBack  = hsb 25 35 9
  , skStart = \_ -> resetWorld
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

resetWorld :: IO World
resetWorld = do
  ks <- freshKeys start S.empty
  let insertAll = do
        mapM_ (\k -> pushRoots [mk0 k]) ks
        snap (show start ++ " INSERTs just lined them up in the root list: no trees yet") noHi
      a = execM insertAll emptyAlg { aUsed = S.fromList ks }
      f = case aFrames a of
            (x : _) -> x
            [] -> wCur initWorld
  pure initWorld { wAlg = a { aFrames = [] }, wCur = f }

-- 使っていないキーを n 個選ぶ（全部使っていたらそこまで）
freshKeys :: Int -> S.Set Int -> IO [Int]
freshKeys n used
  | n <= 0 || S.size used >= maxKey = pure []
  | otherwise = do
      k <- randomRIO (1, maxKey)
      if S.member k used
        then freshKeys n used
        else (k :) <$> freshKeys (n - 1) (S.insert k used)

mk0 :: Int -> T
mk0 k = T k 0 [] False

treeSize, marks :: T -> Int
treeSize n = 1 + sum (map treeSize (tKids n))
marks n = (if tMark n then 1 else 0) + sum (map marks (tKids n))

phiOf :: [T] -> Int
phiOf rs = length rs + 2 * sum (map marks rs)

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
snap msg hi = modify $ \a ->
  let rs = aRoots a
      fr = Frame rs (aMin a) msg hi (sum (map treeSize rs)) (phiOf rs) (length rs) (sum (map marks rs)) (reverse (aLog a))
  in a { aFrames = fr : aFrames a }

begin :: M ()
begin = modify (\a -> a { aWork = 0, aPhi0 = phiOf (aRoots a) })

finish :: String -> M ()
finish name = modify (\a -> a { aLog = take 6 ((name, aWork a, phiOf (aRoots a) - aPhi0 a) : aLog a) })

work :: Int -> M ()
work n = modify (\a -> a { aWork = aWork a + n })

updMin :: Int -> M ()
updMin k = modify (\a -> a { aMin = Just (maybe k (min k) (aMin a)) })

-- 根のリストの端に木を足す
pushRoots :: [T] -> M ()
pushRoots ts = do
  modify (\a -> a { aRoots = aRoots a ++ ts })
  mapM_ (updMin . tKey) ts

-- ---------- 操作 ----------
insertKey :: Int -> M ()
insertKey k = do
  used <- gets aUsed
  if S.member k used
    then snap (show k ++ " is already there") noHi
    else do
      modify (\a -> a { aUsed = S.insert k used })
      begin
      pushRoots [mk0 k]
      work 1
      finish ("INSERT " ++ show k)
      snap ("INSERT " ++ show k ++ ": just put it at the end of the root list. Cost 1, Phi +1") noHi { hKey = Just k }

-- ks はもう 1 つのヒープのキー
unionWith :: [Int] -> M ()
unionWith ks = do
  begin
  modify (\a -> a { aUsed = foldr S.insert (aUsed a) ks })
  -- もう 1 つのヒープにも小さな木を作っておく
  let other = case ks of
        (x : y : rest) ->
          let (a, b) = if x > y then (y, x) else (x, y)
          in T a 1 [mk0 b] False : map mk0 rest
        _ -> map mk0 ks
  snap "Merge with the other Fibonacci heap (below)" noHi { hOther = other }
  pushRoots other
  work 1
  finish "UNION"
  snap "UNION: just join the 2 root lists (actual cost O(1))" noHi

extractMin :: M ()
extractMin = do
  rs0 <- gets aRoots
  mn <- gets aMin
  case find ((== mn) . Just . tKey) rs0 of
    Nothing -> snap "The heap is empty" noHi
    Just z -> do
      let zk = tKey z
      begin
      snap ("EXTRACT-MIN: the min pointer points at " ++ show zk) noHi { hKey = Just zk }
      modify (\a -> a { aRoots = aRoots a ++ map (\c -> c { tMark = False }) (tKids z) })
      work (length (tKids z))
      snap ("Move the " ++ show (length (tKids z)) ++ " children of " ++ show zk ++ " to the root list") noHi { hKey = Just zk }
      modify (\a -> a { aRoots = removeFirst zk (aRoots a), aUsed = S.delete zk (aUsed a) })
      rs <- gets aRoots
      if null rs
        then do
          modify (\a -> a { aMin = Nothing })
          finish ("EXTRACT " ++ show zk)
          snap "Now empty" noHi
        else do
          -- CONSOLIDATE
          let n = sum (map treeSize rs)
              dMax = floor (log (fromIntegral n :: Float) / log ((1 + sqrt 5) / 2)) + 1 :: Int
              a0 = replicate (dMax + 1) Nothing
          snap ("CONSOLIDATE: boxes A[0.." ++ show dMax ++ "] by degree. Same degree? Link the trees") noHi { hA = Just a0 }
          _ <- foldM consolidateOne a0 (map tKey rs)
          modify (\a -> a { aMin = Nothing })
          rs2 <- gets aRoots
          mapM_ (updMin . tKey) rs2
          finish ("EXTRACT " ++ show zk)
          mn2 <- gets aMin
          snap ("Clean: " ++ show (length rs2) ++ " roots (all different degrees). New min = " ++ maybe "-" show mn2) noHi { hKey = mn2 }

-- 根を 1 つ見て、同じ次数の木があるあいだつなぐ
consolidateOne :: [Maybe Int] -> Int -> M [Maybe Int]
consolidateOne a0 w = do
  rs <- gets aRoots
  let d0 = maybe 0 tDeg (find ((== w) . tKey) rs)
  work 1
  snap ("Look at root " ++ show w ++ " (degree " ++ show d0 ++ ")") noHi { hKey = Just w, hA = Just a0 }
  go w d0 a0
  where
    go x d a = case safeAt a d of
      Just (Just other) -> do
        let (big, small) = if x > other then (x, other) else (other, x)
        linkRoots small big
        let a1 = setAt d Nothing a
            a2 = if d + 1 >= length a1 then a1 ++ [Nothing] else a1
        work 1
        snap ("A[" ++ show d ++ "] also has degree " ++ show d ++ ": make " ++ show big ++ " a child of " ++ show small ++ " (degree " ++ show (d + 1) ++ ")") noHi { hKey = Just small, hA = Just a2 }
        go small (d + 1) a2
      _ -> do
        let a3 = setAt d (Just x) a
        snap ("Put " ++ show x ++ " in A[" ++ show d ++ "]") noHi { hKey = Just x, hA = Just a3 }
        pure a3

-- 同じ次数の根を 2 つつなぐ（キーの大きい方を子に）
linkRoots :: Int -> Int -> M ()
linkRoots small big = modify $ \a ->
  case find ((== big) . tKey) (aRoots a) of
    Nothing -> a
    Just y ->
      let y' = y { tMark = False }
          add r | tKey r == small = r { tKids = tKids r ++ [y'], tDeg = tDeg r + 1 }
                | otherwise = r
      in a { aRoots = map add (removeFirst big (aRoots a)) }

-- 先頭に近い、キー k の根を除く
removeFirst :: Int -> [T] -> [T]
removeFirst _ [] = []
removeFirst k (r : rs) = if tKey r == k then rs else r : removeFirst k rs

-- ---------- 配置 ----------
data Tg = Tg { tgP :: !Point, tgRoot, tgMark, tgOther :: !Bool }

leaves :: T -> Int
leaves n = if null (tKids n) then 1 else sum (map leaves (tKids n))

depth :: T -> Int
depth n = if null (tKids n) then 0 else 1 + foldl' max 0 (map depth (tKids n))

layoutForest :: Env -> Bool -> [T] -> Float -> Float -> Float -> ([(Int, Tg)], [(Int, Int)], Float)
layoutForest env isOther rs y0 x0 x1 = (concat placed, concatMap edgesOf rs, u)
  where
    units = sum [fromIntegral (leaves r) + 0.6 | r <- rs] :: Float
    u = min 50 ((x1 - x0) / max 1 units)
    lh = min 78 ((envH env - y0 - 150) / fromIntegral (foldl' max 1 (map depth rs)))
    start0 = (x0 + x1) / 2 - units * u / 2
    (_, placed) = mapAccumL (\x r -> (x + (fromIntegral (leaves r) + 0.6) * u, snd (place r x 0))) start0 rs
    place n left d
      | null (tKids n) = emit n (left + u / 2) d []
      | otherwise =
          let (_, res) = mapAccumL (\l c -> (l + fromIntegral (leaves c) * u, place c l (d + 1))) left (tKids n)
          in emit n (midOf left (map fst res)) d (concatMap snd res)
    emit n cx d sub = (cx, sub ++ [(tKey n, Tg (cx, y0 + fromIntegral (d :: Int) * lh) (d == 0) (tMark n) isOther)])
    edgesOf n = [(tKey n, tKey c) | c <- tKids n] ++ concatMap edgesOf (tKids n)

targets :: Env -> Frame -> M.Map Int Tg
targets env f =
  let (a, _, _) = layoutForest env False (fRoots f) 290 30 (envW env - 300)
      (b, _, _) = layoutForest env True (hOther (fHi f)) (envH env - 200) 30 (envW env - 300)
  in M.fromList (a ++ b)

-- ---------- 時間を進める ----------
stepWorld :: Env -> Float -> World -> IO World
stepWorld env dt w0 = do
  let t1 = if wPaused w0 then wTimer w0 else wTimer w0 + dt
      w1 = case wNext w0 of
             (f : rest) | t1 > speed -> w0 { wCur = f, wNext = rest, wTimer = 0 }
             _ -> w0 { wTimer = t1 }
  w2 <- case wPending w1 of
          (p : ps) | null (wNext w1) && wTimer w1 > speed * 0.5 -> runOp p w1 { wPending = ps, wTimer = 0 }
          _ -> pure w1
  let tg = targets env (wCur w2)
  pure w2 { wKeyPos = M.mapWithKey (\k t -> let p = tgP t
                                            in lerpP 0.14 (M.findWithDefault (fst p, snd p - 50) k (wKeyPos w2)) p) tg }

runOp :: Op -> World -> IO World
runOp op w = do
  m <- case op of
    OpIns k -> pure (insertKey k)
    OpExt -> pure extractMin
    OpUni -> do
      n <- randomRIO (2, 4)
      ks <- freshKeys n (aUsed (wAlg w))
      pure (unionWith ks)
  let a = execM m (wAlg w)
  pure w { wAlg = a { aFrames = [] }, wNext = wNext w ++ reverse (aFrames a) }

-- ---------- 描画 ----------
drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (rootLinks ++ ringBack ++ edgePics ++ nodePics ++ boxesA ++ panel)
  where
    f = wCur w
    hi = fHi f
    (_, edges, u) = layoutForest env False (fRoots f) 290 30 (envW env - 300)
    (_, otherEdges, _) = layoutForest env True (hOther hi) (envH env - 200) 30 (envW env - 300)
    tg = targets env f
    posOf k = M.findWithDefault (0, 0) k (wKeyPos w)
    d = clampF 18 36 (u * 0.72)
    rk = map tKey (fRoots f)
    -- 根のリスト（双方向の環状リスト）
    rootLinks =
      [ lineP env 2 (hsb 25 50 70) (ax + d / 2, ay) (bx - d / 2, by)
      | (a, b) <- zip rk (drop 1 rk), let (ax, ay) = posOf a, let (bx, by) = posOf b ]
    ringBack = case rk of
      (first : _ : _) ->
        let (ax, ay) = posOf (last rk)
            (bx, by) = posOf first
        in [ dashP env 2 (hsb 25 50 70) 5 6 (bezierPts (ax + d / 2, ay) (ax + 40, ay - 45) (bx - 40, by - 45) (bx - d / 2, by)) ]
      _ -> []
    edgePics = [ lineP env 2 (hsb 25 20 50) (posOf a) (posOf b) | (a, b) <- edges ++ otherEdges ]
    nodePics = concat
      [ [ discP env (if isHi then hsb 50 90 100 else if tgOther t then hsb 300 40 60 else if tgRoot t then hsb 25 75 85 else hsb 25 40 45) p (d / 2) ]
        ++ [ discP env (hsb 0 0 5) (x + d * 0.38, y - d * 0.38) (d * 0.175) | tgMark t ]
        ++ [ txtFit env x y (max 14 (d * 0.45)) (d - 2) (if isHi then hsb 0 0 10 else hsb 0 0 100) (show k) ]
        ++ (if isMin then [ txtC env x (y - d / 2 - 26) 14 (hsb 50 90 100) "min"
                          , triP env (hsb 50 90 100) (x - 5, y - d / 2 - 16) (x + 5, y - d / 2 - 16) (x, y - d / 2 - 6) ] else [])
      | (k, t) <- M.toList tg
      , let p@(x, y) = posOf k
            isHi = hKey hi == Just k
            isMin = fMin f == Just k && not (tgOther t) ]
    -- 次数の箱 A
    boxesA = case hA hi of
      Nothing -> []
      Just a -> concat
        [ [ fillBox env (if v /= Nothing then hsb 25 60 40 else hsb 25 20 15) x 150 56 44
          , frameBox env (hsb 25 40 60) 1.5 x 150 56 44
          , txtC env (x + 28) 172 16 (hsb 0 0 100) (maybe "." show v)
          , txtC env (x + 28) 139 14 (hsb 25 30 75) ("A" ++ show i) ]
        | (i, v) <- zip [0 :: Int ..] a, let x = 40 + fromIntegral i * 64 ]
    px = envW env - 280
    histPics = concat
      [ [ txt env px (190 + fromIntegral j * 44) 14 (hsb 0 0 80) nm
        , txt env px (208 + fromIntegral j * 44) 14 (hsb 25 60 95)
            ("real " ++ show wk ++ "  dPhi " ++ (if dp >= 0 then "+" else "") ++ show dp ++ "  = " ++ show (wk + dp)) ]
      | (j, (nm, wk, dp)) <- zip [0 :: Int ..] (fHist f) ]
    bound = if fN f > 0 then log (fromIntegral (fN f)) / log 1.618 else 0 :: Float
    panel =
      [ txt env 24 18 26 (hsb 0 0 100) "Fibonacci heap"
      , txt env 24 56 15 (hsb 0 0 80)
          ("n = " ++ show (fN f) ++ "   roots t = " ++ show (fT f) ++ "   marks m = " ++ show (fM f) ++ "   potential Phi = t + 2m = " ++ show (fPhi f) ++ "   D(n) <= log_phi n = " ++ fmtF 1 bound)
      , txt env 24 90 16 (hsb 50 80 100) (fMsg f)
      , txt env px 160 14 (hsb 0 0 100) "Amortized = real + dPhi"
      ] ++ histPics ++
      [ txt env 24 (envH env - 250) 14 (hsb 300 40 85) "The other heap to merge" | not (null (hOther hi)) ] ++
      [ txt env 24 (envH env - 52) 14 (hsb 0 0 60)
          ("Typing: " ++ (if null (wTyped w) then "-" else wTyped w) ++ "   Black dot = mark   " ++ (if wPaused w then "Paused" else ""))
      , txt env 24 (envH env - 30) 14 (hsb 0 0 60)
          "Space, digits + Enter = insert / X = extract-min / U = union / P = pause / N = next frame / R = reset"
      ]

-- ---------- キー ----------
onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char c) Down _ _ | isDigit c -> pure w { wTyped = lastN 3 (wTyped w ++ [c]) }
  EventKey k Down _ _ | isBackKey k -> pure w { wTyped = take (length (wTyped w) - 1) (wTyped w) }
  EventKey (SpecialKey KeyEnter) Down _ _ | not (null (wTyped w)) ->
    pure (push (OpIns (digitsToInt (wTyped w)))) { wTyped = "" }
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    ks <- freshKeys 1 (aUsed (wAlg w))
    pure $ case ks of
      (k : _) -> push (OpIns k)
      [] -> w
  EventKey (Char c) Down _ _
    | c `elem` "xX" -> pure (push OpExt)
    | c `elem` "uU" -> pure (push OpUni)
    | c `elem` "pP" -> pure w { wPaused = not (wPaused w) }
    | c `elem` "nN" -> pure $ case wNext w of
        (f : rest) -> w { wCur = f, wNext = rest }
        [] -> w
    | c `elem` "rR" -> resetWorld
  _ -> pure w
  where push op = w { wPending = wPending w ++ [op] }

-- ---------- やってみよう ----------
-- 1. X を押す前と後で Phi はどう変わる？ 実コストが大きいのに償却コストが小さいのはなぜ？
-- 2. start を 40 にして X。A の箱がいくつ必要になるか、log_phi n と比べよう
-- 3. スペースを何回も押してから X を押そう。「あとで片づける」の意味が見える

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

-- ベジェ曲線の上の点を並べる
bezierPts :: Point -> Point -> Point -> Point -> [Point]
bezierPts (ax, ay) (bx, by) (cx, cy) (dx, dy) =
  [ (f ax bx cx dx t, f ay by cy dy t) | i <- [0 .. 16 :: Int], let t = fromIntegral i / 16 ]
  where f a b c d t = let u = 1 - t in u * u * u * a + 3 * u * u * t * b + 3 * u * t * t * c + t * t * t * d

-- 塗りつぶした長方形（x, y は左上）
fillBox :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
fillBox env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 長方形の枠（x, y は左上、sw は線の太さ）
frameBox :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
frameBox env c sw x y w h = pathP env sw c [(x - sw / 2, y), (x + w + sw / 2, y), (x + w, y), (x + w, y + h), (x + w + sw / 2, y + h), (x - sw / 2, y + h), (x, y + h), (x, y)]

-- 塗りつぶした円（p は中心、r は半径）
discP :: Env -> Color -> Point -> Float -> Picture
discP env c (x, y) r = Color c (at (toG env x y) (disc (max 0.1 r)))

-- 三角形
triP :: Env -> Color -> Point -> Point -> Point -> Picture
triP env c (a, b) (d, e) (f, g) = Color c (Polygon [toG env a b, toG env d e, toG env f g])

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

-- 列の i 番目を v に置きかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = let (a, b) = splitAt i xs in a ++ v : drop 1 b

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
