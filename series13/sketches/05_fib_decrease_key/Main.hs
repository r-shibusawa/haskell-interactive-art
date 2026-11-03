-- ============================================
-- 05 キーを減らす ─ 切り取りと「連鎖的な切り取り」
-- 親より小さくなった節点は、部分木ごと切り取って根のリストへ（CUT）。親には「1 人子を失った」印をつける
-- 印のある親がもう 1 人失ったら、その親も切り取る（CASCADING-CUT）。この規則が木を太く保つ
-- 節点クリック = キーを減らす / D = 削除モード切りかえ / スペース = ランダムに減らす / X = 最小値を取り出す / R = 新しいヒープ
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.List (find, foldl', mapAccumL)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S

speed :: Float
speed = 0.75          -- ★ 1 コマの秒数

start :: Int
start = 30            -- ★ 最初に入れるキーの数

neg :: Int
neg = -999            -- −∞ の代わり

-- 節点は番号で管理する（親へのポインタをそのまま書ける）
data N = N { nKey :: !Int, nDeg :: !Int, nKids :: ![Int], nPar :: !(Maybe Int), nMark :: !Bool }
type Nodes = IM.IntMap N

data Hi = Hi { hKey :: !(Maybe Int), hPar :: !(Maybe Int), hFrom :: !(Maybe Int) }

noHi :: Hi
noHi = Hi Nothing Nothing Nothing

data Frame = Frame
  { fNodes :: !Nodes, fRoots :: ![Int], fMin :: !(Maybe Int), fMsg :: !String, fHi :: !Hi
  , fPhi, fT, fM :: !Int, fCost :: !String }

data Alg = Alg
  { aNodes :: !Nodes, aRoots :: ![Int], aMin :: !(Maybe Int), aUsed :: !(S.Set Int)
  , aUid :: !Int, aCuts :: !Int, aCost :: !String, aFrames :: ![Frame] }

data Op = OpClick !Int | OpExt | OpRand

data World = World
  { wAlg     :: !Alg
  , wCur     :: !Frame
  , wNext    :: ![Frame]
  , wTimer   :: !Float
  , wPending :: ![Op]
  , wKeyPos  :: !(M.Map Int Point)
  , wDelMode :: !Bool
  }

emptyAlg :: Alg
emptyAlg = Alg IM.empty [] Nothing S.empty 0 0 "" []

initWorld :: World
initWorld = World emptyAlg (Frame IM.empty [] Nothing "" noHi 0 0 0 "") [] 0 [] M.empty False

main :: IO ()
main = runSketch (sketch "13-05 Decrease Key" initWorld)
  { skBack  = hsb 340 30 9
  , skStart = \_ -> resetWorld
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

resetWorld :: IO World
resetWorld = do
  ks <- freshKeys start S.empty
  let a0 = execM (mapM_ addRoot ks >> extractMin False) emptyAlg { aUsed = S.fromList ks }
  -- 少しキーを減らして、印のある節点を作っておく
  -- 孫の節点を親より小さくして切り取り、その親に印をつけておく
  a1 <- foldM prepare a0 [1 .. 5 :: Int]
  let a2 = execM (snap "Click a node to decrease its key. Black dot = mark (already lost 1 child)" noHi) a1
      f = case aFrames a2 of
            (x : _) -> x
            [] -> wCur initWorld
  pure initWorld { wAlg = a2 { aFrames = [] }, wCur = f }
  where
    prepare a _ = do
      let ns = aNodes a
          ok n = case nPar n >>= (`IM.lookup` ns) of
            Just p -> nPar p /= Nothing && not (nMark p)
            Nothing -> False
      c <- pick [i | (i, n) <- IM.toList ns, ok n]
      case c of
        Nothing -> pure a
        Just i -> do
          d <- randomRIO (1, 7)
          let pk = maybe 0 nKey (nPar (getN i ns) >>= (`IM.lookup` ns))
          pure (execM (decrease i (pk - d) False) a)

freshKeys :: Int -> S.Set Int -> IO [Int]
freshKeys n used
  | n <= 0 = pure []
  | otherwise = do
      k <- randomRIO (200, 399)
      if S.member k used
        then freshKeys n used
        else (k :) <$> freshKeys (n - 1) (S.insert k used)

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

getN :: Int -> Nodes -> N
getN = IM.findWithDefault (N 0 0 [] Nothing False)

node :: Int -> M N
node i = gets (getN i . aNodes)

update :: Int -> (N -> N) -> M ()
update i f = modify (\a -> a { aNodes = IM.adjust f i (aNodes a) })

marksOf :: Nodes -> Int
marksOf ns = length (filter nMark (IM.elems ns))

phiOf :: Alg -> Int
phiOf a = length (aRoots a) + 2 * marksOf (aNodes a)

snap :: String -> Hi -> M ()
snap msg hi = modify $ \a ->
  a { aFrames = Frame (aNodes a) (aRoots a) (aMin a) msg hi (phiOf a) (length (aRoots a)) (marksOf (aNodes a)) (aCost a) : aFrames a }

-- 記録するときだけコマを作る
snapIf :: Bool -> String -> Hi -> M ()
snapIf logIt msg hi = if logIt then snap msg hi else pure ()

updMin :: Int -> M ()
updMin k = modify (\a -> a { aMin = Just (maybe k (min k) (aMin a)) })

addRoot :: Int -> M ()
addRoot k = do
  i <- gets aUid
  modify (\a -> a { aUid = i + 1, aNodes = IM.insert i (N k 0 [] Nothing False) (aNodes a), aRoots = aRoots a ++ [i] })
  updMin k

showKey :: Int -> String
showKey k = if k == neg then "-inf" else show k

-- ---------- DECREASE-KEY ----------
cut :: Int -> Int -> Bool -> M ()
cut x y logIt = do
  update y (\n -> n { nKids = filter (/= x) (nKids n), nDeg = nDeg n - 1 })
  update x (\n -> n { nPar = Nothing, nMark = False })
  modify (\a -> a { aRoots = aRoots a ++ [x], aCuts = aCuts a + 1 })
  xn <- node x
  yn <- node y
  snapIf logIt ("CUT: separate " ++ show (nKey xn) ++ " and its subtree from parent " ++ show (nKey yn) ++ ", move to root list (mark removed)") noHi { hKey = Just (nKey xn), hPar = Just (nKey yn) }

cascadingCut :: Int -> Bool -> M ()
cascadingCut y logIt = do
  yn <- node y
  case nPar yn of
    Nothing -> pure ()
    Just z
      | not (nMark yn) -> do
          update y (\n -> n { nMark = True })
          snapIf logIt (show (nKey yn) ++ " lost its first child: mark it and stop") noHi { hKey = Just (nKey yn) }
      | otherwise -> do
          zn <- node z
          snapIf logIt (show (nKey yn) ++ " is already marked (lost a 2nd child): cut it too! (cascade)") noHi { hKey = Just (nKey yn), hPar = Just (nKey zn) }
          cut y z logIt
          cascadingCut z logIt

decrease :: Int -> Int -> Bool -> M ()
decrease x nk0 logIt = do
  used <- gets aUsed
  xn <- node x
  phi0 <- gets phiOf
  let old = nKey xn
      free n = if n /= neg && S.member n used then free (n - 1) else n
      nk = free nk0
  modify (\a -> a { aCuts = 0, aUsed = S.insert nk (S.delete old (aUsed a)) })
  update x (\n -> n { nKey = nk })
  snapIf logIt ("Decrease " ++ show old ++ " to " ++ showKey nk) noHi { hKey = Just nk, hFrom = Just old }
  case nPar xn of
    Just y -> do
      yn <- node y
      if nk < nKey yn
        then cut x y logIt >> cascadingCut y logIt
        else snapIf logIt ("Still larger than parent " ++ show (nKey yn) ++ ": nothing to do") noHi { hKey = Just nk }
    Nothing -> snapIf logIt "It is a root: nothing to do" noHi { hKey = Just nk }
  updMin nk
  if not logIt then pure () else do
    cuts <- gets aCuts
    phi1 <- gets phiOf
    let dphi = phi1 - phi0
    modify (\a -> a { aCost = "DECREASE-KEY: " ++ show cuts ++ " cuts, real cost " ++ show (1 + cuts) ++ ", dPhi = " ++ (if dphi >= 0 then "+" else "") ++ show dphi ++ ", amortized " ++ show (1 + cuts + dphi) ++ " (O(1))" })
    snap (if cuts > 0 then "Done: the " ++ show cuts ++ " cut trees now sit in the root list" else "Done") noHi { hKey = Just nk }

-- ---------- EXTRACT-MIN（04 と同じ）----------
extractMin :: Bool -> M ()
extractMin logIt = do
  roots <- gets aRoots
  ns <- gets aNodes
  mn <- gets aMin
  case find (\r -> Just (nKey (getN r ns)) == mn) roots of
    Nothing -> snapIf logIt "The heap is empty" noHi
    Just z -> do
      let zn = getN z ns
      mapM_ (\c -> update c (\n -> n { nPar = Nothing, nMark = False })) (nKids zn)
      modify (\a -> a { aRoots = filter (/= z) (aRoots a) ++ nKids zn
                      , aNodes = IM.delete z (aNodes a)
                      , aUsed = S.delete (nKey zn) (aUsed a) })
      snapIf logIt ("Extract the minimum " ++ showKey (nKey zn) ++ "; children go to the root list") noHi
      rs <- gets aRoots
      _ <- foldM consolidate IM.empty rs
      modify (\a -> a { aMin = Nothing })
      rs2 <- gets aRoots
      ns2 <- gets aNodes
      mapM_ (\r -> updMin (nKey (getN r ns2))) rs2
      mn2 <- gets aMin
      snapIf logIt ("CONSOLIDATE: link trees of equal degree, now " ++ show (length rs2) ++ " roots") noHi { hKey = mn2 }

-- 次数ごとの箱 A に入れていく。同じ次数の木があったらつなぐ
consolidate :: IM.IntMap Int -> Int -> M (IM.IntMap Int)
consolidate a0 w = do
  wn <- node w
  go w (nDeg wn) a0
  where
    go x d a = case IM.lookup d a of
      Nothing -> pure (IM.insert d x a)
      Just o -> do
        xk <- nKey <$> node x
        ok <- nKey <$> node o
        let (top, bot) = if xk > ok then (o, x) else (x, o)
        modify (\s -> s { aRoots = filter (/= bot) (aRoots s) })
        update top (\n -> n { nKids = nKids n ++ [bot], nDeg = nDeg n + 1 })
        update bot (\n -> n { nPar = Just top, nMark = False })
        go top (d + 1) (IM.delete d a)

-- d はランダムな減らす量
clickNode :: Int -> Int -> Bool -> M ()
clickNode k d delMode = do
  ns <- gets aNodes
  used <- gets aUsed
  case [i | (i, n) <- IM.toList ns, nKey n == k] of
    [] -> pure ()
    (x : _)
      | delMode -> do
          snap ("DELETE " ++ show k ++ ": decrease the key to -inf, then extract the minimum") noHi { hKey = Just k }
          decrease x neg True
          extractMin True
      | otherwise -> do
          let free n = if S.member n used || n == neg then free (n - 1) else n
          decrease x (free (k - d)) True

-- ---------- 配置 ----------
data Tg = Tg { tgP :: !Point, tgRoot, tgMark :: !Bool }

layoutForest :: Env -> Nodes -> [Int] -> Float -> ([(Int, Tg)], [(Int, Int)], Float)
layoutForest env ns rs y0 = (concat placed, concatMap edgesOf rs, u)
  where
    n i = getN i ns
    leaves i = if null (nKids (n i)) then 1 else sum (map leaves (nKids (n i))) :: Int
    depth i = if null (nKids (n i)) then 0 else 1 + foldl' max 0 (map depth (nKids (n i))) :: Int
    units = sum [fromIntegral (leaves r) + 0.6 | r <- rs] :: Float
    u = min 54 ((envW env - 60) / max 1 units)
    lh = min 80 ((envH env - y0 - 120) / fromIntegral (foldl' max 1 (map depth rs)))
    x0 = envW env / 2 - units * u / 2
    (_, placed) = mapAccumL (\x r -> (x + (fromIntegral (leaves r) + 0.6) * u, snd (place r x 0))) x0 rs
    place i left d
      | null (nKids (n i)) = emit i (left + u / 2) d []
      | otherwise =
          let (_, res) = mapAccumL (\l c -> (l + fromIntegral (leaves c) * u, place c l (d + 1))) left (nKids (n i))
          in emit i (midOf left (map fst res)) d (concatMap snd res)
    emit i cx d sub = (cx, sub ++ [(nKey (n i), Tg (cx, y0 + fromIntegral (d :: Int) * lh) (d == 0) (nMark (n i)))])
    edgesOf i = [(nKey (n i), nKey (n c)) | c <- nKids (n i)] ++ concatMap edgesOf (nKids (n i))

-- ---------- 時間を進める ----------
stepWorld :: Env -> Float -> World -> IO World
stepWorld env dt w0 = do
  let t1 = wTimer w0 + dt
      w1 = case wNext w0 of
             (f : rest) | t1 > speed -> w0 { wCur = f, wNext = rest, wTimer = 0 }
             _ -> w0 { wTimer = t1 }
  w2 <- case wPending w1 of
          (p : ps) | null (wNext w1) && wTimer w1 > speed * 0.4 -> runOp p w1 { wPending = ps, wTimer = 0 }
          _ -> pure w1
  let f = wCur w2
      (lst, _, _) = layoutForest env (fNodes f) (fRoots f) 200
      tg = M.fromList lst
      -- キーが変わった節点は前の位置を引きつぐ
      inherited = case (hKey (fHi f), hFrom (fHi f)) of
        (Just k, Just o) | M.notMember k (wKeyPos w2), Just p <- M.lookup o (wKeyPos w2) -> M.insert k p (wKeyPos w2)
        _ -> wKeyPos w2
  pure w2 { wKeyPos = M.mapWithKey (\k t -> lerpP 0.13 (M.findWithDefault (tgP t) k inherited) (tgP t)) tg }

runOp :: Op -> World -> IO World
runOp op w = do
  d <- randomRIO (30, 159)
  m <- case op of
    OpClick k -> pure (clickNode k d (wDelMode w))
    OpExt -> pure (extractMin True)
    OpRand -> do
      let ns = aNodes (wAlg w)
      c <- pick [n | n <- IM.elems ns, nPar n /= Nothing]
      pure (maybe (pure ()) (\n -> clickNode (nKey n) d (wDelMode w)) c)
  let a = execM m (wAlg w)
  pure w { wAlg = a { aFrames = [] }, wNext = wNext w ++ reverse (aFrames a) }

-- ---------- 描画 ----------
drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (rootLinks ++ edgePics ++ nodePics ++ panel)
  where
    f = wCur w
    hi = fHi f
    (lst, edges, u) = layoutForest env (fNodes f) (fRoots f) 200
    tg = M.fromList lst
    posOf k = M.findWithDefault (0, 0) k (wKeyPos w)
    d = clampF 18 38 (u * 0.75)
    mouse = mouseXY env
    rk = [nKey (getN r (fNodes f)) | r <- fRoots f]
    rootLinks =
      [ lineP env 2 (hsb 340 40 60) (ax + d / 2, ay) (bx - d / 2, by)
      | (a, b) <- zip rk (drop 1 rk), let (ax, ay) = posOf a, let (bx, by) = posOf b ]
    edgePics =
      [ lineP env (if cutting then 4 else 2) (if cutting then hsb 0 90 100 else hsb 340 15 50) (posOf a) (posOf b)
      | (a, b) <- edges, let cutting = hPar hi == Just a && hKey hi == Just b ]
    nodePics = concat
      [ [ discP env (if isHi then hsb 50 90 100 else if tgRoot t then hsb 340 60 80 else hsb 340 30 42) p (d / 2) ]
        ++ [ ringP env (if wDelMode w then hsb 0 80 100 else hsb 0 0 100) 3 p (d / 2) | dist2 mouse p < d / 2 ]
        ++ (if tgMark t then [ discP env (hsb 0 0 3) (x + d * 0.4, y - d * 0.4) (d * 0.19), ringP env (hsb 0 0 100) 1.5 (x + d * 0.4, y - d * 0.4) (d * 0.19) ] else [])
        ++ [ txtFit env x y (max 14 (d * 0.4)) (d - 2) (if isHi then hsb 0 0 10 else hsb 0 0 100) (showKey k) ]
        ++ [ txtC env x (y - d / 2 - 14) 14 (hsb 50 90 100) "min" | fMin f == Just k ]
      | (k, t) <- M.toList tg
      , let p@(x, y) = posOf k
            isHi = hKey hi == Just k ]
    panel =
      [ txt env 24 18 26 (hsb 0 0 100) "Decrease key (CUT and CASCADING-CUT)"
      , txt env 24 56 15 (hsb 0 0 80)
          ("Roots t = " ++ show (fT f) ++ "   marks m = " ++ show (fM f) ++ "   Phi = t + 2m = " ++ show (fPhi f) ++ "   Mode: "
           ++ (if wDelMode w then "delete (D to go back)" else "decrease key (D for delete mode)"))
      , txt env 24 86 18 (hsb 50 80 100) (fMsg f)
      , txt env 24 116 14 (hsb 340 40 90) (fCost f)
      , txt env 24 (envH env - 52) 14 (hsb 0 0 60) "Why marks? Cut before a node loses 2 children, so a degree k tree keeps at least F(k+2) nodes (see 06)"
      , txt env 24 (envH env - 30) 14 (hsb 0 0 60) "Click node = decrease key / D = delete mode / Space = random decrease / X = extract-min / R = new heap"
      ]

-- ---------- マウスとキー ----------
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseXY env
        near = [k | (k, p) <- M.toList (wKeyPos w), dist2 m p < 19]
    in pure $ case near of
         (k : _) -> push (OpClick k)
         [] -> w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (push OpRand)
  EventKey (Char c) Down _ _
    | c `elem` "dD" -> pure w { wDelMode = not (wDelMode w) }
    | c `elem` "xX" -> pure (push OpExt)
    | c `elem` "rR" -> resetWorld
  _ -> pure w
  where push op = w { wPending = wPending w ++ [op] }

-- ---------- やってみよう ----------
-- 1. 黒い点のある節点の子をクリックしよう。連鎖的な切り取りはどこまで続く？
-- 2. cascadingCut を「印をつけるだけ」に変えると何が起きる？ 木の形を観察しよう
-- 3. start を 60 にして、スペースをくり返す。償却コストの表示はいつも小さいまま？

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

-- 塗りつぶした円（p は中心、r は半径）
discP :: Env -> Color -> Point -> Float -> Picture
discP env c (x, y) r = Color c (at (toG env x y) (disc (max 0.1 r)))

-- 円の輪郭
ringP :: Env -> Color -> Float -> Point -> Float -> Picture
ringP env c w (x, y) r = Color c (at (toG env x y) (ring (max 0.1 r) w))

-- 2 点の間を t の割合で進む（p5.js の lerp を x, y に）
lerpP :: Float -> Point -> Point -> Point
lerpP t (a, b) (c, d) = (lerpF a c t, lerpF b d t)

-- 列の i 番目（なければ Nothing）
safeAt :: [a] -> Int -> Maybe a
safeAt xs i = if i < 0 then Nothing else case drop i xs of
  (x : _) -> Just x
  [] -> Nothing

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
