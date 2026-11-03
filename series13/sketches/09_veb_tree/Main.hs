-- ============================================
-- 09 van Emde Boas 木 ─ O(lg lg u) の魔法
-- proto-vEB に「最小値 min と最大値 max」を足しただけ。しかも min はかたまりの中に入れない
-- これで SUCCESSOR・INSERT・DELETE が、1 回の呼び出しにつき再帰を高々 1 回しか呼ばなくなる → T(u) = T(√u) + O(1) = O(lg lg u)
-- 下のマスをクリック = INSERT / DELETE / スペース = ランダムな SUCCESSOR / P = PREDECESSOR / 数字 + Enter = SUCCESSOR(x) / ←→ = u（16・256）/ R = やり直し
-- ============================================
module Main (main) where

import Art
import Data.Char (isDigit)
import Data.List (foldl', mapAccumL)
import qualified Data.Set as S

startU :: Int
startU = 16           -- ★ 全体の大きさ（16 か 256）

speed :: Float
speed = 0.4           -- ★ 1 コマの秒数

start :: Int
start = 6             -- ★ 最初に入れる要素の数

-- vEB 木の節点（u = 2 のときは要約もかたまりもない）
data V = V
  { vU :: !Int, vId :: !Int, vMin, vMax :: !(Maybe Int)
  , vSummary :: !(Maybe V), vCluster :: ![V] }

-- 呼び出しの記録の 1 行
data Tr = Tr { trId :: !Int, trTxt :: !String, trD :: !Int, trRet :: !Bool }

data World = World
  { wU      :: !Int
  , wV      :: !V
  , wTrace  :: ![Tr]
  , wTi     :: !Int
  , wTimer  :: !Float
  , wOp     :: !String
  , wResult :: !String
  , wTyped  :: !String
  }

initWorld :: World
initWorld = World startU (snd (mk 0 startU)) [] 0 0 "" "" ""

main :: IO ()
main = runSketch (sketch "13-09 van Emde Boas Tree" initWorld)
  { skBack  = hsb 160 35 7
  , skStart = \_ -> resetWorld startU
  , skStep  = \_ dt w -> pure (stepWorld dt w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

mk :: Int -> Int -> (Int, V)
mk uid 2 = (uid + 1, V 2 uid Nothing Nothing Nothing [])
mk uid u =
  let s = sqrtU u
      (uid1, summ) = mk (uid + 1) s
      (uid2, kids) = mapAccumL (\i _ -> mk i s) uid1 [1 .. s]
  in (uid2, V u uid Nothing Nothing (Just summ) kids)

resetWorld :: Int -> IO World
resetWorld u = do
  let count = start * (if u > 16 then 6 else 1)
  xs <- mapM (const (randomRIO (0, u - 1))) [1 .. count]
  let v0 = snd (mk 0 u)
      addOne v x = if isMember v x then v else snd (insert v x 0)
  pure initWorld { wU = u, wV = foldl' addOne v0 xs }

sqrtU :: Int -> Int
sqrtU u = round (sqrt (fromIntegral u :: Float))

high, low :: Int -> Int -> Int
high u x = x `div` sqrtU u
low u x = x `mod` sqrtU u

idx :: Int -> Int -> Int -> Int
idx u h l = h * sqrtU u + l

clusterAt :: V -> Int -> V
clusterAt n h = case drop h (vCluster n) of
  (c : _) -> c
  [] -> V 2 (-1) Nothing Nothing Nothing []

setCluster :: Int -> V -> V -> V
setCluster h c n = n { vCluster = setAt h c (vCluster n) }

summaryOf :: V -> V
summaryOf n = maybe (V 2 (-1) Nothing Nothing Nothing []) id (vSummary n)

-- ---------- vEB 木の操作 ----------
-- ([記録], 値) の組で、計算しながら記録を集める（タプルは Writer モナドとして使える）
type Log a = ([Tr], a)

note :: Int -> String -> Int -> Bool -> Log ()
note i s d r = ([Tr i s d r], ())

call :: V -> String -> Maybe Int -> Int -> Log ()
call n op x d = note (vId n) (op ++ "(vEB(" ++ show (vU n) ++ ")" ++ maybe "" (\v -> ", " ++ show v) x ++ ")") d False

ret :: V -> Int -> String -> Log ()
ret n d s = note (vId n) ("-> " ++ s) d True

showNil :: Maybe Int -> String
showNil = maybe "NIL" show

isMember :: V -> Int -> Bool
isMember n x
  | Just x == vMin n || Just x == vMax n = True
  | vU n == 2 = False
  | otherwise = isMember (clusterAt n (high (vU n) x)) (low (vU n) x)

successor :: V -> Int -> Int -> Log (Maybe Int)
successor n x d = do
  call n "SUCCESSOR" (Just x) d
  let u = vU n
      h = high u x
      done v = ret n d (showNil v) >> pure v
  case () of
    _ | u == 2 -> done (if x == 0 && vMax n == Just 1 then Just 1 else Nothing)
      | Just m <- vMin n, x < m -> done (Just m)
      | Just ml <- vMax (clusterAt n h), low u x < ml -> do
          o <- successor (clusterAt n h) (low u x) (d + 1)
          done (idx u h <$> o)
      | otherwise -> do
          sc <- successor (summaryOf n) h (d + 1)
          case sc of
            Nothing -> done Nothing
            Just c -> done (idx u c <$> vMin (clusterAt n c))

predecessor :: V -> Int -> Int -> Log (Maybe Int)
predecessor n x d = do
  call n "PREDECESSOR" (Just x) d
  let u = vU n
      h = high u x
      done v = ret n d (showNil v) >> pure v
  case () of
    _ | u == 2 -> done (if x == 1 && vMin n == Just 0 then Just 0 else Nothing)
      | Just m <- vMax n, x > m -> done (Just m)
      | Just ml <- vMin (clusterAt n h), low u x > ml -> do
          o <- predecessor (clusterAt n h) (low u x) (d + 1)
          done (idx u h <$> o)
      | otherwise -> do
          pc <- predecessor (summaryOf n) h (d + 1)
          case pc of
            Nothing -> done (case vMin n of
                               Just m | x > m -> Just m
                               _ -> Nothing)
            Just c -> done (idx u c <$> vMax (clusterAt n c))

insert :: V -> Int -> Int -> Log V
insert n x0 d = do
  call n "INSERT" (Just x0) d
  case vMin n of
    Nothing -> do
      ret n d ("was empty: min = max = " ++ show x0 ++ " (no recursion)")
      pure n { vMin = Just x0, vMax = Just x0 }
    Just m -> do
      -- 新しい min なら、古い min を下へ入れる
      let (x, n1) = if x0 < m then (m, n { vMin = Just x0 }) else (x0, n)
      if x0 < m then note (vId n) ("new min; insert old min " ++ show x ++ " below") d False else pure ()
      n2 <- if vU n <= 2 then pure n1 else do
        let u = vU n
            h = high u x
            l = low u x
            c = clusterAt n1 h
        case vMin c of
          Nothing -> do
            s' <- insert (summaryOf n1) h (d + 1)
            note (vId c) ("empty cluster: O(1) (min = max = " ++ show l ++ ")") (d + 1) False
            pure (setCluster h c { vMin = Just l, vMax = Just l } n1 { vSummary = Just s' })
          Just _ -> do
            c' <- insert c l (d + 1)
            pure (setCluster h c' n1)
      let n3 = if Just x > vMax n2 then n2 { vMax = Just x } else n2
      ret n d "done"
      pure n3

del :: V -> Int -> Int -> Log V
del n x0 d = do
  call n "DELETE" (Just x0) d
  let u = vU n
  case () of
    _ | vMin n == vMax n -> do
          ret n d "had 1 element: now empty"
          pure n { vMin = Nothing, vMax = Nothing }
      | u == 2 -> do
          let m = if x0 == 0 then 1 else 0
          ret n d "done"
          pure n { vMin = Just m, vMax = Just m }
      | otherwise -> do
          -- min を消すときは、次に小さい要素を新しい min にして、それを下から消す
          (x, n1) <-
            if Just x0 == vMin n
              then do
                let fc = maybe 0 id (vMin (summaryOf n))
                    x' = idx u fc (maybe 0 id (vMin (clusterAt n fc)))
                note (vId n) ("delete min: next smallest " ++ show x' ++ " is the new min") d False
                pure (x', n { vMin = Just x' })
              else pure (x0, n)
          let h = high u x
          c' <- del (clusterAt n1 h) (low u x) (d + 1)
          let n2 = setCluster h c' n1
          n3 <- case vMin c' of
            Nothing -> do
              s' <- del (summaryOf n2) h (d + 1)
              let n2' = n2 { vSummary = Just s' }
              pure $ if Just x == vMax n2'
                then n2' { vMax = case vMax s' of
                             Nothing -> vMin n2'
                             Just sm -> idx u sm <$> vMax (clusterAt n2' sm) }
                else n2'
            Just _ ->
              pure $ if Just x == vMax n2
                then n2 { vMax = idx u h <$> vMax c' }
                else n2
          ret n d "done"
          pure n3

-- 操作を始める
run :: String -> String -> [Tr] -> World -> World
run name r tr w = w { wTrace = tr, wTi = 0, wTimer = 0, wOp = name, wResult = r }

runSucc :: Int -> World -> World
runSucc x w =
  let (tr, r) = successor (wV w) x 0
  in run ("SUCCESSOR(" ++ show x ++ ")") (showNil r) tr w

stepWorld :: Float -> World -> World
stepWorld dt w
  | wTi w < length (wTrace w) - 1 =
      let t = wTimer w + dt
      in if t > speed then w { wTimer = 0, wTi = wTi w + 1 } else w { wTimer = t }
  | otherwise = w

-- ---------- 描画 ----------
drawVEB :: Env -> V -> Float -> Float -> Float -> Float -> Int -> S.Set Int -> [Picture]
drawVEB env n x y w h active seen =
  [ fillBox env (if on then hsb 160 45 28 else if was then hsb 160 35 18 else hsb 160 20 (if empty then 8 else 13)) x y w h
  , frameBox env (if on then hsb 50 90 100 else if was then hsb 160 60 80 else hsb 160 20 (if empty then 22 else 40)) (if on then 3.5 else 1.2) x y w h
  ] ++ labels ++ inside
  where
    on = active == vId n
    was = S.member (vId n) seen
    empty = vMin n == Nothing
    dash = maybe "-" show
    title = "vEB(" ++ show (vU n) ++ ")"
    valCol = if empty then hsb 0 0 40 else hsb 50 80 100
    nameCol = hsb 0 0 (if empty then 45 else 85)
    -- 幅に合わせて min と max の書き方を変える（せまい箱は上が min、下が max）
    (labels, top)
      | w >= 150 = ([ txt env (x + 4) (y + 3) 14 nameCol title
                    , txt env (x + 4) (y + 20) 14 valCol ("min " ++ dash (vMin n) ++ "  max " ++ dash (vMax n)) ], 38)
      | w >= 90 = ([ txt env (x + 4) (y + 3) 14 nameCol title
                   , txt env (x + 4) (y + 20) 14 valCol ("min " ++ dash (vMin n))
                   , txt env (x + 4) (y + 37) 14 valCol ("max " ++ dash (vMax n)) ], 56)
      | otherwise = ([ txt env (x + 4) (y + 3) 14 valCol (dash (vMin n))
                     , txt env (x + 4) (y + 20) 14 valCol (dash (vMax n)) ], 38)
    inside
      | vU n == 2 || w < 110 || h < 90 = []
      | otherwise =
          let s = sqrtU (vU n)
              pad = 5
              cw = (w - pad * (fromIntegral s + 1)) / fromIntegral s
              ch = (h - top - pad * 3) / 2
          in drawVEB env (summaryOf n) (x + w / 2 - cw / 2) (y + top) cw ch active seen
             ++ [ txtR env (x + w / 2 - cw / 2 - 6) (y + top + ch / 2) 14 (hsb 330 60 90) "Summary ->" | w > 300 ]
             ++ concat
                [ drawVEB env c (x + pad + fromIntegral i * (cw + pad)) (y + top + ch + pad * 2) cw ch active seen
                | (i, c) <- zip [0 :: Int ..] (vCluster n) ]

-- 下のマスの配置
data Cells = Cells { gW, gCw, gCh, gX0, gY0 :: !Float, gPer, gRows :: !Int }

cellGeom :: Env -> Int -> Cells
cellGeom env u =
  let bw = envW env - 400
      per = if u == 16 then 16 else 32
      rows = u `div` per
      cw = min 48 (bw / fromIntegral per)
      ch = if u == 16 then 34 else 20
  in Cells bw cw ch (24 + bw / 2 - fromIntegral per * cw / 2) (envH env - 70 - fromIntegral rows * ch) per rows

cellUnder :: Env -> Int -> Maybe Int
cellUnder env u =
  let g = cellGeom env u
      (mx, my) = mouseXY env
      c = floor ((mx - gX0 g) / gCw g)
      r = floor ((my - gY0 g) / gCh g)
  in if c < 0 || c >= gPer g || r < 0 || r >= gRows g then Nothing else Just (r * gPer g + c)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (drawVEB env (wV w) 24 112 (gW g) (gY0 g - 130) activeId seen ++ cells ++ tracePics ++ head')
  where
    u = wU w
    h = envH env
    g = cellGeom env u
    shownAll = take (wTi w + 1) (wTrace w)
    activeId = maybe (-1) trId (safeAt (wTrace w) (wTi w))
    seen = S.fromList (map trId shownAll)
    cells = concat
      [ [ fillBox env (if isMember (wV w) i then hsb 160 60 75 else hsb 160 10 15) x y (gCw g - 2) (gCh g - 3)
        , frameBox env (hsb 160 20 40) 1 x y (gCw g - 2) (gCh g - 3) ]
        ++ [ txtFit env (x + gCw g / 2 - 1) (y + gCh g / 2 - 1) 14 (gCw g - 4) (hsb 0 0 100) (show i) ]
      | i <- [0 .. u - 1]
      , let x = gX0 g + fromIntegral (i `mod` gPer g) * gCw g
            y = gY0 g + fromIntegral (i `div` gPer g) * gCh g ]
    -- 呼び出しの記録
    px = envW env - 360
    calls = length (filter (\t -> not (trRet t) && '(' `elem` trTxt t) (wTrace w))
    depth = foldl' max 0 (map trD (wTrace w))
    rows = concat
      [ [ (col, trD t, l) | l <- wrapWords (max 10 (28 - trD t)) (trTxt t) ]
      | (j, t) <- zip [0 :: Int ..] shownAll
      , let col | trRet t = hsb 140 50 85
                | j == wTi w = hsb 50 90 100
                | otherwise = hsb 0 0 80 ]
    tracePics =
      [ txt env px 106 16 (hsb 0 0 100) (if null (wOp w) then "Choose an operation" else wOp w) ] ++
      [ txt env px 126 15 (hsb 0 0 85) (show calls ++ " calls, max depth " ++ show depth) | not (null (wOp w)) ] ++
      [ txt env (px + fromIntegral d * 16) (152 + fromIntegral j * 18) 14 c l
      | (j, (c, d, l)) <- zip [0 :: Int ..] (lastN 26 rows) ] ++
      [ txt env px (h - 110) 17 (hsb 50 90 100) ("Answer: " ++ wResult w)
      | not (null (wTrace w)), wTi w == length (wTrace w) - 1 ]
    lglg = round (logBase 2 (logBase 2 (fromIntegral u :: Float))) :: Int
    head' =
      [ txt env 24 14 24 (hsb 0 0 100) "van Emde Boas tree"
      , txt env 24 46 14 (hsb 0 0 80)
          ("u = " ++ show u ++ ": depth lg lg u = " ++ show lglg ++ ". At most 1 recursion per level, T(u) = T(sqrt u) + O(1). u = 2^64: only 6 levels!")
      , txt env 24 66 14 (hsb 0 0 80)
          "Key idea: min is not stored in the clusters. Insert into an empty cluster is O(1),"
      , txt env 24 84 14 (hsb 0 0 80)
          "so the recursion into the summary and the recursion into the cluster never both happen."
      , txt env px (h - 80) 14 (hsb 0 0 60) ("Typing: " ++ (if null (wTyped w) then "-" else wTyped w))
      , txt env 24 (h - 48) 14 (hsb 0 0 60) "Click a cell = INSERT or DELETE / Space = SUCCESSOR / P = PREDECESSOR"
      , txt env 24 (h - 28) 14 (hsb 0 0 60) "Digits + Enter = SUCCESSOR(x) / Left, Right = u / R = reset   (narrow boxes: min on top, max below)"
      ]

-- ---------- マウスとキー ----------
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    pure $ case cellUnder env u of
      Nothing -> w
      Just i
        | isMember (wV w) i ->
            let (tr, v') = del (wV w) i 0
            in (run ("DELETE(" ++ show i ++ ")") "done" tr w) { wV = v' }
        | otherwise ->
            let (tr, v') = insert (wV w) i 0
            in (run ("INSERT(" ++ show i ++ ")") "done" tr w) { wV = v' }
  EventKey (Char c) Down _ _ | isDigit c -> pure w { wTyped = lastN 3 (wTyped w ++ [c]) }
  EventKey k Down _ _ | isBackKey k -> pure w { wTyped = take (length (wTyped w) - 1) (wTyped w) }
  EventKey (SpecialKey KeyEnter) Down _ _ | not (null (wTyped w)) ->
    pure (runSucc (max 0 (min (u - 1) (digitsToInt (wTyped w)))) w) { wTyped = "" }
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    x <- randomRIO (0, u - 1)
    pure (runSucc x w)
  EventKey (SpecialKey KeyRight) Down _ _ | u /= 256 -> resetWorld 256
  EventKey (SpecialKey KeyLeft) Down _ _ | u /= 16 -> resetWorld 16
  EventKey (Char c) Down _ _
    | c `elem` "pP" -> do
        x <- randomRIO (0, u - 1)
        let (tr, r) = predecessor (wV w) x 0
        pure (run ("PREDECESSOR(" ++ show x ++ ")") (showNil r) tr w)
    | c `elem` "rR" -> resetWorld u
  _ -> pure w
  where u = wU w

-- ---------- やってみよう ----------
-- 1. 08 の proto-vEB と同じ SUCCESSOR で、呼び出しの回数を比べよう
-- 2. u = 256 で空のかたまりに INSERT すると、深さはいくつ？ 再帰が 1 本道になっているのを確かめよう
-- 3. min をかたまりに入れてしまうと、どの行の再帰が 2 本になる？ insert を読んで考えよう

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

-- 右揃えの文字（右端の中央が x, y）
txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtR env x y size c s = txt env (x - fromIntegral (length s) * size * 0.72) (y - size / 2) size c s

-- 太さのある折れ線
pathP :: Env -> Float -> Color -> [Point] -> Picture
pathP env w c ps = Color c (thickPath w [toG env x y | (x, y) <- ps])

-- 塗りつぶした長方形（x, y は左上）
fillBox :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
fillBox env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 長方形の枠（x, y は左上、sw は線の太さ）
frameBox :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
frameBox env c sw x y w h = pathP env sw c [(x - sw / 2, y), (x + w + sw / 2, y), (x + w, y), (x + w, y + h), (x + w + sw / 2, y + h), (x - sw / 2, y + h), (x, y + h), (x, y)]

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

-- 文章を n 文字ごとに折り返す
wrapWords :: Int -> String -> [String]
wrapWords n = go "" . words
  where
    go cur [] = [cur | not (null cur)]
    go "" (x : xs) = go x xs
    go cur (x : xs)
      | length cur + 1 + length x <= n = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs

-- マスの中に入る文字（幅 maxW を超えるときは横に縮める）
txtFit :: Env -> Float -> Float -> Float -> Float -> Color -> String -> Picture
txtFit env x y size maxW c s =
  let (gx, gy) = toG env x y
      w = fromIntegral (length s) * size * 0.9
      k = min 1 (maxW / max 1 w)
  in Translate gx gy (Scale k 1 (labelC 0 0 size c s))
