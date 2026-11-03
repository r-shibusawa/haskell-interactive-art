-- ============================================
-- 08 proto-vEB ─ 「要約」を再帰的に入れ子にする
-- 大きさ u の構造の中に、大きさ √u の「要約」1 つと「かたまり」√u 個。その中にまた √u … と入れ子にしていく
-- MEMBER は 1 本道で O(lg lg u)。でも SUCCESSOR や INSERT は 1 回の呼び出しで再帰を 2 回以上呼ぶので遅い → 09 で改良
-- スペース = ランダムな x の SUCCESSOR / 数字 + Enter = SUCCESSOR(x) / 下の数直線クリック = INSERT・MEMBER / M = MINIMUM / R = やり直し
-- ============================================
module Main (main) where

import Art
import Data.Char (isDigit)
import Data.List (mapAccumL)
import qualified Data.Set as S

bigU :: Int
bigU = 16             -- ★ 全体の大きさ（16 = 2^(2^2)）

speed :: Float
speed = 0.35          -- ★ 1 コマの秒数

start :: Int
start = 5             -- ★ 最初に入れる要素の数

-- proto-vEB 構造。いちばん小さい u = 2 はビットが 2 つ
data Proto
  = Base !Int !Bool !Bool                 -- 番号, A[0], A[1]
  | Rec !Int !Int !Proto ![Proto]         -- 大きさ u, 番号, 要約, かたまり

-- 呼び出しの記録の 1 行
data Tr = Tr { trId :: !Int, trTxt :: !String, trD :: !Int, trRet :: !Bool }

data World = World
  { wV       :: !Proto
  , wTrace   :: ![Tr]
  , wTi      :: !Int
  , wTimer   :: !Float
  , wOp      :: !String
  , wResult  :: !String
  , wTyped   :: !String
  , wMembers :: !(S.Set Int)
  }

initWorld :: World
initWorld = World (snd (mkProto 0 bigU)) [] 0 0 "" "" "" S.empty

main :: IO ()
main = runSketch (sketch "13-08 proto-vEB" initWorld)
  { skBack  = hsb 280 30 8
  , skStart = \_ -> resetWorld
  , skStep  = \_ dt w -> pure (stepWorld dt w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

mkProto :: Int -> Int -> (Int, Proto)
mkProto uid 2 = (uid + 1, Base uid False False)
mkProto uid u =
  let s = sqrtU u
      (uid1, summ) = mkProto (uid + 1) s
      (uid2, kids) = mapAccumL (\i _ -> mkProto i s) uid1 [1 .. s]
  in (uid2, Rec u uid summ kids)

resetWorld :: IO World
resetWorld = do
  xs <- mapM (const (randomRIO (0, bigU - 1))) [1 .. start]
  let v0 = snd (mkProto 0 bigU)
      v1 = foldl (\v x -> snd (ins v x 0)) v0 xs
  pure initWorld { wV = v1, wMembers = S.fromList xs }

sqrtU :: Int -> Int
sqrtU u = round (sqrt (fromIntegral u :: Float))

high, low :: Int -> Int -> Int
high u x = x `div` sqrtU u
low u x = x `mod` sqrtU u

idx :: Int -> Int -> Int -> Int
idx u h l = h * sqrtU u + l

protoU, protoId :: Proto -> Int
protoU (Base _ _ _) = 2
protoU (Rec u _ _ _) = u
protoId (Base i _ _) = i
protoId (Rec _ i _ _) = i

-- かたまりの h 番目（必ずある）
clusterAt :: [Proto] -> Int -> Proto
clusterAt cs h = case drop h cs of
  (c : _) -> c
  [] -> Base (-1) False False

-- ---------- 操作 ----------
-- ([記録], 値) の組で、計算しながら記録を集める（タプルは Writer モナドとして使える）
type Log a = ([Tr], a)

call :: Proto -> String -> Maybe Int -> Int -> Log ()
call n op x d = ([Tr (protoId n) (op ++ "(P" ++ show (protoU n) ++ maybe "" (\v -> ", " ++ show v) x ++ ")") d False], ())

ret :: Proto -> (a -> String) -> a -> Int -> Log a
ret n sh v d = ([Tr (protoId n) ("-> " ++ sh v) d True], v)

showNil :: Maybe Int -> String
showNil = maybe "NIL" show

member :: Proto -> Int -> Int -> Log String
member n x d = do
  call n "MEMBER" (Just x) d
  case n of
    Base _ a b -> ret n id (if (if x == 0 then a else b) then "yes" else "no") d
    Rec u _ _ cs -> do
      r <- member (clusterAt cs (high u x)) (low u x) (d + 1)
      ret n id r d

minimumP :: Proto -> Int -> Log (Maybe Int)
minimumP n d = do
  call n "MINIMUM" Nothing d
  case n of
    Base _ a b -> ret n showNil (if a then Just 0 else if b then Just 1 else Nothing) d
    Rec u _ summ cs -> do
      mc <- minimumP summ (d + 1)
      case mc of
        Nothing -> ret n showNil Nothing d
        Just c -> do
          off <- minimumP (clusterAt cs c) (d + 1)
          ret n showNil (idx u c <$> off) d

successor :: Proto -> Int -> Int -> Log (Maybe Int)
successor n x d = do
  call n "SUCCESSOR" (Just x) d
  case n of
    Base _ _ b -> ret n showNil (if x == 0 && b then Just 1 else Nothing) d
    Rec u _ summ cs -> do
      let h = high u x
      off <- successor (clusterAt cs h) (low u x) (d + 1)
      case off of
        Just o -> ret n showNil (Just (idx u h o)) d
        Nothing -> do
          sc <- successor summ h (d + 1)
          case sc of
            Nothing -> ret n showNil Nothing d
            Just c -> do
              o <- minimumP (clusterAt cs c) (d + 1)
              ret n showNil (idx u c <$> o) d

-- 挿入は新しい木を返す（記録はいつも作るが、使わなければ計算されない）
ins :: Proto -> Int -> Int -> ([Tr], Proto)
ins n x d = do
  call n "INSERT" (Just x) d
  case n of
    Base i a b -> do
      let n' = if x == 0 then Base i True b else Base i a True
      _ <- ret n id "done" d
      pure n'
    Rec u i summ cs -> do
      let h = high u x
      c' <- ins (clusterAt cs h) (low u x) (d + 1)
      s' <- ins summ h (d + 1)
      _ <- ret n id "done" d
      pure (Rec u i s' (setAt h c' cs))

-- 操作を始める
run :: String -> Log String -> World -> World
run name (tr, r) w = w { wTrace = tr, wTi = 0, wTimer = 0, wOp = name, wResult = r }

runSucc :: Int -> World -> World
runSucc x w = run ("SUCCESSOR(" ++ show x ++ ")") (showNil <$> successor (wV w) x 0) w

stepWorld :: Float -> World -> World
stepWorld dt w
  | wTi w < length (wTrace w) - 1 =
      let t = wTimer w + dt
      in if t > speed then w { wTimer = 0, wTi = wTi w + 1 } else w { wTimer = t }
  | otherwise = w

-- ---------- 描画 ----------
drawProto :: Env -> Proto -> Float -> Float -> Float -> Float -> Maybe Int -> Int -> S.Set Int -> [Picture]
drawProto env n x y w h base active seen =
  [ fillBox env (if on then hsb 280 40 28 else if was then hsb 280 30 18 else hsb 280 20 12) x y w h
  , frameBox env (if on then hsb 50 90 100 else if was then hsb 280 50 80 else hsb 280 20 40) (if on then 3.5 else 1.2) x y w h
  , txt env (x + 5) (y + 3) 14 (hsb 0 0 75) ((if w > 170 then "proto-vEB(" else "P(") ++ show (protoU n) ++ ")")
  ] ++ inside
  where
    pid = protoId n
    on = active == pid
    was = S.member pid seen
    inside = case n of
      Base _ a b ->
        let cw = (w - 12) / 2
        in concat
           [ [ fillBox env (if v then hsb 280 60 85 else hsb 280 10 22) cx (y + 22) cw (h - 28)
             , txtC env (cx + cw / 2) (y + 22 + (h - 28) / 2 - 8) 16 (hsb 0 0 100) (if v then "1" else "0") ]
             ++ [ txtC env (cx + cw / 2) (y + h - 16) 14 (hsb 0 0 70) (show (b0 + i)) | Just b0 <- [base] ]
           | (i, v) <- zip [0 ..] [a, b], let cx = x + 4 + fromIntegral i * (cw + 4) ]
      Rec u _ summ cs ->
        let s = sqrtU u
            pad = 6
            top = 20
            cw = (w - pad * (fromIntegral s + 1)) / fromIntegral s
            ch = (h - top - pad * 3) / 2
        in drawProto env summ (x + w / 2 - cw / 2) (y + top) cw ch Nothing active seen
           ++ [ txtR env (x + w / 2 - cw / 2 - 6) (y + top + ch / 2) 14 (hsb 330 60 90) (if w > 300 then "Summary ->" else "sum") ]
           ++ concat
              [ drawProto env c (x + pad + fromIntegral i * (cw + pad)) (y + top + ch + pad * 2) cw ch
                  (fmap (+ i * (u `div` s)) base) active seen
              | (i, c) <- zip [0 ..] cs ]

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (drawProto env (wV w) 24 120 bw bh (Just 0) activeId seen ++ line ++ tracePics ++ head')
  where
    h = envH env
    bw = envW env - 400
    bh = h - 260
    shownAll = take (wTi w + 1) (wTrace w)
    activeId = maybe (-1) trId (safeAt (wTrace w) (wTi w))
    seen = S.fromList (map trId shownAll)
    -- 数直線（集合の中身）
    (cw, nx, ny) = lineGeom env
    line = concat
      [ [ fillBox env (if S.member i (wMembers w) then hsb 280 60 85 else hsb 280 10 16) x ny (cw - 3) 34
        , frameBox env (hsb 280 20 40) 1 x ny (cw - 3) 34
        , txtC env (x + cw / 2 - 1) (ny + 17) 14 (hsb 0 0 100) (show i) ]
      | i <- [0 .. bigU - 1], let x = nx + fromIntegral i * cw ]
    -- 呼び出しの記録
    px = envW env - 360
    start0 = max 0 (length shownAll - 24)
    tracePics =
      [ txt env px 120 16 (hsb 0 0 100)
          (if null (wOp w) then "Choose an operation"
           else wOp w ++ ": " ++ show (length (filter (not . trRet) (wTrace w))) ++ " calls") ] ++
      [ txt env (px + fromIntegral (trD t) * 18) (150 + fromIntegral j * 19) 14
          (if trRet t then hsb 140 50 85 else if start0 + j == wTi w then hsb 50 90 100 else hsb 0 0 80) (trTxt t)
      | (j, t) <- zip [0 :: Int ..] (drop start0 shownAll) ] ++
      [ txt env px (h - 170) 17 (hsb 50 90 100) ("Answer: " ++ wResult w)
      | not (null (wTrace w)), wTi w == length (wTrace w) - 1, not (null (wResult w)) ]
    s = sqrtU bigU
    lglg = round (logBase 2 (logBase 2 (fromIntegral bigU :: Float))) :: Int
    head' =
      [ txt env 24 14 24 (hsb 0 0 100) "proto-vEB structure (recursive summary)"
      , txt env 24 46 14 (hsb 0 0 80)
          ("proto-vEB(" ++ show bigU ++ ") holds a summary and " ++ show s ++ " clusters, all proto-vEB(" ++ show s ++ "). Depth lg lg u = " ++ show lglg ++ ". P(u) = proto-vEB(u)")
      , txt env 24 68 14 (hsb 0 0 80)
          "MEMBER: T(u) = T(sqrt u) + O(1) = O(lg lg u)     INSERT: T(u) = 2T(sqrt u) + O(1) = O(lg u)"
      , txt env 24 90 14 (hsb 0 0 80)
          "SUCCESSOR: T(u) = 2T(sqrt u) + O(lg sqrt u) = O(lg u lg lg u)"
      , txt env 24 (h - 74) 14 (hsb 0 0 60) ("Typing: " ++ (if null (wTyped w) then "-" else wTyped w))
      , txt env 24 (h - 52) 14 (hsb 0 0 60) "Space = SUCCESSOR of a random x / Digits + Enter = SUCCESSOR(x) / M = MINIMUM / R = reset"
      , txt env 24 (h - 30) 14 (hsb 0 0 60) "Click the number line: INSERT if absent, MEMBER if present"
      ]

lineGeom :: Env -> (Float, Float, Float)
lineGeom env =
  let bw = envW env - 400
      cw = min 48 (bw / fromIntegral bigU)
  in (cw, 24 + bw / 2 - fromIntegral bigU * cw / 2, envH env - 120)

-- ---------- マウスとキー ----------
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseXY env
        (cw, nx, ny) = lineGeom env
        i = floor ((mx - nx) / cw) :: Int
    in pure $
         if my > ny && my < ny + 34 && i >= 0 && i < bigU
           then if S.member i (wMembers w)
                  then run ("MEMBER(" ++ show i ++ ")") (member (wV w) i 0) w
                  else
                    let (tr, v') = ins (wV w) i 0
                    in (run ("INSERT(" ++ show i ++ ")") (tr, "done") w) { wV = v', wMembers = S.insert i (wMembers w) }
           else w
  EventKey (Char c) Down _ _ | isDigit c -> pure w { wTyped = lastN 2 (wTyped w ++ [c]) }
  EventKey k Down _ _ | isBackKey k -> pure w { wTyped = take (length (wTyped w) - 1) (wTyped w) }
  EventKey (SpecialKey KeyEnter) Down _ _ | not (null (wTyped w)) ->
    pure (runSucc (max 0 (min (bigU - 1) (digitsToInt (wTyped w)))) w) { wTyped = "" }
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    x <- randomRIO (0, bigU - 1)
    pure (runSucc x w)
  EventKey (Char c) Down _ _
    | c `elem` "mM" -> pure (run "MINIMUM" (showNil <$> minimumP (wV w) 0) w)
    | c `elem` "rR" -> resetWorld
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. MEMBER と SUCCESSOR で、呼び出しの回数を比べよう。なぜこんなに違う？
-- 2. いちばん大きい要素の SUCCESSOR を呼ぶと最悪になるのはなぜ？ 記録を追ってみよう
-- 3. bigU を 256 にしてみよう（箱がとても小さくなる）。呼び出しの回数はどれだけ増える？

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
