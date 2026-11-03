-- ============================================
-- 14 オフライン最小値問題 ─ 先に全部の命令を知っていれば
-- 「挿入（数字）」と「最小値を取り出す（E）」の列が最初から全部わかっているとき、どの E が何を取り出すかを互いに素な集合で求める
-- E で区切ったブロック K1, K2, … を集合にする。小さい数 i から順に：i の入ったブロック K_j の E が i を取り出す → K_j を次のブロックに合併
-- スペース = 1 歩進める / A = 自動再生 / N = 新しい命令列 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', intercalate, mapAccumL, sort, sortOn)
import qualified Data.IntMap.Strict as IM

nKeys :: Int
nKeys = 9             -- ★ 挿入する数の個数（1〜n を 1 回ずつ）

speed :: Float
speed = 0.9           -- ★ 自動再生の秒数

-- 命令：数を挿入する、または最小値を取り出す（E）
data Item = Ins !Int | E deriving Eq

-- ★ 最初の例（N で新しい列）
firstSeq :: [Item]
firstSeq = [Ins 6, Ins 3, E, Ins 8, Ins 1, E, E, Ins 9, Ins 4, E, Ins 2, Ins 7, E, Ins 5, E]

data World = World
  { wSeq       :: ![Item]
  , wN, wM     :: !Int
  , wParent    :: !(IM.IntMap Int)
  , wLabel     :: !(IM.IntMap Int)    -- 集合の根 → ブロック番号
  , wExists    :: !(IM.IntMap Bool)
  , wNextB     :: !(IM.IntMap Int)
  , wExtracted :: !(IM.IntMap Int)    -- E の番号 → 取り出す数
  , wI, wPhase :: !Int
  , wMsg       :: !String
  , wHiBlock, wHiKey, wMergeTo :: !Int
  , wAuto      :: !Bool
  , wTimer     :: !Float
  , wPos       :: !(IM.IntMap Point)
  }

main :: IO ()
main = runSketch (sketch "13-14 Offline Minimum" (restart firstSeq False IM.empty))
  { skBack  = hsb 300 25 9
  , skStep  = \env dt w -> pure (stepWorld env dt w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ブロック番号 1..m+1。要素 = 数 1..n と、各ブロックの代表（空のブロック用）n+1..n+m+1
restart :: [Item] -> Bool -> IM.IntMap Point -> World
restart items auto pos =
  let m = length (filter (== E) items)
      n = length items - m
      base = IM.fromList [(i, i) | i <- [0 .. n + m + 1]]
      (_, parent) = foldl' place' (1, base) items
      place' (b, p) E = (b + 1 :: Int, p)
      place' (b, p) (Ins s) = (b, IM.insert s (n + b) p)   -- 数 s をブロック b の集合へ
      blocks = [1 .. m + 1]
  in World
       { wSeq = items, wN = n, wM = m, wParent = parent
       , wLabel = IM.fromList [(n + j, j) | j <- blocks]
       , wExists = IM.fromList [(j, True) | j <- blocks]
       , wNextB = IM.fromList [(j, j + 1) | j <- blocks]
       , wExtracted = IM.empty, wI = 0, wPhase = 0
       , wMsg = "Blocks K1..K" ++ show (m + 1) ++ " are the sets. Look at the numbers from 1 upward"
       , wHiBlock = -1, wHiKey = -1, wMergeTo = -1
       , wAuto = auto, wTimer = 0, wPos = pos }

newSequence :: Bool -> IO World
newSequence auto = do
  keys <- shuffle [1 .. nKeys]
  (_, items) <- walk 0 keys
  pure (restart items auto IM.empty)
  where
    walk :: Int -> [Int] -> IO (Int, [Item])
    walk h [] = pure (h, [])
    walk h (k : ks) = do
      (h1, es) <- pops (h + 1)
      (h2, rest) <- walk h1 ks
      pure (h2, Ins k : es ++ rest)
    pops h
      | h <= 0 = pure (h, [])
      | otherwise = do
          r <- randomRIO (0, 1 :: Float)
          if r < 0.45
            then do
              (h1, es) <- pops (h - 1)
              pure (h1, E : es)
            else pure (h, [])

-- FIND-SET（親をたどるだけ）
find' :: World -> Int -> Int
find' w x = let p = IM.findWithDefault x x (wParent w) in if p == x then x else find' w p

blockOfKey :: World -> Int -> Int
blockOfKey w k = IM.findWithDefault 0 (find' w k) (wLabel w)

advance :: World -> World
advance w
  | wI w >= n && wPhase w == 0 =
      w { wMsg = "Finished! Does it match the check line below?", wHiBlock = -1, wHiKey = -1 }
  | wPhase w == 0 =
      let i = wI w + 1
          j = blockOfKey w i
          w1 = w { wI = i, wHiKey = i, wHiBlock = j }
      in if j == wM w + 1
           then w1 { wMsg = show i ++ " is in the last block K" ++ show j ++ ": no E extracts it", wMergeTo = -1, wPhase = 0 }
           else w1 { wExtracted = IM.insert j i (wExtracted w)
                   , wMsg = "FIND-SET(" ++ show i ++ ") = K" ++ show j ++ ". E number " ++ show j ++ ", right after K" ++ show j ++ ", extracts " ++ show i
                   , wPhase = 1 }
  | otherwise =
      let j = wHiBlock w
          nextAlive l = if IM.findWithDefault True l (wExists w) then l else nextAlive (IM.findWithDefault (l + 1) l (wNextB w))
          l = nextAlive (IM.findWithDefault (j + 1) j (wNextB w))
          -- K_l = K_j ∪ K_l
          rj = find' w (n + j)
          rl = find' w (n + l)
      in w { wParent = IM.insert rj rl (wParent w)
           , wLabel = IM.insert rl l (wLabel w)
           , wExists = IM.insert j False (wExists w)
           , wMsg = "UNION: merge K" ++ show j ++ " into the next remaining block K" ++ show l ++ ". E number " ++ show j ++ " is used up"
           , wMergeTo = l, wPhase = 0 }
  where n = wN w

-- ---------- 配置 ----------
data Geo = Geo
  { gTw, gTx0, gTy, gBy, gBh :: !Float
  , gBlocks :: !(IM.IntMap (Float, Float))
  , gChips  :: !(IM.IntMap Point)
  }

geometry :: Env -> World -> Geo
geometry env w = Geo tw tx0 ty by bh blocks chips
  where
    items = wSeq w
    len = fromIntegral (max 1 (length items)) :: Float
    tw = min 58 ((envW env - 60) / len)
    tx0 = envW env / 2 - len * tw / 2
    ty = 130
    by = 250
    -- ブロックの左右の端（E の位置で区切る）
    (_, lastStart, blockList) = foldl' stepB (0 :: Int, tx0, []) (zip [0 :: Int ..] items)
    stepB (e, start, acc) (q, it)
      | it == E = let x = tx0 + fromIntegral q * tw
                  in (e + 1, x + tw, (e + 1, (start, x)) : acc)
      | otherwise = (e, start, acc)
    blocks = IM.fromList ((wM w + 1, (lastStart, tx0 + len * tw)) : blockList)
    widthOf j = let (a, b) = IM.findWithDefault (0, 0) j blocks in max (tw * 0.9) (b - a - 6)
    colsOf j = max 1 (floor (widthOf j / 34)) :: Int
    -- 数ごとの所属と、ブロックの中での順番
    (_, placed) = mapAccumL (\cnt k ->
                     let j = blockOfKey w k
                         c = IM.findWithDefault 0 j cnt
                     in (IM.insert j (c + 1) cnt, (k, j, c))) IM.empty [1 .. wN w]
    counts = IM.fromListWith (+) [(j, 1 :: Int) | (_, j, _) <- placed]
    needed j c = 60 + fromIntegral (ceiling (fromIntegral c / fromIntegral (colsOf j) :: Float) :: Int) * 34 :: Float
    bh = IM.foldlWithKey' (\b j c -> max b (needed j c)) 150 counts
    chips = IM.fromList
      [ (k, (a + 3 + 20 + fromIntegral (c `mod` colsOf j) * 34, by + 46 + fromIntegral (c `div` colsOf j) * 34))
      | (k, j, c) <- placed
      , let (a, _) = IM.findWithDefault (0, 0) j blocks ]

-- ---------- 時間を進める ----------
stepWorld :: Env -> Float -> World -> World
stepWorld env dt w0 =
  let w1 | wAuto w0 =
             let t = wTimer w0 + dt
             in if t > speed then (advance w0) { wTimer = 0 } else w0 { wTimer = t }
         | otherwise = w0
      g = geometry env w1
  in w1 { wPos = IM.mapWithKey (\k t -> lerpP 0.12 (IM.findWithDefault t k (wPos w1)) t) (gChips g) }

-- ---------- 描画 ----------
drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (seqPics ++ blockPics ++ chipPics ++ results ++ panel)
  where
    g = geometry env w
    tw = gTw g
    items = wSeq w
    n = wN w
    m = wM w
    ty = gTy g
    -- 命令列
    eNumbers = snd (mapAccumL (\e it -> if it == E then (e + 1, e + 1) else (e, e)) (0 :: Int) items)
    seqPics =
      txt env (gTx0 g) (ty - 22) 14 (hsb 0 0 70) "Instructions (run left to right)" : concat
      [ case it of
          E ->
            let ans = IM.lookup en (wExtracted w)
                done = ans /= Nothing
            in [ fillBox env (if done then hsb 50 85 100 else hsb 300 30 35) (x + 3) ty (tw - 6) 44
               , txtC env (x + tw / 2) (ty + 13) 15 (if done then hsb 0 0 10 else hsb 0 0 100) "E"
               , txtC env (x + tw / 2) (ty + 32) 14 (if done then hsb 0 0 10 else hsb 0 0 100) (maybe "?" show ans) ]
          Ins s ->
            [ fillBox env (hsb 300 20 22) (x + 3) ty (tw - 6) 44
            , txtC env (x + tw / 2) (ty + 22) 18 (hsb 0 0 85) (show s) ]
      | (q, it, en) <- zip3 [0 :: Int ..] items eNumbers
      , let x = gTx0 g + fromIntegral q * tw ]
    -- ブロック（集合）
    by = gBy g
    bh = gBh g
    blockPics = concat
      [ [ fillBox env (hsb 300 25 (if alive then 16 else 10)) (a + 3) by bw bh ]
        ++ [ if alive
               then frameBox env edge sw (a + 3) by bw bh
               else dashP env sw edge 4 5 [(a + 3, by), (a + 3 + bw, by), (a + 3 + bw, by + bh), (a + 3, by + bh), (a + 3, by)] ]
        ++ [ txtC env (a + 3 + bw / 2) (by + 14) 14 (hsb 0 0 (if alive then 85 else 35)) ("K" ++ show j) ]
      | j <- [1 .. m + 1]
      , let (a, b) = IM.findWithDefault (0, 0) j (gBlocks g)
            bw = max (tw * 0.9) (b - a - 6)
            alive = IM.findWithDefault True j (wExists w)
            hot = wHiBlock w == j
            mrg = wMergeTo w == j
            edge | hot = hsb 50 90 100
                 | mrg = hsb 140 70 90
                 | otherwise = hsb 300 30 (if alive then 50 else 20)
            sw = if hot || mrg then 3 else 1.2 ]
    -- 数のチップ（集合の中を動く）
    chipPics = concat
      [ [ discP env (if isHi then hsb 50 90 100 else if done then hsb 300 15 35 else hsb (fromIntegral ((k * 40) `mod` 360)) 50 80) p 15
        , txtC env (fst p) (snd p) 14 (if isHi then hsb 0 0 10 else hsb 0 0 (if done then 60 else 10)) (show k) ]
      | k <- [1 .. n]
      , let p = IM.findWithDefault (0, 0) k (wPos w)
            isHi = wHiKey w == k
            done = k <= wI w ]
    -- 結果
    ry = by + bh + 50
    results =
      [ txt env 30 ry 16 (hsb 0 0 100)
          ("extracted: [ " ++ intercalate ", " [maybe "." show (IM.lookup j (wExtracted w)) | j <- [1 .. m]] ++ " ]")
      , txt env 30 (ry + 30) 16 (hsb 0 0 70)
          ("Check (run with a heap): [ " ++ intercalate ", " (runHeap items) ++ " ]")
      ]
    panel =
      [ txt env 24 18 26 (hsb 0 0 100) "Offline minimum"
      , txt env 24 56 15 (hsb 0 0 80)
          ("Inserts n = " ++ show n ++ ", extracts m = " ++ show m ++ ". Only FIND-SET and UNION: O(n alpha(n)) in all (a heap needs O(n lg n))")
      , txt env 24 84 16 (hsb 50 80 100) (wMsg w)
      , txt env 24 (envH env - 30) 14 (hsb 0 0 60)
          ("Space = 1 step / A = autoplay" ++ (if wAuto w then " (ON)" else "") ++ " / N = new instructions / R = restart")
      ]

-- 確かめ：本当にヒープで順に実行する
runHeap :: [Item] -> [String]
runHeap = go []
  where
    go _ [] = []
    go h (Ins s : rest) = go (s : h) rest
    go h (E : rest) = case sort h of
      [] -> "empty" : go [] rest
      (x : xs) -> show x : go xs rest

-- ---------- キー ----------
onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (advance w)
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "nN" -> newSequence (wAuto w)
    | c `elem` "rR" -> pure (restart (wSeq w) (wAuto w) (wPos w))
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 答えを見る前に、紙の上で firstSeq の答えをヒープで計算してみよう
-- 2. なぜ小さい数から順に見るだけで、正しい E が決まるのか考えよう
-- 3. firstSeq を自分の命令列に書きかえて、取り出すものがないときの動きを見よう

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

-- 列をランダムに並べ直す
shuffle :: [a] -> IO [a]
shuffle xs = do
  rs <- mapM (const (randomRIO (0, 1 :: Double))) xs
  pure (map snd (sortOn fst (zip rs xs)))
