-- ============================================
-- 12 迷路をつくる ─ 互いに素な集合で「輪のない道」
-- マス目どうしの壁をランダムな順に見て、両側がまだ別の集合なら壁をこわして UNION。同じ集合なら残す（ランダム版クラスカル法）
-- 輪ができないので、どの 2 マスの間にも道がちょうど 1 本 ─ 完璧な迷路。色 = 集合。合併のたびに音が鳴る 🔊
-- スペース = 一時停止 / ↑↓ = 速さ / 1〜3 = 大きさ / S = 解く / M = 消音 / R = 作り直す
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, when)
import Data.List (foldl', sortOn)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

startCols, startRows, startPerFrame :: Int
startCols = 24        -- ★ 迷路の大きさ
startRows = 15
startPerFrame = 1     -- ★ 1 フレームに見る壁の数

scaleSteps :: [Int]
scaleSteps = [0, 2, 4, 7, 9]   -- ★ 音階（ペンタトニック、半音の数）

-- 互いに素な集合の森（書きかえが多いので、更新できる配列を使う）
data Forest = Forest
  { fParent :: !(UM.IOVector Int)
  , fRank   :: !(UM.IOVector Int)
  , fSize   :: !(UM.IOVector Int)
  }

data World = World
  { wCols, wRows :: !Int
  , wPerFrame :: !Int
  , wForest   :: !Forest
  , wWalls    :: !(U.Vector (Int, Int))
  , wWi       :: !Int
  , wOpen     :: !IS.IntSet        -- こわした壁
  , wRunning  :: !Bool
  , wMuted    :: !Bool
  , wSolved   :: !(Maybe [Int])
  , wSolveT   :: !Float
  , wLastNote :: !Float
  , wUnions   :: !Int
  , wRejected :: !Int
  , wRootOf   :: !(U.Vector Int)   -- 描画用：各マスの根
  , wSizeOf   :: !(U.Vector Int)   -- 描画用：各マスの集合の大きさ
  }

main :: IO ()
main = do
  w0 <- newMaze startCols startRows startPerFrame True False
  runSketch (sketch "13-12 Maze with Kruskal" w0)
    { skBack  = hsb 260 30 8
    , skStep  = stepWorld
    , skDraw  = drawWorld
    , skEvent = onEvent
    }

newMaze :: Int -> Int -> Int -> Bool -> Bool -> IO World
newMaze cols rows perFrame running muted = do
  let n = cols * rows
  par <- U.thaw (U.enumFromN 0 n)
  rnk <- UM.replicate n 0
  sz <- UM.replicate n 1
  ws <- shuffle
    (  [ (i, i + 1) | r <- [0 .. rows - 1], c <- [0 .. cols - 2], let i = r * cols + c ]
    ++ [ (i, i + cols) | r <- [0 .. rows - 2], c <- [0 .. cols - 1], let i = r * cols + c ] )
  pure World
    { wCols = cols, wRows = rows, wPerFrame = perFrame, wForest = Forest par rnk sz
    , wWalls = U.fromList ws, wWi = 0, wOpen = IS.empty, wRunning = running, wMuted = muted
    , wSolved = Nothing, wSolveT = 0, wLastNote = -1, wUnions = 0, wRejected = 0
    , wRootOf = U.enumFromN 0 n, wSizeOf = U.replicate n 1 }

-- ---------- 互いに素な集合の森（ランク + 経路圧縮）----------
findSet :: Forest -> Int -> IO Int
findSet f x = do
  p <- UM.read (fParent f) x
  if p == x then pure x else do
    r <- findSet f p
    UM.write (fParent f) x r
    pure r

-- 合併できたら True
unionSet :: Forest -> Int -> Int -> IO Bool
unionSet f a b = do
  x0 <- findSet f a
  y0 <- findSet f b
  if x0 == y0 then pure False else do
    rx0 <- UM.read (fRank f) x0
    ry0 <- UM.read (fRank f) y0
    let (x, y) = if rx0 > ry0 then (y0, x0) else (x0, y0)
    UM.write (fParent f) x y
    sx <- UM.read (fSize f) x
    UM.modify (fSize f) (+ sx) y
    when (rx0 == ry0) (UM.modify (fRank f) (+ 1) y)
    pure True

wallKey :: World -> Int -> Int -> Int
wallKey w a b = a * (wCols w * wRows w) + b

-- ---------- 迷路を解く（幅優先探索）----------
solve :: World -> World
solve w =
  let n = wCols w * wRows w
      pairs = [ (k `div` n, k `mod` n) | k <- IS.toList (wOpen w) ]
      adj = IM.fromListWith (++) (concat [ [(a, [b]), (b, [a])] | (a, b) <- pairs ])
      bfs [] prev = prev
      bfs (v : q) prev =
        let new = [ u | u <- IM.findWithDefault [] v adj, IM.notMember u prev ]
        in bfs (q ++ new) (foldl' (\m u -> IM.insert u v m) prev new)
      prevs = bfs [0] (IM.singleton 0 (-1))
      walk v acc = case IM.lookup v prevs of
        Nothing -> acc
        Just p -> if p < 0 then v : acc else walk p (v : acc)
      path = walk (n - 1) []
  in w { wSolved = case path of
           (0 : _) -> Just path
           _ -> Nothing
       , wSolveT = 0 }

-- ---------- 時間を進める ----------
stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  w1 <- if wRunning w0 then foldM (\w _ -> oneWall env w) w0 [1 .. wPerFrame w0] else pure w0
  let n = wCols w1 * wRows w1
      f = wForest w1
  roots <- U.generateM n (findSet f)
  sizes <- U.generateM n (\i -> UM.read (fSize f) (roots U.! i))
  pure w1 { wRootOf = roots, wSizeOf = sizes
          , wSolveT = case wSolved w1 of
              Just p -> min (fromIntegral (length p)) (wSolveT w1 + 1.5)
              Nothing -> wSolveT w1 }

-- 壁を 1 枚見る
oneWall :: Env -> World -> IO World
oneWall env w
  | wWi w >= U.length (wWalls w) = pure w
  | otherwise = do
      let (a, b) = wWalls w U.! wWi w
      joined <- unionSet (wForest w) a b
      if not joined
        then pure w { wWi = wWi w + 1, wRejected = wRejected w + 1 }
        else do
          r <- findSet (wForest w) a
          s <- UM.read (fSize (wForest w)) r
          -- 集合が大きくなるほど高い音にする
          let now = envTime env
              deg = floor (logBase 2 (fromIntegral s :: Float) * 1.2) :: Int
              (oct, k) = deg `divMod` length scaleSteps
              midi = 52 + oct * 12 + maybe 0 id (safeAt scaleSteps k)
              ring' = now - wLastNote w > 0.055
          when (ring' && not (wMuted w)) (playTone (midiFreq midi) 0.12 0.25 Triangle)
          pure w { wWi = wWi w + 1, wUnions = wUnions w + 1
                 , wOpen = IS.insert (wallKey w a b) (wOpen w)
                 , wLastNote = if ring' then now else wLastNote w }

-- ---------- 描画 ----------
drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (cellPics ++ wallPics ++ curPic ++ solvedPic ++ ends ++ panel)
  where
    cols = wCols w
    rows = wRows w
    fc = fromIntegral cols :: Float
    fr = fromIntegral rows :: Float
    cs = min ((envW env - 80) / fc) ((envH env - 200) / fr)
    x0 = envW env / 2 - fc * cs / 2
    y0 = 120
    px c = x0 + fromIntegral c * cs
    py r = y0 + fromIntegral r * cs
    center i = (px (i `mod` cols) + cs / 2, py (i `div` cols) + cs / 2)
    cellPics =
      [ fillBox env (hsb (fromIntegral ((root * 61) `mod` 360)) 55 (25 + fromIntegral (min 60 (sz * 2)))) (px c) (py r) (cs + 0.5) (cs + 0.5)
      | r <- [0 .. rows - 1], c <- [0 .. cols - 1]
      , let i = r * cols + c
            root = wRootOf w U.! i
            sz = wSizeOf w U.! i ]
    sw = max 1.5 (cs * 0.08)
    wallCol = hsb 0 0 95
    closed a b = not (IS.member (wallKey w a b) (wOpen w))
    wallPics =
      frameBox env wallCol sw x0 y0 (fc * cs) (fr * cs) :
      [ lineP env sw wallCol (px (c + 1), py r) (px (c + 1), py (r + 1))
      | r <- [0 .. rows - 1], c <- [0 .. cols - 2], let i = r * cols + c, closed i (i + 1) ] ++
      [ lineP env sw wallCol (px c, py (r + 1)) (px (c + 1), py (r + 1))
      | r <- [0 .. rows - 2], c <- [0 .. cols - 1], let i = r * cols + c, closed i (i + cols) ]
    -- 今見ている壁
    finished = wWi w >= U.length (wWalls w)
    curPic = [ lineP env 4 (hsb 50 90 100) (center a) (center b)
             | not finished, wRunning w, let (a, b) = wWalls w U.! wWi w ]
    -- 解
    solvedPic = case wSolved w of
      Just p -> [ pathP env (cs * 0.3) (hsb 50 100 100) (map center (take (ceiling (wSolveT w)) p)) ]
      Nothing -> []
    ends =
      [ discP env (hsb 140 70 90) (x0 + cs / 2, y0 + cs / 2) (cs * 0.25)
      , discP env (hsb 0 80 95) (x0 + (fc - 0.5) * cs, y0 + (fr - 0.5) * cs) (cs * 0.25) ]
    panel =
      [ txt env 24 18 26 (hsb 0 0 100) "Maze from disjoint sets"
      , txt env 24 56 15 (hsb 0 0 80)
          ("Sets " ++ show (cols * rows - wUnions w) ++ "   Walls broken " ++ show (wUnions w) ++ " (done at cells - 1)   Walls kept " ++ show (wRejected w)
           ++ " (would make a loop)   " ++ (if finished then "Done! S to solve" else ""))
      , txt env 24 80 15 (hsb 0 0 80)
          ("FIND-SET and UNION: " ++ show (wWi w) ++ " times each. With rank + path compression, almost O(1) each")
      , txt env 24 (envH env - 30) 14 (hsb 0 0 60)
          ("Space = pause / Up, Down = speed (" ++ show (wPerFrame w) ++ ") / 1-3 = size / S = solve / M = mute" ++ (if wMuted w then " (muted)" else "") ++ " / R = new maze")
      ]

-- ---------- キー ----------
onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wRunning = not (wRunning w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wPerFrame = min 60 (wPerFrame w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wPerFrame = max 1 (wPerFrame w `div` 2) }
  EventKey (Char '1') Down _ _ -> remake 12 8
  EventKey (Char '2') Down _ _ -> remake 24 15
  EventKey (Char '3') Down _ _ -> remake 48 30
  EventKey (Char c) Down _ _
    | c `elem` "sS" -> pure (solve w)
    | c `elem` "mM" -> pure w { wMuted = not (wMuted w) }
    | c `elem` "rR" -> remake (wCols w) (wRows w)
  _ -> pure w
  where remake c r = newMaze c r (wPerFrame w) (wRunning w) (wMuted w)

-- ---------- やってみよう ----------
-- 1. scaleSteps を変えて、迷路の音楽を作ろう。[0, 3, 5, 7, 10] だとどんな感じ？
-- 2. 壁の順番を shuffle しないで横から順に見ると、どんな迷路になる？
-- 3. こわした壁の数はいつも「マス目 − 1」。なぜ？ 3 を押して確かめよう

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- 文字（左上が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx (gy - size) size c s

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

-- 列の i 番目（なければ Nothing）
safeAt :: [a] -> Int -> Maybe a
safeAt xs i = if i < 0 then Nothing else case drop i xs of
  (x : _) -> Just x
  [] -> Nothing

-- 列をランダムに並べ直す
shuffle :: [a] -> IO [a]
shuffle xs = do
  rs <- mapM (const (randomRIO (0, 1 :: Double))) xs
  pure (map snd (sortOn fst (zip rs xs)))
