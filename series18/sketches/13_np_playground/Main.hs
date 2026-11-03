-- ============================================
-- 13 NP 完全の遊び場（自由制作のベース）
-- グラフを描いて、4 つの NP 完全問題をその場で解かせる。答えが見つかると、選ばれた点が順に音を奏でる
-- 1：最小頂点被覆　2：最大クリーク　3：3 色塗り分け（グラフ彩色）　4：ハミルトン閉路
-- 何もない所をクリック：点（クリックで音）/ 点から点へドラッグ：線 / スペース：解く / R：ランダム / C：消す / S：答えをもう一度鳴らす
-- ★ 色・音階・問題を変えて、自分の「解ける絵」をつくろう
-- ============================================
module Main (main) where

import Art
import Control.Monad (filterM, forM_, replicateM, when)
import Data.Bits (complement, popCount, shiftL, testBit, (.&.), (.|.))
import Data.List (elemIndex, find)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Maybe (isJust)

maxN :: Int
maxN = 16                  -- ★ 点の上限（大きいと解くのに時間がかかる）

stepsPerFrame :: Int
stepsPerFrame = 3000       -- ★ 1 フレームに進める探索の手数

tones :: [Int]
tones = [0, 2, 4, 7, 9, 12, 14, 16]   -- ★ 音階

palette :: [Float]
palette = [350, 45, 190, 120]         -- ★ 色相（3 色塗りの 3 色＋α）

data Node = Node { nX :: !Float, nY :: !Float, nFlash :: !Float }

-- ジェネレーターが進める状態
data S = S
  { sSteps :: !Int
  , sCol   :: !(IM.IntMap Int)     -- 3 色塗りの途中
  , sPath  :: [Int]                -- ハミルトン閉路の途中（新しい点が先頭）
  , sUsed  :: !IS.IntSet
  , sDone  :: Maybe (Maybe [Int])  -- 終わったら Just（答えがなければ Just Nothing）
  }

-- 鳴らす予定の音
data Note = Note
  { qAt   :: !Float    -- 鳴らす時刻（秒）
  , qNode :: !Int
  , qMidi :: !Int
  , qWave :: !Wave
  }

data World = World
  { wNodes   :: [Node]
  , wEdges   :: [(Int, Int)]
  , wProblem :: !Int
  , wS       :: !S
  , wRest    :: [S]
  , wSolving :: !Bool
  , wAnswer  :: Maybe [Int]
  , wStatus  :: String
  , wDrag    :: !Int
  , wQueue   :: [Note]
  }

emptyS :: S
emptyS = S 0 IM.empty [] IS.empty Nothing

emptyWorld :: World
emptyWorld = World [] [] 1 emptyS [] False Nothing "" (-1) []

main :: IO ()
main = runSketch (sketch "18-13 NP-Complete Playground" emptyWorld)
  { skBack  = hsb 240 30 8
  , skStart = \env -> randomGraph env emptyWorld
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

randomGraph :: Env -> World -> IO World
randomGraph env w = do
  ps <- replicateM 10 $ do
    x <- randomRIO (120, envW env - 120)
    y <- randomRIO (170, envH env - 90)
    pure (x, y)
  let ip = zip [0 ..] ps
      limit = min (envW env) (envH env) * 0.35
  es <- filterM (const (chance 0.55)) [(i, j) | (i, a) <- ip, (j, b) <- ip, i < j, dist2 a b < limit]
  pure (clearAnswer w { wNodes = [Node x y 0 | (x, y) <- ps], wEdges = es })

clearAnswer :: World -> World
clearAnswer w = w { wS = emptyS, wRest = [], wSolving = False, wAnswer = Nothing, wStatus = "" }

adj :: World -> Int -> Int -> Bool
adj w a b = (a, b) `elem` wEdges w || (b, a) `elem` wEdges w

-- 隣の点（線を引いた順）
nbrTable :: [(Int, Int)] -> IM.IntMap [Int]
nbrTable = foldl' add IM.empty
  where add m (a, b) = IM.insertWith (flip (++)) b [a] (IM.insertWith (flip (++)) a [b] m)

-- ---------- 解く（ジェネレーターで少しずつ）----------
tick :: Int -> Gen S ()
tick every = do
  modify (\s -> s { sSteps = sSteps s + 1 })
  k <- gets sSteps
  when (k `mod` every == 0) yield

-- 部分集合を小さい順（または大きい順）に調べる
subsetSearch :: Int -> (Int -> Bool) -> Bool -> Gen S (Maybe [Int])
subsetSearch n test wantMin =
  let sizes = if wantMin then [0 .. n] else [n, n - 1 .. 0]
      order = concatMap (\c -> filter ((== c) . popCount) [0 .. (1 `shiftL` n) - 1 :: Int]) sizes
      go [] = pure Nothing
      go (m : ms) = do
        tick 64
        if test m then pure (Just [i | i <- [0 .. n - 1], testBit m i]) else go ms
  in go order

coloring :: Int -> IM.IntMap [Int] -> Gen S (Maybe [Int])
coloring n nb = do
  ok <- go 0
  col <- gets sCol
  pure (if ok then Just [IM.findWithDefault 0 i col | i <- [0 .. n - 1]] else Nothing)
  where
    go v
      | v == n = pure True
      | otherwise = try v [0, 1, 2]
    try _ [] = pure False
    try v (c : cs) = do
      tick 32
      col <- gets sCol
      let free = all (\u -> IM.lookup u col /= Just c) (IM.findWithDefault [] v nb)
      if free
        then do
          modify (\s -> s { sCol = IM.insert v c (sCol s) })
          r <- go (v + 1)
          if r
            then pure True
            else modify (\s -> s { sCol = IM.delete v (sCol s) }) >> try v cs
        else try v cs

hamilton :: Int -> IM.IntMap [Int] -> Gen S (Maybe [Int])
hamilton n nb
  | n < 3 = pure Nothing
  | otherwise = do
      modify (\s -> s { sPath = [0], sUsed = IS.singleton 0 })
      ok <- go
      path <- gets sPath
      pure (if ok then Just (reverse path) else Nothing)
  where
    go = do
      tick 32
      path <- gets sPath
      case path of
        [] -> pure False
        (v : _)
          | length path == n -> pure (0 `elem` IM.findWithDefault [] v nb)
          | otherwise -> try (IM.findWithDefault [] v nb)
    try [] = pure False
    try (u : us) = do
      used <- gets sUsed
      if IS.member u used
        then try us
        else do
          modify (\s -> s { sUsed = IS.insert u (sUsed s), sPath = u : sPath s })
          r <- go
          if r
            then pure True
            else do
              modify (\s -> s { sUsed = IS.delete u (sUsed s), sPath = drop 1 (sPath s) })
              try us

startSolve :: World -> World
startSolve w0 =
  let w = clearAnswer w0
      n = length (wNodes w)
      es = wEdges w
      nb = nbrTable es
      -- 隣の点のビット列（クリークの判定を速くする）
      nbMask v = foldl' (\m u -> m .|. (1 `shiftL` u)) (1 `shiftL` v) (IM.findWithDefault [] v nb) :: Int
      masks = IM.fromList [(v, nbMask v) | v <- [0 .. n - 1]]
      isCover m = all (\(a, b) -> testBit m a || testBit m b) es
      isClique m = all (\v -> not (testBit m v) || m .&. complement (IM.findWithDefault 0 v masks) == 0) [0 .. n - 1]
      solver = case wProblem w of
        1 -> subsetSearch n isCover True
        2 -> subsetSearch n isClique False
        3 -> coloring n nb
        _ -> hamilton n nb
      program = do
        r <- solver
        modify (\s -> s { sDone = Just r })
        yield
  in if n == 0
       then w
       else w { wRest = steps program emptyS, wSolving = True, wStatus = "searching..." }

-- 1 フレーム分だけ探索を進める
runSolver :: Int -> World -> World
runSolver t0 w = case wRest w of
  [] -> w { wSolving = False }
  (s : r)
    | isJust (sDone s) -> w { wS = s, wRest = [] }
    | sSteps s - t0 >= stepsPerFrame -> w { wS = s, wRest = r }
    | otherwise -> runSolver t0 w { wS = s, wRest = r }

stepW :: Env -> Float -> World -> IO World
stepW env _ w0 = do
  let w1 = decay w0
  w2 <- if wSolving w1
    then do
      let w' = runSolver (sSteps (wS w1)) w1
      case sDone (wS w') of
        Just ans -> playAnswer env w' { wAnswer = ans, wSolving = False, wRest = []
                                      , wStatus = maybe "no answer" (const "found!") ans }
        Nothing -> pure w'
    else pure w1
  playDue env w2

decay :: World -> World
decay w = w { wNodes = [p { nFlash = max 0 (nFlash p - 0.05) } | p <- wNodes w] }

-- 予定の時刻が来た音を鳴らして、その点を光らせる
playDue :: Env -> World -> IO World
playDue env w = do
  let now = envTime env
      (due, later) = (filter ((<= now) . qAt) (wQueue w), filter ((> now) . qAt) (wQueue w))
  forM_ due $ \q -> playTone (midiFreq (qMidi q)) 0.25 0.4 (qWave q)
  let lit = IS.fromList (map qNode due)
  pure w { wQueue = later
         , wNodes = [if IS.member i lit then p { nFlash = 1 } else p | (i, p) <- zip [0 ..] (wNodes w)] }

degreeOf :: Env -> Float -> Int
degreeOf env y =
  let top = fromIntegral (length tones) - 0.01
  in floor (clampF 0 top (remap y (envH env) 0 0 top))

toneAt :: Int -> Int
toneAt d = sum (take 1 (drop d tones))

-- 答えを 1 点ずつ音にする。0.16 秒ずつずらして予定に入れる
playAnswer :: Env -> World -> IO World
playAnswer env w = case wAnswer w of
  Nothing -> do
    playTone (midiFreq 40) 0.25 0.6 Sawtooth   -- 答えがないと低い音
    pure w
  Just ans ->
    let problem = wProblem w
        seqV = if problem == 3 then zipWith const [0 ..] ans else ans
        wave = if problem == 1 || problem == 3 then Triangle else Sine
        colorOf v = sum (take 1 (drop v ans))
        note (k, v) =
          let y = maybe 0 nY (nodeAt w v)
              midi = 57 + toneAt (degreeOf env y) + (if problem == 3 then colorOf v * 4 else 0)
          in Note (envTime env + fromIntegral (k :: Int) * 0.16) v midi wave
    in pure w { wQueue = map note (zip [0 ..] seqV) }

nodeAt :: World -> Int -> Maybe Node
nodeAt w i = case drop i (wNodes w) of
  (p : _) -> Just p
  [] -> Nothing

posOf :: World -> Int -> Point
posOf w i = maybe (0, 0) (\p -> (nX p, nY p)) (nodeAt w i)

hueAt :: Int -> Float
hueAt k = sum (take 1 (drop k palette))

drawW :: Env -> World -> Picture
drawW env w =
  let answer = wAnswer w
      problem = wProblem w
      hotEdge (a, b) = case answer of
        Just ans
          | problem == 2 -> a `elem` ans && b `elem` ans
          | problem == 4 ->
              case (elemIndex a ans, elemIndex b ans) of
                (Just i, Just j) -> abs (i - j) == 1 || abs (i - j) == length ans - 1
                _ -> False
        _ -> False
      edge e@(a, b) =
        let hot = hotEdge e
        in seg env (if hot then hsb (hueAt 1) 80 100 else hsba 0 0 100 0.3) (if hot then 5 else 2) (posOf w a) (posOf w b)
      dragLine
        | wDrag w >= 0 = seg env (hsb 0 0 100) 2 (posOf w (wDrag w)) (mouseP5 env (envMouse env))
        | otherwise = Blank
      node (i, p) =
        let (hue, s, on) = case answer of
              Just ans
                | problem == 3 -> (hueAt (sum (take 1 (drop i ans))), 75, True)
                | problem <= 2 && i `elem` ans -> (hueAt (if problem == 1 then 0 else 1), 80, True)
                | problem == 4 -> (hueAt 2, 70, True)
              _ -> (220, 20, False)
            tag = case answer of
              Just ans | problem == 4 -> show (maybe 0 (+ 1) (elemIndex i ans))
              _ -> show i
        in Pictures
             [ dot env (hsb hue s (if on then 100 else 60)) (nX p, nY p) (32 + nFlash p * 30)
             , txtC env (nX p) (nY p) 14 (hsb 0 0 10) tag
             ]
      names = ["Min vertex cover", "Max clique", "3-coloring", "Hamiltonian cycle"]
      name = concat (take 1 (drop (problem - 1) names))
      extra = case answer of
        Just ans | problem <= 2 -> "   " ++ show (length ans) ++ " points"
        _ -> ""
  in Pictures
       ( map edge (wEdges w)
      ++ [dragLine]
      ++ map node (zip [0 ..] (wNodes w))
      ++ [ txt env 20 34 14 (hsb 0 0 100) "1 Vertex cover  2 Clique  3 3-coloring  4 Ham cycle  Space: solve  S: replay  R: random  C: clear"
         , txt env 20 74 24 (hsb (hueAt (problem - 1)) 70 100)
             (name ++ (if null (wStatus w) then "" else "   " ++ wStatus w))
         , txt env 20 104 16 (hsb 0 0 80)
             (show (length (wNodes w)) ++ " points (max " ++ show maxN ++ ")   " ++ show (length (wEdges w))
               ++ " edges   search steps " ++ commas (sSteps (wS w)) ++ extra)
         , txt env 20 (envH env - 22) 15 (hsb 0 0 55) "All NP-complete (optimization: NP-hard). Add points and watch the step count grow."
         ] )

hit :: World -> Point -> Int
hit w m = maybe (-1) fst (find (\(_, p) -> dist2 m (nX p, nY p) < 20) (zip [0 ..] (wNodes w)))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let m@(mx, my) = mouseP5 env p
        h = hit w m
    in if my < 120
         then pure w
         else if h >= 0
           then do
             playTone (midiFreq (57 + toneAt (degreeOf env my))) 0.2 0.3 Triangle
             pure w { wDrag = h, wNodes = [if i == h then q { nFlash = 1 } else q | (i, q) <- zip [0 ..] (wNodes w)] }
           else if length (wNodes w) < maxN
             then pure (clearAnswer w { wDrag = -1, wNodes = wNodes w ++ [Node mx my 0] })
             else pure w { wDrag = -1 }
  EventKey (MouseButton LeftButton) Up _ p
    | wDrag w >= 0 ->
        let from = wDrag w
            to = hit w (mouseP5 env p)
            w1 = w { wDrag = -1 }
        in pure $ if to >= 0 && to /= from && not (adj w from to)
             then clearAnswer w1 { wEdges = wEdges w ++ [(from, to)] }
             else w1
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (startSolve w)
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> pure (clearAnswer w { wProblem = fromEnum c - fromEnum '0' })
    | c `elem` "sS" -> playAnswer env w
    | c `elem` "rR" -> randomGraph env w
    | c `elem` "cC" -> pure (clearAnswer w { wNodes = [], wEdges = [], wQueue = [] })
  _ -> pure w

-- ---------- 自由制作のヒント ----------
-- 1. 星座や顔を描いて、ハミルトン閉路で「一筆書きの曲」をつくる
-- 2. 3 色塗りの色ごとに楽器（Wave）を変えて、3 パートの曲に
-- 3. 問題を 1 つ足してみよう（例：最大独立集合 = 補グラフのクリーク、彩色を 4 色に）

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- gloss のマウス座標を p5.js の座標に直す
mouseP5 :: Env -> Point -> Point
mouseP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 直径 d の円（p5.js の circle）
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

-- 文字：左端の下が (x, y)
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str

-- 文字：中心が (x, y)
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = labelC (gx env x) (gy env y) s c str

-- ---------- 道具：その他 ----------
-- 確率 p で True
chance :: Float -> IO Bool
chance p = (< p) <$> randomRIO (0, 1)

-- 3 桁ごとにカンマを入れる（toLocaleString）
commas :: Int -> String
commas v
  | v < 0 = '-' : commas (negate v)
  | v < 1000 = show v
  | otherwise = commas (v `div` 1000) ++ "," ++ drop 1 (show (1000 + v `mod` 1000))

-- ---------- 道具：ジェネレーター（function*）の代わり ----------
-- yield のたびに状態を 1 つ流す計算。結果は「状態の遅延リスト」になり、
-- World がその残りを持っていて、1 手進めるたびに先頭を 1 つ取る。
newtype Gen s a = Gen { unGen :: s -> (a -> s -> [s]) -> [s] }

instance Functor (Gen s) where
  fmap f (Gen g) = Gen (\s k -> g s (k . f))

instance Applicative (Gen s) where
  pure x = Gen (\s k -> k x s)
  Gen f <*> Gen g = Gen (\s k -> f s (\h s' -> g s' (k . h)))

instance Monad (Gen s) where
  Gen g >>= f = Gen (\s k -> g s (\x s' -> unGen (f x) s' k))

yield :: Gen s ()
yield = Gen (\s k -> s : k () s)

gets :: (s -> a) -> Gen s a
gets f = Gen (\s k -> k (f s) s)

modify :: (s -> s) -> Gen s ()
modify f = Gen (\s k -> k () $! f s)

steps :: Gen s () -> s -> [s]
steps (Gen g) s0 = g s0 (\_ _ -> [])
