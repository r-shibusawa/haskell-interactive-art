-- ============================================
-- 63-12 三目並べの自己対戦 ─ 盤面の「価値」を表に覚えるエージェント同士を、何万回も戦わせる
-- 自分が打ったあとの盤面（アフターステート）の価値 V = 「そこから自分が勝つ見こみ」（勝ち 1、引き分け 0.5、負け 0）。
-- 打つ手は「打ったあとの V がいちばん大きいマス」。時々（ε）でたらめに打って新しい盤面を試す。
-- 次に自分の番が来たら、前の盤面の V を今の盤面の V に近づける（TD 学習）：V ← V + α (V′ − V)
-- 操作：盤をクリックして対戦 / N 新しい対局 / T 先手・後手を交代 / F 学習の速さ / スペース 学習を止める / R 学習をやり直す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (forM_, replicateM, when)
import Control.Monad.ST (ST, runST, stToIO)
import Data.Bits (shiftR, xor, (.|.))
import Data.Foldable (toList)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word32)
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import System.Environment (lookupEnv)
import Data.Functor.Identity (runIdentity)

alphaT, epsT :: Double
alphaT = 0.2                             -- ★ 学習率
epsT = 0.3                               -- ★ 自己対戦でのでたらめな手の割合

winLines :: [(Int, Int, Int)]
winLines = [(0, 1, 2), (3, 4, 5), (6, 7, 8), (0, 3, 6), (1, 4, 7), (2, 5, 8), (0, 4, 8), (2, 4, 6)]

-- 盤：0 空き、1 = X（先手）、2 = O（後手）
type Board = U.Vector Int

-- 価値の表：盤面の番号（3^9 = 19683 とおり）ごとの V。まだ覚えていない盤面は NaN
tableSize :: Int
tableSize = 19683

data World = World
  { wTable     :: !(U.Vector Double)
  , wSize      :: !Int                   -- 覚えた盤面の数
  , wGames     :: !Int
  , wEvalWin, wEvalDraw, wEvalLose :: !(Seq.Seq Double)
  , wSpeed     :: !Int
  , wPaused    :: !Bool
  , wBoard     :: !Board
  , wHumanFirst :: !Bool
  , wTurn      :: !Int
  , wResult    :: !Int
  , wMessage   :: !String
  , wRng       :: !Rng
  , wTest      :: [(String, Bool)] }

speedNames :: [String]
speedNames = ["slow", "normal", "fast"]

main :: IO ()
main = runSketch (sketch "63-12 Tic-tac-toe self-play" (world0 []))
  { skBack  = bgC
  , skStart = \_ -> do
      let rs = selfTest
      printSelfTest rs
      pure (world0 rs)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- seedRng(12)、表は空
world0 :: [(String, Bool)] -> World
world0 rs = newBoard (World emptyTable 0 0 Seq.empty Seq.empty Seq.empty 1 False U.empty True 1 0 "" (seedRng 12) rs)

emptyTable :: U.Vector Double
emptyTable = U.replicate tableSize (0 / 0)

emptyBoard :: Board
emptyBoard = U.replicate 9 0

-- 3 = 引き分け、0 = まだ続く
winnerOf :: Board -> Int
winnerOf b = case [ p | (i, j, k) <- winLines, let p = b U.! i, p /= 0, p == b U.! j, p == b U.! k ] of
  (p : _) -> p
  [] -> if U.elem 0 b then 0 else 3

-- 自分の石を 1 として数にする
keyOf :: Board -> Int -> Int
keyOf b me = U.foldl' (\k v -> k * 3 + (if v == 0 then 0 else if v == me then 1 else 2)) 0 b

-- 表にない盤面：終わっていれば結果、そうでなければ 0.5（look は表を読む関数）
valueWith :: Monad m => (Int -> m Double) -> Board -> Int -> m Double
valueWith look b me = do
  v <- look (keyOf b me)
  pure $ if not (isNaN v) then v else
    let w = winnerOf b in if w == me then 1 else if w == 0 || w == 3 then 0.5 else 0

setAt :: Board -> Int -> Int -> Board
setAt b m v = U.imap (\i x -> if i == m then v else x) b

emptiesOf :: Board -> [Int]
emptiesOf b = [ i | i <- [0 .. 8], b U.! i == 0 ]

-- 手を選ぶ：(マス, でたらめか)。rndM = 乱数、look = 表を読む
chooseMove :: Monad m => m Double -> (Int -> m Double) -> (R Int -> m Int) -> Board -> Int -> Double -> m (Int, Bool)
chooseMove rndM look liftM' b me eps = do
  let moves = emptiesOf b
  u <- rndM
  if u < eps then do
    v <- rndM
    pure (idx 0 moves (floor (v * fromIntegral (length moves))), True)
  else do
    vals <- mapM (\m -> valueWith look (setAt b m me) me) moves
    k <- liftM' (argmaxR vals)
    pure (idx 0 moves k, False)

-- ST で使う表と乱数
data Tab s = Tab { tV :: UM.MVector s Double, tSize :: STRef s Int, tRng :: RngRef s }

chooseS :: Tab s -> Board -> Int -> Double -> ST s (Int, Bool)
chooseS t = chooseMove (rndS (tRng t)) (UM.read (tV t)) (liftR (tRng t))

setV :: Tab s -> Int -> Double -> ST s ()
setV t k v = do
  old <- UM.read (tV t) k
  when (isNaN old) (readSTRef (tSize t) >>= \n -> writeSTRef (tSize t) $! n + 1)
  UM.write (tV t) k v

-- ★ 自己対戦 1 局。どちらの手番も同じ表（自分の石 = 1 として見る）で学ぶ
selfPlayGame :: Tab s -> Double -> Double -> ST s Int
selfPlayGame t alpha eps = go emptyBoard 1 Nothing Nothing
  where
    look = UM.read (tV t)
    go b me prev1 prev2 = do
      (m, explore) <- chooseS t b me eps
      let b' = setAt b m me
          w = winnerOf b'
          prevMe = if me == 1 then prev1 else prev2
      case prevMe of                     -- TD 学習
        Just pb | not explore -> do
          pv <- valueWith look pb me
          nv <- valueWith look b' me
          setV t (keyOf pb me) (pv + alpha * (nv - pv))
        _ -> pure ()
      let (p1, p2) = if me == 1 then (Just b', prev2) else (prev1, Just b')
      if w /= 0
        then do                          -- 終わったら、相手の最後の盤面も結果に近づける
          let o = 3 - me
          case (if o == 1 then p1 else p2) of
            Just pb -> do
              pv <- valueWith look pb o
              setV t (keyOf pb o) (pv + alpha * ((if w == 3 then 0.5 else 0) - pv))
            Nothing -> pure ()
          pure w
        else go b' (3 - me) p1 p2

-- 学習した手（ε = 0）とでたらめな相手。1 勝ち、0 引き分け、−1 負け
vsRandom :: Tab s -> Int -> ST s Int
vsRandom t side = go emptyBoard 1
  where
    go b me = do
      m <- if me == side then fst <$> chooseS t b me 0
           else do u <- rndS (tRng t)
                   let mv = emptiesOf b
                   pure (idx 0 mv (floor (u * fromIntegral (length mv))))
      let b' = setAt b m me
          w = winnerOf b'
      if w /= 0 then pure (if w == 3 then 0 else if w == side then 1 else -1) else go b' (3 - me)

-- 先手・後手 n 局ずつ：(勝ち, 引き分け, 負け) の割合
evaluateT :: Tab s -> Int -> ST s (Double, Double, Double)
evaluateT t n = do
  rs <- mapM (\side -> replicateM n (vsRandom t side)) [1, 2]
  let all' = concat rs
      frac f = fromIntegral (length (filter f all')) / fromIntegral (2 * n)
  pure (frac (> 0), frac (== 0), frac (< 0))

-- ---- 人との対局
newBoard :: World -> World
newBoard w = w { wBoard = emptyBoard, wTurn = 1, wResult = 0
               , wMessage = if wHumanFirst w then "You are X (first). Click a cell" else "You are O (second)" }

agentSide :: World -> Int
agentSide w = if wHumanFirst w then 2 else 1

playAt :: Int -> World -> World
playAt m w =
  let b = setAt (wBoard w) m (wTurn w)
      r = winnerOf b
      w1 = w { wBoard = b, wResult = r, wTurn = 3 - wTurn w }
  in if r /= 0 then w1 { wMessage = if r == 3 then "Draw! N for the next game" else if r == agentSide w then "The agent wins. N for the next game" else "You win! N for the next game" }
     else w1

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  w1 <- if wPaused w0 then pure w0 else train (if wSpeed w0 == 0 then 0.3 else if wSpeed w0 == 1 then 2 else 7) w0
  -- エージェントの番なら 20 フレームごとに打つ
  pure $ if wResult w1 == 0 && wTurn w1 == agentSide w1 && envFrame env `mod` 20 == 0
    then let ((m, _), g) = runWith (wRng w1) (chooseMove rnd (\k -> pure (wTable w1 U.! k)) id (wBoard w1) (wTurn w1) 0)
         in playAt m w1 { wRng = g }
    else w1

-- budget ミリ秒のあいだ自己対戦を続ける（1000 局ごとにでたらめな相手と 200 局）
train :: Double -> World -> IO World
train budget w = do
  t0 <- getMonotonicTime
  mv <- U.thaw (wTable w)
  szRef <- stToIO (newSTRef (wSize w))
  ref <- stToIO (newSTRef (wRng w))
  let tab = Tab mv szRef ref
      loop !games evs = do
        t <- getMonotonicTime
        if (t - t0) * 1000 >= budget then pure (games, evs) else do
          _ <- stToIO (selfPlayGame tab alphaT epsT)
          let games' = games + 1
          evs' <- if games' `mod` 1000 == 0 then (: evs) <$> stToIO (evaluateT tab 100) else pure evs
          loop games' evs'
  (games, evs) <- loop (wGames w) []
  tbl <- U.freeze mv
  sz <- stToIO (readSTRef szRef)
  g <- stToIO (readSTRef ref)
  let add sel s = foldl' (\acc e -> pushCap 100000 (sel e) acc) s (reverse evs)
  pure w { wTable = tbl, wSize = sz, wGames = games, wRng = g
         , wEvalWin = add (\(a, _, _) -> a) (wEvalWin w), wEvalDraw = add (\(_, b, _) -> b) (wEvalDraw w), wEvalLose = add (\(_, _, c) -> c) (wEvalLose w) }

cellSize :: Env -> Float
cellSize env = min 130 ((envH env - 260) / 3)

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Tic-tac-toe self-play: " ++ show (wGames w) ++ " games   boards learned " ++ show (wSize w)) 40 42 22 (grayC 240)
  , say ("Click play   N new game   T first/second (you play " ++ (if wHumanFirst w then "first" else "second") ++ ")   F speed (" ++ idx "" speedNames (wSpeed w) ++ ")   Space stop   R reset") 40 68 15 (rgbC 170 176 210) ]
  -- ---- 盤（空きマスに、エージェントから見た価値）
  ++ concat [ let x = bx + fromIntegral (i `mod` 3) * cs
                  y = by + fromIntegral (i `div` 3) * cs
                  v = wBoard w U.! i
              in [ Color (rgbC 26 30 60) (rectTL x y cs cs), Color (rgbC 80 86 130) (frameTL 2 x y cs cs) ]
                 ++ (if v == 1 then [ Color cyanC (lineW 8 (x + cs * 0.25, y + cs * 0.25) (x + cs * 0.75, y + cs * 0.75))
                                    , Color cyanC (lineW 8 (x + cs * 0.75, y + cs * 0.25) (x + cs * 0.25, y + cs * 0.75)) ]
                     else if v == 2 then [ Color goldC (ringAt (x + cs / 2) (y + cs / 2) (cs * 0.55) 8) ]
                     else if wResult w == 0 then [ sayM (fmtN 2 (agentValue i)) (x + cs / 2) (y + cs / 2) 15 (rgbC 150 156 200) ]
                     else [])
            | i <- [0 .. 8] ]
  ++ [ say (wMessage w) bx (by + 3 * cs + 34) 17 (grayC 230)
     , sayWrap "Gray numbers = value for the agent if it plays there (its chance to win)" bx (by + 3 * cs + 44) (3 * cs + 40) 14 (rgbC 170 176 210)
  -- ---- でたらめな相手との成績
     , plotAxes g "self-play games (x 1000)" "vs a random player (100 games first + 100 second)" [0, jsRound (x1 g / 2), x1 g] [0, 0.5, 1]
     , drawCurve g (wEvalWin w) limeC 2.5 0, drawCurve g (wEvalDraw w) cyanC 2.5 0, drawCurve g (wEvalLose w) pinkC 2.5 0
     , legend (pX g + 12) (pY g + 16) [(limeC, "win"), (cyanC, "draw"), (pinkC, "lose")] ]
  ++ [ say ("Latest: win " ++ pct (Seq.index (wEvalWin w) (n - 1)) ++ "%   draw " ++ pct (Seq.index (wEvalDraw w) (n - 1)) ++ "%   lose " ++ pct (Seq.index (wEvalLose w) (n - 1)) ++ "%")
           (pX g) (pY g + pH g + 50) 15 (grayC 230) | n > 0 ]
  ++ [ sayWrap "With best play from both sides the game is always a draw (game theory: series 30)" (pX g) (pY g + pH g + 58) (pW g) 14 (rgbC 170 176 210)
     , drawSelfTest env (wTest w) ]
  where
    cs = cellSize env
    bx = 60
    by = 110
    me = agentSide w
    agentValue i = runIdentity (valueWith (\k -> pure (wTable w U.! k)) (setAt (wBoard w) i me) me)
    n = Seq.length (wEvalWin w)
    gx = bx + 3 * cs + 90
    g = G gx 130 (envW env - gx - 50) (envH env - 300) 0 (fromIntegral (max 10 n)) 0 1
    pct v = show (jsRoundI (100 * v))

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        cs = cellSize env
        i = floor ((mx - 60) / cs) :: Int
        j = floor ((my - 110) / cs) :: Int
    in if wResult w /= 0 || wTurn w == agentSide w || i < 0 || j < 0 || i > 2 || j > 2 || wBoard w U.! (j * 3 + i) /= 0 then w
       else playAt (j * 3 + i) w
  EventKey (Char ch) Down _ _
    | ch `elem` "nN" -> newBoard w
    | ch `elem` "tT" -> newBoard w { wHumanFirst = not (wHumanFirst w) }
    | ch `elem` "fF" -> w { wSpeed = (wSpeed w + 1) `mod` 3 }
    | ch `elem` "rR" -> newBoard w { wTable = emptyTable, wSize = 0, wGames = 0, wEvalWin = Seq.empty, wEvalDraw = Seq.empty, wEvalLose = Seq.empty }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 5 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("all 8 lines are detected as a win; a full board with no winner = draw"
    , okL && winnerOf (U.fromList [1, 2, 1, 1, 2, 2, 2, 1, 1]) == 3 && winnerOf emptyBoard == 0)
  , ("after 40000 self-play games, never loses to a random player in 400 games (200 first + 200 second) (losses " ++ show lose ++ ")", lose == 0)
  , ("wins over 90% when playing first (" ++ show (jsRoundI (fromIntegral win1 / 2)) ++ "%)", fromIntegral win1 / 200 > (0.9 :: Double))
  , ("all values are within 0-1 (chance to win)", okV && size > 3000)
  , ("takes a winning move when there is one (X wins at the 3rd cell)", winMove == 2) ]
  where
    okL = and [ winnerOf (U.generate 9 (\i -> if i `elem` [a, b, c] then 2 else 0)) == 2 | (a, b, c) <- winLines ]
    (lose, win1, okV, size, winMove) = runST $ do
      mv <- UM.replicate tableSize (0 / 0)
      sz <- newSTRef 0
      ref <- newSTRef (seedRng 2)
      let t = Tab mv sz ref
      forM_ [1 .. 40000 :: Int] $ \_ -> selfPlayGame t alphaT epsT
      rs <- mapM (\side -> mapM (\_ -> (,) side <$> vsRandom t side) [1 .. 200 :: Int]) [1, 2]
      let rs' = concat rs
      tbl <- U.freeze mv
      n <- readSTRef sz
      (m, _) <- chooseS t (U.fromList [1, 1, 0, 2, 2, 0, 0, 0, 0]) 1 0   -- X の番：右上で勝てる
      pure ( length (filter ((< 0) . snd) rs'), length (filter (\(s, r) -> s == 1 && r > 0) rs')
           , U.all (\v -> isNaN v || (v >= 0 && v <= 1)) tbl, n, m )

-- ---------- やってみよう ----------
-- 1. 学習をはじめる前（R の直後）と、何万局かあとで、エージェントと対戦してくらべよう
-- 2. epsT を 0 にすると？（同じ手ばかりで、知らない盤面に弱くなる）
-- 3. alphaT を 0.5 にすると学習は速くなる？ ゆれる？
-- 4. あなたが先手のとき、エージェントに勝てる？ 勝てたら、そのあと学習を続けるとどうなる？
-- 5. 4 × 4 で 4 つ並べる版にすると、盤面の数はどれくらい増える？

-- ==================================================
-- ここから下は「強化学習の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。乱数は p5.js 版と同じ mulberry32 で、同じ順に引く
-- ==================================================
-- ---- 色と小さな道具
bgC, goldC, cyanC, pinkC, limeC, violetC :: Color
bgC = rgbC 14 16 34                     -- 背景（アーケードの紺）
goldC = rgbC 255 196 60
cyanC = rgbC 90 215 245
pinkC = rgbC 245 110 170
limeC = rgbC 160 230 110
violetC = rgbC 175 140 255

negInf, posInf :: Double
negInf = -1 / 0
posInf = 1 / 0

-- v を a〜b に（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

-- いちばん大きい値の番号（同点はランダムに 1 つ。乱数を 1 つ使う）
argmaxR :: [Double] -> R Int
argmaxR q = case reverse best of
    [] -> pure 0
    bs -> do u <- rnd; pure (idx 0 bs (floor (u * fromIntegral (length bs))))
  where
    (_, best) = foldl' step (negInf, []) (zip [0 ..] q)
    step (m, b) (i, v)
      | v > m + 1e-12 = (v, [i])
      | abs (v - m) <= 1e-12 = (m, i : b)
      | otherwise = (m, b)

-- 同点は若い番号
argmaxFirst :: [Double] -> Int
argmaxFirst q = fst (foldl' (\(b, bv) (i, v) -> if v > bv then (i, v) else (b, bv)) (0, idx 0 q 0) (zip [0 ..] q))

maxOf :: [Double] -> Double
maxOf = foldl' (\m v -> if v > m then v else m) negInf

-- ε の確率でランダム、ほかは貪欲
epsGreedy :: [Double] -> Double -> R Int
epsGreedy q eps = do
  u <- rnd
  if u < eps then (\v -> floor (v * fromIntegral (length q))) <$> rnd else argmaxR q

-- ボルツマン分布 exp(h/τ) / Σ（大きい数でもあふれないように最大値を引く）
softmaxArr :: [Double] -> Double -> [Double]
softmaxArr h tau = map (/ s) e
  where
    m = maxOf h
    t = if tau == 0 || isNaN tau then 1 else tau
    e = map (\v -> exp ((v - m) / t)) h
    s = foldl' (+) 0 e

sampleFrom :: [Double] -> R Int
sampleFrom p = do
  u <- rnd
  let go _ _ [] = length p - 1
      go i c (x : xs) = let c' = c + x in if u < c' then i else go (i + 1) c' xs
  pure (go 0 0 p)

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else foldl' (+) 0 a / fromIntegral (length a)

-- 小数点以下 d 桁（toFixed）。−0.00 は 0.00 に、NaN は ?
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let s = showFFloat (Just d) v ""
                in if all (`elem` "-0.") s && take 1 s == "-" then drop 1 s else s

-- リストの i 番目（なければ d）
idx :: a -> [a] -> Int -> a
idx d xs i = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d
-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態はすぐに計算する（正格）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- 種 s で始めて結果だけ取り出す（seedRng(s) してから計算）
withSeed :: Word32 -> R a -> a
withSeed s m = fst (runR m (seedRng s))

-- 今の乱数の状態で計算して、(結果, 次の状態)
runWith :: Rng -> R a -> (a, Rng)
runWith s m = runR m s

rnd :: R Double
rnd = R (\(Rng s) -> case rndStep (Rng s) of (u, s') -> (u, s'))

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

randn :: R Double                             -- 標準正規分布（ボックス-ミュラー）
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))
-- ---- 書きかえられる表（ST）の中で使う乱数（重い学習用。R と同じ列になる）
type RngRef s = STRef s Rng

rndS :: RngRef s -> ST s Double
rndS ref = do
  s <- readSTRef ref
  let (u, s'@(Rng w)) = rndStep s
  w `seq` writeSTRef ref s'
  pure u

-- R の計算を ST の中で（同じ乱数の状態を続けて使う）
liftR :: RngRef s -> R a -> ST s a
liftR ref m = do
  s <- readSTRef ref
  let (a, s'@(Rng w)) = runR m s
  w `seq` writeSTRef ref s'
  pure a
-- ---- グラフの道具（左上が原点、y は下向き）：G 左 上 幅 高さ x0 x1 y0 y1
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv ((v - x0 g) / (x1 g - x0 g)) (-0.02) 1.02) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv ((v - y0 g) / (y1 g - y0 g)) (-0.02) 1.02) * pH g

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgbC 70 76 110) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 18) 14 (rgbC 150 156 190) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (rgbC 150 156 190) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 38) 14 (rgbC 205 210 235), say yl (pX g) (pY g - 8) 14 (rgbC 205 210 235) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- 直前 k 個の平均（ギザギザの学習曲線をならす）
smoothArr :: Seq.Seq Double -> Int -> Seq.Seq Double
smoothArr a k = Seq.fromList (go 0 0 (toList a))
  where
    arr = a
    go _ _ [] = []
    go !i !s (v : vs) = let s1 = s + v - (if i >= k then Seq.index arr (i - k) else 0)
                        in s1 / fromIntegral (min (i + 1) k) : go (i + 1) s1 vs

-- arr の i 番目を x = i + xOff に。長いときは間引いて 600 点まで（色 col、太さ sw）
drawCurve :: G -> Seq.Seq Double -> Color -> Float -> Double -> Picture
drawCurve g arr col sw xOff = Color col (polyline sw pts)
  where
    n = Seq.length arr
    stride = max 1 ((n + 599) `div` 600)
    pt i = (gX g (fromIntegral i + xOff), gY g (Seq.index arr i))
    pts = [ pt i | i <- [0, stride .. n - 1] ] ++ [ pt (n - 1) | n > 1, (n - 1) `mod` stride /= 0 ]

-- 色の見本と名前（items = [(色, 名前)]）
legend :: Float -> Float -> [(Color, String)] -> Picture
legend x y items = Pictures (concat [ [Color c (rectTL x (yy - 5) 16 10), say s (x + 22) (yy + 5) 14 (rgbC 220 225 245)]
                                    | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * fromIntegral k ])

-- 長さ cap までの列に追加（古いものから捨てる。メモリを増やし続けない）
pushCap :: Int -> Double -> Seq.Seq Double -> Seq.Seq Double
pushCap cap !v s = let s' = s Seq.|> v in if Seq.length s' > cap then Seq.drop 1 s' else s'
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 maxW で折り返す文字（左上 = (x, y)）
sayWrap :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayWrap s x y maxW size c = Pictures [ say l x (y + size + fromIntegral k * size * 1.3) size c | (k, l) <- zip [0 :: Int ..] (wrapText maxW size s) ]

wrapText :: Float -> Float -> String -> [String]
wrapText maxW size s = go (words s) ""
  where
    fits t = textW t size <= maxW
    go [] cur = [cur | not (null cur)]
    go (w : ws) "" = go ws w
    go (w : ws) cur = let t = cur ++ " " ++ w in if fits t then go ws t else cur : go (w : ws) ""

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が (x, y) の長方形の中にあるか
inBox :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
inBox (mx, my) (x, y, w, h) = mx > x && mx < x + w && my > y && my < y + h
-- ---- 数の道具
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt = fmtN

-- 有効数字 n 桁に丸めた数（toPrecision）
roundSig :: Int -> Double -> Double
roundSig n v0'
  | v == 0 = 0
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (n - 1 - e) :: Double
                in fromIntegral (round (v * k) :: Integer) / k
  where v = fin 0 v0'

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v = let r = roundSig 3 v
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (jsRoundI v)

jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e9) 1e9)

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d
-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- NaN も ∞ もない
noNaN :: [Double] -> Bool
noNaN = all (\v -> not (isNaN v || isInfinite v))

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 画面の右下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 225 200 else rgbC 255 90 120)
