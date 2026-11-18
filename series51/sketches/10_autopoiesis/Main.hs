-- ============================================
-- 51-10 オートポイエーシス（ヴァレラ・マトゥラーナ・ウリベ 1974 の人工化学の考え方）
-- 触媒 K のそばで、基質 S が 2 つ結びついてリンク L になる。L どうしがつながって膜になり、膜は自然にこわれる
-- こわれた所には、内側で作られた新しい L が入ってつながる → 「自分を作るものを、自分で作り続ける」まとまり
-- 操作：クリック・ドラッグ = 膜をこわす / ↑↓ = こわれやすさ / K = 触媒を取り除く・戻す / スペース = 一時停止 / R = 最初から
-- ============================================
{-# LANGUAGE BangPatterns, RankNTypes #-}
module Main (main) where

import Art
import Control.Monad (filterM, forM_, unless, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nx, ny, nCell :: Int
nx = 48              -- 格子の大きさ
ny = 32
nCell = nx * ny

tEmpty, tSub, tCat, tLink :: Int
tEmpty = 0           -- 空き
tSub = 1             -- 基質 S
tCat = 2             -- 触媒 K
tLink = 3            -- リンク L

pProd, pBond :: Double
pProd = 0.5          -- ★ 触媒のそばで S + S → L になる確率
pBond = 0.6          -- ★ となりの L とつながる確率

pDecay0 :: Double
pDecay0 = 0.0003     -- ★ L がこわれて S + S にもどる確率（1 回の番ごと）

ringR :: Double
ringR = 3            -- ★ はじめの膜の半径（マス）

events :: Int
events = 2500        -- 1 フレームで何回「番」を回すか

-- 世界：種類 aT と、つながり（各マス最大 2 本。-1 = なし）
data AW = AW { aT, aB0, aB1 :: !(U.Vector Int) }

-- 規則の確率と、触媒がはたらくか
data P = P { pPr, pBo, pDe :: !Double, pCat :: !Bool }

data World = World
  { wW       :: !AW
  , wRng     :: !Word32
  , wDecay   :: !Double
  , wCatOn   :: !Bool
  , wPaused  :: !Bool
  , wClosed  :: !Bool
  , wArea    :: !Int           -- 最後に閉じていたときの内側の広さ
  , wWas     :: !Bool
  , wBreach  :: !Int
  , wRepair  :: !Int
  , wHist    :: [(Double, Double, Double)]   -- (L の数, つながり, 閉じているか)（新しい順、400 個まで）
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "51-10 Autopoiesis" world0)
  { skBack  = col (24, 35, 10) 100
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World aw r pDecay0 True False True 0 True 0 0 [] []
  where (aw, r) = makeWorld 74

-- ---- 世界を作る：基質を 75% まき、半径 RING_R の輪の膜と、まん中に触媒
makeWorld :: Word32 -> (AW, Word32)
makeWorld seed = runST $ do
  m <- newM seed
  forM_ [0 .. nCell - 1] $ \i -> do
    u <- rand m
    MU.write (mT m) i (if u < 0.75 then tSub else tEmpty)
  let cx = nx `div` 2
      cy = ny `div` 2
      pts = [ (y * nx + x) | k <- [0 .. 399 :: Int]
            , let a = (fromIntegral k / 400) * (2 * pi) :: Double
                  x = jsRound (fromIntegral cx + ringR * cos a)
                  y = jsRound (fromIntegral cy + ringR * sin a) ]
      ring = reverse (foldl (\acc i -> case acc of
                                [] -> [i]
                                (l : _) -> if l /= i && last acc /= i then i : acc else acc) [] pts)
  forM_ ring $ \i -> MU.write (mT m) i tLink
  forM_ (zip ring (drop 1 ring ++ take 1 ring)) $ \(a, b) -> bond m a b
  MU.write (mT m) (cy * nx + cx) tCat
  freezeM m

-- ---- 1 回の番：ランダムな 1 マスが、その種類の規則で動く
eventM :: M s -> P -> ST s ()
eventM m p = do
  u1 <- rand m
  let i = floor (u1 * fromIntegral nCell)
  t <- rdT m i
  u2 <- rand m
  let d = dirs !! min 3 (floor (u2 * 4))
      j = atD i d 1
  if t == tSub then                                        -- 基質：空きへ動く。つながった膜はとびこえて通りぬける
    when (j >= 0) $ do
      tj <- rdT m j
      if tj == tEmpty then swapM m i j
      else when (tj == tLink) $ do
        bj <- bondsOf m j
        unless (null bj) $ do
          let k = atD i d 2
          when (k >= 0) $ do
            tk <- rdT m k
            when (tk == tEmpty) (swapM m i k)
  else if t == tCat then do                                -- 触媒：動く（膜には近づかない）＋ 生成 S + S → L
    ss <- filterM (\q -> (== tSub) <$> rdT m q) (nb8 i)
    made <- case ss of
      (s0 : s1 : _) | pCat p -> do
        u <- rand m
        if u < pPr p then MU.write (mT m) s0 tLink >> MU.write (mT m) s1 tEmpty >> pure True else pure False
      _ -> pure False
    unless made $ when (j >= 0) $ do
      tj <- rdT m j
      when (tj == tEmpty || tj == tSub) $ do
        nearMem <- anyM (\q -> do tq <- rdT m q
                                  if tq /= tLink then pure False else not . null <$> bondsOf m q) (nb8 j)
        unless nearMem (swapM m i j)                       -- 膜にはくっつかない
  else when (t == tLink) $ do
    u <- rand m
    if u < pDe p then do                                   -- 崩壊 L → S + S（となりに空きがあるときだけ）
      e <- filterM (\q -> (== tEmpty) <$> rdT m q) (nb8 i)
      case e of
        (e0 : _) -> unbondAll m i >> MU.write (mT m) i tSub >> MU.write (mT m) e0 tSub
        [] -> pure ()
    else do
      nearK <- nearCat m i 2
      bi <- bondsOf m i
      when (length bi < 2 && not nearK) $                  -- 結合：となりの L（つながり 2 本未満）とつながる（触媒の 2 マス以内ではつながらない）
        forM_ (nb8 i) $ \q -> do
          tq <- rdT m q
          when (tq == tLink && q /= i) $ do
            bq <- bondsOf m q
            bi' <- bondsOf m i
            when (length bq < 2 && q `notElem` bi' && length bi' < 2) $ do
              v <- rand m
              when (v < pBo p) (bond m i q)
      bi2 <- bondsOf m i
      when (null bi2 && j >= 0) $ do                       -- つながっていない L は動く
        tj <- rdT m j
        when (tj == tEmpty || tj == tSub) (swapM m i j)

-- ---- 膜は閉じている？：触媒から、つながった L を通らずに外の端まで行けなければ「閉じている」（内側の広さも返す）
isClosedM :: M s -> ST s (Bool, Int)
isClosedM m = do
  starts <- filterM (\i -> (== tCat) <$> rdT m i) [0 .. nCell - 1]
  if null starts then pure (False, 0) else do
    seen <- MU.replicate nCell False
    forM_ starts $ \s -> MU.write seen s True
    let go [] !area = pure (True, area)
        go (i : st) !area = do
          let x = i `mod` nx
              y = i `div` nx
          if x == 0 || y == 0 || x == nx - 1 || y == ny - 1 then pure (False, area + 1) else do
            new <- filterM (\j -> do
                      if j < 0 then pure False else do
                        sj <- MU.read seen j
                        if sj then pure False else do
                          wall <- isWall m j
                          if wall then pure False else MU.write seen j True >> pure True)
                    [ atD i d 1 | d <- dirs ]
            go (reverse new ++ st) (area + 1)
    go (reverse starts) 0

isClosed :: AW -> (Bool, Int)
isClosed aw = runST (thawM aw 0 >>= isClosedM)

isWall :: M s -> Int -> ST s Bool
isWall m j = do
  tj <- rdT m j
  if tj /= tLink then pure False else not . null <$> bondsOf m j

-- 種類ごとの数とつながりの数：(S, K, L, 空き, つながり)
counts :: AW -> (Int, Int, Int, Int, Int)
counts aw = ( cnt tSub, cnt tCat, cnt tLink, cnt tEmpty
            , (U.length (U.filter (>= 0) (aB0 aw)) + U.length (U.filter (>= 0) (aB1 aw))) `div` 2 )
  where cnt k = U.length (U.filter (== k) (aT aw))

-- こわす：つながりを切り、空きがあれば L → S + S（数は保存）
breakAtM :: M s -> Int -> Int -> Double -> ST s ()
breakAtM m gx gy rad = forM_ [0 .. nCell - 1] $ \i -> do
  let x = i `mod` nx
      y = i `div` nx
  t <- rdT m i
  when (fromIntegral ((x - gx) ^ (2 :: Int) + (y - gy) ^ (2 :: Int)) <= rad * rad && t == tLink) $ do
    unbondAll m i
    e <- filterM (\q -> (== tEmpty) <$> rdT m q) (nb8 i)
    case e of
      (e0 : _) -> MU.write (mT m) i tSub >> MU.write (mT m) e0 tSub
      [] -> pure ()

-- 世界を変えずに読み書きする小道具：凍った世界を溶かして ST の中で動かし、また凍らせる
withM :: AW -> Word32 -> (forall s. M s -> ST s a) -> (AW, Word32, a)
withM aw r f = runST $ do
  m <- thawM aw r
  a <- f m
  (aw', r') <- freezeM m
  pure (aw', r', a)

params :: World -> P
params w = P pProd pBond (wDecay w) (wCatOn w)

step :: Env -> Float -> World -> IO World
step env _ w0 = pure w { wClosed = cl, wArea = if cl then area else wArea w, wWas = cl
                       , wBreach = wBreach w + (if wWas w && not cl then 1 else 0)
                       , wRepair = wRepair w + (if not (wWas w) && cl then 1 else 0)
                       , wHist = if envFrame env `mod` 4 == 0 then take 400 ((fromIntegral cL, fromIntegral cB, if cl then 1 else 0) : wHist w) else wHist w }
  where
    w = if wPaused w0 then w0
        else let (aw, r, _) = withM (wW w0) (wRng w0) (\m -> forM_ [1 .. events] (\_ -> eventM m (params w0)))
             in w0 { wW = aw, wRng = r }
    (cl, area) = isClosed (wW w)
    (_, _, cL, _, cB) = counts (wW w)

-- マスの大きさ
cellSize :: Env -> Float
cellSize env = min (envW env * 0.6 / fromIntegral nx) ((envH env - 130) / fromIntegral ny)

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Autopoiesis - break the membrane and it repairs itself from inside" 20 36 20 cTxt
  , say ("Click/drag: break membrane   Up/Down fragility " ++ fmt 4 (wDecay w) ++ "   K catalyst " ++ (if wCatOn w then "working" else "stopped")
         ++ "   Space " ++ (if wPaused w then "resume" else "pause") ++ "   R restart") 20 62 15 cSub
  , Color (hsb 28 25 12) (rectTL gx gy (fromIntegral nx * cs) (fromIntegral ny * cs)) ]
  ++ [ cell i (tv U.! i) | i <- [0 .. nCell - 1], tv U.! i /= tEmpty ]
  ++ [ Color (col cTube 100) (thickLine (max 2 (cs * 0.18)) (cen i) (cen j))
     | (bv) <- [aB0 aw, aB1 aw], i <- [0 .. nCell - 1], let j = bv U.! i, j > i ]
  ++ [ say (if wClosed w then "Membrane closed OK" else "Membrane has a hole") hx hy 20 (if wClosed w then cScope else cAlarm)
     , say (if wClosed w then "(inside " ++ show (wArea w) ++ " cells)" else "(catalyst reaches the outside)") hx (hy + 22) 15 (if wClosed w then cScope else cAlarm)
     , say ("S " ++ show cS ++ "   L " ++ show cL ++ "   bonds " ++ show cB ++ "   K " ++ show cK) hx (hy + 50) 15 cTxt
     , say ("S + 2L = " ++ show (cS + 2 * cL) ++ " (never changes)") hx (hy + 74) 15 cTube
     , say ("holes " ++ show (wBreach w) ++ " -> repaired " ++ show (wRepair w)) hx (hy + 98) 15 cBrass
     , say "red = catalyst K, blue dot = S" hx (hy + 126) 14 cSub
     , say "orange = membrane L, yellow = free L" hx (hy + 146) 14 cSub
     , plotFrame hx pgy pgw pgh "L (orange), bonds (yellow), closed (green)"
     , plotLine (map fst3 hist) hx pgy pgw pgh 0 mxL cTube 400 2
     , plotLine (map snd3 hist) hx pgy pgw pgh 0 mxL cBrass 400 1.5
     , plotLine (map thd3 hist) hx (pgy + pgh - 26) pgw 20 0 1 cScope 400 2
     , drawSelfTest env (wTest w) ]
  where
    aw = wW w
    tv = aT aw
    cs = cellSize env
    gx = 20
    gy = 90
    cen i = (gx + fromIntegral (i `mod` nx) * cs + cs / 2, gy + fromIntegral (i `div` nx) * cs + cs / 2)
    cell i t
      | t == tSub = Color (hsb 200 30 55) (at (cen i) (disc (cs * 0.175)))
      | t == tCat = Color (col cAlarm 100) (at (cen i) (disc (cs * 0.475)))
      | otherwise = Color (col (if aB0 aw U.! i >= 0 then cTube else cBrass) 100) (at (cen i) (rectangleSolid (cs * 0.64) (cs * 0.64)))
    (cS, cK, cL, _, cB) = counts aw
    hx = gx + fromIntegral nx * cs + 30
    hy = 110
    pgy = hy + 176
    pgw = envW env - hx - 30
    pgh = min 220 (envH env - pgy - 60)
    hist = reverse (wHist w)
    mxL = maximum (60 : map fst3 hist) * 1.2
    fst3 (a, _, _) = a
    snd3 (_, b, _) = b
    thd3 (_, _, c) = c

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> hitMembrane env w
  EventMotion _ | envDown env -> hitMembrane env w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wDecay = min 0.01 (wDecay w * 1.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wDecay = max 0.0001 (wDecay w / 1.5) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "kK" -> w { wCatOn = not (wCatOn w) }
    | c `elem` "rR" -> let (aw, r) = makeWorldFrom (wRng w)
                       in w { wW = aw, wRng = r, wHist = [], wBreach = 0, wRepair = 0, wWas = True }
  _ -> w

-- R：今の乱数の続きで新しい世界を作る（p5.js 版と同じ）
makeWorldFrom :: Word32 -> (AW, Word32)
makeWorldFrom = makeWorld

hitMembrane :: Env -> World -> World
hitMembrane env w =
  let (mx, my) = p5Mouse env
      cs = cellSize env
      x = (mx - 20) / cs
      y = (my - 90) / cs
  in if x >= 0 && x < fromIntegral nx && y >= 0 && y < fromIntegral ny
       then let (aw, r, _) = withM (wW w) (wRng w) (\m -> breakAtM m (floor x) (floor y) 1.5) in w { wW = aw, wRng = r }
       else w

-- ---- 自己テスト（p5.js 版と同じ 5 項目）
bondsOk :: AW -> Bool
bondsOk aw = and [ ok i | i <- [0 .. nCell - 1] ]
  where
    bs i = [ b | b <- [aB0 aw U.! i, aB1 aw U.! i], b >= 0 ]
    ok i = let b = bs i
           in (null b || aT aw U.! i == tLink)
              && and [ i `elem` bs j && max (abs (i `mod` nx - j `mod` nx)) (abs (i `div` nx - j `div` nx)) == 1 | j <- b ]

-- 触媒を囲む膜の L を 1 つ見つける（塗りつぶしの境目）
wallCellM :: M s -> ST s Int
wallCellM m = do
  ks <- filterM (\i -> (== tCat) <$> rdT m i) [0 .. nCell - 1]
  let k0 = case ks of { (k : _) -> k; [] -> -1 }
  if k0 < 0 then pure (-1) else do
    seen <- MU.replicate nCell False
    MU.write seen k0 True
    let go [] = pure k0
        go (i : st) = scan [ atD i d 1 | d <- dirs ] st
        scan [] st = go st
        scan (j : js) st
          | j < 0 = scan js st
          | otherwise = do
              sj <- MU.read seen j
              if sj then scan js st else do
                wall <- isWall m j
                if wall then pure j else MU.write seen j True >> scan js (j : st)
    go [k0]

selfTest :: [(String, Bool)]
selfTest =
  [ ("the starting ring membrane is closed, every link has 2 bonds", t1)
  , ("80000 turns: S + 2L = " ++ show (sum0) ++ " stays constant (2S -> L, L -> 2S), one catalyst", mass)
  , ("bonds are always: at most 2, both ways, between neighbours, only on L", inv)
  , ("settled membrane (after 150 frames): break 3x3 of the wall -> hole -> closes itself in "
      ++ show (round (fromIntegral tRep / fromIntegral events :: Double) :: Int) ++ " frames (inside " ++ show areaRep ++ " cells)"
    , before && opened && closedAfter)
  , ("without the catalyst making L the membrane thins and opens (L: working " ++ show la ++ " > stopped " ++ show lb ++ ")", la > lb && not (fst (isClosed wb))) ]
  where
    p = P 0.5 0.6 0.0003 True
    (w5, r5) = makeWorld 5
    (s0, _, l0, _, _) = counts w5
    sum0 = s0 + 2 * l0
    t1 = fst (isClosed w5) && bondsOk w5 && and [ aT w5 U.! i /= tLink || (aB0 w5 U.! i >= 0 && aB1 w5 U.! i >= 0) | i <- [0 .. nCell - 1] ]
    (_, _, (mass, inv)) = withM w5 r5 $ \m -> do
      let loop k !ms !iv
            | k >= 80000 = pure (ms, iv)
            | otherwise = do
                eventM m p
                if k `mod` 5000 == 0 then do
                  (aw, _) <- freezeM m
                  let (cs', ck, cl', _, _) = counts aw
                  loop (k + 1) (ms && cs' + 2 * cl' == sum0 && ck == 1) (iv && bondsOk aw)
                else loop (k + 1) ms iv
      loop (0 :: Int) True True
    (w9, r9) = makeWorld 9
    (_, _, (before, opened, closedAfter, tRep, areaRep)) = withM w9 r9 $ \m -> do
      forM_ [1 .. 150 * events] $ \_ -> eventM m p              -- 150 フレームぶん動かして、落ちついた膜で試す
      (b4, _) <- isClosedM m
      wall <- wallCellM m
      breakAtM m (wall `mod` nx) (wall `div` nx) 1.5
      (op, _) <- isClosedM m
      let loop t | t >= 600 * events = pure t
                 | t `mod` 250 /= 0 = eventM m p >> loop (t + 1)
                 | otherwise = do
                     (c, _) <- isClosedM m
                     if c then pure t else eventM m p >> loop (t + 1)
      tEnd <- loop (0 :: Int)
      (ca, ar) <- isClosedM m
      pure (b4, not op, ca, tEnd, ar)
    pa = p { pDe = 0.003 }
    pb = pa { pCat = False }
    (wa0, _) = makeWorld 3
    (wa, _, _) = withM wa0 4 (\m -> forM_ [1 .. 120000 :: Int] (\_ -> eventM m pa))
    (wb, _, _) = withM wa0 4 (\m -> forM_ [1 .. 120000 :: Int] (\_ -> eventM m pb))
    (_, _, la, _, _) = counts wa
    (_, _, lb, _, _) = counts wb

-- ---------- やってみよう ----------
-- 1. 膜をクリックでこわし、直るまでの様子を見よう。どこから L がやってくる？
-- 2. K で触媒を止めると、膜はどうなる？ これは「死」と呼べる？
-- 3. ↑ でこわれやすさを上げていくと、直すのが追いつかなくなるのはどこ？
-- 4. pBond を小さくすると、膜の形はどう変わる？
-- 5. 「生きている」とは何か。この膜は 07 の亀や 04 のホメオスタットとどこがちがう？ 話し合おう

-- ==================================================
-- ここから下は「サイバネティクスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 人工化学の格子（ST の中で書きかえる）
-- 種類、つながり 2 本ぶん、乱数の状態
data M s = M { mT, mB0, mB1 :: MU.MVector s Int, mR :: MU.MVector s Word32 }

newM :: Word32 -> ST s (M s)
newM seed = do
  t <- MU.replicate nCell tEmpty
  b0 <- MU.replicate nCell (-1)
  b1 <- MU.replicate nCell (-1)
  r <- MU.replicate 1 seed
  pure (M t b0 b1 r)

thawM :: AW -> Word32 -> ST s (M s)
thawM aw seed = M <$> U.thaw (aT aw) <*> U.thaw (aB0 aw) <*> U.thaw (aB1 aw) <*> MU.replicate 1 seed

freezeM :: M s -> ST s (AW, Word32)
freezeM m = do
  aw <- AW <$> U.freeze (mT m) <*> U.freeze (mB0 m) <*> U.freeze (mB1 m)
  r <- MU.read (mR m) 0
  pure (aw, r)

-- 0〜1 の乱数（mulberry32）
rand :: M s -> ST s Double
rand m = do
  a0 <- MU.read (mR m) 0
  let (v, Rng a) = nextR (Rng a0)
  MU.write (mR m) 0 a
  pure v

rdT :: M s -> Int -> ST s Int
rdT m = MU.read (mT m)

-- つながりの相手（つないだ順）
bondsOf :: M s -> Int -> ST s [Int]
bondsOf m i = do
  a <- MU.read (mB0 m) i
  b <- MU.read (mB1 m) i
  pure (filter (>= 0) [a, b])

setBonds :: M s -> Int -> [Int] -> ST s ()
setBonds m i bs = do
  MU.write (mB0 m) i (case bs of { (a : _) -> a; _ -> -1 })
  MU.write (mB1 m) i (case bs of { (_ : b : _) -> b; _ -> -1 })

bond :: M s -> Int -> Int -> ST s ()
bond m a b = do
  ba <- bondsOf m a
  unless (b `elem` ba) $ do
    setBonds m a (ba ++ [b])
    bb <- bondsOf m b
    setBonds m b (bb ++ [a])

unbondAll :: M s -> Int -> ST s ()
unbondAll m a = do
  ba <- bondsOf m a
  forM_ ba $ \b -> bondsOf m b >>= setBonds m b . filter (/= a)
  setBonds m a []

swapM :: M s -> Int -> Int -> ST s ()
swapM m a b = do
  MU.swap (mT m) a b
  ba <- bondsOf m a
  bb <- bondsOf m b
  setBonds m a bb
  setBonds m b ba
  forM_ bb $ \c -> bondsOf m c >>= setBonds m c . map (\v -> if v == b then a else v)
  forM_ ba $ \c -> bondsOf m c >>= setBonds m c . map (\v -> if v == a then b else v)

-- まわりの 8 マス（p5.js 版と同じ順）
nb8 :: Int -> [Int]
nb8 i = [ yy * nx + xx | dy <- [-1, 0, 1], dx <- [-1, 0, 1], dx /= 0 || dy /= 0
        , let xx = x + dx
              yy = y + dy
        , xx >= 0 && xx < nx && yy >= 0 && yy < ny ]
  where x = i `mod` nx
        y = i `div` nx

nearCat :: M s -> Int -> Int -> ST s Bool
nearCat m i r = anyM (\q -> (== tCat) <$> rdT m q)
  [ yy * nx + xx | dy <- [-r .. r], dx <- [-r .. r], let xx = i `mod` nx + dx
                                                         yy = i `div` nx + dy
  , xx >= 0 && xx < nx && yy >= 0 && yy < ny ]

dirs :: [(Int, Int)]
dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

-- i から向き d に k マス先（はみ出したら -1）
atD :: Int -> (Int, Int) -> Int -> Int
atD i (dx, dy) k = let x = i `mod` nx + dx * k
                       y = i `div` nx + dy * k
                   in if x >= 0 && x < nx && y >= 0 && y < ny then y * nx + x else -1

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = pure False
anyM f (x : xs) = do
  b <- f x
  if b then pure True else anyM f xs

-- JavaScript の Math.round（.5 は大きい方へ）
jsRound :: Double -> Int
jsRound v = floor (v + 0.5)

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で
p5Mouse :: Env -> (Float, Float)
p5Mouse env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 色は (色相, 彩度, 明るさ)。透明度は 0〜100
type HSB = (Float, Float, Float)

cTxt, cSub, cScope, cTube, cBrass, cAlarm :: HSB
cTxt = (40, 12, 96)
cSub = (35, 18, 72)
cScope = (135, 70, 92)
cTube = (24, 85, 100)
cBrass = (42, 55, 88)
cAlarm = (0, 80, 100)

col :: HSB -> Float -> Color
col (h, s, b) a = hsba h s b (a / 100)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> HSB -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size (col c 100) s))

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- オシロスコープ風の枠
plotFrame :: Float -> Float -> Float -> Float -> String -> Picture
plotFrame x y w h title = Pictures $
  [ Color (hsba 130 40 6 0.9) (rectTL x y w h) ]
  ++ [ Color (hsba 130 40 40 0.4) (Line [(x, y + h * k / 4), (x + w, y + h * k / 4)]) | k <- [1, 2, 3] ]
  ++ [ Color (hsb 42 55 60) (frameTL 1 x y w h) ]
  ++ [ say title (x + 8) (y + 18) 14 cSub | not (null title) ]

-- arr の値を lo〜hi の範囲で、枠 (x, y, w, h) に折れ線で描く（n = 横に並べる点の数）
plotLine :: [Double] -> Float -> Float -> Float -> Float -> Double -> Double -> HSB -> Int -> Float -> Picture
plotLine arr x y w h lo hi c n wt
  | length arr < 2 = Blank
  | otherwise = Color (col c 100) (thickPath wt [ (x + fromIntegral i / fromIntegral (n - 1) * w, y + h - f2 (clampv 0 1 ((fin lo v - lo) / (hi - lo))) * h)
                                              | (i, v) <- zip [0 :: Int ..] arr ])

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- ---- 決まった種（seed）から毎回同じ乱数列（0〜1）を出す（p5.js 版の makeRng と同じ mulberry32）
newtype Rng = Rng Word32

nextR :: Rng -> (Double, Rng)
nextR (Rng a0) = (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)
  where
    a = a0 + 0x6d2b79f5
    t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
    t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))

-- ---- 自己テストの道具
-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 16) 14 (if all snd rs then cScope else cAlarm)
