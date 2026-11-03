-- ============================================
-- 09 メタヒューリスティクス ─ 焼きなまし法（シミュレーテッド・アニーリング）
-- 2-opt（08）は「良くなる手」しか選ばないので局所最適で止まる。焼きなまし法は、ときどき「悪くなる手」もわざと受け入れる
--   悪くなる量 Δ の手を確率 exp(−Δ / T) で受け入れる。温度 T を少しずつ下げると、最初は大胆に、最後は慎重に探す
-- ↑↓ = 冷やす速さ / ←→ = 最初の温度 / スペース = やり直し / H = 温め直す / G = 温度 0（2-opt と同じ）と比べる / クリック = 町を足す / R = 新しい町
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Numeric (showFFloat)
import Data.Ord (comparing)
import Data.List (sortBy)
import Control.Monad (replicateM)
import Control.Monad.ST (ST, runST)
import Data.Char (toLower)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import System.Random (StdGen, newStdGen, randomR)

itersPerFrame :: Int
itersPerFrame = 3000                   -- ★ 1 フレームに試す手の数

data World = World
  { wN :: !Int                         -- ★ 町の数（R のとき）
  , wCx, wCy :: !(U.Vector Double)     -- 町の位置
  , wTour, wBestTour :: !(U.Vector Int)
  , wT0 :: !Double                     -- ★ 最初の温度
  , wCool :: !Double                   -- ★ 1 回ごとに T にかける数（1 に近いほどゆっくり冷える）
  , wT, wCur, wBest :: !Double
  , wGreedy :: !Bool
  , wLogT, wLogCur, wLogBest :: !(Seq.Seq Double)   -- グラフの記録（500 個まで）
  , wAcc :: !Float                     -- 悪くなる手の受け入れ率
  , wRecent :: ![(Float, Bool)]        -- 最近の「悪くなる手」の Δ と受け入れたか
  , wGen :: !StdGen
  }

mapX, mapY, mapW, mapH :: Float
mapX = 30
mapY = 100
mapW = 1280 * 0.5
mapH = 800 - 130

main :: IO ()
main = runSketch (sketch "30-09 Simulated Annealing" (error "skStart makes the world"))
  { skBack  = hsb 225 30 12
  , skStart = \_ -> newCities =<< (World 80 U.empty U.empty U.empty U.empty 200 0.999997 0 0 0 False Seq.empty Seq.empty Seq.empty 0 [] <$> newStdGen)
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

newCities :: World -> IO World
newCities w = do
  xs <- replicateM (wN w) (randomRIO (20, mapW - 20))
  ys <- replicateM (wN w) (randomRIO (20, mapH - 20))
  g <- newStdGen
  restart w { wCx = U.fromList (map (realToFrac . (+ mapX)) xs), wCy = U.fromList (map (realToFrac . (+ mapY)) ys), wGen = g }

restart :: World -> IO World
restart w = do
  s <- shuffleIO [0 .. U.length (wCx w) - 1]
  let t = U.fromList s
      l = tourLen w t
  pure w { wTour = t, wBestTour = t, wCur = l, wBest = l, wT = if wGreedy w then 0 else wT0 w
         , wLogT = Seq.empty, wLogCur = Seq.empty, wLogBest = Seq.empty, wRecent = [], wAcc = 0 }

dd :: U.Vector Double -> U.Vector Double -> Int -> Int -> Double
dd xs ys a b =
  let dx = U.unsafeIndex xs a - U.unsafeIndex xs b
      dy = U.unsafeIndex ys a - U.unsafeIndex ys b
  in sqrt (dx * dx + dy * dy)
{-# INLINE dd #-}

tourLen :: World -> U.Vector Int -> Double
tourLen w t
  | U.null t = 0
  | otherwise = U.sum (U.zipWith (dd (wCx w) (wCy w)) t (U.snoc (U.drop 1 t) (U.head t)))

-- 焼きなましの途中の状態
data SA = SA
  { saGen :: !StdGen
  , saT, saCur, saBest :: !Double
  , saBestTour :: !(U.Vector Int)
  , saUphill :: !Int
  , saRecent :: ![(Float, Bool)]
  , saNRecent :: !Int
  }

-- ★ 焼きなまし法の 1 手：ランダムな 2-opt の手を試す
annealStep :: U.Vector Double -> U.Vector Double -> M.MVector s Int -> SA -> ST s SA
annealStep xs ys tour s@SA{..} = do
  let n = M.length tour
      (i0, g1) = randomR (0, n - 1) saGen
      (j0, g2) = randomR (0, n - 1) g1
      i = min i0 j0
      j = max i0 j0
  if j - i < 2 || (i == 0 && j == n - 1) then pure s { saGen = g2 } else do
    a <- M.unsafeRead tour i
    b <- M.unsafeRead tour (i + 1)
    c <- M.unsafeRead tour j
    e <- M.unsafeRead tour ((j + 1) `mod` n)
    let d = dd xs ys
        delta = d a c + d b e - d a b - d c e
        (u, g3) = randomR (0, 1 :: Double) g2
        accept = delta < 0 || (saT > 1e-9 && u < exp (negate delta / saT))   -- ★ 受け入れの確率
        (u2, g4) = randomR (0, 1 :: Double) g3
        keep = delta > 0 && saNRecent < 400 && u2 < 0.05
        recent = if keep then (realToFrac delta, accept) : saRecent else saRecent
        nRecent = if keep then saNRecent + 1 else saNRecent
    if not accept
      then pure s { saGen = g4, saRecent = recent, saNRecent = nRecent }
      else do
        reverseRange tour (i + 1) j
        let cur = saCur + delta
            up = if delta > 0 then saUphill + 1 else saUphill
        if cur < saBest - 1e-9
          then do
            bt <- U.freeze tour
            pure s { saGen = g4, saCur = cur, saBest = cur, saBestTour = bt, saUphill = up, saRecent = recent, saNRecent = nRecent }
          else pure s { saGen = g4, saCur = cur, saUphill = up, saRecent = recent, saNRecent = nRecent }

-- p 番目から q 番目までを逆向きにする
reverseRange :: M.MVector s Int -> Int -> Int -> ST s ()
reverseRange v !p !q
  | p >= q = pure ()
  | otherwise = M.unsafeSwap v p q >> reverseRange v (p + 1) (q - 1)

-- itersPerFrame 手をまとめて試す
runFrame :: World -> (U.Vector Int, SA)
runFrame World{..} = runST $ do
  mv <- U.thaw wTour
  let loop 0 s = pure s
      loop k s = do
        s1 <- annealStep wCx wCy mv s
        loop (k - 1) s1 { saT = saT s1 * wCool }
  r <- loop (itersPerFrame :: Int) (SA wGen wT wCur wBest wBestTour 0 wRecent (length wRecent))
  t <- U.freeze mv
  pure (t, r)

-- 1 フレーム分を進める
stepWorld :: World -> World
stepWorld w
  | U.length (wCx w) < 5 = w
  | otherwise =
      let (tour, r) = runFrame w
          tNow = if saT r < 0.01 then 0 else saT r
          push v q = let q1 = q Seq.|> v in if Seq.length q1 > 500 then Seq.drop 1 q1 else q1
      in w { wTour = tour, wBestTour = saBestTour r, wGen = saGen r, wT = tNow, wCur = saCur r, wBest = saBest r
           , wLogT = push tNow (wLogT w), wLogCur = push (saCur r) (wLogCur w), wLogBest = push (saBest r) (wLogBest w)
           , wAcc = fromIntegral (saUphill r) / fromIntegral itersPerFrame
           , wRecent = take 300 (saRecent r) }

drawAll :: Env -> World -> Picture
drawAll env w@World{..} = screen env $ Pictures [mapPic, plots, acceptPic, hud]
  where
    wd = envW env
    ht = envH env
    px i = (fl (wCx U.! i), fl (wCy U.! i))

    -- 地図：今の道（薄い）と最良の道
    mapPic = Pictures $
      [ boxL 1 (hsba 0 0 100 0.15) mapX mapY mapW mapH
      , loopPath 1.5 (hsba 30 70 100 0.35) (map px (U.toList wTour))
      , loopPath 2.5 (hsb 50 90 100) (map px (U.toList wBestTour))
      ]
      ++ [dot white (fl x) (fl y) 8 | (x, y) <- zip (U.toList wCx) (U.toList wCy)]

    -- 右：グラフ
    gx = mapX + mapW + 50
    gw = wd - gx - 30
    gh = (ht - 200) / 3 - 30
    y2 = 110 + gh + 50
    hiV = maximum (wCur : toList wLogCur)
    loV = minimum (wBest : toList wLogBest) * 0.95
    plots = Pictures
      [ plotSeries (map fl (toList wLogT)) gx 110 gw gh (fl wT0) (hsb 0 75 100) ("Temperature T = " ++ nf 2 wT)
      , plotSeries (map (fl . subtract loV) (toList wLogCur)) gx y2 gw gh (fl (hiV - loV)) (hsb 30 70 100) ("Length now " ++ nf 0 wCur)
      , plotSeries (map (fl . subtract loV) (toList wLogBest)) gx y2 gw gh (fl (hiV - loV)) (hsb 50 90 100) ""
      , txt (gx + 200) (y2 - 12) 14 (hsb 50 90 100) ("Best length " ++ nf 0 wBest)
      ]

    -- 受け入れ確率 exp(−Δ/T) の曲線と、最近の「悪くなる手」
    ay = 110 + 2 * (gh + 50)
    dMax = 200 :: Float
    tF = fl wT
    acceptPic = Pictures $
      [ seg 1 (hsba 0 0 100 0.25) (gx, ay + gh) (gx + gw, ay + gh)
      , seg 1 (hsba 0 0 100 0.25) (gx, ay) (gx, ay + gh)
      , path 2 (hsb 190 80 100)
          [ (gx + x / dMax * gw, ay + gh - (if tF > 1e-9 then exp (negate x / tF) else 0) * gh)
          | k <- [0 .. 100 :: Int], let x = fromIntegral k * 2 ]
      ]
      ++ [ dot (if ok then hsba 140 80 100 0.7 else hsba 0 80 100 0.5) (gx + min d dMax / dMax * gw) (ay + gh - (if ok then 0.92 else 0.04) * gh) 5
         | (d, ok) <- wRecent ]
      ++ [ txt gx (ay - 10) 14 (gr 85) "D -> exp(-D/T)   green: accepted, red: refused"
         , txt gx (ay + gh + 16) 14 (gr 60) "D = 0"
         , txtR (gx + gw) (ay + gh + 16) 14 (gr 60) ("D = " ++ nf 0 dMax) ]

    hud = Pictures
      [ txt 20 40 16 white ("U/D cooling " ++ show wCool ++ "  L/R T0 " ++ nf 1 wT0 ++ "  Space: restart  H: reheat  G: T=0 " ++ (if wGreedy then "ON" else "off") ++ "  Click: city  R: new")
      , txt 20 68 14 (gr 60) ("Uphill accepted " ++ nf 2 (100 * wAcc) ++ "%   Hot: wander; cooling: settle on good tours. Yellow = best so far.")
      ]

-- 折れ線のグラフ
plotSeries :: [Float] -> Float -> Float -> Float -> Float -> Float -> Color -> String -> Picture
plotSeries arr x y w h vmax col title = Pictures
  [ seg 1 (hsba 0 0 100 0.25) (x, y + h) (x + w, y + h)
  , seg 1 (hsba 0 0 100 0.25) (x, y) (x, y + h)
  , path 2 col [ (x + fromIntegral k / 499 * w, y + h - clampF 0 1 (v / max 1e-9 vmax) * h) | (k, v) <- zip [0 :: Int ..] arr ]
  , if null title then Blank else txt x (y - 12) 14 col title
  ]

-- キーとマウス
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
    in pure $ if mx > mapX && mx < mapX + mapW && my > mapY && my < mapY + mapH
         then addCity mx my w
         else w
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wCool = min 0.9999995 (round8 (wCool w + (1 - wCool w) * 0.5)) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wCool = max 0.9999 (round8 (1 - (1 - wCool w) * 2)) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wT0 = min 2000 (wT0 w * 2) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wT0 = max 5 (wT0 w / 2) }
  EventKey (SpecialKey KeySpace) Down _ _ -> restart w
  EventKey (Char c) Down _ _ -> case toLower c of
    'h' -> pure w { wT = wT0 w * 0.3 }
    'g' -> restart w { wGreedy = not (wGreedy w) }
    'r' -> newCities w
    _ -> pure w
  _ -> pure w

addCity :: Float -> Float -> World -> World
addCity mx my w =
  let t = U.snoc (wTour w) (U.length (wCx w))
      w1 = w { wCx = U.snoc (wCx w) (realToFrac mx), wCy = U.snoc (wCy w) (realToFrac my), wTour = t, wBestTour = t }
      l = tourLen w1 t
  in w1 { wCur = l, wBest = l }

round8 :: Double -> Double
round8 v = fromIntegral (round (v * 1e8) :: Int) / 1e8

-- ---------- やってみよう ----------
-- 1. G（温度 0）と普通の焼きなましで、最後の「最良の長さ」を比べよう。何 % 違う？
-- 2. 冷やす速さ（↓）を速くすると（0.9999）すぐ固まる。ゆっくりだと良いが時間がかかる。ちょうどよい速さは？
-- 3. 温度が高すぎる・低すぎるとき、右下の緑と赤の点はどう分かれる？
-- 4. 手を「2-opt」から「2 つの町を入れかえる」に変えると、成績はどう変わる？（annealStep を書きかえる）


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標で描くための変換
screen :: Env -> Picture -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2)

-- マウスの位置（左上が原点、y は下向き）
mouseP :: Env -> Point
mouseP = toP5 <*> envMouse

-- gloss の座標 → 左上が原点の座標
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字：(x, y) は左端のベースライン
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label x (negate y)

-- 文字：右端を x にそろえる
txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtR x y size = \c s -> label (x - fromIntegral (length s) * size * 0.72) (negate y) size c s

-- 枠だけの長方形（線の太さ t）
boxL :: Float -> Color -> Float -> Float -> Float -> Float -> Picture
boxL t c x y w h = loopPath t c [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- 線分（太さ t）
seg :: Float -> Color -> Point -> Point -> Picture
seg t c (x1, y1) (x2, y2) = Color c (thickLine t (x1, negate y1) (x2, negate y2))

-- 折れ線（太さ t）
path :: Float -> Color -> [Point] -> Picture
path t c ps = Color c (thickPath t [(x, negate y) | (x, y) <- ps])

-- 閉じた折れ線（太さ t）
loopPath :: Float -> Color -> [Point] -> Picture
loopPath t c ps = path t c (ps ++ take 1 ps)

-- 塗りつぶした円：中心 (x, y)、直径 d
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (at (x, negate y) (disc (d / 2)))

-- 灰色（明るさ 0〜100）
gr :: Float -> Color
gr = hsb 0 0

-- 小数点以下 d 桁の文字列（p5.js の nf）
nf :: RealFloat a => Int -> a -> String
nf d x
  | d <= 0 = show (round x :: Int)
  | otherwise = showFFloat (Just d) x ""

-- Double → Float（計算は Double、描くのは Float）
fl :: Double -> Float
fl = realToFrac

-- 並びをランダムに混ぜる（p5.js の shuffle）
shuffleIO :: [a] -> IO [a]
shuffleIO xs = do
  keys <- mapM (const (randomRIO (0, 1 :: Float))) xs
  pure (map snd (sortBy (comparing fst) (zip keys xs)))

