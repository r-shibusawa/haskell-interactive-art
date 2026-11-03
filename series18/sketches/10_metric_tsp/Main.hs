-- ============================================
-- 10 三角不等式つき TSP の 2 倍近似（最小全域木をなぞって近道）
-- (1) 最小全域木（MST）をつくる → (2) 木を深さ優先でなぞる（各辺を 2 回通る）→ (3) 一度行った都市は飛ばして近道
-- 三角不等式（寄り道しないほうが短い）が成り立てば：近道の巡回路 ≤ 木を 2 周 = 2 × MST ≤ 2 × 最適
-- N：距離を「距離の 2 乗」に変える（三角不等式がこわれる）→ 保証が消える。一般の TSP は近似すら NP 困難
-- スペース：次の段階へ / クリック：都市を追加 / R：ランダム / C：消す / N：距離の測り方を切り替え
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (elemIndex)
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

exactLimit :: Int
exactLimit = 9      -- ★ この数までは総当たりで本当の最適解を求める

data World = World
  { wCities  :: [Point]
  , wStage   :: !Int           -- 0 都市 1 MST 2 なぞる 3 近道 4 比較
  , wMst     :: [(Int, Int)]
  , wWalk    :: [Int]
  , wTour    :: [Int]
  , wBest    :: [Int]          -- 遅延評価：④ を見るまで計算されない
  , wBestLen :: Double
  , wAnim    :: !Float
  , wSquared :: !Bool
  , wDist    :: !(U.Vector Double)
  }

main :: IO ()
main = runSketch (sketch "18-10 Metric TSP 2-Approximation" emptyWorld)
  { skBack  = hsb 260 30 9
  , skStart = \env -> randomCities env 8 emptyWorld
  , skStep  = \_ _ w -> pure w { wAnim = wAnim w + 0.04 }
  , skDraw  = drawW
  , skEvent = onEvent
  }

emptyWorld :: World
emptyWorld = World [] 0 [] [] [] [] 0 0 False U.empty

randomCities :: Env -> Int -> World -> IO World
randomCities env n w = do
  cs <- replicateM n $ do
    x <- randomRIO (100, envW env - 100)
    y <- randomRIO (160, envH env - 90)
    pure (x, y)
  pure (recompute w { wCities = cs })

-- 都市の間の距離の表を作る（2-opt の丸め誤差で止まらなくならないよう Double で）
distTable :: Bool -> [Point] -> U.Vector Double
distTable squared cs = U.fromList
  [ let e = realToFrac (dist2 a b) / 100 in if squared then e * e else e | a <- cs, b <- cs ]

lenOf :: U.Vector Double -> Int -> [Int] -> Double
lenOf dt n t = sum (zipWith (\a b -> dt U.! (a * n + b)) t (drop 1 t ++ take 1 t))

recompute :: World -> World
recompute w =
  let cs = wCities w
      n = length cs
      dt = distTable (wSquared w) cs
      d a b = dt U.! (a * n + b)
      w0 = w { wStage = 0, wAnim = 0, wMst = [], wWalk = [], wTour = [], wBest = [], wDist = dt }
      mst = prim d n
      kids = foldl' (\m (p, c) -> IM.insertWith (flip (++)) p [c] m) IM.empty mst
      childrenOf v = IM.findWithDefault [] v kids
      -- 深さ優先でなぞる（行き帰りの全部）と、先行順の巡回路
      walkFrom v = v : concatMap (\c -> walkFrom c ++ [v]) (childrenOf v)
      tourFrom v = v : concatMap tourFrom (childrenOf v)
      tour = tourFrom 0
      best = if n <= exactLimit then bruteBest d n else twoOpt d n tour
  in if n < 3
       then w0
       else w0 { wMst = mst, wWalk = walkFrom 0, wTour = tour, wBest = best, wBestLen = lenOf dt n best }

-- プリム法
prim :: (Int -> Int -> Double) -> Int -> [(Int, Int)]
prim d n = go [(i, d 0 i, 0) | i <- [1 .. n - 1]]
  where
    go [] = []
    go rest =
      let (u, _, p) = firstMin rest
          rest' = [ if d u i < k then (i, d u i, u) else (i, k, q) | (i, k, q) <- rest, i /= u ]
      in (p, u) : go rest'
    firstMin = foldl1 (\a@(_, ka, _) b@(_, kb, _) -> if kb < ka then b else a)

-- すべての巡回路を調べて最短を返す（都市 0 を固定、(n−1)! 通り）
bruteBest :: (Int -> Int -> Double) -> Int -> [Int]
bruteBest d n = reverse (snd (go 0 [0] [1 .. n - 1] (1 / 0, [0])))
  where
    go :: Double -> [Int] -> [Int] -> (Double, [Int]) -> (Double, [Int])
    go !len path@(cur : _) [] best =
      let total = len + d cur 0
      in if total < fst best then (total, path) else best
    go !len path@(cur : _) rest best =
      foldl' (\b x -> go (len + d cur x) (x : path) (filter (/= x) rest) b) best rest
    go _ [] _ best = best

-- 都市が多いときは 2-opt で「かなり良い」巡回路
twoOpt :: (Int -> Int -> Double) -> Int -> [Int] -> [Int]
twoOpt d n start = U.toList (loop (1000 :: Int) (U.fromList start))
  where
    loop 0 t = t
    loop k t =
      let (t', improved) = foldl' tryPair (t, False) [(i, j) | i <- [0 .. n - 2], j <- [i + 2 .. n - 1]]
      in if improved then loop (k - 1) t' else t'
    tryPair (t, imp) (i, j) =
      let a = t U.! i
          b = t U.! (i + 1)
          c = t U.! j
          e = t U.! ((j + 1) `mod` n)
      in if d a c + d b e < d a b + d c e - 1e-9
           then (U.concat [U.take (i + 1) t, U.reverse (U.slice (i + 1) (j - i) t), U.drop (j + 1) t], True)
           else (t, imp)

drawW :: Env -> World -> Picture
drawW env w =
  let cs = wCities w
      n = length cs
      stage = wStage w
      anim = wAnim w
      posC i = case drop i cs of
        (p : _) -> p
        [] -> (0, 0)
      closed t = map (pt env . posC) (t ++ take 1 t)
      bestPic
        | stage >= 4 && not (null (wBest w)) =
            Color (hsba 280 60 100 0.7) (thickPath 10 (closed (wBest w)))
        | otherwise = Blank
      mstCount = if stage == 1 then min (length (wMst w)) (floor (anim * 3)) else length (wMst w)
      mstPic
        | stage >= 1 =
            [ seg env (hsb 150 70 90) (if stage == 1 then 5 else 3) (posC a) (posC b) | (a, b) <- take mstCount (wMst w) ]
        | otherwise = []
      walkPic
        | stage == 2 =
            let m = min (length (wWalk w) - 1) (floor (anim * 4))
                piece (ia, ib) =
                  let a@(ax, ay) = posC ia
                      b@(bx, by) = posC ib
                      ang = atan2 (by - ay) (bx - ax) + pi / 2
                      ox = cos ang * 6
                      oy = sin ang * 6
                  in seg env (hsb 45 90 100) 3 (ax + ox, ay + oy) (bx + ox, by + oy)
            in map piece (take m (zip (wWalk w) (drop 1 (wWalk w))))
        | otherwise = []
      tourPic
        | stage >= 3 =
            let tour = wTour w
                len = length tour
                m = if stage == 3 then min len (floor (anim * 3) + 1) else len
                pairs = zip tour (drop 1 tour ++ take 1 tour)
            in [ seg env (hsb 45 90 100) 4 (posC a) (posC b)
               | (k, (a, b)) <- zip [0 ..] (take m pairs), k < m - 1 || m == len ]
        | otherwise = []
      city (i, p@(x, y)) = Pictures
        [ dot env (if i == 0 then hsb 0 80 100 else hsb 200 60 100) p 24
        , if stage >= 3
            then txtC env x y 14 (hsb 0 0 10) (show (maybe 0 (+ 1) (elemIndex i (wTour w))))
            else Blank
        ]
      names =
        [ "Cities"
        , "Step 1: minimum spanning tree (Prim)"
        , "Step 2: walk around the tree (each edge twice)"
        , "Step 3: skip visited cities (shortcut)"
        , "Step 4: compare with the optimum"
        ]
      dt = wDist w
      d a b = dt U.! (a * n + b)
      mstL = sum [d a b | (a, b) <- wMst w]
      tourL = lenOf dt n (wTour w)
      lines3 =
        [ "MST = " ++ fix 2 mstL
        , "Walk = 2 x MST = " ++ fix 2 (2 * mstL)
        , "Shortcut tour H = " ++ fix 2 tourL
        ]
      stats
        | n >= 3 =
            [ txt env 20 (106 + fromIntegral k * 26) 18 (hsb 0 0 85) s
            | (k, s) <- zip [0 :: Int ..] (take (min stage 3) lines3) ]
            ++ (if stage >= 4
                  then
                    [ txt env 20 184 18 (hsb 280 60 100)
                        ((if n <= exactLimit then "Optimum H* = " else "2-opt improved tour = ") ++ fix 2 (wBestLen w)
                          ++ "    H / H* = " ++ fix 2 (tourL / max 1e-9 (wBestLen w)))
                    , txt env 20 212 18 (if wSquared w then hsb 0 80 100 else hsb 120 70 100)
                        (if wSquared w
                           then "No triangle inequality: no 2x guarantee (can exceed 2)"
                           else "H <= 2 x MST <= 2 x H*: always at most 2")
                    ]
                  else [])
        | otherwise = []
  in Pictures
       ( bestPic
       : mstPic ++ walkPic ++ tourPic
      ++ map city (zip [0 :: Int ..] cs)
      ++ stats
      ++ [ txt env 20 34 15 (hsb 0 0 100)
             ("Space: next step   Click: city   R: random   C: clear   N: metric (now "
               ++ (if wSquared w then "squared, triangle ineq. NO)" else "normal, triangle ineq. OK)"))
         , txt env 20 72 24 (hsb 45 85 100) (concat (take 1 (drop stage names)))
         , txt env 20 (envH env - 22) 15 (hsb 0 0 60) "MST is shorter than a tour minus one edge, so MST <= optimum. Shortcuts only shorten (triangle ineq.)"
         ] )

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let m@(_, my) = mouseP5 env p
    in pure $ if my > 230 && length (wCities w) < 14
         then recompute w { wCities = wCities w ++ [m] }
         else w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wStage = (wStage w + 1) `mod` 5, wAnim = 0 }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> randomCities env 8 w
    | c `elem` "cC" -> pure (recompute w { wCities = [] })
    | c `elem` "nN" -> pure (recompute w { wSquared = not (wSquared w) }) { wStage = wStage w }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. R を何度も押して H/H* を記録。2 に近づくことはある？
-- 2. N で距離の 2 乗にして、都市を一直線上に並べてみよう。比はどうなる？
-- 3. 本物の改良版（クリストフィデス法）は 1.5 倍を保証する。どこを変えると思う？

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
-- 小数点以下 d 桁で表示（p5.js の nf）
fix :: RealFloat a => Int -> a -> String
fix d v = showFFloat (Just d) v ""
