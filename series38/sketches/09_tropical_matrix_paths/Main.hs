-- ============================================
-- 38-09 トロピカル行列積 = 最短経路
-- 距離行列 A（辺がなければ ∞）のトロピカル積 (A ⊗ B)_ij = min_k (A_ik + B_kj)
--   A^{⊗k} の (i, j) 成分 = 「k 本以下の辺を使う i → j の最短距離」。k を増やすと ∞ が埋まっていく
--   A^{⊗(n−1)} = すべての最短距離（Floyd–Warshall と同じ答え）
-- 操作：ノードをドラッグ（辺の長さが変わる）/ ↑↓ = k / クリック = 出発点を選ぶ / R = 辺を引き直す
-- ============================================
module Main (main) where

import Art
import Control.Monad (filterM)
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

n :: Int
n = 6                      -- ★ ノード数

inf :: Float
inf = 1 / 0

data World = World
  { wNodes :: !(U.Vector (Float, Float))   -- 左上原点の座標
  , wArcs :: [(Int, Int)]                  -- 有向辺 (u, v)
  , wK :: !Int             -- 行列のべき
  , wSrc :: !Int           -- 出発点
  , wDrag :: !Int }

makeArcs :: IO [(Int, Int)]
makeArcs = do
  shortcuts <- filterM (\_ -> (< 0.5) <$> randomRIO (0, 1 :: Float)) [0 .. n - 1]   -- ★ 近道をいくつか
  pure ([(i, (i + 1) `mod` n) | i <- [0 .. n - 1]]           -- 一周する輪
        ++ [(i, (i + 2) `mod` n) | i <- shortcuts]
        ++ [(3, 0)])

main :: IO ()
main = runSketch (sketch "38-09 Tropical Matrix Powers" (World U.empty [] 1 0 (-1)))
  { skBack  = bgCol
  , skStart = \env -> do
      arcs <- makeArcs
      let node i = let a = tau * fromIntegral i / fromIntegral n - 1.2
                   in (envW env * 0.28 + 200 * cos a, envH env * 0.5 + 200 * sin a)
      pure (World (U.generate n node) arcs 1 0 (-1))
  , skDraw  = draw
  , skEvent = onEvent
  }

-- ★ 辺の長さ → 重み
wgt :: World -> (Int, Int) -> Float
wgt w (u, v) = fromIntegral (floor (dist2 (wNodes w U.! u) (wNodes w U.! v) / 30 + 0.5) :: Int)

-- 行列は長さ n × n のベクトルで持つ
type Mat = U.Vector Float

idx :: Mat -> Int -> Int -> Float
idx m i j = m U.! (i * n + j)

-- ★ 距離行列 A とトロピカル積
matrixA :: World -> Mat
matrixA w = U.accum min start [ (u * n + v, wgt w (u, v)) | (u, v) <- wArcs w ]
  where start = U.generate (n * n) (\k -> if k `div` n == k `mod` n then 0 else inf)

tmul :: Mat -> Mat -> Mat
tmul a b = U.generate (n * n) $ \k ->
  let (i, j) = k `divMod` n
  in foldl' (\m l -> min m (idx a i l + idx b l j)) inf [0 .. n - 1]

-- 出発点 src からの「k 本以下」の最短経路（前の点を覚えておく：ベルマン・フォードの 1 段ずつ）
data Ent = Ent { enD :: !Float, enPrev :: !Int, enL :: !Int }

pathsFrom :: World -> Mat -> V.Vector (V.Vector Ent)
pathsFrom w a = V.fromList (scanl next start [1 .. wK w])
  where
    start = V.generate n (\v -> Ent (if v == wSrc w then 0 else inf) (-1) 0)
    next prev s = V.generate n $ \v ->
      foldl' (\cand u -> let dd = enD (prev V.! u) + idx a u v
                         in if dd < enD cand - 1e-9 then Ent dd u (s - 1) else cand)
             (prev V.! v) [0 .. n - 1]

draw :: Env -> World -> Picture
draw env w = Pictures (
  concatMap arcPic (wArcs w)
  ++ concatMap pathPic [0 .. n - 1]
  ++ concatMap nodePic [0 .. n - 1]
  -- 行列
  ++ [ txt env mx (my - 40) 16 (gray 1) ("A^" ++ show k ++ "  (shortest distance, at most " ++ show k ++ " edges)") ]
  ++ [ txt env (mx + fromIntegral j * cw + cw + 8) (my - 12) 14 (gray 0.6) ("-> " ++ show j) | j <- [0 .. n - 1] ]
  ++ concat [ txt env mx (my + fromIntegral i * cw + cw / 2 + 4) 14 (gray 0.6) (show i ++ " ->") : map (matCell i) [0 .. n - 1] | i <- [0 .. n - 1] ]
  ++ zipWith (\l str -> txt env mx (my + fromIntegral n * cw + 24 + l * 22) 14 (gray 0.65) str) [0 ..]
       [ "Green cell = finite for the first time at this k"
       , "Yellow = became shorter.  Bright row = start " ++ show src
       , "(A (x) A)_ij = min_l (A_il + A_lj):"
       , "the shortest way through some node l"
       , "Ordinary product Sum_l A_il B_lj with"
       , "+ replaced by min, x replaced by +" ]
  ++ [ txt env 20 34 16 (gray 1) ("Up/Down: k = " ++ show k ++ " (max " ++ show (n - 1) ++ ")   Click: start node   Drag: node (edge length changes)   R: new edges")
     , txt env 20 58 14 (gray 0.65) ("Thick green = shortest paths from " ++ show src ++ " with at most " ++ show k ++ " edges. Number by a node = distance (row " ++ show src ++ ").") ] )
  where
    k = wK w
    src = wSrc w
    a = matrixA w
    powers = iterate (`tmul` a) a           -- A, A^2, A^3, …
    powerAt s = case drop (s - 1) powers of
      (m : _) -> m
      [] -> a
    p = powerAt k
    pprev = if k >= 2 then Just (powerAt (k - 1)) else Nothing
    best = pathsFrom w a
    node i = wNodes w U.! i
    dirOf (ux, uy) (vx, vy) = atan2 (vy - uy) (vx - ux)
    -- 辺
    arcPic arc@(ui, vi) =
      let u@(ux, uy) = node ui
          v@(vx, vy) = node vi
          ang = dirOf u v
          tip = pt env (vx - 22 * cos ang) (vy - 22 * sin ang)
      in [ Color (hsba 0 0 100 0.35) (arrow 2 (pt env ux uy) tip)
         , txt env ((ux + vx) / 2 + 8) ((uy + vy) / 2 - 6) 14 (gray 0.8) (show (round (wgt w arc) :: Int)) ]
    -- 最短経路（k 本以下）を太く
    pathPic v
      | enD (best V.! k V.! v) == inf || v == src = []
      | otherwise = walk v k (0 :: Int)
    walk cur lv guard'
      | cur == src || guard' >= 50 = []
      | enPrev e < 0 = []
      | otherwise = seg env 5 (hsba 90 80 100 0.8) (node (enPrev e)) (node cur) : walk (enPrev e) (enL e) (guard' + 1)
      where e = best V.! lv V.! cur
    -- ノード
    nodePic i =
      let (x, y) = node i
          dd = enD (best V.! k V.! i)
      in [ dotO env x y 40 (if i == src then hsb 35 90 100 else hsb 330 60 100)
         , txtC env x y 18 (gray 0.1) (show i)
         , txt env (x + 24) (y - 20) 14 (hsb 90 80 100) (showV dd) ]
    -- 行列の場所
    mx = envW env * 0.56
    my = 150
    cw = 58
    matCell i j =
      let v = idx p i j
          isNew = maybe False (\m -> idx m i j == inf && v /= inf) pprev
          improved = maybe False (\m -> idx m i j /= inf && v < idx m i j) pprev
          col | v == inf = hsb 0 0 25
              | otherwise = hsb (if isNew then 90 else if improved then 55 else 190) 70 (if i == src then 70 else 45)
          cx = mx + fromIntegral j * cw + cw
          cy = my + fromIntegral i * cw
      in Pictures [ box env cx cy (cw - 4) (cw - 4) col
                  , txtC env (cx + (cw - 4) / 2) (cy + (cw - 4) / 2) 16 (gray 1) (showV v) ]

showV :: Float -> String
showV v = if v == inf then "inf" else show (round v :: Int)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseP env
        hit = [i | i <- [0 .. n - 1], dist2 m (wNodes w U.! i) < 22]
    in pure (case reverse hit of
         (i : _) -> w { wDrag = i, wSrc = i }
         [] -> w { wDrag = -1 })
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 ->
    let (mx, my) = mouseP env
        p = (clampF 30 (envW env * 0.5) mx, clampF 90 (envH env - 30) my)
    in pure w { wNodes = wNodes w U.// [(wDrag w, p)] }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wK = min (n - 1) (wK w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wK = max 1 (wK w - 1) }
  EventKey (Char ch) Down _ _ | ch `elem` "rR" -> do
    arcs <- makeArcs
    pure w { wArcs = arcs }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. k = 1, 2, 3 … と上げて、∞ が埋まる順番を見よう。k = n−1 で全部埋まる？（埋まらないノードがあるのはなぜ？）
-- 2. A^{⊗k} = A^{⊗(k+1)} になったら、それ以上は変わらない（最短経路は n−1 本以下）。何本で安定する？
-- 3. ノードを動かして近道を長くすると、行列のどこが変わる？
-- 4. max 流儀（min → max, ∞ → −∞）にすると「最長経路」。閉路があると発散する。確かめよう


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）→ gloss の座標（中央が原点、y は上向き）
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js と同じ座標で返す
mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

-- 中央揃えの文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = let (gx, gy) = pt env x y in labelC gx gy s c str

-- 左上が (x, y) の塗りつぶした長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 線分（太さ wd）。太さ 1 以下は細い線で描く
seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env wd c (x1', y1') (x2', y2')
  | wd <= 1 = Color c (Line [a, b])
  | otherwise = Color c (thickLine wd a b)
  where a = pt env x1' y1'
        b = pt env x2' y2'

-- 塗りつぶした円（直径 d）
dotP :: Env -> Float -> Float -> Float -> Color -> Picture
dotP env x y d c = Color c (at (pt env x y) (disc (d / 2)))

-- 暗いふちのある円（直径 d）
dotO :: Env -> Float -> Float -> Float -> Color -> Picture
dotO env x y d c = Pictures [dotP env x y (d + 2) (hsb 0 0 10), dotP env x y (d - 2) c]

bgCol :: Color
bgCol = hsb 190 55 10

-- 小数 n 桁の文字列（p5.js の nf）
nf :: Int -> Float -> String
nf n' v = noMinusZero (showFFloat (Just n') v "")

nfd :: Int -> Double -> String
nfd n' v = noMinusZero (showFFloat (Just n') v "")

-- "-0.00" は "0.00" と書く
noMinusZero :: String -> String
noMinusZero str = if all (`elem` "-0.") str then dropWhile (== '-') str else str
