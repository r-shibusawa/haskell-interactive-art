-- ============================================
-- 38-10 トロピカル固有値と周期スケジュール（max-plus）
-- 工程 i の k+1 回目の開始時刻 x_i(k+1) = max_j (A_ij + x_j(k))：「前の工程が全部終わってから」= max，「所要時間を足す」= +
--   この max-plus 系はやがて周期運転になり、周期 λ = トロピカル固有値 = グラフの「最大平均サイクル重み」
--   λ は Karp のアルゴリズムで O(n·m) で求まる。いちばん遅いループ（クリティカル・サイクル）が全体の速さを決める
-- 操作：ノードを上下にドラッグ = 所要時間 τ / スペース = 一時停止 / R = リセット / ↑↓ = 再生の速さ
-- ============================================
module Main (main) where

import Art
import Data.List (elemIndex, foldl', intercalate, sortBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

tau0 :: U.Vector Double
tau0 = U.fromList [1.0, 1.2, 1.5, 1.0, 0.8]      -- ★ 各工程の所要時間

moveT :: Double
moveT = 0.5                                      -- ★ 工程から次の工程へ渡す搬送時間

arcs :: [(Int, Int)]
arcs = [(0, 1), (1, 2), (2, 3), (3, 4), (4, 0), (1, 3), (3, 1)]   -- ★ 工程のつながり（j → i：i は j が終わるのを待つ）

n, kMax :: Int
n = 5
kMax = 14                  -- 計算する回数

negInf :: Double
negInf = negate (1 / 0)

data World = World
  { wTaus :: !(U.Vector Double)
  , wPlaying :: !Bool
  , wSpeed :: !Double
  , wNow :: !Double
  , wDrag :: !Int
  , wDragY :: !Float
  , wDragT :: !Double }

world0 :: World
world0 = World tau0 True 1 0 (-1) 0 0

main :: IO ()
main = runSketch (sketch "38-10 Tropical Eigenvalue and Schedules" world0)
  { skBack  = bgCol
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = draw
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

node :: Env -> Int -> Point
node env i =
  let a = 2 * pi * fromIntegral i / fromIntegral n - pi / 2
  in (envW env * 0.22 + 170 * cos a, envH env * 0.42 + 170 * sin a)

-- ★ max-plus 行列：A_ij = τ_j + 搬送（j → i の辺があれば），A_ii = τ_i（同じ工程は前の仕事が終わるまで始められない），他は −∞
matrixA :: U.Vector Double -> U.Vector Double
matrixA ts = U.generate (n * n) f U.// [ (i * n + j, ts U.! j + moveT) | (j, i) <- arcs ]
  where f k = let (i, j) = k `divMod` n in if i == j then ts U.! i else negInf

at2 :: U.Vector Double -> Int -> Int -> Double
at2 a i j = a U.! (i * n + j)

-- x(0), x(1), … 各回の開始時刻（行 = 回，列 = 工程）
iterateX :: U.Vector Double -> V.Vector (U.Vector Double)
iterateX a = V.iterateN (kMax + 1) next (U.replicate n 0)
  where
    next x = U.generate n $ \i ->
      let m = foldl' (\acc j -> max acc (at2 a i j + x U.! j)) negInf [0 .. n - 1]
      in if isInfinite m then x U.! i else m

-- ★ Karp のアルゴリズム：λ = max_v min_k (D_n(v) − D_k(v)) / (n − k)。D_k(v) = ちょうど k 本の辺で v に着く最大重み
karp :: U.Vector Double -> Double
karp a = foldl' max negInf (map vCycle [0 .. n - 1])
  where
    ds = V.iterateN (n + 1) next (U.replicate n 0)   -- 仮の出発点から全ノードへ重み 0 の辺
    next prev = U.generate n $ \v ->
      foldl' (\acc u -> if at2 a v u > negInf && prev U.! u > negInf then max acc (prev U.! u + at2 a v u) else acc) negInf [0 .. n - 1]
    dn = ds V.! n
    vCycle v
      | dn U.! v == negInf = negInf
      | otherwise = foldl' min (1 / 0)
          [ (dn U.! v - dk U.! v) / fromIntegral (n - k) | k <- [0 .. n - 1], let dk = ds V.! k, dk U.! v > negInf ]

-- 全部の単純サイクルを列挙して平均重みを計算（小さいグラフ用。Karp の答え合わせ）
data Cyc = Cyc { cNodes :: [Int], cMean :: !Double }

cycles :: U.Vector Double -> [Cyc]
cycles a = concatMap (\s -> dfs s s [s] 0) [0 .. n - 1]
  where
    dfs :: Int -> Int -> [Int] -> Double -> [Cyc]
    dfs start v path total = concatMap visit [0 .. n - 1]
      where
        visit u
          | at2 a u v == negInf = []             -- v → u
          | u == start = [Cyc path ((total + at2 a u v) / fromIntegral (length path))]
          | u > start && u `notElem` path = dfs start u (path ++ [u]) (total + at2 a u v)
          | otherwise = []

stepWorld :: World -> World
stepWorld w =
  let xs = iterateX (matrixA (wTaus w))
      now0 = if wPlaying w then wNow w + 0.016 * wSpeed w else wNow w
  in w { wNow = if now0 > (xs V.! kMax) U.! 0 + 1 then 0 else now0 }

draw :: Env -> World -> Picture
draw env w = Pictures (
  concatMap arcPic arcs
  ++ concatMap nodePic [0 .. n - 1]
  -- ダイヤ（タイムライン）
  ++ [ box env (tx - 10) (ty - 30) (tw + 20) (rowH * fromIntegral n + 40) (hsba 0 0 100 0.06) ]
  ++ concatMap rowPic [0 .. n - 1]
  ++ [ seg env 2 (hsb 90 90 100) (sx now, ty - 30) (sx now, ty + rowH * fromIntegral n + 10)
     , txt env (min (envW env - 120) (sx now + 6)) (ty - 34) 14 (hsb 90 90 100) ("t = " ++ nfd 1 now) ]
  -- 周期の測定：最後の 6 回の平均間隔（差は 1.5, 2.0, 1.7 … のように「サイクル長」で一周することがあるので平均する）
  ++ [ txt env tx (ty2 + 40) 16 (gray 1) ("Mean interval of proc 0 (last 6) = " ++ nfd 3 per)
     , txt env tx (ty2 + 62) 16 (gray 1) ("Eigenvalue lambda (Karp) = " ++ nfd 3 lam)
     , txt env tx (ty2 + 84) 14 (gray 0.8) ("Recent intervals: " ++ diffs)
     , txt env tx (ty2 + 110) 14 (hsb 330 70 100) ("Critical cycle: " ++ critStr) ]
  ++ zipWith cycRow [0 ..] (take 6 sortedCyc)
  ++ [ txt env 20 30 16 (gray 1) ("Drag nodes up/down: duration tau   Space: " ++ (if wPlaying w then "pause" else "play") ++ "   Up/Down: speed x" ++ nfd 1 (wSpeed w) ++ "   R: reset")
     , txt env 20 52 14 (gray 0.65) ("x_i(k+1) = max_j (A_ij + x_j(k)),  A_ij = tau_j + move " ++ nfd 1 moveT ++ " (edge j -> i),  A_ii = tau_i")
     , txt env 20 72 14 (gray 0.65) "Bars = k-th run of each process. Number in a node = its tau. Soon evenly spaced: period lambda"
     , txt env 20 (envH env - 44) 14 (gray 0.65) "lambda = largest mean cycle weight = (sum of tau on the cycle) / (cycle length)"
     , txt env 20 (envH env - 24) 14 (gray 0.65) "Unless the pink cycle gets faster, the whole system does not get faster" ] )
  where
    ts = wTaus w
    a = matrixA ts
    xs = iterateX a
    lam = karp a
    cyc = cycles a
    crit = foldl' (\b c -> if cMean c > maybe (-1) cMean b then Just c else b) Nothing cyc
    now = wNow w
    x0 k = (xs V.! k) U.! 0
    critLong = case crit of
      Just c | length (cNodes c) > 1 -> cNodes c
      _ -> []
    -- グラフ
    isCritArc (j, i) = i `elem` critLong && j `elem` critLong &&
      case elemIndex j critLong of
        Just p -> take 1 (drop ((p + 1) `mod` length critLong) critLong) == [i]
        Nothing -> False
    arcPic (j, i) =
      let (ux, uy) = node env j
          (vx, vy) = node env i
          ang = atan2 (vy - uy) (vx - ux)
          on = isCritArc (j, i)
          col = if on then hsb 330 80 100 else hsba 0 0 100 0.35
          o = if (i, j) `elem` arcs then 8 else 0     -- 往復の辺は少しずらす
          ox = negate (sin ang) * o
          oy = cos ang * o
      in [ Color col (arrow (if on then 5 else 2)
             (pt env (ux + ox) (uy + oy))
             (pt env (vx - 24 * cos ang + ox) (vy - 24 * sin ang + oy))) ]
    nodePic i =
      let (x, y) = node env i
          ti = ts U.! i
          active = V.any (\r -> now >= r U.! i && now < r U.! i + ti) xs      -- いま稼働中？
          selfCrit = case crit of
            Just c -> cNodes c == [i]
            Nothing -> False
          fillC = hsb 35 (if active then 95 else 25) (if active then 100 else 60)
      in [ if selfCrit then dotP env x y 53 (hsb 330 80 100) else dotP env x y 50 (hsb 0 0 10)
         , dotP env x y 46 fillC
         , txtC env x (y - 9) 14 (gray 0.1) (show i)
         , txtC env x (y + 10) 14 (gray 0.1) (nfd 1 ti) ]
    -- ダイヤの場所
    tx = envW env * 0.46
    ty = 150
    tw = envW env - tx - 40
    rowH = 44
    ty2 = ty + rowH * fromIntegral n
    tmax = x0 kMax + 1
    sx t = tx + realToFrac (t / tmax) * tw
    rowPic i =
      let y = ty + fromIntegral i * rowH
          ti = ts U.! i
      in [ txt env (tx - 76) (y + 5) 14 (gray 0.6) ("Proc " ++ show i)
         , seg env 1 (hsba 0 0 100 0.2) (tx, y) (tx + tw, y) ]
         ++ concat [ let t0 = (xs V.! k) U.! i
                     in box env (sx t0) (y - 12) (max 2 (realToFrac (ti / tmax) * tw)) 24 (hsba 35 80 100 0.7)
                        : [ txt env (sx t0 + 2) (y - 14) 14 (gray 1) (show k) | k >= 1, even k || tw / realToFrac tmax > 26 ]
                   | k <- [0 .. kMax] ]
    per = (x0 kMax - x0 (kMax - 6)) / 6
    diffs = intercalate ", " [ nfd 2 (x0 (kMax - d) - x0 (kMax - d - 1)) | d <- [2, 1, 0] ]
    critStr = case crit of
      Just c@(Cyc (h : _) _) -> intercalate " -> " (map show (cNodes c)) ++ " -> " ++ show h ++ "   mean = " ++ nfd 3 (cMean c)
      _ -> "-"
    sortedCyc = sortBy (comparing (negate . cMean)) cyc
    cycRow :: Float -> Cyc -> Picture
    cycRow idx c = txt env tx (ty2 + 136 + idx * 22) 14 (gray 0.65)
      ("Cycle " ++ intercalate "->" (map show (cNodes c)) ++ ": total " ++ nfd 1 (cMean c * fromIntegral (length (cNodes c)))
       ++ " / length " ++ show (length (cNodes c)) ++ " = " ++ nfd 3 (cMean c))

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseP env
        hit = [i | i <- [0 .. n - 1], dist2 m (node env i) < 26]
    in case reverse hit of
         (i : _) -> w { wDrag = i, wDragY = snd m, wDragT = wTaus w U.! i }
         [] -> w { wDrag = -1 }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 ->
    let v = max 0.2 (min 5 (wDragT w + realToFrac (wDragY w - snd (mouseP env)) * 0.01))
    in w { wTaus = wTaus w U.// [(wDrag w, v)] }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wSpeed = min 8 (wSpeed w * 1.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wSpeed = max 0.25 (wSpeed w / 1.5) }
  EventKey (Char ch) Down _ _ | ch `elem` "rR" -> w { wTaus = tau0, wNow = 0 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. ピンク（クリティカル）でない工程の τ を長くしても周期が変わらない範囲がある。どこまで？
-- 2. 直近の間隔が 1.5, 2.0, 1.7 … と一定でないのに平均は λ。何回で一周する？（クリティカル・サイクルの長さ = cyclicity）
-- 3. 辺を 1 本足して（arcs に (2, 0) など）サイクルを増やすと λ はどうなる？ 必ず増える？ 搬送時間 moveT を 0 にすると、いちばん遅い工程 1 つが周期を決める
-- 4. 電車のダイヤ：工程 = 駅，τ = 区間の走行時間，辺 = 接続。「全線の最小運転間隔」が λ になる（オランダの鉄道で実際に使われた）


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
