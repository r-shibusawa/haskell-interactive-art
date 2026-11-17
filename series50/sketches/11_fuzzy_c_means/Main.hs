-- ============================================
-- 50-11 ファジィ c-平均法（FCM）
-- 点を c 個のグループに分ける。k-平均法は「どれか 1 つ」に入れるが、FCM は「A に 0.7、B に 0.3」のように所属度で分ける
--   u_ik = 1 ÷ Σ_j (d_ik / d_jk)^(2/(m−1))、中心 v_i = Σ u_ik^m x_k ÷ Σ u_ik^m、目的関数 J = Σ u_ik^m d_ik²（くり返すたびに減る）
-- 操作：クリック = 点を追加 / スペース = 1 回更新 / A = 自動 / ←→ = グループ数 c / ↑↓ = ファジィ化係数 m
--       K = k-平均法と切りかえ / N = 初期化 / R = 新しいデータ
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', sortOn)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import System.Environment (lookupEnv)
import Numeric (showFFloat)

type Pt = (Double, Double)

-- グループの色（RGB）
groupCols :: [(Double, Double, Double)]
groupCols = [(255, 170, 130), (70, 200, 190), (180, 160, 255), (250, 230, 120), (240, 120, 170), (140, 220, 120)]

bgW, bgH :: Int
bgW = 64                -- 背景（所属度の地図）の画素数
bgH = 40

data World = World
  { wNC     :: !Int        -- ★ グループの数 c（最初は 3）
  , wM      :: !Double     -- ★ ファジィ化係数 m（1 に近いほど k-平均法に近い、大きいほどぼやける。最初は 2）
  , wKmeans :: !Bool       -- k-平均法か
  , wAuto   :: !Bool       -- 自動で更新するか
  , wP      :: [Pt]        -- 点（0〜1 の座標）
  , wU      :: [[Double]]  -- wU !! i !! k = 点 k のグループ i への所属度
  , wV      :: [Pt]        -- 中心
  , wJ      :: [Double]    -- J の履歴（最大 60 個）
  , wConv   :: !Bool       -- 収束したか
  , wTimer  :: !Int
  , wSeed   :: !Int        -- 次に使う乱数の種
  , wBg     :: U.Vector Word8   -- 背景の色（RGB × 画素）
  , wTest   :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "50-11 Fuzzy c-means" world0)
  { skBack  = hsb 250 18 14
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (newData world0) { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 3 2.0 False True [] [] [] [] False 0 7 U.empty []

-- 3 つのかたまりの点を 18 個ずつ作る
newData :: World -> World
newData w = initU w { wP = pts, wSeed = wSeed w + 1 }
  where
    r = lcg (wSeed w)
    cs = [(0.25, 0.3), (0.7, 0.35), (0.5, 0.75)] :: [Pt]
    pts = go cs r
    go [] _ = []
    go ((cx, cy) : rest) rs =
      let (here, rs') = takePts (18 :: Int) rs
          takePts 0 xs = ([], xs)
          takePts n (a : b : xs) = let (ps, ys) = takePts (n - 1) xs in ((cx + (a - 0.5) * 0.28, cy + (b - 0.5) * 0.28) : ps, ys)
          takePts _ xs = ([], xs)
      in here ++ go rest rs'

-- 所属度をでたらめに決めて（列の和 = 1）、そこから始める
initU :: World -> World
initU w = iter w { wU = u, wV = v, wJ = [], wConv = False, wSeed = wSeed w + 1 }
  where
    r = lcg (wSeed w)
    ps = wP w
    n = length ps
    nc = wNC w
    raw = [ [0.1 + x | x <- take n (drop (i * n) r)] | i <- [0 .. nc - 1] ]
    sums = foldl' (zipWith (+)) (replicate n 0) raw
    u = [ zipWith (/) row sums | row <- raw ]
    rest = drop (nc * n) r
    v | wKmeans w = if n == 0 then [] else [ getAt (0, 0) ps (floor (x * fromIntegral n)) | x <- take nc rest ]
      | otherwise = fcmV ps u (wM w)

-- 1 回分：中心 → 所属度 → J
iter :: World -> World
iter w
  | length ps < wNC w = w
  | otherwise = makeBg w { wU = u', wV = v', wJ = js, wConv = conv }
  where
    ps = wP w
    (u', v')
      | wKmeans w = let u1 = hardU ps (wV w); v1 = fcmV ps u1 1 in (hardU ps v1, v1)
      | otherwise = let v1 = fcmV ps (wU w) (wM w) in (fcmU ps v1 (wM w), v1)
    j = fcmJ ps u' v' (if wKmeans w then 1 else wM w)
    conv = wConv w || (case reverse (wJ w) of { (l : _) -> l - j < 1e-7; [] -> False })
    js = let h = wJ w ++ [j] in if length h > 60 then drop 1 h else h

-- 背景：その場所に点を置いたら、どのグループにどれだけ所属するか
makeBg :: World -> World
makeBg w = w { wBg = U.fromList (concat [ px x y | y <- [0 .. bgH - 1], x <- [0 .. bgW - 1] ]) }
  where
    px x y =
      let q = [((fromIntegral x + 0.5) / fromIntegral bgW, (fromIntegral y + 0.5) / fromIntegral bgH)]
          u = if wKmeans w then hardU q (wV w) else fcmU q (wV w) (wM w)
          (r, g, b) = mixCol u 0
      in [byte (r * 0.35), byte (g * 0.35), byte (b * 0.4)]
    byte v = fromIntegral (max 0 (min 255 (jsRound v))) :: Word8

-- 所属度の割合でグループの色を混ぜる
mixCol :: [[Double]] -> Int -> (Double, Double, Double)
mixCol u k = foldl' add (0, 0, 0) (zip [0 ..] u)
  where
    add (r, g, b) (i, row) =
      let m = getAt 0 row k
          (cr, cg, cb) = getAt (0, 0, 0) groupCols (i `mod` length groupCols)
      in (r + m * cr, g + m * cg, b + m * cb)

-- 自動のときは 12 フレームごとに 1 回更新
step :: World -> World
step w
  | wAuto w && not (wConv w) =
      let t = wTimer w + 1 in (if t `mod` 12 == 0 then iter else id) w { wTimer = t }
  | otherwise = w

-- 左の点の領域（p5.js より少し下げた：説明が 3 行あるため）
data Area = Area { aX, aY, aW, aH :: !Float }

areaOf :: Env -> Area
areaOf env = Area 20 92 (envW env * 0.58) (envH env - 142)

rgbC :: (Double, Double, Double) -> Color
rgbC (r, g, b) = makeColor (realToFrac r / 255) (realToFrac g / 255) (realToFrac b / 255) 1

draw :: Env -> World -> Picture
draw env w = view env $
  [ Translate (ax + aw / 2) (ay + ah / 2) (Scale (aw / fromIntegral bgW) (negate ah / fromIntegral bgH) bgPic)
  , Color (hsba 0 0 100 0.25) (rectLine (ax + aw / 2) (ay + ah / 2) aw ah) ]
    -- 点：色 = 所属度でグループの色を混ぜたもの
  ++ concat [ [ Color (if k == hover then white else greyN 0.12) (at (sx p, sy p) (disc (if k == hover then 8.5 else 7.5)))
              , Color (rgbC (mixCol (wU w) k)) (at (sx p, sy p) (disc 6.5)) ]
            | (k, p) <- zip [0 ..] (wP w) ]
    -- 中心
  ++ concat [ [ Color (rgbC (getAt (0, 0, 0) groupCols (i `mod` 6))) (Pictures [thickLine 4 (sx v - 12, sy v) (sx v + 12, sy v), thickLine 4 (sx v, sy v - 12) (sx v, sy v + 12)])
              , txt (sx v + 10) (sy v - 10) 14 white ("v" ++ show (i + 1)) ]
            | (i, v) <- zip [0 :: Int ..] (wV w) ]
  ++ [ txt 20 30 17 (hsb 0 0 95) ((if wKmeans w then "k-means (membership is 0 or 1)" else "Fuzzy c-means  m = " ++ fmt 2 (wM w))
                                  ++ "   c = " ++ show (wNC w) ++ "   points: " ++ show (length (wP w)))
     , txt 20 52 15 (hsb 0 0 85) ("Click: add   Space: 1 step   A: auto " ++ (if wAuto w then "ON" else "off")
                                  ++ "   Left/Right: c   Up/Down: m   K: switch   N: init   R: new data")
     , txt 20 74 14 (hsb 0 0 70) "Point color = group colors mixed by membership. Background = membership if a point were there"
       -- 右：J のグラフ
     , Color (hsba 0 0 100 0.06) (rectTL gx gy gw gh)
     , txt (gx + 10) (gy + 22) 16 (hsb 0 0 95) "Objective J = Sum u^m d^2 (per update)" ]
  ++ jGraph
    -- 中心の表
  ++ [ txt gx ty 16 (hsb 0 0 95) "Centers v_i (0..1 coordinates)" ]
  ++ [ txt (gx + 10) (ty + 24 + fromIntegral i * 20) 14 (hsb 0 0 80) ("v" ++ show (i + 1) ++ " = (" ++ fmt 3 vx ++ ", " ++ fmt 3 vy ++ ")")
     | (i, (vx, vy)) <- zip [0 :: Int ..] (wV w) ]
    -- マウスの点の所属度
  ++ [ txt gx my 16 (hsb 0 0 95) (if hover >= 0 then "Memberships of point " ++ show (hover + 1) ++ " (sum = 1)" else "Hover over a point to see its memberships") ]
  ++ (if hover >= 0 then hoverBars else [])
  ++ [ txt 20 (envH env - 20) 14 (if all snd (wTest w) then hsb 160 50 90 else hsb 0 80 100) (selfTestLine (wTest w)) ]
  where
    Area ax ay aw ah = areaOf env
    sx (x, _) = ax + realToFrac x * aw
    sy (_, y) = ay + realToFrac y * ah
    (mx, mY) = p5Mouse env
    hover = fst (foldl' (\(bk, bd) (k, p) -> let d = sqrt ((mx - sx p) ^ (2 :: Int) + (mY - sy p) ^ (2 :: Int))
                                             in if d < 14 && d < bd then (k, d) else (bk, bd))
                        (-1, 1e9) (zip [0 :: Int ..] (wP w)))
    bg = wBg w
    bgPic | U.length bg < bgW * bgH * 3 = Blank
          | otherwise = pixelImage bgW bgH 1 (\x y -> let o = 3 * (y * bgW + x) in (bg U.! o, bg U.! (o + 1), bg U.! (o + 2)))
    gx = envW env * 0.63
    gw = envW env * 0.34
    gy = 100
    gh = envH env * 0.3
    js = wJ w
    jGraph
      | length js > 1 =
          let hi = maximum js
              lo = minimum js
              sp = max 1e-9 (hi - lo)
              nJ = fromIntegral (length js - 1) :: Float
              inc = or (zipWith (\a b -> b > a + 1e-9) js (drop 1 js))
              lastJ = last js
          in [ Color (hsb 20 45 100) (thickPath 2.5 [ (gx + 10 + fromIntegral i / nJ * (gw - 20), gy + gh - 12 - realToFrac ((j - lo) / sp) * (gh - 50))
                                                    | (i, j) <- zip [0 :: Int ..] js ])
             , txt (gx + 10) (gy + 44) 14 (if inc then hsb 0 70 100 else hsb 160 50 90)
                 ("J = " ++ fmt 5 lastJ ++ (if inc then "   increased!" else "   never increased") ++ (if wConv w then "   (converged)" else "")) ]
      | otherwise = []
    ty = gy + gh + 34
    my = ty + 40 + fromIntegral (wNC w) * 20
    hoverBars =
      let us = [getAt 0 row hover | row <- wU w]
      in concat [ [ Color (rgbC (getAt (0, 0, 0) groupCols (i `mod` 6))) (rectTL (gx + 40) (my + 14 + fromIntegral i * 26) ((gw - 140) * realToFrac u) 18)
                  , txt (gx + 10) (my + 28 + fromIntegral i * 26) 14 (hsb 0 0 90) ("u" ++ show (i + 1))
                  , txt (gx + gw - 90) (my + 28 + fromIntegral i * 26) 14 (hsb 0 0 90) (fmt 3 u) ]
                | (i, u) <- zip [0 :: Int ..] us ]
         ++ [ txt (gx + 10) (my + 34 + fromIntegral (length us) * 26) 14 (hsb 0 0 70) ("Sum " ++ fmt 6 (sum us)) ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let Area ax ay aw ah = areaOf env
        (mx, my) = p5Mouse env
        p = (realToFrac ((mx - ax) / aw), realToFrac ((my - ay) / ah))
    in iter w { wP = wP w ++ [p], wU = [row ++ [1 / fromIntegral (wNC w)] | row <- wU w], wConv = False }
  EventKey (SpecialKey KeySpace) Down _ _ -> iter w { wConv = False }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "kK" -> initU w { wKmeans = not (wKmeans w) }
    | c `elem` "nN" -> initU w
    | c `elem` "rR" -> newData w
  EventKey (SpecialKey KeyRight) Down _ _ -> initU w { wNC = min 6 (wNC w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> initU w { wNC = max 2 (wNC w - 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> initU w { wM = min 4 (wM w + 0.25) }
  EventKey (SpecialKey KeyDown) Down _ _ -> initU w { wM = max 1.25 (wM w - 0.25) }
  _ -> w

-- ---- 自己テスト：手で計算できる値と比べる
selfTest :: [(String, Bool)]
selfTest =
  [ ("memberships of every point sum to 1", and [nearTol 1e-12 (getAt 0 (getAt [] u 0) k + getAt 0 (getAt [] u 1) k) 1 | k <- [0 .. length d - 1]])
  , ("J never increases in 30 iterations", nonInc js)
  , ("centers of the two clusters ~ (1/3, 1/3) and (31/3, 31/3)",
       case sortOn fst v of
         [(a0, a1), (b0, b1)] -> nearTol 0.01 a0 (1 / 3) && nearTol 0.01 a1 (1 / 3) && nearTol 0.01 b0 (31 / 3) && nearTol 0.01 b1 (31 / 3)
         _ -> False)
  , ("a point equally far from 2 centers gets 0.5 and 0.5", nearCol (fcmU [(1, 0)] [(0, 0), (2, 0)] 2) 0.5 0.5)
  , ("distances 1 and 2, m = 2: u = 1 / (1 + (1/2)^2) = 0.8", nearCol (fcmU [(1, 0)] [(0, 0), (3, 0)] 2) 0.8 0.2)
  , ("a point on a center belongs to that group with 1", col (fcmU [(0, 0)] [(0, 0), (5, 5)] 2) == Just (1, 0))
  , ("m = 1.1 gives almost 0 or 1 (close to k-means)", maybe False ((> 0.999) . fst) (col (fcmU [(1, 0)] [(0, 0), (3, 0)] 1.1)))
  , ("J never increases with k-means either", nonInc kJ) ]
  where
    d = [(0, 0), (0, 1), (1, 0), (10, 10), (10, 11), (11, 10)] :: [Pt]
    u0 = [[0.6, 0.3, 0.5, 0.4, 0.7, 0.2], [0.4, 0.7, 0.5, 0.6, 0.3, 0.8]]
    runs = take 30 (drop 1 (iterate (\(u, _, _) -> let v1 = fcmV d u 2; u1 = fcmU d v1 2 in (u1, v1, fcmJ d u1 v1 2)) (u0, [], 0)))
    (u, v, _) = last ((u0, [], 0) : runs)
    js = [j | (_, _, j) <- runs]
    nonInc xs = and (zipWith (\a b -> b <= a + 1e-12) xs (drop 1 xs))
    col um = case um of { [[a], [b]] -> Just (a, b); _ -> Nothing }
    nearCol um a b = maybe False (\(x, y) -> near x a && near y b) (col um)
    kRuns = take 10 (drop 1 (iterate (\(kv, _) -> let hu = hardU d kv; kv1 = fcmV d hu 1 in (kv1, fcmJ d (hardU d kv1) kv1 1)) ([(0, 0), (0, 1)], 0)))
    kJ = map snd kRuns

-- ---------- やってみよう ----------
-- 1. ↑↓ で m を変える。m = 1.25 では色がはっきり、m = 4 ではどの点もぼんやり混ざる
-- 2. K で k-平均法にする。境目の点が「どちらか 1 つ」に決められてしまうのを見比べよう
-- 3. 2 つのかたまりのちょうど真ん中をクリックして点を足す。所属度はいくつ？
-- 4. ←→ で c を 2 や 5 にする。本当は 3 つのかたまりなのに、c = 5 だとどう分かれる？
-- 5. N で何度も初期化。J の最後の値はいつも同じ？（局所解に落ちることもある）

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点、y は下向き）で
p5Mouse :: Env -> (Float, Float)
p5Mouse env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 左上が (x, y) の塗りつぶした長方形（p5.js の rect）
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 小数点以下 n 桁で書く（p5.js の toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) v ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Int
jsRound v = floor (v + 0.5)

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ==================================================
-- ここから下は「ファジィの道具」（中身は読まなくても OK。消さないでね）
-- 計算はすべて Double（倍精度）で行う
-- ==================================================
-- ---- 自己テストの道具
near :: Double -> Double -> Bool
near a b = nearTol 1e-9 a b

nearTol :: Double -> Double -> Double -> Bool
nearTol tol a b = abs (a - b) <= tol

-- 結果を端末に出す（テスト中は 1 行のまとめと失敗した項目だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK  " else "NG  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "NG ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- ---- ファジィ c-平均法の更新式（ps：点の列、u !! i !! k：点 k のグループ i への所属度、vs：中心）
d2 :: Pt -> Pt -> Double
d2 (ax, ay) (bx, by) = (ax - bx) * (ax - bx) + (ay - by) * (ay - by)

-- 点ごとの列（グループの数だけ）を、グループごとの行に並べかえる
toRows :: Int -> [[Double]] -> [[Double]]
toRows nv cols = [ [getAt 0 c i | c <- cols] | i <- [0 .. nv - 1] ]

-- 所属度：u_ik = 1 ÷ Σ_j (d_ik / d_jk)^(2/(m−1))。中心にぴったり重なる点はそのグループに 1
fcmU :: [Pt] -> [Pt] -> Double -> [[Double]]
fcmU ps vs m = toRows (length vs) (map colOf ps)
  where
    e = 1 / (m - 1)
    colOf p =
      let ds = map (d2 p) vs
      in case [z | (z, x) <- zip [0 :: Int ..] ds, x < 1e-18] of
           (z : _) -> [if i == z then 1 else 0 | i <- [0 .. length vs - 1]]
           [] -> [ 1 / foldl' (+) 0 [ (di / dj) ** e | dj <- ds ] | di <- ds ]

-- 中心：v_i = Σ u^m x ÷ Σ u^m（だれも所属しないグループは今の点の平均に置く）
fcmV :: [Pt] -> [[Double]] -> Double -> [Pt]
fcmV ps u m = map centerOf u
  where
    centerOf row =
      let (sx, sy, sw) = foldl' (\(a, b, c) ((x, y), r) -> let wt = r ** m in (a + wt * x, b + wt * y, c + wt)) (0, 0, 0) (zip ps row)
      in if sw < 1e-12
           then let n = if null ps then 1 else fromIntegral (length ps)
                in (sum (map fst ps) / n, sum (map snd ps) / n)
           else (sx / sw, sy / sw)

fcmJ :: [Pt] -> [[Double]] -> [Pt] -> Double -> Double
fcmJ ps u vs m = foldl' (+) 0 [ (r ** m) * d2 p v | (row, v) <- zip u vs, (p, r) <- zip ps row ]

-- k-平均法：いちばん近い中心に 1、ほかは 0
hardU :: [Pt] -> [Pt] -> [[Double]]
hardU ps vs = toRows (length vs) (map colOf ps)
  where
    colOf p =
      let b = foldl' (\bi (i, v) -> if d2 p v < d2 p (getAt (0, 0) vs bi) then i else bi) 0 (zip [0 :: Int ..] vs)
      in [if i == b then 1 else 0 | i <- [0 .. length vs - 1]]

-- 決まった順番の乱数（毎回同じ点の並びを作れる）。0〜1 の無限の列
lcg :: Int -> [Double]
lcg s = map (\x -> fromIntegral x / 233280) (drop 1 (iterate (\x -> (x * 9301 + 49297) `mod` 233280) (s * 9301 + 49297)))
