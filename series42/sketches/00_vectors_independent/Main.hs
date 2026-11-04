-- ============================================
-- 42-00 ベクトルの線形独立
-- 平面（2D）や空間（3D）に並んだベクトルから「いくつか選ぶ」と、その集まりは独立か従属か
--   独立 = どれも他の組み合わせで作れない = 階数（rank）が本数と同じ。従属 = 階数が本数より小さい
--   「独立な部分集合の集まり」がマトロイドの出発点。右の表は大きさごとの独立集合の数
-- 操作：先端をクリック = 選ぶ・外す / 先端をドラッグ = 動かす / 2・3 = 平面・空間 / ←→ = 空間を回す / ↑↓ = 本数 / R = ランダム
-- ============================================
module Main (main) where

import Art
import Data.Bits (testBit)
import Data.List (foldl', maximumBy)
import Numeric (showFFloat)

dim0 :: Int
dim0 = 2                 -- ★ 2 = 平面、3 = 空間

nVec0 :: Int
nVec0 = 4                -- ★ ベクトルの本数（3〜6）

unitPx :: Float
unitPx = 110             -- ★ 1 の長さ（px）

-- 階数の計算で丸め誤差が出ないよう、座標は Double で持つ
data Vec = Vec { vX, vY, vZ :: !Double, vOn :: !Bool }

data World = World
  { wDim    :: !Int
  , wN      :: !Int
  , wVecs   :: ![Vec]
  , wTheta  :: !Float      -- 空間を見る向き
  , wDrag   :: !Int        -- ドラッグ中のベクトル（-1 = なし）
  , wMoved  :: !Bool
  , wPrev   :: !Point      -- 前回のマウス位置
  , wFamily :: ![Int]      -- 大きさごとの独立集合の数
  }

main :: IO ()
main = runSketch (sketch "42-00 Linear independence" world0)
  { skBack  = hsb 230 30 10
  , skStart = \_ -> randomVectors world0
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World dim0 nVec0 [] 0.6 (-1) False (0, 0) []

randomVectors :: World -> IO World
randomVectors w = do
  vs <- mapM (const one) [1 .. wN w]
  pure (countFamily w { wVecs = makeDep vs })
  where
    r = randomRIO (-2, 2 :: Double)
    one = do
      x <- r
      y <- r
      z <- if wDim w == 3 then r else pure 0
      pure (Vec x y z False)
    -- 最初から 1 組だけ従属にしておく（3 本目 = 1 本目 + 2 本目）
    makeDep (a : b : _ : rest) =
      a : b : Vec (vX a + vX b) (vY a + vY b) (vZ a + vZ b) False : rest
    makeDep vs = vs

origin :: Env -> Point
origin env = (envW env * 0.33, envH env * 0.55)

-- ★ 空間の点 → 画面（縦軸まわりに theta 回し、奥行きは少し下にずらす斜投影）
toScreen :: Env -> World -> (Double, Double, Double) -> Point
toScreen env w (x0, y0, z0) =
  let (ox, oy) = origin env
      x = realToFrac x0 :: Float
      y = realToFrac y0 :: Float
      z = realToFrac z0 :: Float
      c = cos (wTheta w)
      s = sin (wTheta w)
      xr = x * c + z * s
      zr = -x * s + z * c
  in if wDim w == 2 then (ox + x * unitPx, oy - y * unitPx)
     else (ox + xr * unitPx, oy - y * unitPx + zr * 0.38 * unitPx)

tip :: Env -> World -> Vec -> Point
tip env w v = toScreen env w (vX v, vY v, vZ v)

asRow :: World -> Vec -> [Double]
asRow w v = if wDim w == 2 then [vX v, vY v] else [vX v, vY v, vZ v]

-- 全部の部分集合を調べて、大きさごとに独立なものを数える
countFamily :: World -> World
countFamily w =
  let n = wN w
      vs = wVecs w
      sizes = [ length rows
              | m <- [1 .. (2 :: Int) ^ n - 1]
              , let rows = [ asRow w v | (i, v) <- zip [0 ..] vs, testBit m i ]
              , rankReal rows == length rows ]
      fam = 1 : [ length (filter (== s) sizes) | s <- [1 .. n] ]   -- 空集合はいつも独立
  in w { wFamily = fam }

draw :: Env -> World -> Picture
draw env w =
  let o = origin env
      ts = toScreen env w
      gridLine a b = seg env 1 (gray 0.22) (ts a) (ts b)
      axes = [ seg env 1 (gray 0.35) (ts a) (ts b)
             | (a, b) <- take (wDim w) [((3, 0, 0), (-3, 0, 0)), ((0, 3, 0), (0, -3, 0)), ((0, 0, 3), (0, 0, -3))] ]
      -- 床のグリッド
      grid = if wDim w /= 3 then [] else
        concat [ [gridLine (i, -2, -2) (i, -2, 2), gridLine (-2, -2, i) (2, -2, i)] | i <- [-2 .. 2] ]
      on = filter vOn (wVecs w)
      rows = map (asRow w) on
      r = rankReal rows
      k = length rows
      indep = r == k
      hueSel = if indep then 130 else 5
      -- 選んだベクトルの張る空間（直線 or 平面）をうっすら描く
      spanPic = case on of
        (p : _)
          | r == 1 ->
              seg env 2 (hsba hueSel 60 70 0.5)
                (ts (vX p * 4, vY p * 4, vZ p * 4)) (ts (-vX p * 4, -vY p * 4, -vZ p * 4))
          | wDim w == 3 && k >= 2 && r == 2 ->
              let others = [ q | q <- on, rankReal [asRow w p, asRow w q] == 2 ]
                  q = pick p others 0
                  corner (s, t) = sp env (ts (vX p * s + vX q * t, vY p * s + vY q * t, vZ p * s + vZ q * t))
              in Color (hsba hueSel 60 70 0.18) (Polygon (map corner [(2, 2), (2, -2), (-2, -2), (-2, 2)]))
        _ -> Blank
      vecPic i v =
        let p@(px, py) = tip env w v
            h = if vOn v then hueSel else 210
            c = hsb h (if vOn v then 80 else 30) 95
            name = "v" ++ show (i + 1 :: Int) ++ " (" ++ commaList (map (f1 . realToFrac) (asRow w v)) ++ ")"
        in Pictures [ seg env (if vOn v then 5 else 2.5) c o p
                    , dot env (if vOn v then 9 else 6.5) c p
                    , txt env (px + 12) (py - 14) 15 white name ]
      -- 右のパネル
      px0 = envW env - 440
      py0 = 110
      title | k == 0 = "Nothing chosen"
            | indep = "Independent"
            | otherwise = "Dependent"
      titleC = if k == 0 then white else hsb hueSel 80 100
      famRows = takeUntilZero (zip [0 :: Int ..] (wFamily w))
      famRow (s, c) =
        ( 16
        , if s == k && k > 0 then hsb hueSel 70 100 else white
        , "|A| = " ++ show s ++ ": " ++ show c ++ (if c == 0 then "  (no larger set)" else "") )
  in Pictures $
       axes ++ grid ++ [spanPic] ++ zipWith vecPic [0 ..] (wVecs w) ++
       [ dot env 3 white o
       , txt env 20 40 16 white
           ("Click: select  Drag: move  2/3: plane/space  "
             ++ (if wDim w == 3 then "<- ->: turn  " else "")
             ++ "Up/Down: count " ++ show (wN w) ++ "  R: random")
       , txt env 20 66 14 (gray 0.6) "Independent = rank equals count. Dependent = one chosen vector is made from the others."
       , box env (withA 0.08 white) (px0 - 20) (py0 - 40) 450 350
       , stack env px0 py0
           ([ (28, titleC, title)
            , (18, white, "|A| = " ++ show k ++ "   rank r(A) = " ++ show r)
            , (14, gray 0.6, if wDim w == 2 then "Plane: 3+ are dependent (r <= 2)"
                                            else "Space: 4+ are dependent (r <= 3)")
            , (10, white, "")
            , (16, white, "Independent sets, by size") ] ++ map famRow famRows)
       ]

-- 0 が出たらそこまで（0 の行も含める）
takeUntilZero :: [(Int, Int)] -> [(Int, Int)]
takeUntilZero [] = []
takeUntilZero (x@(_, c) : r) = if c == 0 then [x] else x : takeUntilZero r

commaList :: [String] -> String
commaList [] = ""
commaList [x] = x
commaList (x : r) = x ++ ", " ++ commaList r

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    pure w { wDrag = nearestTip env w, wMoved = False, wPrev = envMouse env }
  EventKey (MouseButton LeftButton) Up _ _ ->
    pure (if wDrag w >= 0 && not (wMoved w)
            then w { wVecs = setAt (wDrag w) (\v -> v { vOn = not (vOn v) }) (wVecs w), wDrag = -1 }
            else w { wDrag = -1 })
  EventMotion pos@(mx, my)
    | wDrag w >= 0 ->
        let (px, py) = wPrev w
            dx = realToFrac ((mx - px) / unitPx) :: Double
            dy = realToFrac ((my - py) / unitPx) :: Double
            c = realToFrac (cos (wTheta w))
            s = realToFrac (sin (wTheta w))
            cl = max (-3) . min 3
            move v
              | wDim w == 2 = v { vX = cl (vX v + dx), vY = cl (vY v + dy) }
              | otherwise = v { vX = cl (vX v + dx * c), vZ = cl (vZ v + dx * s), vY = cl (vY v + dy) }
        in pure (countFamily w { wVecs = setAt (wDrag w) move (wVecs w), wMoved = True, wPrev = pos })
    | otherwise -> pure w { wPrev = pos }
  EventKey (Char '2') Down _ _ ->
    pure (countFamily w { wDim = 2, wVecs = map (\v -> v { vZ = 0 }) (wVecs w) })
  EventKey (Char '3') Down _ _ -> do
    vs <- mapM (\v -> if vZ v == 0 then (\z -> v { vZ = z }) <$> randomRIO (-1.5, 1.5) else pure v) (wVecs w)
    pure (countFamily w { wDim = 3, wVecs = vs })
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wTheta = wTheta w - 0.15 }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wTheta = wTheta w + 0.15 }
  EventKey (SpecialKey KeyUp) Down _ _ -> randomVectors w { wN = min 6 (wN w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> randomVectors w { wN = max 3 (wN w - 1) }
  EventKey (Char c) Down _ _ | c `elem` "rR" -> randomVectors w
  _ -> pure w

nearestTip :: Env -> World -> Int
nearestTip env w = nearestIn 22 [ dist2 (mouseP env) (tip env w v) | v <- wVecs w ]

-- ---------- やってみよう ----------
-- 1. 平面で 2 本だけ選んで、片方をドラッグしてもう片方と同じ直線に乗せる。赤になる瞬間の階数は？
-- 2. 空間で 3 本選び、1 本をドラッグして他の 2 本が張る平面（うすい面）に乗せる。「従属」になる
-- 3. 右の表で「|A| = 3 が 0 個」になるのはどんなとき？（平面ならいつも。空間なら？）
-- 4. 大きさが最大の独立集合（基）はいくつある？ 全部数えると「基の個数」になる

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標（中央が原点、y は上向き）へ
sp :: Env -> Point -> Point
sp env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点の座標で）
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 文字（左下を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sp env (x, y) in label a b size c s

-- 上から順に文字を並べる：(大きさ, 色, 文字)。空の文字は行間だけあける
stack :: Env -> Float -> Float -> [(Float, Color, String)] -> Picture
stack env x y0 rows = Pictures (go y0 rows)
  where go _ [] = []
        go y ((sz, c, s) : r) = txt env x y sz c s : go (y + sz * 1.5) r

-- 太さ w の線分
seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c p q = Color c (thickLine w (sp env p) (sp env q))

-- 塗りつぶしの丸（半径 r）
dot :: Env -> Float -> Color -> Point -> Picture
dot env r c p = Color c (at (sp env p) (disc r))

-- 塗りつぶしの長方形（左上の角 (x, y)、幅 w、高さ h）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (a, b) = sp env (x + w / 2, y + h / 2) in Color c (rect a b w h)

-- リストの i 番目（範囲外なら d を返す安全版）
pick :: a -> [a] -> Int -> a
pick d xs i
  | i < 0 = d
  | otherwise = case drop i xs of { (x : _) -> x; [] -> d }

-- リストの i 番目だけ f で変える
setAt :: Int -> (a -> a) -> [a] -> [a]
setAt i f xs = [ if k == i then f x else x | (k, x) <- zip [0 ..] xs ]

-- 小数 1 桁の文字列
f1 :: Float -> String
f1 x = showFFloat (Just 1) x ""

-- limit より近いものの中でいちばん近い番号（なければ -1）
nearestIn :: Float -> [Float] -> Int
nearestIn limit ds = fst (foldl' pickNear (-1, limit) (zip [0 ..] ds))
  where pickNear (bi, bd) (i, d) = if d < bd then (i, d) else (bi, bd)

-- 実数のベクトルの階数（ガウスの消去法。小さい値は 0 とみなす）
rankReal :: [[Double]] -> Int
rankReal rows = go 0 rows
  where
    n = case rows of { (r : _) -> length r; [] -> 0 }
    val c r = case drop c r of { (v : _) -> v; [] -> 0 }
    go c rs
      | c >= n || null rs = 0
      | abs (val c p) < 1e-6 = go (c + 1) rs
      | otherwise = 1 + go (c + 1) [ zipWith (\x y -> x - f r * y) r p | r <- rest ]
      where
        ip = fst (maximumBy (\a b -> compare (abs (val c (snd a))) (abs (val c (snd b)))) (zip [0 :: Int ..] rs))
        p = case drop ip rs of { (r : _) -> r; [] -> [] }
        rest = [ r | (i, r) <- zip [0 ..] rs, i /= ip ]
        f r = val c r / val c p
