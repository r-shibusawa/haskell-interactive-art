{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 44-10 キャビティ流れ（ふたが動く箱）
-- 正方形の箱の「ふた」だけが横に動く。粘性でひきずられて、中に大きな渦が 1 つできる
-- 計算流体力学（CFD）のプログラムを作ったら、まずこの問題で答え合わせをする定番の問題
-- ナヴィエ・ストークス方程式を「渦度 ω と流れ関数 ψ」の形で解く（圧力を消去できる）：
--   ∂ω/∂t + u ∂ω/∂x + v ∂ω/∂y = ν∇²ω　　∇²ψ = −ω　　u = ∂ψ/∂y,  v = −∂ψ/∂x
-- 操作：上下キー = レイノルズ数 Re / 1〜3 = Re 100・400・1000 / 左右キー = ふたの向き / V = 表示 / R = やり直し
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Control.Monad.ST (ST, runST)
import qualified Data.IntSet as IS
import Data.Array (listArray, (!))
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

n :: Int
n = 64              -- ★ 格子の分割数（64 → 65 × 65 点）

subSteps :: Int
subSteps = 16       -- ★ 1 フレームに進める回数

m :: Int
m = n + 1

h :: Float
h = 1 / fromIntegral n

data World = World
  { wRe   :: !Float            -- ★ レイノルズ数 Re = U L / ν（ふたの速さ U = 1、箱の一辺 L = 1）
  , wLid  :: !Float            -- ふたの速さ（+1 = 右へ、−1 = 左へ）
  , wView :: !Int              -- 0 = 渦度, 1 = 速さ
  , wPsi  :: !(U.Vector Float) -- 流れ関数 ψ（j = 0 が底、j = n がふた）
  , wOm   :: !(U.Vector Float) -- 渦度 ω
  , wT    :: !Float
  , wDots :: ![Point]          -- 粒（0〜1 の座標、y は上向き）
  }

main :: IO ()
main = runSketch (sketch "44-10 Lid-driven cavity" (restart (World 400 1 0 U.empty U.empty 0 [])))
  { skBack  = hsb 215 60 10
  , skStart = \_ -> do
      ds <- replicateM 350 ((,) <$> randomRIO (0, 1) <*> randomRIO (0, 1))
      pure (restart (World 400 1 0 U.empty U.empty 0 ds))
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

restart :: World -> World
restart w = w { wPsi = U.replicate (m * m) 0, wOm = U.replicate (m * m) 0, wT = 0 }

-- 1 歩の時間刻み：陽解法がこわれない刻み（粘性の条件と、移流の条件）
timeStep :: World -> Float
timeStep w = min (0.2 * h * h / (1 / wRe w)) (0.4 * h)

-- ★ ナヴィエ・ストークス方程式（渦度・流れ関数の形）を subSteps 回進める
stepFluid :: World -> World
stepFluid w = runST $ do
  psi <- U.thaw (wPsi w)
  om <- U.thaw (wOm w)
  om2 <- MU.replicate (m * m) 0
  let !nu = 1 / wRe w
      !dt = timeStep w
      !lid = wLid w
  for 1 subSteps $ \_ -> do
    -- ① ∇²ψ = −ω を反復で解く（SOR 法）。壁では ψ = 0（壁を通りぬけない）
    for 1 12 $ \_ -> for 1 (n - 1) $ \j -> do
      l0 <- rd psi (j * m)
      sor psi om j 1 l0
    -- ② 壁の渦度：壁のすぐ内側の ψ から決まる（すべりなし条件。ふたは速さ lid で動く）
    for 0 n $ \i -> do
      a <- rd psi (i + m)
      wr om i (-2 * a / (h * h))                                  -- 底
      b <- rd psi (i + (n - 1) * m)
      wr om (i + n * m) (-2 * b / (h * h) - 2 * lid / h)           -- ふた
      c <- rd psi (1 + i * m)
      wr om (i * m) (-2 * c / (h * h))                             -- 左
      d <- rd psi (n - 1 + i * m)
      wr om (n + i * m) (-2 * d / (h * h))                         -- 右
    -- ③ 渦度を運んで（移流）、広げる（粘性）
    for 1 (n - 1) $ \j -> for 1 (n - 1) $ \i -> do
      let k = i + j * m
      pu <- rd psi (k + m)
      pd <- rd psi (k - m)
      pr <- rd psi (k + 1)
      pl <- rd psi (k - 1)
      o <- rd om k
      ol <- rd om (k - 1)
      orr <- rd om (k + 1)
      od <- rd om (k - m)
      ou <- rd om (k + m)
      let u = (pu - pd) / (2 * h)
          v = -(pr - pl) / (2 * h)
          wx = if u > 0 then o - ol else orr - o   -- 風上差分（流れてくる側の値を使う）
          wy = if v > 0 then o - od else ou - o
          lap = ol + orr + od + ou - 4 * o
          new = o + dt * (-(u * wx + v * wy) / h + nu * lap / (h * h))
      wr om2 k (if isNaN new then 0 else max (-500) (min 500 new))
    for 1 (n - 1) $ \j -> for 1 (n - 1) $ \i -> rd om2 (i + j * m) >>= wr om (i + j * m)
  psi' <- U.freeze psi
  om' <- U.freeze om
  pure w { wPsi = psi', wOm = om', wT = wT w + dt * fromIntegral subSteps }

-- SOR の 1 行（左どなりの新しい値 l は持ち回る）
sor :: Grid s -> Grid s -> Int -> Int -> Float -> ST s ()
sor !psi !om !j !i !l
  | i > n - 1 = pure ()
  | otherwise = do
      let k = i + j * m
      p <- rd psi k
      r <- rd psi (k + 1)
      dn <- rd psi (k - m)
      up <- rd psi (k + m)
      o <- rd om k
      -- p + 1.7 × (0.25 × (l + r + dn + up + h²ω) − p) を、l にかかわらない部分を先に計算する形に並べかえたもの
      let new = (1 - 1.7) * p + 0.425 * (r + dn + up + h * h * o) + 0.425 * l
      wr psi k new
      sor psi om j (i + 1) new

-- 場所 (x, y)（0〜1）での速度
velocityAt :: World -> Float -> Float -> Point
velocityAt w x y =
  let i = clampI 1 (n - 2) (floor (x * fromIntegral n))
      j = clampI 1 (n - 2) (floor (y * fromIntegral n))
      k = i + j * m
      psi = wPsi w
  in ((psi U.! (k + m) - psi U.! (k - m)) / (2 * h), -(psi U.! (k + 1) - psi U.! (k - 1)) / (2 * h))

step :: Env -> Float -> World -> IO World
step _ _ w0 = do
  let w = stepFluid w0
      dtSum = timeStep w0 * fromIntegral subSteps
  ds <- mapM (\(x0, y0) -> do
      let (u, v) = velocityAt w x0 y0
          x = x0 + u * dtSum
          y = y0 + v * dtSum
      r <- randomRIO (0, 1 :: Float)
      if x < 0.005 || x > 0.995 || y < 0.005 || y > 0.995 || r < 0.003 || isNaN x || isNaN y
        then (,) <$> randomRIO (0, 1) <*> randomRIO (0, 1)
        else pure (x, y)) (wDots w)
  pure w { wDots = ds }

draw :: Env -> World -> Picture
draw env w = Pictures
  [ Translate (bx + side / 2 - envW env / 2) (envH env / 2 - by - side / 2)
      (Scale (side / fromIntegral m) (side / fromIntegral m) (pixelImage m m 1 (render w)))
  , view env $
      -- 流線（ψ の等高線）。すみの小さな逆回りの渦は、細かい間かくで別に描く
      [ Color (hsbA 0 0 100 60) (Pictures (map Line (contourChains m m pos f1 0)))
      , Color (hsbA 42 80 100 80) (Pictures (map Line (contourChains m m pos f2 0))) ]
      ++ [ dot (hsbA 0 0 100 75) (bx + x * side) (by + side - y * side) 3 | (x, y) <- wDots w ]
      -- 箱とふた
      ++ [ wall (bx, by) (bx, by + side), wall (bx, by + side) (bx + side, by + side), wall (bx + side, by + side) (bx + side, by)
         , Color (hsb 12 70 100) (Pictures
             [ thickLine 6 (bx, by - 3) (bx + side, by - 3)
             , thickLine 3 (bx + side / 2 - lid * 60, by - 20) (ax, by - 20)
             , thickLine 3 (ax, by - 20) (ax - lid * 10, by - 27)
             , thickLine 3 (ax, by - 20) (ax - lid * 10, by - 13) ])
         , dot (hsb 42 90 100) (sx bi) (sy bj) 10 ]
      -- 右：まん中の縦線での横向きの速さ u(y)（論文の表とくらべる定番のグラフ）
      ++ [ Color (hsb 0 0 40) (Pictures [Line [(mid, by), (mid, by + side)], Line [(gx, by), (gx + gw, by)], Line [(gx, by + side), (gx + gw, by + side)]])
         , path 3 (hsb 185 70 100)
             [ (mid + u * gw / 2, sy j)
             | j <- [0 .. n]
             , let u = if j == 0 then 0 else if j == n then lid
                       else (psi U.! (n `div` 2 + (j + 1) * m) - psi U.! (n `div` 2 + (j - 1) * m)) / (2 * h) ]
         , txt gx (by - 10) 14 (hsb 185 70 100) "u(y) on the middle vertical line"
         , txt (envW env * 0.06) (by + side + 28) 14 (hsb 0 0 65)
             "Check (Ghia et al. 1982, lid to the right): Re 100 -> (0.617, 0.734)  Re 400 -> (0.554, 0.605)  Re 1000 -> (0.531, 0.563)"
         , txt (envW env * 0.06) (by + side + 52) 14 (hsb 0 0 65)
             ("Main vortex center (yellow dot): x = " ++ fx 3 (fromIntegral bi / fromIntegral n) ++ ", y = " ++ fx 3 (fromIntegral bj / fromIntegral n)
              ++ "    min of stream function " ++ fx 4 (-best))
         , txt 20 40 18 (hsb 0 0 100)
             ("Lid-driven cavity   Re = " ++ show (round (wRe w) :: Int) ++ " (Up/Down, 1-3)   lid direction (Left/Right)   view: "
              ++ (if wView w == 1 then "speed" else "vorticity") ++ " (V)   R = reset   t = " ++ fx 1 (wT w))
         , txt 20 68 15 (hsb 0 0 80)
             "Navier-Stokes (vorticity form)  dw/dt + (u.grad)w = nu lap w ,  lap psi = -w     white = streamlines, yellow = corner eddies (x100)" ]
  ]
  where
    psi = wPsi w
    lid = wLid w
    side = min (envW env * 0.52) (envH env * 0.74)
    bx = envW env * 0.06
    by = envH env * 0.18
    sx i = bx + fromIntegral i / fromIntegral n * side
    sy j = by + side - fromIntegral j / fromIntegral n * side
    pos i j = (sx i, sy j)
    f1 = U.map (/ 0.01) psi
    f2 = U.map (\p -> max (p * lid) 0 / 0.0002) psi   -- 大きな渦は ψ × lid < 0、すみの渦は > 0
    (best, bk) = U.foldl' (\(b, bkk) (k, p) -> if -p * lid > b then (-p * lid, k) else (b, bkk)) (0, 0) (U.indexed psi)
    bi = bk `mod` m
    bj = bk `div` m
    ax = bx + side / 2 + lid * 60
    gx = envW env * 0.66
    gw = envW env * 0.28
    mid = gx + gw / 2
    wall a b = Color (hsb 42 40 85) (thickLine 4 a b)

-- 色（渦度 または 速さ）。画像の上の行がふた（j = n）
render :: World -> Int -> Int -> (Word8, Word8, Word8)
render w i y =
  let j = n - y
      k = i + j * m
  in if wView w == 0
       then let s = wOm w U.! k / 6
                a = min (abs s) 1
            in if s > 0 then (b8 (8 + 247 * a), b8 (16 + 110 * a), b8 (34 + 70 * a))
                        else (b8 (8 + 20 * a), b8 (16 + 200 * a), b8 (34 + 221 * a))
       else let (u, v) = if i > 0 && i < n && j > 0 && j < n
                           then velocityAt w (fromIntegral i / fromIntegral n) (fromIntegral j / fromIntegral n)
                           else (if j == n then wLid w else 0, 0)
                sp = min (sqrt (u * u + v * v) * 1.6) 1
            in (b8 (8 + 240 * sp * sp), b8 (16 + 220 * sp), b8 (60 + 150 * sp))

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wRe = min 3000 (wRe w * 1.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wRe = max 10 (wRe w / 1.5) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wLid = 1 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wLid = -1 }
  EventKey (Char '1') Down _ _ -> w { wRe = 100 }
  EventKey (Char '2') Down _ _ -> w { wRe = 400 }
  EventKey (Char '3') Down _ _ -> w { wRe = 1000 }
  EventKey (Char k) Down _ _
    | k `elem` "vV" -> w { wView = 1 - wView w }
    | k `elem` "rR" -> restart w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1・2・3 で Re = 100・400・1000。渦の中心（黄 ●）は Re が上がるとどこへ動く？ 論文の値と比べよう
-- 2. 右下・左下のすみにできる黄色の小さな渦（モファット渦）は、Re でどう育つ？
-- 3. Re = 10 にする。流れは左右ほぼ対称（ストークス流れ）。なぜ対称になる？
-- 4. 左右キーでふたを逆向きに。渦が入れかわるまで何秒かかる？ Re で変わる？
-- 5. n を 32 や 96 にして、渦の中心の位置がどう変わるか調べる（格子を細かくすると論文の値に近づく）

-- ---------- 道具（解くための道具） ----------
type Grid s = MU.MVector s Float

{-# INLINE rd #-}
rd :: Grid s -> Int -> ST s Float
rd = MU.unsafeRead

{-# INLINE wr #-}
wr :: Grid s -> Int -> Float -> ST s ()
wr = MU.unsafeWrite

{-# INLINE for #-}
for :: Int -> Int -> (Int -> ST s ()) -> ST s ()
for lo hi f = go lo
  where go !i | i > hi = pure ()
              | otherwise = f i >> go (i + 1)

-- ---------- 道具（等高線を描く） ----------
-- 等高線（マーチングスクエア法）：nx × ny の格子の値 f が「整数」になる所を線で結ぶ。
-- pos i j = 格子点の画面座標。wrap > 0 なら、値が wrap だけ飛ぶ「切れ目」をまたいでつなぐ。
-- 小さな線分をたくさん描くと遅いので、端がつながる線分を 1 本の折れ線にまとめて返す
contourChains :: Int -> Int -> (Int -> Int -> Point) -> U.Vector Float -> Float -> [[Point]]
contourChains nx ny pos f wrap = chainSegments segs
  where
    segs = concat [ cellSegs i j | j <- [0 .. ny - 2], i <- [0 .. nx - 2] ]
    -- 格子の辺の番号（横の辺は偶数、縦の辺は奇数）
    hEdge i j = 2 * (i + j * nx)
    vEdge i j = 2 * (i + j * nx) + 1
    cellSegs i j =
      let raw = [f U.! (i + j * nx), f U.! (i + 1 + j * nx), f U.! (i + 1 + (j + 1) * nx), f U.! (i + (j + 1) * nx)]
          c0 = head raw
          c = c0 : [ if wrap > 0 then v - wrap * fromIntegral (round ((v - c0) / wrap) :: Int) else v | v <- drop 1 raw ]
          lo = minimum c
          hi = maximum c
          ps = [pos i j, pos (i + 1) j, pos (i + 1) (j + 1), pos i (j + 1)]
          es = [hEdge i j, vEdge (i + 1) j, hEdge i (j + 1), vEdge i j]
          corner = zip3 c ps es
          sides = zip corner (drop 1 corner ++ take 1 corner)
          crossings lv = [ ((e, lv), (ax + s * (bx - ax), ay + s * (by - ay)))
                         | ((a, (ax, ay), e), (b, (bx, by), _)) <- sides
                         , (a < fromIntegral lv) /= (b < fromIntegral lv)
                         , let s = (fromIntegral lv - a) / (b - a) ]
          pairUp (p : q : rest) = (p, q) : pairUp rest
          pairUp _ = []
      in if not (hi - lo < 5) then []   -- 特異点のすぐそば（値が急変）は描かない
         else concat [ pairUp (crossings lv) | lv <- [ceiling lo .. floor hi :: Int] ]

-- 端点（辺の番号と高さの組）を共有する線分をつないで折れ線にする
chainSegments :: [(((Int, Int), Point), ((Int, Int), Point))] -> [[Point]]
chainSegments segList = go 0 IS.empty
  where
    n = length segList
    segV = listArray (0, max 0 (n - 1)) segList
    key (e, lv) = e * 65536 + (lv `mod` 65536)   -- 辺と高さの組を 1 つの整数に
    adj = IM.fromListWith (++) (concat [ [(key ka, [s]), (key kb, [s])] | (s, ((ka, _), (kb, _))) <- zip [0 ..] segList ])
    seg s = segV ! s
    go i used
      | i >= n = []
      | IS.member i used = go (i + 1) used
      | otherwise =
          let ((ka, pa), (kb, pb)) = seg i
              (fwd, u1) = extend kb (IS.insert i used)
              (back, u2) = extend ka u1
          in (reverse back ++ [pa, pb] ++ fwd) : go (i + 1) u2
    -- 端点 k から、まだ使っていない線分をたどれるだけたどる
    extend k used = case [ t | t <- IM.findWithDefault [] (key k) adj, not (IS.member t used) ] of
      (t : _) ->
        let ((ka, pa), (kb, pb)) = seg t
            (nk, np) = if ka == k then (kb, pb) else (ka, pa)
            (rest, u') = extend nk (IS.insert t used)
        in (np : rest, u')
      [] -> ([], used)

-- ---------- 道具 ----------
clampI :: Int -> Int -> Int -> Int
clampI lo hi = max lo . min hi

fx :: Int -> Float -> String
fx d v = showFFloat (Just d) v ""

b8 :: Float -> Word8
b8 v = fromIntegral (max 0 (min 255 (round v :: Int)))

view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h' s b a = hsba h' s b (a / 100)

path :: Float -> Color -> [Point] -> Picture
path w c ps = Color c (Pictures (zipWith (thickLine w) ps (drop 1 ps)))

dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))
