-- ============================================
-- 40-08 ガウス＝ボネの定理 ─ 曲がり方を全部足すと「穴の数」になる
-- 三角形に分けた閉曲面で、各頂点の「角不足」= 2π − （その頂点に集まる三角形の角の和）が離散的なガウス曲率
--   角不足の総和 = 2π χ（χ = オイラー標数 = V − E + F = 2 − 2g、g = 穴の数）
--   球面 → 4π、トーラス → 0、2 つ穴 → −4π。頂点をどう動かしても（形を変えても）総和は変わらない！
-- 操作：1〜3 = 曲面 / 頂点をドラッグ = 変形 / 空いた所をドラッグ = 視点 / N = ランダムに揺らす / L = なめらかにする / R = 元に戻す
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

noiseAmp :: Float
noiseAmp = 14             -- ★ N で揺らす大きさ（px）

type Tri = (Int, Int, Int)

data World = World
  { wSurf :: !Int
  , wV, wV0 :: !(V.Vector V3)       -- 頂点と、最初の頂点
  , wFc :: ![Tri]                   -- 三角形
  , wDefect :: !(U.Vector Double)   -- 各頂点の角不足
  , wNormal :: !(V.Vector V3)       -- 頂点の法線（辺を少し浮かせる）
  , wDrag :: !Int                   -- つかんでいる頂点（なければ −1）
  , wRx, wRy :: !Float
  , wPrev :: !Point }

main :: IO ()
main = runSketch (sketch "40-08 Gauss-Bonnet theorem" (buildSurface 1 world0))
  { skBack  = hsb 220 30 9
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 V.empty V.empty [] U.empty V.empty (-1) (-0.4) 0.5 (0, 0)

-- ★ 曲面の三角形分割を作る
buildSurface :: Int -> World -> World
buildSurface no w =
  let (vs, fc) = case no of
        1 -> icosphere 2 170
        2 -> torusMesh 24 12 150 62
        _ -> genus2 62
  in refresh w { wSurf = no, wV = vs, wV0 = vs, wFc = fc }

-- 頂点が動いたら、角不足と法線を計算し直す
refresh :: World -> World
refresh w = w { wDefect = computeDefects (wV w) (wFc w), wNormal = computeNormals (wV w) (wFc w) }

toD :: V3 -> (Double, Double, Double)
toD (V3 a b c) = (realToFrac a, realToFrac b, realToFrac c)

-- 各頂点の角不足を計算（2π − 角の和）
computeDefects :: V.Vector V3 -> [Tri] -> U.Vector Double
computeDefects vs fc =
  U.accum (-) (U.replicate (V.length vs) (2 * pi))
    (concat [[(a, angle a b c), (b, angle b c a), (c, angle c a b)] | (a, b, c) <- fc])
  where
    angle i j k =
      let (ix, iy, iz) = toD (vs V.! i)
          (jx, jy, jz) = toD (vs V.! j)
          (kx, ky, kz) = toD (vs V.! k)
          (ux, uy, uz) = unitD (jx - ix) (jy - iy) (jz - iz)
          (wx, wy, wz) = unitD (kx - ix) (ky - iy) (kz - iz)
      in acos (max (-1) (min 1 (ux * wx + uy * wy + uz * wz)))
    unitD x y z = let l0 = sqrt (x * x + y * y + z * z)
                      l = if l0 == 0 then 1 else l0
                  in (x / l, y / l, z / l)

computeNormals :: V.Vector V3 -> [Tri] -> V.Vector V3
computeNormals vs fc =
  V.map unit3 $ V.accum add3 (V.replicate (V.length vs) (V3 0 0 0))
    (concat [ [(a, n), (b, n), (c, n)]
            | (a, b, c) <- fc
            , let n = cross3 (sub3 (vs V.! b) (vs V.! a)) (sub3 (vs V.! c) (vs V.! a)) ])

-- 各頂点を隣の平均に近づける（なめらかに。総和は変わらない）
laplacian :: Float -> [Tri] -> V.Vector V3 -> V.Vector V3
laplacian t fc vs =
  let pairs = concat [[(a, b), (b, c), (c, a)] | (a, b, c) <- fc]
      n = V.length vs
      sums = V.accum add3 (V.replicate n (V3 0 0 0)) (concat [[(i, vs V.! j), (j, vs V.! i)] | (i, j) <- pairs])
      cnt = U.accum (+) (U.replicate n (0 :: Int)) (concat [[(i, 1), (j, 1)] | (i, j) <- pairs])
  in V.imap (\i p -> if cnt U.! i == 0 then p else add3 p (scale3 t (sub3 (scale3 (1 / fromIntegral (cnt U.! i)) (sums V.! i)) p))) vs

draw :: Env -> World -> Picture
draw env w =
  let vw = view (wRx w) (wRy w)
      vs = V.map vw (wV w)
      lifted = V.imap (\i p -> vw (add3 p (scale3 1.2 (wNormal w V.! i)))) (wV w)
      edgeCol = hsba 0 0 100 0.45
      triItem (a, b, c) =
        let (r1, g1, b1) = col a
            (r2, g2, b2) = col b
            (r3, g3, b3) = col c
            (z, fill) = faceItem 0.33 vs (Face a b c c ((r1 + r2 + r3) / 3) ((g1 + g2 + g3) / 3) ((b1 + b2 + b3) / 3))
            pa = scr (lifted V.! a)
            pb = scr (lifted V.! b)
            pc = scr (lifted V.! c)
        in (z, Pictures [fill, Color edgeCol (Line [pa, pb, pc, pa])])
      col i = kColor (realToFrac (wDefect w U.! i)) 0.35
      dragged = [ball (hsb 45 90 100) 6 (vs V.! wDrag w) | wDrag w >= 0, wDrag w < V.length vs]
  in Pictures [paint (map triItem (wFc w) ++ dragged), hud env w]

hud :: Env -> World -> Picture
hud env w =
  let h = envH env
      def = wDefect w
      total = U.sum def
      chi = total / (2 * pi)
      nv = V.length (wV w)
      nf' = length (wFc w)
      ne = nf' * 3 `div` 2
      g = hsb 0 0 65
      name = case wSurf w of { 1 -> "Sphere"; 2 -> "Torus"; _ -> "Two-holed surface" }
      d2 n v = nf n (realToFrac (v :: Double))
      dmax = U.foldl' max (-1e9) def
      dmin = U.foldl' min 1e9 def
      -- 角不足のヒストグラム
      (hx, hy, hw, hh) = (envW env - 300, h - 180, 260, 120)
      binOf d = max 0 (min 23 (floor ((d + 1.2) / 2.4 * 24))) :: Int
      bins = U.accum (+) (U.replicate 24 (0 :: Int)) [(binOf d, 1) | d <- U.toList def]
      mx = fromIntegral (U.foldl' max 1 bins) :: Float
      fi k = fromIntegral (k :: Int) :: Float
  in Pictures $
       [ txt env 20 40 16 white ("1-3: surface (" ++ name ++ ")   drag a vertex: deform   drag empty space: view")
       , txt env 20 62 16 white "N: jiggle   L: smooth   R: reset"
       , txt env 20 86 14 g "color = angle defect (red +, blue -, white 0)"
       , txt env 20 106 14 g ("vertices V = " ++ show nv ++ "   edges E = " ++ show ne ++ "   faces F = " ++ show nf' ++ "   ->  V - E + F = " ++ show (nv - ne + nf'))
       , box env 20 (h - 140) 930 116 (hsba 0 0 100 0.08)
       , txt env 36 (h - 100) 20 white ("Sum of defects = " ++ d2 4 total ++ " = 2 pi x " ++ d2 4 chi)
       , txt env 36 (h - 66) 16 white ("-> chi = " ++ show (round chi :: Int) ++ ", holes g = (2 - chi)/2 = " ++ show (round ((2 - chi) / 2) :: Int) ++ "   (does not change when vertices move)")
       , txt env 36 (h - 40) 14 g ("max defect " ++ d2 3 dmax ++ "   min " ++ d2 3 dmin ++ "   (each point changes, the sum does not)")
       , box env (hx - 10) (hy - 30) (hw + 20) (hh + 60) (hsba 0 0 100 0.08) ] ++
       [ box env (hx + fi i / 24 * hw) (hy + hh - bh) (hw / 24 - 1) bh (rgbC (kColor ((fi i + 0.5) / 24 * 2.4 - 1.2) 0.35))
       | i <- [0 .. 23], let bh = fi (bins U.! i) / mx * hh, bh > 0 ] ++
       [ txt env hx (hy - 12) 14 (hsb 0 0 70) "defect histogram (-1.2 to +1.2 rad)" ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> pure w { wPrev = pos, wDrag = pickVertex w pos }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion pos
    | envDown env && wDrag w >= 0 && wDrag w < V.length (wV w) -> pure (dragVertex w pos)
    | envDown env -> let (rx, ry) = dragRot (wPrev w) pos (wRx w, wRy w) in pure w { wRx = rx, wRy = ry, wPrev = pos }
    | otherwise -> pure w { wPrev = pos }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> pure (buildSurface (fromEnum c - fromEnum '0') w)
    | c == 'n' || c == 'N' -> do
        vs <- V.mapM jiggle (wV w)
        pure (refresh w { wV = vs })
    | c == 'l' || c == 'L' -> pure (refresh w { wV = laplacian 0.5 (wFc w) (wV w) })
    | c == 'r' || c == 'R' -> pure (refresh w { wV = wV0 w })
  _ -> pure w

jiggle :: V3 -> IO V3
jiggle (V3 x y z) = do
  dx <- randomRIO (-noiseAmp, noiseAmp)
  dy <- randomRIO (-noiseAmp, noiseAmp)
  dz <- randomRIO (-noiseAmp, noiseAmp)
  pure (V3 (x + dx) (y + dy) (z + dz))

-- いちばん近い頂点をつかむ（16 px 以内）
pickVertex :: World -> Point -> Int
pickVertex w pos =
  let vw = view (wRx w) (wRy w)
      best (bd, bi) i p = let d = dist2 (scr (vw p)) pos in if d < bd then (d, i) else (bd, bi)
  in snd (V.ifoldl' best (16, -1) (wV w))

dragVertex :: World -> Point -> World
dragVertex w pos@(mx, my) =
  let (x0, y0) = wPrev w
      i = wDrag w
      p = wV w V.! i
      V3 _ _ sz = view (wRx w) (wRy w) p
      sc = (camZ - sz) / camZ
      -- 画面の動き（y は下向きに直す）を物体の座標に戻す
      d = invRot (wRx w) (wRy w) (V3 ((mx - x0) * sc) (negate (my - y0) * sc) 0)
      lim = clampF (-400) 400
      V3 a b c = add3 p d
      moved = V3 (lim a) (lim b) (lim c)
  in refresh w { wV = wV w V.// [(i, moved)], wPrev = pos }

-- 画面（カメラ）の座標 → 物体の座標（rotateX, rotateY の逆）
invRot :: Float -> Float -> V3 -> V3
invRot rx ry = rotY (negate ry) . rotX (negate rx)

-- ---------- やってみよう ----------
-- 1. 球面（1）の頂点を 1 つ大きく引っぱると、その頂点の角不足は増え、周りは減る（青くなる）。総和は 4π のまま
-- 2. N を何度も押してぐしゃぐしゃにしても χ は変わらない。L で戻していくと色が白に近づく（曲率が散らばる）
-- 3. 2 つ穴（3）は総和が −4π。「穴が多いほど鞍（青）が多い」のはなぜ？
-- 4. icosphere 2 170 を icosphere 3 170 にして三角形を増やしても総和は同じ（頂点ごとの角不足は小さくなる）

-- ==================================================
-- ここから下は「三角形分割の道具」です
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- | 正二十面体を n 回分割して球に貼った三角形分割
icosphere :: Int -> Float -> (V.Vector V3, [Tri])
icosphere n r =
  let t = (1 + sqrt 5) / 2
      v0 = map (scale3 r . unit3)
             [ V3 (-1) t 0, V3 1 t 0, V3 (-1) (-t) 0, V3 1 (-t) 0, V3 0 (-1) t, V3 0 1 t
             , V3 0 (-1) (-t), V3 0 1 (-t), V3 t 0 (-1), V3 t 0 1, V3 (-t) 0 (-1), V3 (-t) 0 1 ]
      f0 = [ (0, 11, 5), (0, 5, 1), (0, 1, 7), (0, 7, 10), (0, 10, 11), (1, 5, 9), (5, 11, 4), (11, 10, 2), (10, 7, 6), (7, 1, 8)
           , (3, 9, 4), (3, 4, 2), (3, 2, 6), (3, 6, 8), (3, 8, 9), (4, 9, 5), (2, 4, 11), (6, 2, 10), (8, 6, 7), (9, 8, 1) ]
      -- 1 回の分割：辺の中点を作って、三角形を 4 つに分ける
      subdivide (vs, fc) =
        let base = V.fromList vs
            mid (cache, new, cnt) a b =
              let key = (min a b, max a b)
              in case Map.lookup key cache of
                   Just i -> (i, (cache, new, cnt))
                   Nothing ->
                     let m = scale3 r (unit3 (scale3 0.5 (add3 (base V.! a) (base V.! b))))
                     in (cnt, (Map.insert key cnt cache, m : new, cnt + 1))
            cell (st, out) (a, b, c) =
              let (ab, st1) = mid st a b
                  (bc, st2) = mid st1 b c
                  (ca, st3) = mid st2 c a
              in (st3, (a, ab, ca) : (b, bc, ab) : (c, ca, bc) : (ab, bc, ca) : out)
            ((_, newV, _), fc') = foldl' cell ((Map.empty, [], V.length base), []) fc
        in (vs ++ reverse newV, fc')
      (vFinal, fFinal) = iterate subdivide (v0, f0) !! max 0 n
  in (V.fromList vFinal, fFinal)

-- | トーラスの格子三角形分割
torusMesh :: Int -> Int -> Float -> Float -> (V.Vector V3, [Tri])
torusMesh nu nv rr r =
  let fi k m = fromIntegral (k :: Int) / fromIntegral (m :: Int) * tau
      vs = [ V3 ((rr + r * cos v) * cos u) (negate r * sin v) ((rr + r * cos v) * sin u)
           | i <- [0 .. nu - 1], j <- [0 .. nv - 1], let u = fi i nu, let v = fi j nv ]
      idx i j = (i `mod` nu) * nv + (j `mod` nv)
      fc = concat [ [(idx i j, idx (i + 1) j, idx i (j + 1)), (idx (i + 1) j, idx (i + 1) (j + 1), idx i (j + 1))]
                  | i <- [0 .. nu - 1], j <- [0 .. nv - 1] ]
  in (V.fromList vs, fc)

-- | 2 つ穴の曲面：穴が 2 つあいた板を厚みづけして、なめらかにする
genus2 :: Float -> (V.Vector V3, [Tri])
genus2 cs =
  let nx = 7 :: Int
      ny = 3 :: Int
      hole i j = j == 1 && (i == 1 || i == 5)
      -- 頂点の表：(i, j, 上か下か) → 番号
      node (tbl, vs, cnt) key@(i, j, up) = case Map.lookup key tbl of
        Just k -> (k, (tbl, vs, cnt))
        Nothing ->
          let p = V3 ((fromIntegral i - fromIntegral nx / 2) * cs)
                     (if up then negate cs * 1.1 else cs * 1.1)
                     ((fromIntegral j - fromIntegral ny / 2) * cs)
          in (cnt, (Map.insert key cnt tbl, p : vs, cnt + 1))
      cell (st, out) (i, j) =
        let (a, s1) = node st (i, j, True)
            (b, s2) = node s1 (i + 1, j, True)
            (c, s3) = node s2 (i + 1, j + 1, True)
            (d, s4) = node s3 (i, j + 1, True)
            (a2, s5) = node s4 (i, j, False)
            (b2, s6) = node s5 (i + 1, j, False)
            (c2, s7) = node s6 (i + 1, j + 1, False)
            (d2, st1) = node s7 (i, j + 1, False)
            faces = [(a, b, c), (a, c, d), (a2, c2, b2), (a2, d2, c2)]
            -- 境界の辺には壁をつける
            edges = [ (i, j, i + 1, j, i, j - 1), (i + 1, j, i + 1, j + 1, i + 1, j)
                    , (i + 1, j + 1, i, j + 1, i, j + 1), (i, j + 1, i, j, i - 1, j) ]
            filled ni nj = ni >= 0 && nj >= 0 && ni < nx && nj < ny && not (hole ni nj)
            wall (s, acc) (x1, y1, x2, y2, ni, nj)
              | filled ni nj = (s, acc)
              | otherwise =
                  let (p, s1) = node s (x1, y1, True)
                      (q, s2) = node s1 (x2, y2, True)
                      (p2, s3) = node s2 (x1, y1, False)
                      (q2, s4) = node s3 (x2, y2, False)
                  in (s4, (p, q2, q) : (p, p2, q2) : acc)
            (st2, walls) = foldl' wall (st1, []) edges
        in (st2, walls ++ faces ++ out)
      cells = [(i, j) | i <- [0 .. nx - 1], j <- [0 .. ny - 1], not (hole i j)]
      ((_, vsR, _), fc) = foldl' cell ((Map.empty, [], 0 :: Int), []) cells
      vs0 = V.fromList (reverse vsR)
      smoothed = iterate (laplacian 0.6 fc) vs0 !! 5
  in (V.map (scale3 1.35) smoothed, fc)

-- ==================================================
-- ここから下は「3D の道具」です（視点の回転・画家のアルゴリズム・面の陰影）
-- OpenGL の深度バッファの代わりに、奥の絵から順に重ねて描く
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- | カメラまでの距離（p5.js の WEBGL の既定値 (height / 2) / tan(π/6)）
camZ :: Float
camZ = 400 / tan (pi / 6)

-- | 物体の点 → カメラの座標。p5.js の rotateX(rx); rotateY(ry) と同じ順番。y は下向き、z は手前向き
view :: Float -> Float -> V3 -> V3
view rx ry = rotX rx . rotY ry

-- | カメラの座標 → 画面（gloss は y が上向きなので反転する）
scr :: V3 -> Point
scr v = let (x, y) = project camZ v in (x, negate y)

-- | 奥行きつきの絵（奥行きは z、大きいほど手前）
type Item = (Float, Picture)

-- | 奥から順に並べて 1 枚の絵にする（画家のアルゴリズム）
paint :: [Item] -> Picture
paint = Pictures . map snd . depthSort fst

-- | 面より少し手前に描くためのずらし（線や点が面に隠れないように）
lineBias :: Float
lineBias = 3

zOf :: V3 -> Float
zOf (V3 _ _ z) = z

-- | カメラの座標の線分（w は太さ、1 以下なら細い線）
seg :: Color -> Float -> V3 -> V3 -> Item
seg c w a b =
  ( (zOf a + zOf b) / 2 + lineBias
  , Color c (if w <= 1 then Line [scr a, scr b] else thickLine w (scr a) (scr b)) )

-- | 長い線分を n 個に切って描く（途中で面に隠れる線になる）
longSeg :: Int -> Color -> Float -> V3 -> V3 -> [Item]
longSeg n c w a b = pathItems c w [add3 a (scale3 (fromIntegral i / fromIntegral n) (sub3 b a)) | i <- [0 .. n]]

-- | カメラの座標の折れ線
pathItems :: Color -> Float -> [V3] -> [Item]
pathItems c w ps = zipWith (seg c w) ps (drop 1 ps)

-- | カメラの座標の小さな球（p5.js の sphere。画面では円）
ball :: Color -> Float -> V3 -> Item
ball c r v = (zOf v + lineBias, at (scr v) (Color c (disc (r * camZ / max 1 (camZ - zOf v)))))

-- | 面：4 つの頂点の番号（周の順、三角形なら最後を 2 回）と色
data Face = Face !Int !Int !Int !Int !Float !Float !Float

-- | 頂点の配列と面のリスト
data Mesh = Mesh !(V.Vector V3) [Face]

-- | 格子のメッシュ。pos i j が頂点の位置、col i j が頂点の色（面の色は 4 頂点の平均）
gridMesh :: Int -> Int -> (Int -> Int -> V3) -> (Int -> Int -> (Float, Float, Float)) -> Mesh
gridMesh nu nv pos col =
  Mesh (V.fromList [pos i j | i <- [0 .. nu], j <- [0 .. nv]])
       [ let (r1, g1, b1) = col i j
             (r2, g2, b2) = col (i + 1) j
             (r3, g3, b3) = col (i + 1) (j + 1)
             (r4, g4, b4) = col i (j + 1)
         in Face (ix i j) (ix (i + 1) j) (ix (i + 1) (j + 1)) (ix i (j + 1))
                 ((r1 + r2 + r3 + r4) / 4) ((g1 + g2 + g3 + g4) / 4) ((b1 + b2 + b3 + b4) / 4)
       | i <- [0 .. nu - 1], j <- [0 .. nv - 1] ]
  where ix i j = i * (nv + 1) + j

-- | 面を陰影つきの絵にする。vs は回転ずみの頂点、amb は環境光の強さ（両面に光が当たる）
faceItem :: Float -> V.Vector V3 -> Face -> Item
faceItem amb vs (Face ia ib ic id' r g b) =
  let pa = vs V.! ia
      pb = vs V.! ib
      pc = vs V.! ic
      pd = vs V.! id'
      cen = scale3 0.25 (add3 pa (add3 pb (add3 pc pd)))
      n0 = unit3 (cross3 (sub3 pc pa) (sub3 pd pb))
      n = if dot3 n0 (sub3 (V3 0 0 camZ) cen) < 0 then scale3 (-1) n0 else n0
      s = amb + 0.75 * max 0 (dot3 n light1) + 0.31 * max 0 (dot3 n light2)
      tone v = min 1 (v * s)
  in (zOf cen, Color (makeColor (tone r) (tone g) (tone b) 1) (Polygon [scr pa, scr pb, scr pc, scr pd]))

-- | 光の向き（p5.js の directionalLight の逆向き）
light1, light2 :: V3
light1 = unit3 (V3 0.4 (-0.6) 1)
light2 = unit3 (V3 (-0.5) 0.4 (-0.5))

-- | メッシュ全体を絵のリストにする
meshItems :: Float -> V.Vector V3 -> [Face] -> [Item]
meshItems amb vs = map (faceItem amb vs)

-- | ドラッグで視点を回す（p5.js の mouseDragged と同じ）
dragRot :: Point -> Point -> (Float, Float) -> (Float, Float)
dragRot (x0, y0) (x1, y1) (rx, ry) = (clampF (-1.6) 1.6 (rx - (y1 - y0) * 0.01), ry + (x1 - x0) * 0.01)

-- | 曲率 → 色（+ は赤、0 は白、− は青）
kColor :: Float -> Float -> (Float, Float, Float)
kColor v s =
  let t = tanh (v / s)
  in if t > 0 then (1, 1 - 0.85 * t, 1 - 0.85 * t) else (1 + 0.8 * t, 1 + 0.6 * t, 1)

-- | (r, g, b) → gloss の色
rgbC :: (Float, Float, Float) -> Color
rgbC (r, g, b) = makeColor r g b 1

-- ==================================================
-- ここから下は「座標と文字の道具」です
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- | p5.js の座標（左上が原点、y は下向き）→ gloss の座標（中央が原点、y は上向き）
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- | gloss の座標 → p5.js の座標（マウスの位置を読むときに使う）
fromG :: Env -> Point -> Point
fromG env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- | p5.js の座標で文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str

-- | p5.js の座標で長方形を塗る（(x, y) は左上）
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (gx, gy) = toG env (x + w / 2, y + h / 2) in Color c (rect gx gy w h)

-- | p5.js の座標で線分を引く（w は太さ、1 以下なら細い線）
seg2 :: Env -> Color -> Float -> Point -> Point -> Picture
seg2 env c w a b
  | w <= 1 = Color c (Line [toG env a, toG env b])
  | otherwise = Color c (thickLine w (toG env a) (toG env b))

-- | p5.js の座標で折れ線を引く（色は 1 色）
path2 :: Env -> Color -> Float -> [Point] -> Picture
path2 env c w ps
  | w <= 1 = Color c (Line (map (toG env) ps))
  | otherwise = Color c (thickPath w (map (toG env) ps))

-- | p5.js の座標で点（塗りつぶした円）を打つ。r は半径
dotAt :: Env -> Color -> Float -> Point -> Picture
dotAt env c r p = at (toG env p) (Color c (disc r))

-- | p5.js の座標で輪を描く。r は半径、w は線の太さ
ringAt :: Env -> Color -> Float -> Float -> Point -> Picture
ringAt env c r w p
  | w <= 1 = at (toG env p) (Color c (Circle r))
  | otherwise = at (toG env p) (Color c (ring r w))

-- | 小数 n 桁の文字列（p5.js の nf に相当）
nf :: Int -> Float -> String
nf n x = showFFloat (Just n) x ""
