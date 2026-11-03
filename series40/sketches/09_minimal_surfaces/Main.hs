-- ============================================
-- 40-09 極小曲面 ─ シャボン膜は面積を最小にする
-- 針金の枠（境界）を固定して、曲面を「平均曲率 H に比例して法線方向へ動かす」（平均曲率流）と面積がどんどん減り、
--   H = 0 の曲面 = 極小曲面（シャボン膜）に落ち着く。メッシュでは cot 重みのラプラシアンで H n を近似する
--   2 つの輪 → カテノイド。ただし輪の間隔が半径の約 1.33 倍を超えると膜は存在せず、くびれて切れる
-- 操作：1〜3 = 枠 / ↑↓ = 枠の形（輪の間隔・ねじれ・波の高さ）/ スペース = 一時停止 / W = ワイヤー / R = 最初から / ドラッグ = 視点を回す
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Numeric (showFFloat)

r0 :: Float
r0 = 120                 -- ★ 輪の半径（px）

iters :: Int
iters = 3                -- ★ 1 フレームの流れの回数

lambda :: Float
lambda = 0.5             -- ★ 1 回の強さ（0〜1）

data World = World
  { wFrameNo :: !Int
  , wParam   :: !Float            -- 輪の半間隔 d（1）、ねじれの高さ（2）、波の高さ（3）
  , wNu, wNv :: !Int
  , wWrap    :: !Bool
  , wP       :: !(U.Vector Float) -- 頂点（x, y, z を順に並べた配列）
  , wFixed   :: !(U.Vector Bool)  -- 境界なら True
  , wFaces   :: !(U.Vector Int)   -- 三角形（頂点番号を 3 個ずつ）
  , wHval    :: !(U.Vector Float) -- 各頂点の |H|
  , wPaused, wWire, wBroken :: !Bool
  , wSteps   :: !Int
  , wRx, wRy :: !Float
  , wPrev    :: !Point }

main :: IO ()
main = runSketch (sketch "40-09 Minimal surfaces" (buildFrame world0))
  { skBack  = hsb 200 30 9
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 60 40 24 True U.empty U.empty U.empty U.empty False False False 0 (-0.35) 0.5 (0, 0)

-- ★ 枠と最初の曲面
buildFrame :: World -> World
buildFrame w =
  let par = wParam w
      fi k = fromIntegral (k :: Int) :: Float
      (nu, nv, wrap, verts) = case wFrameNo w of
        1 -> -- 2 つの輪：最初は円柱
          ( 40, 24, True
          , [ ([r0 * cos a, z, r0 * sin a], j == 0 || j == 24)
            | i <- [0 .. 39], j <- [0 .. 24]
            , let a = fi i / 40 * tau, let z = negate par + 2 * par * fi j / 24 ] )
        2 -> -- ねじれた四角：角が上下交互、辺はまっすぐ
          ( 30, 30, False
          , [ ([150 * u, negate par * u * v, 150 * v], i == 0 || i == 30 || j == 0 || j == 30)
            | i <- [0 .. 30], j <- [0 .. 30]
            , let u = -1 + 2 * fi i / 30, let v = -1 + 2 * fi j / 30 ] )
        _ -> -- 波打つ輪：円板、境界の高さが sin 3θ
          ( 30, 30, False
          , [ ([1.3 * r0 * x, negate par * sin (3 * th) * r * r * r, 1.3 * r0 * y], i == 0 || i == 30 || j == 0 || j == 30)
            | i <- [0 .. 30], j <- [0 .. 30]
            , let u = -1 + 2 * fi i / 30, let v = -1 + 2 * fi j / 30
            , let x = u * sqrt (1 - v * v / 2), let y = v * sqrt (1 - u * u / 2)
            , let r = norm2 (x, y), let th = atan2 y x ] )
      nU = if wrap then nu else nu + 1
      ident i j = (i `mod` nU) * (nv + 1) + j
      faces = concat [ [ident i j, ident (i + 1) j, ident i (j + 1), ident (i + 1) j, ident (i + 1) (j + 1), ident i (j + 1)]
                     | i <- [0 .. nu - 1], j <- [0 .. nv - 1] ]
  in w { wNu = nu, wNv = nv, wWrap = wrap
       , wP = U.fromList (concatMap fst verts)
       , wFixed = U.fromList (map snd verts)
       , wFaces = U.fromList faces
       , wHval = U.replicate (length verts) 0
       , wBroken = False, wSteps = 0 }

-- 平均曲率流の 1 歩（cot 重みのラプラシアン：Σ w_ij (p_j − p_i) ≈ −2 A H n）
flowStep :: U.Vector Bool -> U.Vector Int -> (U.Vector Float, U.Vector Float) -> (U.Vector Float, U.Vector Float)
flowStep fixed faces (p, _) = runST $ do
  let n = U.length fixed
      nf3 = U.length faces `div` 3
      px i = p U.! (3 * i)
      py i = p U.! (3 * i + 1)
      pz i = p U.! (3 * i + 2)
  acc <- M.replicate (3 * n) (0 :: Float)
  wsum <- M.replicate n (0 :: Float)
  area <- M.replicate n (0 :: Float)
  forM_ [0 .. nf3 - 1] $ \f -> do
    let a = faces U.! (3 * f)
        b = faces U.! (3 * f + 1)
        c = faces U.! (3 * f + 2)
        -- 辺 (i, j) の向かいの角 k
        corner i j k = do
          let ux = px i - px k
              uy = py i - py k
              uz = pz i - pz k
              vx = px j - px k
              vy = py j - py k
              vz = pz j - pz k
              cx = uy * vz - uz * vy
              cy = uz * vx - ux * vz
              cz = ux * vy - uy * vx
              cot = (ux * vx + uy * vy + uz * vz) / (sqrt (cx * cx + cy * cy + cz * cz) + 1e-9)
              w = clampF 0.02 30 cot                                  -- ★ 鈍角での暴走を防ぐ
              dx = w * (px j - px i)
              dy = w * (py j - py i)
              dz = w * (pz j - pz i)
          M.modify acc (+ dx) (3 * i)
          M.modify acc (+ dy) (3 * i + 1)
          M.modify acc (+ dz) (3 * i + 2)
          M.modify acc (subtract dx) (3 * j)
          M.modify acc (subtract dy) (3 * j + 1)
          M.modify acc (subtract dz) (3 * j + 2)
          M.modify wsum (+ w) i
          M.modify wsum (+ w) j
    corner a b c
    corner b c a
    corner c a b
    let ax = px b - px a
        ay = py b - py a
        az = pz b - pz a
        bx = px c - px a
        by = py c - py a
        bz = pz c - pz a
        qx = ay * bz - az * by
        qy = az * bx - ax * bz
        qz = ax * by - ay * bx
        aThird = 0.5 * sqrt (qx * qx + qy * qy + qz * qz) / 3
    M.modify area (+ aThird) a
    M.modify area (+ aThird) b
    M.modify area (+ aThird) c
  out <- U.thaw p
  hv <- M.replicate n (0 :: Float)
  forM_ [0 .. n - 1] $ \i -> do
    ax <- M.read acc (3 * i)
    ay <- M.read acc (3 * i + 1)
    az <- M.read acc (3 * i + 2)
    ws <- M.read wsum i
    ar <- M.read area i
    M.write hv i (sqrt (ax * ax + ay * ay + az * az) / (2 * ar + 1e-9))     -- |H| の近似
    when (not (fixed U.! i) && ws /= 0) $ do
      M.modify out (+ lambda * ax / ws) (3 * i)
      M.modify out (+ lambda * ay / ws) (3 * i + 1)
      M.modify out (+ lambda * az / ws) (3 * i + 2)
  p' <- U.unsafeFreeze out
  h' <- U.unsafeFreeze hv
  pure (p', h')

vertexOf :: U.Vector Float -> Int -> V3
vertexOf p i = V3 (p U.! (3 * i)) (p U.! (3 * i + 1)) (p U.! (3 * i + 2))

totalArea :: World -> Float
totalArea w =
  let p = wP w
      fs = wFaces w
      tri s f =
        let a = vertexOf p (fs U.! (3 * f))
            b = vertexOf p (fs U.! (3 * f + 1))
            c = vertexOf p (fs U.! (3 * f + 2))
        in s + 0.5 * norm3 (cross3 (sub3 b a) (sub3 c a))
  in foldl' tri 0 [0 .. U.length fs `div` 3 - 1]

neckRadius :: World -> Float
neckRadius w =
  let p = wP w
      n = U.length (wFixed w)
  in foldl' (\m i -> if wFixed w U.! i then m else min m (norm2 (p U.! (3 * i), p U.! (3 * i + 2)))) 1e9 [0 .. n - 1]

-- 2 つの輪にかかるカテノイドの理論解 R(z) = c cosh(z/c)（R0 = c cosh(d/c) を満たす大きい方の c）
catenoidC :: Float -> Maybe Float
catenoidC d =
  let lo0 = d / 1.2
      f c = c * cosh (d / c) - r0
      go :: Int -> Float -> Float -> Float
      go 0 lo hi = (lo + hi) / 2
      go k lo hi = let m = (lo + hi) / 2 in if f m > 0 then go (k - 1) lo m else go (k - 1) m hi
  in if f lo0 > 0 then Nothing else Just (go 50 lo0 r0)

step :: Env -> World -> World
step env w0 =
  let up = isDown env (SpecialKey KeyUp)
      down = isDown env (SpecialKey KeyDown)
      w1 | up = buildFrame w0 { wParam = min 160 (wParam w0 + 1.5) }
         | otherwise = w0
      w2 | down = buildFrame w1 { wParam = max (if wFrameNo w1 == 1 then 20 else 0) (wParam w1 - 1.5) }
         | otherwise = w1
      w3 | wPaused w2 || wBroken w2 = w2
         | otherwise =
             let (p', h') = iterate (flowStep (wFixed w2) (wFaces w2)) (wP w2, wHval w2) !! iters
             in w2 { wP = p', wHval = h', wSteps = wSteps w2 + iters }
  in if wFrameNo w3 == 1 && neckRadius w3 < 7 then w3 { wBroken = True } else w3

draw :: Env -> World -> Picture
draw env w =
  let vw = view (wRx w) (wRy w)
      nPts = U.length (wFixed w)
      vs = V.generate nPts (vw . vertexOf (wP w))
      nu = wNu w
      nv = wNv w
      nU = if wWrap w then nu else nu + 1
      ident i j = (i `mod` nU) * (nv + 1) + j
      fs = wFaces w
      colOf i = let t = min 1 (wHval w U.! i * 60) in (0.75 + 0.25 * t, 0.85 - 0.55 * t, 1 - 0.7 * t)
      tri f =
        let a = fs U.! (3 * f)
            b = fs U.! (3 * f + 1)
            c = fs U.! (3 * f + 2)
            (r1, g1, b1) = colOf a
            (r2, g2, b2) = colOf b
            (r3, g3, b3) = colOf c
        in faceItem 0.33 vs (Face a b c c ((r1 + r2 + r3) / 3) ((g1 + g2 + g3) / 3) ((b1 + b2 + b3) / 3))
      faces = if wWire w then [] else map tri [0 .. U.length fs `div` 3 - 1]
      -- 格子線（ワイヤー、または薄く）
      lc = hsba 0 0 100 (if wWire w then 0.8 else 0.25)
      lw = if wWire w then 1.2 else 0.8
      grid = [ seg lc lw (vs V.! ident i j) (vs V.! ident i (j + 1)) | i <- [0, 2 .. nU - 1], j <- [0 .. nv - 1] ]
          ++ [ seg lc lw (vs V.! ident i j) (vs V.! ident (i + 1) j) | j <- [0, 2 .. nv], i <- [0 .. nu - 1] ]
      -- 固定した枠（針金）
      wireCol = hsb 45 90 100
      edge a b = seg wireCol 3 (vs V.! a) (vs V.! b)
      frameE = concat [ [edge (ident i 0) (ident (i + 1) 0), edge (ident i nv) (ident (i + 1) nv)] | i <- [0 .. nu - 1] ]
            ++ (if wWrap w then [] else concat [ [edge (ident 0 j) (ident 0 (j + 1)), edge (ident nu j) (ident nu (j + 1))] | j <- [0 .. nv - 1] ])
      -- 理論解のカテノイドの断面（存在するとき）
      theory = case catenoidC (wParam w) of
        Just c | wFrameNo w == 1 ->
          concat [ pathItems (hsba 130 80 100 0.8) 2
                     [ vw (V3 (r * cos a) z (r * sin a))
                     | j <- [0 .. 40 :: Int]
                     , let z = negate (wParam w) + 2 * wParam w * fromIntegral j / 40
                     , let r = c * cosh (z / c) ]
                 | a <- [0, pi] ]
        _ -> []
  in Pictures [paint (faces ++ grid ++ frameE ++ theory), hud env w]

hud :: Env -> World -> Picture
hud env w =
  let par = wParam w
      n = max 1 (U.length (wHval w))
      avgH = U.sum (wHval w) / fromIntegral n
      g = hsb 0 0 65
      (name, what) = case wFrameNo w of
        1 -> ("Two rings", "half gap d = " ++ nf 0 par ++ " px")
        2 -> ("Twisted square", "twist height " ++ nf 0 par)
        _ -> ("Wavy ring", "wave height " ++ nf 0 par)
      catLine = case catenoidC par of
        Just c -> "green = theory R(z) = c cosh(z/c) (c = " ++ nf 1 c ++ ")"
        Nothing -> "no solution at this gap (2d/R0 = " ++ nf 2 (2 * par / r0) ++ " > 1.33)!"
  in Pictures $
       [ txt env 20 40 16 white ("1-3: frame (" ++ name ++ ")   Up/Down: " ++ what ++ "   Space: " ++ (if wPaused w then "resume" else "pause"))
       , txt env 20 62 16 white "W: wire   R: restart   drag: view"
       , txt env 20 86 14 g ("yellow = fixed wire   color = |H| (white 0, red large)   flow steps " ++ show (wSteps w))
       , txt env 20 106 14 g ("area = " ++ nf 1 (totalArea w / 1000) ++ " x10^3 px^2   mean |H| = " ++ nf 2 (avgH * 1000) ++ " x10^-3")
       ] ++
       (if wFrameNo w == 1
          then [ txt env 20 126 14 g (catLine ++ "   neck radius " ++ nf 1 (neckRadius w) ++ " px") ] ++
               [ txt env 20 160 22 (hsb 0 85 100) "Broken! Two flat disks have less area (bring the rings closer, then R)" | wBroken w ]
          else [])

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> buildFrame w { wFrameNo = fromEnum c - fromEnum '0', wParam = 60 }
    | c == 'w' || c == 'W' -> w { wWire = not (wWire w) }
    | c == 'r' || c == 'R' -> buildFrame w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (MouseButton LeftButton) Down _ pos -> w { wPrev = pos }
  EventMotion pos
    | envDown env -> let (rx, ry) = dragRot (wPrev w) pos (wRx w, wRy w) in w { wRx = rx, wRy = ry, wPrev = pos }
    | otherwise -> w { wPrev = pos }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 2 つの輪（1）：d を少しずつ大きくして、膜が切れる境目を探そう。理論値は 2d/R0 ≈ 1.33（d ≈ 80 px）
-- 2. ねじれた四角（2）はシェルクの曲面の一部に似た鞍になる。ねじれを 0 にすると平面。確かめよう
-- 3. 波打つ輪（3）の sin(3θ) を sin(2θ) や sin(4θ) に変えると？（エンネパー曲面との関係）
-- 4. lambda を 1.0 にすると速いが暴れる。iters を 10 にして収束の速さを見よう。面積がどう減るか記録してみよう

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
