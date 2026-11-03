-- ============================================
-- 12 毛玉の定理（3D）
-- 球に生えた毛を、どうとかしても（連続な向きのそろえ方 ＝ ベクトル場）必ずどこかに「つむじ」（向きが決まらない点）ができる
--   ドーナツ（トーラス）なら、つむじなしで全部とかせる。つむじの「回り方の数」（指数）の合計 ＝ オイラー標数（球 2、トーラス 0）
-- 1 = 東向きの風 / 2 = 北から南へ / 3 = つむじ 1 つ / 4 = ランダムにとかす（もう一度押すと別の向き） / T = トーラス / スペース = 流れる粒 / ドラッグ = 視点
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', intercalate)
import qualified Data.Array as A

nHair, nParts :: Int
nHair = 900          -- ★ 毛の本数
nParts = 160         -- 流れる粒の数

hairLen :: Float
hairLen = 0.16       -- ★ 毛の長さ（半径 = 1）

width, height, rs, camD :: Float
width = 1280
height = 800
rs = min width height * 0.3
camD = 693

-- | 流れる粒
data Part = Part { qU, qV :: !Float, qP :: !V3, qLife :: !Float }

data World = World
  { wField :: !Int
  , wTorus :: !Bool
  , wPhases :: ![Float]
  , wZeros :: ![(V3, Int)]        -- つむじの位置と指数
  , wVecs :: [(V3, V3, Float)]    -- 毛の根元、向き、長さ（場を変えたときに作り直す）
  , wVmax :: Float
  , wParts :: ![Part]
  , wFlow :: !Bool
  , wRotX, wRotY :: !Float
  , wPrev :: !(Float, Float)
  }

main :: IO ()
main = runSketch (sketch "27-12 Hairy Ball Theorem" (World 1 False [] [] [] 1 [] True (-0.4) 0 (0, 0)))
  { skBack  = rgb255 16 20 26
  , skStart = \_ -> do
      ph <- randomPhases
      ps <- mapM (const newPart) [1 .. nParts]
      pure (refresh (World 1 False ph [] [] 1 ps True (-0.4) 0 (0, 0)))
  , skStep  = stepWorld
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \_ ev w -> onEvent ev w
  }

randomPhases :: IO [Float]
randomPhases = mapM (const (randomRIO (0, 6.283))) [1 .. 9 :: Int]

-- | フィボナッチ球面：ほぼ等間隔の点
hairs :: A.Array Int V3
hairs = A.listArray (0, nHair - 1)
  [ V3 (r * cos a) y (r * sin a)
  | i <- [0 .. nHair - 1]
  , let y = 1 - 2 * (fromIntegral i + 0.5) / fromIntegral nHair
  , let r = sqrt (max 0 (1 - y * y))
  , let a = fromIntegral i * pi * (3 - sqrt 5) ]

randomHair :: IO V3
randomHair = (hairs A.!) <$> randomRIO (0, nHair - 1)

newPart :: IO Part
newPart = do
  u <- randomRIO (0, 6.283)
  v <- randomRIO (0, 6.283)
  p <- randomHair
  l <- randomRIO (0, 200)
  pure (Part u v p l)

-- | ★ 球面上のベクトル場：点 p（|p| = 1）での毛の向き（接ベクトル）
field :: Int -> [Float] -> V3 -> V3
field no ph p@(V3 x y z) = case (no, ph) of
  (1, _) -> V3 z 0 (-x)                        -- 縦軸のまわりに回る風
  (2, _) -> tangent p (V3 0 1 0)               -- 上（北）から下（南）へ
  (3, _) ->                                    -- 平面の一様な流れを球にのせる（下の極の 1 点だけがつむじ）
    let k = 1 + y
        k' = if k == 0 then 1e-9 else k
        xx = x / k'
        zz = z / k'
        r2 = xx * xx + zz * zz
        d = (1 + r2) * (1 + r2)
    in if k < 1e-6 then V3 0 0 0
       else scale3 2 (V3 (2 * (1 + r2 - 2 * xx * xx) / d) (-4 * xx / d) (-4 * xx * zz / d))
  (_, [p0, p1, p2, p3, p4, p5, p6, p7, p8]) ->  -- なめらかなランダムの風を球に射影
    tangent p (V3 (sin (2 * y + p0) + sin (3 * z + p1) + 0.6 * cos (x + p2))
                  (sin (2 * z + p3) + sin (3 * x + p4) + 0.6 * cos (y + p5))
                  (sin (2 * x + p6) + sin (3 * y + p7) + 0.6 * cos (z + p8)))
  _ -> V3 0 0 0

tangent :: V3 -> V3 -> V3
tangent p f = sub3 f (scale3 (dot3 f p) p)

-- | 場を変えたときに、毛の向きとつむじを計算し直す
refresh :: World -> World
refresh w =
  let fn = field (wField w) (wPhases w)
      vecs = [ (p, v, norm3 v) | p <- A.elems hairs, let v = fn p ]
      vmax = foldl' (\a (_, _, m) -> max a m) 1e-9 vecs
  in w { wVecs = vecs, wVmax = vmax, wZeros = if wTorus w then [] else findZeros fn }

-- | つむじ（|v| がとても小さい所）を探し、まわりを 1 周したときの毛の回転数（指数）を数える
findZeros :: (V3 -> V3) -> [(V3, Int)]
findZeros fn =
  let na = 36 :: Int
      nb = 72 :: Int
      pts = A.listArray ((0, 0), (na, nb - 1))
              [ V3 (sin th * cos ph) (cos th) (sin th * sin ph)
              | i <- [0 .. na], j <- [0 .. nb - 1]
              , let th = fromIntegral i / fromIntegral na * pi
              , let ph = fromIntegral j / fromIntegral nb * tau ] :: A.Array (Int, Int) V3
      mag = fmap (norm3 . fn) pts
      vmax = maximum (A.elems mag)
      isMin i j =
        let m = mag A.! (i, j)
        in m < vmax * 0.1 &&
           and [ mag A.! (ii, (j + dj + nb) `mod` nb) >= m
               | di <- [-1, 0, 1], dj <- [-1, 0, 1]
               , let ii = i + di, ii >= 0, ii <= na, not (di == 0 && dj == 0) ]
      add zs (i, j)
        | not (isMin i j) = zs
        | any (\(z, _) -> norm3 (sub3 z p) < 0.3) zs = zs
        | idx /= 0 = zs ++ [(p, idx)]         -- 指数 0 は「弱いだけの所」なので除く
        | otherwise = zs
        where p = pts A.! (i, j)
              idx = indexAt fn p
  in foldl' add [] [(i, j) | i <- [0 .. na], j <- [0 .. nb - 1]]

indexAt :: (V3 -> V3) -> V3 -> Int
indexAt fn p0@(V3 _ y0 _) =
  let e1 = unit3 (cross3 p0 (if abs y0 < 0.9 then V3 0 1 0 else V3 1 0 0))
      e2 = cross3 p0 e1
      angle k =
        let t = fromIntegral (k :: Int) / 64 * tau
            q = unit3 (add3 p0 (scale3 0.2 (add3 (scale3 (cos t) e1) (scale3 (sin t) e2))))
            v = fn q
        in atan2 (dot3 v e2) (dot3 v e1)
      as = map angle [0 .. 64]
      delta a b =
        let d0 = b - a
            d1 = if d0 > pi then d0 - tau else d0
        in if d1 < -pi then d1 + tau else d1
  in round (sum (zipWith delta as (drop 1 as)) / tau)

-- | トーラス上の点と、ぐるっと回る向きの場（つむじなし）
torusPt :: Float -> Float -> V3
torusPt u v = V3 ((1 + 0.4 * cos v) * cos u) (0.4 * sin v) ((1 + 0.4 * cos v) * sin u)

torusN :: Float -> Float -> V3
torusN u v = V3 (cos v * cos u) (sin v) (cos v * sin u)

---------------------------------------------------------------- 動き

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  let m@(mx, my) = toP5 (envMouse env)
      (px, py) = wPrev w0
      w1 | envDown env = w0 { wRotY = wRotY w0 + (mx - px) * 0.01, wRotX = clampF (-1.5) 1.5 (wRotX w0 - (my - py) * 0.01) }
         | otherwise = w0 { wRotY = wRotY w0 + 0.003 }
      w2 = w1 { wPrev = m }
      fn = field (wField w2) (wPhases w2)
      vmax = wVmax w2
      moveS q = do
        let v = fn (qP q)
            m0 = norm3 v
            mg = if m0 == 0 then 1 else m0
            p' = unit3 (add3 (qP q) (scale3 (0.012 * min 1 (mg / vmax * 3) / mg) v))
            life = qLife q - 1
        if life < 0
          then do
            h <- randomHair
            pure q { qP = h, qLife = 200 }
          else pure q { qP = p', qLife = life }
      moveT q = pure q { qU = qU q + 0.01, qV = qV q + 0.002 }
  if not (wFlow w2) then pure w2
    else do
      ps <- mapM (if wTorus w2 then moveT else moveS) (wParts w2)
      pure w2 { wParts = ps }

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> do
        ph <- if c == '4' && wField w == 4 then randomPhases else pure (wPhases w)
        pure (refresh w { wPhases = ph, wField = fromEnum c - fromEnum '0', wTorus = False })
    | c `elem` "tT" -> pure (refresh w { wTorus = not (wTorus w) })
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wFlow = not (wFlow w) }
  _ -> pure w

---------------------------------------------------------------- 描画

viewFn :: World -> V3 -> V3
viewFn w =
  let cx = cos (wRotX w)
      sx = sin (wRotX w)
      cy = cos (wRotY w)
      sy = sin (wRotY w)
  in \(V3 x y z) ->
       let x1 = x * cy + z * sy
           z1 = z * cy - x * sy
       in V3 x1 (y * cx - z1 * sx) (y * sx + z1 * cx)

scr :: V3 -> Point
scr v = let (x, y) = project camD v in (x, -y)

zOf :: V3 -> Float
zOf (V3 _ _ z) = z

lightDir :: V3
lightDir = unit3 (V3 0.3 0.6 (-0.7))

-- | 色 (70, 60, 90) の面を明るさ b で塗る色
bodyCol :: Float -> Color
bodyCol b = makeColor (min 1 (70 / 255 * b)) (min 1 (60 / 255 * b)) (min 1 (90 / 255 * b)) 1

drawWorld :: World -> Picture
drawWorld w = Pictures [if wTorus w then drawTorusHair w else drawSphereHair w, drawHud w]

-- | 球と毛。視線から見て裏にある毛・粒・玉は球に隠れるので描かない
drawSphereHair :: World -> Picture
drawSphereHair w =
  let view = viewFn w
      vmax = wVmax w
      -- 透視で見える球のふちの半径
      rr = rs * 0.99
      rEdge = rr * camD / sqrt (camD * camD - rr * rr)
      -- 球：光の側へ少しずつずらした円を重ねて陰影をつける
      V3 lx ly _ = lightDir
      layer i =
        let t = fromIntegral (i :: Int) / 23
            r = rEdge * (1 - t * 0.96)
            off = rEdge * t * 0.75
        in at (-lx * off, ly * off) (Color (bodyCol (100 / 255 + t * 0.95)) (disc r))
      hairPic (p, v, m) =
        let V3 _ _ nz = view p
        in if nz < 0.3 then Blank else
           let s = min 1 (m / vmax * 2.5)
               l = hairLen * s / (if m == 0 then 1 else m)
               a = view (scale3 rs p)
               b = view (scale3 rs (add3 (scale3 1.04 p) (scale3 l v)))
           in Color (makeColor 1 ((120 + 120 * s) / 255) ((60 + 100 * (1 - s)) / 255) 1) (thickLine 2 (scr a) (scr b))
      partPic q =
        let n@(V3 _ _ nz) = view (qP q)
        in if nz < 0.3 then Blank
           else at (scr (scale3 (rs * 1.05) n)) (Color (rgb255 120 230 255) (disc 3))
      zeroPic (p, _) =
        let n@(V3 _ _ nz) = view p
            v = scale3 (rs * 1.03) n
            k = camD / max 1 (camD - zOf v)
        in if nz < 0.2 then Blank else at (scr v) (Color (rgb255 255 60 60) (disc (10 * k)))
  in Pictures $
       map layer [0 .. 23]
       ++ map hairPic (wVecs w)
       ++ (if wFlow w then map partPic (wParts w) else [])
       ++ map zeroPic (wZeros w)

drawTorusHair :: World -> Picture
drawTorusHair w =
  let view = viewFn w
      sS = rs * 0.85
      nu = 48 :: Int
      nv = 24 :: Int
      front n = let V3 _ _ z = view n in z > -0.2
      surfP u v = view (scale3 sS (add3 (torusPt u v) (scale3 (-0.008) (torusN u v))))
      quad i j =
        let u0 = fromIntegral i / fromIntegral nu * tau
            u1 = fromIntegral (i + 1) / fromIntegral nu * tau
            v0 = fromIntegral j / fromIntegral nv * tau
            v1 = fromIntegral (j + 1) / fromIntegral nv * tau
            n = torusN ((u0 + u1) / 2) ((v0 + v1) / 2)
        in if not (front n) then [] else
           let a = surfP u0 v0
               b = surfP u1 v0
               c = surfP u1 v1
               d = surfP u0 v1
               dif = max 0 (negate (dot3 (view n) lightDir))
           in [((zOf a + zOf b + zOf c + zOf d) / 4, Color (bodyCol (100 / 255 + dif)) (Polygon (map scr [a, b, c, d])))]
      hairC = rgb255 255 220 120
      hair i j =
        let u = fromIntegral i / 64 * tau
            v = fromIntegral j / 16 * tau
            n = torusN u v
        in if not (front n) then [] else
           let a = view (scale3 sS (add3 (torusPt u v) (scale3 0.02 n)))
               b = view (scale3 sS (add3 (torusPt (u + 0.1) v) (scale3 0.06 n)))
           in [((zOf a + zOf b) / 2 + 6, Color hairC (thickLine 2 (scr a) (scr b)))]
      partPic q =
        let n = torusN (qU q) (qV q)
        in if not (front n) then [] else
           let a = view (scale3 sS (add3 (torusPt (qU q) (qV q)) (scale3 0.05 n)))
           in [(zOf a + 8, at (scr a) (Color (rgb255 120 230 255) (disc 3)))]
      items = concat [quad i j | i <- [0 .. nu - 1], j <- [0 .. nv - 1]]
              ++ concat [hair i j | i <- [0 .. 63 :: Int], j <- [0 .. 15 :: Int]]
              ++ (if wFlow w then concatMap partPic (wParts w) else [])
  in Pictures (map snd (depthSort fst items))

drawHud :: World -> Picture
drawHud w =
  let y0 = height - 120
      zeros = wZeros w
      sign i = (if i > 0 then "+" else "") ++ show i
      sumI = sum (map snd zeros)
      top = if wTorus w
              then [txt 20 y0 22 (rgb255 255 230 120) "Torus: 0 swirls (chi = 0)"]
              else [ txt 20 y0 22 (rgb255 255 230 120)
                       ("Sphere: " ++ show (length zeros) ++ (if length zeros == 1 then " swirl" else " swirls") ++ "   index " ++ intercalate ", " (map (sign . snd) zeros) ++ "   sum " ++ show sumI)
                   , txt 20 (y0 + 36) 15 (gray 1) "The index sum is always 2 = chi of the sphere (Poincare-Hopf), so you can never have 0 swirls." ]
  in Pictures $
       [ txt 20 40 15 (gray 1) "1: east wind  2: north-south  3: one swirl  4: random (again: new)  T: torus  Space: flow  Drag: view"
       , txt 20 68 14 (gray (170 / 255)) "Hair length = flow strength. Short, dark = swirl (red ball). On a torus, combing east-west removes all swirls." ]
       ++ top
       ++ [ txt 20 (y0 + 66) 14 (gray (170 / 255)) "Earth's wind: somewhere there is always a windless point. A hairstyle with just one swirl is type 3." ]

-- ---------- やってみよう ----------
-- 1. 4 を何回も押して、つむじの数と指数を記録しよう。合計はいつも 2？
-- 2. field に自分の場（たとえば V3 y (-x) 0 を tangent で球にのせる）を足して、つむじを探そう
-- 3. つむじ 1 つ（3）の指数は +2。毛の流れをよく見て、1 周で毛が何回まわるか数えよう
-- 4. トーラスに「穴のまわりを回る」場（v の向き）を足すと、斜めにとかせる。torusPt を使って描いてみよう

-- ==================================================
-- ここから下は「道具」（座標の変換）
-- ==================================================

toP5 :: Point -> (Float, Float)
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = label (x - width / 2) (height / 2 - y) s c str
