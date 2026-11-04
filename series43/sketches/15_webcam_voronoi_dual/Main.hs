-- ============================================
-- 43-15 カメラでボロノイとドロネー（おまけ）
-- カメラの映像を小さなマス目に分け、各マスでいちばん明るい画素を「母点」にする
--   母点が動くとボロノイ図（面）とドロネー三角形分割（頂点をつなぐ）が同時に動く（02 の双対をライブで）
--   カメラが使えないときはランダムに動く点で代用（カメラの許可が必要）
-- 操作：V = ボロノイ / D = ドロネー / B = 両方 / ←→ = 明るさのしきい値 / ↑↓ = マス目の数
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import qualified Data.ByteString as B
import Data.List (foldl', partition, sortOn)
import qualified Data.Vector.Unboxed as U

vw, vh :: Int
vw = 96                -- ★ カメラ画像の大きさ（小さいほど速い）
vh = 72

follow :: Float
follow = 0.25          -- ★ 母点の追従のなめらかさ

-- カメラがないときの動く点
data Mover = Mover { mX :: !Float, mY :: !Float, mVx :: !Float, mVy :: !Float }

data World = World
  { wCam    :: !(Maybe Camera)
  , wFrame  :: !B.ByteString     -- 最新のフレーム
  , wCamOK  :: !Bool
  , wCols   :: !Int              -- ★ マス目の数（母点の最大数 = cols × rows）
  , wRows   :: !Int
  , wThr    :: !Int              -- ★ この明るさより暗いマスは母点にしない
  , wView   :: !Char             -- 'V' / 'D' / 'B'
  , wSites  :: ![Point]          -- 母点（画面座標、なめらかに追いかける）
  , wMovers :: ![Mover]
  }

main :: IO ()
main = runSketch (sketch "43-15 Webcam Voronoi and Delaunay" (World Nothing B.empty False 8 6 120 'B' [] []))
  { skBack  = hsb 250 40 10
  , skStart = \env -> do
      cam <- startCamera vw vh
      ms <- replicateM 24 $ do
        x <- randomRIO (0, envW env)
        y <- randomRIO (100, envH env - 40)
        vx <- randomRIO (-1.5, 1.5)
        vy <- randomRIO (-1.5, 1.5)
        pure (Mover x y vx vy)
      pure (World (Just cam) B.empty False 8 6 120 'B' [] ms)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

fieldRect :: Env -> Panel
fieldRect env = Panel 20 95 (envW env - 40) (envH env - 130)

-- ★ 各マスでいちばん明るい画素を探す → 目標の母点
brightTargets :: Env -> World -> B.ByteString -> [Point]
brightTargets env w f =
  let a = fieldRect env
      cols = wCols w
      rows = wRows w
      cw = fromIntegral vw / fromIntegral cols :: Float
      ch = fromIntegral vh / fromIntegral rows :: Float
      fl v = floor v :: Int
      bright x y =
        let i = (x + y * vw) * 3
        in fromIntegral (B.index f i) + fromIntegral (B.index f (i + 1)) + fromIntegral (B.index f (i + 2)) :: Int
      cell gx gy =
        let pick (best, bx, by) (x, y) =
              let b = bright x y in if b > best then (b, x, y) else (best, bx, by)
            coords = [ (x, y)
                     | y <- [fl (fromIntegral gy * ch) .. fl (fromIntegral (gy + 1) * ch) - 1]
                     , x <- [fl (fromIntegral gx * cw) .. fl (fromIntegral (gx + 1) * cw) - 1] ]
            (b0, x0, y0) = foldl' pick (-1, 0, 0) coords
        in ( ( px a + (fromIntegral x0 + 0.5) / fromIntegral vw * pw a
             , py a + (fromIntegral y0 + 0.5) / fromIntegral vh * ph a )
           , b0 )
      allCells = [cell gx gy | gy <- [0 .. rows - 1], gx <- [0 .. cols - 1]]
      out = [p | (p, b) <- allCells, b >= wThr w * 3]
  in -- しきい値を超えるマスが 3 つ未満なら、明るい順に 3 つは必ず使う
     if length out < 3 then map fst (take 3 (sortOn (negate . snd) allCells)) else out

moveMover :: Panel -> Mover -> Mover
moveMover a m =
  let x = mX m + mVx m
      y = mY m + mVy m
  in m { mX = x, mY = y
       , mVx = if x < px a || x > px a + pw a then negate (mVx m) else mVx m
       , mVy = if y < py a || y > py a + ph a then negate (mVy m) else mVy m }

-- 同じ場所に重なった点を少しずらす（退化を避ける）
separate :: [Point] -> [Point]
separate = reverse . foldl' add []
  where
    add done p = foldl' nudge p (reverse done) : done
    nudge (x, y) (qx, qy)
      | abs (x - qx) < 1 && abs (y - qy) < 1 = (x + 1.5, y + 0.7)
      | otherwise = (x, y)

step :: Env -> Float -> World -> IO World
step env _ w = do
  live <- maybe (pure False) camLive (wCam w)
  frame <- case wCam w of
    Just c | live -> cameraFrame c
    _ -> pure B.empty
  let a = fieldRect env
      m@(mx, my) = mouseP env
      camOK = live && B.length frame >= vw * vh * 3
      movers = if camOK then wMovers w else map (moveMover a) (wMovers w)
      targets
        | camOK = brightTargets env w frame
        | mx > px a && mx < px a + pw a && my > py a && my < py a + ph a =
            [(mX o, mY o) | o <- movers] ++ [m]
        | otherwise = [(mX o, mY o) | o <- movers]
      -- 母点をなめらかに動かす（数が変わったら合わせる）
      old = wSites w
      padded = old ++ drop (length old) targets
      sites = zipWith (\(sx, sy) (tx, ty) -> (lerpF sx tx follow, lerpF sy ty follow)) padded targets
  pure w { wFrame = frame, wCamOK = camOK, wMovers = movers, wSites = separate sites }

draw :: Env -> World -> Picture
draw env w = Pictures
  [ -- カメラの映像を薄く背景に（色を背景の色へ 75% 近づける）
    if wCamOK w then camBack else Blank
  , view env $
      [ box (hsb 0 0 35) (px a) (py a) (pw a) (ph a) ]
      ++ (if v /= 'D'
            then concat [ [ Color (hsbA (180 + fromIntegral ((i * 29) `mod` 80)) 60 70 20) (Polygon c)
                          , closed 2 (hsbA 180 70 100 85) c ]
                        | (i, c) <- zip [0 :: Int ..] cells, length c >= 3 ]
            else [])
      ++ (if v /= 'V'
            then [ seg 1.5 (hsbA 10 80 100 80) (pt i) (pt j)
                 | t <- tris, (i, j) <- [(ta t, tb t), (tb t, tc t), (tc t, ta t)] ]
            else [])
      ++ (if v == 'B'
            then [ dot (hsb 55 90 100) x y 5
                 | t <- tris, let x = realToFrac (tcx t), let y = realToFrac (tcy t)
                 , x > px a, x < px a + pw a, y > py a, y < py a + ph a ]
            else [])
      ++ [ dot (hsb 0 0 100) x y 8 | (x, y) <- sites ]
      ++ [ txt 20 32 18 (hsb 0 0 100)
             ((if wCamOK w then "Camera: " ++ show (length sites) ++ " bright points are the sites"
                           else "No camera -> " ++ show (length sites) ++ " moving points (mouse is a site too)")
              ++ "   V / D / B (now " ++ [v] ++ ")")
         , txt 20 56 14 (hsb 0 0 100) ("threshold " ++ show (wThr w) ++ " (Left/Right)   grid " ++ show (wCols w) ++ "x" ++ show (wRows w) ++ " (Up/Down)")
         , txt 20 76 14 (hsb 0 0 60) "Move a phone light or white paper and the sites follow. Faces and vertices come from the same points and always move together." ]
  ]
  where
    a = fieldRect env
    v = wView w
    sites = wSites w
    pt i = case drop i sites of
      (p : _) -> p
      _ -> (0, 0)
    tris = if length sites >= 3 then delaunay sites else []
    cells = if length sites >= 2 then voronoiCells sites (px a) (py a) (px a + pw a) (py a + ph a) else []
    f = wFrame w
    -- 背景の色 hsb(250, 40, 10) は RGB でおよそ (17, 15, 26)
    mix bg c = round (0.75 * (bg :: Float) + 0.25 * fromIntegral c)
    camBack =
      Translate (px a + pw a / 2 - envW env / 2) (envH env / 2 - py a - ph a / 2)
        (Scale (pw a / fromIntegral vw) (ph a / fromIntegral vh)
          (pixelImage vw vh 1 (\x y ->
            let i = (y * vw + x) * 3
            in (mix 17 (B.index f i), mix 15 (B.index f (i + 1)), mix 26 (B.index f (i + 2))))))

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char k) Down _ _
    | k `elem` "vV" -> w { wView = 'V' }
    | k `elem` "dD" -> w { wView = 'D' }
    | k `elem` "bB" -> w { wView = 'B' }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wThr = min 250 (wThr w + 10) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wThr = max 0 (wThr w - 10) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wCols = min 12 (wCols w + 1), wRows = min 9 (wRows w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wCols = max 2 (wCols w - 1), wRows = max 2 (wRows w - 1) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. しきい値を上げて母点を減らす。3 個になると三角形 1 つとボロノイの頂点 1 つ
-- 2. ライトを 1 つだけ振ると、その周りの面だけが変わる（局所性）
-- 3. 明るい点の代わりに「赤い点」を母点にする（R が G・B より大きい画素）
-- 4. ボロノイの面を母点の明るさ b で塗り分ける（明るいほど白く）

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 左上が (x, y) の長方形の枠
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 太さ w の閉じた折れ線
closed :: Float -> Color -> [Point] -> Picture
closed w c ps = path w c (ps ++ take 1 ps)

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 画面の枠（左上の x, y、幅、高さ）
data Panel = Panel { px :: !Float, py :: !Float, pw :: !Float, ph :: !Float }

-- 多角形を半平面 nx*x + ny*y <= c で切る（Sutherland–Hodgman）
clipHalf :: (Ord a, Fractional a) => [(a, a)] -> a -> a -> a -> [(a, a)]
clipHalf ps nx ny c = concat (zipWith piece ps (drop 1 ps ++ take 1 ps))
  where
    piece a@(ax, ay) (bx, by) =
      let fa = nx * ax + ny * ay - c
          fb = nx * bx + ny * by - c
          t = fa / (fa - fb)
      in [a | fa <= 0] ++ [(ax + (bx - ax) * t, ay + (by - ay) * t) | (fa < 0 && fb > 0) || (fa > 0 && fb < 0)]

-- 三角形（頂点の番号）と外接円（中心 tcx, tcy・半径² tr2）。
-- 超三角形の座標が大きいので、ここだけ桁の多い Double で計算する
data Tri = Tri { ta :: !Int, tb :: !Int, tc :: !Int, tcx :: !Double, tcy :: !Double, tr2 :: !Double }

makeTri :: U.Vector (Double, Double) -> Int -> Int -> Int -> Tri
makeTri ps a b c =
  let (ax, ay) = ps U.! a
      (bx, by) = ps U.! b
      (cx, cy) = ps U.! c
      d0 = 2 * (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by))
      d = if abs d0 < 1e-9 then 1e-9 else d0
      a2 = ax * ax + ay * ay
      b2 = bx * bx + by * by
      c2 = cx * cx + cy * cy
      ox = (a2 * (by - cy) + b2 * (cy - ay) + c2 * (ay - by)) / d
      oy = (a2 * (cx - bx) + b2 * (ax - cx) + c2 * (bx - ax)) / d
  in Tri a b c ox oy ((ax - ox) * (ax - ox) + (ay - oy) * (ay - oy))

-- ドロネー三角形分割（Bowyer–Watson 法）：点を 1 つずつ入れ、外接円に含む三角形を壊して貼り直す
delaunay :: [Point] -> [Tri]
delaunay p0 = filter real (foldl' insert [makeTri ps n (n + 1) (n + 2)] [0 .. n - 1])
  where
    n = length p0
    big = 1e5
    ps = U.fromList ([(realToFrac x, realToFrac y) | (x, y) <- p0] ++ [(-big, -big), (big, -big), (0, big)])
    real t = ta t < n && tb t < n && tc t < n
    edgesOf t = [(ta t, tb t), (tb t, tc t), (tc t, ta t)]
    insert tris i =
      let (x, y) = ps U.! i
          inCircle t = (x - tcx t) * (x - tcx t) + (y - tcy t) * (y - tcy t) < tr2 t
          (bad, keep) = partition inCircle tris
          numbered = zip [0 :: Int ..] bad
          sameEdge a b (p, q) = (p == a && q == b) || (p == b && q == a)
          open = [ (a, b) | (k, t) <- numbered, (a, b) <- edgesOf t
                 , not (any (\(m, u) -> m /= k && any (sameEdge a b) (edgesOf u)) numbered) ]
      in keep ++ [makeTri ps a b i | (a, b) <- open]

-- ボロノイの面：枠を、各母点との垂直二等分線で切っていく（半平面の交わり）
voronoiCells :: [Point] -> Float -> Float -> Float -> Float -> [[Point]]
voronoiCells p0 x0 y0 x1 y1 = [map back (cell i p) | (i, p) <- numbered]
  where
    dbl :: Float -> Double
    dbl = realToFrac
    back (x, y) = (realToFrac x, realToFrac y) :: Point
    numbered = zip [0 :: Int ..] [(dbl x, dbl y) | (x, y) <- p0]
    frame = [(dbl x0, dbl y0), (dbl x1, dbl y0), (dbl x1, dbl y1), (dbl x0, dbl y1)]
    cell i (ax, ay) = foldl' cut frame [q | (j, q) <- numbered, j /= i]
      where
        cut poly (qx, qy) =
          let nx = qx - ax
              ny = qy - ay
          in clipHalf poly nx ny (nx * (ax + qx) / 2 + ny * (ay + qy) / 2)
