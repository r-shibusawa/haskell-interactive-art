-- ============================================
-- 07 結び目（3D）
-- ひもの両端をつないだ輪が「結び目」。ほどけない（切らずに丸い輪に戻せない）結び目はたくさんある
--   トーラス（ドーナツ）の表面を、まわりに p 回・穴を q 回まわる曲線 ＝ トーラス結び目 (p, q)。右は真上から見た図（交点の上下つき）
-- ←→ = p / ↑↓ = q / 8 = 8 の字結び目 / T = トーラス結び目に戻す / スペース = 回転を止める / ドラッグ = 視点
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (foldl', tails)
import qualified Data.IntSet as IS

nPts, nSides :: Int
nPts = 360           -- ★ 曲線の点の数
nSides = 8           -- ★ チューブの断面の角の数

tubeR :: Float
tubeR = 0.13         -- ★ チューブの太さ

width, height, sS, camD, diagW :: Float
width = 1280
height = 800
sS = min width height * 0.13
camD = 693
diagW = fromIntegral (round (min (width * 0.36) (height * 0.6)) :: Int)   -- 結び目の図の大きさ

type D3 = (Double, Double, Double)

-- | チューブの輪 1 つ：断面の点とその法線。色ごとに束ねる
data Tube = Tube !(Float, Float, Float) ![[(V3, V3)]]

data Info = Info { iTitle :: String, iCross :: String, iComps :: Int, iNote :: [String] }

data World = World
  { wP, wQ :: !Int          -- ★ トーラス結び目の (p, q)
  , wEight :: !Bool
  , wRotX, wRotY :: !Float
  , wSpin :: !Bool
  , wPrev :: !(Float, Float)
  -- ↓ (p, q) を変えたときに作り直す
  , wTubes :: [Tube]
  , wDiag :: Picture
  , wDiagCross :: Int
  }

main :: IO ()
main = runSketch (sketch "27-07 Knots" (rebuild (World 2 3 False (-0.9) 0 True (0, 0) [] Blank 0)))
  { skBack  = rgb255 12 14 24
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- | 曲線（成分ごとの点のリスト）をつくる。座標は x, y が水平面、z が高さ
curves :: World -> [[D3]]
curves w
  | wEight w =
      [[ ((2 + cos (2 * t)) * cos (3 * t), (2 + cos (2 * t)) * sin (3 * t), sin (4 * t)) | t <- params ]]
  | otherwise =
      -- ★ トーラス結び目：まわりの角度 φ = p t、チューブのまわりの角度 θ = q t
      let d = gcd (wP w) (wQ w)
          pp = fromIntegral (wP w `div` d)
          qq = fromIntegral (wQ w `div` d)
          comp k =
            let o = 2 * pi * fromIntegral k / fromIntegral d
            in [ (r * cos (pp * t), r * sin (pp * t), -sin (qq * t + o))
               | t <- params, let r = 2 + cos (qq * t + o) ]
      in map comp [0 .. d - 1]
  where params = [fromIntegral i / fromIntegral nPts * 2 * pi | i <- [0 .. nPts - 1]] :: [Double]

palette :: [(Float, Float, Float)]
palette = [(255, 140, 70), (90, 200, 255), (150, 240, 120), (255, 110, 200), (250, 230, 90), (180, 150, 255), (255, 255, 255), (120, 255, 220), (255, 90, 90)]

colOf :: Int -> (Float, Float, Float)
colOf k = case drop (k `mod` length palette) palette of
  (c : _) -> c
  [] -> (255, 255, 255)

rebuild :: World -> World
rebuild w =
  let cs = curves w
      d = length cs
      -- チューブは全体で nPts 点ほどになるよう、本数に合わせて間引く（本数が多いときは p, q が小さく、曲線は単純）
      thin c = [V3 (realToFrac x) (realToFrac y) (realToFrac z) | (i, (x, y, z)) <- zip [0 :: Int ..] c, i `mod` d == 0]
      tubes = [Tube (colOf k) (tube (thin c)) | (k, c) <- zip [0 ..] cs]
      (pic, cross) = drawDiagram cs
  in w { wTubes = tubes, wDiag = pic, wDiagCross = cross }

infoOf :: World -> Info
infoOf w
  | wEight w = Info "Figure-eight knot" "4" 1 ["4 crossings. Not a torus knot.", "Cannot be 3-colored (see sketch 08)."]
  | p == 1 || q == 1 = Info "Unknot (it comes undone)" "0" 1 ["If p or q is 1, it returns to a plain ring."]
  | d > 1 = Info ("Link (" ++ show d ++ " rings)") "-" d ["gcd of p and q is " ++ show d ++ " -> " ++ show d ++ " linked rings"]
  | otherwise =
      Info ("Torus knot (" ++ show p ++ ", " ++ show q ++ ")" ++ nameOf (min p q, max p q))
           (show (min (p * (q - 1)) (q * (p - 1)))) 1
           ["Min crossings = min(p(q-1), q(p-1)).", "(p, q) and (q, p) are the same knot."]
  where
    p = wP w
    q = wQ w
    d = gcd p q
    nameOf k = case k of
      (2, 3) -> " = trefoil"
      (2, 5) -> " = 5_1"
      (2, 7) -> " = 7_1"
      (3, 4) -> " = 8_19"
      (3, 5) -> " = 10_124"
      _ -> ""

---------------------------------------------------------------- 動き

stepWorld :: Env -> World -> World
stepWorld env w =
  let m@(mx, my) = toP5 (envMouse env)
      (px, py) = wPrev w
      w1 | envDown env = w { wRotY = wRotY w + (mx - px) * 0.01, wRotX = clampF (-2.5) 0.5 (wRotX w - (my - py) * 0.01) }
         | wSpin w = w { wRotY = wRotY w + 0.005 }
         | otherwise = w
  in w1 { wPrev = m }

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> change w { wP = min 9 (wP w + 1), wEight = False }
  EventKey (SpecialKey KeyLeft) Down _ _ -> change w { wP = max 1 (wP w - 1), wEight = False }
  EventKey (SpecialKey KeyUp) Down _ _ -> change w { wQ = min 9 (wQ w + 1), wEight = False }
  EventKey (SpecialKey KeyDown) Down _ _ -> change w { wQ = max 1 (wQ w - 1), wEight = False }
  EventKey (Char '8') Down _ _ -> change w { wEight = True }
  EventKey (Char c) Down _ _ | c `elem` "tT" -> change w { wEight = False }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wSpin = not (wSpin w) }
  _ -> w
  where
    change w' | wP w' /= wP w || wQ w' /= wQ w || wEight w' /= wEight w = rebuild w'
              | otherwise = w

---------------------------------------------------------------- 描画

-- | 回転：rotateX(rx) rotateZ(rz) に scale(S) と平行移動をかける（y は下向き）
rotFn :: World -> V3 -> V3
rotFn w =
  let cx = cos (wRotX w)
      sx = sin (wRotX w)
      cz = cos (wRotY w)
      sz = sin (wRotY w)
  in \(V3 x y z) ->
       let x1 = x * cz - y * sz
           y1 = x * sz + y * cz
       in V3 x1 (y1 * cx - z * sx) (y1 * sx + z * cx)

lightDir, halfV :: V3
lightDir = unit3 (V3 0.3 0.5 (-0.8))
halfV = unit3 (V3 (-0.3) (-0.5) 1.8)   -- 光と視線の中間（つやの向き）

-- | チューブの面を奥行きつきの四角形にする。裏を向いた面は描かない
tubeItems :: (V3 -> V3) -> Tube -> [(Float, Picture)]
tubeItems rot (Tube (cr, cg, cb) rings) =
  let toView (p, n) = let V3 x y z = rot p in (V3 (x * sS - width * 0.17) (y * sS + 20) (z * sS), rot n)
      vr = map (map toView) rings
      ringPair a b = concat (zipWith quad (zip a b) (rotate1 (zip a b)))
      quad ((a1, n1), (b1, m1)) ((a2, n2), (b2, m2)) =
        let n = unit3 (add3 n1 (add3 n2 (add3 m1 m2)))
            V3 _ _ nz = n
        in if nz < -0.2 then [] else
           let dif = max 0 (negate (dot3 n lightDir))
               sp = max 0 (dot3 n halfV) ^ (30 :: Int) * 0.3
               s = (90 / 255 + dif)
               ch c = min 1 (c / 255 * s + sp)
               z = zOf (scale3 0.25 (add3 a1 (add3 a2 (add3 b1 b2))))
           in [(z, Color (makeColor (ch cr) (ch cg) (ch cb) 1) (Polygon (map scr [a1, a2, b2, b1])))]
  in concat (zipWith ringPair vr (rotate1 vr))

scr :: V3 -> Point
scr v = let (x, y) = project camD v in (x, -y)

zOf :: V3 -> Float
zOf (V3 _ _ z) = z

drawWorld :: World -> Picture
drawWorld w =
  let items = concatMap (tubeItems (rotFn w)) (wTubes w)
      info = infoOf w
      x0 = width - diagW - 50
      y0 = 120
      lx = x0 - 40
      cDiag = (x0 + diagW / 2 - width / 2, height / 2 - (y0 + diagW / 2))
  in Pictures $
       map snd (depthSort fst items)
       ++ [ txt 20 40 16 (gray 1) ("<- ->: p = " ++ show (wP w) ++ "   Up/Down: q = " ++ show (wQ w)
                                   ++ "   8: figure-eight   T: torus knot   Space: spin " ++ (if wSpin w then "ON" else "off") ++ "   Drag: view")
          , txt 20 68 14 (gray (170 / 255)) "Diagram: the under-strand is cut at crossings. Count depends on the view; the minimum is fixed for each knot."
          , at cDiag (Pictures [Color (withA 0.05 white) (rectangleSolid diagW diagW), wDiag w])
          , txt lx (y0 + diagW + 40) 22 (rgb255 255 230 120) (iTitle info)
          , txt lx (y0 + diagW + 72) 14 (gray 1)
              ("This diagram: " ++ show (wDiagCross w) ++ " crossings   Min: " ++ iCross info ++ "   Rings: " ++ show (iComps info)) ]
       ++ [ txt lx (y0 + diagW + 98 + fromIntegral i * 20) 14 (gray (170 / 255)) s | (i, s) <- zip [0 :: Int ..] (iNote info) ]

-- ---------- やってみよう ----------
-- 1. (2, 3) と (3, 2) を見比べよう。形はちがうのに同じ結び目（三葉結び目）
-- 2. (2, 4) や (3, 6) にすると輪が何本になる？ 最大公約数との関係は？
-- 3. 右の図の交点の数と最小交点数がいちばん近くなる見る向きを探そう（(3, 4) は？）
-- 4. curves に自分の式（たとえば x = sin t + 2 sin 2t, y = cos t − 2 cos 2t, z = −sin 3t）を足してみよう

-- ==================================================
-- ここから下は「チューブと結び目の図の道具」
-- ==================================================

-- | 曲線のまわりにチューブ（平行移動フレームで、つなぎ目のねじれを補正）
tube :: [V3] -> [[(V3, V3)]]
tube c =
  let prevs = rotate1' c
      nexts = rotate1 c
      ts = zipWith (\b a -> unit3 (sub3 b a)) nexts prevs
      t0 = case ts of
        (t : _) -> t
        [] -> V3 0 0 1
      V3 _ _ t0z = t0
      n0 = unit3 (cross3 t0 (if abs t0z < 0.9 then V3 0 0 1 else V3 1 0 0))
      -- 次の接線に垂直になるように法線を送っていく
      transport cur t = let d = dot3 cur t in unit3 (sub3 cur (scale3 d t))
      nsRaw = drop 1 (scanl transport n0 ts)
      lastN = case reverse nsRaw of
        (n : _) -> n
        [] -> n0
      end = transport lastN t0
      alpha = atan2 (dot3 (cross3 end n0) t0) (dot3 end n0)
      n = fromIntegral (max 1 (length c)) :: Float
      ring i p t nr =
        let a = alpha * fromIntegral i / n
            b0 = cross3 t nr
            nn = add3 (scale3 (cos a) nr) (scale3 (sin a) b0)
            bb = cross3 t nn
        in [ let ang = fromIntegral s / fromIntegral nSides * tau
                 off = add3 (scale3 (cos ang) nn) (scale3 (sin ang) bb)
             in (add3 p (scale3 tubeR off), off)
           | s <- [0 .. nSides - 1] ]
  in zipWith4' ring [0 :: Int ..] c ts nsRaw

zipWith4' :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4' f (a : as) (b : bs) (c : cs) (d : ds) = f a b c d : zipWith4' f as bs cs ds
zipWith4' _ _ _ _ _ = []

-- | 線分：始点、終点、成分の番号、通し番号
data Seg = Seg !D3 !D3 !Int !Int

-- | 真上から見た結び目の図：下を通るところにすき間をあける。戻り値は (図, 交点の数)
drawDiagram :: [[D3]] -> (Picture, Int)
drawDiagram cs =
  let sc = realToFrac diagW / 7 :: Double
      raw = [ (a, b, k)
            | (k, c) <- zip [0 ..] cs
            , (a, b) <- everyOther (zip c (rotate2 c)) ]
      segs = zipWith (\i (a, b, k) -> Seg a b k i) [0 ..] raw
      nSeg = length segs
      step (cnt, under) (Seg a1 b1 k1 i : rest) = foldl' (pairStep i k1 a1 b1) (cnt, under) (drop 1 rest)
      step acc [] = acc
      pairStep i _ a1 b1 acc@(!cnt, !under) (Seg a2 b2 _ j)
        | i == 0 && j == nSeg - 1 = acc
        | otherwise = case segHit a1 b1 a2 b2 of
            Nothing -> acc
            Just (t, u) ->
              let zi = zOfD a1 + (zOfD b1 - zOfD a1) * t
                  zj = zOfD a2 + (zOfD b2 - zOfD a2) * u
                  v = if zi < zj then i else j
              in (cnt + 1 :: Int, IS.insert v under)
      (count, under0) = foldl' step (0, IS.empty) (tails segs)
      -- 下を通る側にすき間（1 つ手前の線分も、同じ成分なら消す）
      prevSame v = v > 0 && sameComp segs (v - 1) v
      under1 = IS.union under0 (IS.fromList [v - 1 | v <- IS.toList under0, prevSame v])
      draw (Seg (ax, ay, _) (bx, by, _) k i)
        | IS.member i under1 = Blank
        | otherwise =
            let (r, g, b) = colOf k
            in Color (makeColor (r / 255) (g / 255) (b / 255) 1)
                 (thickLine 4 (realToFrac (ax * sc), realToFrac (ay * sc)) (realToFrac (bx * sc), realToFrac (by * sc)))
  in (Pictures (map draw segs), count)

-- | 通し番号 a と b の線分が同じ成分か（成分ごとの線分の数は同じなので割り算で求まる）
sameComp :: [Seg] -> Int -> Int -> Bool
sameComp segs a b = case segs of
  [] -> False
  _ ->
    let total = length segs
        comps = case reverse segs of
          (Seg _ _ k _ : _) -> k + 1
          [] -> 1
        per = max 1 (total `div` max 1 comps)
    in a `div` per == b `div` per

zOfD :: D3 -> Double
zOfD (_, _, z) = z

segHit :: D3 -> D3 -> D3 -> D3 -> Maybe (Double, Double)
segHit (x1, y1, _) (x2, y2, _) (x3, y3, _) (x4, y4, _)
  | abs d < 1e-12 = Nothing
  | t >= 0 && t < 1 && u >= 0 && u < 1 = Just (t, u)
  | otherwise = Nothing
  where
    d = (x2 - x1) * (y4 - y3) - (y2 - y1) * (x4 - x3)
    t = ((x3 - x1) * (y4 - y3) - (y3 - y1) * (x4 - x3)) / d
    u = ((x3 - x1) * (y2 - y1) - (y3 - y1) * (x2 - x1)) / d

everyOther :: [a] -> [a]
everyOther (a : _ : more) = a : everyOther more
everyOther [a] = [a]
everyOther [] = []

toP5 :: Point -> (Float, Float)
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = label (x - width / 2) (height / 2 - y) s c str

rotate1 :: [a] -> [a]
rotate1 [] = []
rotate1 (x : xs) = xs ++ [x]

-- | 1 つ前へずらす
rotate1' :: [a] -> [a]
rotate1' [] = []
rotate1' xs = case reverse xs of
  (l : r) -> l : reverse r
  [] -> []

rotate2 :: [a] -> [a]
rotate2 = rotate1 . rotate1
