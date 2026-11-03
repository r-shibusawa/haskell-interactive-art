-- ============================================
-- 09 双曲平面（ポアンカレ円板）
-- ガウス曲率がどこでも −1 の「双曲平面」を円板の中に描いた地図。円板のふちは無限に遠い
--   測地線（まっすぐな線）= ふちと直角に交わる円弧。タイルはすべて同じ大きさの正七角形（{7,3}：各頂点に 3 枚）
-- 点 A・B をドラッグ = 測地線と距離 / 背景をドラッグ = 平面を平行移動（メビウス変換）/ T = タイル / C = 同じ半径の円 / ↑↓ = 円の半径 / R = もどす
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), ViewL(..), viewl)
import qualified Data.Set as Set
import Numeric (showFFloat)

tileP, tileQ :: Int
tileP = 7                  -- ★ {p, q} タイル：正 p 角形が各頂点に q 枚（1/p + 1/q < 1/2 なら双曲）
tileQ = 3

maxTiles :: Int
maxTiles = 1200            -- ★ タイルの枚数の上限

-- 複素数（|z| < 1 が円板の中）
data C = C !Double !Double

-- いまのメビウス変換 z → (a z + b) / (b̄ z + ā)
data Mob = Mob !C !C

data Drag = DragNone | DragA | DragB | DragBg deriving Eq

data World = World
  { wA, wB :: !C
  , wTiles, wCircles :: !Bool
  , wRho :: !Double        -- ★ 円の双曲半径
  , wM :: !Mob
  , wDrag :: !Drag
  , wLast :: !C
  }

main :: IO ()
main = runSketch (sketch "28-09 Poincare Disk" initial)
  { skBack  = rgb255 16 16 26
  , skStart = \_ -> length tiling `seq` pure initial   -- タイルは最初に作っておく
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }
  where initial = World startA startB True True 0.8 identMob DragNone (C 0 0)

startA, startB :: C
startA = C (-0.3) 0.1
startB = C 0.45 (-0.2)

identMob :: Mob
identMob = Mob (C 1 0) (C 0 0)

---- 複素数の計算 ----
cmul, cdiv, cadd :: C -> C -> C
cmul (C a b) (C c d) = C (a * c - b * d) (a * d + b * c)
cdiv (C a b) (C c d) = let e = c * c + d * d in C ((a * c + b * d) / e) ((b * c - a * d) / e)
cadd (C a b) (C c d) = C (a + c) (b + d)

conj :: C -> C
conj (C a b) = C a (-b)

cabs2 :: C -> Double
cabs2 (C a b) = a * a + b * b

mob :: Mob -> C -> C
mob (Mob a b) z = cdiv (cadd (cmul a z) b) (cadd (cmul (conj b) z) (conj a))

-- m1 ∘ m2
compose :: Mob -> Mob -> Mob
compose (Mob a1 b1) (Mob a2 b2) =
  let C ax ay = cadd (cmul a1 a2) (cmul b1 (conj b2))
      C bx by = cadd (cmul a1 b2) (cmul b1 (conj a2))
      s = sqrt (max 1e-12 (ax * ax + ay * ay - bx * bx - by * by))
  in Mob (C (ax / s) (ay / s)) (C (bx / s) (by / s))

-- ★ 双曲距離 d(z, w) = arcosh(1 + 2|z − w|² / ((1 − |z|²)(1 − |w|²)))
hdist :: C -> C -> Double
hdist z@(C a b) w@(C c d) =
  acosh (1 + 2 * cabs2 (C (a - c) (b - d)) / ((1 - cabs2 z) * (1 - cabs2 w)))

-- 円 (中心 c, 半径 r) での反転（双曲平面の「鏡映」）
invert :: C -> C -> Double -> C
invert (C zx zy) (C cx cy) r =
  let dx = zx - cx
      dy = zy - cy
      k = r * r / (dx * dx + dy * dy)
  in C (cx + dx * k) (cy + dy * k)

-- 2 点を通る測地線の円（ふちと直角）。原点を通る直線なら Nothing
geoCircle :: C -> C -> Maybe (C, Double)
geoCircle p@(C p0 p1) q@(C q0 q1) =
  let cr = p0 * q1 - p1 * q0
  in if abs cr < 1e-9 then Nothing else
     -- 中心 c は c·p = (1 + |p|²)/2、c·q = (1 + |q|²)/2 を満たす
     let a = (1 + cabs2 p) / 2
         b = (1 + cabs2 q) / 2
         c = C ((a * q1 - b * p1) / cr) ((b * p0 - a * q0) / cr)
     in Just (c, sqrt (max 0 (cabs2 c - 1)))

-- p から q への測地線の弧を n 個の点で
geoArc :: C -> C -> Int -> [C]
geoArc p@(C p0 p1) q@(C q0 q1) n =
  let fi = fromIntegral :: Int -> Double
  in case geoCircle p q of
       Nothing -> [C (p0 + (q0 - p0) * fi i / fi n) (p1 + (q1 - p1) * fi i / fi n) | i <- [0 .. n]]
       Just (C cx cy, r) ->
         let a0 = atan2 (p1 - cy) (p0 - cx)
             a1 = atan2 (q1 - cy) (q0 - cx)
             d0 = a1 - a0
             d | d0 > pi = d0 - 2 * pi
               | d0 < -pi = d0 + 2 * pi
               | otherwise = d0
         in [C (cx + r * cos (a0 + d * fi i / fi n)) (cy + r * sin (a0 + d * fi i / fi n)) | i <- [0 .. n]]

center :: [C] -> C
center ps =
  let n = fromIntegral (length ps)
      C a b = foldl' cadd (C 0 0) ps
  in C (a / n) (b / n)

keyOf :: [C] -> (Int, Int)
keyOf ps = let C a b = center ps in (round (a * 2000), round (b * 2000))

reflectLine :: C -> C -> C -> C
reflectLine (C zx zy) (C px' py) (C qx qy) =
  let dx = qx - px'
      dy = qy - py
      l = dx * dx + dy * dy
      t = ((zx - px') * dx + (zy - py) * dy) / l
      fx = px' + dx * t
      fy = py + dy * t
  in C (2 * fx - zx) (2 * fy - zy)

-- ★ {P, Q} タイルを、中心の正 P 角形から辺での鏡映をくり返してつくる
tiling :: [[C]]
tiling =
  let p = fromIntegral tileP :: Double
      q = fromIntegral tileQ :: Double
      -- 中心の多角形の頂点までの距離：双曲の直角三角形の公式 cosh R = cot(π/P) cot(π/Q)
      ch = 1 / (tan (pi / p) * tan (pi / q))
      r0 = sqrt ((ch - 1) / (ch + 1))      -- tanh(d/2)
      first' = [ C (r0 * cos (2 * pi * fromIntegral i / p + pi / p)) (r0 * sin (2 * pi * fromIntegral i / p + pi / p)) | i <- [0 .. tileP - 1] ]
      edges poly = zip poly (drop 1 poly ++ take 1 poly)
      image poly (a, b) = case geoCircle a b of
        Just (c, r) -> map (\z -> invert z c r) poly
        Nothing -> map (\z -> reflectLine z a b) poly
      go :: Int -> Set.Set (Int, Int) -> Seq [C] -> [[C]]
      go count seen queue = case viewl queue of
        EmptyL -> []
        poly :< rest
          | count >= maxTiles -> []
          | otherwise ->
              let visit (cnt, sn, qu, out) e =
                    let img = image poly e
                        k = keyOf img
                    in if cabs2 (center img) > 0.995 * 0.995 || Set.member k sn || cnt >= maxTiles
                         then (cnt, sn, qu, out)
                         else (cnt + 1, Set.insert k sn, qu |> img, img : out)
                  (c2, s2, q2, new) = foldl' visit (count, seen, rest, []) (edges poly)
              in reverse new ++ go c2 s2 q2
  in first' : go 1 (Set.singleton (keyOf first')) (Seq.singleton first')

---- 画面 ----
diskR :: Env -> Float
diskR env = min (envW env * 0.36) (envH env * 0.42)

scr :: Env -> C -> Point
scr env (C a b) = px env (envW env * 0.38 + realToFrac a * diskR env) (envH env * 0.53 - realToFrac b * diskR env)

unscr :: Env -> Point -> C
unscr env (x, y) =
  let mx = x + envW env / 2
      my = envH env / 2 - y
  in C (realToFrac ((mx - envW env * 0.38) / diskR env)) (realToFrac (-(my - envH env * 0.53) / diskR env))

-- 1 枚のタイル。へこんだ形なので、中心からの三角形の扇で塗る
tilePic :: Env -> Mob -> (Int, [C]) -> [Picture]
tilePic env m (k, poly0) =
  let poly = map (mob m) poly0
      c@(C cx cy) = center poly
      (sizeE, _) = case poly of
        (C a b : _) -> (abs (a - cx) + abs (b - cy), ())
        [] -> (0, ())
      rd = realToFrac (diskR env) :: Double
      size = sizeE * rd
      n | size > 30 = 8
        | size > 8 = 3
        | otherwise = 1 :: Int
      ring' = concat (zipWith (\a b -> init' (geoArc a b n)) poly (drop 1 poly ++ take 1 poly))
      init' xs = take (length xs - 1) xs
      pts = map (scr env) ring'
      fillC = if odd k then rgb255 70 90 160 else rgb255 110 80 170
      cS = scr env c
      fill' = if n <= 3 then [Polygon pts]
              else zipWith (\a b -> Polygon [cS, a, b]) pts (drop 1 pts ++ take 1 pts)
  in if size < 1.5 then [] else
     [ Color fillC (Pictures fill')
     , Color (rgba255 200 210 255 120) (Line (pts ++ take 1 pts)) ]

draw :: Env -> World -> Picture
draw env w =
  let rd = diskR env
      c0 = scr env (C 0 0)
      tiles = if wTiles w then concatMap (tilePic env (wM w)) (zip [0 ..] tiling) else []
      a = wA w
      b = wB w
      gold = rgb255 255 230 90
      -- A と B を通る測地線（ふちからふちまで）と、A から B への線分
      fullLine = case geoCircle a b of
        Just (C cx cy, r) ->
          let pts = [ C (cx + r * cos t) (cy + r * sin t) | i <- [0 .. 200 :: Int], let t = fromIntegral i / 200 * 2 * pi ]
              pieces cur [] = [cur | length cur >= 2]
              pieces cur (z : zs)
                | cabs2 z < 1 = pieces (scr env z : cur) zs
                | otherwise = [cur | length cur >= 2] ++ pieces [] zs
          in map (thickPath 2) (pieces [] pts)
        Nothing ->
          let C ax ay = a
              C bx by = b
              dx = bx - ax
              dy = by - ay
              l = sqrt (dx * dx + dy * dy)
          in [ thickLine 2 (scr env (C (dx / l) (dy / l))) (scr env (C (-dx / l) (-dy / l))) | l > 1e-9 ]
      arc = thickPath 4 (map (scr env) (geoArc a b 60))
      -- 同じ双曲半径の円（ユークリッドでは、ふちに近いほど小さく見える）
      centers = [C 0 0, C 0.5 0, C 0.75 0, C 0.88 0, C 0.95 0, C (-0.3) (-0.6), C (-0.6) (-0.65)]
      circles = if wCircles w
        then map (hcircle env (wRho w) (rgb255 120 255 170)) centers ++ [hcircle env (wRho w) (rgb255 255 140 140) a]
        else []
      mark z nm = let s@(sx, sy) = scr env z
                  in Pictures [at s (Color white (disc 8)), label (sx + 10) (sy + 10) 17 white nm]
      onOff v = if v then "ON" else "off"
      x = envW env * 0.72
      y = 150
      C ax' ay' = a
      C bx' by' = b
      gr v = gray v
  in Pictures $
     [ at c0 (Color (rgb255 28 34 58) (disc rd)) ] ++ tiles ++
     [ at c0 (Color (withA (160 / 255) white) (ring rd 2))
     , Color (withA (120 / 255) gold) (Pictures fullLine)
     , Color gold arc ] ++ circles ++
     [ mark a "A", mark b "B"
     , txt env 20 34 16 white ("A, B drag: geodesic   Background drag: slide   T: tiles " ++ onOff (wTiles w) ++ "   C: circles " ++ onOff (wCircles w) ++ "   U/D: radius " ++ nf 2 (wRho w) ++ "   R: reset")
     , txt env 20 60 14 (gr 0.67) "All heptagon tiles have the same size and shape (in the hyperbolic plane). Smaller near the rim = map distortion."
     , box env (x - 16) (y - 34) (envW env - x + 16) 330 (withA (160 / 255) black)
     , txt env x y 16 gold ("Hyperbolic d(A, B) = " ++ nf 3 (hdist a b))
     , txt env x (y + 26) 14 (gr 0.86) ("Apparent length |A - B| = " ++ nf 3 (sqrt (cabs2 (C (ax' - bx') (ay' - by')))))
     , txt env x (y + 50) 14 (gr 0.86) ("Origin to A = " ++ nf 3 (hdist (C 0 0) a))
     , txt env x (y + 72) 14 (gr 0.86) "(the rim is infinitely far)"
     , txt env x (y + 110) 14 (gr 0.67) "Distance formula:"
     , txt env x (y + 132) 14 (gr 0.67) "d = arcosh(1 + 2|z-w|^2 /"
     , txt env x (y + 152) 14 (gr 0.67) "      ((1-|z|^2)(1-|w|^2)))"
     , txt env x (y + 182) 14 (gr 0.67) ("Green circles: all hyperbolic radius " ++ nf 2 (wRho w) ++ ".")
     , txt env x (y + 204) 14 (gr 0.67) "Near the rim: smaller, center shifts"
     , txt env x (y + 232) 14 (gr 0.67) "Triangle angle sum < 180 (K = -1)"
     , txt env x (y + 260) 14 (gr 0.67) ("Tiles {" ++ show tileP ++ "," ++ show tileQ ++ "}: " ++ show (length tiling) ++ " tiles")
     ]

-- 双曲円：原点の円 |z| = tanh(ρ/2) を c へ平行移動
hcircle :: Env -> Double -> Color -> C -> Picture
hcircle env rho col c@(C cx cy) =
  let r = tanh (rho / 2)
      s = sqrt (max 1e-9 (1 - cabs2 c))
      t = Mob (C (1 / s) 0) (C (cx / s) (cy / s))     -- 原点を c へ送るメビウス変換
      pts = [ scr env (mob t (C (r * cos a) (r * sin a))) | i <- [0 .. 60 :: Int], let a = fromIntegral i / 60 * 2 * pi ]
  in Color col (thickPath 1.5 pts)

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let z = unscr env p
        nearP q = dist2 p (scr env q) < 16
        d | nearP (wB w) = DragB
          | nearP (wA w) = DragA
          | cabs2 z < 1 = DragBg
          | otherwise = DragNone
    in w { wDrag = d, wLast = z }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = DragNone }
  EventMotion p | envDown env ->
    let z0 = unscr env p
        z@(C zx zy) = if cabs2 z0 > 0.985 * 0.985
                        then let C a b = z0
                                 l = sqrt (cabs2 z0)
                             in C (a / l * 0.985) (b / l * 0.985)
                        else z0
        w1 = w { wLast = z }
    in case wDrag w of
         DragA -> w1 { wA = z }
         DragB -> w1 { wB = z }
         DragBg ->
           -- last を z へ動かすメビウス変換（原点経由）を、今の変換に合成する
           let C lx ly = wLast w
               s1 = sqrt (max 1e-9 (1 - cabs2 (wLast w)))
               s2 = sqrt (max 1e-9 (1 - cabs2 z))
               toO = Mob (C (1 / s1) 0) (C (-lx / s1) (-ly / s1))
               fromO = Mob (C (1 / s2) 0) (C (zx / s2) (zy / s2))
           in w1 { wM = compose fromO (compose toO (wM w)) }
         DragNone -> w1
  EventKey (Char c) Down _ _ -> case toLower c of
    't' -> w { wTiles = not (wTiles w) }
    'c' -> w { wCircles = not (wCircles w) }
    'r' -> w { wM = identMob, wA = startA, wB = startB }
    _ -> w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wRho = min 3 (wRho w + 0.1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wRho = max 0.1 (wRho w - 0.1) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. B をふちに近づける。見た目はあまり動かないのに d(A, B) がどんどん大きくなる
-- 2. 背景をドラッグしてタイルを動かす。ふちの小さなタイルが中心に来ると、中心のタイルと同じ大きさになる
-- 3. tileP = 5, tileQ = 4 や tileP = 4, tileQ = 5 に変えると別のタイル。6, 3 だと平面（1/p + 1/q = 1/2）になって作れない
-- 4. A, B と 3 つ目の点で三角形を描き、内角の和を測ってみよう（180° より小さい）

---------------------------------------------------------------- 道具
-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
px :: Env -> Float -> Float -> Point
px env x y = (x - envW env / 2, envH env / 2 - y)

-- 文字（p5 の text と同じく左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (a, b) = px env x y in label a b s c str

-- 左上が (x, y) の塗りつぶし長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (a, b) = px env (x + w / 2) (y + h / 2) in Color c (rect a b w h)

-- p5 の nf：小数点以下 d 桁の文字列
nf :: RealFloat a => Int -> a -> String
nf d x | isNaN x = "--"
       | isInfinite x = if x > 0 then "inf" else "-inf"
       | otherwise = showFFloat (Just d) x ""

degF :: Float -> Float
degF x = x * 180 / pi

-- JavaScript の % と同じ（符号は x に合わせる）
jsMod :: Float -> Float -> Float
jsMod x m = x - m * fromIntegral (truncate (x / m) :: Int)
