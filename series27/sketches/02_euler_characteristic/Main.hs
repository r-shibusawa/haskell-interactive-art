-- ============================================
-- 02 オイラー標数 V − E + F
-- 平面に（交わらないように）点と線を描くと、いつでも 点 V − 線 E + 面 F = 2（外側の面も 1 つと数える）
--   多面体でも V − E + F = 2。ドーナツ形（トーラス）の多面体だと 0 になる。形をどう変えても変わらない数 ＝ 位相不変量
-- 1 = 平面グラフを描く / 2 = 多面体を見る（←→ で選ぶ・ドラッグで回す） / クリック = 点 / 点→点ドラッグ = 線（交わる線は引けない） / D = 最後の線を消す / C = 全部消す
-- ============================================
module Main (main) where

import Art
import Data.Array (accumArray, assocs)
import Data.Bits ((.&.))
import Data.List (elemIndex, findIndex, foldl', sort, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as Set

autoSpin :: Float
autoSpin = 0.006     -- ★ 自動回転の速さ

width, height :: Float
width = 1280
height = 800

type P = (Float, Float)

-- | 面を数えた結果
data Faces = Faces { fF :: !Int, fPolys :: ![[P]], fComps :: !Int }

-- | 多面体：名前、頂点、面（頂点の番号の列）、辺の数
data Solid = Solid { soName :: String, soV :: [V3], soF :: [[Int]], soE :: Int }

data World = World
  { wMode :: !Int
  , wVerts :: ![P]
  , wEdges :: ![(Int, Int)]
  , wDragFrom :: !Int
  , wWarn :: !Int
  , wFaces :: !Faces
  , wSi :: !Int
  , wRx, wRy :: !Float          -- 立体の回転
  , wPrev :: !P                 -- 前のフレームのマウス（movedX, movedY の代わり）
  }

main :: IO ()
main = runSketch (sketch "27-02 Euler Characteristic V - E + F" world0)
  { skBack  = hsb 170 35 12
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = \env w -> drawWorld (toP5 (envMouse env)) w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- | はじめの例：三角形の中に点
world0 :: World
world0 =
  let cx = width * 0.35
      cy = height * 0.55
      s = min width height * 0.28
      vs = [(cx, cy - s), (cx + s, cy + s * 0.7), (cx - s, cy + s * 0.7), (cx, cy + s * 0.1)]
      es = [(0, 1), (1, 2), (2, 0), (3, 0), (3, 1)]
  in World 1 vs es (-1) 0 (countFaces vs es) 0 (-0.5) 0.6 (0, 0)

-- | ★ 面を数える：各点で線を角度順に並べ、「左手を壁につけて歩く」と 1 周で 1 つの面になる
countFaces :: [P] -> [(Int, Int)] -> Faces
countFaces verts edges =
  let vm = IM.fromList (zip [0 ..] verts)
      pos i = IM.findWithDefault (0, 0) i vm
      nbRaw = IM.fromListWith (flip (++)) (concat [[(a, [b]), (b, [a])] | (a, b) <- edges])
      ang v p = let (vx, vy) = pos v; (px, py) = pos p in atan2 (py - vy) (px - vx)
      nb = IM.mapWithKey (\v l -> sortOn (ang v) l) nbRaw
      neighbors i = IM.findWithDefault [] i nb
      -- a → b の次に歩く辺
      nextOf a b =
        let l = neighbors b
            n = length l
            i = fromMaybe 0 (elemIndex a l)
        in fromMaybe a (nth ((i - 1 + n) `mod` max 1 n) l)
      walk seen a b acc guard
        | Set.member (a, b) seen || guard >= (10000 :: Int) = (seen, reverse acc)
        | otherwise = walk (Set.insert (a, b) seen) b (nextOf a b) (pos a : acc) (guard + 1)
      orbitStep (seen, orbits, polys) (u, v)
        | Set.member (u, v) seen = (seen, orbits, polys)
        | otherwise =
            let (seen', poly) = walk seen u v [] 0
                area = sum (zipWith (\(px, py) (qx, qy) -> px * qy - qx * py) poly (rotate1 poly))
            in (seen', orbits + 1, if area > 1 then polys ++ [poly] else polys)   -- 内側の面（外側の面は向きが逆になる）
      (_, orbitsN, polysAll) =
        foldl' orbitStep (Set.empty, 0 :: Int, []) [(u, v) | u <- [0 .. length verts - 1], v <- neighbors u]
      -- つながりの数（孤立した点も 1 つ）
      n = length verts
      merge lab (a, b) =
        let la = IM.findWithDefault a a lab
            lb = IM.findWithDefault b b lab
        in if la == lb then lab else IM.map (\x -> if x == la then lb else x) lab
      labels = foldl' merge (IM.fromList [(i, i) | i <- [0 .. n - 1]]) edges
      comps = Set.size (Set.fromList (IM.elems labels))
      isolated = length [i | i <- [0 .. n - 1], null (neighbors i)]
  in Faces (orbitsN + isolated - comps + 1) polysAll comps

---------------------------------------------------------------- 動き

stepWorld :: Env -> World -> World
stepWorld env w =
  let m@(mx, my) = toP5 (envMouse env)
      (px, py) = wPrev w
      w1 | wMode w /= 2 = w
         | envDown env = w { wRy = wRy w + (mx - px) * 0.01, wRx = wRx w + (my - py) * 0.01 }
         | otherwise = w { wRy = wRy w + autoSpin }
  in w1 { wPrev = m, wWarn = max 0 (wWarn w1 - 1) }

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char '1') Down _ _ -> w { wMode = 1 }
  EventKey (Char '2') Down _ _ -> w { wMode = 2, wDragFrom = -1 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSi = (wSi w + 1) `mod` nS }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSi = (wSi w + nS - 1) `mod` nS }
  EventKey (Char c) Down _ _
    | c `elem` "dD", not (null (wEdges w)) -> setGraph (wVerts w) (init (wEdges w)) w
    | c `elem` "cC" -> setGraph [] [] w { wDragFrom = -1 }
  EventKey (MouseButton LeftButton) Down _ pos | wMode w == 1 ->
    let m@(mx, my) = toP5 pos
        v = vertexAt w m
    in if v >= 0 then w { wDragFrom = v }
       else if my > 90 && mx < width * 0.66 then setGraph (wVerts w ++ [m]) (wEdges w) w
       else w
  EventKey (MouseButton LeftButton) Up _ pos ->
    let u = vertexAt w (toP5 pos)
        a = wDragFrom w
        w1 | a >= 0 && u >= 0 && u /= a && not (any (\(x, y) -> (x == u && y == a) || (y == u && x == a)) (wEdges w)) =
               if crossesAny w a u then w { wWarn = 120 } else setGraph (wVerts w) (wEdges w ++ [(a, u)]) w
           | otherwise = w
    in w1 { wDragFrom = -1 }
  _ -> w
  where nS = length solids

setGraph :: [P] -> [(Int, Int)] -> World -> World
setGraph vs es w = w { wVerts = vs, wEdges = es, wFaces = countFaces vs es }

---------------------------------------------------------------- 描画

drawWorld :: P -> World -> Picture
drawWorld mouse w = Pictures
  [ if wMode w == 1 then drawGraph mouse w else drawSolid w
  , txt 20 40 16 (gray 1) "1: graph  2: solids (<- ->, drag)  Click: point  Point->point drag: line  D: undo line  C: clear"
  , txt 20 68 14 (gray 0.6) "V = vertices, E = edges, F = faces. In a planar graph the big outer face also counts as 1." ]

drawGraph :: P -> World -> Picture
drawGraph mouse w =
  let faces = wFaces w
      v = length (wVerts w)
      e = length (wEdges w)
      f = fF faces
      chi = v - e + f
      x0 = width * 0.7
      y0 = height * 0.3
      pos i = fromMaybe (0, 0) (nth i (wVerts w))
  in Pictures $
       -- 面に色を塗る
       [ scanFill (hsba (fromIntegral ((k * 47) `mod` 360)) 45 55 0.6) poly' | (k, poly') <- zip [0 :: Int ..] (fPolys faces) ]
       ++ [ Color (gray 0.9) (thickLine 4 (pt (pos a)) (pt (pos b))) | (a, b) <- wEdges w ]
       ++ [ if wDragFrom w >= 0 then Color (hsb 45 80 100) (thickLine 2 (pt (pos (wDragFrom w))) (pt mouse)) else Blank ]
       ++ [ at (pt p) (Color (hsb 45 90 100) (disc 9)) | p <- wVerts w ]
       ++ [ txt x0 y0 40 (gray 1) ("V = " ++ show v)
          , txt x0 (y0 + 56) 40 (gray 1) ("E = " ++ show e)
          , txt x0 (y0 + 112) 40 (gray 1) ("F = " ++ show f)
          , txt (x0 - 40) (y0 + 200) 36 (hsb (if chi == 2 then 140 else 45) 70 100) ("V - E + F = " ++ show chi)
          , txt x0 (y0 + 240) 14 (gray 0.7) ("Painted faces " ++ show (length (fPolys faces)) ++ " + outer 1 = F")
          , txt x0 (y0 + 266) 14 (gray 0.7) (show (fComps faces) ++ " piece(s): V - E + F = 1 + " ++ show (fComps faces))
          ]
       ++ [ txt 20 (height - 40) 20 (hsb 0 80 100) "Crossing lines are not allowed (planar graph)" | wWarn w > 0 ]

drawSolid :: World -> Picture
drawSolid w =
  let s = fromMaybe (Solid "" [] [] 0) (nth (wSi w) solids)
      r = min width height * 0.25
      cx = width * 0.35
      cy = height * 0.55
      projected = IM.fromList (zip [0 ..] (map (projectP (wRx w) (wRy w) r cx cy) (soV s)))
      get i = IM.findWithDefault (0, 0, 0) i projected
      xy (x, y, _) = (x, y)
      zOf (_, _, z) = z
      faceZ f = sum (map (zOf . get) f) / fromIntegral (max 1 (length f))
      ordered = depthSort (faceZ . snd) (zip [0 :: Int ..] (soF s))
      facePic (i, f) =
        let ps = map (pt . xy . get) f
        in Pictures
             [ Color (hsba (fromIntegral ((i * 37) `mod` 360)) 50 80 0.45) (Polygon ps)
             , Color (hsba 0 0 100 0.8) (thickPath 2 (ps ++ take 1 ps)) ]
      x0 = width * 0.64
      y0 = 130
      nSol = fromIntegral (length solids)
      row (k, t) =
        let y = y0 + 40 + fromIntegral k * 34
            on = k == wSi w
            c = hsb (if on then 45 else 0) (if on then 80 else 0) (if on then 100 else 75)
            nv = length (soV t)
            nf = length (soF t)
        in Pictures
             [ txt x0 y 16 c (soName t), txt (x0 + 180) y 16 c (show nv), txt (x0 + 230) y 16 c (show (soE t))
             , txt (x0 + 280) y 16 c (show nf), txt (x0 + 340) y 16 c (show (nv - soE t + nf)) ]
  in Pictures $
       map facePic ordered
       ++ [ at (pt (xy p)) (Color (hsb 45 90 100) (disc 3.5)) | p <- IM.elems projected ]
       ++ [ txt x0 y0 16 (gray 1) "Solid", txt (x0 + 180) y0 16 (gray 1) "V", txt (x0 + 230) y0 16 (gray 1) "E"
          , txt (x0 + 280) y0 16 (gray 1) "F", txt (x0 + 340) y0 16 (gray 1) "V-E+F" ]
       ++ map row (zip [0 :: Int ..] solids)
       ++ [ txt x0 (y0 + 60 + nSol * 34) 14 (gray 0.7) "Same shape as a sphere (no hole): chi = 2"
          , txt x0 (y0 + 84 + nSol * 34) 14 (gray 0.7) "Donut shape (1 hole): chi = 0"
          , txt x0 (y0 + 108 + nSol * 34) 14 (gray 0.7) "g holes: chi = 2 - 2g" ]

-- | p5.js と同じ回転と透視（y は下向き）。戻り値は (x, y, 奥行き)
projectP :: Float -> Float -> Float -> Float -> Float -> V3 -> (Float, Float, Float)
projectP rx ry r cx cy (V3 x y z) =
  let y1 = y * cos rx - z * sin rx
      z1 = y * sin rx + z * cos rx
      x2 = x * cos ry + z1 * sin ry
      z2 = -x * sin ry + z1 * cos ry
      k = 4 / (4 - z2)
  in (cx + x2 * r * k, cy + y1 * r * k, z2)

vertexAt :: World -> P -> Int
vertexAt w m = fromMaybe (-1) (findIndex (\v -> dist2 v m < 14) (wVerts w))

-- ---------- やってみよう ----------
-- 1. どんなに複雑な絵を描いても V − E + F = 2。線を 1 本足すと E と F（または V）がどう変わるから？
-- 2. 離れた 2 つの絵を描くと 3 になる。つながりの数との関係は？
-- 3. サッカーボールは五角形 12 枚と六角形 20 枚。χ = 2 から「五角形はいつも 12 枚」を導いてみよう
-- 4. トーラスの分割数（buildSolids の 8 と 4）を変えても χ = 0 のまま。なぜ？

-- ==================================================
-- ここから下は「道具」（座標の変換、塗りつぶし、交差判定、多面体のデータづくり）
-- ==================================================

pt :: P -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

toP5 :: Point -> P
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = let (gx, gy) = pt (x, y) in label gx gy s c str

nth :: Int -> [a] -> Maybe a
nth i xs = case drop i xs of
  (x : _) | i >= 0 -> Just x
  _ -> Nothing

rotate1 :: [a] -> [a]
rotate1 [] = []
rotate1 (x : xs) = xs ++ [x]

-- | へこみのある面の塗りつぶし（gloss の Polygon は凸形しか正しく塗れない）。横の帯ごとに偶奇で塗る
scanFill :: Color -> [P] -> Picture
scanFill c ps = Color c (Pictures [ span1 r a b | (r, xs) <- assocs table, (a, b) <- pairs (sort xs) ])
  where
    step = 3 :: Float
    rows = floor (height / step) - 1 :: Int
    hits = concatMap edgeHits (zip ps (rotate1 ps))
    edgeHits ((ax, ay), (bx, by))
      | ay == by = []
      | otherwise =
          let lo = max 0 (ceiling (min ay by / step - 0.5)) :: Int
              hi = min rows (ceiling (max ay by / step - 0.5) - 1) :: Int
          in [ (r, ax + (bx - ax) * (y - ay) / (by - ay)) | r <- [lo .. hi], let y = (fromIntegral r + 0.5) * step ]
    table = accumArray (flip (:)) [] (0, rows) hits
    span1 r a b =
      let (gx, gy) = pt ((a + b) / 2, (fromIntegral r + 0.5) * step)
      in rect gx gy (b - a) step
    pairs (a : b : more) = (a, b) : pairs more
    pairs _ = []

crossesAny :: World -> Int -> Int -> Bool
crossesAny w a b =
  let pos i = fromMaybe (0, 0) (nth i (wVerts w))
  in or [ segX (pos a) (pos b) (pos ea) (pos eb)
        | (ea, eb) <- wEdges w, ea /= a, eb /= a, ea /= b, eb /= b ]

segX :: P -> P -> P -> P -> Bool
segX p q r s =
  let o (ax, ay) (bx, by) (cx, cy) = signum ((bx - ax) * (cy - ay) - (by - ay) * (cx - ax))
  in o p q r /= o p q s && o r s p /= o r s q

sortAround :: V3 -> [Int] -> (Int -> V3) -> [Int]
sortAround c idx vAt =
  let n = unit3 c
      V3 nx _ _ = n
      t = if abs nx < 0.9 then V3 1 0 0 else V3 0 1 0
      u = unit3 (cross3 n t)
      w = cross3 n u
      ang i = let d = sub3 (vAt i) c in atan2 (dot3 d w) (dot3 d u)
  in sortOn ang idx

addSolid :: String -> [V3] -> [[Int]] -> Solid
addSolid name v f =
  let es = Set.fromList [ (min a b, max a b) | face <- f, (a, b) <- zip face (rotate1 face) ]
      m = maximum (1e-6 : map norm3 v)
  in Solid name (map (scale3 (1 / m)) v) f (Set.size es)

solids :: [Solid]
solids =
  [ addSolid "Tetrahedron" [V3 1 1 1, V3 1 (-1) (-1), V3 (-1) 1 (-1), V3 (-1) (-1) 1] [[0, 1, 2], [0, 1, 3], [0, 2, 3], [1, 2, 3]]
  , addSolid "Cube" cv [[0, 1, 3, 2], [4, 5, 7, 6], [0, 1, 5, 4], [2, 3, 7, 6], [0, 2, 6, 4], [1, 3, 7, 5]]
  , addSolid "Octahedron" [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1)]
      [[a, b, c] | a <- [0, 1], b <- [2, 3], c <- [4, 5]]
  , addSolid "Icosahedron" iv ifc
  , addSolid "Dodecahedron" dv df                      -- 二十面体の面の中心を結ぶ
  , addSolid "Soccer ball" tv tf                       -- 二十面体の角を切り落とす
  , addSolid "Torus" torV torF                         -- ドーナツを四角形で分割
  ]
  where
    cv = [ V3 (bit' i 1) (bit' i 2) (bit' i 4) | i <- [0 .. 7 :: Int] ]
    bit' i b = if i .&. b /= 0 then 1 else -1
    g = (1 + sqrt 5) / 2 :: Float
    iv = concat [ [V3 0 s1 (s2 * g), V3 s1 (s2 * g) 0, V3 (s2 * g) 0 s1] | s1 <- [-1, 1], s2 <- [-1, 1] ]
    ivAt i = fromMaybe (V3 0 0 0) (nth i iv)
    near i j = abs (norm3 (sub3 (ivAt i) (ivAt j)) - 2) < 1e-3
    ifc = [ [i, j, k] | i <- [0 .. 11], j <- [i + 1 .. 11], k <- [j + 1 .. 11], near i j, near j k, near i k ]
    avg3 f = scale3 (1 / 3) (foldr add3 (V3 0 0 0) (map ivAt f))
    dv = map avg3 ifc
    dvAt i = fromMaybe (V3 0 0 0) (nth i dv)
    df = [ sortAround (ivAt i) [k | (k, f) <- zip [0 ..] ifc, i `elem` f] dvAt | i <- [0 .. 11] ]
    -- 切り落としの点：辺 i→j の 1/3 の所
    nearPairs = [ (i, j) | i <- [0 .. 11], j <- [0 .. 11], i /= j, near i j ]
    ptIdx = M.fromList (zip nearPairs [0 ..])
    pt3 i j = M.findWithDefault 0 (i, j) ptIdx
    tv = [ add3 (ivAt i) (scale3 (1 / 3) (sub3 (ivAt j) (ivAt i))) | (i, j) <- nearPairs ]
    tvAt i = fromMaybe (V3 0 0 0) (nth i tv)
    tf = [ [pt3 a b, pt3 b a, pt3 b c, pt3 c b, pt3 c a, pt3 a c] | [a, b, c] <- ifc ]
         ++ [ sortAround (ivAt i) [pt3 i j | j <- [0 .. 11], j /= i, near i j] tvAt | i <- [0 .. 11] ]
    mM = 8 :: Int
    nN = 4 :: Int
    rr = 0.45 :: Float
    torV = [ V3 ((1 + rr * cos v) * cos u) (rr * sin v) ((1 + rr * cos v) * sin u)
           | i <- [0 .. mM - 1], j <- [0 .. nN - 1]
           , let u = fromIntegral i / fromIntegral mM * tau
           , let v = fromIntegral j / fromIntegral nN * tau + pi / 4 ]
    torF = [ [ i * nN + j, ((i + 1) `mod` mM) * nN + j, ((i + 1) `mod` mM) * nN + ((j + 1) `mod` nN), i * nN + ((j + 1) `mod` nN) ]
           | i <- [0 .. mM - 1], j <- [0 .. nN - 1] ]
