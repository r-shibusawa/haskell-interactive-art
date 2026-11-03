-- ============================================
-- 01 アルキメデスの平面充填（半正則タイリング）
-- 2 種類以上の正多角形を使い、「どの頂点のまわりも同じ並び」になる埋め方はちょうど 8 つ
--   頂点のまわりの多角形を順に読んだ「3.4.6.4」などの数字 = 頂点配置（vertex configuration）
-- 1〜8 = タイリング / マウス = 頂点を調べる / ↑↓ = 拡大縮小 / S = 保存
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', intercalate, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Tile = Tile { tx, ty :: !Double, tn :: !Int, trot :: !Double }   -- 座標は 1 辺 = 1 の世界

-- 頂点と、そのまわりの多角形（向き, 辺の数, タイル）
data Vert = Vert { vx, vy :: !Double, vlist :: [(Double, Int, Tile)] }

data World = World
  { wWhich :: !Int      -- ★ 最初に見るタイリング（1〜8 キーの番号 − 1）
  , wZoom  :: !Float    -- ★ 1 辺の長さ（ピクセル）
  , wVerts :: [Vert]
  , wLayer :: Picture   -- タイリングの絵（作っておいて使い回す）
  }

world0 :: World
world0 = World 7 46 [] Blank

main :: IO ()
main = runSketch (sketch "32-01 Archimedean Tilings" world0)
  { skBack  = backCol
  , skStart = \env -> pure (build env world0)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

backCol :: Color
backCol = rgb255 18 20 28

-- ★ 多角形の色（辺の数で決める）
colorOfN :: Int -> Color
colorOfN n = case n of
  3 -> rgb255 255 150 110
  4 -> rgb255 110 190 255
  6 -> rgb255 130 220 150
  8 -> rgb255 250 200 90
  _ -> rgb255 200 150 250

names :: [String]
names =
  [ "3.3.3.3.6 snub hexagonal", "3.6.3.6 trihexagonal (kagome)", "4.8.8 octagon + square"
  , "3.12.12 dodecagon + triangle", "4.6.12 dodecagon, hexagon, square"
  , "3.3.3.4.4 triangle + square bands", "3.3.4.3.4 snub square", "3.4.6.4 triangle, square, hexagon" ]

-- キー番号 → 下の tilingDef の番号
defIndex :: Int -> Int
defIndex i = case drop i [0, 3, 5, 6, 7, 2, 1, 4] of
  (d : _) -> d
  [] -> 0

build :: Env -> World -> World
build env w =
  let (a1, a2, motif) = tilingDef (defIndex (wWhich w))
      z = wZoom w
      zd = realToFrac z :: Double
      wd = realToFrac (envW env) :: Double
      hd = realToFrac (envH env) :: Double
      reach = sqrt (wd * wd + hd * hd) / 2 / zd + 3
      lmin = min (hyp a1) (hyp a2)
      tiles = generateTiles a1 a2 motif (ceiling (reach / lmin) + 2) reach
      visible t = abs (tx t) * zd < wd / 2 + 2 * zd && abs (ty t) * zd < hd / 2 + 2 * zd
      -- 枠線の代わりに、少し小さく塗って背景を見せる（1 枚 = 図形 1 つで軽い）
      fillTile t = Color (colorOfN (tn t)) (Polygon (shrunk z t))
  in w { wVerts = collectVertices tiles
       , wLayer = Pictures (map fillTile (filter visible tiles)) }

drawAll :: Env -> World -> Picture
drawAll env w =
  let z = wZoom w
      (mx0, my0) = envMouse env
      mx = realToFrac (mx0 / z) :: Double
      my = realToFrac (negate my0 / z) :: Double
      nearest (best, bd) v =
        let dx = vx v - mx
            dy = vy v - my
            d = dx * dx + dy * dy
        in if d < bd then (Just v, d) else (best, bd)
      (bestV, _) = foldl' nearest (Nothing, 1e18) (wVerts w)
      highlight = case bestV of
        Nothing -> Blank
        Just v -> Scale 1 (-1) (Color white (Pictures
          ( at (fl (vx v) * z, fl (vy v) * z) (disc 6)
          : [ thickPath 4 (closed (map (toPx z) (polyPts t))) | (_, _, t) <- vlist v ] )))
      name = case drop (wWhich w) names of
        (s : _) -> s
        [] -> ""
  in Pictures
    [ Scale 1 (-1) (wLayer w)
    , highlight
    , onP5 env (Color (rgba255 0 0 0 170) (box 10 10 560 290))
    , txt env 24 40 20 white (show (wWhich w + 1) ++ ". " ++ name)
    , txt env 24 66 15 (gray 0.78) ("At the mouse vertex: " ++ maybe "-" configOf bestV ++ " (angles add to 360)")
    , Pictures [ txt env 24 (96 + fromIntegral i * 22) 15 (if i == wWhich w then white else gray 0.55) (show (i + 1) ++ "  " ++ s)
               | (i, s) <- zip [0 ..] names ]
    , txt env 24 286 15 (gray 0.63) "Any vertex gives the same numbers = semi-regular"
    ]

-- 頂点のまわりの多角形を、角度の順に読む → 「いちばん小さい読み方」にそろえる
configOf :: Vert -> String
configOf v =
  let s = map (\(_, n, _) -> n) (sortOn (\(a, _, _) -> a) (vlist v))
      rots l = [ drop r l ++ take r l | r <- [0 .. length l - 1] ]
      cands = rots s ++ rots (reverse s)
      smallest = foldl' (\b c -> if null b || c < b then c else b) [] cands
  in intercalate "." (map show smallest)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '8' -> pure (build env w { wWhich = fromEnum c - 49 })
    | c == 's' || c == 'S' -> saveSvg "archimedean" env (drawAll env w) >> pure w
  EventKey (SpecialKey KeyUp) Down _ _   -> pure (build env w { wZoom = min 120 (wZoom w + 6) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (build env w { wZoom = max 14 (wZoom w - 6) })
  EventResize _ -> pure (build env w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 8 つ全部で、頂点を 3 か所調べて「同じ数字」になるか確かめよう
-- 2. 数字の角を足すと 360°：3.4.6.4 → 60 + 90 + 120 + 90。他の 7 つも計算しよう
-- 3. 3.3.4.3.4 と 3.3.3.4.4 は使う形が同じなのに並び方がちがう。何がちがう？
-- 4. colorOfN の色を変えて、自分だけの床タイルのデザインにしよう


-- ==================================================
-- ここから下は「タイリングの道具」（格子 + 単位の模様で 8 つを作る）
-- （中身は読まなくても OK）
-- ==================================================
type D2 = (Double, Double)

fl :: Double -> Float
fl = realToFrac

hyp :: D2 -> Double
hyp (a, b) = sqrt (a * a + b * b)

onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

closed :: [a] -> [a]
closed p = p ++ take 1 p

toPx :: Float -> D2 -> Point
toPx z (x, y) = (fl x * z, fl y * z)

s3, s2 :: Double
s3 = sqrt 3
s2 = sqrt 2

circumR :: Int -> Double
circumR k = 1 / (2 * sin (pi / fromIntegral k))

regTile :: Double -> Double -> Int -> Double -> Tile
regTile = Tile

polyPts :: Tile -> [D2]
polyPts t =
  let r = circumR (tn t)
      ang k = trot t + 2 * pi * fromIntegral k / fromIntegral (tn t)
  in [ (tx t + r * cos (ang k), ty t + r * sin (ang k)) | k <- [0 .. tn t - 1] ]

-- 枠の太さ 2 ピクセルぶん小さくした多角形（画面の座標）
shrunk :: Float -> Tile -> [Point]
shrunk z t =
  let n = fromIntegral (tn t) :: Float
      r = fl (circumR (tn t)) * z - 1 / cos (pi / n)
      ang k = fl (trot t) + tau * fromIntegral k / n
      x0 = fl (tx t) * z
      y0 = fl (ty t) * z
  in [ (x0 + r * cos (ang k), y0 + r * sin (ang k)) | k <- [0 .. tn t - 1 :: Int] ]

dirP :: Double -> Double -> D2
dirP a s = (s * cos a, s * sin a)

rotP :: D2 -> Double -> D2
rotP (x, y) a = (x * cos a - y * sin a, x * sin a + y * cos a)

triFromPts :: D2 -> D2 -> D2 -> Tile
triFromPts (ax, ay) (bx, by) (c1, c2) =
  let cx = (ax + bx + c1) / 3
      cy = (ay + by + c2) / 3
  in regTile cx cy 3 (atan2 (ay - cy) (ax - cx))

-- 各辺の外側に正三角形をつける
edgeTris :: Tile -> [Tile]
edgeTris t =
  let v = polyPts t
  in [ regTile (mx + cos na * s3 / 6) (my + sin na * s3 / 6) 3 na
     | ((ax, ay), (bx, by)) <- zip v (drop 1 v ++ take 1 v)
     , let mx = (ax + bx) / 2
           my = (ay + by) / 2
           na = atan2 (my - ty t) (mx - tx t) ]

-- a1, a2 = ずらす向き（格子）、タイル = 1 マス分の多角形（重複は後で消す）
tilingDef :: Int -> (D2, D2, [Tile])
tilingDef i = case i of
  0 -> ( (2, s3), rotP (2, s3) (p / 3)
       , regTile 0 0 6 0 : concat
           [ [triFromPts v vn pa, triFromPts v pp q, triFromPts v q pa, triFromPts pa q r]
           | k <- [0 .. 5 :: Int]
           , let a = fromIntegral k * p / 3
                 v = dirP a 1
                 vn = dirP (a + p / 3) 1
                 pa = dirP (a + p / 6) s3
                 pp = dirP (a - p / 6) s3
                 q = dirP a 2
                 r = rotP (2.5, s3 / 2) a ] )
  1 -> let sq2 = [regTile 0 0 4 (p / 4), regTile (0.5 + (s3 + 1) / 4) (0.5 + (s3 - 1) / 4) 4 (p / 12)]
       in ((0.5, 1 + s3 / 2), (1 + s3 / 2, -0.5), sq2 ++ concatMap edgeTris sq2)
  2 -> ( (1, 0), (0.5, 1 + s3 / 2)
       , [regTile 0.5 0.5 4 (p / 4), regTile 0.5 (1 + s3 / 6) 3 (p / 2), regTile 1 (1 + s3 / 3) 3 (-p / 2)] )
  3 -> ( (2, 0), (1, s3), [regTile 0 0 6 0, regTile 1 (s3 / 3) 3 (-p / 2), regTile 2 (2 * s3 / 3) 3 (p / 2)] )
  4 -> let l = s3 + 1
       in ( (l, 0), (l / 2, l * s3 / 2)
          , [ regTile 0 0 6 (p / 6), regTile (l / 2) 0 4 (p / 4), regTile (l / 4) (l * s3 / 4) 4 (p / 4 + p / 3)
            , regTile (3 * l / 4) (l * s3 / 4) 4 (p / 4 + 2 * p / 3), regTile (l / 2) (l * s3 / 6) 3 (p / 2)
            , regTile l (l * s3 / 3) 3 (-p / 2) ] )
  5 -> let l = 1 + s2 in ((l, 0), (0, l), [regTile 0 0 8 (p / 8), regTile (l / 2) (l / 2) 4 0])
  6 -> let l = 2 + s3
       in ( (l, 0), (l / 2, l * s3 / 2)
          , [regTile 0 0 12 (p / 12), regTile (l / 2) (l * s3 / 6) 3 (-p / 2), regTile l (l * s3 / 3) 3 (p / 2)] )
  _ -> let l = 3 + s3
       in ( (l, 0), (l / 2, l * s3 / 2)
          , [ regTile 0 0 12 (p / 12), regTile (l / 2) 0 4 (p / 4), regTile (l / 4) (l * s3 / 4) 4 (p / 4 + p / 3)
            , regTile (3 * l / 4) (l * s3 / 4) 4 (p / 4 + 2 * p / 3), regTile (l / 2) (l * s3 / 6) 6 0
            , regTile l (l * s3 / 3) 6 0 ] )
  where p = pi :: Double

generateTiles :: D2 -> D2 -> [Tile] -> Int -> Double -> [Tile]
generateTiles (a1x, a1y) (a2x, a2y) motif range reach =
  reverse (snd (foldl' add (S.empty, []) cands))
  where
    cands = [ t { tx = tx t + fromIntegral i * a1x + fromIntegral j * a2x
                , ty = ty t + fromIntegral i * a1y + fromIntegral j * a2y }
            | i <- [-range .. range], j <- [-range .. range], t <- motif ]
    add acc@(seen, out) t
      | abs (tx t) > reach * 1.2 || abs (ty t) > reach = acc
      | S.member k seen = acc
      | otherwise = (S.insert k seen, t : out)
      where k = (round (tx t * 50) :: Int, round (ty t * 50) :: Int)

collectVertices :: [Tile] -> [Vert]
collectVertices ts = filter full (M.elems (foldl' addTile M.empty ts))
  where
    addTile m t = foldl' (addPt t) m (polyPts t)
    addPt t m (x, y) =
      let k = (round (x * 100) :: Int, round (y * 100) :: Int)
          e = (atan2 (ty t - y) (tx t - x), tn t, t)
      in M.insertWith (\_ old -> old { vlist = e : vlist old }) k (Vert x y [e]) m
    -- 角の合計が 360° になっている（周りが全部そろった）頂点だけ使う
    full v = abs (sum [ pi * fromIntegral (n - 2) / fromIntegral n | (_, n, _) <- vlist v ] - 2 * pi) < 1e-6
