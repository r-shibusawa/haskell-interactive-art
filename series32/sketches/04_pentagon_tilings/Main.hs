-- ============================================
-- 04 五角形のタイリング
-- 正五角形は埋まらない（00）。でも「ゆがんだ凸五角形」なら埋まるものがあり、全部で 15 タイプ（2017 年に完全に分類）
--   ここでは タイプ 1（六角形を半分に切る）・タイプ 4（カイロ・タイル）・タイプ 5（花の形）を、形を変えながら試す
-- 1・2・3 = タイプ / ドラッグ = 白い点を動かして形を変える / ←→ = タイプ 1 の切り方 / ↑↓ = 拡大縮小 / S = 保存
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', intercalate)
import Numeric (showFFloat)

data Target = TH1 | TH2 | TZ4 | TZ5 deriving (Eq)

data World = World
  { wType :: !Int        -- ★ 1 = タイプ 1、2 = タイプ 4（カイロ）、3 = タイプ 5
  , wZoom :: !Float      -- ★ 1 単位の長さ（ピクセル）
  , wZ4   :: !Point      -- タイプ 4 の動かせる頂点（辺がほぼ同じ長さのカイロ・タイル）
  , wZ5   :: !Point      -- タイプ 5 の動かせる頂点（角が 60°・120°×4 の「花」の五角形）
  , wH1   :: !Point      -- タイプ 1：中心対称な六角形の頂点
  , wH2   :: !Point
  , wCut  :: !Float      -- タイプ 1：切る位置（0〜1）
  , wDrag :: !(Maybe Target)
  }

world0 :: World
world0 = World 2 150 (0.36, -0.135) (0.43, -0.08) (0.45, 0.85) (-0.6, 0.8) 0.35 Nothing

main :: IO ()
main = runSketch (sketch "32-04 Pentagon Tilings" world0)
  { skBack  = rgb255 20 22 30
  , skDraw  = drawAll
  , skEvent = onEvent
  }

palette :: [Color]
palette = [rgb255 250 160 90, rgb255 90 180 230, rgb255 240 220 120, rgb255 160 120 220, rgb255 110 210 160, rgb255 240 110 140]

-- ★ タイプごとの作り方：タイル (点, 色番号)、基本の 1 枚、動かせる点
makeTiling :: Env -> World -> ([([Point], Int)], [Point], [Point])
makeTiling env w = case wType w of
  1 ->
    -- 中心対称な六角形 h0 h1 h2 −h0 −h1 −h2 を、中心を通る線で 2 つに切る → 合同な五角形 2 枚
    let (h0x, h0y) = (1, 0)
        h1@(h1x, h1y) = wH1 w
        h2@(h2x, h2y) = wH2 w
        p = (h0x + (h1x - h0x) * wCut w, h0y + (h1y - h0y) * wCut w)
        base = [p, h1, h2, (negate h0x, negate h0y), neg p]
        other = map neg base
        (t1x, t1y) = (h2x - h0x, h2y - h0y)
        (t2x, t2y) = (negate h0x - h1x, negate h0y - h1y)
        tiles = concat
          [ [(map (shift ox oy) base, 0), (map (shift ox oy) other, 1)]
          | i <- [-12 .. 12 :: Int], j <- [-12 .. 12 :: Int]
          , let ox = fromIntegral i * t1x + fromIntegral j * t2x
                oy = fromIntegral i * t1y + fromIntegral j * t2y
          , abs ox <= reach && abs oy <= reach ]
    in (tiles, base, [h1, h2])
  2 ->
    -- 4 回回転の中心 A(0,0) と B(0.5,0.5)。Z を A のまわりに 90° 回すと X、X を B のまわりに 90° 回すと Y
    let a = (0, 0)
        b = (0.5, 0.5)
        z = wZ4 w
        x = rotAround z a (pi / 2)
        y = rotAround x b (pi / 2)
        base = [a, x, b, y, z]
        tiles = [ (map (\v -> shift (fromIntegral i) (fromIntegral j) (rotAround v a (fromIntegral k * pi / 2))) base, k)
                | i <- [-8 .. 8 :: Int], j <- [-8 .. 8 :: Int]
                , abs (fromIntegral i) <= reach && abs (fromIntegral j) <= reach
                , k <- [0 .. 3] ]
    in (tiles, base, [z])
  _ ->
    -- 6 回回転の中心 A(0,0) と 3 回回転の中心 D。花びらのように 6 枚が集まる
    let a = (0, 0)
        d = (0.5, sqrt 3 / 6)
        z = wZ5 w
        x = rotAround z a (pi / 3)
        y = rotAround x d (2 * pi / 3)
        base = [a, x, d, y, z]
        tiles = [ (map (\v -> shift ox oy (rotAround v a (fromIntegral k * pi / 3))) base, k)
                | i <- [-10 .. 10 :: Int], j <- [-10 .. 10 :: Int]
                , let ox = fromIntegral i + 0.5 * fromIntegral j
                      oy = fromIntegral j * sqrt 3 / 2
                , abs ox <= reach && abs oy <= reach
                , k <- [0 .. 5] ]
    in (tiles, base, [z])
  where
    reach = max (envW env) (envH env) / wZoom w / 2 + 1.5
    neg (a, b) = (negate a, negate b)
    shift ox oy (a, b) = (a + ox, b + oy)

drawAll :: Env -> World -> Picture
drawAll env w =
  let (tiles, base, hs) = makeTiling env w
      z = wZoom w
      toPx (a, b) = (a * z, b * z)
      colorOf k = case drop (k `mod` length palette) palette of
        (c : _) -> c
        [] -> white
      -- 線の太さ 2 の代わりに、少し内側に縮めて塗る（すき間に背景が見える）
      tilePic (pts, k) = Color (colorOf k) (Polygon (insetConvex 1 (map toPx pts)))
  in Pictures
    [ Scale 1 (-1) (Pictures
        ( map tilePic tiles
          ++ [ Color white (thickPath 4 (closed (map toPx base))) ]   -- 基本の 1 枚を強調
          ++ [ at (toPx h) (Pictures [Color (gray 0.08) (disc 9), Color white (disc 8)]) | h <- hs ] ))
    , drawInfo env w base
    ]

drawInfo :: Env -> World -> [Point] -> Picture
drawInfo env w base =
  let ang = interiorAngles base
      len = map (uncurry dist2) (cyc base)
      title = case wType w of
        1 -> "Type 1: cut a centrally symmetric hexagon in half"
        2 -> "Type 4: Cairo tile (two 90 deg angles)"
        _ -> "Type 5: 6 tiles make a flower (60 and 120 deg)"
      rule = case wType w of
        1 -> "Rule: two parallel sides (two adjacent angles sum to 180)"
        2 -> "Rule: two angles are 90 at the centers, sides around them equal"
        _ -> "Rule: angles 60 and 120 at the centers, sides around them equal"
      rnd a = show (round a :: Int)
  in Pictures
    [ onP5 env (Color (rgba255 0 0 0 175) (box 10 10 720 200))
    , txt env 22 38 19 white (show (wType w) ++ ". " ++ title)
    , txt env 22 66 15 (gray 0.82) ("Angles: " ++ intercalate "  " (map rnd ang) ++ "   total " ++ rnd (sum ang) ++ " deg")
    , txt env 22 90 15 (gray 0.82) ("Sides: " ++ intercalate "  " (map nf2 len))
    , txt env 22 114 15 (gray 0.82) rule
    , txt env 22 144 15 (gray 0.63) "1918 Reinhardt 1-5 / 1968 Kershner 6-8 / 1975 James 10"
    , txt env 22 166 15 (gray 0.63) "1977 Marjorie Rice (amateur) 9, 11-13 / 1985 Stein 14"
    , txt env 22 188 15 (gray 0.63) "2015 Mann, McLoud, Von Derau 15 (computer search) / 2017 Rao: that's all"
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ (px, py) ->
    let z = wZoom w
        m = (px / z, negate py / z)
        cands = case wType w of
          1 -> [(TH1, wH1 w), (TH2, wH2 w)]
          2 -> [(TZ4, wZ4 w)]
          _ -> [(TZ5, wZ5 w)]
        pick acc (t, c) = if dist2 c m < 25 / z then Just t else acc
    in pure w { wDrag = foldl' pick Nothing cands }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = Nothing }
  EventMotion (px, py) -> case wDrag w of
    Nothing -> pure w
    Just t ->
      let z = wZoom w
          w' = setTarget t (px / z, negate py / z) w
          (_, base, _) = makeTiling env w'
          (hx, hy) = wH1 w'
          (kx, ky) = wH2 w'
          -- 形が壊れる（交差・へこむ）なら元に戻す。凸五角形だけを考える
          hexOk = wType w' /= 1 || isConvex [(1, 0), (hx, hy), (kx, ky), (-1, 0), (negate hx, negate hy), (negate kx, negate ky)]
      in pure (if isSimple base && isConvex base && hexOk then w' else w)
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> pure w { wType = fromEnum c - 48 }
    | c == 's' || c == 'S' -> saveSvg "pentagon_tiling" env (drawAll env w) >> pure w
  EventKey (SpecialKey KeyLeft) Down _ _  -> pure w { wCut = max 0.05 (wCut w - 0.05) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wCut = min 0.95 (wCut w + 0.05) }
  EventKey (SpecialKey KeyUp) Down _ _    -> pure w { wZoom = min 300 (wZoom w + 15) }
  EventKey (SpecialKey KeyDown) Down _ _  -> pure w { wZoom = max 50 (wZoom w - 15) }
  _ -> pure w

setTarget :: Target -> Point -> World -> World
setTarget t p w = case t of
  TH1 -> w { wH1 = p }
  TH2 -> w { wH2 = p }
  TZ4 -> w { wZ4 = p }
  TZ5 -> w { wZ5 = p }

-- ---------- やってみよう ----------
-- 1. タイプ 4 で点を動かし、4 つの辺が同じ長さの「本物のカイロ・タイル」を探そう（カイロの石畳の模様）
-- 2. タイプ 1 は ←→ で切り方を変えても必ず埋まる。なぜ？（六角形が埋まるから、その半分も埋まる）
-- 3. タイプ 5 の花の中心に集まる角は 60° × 6 = 360°。他の頂点の角の合計も確かめよう
-- 4. 調べてみよう：マージョリー・ライスはどうやって新しいタイプを見つけた？


-- ==================================================
-- ここから下は「図形の道具」
-- ==================================================
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

nf2 :: Float -> String
nf2 x = showFFloat (Just 2) x ""

closed :: [a] -> [a]
closed p = p ++ take 1 p

cyc :: [a] -> [(a, a)]
cyc p = zip p (drop 1 p ++ take 1 p)

triples :: [a] -> [(a, a, a)]
triples p = zip3 p (drop 1 p ++ take 1 p) (drop 2 p ++ take 2 p)

rotAround :: Point -> Point -> Float -> Point
rotAround (px, py) (cx, cy) a =
  let x = px - cx
      y = py - cy
  in (cx + x * cos a - y * sin a, cy + x * sin a + y * cos a)

area2 :: [Point] -> Float
area2 pts = sum [ ax * by - bx * ay | ((ax, ay), (bx, by)) <- cyc pts ]

-- 内角（へこんだ角にも対応）。各頂点について「前の点, その点, 次の点」で計算する
interiorAngles :: [Point] -> [Float]
interiorAngles pts =
  let sg = if area2 pts > 0 then 1 else -1
      turnAt ((ax, ay), (bx, by), (cx, cy)) =
        let e1x = bx - ax
            e1y = by - ay
            e2x = cx - bx
            e2y = cy - by
            turn = atan2 (e1x * e2y - e1y * e2x) (e1x * e2x + e1y * e2y)
        in (pi - sg * turn) * 180 / pi
      n = length pts
      prevs = drop (n - 1) pts ++ take (n - 1) pts
  in map turnAt (zip3 prevs pts (drop 1 pts ++ take 1 pts))

isSimple :: [Point] -> Bool
isSimple pts =
  let n = length pts
      at' i = case drop (i `mod` n) pts of
        (v : _) -> v
        [] -> (0, 0)
      o (px, py) (qx, qy) (rx, ry) = signum ((qx - px) * (ry - py) - (qy - py) * (rx - px))
      crossing i j =
        let a = at' i
            b = at' (i + 1)
            c = at' j
            d = at' (j + 1)
        in o a b c * o a b d < 0 && o c d a * o c d b < 0
  in not (or [ crossing i j | i <- [0 .. n - 1], j <- [i + 2 .. n - 1], not (i == 0 && j == n - 1) ])

isConvex :: [Point] -> Bool
isConvex pts =
  let turns = [ signum ((bx - ax) * (cy - by) - (by - ay) * (cx - bx)) | ((ax, ay), (bx, by), (cx, cy)) <- triples pts ]
      nz = filter (/= 0) turns
  in all (== 1) nz || all (== (-1)) nz

-- 凸多角形の各辺を内側へ d だけ動かした多角形
insetConvex :: Float -> [Point] -> [Point]
insetConvex d pts =
  let sg = if area2 pts > 0 then 1 else -1 :: Float
      normal ((ax, ay), (bx, by)) =
        let dx = bx - ax
            dy = by - ay
            l = max 1e-6 (sqrt (dx * dx + dy * dy))
        in (negate dy / l * sg, dx / l * sg)
      ns = map normal (cyc pts)
      n = length pts
      prevN = drop (n - 1) ns ++ take (n - 1) ns
      move (x, y) (ax, ay) (bx, by) =
        let k = d / max 0.2 (1 + ax * bx + ay * by)
        in (x + (ax + bx) * k, y + (ay + by) * k)
  in zipWith3 move pts prevN ns
