-- ============================================
-- 02 エッシャー風の変形タイル（平行移動）
-- 正方形や正六角形の 1 つの辺をへこませたら、向かいの辺を「同じ形」でふくらませる
--   → 平行移動しただけでぴったりかみ合う。エッシャーのトカゲや鳥のタイルの基本
-- 真ん中のタイルの白い点をドラッグ / 1 = 正方形 2 = 正六角形 / N = ランダム / R = 戻す / E = 目 / C = 色 / L = 直線・曲線 / S = 保存
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

cell :: Float
cell = 150              -- ★ タイルの大きさ（ピクセル）

kK :: Int
kK = 3                  -- ★ 1 辺あたりの動かせる点の数

data Handle = Handle { hE, hK :: !Int, hX, hY :: !Float, hBig :: !Bool }

data World = World
  { wBase    :: !Int             -- ★ 1 = 正方形、2 = 正六角形
  , wSmooth  :: !Bool            -- ★ 辺をなめらかにする
  , wEyes    :: !Bool
  , wColors  :: !Int
  , wV       :: V.Vector Point   -- もとの多角形の頂点（タイルの中心が原点）
  , wOffs    :: M.Map (Int, Int) Point   -- (辺, 点) → まっすぐな辺からのずれ
  , wOutline :: [Point]          -- できあがったタイルの輪郭
  , wParts   :: Picture          -- 輪郭を凸の部分に分けて塗る絵
  , wDrag    :: Maybe (Int, Int) -- ドラッグ中の点 (辺, 点)
  , wLast    :: !Point           -- 1 つ前のマウス位置（movedX, movedY を求める）
  }

world0 :: World
world0 = makeBase (World 1 True True 0 V.empty M.empty [] Blank Nothing (0, 0))

main :: IO ()
main = runSketch (sketch "32-02 Escher Translation Tiles" world0)
  { skBack  = rgb255 20 22 30
  , skDraw  = drawAll
  , skEvent = onEvent
  }

palettes :: [[Color]]
palettes =
  [ [rgb255 240 120 90, rgb255 250 200 90, rgb255 90 170 230]
  , [rgb255 60 70 110, rgb255 230 230 220, rgb255 200 80 90]
  , [rgb255 120 200 150, rgb255 40 120 110, rgb255 240 220 160]
  ]

makeBase :: World -> World
makeBase w =
  let sq = wBase w == 1
      m = if sq then 4 else 6 :: Int
      r = if sq then cell / sqrt 2 else cell / 1.8
      ang i = if sq then pi / 4 + fromIntegral i * tau / 4 + pi else pi / 6 + fromIntegral i * tau / 6 + pi
      vs = V.fromList [ (r * cos (ang i), r * sin (ang i)) | i <- [0 .. m - 1] ]
  in rebuild w { wV = vs, wOffs = M.empty }

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      ((ax, ay), (bx, by)) = latticeVectors w
      pal = case drop (wColors w) palettes of
        (p : _) -> p
        [] -> []
      range = ceiling (max wd h / cell) + 2 :: Int
      shape = Pictures [wParts w, Color (rgb255 20 22 30) (Line (closed (wOutline w))), if wEyes w then drawEye (cell * 0.12) (negate cell * 0.1) else Blank]
      tile i j =
        let ox = fromIntegral i * ax + fromIntegral j * bx
            oy = fromIntegral i * ay + fromIntegral j * by
            ci = if wBase w == 1 then (i + j) `mod` 2 else (i - j) `mod` 3
            c = case drop ci pal of
              (x : _) -> x
              [] -> white
        in if abs ox > wd / 2 + cell * 1.5 || abs oy > h / 2 + cell * 1.5 then Blank
           else Translate ox oy (Color c shape)
      dragE = fmap fst (wDrag w)
      hs = [ Pictures [ Color (gray 0.08) (at (hX d, hY d) (disc (rad + 1)))
                      , Color (if Just (hE d) == dragE then white else gray 0.9) (at (hX d, hY d) (disc rad)) ]
           | d <- handles w, let rad = if hBig d then 7 else 4.5 ]
      title = (if wBase w == 1 then "Square" else "Hexagon") ++ " -> translation tile  (1 / 2 to switch)"
  in Pictures
    [ -- 中心が原点、y は下向き（p5.js の translate(width / 2, height / 2) と同じ）
      Scale 1 (-1) (Pictures
        ( [ tile i j | i <- [-range .. range], j <- [-range .. range] ]
          ++ [ Color white (thickPath 3 (closed (wOutline w))) ] ++ hs ))
    , onP5 env (Color (rgba255 0 0 0 160) (box 10 10 800 76))
    , txt env 22 38 19 white title
    , txt env 22 66 15 (gray 0.78) "Drag a white dot: the opposite edge moves too (big & small dots pair). N random  R reset"
    ]

-- 平行移動の 2 つの向き
latticeVectors :: World -> (Point, Point)
latticeVectors w
  | wBase w == 1 = (vsub (vAt w 1) (vAt w 0), vsub (vAt w 3) (vAt w 0))
  | otherwise    = (vsub (vAt w 4) (vAt w 0), vsub (vAt w 5) (vAt w 1))

vAt :: World -> Int -> Point
vAt w i = wV w V.! (i `mod` V.length (wV w))

offAt :: World -> Int -> Int -> Point
offAt w e k = M.findWithDefault (0, 0) (e, k) (wOffs w)

-- ★ 辺 e の点の列（まっすぐな辺 + ずれ）
edgePoints :: World -> Int -> [Point]
edgePoints w e =
  let a@(ax, ay) = vAt w e
      b@(bx, by) = vAt w (e + 1)
      mid k = let t = fromIntegral (k + 1) / fromIntegral (kK + 1)
                  (dx, dy) = offAt w e k
              in (ax + (bx - ax) * t + dx, ay + (by - ay) * t + dy)
      pts = a : map mid [0 .. kK - 1] ++ [b]
  in if wSmooth w then smoothLine pts 8 else pts

-- 輪郭：前半の辺はそのまま、後半の辺は「向かいの辺を平行移動して逆向き」にしたもの
rebuild :: World -> World
rebuild w =
  let m = V.length (wV w)
      h = m `div` 2
      edgeOf e
        | e < h = edgePoints w e
        | otherwise =
            let (tx, ty) = vsub (vAt w (e + 1)) (vAt w (e - h))
            in reverse (map (\(x, y) -> (x + tx, y + ty)) (edgePoints w (e - h)))
      outline = concatMap (dropLast . edgeOf) [0 .. m - 1]
  in w { wOutline = outline, wParts = Pictures (map Polygon (convexParts (everyOther outline))) }   -- 塗りは点を半分にして軽く

-- 動かせる点（もとの辺の点 = 大きい点と、向かいの辺のコピー = 小さい点）
handles :: World -> [Handle]
handles w =
  let m = V.length (wV w)
      h = m `div` 2
  in concat
    [ [Handle e k x y True, Handle e k (x + tx) (y + ty) False]
    | e <- [0 .. h - 1]
    , let (ax, ay) = vAt w e
          (bx, by) = vAt w (e + 1)
          (tx, ty) = vsub (vAt w (e + h + 1)) (ax, ay)
    , k <- [0 .. kK - 1]
    , let t = fromIntegral (k + 1) / fromIntegral (kK + 1)
          (dx, dy) = offAt w e k
          x = ax + (bx - ax) * t + dx
          y = ay + (by - ay) * t + dy ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ (px, py) ->
    let m = (px, negate py)
        pick (best, bd) d =
          let e = dist2 (hX d, hY d) m
          in if e < bd then (Just (hE d, hK d), e) else (best, bd)
    in pure w { wDrag = fst (foldl' pick (Nothing, 20) (handles w)), wLast = (px, py) }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = Nothing }
  EventMotion (px, py) -> case wDrag w of
    Nothing -> pure w { wLast = (px, py) }
    Just key ->
      let (lx, ly) = wLast w
          lim = clampF (negate cell * 0.45) (cell * 0.45)
          (ox, oy) = M.findWithDefault (0, 0) key (wOffs w)
          o' = (lim (ox + px - lx), lim (oy - (py - ly)))
      in pure (rebuild w { wOffs = M.insert key o' (wOffs w), wLast = (px, py) })
  EventKey (Char c) Down _ _
    | c == '1' || c == '2' -> pure (makeBase w { wBase = fromEnum c - 48 })
    | c == 'r' || c == 'R' -> pure (makeBase w)
    | c == 'n' || c == 'N' -> randomize w
    | c == 'e' || c == 'E' -> pure w { wEyes = not (wEyes w) }
    | c == 'c' || c == 'C' -> pure w { wColors = (wColors w + 1) `mod` length palettes }
    | c == 'l' || c == 'L' -> pure (rebuild w { wSmooth = not (wSmooth w) })
    | c == 's' || c == 'S' -> saveSvg "escher_tile" env (drawAll env w) >> pure w
  _ -> pure w

randomize :: World -> IO World
randomize w = do
  let h = V.length (wV w) `div` 2
      r = randomRIO (-0.25, 0.25) :: IO Float
  ps <- mapM (\key -> do { a <- r; b <- r; pure (key, (a * cell, b * cell)) })
             [ (e, k) | e <- [0 .. h - 1], k <- [0 .. kK - 1] ]
  pure (rebuild w { wOffs = M.fromList ps })

-- ---------- やってみよう ----------
-- 1. 正方形で、上の辺をへこませると下の辺がふくらむ。動物の頭としっぽを作ってみよう
-- 2. 正六角形は 3 組の辺のペア。鳥や魚の形に挑戦（L で直線 / 曲線を切りかえ）
-- 3. kK を 5 にすると細かい形が作れる。drawEye を改造して顔を描こう
-- 4. 辺の点を大きく動かしすぎると線が交差してタイルが壊れる。どこまで大丈夫？


-- ==================================================
-- ここから下は「道具」（座標・なめらかな線・目・凹んだ形の塗り方）
-- ==================================================
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

closed :: [a] -> [a]
closed p = p ++ take 1 p

everyOther :: [a] -> [a]
everyOther (a : _ : rest) = a : everyOther rest
everyOther l = l

dropLast :: [a] -> [a]
dropLast = reverse . drop 1 . reverse

vsub :: Point -> Point -> Point
vsub (a, b) (c, d) = (a - c, b - d)

-- 点の列をキャットマル・ロム曲線でなめらかにする（両端は固定）
smoothLine :: [Point] -> Int -> [Point]
smoothLine pts seg = go (take 1 pts ++ pts ++ lastOf pts)
  where
    lastOf = take 1 . reverse
    go (p0 : p1 : p2 : p3 : rest) = spans p0 p1 p2 p3 ++ go (p1 : p2 : p3 : rest)
    go _ = lastOf pts
    spans (ax, ay) (bx, by) (cx, cy) (dx, dy) =
      [ (f ax bx cx dx t, f ay by cy dy t) | s <- [0 .. seg - 1], let t = fromIntegral s / fromIntegral seg ]
    f a b c d t = 0.5 * (2 * b + (c - a) * t + (2 * a - 5 * b + 4 * c - d) * t * t + (3 * b - a - 3 * c + d) * t * t * t)

drawEye :: Float -> Float -> Picture
drawEye x y = Pictures
  [ Color white (at (x, y) (disc (cell * 0.06)))
  , Color (gray 0.08) (at (x + cell * 0.015, y) (disc (cell * 0.03))) ]

-- gloss の Polygon は凸の形しか正しく塗れないので、
-- 三角形に切って（耳切り法）、くっつけられるものをくっつけて凸の部分に分ける
convexParts :: [Point] -> [[Point]]
convexParts pts0 = foldl' addTri [] (earClip [] (dropStraight (ccw pts0)))

ccw :: [Point] -> [Point]
ccw ps = if sum [ ax * by - bx * ay | ((ax, ay), (bx, by)) <- cyc ps ] < 0 then reverse ps else ps

cyc :: [a] -> [(a, a)]
cyc p = zip p (drop 1 p ++ take 1 p)

cross3p :: Point -> Point -> Point -> Float
cross3p (ax, ay) (bx, by) (cx, cy) = (bx - ax) * (cy - by) - (by - ay) * (cx - bx)

-- 一直線に並んだ点を取り除く
dropStraight :: [Point] -> [Point]
dropStraight ps =
  [ b | (a, b, c) <- zip3 (drop (n - 1) ps ++ ps) ps (drop 1 ps ++ ps), abs (cross3p a b c) > 1e-2 ]
  where n = length ps

earClip :: [[Point]] -> [Point] -> [[Point]]
earClip acc ps = case ps of
  (_ : _ : _ : _ : _) ->
    let n = length ps
        turned i = drop i ps ++ take i ps
        isEar (a : b : c : rest) = cross3p a b c > 0 && not (any (inTri a b c) rest)
        isEar _ = False
        ears = filter isEar (map turned [0 .. n - 1])
    in case (ears, ps) of
         ((a : b : c : rest) : _, _) -> earClip ([a, b, c] : acc) (a : c : rest)
         (_, a : b : c : rest) -> earClip ([a, b, c] : acc) (a : c : rest)   -- 線が交差しているときも止まらないように
         _ -> acc
  [_, _, _] -> ps : acc
  _ -> acc

inTri :: Point -> Point -> Point -> Point -> Bool
inTri a b c p = cross3p a b p >= 0 && cross3p b c p >= 0 && cross3p c a p >= 0

addTri :: [[Point]] -> [Point] -> [[Point]]
addTri polys tri = go polys
  where
    go [] = [tri]
    go (q : qs) = case mergeTri q tri of
      Just m -> m : qs
      Nothing -> q : go qs

mergeTri :: [Point] -> [Point] -> Maybe [Point]
mergeTri poly tri = case tri of
  [x, y, z] -> firstJust [try b a c | (a, b, c) <- [(x, y, z), (y, z, x), (z, x, y)]]
  _ -> Nothing
  where
    -- tri の辺 a→b が、poly の辺 b→a と同じなら、頂点 c をはさむ
    try a b c = firstJust
      [ if isConvex m then Just m else Nothing
      | (i, (p, q)) <- zip [1 ..] (cyc poly), p == a, q == b
      , let m = drop i poly ++ take i poly ++ [c] ]
    isConvex m = all (\(a, b, c) -> cross3p a b c >= -1e-3) (zip3 m (drop 1 m ++ take 1 m) (drop 2 m ++ take 2 m))
    firstJust xs = case [ v | Just v <- xs ] of
      (v : _) -> Just v
      [] -> Nothing
