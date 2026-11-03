-- ============================================
-- 14 折り紙パターン・ブラシ（自由制作のベース）
-- マス目の上に山折り・谷折りの線を描くと、対称に増えて、右側でタイルのように敷きつめられる（テッセレーション）
--   頂点ごとに前川の定理（M − V = ±2）をチェック。線を引くたびに音が鳴る
-- ドラッグ = 折り線を描く / M・V = 山・谷 / 1〜4 = 対称（なし・鏡・4 回回転・8 方向）/ P = お手本 / Z = 戻す / C = 消す / S = 保存
-- ============================================
module Main (main) where

import Art
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_, void)
import Data.Char (toLower)
import qualified Data.Map.Strict as M

gridN :: Int
gridN = 8                  -- ★ 1 マスの分割数

maxSegs, maxUndo :: Int
maxSegs = 400              -- ★ 単位の線分の上限
maxUndo = 100              -- ★ Z で戻れる回数

-- 単位の線分 (i1, j1) - (i2, j2)。True = 山
data Seg = Seg !Int !Int !Int !Int !Bool

data World = World
  { wMv    :: !Bool        -- True = 山
  , wSym   :: !Int         -- 1 = なし、2 = 左右の鏡、3 = 4 回回転、4 = 8 方向（鏡 + 回転）
  , wSegs  :: [Seg]
  , wUndo  :: [[Seg]]
  , wStart :: !(Maybe (Int, Int))
  , wTiles :: Picture      -- 描いておいたタイル（線が変わるまで使い回す）
  }

main :: IO ()
main = runSketch (sketch "26-14 Crease Pattern Brush" (retile 1280 800 (presetWorld (World True 4 [] [] Nothing Blank))))
  { skBack  = rgb255 34 32 40
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 編集マスの大きさ（左上は (40, 120)）
cellSize :: Env -> Float
cellSize env = min (envW env * 0.4) (envH env - 200)

cx0, cy0 :: Float
cx0 = 40
cy0 = 120

-- ★ 対称の変換（マスの中心のまわり）。座標は 0〜N の整数
symCopies :: Int -> (Int, Int) -> [(Int, Int)]
symCopies sym (i, j) =
  let n = gridN
      base | sym == 2 = [(i, j), (n - i, j)]
           | sym >= 3 = [(i, j), (n - j, i), (n - i, n - j), (j, n - i)]
           | otherwise = [(i, j)]
  in if sym == 4 then base ++ [(n - a, b) | (a, b) <- base] else base

-- 線を引く：対称にコピーし、格子点ごとの短い線分に分けて登録
addStroke :: (Int, Int) -> (Int, Int) -> World -> (World, Bool, Int)
addStroke a b w =
  let as = symCopies (wSym w) a
      bs = symCopies (wSym w) b
      units =
        [ (i1 + di * s, j1 + dj * s, i1 + di * (s + 1), j1 + dj * (s + 1))
        | ((i1, j1), (i2, j2)) <- zip as bs
        , let g = gcd (abs (i2 - i1)) (abs (j2 - j1))
        , g /= 0
        , let di = (i2 - i1) `div` g
        , let dj = (j2 - j1) `div` g
        , s <- [0 .. g - 1] ]
      put segs (p, q, r, t)
        | any same segs = map (\e -> if same e then setMv e else e) segs
        | length segs >= maxSegs = segs
        | otherwise = segs ++ [Seg p q r t (wMv w)]
        where
          same (Seg e1 e2 e3 e4 _) = (e1 == p && e2 == q && e3 == r && e4 == t) || (e1 == r && e2 == t && e3 == p && e4 == q)
          setMv (Seg e1 e2 e3 e4 _) = Seg e1 e2 e3 e4 (wMv w)
      added = not (null units)
  in if added
       then (w { wSegs = foldl put (wSegs w) units, wUndo = take maxUndo (wSegs w : wUndo w) }, True, length as)
       else (w, False, length as)

-- ★ 音：長い線ほど低く、山は三角波・谷はサイン波。対称のコピーの数だけ和音
strokeSound :: Bool -> (Int, Int) -> (Int, Int) -> Int -> IO ()
strokeSound mv (i1, j1) (i2, j2) copies = do
  let len = dist2 (fromIntegral i1, fromIntegral j1) (fromIntegral i2, fromIntegral j2)
      base = 880 / (1 + len * 0.5)
      wave = if mv then Triangle else Sine
  forM_ (zip [0 :: Int ..] (take (min copies 4) [1, 1.25, 1.5, 2])) $ \(k, r) ->
    void (forkIO (threadDelay (k * 60000) >> playTone (clampF 40 4000 (base * r)) 0.12 0.6 wave))

-- お手本：風船の基本形
presetWorld :: World -> World
presetWorld w0 =
  let n = gridN
      s1 = w0 { wSegs = [], wUndo = [], wSym = 2, wMv = False }
      (s2, _, _) = addStroke (0, 0) (n, n) s1                      -- 対角線 = 谷（鏡でもう 1 本）
      (s3, _, _) = addStroke (0, n `div` 2) (n, n `div` 2) s2 { wMv = True }  -- 横の中心線 = 山
  in s3 { wMv = wMv w0, wSym = wSym w0, wUndo = [] }

gridPt :: Env -> Point -> Maybe (Int, Int)
gridPt env (x, y) =
  let cs = cellSize env
      n = fromIntegral gridN
      i = floor ((x - cx0) / cs * n + 0.5) :: Int
      j = floor ((y - cy0) / cs * n + 0.5) :: Int
  in if i < 0 || j < 0 || i > gridN || j > gridN then Nothing else Just (i, j)

-- 頂点ごとの山・谷の数（内側の点だけ）
vertexCheck :: [Seg] -> [((Int, Int), (Int, Int))]
vertexCheck segs =
  let add m (k, isM) = M.insertWith (\(a, b) (c, d) -> (a + c, b + d)) k (if isM then (1, 0) else (0, 1)) m
      cnt = foldl add M.empty (concat [[((a, b), v), ((c, d), v)] | Seg a b c d v <- segs])
  in [e | e@((i, j), _) <- M.toList cnt, i > 0, j > 0, i < gridN, j < gridN]

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let cs = cellSize env
      n = fromIntegral gridN :: Float
      pp (i, j) = sc env (cx0 + fromIntegral i * cs / n, cy0 + fromIntegral j * cs / n)
      (rx, ry) = sc env (cx0 + cs / 2, cy0 + cs / 2)
      paper = Color (rgb255 246 240 226) (rect rx ry cs cs)
      grid = Color (rgba255 0 0 0 25) (Pictures (concat
        [ [thickLine 1 (pp (k, 0)) (pp (k, gridN)), thickLine 1 (pp (0, k)) (pp (gridN, k))] | k <- [0 .. gridN] ]))
      segsPic = map (segPic env cx0 cy0 cs 3) (wSegs w)
      checks = vertexCheck (wSegs w)
      ng = length [() | (_, (a, b)) <- checks, abs (a - b) /= 2]
      dots =
        [ Color (if ok then rgb255 40 180 110 else rgb255 240 60 60) (at (pp v) (disc (if ok then 3.5 else 5.5)))
        | (v, (a, b)) <- checks, let ok = abs (a - b) == 2 ]
      -- ドラッグ中の線
      preview = case (wStart w, gridPt env (mouseXY env)) of
        (Just s, Just g) ->
          [ Color (if wMv w then rgba255 220 70 50 150 else rgba255 50 110 230 150) (thickLine 4 (pp a) (pp b))
          | (a, b) <- zip (symCopies (wSym w) s) (symCopies (wSym w) g) ]
        _ -> []
      symName = case wSym w of { 1 -> "none"; 2 -> "mirror"; 3 -> "4-fold"; _ -> "8-way" }
      hud =
        [ txt env 20 40 18 white ("Drag: draw   M, V: " ++ (if wMv w then "mountain (red)" else "valley (blue)") ++ "   1-4: symmetry " ++ symName ++ "   P: sample   Z: undo   C: clear   S: save")
        , txt env 20 68 15 (gray 0.67) ("Dot color = Maekawa check (green: M - V = +-2, red: cannot fold). NG " ++ show ng ++ " / " ++ show (length checks) ++ " points   segments " ++ show (length (wSegs w)))
        , txt env (cx0 + cs + 40) 108 14 (gray 0.59) "Right: this cell tiled over the plane (tessellation)"
        ]
  in Pictures ([wTiles w, paper, grid] ++ segsPic ++ dots ++ preview ++ hud)

-- 線分 1 本（山は一点鎖線、谷は破線）
segPic :: Env -> Float -> Float -> Float -> Float -> Seg -> Picture
segPic env x0 y0 size w (Seg a b c d isM) =
  let n = fromIntegral gridN
      p = (x0 + fromIntegral a * size / n, y0 + fromIntegral b * size / n)
      q = (x0 + fromIntegral c * size / n, y0 + fromIntegral d * size / n)
  in Color (segCol isM) (Pictures [thickLine w (sc env e) (sc env f) | (e, f) <- dashPairs (segPat w isM) p q])

segCol :: Bool -> Color
segCol isM = if isM then rgb255 220 70 50 else rgb255 50 110 230

segPat :: Float -> Bool -> [Float]
segPat w isM = if isM then [w * 4, w * 1.5, w, w * 1.5] else [w * 2.5, w * 2]

-- タイル（敷きつめ）。線が多いときは 1 枚を大きくして、描く要素の数をおさえる
retile :: Float -> Float -> World -> World
retile wd ht w =
  let cs = min (wd * 0.4) (ht - 200)
      ox = cx0 + cs + 40
      oy = 120
      tw = max 10 (wd - cx0 - cs - 80)
      th = ht - 160
      toG (x, y) = (x - wd / 2, ht / 2 - y)
      n = fromIntegral gridN :: Float
      -- 1 タイルを描く要素の数（1 マス 120 px のとき。大きさを変えても同じ）
      cost = sum [ (if m then 2 else 1) * ceiling (1.25 * dist2 (fromIntegral a, fromIntegral b) (fromIntegral c, fromIntegral d) * 8 / n + 1)
                 | Seg a b c d m <- wSegs w ] :: Int
      nTiles ts = ceiling (tw / ts) * ceiling (th / ts) :: Int
      ts = case [t | t <- [120, 160, 216, 324], nTiles t * cost <= 4000] of
        t : _ -> t
        [] -> 648
      sw = 1.5 * ts / 120
      box = (ox, oy, ox + tw, oy + th)
      tiles =
        [ Color (segCol m) (Pictures
            [ thickLine sw (toG e') (toG f')
            | (e, f) <- dashPairs (segPat sw m) p q
            , Just (e', f') <- [clipSeg box e f] ])
        | row <- [0 .. ceiling (th / ts) - 1 :: Int]
        , col <- [0 .. ceiling (tw / ts) - 1 :: Int]
        , let x0 = ox + fromIntegral col * ts
        , let y0 = oy + fromIntegral row * ts
        , Seg a b c d m <- wSegs w
        , let p = (x0 + fromIntegral a * ts / n, y0 + fromIntegral b * ts / n)
        , let q = (x0 + fromIntegral c * ts / n, y0 + fromIntegral d * ts / n) ]
      (bx, by) = toG (ox + tw / 2, oy + th / 2)
  in w { wTiles = Pictures (Color (rgb255 246 240 226) (rect bx by tw th) : tiles) }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wStart = gridPt env (mouseXY env) }
  EventKey (MouseButton LeftButton) Up _ _ -> case (wStart w, gridPt env (mouseXY env)) of
    (Just s, Just g) | s /= g -> do
      let (w', added, copies) = addStroke s g w
      if added then strokeSound (wMv w) s g copies else pure ()
      pure (re w' { wStart = Nothing })
    _ -> pure w { wStart = Nothing }
  EventKey (Char c) Down _ _ -> case toLower c of
    'm' -> pure w { wMv = True }
    'v' -> pure w { wMv = False }
    'p' -> do
      strokeSound False (0, 0) (gridN, gridN) 2
      strokeSound True (0, gridN `div` 2) (gridN, gridN `div` 2) 2
      pure (re (presetWorld w))
    'c' -> pure (re w { wUndo = take maxUndo (wSegs w : wUndo w), wSegs = [] })
    'z' -> pure $ case wUndo w of
      s : rest -> re w { wSegs = s, wUndo = rest }
      [] -> w
    's' -> saveSvg "crease_pattern" env (drawWorld env w) >> pure w
    d | d >= '1' && d <= '4' -> pure w { wSym = fromEnum d - fromEnum '0' }
    _ -> pure w
  EventResize _ -> pure (re w)
  _ -> pure w
  where re = retile (envW env) (envH env)

-- ---------- やってみよう ----------
-- 1. 8 方向の対称で、赤い点（NG）が 1 つもない模様をつくろう。本物の紙に写して折れるか試す
-- 2. gridN を 6 や 12 に変えると、使える角度が変わる。三角形のマス目にするにはどう変える？
-- 3. 音の決め方（線の長さ → 高さ）を、角度 → 高さに変えてみよう
-- 4. 右のタイルを「1 マスごとに鏡に映す」敷きつめに変えると、つなぎ目の線がそろう

-- ==================================================
-- ここから下は「画面の道具」（座標の変換・破線・切りつめ）
-- ==================================================

sc :: Env -> Point -> Point
sc env (x, y) = (x - envW env / 2, envH env / 2 - y)

mouseXY :: Env -> Point
mouseXY env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sc env (x, y) in label a b size c s

-- 破線の 1 つ 1 つの線分
dashPairs :: [Float] -> Point -> Point -> [(Point, Point)]
dashPairs pat a@(ax, ay) (bx, by) =
  let l = dist2 a (bx, by)
      pt s = (ax + (bx - ax) * s / l, ay + (by - ay) * s / l)
      go s (on : off : rest)
        | s >= l = []
        | otherwise = (pt s, pt (min l (s + on))) : go (s + on + off) rest
      go _ _ = []
  in if l < 1e-6 then [] else go 0 (cycle pat)

-- 線分を長方形の中に切りつめる（リャン・バースキーの方法）
clipSeg :: (Float, Float, Float, Float) -> Point -> Point -> Maybe (Point, Point)
clipSeg (x0, y0, x1, y1) (ax, ay) (bx, by) =
  let dx = bx - ax
      dy = by - ay
      edge (t0, t1) (p, q)
        | p == 0 = if q < 0 then (1, 0) else (t0, t1)
        | p < 0 = (max t0 (q / p), t1)
        | otherwise = (t0, min t1 (q / p))
      (ta, tb) = foldl edge (0, 1) [(-dx, ax - x0), (dx, x1 - ax), (-dy, ay - y0), (dy, y1 - ay)]
  in if ta > tb then Nothing
     else Just ((ax + dx * ta, ay + dy * ta), (ax + dx * tb, ay + dy * tb))
