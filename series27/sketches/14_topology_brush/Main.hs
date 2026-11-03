-- ============================================
-- 14 トポロジカル・ブラシ
-- 正方形の辺を貼り合わせた世界（トーラス・クラインの壺・射影平面）でお絵かき。辺の外へ描くと、貼り合わせた相手の辺から線が出てくる
--   自由制作のベース：貼り方・色・音の決め方を変えよう。辺を越えるたびに音が鳴る
-- クリックで音をスタート / ドラッグ = 描く（正方形の外まで描いてよい） / 1 = トーラス / 2 = クラインの壺 / 3 = 射影平面 / 0 = ふつうの紙
-- A = 自動でまっすぐ進むペン / V = 貼り合わせを並べて見る / ↑↓ = 太さ / ←→ = A のペンの向き / S = 保存 / C = 消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

resN :: Int
resN = 256           -- ★ 絵を覚えるマスの数（1 辺）。大きいほどきれいで重い

width, height, qQ, x0, y0 :: Float
width = 1280
height = 800
qQ = fromIntegral (floor (min (width * 0.5) (height * 0.72)) :: Int)      -- 正方形の大きさ
x0 = fromIntegral (floor (width * 0.42 - qQ / 2) :: Int)
y0 = fromIntegral (floor (height * 0.55 - qQ / 2) :: Int)

type P = (Float, Float)
type Glue = Float -> Float -> P

data World = World
  { wSurf :: !Int
  , wCanvas :: !(U.Vector Word8)   -- 絵（R, G, B の順に resN × resN マス）
  , wImg :: Picture                -- 絵の画像（絵が変わったときだけ作り直す）
  , wBrush :: !Float               -- ★ ペンの太さ
  , wHue :: !Float
  , wPrevRaw :: !(Maybe P)
  , wAuto :: !Bool
  , wPos, wDir :: !P
  , wTiles :: !Bool
  , wLastTile :: !(Int, Int)
  , wAudio :: !Bool
  , wToneTimer :: !Float
  }

main :: IO ()
main = runSketch (sketch "27-14 Topological Brush" world0)
  { skBack  = hsb 230 25 9
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

canvasBack :: (Word8, Word8, Word8)
canvasBack = colorBytes (hsb 230 30 14)

blankCanvas :: U.Vector Word8
blankCanvas =
  let (r, g, b) = canvasBack
  in U.generate (resN * resN * 3) (\i -> case i `mod` 3 of
       0 -> r
       1 -> g
       _ -> b)

world0 :: World
world0 = World 1 blankCanvas (canvasImage blankCanvas) 8 0 Nothing False (0.3, 0.6) (cos 0.4, -sin 0.4) False (0, 0) False 0

-- | ★ 貼り合わせの写像：となりのマスの点 → 正方形の中の点（座標は 0〜1、y は下向き）
maps :: Int -> [Glue]
maps surf
  | surf == 0 = [\x y -> (x, y)]
  | surf == 1 || surf == 2 =
      [ if isFlip then \x y -> (x - i, (1 - y) + j) else \x y -> (x - i, y - j)
      | i <- [-1, 0, 1], j <- [-1, 0, 1]
      , let isFlip = surf == 2 && i /= 0 ]                   -- クラインの壺：左右の辺を越えると上下さかさま
  | otherwise =
      -- 射影平面：左右を越えると上下さかさま、上下を越えると左右さかさま
      [ \x y -> (x, y), \x y -> (x - 1, 1 - y), \x y -> (x + 1, 1 - y), \x y -> (1 - x, y + 1), \x y -> (1 - x, y - 1)
      , \x y -> (2 - x, 2 - y), \x y -> (-x, -y), \x y -> (2 - x, -y), \x y -> (-x, 2 - y) ]

-- | 正方形の外の点を、正方形の中の同じ点に戻す
canonical :: Int -> Float -> Float -> (P, Glue)
canonical surf x y =
  let inside (a, b) = a >= 0 && a <= 1 && b >= 0 && b <= 1
  in case [(g x y, g) | g <- maps surf, inside (g x y)] of
       (r : _) -> r
       [] -> ((clampF 0 1 x, clampF 0 1 y), \u v -> (u, v))

-- | 線分を、貼り合わせのすべての写し先に描く（正方形からはみ出た部分は自動で切れる）
stroke2 :: P -> P -> World -> World
stroke2 (ax, ay) (bx, by) w =
  let segs = [(g ax ay, g bx by) | g <- maps (wSurf w)]
      cv = paint (colorBytes (hsb (wHue w) 70 100)) (wBrush w) segs (wCanvas w)
  in w { wCanvas = cv, wImg = canvasImage cv }

---------------------------------------------------------------- 動き

stepWorld :: Env -> Float -> World -> IO World
stepWorld env dt w0 = do
  let w1 = w0 { wHue = wrapF 360 (wHue w0 + 0.4) }
  (w2, pingF) <- pure (if wAuto w1 then stepAuto w1 else (w1, Nothing))
  when (wAudio w2) (mapM_ ping pingF)
  -- 描いている間の音（短い音をくり返す）
  let (_, my) = toP5 (envMouse env)
      active = wAuto w2 || envDown env
      yy = if wAuto w2 then snd (wPos w2) else clampF 0 1 ((my - y0) / qQ)
      timer = wToneTimer w2 - dt
  if wAudio w2 && active && timer <= 0
    then do
      playTone (clampF 50 2000 (200 + 400 * (1 - yy))) 0.06 0.18 Triangle
      pure w2 { wToneTimer = 0.15 }
    else pure w2 { wToneTimer = max 0 timer }

-- | A：まっすぐ進むペン。辺を越えたら相手の辺から、向きも貼り合わせどおりに変えて入る
stepAuto :: World -> (World, Maybe Float)
stepAuto w =
  let v = 0.006
      (px, py) = wPos w
      (dx, dy) = wDir w
      nx = px + dx * v
      ny = py + dy * v
      w1 = stroke2 (px, py) (nx, ny) w
      outX = nx < 0 || nx > 1
      outY = ny < 0 || ny > 1
  in if outX || outY
       then
         let ((cx, cy), g) = canonical (wSurf w) nx ny
             (ex, ey) = g (nx + dx * 1e-3) (ny + dy * 1e-3)
             ddx = (ex - cx) / 1e-3
             ddy = (ey - cy) / 1e-3
             m0 = sqrt (ddx * ddx + ddy * ddy)
             m = if m0 == 0 then 1 else m0
             d1x = ddx / m
             d1y = ddy / m
             dir' | wSurf w == 0 = (if outX then -d1x else d1x, if outY then -d1y else d1y)
                  | otherwise = (d1x, d1y)
         in (w1 { wDir = dir', wPos = (cx, cy) }, Just (if outX then 660 else 880))
       else (w1 { wPos = (nx, ny) }, Nothing)

-- | 短い「チン」（辺を越えたとき）
ping :: Float -> IO ()
ping f = playTone f 0.25 0.6 Sine

tileOf :: P -> (Int, Int)
tileOf (x, y) = (floor x, floor y)

rawOf :: Point -> P
rawOf pos = let (mx, my) = toP5 pos in ((mx - x0) / qQ, (my - y0) / qQ)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let raw = rawOf pos in pure w { wAudio = True, wPrevRaw = Just raw, wLastTile = tileOf raw }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wPrevRaw = Nothing }
  EventMotion pos -> case wPrevRaw w of
    Just prev | not (wTiles w) -> do
      let (rx, ry) = rawOf pos
          raw = (clampF (-0.95) 1.95 rx, clampF (-0.95) 1.95 ry)
          t = tileOf raw
          w1 = stroke2 prev raw w
      if t /= wLastTile w
        then do
          when (wAudio w) (ping (if t == (0, 0) then 520 else 780))
          pure w1 { wPrevRaw = Just raw, wLastTile = t }
        else pure w1 { wPrevRaw = Just raw }
    _ -> pure w
  EventKey (SpecialKey k) Down _ _ -> pure (case k of
    KeyUp -> w' { wBrush = min 40 (wBrush w + 2) }
    KeyDown -> w' { wBrush = max 2 (wBrush w - 2) }
    KeyLeft -> turn (-0.1)
    KeyRight -> turn 0.1
    _ -> w')
  EventKey (Char c) Down _ _
    | c `elem` "0123" -> pure w' { wSurf = fromEnum c - fromEnum '0' }
    | c `elem` "aA" -> pure w' { wAuto = not (wAuto w) }
    | c `elem` "vV" -> pure w' { wTiles = not (wTiles w) }
    | c `elem` "sS" -> saveSvg "topology_brush" env (drawWorld env w) >> pure w'
    | c `elem` "cC" -> pure w' { wCanvas = blankCanvas, wImg = canvasImage blankCanvas }
    | otherwise -> pure w'
  _ -> pure w
  where
    w' = w { wAudio = True }
    turn d =
      let (dx, dy) = wDir w
          a = atan2 dy dx + d
      in w' { wDir = (cos a, sin a) }

---------------------------------------------------------------- 描画

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let main' = if wTiles w then drawTiles w else drawSquare env w
      name = case wSurf w of
        0 -> "Plain paper (no gluing)"
        1 -> "Torus"
        2 -> "Klein bottle"
        _ -> "Projective plane"
  in Pictures
       [ main'
       , txt x0 (y0 - 24) 26 (gray 1) name
       , txt 20 30 16 (gray 1) ((if wAudio w then "Sound on" else "Click for sound")
                                ++ "   Drag: draw (may go outside)   1 2 3 0: gluing   A: auto pen " ++ (if wAuto w then "ON" else "off"))
       , txt 20 54 16 (gray 1) "V: tile view   Up/Down: width   <- ->: pen direction   S: save   C: clear"
       , txt 20 78 14 (gray 0.6) "Edges with the same color of arrow are glued. White circle = pen position inside the square."
       ]

drawSquare :: Env -> World -> Picture
drawSquare env w =
  let (mx, my) = toP5 (envMouse env)
      ((cx, cy), _) = canonical (wSurf w) ((mx - x0) / qQ) ((my - y0) / qQ)
      k = qQ / fromIntegral resN
      (px, py) = wPos w
  in Pictures $
       [ at (pt (x0 + qQ / 2, y0 + qQ / 2)) (Scale k k (wImg w))
       , drawEdges x0 y0 qQ (wSurf w)
       , at (pt (x0 + cx * qQ, y0 + cy * qQ)) (Color (gray 1) (ring (wBrush w / 2 + 5) 2)) ]
       ++ [ at (pt (x0 + px * qQ, y0 + py * qQ)) (Color (hsb 50 90 100) (disc 7)) | wAuto w ]

drawEdges :: Float -> Float -> Float -> Int -> Picture
drawEdges x y s surf
  | surf == 0 = Color (hsb 0 0 50) (Pictures [thickPath 2 (map pt [(x, y), (x + s, y), (x + s, y + s), (x, y + s), (x, y)])])
  | otherwise = Pictures (map edge es)
  where
    -- [辺, 向き(+1 = 右・下へ), 色]
    es = case surf of
      1 -> [('T', 1, 20), ('B', 1, 20), ('L', 1, 200), ('R', 1, 200)]
      2 -> [('T', 1, 20), ('B', 1, 20), ('L', 1, 200), ('R', -1, 200)]
      _ -> [('T', 1, 20), ('B', -1, 20), ('L', 1, 200), ('R', -1, 200)]
    edge :: (Char, Int, Float) -> Picture
    edge (e, d, h) =
      let (ax, ay, bx, by) = case e of
            'T' -> (x, y, x + s, y)
            'B' -> (x, y + s, x + s, y + s)
            'L' -> (x, y, x, y + s)
            _ -> (x + s, y, x + s, y + s)
          (x1, y1, x2, y2) = if d < 0 then (bx, by, ax, ay) else (ax, ay, bx, by)
          mx = (x1 + x2) / 2
          my = (y1 + y2) / 2
          an = atan2 (y2 - y1) (x2 - x1)
          ln p q = thickLine 5 (pt p) (pt q)
      in Color (hsb h 80 100) (Pictures
           [ ln (x1, y1) (x2, y2)
           , ln (mx, my) (mx - 16 * cos (an - 0.6), my - 16 * sin (an - 0.6))
           , ln (mx, my) (mx - 16 * cos (an + 0.6), my - 16 * sin (an + 0.6)) ])

-- | V：となりのマスに、貼り合わせどおりに裏返した絵を並べる（つながって見える）
drawTiles :: World -> Picture
drawTiles w =
  let s = fromIntegral (floor (min (width - 40) (height - 130) / 3) :: Int) :: Float
      ox = width / 2 - 1.5 * s
      oy = 100
      surf = wSurf w
      k = s / fromIntegral resN
      tile i j =
        let fy = (surf == 2 || surf == 3) && i /= (0 :: Int)
            fx = surf == 3 && j /= (0 :: Int)
            c = (ox + (fromIntegral i + 1.5) * s, oy + (fromIntegral j + 1.5) * s)
            (gx, gy) = pt c
            sx = if fx then -k else k
            sy = if fy then -k else k
        in Pictures $
             [at (pt c) (Scale sx sy (wImg w))]
             ++ [ Color (hsba 230 25 9 0.4) (rect gx gy s s) | i /= 0 || j /= 0 ]   -- まわりの 8 枚は少し暗く
  in Pictures $
       [tile i j | i <- [-1, 0, 1], j <- [-1, 0, 1]]
       ++ [Color (gray 1) (thickPath 2 (map pt [(ox + s, oy + s), (ox + 2 * s, oy + s), (ox + 2 * s, oy + 2 * s), (ox + s, oy + 2 * s), (ox + s, oy + s)]))]
       ++ [ txt 100 (oy + 3 * s + 20) 14 (gray 0.7) "A projective plane does not tile neatly (corners do not match). The outer 8 are a guide."
          | surf == 3 ]

-- ---------- やってみよう ----------
-- 1. トーラスで自動ペン（A）を走らせ、←→ で向きを変えよう。線が閉じる向きと、どこまでも埋めつくす向きがある
-- 2. クラインの壺で正方形の右の辺の外に自分の名前を書くと、左の辺からどう出てくる？（V で並べて見る）
-- 3. ping の音の高さを、越えた辺（上下・左右）や曲面ごとに変えて「辺を越える音楽」にしよう
-- 4. maps に自分の貼り方を追加しよう（たとえば上下の辺だけさかさまに貼る）

-- ==================================================
-- ここから下は「道具」（座標の変換、絵のマスを塗る）
-- ==================================================

pt :: P -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

toP5 :: Point -> P
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = let (gx, gy) = pt (x, y) in label gx gy s c str

colorBytes :: Color -> (Word8, Word8, Word8)
colorBytes c =
  let (r, g, b, _) = rgbaOfColor c
      q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))
  in (q r, q g, q b)

canvasImage :: U.Vector Word8 -> Picture
canvasImage cv = pixelImage resN resN 1 (\x y ->
  let i = (y * resN + x) * 3
  in (U.unsafeIndex cv i, U.unsafeIndex cv (i + 1), U.unsafeIndex cv (i + 2)))

-- | 線分（座標は 0〜1）から距離が太さの半分以内のマスを塗る（端は丸くなる）
paint :: (Word8, Word8, Word8) -> Float -> [(P, P)] -> U.Vector Word8 -> U.Vector Word8
paint (cr, cg, cb) brush segs = U.modify $ \mv ->
  forM_ segs $ \((a0, b0), (a1, b1)) -> do
    let n = fromIntegral resN :: Float
        r = max 0.6 (brush / 2 / qQ * n)
        ax = a0 * n
        ay = b0 * n
        bx = a1 * n
        by = b1 * n
        lo v w = max 0 (floor (min v w - r))
        hi v w = min (resN - 1) (ceiling (max v w + r))
        dx = bx - ax
        dy = by - ay
        len2 = dx * dx + dy * dy
    forM_ [lo ay by .. hi ay by] $ \j ->
      forM_ [lo ax bx .. hi ax bx] $ \i -> do
        let px = fromIntegral i + 0.5
            py = fromIntegral j + 0.5
            t = if len2 == 0 then 0 else clampF 0 1 (((px - ax) * dx + (py - ay) * dy) / len2)
            qx = px - (ax + t * dx)
            qy = py - (ay + t * dy)
        when (qx * qx + qy * qy <= r * r) $ do
          let o = (j * resN + i) * 3
          MU.unsafeWrite mv o cr
          MU.unsafeWrite mv (o + 1) cg
          MU.unsafeWrite mv (o + 2) cb
