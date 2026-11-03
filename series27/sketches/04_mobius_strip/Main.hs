-- ============================================
-- 04 メビウスの帯（3D）
-- 紙テープを半回転ねじってつなぐと、表と裏の区別がない「片側しかない」曲面になる
--   アリが真ん中を歩くと、1 周で裏側（さかさま）に、2 周で元に戻る。真ん中で切ると？ 1/3 で切ると？
-- 0 = ふつうの輪 / 1 = メビウスの帯 / 2 = 真ん中で切る / 3 = 端から 1/3 で切る / O = 切った帯を広げる / ↑↓ = アリの速さ / スペース = 止める / ドラッグ = 視点
-- ============================================
module Main (main) where

import Art
import Text.Printf (printf)

width, height, rR, wd, camD, slit :: Float
width = 1280
height = 800
rR = min width height * 0.24     -- 帯の半径
wd = rR * 0.36                   -- 帯の半分の幅
camD = 693                       -- カメラまでの距離（p5.js の標準と同じ）
slit = 5                         -- ★ 切れ目の幅（px）

-- | 帯 1 本：u の範囲、幅の範囲、色、広げられるか、広げたときの大きさ
data Piece = Piece { pU0, pU1, pS0, pS1 :: !Float, pHue :: !(Float, Float, Float), pOpen :: !Bool, pBig :: !Float }

data World = World
  { wMode :: !Int
  , wAntU, wAntSpeed :: !Float   -- ★ アリの位置（角度）と速さ
  , wPaused :: !Bool
  , wOpenT, wOpenGoal :: !Float
  , wRotX, wRotY :: !Float
  , wTrail :: ![V3]              -- アリの跡（新しい順、260 点まで）
  , wPrev :: !(Float, Float)     -- 前のフレームのマウス
  }

main :: IO ()
main = runSketch (sketch "27-04 Mobius Strip" (World 1 0 0.012 False 0 0 (-0.6) 0.3 [] (0, 0)))
  { skBack  = rgb255 18 16 28
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- | ★ k 回半ねじった帯の点：u = 帯にそった角度、s = 幅方向（−Wd〜Wd）
band :: Float -> Float -> Float -> V3
band u s k =
  let c = cos (k * u / 2)
      sn = sin (k * u / 2)
  in V3 ((rR + s * c) * cos u) (s * sn) ((rR + s * c) * sin u)

-- | 切った帯（u が 0〜4π の 1 本の長い帯）を大きな輪に広げた形：2 回まるごとねじれている
opened :: Float -> Float -> Piece -> V3
opened u s p =
  let v = u / 2
      sc = s - (pS0 p + pS1 p) / 2
      phi = 2 * v
      rb = rR * pBig p
      rx = cos v
      rz = sin v
  in V3 (rb * rx + sc * cos phi * rx) (sc * sin phi) (rb * rz + sc * cos phi * rz)

pointAt :: World -> Piece -> Float -> Float -> V3
pointAt w p u s =
  let a = band u s (if wMode w == 0 then 0 else 1)
      t = wOpenT w
  in if pOpen p && t > 0
       then add3 a (scale3 t (sub3 (opened u s p) a))
       else a

-- | 今のモードで描く帯のリスト
pieces :: Int -> [Piece]
pieces m
  | m == 0 = [Piece 0 tau (-wd) wd (90, 200, 255) False 1]
  | m == 1 = [Piece 0 tau (-wd) wd (255, 170, 80) False 1]
  | m == 2 = [Piece 0 (2 * tau) slit wd (120, 220, 160) True 1.5]
  | otherwise =
      let t = wd / 3
      in [ Piece 0 tau (-t + slit) (t - slit) (255, 170, 80) False 1
         , Piece 0 (2 * tau) (t + slit) wd (150, 140, 255) True 1.7 ]

-- | 面の向き（法線）を数値微分で求める
normalAt :: World -> Piece -> Float -> Float -> V3
normalAt w p u s =
  let a = pointAt w p u s
      du = sub3 (pointAt w p (u + 0.01) s) a
      ds = sub3 (pointAt w p u (s + 0.5)) a
  in unit3 (cross3 du ds)

-- | アリがいる帯（いちばん長い帯）
antPiece :: World -> Piece
antPiece w = case reverse (pieces (wMode w)) of
  (p : _) -> p
  [] -> Piece 0 tau (-wd) wd (255, 255, 255) False 1

---------------------------------------------------------------- 動き

stepWorld :: Env -> World -> World
stepWorld env w0 =
  let m@(mx, my) = toP5 (envMouse env)
      (px, py) = wPrev w0
      w1 | envDown env = w0 { wRotY = wRotY w0 + (mx - px) * 0.01
                            , wRotX = clampF (-1.5) 1.5 (wRotX w0 - (my - py) * 0.01) }
         | otherwise = w0
      w2 = w1 { wOpenT = wOpenT w1 + (wOpenGoal w1 - wOpenT w1) * 0.05
              , wAntU = if wPaused w1 then wAntU w1 else wAntU w1 + wAntSpeed w1
              , wPrev = m }
      ap = antPiece w2
      uLen = pU1 ap - pU0 ap
      sMid = (pS0 ap + pS1 ap) / 2
      ua = pU0 ap + wrapF (2 * uLen) (wAntU w2)
      n = normalAt w2 ap ua sMid
      pos = pointAt w2 ap ua sMid
  in w2 { wTrail = take 260 (add3 pos (scale3 (9 * 0.4) n) : wTrail w2) }

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "0123" -> w { wMode = fromEnum c - fromEnum '0', wTrail = [], wAntU = 0, wOpenGoal = 0, wOpenT = 0 }
    | c `elem` "oO" -> w { wOpenGoal = if wOpenGoal w /= 0 then 0 else 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wAntSpeed = min 0.06 (wAntSpeed w + 0.004) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wAntSpeed = max 0 (wAntSpeed w - 0.004) }
  _ -> w

---------------------------------------------------------------- 描画

-- | 回転してカメラから見た座標へ（y は下向きのまま）
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

-- | 画面の座標へ（透視投影。y を上向きにする）
scr :: V3 -> Point
scr v = let (x, y) = project camD v in (x, -y)

zOf :: V3 -> Float
zOf (V3 _ _ z) = z

drawPiece :: World -> (V3 -> V3) -> Piece -> [(Float, Picture)]
drawPiece w view p =
  let nu = round ((pU1 p - pU0 p) / tau * 110) :: Int
      ns = 4 :: Int
      us = [pU0 p + (pU1 p - pU0 p) * fromIntegral i / fromIntegral nu | i <- [0 .. nu]]
      ss = [pS0 p + (pS1 p - pS0 p) * fromIntegral j / fromIntegral ns | j <- [0 .. ns]]
      rows = [[view (pointAt w p u s) | u <- us] | s <- ss]
      gs = [0.6 + 0.4 * cos (u / (pU1 p - pU0 p) * tau) | u <- us]     -- 帯にそって色を変える
      quad a b c d g = (zOf (scale3 0.25 (add3 a (add3 b (add3 c d)))), Color (shade (pHue p) g a b d) (Polygon (map scr [a, b, c, d])))
      strip r1 r2 = zipWith6' quad r1 (drop 1 r1) (drop 1 r2) r2 gs
  in concat (zipWith strip rows (drop 1 rows))

zipWith6' :: (a -> a -> a -> a -> b -> c) -> [a] -> [a] -> [a] -> [a] -> [b] -> [c]
zipWith6' f (a : as) (b : bs) (c : cs) (d : ds) (e : es) = f a b c d e : zipWith6' f as bs cs ds es
zipWith6' _ _ _ _ _ _ = []

-- | 環境光 + 平行光（両面を照らす）
shade :: (Float, Float, Float) -> Float -> V3 -> V3 -> V3 -> Color
shade (r, g, b) k a c d =
  let n = unit3 (cross3 (sub3 c a) (sub3 d a))
      l = abs (dot3 n lightDir)
      s = (0.43 + 0.75 * l) * k / 255
  in makeColor (min 1 (r * s)) (min 1 (g * s)) (min 1 (b * s)) 1

lightDir :: V3
lightDir = unit3 (V3 0.3 0.7 (-0.6))

drawWorld :: World -> Picture
drawWorld w =
  let view = viewFn w
      ps = pieces (wMode w)
      faces = concatMap (drawPiece w view) ps
      -- 縁（ふち）：メビウスの帯の縁は 1 本だけ
      edgeCol = rgb255 255 230 120
      edgeOf p s =
        let vs = [view (pointAt w p (pU0 p + (pU1 p - pU0 p) * fromIntegral i / 150) s) | i <- [0 .. 150 :: Int]]
        in segs edgeCol 3 10 vs
      edges = concat [edgeOf p s | p <- ps, s <- [pS0 p, pS1 p]]
      -- アリ：いちばん長い帯の真ん中を歩く
      ap = antPiece w
      uLen = pU1 ap - pU0 ap
      sMid = (pS0 ap + pS1 ap) / 2
      ua = pU0 ap + wrapF (2 * uLen) (wAntU w)
      n = normalAt w ap ua sMid
      pos = pointAt w ap ua sMid
      up = scale3 9 n
      tang = sub3 (pointAt w ap (ua + 0.02) sMid) pos
      tl = let m = norm3 tang in if m == 0 then 1 else m
      red' = rgb255 255 80 80
      trail = segs red' 3 12 (map view (wTrail w))
      body j =
        let v = view (add3 pos (add3 up (scale3 (fromIntegral j * 9 / tl) tang)))
            k = camD / max 1 (camD - zOf v)
        in (zOf v + 16, at (scr v) (Color (gray (40 / 255)) (disc ((if j == (1 :: Int) then 6 else 5) * k))))
      stick =
        let a = view pos
            b = view (add3 pos (scale3 3 up))
        in (zOf b + 18, Color red' (thickLine 4 (scr a) (scr b)))
      items = faces ++ edges ++ trail ++ map body [-1, 0, 1] ++ [stick]
  in Pictures (map snd (depthSort fst items) ++ [drawHud w uLen])

-- | 折れ線を、奥行きつきの線分に分ける
segs :: Color -> Float -> Float -> [V3] -> [(Float, Picture)]
segs c wid bias vs = zipWith seg vs (drop 1 vs)
  where seg a b = ((zOf a + zOf b) / 2 + bias, Color c (thickLine wid (scr a) (scr b)))

drawHud :: World -> Float -> Picture
drawHud w uLen =
  let m = wMode w
      laps = wAntU w / uLen
      names = case m of
        0 -> "Plain ring: has 2 sides and 2 edges"
        1 -> "Mobius strip: only 1 side and only 1 edge!"
        2 -> "Cut in the middle: not 2 pieces, but 1 ring, twice as long, 2 full twists"
        _ -> "Cut at 1/3: thin Mobius (orange) + long 2-twist ring (purple), linked"
      ant = case m of
        0 -> "Ant: stays on the same side, however many laps"
        1 -> printf "Ant: lap %.1f -> %s. Back to start after 2 laps" laps
               (if even (floor laps :: Int) then "starting side" else "the other side (upside down)")
        _ -> "Pieces after the cut: " ++ show (length (pieces m)) ++ "   (press O to open and check the shape)"
  in Pictures
       [ txt 20 40 16 (gray 1) "0: ring  1: Mobius  2: cut middle  3: cut 1/3  O: open  Up/Down: speed  Space: stop  Drag: view"
       , txt 20 68 14 (gray (170 / 255)) "Yellow = edge. Red line = ant's trail, red stick = the way the ant's head points"
       , txt 20 (height - 70) 20 (rgb255 255 230 120) names
       , txt 20 (height - 34) 16 (gray 1) ant
       ]

-- ---------- やってみよう ----------
-- 1. band の k を 2 や 3 にすると？ 何回ねじると「片側しかない」になる？（奇数？偶数？）
-- 2. 本物の紙テープでメビウスの帯をつくって、真ん中と 1/3 で切ってみよう。画面と同じになる？
-- 3. 2 で切った帯をもう一度真ん中で切ると何本になる？ 予想してから紙で確かめよう
-- 4. アリの跡の色を周回ごとに変えて、「1 周目は表、2 周目は裏」を見えるようにしよう

-- ==================================================
-- ここから下は「道具」（座標の変換）
-- ==================================================

-- | 画面の左上が原点の座標で文字を書く
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = label (x - width / 2) (height / 2 - y) s c str

toP5 :: Point -> (Float, Float)
toP5 (x, y) = (x + width / 2, height / 2 - y)
