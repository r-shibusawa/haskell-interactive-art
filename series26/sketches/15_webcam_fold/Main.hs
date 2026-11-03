-- ============================================
-- 15 カメラで折る（おまけ）
-- カメラの映像を「紙」にして、マウスで引いた線で折る。折られた部分は鏡に映したように裏返り、紙の裏（白）が見える
--   00 と同じ「面の切り分け + 鏡映」を、映像を貼った紙でやる。何回でも重ねて折れる
-- ドラッグ = 折り線を引く（線の右側が手前に折れる）/ Z = 戻す / R = 広げる / B = カメラ ⇄ 模様 / S = 保存
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Word (Word8)

vw, vh :: Int
vw = 320                 -- ★ カメラ画像の大きさ
vh = 240

foldFrames :: Int
foldFrames = 24          -- ★ 折るアニメーションの長さ（フレーム）

backAlpha :: Float
backAlpha = 0.82         -- ★ 裏の白さ（1 = 真っ白、0 = 映像が透ける）

maxFaces, maxPixels :: Int
maxFaces = 64            -- ★ 面の数の上限（これ以上は折らない）
maxPixels = 78000        -- ★ 1 フレームに計算する画素の数

-- 紙の面：今の位置、もとの紙 → 今 の変換、裏返っているか、重なりの順、折っている途中で動く面か
data Face = Face { fPts :: [Point], fM :: !Mat, fBack :: !Bool, fZ :: !Int, fMoving :: !Bool }

data Anim = Anim { aL :: !Ln, aT :: !Int, aFaces :: [Face] }

data World = World
  { wFaces :: [Face]
  , wHist  :: [[Face]]
  , wDragA :: !(Maybe Point)
  , wAnim  :: !(Maybe Anim)
  , wUseP  :: !Bool             -- 模様を使うか
  , wCam   :: !(Maybe Camera)
  , wLive  :: !Bool             -- カメラの映像が来ているか
  , wFrame :: !B.ByteString     -- 紙に貼る画像（RGB、vw × vh）
  }

main :: IO ()
main = runSketch (sketch "26-15 Folding the Camera" (newWorld 1280 800))
  { skBack  = rgb255 30 28 36
  , skStart = \env -> do
      cam <- startCamera vw vh
      pure $! (newWorld (envW env) (envH env)) { wCam = Just cam }
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

newWorld :: Float -> Float -> World
newWorld wd ht = World (newFaces wd ht) [] Nothing Nothing False Nothing False patternBytes

-- 紙の大きさと左上
paperRect :: Float -> Float -> (Float, Float, Float, Float)
paperRect wd ht =
  let h = min (ht - 200) ((wd - 120) * 0.75)
      w = h * 4 / 3
  in (w, h, wd / 2 - w / 2, 120)

newFaces :: Float -> Float -> [Face]
newFaces wd ht =
  let (w, h, ox, oy) = paperRect wd ht
  in [Face [(ox, oy), (ox + w, oy), (ox + w, oy + h), (ox, oy + h)] ident False 0 False]

-- ★ 折る：直線 L の side 側の面を鏡に映して上に重ねる（00 と同じ）
--   moving の面は、アニメーションの最後に鏡映をかける
foldFaces :: [Face] -> Ln -> Float -> [Face]
foldFaces fs ln side =
  let maxZ = maximum (0 : map fZ fs)
      one f =
        let keep = clipHalf (fPts f) ln (-side)
            move = clipHalf (fPts f) ln side
        in [f { fPts = keep, fMoving = False } | length keep >= 3]
           ++ [f { fPts = move, fZ = 2 * maxZ + 1 - fZ f, fMoving = True } | length move >= 3]
  in concatMap one fs

-- 折っている途中の面（途中：折り線からの距離が cos φ 倍）
inProgress :: Anim -> [Face]
inProgress an =
  let phi = pi * min 1 (fromIntegral (aT an) / fromIntegral foldFrames) :: Float
      p = partialFold (aL an) (1 - cos phi)
      f' f | fMoving f = f { fPts = map (apply p) (fPts f), fM = compose p (fM f)
                           , fBack = if phi > pi / 2 then not (fBack f) else fBack f }
           | otherwise = f
  in map f' (aFaces an)

-- 折り終わった面
finished :: Anim -> [Face]
finished an =
  let r = partialFold (aL an) 2
      f' f | fMoving f = f { fPts = map (apply r) (fPts f), fM = compose r (fM f), fBack = not (fBack f), fMoving = False }
           | otherwise = f
  in map f' (aFaces an)

stepWorld :: Env -> Float -> World -> IO World
stepWorld _ _ w0 = do
  -- カメラの映像を受け取る。使えないときは模様の紙
  (live, frame) <- case wCam w0 of
    Just cam -> do
      l <- camLive cam
      if l && not (wUseP w0)
        then do f <- cameraFrame cam
                pure (True, f)
        else pure (l, patternBytes)
    Nothing -> pure (False, patternBytes)
  let w1 = w0 { wLive = live, wFrame = frame }
  pure $ case wAnim w1 of
    Just an
      | aT an + 1 > foldFrames -> w1 { wFaces = finished an, wAnim = Nothing }
      | otherwise -> w1 { wAnim = Just an { aT = aT an + 1 } }
    Nothing -> w1

camReady :: World -> Bool
camReady w = wLive w && not (wUseP w)

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let shown = maybe (wFaces w) inProgress (wAnim w)
      (mx, my) = mouseXY env
      guide = case (wDragA w, wAnim w) of
        (Just a@(ax, ay), Nothing) | dist2 a (mx, my) > 10 ->
          let Ln (px, py) (ux, uy) = lineOf a (mx, my)
              nx = -uy
              ny = ux
              cx = (ax + mx) / 2
              cy = (ay + my) / 2
              col = rgb255 255 220 90
          in [ Color col (dashed 2 [10, 8] (sc env (px - ux * 3000, py - uy * 3000)) (sc env (px + ux * 3000, py + uy * 3000)))
             -- 折れる側に矢印
             , Color col (thickLine 3 (sc env (cx + nx * 50, cy + ny * 50)) (sc env (cx - nx * 30, cy - ny * 30)))
             , Color col (at (sc env (cx - nx * 30, cy - ny * 30)) (disc 5))
             ]
        _ -> []
      outlines = [ Color (rgba255 60 40 40 153) (thickPath 1.5 (qs ++ take 1 qs))
                 | f <- sortOn fZ shown, let qs = map (sc env) (fPts f) ]
      hud =
        [ txt env 20 40 18 white ("Drag: draw a fold line (its right side folds toward you)   Z: undo   R: unfold   B: " ++ (if camReady w then "camera" else "pattern") ++ "   S: save   layers " ++ show (length (wFaces w)))
        , txt env 20 68 15 (gray 0.67)
            (if camReady w
               then "Paper = camera image. Folded parts keep moving: they mirror the image at their original place on the paper."
               else "No camera: a patterned paper. With a camera, the paper shows live video (B switches).")
        ]
  in Pictures ([renderPaper env w shown] ++ outlines ++ guide ++ hud)

---------------------------------------------------------------- 紙を画素に描く

-- 画素を調べるために前もって計算した面
data HP = HP !Float !Float !Float                      -- 半平面 a x + b y + c >= 0
data FC = FC [HP] !Float !Float !Float !Float !Float !Float !Bool

compile :: Face -> FC
compile f =
  let ps = fPts f
      es = edges ps
      area = sum [ax * by - bx * ay | ((ax, ay), (bx, by)) <- es]
      sgn = if area >= 0 then 1 else -1
      hps = [ HP (nx * sgn) (ny * sgn) (-(nx * ax + ny * ay) * sgn)
            | ((ax, ay), (bx, by)) <- es
            , let nx = -(by - ay)
            , let ny = bx - ax ]
      Mat a0 b0 c0 d0 e f0 = fM f
      det = a0 * d0 - b0 * c0
      ok = abs det > 1e-3 && abs area > 1e-3 && length ps >= 3
      a = d0 / det
      b = -b0 / det
      c = -c0 / det
      d = a0 / det
  in if ok
       then FC hps a b c d (-(a * e + b * f0)) (-(c * e + d * f0)) (fBack f)
       else FC [HP 0 0 (-1)] 1 0 0 1 0 0 False    -- 真横から見ている面は描かない

insideHP :: Float -> Float -> [HP] -> Bool
insideHP _ _ [] = True
insideHP x y (HP a b c : r) = a * x + b * y + c >= 0 && insideHP x y r

-- 面ごとに、もとの紙の位置の画像を変換 M で貼る（上の面から順に調べる）
renderPaper :: Env -> World -> [Face] -> Picture
renderPaper env w shown =
  let wd = envW env
      ht = envH env
      (pw, ph, ox, oy) = paperRect wd ht
      allPts = concatMap fPts shown
      x0 = max 0 (minimum (wd : map fst allPts))
      x1 = min wd (maximum (0 : map fst allPts))
      y0 = max 0 (minimum (ht : map snd allPts))
      y1 = min ht (maximum (0 : map snd allPts))
      bw = max 1 (x1 - x0)
      bh = max 1 (y1 - y0)
      k = max 2.5 (sqrt (bw * bh / fromIntegral maxPixels))
      iw = max 1 (ceiling (bw / k)) :: Int
      ih = max 1 (ceiling (bh / k)) :: Int
      faces = map compile (reverse (sortOn fZ shown))
      frame = wFrame w
      ok = B.length frame >= vw * vh * 3
      fvw = fromIntegral vw / pw
      fvh = fromIntegral vh / ph
      bg = (30, 28, 36) :: (Word8, Word8, Word8)
      sample u v =
        let sx = max 0 (min (vw - 1) (floor ((u - ox) * fvw)))
            sy = max 0 (min (vh - 1) (floor ((v - oy) * fvh)))
            i = (sy * vw + sx) * 3
        in if ok then (BU.unsafeIndex frame i, BU.unsafeIndex frame (i + 1), BU.unsafeIndex frame (i + 2)) else (120, 120, 120)
      -- 裏は紙の色を重ねる
      backMix :: Word8 -> Float -> Word8
      backMix c t = round (fromIntegral c * (1 - backAlpha) + t * backAlpha)
      pixel ix iy =
        let x = x0 + (fromIntegral ix + 0.5) * k
            y = y0 + (fromIntegral iy + 0.5) * k
            go [] = bg
            go (FC hs a b c d e f back : rest)
              | insideHP x y hs =
                  let u = a * x + b * y + e
                      v = c * x + d * y + f
                      (r, g, bl) = sample u v
                  in if back then (backMix r 244, backMix g 238, backMix bl 224) else (r, g, bl)
              | otherwise = go rest
        in go faces
      (gx, gy) = sc env (x0 + bw / 2, y0 + bh / 2)
  in if null shown then Blank
     else Translate gx gy (pixelImage iw ih k pixel)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    pure (if null (wAnim w) then w { wDragA = Just (mouseXY env) } else w)
  EventKey (MouseButton LeftButton) Up _ _ ->
    let m = mouseXY env
    in pure $ case (wDragA w, wAnim w) of
         (Just a, Nothing) | dist2 a m > 10 ->
           let ln = lineOf a m
               res = foldFaces (wFaces w) ln 1           -- 線の右側（画面の向きで）が折れる
           in if any fMoving res && length res <= maxFaces
                then w { wDragA = Nothing, wHist = wFaces w : wHist w, wAnim = Just (Anim ln 0 res) }
                else w { wDragA = Nothing }
         _ -> w { wDragA = Nothing }
  EventKey (Char c) Down _ _ -> case toLower c of
    'z' -> pure $ case (wHist w, wAnim w) of
      (f : r, Nothing) -> w { wFaces = f, wHist = r }
      _ -> w
    'r' -> pure (reset w)
    'b' -> pure w { wUseP = not (wUseP w) }
    's' -> saveSvg "folded_camera" env (drawWorld env w) >> pure w
    _ -> pure w
  EventResize _ -> pure (reset w)
  _ -> pure w
  where reset v = v { wFaces = newFaces (envW env) (envH env), wHist = [], wAnim = Nothing }

---------------------------------------------------------------- カメラがないときの紙

-- 青海波（せいがいは）の模様と PAPER の文字。もとの p5.js 版の 640 × 480 の絵を vw × vh に縮めて計算する
patternBytes :: B.ByteString
patternBytes = B.pack (concat [patternAt x y | y <- [0 .. vh - 1], x <- [0 .. vw - 1]])

patternAt :: Int -> Int -> [Word8]
patternAt ix iy =
  let x = fromIntegral (ix * 2 + 1) :: Float
      y = fromIntegral (iy * 2 + 1) :: Float
      r = 40 :: Float
      -- 後から描いた円（行・列の大きい方）が上に重なる
      hits = [ d
             | row <- [min 25 (floor ((y + r) / 20)), min 25 (floor ((y + r) / 20)) - 1 .. max (-1) (ceiling ((y - r) / 20))]
             , col <- [17, 16 .. -1]
             , let cx = fromIntegral (col :: Int) * r * 2 + fromIntegral (row `rem` 2 :: Int) * r
             , let cy = fromIntegral (row :: Int) * r / 2
             , abs (x - cx) <= r
             , let d = dist2 (x, y) (cx, cy)
             , d <= r ]
      base = case hits of
        [] -> [30, 60, 120]
        d : _ -> ringColor d
  in if inLetters ix iy then [255, 255, 255] else base

ringColor :: Float -> [Word8]
ringColor d =
  let k = max 1 (ceiling (d / 10)) :: Int
      edge = abs (d - fromIntegral k * 10) < 1.4
  in if edge then [20, 40, 90]
     else if odd k then [40, 90, 170] else [235, 225, 200]

-- 5 × 7 の文字で書いた PAPER
inLetters :: Int -> Int -> Bool
inLetters ix iy =
  let cell = 3
      lx = (ix - 10) `div` cell
      ly = (iy - 212) `div` cell
      (li, cx) = lx `divMod` 6
  in iy >= 212 && ix >= 10 && ly < 7 && li < 5 && cx < 5 &&
     case drop li letters of
       g : _ -> case drop ly g of
         row : _ -> case drop cx row of
           ch : _ -> ch == '#'
           [] -> False
         [] -> False
       [] -> False

letters :: [[String]]
letters = [lP, lA, lP, lE, lR]
  where
    lP = ["####.", "#...#", "#...#", "####.", "#....", "#....", "#...."]
    lA = [".###.", "#...#", "#...#", "#####", "#...#", "#...#", "#...#"]
    lE = ["#####", "#....", "#....", "####.", "#....", "#....", "#####"]
    lR = ["####.", "#...#", "#...#", "####.", "#.#..", "#..#.", "#...#"]

-- ---------- やってみよう ----------
-- 1. 自分の顔を半分に折ってみよう。折り線の上の点だけは動かない（鏡映の「軸」）
-- 2. 4 回折ってから R。00 の「折り目」と同じように、どこを折ったか思い出せる？
-- 3. backAlpha を 0.3 にすると裏から映像が透ける。トレーシングペーパーみたいに
-- 4. foldFrames を大きくしてゆっくり折る。途中の形は「折り線からの距離 × cos φ」で計算している

-- ==================================================
-- ここから下は「折り紙の幾何の道具」（00 と同じ）と画面の道具
-- ==================================================

data Ln = Ln !Point !Point

lineOf :: Point -> Point -> Ln
lineOf a@(ax, ay) (bx, by) =
  let dx = bx - ax
      dy = by - ay
      l0 = sqrt (dx * dx + dy * dy)
      l = if l0 == 0 then 1 else l0
  in Ln a (dx / l, dy / l)

sdist :: Point -> Ln -> Float
sdist (qx, qy) (Ln (px, py) (ux, uy)) = (qx - px) * (-uy) + (qy - py) * ux

edges :: [a] -> [(a, a)]
edges ps = zip ps (drop 1 ps ++ take 1 ps)

clipHalf :: [Point] -> Ln -> Float -> [Point]
clipHalf pts ln side = concatMap one (edges pts)
  where
    one (a@(ax, ay), b@(bx, by)) =
      let sa = sdist a ln * side
          sb = sdist b ln * side
          t = sa / (sa - sb)
      in [a | sa >= -1e-3]
         ++ [(ax + (bx - ax) * t, ay + (by - ay) * t) | (sa > 1e-3 && sb < -1e-3) || (sa < -1e-3 && sb > 1e-3)]

data Mat = Mat !Float !Float !Float !Float !Float !Float

ident :: Mat
ident = Mat 1 0 0 1 0 0

apply :: Mat -> Point -> Point
apply (Mat a b c d e f) (x, y) = (a * x + b * y + e, c * x + d * y + f)

compose :: Mat -> Mat -> Mat       -- B をしてから A をする
compose (Mat a b c d e f) (Mat a2 b2 c2 d2 e2 f2) =
  Mat (a * a2 + b * c2) (a * b2 + b * d2) (c * a2 + d * c2) (c * b2 + d * d2)
      (a * e2 + b * f2 + e) (c * e2 + d * f2 + f)

-- 折り線 L からの距離を (1 − k) 倍にする変換。k = 2 で鏡映、k = 1 − cos φ で折りの途中
partialFold :: Ln -> Float -> Mat
partialFold (Ln (px, py) (ux, uy)) k =
  let nx = -uy
      ny = ux
      a = 1 - k * nx * nx
      b = -k * nx * ny
      d = 1 - k * ny * ny
  in Mat a b b d (px - (a * px + b * py)) (py - (b * px + d * py))

sc :: Env -> Point -> Point
sc env (x, y) = (x - envW env / 2, envH env / 2 - y)

mouseXY :: Env -> Point
mouseXY env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sc env (x, y) in label a b size c s

dashed :: Float -> [Float] -> Point -> Point -> Picture
dashed w pat a@(ax, ay) (bx, by) =
  let l = dist2 a (bx, by)
      pt s = (ax + (bx - ax) * s / l, ay + (by - ay) * s / l)
      go s (on : off : rest)
        | s >= l = []
        | otherwise = thickLine w (pt s) (pt (min l (s + on))) : go (s + on + off) rest
      go _ _ = []
  in if l < 1e-6 then Blank else Pictures (go 0 (cycle pat))
