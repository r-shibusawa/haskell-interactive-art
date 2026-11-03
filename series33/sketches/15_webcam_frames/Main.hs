-- ============================================
-- 15 カメラで黄金比を探す
-- カメラの映像に、黄金比・白銀比の長方形と螺旋のフレームを重ねる。顔や物にぴったり合わせて構図を探そう
-- カメラが使えないときは、作った風景（夕日と山）が代わりに出る
-- ドラッグ = フレームを動かす / 右下の角をドラッグ = 大きさ / 1〜4 = 比 / T = 縦横 / F = 螺旋の向き / G = 分割線 / スペース = 保存
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import Numeric (showFFloat)

data DragMode = NoDrag | Move | Size deriving Eq

data World = World
  { wCam      :: !Camera
  , wFrame    :: !B.ByteString
  , wLive     :: !Bool
  , wX, wY    :: !Float     -- フレームの左上
  , wLong     :: !Float     -- フレームの長い辺の長さ
  , wRatio    :: !Int
  , wPortrait :: !Bool
  , wFlip     :: !Int       -- 螺旋の向き（0〜3）
  , wLines    :: !Bool
  , wDrag     :: !DragMode
  , wGrab     :: !Point
  , wFlash    :: !Float
  }

-- 比の表（名前, 値, n, 色相）
ratioList :: [(String, Double, Int, Float)]
ratioList =
  [ ("Golden phi", (1 + sqrt 5) / 2, 1, 45)
  , ("Silver (Yamato) sqrt2", sqrt 2, 0, 200)
  , ("Silver 1+sqrt2", 1 + sqrt 2, 2, 210)
  , ("Bronze", (3 + sqrt 13) / 2, 3, 25)
  ]

ratioAt :: Int -> (String, Double, Int, Float)
ratioAt i = case drop i ratioList of
  (r : _) -> r
  [] -> ("Golden phi", (1 + sqrt 5) / 2, 1, 45)

-- カメラの解像度（★ 大きくすると細かいが重い）
camW, camH :: Int
camW = 320
camH = 240

main :: IO ()
-- 最初の世界はカメラを開く skStart が作るので、sketch に渡す値は使われない
main = runSketch (sketch "33-15 Webcam Frames" (error "skStart"))
  { skBack  = black
  , skStart = startWorld
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

startWorld :: Env -> IO World
startWorld env = do
  cam <- startCamera camW camH
  pure World
    { wCam = cam, wFrame = B.empty, wLive = False
    , wX = envW env * 0.25, wY = envH env * 0.25, wLong = envW env * 0.5
    , wRatio = 0, wPortrait = False, wFlip = 0, wLines = True
    , wDrag = NoDrag, wGrab = (0, 0), wFlash = 0 }

-- 画面上のフレームの大きさ
frameSize :: World -> (Float, Float)
frameSize w =
  let (_, v, _, _) = ratioAt (wRatio w)
      r = realToFrac v
  in if wPortrait w then (wLong w / r, wLong w) else (wLong w, wLong w / r)

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = do
  live <- camLive (wCam w)
  frame <- if live then cameraFrame (wCam w) else pure B.empty
  let (mx, my) = mouseP env
      w1 = case wDrag w of
        Move -> let (gx, gy) = wGrab w in w { wX = mx - gx, wY = my - gy }
        Size -> w { wLong = if wPortrait w then max (my - wY w) 60 else max (mx - wX w) 60 }
        NoDrag -> w
  pure w1 { wLive = live, wFrame = frame, wFlash = max 0 (wFlash w1 - 8) }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ background
  , drawFrame env w (wX w) (wY w) fw fh
  , Color (hsba 0 0 100 (wFlash w / 100)) (rect 0 0 (envW env) (envH env))   -- 撮影したときの白いフラッシュ
  , Color (hsba 0 0 0 0.55) (box env 0 0 (envW env) 80)
  , txt env 20 26 15 white (name ++ " = " ++ nfD 3 v ++ " (1-4)   T: " ++ (if wPortrait w then "portrait" else "landscape") ++ "   F spiral   G lines " ++ onOff (wLines w) ++ "   Space save")
  , txt env 20 48 14 (gray 0.75) ((if wLive w then "Camera" else "No camera: showing a made-up scene") ++ ". Drag the frame to move it, its white corner to resize.")
  , txt env 20 70 14 (gray 0.75) "Try matching eye height to a dividing line."
  ]
  where
    (fw, fh) = frameSize w
    (name, v, _, _) = ratioAt (wRatio w)
    background
      | wLive w && B.length (wFrame w) >= camW * camH * 3 =
          -- 映像（画面いっぱいに、縦横比を保って）
          camImage (wCam w) (wFrame w) (max (envW env / fromIntegral camW) (envH env / fromIntegral camH))
      | otherwise = fakeScene env

-- 長方形・正方形の分割・螺旋・黄金分割線
drawFrame :: Env -> World -> Float -> Float -> Float -> Float -> Picture
drawFrame env w x y fw fh = Pictures
  ( [ Color (hsba hue 70 100 0.9) (thickPath 3 (map tf [(0, 0), (iw, 0), (iw, ih), (0, ih), (0, 0)])) ]
    ++ [ Color (hsba hue 50 100 0.7) (thickPath 1.5 (map tf [(sx, sy), (sx + sq, sy), (sx + sq, sy + sq), (sx, sy + sq), (sx, sy)]))
       | b <- blocks, (sx, sy, sq) <- blockSquares b ]
    ++ [ Color (hsb 50 90 100) (thickPath 3 (concatMap (map tf . arcPoints) blocks)) ]
    ++ guides
    ++ [ Color white (box env (x + fw - 10) (y + fh - 10) 20 20) ])   -- 大きさを変える角
  where
    (_, v, rn, hue) = ratioAt (wRatio w)
    -- 中身はいつも横長として描き、最後に画面の向きへ変換する
    (iw, ih) = if wPortrait w then (fh, fw) else (fw, fh)
    flipX = if wFlip w == 1 || wFlip w == 3 then -1 else 1
    flipY = if wFlip w >= 2 then -1 else 1
    tf (px, py) =
      let a = (px - iw / 2) * flipX
          b = (py - ih / 2) * flipY
          (c, d) = if wPortrait w then (-b, a) else (a, b)    -- 縦長なら 90° 回す
      in pt env (x + fw / 2 + c) (y + fh / 2 + d)
    blocks = divide 0 0 0 (realToFrac iw) (realToFrac ih)
    -- 黄金分割線（1/φ² と 1/φ）または三分割
    golden = rn == 1
    t1 = if golden then realToFrac (1 - 1 / v) else 1 / 3 :: Float
    t2 = if golden then realToFrac (1 / v) else 2 / 3 :: Float
    guides
      | wLines w =
          [ Color (hsba 0 0 100 0.6) (Pictures
              [ dashed (tf a) (tf b)
              | (a, b) <- [ ((iw * t1, 0), (iw * t1, ih)), ((iw * t2, 0), (iw * t2, ih))
                          , ((0, ih * t1), (iw, ih * t1)), ((0, ih * t2), (iw, ih * t2)) ] ]) ]
      | otherwise = []

-- 正方形のブロック：d 段目、左上 (bx, by)、幅と高さ、枚数 k、一辺 sq
data Block = Block !Int !Double !Double !Double !Double !Int !Double

-- 正方形を取っていく：左 → 上 → 右 → 下 の順に、入るだけ（貴金属比なら毎回 n 枚、√2 は 1 枚のあと 2 枚ずつ）
-- 誤差がふくらまないように Double で計算する
divide :: Int -> Double -> Double -> Double -> Double -> [Block]
divide d ox oy ww hh
  | d >= 12 || min ww hh <= 2 = []
  | otherwise =
      let dir = d `mod` 4
          sq = min ww hh
          k = max 1 (floor (max ww hh / sq + 1e-6)) :: Int
          fk = fromIntegral k * sq
      in case dir of
           0 -> Block d ox oy fk sq k sq : divide (d + 1) (ox + fk) oy (ww - fk) hh
           1 -> Block d ox oy sq fk k sq : divide (d + 1) ox (oy + fk) ww (hh - fk)
           2 -> Block d (ox + ww - fk) oy fk sq k sq : divide (d + 1) ox oy (ww - fk) hh
           _ -> Block d ox (oy + hh - fk) sq fk k sq : divide (d + 1) ox oy ww (hh - fk)

blockSquares :: Block -> [(Float, Float, Float)]
blockSquares (Block d bx by _ _ k sq) =
  [ if even (d `mod` 4)
      then (realToFrac (bx + fj * sq), realToFrac by, realToFrac sq)
      else (realToFrac bx, realToFrac (by + fj * sq), realToFrac sq)
  | j <- [0 .. k - 1], let fj = fromIntegral j ]

-- このブロックを横切る 1/4 楕円（04 と同じ作り方）：角 O から u 方向に k 枚、v 方向に 1 枚
arcPoints :: Block -> [Point]
arcPoints (Block d bx by bw bh k sq) =
  let dir = d `mod` 4
      (ox, oy) = case dir of
        0 -> (bx, by)
        1 -> (bx + bw, by)
        2 -> (bx + bw, by + bh)
        _ -> (bx, by + bh)
      (ux, uy) = case dir of
        0 -> (1, 0)
        1 -> (0, 1)
        2 -> (-1, 0)
        _ -> (0, -1)
      (vx, vy) = (-uy, ux)
      a = fromIntegral k * sq
      b = sq
      qx = ox + a * ux + b * vx
      qy = oy + a * uy + b * vy
  in [ ( realToFrac (qx - a * cos th * ux - b * sin th * vx)
       , realToFrac (qy - a * cos th * uy - b * sin th * vy) )
     | i <- [0 .. 12 :: Int], let th = fromIntegral i * (pi / 2) / 12 :: Double ]

-- カメラがないときの風景（夕日と山）
fakeScene :: Env -> Picture
fakeScene env = Pictures (sky ++ [sun] ++ mountain)
  where
    w = envW env
    h = envH env
    sky = [ Color (hsb (20 + y / h * 20) 60 (100 - y / h * 50)) (box env 0 y w 9)
          | i <- [0 .. floor (h / 8) :: Int], let y = fromIntegral i * 8 ]
    sun = Color (hsb 40 80 100) (at (pt env (w * 0.618) (h * 0.42)) (disc (min w h * 0.09)))
    ridge x = h * 0.62 - noise2 (x * 0.004) 0 * h * 0.25
    -- へこんだ多角形は描けないので、細い台形を並べる
    mountain =
      [ Color (hsb 250 40 25) (Polygon [pt env x h, pt env x (ridge x), pt env (x + 20) (ridge (x + 20)), pt env (x + 20) h])
      | i <- [0 .. floor (w / 20) :: Int], let x = fromIntegral i * 20 ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
        (fw, fh) = frameSize w
    in pure $
       if abs (mx - (wX w + fw)) < 20 && abs (my - (wY w + fh)) < 20 then w { wDrag = Size }
       else if mx > wX w && mx < wX w + fw && my > wY w && my < wY w + fh
         then w { wDrag = Move, wGrab = (mx - wX w, my - wY w) }
         else w
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = NoDrag }
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    saveSvg "golden_frame" env (drawWorld env w)
    pure w { wFlash = 80 }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> pure w { wRatio = fromEnum c - fromEnum '1' }
    | c == 't' || c == 'T' ->
        let p = not (wPortrait w)
            -- 画面からはみ出さないように
            y' = if p then min (wY w) 100 else wY w
            l' = if p then min (wLong w) (envH env - y' - 20) else wLong w
        in pure w { wPortrait = p, wY = y', wLong = l' }
    | c == 'f' || c == 'F' -> pure w { wFlip = (wFlip w + 1) `mod` 4 }
    | c == 'g' || c == 'G' -> pure w { wLines = not (wLines w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 顔を枠に入れて、目・鼻・口がどの分割線の近くにあるか確かめよう
-- 2. 部屋の窓・本・ポスターを映して、どの比の枠にいちばん合うか探そう
-- 3. 分割線を「三分割（1/3）」と「黄金分割（0.382）」で切りかえて、写真の印象を比べよう
-- 4. 枠の中だけを切り取って表示するように改造してみよう（pixelImage を使う）

-- ---------- 道具 ----------
-- 螺旋は「正方形のブロックごとに 1/4 楕円」をつないだ近似（黄金比なら 1/4 円 = よく見る黄金螺旋）
onOff :: Bool -> String
onOff b = if b then "ON" else "off"

nfD :: Int -> Double -> String
nfD d v = showFFloat (Just d) v ""

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = Polygon [pt env x y, pt env (x + w) y, pt env (x + w) (y + h), pt env x (y + h)]

-- 点線
dashed :: Point -> Point -> Picture
dashed a@(ax, ay) b@(bx, by) =
  let len = dist2 a b
      n = floor (len / 12) :: Int
      f t = (ax + (bx - ax) * t, ay + (by - ay) * t)
  in Pictures [ Line [f (fromIntegral i * 12 / len), f ((fromIntegral i * 12 + 6) / len)] | i <- [0 .. n], len > 0 ]
