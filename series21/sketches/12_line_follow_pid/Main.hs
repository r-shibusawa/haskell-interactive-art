-- ============================================
-- 12 ライントレースと PID
-- 床の線を 5 個の明るさセンサーで読み、「線が中央からどれだけずれているか」= 誤差 e を回転速度に変える
--   P：e に比例　I：e の積み重ね（ずれっぱなしを直す）　D：e の変化（曲がり始めを先読みして振動を抑える）
--   ω = Kp e + Ki Σe + Kd Δe
-- ←→ = Kp / ↑↓ = Kd / I = 積分項 ON/OFF / 1〜3 = コース / F・S = 速く・遅く / R = スタートに戻す
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.ST (newArray, runSTUArray)
import Data.Array.Unboxed (UArray)
import Data.Char (toLower)
import Data.List (foldl')
import Data.Word (Word8)
import Text.Printf (printf)

nSensors :: Int
nSensors = 5              -- ★ センサーの数

sensorSpacing, sensorAhead, lineWidth :: Float
sensorSpacing = 9         -- ★ センサーの間隔（px）
sensorAhead = 22          -- ★ ロボットの中心からセンサーまでの距離
lineWidth = 14            -- ★ 線の太さ

data Reading = Reading !Float !Float !Float    -- x, y, 明るさ

data World = World
  { wTrack    :: !(UArray Int Word8)   -- コースを描いた画像（明るさ 0〜255）
  , wTrackPic :: !Picture              -- 画面に見せるコース
  , wX        :: !Float
  , wY        :: !Float
  , wTh       :: !Float
  , wKp       :: !Float   -- ★ ゲイン
  , wKi       :: !Float
  , wKd       :: !Float
  , wUseI     :: !Bool
  , wSpeed    :: !Float   -- ★ 前進速度
  , wESum     :: !Float
  , wEPrev    :: !Float
  , wLastDir  :: !Float
  , wCourse   :: !Int
  , wHist     :: ![Float] -- 誤差の履歴（新しい順、300 個まで）
  , wRead     :: ![Reading]
  , wLost     :: !Int
  }

main :: IO ()
main = runSketch (sketch "21-12 Line Follow PID" world0)
  { skBack  = hsb 215 35 12
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = makeTrack World
  { wTrack = blankTrack, wTrackPic = Blank, wX = 0, wY = 0, wTh = 0
  , wKp = 0.06, wKi = 0, wKd = 0.4, wUseI = False, wSpeed = 2.5
  , wESum = 0, wEPrev = 0, wLastDir = 1, wCourse = 1, wHist = [], wRead = [], wLost = 0 }

-- ★ コース：中心からの半径を角度の関数で決める閉じた曲線
trackPoint :: Int -> Float -> Point
trackPoint course t =
  let cx = width / 2
      cy = height / 2 + 40
      bigR = min width (height - 120) * 0.36
      r = case course of
            1 -> bigR
            2 -> bigR * (1 + 0.25 * sin (3 * t))
            _ -> bigR * (1 + 0.18 * sin (5 * t) + 0.12 * cos (2 * t))
  in (cx + r * cos t * 1.25, cy + r * sin t * 0.85)

-- コースを画像に描き、ロボットをスタートへ置く
makeTrack :: World -> World
makeTrack w =
  let course = wCourse w
      pts = [ trackPoint course (fromIntegral i * 0.02) | i <- [0 .. 314 :: Int] ]
      closed = pts ++ take 1 pts
      p0@(x0, y0) = trackPoint course 0
      (x1, y1) = trackPoint course 0.02
  in w { wTrack = drawTrack closed
       , wTrackPic = Pictures
           [ Color black (boxAt 0 0 width height)
           , Color white (strip lineWidth pts) ]
       , wX = fst p0, wY = snd p0, wTh = atan2 (y1 - y0) (x1 - x0)
       , wESum = 0, wEPrev = 0, wHist = [], wLost = 0 }

-- 太い折れ線を 1 画素 1 バイトの画像に塗る（縁は少しぼかす）
drawTrack :: [Point] -> UArray Int Word8
drawTrack pts = runSTUArray $ do
  arr <- newArray (0, iw * ih - 1) 0
  forM_ (zip pts (drop 1 pts)) $ \(a@(ax, ay), b@(bx, by)) -> do
    let r = lineWidth / 2 + 1
        xLo = max 0 (floor (min ax bx - r))
        xHi = min (iw - 1) (ceiling (max ax bx + r))
        yLo = max 0 (floor (min ay by - r))
        yHi = min (ih - 1) (ceiling (max ay by + r))
    forM_ [yLo .. yHi] $ \y -> forM_ [xLo .. xHi] $ \x -> do
      let d = segDist a b (fromIntegral x + 0.5, fromIntegral y + 0.5)
          v = round (255 * clampF 0 1 (lineWidth / 2 + 0.5 - d)) :: Word8
          i = x + y * iw
      old <- unsafeRead arr i
      when (v > old) (unsafeWrite arr i v)
  pure arr

-- 画像の明るさを読む（0〜1）
brightAt :: World -> Float -> Float -> Float
brightAt w x y =
  let ix = floor x :: Int
      iy = floor y :: Int
  in if ix < 0 || iy < 0 || ix >= iw || iy >= ih then 0
     else fromIntegral (unsafeAt (wTrack w) (ix + iy * iw)) / 255

step :: World -> World
step w =
  let th = wTh w
      half = fromIntegral (nSensors - 1) / 2
      -- センサーの位置と値
      sensor i =
        let off = (fromIntegral i - half) * sensorSpacing
            sx = wX w + cos th * sensorAhead - sin th * off
            sy = wY w + sin th * sensorAhead + cos th * off
        in (off, Reading sx sy (brightAt w sx sy))
      sensors = map sensor [0 .. nSensors - 1]
      weighted = sum [b * off | (off, Reading _ _ b) <- sensors]
      total = sum [b | (_, Reading _ _ b) <- sensors]
      -- ★ 誤差 e：線がセンサー列のどこにあるか（重みつき平均、右が＋）
      found = total > 0.2
      e | found = weighted / total
        | otherwise = wLastDir w * sensorSpacing * half   -- 見失ったら最後に見た側へ
      lastDir | found = if e >= 0 then 1 else -1
              | otherwise = wLastDir w
      lost = if found then 0 else wLost w + 1
      eSum = clampF (-400) 400 (wESum w + e)
      dE = e - wEPrev w
      om = clampF (-0.2) 0.2 (wKp w * e + (if wUseI w then wKi w * eSum else 0) + wKd w * dE)
      th' = th + om
      v = if lost > 30 then wSpeed w * 0.4 else wSpeed w
  in w { wTh = th', wX = wX w + v * cos th', wY = wY w + v * sin th'
       , wESum = eSum, wEPrev = e, wLastDir = lastDir, wLost = lost
       , wHist = take 300 (e : wHist w), wRead = map snd sensors }

drawAll :: World -> Picture
drawAll w =
  let robot = (wX w, wY w)
      th = wTh w
      gx = width - 330
      gy = 200
      gw = 300
      gh = 90
      graph = [ (gx + fromIntegral i / 300 * gw, gy - clampF (-1) 1 (e / 20) * gh / 2)
              | (i, e) <- zip [0 :: Int ..] (reverse (wHist w)) ]
      dot (Reading x y v) = Pictures
        [ Color (if v > 0.5 then hsb 45 90 100 else hsb 0 90 30) (at (x, y) (disc 4))
        , Color (hsb 0 0 100) (at (x, y) (ring 4 1)) ]
  in Pictures
  [ view
      ( [ wTrackPic w
        , Color (hsb 170 60 95) (bodyRect robot th (-18) (-14) 36 28)
        , Color (hsb 0 0 100) (bodyTri robot th (18, 0) (8, -7) (8, 7)) ]
        ++ map dot (wRead w)
        ++ [ -- 誤差グラフ
             Color (hsbA 0 0 100 8) (boxAt (gx - 10) (gy - gh / 2 - 10) (gw + 20) (gh + 20))
           , Color (hsb 0 0 40) (Line [(gx, gy), (gx + gw, gy)])
           , Color (hsb 45 90 100) (thickPath 2 graph) ] )
  , txt gx (gy - gh / 2 - 16) 14 (hsb 0 0 60) "Error e (if it shakes, raise Kd)"
  , txt 20 34 16 (hsb 0 0 100) ("Kp = " ++ nf 3 (wKp w) ++ " (L/R)   Kd = " ++ nf 2 (wKd w) ++ " (Up/Down)   I: integral "
                                ++ (if wUseI w then "ON Ki = " ++ nf 4 (wKi w) else "off") ++ "   Speed " ++ nf 1 (wSpeed w) ++ " (F, S)")
  , txt 20 58 14 (hsb 0 0 100) ("Course " ++ show (wCourse w) ++ " (1-3)   R: back to start" ++ (if wLost w > 0 then "   Lost the line!" else ""))
  , txt 20 78 14 (hsb 0 0 60) "Yellow sensor = on the line. e = line position (right is +). P alone shakes at curves, D calms it. How fast can it go?"
  ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey k) Down _ _ -> case k of
    KeyRight -> w { wKp = min 0.5 (wKp w * 1.3) }
    KeyLeft -> w { wKp = max 0.005 (wKp w / 1.3) }
    KeyUp -> w { wKd = min 3 (wKd w + 0.1) }
    KeyDown -> w { wKd = max 0 (wKd w - 0.1) }
    _ -> w
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> makeTrack w { wCourse = fromEnum c - fromEnum '0' }
    | otherwise -> case toLower c of
        'i' -> w { wUseI = not (wUseI w), wKi = 0.0005, wESum = 0 }
        'f' -> w { wSpeed = min 8 (wSpeed w + 0.5) }
        's' -> w { wSpeed = max 0.5 (wSpeed w - 0.5) }
        'r' -> makeTrack w
        _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. Kd = 0 にして Kp を上げていく。ふらつき始める Kp はいくつ？ そこから Kd を足して落ち着かせる
-- 2. 速さを 6 にしてコース 3 を走れる Kp・Kd を探す（ロボコンのチューニングそのもの）
-- 3. nSensors を 2 にすると？（左右だけ）8 にすると？ センサーの数と走りの質
-- 4. 積分 ON で、lineWidth を 4 にしてみる。小さなずれが残るときに効く


-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は p5 の text と同じ左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 左上の角が (x, y) の長方形（p5 の rect）
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y bw bh = rect (x + bw / 2) (y + bh / 2) bw bh

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 小数点以下 d 桁の文字列（p5 の nf）
nf :: Int -> Float -> String
nf d = printf ("%." ++ show d ++ "f")

-- (px, py) に向き th で置いた機体の中の点を、画面の座標に変える（p5 の translate + rotate）
place :: Point -> Float -> Point -> Point
place (px, py) th (x, y) = (px + x * cos th - y * sin th, py + x * sin th + y * cos th)

-- 機体の中の長方形（左上 (x, y)、幅 bw、高さ bh）
bodyRect :: Point -> Float -> Float -> Float -> Float -> Float -> Picture
bodyRect p th x y bw bh = Polygon (map (place p th) [(x, y), (x + bw, y), (x + bw, y + bh), (x, y + bh)])

-- 機体の中の三角形
bodyTri :: Point -> Float -> Point -> Point -> Point -> Picture
bodyTri p th a b c = Polygon (map (place p th) [a, b, c])

-- 閉じた折れ線を太い帯にする（となりの四角と辺を共有するので、曲がり角にすき間ができない）
strip :: Float -> [Point] -> Picture
strip w pts =
  let closed = pts ++ take 1 pts
      prev = drop (length pts - 1) pts ++ pts
      next = drop 1 closed ++ take 1 (drop 1 closed)
      -- 各点の法線（前後の点を結ぶ向きに直角）
      side (px, py) (nx, ny) (x, y) =
        let dx = nx - px
            dy = ny - py
            l = max 1e-6 (sqrt (dx * dx + dy * dy))
            ox = -dy / l * w / 2
            oy = dx / l * w / 2
        in ((x + ox, y + oy), (x - ox, y - oy))
      rails = zipWith3 side prev next closed
      quad (a, b) (c, d) = Polygon [a, c, d, b]
  in Pictures (zipWith quad rails (drop 1 rails))

-- 画像の大きさ（整数）
iw, ih :: Int
iw = round width
ih = round height

blankTrack :: UArray Int Word8
blankTrack = runSTUArray (newArray (0, 0) 0)

-- 点 p と線分 ab の距離
segDist :: Point -> Point -> Point -> Float
segDist (ax, ay) (bx, by) (px, py) =
  let dx = bx - ax
      dy = by - ay
      l2 = dx * dx + dy * dy
      t = if l2 < 1e-9 then 0 else clampF 0 1 (((px - ax) * dx + (py - ay) * dy) / l2)
      qx = ax + t * dx - px
      qy = ay + t * dy - py
  in sqrt (qx * qx + qy * qy)
