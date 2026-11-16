-- ============================================
-- 45-15 カメラの動きで歯車を回す 📷（おまけ）
-- カメラの前で手を振ると、その「動きの大きさ」（フレーム差分）が入力軸のトルクになり、歯車列でおもりを巻き上げる
-- 減速比が小さいと力が足りず持ち上がらない。減速比を大きくすると、ゆっくりだが軽い力で上がる（力 ⇄ 速さの交換）
-- 操作：1〜3 = 歯車の段数（減速比）/ ↑↓ = おもりの重さ / K = 逆転止め（ラチェット）/ R = おもりを下ろす
--       カメラが使えないときはマウスを動かす
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

vw, vh :: Int
vw = 160
vh = 120

motionGain, maxTorque, friction :: Float
motionGain = 9          -- ★ 動き → トルクの感度
maxTorque = 1.0         -- ★ 入力トルクの上限
friction = 0.08         -- ★ まさつ

gm, pa, maxLift, drumR :: Float
gm = 6                  -- モジュール
pa = 20 * pi / 180      -- 圧力角
maxLift = 240
drumR = 28

data World = World
  { wCam      :: !(Maybe Camera)
  , wFrame    :: !B.ByteString
  , wCamOK    :: !Bool
  , wPrevGray :: !(U.Vector Float)   -- 前のフレームの明るさ（4 画素おき）
  , wCells    :: ![(Int, Int)]       -- 動いた場所（表示用）
  , wPrevMouse :: !Point
  , wMotion   :: !Float              -- 動きの大きさ（0〜1）
  , wTorqueIn :: !Float
  , wStages   :: !Int                -- 歯車の段数（1〜3）
  , wLoad     :: !Float              -- ★ おもりが出力軸にかけるトルク（重さ）
  , wRatchet  :: !Bool               -- 逆転止め
  , wOmega    :: !Float              -- 入力軸の角速度
  , wRot0     :: !Float              -- 入力軸の回転角
  , wLifted   :: !Float
  , wGoals    :: !Int
  }

world0 :: World
world0 = World Nothing B.empty False U.empty [] (0, 0) 0 0 2 3.0 True 0 0 0 0

main :: IO ()
main = runSketch (sketch "45-15 Webcam gears" world0)
  { skBack  = hsb 212 45 13
  , skStart = \_ -> do
      cam <- startCamera vw vh
      pure world0 { wCam = Just cam }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- 歯車：中心、歯数、同軸か、回転角、速さの比
data Gear = Gear { gx, gy :: !Float, gz :: !Int, gCoax :: !Bool, gRot :: !Float, gSpeed :: !Float }

-- ★ 歯車列を作る：[歯数, 同軸か] を順に並べる。段数が増えるほど減速比が大きい
--   回転角と速さの比は 04 と同じく親から順に決める（入力軸の角 rot0 から）
train :: Env -> Int -> Float -> [Gear]
train env stages rot0 = reverse (foldl' add [] spec)
  where
    spec = [(14, False), (36, False)]
           ++ (if stages >= 2 then [(12, True), (48, False)] else [])
           ++ (if stages >= 3 then [(12, True), (40, False)] else [])
    add [] _ = [Gear (envW env * 0.4) (envH env * 0.42) 14 False rot0 1]
    add gs@(p : _) (z, coax)
      | coax = Gear (gx p) (gy p) z True (gRot p) (gSpeed p) : gs
      | otherwise =
          let a = gm * fromIntegral (gz p + z) / 2
              nMesh = length (filter (not . gCoax) gs)          -- いままでにかみ合わせた回数で向きを交互に
              ang = if odd nMesh then 0.38 else -0.38
              zf = fromIntegral z
          in Gear (gx p + a * cos ang) (gy p + a * sin ang) z False
                  (ang + pi + pi / zf - (gRot p - ang) * fromIntegral (gz p) / zf)
                  (-(gSpeed p) * fromIntegral (gz p) / zf) : gs

-- 減速比 i = |1 / 出力の速さ|
ratioOf :: [Gear] -> Float
ratioOf gs = case reverse gs of
  (g : _) -> abs (1 / gSpeed g)
  [] -> 1

-- ★ フレーム差分：前のフレームと明るさが大きく変わった画素の割合
measureMotion :: B.ByteString -> U.Vector Float -> (Float, U.Vector Float, [(Int, Int)])
measureMotion f prev = (fromIntegral (length cells) / fromIntegral n, gray, cells)
  where
    cw = vw `div` 4
    ch = vh `div` 4
    n = cw * ch
    gray = U.generate n (\idx ->
      let (j, i) = idx `divMod` cw
          k = (i * 4 + j * 4 * vw) * 3
          g o = fromIntegral (BU.unsafeIndex f (k + o)) :: Float
      in (g 0 + g 1 + g 2) / 3)
    cells = if U.length prev /= n then []
            else [ (i, j) | idx <- [0 .. n - 1], abs (gray U.! idx - prev U.! idx) > 22, let (j, i) = idx `divMod` cw ]

step :: Env -> Float -> World -> IO World
step env _ w = do
  live <- maybe (pure False) camLive (wCam w)
  frame <- case wCam w of
    Just c | live -> cameraFrame c
    _ -> pure B.empty
  let camOK = live && B.length frame >= vw * vh * 3
      mp = mouseP env
      (m, gray, cells)
        | camOK = measureMotion frame (wPrevGray w)
        | otherwise = (clampF 0 1 (dist2 mp (wPrevMouse w) / 120), U.empty, [])   -- カメラなし：マウスの速さ
      motion = lerpF (wMotion w) (clampF 0 1 m) 0.3
      torqueIn = min (motion * motionGain) maxTorque
      -- ---- 力学：J ω′ = 入力トルク − まさつ − おもりのトルク / 減速比
      ratio = ratioOf (train env (wStages w) (wRot0 w))
      back = if wLifted w > 0 || torqueIn > wLoad w / ratio then wLoad w / ratio else 0   -- 床に着いているときは引かれない
      om1 = wOmega w + (torqueIn - friction * wOmega w - back) / 60 * 6
      om2 = clampF (-20) 30 (if wRatchet w && om1 < 0 then 0 else om1)
      rot0 = wRot0 w + om2 / 60
      lifted1 = wLifted w + om2 / 60 / ratio * drumR
      (lifted2, om3) = if lifted1 <= 0 then (0, max 0 om2) else (lifted1, om2)
      w' = w { wFrame = frame, wCamOK = camOK, wPrevGray = gray, wCells = cells, wPrevMouse = mp
             , wMotion = motion, wTorqueIn = torqueIn, wRot0 = rot0 }
  pure $ if lifted2 >= maxLift
           then w' { wGoals = wGoals w + 1, wLifted = 0, wOmega = 0 }
           else w' { wLifted = lifted2, wOmega = om3 }

draw :: Env -> World -> Picture
draw env w = Pictures
  [ if wCamOK w then camPic else Blank
  , view env $
      -- ---- 歯車列
      concat [ [ gearPic (gearColor i g) (gz g) (makeGearOutline (gz g) gm pa 1 1.25) (gx g) (gy g) (gRot g)
               , dot (hsb 0 0 100) (gx g) (gy g) 9
               , let r = gm * fromIntegral (gz g) / 2 - gm * 2.4
                 in dot (hsb 0 0 100) (gx g + r * cos (gRot g)) (gy g + r * sin (gRot g)) 6 ]
             | (i, g) <- zip [0 ..] gs ]
      ++ [ txt (gx g - 16) (gy g - gm * fromIntegral (gz g) / 2 - gm - 8) 14 (hsb 0 0 100) ("z = " ++ show (gz g)) | g <- gs, not (gCoax g) ]
      -- 入力トルクの矢印（赤い歯車のまわり）
      ++ [ path 5 (hsb 0 80 100) [ (x0 + 75 * cos a, y0 + 75 * sin a) | k <- [0 .. 40 :: Int]
                                 , let a = -pi / 2 + wTorqueIn w * 4.5 * fromIntegral k / 40 ]
         | wTorqueIn w > 0.02 ]
      -- ドラムとおもり
      ++ [ dot (hsb 0 0 75) (gx gl) (gy gl) (drumR * 2)
         , seg 2 (hsb 0 0 85) (wx, gy gl) (wx, wy)
         , Color (hsb 35 80 95) (box (wx - 26) wy 52 36)
         , txt (wx - 12) (wy + 23) 14 (hsb 0 0 10) (fmt 1 (wLoad w))
         , seg 2 (hsb 0 0 45) (wx - 70, floorY) (wx + 70, floorY)
         , seg 2 (hsbA 140 60 100 70) (wx - 70, floorY - 36 - maxLift) (wx + 70, floorY - 36 - maxLift)
         , txt (wx + 34) (floorY - 40 - maxLift) 14 (hsb 140 60 100) "goal" ]
      -- ---- カメラ映像と動いた場所
      ++ (if wCamOK w
            then [ Color (hsbA 50 90 100 55) (box (vx + fromIntegral (i * 4) * vs) (vy + fromIntegral (j * 4) * vs) (4 * vs) (4 * vs)) | (i, j) <- wCells w ]
            else [ Color (hsbA 0 0 100 8) (box vx vy camW camH)
                 , txt (vx + 30) (vy + 124) 14 (hsb 0 0 70) "No camera: move the mouse to turn" ])
      ++ [ Color (hsbA 0 0 100 40) (Line [(vx, vy), (vx + camW, vy), (vx + camW, vy + camH), (vx, vy + camH), (vx, vy)]) ]
      -- メーター
      ++ concat [ [ txt vx (my + fromIntegral i * 30 + 4) 14 (hsb 0 0 100) name
                  , Color (hsbA 0 0 100 10) (box (vx + 120) (my + fromIntegral i * 30 - 9) 200 14)
                  , Color (hsb hue 75 100) (box (vx + 120) (my + fromIntegral i * 30 - 9) (200 * clampF 0 1 v) 14) ]
                | (i, (name, v, hue)) <- zip [0 :: Int ..] bars ]
      ++ [ txt vx (my + 100) 14 (hsb (if canLift then 140 else 0) 70 100)
             (if canLift then "Lifts when input torque beats the needed torque" else "Too weak: no shaking will lift it -> add stages")
      -- ---- 文字
         , txt 20 40 18 (hsb 0 0 100) ("Wave hands: turn   1-3: stages " ++ show (wStages w) ++ " (ratio " ++ fmt 1 ratio ++ ")   Up/Down: load "
                                        ++ fmt 1 (wLoad w) ++ "   K: ratchet " ++ (if wRatchet w then "ON" else "off") ++ "   R: lower   goals " ++ show (wGoals w))
         , txt 20 68 15 (hsb 0 0 60) "Output torque = input x ratio, output speed = input / ratio. Seen from the input, the load is 1/ratio as heavy." ]
  ]
  where
    gs = train env (wStages w) (wRot0 w)
    ratio = ratioOf gs
    n = length gs
    gearColor i g = hsba (if gCoax g then 320 else if i == 0 then 8 else if i == n - 1 then 45 else 200) 55 (if gCoax g then 60 else 78) 0.94
    (x0, y0) = case gs of
      (g : _) -> (gx g, gy g)
      [] -> (0, 0)
    gl = case reverse gs of
      (g : _) -> g
      [] -> Gear 0 0 14 False 0 1
    wx = gx gl + drumR
    floorY = envH env - 50
    wy = floorY - 36 - wLifted w
    vx = 30
    vy = 120
    vs = 2
    camW = fromIntegral vw * vs
    camH = fromIntegral vh * vs
    my = vy + camH + 34
    bars = [ ("motion", wMotion w, 50), ("input torque", wTorqueIn w / maxTorque, 0)
           , ("needed torque", min (wLoad w / ratio / maxTorque) 1, 200) ]
    canLift = wLoad w / ratio < maxTorque
    f = wFrame w
    camPic = Translate (vx + camW / 2 - envW env / 2) (envH env / 2 - vy - camH / 2)
               (pixelImage vw vh vs (\x y -> let i = (y * vw + x) * 3 in (B.index f i, B.index f (i + 1), B.index f (i + 2))))

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char k) Down _ _
    | k `elem` "123" -> w { wStages = fromEnum k - fromEnum '0', wOmega = 0, wRot0 = 0, wLifted = 0 }
    | k `elem` "kK" -> w { wRatchet = not (wRatchet w) }
    | k `elem` "rR" -> w { wLifted = 0, wOmega = 0 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wLoad = clampF 0.5 12 (wLoad w + 0.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wLoad = clampF 0.5 12 (wLoad w - 0.5) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1 段（1）ではおもり 3.0 が上がらない。2 段、3 段にすると？ 上がる速さはどう変わる？
-- 2. おもりを 12 まで重くして、3 段で持ち上げよう。ウインチやジャッキが小さな力で重い物を上げるしくみ
-- 3. K で逆転止めを外すと、手を止めたとたんに落ちる。減速比が大きいと落ちる速さは？
-- 4. 仕事 = 力 × 距離。減速しても仕事は増えない（まさつで少し減る）。成功 1 回に必要な「振る回数」を段数ごとに比べよう
-- 5. measureMotion を左右半分に分け、左で振ると逆回転するように改造しよう

-- ---------- 歯車の道具（インボリュート歯形の輪郭を作る） ----------
-- 歯数 z、モジュール m、圧力角 pa（ラジアン）の歯車の輪郭を点のリストで返す
--   歯 1 枚の中心が角度 0（+x 方向）を向く。ha・hf は歯先・歯元の高さ（モジュールの何倍か）
--   インボリュート：基礎円 rb に巻いた糸をほどくときの糸の先。ほどいた角 t の点は
--   半径 rb√(1 + t²)、角度 inv = t − atan t
makeGearOutline :: Int -> Float -> Float -> Float -> Float -> [Point]
makeGearOutline z m pa' ha hf = concatMap tooth [0 .. z - 1]
  where
    zf = fromIntegral z
    r = m * zf / 2
    rb = r * cos pa'
    ra = r + ha * m
    rf = max (r - hf * m) m
    half = pi / (2 * zf) + tan pa' - pa'                -- 基礎円の上での歯の半角
    t0 = if rf > rb then sqrt ((rf / rb) * (rf / rb) - 1) else 0
    t1 = sqrt (max ((ra / rb) * (ra / rb) - 1) (t0 * t0))
    flank = [(rf, half) | rf < rb]                      -- 基礎円より内側はまっすぐ
            ++ [ (rb * sqrt (1 + t * t), max (half - (t - atan t)) 0)
               | i <- [0 .. 8 :: Int], let t = t0 + (t1 - t0) * fromIntegral i / 8 ]
    tooth k =
      let c = fromIntegral k * 2 * pi / zf
      in (rf * cos (c - pi / zf), rf * sin (c - pi / zf))                      -- 歯底の真ん中
         : [ (rr * cos (c - a), rr * sin (c - a)) | (rr, a) <- flank ]
         ++ [ (rr * cos (c + a), rr * sin (c + a)) | (rr, a) <- reverse flank ]

-- 輪郭を (x, y) に角度 rot で描く。gloss の Polygon は凹んだ形を塗れないので、
-- 歯底の円（disc）と、歯 1 枚ずつの凸な多角形（インボリュートの両側の歯面）に分けて塗り、輪郭線を重ねる
gearPic :: Color -> Int -> [Point] -> Float -> Float -> Float -> Picture
gearPic c z pts x y rot =
  let tr (px, py) = (x + px * cos rot - py * sin rot, y + px * sin rot + py * cos rot)
      ps = map tr pts
      closed = ps ++ take 1 ps
      per = length pts `div` max 1 z                     -- 歯 1 枚ぶんの点の数（最初の 1 点は歯底の真ん中）
      teeth = [ take (per - 1) (drop (k * per + 1) ps) | k <- [0 .. z - 1] ]
      rf = case pts of
        ((px, py) : _) -> sqrt (px * px + py * py)
        [] -> 0
  in Pictures [ Color c (Translate x y (disc rf))
              , Color c (Pictures (map Polygon teeth))
              , Color (hsbA 0 0 100 70) (Line closed) ]

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 左上が (x, y)、幅 w、高さ h の塗りつぶした長方形（p5.js の rect）
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
