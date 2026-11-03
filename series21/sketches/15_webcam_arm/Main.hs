-- ============================================
-- 15 カメラで操るアーム（おまけ）
-- カメラの映像から「いちばん明るい所」または「選んだ色」の塊を見つけ、その位置をアームの目標にする
--   見つけた画素の平均位置（重心）= 目標 → 2 リンクの IK（01）で腕が追いかける
-- 映像をクリック = その色を追う / B = いちばん明るい所を追う / ←→ = 色のしきい値 / E = ひじの向き（カメラがなければ作った映像で動く）
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Char (toLower)
import Data.List (foldl')

vw, vh, minPixels :: Int
vw = 160                  -- ★ カメラ画像の大きさ（小さいほど速い）
vh = 120
minPixels = 15            -- ★ これより少ないと「見つからない」

l1, l2, smoothRate, videoS :: Float
l1 = 180
l2 = 140
smoothRate = 0.12
videoS = 2                -- カメラ映像を何倍で見せるか

data Blob = Blob !Float !Float !Int     -- 重心（カメラ座標）と画素数

data World = World
  { wCam    :: !(Maybe Camera)
  , wFrame  :: !B.ByteString
  , wLive   :: !Bool
  , wQ1     :: !Float
  , wQ2     :: !Float
  , wUp     :: !Bool
  , wColor  :: !Bool                 -- False = 明るい所、True = 選んだ色
  , wPick   :: !(Int, Int, Int)      -- 追う色
  , wThr    :: !Int                  -- ★ 色の近さのしきい値（小さいほど厳しい）
  , wBlob   :: !(Maybe Blob)         -- 見つけた重心
  , wTarget :: !Point
  }

main :: IO ()
main = runSketch (sketch "21-15 Webcam Arm" world0)
  { skBack  = hsb 215 35 12
  , skStart = \_ -> do
      cam <- startCamera vw vh
      pure world0 { wCam = Just cam }
  , skStep  = \env _ w -> step env w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = World Nothing B.empty False 0.8 0.9 True False (255, 255, 255) 60 Nothing (width * 0.6, height * 0.4)

basePos :: Point
basePos = (width * 0.4, height * 0.65)

-- 映像の左上の位置
videoX, videoY :: Float
videoX = width - fromIntegral vw * videoS - 20
videoY = 100

inverse :: Point -> Bool -> (Float, Float)
inverse (tx, ty) up =
  let (bx, by) = basePos
      x = tx - bx
      y = by - ty
      r = clampF (abs (l1 - l2) + 0.01) (l1 + l2 - 0.01) (sqrt (x * x + y * y))
      c2 = clampF (-1) 1 ((r * r - l1 * l1 - l2 * l2) / (2 * l1 * l2))
      a2 = acos c2 * (if up then 1 else -1)
  in (atan2 y x - atan2 (l2 * sin a2) (l1 + l2 * cos a2), a2)

forward :: Float -> Float -> (Point, Point)
forward a1 a2 =
  let (bx, by) = basePos
      ex = bx + l1 * cos a1
      ey = by - l1 * sin a1
  in ((ex, ey), (ex + l2 * cos (a1 + a2), ey - l2 * sin (a1 + a2)))

approach :: Float -> Float -> Float
approach cur goal = let d = goal - cur in cur + atan2 (sin d) (cos d) * smoothRate

-- i 番目の画素の RGB
pixelAt :: B.ByteString -> Int -> (Int, Int, Int)
pixelAt f i =
  let g k = fromIntegral (BU.unsafeIndex f (i * 3 + k))
  in (g 0, g 1, g 2)

-- 条件に合う画素の座標の合計と数
data Acc = Acc !Int !Int !Int

-- ★ 画像から塊の重心を探す
findBlob :: World -> B.ByteString -> Maybe Blob
findBlob w f
  | B.length f < total * 3 = Nothing
  | not (wColor w) && maxB < 300 = Nothing
  | n < minPixels = Nothing
  | otherwise = Just (Blob (fromIntegral sx / fromIntegral n) (fromIntegral sy / fromIntegral n) n)
  where
    total = vw * vh
    brightness i = let (r, g, b) = pixelAt f i in r + g + b
    -- いちばん明るい画素のまわり（上位）を平均：まず最大を探し、その 85% 以上の画素を集める
    maxB = foldl' (\m i -> max m (brightness i)) 0 [0 .. total - 1]
    thr = fromIntegral maxB * 0.85 :: Float
    (pr, pg, pb) = wPick w
    hit i
      | wColor w = let (r, g, b) = pixelAt f i in abs (r - pr) + abs (g - pg) + abs (b - pb) < wThr w
      | otherwise = fromIntegral (brightness i) >= thr
    Acc sx sy n = foldl' add (Acc 0 0 0) [0 .. total - 1]
    add acc@(Acc ax ay c) i
      | hit i = Acc (ax + i `mod` vw) (ay + i `div` vw) (c + 1)
      | otherwise = acc

step :: Env -> World -> IO World
step env w = case wCam w of
  Nothing -> pure w
  Just cam -> do
    f <- cameraFrame cam
    live <- camLive cam
    let (bx, by) = basePos
        blob = findBlob w f
        reach = l1 + l2
        (tx0, ty0) = wTarget w
        target = case blob of
          -- カメラ座標 → アームの届く範囲（ドーナツの上半分くらい）に写す
          Just (Blob x y _) ->
            let nx = x / fromIntegral vw
                ny = y / fromIntegral vh
                gx = bx + (nx - 0.5) * reach * 1.8
                gy = by - 40 - (1 - ny) * reach * 0.9
            in (lerpF tx0 gx 0.3, lerpF ty0 gy 0.3)
          -- 見つからないときはマウスが目標
          Nothing | envMouse env /= (0, 0) -> toP5 (envMouse env)
                  | otherwise -> wTarget w
        (a1, a2) = inverse target (wUp w)
    pure w { wFrame = f, wLive = live, wBlob = blob, wTarget = target
           , wQ1 = approach (wQ1 w) a1, wQ2 = approach (wQ2 w) a2 }

drawAll :: World -> Picture
drawAll w =
  let b@(bx, by) = basePos
      (e, h) = forward (wQ1 w) (wQ2 w)
      vwF = fromIntegral vw * videoS
      vhF = fromIntegral vh * videoS
      hasFrame = B.length (wFrame w) >= vw * vh * 3
      video = case wCam w of
        Just cam | hasFrame ->
          at (videoX + vwF / 2 - width / 2, height / 2 - (videoY + vhF / 2)) (camImage cam (wFrame w) videoS)
        _ -> Blank
      (pr, pg, pb) = wPick w
      blobMark = case wBlob w of
        Just (Blob x y _) ->
          let cx = videoX + x * videoS
              cy = videoY + y * videoS
          in Color (hsb 120 90 100) (Pictures
               [ at (cx, cy) (ring 10 2)
               , thickLine 2 (cx - 14, cy) (cx + 14, cy)
               , thickLine 2 (cx, cy - 14) (cx, cy + 14) ])
        Nothing -> Blank
      status = case wBlob w of
        Just (Blob _ _ n) -> "Found (" ++ show n ++ " px)"
        Nothing -> "Not found: mouse is the target"
  in Pictures
  [ view
      [ Color (hsbA 45 80 100 30) (Pictures [at b (ring (l1 + l2) 1), at b (ring (abs (l1 - l2)) 1)])   -- 届く範囲
      , Color (hsb 200 60 95) (thickLine 14 b e)
      , Color (hsb 320 60 95) (thickLine 14 e h)
      , Color (hsb 0 0 100) (Pictures [at b (disc 11), at e (disc 9)])
      , Color (hsb 45 90 100) (at h (disc 8))
      , Color (hsb 0 0 40) (boxAt (bx - 30) by 60 14)
      , Color (hsb 120 90 100) (at (wTarget w) (ring 11 2))
      ]
  , if hasFrame then Blank else view [Color (hsb 0 0 30) (boxAt videoX videoY vwF vhF)]
  , video
  , view
      [ Color (hsbA 0 0 100 60) (rectLine (videoX + vwF / 2) (videoY + vhF / 2) vwF vhF)
      , blobMark
      , if wColor w then Color (rgb255 pr pg pb) (boxAt videoX (videoY + vhF + 8) 40 24) else Blank
      ]
  , if hasFrame then Blank else txt (videoX + 20) (videoY + vhF / 2) 14 (hsb 0 0 70) "Waiting for camera..."
  , if wLive w then Blank else txt videoX (videoY + vhF + 56) 14 (hsb 0 0 70) "No camera: demo video"
  , txt 20 34 16 (hsb 0 0 100) ("Track: " ++ (if wColor w then "picked color" else "brightest spot (B)") ++ "   Click video: track that color   Threshold "
                                ++ show (wThr w) ++ " (L/R)   E: elbow")
  , txt 20 58 14 (hsb 0 0 100) status
  , txt 20 78 14 (hsb 0 0 60) "Move a phone light or white paper and the arm follows. Pick a color not in the background. No blob: mouse."
  ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 pos
        vwF = fromIntegral vw * videoS
        vhF = fromIntegral vh * videoS
        inside = mx >= videoX && mx <= videoX + vwF && my >= videoY && my <= videoY + vhF
        ix = max 0 (min (vw - 1) (floor ((mx - videoX) / videoS)))
        iy = max 0 (min (vh - 1) (floor ((my - videoY) / videoS)))
    in if inside && B.length (wFrame w) >= vw * vh * 3
         then w { wPick = pixelAt (wFrame w) (ix + iy * vw), wColor = True }
         else w
  EventKey (Char c) Down _ _ -> case toLower c of
    'b' -> w { wColor = False }
    'e' -> w { wUp = not (wUp w) }
    _ -> w
  EventKey (SpecialKey k) Down _ _ -> case k of
    KeyRight -> w { wThr = min 250 (wThr w + 10) }
    KeyLeft -> w { wThr = max 10 (wThr w - 10) }
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 色モードで 2 色を追って（赤と青）、2 本の腕を動かす。両手で指揮者のように
-- 2. 重心だけでなく「大きさ（画素数）」も使って、近づけたら腕を伸ばす・遠ざけたら縮める
-- 3. 14 の音をつなぐ：カメラで動かした腕がモーター音を出す
-- 4. 重心は「平均」なので 2 つの塊があると真ん中になる。いちばん大きい塊だけ選ぶには？（連結成分 = シリーズ 9 の β0）


-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- マウスの位置（gloss の座標：中央が原点、y は上向き）を p5 と同じ座標（左上が原点、y は下向き）に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

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

