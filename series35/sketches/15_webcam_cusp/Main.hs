-- ============================================
-- 15 カメラで操作するカスプ（おまけ）
-- カメラの映像から「いちばん明るい所」または「動いた所」を見つけ、その位置をコントロール (a, b) にする
--   スマホのライトや手を動かすと、カスプの系 V = x⁴ + a·x² + b·x の状態 x が谷をすべり、折り目でとぶ
--   カメラが使えないときはマウスで動く（フォールバック）
-- 1 = 明るい所を追う / 2 = 動いた所を追う / ↑↓ = 動きのしきい値 / C = 軌跡を消す（ffmpeg とカメラの許可が必要）
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Numeric (showFFloat)

vw, vh :: Int
vw = 160                   -- ★ カメラ画像の大きさ（小さいほど速い）
vh = 120

data Pt = Pt !Float !Float !Float      -- (a, b, x)

data World = World
  { wCam    :: !(Maybe Camera)
  , wKind   :: !Int        -- 1 明るさ, 2 動き
  , wThr    :: !Int        -- ★ 動きのしきい値
  , wPrev   :: !(Maybe B.ByteString)
  , wImage  :: !Picture
  , wLive   :: !Bool
  , wTarget :: !(Maybe Point)
  , wA      :: !Float
  , wB      :: !Float
  , wX      :: !Float
  , wTrail  :: !(Seq Pt)
  , wFlash  :: !Float
  , wJumps  :: !Int
  }

world0 :: World
world0 = World Nothing 1 40 Nothing Blank False Nothing (-1) 0 1 Seq.empty 0 0

main :: IO ()
main = runSketch (sketch "35-15 Webcam Cusp" world0)
  { skBack  = hsb 170 30 10
  , skStart = \_ -> do
      cam <- startCamera vw vh
      pure world0 { wCam = Just cam }
  , skStep  = stepWorld
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- ★ 画像から目標の位置（0〜1）を探す。結果と、次に使う「1 つ前の画像」を返す
findTarget :: Int -> Int -> Maybe B.ByteString -> B.ByteString -> (Maybe Point, Maybe B.ByteString)
findTarget kind thr prev px
  | B.length px < vw * vh * 3 = (Nothing, prev)
  | kind == 1 =
      let maxB = maxBright px
      in if maxB < 300 then (Nothing, prev)
         else (center (sumWhere (\i -> sum3 px i * 10 >= maxB * 9)) 1, prev)
  | otherwise =
      case prev of
        Just old | B.length old == B.length px ->
          (center (sumWhere (\i -> diff3 px old i > thr * 3)) 20, Just px)
        _ -> (Nothing, Just px)
  where
    center (sx, sy, n) least
      | n < least || n == 0 = Nothing
      | otherwise = Just ( fromIntegral sx / fromIntegral n / fromIntegral vw
                         , fromIntegral sy / fromIntegral n / fromIntegral vh )

-- 条件をみたす画素の x, y の合計と個数
sumWhere :: (Int -> Bool) -> (Int, Int, Int)
sumWhere test = go 0 0 0 0
  where
    total = vw * vh
    go :: Int -> Int -> Int -> Int -> (Int, Int, Int)
    go !i !sx !sy !n
      | i >= total = (sx, sy, n)
      | test i = go (i + 1) (sx + i `mod` vw) (sy + i `div` vw) (n + 1)
      | otherwise = go (i + 1) sx sy n

sum3 :: B.ByteString -> Int -> Int
sum3 b i = g (i * 3) + g (i * 3 + 1) + g (i * 3 + 2)
  where g k = fromIntegral (BU.unsafeIndex b k)

diff3 :: B.ByteString -> B.ByteString -> Int -> Int
diff3 a b i = d (i * 3) + d (i * 3 + 1) + d (i * 3 + 2)
  where d k = abs (fromIntegral (BU.unsafeIndex a k) - fromIntegral (BU.unsafeIndex b k) :: Int)

maxBright :: B.ByteString -> Int
maxBright b = go 0 0
  where
    go :: Int -> Int -> Int
    go !i !m | i >= vw * vh = m
             | otherwise = go (i + 1) (max m (sum3 b i))

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  (live, tg, prev, img) <- case wCam w0 of
    Nothing -> pure (False, Nothing, wPrev w0, Blank)
    Just cam -> do
      on <- camLive cam
      -- カメラが使えないときは合成映像を追わず、p5.js 版と同じくマウスで操作する
      if not on then pure (False, Nothing, Nothing, Blank) else do
        f <- cameraFrame cam
        let (t, p) = findTarget (wKind w0) (wThr w0) (wPrev w0) f
            k = videoWidth env / fromIntegral vw
        pure (True, t, p, if B.length f >= vw * vh * 3 then camImage cam f k else Blank)
  let (mx, my) = mouseP5 env
      (tb, ta) = case tg of
        Just (u, v) -> (-3 + 6 * u, 1.5 - 4.5 * v)
        Nothing -> ( clampF (-3) 3 (remap mx 0 (envW env) (-3) 3)
                   , clampF (-3) 1.5 (remap my 100 (envH env) 1.5 (-3)) )
      a = wA w0 + (ta - wA w0) * 0.15
      b = wB w0 + (tb - wB w0) * 0.15
      descend :: Int -> Float -> Float
      descend 0 v = v
      descend k v = descend (k - 1) (v - 0.004 * (4 * v * v * v + 2 * a * v + b))
      x = clampF (-1.8) 1.8 (descend 20 (wX w0))
      jump = abs (x - wX w0) > 0.06 && wFlash w0 < 20
      flash = max 0 ((if jump then 40 else wFlash w0) - 1)
      tr = let s = wTrail w0 |> Pt a b x in if Seq.length s > 400 then Seq.drop 1 s else s
  pure w0 { wLive = live, wTarget = tg, wPrev = prev, wImage = img, wA = a, wB = b, wX = x
          , wFlash = flash, wTrail = tr, wJumps = if jump then wJumps w0 + 1 else wJumps w0 }

planeSize, videoLeft, videoWidth :: Env -> Float
planeSize env = min (envH env - 150) (envW env * 0.5)
videoLeft env = 30 + planeSize env + 30
videoWidth env = min (envW env - videoLeft env - 30) 320

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      py = 110
      sz = planeSize env
      px0 = 30
      a = wA w
      b = wB w
      x = wX w
      flash = wFlash w
      warm = hsb 25 90 100
      cool = hsb 195 80 100
      panel = hsb 170 25 17
      toB v = px0 + (v + 3) / 6 * sz
      toA v = py + (1.5 - v) / 4.5 * sz
      -- カスプの内側：へこんだ形なので、横長の台形に分けて塗る
      half = [ (-6 * s * s, 8 * s * s * s) | i <- [0 .. 35 :: Int], let s = 0.02 * fromIntegral i ]
      inner = zipWith (\(a0, b0) (a1, b1) -> Polygon [(toB (negate b0), toA a0), (toB b0, toA a0), (toB b1, toA a1), (toB (negate b1), toA a1)]) half (drop 1 half)
      outline = [ (toB (8 * s * s * s), toA (-6 * s * s)) | i <- [-35 .. 35 :: Int], let s = 0.02 * fromIntegral i ]
      dots = [ Color (withA 0.4 (if tx > 0 then warm else cool)) (at (toB tb, toA ta) (disc 2.5)) | Pt ta tb tx <- toList (wTrail w) ]
      -- 右上：カメラ映像
      rx = videoLeft env
      rw = wd - rx - 30
      vwid = videoWidth env
      vhei = vwid * fromIntegral vh / fromIntegral vw
      -- 右下：いまの地形 V(x)
      gy = py + vhei + 40
      gh = h - gy - 40
      toX v = rx + (v + 1.8) / 3.6 * rw
      vOf v = v * v * v * v + a * v * v + b * v
      xs100 = [ -1.8 + 3.6 * fromIntegral i / 100 | i <- [0 .. 100 :: Int] ] :: [Float]
      vmin = foldr (min . vOf) 1e9 xs100
      toY v = gy + gh - 15 - (v - vmin) * gh * 0.15
      status | not (wLive w) = "No camera: using the mouse"
             | otherwise = case wTarget w of
                 Nothing -> "Not found: using the mouse"
                 Just _ -> if wKind w == 1 then "Tracking the bright spot" else "Tracking the motion"
  in Pictures
    [ onP5 env (Pictures
        [ Color panel (box px0 py sz sz)
        , if flash > 0 then Color (hsbA 50 90 100 flash) (box px0 py sz sz) else Blank
        , Color (hsbA 50 80 100 25) (Pictures inner)
        , Color (hsb 50 80 100) (thickPath 2 (outline ++ take 1 outline))
        , Pictures dots
        , Color (if x > 0 then warm else cool) (at (toB b, toA a) (disc 11))
        , if wLive w then Blank else Color (gray 0.2) (box rx py vwid vhei)
        ])
    , if wLive w then Translate (rx + vwid / 2 - wd / 2) (h / 2 - py - vhei / 2) (wImage w) else Blank
    , onP5 env (Pictures
        [ case wTarget w of
            Just (u, v) -> Color (hsb 120 90 100) (at (rx + u * vwid, py + v * vhei) (ring 10 3))
            Nothing -> Blank
        , Color panel (box rx gy rw gh)
        , Color (hsb 160 60 90) (thickPath 3 [ (toX v, clampF gy (gy + gh) (toY (vOf v))) | v <- xs100 ])
        , Color (hsb 45 90 100) (at (toX x, clampF (gy + 10) (gy + gh) (toY (vOf x) - 10)) (disc 9))
        ])
    , txt env (px0 + 12) (py + 22) 14 (gray 0.85) "Control (a, b): the video position goes in here. Yellow = cusp"
    , txt env rx (py + vhei + 20) 14 (gray 0.85) status
    , txt env (rx + 12) (gy + 20) 14 (gray 0.85) "Current terrain V(x) and the ball"
    , txt env 20 36 16 white ("1: bright  2: motion (" ++ (if wKind w == 1 then "bright" else "motion") ++ ")   Up/Dn: threshold " ++ show (wThr w)
                              ++ "   C: clear trail   jumps " ++ show (wJumps w) ++ "   x = " ++ nf 2 x)
    , txt env 20 62 14 (gray 0.6) "Swing a light left and right: inside the cusp the ball jumps at different places each way."
    , txt env 20 84 14 (gray 0.6) "Lower on screen (a < 0), the jumps are bigger."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (Char '1') Down _ _ -> w { wKind = 1 }
  EventKey (Char '2') Down _ _ -> w { wKind = 2, wPrev = Nothing }
  EventKey (SpecialKey KeyUp) Down _ _   -> w { wThr = min 120 (wThr w + 5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wThr = max 10 (wThr w - 5) }
  EventKey (Char c) Down _ _ | c `elem` "cC" -> w { wTrail = Seq.empty }
  _ -> w

-- ---------- やってみよう ----------
-- 1. ライトを画面の上の方（a > 0）と下の方（a < 0）で左右に振り、とび方を比べよう
-- 2. 2（動き）で、手をゆっくり動かしたときと速く動かしたときの違いは？
-- 3. 映像の明るさ全体の平均を a に、明るい所の横位置を b にしてみよう（部屋の電気で地形が変わる）
-- 4. とんだ瞬間に 13 の playTone を鳴らして、カメラの楽器にしよう


-- ==================================================
-- ここから下は「道具」（座標の変換・色・数字）
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を画面に合わせる
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- p5.js の text(s, x, y) と同じ位置に文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

-- マウスの位置（左上が原点、y は下向き）
mouseP5 :: Env -> Point
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上が (x, y) の長方形（p5.js の rect）
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

-- 透明度を 0〜100 で書く HSB（p5.js と同じ）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

onOff :: Bool -> String
onOff b = if b then "ON" else "off"
