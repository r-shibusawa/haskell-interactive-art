-- ============================================
-- 12 勾配降下法
-- 山の斜面で「いちばん急な下り坂（−勾配）」の方向へ少しずつ進むと、谷底（最小値）に近づく
--   x ← x − η·∇f(x)    η（学習率）が小さすぎると遅く、大きすぎると行き過ぎて振動・発散する
-- P キーで「制約つき」：動ける範囲（円）の外に出たら、いちばん近い円の中の点に戻す（射影）
-- 操作：クリック = 出発点 / スペース = 再生・停止 / → = 1 歩 / ↑↓ = 学習率 η
--       P = 制約（円）あり⇄なし / 1〜3 = 地形（細長い谷・バナナ形の谷・谷がいくつも）
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

speed :: Int
speed = 6               -- ★ 何フレームに 1 歩

-- 数値微分の誤差を小さくするため、座標と f は Double で計算する
data World = World
  { wFn       :: !Int
  , wEta      :: !Double             -- ★ 学習率
  , wCons     :: !Bool
  , wPos      :: !(Double, Double)
  , wTrail    :: ![(Double, Double)] -- たどった道（新しい順、400 点まで）
  , wFHist    :: ![Double]           -- f の履歴（新しい順）
  , wDiverged :: !Bool
  , wAuto     :: !Bool
  , wTerrain  :: !Picture            -- 地形の絵（あらかじめ描いておく）
  }

main :: IO ()
main = runSketch (sketch "17-12 Gradient descent" initialWorld)
  { skBack  = cBg
  , skStart = \env -> pure (pickFn env 1 initialWorld)
  , skStep  = \env _ w -> pure (if wAuto w && envFrame env `mod` speed == 0 then descend w else w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialWorld :: World
initialWorld = World 1 0.05 False (0, 0) [] [] False True Blank

-- ★ 地形（損失関数）
f :: Int -> Double -> Double -> Double
f 1 x y = 0.5 * (x * x + 10 * y * y)                                    -- 細長いお椀
f 2 x y = (1 - x) ^ (2 :: Int) + 6 * (y - x * x) ^ (2 :: Int)           -- ローゼンブロック（バナナ形）
f _ x y = 0.15 * (x * x + y * y) - cos (1.8 * x) * cos (1.8 * y)        -- 谷がいくつも

grad :: Int -> Double -> Double -> (Double, Double)
grad fn x y =
  let h = 1e-4
  in ((f fn (x + h) y - f fn (x - h) y) / (2 * h), (f fn x (y + h) - f fn x (y - h)) / (2 * h))

-- 制約：中心 (cx, cy)、半径 r の円
disk :: Int -> (Double, Double, Double)
disk fn = if fn == 2 then (-0.8, 1.2, 0.9) else (-1.6, 0.9, 1.0)

-- 表示する範囲 (x0, x1, y0, y1)
domOf :: Int -> (Double, Double, Double, Double)
domOf fn = if fn == 2 then (-2, 2, -1, 3) else (-3.5, 3.5, -2.5, 2.5)

pickFn :: Env -> Int -> World -> World
pickFn env k w =
  let eta = case k of { 2 -> 0.02; 1 -> 0.05; _ -> 0.2 }
      sx = if k == 2 then -1.5 else 3
      sy = case k of { 2 -> 2.5; 1 -> 1.8; _ -> 1.5 }
  in startAt sx sy w { wFn = k, wEta = eta, wTerrain = drawTerrain env k }

-- 画面座標 ⇄ 地形の座標（地図部分は左側）
mapW :: Env -> Float
mapW env = envW env - 300

toScreen :: Env -> Int -> (Double, Double) -> Point
toScreen env fn (x, y) =
  let (x0, x1, y0, y1) = domOf fn
      tx = realToFrac ((x - x0) / (x1 - x0))
      ty = realToFrac ((y - y0) / (y1 - y0))
  in (20 + tx * (mapW env - 40), (envH env - 20) + ty * (110 - (envH env - 20)))

toWorld :: Env -> Int -> Point -> (Double, Double)
toWorld env fn (sx, sy) =
  let (x0, x1, y0, y1) = domOf fn
      tx = realToFrac ((sx - 20) / (mapW env - 40))
      ty = realToFrac ((sy - (envH env - 20)) / (110 - (envH env - 20)))
  in (x0 + tx * (x1 - x0), y0 + ty * (y1 - y0))

-- 地形を 6 ピクセルのマスで塗る（対数スケールで色分け）
drawTerrain :: Env -> Int -> Picture
drawTerrain env fn =
  let g = 6 :: Int
      gF = fromIntegral g :: Float
      cols = max 1 (ceiling ((mapW env - 40) / gF)) :: Int
      rows = max 1 (ceiling ((envH env - 130) / gF)) :: Int
      val c r =
        let (x, y) = toWorld env fn (20 + fromIntegral c * gF + gF / 2, 110 + fromIntegral r * gF + gF / 2)
        in f fn x y
      vals = U.generate (cols * rows) (\i -> val (i `mod` cols) (i `div` cols))
      lo = U.minimum vals
      hi = U.maximum vals
      colorAt c r =
        let v = vals U.! (r * cols + c)
            t = log (1 + (v - lo)) / log (1 + (hi - lo))
            band = fromIntegral (floor (t * 14) :: Int) / 14 :: Float
            mix a b = (a + (b - a) * band) / 255
        in (mix 0x2a 0xe7, mix 0x1d 0xb5, mix 0x4a 0xff)
      cx = 20 + fromIntegral cols * gF / 2
      cy = 110 + fromIntegral rows * gF / 2
  in at (pt env (cx, cy)) (pixelImageF cols rows gF colorAt)

startAt :: Double -> Double -> World -> World
startAt x y w = w { wPos = (x, y), wTrail = [(x, y)], wFHist = [f (wFn w) x y], wDiverged = False }

-- ★ 1 歩進む
descend :: World -> World
descend w
  | wDiverged w = w
  | otherwise =
      let (px, py) = wPos w
          fn = wFn w
          (gx', gy') = grad fn px py
          x0 = px - wEta w * gx'
          y0 = py - wEta w * gy'
          (x, y)
            | wCons w =   -- 射影：円の外に出たら円周上のいちばん近い点へ
                let (cx, cy, r0) = disk fn
                    dx = x0 - cx
                    dy = y0 - cy
                    r = sqrt (dx * dx + dy * dy)
                in if r > r0 then (cx + dx / r * r0, cy + dy / r * r0) else (x0, y0)
            | otherwise = (x0, y0)
          bad v = isNaN v || isInfinite v || abs v > 1e3
      in if bad x || bad y
           then w { wDiverged = True }
           else w { wPos = (x, y), wTrail = take 400 ((x, y) : wTrail w)
                  , wFHist = take 400 (f fn x y : wFHist w) }

drawW :: Env -> World -> Picture
drawW env w =
  let ww = envW env
      hh = envH env
      fn = wFn w
      scr = toScreen env fn
      (x0, x1, y0, y1) = domOf fn
      -- 制約の円（点線）
      consPic
        | wCons w =
            let (cx, cy, r) = disk fn
                (sx, sy) = scr (cx, cy)
                rx = realToFrac (r / (x1 - x0)) * (mapW env - 40)
                ry = realToFrac (r / (y1 - y0)) * (hh - 130)
                onE a = (sx + rx * cos a, sy + ry * sin a)
                count = 40 :: Int
            in Pictures [ seg env cMint 3 (onE (tau * fromIntegral i / fromIntegral count))
                                         (onE (tau * (fromIntegral i + 0.55) / fromIntegral count))
                        | i <- [0 .. count - 1] ]
        | otherwise = Blank
      trailS = map scr (reverse (wTrail w))
      posS@(sx', sy') = scr (wPos w)
      (gxv, gyv) = grad fn (fst (wPos w)) (snd (wPos w))
      m = sqrt (gxv * gxv + gyv * gyv)
      -- −勾配の矢印
      arrowPic
        | m > 1e-3 =
            let l = 60
                ux = realToFrac (gxv / m)
                uy = realToFrac (gyv / m)
            in seg env cBad 3 posS (sx' - ux * l, sy' + uy * l)
        | otherwise = Blank
      -- パネル
      px = ww - 280
      hist = reverse (wFHist w)
      graph
        | length hist > 1 =
            let lo = minimum hist
                hi = maximum hist
                n = length hist
                vx i = px + 20 + fromIntegral (i :: Int) / fromIntegral (n - 1) * 220
                vy v = 220 - realToFrac ((v - lo) / max (hi - lo) 1e-9) * 150
            in path env cOrchid 2 [(vx i, vy v) | (i, v) <- zip [0 ..] hist]
        | otherwise = Blank
      (ax, ay) = wPos w
      infoLines =
        [ "eta = " ++ prec2 (wEta w)
        , "x = (" ++ fixD 3 ax ++ ", " ++ fixD 3 ay ++ ")"
        , "f(x) = " ++ fixD 4 (f fn ax ay)
        , "Steps " ++ show (length (wTrail w) - 1)
        , "Disk: " ++ (if wCons w then "on (inside only)" else "off") ]
  in Pictures
       [ wTerrain w
       , consPic
       , path env cGold 2.5 trailS
       , Pictures (map (\p -> dot env cGold p 5) trailS)
       , arrowPic
       , dot env cFg posS 14
       , box env cPanel px 20 260 (hh - 40)
       , txt env (px + 18) 36 17 cFg "Change of f(x)"
       , graph
       , Pictures [ txt env (px + 18) (240 + fromIntegral k * 22) 15 cFg l | (k, l) <- zip [0 :: Int ..] infoLines ]
       , if wDiverged w
           then Pictures [ txt env (px + 18) 380 18 cBad "Diverged!"
                         , txt env (px + 18) 404 18 cBad "Try a smaller eta" ]
           else Blank
       , txt env (px + 18) (hh - 90) 14 cDim "Red arrow = -grad f"
       , txt env (px + 18) (hh - 68) 14 cDim "Gold = path taken"
       , txt env 30 24 22 cFg "Gradient descent: small steps downhill"
       , txt env 30 56 14 cDim "Click start  Space play/stop  Right step  Up/Down eta  P disk  1 valley  2 banana  3 bumpy"
       ]

-- 小数の表示
fixD :: Int -> Double -> String
fixD d v = showFFloat (Just d) v ""

-- 有効数字 2 桁（JavaScript の toPrecision(2)）
prec2 :: Double -> String
prec2 v
  | v <= 0 = "0"
  | otherwise = fixD (max 0 (1 - floor (logBase 10 v))) v

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | mx < mapW env - 20 && my > 110 ->
        let (x, y) = toWorld env (wFn w) m in startAt x y w
  EventResize _ -> pickFn env (wFn w) w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> descend w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wEta = min 1e6 (wEta w * 1.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wEta = max 1e-9 (wEta w / 1.5) }
  EventKey (Char c) Down _ _
    | c `elem` "pP" ->
        let w1 = w { wCons = not (wCons w) }
        in case reverse (wTrail w1) of
             ((x, y) : _) -> startAt x y w1
             [] -> w1
    | c `elem` "123" -> pickFn env (fromEnum c - fromEnum '0') w
  _ -> w
  where m@(mx, my) = mouse env

-- ---------- やってみよう ----------
-- 1. 細長い谷（1）で η を少しずつ上げて、振動が始まる値、発散する値を探す
-- 2. 谷がいくつも（3）で、出発点を変えると、たどり着く谷はどう変わる？
-- 3. P で制約をつけて、円のふちに沿って動く様子を見る

-- ---------- 道具：色 ----------
cBg, cPanel, cFg, cDim, cOrchid, cMint, cGold, cBad, cEdge, cGreen, cRed, cAmber, cBlue :: Color
cBg     = rgb255 0x14 0x10 0x1f
cPanel  = rgb255 0x22 0x1a 0x33
cFg     = rgb255 0xe9 0xe4 0xf5
cDim    = rgb255 0x8c 0x84 0xa3
cOrchid = rgb255 0xf0 0xa3 0xff
cMint   = rgb255 0x6f 0xe3 0xd0
cGold   = rgb255 0xff 0xcf 0x6b
cBad    = rgb255 0xff 0x6b 0x81
cEdge   = rgb255 0x3a 0x2f 0x52
cGreen  = rgb255 0x2f 0x5e 0x57
cRed    = rgb255 0x5e 0x2a 0x3a
cAmber  = rgb255 0x5a 0x4a 0x1f
cBlue   = rgb255 0x2f 0x4a 0x5e

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置を p5.js の座標で
mouse :: Env -> Point
mouse env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の枠
outline :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
outline env c t x y w h = Color c (thickPath t (map (pt env) [(x - t / 2, y), (x + w + t / 2, y), (x + w, y), (x + w, y + h), (x + w + t / 2, y + h), (x - t / 2, y + h), (x, y + h), (x, y)]))

-- 太さ t の線分（p5.js の line）と折れ線
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

path :: Env -> Color -> Float -> [Point] -> Picture
path env c t ps = Color c (thickPath t (map (pt env) ps))

-- 点線
dashed :: Env -> Color -> Float -> Float -> Point -> Point -> Picture
dashed env c t d a@(x1, y1) b@(x2, y2) =
  let l = dist2 a b
      k = floor (l / (2 * d)) :: Int
      f s = (x1 + (x2 - x1) * s / max l 1e-6, y1 + (y2 - y1) * s / max l 1e-6)
  in Pictures [seg env c t (f (2 * d * fromIntegral i)) (f (min l (2 * d * fromIntegral i + d))) | i <- [0 .. k]]

-- 直径 d の円（p5.js の circle）と、太さ t の輪
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

hoop :: Env -> Color -> Float -> Point -> Float -> Picture
hoop env c t p d = Color c (at (pt env p) (ring (d / 2) t))

-- 塗りつぶしの多角形（凸形のみ）
fillPoly :: Env -> Color -> [Point] -> Picture
fillPoly env c ps = Color c (Polygon (map (pt env) ps))

-- 文字：txt は左上が (x, y)、txtM は左端で上下中央、txtC は中心が (x, y)
txt, txtM, txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env (y + s)) s c str
txtM env x y s c str = label (gx env x) (gy env (y + s / 2)) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str

-- 小数点以下 d 桁（JavaScript の toFixed）
fix :: Int -> Float -> String
fix d v = showFFloat (Just d) v ""

