-- ============================================
-- 12 加法混色と減法混色
-- 光を足す（加法混色）：赤 + 緑 + 青 のスポットライトが重なると白に近づく（ディスプレイ）
-- 色を引く（減法混色）：シアン・マゼンタ・イエローのフィルタを重ねると黒に近づく（印刷・絵の具）
--   目には 3 種類の錐体（L・M・S）しかない。だから 3 色で「ほぼすべての色」が作れる
-- スポット / フィルタをドラッグ / S = 加法 ⇄ 減法 / ↑↓ = 大きさ / ←→ = 明るさ / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Numeric (showFFloat)

-- gloss には加算合成がないので、重なりはピクセルごとに式で計算する
gw, gh, rings :: Int
gw = 356
gh = 222
rings = 24                 -- ★ 同心円の数

cell, canvasW, canvasH :: Float
cell = 3.6                 -- ★ 計算の粗さ（px）
canvasW = 1280
canvasH = 800

data Spot = Spot
  { sX, sY :: !Float
  , sCh    :: !Int         -- 加法ならこのチャンネルを足す、減法ならこのチャンネルを吸収する
  , sNames :: !(String, String) }

data World = World
  { wSub    :: !Bool
  , wRadius :: !Float      -- ★ スポットの半径
  , wLevel  :: !Float      -- ★ 明るさ（0.2〜1）
  , wSpots  :: ![Spot]
  , wDrag   :: !Int        -- つかんでいるスポット（−1 = なし）
  }

spots0 :: Env -> [Spot]
spots0 env =
  let cx = envW env * 0.36
      cy = envH env * 0.5
      r = 95
  in [ Spot cx (cy - r) 0 ("Red", "Cyan")
     , Spot (cx - r * 0.87) (cy + r * 0.5) 1 ("Green", "Magenta")
     , Spot (cx + r * 0.87) (cy + r * 0.5) 2 ("Blue", "Yellow") ]

world0 :: World
world0 = World False 170 1 [] (-1)

main :: IO ()
main = runSketch (sketch "36-12 Additive and Subtractive Color" world0)
  { skBack  = black
  , skStart = \env -> pure world0 { wSpots = spots0 env }
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- 点 (x, y) でのスポットの強さ（中心 1 → 縁 0 のなめらかな山）
falloff :: Float -> Spot -> Float -> Float -> Float
falloff radius s x y =
  let d = norm2 (x - sX s, y - sY s) / radius
  in if d >= 1 then 0 else (1 - d * d) ** 1.5

-- ★ 距離ごとの重なりの表：外の輪から順に重ねた結果（添字 k = 何番目の輪まで入っているか）
--    加法 = 足す、減法 = かけ算
ringTable :: Bool -> Float -> U.Vector Float
ringTable sub level = U.fromList [ value k | k <- [0 .. rings + 1] ]
  where
    w k = (1 - (fromIntegral k / fromIntegral rings) ^ (2 :: Int)) ** 1.5 * level * 2.2 / fromIntegral rings :: Float
    inside k = [ w j | j <- [max 1 k .. rings] ]
    value k
      | sub = foldl' (\p x -> p * (1 - x)) 1 (inside k)
      | otherwise = sum (inside k)

drawAll :: Env -> World -> Picture
drawAll env w =
  let sub = wSub w
      radius = wRadius w
      level = wLevel w
      table = ringTable sub level
      -- スポットをほどいて持っておく（速さのため）
      (ox, oy) = ((envW env - canvasW) / 2, (envH env - canvasH) / 2)
      layer ch = [ (sX s - ox, sY s - oy) | s <- wSpots w, sCh s == ch ]
      lay0 = layer 0
      lay1 = layer 1
      lay2 = layer 2
      tableAt px py (sx, sy) =
        let dx = px - sx
            dy = py - sy
            d2 = dx * dx + dy * dy
        in if d2 >= radius * radius
             then (if sub then 1 else 0)
             else U.unsafeIndex table (ceiling (sqrt d2 / radius * fromIntegral rings))
      channel :: Float -> Float -> [Point] -> Word8
      channel px py ss
        | sub = q (255 * foldl' (\p s -> p * tableAt px py s) 1 ss)
        | otherwise = q (255 * foldl' (\p s -> p + tableAt px py s) 0 ss)
      q v = fromIntegral (max 0 (min 255 (round v :: Int)))
      pixel i j =
        let px = (fromIntegral i + 0.5) * cell
            py = (fromIntegral j + 0.5) * cell
        in (channel px py lay0, channel px py lay1, channel px py lay2)
      fgC = if sub then black else white
      -- マウスの位置の色を計算して表示（式で）
      (mx, my) = mouseP5 env
      ws = [ (sCh s, falloff radius s mx my * level) | s <- wSpots w ]
      mixed ch
        | sub = 255 * foldl' (\p (c, x) -> if c == ch then p * (1 - x) else p) 1 ws
        | otherwise = min 255 (foldl' (\p (c, x) -> if c == ch then p + 255 * x else p) 0 ws)
      (cR, cG, cB) = (mixed 0, mixed 1, mixed 2)
      wd = envW env
      h = envH env
      px0 = wd - 500
      py0 = 110
      -- 下：スペクトルと錐体の感度（簡易近似）
      gx = 60
      gwid = wd - 120
      gy = h - 50
      ghei = 110
      nmAt x = 400 + x / gwid * 300
      spectrum = [ Color (col255 (wavelengthToRGB (nmAt x))) (box (gx + x) (gy - 14) 1.5 14)
                 | k <- [0 .. floor gwid - 1 :: Int], let x = fromIntegral k ]
      cone (peak, sd, c) = Color c (thickPath 2
        [ (gx + x, gy - 18 - exp (negate ((nmAt x - peak) ^ (2 :: Int)) / (2 * sd * sd)) * ghei)
        | k <- [0 .. floor (gwid / 3) :: Int], let x = fromIntegral k * 3 ])
      cones = [ (445, 22, rgb255 90 120 255), (540, 40, rgb255 60 220 90), (570, 45, rgb255 255 90 70) ]
      nameOf s = (if sub then snd else fst) (sNames s)
  in Pictures
    [ Color (if sub then white else black) (rect 0 0 wd h)
    , pixelImage gw gh cell pixel
    , Pictures [ txt env (sX s - 20) (sY s - radius - 10) 14 fgC (nameOf s) | s <- wSpots w ]
    , onP5 env (Pictures
        ( [ Color (if sub then gray (235 / 255) else rgb255 25 25 30) (box (px0 - 16) (py0 - 30) 490 150)
          , Color (makeColor (cR / 255) (cG / 255) (cB / 255) 1) (box px0 (py0 - 10) 70 70)
          , Color fgC (rectLine (px0 + 35) (py0 + 25) 70 70) ]
          ++ spectrum ++ map cone cones ))
    , txt env (px0 + 90) (py0 + 4) 15 fgC "Color at the mouse"
    , txt env (px0 + 90) (py0 + 30) 15 fgC ("R " ++ nf 0 cR ++ "  G " ++ nf 0 cG ++ "  B " ++ nf 0 cB)
    , txt env (px0 + 90) (py0 + 56) 14 fgC
        (if sub then "White x (1-a1) x (1-a2)...: layers darken" else "Red + green + blue: layers brighten")
    , txt env px0 (py0 + 96) 14 fgC
        (if sub then "C + M + Y = black in theory. Print adds K (CMYK)" else "R+G+B = white. R+G yellow, G+B cyan, R+B magenta")
    , txt env gx (gy + 18) 14 fgC "400 nm"
    , txt env (gx + gwid - 60) (gy + 18) 14 fgC "700 nm"
    , txt env gx (gy - ghei - 30) 14 fgC
        "Cone sensitivity, S M L (simple Gaussians). Any light reduces to 3 responses, so 3 colors can make it."
    , txt env 20 40 18 fgC
        ((if sub then "Subtractive (filters, paint)" else "Additive (light)") ++ " (S)  Radius " ++ nf 0 radius
         ++ " (Up/Down)  Brightness " ++ nf 1 level ++ " (L/R)  R: reset")
    , txt env 20 68 15 (if sub then gray (80 / 255) else gray (180 / 255))
        ("Drag the circles to overlap. " ++ (if sub then "Three filters approach black."
           else "Three lights approach white. Two make yellow, cyan, magenta."))
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    w { wDrag = foldl' (\b (i, s) -> if dist2 mouse (sX s, sY s) < wRadius w * 0.6 then i else b) (-1) (zip [0 ..] (wSpots w)) }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 && envDown env ->
    w { wSpots = [ if i == wDrag w then s { sX = fst mouse, sY = snd mouse } else s | (i, s) <- zip [0 ..] (wSpots w) ] }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wRadius = min 320 (wRadius w + 10) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wRadius = max 60 (wRadius w - 10) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wLevel = min 1 (wLevel w + 0.1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wLevel = max 0.2 (wLevel w - 0.1) }
  EventKey (Char c) Down _ _
    | c `elem` "sS" -> w { wSub = not (wSub w) }
    | c `elem` "rR" -> w { wSpots = spots0 env, wRadius = 170, wLevel = 1 }
  _ -> w
  where mouse = mouseP5 env

-- ---------- やってみよう ----------
-- 1. 赤と緑だけ重ねると黄色。でもスペクトルの「黄色の光（580 nm）」は入っていない。目には区別できない（条件等色）
-- 2. 減法混色（S）でシアンとイエローを重ねると緑。絵の具の「青 + 黄 = 緑」はこれ
-- 3. 4 つ目のスポット（白や黄）を spots0 に足してみよう
-- 4. 錐体の感度のピークを動かすと「色覚の違い」のモデルになる。L と M が近いとどうなる？


-- ==================================================
-- ここから下は「道具」（座標の変換・数字）
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

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

-- ==================================================
-- ここから下は「色の道具」（波長 nm → RGB の近似、値は 0〜255）
-- ==================================================
wavelengthToRGB :: Float -> (Float, Float, Float)
wavelengthToRGB nm0 =
  let nm = max 380 (min 780 nm0)
      (r, g, b)
        | nm < 440 = (negate (nm - 440) / 60, 0, 1)
        | nm < 490 = (0, (nm - 440) / 50, 1)
        | nm < 510 = (0, 1, negate (nm - 510) / 20)
        | nm < 580 = ((nm - 510) / 70, 1, 0)
        | nm < 645 = (1, negate (nm - 645) / 65, 0)
        | otherwise = (1, 0, 0)
      f | nm < 420 = 0.3 + 0.7 * (nm - 380) / 40
        | nm > 700 = 0.3 + 0.7 * (780 - nm) / 80
        | otherwise = 1
      q v = 255 * (v * f) ** 0.8
  in (q r, q g, q b)

-- 0〜255 の値から色を作る
col255 :: (Float, Float, Float) -> Color
col255 (r, g, b) = makeColor (r / 255) (g / 255) (b / 255) 1
