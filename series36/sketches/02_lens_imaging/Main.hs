-- ============================================
-- 02 薄レンズの結像
-- 物体（矢印）から出た光がレンズで曲がり、像を結ぶ
--   レンズの公式： 1/a + 1/b = 1/f   （a = 物体距離、b = 像距離、f = 焦点距離）   倍率 m = −b/a
--   主光線 3 本：① 軸に平行 → 焦点 F' を通る　② 中心を通る → 直進　③ 焦点 F を通る → 軸に平行
-- 矢印の先をドラッグ = 物体の位置と高さ / ↑↓ = 焦点距離 f / C = 凸 ⇄ 凹 / R = リセット
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

data World = World
  { wF    :: !Float        -- ★ 焦点距離（px）正 = 凸レンズ、負 = 凹レンズ
  , wObjX :: !Float        -- 物体：レンズからの距離（左が負）
  , wObjH :: !Float        -- 物体の高さ
  , wDrag :: !Bool
  }

world0 :: World
world0 = World 150 (-380) 110 False

main :: IO ()
main = runSketch (sketch "36-02 Thin Lens Imaging" world0)
  { skBack  = hsb 225 35 10
  , skDraw  = drawAll
  , skEvent = onEvent
  }

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h0 = envH env
      cx = wd * 0.5
      axis = h0 * 0.55
      f = wF w
      a = negate (wObjX w)
      h = wObjH w
      -- ★ レンズの公式で像の位置を求める（a = f のときは無限遠 → 大きな値に抑える）
      inv = 1 / f - 1 / a
      b | abs inv < 1e-6 = (if inv >= 0 then 1 else -1) * 1e6
        | otherwise = 1 / inv
      m = negate b / a             -- 倍率
      hi = h * m                   -- 像の高さ（負 = 倒立）
      virtual = b < 0
      finite = abs b < 1e5
      -- 主光線 3 本（物体の先端から）
      ox = cx + wObjX w
      oy = axis - h
      fx = cx - f
      den = fx - ox
      dyF = (axis - oy) / (if abs den < 1e-3 then 1e-3 else den)
      rays =
        [ ((cx, oy), (abs f, signum f * (axis - oy)), 45)              -- ① 平行 → F' を通る（凹なら F から出たように広がる）
        , ((cx, axis), (cx - ox, axis - oy), 170)                      -- ② 中心を通る
        , ((cx, oy + dyF * (cx - ox)), (1, 0), 320) ]                  -- ③ F を通ってレンズへ → 平行
      drawRay (hit@(hx, hy), (rx, ry), hue) =
        let len = max 1e-6 (norm2 (rx, ry))
            dx = rx / len
            dy = ry / len
        in Pictures
          [ Color (hsbA hue 70 100 85) (thickLine 2 (ox, oy) hit)                                -- レンズまで
          , Color (hsbA hue 70 100 85) (thickLine 2 hit (hx + dx * 3000, hy + dy * 3000))        -- レンズの後
          , if virtual then Color (hsbA hue 70 100 40) (dashed hit (hx - dx * 3000, hy - dy * 3000)) else Blank ]
      focusMarks =
        [ Pictures
            [ Color white (at (cx + s * f, axis) (disc 3.5))
            , Color (hsb 0 0 60) (at (cx + s * 2 * f, axis) (disc 2.5)) ]
        | s <- [-1, 1] ]
      focusLabels =
        [ txt env (cx + s * f - 5) (axis + 22) 14 white (if s * signum f < 0 then "F" else "F'")
        | s <- [-1, 1] ]
      ix = cx + b
      px = wd - 520
      py = 120
      bs = if finite then nf 0 b else "inf"
      kind | not finite = "Image at infinity (parallel light)"
           | otherwise = (if virtual then "Virtual" else "Real") ++ ", "
                      ++ (if hi < 0 then "inverted" else "upright") ++ ", "
                      ++ (if abs m > 1 then "enlarged" else "reduced")
  in Pictures
    [ onP5 env (Pictures
        ( [ Color (hsbA 0 0 100 40) (Line [(0, axis), (wd, axis)])
          , drawLens cx axis (f > 0) ]
          ++ focusMarks ++ map drawRay rays ++
          -- 物体と像
          [ drawArrow ox axis oy 45 100
          , if finite then drawArrow ix axis (axis - hi) (if virtual then 320 else 170) (if virtual then 55 else 100) else Blank
          -- スクリーン（実像ならここに映る）
          , if not virtual && finite then Color (hsbA 0 0 100 30) (thickLine 6 (ix, axis - 260) (ix, axis + 260)) else Blank
          , Color (hsbA 0 0 100 8) (box (px - 16) (py - 30) 510 175)
          ]))
    , Pictures focusLabels
    , txt env 20 40 18 white
        ((if f > 0 then "Convex lens" else "Concave lens") ++ "  f = " ++ nf 0 f ++ " px (Up/Down)  C: convex/concave  R: reset")
    , txt env 20 68 15 (hsb 0 0 60)
        "Drag the yellow arrow. Yellow = parallel ray, cyan = center ray, pink = focal ray. Dashes: apparent origin."
    , txt env px py 16 white "1/a + 1/b = 1/f"
    , txt env px (py + 30) 16 (hsb 45 90 100)
        ("a = " ++ nf 0 a ++ "  b = " ++ bs ++ "  ->  1/" ++ nf 0 a ++ " + 1/" ++ bs ++ " = 1/" ++ nf 0 f)
    , txt env px (py + 56) 16 (hsb 45 90 100) ("Magnification m = -b/a = " ++ nf 2 m)
    , txt env px (py + 88) 16 white kind
    , txt env px (py + 116) 14 (hsb 0 0 70)
        (if virtual then "Look through the lens: seen on the object's side (magnifier)"
                    else "A screen here shows the image (camera, projector)")
    ]

-- レンズ（凹レンズはへこんだ多角形なので、2 つの台形に分けて塗る）
drawLens :: Float -> Float -> Bool -> Picture
drawLens cx cy convex =
  Pictures
    [ Color (hsbA 190 40 100 25) shape
    , Color (hsbA 190 40 100 80) (thickLine 2 (cx, cy - 270) (cx, cy + 270)) ]
  where
    shape
      | convex = Polygon [ (cx + 18 * cos a, cy + 270 * sin a)
                         | k <- [0 .. 47 :: Int], let a = tau * fromIntegral k / 48 ]
      | otherwise = Pictures
          [ Polygon [(cx - 18, cy - 270), (cx + 18, cy - 270), (cx + 5, cy), (cx - 5, cy)]
          , Polygon [(cx - 5, cy), (cx + 5, cy), (cx + 18, cy + 270), (cx - 18, cy + 270)] ]

drawArrow :: Float -> Float -> Float -> Float -> Float -> Picture
drawArrow x y0 y1 hue bri =
  let s = if y1 < y0 then -1 else 1
  in Color (hsb hue 70 bri) (Pictures
       [ thickLine 4 (x, y0) (x, y1)
       , Polygon [(x, y1), (x - 9, y1 - s * 16), (x + 9, y1 - s * 16)] ])

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    w { wDrag = dist2 (mx, my) (cx + wObjX w, axis - wObjH w) < 40 || mx < cx - 20 }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = False }
  EventMotion _ | wDrag w && envDown env ->
    let hh = clampF (-260) 260 (axis - my)
    in w { wObjX = min (-15) (mx - cx), wObjH = if abs hh < 10 then 10 else hh }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wF = wF w + (if wF w > 0 then 10 else -10) }
  EventKey (SpecialKey KeyDown) Down _ _
    | abs (wF w) > 40 -> w { wF = wF w - (if wF w > 0 then 10 else -10) }
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> w { wF = negate (wF w) }
    | c `elem` "rR" -> w { wF = 150, wObjX = -380, wObjH = 110 }
  _ -> w
  where
    (mx, my) = mouseP5 env
    cx = envW env * 0.5
    axis = envH env * 0.55

-- ---------- やってみよう ----------
-- 1. 物体を遠くから F に近づける：像はどう動く？ a = f でちょうど無限遠、さらに近づくと虚像になる
-- 2. a = 2f のとき b = ? 倍率は？（等倍の位置 → コピー機の原理）
-- 3. 凹レンズ（C）はいつも「正立・縮小・虚像」。物体をどこに置いても確かめよう
-- 4. 主光線 3 本はどれか 2 本あれば像の位置が決まる。rays から 1 本消しても同じ場所で交わるか確かめよう


-- ==================================================
-- ここから下は「道具」（座標の変換・色・数字・破線）
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

-- 点線
dashed :: Point -> Point -> Picture
dashed (x1, y1) (x2, y2) =
  let n = max 1 (floor (dist2 (x1, y1) (x2, y2) / 14)) :: Int
      f i = fromIntegral i / fromIntegral n
      pt t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
  in Pictures [ Line [pt (f i), pt (f (i + 1))] | i <- [0, 2 .. n - 1] ]
