-- ============================================
-- 03 カスプのカタストロフィー（cusp）
-- V(x) = x⁴ + a·x² + b·x。平衡点 V'(x) = 4x³ + 2a·x + b = 0 を (a, b, x) の空間に描くと「折りたたまれた布」になる
--   状態（黄の玉）はいまいる布の上をすべり、布の折り目（へり）まで来ると下の布へ「落ちる」
--   折り目を下の平面 (a, b) に映すと、とがった曲線（カスプ）8a³ + 27b² = 0。この内側では谷が 2 つある
-- 左下の四角をドラッグ = コントロール点 (a, b) を動かす / それ以外をドラッグ = 視点 / ←→ = b / ↑↓ = a / A = 自動で回る / R = 視点を戻す
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Numeric (showFFloat)

sx, sz, sy, planeY, ins :: Float
sx = 95            -- ★ 見た目の拡大率（b → 横）
sz = 75            -- ★ a → 奥
sy = 120           -- ★ x → 上
planeY = 240       -- ★ 下の平面の位置
ins = 260          -- 左下の四角の大きさ

data Pt3 = Pt3 !Float !Float !Float      -- (a, b, x)

data World = World
  { wA     :: !Float        -- コントロール（下の平面の点）
  , wB     :: !Float
  , wX     :: !Float        -- 状態（布の上の高さ）
  , wAuto  :: !Bool
  , wT     :: !Float
  , wTrail :: !(Seq Pt3)
  , wFlash :: !Float
  , wJumps :: !Int
  , wRotX  :: !Float
  , wRotY  :: !Float
  , wDrag  :: !Bool         -- 視点をドラッグ中か
  , wLast  :: !Point        -- 1 つ前のマウス位置（movedX, movedY の代わり）
  }

world0 :: World
world0 = World (-2) (-2) (-1) True 0 Seq.empty 0 0 (-0.45) (-0.6) False (0, 0)

main :: IO ()
main = runSketch (sketch "35-03 Cusp Surface" world0)
  { skBack  = rgb255 12 16 32
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

stepWorld :: World -> World
stepWorld w =
  let t = if wAuto w then wT w + 0.006 else wT w
      a = if wAuto w then -1.3 + 1.1 * sin (t * 0.7) else wA w
      b = if wAuto w then 2.6 * cos t else wB w
      -- 状態は坂を下る：dx/dt = −V'(x)（速いので小さな刻みで）
      descend :: Int -> Float -> Float
      descend 0 v = v
      descend k v = descend (k - 1) (v - 0.004 * (4 * v * v * v + 2 * a * v + b))
      x = clampF (-1.8) 1.8 (descend 20 (wX w))
      jump = abs (x - wX w) > 0.05
      counted = jump && wFlash w < 20
      flash0 = if jump then 40 else wFlash w
  in w { wT = t, wA = a, wB = b, wX = x
       , wJumps = if counted then wJumps w + 1 else wJumps w
       , wFlash = max 0 (flash0 - 1)
       , wTrail = let s = wTrail w |> Pt3 a b x in if Seq.length s > 300 then Seq.drop 1 s else s }

---------------------------------------------------------------- 布（平衡点の面）

data Face = Face ![V3] !V3 !Float !Float !Float   -- 頂点（3〜5 個）、法線、色

-- ★ 布：a と x を決めると b = −4x³ − 2a·x が決まる。(a, x) の格子から三角形をつくり、b が範囲の外の部分は切り落とす
faces :: [Face]
faces =
  [ f
  | i <- [0 .. na - 1], j <- [0 .. nx - 1]
  , tri <- [[(i, j), (i + 1, j), (i + 1, j + 1)], [(i, j), (i + 1, j + 1), (i, j + 1)]]
  , f <- face (map vert tri) ]
  where
    na = 26 :: Int     -- ★ 格子の細かさ（細かいほど重い）
    nx = 72 :: Int
    vert (i, j) =
      let aa = -3 + 4.5 * fromIntegral i / fromIntegral na :: Float
          xx = -1.6 + 3.2 * fromIntegral j / fromIntegral nx :: Float
      in (aa, xx, -4 * xx * xx * xx - 2 * aa * xx)
    pos (aa, xx, bb) = V3 (bb * sx) (negate xx * sy) (aa * sz)
    face tri = case (map pos tri, clipB 3 (clipB (-3) tri)) of
      ([p, q, r], cut@(_ : _ : _ : _)) ->
        let n = unit3 (cross3 (sub3 q p) (sub3 r p))
            m = fromIntegral (length cut)
            aM = sum [ aa | (aa, _, _) <- cut ] / m
            xM = sum [ xx | (_, xx, _) <- cut ] / m
            (cr, cg, cb) = sheetColor aM xM
        in [Face (map pos cut) n cr cg cb]
      _ -> []

-- 多角形を b = lim の線で切り、原点の側だけ残す（lim は 3 か −3）
clipB :: Float -> [(Float, Float, Float)] -> [(Float, Float, Float)]
clipB lim poly = concat (zipWith cut poly (drop 1 poly ++ take 1 poly))
  where
    inside (_, _, bb) = bb / lim <= 1
    cut p@(a0, x0, b0) q@(a1, x1, b1) =
      let s = (lim - b0) / (b1 - b0)
      in [p | inside p] ++ [(lerpF a0 a1 s, lerpF x0 x1 s, lim) | inside p /= inside q]

-- 布の色：真ん中（V'' < 0）は暗い赤、上の谷はオレンジ、下の谷は水色
sheetColor :: Float -> Float -> (Float, Float, Float)
sheetColor aa xx
  | 12 * xx * xx + 2 * aa < 0 = (130 / 255, 40 / 255, 60 / 255)
  | xx > 0 = (1, (150 + xx * 40) / 255, 70 / 255)
  | otherwise = (90 / 255, (170 - xx * 30) / 255, 1)

---------------------------------------------------------------- 描く

drawAll :: Env -> World -> Picture
drawAll env w =
  let ez = envH env / 2 / tan (pi / 6)
      (cxr, sxr) = (cos (wRotX w), sin (wRotX w))
      (cyr, syr) = (cos (wRotY w), sin (wRotY w))
      -- p5.js の rotateX(rotX); rotateY(rotY) と同じ順（y は下向きのまま計算し、最後に上下を返す）
      view (V3 x y z) =
        let x1 = x * cyr + z * syr
            z1 = z * cyr - x * syr
        in V3 x1 (y * cxr - z1 * sxr) (y * sxr + z1 * cxr)
      pr v = let (px, py) = project ez v in (px, negate py)
      depth (V3 _ _ z) = z
      kOf (V3 _ _ z) = ez / max 1 (ez - z)
      -- ★ 光：環境光 + 平行光（裏からも見えるように両面を照らす）
      lightDir = unit3 (V3 0.3 0.8 (-0.5))
      faceItem (Face vs n cr cg cb) =
        let vs' = map view vs
            lam = 90 / 255 + 0.8 * abs (dot3 (view n) lightDir)
            c = makeColor (cr * min 1 lam) (cg * min 1 lam) (cb * min 1 lam) 1
        in (sum (map depth vs') / fromIntegral (length vs'), Color c (Polygon (map pr vs')))
      segItem wid c a0 b0 =
        let a' = view a0; b' = view b0
        in ((depth a' + depth b') / 2 + 3, Color c (thickLine wid (pr a') (pr b')))
      ballItem r c v =
        let v' = view v in (depth v' + 3, at (pr v') (Color c (disc (r * kOf v'))))
      p3 aa bb xx = V3 (bb * sx) (negate xx * sy) (aa * sz)
      ss = [ -0.7 + 0.02 * fromIntegral i | i <- [0 .. 70 :: Int] ] :: [Float]
      foldPts = [ V3 (8 * s * s * s * sx) (negate s * sy) (-6 * s * s * sz) | s <- ss ]
      shadowPts = [ V3 (8 * s * s * s * sx) (planeY - 1) (-6 * s * s * sz) | s <- ss ]
      path c wid ps = zipWith (segItem wid c) ps (drop 1 ps)
      trailPts = [ p3 aa bb xx | Pt3 aa bb xx <- toList (wTrail w) ]
      a = wA w
      b = wB w
      x = wX w
      flash = wFlash w
      statePos = p3 a b x
      shadowPos = V3 (b * sx) (planeY - 2) (a * sz)
      items = map faceItem faces
           ++ path (rgb255 255 120 120) 3 foldPts
           ++ path (rgb255 255 220 90) 3 shadowPts
           ++ path (rgb255 120 230 255) 2 trailPts
           ++ [ let v0 = view statePos; v1 = view (V3 (b * sx) planeY (a * sz))
                in ((depth v0 + depth v1) / 2, Color (rgba255 255 255 255 120) (Line [pr v0, pr v1]))
              , ballItem 6 (rgb255 255 220 90) shadowPos
              , ballItem 11 (rgb255 255 220 90) statePos ]
           ++ [ let v' = view statePos
                in (depth v' + 4, at (pr v') (Color (rgba255 255 240 150 (round (flash * 4))) (disc ((14 + 40 - flash) * kOf v'))))
              | flash > 0 ]
      -- 下の平面 (a, b)。上から見ているときは先に、下から見ているときは後に描く
      planeZ = -0.75 * sz
      corner px pz = pr (view (V3 (px * 3 * sx) planeY (planeZ + pz * 2.25 * sz)))
      plane = Color (rgb255 40 55 90) (Polygon [corner (-1) (-1), corner 1 (-1), corner 1 1, corner (-1) 1])
      above = negate (sin (wRotX w)) > 0
      sorted = map snd (depthSort fst items)
  in Pictures
    ( (if above then [plane] else [])
   ++ sorted
   ++ (if above then [] else [plane])
   ++ [ drawInset env w
      , txt env 20 36 16 white ("(a, b) = (" ++ nf 2 a ++ ", " ++ nf 2 b ++ ")  x = " ++ nf 2 x ++ "  jumps " ++ show (wJumps w)
                                ++ "  A: auto " ++ onOff (wAuto w) ++ "  <- -> b  Up/Dn a  Drag: view  R: view")
      , txt env 20 62 14 (gray 0.67) "Sheet = equilibria (a, b, x). Orange upper valley, blue lower valley, dark red middle hill (unstable)."
      , txt env 20 84 14 (gray 0.67) "Red line = fold, yellow line = its shadow (cusp 8a^3 + 27b^2 = 0)."
      ] )

-- 左下：コントロール平面（横 = b、縦 = a）
drawInset :: Env -> World -> Picture
drawInset env w =
  let oy = envH env - ins - 56
      ox = 40
      toP aa bb = (ox + (bb + 3) / 6 * ins, oy + (1.5 - aa) / 4.5 * ins)
      ss = [ -0.7 + 0.02 * fromIntegral i | i <- [0 .. 70 :: Int] ] :: [Float]
      cusp = [ toP (-6 * s * s) (8 * s * s * s) | s <- ss ]
      -- カスプの内側（谷が 2 つ）：へこんだ形なので、左右の点を結ぶ横長の台形に分けて塗る
      half = [ (-6 * s * s, 8 * s * s * s) | i <- [0 .. 35 :: Int], let s = 0.02 * fromIntegral i ]
      inner = zipWith (\(a0, b0) (a1, b1) -> Polygon [toP a0 (negate b0), toP a0 b0, toP a1 b1, toP a1 (negate b1)]) half (drop 1 half)
      trail = toList (wTrail w)
      dots = [ at (toP aa bb) (disc 1.5) | (i, Pt3 aa bb _) <- zip [0 :: Int ..] trail, i `mod` 3 == 0 ]
      x = wX w
  in Pictures
    [ onP5 env (Pictures
        [ Color (rgba255 0 0 0 170) (box 20 (envH env - ins - 90) (ins + 40) (ins + 70))
        , Color (rgb255 40 50 80) (box ox oy ins ins)
        , Color (rgba255 255 220 90 60) (Pictures inner)
        , Color (rgb255 255 220 90) (thickPath 2 (cusp ++ take 1 cusp))
        , Color (rgba255 120 230 255 150) (Pictures dots)
        , Color (if x > 0 then rgb255 255 160 80 else rgb255 120 200 255) (at (toP (wA w) (wB w)) (disc 7))
        ])
    , txt env ox (oy - 12) 14 white "Control (a, b): drag"
    , txt env ox (oy + ins + 18) 14 (gray 0.8) "across = b"
    , txt env (ox + 110) (oy + ins + 18) 14 (rgb255 255 220 90) "yellow: 2 valleys"
    ]

inInset :: Env -> Bool
inInset env =
  let (mx, my) = mouseP5 env
      h = envH env
  in mx > 40 && mx < 40 + ins && my > h - ins - 56 && my < h - 56

setFromMouse :: Env -> World -> World
setFromMouse env w =
  let (mx, my) = mouseP5 env
  in w { wB = clampF (-3) 3 ((mx - 40) / ins * 6 - 3)
       , wA = clampF (-3) 1.5 (1.5 - (my - (envH env - ins - 56)) / ins * 4.5) }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | inInset env -> (setFromMouse env w) { wAuto = False, wDrag = False, wLast = m }
    | otherwise -> w { wDrag = True, wLast = m }
  EventMotion _
    | envDown env && wDrag w ->
        let (mx0, my0) = wLast w
            (mx, my) = m
        in w { wRotY = wRotY w + (mx - mx0) * 0.01
             , wRotX = clampF (-1.5) 1.5 (wRotX w - (my - my0) * 0.01), wLast = m }
    | envDown env && inInset env -> (setFromMouse env w) { wLast = m }
    | otherwise -> w { wLast = m }
  EventKey (SpecialKey KeyLeft) Down _ _  -> w { wAuto = False, wB = max (-3) (wB w - 0.1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wAuto = False, wB = min 3 (wB w + 0.1) }
  EventKey (SpecialKey KeyUp) Down _ _    -> w { wAuto = False, wA = min 1.5 (wA w + 0.1) }
  EventKey (SpecialKey KeyDown) Down _ _  -> w { wAuto = False, wA = max (-3) (wA w - 0.1) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> w { wRotX = -0.45, wRotY = -0.6 }
  _ -> w
  where m = mouseP5 env

-- ---------- やってみよう ----------
-- 1. a = 0.5（カスプの外）で b を左右に動かすと x はなめらか。a = −2 だと？ 同じ b でも x が 2 つありうる
-- 2. カスプのとがった点 (0, 0) のすぐ上を通る道と、すぐ下を通る道で、玉の動きを比べよう
-- 3. 自動の道（stepWorld の a, b の式）を変えて、カスプの内側をぐるぐる回る道をつくると、とぶ回数は？
-- 4. 布の色を「谷の深さ」で塗り分けてみよう（sheetColor の中を変える）


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
