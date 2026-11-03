-- ============================================
-- 02 折り目のカタストロフィー（fold）
-- いちばん単純なカタストロフィー：V(x) = x³ + a·x
--   a < 0 のとき谷（x = +√(−a/3)）と山（x = −√(−a/3)）がある。a = 0 で 2 つが合体して消える（分岐集合は a = 0 の 1 点）
--   谷がなくなるとボールは坂を転がり落ち、a を戻しても帰ってこない。谷が浅くなるほど、揺らしたあと戻るのが遅くなる（臨界減速）
-- マウス左右（または ←→）= a / P = ボールを押す / A = a をゆっくり 0 へ近づける / R = ボールを谷へ戻す
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

damping, kick :: Float
damping = 2.2              -- ★ 摩擦
kick = 1.2                 -- ★ P で押す強さ

data World = World
  { wA        :: !Float
  , wX        :: !Float
  , wV        :: !Float
  , wFallen   :: !Bool
  , wRamp     :: !Bool
  , wRecover  :: ![(Float, Int)]   -- 押してから戻るまでの時間 (a, フレーム数)。古い順、12 個まで
  , wKick     :: !Int              -- 押したフレーム（-1 = なし）
  , wKickA    :: !Float
  }

world0 :: World
world0 = World (-1.5) 0.7 0 False False [] (-1) 0

main :: IO ()
main = runSketch (sketch "35-02 Fold Catastrophe" world0)
  { skBack  = hsb 20 25 12
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

vOf, dV :: Float -> Float -> Float
vOf a x = x * x * x + a * x
dV a x = 3 * x * x + a

stepWorld :: Env -> World -> World
stepWorld env w0 =
  let a = if wRamp w0 then min 0.4 (wA w0 + 0.0015) else wA w0
      (x1, v1) = go (4 :: Int) (wX w0) (wV w0)
      go 0 x v = (x, v)
      go k x v =
        let dt = 1 / 120
            v' = v + (-6 * dV a x - damping * v) * dt
        in go (k - 1) (x + v' * dt) v'
      fell = x1 < -2.5
      x2 = min 2.5 (if fell then -2.5 else x1)
      v2 = if fell then 0 else v1
      w1 = w0 { wA = a, wX = x2, wV = v2, wFallen = wFallen w0 || fell }
      back = wKick w0 >= 0 && a < 0 && abs (x2 - sqrt (negate a / 3)) < 0.02 && abs v2 < 0.02
  in if back
       then w1 { wRecover = lastN 12 (wRecover w0 ++ [(wKickA w0, envFrame env - wKick w0)]), wKick = -1 }
       else w1

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      a = wA w
      x = wX w
      xm = sqrt (negate a / 3)
      lx = 40
      lw = wd * 0.55
      py = 110
      ph = h - py - 40
      panel = hsb 20 20 18
      toX xx = lx + (xx + 2.5) / 5 * lw
      toY vv = py + ph * 0.5 - vv * ph * 0.08
      curve = [ (toX xx, clampF py (py + ph) (toY (vOf a xx))) | i <- [0 .. 200 :: Int], let xx = -2.5 + 5 * fromIntegral i / 200 ]
      -- 右上：平衡点の図（a = −3x²）
      rx = lx + lw + 30
      rw = wd - rx - 40
      rh = ph * 0.5 - 15
      toA aa = rx + (aa + 3) / 3.6 * rw
      toXr xx = py + rh / 2 - xx / 1.1 * (rh / 2 - 10)
      branch s = [ (toA (-3 * xx * xx), toXr xx) | i <- [0 .. 50 :: Int], let xx = s * fromIntegral i * 0.02 ]
      -- 右下：押してから戻るまでの時間
      gy = py + rh + 30
      gh = ph - rh - 30
      cell = (rw - 32) / 12
      bars = [ Color (hsb 200 60 90) (box bx (gy + gh - 42 - bh) (cell - 6) bh)
             | (i, (_, f)) <- zip [0 :: Int ..] (wRecover w)
             , let bh = min (gh - 80) (fromIntegral f * 0.6)
                   bx = rx + 16 + fromIntegral i * cell ]
      barText = [ txt env (rx + 16 + fromIntegral i * cell) (gy + gh - (if even i then 26 else 8)) 14 (gray 0.8) (nf 2 ra)
                | (i, (ra, _)) <- zip [0 :: Int ..] (wRecover w) ]
  in Pictures
    [ onP5 env (Pictures
        [ Color panel (box lx py lw ph)
        , Color (hsb 30 50 45) (underCurve (py + ph) curve)
        , if a < 0
            then Pictures [ Color (hsb 150 60 100) (at (toX xm, toY (vOf a xm) + 14) (disc 6))
                          , Color (hsb 0 70 100) (at (toX (negate xm), toY (vOf a (negate xm)) - 14) (disc 6)) ]
            else Blank
        , Color (hsb 45 90 100) (at (toX x, clampF (py + 14) (py + ph - 14) (toY (vOf a x) - 14)) (disc 13))
        , Color panel (box rx py rw rh)
        , Color (hsbA 0 0 100 20) (Pictures [Line [(toA 0, py), (toA 0, py + rh)], Line [(rx, toXr 0), (rx + rw, toXr 0)]])
        , Color (hsb 150 60 100) (thickPath 4 (branch 1))
        , Color (hsb 0 70 100) (thickPath 2 (branch (-1)))
        , Color (hsb 45 90 100) (at (toA 0, toXr 0) (disc 7))
        , Color (hsb 200 70 100) (thickLine 2 (toA a, py) (toA a, py + rh))
        , if abs x < 1.1 then Color (hsb 45 90 100) (at (toA a, toXr x) (disc 6)) else Blank
        , Color panel (box rx gy rw gh)
        , Pictures bars
        ])
    , if wFallen w then txt env (lx + 30) (py + 50) 30 (hsb 0 80 100) "Fell! Make a < 0, then R" else Blank
    , txt env (rx + 12) (py + 22) 14 (gray 0.8) "Green valley, red hill, yellow: a = 0"
    , txt env (rx + 12) (gy + 22) 14 (gray 0.8) "P: frames to get back to the valley"
    , txt env (rx + 12) (gy + 42) 14 (gray 0.8) "(slower as a nears 0)"
    , Pictures barText
    , txt env 20 36 16 white ("a = " ++ nf 3 a ++ " (mouse / <- ->)   P: kick   A: ramp to 0 " ++ onOff (wRamp w)
                              ++ "   R: back   V'' = " ++ (if a < 0 then nf 2 (6 * xm) else "no valley"))
    , txt env 20 62 14 (gray 0.6) "As a nears 0 the valley gets shallow and flat, and for a > 0 it is gone. The slower return"
    , txt env 20 84 14 (gray 0.6) "warns of the sudden change before it happens (see sketch 12)."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventMotion _ | not (envDown env) && my > 90 ->
    w { wA = clampF (-3) 0.6 (remap mx 0 (envW env) (-3) 0.6), wRamp = False }
  EventKey (SpecialKey KeyLeft) Down _ _  -> w { wA = max (-3) (wA w - 0.05) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wA = min 0.6 (wA w + 0.05) }
  EventKey (Char c) Down _ _
    | c `elem` "pP" -> w { wV = wV w - kick, wKick = envFrame env, wKickA = wA w }
    | c `elem` "aA" -> let r = not (wRamp w) in w { wRamp = r, wA = if r then min (wA w) (-1.5) else wA w }
    | c `elem` "rR" ->
        let a = if wA w >= 0 then -1 else wA w
        in w { wA = a, wX = sqrt (negate a / 3), wV = 0, wFallen = False, wKick = -1 }
  _ -> w
  where (mx, my) = mouseP5 env

-- ---------- やってみよう ----------
-- 1. a = −2, −1, −0.5, −0.2 で P を押し、戻るまでの時間を比べよう。グラフはどんな形になる？
-- 2. kick を大きくすると、谷があっても山を越えて落ちる。何 a で、どのくらい押すと落ちる？
-- 3. 谷の深さ（山と谷の高さの差）を式で出してみよう：ΔV = 4(−a/3)^(3/2)
-- 4. ボールに小さなランダムな揺れを毎フレーム加えると（skStep で randomRIO (-0.05, 0.05)）、a が 0 に近いとき揺れはどうなる？


-- ==================================================
-- ここから下は「道具」（座標の変換・塗りつぶし・色・数字）
-- ==================================================
-- 曲線の下を塗る。gloss はへこんだ多角形を塗れないので、細い台形に分ける
underCurve :: Float -> [Point] -> Picture
underCurve bottom ps = Pictures (zipWith quad ps (drop 1 ps))
  where quad (x0, y0) (x1, y1) = Polygon [(x0, bottom), (x1, bottom), (x1, y1), (x0, y0)]

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

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
