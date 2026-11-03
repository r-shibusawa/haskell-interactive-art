-- ============================================
-- 00 なめらかな変化と突然の変化
-- 谷が 2 つある地形 V(x) = x⁴ − 2x² + b·x の上をボールが転がる。b（傾き）をゆっくり変えると、
--   ボールのいる谷がだんだん浅くなり、ある瞬間に「消える」→ ボールはとなりの谷へ一気に落ちる
-- 原因（b）はなめらかなのに、結果（x）は突然とぶ。これがカタストロフィー
-- マウス左右（または ←→）= b / A = 自動で往復 / スペース = 一時停止 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Numeric (showFFloat)

damping, gravity :: Float
damping = 2.5              -- ★ 摩擦（大きいほどすぐ止まる）
gravity = 6                -- ★ 転がる力の強さ

data World = World
  { wB        :: !Float        -- 地形の傾き（コントロール）
  , wX        :: !Float        -- ボールの位置（状態）
  , wV        :: !Float        -- ボールの速度
  , wAuto     :: !Bool
  , wPaused   :: !Bool
  , wUseMouse :: !Bool
  , wLogX     :: !(Seq Float)  -- 時間グラフ用
  , wLogB     :: !(Seq Float)
  , wJumps    :: !Int          -- とんだ回数（20 まで）
  , wT        :: !Float
  , wFlash    :: !Float
  , wLastSide :: !Int
  }

world0 :: World
world0 = World (-2.2) 1.2 0 True False False Seq.empty Seq.empty 0 0 0 (-1)

main :: IO ()
main = runSketch (sketch "35-00 Smooth and Sudden" world0)
  { skBack  = hsb 250 30 12
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

vOf, dV :: Float -> Float -> Float
vOf x b = x * x * x * x - 2 * x * x + b * x
dV x b = 4 * x * x * x - 4 * x + b

stepWorld :: World -> World
stepWorld w0
  | wPaused w0 = cool w0
  | otherwise =
      let t = wT w0 + 1
          b = if wAuto w0 && not (wUseMouse w0) then 2.4 * sin (t * 0.006 - pi / 2) else wB w0
          (x, v) = ballSteps (6 :: Int) b (wX w0) (wV w0)      -- ★ 小さな刻みで何回か計算
          side = if x < 0 then -1 else 1
          jumped = side /= wLastSide w0 && abs v > 0.5
          lastSide | jumped = side
                   | abs v < 0.05 = side
                   | otherwise = wLastSide w0
      in cool w0 { wT = t, wB = b, wX = x, wV = v, wLastSide = lastSide
                 , wJumps = if jumped then min 20 (wJumps w0 + 1) else wJumps w0
                 , wFlash = if jumped then 31 else wFlash w0
                 , wLogX = pushN 900 x (wLogX w0), wLogB = pushN 900 b (wLogB w0) }
  where cool w = w { wFlash = max 0 (wFlash w - 1) }

-- ★ ボールの運動：坂を下る力 − 摩擦
ballSteps :: Int -> Float -> Float -> Float -> (Float, Float)
ballSteps 0 _ x v = (x, v)
ballSteps k b x v =
  let dt = 1 / 360
      v1 = v + (negate gravity * dV x b - damping * v) * dt * 6
      x1 = clampF (-2.2) 2.2 (x + v1 * dt * 6)
      v2 = if abs x1 >= 2.2 then 0 else v1
  in ballSteps (k - 1) b x1 v2

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      b = wB w
      x = wX w
      px = 80
      pw = wd - 160
      py = 110
      ph = h * 0.5
      toX xx = px + (xx + 2.2) / 4.4 * pw
      toY vv = py + ph - (vv + 3) / 10 * ph
      clampY = clampF py (py + ph)
      curve = [ (toX xx, clampY (toY (vOf xx b))) | i <- [0 .. 200 :: Int], let xx = -2.2 + 4.4 * fromIntegral i / 200 ]
      marks = [ m | e <- equilibria b, let yy = toY (vOf e b), yy >= py, yy <= py + ph, let m = eqMark (toX e) yy (12 * e * e - 4 > 0) ]
      bx = toX x
      by = toY (vOf x b) - 14
      flash = wFlash w
      mx = px + pw / 2 + b / 3 * pw / 2
      mid = px + pw / 2
      bc = 1.5396
      gy = py + ph + 60
      gh = h - gy - 30
      graph k ys = [ (px + fromIntegral i / 900 * pw, gy + gh / 2 - v * k * gh / 2.2) | (i, v) <- zip [0 :: Int ..] (toList ys) ]
  in Pictures
    [ onP5 env (Pictures
        [ Color (hsb 250 25 18) (box px py pw ph)
        , Color (hsb 140 45 45) (underCurve (py + ph) curve)      -- 地形（細い台形を並べて塗る）
        , Pictures marks
        , if flash > 0 then Color (hsbA 45 90 100 (flash * 2)) (at (bx, by) (disc ((40 + flash * 3) / 2))) else Blank
        , Color (hsb 45 90 100) (at (bx, clampY by) (disc 14))
          -- 傾きのメーター
        , Color (hsbA 0 0 100 40) (thickLine 2 (mid, py + ph + 18) (mid, py + ph + 34))
        , Color (hsb 200 70 100) (thickLine 6 (mid, py + ph + 26) (mx, py + ph + 26))
        , Color (hsb 200 70 100) (at (mx, py + ph + 26) (disc 7))
          -- 谷が消える境目 b = ±1.54
        , Color (hsb 0 70 90) (Pictures [ box (mid + s * bc / 3 * pw / 2 - 1) (py + ph + 14) 3 24 | s <- [-1, 1] ])
          -- 時間のグラフ
        , Color (hsb 250 25 18) (box px gy pw gh)
        , Color (hsbA 0 0 100 20) (Line [(px, gy + gh / 2), (px + pw, gy + gh / 2)])
        , Color (hsbA 200 70 100 70) (thickPath 2 (graph (1 / 3) (wLogB w)))
        , Color (hsb 45 90 100) (thickPath 3 (graph (1 / 2) (wLogX w)))
        ])
    , txt env (px + 10) (gy + 20) 14 (hsb 200 70 100) "blue = b (cause: smooth)"
    , txt env (px + 330) (gy + 20) 14 (hsb 45 90 100) "yellow = x (result: jumps)"
    , txt env 20 36 18 white ("b = " ++ nf 2 b ++ " (mouse / <- ->)   A: auto " ++ onOff (wAuto w)
                              ++ "   Space: " ++ (if wPaused w then "resume" else "pause") ++ "   R: restart   jumps " ++ show (wJumps w))
    , txt env 20 62 14 (gray 0.6) "Green ^ = valley (stable), red v = hill (unstable). Past the red marks (b = +-1.54) a valley and"
    , txt env 20 84 14 (gray 0.6) "a hill collide and vanish, and the ball falls into the next valley."
    ]

-- 谷と山の印
eqMark :: Float -> Float -> Bool -> Picture
eqMark x y stable
  | stable = Color (hsb 140 30 90) (Polygon [(x, y + 6), (x - 8, y + 20), (x + 8, y + 20)])
  | otherwise = Color (hsb 0 70 90) (Polygon [(x, y - 6), (x - 8, y - 20), (x + 8, y - 20)])

-- V'(x) = 0 の解（谷と山）を細かく調べて探す
equilibria :: Float -> [Float]
equilibria b = go (1 :: Int) (dV (-2.2) b)
  where
    go i prev
      | i > 400 = []
      | otherwise =
          let xx = -2.2 + 4.4 * fromIntegral i / 400
              cur = dV xx b
              rest = go (i + 1) cur
          in if prev * cur <= 0 then (xx - 4.4 / 400 * cur / (cur - prev + 1e-12)) : rest else rest

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventMotion _ | not (envDown env) ->
    let (mx, my) = mouseP5 env
    in if my > 90 then w { wUseMouse = True, wB = clampF (-3) 3 (remap mx 80 (envW env - 80) (-3) 3) } else w
  EventKey (SpecialKey KeyLeft) Down _ _  -> w { wUseMouse = True, wB = max (-3) (wB w - 0.1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wUseMouse = True, wB = min 3 (wB w + 0.1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wAuto = not (wAuto w), wUseMouse = False }
    | c `elem` "rR" -> world0 { wAuto = wAuto w, wPaused = wPaused w, wLastSide = wLastSide w, wFlash = wFlash w }
  _ -> w

-- ---------- やってみよう ----------
-- 1. b を −3 から +3 までゆっくり動かし、ボールがとぶ b の値を記録しよう。戻るときは同じ値でとぶ？（04 ヒステリシス）
-- 2. damping を 0.3 にすると、とんだ後にボールが行ったり来たりする。10 にすると？
-- 3. vOf と dV の「−2x²」を「−1x²」にすると谷が浅くなる。とぶ b の値はどう変わる？
-- 4. 地形を x⁴ − 2x² + b·x + 0.5·sin(5x) にして、谷をたくさんつくってみよう（dV も微分して書きかえる）


-- ==================================================
-- ここから下は「道具」（座標の変換・塗りつぶし・記録）
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

-- 曲線の下を塗る。gloss はへこんだ多角形を塗れないので、細い台形に分ける
underCurve :: Float -> [Point] -> Picture
underCurve bottom ps = Pictures (zipWith quad ps (drop 1 ps))
  where quad (x0, y0) (x1, y1) = Polygon [(x0, bottom), (x1, bottom), (x1, y1), (x0, y0)]

-- 透明度を 0〜100 で書く HSB（p5.js と同じ）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 長さ n までの記録に足す
pushN :: Int -> a -> Seq a -> Seq a
pushN n x s = let s' = s |> x in if Seq.length s' > n then Seq.drop 1 s' else s'

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

onOff :: Bool -> String
onOff b = if b then "ON" else "off"
