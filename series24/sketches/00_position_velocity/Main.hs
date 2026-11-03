-- ============================================
-- 00 位置・速度・加速度
-- ボールの位置 x(t) を記録して、速度 v = dx/dt（グラフの傾き）、加速度 a = dv/dt（v の傾き）を計算する
--   左のレールのボールをドラッグして動かしてみよう。右のグラフの黄色い線が「いまの傾き」
-- ドラッグ = ボールを動かす / 1 = 自分で動かす / 2 = 単振動 / 3 = 坂道と壁 / スペース = 一時停止
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Text.Printf (printf)

smoothV, smoothA, amp, omega, slope :: Float
smoothV = 0.35   -- ★ 速度のなめらかさ（0〜1、小さいほどノイズが減るが遅れる）
smoothA = 0.2    -- ★ 加速度のなめらかさ
amp = 0.8        -- ★ 単振動の振幅 [m]
omega = 2.5      -- ★ 単振動の角振動数 [rad/s]
slope = 3.0      -- ★ 坂道モードの加速度 [m/s²]

histLen :: Int
histLen = 600

dt :: Float
dt = 1 / 60

data World = World
  { wMode :: !Int          -- 1: ドラッグ, 2: 単振動, 3: 坂道（一定の加速度）
  , wX, wV, wA :: !Float   -- 位置 [m]、速度 [m/s]、加速度 [m/s²]
  , wXPrev, wVPrev, wVSim :: !Float
  , wT :: !Float
  , wPaused, wDragging :: !Bool
  , hX, hV, hA, hVt, hAt :: !(Seq.Seq Float)  -- グラフの記録（hVt, hAt の NaN は「式の値がない」）
  }

main :: IO ()
main = runSketch (sketch "24-00 Position, Velocity, Acceleration" world0)
  { skBack  = hsb 225 30 11
  , skStep  = \env _ w -> pure (if wPaused w then w else stepWorld (fst (toP5 (envMouse env))) w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = World 1 0 0 0 0 0 0 0 False False Seq.empty Seq.empty Seq.empty Seq.empty Seq.empty

stepWorld :: Float -> World -> World
stepWorld mx w0 =
  let t = wT w0 + dt
      w = w0 { wT = t }
      nan = 0 / 0 :: Float
      (x1, vs1, vTrue, aTrue) = case wMode w of
        1 -> -- マウスで動かす：位置だけを決め、速度と加速度は差分で求める
          let target = clampF (-1) 1 (remap mx (width * 0.06) (width * 0.44) (-1) 1)
          in (if wDragging w then wX w + (target - wX w) * 0.6 else wX w, wVSim w, nan, nan)
        2 -> ( amp * sin (omega * t)                     -- 位置の式
             , wVSim w
             , amp * omega * cos (omega * t)             -- 微分すると速度
             , -amp * omega * omega * sin (omega * t) )  -- もう一度微分すると加速度
        _ -> -- 坂道：左へ一定の加速度、左の壁で跳ね返る
          let vs0 = wVSim w - slope * dt
              x0 = wX w + vs0 * dt
              (x', vs') | x0 < -1 = (-1, abs vs0 * 0.95)
                        | x0 > 1 = (1, -abs vs0)
                        | otherwise = (x0, vs0)
          in (x', vs', vs', -slope)
      -- ★ 微分 = 「少しの時間でどれだけ変わったか」
      vRaw = (x1 - wXPrev w) / dt
      v1 = wV w + (vRaw - wV w) * smoothV
      aRaw = (v1 - wVPrev w) / dt
      a1 = wA w + (aRaw - wA w) * smoothA
      v2 = clampF (-50) 50 v1
      a2 = clampF (-500) 500 a1
  in w { wX = x1, wV = v2, wA = a2, wVSim = vs1, wXPrev = x1, wVPrev = v2
       , hX = pushHist (hX w) x1, hV = pushHist (hV w) v2, hA = pushHist (hA w) a2
       , hVt = pushHist (hVt w) vTrue, hAt = pushHist (hAt w) aTrue }

railX :: Float -> Float
railX xm = remap xm (-1) 1 (width * 0.06) (width * 0.44)

drawAll :: World -> Picture
drawAll w = Pictures
  [ view ( Color (hsb 0 0 40) (thickLine 4 (railX (-1), ry + 26) (railX 1, ry + 26))
         : [Color (hsb 0 0 60) (boxAt (railX m - 1) (ry + 26) 2 10) | m <- ticks]
        ++ [Color (hsb 0 0 30) (boxAt (railX (-1) - 14) (ry - 40) 10 70) | wMode w == 3]
        ++ [Color (hsb 30 80 100) (at (bx, ry) (disc 22))] )
  , Pictures [txtC (railX m) (ry + 52) 14 (hsb 0 0 60) (nf 1 m ++ " m") | m <- ticks]
    -- 速度（緑）と加速度（赤）の矢印
  , arrowTo bx (ry - 40) (bx + wV w * 40) (ry - 40) (hsb 130 80 100) "v"
  , arrowTo bx (ry + 80) (bx + wA w * 4) (ry + 80) (hsb 0 80 100) "a"
    -- グラフ 3 段（右）
  , drawPlot gx (height * 0.12) gw gh [Series (hX w) (hsb 30 70 100) (hsb 30 70 100) "Position x [m]"] (-1.2) 1.2 "x(t)"
  , drawPlot gx (height * 0.38) gw gh [Series (hVt w) (hsb 0 0 55) (hsb 0 0 70) "Formula", Series (hV w) (hsb 130 70 100) (hsb 130 70 100) "Velocity v = dx/dt"] (-5) 5 "v(t)"
  , drawPlot gx (height * 0.64) gw gh [Series (hAt w) (hsb 0 0 55) (hsb 0 0 70) "Formula", Series (hA w) (hsb 0 70 100) (hsb 0 70 100) "Accel a = dv/dt"] (-40) 40 "a(t)"
    -- ★ x(t) の右端に「接線」を引く：傾き = いまの速度
  , tangent gx (height * 0.12) gw gh (wX w) (wV w) (-1.2) 1.2
  , tangent gx (height * 0.38) gw gh (wV w) (wA w) (-5) 5
  , txt 20 36 18 (gray 1) ("Mode " ++ show (wMode w) ++ ": " ++ modeName ++ "  (1-3)   Space: " ++ (if wPaused w then "resume" else "pause"))
  , txt 20 66 15 (hsb 0 0 65) ("x = " ++ nf 2 (wX w) ++ " m   v = " ++ nf 2 (wV w) ++ " m/s   a = " ++ nf 1 (wA w) ++ " m/s^2   Yellow line = slope. Slope of x is v, slope of v is a")
  , txt 20 (height - 30) 15 (hsb 0 0 65) "Move fast: v is large. Turn sharply: a is large. Can you move at constant speed so that a is near 0?"
  ]
  where
    ry = height * 0.5
    ticks = [-1, -0.5, 0, 0.5, 1]
    bx = railX (wX w)
    gx = width * 0.5
    gw = width * 0.46
    gh = height * 0.22
    modeName = case wMode w of
      1 -> "drag it yourself"
      2 -> "simple harmonic x = A sin wt"
      _ -> "slope, constant a = -" ++ nf 1 slope ++ " m/s^2"

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos
    | fst (toP5 pos) < width * 0.48 -> w { wDragging = True, wMode = 1 }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDragging = False }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char '1') Down _ _ -> w { wMode = 1, wT = 0 }
  EventKey (Char '2') Down _ _ -> w { wMode = 2, wT = 0 }
  EventKey (Char '3') Down _ _ -> w { wMode = 3, wT = 0, wX = 0.9, wXPrev = 0.9, wVSim = 0 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. モード 1 で「等速」で動かしてみる。v のグラフが平らになり a ≈ 0 になる？ 人の手は意外と加速している
-- 2. モード 2 で x, v, a の山の位置を比べる。v は x より 1/4 周期早い、a は x と逆向き（a = −ω²x）
-- 3. smoothV を 1 にする（なめらかにしない）。差分はノイズを大きくする → 2 回微分の a はもっとガタガタ
-- 4. モード 3 の v(t) は直線、x(t) は放物線。slope を変えて傾きの変化を見る


-- ==================================================
-- 道具（グラフと矢印を描く）
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- マウスの位置（gloss の座標）を p5 と同じ座標（左上が原点、y は下向き）に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は左下の位置（p5 の座標）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtC, txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y s = \c str -> txt (x - fromIntegral (length str) * s * 0.36) y s c str
txtR x y s = \c str -> txt (x - fromIntegral (length str) * s * 0.72) y s c str

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y w h = rect (x + w / 2) (y + h / 2) w h

-- p5 の nf(v, 1, d)：小数 d 桁の文字列
nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

pushHist :: Seq.Seq Float -> Float -> Seq.Seq Float
pushHist s !v =
  let s' = s Seq.|> v
  in if Seq.length s' > histLen then Seq.drop 1 s' else s'

arrowTo :: Float -> Float -> Float -> Float -> Color -> String -> Picture
arrowTo x1 y1 x2 y2 c name = Pictures
  [ view (Color c (thickLine 3 (x1, y1) (x2, y2))
          : [Color c (Polygon [(x2, y2), side (-0.4), side 0.4]) | abs (x2 - x1) + abs (y2 - y1) > 6])
  , txt (x1 - 14) (y1 + 5) 14 c name
  ]
  where
    ang = atan2 (y2 - y1) (x2 - x1)
    side d = (x2 - 10 * cos (ang + d), y2 - 10 * sin (ang + d))

-- グラフの右端に、傾き slopeVal の接線を描く（横軸は 1 目盛り = 1 フレーム）
tangent :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Picture
tangent x0 y0 w h val slopeVal lo hi =
  let pxPerSec = w / (fromIntegral histLen * dt)
      pxPerUnit = h / (hi - lo)
      px = x0 + w
      py = remap (clampF lo hi val) lo hi (y0 + h) y0
      ddx = pxPerSec
      ddy = -slopeVal * pxPerUnit
      c = hsb 55 90 100
  in view [Color c (thickLine 2 (px - ddx, py - ddy) (px, py)), Color c (at (px, py) (disc 4))]

data Series = Series
  { sData :: !(Seq.Seq Float)
  , sLine, sText :: !Color
  , sName :: String
  }

-- 時間プロット：枠 (x, y, w, h) に series を描く（縦の範囲 lo〜hi）。NaN のところは線を切る
drawPlot :: Float -> Float -> Float -> Float -> [Series] -> Float -> Float -> String -> Picture
drawPlot x0 y0 w h series lo hi title = Pictures
  ( view ( Color (hsb 0 0 14) (boxAt x0 y0 w h)
         : [Color (hsb 0 0 30) (Line [(x0, yz), (x0 + w, yz)]) | yz > y0 && yz < y0 + h]
        ++ map curve series )
  : txt (x0 + 8) (y0 + 20) 14 (hsb 0 0 70) title
  : legend (x0 + w - 8) series )
  where
    yz = remap 0 lo hi (y0 + h) y0
    curve s =
      let d = toList (sData s)
          off = histLen - length d
          pts = [ if isNaN v then Nothing
                  else Just ( remap (fromIntegral (i + off)) 0 (fromIntegral histLen - 1) x0 (x0 + w)
                            , remap (clampF lo hi v) lo hi (y0 + h) y0 )
                | (i, v) <- zip [0 :: Int ..] d, even i ]   -- 1 つおきに間引いて描く
      in Color (sLine s) (Pictures (map (thickPath 2) (segments pts)))
    legend _ [] = []
    legend lx (s : rest) = txtR lx (y0 + 20) 14 (sText s) (sName s)
                         : legend (lx - fromIntegral (length (sName s)) * 14 * 0.72 - 14) rest

-- Nothing で区切って、線のかたまりに分ける
segments :: [Maybe Point] -> [[Point]]
segments ps = case dropWhile isNone ps of
  [] -> []
  rest -> let (a, b) = span (not . isNone) rest
          in [p | Just p <- a] : segments b
  where isNone m = case m of { Nothing -> True; _ -> False }
