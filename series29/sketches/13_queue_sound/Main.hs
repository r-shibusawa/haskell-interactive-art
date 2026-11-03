-- ============================================
-- 13 待ち行列を聞く
-- M/M/1 待ち行列を音にする：客の到着 = 「カチッ」、サービス終了 = 「ポーン」（行列が長いほど高い音）
--   後ろでずっと鳴る低い音（ドローン）の高さ = いまの系内人数。ρ → 1 にすると、空いた静けさと渋滞のうなりが交互に来る
-- ←→ = ρ / ↑↓ = テンポ（μ）/ M = ドローンの ON/OFF / スペース = 一時停止
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (PrintfArg, printf)

baseNote :: Int
baseNote = 60        -- ★ いちばん低い音（MIDI ノート番号 60 = ド）

droneEvery :: Float
droneEvery = 0.5     -- ★ ドローンを鳴らし直す間隔（秒）。連続音の代わりに短い音を重ねる

data Ripple = Ripple !Float !Float !Float !Float   -- x, y, 直径, 色相

data World = World
  { wRho, wMu :: !Float     -- ★ 利用率、1 秒あたりのサービス数（テンポ）
  , wDrone, wPaused :: !Bool
  , wT, wNextArr, wNextDep :: !Double
  , wN :: !Int
  , wRec :: !(Seq Int)
  , wRings :: !(Seq Ripple)
  , wDroneT :: !Float       -- 次にドローンを鳴らすまでの時間
  }

main :: IO ()
main = runSketch (sketch "29-13 Listening to a Queue" world0)
  { skBack  = hsb 240 30 10
  , skStart = \_ -> (\a -> world0 { wNextArr = a }) <$> expRand (lamOf world0)
  , skStep  = \_ dt w -> stepWorld dt w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World 0.7 2 True False 0 0 infinity 0 Seq.empty Seq.empty 0

lamOf, muOf :: World -> Double
lamOf w = realToFrac (wRho w * wMu w)
muOf = realToFrac . wMu

qx, qy :: Float
qx = width * 0.3
qy = height * 0.35

maxDots :: Int
maxDots = 27    -- 1280 の画面に収まる点の数

pushRing :: Ripple -> World -> World
pushRing r w =
  let s = wRings w |> r
  in w { wRings = if Seq.length s > 120 then Seq.drop (Seq.length s - 120) s else s }

advance :: Double -> World -> IO World
advance tEnd w
  | min (wNextArr w) (wNextDep w) > tEnd = pure w { wT = tEnd }
  | wNextArr w <= wNextDep w = do                   -- 到着
      let t = wNextArr w
          n = wN w + 1
      a <- expRand (lamOf w)
      s <- expRand (muOf w)
      playTone 3000 0.15 0.02 Square                -- ★ 到着の音（短く高い音で「カチッ」）
      advance tEnd (clampN (pushRing (Ripple (qx - fromIntegral (min n maxDots) * 14) qy 6 30)
        w { wT = t, wN = n, wNextArr = t + a, wNextDep = if n == 1 then t + s else wNextDep w }))
  | otherwise = do                                  -- サービス終了
      let t = wNextDep w
          n = wN w - 1
          k = min n 14
      s <- expRand (muOf w)
      playTone (scaleNote baseNote k) 0.18 0.35 Triangle   -- ★ 行列の長さ → 音の高さ
      advance tEnd (clampN (pushRing (Ripple (qx + 40) qy 10 (140 + fromIntegral k * 10))
        w { wT = t, wN = n, wNextDep = if n > 0 then t + s else infinity }))
  where clampN w1 = if wN w1 > 200 then w1 { wN = 200 } else w1

stepWorld :: Float -> World -> IO World
stepWorld dt w0 = do
  w1 <- if wPaused w0 then pure w0 else advance (wT w0 + 1 / 60) w0
  let r = wRec w1 |> wN w1
      grow (Ripple x y d h) = Ripple x y (d + 3) h
      alive (Ripple _ _ d _) = d < 100
      dt1 = wDroneT w1 - dt
      n = wN w1
      fire = dt1 <= 0
  -- ドローンの高さと大きさ：人数が 12 人増えると 1 オクターブ上がる
  if fire && wDrone w1
    then playTone (55 * 2 ** (fromIntegral (min n 30) / 12)) (min 0.12 (0.02 + fromIntegral n * 0.006)) (droneEvery + 0.1) Sawtooth
    else pure ()
  pure w1 { wRec = if fromIntegral (Seq.length r) > width - 80 then Seq.drop 1 r else r
          , wRings = Seq.filter alive (fmap grow (wRings w1))
          , wDroneT = if fire then droneEvery else dt1 }

drawAll :: World -> Picture
drawAll w = Pictures
  ( -- 窓口と行列
    box (qx + 20) (qy - 30) 50 60 (hsb 200 50 (if n > 0 then 80 else 30))
  : [ dot (qx + 40) qy 22 (hsb 140 70 95) | n > 0 ]
 ++ [ dot (qx - fromIntegral i * 14) qy 11 (hsb 30 70 100) | i <- [1 .. min n maxDots - 1] ]
 ++ [ txt 6 (qy - 14) 14 (hsb 0 80 100) ("+" ++ show (n - maxDots)) | n > maxDots ]
    -- 音の波紋
 ++ [ hoop x y d 2 (hsba h 70 100 ((100 - d) / 100)) | Ripple x y d h <- toList (wRings w) ]
    -- 系内人数の記録（色 = 音の高さ）
 ++ [ seg 1 (hsba 0 0 100 0.25) (40, gy) (width - 40, gy) ]
 ++ [ seg 1 (hsb (140 + fromIntegral (min v 14) * 10) 70 100) (40 + i, gy) (40 + i, gy - fromIntegral v / top * gh)
    | (i, v) <- zip [0 ..] recs, v > 0 ]
 ++ [ txt 44 (gy - gh - 8) 14 (hsb 0 0 60) ("People in system (max " ++ show (round top :: Int) ++ "). Mean rho/(1-rho) = " ++ nf 1 (rho / (1 - rho)))
    , txt tx ty 16 (gray 1) ("In system now  " ++ show n)
    , txt tx (ty + 26) 16 (gray 1) ("Arrival rate lambda = rho mu = " ++ nf 2 (rho * wMu w) ++ " /s")
    , txt tx (ty + 52) 16 (gray 1) ("Service rate mu = " ++ nf 1 (wMu w) ++ " /s")
    , txt tx (ty + 86) 14 (hsb 0 0 60) "Click = arrival (a random rhythm: a Poisson process)"
    , txt tx (ty + 108) 14 (hsb 0 0 60) "Bong = service end. Longer line, higher scale note"
    , txt tx (ty + 130) 14 (hsb 0 0 60) "Hum = drone. 12 more people, one octave up"
    , txt 20 40 18 (gray 1) ("rho = " ++ nf 2 rho ++ " (<- ->)  Tempo mu = " ++ nf 1 (wMu w) ++ " (Up/Down)  M: drone " ++ (if wDrone w then "ON" else "off")
        ++ "  Space: " ++ (if wPaused w then "resume" else "pause"))
    , txt 20 68 15 (hsb 0 0 60) "At rho = 0.5 the bongs stay low. Above 0.9 high notes persist and the drone climbs. Close your eyes and guess." ] )
  where
    n = wN w
    rho = wRho w
    gy = height - 60
    gh = height * 0.35
    recs = toList (wRec w)
    top = fromIntegral (maximum (10 : recs)) :: Float
    tx = width * 0.55
    ty = 140

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> rearm w { wRho = roundTo 100 (min 0.98 (wRho w + 0.05)) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rearm w { wRho = roundTo 100 (max 0.1 (wRho w - 0.05)) }
  EventKey (SpecialKey KeyUp) Down _ _ -> rearm w { wMu = min 8 (wMu w + 0.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> rearm w { wMu = max 0.5 (wMu w - 0.5) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _ | c `elem` "mM" -> pure w { wDrone = not (wDrone w) }
  _ -> pure w
  where rearm w1 = (\a -> w1 { wNextArr = wT w1 + a }) <$> expRand (lamOf w1)

-- ---------- やってみよう ----------
-- 1. ρ = 0.5 と 0.95 を交互に聞く。音だけで区別できる？ 友達に当ててもらう
-- 2. サービス時間を一定（1 / mu）に変えると「ポーン」のリズムが規則的になる。行列の音はどう変わる？（08）
-- 3. 到着の音を、到着した客の待ち時間で変える（長く待ちそうなら低い音）
-- 4. 到着の音を別の波形（Sine や Sawtooth）に変えて、聞き分けやすい組み合わせを探す

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Float -> Float -> Point
pt x y = (x - width / 2, height / 2 - y)

-- 左上の角が (x, y) の長方形
box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)

-- 中心 (x, y)、直径 d の円
dot :: Float -> Float -> Float -> Color -> Picture
dot x y d c = Color c (at (pt x y) (disc (d / 2)))

-- 中心 (x, y)、直径 d、太さ wd の輪
hoop :: Float -> Float -> Float -> Float -> Color -> Picture
hoop x y d wd c = Color c (at (pt x y) (ring (d / 2) wd))

-- 太さ wd の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg wd c (x1, y1) (x2, y2) = Color c (thickLine wd (pt x1 y1) (pt x2 y2))

-- 文字：(x, y) は左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- p5 の nf(v, 1, d)：小数 d 桁の文字列
nf :: PrintfArg a => Int -> a -> String
nf d v = printf "%.*f" d v

-- 平均 1/rate の指数分布の乱数（次の到着までの時間など）
expRand :: Double -> IO Double
expRand rate = do
  u <- randomRIO (0, 1 :: Double)
  pure (-log (max 1e-12 (1 - u)) / rate)

infinity :: Double
infinity = 1 / 0

-- p5 の round(v * k) / k
roundTo :: Float -> Float -> Float
roundTo k v = fromIntegral (round (v * k) :: Int) / k
