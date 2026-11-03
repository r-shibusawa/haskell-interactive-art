-- ============================================
-- 12 生態系の急変（レジームシフト）
-- 草原の草の量 x と、草を食べる牛の数 c：dx/dt = r·x(1 − x/K) − c·x²/(1 + x²)（メイの放牧モデル）
--   牛をゆっくり増やすと、草はなめらかに少しずつ減る……が、ある所で一気に「はげ山」へ崩れる。牛を戻しても草は戻らない
--   湖の富栄養化、サンゴ礁、砂漠化でも同じことが起きる。崩れる前に「ゆれが大きく・ゆっくり」になる（早期警報シグナル）
-- ←→ = 牛の数 c / A = c をゆっくり増やす・減らす / ↑↓ = ゆらぎの大きさ / R = 最初から
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, replicateM)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Numeric (showFFloat)

rGrow, kCap :: Float
rGrow = 1                  -- ★ 草の成長の速さ
kCap = 10                  -- ★ 草の上限

windowLen :: Int
windowLen = 150            -- ★ 警報を計算する窓の長さ

data Blade = Blade !Float !Float !Float

data World = World
  { wC      :: !Float      -- 牛の数（食べる強さ）
  , wX      :: !Float      -- 草の量
  , wSigma  :: !Float      -- ★ 天気などのゆらぎ
  , wAuto   :: !Bool
  , wDir    :: !Float
  , wXs     :: !(Seq Float)
  , wVars   :: !(Seq Float)
  , wAcs    :: !(Seq Float)
  , wVr     :: !Float
  , wAc     :: !Float
  , wBlades :: ![Blade]
  }

world0 :: World
world0 = World 1.2 8.5 0.25 False 1 Seq.empty Seq.empty Seq.empty 0 0 []

main :: IO ()
main = runSketch (sketch "35-12 Regime Shift" world0)
  { skBack  = hsb 100 15 11
  , skStart = \_ -> do
      bl <- replicateM 160 (Blade <$> randomRIO (0, 1) <*> randomRIO (0.6, 1) <*> randomRIO (-0.3, 0.3))
      pure world0 { wBlades = bl }
  , skStep  = \_ _ w -> stepWorld w
  , skDraw  = drawAll
  , skEvent = onEvent
  }

growth :: Float -> Float -> Float
growth x c = rGrow * x * (1 - x / kCap) - c * x * x / (1 + x * x)

-- f = 0 を c について解く（x > 0）
cOf :: Float -> Float
cOf x = rGrow * (1 - x / kCap) * (1 + x * x) / x

stepWorld :: World -> IO World
stepWorld w0 = do
  let c0 = wC w0
      c1 = if wAuto w0 then c0 + wDir w0 * 0.0008 else c0
      dir | not (wAuto w0) = wDir w0
          | c1 > 2.9 = -1
          | c1 < 1.2 = 1
          | otherwise = wDir w0
      dt = 0.05
  x <- foldM (\v _ -> do
                g <- gaussian
                pure (clampF 0.01 (kCap * 1.2) (v + growth v c1 * dt + wSigma w0 * sqrt dt * g)))
             (wX w0) [1 .. 4 :: Int]
  let xs = pushN 600 x (wXs w0)
      -- ★ 早期警報：最近 W 個の分散と、1 つずれの自己相関
      win = toList (Seq.drop (Seq.length xs - windowLen) xs)
      n = fromIntegral (length win) :: Float
      m = foldl' (+) 0 win / n
      vr = foldl' (\s v -> s + (v - m) * (v - m)) 0 win / n
      acSum = foldl' (+) 0 (zipWith (\a b -> (b - m) * (a - m)) win (drop 1 win))
      ac = if vr > 1e-9 then acSum / (n - 1) / vr else 0
  pure w0 { wC = c1, wDir = dir, wX = x, wXs = xs, wVr = vr, wAc = ac
          , wVars = pushN 600 vr (wVars w0), wAcs = pushN 600 ac (wAcs w0) }

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      x = wX w
      c = wC w
      py = 110
      lw = wd * 0.45
      top = h * 0.42
      groundY = top - 40
      -- 草原
      blades = [ Color (hsb 100 70 (40 + 40 * x / kCap)) (thickLine 3 (bx, groundY) (bx + s * hh, groundY - hh))
               | Blade bu bh s <- wBlades w
               , let bx = 40 + bu * (lw - 20)
                     hh = (x / kCap) * 110 * bh ]
      span_ = lw - 60
      cows = [ Pictures [ Color (gray 0.95) (ellipseAt cx cy 34 20)
                        , Color (gray 0.15) (Pictures [ellipseAt (cx - 6) (cy - 2) 9 7, ellipseAt (cx + 18) (cy - 5) 12 10]) ]
             | i <- [0 .. (round (c * 3) :: Int) - 1]
             , let sgn = if odd i then 1 else -1
                   cx = 60 + wrapF span_ (fromIntegral i * 97 + fromIntegral (envFrame env) * 0.2 * sgn)
                   cy = top - 55 - fromIntegral (i `mod` 3) * 8 ]
      -- 右上：分岐図（横 = c、縦 = x）
      rx = 60 + lw
      rw = wd - rx - 30
      toC v = rx + (v - 1) / 2 * rw
      toX v = top - 10 - v / kCap * (top - py - 30)
      green = hsb 100 60 90
      branch = [ Color (if stable then green else hsb 0 70 90) (thickLine (if stable then 4 else 1.5) (toC c0, toX x0) (toC c1, toX x1))
               | i <- [1 .. 499 :: Int]
               , let x0 = 0.02 + fromIntegral i * 0.02
                     x1 = x0 + 0.02
                     c0 = cOf x0
                     c1 = cOf x1
               , c0 >= 1, c0 <= 3, c1 >= 1, c1 <= 3
               , let stable = growth (x0 + 0.001) c0 - growth (x0 - 0.001) c0 < 0 ]
      -- 下：時間のグラフ
      gy = top + 20
      gh = (h - gy - 30) / 3 - 10
      autoText | not (wAuto w) = "off"
               | wDir w > 0 = "ON up"
               | otherwise = "ON down"
  in Pictures
    [ onP5 env (Pictures
        [ Color (hsb 200 40 35) (box 30 py lw (top - py))
        , Color (hsb 35 45 35) (box 30 groundY lw 40)
        , Pictures blades
        , Pictures cows
        , Color (hsb 100 10 17) (box rx py rw (top - py))
        , Pictures branch
        , Color green (thickLine 4 (toC 1, toX 0) (toC 3, toX 0))
        , Color (hsb 45 90 100) (at (toC c, toX x) (disc 6))
        , Color (hsbA 200 60 100 50) (Line [(toC c, py), (toC c, top)])
        ])
    , txt env 44 (py + 22) 14 white ("Grass x = " ++ nf 2 x ++ "   Cows c = " ++ nf 2 c)
    , txt env (rx + 12) (py + 22) 14 (gray 0.85) "Bifurcation: green stable, red unstable; c from 1 to 3"
    , plotLine env (wXs w) 0 kCap 30 gy (wd - 60) gh green "Grass x"
    , plotLine env (wVars w) 0 1.5 30 (gy + gh + 10) (wd - 60) gh (hsb 30 80 100) ("Variance (size of swings) = " ++ nf 3 (wVr w))
    , plotLine env (wAcs w) 0 1 30 (gy + 2 * (gh + 10)) (wd - 60) gh (hsb 280 50 100) ("Autocorrelation (slowness) = " ++ nf 2 (wAc w))
    , txt env 20 36 18 white ("Cows c = " ++ nf 3 c ++ " (<- ->)   A: auto " ++ autoText ++ "   noise s = " ++ nf 2 (wSigma w) ++ " (Up/Dn)   R: restart")
    , txt env 20 62 14 (gray 0.6) "Near c = 2.6 the grassland branch vanishes and it collapses. Recovery needs c back near 1.8 (hysteresis)."
    , txt env 20 84 14 (gray 0.6) "Shortly before a collapse, variance and autocorrelation slowly rise."
    ]

plotLine :: Env -> Seq Float -> Float -> Float -> Float -> Float -> Float -> Float -> Color -> String -> Picture
plotLine env arr lo hi x0 y0 w h col name =
  let pts = [ (x0 + fromIntegral i / 600 * w, y0 + h - 4 - clampF 0 1 ((v - lo) / (hi - lo)) * (h - 8))
            | (i, v) <- zip [0 :: Int ..] (toList arr) ]
  in Pictures
    [ onP5 env (Pictures [Color (hsb 100 10 17) (box x0 y0 w h), Color col (thickPath 2 pts)])
    , txt env (x0 + 10) (y0 + 16) 14 col name
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyLeft) Down _ _  -> w { wAuto = False, wC = max 1 (wC w - 0.05) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wAuto = False, wC = min 3 (wC w + 0.05) }
  EventKey (SpecialKey KeyUp) Down _ _    -> w { wSigma = min 1 (wSigma w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _  -> w { wSigma = max 0 (wSigma w - 0.05) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> world0 { wBlades = wBlades w, wSigma = wSigma w }
  _ -> w

-- ---------- やってみよう ----------
-- 1. A で牛をゆっくり増やし、崩壊する c を記録。そのあと減らして、草原が戻る c を記録しよう
-- 2. σ を 0.8 にすると、崩壊はもっと早く（小さい c で）起きる。なぜ？（谷が浅いと、ゆれで山をこえる）
-- 3. 崩壊の何フレーム前から分散が上がり始めた？ 警報として使えそう？
-- 4. 牛 c を時間とともに減らす「回復作戦」を考えよう。どこまで減らせば草原に戻る？


-- ==================================================
-- ここから下は「道具」（乱数・だ円・記録・座標の変換・色・数字）
-- ==================================================
-- 正規分布の乱数（p5.js の randomGaussian）
gaussian :: IO Float
gaussian = do
  u1 <- randomRIO (1e-6, 1)
  u2 <- randomRIO (0, 1)
  pure (sqrt (-2 * log u1) * cos (tau * u2))

-- 中心 (x, y)、幅 w、高さ h のだ円
ellipseAt :: Float -> Float -> Float -> Float -> Picture
ellipseAt x y w h = Translate x y (Scale 1 (h / w) (disc (w / 2)))

-- 長さ n までの記録に足す
pushN :: Int -> a -> Seq a -> Seq a
pushN n x s = let s' = s |> x in if Seq.length s' > n then Seq.drop 1 s' else s'

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
