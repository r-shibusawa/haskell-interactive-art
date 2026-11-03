-- ============================================
-- 09 三体問題とラグランジュ点
-- 太陽と惑星と一緒に回る座標（回転座標系）で見ると、重力と遠心力がつり合う点が 5 つある（L1〜L5）
--   色の帯 = 有効ポテンシャルの等高線。L4・L5 は山の頂上なのに、コリオリの力のおかげで小さな星が留まる
-- クリック = 小さな星を置く / 4 = L4 のまわりに置く / 1 = L1 のまわりに置く / ↑↓ = 質量比 μ / F = 8 の字の三体 / P = 少しずらす / C = 消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.Foldable (foldl', toList)
import qualified Data.Sequence as Seq
import Data.Word (Word8)
import Text.Printf (printf)

speedSteps :: Int
speedSteps = 6      -- ★ 1 フレームに何ステップ進めるか

hStep :: Float
hStep = 0.002       -- ★ 時間の刻み（無次元）

histLen :: Int
histLen = 600

-- 小さな星 [x, y, vx, vy]
data Part = Part { pS :: ![Float], pHue :: !Float, pTrail :: !(Seq.Seq Point) }

-- 8 の字の星
data Body = Body { bHue :: !Float, bTrail :: !(Seq.Seq Point) }

data World = World
  { wMu    :: !Float          -- ★ 惑星の質量 /（太陽 + 惑星）。0.0385 より大きいと L4 は不安定
  , wParts :: ![Part]
  , wL     :: ![Point]        -- L1〜L5
  , wBG    :: !Picture        -- 背景の等高線
  , wFig8  :: !Bool
  , wAll   :: ![Float]        -- 8 の字の状態 [x1, y1, vx1, vy1, x2, ...]
  , wBodies :: ![Body]
  , hC, hE :: !(Seq.Seq Float)
  }

main :: IO ()
main = runSketch (sketch "24-09 Three Bodies and Lagrange Points" (rebuild world0))
  { skBack  = hsb 250 30 8
  , skStart = \_ -> addCluster 3 0.02 (rebuild world0)
  , skStep  = \_ _ w -> pure (if wFig8 w then stepFig8 w else stepParts w)
  , skDraw  = \_ w -> if wFig8 w then drawFig8 w else drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World 0.01 [] [] Blank False [] [] Seq.empty Seq.empty

cxS, cyS, scS :: Float
cxS = width * 0.25
cyS = height * 0.54
scS = min (width * 0.44 / 3.2) ((height - 150) / 2.6)

-- 有効ポテンシャル Ω = (x² + y²)/2 + (1 − μ)/r₁ + μ/r₂（遠心力 + 2 つの重力）
omegaP :: Float -> Float -> Float -> Float
omegaP mu x y =
  let r1 = sqrt ((x + mu) * (x + mu) + y * y)
      r2 = sqrt ((x - 1 + mu) * (x - 1 + mu) + y * y)
  in (x * x + y * y) / 2 + (1 - mu) / max r1 1e-3 + mu / max r2 1e-3

-- ★ 回転座標系の運動方程式：重力 + 遠心力 + コリオリの力（2ẏ, −2ẋ）
deriv :: Float -> [Float] -> [Float]
deriv mu [x, y, vx, vy] =
  let r1 = max (sqrt ((x + mu) * (x + mu) + y * y)) 0.01
      r2 = max (sqrt ((x - 1 + mu) * (x - 1 + mu) + y * y)) 0.01
      r13 = r1 * r1 * r1
      r23 = r2 * r2 * r2
      ax = 2 * vy + x - (1 - mu) * (x + mu) / r13 - mu * (x - 1 + mu) / r23
      ay = -2 * vx + y - (1 - mu) * y / r13 - mu * y / r23
  in [vx, vy, ax, ay]
deriv _ s = map (const 0) s

rk4 :: Float -> ([Float] -> [Float]) -> [Float] -> [Float]
rk4 dt f s =
  let k1 = f s
      k2 = f (zipWith (\v k -> v + k * dt / 2) s k1)
      k3 = f (zipWith (\v k -> v + k * dt / 2) s k2)
      k4 = f (zipWith (\v k -> v + k * dt) s k3)
      next = zipWith3 (\v a b -> v + dt / 6 * (a + 2 * b)) s k1 k2
      next2 = zipWith3 (\v a b -> v + dt / 6 * (2 * a + b)) next k3 k4
  in strictList next2

rebuild :: World -> World
rebuild w =
  let mu = wMu w
      ab3 v = let a = abs v in a * a * a
      -- L1〜L3：x 軸上で力がつり合う点を二分法で探す
      fx x = x - (1 - mu) * (x + mu) / ab3 (x + mu) - mu * (x - 1 + mu) / ab3 (x - 1 + mu)
      bis :: Float -> Float -> Float
      bis a0 b0 = go (80 :: Int) a0 b0
        where go 0 a b = (a + b) / 2
              go i a b = let c = (a + b) / 2
                         in if fx a * fx c <= 0 then go (i - 1) a c else go (i - 1) c b
      ls = [ (bis (-mu + 1e-4) (1 - mu - 1e-4), 0), (bis (1 - mu + 1e-4) 2, 0), (bis (-2) (-mu - 1e-4), 0)
           , (0.5 - mu, sqrt 3 / 2), (0.5 - mu, -sqrt 3 / 2) ]
      cL = case ls of
        ((x, _) : _) -> omegaP mu x 0
        [] -> 1.5
      -- 背景：Ω の等高線を色の帯で（小さい画像に描いて拡大）
      bgW = 180 :: Int
      bgH = 146 :: Int
      pix i j =
        let x = (fromIntegral i / fromIntegral bgW - 0.5) * (width * 0.46) / scS
            y = -(fromIntegral j / fromIntegral bgH - 0.5) * (height - 120) / scS
            o = min (omegaP mu x y) (cL + 0.2)        -- 遠く・星の近くは同じ色に
            band = floor (o / 0.015) :: Int
        in hsb2rgb (remap (clampF 1.4 (cL + 0.15) o) 1.4 (cL + 0.15) 240 330) 0.45 (if odd band then 0.26 else 0.19)
      bg = Translate (cxS - width / 2) (height / 2 - cyS)
             (Scale (width * 0.46 / fromIntegral bgW) ((height - 120) / fromIntegral bgH) (pixelImage bgW bgH 1 pix))
  in w { wL = ls, wBG = bg }

addAt :: Float -> Float -> Float -> Float -> Float -> World -> World
addAt x y vx vy hue w =
  let ps = wParts w ++ [Part [x, y, vx, vy] hue Seq.empty]
  in w { wParts = if length ps > 40 then drop 1 ps else ps }

addCluster :: Int -> Float -> World -> IO World
addCluster li spread w = case drop li (wL w) of
  ((lx, ly) : _) -> do
    offs <- replicateM 8 ((,) <$> randomRIO (-0.5, 0.5) <*> randomRIO (-0.5, 0.5))
    pure (foldl' (\q (a, b) -> addAt (lx + a * spread) (ly + b * spread) 0 0 (if li == 3 then 140 else 20) q) w offs)
  [] -> pure w

jacobi :: Float -> [Float] -> Float
jacobi mu [x, y, vx, vy] = 2 * omegaP mu x y - vx * vx - vy * vy
jacobi _ _ = 0

stepParts :: World -> World
stepParts w =
  let mu = wMu w
      adv :: Int -> [Float] -> [Float]
      adv 0 s = s
      adv k s = adv (k - 1) (rk4 hStep (deriv mu) s)
      moved = [p { pS = adv speedSteps (pS p) } | p <- wParts w]
      alive = filter ok moved
      ok p = case pS p of
        [x, y, vx, vy] -> not (any isBad [x, y, vx, vy]) && sqrt (x * x + y * y) < 3
        _ -> False
      withTrail = [ case pS p of
                      (x : y : _) -> let t = pTrail p Seq.|> (x, y)
                                     in p { pTrail = if Seq.length t > 500 then Seq.drop 1 t else t }
                      _ -> p
                  | p <- alive ]
      hC' = case withTrail of
        (p : _) -> pushHist (hC w) (jacobi mu (pS p))
        [] -> hC w
  in w { wParts = withTrail, hC = hC' }

isBad :: Float -> Bool
isBad v = isNaN v || isInfinite v

drawAll :: World -> Picture
drawAll w = Pictures
  [ wBG w
  , view ( Color (hsb 45 90 100) (at (xS (-mu), yS 0) (disc 15))
         : Color (hsb 200 60 100) (at (xS (1 - mu), yS 0) (disc ((8 + 60 * sqrt mu) / 2)))
         : [Color (gray 1) (at (xS lx, yS ly) (ring 6 1.5)) | (lx, ly) <- wL w]
        ++ concatMap partPic (wParts w) )
  , Pictures [txt (xS lx + 8) (yS ly - 8) 14 (gray 1) ("L" ++ show i) | (i, (lx, ly)) <- zip [1 :: Int ..] (wL w)]
    -- 右：説明とヤコビ積分
  , view [Color (hsb 0 0 12) (boxAt gx (height * 0.12) gw (height * 0.36))]
  , Pictures (zipWith (\i s -> txt (gx + 14) (height * 0.12 + 30 + i * 28) 14 (hsb 0 0 85) s) [0 ..] infoLines)
  , drawPlot gx (height * 0.54) gw (height * 0.34) (hC w) hcLo hcHi (hsb 140 70 100) "Jacobi integral C = 2W - v^2 of star 1"
  , txt 20 36 18 (gray 1) "Click: place star   4: at L4   1: at L1   Up/Down: mu   F: figure-8 bodies   C: clear"
  , txt 20 66 14 (hsb 0 0 65) "In the rotating frame: centrifugal force (outward), Coriolis force (across v). A star placed at rest soon curves."
  ]
  where
    mu = wMu w
    xS x = cxS + x * scS
    yS y = cyS - y * scS
    gx = width * 0.52
    gw = width * 0.44
    (hcLo, hcHi) | Seq.null (hC w) = (0, 1)
                 | otherwise = (foldl' min (1 / 0) (hC w) - 0.01, foldl' max (-1 / 0) (hC w) + 0.01)
    lxOf i = case drop i (wL w) of
      ((x, _) : _) -> x
      [] -> 0
    infoLines =
      [ "mu = " ++ nf 4 mu ++ " (Up/Down)  L4,L5 stable if mu<0.0385: " ++ (if mu < 0.0385 then "YES" else "NO")
      , "L1 " ++ nf 3 (lxOf 0) ++ "  L2 " ++ nf 3 (lxOf 1) ++ "  L3 " ++ nf 3 (lxOf 2) ++ "  (sun-planet = 1)"
      , "L4, L5 form equilateral triangles (green stars)"
      , "L1 is a saddle: a small shift falls away (orange)"
      , "Real: Jupiter's Trojans (L4, L5), JWST (L2)"
      ]
    partPic p = case pS p of
      (x : y : _) ->
        [ Color (hsba (pHue p) 70 100 0.6) (Line (every2 [(xS a, yS b) | (a, b) <- toList (pTrail p)]))
        , Color (hsb (pHue p) 70 100) (at (xS x, yS y) (disc 3.5)) ]
      _ -> []

-- 8 の字の解（Chenciner–Montgomery 2000）：同じ質量 3 つが同じ 8 の字を追いかけあう
startFig8 :: Float -> World -> World
startFig8 eps w =
  let x1 = 0.97000436
      y1 = -0.24308753
      vx3 = -0.93240737
      vy3 = -0.86473146
  in w { wAll = [ x1, y1, -vx3 / 2, -vy3 / 2
                , -x1, -y1, -vx3 / 2, -vy3 / 2
                , eps, 0, vx3, vy3 ]
       , wBodies = [Body h Seq.empty | h <- [30, 200, 320]]
       , hE = Seq.empty }

-- 4 個ずつ（x, y, vx, vy）に分ける
chunks4 :: [Float] -> [(Float, Float, Float, Float)]
chunks4 (a : b : c : d : rest) = (a, b, c, d) : chunks4 rest
chunks4 _ = []

fig8Deriv :: [Float] -> [Float]
fig8Deriv s =
  let bs = chunks4 s
      accOf (xi, yi, _, _) =
        foldl' (\(ax, ay) (xj, yj, _, _) ->
                  let dx = xj - xi
                      dy = yj - yi
                  in if dx == 0 && dy == 0 then (ax, ay) else
                     let r = max (sqrt (dx * dx + dy * dy)) 0.02
                         r3 = r * r * r
                     in (ax + dx / r3, ay + dy / r3)) (0, 0) bs
  in concat [ [vx, vy, ax, ay] | b@(_, _, vx, vy) <- bs, let (ax, ay) = accOf b ]

fig8Energy :: [Float] -> Float
fig8Energy s =
  let bs = chunks4 s
      kin = sum [0.5 * (vx * vx + vy * vy) | (_, _, vx, vy) <- bs]
      pairs = [(a, b) | (i, a) <- zip [0 :: Int ..] bs, (j, b) <- zip [0 ..] bs, j > i]
      pot = sum [ 1 / max (sqrt ((xa - xb) * (xa - xb) + (ya - yb) * (ya - yb))) 0.02
                | ((xa, ya, _, _), (xb, yb, _, _)) <- pairs ]
  in kin - pot

stepFig8 :: World -> World
stepFig8 w =
  let adv :: Int -> [Float] -> [Float]
      adv 0 s = s
      adv k s = adv (k - 1) (rk4 0.001 fig8Deriv s)
      s1 = adv 12 (wAll w)
  in if any isBad s1 then startFig8 0 w else
     w { wAll = s1
       , wBodies = [ let t = bTrail b Seq.|> (x, y)
                     in b { bTrail = if Seq.length t > 700 then Seq.drop 1 t else t }
                   | (b, (x, y, _, _)) <- zip (wBodies w) (chunks4 s1) ]
       , hE = pushHist (hE w) (fig8Energy s1) }

drawFig8 :: World -> Picture
drawFig8 w = Pictures
  [ view (concat
      [ [ Color (hsba (bHue b) 70 100 0.6) (thickPath 2 (every2 [(xS a, yS c) | (a, c) <- toList (bTrail b)]))
        , Color (hsb (bHue b) 70 100) (at (xS x, yS y) (disc 11)) ]
      | (b, (x, y, _, _)) <- zip (wBodies w) bs ])
  , drawPlot (width * 0.52) (height * 0.2) (width * 0.44) (height * 0.4) (hE w) (-1.4) (-1.2) (hsb 55 70 100) "Total energy (should be constant)"
  , txt 20 36 18 (gray 1) "Figure-8 three bodies (Chenciner-Montgomery)   P: nudge the middle star   F: back"
  , txt 20 58 14 (hsb 0 0 65) ("Three equal masses chase each other on one figure 8: a special periodic solution. Total momentum (" ++ nf 4 mx ++ ", " ++ nf 4 my ++ ").")
  , txt 20 78 14 (hsb 0 0 65) "Nudge it and it may fall apart some day (chaos, series 4)."
  ]
  where
    s = min (width * 0.2) (height * 0.35)
    xS x = width * 0.25 + x * s
    yS y = height * 0.54 - y * s
    bs = chunks4 (wAll w)
    mx = sum [vx | (_, _, vx, _) <- bs]
    my = sum [vy | (_, _, _, vy) <- bs]

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos
    | not (wFig8 w) && mx <= width * 0.48 && my >= 90 ->
        pure (addAt ((mx - cxS) / scS) (-(my - cyS) / scS) 0 0 55 w)
    where (mx, my) = toP5 pos
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (rebuild w { wMu = min (wMu w * 1.5) 0.3, wParts = [], hC = Seq.empty })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (rebuild w { wMu = max (wMu w / 1.5) 0.001, wParts = [], hC = Seq.empty })
  EventKey (Char '4') Down _ _ -> addCluster 3 0.04 w
  EventKey (Char '1') Down _ _ -> addCluster 0 0.01 w
  EventKey (Char c) Down _ _ -> pure $ case toLower c of
    'c' -> w { wParts = [], hC = Seq.empty }
    'f' -> let on = not (wFig8 w) in (if on then startFig8 0 else id) w { wFig8 = on }
    'p' | wFig8 w -> startFig8 0.01 w
    _ -> w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 4 で L4 に星を置く。星はおたまじゃくしのような形でゆっくり回るが、L4 から離れない
-- 2. μ を 0.05 以上にして同じことをする。今度は飛び去っていく（ラウスの条件）
-- 3. 1 で L1 に置くと、太陽か惑星のどちらかへ落ちていく。峠の上のボールと同じ
-- 4. F の 8 の字で P を押し、何周で崩れるか数える。刻み h を小さくすると長持ちする？


-- ==================================================
-- 道具（グラフを描く）
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

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y w h = rect (x + w / 2) (y + h / 2) w h

nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

every2 :: [a] -> [a]
every2 (a : _ : c : rest) = a : every2 (c : rest)
every2 xs = xs

-- リストの中身まで計算を済ませる（遅延評価のたまりを防ぐ）
strictList :: [Float] -> [Float]
strictList xs = foldr seq xs xs

-- HSB（色相 0〜360、彩度・明るさ 0〜1）→ RGB（0〜255）
hsb2rgb :: Float -> Float -> Float -> (Word8, Word8, Word8)
hsb2rgb hh ss vv = (f 5, f 3, f 1)
  where
    f :: Float -> Word8
    f n = let k = wrapF 6 (n + hh / 60)
          in fromIntegral (round (255 * (vv - vv * ss * max 0 (min k (min (4 - k) 1)))) :: Int)

pushHist :: Seq.Seq Float -> Float -> Seq.Seq Float
pushHist s !v =
  let s' = s Seq.|> v
  in if Seq.length s' > histLen then Seq.drop 1 s' else s'

-- 時間プロット（縦の範囲 lo〜hi も表示する）
drawPlot :: Float -> Float -> Float -> Float -> Seq.Seq Float -> Float -> Float -> Color -> String -> Picture
drawPlot x0 y0 w h0 d lo hi c title = Pictures
  [ view [ Color (hsb 0 0 12) (boxAt x0 y0 w h0)
         , Color c (thickPath 2
             [ ( remap (fromIntegral (i + off)) 0 (fromIntegral histLen - 1) x0 (x0 + w)
               , remap (clampF lo hi v) lo hi (y0 + h0 - 10) (y0 + 30) )
             | (i, v) <- zip [0 :: Int ..] (toList d), even i ]) ]
  , txt (x0 + 8) (y0 + 20) 14 (hsb 0 0 70) title
  , txt (x0 + 8) (y0 + h0 - 14) 14 (hsb 0 0 70) ("Range " ++ nf 3 lo ++ " to " ++ nf 3 hi)
  ]
  where off = histLen - Seq.length d
