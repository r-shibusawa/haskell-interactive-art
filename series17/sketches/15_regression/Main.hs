-- ============================================
-- 15 勾配降下法で直線をあてはめる（線形回帰）
-- 直線 y = w·x + b の「ずれ（残差）の 2 乗の平均」を損失として、(w, b) を勾配降下で動かす
-- 右の小さな地図は (w, b) の損失の地形。金の点がそこを下っていく
-- 点線は一発で解ける最小 2 乗法の答え。勾配降下はそこに近づいていく
-- 操作：クリック = 点を追加 / スペース = 再生・停止 / → = 1 歩 / ↑↓ = 学習率
--       G = 全部の点を使う（バッチ）⇄ 1 点ずつ（確率的） / R = 新しいデータ / C = 全部消す
--       1〜3 = お手本（右上がり・はずれ値あり・急な坂）
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

speed :: Int
speed = 4               -- ★ 何フレームに 1 歩

startW, startB :: Float
startW = -1             -- ★ 直線の傾きの初期値
startB = 0.8            -- ★ 切片の初期値

-- 損失の地図の範囲
w0, w1, b0, b1 :: Float
w0 = -3
w1 = 3
b0 = -2
b1 = 2

data World = World
  { wPts   :: ![Point]    -- 正規化した座標（x, y とも −1〜1 くらい）
  , wW     :: !Float
  , wB     :: !Float
  , wEta   :: !Float      -- ★ 学習率
  , wSgd   :: !Bool       -- 確率的（1 点ずつ）か
  , wAuto  :: !Bool
  , wPath  :: ![Point]    -- (w, b) の道（新しい順、500 点まで）
  , wLoss  :: ![Float]    -- 損失の履歴（新しい順）
  , wSurf  :: !Picture    -- 損失の地図
  }

main :: IO ()
main = runSketch (sketch "17-15 Line fitting" initialWorld)
  { skBack  = cBg
  , skStart = \_ -> preset 1 initialWorld
  , skStep  = \env _ w -> if wAuto w && envFrame env `mod` speed == 0 then descend w else pure w
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialWorld :: World
initialWorld = World [] startW startB 0.1 False True [] [] Blank

preset :: Int -> World -> IO World
preset k w = do
  let slope = if k == 3 then 2.2 else 0.7
      icpt = if k == 3 then -0.3 else 0.1
  ps <- replicateM 30 $ do
    x <- randomRIO (-1, 1)
    g <- gaussian
    pure (x, slope * x + icpt + g * 0.15)
  let extra = if k == 2 then [(0.8, -1.2), (0.9, -1.0)] else []   -- はずれ値
  pure (resetModel w { wPts = ps ++ extra })

resetModel :: World -> World
resetModel w = w { wW = startW, wB = startB, wPath = [(startW, startB)]
                 , wLoss = [loss (wPts w) startW startB], wSurf = drawSurface (wPts w) }

loss :: [Point] -> Float -> Float -> Float
loss [] _ _ = 0
loss ps a c = foldl' (\s (x, y) -> let r = a * x + c - y in s + r * r) 0 ps / fromIntegral (length ps)

-- 最小 2 乗法の答え（正規方程式）
leastSquares :: [Point] -> Maybe (Float, Float)
leastSquares ps
  | n < 2 || sxx < 1e-12 = Nothing
  | otherwise = Just (a, my - a * mx)
  where
    n = length ps
    c = fromIntegral n
    mx = sum (map fst ps) / c
    my = sum (map snd ps) / c
    sxx = sum [(x - mx) * (x - mx) | (x, _) <- ps]
    sxy = sum [(x - mx) * (y - my) | (x, y) <- ps]
    a = sxy / sxx

-- ★ 1 歩
descend :: World -> IO World
descend w
  | null (wPts w) = pure w
  | otherwise = do
      use <- if wSgd w
        then (\i -> take 1 (drop i (wPts w))) <$> randomRIO (0, length (wPts w) - 1)
        else pure (wPts w)
      let c = fromIntegral (length use)
          (gw, gb) = foldl' (\(a, e) (x, y) -> let r = wW w * x + wB w - y in (a + 2 * r * x, e + 2 * r)) (0, 0) use
          a' = wW w - wEta w * gw / c
          b' = wB w - wEta w * gb / c
          bad v = isNaN v || isInfinite v || abs v > 1e4
      pure $ if bad a' || bad b'
        then resetModel w
        else w { wW = a', wB = b', wPath = take 500 ((a', b') : wPath w)
               , wLoss = take 500 (loss (wPts w) a' b' : wLoss w) }

-- 画面 ⇄ データの座標
plotW :: Env -> Float
plotW env = envW env - 340

sx, sy :: Env -> Float -> Float
sx env x = remap x (-1.3) 1.3 40 (plotW env - 20)
sy env y = remap y (-2) 2 (envH env - 30) 110

-- 損失の地図：6 ピクセルのマスを 40 × 40 個（対数スケールで色分け）
drawSurface :: [Point] -> Picture
drawSurface ps =
  let n = 40 :: Int
      big = 240 :: Float
      g = big / fromIntegral n
      val i j =
        let a = remap (fromIntegral i * g + g / 2) 0 big w0 w1
            c = remap (fromIntegral j * g + g / 2) big 0 b0 b1
        in loss ps a c
      vals = U.generate (n * n) (\k -> val (k `mod` n) (k `div` n))
      lo = U.minimum vals
      hi = U.maximum vals
      colorAt i j =
        let v = vals U.! (j * n + i)
            t = if hi - lo > 1e-12 then log (1 + v - lo) / log (1 + hi - lo) else 0
            band = fromIntegral (floor (t * 12) :: Int) / 12
            mix a c = (a + (c - a) * band) / 255
        in (mix 0x2a 0xe7, mix 0x1d 0xb5, mix 0x4a 0xff)
  in pixelImageF n n g colorAt

drawW :: Env -> World -> Picture
drawW env w =
  let ww = envW env
      hh = envH env
      sxE = sx env
      syE = sy env
      a = wW w
      b = wB w
      ps = wPts w
      ls = leastSquares ps
      residual (x, y) = seg env cBad 1.5 (sxE x, syE y) (sxE x, syE (a * x + b))
      lineOf (c, d) = ((sxE (-1.3), syE (c * (-1.3) + d)), (sxE 1.3, syE (c * 1.3 + d)))
      lsLine = case ls of
        Just v -> let (p, q) = lineOf v in dashed env cMint 2 8 p q
        Nothing -> Blank
      (cur1, cur2) = lineOf (a, b)
      -- 右のパネル：損失の地図
      px = ww - 310
      py = 20
      mapX v = px + 25 + remap v w0 w1 0 240
      mapY v = py + 44 + remap v b1 b0 0 240
      onMap (c, d) = (mapX (clampF w0 w1 c), mapY (clampF b0 b1 d))
      -- 損失の推移
      hist = reverse (wLoss w)
      lossLine
        | length hist > 1 =
            let lo = minimum hist
                hi = maximum hist
                n = length hist
                vx i = px + 25 + fromIntegral (i :: Int) / fromIntegral (n - 1) * 240
                vy v = py + 420 - (v - lo) / max (hi - lo) 1e-9 * 90
            in path env cOrchid 2 [(vx i, vy v) | (i, v) <- zip [0 ..] hist]
        | otherwise = Blank
      info =
        [ "Loss (mean sq. err) " ++ fix 4 (loss ps a b)
        , "w = " ++ fix 3 a ++ "   b = " ++ fix 3 b
        , "eta = " ++ prec2 (wEta w)
        , if wSgd w then "SGD (1 point): wobbly" else "Batch (all points): smooth" ]
  in Pictures
       [ seg env cEdge 1 (sxE (-1.3), syE 0) (sxE 1.3, syE 0)
       , seg env cEdge 1 (sxE 0, syE (-2)) (sxE 0, syE 2)
       , Pictures (map residual ps)
       , lsLine
       , seg env cGold 4 cur1 cur2
       , Pictures [dot env cOrchid (sxE x, syE y) 10 | (x, y) <- ps]
       , box env cPanel px py 290 (hh - 40)
       , txt env (px + 20) (py + 14) 16 cFg "Loss map of (w, b)"
       , at (pt env (px + 145, py + 164)) (wSurf w)
       , path env cGold 1.5 (map onMap (reverse (wPath w)))
       , dot env cGold (onMap (a, b)) 10
       , case ls of
           Just v -> dot env cMint (onMap v) 8
           Nothing -> Blank
       , txt env (px + 20) (py + 290) 14 cDim "Across: slope w   Up: b"
       , txt env (px + 20) (py + 308) 14 cDim "Green = least squares"
       , lossLine
       , Pictures [ txt env (px + 20) (py + 440 + fromIntegral k * 20) 14 cFg l | (k, l) <- zip [0 :: Int ..] info ]
       , case ls of
           Just (c, d) -> txt env (px + 20) (py + 530) 14 cMint ("LSQ: w = " ++ fix 3 c ++ ", b = " ++ fix 3 d)
           Nothing -> Blank
       , txt env 30 24 22 cFg "Fit a line by gradient descent"
       , txt env 30 56 14 cDim "Click add  Space play/stop  Right step  Up/Down eta  G batch/SGD  R new  C clear  1-3"
       , if length ps < 2 then txt env 30 84 14 cBad "Click at least 2 points" else Blank
       ]

-- 有効数字 2 桁（JavaScript の toPrecision(2)）
prec2 :: Float -> String
prec2 v
  | v <= 0 = "0"
  | otherwise = fix (max 0 (1 - floor (logBase 10 v))) v

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | mx < plotW env && my > 100 ->
        let ps = wPts w ++ [ (remap mx 40 (plotW env - 20) (-1.3) 1.3, remap my (envH env - 30) 110 (-2) 2) ]
        in pure w { wPts = ps, wSurf = drawSurface ps }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wAuto = not (wAuto w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> descend w
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wEta = min (wEta w * 1.5) 5 }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wEta = max 1e-6 (wEta w / 1.5) }
  EventKey (Char c) Down _ _
    | c `elem` "gG" -> pure w { wSgd = not (wSgd w) }
    | c `elem` "rR" -> preset 1 w
    | c `elem` "cC" -> pure (resetModel w { wPts = [] })
    | c `elem` "123" -> preset (fromEnum c - fromEnum '0') w
  _ -> pure w
  where (mx, my) = mouse env

-- ---------- やってみよう ----------
-- 1. ↑ で学習率を上げて、発散して最初に戻るまでを見る
-- 2. G で確率的にして、金の点の動きがふらつくのを地図で見る
-- 3. お手本 2（はずれ値あり）で、最小 2 乗法の線がどれくらい引っぱられるか見る

-- ---------- 道具：乱数 ----------
-- 正規分布の乱数（p5.js の randomGaussian）。ボックス＝ミュラー法
gaussian :: IO Float
gaussian = do
  u <- randomRIO (1e-6, 1)
  v <- randomRIO (0, 1)
  pure (sqrt (-2 * log u) * cos (tau * v))

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

