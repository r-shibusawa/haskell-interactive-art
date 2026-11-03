-- ============================================
-- 11 アンマン・ビーンカー（八回対称）タイリング ─ 白銀比の準結晶
-- 正方形と 45° のひし形だけで、周期をもたずに平面をしきつめる。拡大率（インフレーション）は 1 + √2
-- ここでは de Bruijn の「多重格子法」でつくる：4 方向の平行線を引き、交点 1 つをタイル 1 枚に置きかえる
--   ひし形と正方形の枚数の比は √2 に近づく
-- ↑↓ = 広さ（線の本数）/ ←→ = 格子をずらす（フェイゾン）/ スペース = ずらし続ける / G = 格子線 / O = 八角形の目印
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

-- タイル 1 枚：方向の組 (r, s) と 4 つの角（1 辺 = 1 の座標）
data Tile = Tile
  { tDir  :: !Int
  , tA, tB, tC, tD :: !Point
  }

data World = World
  { wK      :: !Int      -- ★ 各方向の線の本数（片側）
  , wShift  :: !Double   -- ←→ で動かすずれ
  , wMoving :: !Bool
  , wGrid   :: !Bool
  , wOct    :: !Bool
  , wTiles  :: [Tile]
  , wSq     :: !Int      -- 正方形の枚数
  , wRh     :: !Int      -- ひし形の枚数
  }

gamma :: [Double]
gamma = [0.21, 0.37, 0.13, 0.29]   -- ★ 4 方向の格子のずれ

-- 4 方向の単位ベクトル
dirs :: [(Double, Double)]
dirs = [ (cos a, sin a) | j <- [0 .. 3 :: Int], let a = fromIntegral j * pi / 4 ]

initialWorld :: World
initialWorld = build (World 8 0 False False True [] 0 0)

main :: IO ()
main = runSketch (sketch "33-11 Ammann-Beenker" initialWorld)
  { skBack  = hsb 215 30 10
  , skStep  = \_ _ w -> pure (if wMoving w then build w { wShift = wShift w + 0.004 } else w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- ずらしたあとの格子の位置
offsetOf :: Double -> Int -> Double -> Double
offsetOf sh j v = v + sh * (if even j then 1 else -0.6)

-- 多重格子法：方向 r, s の線の交点ごとにタイルを 1 枚つくる
build :: World -> World
build w =
  let bigK = wK w
      ks = [-bigK .. bigK]
      g = zipWith (offsetOf (wShift w)) [0 ..] gamma
      idx = zip3 [0 :: Int ..] dirs g
      limit = fromIntegral (bigK * bigK) * 0.8 :: Double
      tiles =
        [ Tile (r * 4 + s) (corner 0 0) (corner 1 0) (corner 1 1) (corner 0 1)
        | (r, (ax, ay), gr) <- idx
        , (s, (bx, by), gs) <- idx
        , s > r
        , let det = ax * by - ay * bx
        , kr <- ks
        , ks' <- ks
        -- x·e_r = kr + g_r, x·e_s = ks + g_s を解く
        , let c1 = fromIntegral kr + gr
        , let c2 = fromIntegral ks' + gs
        , let px = (c1 * by - ay * c2) / det
        , let py = (ax * c2 - c1 * bx) / det
        , px * px + py * py <= limit      -- 円の中だけ
        -- 交点が他の方向のどの帯にあるか → 頂点の整数座標
        , let band j (ex, ey) gj
                | j == r = kr
                | j == s = ks'
                | otherwise = ceiling (px * ex + py * ey - gj) :: Int
        , let kk = [ (j, e, band j e gj) | (j, e, gj) <- idx ]
        , let corner dr ds =
                let sumUp (sx, sy) (j, (ex, ey), k0) =
                      let k = fromIntegral (k0 + (if j == r then dr else 0) + (if j == s then ds else 0))
                      in (sx + k * ex, sy + k * ey)
                    (x, y) = foldl sumUp (0 :: Double, 0 :: Double) kk
                in (realToFrac x, realToFrac y) :: Point
        ]
      isSquare t = tDir t == 2 || tDir t == 7   -- (0,2) と (1,3) は直交する = 正方形
      nSq = length (filter isSquare tiles)
  in w { wTiles = tiles, wSq = nSq, wRh = length tiles - nSq }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (concatMap tilePic (wTiles w) ++ gridLines ++ octagon ++ hud)
  where
    bigK = fromIntegral (wK w) :: Float
    edge = min (envW env) (envH env - 90) / (bigK * 1.85)
    cx = envW env / 2
    cy = envH env / 2 + 20
    place (x, y) = pt env (cx + x * edge * 0.5) (cy + y * edge * 0.5)
    outline = hsb 220 30 15
    tilePic t =
      let ps = map place [tA t, tB t, tC t, tD t]
          d = tDir t
          col | d == 2 || d == 7 = hsb 200 45 90
              | d == 1 || d == 11 = hsb 40 60 95
              | d == 6 = hsb 25 60 95
              | otherwise = hsb 50 60 95
      in [ Color col (Polygon ps), Color outline (Line (ps ++ take 1 ps)) ]
    -- 格子線（線 1 本 = タイルの帯 1 本、交点 1 つ = タイル 1 枚）
    gridLines
      | wGrid w =
          [ Color (hsba 330 80 100 0.5) (Line [pt env (px - ey * 2000) (py + ex * 2000), pt env (px + ey * 2000) (py - ex * 2000)])
          | (j, (dx, dy), g0) <- zip3 [0 :: Int ..] dirs gamma
          , let ex = realToFrac dx :: Float
          , let ey = realToFrac dy :: Float
          , k <- [-wK w .. wK w]
          , let d = realToFrac (fromIntegral k + offsetOf (wShift w) j g0) * edge
          , let px = cx + ex * d
          , let py = cy + ey * d ]
      | otherwise = []
    -- 八回対称の目印
    octagon
      | wOct w =
          let ps = [ pt env (cx + edge * 3 * cos a) (cy + edge * 3 * sin a)
                   | i <- [0 .. 8 :: Int], let a = fromIntegral i * pi / 4 + pi / 8 ]
          in [ Color (hsba 0 0 100 0.6) (thickPath 2 ps) ]
      | otherwise = []
    hud =
      [ Color (hsba 0 0 0 0.55) (box env 0 0 (envW env) 84)
      , txt env 20 32 15 white ("K = " ++ show (wK w) ++ " (Up/Dn)   shift " ++ nfD 2 (wShift w) ++ " (L/R, Space move " ++ onOff (wMoving w) ++ ")   G grid " ++ onOff (wGrid w) ++ "   O octagon " ++ onOff (wOct w))
      , txt env 20 62 14 (gray 0.8) ("Rhombi " ++ show (wRh w) ++ " / squares " ++ show (wSq w) ++ " = " ++ nfD 4 (fromIntegral (wRh w) / fromIntegral (max 1 (wSq w))) ++ " (-> sqrt2 = 1.4142). Inflation factor 1 + sqrt2.")
      ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _    -> build w { wK = min (wK w + 1) 14 }
  EventKey (SpecialKey KeyDown) Down _ _  -> build w { wK = max (wK w - 1) 3 }
  EventKey (SpecialKey KeyRight) Down _ _ -> build w { wShift = wShift w + 0.05 }
  EventKey (SpecialKey KeyLeft) Down _ _  -> build w { wShift = wShift w - 0.05 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wMoving = not (wMoving w) }
  EventKey (Char c) Down _ _
    | c == 'g' || c == 'G' -> w { wGrid = not (wGrid w) }
    | c == 'o' || c == 'O' -> w { wOct = not (wOct w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 正方形のまわりに 8 枚のひし形が集まった「八角形の星」を探そう
-- 2. スペースでずらすと、タイルがところどころでパタッと入れかわる（フェイゾンフリップ）
-- 3. gamma を全部 0.5 にすると？（線が 1 点で何本も交わってしまい、タイルが重なる）
-- 4. 方向を 5 本（pi / 5 ずつ）にすると、黄金比のペンローズ・タイリングになる（シリーズ 32 と比べよう）

-- ---------- 道具 ----------
onOff :: Bool -> String
onOff b = if b then "ON" else "off"

nfD :: Int -> Double -> String
nfD d v = showFFloat (Just d) v ""

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = Polygon [pt env x y, pt env (x + w) y, pt env (x + w) (y + h), pt env x (y + h)]
