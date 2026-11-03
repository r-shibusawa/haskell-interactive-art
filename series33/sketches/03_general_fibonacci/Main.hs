-- ============================================
-- 03 一般化フィボナッチ数列 ─ 比は φn に近づく
-- a(k+1) = n·a(k) + a(k−1)。n=1 フィボナッチ数、n=2 ペル数、n=3 …
-- となりどうしの比 a(k+1)/a(k) は、どんな出発点からでも φn に吸い寄せられる
-- ←→ = n / A = 出発点をランダムに / Z = 0, 1 に戻す / スペース = 一時停止 / ↑↓ = 速さ
-- ============================================
-- 項は Integer で計算するので、大きくなっても桁落ちしない。
module Main (main) where

import Art
import Numeric (showEFloat, showFFloat)

data World = World
  { wN      :: !Int                 -- ★ 漸化式の係数
  , wStart  :: !(Integer, Integer)  -- ★ はじめの 2 項
  , wSeq    :: [Integer]            -- 数列（maxK 項）
  , wShownK :: !Int                 -- いま何項目まで見えているか
  , wSpeed  :: !Int                 -- 何フレームごとに 1 項増やすか
  , wPaused :: !Bool
  , wTimer  :: !Int
  }

maxK :: Int
maxK = 24                           -- ★ 何項まで

initialWorld :: World
initialWorld = rebuild (World 1 (0, 1) [] 2 30 False 0)

main :: IO ()
main = runSketch (sketch "33-03 General Fibonacci" initialWorld)
  { skBack  = hsb 210 30 11
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> onEvent ev w
  }

-- 数列を作り直す
rebuild :: World -> World
rebuild w =
  let (a, b) = wStart w
      n = fromIntegral (wN w) :: Integer
      s = a : b : zipWith (\p q -> n * q + p) s (drop 1 s)
  in w { wSeq = take maxK s, wShownK = 2 }

stepWorld :: World -> World
stepWorld w
  | wPaused w = w
  | wTimer w + 1 > wSpeed w = w { wTimer = 0, wShownK = if wShownK w >= maxK then 2 else wShownK w + 1 }
  | otherwise = w { wTimer = wTimer w + 1 }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (hud ++ table ++ graph1 ++ graph2)
  where
    n = wN w
    phi = metallic (fromIntegral n)
    (s0, s1) = wStart w
    hud =
      [ txt env 20 24 15 white ("a(k+1) = " ++ show n ++ "*a(k) + a(k-1)   n = " ++ show n ++ " (L/R) " ++ seqName w ++ "   start " ++ show s0 ++ ", " ++ show s1)
      , txt env 20 46 14 (gray 0.75) "A random start   Z back to 0, 1   Up/Dn speed   Space pause"
      , txt env 20 68 14 (gray 0.75) ("The ratio approaches phi" ++ show n ++ " = " ++ nfD 8 phi ++ ". The error changes by about -1/phi^2 per term.")
      ]
    -- 表
    tx = 24
    ty = 110
    pairs = zip3 [0 :: Int ..] (wSeq w) (Nothing : map Just (wSeq w))
    rows = min (wShownK w) (floor ((envH env - 160) / 22))
    first = max 0 (wShownK w - rows)
    table =
      [ txt env tx ty 14 white "k", txt env (tx + 34) ty 14 white "a(k)"
      , txt env (tx + 366) ty 14 white "a(k)/a(k-1)", txt env (tx + 516) ty 14 white "- phi" ]
      ++ concat
        [ let yy = ty + 24 + fromIntegral (k - first) * 22
              col = if k == wShownK w - 1 then hsb 50 80 100 else gray 0.8
              ratioCols = case prev of
                Just p | p /= 0 ->
                  let r = fromIntegral a / fromIntegral p :: Double
                  in [ txt env (tx + 366) yy 14 col (nfD 8 r)
                     , txt env (tx + 516) yy 14 col (showEFloat (Just 1) (r - phi) "") ]
                _ -> []
          in txt env tx yy 14 col (show k) : txt env (tx + 34) yy 14 col (show a) : ratioCols
        | (k, a, prev) <- pairs, k >= first, k < wShownK w ]
    -- グラフ 1：比の収束
    gx = envW env * 0.53
    gy = 100
    gw = envW env * 0.45
    gh = (envH env - 160) * 0.5
    lo = phi - 1.2
    hi = phi + 1.2
    yOf :: Double -> Float
    yOf v = gy + gh - remap (realToFrac (max lo (min hi v))) (realToFrac lo) (realToFrac hi) 0 gh
    ratioPts = [ (gx + fromIntegral k * gw / fromIntegral maxK, yOf (fromIntegral a / fromIntegral p))
               | (k, a, Just p) <- pairs, k >= 2, k < wShownK w, p /= 0 ]
    graph1 =
      [ frameBox env gx gy gw gh ("Ratio a(k)/a(k-1) (dashed = phi" ++ show n ++ ")")
      , Color (hsb 50 90 100) (dashed (pt env gx (yOf phi)) (pt env (gx + gw) (yOf phi)))
      , Color (hsb 190 70 100) (thickPath 2 (pt env gx (yOf phi) : map (uncurry (pt env)) ratioPts))
      ]
      ++ [ Color (hsb 190 70 100) (at (pt env x y) (disc 3.5)) | (x, y) <- ratioPts ]
    -- グラフ 2：項の大きさ（対数）→ 直線 = φ^k で増える
    hy = gy + gh + 40
    lastA = case reverse (wSeq w) of
              (a : _) -> a
              [] -> 1
    maxLog = log (max 2 (fromIntegral (abs lastA))) :: Double
    graph2 =
      [ frameBox env gx hy gw (gh - 20) "log a(k): straight = times phi each step" ]
      ++ [ let v = log (max 1 (fromIntegral (abs a))) :: Double
               bh = realToFrac (v / maxLog) * (gh - 40)
           in Color (if odd k then hsb 25 60 95 else hsb 200 60 95)
                (box env (gx + fromIntegral k * gw / fromIntegral maxK + 2) (hy + gh - 20 - bh) (gw / fromIntegral maxK - 4) bh)
         | (k, a, _) <- pairs, k < wShownK w ]
      ++ [ txt env (gx + 10) (hy + 40) 14 (gray 0.7) ("Slope = log phi" ++ show n ++ " = " ++ nfD 4 (log phi)) ]

frameBox :: Env -> Float -> Float -> Float -> Float -> String -> Picture
frameBox env x y w h title = Pictures
  [ Color (withA 0.18 white) (Line [pt env x y, pt env (x + w) y, pt env (x + w) (y + h), pt env x (y + h), pt env x y])
  , txt env (x + 10) (y + 20) 14 (gray 0.8) title
  ]

seqName :: World -> String
seqName w = case (wStart w, wN w) of
  ((0, 1), 1) -> "Fibonacci"
  ((0, 1), 2) -> "Pell"
  ((2, 1), 1) -> "Lucas"
  _ -> ""

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (rebuild w { wN = min (wN w + 1) 8 })
  EventKey (SpecialKey KeyLeft) Down _ _  -> pure (rebuild w { wN = max (wN w - 1) 1 })
  EventKey (SpecialKey KeyUp) Down _ _    -> pure w { wSpeed = max 4 (wSpeed w - 6) }
  EventKey (SpecialKey KeyDown) Down _ _  -> pure w { wSpeed = min 90 (wSpeed w + 6) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c == 'a' || c == 'A' -> do
        a <- randomRIO (0, 19)
        b <- randomRIO (1, 19)
        pure (rebuild w { wStart = (a, b) })
    | c == 'z' || c == 'Z' -> pure (rebuild w { wStart = (0, 1) })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. A を何度も押して、どんな出発点でも同じ φ に近づくことを確かめよう
-- 2. wStart = (2, 1), n = 1 はリュカ数。フィボナッチ数とどちらが速く φ に近づく？
-- 3. ペル数（n = 2）で、(a(k) + a(k−1)) / a(k) が √2 に近づくのを表に列を足して確かめよう
-- 4. 漸化式を a(k+1) = a(k) + 2·a(k−1) に変えると比は何に近づく？（答え：2）

-- ---------- 道具 ----------
metallic :: Double -> Double
metallic k = (k + sqrt (k * k + 4)) / 2

nfD :: Int -> Double -> String
nfD d v = showFFloat (Just d) v ""

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = Polygon [pt env x y, pt env (x + w) y, pt env (x + w) (y + h), pt env x (y + h)]

-- 点線
dashed :: Point -> Point -> Picture
dashed a@(ax, ay) b@(bx, by) =
  let len = dist2 a b
      n = floor (len / 12) :: Int
      f t = (ax + (bx - ax) * t, ay + (by - ay) * t)
  in Pictures [ Line [f (fromIntegral i * 12 / len), f ((fromIntegral i * 12 + 6) / len)] | i <- [0 .. n], len > 0 ]
