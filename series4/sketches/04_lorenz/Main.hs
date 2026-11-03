-- ============================================
-- 04 ローレンツ・アトラクター（カオスの蝶）
-- 気象学者ローレンツが天気の式を3つに単純化したら、
-- 「決まった形」の上を「二度と同じ道を通らず」まわり続ける軌道が現れた
-- マウスで回転 / クリックで点を追加（わずかにずれた場所から）/ C で消す
-- ============================================
module Main (main) where

import Art

-- 軌跡の 1 点
data P3 = P3 !Float !Float !Float

-- 動く点。わずかな差を追うので Double で計算する
data Mover = Mover
  { mX, mY, mZ :: !Double
  , mHue       :: !Float
  , mTrail     :: ![P3]    -- 軌跡（新しいものが先頭）
  }

newtype World = World { wMovers :: [Mover] }

sigma, rho, beta :: Double
sigma = 10                  -- ★ ローレンツの3つの定数
rho = 28
beta = 8 / 3

trailMax :: Int
trailMax = 3000             -- ★ 1つの点の軌跡の長さ

drawBudget :: Int
drawBudget = 7000           -- ★ 1フレームに描く線分の上限（点が増えたら間引く）

maxMovers :: Int
maxMovers = 12              -- ★ 点の数の上限

main :: IO ()
main = runSketch (sketch "04-04 Lorenz Attractor" (World [Mover 0.1 0 0 180 []]))
  { skBack  = hsb 240 40 8
  , skStep  = \_ _ w -> pure w { wMovers = map advance (wMovers w) }
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ ローレンツ方程式：3つの数が、おたがいを変えあう
lorenz :: Mover -> Mover
lorenz m = point `seq` m { mX = nx, mY = ny, mZ = nz, mTrail = point : mTrail m }
  where
    dt = 0.005
    x = mX m; y = mY m; z = mZ m
    dx = sigma * (y - x)
    dy = x * (rho - z) - y
    dz = x * y - beta * z
    nx = x + dx * dt
    ny = y + dy * dt
    nz = z + dz * dt
    point = P3 (toF nx) (toF ny) (toF nz)

advance :: Mover -> Mover
advance m0 =
  let m = lorenz (lorenz (lorenz m0))        -- 1フレームに 3 ステップ
  in m { mTrail = take trailMax (mTrail m) }

-- 3D の点 → 回転 → 画面の 2D 座標（画面の中央が原点）
toScreen :: Float -> Float -> P3 -> Point
toScreen ry rx (P3 x y z) = (x1, z2 - 20)
  where
    -- 中心を合わせて縮尺
    xx = x * 9; yy = y * 9; zz = (z - 25) * 9
    -- Y軸回転
    x1 = xx * cos ry - yy * sin ry
    y1 = xx * sin ry + yy * cos ry
    -- X軸回転
    z2 = y1 * sin rx + zz * cos rx

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  ( map drawMover (wMovers w) ++
  [ txt env 20 40 18 white
      ("Mouse: rotate   Click: add a point (tiny offset)   C: clear   Points " ++ show (length (wMovers w)))
  , txt env 20 66 14 (hsb 0 0 60)
      "dx = sigma(y-x)   dy = x(rho-z) - y   dz = xy - beta z    sigma=10, rho=28, beta=8/3"
  ])
  where
    (mx, my) = mouseP5 env
    ry = remap mx 0 (envW env) (-pi) pi
    rx = remap my 0 (envH env) (-0.8) 0.8
    total = sum (map (length . mTrail) (wMovers w))
    stride = max 1 ((total + drawBudget - 1) `div` drawBudget)   -- 間引く間隔
    drawMover m =
      let pts = map (toScreen ry rx) (reverse (everyNth stride (mTrail m)))
          n = length pts
          seg i a b = Color (hsba (mHue m) 70 100 (remap (fromIntegral i) 0 (fromIntegral n) 5 100 / 100))
                            (thickLine 1.5 a b)
      in Pictures
           [ Pictures (zipWith3 seg [0 :: Int ..] pts (drop 1 pts))
           , Color (hsb (mHue m) 60 100)
               (at (toScreen ry rx (P3 (toF (mX m)) (toF (mY m)) (toF (mZ m)))) (disc 5)) ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | snd (mouseP5 env) >= 50 && length (wMovers w) < maxMovers ->
        pure $ case reverse (wMovers w) of
          (b : _) -> w { wMovers = wMovers w ++
                          [Mover (mX b + 0.0001) (mY b) (mZ b) (wrapF 360 (mHue b + 70)) []] }
          [] -> w
  EventKey (Char c) Down _ _ | c == 'c' || c == 'C' -> pure w { wMovers = take 1 (wMovers w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. クリックして2つ目の点を足す。最初はぴったり重なるのに、やがて別の羽根へ飛ぶ
-- 2. rho（28）を 15 にすると、カオスではなくなり1点に落ち着く。 99 にすると？
-- 3. 軌跡の色を z の高さで変えてみよう

-- ---------- 道具 ----------

-- リストを n 個おきに取り出す
everyNth :: Int -> [a] -> [a]
everyNth n xs = case xs of
  [] -> []
  (y : _) -> y : everyNth n (drop n xs)

toF :: Double -> Float
toF = realToFrac

-- p5.js 風の座標（左上が原点、y は下向き）を gloss の座標に直す
sc :: Env -> Float -> Float -> Point
sc env x y = (x - envW env / 2, envH env / 2 - y)

mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size col s = let (gx, gy) = sc env x y in label gx gy size col s
