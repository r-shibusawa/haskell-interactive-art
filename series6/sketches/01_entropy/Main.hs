-- ============================================
-- 01 エントロピー（情報の量を測る）
-- 「表が出る確率 p」のコインを投げた結果は、何ビットの情報か？
-- p = 0.5 なら 1 ビット（いちばん予測できない）。p = 1 なら 0 ビット（投げる前からわかる）
-- マウス左右 = p / クリックでコインを 100 回投げる
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Numeric (showFFloat)

newtype World = World { wResults :: [Bool] }   -- 投げた結果（True = 表）

tosses :: Int
tosses = 100              -- ★ 投げる回数

main :: IO ()
main = runSketch (sketch "06-01 Entropy" (World []))
  { skBack  = hsb 230 40 10
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ エントロピー H = −Σ p log2 p（単位：ビット）
entropy :: [Float] -> Float
entropy probs = negate (sum [q * logBase 2 q | q <- probs, q > 0])

-- マウスの左右で p を決める
probOf :: Env -> Float
probOf env = clampF 0 1 (fst (mouseXY env) / envW env)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ drawCurve env p h
  , drawCoins env p h (wResults w)
  , txt env 20 40 18 (hsb 0 0 100) ("p = " ++ nf 2 p ++ " (mouse X)   Entropy H = " ++ nf 3 h ++ " bits per toss")
  , txt env 20 68 15 (hsb 0 0 60) "H = -p log2 p - (1-p) log2 (1-p).  Less predictable = more info. All heads needs 0 bits."
  ]
  where
    p = probOf env
    h = entropy [p, 1 - p]

-- ---- 左：H(p) の曲線 ----
drawCurve :: Env -> Float -> Float -> Picture
drawCurve env p h = Pictures
  [ Color (hsb 0 0 30) (Pictures [Line [toG env left base, toG env right base], Line [toG env left base, toG env left top]])
  , Color (hsb 170 80 100) (thickPath 3 [curvePoint (fromIntegral i / 100) | i <- [0 .. 100 :: Int]])
  , Color (hsb 40 90 100) (at (toG env (qx p) (hy h)) (disc 9))
  , t (left - 10) (base + 24) "p = 0"
  , t ((left + right) / 2 - 10) (base + 24) "0.5"
  , t (right - 5) (base + 24) "1"
  , t (left - 70) (top + 5) "1 bit"
  , t (left - 20) (base + 5) "0"
  , t ((left + right) / 2 - 110) (base + 48) "Probability of heads p ->"
  ]
  where
    left = 80
    right = envW env * 0.55
    base = envH env * 0.8
    top = envH env * 0.2
    qx q = remap q 0 1 left right
    hy v = remap v 0 1 base top
    curvePoint q = toG env (qx q) (hy (entropy [q, 1 - q]))
    t x y = txt env x y 14 (hsb 0 0 60)

-- ---- 右：投げた結果 ----
drawCoins :: Env -> Float -> Float -> [Bool] -> Picture
drawCoins env _ h results = Pictures
  ( txt env rx (envH env * 0.2) 18 (hsb 0 0 100) "Coin x 100 (click)"
  : zipWith coin [0 :: Int ..] results
  ++ summary )
  where
    rx = envW env * 0.62
    stepX = min 28 ((envW env - rx - 30) / 20)     -- 20 個で 1 行。画面に収まる間隔
    top = envH env * 0.25
    coin i r =
      let x = rx + fromIntegral (i `mod` 20) * stepX
          y = top + fromIntegral (i `div` 20) * stepX
      in Color (if r then hsb 45 90 100 else hsb 230 30 40) (at (toG env x y) (disc (stepX / 2 - 1)))
    heads = length (filter id results)
    below = top + 5 * stepX + 10
    summary
      | null results = []
      | otherwise =
          [ txt env rx below 15 (hsb 0 0 60) ("Heads " ++ show heads ++ "   Tails " ++ show (tosses - heads))
          , txt env rx (below + 22) 15 (hsb 0 0 60) ("Telling the result takes about " ++ nf 0 (h * 100) ++ " bits")
          , txt env rx (below + 44) 15 (hsb 0 0 60) ("(not " ++ show tosses ++ " bits)")
          ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> do
    rs <- replicateM tosses (randomRIO (0, 1 :: Float))
    pure w { wResults = map (< probOf env) rs }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. p = 0.9 のとき、100 回の結果は何ビットで伝えられる？（裏の位置だけ教えればいい）
-- 2. サイコロ（6通り、全部 1/6）のエントロピーは？ entropy (replicate 6 (1/6))
-- 3. 日本語の文章は 1 文字あたり何ビット？（文字の出やすさに偏りがあるので、意外と少ない）

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 小数を d 桁で文字にする（p5.js の nf）
nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
