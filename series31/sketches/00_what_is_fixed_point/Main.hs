-- ============================================
-- 00 不動点とは
-- 写像 f で動かしても「動かない点」 f(x*) = x* が不動点。グラフでは y = f(x) と対角線 y = x の交点
--   右：上の線の点 x が下の線の f(x) へ移る矢印。まっすぐ下を向く矢印が不動点
-- ドラッグ = 制御点（白丸）を上下に動かして f の形を変える / 1〜4 = 形の見本 / マウスを左のグラフに乗せる = その x の行き先
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

data World = World
  { wYs    :: !(U.Vector Float)  -- ★ 制御点の高さ（x = 0, 1/6, …, 1 での f の値）
  , wDrag  :: !Int               -- つかんでいる制御点（-1 = なし）
  , wFixed :: ![Float]           -- 見つけた不動点
  }

samples :: Int
samples = 600                    -- ★ 交点を探すときの分割数

main :: IO ()
main = runSketch (sketch "31-00 What is a Fixed Point" (withYs preset1 (-1)))
  { skBack  = hsb 230 30 11
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

preset1, preset2, preset3 :: [Float]
preset1 = [0.75, 0.7, 0.6, 0.45, 0.35, 0.3, 0.2]
preset2 = [0.05, 0.08, 0.15, 0.5, 0.85, 0.92, 0.95]
preset3 = [0.3, 0.05, 0.6, 0.2, 0.9, 0.4, 0.95]

withYs :: [Float] -> Int -> World
withYs ys d = let v = U.fromList ys in World v d (findFixed v)

-- ★ 写像 f：制御点をなめらかにつなぐ曲線（Catmull-Rom）。値は [0, 1] に収める
fx :: U.Vector Float -> Float -> Float
fx ys x =
  let n  = U.length ys - 1
      u  = clampF 0 1 x * fromIntegral n
      i  = min (floor u) (n - 1)
      t  = u - fromIntegral i
      p0 = ys U.! max (i - 1) 0
      p1 = ys U.! i
      p2 = ys U.! (i + 1)
      p3 = ys U.! min (i + 2) n
      m1 = (p2 - p0) / 2
      m2 = (p3 - p1) / 2
      t2 = t * t
      t3 = t2 * t
      v  = (2 * t3 - 3 * t2 + 1) * p1 + (t3 - 2 * t2 + t) * m1 + (-2 * t3 + 3 * t2) * p2 + (t3 - t2) * m2
  in clampF 0 1 v

-- 不動点を探す：g(x) = f(x) − x の符号が変わる所を見つけて、二分法で絞り込む
findFixed :: U.Vector Float -> [Float]
findFixed ys = first ++ go 1 0 g0
  where
    gf x = fx ys x - x
    g0 = gf 0
    first = [0 | abs g0 < 1e-9]
    go s prevX prevG
      | s > samples = []
      | otherwise =
          let x = fromIntegral s / fromIntegral samples
              g = gf x
              rest = go (s + 1) x g
          in if (prevG > 0 && g <= 0) || (prevG < 0 && g >= 0)
               then bisect (40 :: Int) prevX x : rest
               else rest
    bisect 0 a b = (a + b) / 2
    bisect k a b =
      let m = (a + b) / 2
      in if gf a * gf m <= 0 then bisect (k - 1) a m else bisect (k - 1) m b

-- 画面のレイアウト
px0, py0 :: Float
px0 = 60
py0 = 110

sizeOf :: Env -> Float
sizeOf env = min (envW env * 0.44) (envH env - 190)

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $
  [ box (hsb 0 0 40) px0 py0 s s
  , ln 2 (hsba 0 0 70 0.7) (toP 0 0) (toP 1 1)
  , Color (hsb 190 70 100) (thickPath 3 [toP x (f x) | i <- [0 .. 200 :: Int], let x = fromIntegral i / 200])
  ]
  ++ [ spot (hsba 0 0 100 (if i == wDrag w then 1 else 0.7)) (toP x (ys U.! i)) 14
     | i <- [0 .. n], let x = fromIntegral i / fromIntegral n ]
  ++ concat [ [ln 1 (hsba 45 90 100 0.6) (toP x 0) (toP x x), spot (hsb 45 90 100) (toP x x) 16] | x <- wFixed w ]
  ++ hover
  ++ [ txt (px0 - 14) (py0 + s + 4) 14 c60 "0"
     , txt (px0 + s - 4) (py0 + s + 18) 14 c60 "1"
     , txt (px0 - 14) (py0 + 6) 14 c60 "1"
     , let (a, b) = toP 0.8 0.92 in txt a b 14 c60 "y = x"
     , let (a, b) = toP 0.02 (f 0.02) in txt a (b - 12) 14 (hsb 190 70 100) "y = f(x)"
     ]
  ++ drawArrows env w s
  ++ [ txt 20 34 16 white ("Fixed points f(x*) = x*: " ++ show (length (wFixed w)) ++ "    Drag: move a control point    1-4: presets    Hover: image of x")
     , txt 20 60 14 c60 "If f maps [0,1] into [0,1] continuously, the curve starts on or above the diagonal (f(0) >= 0) and ends"
     , txt 20 80 14 c60 "on or below it (f(1) <= 1), so it must cross the diagonal: there is at least one fixed point."
     ]
  ++ [ txt (px0 + s + 60) (py0 + s - 24 + fromIntegral i * 20) 14 (hsb 45 90 100) l | (i, l) <- zip [0 :: Int ..] fixedLines ]
  where
    ys = wYs w
    n = U.length ys - 1
    s = sizeOf env
    f = fx ys
    toP x y = (px0 + x * s, py0 + s - y * s)
    c60 = hsb 0 0 60
    fixedLines = case chunks 6 (map (nf 3) (wFixed w)) of
      [] -> ["x* = (none)"]
      (l : ls) -> ("x* = " ++ intercalate ",  " l) : map (intercalate ",  ") ls
    (mx, my) = mouseP env
    pink = hsb 320 70 100
    hover
      | mx > px0 && mx < px0 + s && my > py0 && my < py0 + s && wDrag w < 0 =
          let x = (mx - px0) / s
              y = f x
              (tx, ty) = toP y 0
              (bx, by) = toP x 0
          in [ ln 2 pink (toP x 0) (toP x y)
             , ln 2 pink (toP x y) (toP y y)
             , ln 2 pink (toP y y) (toP y 0)
             , spot pink (bx, by) 10
             , Color pink (Polygon [(tx - 6, ty - 8), (tx + 6, ty - 8), (tx, ty + 2)])
             , txt (min (bx + 8) (px0 + s - 200)) (by + 22) 14 pink ("x = " ++ nf 2 x ++ " -> f(x) = " ++ nf 2 y)
             ]
      | otherwise = []

-- 右側：矢印の図。上の線の x から下の線の f(x) へ
drawArrows :: Env -> World -> Float -> [Picture]
drawArrows env w s
  | x1 - x0 < 200 = []
  | otherwise =
      [ ln 2 (hsb 0 0 60) (x0, y1) (x1, y1)
      , ln 2 (hsb 0 0 60) (x0, y2) (x1, y2)
      , txt x0 (y1 - 12) 14 (hsb 0 0 70) "x (before)"
      , txt x0 (y2 + 26) 14 (hsb 0 0 70) "f(x) (after)"
      ]
      ++ [ ln 1.5 (arrowColor (y - x)) (lerpF x0 x1 x, y1) (lerpF x0 x1 y, y2)
         | i <- [0 .. arrows], let x = fromIntegral i / fromIntegral arrows, let y = fx (wYs w) x ]
      ++ [ ln 4 (hsb 45 90 100) (lerpF x0 x1 x, y1) (lerpF x0 x1 x, y2) | x <- wFixed w ]
      ++ [ txt x0 (y2 + 52) 14 (hsb 190 60 100) "Cyan = moves right (f(x) > x)"
         , txt x0 (y2 + 72) 14 (hsb 330 60 100) "Pink = moves left (f(x) < x)"
         , txt x0 (y2 + 92) 14 (hsb 45 90 100) "Yellow = does not move"
         , txt x0 (y2 + 116) 14 (hsb 0 0 60) "The left end can only move right, the right end only"
         , txt x0 (y2 + 136) 14 (hsb 0 0 60) "left: somewhere in between, a point does not move."
         ]
  where
    x0 = px0 + s + 60
    x1 = envW env - 60
    y1 = py0 + 40
    y2 = py0 + s * 0.62
    arrows = 28 :: Int
    arrowColor g
      | abs g < 0.02 = hsb 45 90 100        -- ほとんど動かない点は黄色
      | g > 0 = hsba 190 60 100 0.7         -- 右へ動く点は水色
      | otherwise = hsba 330 60 100 0.7     -- 左へ動く点は桃色

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let s = sizeOf env
        m = mouseP env
        n = U.length (wYs w) - 1
        toP i = (px0 + fromIntegral i / fromIntegral n * s, py0 + s - (wYs w U.! i) * s)
        hits = [i | i <- [0 .. n], dist2 m (toP i) < 16]
    in pure w { wDrag = if null hits then -1 else last hits }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 ->
    let s = sizeOf env
        (_, my) = mouseP env
        v = clampF 0 1 ((py0 + s - my) / s)
        ys = wYs w U.// [(wDrag w, v)]
    in pure w { wYs = ys, wFixed = findFixed ys }
  EventKey (Char '1') Down _ _ -> pure (withYs preset1 (wDrag w))
  EventKey (Char '2') Down _ _ -> pure (withYs preset2 (wDrag w))
  EventKey (Char '3') Down _ _ -> pure (withYs preset3 (wDrag w))
  EventKey (Char '4') Down _ _ -> do
    ys <- replicateM (U.length (wYs w)) (randomRIO (0, 1))
    pure (withYs ys (wDrag w))
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 不動点を 0 個にできる？ 制御点をどう動かしても対角線をまたいでしまう理由を言葉で説明しよう
-- 2. 見本 2 は不動点が 3 つ。真ん中の不動点と両端の不動点は何が違う？（01 のクモの巣図で確かめる）
-- 3. fx の最後の clampF 0 1 v を外して、値を 1.2 まで出せるようにすると不動点が消せるか
-- 4. preset1 の数を 7 → 12 に増やして、不動点が 7 つある f を作ろう

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5 と同じ座標（左上が原点、y は下向き）で描くための枠
screen :: Env -> [Picture] -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 文字（y 軸を反転してあるので、文字だけ元の向きに戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = Translate x y (Scale 1 (-1) (label 0 0 s c str))

mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

ln :: Float -> Color -> Point -> Point -> Picture
ln w c a b = Color c (thickLine w a b)

-- 塗りつぶした円（d は直径）
spot :: Color -> Point -> Float -> Picture
spot c (x, y) d = Color c (Translate x y (disc (d / 2)))

-- 左上 (x, y)、幅 w、高さ h の枠
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

nf :: RealFloat a => Int -> a -> String
nf d v = showFFloat (Just d) v ""

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks k xs = let (a, b) = splitAt k xs in a : chunks k b
