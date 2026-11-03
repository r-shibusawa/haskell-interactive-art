-- ============================================
-- 10 回転数と代数学の基本定理
-- 多項式 p(z) = (z − α₁)(z − α₂)… に、半径 r の円 |z| = r をまるごと入れると、右に閉じた曲線ができる
--   その曲線が 0 のまわりを回る回数 ＝ 円の内側にある根の数。r を大きくすると必ず n 回 → n 次式には根が n 個ある（代数学の基本定理）
-- ↑↓ = 円の半径 r / 根をドラッグ = 動かす / N = 根を足す / D = 根を減らす / スペース = r を自動で動かす
-- ============================================
module Main (main) where

import Art
import Data.List (findIndex, foldl')
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

mM :: Int
mM = 500             -- ★ 円の上の点の数

width, height, zs, rW, zcx, zcy, wcx, wcy :: Float
width = 1280
height = 800
zs = min (width * 0.2) (height * 0.32) / 1.25
rW = min (width * 0.22) (height * 0.34)
zcx = width * 0.26
zcy = height * 0.56
wcx = width * 0.72
wcy = height * 0.56

type P = (Float, Float)

data World = World
  { wRoots :: ![P]
  , wR :: !Float      -- ★ 円の半径
  , wSweep :: !Bool
  , wSweepT :: !Float
  , wDrag :: !Int
  }

main :: IO ()
main = runSketch (sketch "27-10 Winding Number and the Fundamental Theorem of Algebra" world0)
  { skBack  = hsb 215 30 12
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = \env w -> drawWorld (envFrame env) w
  , skEvent = onEvent
  }

world0 :: World
world0 = World [(-1.0, 0.35), (0.75, 0.6), (0.25, -0.9)] 1 False 0 (-1)

-- | ★ p(z) = Π (z − α_k)（複素数のかけ算）
polyAt :: [P] -> Float -> Float -> P
polyAt roots x y =
  foldl' (\(re, im) (ax, ay) ->
            let dx = x - ax
                dy = y - ay
            in (re * dx - im * dy, re * dy + im * dx)) (1, 0) roots

---------------------------------------------------------------- 動き

stepWorld :: Env -> World -> World
stepWorld env w0 =
  let w1 = if wSweep w0
             then let t = wSweepT w0 + 0.006 in w0 { wSweepT = t, wR = 1.25 + 1.15 * sin (t - pi / 2) }
             else w0
      r1 = if isDown env (SpecialKey KeyUp) then min 2.4 (wR w1 + 0.01) else wR w1
      r2 = if isDown env (SpecialKey KeyDown) then max 0.05 (r1 - 0.01) else r1
      (mx, my) = toP5 (envMouse env)
      moved = (clampF (-2.2) 2.2 ((mx - zcx) / zs), clampF (-2.2) 2.2 (-(my - zcy) / zs))
      roots | wDrag w1 >= 0 = [if i == wDrag w1 then moved else a | (i, a) <- zip [0 ..] (wRoots w1)]
            | otherwise = wRoots w1
  in w1 { wR = r2, wRoots = roots }

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let m = toP5 pos
        near (ax, ay) = dist2 (zcx + ax * zs, zcy - ay * zs) m < 16
    in pure w { wDrag = maybe (-1) id (findIndex near (wRoots w)) }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventKey (Char c) Down _ _
    | c `elem` "nN" && length (wRoots w) < 7 -> do
        x <- randomRIO (-1.5, 1.5)
        y <- randomRIO (-1.5, 1.5)
        pure w { wRoots = wRoots w ++ [(x, y)] }
    | c `elem` "dD" && length (wRoots w) > 1 -> pure w { wRoots = take (length (wRoots w) - 1) (wRoots w), wDrag = -1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wSweep = not (wSweep w) }
  _ -> pure w

---------------------------------------------------------------- 描画

drawWorld :: Int -> World -> Picture
drawWorld frame w =
  let r = wR w
      roots = wRoots w
      fM = fromIntegral mM :: Float
      -- 円の上の点と、その行き先 w = p(z)
      ws = U.generate (mM + 1) (\i -> let th = fromIntegral i / fM * tau in polyAt roots (r * cos th) (r * sin th))
      kK = U.sum (U.map norm2 ws) / (fM + 1) + 1e-9
      -- ★ 回転数：w の向き（偏角）の変化を合計して 2π で割る
      delta (ar, ai) (br, bi) =
        let d0 = atan2 bi br - atan2 ai ar
            d1 = if d0 > pi then d0 - tau else d0
        in if d1 < -pi then d1 + tau else d1
      total = U.sum (U.zipWith delta ws (U.drop 1 ws))
      wind = round (total / tau) :: Int
      inside = length (filter (\a -> norm2 a < r) roots)
      -- 左：z の平面
      zPt a = pt (zcx + r * zs * cos a, zcy - r * zs * sin a)
      circleSeg i =
        let a = fromIntegral i / fM * tau
            b = fromIntegral (i + 2) / fM * tau
        in Color (hsb (fromIntegral i / fM * 360) 70 100) (thickLine 5 (zPt a) (zPt b))
      rootPic a@(ax, ay) =
        let isIn = norm2 a < r
        in at (pt (zcx + ax * zs, zcy - ay * zs)) (Color (if isIn then hsb 50 90 100 else gray 1) (disc 9))
      -- 右：w = p(z) の平面（遠くを縮めて表示：w / (|w| + K)）
      toW (re, im) =
        let s = rW / (sqrt (re * re + im * im) + kK)
        in (wcx + re * s, wcy - im * s)
      wSeg i =
        Color (hsb (fromIntegral i / fM * 360) 70 100)
          (thickLine 3 (pt (toW (ws U.! i))) (pt (toW (ws U.! (i + 1)))))
      -- 動く目印
      k = (frame * 2) `mod` mM
      th = fromIntegral k / fM * tau
      mw = toW (ws U.! k)
  in Pictures $
       [ axes zcx zcy (zs * 2.2) "z plane"
       , axes wcx wcy (rW * 1.1) "w = p(z) plane" ]
       ++ map circleSeg [0, 2 .. mM - 1]
       ++ map rootPic roots
       ++ map wSeg [0 .. mM - 1]
       ++ [ at (pt (wcx, wcy)) (Color (gray 1) (disc 6))
          , at (zPt th) (Color (gray 1) (disc 7))
          , at (pt mw) (Color (gray 1) (disc 7))
          , Color (hsba 0 0 100 0.5) (Line [pt (wcx, wcy), pt mw])
          , txt 20 (height - 70) 22 (gray 1)
              (printf "r = %.2f   Turns around 0 = %d   Roots inside the circle = %d" r wind inside)
          , txt 20 (height - 38) 16 (hsb (if wind == inside then 130 else 0) 60 100)
              (if wind == inside then "They match! (argument principle)" else "The circle is passing right over a root")
          , txt (width * 0.45) (height - 38) 14 (gray 0.7)
              ("Degree n = " ++ show (length roots) ++ ". Large r gives n turns, because p(z) is about z^n")
          , txt 20 40 16 (gray 1) ("Up/Down: radius r   Drag a root   N: add root   D: remove root   Space: auto r " ++ (if wSweep w then "ON" else "off"))
          , txt 20 68 14 (gray 0.6) "Circle color matches the right curve. Yellow = root inside. When the circle crosses a root, the curve passes 0."
          ]

-- | 座標軸（文字が画面の外や説明文の下に入らない位置に置く）
axes :: Float -> Float -> Float -> String -> Picture
axes cx cy l title =
  let top = max 90 (cy - l)
      bottom = min (height - 90) (cy + l)
      left = max 0 (cx - l)
      right = min width (cx + l)
  in Pictures
       [ Color (hsb 0 0 40) (Line [pt (left, cy), pt (right, cy)])
       , Color (hsb 0 0 40) (Line [pt (cx, top), pt (cx, bottom)])
       , txt (max 20 (cx - l)) (top + 14) 15 (gray 0.6) title ]

-- ---------- やってみよう ----------
-- 1. r をゆっくり大きくして、根を 1 つ追い越すたびに回る回数が 1 ずつ増えるのを確かめよう
-- 2. 根を 2 つ重ねる（重根）と、右の曲線はどうなる？
-- 3. polyAt を p(z) = z² + 1 のように係数で書きかえてみよう。根は ±i（図の上下）にある？
-- 4. 「円を大きくすると n 回、小さくすると 0 回 → 途中で必ず 0 を通る」。これが代数学の基本定理の証明のアイデア

-- ==================================================
-- ここから下は「道具」（座標の変換）
-- ==================================================

pt :: P -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

toP5 :: Point -> P
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = let (gx, gy) = pt (x, y) in label gx gy s c str
