-- ============================================
-- 06 中間値の定理と 1 次元のブラウワー
-- [0,1] を [0,1] の中へ「ペンを離さずに」写す f は必ず不動点を持つ：g(x) = f(x) − x は g(0) ≥ 0、g(1) ≤ 0 → 途中で 0（中間値の定理）
--   証明がそのまま探し方になる：真ん中の符号を見て半分を捨てる（二分法）。1 回で誤差が半分
-- ドラッグ = 曲線を描く（左から右へ）/ スペース = 二分法を 1 回 / A = 自動 / J = ペンを離すと「跳ぶ」/ R = ランダムな曲線 / C = 消す
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import Numeric (showEFloat, showFFloat)

nb :: Int
nb = 240                      -- ★ 曲線を何本の縦の区間で持つか

empty' :: Double
empty' = -1                   -- まだ描いていない区間のしるし

-- 二分法の 1 回分の記録（桁を見るので計算は Double で行う）
data Step = Step { hA, hB, hM, hG :: !Double }

data World = World
  { wVals :: !(U.Vector Double)   -- 描いた値（未定義は empty'）
  , wFv   :: !(U.Vector Double)   -- すき間を埋めた値
  , wJump :: !Bool                -- True：ペンを離した所はつながずに跳ぶ（連続でない）
  , wA    :: !Double
  , wB    :: !Double
  , wHist :: ![Step]
  , wAuto :: !Bool
  , wLast :: !Int
  }

main :: IO ()
main = runSketch (sketch "31-06 IVT and Bisection" world0)
  { skBack  = hsb 230 30 11
  , skStart = \_ -> randomCurve world0
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = rebuild (World (U.replicate nb empty') U.empty False 0 1 [] False (-1))

randomCurve :: World -> IO World
randomCurve w = do
  p0 <- randomRIO (0, 2 * pi)
  p1 <- randomRIO (0, 2 * pi)
  k0 <- randomRIO (2, 5)
  k1 <- randomRIO (5, 11)
  let val i = let x = fromIntegral i / fromIntegral (nb - 1)
              in clampD 0 1 (0.5 + 0.3 * sin (k0 * x + p0) + 0.15 * sin (k1 * x + p1))
  pure (rebuild w { wVals = U.generate nb val })

-- すき間を埋める：ふつうは直線でつなぐ（連続）、J のときは左の値のまま（跳ぶ）
rebuild :: World -> World
rebuild w =
  let vals = wVals w
      idx = [i | i <- [0 .. nb - 1], vals U.! i /= empty']
      fill i
        | vals U.! i /= empty' = vals U.! i
        | otherwise =
            case (takeWhile (< i) idx, dropWhile (<= i) idx) of
              ([], r : _) -> vals U.! r
              (ls@(_ : _), []) -> vals U.! last ls
              (ls@(_ : _), r : _) ->
                let l = last ls
                in if wJump w then vals U.! l
                   else lerpD (vals U.! l) (vals U.! r) (fromIntegral (i - l) / fromIntegral (r - l))
              ([], []) -> 0.5
  in w { wFv = U.generate nb fill, wA = 0, wB = 1, wHist = [] }

-- ★ f(x)：連続モードは直線でつなぐ、跳ぶモードは区間ごとに一定の値
fv :: World -> Double -> Double
fv w x =
  let u = clampD 0 1 x * fromIntegral (nb - 1)
  in if wJump w
       then wFv w U.! floor (u + 0.5)
       else let i = min (floor u) (nb - 2)
            in lerpD (wFv w U.! i) (wFv w U.! (i + 1)) (u - fromIntegral i)

gOf :: World -> Double -> Double
gOf w x = fv w x - x

bisect :: World -> World
bisect w
  | b - a < 1e-12 = w
  | gm == 0 = w' { wA = m, wB = m }
  | gOf w a * gm < 0 = w' { wB = m }     -- 符号が変わる側の半分を残す
  | otherwise = w' { wA = m }
  where
    a = wA w
    b = wB w
    m = (a + b) / 2
    gm = gOf w m
    w' = w { wHist = wHist w ++ [Step a b m gm] }

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | wAuto w && envFrame env `mod` 30 == 0 && length (wHist w) < 30 = pure (bisect w)
  | otherwise = pure w

-- レイアウト
gx, gy :: Float
gx = 60
gy = 100

sizeOf :: Env -> Float
sizeOf env = min (envW env * 0.46) (envH env - 170)

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $
  -- 残っている区間 [a, b] を帯で
  [ fillBox (hsba 45 90 100 0.18) (xs (wA w)) gy (max 2 (realToFrac (wB w - wA w) * s)) s
  , box (hsb 0 0 40) gx gy s s
  , ln 2 (hsba 0 0 70 0.7) (xs 0, ys 0) (xs 1, ys 1)
  ]
  -- 曲線（g の符号で色分け：上 = f(x) > x、下 = f(x) < x）
  ++ [ ln 3 (hsb (if gOf w x1 >= 0 then 190 else 330) 70 100) (xs x1, ys (fv w x1)) (xs x2, ys (fv w x2))
     | i <- [0 .. 399 :: Int], let x1 = fromIntegral i / 400, let x2 = fromIntegral (i + 1) / 400 ]
  ++ [ spot (hsb 190 70 100) (xs 0, ys (fv w 0)) 10
     , spot (hsb 330 70 100) (xs 1, ys (fv w 1)) 10 ]
  -- 二分法の点
  ++ [ Color (hsba 0 0 100 ((30 + 70 * fromIntegral i / fromIntegral hn) / 100)) (Line [(xs (hM h), ys 0), (xs (hM h), ys (fv w (hM h)))])
     | (i, h) <- zip [0 :: Int ..] hist ]
  ++ [ spot (hsb 45 90 100) (xs m, ys (fv w m)) 12
     , txt (gx - 14) (gy + s + 4) 14 (hsb 0 0 60) "0"
     , txt (gx + s - 4) (gy + s + 18) 14 (hsb 0 0 60) "1"
     , txt (gx - 14) (gy + 6) 14 (hsb 0 0 60) "1"
     -- 右：二分法の表
     , txt tx ty 15 white "n", txt (tx + 40) ty 15 white "a", txt (tx + 170) ty 15 white "b", txt (tx + 300) ty 15 white "g(mid)"
     ]
  ++ concat [ let y = ty + 24 + fromIntegral k * 21
                  c = hsb (if hG h >= 0 then 190 else 330) 60 100
              in [ txt tx y 14 c (show (i + 1)), txt (tx + 40) y 14 c (nf 8 (hA h)), txt (tx + 170) y 14 c (nf 8 (hB h))
                 , txt (tx + 300) y 14 c ((if hG h >= 0 then "+" else "-") ++ nf 6 (abs (hG h))) ]
            | (k, (i, h)) <- zip [0 :: Int ..] (drop n0 (zip [0 :: Int ..] hist)) ]
  ++ [ txt tx by 15 white ("width b - a = 2^-n = " ++ showEFloat (Just 2) (wB w - wA w) "" ++ "  -> mid " ++ nf 8 m) ]
  ++ [ txt tx (by + 26 + fromIntegral i * 20) 14 (hsb (if stuck then 0 else 140) 80 100) l | (i, l) <- zip [0 :: Int ..] status ]
  ++ [ txt tx (by + 76) 14 (hsb 0 0 60) "Cyan = f(x) > x (moves right), pink = f(x) < x (moves left)."
     , txt tx (by + 96) 14 (hsb 0 0 60) "The left end is always cyan, the right end always pink."
     , txt tx (by + 116) 14 (hsb 0 0 60) "Without lifting the pen, going from cyan to pink must pass"
     , txt tx (by + 136) 14 (hsb 0 0 60) "through the diagonal (f(x) = x)."
     , txt 20 34 15 white ("Drag: draw   Space: bisect   A: auto " ++ (if wAuto w then "ON" else "off") ++ "   J: jump mode " ++ (if wJump w then "ON (discontinuous)" else "off (continuous)") ++ "   R: random   C: clear   n = " ++ show hn)
     , txt 20 60 14 (hsb 0 0 60) "The curve cannot leave the 0-1 box (f: [0,1] -> [0,1]). Turn J on and lift the pen while drawing to make an f with no fixed point."
     ]
  where
    s = sizeOf env
    xs :: Double -> Float
    xs x = gx + realToFrac x * s
    ys :: Double -> Float
    ys y = gy + s - realToFrac y * s
    hist = wHist w
    hn = length hist
    n0 = max 0 (hn - 14)
    m = (wA w + wB w) / 2
    tx = gx + s + 50
    ty = gy + 10
    by = ty + 24 + 14 * 21 + 20
    stuck = (wB w - wA w) < 1e-5 && abs (gOf w m) > 0.01
    status
      | stuck = ["The interval shrank but |g| does not reach 0:", "we found a jump, not a fixed point."]
      | hn > 0 = ["|g(mid)| = " ++ nf 8 (abs (gOf w m)), "(each step halves the width; 10 steps: about 1/1000)"]
      | otherwise = ["|g(mid)| = " ++ nf 8 (abs (gOf w m))]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (addPoint env w { wLast = -1 })
  EventMotion _ | envDown env -> pure (addPoint env w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (bisect w)
  EventKey (Char c) Down _ _
    | toLower c == 'a' -> pure w { wAuto = not (wAuto w) }
    | toLower c == 'j' -> pure (rebuild w { wJump = not (wJump w) })
    | toLower c == 'r' -> randomCurve w
    | toLower c == 'c' -> pure (rebuild w { wVals = U.replicate nb empty' })
  _ -> pure w

addPoint :: Env -> World -> World
addPoint env w
  | mx < gx - 10 || mx > gx + s + 10 || my < gy - 30 || my > gy + s + 30 = w
  | otherwise =
      let i = floor (clampF 0 1 ((mx - gx) / s) * fromIntegral (nb - 1) + 0.5) :: Int
          v = realToFrac (clampF 0 1 ((gy + s - my) / s)) :: Double
          l = wLast w
          -- ドラッグ中はすき間を直線で埋める（ペンを離していない）
          between
            | l >= 0 && l /= i =
                let v0 = wVals w U.! l
                    s0 = signum (i - l)
                in [ (j, lerpD v0 v (fromIntegral (j - l) / fromIntegral (i - l))) | j <- [l + s0, l + 2 * s0 .. i - s0] ]
            | otherwise = []
      in rebuild w { wVals = wVals w U.// (between ++ [(i, v)]), wLast = i }
  where
    (mx, my) = mouseP env
    s = sizeOf env

-- ---------- やってみよう ----------
-- 1. C で消してから、対角線を 5 回またぐ曲線を描こう。二分法はどの不動点にたどり着く？ それはなぜ？
-- 2. J をオンにして、左半分は上、右半分は下に「ペンを離して」描く。対角線に触れない f を作れたら二分法を走らせる
-- 3. 10 回で幅は 1/1024、20 回で約 100 万分の 1。小数 6 桁の精度には何回要る？
-- 4. 2 次元（07）では「符号」の代わりに何を数えればよいだろう？（ヒント：回る回数）

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

-- 輪（d は直径、w は太さ）
hoop :: Float -> Color -> Point -> Float -> Picture
hoop w c (x, y) d = Color c (Translate x y (ring (d / 2) w))

-- 左上 (x, y)、幅 w、高さ h の枠
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし
fillBox :: Color -> Float -> Float -> Float -> Float -> Picture
fillBox c x y w h = Color c (rect (x + w / 2) (y + h / 2) w h)

nf :: RealFloat a => Int -> a -> String
nf d v = showFFloat (Just d) v ""

clampD :: Double -> Double -> Double -> Double
clampD lo hi = max lo . min hi

lerpD :: Double -> Double -> Double -> Double
lerpD a b t = a + (b - a) * t
