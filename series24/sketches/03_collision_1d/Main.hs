-- ============================================
-- 03 運動量保存と衝突
-- ぶつかっても運動量 p = m₁v₁ + m₂v₂ の合計は変わらない。運動エネルギーは反発係数 e しだいで減る
--   e = 1 弾性衝突、e = 0 くっつく（完全非弾性）。N でニュートンのゆりかご
-- ←→ = 反発係数 e / ↑↓ = 質量比 m₂/m₁ / スペース = もう一度 / N = ゆりかご / 1〜4 = 持ち上げる玉の数
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Text.Printf (printf)

v1start :: Float
v1start = 1.5     -- ★ 台車 1 の初速 [m/s]（台車 2 は止まっている）

histLen :: Int
histLen = 600

dt :: Float
dt = 1 / 60

data Cart = Cart { cx, cv, cm, cw :: !Float }
data Ball = Ball { bTh, bOm :: !Float }

data World = World
  { wE      :: !Float       -- ★ 反発係数（0〜1）
  , wRatio  :: !Float       -- ★ 質量比 m₂ / m₁
  , wCradle :: !Bool
  , wA, wB  :: !Cart
  , wBalls  :: ![Ball]
  , wLift   :: !Int
  , wSimT   :: !Float
  , hP, hK, hV1, hV2 :: !(Seq.Seq Float)
  }

main :: IO ()
main = runSketch (sketch "24-03 Momentum and Collisions" world0)
  { skBack  = hsb 250 25 11
  , skStep  = \_ _ w -> pure (if wCradle w then stepCradle w else stepCarts w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = restart (World 1 1 False c c [] 1 0 Seq.empty Seq.empty Seq.empty Seq.empty)
  where c = Cart 0 0 1 0.3

restart :: World -> World
restart w = w
  { wA = Cart (-1.6) v1start 1 0.3
  , wB = Cart 0.2 0 (wRatio w) (0.3 * sqrt (sqrt (wRatio w)))
  , wBalls = [Ball (if i < wLift w then -0.6 else 0) 0 | i <- [0 .. 4]]
  , wSimT = 0 }

-- ★ 1 次元の衝突（反発係数 e）：運動量保存 + 「離れる速さ = e × 近づく速さ」
collide :: Float -> (Float, Float) -> (Float, Float) -> (Float, Float)
collide e (ma, va) (mb, vb) =
  let p = ma * va + mb * vb
      m = ma + mb
  in ((p + mb * e * (vb - va)) / m, (p + ma * e * (va - vb)) / m)

stepCarts :: World -> World
stepCarts w0' =
  let sub :: Int -> Cart -> Cart -> (Cart, Cart)
      sub 0 a b = (a, b)
      sub n a b =
        let a1 = a { cx = cx a + cv a * dt / 4 }
            b1 = b { cx = cx b + cv b * dt / 4 }
        in if cx b1 - cx a1 < (cw a1 + cw b1) / 2 && cv a1 > cv b1
             then let (va, vb) = collide (wE w0') (cm a1, cv a1) (cm b1, cv b1)
                  in sub (n - 1) a1 { cv = va } b1 { cv = vb }
             else sub (n - 1) a1 b1
      (a2, b2) = sub 4 (wA w0') (wB w0')
      w1 = w0' { wA = a2, wB = b2, wSimT = wSimT w0' + dt }
      w = if abs (cx a2) > 2.6 || abs (cx b2) > 2.6 || wSimT w1 > 12 then restart w1 else w1
      a = wA w
      b = wB w
      p = cm a * cv a + cm b * cv b
      k = 0.5 * cm a * cv a * cv a + 0.5 * cm b * cv b * cv b
  in w { hP = pushHist (hP w) p, hK = pushHist (hK w) k, hV1 = pushHist (hV1 w) (cv a), hV2 = pushHist (hV2 w) (cv b) }

-- ---- ニュートンのゆりかご：5 つの振り子、触れたら 1 次元の衝突
lp, rB, topY, cxC, gL :: Float
lp = height * 0.32
rB = height * 0.04
topY = height * 0.2
cxC = width * 0.25
gL = 9.8 / (lp / 200)   -- g / L（見た目の長さに合わせる）

stepCradle :: World -> World
stepCradle w =
  let h = dt / 20
      swing b = let om = bOm b - gL * sin (bTh b) * h in Ball (bTh b + om * h) om
      -- 隣どうしが重なって近づいていたら衝突（何回か繰り返して伝える）
      pass :: Int -> [Ball] -> [Ball]
      pass i (a : b : rest) =
        let xa = (fromIntegral i - 2) * 2 * rB + lp * sin (bTh a)
            xb = (fromIntegral i - 1) * 2 * rB + lp * sin (bTh b)
        in if xb - xa < 2 * rB + 0.5 && bOm a * cos (bTh a) > bOm b * cos (bTh b)
             then let (va, vb) = collide (wE w) (1, bOm a) (1, bOm b)
                  in a { bOm = va } : pass (i + 1) (b { bOm = vb } : rest)
             else a : pass (i + 1) (b : rest)
      pass _ xs = xs
      sub :: Int -> [Ball] -> [Ball]
      sub 0 bs = bs
      sub n bs = let !bs' = forceList (iterN 5 (pass 0) (map swing bs)) in sub (n - 1) bs'
      balls = sub 20 (wBalls w)
      p = sum (map bOm balls)
      k = sum [0.5 * bOm b * bOm b + gL * (1 - cos (bTh b)) | b <- balls]
  in w { wBalls = balls, hP = pushHist (hP w) p, hK = pushHist (hK w) k }

drawAll :: World -> Picture
drawAll w = Pictures
  [ if wCradle w then drawCradle w else drawCarts w
  , txt 20 36 17 (gray 1) ((if wCradle w then "Newton's cradle (1-4 balls)" else "Cart collision") ++ "   e = " ++ nf 2 (wE w)
                           ++ " (Left/Right)   m2/m1 = " ++ nf 2 (wRatio w) ++ " (Up/Down)   Space: again   N: switch")
  , txt 20 66 14 (hsb 0 0 65) "v1' = (m1 v1 + m2 v2 + m2 e (v2 - v1)) / (m1 + m2).  e = 1 keeps kinetic energy; e < 1 loses it as heat and sound."
  ]

drawCarts :: World -> Picture
drawCarts w = Pictures
  [ view [Color (hsb 0 0 40) (thickLine 3 (width * 0.03, y + 34) (width * 0.47, y + 34))]
  , cart 0 30 (-18) (wA w)
  , cart 1 200 18 (wB w)
    -- 棒グラフ：運動量と運動エネルギー
  , bars bx by bw "Momentum p = mv" [cm a * cv a, cm b * cv b] 2.4 True
  , bars bx (by + 110) bw "Kinetic energy K = mv^2/2" [0.5 * cm a * cv a * cv a, 0.5 * cm b * cv b * cv b] 2.4 False
  , drawPlot gx (height * 0.14) gw (height * 0.32) [Series (hP w) 55 "Momentum", Series (hK w) 320 "Energy"] (-0.5) 3 "Totals before and after"
  , drawPlot gx (height * 0.52) gw (height * 0.32) [Series (hV1 w) 30 "v1", Series (hV2 w) 200 "v2"] (-3) 3 "Velocity [m/s]"
  ]
  where
    a = wA w
    b = wB w
    y = height * 0.42
    sx x = remap x (-2.4) 2.4 (width * 0.03) (width * 0.47)
    pxPerM = (width * 0.44) / 4.8
    bx = width * 0.05
    by = height * 0.62
    bw = width * 0.4
    gx = width * 0.5
    gw = width * 0.46
    cart :: Int -> Float -> Float -> Cart -> Picture
    cart i hue shift c =
      let cw' = cw c * pxPerM
          hh = 30 + 12 * sqrt (cm c)
          px = sx (cx c)
          col = hsb hue 70 100
      in Pictures
           [ view [ Color col (boxAt (px - cw' / 2) (y + 26 - hh) cw' hh)
                  , Color (hsb 0 0 20) (at (px - cw' * 0.3, y + 28) (disc 6))
                  , Color (hsb 0 0 20) (at (px + cw' * 0.3, y + 28) (disc 6))
                  , Color col (thickLine 3 (px, y - hh - 4) (px + cv c * 40, y - hh - 4)) ]   -- 速度の矢印
           , txtC (px + shift) (y + 16 - hh) 14 (gray 1) ("m" ++ show (i + 1) ++ "=" ++ nf 2 (cm c)) ]

-- 合計と内訳の棒
bars :: Float -> Float -> Float -> String -> [Float] -> Float -> Bool -> Picture
bars x y w title vals vmax signed = Pictures
  [ txt x y 14 (hsb 0 0 75) (title ++ "   total " ++ nf 3 (sum vals))
  , view ( stack x0 (zip [30, 200] vals)
        ++ [ Color (hsb 0 0 60) (boxAt x0 (y + 40) (sum vals * s) 10)
           , Color (hsb 0 0 50) (Line [(x0, y + 8), (x0, y + 56)]) ] )
  ]
  where
    x0 = if signed then x + w * 0.3 else x
    s = (w * 0.7) / vmax
    stack _ [] = []
    stack cur ((hue, v) : rest) = Color (hsb hue 70 100) (boxAt cur (y + 12) (v * s) 22) : stack (cur + v * s) rest

drawCradle :: World -> Picture
drawCradle w = Pictures
  [ view ( Color (hsb 0 0 60) (thickLine 6 (cxC - 4 * rB, topY) (cxC + 4 * rB, topY))
         : concat (zipWith ballPic [0 :: Int ..] (wBalls w)) )
  , drawPlot (width * 0.5) (height * 0.2) (width * 0.46) (height * 0.5)
      [Series (hP w) 55 "Sum of w", Series (hK w) 320 "Energy"] (-4) 12 "Totals (with e < 1 they fade out)"
  , txt 20 (height - 30) 15 (hsb 0 0 65) "Equal masses and e = 1 swap velocities: as many balls fly out as you lifted."
  ]
  where
    ballPic i b =
      let ax = cxC + (fromIntegral i - 2) * 2 * rB
          p = (ax + lp * sin (bTh b), topY + lp * cos (bTh b))
      in [ Color (hsb 0 0 70) (thickLine 1.5 (ax, topY) p)
         , Color (hsb 45 20 90) (at p (disc rB)) ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wE = min (fromIntegral (round ((wE w + 0.1) * 10) :: Int) / 10) 1 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wE = max (fromIntegral (round ((wE w - 0.1) * 10) :: Int) / 10) 0 }
  EventKey (SpecialKey KeyUp) Down _ _ -> restart w { wRatio = min (wRatio w * 2) 16 }
  EventKey (SpecialKey KeyDown) Down _ _ -> restart w { wRatio = max (wRatio w / 2) (1 / 16) }
  EventKey (SpecialKey KeySpace) Down _ _ -> restart w
  EventKey (Char c) Down _ _
    | toLower c == 'n' -> restart w { wCradle = not (wCradle w), hP = Seq.empty, hK = Seq.empty }
    | c >= '1' && c <= '4' -> restart w { wLift = fromEnum c - fromEnum '0', wCradle = True }
  _ -> w

-- ---------- やってみよう ----------
-- 1. e = 1、質量比 1 で衝突。台車 1 がぴたっと止まり、台車 2 が同じ速さで動き出す（速度の交換）
-- 2. 質量比 16（重いトラックに軽い車）と 1/16（ピンポン球にボウリング球）で、跳ね返りはどうなる？
-- 3. e = 0 にすると 2 台はくっつく。失われた運動エネルギーは何 % ？ 質量比で変わる
-- 4. ゆりかごで e = 0.9 にすると？ 本物のゆりかごがだんだん「まとまって」揺れる理由


-- ==================================================
-- 道具（グラフを描く）
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 と同じ座標（左上が原点、y は下向き）で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は左下の位置（p5 の座標）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtC, txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y s c str = txt (x - fromIntegral (length str) * s * 0.36) y s c str
txtR x y s c str = txt (x - fromIntegral (length str) * s * 0.72) y s c str

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y w h = rect (x + w / 2) (y + h / 2) w h

nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

iterN :: Int -> (a -> a) -> a -> a
iterN 0 _ x = x
iterN n f x = iterN (n - 1) f (f x)

-- リストの中身まで計算を済ませる（遅延評価のたまりを防ぐ）
forceList :: [Ball] -> [Ball]
forceList xs = foldr (\b r -> b `seq` r) xs xs

pushHist :: Seq.Seq Float -> Float -> Seq.Seq Float
pushHist s !v =
  let s' = s Seq.|> v
  in if Seq.length s' > histLen then Seq.drop 1 s' else s'

data Series = Series
  { sData :: !(Seq.Seq Float)
  , sHue  :: !Float
  , sName :: String
  }

-- 時間プロット：枠 (x, y, w, h) に series を描く（縦の範囲 lo〜hi）
drawPlot :: Float -> Float -> Float -> Float -> [Series] -> Float -> Float -> String -> Picture
drawPlot x0 y0 w h series lo hi title = Pictures
  ( view ( Color (hsb 0 0 14) (boxAt x0 y0 w h)
         : [Color (hsb 0 0 30) (Line [(x0, yz), (x0 + w, yz)]) | yz > y0 && yz < y0 + h]
        ++ map curve series )
  : txt (x0 + 8) (y0 + 20) 14 (hsb 0 0 70) title
  : legend (x0 + w - 8) series )
  where
    yz = remap 0 lo hi (y0 + h) y0
    curve s =
      let d = toList (sData s)
          off = histLen - length d
      in Color (hsb (sHue s) 70 100) (thickPath 2
           [ ( remap (fromIntegral (i + off)) 0 (fromIntegral histLen - 1) x0 (x0 + w)
             , remap (clampF lo hi v) lo hi (y0 + h) y0 )
           | (i, v) <- zip [0 :: Int ..] d, even i ])
    legend _ [] = []
    legend lx (s : rest) = txtR lx (y0 + 20) 14 (hsb (sHue s) 70 100) (sName s)
                         : legend (lx - fromIntegral (length (sName s)) * 14 * 0.72 - 14) rest
