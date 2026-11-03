-- ============================================
-- 05 ジーマンのカタストロフィー・マシン
-- 円板（中心で自由に回る）の縁の 1 点に、ゴムひもを 2 本つなぐ。1 本は上の固定点 A へ、もう 1 本の端 Q をマウスで動かす
--   ゴムの伸びのエネルギー E(θ) がいちばん小さい角度へ円板は回る。Q をゆっくり動かしても、ある所で円板が「パチン」と回る
--   黄色い領域 = エネルギーの谷が 2 つある Q の場所。4 つのとがった点（カスプ）をもつひし形。へりをこえるときにとぶ
-- マウス = 端 Q / A = 自動で左右に往復 / T = 黄色い領域の表示 / C = 軌跡を消す / ↑↓ = ゴムの自然長
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

rDisk, dA, damping :: Float
rDisk = 1                  -- ★ 円板の半径
dA = 2                     -- ★ 中心から固定点 A までの距離
damping = 1.8              -- ★ 回転の摩擦

data World = World
  { wL0     :: !Float      -- ★ ゴムの自然長（これより短いとたるむ）
  , wTheta  :: !Float      -- 円板の角度
  , wOmega  :: !Float      -- 角速度
  , wQ      :: !Point      -- 動かす端（単位は U）
  , wAuto   :: !Bool
  , wShow   :: !Bool
  , wT      :: !Float
  , wTrail  :: !(Seq Point)
  , wLog    :: !(Seq Float)
  , wSnaps  :: !Int
  , wFlash  :: !Float
  , wRegion :: !Picture    -- 谷が 2 つの領域の絵
  }

world0 :: World
world0 = World 1 0.3 0 (0.6, 3) False True 0 Seq.empty Seq.empty 0 0 Blank

main :: IO ()
main = runSketch (sketch "35-05 Zeeman Machine" world0)
  { skBack  = hsb 205 30 12
  , skStart = \env -> pure world0 { wRegion = buildRegion env (wL0 world0) }
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- 1 単位の大きさと円板の中心（画面）
unitOf :: Env -> Float
unitOf env = min ((envH env - 130) / 6.6) 110

centerOf :: Env -> Point
centerOf env = (envW env * 0.36, 110 + 2.3 * unitOf env)

-- 円板の縁の点 P(θ)（θ = 0 で真下）
rim :: Float -> Point
rim th = (rDisk * sin th, rDisk * cos th)

-- ★ エネルギー：のびたゴムだけが E = ½(のび)² をもつ
energy :: Float -> Float -> Point -> Float
energy l0 th (qx, qy) =
  let (px, py) = rim th
      part sx sy =
        let d = sqrt ((px - sx) * (px - sx) + (py - sy) * (py - sy)) - l0
        in if d > 0 then 0.5 * d * d else 0
  in part 0 (negate dA) + part qx qy

dE :: Float -> Float -> Point -> Float
dE l0 th q = (energy l0 (th + 1e-3) q - energy l0 (th - 1e-3) q) / 2e-3

-- θ を一周調べて、エネルギーの谷の数を数える
countMinima :: Float -> Point -> Int
countMinima l0 q =
  let n = 120
      es = U.generate n (\i -> energy l0 (tau * fromIntegral i / fromIntegral n) q)
      isMin i = let e = es U.! i in e < es U.! ((i + n - 1) `mod` n) && e <= es U.! ((i + 1) `mod` n)
  in length (filter isMin [0 .. n - 1])

-- 谷が 2 つ以上の Q を 4 ピクセルごとに調べて、横につながったマスを 1 枚の長方形にまとめる
buildRegion :: Env -> Float -> Picture
buildRegion env l0 =
  let u = unitOf env
      (cx, cy) = centerOf env
      cell = 4 :: Float
      nx = floor (envW env / cell) :: Int
      nRows = floor ((envH env - cy - 0.3 * u) / cell) :: Int
      rowAt j =
        let py = cy + 0.3 * u + fromIntegral j * cell
            hit i =
              let q = ((fromIntegral i * cell + cell / 2 - cx) / u, (py + cell / 2 - cy) / u)
              in abs (fst q) <= 2.5 && countMinima l0 q >= 2
            runs = spans [ i | i <- [0 .. nx - 1], hit i ]
        in [ box (fromIntegral i0 * cell) py (fromIntegral (i1 - i0 + 1) * cell) cell | (i0, i1) <- runs ]
      pics = concatMap rowAt [0 .. nRows]
  in length pics `seq` Color (hsbA 50 80 100 35) (Pictures pics)

-- 並んだ番号を (始め, 終わり) にまとめる
spans :: [Int] -> [(Int, Int)]
spans [] = []
spans (i : is) = go i i is
  where
    go a b (c : cs) | c == b + 1 = go a c cs
                    | otherwise = (a, b) : go c c cs
    go a b [] = [(a, b)]

stepWorld :: Env -> World -> World
stepWorld env w =
  let u = unitOf env
      (cx, cy) = centerOf env
      (mx, my) = mouseP5 env
      t = if wAuto w then wT w + 0.01 else wT w
      q | wAuto w = (1.3 * sin t, 3)
        | my > 100 = (clampF (-3) 3 ((mx - cx) / u), clampF 0.5 5 ((my - cy) / u))
        | otherwise = wQ w
      -- 円板の運動（小さな刻みで）
      go :: Int -> Float -> Float -> (Float, Float)
      go 0 th om = (th, om)
      go k th om =
        let dt = 1 / 240
            om1 = clampF (-40) 40 (om + (-40 * dE (wL0 w) th q - damping * om) * dt)
        in go (k - 1) (th + om1 * dt) om1
      (th1, om1) = go 8 (wTheta w) (wOmega w)
      theta = wrapF tau (th1 + pi) - pi
      snap = abs om1 > 3 && wFlash w == 0
      flash = max 0 ((if snap then 40 else wFlash w) - 1)
  in w { wT = t, wQ = q, wTheta = theta, wOmega = om1, wFlash = flash
       , wSnaps = if snap then wSnaps w + 1 else wSnaps w
       , wTrail = pushN 600 q (wTrail w), wLog = pushN 500 theta (wLog w) }

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      u = unitOf env
      (cx, cy) = centerOf env
      l0 = wL0 w
      theta = wTheta w
      q@(qx, qy) = wQ w
      toS (x, y) = (cx + x * u, cy + y * u)
      p@(pxs, pys) = toS (rim theta)
      aP@(ax, ay) = (cx, cy - dA * u)
      qs@(qsx, qsy) = toS q
      band s =
        let stretch = max 0 (dist2 p s / u - l0)
        in Color (hsb 330 (60 + stretch * 20) 100) (thickLine (clampF 2 6 (remap stretch 0 3 6 2)) p s)
      flash = wFlash w
      panel = hsb 205 25 18
      gx = wd * 0.66
      gy = 110
      gw = wd - gx - 30
      gh = h * 0.4
      ey = gy + gh + 30
      eh = h - ey - 30
      angle = [ (gx + fromIntegral i / 500 * gw, gy + gh / 2 - v / pi * (gh / 2 - 8)) | (i, v) <- zip [0 :: Int ..] (toList (wLog w)) ]
      es = [ energy l0 (negate pi + tau * fromIntegral i / 120) q | i <- [0 .. 120 :: Int] ]
      emax = foldr max 0 es
      toE e = ey + eh - 10 - e / (emax + 1e-6) * (eh - 40)
      ecurve = [ (gx + fromIntegral i / 120 * gw, toE e) | (i, e) <- zip [0 :: Int ..] es ]
  in Pictures
    [ onP5 env (Pictures
        [ if wShow w then wRegion w else Blank
        , Color (hsbA 190 60 100 50) (thickPath 2 (map toS (toList (wTrail w))))
        , band aP
        , band qs
        , Color (gray 0.85) (at aP (disc 8))
        , Color (hsb 210 20 (if flash > 0 then 60 + flash else 55)) (at (cx, cy) (disc (rDisk * u)))
        , Color white (thickLine 3 (cx, cy) p)
        , Color (hsb 45 90 100) (at p (disc 8))
        , Color white (at (cx, cy) (disc 4))
        , Color (hsb 190 70 100) (at qs (disc 9))
          -- 右：円板の角度のグラフ
        , Color panel (box gx gy gw gh)
        , Color (hsbA 0 0 100 15) (Line [(gx, gy + gh / 2), (gx + gw, gy + gh / 2)])
        , Color (hsb 45 90 100) (thickPath 2 angle)
          -- エネルギー E(θ) のグラフ
        , Color panel (box gx ey gw eh)
        , Color (hsb 330 60 100) (thickPath 2 ecurve)
        , Color (hsb 45 90 100) (at (gx + (theta + pi) / tau * gw, toE (energy l0 theta q)) (disc 6))
        ])
    , txt env (ax + 12) (ay + 5) 14 (gray 0.8) "A (fixed)"
    , txt env (qsx + 12) (qsy + 5) 14 (gray 0.8) "Q"
    , txt env (gx + 12) (gy + 22) 14 (gray 0.8) "Disk angle vs time; step = snap"
    , txt env (gx + 12) (ey + 22) 14 (gray 0.8) ("E(theta), -180..180  valleys " ++ show (countMinima l0 q))
    , txt env 20 36 16 white ("Mouse = Q   A: auto " ++ onOff (wAuto w) ++ "   T: region   C: clear trail   Up/Dn: l0 = "
                              ++ nf 2 l0 ++ "   snaps " ++ show (wSnaps w))
    , txt env 20 62 14 (gray 0.6) "Inside the yellow region the disk hardly moves. Cross its left or right edge and a valley vanishes:"
    , txt env 20 84 14 (gray 0.6) "snap! Pass below the pointed tip (cusp) and the disk turns smoothly."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ ->
    let l = min 1.6 (wL0 w + 0.1) in w { wL0 = l, wRegion = buildRegion env l }
  EventKey (SpecialKey KeyDown) Down _ _ ->
    let l = max 0.4 (wL0 w - 0.1) in w { wL0 = l, wRegion = buildRegion env l }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "tT" -> w { wShow = not (wShow w) }
    | c `elem` "cC" -> w { wTrail = Seq.empty }
  _ -> w

-- ---------- やってみよう ----------
-- 1. Q を黄色い領域の左のへりから右のへりへゆっくり動かし、パチンと回る場所を確かめよう。戻るときは？
-- 2. 黄色い領域のとがった先より下（遠く）を通ると、パチンとならない。なぜ？（04 のループが消える a > 0 と同じ）
-- 3. dA を 2.5 にしたり、l0 を変えたりすると、カスプの位置と大きさはどう変わる？
-- 4. 本物をつくろう：厚紙の円板・画びょう・輪ゴム 2 本でできる（ジーマン 1972）


-- ==================================================
-- ここから下は「道具」（座標の変換・記録・色・数字）
-- ==================================================
-- 長さ n までの記録に足す
pushN :: Int -> a -> Seq a -> Seq a
pushN n x s = let s' = s |> x in if Seq.length s' > n then Seq.drop 1 s' else s'

-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を画面に合わせる
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- p5.js の text(s, x, y) と同じ位置に文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

-- マウスの位置（左上が原点、y は下向き）
mouseP5 :: Env -> Point
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上が (x, y) の長方形（p5.js の rect）
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

-- 透明度を 0〜100 で書く HSB（p5.js と同じ）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

onOff :: Bool -> String
onOff b = if b then "ON" else "off"
