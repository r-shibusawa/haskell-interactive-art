-- ============================================
-- 10 アーチの座屈
-- 1 = 浅いアーチ（2 本の棒）：上から押す力 P をゆっくり増やすと、ある所で「ペコッ」と下へ反転する（飛び移り座屈）
--   力を戻しても元に戻らない（ヒステリシス）。ペットボトルのへこみ、缶のふた、ヘアピンの「パチン」
-- 2 = 柱（オイラー座屈）：P が 1 をこえると柱は左右どちらかに曲がる。小さな横の力 ε（不完全さ）が向きを決める
--   V = q⁴/4 + (1 − P)q²/2 − εq はカスプそのもの。(P, ε) の平面に 4(1 − P)³ + 27ε² = 0 のカスプが現れる
-- 1/2 = 切りかえ / ↑↓ = 力 P / ←→ = 横の力 ε（2） / A = 自動で押したり戻したり / R = 戻す
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Numeric (showFFloat)

kSpring, h0 :: Float
kSpring = 10               -- ★ 棒のばねの強さ
h0 = 0.5                   -- ★ アーチの高さ（半分の幅 = 1）

data Rec = Rec !Float !Float !Float     -- (P, 状態, ε)

data World = World
  { wScene :: !Int
  , wP     :: !Float       -- 力
  , wEps   :: !Float       -- 不完全さ
  , wY     :: !Float       -- アーチの頂点
  , wVy    :: !Float
  , wQ     :: !Float       -- 柱の曲がり
  , wVq    :: !Float
  , wAuto  :: !Bool
  , wT     :: !Float
  , wTrail :: !(Seq Rec)
  , wSnaps :: !Int
  , wLast  :: !Int
  }

world0 :: World
world0 = World 1 0 0.05 h0 0 0 0 True 0 Seq.empty 0 (-100)

main :: IO ()
main = runSketch (sketch "35-10 Arch Buckling" world0)
  { skBack  = hsb 30 20 11
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- アーチ：頂点が y のときにつり合う力
archP :: Float -> Float
archP yy =
  let s = sqrt (1 + yy * yy)
      s0 = sqrt (1 + h0 * h0)
  in -2 * kSpring * (s - s0) * yy / s

-- アーチが耐えられる最大の力を先に調べる
pMax :: Float
pMax = foldr (max . archP) 0 [ fromIntegral i * 0.001 | i <- [0 .. 500 :: Int] ]

stepWorld :: Env -> World -> World
stepWorld env w0 =
  let t = if wAuto w0 then wT w0 + 0.006 else wT w0
      arch = wScene w0 == 1
      p | wAuto w0 && arch = 1.35 * pMax * sin t
        | wAuto w0 = 1.6 + 0.4 * sin (t * 0.5)
        | otherwise = wP w0
      eps | wAuto w0 && not arch = 0.25 * sin (t * 1.7)
          | otherwise = wEps w0
      dt = 1 / 300
      archStep (y, v) =
        let v1 = v + ((archP y - p) * 40 - 6 * v) * dt
        in (clampF (-1.5) 1.5 (y + v1 * dt), v1)
      colStep (q, v) =
        let f = negate (q * q * q + (1 - p) * q - eps)      -- −V'(q)
            v1 = v + (f * 60 - 8 * v) * dt
        in (clampF (-2) 2 (q + v1 * dt), v1)
      times :: Int -> (a -> a) -> a -> a
      times 0 _ a = a
      times n f a = let b = f a in b `seq` times (n - 1) f b
      (y2, vy2) = if arch then times 10 archStep (wY w0, wVy w0) else (wY w0, wVy w0)
      (q2, vq2) = if arch then (wQ w0, wVq w0) else times 10 colStep (wQ w0, wVq w0)
      moved = if arch then abs (y2 - wY w0) else abs (q2 - wQ w0)
      snap = moved > 0.02 && envFrame env - wLast w0 > 30
      r = Rec p (if arch then y2 else q2) eps
      tr = let s = wTrail w0 |> r in if Seq.length s > 1200 then Seq.drop 1 s else s
  in w0 { wT = t, wP = p, wEps = eps, wY = y2, wVy = vy2, wQ = q2, wVq = vq2, wTrail = tr
        , wSnaps = if snap then wSnaps w0 + 1 else wSnaps w0
        , wLast = if snap then envFrame env else wLast w0 }

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      lw = wd * 0.5
      arch = wScene w == 1
  in Pictures
    [ if arch then drawArch env w lw else drawColumn env w lw
    , drawGraph env w (lw + 30) 110 (wd - lw - 60) (h - 150)
    , txt env 20 36 18 white
        ((if arch then "1: shallow arch" else "2: column (Euler)") ++ "   P = " ++ nf 2 (wP w) ++ " (Up/Dn)"
         ++ (if arch then "" else "   eps = " ++ nf 3 (wEps w) ++ " (<- ->)")
         ++ "   A: auto " ++ onOff (wAuto w) ++ "   R: reset   snaps " ++ show (wSnaps w))
    , txt env 20 62 14 (gray 0.6)
        (if arch then "Right: P across, apex height up. Past the curve's peak (P = " ++ nf 2 pMax ++ ") no balance is left: it jumps"
         else "Right: the (P, eps) plane. Inside the yellow cusp the column can bend 2 ways. For P > 1, changing")
    , txt env 20 84 14 (gray 0.6)
        (if arch then "to the lower branch. Coming back, it jumps at -" ++ nf 2 pMax ++ "."
         else "the sign of eps makes it snap to the other side at the cusp's edge.")
    ]

drawArch :: Env -> World -> Float -> Picture
drawArch env w lw =
  let u = min (lw * 0.35) 220
      cx = lw / 2 + 20
      cy = envH env * 0.55
      y = wY w
      p = wP w
      top@(tx, ty) = (cx, cy - y * u)
      l = abs (p / pMax * 80)
      col = if y > 0 then hsb 30 80 100 else hsb 190 70 100
      red = hsb 0 80 100
  in onP5 env (Pictures
    [ Color (hsb 30 15 25) (Pictures [box (cx - u * 1.3) (cy + 6) (u * 0.3) 40, box (cx + u) (cy + 6) (u * 0.3) 40])
    , Color col (Pictures [thickLine 14 (cx - u, cy) top, thickLine 14 (cx + u, cy) top])
    , Color white (Pictures [at (cx - u, cy) (disc 8), at (cx + u, cy) (disc 8), at top (disc 9)])
      -- 力の矢印
    , Color red (thickLine 5 (tx, ty - 20 - l) (tx, ty - 20))
    , Color red (if p > 0 then Polygon [(tx, ty - 14), (tx - 10, ty - 30), (tx + 10, ty - 30)]
                 else if p < 0 then Polygon [(tx, ty - 24 - l - 10), (tx - 10, ty - 20 - l + 4), (tx + 10, ty - 20 - l + 4)]
                 else Blank)
    ])

drawColumn :: Env -> World -> Float -> Picture
drawColumn env w lw =
  let h = envH env
      hh = h - 300
      cx = lw / 2 + 20
      y0 = h - 80
      y1 = y0 - hh
      q = wQ w
      col | abs q < 0.05 = hsb 0 0 90
          | q > 0 = hsb 30 80 100
          | otherwise = hsb 190 70 100
      ex = wEps w * 300
      off = if ex > 0 then 20 else -20
      red = hsb 0 80 100
  in Pictures
    [ onP5 env (Pictures
        [ Color (hsb 30 15 25) (Pictures [box (cx - 60) y0 120 20, box (cx - 60) (y1 - 20) 120 20])
        , Color col (thickPath 12 [ (cx + q * 90 * sin (pi * s), y0 - hh * s) | i <- [0 .. 40 :: Int], let s = fromIntegral i / 40 ])
          -- 上から押す力 P と、横の力 ε
        , Color red (thickLine 5 (cx, y1 - 90) (cx, y1 - 30))
        , Color red (Polygon [(cx, y1 - 22), (cx - 10, y1 - 38), (cx + 10, y1 - 38)])
        , Color (hsb 50 90 100) (thickLine 4 (cx - ex - off, y0 - hh / 2) (cx - off, y0 - hh / 2))
        ])
    , txt env (cx + 14) (y1 - 50) 14 (gray 0.8) "P"
    , txt env (cx - ex - 30) (y0 - hh / 2 - 10) 14 (gray 0.8) "eps"
    ]

drawGraph :: Env -> World -> Float -> Float -> Float -> Float -> Picture
drawGraph env w gx gy gw gh =
  let trail = toList (wTrail w)
      back = onP5 env (Pictures
        [ Color (hsb 30 15 18) (box gx gy gw gh)
        , Color (hsbA 0 0 100 15) (Pictures [Line [(gx + gw / 2, gy), (gx + gw / 2, gy + gh)], Line [(gx, gy + gh / 2), (gx + gw, gy + gh / 2)]])
        ])
  in if wScene w == 1
    then
      let toP p = gx + gw / 2 + p / (1.5 * pMax) * gw / 2
          toS s = gy + gh / 2 - s / 0.8 * (gh / 2 - 10)
          pts = [ if abs p < 1.5 * pMax then Just (toP p, toS yy) else Nothing
                | i <- [0 .. 160 :: Int], let yy = -0.8 + 0.01 * fromIntegral i, let p = archP yy ]
          link (Just a) (Just b) = [thickLine 3 a b]
          link _ _ = []
      in Pictures
        [ back
        , onP5 env (Pictures
            [ Color (hsbA 0 0 100 40) (Pictures (concat (zipWith link pts (drop 1 pts))))
            , Color (hsbA 30 80 100 60) (thickPath 2 [ (toP p, toS s) | Rec p s _ <- trail ])
            , Color (hsb 50 90 100) (at (toP (wP w), toS (wY w)) (disc 7))
            ])
        , txt env (gx + 12) (gy + 22) 14 (gray 0.8) "P across, y up. White: balance, orange: path"
        ]
    else
      let toP p = gx + 20 + p / 2.2 * (gw - 40)
          toE e = gy + gh / 2 - e / 0.55 * (gh / 2 - 20)
          edge p = 2 * ((p - 1) / 3) ** 1.5
          ps = [ 1 + 0.02 * fromIntegral i | i <- [0 .. 60 :: Int] ] :: [Float]
          -- カスプの内側：へこんだ形なので、縦長の台形に分けて塗る
          strips = zipWith (\p0 p1 -> Polygon [ (toP p0, toE (edge p0)), (toP p1, toE (edge p1))
                                              , (toP p1, toE (negate (edge p1))), (toP p0, toE (negate (edge p0))) ]) ps (drop 1 ps)
          outline = [ (toP p, toE (edge p)) | p <- ps ] ++ [ (toP p, toE (negate (edge p))) | p <- reverse ps ]
      in Pictures
        [ back
        , onP5 env (Pictures
            [ Color (hsbA 50 90 100 40) (Pictures strips)
            , Color (hsb 50 90 100) (thickPath 2 (outline ++ take 1 outline))
            , Pictures [ Color (if s > 0 then hsbA 30 80 100 50 else hsbA 190 70 100 50) (at (toP p, toE e) (disc 1.5)) | Rec p s e <- trail ]
            , Color white (at (toP (wP w), toE (wEps w)) (disc 7))
            ])
        , txt env (gx + 12) (gy + 22) 14 (gray 0.8) "P across (0 to 2.2), eps up. Dot color = bend:"
        , txt env (gx + 12) (gy + 44) 14 (gray 0.8) "orange right, blue left"
        , txt env (toP 1 - 20) (gy + gh - 12) 14 (gray 0.8) "P = 1 (Euler load)"
        ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _    -> limit w { wAuto = False, wP = wP w + step }
  EventKey (SpecialKey KeyDown) Down _ _  -> limit w { wAuto = False, wP = wP w - step }
  EventKey (SpecialKey KeyRight) Down _ _ -> limit w { wAuto = False, wEps = min 0.35 (wEps w + 0.02) }
  EventKey (SpecialKey KeyLeft) Down _ _  -> limit w { wAuto = False, wEps = max (-0.35) (wEps w - 0.02) }
  EventKey (Char c) Down _ _
    | c `elem` "12" ->
        let s = fromEnum c - 48
        in (reset s (limit w)) { wScene = s, wTrail = Seq.empty }
    | c `elem` "aA" -> (limit w) { wAuto = not (wAuto w) }
    | c `elem` "rR" -> (reset (wScene w) (limit w)) { wTrail = Seq.empty, wSnaps = 0 }
  EventKey (Char _) Down _ _ -> limit w
  EventKey (SpecialKey _) Down _ _ -> limit w
  _ -> w
  where
    step = if wScene w == 1 then pMax * 0.05 else 0.05
    limit v = v { wP = if wScene v == 1 then clampF (-1.5 * pMax) (1.5 * pMax) (wP v) else clampF 0 2.2 (wP v) }
    reset s v = v { wP = if s == 1 then 0 else 0.5, wY = h0, wVy = 0, wQ = 0, wVq = 0 }

-- ---------- やってみよう ----------
-- 1. アーチ（1）で ↑ を少しずつ押し、ペコッとなる P を記録。h0 を 0.3 にすると耐えられる力はどうなる？
-- 2. 柱（2）で ε = 0 にして P を 1 より大きくすると、柱はどちらに曲がる？（ほんの少しの誤差で決まる）
-- 3. 柱で P = 1.8 に固定し、ε を + から − へゆっくり。パチンと移る ε を黄のカスプのへりと比べよう
-- 4. 定規を両手で押して曲げてみよう。横から指で押すと、反対側へパチンと移る


-- ==================================================
-- ここから下は「道具」（座標の変換・色・数字）
-- ==================================================
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
