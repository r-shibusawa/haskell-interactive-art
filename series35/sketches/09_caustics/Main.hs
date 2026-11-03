-- ============================================
-- 09 光のコースティクス
-- コーヒーカップの底に見える明るい曲線や、プールの底でゆれる光の網目 = コースティクス（火線）
--   たくさんの光線が反射・屈折して「重なる」所が明るくなる。その線は折り目（fold）で、とがった点はカスプ
--   光の世界にもカタストロフィーがある（ベリー、ナイ）。光線の束 = 地形、明るい線 = 分岐集合
-- 1 = カップ（反射） / 2 = 水面（屈折） / マウス = 光源の位置（1）・光の向き（2） / P = 平行光と点光源（1） / ↑↓ = 光線の数 / スペース = 波を止める（2）
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

n2, waveAmp :: Float
n2 = 1.33                  -- ★ 水の屈折率
waveAmp = 18               -- ★ 波の高さ（px）

data World = World
  { wScene    :: !Int
  , wRays     :: !Int      -- ★ 光線の数
  , wParallel :: !Bool     -- カップ：平行光か点光源か
  , wWave     :: !Bool
  , wT        :: !Float
  }

world0 :: World
world0 = World 1 500 True True 0

main :: IO ()
main = runSketch (sketch "35-09 Caustics" world0)
  { skBack  = hsb 230 40 6
  , skStep  = \_ _ w -> pure (if wWave w then w { wT = wT w + 0.02 } else w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

drawAll :: Env -> World -> Picture
drawAll env w =
  let cupScene = wScene w == 1
  in Pictures
    [ onP5 env (if cupScene then cup env w else water env w)
    , txt env 20 36 14 white
        ((if cupScene then "1: cup (reflection)" else "2: water (refraction)") ++ "   1/2: switch   mouse = "
         ++ (if cupScene then "light" else "direction") ++ "   P: " ++ (if wParallel w then "parallel" else "point")
         ++ "   Up/Dn: " ++ show (wRays w) ++ " rays   Space: wave " ++ onOff (wWave w))
    , txt env 20 62 14 (gray 0.6)
        (if cupScene then "Parallel light makes a nephroid, a light on the rim a cardioid. The pointed tip is a cusp."
         else "The bumpy surface acts as lenses. Where rays cross on the floor, a bright net appears.")
    , txt env 20 84 14 (gray 0.6)
        (if cupScene then "The bright curve is where the reflected rays fold back (their envelope)."
         else "Net lines are folds, with cusps near the crossings. What if the waves get taller?")
    ]

-- ---- カップ：円の内側の鏡で 1 回反射
cup :: Env -> World -> Picture
cup env w =
  let wd = envW env
      h = envH env
      cx = wd / 2
      cy = h / 2 + 40
      r = min wd (h - 120) * 0.42
      (mx, my) = mouseP5 env
      ang = atan2 (my - cy) (mx - cx)
      dS = dist2 (mx, my) (cx, cy)
      (sx, sy) | dS > r * 0.98 = (cx + (mx - cx) / dS * r * 0.98, cy + (my - cy) / dS * r * 0.98)
               | otherwise = (mx, my)
      n = wRays w
      par = wParallel w
      ray i =
        let (ox', oy', dx, dy)
              | par =
                  let dx0 = negate (cos ang)
                      dy0 = negate (sin ang)
                      s = negate r + 2 * r * (fromIntegral i + 0.5) / fromIntegral n
                  in (cx - dx0 * 2 * r - dy0 * s, cy - dy0 * 2 * r + dx0 * s, dx0, dy0)   -- マウスの方から来る平行光
              | otherwise =
                  let th = tau * fromIntegral i / fromIntegral n
                  in (sx, sy, cos th, sin th)
            -- 円との交点（遠い方）= 鏡に当たる点 P
            ox = ox' - cx
            oy = oy' - cy
            bq = ox * dx + oy * dy
            cq = ox * ox + oy * oy - r * r
            dq = bq * bq - cq
        in if dq <= 0 then ([], []) else
           let tq = negate bq + sqrt dq
               px = ox' + dx * tq
               py = oy' + dy * tq
               tin = if par then negate bq - sqrt dq else 0
               -- ★ 反射：r = d − 2(d・n)n
               nx = (px - cx) / r
               ny = (py - cy) / r
               dn = dx * nx + dy * ny
               rx = dx - 2 * dn * nx
               ry = dy - 2 * dn * ny
               -- 反射した光は次に縁に当たるまで進む（弦の長さ = −2(P−C)・r）
               l = -2 * ((px - cx) * rx + (py - cy) * ry)
           in ( [Line [(ox' + dx * tin, oy' + dy * tin), (px, py)]]
              , [Line [(px, py), (px + rx * l, py + ry * l)]] )
      rays = map ray [0 .. n - 1]
  in Pictures
    [ Color (gray 0.6) (at (cx, cy) (ring r 3))
    , Color (hsbA 50 30 100 4) (Pictures (concatMap fst rays))
    , Color (hsbA 40 70 100 9) (Pictures (concatMap snd rays))
    , if par then Blank else Color (hsb 50 30 100) (at (sx, sy) (disc 5))
    ]

-- ---- 水面：上から来た光が波打つ水面で屈折し、底に届く
water :: Env -> World -> Picture
water env w =
  let wd = envW env
      hh = envH env
      top = 110
      surfY = hh * 0.32
      floorY = hh - 60
      (mx, _) = mouseP5 env
      t = wT w
      tilt = clampF (-0.5) 0.5 ((mx - wd / 2) / wd) * 0.6     -- 光の向き
      h x = surfY + waveAmp * sin (x * 0.012 + t) + waveAmp * 0.6 * sin (x * 0.027 - t * 1.3) + waveAmp * 0.3 * sin (x * 0.051 + t * 0.7)
      nAll = wRays w * 2
      dx = sin tilt
      dy = cos tilt
      ray i =
        let x0 = -100 + (wd + 200) * (fromIntegral i + 0.5) / fromIntegral nAll
            xs = x0 + dx / dy * (surfY - top)
            ys = h xs
            -- 水面の法線（上向き）
            slope = h (xs + 0.5) - h (xs - 0.5)
            nl = sqrt (slope * slope + 1)
            nx = slope / nl
            ny = -1 / nl
            -- ★ スネルの法則（ベクトル形）：n1 sinθ1 = n2 sinθ2
            eta = 1 / n2
            c1 = negate (dx * nx + dy * ny)
            k = 1 - eta * eta * (1 - c1 * c1)
        in if k < 0 then Nothing else
           let c2 = sqrt k
               rx = eta * dx + (eta * c1 - c2) * nx
               ry = eta * dy + (eta * c1 - c2) * ny
           in if ry < 1e-4 then Nothing else
              let tt = (floorY - ys) / ry
                  xf = xs + rx * tt
              in Just ( Line [(xs - dx / dy * (ys - top), top), (xs, ys)]
                      , Line [(xs, ys), (xf, floorY)]
                      , floor (xf / wd * 240) :: Int )
      rays = [ r | i <- [0 .. nAll - 1], Just r <- [ray i] ]
      bins = U.accum (+) (U.replicate 240 (0 :: Int)) [ (bi, 1) | (_, _, bi) <- rays, bi >= 0, bi < 240 ]
      mean = fromIntegral nAll / 240 * (wd / (wd + 200))
      cellW = wd / 240
      surface = [ (x, h x) | i <- [0 .. floor (wd / 8) :: Int], let x = fromIntegral i * 8 ]
  in Pictures
    [ Color (hsb 205 70 18) (box 0 surfY wd (floorY - surfY))
    , Color (hsbA 55 30 100 5) (Pictures [ a | (a, _, _) <- rays ])
    , Color (hsbA 190 50 100 7) (Pictures [ b | (_, b, _) <- rays ])
    , Color (hsbA 190 40 100 60) (thickPath 2 surface)
      -- 底の明るさ（光線が集まるほど明るい）
    , Pictures [ Color (hsb 55 40 (100 * min 1 (fromIntegral c / (mean * 4)))) (box (fromIntegral i * cellW) floorY (cellW + 1) 40)
               | (i, c) <- zip [0 :: Int ..] (U.toList bins) ]
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (Char '1') Down _ _ -> w { wScene = 1 }
  EventKey (Char '2') Down _ _ -> w { wScene = 2 }
  EventKey (Char c) Down _ _ | c `elem` "pP" -> w { wParallel = not (wParallel w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wWave = not (wWave w) }
  EventKey (SpecialKey KeyUp) Down _ _    -> w { wRays = min 1500 (wRays w + 100) }
  EventKey (SpecialKey KeyDown) Down _ _  -> w { wRays = max 100 (wRays w - 100) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. カップで点光源（P）にして、光源を円の縁にぴったり置くとカーディオイド。中心に近づけると？
-- 2. 水面で waveAmp を 5 と 40 にして、網目の線がいつ現れるか比べよう（波が小さいと線は底より下で交わる）
-- 3. n2 を 2.4（ダイヤモンド）にすると網目はどう変わる？
-- 4. ガラスのコップ・スプーン・指輪に光を当てて、本物のコースティクスを探そう。写真と見比べる


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
