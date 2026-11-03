-- ============================================
-- 11 船の転覆と復原力
-- 船が傾くと、水に沈んだ部分の中心（浮心 B）が低い側へ動き、重心 G との間に「起き上がる力のうで」GZ ができる
--   外から傾ける力（風・波）M を少しずつ強くしても、船はなめらかに少し傾くだけ。でも M が GZ の山をこえると
--   つり合う角度がなくなり、一気に転覆する（折り目のカタストロフィー）。逆さまの船も安定 → 元には戻らない
-- ↑↓ = 風の力 M / ←→ = 重心の高さ / W = 波 / A = 風をゆっくり強める / R = 起こす
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

hull :: [Point]
hull = [(-1, -0.6), (1, -0.6), (1.05, 0.6), (-1.05, 0.6)]   -- ★ 船の断面（左右対称、y が上）

fillRatio :: Float
fillRatio = 0.4            -- ★ 断面のうち水に沈む割合

nTab :: Int
nTab = 360

data World = World
  { wZg    :: !Float       -- ★ 重心の高さ（断面の中心が 0）
  , wM     :: !Float       -- 外からの傾ける力
  , wPhi   :: !Float       -- 傾き（ラジアン）
  , wOmega :: !Float       -- 角速度
  , wWaves :: !Bool
  , wAuto  :: !Bool
  , wGz    :: !(U.Vector Float)   -- 角度ごとの GZ
  , wC     :: !(U.Vector Float)   -- 角度ごとの水面の高さ
  , wT     :: !Float
  }

world0 :: World
world0 = let (g, c) = buildTable 0 in World 0 0 0 0 False False g c 0

main :: IO ()
main = runSketch (sketch "35-11 Ship Capsize" world0)
  { skBack  = hsb 210 35 14
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- ---- 船の計算：角度 φ で傾けたとき、決まった面積だけ沈む水面の高さ c を探し、沈んだ部分の中心 B を求める
rot :: Float -> Point -> Point
rot a (x, y) = (x * cos a - y * sin a, x * sin a + y * cos a)

edges :: [a] -> [(a, a)]
edges p = zip p (drop 1 p ++ take 1 p)

-- y < c の部分だけ残す
clipBelow :: [Point] -> Float -> [Point]
clipBelow poly c = concatMap cut (edges poly)
  where
    cut (p@(px, py), (qx, qy)) =
      let pin = py < c
          qin = qy < c
          s = (c - py) / (qy - py)
      in [p | pin] ++ [(px + s * (qx - px), c) | pin /= qin]

-- 面積と重心
areaCentroid :: [Point] -> (Float, Float, Float)
areaCentroid poly =
  let terms = [ (cr, (px + qx) * cr, (py + qy) * cr) | ((px, py), (qx, qy)) <- edges poly, let cr = px * qy - qx * py ]
      a2 = sum [ t | (t, _, _) <- terms ]
      cx = sum [ t | (_, t, _) <- terms ]
      cy = sum [ t | (_, _, t) <- terms ]
      a = a2 / 2
  in if abs a < 1e-9 then (0, 0, 0) else (abs a, cx / (6 * a), cy / (6 * a))

buildTable :: Float -> (U.Vector Float, U.Vector Float)
buildTable zg =
  let (total, _, _) = areaCentroid hull
      target = total * fillRatio
      entry i =
        let a = negate pi + tau * fromIntegral i / fromIntegral nTab
            poly = map (rot a) hull
            bisect :: Int -> Float -> Float -> Float
            bisect 0 lo _ = lo
            bisect k lo hi =
              let mid = (lo + hi) / 2
                  (ar, _, _) = areaCentroid (clipBelow poly mid)
              in if ar < target then bisect (k - 1) mid hi else bisect (k - 1) lo mid
            c = bisect 30 (-2) 2
            (_, bx, _) = areaCentroid (clipBelow poly c)
            (gx, _) = rot a (0, zg)
        in (gx - bx, c)        -- ★ GZ > 0 なら起き上がる向きの力
      tab = map entry [0 .. nTab]
  in (U.fromList (map fst tab), U.fromList (map snd tab))

lookupTab :: U.Vector Float -> Float -> Float
lookupTab tab a0 =
  let a = wrapF tau (a0 + pi) - pi
      f = (a + pi) / tau * fromIntegral nTab
      i = max 0 (min (nTab - 1) (floor f))
  in tab U.! i + (tab U.! (i + 1) - tab U.! i) * (f - fromIntegral i)

mTotal :: World -> Float
mTotal w = wM w + (if wWaves w then 0.12 * sin (wT w) else 0)

stepWorld :: World -> World
stepWorld w0 =
  let w = w0 { wM = if wAuto w0 then min 0.6 (wM w0 + 0.0004) else wM w0
             , wT = if wWaves w0 then wT w0 + 0.03 else wT w0 }
      mt = mTotal w
      go :: Int -> Float -> Float -> (Float, Float)
      go 0 p o = (p, o)
      go k p o =
        let dt = 1 / 360
            o1 = clampF (-20) 20 (o + (negate (lookupTab (wGz w) p) * 30 + mt * 30 - 1.2 * o) * dt * 6)
        in go (k - 1) (p + o1 * dt * 6) o1
      (phi, om) = go 6 (wPhi w) (wOmega w)
  in w { wPhi = wrapF tau (phi + pi) - pi, wOmega = om }

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      phi = wPhi w
      mt = mTotal w
      capsized = abs phi > pi / 2
      -- 左：船
      lw = wd * 0.5
      u = min (lw / 4.5) 130
      cx = lw / 2 + 10
      wy = h * 0.55
      c = lookupTab (wC w) phi
      toS p = let (x, y) = rot phi p in (cx + x * u, wy - (y - c) * u)
      seaY = wy + (if wWaves w then 6 * sin (wT w * 2) else 0)
      (_, bx, by) = areaCentroid (clipBelow (map (rot phi) hull) c)
      gs@(gsx, gsy) = toS (0, wZg w)
      bs@(bsx, bsy) = (cx + bx * u, wy - (by - c) * u)
      wl = mt * 300
      -- 右：GZ 曲線
      gx = lw + 30
      gy = 110
      gw = wd - gx - 30
      gh = h - 150
      gmax = max 1e-6 (U.maximum (U.map abs (wGz w)))
      toA a = gx + (a + pi) / tau * gw
      toG v = gy + gh / 2 - v / (gmax * 1.2) * (gh / 2 - 20)
      curve = [ (toA (negate pi + tau * fromIntegral i / fromIntegral nTab), toG v) | (i, v) <- zip [0 :: Int ..] (U.toList (wGz w)) ]
  in Pictures
    [ onP5 env (Pictures
        [ Color (hsbA 205 70 45 80) (box 0 seaY (lw + 20) (h - wy))
        , Color (if capsized then hsb 0 60 70 else hsb 30 30 85) (Polygon (map toS hull))
          -- マスト
        , Color (gray 0.9) (thickLine 4 (toS (0, 0.6)) (toS (0, 1.8)))
        , Color (hsbA 0 0 95 80) (Polygon [toS (0.05, 1.7), toS (0.05, 0.75), toS (0.8, 0.75)])
          -- G と B
        , Color (hsb 0 80 100) (at gs (disc 7))
        , Color (hsb 190 80 100) (at bs (disc 7))
        , Color (hsb 0 80 100) (thickLine 2 gs (gsx, gsy + 50))
        , Color (hsb 190 80 100) (thickLine 2 bs (bsx, bsy - 50))
          -- 風の矢印
        , Color (hsb 50 80 100) (thickLine 5 (cx - wl, 140) (cx, 140))
          -- GZ のグラフ
        , Color (hsb 210 30 20) (box gx gy gw gh)
        , Color (hsbA 0 0 100 15) (Pictures [Line [(gx, toG 0), (gx + gw, toG 0)], Line [(toA 0, gy), (toA 0, gy + gh)]])
        , Color (hsb 30 60 100) (thickPath 3 curve)
        , Color (hsb 50 80 100) (thickLine 2 (gx, toG mt) (gx + gw, toG mt))
        , Color white (at (toA phi, toG (lookupTab (wGz w) phi)) (disc 7))
        ])
    , txt env (gsx + 10) (gsy - 6) 14 white "G"
    , txt env (bsx + 10) (bsy + 16) 14 white "B"
    , if capsized then txt env 30 (h - 40) 30 (hsb 0 80 100) "Capsized! R to right it" else Blank
    , txt env (gx + 12) (gy + 22) 14 (gray 0.85) "Tilt phi across (-180..180), lever GZ up (orange)."
    , txt env (gx + 12) (gy + 44) 14 (gray 0.85) "Yellow = wind M. Crossings = balance angles."
    , txt env (gx + 12) (gy + 66) 14 (gray 0.85) "Yellow above the peak: no crossing = capsize."
    , txt env 20 36 14 white ("phi = " ++ nf 1 (phi * 180 / pi) ++ " deg   wind M = " ++ nf 3 (wM w) ++ " (Up/Dn)   CG " ++ nf 2 (wZg w)
                              ++ " (<- ->)   W: waves " ++ onOff (wWaves w) ++ "   A: ramp " ++ onOff (wAuto w) ++ "   R: right")
    , txt env 20 62 14 (gray 0.6) "The higher the center of gravity G, the lower the GZ peak, and a weaker wind capsizes the ship."
    , txt env 20 84 14 (gray 0.6) "That is why overloaded ships, or a crowd on one side, are dangerous."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _    -> w { wAuto = False, wM = min 0.6 (wM w + 0.01) }
  EventKey (SpecialKey KeyDown) Down _ _  -> w { wAuto = False, wM = max (-0.6) (wM w - 0.01) }
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild (min 0.8 (wZg w + 0.05))
  EventKey (SpecialKey KeyLeft) Down _ _  -> rebuild (max (-0.5) (wZg w - 0.05))
  EventKey (Char c) Down _ _
    | c `elem` "wW" -> w { wWaves = not (wWaves w) }
    | c `elem` "aA" -> let a = not (wAuto w) in w { wAuto = a, wM = if a then 0 else wM w }
    | c `elem` "rR" -> w { wPhi = 0, wOmega = 0, wM = 0, wAuto = False }
  _ -> w
  where
    rebuild z = let (g, c) = buildTable z in w { wZg = z, wGz = g, wC = c }

-- ---------- やってみよう ----------
-- 1. A で風をゆっくり強め、転覆した M の値を記録しよう。重心を上げる（→）とその値はどうなる？
-- 2. 転覆したあと M を 0 に戻しても、船は起き上がらない。GZ のグラフで理由を説明しよう
-- 3. 波（W）があると、GZ の山より弱い風でも転覆することがある。なぜ？
-- 4. hull の形を変えよう：幅を広くする、船底を V 字にする（点を 1 つ足す）。GZ の山はどう変わる？
--    （へこんだ形にしたら、描くときに三角形に分けること。gloss はへこんだ多角形を塗れない）


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
