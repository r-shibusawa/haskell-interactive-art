-- ============================================
-- 05 外積と線分の交差判定
-- 外積 (b − a) × (c − a) の「符号」だけで、c が線分 a→b の左にあるか右にあるかがわかる
-- 2 本の線分は「互いに相手の両側にまたがっている」とき交わる
-- 一直線上（外積 = 0）のときだけ、範囲に入っているか（ON-SEGMENT）を調べる
-- 操作：点をドラッグ / R = ランダム / 1〜4 = お手本（交差・離れている・一直線・端が接する） / A = 面積の表示
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Numeric (showFFloat)

data World = World
  { wPts  :: ![Point]   -- p1, p2, p3, p4（p5.js と同じ座標）
  , wDrag :: !Int       -- つかんでいる点（-1 = なし）
  , wArea :: !Bool      -- ★ 外積の大きさ（平行四辺形の面積）を表示する
  }

main :: IO ()
main = runSketch (sketch "17-05 Cross product and segments" (World [] (-1) True))
  { skBack  = cBg
  , skStart = \env -> pure (World (preset env 1) (-1) True)
  , skDraw  = drawW
  , skEvent = onEvent
  }

preset :: Env -> Int -> [Point]
preset env k =
  let cx = envW env * 0.42
      cy = envH env * 0.55
      u = min (envW env) (envH env) * 0.28
      table = case k of
        1 -> [(-1, -0.6), (1, 0.7), (-0.8, 0.8), (0.9, -0.7)]
        2 -> [(-1, -0.6), (0.2, 0.1), (-0.2, 0.9), (0.9, 0.3)]
        3 -> [(-1, -0.5), (0.3, 0.15), (-0.2, -0.1), (1, 0.5)]
        _ -> [(-1, 0.6), (1, 0.6), (0, 0.6), (0.4, -0.8)]
  in [(cx + x * u, cy + y * u) | (x, y) <- table]

-- 画面は y が下向きなので、y を反転して「数学の向き」で外積を計算する
turn :: Point -> Point -> Point -> Float
turn (ox, oy) (ax, ay) (bx, by) = (ax - ox) * negate (by - oy) - negate (ay - oy) * (bx - ox)

onSegment :: Point -> Point -> Point -> Bool
onSegment (ax, ay) (bx, by) (cx, cy) =
  min ax bx <= cx && cx <= max ax bx && min ay by <= cy && cy <= max ay by

drawW :: Env -> World -> Picture
drawW env w = case wPts w of
  (p1 : p2 : p3 : p4 : _) -> scene env w p1 p2 p3 p4
  _ -> Blank

scene :: Env -> World -> Point -> Point -> Point -> Point -> Picture
scene env w p1 p2 p3 p4 =
  let -- ★ 2 本の線分が交わるか
      d1 = turn p3 p4 p1
      d2 = turn p3 p4 p2
      d3 = turn p1 p2 p3
      d4 = turn p1 p2 p4
      straddle1 = (d1 > 0 && d2 < 0) || (d1 < 0 && d2 > 0)
      straddle2 = (d3 > 0 && d4 < 0) || (d3 < 0 && d4 > 0)
      (hit, why)
        | straddle1 && straddle2 = (True, "Each straddles the other")
        | abs d1 < 1 && onSegment p3 p4 p1 = (True, "p1 lies on segment p3p4")
        | abs d2 < 1 && onSegment p3 p4 p2 = (True, "p2 lies on segment p3p4")
        | abs d3 < 1 && onSegment p1 p2 p3 = (True, "p3 lies on segment p1p2")
        | abs d4 < 1 && onSegment p1 p2 p4 = (True, "p4 lies on segment p1p2")
        | not straddle1 = (False, "p1, p2 on the same side of p3p4")
        | otherwise = (False, "p3, p4 on the same side of p1p2")
      (x1, y1) = p1
      (x2, y2) = p2
      (x3, y3) = p3
      (x4, y4) = p4
      -- 外積の平行四辺形（p1→p2 と p1→p3）
      areaPart
        | wArea w = Pictures
            [ fillPoly env (withA 0.14 (if d3 > 0 then cMint else cBad)) [p1, p2, (x2 + x3 - x1, y2 + y3 - y1), p3]
            , dashed env cDim 1 5 p1 p3
            , dashed env cDim 1 5 p1 p4 ]
        | otherwise = Blank
      -- 交点
      crossMark
        | hit =
            let den = (x2 - x1) * (y4 - y3) - (y2 - y1) * (x4 - x3)
                xp | abs den > 1e-9 =
                       let t = ((x3 - x1) * (y4 - y3) - (y3 - y1) * (x4 - x3)) / den
                       in (x1 + t * (x2 - x1), y1 + t * (y2 - y1))
                   | onSegment p3 p4 p1 = p1
                   | onSegment p3 p4 p2 = p2
                   | onSegment p1 p2 p3 = p3
                   | otherwise = p4
            in hoop env cGold 3 xp (26 + sin (fromIntegral (envFrame env) * 0.15) * 4)
        | otherwise = Blank
      names = ["p1", "p2", "p3", "p4"]
      pointPic (i, p@(x, y), nm) = Pictures
        [ dot env (if i < (2 :: Int) then cOrchid else cMint) p (if wDrag w == i then 22 else 16)
        , txt env (x + 12) (y - 22) 16 cFg nm ]
      -- パネル
      px = envW env - 400
      py = 90
      row (k, nm, lab, v) =
        let y = py + 60 + fromIntegral (k :: Int) * 34
            side | abs v < 1 = " line 0"
                 | v > 0 = " left +"
                 | otherwise = " right -"
        in Pictures
             [ txt env (px + 20) y 15 (if k < 2 then cMint else cOrchid) nm
             , txt env (px + 60) y 15 cFg (lab ++ side)
             , txt env (px + 285) y 15 (if abs v < 1 then cGold else if v > 0 then cMint else cBad) (fix 1 (v / 1000) ++ "k") ]
      rows = [ (0, "d1", "p1 of p3p4:", d1), (1, "d2", "p2 of p3p4:", d2)
             , (2, "d3", "p3 of p1p2:", d3), (3, "d4", "p4 of p1p2:", d4) ]
      hh = envH env
  in Pictures
       [ areaPart
       , arrowSeg env p1 p2 cOrchid
       , arrowSeg env p3 p4 cMint
       , crossMark
       , Pictures (map pointPic (zip3 [0 ..] [p1, p2, p3, p4] names))
       , box env cPanel px py 370 330
       , txt env (px + 20) (py + 18) 18 cFg "Cross sign (left +, right -)"
       , Pictures (map row rows)
       , txt env (px + 20) (py + 210) 24 (if hit then cMint else cBad) (if hit then "They cross!" else "No crossing")
       , txt env (px + 20) (py + 246) 14 cDim why
       , txt env (px + 20) (py + 290) 14 cDim "Shaded area = |(p2-p1)x(p3-p1)|"
       , txt env 30 24 22 cFg "Cross product and segment intersection"
       , txt env 30 56 14 cDim "Drag points   R random   1-4 examples (cross / apart / collinear / touching)   A show area"
       , txt env 30 (hh - 70) 15 cFg "Cross (b-a)x(c-a) = (bx-ax)(cy-ay) - (by-ay)(cx-ax)"
       , txt env 30 (hh - 48) 15 cFg "No division or trigonometry: robust and fast"
       ]

-- 線分（真ん中に矢印）
arrowSeg :: Env -> Point -> Point -> Color -> Picture
arrowSeg env a@(ax, ay) b@(bx, by) col =
  let ang = atan2 (by - ay) (bx - ax)
      mx = (ax + bx) / 2
      my = (ay + by) / 2
      off (u, v) = (mx + u * cos ang - v * sin ang, my + u * sin ang + v * cos ang)
  in Pictures [seg env col 4 a b, fillPoly env col (map off [(10, 0), (-6, -7), (-6, 7)])]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouse env
        near = [i | (i, p) <- zip [0 ..] (wPts w), dist2 m p < 24]
    in pure w { wDrag = foldl (\_ i -> i) (-1) near }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion _
    | wDrag w >= 0 -> pure w { wPts = [if i == wDrag w then mouse env else p | (i, p) <- zip [0 ..] (wPts w)] }
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> pure w { wPts = preset env (fromEnum c - fromEnum '0') }
    | c `elem` "rR" -> do
        ps <- replicateM 4 ((,) <$> randomRIO (80, envW env - 460) <*> randomRIO (120, envH env - 120))
        pure w { wPts = ps }
    | c `elem` "aA" -> pure w { wArea = not (wArea w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. お手本 3（一直線）で点を少しずつ動かし、d の符号がどこで変わるか見る
-- 2. turn の y の反転をやめると、左右の表示はどうなる？
-- 3. abs v < 1 の「ほぼ 0」のしきい値を変えて、判定がどう変わるか試そう

-- ---------- 道具：色 ----------
cBg, cPanel, cFg, cDim, cOrchid, cMint, cGold, cBad, cEdge, cGreen, cRed, cAmber, cBlue :: Color
cBg     = rgb255 0x14 0x10 0x1f
cPanel  = rgb255 0x22 0x1a 0x33
cFg     = rgb255 0xe9 0xe4 0xf5
cDim    = rgb255 0x8c 0x84 0xa3
cOrchid = rgb255 0xf0 0xa3 0xff
cMint   = rgb255 0x6f 0xe3 0xd0
cGold   = rgb255 0xff 0xcf 0x6b
cBad    = rgb255 0xff 0x6b 0x81
cEdge   = rgb255 0x3a 0x2f 0x52
cGreen  = rgb255 0x2f 0x5e 0x57
cRed    = rgb255 0x5e 0x2a 0x3a
cAmber  = rgb255 0x5a 0x4a 0x1f
cBlue   = rgb255 0x2f 0x4a 0x5e

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置を p5.js の座標で
mouse :: Env -> Point
mouse env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の枠
outline :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
outline env c t x y w h = Color c (thickPath t (map (pt env) [(x - t / 2, y), (x + w + t / 2, y), (x + w, y), (x + w, y + h), (x + w + t / 2, y + h), (x - t / 2, y + h), (x, y + h), (x, y)]))

-- 太さ t の線分（p5.js の line）と折れ線
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

path :: Env -> Color -> Float -> [Point] -> Picture
path env c t ps = Color c (thickPath t (map (pt env) ps))

-- 点線
dashed :: Env -> Color -> Float -> Float -> Point -> Point -> Picture
dashed env c t d a@(x1, y1) b@(x2, y2) =
  let l = dist2 a b
      k = floor (l / (2 * d)) :: Int
      f s = (x1 + (x2 - x1) * s / max l 1e-6, y1 + (y2 - y1) * s / max l 1e-6)
  in Pictures [seg env c t (f (2 * d * fromIntegral i)) (f (min l (2 * d * fromIntegral i + d))) | i <- [0 .. k]]

-- 直径 d の円（p5.js の circle）と、太さ t の輪
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

hoop :: Env -> Color -> Float -> Point -> Float -> Picture
hoop env c t p d = Color c (at (pt env p) (ring (d / 2) t))

-- 塗りつぶしの多角形（凸形のみ）
fillPoly :: Env -> Color -> [Point] -> Picture
fillPoly env c ps = Color c (Polygon (map (pt env) ps))

-- 文字：txt は左上が (x, y)、txtM は左端で上下中央、txtC は中心が (x, y)
txt, txtM, txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env (y + s)) s c str
txtM env x y s c str = label (gx env x) (gy env (y + s / 2)) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str

-- 小数点以下 d 桁（JavaScript の toFixed）
fix :: Int -> Float -> String
fix d v = showFFloat (Just d) v ""

