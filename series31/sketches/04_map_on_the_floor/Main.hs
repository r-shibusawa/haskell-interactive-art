-- ============================================
-- 04 地図の不動点（部屋の地図を床に置く）
-- 部屋の地図を、その部屋の床に置く。どう置いても（回しても・縮めても）地図上の 1 点だけが「真下の床とぴったり同じ場所」になる
--   小さい地図 = 大きい地図を相似変換 T(p) = c + sR(p − O) したもの。不動点は (I − sR) p = c − sR·O を解けば求まる
-- ドラッグ = 小さい地図を動かす / ←→ = 回す / ↑↓ = 大きさ / N = 地図の中の地図（入れ子）/ マウスを乗せる = その点の行き先 / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Numeric (showFFloat)

data World = World
  { wS      :: !Float   -- ★ 縮小率（小さい地図の大きさ）
  , wTheta  :: !Float   -- ★ 回転角
  , wC      :: !Point   -- 小さい地図の左上（大きい地図の左上からの位置を、地図の幅で割った値）
  , wNested :: !Bool
  , wDrag   :: !(Maybe Point)  -- つかんだ位置と小さい地図の左上の差
  }

world0 :: World
world0 = World 0.32 0.35 (0.45, 0.25 / 1.6) False Nothing

main :: IO ()
main = runSketch (sketch "31-04 Map on the Floor" world0)
  { skBack  = hsb 230 30 11
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- レイアウト
bigW, bigH :: Env -> Float
bigW env = min (envW env - 80) ((envH env - 140) * 1.6)
bigH env = bigW env / 1.6

originO :: Env -> Point
originO env = ((envW env - bigW env) / 2, 110)

-- 小さい地図の左上（画面座標）
cornerC :: Env -> World -> Point
cornerC env w = let (ox, oy) = originO env
                    (u, v) = wC w
                in (ox + u * bigW env, oy + v * bigW env)

-- ★ 相似変換 T：大きい地図の点 p → 小さい地図の上の同じ場所
mapT :: Env -> World -> Point -> Point
mapT env w (px, py) =
  let (ox, oy) = originO env
      (cx, cy) = cornerC env w
      dx = px - ox
      dy = py - oy
      cs = cos (wTheta w)
      sn = sin (wTheta w)
      s = wS w
  in (cx + s * (cs * dx - sn * dy), cy + s * (sn * dx + cs * dy))

-- 不動点：p = c + sR(p − O) を 2×2 の連立方程式として解く
fixedPoint :: Env -> World -> Point
fixedPoint env w =
  let (ox, oy) = originO env
      (cx, cy) = cornerC env w
      cs = cos (wTheta w)
      sn = sin (wTheta w)
      s = wS w
      a = 1 - s * cs           -- (I − sR)
      b = s * sn
      cc = negate s * sn
      d = 1 - s * cs
      rx = cx - s * (cs * ox - sn * oy)
      ry = cy - s * (sn * ox + cs * oy)
      det = a * d - b * cc
  in ((d * rx - b * ry) / det, (a * ry - cc * rx) / det)

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = pure w    -- ←→↑↓ は押している間ずっと効く
  { wTheta = wTheta w + (if isDown env (SpecialKey KeyRight) then 0.02 else 0) - (if isDown env (SpecialKey KeyLeft) then 0.02 else 0)
  , wS = (if isDown env (SpecialKey KeyUp) then min 0.9 . (+ 0.005) else id)
         . (if isDown env (SpecialKey KeyDown) then max 0.08 . subtract 0.005 else id) $ wS w
  }

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $
  [ Translate ox oy room ]
  ++ nestedMaps
  ++ trail
  ++ [ hoop 3 (hsb 0 90 100) fp 26
     , ln 3 (hsb 0 90 100) (fx - 18, fy) (fx + 18, fy)
     , ln 3 (hsb 0 90 100) (fx, fy - 18) (fx, fy + 18)
     , spot (hsb 0 90 100) fp 6
     , txt 20 34 15 white ("scale s = " ++ nf 2 (wS w) ++ " (Up/Down)   rotation = " ++ nf 0 (wTheta w * 180 / pi) ++ " deg (Left/Right)   Drag: move map   N: nested " ++ (if wNested w then "ON" else "off") ++ "   R: reset")
     ]
  ++ [ txt 20 60 14 (hsb 0 0 60) l | l <- take 1 descr ]
  ++ [ txt 20 80 14 (hsb 0 0 60) l | l <- drop 1 descr ]
  ++ [ txt 20 (envH env - 12) 14 (hsb 190 80 100) "Cyan: mouse point p -> T(p) -> T^2(p) -> ...   Nested (N): maps inside maps shrink to the fixed point (sketch 02)" ]
  where
    (ox, oy) = originO env
    bw = bigW env
    bh = bigH env
    (cx, cy) = cornerC env w
    room = roomPicture bw bh
    s = wS w
    fp@(fx, fy) = fixedPoint env w
    inside = fx > ox && fx < ox + bw && fy > oy && fy < oy + bh
    depth = if wNested w then 8 else 1 :: Int
    deg = wTheta w * 180 / pi
    -- 1 段ぶんの変換：translate(c) rotate(θ) scale(s) translate(−O)
    oneStep pic = Translate cx cy (Rotate (negate deg) (Scale s s (Translate (negate ox) (negate oy) pic)))
    mapped = Translate ox oy (Pictures [ fillBox (makeColor 0 0 0 0.35) (-bw * 0.015) (-bw * 0.015) (bw * 1.03) (bh + bw * 0.03), room ])
    corners = [(ox, oy), (ox + bw, oy), (ox + bw, oy + bh), (ox, oy + bh), (ox, oy)]
    tn = mapT env w
    nestedMaps =
      concat [ [ nTimes k oneStep mapped, Color (hsb 45 90 100) (thickPath 3 (map (nTimes k tn) corners)) ] | k <- [1 .. depth] ]
    (mx, my) = mouseP env
    hover = mx > ox && mx < ox + bw && my > oy && my < oy + bh && noDrag
    noDrag = case wDrag w of { Nothing -> True; _ -> False }
    -- マウスの点 p → T(p) → T²(p) → … （不動点へ吸い込まれる）
    trail
      | hover =
          let ps = take 7 (iterate tn (mx, my))
              col i = hsba 190 80 100 ((90 - fromIntegral i * 12) / 100)
          in concat [ [ ln 2 (col i) p q, spot (col i) q 7 ] | (i, p, q) <- zip3 [0 :: Int ..] ps (drop 1 ps) ]
             ++ [ spot (hsb 190 80 100) (mx, my) 10 ]
      | otherwise = []
    descr
      | inside = [ "Red cross = fixed point. That spot on the big map and the same spot on the small map coincide on the screen"
                 , "(\"here\" on the map laid on top is really here)." ]
      | otherwise = [ "If the small map sticks out of the room, the fixed point may leave the room"
                    , "(Brouwer's condition \"maps into itself\" is broken)." ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
        (cx, cy) = cornerC env w
    in pure (if insideSmall env w (mx, my) then w { wDrag = Just (mx - cx, my - cy) } else w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = Nothing }
  EventMotion _ -> case wDrag w of
    Just (gx, gy) ->
      let (mx, my) = mouseP env
          (ox, oy) = originO env
      in pure w { wC = ((mx - gx - ox) / bigW env, (my - gy - oy) / bigW env) }
    Nothing -> pure w
  EventKey (Char c) Down _ _
    | toLower c == 'n' -> pure w { wNested = not (wNested w) }
    | toLower c == 'r' -> pure w { wS = wS world0, wTheta = wTheta world0, wC = wC world0 }
  _ -> pure w

-- 小さい地図の中か？：逆変換で大きい地図の座標に戻して判定
insideSmall :: Env -> World -> Point -> Bool
insideSmall env w (x, y) =
  let (cx, cy) = cornerC env w
      dx = x - cx
      dy = y - cy
      cs = cos (negate (wTheta w))
      sn = sin (negate (wTheta w))
      u = (cs * dx - sn * dy) / wS w
      v = (sn * dx + cs * dy) / wS w
  in u >= 0 && u <= bigW env && v >= 0 && v <= bigH env

-- ---------- やってみよう ----------
-- 1. 小さい地図を大きい地図の左上ぴったりに重ね、θ = 0 にすると不動点はどこ？（左上の角）
-- 2. 小さい地図を部屋の外へドラッグすると不動点はどうなる？ 部屋の中に不動点が残るのはどんなとき？
-- 3. s を 0.9 にして回すと不動点はどう動く？ s = 1（同じ大きさ）にすると何が起きる？（det = 0 に注意）
-- 4. roomPicture を書きかえて自分の部屋・教室・キャンパスの地図にしよう

-- ==================================================
-- ここから下は「道具」（部屋の地図を描く）
-- ==================================================
-- 部屋の地図（左上が (0, 0)、y は下向き）
roomPicture :: Float -> Float -> Picture
roomPicture gw gh = Pictures $
  [ fillBox (hsb 35 20 85) 0 0 gw gh ]
  ++ [ Color (hsb 35 15 75) (Line [(gw / 16 * i, 0), (gw / 16 * i, gh)]) | i <- [0 .. 15] ]     -- 床板
  ++ [ fillBox (hsb 20 50 60) (gw * 0.08) (gh * 0.1) (gw * 0.3) (gh * 0.16)                     -- 本棚
     , fillBox (hsb 210 40 65) (gw * 0.62) (gh * 0.62) (gw * 0.3) (gh * 0.24)                   -- ソファ
     , Color (hsb 30 60 70) (Translate (gw * 0.5) (gh * 0.5) (Scale 1 (gh * 0.3 / (gw * 0.22)) (disc (gw * 0.11)))) ] -- テーブル
  ++ [ spot (hsb 120 50 55) (gw * a, gh * b) (gw * 0.06) | (a, b) <- [(0.9, 0.12), (0.1, 0.85), (0.92, 0.45)] ]  -- 植木
  ++ [ fillBox (hsb 0 60 75) (gw * 0.15) (gh * 0.55) (gw * 0.14) (gh * 0.25)                    -- ベッド
     , fillBox white (gw * 0.17) (gh * 0.57) (gw * 0.1) (gh * 0.06)
     , Color wall (Translate (gw * 0.75) 0 (Scale 1 (-1) (ThickArc (-90) 0 (gw * 0.07) 3)))     -- ドア
     , fillBox wall (-e) (-e) (gw + 2 * e) (2 * e)                                              -- 壁
     , fillBox wall (-e) (gh - e) (gw + 2 * e) (2 * e)
     , fillBox wall (-e) (-e) (2 * e) (gh + 2 * e)
     , fillBox wall (gw - e) (-e) (2 * e) (gh + 2 * e)
     , txt (gw * 0.17) (gh * 0.2) t wall "Shelf"
     , txt (gw * 0.71) (gh * 0.75) t wall "Sofa"
     , txt (gw * 0.45) (gh * 0.51) t wall "Table"
     , txt (gw * 0.18) (gh * 0.72) t wall "Bed"
     , txt (gw * 0.77) (gh * 0.08) t wall "Door"
     , txt (gw * 0.93) (gh * 0.94) t wall "N ^"
     ]
  where
    wall = hsb 220 30 25
    t = gw * 0.022
    e = gw * 0.0075

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f = foldr (.) id (replicate n f)

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
