-- ============================================
-- 06 平坦折りの条件：前川の定理
-- 紙の 1 点（頂点）に集まる折り目が、ぺたんと平らにたためるなら「山の数 − 谷の数 = ±2」
--   だから折り目の本数はかならず偶数。条件を満たさない折り目は、どうやっても平らにならない
-- クリック = 折り目を追加（既存の折り目なら 山 ⇄ 谷）/ D = マウスに近い折り目を消す / Shift = 角度を自由に
-- 1〜4 = 例 / R = ランダム / C = 全部消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (sortOn)
import Numeric (showFFloat)

snapDeg :: Float
snapDeg = 15             -- ★ 角度の吸いつき（度）

-- 折り目：角度（ラジアン）と山か谷か（True = 山）
data Crease = Crease { cA :: !Float, cM :: !Bool }

newtype World = World { wCreases :: [Crease] }

main :: IO ()
main = runSketch (sketch "26-06 Maekawa's Theorem" (World (preset 1)))
  { skBack  = rgb255 34 32 40
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 紙の中心と半分の大きさ
layoutOf :: Env -> (Float, Float, Float)
layoutOf env =
  let half = min (envW env * 0.28) ((envH env - 150) / 2)
  in (envW env * 0.36, 110 + half, half)

deg :: Float -> Float
deg g = g * pi / 180

preset :: Int -> [Crease]
preset k = case k of
  1 -> [Crease (deg 0) True, Crease (deg 90) True, Crease (deg 180) True, Crease (deg 270) False]   -- 半分に 2 回
  2 -> [Crease (deg (fromIntegral g)) (g `mod` 90 == 0) | g <- [0, 45, 135, 180, 225, 315 :: Int]]  -- 風船の基本形
  3 -> [Crease (deg g) (odd i) | (i, g) <- zip [0 :: Int ..] [0, 90, 180, 270]]                      -- 山 2・谷 2（たためない）
  _ -> [Crease (deg g) (not (i == 0 || i == 3)) | (i, g) <- zip [0 :: Int ..] [0, 60, 120, 180, 240, 300]]  -- 6 本（山 4・谷 2）

counts :: [Crease] -> (Int, Int)
counts cs = let m = length (filter cM cs) in (m, length cs - m)

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let (cx, cy, half) = layoutOf env
      cs = wCreases w
      sorted = sortOn cA cs
      n = length cs
      paper = Color (rgb255 246 240 226) (at (sc env (cx, cy)) (rectangleSolid (half * 2) (half * 2)))
      -- 区切られた角（セクター）を色分け
      sectors = concat (zipWith sector [0 :: Int ..] (zip sorted (drop 1 (map cA sorted) ++ map ((+ tau) . cA) (take 1 sorted))))
      sector i (c, a1) =
        let a0 = cA c
            am = (a0 + a1) / 2
            (lx, ly) = sc env (cx + cos am * half * 0.3, cy + sin am * half * 0.3)
        in [ Color (if odd i then rgb255 240 220 200 else rgb255 225 232 240) (pieSlice env (cx, cy) (half * 0.45) a0 a1)
           , labelC lx ly 14 (gray 0.31) (nf 0 ((a1 - a0) * 180 / pi) ++ " deg") ]
      -- 折り目
      creases = map creasePic cs
      creasePic c =
        let e = rayToSquare (cx, cy, half) (cA c)
        in if cM c
             then Color (rgb255 220 70 50) (dashed 3 [16, 6, 3, 6] (sc env (cx, cy)) (sc env e))
             else Color (rgb255 50 110 230) (dashed 3 [9, 7] (sc env (cx, cy)) (sc env e))
      -- マウスの位置に入る折り目の予告
      (mx, my) = mouseXY env
      ma = mouseAngle (cx, cy) (mx, my)
      preview
        | dist2 (mx, my) (cx, cy) > 30 && insideSquare (cx, cy, half) (mx, my) =
            case nearest cs ma of
              Just (_, c, d) | d < 5 * pi / 180 ->
                [Color (rgba255 255 190 40 200) (thickLine 7 (sc env (cx, cy)) (sc env (rayToSquare (cx, cy, half) (cA c))))]
              _ -> [Color (rgba255 0 0 0 60) (thickLine 2 (sc env (cx, cy)) (sc env (rayToSquare (cx, cy, half) ma)))]
        | otherwise = []
      center = Color (gray 0.12) (at (sc env (cx, cy)) (disc 6))
      -- 判定パネル
      (m, v) = counts cs
      ok = abs (m - v) == 2
      tx = cx + half + 50
      y = 140
      boxW = envW env - tx - 20
      (bx, by) = sc env (tx + 190, y + 188)
      panel =
        [ txt env tx y 30 (rgb255 230 90 70) ("Mountain M = " ++ show m)
        , txt env tx (y + 44) 30 (rgb255 90 150 255) ("Valley V = " ++ show v)
        , txt env tx (y + 96) 30 white ("M - V = " ++ show (m - v))
        , txt env tx (y + 136) 16 (gray 0.78) ("Creases: " ++ show n ++ (if even n then " (even)" else " (odd: can never fold flat)"))
        , Color (if ok then rgb255 80 220 140 else rgb255 255 110 90) (rect bx by 380 56)
        , txt env (tx + 16) (y + 196) 20 (gray 0.08) (if ok then "Maekawa OK (M - V = +-2)" else "Maekawa NG")
        ]
        ++ textBox env tx (y + 232) boxW 15 (gray 0.75)
             "Why +-2? Walk once around a flat-folded vertex: each valley turns +180 deg, each mountain -180 deg. One lap turns exactly +-360 deg, so 180V - 180M = +-360 and V - M = +-2."
        ++ textBox env tx (y + 360) boxW 15 (rgb255 255 190 40)
             "Note: Maekawa is only a NECESSARY condition. Even when it holds, bad angles cannot fold flat (see 07 Kawasaki)."
      hud =
        [ txt env 20 40 16 white "Click: add crease / flip M-V   D: delete nearest   Shift: free angle   1-4: examples   R: random   C: clear"
        , txt env 20 68 15 (gray 0.67) "Red dash-dot = mountain fold (M), blue dashes = valley fold (V), seen from the front of the paper."
        ]
  in Pictures ([paper] ++ sectors ++ creases ++ preview ++ [center] ++ panel ++ hud)

mouseAngle :: Point -> Point -> Float
mouseAngle (cx, cy) (mx, my) = let a = atan2 (my - cy) (mx - cx) in if a < 0 then a + tau else a

-- いちばん近い折り目（番号、折り目、角度の差）
nearest :: [Crease] -> Float -> Maybe (Int, Crease, Float)
nearest cs a = foldl pick Nothing (zip [0 ..] cs)
  where
    pick best (i, c) =
      let d = abs (atan2 (sin (cA c - a)) (cos (cA c - a)))
      in case best of
           Just (_, _, bd) | d >= bd -> best
           _ -> Just (i, c, d)

insideSquare :: (Float, Float, Float) -> Point -> Bool
insideSquare (cx, cy, half) (x, y) = abs (x - cx) <= half && abs (y - cy) <= half

rayToSquare :: (Float, Float, Float) -> Float -> Point
rayToSquare (cx, cy, half) a =
  let dx = cos a
      dy = sin a
      t = half / max (abs dx) (abs dy)
  in (cx + dx * t, cy + dy * t)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down mods _ -> pure (press mods)
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> pure (World (preset (fromEnum c - fromEnum '0')))
    | otherwise -> case toLower c of
        'c' -> pure (World [])
        'd' -> pure $ case nearest (wCreases w) (mouseAngle (cxy) (mouseXY env)) of
          Just (i, _, _) -> World (take i (wCreases w) ++ drop (i + 1) (wCreases w))
          Nothing -> w
        'r' -> do
          k <- randomRIO (2, 4 :: Int)
          World <$> replicateM (2 * k) (Crease <$> randomRIO (0, tau) <*> randomRIO (False, True))
        _ -> pure w
  _ -> pure w
  where
    lay@(cx0, cy0, _) = layoutOf env
    cxy = (cx0, cy0)
    press mods =
      let m = mouseXY env
          cs = wCreases w
          a = mouseAngle cxy m
      in if not (insideSquare lay m) || dist2 m cxy < 30 then w else
         case nearest cs a of
           Just (i, _, d) | d < 5 * pi / 180 ->
             World [if j == i then c { cM = not (cM c) } else c | (j, c) <- zip [0 ..] cs]
           _ ->
             let a' | shift mods == Down = a
                    | otherwise = wrapF tau (fromIntegral (round (a * 180 / pi / snapDeg) :: Int) * snapDeg * pi / 180)
             in if length cs < 16 then World (cs ++ [Crease a' True]) else w

-- ---------- やってみよう ----------
-- 1. 1 の例（半分に 2 回折る）の山と谷を 1 本だけ反転すると？ 本物の紙でも確かめよう
-- 2. 本数を奇数にすると M − V = ±2 は絶対に成り立たない。なぜ？
-- 3. R でランダムな折り目をつくり、クリックで山谷を変えて OK にしよう。それで本当にたためるかは 07 で
-- 4. 1 点に 8 本の折り目（風車・鶴の基本形の中心）を描いて、山と谷を決めてみよう

-- ==================================================
-- ここから下は「画面の道具」
-- ==================================================

-- p5.js の座標（左上が原点、y は下向き）→ gloss の座標
sc :: Env -> Point -> Point
sc env (x, y) = (x - envW env / 2, envH env / 2 - y)

mouseXY :: Env -> Point
mouseXY env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sc env (x, y) in label a b size c s

nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""

-- 扇形（180° をこえても大丈夫）
pieSlice :: Env -> Point -> Float -> Float -> Float -> Picture
pieSlice env c@(cx, cy) r a0 a1 =
  let parts = max 1 (ceiling ((a1 - a0) / 2.5)) :: Int     -- 凸になるように分けて塗る
      steps = 16 :: Int
      pt p k = sc env (cx + r * cos a, cy + r * sin a)
        where t = (fromIntegral p + fromIntegral k / fromIntegral steps) / fromIntegral parts
              a = a0 + (a1 - a0) * t
  in Pictures [Polygon (sc env c : [pt p k | k <- [0 .. steps]]) | p <- [0 .. parts - 1]]

textBox :: Env -> Float -> Float -> Float -> Float -> Color -> String -> [Picture]
textBox env x y w size c s =
  [ txt env x (y + size + fromIntegral i * size * 1.3) size c l | (i, l) <- zip [0 :: Int ..] (wrapW w size s) ]

wrapW :: Float -> Float -> String -> [String]
wrapW w size = go "" . words
  where
    maxC = floor (w / (size * 0.72)) :: Int
    go cur [] = [cur | not (null cur)]
    go "" (x : xs) = go x xs
    go cur (x : xs)
      | length cur + 1 + length x <= maxC = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs

dashed :: Float -> [Float] -> Point -> Point -> Picture
dashed w pat a@(ax, ay) (bx, by) =
  let l = dist2 a (bx, by)
      pt s = (ax + (bx - ax) * s / l, ay + (by - ay) * s / l)
      go s (on : off : rest)
        | s >= l = []
        | otherwise = thickLine w (pt s) (pt (min l (s + on))) : go (s + on + off) rest
      go _ _ = []
  in if l < 1e-6 then Blank else Pictures (go 0 (cycle pat))
