-- ============================================
-- 07 川崎の定理
-- 1 つの頂点に集まる折り目が平らにたためるのは、間の角を 1 つおきに足すと 180° になるとき
--   θ1 − θ2 + θ3 − θ4 + … = 0 ⇔ θ1 + θ3 + … = θ2 + θ4 + … = 180°
-- ドラッグ = 折り目の端（●）を動かす / 4・6・8 = 折り目の本数 / K = 最後に動かした折り目で条件を満たす / R = ランダム
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (sortOn)
import Numeric (showFFloat)

gapPx :: Float
gapPx = 9                   -- ★ 右の「たたんだ図」で層をずらす幅

data World = World
  { wAngs :: [Float]        -- 折り目の角度（ラジアン、小さい順）
  , wDrag :: !Int           -- 動かしている折り目（-1 = なし）
  , wLast :: !Int           -- 最後に動かした折り目
  }

main :: IO ()
main = runSketch (sketch "26-07 Kawasaki's Theorem" (World [] (-1) 0))
  { skBack  = rgb255 34 32 40
  , skStart = \_ -> (\a -> World a (-1) 0) <$> randomize 4
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 円の半径と中心
layoutOf :: Env -> (Float, Float, Float)
layoutOf env =
  let r = min (envW env * 0.2) ((envH env - 150) / 2.3)
  in (envW env * 0.27, 100 + r * 1.1, r)

randomize :: Int -> IO [Float]
randomize n = do
  as <- mapM (\i -> do
                 d <- randomRIO (-0.3, 0.3)
                 pure ((fromIntegral i + d) * tau / fromIntegral n + 0.3)) [0 .. n - 1]
  pure (sortOn id (map (wrapF tau) as))

-- 間の角 θ_i（折り目 i から i+1 まで）
sectors :: [Float] -> [Float]
sectors as = zipWith (-) (drop 1 as ++ map (+ tau) (take 1 as)) as

altSum :: [Float] -> Float
altSum th = sum [if odd i then -t else t | (i, t) <- zip [0 :: Int ..] th]

-- 折り目 j の両どなりの角度（動かせる範囲）
neighbors :: [Float] -> Int -> (Float, Float)
neighbors as j =
  let n = length as
      get i = case drop i as of { x : _ -> x; [] -> 0 }
      lo = if j > 0 then get (j - 1) else get (n - 1) - tau
      hi = if j < n - 1 then get (j + 1) else get 0 + tau
  in (lo, hi)

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = take i xs ++ [v] ++ drop (i + 1) xs

-- 並べ直して、動かした折り目 j の新しい番号も返す
resort :: Int -> [Float] -> ([Float], Int)
resort j as =
  let tagged = sortOn fst [(wrapF tau a, i) | (i, a) <- zip [0 ..] as]
      j' = case [k | (k, (_, i)) <- zip [0 ..] tagged, i == j] of
             k : _ -> k
             [] -> -1
  in (map fst tagged, j')

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | wDrag w < 0 = pure w
  | otherwise =
      let (cx, cy, _) = layoutOf env
          (mx, my) = mouseXY env
          j = wDrag w
          (lo, hi) = neighbors (wAngs w) j
          a0 = atan2 (my - cy) (mx - cx)
          a1 = until (>= lo) (+ tau) a0
          a2 = until (<= hi) (subtract tau) a1
          as' = setAt j (clampF (lo + 0.04) (hi - 0.04) a2) (wAngs w)
          (as'', j') = resort j as'
      in pure w { wAngs = as'', wDrag = j', wLast = j' }

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let (cx, cy, r) = layoutOf env
      as = wAngs w
      th = sectors as
      s = altSum th
      ok = abs (s * 180 / pi) < 0.5
      c0 = sc env (cx, cy)
      -- 左：展開図（紙と折り目）
      paper = Color (rgb255 246 240 226) (at c0 (disc (r * 1.1)))
      pies = [ Color (if odd i then rgb255 240 200 170 else rgb255 190 215 240) (pieSlice env (cx, cy) (r * 0.6) a (a + t))
             | (i, a, t) <- zip3 [0 :: Int ..] as th ]
      pieLabels = concat
        [ let am = a + t / 2
              (lx, ly) = sc env (cx + cos am * r * 0.85, cy + sin am * r * 0.85)
          in [ labelC lx (ly + 9) 15 (gray 0.16) ("th" ++ show (i + 1))
             , labelC lx (ly - 9) 15 (gray 0.16) (nf 1 (t * 180 / pi)) ]
        | (i, a, t) <- zip3 [0 :: Int ..] as th ]
      crease = concat
        [ [ Color (if i == wLast w then rgb255 255 170 30 else gray 0.35)
              (thickLine 3 c0 (sc env (cx + cos a * r * 1.1, cy + sin a * r * 1.1)))
          , Color (if i == wDrag w then rgb255 255 170 30 else gray 0.16)
              (at (sc env (cx + cos a * r, cy + sin a * r)) (disc 8)) ]
        | (i, a) <- zip [0 ..] as ]
      -- 右：たたんだ図。区切りごとに向きを反転しながら角を足す（φ = θ1 − θ2 + θ3 − …）
      fx = envW env * 0.66
      fy = cy
      layers = zip3 [0 :: Int ..] th (scanl (\p (i, t) -> p + (if odd i then -t else t)) (-(pi / 2)) (zip [0 :: Int ..] th))
      foldPic = concat
        [ let dir = if odd i then -1 else 1
              r0 = r * 0.6 + fromIntegral i * gapPx
              a1 = phi + dir * t
              n = 40 :: Int
              pts = [sc env (fx + r0 * cos a, fy + r0 * sin a)
                    | k <- [0 .. n], let a = phi + (a1 - phi) * fromIntegral k / fromIntegral n]
          in [ Color (if odd i then rgb255 240 160 120 else rgb255 120 170 240) (thickPath 5 pts)
             , Color (gray 0.78) (thickLine 1.5 (sc env (fx, fy)) (sc env (fx + cos a1 * (r0 + gapPx), fy + sin a1 * (r0 + gapPx)))) ]
        | (i, t, phi) <- layers ]
      startPic =
        [ Color white (thickLine 2 (sc env (fx, fy)) (sc env (fx, fy - r * 0.6)))
        , Color white (at (sc env (fx, fy)) (disc 5)) ]
      gapDeg = s * 180 / pi
      okCol = if ok then rgb255 80 220 140 else rgb255 255 110 90
      foldMsg = txt env (fx - r * 0.9) (fy - r * 0.6 - 60) 18 okCol
        (if ok then "Last layer is back at the start: it closes!" else "Off by " ++ nf 1 gapDeg ++ " deg: it does not close (paper tears)")
      -- パネル
      odds = sum [t | (i, t) <- zip [0 :: Int ..] th, even i]
      evens = sum [t | (i, t) <- zip [0 :: Int ..] th, odd i]
      tx = envW env * 0.47
      y = envH env - 120
      panel =
        [ txt env tx y 22 (rgb255 120 170 240) ("th1 + th3 + ... = " ++ nf 1 (odds * 180 / pi) ++ " deg")
        , txt env tx (y + 32) 22 (rgb255 240 160 120) ("th2 + th4 + ... = " ++ nf 1 (evens * 180 / pi) ++ " deg")
        , txt env tx (y + 70) 22 okCol (if ok then "Kawasaki OK" else "Kawasaki NG (make both 180 deg)")
        ]
      hud =
        [ txt env 20 40 18 white "Drag: crease ends (dots)   4, 6, 8: number of creases   K: fix with the orange crease   R: random"
        , txt env 20 68 15 (gray 0.67) "Left = unfolded paper. Right = layers when folded flat: angles stack in alternating directions."
        ]
  in Pictures ([paper] ++ pies ++ pieLabels ++ crease ++ foldPic ++ startPic ++ [foldMsg] ++ panel ++ hud)

-- ★ K：折り目 j を δ 動かすと θ_{j-1} が +δ、θ_j が −δ → 交互和は 2δ だけ変わる
fixKawasaki :: World -> World
fixKawasaki w =
  let as = wAngs w
      n = length as
      j = wLast w
      s = altSum (sectors as)
      sign = if odd ((j - 1 + n) `mod` n) then -1 else 1
      d = -s / (2 * sign)
      (lo, hi) = neighbors as j
      a = (case drop j as of { x : _ -> x; [] -> 0 }) + d
  in if a > lo + 0.01 && a < hi - 0.01
       then let (as', j') = resort j (setAt j a as) in w { wAngs = as', wLast = j', wDrag = -1 }
       else w

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (cx, cy, r) = layoutOf env
        m = mouseXY env
        hits = [i | (i, a) <- zip [0 ..] (wAngs w), dist2 m (cx + cos a * r, cy + sin a * r) < 22]
    in pure $ case reverse hits of
         i : _ -> w { wDrag = i, wLast = i }
         [] -> w { wDrag = -1 }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventKey (Char c) Down _ _
    | c `elem` "468" -> do
        as <- randomize (fromEnum c - fromEnum '0')
        pure w { wAngs = as, wLast = 0, wDrag = -1 }
    | toLower c == 'r' -> do
        as <- randomize (length (wAngs w))
        pure w { wAngs = as, wDrag = -1 }
    | toLower c == 'k' -> pure (fixKawasaki w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 4 本の折り目で、θ1 + θ3 = 180° にしたまま θ1 だけを変えてみよう。どんな形でもたためる？
-- 2. 右の図の「ずれ」が 0 になる瞬間を、ドラッグだけでつくってみよう
-- 3. 6 本・8 本にして K を押す。どの折り目を動かしても直せる？
-- 4. 06 の前川の定理（山と谷の数）と川崎の定理（角度）。両方そろっても、まだたためない例がある（大・小・大の補題を調べよう）

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

pieSlice :: Env -> Point -> Float -> Float -> Float -> Picture
pieSlice env c@(cx, cy) r a0 a1 =
  let parts = max 1 (ceiling ((a1 - a0) / 2.5)) :: Int     -- 凸になるように分けて塗る
      steps = 16 :: Int
      pt p k = sc env (cx + r * cos a, cy + r * sin a)
        where t = (fromIntegral p + fromIntegral k / fromIntegral steps) / fromIntegral parts
              a = a0 + (a1 - a0) * t
  in Pictures [Polygon (sc env c : [pt p k | k <- [0 .. steps]]) | p <- [0 .. parts - 1]]
