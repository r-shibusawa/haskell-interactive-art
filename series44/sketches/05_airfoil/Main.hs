-- ============================================
-- 44-05 翼型 ─ ジューコフスキー変換
-- 円柱まわりの流れ（04）を、写像 z = ζ + c²/ζ で「翼のまわりの流れ」に写す
-- 円の中心をずらすと、厚み（左右）と反り（上下）のある翼になる
-- 循環 Γ は「流れが後縁（とがった後ろの端）からなめらかに離れる」ように決まる（クッタの条件）
--   Γ = 4πUR sin(α + β)　→　揚力係数 C_L ≈ 2π sin(α + β)
-- 操作：左右キー = 迎角 α / 上下キー = 反り / 1・2 = 厚み / K = クッタの条件 ON/OFF / R = 元に戻す
-- ============================================
module Main (main) where

import Art
import qualified Data.IntSet as IS
import Data.Array (listArray, (!))
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

u0 :: Float
u0 = 2.0            -- ★ 一様流の速さ（翼の長さ ≈ 4 を 2 秒で通る）

cc :: Float
cc = 1              -- 写像 z = ζ + c²/ζ の c（翼の長さ ≈ 4c）

nt, nr :: Int
nt = 121            -- 極座標の格子（角度の数）
nr = 34             -- 　　　　　　（半径の数）

data World = World
  { wAlphaDeg :: !Int
  , wCamber   :: !Float      -- ★ 反り（円の中心を上にずらす量）
  , wThick    :: !Float      -- ★ 厚み（円の中心を左にずらす量）
  , wKutta    :: !Bool
  , wT        :: !Float
  , wLast     :: !Float      -- 最後に粒の列を放した時刻
  , wCols     :: ![[Point]]  -- 粒の列（ζ 平面の座標）
  , wLines    :: ![[Point]]  -- 流線（描き直したときだけ計算）
  }

-- 翼の形と流れを決める量
data Geo = Geo { gR :: !Float, gBeta :: !Float, gAl :: !Float, gG :: !Float, gMux :: !Float, gMuy :: !Float
               , gUnit :: !Float, gOx :: !Float, gOy :: !Float }

world0 :: World
world0 = World 6 0.1 0.1 True 0 0 [] []

main :: IO ()
main = runSketch (sketch "44-05 Airfoil by the Joukowski transform" world0)
  { skBack  = hsb 215 60 10
  , skStart = \env -> pure (rebuild env world0)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

geo :: Env -> World -> Geo
geo env w =
  let al = fromIntegral (wAlphaDeg w) * pi / 180
      mux = -(wThick w)
      muy = wCamber w
      r = sqrt ((cc - mux) * (cc - mux) + muy * muy)   -- 円は ζ = c（→ 後縁）を通る
      beta = asin (muy / r)
      g = if wKutta w then 2 * u0 * r * sin (al + beta) else 0   -- ★ クッタの条件（G = Γ/2π）
  in Geo r beta al g mux muy (envW env / 13) (envW env * 0.48) (envH env * 0.55)

-- ζ 平面の点 → 翼の平面 z → 画面（流れが水平になるよう −α 回す）
toScreen :: Geo -> Float -> Float -> Point
toScreen gm zx zy =
  let d = max 1e-6 (zx * zx + zy * zy)
      x = zx + cc * cc * zx / d        -- ★ z = ζ + c²/ζ
      y = zy - cc * cc * zy / d
      ca = cos (gAl gm)
      sa = sin (gAl gm)
  in (gOx gm + (x * ca + y * sa) * gUnit gm, gOy gm - (-x * sa + y * ca) * gUnit gm)

-- ζ 平面での流れ関数 ψ（円柱の流れを α だけ傾けたもの。04 と同じ式）
psiAt :: Geo -> Float -> Float -> Float
psiAt gm r th = u0 * (r - gR gm * gR gm / r) * sin (th - gAl gm) + gG gm * log (r / gR gm)

-- 粒子の動き dζ/dt = (dW/dζ の共役) ÷ |dz/dζ|²
zetaVelocity :: Geo -> Float -> Float -> Point
zetaVelocity gm zx zy =
  let rr = gR gm
      g = gG gm
      dx = zx - gMux gm
      dy = zy - gMuy gm
      ca = cos (gAl gm)
      sa = sin (gAl gm)
      px = dx * ca + dy * sa          -- 流れの向きに合わせた座標
      py = -dx * sa + dy * ca
      r2 = max (px * px + py * py) (rr * rr)
      ax = u0 * (1 - rr * rr * (px * px - py * py) / (r2 * r2)) + g * py / r2
      ay = u0 * (rr * rr * 2 * px * py / (r2 * r2)) + g * px / r2
      wx = ax * ca + ay * sa          -- dW/dζ
      wy = -ax * sa + ay * ca
      z2 = max 1e-6 (zx * zx + zy * zy)
      jx = 1 - cc * cc * (zx * zx - zy * zy) / (z2 * z2)
      jy = cc * cc * 2 * zx * zy / (z2 * z2)
      j2 = max (jx * jx + jy * jy) 0.03
  in (wx / j2, -wy / j2)

-- 円のまわりの極座標の格子で ψ を計算し、格子ごと翼の平面へ写して等高線を引く
rebuild :: Env -> World -> World
rebuild env w =
  let gm = geo env w
      rad j = gR gm * 9 ** (fromIntegral j / fromIntegral (nr - 1))
      ang i = fromIntegral i * 2 * pi / fromIntegral (nt - 1)
      f = U.generate (nt * nr) (\n -> let (j, i) = n `divMod` nt in psiAt gm (rad j) (ang i) / (u0 * 0.22))
      pos = U.generate (nt * nr) (\n -> let (j, i) = n `divMod` nt
                                        in toScreen gm (gMux gm + rad j * cos (ang i)) (gMuy gm + rad j * sin (ang i)))
  in w { wLines = contourChains nt nr (\i j -> pos U.! (i + j * nt)) f 0 }

step :: Env -> Float -> World -> IO World
step env dt0 w = pure $
  let dt = min dt0 0.033
      t = wT w + dt
      gm = geo env w
      al = gAl gm
      -- タイムライン：左から縦 1 列の粒を 0.4 秒ごとに放す（上を通る粒のほうが先に着く！）
      newCol = [ (x * cos al - y * sin al, x * sin al + y * cos al)
               | k <- [-12 .. 12 :: Int], let x = -6.5, let y = fromIntegral k * 0.22 + 0.11 ]
      (cols, lastT) = if t - wLast w > 0.4
                        then (lastN 22 (wCols w ++ [newCol]), t)
                        else (wCols w, wLast w)
      move (x, y) = let (vx, vy) = zetaVelocity gm x y
                        sp = sqrt (vx * vx + vy * vy)
                        lim = min 1 (12 / (sp + 1e-6))
                    in (x + vx * lim * dt, y + vy * lim * dt)
  in w { wT = t, wLast = lastT, wCols = map (map move) cols }

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (hsbA 185 60 100 60) (Pictures (map Line (wLines w))) ]
  -- 粒の列
  ++ concat [ [ Color (hsbA 42 70 100 70) (Line seg) | seg <- splitFar ss ]
              ++ [ dot (hsb 42 60 100) x y 4 | (x, y) <- ss ]
            | col <- wCols w, let ss = map (uncurry (toScreen gm)) col ]
  -- 翼：中を塗り、表面を圧力で色分けする（青 = 低圧、赤 = 高圧）
  ++ [ Color (hsb 215 30 30) (Polygon [a, b, c, d])
     | k <- [0 .. n `div` 2 - 1]
     , let p k' = let th = -gBeta gm + fromIntegral k' * 2 * pi / fromIntegral n
                  in toScreen gm (mux + rr * cos th) (muy + rr * sin th)
     , let a = p k, let b = p (k + 1), let c = p (n - k - 1), let d = p (n - k) ]
  ++ [ Color (hsb (if cp > 0 then 8 else 205) 85 (if cp > 0 then 40 + 60 * cp else 40 - 20 * cp)) (thickLine 5 a b)
     | (k, a, b) <- zip3 [0 .. n - 1] pts (drop 1 pts)
     , let th = (fromIntegral k + 0.5) * 2 * pi / fromIntegral n
     , let zx = mux + rr * cos th
     , let zy = muy + rr * sin th
     , let vt = -2 * u0 * sin (th - al) - g / rr          -- 円の表面の速さ
     , let z2 = zx * zx + zy * zy
     , let jx = 1 - cc * cc * (zx * zx - zy * zy) / (z2 * z2)
     , let jy = cc * cc * 2 * zx * zy / (z2 * z2)
     , let sp = abs vt / sqrt (max (jx * jx + jy * jy) 0.01)   -- 翼の表面の速さ
     , let cp = clampF (-3) 1 (1 - sp * sp / (u0 * u0)) ]
  -- 揚力の矢印
  ++ (if abs len > 2
        then let hy = oy - len + (if len > 0 then 12 else -12)
             in [ Color (hsb 130 70 100) (Pictures [ thickLine 5 (ox, oy) (ox, oy - len)
                                                   , thickLine 5 (ox, oy - len) (ox - 9, hy)
                                                   , thickLine 5 (ox, oy - len) (ox + 9, hy) ]) ]
        else [])
  -- 左下：もとの円（ζ 平面）
  ++ [ Color (hsb 0 0 50) (Line [(ix - 90, iy), (ix + 90, iy)])
     , Color (hsb 0 0 50) (Line [(ix, iy - 90), (ix, iy + 90)])
     , Color (hsb 185 60 100) (Translate (ix + mux * iu) (iy - muy * iu) (ring (rr * iu) 2))
     , dot (hsb 42 90 100) (ix + cc * iu) iy 9
     , dot (hsb 42 90 100) (ix + mux * iu) (iy - muy * iu) 5
     , txt (ix - 90) (iy + 108) 14 (hsb 0 0 65) "circle in the zeta plane (yellow = trailing edge)"
     , txt 20 40 18 (hsb 0 0 100)
         ("Angle of attack = " ++ show (wAlphaDeg w) ++ " deg (Left/Right)   camber " ++ f2 (wCamber w) ++ " (Up/Down)   thickness "
          ++ f2 (wThick w) ++ " (1/2)   Kutta condition " ++ (if wKutta w then "ON" else "OFF") ++ " (K)")
     , txt 20 68 15 (hsb 0 0 80)
         ("Circulation Gamma = " ++ f2 (2 * pi * g) ++ "   lift coefficient C_L = " ++ f2 cl
          ++ "   (thin airfoil theory: 2 pi sin(a + b) = " ++ f2 (2 * pi * sin (al + gBeta gm)) ++ ")")
     , txt 20 94 15 (hsb 0 0 65)
         (if wKutta w
            then "Yellow columns: particles over the top reach the trailing edge FIRST, not at the same time. Fast top = low pressure (blue)."
            else "Gamma = 0: flow wraps around the sharp edge (infinite speed), no lift. Real viscosity forbids this and creates Gamma.") ]
  where
    gm = geo env w
    rr = gR gm
    al = gAl gm
    g = gG gm
    mux = gMux gm
    muy = gMuy gm
    ox = gOx gm
    oy = gOy gm
    n = 180 :: Int
    pts = [ toScreen gm (mux + rr * cos th) (muy + rr * sin th) | k <- [0 .. n], let th = fromIntegral k * 2 * pi / fromIntegral n ]
    cl = pi * g / (u0 * cc)   -- C_L = L / (½ρU² × 4c),  L = ρUΓ
    len = clampF (-200) 200 (cl * 90)
    ix = 110
    iy = envH env - 120
    iu = 55

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> upd w { wAlphaDeg = min 18 (wAlphaDeg w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> upd w { wAlphaDeg = max (-10) (wAlphaDeg w - 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> upd w { wCamber = min 0.3 (wCamber w + 0.02) }
  EventKey (SpecialKey KeyDown) Down _ _ -> upd w { wCamber = max 0 (wCamber w - 0.02) }
  EventKey (Char '1') Down _ _ -> upd w { wThick = max 0.02 (wThick w - 0.02) }
  EventKey (Char '2') Down _ _ -> upd w { wThick = min 0.3 (wThick w + 0.02) }
  EventKey (Char k) Down _ _
    | k `elem` "kK" -> upd w { wKutta = not (wKutta w) }
    | k `elem` "rR" -> upd w { wAlphaDeg = 6, wCamber = 0.1, wThick = 0.1, wKutta = True }
  _ -> w
  where
    r2 v = fromIntegral (round (v * 100) :: Int) / 100
    upd v = rebuild env v { wCamber = r2 (wCamber v), wThick = r2 (wThick v), wCols = [] }

-- ---------- やってみよう ----------
-- 1. 迎角を 0° → 12° と上げる。C_L はほぼ直線で増える（1° あたり約 0.11）
-- 2. 反りを 0 にして α = 0° にすると揚力は？ 反りがあると α = 0° でも揚力が出るのはなぜ？
-- 3. K でクッタの条件を切る。後縁のまわりの流線と、粒の列の進み方を比べる
-- 4. 厚みを 0.02 にすると「板」に近づく。厚みを変えても C_L があまり変わらないことを確かめる
-- 5. この理論には「失速」がない（粘性を無視しているから）。本物の翼は α が 15° 前後で流れがはがれる

-- ---------- 道具（等高線を描く） ----------
-- 等高線（マーチングスクエア法）：nx × ny の格子の値 f が「整数」になる所を線で結ぶ。
-- pos i j = 格子点の画面座標。wrap > 0 なら、値が wrap だけ飛ぶ「切れ目」をまたいでつなぐ。
-- 小さな線分をたくさん描くと遅いので、端がつながる線分を 1 本の折れ線にまとめて返す
contourChains :: Int -> Int -> (Int -> Int -> Point) -> U.Vector Float -> Float -> [[Point]]
contourChains nx ny pos f wrap = chainSegments segs
  where
    segs = concat [ cellSegs i j | j <- [0 .. ny - 2], i <- [0 .. nx - 2] ]
    -- 格子の辺の番号（横の辺は偶数、縦の辺は奇数）
    hEdge i j = 2 * (i + j * nx)
    vEdge i j = 2 * (i + j * nx) + 1
    cellSegs i j =
      let raw = [f U.! (i + j * nx), f U.! (i + 1 + j * nx), f U.! (i + 1 + (j + 1) * nx), f U.! (i + (j + 1) * nx)]
          c0 = head raw
          c = c0 : [ if wrap > 0 then v - wrap * fromIntegral (round ((v - c0) / wrap) :: Int) else v | v <- drop 1 raw ]
          lo = minimum c
          hi = maximum c
          ps = [pos i j, pos (i + 1) j, pos (i + 1) (j + 1), pos i (j + 1)]
          es = [hEdge i j, vEdge (i + 1) j, hEdge i (j + 1), vEdge i j]
          corner = zip3 c ps es
          sides = zip corner (drop 1 corner ++ take 1 corner)
          crossings lv = [ ((e, lv), (ax + s * (bx - ax), ay + s * (by - ay)))
                         | ((a, (ax, ay), e), (b, (bx, by), _)) <- sides
                         , (a < fromIntegral lv) /= (b < fromIntegral lv)
                         , let s = (fromIntegral lv - a) / (b - a) ]
          pairUp (p : q : rest) = (p, q) : pairUp rest
          pairUp _ = []
      in if not (hi - lo < 5) then []   -- 特異点のすぐそば（値が急変）は描かない
         else concat [ pairUp (crossings lv) | lv <- [ceiling lo .. floor hi :: Int] ]

-- 端点（辺の番号と高さの組）を共有する線分をつないで折れ線にする
chainSegments :: [(((Int, Int), Point), ((Int, Int), Point))] -> [[Point]]
chainSegments segList = go 0 IS.empty
  where
    n = length segList
    segV = listArray (0, max 0 (n - 1)) segList
    key (e, lv) = e * 65536 + (lv `mod` 65536)   -- 辺と高さの組を 1 つの整数に
    adj = IM.fromListWith (++) (concat [ [(key ka, [s]), (key kb, [s])] | (s, ((ka, _), (kb, _))) <- zip [0 ..] segList ])
    seg s = segV ! s
    go i used
      | i >= n = []
      | IS.member i used = go (i + 1) used
      | otherwise =
          let ((ka, pa), (kb, pb)) = seg i
              (fwd, u1) = extend kb (IS.insert i used)
              (back, u2) = extend ka u1
          in (reverse back ++ [pa, pb] ++ fwd) : go (i + 1) u2
    -- 端点 k から、まだ使っていない線分をたどれるだけたどる
    extend k used = case [ t | t <- IM.findWithDefault [] (key k) adj, not (IS.member t used) ] of
      (t : _) ->
        let ((ka, pa), (kb, pb)) = seg t
            (nk, np) = if ka == k then (kb, pb) else (ka, pa)
            (rest, u') = extend nk (IS.insert t used)
        in (np : rest, u')
      [] -> ([], used)

-- ---------- 道具 ----------
f2 :: Float -> String
f2 v = showFFloat (Just 2) v ""

-- 最後の n 個だけ残す
lastN :: Int -> [a] -> [a]
lastN k xs = drop (length xs - k) xs

-- 隣どうしが 50 px 以上はなれた所で折れ線を切る
splitFar :: [Point] -> [[Point]]
splitFar [] = []
splitFar (p : ps) = go [p] p ps
  where
    go acc _ [] = [reverse acc]
    go acc q (r : rs)
      | dist2 q r < 50 = go (r : acc) r rs
      | otherwise = reverse acc : go [r] r rs

view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))
