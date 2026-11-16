-- ============================================
-- 45-09 座屈
-- 細長い柱を押すと、つぶれる前に、ある荷重で突然横に曲がる（座屈）
-- オイラーの座屈荷重：Pcr = π² E I / (K L)²。端の支え方で有効長さ K L が変わり、強さは最大 16 倍ちがう
-- 操作：↑↓（押し続ける）または上下ドラッグ = 荷重 P / ←→ = 柱の長さ L / D = 直径 / R = 荷重を 0 に
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

youngE, yieldS, imperfect :: Float
youngE = 206e9            -- ★ ヤング率（Pa、鋼）
yieldS = 250e6            -- ★ 降伏応力（Pa）
imperfect = 0.002         -- ★ 初期不整（最初からあるわずかな曲がり、有効長さに対する比）

-- ★ 端末条件：名前、有効長さ係数 K、絵の倍率、座屈モードの形（u = 0 が下端、1 が上端）
data End = End { eTitle :: String, eK :: Float, eGain :: Float, eShape :: Float -> Float }

ends :: [End]
ends =
  [ End "Pinned-pinned" 1.0 1.0 (\u -> sin (pi * u))
  , End "Fixed-free" 2.0 1.0 (\u -> 1 - cos (pi * u / 2))
  , End "Fixed-fixed" 0.5 2.0 (\u -> (1 - cos (2 * pi * u)) / 2)
  , End "Fixed-pinned" 0.699 1.5 (\u -> (sin (4.4934 * u) - 4.4934 * cos (4.4934 * u) + 4.4934 * (1 - u)) / 6.283) ]

data World = World
  { wLen   :: !Float   -- ★ 柱の長さ L（m）
  , wDia   :: !Float   -- ★ 丸棒の直径 d（mm）
  , wP     :: !Float   -- 荷重 P（N）
  , wPrevY :: !Float   -- 1 つ前のマウスの y（ドラッグ用）
  }

main :: IO ()
main = runSketch (sketch "45-09 Buckling" (World 1.0 10 100 0))
  { skBack  = hsb 212 45 13
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

secI, secA :: World -> Float
secI w = pi * (wDia w / 1000) ^ (4 :: Int) / 64      -- 断面二次モーメント
secA w = pi * (wDia w / 1000) ^ (2 :: Int) / 4       -- 断面積

critical :: World -> End -> Float
critical w en = pi * pi * youngE * secI w / (eK en * wLen w) ^ (2 :: Int)

-- ★ 横たわみの大きさ δ：初期不整つきの柱のつり合い Pcr δ (1 + k δ²) = P (δ + e0) を 2 分法で解く
--   P < Pcr ではほぼ 0、Pcr を超えると急に大きくなる（k = π²/8Le² はエラスティカの近似）
deflection :: World -> End -> Float
deflection w en = min (bisect (40 :: Int) 0 le) (0.25 * wLen w / eGain en)   -- 絵がくずれないように上限をつける
  where
    le = eK en * wLen w
    pcr = critical w en
    e0 = imperfect * le
    k = pi * pi / (8 * le * le)
    bisect 0 lo _ = lo
    bisect n lo hi =
      let mid = (lo + hi) / 2
      in if pcr * mid * (1 + k * mid * mid) - wP w * (mid + e0) < 0 then bisect (n - 1) mid hi else bisect (n - 1) lo mid

step :: Env -> World -> World
step env w = w { wP = clampF 10 60000 p2 }
  where
    p1 = if isDown env (SpecialKey KeyUp) then wP w * 1.02 else wP w
    p2 = if isDown env (SpecialKey KeyDown) then p1 / 1.02 else p1

draw :: Env -> World -> Picture
draw env w = view env $
  concat (zipWith column [0 ..] ends)
  -- ---- 右：オイラー曲線（細長比 λ = Le / k と 座屈応力 σcr = π²E / λ²）。両対数グラフでは傾き −2 の直線
  ++ [ seg 1 (hsb 0 0 45) (gx, gy) (gx, gy + gh), seg 1 (hsb 0 0 45) (gx, gy + gh) (gx + gw, gy + gh)
     , path 2.5 (hsb 200 60 100) [ (xL l, yS (pi * pi * youngE / (l * l))) | l <- takeWhile (<= 1000) (iterate (* 1.1) 20) ]
     , seg 1.5 (hsbA 320 60 100 80) (gx, yS yieldS) (gx + gw, yS yieldS)    -- 降伏
     , seg 1.5 (hsb 50 90 100) (gx, yS sigma) (gx + gw, yS sigma)           -- いまの応力
     , txt (gx + gw - 200) (yS yieldS - 6) 14 (hsb 320 50 100) "yield sy (short columns)"
     , txt (gx + 8) (yS sigma - 6) 14 (hsb 50 90 100) ("stress now P/A = " ++ fmt 1 (sigma / 1e6) ++ " MPa")
     , txt (gx + 10) (gy + gh - 12) 14 (hsb 200 50 100) "Euler: s_cr = pi^2 E / lam^2 (slope -2)"
     , txt (gx + gw - 200) (gy + gh + 36) 14 (hsb 0 0 60) "slenderness lam = Le/k (log)"
     , txt (gx - 4) (gy - 10) 14 (hsb 0 0 60) "stress (MPa, log)" ]
  ++ [ txt (xL l - 10) (gy + gh + 18) 14 (hsb 0 0 60) (show (round l :: Int)) | l <- [20, 50, 100, 200, 500, 1000] ]
  ++ [ txt (gx - 42) (yS (v * 1e6) + 5) 14 (hsb 0 0 60) (show (round v :: Int)) | v <- [1, 10, 100, 1000] ]
  ++ concat [ [ dot (hsb (if wP w > critical w en then 0 else 140) 70 100) (xL lam) (yS scr) 12
              , txt (labelX (xL lam) (eTitle en)) (yS scr - 7) 14 (hsb 0 0 100) (eTitle en) ]
            | en <- ends, let lam = eK en * wLen w / rg, let scr = pi * pi * youngE / (lam * lam) ]
  -- ---- 文字
  ++ [ txt 20 40 16 (hsb 0 0 100) ("Up/Down or drag: load P = " ++ fmt 0 (wP w) ++ " N   Left/Right: L = " ++ fmt 1 (wLen w)
                                   ++ " m   D: d = " ++ fmt 0 (wDia w) ++ " mm   R: reset load")
     , txt 20 68 15 (hsb 0 0 60) "Same rod, same load: the end supports decide the buckling order. Past Pcr the side bend jumps up."
     , Color (hsbA 0 0 100 8) (rect (ix - 14 + bw / 2) (iy - 26 + 62) bw 124)
     , txt ix iy 16 (hsb 0 0 100) "Pcr = pi^2 E I / (K L)^2,  I = pi d^4 / 64"
     , txt ix (iy + 24) 14 (hsb 0 0 70) ("I = " ++ fmt 0 (secI w * 1e12) ++ " mm^4  A = " ++ fmt 1 (secA w * 1e6) ++ " mm^2  E = "
                                         ++ fmt 0 (youngE / 1e9) ++ " GPa")
     , txt ix (iy + 48) 14 (if crushed then hsb 0 80 100 else hsb 0 0 70)
         (if crushed then "Stress > yield: crushes before buckling" else "2x length -> Pcr / 4,  2x diameter -> Pcr x 16")
     , txt ix (iy + 72) 14 (hsb 0 0 55) ("Imperfection " ++ fmt 3 imperfect ++ " x Le: bends just before Pcr") ]
  where
    pxPerM = min 330 ((envH env - 300) / wLen w)
    baseY = envH env - 120
    colW = envW env * 0.56 / 4
    sigma = wP w / secA w
    crushed = sigma >= yieldS
    gx = envW env * 0.64
    gy = 150
    gw = envW env - gx - 50
    gh = envH env - gy - 190
    -- 点のラベルは右にはみ出すときだけ左側に書く
    labelX x s = if x + 9 + fromIntegral (length s) * 11.2 > envW env - 6 then x - 9 - fromIntegral (length s) * 11.2 else x + 9
    rg = wDia w / 1000 / 4                                -- 丸棒の断面二次半径 k = d/4
    log10 = logBase 10
    xL l = gx + (log10 (clampF 20 1000 l) - log10 20) / (3 - log10 20) * gw
    yS sg = gy + gh - log10 (clampF 1 1000 (sg / 1e6)) / 3 * gh
    ix = envW env * 0.6
    iy = envH env - 112
    bw = envW env - ix - 6
    column :: Int -> End -> [Picture]
    column c en =
      let x0 = 50 + colW * (fromIntegral c + 0.5)
          pcr = critical w en
          dlt = deflection w en * eGain en
          buckled = wP w > pcr
          -- 柱の形：横たわみ w(u) を決め、弧の長さが変わらないように縦方向を縮める
          n = 60 :: Int
          ds = wLen w / fromIntegral n
          build (acc@((wPrev, hNow) : _)) i =
            let wv = dlt * eShape en (fromIntegral i / fromIntegral n)
                dw = wv - wPrev
            in (wv, hNow + sqrt (max (ds * ds - dw * dw) 0)) : acc
          build [] _ = []
          pts = reverse (foldl build [(0, 0)] [1 .. n])
          (wTop, hTop) = case pts of
            [] -> (0, 0)
            _ -> last pts
          topY = baseY - hTop * pxPerM
          topX = x0 + wTop * pxPerM
          supp = Color (hsb 0 0 40) . Pictures
          al = 20 + logBase 10 (wP w) * 14
          ay = topY - (if c == 2 then 20 else 8)
      in [ supp ((if c == 0 then [Polygon [(x0, baseY), (x0 - 14, baseY + 22), (x0 + 14, baseY + 22)]]
                               else [rect x0 (baseY + 9) 68 18])
                 ++ (if c == 0 || c == 3 then [rect (topX - 22) topY 8 24, rect (topX + 22) topY 8 24] else [])   -- 横に動けないガイド
                 ++ (if c == 2 then [rect topX (topY - 9) 68 14] else []))
         -- 柱
         , path (clampF 3 14 (wDia w * 0.6)) (if buckled then hsb 0 75 100 else hsb 200 50 100)
                [ (x0 + px * pxPerM, baseY - py * pxPerM) | (px, py) <- pts ]
         -- 荷重の矢印
         , path 3 (hsb 50 90 100) [(topX, ay - al), (topX, ay)]
         , path 3 (hsb 50 90 100) [(topX - 7, ay - 10), (topX, ay), (topX + 7, ay - 10)]
         -- ラベル
         , txt (x0 - 60) (baseY + 38) 15 (hsb 0 0 100) (eTitle en)
         , txt (x0 - 60) (baseY + 56) 14 (hsb 0 0 65) ("K = " ++ show (eK en))
         , txt (x0 - 60) (baseY + 73) 14 (hsb 0 0 65) ("Le = " ++ fmt 2 (eK en * wLen w) ++ " m")
         , txt (x0 - 60) (baseY + 91) 14 (hsb (if buckled then 0 else 140) 70 100) ("Pcr = " ++ fmt 0 pcr ++ " N") ]
         ++ [ txt (x0 - 60) (baseY + 109) 15 (hsb 0 70 100) "BUCKLED!" | buckled ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventMotion _
    | envDown env -> w { wP = clampF 10 60000 (wP w * 1.01 ** (my - wPrevY w)), wPrevY = my }
    | otherwise -> w { wPrevY = my }
  EventKey (MouseButton LeftButton) Down _ _ -> w { wPrevY = my }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wLen = clampF 0.3 1.5 (wLen w - 0.1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wLen = clampF 0.3 1.5 (wLen w + 0.1) }
  EventKey (Char k) Down _ _
    | k `elem` "dD" -> w { wDia = if wDia w >= 20 then 6 else wDia w + 2 }
    | k `elem` "rR" -> w { wP = 100 }
  _ -> w
  where (_, my) = mouseP env

-- ---------- やってみよう ----------
-- 1. ↑ を押し続けて、座屈する順番を見よう。固定–自由 → 両端ピン → 固定–ピン → 両端固定。Pcr の比は 1 : 4 : 8.2 : 16
-- 2. L を半分にすると Pcr は 4 倍。定規を両手で押して、長いときと短く持ったときを比べてみよう
-- 3. D で直径を太くする。I は d の 4 乗なので急に強くなる。同じ重さなら中空パイプが有利な理由を考えよう
-- 4. L = 0.3 m、d = 20 mm にすると、座屈より先に降伏する（右のグラフで点がピンクの線より上にある）
-- 5. imperfect を 0.02 にすると「突然」ではなくなる。現実の柱が Pcr より小さい荷重で設計される理由

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線（太い線はつなぎ目に円を置いてなめらかにする）
path :: Float -> Color -> [Point] -> Picture
path w c ps
  | w <= 1 = Color c (Line ps)
  | otherwise = Pictures (zipWith (seg w c) ps (drop 1 ps) ++ [ dot c x y w | w >= 6, (x, y) <- ps ])

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
