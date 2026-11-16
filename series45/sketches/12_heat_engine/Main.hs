-- ============================================
-- 45-12 熱機関（オットーサイクルとカルノーサイクル）
-- 気体を「圧縮 → 加熱 → 膨張 → 冷却」と 1 周させると、p–V 線図で囲んだ面積のぶんだけ仕事が取り出せる
-- オットー（ガソリンエンジン）：効率 η = 1 − 1 / ε^(κ−1)。カルノー（理想の上限）：η = 1 − Tc / Th
-- 操作：1 = オットー / 2 = カルノー / ←→ = 圧縮比（カルノーは等温膨張の比）/ ↑↓ = 高温側の温度 / L = 対数目盛 / スペース = 止める
-- ============================================
module Main (main) where

import Art
import Data.Array (Array, listArray, (!), elems, bounds)
import Data.List (foldl')
import Numeric (showFFloat)

kappa :: Float
kappa = 1.4               -- 比熱比 κ（空気）

tLow, nR :: Float
tLow = 300                -- 低温側 300 K
nR = 100 / 300            -- 気体の量（V = 1 で 100 kPa になるように）

runSpeed :: Float
runSpeed = 1.2            -- ★ アニメーションの速さ

nPer :: Int
nPer = 60                 -- 1 行程の点の数

-- 状態：体積、温度、圧力、熱の出入り（+1 入る / 0 断熱 / −1 捨てる）
data St = St { sv :: !Float, st :: !Float, sp :: !Float, sheat :: !Int }

data Info = Info { iQin, iEta, iTHigh, iWork, iEtaArea :: !Float, iNames :: ![String] }

data World = World
  { wCycle   :: !Int      -- 1 オットー / 2 カルノー
  , wComp    :: !Float    -- ★ オットーの圧縮比 ε
  , wTmax    :: !Float    -- ★ オットーの最高温度（K）
  , wIso     :: !Float    -- ★ カルノーの等温膨張の体積比
  , wTh      :: !Float    -- ★ カルノーの高温熱源（K）
  , wCyc     :: !(Array Int St)
  , wInfo    :: !Info
  , wPos     :: !Float
  , wPlaying :: !Bool
  , wLog     :: !Bool
  }

world0 :: World
world0 = buildCycle (World 1 8 2000 2.5 600 (listArray (0, 0) [St 1 1 1 0]) (Info 1 0 1 0 0 []) 0 True False)

main :: IO ()
main = runSketch (sketch "45-12 Heat engine - Otto and Carnot" world0)
  { skBack  = hsb 212 45 13
  , skStep  = \_ _ w -> pure (if wPlaying w then w { wPos = wrapF (fromIntegral (nCyc w)) (wPos w + runSpeed) } else w)
  , skDraw  = draw
  , skEvent = event
  }

nCyc :: World -> Int
nCyc w = let (a, b) = bounds (wCyc w) in b - a + 1

-- ★ サイクルを 4 つの行程（各 60 点）で作る。p V = nR T、断熱変化は T V^(κ−1) = 一定
buildCycle :: World -> World
buildCycle w = w { wCyc = arr, wInfo = info { iWork = work, iEtaArea = work / iQin info }
                 , wPos = wrapF (fromIntegral (length pts)) (wPos w) }
  where
    n = nPer
    g = kappa - 1
    put v t h = St v t (nR * t / v) h
    ease u = (1 - cos (pi * u)) / 2                  -- ピストンの動きをなめらかに
    us = [fromIntegral i / fromIntegral n | i <- [0 .. n - 1]]
    eps = wComp w
    (pts, info)
      | wCycle w == 1 =
          let v2 = 1 / eps
              t2 = tLow * eps ** g
              t3 = max (wTmax w) (t2 + 100)
              t4 = t3 * eps ** (-g)
              cv = nR / g
          in ( [let v = 1 + (v2 - 1) * ease u in put v (tLow * (1 / v) ** g) 0 | u <- us]        -- 1→2 断熱圧縮
               ++ [put v2 (t2 + (t3 - t2) * u) 1 | u <- us]                                       -- 2→3 等積加熱（燃焼）
               ++ [let v = v2 + (1 - v2) * ease u in put v (t3 * (v2 / v) ** g) 0 | u <- us]     -- 3→4 断熱膨張
               ++ [put 1 (t4 + (tLow - t4) * u) (-1) | u <- us]                                   -- 4→1 等積冷却（排気）
             , Info (cv * (t3 - t2)) (1 - eps ** (-g)) t3 0 0
                 ["adiabatic compression", "constant-V heating (combustion)", "adiabatic expansion (work)", "constant-V cooling (exhaust)"] )
      | otherwise =
          let th = wTh w
              k = (th / tLow) ** (1 / g)
              va = 1 / (wIso w * k)
              vb = wIso w * va
              vd = va * k
          in ( [put (va + (vb - va) * ease u) th 1 | u <- us]                                     -- a→b 等温膨張（吸熱）
               ++ [let v = vb + (1 - vb) * ease u in put v (th * (vb / v) ** g) 0 | u <- us]     -- b→c 断熱膨張
               ++ [put (1 + (vd - 1) * ease u) tLow (-1) | u <- us]                               -- c→d 等温圧縮（放熱）
               ++ [let v = vd + (va - vd) * ease u in put v (tLow * (vd / v) ** g) 0 | u <- us]  -- d→a 断熱圧縮
             , Info (nR * th * log (wIso w)) (1 - tLow / th) th 0 0
                 ["isothermal expansion (heat in)", "adiabatic expansion", "isothermal compression (heat out)", "adiabatic compression"] )
    arr = listArray (0, length pts - 1) pts
    -- 仕事 = 囲んだ面積 ∮p dV（台形公式）
    work = foldl' (+) 0 [ (sp a + sp b) / 2 * (sv b - sv a) | (a, b) <- zip pts (drop 1 pts ++ take 1 pts) ]

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 左：シリンダーとピストン
  [ Color (hsbA (lerpF 220 0 hot) 70 (lerpF 45 100 hot) 85) (box cx0 (cyl - bore / 2) (pxp - cx0) bore)   -- 気体
  , Color (hsb 0 0 35) (Pictures [ box (cx0 - 14) (cyl - bore / 2 - 12) (cylLen + 80) 12
                                 , box (cx0 - 14) (cyl + bore / 2) (cylLen + 80) 12
                                 , box (cx0 - 14) (cyl - bore / 2 - 12) 14 (bore + 24) ])
  , Color (hsb 40 15 85) (box pxp (cyl - bore / 2) 30 bore)                     -- ピストン
  , seg 8 (hsb 0 0 70) (pxp + 30, cyl) (pxp + 150, cyl) ]
  -- 熱の出入り
  ++ [ txt cx0 (cyl - bore / 2 - 26) 15 (hsb 10 85 100) (if otto then "FIRE: ignition, combustion (heat in)" else "FIRE: heat in from hot source") | sheat now > 0 ]
  ++ [ txt cx0 (cyl - bore / 2 - 26) 15 (hsb 210 60 100) (if otto then "COLD: exhaust (heat out)" else "COLD: heat out to cold sink") | sheat now < 0 ]
  ++ [ txt cx0 (cyl + bore / 2 + 40) 15 (hsb 0 0 100) ("Stroke " ++ show (stroke4 + 1) ++ ": " ++ nameOf stroke4)
     , txt cx0 (cyl + bore / 2 + 64) 14 (hsb 0 0 70) ("V = " ++ fmt 3 (sv now) ++ "   p = " ++ fmt 0 (sp now) ++ " kPa   T = " ++ fmt 0 (st now) ++ " K")
  -- ---- 右：p–V 線図
     , seg 1 (hsb 0 0 45) (gx, gy) (gx, gy + gh), seg 1 (hsb 0 0 45) (gx, gy + gh) (gx + gw, gy + gh)
     , fillPoly (hsbA 35 80 100 22) [(xOf s, yOf s) | s <- cyc]           -- 囲んだ面積 = 仕事
     , Color (hsbA 0 0 100 40) (poly' [(xOf s, yOf s) | s <- cyc]) ]
  ++ [ path 3 (strokeCol (sheat (arr ! (k * nPer))))
         [ let s = arr ! (i `mod` nn) in (xOf s, yOf s) | i <- [k * nPer .. k * nPer + nPer] ]
     | k <- [0 .. 3] ]
  ++ [ dot (hsb 0 0 100) (xOf now) (yOf now) 14 ]
  ++ [ let s = arr ! (k * nPer) in txt (xOf s + 8) (yOf s - 8) 14 (hsb 0 0 100) (if otto then show (k + 1) else ["abcd" !! k])
     | k <- [0 .. 3] ]
  ++ [ txt (gx + gw * 0.5) (gy + gh + 20) 14 (hsb 0 0 60) ("volume V" ++ (if wLog w then " (log)" else ""))
     , txt (gx - 10) (gy - 10) 14 (hsb 0 0 60) ("pressure p (kPa)" ++ (if wLog w then " (log)" else ""))
     , txt (gx - 44) (yOfP pTop + 4) 14 (hsb 0 0 60) (fmt 0 pTop)
     , txt (xOfV 1 - 8) (gy + gh + 20) 14 (hsb 0 0 60) "1.0"
     , txt (xOfV vLow - 12) (gy + gh + 20) 14 (hsb 0 0 60) (fmt 3 vLow)
     , txt gx (gy + gh + 44) 14 (hsb 0 0 70) "red = heat in, blue = heat out, white = adiabatic. Orange area = work per cycle"
  -- ---- 文字
     , txt 20 40 18 (hsb 0 0 100) ("1: Otto  2: Carnot (now " ++ (if otto then "Otto" else "Carnot") ++ ")   " ++ ctl
                                    ++ "   L: log axes   Space: " ++ (if wPlaying w then "stop" else "run"))
     , txt 20 68 15 (hsb 0 0 60) "Put heat Q in, take work W out, dump the rest. Efficiency = W / Qin. Dumped heat can't be 0 (2nd law)."
     , Color (hsbA 0 0 100 8) (box (ix - 14) (iy - 26) (envW env * 0.4) 142)
     , txt ix iy 16 (hsb 0 0 100) (if otto then "Otto: eta = 1 - 1 / e^(k-1)   (k = 1.4)" else "Carnot: eta = 1 - Tc / Th   (Tc = 300 K)")
     , txt ix (iy + 28) 16 (hsb 35 90 100) ("formula eta = " ++ fmt 1 (iEta info * 100) ++ " %    from area = " ++ fmt 1 (iEtaArea info * 100) ++ " %")
     , txt ix (iy + 56) 16 (hsb 190 70 100) ("Qin = " ++ fmt 1 (iQin info) ++ "  W = int p dV = " ++ fmt 1 (iWork info)
                                             ++ "  Qout = " ++ fmt 1 (iQin info - iWork info) ++ " (kPa.V)")
     , txt ix (iy + 82) 14 (hsb 0 0 60) ("Carnot limit for " ++ fmt 0 tLow ++ " K - " ++ fmt 0 (iTHigh info) ++ " K = " ++ fmt 1 (carnotLimit * 100) ++ " %")
     , txt ix (iy + 102) 14 (hsb 0 0 60) (if otto then "Higher compression -> higher eta, but knocking sets a limit"
                                                  else "Best eta, but the loop is thin: press L for log axes") ]
  where
    arr = wCyc w
    cyc = elems arr
    nn = nCyc w
    info = wInfo w
    otto = wCycle w == 1
    pos = wPos w
    i0 = floor pos `mod` nn
    fr = pos - fromIntegral (floor pos :: Int)
    a = arr ! i0
    b = arr ! ((i0 + 1) `mod` nn)
    now = St (lerpF (sv a) (sv b) fr) (lerpF (st a) (st b) fr) (lerpF (sp a) (sp b) fr) (sheat a)
    stroke4 = min 3 (i0 `div` nPer)
    nameOf k = case drop k (iNames info) of
      (s : _) -> s
      [] -> ""
    cx0 = 70
    cyl = envH env * 0.42
    cylLen = envW env * 0.3
    bore = 130
    pxp = cx0 + sv now * cylLen
    hot = clampF 0 1 ((st now - tLow) / (iTHigh info - tLow))
    gx = envW env * 0.5
    gw = envW env - gx - 60
    gy = 130
    gh = envH env - gy - 190
    pTop = foldl' max 0 (map sp cyc)
    pLow = foldl' min 1e9 (map sp cyc)
    vLow = foldl' min 1 (map sv cyc)
    lg = logBase 10
    xOfV v | wLog w = let lv0 = lg (vLow * 0.8); lv1 = lg 1.2 in gx + (lg v - lv0) / (lv1 - lv0) * gw
           | otherwise = gx + v / 1.08 * gw
    yOfP p | wLog w = let lp0 = lg (pLow * 0.7); lp1 = lg (pTop * 1.3) in gy + gh - (lg p - lp0) / (lp1 - lp0) * gh
           | otherwise = gy + gh - p / (pTop * 1.08) * gh
    xOf = xOfV . sv
    yOf = yOfP . sp
    strokeCol h = hsb (if h > 0 then 10 else if h < 0 then 210 else 0) (if h == 0 then 0 else 75) 100
    ctl | otto = "Left/Right: compression e = " ++ fmt 0 (wComp w) ++ "   Up/Down: max T " ++ fmt 0 (wTmax w) ++ " K"
        | otherwise = "Left/Right: isothermal ratio " ++ fmt 1 (wIso w) ++ "   Up/Down: Th = " ++ fmt 0 (wTh w) ++ " K"
    ix = 56
    iy = envH env - 150
    carnotLimit = 1 - tLow / iTHigh info

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char k) Down _ _
    | k == '1' -> buildCycle w { wCycle = 1 }
    | k == '2' -> buildCycle w { wCycle = 2 }
    | k `elem` "lL" -> w { wLog = not (wLog w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> buildCycle (side (-1) w)
  EventKey (SpecialKey KeyRight) Down _ _ -> buildCycle (side 1 w)
  EventKey (SpecialKey KeyUp) Down _ _ -> buildCycle (temp 1 w)
  EventKey (SpecialKey KeyDown) Down _ _ -> buildCycle (temp (-1) w)
  _ -> w
  where
    side d v | wCycle v == 1 = v { wComp = clampF 3 16 (wComp v + d) }
             | otherwise = v { wIso = clampF 1.5 5 (wIso v + 0.5 * d) }
    temp d v | wCycle v == 1 = v { wTmax = clampF 1200 3000 (wTmax v + 200 * d) }
             | otherwise = v { wTh = clampF 400 1200 (wTh v + 100 * d) }

-- ---------- やってみよう ----------
-- 1. オットーで圧縮比を 4 → 12 と上げ、効率を表にしよう。式 1 − 1/ε^0.4 と面積からの値は一致する？
-- 2. オットーの最高温度を変えても効率は変わらない（圧縮比だけで決まる）。変わるのは何？（1 周の仕事）
-- 3. カルノーで Th を 400 → 1200 K に。効率は上がるが、L を押さないと線図がつぶれて見えない。体積比はいくつ？
-- 4. オットーの効率は、同じ温度範囲のカルノー効率より必ず低い。差が生まれるのはどの行程？
-- 5. ディーゼルサイクル（加熱を等圧で行う）を 3 番目として作ってみよう。buildCycle の 2→3 を「p 一定で V が増える」に変える

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 左上が (x, y)、幅 w、高さ h の塗りつぶした長方形（p5.js の rect）
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = if w <= 1 then Color c (Line ps) else Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 閉じた折れ線
poly' :: [Point] -> Picture
poly' [] = Blank
poly' ps@(p : _) = Line (ps ++ [p])

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 凹んだ多角形も塗れるよう、y = 一定の線で細く切って塗る（gloss の Polygon は凸の形しか正しく塗れない）
fillPoly :: Color -> [Point] -> Picture
fillPoly _ [] = Blank
fillPoly c ps = Color c (Pictures [ Polygon [(x0, y), (x1, y), (x1, y + dy), (x0, y + dy)]
                                  | k <- [0 .. nStrip - 1]
                                  , let y = ylo + fromIntegral k * dy
                                        ym = y + dy / 2
                                  , (x0, x1) <- pairs (crossings ym) ])
  where
    ys = map snd ps
    ylo = minimum ys
    yhi = maximum ys
    nStrip = 120 :: Int
    dy = (yhi - ylo) / fromIntegral nStrip
    edges = zip ps (drop 1 ps ++ take 1 ps)
    crossings y = sortF [ x1 + (y - y1) / (y2 - y1) * (x2 - x1)
                        | ((x1, y1), (x2, y2)) <- edges, (y1 <= y && y < y2) || (y2 <= y && y < y1) ]
    pairs (a : b : rest) = (a, b) : pairs rest
    pairs _ = []
    sortF = foldr ins []
    ins v [] = [v]
    ins v (x : xs) = if v <= x then v : x : xs else x : ins v xs

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
