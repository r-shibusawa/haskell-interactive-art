-- ============================================
-- 54-00 クーロンの法則 ─ 電荷どうしが押し合い・引き合う力
-- 2 つの点電荷の力の大きさ F = k q₁ q₂ / r²（k = 8.99×10⁹ N·m²/C²）。距離が 2 倍で力は 1/4
-- 同じ符号は反発、ちがう符号は引き合う。たくさんあるときは 1 組ずつの力を足す（重ね合わせ）
-- 操作：ドラッグ = 電荷を動かす / A・N = ＋・− を追加 / F = 符号 / ↑↓ = 大きさ / スペース = 自由に動かす / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Numeric (showFFloat)
import System.Environment (lookupEnv)

kc :: Double
kc = 8.988e9                           -- クーロン定数 k [N·m²/C²]

pxM :: Double
pxM = 0.001                            -- ★ 1 px = 1 mm

mass :: Double
mass = 0.005                           -- ★ 電荷をのせた玉の質量 [kg]（5 g）

slow :: Double
slow = 0.05                            -- ★ スローモーション（1 秒で 0.05 秒ぶん進む）

dragK :: Double
dragK = 1.5                            -- ★ 空気の抵抗（速さに比例してブレーキ）[1/s]

contact :: Double
contact = 0.016                        -- 玉の直径 [m]（これより近づかない）

-- 電荷 1 つ：位置 [px]（p5.js と同じ、左上が原点）、電荷 [μC]、速さ [m/s]
data Charge = Charge { cX, cY, cQ, cVx, cVy :: !Double }

data World = World
  { wCs      :: [Charge]
  , wGrab    :: !Int                   -- つかんでいる電荷（-1 = なし）
  , wRun     :: !Bool
  , wPairA   :: !Int
  , wPairB   :: !Int
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "54-00 Coulomb's law" (resetCharges 1280 800 world0))
  { skBack  = rgb255 14 18 26
  , skStart = \env -> do
      printSelfTest selfTest
      pure (resetCharges (envW env) (envH env) world0) { wTest = selfTest }
  , skStep  = \env dt w -> pure (step env dt w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [] (-1) False 0 1 []

resetCharges :: Float -> Float -> World -> World
resetCharges wd ht w = w { wCs = [Charge (cx - 150) cy 1 0 0, Charge (cx + 150) cy (-1) 0 0, Charge cx (cy - 200) 0.5 0 0]
                         , wPairA = 0, wPairB = 1, wRun = False }
  where cx = realToFrac wd * 0.36
        cy = realToFrac ht * 0.55

-- 動かしているときは 1 フレームを 20 回に分けて進める
step :: Env -> Float -> World -> World
step env dt w
  | wRun w = w { wCs = iterate (moveStep (dims env) (wGrab w) h True) (wCs w) !!! 20 }
  | otherwise = w
  where h = realToFrac (min dt 0.05) * slow / 20

dims :: Env -> (Double, Double)
dims env = (realToFrac (envW env), realToFrac (envH env))

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  -- 1 組ずつの力（細い矢印）と、合計の力（太い矢印）。長さは力の対数
  concat [ [ Color (rgba255 150 160 190 120) (arrowTo 1 6 (p a) (p a `plus` (ux * len m, uy * len m)))
           | (j, b) <- zip [0 :: Int ..] cs, i /= j
           , let (fx, fy) = pairForce a b
                 m = sqrt (fx * fx + fy * fy)
           , m > 0, let ux = f2 (fx / m); uy = f2 (fy / m) ]
           ++ [ Color (rgb255 255 200 90) (arrowTo 3 11 (p a) (p a `plus` (f2 (fx / m) * len m, f2 (fy / m) * len m))) | m > 0 ]
         | (i, a, (fx, fy)) <- zip3 [0 :: Int ..] cs bigF, let m = sqrt (fx * fx + fy * fy) ]
  ++ [ Color (rgba255 120 220 255 140) (dashed 5 6 [p a, p b]) | Just a <- [mA], Just b <- [mB] ]
  ++ concat [ [ Color (if cQ c > 0 then rgb255 240 90 70 else rgb255 70 150 250) (circleD (f2 (cX c)) (f2 (cY c)) (16 + 8 * f2 (sqrt (abs (cQ c)))))
              , sayC (if cQ c > 0 then "+" else "-") (f2 (cX c)) (f2 (cY c) + 6) 16 white ]
            | c <- cs ]
  ++ [ drawGraph g mA mB
     , say ("Drag: move  A/N add +/-  F flip sign  Up/Down size  Space run (" ++ (if wRun w then "running" else "stopped") ++ ")  R reset") 20 34 17 (gray 0.92)
     , say "Thick yellow = total force, gray = force from each pair (arrow length ~ log of the force)." 20 58 14 (gray 0.67)
     , say ("Charges in uC, 1 px = 1 mm, ball mass " ++ showJ (mass * 1000) ++ " g, time runs " ++ showJ slow ++ "x (slow motion)") 20 78 14 (gray 0.67) ]
  ++ [ say ("Charge " ++ show (i + 1) ++ ": q = " ++ (if cQ c > 0 then "+" else "") ++ fmt 1 (cQ c) ++ " uC   total |F| = " ++ sci 3 (sqrt (fx * fx + fy * fy)) ++ " N")
           20 (108 + 22 * fromIntegral i) 15 (if cQ c > 0 then rgb255 255 140 120 else rgb255 130 180 255)
     | (i, c, (fx, fy)) <- zip3 [0 :: Int ..] cs bigF ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    cs = wCs w
    bigF = netForces cs
    p c = (f2 (cX c), f2 (cY c))
    plus (a, b) (c, d) = (a + c, b + d)
    len m = 26 * f2 (logBase 10 (1 + m / 0.002))
    mA = getAtM cs (wPairA w)
    mB = getAtM cs (wPairB w)
    g = (envW env * 0.7, 140, envW env * 0.26, envH env * 0.42)

-- ---- 右：選んだ 2 つの F と r のグラフ（両対数）
drawGraph :: (Float, Float, Float, Float) -> Maybe Charge -> Maybe Charge -> Picture
drawGraph (gx, gy, gw, gh) mA mB = Pictures $
  [ Color (rgb255 50 58 74) (Line [(xr r, gy), (xr r, gy + gh)]) | r <- rTicks ]
  ++ [ Color (rgb255 50 58 74) (Line [(gx, yf f), (gx + gw, yf f)]) | f <- fTicks ]
  ++ [ Color (rgb255 70 80 100) (frameTL 1 gx gy gw gh) ]
  ++ [ sayC (showJ (fromIntegral (round (r * 100) :: Int)) ++ " cm") (xr r) (gy + gh + 18) 14 (gray 0.55) | r <- rTicks ]
  ++ [ sayR (sci 1 f ++ " N") (gx - 5) (yf f + 5) 14 (gray 0.55) | f <- fTicks ]
  ++ [ say "Dashed pair: force F vs distance r (log-log)" (gx - 40) (gy - 30) 14 (gray 0.86) ]
  ++ case (mA, mB) of
       (Just a, Just b) ->
         let k2 = kc * abs (cQ a * cQ b) * 1e-12
             pts = [ (xr r, yf (k2 / (r * r))) | i <- [0 .. 80 :: Int], let r = r0 * (r1 / r0) ** (fromIntegral i / 80) ]
             r = max contact (sqrt ((cX a - cX b) ^ (2 :: Int) + (cY a - cY b) ^ (2 :: Int)) * pxM)
             f = k2 / (r * r)
         in [ Color (rgb255 120 220 255) (polyline 2 pts)
            , Color (rgb255 255 200 90) (circleD (xr r) (yf f) 12) ]
            ++ (if 2 * r < r1
                  then [ Color (rgba255 255 200 90 120) (circleD (xr (2 * r)) (yf (f / 4)) 9)
                       , say "2x distance -> 1/4" (xr (2 * r) + 8) (yf (f / 4) - 6) 14 (gray 0.78) ]
                  else [])
            ++ [ say ("r = " ++ fmt 1 (r * 100) ++ " cm   F = " ++ sci 3 f ++ " N (" ++ (if cQ a * cQ b > 0 then "repel" else "attract") ++ ")") (gx - 40) (gy + gh + 46) 15 (gray 0.92)
               , say "Slope -2 line = inverse square law." (gx - 40) (gy + gh + 68) 14 (gray 0.67)
               , say "Doubling q shifts the line up." (gx - 40) (gy + gh + 88) 14 (gray 0.67) ]
       _ -> []
  where
    r0 = 0.02; r1 = 1.0; f0 = 1e-3; f1 = 100
    rTicks = [0.02, 0.05, 0.1, 0.2, 0.5, 1]
    fTicks = [1e-3, 1e-2, 0.1, 1, 10, 100]
    xr v = gx + f2 (clampv 0 1 (log (v / r0) / log (r1 / r0))) * gw
    yf v = gy + gh - f2 (clampv 0 1 (log (v / f0) / log (f1 / f0))) * gh

-- ---- マウスとキー
event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let cands = [ (d, i) | (i, c) <- zip [0 ..] (wCs w), let d = hyp (cX c - mx) (cY c - my), d < 26 ]
        g = pickMin (-1) cands
        w1 = w { wGrab = g }
    in pure (if g >= 0 && g /= wPairB w then w1 { wPairB = wPairA w, wPairA = g } else w1)
  EventMotion _ | envDown env && wGrab w >= 0 ->
    pure w { wCs = updateAt (wGrab w) (\c -> c { cX = clampv 10 (wd - 10) mx, cY = clampv 10 (ht - 10) my, cVx = 0, cVy = 0 }) (wCs w) }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wGrab = -1 }
  EventKey k Down _ _ -> pure (key k w)
  _ -> pure w
  where
    (mx0, my0) = mouseP5 env
    mx = realToFrac mx0
    my = realToFrac my0
    (wd, ht) = dims env
    key k w0 =
      let w1 = case k of
            SpecialKey KeySpace -> w0 { wRun = not (wRun w0) }
            Char c | c `elem` "rR" -> resetCharges (envW env) (envH env) w0
                   | c `elem` "aAnN" && length (wCs w0) < 8 ->
                       let x = clampv 20 (wd - 20) mx
                           y = clampv 20 (ht - 20) my
                       in if all (\o -> hyp (cX o - x) (cY o - y) > 24) (wCs w0)
                            then w0 { wCs = wCs w0 ++ [Charge x y (if c `elem` "aA" then 1 else -1) 0 0] } else w0
            _ -> w0
          -- いちばん近い電荷を変える
          i = pickMin (-1) [ (hyp (cX c - mx) (cY c - my), j) | (j, c) <- zip [0 ..] (wCs w1) ]
          upd f = w1 { wCs = updateAt i f (wCs w1) }
      in if i < 0 then w1 else case k of
           Char c | c `elem` "fF" -> upd (\o -> o { cQ = cQ o * (-1) })
           SpecialKey KeyUp -> upd (\o -> o { cQ = signum (cQ o) * min 5 (abs (cQ o) + 0.5) })
           SpecialKey KeyDown -> upd (\o -> o { cQ = signum (cQ o) * max 0.5 (abs (cQ o) - 0.5) })
           _ -> w1

-- いちばん小さい d の番号（同じなら先のもの）
pickMin :: Int -> [(Double, Int)] -> Int
pickMin d xs = snd (foldl' (\(bd, bi) (e, i) -> if e < bd then (e, i) else (bd, bi)) (1e9, d) xs)

-- ---- 自己テスト：手で計算できる値と比べる（p5.js 版と同じ 7 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("distance x2 -> force 1/4", nearRel (fst (pairForceRaw b a) / fst (pairForceRaw b2 a)) 4 1e-12)
  , ("1 uC and 1 uC at 1 m -> F = 8.99e-3 N", nearRel (uncurry hyp (pairForceRaw (ch 1000 0 1) (ch 0 0 1))) 8.988e-3 1e-9)
  , ("same signs repel (push apart), opposite signs attract", fst (pairForceRaw b a) > 0 && fst (pairForceRaw (ch 100 0 (-3)) a) < 0)
  , ("action-reaction: all forces add up to 0", hyp sx sy < 1e-12 * big)
  , ("superposition: total force = sum of each pair's force"
    , nearRel (fst fn0) (fst p1 + fst p2 + fst p3) 1e-12 && nearRel (snd fn0) (snd p1 + snd p2 + snd p3) 1e-12)
  , ("no walls and no drag: total momentum stays 0"
    , case ms of
        [m0, m1] -> abs (cVx m0 + cVx m1) < 1e-12 && abs (cVy m0 + cVy m1) < 1e-12 && abs (cVx m0) > 1e-4
        _ -> False)
  , ("collisions do not break the numbers (no NaN, speed <= 2 m/s)"
    , all (\c -> not (isBad (cX c + cY c + cVx c + cVy c)) && hyp (cVx c) (cVy c) <= 2 + 1e-9) hot) ]
  where
    ch x y q = Charge x y q 0 0
    a = ch 0 0 2
    b = ch 100 0 3
    b2 = ch 200 0 3
    cs = [ch 0 0 1, ch 120 40 (-2), ch (-60) 200 0.5, ch 300 (-90) 3]
    fn = netForces cs
    sx = sum (map fst fn)
    sy = sum (map snd fn)
    big = maximum (0 : map (uncurry hyp) fn)
    fn0 = getAt (0, 0) fn 0
    (p1, p2, p3) = case cs of
      (c0 : c1 : c2 : c3 : _) -> (pairForceRaw c0 c1, pairForceRaw c0 c2, pairForceRaw c0 c3)
      _ -> ((0, 0), (0, 0), (0, 0))
    ms = iterate (moveStep (1280, 800) (-1) 1e-4 False) [ch 400 300 1, ch 700 360 2] !!! 400
    hot = iterate (moveStep (1280, 800) (-1) 2e-4 True) [ch 400 300 5, ch 430 300 (-5)] !!! 3000
    isBad v = isNaN v || isInfinite v

-- ---------- やってみよう ----------
-- 1. 2 つの電荷を 10 cm と 20 cm に置いて、F が 1/4 になることをグラフで確かめよう
-- 2. ↑ で片方を 2 μC にすると、2 つの電荷が受ける力はそれぞれ何倍？（作用・反作用）
-- 3. スペースで動かして、＋ 1 つと − 2 つ、＋ 3 つなどの動き方を比べよう
-- 4. 電荷を一直線に 3 つ並べて、まん中の電荷の合計の力が 0 になる置き方を探そう
-- 5. kc を 2 倍にした世界では、同じ距離で力は何倍？ mass や dragK も変えてみよう


-- ==================================================
-- ここから下は「電磁気の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- b が a から受けるクーロン力 [N]。pairForce は接触より近くならない
pairForceRaw :: Charge -> Charge -> (Double, Double)
pairForceRaw b a
  | r2 == 0 = (0, 0)
  | otherwise = (f * dx, f * dy)
  where
    dx = (cX b - cX a) * pxM
    dy = (cY b - cY a) * pxM
    r2 = dx * dx + dy * dy
    f = (kc * cQ a * cQ b * 1e-12) / (r2 * sqrt r2)

pairForce :: Charge -> Charge -> (Double, Double)
pairForce b a
  | r == 0 = (0, 0)
  | otherwise = ((f * dx) / r, (f * dy) / r)
  where
    dx = (cX b - cX a) * pxM
    dy = (cY b - cY a) * pxM
    r = hyp dx dy
    rc = max r contact
    f = (kc * cQ a * cQ b * 1e-12) / (rc * rc)

-- 全部の電荷が受ける力（1 組ずつ足す。作用・反作用）
netForces :: [Charge] -> [(Double, Double)]
netForces cs = toList (foldl' add (Seq.replicate n (0, 0)) [ (i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ])
  where
    arr = Seq.fromList cs
    n = Seq.length arr
    add acc (i, j) =
      let (fx, fy) = pairForce (Seq.index arr j) (Seq.index arr i)
      in Seq.adjust' (\(x, y) -> (x - fx, y - fy)) i (Seq.adjust' (\(x, y) -> (x + fx, y + fy)) j acc)

-- ---- 運動を dt 秒ぶん進める（半陰的オイラー法）。walls = True なら 壁・抵抗・衝突・速さの上限あり
--      (wd, ht) = 画面の大きさ、grab = つかんでいる電荷（動かさない）
moveStep :: (Double, Double) -> Int -> Double -> Bool -> [Charge] -> [Charge]
moveStep (wd, ht) grab dt walls cs0
  | not walls = moved
  | otherwise = map wall (toList (foldl' collide (Seq.fromList moved) [ (i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ]))
  where
    n = length cs0
    bigF = netForces cs0
    moved = zipWith3 one [0 ..] cs0 bigF
    one i c (fx, fy)
      | i == grab = c
      | otherwise =
          let vx1 = cVx c + (fx / mass) * dt
              vy1 = cVy c + (fy / mass) * dt
              (vx2, vy2) = if walls then (vx1 * exp (-dragK * dt), vy1 * exp (-dragK * dt)) else (vx1, vy1)
              v = hyp vx2 vy2
              (vx, vy) = if walls && v > 2 then (vx2 * 2 / v, vy2 * 2 / v) else (vx2, vy2)
          in c { cVx = vx, cVy = vy, cX = cX c + (vx * dt) / pxM, cY = cY c + (vy * dt) / pxM }
    -- 玉どうしは重ならない（やわらかくはね返る）
    collide sq (i, j) =
      let a = Seq.index sq i
          b = Seq.index sq j
          dx0 = cX b - cX a
          dy0 = cY b - cY a
          d0 = hyp dx0 dy0
          dmin = contact / pxM
          (dx, dy, d) = if d0 < 1e-6 then (1, 0, 1) else (dx0, dy0, d0)
          ux = dx / d
          uy = dy / d
          push = (dmin - d) / 2
          a1 = a { cX = cX a - ux * push, cY = cY a - uy * push }
          b1 = b { cX = cX b + ux * push, cY = cY b + uy * push }
          rel = (cVx b1 - cVx a1) * ux + (cVy b1 - cVy a1) * uy
          (a2, b2) = if rel < 0
                       then ( a1 { cVx = cVx a1 + 0.75 * rel * ux, cVy = cVy a1 + 0.75 * rel * uy }
                            , b1 { cVx = cVx b1 - 0.75 * rel * ux, cVy = cVy b1 - 0.75 * rel * uy } )
                       else (a1, b1)
      in if d0 < dmin then Seq.update j b2 (Seq.update i a2 sq) else sq
    wall c0 =
      let c1 | cX c0 < 10 = c0 { cX = 10, cVx = abs (cVx c0) * 0.5 }
             | otherwise = c0
          c2 | cX c1 > wd - 10 = c1 { cX = wd - 10, cVx = -(abs (cVx c1)) * 0.5 }
             | otherwise = c1
          c3 | cY c2 < 10 = c2 { cY = 10, cVy = abs (cVy c2) * 0.5 }
             | otherwise = c2
          c4 | cY c3 > ht - 10 = c3 { cY = ht - 10, cVy = -(abs (cVy c3)) * 0.5 }
             | otherwise = c3
      in c4

-- ---- 数の道具
hyp :: Double -> Double -> Double
hyp a b = sqrt (a * a + b * b)

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 大きい数・小さい数を読みやすく：0.000032 → "3.20e-5"（有効数字 sig 桁）
sci :: Int -> Double -> String
sci sig v
  | isNaN v || isInfinite v = "-"
  | v == 0 = "0"
  | e >= -2 && e <= 3 = fmt (max 0 (sig - 1 - e)) v
  | otherwise = fmt (sig - 1) (v / 10 ^^ e) ++ "e" ++ show e
  where
    e0 = floor (logBase 10 (abs v)) :: Int
    e | 10 ^^ (e0 + 1) <= abs v = e0 + 1
      | 10 ^^ e0 > abs v = e0 - 1
      | otherwise = e0

-- JavaScript の数の文字列（整数なら小数点なし）
showJ :: Double -> String
showJ v | isNaN v || isInfinite v = "0"
        | v == fromIntegral (round v :: Integer) && abs v < 1e15 = show (round v :: Integer)
        | otherwise = showFFloat Nothing v ""

-- リストの i 番目（なければ d / Nothing）
getAt :: a -> [a] -> Int -> a
getAt d xs i = maybe d id (getAtM xs i)

getAtM :: [a] -> Int -> Maybe a
getAtM xs i = case drop i xs of
  (x : _) | i >= 0 -> Just x
  _ -> Nothing

-- i 番目だけ f で変える
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs = [ if j == i then f x else x | (j, x) <- zip [0 ..] xs ]

-- n 回くり返した結果（iterate と組み合わせる。途中を正格に評価する）
(!!!) :: [a] -> Int -> a
xs !!! n = go xs n
  where go (y : ys) k | k <= 0 = y
                      | otherwise = y `seq` go ys (k - 1)
        go [] _ = error "empty"
infixl 9 !!!

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点）で
mouseP5 :: Env -> (Float, Float)
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 文字のおよその幅
textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中央揃え・右揃えの文字（y は文字の下）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 線分（太さ w。1 以下なら細い線）
seg :: Float -> Point -> Point -> Picture
seg w a b = if w <= 1 then Line [a, b] else thickLine w a b

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ w）
polyline :: Float -> [Point] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1 then Line pts else thickPath w pts
  _ -> Blank

-- (x1, y1) → (x2, y2) の矢印。太さ w、hs = 矢じりの大きさ
arrowTo :: Float -> Float -> Point -> Point -> Picture
arrowTo w hs (x1, y1) (x2, y2)
  | not (l > 0.5) = Blank
  | otherwise = Pictures [ seg w (x1, y1) (x2, y2)
                         , seg w (x2, y2) (x2 - h * ux + h * 0.5 * uy, y2 - h * uy - h * 0.5 * ux)
                         , seg w (x2, y2) (x2 - h * ux - h * 0.5 * uy, y2 - h * uy + h * 0.5 * ux) ]
  where
    dx = x2 - x1
    dy = y2 - y1
    l = sqrt (dx * dx + dy * dy)
    ux = dx / l
    uy = dy / l
    h = min hs (l * 0.5)

-- 点線（線 on、すきま off ピクセル）
dashed :: Float -> Float -> [Point] -> Picture
dashed on off ps = Pictures (go True on ps)
  where
    go _ _ [] = []
    go _ _ [_] = []
    go dr left (a@(x1, y1) : rest@(b@(x2, y2) : _))
      | l < 1e-6 = go dr left rest
      | l <= left = [ Line [a, b] | dr ] ++ go dr (left - l) rest
      | otherwise = let m = (x1 + (x2 - x1) * left / l, y1 + (y2 - y1) * left / l)
                    in [ Line [a, m] | dr ] ++ go (not dr) (if dr then off else on) (m : rest)
      where l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

-- 左上が (x, y) の長方形の枠（太さ wt。1 以下なら細い線）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h
  | wt <= 1 = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
  | otherwise = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 円（直径 d、p5.js の circle と同じ）
circleD :: Float -> Float -> Float -> Picture
circleD x y d = at (x, y) (disc (d / 2))

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 相対誤差 rel 以内
nearRel :: Double -> Double -> Double -> Bool
nearRel a b rel = near a b (abs b * rel)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if not (null rs) && all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 画面の左下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 12) 14 (if not (null rs) && all snd rs then rgb255 120 220 160 else rgb255 255 100 90)
