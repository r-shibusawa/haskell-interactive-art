-- ============================================
-- 45-06 応力–ひずみ線図（引張試験）
-- 試験片を引っ張って「応力 σ = 力 / 断面積」と「ひずみ ε = 伸び / 元の長さ」の関係を描く
-- 弾性域（戻る）→ 降伏 → 加工硬化 → 最大荷重 → くびれ → 破断。途中で戻すと永久ひずみが残る
-- 操作：ドラッグ（左右）= 引っ張る・戻す / 1〜3 = 材料（軟鋼・アルミ合金・ゴム）/ Z = 弾性域を拡大 / R = 新しい試験片
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

-- ★ 材料の表：E ヤング率（MPa）、sy 降伏応力、plateau 降伏棚の終わり、eu・su 最大応力のひずみと値、ef・sf 破断
data Material = Material
  { mTitle :: String, mHue :: !Float, mE :: !Float, mSy :: !Float, mPlateau :: !Float
  , mEu :: !Float, mSu :: !Float, mEf :: !Float, mSf :: !Float, mRubber :: !Bool }

steel :: Material
steel = Material "Mild steel (like SS400)" 205 206000 250 0.02 0.2 420 0.34 330 False

materials :: [Material]
materials =
  [ steel
  , Material "Aluminium alloy (like 6061-T6)" 45 69000 275 0 0.09 310 0.15 270 False
  , Material "Rubber (hyperelastic)" 330 3 0 0 5 0 5 0 True ]

maxTrace :: Int
maxTrace = 4000                -- ★ 覚えておく線の点の数

data World = World
  { wMat       :: !Int
  , wStrain    :: !Float
  , wStrainMax :: !Float
  , wBroken    :: !Bool
  , wTrace     :: ![(Float, Float)]  -- 描いてきた (ε, σ) の点（新しい順）
  , wLen       :: !Int               -- wTrace の長さ
  , wWork      :: !Float
  , wLastE     :: !Float
  , wLastS     :: !Float
  , wZoom      :: !Bool
  , wPmx       :: !Float             -- 前のマウスの x（p5.js の pmouseX）
  }

resetTest :: World -> World
resetTest w = w { wStrain = 0, wStrainMax = 0, wBroken = False, wTrace = [(0, 0)], wLen = 1, wWork = 0, wLastE = 0, wLastS = 0 }

world0 :: World
world0 = resetTest (World 0 0 0 False [] 0 0 0 0 False 0)

main :: IO ()
main = runSketch (sketch "45-06 Stress-strain curve" world0)
  { skBack  = hsb 212 45 13
  , skDraw  = draw
  , skEvent = event
  }

matOf :: World -> Material
matOf w = case drop (wMat w) materials of
  (m : _) -> m
  [] -> steel

-- ★ 一方向に引っ張り続けたときの曲線（包絡線）
envelope :: Material -> Float -> Float
envelope mt e
  | mRubber mt = let lam = 1 + e in (mE mt / 3) * (lam - 1 / (lam * lam))   -- ネオフック：σ = G(λ − 1/λ²)
  | e <= ey = mE mt * e                                                       -- 弾性域：フックの法則 σ = Eε
  | e <= mPlateau mt = mSy mt                                                 -- 降伏棚
  | e <= mEu mt = let u = (mEu mt - e) / (mEu mt - e0) in mSu mt - (mSu mt - mSy mt) * u * u   -- 加工硬化
  | otherwise = let u = (e - mEu mt) / (mEf mt - mEu mt) in mSu mt - (mSu mt - mSf mt) * u * u -- くびれ（公称応力は下がる）
  where
    ey = mSy mt / mE mt
    e0 = max ey (mPlateau mt)

-- ★ いまの応力：これまでの最大ひずみより手前なら、傾き E の直線で除荷・再負荷
stressAt :: World -> Material -> Float -> Float
stressAt w mt e
  | wBroken w = 0
  | mRubber mt || e >= wStrainMax w = envelope mt e
  | otherwise = max (envelope mt (wStrainMax w) - mE mt * (wStrainMax w - e)) 0

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 試験片（上）：ひずみに合わせて伸び、細くなる
  [ Color (hsb 0 0 45) (rect (sx - 25) sy0 50 68)                     -- 固定側のつかみ
  , Color (hsb 0 0 45) (rect (sx + len + gapB + 25) sy0 50 68)        -- 動く側のつかみ
  , Color (hsb (mHue mt) 45 85) (Pictures (specimen 0 ++ specimen 1))
  , if fl > 2 then Pictures [ seg 4 (hsb 0 80 100) (fx, sy0) (fx + fl, sy0)
                            , seg 4 (hsb 0 80 100) (fx + fl, sy0) (fx + fl - 10, sy0 - 7)
                            , seg 4 (hsb 0 80 100) (fx + fl, sy0) (fx + fl - 10, sy0 + 7) ] else Blank
  , txt (sx - 50) (sy0 - 48) 14 (hsb 0 0 100) (mTitle mt)
  , txt (sx - 50) (sy0 + 62) 14 (hsb 0 0 60) (if wBroken w then "Broken! R for a new specimen" else "<- drag to pull / release ->")
  -- ---- 線図（下）
  , seg 1 (hsb 0 0 45) (gx, gy) (gx, gy + gh), seg 1 (hsb 0 0 45) (gx, gy + gh) (gx + gw, gy + gh) ]
  ++ concat [ [ txt (xE (eMax * fi / 5) - 10) (gy + gh + 18) 14 (hsb 0 0 60) (fmt (if eMax < 0.05 then 2 else 0) (eMax * fi / 5 * 100) ++ "%")
              , txt (gx - 50) (yS (sMax * fi / 5) + 4) 14 (hsb 0 0 60) (fmt (if mRubber mt then 1 else 0) (sMax * fi / 5)) ]
            | i <- [0 .. 5 :: Int], let fi = fromIntegral i ]
  ++ [ txt (gx + gw - 70) (gy + gh + 40) 14 (hsb 0 0 60) "strain e"
     , txt (gx - 46) (gy - 12) 14 (hsb 0 0 60) "stress s (MPa)"
     -- 包絡線（うすく）
     , path 1.5 (hsbA (mHue mt) 30 70 40) [ (xE e, yS (envelope mt e)) | i <- [0 .. 300 :: Int], let e = min eMax (mEf mt) * fromIntegral i / 300 ]
     -- たどってきた線
     , path 3 (hsb (mHue mt) 60 100) (thin [ (xE e, yS s) | (e, s) <- reverse (wTrace w), e <= eMax ]) ]
  ++ (if mRubber mt then [] else
       [ seg 1 (hsbA 0 0 100 35) (xE 0, yS (mSy mt)) (xE (min eMax (mEf mt)), yS (mSy mt))
       , txt (xE (min eMax (mEf mt)) - 130) (yS (mSy mt) - 6) 14 (hsb 0 0 75) ("yield sy = " ++ fmt 0 (mSy mt)) ]
       ++ [ txt (xE (mEu mt) - 60) (yS (mSu mt) - 10) 14 (hsb 0 0 75) ("tensile strength su = " ++ fmt 0 (mSu mt)) | not (wZoom w) ])
  ++ [ dot (hsb 0 0 100) (xE (wStrain w)) (yS sigma) 12 | wStrain w <= eMax ]
  -- ---- 数値
  ++ [ txt 20 40 18 (hsb 0 0 100) ("Drag: pull   1-3: material   Z: zoom elastic part " ++ (if wZoom w then "ON" else "off") ++ "   R: new specimen")
     , txt 20 68 15 (hsb 0 0 60) "Below yield it springs back (elastic). Past yield, unloading follows a slope-E line and leaves permanent strain."
     , Color (hsbA 0 0 100 8) (rect (ix - 16 + 195) (iy - 30 + 125) 390 250)
     , txt ix iy 16 (hsb 0 0 100) "Hooke's law: s = E e"
     , txt ix (iy + 28) 16 (hsb 0 0 100) ("Young's modulus E = " ++ (if mRubber mt then fmt 0 (mE mt) ++ " MPa" else fmt 0 (mE mt / 1000) ++ " GPa"))
     , txt ix (iy + 62) 16 (hsb 35 90 100) ("strain e = " ++ fmt 2 (wStrain w * 100) ++ " %")
     , txt ix (iy + 88) 16 (hsb 35 90 100) ("stress s = " ++ fmt (if mRubber mt then 2 else 0) sigma ++ " MPa")
     , txt ix (iy + 122) 16 (hsb 190 70 100) ("permanent strain = " ++ fmt 2 (plastic * 100) ++ " %")
     , txt ix (iy + 148) 16 (hsb 190 70 100) ("energy absorbed = " ++ fmt 1 (wWork w) ++ " MJ/m^3")
     , txt ix (iy + 180) 14 (hsb 0 0 60) ("stage: " ++ stage)
     , txt ix (iy + 202) 14 (hsb 0 0 60) "energy = area under curve, int s de (toughness)" ]
  where
    mt = matOf w
    sigma = stressAt w mt (wStrain w)
    sPeak = if mRubber mt then envelope mt (mEf mt) else mSu mt
    ey = if mRubber mt then 0 else mSy mt / mE mt
    plastic = if mRubber mt || wBroken w then 0 else max (wStrainMax w - envelope mt (wStrainMax w) / mE mt) 0   -- 永久ひずみ
    -- 試験片
    sx = 120
    sy0 = 150
    len0 = min (envW env * 0.32) 360
    stretch = if mRubber mt then wStrain w * 0.35 else wStrain w * 1.2   -- 絵の上での伸び（見やすく誇張）
    len = len0 * (1 + stretch)
    necking = if not (mRubber mt) && wStrainMax w > mEu mt then clampF 0 1 ((wStrainMax w - mEu mt) / (mEf mt - mEu mt)) else 0
    wide = 34 / sqrt (1 + (if mRubber mt then wStrain w * 0.6 else wStrain w))
    gapB = if wBroken w then 14 else 0
    -- 半分ずつ、細い台形に分けて塗る（くびれた形は凹んでいるので）
    specimen :: Int -> [Picture]
    specimen half =
      let xAt u = if half == 0 then sx + (len / 2) * u else sx + len + gapB - (len / 2) * u
          hw u = (wide / 2) * (1 - 0.6 * necking * exp (negate (((1 - u) * 5) ** 2)))
          us = [fromIntegral i / 20 | i <- [0 .. 20 :: Int]]
      in zipWith (\u1 u2 -> Polygon [(xAt u1, sy0 - hw u1), (xAt u2, sy0 - hw u2), (xAt u2, sy0 + hw u2), (xAt u1, sy0 + hw u1)]) us (drop 1 us)
    -- 力の矢印
    fx = sx + len + gapB + 50
    fl = sigma / sPeak * 90
    -- 線図
    gx = 90
    gy = 250
    gw = envW env - 520
    gh = envH env - gy - 70
    eMax = if wZoom w && not (mRubber mt) then 0.01 else mEf mt * 1.08
    sMax = sPeak * 1.15
    xE e = gx + e / eMax * gw
    yS s = gy + gh - s / sMax * gh
    ix = envW env - 400
    iy = 270
    stage | wBroken w = "broken"
          | mRubber mt = "hyperelastic (springs back)"
          | wStrain w < wStrainMax w - 1e-6 = "unload / reload (elastic)"
          | wStrain w <= ey = "elastic"
          | wStrain w <= mPlateau mt = "yield plateau"
          | wStrain w <= mEu mt = "strain hardening"
          | otherwise = "necking"

-- 点が多すぎるときは間引いて描く（最後の点は残す）
thin :: [Point] -> [Point]
thin ps
  | n <= 1500 = ps
  | otherwise = [p | (i, p) <- zip [0 :: Int ..] ps, i `mod` k == 0 || i == n - 1]
  where n = length ps
        k = n `div` 1500 + 1

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w { wPmx = fst (mouseP env) }
  EventMotion _ ->
    let mx = fst (mouseP env)
        w' = if envDown env then pull env (mx - wPmx w) w else w
    in w' { wPmx = mx }
  EventKey (Char k) Down _ _
    | k `elem` "123" -> resetTest w { wMat = fromEnum k - fromEnum '1' }
    | k `elem` "zZ" -> w { wZoom = not (wZoom w) }
    | k `elem` "rR" -> resetTest w
  _ -> w

-- ドラッグで引っ張る・戻す
pull :: Env -> Float -> World -> World
pull env dmx w
  | wBroken w = w
  | otherwise = trim (go 1 w { wStrain = target })
  where
    mt = matOf w
    range = mEf mt * 1.05
    -- 戻せるのは力が 0 になるところ（永久ひずみ）まで。それ以上戻すと試験片がたるむだけ
    slack = if mRubber mt then 0 else max (wStrainMax w - envelope mt (wStrainMax w) / mE mt) 0
    target = clampF slack range (wStrain w + dmx / (envW env * 0.5) * range * (if wZoom w && not (mRubber mt) then 0.03 else 1))
    lastE = case wTrace w of
      ((e, _) : _) -> e
      [] -> 0
    -- 細かく分けて進める（速くドラッグしても線が飛ばないように）
    n = max 1 (ceiling (abs (target - lastE) / (range / 400))) :: Int
    go i v
      | i > n = v
      | otherwise =
          let e = lastE + (target - lastE) * fromIntegral i / fromIntegral n
              s = stressAt v mt e
              v1 = v { wStrainMax = max e (wStrainMax v)
                     , wWork = wWork v + (s + wLastS v) / 2 * (e - wLastE v)   -- 台形公式で ∫σ dε
                     , wLastE = e, wLastS = s
                     , wTrace = (e, s) : wTrace v, wLen = wLen v + 1 }
          in if e >= mEf mt
               then v1 { wBroken = True, wTrace = (e, 0) : wTrace v1, wLen = wLen v1 + 1, wStrain = e }
               else go (i + 1) v1
    trim v = if wLen v > maxTrace then v { wTrace = take maxTrace (wTrace v), wLen = maxTrace } else v

-- ---------- やってみよう ----------
-- 1. 軟鋼を 5% まで引っ張ってから戻そう。戻りの線は最初の直線と平行。横軸と交わる点が永久ひずみ
-- 2. もう一度引っ張ると、前より高い応力まで弾性のまま（加工硬化）。針金を曲げ伸ばしすると硬くなる理由
-- 3. Z で拡大して弾性域を見よう。軟鋼は 0.12% で降伏する。1 m の棒なら何 mm 伸びたところ？
-- 4. 軟鋼とアルミを破断まで引っ張り、吸収したエネルギー（面積）を比べよう。ねばり強いのは？
-- 5. materials に「鋳鉄（E 100000、ほとんど伸びずに破断：ef 0.005）」を足そう。もろい材料の線図はどんな形？

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

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = if w <= 1 then Color c (Line ps) else Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
