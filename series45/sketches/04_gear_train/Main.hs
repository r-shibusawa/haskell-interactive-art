-- ============================================
-- 45-04 歯車列と減速比
-- 歯車をつなぐと、回転数は歯数の逆比で変わり、トルクはその分だけ大きくなる（速さ ⇄ 力の交換）
-- 間にはさんだだけの歯車（遊び歯車）は向きを変えるだけ。同じ軸に大小 2 枚（複合歯車）で大きく減速できる
-- 操作：クリック = その向きに歯車を足す（遠いほど歯数が多い）/ C = 最後の歯車と同じ軸に小歯車 / Z = 1 枚戻す
--       ↑↓ = 入力の回転数 / R = リセット
-- ============================================
module Main (main) where

import Art
import qualified Data.Map.Strict as M
import Numeric (showFFloat)

gearM :: Float
gearM = 7               -- ★ モジュール（歯の大きさ、px）

pinionZ :: Int
pinionZ = 12            -- ★ C で足す小歯車の歯数

efficiency :: Float
efficiency = 0.97       -- ★ かみ合い 1 回あたりの伝達効率

inputTorque :: Float
inputTorque = 1.0       -- ★ 入力トルク（N·m）

maxGears :: Int
maxGears = 14

pa :: Float
pa = 20 * pi / 180      -- 圧力角 20°

-- 歯車 1 枚：中心、歯数、つながる親、同軸か、かみ合いの向き
data Gear = Gear
  { gX, gY   :: !Float
  , gZ       :: !Int
  , gParent  :: !Int
  , gCoax    :: !Bool
  , gDirAng  :: !Float
  }

data World = World
  { wGears :: ![Gear]
  , wRpm   :: !Float          -- ★ 入力（最初の歯車）の回転数
  , wRot0  :: !Float          -- 入力歯車の回転角
  }

main :: IO ()
main = runSketch (sketch "45-04 Gear train" (World [] 120 0))
  { skBack  = hsb 212 45 13
  , skStart = \env -> pure (resetTrain env (World [] 120 0))
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

resetTrain :: Env -> World -> World
resetTrain env w =
  let x0 = envW env * 0.16
      y0 = envH env * 0.55
      g0 = Gear x0 y0 14 (-1) False 0
  in addMesh (x0 + 180, y0) w { wGears = [g0] }

-- 輪郭は歯数ごとに作り置き（04 で使う歯数は 8〜60）
outlines :: M.Map Int Outline
outlines = M.fromList [(z, makeGearOutline z gearM pa 1 1.25) | z <- [8 .. 60]]

outlineOf :: Int -> Outline
outlineOf z = M.findWithDefault (makeGearOutline z gearM pa 1 1.25) z outlines

-- ★ 中心距離 a = m(z1 + z2)/2 から歯数を決める
zFor :: Gear -> Float -> Int
zFor p d = max 8 (min 60 (round (2 * d / gearM - fromIntegral (gZ p))))

-- (mx, my) の向きに、最後の歯車とかみ合う歯車を足す
addMesh :: Point -> World -> World
addMesh (mx, my) w = case lastOf (wGears w) of
  Just p | length (wGears w) < maxGears && d >= 1 ->
    let z = zFor p d
        a = gearM * fromIntegral (gZ p + z) / 2
        ang = atan2 (my - gY p) (mx - gX p)
        n = length (wGears w)
    in w { wGears = wGears w ++ [Gear (gX p + a * cos ang) (gY p + a * sin ang) z (n - 1) False ang] }
    where d = dist2 (mx, my) (gX p, gY p)
  _ -> w

-- ★ 回転数と回転角を親から順に決める → [(回転角, 回転数)]
updateTrain :: World -> [(Float, Float)]
updateTrain w = reverse (foldl add [] (zip [0 :: Int ..] (wGears w)))
  where
    zs = map gZ (wGears w)
    add acc (0, _) = [(wRot0 w, wRpm w)] ++ acc
    add acc (_, g) = case acc of
      [] -> [(0, 0)]
      ((prot, prpm) : _) ->                      -- 親はいつも 1 つ前の歯車
        let pz = fromIntegral (zAt (gParent g))
            gz = fromIntegral (gZ g)
        in if gCoax g then (prot, prpm) : acc    -- 同じ軸：同じ回転
           else ( gDirAng g + pi + pi / gz - (prot - gDirAng g) * pz / gz   -- 歯が谷に入る角度
                , -prpm * pz / gz ) : acc        -- かみ合い：歯数の逆比で逆回転
    zAt i = case drop i zs of
      (z : _) -> z
      [] -> 1

step :: World -> World
step w = w { wRot0 = wRot0 w + (wRpm w / 60) * tau / 60 * 0.25 }   -- 画面は 1/4 のスローモーション

draw :: Env -> World -> Picture
draw env w = view env $
  concat (zipWith3 drawGear [0 ..] gs st)
  ++ concat (zipWith gearLabel gs st)
  ++ preview
  ++ [ txt 20 40 18 (hsb 0 0 100) ("Click: add gear   C: small gear on same shaft (z = " ++ show pinionZ ++ ")   Z: undo   Up/Down: input "
                                    ++ fmt 0 (wRpm w) ++ " rpm   R: reset")
     , txt 20 68 15 (hsb 0 0 60) "Red = input, yellow = output. Idler gears cancel out of the ratio. For big reduction use C (compound gear)."
     , Color (hsbA 0 0 0 55) (rect (ix - 16 + 210) (iy - 30 + 75) 420 150)
     , txt ix iy 16 (hsb 0 0 100) "n2 / n1 = - z1 / z2 (each mesh reverses)"
     , txt ix (iy + 28) 16 (hsb 35 90 100) ("Ratio i = n_in / n_out = " ++ fmt 2 ratio ++ (if abs ratio >= 1 then " (reduction)" else " (step-up)"))
     , txt ix (iy + 54) 16 (hsb 190 70 100) ("Output " ++ fmt 1 (abs lastRpm) ++ " rpm, " ++ (if ratio > 0 then "same way as input" else "opposite to input"))
     , txt ix (iy + 80) 16 (hsb 320 60 100) ("Out torque = " ++ fmt 1 inputTorque ++ " x |i| x eta = " ++ fmt 2 outTorque ++ " N.m")
     , txt ix (iy + 104) 14 (hsb 0 0 60) ("eta = " ++ show efficiency ++ "^" ++ show meshes ++ " = " ++ fmt 3 eta ++ " (loss of " ++ show meshes ++ " meshes)") ]
  where
    gs = wGears w
    st = updateTrain w
    n = length gs
    meshes = length [() | (i, g) <- zip [0 :: Int ..] gs, i > 0, not (gCoax g)]
    lastRpm = maybe (wRpm w) snd (lastOf st)
    ratio = wRpm w / (if abs lastRpm < 1e-9 then 1e-9 else lastRpm)   -- 速度比 i = 入力 / 出力
    eta = efficiency ^ meshes
    outTorque = inputTorque * abs ratio * eta
    ix = envW env - 430
    iy = envH env - 140
    drawGear :: Int -> Gear -> (Float, Float) -> [Picture]
    drawGear i g (rot, rpm) =
      let r = gearM * fromIntegral (gZ g) / 2
          hue | gCoax g = 320 | i == 0 = 8 | i == n - 1 = 45 | otherwise = 200
          fillC = hsbA hue 55 (if gCoax g then 60 else 75) (if gCoax g then 100 else 92)
          s = if rpm >= 0 then 1 else -1 :: Float
          ar = min (r * 0.5) 40
          ea = if s > 0 then -0.9 else -2.2
          (ex, ey) = (gX g + ar * cos ea, gY g + ar * sin ea)
          ta = atan2 (s * cos ea) (-s * sin ea)            -- 矢印の先での進行方向
          arcPts = [ (gX g + ar * cos a, gY g + ar * sin a) | k <- [0 .. 16 :: Int], let a = -2.2 + 1.3 * fromIntegral k / 16 ]
      in [ fillOutline fillC (hsbA 0 0 100 70) (outlineOf (gZ g)) (gX g, gY g) rot
         , dot (hsb 0 0 100) (gX g) (gY g) 10
         , dot (hsb 0 0 100) (gX g + (r - gearM * 2.2) * cos rot) (gY g + (r - gearM * 2.2) * sin rot) 6
         -- 回転方向の矢印（弧）
         , path 2 (hsbA 0 0 100 80) arcPts
         , seg 2 (hsbA 0 0 100 80) (ex, ey) (ex - 9 * cos (ta + 0.5), ey - 9 * sin (ta + 0.5))
         , seg 2 (hsbA 0 0 100 80) (ex, ey) (ex - 9 * cos (ta - 0.5), ey - 9 * sin (ta - 0.5)) ]
    -- ラベルは歯車の上に重ねて最後に描く
    gearLabel g (_, rpm) =
      let r = gearM * fromIntegral (gZ g) / 2
      in if not (gCoax g)
           then [txt (gX g - 46) (gY g + r + gearM + 18) 14 (hsb 0 0 100) ("z = " ++ show (gZ g) ++ "  " ++ fmt 1 (abs rpm) ++ " rpm")]
           else [txt (gX g - 30) (gY g - r - gearM - 8) 14 (hsb 0 0 100) ("coax z = " ++ show (gZ g))]
    -- マウスの向きに足せる歯車の下書き
    preview = case lastOf gs of
      Just l ->
        let (mx, my) = mouseP env
            d = dist2 (mx, my) (gX l, gY l)
            z = zFor l d
            a = gearM * fromIntegral (gZ l + z) / 2
        in [ Color (hsbA 0 0 100 30) (at (gX l + a * (mx - gX l) / d, gY l + a * (my - gY l) / d) (Circle (gearM * fromIntegral z / 2)))
           | d > 1 && my > 90 && n < maxGears ]
      Nothing -> []

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ | snd (mouseP env) > 90 -> addMesh (mouseP env) w
  EventKey (Char k) Down _ _
    | k `elem` "cC" -> case lastOf (wGears w) of
        Just p | length (wGears w) < maxGears && not (gCoax p) && gZ p > pinionZ ->
          w { wGears = wGears w ++ [Gear (gX p) (gY p) pinionZ (length (wGears w) - 1) True 0] }
        _ -> w
    | k `elem` "zZ" && length (wGears w) > 1 -> w { wGears = init' (wGears w) }
    | k `elem` "rR" -> resetTrain env w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wRpm = clampF 30 600 (wRpm w + 30) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wRpm = clampF 30 600 (wRpm w - 30) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 歯車を横に 3 枚並べよう。真ん中の歯数を変えても（Z で戻して距離を変える）速度比は同じ。なぜ？
-- 2. 大歯車 → C → 大歯車 → C → 大歯車 と 2 段の複合歯車列を作り、速度比 20 以上を目指そう
-- 3. かみ合いの回数が偶数なら同じ向き、奇数なら逆向き。確かめよう
-- 4. 時計の「分針 → 時針」は 1/12。歯数 8 以上 60 以下の歯車で 12 : 1 を作るには？（例：3 × 4）
-- 5. efficiency を 0.9 にすると、段数を増やしたときトルクはどれだけ損をする？

-- ---------- 歯車の道具（インボリュート歯形の輪郭を作る） ----------
-- 輪郭：歯底円の半径、歯 1 枚の側面 [(半径, 歯の中心からの角度)]、歯数
data Outline = Outline { oRf :: !Float, oFlank :: ![(Float, Float)], oZ :: !Int }

-- 歯数 z、モジュール m、圧力角 pa（ラジアン）、歯先・歯元の高さ ha・hf（モジュールの何倍か）
--   歯 1 枚の中心が角度 0（+x 方向）を向く
--   インボリュート：基礎円 rb に巻いた糸をほどくときの糸の先。ほどいた角 t の点は
--   半径 rb√(1 + t²)、角度 inv = t − atan t
makeGearOutline :: Int -> Float -> Float -> Float -> Float -> Outline
makeGearOutline z m pa' ha hf = Outline rf flank z
  where
    zf = fromIntegral z
    r = m * zf / 2
    rb = r * cos pa'
    ra = r + ha * m
    rf = max (r - hf * m) m
    half = pi / (2 * zf) + tan pa' - pa'                  -- 基礎円の上での歯の半角
    t0 = if rf > rb then sqrt ((rf / rb) * (rf / rb) - 1) else 0
    t1 = sqrt (max ((ra / rb) * (ra / rb) - 1) (t0 * t0))
    flank = [(rf, half) | rf < rb]                         -- 基礎円より内側はまっすぐ
         ++ [ (rb * sqrt (1 + t * t), max (half - (t - atan t)) 0)
            | i <- [0 .. 8 :: Int], let t = t0 + (t1 - t0) * fromIntegral i / 8 ]

-- 輪郭の点の列（p5.js 版と同じ順：歯底の真ん中 → 片側の歯面 → 反対側の歯面）
outlinePoints :: Outline -> Point -> Float -> [Point]
outlinePoints o c rot = concat
  [ rotPt c rot (oRf o * cos (a - pi / zf), oRf o * sin (a - pi / zf))
    : [rotPt c rot (fr * cos (a - fa), fr * sin (a - fa)) | (fr, fa) <- oFlank o]
    ++ [rotPt c rot (fr * cos (a + fa), fr * sin (a + fa)) | (fr, fa) <- reverse (oFlank o)]
  | k <- [0 .. oZ o - 1], let a = fromIntegral k * tau / zf ]
  where zf = fromIntegral (oZ o)

-- 輪郭を (x, y) に角度 rot で塗って縁取る。
--   歯車の形は凹んでいるので、歯底円の円板と、歯 1 枚ずつの凸な多角形に分けて塗る
fillOutline :: Color -> Color -> Outline -> Point -> Float -> Picture
fillOutline fc lc o c@(cx, cy) rot = Pictures
  [ Color fc (at (cx, cy) (disc (oRf o)))
  , Color fc (Pictures [ Polygon ([rotPt c rot (fr * cos (a - fa), fr * sin (a - fa)) | (fr, fa) <- oFlank o]
                                 ++ [rotPt c rot (fr * cos (a + fa), fr * sin (a + fa)) | (fr, fa) <- reverse (oFlank o)])
                       | k <- [0 .. oZ o - 1], let a = fromIntegral k * tau / fromIntegral (oZ o) ])
  , Color lc (poly (outlinePoints o c rot)) ]

rotPt :: Point -> Float -> Point -> Point
rotPt (cx, cy) rot (x, y) = (cx + x * cos rot - y * sin rot, cy + x * sin rot + y * cos rot)

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

-- 空のリストでも安全な last と init
lastOf :: [a] -> Maybe a
lastOf [] = Nothing
lastOf xs = Just (last xs)

init' :: [a] -> [a]
init' [] = []
init' xs = init xs
