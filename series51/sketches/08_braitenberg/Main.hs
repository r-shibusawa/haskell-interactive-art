-- ============================================
-- 51-08 ブライテンベルクの乗り物（1984）
-- センサー 2 つ・モーター 2 つ。つなぎ方を変えるだけで、見る人には「こわがり」「攻撃的」「好き」「探検家」に見える
-- 同じ側／交差 × 興奮（明るいほど速く）／抑制（明るいほど遅く）の 4 通り。中身は数行の足し算
-- 操作：1〜4 = 全員のつなぎ方 / M = ばらばら（混ぜる） / ドラッグ = 光を動かす / クリック（空き地）= 光を足す
--       V = 乗り物を足す / T = 軌跡を消す / R = やり直し
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (findIndex, foldl')
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- 配線：名前、交差するか、抑制か
data Wire = Wire { wiName :: String, wiCross, wiInhib :: !Bool }

wires :: [Wire]
wires = [ Wire "2a fear" False False, Wire "2b aggression" True False, Wire "3a love" False True, Wire "3b explorer" True True ]

vMax, base, ks :: Double
vMax = 120                 -- ★ モーターの最大の速さ
base = 8                   -- ★ モーターの最小の速さ
ks = 160                   -- ★ センサーの効きぐあい

axle, sensA, sensD :: Double
axle = 30                  -- 車輪の間
sensA = 0.5                -- センサーの角度
sensD = 16                 -- センサーの前への距離

-- 光（位置と強さ）
data Lamp = Lamp { lX, lY, lP :: !Double }

-- 乗り物：位置・向き・配線・軌跡（Nothing は画面の端をこえた切れ目）・センサーとモーターの値
data Car = Car { cX, cY, cTh :: !Double, cW :: !Int, cTrail :: [Maybe (Double, Double)]
               , cSL, cSR, cML, cMR :: !Double }

data World = World
  { wLamps :: [Lamp]
  , wCars  :: [Car]
  , wDragI :: !Int
  , wSel   :: !Int
  , wMix   :: !Bool
  , wW, wH :: !Double
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "51-08 Braitenberg vehicles" (World [] [] (-1) 0 True 1280 800 []))
  { skBack  = col (24, 35, 10) 100
  , skStart = \env -> do
      printSelfTest selfTest
      pure (resetAll (World [] [] (-1) 0 True (realToFrac (envW env)) (realToFrac (envH env)) selfTest))
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

resetAll :: World -> World
resetAll w = w { wLamps = [Lamp (wd * 0.45) (ht * 0.5) 1, Lamp (wd * 0.72) (ht * 0.35) 1]
               , wCars = [ makeCar (wd * (0.1 + 0.1 * fromIntegral i)) (ht * (if odd i then 0.25 else 0.8)) (fromIntegral i * 0.8)
                                   (if wMix w then i `mod` 4 else wSel w) | i <- [0 .. 7 :: Int] ] }
  where wd = wW w
        ht = wH w

makeCar :: Double -> Double -> Double -> Int -> Car
makeCar x y th wi = Car x y th wi [] 0 0 0 0

-- ---- センサー：明るさ = Σ P ÷ (1 + (距離/150)²) × (0.5 + 0.5 cos(センサーの向きと光の向きのずれ))
sensorAt :: Double -> Double -> Double -> [Lamp] -> Double
sensorAt x y dir = foldl' (\s l -> let dx = lX l - x
                                       dy = lY l - y
                                       d = sqrt (dx * dx + dy * dy)
                                   in s + (lP l / (1 + (d / 150) ^ (2 :: Int))) * (0.5 + 0.5 * cos (atan2 dy dx - dir))) 0

-- ---- 1 きざみ：センサー → （配線）→ モーター → 車輪の速さのちがいで曲がる。戻り値は (速さ, 回る速さ, 新しい乗り物)
carStep :: [Lamp] -> Double -> Double -> Double -> Bool -> Car -> (Double, Double, Car)
carStep ls dt ww hh wrap c = (v, om, c { cSL = sL, cSR = sR, cML = mL, cMR = mR, cTh = th, cX = x', cY = y' })
  where
    a = cTh c - sensA                      -- 画面は y が下向きなので、角度が小さい側 = 左
    b = cTh c + sensA
    sL = sensorAt (cX c + sensD * cos a) (cY c + sensD * sin a) a ls
    sR = sensorAt (cX c + sensD * cos b) (cY c + sensD * sin b) b ls
    wi = getAt (head' wires) wires (cW c)
    (inL, inR) = if wiCross wi then (sR, sL) else (sL, sR)
    motor s = clampv 0 (vMax * 1.5) (if wiInhib wi then vMax - ks * s else base + ks * s)
    mL = motor inL
    mR = motor inR
    v = (mL + mR) / 2
    om = (mL - mR) / axle                  -- 左が速い → 右へ曲がる
    th = cTh c + om * dt
    x1 = cX c + v * cos th * dt
    y1 = cY c + v * sin th * dt
    (x', y') = if wrap then (jsMod (x1 + ww) ww, jsMod (y1 + hh) hh) else (x1, y1)
    head' (z : _) = z
    head' [] = Wire "" False False

step :: Env -> World -> World
step env w = w { wCars = map one (wCars w) }
  where
    ls = wLamps w
    wd = realToFrac (envW env)
    ht = realToFrac (envH env)
    adv c = let (_, _, c') = carStep ls (1 / 120) wd ht True c in c'
    one c =
      let c2 = adv (adv c)
          tr0 = if abs (cX c2 - cX c) > 50 || abs (cY c2 - cY c) > 50 then cTrail c ++ [Nothing] else cTrail c
          tr1 = if envFrame env `mod` 3 == 0
                  then let t = tr0 ++ [Just (cX c2, cY c2)] in if length t > 240 then drop 1 t else t
                  else tr0
      in c2 { cTrail = tr1 }

carCols :: [HSB]
carCols = [(200, 50, 95), cAlarm, cScope, cBrass]

draw :: Env -> World -> Picture
draw env w = view env $
  concat [ [ Color (hsba 48 60 100 0.05) (at (p (lX l, lY l)) (disc (fromIntegral r * 35))) | r <- [6, 5 .. 1 :: Int] ]
           ++ [ Color (hsb 48 40 100) (at (p (lX l, lY l)) (disc 13)) ] | l <- wLamps w ]
  ++ concat [ drawCar c | c <- wCars w ]
  ++ [ say "Braitenberg vehicles - the wiring alone creates a 'character'" 20 34 20 cTxt
     , say ("1-4 wiring for all   M mix" ++ (if wMix w then " (now)" else "") ++ "   Drag: move a light") 20 60 15 cSub
     , say "Click empty space: add light   V add vehicle   T clear trails   R reset" 20 82 15 cSub
       -- 配線の図（4 種類）
     , Color (hsba 25 35 13 0.92) (rectTL (px - 10) (py - 10) 300 340) ]
  ++ concat (zipWith wireDiagram [0 ..] wires)
  ++ case wCars w of
       (c : _) -> [ say ("Vehicle 1 (" ++ wiName (getAt (Wire "" False False) wires (cW c)) ++ ")   sensors L " ++ fmt 3 (cSL c) ++ " R " ++ fmt 3 (cSR c)
                         ++ "  ->  motors L " ++ fmt 1 (cML c) ++ " R " ++ fmt 1 (cMR c) ++ "  ->  turning " ++ fmt 2 ((cML c - cMR c) / axle) ++ " rad/s")
                         20 (envH env - 44) 14 cTxt ]
       [] -> []
  ++ [ drawSelfTest env (wTest w) ]
  where
    p (x, y) = (f2 x, f2 y)
    px = envW env - 310
    py = 90
    drawCar c =
      let cc = getAt cTxt carCols (cW c)
          segs = splitSegs (cTrail c ++ [Just (cX c, cY c)])
      in [ Color (col cc 35) (thickPath 2 (map p s)) | s <- segs, length s > 1 ]
         ++ [ Translate (f2 (cX c)) (f2 (cY c)) (Rotate (negate (f2 (cTh c)) * 180 / pi) (Pictures
              [ Color (col cc 100) (rectTL (-16) (-12) 30 24)
              , Color (hsb 0 0 15) (rectTL (-12) (-16) 14 5), Color (hsb 0 0 15) (rectTL (-12) 11 14 5)
              , Color (hsb 48 60 (60 + 40 * f2 (min 1 (cSL c)))) (at (p (sensD * cos (-sensA), sensD * sin (-sensA))) (disc 4.5))
              , Color (hsb 48 60 (60 + 40 * f2 (min 1 (cSR c)))) (at (p (sensD * cos sensA, sensD * sin sensA)) (disc 4.5)) ])) ]
    wireDiagram :: Int -> Wire -> [Picture]
    wireDiagram i wi =
      let x = px + fromIntegral (i `mod` 2) * 140
          y = py + fromIntegral (i `div` 2) * 150
          sx1 = x + 30
          sx2 = x + 90
          sy = y + 34
          my = y + 110
          cc = getAt cTxt carCols i
          ln a b = Color (col cc 100) (if wiInhib wi then dashed 3 a b else thickLine 3 a b)
      in [ say (wiName wi) x (y + 14) 14 cc
         , Color (hsb 48 60 90) (at (sx1, sy) (disc 6)), Color (hsb 48 60 90) (at (sx2, sy) (disc 6))
         , Color (hsb 0 0 60) (rectTL (sx1 - 8) my 16 22), Color (hsb 0 0 60) (rectTL (sx2 - 8) my 16 22) ]
         ++ (if wiCross wi then [ln (sx1, sy) (sx2, my), ln (sx2, sy) (sx1, my)] else [ln (sx1, sy) (sx1, my), ln (sx2, sy) (sx2, my)])
         ++ [ say (if wiInhib wi then "inhibitory" else "excitatory") (x + 18) (y + 140) 14 cSub ]

-- 軌跡を切れ目（Nothing）で分ける
splitSegs :: [Maybe a] -> [[a]]
splitSegs xs = case break (\m -> case m of { Nothing -> True; _ -> False }) xs of
  (a, []) -> [ [ v | Just v <- a ] ]
  (a, _ : rest) -> [ v | Just v <- a ] : splitSegs rest

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = p5Mouse env
        x = realToFrac mx
        y = realToFrac my
        i = maybe (-1) id (findIndex (\l -> sqrt ((x - lX l) ^ (2 :: Int) + (y - lY l) ^ (2 :: Int)) < 30) (wLamps w))
    in pure $ if i < 0 && mx < envW env - 310 && length (wLamps w) < 5 then w { wDragI = i, wLamps = wLamps w ++ [Lamp x y 1] } else w { wDragI = i }
  EventMotion _ | wDragI w >= 0 && envDown env ->
    let (mx, my) = p5Mouse env
    in pure w { wLamps = [ if k == wDragI w then l { lX = realToFrac mx, lY = realToFrac my } else l | (k, l) <- zip [0 ..] (wLamps w) ] }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDragI = -1 }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> let s = fromEnum c - fromEnum '1' in pure w { wSel = s, wMix = False, wCars = [ k { cW = s } | k <- wCars w ] }
    | c `elem` "mM" -> pure w { wMix = True, wCars = [ k { cW = i `mod` 4 } | (i, k) <- zip [0 ..] (wCars w) ] }
    | c `elem` "vV" && length (wCars w) < 24 -> do
        x <- randomRIO (0, realToFrac (envW env))
        y <- randomRIO (0, realToFrac (envH env))
        th <- randomRIO (0, 2 * pi)
        pure w { wCars = wCars w ++ [makeCar x y th (if wMix w then length (wCars w) `mod` 4 else wSel w)] }
    | c `elem` "tT" -> pure w { wCars = [ k { cTrail = [] } | k <- wCars w ] }
    | c `elem` "rR" -> pure (resetAll w { wW = realToFrac (envW env), wH = realToFrac (envH env) })
  _ -> pure w

-- ---- 自己テスト（光 1 つ、まわりに壁なし。p5.js 版と同じ 6 項目）
data Run = Run { rC :: Car, rMinD, rD, rD0, rV :: Double }

selfTest :: [(String, Bool)]
selfTest =
  [ ("light dead ahead: all 4 wirings drive both motors equally -> straight"
    , and [ near (omOf l1 i) 0 1e-12 | i <- [0 .. 3] ])
  , ("light ahead-left: 2a and 3b turn right (away), 2b and 3a turn left (toward)"
    , case map (omOf lLeft) [0 .. 3] of { [o0, o1, o2, o3] -> o0 > 0 && o3 > 0 && o1 < 0 && o2 < 0; _ -> False })
  , ("2b aggression: hits the light (closest " ++ fmt 1 (rMinD b) ++ " < 20)", rMinD b < 20)
  , ("2a fear: after 25 s farther (" ++ fmt 0 (rD a) ++ ") than at the start (" ++ fmt 0 (rD0 a) ++ ")", rD a > rD0 a)
  , ("3a love: near the light (" ++ fmt 0 (rD l) ++ " < 150) and slow (speed " ++ fmt 1 (rV l) ++ " < " ++ num (0.25 * vMax) ++ ")"
    , rD l < 150 && rV l < 0.25 * vMax)
  , ("no wiring gives NaN after 30 s", all (\r -> let c = rC r in not (isNaN (cX c + cY c + cTh c) || isInfinite (cX c + cY c + cTh c))) [a, b, l, e]) ]
  where
    l1 = [Lamp 400 0 1]
    lLeft = [Lamp 300 (-200) 1]                  -- 前の左（y が上 = 画面の上）
    omOf ls i = let (_, om, _) = carStep ls 1e-9 0 0 False (makeCar 0 0 0 i) in om
    dist c = sqrt ((cX c - 400) ^ (2 :: Int) + cY c ^ (2 :: Int))
    run wi tEnd =
      let go (c, md, _) _ = let (v, _, c') = carStep l1 (1 / 120) 0 0 False c in (c', min md (dist c'), v)
          (cEnd, minD, vEnd) = foldl' go (makeCar 0 300 (-0.4) wi, 1e9, 0) [1 .. round (tEnd * 120) :: Int]
      in Run cEnd minD (dist cEnd) (sqrt (400 * 400 + 300 * 300)) vEnd
    b = run 1 25
    a = run 0 25
    l = run 2 30
    e = run 3 30

-- ---------- やってみよう ----------
-- 1. 1〜4 で全員のつなぎ方をそろえ、光をドラッグして動かそう。どれが「こわがり」に見える？
-- 2. 光を 2 つ近くに置くと、3a（好き）はどちらを選ぶ？ 3b（探検家）は？
-- 3. ks を 2 倍にすると、性格は「強く」なる？
-- 4. 自分で配線を考えよう：左センサーを両方のモーターに興奮でつなぐと？（wires に 1 行足して carStep を少し変える）
-- 5. ブライテンベルクの「作るのはやさしく、外から見て中身を当てるのは難しい」（下から作る心理学）を、06 のブラックボックスと比べよう

-- ==================================================
-- ここから下は「サイバネティクスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で
p5Mouse :: Env -> (Float, Float)
p5Mouse env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 色は (色相, 彩度, 明るさ)。透明度は 0〜100
type HSB = (Float, Float, Float)

cTxt, cSub, cScope, cTube, cBrass, cAlarm :: HSB
cTxt = (40, 12, 96)
cSub = (35, 18, 72)
cScope = (135, 70, 92)
cTube = (24, 85, 100)
cBrass = (42, 55, 88)
cAlarm = (0, 80, 100)

col :: HSB -> Float -> Color
col (h, s, b) a = hsba h s b (a / 100)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> HSB -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size (col c 100) s))

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 点線（太さ wt、5 描いて 4 あける）
dashed :: Float -> Point -> Point -> Picture
dashed wt (x1, y1) (x2, y2) =
  let l = sqrt ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int))
      pt s = (x1 + (x2 - x1) * s / l, y1 + (y2 - y1) * s / l)
  in if l < 1e-6 then Blank else Pictures [ thickLine wt (pt s) (pt (min l (s + 5))) | s <- [0, 9 .. l] ]

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

f2 :: Double -> Float
f2 = realToFrac

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (if isNaN v || isInfinite v then 0 else v) ""

-- JavaScript の数の書き方に近い形（整数なら小数点なし）
num :: Double -> String
num v | v == fromIntegral (round v :: Int) = show (round v :: Int)
      | otherwise = showFFloat Nothing v ""

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- JavaScript の a % b（割った商を 0 の向きに切り捨てた余り。符号は a と同じ）
jsMod :: Double -> Double -> Double
jsMod a b = a - b * fromIntegral (truncate (a / b) :: Integer)

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 16) 14 (if all snd rs then cScope else cAlarm)
