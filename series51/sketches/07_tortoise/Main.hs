-- ============================================
-- 51-07 グレイ・ウォルターの亀（1948〜49）
-- 目（光センサー）1 つ、モーター 2 つ、真空管 2 本ほどの小さなロボット。それだけで「さがす・近づく・まぶしいと離れる・
-- ぶつかると押し返す・電池が減ると巣（充電所）へ帰る」をくり返し、生きもののように見える
-- 操作：ドラッグ = ランプを動かす / B = 電池を減らす / O = 障害物の ON/OFF / T = 軌跡を消す / R = やり直し
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import Numeric (showFFloat)
import System.Environment (lookupEnv)

wwW, wwH :: Double
wwW = 1200                 -- 世界の大きさ（画面に合わせて縮める）
wwH = 720

iSee, iDazzle :: Double
iSee = 0.12                -- ★ 「光が見えた」しきい値
iDazzle = 0.6              -- ★ 「まぶしい」しきい値

drain, lowBat :: Double
drain = 1 / 80             -- ★ 電池の減る速さ（1 秒あたり）
lowBat = 0.3               -- ★ この電池量より下で巣へ帰る

robotR :: Double
robotR = 18

-- ふるまい（状態）
data St = Explore | Approach | Avoid | Bump | Seek | Charge deriving Eq

stateName :: St -> String
stateName s = case s of
  Explore -> "searching"
  Approach -> "approaching"
  Avoid -> "dazzled: away"
  Bump -> "bumped: push back"
  Seek -> "low battery: home"
  Charge -> "charging"

-- 光（位置と強さ）と障害物（位置と半径）
data Light = Light { lX, lY, lP :: !Double }
data Obs = Obs { oX, oY, oR :: !Double }

-- 亀：位置・車体の向き th・目の向き psi（車体に対して）・状態・電池・記録
data Tort = Tort { tX, tY, tTh, tPsi :: !Double, tState :: !St, tTimer, tBat :: !Double
                 , tBumps, tCharges :: !Int, tMinLamp :: !Double }

-- 世界（ランプ・巣・障害物）
data Arena = Arena { aLamp, aHutch :: !Light, aObs :: [Obs] }

data World = World
  { wLamp   :: !Light
  , wUseObs :: !Bool
  , wT      :: !Tort
  , wTrail  :: Seq (Double, Double)     -- 軌跡（900 点まで）
  , wHist   :: Seq (Double, Double)     -- 光と電池の履歴（600 個まで）
  , wI      :: !Double
  , wDrag   :: !Bool
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "51-07 Grey Walter's tortoise" world0)
  { skBack  = col (24, 35, 10) 100
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

lamp0, hutch0 :: Light
lamp0 = Light 850 330 1
hutch0 = Light 110 610 1.4

world0 :: World
world0 = resetAll (World lamp0 True makeTortoise Seq.empty Seq.empty 0 False [])

makeObstacles :: [Obs]
makeObstacles = [Obs 520 250 60, Obs 640 560 50, Obs 330 380 45]

makeTortoise :: Tort
makeTortoise = Tort 300 150 0.3 0 Explore 0 1 0 0 1e9

resetAll :: World -> World
resetAll w = w { wT = makeTortoise, wTrail = Seq.empty, wHist = Seq.empty }

arenaOf :: World -> Arena
arenaOf w = Arena (wLamp w) hutch0 (if wUseObs w then makeObstacles else [])

-- ---- 光センサー：明るさ = P ÷ (1 + (距離/200)²) × （目の向きとのずれの cos の 4 乗）
lightFrom :: Light -> Double -> Double -> Double -> Double
lightFrom l x y dir =
  let dx = lX l - x
      dy = lY l - y
      d = sqrt (dx * dx + dy * dy)
      c = cos (atan2 dy dx - dir)
  in if c <= 0 then 0 else (lP l / (1 + (d / 200) ^ (2 :: Int))) * c ^ (4 :: Int)

sense :: Tort -> Arena -> Double
sense t a
  | tBat t < lowBat || tState t == Charge = lightFrom (aHutch a) (tX t) (tY t) dir   -- 電池が少ないと巣の光だけを見る（単純化）
  | otherwise = lightFrom (aLamp a) (tX t) (tY t) dir + 0.3 * lightFrom (aHutch a) (tX t) (tY t) dir
  where dir = tTh t + tPsi t

-- 角度の差を −π〜π に（JavaScript の % と同じ jsMod を使う）
angDiff :: Double -> Double -> Double
angDiff a b =
  let d0 = jsMod (a - b) (2 * pi)
      d1 = if d0 > pi then d0 - 2 * pi else d0
  in if d1 < -pi then d1 + 2 * pi else d1

-- ---- 1 きざみ（dt 秒）：状態ごとに速さ v と回る速さ ω を決めて動かす。戻り値は (目に入った光, 新しい亀)
tortoiseStep :: Arena -> Double -> Tort -> (Double, Tort)
tortoiseStep a dt t0 = (iNow, t4)
  where
    iNow = sense t0 a
    t1 = t0 { tBat = clampv 0 1 (tBat t0 - drain * dt), tTimer = tTimer t0 - dt }
    dh = sqrt ((tX t1 - lX (aHutch a)) ^ (2 :: Int) + (tY t1 - lY (aHutch a)) ^ (2 :: Int))
    (t2, v, om)
      | tState t1 == Charge =
          let b = clampv 0 1 (tBat t1 + 0.25 * dt)
          in if b >= 1 then (t1 { tBat = b, tState = Avoid, tTimer = 1.5, tCharges = tCharges t1 + 1 }, 0, 0) else (t1 { tBat = b }, 0, 0)
      | tState t1 == Bump || tState t1 == Avoid =
          ( if tTimer t1 <= 0 then t1 { tState = Explore } else t1, if tState t1 == Bump then -50 else 30, 2.8 )
      | tBat t1 < lowBat && dh < 45 = (t1 { tState = Charge }, 0, 0)
      | tBat t1 >= lowBat && iNow > iDazzle = (t1 { tState = Avoid, tTimer = 1.2 }, 0, 0)
      | iNow > iSee =                        -- 光が見えた：目を明るい側へ少しずつ寄せ、目の向きへ車体を回す
          let ts = t1 { tState = if tBat t1 < lowBat then Seek else Approach }
              ip = sense ts { tPsi = tPsi ts + 0.05 } a
              im = sense ts { tPsi = tPsi ts - 0.05 } a
              psi1 = tPsi ts + 2.5 * dt * signum (ip - im)
              turn = clampv (-2.5) 2.5 (3 * psi1)
          in (ts { tPsi = psi1 - turn * dt }, 110, turn)
      | otherwise =                          -- さがす：目をぐるぐる回しながらゆっくり進む
          (t1 { tState = Explore, tPsi = angDiff (tPsi t1 + 3.0 * dt) 0 }, 60, 0.6)
    th = tTh t2 + om * dt
    nx = tX t2 + v * cos th * dt
    ny = tY t2 + v * sin th * dt
    hit = nx < robotR || nx > wwW - robotR || ny < robotR || ny > wwH - robotR
          || any (\o -> sqrt ((nx - oX o) ^ (2 :: Int) + (ny - oY o) ^ (2 :: Int)) < oR o + robotR) (aObs a)
    t3 | hit && tState t2 /= Bump = t2 { tTh = th, tState = Bump, tTimer = 0.7, tBumps = tBumps t2 + 1 }
       | not hit = t2 { tTh = th, tX = nx, tY = ny }
       | otherwise = t2 { tTh = th }
    t4 = t3 { tMinLamp = min (tMinLamp t3) (sqrt ((tX t3 - lX (aLamp a)) ^ (2 :: Int) + (tY t3 - lY (aLamp a)) ^ (2 :: Int))) }

step :: Env -> World -> World
step env w = w { wT = t', wI = i', wTrail = trail, wHist = keepLast 600 (wHist w |> (i', tBat t')) }
  where
    ar = arenaOf w
    (_, ta) = tortoiseStep ar (1 / 120) (wT w)
    (i', t') = tortoiseStep ar (1 / 120) ta
    trail = if envFrame env `mod` 3 == 0 then keepLast 900 (wTrail w |> (tX t', tY t')) else wTrail w

-- 画面への縮尺と左上
layout :: Env -> (Float, Float, Float)
layout env = (min ((envW env - 380) / f2 wwW) ((envH env - 110) / f2 wwH), 20, 90)

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Grey Walter's tortoise - one eye, two motors, yet it looks alive" 20 34 20 cTxt
  , say ("Drag: move the lamp   B drain battery   O obstacles " ++ onOff (wUseObs w) ++ "   T clear trail   R restart") 20 60 15 cSub
  , Translate ox oy (Scale sc sc (Pictures arena))
  , say "lamp" (ox + f2 (lX lamp) * sc - 20) (oy + f2 (lY lamp) * sc + 34) 14 cBrass
  , say "hutch (charger)" (ox + f2 (lX hutch0 - 50) * sc) (oy + f2 (lY hutch0 - 58) * sc) 14 cScope
    -- ---- 計器
  , say "Behaviour now" hx hy 14 cSub
  , say (stateName st) hx (hy + 30) 20 (if st == Bump || st == Avoid then cAlarm else if st == Seek || st == Charge then cScope else cTube)
  , say ("Light in the eye I = " ++ fmt 3 (wI w)) hx (hy + 66) 15 cTxt
  , say ("seen > " ++ num iSee ++ "   dazzled > " ++ num iDazzle) hx (hy + 88) 14 cSub
  , say ("Battery " ++ fmt 0 (100 * tBat t) ++ "%" ++ (if tBat t < lowBat then " (low: home)" else "")) hx (hy + 118) 15 (if tBat t < lowBat then cAlarm else cTxt)
  , Color (hsba 0 0 100 0.1) (rectTL hx (hy + 128) barW 14)
  , Color (col (if tBat t < lowBat then cAlarm else cScope) 100) (rectTL hx (hy + 128) (barW * f2 (tBat t)) 14)
  , say ("Closest to the lamp: " ++ fmt 0 (tMinLamp t)) hx (hy + 170) 14 cTxt
  , say ("Bumps " ++ show (tBumps t) ++ "   charges " ++ show (tCharges t)) hx (hy + 194) 14 cTxt
  , plotFrame hx gy gw gh "I orange, battery green"
  , hLine iDazzle hx gy gw gh 0 1.2 cAlarm
  , hLine iSee hx gy gw gh 0 1.2 cBrass
  , plotLine (map fst hist) hx gy gw gh 0 1.2 cTube 600 1.5
  , plotLine (map snd hist) hx gy gw gh 0 1.2 cScope 600 2
  , drawSelfTest env (wTest w) ]
  where
    (sc, ox, oy) = layout env
    t = wT w
    st = tState t
    lamp = wLamp w
    hist = toList (wHist w)
    hx = ox + f2 wwW * sc + 30
    hy = 110
    barW = min 300 (envW env - hx - 30)
    gy = hy + 220
    gw = envW env - hx - 30
    gh = min 200 (envH env - gy - 60)
    p (x, y) = (f2 x, f2 y)
    -- 世界の中（世界の座標で描き、まとめて縮める）
    arena =
      [ Color (hsb 28 25 14) (rectTL 0 0 (f2 wwW) (f2 wwH)) ]
      ++ [ Color (hsba hue 60 100 0.06) (at (p (lX l, lY l)) (disc (fromIntegral r * 45))) | (l, hue) <- [(lamp, 45), (hutch0, 140)], r <- [5, 4, 3, 2, 1 :: Int] ]
      ++ [ Color (hsb 48 40 100) (at (p (lX lamp, lY lamp)) (disc 15))
         , Color (col cScope 100) (frameTL 3 (f2 (lX hutch0) - 50) (f2 (lY hutch0) - 50) 100 100)
         , Color (col cScope 100) (at (p (lX hutch0, lY hutch0)) (disc 9)) ]
      ++ [ Color (hsb 30 20 35) (at (p (oX o, oY o)) (disc (f2 (oR o)))) | o <- arenaObs ]
      ++ [ Color (col cTube 45) (thickPath 2 (map p (toList (wTrail w)))) | Seq.length (wTrail w) > 1 ]
      ++ [ Translate (f2 (tX t)) (f2 (tY t)) (Rotate (negate (f2 (tTh t)) * 180 / pi) (Pictures robot)) ]
    arenaObs = if wUseObs w then makeObstacles else []
    ps = f2 (tPsi t)
    robot =
      [ Color (col cBrass 100) (Scale 1.3 1 (disc (f2 robotR)))
      , Color (hsb 25 50 30) (Scale (16 / 12) 1 (disc (f2 robotR * 0.6)))
      , Color (col cTube 100) (thickLine 3 (0, 0) (60 * cos ps, 60 * sin ps))
      , Color (col cTube 18) (Polygon ((0, 0) : [ (130 * cos a, 130 * sin a) | k <- [0 .. 12 :: Int], let a = ps - 0.45 + 0.9 * fromIntegral k / 12 ])) ]

-- マウスの位置を世界の座標に
toWorld :: Env -> (Double, Double)
toWorld env = let (mx, my) = p5Mouse env
                  (sc, ox, oy) = layout env
              in (realToFrac ((mx - ox) / sc), realToFrac ((my - oy) / sc))

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (x, y) = toWorld env
        l = wLamp w
        drag = sqrt ((x - lX l) ^ (2 :: Int) + (y - lY l) ^ (2 :: Int)) < 80
    in if not drag && x > 0 && x < wwW && y > 0 && y < wwH then w { wDrag = False, wLamp = l { lX = x, lY = y } } else w { wDrag = drag }
  EventMotion _ | wDrag w && envDown env ->
    let (x, y) = toWorld env
    in w { wLamp = (wLamp w) { lX = clampv 20 (wwW - 20) x, lY = clampv 20 (wwH - 20) y } }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = False }
  EventKey (Char c) Down _ _
    | c `elem` "bB" -> w { wT = (wT w) { tBat = min (tBat (wT w)) 0.2 } }
    | c `elem` "oO" -> w { wUseObs = not (wUseObs w) }
    | c `elem` "tT" -> w { wTrail = Seq.empty }
    | c `elem` "rR" -> resetAll w
  _ -> w

-- ---- 自己テスト（決まった世界で、決まった時間だけ動かす。p5.js 版と同じ 5 項目）
data Run = Run { rT :: Tort, rMinObs :: Double, rOk :: Bool, rMaxBat :: Double }

runWorld :: Double -> Double -> Bool -> Run
runWorld tEnd bat withObs = foldl' go (Run makeTortoise { tBat = bat } 1e9 True 0) [0 .. round (tEnd * 120) - 1 :: Int]
  where
    ar = Arena lamp0 hutch0 (if withObs then makeObstacles else [])
    go (Run t mo ok mb) k =
      let (_, t') = tortoiseStep ar (1 / 120) t
          mo' = foldl' (\m o -> min m (sqrt ((tX t' - oX o) ^ (2 :: Int) + (tY t' - oY o) ^ (2 :: Int)) - oR o - robotR)) mo (aObs ar)
          ok' = ok && all (\v -> not (isNaN v || isInfinite v)) [tX t', tY t', tTh t', tPsi t', tBat t']
          mb' = if k > 10 then max mb (tBat t') else mb
      in mo' `seq` mb' `seq` Run t' mo' ok' mb'

selfTest :: [(String, Bool)]
selfTest =
  [ ("light sensor: darker when farther (monotone), 0 from straight behind", mono && lightFrom l0 100 0 0 == 0)
  , ("approaches the lamp (closest " ++ fmt 0 (tMinLamp (rT a)) ++ " < 250) but not inside half the dazzle distance " ++ fmt 0 dz
    , tMinLamp (rT a) < 250 && tMinLamp (rT a) > dz / 2)
  , ("from 25% battery: goes home and charges (" ++ show (tCharges (rT b)) ++ " charges in 90 s, max battery " ++ fmt 0 (100 * rMaxBat b) ++ "%)"
    , rMaxBat b > 0.95 && tCharges (rT b) >= 1)
  , ("with obstacles it never sinks in (min gap " ++ fmt 1 (rMinObs c) ++ " >= 0), pushed back " ++ show (tBumps (rT c)) ++ " times", rMinObs c >= -1e-9)
  , ("120 s of running: position, heading, battery never NaN", rOk a && rOk b && rOk c) ]
  where
    l0 = Light 0 0 1
    vals = [ lightFrom l0 (-d) 0 0 | d <- [0, 10 .. 990] ]
    mono = and (zipWith (>=) (2 : vals) vals)
    a = runWorld 25 1 False
    dz = 200 * sqrt (1 / iDazzle - 1)
    b = runWorld 90 0.25 False
    c = runWorld 120 1 True

-- ---------- やってみよう ----------
-- 1. ランプをドラッグして動かそう。亀はついてくる？ 近づきすぎると何をする？
-- 2. B で電池を減らすと、ふるまいはどう変わる？ 巣で充電したあとは？
-- 3. iDazzle を 2 にすると（まぶしさを感じない）、ランプのまわりでどう動く？
-- 4. ランプを巣のすぐそばに置くと、亀はどちらへ行く？
-- 5. ウォルターは、亀が鏡の前で「自分を見て」ふらふらする様子を観察した。センサーの前に自分の光があったら？ と想像しよう

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

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

onOff :: Bool -> String
onOff b = if b then "ON" else "OFF"

-- オシロスコープ風の枠
plotFrame :: Float -> Float -> Float -> Float -> String -> Picture
plotFrame x y w h title = Pictures $
  [ Color (hsba 130 40 6 0.9) (rectTL x y w h) ]
  ++ [ Color (hsba 130 40 40 0.4) (Line [(x, y + h * k / 4), (x + w, y + h * k / 4)]) | k <- [1, 2, 3] ]
  ++ [ Color (hsb 42 55 60) (frameTL 1 x y w h) ]
  ++ [ say title (x + 8) (y + 18) 14 cSub | not (null title) ]

-- arr の値を lo〜hi の範囲で、枠 (x, y, w, h) に折れ線で描く（n = 横に並べる点の数）
plotLine :: [Double] -> Float -> Float -> Float -> Float -> Double -> Double -> HSB -> Int -> Float -> Picture
plotLine arr x y w h lo hi c n wt
  | length arr < 2 = Blank
  | otherwise = Color (col c 100) (thickPath wt [ (x + fromIntegral i / fromIntegral (n - 1) * w, y + h - f2 (clampv 0 1 ((fin lo v - lo) / (hi - lo))) * h)
                                              | (i, v) <- zip [0 :: Int ..] arr ])

-- グラフの中の水平線（目標値など、点線）
hLine :: Double -> Float -> Float -> Float -> Float -> Double -> Double -> HSB -> Picture
hLine v x y w h lo hi c =
  let yy = y + h - f2 (clampv 0 1 ((v - lo) / (hi - lo))) * h
  in Color (col c 70) (Pictures [ Line [(x + s, yy), (x + min w (s + 6), yy)] | s <- [0, 11 .. w] ])

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- JavaScript の数の書き方に近い形（整数なら小数点なし）
num :: Double -> String
num v | v == fromIntegral (round v :: Int) = show (round v :: Int)
      | otherwise = showFFloat Nothing v ""

-- 新しい方から n 個だけ残す
keepLast :: Int -> Seq a -> Seq a
keepLast n s = if Seq.length s > n then Seq.drop (Seq.length s - n) s else s

-- JavaScript の a % b（割った商を 0 の向きに切り捨てた余り。符号は a と同じ）
jsMod :: Double -> Double -> Double
jsMod a b = a - b * fromIntegral (truncate (a / b) :: Integer)

-- ---- 自己テストの道具
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
