-- ============================================
-- 53-05 惑星の逆行 ─ 火星がときどき「後もどり」して見えるのはなぜ？
-- 04 と同じ軌道要素で地球と火星の位置を計算し、地球から見た黄経 λ・黄緯 β を星空の上に描く
-- 地球が内側から火星を追いこす「衝」のころだけ、λ が減る（西へもどる）＝ 逆行
-- P = プトレマイオスの周転円（円軌道の近似）を重ねる：周転円の半径 = 地球の軌道、回る速さ = 太陽 → 地動説と同じ見え方
-- 操作：←→ = 前・次の衝 / 1 火星 2 木星 3 土星 / スペース = 再生/停止 / P = 周転円 / T = 今日に近い衝
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Vector as V
import Foreign.C.Types (CTime(..))
import Foreign.Ptr (Ptr, nullPtr)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- 名前と、衝の前後に描く日数
pls :: [(Planet, Int)]
pls = [(Mars, 220), (Jupiter, 200), (Saturn, 200)]   -- ★

-- 1 日分の記録：衝からの日数、JD、黄経、黄緯、周転円の黄経、地球と惑星の位置、逆行中か
data TP = TP { tD :: !Int, tJ, tLam, tBet, tEpi :: !Double, tE, tP :: !(Double, Double), tRetro :: !Bool }

data World = World
  { wPi      :: !Int
  , wJdOpp   :: !Double
  , wT0      :: !Double
  , wPlaying :: !Bool
  , wEpi     :: !Bool
  , wTrack   :: !(V.Vector TP)
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "53-05 Retrograde motion" world0)
  { skBack  = rgb255 6 10 28
  , skStart = \_ -> do
      printSelfTest selfTest
      now <- jdNowUT
      let o = nextOpposition Mars (now - 400)
      pure world0 { wJdOpp = o, wTrack = makeTrack 0 o, wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 0 2459136 0 True True (makeTrack 0 2459136) []

plOf :: Int -> (Planet, Int)
plOf i = getAt (Mars, 220) pls i

step :: World -> World
step w
  | wPlaying w = w { wT0 = jsRem (wT0 w + 0.5) (fromIntegral (max 1 (V.length (wTrack w)))) }
  | otherwise = w

-- 衝からのずれ：地球から見た惑星と太陽の黄経の差 − 180°（0 になる日が衝）
oppOff :: Planet -> Double -> Double
oppOff name jd = wrap180 (fst (geoLonLat name jd) - sunGeoLon jd - 180)

nextOpposition :: Planet -> Double -> Double
nextOpposition name jd = go 0
  where
    go :: Int -> Double
    go k | k >= 400 = jd
         | oppOff name a > 0 && oppOff name b <= 0 = bisect a b (0 :: Int)   -- + から − に変わったら二分法（衝の前後で離角は減っていく）
         | otherwise = go (k + 1)
      where a = jd + fromIntegral k * 5
            b = a + 5
    bisect a b i | i >= 40 = (a + b) / 2
                 | otherwise = let m = (a + b) / 2 in if oppOff name m > 0 then bisect m b (i + 1) else bisect a m (i + 1)

prevOpposition :: Planet -> Double -> Double
prevOpposition name jd = if c < jd - 1 then c else jd
  where syn = 365.25 / (1 - 1 / periodYr name)
        c = nextOpposition name (jd - syn - 30)

-- 円軌道で近似した 2 つの見方（同じ答えになる）
meanLon :: Planet -> Double -> Double
meanLon name jd = let (p0, dp) = elemOf name in (elL p0 + elL dp * (jd - 2451545) / 36525) * degR

-- 地動説：惑星の位置 − 地球の位置
lonHelioCircle :: Planet -> Double -> Double
lonHelioCircle name jd = wrap360 (atan2 (a * sin l - sin le) (a * cos l - cos le) / degR)
  where a = elA (fst (elemOf name))
        l = meanLon name jd
        le = meanLon Earth jd

-- 天動説：導円（半径 a、惑星の速さ）＋ 周転円（半径 1、太陽の向きと平行）
lonEpicycle :: Planet -> Double -> Double
lonEpicycle name jd = wrap360 (atan2 y x / degR)
  where a = elA (fst (elemOf name))
        l = meanLon name jd
        s = meanLon Earth jd + pi
        x = a * cos l + cos s
        y = a * sin l + sin s

makeTrack :: Int -> Double -> V.Vector TP
makeTrack pIdx jdOpp = V.imap retro raw
  where
    (name, span') = plOf pIdx
    raw = V.fromList [ TP d j lam bet (lonEpicycle name j) (xy (planetPos Earth j)) (xy (planetPos name j)) False
                     | d <- [-span', -span' + 2 .. span'], let j = jdOpp + fromIntegral d, let (lam, bet) = geoLonLat name j ]
    xy (x, y, _) = (x, y)
    n = V.length raw
    retro i t = let a = raw V.! max 0 (i - 1)
                    b = raw V.! min (n - 1) (i + 1)
                in t { tRetro = wrap180 (tLam b - tLam a) < 0 }

-- 留（逆行の始まりと終わり）の日付
stations :: V.Vector TP -> [Double]
stations tr = [ tJ b | (a, b) <- zip (V.toList tr) (drop 1 (V.toList tr)), tRetro a /= tRetro b ]

-- ---- 描く
draw :: Env -> World -> Picture
draw env w
  | V.null tr = Blank
  | otherwise = view env $
  -- ---- 太陽中心の図（視線を外の「星空の円」までのばす）
  [ Color (rgb255 60 70 110) (at (cx, cy) (Circle (rr * 1.12)))
  , Color (rgb255 255 210 90) (circleD cx cy 14) ]
  ++ [ sightLine q | (i, q) <- zip [0 :: Int ..] (V.toList tr), i `mod` 10 == 0 ]
  ++ [ Color (rgb255 90 150 240) (polyline 2 [ toScr (tE q) | q <- V.toList tr ])
     , Color (rgb255 230 120 90) (polyline 2 [ toScr (tP q) | q <- V.toList tr ])
     , Color (rgb255 90 150 240) (uncurry circleD (toScr (tE cur)) 14)
     , Color (rgb255 230 120 90) (uncurry circleD (toScr (tP cur)) 14)
     , Color (rgb255 255 230 160) (Line [toScr (tE cur), toScr (tP cur)])
     , say "Sun-centred, seen from above. Red sight lines = retrograde" (cx - rr * 1.12) (cy + rr * 1.12 + 30) 14 (rgb255 200 205 225) ]
  -- ---- 星空の上の動き（λ は左ほど大きい：南の空を見たときの東）
  ++ [ Color (rgb255 70 80 120) (frameTL 1 gx gy gw gh) ]
  ++ segs (\q -> (xL (tLam q), yB (tBet q)))
  ++ [ Color white (circleD (xL (tLam cur)) (yB (tBet cur)) 12)
     , say (show name ++ " on the sky (longitude: <- east  west ->, latitude x " ++ fmt 0 (realToFrac (gh / 2) / bm) ++ ")") gx (gy - 8) 14 (rgb255 200 205 225) ]
  ++ [ say (take 7 (dateStr (tJ q))) (xL (tLam q) - 20) (yB (tBet q) + (if i `mod` 60 /= 0 then 18 else -10)) 14 (rgb255 160 170 200)
     | (i, q) <- zip [0 :: Int ..] (V.toList tr), i `mod` 30 == 0 ]
  -- ---- λ と時間（逆行は赤）。P で周転円の近似を重ねる
  ++ [ Color (rgb255 70 80 120) (frameTL 1 gx gy2 gw gh2) ]
  ++ segs (\q -> (xD (tD q), yL (tLam q)))
  ++ [ Color (rgba255 140 220 200 200) (polyline 1.5 [ (xD (tD q), yL (tEpi q)) | q <- V.toList tr ]) | wEpi w ]
  ++ [ Color (rgba255 255 255 255 120) (Line [(xD (tD cur), gy2), (xD (tD cur), gy2 + gh2)])
     , say ("Longitude seen from Earth (up) vs date (across, centre = opposition " ++ dateStr (wJdOpp w) ++ ")") gx (gy2 - 26) 14 (rgb255 200 205 225) ]
  ++ [ say "Green = epicycle model (circular orbits)" gx (gy2 - 8) 14 (rgb255 140 220 200) | wEpi w ]
  ++ (case stations tr of
        [s1, s2] -> [ say ("Retrograde: " ++ dateStr s1 ++ " to " ++ dateStr s2 ++ " (" ++ fmt 0 (s2 - s1) ++ " days)   synodic period 1/(1 - 1/T) = "
                           ++ fmt 1 (365.25 / (1 - 1 / periodYr name)) ++ " days") gx (gy2 + gh2 + 24) 14 (rgb255 255 150 130) ]
        _ -> [])
  ++ [ drawEpicycle (gx + 60) (gy2 + 60) 42 name (tJ cur) | wEpi w ]
  ++ [ say "Retrograde | Left/Right prev/next opposition  1 Mars 2 Jupiter 3 Saturn  Space play  P epicycle  T today" 20 34 18 (rgb255 240 236 215)
     , say (dateStr (tJ cur) ++ "   " ++ show name ++ ": longitude = " ++ fmt 2 (tLam cur) ++ "   latitude = " ++ fmt 2 (tBet cur) ++ "   "
            ++ (if tRetro cur then "retrograde (moving west)" else "prograde (moving east)")) 20 60 16 (rgb255 250 225 140)
     , drawSelfTest env (wTest w) ]
  where
    tr = wTrack w
    (name, span') = plOf (wPi w)
    n = V.length tr
    cur = tr V.! max 0 (min (n - 1) (floor (wT0 w)))
    lam0 = tLam (tr V.! (n `div` 2))
    cx = envW env * 0.22
    cy = envH env * 0.53
    rr = min (envW env * 0.17) (envH env * 0.34)
    aP = elA (fst (elemOf name))
    sc = realToFrac (rr * 0.8) / max 1.7 (aP * 1.1) :: Double
    toScr (x, y) = (cx + f2 (x * sc), cy - f2 (y * sc))
    -- 地球から惑星の向きへのばした線が、外の円（星空）と交わる点まで：|e + k·d| = Rc を k について解く
    sightLine q =
      let (ex0, ey0) = tE q
          (px, py) = tP q
          dx = px - ex0
          dy = py - ey0
          l = max 1e-12 (sqrt (dx * dx + dy * dy))
          rc = realToFrac (rr * 1.12) / sc
          ux = dx / l
          uy = dy / l
          bq = ex0 * ux + ey0 * uy
          k = -bq + sqrt (max 0 (bq * bq - (ex0 * ex0 + ey0 * ey0 - rc * rc)))
          (sx, sy) = toScr (ex0, ey0)
      in Color (if tRetro q then rgba255 255 120 100 150 else rgba255 150 170 230 70)
               (Line [(sx, sy), (sx + f2 (ux * k * sc), sy - f2 (uy * k * sc))])
    gx = envW env * 0.47
    gw = envW env - gx - 30
    gy = 110
    gh = envH env * 0.3
    lams = [ wrap180 (tLam q - lam0) | q <- V.toList tr ]
    wd = max 10 (foldl' max 0 (map abs lams) * 1.1)
    bm = max 1 (foldl' max 0 (map (abs . tBet) (V.toList tr)) * 1.3)
    xL l = gx + gw / 2 - f2 (wrap180 (l - lam0) / wd) * (gw / 2)
    yB b = gy + gh / 2 - f2 (b / bm) * (gh / 2)
    gy2 = gy + gh + 60
    gh2 = envH env - gy2 - 70
    yL l = gy2 + gh2 / 2 - f2 (wrap180 (l - lam0) / wd) * (gh2 / 2)
    xD d = gx + f2 (fromIntegral (d + span') / fromIntegral (2 * span')) * gw
    segs f = [ Color (if tRetro b then rgb255 255 120 100 else rgb255 230 210 160) (thickLine 2.5 (f a) (f b))
             | (a, b) <- zip (V.toList tr) (drop 1 (V.toList tr)) ]

-- 周転円の小さな図：地球が中心、導円の上を回る点のまわりを惑星が回る
drawEpicycle :: Float -> Float -> Float -> Planet -> Double -> Picture
drawEpicycle x y r name j = Pictures
  [ Color (rgba255 140 220 200 120) (at (x, y) (Circle (f2 a * s)))
  , Color (rgba255 140 220 200 120) (at (ex, ey) (Circle s))
  , Color (rgb255 140 220 200) (Line [(x, y), (ex, ey)])
  , Color (rgb255 140 220 200) (Line [(ex, ey), (px, py)])
  , Color (rgb255 90 150 240) (circleD x y 8)
  , Color (rgb255 230 120 90) (circleD px py 8)
  , say "Epicycle (centre = Earth)" (x + r + 8) y 14 (rgb255 140 220 200) ]
  where
    a = elA (fst (elemOf name))
    s = r / f2 (a + 1)
    l = f2 (meanLon name j)
    sa = f2 (meanLon Earth j + pi)
    ex = x + f2 a * s * cos l
    ey = y - f2 a * s * sin l
    px = ex + s * cos sa
    py = ey - s * sin sa

-- ---- キー
event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (Char c) Down _ _ | c `elem` "pP" -> pure w { wEpi = not (wEpi w) }
  EventKey k Down _ _ | isKey k -> do
    w' <- key k
    let o = clampv (2378496.5 + 400) (2470171.5 - 400) (wJdOpp w')
    pure w' { wJdOpp = o, wTrack = makeTrack (wPi w') o, wT0 = 0 }
  _ -> pure w
  where
    isKey k = case k of { MouseButton _ -> False; _ -> True }
    name = fst (plOf (wPi w))
    key k = case k of
      SpecialKey KeyRight -> pure w { wJdOpp = nextOpposition name (wJdOpp w + 30) }
      SpecialKey KeyLeft -> pure w { wJdOpp = prevOpposition name (wJdOpp w) }
      Char c
        | c >= '1' && c <= '3' -> do now <- jdNowUT
                                     let i = fromEnum c - fromEnum '1'
                                     pure w { wPi = i, wJdOpp = nextOpposition (fst (plOf i)) (now - 400) }
        | c `elem` "tT" -> do now <- jdNowUT
                              pure w { wJdOpp = nextOpposition name (now - 400) }
      _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Mars synodic period = 1 / (1 - 1/1.881) yr ~ 780 days", near (365.25 / (1 - 1 / periodYr Mars)) 779.9 1.5)
  , ("Mars opposition of 2020 is on October 13 (+-1 day)", abs (o - jdFromDate 2020 10 13.5 0) < 1.2)
  , ("next oppositions 2022-12-08 and 2025-01-16 (+-2 days)", abs (o2 - jdFromDate 2022 12 8 0) < 2 && abs (o3 - jdFromDate 2025 1 16 0) < 2)
  , ("2020 retrograde from Sep 9-10 to Nov 13-14, opposition in between"
    , case s of
        [a, b] -> a > jdFromDate 2020 9 7 0 && a < jdFromDate 2020 9 12 0 && b > jdFromDate 2020 11 11 0 && b < jdFromDate 2020 11 16 0 && a < o && o < b
        _ -> False)
  , ("Jupiter's retrograde lasts about 4 months (110 to 130 days)"
    , case sj of
        [a, b] -> b - a > 110 && b - a < 130
        _ -> False)
  , ("epicycle (geocentric) and circular heliocentric point the same way (< 1e-9 deg)", worst < 1e-9) ]
  where
    o = nextOpposition Mars (jdFromDate 2020 9 1 0)
    o2 = nextOpposition Mars (o + 30)
    o3 = nextOpposition Mars (o2 + 30)
    s = stations (makeTrack 0 o)
    sj = stations (makeTrack 1 (nextOpposition Jupiter (jdFromDate 2023 6 1 0)))
    worst = foldl' max 0 [ abs (wrap180 (lonEpicycle Mars j - lonHelioCircle Mars j)) | k <- [0 .. 199 :: Int], let j = 2451545 + fromIntegral k * 13.7 ]

-- ---------- やってみよう ----------
-- 1. → で次の衝へ。逆行の期間と、逆行の「ループ」の形は毎回同じ？
-- 2. 2 の木星、3 の土星にすると、逆行の幅と期間はどう変わる？（遠いほど小さく、長い）
-- 3. 左の図の赤い視線が交差するところ = 地球が惑星を追いこしているところ
-- 4. P の周転円（緑）と本当の動き（黄・赤）のずれは、どこで大きい？（楕円軌道の効果）
-- 5. pls に (Uranus, 200) を足して、もっと遠い惑星の逆行を見よう（キー 4 も足す）


-- ==================================================
-- ここから下は「天文の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- 度 → ラジアン
degR :: Double
degR = pi / 180

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- 0〜360 にそろえる（JavaScript の ((a % 360) + 360) % 360 と同じ）
wrap360 :: Double -> Double
wrap360 a = jsRem (jsRem a 360 + 360) 360

wrap180 :: Double -> Double
wrap180 a = wrap360 (a + 180) - 180

-- JavaScript の % （割られる数と同じ符号の余り）
jsRem :: Double -> Double -> Double
jsRem x y = x - y * fromIntegral (truncate (x / y) :: Integer)

-- ---- グラフの道具：枠 (x, y, w, h) と値の範囲。対数の軸も
data Graph = Graph { gx, gy, gw, gh :: !Float, gx0, gx1, gy0, gy1 :: !Double, glogx, glogy :: !Bool }

gX :: Graph -> Double -> Float
gX g v = gx g + f2 (clampv (-0.02) 1.02 (fin 0 t)) * gw g
  where t = if glogx g then (log v - log (gx0 g)) / (log (gx1 g) - log (gx0 g)) else (v - gx0 g) / (gx1 g - gx0 g)

gY :: Graph -> Double -> Float
gY g v = gy g + gh g - f2 (clampv (-0.02) 1.02 (fin 0 t)) * gh g
  where t = if glogy g then (log v - log (gy0 g)) / (log (gy1 g) - log (gy0 g)) else (v - gy0 g) / (gy1 g - gy0 g)

-- 枠・目盛り・軸の名前
plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgb255 80 90 120) (frameTL 1 (gx g) (gy g) (gw g) (gh g)) ]
  ++ [ sayC (prec3 v) (gX g v) (gy g + gh g + 18) 14 (rgb255 160 170 195) | v <- xt ]
  ++ [ sayR (prec3 v) (gx g - 5) (gY g v + 6) 14 (rgb255 160 170 195) | v <- yt ]
  ++ [ sayR xl (gx g + gw g) (gy g + gh g + 38) 14 (rgb255 205 210 225)
     , say yl (gx g) (gy g - 8) 14 (rgb255 205 210 225) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ w）
polyline :: Float -> [Point] -> Picture
polyline w pts = if length pts < 2 then Blank else if w <= 1 then Line pts else thickPath w pts

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

-- 左上が (x, y) の長方形の枠（太さ wt。1 以下なら細い線）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h
  | wt <= 1 = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
  | otherwise = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 円（直径 d、p5.js の circle と同じ）
circleD :: Float -> Float -> Float -> Picture
circleD x y d = at (x, y) (disc (d / 2))

-- ---- 数の道具
-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 3 桁で、余分な 0 を書かない（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v
  | v == 0 || isNaN v || isInfinite v = "0"
  | e > 2 = show (round (v / 10 ^^ (e - 2)) * 10 ^ (e - 2) :: Integer)
  | otherwise = strip (showFFloat (Just (2 - e)) v "")
  where
    e = floor (logBase 10 (abs v) + 1e-12) :: Int
    strip s = if '.' `elem` s then let t = reverse (dropWhile (== '0') (reverse s)) in if last' t == '.' then init t else t else s
    last' t = if null t then ' ' else last t

-- JavaScript の数の文字列（整数なら小数点なし）
showJ :: Double -> String
showJ v | isNaN v || isInfinite v = "0"
        | v == fromIntegral (round v :: Integer) && abs v < 1e15 = show (round v :: Integer)
        | otherwise = show v

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

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

-- 画面の左下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 12) 14 (if all snd rs then rgb255 130 215 160 else rgb255 255 95 95)

-- ---- 日付 ⇔ ユリウス日 JD（グレゴリオ暦、時刻は世界時 UT。2000-01-01 12:00 UT = 2451545.0）
jdFromDate :: Double -> Double -> Double -> Double -> Double
jdFromDate y0 m0 d h = ffloor (365.25 * (y + 4716)) + ffloor (30.6001 * (m + 1)) + d + b - 1524.5 + h / 24
  where
    (y, m) = if m0 <= 2 then (y0 - 1, m0 + 12) else (y0, m0)
    a = ffloor (y / 100)
    b = 2 - a + ffloor (a / 4)

-- JD → (年, 月, 日, 時)
dateFromJd :: Double -> (Int, Int, Int, Double)
dateFromJd jd = (round yy, round mm, round dd, f * 24)
  where
    z = ffloor (jd + 0.5)
    f = jd + 0.5 - z
    al = ffloor ((z - 1867216.25) / 36524.25)
    a = z + 1 + al - ffloor (al / 4)
    b = a + 1524
    c = ffloor ((b - 122.1) / 365.25)
    dd0 = ffloor (365.25 * c)
    e = ffloor ((b - dd0) / 30.6001)
    dd = b - dd0 - ffloor (30.6001 * e)
    mm = if e < 14 then e - 1 else e - 13
    yy = if mm > 2 then c - 4716 else c - 4715

-- "2024-02-29" の形
dateStr :: Double -> String
dateStr jd = show y ++ "-" ++ pad2 m ++ "-" ++ pad2 d
  where (y, m, d, _) = dateFromJd jd
        pad2 k = (if k < 10 then "0" else "") ++ show k

ffloor :: Double -> Double
ffloor x = fromIntegral (floor x :: Integer)

-- いまの JD（1970-01-01 0:00 UT = 2440587.5）。C の time() で秒を読む
foreign import ccall unsafe "time" c_time :: Ptr CTime -> IO CTime

jdNowUT :: IO Double
jdNowUT = do
  CTime s <- c_time nullPtr
  pure (2440587.5 + fromIntegral s / 86400)

-- ---- 惑星の位置：J2000 の軌道要素（JPL の近似式、1800〜2050 年で誤差は数分角程度）
data Planet = Mercury | Venus | Earth | Mars | Jupiter | Saturn | Uranus | Neptune
  deriving (Eq, Show, Enum, Bounded)

allPlanets :: [Planet]
allPlanets = [minBound .. maxBound]

-- a (AU), e, I (度), L 平均黄経 (度), ϖ 近日点黄経 (度), Ω 昇交点黄経 (度)
data El = El { elA, elE, elI, elL, elW, elO :: !Double }

-- J2000 の値と、それぞれの 1 世紀あたりの変化
elemOf :: Planet -> (El, El)
elemOf p = case p of
  Mercury -> (El 0.38709927 0.20563593 7.00497902 252.25032350 77.45779628 48.33076593, El 0.00000037 0.00001906 (-0.00594749) 149472.67411175 0.16047689 (-0.12534081))
  Venus   -> (El 0.72333566 0.00677672 3.39467605 181.97909950 131.60246718 76.67984255, El 0.00000390 (-0.00004107) (-0.00078890) 58517.81538729 0.00268329 (-0.27769418))
  Earth   -> (El 1.00000261 0.01671123 (-0.00001531) 100.46457166 102.93768193 0.0, El 0.00000562 (-0.00004392) (-0.01294668) 35999.37244981 0.32327364 0.0)
  Mars    -> (El 1.52371034 0.09339410 1.84969142 (-4.55343205) (-23.94362959) 49.55953891, El 0.00001847 0.00007882 (-0.00813131) 19140.30268499 0.44441088 (-0.29257343))
  Jupiter -> (El 5.20288700 0.04838624 1.30439695 34.39644051 14.72847983 100.47390909, El (-0.00011607) (-0.00013253) (-0.00183714) 3034.74612775 0.21252668 0.20469106)
  Saturn  -> (El 9.53667594 0.05386179 2.48599187 49.95424423 92.59887831 113.66242448, El (-0.00125060) (-0.00050991) 0.00193609 1222.49362201 (-0.41897216) (-0.28867794))
  Uranus  -> (El 19.18916464 0.04725744 0.77263783 313.23810451 170.95427630 74.01692503, El (-0.00196176) (-0.00004397) (-0.00242939) 428.48202785 0.40805281 0.04240589)
  Neptune -> (El 30.06992276 0.00859048 1.77004347 (-55.12002969) 44.96476227 131.78422574, El 0.00026291 0.00005105 0.00035372 218.45945325 (-0.32241464) (-0.01262724))

-- ケプラー方程式 E − e sin E = M をニュートン法で解く（M, E はラジアン）
solveKepler :: Double -> Double -> Double
solveKepler m e = go e0 (0 :: Int)
  where
    e0 = if e < 0.8 then m else (if m < 0 then -pi else pi)   -- 最初の値（M は −π〜π）
    go ea i
      | i >= 30 = ea
      | otherwise = let d = (ea - e * sin ea - m) / (1 - e * cos ea)
                        ea' = ea - d
                    in if abs d < 1e-13 then ea' else go ea' (i + 1)

-- 日付 jd の、太陽を中心とした黄道座標 (x, y, z)（AU）
planetPos :: Planet -> Double -> (Double, Double, Double)
planetPos name jd =
  ( (co * cO - so * sO * cI) * xp + (-so * cO - co * sO * cI) * yp
  , (co * sO + so * cO * cI) * xp + (-so * sO + co * cO * cI) * yp
  , so * sI * xp + co * sI * yp )
  where
    (p0, dp) = elemOf name
    t = (jd - 2451545.0) / 36525
    a = elA p0 + elA dp * t
    e = elE p0 + elE dp * t
    i = (elI p0 + elI dp * t) * degR
    l = elL p0 + elL dp * t
    w = elW p0 + elW dp * t
    o = (elO p0 + elO dp * t) * degR
    om = w * degR - o
    m = wrap180 (l - w) * degR
    ea = solveKepler m e
    xp = a * (cos ea - e)
    yp = a * sqrt (1 - e * e) * sin ea
    co = cos om
    so = sin om
    cO = cos o
    sO = sin o
    cI = cos i
    sI = sin i

-- 公転周期 [年] = 360° ÷ 平均黄経の変化（1 年あたり）
periodYr :: Planet -> Double
periodYr name = 36000 / elL (snd (elemOf name))

-- 地球から見た黄経・黄緯 [度]
geoLonLat :: Planet -> Double -> (Double, Double)
geoLonLat name jd = (wrap360 (atan2 y x / degR), atan2 z (sqrt (x * x + y * y)) / degR)
  where
    (px, py, pz) = planetPos name jd
    (qx, qy, qz) = planetPos Earth jd
    x = px - qx
    y = py - qy
    z = pz - qz

sunGeoLon :: Double -> Double
sunGeoLon jd = let (qx, qy, _) = planetPos Earth jd in wrap360 (atan2 (-qy) (-qx) / degR)

hypot3 :: (Double, Double, Double) -> Double
hypot3 (x, y, z) = sqrt (x * x + y * y + z * z)
