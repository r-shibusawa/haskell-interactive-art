-- ============================================
-- 48-10 運動残像（滝の錯視）
-- 回るうずまきの真ん中を 20 秒見つめてから、止まった絵を見ると、逆向きに回って（ふくらんで・縮んで）見える
-- 同じ向きの動きを見続けると、その向きを感じる細胞が「疲れて」、止まっている所で反対向きが勝つ
-- 操作：スペース = スタート（順応 → テスト）/ ↑↓ = 見つめる時間 / ←→ = 回る速さ / D = 向き / 1・2 = テストの絵
--   T = 真実（テストの絵が本当に止まっているか、フレームごとの画素の差を表示）
-- 長く見つめて気分が悪くなったら目を離す
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.IntMap.Strict as IM
import System.Environment (lookupEnv)
import System.Random (mkStdGen, randomR, StdGen)
import Text.Printf (printf)

testSec :: Float
testSec = 12                 -- ★ テストの絵を見せる時間（秒）

arms :: Int
arms = 4                     -- ★ うずまきの腕の数

maxHz :: Float
maxHz = 3                    -- 安全のため、1 点の明暗の切りかわりは 1 秒に 3 回まで（腕 × 速さ ≤ 3）

lightG, darkG :: Int
lightG = 190                 -- ★ コントラストはひかえめに
darkG = 60

data Phase = Ready | Adapt | Test deriving (Eq, Show)

data World = World
  { wAdapt :: !Float           -- ★ 見つめる時間（秒）
  , wSpeed :: !Float           -- ★ 回る速さ（1 秒に何回転）
  , wDir   :: !Float           -- 1 = 時計回り、−1 = 反時計回り
  , wKind  :: !Int             -- テストの絵（1 = うずまき、2 = 水玉）
  , wTruth :: !Bool
  , wStart :: !(Maybe Float)   -- スタートした時刻（秒）
  , wLast  :: !(Maybe Int)     -- 前の確認のときの画素の合計
  , wDiff  :: !Int
  , wTest  :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "48-10 Motion aftereffect" world0)
  { skBack  = g' 150
  , skStart = \env -> do
      let rs = selfTest env world0
      printSelfTest rs
      pure world0 { wTest = rs }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 20 (safeSpeed 0.5) 1 1 False Nothing Nothing 0 []

-- いまの段階：待ち → 見つめる → テスト（止まった絵）→ 待ち
phaseAt :: World -> Float -> Phase
phaseAt w t
  | t < 0 = Ready
  | t < wAdapt w = Adapt
  | t < wAdapt w + testSec = Test
  | otherwise = Ready

-- 安全：腕 × 速さ が 3 回/秒 をこえないようにする
safeSpeed :: Float -> Float
safeSpeed = clampF 0 (maxHz / fromIntegral arms)

elapsed :: Env -> World -> Float
elapsed env w = maybe (-1) (envTime env -) (wStart w)

-- 回転の角度（見つめる段階だけ回す。速さはここでも上限をかける）
rotation :: Env -> World -> Float
rotation env w = let t = elapsed env w in if phaseAt w t == Adapt then wDir w * tau * safeSpeed (wSpeed w) * t else 0

geom :: Env -> (Float, Float, Float)
geom env = (fromIntegral (jsRound (envW env / 2)), fromIntegral (jsRound (envH env / 2 + 20)), fromIntegral (jsRound (min (envW env) (envH env) * 0.36)))

-- ---- 図：明るい円の上に、うずまき（または止まった水玉）と見つめる点
frame :: Env -> World -> Phase -> Float -> [Shape]
frame env w ph rot =
  Dot (g lightG) cx cy r
  : (if ph == Test && wKind w == 2 then texture env else spiral env rot)
  ++ [ Dot (220, 40, 40) cx cy 5 ]
  where (cx, cy, r) = geom env

-- アルキメデスのうずまき r = b θ（腕 arm、回転 rot）。点は (x, y, 半径)
spiralPts :: Env -> Int -> Float -> [(Float, Float, Float)]
spiralPts env arm rot =
  [ (cx + rr * cos a, cy + rr * sin a, rr)
  | i <- [0 .. n]
  , let th = fromIntegral i / fromIntegral n * turns * tau
        rr = th / (turns * tau) * r
        a = th + rot + fromIntegral arm * tau / fromIntegral arms ]
  where
    (cx, cy, r) = geom env
    turns = 3
    n = 240 :: Int

spiral :: Env -> Float -> [Shape]
spiral env rot = concat [ zipWith (\(x1, y1, _) (x2, y2, _) -> RSeg (g darkG) sw (x1, y1) (x2, y2)) ps (drop 1 ps)
                        | k <- [0 .. arms - 1], let ps = spiralPts env k rot ]
  where
    (_, _, r) = geom env
    sw = r / (fromIntegral arms * 3 * 2)       -- 線とすき間が同じ幅（1 本の腕は 3 回転）

-- テストの絵 2：止まった水玉の模様（毎回同じになるよう、乱数の種を固定）
texture :: Env -> [Shape]
texture env = go (260 :: Int) (mkStdGen 48)
  where
    (cx, cy, r) = geom env
    go 0 _ = []
    go k gen0 =
      let (a, gen1) = randomR (0, tau) gen0
          (u, gen2) = randomR (0, 1) gen1
          (d, gen3) = randomR (6, 22) gen2 :: (Float, StdGen)
          rr = r * sqrt u * 0.95
      in Dot (g (darkG + 30)) (cx + rr * cos a) (cy + rr * sin a) (d / 2) : go (k - 1) gen3

-- 画面の画素を間引いて合計（変化の確認用）
sampleSum :: Env -> [Shape] -> Int
sampleSum env sc = sum [ v | y <- [cy - r, cy - r + 9 .. cy + r - 1], x <- [cx - r, cx - r + 9 .. cx + r - 1]
                           , let (v, _, _) = readPixI (g 150) ix (jsRound x) (jsRound y) ]
  where
    (cx, cy, r) = geom env
    ix = indexShapes sc

step :: Env -> Float -> World -> IO World
step env _ w0 = pure w2
  where
    t = elapsed env w0
    w1 = if phaseAt w0 t == Ready && t >= 0 then w0 { wStart = Nothing } else w0   -- 終わったら待ちに戻る
    -- 真実：画面の画素を前の確認と比べる（10 フレームごと）
    w2 | wTruth w1 && envFrame env `mod` 10 == 0 =
           let ph = phaseAt w1 (elapsed env w1)
               s = sampleSum env (frame env w1 ph (rotation env w1))
           in w1 { wDiff = maybe 0 (\l -> abs (s - l)) (wLast w1), wLast = Just s }
       | otherwise = w1

draw :: Env -> World -> Picture
draw env w = view env $
  [ render (frame env w ph (rotation env w))
  , Color (g' 235) (rect (envW env / 2) 43 (envW env) 86)
  , txt 20 26 18 (g' 20) (printf "Space: start   Up/Down: stare time %d s   Left/Right: speed %.2f turns/s   D: %s"
                            (round (wAdapt w) :: Int) (safeSpeed (wSpeed w)) (if wDir w > 0 then "clockwise" else "anticlockwise"))
  , txt 20 50 18 (g' 20) "1/2: test picture   T: truth"
  , txt 20 74 15 (rgb255 160 30 30) (printf "Stare at the red dot only. Look away if you feel unwell. (flicker %.1f / s, max %.0f / s)" (fromIntegral arms * safeSpeed (wSpeed w)) maxHz)
  , txtC (envW env / 2) (envH env - 30) 26 (g' 20) msg ]
  ++ (if ph /= Ready
        then [ Color (g' 120) (rect (envW env / 2) (envH env - 17) 400 6)
             , Color (if ph == Adapt then rgb255 60 120 220 else rgb255 220 120 40) (rect (envW env / 2 - 200 + 200 * f) (envH env - 17) (400 * f) 6) ]
        else [])
  ++ (if wTruth w
        then [ Color (g' 235) (rect (envW env - 160) 135 280 90)
             , txt (envW env - 285) 118 15 (g' 20) ("Pixel difference: " ++ show (wDiff w))
             , txt (envW env - 285) 146 15 (g' 20) (case ph of
                   Test -> if wDiff w == 0 then "-> test picture: no pixel moved" else "-> just switched"
                   Adapt -> "-> it is really turning now"
                   Ready -> "-> waiting (still)") ]
        else [])
  ++ [ txt 20 (envH env - 16) 14 (testColour (wTest w)) (selfTestLine (wTest w)) ]
  where
    t = elapsed env w
    ph = phaseAt w t
    f = clampF 0 1 (t / (wAdapt w + testSec))
    msg = case ph of
      Ready -> "Press Space to start"
      Adapt -> "Stare ... " ++ show (ceiling (wAdapt w - t) :: Int) ++ " s left"
      Test -> "Still picture. Does it turn backwards? (" ++ show (ceiling (wAdapt w + testSec - t) :: Int) ++ ")"

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wStart = Just (envTime env), wLast = Nothing }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wAdapt = min 60 (wAdapt w + 5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wAdapt = max 5 (wAdapt w - 5) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSpeed = safeSpeed (wSpeed w + 0.05) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSpeed = safeSpeed (wSpeed w - 0.05) }
  EventKey (Char c) Down _ _
    | c `elem` "dD" -> w { wDir = negate (wDir w) }
    | c `elem` "12" -> w { wKind = fromEnum c - fromEnum '0' }
    | c `elem` "tT" -> w { wTruth = not (wTruth w), wLast = Nothing }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 見つめる時間を 5 秒と 30 秒にして、残像が続く時間を数える（ストップウォッチで）
-- 2. テストの絵を 2（水玉）にしても逆に動いて見える？ 止まっているのに動いて見える
-- 3. 片目をつぶって見つめ、テストのときは反対の目で見る。残像は移る？（脳のどこで起きているか）
-- 4. D で向きを変えると、残像の向きも変わる？
-- 5. 見つめる点から目をそらして見つめた場合と比べよう

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃えの文字
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y size c s = txt (x - fromIntegral (length s) * size * 0.36) y size c s

-- 灰色（0〜255）
g :: Int -> RGB
g v = (v, v, v)

g' :: Int -> Color
g' v = rgb255 v v v

-- JavaScript の Math.round と同じ（0.5 は上へ）
jsRound :: Float -> Int
jsRound x = floor (x + 0.5)

-- ==================================================
-- ここから下は「錯視の道具」（画素を読む・自己テスト。中身は読まなくても OK。消さないでね）
-- ==================================================
-- ---- 図は「形のリスト」で作る。同じリストから、画面の絵（render）と「画素を読む」関数（readPix）の両方を作る
--   座標は p5.js と同じ（左上が原点、y は下向き）。色は 0〜255 の整数なので、ぴったり同じ値かどうかを比べられる
type RGB = (Int, Int, Int)

data Shape
  = Dot !RGB !Float !Float !Float                 -- 中心 (x, y)・半径 の円
  | RSeg !RGB !Float !Point !Point                -- 太さ w の線分（はしは丸い）

-- 形のリスト → 画面の絵
render :: [Shape] -> Picture
render = Pictures . map one
  where
    one sh = case sh of
      Dot c x y r -> Color (col c) (at (x, y) (disc r))
      RSeg c w p q -> Color (col c) (Pictures [thickLine w p q, at p (disc (w / 2)), at q (disc (w / 2))])
    col (r, gg, b) = rgb255 r gg b

-- 形が点 p をおおっていれば、その色
covers :: Shape -> Point -> Maybe RGB
covers sh p = case sh of
  Dot c x y r -> hit c (dist2 p (x, y) <= r)
  RSeg c w a b -> hit c (segDist a b p <= w / 2)
  where hit c ok = if ok then Just c else Nothing

-- 背景 bg の上に形を順に重ねたときの、点 p の色
colourAt :: RGB -> [Shape] -> Point -> RGB
colourAt bg shs p = foldl' (\d sh -> maybe d id (covers sh p)) bg shs

-- 画素 (x, y) の色（画素の真ん中を読む。p5.js の readPix）
readPix :: RGB -> [Shape] -> Int -> Int -> RGB
readPix bg shs x y = colourAt bg shs (fromIntegral x + 0.5, fromIntegral y + 0.5)

-- 点と線分の距離
segDist :: Point -> Point -> Point -> Float
segDist (ax, ay) (bx, by) (px, py) =
  let dx = bx - ax
      dy = by - ay
      l2 = dx * dx + dy * dy
      t = if l2 < 1e-9 then 0 else clampF 0 1 (((px - ax) * dx + (py - ay) * dy) / l2)
  in dist2 (px, py) (ax + t * dx, ay + t * dy)

-- ---- 索引：画面を 16 px のます目に分け、ます目ごとに「そこにかかる形」を描く順に覚える
--   readPixI は、その点のます目の形だけを調べる（readPix と同じ答えで、ずっと速い）
cellOf :: Int -> Int -> Int
cellOf cx cy = cy * 4096 + cx

indexShapes :: [Shape] -> IM.IntMap [Shape]
indexShapes shs = IM.map reverse (foldl' add IM.empty shs)
  where
    add m sh =
      let (x0, y0, x1, y1) = bbox sh
          cs = [ cellOf i j | j <- [cl y0 .. cl y1], i <- [cl x0 .. cl x1] ]
      in foldl' (\mm c -> IM.insertWith (++) c [sh] mm) m cs
    cl v = max 0 (min 4095 (floor (v / 16)))

-- 形がはいる長方形（左, 上, 右, 下）
bbox :: Shape -> (Float, Float, Float, Float)
bbox sh = case sh of
  Dot _ x y r -> (x - r, y - r, x + r, y + r)
  RSeg _ w (ax, ay) (bx, by) -> (min ax bx - w, min ay by - w, max ax bx + w, max ay by + w)

readPixI :: RGB -> IM.IntMap [Shape] -> Int -> Int -> RGB
readPixI bg ix x y = readPix bg (IM.findWithDefault [] (cellOf (c x) (c y)) ix) x y
  where c v = max 0 (min 4095 (v `div` 16))

-- ---- 自己テスト：タイマー・安全の上限・うずまきの形・テストの絵が止まっていること
selfTest :: Env -> World -> [(String, Bool)]
selfTest env w =
  [ (printf "timer: stare until %.0f s, then %.0f s test, then wait" (wAdapt w) testSec
    , phaseAt w (-1) == Ready && phaseAt w 0 == Adapt && phaseAt w (wAdapt w - 0.01) == Adapt
      && phaseAt w (wAdapt w) == Test && phaseAt w (wAdapt w + testSec + 0.01) == Ready)
  , (printf "safety: for any requested speed, arms x speed <= %.0f per second" maxHz
    , all (\s -> fromIntegral arms * safeSpeed s <= maxHz + 1e-6) [0, 0.3, 1, 5, 100] && safeSpeed (-3) == 0)
  , ("spiral: radius grows outwards, ends at R = " ++ show (round r :: Int)
    , and (zipWith (\(_, _, a) (_, _, b) -> b > a) p (drop 1 p)) && abs (lastR - r) < 1e-4)
  , ("arms are 360 / " ++ show arms ++ " deg apart (turning one gives the next)"
    , and (zipWith (\(x1, y1, _) (x2, y2, _) -> abs (x1 - x2) < 1e-3 && abs (y1 - y2) < 1e-3) q q2))
  , ("test pictures (1, 2): drawn twice, the pixel sum is the same (still)", same)
  , ("gentle contrast: spiral stays between " ++ show darkG ++ " and " ++ show lightG
    , darkG >= 40 && lightG <= 200 && lv >= darkG && lv <= lightG) ]
  where
    (cx, cy, r) = geom env
    p = spiralPts env 0 0
    lastR = case reverse p of
      ((_, _, rr) : _) -> rr
      [] -> 0
    q = spiralPts env 1 0
    q2 = spiralPts env 0 (tau / fromIntegral arms)
    -- 2 回、別々に描いた絵の画素を比べる
    same = and [ sampleSum env (frame env w { wKind = k } Test 0) == sampleSum env (frame env w { wKind = k, wDir = -1 } Test 0) | k <- [1, 2] ]
    (lv, _, _) = readPix (g 150) (frame env w Ready 0) (jsRound (cx + r * 0.98)) (jsRound cy)

-- 結果を端末に出す（テスト中は 1 行のまとめと失敗した項目だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(s, ok) -> putStrLn ((if ok then "OK  " else "NG  ") ++ s)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if not (null rs) && all snd rs then "OK " else "NG ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

testColour :: [(String, Bool)] -> Color
testColour rs = if not (null rs) && all snd rs then rgb255 30 130 60 else rgb255 200 40 40
