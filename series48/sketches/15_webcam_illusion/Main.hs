-- ============================================
-- 48-15 カメラ映像に錯視フィルタ 📷
-- 1 同時対比：映像のいちばん暗い所と明るい所に、まったく同じ灰色の四角を置く → 違う明るさに見える
-- 2 きらめき格子：映像の上に灰色の格子と白い円を重ねる → 円の中に黒い点がちらつく
-- 3 補色残像：色を反転した映像を見つめてから、白黒の映像へ → 白黒なのに色が見える
-- 操作：1〜3 = 切替 / スペース = 3 のスタート / ↑↓ = 3 の見つめる時間・2 の格子の細かさ / C = カメラ ⇔ テスト画像 / S = 保存
--   T またはマウスを押している間 = 真実を見る（2 つの四角を同じ灰色の帯でつなぎ、画素を読む）
--   長く見つめて気分が悪くなったら目を離す
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import System.Environment (lookupEnv)

gV :: Int
gV = 128                     -- ★ 同時対比の四角の灰色

testSec :: Float
testSec = 10                 -- 白黒の映像を見せる時間（秒）

vw, vh :: Int
vw = 160                     -- 映像を処理する大きさ
vh = 120

-- 暗い所・明るい所（画面に対する割合 0〜1）と、そのブロックの明るさの平均
data Spot = Spot { sx, sy, sm :: !Float }

data World = World
  { wWhich  :: !Int
  , wCell   :: !Float          -- ★ きらめき格子のマスの大きさ px
  , wAdapt  :: !Float          -- ★ 見つめる時間（秒）
  , wStart  :: !Float          -- スタートした時刻（秒）。−1 = 待ち
  , wTruth  :: !Bool
  , wUseCam :: !Bool
  , wCam    :: Maybe Camera
  , wLive   :: !Bool           -- 本物のカメラ映像が来ているか
  , wSrc    :: !(U.Vector Int) -- いまの映像（160 × 120、0xRRGGBB）
  , wOut    :: !(U.Vector Int) -- フィルタをかけた映像
  , wOutPic :: Picture
  , wSpots  :: Maybe (Spot, Spot)
  , wFound  :: !(Spot, Spot)
  , wTest   :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "48-15 Illusion filters on the camera" w0)
  { skBack  = g' 0
  , skStart = \env -> do
      cam <- startCamera vw vh
      let rs = selfTest env w0
      printSelfTest rs
      pure w0 { wCam = Just cam, wTest = rs }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }
  where w0 = process (-1) (World 1 90 15 (-1) False True Nothing False testImg U.empty Blank Nothing (findSpots testImg) [])

data Phase = Ready | Adapt | Test deriving (Eq, Show)

phaseAt :: World -> Float -> Phase
phaseAt w t
  | t < 0 = Ready
  | t < wAdapt w = Adapt
  | t < wAdapt w + testSec = Test
  | otherwise = Ready

elapsed :: Env -> World -> Float
elapsed env w = if wStart w < 0 then -1 else envTime env - wStart w

-- テスト画像（カメラがないとき）：明るさのグラデーションと色の図形
testImg :: U.Vector Int
testImg = U.generate (vw * vh) (\i -> let (y, x) = i `divMod` vw in pack (readPix (g 0) shapes x y))
  where
    w = fromIntegral vw
    h = fromIntegral vh
    shapes = [ Box (jsRound (20 + 200 * fromIntegral x / w), jsRound (30 + 120 * fromIntegral x / w), 90) (fromIntegral x) 0 1 h | x <- [0 .. vw - 1] ]
             ++ [ Dot (240, 200, 40) (w * 0.75) (h * 0.35) 20
                , Box (220, 50, 60) (w * 0.15) (h * 0.55) 40 30
                , Poly (60, 180, 90) [(w * 0.45, h * 0.9), (w * 0.6, h * 0.55), (w * 0.75, h * 0.9)] ]

grayOf :: Int -> Int -> Int -> Int
grayOf r gg b = jsRound (0.299 * fromIntegral r + 0.587 * fromIntegral gg + 0.114 * fromIntegral b)

invert :: RGB -> RGB
invert (r, gg, b) = (255 - r, 255 - gg, 255 - b)

-- 映像を 4 × 4 のブロックに分け、いちばん暗い・明るいブロックの中心を探す
findSpots :: U.Vector Int -> (Spot, Spot)
findSpots px =
  let bw = vw `div` 4
      bh = vh `div` 4
      blocks = [ Spot ((fromIntegral bx + 0.5) / 4) ((fromIntegral by + 0.5) / 4) (fromIntegral s / fromIntegral n)
               | by <- [0 .. 3], bx <- [0 .. 3]
               , let vals = [ let (r, gg, b) = unpack (px U.! (y * vw + x)) in grayOf r gg b
                            | y <- [by * bh, by * bh + 2 .. (by + 1) * bh - 1], x <- [bx * bw, bx * bw + 2 .. (bx + 1) * bw - 1] ]
                     s = sum vals
                     n = max 1 (length vals) ]
      pick better = foldl' (\a c -> if better (sm c) (sm a) then c else a)
  in case blocks of
       (b0 : rest) -> (pick (<) b0 rest, pick (>) b0 rest)
       [] -> (Spot 0.25 0.5 0, Spot 0.75 0.5 255)

-- ---- 映像にフィルタをかける（1 = 白黒、2 = 暗く、3 = 補色 → 白黒 → そのまま）
process :: Float -> World -> World
process t w = w { wOut = out, wOutPic = imgPicture vw vh (\x y -> unpack (out U.! (y * vw + x))) }
  where
    ph = phaseAt w t
    out = U.map (pack . filt . unpack) (wSrc w)
    filt c@(r, gg, b) = case wWhich w of
      1 -> let y = grayOf r gg b in (y, y, y)
      2 -> (dk r, dk gg, dk b)
      _ -> case ph of
             Adapt -> invert c
             Test -> let y = grayOf r gg b in (y, y, y)
             Ready -> c
    dk v = jsRound (fromIntegral v * 0.35)

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  live <- maybe (pure False) camLive (wCam w0)
  src <- case wCam w0 of
    Just cam | live && wUseCam w0 -> do
      fr <- cameraFrame cam
      pure (if B.length fr >= vw * vh * 3 then U.generate (vw * vh) (\i -> let k = 3 * i; byte j = fromIntegral (BU.unsafeIndex fr (k + j)) in pack (byte 0, byte 1, byte 2)) else testImg)
    _ -> pure testImg
  let w1 = w0 { wLive = live && wUseCam w0, wSrc = src }
      w2 = if phaseAt w1 (elapsed env w1) == Ready && wStart w1 >= 0 then w1 { wStart = -1 } else w1
      found = findSpots src
      ease a b = a + (b - a) * 0.05
      move (Spot x y _) (Spot x' y' m') = Spot (ease x x') (ease y y') m'
      spots = case wSpots w2 of
        Nothing -> (fst found, snd found)
        Just (lo, hi) -> (move lo (fst found), move hi (snd found))
  pure (process (elapsed env w2) w2 { wFound = found, wSpots = Just spots })

-- 四角の位置（画面の px）
spotPx :: Env -> Spot -> (Int, Int)
spotPx env s = (jsRound (sx s * envW env), jsRound (sy s * envH env))

scene :: Env -> World -> Bool -> [Shape]
scene env w truth =
  [ Img 0 0 (envW env / fromIntegral vw) (envH env / fromIntegral vh) vw vh (\x y -> unpack (wOut w U.! (y * vw + x))) (wOutPic w) ]
  ++ case wWhich w of
       1 -> case wSpots w of
              Nothing -> []
              Just (lo, hi) ->
                let (ax, ay) = spotPx env lo
                    (bx, by) = spotPx env hi
                    fa = fromIntegral ax
                    fb = fromIntegral bx
                    ga = fromIntegral ay
                    gb = fromIntegral by
                in [ Box (g gV) (fa - s / 2) (ga - s / 2) s s, Box (g gV) (fb - s / 2) (gb - s / 2) s s ]
                   ++ (if truth then [ Box (g gV) (min fa fb) (ga - 6) (abs (fb - fa)) 12, Box (g gV) (fb - 6) (min ga gb) 12 (abs (gb - ga)) ] else [])
       2 -> let st = fromIntegral (jsRound (wCell w * 0.2))
                c = wCell w
            in [ Box (g 110) x 0 st (envH env) | x <- [0, c .. envW env - 0.01] ]
               ++ [ Box (g 110) 0 y (envW env) st | y <- [0, c .. envH env - 0.01] ]
               ++ [ Dot (g 255) (x + st / 2) (y + st / 2) (st * 0.625) | x <- [0, c .. envW env + c - 0.01], y <- [0, c .. envH env + c - 0.01] ]
       _ | ph /= Ready -> [ Seg (g 0) 3 (cx - 12, cy) (cx + 12, cy), Seg (g 0) 3 (cx, cy - 12) (cx, cy + 12) ]
         | otherwise -> []
  where
    s = fromIntegral (jsRound (min (envW env) (envH env) * 0.1))
    ph = phaseAt w (elapsed env w)
    cx = envW env / 2
    cy = envH env / 2

draw :: Env -> World -> Picture
draw env w = view env $
  -- 文字の帯は映像の上に重ねる。1 では四角を帯より上に描いて、四角の灰色が帯で変わらないようにする
  [ render (if wWhich w == 1 then take 1 sc ++ [bar] ++ drop 1 sc else sc ++ [bar]) ]
  ++ (if truth && wWhich w == 1 then
        case wSpots w of
          Just (lo, hi) ->
            let (ax, ay) = spotPx env lo
                (bx, by) = spotPx env hi
            in [ txtC (fromIntegral ax) (fromIntegral ay + s / 2 + 20) 15 (rgb255 255 230 60) ("grey " ++ show gV ++ " on dark " ++ show (jsRound (sm (fst (wFound w)))))
               , txtC (fromIntegral bx) (fromIntegral by + s / 2 + 20) 15 (rgb255 255 230 60) ("grey " ++ show gV ++ " on light " ++ show (jsRound (sm (snd (wFound w))))) ]
          Nothing -> []
      else [])
  ++ [ txt 20 28 18 (g' 255) ("1-3: " ++ names ++ "   " ++ ctl ++ "   C: " ++ (if wLive w then "camera" else "test image") ++ "   S: save")
     , txt 20 52 15 (rgb255 255 200 200) msg
     , txt 20 76 15 (rgb255 255 200 200) "Look away if you feel unwell."
     , txt 20 (envH env - 16) 14 (testColour (wTest w)) (selfTestLine (wTest w)) ]
  where
    truth = wTruth w || envDown env
    sc = scene env w truth
    bar = Fade 160 (Box (g 0) 0 0 (envW env) 90)
    s = fromIntegral (jsRound (min (envW env) (envH env) * 0.1))
    t = elapsed env w
    ph = phaseAt w t
    names = case wWhich w of { 1 -> "simultaneous contrast"; 2 -> "scintillating grid"; _ -> "colour afterimage" }
    ctl = case wWhich w of
      3 -> "Space: start   Up/Down: stare " ++ show (round (wAdapt w) :: Int) ++ " s"
      2 -> "Up/Down: cell " ++ show (round (wCell w) :: Int) ++ " px"
      _ -> "T: join with a strip"
    msg = case wWhich w of
      1 -> "Both squares are the same grey " ++ show gV ++ ": lighter over dark, darker over light."
      2 -> "Dark dots flicker inside the white discs (no dots are drawn anywhere)."
      _ -> case ph of
             Adapt -> "Stare at the + ... " ++ show (ceiling (wAdapt w - t) :: Int) ++ " s left"
             Test -> "A grey video. Do you see colours?"
             Ready -> "Press space to start."

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "123" -> pure w { wWhich = fromEnum c - fromEnum '0', wStart = -1 }
    | c `elem` "cC" -> pure w { wUseCam = not (wUseCam w) }
    | c `elem` "tT" -> pure w { wTruth = not (wTruth w) }
    | c `elem` "sS" -> do saveSvg "illusion_camera" env (draw env w)
                          pure w
  EventKey (SpecialKey KeySpace) Down _ _ | wWhich w == 3 -> pure w { wStart = envTime env }
  EventKey (SpecialKey KeyUp) Down _ _
    | wWhich w == 3 -> pure w { wAdapt = min 40 (wAdapt w + 5) }
    | otherwise -> pure w { wCell = min 200 (wCell w + 10) }
  EventKey (SpecialKey KeyDown) Down _ _
    | wWhich w == 3 -> pure w { wAdapt = max 5 (wAdapt w - 5) }
    | otherwise -> pure w { wCell = max 40 (wCell w - 10) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 1 で、暗い服や明るい窓の前で手を動かす。四角は同じ灰色のまま、見え方だけ変わる
-- 2. T で帯をつなぐと、2 つが同じ色だと分かる。でも帯を消すとまた違って見える
-- 3. 2 の格子の大きさ（↑↓）で、ちらつきの強さは変わる？
-- 4. 3 で自分の顔の補色を 15 秒見つめてから白黒へ。顔色はどう見える？
-- 5. gV を 60 や 200 に変えて、どの明るさで一番錯視が強い？

pack :: RGB -> Int
pack (r, gg, b) = r * 65536 + gg * 256 + b

unpack :: Int -> RGB
unpack v = (v `div` 65536, (v `div` 256) `mod` 256, v `mod` 256)

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字
txtC, txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y size c s = txt (x - fromIntegral (length s) * size * 0.36) y size c s
txtR x y size c s = txt (x - fromIntegral (length s) * size * 0.72) y size c s

-- 灰色（0〜255）
g :: Int -> RGB
g v = (v, v, v)

g' :: Int -> Color
g' v = rgb255 v v v

c' :: RGB -> Color
c' (r, gg, b) = rgb255 r gg b

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
  = Box !RGB !Float !Float !Float !Float          -- 左上 (x, y)・幅・高さ の長方形
  | Dot !RGB !Float !Float !Float                 -- 中心 (x, y)・半径 の円
  | Ell !RGB !Float !Float !Float !Float          -- 中心 (x, y)・横の半径・縦の半径 のだ円
  | Pie !RGB !Float !Float !Float !Float !Float   -- 中心 (x, y)・半径・角度 a0 から a1 まで の扇形（p5.js の arc PIE）
  | Seg !RGB !Float !Point !Point                 -- 太さ w の線分（はしは四角）
  | RSeg !RGB !Float !Point !Point                -- 太さ w の線分（はしは丸い）
  | Poly !RGB [Point]                             -- 凸多角形
  | Img !Float !Float !Float !Float !Int !Int (Int -> Int -> RGB) Picture
      -- 画像：左上 (x, y)・横と縦の倍率・幅・高さ（画素）・画素の色・作っておいた絵
  | Fade !Int Shape                               -- 透明度 0〜255 をつける

-- 形のリスト → 画面の絵
render :: [Shape] -> Picture
render = Pictures . map (recolour 255)
  where
    recolour a sh = case sh of
      Box c x y w h -> Color (col a c) (rect (x + w / 2) (y + h / 2) w h)
      Dot c x y r -> Color (col a c) (at (x, y) (disc r))
      Ell c x y rx ry -> Color (col a c) (Translate x y (Scale rx ry (circleSolid 1)))
      Pie c x y r a0 a1 -> Color (col a c) (Pictures [ Polygon ((x, y) : [ (x + r * cos t, y + r * sin t) | t <- arcPts b0 b1 ]) | (b0, b1) <- pieces a0 a1 ])
      Seg c w p q -> Color (col a c) (thickLine w p q)
      RSeg c w p@(px, py) q@(qx, qy) -> Color (col a c) (Pictures [thickLine w p q, at (px, py) (disc (w / 2)), at (qx, qy) (disc (w / 2))])
      Poly c ps -> Color (col a c) (Polygon ps)
      Img x y kx ky iw ih _ pic -> Translate (x + fromIntegral iw * kx / 2) (y + fromIntegral ih * ky / 2) (Scale kx (-ky) pic)
      Fade b s -> recolour (a * b `div` 255) s
    col a (r, gg, b) = rgba255 r gg b a
    -- 扇形は 90° 以下に分けると凸になる（gloss の Polygon は凸の形だけ）
    pieces a0 a1 = let n = max 1 (ceiling ((a1 - a0) / (pi / 2)) :: Int)
                       d = (a1 - a0) / fromIntegral n
                   in [ (a0 + fromIntegral i * d, a0 + fromIntegral (i + 1) * d) | i <- [0 .. n - 1] ]
    arcPts b0 b1 = [ b0 + (b1 - b0) * fromIntegral i / 12 | i <- [0 .. 12 :: Int] ]

-- 画素の絵を作る（左上が (0, 0)、1 画素 = 1）。Img に入れて使う
imgPicture :: Int -> Int -> (Int -> Int -> RGB) -> Picture
imgPicture iw ih f = pixelImage iw ih 1 (\x y -> let (r, gg, b) = f x y in (fromIntegral r, fromIntegral gg, fromIntegral b))

-- 形が点 p をおおっていれば (色, 透明度)
covers :: Shape -> Point -> Maybe (RGB, Int)
covers sh p@(px, py) = case sh of
  Box c x y w h -> hit c (px >= x && px < x + w && py >= y && py < y + h)
  Dot c x y r -> hit c (dist2 p (x, y) <= r)
  Ell c x y rx ry -> hit c (((px - x) / rx) ^ (2 :: Int) + ((py - y) / ry) ^ (2 :: Int) <= 1)
  Pie c x y r a0 a1 -> hit c (dist2 p (x, y) <= r && wrapF (2 * pi) (atan2 (py - y) (px - x) - a0) <= a1 - a0)
  Seg c w a b -> hit c (segDist True a b p <= w / 2)
  RSeg c w a b -> hit c (segDist False a b p <= w / 2)
  Poly c ps -> hit c (inside ps p)
  Img x y kx ky iw ih f _ ->
    let ix = floor ((px - x) / kx)
        iy = floor ((py - y) / ky)
    in if ix >= 0 && iy >= 0 && ix < iw && iy < ih then Just (f ix iy, 255) else Nothing
  Fade a s -> fmap (\(c, b) -> (c, a * b `div` 255)) (covers s p)
  where hit c ok = if ok then Just (c, 255) else Nothing

-- 背景 bg の上に形を順に重ねたときの、点 p の色
colourAt :: RGB -> [Shape] -> Point -> RGB
colourAt bg shs p = foldl' over bg shs
  where
    over d sh = case covers sh p of
      Nothing -> d
      Just (c, 255) -> c
      Just (c, a) -> blend a c d
    blend a (r, gg, b) (r', g2, b') = (mix r r', mix gg g2, mix b b')
      where mix s t = (s * a + t * (255 - a) + 127) `div` 255

-- 画素 (x, y) の色（画素の真ん中を読む。p5.js の readPix）
readPix :: RGB -> [Shape] -> Int -> Int -> RGB
readPix bg shs x y = colourAt bg shs (fromIntegral x + 0.5, fromIntegral y + 0.5)

-- 点と線分の距離（butt = True なら はしの外は遠いとみなす）
segDist :: Bool -> Point -> Point -> Point -> Float
segDist butt (ax, ay) (bx, by) (px, py) =
  let dx = bx - ax
      dy = by - ay
      l2 = dx * dx + dy * dy
      t0 = if l2 < 1e-9 then 0 else ((px - ax) * dx + (py - ay) * dy) / l2
      t = clampF 0 1 t0
  in if butt && (t0 < 0 || t0 > 1) then 1e9 else dist2 (px, py) (ax + t * dx, ay + t * dy)

-- 多角形の内側か（偶奇の規則）
inside :: [Point] -> Point -> Bool
inside ps (x, y) = odd (length [() | ((x1, y1), (x2, y2)) <- zip ps (drop 1 ps ++ take 1 ps)
                                   , (y1 > y) /= (y2 > y), x < x1 + (y - y1) * (x2 - x1) / (y2 - y1)])

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
testColour rs = if not (null rs) && all snd rs then rgb255 60 170 90 else rgb255 220 60 60

-- ---- 自己テスト：テスト画像で描いて、2 つの四角が同じ灰色か画素で確かめる。計算の道具も確かめる
selfTest :: Env -> World -> [(String, Bool)]
selfTest env w0 =
  [ ("contrast: square over dark " ++ showRGB a ++ " = square over light " ++ showRGB b ++ " = " ++ show gV, a == b && a == g gV)
  , ("found a dark block (" ++ show (jsRound (sm lo)) ++ ") and a light block (" ++ show (jsRound (sm hi)) ++ ")", sm lo < sm hi - 50)
  , ("complement: (30, 200, 90) -> (225, 55, 165), twice gives it back", invert (30, 200, 90) == (225, 55, 165) && invert (invert (30, 200, 90)) == (30, 200, 90))
  , ("grey: white 255, black 0, R = G = B", grayOf 255 255 255 == 255 && grayOf 0 0 0 == 0)
  , ("afterimage 'ready' shows the original colours", let (r, gg, bb) = c in r /= gg || gg /= bb)
  , ("timer: stare -> grey -> ready", phaseAt w0 (-1) == Ready && phaseAt w0 1 == Adapt && phaseAt w0 (wAdapt w0 + 1) == Test && phaseAt w0 (wAdapt w0 + testSec + 1) == Ready) ]
  where
    (lo, hi) = findSpots testImg
    w1 = process (-1) w0 { wSrc = testImg, wWhich = 1, wSpots = Just (lo, hi) }
    sh1 = scene env w1 False
    rp shs (x, y) = readPix (g 0) shs x y
    a = rp sh1 (spotPx env lo)
    b = rp sh1 (spotPx env hi)
    w3 = process (-1) w0 { wSrc = testImg, wWhich = 3, wStart = -1 }
    c = rp (scene env w3 False) (jsRound (envW env / 4), jsRound (envH env / 2))

showRGB :: RGB -> String
showRGB (r, gg, b) = "(" ++ show r ++ "," ++ show gg ++ "," ++ show b ++ ")"
