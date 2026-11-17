-- ============================================
-- 48-14 音の錯覚 ─ 無限に上がり続けるシェパード音 🔊
-- 1 オクターブずつ離れた音（倍音）を同時に鳴らし、音量を「真ん中の高さで大きく、低い・高い所で小さく」（ガウス窓）する
-- 全部を少しずつ上げていくと、上で消える音と下から現れる音が入れかわり、いつまでも上がり続けて聞こえる
-- 操作：クリック = 音を出す / スペース = 再生・停止 / 1 = 階段（ドレミ…）/ 2 = なめらか / ↑↓ = 速さ / ←→ = 窓の幅 σ
--   D = 上がる ⇔ 下がる / O = 1 本だけ（錯覚をやめる）/ T = 真実（1 オクターブ後の音の構成を重ねる）/ S = 画像を保存
-- この版の音は「短い音の連なり」：1 段ごとに、倍音を全部まとめた和音を 1 回鳴らす（階段のシェパード音階）
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.List (foldl', sort, sortOn)
import System.Environment (lookupEnv)
import Text.Printf (printf)

np :: Int
np = 9                       -- ★ 倍音（1 オクターブずつ）の数

fmin :: Double
fmin = 27.5                  -- ★ いちばん低い音の周波数 Hz

volume :: Float
volume = 0.45                -- ★ 音の大きさ（倍音の音量の合計）

data World = World
  { wSigma   :: !Double        -- ★ ガウス窓の幅（オクターブ）
  , wRate    :: !Double        -- ★ なめらか：1 秒に何オクターブ上がるか
  , wNoteSec :: !Double        -- ★ 階段：1 音の長さ（秒）
  , wMode    :: !Int           -- 1 = 階段、2 = なめらか
  , wDir     :: !Double        -- +1 = 上がる、−1 = 下がる
  , wPlaying :: !Bool
  , wOnlyOne :: !Bool
  , wTruth   :: !Bool
  , wPos     :: !Double        -- 上がった量（オクターブ、0〜np でくり返す）
  , wTimer   :: !Double        -- 次の段・次の音までの時間をはかる
  , wSteps   :: !Int
  , wAudio   :: !Bool          -- クリックかキーで音を出しはじめる
  , wTest    :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "48-14 Shepard tone" w0)
  { skBack  = rgb255 18 20 30
  , skStart = \_ -> do
      let rs = selfTest w0
      printSelfTest rs
      pure w0 { wTest = rs }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }
  where w0 = World 1.3 0.08 0.45 1 1 True False False 0 0 0 False []

-- ---- シェパード音の設計図：u オクターブ上がったときの倍音の (周波数, 音量)（音量の合計は 1）
-- k 番目の音は (k + u) を np で割った余りの高さ。いちばん上を超えた音は、いちばん下（音量ほぼ 0）へ戻る
partials :: World -> Double -> [(Double, Double)]
partials w u
  | wOnlyOne w = [ (fr, if k == np `div` 2 then 1 else 0) | (k, (fr, _)) <- zip [0 ..] norm ]
  | otherwise = norm
  where
    mid = log2 fmin + fromIntegral np / 2
    raw = [ (2 ** lf, exp (-((lf - mid) ^ (2 :: Int)) / (2 * wSigma w * wSigma w)))
          | k <- [0 .. np - 1], let lf = log2 fmin + modN (fromIntegral k + u) (fromIntegral np) ]
    s = sum (map snd raw)
    norm = [ (fr, v / s) | (fr, v) <- raw ]

modN :: Double -> Double -> Double
modN x n = x - n * fromIntegral (floor (x / n) :: Int)

-- 1 オクターブの中のどこか（0〜1）
phaseOf :: Double -> Double
phaseOf u = modN u 1

log2 :: Double -> Double
log2 x = log x / log 2

-- 階段を 1 段（半音 = 1/12 オクターブ）上がる・下がる
stepPos :: Double -> Double -> Double
stepPos dir u = modN (fromIntegral (jsRoundD ((u + dir / 12) * 12)) / 12) (fromIntegral np)

step :: Env -> Float -> World -> IO World
step _ dt0 w
  | not (wPlaying w) = pure w
  | wMode w == 2 = do
      -- なめらか：位置は毎フレーム進め、音は 0.25 秒ごとに鳴らす（音の高さは 1/24 オクターブ単位にそろえる）
      let w1 = w { wPos = modN (wPos w + wDir w * wRate w * dt) (fromIntegral np), wTimer = wTimer w + dt }
      if wTimer w1 >= 0.25
        then do chord w1 (fromIntegral (jsRoundD (wPos w1 * 24)) / 24) 0.25
                pure w1 { wTimer = 0 }
        else pure w1
  | otherwise = do
      let t = wTimer w + dt
      if t >= wNoteSec w
        then do let w1 = w { wTimer = 0, wSteps = wSteps w + 1, wPos = stepPos (wDir w) (wPos w) }
                chord w1 (wPos w1) (wNoteSec w1)
                pure w1
        else pure w { wTimer = t }
  where dt = min 0.05 (realToFrac dt0)

-- 1 つの和音：倍音を、ガウス窓の音量のまま同時に鳴らす（ほとんど聞こえない倍音は省き、多くて 6 音）
chord :: World -> Double -> Double -> IO ()
chord w u dur = when (wAudio w) $
  forM_ (take 6 (sortOn (negate . snd) [ p | p@(fr, v) <- partials w u, v >= 0.01, fr <= 20000 ])) $ \(fr, v) ->
    playTone (realToFrac fr) (volume * realToFrac v) (realToFrac dur * 0.9) Sine

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (rgb255 90 110 160) (thickPath 2 [ (xOf (2 ** lf), yOf (exp (-((lf - mid) ^ (2 :: Int)) / (2 * wSigma w * wSigma w)) / s0))
                                           | i <- [0 .. 199 :: Int], let lf = log2 20 + (log2 20000 - log2 20) * fromIntegral i / 199 ]) ]
  -- 倍音の棒
  ++ concat [ [ Color (rgb255 255 200 80) (thickLine 6 (xOf fr, y1) (xOf fr, yOf v))
              , txtC (xOf fr) (y1 + 20) 14 (g' 200) (show (jsRoundD fr)) ] | (fr, v) <- ps ]
  ++ (if wTruth w then   -- 1 オクターブ後（位相 + 1）の構成：まったく同じ所に重なる
        [ Color (rgb255 80 200 255) (at (xOf fr, yOf v) (ring 8 2)) | (fr, v) <- partials w (wPos w + 1) ]
        ++ [ txt x0 (y0 - 20) 15 (rgb255 80 200 255) "Blue rings = the mix one octave later -> exactly on the bar tops (the very same sound again)" ]
      else [])
  -- 目盛り
  ++ [ Color (g' 70) (Line [(x0, y1), (x1, y1)]) ]
  ++ [ txtC (xOf fr) (y1 + 44) 14 (g' 140) (if fr >= 1000 then show (round (fr / 1000) :: Int) ++ "k" else show (round fr :: Int))
     | fr <- [20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000 :: Double] ]
  ++ [ txtC ((x0 + x1) / 2) (y1 + 66) 14 (g' 140) "frequency Hz (log scale)" ]
  -- 音の時計（位相）
  ++ [ Color (g' 80) (at (cx, cy) (ring r 2))
     , Color (rgb255 255 200 80) (thickLine 4 (cx, cy) (cx + r * cos a, cy + r * sin a))
     , txtC cx (cy + r + 24) 14 (g' 200) ("1 turn = 1 octave (" ++ (if wMode w == 1 then "12 steps" else "smooth") ++ "). It always comes back.") ]
  ++ [ txt 20 30 18 (g' 235) ("Space: " ++ (if wPlaying w then "stop" else "play") ++ "   1/2: " ++ (if wMode w == 1 then "steps" else "smooth")
                              ++ "   Up/Down: speed   Left/Right: width sigma = " ++ printf "%.1f" (wSigma w))
     , txt 20 54 18 (g' 235) ("D: " ++ (if wDir w > 0 then "rising" else "falling") ++ "   O: one tone only " ++ (if wOnlyOne w then "ON" else "off") ++ "   T: truth   S: save")
     , txt 20 78 15 (g' 170) (if wAudio w then "Sound is on (quiet). " ++ (if wMode w == 1 then "Steps climbed: " ++ show (wSteps w) else printf "phase %.3f" (phaseOf (wPos w)))
                              else "Click (or press a key) to start the sound")
     , txt 20 (envH env - 16) 14 (testColour (wTest w)) (selfTestLine (wTest w)) ]
  where
    ps = partials w (wPos w)
    x0 = 60
    x1 = envW env - 60
    y0 = 140
    y1 = envH env * 0.62
    xOf fr = x0 + realToFrac ((log2 fr - log2 20) / (log2 20000 - log2 20)) * (x1 - x0)
    yOf v = y1 - realToFrac v * (y1 - y0) * 2.2
    mid = log2 fmin + fromIntegral np / 2
    s0 = sum [ exp (-((log2 fr - mid) ^ (2 :: Int)) / (2 * wSigma w * wSigma w)) | (fr, _) <- partials w 0 ]
    cx = envW env / 2
    cy = envH env * 0.82
    r = min 80 (envH env * 0.1)
    a = -pi / 2 + realToFrac (phaseOf (wPos w)) * tau

event :: Env -> Event -> World -> IO World
event env ev w0 = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wRate = min 0.5 (wRate w * 1.25), wNoteSec = max 0.12 (wNoteSec w / 1.25) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wRate = max 0.01 (wRate w / 1.25), wNoteSec = min 2 (wNoteSec w * 1.25) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wSigma = min 4 (wSigma w + 0.1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wSigma = max 0.4 (wSigma w - 0.1) }
  EventKey (Char c) Down _ _
    | c `elem` "12" -> pure w { wMode = fromEnum c - fromEnum '0' }
    | c `elem` "dD" -> pure w { wDir = negate (wDir w) }
    | c `elem` "oO" -> pure w { wOnlyOne = not (wOnlyOne w) }
    | c `elem` "tT" -> pure w { wTruth = not (wTruth w) }
    | c `elem` "sS" -> do saveSvg "shepard" env (draw env w)
                          pure w
  EventKey _ Down _ _ -> pure w
  _ -> pure w0
  where w = w0 { wAudio = True }      -- 最初のクリックかキーで音を出しはじめる

-- ---------- やってみよう ----------
-- 1. 階段（1）で 12 段上がると、時計は 1 周して最初の音に戻る。でも「上がり続けた」ように聞こえる？
-- 2. O で 1 本だけにすると、1 オクターブ上がったところで急に下へ戻るのが聞こえる（ごまかしが見える）
-- 3. 窓の幅 σ を 0.4（せまい）と 4（広い）にすると、錯覚はどう変わる？
-- 4. D で下がり続ける音に。映画やゲームで「緊張が高まり続ける」効果に使われている
-- 5. fmin や np を変えて、自分の「終わらない音」を作ろう

jsRoundD :: Double -> Int
jsRoundD x = floor (x + 0.5)

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

-- ---- 自己テスト：シェパード音の設計図（画面の棒と鳴らす和音に使う partials）の計算を確かめる
selfTest :: World -> [(String, Bool)]
selfTest w =
  [ ("volumes always add up to 1 (phase 0, 0.3, 0.77)", all (\ph -> abs (sum (map snd (partials w ph)) - 1) < 1e-12) [0, 0.3, 0.77])
  , ("one octave up (u -> u + 1) gives exactly the same set of frequencies and volumes (u = 0, 0.4)", key2 (partials w 0) == key2 (partials w 1) && key2 (partials w 0.4) == key2 (partials w 1.4))
  , ("partials are exactly one octave (x 2) apart", and (zipWith (\(fa, _) (fb, _) -> abs (fb / fa - 2) < 1e-12) p0 (drop 1 p0)))
  , ("every audible partial is within 20-20000 Hz", all (\ph -> all (\(fr, v) -> v < 1e-3 || (fr >= 20 && fr <= 20000)) (partials w ph)) [0, 0.5, 0.99])
  , (printf "the end partials are almost silent (swaps go unnoticed): %.1e" w0v, w0v < 0.01 && lastW < 0.01)
  , ("staircase: 12 steps = one octave up (u = " ++ show u12 ++ "), clock back at start; going down stays in 0..np",
       u12 == 1 && phaseOf u12 == 0 && modN (-0.25) (fromIntegral np) == fromIntegral np - 0.25) ]
  where
    key2 ps = sort [ (round (fr * 1e6) :: Integer, round (v * 1e12) :: Integer) | (fr, v) <- ps ]
    p0 = partials w 0
    w0v = case p0 of { ((_, v) : _) -> v; [] -> 1 }
    lastW = case reverse (partials w 0.999) of { ((_, v) : _) -> v; [] -> 1 }
    u12 = iterate (stepPos 1) 0 !! 12
