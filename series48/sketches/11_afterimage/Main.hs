-- ============================================
-- 48-11 補色残像 ─ 白黒の写真に色が見える
-- 色を反転した絵（補色：255 − R, 255 − G, 255 − B）の真ん中の + を見つめてから、白黒の絵に切りかえると
-- しばらくの間、白黒の絵に本当の色が見える。目の色のセンサーが疲れて、反対の色が強く感じられるから
-- 操作：スペース = スタート / ↑↓ = 見つめる時間 / 1〜3 = 絵を選ぶ / T = 真実（マウスの下の画素の RGB を表示）
--   長く見つめて気分が悪くなったら目を離す
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import System.Environment (lookupEnv)

testSec :: Float
testSec = 10                 -- ★ 白黒の絵を見せる時間（秒）

pixK :: Float
pixK = 2.5                   -- 絵の 1 画素を画面の何 px にするか（計算を軽くするため）

-- 3 枚の絵（元の色・補色・白黒）。画素は 0xRRGGBB の整数で持つ
data Pics = Pics
  { pW, pH  :: !Int
  , pOrig   :: !(U.Vector Int)
  , pComp   :: !(U.Vector Int)
  , pGrey   :: !(U.Vector Int)
  , pOrigP, pCompP, pGreyP :: Picture
  }

data World = World
  { wAdapt :: !Float           -- ★ 見つめる時間（秒）
  , wScene :: !Int             -- 絵の番号 1〜3
  , wTruth :: !Bool
  , wStart :: !Float           -- スタートした時刻（秒）。−1 = 待ち
  , wPics  :: Pics
  , wTest  :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "48-11 Colour afterimage" w0)
  { skBack  = g' 30
  , skStart = \env -> do
      let w = w0 { wPics = makePics env 1 }
          rs = selfTest w
      printSelfTest rs
      pure w { wTest = rs }
  , skStep  = \env _ w -> pure (if phaseAt w (elapsed env w) == Ready && wStart w >= 0 then w { wStart = -1 } else w)
  , skDraw  = draw
  , skEvent = event
  }
  where w0 = World 20 1 False (-1) (makePics env1 1) []
        env1 = Env 1280 800 (0, 0) False 0 0 S.empty

data Phase = Ready | Adapt | Test deriving (Eq, Show)

phaseAt :: World -> Float -> Phase
phaseAt w t
  | t < 0 = Ready
  | t < wAdapt w = Adapt
  | t < wAdapt w + testSec = Test
  | otherwise = Ready

elapsed :: Env -> World -> Float
elapsed env w = if wStart w < 0 then -1 else envTime env - wStart w

-- 絵を置く枠（左上 x, y、幅、高さ）
frameBox :: Env -> (Float, Float, Float, Float)
frameBox env =
  let w = jsRound (min (envW env * 0.62) ((envH env - 170) * 1.6))
      h = jsRound (fromIntegral w / 1.6)
  in (fromIntegral (jsRound ((envW env - fromIntegral w) / 2)), 90, fromIntegral w, fromIntegral h)

-- ---- 3 枚の絵：元の色 → 補色 → 白黒（明るさだけ）
makePics :: Env -> Int -> Pics
makePics env k =
  let (_, _, bw, bh) = frameBox env
      iw = max 8 (jsRound (bw / pixK))
      ih = max 5 (jsRound (bh / pixK))
      (bg, shs) = drawing k (fromIntegral iw) (fromIntegral ih)
      orig = U.generate (iw * ih) (\i -> let (y, x) = i `divMod` iw in pack (readPix bg shs x y))
      comp = U.map (pack . complement . unpack) orig
      grey = U.map (\v -> let l = lumaOf (unpack v) in pack (l, l, l)) orig
      pic v = imgPicture iw ih (\x y -> unpack (v U.! (y * iw + x)))
  in Pics iw ih orig comp grey (pic orig) (pic comp) (pic grey)

complement :: RGB -> RGB
complement (r, gg, b) = (255 - r, 255 - gg, 255 - b)

-- テレビと同じ明るさの式
lumaOf :: RGB -> Int
lumaOf (r, gg, b) = jsRound (0.299 * fromIntegral r + 0.587 * fromIntegral gg + 0.114 * fromIntegral b)

-- 元の絵（背景の色と形のリスト）。w × h は絵の画素数
drawing :: Int -> Float -> Float -> (RGB, [Shape])
drawing k w h = case k of
  1 -> ( (70, 140, 235)                                    -- 風景：空・太陽・丘・家
       , [ Dot (250, 220, 40) (w * 0.78) (h * 0.25) (h * 0.13)
         , Ell (60, 170, 70) (w * 0.25) (h * 1.05) (w * 0.45) (h * 0.45)
         , Ell (40, 140, 60) (w * 0.8) (h * 1.1) (w * 0.45) (h * 0.4)
         , Box (220, 50, 50) (w * 0.42) (h * 0.5) (w * 0.18) (h * 0.22)
         , Poly (220, 50, 50) [(w * 0.4, h * 0.5), (w * 0.62, h * 0.5), (w * 0.51, h * 0.36)]
         , Box (250, 240, 210) (w * 0.48) (h * 0.6) (w * 0.05) (h * 0.12) ] )
  2 -> ( g 0                                                -- 3 色の帯と円
       , [ Box c (w * fromIntegral i / 3) 0 (w / 3 + 1) h | (i, c) <- zip [0 :: Int ..] [(40, 90, 220), (245, 245, 245), (220, 40, 50)] ]
         ++ [ Dot (250, 200, 30) (w / 2) (h / 2) (h * 0.225) ] )
  _ -> ( g 235                                              -- 色とりどりの円
       , [ Dot (hsbRGB (fromIntegral ((i * 72 + j * 24) `mod` 360)) 80 95) ((fromIntegral i + 0.5) * w / 5) ((fromIntegral j + 0.5) * h / 3) (h * 0.13)
         | i <- [0 .. 4 :: Int], j <- [0 .. 2 :: Int] ] )

-- ---- 画面：いま見せる絵、待ちのときは 3 枚の小さな絵、上の帯
screen :: Env -> World -> [Shape]
screen env w =
  [ Img bx by kx ky (pW p) (pH p) (pixOf v) pic ]
  ++ (if ph == Ready then [ Img (bx + fromIntegral i * (tw + 20)) ty (tw / fromIntegral (pW p)) (th / fromIntegral (pH p)) (pW p) (pH p) (pixOf v') pic'
                          | (i, (v', pic')) <- zip [0 :: Int ..] [(pOrig p, pOrigP p), (pComp p, pCompP p), (pGrey p, pGreyP p)] ]
      else [ Seg (g 0) 3 (cx - 12, cy) (cx + 12, cy), Seg (g 0) 3 (cx, cy - 12) (cx, cy + 12) ])
  ++ [ Box (g 235) 0 0 (envW env) 70 ]
  where
    p = wPics w
    ph = phaseAt w (elapsed env w)
    (bx, by, bw, bh) = frameBox env
    kx = bw / fromIntegral (pW p)
    ky = bh / fromIntegral (pH p)
    (v, pic) = case ph of
      Adapt -> (pComp p, pCompP p)
      Test -> (pGrey p, pGreyP p)
      Ready -> (pOrig p, pOrigP p)
    tw = fromIntegral (jsRound (bw / 3.8))
    th = fromIntegral (jsRound (tw / 1.6))
    ty = by + bh + 12
    cx = bx + bw / 2
    cy = by + bh / 2
    pixOf vec x y = unpack (vec U.! (y * pW p + x))

draw :: Env -> World -> Picture
draw env w = view env $
  [ render shs ]
  ++ (if ph == Ready then [ txt (bx + fromIntegral i * (tw + 20)) (by + bh + 12 + th + 16) 14 (g' 220) nm
                          | (i, nm) <- zip [0 :: Int ..] ["original", "complement (stare)", "grey (test)"] ] else [])
  ++ [ txt 20 30 18 (g' 20) ("Space: start   Up/Down: stare " ++ show (round (wAdapt w) :: Int) ++ " s   1-3: picture " ++ show (wScene w) ++ "   T: truth (pixel under mouse)")
     , txt 20 56 15 (rgb255 160 30 30) "Stare only at the +, try not to blink. Look away if you feel unwell."
     , txtC (envW env / 2) (envH env - 8) 22 (g' 230) msg ]
  ++ (if wTruth w && mx >= 0 && my >= 0 && mx < envW env && my < envH env then
        let c@(r, gg, b) = colourAt (g 30) shs (mx, my)
        in [ Color (g' 255) (rect (envW env - 125) 130 210 80), Color (c' c) (rect (envW env - 190) 130 60 60)
           , txt (envW env - 150) 122 15 (g' 20) ("R " ++ show r ++ " G " ++ show gg)
           , txt (envW env - 150) 146 15 (g' 20) ("B " ++ show b ++ (if r == gg && gg == b then " (no colour)" else "")) ]
      else [])
  ++ [ txt 20 (envH env - 16) 14 (testColour (wTest w)) (selfTestLine (wTest w)) ]
  where
    shs = screen env w
    t = elapsed env w
    ph = phaseAt w t
    (bx, by, bw, bh) = frameBox env
    tw = fromIntegral (jsRound (bw / 3.8))
    th = fromIntegral (jsRound (tw / 1.6))
    (mx0, my0) = envMouse env
    mx = mx0 + envW env / 2
    my = envH env / 2 - my0
    msg = case ph of
      Ready -> "Press space to start"
      Adapt -> "Stare at the + ... " ++ show (ceiling (wAdapt w - t) :: Int) ++ " s left"
      Test -> "A grey picture. Do you see colours? (" ++ show (ceiling (wAdapt w + testSec - t) :: Int) ++ ")"

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wStart = envTime env }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wAdapt = min 60 (wAdapt w + 5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wAdapt = max 5 (wAdapt w - 5) }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> let k = fromEnum c - fromEnum '0' in w { wScene = k, wStart = -1, wPics = makePics env k }
    | c `elem` "tT" -> w { wTruth = not (wTruth w) }
  EventResize _ -> w { wPics = makePics env (wScene w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 見つめる時間 5 秒・20 秒・40 秒で、色が見えている時間をくらべる
-- 2. テスト中に T を押してマウスで白黒の絵をなぞる。R = G = B（色なし）なのに色が見える
-- 3. + から目をそらして見つめると、残像の色がずれる・ぼやける
-- 4. 2（3 色の帯）で、見える色は元の色と同じ？ 少しちがう？
-- 5. drawing に自分の絵を描こう（国旗、好きなキャラクターの色 など）

pack :: RGB -> Int
pack (r, gg, b) = r * 65536 + gg * 256 + b

unpack :: Int -> RGB
unpack v = (v `div` 65536, (v `div` 256) `mod` 256, v `mod` 256)

-- HSB（360, 100, 100）→ 0〜255 の RGB
hsbRGB :: Float -> Float -> Float -> RGB
hsbRGB h s b = let (r, gg, bb, _) = rgbaOfColor (hsb h s b) in (q r, q gg, q bb)
  where q x = jsRound (x * 255)

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

-- ---- 自己テスト：補色と白黒の計算を、絵の全部の画素で確かめる
selfTest :: World -> [(String, Bool)]
selfTest w =
  [ ("all " ++ show n ++ " pixels: original + complement = 255 (R, G, B each)", allSum)
  , ("complement of complement = original", complement (complement (12, 200, 99)) == (12, 200, 99))
  , ("grey picture: R = G = B at every pixel (no colour at all)", allGrey)
  , ("grey = 0.299 R + 0.587 G + 0.114 B (white 255, black 0)", allLuma && lumaOf (255, 255, 255) == 255 && lumaOf (0, 0, 0) == 0)
  , ("timer: stare -> test -> ready", phaseAt w (-1) == Ready && phaseAt w 1 == Adapt && phaseAt w (wAdapt w + 1) == Test && phaseAt w (wAdapt w + testSec + 1) == Ready)
  , ("the original has colours (" ++ show (S.size colours) ++ " or more)", S.size colours >= 4) ]
  where
    p = wPics w
    n = U.length (pOrig p)
    orig = U.map unpack (pOrig p)
    comp = U.map unpack (pComp p)
    grey = U.map unpack (pGrey p)
    allSum = U.and (U.zipWith (\(a, b, c) (d, e, f) -> a + d == 255 && b + e == 255 && c + f == 255) orig comp)
    allGrey = U.all (\(a, b, c) -> a == b && b == c) grey
    allLuma = U.and (U.zipWith (\o (a, _, _) -> a == lumaOf o) orig grey)
    colours = S.fromList [ pOrig p U.! i | i <- [0, 97 .. n - 1] ]
