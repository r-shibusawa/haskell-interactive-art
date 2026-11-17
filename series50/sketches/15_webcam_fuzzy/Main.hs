-- ============================================
-- 50-15 カメラ映像のファジィ処理
-- 画素ごとに「肌色」「明るい」「鮮やか」の所属度を計算し、4 つのファジィ規則で色を決める（ファジィな領域分け）
--   色 = Σ wᵢ · 色ᵢ ÷ Σ wᵢ（高木－菅野型の 0 次：後件が「色」という定数）。どこにも当てはまらない画素は背景色
-- 操作：1〜3 = 表示（塗り分け・ドット・所属度マップ） / ←→ = マップの所属度を選ぶ / T = かつ を min ↔ 積
--       V = カメラ ↔ テスト画像 / S = 保存（SVG）
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as U
import Data.List (foldl')
import Data.Word (Word8)
import System.Environment (lookupEnv)
import Numeric (showFFloat)

camW, camH :: Int
camW = 160                  -- カメラの大きさ（小さくして計算を軽く）
camH = 120

type RGB = (Double, Double, Double)

-- ★ 規則：もし … ならば この色（RGB）
rules :: [(String, RGB)]
rules =
  [ ("1 skin & bright", (255, 190, 160))       -- 肌色 かつ 明るい
  , ("2 dark", (40, 30, 80))                   -- 暗い
  , ("3 vivid & !skin", (40, 200, 190))        -- 鮮やか かつ 肌色でない
  , ("4 pale & !skin", (200, 195, 230)) ]      -- 明るい かつ 鮮やかでない かつ 肌色でない

bgCol :: RGB
bgCol = (90, 85, 110)       -- ★ どの規則も発火しないときの色

skinHue :: Double
skinHue = 20                -- ★ 肌色の色相 [度]

mapNames :: [String]
mapNames = ["skin", "bright", "vivid"]

data World = World
  { wView   :: !Int                 -- 1 塗り分け / 2 ドット / 3 所属度マップ
  , wMapSel :: !Int                 -- マップに出す所属度
  , wProd   :: !Bool                -- かつ を積にするか
  , wUseCam :: !Bool                -- カメラを使うか
  , wCam    :: Maybe Camera
  , wLive   :: !Bool                -- 本物のカメラ映像が来ているか
  , wSrc    :: U.Vector Word8       -- 入力の画素（RGB）
  , wOut    :: U.Vector (Word8, Word8, Word8)     -- 出力の色
  , wMem    :: U.Vector (Double, Double, Double)  -- 所属度 [肌色, 明るい, 鮮やか]
  , wOutPic, wMapPic, wSrcPic :: Picture          -- 画像（計算しなおしたときだけ作る）
  , wTest   :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "50-15 Fuzzy camera" w0)
  { skBack  = hsb 250 18 14
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera camW camH
      pure (process w0 { wCam = Just cam, wTest = selfTest })
  , skStep  = \_ _ w -> stepCam w
  , skDraw  = draw
  , skEvent = event
  }
  where w0 = World 1 0 False True Nothing False testImage U.empty U.empty Blank Blank Blank []

-- カメラが使えればその画素、使えなければテスト画像
stepCam :: World -> IO World
stepCam w = case wCam w of
  Just cam | wUseCam w -> do
    live <- camLive cam
    if live
      then do
        f <- cameraFrame cam
        pure (if B.length f >= camW * camH * 3
                then process w { wLive = True, wSrc = U.generate (camW * camH * 3) (B.index f) }
                else w { wLive = True })
      else pure (useTest w)
  _ -> pure (useTest w)
  where
    useTest v
      | wLive v = process v { wLive = False, wSrc = testImage }
      | otherwise = v

-- 全部の画素を処理（19200 画素）
process :: World -> World
process w = w { wOut = out, wMem = mem
              , wOutPic = pixelImage camW camH 1 (\x y -> out U.! (y * camW + x))
              , wMapPic = pixelImage camW camH 1 (\x y -> let mv = selM (mem U.! (y * camW + x)) * 255
                                                           in (byte (mv * 0.6), byte (mv * 0.95), byte mv))
              , wSrcPic = pixelImage camW camH 1 (\x y -> let o = 3 * (y * camW + x) in (src U.! o, src U.! (o + 1), src U.! (o + 2))) }
  where
    src = wSrc w
    res = U.generate (camW * camH) (\i -> let o = 3 * i in pixelFast (wProd w) (px o) (px (o + 1)) (px (o + 2)))
    px o = fromIntegral (src U.! o)
    out = U.map fst res
    mem = U.map snd res
    selM (a, b, c) = case wMapSel w of { 0 -> a; 1 -> b; _ -> c }

byte :: Double -> Word8
byte v = fromIntegral (max 0 (min 255 (round v :: Int)))

-- 画面の配置
data Lay = Lay { lSc, lIw, lIh, lX0, lY0 :: !Float }

layOf :: Env -> Lay
layOf env = Lay sc (fromIntegral camW * sc) (fromIntegral camH * sc) 20 90
  where sc = min (envW env * 0.66 / fromIntegral camW) ((envH env - 130) / fromIntegral camH)

draw :: Env -> World -> Picture
draw env w = Pictures [view env (picture env w)]

picture :: Env -> World -> [Picture]
picture env w =
  [ case wView w of
      1 -> img (wOutPic w)
      3 -> img (wMapPic w)
      _ -> Pictures (Color (hsb 250 18 8) (rectTL x0 y0 iw ih) : dots)
    -- 小さく元の画像
  , Translate (x0 + iw - fromIntegral camW / 2 - 8) (y0 + 8 + fromIntegral camH / 2) (Scale 1 (-1) (wSrcPic w))
    -- マウスの画素
  , Color white (rectLine (x0 + fromIntegral px * sc) (y0 + fromIntegral py * sc) 12 12)
  , txt hx hy 15 (hsb 0 0 95) ("Pixel (" ++ show px ++ ", " ++ show py ++ ")  RGB = " ++ show r0 ++ ", " ++ show g0 ++ ", " ++ show b0)
  , txt hx (hy + 22) 14 (hsb 0 0 70) ("hue " ++ fmt 0 (fpH f) ++ " deg   sat " ++ fmt 2 (fpS f) ++ "   value " ++ fmt 2 (fpV f)) ]
  ++ concat [ bar hx (hy + 44 + fromIntegral i * 30) bw n m (hsb 175 60 80)
            | (i, n, m) <- zip3 [0 :: Int ..] mapNames (let (a, b, c) = fpM f in [a, b, c]) ]
  ++ [ txt hx (hy + 150) 16 (hsb 0 0 95) ("Rule strengths w (AND = " ++ (if wProd w then "product" else "min") ++ ")") ]
  ++ concat [ bar hx (hy + 168 + fromIntegral i * 30) bw n wi (rgbC c) | (i, (n, c), wi) <- zip3 [0 :: Int ..] rules (fpW f) ]
  ++ [ Color (rgbC (fpCol f)) (rectTL hx (hy + 300) 60 40)
     , txt (hx + 72) (hy + 314) 14 (hsb 0 0 90) "Output = Sum w*color / Sum w"
     , txt (hx + 72) (hy + 334) 14 (hsb 0 0 90) ("Sum w = " ++ fmt 3 sumW ++ (if sumW < 1e-6 then "  (none fired: background)" else ""))
     , txt (hx + 72) (hy + 354) 14 (hsb 0 0 90) ("= (" ++ (let (a, b, c) = fpCol f in show (jsRound a) ++ ", " ++ show (jsRound b) ++ ", " ++ show (jsRound c)) ++ ")")
     , txt 20 32 17 (hsb 0 0 95) ((if wLive w then "Camera" else "Test image (V: camera)") ++ "  |  1: colors   2: dots   3: membership map (Left/Right: "
                                  ++ getAt "" mapNames (wMapSel w) ++ ")")
     , txt 20 56 15 (hsb 0 0 85) ("T: AND = " ++ (if wProd w then "product" else "min") ++ "   V: switch   S: save    Skin = hue near "
                                  ++ show (round skinHue :: Int) ++ " deg (gauss) AND moderate saturation (triangle)")
     , txt 20 78 14 (hsb 0 0 70) "Bright = sigmoid of value, vivid = sigmoid of saturation"
     , txt 20 (envH env - 20) 14 (if all snd (wTest w) then hsb 160 50 90 else hsb 0 80 100) (selfTestLine (wTest w)) ]
  where
    Lay sc iw ih x0 y0 = layOf env
    img p = Translate (x0 + iw / 2) (y0 + ih / 2) (Scale sc (-sc) p)
    out = wOut w
    mem = wMem w
    dots
      | U.length out < camW * camH || U.length mem < camW * camH = []
      | otherwise =
          [ Color (let (r, g, b) = out U.! k in makeColorI (fromIntegral r) (fromIntegral g) (fromIntegral b) 255)
                  (at (x0 + fromIntegral x * sc, y0 + fromIntegral y * sc) (disc (sc * (0.6 + 1.6 * realToFrac (max m0 m2)) / 2)))
          | y <- [1, 3 .. camH - 1], x <- [1, 3 .. camW - 1]
          , let k = y * camW + x, let (m0, _, m2) = mem U.! k ]
    (mx, my) = p5Mouse env
    px0 = floor ((mx - x0) / sc) :: Int
    py0 = floor ((my - y0) / sc) :: Int
    inside = px0 >= 0 && px0 < camW && py0 >= 0 && py0 < camH
    (px, py) = if inside then (px0, py0) else (camW `div` 2, camH `div` 2)
    o = 3 * (py * camW + px)
    srcAt k = if k < U.length (wSrc w) then wSrc w U.! k else 0
    (r0, g0, b0) = (srcAt o, srcAt (o + 1), srcAt (o + 2))
    f = fuzzyPixel (wProd w) (fromIntegral r0) (fromIntegral g0) (fromIntegral b0)
    sumW = sum (fpW f)
    hx = x0 + iw + 30
    hy = 110
    bw = envW env - hx - 40

-- 所属度・強さの棒
bar :: Float -> Float -> Float -> String -> Double -> Color -> [Picture]
bar x y w name v c =
  [ Color (hsba 0 0 100 0.08) (rectTL (x + 150) (y - 14) (w - 210) 18)
  , Color c (rectTL (x + 150) (y - 14) ((w - 210) * realToFrac v) 18)
  , txt x y 14 (hsb 0 0 85) name
  , txt (x + w - 50) y 14 (hsb 0 0 85) (fmt 3 v) ]

rgbC :: RGB -> Color
rgbC (r, g, b) = makeColor (realToFrac r / 255) (realToFrac g / 255) (realToFrac b / 255) 1

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "123" -> pure w { wView = fromEnum c - fromEnum '0' }
    | c `elem` "tT" -> pure (process w { wProd = not (wProd w) })
    | c `elem` "vV" -> pure w { wUseCam = not (wUseCam w) }
    | c `elem` "sS" -> do
        saveSvg "fuzzy_camera" env (draw env w)
        pure w
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (process w { wMapSel = (wMapSel w + 1) `mod` 3 })
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (process w { wMapSel = (wMapSel w + 2) `mod` 3 })
  _ -> pure w

-- ---- 自己テスト：手で計算できる値と比べる
selfTest :: [(String, Bool)]
selfTest =
  [ ("w = (0.5, 0.5, 0, 0) -> mean of the colors of rules 1 and 2 (147.5, 110, 120)", near3 (blendRules [0.5, 0.5, 0, 0]) (147.5, 110, 120))
  , ("only one rule -> its color whatever the strength (normalized by Sum w)", near3 (blendRules [0.3, 0, 0, 0]) (255, 190, 160))
  , ("no rule fires (Sum w = 0) -> background color, no NaN", blendRules [0, 0, 0, 0] == bgCol)
  , ("w = (0.2, 0, 0.6, 0) -> (0.2*255 + 0.6*40) / 0.8 = 93.75",
       let (a, b, _) = blendRules [0.2, 0, 0.6, 0] in near a 93.75 && near b ((0.2 * 190 + 0.6 * 200) / 0.8))
  , ("RGB -> HSV: red = (0 deg, 1, 1), blue = (240 deg, 1, 1)",
       let (h, s, v) = rgb2hsv 255 0 0; (h2, _, _) = rgb2hsv 0 0 255 in near h 0 && near s 1 && near v 1 && near h2 240)
  , ("hue difference on a circle: 350 deg and 20 deg differ by 30 deg", near (hueDiff 350 20) 30 && near (hueDiff 20 350) 30)
  , ("black pixel: bright ~ 0 -> rule \"dark\" is the strongest",
       let (_, b, _) = fpM k; ws = fpW k; w1 = getAt 0 ws 1
       in b < 0.01 && w1 > 0.99 && and [i == 1 || wi <= w1 | (i, wi) <- zip [0 :: Int ..] ws]) ]
  where
    near3 (a, b, c) (x, y, z) = near a x && near b y && near c z
    k = fuzzyPixel False 0 0 0

-- ---------- やってみよう ----------
-- 1. 3 で所属度マップにして ←→。自分の顔や手は「肌色」でどれくらい白くなる？ skinHue を変えて合わせよう
-- 2. rules の色を自分の好きな配色に変える（夕焼け、海、ネオン…）
-- 3. 規則を 1 つ足してみよう：「鮮やか かつ 暗い ならば 紫」（fuzzyPixel の ws に 1 つ、rules に 1 行足す）
-- 4. T で 積 にすると境目がやわらかくなる？ 2 のドット表示で見比べよう
-- 5. S で保存して、元の写真と並べてみよう

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点、y は下向き）で
p5Mouse :: Env -> (Float, Float)
p5Mouse env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 左上が (x, y) の塗りつぶした長方形（p5.js の rect）
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 小数点以下 n 桁で書く（p5.js の toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) v ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Int
jsRound v = floor (v + 0.5)

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ==================================================
-- ここから下は「ファジィの道具」（中身は読まなくても OK。消さないでね）
-- 計算はすべて Double（倍精度）で行う
-- ==================================================
-- ---- メンバーシップ関数（所属度 μ を 0〜1 で返す）
trimf :: Double -> Double -> Double -> Double -> Double      -- 三角：a で 0、b で 1、c で 0
trimf x a b c
  | x < a || x > c = 0
  | x == b = 1
  | x < b = if b > a then (x - a) / (b - a) else 1
  | otherwise = if c > b then (c - x) / (c - b) else 1

gaussmf :: Double -> Double -> Double -> Double
gaussmf x c s = exp (-((x - c) * (x - c)) / (2 * s * s))

sigmf :: Double -> Double -> Double -> Double
sigmf x a c = 1 / (1 + exp (-a * (x - c)))

fnot :: Double -> Double
fnot a = 1 - a

-- ---- 自己テストの道具
near :: Double -> Double -> Bool
near a b = nearTol 1e-9 a b

nearTol :: Double -> Double -> Double -> Bool
nearTol tol a b = abs (a - b) <= tol

-- 結果を端末に出す（テスト中は 1 行のまとめと失敗した項目だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK  " else "NG  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "NG ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- ---- 画素のファジィ処理
-- JavaScript の % と同じ余り（符号はわられる数と同じ）
jsMod :: Double -> Double -> Double
jsMod a m = a - m * fromIntegral (truncate (a / m) :: Int)

rgb2hsv :: Double -> Double -> Double -> (Double, Double, Double)
rgb2hsv r0 g0 b0 = (if h < 0 then h + 360 else h, if mx > 0 then d / mx else 0, mx)
  where
    r = r0 / 255
    g = g0 / 255
    b = b0 / 255
    mx = max r (max g b)
    mn = min r (min g b)
    d = mx - mn
    h | d <= 1e-9 = 0
      | mx == r = 60 * jsMod ((g - b) / d) 6
      | mx == g = 60 * ((b - r) / d + 2)
      | otherwise = 60 * ((r - g) / d + 4)

hueDiff :: Double -> Double -> Double
hueDiff a b = let d = jsMod (abs (a - b)) 360 in if d > 180 then 360 - d else d

-- 1 つの画素の結果：色相・彩度・明度、所属度 [肌色, 明るい, 鮮やか]、規則の強さ w、出力の色
data FP = FP { fpH, fpS, fpV :: !Double, fpM :: !(Double, Double, Double), fpW :: [Double], fpCol :: !RGB }

fuzzyPixel :: Bool -> Double -> Double -> Double -> FP
fuzzyPixel prod r g b = FP h s v (skin, bright, vivid) ws (blendRules ws)
  where
    (h, s, v) = rgb2hsv r g b
    bright = sigmf v 12 0.5
    vivid = sigmf s 12 0.5
    skin = gaussmf (hueDiff h skinHue) 0 14 * trimf s 0.12 0.4 0.75 * sigmf v 15 0.3
    andF a c = if prod then a * c else min a c
    ws = [ andF skin bright
         , fnot bright
         , andF vivid (fnot skin)
         , andF (andF bright (fnot vivid)) (fnot skin) ]

-- fuzzyPixel と同じ計算を、リストを作らずに速く（画面全体の処理用）。返り値：(出力の色, 所属度)
pixelFast :: Bool -> Double -> Double -> Double -> ((Word8, Word8, Word8), (Double, Double, Double))
pixelFast prod r g b
  | sw < 1e-9 = (bytes bgCol, (skin, bright, vivid))
  | otherwise = ((byte (cr / sw), byte (cg / sw), byte (cb / sw)), (skin, bright, vivid))
  where
    (h, s, v) = rgb2hsv r g b
    bright = sigmf v 12 0.5
    vivid = sigmf s 12 0.5
    skin = gaussmf (hueDiff h skinHue) 0 14 * trimf s 0.12 0.4 0.75 * sigmf v 15 0.3
    andF a c = if prod then a * c else min a c
    w1 = andF skin bright
    w2 = fnot bright
    w3 = andF vivid (fnot skin)
    w4 = andF (andF bright (fnot vivid)) (fnot skin)
    sw = w1 + w2 + w3 + w4
    (cr, cg, cb) = foldl' (\(a, bb, c) (wi, (_, (rr, gg, b2))) -> (a + wi * rr, bb + wi * gg, c + wi * b2)) (0, 0, 0) (zip [w1, w2, w3, w4] rules)
    bytes (a, bb, c) = (byte a, byte bb, byte c)

-- 色 = Σ w·色 ÷ Σ w（Σ w が 0 なら背景色）
blendRules :: [Double] -> RGB
blendRules ws
  | sw < 1e-9 = bgCol
  | otherwise = (cr / sw, cg / sw, cb / sw)
  where
    (sw, cr, cg, cb) = foldl (\(s, a, b, c) (wi, (_, (r, g, bb))) -> (s + wi, a + wi * r, b + wi * g, c + wi * bb)) (0, 0, 0, 0) (zip ws rules)

-- カメラが使えないときのテスト画像（RGB）：肌色の円・鮮やかなしま・明暗のグラデーション
testImage :: U.Vector Word8
testImage = U.concatMap (\i -> let (y, x) = i `divMod` camW; (r, g, b) = pix x y in U.fromList [byte r, byte g, byte b])
                        (U.enumFromN 0 (camW * camH))
  where
    pix :: Int -> Int -> RGB
    pix x y =
      let t = fromIntegral x / fromIntegral camW
          c0 = (40 + 180 * t, 40 + 170 * t, 60 + 170 * t)
          c1 = if y > 92 then (if odd (x `div` 20) then (230, 40, 60) else (30, 160, 230)) else c0
          dx = fromIntegral (x - 60)
          dy = fromIntegral (y - 48)
          c2 = if dx * dx + dy * dy < 900 then (225, 165, 135 - (dx + dy) * 0.6) else c1
      in if (x - 125) * (x - 125) + (y - 40) * (y - 40) < 300 then (40, 220, 90) else c2
