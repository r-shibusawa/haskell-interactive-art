-- ============================================
-- 57-15 カメラ映像を 3D で回す ─ 物体の回転と「色の回転」
-- 映像を立方体の面に貼り、ドラッグ（アークボール、08）で回す。さらに画素の色 (r, g, b) を純四元数とみなし、q (rgb) q* で色空間を回す
-- 灰色の軸 (1, 1, 1) まわりに回すと、明るさはそのままで色相だけが回る（120° で 赤 → 緑 → 青）
-- 操作：ドラッグ 立方体を回す / ← → 色の回転角 / ↑↓ 色の回転軸 / A 色を回し続ける / V カメラ ⇔ テスト画像 / S 保存
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.List (foldl', intercalate, sortOn)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

capW, capH :: Int
capW = 160                             -- ★ 処理する映像の大きさ（小さいほど速い）
capH = 120

gridU, gridV :: Int
gridU = 20                             -- ★ 立方体の 1 つの面を何個の四角に分けて映像を貼るか（横・縦）
gridV = 15

-- 色を回す軸と名前
axesC :: [(Vec, String)]
axesC = [ (Vec 1 1 1, "gray axis (1, 1, 1): the hue turns"), (Vec 1 0 0, "red axis"), (Vec 0 1 0, "green axis"), (Vec 0 0 1, "blue axis") ]

data World = World
  { wCam     :: Maybe Camera
  , wUseCam  :: !Bool
  , wLive    :: !Bool                  -- 本物のカメラ映像を使っているか
  , wSrc     :: !(U.Vector Word8)      -- 元の映像（RGB）
  , wBuf     :: !(U.Vector Word8)      -- 色を回した映像（RGB）
  , wAxisIdx :: !Int
  , wColAng  :: !Double
  , wAutoCol :: !Bool
  , wQObj    :: !Q
  , wDragFrom :: !(Maybe Vec)
  , wTest    :: [(String, Bool)] }

world0 :: World
world0 = World Nothing True False testImg testImg 0 (pi / 3) False
               (qMul (qAxisAngle (Vec 1 0 0) 0.35) (qAxisAngle (Vec 0 1 0) (-0.6))) Nothing []

main :: IO ()
main = runSketch (sketch "57-15 Camera image turned with quaternions" world0)
  { skBack  = rgbC 14 16 30
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera capW capH        -- 左右反転した映像が来る（p5.js 版の flipped: true）
      pure world0 { wCam = Just cam, wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- カメラがないときの画像：色相の輪と灰色の帯、白い円
testImg :: U.Vector Word8
testImg = U.generate (capW * capH * 3) px
  where
    px i = let (j, ch) = i `divMod` 3
               (y, x) = j `divMod` capW
               fx = fromIntegral x :: Float
               fy = fromIntegral y :: Float
               bx = fromIntegral (4 * (x `div` 4)) :: Float
               by = fromIntegral (4 * (y `div` 4)) :: Float
               c | (fx + 0.5 - 80) ^ (2 :: Int) + (fy + 0.5 - 45.6) ^ (2 :: Int) < 13 * 13 = hsb 0 0 100
                 | fy >= 90 = hsb 0 0 ((bx / 160) * 100)
                 | otherwise = hsb ((bx / 160) * 360) (30 + (by / 120) * 90) 95
               (r, g, b, _) = rgbaOfColor c
           in toB (getAt 0 [r, g, b] ch * 255)
    toB v = fromIntegral (max 0 (min 255 (round v :: Int))) :: Word8

-- 色を回す四元数と行列（q v q* と同じ、自己テストで確認）
colorQ :: World -> Q
colorQ w = qAxisAngle (fst (getAt (Vec 1 1 1, "") axesC (wAxisIdx w))) (wColAng w)

-- 画素ごとに灰色 (0.5, 0.5, 0.5) を原点にして行列 M を掛ける
rotateColors :: Mat -> U.Vector Word8 -> U.Vector Word8
rotateColors (Mat (Vec m00 m01 m02) (Vec m10 m11 m12) (Vec m20 m21 m22)) src = U.generate (U.length src) f
  where
    f i = let o = i - i `mod` 3
              c k = fromIntegral (src U.! (o + k)) / 255 - 0.5 :: Double
              r = c 0; g = c 1; b = c 2
              v = case i `mod` 3 of
                0 -> m00 * r + m01 * g + m02 * b
                1 -> m10 * r + m11 * g + m12 * b
                _ -> m20 * r + m21 * g + m22 * b
          in fromIntegral (max 0 (min 255 (round (255 * (v + 0.5)) :: Int)))

step :: Env -> Float -> World -> IO World
step env _ w0' = do
  let w1 = if wAutoCol w0' then w0' { wColAng = jsMod (wColAng w0' + 0.02) (4 * pi) } else w0'
      w2 = case wDragFrom w1 of
        Just from | envDown env ->
          let to = arcballVec env (mouseP5 env)
          in w1 { wQObj = qUnit (qMul (arcballQ from to) (wQObj w1)), wDragFrom = Just to }
        _ -> w1
  -- カメラの映像が届くまではテスト画像
  (live, src) <- case wCam w2 of
    Just cam | wUseCam w2 -> do
      ok <- camLive cam
      if not ok then pure (False, testImg) else do
        fr <- cameraFrame cam
        pure (if B.length fr >= capW * capH * 3
                then (True, U.generate (capW * capH * 3) (BU.unsafeIndex fr))
                else (False, testImg))
    _ -> pure (False, testImg)
  pure w2 { wLive = live, wSrc = src, wBuf = rotateColors (qToMat (colorQ w2)) src }

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawItems cubeItems
  , say ("Camera image turned in 3D, colour rotation q (rgb) q*   " ++ (if wLive w then "[camera]" else "[test image]")) 20 34 20 (grayC 240)
  , say "Drag: turn the cube   Left/Right colour angle   Up/Down colour axis   A: keep turning   V: camera <-> test image   S save" 20 60 15 (grayC 175)
  , say (snd (getAt (Vec 1 1 1, "") axesC (wAxisIdx w)) ++ "   angle " ++ fmt 0 (wColAng w * 180 / pi) ++ " deg") x y 16 (rgbC 255 210 110)
  , say ("q = " ++ qStr 3 q) x (y + 24) 14 (grayC 220)
  , say "colour rotation matrix (around gray 0.5)" x (y + 52) 14 (grayC 220) ]
  ++ [ say (intercalate "   " [ (if v >= 0 then " " else "") ++ fmt 3 v | v <- row ]) (x + 10) (y + 74 + fromIntegral k * 19) 14 (grayC 220)
     | (k, row) <- zip [0 :: Int ..] (matList m) ]
  -- 赤・緑・青・灰色がどこへ行くか
  ++ concat [ [ Color (rgbD r g b 255) (rectTL (x + fromIntegral n * 70) (y + 150) 28 28)
              , Color (let Vec a b' c = qRot q (Vec (r / 255 - 0.5) (g / 255 - 0.5) (b / 255 - 0.5))
                           cl t = clampv (255 * (t + 0.5)) 0 255
                       in rgbD (cl a) (cl b') (cl c) 255) (rectTL (x + fromIntegral n * 70 + 30) (y + 150) 28 28) ]
            | (n, (r, g, b)) <- zip [0 :: Int ..] [(255, 0, 0), (0, 255, 0), (0, 0, 255), (128, 128, 128), (255, 200, 80)] ]
  ++ [ say "left = original, right = turned (gray stays)" x (y + 200) 14 (grayC 170)
     , at (x + 80, y + 280) (Scale 1 (-1) (img (wSrc w)))
     , at (x + 250, y + 280) (Scale 1 (-1) (img (wBuf w)))
     , say "original" x (y + 358) 14 (grayC 170), say "colours turned" (x + 170) (y + 358) 14 (grayC 170)
     , drawSelfTest env (wTest w) ]
  where
    x = 20; y = 120
    q = colorQ w
    m = qToMat q
    sc = realToFrac (min (envW env) (envH env)) * 0.2
    img v = pixelImage capW capH 1 (\px py -> let o = (py * capW + px) * 3 in (v U.! o, v U.! (o + 1), v U.! (o + 2)))
    corners = map (qRot (wQObj w)) cubeCorners
    -- 立方体の 6 面に映像を貼る（頂点は q v q* で回す）。gloss は画像をゆがめられないので、面を小さな四角に分けて色をぬる
    cubeItems =
      [ Item (cz * sc) (Pictures
          [ Color (pixelC (wBuf w) uc vc) (Polygon (map (glP env sc . bil) [(u0, v0), (u1, v0), (u1, v1), (u0, v1)]))
          | i <- [0 .. gridU - 1], j <- [0 .. gridV - 1]
          , let u0 = fromIntegral i / fromIntegral gridU; u1 = fromIntegral (i + 1) / fromIntegral gridU
                v0 = fromIntegral j / fromIntegral gridV; v1 = fromIntegral (j + 1) / fromIntegral gridV
                uc = (u0 + u1) / 2; vc = (v0 + v1) / 2 ])
      | (n, idx) <- cubeFaces
      , let fp = map (getAt (Vec 0 0 0) corners) idx
            -- テクスチャの座標 (u, v)：頂点 0 → (0, 1)、1 → (1, 1)、2 → (1, 0)、3 → (0, 0)
            c0 = getAt (Vec 0 0 0) fp 0; c1 = getAt (Vec 0 0 0) fp 1; c2 = getAt (Vec 0 0 0) fp 2; c3 = getAt (Vec 0 0 0) fp 3
            bil (u, v) = foldl' v3Add (Vec 0 0 0) [ v3Scl c3 ((1 - u) * (1 - v)), v3Scl c2 (u * (1 - v)), v3Scl c1 (u * v), v3Scl c0 ((1 - u) * v) ]
            ctr = v3Scl (foldl' v3Add (Vec 0 0 0) fp) 0.25
            Vec _ _ cz = ctr
            nw = qRot (wQObj w) n
      , v3Dot nw (v3Sub (Vec 0 0 (eyeZ env / sc)) ctr) > 0 ]         -- 裏を向いた面は描かない
    pixelC v u vv = let px = min (capW - 1) (floor (u * fromIntegral capW))
                        py = min (capH - 1) (floor (vv * fromIntegral capH))
                        o = (py * capW + px) * 3
                        c k = fromIntegral (v U.! (o + k)) :: Double
                    in rgbD (c 0) (c 1) (c 2) 255

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wDragFrom = Just (arcballVec env (mouseP5 env)) }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDragFrom = Nothing }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wColAng = wColAng w + pi / 12 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wColAng = wColAng w - pi / 12 }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wAxisIdx = (wAxisIdx w + 1) `mod` length axesC }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wAxisIdx = (wAxisIdx w + length axesC - 1) `mod` length axesC }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAutoCol = not (wAutoCol w) }
    | c `elem` "vV" -> pure w { wUseCam = not (wUseCam w) }
    | c `elem` "sS" -> saveSvg "quaternion_color" env (draw env w) >> pure w
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("colour rotation matrix = q (rgb) q* for every pixel (200 colours)", e1 < 1e-12)
  , ("around the gray axis: brightness r + g + b stays", e2 < 1e-12)
  , ("around the gray axis: distance from gray (saturation) stays, gray does not move", e3 < 1e-12 && e4 < 1e-12)
  , ("120 deg around the gray axis: red -> green -> blue -> red"
    , v3Dist (qRot q120 red) (Vec (-0.5) 0.5 (-0.5)) < 1e-12 && v3Dist (qRot q120 (qRot q120 red)) (Vec (-0.5) (-0.5) 0.5) < 1e-12)
  , ("a 360 deg turn gives the original colour (also with q = -1)", v3Dist (qRot (qAxisAngle gray (2 * pi)) red) red < 1e-12)
  , ("1000 arcball turns keep |q| = 1", abs (qNorm qa - 1) < 1e-14) ]
  where
    gray = v3Unit (Vec 1 1 1)
    (trials, qa) = evalGen 15 $ do
      ts <- replicateM 200 $ do
        ang <- rndG
        a <- rndG; b <- rndG; c <- rndG
        pure (ang * 7, Vec (a - 0.5) (b - 0.5) (c - 0.5))
      let unitG = do { a <- rndNormalG; b <- rndNormalG; c <- rndNormalG; pure (v3Unit (Vec a b c)) }
          loop :: Int -> Q -> Gen Q
          loop 0 q = pure q
          loop k q = do { u <- unitG; v <- unitG; loop (k - 1) (qUnit (qMul (arcballQ u v) q)) }
      q <- loop 1000 (Q 1 0 0 0)
      pure (ts, q)
    errs = [ ( v3Dist w (matVec mm v)
             , abs (wx + wy + wz - (vx + vy + vz))
             , abs (v3Len (v3Add v (v3Scl gray (negate (v3Dot v gray)))) - v3Len (v3Add w (v3Scl gray (negate (v3Dot w gray)))))
             , v3Dist (qRot q gr) gr )
           | (ang, v@(Vec vx vy vz)) <- trials
           , let q = qAxisAngle gray ang; mm = qToMat q; w@(Vec wx wy wz) = qRot q v; gr = Vec 0.2 0.2 0.2 ]
    e1 = maximum (0 : [ a | (a, _, _, _) <- errs ])
    e2 = maximum (0 : [ a | (_, a, _, _) <- errs ])
    e3 = maximum (0 : [ a | (_, _, a, _) <- errs ])
    e4 = maximum (0 : [ a | (_, _, _, a) <- errs ])
    q120 = qAxisAngle gray (2 * pi / 3)
    red = Vec 0.5 (-0.5) (-0.5)

-- ---------- やってみよう ----------
-- 1. ← → で色を 120° 回そう。赤い物は何色になる？ 240° では？
-- 2. ↑↓ で軸を「赤の軸」にすると、赤はそのままで緑と青が混ざる。灰色の軸とどうちがう？
-- 3. A で色を回し続けながら、立方体をドラッグで回そう。物体の回転と色の回転は同じ q v q*
-- 4. 回した色が 0〜255 からはみ出すと切られる。どの軸・角でいちばん切られる？（灰色の軸では切られにくい理由は？）
-- 5. S で保存して「色が回る自画像」を作ろう

-- ==================================================
-- ここから下は「四元数の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）。自己テストで使う
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rnd :: Rng -> (Double, Rng)
rnd (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 標準正規分布（ボックス＝ミュラー法）
rndNormal :: Rng -> (Double, Rng)
rndNormal r0 = let (a, r1) = rnd r0
                   (v, r2) = rnd r1
                   u = 1 - a
               in (sqrt (-2 * log u) * cos (2 * pi * v), r2)

-- 乱数を引きながら計算する小さなモナド（JavaScript と同じく左から順に引く）
newtype Gen a = Gen { runGen :: Rng -> (a, Rng) }

instance Functor Gen where
  fmap f (Gen g) = Gen (\r -> case g r of (a, r1) -> (f a, r1))

instance Applicative Gen where
  pure a = Gen (\r -> (a, r))
  Gen f <*> Gen g = Gen (\r -> case f r of
                                 (h, !r1) -> case g r1 of (a, !r2) -> (h a, r2))

instance Monad Gen where
  Gen g >>= k = Gen (\r -> case g r of (a, !r1) -> runGen (k a) r1)

rndG, rndNormalG :: Gen Double
rndG = Gen rnd
rndNormalG = Gen rndNormal

-- 種 s から始めて m を計算する
evalGen :: Word32 -> Gen a -> a
evalGen s m = fst (runGen m (seedRng s))

clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else v

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標に
mouseP5 :: Env -> (Double, Double)
mouseP5 env = let (x, y) = envMouse env in (realToFrac (x + envW env / 2), realToFrac (envH env / 2 - y))

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC r g b a = makeColorI r g b (max 0 (min 255 a))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 0〜255 の小数で色を作る（明るさを掛けた色など）
rgbD :: Double -> Double -> Double -> Double -> Color
rgbD r g b a = makeColor (c r) (c g) (c b) (c a)
  where c v = f2 (clampv (v / 255) 0 1)

-- 左下が (x, y) の文字（y は文字の下の線 = p5.js の BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央（textAlign(CENTER, CENTER)）
sayCC :: String -> Float -> Float -> Float -> Color -> Picture
sayCC s x y size c = sayC s x (y + size * 0.36) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 w の箱に折り返して書く（p5.js の text(s, x, y, w, h)。(x, y) は箱の左上）
sayBox :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayBox s x y w size c = Pictures [ say ln x (y + size + fromIntegral k * size * 1.25) size c | (k, ln) <- zip [0 :: Int ..] (wrapWords (words s)) ]
  where
    wrapWords [] = []
    wrapWords (h : t) = go h t
    go cur [] = [cur]
    go cur (x1 : xs)
      | textW (cur ++ " " ++ x1) size <= w = go (cur ++ " " ++ x1) xs
      | otherwise = cur : go x1 xs

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 中心 (x, y)、半径 r の円の線（太さ wt）
circleLine :: Float -> Float -> Float -> Float -> Picture
circleLine wt x y r = Translate x y (ThickCircle r wt)

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- ---- 数の道具
-- Math.hypot（JavaScript と同じ計算：最大値で割ってカハンの足し算）
hypotL :: [Double] -> Double
hypotL xs =
  let as = map abs xs
      m = maximum (0 : as)
      go (!s, !c) v = let n = v / m
                          summand = n * n - c
                          pre = s + summand
                      in (pre, (pre - s) - summand)
  in if any isInfinite as then 1 / 0 else if any isNaN as then 0 / 0 else if m == 0 then 0
     else sqrt (fst (foldl' go (0, 0) as)) * m

hypot3 :: Double -> Double -> Double -> Double
hypot3 a b c = hypotL [a, b, c]

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- JavaScript の % （0 の方へ切り捨てた余り）
jsMod :: Double -> Double -> Double
jsMod a b = a - b * fromIntegral (truncate (a / b) :: Integer)

-- Math.round（.5 は上へ）
jsRound :: Double -> Double
jsRound v = fromIntegral (floor (fin 0 v + 0.5) :: Integer)

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v0 = let v = fin 0 v0
               s = showFFloat (Just n) v ""
           in if all (`elem` "-0.") s then dropWhile (== '-') s else s

-- 指数で書く（toExponential(n)、例 1.2e-16）
expo :: Int -> Double -> String
expo n v0
  | v == 0 = fmt n 0 ++ "e+0"
  | otherwise = let s = showEFloat (Just n) v ""
                    (m, e) = break (== 'e') s
                    e' = drop 1 e
                in m ++ "e" ++ (if take 1 e' == "-" then e' else '+' : e')
  where v = fin 0 v0
-- ---- 四元数 Q w x y z = w + xi + yj + zk
data Q = Q !Double !Double !Double !Double deriving (Eq, Show)

qMul :: Q -> Q -> Q                    -- ハミルトン積（i² = j² = k² = ijk = −1）
qMul (Q a0 a1 a2 a3) (Q b0 b1 b2 b3) =
  Q (a0 * b0 - a1 * b1 - a2 * b2 - a3 * b3)
    (a0 * b1 + a1 * b0 + a2 * b3 - a3 * b2)
    (a0 * b2 - a1 * b3 + a2 * b0 + a3 * b1)
    (a0 * b3 + a1 * b2 - a2 * b1 + a3 * b0)

qConj :: Q -> Q                        -- 共役 q*
qConj (Q w x y z) = Q w (-x) (-y) (-z)

qAdd, qSub :: Q -> Q -> Q
qAdd (Q a b c d) (Q e f g h) = Q (a + e) (b + f) (c + g) (d + h)
qSub (Q a b c d) (Q e f g h) = Q (a - e) (b - f) (c - g) (d - h)

qScl :: Q -> Double -> Q
qScl (Q a b c d) s = Q (a * s) (b * s) (c * s) (d * s)

qDot :: Q -> Q -> Double
qDot (Q a b c d) (Q e f g h) = a * e + b * f + c * g + d * h

qNorm :: Q -> Double                   -- |q|
qNorm a = sqrt (qDot a a)

qUnit :: Q -> Q
qUnit a = let n = qNorm a in if n > 1e-300 then qScl a (1 / n) else Q 1 0 0 0

qInv :: Q -> Q                         -- q⁻¹ = q*/|q|²
qInv a = let n2 = qDot a a in if n2 > 1e-300 then qScl (qConj a) (1 / n2) else Q 0 0 0 0

qDist :: Q -> Q -> Double
qDist a b = qNorm (qSub a b)

qList :: Q -> [Double]
qList (Q w x y z) = [w, x, y, z]

qAt :: Q -> Int -> Double              -- 成分 0〜3
qAt q i = getAt 0 (qList q) i

qAxisAngle :: Vec -> Double -> Q       -- 軸 n まわりに角度 th の回転：cos(th/2) + sin(th/2) n
qAxisAngle n th = let Vec ux uy uz = v3Unit n
                      s = sin (th / 2)
                  in Q (cos (th / 2)) (ux * s) (uy * s) (uz * s)

qRot :: Q -> Vec -> Vec                -- v を q v q* で回す（v は 3 次元ベクトル → 純四元数）
qRot q (Vec x y z) = let Q _ a b c = qMul (qMul q (Q 0 x y z)) (qConj q) in Vec a b c

qToMat :: Q -> Mat                     -- 同じ q から作った 3×3 回転行列（単位四元数のとき）
qToMat (Q w x y z) =
  Mat (Vec (1 - 2 * (y * y + z * z)) (2 * (x * y - w * z)) (2 * (x * z + w * y)))
      (Vec (2 * (x * y + w * z)) (1 - 2 * (x * x + z * z)) (2 * (y * z - w * x)))
      (Vec (2 * (x * z - w * y)) (2 * (y * z + w * x)) (1 - 2 * (x * x + y * y)))

qExp :: Q -> Q                         -- exp(w + v) = e^w (cos|v| + sin|v| v/|v|)
qExp (Q a0 a1 a2 a3) =
  let vn = hypot3 a1 a2 a3
      ew = exp a0
      s = if vn > 1e-12 then sin vn / vn else 1 - (vn * vn) / 6
  in Q (ew * cos vn) (ew * s * a1) (ew * s * a2) (ew * s * a3)

qLog :: Q -> Q                         -- log q = ln|q| + acos(w/|q|) v/|v|
qLog a@(Q a0 a1 a2 a3) =
  let n = qNorm a
      vn = hypot3 a1 a2 a3
      th = atan2 vn a0
      s = if vn > 1e-12 then th / vn else 1 / max n 1e-300
  in Q (log (max n 1e-300)) (s * a1) (s * a2) (s * a3)

qSlerp :: Q -> Q -> Double -> Bool -> Q   -- 球面線形補間。shortest なら −b と近い方の道
qSlerp a b0 t shortest =
  let d0 = qDot a b0
      (b, d1) = if shortest && d0 < 0 then (qScl b0 (-1), -d0) else (b0, d0)
      d = clampv d1 (-1) 1
      om = acos d
      so = sin om
  in if so < 1e-9 then qUnit (qAdd (qScl a (1 - t)) (qScl b t))       -- ほぼ同じ向き → lerp で十分
     else qAdd (qScl a (sin ((1 - t) * om) / so)) (qScl b (sin (t * om) / so))

qNlerp :: Q -> Q -> Double -> Bool -> Q   -- 正規化した線形補間
qNlerp a b0 t shortest =
  let b = if shortest && qDot a b0 < 0 then qScl b0 (-1) else b0
  in qUnit (qAdd (qScl a (1 - t)) (qScl b t))

qAngle :: Q -> Q -> Double             -- 2 つの姿勢の間の回転角（0〜π）
qAngle a b = let Q d0 d1 d2 d3 = qMul (qConj (qUnit a)) (qUnit b)
             in 2 * atan2 (hypot3 d1 d2 d3) (abs d0)

qRandom :: Gen Q                       -- 一様にランダムな単位四元数（S³ の上の点）
qRandom = do
  a <- rndNormalG; b <- rndNormalG; c <- rndNormalG; d <- rndNormalG
  pure (qUnit (Q a b c d))

qStr :: Int -> Q -> String             -- (w, x, y, z) を小数点以下 dg 桁で
qStr dg q = "(" ++ intercalate ", " [ fmt dg (if abs v < 0.5 * 10 ** negate (fromIntegral dg) then 0 else v) | v <- qList q ] ++ ")"

-- ---- 3 次元ベクトルと 3×3 行列（行列は 3 つの行）
data Vec = Vec !Double !Double !Double deriving (Eq, Show)
data Mat = Mat !Vec !Vec !Vec deriving (Eq, Show)

v3Dot :: Vec -> Vec -> Double
v3Dot (Vec a b c) (Vec d e f) = a * d + b * e + c * f

v3Cross :: Vec -> Vec -> Vec
v3Cross (Vec a0 a1 a2) (Vec b0 b1 b2) = Vec (a1 * b2 - a2 * b1) (a2 * b0 - a0 * b2) (a0 * b1 - a1 * b0)

v3Len :: Vec -> Double
v3Len (Vec a b c) = hypot3 a b c

v3Unit :: Vec -> Vec
v3Unit a@(Vec x y z) = let n = v3Len a in if n > 1e-300 then Vec (x / n) (y / n) (z / n) else Vec 0 0 1

v3Add, v3Sub :: Vec -> Vec -> Vec
v3Add (Vec a b c) (Vec d e f) = Vec (a + d) (b + e) (c + f)
v3Sub (Vec a b c) (Vec d e f) = Vec (a - d) (b - e) (c - f)

v3Scl :: Vec -> Double -> Vec
v3Scl (Vec a b c) s = Vec (a * s) (b * s) (c * s)

v3Dist :: Vec -> Vec -> Double
v3Dist (Vec a b c) (Vec d e f) = hypot3 (a - d) (b - e) (c - f)

v3List :: Vec -> [Double]
v3List (Vec a b c) = [a, b, c]

matVec :: Mat -> Vec -> Vec
matVec (Mat r0 r1 r2) v = Vec (v3Dot r0 v) (v3Dot r1 v) (v3Dot r2 v)

matT :: Mat -> Mat
matT (Mat (Vec a b c) (Vec d e f) (Vec g h i)) = Mat (Vec a d g) (Vec b e h) (Vec c f i)

matMul :: Mat -> Mat -> Mat
matMul (Mat r0 r1 r2) b = let Mat c0 c1 c2 = matT b
                              row r = Vec (v3Dot r c0) (v3Dot r c1) (v3Dot r c2)
                          in Mat (row r0) (row r1) (row r2)

matDet :: Mat -> Double
matDet (Mat r0 r1 r2) = v3Dot r0 (v3Cross r1 r2)

matList :: Mat -> [[Double]]
matList (Mat a b c) = map v3List [a, b, c]

matDiff :: Mat -> Mat -> Double        -- 成分の差の最大
matDiff a b = maximum (0 : zipWith (\x y -> abs (x - y)) (concat (matList a)) (concat (matList b)))

matOrthoErr :: Mat -> Double           -- |MᵀM − I| の最大成分（回転行列なら 0）
matOrthoErr a = matDiff (matMul (matT a) a) matI

matI :: Mat
matI = Mat (Vec 1 0 0) (Vec 0 1 0) (Vec 0 0 1)
-- ---- 3D の道具（p5.js の WEBGL の代わり）：回転は全部 q v q* で計算し、頂点を自分で透視投影する
--   数学の向き（x 右・y 上・z 手前、右手系）。目は z = eyeZ（p5.js の WEBGL の標準のカメラと同じ距離）にある
--   面も線も「奥行き z と絵」の組（Item）にして、奥から順に描く（画家のアルゴリズム）
data Item = Item !Double Picture

eyeZ :: Env -> Double
eyeZ env = realToFrac (envH env) / 2 / tan (pi / 6)

-- 3D の点 v（拡大 s）を画面（p5.js の座標）へ
glP :: Env -> Double -> Vec -> (Float, Float)
glP env s (Vec x y z) =
  let d = eyeZ env
      f = d / max 1 (d - z * s)
  in (envW env / 2 + f2 (x * s * f), envH env / 2 - f2 (y * s * f))

-- 奥から順に並べて 1 枚の絵に
drawItems :: [Item] -> Picture
drawItems its = Pictures [ p | Item _ p <- sortOn (\(Item z _) -> z) its ]

lightDir :: Vec
lightDir = v3Unit (Vec 0.4 0.7 0.8)

-- 線分 a → b（長い線は奥行きを正しく並べるため短く分ける）
glLine :: Env -> Double -> Color -> Float -> Vec -> Vec -> [Item]
glLine env s col w a b =
  let n = max 1 (min 12 (ceiling (v3Dist a b / 0.3) :: Int))
      pt k = v3Add a (v3Scl (v3Sub b a) (fromIntegral k / fromIntegral n))
  in [ Item (0.5 * (zOf p + zOf q) * s + 0.5) (Color col (thickLine w (glP env s p) (glP env s q)))
     | k <- [0 .. n - 1], let p = pt k, let q = pt (k + 1) ]
  where zOf (Vec _ _ z) = z

-- 折れ線（beginShape / vertex / endShape）
glPath :: Env -> Double -> Color -> Float -> [Vec] -> [Item]
glPath env s col w ps = concat (zipWith (glLine env s col w) ps (drop 1 ps))

-- a → b の矢印（線 + 先の 4 本のひげ）
glArrow :: Env -> Double -> Color -> Float -> Vec -> Vec -> [Item]
glArrow env s col w a b =
  let d = v3Sub b a
      l = v3Len d
      u = v3Scl d (1 / l)
      Vec _ uy _ = u
      side = v3Unit (v3Cross u (if abs uy < 0.9 then Vec 0 1 0 else Vec 1 0 0))
      up = v3Cross u side
      h = min 0.25 (l * 0.3)
  in glLine env s col w a b
     ++ if l < 1e-9 then [] else
          concat [ glLine env s col w b (v3Add (v3Add b (v3Scl u (-h))) (v3Scl t (h * 0.4))) | t <- [side, up, v3Scl side (-1), v3Scl up (-1)] ]

-- 箱を組み合わせた物体。Part 中心 半分の大きさ 色(r, g, b)
data Part = Part !Vec !Vec !(Double, Double, Double)

cubeFaces :: [(Vec, [Int])]
cubeFaces = [ (Vec 1 0 0, [1, 2, 6, 5]), (Vec (-1) 0 0, [0, 4, 7, 3]), (Vec 0 1 0, [2, 3, 7, 6])
            , (Vec 0 (-1) 0, [0, 1, 5, 4]), (Vec 0 0 1, [4, 5, 6, 7]), (Vec 0 0 (-1), [0, 3, 2, 1]) ]

cubeCorners :: [Vec]
cubeCorners = [ Vec (-1) (-1) (-1), Vec 1 (-1) (-1), Vec 1 1 (-1), Vec (-1) 1 (-1)
              , Vec (-1) (-1) 1, Vec 1 (-1) 1, Vec 1 1 1, Vec (-1) 1 1 ]

partCorners :: (Vec -> Vec) -> Vec -> Part -> [Vec]
partCorners rotFn offset (Part (Vec cx cy cz) (Vec hx hy hz) _) =
  [ v3Add (rotFn (Vec (cx + x * hx) (cy + y * hy) (cz + z * hz))) offset | Vec x y z <- cubeCorners ]

-- 物体を回転 rotFn（v → 回した v）で描く。alpha は 0〜255。裏を向いた面は描かない
glParts :: Env -> Double -> (Vec -> Vec) -> Vec -> Double -> [Part] -> [Item]
glParts env s rotFn offset alpha parts =
  [ Item (cz * s) (Color (rgbD (r * b) (g * b) (bl * b) alpha) (Polygon (map (glP env s) fp)))
  | part@(Part _ _ (r, g, bl)) <- parts
  , let pts = partCorners rotFn offset part
  , (n, idx) <- cubeFaces
  , let fp = map (getAt (Vec 0 0 0) pts) idx
        ctr = v3Scl (foldl' v3Add (Vec 0 0 0) fp) 0.25
        Vec _ _ cz = ctr
        nw = v3Unit (rotFn n)
        b = 0.42 + 0.58 * max 0 (v3Dot nw lightDir)
  , v3Dot nw (v3Sub (Vec 0 0 (eyeZ env / s)) ctr) > 0 ]

-- 同じ物体を針金（辺だけ）で。面より少し手前に置いて、面に隠れないようにする
glWire :: Env -> Double -> (Vec -> Vec) -> Color -> Vec -> [Part] -> [Item]
glWire env s rotFn col offset parts =
  [ Item (z + 0.08 * s) p
  | part <- parts
  , let pts = partCorners rotFn offset part
  , (a, b) <- [(0, 1), (1, 2), (2, 3), (3, 0), (4, 5), (5, 6), (6, 7), (7, 4), (0, 4), (1, 5), (2, 6), (3, 7)]
  , Item z p <- glLine env s col 1.5 (getAt (Vec 0 0 0) pts a) (getAt (Vec 0 0 0) pts b) ]

-- 飛行機のような物体（向きが分かりやすい）。機首 = +z、上 = +y、右の翼 = +x
planeParts :: [Part]
planeParts =
  [ Part (Vec 0 0 0) (Vec 0.18 0.18 0.9) (235, 235, 240)
  , Part (Vec 0 0 0.05) (Vec 1.0 0.04 0.26) (90, 200, 235)
  , Part (Vec 0 0.28 (-0.72)) (Vec 0.04 0.26 0.16) (245, 200, 80)
  , Part (Vec 0 0 (-0.75)) (Vec 0.4 0.03 0.12) (90, 200, 235)
  , Part (Vec 0 0.02 0.95) (Vec 0.1 0.1 0.06) (240, 110, 90) ]

-- ---- アークボール（仮想トラックボール）：画面の点 → 球面の点 → 2 点を結ぶ回転の四元数
arcballVec :: Env -> (Double, Double) -> Vec
arcballVec env (mx, my) =
  let w = realToFrac (envW env); h = realToFrac (envH env)
      rr = 0.42 * min w h
      x = (mx - w / 2) / rr
      y = negate (my - h / 2) / rr
      d = x * x + y * y
  in if d < 1 then Vec x y (sqrt (1 - d)) else v3Unit (Vec x y 0)

arcballQ :: Vec -> Vec -> Q            -- a を b に重ねる最小の回転（角度 = a と b の間）
arcballQ a b = let Vec cx cy cz = v3Cross a b in qUnit (Q (1 + v3Dot a b) cx cy cz)
