-- ============================================
-- 57-11 四元数のジュリア集合 ─ z ← z² + c を 4 次元で
-- 2 次元のジュリア集合（複素数）と同じ式を四元数で計算すると、4 次元のフラクタルになる。その 3 次元の切り口を描く
-- 描き方：距離推定 DE = ½|z| ln|z| / |z'|（表面までの距離の目安）だけ光線を進める「レイマーチング」。小さな画像に少しずつ描いて拡大
-- 操作：1〜5 c を選ぶ / ← → c の i 成分 / ↑↓ 切り口（4 つ目の座標）/ ドラッグ 回す / Q 細かさ / スペース 自動で回す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

-- ★ c の候補
cList :: [Q]
cList = [ Q (-0.2) 0.8 0 0, Q (-0.125) (-0.256) 0.847 0.0895, Q 0.185 0.478 0.125 (-0.392)
        , Q (-0.291) (-0.399) 0.339 0.437, Q (-0.45) 0.447 0.181 0.306 ]

maxIt, maxStep :: Int
maxIt = 10                             -- ★ z ← z² + c の回数
maxStep = 90                           -- ★ 光線を進める回数の上限

bound :: Double
bound = 1.7                            -- ★ この半径の球の外には形がないとする

resList :: [Int]
resList = [160, 240, 320]              -- ★ 描く画像の横の画素数（Q で切りかえ。p5.js 版の 360 は画素が多すぎるので 320）

pixPerFrame :: Int
pixPerFrame = 2000                     -- ★ 1 フレームに描く画素の数（p5.js 版は 12 ミリ秒ぶん）

data World = World
  { wC       :: !Q
  , wSlice   :: !Double                -- 切り口の 4 つ目の座標
  , wRes     :: !Int
  , wImgW    :: !Int
  , wImgH    :: !Int
  , wImg     :: !(U.Vector Word8)      -- 画像（r, g, b を並べたもの）
  , wPix     :: !Int                   -- 次に描く画素の番号（行 × 幅 + 列）
  , wCam     :: !Q
  , wAuto    :: !Bool
  , wDrag    :: !(Maybe (Double, Double))
  , wT0      :: !Double                -- 描き始めた時刻
  , wDoneSec :: !Double                -- 描き終わるのにかかった秒数（0 = まだ）
  , wTest    :: [(String, Bool)] }

world0 :: World
world0 = World (getAt (Q 0 0 0 0) cList 0) 0 1 0 0 U.empty 0
               (qMul (qAxisAngle (Vec 1 0 0) (-0.4)) (qAxisAngle (Vec 0 1 0) 0.5)) False Nothing 0 0 []

main :: IO ()
main = runSketch (sketch "57-11 Quaternion Julia sets" world0)
  { skBack  = rgbC 0 0 0
  , skStart = \env -> do
      printSelfTest selfTest
      pure (restart env world0 { wTest = selfTest })
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

-- 描き直し（大きさが変わったときだけ画像を作り直す。前の絵は上書きされるまで残る）
restart :: Env -> World -> World
restart env w =
  let bw = getAt 240 resList (wRes w)
      bh = max 2 (round (fromIntegral bw * envH env / envW env))
      img | wImgW w == bw && wImgH w == bh && U.length (wImg w) == bw * bh * 3 = wImg w
          | otherwise = U.generate (bw * bh * 3) (\o -> if o `mod` 3 == 2 then 30 else 18)
  in w { wImgW = bw, wImgH = bh, wImg = img, wPix = 0, wT0 = realToFrac (envTime env), wDoneSec = 0 }

qSq :: Q -> Q                          -- z² = (w² − |v|², 2wv)
qSq (Q a b c d) = Q (a * a - b * b - c * c - d * d) (2 * a * b) (2 * a * c) (2 * a * d)

-- 3 次元の点 p（切り口の 4 つ目 = sw）から表面までの距離の目安
juliaDE :: Double -> Vec -> Q -> Double
juliaDE sw (Vec x y z) c = go 0 z0 1 (qNorm z0)
  where
    z0 = Q x y z sw
    go !n !zz !dz !r
      | n < maxIt && r < 4 = let dz' = 2 * r * dz
                                 zz' = qAdd (qSq zz) c
                             in go (n + 1) zz' dz' (qNorm zz')
      | r < 4 = 0
      | otherwise = (0.5 * r * log r) / dz

-- 光線 ro + t rd を DE ずつ進める。当たれば (t, 回数)、なければ (−1, 回数)
march :: Double -> Vec -> Vec -> Q -> (Double, Int)
march sw ro rd c
  | disc < 0 = (-1, 0)
  | otherwise = go 0 (max 0 (-bq - sqrt disc))
  where
    bq = v3Dot ro rd
    disc = bq * bq - v3Dot ro ro + bound * bound      -- 半径 bound の球に入らない光線は計算しない
    tEnd = -bq + sqrt disc
    go !steps !t
      | steps >= maxStep = (-1, steps)
      | otherwise =
          let d = juliaDE sw (v3Add ro (v3Scl rd t)) c
          in if d < 0.0015 * t then (t, steps)
             else let t' = t + d in if t' > tEnd then (-1, steps) else go (steps + 1) t'

shade :: Double -> Q -> Vec -> Q -> Int -> (Double, Double, Double)
shade sw cam p c steps =
  let e = 0.002
      ks = [Vec 1 (-1) (-1), Vec (-1) (-1) 1, Vec (-1) 1 (-1), Vec 1 1 1]
      nrm@(Vec nx ny nz) = v3Unit (foldl' (\acc v -> v3Add acc (v3Scl v (juliaDE sw (v3Add p (v3Scl v e)) c))) (Vec 0 0 0) ks)
      l = qRot cam (v3Unit (Vec 0.5 0.8 0.6))
      dif = max 0 (v3Dot nrm l)
      ao = 1 - fromIntegral steps / fromIntegral maxStep
      b = (0.25 + 0.75 * dif) * (0.4 + 0.6 * ao)
  in (b * (180 + 75 * (0.5 + 0.5 * nx)), b * (150 + 90 * (0.5 + 0.5 * ny)), b * (210 + 45 * (0.5 + 0.5 * nz)))

-- 画素 1 つの色
pixelColor :: World -> Int -> (Double, Double, Double)
pixelColor w i =
  let bw = wImgW w; bh = wImgH w
      x = i `mod` bw; row = i `div` bw
      fo = 2.0
      ro = qRot (wCam w) (Vec 0 0 3.2)
      hw = fromIntegral bh / 2
      rd = qRot (wCam w) (v3Unit (Vec ((fromIntegral x - fromIntegral bw / 2) / hw) (negate (fromIntegral row - fromIntegral bh / 2) / hw) (-fo)))
      (t, steps) = march (wSlice w) ro rd (wC w)
      g = 18 + 14 * (fromIntegral row / fromIntegral bh)
  in if t > 0 then shade (wSlice w) (wCam w) (v3Add ro (v3Scl rd t)) (wC w) steps else (g, g + 2, g + 16)

step :: Env -> World -> World
step env w0' =
      let total = wImgW w0' * wImgH w0'
          w1 = if wAuto w0' && wPix w0' >= total && total > 0          -- 描き終わってから少し回す
                 then restart env w0' { wCam = qMul (qAxisAngle (Vec 0 1 0) 0.15) (wCam w0') } else w0'
          p0 = wPix w1
          p1 = min total (p0 + pixPerFrame)
          w2 = if p1 > p0
                 then let chunk = U.concatMap (\i -> let (r, g, b) = pixelColor w1 i in U.fromList [byte r, byte g, byte b]) (U.enumFromN p0 (p1 - p0))
                          img = wImg w1
                      in w1 { wImg = U.concat [U.take (p0 * 3) img, chunk, U.drop (p1 * 3) img], wPix = p1 }
                 else w1
      in if wPix w2 >= total && wDoneSec w2 == 0 && total > 0
           then w2 { wDoneSec = max 1e-3 (realToFrac (envTime env) - wT0 w2) } else w2
  where byte v = fromIntegral (max 0 (min 255 (round v :: Int))) :: Word8

draw :: Env -> World -> Picture
draw env w = Pictures
  [ if bw > 0 then pixelImage bw bh k (\x y -> let o = (y * bw + x) * 3 in (img U.! o, img U.! (o + 1), img U.! (o + 2))) else Blank
  , view env $
      [ Color (rgbC 255 200 80) (thickLine 2 (0, yl) (ww, yl)) | wPix w < total ]
      ++ [ Color (rgbaC 0 0 0 150) (rectTL 0 0 ww 100)
         , say ("Quaternion Julia set  z <- z^2 + c,  c = " ++ qStr 3 (wC w) ++ ",  slice (4th coord) = " ++ fmt 2 (wSlice w)) 20 36 20 (grayC 240)
         , say ("1-5: c   Left/Right: i part of c   Up/Down: slice   Drag: turn   Q: detail (now " ++ show bw
                ++ " px wide)   Space: auto turn") 20 62 15 (grayC 175)
         , say (if wPix w < total then "drawing... " ++ show (round (100 * fromIntegral (wPix w) / fromIntegral (max 1 total) :: Double) :: Int) ++ "%"
                else "done (" ++ show bw ++ " x " ++ show bh ++ " pixels, " ++ fmt 1 (wDoneSec w) ++ " s)") 20 86 15 (grayC 175)
         , drawSelfTest env (wTest w) ] ]
  where
    bw = wImgW w; bh = wImgH w; img = wImg w
    total = bw * bh
    ww = envW env
    k = ww / fromIntegral (max 1 bw)
    yl = fromIntegral (wPix w `div` max 1 bw) / fromIntegral (max 1 bh) * envH env

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w { wDrag = Just (mouseP5 env) }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = Nothing }
  EventMotion _ | Just (px, py) <- wDrag w, envDown env ->
    let (mx, my) = mouseP5 env
        dx = mx - px; dy = my - py
        w1 = w { wDrag = Just (mx, my) }
    in if abs dx + abs dy > 0
         then restart env w1 { wCam = qUnit (qMul (qMul (qAxisAngle (Vec 0 1 0) (-dx * 0.008)) (wCam w)) (qAxisAngle (Vec 1 0 0) (-dy * 0.008))) }
         else w1
  -- キーを押したら（どのキーでも）描き直す
  EventKey (Char c) Down _ _ -> restart env (keyChar c w)
  EventKey (SpecialKey k) Down _ _ -> restart env (keySpecial k w)
  _ -> w

keyChar :: Char -> World -> World
keyChar c w
  | c >= '1' && c <= '5' = w { wC = getAt (wC w) cList (fromEnum c - fromEnum '1') }
  | c `elem` "qQ" = w { wRes = (wRes w + 1) `mod` 3 }
  | otherwise = w

keySpecial :: SpecialKey -> World -> World
keySpecial k w = case k of
  KeyRight -> w { wC = let Q a b c d = wC w in Q a (min 1.2 (b + 0.02)) c d }
  KeyLeft -> w { wC = let Q a b c d = wC w in Q a (max (-1.2) (b - 0.02)) c d }
  KeyUp -> w { wSlice = min 1 (wSlice w + 0.05) }
  KeyDown -> w { wSlice = max (-1) (wSlice w - 0.05) }
  KeySpace -> w { wAuto = not (wAuto w) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("z^2 = (w^2 - |v|^2, 2wv) = qMul(z, z)", e1 < 1e-12)
  , ("on the (1, i) plane it is the complex z^2 -> the 2D Julia set is this slice", e6 < 1e-12)
  , ("c = 0 (the set is the unit ball): DE = r ln(r) / 2 (difference " ++ expo 1 e2 ++ ")", e2 < 1e-12)
  , ("c = 0: DE never exceeds the true distance r - 1 (1 < r <= 4) -> rays never jump over the surface", ok3)
  , ("c = 0, a ray from the front hits t ~ 2.2 (surface of the unit ball): t = " ++ fmt 4 hitT, abs (hitT - 2.2) < 0.01)
  , ("far points (|p| = 3) escape for every c (DE > 0), the origin with c = 0 does not"
    , all (\c -> juliaDE 0 (Vec 3 0 0) c > 0 && juliaDE 0 (Vec 0 3 0) c > 0) cList && juliaDE 0 (Vec 0 0 0) (Q 0 0 0 0) == 0) ]
  where
    (e1, e6, e2, ok3) = evalGen 11 $ do
      xs <- replicateM 100 $ do
        z <- Q <$> rndNormalG <*> rndNormalG <*> rndNormalG <*> rndNormalG
        a <- rndNormalG; b <- rndNormalG
        let Q s0 s1 s2 s3 = qSq (Q a b 0 0)
        pure (qDist (qSq z) (qMul z z), maximum [abs (s0 - (a * a - b * b)), abs (s1 - 2 * a * b), abs s2 + abs s3])
      ys <- replicateM 50 $ do
        r0 <- rndG
        u <- v3Unit <$> (Vec <$> rndNormalG <*> rndNormalG <*> rndNormalG)
        let r = 1.05 + 2.9 * r0
            d = juliaDE 0 (v3Scl u r) (Q 0 0 0 0)
        pure (abs (d - 0.5 * r * log r), d > r - 1 + 1e-12)
      pure (maximum (0 : map fst xs), maximum (0 : map snd xs), maximum (0 : map fst ys), not (any snd ys))
    (hitT, _) = march 0 (Vec 0 0 3.2) (Vec 0 0 (-1)) (Q 0 0 0 0)

-- ---------- やってみよう ----------
-- 1. 1〜5 で c を変えよう。どれがつながった形？ ばらばらの形？（2 次元のジュリア集合と同じく c で決まる）
-- 2. ↑↓ で切り口を動かすと、4 次元の形の別の断面が見える。CT スキャンのように 1 枚ずつ
-- 3. cList に Q (-1) 0 0 0 を足すと？（2 次元の「サンマルコ」の形を回したような立体）
-- 4. maxIt を 6 や 16 に変えると、形と描く時間はどう変わる？
-- 5. qSq の代わりに qMul (qMul z z) z（z³）にすると、別のフラクタルになる（DE の 0.5 と 2 も直す）

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
