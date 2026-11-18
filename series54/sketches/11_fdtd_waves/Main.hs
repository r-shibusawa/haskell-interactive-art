{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 54-11 マクスウェル方程式を解く（FDTD 法）─ 電場と磁場が交互に生み合って波になる
-- 格子の上で「磁場の変化 ← 電場の回転」「電場の変化 ← 磁場の回転」を半歩ずつ交互に計算（ヤジー格子、2 次元 TM 波）
-- 色 = 紙に垂直な電場 Ez（赤 ＋、青 −）。銅色 = 金属（Ez = 0）。外の枠は波を吸いこむ（ムーアの境界）
-- 操作：クリック = 波のパルス / ドラッグ = 金属を描く / スペース = アンテナ（連続波）/ ↑↓ = 波長（←→ は 10 ずつ）
--       1 自由空間 2 金属の壁 3 共振器 4 すき間 / C = 消す / R = 最初から
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word (Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nx, ny :: Int
nx = 200                               -- 格子の数（横）
ny = 125                               -- 格子の数（たて）

sc :: Double
sc = 0.5                               -- ★ クーラン数 c·Δt/Δx（2 次元では 1/√2 = 0.707 以下でないと発散する）

dxCm :: Double
dxCm = 1                               -- ★ 1 マス = 1 cm（光の速さ c = 3×10⁸ m/s）

steps :: Int
steps = 3                              -- 1 フレームに進める歩数

-- パルス：格子の (i, j) と、出した時刻
data Pulse = Pulse { pI, pJ, pT0 :: !Int }

data World = World
  { wEz      :: !(U.Vector Double)     -- 電場 Ez
  , wHx      :: !(U.Vector Double)     -- 磁場 Hx
  , wHy      :: !(U.Vector Double)     -- 磁場 Hy
  , wMetal   :: !(U.Vector Word8)      -- 金属なら 1
  , wPreset  :: !Int
  , wCw      :: !Bool                  -- アンテナ（連続波）
  , wLam     :: !Int                   -- 波長 [マス]
  , wAnt     :: !(Maybe (Int, Int))    -- アンテナの場所
  , wPulses  :: [Pulse]
  , wN       :: !Int                   -- 進めた歩数
  , wCwStart :: !Int
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "54-11 Maxwell's equations (FDTD)" world0)
  { skBack  = rgb255 14 18 26
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = makePreset 1 (World e e e (U.replicate (nx * ny) 0) 1 True 20 Nothing [] 0 0 [])
  where e = U.replicate (nx * ny) 0

-- 1 自由空間 2 金属の壁 3 共振器 4 すき間
makePreset :: Int -> World -> World
makePreset p w = w { wPreset = p, wEz = z, wHx = z, wHy = z, wMetal = metal, wPulses = [], wN = 0, wCwStart = 0
                   , wAnt = Just (if p == 3 then (70, 50) else (50, ny `div` 2)) }
  where
    z = U.replicate (nx * ny) 0
    isMetal i j = case p of
      2 -> i == 150 && j >= 10 && j < ny - 10
      3 -> ((j == 25 || j == 100) && i >= 30 && i <= 170) || ((i == 30 || i == 170) && j >= 25 && j <= 100)
      4 -> i == 100 && abs (fromIntegral j - fromIntegral ny / 2 :: Double) > 6
      _ -> False
    metal = U.generate (nx * ny) (\k -> let (j, i) = k `divMod` nx in if isMetal i j then 1 else 0)

-- 1 フレーム = 3 歩
step :: World -> World
step w = w { wEz = ez', wHx = hx', wHy = hy', wN = n1, wPulses = filter (\p -> n1 - pT0 p < 60) (wPulses w) }
  where
    n1 = wN w + steps
    (ez', hx', hy') = runST $ do
      ez <- U.thaw (wEz w)
      hx <- U.thaw (wHx w)
      hy <- U.thaw (wHy w)
      forM_ [0 .. steps - 1] $ \s -> do
        let n = wN w + s
        -- 連続波：周波数 = c/λ（ゆっくり始める）
        case wAnt w of
          Just (ai, aj) | wCw w -> do
            let t = fromIntegral (n - wCwStart w)
            addAt ez (aj * nx + ai) (0.5 * min 1 (t / 80) * sin ((2 * pi * sc * t) / fromIntegral (wLam w)))
          _ -> pure ()
        forM_ (wPulses w) $ \p -> do
          let a = fromIntegral (n - pT0 p - 24) / 8 :: Double
          when (abs a < 3.5) $ addAt ez (pJ p * nx + pI p) (1.2 * exp (-a * a))
        fdtdStep nx ny (wMetal w) ez hx hy sc True
        -- 共振器の壁の小さな損失（ないと共振でどこまでも大きくなる）
        when (wPreset w == 3) $ forM_ [0 .. nx * ny - 1] $ \k -> M.unsafeModify ez (* 0.9997) k
      (,,) <$> U.freeze ez <*> U.freeze hx <*> U.freeze hy
    addAt v k x = M.modify v (+ x) k

-- ---- 描く
layout :: Env -> (Float, Float, Float)
layout env = (cs, (envW env - fromIntegral nx * cs) / 2, 130)
  where cs = min ((envW env - 40) / fromIntegral nx) ((envH env - 160) / fromIntegral ny)

draw :: Env -> World -> Picture
draw env w = view env $
  [ at (ox + fromIntegral nx * cs / 2, oy + fromIntegral ny * cs / 2) (Scale 1 (-1) (pixelImage nx ny cs px)) ]
  ++ [ Color (rgb255 255 240 150) (at (ox + (fromIntegral ai + 0.5) * cs, oy + (fromIntegral aj + 0.5) * cs) (ring 6 2)) | wCw w, Just (ai, aj) <- [wAnt w] ]
  ++ [ say ("1 free space  2 metal wall  3 cavity  4 slit | click pulse  drag metal  Space antenna (" ++ (if wCw w then "ON" else "OFF")
            ++ ")  arrows wavelength  C clear  R reset") 20 28 16 (gray 0.92)
     , say ("Wavelength = " ++ show (wLam w) ++ " cm -> f = c/lambda = " ++ fmt 2 fGHz ++ " GHz | dt = " ++ fmt 1 ((sc * dxCm) / 3e10 * 1e12)
            ++ " ps (Courant " ++ showJ sc ++ " <= 1/sqrt2)   time " ++ fmt 2 tNs ++ " ns") 20 52 15 (rgb255 255 230 120) ]
  ++ [ say s 20 (74 + 18 * fromIntegral k) 14 (gray 0.67) | (k, s) <- zip [0 :: Int ..] (wordWrap (envW env - 40) 14 note) ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    (cs, ox, oy) = layout env
    ez = wEz w
    metal = wMetal w
    fGHz = 30 / (fromIntegral (wLam w) * dxCm)
    tNs = (fromIntegral (wN w) * sc * dxCm) / 30
    -- 弱い波も見えるように √ で強める
    px x y =
      let k = y * nx + x
          e = U.unsafeIndex ez k
          v = signum e * sqrt (min 1 (abs e * 5))
      in if U.unsafeIndex metal k /= 0 then (200, 125, 60)
         else ( b8 (18 + (if v > 0 then 237 * v else 0) + (if v < 0 then -40 * v else 0))
              , b8 (22 + 70 * abs v)
              , b8 (34 + (if v < 0 then -221 * v else 0) + (if v > 0 then 40 * v else 0)) )
    note = "Yee grid: Hx -= S*dEz/dy, Hy += S*dEz/dx, Ez += S*(dHy/dx - dHx/dy) (S = c*dt/dx). " ++ case wPreset w of
      3 -> "Cavity (140 x 75 cm metal box): standing waves at lambda = 2/sqrt((m/140)^2 + (n/75)^2). (1,1) = " ++ fmt 1 (cavLam 1 1)
           ++ ", (2,1) = " ++ fmt 1 (cavLam 2 1) ++ ", (3,1) = " ++ fmt 1 (cavLam 3 1) ++ ", (1,2) = " ++ fmt 1 (cavLam 1 2)
           ++ " cm. Match it with the arrow keys and the wave grows big."
      2 -> "Reflection from a metal wall: with the antenna ON the outgoing and returning waves add up to a standing wave (nodes every lambda/2)."
      4 -> "Through the slit (12 cm) the wave bends around (diffraction). Compare wavelengths longer and shorter than the slit."
      _ -> "Spreads in a circle from one point. At the outer frame it is absorbed and hardly reflects."

cavLam :: Double -> Double -> Double
cavLam m k = 2 / sqrt ((m / 140) ** 2 + (k / 75) ** 2)

b8 :: Double -> Word8
b8 v = fromIntegral (max 0 (min 255 (round v :: Int)))

-- ---- マウスとキー
cellAt :: Env -> Maybe (Int, Int)
cellAt env = if i > 0 && j > 0 && i < nx - 1 && j < ny - 1 then Just (i, j) else Nothing
  where
    (cs, ox, oy) = layout env
    (mx, my) = mouseP5 env
    i = floor ((mx - ox) / cs)
    j = floor ((my - oy) / cs)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> case cellAt env of
    Just (i, j) | U.unsafeIndex (wMetal w) (j * nx + i) == 0 -> w { wPulses = wPulses w ++ [Pulse i j (wN w)], wAnt = Just (i, j) }
    _ -> w
  EventMotion _ | envDown env -> case cellAt env of
    Just (i, j) ->
      let ks = [ (j + dj) * nx + i + di | dj <- [-1 .. 1], di <- [-1 .. 1] ]
      in w { wMetal = wMetal w U.// [ (k, 1) | k <- ks ], wEz = wEz w U.// [ (k, 0) | k <- ks ] }
    Nothing -> w
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> makePreset (fromEnum c - fromEnum '0') w
    | c `elem` "cC" -> let z = U.replicate (nx * ny) 0 in w { wEz = z, wHx = z, wHy = z, wPulses = [] }
    | c `elem` "rR" -> makePreset (wPreset w) w { wLam = 20, wCw = True }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wCw = not (wCw w), wCwStart = wN w }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wLam = min 160 (wLam w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wLam = max 6 (wLam w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wLam = min 160 (wLam w + 10) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wLam = max 6 (wLam w - 10) }
  _ -> w

-- ---- 自己テスト（小さな格子で。p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Courant condition c*dt/dx = " ++ showJ sc ++ " <= 1/sqrt2", sc <= sqrt 0.5)
  , ("pulse travels at light speed (S cells per step, within 3%): " ++ fmt 3 (v / sc) ++ " c", nearRel v sc 0.03)
  , ("Mur boundary: after the wave leaves, energy < 3% of the start", eEnd < 0.03 * eStart)
  , ("inside a metal box energy neither grows nor shrinks (averages within 1%)", first > 0 && nearRel lastE first 0.01)
  , ("period of the (1,1) standing wave in a metal box = formula (within 1%)", length cr > 3 && nearRel tm tth 0.01)
  , ("with S = 0.9 (> 1/sqrt2) the numbers blow up (that's why the condition is needed)", not (blowE < 1e6)) ]
  where
    -- (a) 波の速さ：ガウスの山から広がる波の山が x = 30 と 60 を通る時刻
    nG = 181
    c0 = 90
    gauss = U.generate (nG * nG) (\k -> let (j, i) = k `divMod` nG in exp (-(fromIntegral ((i - c0) ^ (2 :: Int) + (j - c0) ^ (2 :: Int))) / (2 * 6 * 6)))
    tA = peakTime nG nG gauss (c0 + 30) c0 400
    tB = peakTime nG nG gauss (c0 + 60) c0 400
    v = 30 / (tB - tA)
    -- (b) 吸いこむ境界：枠の外へ出たあと、残るエネルギーは少し
    (eStart, eEnd) = runST $ do
      g <- newGrid 101 101 (U.replicate (101 * 101) 0)
      blob g 50 50 3
      forM_ [1 .. 40 :: Int] $ \_ -> gStep g sc True
      e0 <- energy g
      forM_ [1 .. 600 :: Int] $ \_ -> gStep g sc True
      e1 <- energy g
      pure (e0, e1)
    -- (c) 金属の箱：エネルギーは増えも減りもしない（電場と磁場は半歩ずれているので平均で比べる）
    boxMetal = U.generate (41 * 31) (\k -> let (j, i) = k `divMod` 41 in if i == 0 || i == 40 || j == 0 || j == 30 then 1 else 0)
    (first, lastE) = runST $ do
      g <- newGrid 41 31 boxMetal
      blob g 12 15 2.5
      let go k (f, l) | k >= (4000 :: Int) = pure (f, l)
                      | otherwise = do
                          gStep g sc False
                          e <- energy g
                          go (k + 1) (if k < 500 then f + e / 500 else f, if k >= 3500 then l + e / 500 else l)
      go 0 (0, 0)
    -- (d) 共振器の (1,1) モード：周期 = 2/(S·√((1/Lx)² + (1/Ly)²)) 歩
    cr = runST $ do
      g <- newGrid 41 31 boxMetal
      forM_ [1 .. 29] $ \j -> forM_ [1 .. 39] $ \i ->
        M.write (gEz g) (j * 41 + i) (sin ((pi * fromIntegral i) / 40) * sin ((pi * fromIntegral j) / 30))
      let go k prev acc | k >= (1500 :: Int) = pure (reverse acc)
                        | otherwise = do
                            gStep g sc False
                            e <- M.read (gEz g) (15 * 41 + 20)
                            go (k + 1) e (if prev > 0 && e <= 0 then (fromIntegral k + prev / (prev - e)) : acc else acc)
      go 0 1 []
    tm = case cr of
      (c1 : _ : _) -> (last cr - c1) / fromIntegral (length cr - 1)
      _ -> 0
    tth = 2 / (sc * sqrt (1 / 1600 + 1 / 900))
    -- (e) クーラン条件をやぶると発散する
    blowE = runST $ do
      g <- newGrid 30 30 (U.replicate 900 0)
      forM_ [0 .. 899] $ \k -> M.write (gEz g) k (sin (fromIntegral k * 12.9898) * 0.01)
      forM_ [1 .. 300 :: Int] $ \_ -> gStep g 0.9 False
      energy g

-- 波の山がその点を通る時刻（放物線で細かく）
peakTime :: Int -> Int -> U.Vector Double -> Int -> Int -> Int -> Double
peakTime gnx gny ez0 i j nSteps = fromIntegral bt + (0.5 * (a - c)) / (a - 2 * b + c)
  where
    vals = runST $ do
      g <- newGrid gnx gny (U.replicate (gnx * gny) 0)
      forM_ [0 .. gnx * gny - 1] $ \k -> M.write (gEz g) k (U.unsafeIndex ez0 k)
      U.generateM nSteps (\_ -> gStep g sc True >> M.read (gEz g) (j * gnx + i))
    (_, bt) = foldl' (\(best, t) k -> let x = vals U.! k in if x > best then (x, k) else (best, t)) (-1, 0) [1 .. nSteps - 2]
    a = vals U.! (bt - 1)
    b = vals U.! bt
    c = vals U.! (bt + 1)

-- ---------- やってみよう ----------
-- 1. 1 でクリックしてパルスを出し、円がどれくらいの速さで広がるか確かめよう（1 フレーム 3 歩 × 0.5 マス）
-- 2. 2 でアンテナを ON。壁の前にできる定在波の節の間隔は λ/2 になっている？
-- 3. 3 の共振器で ↑↓ を押し、(1,1) や (2,1) の波長に合わせると波が大きく育つ（電子レンジの中もこれ）
-- 4. 4 のすき間で、波長を 6 cm と 40 cm にして回りこみ方を比べよう
-- 5. sc を 0.75 にするとどうなる？（自己テストの最後の項目と同じことが起きる）


-- ==================================================
-- ここから下は「電磁気の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 自己テスト用の小さな格子（書きかえられる配列）
data Grid s = Grid { gNx, gNy :: !Int, gEz, gHx, gHy :: !(M.MVector s Double), gMetal :: !(U.Vector Word8) }

newGrid :: Int -> Int -> U.Vector Word8 -> ST s (Grid s)
newGrid a b metal = Grid a b <$> M.replicate (a * b) 0 <*> M.replicate (a * b) 0 <*> M.replicate (a * b) 0 <*> pure metal

gStep :: Grid s -> Double -> Bool -> ST s ()
gStep g s mur = fdtdStep (gNx g) (gNy g) (gMetal g) (gEz g) (gHx g) (gHy g) s mur

blob :: Grid s -> Double -> Double -> Double -> ST s ()
blob g ci cj sg = forM_ [1 .. gNy g - 2] $ \j -> forM_ [1 .. gNx g - 2] $ \i ->
  M.write (gEz g) (j * gNx g + i) (exp (-((fromIntegral i - ci) ** 2 + (fromIntegral j - cj) ** 2) / (2 * sg * sg)))

-- 場のエネルギー（の目安）Σ Ez² + Hx² + Hy²
energy :: Grid s -> ST s Double
energy g = go 0 0
  where
    n = gNx g * gNy g
    go k acc | k >= n = pure acc
             | otherwise = do
                 e <- M.unsafeRead (gEz g) k
                 a <- M.unsafeRead (gHx g) k
                 b <- M.unsafeRead (gHy g) k
                 go (k + 1) (acc + e * e + a * a + b * b)

-- ---- FDTD の 1 歩（Ez, Hx, Hy は「真空のインピーダンス」でそろえた単位、S = cΔt/Δx）。mur = True なら外の枠で吸いこむ
fdtdStep :: Int -> Int -> U.Vector Word8 -> M.MVector s Double -> M.MVector s Double -> M.MVector s Double -> Double -> Bool -> ST s ()
fdtdStep gx gy metal ez hx hy s mur = do
  -- 磁場を半歩
  loop 0 (gy - 1) $ \j -> loop 0 gx $ \i -> do
    let k = j * gx + i
    a <- M.unsafeRead ez (k + gx)
    b <- M.unsafeRead ez k
    M.unsafeModify hx (\h -> h - s * (a - b)) k
  loop 0 gy $ \j -> loop 0 (gx - 1) $ \i -> do
    let k = j * gx + i
    a <- M.unsafeRead ez (k + 1)
    b <- M.unsafeRead ez k
    M.unsafeModify hy (\h -> h + s * (a - b)) k
  old <- if mur then U.freeze ez else pure U.empty
  -- 電場を半歩
  loop 1 (gy - 1) $ \j -> loop 1 (gx - 1) $ \i -> do
    let k = j * gx + i
    if U.unsafeIndex metal k /= 0 then M.unsafeWrite ez k 0 else do
      e <- M.unsafeRead ez k
      y0 <- M.unsafeRead hy k
      y1 <- M.unsafeRead hy (k - 1)
      x0 <- M.unsafeRead hx k
      x1 <- M.unsafeRead hx (k - gx)
      M.unsafeWrite ez k (e + s * (y0 - y1 - x0 + x1))
  -- ムーアの 1 次の吸収境界（old = 電場を更新する前の値）
  when mur $ do
    let a = (s - 1) / (s + 1)
        o k = U.unsafeIndex old k
    loop 0 gy $ \j -> do
      e1 <- M.unsafeRead ez (j * gx + 1)
      M.unsafeWrite ez (j * gx) (o (j * gx + 1) + a * (e1 - o (j * gx)))
      e2 <- M.unsafeRead ez (j * gx + gx - 2)
      M.unsafeWrite ez (j * gx + gx - 1) (o (j * gx + gx - 2) + a * (e2 - o (j * gx + gx - 1)))
    loop 0 gx $ \i -> do
      e1 <- M.unsafeRead ez (gx + i)
      M.unsafeWrite ez i (o (gx + i) + a * (e1 - o i))
      e2 <- M.unsafeRead ez ((gy - 2) * gx + i)
      M.unsafeWrite ez ((gy - 1) * gx + i) (o ((gy - 2) * gx + i) + a * (e2 - o ((gy - 1) * gx + i)))

-- a 以上 b 未満の整数でくり返す
loop :: Int -> Int -> (Int -> ST s ()) -> ST s ()
loop a b f = go a
  where go !k | k >= b = pure ()
              | otherwise = f k >> go (k + 1)
{-# INLINE loop #-}

-- ---- 数の道具
-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- JavaScript の数の文字列（整数なら小数点なし）
showJ :: Double -> String
showJ v | isNaN v || isInfinite v = "0"
        | v == fromIntegral (round v :: Integer) && abs v < 1e15 = show (round v :: Integer)
        | otherwise = showFFloat Nothing v ""

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点）で
mouseP5 :: Env -> (Float, Float)
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 幅 maxW に収まるように単語の切れ目で折り返す
wordWrap :: Float -> Float -> String -> [String]
wordWrap maxW size s = reverse (go (words s) "" [])
  where
    go [] cur acc = if null cur then acc else cur : acc
    go (x : xs) cur acc
      | null cur = go xs x acc
      | fromIntegral (length cur + 1 + length x) * size * 0.62 <= maxW = go xs (cur ++ " " ++ x) acc
      | otherwise = go xs x (cur : acc)

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 相対誤差 rel 以内
nearRel :: Double -> Double -> Double -> Bool
nearRel a b rel = near a b (abs b * rel)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if not (null rs) && all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 画面の左下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 12) 14 (if not (null rs) && all snd rs then rgb255 120 220 160 else rgb255 255 100 90)
