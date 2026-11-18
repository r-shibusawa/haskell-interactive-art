-- ============================================
-- 55-11 イジング模型 ─ 小さな磁石（スピン ↑↓）の集まりが、温度を下げると突然そろう（相転移）
-- エネルギー E = −J Σ(となり同士の積) − h Σ s。メトロポリス法：1 つのスピンを反転してみて、
-- E が下がれば必ず、上がるなら確率 e^(−ΔE/kT) で受け入れる（市松模様の順に、全部を 1 回ずつ = 1 スイープ）
-- 臨界温度 Tc = 2/ln(1 + √2) ≈ 2.269（オンサーガーの厳密解、J = k = 1）の近くで、ゆらぎがとても大きくなる
-- 操作：← → 温度 / 下のバーをクリック・ドラッグ / ↑↓ 外部磁場 h / スペース 一時停止 / 1 全部 ↑ / R ばらばら
--       A 温度をゆっくり下げる / C 記録を消す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

lsize :: Int
lsize = 128                -- ★ 1 辺のスピンの数

sweeps :: Int
sweeps = 2                 -- ★ 1 コマのスイープ数

tcrit :: Double
tcrit = 2 / log (1 + sqrt 2)

-- 温度ごとの記録：回数、|m| の合計、m² の合計
data Rec = Rec { rN :: !Int, rAm, rM2 :: !Double }

data World = World
  { wSpin    :: !(U.Vector Int)      -- スピン（+1 / −1）
  , wRng     :: !Rng                 -- 乱数の状態（コマをまたいで使う）
  , wTemp    :: !Double              -- ★ 温度 kT/J
  , wH       :: !Double              -- ★ 外部磁場 h
  , wPaused  :: !Bool
  , wScan    :: !Bool                -- ゆっくり冷やしているか
  , wLogM    :: [(Double, Double)]   -- (磁化 m, E/N)（新しい順、300 個まで）
  , wRec     :: !(M.Map String Rec)  -- 温度（小数 2 桁の文字）ごとの記録
  , wTable   :: !(U.Vector Double)   -- 受け入れる確率の表
  , wTableT  :: !Double
  , wTableH  :: !Double
  , wFrames  :: !Int                 -- いまの温度になってからのコマ数
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "55-11 Ising model" world0)
  { skBack  = rgbC 20 20 26
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World sp r1 2.0 0 False False [] M.empty (U.replicate 10 0) (-1) 99 0 []
  where (sp, r1) = makeSpins lsize False (seedRng 11)

-- 全部 ↑（up = True）か、ばらばら
makeSpins :: Int -> Bool -> Rng -> (U.Vector Int, Rng)
makeSpins n up r0
  | up = (U.replicate (n * n) 1, r0)
  | otherwise = let (vs, r') = rndList (n * n) r0 in (U.fromList [ if v < 0.5 then 1 else -1 | v <- vs ], r')

-- 受け入れる確率の表：となり 4 つの和 nb（−4〜4）と自分 s から ΔE = 2s(nb + h)
makeTable :: Double -> Double -> U.Vector Double
makeTable t h = U.fromList [ if de <= 0 then 1 else exp (-de / t)
                           | k <- [0 .. 4 :: Int], si <- [0, 1 :: Int]
                           , let nb = fromIntegral (2 * k - 4); s = if si == 1 then 1 else -1; de = 2 * s * (nb + h) ]

-- 市松模様の 2 色ずつ更新（周期境界）。乱数は「受け入れが 1 未満のときだけ」引く
sweepM :: Int -> U.Vector Double -> UM.MVector s Int -> Rng -> ST s Rng
sweepM n a s r0 = goP 0 r0
  where
    goP p !r | p >= 2 = pure r
             | otherwise = goY p 0 r >>= goP (p + 1)
    goY p y !r | y >= n = pure r
               | otherwise = goX p y ((y + p) `mod` 2) r >>= goY p (y + 1)
    goX p y x !r
      | x >= n = pure r
      | otherwise = do
          let up = ((y - 1 + n) `mod` n) * n; dn = ((y + 1) `mod` n) * n; row = y * n; i = row + x
          a1 <- UM.unsafeRead s (row + (x + 1) `mod` n)
          a2 <- UM.unsafeRead s (row + (x - 1 + n) `mod` n)
          a3 <- UM.unsafeRead s (up + x)
          a4 <- UM.unsafeRead s (dn + x)
          si <- UM.unsafeRead s i
          let pa = U.unsafeIndex a (a1 + a2 + a3 + a4 + 4 + (if si > 0 then 1 else 0))
          if pa >= 1 then UM.unsafeWrite s i (negate si) >> goX p y (x + 2) r
          else do let (v, r') = rnd r
                  when (v < pa) (UM.unsafeWrite s i (negate si))
                  goX p y (x + 2) r'

-- k スイープまとめて
sweepN :: Int -> Int -> U.Vector Double -> (U.Vector Int, Rng) -> (U.Vector Int, Rng)
sweepN k n a (s0, r0) = runST $ do
  s <- U.thaw s0
  let go 0 !r = pure r
      go j !r = sweepM n a s r >>= go (j - 1 :: Int)
  r' <- go k r0
  s' <- U.freeze s
  pure (s', r')

magnet :: U.Vector Int -> Double
magnet s = fromIntegral (U.sum s) / fromIntegral (U.length s)

energyPer :: U.Vector Int -> Int -> Double -> Double
energyPer s n h = foldl' f 0 [0 .. n * n - 1] / fromIntegral (n * n)
  where f e i = let (y, x) = i `divMod` n
                    si = s U.! i
                in e - (fromIntegral (si * (s U.! (y * n + (x + 1) `mod` n) + s U.! (((y + 1) `mod` n) * n + x))) + h * fromIntegral si)

-- 自発磁化の厳密解
onsagerM :: Double -> Double
onsagerM t = if t < tcrit then (1 - sinh (2 / t) ** (-4)) ** (1 / 8) else 0

-- 小数 2 桁に丸める（Math.round(v * 100) / 100）
r2 :: Double -> Double
r2 v = jsRound (v * 100) / 100

step :: Env -> World -> World
step env w0 = drag (run (table scan))
  where
    scan | wScan w0 && envFrame env `mod` 150 == 0 =
             let t = r2 (wTemp w0 - 0.05) in if t < 1.5 then w0 { wTemp = 1.5, wScan = False } else w0 { wTemp = t }
         | otherwise = w0
    table w | wTableT w /= wTemp w || wTableH w /= wH w = w { wTable = makeTable (wTemp w) (wH w), wTableT = wTemp w, wTableH = wH w, wFrames = 0 }
            | otherwise = w
    run w | wPaused w = w
          | otherwise =
              let (s, r) = sweepN sweeps lsize (wTable w) (wSpin w, wRng w)
                  fr = wFrames w + 1
                  m = magnet s
                  e = energyPer s lsize (wH w)
                  rec' | fr > 40 && wH w == 0 = M.alter (Just . add m) (fmt 2 (wTemp w)) (wRec w)   -- 温度ごとに |m| と m² の平均を記録
                       | otherwise = wRec w
              in w { wSpin = s, wRng = r, wFrames = fr, wLogM = take 300 ((m, e) : wLogM w), wRec = rec' }
    add m Nothing = Rec 1 (abs m) (m * m)
    add m (Just (Rec n a b)) = Rec (n + 1) (a + abs m) (b + m * m)
    -- 温度のバーをドラッグ
    drag w = let (mx, my) = mouseP env
                 L sz ox _ sy = layout env
             in if envDown env && my > sy - 15 && my < sy + 25 && mx >= ox && mx <= ox + sz
                  then w { wTemp = r2 (clampv 0.5 5 (0.5 + realToFrac ((mx - ox) / sz) * 4.5)) } else w

-- 画面の配置：絵の大きさ、左、上、バーの y
data L = L !Float !Float !Float !Float

layout :: Env -> L
layout env = let sz = min (envW env * 0.45) (envH env - 210) in L sz 20 110 (110 + sz + 26)

mouseP :: Env -> (Float, Float)
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- スピンの絵（オレンジ = ↑、青 = ↓）
  [ at (ox + sz / 2, oy + sz / 2) (Scale 1 (-1) (pixelImage lsize lsize (sz / fromIntegral lsize)
      (\x y -> if wSpin w U.! (y * lsize + x) > 0 then (255, 150, 70) else (40, 90, 200)))) ]
  -- 温度のバー
  ++ [ Color (grayC 60) (rectTL ox sy sz 10)
     , Color (rgbC 255 210 120) (at (ox + f2 ((wTemp w - 0.5) / 4.5) * sz, sy + 5) (disc 9))
     , Color (rgbC 255 120 120) (thickLine 2 (tcx, sy - 6) (tcx, sy + 16)) ]
  -- ---- グラフ 1：m(t) と E/N
  ++ [ plotAxes g1 "time ->" "magnetization m (orange) and energy E/N (blue)" [] [-2, -1, 0, 1]
     , Color (rgbC 255 160 80) (polyline 2 [ (gX g1 (fromIntegral k), gY g1 m) | (k, (m, _)) <- zip [0 :: Int ..] lg ])
     , Color (rgbC 110 170 255) (polyline 2 [ (gX g1 (fromIntegral k), gY g1 e) | (k, (_, e)) <- zip [0 :: Int ..] lg ]) ]
  -- ---- グラフ 2：温度ごとの |m| と、ゆらぎ（帯磁率）
  ++ [ plotAxes g2 "temperature kT/J" "mean |m| (dots) and Onsager's exact solution (line)" [1, 2, roundSig 4 tcrit, 3, 4] [0, 0.5, 1]
     , Color (rgbC 130 200 255) (polyline 2 [ (gX g2 t, gY g2 (onsagerM t)) | k <- [0 .. 300 :: Int], let t = 1 + fromIntegral k / 300 * 3 ])
     , Color (rgbaC 255 120 120 120) (Line [(gX g2 tcrit, pY g2), (gX g2 tcrit, pY g2 + pH g2)]) ]
  ++ concat [ [ Color (rgbC 255 160 80) (at (gX g2 t, gY g2 am) (disc 3.5))
              , Color (rgbC 200 130 255) (at (gX g2 t, gY g2 (chi / chiMax * 0.95)) (disc 2.5)) ]
            | (t, am, chi) <- recs ]
  ++ [ say "Purple = susceptibility chi = N(<m^2> - <|m|>^2)/T (max = 1)" (pX g2 + 8) (pY g2 + 18) 14 (rgbC 200 130 255) ]
  -- ---- 文字
  ++ [ say ("Ising model " ++ show lsize ++ " x " ++ show lsize ++ "   kT/J = " ++ fmt 2 (wTemp w)
            ++ (if wTemp w < tcrit then " (below Tc: aligns)" else " (above Tc: random)") ++ "   field h = " ++ fmt 2 (wH w)
            ++ "   m = " ++ fmt 3 mNow) 20 36 18 (grayC 240)
     , say ("Left/Right temp   drag bar   Up/Down field   Space pause   1 all up   R random   A slow cooling"
            ++ (if wScan w then " (on)" else "") ++ "   C clear") 20 62 15 (grayC 175)
     , say ("Orange = up spin, blue = down spin. Near Tc ~ " ++ fmt 3 tcrit ++ " (red line) clusters of all sizes come and go") 20 86 15 (grayC 175) ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    L sz ox oy sy = layout env
    tcx = ox + f2 ((tcrit - 0.5) / 4.5) * sz
    lg = reverse (wLogM w)
    mNow = case wLogM w of
             ((m, _) : _) -> m
             [] -> 0
    gx = ox + sz + 70
    gw = envW env - gx - 30
    gh = (envH env - 290) / 2
    g1 = graph gx 120 gw gh 0 300 (-2.1) 1.05
    g2 = graph gx (120 + gh + 80) gw gh 1 4 0 1.05
    nn = fromIntegral (lsize * lsize)
    recs = [ (t, am, chi) | (k, Rec n a b) <- M.toList (wRec w), n > 0
           , let t = read k :: Double; am = a / fromIntegral n; chi = nn * (b / fromIntegral n - am * am) / t ]
    chiMax = foldl' (\c (_, _, x) -> max c x) 1e-9 recs

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wTemp = min 5 (r2 (wTemp w + 0.05)) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wTemp = max 0.5 (r2 (wTemp w - 0.05)) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wH = min 1 (r2 (wH w + 0.05)) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wH = max (-1) (r2 (wH w - 0.05)) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c == '1' -> let (s, r) = makeSpins lsize True (wRng w) in w { wSpin = s, wRng = r }
    | c `elem` "rR" -> let (s, r) = makeSpins lsize False (wRng w) in w { wSpin = s, wRng = r }
    | c `elem` "aA" -> if wScan w then w { wScan = False }
                       else let (s, r) = makeSpins lsize False (wRng w) in w { wScan = True, wTemp = 3.5, wSpin = s, wRng = r }
    | c `elem` "cC" -> w { wRec = M.empty }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest =
  [ ("all up: E/N = -2J, checkerboard: +2J", near (energyPer s0 n 0) (-2) 1e-9 && near (energyPer checker n 0) 2 1e-9)
  , ("detailed balance: ratio of acceptance = e^(-dE/kT) (dE = 4, 8)"
    , near (a U.! 7 / a U.! 3) (exp (-4 / 1.7)) 1e-12 && near (a U.! 9) (exp (-8 / 1.7)) 1e-12 && a U.! 1 == 1)
  , ("kT -> 0: stays in the all-up ground state (m = 1)", magnet s1 == 1)
  , ("kT = 10 (hot): <|m|> = " ++ fmt 3 amHot ++ " is small (< 0.1)", amHot < 0.1)
  , ("Onsager: Tc = 2/ln(1 + sqrt 2) = 2.2692, M(kT = 2) = 0.9113", near tcrit 2.26919 1e-5 && near (onsagerM 2) 0.91132 1e-4)
  , ("Metropolis at kT = 1.8: <|m|> = " ++ fmt 3 amC ++ " ~ exact " ++ fmt 3 (onsagerM 1.8) ++ " (+-0.03)", abs (amC - onsagerM 1.8) < 0.03) ]
  where
    n = 32
    rg0 = seedRng 111
    (s0, rg1) = makeSpins n True rg0
    checker = U.generate (n * n) (\i -> if ((i `mod` n) + i `div` n) `mod` 2 == 1 then 1 else -1)
    a = makeTable 1.7 0
    (s1, rg2) = sweepN 50 n (makeTable 0.01 0) (s0, rg1)
    -- 400 スイープ、100 回目から |m| を平均
    avgAbsM t st = let th = makeTable t 0
                       go (!acc, cur) k = let cur'@(s, _) = sweepN 1 n th cur
                                          in (if k >= 100 then acc + abs (magnet s) / 300 else acc, cur')
                   in foldl' go (0, st) [0 .. 399 :: Int]
    (hotS, rg3) = makeSpins n True rg2
    (amHot, (_, rg4)) = avgAbsM 10 (hotS, rg3)
    (cS, rg5) = makeSpins n True rg4
    (amC, _) = avgAbsM 1.8 (cS, rg5)

-- ---------- やってみよう ----------
-- 1. R でばらばらにして、温度 1.5 にしよう。かたまり（磁区）がだんだん大きくなる様子を見よう
-- 2. A でゆっくり冷やすと、Tc ≈ 2.27 のあたりで |m| が急に立ち上がる。点は厳密解の線に乗る？
-- 3. 紫の点（帯磁率 = ゆらぎ）は、どの温度でいちばん大きい？
-- 4. Tc より低い温度で ↑↓ の磁場をかけると、向きがそろって反転する？（ヒステリシス）
-- 5. lsize を 32 にすると、Tc の近くのふるまいはどう変わる？（有限の大きさの効果）

-- ==================================================
-- ここから下は「熱力学の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
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

-- 乱数を n 個まとめて
rndList :: Int -> Rng -> ([Double], Rng)
rndList n r0 = go n r0 []
  where go 0 r acc = (reverse acc, r)
        go k r acc = let (v, r') = rnd r in go (k - 1) r' (v : acc)

-- ---- グラフの道具（左上が原点、y は下向き）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double, logX, logY :: !Bool }

graph :: Float -> Float -> Float -> Float -> Double -> Double -> Double -> Double -> G
graph x y w h a b c d = G x y w h a b c d False False

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv (-0.02) 1.02 t) * pW g
  where t | logX g = (log v - log (x0 g)) / (log (x1 g) - log (x0 g))
          | otherwise = (v - x0 g) / (x1 g - x0 g)

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv (-0.02) 1.02 t) * pH g
  where t | logY g = (log v - log (y0 g)) / (log (y1 g) - log (y0 g))
          | otherwise = (v - y0 g) / (y1 g - y0 g)

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (grayC 95) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 12) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 34) 14 (grayC 205), say yl (pX g) (pY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 2 色の間（lerpColor）
lerpC :: (Float, Float, Float) -> (Float, Float, Float) -> Double -> Color
lerpC (r1, g1, b1) (r2, g2, b2) t = makeColor (l r1 r2) (l g1 g2) (l b1 b2) 1
  where l a b = (a + (b - a) * f2 t) / 255

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

hypot :: Double -> Double -> Double
hypot a b = sqrt (a * a + b * b)

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 n 桁に丸めた数（toPrecision）
roundSig :: Int -> Double -> Double
roundSig n v0'
  | v == 0 = 0
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (n - 1 - e) :: Double
                in fromIntegral (round (v * k) :: Integer) / k
  where v = fin 0 v0'

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v = let r = roundSig 3 v
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (jsRoundI v)

jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

-- 指数表記（toExponential）
expo :: Int -> Double -> String
expo n v = let s = showEFloat (Just n) (fin 0 v) ""
               (a, b) = break (== 'e') s
           in case b of
                ('e' : '-' : _) -> s
                ('e' : rest) -> a ++ "e+" ++ rest
                _ -> s

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

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
