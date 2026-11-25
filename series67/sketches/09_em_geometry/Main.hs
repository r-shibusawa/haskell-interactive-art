-- ============================================
-- 67-09 EM 法は 2 つの射影 ─ 2 つの正規分布の混合 w1 N(μ1, 1) + w2 N(μ2, 1) を 1 次元のデータに当てはめる（未知は μ1, μ2 だけ）
-- E ステップ = e-射影：いまのモデルから「データに合う分布の集まり」へ。各点がどちらの山から来たかの確率 r_ik を計算
-- M ステップ = m-射影：その r_ik を重みにして平均を取り直す（モデルの集まりへ戻る）。くり返すたびに対数尤度 log L は必ず増えるか同じ
-- 自由エネルギー F = log L − Σ KL(r_i‖事後確率_i)：E で F = log L（すき間 0）、M で F が上がる。（クラスタリングとしての EM はシリーズ 61 の 08）
-- 操作：クリック 右の平面で出発点 / スペース 半歩（E か M）/ A 自動 / D 新しいデータ / R 出発点にもどる
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (orange, violet, rose)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8, Word32)
import Numeric (log1p, showEFloat)
import System.Environment (lookupEnv)

wMix :: (Double, Double)
wMix = (0.4, 0.6)                             -- ★ 2 つの山の重み（既知とする）

trueMu :: (Double, Double)
trueMu = (-1.5, 2)                            -- ★ データを作る本当の平均

nData :: Int
nData = 150

type Mu = (Double, Double)

data World = World
  { wXs     :: U.Vector Double                -- データ
  , wMu     :: !Mu                            -- いまの (μ1, μ2)
  , wStart  :: !Mu                            -- 出発点
  , wResp   :: U.Vector (Double, Double)      -- r_i1, r_i2
  , wPhaseE :: !Bool                          -- 次は E ステップか
  , wPath   :: [Mu]                           -- EM の道（古い順）
  , wLogLs  :: [Double]                       -- そのときの log L
  , wAuto   :: !Bool
  , wSeedD  :: !Int                           -- データの種
  , wField  :: Maybe Field                    -- log L の等高線
  , wFKey   :: !(Int, Float, Float)           -- 等高線を作ったときの (種, 幅, 高さ)
  , wTick   :: !Int
  , wTest   :: [Check] }

main :: IO ()
main = runSketch (sketch "67-09 EM as two projections" world0)
  { skBack  = col bgC
  , skStart = \env -> do
      printSelfTest selfTest
      pure (refield env world0 { wTest = selfTest })
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = restart (World xs0 (-3.8, -2.5) (-3.8, -2.5) U.empty True [] [] True 9 Nothing (-1, 0, 0) 0 [])
  where xs0 = makeData 9

-- データを作る：種 seedD から、重み W_MIX で山を選んで N(μ_c, 1)
makeData :: Int -> U.Vector Double
makeData s = U.fromList $ withSeed s $ mapM (\_ -> do
  u <- rnd
  g <- gaussRnd
  pure ((if u < fst wMix then fst trueMu else snd trueMu) + g)) [1 .. nData]

restart :: World -> World
restart w = let mu = wStart w
            in w { wMu = mu, wResp = posterior (wXs w) mu, wPhaseE = True, wPath = [mu], wLogLs = [logLik (wXs w) mu] }

npdf :: Double -> Double -> Double
npdf x m = exp (-0.5 * (x - m) * (x - m)) / sqrt (2 * pi)

logLik :: U.Vector Double -> Mu -> Double
logLik xs (m1, m2) = U.foldl' (\s x -> s + log (max 1e-300 (fst wMix * npdf x m1 + snd wMix * npdf x m2))) 0 xs

-- ★ E ステップ：各点がどちらの山から来たかの確率
posterior :: U.Vector Double -> Mu -> U.Vector (Double, Double)
posterior xs (m1, m2) = U.map (\x -> let a = fst wMix * npdf x m1
                                         b = snd wMix * npdf x m2
                                         s = max (a + b) 1e-300
                                     in (a / s, b / s)) xs

-- ★ M ステップ：重みつき平均
mStep :: U.Vector Double -> U.Vector (Double, Double) -> Mu
mStep xs r = (wavg fst, wavg snd)
  where wavg sel = let (sw, sx) = U.foldl' (\(!a, !b) (ri, x) -> (a + sel ri, b + sel ri * x)) (0, 0) (U.zip r xs)
                   in if sw > 1e-12 then sx / sw else 0

-- F(r, μ) = Σ_i Σ_k r_ik log(w_k N(x_i; μ_k) / r_ik)
freeEnergy :: U.Vector Double -> U.Vector (Double, Double) -> Mu -> Double
freeEnergy xs r (m1, m2) = U.foldl' term 0 (U.zip r xs)
  where term s ((r1, r2), x) = let s1 = if r1 > 0 then s + r1 * log (max 1e-300 (fst wMix * npdf x m1) / r1) else s
                               in if r2 > 0 then s1 + r2 * log (max 1e-300 (snd wMix * npdf x m2) / r2) else s1

halfStep :: World -> World
halfStep w
  | wPhaseE w = w { wResp = posterior (wXs w) (wMu w), wPhaseE = False }
  | otherwise = let mu = mStep (wXs w) (wResp w)
                    path = wPath w ++ [mu]
                    ls = wLogLs w ++ [logLik (wXs w) mu]
                    cut xs = if length xs > 200 then drop 1 xs else xs
                in w { wMu = mu, wPhaseE = True, wPath = cut path, wLogLs = cut ls }

plane :: Env -> PBox
plane env = let s = min (d2 (envW env) * 0.38) (d2 (envH env) * 0.62)
            in plotBox (d2 (envW env) * 0.56) 110 s s (-4) 4 (-4) 4

-- Σ_m log(ta[ia + m] + tb[ib + m])（等高線用の速いループ。描く範囲では和が 1e-300 より小さくならないので、
-- logLik の max(1e-300, …) は省いた。GHC では log(max …) がとても遅くなるため）
sumLogs :: U.Vector Double -> U.Vector Double -> Int -> Int -> Int -> Double
sumLogs ta tb ia ib n = go 0 0
  where go :: Int -> Double -> Double
        go !m !s = if m >= n then s
                   else go (m + 1) (s + log (U.unsafeIndex ta (ia + m) + U.unsafeIndex tb (ib + m)))

-- 等高線は データか画面の大きさが変わったときだけ作り直す
refield :: Env -> World -> World
refield env w
  | wFKey w == key = w
  | otherwise = w { wFKey = key, wField = Just (llField (plane env) (wXs w)) }
  where key = (wSeedD w, envW env, envH env)

-- log L の等高線（gridField と同じ絵）。log L(μ1, μ2) = Σ log(w1 N(x; μ1) + w2 N(x; μ2)) なので、
-- 列ごとの w1 N(x; μ1) と行ごとの w2 N(x; μ2) を先に表にしておくと、exp の計算がずっと少なくてすむ
llField :: PBox -> U.Vector Double -> Field
llField pb xs =
  let opt = fieldOpt { foLevel = 0.4, foVmax = 6, foCap = 5.8, foC0 = (60, 85, 125), foC1 = (24, 34, 54), foRes = 4 }
      res = 4 :: Int
      gw = ceiling (pbW pb / i2d res) :: Int
      gh = ceiling (pbH pb / i2d res) :: Int
      n = U.length xs
      topL = logLik xs trueMu + 5
      uOf i = fst (fromScr pb (pbX pb + (i2d i + 0.5) * i2d res) (pbY pb))
      vOf j = snd (fromScr pb (pbX pb) (pbY pb + (i2d j + 0.5) * i2d res))
      ta = U.generate (gw * n) (\k -> let (i, m) = k `divMod` n in fst wMix * npdf (xs U.! m) (uOf i))
      tb = U.generate (gh * n) (\k -> let (j, m) = k `divMod` n in snd wMix * npdf (xs U.! m) (vOf j))
      cell i j = sumLogs ta tb (i * n) (j * n) n
      vals = U.generate (gw * gh) (\k -> let (j, i) = k `divMod` gw in log1p (max 0 (topL - cell i j)))
      bandOf v = floor (clampv (v / foLevel opt) (-1e6) 1e6) :: Int
      pic = bandImage gw gh vals bandOf (\v -> v / foVmax opt) opt 235 170
  in vals `seq` Field pic (f2 (pbX pb)) (f2 (pbY pb)) (i2f (gw * res)) (i2f (gh * res)) gw gh

step :: Env -> World -> World
step env w0 =
  let w = refield env w0
  in if wAuto w then let t = wTick w + 1
                         w' = w { wTick = t }
                     in if t `mod` 20 == 0 then halfStep w' else w'
     else w

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawField (wField w)
  , hudTitle "EM = two projections: E step (e-projection) and M step (m-projection)"
             ("Click start point on the right plane   Space half step (next: " ++ (if wPhaseE w then "E" else "M") ++ " step)   A auto "
              ++ (if wAuto w then "ON" else "OFF") ++ "   D new data   R back to start")
  -- ---- 右：(μ1, μ2) 平面と log L の等高線、EM の道
  , drawAxes pb "mu1" "mu2  (contours of log L)"
  , Color (colA 120 dimc) (lineW 1 (toScrF pb (-4) (-4)) (toScrF pb 4 4))
  , plotLine pb (wPath w) (col orange) 2.5
  , Pictures [ let (x, y) = toScrF pb a b in Color (col orange) (circleAt x y 5) | (a, b) <- wPath w ]
  , let (cx, cy) = toScrF pb (fst (wMu w)) (snd (wMu w))
    in Pictures [Color (col bgC) (circleAt cx cy 18), Color (col (if wPhaseE w then orange else violet)) (circleAt cx cy 14)]
  , sayT "Two peaks across the diagonal mu1 = mu2 (labels swapped)." (f2 (pbX pb)) (f2 (pbY pb + pbH pb) + 34) 14 (col dimc) False
  , sayT "Weights differ, so the peaks have different heights." (f2 (pbX pb)) (f2 (pbY pb + pbH pb) + 52) 14 (col dimc) False
  -- ---- 左上：データのヒストグラムと混合分布
  , drawAxes b1 ("x (" ++ show nData ++ " data points, dot colour = share r_i1)") "density"
  , Color (col wire) $ Pictures
      [ rectTL x0 y0 (x1 - x0 - 1) (f2 (pbY b1 + pbH b1) - y0)
      | i <- [0 .. 47], let h = i2d (bins U.! i) / (i2d nData * 0.25)
      , let (x0, y0) = toScrF b1 (-6 + i2d i * 0.25) h, let (x1, _) = toScrF b1 (-6 + i2d (i + 1) * 0.25) 0 ]
  , plotLine b1 [ (x, fst wMix * npdf x m1) | x <- grid ] (col orange) 2
  , plotLine b1 [ (x, snd wMix * npdf x m2) | x <- grid ] (col violet) 2
  , plotLine b1 [ (x, fst wMix * npdf x m1 + snd wMix * npdf x m2) | x <- grid ] (col ink) 2.5
  , Pictures [ Color (col (mixC t)) (circleAt (fst (toScrF b1 x 0)) (f2 (pbY b1 + pbH b1) - 6 - i2f (i `mod` 5) * 4) 5)
             | (i, x) <- zip [0 :: Int ..] (U.toList (wXs w)), let t = if i < U.length (wResp w) then fst (wResp w U.! i) else 0.5 ]
  -- ---- 左下：log L と F
  , drawAxes b2 "iterations (number of M steps)" "log L: never decreases"
  , plotLine b2 (zip (map i2d [0 ..]) (wLogLs w)) (col lime) 2.5
  , say ("mu = (" ++ fmtN 3 m1 ++ ", " ++ fmtN 3 m2 ++ ")   log L = " ++ fmtN 4 lL ++ "   F = " ++ fmtN 4 fE) 70 ty 15 (col ink)
  , say (if not (wPhaseE w) then "Right after E: gap log L - F = " ++ fmtN 8 (lL - fE) ++ " (exactly 0)"
         else "Right after M: gap log L - F = " ++ fmtN 6 (lL - fE) ++ " (0 again after next E)") 70 (ty + 24) 15 (col (if wPhaseE w then orange else violet))
  , drawSelfTest env (wTest w) ]
  where
    pb = plane env
    (m1, m2) = wMu w
    b1 = plotBox 70 110 (d2 (envW env) * 0.42) (d2 (envH env) * 0.33) (-6) 6 0 0.45
    bins = U.accum (+) (U.replicate 48 (0 :: Int)) [ (i, 1) | x <- U.toList (wXs w), let i = floor ((x + 6) / 0.25), i >= 0, i < 48 ]
    grid = [ -6 + i2d k / 20 | k <- [0 .. 240 :: Int] ]
    mixC t = let (a0, a1, a2) = orange
                 (b0', b1', b2') = violet
                 tf = f2 t
             in (a0 * tf + b0' * (1 - tf), a1 * tf + b1' * (1 - tf), a2 * tf + b2' * (1 - tf))
    lL = logLik (wXs w) (wMu w)
    fE = freeEnergy (wXs w) (wResp w) (wMu w)
    ls = wLogLs w
    lo = minOr 0 ls - 1
    hi = maxOr 0 ls + 1
    b2 = plotBox 70 (d2 (envH env) * 0.6) (d2 (envW env) * 0.42) (d2 (envH env) * 0.25) 0 (max 10 (i2d (length ls - 1))) lo hi
    ty = f2 (pbY b2 + pbH b2) + 50

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | inRect mx my (pbBox pb) -> let (a, b) = fromScr pb mx my in restart w { wStart = (clampv a (-4) 4, clampv b (-4) 4) }
  EventKey (SpecialKey KeySpace) Down _ _ -> halfStep w { wAuto = False }
  EventKey (Char c) Down _ _
    | c == 'a' || c == 'A' -> w { wAuto = not (wAuto w) }
    | c == 'd' || c == 'D' -> let s = wSeedD w + 1 in restart w { wSeedD = s, wXs = makeData s }
    | c == 'r' || c == 'R' -> restart w
  _ -> w
  where pb = plane env
        (mx, my) = mouseD env

-- ---------- やってみよう ----------
-- 1. 対角線の向こう側（μ1 > μ2）から出発すると、どちらの山に着く？ log L は本当の答えより低い？
-- 2. 対角線のちょうど上から出発すると？（2 つの山が同じ平均のまま動けない「鞍点」）
-- 3. trueMu を (-0.5, 0.5) に近づけると、EM はゆっくりになる。等高線の形と比べよう
-- 4. wMix を (0.5, 0.5) にすると、2 つの山の高さは同じになる？
-- 5. 分散 σ も未知にすると、1 点に山が縮んで log L → ∞ になることがある。どう防ぐ？

-- ---- 自己テスト
selfTest :: [Check]
selfTest =
  [ ("10 か所から 60 回ずつ：log L は一度も減らない", all (\(a, _, _, _, _) -> a) runs)
  , ("E ステップの直後は F = log L（誤差 1e-9）、いつも F ≤ log L", all (\(_, b, c, _, _) -> b && c) runs)
  , ("M ステップの重みつき平均は F を最大にする（±0.001 ずらすと必ず下がる）", all (\(_, _, _, d, _) -> d) runs)
  , ("log L − F = Σ KL(r_i ‖ 事後確率_i)（誤差 1e-8）", all (\(_, _, _, _, e) -> e) runs)
  , ("収束した点では log L の傾き ≈ 0（" ++ fmtN 6 (abs g1 + abs g2) ++ "）、答え ≈ 本当の平均 (" ++ fmtN 2 c1 ++ ", " ++ fmtN 2 c2 ++ ")"
    , abs g1 + abs g2 < 1e-4 && abs (c1 - fst trueMu) < 0.5 && abs (c2 - snd trueMu) < 0.5) ]
  where
    xs = makeData 9
    starts = withSeed 99 (mapM (\_ -> do a <- rnd
                                         b <- rnd
                                         pure (a * 8 - 4, b * 8 - 4)) [1 .. 10 :: Int])
    runs = map oneRun starts
    -- 1 つの出発点から 60 回：(減らない, E の直後 F = log L, F ≤ log L, M が最大, 分解)
    oneRun mu0 = go (60 :: Int) mu0 (logLik xs mu0) (True, True, True, True, True)
    go 0 _ _ acc = acc
    go k mu prev (okMono, okGap0, okGap, okMmax, okDec) =
      let r = posterior xs mu
          g0 = abs (freeEnergy xs r mu - logLik xs mu) <= 1e-9
          mu2@(a, b) = mStep xs r
          fF2 = freeEnergy xs r mu2
          mmax = and [ freeEnergy xs r m3 <= fF2 | e <- [-1e-3, 1e-3], m3 <- [(a + e, b), (a, b + e)] ]
          gapOk = not (fF2 > logLik xs mu2 + 1e-9)
          post = posterior xs mu2
          kl = sumOf [ divKL [r1, r2] [q1, q2] | ((r1, r2), (q1, q2)) <- zip (U.toList r) (U.toList post) ]
          dec = abs (logLik xs mu2 - fF2 - kl) <= 1e-8
          now = logLik xs mu2
          mono = not (now < prev - 1e-9)
      in go (k - 1) mu2 now (okMono && mono, okGap0 && g0, okGap && gapOk, okMmax && mmax, okDec && dec)
    (c1, c2) = foldl' (\mu _ -> mStep xs (posterior xs mu)) (-3, 3) [1 .. 400 :: Int]
    h = 1e-5
    g1 = (logLik xs (c1 + h, c2) - logLik xs (c1 - h, c2)) / (2 * h)
    g2 = (logLik xs (c1, c2 + h) - logLik xs (c1, c2 - h)) / (2 * h)

-- ==================================================
-- ここから下は「情報幾何の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。確率分布 p = [p1, p2, ...]（合計 1）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（等高線地図の色）
type RGB = (Float, Float, Float)

bgC, ink, orange, violet, teal, rose, lime, sand, dimc, panel, wire :: RGB
bgC = (22, 33, 52)                            -- 背景（深いスレートブルー）
ink = (240, 236, 226)                         -- 文字（紙の白）
orange = (255, 150, 75)
violet = (170, 140, 255)
teal = (95, 205, 190)
rose = (255, 110, 125)
lime = (150, 225, 120)
sand = (235, 210, 150)
dimc = (140, 152, 175)
panel = (32, 46, 70)
wire = (62, 80, 110)

-- 結果ごとの色（8 色をくり返す）
outC :: Int -> RGB
outC i = at' [(255, 150, 75), (95, 205, 190), (170, 140, 255), (235, 210, 150), (255, 110, 125), (150, 225, 120), (120, 175, 255), (230, 140, 210)] (i `mod` 8) ink

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

fin :: Double -> Double -> Double             -- NaN や ∞ なら d
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float                         -- 描くときの Float（NaN・∞ は 0）
f2 = realToFrac . fin 0

d2 :: Float -> Double                         -- マウスの位置などを計算用の Double に
d2 = realToFrac

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- リストの i 番目を f で変える
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

setAt :: Int -> a -> [a] -> [a]
setAt i v = modifyAt i (const v)

-- 厳密にリストを評価する（遅延評価の「つけ」をためない）
forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Int
jsRound v0 = let v = clampv v0 (-1e15) 1e15
                 r = floor v
             in if v - fromIntegral r >= 0.5 then r + 1 else r

-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）。∞ は inf
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = if d > 0 then ip ++ "." ++ fp else ip
                in if n == 0 || v > 0 then body else '-' : body

-- fmtP(p, d)：(0.50, 0.30, 0.20) の形
fmtP :: Int -> [Double] -> String
fmtP d p = "(" ++ intercalate ", " (map (fmtN d) p) ++ ")"

-- v.toExponential(1)：1.2e-15 の形
fmtE :: Double -> String
fmtE v = if isNaN v || isInfinite v then fmtN 2 v else plusE (showEFloat (Just 1) v "")
  where plusE s = case break (== 'e') s of
          (m, 'e' : '-' : ex) -> m ++ "e-" ++ ex
          (m, 'e' : ex) -> m ++ "e+" ++ ex
          _ -> s

-- 左上のタイトルと操作の説明（説明は \n で改行、行の間 19）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- 長方形 r = {x, y, w, h}
data Box = Box { bxX :: !Double, bxY :: !Double, bxW :: !Double, bxH :: !Double }

inRect :: Double -> Double -> Box -> Bool
inRect x y r = x >= bxX r && x <= bxX r + bxW r && y >= bxY r && y <= bxY r + bxH r

-- 等高線の地図（作っておいた画像と、置く場所）
data Field = Field { fdPic :: Picture, fdX :: !Float, fdY :: !Float, fdW :: !Float, fdH :: !Float, fdIW :: !Int, fdIH :: !Int }

drawField :: Maybe Field -> Picture           -- 等高線の地図（画像）を描く
drawField mf = case mf of
  Just f -> imageBox (fdX f) (fdY f) (fdW f) (fdH f) (fdIW f) (fdIH f) (fdPic f)
  Nothing -> Blank

-- 地図の作り方（p5.js 版の opt）
data FieldOpt = FieldOpt { foLevel :: !Double, foVmin :: !Double, foVmax :: !Double, foC0 :: RGB, foC1 :: RGB, foCap :: !Double, foRes :: !Int }

fieldOpt :: FieldOpt                          -- p5.js 版の既定の値（res は 0 = 道具ごとの既定）
fieldOpt = FieldOpt 0.1 0 1 (40, 60, 95) orange (1 / 0) 0

-- 色の帯と線の画像を作る（vals = 各画素の値、NaN は外 = 背景の色。edge の線は背景の上に重ねた色）
bandImage :: Int -> Int -> U.Vector Double -> (Double -> Int) -> (Double -> Double) -> FieldOpt -> Float -> Float -> Picture
bandImage gw gh vals bandOf tOf opt edgeV edgeA = pixelImage gw gh 1 px
  where
    (b0, b1, b2) = bgC
    blend a c bgc = a / 255 * c + (1 - a / 255) * bgc
    (c00, c01, c02) = foC0 opt
    (c10, c11, c12) = foC1 opt
    w8 :: Float -> Word8
    w8 v = fromIntegral (max 0 (min 255 (round v :: Int)))
    px i j =
      let k = j * gw + i
          v = vals U.! k
          band = bandOf v
          okN q = not (isNaN q)
          edge = v < foCap opt && ((i + 1 < gw && okN (vals U.! (k + 1)) && bandOf (vals U.! (k + 1)) /= band)
                                   || (j + 1 < gh && okN (vals U.! (k + gw)) && bandOf (vals U.! (k + gw)) /= band))
          t = realToFrac (clampv (tOf v) 0 1)
          sh = if odd band then 0.9 else 1
          mixc a b = (a + (b - a) * t) * sh
      in if isNaN v then (w8 b0, w8 b1, w8 b2)
         else if edge then (w8 (blend edgeA edgeV b0), w8 (blend edgeA edgeV b1), w8 (blend edgeA edgeV b2))
         else (w8 (mixc c00 c10), w8 (mixc c01 c11), w8 (mixc c02 c12))

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：種 s の列（0 なら 1）
rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 計算を種 s から実行して、結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

-- n 回くり返す（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- 標準正規分布（ボックス-ミュラー法、u = 1 − rnd() を先に引く）
gaussRnd :: R Double
gaussRnd = do
  u0 <- rnd
  v <- rnd
  pure (sqrt (-2 * log (1 - u0)) * cos (2 * pi * v))

-- でたらめな確率分布（k 個、どれも floor 以上）
rndDist :: Int -> Double -> R [Double]
rndDist k fl = do
  es <- mapM (\_ -> fmap (\u -> -log (1 - u) + 1e-9) rnd) [1 .. k]
  pure (mixFloor (normP es) fl)

-- p にしたがって番号を 1 つ（rnd() を 1 回）
sampleIndex :: [Double] -> R Int
sampleIndex p = do
  u <- rnd
  let go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- ---- 情報の道具：確率分布 p = [p1, p2, ...]（合計 1）。log(0) は 0·log 0 = 0 として扱う
tiny :: Double
tiny = 1e-300

posInf :: Double
posInf = 1 / 0

normP :: [Double] -> [Double]
normP p = let s = sumOf (map (max 0) p)
          in if not (s > 0) then map (const (1 / i2d (length p))) p else map (\v -> max 0 v / s) p

-- どの確率も f 以上に（0 を避ける）
mixFloor :: [Double] -> Double -> [Double]
mixFloor p f = if f == 0 then p else normP (map (max f) p)

safeLog :: Double -> Double
safeLog v = log (max v tiny)

-- エントロピー（ナット）
entropyH :: [Double] -> Double
entropyH = foldl' (\h v -> if v > 0 then h - v * log v else h) 0

-- KL ダイバージェンス D(p‖q)（ナット）。q が 0 で p > 0 なら ∞
divKL :: [Double] -> [Double] -> Double
divKL p q = go 0 (zip p (q ++ repeat 0))
  where go !d [] = max 0 d
        go !d ((a, b) : rest)
          | a <= 0 = go d rest
          | b <= 0 = posInf
          | otherwise = go (d + a * log (a / b)) rest

-- 交差エントロピー H(p, q)
crossH :: [Double] -> [Double] -> Double
crossH p q = foldl' (\h (a, b) -> if a > 0 then h - a * safeLog b else h) 0 (zip p q)

bhatta :: [Double] -> [Double] -> Double     -- Σ √(p q)
bhatta p q = min 1 (foldl' (\s (a, b) -> s + sqrt (max 0 (a * b))) 0 (zip p q))

-- フィッシャー-ラオ距離 = 球の大円の長さ
fisherRao :: [Double] -> [Double] -> Double
fisherRao p q = 2 * acos (clampv (bhatta p q) (-1) 1)

-- Σ(√p − √q)²
hellingerSq :: [Double] -> [Double] -> Double
hellingerSq p q = foldl' (\s (a, b) -> let d = sqrt a - sqrt b in s + d * d) 0 (zip p q)

tvDist :: [Double] -> [Double] -> Double     -- 全変動距離
tvDist p q = foldl' (\s (a, b) -> s + abs (a - b)) 0 (zip p q) / 2

-- α-ダイバージェンス（甘利の流儀）。α → −1 で D(p‖q)、α → +1 で D(q‖p)
alphaDiv :: [Double] -> [Double] -> Double -> Double
alphaDiv p q a
  | abs (a + 1) < 1e-12 = divKL p q
  | abs (a - 1) < 1e-12 = divKL q p
  | otherwise = let ea = (1 - a) / 2
                    eb = (1 + a) / 2
                    s = foldl' (\acc (x, y) -> acc + max x 0 ** ea * max y 0 ** eb) 0 (zip p q)
                in max 0 ((4 / (1 - a * a)) * (1 - s))

-- m-測地線：確率をそのまま混ぜる
mixM :: [Double] -> [Double] -> Double -> [Double]
mixM p q t = zipWith (\v w -> (1 - t) * v + t * w) p q

-- e-測地線：対数で混ぜて正規化
mixE :: [Double] -> [Double] -> Double -> [Double]
mixE p q t = normP (zipWith (\v w -> exp ((1 - t) * safeLog v + t * safeLog w)) p q)

-- α-測地線：u^((1−α)/2) で混ぜて正規化（α = −1 は m、α = +1 は e）
mixAlpha :: [Double] -> [Double] -> Double -> Double -> [Double]
mixAlpha p q t a
  | abs (a - 1) < 1e-9 = mixE p q t
  | otherwise = let ex = (1 - a) / 2
                in normP (zipWith (\v w -> ((1 - t) * v ** ex + t * w ** ex) ** (1 / ex)) p q)

bitsOf :: Double -> Double
bitsOf nat = nat / log 2

-- ---- 2 次元の図の枠（データ座標 ↔ 画面）
data PBox = PBox { pbX :: !Double, pbY :: !Double, pbW :: !Double, pbH :: !Double, pbX0 :: !Double, pbX1 :: !Double, pbY0 :: !Double, pbY1 :: !Double }

plotBox :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> PBox
plotBox = PBox

pbBox :: PBox -> Box                          -- 枠の長方形（inRect 用）
pbBox b = Box (pbX b) (pbY b) (pbW b) (pbH b)

toScr :: PBox -> Double -> Double -> (Double, Double)
toScr b u v = (pbX b + (u - pbX0 b) / (pbX1 b - pbX0 b) * pbW b, pbY b + pbH b - (v - pbY0 b) / (pbY1 b - pbY0 b) * pbH b)

toScrF :: PBox -> Double -> Double -> (Float, Float)
toScrF b u v = let (x, y) = toScr b u v in (f2 x, f2 y)

fromScr :: PBox -> Double -> Double -> (Double, Double)
fromScr b sx sy = (pbX0 b + (sx - pbX b) / pbW b * (pbX1 b - pbX0 b), pbY0 b + (pbY b + pbH b - sy) / pbH b * (pbY1 b - pbY0 b))

drawAxes :: PBox -> String -> String -> Picture
drawAxes b xl yl = Pictures
  [ Color (col wire) (frameTL 1 x y w h)
  , sayT xl (x + w / 2) (y + h + 16) 14 (col dimc) True
  , sayT (fmtN 1 (pbX0 b)) x (y + h + 2) 14 (col dimc) False
  , sayR (fmtN 1 (pbX1 b)) (x + w) (y + h + 14) 14 (col dimc)
  , sayRM (fmtN 1 (pbY0 b)) (x - 4) (y + h) 14 (col dimc)
  , sayRM (fmtN 1 (pbY1 b)) (x - 4) y 14 (col dimc)
  , say yl x (y - 5) 14 (col dimc) ]
  where x = f2 (pbX b)
        y = f2 (pbY b)
        w = f2 (pbW b)
        h = f2 (pbH b)

-- データ座標の点列を折れ線に（枠の外の部分は枠のふちで切る）
plotLine :: PBox -> [(Double, Double)] -> Color -> Float -> Picture
plotLine b pts c w = Color c (Pictures (map (polyline w . map (\(x, y) -> (f2 x, f2 y))) (clipRuns b pts)))

-- 枠の中に入る部分の折れ線の列（リャン-バースキー法で線分を切る）
clipRuns :: PBox -> [(Double, Double)] -> [[(Double, Double)]]
clipRuns b pts = finish (foldl' stepP ([], [], Nothing) pts)
  where
    x0 = pbX b - 1
    x1 = pbX b + pbW b + 1
    y0 = pbY b - 1
    y1 = pbY b + pbH b + 1
    flush run done = if length run >= 2 then reverse run : done else done
    finish (run, done, _) = reverse (flush run done)
    clip (ax, ay) (cx, cy) =
      let dx = cx - ax
          dy = cy - ay
          go (t0, t1) (p, q)
            | p == 0 = if q < 0 then Nothing else Just (t0, t1)
            | otherwise = let r = q / p in Just (if p < 0 then (max t0 r, t1) else (t0, min t1 r))
          res = foldl' (\acc pq -> acc >>= \tt -> go tt pq) (Just (0, 1)) [(-dx, ax - x0), (dx, x1 - ax), (-dy, ay - y0), (dy, y1 - ay)]
      in case res of
           Just (t0, t1) | t0 <= t1 -> Just (t0, t1)
           _ -> Nothing
    stepP (run, done, prev) (u, v) =
      let s@(sx, sy) = toScr b u v
          good = not (isNaN sx || isNaN sy || isInfinite sx || isInfinite sy)
      in if not good then ([], flush run done, Nothing) else
         case prev of
           Nothing -> (run, done, Just s)
           Just pv@(px, py) -> case clip pv s of
             Nothing -> ([], flush run done, Just s)
             Just (t0, t1) ->
               let pa = (px + t0 * (sx - px), py + t0 * (sy - py))
                   pb = (px + t1 * (sx - px), py + t1 * (sy - py))
                   (run1, done1) = if t0 > 0 then ([pa], flush run done) else if null run then ([pa], done) else (run, done)
                   run2 = pb : run1
               in if t1 < 1 then ([], flush run2 done1, Just s) else (run2, done1, Just s)

-- 長方形の上の等高線の地図（fn(u, v)）
gridField :: PBox -> (Double -> Double -> Double) -> FieldOpt -> Field
gridField b fn opt =
  let res = if foRes opt > 0 then foRes opt else 3
      gw = ceiling (pbW b / i2d res) :: Int
      gh = ceiling (pbH b / i2d res) :: Int
      vals = U.generate (gw * gh) $ \k ->
        let (j, i) = k `divMod` gw
            (u, v) = fromScr b (pbX b + (i2d i + 0.5) * i2d res) (pbY b + (i2d j + 0.5) * i2d res)
        in fn u v
      lv = foLevel opt
      vmin = foVmin opt
      vmax = foVmax opt
      bandOf v = floor (clampv ((v - vmin) / lv) (-1e6) 1e6) :: Int
      pic = bandImage gw gh (U.map (\v -> if isNaN v then -1e300 else v) vals) bandOf (\v -> (v - vmin) / (vmax - vmin)) opt 235 170
  in vals `seq` Field pic (f2 (pbX b)) (f2 (pbY b)) (i2f (gw * res)) (i2f (gh * res)) gw gh

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

mouseD :: Env -> (Double, Double)             -- 計算用の Double で
mouseD env = let (x, y) = mouseP env in (d2 x, d2 y)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 左揃え・右揃えで上下は中央（textAlign(LEFT, CENTER)、(RIGHT, CENTER)）
sayLM, sayRM :: String -> Float -> Float -> Float -> Color -> Picture
sayLM s x y size c = say s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c

-- 上が y の文字（textAlign(…, TOP)）。center = True なら中央揃え
sayT :: String -> Float -> Float -> Float -> Color -> Bool -> Picture
sayT s x y size c center = (if center then sayC else say) s x (y + size * 0.85) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (max 0 d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (max 0 d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> thickPath w pts
  _ -> Blank

-- 点線（長さ on の線と off のすき間をくり返す）
dashLine :: Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine w on off (x1, y1) (x2, y2) =
  let l = hyp (x2 - x1) (y2 - y1)
      n = floor (l / (on + off)) :: Int
      ux = if l == 0 then 0 else (x2 - x1) / l
      uy = if l == 0 then 0 else (y2 - y1) / l
  in Pictures [ lineW w (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + on), y1 + uy * min l (s + on)) | k <- [0 .. n], let s = i2f k * (on + off), s < l ]

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（iw × ih の画像）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral (max 1 iw)) (negate h / fromIntegral (max 1 ih)) pic)

-- ---- 自己テストの道具
type Check = (String, Bool)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [Check] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestOk :: [Check] -> Bool
selfTestOk = all snd

selfTestLine :: [Check] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [Check] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col lime else col rose)
