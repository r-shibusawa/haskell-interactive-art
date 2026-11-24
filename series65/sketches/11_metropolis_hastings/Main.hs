-- ============================================
-- 65-11 メトロポリス-ヘイスティングス法 ─ 「山が 2 つある分布」から、マルコフ連鎖でサンプルを作る
-- いまの点 x から、ずれ幅 σ の正規分布で次の候補 x′ を出す（提案）。
-- 目標の高さ f が上がるなら必ず移り、下がるなら確率 f(x′)/f(x) で移る（だめならその場にとどまる = 同じ点をもう 1 回数える）。
-- こうして作った連鎖の定常分布はちょうど目標の分布 → 長く歩くとヒストグラムが等高線に近づく
-- 幅が小さすぎると少しずつしか動けず、山から山へ渡れない。大きすぎるとほとんど断られる。自己相関と有効サンプル数（ESS）で比べる
-- 操作：↑↓ 提案の幅 σ / F 速さ / クリック そこから歩き直す / H ヒストグラム ⇔ 目標の色 / スペース 一時停止 / R やり直し
-- （同じメトロポリス法でスピンを 1 つずつ反転するのが、シリーズ 55 の 11 イジング模型）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- ★ 目標の分布：2 つの正規分布の山の混ぜ合わせ（重み、中心 x、中心 y、広がり）
peaks :: [(Double, Double, Double, Double)]
peaks = [ (0.45, -1.4, -0.4, 0.55)
        , (0.55, 1.5, 0.6, 0.8) ]

widths :: [Double]
widths = [0.05, 0.2, 0.5, 1, 2, 5, 15]        -- ★ 提案の幅 σ の候補

speedNames :: [String]
speedNames = ["slow", "1 step/frame", "50 steps/frame", "full"]

fullSteps :: Int
fullSteps = 2000                              -- ★ 「全力」のときの 1 フレームの歩数

xr0, xr1, yr0, yr1 :: Double                  -- 描く範囲
xr0 = -4
xr1 = 4
yr0 = -3
yr1 = 3

binsX, binsY, keep :: Int
binsX = 80                                    -- ヒストグラムの細かさ
binsY = 60
keep = 20000                                  -- 自己相関に使う最近のサンプル数

imgK :: Float
imgK = 3                                      -- 背景の絵は 1/3 の大きさで作って 3 倍にのばす

data World = World
  { wIdx     :: !Int                          -- σ の番号
  , wSpeed   :: !Int
  , wPaused  :: !Bool
  , wShowH   :: !Bool
  , wCur     :: !(Double, Double)
  , wN       :: !Int                          -- 歩数
  , wAcc     :: !Int                          -- 採択した数
  , wHist    :: U.Vector Int
  , wRecent  :: U.Vector Double               -- 最近の x
  , wTrail   :: [(Double, Double)]            -- 軌跡（新しい順、300 点まで）
  , wSX, wSY, wSXX :: !Double
  , wAcf     :: [Double]
  , wEss     :: !Double
  , wIW, wIH :: !Int                          -- 背景の絵の大きさ
  , wBg      :: U.Vector (Word8, Word8, Word8)
  , wImg     :: Picture                       -- 背景（＋ヒストグラム）
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "65-11 Metropolis-Hastings" world0)
  { skBack  = col bgCol
  , skStart = \env -> do
      printSelfTest selfTest
      let (_, _, pw, ph) = plotBox env
          iw = round (pw / imgK)
          ih = round (ph / imgK)
          bg = makeBackground iw ih
          w = world0 { wTest = selfTest, wIW = iw, wIH = ih, wBg = bg }
      pure w { wImg = bgPicture w }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = restart (-3, 2.5) (World 3 1 False True (0, 0) 0 0 U.empty U.empty [] 0 0 0 [] 0 1 1 U.empty Blank (rngOf 11) [])

restart :: (Double, Double) -> World -> World
restart p w = w { wCur = p, wN = 0, wAcc = 0, wHist = U.replicate (binsX * binsY) 0, wRecent = U.empty, wTrail = [p]
                , wSX = 0, wSY = 0, wSXX = 0, wAcf = [], wEss = 0 }

-- ---- 目標の密度（合計 1 になるように正規化した形）
targetDensity :: Double -> Double -> Double
targetDensity x y = foldl' (\f (w, mx, my, sd) -> let r2 = (x - mx) ** 2 + (y - my) ** 2 in f + (w / (2 * pi * sd * sd)) * exp (negate r2 / (2 * sd * sd))) 0 peaks

-- 混ぜ合わせの平均と分散（式で正確に）：(平均 x, 平均 y, x の分散)
targetMoments :: (Double, Double, Double)
targetMoments =
  let (mx, my, ex2) = foldl' (\(a, b, c) (w, px, py, sd) -> (a + w * px, b + w * py, c + w * (sd * sd + px * px))) (0, 0, 0) peaks
  in (mx, my, ex2 - mx * mx)

-- ★ メトロポリス-ヘイスティングスの 1 歩（提案は左右対称な正規分布なので q の比は 1）。戻り値 (次の点, 採択したか)
mhStep :: (Double, Double) -> Double -> R ((Double, Double), Bool)
mhStep (px, py) sig = do
  a <- randn
  b <- randn
  let q = (px + sig * a, py + sig * b)
      ratio = uncurry targetDensity q / max (targetDensity px py) 1e-300
  u <- rnd
  pure (if u < ratio then (q, True) else ((px, py), False))

meanU :: U.Vector Double -> Double
meanU v = if U.null v then 0 else U.foldl' (+) 0 v / i2d (U.length v)

-- 自己相関 ρ_k
autocorr :: U.Vector Double -> Int -> [Double]
autocorr xs maxLag =
  let n = U.length xs
      m = meanU xs
      d = U.map (subtract m) xs
      v = U.foldl' (\s x -> s + x * x) 0 d
  in if n < 3 || v <= 0 then [1] else
       [ U.foldl' (+) 0 (U.zipWith (*) d (U.drop k d)) / v | k <- [0 .. min maxLag (n - 2)] ]

-- 有効サンプル数 ESS = n / (1 + 2 Σ ρ_k)（ずれ k を 1 つずつ計算し、ρ_k ≤ 0 になったら止める）
essOf :: U.Vector Double -> Int -> Double
essOf xs maxLag =
  let n = U.length xs
      m = meanU xs
      d = U.map (subtract m) xs
      v = U.foldl' (\s x -> s + x * x) 0 d
      go !k !s
        | k < min maxLag (n - 1) = let c = U.foldl' (+) 0 (U.zipWith (*) d (U.drop k d)) in if c <= 0 then s else go (k + 1) (s + c / v)
        | otherwise = s
  in if n < 3 || v <= 0 then i2d n else i2d n / (1 + 2 * go 1 0)

-- k 歩すすめる
runSteps :: Int -> World -> World
runSteps k w
  | k <= 0 = w
  | otherwise =
      let sig = at' widths (wIdx w) 1
          go 0 _ acc = pure (reverse acc)
          go j p acc = do (q, a) <- mhStep p sig; go (j - 1 :: Int) q ((q, a) : acc)
          (new, g) = runR (go k (wCur w) []) (wRng w)
          pts = map fst new
          (sx, sy, sxx) = foldl' (\(a, b, c) (x, y) -> (a + x, b + y, c + x * x)) (wSX w, wSY w, wSXX w) pts
          bins = [ (by * binsX + bx, 1) | (x, y) <- pts
                 , let bx = floor ((x - xr0) / (xr1 - xr0) * i2d binsX), let by = floor ((yr1 - y) / (yr1 - yr0) * i2d binsY)
                 , bx >= 0, by >= 0, bx < binsX, by < binsY ]
          rec0 = wRecent w U.++ U.fromList (map fst pts)
      in w { wCur = lastOr (wCur w) pts, wN = wN w + k, wAcc = wAcc w + length (filter snd new)
           , wSX = sx, wSY = sy, wSXX = sxx, wHist = U.accum (+) (wHist w) bins
           , wRecent = U.drop (U.length rec0 - keep) rec0, wTrail = take 300 (reverse pts ++ wTrail w), wRng = g }

step :: Env -> Float -> World -> IO World
step env _ w0 = pure w3
  where
    fc = envFrame env
    w1 | wPaused w0 = w0
       | otherwise = runSteps (case wSpeed w0 of
                                 0 -> if fc `mod` 8 == 0 then 1 else 0
                                 1 -> 1
                                 2 -> 50
                                 _ -> fullSteps) w0
    n = U.length (wRecent w1)
    w2 = if fc `mod` 30 == 1 && n > 50
           then let lastK = U.drop (n - 4000) (wRecent w1)
                in w1 { wAcf = autocorr lastK 60, wEss = essOf lastK 400 * (i2d n / i2d (min 4000 n)) }
           else w1
    w3 = if wShowH w2 && wN w2 > 0 && fc `mod` 10 == 1 then w2 { wImg = histPicture w2 } else w2

-- 平面の場所（左上 x, y、幅、高さ）
plotBox :: Env -> (Float, Float, Float, Float)
plotBox env = let ph = min (envH env - 150) (envW env * 0.56 * 0.75) in (40, 96, ph * 4 / 3, ph)

-- 目標の密度を色で、等高線をクリーム色の点で（1 回だけ計算）
makeBackground :: Int -> Int -> U.Vector (Word8, Word8, Word8)
makeBackground w h =
  let vals = U.generate (w * h) (\k -> let (j, i) = k `divMod` w in targetDensity (xr0 + (i2d i + 0.5) / i2d w * (xr1 - xr0)) (yr1 - (i2d j + 0.5) / i2d h * (yr1 - yr0)))
      fmax = U.maximum vals
      band v = floor (v / fmax * 8) :: Int
      q v = fromIntegral (max 0 (min 255 (round v :: Int))) :: Word8
  in U.generate (w * h) (\k -> let (j, i) = k `divMod` w
                                   v = vals U.! k
                                   t = sqrt (v / fmax)
                                   edge = i > 0 && j > 0 && (band v /= band (vals U.! (k - 1)) || band v /= band (vals U.! (k - w)))
                               in if edge then (200, 190, 170) else (q (38 + t * 30), q (34 + t * 70), q (32 + t * 66)))

bgPicture :: World -> Picture
bgPicture w = pixelImage (wIW w) (wIH w) 1 (\x y -> wBg w U.! (y * wIW w + x))

-- ヒストグラム（濃い = たくさん）を、背景の色に自分で混ぜる（透明度 220 × √(数 / 最大)）
histPicture :: World -> Picture
histPicture w =
  let hs = wHist w
      hmax = i2d (max 1 (U.maximum hs))
      iw = wIW w
      ih = wIH w
      (dr, dg, db) = dice
  in pixelImage iw ih 1 (\x y ->
       let (r, g, b) = wBg w U.! (y * iw + x)
           bx = min (binsX - 1) (x * binsX `div` iw)
           by = min (binsY - 1) (y * binsY `div` ih)
           a = 220 * sqrt (i2d (hs U.! (by * binsX + bx)) / hmax) / 255
           mixC c d = fromIntegral (max 0 (min 255 (round (fromIntegral c * (1 - a) + realToFrac d * a) :: Int))) :: Word8
       in (mixC r dr, mixC g dg, mixC b db))

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Metropolis-Hastings: " ++ show (wN w) ++ " steps   proposal sigma = " ++ showD sig ++ "   accepted " ++ show (if wN w > 0 then jsRoundI (100 * i2d (wAcc w) / i2d (wN w)) else 0) ++ "%")
             ("Up/Down sigma   F speed (" ++ at' speedNames (wSpeed w) "" ++ ")   click: restart there   H " ++ (if wShowH w then "histogram" else "target colours") ++ "   Space " ++ (if wPaused w then "resume" else "pause") ++ "   R reset")
  -- ---- 平面：目標の色と等高線 ＋ ヒストグラム ＋ 軌跡
  , imageBox px py pw ph (wIW w) (wIH w) (if wShowH w && wN w > 0 then wImg w else bgPicture w)
  , Color (colA 170 mustard) (polyline 1.2 (map toS (reverse (take 120 (wTrail w)))))
  , let (cx, cy) = toS (wCur w) in Pictures [ Color (col dice) (circleAt cx cy 12)
                                            , Color (colA 120 cream) (ringAt cx cy (f2 (2 * sig / (xr1 - xr0)) * pw) 1) ]
  -- ---- 右側：x 座標の軌跡（トレース）、自己相関、数値
  , plotAxes g1 "last 500 steps" "x trace (does it hop between the peaks?)" [0, 500] [-2, 0, 2]
  , drawCurve g1 (U.toList tl) teal 1.5 (500 - i2d (U.length tl))
  , plotAxes g2 "lag k" "autocorrelation rho_k (the faster to 0, the better)" [0, 30, 60] [0, 0.5, 1]
  , Color (col (84, 76, 70)) (lineW 1 (grX g2, gY g2 0) (grX g2 + grW g2, gY g2 0))
  , if length (wAcf w) > 1 then drawCurve g2 (wAcf w) dice 2.5 0 else Blank
  , say ("sample mean x = " ++ fmtN 3 (wSX w / n) ++ "   (target " ++ fmtN 3 mmx ++ ")") rx ty 15 (col cream)
  , say ("sample mean y = " ++ fmtN 3 (wSY w / n) ++ "   (target " ++ fmtN 3 mmy ++ ")") rx (ty + 24) 15 (col cream)
  , say ("x variance = " ++ fmtN 3 (wSXX w / n - (wSX w / n) ** 2) ++ "   (target " ++ fmtN 3 mvx ++ ")") rx (ty + 48) 15 (col cream)
  , say ("effective sample size ESS ~ " ++ (if U.length (wRecent w) > 50 then show (jsRoundI (wEss w)) else "-") ++ " (of " ++ show (wN w) ++ " steps)") rx (ty + 80) 15 (col mustard)
  , Pictures [ say s rx (ty + 106 + 18 * i2f i) 14 (col sub) | (i, s) <- zip [0 :: Int ..]
      [ "ESS = how many independent samples this is worth."
      , "Small sigma: tiny steps. Large sigma: rejected,"
      , "stays on the same point -> both give a small ESS" ] ]
  , drawSelfTest env (wTest w) ]
  where
    sig = at' widths (wIdx w) 1
    (px, py, pw, ph) = plotBox env
    toS (x, y) = (px + f2 ((x - xr0) / (xr1 - xr0)) * pw, py + f2 ((yr1 - y) / (yr1 - yr0)) * ph)
    rx = px + pw + 60
    rw = envW env - rx - 40
    g1 = Graph rx 110 rw 120 0 500 (-4) 4
    g2 = Graph rx 290 rw 120 0 60 (-0.2) 1
    rl = U.length (wRecent w)
    tl = U.drop (rl - 500) (wRecent w)
    (mmx, mmy, mvx) = targetMoments
    n = i2d (max 1 (wN w))
    ty = 470

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        (px, py, pw, ph) = plotBox env
        u = realToFrac ((mx - px) / pw)
        v = realToFrac ((my - py) / ph)
    in if u < 0 || v < 0 || u > 1 || v > 1 then w else restart (xr0 + u * (xr1 - xr0), yr1 - v * (yr1 - yr0)) w
  EventKey (SpecialKey KeyUp) Down _ _ -> restart (wCur w) w { wIdx = min (length widths - 1) (wIdx w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> restart (wCur w) w { wIdx = max 0 (wIdx w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c == 'f' || c == 'F' -> w { wSpeed = (wSpeed w + 1) `mod` length speedNames }
    | c == 'h' || c == 'H' -> w { wShowH = not (wShowH w) }
    | c == 'r' || c == 'R' -> restart (-3, 2.5) w
  _ -> w

-- ---------- やってみよう ----------
-- ・σ = 0.05 と σ = 15 で、トレースと自己相関の形を見比べる。採択率がいくつくらいのとき ESS が最大？
-- ・peaks の 2 つの山を遠くに離す（中心 x = ±3）。σ = 1 でも山を渡れなくなる → 「混ざらない」連鎖
-- ・クリックで右の山の中から歩き直す。最初の何百歩か（バーンイン）を捨てるべき理由が見える
-- ・targetDensity を自分の好きな形（輪っか、バナナ形）にしてみる。正規化の定数は要らない（比しか使わない）

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("離散のメトロポリス：行の合計 = 1、定常分布 = 目標（連立方程式）", rowSumErr km 1 < 1e-12 && maxAbsDiffV (maybe [] id (stationaryExact km)) tgt < 1e-12)
  , ("詳細つり合い π_i P_ij = π_j P_ji（ずれ " ++ fmtE db ++ "）", db < 1e-14)
  , ("σ = 1：平均 x " ++ fmtN 3 mx ++ " ≈ " ++ fmtN 3 mmx ++ "（ESS " ++ show (jsRoundI ess) ++ "、許容 4 SE）", abs (mx - mmx) < 4 * se)
  , ("σ = 1：平均 y ≈ " ++ fmtN 3 mmy ++ "、x の分散 ≈ " ++ fmtN 3 mvx, abs (meanU ys - mmy) < 4 * sqrt (1 / ess) && abs (vx - mvx) < 0.12 * mvx)
  , ("採択率は σ = 0.05 → 1 → 15 で下がる（" ++ joinWith " → " [ show (jsRoundI (100 * v)) ++ "%" | v <- rate ] ++ "）", at' rate 0 0 > at' rate 1 0 && at' rate 1 0 > at' rate 2 0)
  , ("ESS は σ = 1 が最大（" ++ joinWith ", " (map (show . jsRoundI) essW) ++ "）", at' essW 1 0 > at' essW 0 0 && at' essW 1 0 > at' essW 2 0)
  , ("ESS の式：独立なら ≈ n（" ++ show (jsRoundI e1) ++ "）、AR(1) ρ = 0.9 なら ≈ " ++ show (jsRoundI e2t) ++ "（" ++ show (jsRoundI e2) ++ "）", e1 > 15000 && abs (e2 / e2t - 1) < 0.3)
  , ("密度の値に NaN なし（遠くでも）", noNaN [targetDensity 1e6 (-1e6), targetDensity 0 0] && targetDensity 0 0 > 0) ]
  where
    -- (1) 離散の場合：5 状態の目標 wt にメトロポリス法の遷移行列を正確に作る
    wt = [1, 3, 2, 5, 4] :: [Double]
    z = sumOf wt
    tgt = map (/ z) wt
    nS = 5
    km = [ let offs = [ (j, 0.5 * min 1 (at' wt j 0 / at' wt i 1)) | j <- [i - 1, i + 1], j >= 0, j < nS ]
               row0 = [ maybe 0 id (lookup j offs) | j <- [0 .. nS - 1] ]
           in setAt i (1 - sumOf row0) row0
         | i <- [0 .. nS - 1] ]
    db = foldl' maxN 0 [ abs (at' tgt i 0 * at' (at' km i []) j 0 - at' tgt j 0 * at' (at' km j []) i 0) | i <- [0 .. nS - 1], j <- [0 .. nS - 1] ]
    (mmx, mmy, mvx) = targetMoments
    -- (2) 2 次元：σ = 1 で 6 万歩
    chain :: Int -> Double -> (Double, Double) -> R ([(Double, Double)], Int)
    chain cnt sg p0 = go cnt p0 [] 0
      where go 0 _ acc a = pure (reverse acc, a)
            go k p acc !a = do (q, ok) <- mhStep p sg; go (k - 1) q (q : acc) (if ok then a + 1 else a)
    (xs, ys) = withSeed 101 $ do
      p <- loopR 2000 (\q -> fst <$> mhStep q 1) (0, 0)        -- バーンイン
      (pts, _) <- chain 60000 1 p
      pure (U.fromList (map fst pts), U.fromList (map snd pts))
    ess = essOf xs 2000
    se = sqrt (mvx / ess)
    mx = meanU xs
    vx = meanU (U.map (\v -> (v - mx) ** 2) xs)
    -- (3) 幅を変えると
    runs = [ withSeed 202 (do (pts, a) <- chain 15000 sg (0, 0); pure (i2d a / 15000, essOf (U.fromList (map fst pts)) 3000)) | sg <- [0.05, 1, 15] ]
    rate = map fst runs
    essW = map snd runs
    -- (4) ESS の計算の確認
    (iid, ar) = withSeed 303 $ do
      let go 0 _ a b = pure (U.fromList (reverse a), U.fromList (reverse b))
          go k v a b = do
            x <- randn
            r <- randn
            let v' = 0.9 * v + sqrt (1 - 0.81) * r
            go (k - 1 :: Int) v' (x : a) (v' : b)
      go 20000 0 [] []
    e1 = essOf iid 2000
    e2 = essOf ar 2000
    e2t = (20000 * 0.1) / 1.9

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  _ -> foldr1 (\a b -> a ++ sep ++ b) xs

-- ==================================================
-- ここから下は「マルコフ連鎖の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。行列 = 行のリスト、分布 = 横ベクトル（リスト）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具
type Vec = [Double]                           -- 分布・ベクトル
type Mat = [[Double]]                         -- 行列（transP !! i !! j = i の次に j へ行く確率、行の和 = 1）
type RGB = (Float, Float, Float)

bgCol, cream, dice, teal, mustard, plum, sub :: RGB
bgCol = (32, 28, 26)                          -- 背景（あたたかい炭色）
cream = (246, 236, 214)
dice = (236, 96, 58)
teal = (44, 178, 166)
mustard = (236, 190, 82)
plum = (190, 130, 200)
sub = (190, 178, 160)

stateCols :: [RGB]                            -- ★ 状態の色（8 色をくり返す）
stateCols = [(236, 96, 58), (44, 178, 166), (236, 190, 82), (190, 130, 200), (120, 190, 240), (150, 210, 110), (240, 140, 170), (200, 170, 130)]

stateCol :: Int -> RGB
stateCol i = stateCols !! (i `mod` 8)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

grayC :: Float -> Color
grayC v = col (v, v, v)

scaleRGB :: Float -> RGB -> RGB
scaleRGB k (r, g, b) = (r * k, g * k, b * k)

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

meanOf :: [Double] -> Double
meanOf xs = if null xs then 0 else sumOf xs / fromIntegral (length xs)

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float                         -- 描くときの Float（NaN は 0）
f2 = realToFrac . fin 0

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

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

-- 行列の (i, j) を f で変える
modifyMat :: Int -> Int -> (Double -> Double) -> Mat -> Mat
modifyMat i j f = modifyAt i (modifyAt j f)

-- 長さ √(a² + b²)（Math.hypot）
hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Integer
jsRound v = let r = floor v in if v - fromIntegral r >= 0.5 then r + 1 else r

jsRoundI :: Double -> Int
jsRoundI v = if isNaN v || isInfinite v then 0 else fromIntegral (jsRound (clampv v (-1e15) 1e15))

-- Math.round(v * 100) / 100
round2 :: Double -> Double
round2 v = fromIntegral (jsRound (v * 100)) / 100

round1 :: Double -> Double
round1 v = fromIntegral (jsRound (v * 10)) / 10

-- 確率 p !! i で i を返す（rnd() を 1 回だけ引く。p5.js 版の sampleFrom）
sampleFrom :: [Double] -> R Int
sampleFrom p = do
  u0 <- rnd
  let u = u0 * sumOf p
      go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- ---- 数の書き方
-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）。−0.00 は 0.00
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

-- v.toExponential(1)：1.2e-15 の形
fmtE :: Double -> String
fmtE v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0.0e+0"
  | otherwise =
      let r = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          e1 = head ([e | e <- [e0 - 1, e0, e0 + 1], 10 ^^ e <= r, r < 10 ^^ (e + 1)] ++ [e0])
          n0 = floor (r / 10 ^^ (e1 - 1) + 1 / 2) :: Integer
          (n, e) = if n0 >= 100 then (10, e1 + 1) else (n0, e1)
      in (if v < 0 then "-" else "") ++ show (n `div` 10) ++ "." ++ show (n `mod` 10) ++ "e" ++ (if e >= 0 then "+" else "-") ++ show (abs e)

-- 整数を 3 けたごとのコンマで（toLocaleString）
commas :: Integer -> String
commas n | n < 0 = '-' : commas (negate n)
         | otherwise = reverse (go (reverse (show n)))
  where go s = case splitAt 3 s of
          (a, []) -> a
          (a, rest) -> a ++ "," ++ go rest

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v0 = let v = fin 0 v0 in
  if v == 0 then "0" else
    let e = floor (logBase 10 (abs v)) :: Int
        k = 10 ^^ (2 - e) :: Double
        r = fromIntegral (jsRound (v * k)) / k
    in if r == fromIntegral (jsRound r) then show (jsRound r) else showFFloat Nothing r ""

showD :: Double -> String                     -- JavaScript の String(v) に近い書き方（0.05、2、0.1）
showD v = if v == fromIntegral (jsRound v) && abs v < 1e15 then show (jsRound v) else showFFloat Nothing v ""

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

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

randn :: R Double                             -- 標準正規分布（ボックス-ミュラー、u = 1 − rnd()）
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- n 回くり返して結果を順に集める（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- ---- 行列の道具（行列 = 行のリスト。遷移行列 transP !! i !! j = i の次に j へ行く確率、行の和 = 1）
zerosMat :: Int -> Mat
zerosMat n = replicate n (replicate n 0)

zerosMat2 :: Int -> Int -> Mat
zerosMat2 n m = replicate n (replicate m 0)

eyeMat :: Int -> Mat
eyeMat n = [[if i == j then 1 else 0 | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]

nCols :: Mat -> Int
nCols a = length (headOr [] a)

-- 行列の積 (a b)[i][j] = Σ_k a[i][k] b[k][j]（k の順に足す）
matMul :: Mat -> Mat -> Mat
matMul a b = [ foldl' (\acc (v, brow) -> if v == 0 then acc else zipWith (\c x -> c + v * x) acc brow) (replicate (nCols b) 0) (zip arow b) | arow <- a ]

-- 横ベクトル × 行列：次の時刻の分布 = いまの分布 × P
vecMat :: Vec -> Mat -> Vec
vecMat v a = foldl' (\acc (x, row) -> if x == 0 then acc else forceL (zipWith (\c y -> c + x * y) acc row)) (replicate (nCols a) 0) (zip v a)

forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

matPow :: Mat -> Int -> Mat                   -- P^k（k 回かけるだけ）
matPow a k = iterate (`matMul` a) (eyeMat (length a)) !! max 0 k

-- 2 つの行列（またはベクトル）のいちばん大きなずれ
maxAbsDiff :: Mat -> Mat -> Double
maxAbsDiff a b = foldl' maxN 0 (zipWith maxAbsDiffV a b)

maxAbsDiffV :: Vec -> Vec -> Double
maxAbsDiffV a b = foldl' (\d (x, y) -> maxN d (abs (x - y))) 0 (zip a b)

-- Math.max と同じく NaN が混じったら NaN
maxN :: Double -> Double -> Double
maxN a b = if isNaN a || isNaN b then 0 / 0 else max a b

rowSumErr :: Mat -> Double -> Double          -- 行の和と target のいちばん大きなずれ
rowSumErr a target = foldl' (\d r -> max d (abs (sumOf r - target))) 0 a

-- 行の和を 1 に。全部 0 の行は「その場にとどまる」にする（NaN を出さない）
fixRows :: Mat -> Mat
fixRows a = [ let s = sumOf r in if not (s > 1e-12) then [if j == i then 1 else 0 | j <- [0 .. length r - 1]] else map (/ s) r
            | (i, r) <- zip [0 :: Int ..] a ]

-- ガウスの消去法（部分ピボット）で a x = b。解けなければ Nothing
solveLinear :: Mat -> Vec -> Maybe Vec
solveLinear a b = fmap (map (headOr 0)) (solveMany a (map (: []) b))

-- 右辺が何本あっても同じ手順で消去する（右辺 1 本ずつ解くのとまったく同じ計算になる）
solveMany :: Mat -> Mat -> Maybe Mat
solveMany a bs = go 0 (zipWith (++) a bs)
  where
    n = length a
    go c m
      | c >= n = Just [ map (/ at' r i 1) (drop n r) | (i, r) <- zip [0 ..] m ]
      | otherwise =
          let colC = [ abs (at' r c 0) | r <- m ]
              p = foldl' (\pb r -> if at' colC r 0 > at' colC pb 0 then r else pb) c [c + 1 .. n - 1]
          in if at' colC p 0 < 1e-13 then Nothing else
               let rc = at' m p []
                   rp = at' m c []
                   m1 = [ if r == c then rc else if r == p then rp else row | (r, row) <- zip [0 ..] m ]
                   piv = at' rc c 1
                   m2 = [ if r == c then row else
                            let f = at' row c 0 / piv
                            in if f == 0 then row else forceL (take c row ++ zipWith (\x y -> x - f * y) (drop c row) (drop c rc))
                        | (r, row) <- zip [0 ..] m1 ]
               in go (c + 1) m2

-- 逆行列（a x = e_j を列ごとに解いたもの）。逆がなければ Nothing
matInv :: Mat -> Maybe Mat
matInv a = solveMany a (eyeMat (length a))

transposeM :: Mat -> Mat
transposeM a = if null a then [] else [ [ at' r j 0 | r <- a ] | j <- [0 .. nCols a - 1] ]

-- 定常分布：π P = π と Σ π = 1 を連立方程式として解く（(P^T − I) π = 0 の最後の式を Σ π = 1 に置きかえる）
stationaryExact :: Mat -> Maybe Vec
stationaryExact a =
  let n = length a
      m = [ if i == n - 1 then replicate n 1 else [ at' (at' a j []) i 0 - (if i == j then 1 else 0) | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]
      b = [ if i == n - 1 then 1 else 0 | i <- [0 .. n - 1] ]
  in if n == 0 then Nothing else solveLinear m b

-- べき乗法：π ← π P を変化が tol 未満になるまで。戻り値 (π, くり返した回数)
stationaryPower :: Mat -> Double -> Int -> Maybe Vec -> (Vec, Int)
stationaryPower a tol maxIt start = go v0 0 posInf
  where
    n = length a
    v0 = maybe (replicate n (1 / fromIntegral n)) id start
    go v its d
      | d > tol && its < maxIt = let w = vecMat v a in go w (its + 1) (maxAbsDiffV w v)
      | otherwise = (v, its)

tvDist :: Vec -> Vec -> Double                -- 全変動距離
tvDist p q = foldl' (+) 0 (zipWith (\x y -> abs (x - y)) p q) / 2

-- エントロピー率 H = −Σ_i π_i Σ_j P_ij log₂ P_ij（1 歩あたりの情報量、ビット）
entropyRate :: Mat -> Vec -> Double
entropyRate a pv = foldl' (\h (row, p) -> foldl' (\h2 v -> if v > 0 then h2 - p * v * logBase 2 v else h2) h row) 0 (zip a pv)

posInf :: Double
posInf = 1 / 0

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

gPt :: Graph -> Double -> Double -> (Float, Float)
gPt g a b = (gX g a, gY g b)

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (col (84, 76, 70)) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (col sub) | v <- xt ]
  ++ [ sayRM (prec3 v) (grX g - 5) (gY g v) 14 (col sub) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 36) 14 (col cream), say yl (grX g) (grY g - 8) 14 (col cream) ]

-- arr !! i を x = i (+ xOff) に。長いときは間引いて 600 点まで
drawCurve :: Graph -> [Double] -> RGB -> Float -> Double -> Picture
drawCurve g arr c sw xOff =
  let n = length arr
      stride = max 1 (ceiling (i2d n / 600)) :: Int
      pts = [ (gX g (i2d i + xOff), gY g v) | (i, v) <- zip [0 ..] arr, i `mod` stride == 0 ]
            ++ [ (gX g (i2d (n - 1) + xOff), gY g (lastOr 0 arr)) | n > 1, (n - 1) `mod` stride /= 0 ]
  in Color (col c) (polyline sw pts)

legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures [ Pictures [Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col cream)]
                            | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- 棒グラフ（top = 縦軸の上限）
drawBars :: Float -> Float -> Float -> Float -> [Double] -> (Int -> RGB) -> Double -> Maybe [String] -> Picture
drawBars x y w h vals cols top names =
  let n = length vals
      bw = w / i2f (max 1 n)
  in Pictures $ Color (col (84, 76, 70)) (lineW 1 (x, y + h) (x + w, y + h))
       : [ Pictures [ Color (col (cols i)) (rectTL (x + i2f i * bw + bw * 0.12) (y + h - hh) (bw * 0.76) hh)
                    , case names of
                        Just ns -> sayT (at' ns i "") (x + i2f i * bw + bw / 2) (y + h + 4) 14 (col sub) True
                        Nothing -> Blank ]
         | (i, v) <- zip [0 ..] vals, let hh = f2 (clampv (v / (if top == 0 then 1 else top)) 0 1) * h ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

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

-- 見出しと操作の説明（左上）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures [ say t 40 42 22 (col cream), say s 40 68 15 (col sub) ]

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（iw × ih の画像）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = all (\v -> not (isNaN v || isInfinite v))

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col (120, 220, 190) else col (255, 90, 90))
