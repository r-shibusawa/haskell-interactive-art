-- ============================================
-- 68-06 損失の地形 ─ 隠れ 3 個（tanh）の小さな回帰ネットの重みは 10 個 → 損失は 10 次元の山と谷
-- 10 次元は見えないので、学習で着いた点 θ* を通る「平面」で切った断面を、等高線（左）と 3D（右）で描く：L(θ* + a d1 + b d2)
--   1 ランダムな 2 方向　2 学習の軌跡の主成分（いちばん動いた 2 方向）＋軌跡　3 隠れニューロンの入れかえ：θ* と、1 番と 2 番を入れかえた πθ*
-- 入れかえても関数はまったく同じ → 損失も同じ。だから谷は必ず何個もあり、まっすぐ結ぶと途中に峠ができる
-- 操作：1 2 3 断面 / D 方向を引き直す / R 別の種で学習し直す / ← → 3D を回す / スペース 回転を止める / T 軌跡のアニメ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (violet)
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when, unless)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.), (.&.), shiftL, testBit)
import Data.List (foldl', intercalate, sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (floatToDigits)
import System.Environment (lookupEnv)

hid, nPts, res :: Int
hid = 3                    -- ★ 隠れ層の数
nPts = 16                  -- ★ データの点の数
res = 120                  -- ★ 等高線の細かさ

rowsPerFrame :: Int
rowsPerFrame = 12          -- ★ 1 フレームに計算する等高線の行の数

sliceNames :: [String]
sliceNames = ["2 random directions", "principal components of the path", "swap symmetry of hidden neurons"]

-- データ：x と y
data Dat = Dat { dX :: !Vec, dY :: !Vec }

data World = World
  { wDat      :: !Dat
  , wStar     :: !Vec             -- 学習で着いた点 θ*
  , wInit     :: !Vec
  , wTraj     :: !(V.Vector Vec)  -- 学習の軌跡（10 歩ごと）
  , wSeedL    :: !Int
  , wMode     :: !Int
  , wDirs     :: !(Vec, Vec)
  , wRanges   :: !(Double, Double)
  , wRows     :: ![Vec]           -- 計算ずみの行（新しい順）
  , wGrid     :: !Vec             -- できあがった格子（res × res の log₁₀ L）
  , wConImg   :: !(Maybe Picture) -- 等高線の絵（格子ができたら 1 回だけ描く）
  , wRotA     :: !Double
  , wSpin     :: !Bool
  , wTrajT    :: !Int
  , wLossInit :: !Double
  , wRng      :: !Rng
  , wTest     :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "68-06 Loss landscape" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      _ <- evaluate (U.sum (wStar world0) + U.sum (fst (wDirs world0)))
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = retrain (World (Dat U.empty U.empty) U.empty U.empty V.empty 1 1 (U.empty, U.empty) (1, 1) [] U.empty Nothing 0.6 True 0 0 (rngOf 1) [])

-- ---- データとネット（θ = [w1 ×3, b1 ×3, w2 ×3, b2]、出力 = b2 + Σ w2_j tanh(w1_j x + b1_j)）
makeData :: Int -> Dat
makeData seed = withSeed seed $ do
  ps <- mapM (\i -> do g <- randn
                       let x = -1.5 + (3 * i2d i) / i2d (nPts - 1)
                       pure (x, sin (2.2 * x) + 0.1 * g)) [0 .. nPts - 1]
  pure (Dat (U.fromList (map fst ps)) (U.fromList (map snd ps)))

netOut :: Vec -> Double -> Double
netOut th x = foldl' (\o j -> o + (th U.! (2 * hid + j)) * tanh ((th U.! j) * x + th U.! (hid + j))) (th U.! (3 * hid)) [0 .. hid - 1]

lossOf :: Dat -> Vec -> Double
lossOf d th = U.foldl' (+) 0 (U.zipWith (\x y -> let e = netOut th x - y in e * e) (dX d) (dY d)) / i2d (2 * nPts)

gradOf :: Dat -> Vec -> Vec
gradOf d th = runST $ do
  g <- MU.replicate (U.length th) 0
  forM_ [0 .. nPts - 1] $ \i -> do
    let x = dX d U.! i
        hs = [ tanh ((th U.! j) * x + th U.! (hid + j)) | j <- [0 .. hid - 1] ]
        o = foldl' (\acc (j, h) -> acc + (th U.! (2 * hid + j)) * h) (th U.! (3 * hid)) (zip [0 ..] hs)
        dd = (o - dY d U.! i) / i2d nPts
    MU.modify g (+ dd) (3 * hid)
    forM_ (zip [0 ..] hs) $ \(j, h) -> do
      MU.modify g (+ dd * h) (2 * hid + j)
      let dz = dd * (th U.! (2 * hid + j)) * (1 - h * h)
      MU.modify g (+ dz) (hid + j)
      MU.modify g (+ dz * x) j
  U.freeze g

-- 隠れニューロン i と j を入れかえる（π）、ニューロン j の符号を全部反転（tanh は奇関数）
permuteHidden :: Vec -> Int -> Int -> Vec
permuteHidden th i j = th U.// concat [ [(blk * hid + i, th U.! (blk * hid + j)), (blk * hid + j, th U.! (blk * hid + i))] | blk <- [0 .. 2] ]

flipHidden :: Vec -> Int -> Vec
flipHidden th j = th U.// [ (blk * hid + j, negate (th U.! (blk * hid + j))) | blk <- [0 .. 2] ]

-- Adam で学習し、every 歩ごとに軌跡を記録
trainAdam :: Dat -> Vec -> Int -> Double -> Int -> V.Vector Vec
trainAdam d th0 iters lr every = V.fromList (reverse (go 1 th0 z z [th0]))
  where
    z = U.replicate (U.length th0) 0
    go t th m v path
      | t > iters = path
      | otherwise =
          let g = gradOf d th
              m' = U.zipWith (\mv gv -> 0.9 * mv + 0.1 * gv) m g
              v' = U.zipWith (\vv gv -> 0.999 * vv + 0.001 * gv * gv) v g
              c1 = 1 - 0.9 ** i2d t
              c2 = 1 - 0.999 ** i2d t
              th' = U.zipWith3 (\p mv vv -> clampv (p - (lr * mv) / c1 / (sqrt (vv / c2) + 1e-8)) (-30) 30) th m' v'
          in go (t + 1) th' m' v' (if t `mod` every == 0 then th' : path else path)

retrain :: World -> World
retrain w =
  let d = makeData (60 + wSeedL w)
      (th0, g) = runR (seedRng (600 + wSeedL w) >> (U.fromList <$> mapM (const ((0.8 *) <$> randn)) [1 .. 3 * hid + 1])) (rngOf 1)
      traj = trainAdam d th0 3000 0.03 10
  in makeDirs w { wDat = d, wInit = th0, wLossInit = lossOf d th0, wTraj = traj, wStar = V.last traj, wRng = g }

-- ★ 主成分：軌跡 (θ_t − θ*) の共分散行列の固有ベクトルを、べき乗法（＋ 1 本目を引いてもう 1 回）で
principalDirs :: V.Vector Vec -> Vec -> ([Vec], Double)
principalDirs path center = (outs, (at' lams 0 0 + at' lams 1 0) / max 1e-300 tr)
  where
    n = U.length center
    cov = V.generate n (\a -> U.generate n (\b -> V.foldl' (\s p -> s + (p U.! a - center U.! a) * (p U.! b - center U.! b)) 0 path))
    tr = foldl' (\s a -> s + (cov V.! a) U.! a) 0 [0 .. n - 1]
    (outs, lams) = foldl' one ([], []) [0, 1]
    one (out, ls) k =
      let v0 = U.generate n (\q -> sin (i2d q + 1 + i2d k))
          it 500 v = v
          it i v = let w0 = matVec cov v
                       w = foldl' (\ww o -> let c = dotV ww o in U.zipWith (\x oq -> x - c * oq) ww o) w0 out
                       nw = normV w
                   in if not (nw > 0) then v else it (i + 1 :: Int) (U.map (/ nw) w)
          v = it 0 v0
      in (out ++ [v], ls ++ [dotV v (matVec cov v)])

makeDirs :: World -> World
makeDirs w = case wMode w of
    0 -> let ((d1, d2), g) = runR ((,) <$> rdir <*> rdir) (wRng w)
         in reset w { wDirs = (d1, d2), wRanges = (1, 1), wRng = g }
    1 -> let (pc, _) = principalDirs (wTraj w) th
             p1 = at' pc 0 U.empty
             p2 = at' pc 1 U.empty
             ext pv = V.foldl' (\m p -> max m (abs (dotV pv (U.zipWith (-) p th)))) negInf (wTraj w)
         in reset w { wDirs = (p1, p2), wRanges = (max 0.3 (ext p1 * 1.2), max 0.3 (ext p2 * 1.2)) }
    _ -> let d1 = U.zipWith (-) (permuteHidden th 0 1) th
             (r0, g) = runR (U.fromList <$> mapM (const randn) [1 .. n]) (wRng w)
             c = dotV r0 d1 / max 1e-12 (dotV d1 d1)
             r = U.zipWith (\v dq -> v - c * dq) r0 d1
             nr = normV r
         in reset w { wDirs = (d1, U.map (\v -> v / nr * normV d1 * 0.5) r), wRanges = (1.5, 1), wRng = g }
  where
    th = wStar w
    n = U.length th
    sc = normV th
    rdir = do d <- U.fromList <$> mapM (const randn) [1 .. n]
              let nd = normV d
              pure (U.map (\v -> v / nd * sc * 0.6) d)
    reset x = x { wRows = [], wGrid = U.empty, wConImg = Nothing }

offA :: World -> Double
offA w = if wMode w == 2 then 0.25 else 0

sliceAB :: World -> Int -> Int -> (Double, Double)
sliceAB w i j = ((-1 + (2 * (i2d i + 0.5)) / i2d res) * fst (wRanges w) + offA w, (1 - (2 * (i2d j + 0.5)) / i2d res) * snd (wRanges w))

pointAt :: World -> Double -> Double -> Vec
pointAt w a b = let (d1, d2) = wDirs w in U.zipWith3 (\v p q -> v + a * p + b * q) (wStar w) d1 d2

-- 軌跡の点を平面の座標 (a, b) に（2 × 2 の最小 2 乗）
projectAB :: World -> Vec -> (Double, Double)
projectAB w th =
  let (d1, d2) = wDirs w
      d = U.zipWith (-) th (wStar w)
      g11 = dotV d1 d1
      g12 = dotV d1 d2
      g22 = dotV d2 d2
      r1 = dotV d d1
      r2 = dotV d d2
      det = g11 * g22 - g12 * g12
  in if det > 1e-300 then ((r1 * g22 - r2 * g12) / det, (r2 * g11 - r1 * g12) / det) else (0, 0)

lossCol :: Double -> Bool -> RGB
lossCol lg band = let t = clampv ((lg + 3) / 4) 0 1
                      c = if t < 0.5 then mixCol (20, 70, 90) lime (t * 2) else mixCol lime mag ((t - 0.5) * 2)
                      (r, g, b) = c
                  in if band then (r * 0.55, g * 0.55, b * 0.55) else c

gridDone :: World -> Bool
gridDone w = not (U.null (wGrid w))

-- 等高線：log₁₀ L を 1/3 ずつの帯にして、帯の境目を暗く
paintContours :: Vec -> Picture
paintContours grid = gridImage res res (\i j ->
  let v = grid U.! (j * res + i)
      bnd = floor (v * 3) :: Int
      edge = (i + 1 < res && floor (grid U.! (j * res + i + 1) * 3) /= bnd) || (j + 1 < res && floor (grid U.! ((j + 1) * res + i) * 3) /= bnd)
  in lossCol v edge)

step :: Env -> Float -> World -> IO World
step _ _ w0 = pure w2 { wRotA = if wSpin w2 then wRotA w2 + 0.004 else wRotA w2, wTrajT = (wTrajT w2 + 2) `mod` (V.length (wTraj w2) + 60) }
  where
    nDone = length (wRows w0)
    newRows = [ U.generate res (\i -> let (a, b) = sliceAB w0 i j in logBase 10 (max 1e-6 (lossOf (wDat w0) (pointAt w0 a b))))
              | j <- [nDone .. min res (nDone + rowsPerFrame) - 1] ]
    w1 = if gridDone w0 then w0 else w0 { wRows = reverse newRows ++ wRows w0 }
    w2 = if not (gridDone w1) && length (wRows w1) >= res
           then let grid = U.concat (reverse (wRows w1)) in w1 { wGrid = grid, wConImg = Just (paintContours grid) }
           else w1

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Loss landscape: " ++ at' sliceNames (wMode w) "" ++ "   L(theta*) = " ++ fmtE (lossOf d th) ++ " (start " ++ fmtN (wLossInit w) 3 ++ ")")
             ("1 2 3 slice   D new directions   R retrain with another seed   Left/Right turn 3D   Space spin " ++ (if wSpin w then "ON" else "OFF")
              ++ "   T replay path\n(10 weights, " ++ show nPts ++ " data points, Adam 3000 steps)")
  , case wConImg w of
      Just img -> imageBox 40 100 sz sz res res img
      Nothing -> Pictures [Color (col panel) (rectTL 40 100 sz sz), say ("computing... " ++ show (length (wRows w)) ++ " / " ++ show res) 60 130 14 (col sub)]
  , if wMode w /= 2
      then let pts = filter (\(px, _) -> px > 40 - 20 && px < 40 + sz + 20) [ toS (projectAB w p) | p <- V.toList (wTraj w) ]
               (qx, qy) = toS (projectAB w (wTraj w V.! min (V.length (wTraj w) - 1) (wTrajT w)))
           in Pictures [Color (colA 200 ink) (polyline 1.5 pts), Color (col amber) (circleAt qx qy 11)]
      else Blank
  , let (sx, sy) = toS (0, 0) in Pictures [Color (col ink) (ringAt sx sy 14 2), say "theta*" (sx + 10) (sy - 8) 14 (col ink)]
  , if wMode w == 2 then let (sx, sy) = toS (1, 0) in Pictures [Color (col ink) (ringAt sx sy 14 1), say "pi theta* (swapped)" (sx + 10) (sy - 8) 14 (col ink)] else Blank
  , say note 40 (100 + sz + 22) 14 (col sub)
  , say "color: dark blue = low, lime -> magenta = high (bands of log10 L = contours)" 40 (100 + sz + 42) 14 (col sub)
  -- ---- 右：3D のワイヤーフレーム（2D の画面に自分で射影）
  , if gridDone w then drawWire w cx cy sc3 else Blank
  , if wMode w == 2 && gridDone w
      then let line1 = [ lossOf d (pointAt w a 0) | i <- [0 .. res - 1], let (a, _) = sliceAB w i (res `div` 2), a >= 0 && a <= 1 ]
           in say ("on the straight line theta* -> pi theta*, highest L = " ++ fmtN (maxOr 0 line1) 4 ++ " (both ends " ++ fmtE (lossOf d th) ++ ")") (40 + sz + 40) (envH env - 60) 14 (col ink)
      else Blank
  , drawSelfTest env (wTest w) ]
  where
    d = wDat w
    th = wStar w
    sz = min (envW env * 0.42) (envH env - 210)
    (ra, rb) = wRanges w
    toS (a, b) = (40 + f2 (((a - offA w) / ra + 1) / 2) * sz, 100 + f2 (1 - (b / rb + 1) / 2) * sz)
    note = case wMode w of
      0 -> "random directions usually look like a bowl (only a tiny part of 10-D)"
      1 -> "white = training path projected on the plane, orange = current point"
      _ -> "two valleys of the same depth; the straight path between them climbs a hill"
    cx = 40 + sz + (envW env - 40 - sz) / 2
    cy = 100 + sz * 0.5
    sc3 = min (envW env - 40 - sz - 60) sz * 0.42

-- 高さ = log₁₀ L。40 × 40 の網を回して斜めから見る
drawWire :: World -> Float -> Float -> Float -> Picture
drawWire w cx cy sc = Pictures $
  [ Color (colA 200 (lossCol (gridAt (rowOf j) (res `div` 2)) False)) (polyline 1 [ proj (u i) (u j) (hAt i j) | i <- [0 .. n] ]) | j <- [0 .. n] ]
  ++ [ Color (colA 220 wire) (polyline 1 [ proj (u i) (u j) (hAt i j) | j <- [0 .. n] ]) | i <- [0 .. n] ]
  ++ [ let (px, py) = proj (if wMode w == 2 then -0.25 / fst (wRanges w) * 1 else 0) 0 (clampv ((logBase 10 (max 1e-6 (lossOf (wDat w) (wStar w))) + 1) / 2) (-1.5) 1.5)
       in Color (col ink) (circleAt px py 10) ]
  where
    n = 40 :: Int
    step3 = i2d res / i2d n
    ca = cos (wRotA w)
    sa = sin (wRotA w)
    u k = i2d k / i2d n * 2 - 1
    rowOf k = min (res - 1) (jsRound (i2d k * step3))
    gridAt r c = wGrid w U.! (r * res + c)
    hAt i j = clampv ((gridAt (rowOf j) (rowOf i) + 1) / 2) (-1.5) 1.5
    proj uu vv h = let x = uu * ca - vv * sa
                       y = uu * sa + vv * ca
                   in (cx + f2 x * sc, cy + f2 y * sc * 0.45 - f2 h * sc * 0.35)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "123" -> makeDirs w { wMode = fromEnum c - fromEnum '1', wRng = rngOf (700 + envFrame env) }
    | c `elem` "dD" -> makeDirs w { wRng = rngOf (800 + envFrame env) }
    | c `elem` "rR" -> retrain w { wSeedL = wSeedL w + 1 }
    | c `elem` "tT" -> w { wTrajT = 0 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wRotA = wRotA w + 0.2 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wRotA = wRotA w - 0.2 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wSpin = not (wSpin w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1 で D を何回か押す。どの方向で切っても θ* は谷底に見える？
-- 2. 2 の軌跡：まっすぐ谷へ向かう？ 途中で平らな所（ゆっくり進む所）はある？
-- 3. 3 で、2 つの谷の深さが本当に同じか確かめる。hid を 2 や 6 にすると入れかえ方は何通り？（ヒント：hid! × 2^hid）
-- 4. nPts を 6 に減らすと、谷は広くなる？ 細くなる？

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("勾配 = 中心差分（相対誤差 " ++ fmtE worst ++ "）", worst < 1e-6)
  , ("Adam 3000 歩で損失が 1/10 以下（" ++ fmtN l0 3 ++ " → " ++ fmtE l1 ++ "）、着いた所の勾配は小さい", l1 < l0 / 10 && normV (gradOf d ts) < 1e-3)
  , ("隠れニューロンの入れかえ・符号の反転で損失は変わらない（ずれ " ++ fmtE (max dp df) ++ "）", dp < 1e-12 && df < 1e-12)
  , ("θ* と πθ* の真ん中は谷より高い（峠：" ++ fmtN (lossOf d mid) 3 ++ " > " ++ fmtE l1 ++ "）", lossOf d mid > 2 * l1)
  , ("主成分の 2 方向は長さ 1 で直交、軌跡の動きの " ++ fmtN (100 * expl) 0 ++ "% を説明"
    , near (normV p1) 1 1e-9 && near (normV p2) 1 1e-9 && abs (dotV p1 p2) < 1e-6 && expl > 0.6) ]
  where
    d = makeData 61
    th0 = withSeed 601 (U.fromList <$> mapM (const ((0.8 *) <$> randn)) [1 .. 3 * hid + 1])
    g = gradOf d th0
    worst = foldl' (\m q -> let a = th0 U.// [(q, th0 U.! q + 1e-6)]
                                b = th0 U.// [(q, th0 U.! q - 1e-6)]
                            in max m (abs (g U.! q - (lossOf d a - lossOf d b) / 2e-6) / max 1e-6 (abs (g U.! q)))) 0 [0 .. U.length th0 - 1]
    path = trainAdam d th0 3000 0.03 10
    ts = V.last path
    l0 = lossOf d th0
    l1 = lossOf d ts
    dp = abs (lossOf d (permuteHidden ts 0 1) - l1)
    df = abs (lossOf d (flipHidden ts 2) - l1)
    mid = U.zipWith (\v p -> (v + p) / 2) ts (permuteHidden ts 0 1)
    (pcs, expl) = principalDirs path ts
    p1 = at' pcs 0 U.empty
    p2 = at' pcs 1 U.empty

-- ==================================================
-- ここから下は「神経回路網の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。座標は p5.js 版と同じ（左上が原点、y は下向き）で、view で gloss の座標に直す
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（神経細胞：深い青緑の黒、あたたかい白、シナプスのマゼンタ、軸索のライム）
type RGB = (Double, Double, Double)
type Vec = U.Vector Double                    -- ベクトル（unboxed）

bgCol, ink, mag, lime, amber, sky, violet, sub, panel, wire :: RGB
bgCol = (8, 22, 24)                           -- 背景（深い青緑の黒）
ink = (246, 238, 224)                         -- 文字（あたたかい白）
mag = (232, 72, 160)
lime = (184, 232, 76)
amber = (250, 186, 72)
sky = (96, 196, 236)
violet = (168, 140, 250)
sub = (150, 178, 172)
panel = (16, 38, 40)
wire = (44, 76, 76)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Double -> RGB -> Color                -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Double -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

clampI :: Int -> Int -> Int -> Int
clampI v a b = max a (min b v)

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

f2 :: Double -> Float                         -- 描くときの Float（NaN と無限大は 0）
f2 v = if isNaN v || isInfinite v then 0 else realToFrac v

d2 :: Float -> Double
d2 = realToFrac

isFin :: Double -> Bool
isFin v = not (isNaN v || isInfinite v)

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

sumOf :: [Double] -> Double
sumOf = foldl' (+) 0

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else sumOf a / i2d (length a)

sigm :: Double -> Double
sigm z = 1 / (1 + exp (-(clampv z (-40) 40)))

-- JavaScript の Math.round（.5 は大きいほうへ）と Math.floor
jsRound :: Double -> Int
jsRound v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e15) 1e15)

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

-- 最初にいちばん大きい値の番号（indexOf(Math.max(...))）
argMax :: [Double] -> Int
argMax xs = fst (foldl' (\(bi, bv) (i, v) -> if v > bv then (i, v) else (bi, bv)) (0, headOr 0 xs) (zip [0 ..] xs))

-- 数を書く（p5.js 版の fmtN：toFixed(d)、"-0.00" は "0.00" に）
fmtN :: Double -> Int -> String
fmtN v d
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = (if v < 0 && n > 0 then "-" else "") ++ body
  where
    n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
    ds = show n
    padded = replicate (d + 1 - length ds) '0' ++ ds
    (ip, fp) = splitAt (length padded - d) padded
    body = if d > 0 then ip ++ "." ++ fp else ip

-- p5.js 版の fmtE：toExponential(1)（例 6.7e-9）。0 ちょうどは "0"
fmtE :: Double -> String
fmtE v
  | isNaN v || isInfinite v = "?"
  | v == 0 = "0"
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          pick e = (floor (a / (10 ^^ (e - 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= 100 then pick (e0 + 1) else if n < 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= 100 then (n1 `div` 10, e1 + 1) else (n1, e1)
          ds = show n2
      in (if v < 0 then "-" else "") ++ take 1 ds ++ "." ++ drop 1 ds ++ "e" ++ (if e2 >= 0 then "+" else "-") ++ show (abs e2)

-- JavaScript の数の書き方（String(v)：いちばん短い 10 進数）
jsNum :: Double -> String
jsNum v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0"
  | v < 0 = '-' : jsNum (negate v)
  | otherwise =
      let (ds0, e) = floatToDigits 10 v
          ds = concatMap show ds0
          k = length ds
      in if e >= k && e <= 21 then ds ++ replicate (e - k) '0'
         else if e > 0 && e <= 21 then take e ds ++ "." ++ drop e ds
         else if e > -6 && e <= 0 then "0." ++ replicate (negate e) '0' ++ ds
         else take 1 ds ++ (if k > 1 then "." ++ drop 1 ds else "") ++ "e" ++ (if e - 1 >= 0 then "+" else "-") ++ show (abs (e - 1))

-- +v.toPrecision(k)（有効数字 k 桁に丸めた数）と、その書き方（目盛りの数）
roundPrec :: Int -> Double -> Double
roundPrec k v
  | not (isFin v) || v == 0 = 0
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          top = 10 ^ k :: Integer
          pick e = (floor (a / (10 ^^ (e - k + 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= top then pick (e0 + 1) else if n < top `div` 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= top then (n1 `div` 10, e1 + 1) else (n1, e1)
          r = fromRational (fromIntegral n2 * 10 ^^ (e2 - k + 1)) :: Double
      in if v < 0 then negate r else r

precN :: Int -> Double -> String
precN k = jsNum . roundPrec k

prec3 :: Double -> String
prec3 = precN 3

-- 左上のタイトルと操作の説明（説明は "\n" で 2 行にできる）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col sub) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- mulberry32 は状態に毎回同じ数 0x6d2b79f5 を足すだけ → k 回目の乱数は「状態 + k × その数」から直接作れる
rndOut :: Word32 -> Double
rndOut a =
  let t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296

rngStep :: Word32
rngStep = 0x6d2b79f5

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：列を種 s から始め直す（0 なら 1）
seedRng :: Int -> R ()
seedRng s = R (\_ -> let w = fromIntegral s :: Word32 in ((), Rng (if w == 0 then 1 else w)))

rngOf :: Int -> Rng
rngOf s = snd (runR (seedRng s) (Rng 1))

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double                               -- 0〜1 の一様乱数
rnd = R (\(Rng s) -> let a = s + rngStep in (rndOut a, Rng a))

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * i2d (b - a + 1)))

-- 標準正規分布（ボックス-ミュラー、u = 1 − rnd() なので log(0) にならない）
randn :: R Double
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- rnd() を n 回、まとめて（unboxed のベクトルに）
rndVec :: Int -> R (U.Vector Double)
rndVec n = R (\(Rng s) -> (U.generate n (\k -> rndOut (s + fromIntegral (k + 1) * rngStep)), Rng (s + fromIntegral n * rngStep)))

-- randn() を n 回、まとめて
randnVec :: Int -> R (U.Vector Double)
randnVec n = R (\(Rng s) -> (U.generate n (gauss s), Rng (s + fromIntegral (2 * n) * rngStep)))
  where gauss s k = let u = 1 - rndOut (s + fromIntegral (2 * k + 1) * rngStep)
                        v = rndOut (s + fromIntegral (2 * k + 2) * rngStep)
                    in sqrt (-2 * log u) * cos (2 * pi * v)

-- フィッシャー-イェーツのシャッフル（後ろから i と 0〜i のどこかを入れかえる）
shuffleU :: U.Unbox a => U.Vector a -> R (U.Vector a)
shuffleU v = do
  let n = U.length v
  js <- mapM (\i -> do u <- rnd; pure (i, floor (u * i2d (i + 1)))) [n - 1, n - 2 .. 1]
  pure (U.modify (\mv -> mapM_ (\(i, j) -> MU.swap mv i j) js) v)

-- ---- 行列の道具（ベクトル = U.Vector Double、行列 = 行のベクトル）
type Mat = V.Vector Vec

dotV :: Vec -> Vec -> Double
dotV a b = go 0 0
  where n = min (U.length a) (U.length b)
        go !i !s = if i >= n then s else go (i + 1) (s + U.unsafeIndex a i * U.unsafeIndex b i)

normV :: Vec -> Double
normV a = sqrt (dotV a a)

matVec :: Mat -> Vec -> Vec
matVec m v = U.generate (V.length m) (\i -> dotV (m V.! i) v)

zerosMat :: Int -> Int -> Mat
zerosMat n m = V.replicate n (U.replicate m 0)

-- ガウスの消去法（部分ピボット）で a x = b（a は n × n を行の順に並べたもの）。解けなければ Nothing
solveLinearU :: Int -> U.Vector Double -> U.Vector Double -> Maybe Vec
solveLinearU n a b = runST $ do
  let w = n + 1
  m <- MU.new (n * w)
  forM_ [0 .. n - 1] $ \r -> do
    forM_ [0 .. n - 1] $ \k -> MU.write m (r * w + k) (a U.! (r * n + k))
    MU.write m (r * w + n) (b U.! r)
  let elim c
        | c >= n = pure True
        | otherwise = do
            let findP !p !r
                  | r >= n = pure p
                  | otherwise = do
                      x <- MU.read m (r * w + c)
                      y <- MU.read m (p * w + c)
                      findP (if abs x > abs y then r else p) (r + 1)
            p <- findP c (c + 1)
            mp <- MU.read m (p * w + c)
            if abs mp < 1e-300 then pure False else do
              when (p /= c) $ forM_ [0 .. n] $ \k -> MU.swap m (c * w + k) (p * w + k)
              mcc <- MU.read m (c * w + c)
              forM_ [c + 1 .. n - 1] $ \r -> do
                mrc <- MU.read m (r * w + c)
                let f = mrc / mcc
                when (f /= 0) $ forM_ [c .. n] $ \k -> do
                  ck <- MU.read m (c * w + k)
                  MU.modify m (\v -> v - f * ck) (r * w + k)
              elim (c + 1)
  ok <- elim 0
  if not ok then pure Nothing else do
    x <- MU.replicate n 0
    forM_ [n - 1, n - 2 .. 0] $ \i -> do
      s0 <- MU.read m (i * w + n)
      let go !k !s | k >= n = pure s
                   | otherwise = do mik <- MU.read m (i * w + k); xk <- MU.read x k; go (k + 1) (s - mik * xk)
      s <- go (i + 1) s0
      mii <- MU.read m (i * w + i)
      MU.write x i (s / mii)
    xs <- U.freeze x
    pure (if U.all isFin xs then Just xs else Nothing)

solveLinear :: Mat -> Vec -> Maybe Vec
solveLinear a b = solveLinearU (U.length b) (U.concat (V.toList a)) b

-- リッジ回帰 (ΦᵀΦ + λI) w = Φᵀ y。phi = 行がデータ、列が特徴
ridgeSolve :: Mat -> Vec -> Double -> Vec
ridgeSolve phi y lam = fromMaybe (U.replicate p 0) (solveLinearU p a b)
  where
    p = U.length (if V.null phi then U.empty else V.head phi)
    (a, b) = runST $ do
      am <- MU.replicate (p * p) 0
      bm <- MU.replicate p 0
      forM_ [0 .. V.length phi - 1] $ \i -> do
        let f = phi V.! i
            yi = y U.! i
        forM_ [0 .. p - 1] $ \j -> do
          let fj = U.unsafeIndex f j
          MU.modify bm (+ fj * yi) j
          forM_ [j .. p - 1] $ \k -> MU.unsafeModify am (+ fj * U.unsafeIndex f k) (j * p + k)
      forM_ [0 .. p - 1] $ \j -> do
        MU.modify am (+ lam) (j * p + j)
        forM_ [0 .. j - 1] $ \k -> MU.read am (k * p + j) >>= MU.write am (j * p + k)
      (,) <$> U.freeze am <*> U.freeze bm

-- ---- 画像の道具：幅 iw × 高さ ih の画像の全部の画素に fn u v → (r, g, b) を塗る（u は右向き、v は上向き、どちらも 0〜1）
paintImage :: Int -> Int -> (Double -> Double -> RGB) -> Picture
paintImage iw ih fn = pixelImage iw ih 1 (\px py ->
  let (r, g, b) = fn ((i2d px + 0.5) / i2d iw) (1 - (i2d py + 0.5) / i2d ih)
  in (w8 r, w8 g, w8 b))

-- マス目の画像：fn i j（i = 左から、j = 上から）→ (r, g, b)
gridImage :: Int -> Int -> (Int -> Int -> RGB) -> Picture
gridImage iw ih fn = pixelImage iw ih 1 (\i j -> let (r, g, b) = fn i j in (w8 r, w8 g, w8 b))

w8 :: Double -> Word8                         -- Uint8ClampedArray と同じ丸め（ちょうど半分は偶数へ）
w8 v = fromIntegral (c8 v)

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（画面の道具の座標で）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / i2f iw) (negate h / i2f ih) pic)

mixCol :: RGB -> RGB -> Double -> RGB
mixCol (a0, a1, a2) (b0, b1, b2) t0 = let t = clampv t0 0 1 in (a0 + (b0 - a0) * t, a1 + (b1 - a1) * t, a2 + (b2 - a2) * t)

divCol :: Double -> RGB                       -- −1〜1 → ライム・暗い・マゼンタ
divCol v = if v >= 0 then mixCol panel mag v else mixCol panel lime (negate v)

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (col wire) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (col sub) | v <- xt ]
  ++ [ sayRM (prec3 v) (grX g - 5) (gY g v) 14 (col sub) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 36) 14 (col ink), say yl (grX g) (grY g - 8) 14 (col ink) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ 1.5 までは gloss の細い線）
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1.5 then Line pts else thickPath w pts
  _ -> Blank

-- (xs[i], ys[i]) を折れ線で（有限の値だけ）
plotXY :: Graph -> [Double] -> [Double] -> Color -> Float -> Picture
plotXY g xs ys c w = Color c (polyline w [ (gX g x, gY g y) | (x, y) <- zip xs ys, isFin y ])

-- 凡例：items = [(色, 名前)]
legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures
  [ Pictures [Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col ink)]
  | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）、右揃え・左揃えで上下は中央
sayM, sayRM, sayLM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c
sayLM s x y size c = say s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = i2f (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = if wt <= 1.2 then Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
                     else thickPath wt [(x - wt / 2, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分（細いときは gloss の線）
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW wt a b = if wt <= 1.5 then Line [a, b] else thickLine wt a b

-- 線分を箱（左上 x, y、幅 w、高さ h）の中だけに切る（p5.js 版の clip の代わり）
clipSeg :: Float -> Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Maybe ((Float, Float), (Float, Float))
clipSeg bx by bw bh (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
      edges = [(-dx, x1 - bx), (dx, bx + bw - x1), (-dy, y1 - by), (dy, by + bh - y1)]
      step (t0, t1) (p, q)
        | p == 0 = if q < 0 then (1, 0) else (t0, t1)
        | p < 0 = (max t0 (q / p), t1)
        | otherwise = (t0, min t1 (q / p))
      (a, b) = foldl' step (0, 1) edges
  in if a > b then Nothing else Just ((x1 + a * dx, y1 + a * dy), (x1 + b * dx, y1 + b * dy))

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashed on off (x1, y1) (x2, y2) =
  let l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
      k = max 1 (ceiling (l / (on + off))) :: Int
      p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
      fr = on / (on + off)
  in Pictures [ Line [p (i2f i / i2f k), p ((i2f i + fr) / i2f k)] | i <- [0 .. k - 1] ]

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = isFin a && abs (a - b) <= tol

relErr :: Double -> Double -> Double
relErr a b = abs (a - b) / max 1e-12 (abs a + abs b)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then makeColorI 150 230 170 255 else makeColorI 255 90 110 255)
