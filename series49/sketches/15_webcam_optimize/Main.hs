-- ============================================
-- 49-15 カメラ画像で最適化 📷
-- 画像の明るさを関数 f(u, v) とみなし（ぼかした明るさの格子を双線形補間）、たくさんの粒子が勾配法で暗い所へ流れる
--   v ← βv − η∇f + ノイズ、x ← x + v（β = 慣性、04 の重い球）。軌跡が絵になる。カメラが使えないときはテスト画像
-- 操作：↑↓ = ステップ幅 η / M = 慣性 / N = ノイズ / V = 関数（明るさ）を表示 / T = テスト画像 / スペース = 止める / C = 消す / S = 保存
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', sort)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

gw, gh :: Int
gw = 96                        -- ★ 明るさの格子（横・縦のます目）
gh = 72

np :: Int
np = 1600                      -- ★ 粒子の数

tw, th :: Int
tw = 320                       -- 軌跡を描きためる画像の大きさ（画面の 1/4。8 万画素以下）
th = 200

data Part = Part { pU, pV, pVu, pVv :: !Double }

data World = World
  { wCam    :: Maybe Camera
  , wLive   :: !Bool                 -- いまカメラの映像を使っているか
  , wEta    :: !Double
  , wBeta   :: !Double
  , wNoise  :: !Bool
  , wShowF  :: !Bool
  , wUseTest :: !Bool
  , wPaused :: !Bool
  , wGrid   :: !(U.Vector Double)    -- ぼかした明るさ（0〜1）
  , wBase   :: !(U.Vector (Float, Float, Float))    -- 背景の色（明るさの関数を薄く。格子を変えたときだけ計算）
  , wTrail  :: !(U.Vector Float)     -- 軌跡（色 × 濃さ の r, g, b と 濃さ a。p5.js の TR に当たる）
  , wParts  :: [Part]
  , wTt     :: !Double
  , wFMean  :: [Double]              -- 粒子の f の平均（新しい順、300 個まで）
  , wRnd    :: [Double]
  , wTests  :: [Test]
  }

main :: IO ()
main = runSketch (sketch "49-15 Optimizing on the camera image" world0)
  { skBack  = cBg
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera 160 120
      seed <- randomRIO (1, 1000000)
      let rs = rngList seed
          (ps, rs') = spawnN np rs
      updateGrid world0 { wCam = Just cam, wParts = ps, wRnd = rs', wTests = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World Nothing False 24 0.85 True True False False (U.replicate (gw * gh) 0.5) (U.replicate (tw * th) (0, 0, 0)) (U.replicate (tw * th * 4) 0) [] 0 [] (rngList 1) []

spawn :: [Double] -> (Part, [Double])
spawn (a : b : rs) = (Part (a * fromIntegral (gw - 1)) (b * fromIntegral (gh - 1)) 0 0, rs)
spawn rs = (Part 0 0 0 0, rs)

spawnN :: Int -> [Double] -> ([Part], [Double])
spawnN 0 rs = ([], rs)
spawnN n rs = let (p, rs1) = spawn rs; (ps, rs2) = spawnN (n - 1) rs1 in (p : ps, rs2)

-- 明るさの格子をつくる：カメラ（なければテスト画像）→ 2 回ぼかす
updateGrid :: World -> IO World
updateGrid w = do
  (raw, ok) <- case wCam w of
    Just cam | not (wUseTest w) -> do
      live <- camLive cam
      if not live then pure (U.empty, False) else do
        fr <- cameraFrame cam
        let (cw, chh) = cameraSize cam
            r = U.generate (gw * gh) $ \q ->
                  let (j, i) = q `divMod` gw
                  in realToFrac (camGray cam fr (i * cw `div` gw) (j * chh `div` gh)) :: Double
        pure (r, U.any (> 0.02) r)
    _ -> pure (U.empty, False)
  let raw' = if ok then raw else U.generate (gw * gh) (\q -> let (j, i) = q `divMod` gw in testImage i j (wTt w))
  let grid = boxBlur (boxBlur raw' gw gh) gw gh
      ta = 150 / 255 :: Double
      base = U.generate (tw * th) $ \pxl ->
        let (y, x) = pxl `divMod` tw
            u = (fromIntegral x + 0.5) / fromIntegral tw * fromIntegral gw - 0.5
            v = (fromIntegral y + 0.5) / fromIntegral th * fromIntegral gh - 0.5
            (b, _, _) = bil grid gw gh u v
        in ( realToFrac (12 * (1 - ta) + (20 + 120 * b) * ta)
           , realToFrac (36 * (1 - ta) + (45 + 150 * b) * ta)
           , realToFrac (69 * (1 - ta) + (80 + 160 * b) * ta) )
  pure w { wGrid = grid, wBase = base, wLive = ok }

-- テスト画像：ゆっくり動くしま模様 ＋ 暗い穴 2 つ
testImage :: Int -> Int -> Double -> Double
testImage i0 j0 t =
  0.55 + 0.3 * sin (i * 0.16 + t) * cos (j * 0.13 - 0.7 * t)
  - 0.4 * exp (-((i - 30 - 10 * cos t) ^ (2 :: Int) + (j - 36) ^ (2 :: Int)) / 120)
  - 0.35 * exp (-((i - 70) ^ (2 :: Int) + (j - 30 - 8 * sin t) ^ (2 :: Int)) / 90)
  where
    i = fromIntegral i0
    j = fromIntegral j0

-- 3×3 の平均（端は内側だけで平均）
boxBlur :: U.Vector Double -> Int -> Int -> U.Vector Double
boxBlur a w h = U.generate (w * h) $ \q ->
  let (j, i) = q `divMod` w
      vs = [a U.! (b * w + c) | dj <- [-1, 0, 1], di <- [-1, 0, 1], let c = i + di, let b = j + dj, c >= 0, c < w, b >= 0, b < h]
  in sum vs / fromIntegral (length vs)

-- 双線形補間の値と勾配（ます目の中では本当の勾配）：(f, ∂f/∂u, ∂f/∂v)
bil :: U.Vector Double -> Int -> Int -> Double -> Double -> (Double, Double, Double)
bil g w h u0 v0 = ((1 - s) * (1 - t) * a + s * (1 - t) * b + (1 - s) * t * c + s * t * d, (1 - t) * (b - a) + t * (d - c), (1 - s) * (c - a) + s * (d - b))
  where
    u = max 0 (min (fromIntegral (w - 1)) u0)
    v = max 0 (min (fromIntegral (h - 1)) v0)
    i = min (w - 2) (floor u)
    j = min (h - 2) (floor v)
    s = u - fromIntegral i
    t = v - fromIntegral j
    a = g U.! (j * w + i)
    b = g U.! (j * w + i + 1)
    c = g U.! ((j + 1) * w + i)
    d = g U.! ((j + 1) * w + i + 1)

-- 粒子を 1 回動かす（慣性つき勾配法）。返すのは（粒子、動く前の f、残りの乱数）
move :: U.Vector Double -> Double -> Double -> Double -> Part -> [Double] -> (Part, Double, [Double])
move g eta beta noise p rs0 = (Part u' v' vu2 vv2, f, rs2)
  where
    (f, gu, gv) = bil g gw gh (pU p) (pV p)
    (r1, rs1) = next rs0
    (r2, rs2) = next rs1
    vu = beta * pVu p - eta * gu + noise * (r1 - 0.5)
    vv = beta * pVv p - eta * gv + noise * (r2 - 0.5)
    sp = hypot vu vv
    k = if sp <= 2 then 1 else if sp > 0 && finite sp then 2 / sp else 0   -- 1 回に 2 ます以内
    vu2 = vu * k
    vv2 = vv * k
    u' = max 0 (min (fromIntegral (gw - 1)) (pU p + vu2))
    v' = max 0 (min (fromIntegral (gh - 1)) (pV p + vv2))
    next (x : xs) = (x, xs)
    next [] = (0.5, [])

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  w1 <- if envFrame env `mod` 3 == 0 then updateGrid w0 { wTt = wTt w0 + 0.03 } else pure w0
  let sxs = fromIntegral tw / fromIntegral (gw - 1) :: Double     -- 格子 → 軌跡の画像
      sys = fromIntegral th / fromIntegral (gh - 1) :: Double
      noise = if wNoise w1 then 0.25 else 0
      -- 粒子を動かし、動いた線分（速さで 3 色）を集める
      go [] rs accP accS sm = (reverse accP, accS, sm, rs)
      go (p : ps) rs accP accS sm =
        let (q, f, rs1) = move (wGrid w1) (wEta w1) (wBeta w1) noise p rs
            k = min 2 (floor (hypot (pVu q) (pVv q) * 2.2)) :: Int
            seg = (k, pU p * sxs, pV p * sys, pU q * sxs, pV q * sys)
            (q', rs2) = case rs1 of
              (r : rest) | r < 0.004 -> spawn rest
              (_ : rest) -> (q, rest)
              [] -> (q, [])
        in go ps rs2 (q' : accP) (seg : accS) (sm + f)
      (ps', segs, sm, rs') = if wPaused w1 then (wParts w1, [], 0, wRnd w1) else go (wParts w1) (wRnd w1) [] [] 0
      trail = U.modify (\mv -> forM_ segs (plotSeg mv)) (U.map (* 0.965) (wTrail w1))   -- 少しずつ消えてから、新しい線を重ねる
  pure w1 { wParts = ps', wTrail = trail, wRnd = rs'
          , wFMean = if wPaused w1 then wFMean w1 else take 300 (sm / fromIntegral np : wFMean w1) }

-- 線分を軌跡の画像に重ねて塗る（色 × 濃さ で足しこむ）
plotSeg :: MU.MVector s Float -> (Int, Double, Double, Double, Double) -> ST s ()
plotSeg mv (k, x0, y0, x1, y1) =
  forM_ [0 .. n] $ \s -> do
    let t = fromIntegral s / fromIntegral (max 1 n) :: Double
        x = floor (x0 + (x1 - x0) * t) :: Int
        y = floor (y0 + (y1 - y0) * t) :: Int
    when (x >= 0 && x < tw && y >= 0 && y < th) $ do
      let o = (y * tw + x) * 4
      let blend c v = MU.read mv (o + c) >>= \old -> MU.write mv (o + c) (v * al + old * (1 - al))
      blend 0 r
      blend 1 g
      blend 2 b
      blend 3 1
  where
    n = ceiling (max (abs (x1 - x0)) (abs (y1 - y0))) :: Int
    (r, g, b, a0) = getAt (95, 216, 240, 0.5) [(95, 216, 240, 0.5), (255, 210, 74, 0.6), (255, 122, 47, 0.75)] k
    al = a0 * 0.45 :: Float     -- 画素が画面の 4 倍なので、線の太さ 1.6 px のぶん薄く

draw :: Env -> World -> Picture
draw env w = view env
  [ Translate (envW env / 2) (envH env / 2) (Scale 1 (-1) (pixelImage tw th (envW env / fromIntegral tw) pix))
  , Color (rgba255 12 36 69 200) (rectTL 10 8 770 96)
  , hud [ ("Optimize on the camera image -- go down brightness f (" ++ (if wLive w then "camera" else "test image") ++ ")", cInk)
        , ("Up/Down eta = " ++ fmt 3 (wEta w) ++ "   M inertia beta = " ++ fmt 3 (wBeta w) ++ "   N noise " ++ (if wNoise w then "ON" else "off") ++ "   V function " ++ (if wShowF w then "ON" else "off"), cDim)
        , ("T test image   space stop   C clear   S save", cDim)
        , ("mean f of particles = " ++ fmt 3 (getAt 0 (wFMean w) 0) ++ " (whole image " ++ fmt 3 (U.sum (wGrid w) / fromIntegral (U.length (wGrid w))) ++ ")  <- falling = gathering in the dark", cYe) ] 20 14 14 1.45
  , Color (rgba255 12 36 69 200) (rectTL 8 (envH env - 34) 160 28)
  , drawSelfTest env (wTests w) ]
  where
    tr = wTrail w
    bs = wBase w
    -- 背景（明るさの関数を薄く）＋ 軌跡
    pix x y =
      let q = y * tw + x
          o = q * 4
          a = tr U.! (o + 3)
          (b0, b1, b2) = if wShowF w then bs U.! q else (12, 36, 69)
          ch c base = let v = tr U.! (o + c) + base * (1 - a) in fromIntegral (max 0 (min 255 (floor v :: Int)))
      in (ch 0 b0, ch 1 b1, ch 2 b2)

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wEta = min 400 (wEta w * 1.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wEta = max 1 (wEta w / 1.5) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "mM" -> pure w { wBeta = if wBeta w > 0 then 0 else 0.85 }
    | c `elem` "nN" -> pure w { wNoise = not (wNoise w) }
    | c `elem` "vV" -> pure w { wShowF = not (wShowF w) }
    | c `elem` "tT" -> pure w { wUseTest = not (wUseTest w) }
    | c `elem` "cC" -> pure w { wTrail = U.replicate (tw * th * 4) 0 }
    | c `elem` "sS" -> saveSvg "optimize_brush" env (draw env w) >> pure w
  _ -> pure w

---------------------------------------------------------------- 自己テスト

selfTest :: [Test]
selfTest =
  [ stOk "bilinear gradient = finite differences (inside the cells)" (eFD < 1e-8) (fmt 3 eFD)
  , stOk "at grid points the interpolation = the original value" (eNode < 1e-15) ""
  , stOk "quadratic on a grid: |interpolated grad - true grad| <= 1/2 max|f''| (cell 1)" (eTrue <= 0.5 * (4 / 1000) + 1e-12) (fmt 3 eTrue)
  , stOk "gradient descent (Armijo): f drops every step; all 20 starts end within half a cell of the darkest node (30, 21)" (okGD && mono) ""
  , stOk "blurring a uniform image leaves it unchanged" (U.all (\b -> abs (b - 0.37) < 1e-15) (boxBlur (U.replicate (w * h) 0.37) w h)) ""
  , stOk "even with eta = 1e9 the particle stays on screen and finite (at most 2 cells per step)" okClamp "" ]
  where
    w = 60
    h = 50
    qf u v = ((u - 30) ^ (2 :: Int) + 2 * (v - 21) ^ (2 :: Int)) / 1000
    qg = U.generate (w * h) (\k -> let (j, i) = k `divMod` w in qf (fromIntegral i) (fromIntegral j))
    fF u v = let (f, _, _) = bil qg w h u v in f
    gF u v = let (_, gu, gv) = bil qg w h u v in (gu, gv)
    fn = fn0 { fnF = fF, fnG = gF }
    -- 200 回：u, v を 2 個ずつ、格子点を 2 個（乱数は rngSeed(4) を順に）
    (eFD, eTrue, eNode, rsA) = foldl' one (0, 0, 0, rngList 4) [1 .. 200 :: Int]
    one (a, b, c, r1 : r2 : r3 : r4 : r5 : r6 : rs) _ =
      let u = fromIntegral (floor (r1 * fromIntegral (w - 2)) :: Int) + 0.1 + 0.8 * r2
          v = fromIntegral (floor (r3 * fromIntegral (h - 2)) :: Int) + 0.1 + 0.8 * r4
          (g0, g1) = gF u v
          (n0, n1) = fdGrad fF u v 1e-5
          i = floor (r5 * fromIntegral w) :: Int
          j = floor (r6 * fromIntegral h) :: Int
      in ( maximum [a, abs (g0 - n0), abs (g1 - n1)]
         , maximum [b, abs (g0 - 2 * (u - 30) / 1000), abs (g1 - 4 * (v - 21) / 1000)]
         , max c (abs (fF (fromIntegral i) (fromIntegral j) - qf (fromIntegral i) (fromIntegral j)))
         , rs )
    one acc _ = acc
    -- 20 個の初期点から勾配法
    (okGD, mono, rsB) = foldl' gd (True, True, rsA) [1 .. 20 :: Int]
    gd (ok, mo, r1 : r2 : rs) _ =
      let p0 = (2 + r1 * fromIntegral (w - 4), 2 + r2 * fromIntegral (h - 4))
          run :: Int -> V2 -> Double -> Bool -> (V2, Bool)
          run it p lst m
            | it >= 500 = (p, m)
            | vlen g < 1e-12 = (p, m)
            | t * vlen g < 1e-9 = (p, m)
            | otherwise = let p' = vaxpy (-t) g p
                              fv = uncurry fF p'
                          in run (it + 1) p' fv (m && fv <= lst + 1e-15)
            where
              g = uncurry gF p
              t = arT (armijo fn p (vscale (-1) g) 1e-4 0.5 400)
          (pe, m') = run 0 p0 (uncurry fF p0) mo
      in (ok && vlen (vsub pe (30, 21)) < 0.5, m', rs)
    gd acc _ = acc
    -- η = 10⁹、β = 0.99、ノイズ 50 で 300 回
    tg = U.generate (gw * gh) (\k -> testImage (k `mod` gw) (k `div` gw) 0)
    okClamp = fst (foldl' (\(ok, (p, rs)) _ ->
                     let (q, _, rs') = move tg 1e9 0.99 50 p rs
                     in (ok && finite (pU q + pV q) && pU q >= 0 && pU q <= fromIntegral (gw - 1) && pV q >= 0 && pV q <= fromIntegral (gh - 1), (q, rs')))
                   (True, (Part 10 10 0 0, rsB)) [1 .. 300 :: Int])

-- ---------- やってみよう ----------
-- 1. 顔を近づけたり手をかざしたりすると、暗い所（目・髪・影）へ粒子が流れこむ
-- 2. M で慣性を切ると（β = 0：ふつうの勾配法）、粒子はすぐ止まる。慣性ありだと谷を行き過ぎて戻る（04 の重い球）
-- 3. ↑ で η を大きくすると谷を飛び越えて落ち着かない。↓ で小さくすると細かい模様に沿う
-- 4. move の −eta を +eta にすると明るい所へ（勾配上昇法）。色を変えて「光に集まる虫」に
-- 5. N のノイズを消すと、粒子は一番近い谷で止まる（局所最小）。ノイズは別の谷へ移るきっかけ（13）

-- ==================================================
-- ここから下は「最適化の道具」（中身は読まなくても OK。消さないでね）
-- テスト関数（値・勾配・ヘッセ行列）、等高線、対数グラフ、2×2 行列、直線探索、自己テスト
-- 計算はすべて Double（倍精度）で行い、描くときだけ Float にする（1e-10 より小さい誤差まで測るため）
-- ==================================================

-- ---- 色（p5.js 版の PAL）
cBg, cInk, cDim, cLine, cOr, cCy, cGr, cYe, cPk, cVi, cRd :: Color
cBg = rgb255 12 36 69
cInk = rgb255 232 242 251
cDim = rgb255 142 178 207
cLine = rgb255 44 90 133
cOr = rgb255 255 122 47
cCy = rgb255 95 216 240
cGr = rgb255 141 224 122
cYe = rgb255 255 210 74
cPk = rgb255 255 111 168
cVi = rgb255 183 156 255
cRd = rgb255 255 90 90

-- ---- 2 次元のベクトルと 2×2 行列
type V2 = (Double, Double)
data M2 = M2 !Double !Double !Double !Double     -- [[a, b], [c, d]]

vadd, vsub :: V2 -> V2 -> V2
vadd (a, b) (c, d) = (a + c, b + d)
vsub (a, b) (c, d) = (a - c, b - d)

vscale :: Double -> V2 -> V2
vscale k (a, b) = (k * a, k * b)

vaxpy :: Double -> V2 -> V2 -> V2    -- a·x + y
vaxpy k (a, b) (c, d) = (c + k * a, d + k * b)

vdot :: V2 -> V2 -> Double
vdot (a, b) (c, d) = a * c + b * d

vlen :: V2 -> Double
vlen (a, b) = hypot a b

hypot :: Double -> Double -> Double
hypot a b = let m = max (abs a) (abs b) in if m == 0 || isInfinite m then m else m * sqrt ((a / m) ^ (2 :: Int) + (b / m) ^ (2 :: Int))

mvec :: M2 -> V2 -> V2
mvec (M2 a b c d) (x, y) = (a * x + b * y, c * x + d * y)

-- 描くために Float へ（NaN は 0、とても大きい値は ±1e5 に切りつめる）
d2f :: Double -> Float
d2f v = if isNaN v then 0 else realToFrac (max (-1e5) (min 1e5 v))

-- 発散したか（NaN・無限大・とても大きい）
bad :: Double -> Bool
bad v = isNaN v || isInfinite v || abs v > 1e12

finite :: Double -> Bool
finite v = not (isNaN v || isInfinite v)

-- 対称行列の固有値（小さい順 l1 ≤ l2）と固有ベクトル
data Eig = Eig { eL1, eL2 :: !Double, eV1, eV2 :: !V2 }

eig2 :: M2 -> Eig
eig2 (M2 a b _ d) = Eig l1 (t + r) v (negate (snd v), fst v)
  where
    t = (a + d) / 2
    r = hypot ((a - d) / 2) b
    l1 = t - r
    v0 | abs b > 1e-14 * (abs a + abs d + 1) = (l1 - d, b)
       | a <= d = (1, 0)
       | otherwise = (0, 1)
    v = vscale (1 / vlen v0) v0

solve2 :: M2 -> V2 -> V2
solve2 (M2 a b c d) (r0, r1) = let det = a * d - b * c in ((d * r0 - b * r1) / det, (a * r1 - c * r0) / det)

-- ---- テスト関数：値 f、勾配 g、ヘッセ行列 h、最小点 xs、最小値 fs、表示範囲 view（中心 x, 中心 y, 横の半幅）
data Fn = Fn
  { fnName  :: String
  , fnF     :: Double -> Double -> Double
  , fnG     :: Double -> Double -> V2
  , fnH     :: Double -> Double -> M2
  , fnXs    :: [V2]
  , fnFs    :: Double
  , fnView  :: (Double, Double, Double)
  , fnStart :: V2
  , fnMu    :: Double            -- 強凸性 μ（0 = わからない）
  , fnL     :: Double            -- 勾配のリプシッツ定数 L（0 = わからない）
  , fnConvex :: Bool
  , fnTh    :: Double            -- 2 次関数の回転角
  }

fn0 :: Fn
fn0 = Fn "" (\_ _ -> 0) (\_ _ -> (0, 0)) (\_ _ -> M2 0 0 0 0) [] 0 (0, 0, 1) (0, 0) 0 0 False 0

-- f = ½ xᵀAx、A の固有値は μ = 1 と L = κ（θ だけ回転）
quadFn :: Double -> Double -> Fn
quadFn kap th = fn0
  { fnName = "quadratic kappa = " ++ fmt 3 kap
  , fnF = \x y -> 0.5 * (a * x * x + 2 * b * x * y + d * y * y)
  , fnG = \x y -> (a * x + b * y, b * x + d * y)
  , fnH = \_ _ -> M2 a b b d
  , fnMu = 1, fnL = kap, fnXs = [(0, 0)], fnFs = 0, fnView = (0, 0, 3.2), fnStart = (-2.6, 1.6), fnConvex = True, fnTh = th }
  where
    c = cos th
    s = sin th
    a = c * c + kap * s * s
    b = (1 - kap) * c * s
    d = s * s + kap * c * c

rosen :: Fn
rosen = fn0
  { fnName = "Rosenbrock"
  , fnF = \x y -> (1 - x) ^ (2 :: Int) + 100 * (y - x * x) ^ (2 :: Int)
  , fnG = \x y -> (-2 * (1 - x) - 400 * x * (y - x * x), 200 * (y - x * x))
  , fnH = \x y -> M2 (2 - 400 * (y - x * x) + 800 * x * x) (-400 * x) (-400 * x) 200
  , fnXs = [(1, 1)], fnFs = 0, fnView = (-0.2, 0.9, 2.1), fnStart = (-1.2, 1) }

himmel :: Fn
himmel = h { fnXs = map (\p -> fromMaybe p (criticalPoint h p 60)) (fnXs h) }   -- 4 つの最小点を ∇f = 0 のニュートン法で精密に
  where
    h = fn0
      { fnName = "Himmelblau"
      , fnF = \x y -> (x * x + y - 11) ^ (2 :: Int) + (x + y * y - 7) ^ (2 :: Int)
      , fnG = \x y -> (4 * x * (x * x + y - 11) + 2 * (x + y * y - 7), 2 * (x * x + y - 11) + 4 * y * (x + y * y - 7))
      , fnH = \x y -> M2 (12 * x * x + 4 * y - 42) (4 * x + 4 * y) (4 * x + 4 * y) (4 * x + 12 * y * y - 26)
      , fnXs = [(3, 2), (-2.8051, 3.1313), (-3.7793, -3.2832), (3.5844, -1.8481)], fnFs = 0, fnView = (0, 0, 6.2), fnStart = (-0.3, -0.9) }

beale :: Fn
beale = fn0
  { fnName = "Beale"
  , fnF = \x y -> sum [r * r | (r, _, _, _, _) <- terms x y]
  , fnG = \x y -> foldl' (\(p, q) (r, rx, ry, _, _) -> (p + 2 * r * rx, q + 2 * r * ry)) (0, 0) (terms x y)
  , fnH = \x y -> let (a, b, d) = foldl' (\(a0, b0, d0) (r, rx, ry, rxy, ryy) -> (a0 + 2 * rx * rx, b0 + 2 * (rx * ry + r * rxy), d0 + 2 * (ry * ry + r * ryy))) (0, 0, 0) (terms x y)
                  in M2 a b b d
  , fnXs = [(3, 0.5)], fnFs = 0, fnView = (0.6, 0, 4.6), fnStart = (-1.5, 1.8) }
  where
    terms x y = [ (c - x + x * y ^ n, y ^ n - 1, fromIntegral n * x * y ^ (n - 1), fromIntegral n * y ^ (n - 1)
                  , if n > 1 then fromIntegral (n * (n - 1)) * x * y ^ (n - 2) else 0)
                | (c, n) <- zip [1.5, 2.25, 2.625] [1 :: Int ..] ]

-- log(e^(x+3y−0.1) + e^(x−3y−0.1) + e^(−x−0.1))：凸。最小点 (−ln2 / 2, 0)、最小値 (3/2)ln2 − 0.1
lse :: Fn
lse = fn0
  { fnName = "log-sum-exp"
  , fnF = \x y -> fst (pr x y)
  , fnG = \x y -> let (p0, p1, p2) = snd (pr x y) in (p0 + p1 - p2, 3 * p0 - 3 * p1)
  , fnH = \x y -> let (p0, p1, p2) = snd (pr x y)
                      g0 = p0 + p1 - p2
                      g1 = 3 * p0 - 3 * p1
                  in M2 (1 - g0 * g0) (g1 - g0 * g1) (g1 - g0 * g1) (9 * (p0 + p1) - g1 * g1)
  , fnXs = [(-log 2 / 2, 0)], fnFs = 1.5 * log 2 - 0.1, fnView = (-0.4, 0, 2.4), fnStart = (-2, 1.1), fnConvex = True, fnL = 10 }
  where
    pr x y =
      let z0 = x + 3 * y - 0.1
          z1 = x - 3 * y - 0.1
          z2 = -x - 0.1
          m = max z0 (max z1 z2)
          e0 = exp (z0 - m)
          e1 = exp (z1 - m)
          e2 = exp (z2 - m)
          s = e0 + e1 + e2
      in (m + log s, (e0 / s, e1 / s, e2 / s))

-- 最小点までの距離（最小点が複数なら近いほう）
errTo :: Fn -> V2 -> Double
errTo f p = foldl' min (1 / 0) [vlen (vsub p q) | q <- fnXs f]

-- ---- 微分のチェック（中心差分）。H は「解析的な勾配」の差分と比べる
fdGrad :: (Double -> Double -> Double) -> Double -> Double -> Double -> V2
fdGrad fn x y h = ((fn (x + h) y - fn (x - h) y) / (2 * h), (fn x (y + h) - fn x (y - h)) / (2 * h))

derivErr :: Fn -> [V2] -> (Double, Double)
derivErr f pts = foldl' one (0, 0) pts
  where
    h = 1e-6
    rel a b = abs (a - b) / max 1 (abs b)
    one (eg, eh) (x, y) =
      let (g0, g1) = fnG f x y
          (n0, n1) = fdGrad (fnF f) x y h
          M2 ha hb hc hd = fnH f x y
          (ax1, ay1) = fnG f (x + h) y
          (ax0, ay0) = fnG f (x - h) y
          (bx1, by1) = fnG f x (y + h)
          (bx0, by0) = fnG f x (y - h)
          nm = [(ax1 - ax0) / (2 * h), (bx1 - bx0) / (2 * h), (ay1 - ay0) / (2 * h), (by1 - by0) / (2 * h)]
      in (maximum [eg, rel g0 n0, rel g1 n1], maximum (eh : zipWith rel [ha, hb, hc, hd] nm))

-- ∇f = 0 をニュートン法で解く（停留点を探す）。見つからなければ Nothing
criticalPoint :: Fn -> V2 -> Int -> Maybe V2
criticalPoint f p0 it = go 0 p0
  where
    go k p
      | k >= it = done p
      | not (finite (fst d + snd d)) = Nothing
      | vlen d < 1e-14 = done p'
      | otherwise = go (k + 1) p'
      where
        d = solve2 (uncurry (fnH f) p) (uncurry (fnG f) p)
        p' = vsub p d
    done p = if vlen (uncurry (fnG f) p) < 1e-8 then Just p else Nothing

-- アルミホの条件 f(x + t d) ≤ f(x) + c₁ t ∇fᵀd になるまで t を β 倍（試した点も返す）
data Armijo = Armijo { arT :: !Double, arTries :: [(Double, Double)], arSlope :: !Double }

armijo :: Fn -> V2 -> V2 -> Double -> Double -> Double -> Armijo
armijo f p d c1 beta t0 = go (0 :: Int) t0 []
  where
    f0 = uncurry (fnF f) p
    sl = vdot (uncurry (fnG f) p) d
    go k t acc
      | k >= 60 = Armijo t (reverse acc) sl
      | fq <= f0 + c1 * t * sl = Armijo t (reverse acc') sl
      | otherwise = go (k + 1) (t * beta) acc'
      where
        fq = uncurry (fnF f) (vaxpy t d p)
        acc' = (t, fq) : acc

armijo0 :: Fn -> V2 -> V2 -> Armijo
armijo0 f p d = armijo f p d 1e-4 0.5 1

-- ---- 画面と世界の座標（縦横の縮尺は同じ）。画面は p5.js と同じ（左上が原点、y は下向き）
data View = View { vPx, vPy, vPw, vPh, vCx, vCy, vHw, vSc, vHh :: !Double }

makeView :: Double -> Double -> Double -> Double -> (Double, Double, Double) -> View
makeView px py pw ph (cx, cy, hw) = View px py pw' ph' cx cy hw sc (ph' / (2 * sc))
  where
    pw' = fromIntegral (floor (pw + 0.5) :: Int)
    ph' = fromIntegral (floor (ph + 0.5) :: Int)
    sc = pw' / (2 * hw)

toSD :: View -> V2 -> V2
toSD v (x, y) = (vPx v + vPw v / 2 + (x - vCx v) * vSc v, vPy v + vPh v / 2 - (y - vCy v) * vSc v)

toS :: View -> V2 -> Point
toS v p = let (a, b) = toSD v p in (d2f a, d2f b)

toWD :: View -> V2 -> V2
toWD v (sx, sy) = (vCx v + (sx - vPx v - vPw v / 2) / vSc v, vCy v - (sy - vPy v - vPh v / 2) / vSc v)

toW :: View -> Point -> V2
toW v (sx, sy) = toWD v (realToFrac sx, realToFrac sy)

inside :: View -> Point -> Bool
inside v (sx, sy) = let x = realToFrac sx; y = realToFrac sy in x >= vPx v && x <= vPx v + vPw v && y >= vPy v && y <= vPy v + vPh v

-- 等高線の画像：格子で値を計算 → 分位点で段を決め、色ぬり ＋ マーチングスクエアで線（関数を変えたときだけ呼ぶ）
--   返すのは（絵、段の値）。絵は view の中（p5.js の座標）でそのまま使う
contourImage :: View -> (Double -> Double -> Double) -> Int -> (Picture, [Double])
contourImage v fz nLev = contourImageT v fz nLev (\_ _ -> Nothing)

-- tint x y = Just (r, g, b, 透明度 0〜1) なら、その場所の色に重ねる
contourImageT :: View -> (Double -> Double -> Double) -> Int -> (Double -> Double -> Maybe (Double, Double, Double, Double)) -> (Picture, [Double])
contourImageT v fz nLev tint = (Pictures ([image, Color (rgba255 120 200 240 31) (Pictures gridLines)] ++ map contourOf levIx), levs)
  where
    cs = 5 :: Double
    nx = ceiling (vPw v / cs) + 1 :: Int
    ny = ceiling (vPh v / cs) + 1 :: Int
    zs = U.generate (nx * ny) $ \q ->
      let (j, i) = q `divMod` nx
          (x, y) = toWD v (vPx v + fromIntegral i * cs, vPy v + fromIntegral j * cs)
          z = fz x y
      in if finite z then z else 1e300
    zAt i j = zs U.! (j * nx + i)
    srt = U.fromList (sort (filter (< 1e300) (U.toList zs)))
    ns = U.length srt
    levs = if ns == 0 then [] else
      [ srt U.! min (ns - 1) (floor (fromIntegral ns * (fromIntegral k / fromIntegral (nLev + 1)) ** (1.7 :: Double))) | k <- [1 .. nLev] ]
    bands = U.map (\z -> if z >= 1e300 then 255 else length (takeWhile (< z) levs)) zs :: U.Vector Int
    -- 色ぬり（8 万画素以下になるように拡大率 k を選ぶ）
    k = getAt 5 [kk | kk <- [1, 2, 2.5, 4, 5], fromIntegral (iw kk * ih kk) <= (80000 :: Double)] 0
    iw kk = max 1 (floor (vPw v / kk)) :: Int
    ih kk = max 1 (floor (vPh v / kk)) :: Int
    w' = iw k
    h' = ih k
    pix x y =
      let sx = (fromIntegral x + 0.5) * k
          sy = (fromIntegral y + 0.5) * k
          i = min (nx - 1) (floor (sx / cs + 0.5))
          j = min (ny - 1) (floor (sy / cs + 0.5))
          b = bands U.! (j * nx + i)
          t = if b == 255 then 1.25 else fromIntegral b / fromIntegral nLev
          base = (14 + 34 * (1 - t), 40 + 62 * (1 - t), 72 + 70 * (1 - t))
          (cr, cg, cb) = case uncurry tint (toWD v (vPx v + sx, vPy v + sy)) of
            Nothing -> base
            Just (r, g, bb, a) -> let (r0, g0, b0) = base in (r0 * (1 - a) + r * a, g0 * (1 - a) + g * a, b0 * (1 - a) + bb * a)
          q c = fromIntegral (max 0 (min 255 (floor c :: Int)))
      in (q cr, q cg, q cb)
    image = Translate (realToFrac (vPx v + fromIntegral w' * k / 2)) (realToFrac (vPy v + fromIntegral h' * k / 2))
              (Scale 1 (-1) (pixelImage w' h' (realToFrac k) pix))
    -- 方眼（製図用紙）
    gs0 = 10 ** fromIntegral (floor (logBase 10 (vHw v / 2) + 1e-12) :: Int)
    gs1 = if vHw v / gs0 > 10 then gs0 * 2 else gs0
    gs = if vHw v / gs1 > 10 then gs1 * 2.5 else gs1
    gridLines =
      [ Line [(sx, realToFrac (vPy v)), (sx, realToFrac (vPy v + vPh v))]
      | x <- takeWhile (< vCx v + vHw v) [fromIntegral (ceiling ((vCx v - vHw v) / gs) :: Int) * gs + fromIntegral n * gs | n <- [0 :: Int ..]]
      , let sx = fst (toS v (x, 0)) ]
      ++
      [ Line [(realToFrac (vPx v), sy), (realToFrac (vPx v + vPw v), sy)]
      | y <- takeWhile (< vCy v + vHh v) [fromIntegral (ceiling ((vCy v - vHh v) / gs) :: Int) * gs + fromIntegral n * gs | n <- [0 :: Int ..]]
      , let sy = snd (toS v (0, y)) ]
    -- マーチングスクエア：ます目の 4 隅の最小〜最大にかかる段だけ調べ、線分を辺の番号でつないで長い折れ線にする
    hid i j = 2 * (j * nx + i)
    vid i j = 2 * (j * nx + i) + 1
    levIx = zip [0 :: Int ..] levs
    segMap = IM.fromListWith (++) (concat [ cellSegs i j | j <- [0 .. ny - 2], i <- [0 .. nx - 2] ])
    cellSegs i j =
      let a = zAt i j
          b = zAt (i + 1) j
          c = zAt (i + 1) (j + 1)
          d = zAt i (j + 1)
          lo = min (min a b) (min c d)
          hi = max (max a b) (max c d)
          fi = fromIntegral i :: Double
          fj = fromIntegral j :: Double
          one lv =
            let up z = z > lv
                ps = [ (hid i j, (fi + (lv - a) / (b - a), fj)) | up a /= up b ]
                  ++ [ (vid (i + 1) j, (fi + 1, fj + (lv - b) / (c - b))) | up b /= up c ]
                  ++ [ (hid i (j + 1), (fi + (lv - d) / (c - d), fj + 1)) | up d /= up c ]
                  ++ [ (vid i j, (fi, fj + (lv - a) / (d - a))) | up a /= up d ]
            in pairUp ps
      in [ (li, ss) | (li, lv) <- takeWhile ((< hi) . snd) (dropWhile ((< lo) . snd) levIx), let ss = one lv, not (null ss) ]
    pairUp (p : q : rest) = (p, q) : pairUp rest
    pairUp _ = []
    scr (gx, gy) = (d2f (vPx v + gx * cs), d2f (vPy v + gy * cs)) :: Point
    contourOf (li, _) = Color (rgba255 110 215 250 140) (Pictures [Line (map scr c) | c <- chains (IM.findWithDefault [] li segMap)])

-- 線分（端の番号つき）をつないで折れ線のリストにする
chains :: [((Int, V2), (Int, V2))] -> [[V2]]
chains ss = go starts IS.empty
  where
    adj = IM.fromListWith (++) (concat [[(ka, [kb]), (kb, [ka])] | ((ka, _), (kb, _)) <- ss])
    pos = IM.fromList (concat [[(ka, pa), (kb, pb)] | ((ka, pa), (kb, pb)) <- ss])
    starts = [k | (k, ns) <- IM.toList adj, length ns == 1] ++ IM.keys adj
    nbrs k = IM.findWithDefault [] k adj
    go [] _ = []
    go (s : rest) seen
      | IS.member s seen = go rest seen
      | otherwise = let (path, seen') = walk s s (IS.insert s seen) [s]
                    in map (\k -> IM.findWithDefault (0, 0) k pos) path : go rest seen'
    walk s cur seen acc = case [n | n <- nbrs cur, not (IS.member n seen)] of
      (n : _) -> walk s n (IS.insert n seen) (n : acc)
      [] -> (if length acc > 2 && s `elem` nbrs cur then s : acc else acc, seen)

-- ---- 対数グラフ：0 以下・NaN は描かない
data Series = Series { sV :: [Double], sC :: Color, sName :: String, sDash :: Bool, sWt :: Float }

series :: [Double] -> Color -> String -> Series
series vs c nm = Series vs c nm False 2

data PlotOpt = PlotOpt { poLo, poHi :: Maybe Double, poLogx :: Bool, poTitle :: String, poKmax :: Maybe Int, poXlabel :: String }

plotOpt :: PlotOpt
plotOpt = PlotOpt Nothing Nothing False "" Nothing "iteration k"

logPlot :: Float -> Float -> Float -> Float -> [Series] -> PlotOpt -> Picture
logPlot x y w h ss opt = Pictures $
  [ Color (rgba255 8 26 50 230) (rectTL x y w h), Color cLine (rectLineTL x y w h) ]
  ++ concat [ [ Color (rgb255 40 80 120) (Line [(x, yy), (x + w, yy)]), txtR (x - 4) (yy + 5) 14 cDim ("1e" ++ show e) ]
            | e <- takeWhile (<= hi) [lo, lo + de ..], let yy = yOf (10 ^^ e) ]
  ++ [ txtC (xOf k) (y + h + 12) 14 cDim (show k) | k <- kt ]
  ++ [ txtC (x + w / 2) (y + h + 30) 14 cDim (poXlabel opt) ]
  ++ map curve ss
  ++ concat [ [ txtR (x + w - 34) (yy + 12) 14 (sC s) (sName s)
              , Color (sC s) (if sDash s then dashLine 2 4 3 [(x + w - 28, yy + 7), (x + w - 8, yy + 7)] else fatLine 2 [(x + w - 28, yy + 7), (x + w - 8, yy + 7)]) ]
            | (i, s) <- zip [0 :: Int ..] (filter (not . null . sName) ss), let yy = y + 6 + 17 * fromIntegral i ]
  ++ [ txt x (y - 6) 14 cInk (poTitle opt) | not (null (poTitle opt)) ]
  where
    good v = v > 0 && finite v
    allV = [logBase 10 v | s <- ss, v <- sV s, good v]
    mn = foldl' min (1 / 0) allV
    mx = foldl' max (-1 / 0) allV
    kmax = case poKmax opt of
      Just km | km > 0 -> km
      _ -> maximum (1 : [length (sV s) - 1 | s <- ss])
    lo = case poLo opt of
      Just l -> floor l
      Nothing -> if null allV then -16 else max (-17) (floor mn) :: Int
    hi0 = case poHi opt of
      Just hv -> ceiling hv
      Nothing -> if null allV then 2 else ceiling mx :: Int
    hi = if hi0 <= lo then lo + 1 else hi0
    de = max 1 (ceiling (fromIntegral (hi - lo) / 8 :: Double)) :: Int
    xOf :: Int -> Float
    xOf k = x + w * (if poLogx opt then realToFrac (log (1 + fromIntegral k) / log (1 + fromIntegral kmax) :: Double) else fromIntegral k / fromIntegral kmax)
    yOf :: Double -> Float
    yOf v = y + h - h * d2f ((logBase 10 v - fromIntegral lo) / fromIntegral (hi - lo))
    kt = if poLogx opt then filter (<= kmax) [1, 10, 100, 1000, 10000] else [0, (kmax + 1) `div` 2, kmax]
    curve s = Color (sC s) (Pictures [ case run of
                                         [p] -> at p (disc (sWt s / 2 + 0.5))     -- 1 点だけなら点を打つ
                                         _ -> if sDash s then dashLine (sWt s) 6 5 run else fatLine (sWt s) run
                                     | run <- runs s ])
    runs s = filter (not . null) (splitRuns [ if good v then Just (xOf k, max y (min (y + h) (yOf v))) else Nothing | (k, v) <- zip [0 .. kmax] (sV s) ])
    splitRuns ps = case break (== Nothing) ps of
      (a, []) -> [[p | Just p <- a]]
      (a, _ : rest) -> [p | Just p <- a] : splitRuns rest

-- ---- 描く道具
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- gloss の座標 → p5.js の座標（左上が原点、y は下向き）
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

p5Mouse :: Env -> Point
p5Mouse env = toP5 env (envMouse env)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中心が (x, y) の文字
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y size c s = txt (x - fromIntegral (length s) * size * 0.36) (y + size / 2) size c s

-- 右端が x、下が y の文字
txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtR x y size c s = txt (x - fromIntegral (length s) * size * 0.72) y size c s

-- 左上が (x, y) の長方形（p5.js の rect）：塗り・枠
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

rectLineTL :: Float -> Float -> Float -> Float -> Picture
rectLineTL x y w h = rectLine (x + w / 2) (y + h / 2) w h

-- 太さ w の折れ線（細い線を少しずつずらして重ねる。点が多くても描く要素が少なくてすむ）
fatLine :: Float -> [Point] -> Picture
fatLine w ps
  | w <= 1.5 = Line ps
  | otherwise = Pictures [Translate dx dy (Line ps) | (dx, dy) <- (0, 0) : concat [[(o, 0), (-o, 0), (0, o), (0, -o)] | o <- offs]]
  where offs = takeWhile (<= (w - 1) / 2 + 1e-3) [0.5, 1 ..]

-- 点線（on 描いて off あける）
dashLine :: Float -> Float -> Float -> [Point] -> Picture
dashLine w on off ps = Pictures (map (fatLine w) (go True on (zip ps (drop 1 ps)) []))
  where
    go _ _ [] cur = [reverse cur | length cur >= 2]
    go drawing left ((a, b) : rest) cur
      | l < 1e-6 = go drawing left rest cur
      | left >= l = go drawing (left - l) rest (if drawing then b : start a cur else [])
      | otherwise =
          let m = (fst a + (fst b - fst a) * left / l, snd a + (snd b - snd a) * left / l)
          in if drawing then reverse (m : start a cur) : go False off ((m, b) : rest) []
             else go True on ((m, b) : rest) [m]
      where l = dist2 a b
    start a cur = if null cur then [a] else cur

-- 矢印（色 c、太さ w）
arrowP :: Point -> Point -> Color -> Float -> Picture
arrowP (x1, y1) (x2, y2) c w = Color c (Pictures ([thickLine w (x1, y1) (x2, y2)] ++ [Polygon [(x2, y2), hd (-0.4), hd 0.4] | l > 2]))
  where
    a = atan2 (y2 - y1) (x2 - x1)
    l = sqrt ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int))
    hl = min 12 (l * 0.4)
    hd d = (x2 - hl * cos (a + d), y2 - hl * sin (a + d))

-- 白いふちの丸（半径 r）
dot :: Point -> Float -> Color -> Picture
dot p r c = at p (Pictures [Color white (disc r), Color c (disc (r - 1.2))])

-- 反復点の列を折れ線と点で
drawPath :: View -> [V2] -> Color -> Float -> Bool -> Picture
drawPath _ [] _ _ _ = Blank
drawPath v ps c w dots = Pictures ([Color c (fatLine w ss) | length ss >= 2] ++ ds ++ [dot (last ss) 5.5 c])
  where
    ss = map (toS v) ps
    stepN = max 1 (length ss `div` 60)
    ds = if dots then [Color c (at s (disc 2.5)) | (i, s) <- zip [0 :: Int ..] ss, i `mod` stepN == 0] else []

-- 文字の列（左上が (x, y)、大きさ sz、行の間隔 lh）
hud :: [(String, Color)] -> Float -> Float -> Float -> Float -> Picture
hud ls x y sz lh = Pictures [txt x (y + fromIntegral i * sz * lh + sz) sz c s | (i, (s, c)) <- zip [0 :: Int ..] ls, not (null s)]

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- 数の表示（発散・とても小さい数も）：有効数字 d 桁
fmt :: Int -> Double -> String
fmt d v
  | bad v = "diverged"
  | v == 0 = "0"
  | a < 1e-3 || a >= 1e5 = showFFloat (Just (d - 1)) (v / 10 ^^ e) "" ++ "e" ++ show e
  | otherwise =
      let r = fromIntegral (round (v / 10 ^^ (e - d + 1)) :: Integer) * 10 ^^ (e - d + 1) :: Double
      in if e - d + 1 >= 0 then show (round r :: Integer) else stripZ (showFFloat (Just (d - 1 - e)) r "")
  where
    a = abs v
    e = log10i a
    stripZ s = if '.' `elem` s then let t = reverse (dropWhile (== '0') (reverse s)) in if last t == '.' then init t else t else s

-- floor(log10 a)（10 のべきでも正確に）
log10i :: Double -> Int
log10i a = let e0 = floor (logBase 10 a) :: Int in if 10 ^^ (e0 + 1) <= a then e0 + 1 else if 10 ^^ e0 > a then e0 - 1 else e0

-- 決まった種から同じ乱数の列（p5.js 版と同じ mulberry32）
rngList :: Int -> [Double]
rngList seed = go (fromIntegral seed :: Word32)
  where
    go s0 =
      let s = s0 + 0x6d2b79f5
          t1 = (s `xor` (s `shiftR` 15)) * (1 .|. s)
          t2 = (t1 + ((t1 `xor` (t1 `shiftR` 7)) * (61 .|. t1))) `xor` t1
          r = t2 `xor` (t2 `shiftR` 14)
      in fromIntegral r / 4294967296 : go s

-- ---- 自己テスト（起動時に、知られている答えと照合する）：（項目, 合格か, 実測値など）
type Test = (String, Bool, String)

stOk :: String -> Bool -> String -> Test
stOk name ok info = (name, ok, info)

-- 結果を端末に出す（テストモードでは失敗した項目とまとめの 1 行だけ）
printSelfTest :: [Test] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else [r | r@(_, ok, _) <- rs, not ok]
  mapM_ (\(nm, ok, info) -> putStrLn ((if ok then "OK  " else "NG  ") ++ nm ++ (if null info then "" else "  [" ++ info ++ "]"))) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [Test] -> String
selfTestLine rs = "Self-test " ++ (if allOk then "OK " else "NG ") ++ show (length [() | (_, True, _) <- rs]) ++ " / " ++ show (length rs)
  where allOk = and [ok | (_, ok, _) <- rs]

drawSelfTest :: Env -> [Test] -> Picture
drawSelfTest env rs = txt 16 (envH env - 12) 14 (if and [ok | (_, ok, _) <- rs] then cGr else cRd) (selfTestLine rs)
