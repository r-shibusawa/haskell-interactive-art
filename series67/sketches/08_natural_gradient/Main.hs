-- ============================================
-- 67-08 自然勾配 ─ 正規分布 N(μ, σ) をデータに合わせる（平均の負の対数尤度 L を小さくする）学習
-- ふつうの勾配降下は「どの座標で書くか」で道が変わる：(μ, σ) で下るのと (μ, log σ) で下るのとでは別の道
-- 自然勾配は勾配にフィッシャー行列の逆を掛ける Δ = −η F⁻¹ ∇L（2×2 の連立方程式を解く）→ 座標を変えてもほとんど同じ道
-- 分布の空間そのものの「最急降下」だから。左 = (μ, σ) 平面、右 = (μ, log σ) 平面に同じ 4 本の道（最適化の工夫はシリーズ 49）
-- 操作：クリック 出発点 / スペース 学習を止める・動かす / R 最初から / D 新しいデータ / ↑↓ 学習率
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

nData :: Int
nData = 200                                   -- ★ データの数

trueMu, trueSg :: Double
trueMu = 1.5                                  -- ★ データを作る正規分布
trueSg = 0.6

-- 4 本の道：(名前, 自然勾配か, (μ, log σ) の座標か = 点線)
kinds :: [(String, Bool, Bool)]
kinds = [ ("plain, (mu, sigma)", False, False), ("plain, (mu, log sigma)", False, True)
        , ("natural, (mu, sigma)", True, False), ("natural, (mu, log sigma)", True, True) ]

type P2 = (Double, Double)                    -- (μ, σ)

data World = World
  { wLrPlain :: !Double
  , wLrNat   :: !Double
  , wMData   :: !Double                       -- データの平均
  , wVData   :: !Double                       -- データの分散
  , wStart   :: !P2
  , wPaths   :: [[P2]]                        -- 4 本の道（古い順）
  , wRunning :: !Bool
  , wSeedD   :: !Int
  , wFields  :: Maybe (Field, Field)          -- L の等高線（データと画面の幅が変わったら作り直す）
  , wKey     :: String
  , wTest    :: [Check] }

main :: IO ()
main = runSketch (sketch "67-08 Natural gradient" world0)
  { skBack  = col bgC
  , skStart = \env -> do
      printSelfTest selfTest
      pure (step env world0 { wTest = selfTest })
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = resetPaths (makeData (World 0.1 0.08 0 1 (-2, 2.2) [] True 5 Nothing "" []))

boxA, boxB :: Env -> PBox
boxA env = let w = min (d2 (envW env) * 0.4) (d2 (envH env) * 0.62) in plotBox 70 110 w w (-3) 3 0.05 3
boxB env = let w = min (d2 (envW env) * 0.4) (d2 (envH env) * 0.62) in plotBox (d2 (envW env) * 0.52) 110 w w (-3) 3 (log 0.05) (log 3)

-- 種 seedD で正規分布のデータを作り、平均と分散だけ覚える
makeData :: World -> World
makeData w =
  let xs = withSeed (wSeedD w) (mapM (\_ -> fmap (\g -> trueMu + trueSg * g) gaussRnd) [1 .. nData])
      m = sumOf xs / i2d nData
      v = foldl' (\s x -> s + (x - m) ^ (2 :: Int)) 0 xs / i2d nData
  in w { wMData = m, wVData = v }

-- ★ 平均の負の対数尤度（定数を除く）
lossOf :: World -> Double -> Double -> Double
lossOf w mu sg = log sg + ((mu - wMData w) ^ (2 :: Int) + wVData w) / (2 * sg * sg)

-- (μ, σ) での勾配
gradA :: World -> Double -> Double -> (Double, Double)
gradA w mu sg = ((mu - wMData w) / (sg * sg), 1 / sg - ((mu - wMData w) ^ (2 :: Int) + wVData w) / (sg * sg * sg))

-- ★ 1 歩：kind 0,1 はふつう、2,3 は自然勾配。偶数は (μ, σ)、奇数は (μ, ℓ = log σ) の座標で計算する
stepOf :: World -> Int -> P2 -> P2
stepOf w kind (mu, sg) =
  let (g0, g1) = gradA w mu sg
      lp = wLrPlain w
      ln = wLrNat w
  in case kind of
       0 -> (mu - lp * g0, sg - lp * g1)
       1 -> let gl = sg * g1 in (mu - lp * g0, sg * exp (-lp * gl))     -- ∂L/∂ℓ = σ ∂L/∂σ
       2 -> let d = solveLin [[1 / (sg * sg), 0], [0, 2 / (sg * sg)]] [g0, g1]   -- F = diag(1/σ², 2/σ²)
            in (mu - ln * at' d 0 0, sg - ln * at' d 1 0)
       _ -> let d = solveLin [[1 / (sg * sg), 0], [0, 2]] [g0, sg * g1]         -- F = diag(1/σ², 2)
            in (mu - ln * at' d 0 0, sg * exp (-ln * at' d 1 0))

safeStep :: World -> Int -> P2 -> P2
safeStep w kind p = let (a, b) = stepOf w kind p in (clampv a (-6) 6, clampv b 0.05 6)

resetPaths :: World -> World
resetPaths w = w { wPaths = map (const [wStart w]) kinds }

-- 4 本とも 1 歩ずつ進める
advance :: World -> World
advance w = w { wPaths = [ forceP (ps ++ [safeStep w k (lastOr (wStart w) ps)]) | (k, ps) <- zip [0 ..] (wPaths w) ] }
  where forceP ps = foldl' (\() (a, b) -> a `seq` b `seq` ()) () ps `seq` ps

step :: Env -> World -> World
step env w0 =
  let key = show (wMData w0) ++ "/" ++ show (envW env)
      l0 = lossOf w0 (wMData w0) (sqrt (wVData w0))
      opt = fieldOpt { foLevel = 0.15, foVmax = 2.5, foCap = 2.3, foC0 = (52, 74, 112), foC1 = (28, 38, 60), foRes = 3 }
      w1 = if key == wKey w0 then w0 else
             w0 { wKey = key
                , wFields = Just ( gridField (boxA env) (\u v -> log1p (max 0 (lossOf w0 u v - l0))) opt
                                 , gridField (boxB env) (\u v -> log1p (max 0 (lossOf w0 u (exp v) - l0))) opt ) }
  in if wRunning w1 && length (headOr [] (wPaths w1)) < 600 then advance w1 else w1

-- 2 本の道の、同じ歩数どうしのずれの最大
gapOf :: [[P2]] -> Int -> Int -> Double
gapOf ps a b = foldl' max 0 (zipWith (\(x0, y0) (x1, y1) -> sqrt ((x0 - x1) ^ (2 :: Int) + (y0 - y1) ^ (2 :: Int))) (at' ps a []) (at' ps b []))

draw :: Env -> World -> Picture
draw env w = view env $
  [ maybe Blank (drawField . Just . fst) (wFields w), maybe Blank (drawField . Just . snd) (wFields w)
  , hudTitle "Natural gradient: the same path in any coordinates"
             ("Click start point   Space " ++ (if wRunning w then "learning" else "paused") ++ "   R restart   D new data   Up/Down learning rate x" ++ fmtN 2 (wLrPlain w / 0.1))
  , drawAxes ba "mu" "sigma  ((mu, sigma) plane, contours of L)"
  , drawAxes bb "mu" "log sigma  ((mu, log sigma) plane)" ]
  ++ concat [ [ pathPic ba ps c dsh, pathPic bb [ (a, log s) | (a, s) <- ps ] c dsh ]
            | ((_, nat, dsh), ps) <- zip kinds (wPaths w), let c = if nat then violet else orange ]
  ++ [ Color (col lime) (ringAt x y 16 2) | (b, (u, v)) <- [(ba, (wMData w, sqrt (wVData w))), (bb, (wMData w, log (sqrt (wVData w))))], let (x, y) = toScrF b u v ]
  ++ [ Color (col ink) (circleAt x y 10) | (b, (u, v)) <- [(ba, wStart w), (bb, (fst (wStart w), log (snd (wStart w))))], let (x, y) = toScrF b u v ]
  -- ---- 下：凡例と数値
  ++ [ say ((if dsh then "- - " else "--- ") ++ nm ++ ": (" ++ fmtN 3 a ++ ", " ++ fmtN 3 s ++ ") L=" ++ fmtN 4 (lossOf w a s)) 70 (y0 + 22 * i2f k) 15 (col (if nat then violet else orange))
     | (k, ((nm, nat, dsh), ps)) <- zip [0 :: Int ..] (zip kinds (wPaths w)), let (a, s) = lastOr (wStart w) ps ]
  ++ [ say "Largest gap between the two coordinates:" rx y0 15 (col ink)
     , say ("  plain " ++ fmtN 3 (gapOf (wPaths w) 0 1) ++ "   natural " ++ fmtN 4 (gapOf (wPaths w) 2 3)) rx (y0 + 20) 15 (col ink)
     , say ("Answer (green ring) = data mean " ++ fmtN 3 (wMData w) ++ ", sd " ++ fmtN 3 (sqrt (wVData w)) ++ " (MLE)") rx (y0 + 44) 14 (col dimc)
     , say "Fisher matrix: (mu, sigma): diag(1/s^2, 2/s^2)" rx (y0 + 64) 14 (col dimc)
     , say "               (mu, log sigma): diag(1/s^2, 2)" rx (y0 + 82) 14 (col dimc)
     , say "Plain gradient gets huge where sigma is small:" rx (y0 + 102) 14 (col dimc)
     , say "detours and overshoots" rx (y0 + 120) 14 (col dimc)
     , drawSelfTest env (wTest w) ]
  where
    ba = boxA env
    bb = boxB env
    y0 = f2 (pbY ba + pbH ba) + 50
    rx = envW env * 0.52

-- 道を描く（点線のときは、つながった折れ線の上に 7 と 5 の点線）
pathPic :: PBox -> [P2] -> RGB -> Bool -> Picture
pathPic b ps c dsh
  | not dsh = plotLine b ps (col c) 2.5
  | otherwise = Color (col c) (Pictures [ dashPolyline 2.5 7 5 (map (\(x, y) -> (f2 x, f2 y)) run) | run <- clipRuns b ps ])

-- 折れ線に沿って、線とすき間をくり返す（setLineDash と同じく、角をまたいでも模様が続く）
dashPolyline :: Float -> Float -> Float -> [(Float, Float)] -> Picture
dashPolyline w on off pts = Pictures (go 0 pts [])
  where
    per = on + off
    go _ [] acc = acc
    go _ [_] acc = acc
    go ph ((x1, y1) : rest@((x2, y2) : _)) acc =
      let l = hyp (x2 - x1) (y2 - y1)
          ux = if l == 0 then 0 else (x2 - x1) / l
          uy = if l == 0 then 0 else (y2 - y1) / l
          pt s = (x1 + ux * s, y1 + uy * s)
          -- この線分の中で「線」の部分：模様の位置 ph から始める
          segs = [ (max 0 a, min l (a + on)) | k <- [0 .. ceiling ((l + ph) / per) :: Int], let a = i2f k * per - ph, a + on > 0, a < l ]
          pics = [ lineW w (pt a) (pt b) | (a, b) <- segs, b > a ]
          ph' = let v = ph + l in v - per * i2f (floor (v / per) :: Int)
      in go ph' rest (pics ++ acc)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    foldl' (\v (b, isLog) -> if inRect mx my (pbBox b)
                               then let (u, s) = fromScr b mx my
                                    in resetPaths v { wStart = (clampv u (-3) 3, clampv (if isLog then exp s else s) 0.08 3), wRunning = True }
                               else v) w [(boxA env, False), (boxB env, True)]
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wRunning = not (wRunning w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> resetPaths w { wLrPlain = min 0.4 (wLrPlain w * 1.5), wLrNat = min 0.5 (wLrNat w * 1.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> resetPaths w { wLrPlain = max 0.01 (wLrPlain w / 1.5), wLrNat = max 0.01 (wLrNat w / 1.5) }
  EventKey (Char c) Down _ _
    | c == 'r' || c == 'R' -> resetPaths w
    | c == 'd' || c == 'D' -> resetPaths (makeData w { wSeedD = wSeedD w + 1 })
  _ -> w
  where (mx, my) = mouseD env

-- ---------- やってみよう ----------
-- 1. σ の小さい所（下のほう）から出発すると、ふつうの勾配（オレンジ）はどうなる？ 自然勾配（紫）は？
-- 2. ↑ で学習率を上げると、どちらが先にこわれる？
-- 3. 3 つ目の座標 (μ, 1/σ²) でもふつうの勾配と自然勾配を足してみよう。自然勾配の道はやっぱり同じ？
-- 4. 自然勾配の 1 歩の大きさを KL で測ると、どこでもだいたい同じ（½ η² ∇Lᵀ F⁻¹ ∇L）。確かめよう
-- 5. 深層学習の Adam（シリーズ 49）は「対角だけのフィッシャー行列」で割る自然勾配の親戚と見ることもできる

-- ---- 自己テスト
selfTest :: [Check]
selfTest =
  [ ("勾配の式 = 数値微分（誤差 1e-7）", abs (g0 - fd0) < 1e-7 && abs (g1 - fd1) < 1e-7)
  , ("スコアの分散（6 万回）≈ フィッシャー行列 diag(1/σ², 2/σ²)（3% 以内、非対角 ≈ 0）"
    , abs (c0 / n * sg * sg - 1) < 0.03 && abs (c1 / n * sg * sg / 2 - 1) < 0.03 && abs (c2 / n) < 0.03)
  , ("2×2 の連立方程式 F x = ∇L の解の残差 < 1e-14", abs (at' rr 0 0 - 0.3) < 1e-14 && abs (at' rr 1 0 + 0.7) < 1e-14)
  , ("自然勾配の 2 つの座標の道はほぼ同じ（ずれ " ++ fmtN 4 (gapOf ps 2 3) ++ " < 0.05）", gapOf ps 2 3 < 0.05)
  , ("ふつうの勾配の 2 つの道は大きくちがう（ずれ " ++ fmtN 3 (gapOf ps 0 1) ++ " > 0.3、自然勾配の 6 倍以上）", gapOf ps 0 1 > 0.3 && gapOf ps 0 1 > 6 * gapOf ps 2 3)
  , ("自然勾配は 400 歩で最尤推定 (平均, 標準偏差) に着く（誤差 1e-4）"
    , and [ abs (a - wMData w) < 1e-4 && abs (s - sqrt (wVData w)) < 1e-4 | k <- [2, 3], let (a, s) = at' (at' ps k []) 400 (0 / 0, 0 / 0) ]) ]
  where
    w = world0
    h = 1e-6
    (pm, ps') = (0.4, 0.9)
    (g0, g1) = gradA w pm ps'
    fd0 = (lossOf w (pm + h) ps' - lossOf w (pm - h) ps') / (2 * h)
    fd1 = (lossOf w pm (ps' + h) - lossOf w pm (ps' - h)) / (2 * h)
    mu = 0.5
    sg = 1.3
    n = 60000 :: Double
    (c0, c1, c2) = withSeed 808 (loopR 60000 (\(!s0, !s1, !s2) -> do
                     g <- gaussRnd
                     let x = mu + sg * g
                         a = (x - mu) / (sg * sg)
                         b = -1 / sg + (x - mu) ^ (2 :: Int) / sg ^ (3 :: Int)
                     pure (s0 + a * a, s1 + b * b, s2 + a * b)) (0, 0, 0))
    am = [[1 / (sg * sg), 0.2], [0.2, 2 / (sg * sg)]]
    rr = matVec am (solveLin am [0.3, -0.7])
    ps = wPaths (iterate advance (resetPaths w { wStart = (-2, 2.2) }) !! 400)

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

-- ---- 小さな連立一次方程式 A x = b（ガウスの消去法、部分ピボット）
solveLin :: [[Double]] -> [Double] -> [Double]
solveLin a b = backSub (elim 0 (zipWith (\r v -> r ++ [v]) a b))
  where
    n = length b
    safeD d = if abs d < 1e-300 then 1e-300 else d
    elim c m
      | c >= n = m
      | otherwise =
          let col' = [ abs (at' (at' m r []) c 0) | r <- [0 .. n - 1] ]
              piv = foldl' (\pv r -> if at' col' r 0 > at' col' pv 0 then r else pv) c [c + 1 .. n - 1]
              rc = at' m piv []
              rp = at' m c []
              m1 = [ if r == c then rc else if r == piv then rp else row | (r, row) <- zip [0 ..] m ]
              d = safeD (at' rc c 0)
              m2 = [ if r <= c then row else let f = at' row c 0 / d in [ if k >= c then v - f * at' rc k 0 else v | (k, v) <- zip [0 ..] row ]
                   | (r, row) <- zip [0 :: Int ..] m1 ]
          in elim (c + 1) (map forceL m2)
    backSub m = foldl' (\x r -> let row = at' m r []
                                    s = foldl' (\acc k -> acc - at' row k 0 * at' x k 0) (at' row n 0) [r + 1 .. n - 1]
                                in setAt r (s / safeD (at' row r 0)) x) (replicate n 0) [n - 1, n - 2 .. 0]

matVec :: [[Double]] -> [Double] -> [Double]
matVec a x = [ foldl' (+) 0 (zipWith (*) r x) | r <- a ]

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
