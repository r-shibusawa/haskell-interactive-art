-- ============================================
-- 50-08 高木-菅野型ファジィモデル
-- 規則の後件が「1 次式」：もし x が Aᵢ なら y = pᵢ x + qᵢ。出力は所属度で重みをつけた平均
--   y = Σ μᵢ(x) (pᵢ x + qᵢ) ÷ Σ μᵢ(x)　→　少ない規則で曲線（sin など）を近似できる
-- 操作：マウス = x を選ぶ / ←→ = 規則の数 / L = 後件の決め方（接線 ⇔ 最小二乗）/ 1〜3 = 近似する関数 / ↑↓ = MF の幅
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import System.Environment (lookupEnv)
import Numeric (showFFloat)

-- 近似する関数：名前・式・導関数・範囲
data Target = Target { tName :: String, tF :: Double -> Double, tDf :: Double -> Double, tLo :: !Double, tHi :: !Double }

targets :: [Target]
targets =
  [ Target "sin x" sin cos 0 (2 * pi)
  , Target "x^2 / 4 - 1" (\x -> (x * x) / 4 - 1) (\x -> x / 2) (-3) 3
  , Target "tanh(3x)" (\x -> tanh (3 * x)) (\x -> 3 / cosh (3 * x) ^ (2 :: Int)) (-2) 2 ]

-- 規則の集まり：前件のガウス MF と、後件の (pᵢ, qᵢ)
data Model = Model { mMfs :: [MF], mPq :: [(Double, Double)] }

-- 誤差：RMS と最大
data Err = Err { eRms :: !Double, eMax :: !Double }

data World = World
  { wN     :: !Int        -- ★ 規則の数（最初は 5）
  , wWidth :: !Double     -- ★ ガウス MF の幅（中心の間隔の何倍か。最初は 0.55）
  , wLS    :: !Bool       -- ★ True = 最小二乗で pᵢ, qᵢ を決める、False = 中心での接線
  , wTg    :: !Int        -- 近似する関数の番号
  , wModel :: Model
  , wErrs  :: [Err]       -- 規則の数 2〜12 の誤差
  , wCur   :: Err         -- 今のモデルの誤差
  , wTest  :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "50-08 Takagi-Sugeno model" world0)
  { skBack  = hsb 250 18 14
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = rebuild (World 5 0.55 True 0 (Model [] []) [] (Err 0 0) [])

target :: World -> Target
target w = getAt (head' targets) targets (wTg w)
  where head' ts = case ts of { (t : _) -> t; [] -> Target "x" id (const 1) 0 1 }

-- 規則を作る：中心 cᵢ を等間隔に、ガウス MF、後件 (pᵢ, qᵢ)
buildModel :: Target -> Int -> Bool -> Double -> Model
buildModel t n ls wk = Model mfs pq
  where
    d = (tHi t - tLo t) / fromIntegral (n - 1)
    mfs = [ Gauss (tLo t + fromIntegral i * d) (wk * d) | i <- [0 .. n - 1] ]
    pq = if ls then fitLS t mfs else [ let p = tDf t c in (p, tF t c - p * c) | Gauss c _ <- mfs ]

rebuild :: World -> World
rebuild w = w { wModel = m, wErrs = [ errorOf t (buildModel t n (wLS w) (wWidth w)) | n <- [2 .. 12] ], wCur = errorOf t m }
  where
    t = target w
    m = buildModel t (wN w) (wLS w) (wWidth w)

-- 高木-菅野の出力：重み付き平均（どの規則も発火しなければ Nothing）
tsk :: [MF] -> [(Double, Double)] -> Double -> Maybe Double
tsk mfs pq x = if sw > 1e-12 then Just (sy / sw) else Nothing
  where
    (sw, sy) = foldl' (\(a, b) (mf, (p, q)) -> let wv = evalMF mf x in (a + wv, b + wv * (p * x + q))) (0, 0) (zip mfs pq)

errorOf :: Target -> Model -> Err
errorOf t m = Err (sqrt (s / fromIntegral (n + 1))) mx
  where
    n = 400 :: Int
    es = [ maybe 0 id (tsk (mMfs m) (mPq m) x) - tF t x | k <- [0 .. n], let x = tLo t + ((tHi t - tLo t) * fromIntegral k) / fromIntegral n ]
    s = foldl' (\a e -> a + e * e) 0 es
    mx = foldl' (\a e -> max a (abs e)) 0 es

-- 最小二乗：y ≈ Σ wᵢ(x)(pᵢ x + qᵢ) を 201 点で合わせる（正規方程式をガウスの消去法で）
fitLS :: Target -> [MF] -> [(Double, Double)]
fitLS t mfs = pairs th
  where
    n = length mfs
    m = 2 * n
    xs = [ tLo t + ((tHi t - tLo t) * fromIntegral k) / 200 | k <- [0 .. 200 :: Int] ]
    rows = [ concat [ [(wi / sw) * x, wi / sw] | wi <- ws ]
           | x <- xs, let ws = map (`evalMF` x) mfs, let s0 = sum ws, let sw = if s0 == 0 then 1 else s0 ]
    bs = map (tF t) xs
    cols = transposeN m rows
    mat = [ [ (if i == j then 1e-9 else 0) + sum (zipWith (*) ci cj) | (j, cj) <- zip [0 :: Int ..] cols ] | (i, ci) <- zip [0 ..] cols ]
    rhs = [ sum (zipWith (*) ci bs) | ci <- cols ]
    th = solve mat rhs
    pairs (a : b : rest) = (a, b) : pairs rest
    pairs _ = []

-- 行列の転置（行の長さは m）
transposeN :: Int -> [[Double]] -> [[Double]]
transposeN m rows = [ map (\r -> getAt 0 r j) rows | j <- [0 .. m - 1] ]

draw :: Env -> World -> Picture
draw env w = view env $
  [ txt 20 34 17 (hsb 0 0 95) ("Left/Right: rules " ++ show n ++ "   L: consequents = " ++ (if wLS w then "least squares" else "tangent at center")
                               ++ "   1-3: f = " ++ tName t ++ "   Up/Down: MF width x" ++ fmt 2 (wWidth w) ++ "   mouse: x")
  , txt 20 60 15 (hsb 0 0 70) "thin = each rule's line p_i x + q_i (brightness = weight at x), white = true f, orange = Takagi-Sugeno output"
  , Color (hsba 0 0 100 0.2) (rectLineTL px py pw ph)
  , Color (hsba 0 0 100 0.2) (Line [(px, yOf 0), (px + pw, yOf 0)]) ]
  -- 規則ごとの 1 次式
  ++ [ Color (hsba 175 60 85 ((25 + 75 * realToFrac (if sw > 0 then wi / sw else 0)) / 100))
         (thickLine 2 (xOf x0, yOf (clampv (-1.6) 1.6 (p * x0 + q))) (xOf x1, yOf (clampv (-1.6) 1.6 (p * x1 + q))))
     | (Gauss c _, (p, q), wi) <- zip3 (mMfs m) (mPq m) ws
     , let d = (tHi t - tLo t) / fromIntegral (n - 1), let x0 = max (tLo t) (c - d), let x1 = min (tHi t) (c + d) ]
  ++ [ plotFn xOf yOf t (tF t) (hsba 0 0 100 0.7) 2
     , plotFn xOf yOf t (\x -> maybe 0 id (tsk (mMfs m) (mPq m) x)) (hsb 20 55 100) 3.5 ]
  -- MF（下の帯）
  ++ [ plotFn xOf (\v -> my + mh - realToFrac v * mh) t (evalMF mf) (hsba 260 30 90 0.7) 1.5 | mf <- mMfs m ]
  ++ [ Color white (thickLine 1.5 (xOf xm, py) (xOf xm, my + mh))
     , Color (hsb 20 55 100) (at (xOf xm, yOf (maybe 0 id yo)) (disc 6))
     , txt px (my + mh + 20) 14 (hsb 0 0 70) "antecedent membership functions mu_i(x) (Gaussian)" ]
  -- 表：今の x での計算
  ++ [ txt tx ty 17 (hsb 0 0 95) ("Computation at x = " ++ fmt 3 xm) ]
  ++ [ txt (tx + cx) (ty + 26) 14 (hsb 0 0 70) h | (cx, h) <- zip cols ["rule", "mu_i", "w=mu/Sum", "y=p x+q", "w y"] ]
  ++ concat [ let yi = p * xm + q
                  wb = if sw > 0 then wi / sw else 0
                  yy = ty + 26 + fromIntegral (i + 1) * rh
              in Color (hsba 175 60 50 0.6) (rectTL (tx + getAt 0 cols 2) (yy - rh + 8) (realToFrac wb * 60) (rh - 8))
                 : [ txt (tx + cx) yy 14 (hsb 0 0 92) s
                   | (cx, s) <- zip cols [show (i + 1), fmt 3 wi, fmt 3 wb, fmt 3 yi, fmt 3 (wb * yi)] ]
            | (i, ((p, q), wi)) <- zip [0 :: Int ..] (zip (mPq m) ws) ]
  ++ [ txt tx ye 18 (hsb 20 55 100) ("output y = Sum w_i y_i = " ++ fmt 4 tot)
     , txt tx (ye + 24) 15 (hsb 0 0 80) ("true f(x) = " ++ fmt 4 (tF t xm) ++ "   error = " ++ fmt 4 (tot - tF t xm))
  -- 規則の数と誤差のグラフ
     , txt gx (gy - 26) 14 (hsb 0 0 70) "rules 2-12 and max error (log scale)"
     , txt gx (gy - 8) 14 (hsb 0 0 70) ("now: RMS " ++ fmt 4 (eRms (wCur w)) ++ ", max " ++ fmt 4 (eMax (wCur w))) ]
  ++ concat [ let bw = gw / fromIntegral (length (wErrs w))
                  bh = realToFrac (clampv 0 1 ((logBase 10 (max (eMax e) 1e-5) + 5) / (logBase 10 emax + 5))) * gh
              in [ Color (if k + 2 == n then hsb 20 55 100 else hsb 260 30 70) (rectTL (gx + fromIntegral k * bw + 2) (gy + gh - bh) (bw - 4) bh)
                 , txt (gx + fromIntegral k * bw + bw / 2 - 7) (gy + gh + 18) 14 (hsb 0 0 70) (show (k + 2)) ]
            | (k, e) <- zip [0 :: Int ..] (wErrs w) ]
  ++ [ txt 20 (envH env - 20) 14 (if all snd (wTest w) then hsb 160 50 90 else hsb 0 80 100) (selfTestLine (wTest w)) ]
  where
    t = target w
    m = wModel w
    n = wN w
    px = 50
    py = 110
    pw = envW env * 0.58
    ph = envH env * 0.5
    xOf x = px + realToFrac ((x - tLo t) / (tHi t - tLo t)) * pw
    yOf y = py + ph / 2 - realToFrac (y / 1.6) * (ph / 2)
    (mxp, _) = p5Mouse env
    xm = clampv (tLo t) (tHi t) (tLo t + realToFrac ((mxp - px) / pw) * (tHi t - tLo t))
    ws = map (`evalMF` xm) (mMfs m)
    sw = sum ws
    my = py + ph + 20
    mh = envH env * 0.14
    yo = tsk (mMfs m) (mPq m) xm
    tx = envW env * 0.64
    ty = 110
    rh = min 26 (envH env * 0.5 / fromIntegral (n + 3))
    cols = [0, 44, 110, 205, 310] :: [Float]
    tot = sum [ (if sw > 0 then wi / sw else 0) * (p * xm + q) | ((p, q), wi) <- zip (mPq m) ws ]
    ye = ty + 26 + fromIntegral (n + 1) * rh + 8
    gx = tx
    gy = ye + 82          -- p5.js は ye + 60（2 行の見出しのぶん下げた）
    gw = envW env - tx - 40
    gh = envH env - gy - 70
    emax = max 1e-6 (foldl' max 0 (map eMax (wErrs w)))

-- 関数のグラフ（201 点の折れ線、縦は −1.6〜1.6 に切る）
plotFn :: (Double -> Float) -> (Double -> Float) -> Target -> (Double -> Double) -> Color -> Float -> Picture
plotFn xOf yOf t fn col sw = Color col (thickPath sw [ (xOf x, yOf (clampv (-1.6) 1.6 (fn x)))
                                                    | k <- [0 .. 200 :: Int], let x = tLo t + ((tHi t - tLo t) * fromIntegral k) / 200 ])

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild w { wN = max 2 (wN w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild w { wN = min 12 (wN w + 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild w { wWidth = min 1.5 (wWidth w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild w { wWidth = max 0.2 (wWidth w - 0.05) }
  EventKey (Char c) Down _ _
    | c == 'l' || c == 'L' -> rebuild w { wLS = not (wLS w) }
    | c `elem` "123" -> rebuild w { wTg = fromEnum c - fromEnum '1' }
  _ -> w

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("2 rules: at x = 1.5 mu = 0.25, 0.75, y1 = x + 1 = 2.5, y2 = 3x = 4.5 -> 0.25*2.5 + 0.75*4.5 = 4", nearM 1e-12 (tsk m2 pq2 1.5) 4)
  , ("if every consequent is y = 2x + 1, the output is exactly 2x + 1 (divided by the sum of weights)"
    , all (\x -> nearM 1e-12 (tsk m2 [(2, 1), (2, 1)] x) (2 * x + 1)) [0.3, 1.1, 1.9])
  , ("x where no rule fires (outside) -> no division by 0, 'no value'", maybe True (const False) (tsk m2 pq2 9))
  , ("least squares: a linear target gives error 0 (< 1e-6)", eMax (errorOf lin (buildModel lin 4 True 0.55)) < 1e-6)
  , ("sin x, tangent consequents: 3 -> 8 rules lowers the max error (" ++ fmt 3 e3 ++ " -> " ++ fmt 3 e8 ++ ")", e8 < e3)
  , ("sin x, 5 rules: least squares beats tangents", eMax (errorOf s (buildModel s 5 True 0.55)) < eMax (errorOf s (buildModel s 5 False 0.55)))
  , ("sin x with 8 rules by least squares: max error < 0.01", eMax (errorOf s (buildModel s 8 True 0.55)) < 0.01)
  , ("tangent consequents: rule i's line has the same value and slope as sin at its center c_i"
    , and [ nearTol 1e-12 (p * c + q) (sin c) && nearTol 1e-12 p (cos c) | (Gauss c _, (p, q)) <- zip (mMfs tg0) (mPq tg0) ]) ]
  where
    m2 = [Tri (-2) 0 2, Tri 0 2 4]
    pq2 = [(1, 1), (3, 0)]
    lin = Target "lin" (\x -> 3 - 0.5 * x) (const (-0.5)) 0 4
    s = case targets of { (t : _) -> t; [] -> lin }
    e3 = eMax (errorOf s (buildModel s 3 False 0.55))
    e8 = eMax (errorOf s (buildModel s 8 False 0.55))
    tg0 = buildModel s 5 False 0.55

-- ---------- やってみよう ----------
-- 1. 規則 2 個で sin x はどれくらい近似できる？ 何個で誤差 0.01 を切る？
-- 2. L で「接線」にすると、規則が少ないときの誤差はどうなる？ 最小二乗とくらべる
-- 3. ↑↓ で MF の幅を変えると、出力がギザギザになったり、なまったりする
-- 4. 2 の x²/4 − 1 は、規則 2 個でも意外とよく合う。なぜ？
-- 5. マウスで x を動かして、表の wᵢ · yᵢ を全部足すと出力になることを確かめる
-- 6. 最小二乗の 1 次式（細い線）は、接線とはかなりちがう向きになる。誤差は小さいが「規則の意味」は読みにくい

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

-- 左上が (x, y) の長方形の枠
rectLineTL :: Float -> Float -> Float -> Float -> Picture
rectLineTL x y w h = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- 小数点以下 n 桁で書く（p5.js の toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) v ""

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

-- メンバーシップ関数の形とパラメータ
data MF = Tri !Double !Double !Double | Gauss !Double !Double

evalMF :: MF -> Double -> Double
evalMF mf x = case mf of
  Tri a b c -> trimf x a b c
  Gauss c s -> gaussmf x c s

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- 連立 1 次方程式 M θ = r（部分ピボットつきガウスの消去法）。ピボットが 0 に近い列はとばし、その解は 0
solve :: [[Double]] -> [Double] -> [Double]
solve mat r = backSub (elim 0 (zipWith (\row v -> row ++ [v]) mat r))
  where
    n = length r
    at2 a i j = getAt 0 (getAt [] a i) j
    elim c a
      | c >= n = a
      | otherwise =
          let p = foldl' (\best i -> if abs (at2 a i c) > abs (at2 a best c) then i else best) c [c + 1 .. n - 1]
              a1 = [ if i == c then getAt [] a p else if i == p then getAt [] a c else row | (i, row) <- zip [0 ..] a ]
              piv = getAt [] a1 c
              pc = getAt 0 piv c
          in if abs pc < 1e-14 then elim (c + 1) a1
             else elim (c + 1) [ if i <= c then row
                                 else let fct = getAt 0 row c / pc
                                      in [ if j >= c then v - fct * pv else v | (j, v, pv) <- zip3 [0 ..] row piv ]
                               | (i, row) <- zip [0 ..] a1 ]
    backSub a = foldl' step [] [n - 1, n - 2 .. 0]
      where
        step xs i =
          let row = getAt [] a i
              s = getAt 0 row n - sum (zipWith (*) (drop (i + 1) row) xs)
              d = getAt 0 row i
          in (if abs d < 1e-14 then 0 else s / d) : xs

-- ---- 自己テストの道具
nearTol :: Double -> Double -> Double -> Bool
nearTol tol a b = abs (a - b) <= tol

-- 値がないこともある数（Nothing は不合格）
nearM :: Double -> Maybe Double -> Double -> Bool
nearM tol ma b = maybe False (\a -> nearTol tol a b) ma

-- 結果を端末に出す（テスト中は 1 行のまとめと失敗した項目だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK  " else "NG  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "NG ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)
