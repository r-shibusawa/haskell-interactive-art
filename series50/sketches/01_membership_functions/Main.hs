-- ============================================
-- 50-01 メンバーシップ関数の形
-- 三角・台形・ガウス・シグモイド・ベル。どれも x に所属度 μ(x)（0〜1）を対応させる関数
-- 台（μ > 0 の範囲）・核（μ = 1 の範囲）・α カット（μ ≥ α の範囲）・高さ（μ の最大）・正規（高さ = 1）を式で計算して表示
-- 操作：1〜5 = 形 / ○ をドラッグ = パラメータ / ↑↓ = α / ←→ = ベルの肩の急さ b
-- ============================================
module Main (main) where

import Art
import System.Environment (lookupEnv)
import Numeric (showFFloat)

xLo, xHi :: Double
xLo = 0
xHi = 10

-- 形の種類（記号と画面の名前）
shapes :: [(String, String)]
shapes = [("tri", "Triangle"), ("trap", "Trapezoid"), ("gauss", "Gaussian"), ("sig", "Sigmoid"), ("bell", "Bell")]

data World = World
  { wShape :: !Int
  , wAlpha :: !Double          -- ★ α カットの α（最初は 0.5）
  -- ★ それぞれの形のパラメータ（ドラッグで変わる）
  , wTri   :: [Double]         -- [a, b, c]
  , wTrap  :: [Double]         -- [a, b, c, d]
  , wGauss :: [Double]         -- 中心 c、幅 σ
  , wSig   :: [Double]         -- 傾き a、真ん中 c
  , wBell  :: [Double]         -- 幅 a、肩 b、中心 c
  , wDrag  :: !Int             -- ドラッグ中の点（なければ −1）
  , wTest  :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "50-01 Membership functions" world0)
  { skBack  = hsb 250 18 14
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 0 0.5 [2, 3, 4] [1.5, 3.5, 6, 8] [5, 1.2] [2, 5] [1.5, 2, 5] (-1) []

shapeKey :: World -> String
shapeKey w = fst (getAt ("tri", "") shapes (wShape w))

params :: World -> [Double]
params w = case shapeKey w of
  "tri" -> wTri w
  "trap" -> wTrap w
  "gauss" -> wGauss w
  "sig" -> wSig w
  _ -> wBell w

curMF :: World -> MF
curMF w = mfOf (shapeKey w) (params w)

mfOf :: String -> [Double] -> MF
mfOf t p = case t of
  "tri" -> Tri (g 0) (g 1) (g 2)
  "trap" -> Trap (g 0) (g 1) (g 2) (g 3)
  "gauss" -> Gauss (g 0) (g 1)
  "sig" -> Sig (g 0) (g 1)
  _ -> Bell (g 0) (g 1) (g 2)
  where g = getAt 0 p

-- ドラッグできる点（x 座標）。ガウスは c と c + σ、シグモイドは c と c + 2/a、ベルは c と c + a
handles :: World -> [Double]
handles w = case shapeKey w of
  "gauss" -> [g 0, g 0 + g 1]
  "sig" -> [g 1, g 1 + 2 / g 0]
  "bell" -> [g 2, g 2 + g 0]
  _ -> p
  where
    p = params w
    g = getAt 0 p

setHandle :: Int -> Double -> World -> World
setHandle k x w = case shapeKey w of
  "tri" -> w { wTri = ordered False (setAt k x (wTri w)) }
  "trap" -> w { wTrap = ordered True (setAt k x (wTrap w)) }
  "gauss" -> let p = wGauss w in w { wGauss = if k == 0 then setAt 0 x p else setAt 1 (clampv 0.1 5 (x - getAt 0 p 0)) p }
  "sig" -> let p = wSig w
               d = x - getAt 0 p 1
           in w { wSig = if k == 0 then setAt 1 x p else setAt 0 (if abs d < 0.1 then (if d < 0 then -20 else 20) else 2 / d) p }
  _ -> let p = wBell w in w { wBell = if k == 0 then setAt 2 x p else setAt 0 (clampv 0.1 5 (x - getAt 0 p 2)) p }
  where
    -- 順番を保つ：前から「前の点 + 0.05 以上」、後ろから「次の点以下」（台形の 2 番目と 3 番目は重なってよい）
    ordered isTrap p =
      let fwd = foldl (\acc (i, v) -> case acc of
                         [] -> [v]
                         _ -> acc ++ [max v (last acc + (if i == (2 :: Int) && isTrap then 0 else 0.05))]) [] (zip [0 ..] p)
      in foldr (\v acc -> case acc of
                  [] -> [v]
                  (n : _) -> min v n : acc) [] fwd

-- ---- 台・核・α カット・高さを式で求める（[左, 右] の区間。空なら Nothing）
data Analysis = Analysis { aSup :: (Double, Double), aCore :: Maybe (Double, Double), aH :: Double, aCut :: (Double, Double) }

analyze :: String -> [Double] -> Double -> Analysis
analyze t p al = case t of
  "tri" -> Analysis (g 0, g 2) (Just (g 1, g 1)) 1 (cutTri (g 0, g 1, g 2) al)
  "trap" -> Analysis (g 0, g 3) (Just (g 1, g 2)) 1 (g 0 + al * (g 1 - g 0), g 3 - al * (g 3 - g 2))
  "gauss" -> let r = g 1 * sqrt (-2 * log al) in Analysis whole (Just (g 0, g 0)) 1 (g 0 - r, g 0 + r)
  "sig" -> let x = g 1 - log ((1 - al) / al) / g 0
           in Analysis whole Nothing 1 (if g 0 > 0 then (max xLo x, xHi) else (xLo, min xHi x))
  _ -> let r = g 0 * ((1 - al) / al) ** (1 / (2 * g 1)) in Analysis whole (Just (g 2, g 2)) 1 (g 2 - r, g 2 + r)
  where
    g = getAt 0 p
    whole = (xLo, xHi)       -- 台が全体（式では (−∞, ∞) を画面の範囲で切る）

ivStr :: (Double, Double) -> String
ivStr (a, b) = if a <= b then "[" ++ fmt 2 a ++ ", " ++ fmt 2 b ++ "]" else "empty"

-- グラフの枠
data Graph = Graph { gX, gY, gW, gH :: !Float }

graphOf :: Env -> Graph
graphOf env = Graph 70 110 (envW env * 0.62) (envH env * 0.55)

draw :: Env -> World -> Picture
draw env w = view env $
  [ txt 20 34 17 (hsb 0 0 95) ("1-5: shape = " ++ unwords [if i == wShape w then "[" ++ s ++ "]" else s | (i, (_, s)) <- zip [0 ..] shapes])
  , txt 20 58 16 (hsb 0 0 95) ("Drag o: parameters    Up/Down: alpha = " ++ fmt 2 al ++ (if t == "bell" then "    Left/Right: b = " ++ fmt 1 (getAt 0 (wBell w) 1) else ""))
  , txt 20 82 15 (hsb 0 0 70) "Support: mu > 0.  Core: mu = 1.  Alpha-cut: mu >= alpha.  Stacking all alpha-cuts gives back the set"
  , Color (hsb 0 0 40) (Line [(gx, yOf 0), (gx + gw, yOf 0)])
  , Color (hsb 0 0 40) (Line [(gx, gy), (gx, yOf 0)]) ]
  ++ [ txt (xOf x - 5) (yOf 0 + 18) 14 (hsb 0 0 60) (show (round x :: Int)) | x <- [xLo .. xHi] ]
  ++ [ txt (gx - 18) (yOf 1 + 5) 14 (hsb 0 0 60) "1", txt (gx - 18) (yOf 0 + 5) 14 (hsb 0 0 60) "0"
       -- α の線と α カット（帯）
     , Color (hsba 260 30 90 0.6) (Line [(gx, yOf al), (gx + gw, yOf al)]) ]
  ++ (let (ca, cb) = aCut an
          a = clampv xLo xHi ca
          b = clampv xLo xHi cb
      in if cb >= ca
           then [ Color (hsba 260 30 90 0.25) (rectTL (xOf a) (yOf al) (max 2 (xOf b - xOf a)) (yOf 0 - yOf al))
                , Color (hsb 260 30 90) (thickLine 6 (xOf a, yOf 0 + 30) (xOf b, yOf 0 + 30)) ]
           else [])
     -- 台と核
  ++ [ Color (hsb 175 60 80) (thickLine 6 (xOf (clampv xLo xHi (fst (aSup an))), yOf 0 + 46) (xOf (clampv xLo xHi (snd (aSup an))), yOf 0 + 46)) ]
  ++ [ Color (hsb 20 45 100) (thickLine 6 (xOf (clampv xLo xHi c0), yOf 0 + 62) (xOf (clampv xLo xHi c1) + 1, yOf 0 + 62)) | Just (c0, c1) <- [aCore an] ]
  ++ [ txt (gx + gw + 10) (yOf 0 + 36) 14 (hsb 260 30 90) "alpha-cut"
     , txt (gx + gw + 10) (yOf 0 + 52) 14 (hsb 175 60 80) "support"
     , txt (gx + gw + 10) (yOf 0 + 68) 14 (hsb 20 45 100) "core"
       -- 曲線
     , Color (hsb 20 45 100) (thickPath 4 [ (xOf x, yOf (evalMF mf x)) | i <- [0 .. 400 :: Int], let x = xLo + (xHi - xLo) * fromIntegral i / 400 ]) ]
     -- ドラッグできる点
  ++ concat [ [ Color (if k == wDrag w then hsb 20 45 100 else hsb 250 18 14) (at p (disc 9))
              , Color (hsb 0 0 100) (at p (ring 2 9)) ]
            | (k, hx) <- zip [0 ..] (handles w), let p = (xOf (clampv xLo xHi hx), yOf (evalMF mf hx)) ]
     -- ---- 右の表
  ++ [ txt tx ty 20 (hsb 0 0 95) (snd (getAt ("", "") shapes (wShape w)) ++ "  " ++ t ++ "mf") ]
  ++ concat [ [ txt tx (ty + 40 + fromIntegral i * 30) 15 (hsb 0 0 65) k, txt (tx + 100) (ty + 40 + fromIntegral i * 30) 15 (hsb 0 0 95) v ]
            | (i, (k, v)) <- zip [0 :: Int ..] rows ]
  ++ mouseRow
  ++ [ txt 20 (envH env - 20) 14 (if all snd (wTest w) then hsb 160 50 90 else hsb 0 80 100) (selfTestLine (wTest w)) ]
  where
    Graph gx gy gw gh = graphOf env
    xOf x = gx + realToFrac ((x - xLo) / (xHi - xLo)) * gw
    yOf m = gy + gh - realToFrac m * gh
    al = wAlpha w
    t = shapeKey w
    mf = curMF w
    an = analyze t (params w) al
    tx = gx + gw + 70
    ty = 130
    rows =
      [ ("params", commaSep (map (fmt 2) (params w)))
      , ("support", ivStr (aSup an) ++ (if t == "tri" || t == "trap" then "" else " = all"))
      , ("core", maybe "empty (never 1)" ivStr (aCore an))
      , ("alpha-cut", ivStr (aCut an))
      , ("height", fmt 2 (aH an) ++ (case aCore an of { Nothing -> " (sup)"; _ -> "" }))
      , ("normal?", case aCore an of { Nothing -> "no"; _ -> "yes (mu = 1 exists)" })
      , ("convex?", if t == "sig" then "yes (one-sided)" else "yes (one interval)") ]
    (mx, _) = p5Mouse env
    mouseRow
      | mx > gx && mx < gx + gw =
          let x = xLo + realToFrac ((mx - gx) / gw) * (xHi - xLo)
          in [ Color (hsba 0 0 100 0.3) (Line [(xOf x, gy), (xOf x, yOf 0)])
             , txt tx (ty + 290) 15 (hsb 0 0 95) ("x = " ++ fmt 2 x ++ "   mu(x) = " ++ fmt 3 (evalMF mf x)) ]
      | otherwise = []

commaSep :: [String] -> String
commaSep [] = ""
commaSep (s : ss) = s ++ concatMap (", " ++) ss

-- グラフの x 座標 → 値
plotX :: Env -> Float -> Double
plotX env mx = let Graph gx _ gw _ = graphOf env in xLo + realToFrac ((mx - gx) / gw) * (xHi - xLo)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w { wDrag = pick }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion _ | envDown env && wDrag w >= 0 -> setHandle (wDrag w) (clampv xLo xHi (plotX env mx)) w
  EventKey (Char c) Down _ _ | c >= '1' && c <= '5' -> w { wShape = fromEnum c - fromEnum '1' }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wAlpha = clampv 0.05 1 (wAlpha w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wAlpha = clampv 0.05 1 (wAlpha w - 0.05) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wBell = setAt 1 (clampv 0.5 10 (getAt 0 (wBell w) 1 - 0.5)) (wBell w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wBell = setAt 1 (clampv 0.5 10 (getAt 0 (wBell w) 1 + 0.5)) (wBell w) }
  _ -> w
  where
    (mx, my) = p5Mouse env
    Graph gx gy gw gh = graphOf env
    mf = curMF w
    -- いちばん近い点（40 ピクセル以内）
    cands = [ (d, k) | (k, hx) <- zip [0 ..] (handles w)
            , let px = gx + realToFrac ((hx - xLo) / (xHi - xLo)) * gw
                  py = gy + gh - realToFrac (evalMF mf hx) * gh
                  d = sqrt ((mx - px) ^ (2 :: Int) + (my - py) ^ (2 :: Int))
            , d < 40 ]
    pick = case cands of
      [] -> -1
      _ -> snd (minimum cands)

-- ---- 自己テスト：手で計算できる値と比べる
selfTest :: [(String, Bool)]
selfTest =
  [ ("triangle (2, 3, 4): mu(3) = 1, mu(2.5) = 0.5, mu(4.5) = 0", trimf 3 2 3 4 == 1 && near (trimf 2.5 2 3 4) 0.5 && trimf 4.5 2 3 4 == 0)
  , ("0.5-cut of triangle (2, 3, 4) = [2.5, 3.5]", let (c0, c1) = cutTri (2, 3, 4) 0.5 in near c0 2.5 && near c1 3.5)
  , ("trapezoid (1, 2, 4, 6): 1 on the core [2, 4], mu(5) = 0.5", trapmf 2 1 2 4 6 == 1 && trapmf 4 1 2 4 6 == 1 && near (trapmf 5 1 2 4 6) 0.5)
  , ("Gaussian: mu(c +- sigma) = e^(-1/2) = 0.6065", near (gaussmf 6 5 1) (exp (-0.5)) && near (gaussmf 4 5 1) (exp (-0.5)))
  , ("bell (a, b, c): mu(c +- a) = 0.5, mu(c) = 1", near (bellmf 6.5 1.5 2 5) 0.5 && near (bellmf 3.5 1.5 2 5) 0.5 && bellmf 5 1.5 2 5 == 1)
  , ("sigmoid: mu(c) = 0.5", near (sigmf 5 2 5) 0.5)
    -- 式の α カットと、細かく調べた α カットが一致するか（ガウス・ベル・シグモイド）
  , ("alpha-cut formula = scanned range (Gauss, bell, sigmoid x 3 alphas)", and [ scanOk t p al | (t, p) <- [("gauss", [5, 1.2]), ("bell", [1.5, 2, 5]), ("sig", [2, 5])], al <- [0.2, 0.5, 0.8] ]) ]
  where
    scanOk t p al =
      let mf = mfOf t p
          xs = [x | i <- [0 .. 100000 :: Int], let x = 10 * fromIntegral i / 100000, evalMF mf x >= al]
          (c0, c1) = aCut (analyze t p al)
      in case xs of
           [] -> False
           (lo : _) -> nearTol 2e-4 lo c0 && nearTol 2e-4 (last xs) c1

-- ---------- やってみよう ----------
-- 1. 三角の 3 つの点をドラッグして、左右非対称な「だいたい 3」を作ろう。α カットはどう変わる？
-- 2. 台形の真ん中の 2 点を重ねると三角になる。核が「区間」から「点」になるのを見よう
-- 3. ベルで ←→ を押して b を大きくすると、だんだん台形（肩の急な形）に近づく
-- 4. シグモイドの傾きの点を左に動かすと a が負になり、「大きい」が「小さい」に変わる
-- 5. ↑↓ で α を動かし、α = 1 のとき α カットが核と一致することを確かめよう

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

-- 横は中央、y が文字の下の線（p5.js の textAlign(CENTER)）
txtCB :: Float -> Float -> Float -> Color -> String -> Picture
txtCB x y size c s = txt (x - fromIntegral (length s) * size * 0.36) y size c s

-- 左上が (x, y) の塗りつぶした長方形（p5.js の rect）
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 小数点以下 n 桁で書く（p5.js の toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) v ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Int
jsRound v = floor (v + 0.5)


-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- リストの i 番目を v に変える
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if j == i then v else x | (j, x) <- zip [0 ..] xs]

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

trapmf :: Double -> Double -> Double -> Double -> Double -> Double   -- 台形：b〜c が 1
trapmf x a b c d
  | x < a || x > d = 0
  | x >= b && x <= c = 1
  | x < b = (x - a) / (b - a)
  | otherwise = (d - x) / (d - c)

gaussmf :: Double -> Double -> Double -> Double
gaussmf x c s = exp (-((x - c) * (x - c)) / (2 * s * s))

sigmf :: Double -> Double -> Double -> Double
sigmf x a c = 1 / (1 + exp (-a * (x - c)))

bellmf :: Double -> Double -> Double -> Double -> Double
bellmf x a b c = 1 / (1 + abs ((x - c) / a) ** (2 * b))

-- メンバーシップ関数の形とパラメータ
data MF = Tri !Double !Double !Double | Trap !Double !Double !Double !Double
        | Gauss !Double !Double | Sig !Double !Double | Bell !Double !Double !Double

evalMF :: MF -> Double -> Double
evalMF mf x = case mf of
  Tri a b c -> trimf x a b c
  Trap a b c d -> trapmf x a b c d
  Gauss c s -> gaussmf x c s
  Sig a c -> sigmf x a c
  Bell a b c -> bellmf x a b c

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- ---- 三角ファジィ数 (a, b, c) の α カット [a + α(b − a), c − α(c − b)] と区間の演算
cutTri :: (Double, Double, Double) -> Double -> (Double, Double)
cutTri (a, b, c) al = (a + al * (b - a), c - al * (c - b))

-- 区間の四則（÷ は 0 をふくむ区間では計算できない → Nothing）
ivOps :: [(String, (Double, Double) -> (Double, Double) -> Maybe (Double, Double))]
ivOps =
  [ ("+", \(p0, p1) (q0, q1) -> Just (p0 + q0, p1 + q1))
  , ("-", \(p0, p1) (q0, q1) -> Just (p0 - q1, p1 - q0))
  , ("x", \p q -> Just (ivMul p q))
  , ("/", \p (q0, q1) -> if q0 <= 0 && q1 >= 0 then Nothing else Just (ivMul p (1 / q1, 1 / q0))) ]

ivMul :: (Double, Double) -> (Double, Double) -> (Double, Double)
ivMul (p0, p1) (q0, q1) = let v = [p0 * q0, p0 * q1, p1 * q0, p1 * q1] in (minimum v, maximum v)

-- ---- 自己テストの道具
near :: Double -> Double -> Bool
near a b = nearTol 1e-9 a b

nearTol :: Double -> Double -> Double -> Bool
nearTol tol a b = abs (a - b) <= tol

-- 結果を端末に出す（テスト中は 1 行のまとめと失敗した項目だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK  " else "NG  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "NG ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

