-- ============================================
-- 50-06 マムダニ型推論（エアコンの強さを決める）
-- 「暑くて じめじめ なら 強く」のような 9 個の規則で、温度と湿度からエアコンの強さを決める
--   入力 → 所属度（ファジィ化）→ 規則の強さ（min か 積）→ 後件を切る/縮める → max で集約 → 重心 → 出力
-- 操作：マウス = 入力のグラフをドラッグ / ←→ = 温度 / ↑↓ = 湿度 / 1 = 「かつ」min ⇔ 積 / 2 = 含意 切る ⇔ 縮める
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (foldl')
import Data.Word (Word8)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Vector.Unboxed as U
import System.Environment (lookupEnv)
import Numeric (showFFloat)

outNames :: [String]
outNames = ["very weak", "weak", "medium", "strong", "very strong"]

outShort :: [String]
outShort = ["v.weak", "weak", "medium", "strong", "v.strong"]

-- ★ 規則の表：行 = 温度（低い・ふつう・高い）、列 = 湿度（低い・ふつう・高い）、中身 = 出力の番号
rulesTable :: [[Int]]
rulesTable =
  [ [0, 1, 1]
  , [1, 2, 3]
  , [2, 3, 4] ]

surfN :: Int
surfN = 40            -- 出力の曲面の細かさ（40 × 40 の入力で推論）

data World = World
  { wTemp  :: !Double          -- ★ はじめの入力：温度 ℃（最初は 29）
  , wHum   :: !Double          -- ★ はじめの入力：湿度 %（最初は 70）
  , wSys   :: Sys
  , wSurf  :: U.Vector Double  -- 出力の曲面（設定を変えたときだけ計算）
  , wTest  :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "50-06 Mamdani inference" world0)
  { skBack  = hsb 250 18 14
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = rebuildSurface (World 29 70 (makeSystem False False) U.empty [])

outCol :: [Color]
outCol = [hsb 200 45 90, hsb 175 60 80, hsb 260 30 90, hsb 20 45 100, hsb 0 60 100]

hueOf :: Int -> Float
hueOf k = getAt 0 [200, 175, 260, 20, 0] k

-- 2 入力 1 出力のマムダニ型システム（andProd：「かつ」を積に、impProd：含意を「縮める」に）
makeSystem :: Bool -> Bool -> Sys
makeSystem andProd impProd = Sys
  { sInputs = [ Var "Temperature" "C" 10 40 (evenTri 10 40 ["low", "mid", "high"])
              , Var "Humidity" "%" 20 90 (evenTri 20 90 ["low", "mid", "high"]) ]
  , sOutput = Var "Power" "" 0 100 (evenTri 0 100 outNames)
  , sRules = [ Rule [i, j] (getAt 0 (getAt [] rulesTable i) j) | i <- [0 .. 2], j <- [0 .. 2] ]
  , sAndProd = andProd, sImpProd = impProd, sN = 401 }

-- 出力の曲面（40 × 40 の入力で推論した結果）。設定を変えたときだけ計算
rebuildSurface :: World -> World
rebuildSurface w = w { wSurf = U.fromList [ resY (mamdani s2 [10 + 30 * fromIntegral i / fromIntegral (surfN - 1), 90 - 70 * fromIntegral j / fromIntegral (surfN - 1)])
                                          | j <- [0 .. surfN - 1], i <- [0 .. surfN - 1] ] }
  where s2 = (wSys w) { sN = 101 }

surfColor :: Double -> (Word8, Word8, Word8)
surfColor y = let (r, g, b, _) = rgbaOfColor (hsb (wrapF 360 (220 + 1.6 * f y)) (30 + f y * 0.35) (40 + f y * 0.55))
              in (q r, q g, q b)
  where q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))

draw :: Env -> World -> Picture
draw env w = view env $
  [ txt 20 30 17 (hsb 0 0 95) ("Left/Right: temperature " ++ fmt 1 (wTemp w) ++ " C    Up/Down: humidity " ++ fmt 0 (wHum w) ++ " %    (or drag the input graphs)")
  , txt 20 53 16 (hsb 0 0 95) ("1: AND = " ++ (if sAndProd sys then "product" else "min") ++ "    2: implication = " ++ (if sImpProd sys then "scale (product)" else "clip (min)"))
  , txt 20 75 14 (hsb 0 0 70) "(1) fuzzify -> (2) rule strength w -> (3) clip consequents -> (4) aggregate by max -> (5) centroid = output" ]
  -- ① 入力のメンバーシップ関数と所属度
  ++ concat [ inputPanel k v | (k, v) <- zip [0 :: Int ..] (sInputs sys) ]
  -- ③ 出力の集合と、切った後件
  ++ [ drawSets c1 yo cw ph 0 100 (map fsMF (vSets (sOutput sys))) (repeat (hsba 0 0 100 0.25)) ]
  ++ [ fillCurve c1 yo cw ph oxs ys (getAt white outCol k)
     | (k, (s, fs)) <- zip [0 :: Int ..] (zip (resStr res) (vSets (sOutput sys))), s > 0
     , let ys = [ if sImpProd sys then s * m else min s m | x <- resOx res, let m = evalMF (fsMF fs) x ] ]
  ++ [ txt c1 (yo - 8) 15 (hsb 0 0 95) ("(3) consequents (" ++ (if sImpProd sys then "scaled by w" else "clipped at w") ++ ")") ]
  -- ② 規則の表（セルの明るさ = 強さ w）
  ++ [ txt c2 92 14 (hsb 0 0 70) ("(2) rule strength w = " ++ (if sAndProd sys then "product" else "min") ++ "(mu temp, mu hum)") ]
  ++ [ txt (tx + fromIntegral j * cs + 4) (ty - 4) 14 (hsb 0 0 70) ("hum " ++ fsName s) | (j, s) <- zip [0 :: Int ..] (vSets (getAt dummyVar (sInputs sys) 1)) ]
  ++ concat [ ruleCell n r wv | (n, (r, wv)) <- zip [0 :: Int ..] (zip (sRules sys) (resW res)) ]
  ++ [ Translate (tx - 10) (ty + fromIntegral i * cs + cs * 0.8) (Rotate 90 (txt 0 0 14 (hsb 0 0 70) ("temp " ++ fsName s)))
     | (i, s) <- zip [0 :: Int ..] (vSets (getAt dummyVar (sInputs sys) 0)) ]
  -- ④ 集約と ⑤ 重心
  ++ [ drawSets c2 yo cw ph 0 100 [] []
     , fillCurve c2 yo cw ph oxs (resAgg res) (hsb 20 45 100)
     , Color (hsb 175 60 90) (thickLine 3 (xc, yo - 4) (xc, yo + ph + 4))
     , Color (hsb 175 60 90) (Polygon [(xc - 9, yo + ph + 16), (xc + 9, yo + ph + 16), (xc, yo + ph + 3)])
     , txt c2 (yo - 8) 15 (hsb 0 0 95) ("(4) aggregate by max   (5) centroid (" ++ show (sN sys) ++ " points)")
     , txt c2 (yo + ph + 70) 14 (hsb 0 0 70) "centroid = int x mu dx / int mu dx (exact on each segment)"
     , txt c2 (yo + ph + 46) 24 (hsb 175 60 90) ("output = " ++ fmt 2 (resY res) ++ (if resFired res then "" else "  (nothing fired -> middle)")) ]
  -- 出力の曲面（ヒートマップ）
  ++ [ Translate (c3 + hs / 2) (100 + hs / 2) (Scale (hs / fromIntegral surfN) (negate hs / fromIntegral surfN)
         (pixelImage surfN surfN 1 (\i j -> surfColor (U.unsafeIndex (wSurf w) (j * surfN + i)))))
       | U.length (wSurf w) == surfN * surfN ]
  ++ [ Color white (at (mx, my) (thickCircle 6 2))
     , txt c3 86 14 (hsb 0 0 95) "Output surface: x temp, y humidity, color power"
     , txt c3 (100 + hs + 18) 14 (hsb 0 0 95) "10 C", txt (c3 + hs - 36) (100 + hs + 18) 14 (hsb 0 0 95) "40 C"
     , txt c3 (100 + hs + 44) 14 (hsb 0 0 70) ("fired rules: " ++ unwords ["R" ++ show (n + 1) | (n, wv) <- zip [0 :: Int ..] (resW res), wv > 0])
     , txt c3 (100 + hs + 66) 14 (hsb 0 0 70) ("area = " ++ fmt 2 (areaOf (resOx res) (resAgg res)))
     , txt 20 (envH env - 20) 14 (if all snd (wTest w) then hsb 160 50 90 else hsb 0 80 100) (selfTestLine (wTest w)) ]
  where
    sys = wSys w
    res = mamdani sys [wTemp w, wHum w]
    oxs = resOx res
    ww = envW env
    c1 = 20
    cw = ww * 0.3
    c2 = ww * 0.35
    c3 = ww * 0.69
    ph = min 150 ((envH env - 300) / 3)
    yo = 100 + 2 * (ph + 50)
    cs = min ((cw - 60) / 3) ((2 * ph + 20) / 3.3)
    tx = c2 + 60
    ty = 120
    xc = c2 + realToFrac (resY res / 100) * cw
    hs = min (ww - c3 - 30) (2 * ph + 50)
    mx = c3 + realToFrac ((wTemp w - 10) / 30) * hs
    my = 100 + realToFrac ((90 - wHum w) / 70) * hs
    inputPanel k v =
      let y0 = 100 + fromIntegral k * (ph + 50)
          val = if k == 1 then wHum w else wTemp w
          xv = c1 + realToFrac ((val - vLo v) / (vHi v - vLo v)) * cw
          degs = getAt [] (resDeg res) k
          cols = [getAt white outCol 1, getAt white outCol 2, getAt white outCol 3]
          nS = length (vSets v)
          -- 所属度は各集合の頂点の上に書く
          degLabel i (s, d) =
            let str = fsName s ++ " " ++ fmt 2 d
                px = c1 + cw * fromIntegral i / fromIntegral (max 1 (nS - 1))
                wd = fromIntegral (length str) * 14 * 0.6
                lx = if i == 0 then c1 + 4 else if i == nS - 1 then c1 + cw - wd - 4 else px - wd / 2
            in txt lx (y0 + 18) 14 (getAt white cols i) str
      in [ drawSets c1 y0 cw ph (vLo v) (vHi v) (map fsMF (vSets v)) cols
         , Color (hsb 0 0 95) (thickLine 2 (xv, y0) (xv, y0 + ph))
         , txt c1 (y0 - 8) 15 (hsb 0 0 95) ("(1) " ++ vName v ++ " = " ++ fmt 1 val ++ " " ++ vUnit v) ]
         ++ zipWith degLabel [0 :: Int ..] (zip (vSets v) degs)
    ruleCell n r wv =
      let (i, j) = case rIfs r of { (a : b : _) -> (a, b); _ -> (0, 0) }
          x = tx + fromIntegral j * cs
          y = ty + fromIntegral i * cs
          tcol = hsb 0 0 (if wv > 0.5 then 10 else 95)
      in [ Color (hsb (hueOf (rThen r)) 40 (18 + 70 * realToFrac wv)) (rectTL x y (cs - 6) (cs - 6))
         , Color (hsba 0 0 100 0.3) (rectLineTL x y (cs - 6) (cs - 6))
         , txt (x + 6) (y + 20) 14 tcol ("R" ++ show (n + 1) ++ " ->")
         , txt (x + 6) (y + 38) 14 tcol (getAt "" outShort (rThen r))
         , txt (x + 6) (y + min (cs - 16) 66) 18 tcol ("w=" ++ fmt 2 wv) ]

dummyVar :: Var
dummyVar = Var "" "" 0 1 []

-- グラフの枠と MF（折れ線）を描く
drawSets :: Float -> Float -> Float -> Float -> Double -> Double -> [MF] -> [Color] -> Picture
drawSets x y w h lo hi mfs cols = Pictures $
  Color (hsba 0 0 100 0.25) (rectLineTL x y w h)
  : [ Color c (thickPath 2.5 [ (x + fromIntegral i / 120 * w, y + h - realToFrac (evalMF mf v) * h)
                              | i <- [0 .. 120 :: Int], let v = lo + (hi - lo) * fromIntegral i / 120 ])
    | (mf, c) <- zip mfs cols ]

-- 折れ線の下を塗る（gloss は凸の多角形しか塗れないので、細い台形を並べる）
fillCurve :: Float -> Float -> Float -> Float -> [Double] -> [Double] -> Color -> Picture
fillCurve x y w h xs ys col = case (xs, reverse xs) of
  (lo : _, hi : _) | hi > lo ->
    let pts = [ (x + realToFrac ((xv - lo) / (hi - lo)) * w, y + h - realToFrac yv * h) | (xv, yv) <- every 2 (zip xs ys) ]
        quads = [ Polygon [(x0, y + h), (x0, y0), (x1, y1), (x1, y + h)] | ((x0, y0), (x1, y1)) <- zip pts (drop 1 pts) ]
    in Pictures [Color (withA 0.45 col) (Pictures quads), Color col (thickPath 2 pts)]
  _ -> Blank

-- k 個おきに取り出す（最後の点は必ず入れる）
every :: Int -> [a] -> [a]
every k xs = go (0 :: Int) xs
  where
    go _ [] = []
    go i [z] = [z]
    go i (z : zs) = if i `mod` k == 0 then z : go (i + 1) zs else go (i + 1) zs

setFromMouse :: Env -> World -> World
setFromMouse env w
  | mx < 20 || mx > 20 + cw = w
  | my > 100 && my < 100 + ph = w { wTemp = clampv 10 40 (10 + 30 * u) }
  | my > 150 + ph && my < 150 + 2 * ph = w { wHum = clampv 20 90 (20 + 70 * u) }
  | otherwise = w
  where
    (mx, my) = p5Mouse env
    cw = envW env * 0.3
    ph = min 150 ((envH env - 300) / 3)
    u = realToFrac ((mx - 20) / cw)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> setFromMouse env w
  EventMotion _ | envDown env -> setFromMouse env w
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wTemp = clampv 10 40 (wTemp w - 0.5) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wTemp = clampv 10 40 (wTemp w + 0.5) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wHum = clampv 20 90 (wHum w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wHum = clampv 20 90 (wHum w - 1) }
  EventKey (Char '1') Down _ _ -> rebuildSurface w { wSys = makeSystem (not (sAndProd (wSys w))) (sImpProd (wSys w)) }
  EventKey (Char '2') Down _ _ -> rebuildSurface w { wSys = makeSystem (sAndProd (wSys w)) (not (sImpProd (wSys w))) }
  _ -> w

-- ---- 自己テスト：手で計算できる値と比べる
selfTest :: [(String, Bool)]
selfTest =
  [ ("25 C, 55 % (both 'mid' peaks) -> only R5 fires with w = 1, output = centroid of 'medium' = 50"
    , length (filter (> 0) (resW r1)) == 1 && near (getAt 0 (resW r1) 4) 1 && nearM 1e-6 (Just (resY r1)) 50)
  , ("10 C, 20 % -> only R1. 'very weak' is a right triangle 0..25 -> centroid 25/3 = 8.33", nearTol 1e-6 (resY r2) (25 / 3))
  , ("17.5 C, 55 % -> R2 and R5 with w = 0.5; clipped 'weak' + 'medium' symmetric about 37.5"
    , near (getAt 0 (resW r3) 1) 0.5 && near (getAt 0 (resW r3) 4) 0.5 && nearTol 1e-6 (resY r3) 37.5)
  , ("17.5 C, 37.5 % -> 4 rules: 0.5 with min, 0.25 with product"
    , all (\k -> near (getAt 0 (resW r4) k) 0.5 && near (getAt 0 (resW rp) k) 0.25) [0, 1, 3, 4])
  , ("triangle (25, 50, 75) clipped at 0.6: centroid = 50, area = 0.6 x (50 + 20) / 2 = 21"
    , nearM 1e-9 (centroid xs ys) 50 && nearTol 1e-9 (areaOf xs ys) 21)
  , ("no rule fires -> no division by 0, returns the middle 50", not (resFired empty) && resY empty == 50)
  , ("output stays in 0..100 and never weakens as the temperature rises (the table is monotone)", mono) ]
  where
    s = makeSystem False False
    r1 = mamdani s [25, 55]
    r2 = mamdani s [10, 20]
    r3 = mamdani s [17.5, 55]
    r4 = mamdani s [17.5, 37.5]
    rp = mamdani (makeSystem True False) [17.5, 37.5]
    xs = [fromIntegral i * 0.25 | i <- [0 .. 400 :: Int]]
    ys = [min 0.6 (trimf x 25 50 75) | x <- xs]
    empty = mamdani s { sRules = [] } [30, 60]
    mono = and [ ok | h <- [20, 30 .. 90]
                    , let outs = [resY (mamdani s { sN = 201 } [t, h]) | t <- [10, 12 .. 40]]
                    , ok <- zipWith (\prev y -> not (y < prev - 1e-6 || y < 0 || y > 100)) (-1 : outs) outs ]

-- ---------- やってみよう ----------
-- 1. 温度を 17.5℃ にすると「低い」と「ふつう」が 0.5 ずつ。どの規則が発火する？
-- 2. 1 で「かつ」を積にすると、w はどう変わる？ 曲面はなめらかになる？
-- 3. 2 で「縮める」にすると、集約の形と出力が少し変わる
-- 4. rulesTable を書きかえて（例：湿度が高いと とても強）、曲面の形を見る
-- 5. 出力の集合を 3 個にすると、曲面はどうなる？（outNames と rulesTable を変える）

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

-- Double → Float（描くときだけ）
f :: Double -> Float
f = realToFrac

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

-- ---- 非ファジィ化（xs, ys は同じ長さ。ys は折れ線の高さ）
-- 重心 = ∫x μ dx ÷ ∫μ dx（折れ線の区間ごとに正確に積分）。どの規則も発火しなければ Nothing
centroid :: [Double] -> [Double] -> Maybe Double
centroid xs ys = if a > 1e-12 then Just (m / a) else Nothing
  where
    ps = zip xs ys
    (a, m) = foldl' step (0, 0) (zip ps (drop 1 ps))
    step (!aa, !mm) ((x0, y0), (x1, y1)) =
      let h = x1 - x0
      in (aa + (h * (y0 + y1)) / 2, mm + (h * (x0 * (2 * y0 + y1) + x1 * (y0 + 2 * y1))) / 6)

areaOf :: [Double] -> [Double] -> Double
areaOf xs ys = foldl' (\acc ((x0, y0), (x1, y1)) -> acc + ((x1 - x0) * (y0 + y1)) / 2) 0 (zip ps (drop 1 ps))
  where ps = zip xs ys

-- ---- マムダニ型推論
-- 言語変数：名前・単位・範囲・ファジィ集合の列。規則：前件の集合の番号（入力ごと、−1 は使わない）と後件の番号
data FSet = FSet { fsName :: String, fsMF :: MF }
data Var = Var { vName :: String, vUnit :: String, vLo :: !Double, vHi :: !Double, vSets :: [FSet] }
data Rule = Rule { rIfs :: [Int], rThen :: !Int }
data Sys = Sys { sInputs :: [Var], sOutput :: Var, sRules :: [Rule]
               , sAndProd :: !Bool     -- 「かつ」：False = min、True = 積
               , sImpProd :: !Bool     -- 含意：False = 切る（min）、True = 縮める（積）
               , sN :: !Int }          -- 出力を何点で積分するか

-- 推論の途中経過を全部返す：所属度・規則の強さ・後件ごとの強さ・集約した形・重心
data Res = Res { resDeg :: [[Double]], resW :: [Double], resStr :: [Double]
               , resOx :: [Double], resAgg :: [Double], resY :: !Double, resFired :: !Bool }

mamdani :: Sys -> [Double] -> Res
mamdani sys xin = Res deg ws str ox agg (fromMaybe ((vLo o + vHi o) / 2) c) (isJust c)
  where
    deg = [ [evalMF (fsMF s) (clampv (vLo v) (vHi v) x) | s <- vSets v] | (v, x) <- zip (sInputs sys) xin ]
    ws = [ foldl' andF 1 (zip [0 ..] (rIfs r)) | r <- sRules sys ]
    andF acc (i, si)
      | si < 0 = acc
      | sAndProd sys = acc * d
      | otherwise = min acc d
      where d = getAt 0 (getAt [] deg i) si
    o = sOutput sys
    str = [ foldl' max 0 [wv | (r, wv) <- zip (sRules sys) ws, rThen r == k] | k <- [0 .. length (vSets o) - 1] ]   -- 同じ後件は max でまとめる
    n = sN sys
    ox = [ vLo o + ((vHi o - vLo o) * fromIntegral i) / fromIntegral (n - 1) | i <- [0 .. n - 1] ]
    live = [ (s, fsMF fs) | (s, fs) <- zip str (vSets o), s > 0 ]
    agg = [ foldl' (\a (s, mf) -> let m = evalMF mf x in max a (if sImpProd sys then s * m else min s m)) 0 live | x <- ox ]   -- 切る（min）か縮める（積）
    c = centroid ox agg

-- 等間隔の三角 MF を n 個（両端は肩）
evenTri :: Double -> Double -> [String] -> [FSet]
evenTri lo hi names = [ FSet nm (Tri (lo + fromIntegral (i - 1) * d) (lo + fromIntegral i * d) (lo + fromIntegral (i + 1) * d))
                      | (i, nm) <- zip [0 :: Int ..] names ]
  where d = (hi - lo) / fromIntegral (length names - 1)

-- ---- 自己テストの道具
near :: Double -> Double -> Bool
near a b = nearTol 1e-9 a b

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
