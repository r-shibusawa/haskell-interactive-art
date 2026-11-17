-- ============================================
-- 50-02 ファジィ集合の演算 ─ かつ・または・でない
-- AND（t ノルム）：min・積・ウカシェヴィチ max(0, a + b − 1)　OR（s ノルム）：max・確率和 a + b − ab・有界和
-- NOT（補集合）：1 − μ。ファジィでは「A かつ A でない」が空にならない（排中律・矛盾律が成り立たない）
-- 操作：ドラッグ = A・B を動かす / 1〜3 = AND の種類 / 4〜6 = OR の種類 / スペース = 上の図を AND → OR → NOT A
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import System.Environment (lookupEnv)
import Numeric (showFFloat)

wA, wB :: Double
wA = 2.5             -- ★ 三角の半分の幅
wB = 2

xLo, xHi :: Double
xLo = 0
xHi = 10

nX :: Int
nX = 1000            -- x は 0.01 刻み（三角の折れ目が格子に乗る）

views :: [String]
views = ["A AND B", "A OR B", "NOT A"]

data World = World
  { wPA   :: !Double     -- ★ A の頂点の位置（最初は 4）
  , wPB   :: !Double     -- ★ B の頂点の位置（最初は 6.5）
  , wTn   :: !Int        -- AND の種類
  , wSn   :: !Int        -- OR の種類
  , wView :: !Int        -- 上の図：0 = AND、1 = OR、2 = NOT A
  , wTest :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "50-02 Fuzzy set operations" world0)
  { skBack  = hsb 250 18 14
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 4 6.5 0 0 0 []

muA, muB :: World -> Double -> Double
muA w x = trimf x (wPA w - wA) (wPA w) (wPA w + wA)
muB w x = trimf x (wPB w - wB) (wPB w) (wPB w + wB)

tnF, snF :: World -> Double -> Double -> Double
tnF w = snd (getAt ("", min) tNorms (wTn w))
snF w = snd (getAt ("", max) sNorms (wSn w))

tnName, snName :: World -> String
tnName w = fst (getAt ("", min) tNorms (wTn w))
snName w = fst (getAt ("", max) sNorms (wSn w))

opAt :: World -> Double -> Double
opAt w x = case wView w of
  0 -> tnF w a b
  1 -> snF w a b
  _ -> fnot a
  where
    a = muA w x
    b = muB w x

-- 1 本の曲線：(関数, 色, 下を塗るか)
data Curve = Curve (Double -> Double) Color Bool

draw :: Env -> World -> Picture
draw env w = view env $
  [ txt 20 34 17 (hsb 0 0 95) ("Drag: move A, B    1-3: AND = " ++ tnName w ++ "    4-6: OR = " ++ snName w)
  , txt 20 58 16 (hsb 0 0 95) ("Space: top plot = " ++ getAt "" views (wView w))
  , txt 20 82 15 (hsb 0 0 70) "Same two sets, different AND / OR (t-norm / s-norm), different shapes. Bottom: A with NOT A"
    -- ---- 上：A・B と演算の結果
  , plotBox gx 116 gw (envH env * 0.36)
      [ Curve (opAt w) (hsb 260 30 90) True, Curve (muA w) (hsb 20 45 100) False, Curve (muB w) (hsb 175 60 80) False ]
      (getAt "" views (wView w) ++ "  (" ++ (case wView w of { 0 -> tnName w; 1 -> snName w; _ -> "1 - mu" }) ++ ")")
    -- ---- 下：A ∧ ¬A と A ∨ ¬A
  , plotBox gx y2 gw h2
      [ Curve andNot (hsb 0 70 100) True, Curve orNot (hsb 175 60 80) False, Curve (muA w) (hsba 20 45 100 0.5) False ]
      "A AND (NOT A) = red,  A OR (NOT A) = teal" ]
  -- ---- 右：数値の表
  ++ [ txt tx ty 18 (hsb 0 0 95) ("mouse x = " ++ fmt 2 x) ]
  ++ concat [ [ txt tx yy 15 (if sel then hsb 260 30 90 else hsb 0 0 70) nm
              , txt (tx + 170) yy 15 (hsb 0 0 95) (fmt 3 v)
              , Color (if sel then hsb 260 30 90 else hsb 0 0 50) (rectTL (tx + 228) (yy - 12) (realToFrac v * 100) 12) ]
            | (i, (nm, v, sel)) <- zip [0 :: Int ..] rows, let yy = ty + 30 + fromIntegral i * 28 ]
  ++ [ txt tx yy0 16 (hsb 0 0 95) ("max (A AND NOT A) = " ++ fmt 3 mx)
     , txt tx (yy0 + 28) 16 (hsb 0 0 95) ("min (A OR NOT A) = " ++ fmt 3 mn) ]
  ++ zipWith (\k s -> txt tx (yy0 + 60 + k * 20) 14 (hsb 0 0 70) s) [0 ..]
       ((if mx > 1e-9 then ["Crisp sets would give 0 ->", "fuzzy sets keep a little contradiction"]
                      else ["Lukasiewicz AND: A AND NOT A = empty!"])
        ++ [""] ++
        (if mn < 1 - 1e-9 then ["Crisp sets would give 1 ->", "the excluded middle fails"]
                          else ["This OR gives A OR NOT A = everything", "(the excluded middle holds)"]))
  ++ [ txt 20 (envH env - 20) 14 (if all snd (wTest w) then hsb 160 50 90 else hsb 0 80 100) (selfTestLine (wTest w)) ]
  where
    gx = 70
    gw = envW env * 0.6
    y2 = 116 + envH env * 0.36 + 56
    h2 = envH env * 0.28
    andNot x = tnF w (muA w x) (fnot (muA w x))
    orNot x = snF w (muA w x) (fnot (muA w x))
    grid = [xLo + (xHi - xLo) * fromIntegral i / fromIntegral nX | i <- [0 .. nX]]
    mx = foldl' max 0 (map andNot grid)
    mn = foldl' min 1 (map orNot grid)
    tx = gx + gw + 50
    ty = 130
    (mX, _) = p5Mouse env
    x = clampv xLo xHi (xLo + realToFrac ((mX - gx) / gw) * (xHi - xLo))
    a = muA w x
    b = muB w x
    rows = [("muA", a, False), ("muB", b, False), ("NOT A: 1 - muA", fnot a, False)]
           ++ [("AND " ++ nm, f a b, i == wTn w) | (i, (nm, f)) <- zip [0 ..] tNorms]
           ++ [("OR " ++ nm, f a b, i == wSn w) | (i, (nm, f)) <- zip [0 ..] sNorms]
    yy0 = ty + 30 + fromIntegral (length rows) * 28 + 20

-- グラフ 1 枚（塗る曲線は細い縦の台形を並べて塗る）
plotBox :: Float -> Float -> Float -> Float -> [Curve] -> String -> Picture
plotBox gx gy gw gh curves title = Pictures $
  [ Color (hsb 0 0 40) (Line [(gx, yOf 0), (gx + gw, yOf 0)])
  , Color (hsb 0 0 40) (Line [(gx, gy), (gx, yOf 0)]) ]
  ++ [ txt (xOf x - 5) (yOf 0 + 18) 14 (hsb 0 0 60) (show (round x :: Int)) | x <- [xLo .. xHi] ]
  ++ [ txt (gx - 18) (yOf 1 + 5) 14 (hsb 0 0 60) "1", txt (gx - 18) (yOf 0 + 5) 14 (hsb 0 0 60) "0"
     , txt (gx + 8) (gy - 8) 14 (hsb 0 0 85) title ]
  ++ concatMap curve curves
  where
    xOf x = gx + realToFrac ((x - xLo) / (xHi - xLo)) * gw
    yOf m = gy + gh - realToFrac m * gh
    xs = [xLo + (xHi - xLo) * fromIntegral i / 300 | i <- [0 .. 300 :: Int]]
    curve (Curve f col fillIt) =
      let pts = [(xOf x, yOf (f x)) | x <- xs]
      in [ Color (withA 0.3 col) (Pictures [Polygon [(x0, yOf 0), p0, p1, (x1, yOf 0)] | (p0@(x0, _), p1@(x1, _)) <- zip pts (drop 1 pts)]) | fillIt ]
         ++ [ Color col (thickPath (if fillIt then 2 else 3) pts) ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w        -- p5.js 版と同じく、押しただけでは動かない
  EventMotion _ | envDown env -> dragTo
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> w { wTn = fromEnum c - fromEnum '1' }
                             | c >= '4' && c <= '6' -> w { wSn = fromEnum c - fromEnum '4' }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wView = (wView w + 1) `mod` 3 }
  _ -> w
  where
    (mx, _) = p5Mouse env
    gx = 70
    gw = envW env * 0.6
    x = clampv 1 9 (xLo + realToFrac ((mx - gx) / gw) * (xHi - xLo))
    dragTo
      | mx < gx || mx > gx + gw = w
      | abs (x - wPA w) <= abs (x - wPB w) = w { wPA = x }
      | otherwise = w { wPB = x }

-- ---- 自己テスト：手で計算できる値と比べる
selfTest :: [(String, Bool)]
selfTest =
  [ ("max_x min(muA, 1 - muA) = 0.5 (triangle (2, 5, 8))", nearTol 1e-6 (foldl' max (-1 / 0) [min a (1 - a) | a <- as]) 0.5)
  , ("min_x max(muA, 1 - muA) = 0.5 (excluded middle fails)", nearTol 1e-6 (foldl' min (1 / 0) [max a (1 - a) | a <- as]) 0.5)
  , ("Lukasiewicz: A AND NOT A = 0, bounded sum: A OR NOT A = 1 (every x)", all (\a -> near (t 2 a (1 - a)) 0 && near (s 2 a (1 - a)) 1) as)
  , ("De Morgan: NOT(A AND B) = NOT A OR NOT B (min/max, product/prob. sum)",
      and [near (1 - min a b) (max (1 - a) (1 - b)) && near (1 - a * b) (s 1 (1 - a) (1 - b)) | (a, b) <- zip as bs])
  , ("every t-norm <= min, every s-norm >= max",
      and [all (\(_, f) -> f a b <= min a b + 1e-12) tNorms && all (\(_, f) -> f a b >= max a b - 1e-12) sNorms | (a, b) <- zip as bs])
  , ("by hand: a = 0.6, b = 0.7 -> min 0.6, product 0.42, Lukasiewicz 0.3, prob. sum 0.88",
      near (t 1 0.6 0.7) 0.42 && near (t 2 0.6 0.7) 0.3 && near (s 1 0.6 0.7) 0.88 && t 0 0.6 0.7 == 0.6)
  , ("boundary: T(a, 1) = a, S(a, 0) = a", and [all (\(_, f) -> near (f a 1) a) tNorms && all (\(_, f) -> near (f a 0) a) sNorms | a <- [0, 0.3, 1]]) ]
  where
    grid f = [f (xLo + (xHi - xLo) * fromIntegral i / fromIntegral nX) | i <- [0 .. nX]]
    as = grid (\x -> trimf x 2 5 8)
    bs = grid (\x -> trimf x 4 6 8)
    t k = snd (getAt ("", min) tNorms k)
    s k = snd (getAt ("", max) sNorms k)

-- ---------- やってみよう ----------
-- 1. A と B を離すと AND（min）は 0 に、OR は 2 つの山になる。重ねるとどうなる？
-- 2. 1〜3 で AND を切りかえ、積はいつも min 以下、ウカシェヴィチはさらに小さいことを確かめよう
-- 3. 下の図：min / max では A ∧ ¬A の最大が 0.5。ちょうど μA = 0.5 の点で起きる
-- 4. 3（ウカシェヴィチ）と 6（有界和）を選ぶと、矛盾律も排中律も成り立つ！ そのかわり何が失われる？
-- 5. 三角の幅 wA・wB を変えて、「ほぼ同じ」2 つの集合の AND を見てみよう

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

-- ---- 演算：t ノルム（かつ）・s ノルム（または）・補集合（でない）
tNorms :: [(String, Double -> Double -> Double)]
tNorms = [("min", min), ("prod", (*)), ("Lukasiewicz", \a b -> max 0 (a + b - 1))]

sNorms :: [(String, Double -> Double -> Double)]
sNorms = [("max", max), ("prob sum", \a b -> a + b - a * b), ("bounded", \a b -> min 1 (a + b))]

fnot :: Double -> Double
fnot a = 1 - a

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

