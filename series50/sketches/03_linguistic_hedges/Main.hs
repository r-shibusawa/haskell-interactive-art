-- ============================================
-- 50-03 言語変数とヘッジ ─「とても暑い」「やや寒い」
-- 言語変数「気温」の値は 寒い・快適・暑い という言葉（それぞれファジィ集合）
-- ヘッジ（言葉の強め・弱め）は μ の計算：とても = μ²（集中）、やや = √μ（拡張）、〜ではない = 1 − μ
-- 操作：←→ = ヘッジ / ↑↓ = 言葉 / N = 「ない」の位置 / マウス左右 = 今の気温
-- ============================================
module Main (main) where

import Art
import System.Environment (lookupEnv)
import Numeric (showFFloat)

xLo, xHi :: Double
xLo = 0
xHi = 40

-- ★ 気温の言葉（台形・三角の形と位置を変えてみよう）：(名前, 形, 色の HSB)
terms :: [(String, MF, (Float, Float, Float))]
terms =
  [ ("cold", Trap 0 0 8 16, (200, 50, 95))
  , ("pleasant", Tri 14 22 30, (120, 40, 85))
  , ("hot", Trap 24 32 40 40, (20, 60, 100)) ]

-- ★ ヘッジ：μ を何乗するか（とても = 2、やや = 0.5）
hedges :: [(String, Double -> Double)]
hedges =
  [ ("(none)", id)
  , ("very", \m -> m * m)
  , ("somewhat", sqrt)
  , ("very very", \m -> m ** 4)
  , ("really", \m -> if m <= 0.5 then 2 * m * m else 1 - 2 * (1 - m) * (1 - m)) ]   -- 強調（INT）：0.5 より上は上げ、下は下げる

negs :: [String]
negs = ["", "NOT (very hot) type", "very (NOT hot) type"]

data World = World
  { wHi   :: !Int     -- ヘッジ
  , wTi   :: !Int     -- 言葉
  , wNi   :: !Int     -- 「ない」の位置
  , wTest :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "50-03 Linguistic variables and hedges" world0)
  { skBack  = hsb 250 18 14
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 2 0 []

termAt :: Int -> (String, MF, (Float, Float, Float))
termAt = getAt ("", Tri 0 1 2, (0, 0, 0)) terms

hedgeAt :: Int -> (String, Double -> Double)
hedgeAt = getAt ("", id) hedges

-- 文の所属度：n = 0 そのまま / 1 = 1 − hedge(μ)（「とても暑い」ではない）/ 2 = hedge(1 − μ)（とても「暑いではない」）
phraseMu :: Int -> Int -> Int -> Double -> Double
phraseMu h t n x = case n of
  1 -> fnot (f m)
  2 -> f (fnot m)
  _ -> clampv 0 1 (f m)
  where
    (_, mf, _) = termAt t
    m = evalMF mf x
    f = snd (hedgeAt h)

phraseText :: Int -> Int -> Int -> String
phraseText h t n = case n of
  1 -> "NOT (" ++ hd ++ nm ++ ")"
  2 -> hd ++ "(NOT " ++ nm ++ ")"
  _ -> hd ++ nm
  where
    hd = if h == 0 then "" else fst (hedgeAt h) ++ " "
    (nm, _, _) = termAt t

termCol :: Float -> (Float, Float, Float) -> Color
termCol a (h, s, b) = hsba h s b a

draw :: Env -> World -> Picture
draw env w = view env $
  [ txt 20 34 17 (hsb 0 0 95) ("Left/Right: hedge = " ++ fst (hedgeAt (wHi w)) ++ "    Up/Down: word = " ++ tName ++ "    N: NOT = " ++ (if null neg then "none" else neg))
  , txt 20 58 16 (hsb 0 0 95) "Mouse left/right: temperature"
  , txt 20 82 15 (hsb 0 0 70) "very = mu^2 (concentrate), somewhat = sqrt mu (dilate), NOT = 1 - mu.  NOT (very hot) and very (NOT hot) differ"
  , Color (hsb 0 0 40) (Line [(gx, yOf 0), (gx + gw, yOf 0)])
  , Color (hsb 0 0 40) (Line [(gx, gy), (gx, yOf 0)]) ]
  ++ [ txt (xOf x - 12) (yOf 0 + 18) 14 (hsb 0 0 60) (show (round x :: Int) ++ "C") | x <- [xLo, xLo + 5 .. xHi] ]
  ++ [ txt (gx - 18) (yOf 1 + 5) 14 (hsb 0 0 60) "1", txt (gx - 18) (yOf 0 + 5) 14 (hsb 0 0 60) "0" ]
  -- 3 つの言葉（うすく）
  ++ concat [ [ Color (termCol (if k == wTi w then 0.7 else 0.3) col) (thickPath (if k == wTi w then 3 else 2) (curve (evalMF mf)))
              , txt (xOf (mid mf) - 14) (gy - 10) 14 (termCol 0.8 col) nm ]
            | (k, (nm, mf, col)) <- zip [0 ..] terms ]
  -- 組み立てた文の関数
  ++ [ Color (hsba 260 30 90 0.22) (Pictures [Polygon [(x0, yOf 0), p0, p1, (x1, yOf 0)] | (p0@(x0, _), p1@(x1, _)) <- zip ph (drop 1 ph)])
     , Color (hsb 260 30 90) (thickPath 4 ph)
       -- 今の気温
     , Color (hsba 0 0 100 0.5) (Line [(xOf temp, gy), (xOf temp, yOf 0)])
     , Color (hsb 0 0 100) (at (xOf temp, yOf m) (disc 6))
     , txt gx 124 24 (hsb 0 0 95) (fmt 1 temp ++ "C is \"" ++ phraseText (wHi w) (wTi w) (wNi w) ++ "\" to degree " ++ fmt 3 m)
       -- ---- 右：言葉 × ヘッジ の表（今の気温で）
     , txt tx (ty - 20) 15 (hsb 0 0 80) ("Membership at " ++ fmt 1 temp ++ "C") ]
  ++ [ txt (tx + 100 + fromIntegral k * colW) (ty + 6) 15 (termCol 1 col) nm | (k, (nm, _, col)) <- zip [0 :: Int ..] terms ]
  ++ concat [ [ txt tx (yy + 22) 14 (hsb 0 0 75) hn ]
              ++ concat [ [ Color (hsb 260 30 (20 + 70 * realToFrac v)) (rectTL xx yy (colW - 8) (ch - 6)) ]
                          ++ [ Color (hsb 0 0 100) (thickPath 2 [(xx, yy), (xx + colW - 8, yy), (xx + colW - 8, yy + ch - 6), (xx, yy + ch - 6), (xx, yy)]) | h == wHi w && k == wTi w ]
                          ++ [ txt (xx + 8) (yy + 20) 14 (hsb 0 0 (if v > 0.6 then 5 else 95)) (fmt 2 v) ]
                        | k <- [0 .. length terms - 1], let v = phraseMu h k (wNi w) temp, let xx = tx + 100 + fromIntegral k * colW ]
            | (h, (hn, _)) <- zip [0 ..] hedges, let yy = ty + 20 + fromIntegral h * ch ]
  ++ [ txt tx yb 14 (hsb 0 0 70) ("NOT in the table: " ++ (if null neg then "none (N to switch)" else neg))
     , txt tx (yb + 26) 14 (hsb 0 0 70) ("original mu = " ++ fmt 3 m0 ++ "   1 - mu = " ++ fmt 3 (1 - m0))
     , txt tx (yb + 48) 14 (hsb 0 0 70) ("mu^2 = " ++ fmt 3 (m0 * m0) ++ "   sqrt mu = " ++ fmt 3 (sqrt m0))
     , txt 20 (envH env - 20) 14 (if all snd (wTest w) then hsb 160 50 90 else hsb 0 80 100) (selfTestLine (wTest w)) ]
  where
    gx = 70
    gy = 150
    gw = envW env * 0.6
    gh = envH env * 0.55
    xOf x = gx + realToFrac ((x - xLo) / (xHi - xLo)) * gw
    yOf v = gy + gh - realToFrac v * gh
    curve f = [ (xOf x, yOf (f x)) | i <- [0 .. 200 :: Int], let x = xLo + (xHi - xLo) * fromIntegral i / 200 ]
    (mx, _) = p5Mouse env
    temp = clampv xLo xHi (xLo + realToFrac ((mx - gx) / gw) * (xHi - xLo))
    ph = curve (phraseMu (wHi w) (wTi w) (wNi w))
    m = phraseMu (wHi w) (wTi w) (wNi w) temp
    (tName, tMf, _) = termAt (wTi w)
    neg = getAt "" negs (wNi w)
    mid mf = case mf of
      Tri a _ c -> (a + c) / 2
      Trap a _ _ d -> (a + d) / 2
      _ -> 0
    tx = gx + gw + 40
    ty = 150
    ch = 34
    colW = (envW env - tx - 120) / 3
    yb = ty + 40 + fromIntegral (length hedges) * ch
    m0 = evalMF tMf temp

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wHi = (wHi w + 1) `mod` nh }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wHi = (wHi w + nh - 1) `mod` nh }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wTi = (wTi w + 1) `mod` nt }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wTi = (wTi w + nt - 1) `mod` nt }
  EventKey (Char c) Down _ _ | c == 'n' || c == 'N' -> w { wNi = (wNi w + 1) `mod` length negs }
  _ -> w
  where
    nh = length hedges
    nt = length terms

-- ---- 自己テスト：手で計算できる値と比べる
selfTest :: [(String, Bool)]
selfTest =
  [ ("very 0.5 -> 0.25", near (hf 1 0.5) 0.25)
  , ("somewhat 0.25 -> 0.5", near (hf 2 0.25) 0.5)
  , ("NOT 0.3 -> 0.7", near (fnot 0.3) 0.7)
  , ("NOT (very hot) /= very (NOT hot) (mu = 0.5 gives 0.75 and 0.25)", near (phraseMu 1 2 1 28) 0.75 && near (phraseMu 1 2 2 28) 0.25)
  , ("hot(32C) = 1, cold(12C) = 0.5, pleasant(18C) = 0.5", tm 2 32 == 1 && near (tm 0 12) 0.5 && near (tm 1 18) 0.5)
  , ("everywhere: very <= original <= somewhat",
      and [ m * m <= m + 1e-12 && m <= sqrt m + 1e-12 | i <- [0 .. 160 :: Int], let x = 0.25 * fromIntegral i, k <- [0 .. 2], let m = tm k x ])
  , ("hedges keep 0 and 1 fixed", all (\h -> hf h 0 == 0 && hf h 1 == 1) [1, 2, 3]) ]
  where
    hf h = snd (hedgeAt h)
    tm k x = let (_, mf, _) = termAt k in evalMF mf x

-- ---------- やってみよう ----------
-- 1. 「とても暑い」と「やや暑い」で、所属度 0.5 になる気温はそれぞれ何℃？
-- 2. N で「『とても暑い』ではない」と「とても『暑いではない』」を比べ、グラフの違いを説明しよう
-- 3. 「とてもとても」（μ⁴）を何回も重ねると、どんな形に近づく？
-- 4. 「本当に」（強調 INT）は 0.5 を境に、上は 1 へ・下は 0 へ寄せる。何回も重ねるとクリスプ集合に近づく
-- 5. terms に「ぬるい」「蒸し暑い」を足して、自分の言語変数を作ろう

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

