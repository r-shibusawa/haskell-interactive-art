-- ============================================
-- 50-00 クリスプ集合とファジィ集合 ─「背が高い」
-- クリスプ（ふつうの）集合：しきい値より上なら 1、下なら 0。174.9 cm と 175.0 cm で天と地
-- ファジィ集合：所属度 μ を 0〜1 のなめらかな関数（シグモイド μ = 1 / (1 + e^(−a(x − c)))）で決める
-- 操作：グラフをドラッグ（または ←→）= しきい値 c / ↑↓ = 傾き a / スペース = 別の人たち / C = 色をクリスプ⇔ファジィ
-- ============================================
module Main (main) where

import Art
import Data.List (sort)
import System.Environment (lookupEnv)
import Numeric (showFFloat)

nPeople :: Int
nPeople = 16          -- ★ 並べる人数

xLo, xHi :: Double
xLo = 145             -- グラフの横軸の範囲 [cm]
xHi = 200

data World = World
  { wC0     :: !Double     -- ★ しきい値（シグモイドの真ん中）[cm]（最初は 175）
  , wSlope  :: !Double     -- ★ シグモイドの傾き a（大きいほどクリスプに近い。最初は 0.35）
  , wPeople :: [Double]    -- 身長（小さい順）
  , wCrisp  :: !Bool       -- 色をクリスプで塗るか
  , wTest   :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "50-00 Crisp vs fuzzy sets" world0)
  { skBack  = hsb 250 18 14
  , skStart = \_ -> do
      printSelfTest selfTest
      ps <- newPeople
      pure world0 { wPeople = ps, wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 175 0.35 [] False []

-- 152〜196 cm の人を nPeople 人（0.1 cm 単位）
newPeople :: IO [Double]
newPeople = do
  hs <- mapM (const (randomRIO (152, 196 :: Double))) [1 .. nPeople]
  pure (sort [fromIntegral (jsRound (h * 10)) / 10 | h <- hs])

crispTall :: Double -> Double -> Double
crispTall c0 h = if h >= c0 then 1 else 0          -- クリスプ：0 か 1

fuzzyTall :: World -> Double -> Double
fuzzyTall w h = sigmf h (wSlope w) (wC0 w)          -- ファジィ：0〜1

-- グラフの枠（p5.js は gy = 100。説明を 2 行にしたので少し下げた）
data Graph = Graph { gX, gY, gW, gH :: !Float }

graphOf :: Env -> Graph
graphOf env = Graph 70 116 (envW env - 140) (envH env * 0.3)

draw :: Env -> World -> Picture
draw env w = view env $
  [ txt 20 34 17 (hsb 0 0 95) ("Drag or Left/Right: threshold c = " ++ fmt 1 (wC0 w) ++ " cm    Up/Down: slope a = " ++ fmt 2 (wSlope w))
  , txt 20 58 16 (hsb 0 0 95) ("Space: new people    C: colors = " ++ (if wCrisp w then "crisp" else "fuzzy"))
  , txt 20 82 15 (hsb 0 0 70) "\"Tall\": split by a threshold into 0 / 1 (orange), or a smooth membership mu (teal)"
    -- 軸
  , Color (hsb 0 0 40) (Line [(gx, gy + gh), (gx + gw, gy + gh)])
  , Color (hsb 0 0 40) (Line [(gx, gy), (gx, gy + gh)]) ]
  ++ [ txt (xOf h - 14) (gy + gh + 18) 14 (hsb 0 0 60) (show (round h :: Int)) | h <- [150, 155 .. xHi] ]
  ++ [ txt (gx - 18) (yOf 1 + 5) 14 (hsb 0 0 60) "1", txt (gx - 34) (yOf 0.5 + 5) 14 (hsb 0 0 60) "0.5"
     , txt (gx - 18) (yOf 0 + 5) 14 (hsb 0 0 60) "0"
     , Color (hsb 0 0 30) (Line [(gx, yOf 0.5), (gx + gw, yOf 0.5)])
       -- クリスプ（階段）
     , Color (hsb 20 45 100) (thickPath 3 [(xOf xLo, yOf 0), (xOf (wC0 w), yOf 0), (xOf (wC0 w), yOf 1), (xOf xHi, yOf 1)])
       -- ファジィ（シグモイド）
     , Color (hsb 175 60 80) (thickPath 4 [ (xOf h, yOf (fuzzyTall w h))
                                          | i <- [0 .. 200 :: Int], let h = xLo + (xHi - xLo) * fromIntegral i / 200 ]) ]
     -- 人の位置
  ++ concat [ [ Color (hsba 0 0 100 0.4) (Line [(xOf h, gy + gh), (xOf h, yOf (fuzzyTall w h))])
              , Color (hsb 175 60 80) (at (xOf h, yOf (fuzzyTall w h)) (disc 4)) ]
            | h <- wPeople w ]
     -- 人のアイコン
  ++ concat [ person i h | (i, h) <- zip [0 :: Int ..] (wPeople w) ]
  ++ [ txt 14 (base + 20) 14 (hsb 0 0 70) "cm", txt 14 (base + 40) 14 (hsb 20 45 100) "crisp"
     , txt 14 (base + 60) 14 (hsb 175 60 80) "mu"
       -- 集計：クリスプの人数と、ファジィの「濃さの合計」（シグマ・カウント）
     , txt 70 (gy + gh + 46) 16 (hsb 0 0 95)
         ("Number of \"tall\" people   crisp: " ++ show (round sumC :: Int) ++ "   fuzzy (sum of mu): " ++ fmt 2 sumF)
     , txt 20 (envH env - 20) 14 (if all snd (wTest w) then hsb 160 50 90 else hsb 0 80 100) (selfTestLine (wTest w)) ]
  where
    Graph gx gy gw gh = graphOf env
    xOf h = gx + realToFrac ((h - xLo) / (xHi - xLo)) * gw
    yOf m = gy + gh - realToFrac m * gh
    n = length (wPeople w)
    cw = (envW env - 140) / fromIntegral (max 1 n)
    base = envH env - 120
    sc = envH env * 0.27 / 200
    sumC = sum [crispTall (wC0 w) h | h <- wPeople w]
    sumF = sum [fuzzyTall w h | h <- wPeople w]
    person i h =
      let mc = crispTall (wC0 w) h
          mf = fuzzyTall w h
          m = realToFrac (if wCrisp w then mc else mf)
          cx = 70 + cw * (fromIntegral i + 0.5)
          hh = realToFrac h * sc
          top = base - hh
          col = hsb (lerpF 260 20 m) (lerpF 15 60 m) (lerpF 45 100 m)
      in [ Color col (at (cx, top + 10) (disc 10))
         , Color col (rectTL (cx - 9) (top + 22) 18 (hh - 22))
         , txtCB cx (base + 20) 14 (hsb 0 0 95) (fmt 1 h)
         , txtCB cx (base + 40) 14 (hsb 20 45 100) (show (round mc :: Int))
         , txtCB cx (base + 60) 14 (hsb 175 60 80) (fmt 2 mf) ]

-- グラフの上（少し外も）をクリック・ドラッグするとしきい値が動く
setByMouse :: Env -> World -> World
setByMouse env w
  | my > gy - 20 && my < gy + gh + 30 = w { wC0 = clampv 150 195 (xLo + realToFrac ((mx - gx) / gw) * (xHi - xLo)) }
  | otherwise = w
  where
    Graph gx gy gw gh = graphOf env
    (mx, my) = p5Mouse env

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (setByMouse env w)
  EventMotion _ | envDown env -> pure (setByMouse env w)
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wC0 = clampv 150 195 (wC0 w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wC0 = clampv 150 195 (wC0 w + 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSlope = clampv 0.05 20 (wSlope w * 1.4) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSlope = clampv 0.05 20 (wSlope w / 1.4) }
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    ps <- newPeople
    pure w { wPeople = ps }
  EventKey (Char c) Down _ _ | c == 'c' || c == 'C' -> pure w { wCrisp = not (wCrisp w) }
  _ -> pure w

-- ---- 自己テスト：手で計算できる値と比べる
selfTest :: [(String, Bool)]
selfTest =
  [ ("sigmoid: mu = 0.5 at the center c", near (sigmf 175 0.35 175) 0.5)
  , ("crisp: 175.0 cm -> 1, 174.9 cm -> 0", crispTall 175 175 == 1 && crispTall 175 174.9 == 0)
  , ("symmetry mu(c + d) + mu(c - d) = 1 (d = 7)", near (sigmf 182 0.35 175 + sigmf 168 0.35 175) 1)
  , ("a = ln 3 / 2 gives mu(c + 2) = 0.75", near (sigmf 177 (log 3 / 2) 175) 0.75)
  , ("mu increases monotonically and stays in 0..1", mono && inside)
  , ("two people 170 cm and 180 cm: sum of mu = 1 (c = 175)", near (sigmf 170 0.35 175 + sigmf 180 0.35 175) 1)
  , ("a large slope (a = 50) is almost crisp", sigmf 175.5 50 175 > 0.999 && sigmf 174.5 50 175 < 0.001) ]
  where
    ms = [sigmf (140 + 0.5 * fromIntegral i) 0.35 175 | i <- [0 .. 140 :: Int]]   -- h = 140, 140.5, …, 210
    mono = and (zipWith (<=) ms (drop 1 ms))
    inside = all (\m -> m >= 0 && m <= 1) ms

-- ---------- やってみよう ----------
-- 1. ↑ を何回も押して傾き a を大きくすると、シグモイドはクリスプの階段にどんどん近づく
-- 2. しきい値ちょうど付近の 2 人（174 cm と 176 cm）で、クリスプとファジィの答えを比べよう
-- 3. 「背が高い人の数」：クリスプは整数、ファジィは Σμ（小数）。どちらが実感に近い？
-- 4. nPeople や world0 の c0 を変えて、男子・女子・子どもなど、集団ごとに「背が高い」の基準を考えよう
-- 5. fuzzyTall の sigmf を gaussmf にすると「ふつうの背」になる。どんな形にすればよいか考えよう

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
tNorms = [("min", min), ("product", (*)), ("Lukasiewicz", \a b -> max 0 (a + b - 1))]

sNorms :: [(String, Double -> Double -> Double)]
sNorms = [("max", max), ("probabilistic sum", \a b -> a + b - a * b), ("bounded sum", \a b -> min 1 (a + b))]

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

