-- ============================================
-- 50-05 拡張原理とファジィ数 ─「約 3」+「約 5」= ?
-- 三角ファジィ数 (a, b, c)：b で 1、a と c で 0 の「だいたい b」。計算は α カット（区間）ごとに区間演算でする
--   [p1, p2] + [q1, q2] = [p1 + q1, p2 + q2]、× は 4 つの積の最小・最大、÷ は 0 をふくむと計算できない
-- 点は拡張原理 μ(z) = sup_{x ∘ y = z} min(μA(x), μB(y)) を総当たりで計算した値（α カットの線と重なる）
-- 操作：1〜4 = + − × ÷ / ○ をドラッグ = A・B の 3 つの点 / ↑↓ = 表示する α
-- ============================================
module Main (main) where

import Art
import Data.Array (accumArray, elems)
import System.Environment (lookupEnv)
import Numeric (showFFloat)

nAlpha :: Int
nAlpha = 20                -- ★ α カットを何段に分けるか

xLo, xHi :: Double
xLo = 0
xHi = 10

type Tri3 = (Double, Double, Double)

data World = World
  { wA     :: !Tri3                        -- ★ 約 3（最初は (2, 3, 4)）
  , wB     :: !Tri3                        -- ★ 約 5（最初は (4, 5, 6)）
  , wOp    :: !Int
  , wAlpha :: !Double
  , wRes   :: [Maybe (Double, Double, Double)]   -- α カットの答え (α, 左, 右)。割れないときは Nothing
  , wBrute :: [(Double, Double)]           -- 拡張原理の総当たり (z, μ)
  , wZLo   :: !Double
  , wZHi   :: !Double
  , wDrag  :: Maybe (Int, Int)             -- ドラッグ中の点（0 = A / 1 = B、何番目の点か）
  , wTest  :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "50-05 Extension principle and fuzzy numbers" (recompute world0))
  { skBack  = hsb 250 18 14
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (recompute world0) { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World (2, 3, 4) (4, 5, 6) 0 0.5 [] [] 0 1 Nothing []

opAt :: Int -> (String, (Double, Double) -> (Double, Double) -> Maybe (Double, Double))
opAt = getAt ("+", \_ _ -> Nothing) ivOps

-- α カットを全部計算＋拡張原理の総当たり
recompute :: World -> World
recompute w = w { wRes = res, wBrute = extensionBrute (wA w) (wB w) nm zl zh 120, wZLo = zl, wZHi = zh }
  where
    (nm, f) = opAt (wOp w)
    res = [ fmap (\(l, r) -> (al, l, r)) (f (cutTri (wA w) al) (cutTri (wB w) al))
          | k <- [0 .. nAlpha], let al = fromIntegral k / fromIntegral nAlpha ]
    (zl, zh) = case res of
      (Just (_, l, r) : _) -> let pad = max 0.5 ((r - l) * 0.08) in (l - pad, r + pad)
      _ -> (wZLo w, wZHi w)

-- 上のグラフの枠（p5.js は gy = 100。説明を 2 行にしたので少し下げた）
gX, gY :: Float
gX = 70
gY = 116

gWOf, gHOf :: Env -> Float
gWOf env = envW env * 0.6
gHOf env = envH env * 0.28

draw :: Env -> World -> Picture
draw env w = view env $
  [ txt 20 30 17 (hsb 0 0 95) ("1-4: operation = " ++ unwords [if i == wOp w then "[" ++ n ++ "]" else n | (i, (n, _)) <- zip [0 ..] ivOps]
                               ++ "    Drag o: shapes of A, B    Up/Down: alpha = " ++ fmt 2 (wAlpha w))
  , txt 20 54 15 (hsb 0 0 70) "Compute alpha-cuts (mu >= alpha) with interval arithmetic and stack them at height alpha: that is the answer."
  , txt 20 74 15 (hsb 0 0 70) "Dots = extension principle computed by brute force"
  , axes gx gy gw gh xLo xHi ]
  -- ---- 上：A と B
  ++ concat [ [ Color col (thickPath 3 [(xOf a, yOf 0), (xOf b, yOf 1), (xOf c, yOf 0)])
              , let (c0, c1) = cutTri t (wAlpha w) in Color col (thickLine 6 (xOf c0, yOf (wAlpha w)) (xOf c1, yOf (wAlpha w))) ]
              ++ concat [ [Color (hsb 250 18 14) (at p (disc 8)), Color (hsb 0 0 100) (at p (ring 2 8))]
                        | (i, v) <- zip [0 :: Int ..] [a, b, c], let p = (xOf v, yOf (if i == 1 then 1 else 0)) ]
              ++ [ txt (xOf b - 40) (gy - 8) 16 col (nm ++ " = (" ++ fmt 1 a ++ ", " ++ fmt 1 b ++ ", " ++ fmt 1 c ++ ")") ]
            | (t@(a, b, c), col, nm) <- [(wA w, hsb 20 45 100, "A"), (wB w, hsb 175 60 80, "B")] ]
  -- ---- 下：答え A ∘ B
  ++ (case [r | Just r <- res] of
        _ | not ok0 -> [ txt gx (y2 + 60) 22 (hsb 0 70 100) "The support of B contains 0: cannot divide (an interval divides by 0)" ]
        rs ->
          let left = [(zOf l, y2Of al) | (al, l, _) <- rs]
              right = [(zOf r, y2Of al) | (al, _, r) <- rs]
              (_, l0, r0) = head' rs
              (_, lc, _) = last rs
          in [ axes gx y2 gw h2 (wZLo w) (wZHi w)
             , Color (hsba 260 30 90 0.22) (Pictures [Polygon [p0, p1, q1, q0] | ((p0, q0), (p1, q1)) <- zip (zip left right) (drop 1 (zip left right))])
             , Color (hsb 260 30 90) (thickPath 3 (left ++ reverse right))
               -- 三角で近似した形（× ÷ では本当の答えは三角にならない）
             , Color (hsba 0 0 100 0.4) (Line [(zOf l0, y2Of 0), (zOf lc, y2Of 1), (zOf r0, y2Of 0)])
               -- 拡張原理の総当たり（点）
             , Color (hsb 20 45 100) (Pictures [at (zOf z, y2Of m) (disc 2) | (z, m) <- wBrute w, m > 0.001]) ]
             ++ [ Color (hsb 260 30 90) (thickLine 6 (zOf c0, y2Of (wAlpha w)) (zOf c1, y2Of (wAlpha w)))
                | Just (c0, c1) <- [opF (cutTri (wA w) (wAlpha w)) (cutTri (wB w) (wAlpha w))] ]
             ++ [ txt gx (y2 - 30) 15 (hsb 0 0 85) ("A " ++ opName ++ " B   purple: alpha-cut interval arithmetic   orange dots: extension principle")
                , txt gx (y2 - 10) 15 (hsb 0 0 85) "thin line: triangle through the 3 points" ])
  -- ---- 右：α カットの表
  ++ [ txt tx ty 17 (hsb 0 0 95) "Alpha-cut interval arithmetic" ]
  ++ concat [ [ txt tx yy 14 (if i == 5 then hsb 260 30 90 else hsb 0 0 70) ("alpha = " ++ fmt 2 al ++ (if i == 5 then "  (Up/Down)" else ""))
              , txt tx (yy + 20) 14 (hsb 0 0 95) (ivS a ++ " " ++ opName ++ " " ++ ivS b)
              , txt (tx + 10) (yy + 38) 14 (hsb 260 30 90) ("= " ++ maybe "cannot compute" ivS (opF a b)) ]
            | (i, al) <- zip [0 :: Int ..] [0, 0.25, 0.5, 0.75, 1, wAlpha w]
            , let a = cutTri (wA w) al, let b = cutTri (wB w) al, let yy = ty + 34 + fromIntegral i * 58 ]
  ++ (case (res, last' res) of
        (Just (_, l, r) : _, Just (Just (_, lc, _))) ->
          [ txt tx (ty + 34 + 6 * 58 + 10) 14 (hsb 0 0 70) ("answer: support = " ++ ivS (l, r))
          , txt tx (ty + 34 + 6 * 58 + 30) 14 (hsb 0 0 70) ("core = " ++ fmt 2 lc) ]
        _ -> [])
  ++ [ txt 20 (envH env - 20) 14 (if all snd (wTest w) then hsb 160 50 90 else hsb 0 80 100) (selfTestLine (wTest w)) ]
  where
    gx = gX
    gy = gY
    gw = gWOf env
    gh = gHOf env
    xOf x = gx + realToFrac ((x - xLo) / (xHi - xLo)) * gw
    yOf m = gy + gh - realToFrac m * gh
    y2 = gy + gh + 70
    h2 = envH env * 0.36
    zOf z = gx + realToFrac ((z - wZLo w) / (wZHi w - wZLo w)) * gw
    y2Of m = y2 + h2 - realToFrac m * h2
    res = wRes w
    ok0 = case res of { (Just _ : _) -> True; _ -> False }
    (opName, opF) = opAt (wOp w)
    tx = gx + gw + 40
    ty = 120
    head' rs = case rs of { (r : _) -> r; [] -> (0, 0, 0) }
    last' xs = if null xs then Nothing else Just (last xs)

ivS :: (Double, Double) -> String
ivS (a, b) = "[" ++ fmt 2 a ++ ", " ++ fmt 2 b ++ "]"

-- 軸と目盛り（目盛りの間隔は 1・2・5 × 10^n から選ぶ）
axes :: Float -> Float -> Float -> Float -> Double -> Double -> Picture
axes gx gy gw gh lo hi = Pictures $
  [ Color (hsb 0 0 40) (Line [(gx, gy + gh), (gx + gw, gy + gh)])
  , Color (hsb 0 0 40) (Line [(gx, gy), (gx, gy + gh)]) ]
  ++ [ txt (gx + realToFrac ((v - lo) / (hi - lo)) * gw - 6) (gy + gh + 18) 14 (hsb 0 0 60) (shortNum v)
     | k <- [0 .. 40 :: Int], let v = fromIntegral (ceiling (lo / st) :: Int) * st + fromIntegral k * st, v <= hi + 1e-9 ]
  ++ [ txt (gx - 18) (gy + 5) 14 (hsb 0 0 60) "1", txt (gx - 18) (gy + gh + 5) 14 (hsb 0 0 60) "0" ]
  where
    raw = (hi - lo) / 8
    mag = 10 ^^ (floor (logBase 10 (max 1e-6 raw)) :: Int)
    st | raw / mag < 2 = 2 * mag
       | raw / mag < 5 = 5 * mag
       | otherwise = 10 * mag

-- 小数点以下 2 桁まで、余分な 0 を消して書く（p5.js の +v.toFixed(2)）
shortNum :: Double -> String
shortNum v = let s = fmt 2 v
                 t = reverse (dropWhile (== '0') (reverse s))
                 u = if take 1 (reverse t) == "." then init t else t
             in if u == "-0" then "0" else u

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w { wDrag = pick }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = Nothing }
  EventMotion _ | envDown env, Just (s, i) <- wDrag w -> recompute (if s == 0 then w { wA = move i (wA w) } else w { wB = move i (wB w) })
  EventKey (Char c) Down _ _ | c >= '1' && c <= '4' -> recompute w { wOp = fromEnum c - fromEnum '1' }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wAlpha = clampv 0 1 (wAlpha w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wAlpha = clampv 0 1 (wAlpha w - 0.05) }
  _ -> w
  where
    (mx, my) = p5Mouse env
    gw = gWOf env
    gh = gHOf env
    cands = [ (d, (s, i)) | (s, (a, b, c)) <- zip [0 ..] [wA w, wB w], (i, v) <- zip [0 ..] [a, b, c]
            , let d = sqrt ((mx - (gX + realToFrac ((v - xLo) / (xHi - xLo)) * gw)) ^ (2 :: Int) + (my - (gY + gh - (if i == 1 then gh else 0))) ^ (2 :: Int))
            , d < 30 ]
    pick = case cands of
      [] -> Nothing
      _ -> Just (snd (minimum cands))
    x = clampv xLo xHi (fromIntegral (jsRound ((xLo + realToFrac ((mx - gX) / gw) * (xHi - xLo)) * 10)) / 10)
    move i (a, b, c) = case i of
      0 -> (min x b, b, c)
      2 -> (a, b, max x b)
      _ -> (a, clampv a c x, c)

-- ---- 自己テスト：手で計算できる値と比べる
selfTest :: [(String, Bool)]
selfTest =
  [ ("(2, 3, 4) + (4, 5, 6) = (6, 8, 10): [6, 10] at alpha = 0, [8, 8] at alpha = 1", eq (op 0 0) (6, 10) && eq (op 0 1) (8, 8))
  , ("0.5-cut of the sum = [7, 9]", eq (op 0 0.5) (7, 9))
  , ("(2, 3, 4) - (4, 5, 6) = (-4, -2, 0)", eq (op 1 0) (-4, 0) && eq (op 1 1) (-2, -2))
  , ("x: [8, 24] at alpha = 0, 15 at alpha = 1, [11.25, 19.25] at alpha = 0.5 (not a triangle)", eq (op 2 0) (8, 24) && eq (op 2 1) (15, 15) && eq (op 2 0.5) (11.25, 19.25))
  , ("/: [2/6, 4/4] = [1/3, 1] at alpha = 0", eq (op 3 0) (1 / 3, 1))
  , ("an interval containing 0 cannot divide (null)", case snd (opAt 3) (1, 2) (-1, 1) of { Nothing -> True; _ -> False })
  , ("extension principle by brute force: mu(8) = 1, mu(7) = 0.5, mu(10) = 0 (sum)", nearTol 1e-6 (at 8) 1 && nearTol 1e-6 (at 7) 0.5 && nearTol 1e-6 (at 10) 0) ]
  where
    a0 = (2, 3, 4)
    b0 = (4, 5, 6)
    op k al = snd (opAt k) (cutTri a0 al) (cutTri b0 al)
    eq p (q0, q1) = case p of
      Just (p0, p1) -> near p0 q0 && near p1 q1
      Nothing -> False
    br = extensionBrute a0 b0 "+" 5 11 60
    at z = foldl (\m (bz, bm) -> if abs (bz - z) < 1e-6 then bm else m) (-1) br

-- ---------- やってみよう ----------
-- 1. 「約 3」+「約 5」は「約 8」。でも幅は広がる（あいまいさは足し算でたまる）
-- 2. 「約 3」−「約 3」は 0 にならない！ A を B と同じ形にして確かめよう
-- 3. × と ÷ では答えのふちが曲線になり、細線（三角の近似）とずれる。どこがいちばんずれる？
-- 4. B の左の点を 0 まで動かして ÷ を選ぶと、なぜ計算できなくなる？
-- 5. nAlpha を 2 や 4 に減らすと、α カットの段々がよく見える

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

-- ---- 拡張原理を総当たりで：x, y を 81 × 81 点ためし、z = x ∘ y の格子点ごとに min(μA(x), μB(y)) の最大をとる
extensionBrute :: (Double, Double, Double) -> (Double, Double, Double) -> String -> Double -> Double -> Int -> [(Double, Double)]
extensionBrute (a0, a1, a2) (b0, b1, b2) opName zLo zHi nz = zip zs (elems arr)
  where
    zs = [zLo + (zHi - zLo) * fromIntegral i / fromIntegral nz | i <- [0 .. nz]]
    dz = (zHi - zLo) / fromIntegral nz
    n = 80 :: Int
    arr = accumArray max 0 (0, nz)
      [ (k, m)
      | i <- [0 .. n], j <- [0 .. n]
      , let x = a0 + (a2 - a0) * fromIntegral i / fromIntegral n
            y = b0 + (b2 - b0) * fromIntegral j / fromIntegral n
            z = case opName of
                  "+" -> x + y
                  "-" -> x - y
                  "x" -> x * y
                  _ -> if y == 0 then 0 / 0 else x / y
      , not (isNaN z || isInfinite z)
      , let k = jsRound ((z - zLo) / dz)
      , k >= 0 && k <= nz
      , abs (z - (zLo + (zHi - zLo) * fromIntegral k / fromIntegral nz)) < dz * 0.25    -- z の格子点のすぐ近くだけ数える
      , let m = min (trimf x a0 a1 a2) (trimf y b0 b1 b2) ]

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

