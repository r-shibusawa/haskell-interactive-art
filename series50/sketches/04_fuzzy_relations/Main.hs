-- ============================================
-- 50-04 ファジィ関係と合成
-- ファジィ関係 R(x, y) =「x と y がどのくらい関係があるか」（0〜1 の行列）
-- max-min 合成：(R∘S)(x, z) = max_y min(R(x, y), S(y, z))。「x → y → z」の道のうち、いちばん太い道の細いところ
-- 操作：1 = 合成のアニメ / 2 = 「似ている」関係の推移閉包 / スペース = 1 歩 / A = 自動 / セルをクリック = +0.1 / ↑↓ = α
-- ============================================
module Main (main) where

import Art
import System.Environment (lookupEnv)
import Numeric (showFFloat)

type Mat = [[Double]]

speed :: Double
speed = 6            -- ★ 1 秒に何歩すすむか

-- ★ 合成する 2 つの関係（行 = x、列 = y）と（行 = y、列 = z）
r0, q0 :: Mat
r0 = [[0.7, 0.5, 0.2, 0.0], [0.8, 0.4, 0.9, 0.3], [0.1, 0.6, 0.3, 1.0]]
q0 = [[0.9, 0.6, 0.5], [0.1, 0.7, 0.5], [0.4, 0.3, 0.8], [0.6, 0.2, 0.7]]

-- ★ 「似ている」関係（反射的：自分とは 1、対称的：R(a, b) = R(b, a)）
colNames :: [String]
colNames = ["red", "orange", "yellow", "green", "blue"]

sim0 :: Mat
sim0 = [[1, 0.8, 0.3, 0, 0], [0.8, 1, 0.7, 0.1, 0], [0.3, 0.7, 1, 0.5, 0.1], [0, 0.1, 0.5, 1, 0.6], [0, 0, 0.1, 0.6, 1]]

data World = World
  { wPage    :: !Int
  , wAuto    :: !Bool
  , wTimer   :: !Double
  , wR       :: Mat
  , wQ       :: Mat
  , wT       :: [[Maybe Double]]   -- 合成の結果（まだのセルは Nothing）
  , wCi, wCk, wCj :: !Int          -- いま計算しているセル (x, z) と見ている y
  , wSim     :: Mat
  , wClosure :: [Mat]              -- くり返しの途中経過（最後が今）
  , wAlpha   :: !Double
  , wTest    :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "50-04 Fuzzy relations and composition" (restart world0))
  { skBack  = hsb 250 18 14
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (restart world0) { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 True 0 r0 q0 [] 0 0 0 sim0 [] 0.6 []

restart :: World -> World
restart w = w { wT = [[Nothing | _ <- getAt [] (wQ w) 0] | _ <- wR w], wCi = 0, wCk = 0, wCj = 0, wClosure = [wSim w] }

-- 合成のアニメを 1 歩：y（j）を 1 つ進め、全部見たら次のセルへ
stepComp :: World -> World
stepComp w
  | wCi w >= length (wR w) = restart w
  | cj < length (wQ w) = w { wCj = cj }
  | otherwise =
      let tt = setAt (wCi w) (setAt (wCk w) (Just (maxMinCell (wR w) (wQ w) (wCi w) (wCk w))) (getAt [] (wT w) (wCi w))) (wT w)
          ck = wCk w + 1
          nz = length (getAt [] (wQ w) 0)
      in if ck >= nz then w { wT = tt, wCj = 0, wCk = 0, wCi = wCi w + 1 } else w { wT = tt, wCj = 0, wCk = ck }
  where cj = wCj w + 1

stepClosure :: World -> World
stepClosure w = if sameM n c then w else w { wClosure = wClosure w ++ [n] }
  where
    c = lastClosure w
    n = maxOf c (maxMin c c)

lastClosure :: World -> Mat
lastClosure w = case wClosure w of
  [] -> wSim w
  cs -> last cs

step :: Env -> Float -> World -> IO World
step _ dt w
  | not (wAuto w) = pure w
  | t >= 1 = pure (if wPage w == 1 then stepComp w' else stepClosure w') { wTimer = 0 }
  | otherwise = pure w'
  where
    t = wTimer w + (if wPage w == 1 then speed else 0.7) * realToFrac dt
    w' = w { wTimer = t }

compCs :: Env -> Float
compCs env = minimum [70, envW env / 16, envH env / 12]

closCs :: Env -> Float
closCs env = min 64 (envW env / 18)

draw :: Env -> World -> Picture
draw env w = view env $
  [ txt 20 34 17 (hsb 0 0 95) ("1: composition   2: transitive closure (now " ++ show (wPage w) ++ ")   Space: 1 step   A: auto " ++ (if wAuto w then "ON" else "off"))
  , txt 20 58 16 (hsb 0 0 95) ("Click a cell: +0.1    Up/Down: alpha = " ++ fmt 1 (wAlpha w)) ]
  ++ zipWith (\k s -> txt 20 (82 + k * 20) 15 (hsb 0 0 70) s) [0 ..]
       (if wPage w == 1
          then ["(R o S)(x, z) = max_y min(R(x, y), S(y, z)): compare every path x -> z; take the thinnest point of the thickest path"]
          else [ "\"Similar\" is reflexive and symmetric, not always transitive (red-orange 0.8, orange-yellow 0.7, red-yellow 0.3)" ])
  ++ (if wPage w == 1 then drawComp env w else drawClosure env w)
  ++ [ txt 20 (envH env - 20) 14 (if all snd (wTest w) then hsb 160 50 90 else hsb 0 80 100) (selfTestLine (wTest w)) ]

drawComp :: Env -> World -> [Picture]
drawComp env w =
  [ drawMat (map (map Just) r) xR y0 cs "R (x -> y)" "x" "y" (\i j -> not done && i == ci && j <= cj) Nothing
  , drawMat (map (map Just) q) xS y0 cs "S (y -> z)" "y" "z" (\j k -> not done && k == ck && j <= cj) Nothing
  , drawMat (wT w) xT y0 cs "R o S (x -> z)" "x" "z" (\i k -> not done && i == ci && k == ck) Nothing ]
  ++ (if done
        then [ txt 80 by 18 (hsb 20 45 100) "Done! Space to run again" ]
        else [ txt bx by 18 (hsb 0 0 95) ("(R o S)(x" ++ show (ci + 1) ++ ", z" ++ show (ck + 1) ++ ") = max(") ]
             ++ concat [ [ txt (bx + 20) yy 16 (if seen then hsb 0 0 90 else hsb 0 0 40)
                                ("y" ++ show (j + 1) ++ ": min(" ++ fmt 1 a ++ ", " ++ fmt 1 b ++ ") = " ++ (if seen then fmt 1 m else "?")) ]
                         ++ [ Color (if m == best then hsb 20 45 100 else hsb 175 60 80) (rectTL (bx + 300) (yy - 14) (realToFrac m * 240) 16) | seen ]
                       | j <- [0 .. length q - 1]
                       , let a = getAt 0 (getAt [] r ci) j
                             b = getAt 0 (getAt [] q j) ck
                             m = min a b
                             seen = j <= cj
                             yy = by + 30 + fromIntegral j * 34 ]
             ++ [ txt bx (by + 30 + fromIntegral (length q) * 34 + 10) 18 (hsb 20 45 100)
                    (") = " ++ (if cj == length q - 1 then fmt 1 best else "...  (max so far " ++ fmt 1 best ++ ")")) ])
  where
    r = wR w
    q = wQ w
    ci = wCi w
    ck = wCk w
    cj = wCj w
    cs = compCs env
    y0 = 150
    xR = 80
    xS = xR + cs * 4 + 80
    xT = xS + cs * 3 + 80
    done = ci >= length r
    bx = 80
    by = y0 + cs * 4 + 70
    best = maximum ((-1) : [min (getAt 0 (getAt [] r ci) j) (getAt 0 (getAt [] q j) ck) | j <- [0 .. cj]])

drawClosure :: Env -> World -> [Picture]
drawClosure env w =
  [ txt tx (y0 - 30) 16 (hsb 0 0 95) "Repeat R <- max(R, R o R)"
  , txt tx (y0 - 8) 16 (hsb 0 0 95) "to make it transitive (Space / A)"
  , drawMat (map (map Just) sim) 80 y0 cs "original \"similar\" R" "" "" (\_ _ -> False) (Just colNames)
  , drawMat (map (map Just) lst) (80 + cs * 5 + 120) y0 cs ("closure (" ++ show (length (wClosure w) - 1) ++ " steps)") "" ""
      (\i j -> getAt 0 (getAt [] lst i) j > getAt 0 (getAt [] sim i) j + 1e-9) (Just colNames)
  , txt tx (y0 + 30) 16 (hsb 0 0 95) "Transitive? (R o R <= R)"
  , txt tx (y0 + 54) 16 (hsb 0 0 95) ("original: " ++ (if leq (maxMin sim sim) sim then "yes" else "no") ++ "   closure: " ++ (if trans then "yes" else "not yet"))
  , txt tx (y0 + 90) 16 (hsb 0 0 95) "Glowing border = cells that grew"
  , txt tx (y0 + 114) 16 (hsb 0 0 95) "during the repetition"
    -- α カットで仲間分け（閉包は類似関係なので、α カットは同値関係になる）
  , txt 80 (y0 + cs * 5 + 80) 18 (hsb 0 0 95) ("Groups at alpha = " ++ fmt 1 (wAlpha w) ++ " (alpha-cut of the closure)") ]
  ++ concat (zipWith group gxs groups)
  ++ [ txt 80 (gy + 112) 15 (hsb 0 0 70) "Up/Down changes alpha: groups merge or split (the larger alpha, the finer the split)" ]
  where
    sim = wSim w
    lst = lastClosure w
    cs = closCs env
    y0 = 150
    tx = 80 + cs * 10 + 220
    trans = leq (maxMin lst lst) lst
    groups = classesAt lst (wAlpha w)
    gy = y0 + cs * 5 + 96
    wOf g = fromIntegral (length g) * 80 + 20
    gxs = scanl (\x g -> x + wOf g + 20) 80 groups
    group gx g =
      [ Color (hsba 260 30 90 0.25) (rectTL gx gy (wOf g) 84)
      , Color (hsb 260 30 90) (thickPath 2 [(gx, gy), (gx + wOf g, gy), (gx + wOf g, gy + 84), (gx, gy + 84), (gx, gy)]) ]
      ++ concat [ [ Color (hsb (getAt 0 [0, 30, 55, 120, 220] m) 70 95) (at (cx, gy + 32) (disc 18))
                  , txtCB cx (gy + 74) 14 (hsb 0 0 90) (getAt "" colNames m) ]
                | (qn, m) <- zip [0 :: Int ..] g, let cx = gx + 50 + fromIntegral qn * 80 ]

-- 行列を描く。hl i j が True のセルは光る
drawMat :: [[Maybe Double]] -> Float -> Float -> Float -> String -> String -> String -> (Int -> Int -> Bool) -> Maybe [String] -> Picture
drawMat a x0 y0 cs title rn cn hl names = Pictures $
  [ txt x0 (y0 - 30) 16 (hsb 0 0 85) title ]
  ++ concat [ [ let s = maybe (rn ++ show (i + 1)) (\ns -> getAt "" ns i) names
                in txt (x0 - 8 - fromIntegral (length s) * 14 * 0.72) (y0 + fromIntegral i * cs + cs / 2 + 5) 14 (hsb 0 0 60) s ]
              ++ concat [ [ txtCB (x0 + fromIntegral j * cs + cs / 2) (y0 - 8) 14 (hsb 0 0 60) (maybe (cn ++ show (j + 1)) (\ns -> getAt "" ns j) names) | i == 0 ]
                          ++ [ Color (if hl i j then hsb 175 60 90 else hsb 250 18 14) (rectTL cx cy cs cs)
                             , Color (case v of { Nothing -> hsb 0 0 20; Just u -> hsb 20 (45 * realToFrac u + 5) (20 + 80 * realToFrac u) })
                                 (rectTL (cx + bw) (cy + bw) (cs - 2 * bw) (cs - 2 * bw))
                             , txtCB (cx + cs / 2) (cy + cs / 2 + 5) 14 (hsb 0 0 (case v of { Just u | u > 0.6 -> 10; _ -> 95 })) (maybe "?" (fmt 1) v) ]
                        | (j, v) <- zip [0 ..] row, let cx = x0 + fromIntegral j * cs, let cy = y0 + fromIntegral i * cs
                        , let bw = if hl i j then 3 else 1 ]
            | (i, row) <- zip [0 ..] a ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> click
  EventKey (Char '1') Down _ _ -> restart w { wPage = 1 }
  EventKey (Char '2') Down _ _ -> restart w { wPage = 2 }
  EventKey (SpecialKey KeySpace) Down _ _ -> if wPage w == 1 then stepComp w else stepClosure w
  EventKey (Char c) Down _ _ | c == 'a' || c == 'A' -> w { wAuto = not (wAuto w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wAlpha = clampv 0.1 1 (fromIntegral (jsRound (wAlpha w * 10 + 1)) / 10) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wAlpha = clampv 0.1 1 (fromIntegral (jsRound (wAlpha w * 10 - 1)) / 10) }
  _ -> w
  where
    (mx, my) = p5Mouse env
    hit a x0 y0 cs =
      let j = floor ((mx - x0) / cs) :: Int
          i = floor ((my - y0) / cs) :: Int
      in if i >= 0 && j >= 0 && i < length a && j < length (getAt [] a 0) then Just (i, j) else Nothing
    bump v = fromIntegral ((jsRound (v * 10) + 1) `mod` 11) / 10
    upd (i, j) f a = setAt i (setAt j (f (getAt 0 (getAt [] a i) j)) (getAt [] a i)) a
    click
      | wPage w == 1 =
          let cs = compCs env
              xS = 80 + cs * 4 + 80
              w1 = maybe w (\h -> restart w { wR = upd h bump (wR w) }) (hit (wR w) 80 150 cs)
          in maybe w1 (\h -> restart w1 { wQ = upd h bump (wQ w1) }) (hit (wQ w1) xS 150 cs)
      | otherwise = case hit (wSim w) 80 150 (closCs env) of
          Just (i, j) | i /= j ->
            let v = bump (getAt 0 (getAt [] (wSim w) i) j)
            in restart w { wSim = upd (j, i) (const v) (upd (i, j) (const v) (wSim w)) }
          _ -> w

-- ---- 自己テスト：手で計算できる値と比べる
selfTest :: [(String, Bool)]
selfTest =
  [ ("by hand: [[0.7, 0.5], [0.8, 0.4]] o [[0.9, 0.6, 0.5], [0.1, 0.7, 0.5]] = [[0.7, 0.6, 0.5], [0.8, 0.6, 0.5]]",
      sameM (maxMin [[0.7, 0.5], [0.8, 0.4]] [[0.9, 0.6, 0.5], [0.1, 0.7, 0.5]]) [[0.7, 0.6, 0.5], [0.8, 0.6, 0.5]])
  , ("composing with the identity I keeps R: R o I = R", sameM (maxMin r0 ident) r0)
  , ("associative: (R o S) o S' = R o (S o S')", sameM (maxMin (maxMin r0 q0) s') (maxMin r0 (maxMin q0 s')))
  , ("closure of 3: (1, 3) = min(0.8, 0.4) = 0.4", sameM c3 [[1, 0.8, 0.4], [0.8, 1, 0.4], [0.4, 0.4, 1]])
  , ("closure is transitive (C o C <= C) and >= the original R", leq (maxMin cl cl) cl && leq sim0 cl)
  , ("closure of the closure is the same (no more change)", sameM (transClosure cl) cl)
  , ("groups of the 0.5-cut of the closure of 3 = {1, 2}, {3}", classesAt c3 0.5 == [[0, 1], [2]]) ]
  where
    ident = [[if i == j then 1 else 0 | j <- [0 .. 3 :: Int]] | i <- [0 .. 3 :: Int]]
    s' = [[0.2, 0.9], [0.5, 0.4], [1, 0.3]]
    c3 = transClosure [[1, 0.8, 0], [0.8, 1, 0.4], [0, 0.4, 1]]
    cl = transClosure sim0

-- ---------- やってみよう ----------
-- 1. R のセルをクリックして 0 にすると、合成のどのセルが変わる？「道」が 1 本消えるから
-- 2. 合成の 1 つのセルで、どの y を通る道が答えを決めたか（オレンジの棒）を読もう
-- 3. 推移閉包で、赤〜青の「似ている度」はいくつになった？ どの色を通る道？
-- 4. α を 0.1〜1 まで動かすと、5 つの色の仲間分けが木のように分かれていく（階層クラスタリング）
-- 5. maxMinCell の min を積（*）にかえた「max-積 合成」にして、結果を比べよう

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

-- ---- ファジィ関係（行列）の道具
-- max-min 合成の 1 つのセル：max_j min(A[i][j], B[j][k])
maxMinCell :: [[Double]] -> [[Double]] -> Int -> Int -> Double
maxMinCell a b i k = foldl max 0 [min (getAt 0 ra j) (getAt 0 rb k) | (j, rb) <- zip [0 ..] b]
  where ra = getAt [] a i

maxMin :: [[Double]] -> [[Double]] -> [[Double]]
maxMin a b = [[maxMinCell a b i k | k <- [0 .. length (getAt [] b 0) - 1]] | i <- [0 .. length a - 1]]

maxOf :: [[Double]] -> [[Double]] -> [[Double]]
maxOf = zipWith (zipWith max)

sameM :: [[Double]] -> [[Double]] -> Bool
sameM a b = length a == length b && and (zipWith (\r s -> length r == length s && and (zipWith (\v u -> abs (v - u) < 1e-9) r s)) a b)

leq :: [[Double]] -> [[Double]] -> Bool
leq a b = and (zipWith (\r s -> and (zipWith (\v u -> v <= u + 1e-9) r s)) a b)

-- 推移閉包：C ← max(C, C∘C) を変わらなくなるまで（最大 20 回）
transClosure :: [[Double]] -> [[Double]]
transClosure = go (20 :: Int)
  where
    go 0 c = c
    go n c = let nx = maxOf c (maxMin c c) in if sameM nx c then c else go (n - 1) nx

-- α カットで同値類に分ける（C は推移閉包）
classesAt :: [[Double]] -> Double -> [[Int]]
classesAt c al = go [0 .. length c - 1] []
  where
    go [] _ = []
    go (i : rest) seen
      | i `elem` seen = go rest seen
      | otherwise =
          let g = [j | j <- [0 .. length c - 1], j `notElem` seen, getAt 0 (getAt [] c i) j >= al - 1e-9]
          in g : go rest (seen ++ g)

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

