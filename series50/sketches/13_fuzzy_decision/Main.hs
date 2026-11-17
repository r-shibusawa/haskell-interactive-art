-- ============================================
-- 50-13 ファジィ意思決定（ベルマン－ザデーの決定）
-- 部屋探し：「安い」「駅に近い」「評判が良い」をそれぞれ所属度 0〜1 で評価する
--   決定 D = 目標と制約の交わり（かつ = min）。D がいちばん大きい候補を選ぶ。重み w は μ^w（w が大きいほどきびしく効く）
-- 操作：←→ = 基準を選ぶ / ↑↓ = その基準の重み w / T = かつ を min ↔ 積 / 下のグラフをドラッグ = 制約 C を動かす
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import System.Environment (lookupEnv)
import Numeric (showFFloat)

-- 候補：名前、家賃 [万円/月]、駅まで [分]、評判 [★]
data Opt = Opt { oName :: String, oPrice :: !Double, oWalk :: !Double, oStar :: !Double }

opts :: [Opt]
opts =                       -- ★ 候補
  [ Opt "A" 6.5 25 3.8
  , Opt "B" 8.0 10 4.2
  , Opt "C" 5.0 40 3.0
  , Opt "D" 7.0 15 4.6
  , Opt "E" 9.5 5 4.8 ]

-- 基準：名前、単位、値の取り出し方、グラフの範囲、メンバーシップ関数
data Crit = Crit { cName :: String, cUnit :: String, cKey :: Opt -> Double, cLo :: !Double, cHi :: !Double, cMF :: MF }

crits :: [Crit]
crits =                      -- ★ 基準のメンバーシップ関数（台形）
  [ Crit "Cheap" "10k yen" oPrice 3 11 (Trap 0 0 5 10)
  , Crit "Near station" "min" oWalk 0 50 (Trap 0 0 5 45)
  , Crit "Good reviews" "stars" oStar 2.5 5 (Trap 3 4.8 5 5) ]

data World = World
  { wWts  :: [Double]     -- ★ 重み（μ^w）。最初は [1, 1, 1]
  , wSel  :: !Int         -- 選んでいる基準
  , wProd :: !Bool        -- かつ を積にするか
  , wCMid :: !Double      -- 下のグラフ：制約 C の真ん中（ドラッグで動かす）
  , wTest :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "50-13 Fuzzy decision making" world0)
  { skBack  = hsb 250 18 14
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [1, 1, 1] 0 False 6 []

goalG :: Double -> Double
goalG x = clampv 0 1 (x / 10)

constrC :: Double -> Double -> Double
constrC x m = clampv 0 1 (0.5 - (x - m) / 10)     -- m で 0.5、傾き −1/10

-- 下のグラフの枠
data Plot = Plot { pX, pY, pW, pH :: !Float }

plotOf :: Env -> Plot
plotOf env = Plot 20 by (envW env - 60) (envH env - by - 60)
  where by = 100 + envH env * 0.2 + 80 + 26 * fromIntegral (length opts) + 50

draw :: Env -> World -> Picture
draw env w = view env $
  [ txt 20 36 17 (hsb 0 0 95) ("Left/Right: criterion (" ++ cName (getAt (head' crits) crits (wSel w)) ++ ")   Up/Down: weight w = "
                               ++ fmt 2 (wt (wSel w)) ++ "   T: AND = " ++ (if wProd w then "product" else "min") ++ "   Drag the lower graph")
  , txt 20 62 15 (hsb 0 0 70) "D = cheap^w1 AND near^w2 AND good^w3: the weakest one decides (min); product lets all count a bit" ]
  ++ concatMap critPanel (zip [0 ..] crits)
    -- 中段：表と D の棒
  ++ [ txt (getAt 0 cols i) ty 16 (hsb 0 0 95) h
     | (i, h) <- zip [0 ..] ["Option", "Rent", "Walk", "Rating", "cheap^w", "near^w", "good^w", "Decision D"] ]
  ++ concat [ let y = ty + 26 + fromIntegral j * 26
                  c = if j == best then hsb 20 45 100 else hsb 0 0 85
              in [ txt (getAt 0 cols 0) y 15 c (oName o ++ (if j == best then " *" else ""))
                 , txt (getAt 0 cols 1) y 15 c (fmt 1 (oPrice o))
                 , txt (getAt 0 cols 2) y 15 c (show (round (oWalk o) :: Int) ++ " min")
                 , txt (getAt 0 cols 3) y 15 c (fmt 1 (oStar o)) ]
                 ++ [ txt (getAt 0 cols (4 + i)) y 15 c (fmt 3 m) | (i, m) <- zip [0 ..] row ]
                 ++ [ txt (getAt 0 cols 7) y 15 c (fmt 3 d)
                    , Color (if j == best then hsb 20 45 100 else hsb 260 30 70)
                            (rectTL (getAt 0 cols 8) (y - 14) ((envW env - getAt 0 cols 8 - 40) * realToFrac d) 16) ]
            | (j, o, row, d) <- zip4 [0 ..] opts mus ds ]
    -- 下段：連続のベルマン－ザデー（目標 G と制約 C の交わり）
  ++ [ Color (hsba 0 0 100 0.05) (rectTL bx by bw bh) ]
  ++ [ Color (hsba 20 45 100 0.3) (Polygon [(x1, yOf 0), (x1, y1), (x2, y2), (x2, yOf 0)])
     | ((x1, y1), (x2, y2)) <- zip dPts (drop 1 dPts) ]
  ++ [ Color (hsb 175 60 80) (thickPath 2.5 [ (xOf x, yOf (goalG x)) | x <- xsC ])
     , Color (hsb 260 30 90) (thickPath 2.5 [ (xOf x, yOf (constrC x (wCMid w))) | x <- xsC ])
     , Color (hsb 20 45 100) (thickPath 4 dPts)
     , Color (hsb 0 70 100) (thickLine 2 (xOf argX, yOf 0) (xOf argX, yOf dMax))
     , txt (bx + 10) (by + 20) 15 (hsb 0 0 95) "Continuous example: x = how large a room to choose (0..10).  Goal G \"large\" = x/10 (teal)"
     , txt (bx + 10) (by + 40) 15 (hsb 0 0 95) "Constraint C \"fits the budget\" (purple, drag to move).  D = G AND C (orange)"
     , txt (xOf argX + 8) (yOf dMax - 8) 15 (hsb 0 70 100) ("best x* = " ++ fmt 2 argX ++ ", D(x*) = " ++ fmt 3 dMax)
     , txt 20 (envH env - 20) 14 (if all snd (wTest w) then hsb 160 50 90 else hsb 0 80 100) (selfTestLine (wTest w)) ]
  where
    wt i = getAt 1 (wWts w) i
    (mus, ds, best) = decide opts crits (wWts w) (wProd w)
    pw = (envW env - 80) / 3
    ph = envH env * 0.2
    py = 100
    ty = py + ph + 80
    cols = [20, 90, 190, 290, 390, 520, 640, 760, 900] :: [Float]
    -- 上段：3 つのメンバーシップ関数と候補の位置
    critPanel (i, c) =
      let px = 20 + fromIntegral i * (pw + 20)
          xO v = px + 10 + realToFrac ((v - cLo c) / (cHi c - cLo c)) * (pw - 20)
          yO m = py + ph + 20 - realToFrac m * (ph - 20)
          isSel = i == wSel w
      in [ Color (hsba 0 0 100 (if isSel then 0.12 else 0.05)) (rectTL px py pw (ph + 40))
         , Color (if isSel then hsb 20 45 100 else hsba 0 0 100 0.15) (rectLine (px + pw / 2) (py + (ph + 40) / 2) pw (ph + 40))
         , txt (px + 10) (py + 22) 16 (hsb 0 0 95) (cName c ++ " (" ++ cUnit c ++ ")   w = " ++ fmt 2 (wt i))
         , Color (hsb 175 60 80) (thickPath 2.5 [ (xO v, yO (evalMF (cMF c) v ** wt i))
                                                | k <- [0 .. 80 :: Int], let v = cLo c + (cHi c - cLo c) * fromIntegral k / 80 ]) ]
         ++ concat [ let v = cKey c o
                         m = getAt 0 (getAt [] mus j) i
                     in [ Color (hsba 0 0 100 0.3) (Line [(xO v, yO 0), (xO v, yO m)])
                        , Color (if j == best then hsb 20 45 100 else hsb 260 30 90) (at (xO v, yO m) (disc 5))
                        , txt (xO v - 5) (yO m - 8) 14 (hsb 0 0 90) (oName o) ]
                   | (j, o) <- zip [0 ..] opts ]
    Plot bx by bw bh = plotOf env
    xOf x = bx + 20 + realToFrac (x / 10) * (bw - 40)
    yOf m = by + bh - 14 - realToFrac m * (bh - 40)
    xsC = [fromIntegral k / 20 | k <- [0 .. 200 :: Int]] :: [Double]
    dVals = [ (x, min (goalG x) (constrC x (wCMid w))) | x <- xsC ]
    dPts = [ (xOf x, yOf d) | (x, d) <- dVals ]
    (argX, dMax) = foldl' (\(a, dm) (x, d) -> if d > dm + 1e-12 then (x, d) else (a, dm)) (0, -1) dVals
    head' xs = case xs of { (x : _) -> x; [] -> Crit "" "" oPrice 0 1 (Tri 0 0 0) }
    zip4 (a : as) (b : bs) (c : cs) (d : dd) = (a, b, c, d) : zip4 as bs cs dd
    zip4 _ _ _ _ = []

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventMotion _ | envDown env ->
    let Plot bx by bw bh = plotOf env
        (mx, my) = p5Mouse env
    in if my < by || my > by + bh then w else w { wCMid = clampv 0 10 (realToFrac ((mx - bx - 20) / (bw - 40)) * 10) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSel = (wSel w + 1) `mod` 3 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSel = (wSel w + 2) `mod` 3 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wWts = setAt (wSel w) (min 3 . (+ 0.25)) (wWts w) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wWts = setAt (wSel w) (max 0 . subtract 0.25) (wWts w) }
  EventKey (Char c) Down _ _ | c `elem` "tT" -> w { wProd = not (wProd w) }
  _ -> w
  where setAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

-- ---- 自己テスト：手で計算できる値と比べる
selfTest :: [(String, Bool)]
selfTest =
  [ ("G = x/10, C = 1 - x/10 -> D = min is largest at x = 5, D = 0.5", near am 5 && near dm 0.5)
  , ("with center m = 5 the C of this screen equals 1 - x/10", all (\x -> near (constrC x 5) (1 - x / 10)) [0, 2.5, 5, 7.5, 10])
  , ("option D memberships = (0.6, 0.75, 0.889), decision 0.6",
       near (mu 3 0) 0.6 && near (mu 3 1) 0.75 && near (mu 3 2) (1.6 / 1.8) && near (dOf r 3) 0.6)
  , ("all weights 1 -> plain min: A 0.444, B 0.4, C 0, D 0.6, E 0.1",
       and [near (dOf r j) v | (j, v) <- zip [0 ..] [0.8 / 1.8, 0.4, 0, 0.6, 0.1]])
  , ("best is D, ranking D > A > B > E > C",
       bestOf r == 3 && and (zipWith (\a b -> dOf r a > dOf r b) order (drop 1 order)))
  , ("weight of \"cheap\" 0 (mu^0 = 1, ignored): E is first with 1", bestOf r0 == 4 && near (dOf r0 4) 1)
  , ("weight of \"cheap\" 2: D = min(0.36, 0.75, 0.889) = 0.36", near (dOf r2 3) 0.36)
  , ("product: decision of D = 0.6 x 0.75 x 0.889 = 0.4", near (dOf (decide opts crits [1, 1, 1] True) 3) (0.6 * 0.75 * (1.6 / 1.8))) ]
  where
    (am, dm) = foldl' (\(a, d0) k -> let x = fromIntegral k / 10; d = min (x / 10) (1 - x / 10)
                                     in if d > d0 then (x, d) else (a, d0)) (-1, -1) [0 .. 100 :: Int]
    r = decide opts crits [1, 1, 1] False
    r0 = decide opts crits [0, 1, 1] False
    r2 = decide opts crits [2, 1, 1] False
    mu j i = let (ms, _, _) = r in getAt 0 (getAt [] ms j) i
    dOf (_, ds, _) j = getAt 0 ds j
    bestOf (_, _, b) = b
    order = [3, 0, 1, 4, 2]

-- ---------- やってみよう ----------
-- 1. ↑↓ で「安い」の重みを 0 → 3 と変える。どこで一番が D から ほかの候補に入れかわる？
-- 2. T で積にすると、順位はどう変わる？ min は「いちばん弱い所」だけを見る
-- 3. 自分の候補を opts に足そう（家賃 6 万、駅 30 分、評判 4.9 など）
-- 4. crits の台形を変えて「安い」を自分の感覚に合わせる（例 Trap 0 0 6 9）
-- 5. 下のグラフで C を動かす。x* はいつも G と C の交わる点になる？

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

-- 小数点以下 n 桁で書く（p5.js の toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) v ""

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

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

-- ---- ベルマン－ザデーの決定
-- 返り値：(候補ごとの μ^w の列, 決定 D, いちばん良い候補の番号)
decide :: [Opt] -> [Crit] -> [Double] -> Bool -> ([[Double]], [Double], Int)
decide os cs ws prod = (mus, ds, best)
  where
    mus = [ [ evalMF (cMF c) (cKey c o) ** getAt 1 ws i | (i, c) <- zip [0 ..] cs ] | o <- os ]
    ds = [ foldl' (\a m -> if prod then a * m else min a m) 1 row | row <- mus ]
    best = foldl' (\b (j, d) -> if d > getAt 0 ds b then j else b) 0 (zip [0 ..] ds)
