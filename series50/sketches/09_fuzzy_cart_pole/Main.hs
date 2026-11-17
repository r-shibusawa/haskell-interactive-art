-- ============================================
-- 50-09 倒立振子のファジィ制御
-- 「棒が右に倒れかけていて、右に速く倒れているなら、台車を右に強く押す」― 7 × 7 = 49 個の規則で棒を立てる
--   入力：角度 θ・角速度 ω → マムダニ推論 → 力 F。台車は運動方程式を RK4 で解いて動かす。うすい台車は PID（比べる用）
-- 操作：クリック = 棒をつつく / 表のマスをクリック = 規則の出力を変える / R = やり直し / ↑↓ = はじめの角度
--       P = PID 表示 / X = 中央に戻す項 / D = 表をもとに
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (simulate)
import Data.List (foldl')
import System.Environment (lookupEnv)
import Numeric (showFFloat)

massM, massm, halfL, grav :: Double
massM = 1.0          -- ★ 台車の質量
massm = 0.1          -- ★ 棒の質量
halfL = 0.5          -- ★ 棒の半分の長さ（series20 と同じ式）
grav = 9.8

fMax :: Double
fMax = 30            -- ★ 力の最大 [N]

thR, omR :: Double
thR = 0.4            -- ★ 角度の範囲（これより外は端の集合に 1）
omR = 2.0            -- ★ 角速度の範囲

kX, kV :: Double
kX = 1.0             -- ★ 台車を中央へ戻す項 F_c = KX·x + KV·v（ファジィ規則の外）
kV = 2.0

kP, kI, kD :: Double
kP = 40              -- ★ 比べる PID のゲイン（角度に対して）
kI = 5
kD = 8

l7 :: [String]
l7 = ["NB", "NM", "NS", "ZE", "PS", "PM", "PB"]   -- 負で大・中・小、ゼロ、正で小・中・大

hStep :: Double
hStep = 1 / 240      -- RK4 の時間きざみ（1 フレームに 4 回）

benchPerFrame :: Int
benchPerFrame = 300  -- 成績の計算（10 秒 = 2400 歩）を 1 フレームに何歩ずつ進めるか

-- 制御器の種類（PID の積分の値は台車の中に持つ）
data Ctrl = CFuzzy | CPid | CZero deriving Eq

-- 1 台分のシミュレーション（状態・制御器・成績）
data Cart = Cart
  { cS      :: [Double]    -- [x, x', θ, θ']
  , cCtrl   :: !Ctrl
  , cI      :: !Double     -- PID の積分
  , cF      :: !Double     -- いまの力
  , cT      :: !Double
  , cT0     :: !Double     -- 最後につついた時刻
  , cSettle :: !Double     -- 整定時間
  , cMaxTh  :: !Double
  , cOver   :: !Double     -- 行きすぎ
  , cSgn    :: !Double
  , cSe     :: !Double     -- θ² の合計
  , cSf     :: !Double     -- F² の合計
  , cN      :: !Int
  , cMaxX   :: !Double
  , cFallen :: !Bool }

-- 成績（HUD と自己テストで同じ関数を使う）
data Res = Res { rFallen :: !Bool, rSettle, rMaxTh, rOver, rRmsTh, rRmsF, rX, rMaxX :: !Double }

-- 制御器が見る設定（規則の推論系と、中央に戻す項を使うか）
data Setup = Setup { stSys :: FSys, stCenter :: !Bool }

data World = World
  { wRule    :: [[Int]]     -- ★ 規則の表：wRule[角度][角速度] = 力の番号
  , wCenter  :: !Bool       -- 中央に戻す項
  , wTh0     :: !Int        -- ★ はじめの角度 [度]（最初は 10）
  , wShowPID :: !Bool
  , wFz      :: !Cart
  , wPd      :: !Cart
  , wHist    :: [(Double, Double)]   -- 角度の履歴（新しい順、600 個まで）
  , wJob     :: Maybe (Setup, Cart, Cart)   -- 計算中の成績（ファジィ、PID）
  , wBench   :: Maybe (Res, Res)
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "50-09 Fuzzy cart-pole" world0)
  { skBack  = hsb 250 18 14
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

-- ★ 規則の表のもと：「角度 + 角速度」が大きいほど右へ強く（よく使われる形）
defaultTable :: [[Int]]
defaultTable = [ [ round (clampv 0 6 (fromIntegral (i + j - 3))) | j <- [0 .. 6 :: Int] ] | i <- [0 .. 6 :: Int] ]

world0 :: World
world0 = rebuild (resetAll (World defaultTable True 10 True (makeCart CFuzzy 10) (makeCart CPid 10) [] Nothing Nothing []))

-- 規則の表から推論系を作る
mkSys :: [[Int]] -> FSys
mkSys tab = FSys [FVar (-thR) thR (evenTri (-thR) thR l7), FVar (-omR) omR (evenTri (-omR) omR l7)]
                 (FVar (-fMax) fMax (evenTri (-fMax) fMax l7))
                 [ Rule [i, j] (getAt 3 (getAt [] tab i) j) | i <- [0 .. 6], j <- [0 .. 6] ] False False 121

setupOf :: World -> Setup
setupOf w = Setup (mkSys (wRule w)) (wCenter w)

-- 表や設定が変わったら成績を計算しなおす（何フレームかに分けて進める）
rebuild :: World -> World
rebuild w = w { wJob = Just (st, benchCart CFuzzy (wTh0 w), benchCart CPid (wTh0 w)), wBench = Nothing }
  where st = setupOf w

resetAll :: World -> World
resetAll w = w { wFz = makeCart CFuzzy (wTh0 w), wPd = makeCart CPid (wTh0 w), wHist = [] }

-- ★ 倒立振子の運動方程式（θ = 0 が直立、右に倒れると正）
cartPoleDeriv :: [Double] -> Double -> [Double]
cartPoleDeriv s u = case s of
  [_, xd, th, thd] ->
    let sn = sin th
        cs = cos th
        tmp = (u + massm * halfL * thd * thd * sn) / (massM + massm)
        thdd = (grav * sn - cs * tmp) / (halfL * (4 / 3 - (massm * cs * cs) / (massM + massm)))
    in [xd, tmp - (massm * halfL * thdd * cs) / (massM + massm), thd, thdd]
  _ -> [0, 0, 0, 0]

-- 制御器：状態 → (力, 新しい積分)
control :: Setup -> Cart -> (Double, Double)
control st c = case cCtrl c of
  CFuzzy -> (clampv (-fMax) fMax (mY (mamdani (stSys st) [th, thd]) + centerTerm st s), cI c)
  CPid -> let i' = clampv (-2) 2 (cI c + th * hStep)
          in (clampv (-fMax) fMax (kP * th + kI * i' + kD * thd + centerTerm st s), i')
  CZero -> (0, cI c)
  where
    s = cS c
    th = getAt 0 s 2
    thd = getAt 0 s 3

centerTerm :: Setup -> [Double] -> Double
centerTerm st s = if stCenter st then kX * getAt 0 s 0 + kV * getAt 0 s 1 else 0

makeCart :: Ctrl -> Int -> Cart
makeCart ctrl deg = Cart [0, 0, fromIntegral deg * pi / 180, 0] ctrl 0 0 0 0 0 0 0 1 0 0 0 0 False

-- 1 歩（H 秒）進めて成績をつける
advance :: Setup -> Cart -> Cart
advance st c
  | cFallen c = c
  | otherwise =
      c { cS = s', cI = i', cF = f, cT = t
        , cSettle = if a > 0.0349 then t - cT0 c else cSettle c   -- 2° を超えていた最後の時刻 = 整定時間
        , cOver = max (cOver c) (-(cSgn c) * th)                    -- 行きすぎ：反対側へ倒れた最大の角度
        , cMaxTh = max (cMaxTh c) a, cMaxX = max (cMaxX c) (abs x)
        , cSe = cSe c + th * th, cSf = cSf c + f * f, cN = cN c + 1
        , cFallen = a > 1.2 }
  where
    (f, i') = control st c
    s1 = rk4 cartPoleDeriv (cS c) f hStep
    s' = case s1 of
      [x0, xd0, th0, thd0] -> [clampv (-4) 4 x0, clampv (-30) 30 xd0, th0, clampv (-40) 40 thd0]
      _ -> [0, 0, 0, 0]
    x = getAt 0 s' 0
    th = getAt 0 s' 2
    t = cT c + hStep
    a = abs th

-- ベンチマーク：はじめの角度 deg から T 秒
benchCartD :: Ctrl -> Double -> Cart
benchCartD ctrl deg = (makeCart ctrl 0) { cS = [0, 0, deg * pi / 180, 0], cSgn = if deg == 0 then 1 else signum deg }

benchCart :: Ctrl -> Int -> Cart
benchCart ctrl deg = benchCartD ctrl (fromIntegral deg)

runFor :: Setup -> Double -> Int -> Cart -> Cart
runFor st tEnd = go
  where
    go 0 c = c
    go k c | cT c < tEnd && not (cFallen c) = let c' = advance st c in c' `seq` go (k - 1) c'
           | otherwise = c

finished :: Double -> Cart -> Bool
finished tEnd c = cFallen c || cT c >= tEnd

result :: Cart -> Res
result c = Res (cFallen c) (cSettle c) (cMaxTh c) (cOver c) (sqrt (cSe c / n)) (sqrt (cSf c / n)) (getAt 0 (cS c) 0) (cMaxX c)
  where n = fromIntegral (max 1 (cN c))

simulate :: Setup -> Ctrl -> Double -> Double -> Res
simulate st ctrl deg tEnd = result (runFor st tEnd maxBound (benchCartD ctrl deg))

-- 毎フレーム：4 歩進める、履歴、成績の計算を少し進める
step :: World -> World
step w = w { wFz = fz, wPd = pd, wHist = take 600 ((getAt 0 (cS fz) 2, getAt 0 (cS pd) 2) : wHist w), wJob = job, wBench = bench }
  where
    st = setupOf w
    four c = advance st (advance st (advance st (advance st c)))
    fz = four (wFz w)
    pd = if wShowPID w then four (wPd w) else wPd w
    (job, bench) = case wJob w of
      Nothing -> (Nothing, wBench w)
      Just (st', a, b) ->
        let a' = runFor st' 10 benchPerFrame a
            b' = runFor st' 10 benchPerFrame b
        in if finished 10 a' && finished 10 b' then (Nothing, Just (result a', result b')) else (Just (st', a', b'), Nothing)

-- 規則の表の位置と大きさ
tableAt :: Env -> (Float, Float, Float)
tableAt env = (envW env * 0.54, 110, min 52 (envH env * 0.48 / 8))

draw :: Env -> World -> Picture
draw env w = view env $
  [ txt 20 30 16 (hsb 0 0 95) ("Click: poke   Table cell: change rule   R: restart   Up/Down: start angle " ++ show (wTh0 w) ++ " deg")
  , txt 20 52 16 (hsb 0 0 95) ("P: PID " ++ onOff (wShowPID w) ++ "   X: centering term " ++ onOff (wCenter w) ++ "   D: reset table")
  , txt 20 76 15 (hsb 0 0 70) ("F = fuzzy(th, w)" ++ (if wCenter w then " + " ++ show (round kX :: Int) ++ "*x + " ++ show (round kV :: Int) ++ "*v (back to center)" else "")
                               ++ "   orange = fuzzy, pale blue = PID")
    -- 台車と棒
  , Color (hsb 0 0 50) (thickLine 3 (30, railY + 22) (sw - 30, railY + 22)) ]
  ++ [ drawCart (xOf (getAt 0 (cS pd) 0)) railY (f2 (getAt 0 (cS pd) 2)) lp (hsba 190 40 90 0.45) (cFallen pd) | wShowPID w ]
  ++ [ drawCart fzX railY (f2 (getAt 0 (cS fz) 2)) lp (hsb 20 50 100) (cFallen fz)
     , Color (hsb 140 60 90) (thickLine 5 (fzX, railY) (fzX + f2 (cF fz) * 3, railY))
     , txtCB (clampF (half + 10) (sw - half) fzX) (railY + 50) 14 (hsb 0 0 90) info ]
  ++ (if cFallen fz then [ txt 40 (railY - lp - 40) 28 (hsb 0 70 100) "Fell over! R: restart"
                         , txt 40 (railY - lp - 14) 16 (hsb 0 70 100) "(rule table changed too much?)" ] else [])
     -- 角度のグラフ
  ++ [ Color (hsba 0 0 100 0.2) (frameTL gx gy gw gh)
     , Color (hsba 0 0 100 0.2) (Line [(gx, gy + gh / 2), (gx + gw, gy + gh / 2)]) ]
  ++ [ Color col (thickPath 2 [ (gx + fromIntegral i / 600 * gw, gy + gh / 2 - f2 (clampv (-1) 1 (sel h / 0.35)) * (gh / 2))
                              | (i, h) <- zip [0 :: Int ..] (reverse (wHist w)) ])
     | (k, col, sel) <- [(1 :: Int, hsba 190 40 90 0.6, snd), (0, hsb 20 50 100, fst)], k == 0 || wShowPID w, length (wHist w) > 1 ]
  ++ [ txt (gx + 6) (gy + 18) 14 (hsb 0 0 70) "Angle th (+-20 deg), last 10 s" ]
     -- 規則の表（明るさ = 発火の強さ）
  ++ [ txt tx (ty - 12) 14 (hsb 0 0 80) "Rule table  row: angle th  col: ang. velocity w  cell: force F" ]
  ++ [ txt (tx + fromIntegral (j + 1) * cs + 8) (ty + cs * 0.6) 14 (hsb 0 0 70) nm | (j, nm) <- zip [0 :: Int ..] l7 ]
  ++ [ txt (tx + 6) (ty + (fromIntegral i + 1.6) * cs) 14 (hsb 0 0 70) nm | (i, nm) <- zip [0 :: Int ..] l7 ]
  ++ concat [ cell i j | i <- [0 .. 6], j <- [0 .. 6] ]
     -- 成績の表
  ++ concat [ [ txt (mx + (if k > 0 then cw * (fromIntegral k + 0.3) else 0)) (my + fromIntegral i * 24) (if i == 0 then 14 else 15)
                    (if i == 0 then hsb 0 0 70 else if k == 1 then hsb 20 50 100 else if k == 2 then hsb 190 40 90 else hsb 0 0 85) c
              | (k, c) <- zip [0 :: Int ..] row ]
            | (i, row) <- zip [0 :: Int ..] rows ]
  ++ [ txt 20 (envH env - 20) 14 (if all snd (wTest w) then hsb 160 50 90 else hsb 0 80 100) (selfTestLine (wTest w)) ]
  where
    fz = wFz w
    pd = wPd w
    st = setupOf w
    r = mamdani (stSys st) [getAt 0 (cS fz) 2, getAt 0 (cS fz) 3]
    sw = envW env * 0.5
    railY = envH env * 0.42
    xOf xw = 30 + f2 ((xw + 4) / 8) * (sw - 60)
    lp = (sw - 60) / 8 * 2 * f2 halfL * 1.6
    fzX = xOf (getAt 0 (cS fz) 0)
    info = "th = " ++ fmt 1 (getAt 0 (cS fz) 2 * 180 / pi) ++ " deg  w = " ++ fmt 2 (getAt 0 (cS fz) 3)
           ++ "  F = " ++ fmt 1 (cF fz) ++ " N (fuzzy " ++ fmt 1 (mY r) ++ ")"
    half = fromIntegral (length info) * 14 * 0.36
    gx = 30
    gy = railY + 80
    gw = sw - 60
    gh = envH env - gy - 60
    (tx, ty, cs) = tableAt env
    cell i j =
      let wv = getAt 0 (mW r) (i * 7 + j)
          k = getAt 3 (getAt [] (wRule w) i) j
          x = tx + fromIntegral (j + 1) * cs
          y = ty + fromIntegral (i + 1) * cs
          hue = if k < 3 then 190 else if k > 3 then 20 else 260
      in [ Color (hsb hue (25 + fromIntegral (abs (k - 3)) * 15) (22 + 70 * f2 wv)) (rectTL x y (cs - 4) (cs - 4))
         , Color (hsba 0 0 100 (if wv > 0 then 0.8 else 0.15)) (if wv > 0 then thickPath 2 (box x y (cs - 4)) else frameTL x y (cs - 4) (cs - 4))
         , txt (x + 7) (y + cs * 0.42) 14 (hsb 0 0 (if wv > 0.6 then 10 else 92)) (getAt "" l7 k) ]
         ++ [ txt (x + 4) (y + cs * 0.8) 14 (hsb 0 0 (if wv > 0.6 then 10 else 92)) (fmt 2 wv) | wv > 0 ]
    box x y s = [(x, y), (x + s, y), (x + s, y + s), (x, y + s), (x, y)]
    my = ty + 8.4 * cs
    mx = tx
    cw = (envW env - tx - 30) / 3
    deg v = fmt 2 (v * 180 / pi) ++ " deg"
    secs v = fmt 2 v ++ " s"
    bw f = case wBench w of
      Just (a, b) -> (f a, f b)
      Nothing -> ("...", "...")
    benchRow name f = let (a, b) = bw f in [name, a, b]
    rows = [ ["", "Fuzzy", "PID"]
           , ["Now: settling (|th| < 2 deg)", secs (cSettle fz), secs (cSettle pd)]
           , ["Now: overshoot", deg (cOver fz), deg (cOver pd)]
           , ["Now: cart max |x|", fmt 2 (cMaxX fz) ++ " m", fmt 2 (cMaxX pd) ++ " m"]
           , benchRow (show (wTh0 w) ++ " deg, 10 s: settling") (\b -> if rFallen b then "fell" else secs (rSettle b))
           , benchRow "  overshoot" (deg . rOver)
           , benchRow "  RMS angle" (deg . rRmsTh)
           , benchRow "  RMS force" (\b -> fmt 2 (rRmsF b) ++ " N")
           , benchRow "  cart max |x|" (\b -> fmt 2 (rMaxX b) ++ " m") ]

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- 台車と棒（cx：台車の中心、th：角度）
drawCart :: Float -> Float -> Float -> Float -> Color -> Bool -> Picture
drawCart cx y th lp col fallen = Pictures
  [ Color col (rectTL (cx - 34) (y - 18) 68 36)
  , Color (hsb 0 0 30) (at (cx - 20, y + 18) (disc 7)), Color (hsb 0 0 30) (at (cx + 20, y + 18) (disc 7))
  , Color pc (thickLine 7 (cx, y - 18) (tx, ty))
  , Color pc (at (tx, ty) (disc 10)) ]
  where
    tx = cx + lp * sin th
    ty = y - 18 - lp * cos th
    pc = if fallen then hsb 0 70 100 else col

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | i >= 0 && i < 7 && j >= 0 && j < 7 ->      -- 表のマス：規則の出力を 1 つ進める
        rebuild w { wRule = [ [ if (a, b) == (i, j) then (k + 1) `mod` 7 else k | (b, k) <- zip [0 ..] row ] | (a, row) <- zip [0 ..] (wRule w) ] }
    | mx < envW env * 0.5 ->                      -- つつく：角速度を足す（ファジィも PID も同じだけ）
        let d = if mx < 30 + f2 ((getAt 0 (cS (wFz w)) 0 + 4) / 8) * (envW env * 0.5 - 60) then -0.8 else 0.8
            poke c = c { cS = [ if q == 3 then v + d else v | (q, v) <- zip [0 :: Int ..] (cS c) ]
                       , cT0 = cT c, cSettle = 0, cMaxTh = 0, cMaxX = 0, cOver = 0, cSgn = signum d }
        in w { wFz = poke (wFz w), wPd = poke (wPd w) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> resetAll w
    | c `elem` "pP" -> w { wShowPID = not (wShowPID w) }
    | c `elem` "xX" -> rebuild w { wCenter = not (wCenter w) }
    | c `elem` "dD" -> resetAll (rebuild w { wRule = defaultTable })
  EventKey (SpecialKey KeyUp) Down _ _ -> resetAll (rebuild w { wTh0 = min 20 (wTh0 w + 1) })
  EventKey (SpecialKey KeyDown) Down _ _ -> resetAll (rebuild w { wTh0 = max 1 (wTh0 w - 1) })
  _ -> w
  where
    (mx, my) = p5Mouse env
    (tx, ty, cs) = tableAt env
    j = floor ((mx - tx) / cs) - 1 :: Int
    i = floor ((my - ty) / cs) - 1 :: Int

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("rule table is point-symmetric (mirror -> opposite force)", anti)
  , ("th = 0, w = 0 -> only ZE fires -> F = 0", nearTol 1e-9 (mY (mamdani sys [0, 0])) 0)
  , ("th = peak of PS, w = 0 -> only rule (PS, ZE) -> F = centroid of PS = Fmax/3 = 10 N"
    , length (filter (> 0) (mW r)) == 1 && nearTol 1e-9 (mY r) (fMax / 3))
  , ("w = 0: a larger angle never gives a weaker force (monotone)", mono)
  , ("fuzzy: from 10 deg stays up 10 s, |th| < 2 deg within 3 s, cart back to |x| < 0.1 m"
    , not (rFallen b) && rSettle b < 3 && abs (rX b) < 0.1)
  , ("fuzzy: also stands from 2 deg", not (rFallen b2) && rMaxTh b2 < 0.04)
  , ("PID (for comparison) also stands from 10 deg", not (rFallen (simulate st CPid 10 10)))
  , ("zero force falls within 3 s (the equations are unstable)", rFallen (simulate st CZero 10 3)) ]
  where
    tab = defaultTable
    sys = mkSys tab
    st = Setup sys True
    anti = and [ getAt 0 (getAt [] tab (6 - i)) (6 - j) == 6 - getAt 0 (getAt [] tab i) j | i <- [0 .. 6], j <- [0 .. 6] ]
    psPeak = case drop 4 (fvSets (head' (syInputs sys))) of
      (FSet _ (Tri _ pk _) : _) -> pk
      _ -> 0
    head' xs = case xs of { (x : _) -> x; [] -> FVar 0 0 [] }
    r = mamdani sys [psPeak, 0]
    ys = [ mY (mamdani sys [t, 0]) | t <- takeWhile (<= 0.5) (iterate (+ 0.02) (-0.5)) ]
    mono = and (zipWith (\p y -> y >= p - 1e-9) ys (drop 1 ys))
    b = simulate st CFuzzy 10 10
    b2 = simulate st CFuzzy 2 10

-- ---------- やってみよう ----------
-- 1. 表のまん中（ZE, ZE）を PB にすると何が起きる？ D でもとに戻せる
-- 2. X で「中央に戻す項」を切ると、棒は立つが台車はどこへ行く？
-- 3. ↑↓ ではじめの角度を大きくすると、どこから倒れる？ ファジィと PID で比べる
-- 4. thR（角度の範囲）を 0.2 にすると、少しの傾きで強く押す制御器になる。成績の表はどう変わる？
-- 5. 右の列（ω が PB）を全部 ZE にすると、ブレーキ（角速度の項）がなくなる

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

-- 左上が (x, y) の長方形の枠
frameTL :: Float -> Float -> Float -> Float -> Picture
frameTL x y w h = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]

-- 小数点以下 n 桁で書く（p5.js の toFixed / nf）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) v ""

-- Double を画面の Float に
f2 :: Double -> Float
f2 = realToFrac

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

-- メンバーシップ関数の形とパラメータ（このスケッチは三角だけ使う）
data MF = Tri !Double !Double !Double

evalMF :: MF -> Double -> Double
evalMF (Tri a b c) x = trimf x a b c

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- ---- 非ファジィ化：重心 = ∫x μ dx ÷ ∫μ dx（折れ線の区間ごとに正確に積分）。どの規則も発火しなければ Nothing
centroid :: [Double] -> [Double] -> Maybe Double
centroid xs ys = if a > 1e-12 then Just (m / a) else Nothing
  where
    (a, m) = foldl' add (0, 0) (zip4' xs (drop 1 xs) ys (drop 1 ys))
    add (!aa, !mm) (x0, x1, y0, y1) =
      let h = x1 - x0
      in (aa + h * (y0 + y1) / 2, mm + h * (x0 * (2 * y0 + y1) + x1 * (y0 + 2 * y1)) / 6)
    zip4' (p : ps) (q : qs) (r : rs) (s : ss) = (p, q, r, s) : zip4' ps qs rs ss
    zip4' _ _ _ _ = []

-- ---- マムダニ型推論
-- 集合（名前とメンバーシップ関数）、変数（範囲と集合）、規則（入力ごとの集合の番号 → 出力の集合の番号。−1 は「使わない」）
data FSet = FSet { fsName :: String, fsMF :: MF }
data FVar = FVar { fvLo :: !Double, fvHi :: !Double, fvSets :: [FSet] }
data Rule = Rule { rIfs :: [Int], rThen :: !Int }
data FSys = FSys
  { syInputs :: [FVar], syOutput :: FVar, syRules :: [Rule]
  , syAndProd :: !Bool        -- かつ：False = min、True = 積
  , syImpProd :: !Bool        -- 含意：False = 切る（min）、True = 縮める（積）
  , syN :: !Int }             -- 出力を何点で調べるか

-- 推論の途中の値も全部返す（所属度・規則の強さ・後件ごとの強さ・集約した形・重心）
data MRes = MRes
  { mDeg :: [[Double]], mW :: [Double], mStr :: [Double]
  , mOx :: [Double], mAgg :: [Double], mY :: !Double, mFired :: !Bool }

mamdani :: FSys -> [Double] -> MRes
mamdani sys xin = MRes deg w str ox agg (maybe ((fvLo o + fvHi o) / 2) id c) (c /= Nothing)
  where
    deg = [ [evalMF (fsMF s) (clampv (fvLo v) (fvHi v) xi) | s <- fvSets v] | (v, xi) <- zip (syInputs sys) xin ]
    w = [ foldl' (\acc (i, si) -> if si < 0 then acc
                                   else let d = getAt 0 (getAt [] deg i) si
                                        in if syAndProd sys then acc * d else min acc d)
                 1 (zip [0 ..] (rIfs r))
        | r <- syRules sys ]
    o = syOutput sys
    nSets = length (fvSets o)
    -- 同じ後件は max でまとめる
    str = [ foldl' max 0 [wk | (r, wk) <- zip (syRules sys) w, rThen r == k] | k <- [0 .. nSets - 1] ]
    live = [ (s, fsMF f) | (s, f) <- zip str (fvSets o), s > 0 ]
    n = max 2 (syN sys)
    ox = [ fvLo o + (fvHi o - fvLo o) * fromIntegral i / fromIntegral (n - 1) | i <- [0 .. n - 1] ]
    agg = [ foldl' (\a (s, mf) -> let m = evalMF mf x in max a (if syImpProd sys then s * m else min s m)) 0 live | x <- ox ]
    c = centroid ox agg

-- 等間隔の三角 MF を n 個（両端は肩）
evenTri :: Double -> Double -> [String] -> [FSet]
evenTri lo hi names =
  [ FSet nm (Tri (lo + (i - 1) * d) (lo + i * d) (lo + (i + 1) * d)) | (i, nm) <- zip [0 ..] names ]
  where d = (hi - lo) / fromIntegral (length names - 1)

-- 4 次のルンゲ＝クッタ法：s' = f(s, u) を h だけ進める
rk4 :: ([Double] -> Double -> [Double]) -> [Double] -> Double -> Double -> [Double]
rk4 f s u h = zipWith5' (\v a b c d -> v + (h / 6) * (a + 2 * b + 2 * c + d)) s k1 k2 k3 k4
  where
    k1 = f s u
    k2 = f (zipWith (\v k -> v + (h / 2) * k) s k1) u
    k3 = f (zipWith (\v k -> v + (h / 2) * k) s k2) u
    k4 = f (zipWith (\v k -> v + h * k) s k3) u
    zipWith5' g (a : as) (b : bs) (c : cs) (d : ds) (e : es) = g a b c d e : zipWith5' g as bs cs ds es
    zipWith5' _ _ _ _ _ _ = []

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
