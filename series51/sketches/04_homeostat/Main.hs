-- ============================================
-- 51-04 ホメオスタットと超安定性（アシュビー 1948）
-- 4 つのユニットの針が互いに押し合う（結合の重み W）。針が赤い範囲（|x| > 1）に出ると、そのユニットの
-- 「ユニセレクター」が 1 段進み、入ってくる結合の重みを表の次の値に取りかえる。やがて全体が落ちつく組み合わせが見つかる
-- 操作：クリック = ユニットの針をはじく / 重みのマスをクリック・D = 結合を 1 本ひっくり返す（環境を変える）
--       U = ユニセレクターを止める / スペース = 一時停止 / R = 最初から（次の種）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nU :: Int
nU = 4               -- ユニットの数

damp, selfW, noiseA, gain, limitX, stableT :: Double
damp = 1.0           -- ★ 針の粘り（水の中で動く磁石のブレーキ）
selfW = -0.5         -- ★ 自分自身への結合（固定）
noiseA = 0.3         -- ★ 小さなゆらぎ（本物の装置にもある。不安定な配線を見のがさない）
gain = 1.0           -- ★ 他のユニットからの結合の大きさ（−GAIN〜GAIN）
limitX = 1           -- ★ 本質変数の範囲（針がこれを超えると取りかえ）
stableT = 30         -- この秒数だけ取りかえがなければ「安定」

posN :: Int
posN = 25            -- ユニセレクターの段の数（アシュビーの装置と同じ 25）

speed :: Int
speed = 3            -- 何倍速で動かすか

-- ---- ホメオスタットの状態
data Homeo = Homeo
  { hRng    :: !Rng
  , hX, hV  :: [Double]
  , hPos    :: [Int]
  , hRef    :: [Double]        -- 取りかえた後の休み時間
  , hSteps  :: !Int
  , hPer    :: [Int]
  , hT      :: !Double
  , hLast   :: !Double
  , hTables :: [[[Double]]]    -- hTables !! i !! 段 = ユニット i の入ってくる重み
  , hW      :: [[Double]] }

data World = World
  { wSeed   :: !Int
  , wHm     :: !Homeo
  , wHist   :: [[Double]]      -- 針の履歴（新しい順、600 個まで）
  , wPaused :: !Bool
  , wUni    :: !Bool
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "51-04 Homeostat" (World 3 (makeHomeostat 3) [] False True []))
  { skBack  = col (24, 35, 10) 100
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (World 3 (makeHomeostat 3) [] False True selfTest)   -- ★ 種 3 から
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

makeHomeostat :: Int -> Homeo
makeHomeostat sd = Homeo rng [0.3, -0.2, 0.1, 0.4] [0, 0, 0, 0] [0, 0, 0, 0] [0, 0, 0, 0] 0 [0, 0, 0, 0] 0 0 tables (map (getAt [] `flip` 0) tables)
  where
    -- ユニット i の「入ってくる重み」の表 25 段（乱数で作って固定）
    (tables, rng) = runRng (mapM (\i -> mapM (\_ -> mapM (\j -> if i == j then pure selfW else (\r -> gain * (2 * r - 1)) <$> nextR) [0 .. nU - 1]) [1 .. posN]) [0 .. nU - 1]) (makeRng (fromIntegral sd))

-- 1 きざみ：x_i'' = −DAMP·x_i' + Σ W_ij x_j + 小さなゆらぎ（針は ±1.3 の壁で止まる）
homeoStep :: Double -> Bool -> Double -> Homeo -> Homeo
homeoStep dt uni noise h = finish (foldl' unit (h { hRng = rng1, hV = v1, hX = [], hRef = [], hPos = [], hPer = [], hW = [] }) (zip7 [0 ..] (hX h) v1 (hRef h) (hPos h) (hPer h) (hW h)))
  where
    -- まず全部の速さ（乱数はユニットの順に 1 つずつ）
    (v1, rng1) = runRng (mapM (\(vi, wi) -> do
                    r <- nextR
                    let a = -damp * vi + noise * (2 * r - 1) + sum (zipWith (*) wi (hX h))
                    pure (clampv (-20) 20 (vi + dt * a))) (zip (hV h) (hW h))) (hRng h)
    unit acc (i, x0, vi, ref0, p0, per0, w0) =
      let x1 = x0 + dt * vi
          (x2, v2) | x1 > 1.3 = (1.3, 0)
                   | x1 < -1.3 = (-1.3, 0)
                   | otherwise = (x1, vi)
          ref1 = ref0 - dt
          swap = uni && abs x2 > limitX && ref1 <= 0       -- 本質変数が範囲の外 → ユニセレクターが 1 段進む
          p1 = if swap then (p0 + 1) `mod` posN else p0
      in acc { hX = hX acc ++ [x2], hV = setAt i v2 (hV acc), hRef = hRef acc ++ [if swap then 0.5 else ref1]
             , hPos = hPos acc ++ [p1], hPer = hPer acc ++ [if swap then per0 + 1 else per0]
             , hW = hW acc ++ [if swap then getAt [] (getAt [] (hTables h) i) p1 else w0]
             , hSteps = if swap then hSteps acc + 1 else hSteps acc, hLast = if swap then hT h else hLast acc }
    finish acc = acc { hT = hT h + dt }
    zip7 (a : as) (b : bs) (c : cs) (d : ds) (e : es) (f : fs) (g : gs) = (a, b, c, d, e, f, g) : zip7 as bs cs ds es fs gs
    zip7 _ _ _ _ _ _ _ = []

isSettled :: Homeo -> Bool
isSettled h = hT h - hLast h > stableT

step :: World -> World
step w = w { wHm = hm, wHist = take 600 (hX hm : wHist w) }
  where hm = if wPaused w then wHm w else foldl' (\h _ -> homeoStep (1 / 120) (wUni w) noiseA h) (wHm w) [1 .. speed * 2]

-- 画面の配置：メーターの幅、重みの表の左上とマスの大きさ
meterW :: Env -> Float
meterW env = min 260 ((envW env - 60) / 4 - 20)

tableAt :: Env -> (Float, Float, Float)
tableAt env = (30, 110 + meterW env * 0.95 + 40, 62)

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Homeostat - rewires itself until it settles (ultrastability)" 20 36 20 cTxt
  , say "Click: kick a needle   weight cell / D: flip a link (change the environment)" 20 60 15 cSub
  , say ("U uniselector " ++ (if wUni w then "ON" else "stopped")
         ++ "   Space " ++ (if wPaused w then "resume" else "pause") ++ "   R restart (seed " ++ show (wSeed w) ++ ")") 20 82 15 cSub ]
  -- ---- 4 つの針（メーター）
  ++ concat [ meter i | i <- [0 .. nU - 1] ]
  -- ---- 重みの表
  ++ [ say "Weights W (row = receiving unit, click to flip)" tx (ty - 10) 14 cSub ]
  ++ concat [ let wv = f2 (getAt 0 (getAt [] (hW hm) i) j)
                  c = if wv >= 0 then hsb 24 80 (30 + 60 * abs wv) else hsb 190 60 (30 + 60 * abs wv)
                  cxl = tx + fromIntegral j * cs
                  cyl = ty + fromIntegral i * cs
              in [ Color c (rectTL cxl cyl (cs - 4) (cs - 4)), sayC (fmt 2 (realToFrac wv)) (cxl + cs / 2 - 2) (cyl + cs / 2 - 2) 14 cTxt ]
            | i <- [0 .. nU - 1], j <- [0 .. nU - 1] ]
  ++ [ say ("Total swaps: " ++ show (hSteps hm)) sx (ty + 10) 18 cTube
     , say ("Since last swap " ++ fmt 1 (hT hm - hLast hm) ++ " s (" ++ num stableT ++ " s in a row = stable)") sx (ty + 38) 15 cTxt
     , say (if settled then "Stable OK - found wiring that keeps every needle in range"
            else if wUni w then "Searching... (a unit out of range rewires itself)"
            else "Uniselector stopped: if unstable, needles stick to the wall") sx (ty + 66) 16 (if settled then cScope else cBrass)
     , say ("Time " ++ fmt 0 (hT hm) ++ " s (x3 speed). No goal of its own: only \"change when out of range\"") sx (ty + 92) 14 cSub
  -- ---- 時間グラフ
     , plotFrame gx gy gwid ght "4 needles x (+-1.3, red dotted = +-1)"
     , hLine limitX gx gy gwid ght (-1.3) 1.3 cAlarm
     , hLine (-limitX) gx gy gwid ght (-1.3) 1.3 cAlarm ]
  ++ [ plotLine (map (\xs -> getAt 0 xs i) hist) gx gy gwid ght (-1.3) 1.3 c 600 1.6
     | (i, c) <- zip [0 ..] [cTube, cScope, cBrass, (200, 50, 95)] ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    hm = wHm w
    uw = meterW env
    uy = 110
    (tx, ty, cs) = tableAt env
    settled = isSettled hm
    sx = tx + fromIntegral nU * cs + 30
    gx = sx
    gy = ty + 110
    gwid = envW env - gx - 30
    ght = envH env - gy - 40
    hist = reverse (wHist w)
    ang v = -pi / 2 + v / 1.3 * 1.1
    arcPts cx cy r a b = [ (cx + r * cos t, cy + r * sin t) | k <- [0 .. 24 :: Int], let t = a + (b - a) * fromIntegral k / 24 ]
    meter i =
      let ux = 30 + fromIntegral i * (uw + 20)
          cx = ux + uw / 2
          cy = uy + uw * 0.55
          r = uw * 0.42
          xv = getAt 0 (hX hm) i
          out = abs xv > limitX
          a = ang (f2 xv)
      in [ Color (hsb 28 30 16) (rectTL ux uy uw (uw * 0.95))
         , Color (col cScope 50) (thickPath 10 (arcPts cx cy r (ang (-1)) (ang 1)))
         , Color (col cAlarm 60) (thickPath 10 (arcPts cx cy r (ang (-1.3)) (ang (-1))))
         , Color (col cAlarm 60) (thickPath 10 (arcPts cx cy r (ang 1) (ang 1.3)))
         , Color (col (if out then cAlarm else cTube) 100) (thickLine 4 (cx, cy) (cx + r * 0.95 * cos a, cy + r * 0.95 * sin a))
         , Color (col cBrass 100) (at (cx, cy) (disc 7))
         , say ("Unit " ++ show (i + 1) ++ "   x = " ++ fmt 2 xv) (ux + 12) (cy + 30) 15 (if out then cAlarm else cTxt)
         , say ("Pos " ++ show (getAt 0 (hPos hm) i + 1) ++ "/" ++ show posN ++ "   swaps " ++ show (getAt 0 (hPer hm) i)) (ux + 12) (cy + 52) 14 cSub ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = p5Mouse env
        (tx, ty, cs) = tableAt env
        uw = meterW env
        ci = floor ((my - ty) / cs) :: Int
        cj = floor ((mx - tx) / cs) :: Int
        i = floor ((mx - 30) / (uw + 20)) :: Int
        hm = wHm w
    in if ci >= 0 && ci < nU && cj >= 0 && cj < nU && ci /= cj then w { wHm = flipCell ci cj hm }   -- 重みのマスをクリック = その結合をひっくり返す
       else if i >= 0 && i < nU && my > 100 && my < 110 + uw
         then w { wHm = hm { hX = setAt i (if getAt 0 (hX hm) i > 0 then -0.9 else 0.9) (hX hm), hV = setAt i 0 (hV hm) } }
         else w
  EventKey (Char c) Down _ _
    | c `elem` "dD" -> w { wHm = flipOne (wHm w) }
    | c `elem` "uU" -> w { wUni = not (wUni w) }
    | c `elem` "rR" -> w { wSeed = wSeed w + 1, wHm = makeHomeostat (wSeed w + 1), wHist = [] }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- 環境が変わる：どこか 1 本の結合の符号を、表ごとひっくり返す（アシュビーが実験でやったこと）
flipOne :: Homeo -> Homeo
flipOne h = flipCell i j h
  where
    i = floor (fmod (hT h * 7.3) (fromIntegral nU)) :: Int
    j = (i + 1 + floor (fmod (hT h * 3.1) (fromIntegral (nU - 1)))) `mod` nU
    fmod a b = a - b * fromIntegral (truncate (a / b) :: Int)

flipCell :: Int -> Int -> Homeo -> Homeo
flipCell i j h = h { hTables = [ if k == i then map neg t else t | (k, t) <- zip [0 ..] (hTables h) ]
                   , hW = [ if k == i then neg row else row | (k, row) <- zip [0 ..] (hW h) ]
                   , hX = setAt i (clampv (-1.3) 1.3 (getAt 0 (hX h) i + 0.5)) (hX h) }
  where neg row = setAt j (negate (getAt 0 row j)) row

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
-- 重みを固定してゆらし、200 秒で落ちつくか
decays :: [[Double]] -> Bool
decays wm = maximum (map abs (hX h)) < 1e-3
  where h = iterateN (120 * 200) (homeoStep (1 / 120) False 0) ((makeHomeostat 1) { hW = wm, hX = [0.5, -0.5, 0.5, -0.5] })

iterateN :: Int -> (a -> a) -> a -> a
iterateN n f x0 = go n x0
  where go 0 x = x
        go k x = let x' = f x in x' `seq` go (k - 1) x'

-- 種 3 で安定するまで（900 秒まで）まわす
settleRun :: Int -> Homeo
settleRun sd = go (makeHomeostat sd)
  where go h | isSettled h || hT h >= 900 = h
             | otherwise = let h' = homeoStep (1 / 120) True noiseA h in h' `seq` go h'

selfTest :: [(String, Bool)]
selfTest =
  [ ("seed 3: " ++ show (hSteps h) ++ " swaps, stable at " ++ fmt 1 (hLast h) ++ " s", isSettled h && hSteps h > 0)
  , ("the found weights really are stable: fixed, no noise, |x| < 0.001 after 200 s", decays (hW h))
  , ("the first wiring (step 1 of each table) is unstable when fixed", not (decays (map (getAt [] `flip` 0) (hTables (makeHomeostat 3)))))
  , ("no links (only self -0.5): never swaps", hSteps free == 0)
  , ("all +1 positive feedback leaves the range and swaps within 5 s (" ++ fmt 2 tFirst ++ " s)", tFirst > 0 && tFirst < 5)
  , ("uniselector stopped: the first wiring sticks needles to the wall (+-1.3)", hSteps stuck == 0 && any (\v -> abs v > 1.29) (hX stuck)) ]
  where
    h = settleRun 3
    diag v = [ [ if i == j then selfW else v | j <- [0 .. nU - 1] ] | i <- [0 .. nU - 1] ]
    free = iterateN (120 * 30) (homeoStep (1 / 120) True 0) ((makeHomeostat 1) { hW = diag 0, hX = [0.6, -0.6, 0.6, -0.6] })
    tFirst = go (0 :: Int) ((makeHomeostat 1) { hW = diag 1 })
      where go k b | k >= 120 * 10 = -1
                   | otherwise = let b' = homeoStep (1 / 120) True 0 b in if hSteps b' > 0 then hT b' else go (k + 1) b'
    stuck = iterateN (120 * 60) (homeoStep (1 / 120) False 0) (makeHomeostat 3)

-- ---------- やってみよう ----------
-- 1. 安定したら D で環境を変えよう。もう一度安定するまで何回取りかえた？
-- 2. U でユニセレクターを止めてから D。針はどうなる？ もう一度 U で ON にすると？
-- 3. 安定しているときクリックで針をはじく。取りかえは起きる？（ゆれが小さければ起きない＝ふつうの安定）
-- 4. selfW を 0 にすると、安定するまでの回数はどう変わる？ gain を 2 にすると？
-- 5. 「目標を知らないのに、目標を達成する」とはどういうことか、話し合おう

-- ==================================================
-- ここから下は「サイバネティクスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 色は (色相, 彩度, 明るさ)。透明度は 0〜100
type HSB = (Float, Float, Float)

cTxt, cSub, cScope, cTube, cBrass, cAlarm :: HSB
cTxt = (40, 12, 96)
cSub = (35, 18, 72)
cScope = (135, 70, 92)
cTube = (24, 85, 100)
cBrass = (42, 55, 88)
cAlarm = (0, 80, 100)

col :: HSB -> Float -> Color
col (h, s, b) a = hsba h s b (a / 100)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> HSB -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size (col c 100) s))

-- 中央が (x, y) の文字（textAlign(CENTER, CENTER)）
sayC :: String -> Float -> Float -> Float -> HSB -> Picture
sayC s x y size c = say s (x - fromIntegral (length s) * size * 0.36) (y + size / 2) size c

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- オシロスコープ風の枠
plotFrame :: Float -> Float -> Float -> Float -> String -> Picture
plotFrame x y w h title = Pictures $
  [ Color (hsba 130 40 6 0.9) (rectTL x y w h) ]
  ++ [ Color (hsba 130 40 40 0.4) (Line [(x, y + h * k / 4), (x + w, y + h * k / 4)]) | k <- [1, 2, 3] ]
  ++ [ Color (hsb 42 55 60) (frameTL 1 x y w h) ]
  ++ [ say title (x + 8) (y + 18) 14 cSub | not (null title) ]

-- arr の値を lo〜hi の範囲で、枠 (x, y, w, h) に折れ線で描く（n = 横に並べる点の数）
plotLine :: [Double] -> Float -> Float -> Float -> Float -> Double -> Double -> HSB -> Int -> Float -> Picture
plotLine arr x y w h lo hi c n wt
  | length arr < 2 = Blank
  | otherwise = Color (col c 100) (thickPath wt [ (x + fromIntegral i / fromIntegral (n - 1) * w, y + h - f2 (clampv 0 1 ((fin lo v - lo) / (hi - lo))) * h)
                                              | (i, v) <- zip [0 :: Int ..] arr ])

-- グラフの中の水平線（目標値など、点線）
hLine :: Double -> Float -> Float -> Float -> Float -> Double -> Double -> HSB -> Picture
hLine v x y w h lo hi c =
  let yy = y + h - f2 (clampv 0 1 ((v - lo) / (hi - lo))) * h
  in Color (col c 70) (Pictures [ Line [(x + s, yy), (x + min w (s + 6), yy)] | s <- [0, 11 .. w] ])

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- JavaScript の数の書き方に近い形（整数なら小数点なし）
num :: Double -> String
num v | v == fromIntegral (round v :: Int) = show (round v :: Int)
      | otherwise = showFFloat Nothing v ""

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ---- 自己テストの道具
-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 16) 14 (if all snd rs then cScope else cAlarm)

-- マウスの位置を p5.js の座標（左上が原点、y は下向き）で
p5Mouse :: Env -> (Float, Float)
p5Mouse env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- リストの i 番目を v に取りかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [ if k == i then v else x | (k, x) <- zip [0 ..] xs ]

-- ---- 決まった種（seed）から毎回同じ乱数列（0〜1）を出す（p5.js 版の makeRng と同じ mulberry32）
newtype Rng = Rng Word32

makeRng :: Word32 -> Rng
makeRng = Rng

nextRng :: Rng -> (Double, Rng)
nextRng (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に使う計算（状態を手で受けわたさなくてよいように）
newtype R a = R (Rng -> (a, Rng))

instance Functor R where fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where R g >>= k = R (\s -> let (a, s1) = g s; R h = k a in h s1)

nextR :: R Double
nextR = R nextRng

runRng :: R a -> Rng -> (a, Rng)
runRng (R g) = g
