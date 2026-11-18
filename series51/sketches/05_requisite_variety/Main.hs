-- ============================================
-- 51-05 必要多様度の法則（アシュビー 1956）
-- 外乱 D がやってくる → あなたが調整 R を選ぶ → 表から結果 O が決まる。結果の種類をできるだけ少なく（一定に）保とう
-- でも結果の種類 V(O) は V(D) ÷ V(R) より少なくできない。「多様性をおさえられるのは多様性だけ」
-- 操作：1〜9 またはマスをクリック = 調整を選ぶ / ←→ = 調整の種類 V(R) / A = 最適な調整器が自動で / N = 新しい表
--       スペース = 次の外乱 / R = 記録を消す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (popCount, shiftL, shiftR, testBit, xor, (.|.))
import Data.List (find, foldl', nub)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nD :: Int
nD = 9                         -- 外乱の種類 V(D)

vrList :: [Int]
vrList = [1, 2, 3, 9]          -- ★ 選べる調整の種類 V(R)

letters :: String
letters = "abcdefghi"

-- 最適な調整器：めざす結果の数 V、結果の集合（ビット）、外乱ごとに選ぶ調整
data Best = Best { bV :: !Int, bSet :: !Int, bMap :: [Int] }

data World = World
  { wVrI    :: !Int
  , wTable  :: [[Int]]          -- 表 T[d][r] = 結果
  , wSeed   :: !Int
  , wCurD   :: !Int
  , wTally  :: [Int]
  , wRounds :: !Int
  , wAuto   :: !Bool
  , wAutoT  :: !Double
  , wBest   :: Maybe Best
  , wLastO  :: !Int
  , wRng    :: !Word32
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "51-05 Requisite variety" world0)
  { skBack  = col (24, 35, 10) 100
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ dt w -> pure (step (realToFrac dt) w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = newTable 0 (World 2 [] 0 0 [] 0 False 0 Nothing (-1) 51 [])

vrOf :: World -> Int
vrOf w = getAt 3 vrList (wVrI w)

-- 表 T[d][r] = 結果。どの列（1 つの調整）も、外乱がちがえば結果もちがう（列は 0〜8 の並べかえ）
makeTable :: Int -> [[Int]]
makeTable 0 = [ [ (d + k) `mod` nD | k <- [0, 1, 2, 5, 7, 3, 8, 4, 6] ] | d <- [0 .. nD - 1] ]
makeTable sd = [ [ getAt 0 p d | p <- cols ] | d <- [0 .. nD - 1] ]
  where
    cols = fst (foldl' (\(cs, r) _ -> let (p, r') = shuffle r in (cs ++ [p], r')) ([], fromIntegral sd) [1 .. 9 :: Int])
    -- フィッシャー–イェーツ法（p5.js 版と同じ順に乱数を使う）
    shuffle r0 = foldl' sw ([0 .. nD - 1], r0) [nD - 1, nD - 2 .. 1]
    sw (p, r) i = let (u, r') = rand r
                      j = floor (u * fromIntegral (i + 1))
                  in (swapAt i j p, r')

newTable :: Int -> World -> World
newTable sd w = resetTally w { wSeed = sd, wTable = makeTable sd }

resetTally :: World -> World
resetTally w = w { wTally = replicate nD 0, wRounds = 0, wLastO = -1, wBest = bestRegulator (wTable w) (vrOf w), wCurD = d, wRng = g }
  where (d, g) = randInt nD (wRng w)

-- ---- 最適な調整器：結果の集合 O' を小さい順にさがし、「どの外乱にも O' に入る調整がある」最小の O' を見つける
bestRegulator :: [[Int]] -> Int -> Maybe Best
bestRegulator t vr = find (const True)
  [ Best size m mp | size <- [1 .. nD], m <- [1 .. (1 `shiftL` nD) - 1], popCount m == size, Just mp <- [mapping m] ]
  where
    mapping m = mapM (\row -> find (\c -> testBit m (getAt 0 row c)) [0 .. vr - 1]) t

-- 多様度とエントロピー（ビット）
varietyOf :: [Int] -> Int
varietyOf = length . filter (> 0)

entropyBits :: [Int] -> Double
entropyBits cs
  | n == 0 = 0
  | otherwise = foldl' (\h c -> if c > 0 then h - (fromIntegral c / n) * logBase 2 (fromIntegral c / n) else h) 0 cs
  where n = fromIntegral (sum cs) :: Double

log2 :: Double -> Double
log2 = logBase 2

choose :: Int -> World -> World
choose r w
  | r < 0 || r >= vrOf w = w
  | otherwise =
      let o = getAt 0 (getAt [] (wTable w) (wCurD w)) r
          (d, g) = randInt nD (wRng w)
      in w { wLastO = o, wTally = [ if i == o then c + 1 else c | (i, c) <- zip [0 ..] (wTally w) ], wRounds = wRounds w + 1, wCurD = d, wRng = g }

step :: Double -> World -> World
step dt w
  | wAuto w = let t = wAutoT w + dt
              in if t > 0.25 then choose (maybe (-1) (\b -> getAt (-1) (bMap b) (wCurD w)) (wBest w)) w { wAutoT = 0 } else w { wAutoT = t }
  | otherwise = w

-- 表の位置とマスの大きさ
tableAt :: Env -> (Float, Float, Float)
tableAt env = (40, 120, minimum [54, (envH env - 220) / 10, (envW env * 0.5 - 80) / 10])

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Requisite variety - V(O) can never be smaller than V(D) / V(R)" 20 34 20 cTxt
  , say ("1-" ++ show vr ++ " or a cell: choose R   Left/Right V(R) = " ++ show vr ++ "   A best regulator " ++ onOff (wAuto w)) 20 60 15 cSub
  , say "N new table   Space next disturbance   R clear record" 20 82 15 cSub
    -- ---- 結果の表
  , say "D \\ R" tx (ty - 14) 14 cSub ]
  ++ [ say ("R" ++ show (c + 1)) (tx + fromIntegral (c + 1) * cs + cs * 0.25) (ty + cs * 0.6) 15 cTube | c <- [0 .. vr - 1] ]
  ++ concat [ row d | d <- [0 .. nD - 1] ]
  ++ [ say (if wAuto w then "Green cells = outcomes the best regulator aims at (" ++ show bestV ++ " kinds)"
                       else "Press A: the outcomes the best regulator aims at turn green") tx (ty + 11 * cs) 14 cScope ]
    -- 今の外乱
  ++ [ Color (col cTube 25) (rectTL cx cy 220 110)
     , say "Disturbance!" (cx + 16) (cy + 30) 16 cTube
     , say ("D" ++ show (wCurD w + 1)) (cx + 16) (cy + 82) 44 cTxt ]
  ++ [ say ("last: " ++ [getAt '?' letters (wLastO w)]) (cx + 110) (cy + 82) 18 cScope | wLastO w >= 0 ]
    -- ---- 記録と法則
  ++ [ say ("Histogram of outcomes   " ++ show (wRounds w) ++ " rounds") hx hy 15 cTxt ]
  ++ concat [ [ Color (col cScope 70) (rectTL (hx + fromIntegral o * (bw / 9)) (hy + 130 - 110 * fromIntegral c / mxT) (bw / 9 - 4) (110 * fromIntegral c / mxT))
              , say [getAt '?' letters o] (hx + fromIntegral o * (bw / 9) + 8) (hy + 150) 14 cSub ]
            | (o, c) <- zip [0 :: Int ..] (wTally w) ]
  ++ [ say ("V(D) = 9   V(R) = " ++ show vr ++ "   -> bound V(D)/V(R) = " ++ fmt 2 (9 / fromIntegral vr)) hx ly 15 cTxt
     , say ("Your V(O) = " ++ show vo ++ "   (best regulator: " ++ show bestV ++ ")") hx (ly + 26) 16 (if vo <= bestV && wRounds w > 0 then cScope else cTube)
     , say "In bits: log2 V(O) >= log2 V(D) - log2 V(R)" hx (ly + 52) 15 cTxt
     , say ("   " ++ (if vo > 0 then fmt 3 (log2 (fromIntegral vo)) else "-") ++ " >= " ++ fmt 3 (log2 9) ++ " - " ++ fmt 3 (log2 (fromIntegral vr))
            ++ " = " ++ fmt 3 (log2 9 - log2 (fromIntegral vr))) hx (ly + 74) 15 cTxt
     , say ("Entropy H(O) = " ++ fmt 3 (entropyBits (wTally w)) ++ " bits (H(D) = " ++ fmt 3 (log2 9) ++ ")") hx (ly + 98) 15 cTxt
     , say "Each column: different D -> different O," hx (ly + 126) 14 cSub
     , say "so one R sends only one D to each outcome" hx (ly + 146) 14 cSub
     , say "-> with V(R) choices, at most V(R) D's per O" hx (ly + 166) 14 cSub
     , say "-> V(O) >= V(D) / V(R)" hx (ly + 186) 14 cSub
     , drawSelfTest env (wTest w) ]
  where
    vr = vrOf w
    (tx, ty, cs) = tableAt env
    bestV = maybe 0 bV (wBest w)
    row d =
      let y = ty + fromIntegral (d + 1) * cs
          isCur = d == wCurD w
      in say ("D" ++ show (d + 1)) (tx + 6) (y + cs * 0.6) 15 (if isCur then cTxt else cSub)
         : concat [ let o = getAt 0 (getAt [] (wTable w) d) c
                        inBest = wAuto w && maybe False (\b -> testBit (bSet b) o) (wBest w)
                        pick = maybe False (\b -> getAt (-1) (bMap b) d == c) (wBest w)
                        x = tx + fromIntegral (c + 1) * cs
                    in [ Color (if inBest then hsb 135 50 30 else hsb 28 25 18) (rectTL x y (cs - 4) (cs - 4))
                       , Color (if isCur then col cTube 100 else hsb 30 20 30) (frameTL (if isCur then 2 else 1) x y (cs - 4) (cs - 4))
                       , sayC [getAt '?' letters o] (x + cs / 2 - 2) (y + cs / 2 - 2) 18 (if pick && wAuto w then cTube else cTxt) ]
                  | c <- [0 .. vr - 1] ]
    cx = tx + fromIntegral (max (vr + 2) 5) * cs + 20
    cy = ty
    hx = cx
    hy = cy + 150
    bw = min 380 (envW env - hx - 30)
    mxT = fromIntegral (maximum (1 : wTally w)) :: Float
    vo = varietyOf (wTally w)
    ly = hy + 190

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = p5Mouse env
        (tx, ty, cs) = tableAt env
        c = floor ((mx - tx) / cs) - 1
        d = floor ((my - ty) / cs) - 1
    in if d == wCurD w then choose c w else w
  EventKey (Char ch) Down _ _
    | ch >= '1' && ch <= '9' -> choose (fromEnum ch - fromEnum '1') w
    | ch `elem` "aA" -> w { wAuto = not (wAuto w) }
    | ch `elem` "nN" -> newTable (wSeed w + 1) w
    | ch `elem` "rR" -> resetTally w
  EventKey (SpecialKey KeyRight) Down _ _ -> resetTally w { wVrI = (wVrI w + 1) `mod` length vrList }
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetTally w { wVrI = (wVrI w + length vrList - 1) `mod` length vrList }
  EventKey (SpecialKey KeySpace) Down _ _ -> let (d, g) = randInt nD (wRng w) in w { wCurD = d, wRng = g }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 7 項目）
isPermCols :: [[Int]] -> Int -> Bool
isPermCols t vr = and [ length (nub [ getAt (-1) r c | r <- t ]) == nD | c <- [0 .. vr - 1] ]

selfTest :: [(String, Bool)]
selfTest =
  [ ("premise: every column of every table is a permutation of 0..8", and [ isPermCols (makeTable s) 9 | s <- [1 .. 30] ] && isPermCols t0 9)
  , ("minimum V(O) for V(R) = 1, 2, 3, 9: " ++ commas res ++ " (= ceil(9/V(R)) = 9, 5, 3, 1)", res == [9, 5, 3, 1])
  , ("30 random tables x 4 V(R): even the best has V(O) >= V(D)/V(R)"
    , and [ maybe False (\b -> bV b >= (nD + vr - 1) `div` vr) (bestRegulator (makeTable s) vr) | s <- [1 .. 30], vr <- vrList ])
  , ("best map, each of 9 disturbances once -> " ++ show (varietyOf cnt) ++ " kinds = the minimum found", Just (varietyOf cnt) == fmap bV b3)
  , ("H(O) = " ++ fmt 4 (entropyBits cnt) ++ " = log2 9 - log2 3 (equality)", near (entropyBits cnt) (log2 9 - log2 3) 1e-12)
  , ("break the premise (all outcomes a): V(O) = 1 < 9/3", fmap bV (bestRegulator (replicate nD [0, 0, 0]) 3) == Just 1)
  , ("entropy: one outcome = 0 bits, 9 equal = log2 9", near (entropyBits [5, 0, 0]) 0 1e-9 && near (entropyBits (replicate 9 2)) (log2 9) 1e-12) ]
  where
    t0 = makeTable 0
    res = [ maybe 0 bV (bestRegulator t0 vr) | vr <- vrList ]
    b3 = bestRegulator t0 3
    cnt = case b3 of
      Nothing -> replicate nD 0
      Just b -> foldl' (\cs d -> let o = getAt 0 (getAt [] t0 d) (getAt 0 (bMap b) d) in [ if i == o then c + 1 else c | (i, c) <- zip [0 ..] cs ]) (replicate nD 0) [0 .. nD - 1]
    commas xs = foldr1 (\a b -> a ++ ", " ++ b) (map show xs)

-- ---------- やってみよう ----------
-- 1. V(R) = 3 で 30 回やってみよう。結果を 3 種類だけにおさえられた？ 最適な調整器（A）と比べよう
-- 2. ←→ で V(R) = 1 にすると？ 何もできない調整器は、外乱をそのまま結果に通してしまう
-- 3. V(R) = 2 では、どうがんばっても 5 種類より少なくできない。なぜ 4.5 ではなく 5？
-- 4. N で表を変えると、最適な V(O) はどう変わる？ 法則の下限より多くなる表はある？
-- 5. 自分の生活で「外乱」「調整」「結果」を考えよう（天気と服、体温と汗、クラスと先生…）

-- ==================================================
-- ここから下は「サイバネティクスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で
p5Mouse :: Env -> (Float, Float)
p5Mouse env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

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

onOff :: Bool -> String
onOff b = if b then "ON" else "OFF"

-- ---- 数の道具
f2 :: Double -> Float
f2 = realToFrac

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (if isNaN v || isInfinite v then 0 else v) ""

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- i 番目と j 番目を入れかえる
swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs = case (drop i xs, drop j xs) of
  (a : _, b : _) -> [ if k == i then b else if k == j then a else x | (k, x) <- zip [0 ..] xs ]
  _ -> xs

-- ---- 決まった種（seed）から毎回同じ乱数列（0〜1）を出す（p5.js 版の makeRng と同じ mulberry32）
rand :: Word32 -> (Double, Word32)
rand a0 = (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, a)
  where
    a = a0 + 0x6d2b79f5
    t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
    t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))

-- 0〜n−1 の整数（Math.floor(rng() * n)）
randInt :: Int -> Word32 -> (Int, Word32)
randInt n g = let (u, g') = rand g in (min (n - 1) (floor (u * fromIntegral n)), g')

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

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
