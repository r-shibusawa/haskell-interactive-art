-- ============================================
-- 51-12 互いを観察する系（二次サイバネティクス）
-- コイン合わせ：A は「同じ面」なら勝ち、B は「ちがう面」なら勝ち。どちらも相手の過去の手から次の手を予測する
-- でも相手も自分を予測している。観察する者が、観察される系の一部になる → 手の列はどんどん「読めない」（エントロピー 1 ビット）へ
-- 操作：↑↓ = A の記憶の長さ / ←→ = B の記憶の長さ（自動のとき） / H = あなた vs B（← = 表、→ = 裏） / スペース = 速さ / R = やり直し
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftL, shiftR, xor, (.|.))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

forget, sure :: Double
forget = 0.98        -- ★ 古い記録をだんだん忘れる（相手の変化についていくため）
sure = 0.1           -- ★ 予測に自信がないとみなす幅

win :: Int
win = 200            -- 成績を数える直近の手数

speeds :: [Int]
speeds = [1, 4, 20]

-- 予測する人：記憶の長さ k、A（同じ面で勝ち）か、文脈ごとの回数、相手の手（新しい順、64 個まで）
data Player = Player { pK :: !Int, pMatcher :: !Bool, pCnt :: !(U.Vector Double), pOpp :: [Int] }

-- 対戦の記録（どれも新しい順、600 個まで）
data Game = Game { gA, gB, gHitA, gHitB :: [Int], gWinA, gN :: !Int }

data World = World
  { wKA, wKB  :: !Int          -- ★ 何手前まで覚えて予測するか（0〜6）
  , wA, wB    :: !Player
  , wRng      :: !Rng
  , wGame     :: !Game
  , wHuman    :: !Bool
  , wSpI      :: !Int
  , wLast     :: String
  , wTest     :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "51-12 Mutual observers" world0)
  { skBack  = col (24, 35, 10) 100
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = resetAll (World 3 1 (makePlayer 3 True) (makePlayer 1 False) (makeRng 12) game0 False 1 "" [])

game0 :: Game
game0 = Game [] [] [] [] 0 0

resetAll :: World -> World
resetAll w = w { wRng = makeRng 12, wA = makePlayer (wKA w) True, wB = makePlayer (wKB w) False, wGame = game0 }

-- ---- 予測する人：相手の直近 k 手の並び（文脈）ごとに、次に 0 / 1 が来た回数を数える
makePlayer :: Int -> Bool -> Player
makePlayer k m = Player k m (U.replicate ((1 `shiftL` k) * 2) 0) []

ctx :: Player -> Int
ctx p = foldl' (\c i -> c * 2 + getAt 0 (pOpp p) i) 0 [0 .. pK p - 1]

probOne :: Player -> Double
probOne p
  | length (pOpp p) < pK p = 0.5
  | otherwise = let c = ctx p
                    n0 = pCnt p U.! (2 * c)
                    n1 = pCnt p U.! (2 * c + 1)
                in (n1 + 0.5) / (n0 + n1 + 1)

-- 自信がない（確率が 0.5 ± sure の中）ときは、コインを投げて決める
predict :: Player -> Rng -> (Int, Rng)
predict p r
  | abs (q - 0.5) < sure = let (v, r') = nextR r in (if v < 0.5 then 1 else 0, r')
  | otherwise = (if q > 0.5 then 1 else 0, r)
  where q = probOne p

-- (予想, 手)
chooseMove :: Player -> Rng -> ((Int, Int), Rng)
chooseMove p r = let (g, r') = predict p r in ((g, if pMatcher p then g else 1 - g), r')

learn :: Player -> Int -> Player
learn p oppMove = p { pCnt = cnt', pOpp = take 64 (oppMove : pOpp p) }
  where
    cnt' | length (pOpp p) >= pK p = let c = U.map (* forget) (pCnt p)
                                         i = 2 * ctx p + oppMove
                                     in c U.// [(i, c U.! i + 1)]
         | otherwise = pCnt p

-- ---- エントロピー（ビット）：1 手ずつの H1 と、m 手前までを知ったときの条件つき H(x | 前 m 手)
entropy1 :: [Int] -> Double
entropy1 [] = 0
entropy1 xs = let n = fromIntegral (length xs)
                  p = fromIntegral (sum xs) / n
              in if p <= 0 || p >= 1 then 0 else -(p * logBase 2 p + (1 - p) * logBase 2 (1 - p))

-- xs は古い順
condEntropy :: [Int] -> Int -> Double
condEntropy xs m = if n > 0 then h else 0
  where
    v = U.fromList xs
    keys = [ (U.toList (U.slice (i - m) m v), v U.! i) | i <- [m .. U.length v - 1] ]
    tab = M.fromListWith (\(a1, b1) (a2, b2) -> (a1 + a2, b1 + b2)) [ (k, if x == 0 then (1, 0) else (0, 1)) | (k, x) <- keys ] :: M.Map [Int] (Int, Int)
    n = length keys
    h = sum [ -(fromIntegral c / fromIntegral n) * logBase 2 (fromIntegral c / fromIntegral (a + b))
            | (a, b) <- M.elems tab, c <- [a, b], c > 0 ]

-- 1 回の対戦（moveA = Just ならあなたの手）
playRound :: Maybe Int -> World -> World
playRound moveA w = w { wA = learn (wA w) mb, wB = learn (wB w) ma, wRng = r2
                      , wGame = g { gA = cap (ma : gA g), gB = cap (mb : gB g)
                                  , gHitA = cap ((if ga == mb then 1 else 0) : gHitA g)
                                  , gHitB = cap ((if gbG == ma then 1 else 0) : gHitB g)
                                  , gWinA = gWinA g + (if ma == mb then 1 else 0), gN = gN g + 1 } }
  where
    ((ga, mvA), r1) = chooseMove (wA w) (wRng w)
    ((gbG, mb), r2) = chooseMove (wB w) r1
    ma = maybe mvA id moveA
    g = wGame w
    cap = take 600

-- 直近 win 手の平均
rate :: [Int] -> Double
rate xs = let s = take win xs in if null s then 0 else fromIntegral (sum s) / fromIntegral (length s)

step :: Env -> Float -> World -> IO World
step env _ w
  | wHuman w = pure w
  | wSpI w == 0 = pure (if envFrame env `mod` 20 == 0 then playRound Nothing w else w)
  | otherwise = pure (iterate (playRound Nothing) w !! getAt 1 speeds (wSpI w))

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Mutual observers: predicting the one who predicts you" 20 36 20 cTxt
  , say (if wHuman w
           then "You (A: win on same face) vs predictor B   Left = heads(0)  Right = tails(1)   H auto   R reset"
           else "Up/Down A memory " ++ show (wKA w) ++ "   Left/Right B memory " ++ show (wKB w) ++ "   H you play A   Space speed x"
                ++ show (getAt 1 speeds (wSpI w)) ++ "   R reset") 20 62 15 cSub
    -- ---- 手の列（上 = A、下 = B）
  , say "A's moves (A wins on the same face)" x0 y0 14 cTube
  , say "B's moves" x0 (y0 + 76) 14 cScope ]
  ++ concat [ [ Color (if a == 1 then col cTube 100 else hsb 24 40 30) (rectTL (x0 + fi * cw) (y0 + 10) (cw - 1) 36)
              , Color (if b == 1 then col cScope 100 else hsb 135 40 25) (rectTL (x0 + fi * cw) (y0 + 80) (cw - 1) 36) ]
              ++ [ Color (col cBrass 100) (rectTL (x0 + fi * cw) (y0 + 52) (cw - 1) 8) | a == b ]
            | (fi, a, b) <- zip3 [0 ..] sa sb ]
  ++ [ say "yellow = same face (A wins)" x0 (y0 + 140) 14 cBrass ]
    -- ---- 成績
  ++ concat [ [ say c (mx + (if k > 0 then 330 + fromIntegral (k - 1) * 200 else 0)) (my + fromIntegral i * 30) (if i > 0 then 16 else 14)
                    (if k == 1 then cTube else if k == 2 then cScope else cTxt)
              | (k, c) <- zip [0 :: Int ..] r ]
            | (i, r) <- zip [0 :: Int ..] rows ]
  ++ [ say ("Games " ++ show (gN g) ++ "   A wins " ++ show (gWinA g) ++ " ("
            ++ (if gN g > 0 then fmt 1 (100 * fromIntegral (gWinA g) / fromIntegral (gN g)) ++ "%" else "-") ++ ")") mx (my + 136) 17 cTxt ]
  ++ [ say (wLast w) mx (my + 166) 16 cBrass | wHuman w ]
  ++ [ say "Read the opponent and you win -> the one who is read" mx (my + 196) 14 cSub
     , say "changes -> moves drift toward unreadable (random, 1 bit)" mx (my + 216) 14 cSub ]
    -- ---- 的中率のグラフ
  ++ [ plotFrame gx gy gwid ght "Hit rate (mean of 20, 0 to 1, dashed = 0.5)"
     , hLine 0.5 gx gy gwid ght 0 1 cBrass ]
  ++ [ plotLine (roll (gHitA g)) gx gy gwid ght 0 1 cTube 581 1.8 | not (wHuman w) ]
  ++ [ plotLine (roll (gHitB g)) gx gy gwid ght 0 1 cScope 581 1.8
     , drawSelfTest env (wTest w) ]
  where
    g = wGame w
    x0 = 30
    y0 = 100
    cw = max 4 ((envW env - 60) / 120)
    sa = reverse (take 120 (gA g))
    sb = reverse (take 120 (gB g))
    mx = 30
    my = y0 + 190
    lastW xs = reverse (take win xs)
    rows = [ ["", "A (" ++ (if wHuman w then "you" else "memory " ++ show (wKA w)) ++ ")", "B (memory " ++ show (wKB w) ++ ")"]
           , ["Hit rate (last " ++ show win ++ ")", if wHuman w then "-" else fmt 1 (100 * rate (gHitA g)) ++ "%", fmt 1 (100 * rate (gHitB g)) ++ "%"]
           , ["Entropy H1 (bits)", fmt 3 (entropy1 (lastW (gA g))), fmt 3 (entropy1 (lastW (gB g)))]
           , ["H given previous 2", fmt 3 (condEntropy (lastW (gA g)) 2), fmt 3 (condEntropy (lastW (gB g)) 2)] ]
    gx = envW env * 0.62
    gy = my - 10
    gwid = envW env - gx - 30
    ght = envH env - gy - 50
    -- 20 手の移動平均（古い順）
    roll xs = let v = U.fromList (reverse xs)
                  cs = U.scanl' (+) 0 (U.map fromIntegral v) :: U.Vector Double
              in [ (cs U.! i - cs U.! (i - 20)) / 20 | i <- [20 .. U.length v] ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey k) Down _ _ | wHuman w && (k == KeyLeft || k == KeyRight) ->
    let m = if k == KeyLeft then 0 else 1
        (bGuess, r') = predict (wB w) (wRng w)
        w' = playRound (Just m) w { wRng = r' }
        bm = getAt 0 (gB (wGame w')) 0
        face x = if x == 1 then "tails" else "heads"
    in w' { wLast = "You " ++ face m ++ ", B " ++ face bm ++ " (B guessed " ++ face bGuess ++ ")" ++ (if m == bm then "   you win" else "   B wins") }
  EventKey (SpecialKey KeyUp) Down _ _ -> resetAll w { wKA = min 6 (wKA w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> resetAll w { wKA = max 0 (wKA w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> resetAll w { wKB = min 6 (wKB w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetAll w { wKB = max 0 (wKB w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "hH" -> resetAll w { wHuman = not (wHuman w) }
    | c `elem` "rR" -> resetAll w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wSpI = (wSpI w + 1) `mod` length speeds }
  _ -> w

-- ---- 自己テスト：決まった相手に対する予測（p5.js 版と同じ 5 項目）
-- 相手の手の列 xs に対して、後半の的中率
accVs :: Int -> [Int] -> Double
accVs k xs = fromIntegral hit / fromIntegral (n `div` 2)
  where
    n = length xs
    (_, _, hit) = foldl' go (makePlayer k True, makeRng 3, 0 :: Int) (zip [0 ..] xs)
    go (!p, !r, !h) (t, x) = let (g, r') = predict p r
                             in (learn p x, r', if t >= n `div` 2 && g == x then h + 1 else h)

selfTest :: [(String, Bool)]
selfTest =
  [ ("alternating 0101...: memory 1 hits " ++ fmt 0 (100 * aAlt) ++ "% (> 95%)", aAlt > 0.95)
  , ("001001...: memory 2 " ++ fmt 0 (100 * a2) ++ "% beats memory 0 " ++ fmt 0 (100 * a0) ++ "% (needs 2 back to see the period)", a2 > 0.95 && a0 < 0.7)
  , ("random opponent: any predictor ~50% (" ++ fmt 1 (100 * ar) ++ "%, within 3%)", abs (ar - 0.5) < 0.03)
  , ("entropy: constant 0 bit; 0101... has H1 = 1 bit but 0 given previous 2"
    , near (entropy1 [1, 1, 1, 1]) 0 1e-9 && near (entropy1 [0, 1, 0, 1]) 1 1e-9 && near (condEntropy [0, 1, 0, 1, 0, 1, 0, 1] 2) 0 1e-9)
  , ("two predictors, 3000 games: A's moves hard to read even given previous 2 (H = " ++ fmt 3 h ++ " > 0.8 bit)", h > 0.8) ]
  where
    aAlt = accVs 1 [ t `mod` 2 | t <- [0 .. 399] ]
    per3 = [ if t `mod` 3 == 2 then 1 else 0 | t <- [0 .. 599 :: Int] ]
    a0 = accVs 0 per3
    a2 = accVs 2 per3
    ar = accVs 3 [ if v < 0.5 then 1 else 0 | v <- take 8000 (rngList (makeRng 99)) ]
    (_, _, _, xsRev) = foldl' play (makePlayer 3 True, makePlayer 1 False, makeRng 5, []) [1 .. 3000 :: Int]
    play (!pa, !pb, !r, acc) _ = let ((_, a), r1) = chooseMove pa r
                                     ((_, b), r2) = chooseMove pb r1
                                 in (learn pa b, learn pb a, r2, a : acc)
    h = condEntropy (reverse (take 1000 xsRev)) 2

-- ---------- やってみよう ----------
-- 1. H を押して、あなたが A になろう。B（予測器）に何 % 読まれる？ 50% より読まれたら、あなたの手にはくせがある
-- 2. 記憶の長い方が勝つ？ ↑↓ ←→ で A と B の記憶を変えて、勝率を比べよう
-- 3. 予測器どうしだと、手の列のエントロピーはどこへ行く？「読まれないこと」が最善になる理由は？
-- 4. forget を 1（忘れない）にすると、ふるまいはどう変わる？
-- 5. 「観察する人も観察される系の中にいる」例を身のまわりで探そう（テスト対策、流行、株価…）

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

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ---- 決まった種（seed）から毎回同じ乱数列（0〜1）を出す（p5.js 版の makeRng と同じ mulberry32）
newtype Rng = Rng Word32

makeRng :: Word32 -> Rng
makeRng = Rng

nextR :: Rng -> (Double, Rng)
nextR (Rng a0) = (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)
  where
    a = a0 + 0x6d2b79f5
    t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
    t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))

rngList :: Rng -> [Double]
rngList r = let (v, r') = nextR r in v : rngList r'

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
