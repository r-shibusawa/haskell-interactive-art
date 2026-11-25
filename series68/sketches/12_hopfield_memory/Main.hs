-- ============================================
-- 68-12 ホップフィールド網 ─ 10 × 10 = 100 個のニューロン（+1 = 白、−1 = 黒）が全部たがいにつながった連想記憶
-- 覚える：ヘッブ則 W_ij = Σ_μ ξ_i^μ ξ_j^μ（いっしょに白・黒になる 2 つはつながりを強く）、自分自身とはつながない W_ii = 0
-- 思い出す：ニューロンを 1 つずつ選び、s_i ← sign(Σ_j W_ij s_j)。エネルギー E = −½ sᵀ W s は 1 回ごとに必ず下がるか同じ
--   → 谷（覚えた絵）に転がり落ちる。ノイズで汚した絵から元の絵を思い出せる
-- 覚える数が多すぎると（ランダムな絵なら 約 0.14 × 100 = 14 枚）、思い出せなくなり、まざった「偽の記憶」に落ちる
-- 操作：クリック 覚えた絵を汚して入れる / ドラッグ 状態に描く（Shift で黒）/ ↑↓ 覚える絵の数 / N ノイズを足す
--       スペース 一時停止 / M 容量の実験 ⇔ 絵 / R 状態をランダムに
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (violet)
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when, unless)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR, xor, (.|.), (.&.), shiftL, testBit)
import Data.List (foldl', intercalate, sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (floatToDigits)
import System.Environment (lookupEnv)

-- ★ 覚える絵（# = 白）。自分の絵に書きかえてよい（名前は画面に出すので英語）
pics :: [(String, [String])]
pics =
  [ ("Heart", ["..........", ".##...##..", "####.####.", "#########.", "#########.", ".#######..", "..#####...", "...###....", "....#.....", ".........."])
  , ("House", ["....##....", "...####...", "..######..", ".########.", "##########", ".#......#.", ".#.##...#.", ".#.##.###.", ".#....###.", ".########."])
  , ("Face", ["..######..", ".#......#.", "#..#..#..#", "#........#", "#........#", "#.#....#.#", "#..####..#", "#........#", ".#......#.", "..######.."])
  , ("Cross", ["....##....", "....##....", "....##....", "##########", "##########", "....##....", "....##....", "....##....", "....##....", "....##...."])
  , ("Moon", ["...####...", "..##......", ".##.......", "##........", "##........", "##........", "##........", ".##.......", "..##......", "...####..."])
  , ("Stripes", replicate 10 "#.#.#.#.#.") ]

nn :: Int
nn = 100                   -- ニューロンの数

noiseFrac :: Double
noiseFrac = 0.2            -- ★ 汚す割合

updatesPerFrame :: Int
updatesPerFrame = 3        -- ★ 1 フレームの更新数

type State = U.Vector Int  -- +1 / −1 が 100 個
type Weights = U.Vector Int   -- W_ij を i 行 j 列の順に（整数）

-- 容量の実験の 1 点：重なりの平均と、固定点のままの割合
data CapPt = CapPt { cpM :: !Double, cpFixed :: !Double }

data World = World
  { wStored  :: !Int
  , wW       :: !Weights
  , wS       :: !State
  , wELog    :: ![Int]          -- エネルギーの記録（新しい順、1500 個まで）
  , wPaused  :: !Bool
  , wCap     :: !Bool           -- 容量の実験の画面
  , wCurve   :: ![CapPt]        -- 容量の曲線（P = 1, 2, …）
  , wLastI   :: !Int
  , wStable  :: !Int
  , wSeedH   :: !Int
  , wShift   :: !Bool
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "68-12 Hopfield memory" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = corruptFrom 0 (learnPics (World 3 U.empty (U.replicate nn 1) [] False False [] (-1) 0 1 False (rngOf 1) []))

-- 乱数を引く計算を、World の乱数の列で実行する
runW :: R a -> World -> (a, World)
runW m w = let (a, g) = runR m (wRng w) in (a, w { wRng = g })

picVec :: Int -> State
picVec k = U.fromList [ if ch == '#' then 1 else -1 | r <- snd (at' pics k ("", [])), ch <- r ]

-- ★ ヘッブ則：W_ij = Σ_μ ξ_i ξ_j（i ≠ j）。整数のまま（1/N で割らない）なので、エネルギーの比較がぴったり
hebbian :: [State] -> Weights
hebbian pats = U.generate (nn * nn) (\ij -> let (i, j) = ij `quotRem` nn
                                            in if i == j then 0 else foldl' (\s p -> s + (p U.! i) * (p U.! j)) 0 pats)

localField :: Weights -> State -> Int -> Int
localField w s i = go 0 0
  where o = i * nn
        go !j !h = if j >= nn then h else go (j + 1) (h + U.unsafeIndex w (o + j) * U.unsafeIndex s j)

energyOf :: Weights -> State -> Int
energyOf w s = negate (U.ifoldl' (\e i si -> e + si * localField w s i) 0 s) `quot` 2

-- ★ 1 個の更新：s_i ← sign(h_i)（h_i = 0 ならそのまま）。変わったら ΔE = −2 s_i(新) h_i ≤ 0
updateOne :: Weights -> State -> Int -> (State, Bool)
updateOne w s i = let h = localField w s i
                      old = s U.! i
                      nv = if h > 0 then 1 else if h < 0 then -1 else old
                  in if nv /= old then (s U.// [(i, nv)], True) else (s, False)

-- 番号の列の順に 1 つずつ更新（変わった数も返す）
sweepOrder :: Weights -> State -> U.Vector Int -> (State, Int)
sweepOrder w s0 order = runST $ do
  s <- U.thaw s0
  let go !q !ch
        | q >= U.length order = pure ch
        | otherwise = do
            let i = order U.! q
            let o = i * nn
                field !j !acc = if j >= nn then pure acc else do
                  sj <- MU.unsafeRead s j
                  field (j + 1) (acc + U.unsafeIndex w (o + j) * sj)
            h <- field 0 0
            old <- MU.read s i
            let nv = if h > 0 then 1 else if h < 0 then -1 else old
            MU.write s i nv
            go (q + 1) (if nv /= old then ch + 1 else ch)
  ch <- go 0 0
  s' <- U.freeze s
  pure (s', ch)

-- ランダムな順番で全部のニューロンを更新、変化がなくなるまで（順番の列は前の回をさらにシャッフルする）
recall :: Weights -> State -> Int -> R State
recall w s0 maxSweeps = go 0 s0 (U.enumFromN 0 nn)
  where
    go sw s order
      | sw >= maxSweeps = pure s
      | otherwise = do
          order' <- shuffleU order
          let (s', ch) = sweepOrder w s order'
          if ch == 0 then pure s' else go (sw + 1) s' order'

overlap :: State -> State -> Double
overlap a b = i2d (U.sum (U.zipWith (*) a b)) / i2d nn

randomPattern :: R State
randomPattern = U.map (\u -> if u < 0.5 then 1 else -1) <$> rndVec nn

flipSome :: Double -> State -> R State
flipSome fr s = do
  us <- rndVec (U.length s)
  pure (U.zipWith (\v u -> if u < fr then negate v else v) s us)

learnPics :: World -> World
learnPics w = w { wW = hebbian (map picVec [0 .. wStored w - 1]) }

resetE :: World -> World
resetE w = w { wELog = [energyOf (wW w) (wS w)] }

pushE :: World -> World
pushE w = w { wELog = take 1500 (energyOf (wW w) (wS w) : wELog w) }

corruptFrom :: Int -> World -> World
corruptFrom k w0 =
  let w = w0 { wRng = rngOf (1200 + wSeedH w0), wSeedH = wSeedH w0 + 1 }
      (s, w1) = runW (flipSome noiseFrac (picVec k)) w
  in resetE w1 { wS = s, wStable = 0 }

-- 容量の実験（ランダムな絵）：P 枚覚えて、それぞれ 10% 汚してから思い出したときの重なり（1 = 完全）の平均
capPoint :: Int -> Int -> R CapPt
capPoint p seed = do
  seedRng seed
  pats <- mapM (const randomPattern) [1 .. p]
  let w = hebbian pats
  rs <- mapM (\pk -> do noisy <- flipSome 0.1 pk
                        out <- recall w noisy 20
                        let (_, moved) = sweepOrder w pk (U.enumFromN 0 nn)
                        pure (overlap out pk, if moved == 0 then 1 else 0 :: Int)) pats
  pure (CapPt (sumOf (map fst rs) / i2d p) (i2d (sum (map snd rs)) / i2d p))

step :: Env -> Float -> World -> IO World
step _ _ w0 = pure w2
  where
    w1 = if not (wPaused w0) && not (wCap w0) then foldl' (\w _ -> one w) w0 [1 .. updatesPerFrame] else w0
    one w = let (u, wa) = runW rnd w
                i = floor (u * i2d nn)
                (s, ch) = updateOne (wW wa) (wS wa) i
            in pushE wa { wS = s, wLastI = i, wStable = if ch then 0 else wStable wa + 1 }
    w2 = if wCap w1 && length (wCurve w1) < 40
           then let p = length (wCurve w1) + 1
                    (c, w') = runW (capPoint p (3000 + p)) w1
                in w' { wCurve = wCurve w1 ++ [c] }
           else w1

cellSize :: Env -> Float
cellSize env = min 36 ((envH env - 300) / 10)

thumbX :: Int -> Float
thumbX k = 40 + i2f k * (7 * 10 + 56)

draw :: Env -> World -> Picture
draw env w
  | wCap w = view env [drawCapacity env w, drawSelfTest env (wTest w)]
  | otherwise = view env $
  [ hudTitle ("Hopfield net: " ++ show (wStored w) ++ " stored pictures (100 neurons)   energy E = " ++ show (headOr 0 (wELog w)) ++ (if wStable w > 300 then "  -> stopped (bottom of a valley)" else ""))
             ("click a picture below: put it in with noise   drag: draw on the state (Shift = black)   Up/Down number stored   N noise\nSpace " ++ (if wPaused w then "resume" else "pause") ++ "   M capacity experiment   R random state")
  -- ---- 左：いまの状態
  , Pictures [ Color (if (wS w U.! (j * 10 + i)) > 0 then col ink else col (26, 48, 50)) (rectTL (gx + i2f i * cs) (gy + i2f j * cs) (cs - 2) (cs - 2)) | j <- [0 .. 9], i <- [0 .. 9] ]
  , if wLastI w >= 0 then Color (col mag) (frameTL 2 (gx + i2f (wLastI w `mod` 10) * cs - 1) (gy + i2f (wLastI w `div` 10) * cs - 1) cs cs) else Blank
  , say "state s (magenta frame = neuron just updated)" gx (gy + 10 * cs + 20) 14 (col sub)
  -- ---- 下：覚えた絵と重なり
  , Pictures [ let x0 = thumbX k
                   on = k < wStored w
                   v = picVec k
               in Pictures [ Pictures [ Color (col (if v U.! q > 0 then (if on then ink else (90, 100, 100)) else (20, 38, 40))) (rectTL (x0 + i2f (q `mod` 10) * ts) (ty + i2f (q `div` 10) * ts) (ts - 1) (ts - 1)) | q <- [0 .. nn - 1] ]
                           , say (fst (at' pics k ("", []))) x0 (ty + 10 * ts + 16) 14 (col (if on then (if k == best then lime else ink) else sub))
                           , say (if on then "m = " ++ fmtN (at' ov k 0) 2 else "(not stored)") x0 (ty + 10 * ts + 34) 14 (col (if on then (if k == best then lime else ink) else sub)) ]
             | k <- [0 .. length pics - 1] ]
  -- ---- 右：エネルギー
  , plotAxes g "updates" "energy E = -1/2 s'Ws (goes down or stays, never up)" [0, grX1 g] [i2d (jsRound (grY0 g)), i2d (jsRound (grY1 g))]
  , plotXY g (map i2d [0 .. length es - 1]) (map i2d es) (col amber) 2
  , Pictures [ let e = i2d (energyOf (wW w) (picVec k)) in Color (colA 120 lime) (Line [(grX g, gY g e), (grX g + grW g, gY g e)]) | k <- [0 .. wStored w - 1] ]
  , say "green lines = energy of each stored picture (valley floors). m = overlap (1 same, -1 inverted)" (rx + 20) by 14 (col sub)
  , say "The inverted picture has the same energy -> it can fall into the inverse too (Hebb can't tell xi from -xi)" (rx + 20) (by + 20) 14 (col sub)
  , say "Hand-drawn pictures look alike (much black), so they mix sooner than random ones. M: capacity" (rx + 20) (by + 40) 14 (col sub)
  , drawSelfTest env (wTest w) ]
  where
    cs = cellSize env
    gx = 40
    gy = 100
    ty = gy + 10 * cs + 50
    ts = 7
    ov = [ overlap (wS w) (picVec k) | k <- [0 .. wStored w - 1] ]
    best = argMax ov
    es = reverse (wELog w)
    rx = gx + 10 * cs + 50
    g = Graph (rx + 20) 110 (envW env - rx - 70) ((envH env - 260) * 0.55) 0 (max 10 (i2d (length es - 1)))
              (i2d (minOr 0 es) - 20) (i2d (maxOr 0 es) + 20)
    by = grY g + grH g + 50

drawCapacity :: Env -> World -> Picture
drawCapacity env w = Pictures
  [ hudTitle "Hopfield capacity: store P random pictures, add 10% noise, recall (N = 100)"
             ("M back to the pictures   (P = 1..40, computed " ++ show (length cur) ++ " / 40)")
  , plotAxes g "stored / neurons = alpha = P/N" "mean overlap after recall (1 = perfect), and share of stored pictures that stay fixed" [0, 0.1, 0.138, 0.2, 0.3, 0.4] [0, 0.5, 1]
  , Color (col amber) (Line [(gX g 0.138, grY g), (gX g 0.138, grY g + grH g)])
  , say "theory: alpha ~ 0.138 (for large N)" (gX g 0.138 + 6) (grY g + 18) 14 (col amber)
  , plotXY g as (map cpM cur) (col mag) 3
  , plotXY g as (map cpFixed cur) (col sky) 2
  , legend (grX g + grW g - 300) (grY g + 30) [(mag, "mean overlap after recall"), (sky, "stored picture is still a fixed point")]
  , say "N = 100 is small, so the sharp drop is smoothed. For large alpha even the stored pictures move (to false memories)." (grX g) (grY g + grH g + 50) 14 (col sub) ]
  where
    cur = wCurve w
    g = Graph 90 110 (envW env - 180) (envH env - 280) 0 0.4 0 1.05
    as = [ i2d i / i2d nn | i <- [1 .. length cur] ]

-- 状態のマスに描く（Shift で黒）
paintState :: Env -> (Float, Float) -> World -> World
paintState env (mx, my) w
  | wCap w = w
  | otherwise =
      let cs = cellSize env
          i = floor ((mx - 40) / cs) :: Int
          j = floor ((my - 100) / cs) :: Int
      in if i >= 0 && i < 10 && j >= 0 && j < 10
           then pushE w { wS = wS w U.// [(j * 10 + i, if wShift w then -1 else 1)], wStable = 0 }
           else w

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down mods pos ->
    let (mx, my) = toP5 env pos
        cs = cellSize env
        ty = 100 + 10 * cs + 50
        ws = w { wShift = shift mods == Down || shiftHeld }
        hit = [ k | k <- [0 .. length pics - 1], let x0 = thumbX k, mx > x0, mx < x0 + 70, my > ty, my < ty + 70, k < wStored w ]
        w1 = foldl' (\a k -> corruptFrom k a) ws hit
    in paintState env (mx, my) w1
  EventMotion pos | envDown env -> paintState env (toP5 env pos) w { wShift = wShift w || shiftHeld }
  EventKey (SpecialKey KeyUp) Down _ _ -> resetE (learnPics w { wStored = min (length pics) (wStored w + 1) })
  EventKey (SpecialKey KeyDown) Down _ _ -> resetE (learnPics w { wStored = max 1 (wStored w - 1) })
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "nN" -> let (s, w1) = runW (flipSome 0.1 (wS w)) w in pushE w1 { wS = s, wStable = 0 }
    | c `elem` "mM" -> w { wCap = not (wCap w) }
    | c `elem` "rR" -> let (s, w1) = runW randomPattern w in resetE w1 { wS = s, wStable = 0 }
  _ -> w
  where shiftHeld = isDown env (SpecialKey KeyShiftL) || isDown env (SpecialKey KeyShiftR)

-- ---------- やってみよう ----------
-- 1. 覚える数 1 → 6 と増やしながら、汚した「顔」を思い出せるか試す。どこから混ざる？
-- 2. R でランダムな状態から始めると、どの絵（または反転した絵）に落ちる？
-- 3. noiseFrac を 0.4、0.5 にする。半分汚すとどうなる？
-- 4. pics に自分の 10 × 10 の絵を足す。似ていない絵どうしのほうが、たくさん覚えられる？

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("ヘッブ則の W は対称で、対角が 0（整数）", sym)
  , ("1 個ずつの更新 3000 回：エネルギーは一度も上がらない", mono)
  , ("ΔE = −(s_i の変化) × h_i が毎回ぴったり", dOk)
  , ("ランダムな絵 5 枚（α = 0.05）：覚えた絵は全部そのまま止まる（固定点）", all (\p -> overlap (fst (sweepOrder w p (U.enumFromN 0 nn))) p == 1) pats)
  , ("3 枚覚えて 15% 汚した絵から、元の絵を完全に思い出す", ok3)
  , ("容量：5 枚（α = 0.05）なら重なり " ++ fmtN (cpM lo) 3 ++ "、30 枚（α = 0.3）では " ++ fmtN (cpM hi) 2 ++ " に落ちる", cpM lo > 0.99 && cpM hi < 0.8) ]
  where
    ((pats, mono, dOk, ok3, lo, hi), _) = flip runR (rngOf 1212) $ do
      ps <- mapM (const randomPattern) [1 .. 5 :: Int]
      let wt = hebbian ps
      s0 <- randomPattern
      let loop t s e mn dk
            | t >= (3000 :: Int) = pure (mn, dk)
            | otherwise = do
                u <- rnd
                let i = floor (u * i2d nn)
                    h = localField wt s i
                    old = s U.! i
                    (s', _) = updateOne wt s i
                    e2 = energyOf wt s'
                loop (t + 1) s' e2 (mn && not (e2 > e)) (dk && e2 - e == negate ((s' U.! i) - old) * h)
      (mn, dk) <- loop 0 s0 (energyOf wt s0) True True
      let w3 = hebbian (take 3 ps)
      oks <- mapM (\p -> do noisy <- flipSome 0.15 p
                            out <- recall w3 noisy 30
                            pure (overlap out p == 1)) (take 3 ps)
      l <- capPoint 5 3005
      hh <- capPoint 30 3030
      pure (ps, mn, dk, and oks, l, hh)
    w = hebbian pats
    sym = and [ w U.! (i * nn + i) == 0 && and [ w U.! (i * nn + j) == w U.! (j * nn + i) | j <- [0 .. nn - 1] ] | i <- [0 .. nn - 1] ]

-- ==================================================
-- ここから下は「神経回路網の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。座標は p5.js 版と同じ（左上が原点、y は下向き）で、view で gloss の座標に直す
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（神経細胞：深い青緑の黒、あたたかい白、シナプスのマゼンタ、軸索のライム）
type RGB = (Double, Double, Double)
type Vec = U.Vector Double                    -- ベクトル（unboxed）

bgCol, ink, mag, lime, amber, sky, violet, sub, panel, wire :: RGB
bgCol = (8, 22, 24)                           -- 背景（深い青緑の黒）
ink = (246, 238, 224)                         -- 文字（あたたかい白）
mag = (232, 72, 160)
lime = (184, 232, 76)
amber = (250, 186, 72)
sky = (96, 196, 236)
violet = (168, 140, 250)
sub = (150, 178, 172)
panel = (16, 38, 40)
wire = (44, 76, 76)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Double -> RGB -> Color                -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Double -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

clampI :: Int -> Int -> Int -> Int
clampI v a b = max a (min b v)

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

f2 :: Double -> Float                         -- 描くときの Float（NaN と無限大は 0）
f2 v = if isNaN v || isInfinite v then 0 else realToFrac v

d2 :: Float -> Double
d2 = realToFrac

isFin :: Double -> Bool
isFin v = not (isNaN v || isInfinite v)

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

sumOf :: [Double] -> Double
sumOf = foldl' (+) 0

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else sumOf a / i2d (length a)

sigm :: Double -> Double
sigm z = 1 / (1 + exp (-(clampv z (-40) 40)))

-- JavaScript の Math.round（.5 は大きいほうへ）と Math.floor
jsRound :: Double -> Int
jsRound v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e15) 1e15)

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- 最初にいちばん大きい値の番号（indexOf(Math.max(...))）
argMax :: [Double] -> Int
argMax xs = fst (foldl' (\(bi, bv) (i, v) -> if v > bv then (i, v) else (bi, bv)) (0, headOr 0 xs) (zip [0 ..] xs))

-- 数を書く（p5.js 版の fmtN：toFixed(d)、"-0.00" は "0.00" に）
fmtN :: Double -> Int -> String
fmtN v d
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = (if v < 0 && n > 0 then "-" else "") ++ body
  where
    n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
    ds = show n
    padded = replicate (d + 1 - length ds) '0' ++ ds
    (ip, fp) = splitAt (length padded - d) padded
    body = if d > 0 then ip ++ "." ++ fp else ip

-- p5.js 版の fmtE：toExponential(1)（例 6.7e-9）。0 ちょうどは "0"
fmtE :: Double -> String
fmtE v
  | isNaN v || isInfinite v = "?"
  | v == 0 = "0"
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          pick e = (floor (a / (10 ^^ (e - 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= 100 then pick (e0 + 1) else if n < 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= 100 then (n1 `div` 10, e1 + 1) else (n1, e1)
          ds = show n2
      in (if v < 0 then "-" else "") ++ take 1 ds ++ "." ++ drop 1 ds ++ "e" ++ (if e2 >= 0 then "+" else "-") ++ show (abs e2)

-- JavaScript の数の書き方（String(v)：いちばん短い 10 進数）
jsNum :: Double -> String
jsNum v
  | isNaN v = "NaN"
  | isInfinite v = if v > 0 then "Infinity" else "-Infinity"
  | v == 0 = "0"
  | v < 0 = '-' : jsNum (negate v)
  | otherwise =
      let (ds0, e) = floatToDigits 10 v
          ds = concatMap show ds0
          k = length ds
      in if e >= k && e <= 21 then ds ++ replicate (e - k) '0'
         else if e > 0 && e <= 21 then take e ds ++ "." ++ drop e ds
         else if e > -6 && e <= 0 then "0." ++ replicate (negate e) '0' ++ ds
         else take 1 ds ++ (if k > 1 then "." ++ drop 1 ds else "") ++ "e" ++ (if e - 1 >= 0 then "+" else "-") ++ show (abs (e - 1))

-- +v.toPrecision(k)（有効数字 k 桁に丸めた数）と、その書き方（目盛りの数）
roundPrec :: Int -> Double -> Double
roundPrec k v
  | not (isFin v) || v == 0 = 0
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          top = 10 ^ k :: Integer
          pick e = (floor (a / (10 ^^ (e - k + 1)) + 1 / 2) :: Integer, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= top then pick (e0 + 1) else if n < top `div` 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= top then (n1 `div` 10, e1 + 1) else (n1, e1)
          r = fromRational (fromIntegral n2 * 10 ^^ (e2 - k + 1)) :: Double
      in if v < 0 then negate r else r

precN :: Int -> Double -> String
precN k = jsNum . roundPrec k

prec3 :: Double -> String
prec3 = precN 3

-- 左上のタイトルと操作の説明（説明は "\n" で 2 行にできる）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col sub) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- mulberry32 は状態に毎回同じ数 0x6d2b79f5 を足すだけ → k 回目の乱数は「状態 + k × その数」から直接作れる
rndOut :: Word32 -> Double
rndOut a =
  let t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296

rngStep :: Word32
rngStep = 0x6d2b79f5

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：列を種 s から始め直す（0 なら 1）
seedRng :: Int -> R ()
seedRng s = R (\_ -> let w = fromIntegral s :: Word32 in ((), Rng (if w == 0 then 1 else w)))

rngOf :: Int -> Rng
rngOf s = snd (runR (seedRng s) (Rng 1))

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double                               -- 0〜1 の一様乱数
rnd = R (\(Rng s) -> let a = s + rngStep in (rndOut a, Rng a))

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * i2d (b - a + 1)))

-- 標準正規分布（ボックス-ミュラー、u = 1 − rnd() なので log(0) にならない）
randn :: R Double
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- rnd() を n 回、まとめて（unboxed のベクトルに）
rndVec :: Int -> R (U.Vector Double)
rndVec n = R (\(Rng s) -> (U.generate n (\k -> rndOut (s + fromIntegral (k + 1) * rngStep)), Rng (s + fromIntegral n * rngStep)))

-- randn() を n 回、まとめて
randnVec :: Int -> R (U.Vector Double)
randnVec n = R (\(Rng s) -> (U.generate n (gauss s), Rng (s + fromIntegral (2 * n) * rngStep)))
  where gauss s k = let u = 1 - rndOut (s + fromIntegral (2 * k + 1) * rngStep)
                        v = rndOut (s + fromIntegral (2 * k + 2) * rngStep)
                    in sqrt (-2 * log u) * cos (2 * pi * v)

-- フィッシャー-イェーツのシャッフル（後ろから i と 0〜i のどこかを入れかえる）
shuffleU :: U.Unbox a => U.Vector a -> R (U.Vector a)
shuffleU v = do
  let n = U.length v
  js <- mapM (\i -> do u <- rnd; pure (i, floor (u * i2d (i + 1)))) [n - 1, n - 2 .. 1]
  pure (U.modify (\mv -> mapM_ (\(i, j) -> MU.swap mv i j) js) v)

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (col wire) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (col sub) | v <- xt ]
  ++ [ sayRM (prec3 v) (grX g - 5) (gY g v) 14 (col sub) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 36) 14 (col ink), say yl (grX g) (grY g - 8) 14 (col ink) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ 1.5 までは gloss の細い線）
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1.5 then Line pts else thickPath w pts
  _ -> Blank

-- (xs[i], ys[i]) を折れ線で（有限の値だけ）
plotXY :: Graph -> [Double] -> [Double] -> Color -> Float -> Picture
plotXY g xs ys c w = Color c (polyline w [ (gX g x, gY g y) | (x, y) <- zip xs ys, isFin y ])

-- 凡例：items = [(色, 名前)]
legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures
  [ Pictures [Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col ink)]
  | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）、右揃え・左揃えで上下は中央
sayM, sayRM, sayLM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c
sayLM s x y size c = say s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = i2f (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = if wt <= 1.2 then Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
                     else thickPath wt [(x - wt / 2, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分（細いときは gloss の線）
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW wt a b = if wt <= 1.5 then Line [a, b] else thickLine wt a b

-- 線分を箱（左上 x, y、幅 w、高さ h）の中だけに切る（p5.js 版の clip の代わり）
clipSeg :: Float -> Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Maybe ((Float, Float), (Float, Float))
clipSeg bx by bw bh (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
      edges = [(-dx, x1 - bx), (dx, bx + bw - x1), (-dy, y1 - by), (dy, by + bh - y1)]
      step (t0, t1) (p, q)
        | p == 0 = if q < 0 then (1, 0) else (t0, t1)
        | p < 0 = (max t0 (q / p), t1)
        | otherwise = (t0, min t1 (q / p))
      (a, b) = foldl' step (0, 1) edges
  in if a > b then Nothing else Just ((x1 + a * dx, y1 + a * dy), (x1 + b * dx, y1 + b * dy))

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashed on off (x1, y1) (x2, y2) =
  let l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
      k = max 1 (ceiling (l / (on + off))) :: Int
      p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
      fr = on / (on + off)
  in Pictures [ Line [p (i2f i / i2f k), p ((i2f i + fr) / i2f k)] | i <- [0 .. k - 1] ]

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = isFin a && abs (a - b) <= tol

relErr :: Double -> Double -> Double
relErr a b = abs (a - b) / max 1e-12 (abs a + abs b)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then makeColorI 150 230 170 255 else makeColorI 255 90 110 255)
