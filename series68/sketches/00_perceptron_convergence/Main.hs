-- ============================================
-- 68-00 パーセプトロンと収束定理 ─ ローゼンブラットの学習則：まちがえた点 x のぶんだけ、重みを w ← w + y x と動かす
-- 点 x = (x1, x2) にバイアスの 1 を足した (x1, x2, 1) で考える（直線 w1 x1 + w2 x2 + w3 = 0）。y = +1（マゼンタ）/ −1（ライム）
-- 収束定理（ノビコフ）：直線で分けられるなら、修正の回数 k は必ず (R/γ)² 以下で止まる
--   R = いちばん遠い点の長さ ‖(x, 1)‖、γ = 余白（分ける単位ベクトル u に対して、いちばん近い点までの距離 y u·(x, 1)）
-- 証明の 2 本の式：u·w ≥ kγ（毎回 γ 以上近づく）と ‖w‖² ≤ kR²（長さは √k R までしか伸びない）→ k ≤ (R/γ)²
-- しきい値のニューロン（マカロック-ピッツ、series51/09）に「学ぶ」しくみを足したもの。XOR は分けられないので止まらない
-- 操作：クリック 点を置く / 1 2 置く色 / スペース 一時停止 / ← → 速さ / R 新しいデータ / X XOR の配置 / C 全部消す
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

nPts :: Int
nPts = 40                  -- ★ 種つきデータの点の数

margin0 :: Double
margin0 = 0.08             -- ★ 種つきデータの余白（小さいほど修正が増える）

speeds :: [Double]
speeds = [0.1, 0.34, 1, 5, 40]   -- 1 フレームに調べる点の数

-- 点：x = (x1, x2)、y = +1 / −1
data Pt = Pt { ptX1 :: !Double, ptX2 :: !Double, ptY :: !Int }

-- 3 次元のベクトル（w1, w2, w3）
type W3 = (Double, Double, Double)

-- 修正のたびの記録：k、u*·w、‖w‖
data Hist = Hist { hK :: !Int, hUW :: !Double, hWW :: !Double }

data World = World
  { wPts       :: ![Pt]
  , wW         :: !W3
  , wK         :: !Int            -- 修正の回数
  , wTicks     :: !Int
  , wCur       :: !Int            -- 次に調べる点
  , wClean     :: !Int            -- 続けて正しかった点の数
  , wEpochs    :: !Int
  , wDone      :: !Bool
  , wUStar     :: !(Maybe W3)     -- データを作った直線（自分で置いたら Nothing）
  , wPaused    :: !Bool
  , wSpeed     :: !Int
  , wAddCls    :: !Int
  , wSeed      :: !Int
  , wAcc       :: !Double
  , wHist      :: ![Hist]         -- 新しい順（4000 個まで）
  , wFlash     :: !Int
  , wTest      :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "68-00 Perceptron convergence" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = makeSeparable 1 (World [] (0, 0, 0) 0 0 0 0 0 False Nothing False 2 1 1 0 [] 0 [])

-- ---- データ：ランダムな直線 u*（3 次元の単位ベクトル）で色を決め、余白 margin0 より近い点は捨てる
separableData :: Int -> Int -> Double -> ([Pt], W3)
separableData seed n m0 = withSeed seed $ do
  a0 <- rnd
  let a = 2 * pi * a0
  c <- rnd
  let (u0, u1, u2) = (cos a, sin a, 0.6 * (c - 0.5))
      nu = normW (u0, u1, u2)
      u = (u0 / nu, u1 / nu, u2 / nu)
      loop acc k
        | k >= n = pure (reverse acc)
        | otherwise = do
            p1 <- rnd
            p2 <- rnd
            let x1 = 2 * p1 - 1
                x2 = 2 * p2 - 1
                (v0, v1, v2) = u
                s = v0 * x1 + v1 * x2 + v2
            if abs s >= m0 then loop (Pt x1 x2 (if s > 0 then 1 else -1) : acc) (k + 1) else loop acc k
  ps <- loop [] 0
  pure (ps, u)

makeSeparable :: Int -> World -> World
makeSeparable seed w = let (ps, u) = separableData seed nPts margin0 in resetLearn w { wPts = ps, wUStar = Just u, wSeed = seed }

makeXor :: World -> World
makeXor w = resetLearn w { wPts = ps, wUStar = Nothing }
  where
    ps = withSeed (wSeed w + 100) (mapM one [ (q, k) | q <- [0 .. 3 :: Int], k <- [0 .. 5 :: Int] ])
    one (q, _) = do
      let cx = if odd q then 0.5 else -0.5
          cy = if q < 2 then 0.5 else -0.5
      a <- rnd
      b <- rnd
      pure (Pt (cx + 0.3 * (a - 0.5)) (cy + 0.3 * (b - 0.5)) (if q == 0 || q == 3 then 1 else -1))

resetLearn :: World -> World
resetLearn w = w { wW = (0, 0, 0), wK = 0, wTicks = 0, wCur = 0, wClean = 0, wEpochs = 0, wDone = False, wHist = [Hist 0 0 0] }

dotW :: W3 -> W3 -> Double
dotW (a0, a1, a2) (b0, b1, b2) = a0 * b0 + a1 * b1 + a2 * b2

normW :: W3 -> Double
normW a = sqrt (dotW a a)

aug :: Pt -> W3                               -- バイアスの 1 を足す
aug p = (ptX1 p, ptX2 p, 1)

radiusR :: [Pt] -> Double
radiusR = foldl' (\r p -> max r (normW (aug p))) 0

-- 単位ベクトル u の余白（負なら分けられていない）
marginOf :: [Pt] -> W3 -> Double
marginOf ps u = let nu = normW u in if not (nu > 0) then -1 else foldl' (\g p -> min g ((i2d (ptY p) * dotW u (aug p)) / nu)) posInf ps

-- ★ パーセプトロンの 1 回：点 p を調べ、まちがい（y w·x ≤ 0）なら w ← w + η y x
perceptronTick :: W3 -> Pt -> Double -> (W3, Bool)
perceptronTick w@(w0, w1, w2) p eta =
  let (x0, x1, x2) = aug p
      y = i2d (ptY p)
  in if y * dotW w (x0, x1, x2) <= 0 then ((w0 + eta * y * x0, w1 + eta * y * x1, w2 + eta * y * x2), True) else (w, False)

-- 収束まで（または maxEpochs 周まで）回す。u があれば証明の 2 本の式も毎回確かめる
data RunResult = RunResult { rW :: !W3, rK :: !Int, rConv :: !Bool, rEpochs :: !Int, rProof :: !Bool }

perceptronRun :: [Pt] -> Double -> Int -> Maybe W3 -> RunResult
perceptronRun ps eta maxEpochs mu = epoch 0 (0, 0, 0) 0 True
  where
    r = radiusR ps
    gam = maybe 0 (marginOf ps) mu
    epoch e w k proof
      | e >= maxEpochs = RunResult w k False maxEpochs proof
      | otherwise =
          let (w', k', bad, proof') = foldl' visit (w, k, 0 :: Int, proof) ps
          in if bad == 0 then RunResult w' k' True (e + 1) proof' else epoch (e + 1) w' k' proof'
    visit (w, k, bad, proof) p =
      let (w', ch) = perceptronTick w p eta
      in if not ch then (w, k, bad, proof) else
           let k' = k + 1
               proof' = case mu of
                 Nothing -> proof
                 Just u -> let (a, b, c) = w'
                               s = (a / eta, b / eta, c / eta)
                           in proof && not (dotW u s < i2d k' * gam - 1e-9 || dotW s s > i2d k' * r * r + 1e-9)
           in (w', k', bad + 1, proof')

learnTick :: World -> World
learnTick w
  | wDone w || null ps = w
  | otherwise =
      let p = at' ps (wCur w `mod` n) (Pt 0 0 1)
          (w', ch) = perceptronTick (wW w) p 1
          w1 = if ch
                 then w { wW = w', wK = wK w + 1, wClean = 0, wFlash = 12
                        , wHist = take 4000 (Hist (wK w + 1) (maybe 0 (`dotW` w') (wUStar w)) (normW w') : wHist w) }
                 else w { wClean = wClean w + 1 }
          cur = wCur w1 + 1
          w2 = w1 { wCur = cur, wTicks = wTicks w1 + 1, wEpochs = if cur `mod` n == 0 then wEpochs w1 + 1 else wEpochs w1 }
      in if wClean w2 >= n then w2 { wDone = True } else w2
  where ps = wPts w
        n = length ps

step :: Env -> Float -> World -> IO World
step _ _ w0 = pure (w2 { wFlash = max 0 (wFlash w2 - 1) })
  where
    w2 = if wPaused w0 then w0 else run (w0 { wAcc = wAcc w0 + at' speeds (wSpeed w0) 1 })
    run w = if wAcc w >= 1 then run ((learnTick w) { wAcc = wAcc w - 1 }) else w

sideOf :: Env -> Float
sideOf env = min (envH env - 130) (envW env * 0.47)

-- [−1, 1]² の点 → 画面
toBox :: Float -> Double -> Double -> (Float, Float)
toBox side x1 x2 = (30 + f2 ((x1 + 1) / 2) * side, 90 + f2 ((1 - x2) / 2) * side)

-- w1 x1 + w2 x2 + w3 = 0 を [−1, 1]² の中に描く
drawLine :: Float -> W3 -> Color -> Float -> Picture
drawLine side (w0, w1, w2) c sw =
  let pa | abs w1 > 1e-9 = [ (x1, -(w0 * x1 + w2) / w1) | x1 <- [-1, 1] ]
         | abs w0 > 1e-9 = [ (-w2 / w0, x2) | x2 <- [-1, 1] ]
         | otherwise = []
  in case pa of
       [(a1, a2), (b1, b2)] -> case clipSeg 30 90 side side (toBox side a1 a2) (toBox side b1 b2) of
         Just (p, q) -> Color c (lineW sw p q)
         Nothing -> Blank
       _ -> Blank

draw :: Env -> World -> Picture
draw env w = view env $
  [ hudTitle ("Perceptron convergence: " ++ show (wK w) ++ " corrections, " ++ show (wEpochs w) ++ " epochs"
              ++ (if wDone w then "  -> stopped (all correct)" else if not (null ps) && wEpochs w >= 300 then "  -> not stopping (not separable?)" else ""))
             ("click add point (" ++ (if wAddCls w > 0 then "magenta +1" else "lime -1") ++ ", 1 2 switch)   Space " ++ (if wPaused w then "resume" else "pause")
              ++ "   Left/Right speed " ++ jsNum (at' speeds (wSpeed w) 1) ++ " pts/frame   R new data   X XOR   C clear")
  -- ---- 左：点と直線
  , Color (col panel) (rectTL 30 90 side side)
  , Color (col wire) (Pictures [Line [(30 + side / 2, 90), (30 + side / 2, 90 + side)], Line [(30, 90 + side / 2), (30 + side, 90 + side / 2)]])
  , maybe Blank (\u -> drawLine side u (colA 120 sub) 1) (wUStar w)
  , if normW (wW w) > 0 then drawLine side (wW w) (col (if wFlash w > 0 then amber else ink)) 3 else Blank
  , Pictures [ let (sx, sy) = toBox side (ptX1 p) (ptX2 p)
                   bad = normW (wW w) > 0 && i2d (ptY p) * dotW (wW w) (aug p) <= 0
               in Pictures [ Color (if bad then col amber else grayC 10) (circleAt sx sy (if bad then 17 else 15))
                           , Color (col (if ptY p > 0 then mag else lime)) (circleAt sx sy 13) ]
             | p <- ps ]
  , if not (null ps) && not (wDone w)
      then let p = at' ps (wCur w `mod` length ps) (Pt 0 0 1)
               (sx, sy) = toBox side (ptX1 p) (ptX2 p)
           in Color (col ink) (ringAt sx sy 24 1.5)
      else Blank
  , say ("white = current line w.(x, 1) = 0" ++ (if isJust' (wUStar w) then "   faint = line u* that made the data" else "") ++ "   orange rim = wrong point") 30 (90 + side + 22) 14 (col sub)
  -- ---- 右：数値と証明の 2 本の式
  , say ("w = (" ++ intercalate ", " [fmtN a 2, fmtN b 2, fmtN c 2] ++ ")   points " ++ show (length ps)) rx 112 16 (col ink)
  , say ("R = max |(x, 1)| = " ++ fmtN r 3) rx 140 16 (col ink)
  , say ("gamma = margin = " ++ (if gam > 0 then fmtN gam 4 else "- (no separating line yet)") ++ (if isJust' (wUStar w) then " (measured with u*)" else if wDone w then " (measured with the final line)" else "")) rx 166 16 (col ink)
  , say ("bound (R/gamma)^2 = " ++ (if isFin bound then fmtN bound 1 else "-") ++ "    corrections k = " ++ show (wK w)
         ++ (if isFin bound then (if i2d (wK w) <= bound then "  <= bound  OK" else "  > bound  NG") else "")) rx 200 18 (col amber)
  , plotAxes g "corrections k" "proof: |w| <= sqrt(k) R (upper) and u.w >= k gamma (lower)" [0, grX1 g] [0, roundPrec 2 (grY1 g)]
  , plotXY g kk (map (\k -> r * sqrt k) kk) (colA 140 sky) 1
  , plotXY g ks ww (col sky) 2
  , if isJust' (wUStar w) then Pictures [plotXY g kk (map (* gam) kk) (colA 140 mag) 1, plotXY g ks uw (col mag) 2] else Blank
  , legend (rx + 10) (grY g + 18) ((sky, "|w| (thick) and sqrt(k) R (thin)") : [ (mag, "u*.w (thick) and k gamma (thin)") | isJust' (wUStar w) ])
  , say "Each correction adds at least gamma to u.w, and at most R^2 to |w|^2." rx ty 14 (col sub)
  , say "u.w <= |w|, so k gamma <= sqrt(k) R -> k <= (R/gamma)^2, whatever the order." rx (ty + 20) 14 (col sub)
  , say "X (XOR) cannot be split by any line -> corrections never end (05 solves it)." rx (ty + 40) 14 (col sub)
  , drawSelfTest env (wTest w) ]
  where
    ps = wPts w
    side = sideOf env
    (a, b, c) = wW w
    r = radiusR ps
    mu = case wUStar w of
      Just u -> Just u
      Nothing -> if wDone w then Just (wW w) else Nothing
    gam = maybe (0 / 0) (marginOf ps) mu
    bound = if gam > 0 then (r / gam) * (r / gam) else posInf
    rx = 30 + side + 50
    rw = envW env - rx - 40
    hs = reverse (wHist w)
    ks = map (i2d . hK) hs
    ww = map hWW hs
    uw = map hUW hs
    x1 = max 10 (i2d (wK w))
    g = Graph rx 250 rw (min 260 (envH env - 420)) 0 x1 0 (maximum (1 : r * sqrt x1 : ww))
    kk = [ i2d i / 60 * x1 | i <- [0 .. 60 :: Int] ]
    ty = grY g + grH g + 56
    isJust' = maybe False (const True)

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        side = sideOf env
        x = d2 ((mx - 30) / side) * 2 - 1
        y = 1 - d2 ((my - 90) / side) * 2
    in if abs x <= 1 && abs y <= 1 then w { wPts = wPts w ++ [Pt x y (wAddCls w)], wUStar = Nothing, wDone = False, wClean = 0 } else w
  EventKey (Char '1') Down _ _ -> w { wAddCls = 1 }
  EventKey (Char '2') Down _ _ -> w { wAddCls = -1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSpeed = min (length speeds - 1) (wSpeed w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSpeed = max 0 (wSpeed w - 1) }
  EventKey (Char ch) Down _ _
    | ch `elem` "rR" -> makeSeparable (wSeed w + 1) w
    | ch `elem` "xX" -> makeXor w
    | ch `elem` "cC" -> resetLearn w { wPts = [], wUStar = Nothing }
  _ -> w

-- ---------- やってみよう ----------
-- 1. margin0 を 0.02 や 0.2 にする。修正の回数と上限 (R/γ)² はどう変わる？
-- 2. R でデータを何回も作り直す。修正の回数が上限をこえることはある？
-- 3. C で消して、自分で点を置く。余白の小さい配置（境目ぎりぎりの点）だと修正が増える？
-- 4. X の XOR で、何周しても止まらないことを確かめる。点を 1 つ消したら止まる？

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("6 つの種のデータで止まり、修正の回数 ≤ (R/γ)²（種 1：" ++ info ++ "）", allOk)
  , ("証明の 2 本の式 u·w ≥ kγ と ‖w‖² ≤ kR² が毎回成り立つ", proofOk)
  , ("学習率 η を 0.1 にしても、w = 0 から始めれば修正の回数は同じ（w が 0.1 倍になるだけ）", rK r1 == rK r2 && and (zipWith (\v v2 -> near (v * 0.1) v2 1e-9) (l3 (rW r1)) (l3 (rW r2))))
  , ("XOR の 4 点：2000 周しても止まらない（修正 " ++ show (rK rx) ++ " 回）", not (rConv rx) && rK rx > 2000)
  , ("点を (3, 3) ずらしても、バイアスつきなら分けられて止まる", rConv rs && marginOf shifted (rW rs) > 0) ]
  where
    runs = [ let (ps, u) = separableData s nPts margin0
                 rr = radiusR ps
                 gm = marginOf ps u
                 res = perceptronRun ps 1 5000 (Just u)
             in (res, rr, gm, ps) | s <- [1 .. 6] ]
    allOk = and [ rConv res && i2d (rK res) <= (rr / gm) ** 2 && marginOf ps (rW res) > 0 | (res, rr, gm, ps) <- runs ]
    proofOk = all (\(res, _, _, _) -> rProof res) runs
    info = case runs of
      ((res, rr, gm, _) : _) -> "k = " ++ show (rK res) ++ " ≤ " ++ fmtN ((rr / gm) ** 2) 0
      [] -> ""
    (d3, _) = separableData 3 nPts margin0
    r1 = perceptronRun d3 1 5000 Nothing
    r2 = perceptronRun d3 0.1 5000 Nothing
    l3 (p, q, s) = [p, q, s]
    xorPts = [Pt 0.5 0.5 1, Pt (-0.5) (-0.5) 1, Pt (-0.5) 0.5 (-1), Pt 0.5 (-0.5) (-1)]
    rx = perceptronRun xorPts 1 2000 Nothing
    shifted = [ Pt (ptX1 p + 3) (ptX2 p + 3) (ptY p) | p <- d3 ]
    rs = perceptronRun shifted 1 20000 Nothing

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

-- ---- 行列の道具（ベクトル = U.Vector Double、行列 = 行のベクトル）
type Mat = V.Vector Vec

dotV :: Vec -> Vec -> Double
dotV a b = go 0 0
  where n = min (U.length a) (U.length b)
        go !i !s = if i >= n then s else go (i + 1) (s + U.unsafeIndex a i * U.unsafeIndex b i)

normV :: Vec -> Double
normV a = sqrt (dotV a a)

matVec :: Mat -> Vec -> Vec
matVec m v = U.generate (V.length m) (\i -> dotV (m V.! i) v)

zerosMat :: Int -> Int -> Mat
zerosMat n m = V.replicate n (U.replicate m 0)

-- ガウスの消去法（部分ピボット）で a x = b（a は n × n を行の順に並べたもの）。解けなければ Nothing
solveLinearU :: Int -> U.Vector Double -> U.Vector Double -> Maybe Vec
solveLinearU n a b = runST $ do
  let w = n + 1
  m <- MU.new (n * w)
  forM_ [0 .. n - 1] $ \r -> do
    forM_ [0 .. n - 1] $ \k -> MU.write m (r * w + k) (a U.! (r * n + k))
    MU.write m (r * w + n) (b U.! r)
  let elim c
        | c >= n = pure True
        | otherwise = do
            let findP !p !r
                  | r >= n = pure p
                  | otherwise = do
                      x <- MU.read m (r * w + c)
                      y <- MU.read m (p * w + c)
                      findP (if abs x > abs y then r else p) (r + 1)
            p <- findP c (c + 1)
            mp <- MU.read m (p * w + c)
            if abs mp < 1e-300 then pure False else do
              when (p /= c) $ forM_ [0 .. n] $ \k -> MU.swap m (c * w + k) (p * w + k)
              mcc <- MU.read m (c * w + c)
              forM_ [c + 1 .. n - 1] $ \r -> do
                mrc <- MU.read m (r * w + c)
                let f = mrc / mcc
                when (f /= 0) $ forM_ [c .. n] $ \k -> do
                  ck <- MU.read m (c * w + k)
                  MU.modify m (\v -> v - f * ck) (r * w + k)
              elim (c + 1)
  ok <- elim 0
  if not ok then pure Nothing else do
    x <- MU.replicate n 0
    forM_ [n - 1, n - 2 .. 0] $ \i -> do
      s0 <- MU.read m (i * w + n)
      let go !k !s | k >= n = pure s
                   | otherwise = do mik <- MU.read m (i * w + k); xk <- MU.read x k; go (k + 1) (s - mik * xk)
      s <- go (i + 1) s0
      mii <- MU.read m (i * w + i)
      MU.write x i (s / mii)
    xs <- U.freeze x
    pure (if U.all isFin xs then Just xs else Nothing)

solveLinear :: Mat -> Vec -> Maybe Vec
solveLinear a b = solveLinearU (U.length b) (U.concat (V.toList a)) b

-- リッジ回帰 (ΦᵀΦ + λI) w = Φᵀ y。phi = 行がデータ、列が特徴
ridgeSolve :: Mat -> Vec -> Double -> Vec
ridgeSolve phi y lam = fromMaybe (U.replicate p 0) (solveLinearU p a b)
  where
    p = U.length (if V.null phi then U.empty else V.head phi)
    (a, b) = runST $ do
      am <- MU.replicate (p * p) 0
      bm <- MU.replicate p 0
      forM_ [0 .. V.length phi - 1] $ \i -> do
        let f = phi V.! i
            yi = y U.! i
        forM_ [0 .. p - 1] $ \j -> do
          let fj = U.unsafeIndex f j
          MU.modify bm (+ fj * yi) j
          forM_ [j .. p - 1] $ \k -> MU.unsafeModify am (+ fj * U.unsafeIndex f k) (j * p + k)
      forM_ [0 .. p - 1] $ \j -> do
        MU.modify am (+ lam) (j * p + j)
        forM_ [0 .. j - 1] $ \k -> MU.read am (k * p + j) >>= MU.write am (j * p + k)
      (,) <$> U.freeze am <*> U.freeze bm

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
