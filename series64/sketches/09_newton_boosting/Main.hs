-- ============================================
-- 64-09 2 次の勾配ブースティング ─ 勾配 g（1 階微分）だけでなく、ヘッセ h（2 階微分）も使って木を作る（XGBoost の考え方）
-- ロジスティック損失：p = σ(F)、g = p − y、h = p(1 − p)。葉の値 w = −G/(H + λ)（G, H = 葉の中の g, h の合計）
--   分割の利得 = ½ [G_L²/(H_L + λ) + G_R²/(H_R + λ) − G²/(H + λ)] − γ。利得が正のときだけ分ける
--   λ（↑↓）は葉の値を小さく、γ（← →）は木を小さくする正則化。1 次（series62/11 と同じ：残差 y − p に木をあてはめる）と比べる
--   右下の木 = いま作った木（葉の数字 = w、節の数字 = 利得）
-- 操作：↑↓ λ / ← → γ / スペース 一時停止 / D データの形 / R データを作り直す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (runST)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate, sortBy, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import GHC.Clock (getMonotonicTime)
import System.Environment (lookupEnv)

lams, gams :: [Double]
lams = [0, 0.5, 1, 2, 5, 10, 30]     -- ★ λ の候補
gams = [0, 0.5, 1, 2, 4, 8]          -- ★ γ の候補

nu :: Double
nu = 0.3                   -- ★ 学習率

depthMax, maxR, cells :: Int
depthMax = 3               -- ★ 木の深さ
maxR = 120
cells = 60

-- 2 次の木：葉 = w（と G, H）、節 = 特徴量・しきい値・利得
data NT = NLeaf { ntW :: !Double, ntDepth :: !Int }
        | NNode { ntF :: !Int, ntThr :: !Double, ntGain :: !Double, ntDepth :: !Int, ntL :: NT, ntR :: NT }

data Opt = Opt { oLam :: !Double, oGam :: !Double, oMaxDepth :: !Int }

-- ブースター：second = True なら 2 次（h = p(1 − p)、λ, γ つき）、False なら 1 次（h = 1、λ = γ = 0 → 葉 = 残差の平均）
data Booster = Booster { bSecond :: !Bool, bTrees :: ![NT], bF :: !(U.Vector Double), bFt :: !(U.Vector Double)
                       , bLoss :: ![Double], bLeaves :: ![Int], bGrid :: !(U.Vector Float) }   -- bTrees, bLoss, bLeaves は新しい順

data World = World
  { wLamI   :: !Int
  , wGamI   :: !Int
  , wKind   :: !Int
  , wSeed   :: !Int
  , wPaused :: !Bool
  , wTrain  :: !DataSet
  , wTestD  :: !DataSet
  , wNewton :: !Booster
  , wFirst  :: !Booster
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "64-09 Newton boosting" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = restart (World 2 0 4 5 False emptyD emptyD (makeBooster emptyD emptyD True) (makeBooster emptyD emptyD False) [])

emptyD :: DataSet
emptyD = DataSet V.empty U.empty

restart :: World -> World
restart w =
  let tr = makeData2D (wKind w) 300 0.08 (wSeed w)
      te = makeData2D (wKind w) 1000 0.08 (wSeed w + 77)
  in w { wPaused = False, wTrain = tr, wTestD = te, wNewton = makeBooster tr te True, wFirst = makeBooster tr te False }

-- ブースターを作り直す（λ, γ を変えたとき）
reboost :: World -> World
reboost w = w { wNewton = makeBooster (wTrain w) (wTestD w) True, wFirst = makeBooster (wTrain w) (wTestD w) False }

-- ★ g と h で木を作る（貪欲に：全部の特徴量 × 全部のしきい値の利得を調べる）
growNT :: V.Vector Pt -> U.Vector Double -> U.Vector Double -> [Int] -> Int -> Opt -> NT
growNT xs g h idx depth o
  | depth >= oMaxDepth o || length idx < 2 = leaf
  | otherwise = case best of
      Just (f, thr, gain) | gain > 0 ->
        NNode f thr gain depth (growNT xs g h (filter (\i -> xAt xs i f <= thr) idx) (depth + 1) o)
                               (growNT xs g h (filter (\i -> xAt xs i f > thr) idx) (depth + 1) o)
      _ -> leaf
  where
    gG = foldl' (\s i -> s + g U.! i) 0 idx
    hH = foldl' (\s i -> s + h U.! i) 0 idx
    leaf = NLeaf (-gG / (hH + oLam o)) depth
    base = (gG * gG) / (hH + oLam o)
    best = foldl' scanF Nothing [0, 1]
    scanF b0 f =
      let ord = U.fromList (sortOn (\i -> xAt xs i f) idx)
          n = U.length ord
          stepQ (!b, !gl, !hl) q =
            let i = ord U.! q
                gl' = gl + g U.! i
                hl' = hl + h U.! i
                v0 = xAt xs i f
                v1 = xAt xs (ord U.! (q + 1)) f
                gr = gG - gl'
                gain = 0.5 * ((gl' * gl') / (hl' + oLam o) + (gr * gr) / (hH - hl' + oLam o) - base) - oGam o
                b' = if not (v1 > v0) then b else case b of
                       Just (_, _, g0) | not (gain > g0) -> b
                       _ -> Just (f, (v0 + v1) / 2, gain)
            in (b', gl', hl')
          (bb, _, _) = foldl' stepQ (b0, 0, 0) [0 .. n - 2]
      in bb

ntValue :: NT -> Pt -> Double
ntValue t x = case t of
  NLeaf w _ -> w
  NNode f thr _ _ l r -> ntValue (if x U.! f <= thr then l else r) x

ntLeaves :: NT -> Int
ntLeaves t = case t of
  NLeaf _ _ -> 1
  NNode _ _ _ _ l r -> ntLeaves l + ntLeaves r

logLoss :: U.Vector Double -> U.Vector Int -> Double
logLoss fv ys = let n = U.length ys
                    s = U.ifoldl' (\acc i f -> let p = clampv (sigm f) 1e-12 (1 - 1e-12)
                                               in acc - (if ys U.! i /= 0 then log p else log (1 - p))) 0 fv
                in s / fromIntegral n

makeBooster :: DataSet -> DataSet -> Bool -> Booster
makeBooster tr te second =
  let n = V.length (dsX tr)
  in Booster second [] (U.replicate n 0) (U.replicate (V.length (dsX te)) 0) [logLoss (U.replicate n 0) (dsY tr)] [] (U.replicate (cells * cells) 0)

boostOne :: DataSet -> DataSet -> Opt -> Booster -> Booster
boostOne tr te o b =
  let ps = U.map sigm (bF b)
      g = U.imap (\i p -> p - fromIntegral (dsY tr U.! i)) ps
      h = if bSecond b then U.map (\p -> max 1e-6 (p * (1 - p))) ps else U.map (const 1) ps
      t = growNT (dsX tr) g h [0 .. V.length (dsX tr) - 1] 0 (if bSecond b then o else Opt 0 0 (oMaxDepth o))
      f' = U.imap (\i f -> f + nu * ntValue t (dsX tr V.! i)) (bF b)
      ft' = U.imap (\i f -> f + nu * ntValue t (dsX te V.! i)) (bFt b)
  in b { bTrees = t : bTrees b, bF = f', bFt = ft', bLoss = logLoss f' (dsY tr) : bLoss b, bLeaves = ntLeaves t : bLeaves b }

opts :: World -> Opt
opts w = Opt (headOr 0 (drop (wLamI w) lams)) (headOr 0 (drop (wGamI w) gams)) depthMax

accOf :: World -> Booster -> Double
accOf w b = let ys = dsY (wTestD w)
                ok = U.length (U.filter id (U.imap (\i f -> (if f > 0 then 1 else 0) == ys U.! i) (bFt b)))
            in fromIntegral ok / fromIntegral (max 1 (U.length ys))

step :: Env -> Float -> World -> IO World
step _ _ w
  | wPaused w || length (bTrees (wNewton w)) >= maxR = pure w
  | otherwise =
      let nb = boostOne (wTrain w) (wTestD w) (opts w) (wNewton w)
          fb = boostOne (wTrain w) (wTestD w) (opts w) (wFirst w)
          t = headOr (NLeaf 0 0) (bTrees nb)            -- 新しい木の分だけマス目の F に足す
          grid = U.imap (\q v -> realToFrac (realToFrac v + nu * ntValue t (cellPt cells q) :: Double)) (bGrid nb)
      in pure w { wNewton = nb { bGrid = grid }, wFirst = fb }

draw :: Env -> World -> Picture
draw env w = view env
  [ imageBox 40 110 side side cells cells (gridImage cells (\q -> probCol (sigm (realToFrac (bGrid nb U.! q))) 0.8))
  , frameBox bx frameCol
  , drawPoints bx (dsX (wTrain w)) (dsY (wTrain w)) 6
  , sayWrap ("Newton boosting probability sigma(F). Test accuracy " ++ pctStr (accOf w nb) ++ " (first order " ++ pctStr (accOf w fb) ++ ")") 40 (110 + side + 6) side 14 (rgbC 200 185 170)
  -- ---- 右上：訓練の損失
  , plotAxes g "round" "training loss: gold = 2nd order, teal = 1st" [0, fromIntegral maxR / 2, fromIntegral maxR] [0, roundTo 2 (top / 2), roundTo 2 top]
  , Color (colOf teal) (polyline 2.5 [ gPt g (fromIntegral k) v | (k, v) <- zip [0 :: Int ..] (reverse (bLoss fb)) ])
  , Color (colOf gold) (polyline 2.5 [ gPt g (fromIntegral k) v | (k, v) <- zip [0 :: Int ..] (reverse (bLoss nb)) ])
  -- ---- 右上の右：葉の数
  , plotAxes g2 "round" "leaves of the 2nd order tree" [0, fromIntegral maxR] [0, 2 ^^ depthMax]
  , Color (colOf gold) (Pictures [ rectTL (gX g2 (fromIntegral k)) (gY g2 (fromIntegral l)) (max 1 (grW g2 / fromIntegral maxR - 1)) (grY g2 + grH g2 - gY g2 (fromIntegral l))
                                 | (k, l) <- zip [0 :: Int ..] (reverse (bLeaves nb)) ])
  -- ---- 右下：いまの木
  , say "current tree: node = gain, leaf = w = -G/(H + lambda)" rx ty 15 (colOf ink)
  , case bTrees nb of
      (t : _) -> drawNT t rx (ty + 30) (envW env - rx - 50) (envH env - ty - 90)
      [] -> Blank
  -- ---- 文字
  , say ("Newton boosting   data: " ++ shapeName (wKind w) ++ "   round " ++ show (length (bTrees nb)) ++ "   learning rate nu = " ++ fmtNum 2 nu) 40 40 20 (colOf ink)
  , say ("Up/Down lambda = " ++ fmtNum 2 lam ++ "   <- -> gamma = " ++ fmtNum 2 gam ++ "   Space pause" ++ (if wPaused w then " (paused)" else "") ++ "   D data shape   R new data") 40 66 15 (rgbC 170 155 145)
  , drawSelfTest env (wTest w) ]
  where
    nb = wNewton w
    fb = wFirst w
    o = opts w
    lam = oLam o
    gam = oGam o
    side = min (envH env - 200) (envW env * 0.36)
    bx = Box 40 110 side side
    rx = 40 + side + 70
    top = maximum (bLoss nb ++ bLoss fb)
    g = Graph rx 110 ((envW env - rx - 50) * 0.55) (envH env * 0.34) 0 (fromIntegral maxR) 0 (top * 1.05)
    g2 = Graph (grX g + grW g + 50) (grY g) (envW env - grX g - grW g - 100) (grH g) 0 (fromIntegral maxR) 0 (2 ^^ depthMax + 0.5)
    ty = grY g + grH g + 66

-- 木の図：葉を左から並べ、親は子のまん中（節は 2 行：しきい値と利得）
drawNT :: NT -> Float -> Float -> Float -> Float -> Picture
drawNT t x0 y0 w h = let (pics, _, _) = go t 0 in Pictures pics
  where
    nl = ntLeaves t
    cw = w / fromIntegral (max 1 nl)
    rh = min 64 (h / fromIntegral (depthMax + 1))
    bw = max 20 (min 110 (cw - 4))
    posOf q k = (x0 + (k + 0.5) * cw, y0 + fromIntegral (ntDepth q) * rh + 18)
    -- go q k = (絵, 次の葉の番号, この節の位置)。k = 左から何番目の葉か
    go :: NT -> Int -> ([Picture], Int, Float)
    go q k = case q of
      NLeaf wv _ ->
        let (x, y) = posOf q (fromIntegral k)
        in ([ Color (colOf (probCol (sigm (3 * wv)) 1)) (rectTL (x - bw / 2) (y - 14) bw 28)
            , sayM (fmtNum 2 wv) x y 14 (colOf ink) ], k + 1, fromIntegral k)
      NNode f thr gain _ l r ->
        let (pl, k1, a) = go l k
            (pr, k2, b) = go r k1
            mid = (a + b) / 2
            (x, y) = posOf q mid
            (lx, ly) = posOf l a
            (rx', ry) = posOf r b
            off c = if isLeaf c then 14 else 18
        in ( [ Color (rgbC 120 95 100) (lineW 1.2 (x, y + 18) (lx, ly - off l))
             , Color (rgbC 120 95 100) (lineW 1.2 (x, y + 18) (rx', ry - off r)) ]
             ++ pl ++ pr
             ++ [ Color (rgbC 60 40 48) (rectTL (x - bw / 2) (y - 18) bw 36)
                , sayM ((if f == 1 then "y" else "x") ++ "<=" ++ fmtNum 2 thr) x (y - 8) 14 (colOf ink)
                , sayM (fmtNum 1 gain) x (y + 9) 14 (colOf ink) ], k2, mid)
    isLeaf c = case c of
      NLeaf _ _ -> True
      _ -> False

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> reboost w { wLamI = min (length lams - 1) (wLamI w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> reboost w { wLamI = max 0 (wLamI w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> reboost w { wGamI = min (length gams - 1) (wGamI w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> reboost w { wGamI = max 0 (wGamI w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c == 'd' || c == 'D' -> restart w { wKind = (wKind w + 1) `mod` 6 }
    | c == 'r' || c == 'R' -> restart w { wSeed = wSeed w + 1 }
  _ -> w

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("g = p − y と h = p(1 − p) は、ロジスティック損失の 1 階・2 階の数値微分と一致", all okD [0, 1])
  , ("葉の値 = −G/(H + λ)、それが 2 次の目的関数 Σ(g·w + ½h·w²) + ½λw² の最小点"
    , near lw (-gS / (hS + 1)) 1e-12 && obj allI lw < obj allI (lw + 1e-3) && obj allI lw < obj allI (lw - 1e-3))
  , ("分割の利得の式 = 目的関数を直接計算した減り（" ++ fmtNum 6 stGain ++ " = " ++ fmtNum 6 direct ++ "、1e-9）", near stGain direct 1e-9)
  , ("選んだ分割の利得 ≥ 総当たり（全部の中点で直接計算）の最大", stGain >= bestB - 1e-9)
  , ("同じ g, h なら γ を大きくするほど葉は減る（γ = 0, 1, 5, 20, 80 → " ++ intercalate ", " (map show lv) ++ "）"
    , and (zipWith (<=) (drop 1 lv) lv) && lastOr 0 lv < headOr 0 lv)
  , ("30 ラウンド後の訓練の損失：2 次 " ++ fmtNum 4 ln ++ " < 1 次 " ++ fmtNum 4 lf ++ "（同じ ν = " ++ fmtNum 2 nu ++ "、深さ 3）", ln < lf && noNaN (U.toList (bF nb30))) ]
  where
    w0 = restart world0
    tr = wTrain w0
    te = wTestD w0
    xs = dsX tr
    ys = dsY tr
    n = V.length xs
    -- ロジスティック損失 L(F) = −y log σ(F) − (1 − y) log(1 − σ(F)) の g, h を数値微分で
    okD y = let lf' f = -(if y == (1 :: Int) then log (sigm f) else log (1 - sigm f))
                f0 = 0.7
                e = 1e-4
                p = sigm f0
            in not (abs ((lf' (f0 + e) - lf' (f0 - e)) / (2 * e) - (p - fromIntegral y)) > 1e-7
                    || abs ((lf' (f0 + e) - 2 * lf' f0 + lf' (f0 - e)) / (e * e) - p * (1 - p)) > 1e-5)
    g = U.generate n (\i -> 0.5 - fromIntegral (ys U.! i))
    h = U.replicate n (0.5 * (1 - 0.5))
    allI = [0 .. n - 1]
    o = Opt 1 0 1
    -- 2 次の近似の目的関数
    obj ids wv = foldl' (\s i -> s + (g U.! i * wv + 0.5 * (h U.! i) * wv * wv)) (0.5 * oLam o * wv * wv) ids
    lw = ntW' (growNT xs g h allI 0 (Opt 1 0 0))
    ntW' t = case t of
      NLeaf v _ -> v
      _ -> 0
    gS = sumU g
    hS = sumU h
    st = growNT xs g h allI 0 o
    (stGain, direct) = case st of
      NNode f thr gn _ l r -> (gn, obj allI lw - obj (filter (\i -> xAt xs i f <= thr) allI) (ntW' l) - obj (filter (\i -> xAt xs i f > thr) allI) (ntW' r))
      _ -> (0, 1)
    -- 総当たり：全部の中点で直接計算した減りの最大
    bestB = maxOr negInf
      [ obj allI lw - obj a wa - obj b wb
      | f <- [0, 1]
      , let vals = uniq (sortOn id [ x U.! f | x <- V.toList xs ])
      , (v0, v1) <- zip vals (drop 1 vals)
      , let th = (v0 + v1) / 2
            a = filter (\i -> xAt xs i f <= th) allI
            b = filter (\i -> xAt xs i f > th) allI
            wa = -sumL (map (g U.!) a) / (sumL (map (h U.!) a) + 1)
            wb = -sumL (map (g U.!) b) / (sumL (map (h U.!) b) + 1) ]
    uniq (a : b : r) = if a == b then uniq (b : r) else a : uniq (b : r)
    uniq r = r
    lv = [ ntLeaves (growNT xs g h allI 0 (Opt 1 gm 4)) | gm <- [0, 1, 5, 20, 80] ]
    run k b = iterate (boostOne tr te (Opt 1 0 3)) b !! k
    nb30 = run 30 (makeBooster tr te True)
    fb30 = run 30 (makeBooster tr te False)
    ln = headOr 0 (bLoss nb30)
    lf = headOr 0 (bLoss fb30)

-- ---------- やってみよう ----------
-- 1. 損失のグラフで、2 次（金）が 1 次（青緑）より速く下がるのを確かめよう。ν を 0.1 や 1 にすると？
-- 2. ← → で γ を大きくすると、右上の葉の数が減る。γ = 8 だと木はほとんど切り株に
-- 3. ↑↓ で λ を大きくすると、葉の数字（w）が小さくなる。損失の下がり方は？
-- 4. 木の図の利得の数字は、根に近いほど大きい？ 利得 < γ の分割はしない
-- 5. 2 次の利得は「この分割で 2 次の近似の損失がどれだけ減るか」。自己テストでは直接計算と比べている

-- ==================================================
-- ここから下は「アンサンブル学習の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。点 x = U.Vector Double（x1, x2, …）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 小さな道具：はさむ・合計・平均・いちばん大きい番号・数の表示
type Pt = U.Vector Double                     -- 点（x1, x2, …）

pt2 :: Double -> Double -> Pt
pt2 u v = U.fromList [u, v]

-- データ：点 X とクラス y（0, 1）
data DataSet = DataSet { dsX :: !(V.Vector Pt), dsY :: !(U.Vector Int) }

xAt :: V.Vector Pt -> Int -> Int -> Double    -- X[i][f]
xAt xs i f = (xs V.! i) U.! f

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

-- はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumL :: [Double] -> Double
sumL = foldl' (+) 0

sumU :: U.Vector Double -> Double
sumU = U.foldl' (+) 0

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else sumL a / fromIntegral (length a)

-- ÷n の分散
varOf :: [Double] -> Double
varOf a = let m = meanOf a in if null a then 0 else foldl' (\s v -> s + (v - m) * (v - m)) 0 a / fromIntegral (length a)

-- いちばん大きい番号（同点なら小さい番号）
argMaxL :: [Double] -> Int
argMaxL [] = 0
argMaxL (a0 : as) = go 1 0 a0 as
  where go _ b _ [] = b
        go !i !b !bv (v : vs) = if v > bv then go (i + 1) i v vs else go (i + 1) b bv vs

-- いちばん小さい値の最初の番号（indexOf(Math.min(...))）
argMinL :: [Double] -> Int
argMinL = argMaxL . map negate

sigm :: Double -> Double
sigm z = 1 / (1 + exp (-(clampv z (-40) 40)))

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

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

-- 数を短く書く（p5.js 版の fmtNum：toFixed(d) のあと余分な 0 を消す。0.000001 より小さいと 1e-7 の形）
fmtNum :: Int -> Double -> String
fmtNum d v
  | isNaN v || isInfinite v = "?"
  | n == 0 = "0"
  | otherwise = (if v < 0 then "-" else "") ++ body
  where
    n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
    ds = show n
    padded = replicate (d + 1 - length ds) '0' ++ ds
    (ip, fp) = splitAt (length padded - d) padded
    tiny = fromIntegral n / (10 ^^ d :: Double) < 1e-6
    sig = reverse (dropWhile (== '0') (reverse ds))
    ex = length ds - 1 - d
    body | tiny = take 1 sig ++ (if length sig > 1 then "." ++ drop 1 sig else "") ++ "e" ++ show ex
         | d > 0 = stripZeros (ip ++ "." ++ fp)
         | otherwise = ip
    stripZeros s = let t = reverse (dropWhile (== '0') (reverse s)) in if lastOr ' ' t == '.' then init t else t

pctStr :: Double -> String
pctStr v = fmtNum 1 (100 * v) ++ "%"

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

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

-- 計算を実行して、結果と次の状態
evalR :: Rng -> R a -> (a, Rng)
evalR g m = runR m g

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

rndGauss :: R Double                          -- 標準正規分布（ボックス-ミュラー）
rndGauss = do
  u0 <- rnd
  v <- rnd
  let u = max 1e-12 u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- フィッシャー-イェーツのシャッフル（後ろから i と 0〜i のどこかを入れかえる）
shuffleU :: U.Unbox a => U.Vector a -> R (U.Vector a)
shuffleU v = do
  let n = U.length v
  js <- mapM (\i -> do u <- rnd; pure (i, floor (u * fromIntegral (i + 1)))) [n - 1, n - 2 .. 1]
  pure (U.modify (\mv -> mapM_ (\(i, j) -> MU.swap mv i j) js) v)

-- ブートストラップ：0〜n−1 を重複ありで n 個
bootIdx :: Int -> R (U.Vector Int)
bootIdx n = U.fromList <$> mapM (const (do u <- rnd; pure (floor (u * fromIntegral n)))) [1 .. n]

-- ---- 合成データ（[0, 1]² の 2 次元、2 クラス、種つき）。kind = 0 かたまり / 1 斜め / 2 円 / 3 XOR / 4 三日月 / 5 しま
shapeNames :: [String]
shapeNames = ["blobs", "diagonal", "circle", "XOR", "moons", "stripes"]

shapeName :: Int -> String
shapeName k = headOr "?" (drop k shapeNames)

trueLabel :: Int -> Double -> Double -> Int
trueLabel kind u v = case kind of
  0 -> if sq (u - 0.32) + sq (v - 0.36) < sq (u - 0.68) + sq (v - 0.64) + 0.03 * sin (9 * u) then 0 else 1
  1 -> if u + 0.8 * v > 0.9 then 1 else 0
  2 -> if sq (u - 0.5) + sq (v - 0.5) < 0.09 then 1 else 0
  3 -> if (u > 0.5) /= (v > 0.5) then 1 else 0
  4 -> if v > 0.5 + 0.28 * cos (pi * (1.6 * u - 0.3)) - 0.18 then 1 else 0
  _ -> if sin (10 * u + 3 * v) > 0 then 1 else 0
  where sq a = a * a

-- n 個、flip = ラベルをわざと入れかえる割合（ノイズ）
makeData2DR :: Int -> Int -> Double -> Int -> R DataSet
makeData2DR kind n flipP seed = do
  seedRng seed
  ps <- mapM (const one) [1 .. n]
  pure (DataSet (V.fromList (map fst ps)) (U.fromList (map snd ps)))
  where one = do
          a <- rnd
          b <- rnd
          let u = 0.02 + 0.96 * a
              v = 0.02 + 0.96 * b
              c = trueLabel kind u v
          f <- rnd
          pure (pt2 u v, if f < flipP then 1 - c else c)

makeData2D :: Int -> Int -> Double -> Int -> DataSet
makeData2D kind n flipP seed = withSeed 1 (makeData2DR kind n flipP seed)

-- ---- 描く道具：色（オーケストラ：えんじ色の黒、象牙色、金管の金、弦の青緑）、領域の絵、点
type RGB = (Double, Double, Double)

bgCol, ink, gold, teal, wine :: RGB
bgCol = (24, 11, 16)                          -- 背景（えんじ色の黒）
ink = (242, 233, 214)
gold = (226, 176, 74)
teal = (64, 186, 176)
wine = (196, 72, 96)

clsCol :: Int -> RGB                          -- ★ クラス 0（金）・クラス 1（青緑）の色
clsCol c = if c == 0 then (226, 170, 70) else (60, 182, 176)

colOf :: RGB -> Color
colOf (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) 255

colA :: Int -> RGB -> Color
colA a (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) a

rnd8 :: Double -> Int
rnd8 v = max 0 (min 255 (round (fin 0 v)))

-- クラス 1 の確率 → 背景と混ぜた色（k = 濃さ）
probCol :: Double -> Double -> RGB
probCol p k = let a = clampv k 0 1 * abs (2 * p - 1) + 0.12
                  (cr, cg, cb) = clsCol (if p > 0.5 then 1 else 0)
                  (br, bg, bb) = bgCol
              in (br + (cr - br) * a, bg + (cg - bg) * a, bb + (cb - bb) * a)

-- 幅 iw × 高さ ih の画像：全部の画素に fn u v → (r, g, b) を塗る（u は右向き、v は上向き、どちらも 0〜1）
paintImage :: Int -> Int -> (Double -> Double -> RGB) -> Picture
paintImage iw ih fn = pixelImage iw ih 1 (\px py ->
  let (r, g, b) = fn ((fromIntegral px + 0.5) / fromIntegral iw) (1 - (fromIntegral py + 0.5) / fromIntegral ih)
  in (w8 r, w8 g, w8 b))
  where w8 v = fromIntegral (rnd8 v) :: Word8

-- マス目の値（行 j = 上から、列 i = 左から）を色にした画像
gridImage :: Int -> (Int -> RGB) -> Picture
gridImage cells fn = pixelImage cells cells 1 (\i j -> let (r, g, b) = fn (i + cells * j) in (w8 r, w8 g, w8 b))
  where w8 v = fromIntegral (rnd8 v) :: Word8

-- マス目の中心の点（p5.js 版の [(i + 0.5)/CELLS, 1 − (j + 0.5)/CELLS]）
cellPt :: Int -> Int -> Pt
cellPt cells q = let (j, i) = q `quotRem` cells
                 in pt2 ((fromIntegral i + 0.5) / fromIntegral cells) (1 - (fromIntegral j + 0.5) / fromIntegral cells)

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（画面の道具の座標で）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

data Box = Box { bxX :: !Float, bxY :: !Float, bxW :: !Float, bxH :: !Float }

toPx :: Box -> Double -> Double -> (Float, Float)   -- [0,1]² → 画面
toPx bx u v = (bxX bx + f2 u * bxW bx, bxY bx + (1 - f2 v) * bxH bx)

fromPx :: Box -> (Float, Float) -> (Double, Double)
fromPx bx (px, py) = (realToFrac ((px - bxX bx) / bxW bx), realToFrac (1 - (py - bxY bx) / bxH bx))

-- 点を描く（直径 r、ふちは黒）
drawPoints :: Box -> V.Vector Pt -> U.Vector Int -> Float -> Picture
drawPoints bx xs ys r = Pictures
  [ Pictures [Color (grayC 10) (circleAt px py (r + 2)), Color (colOf (clsCol (ys U.! i))) (circleAt px py r)]
  | i <- [0 .. V.length xs - 1]
  , let (px, py) = toPx bx (xAt xs i 0) (xAt xs i 1) ]

frameBox :: Box -> RGB -> Picture
frameBox bx c = Color (colOf c) (frameTL 1 (bxX bx) (bxY bx) (bxW bx) (bxH bx))

frameCol :: RGB                               -- 枠のふつうの色
frameCol = (110, 80, 85)

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

gPt :: Graph -> Double -> Double -> (Float, Float)
gPt g a b = (gX g a, gY g b)

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgbC 95 70 75) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (rgbC 175 160 150) | v <- xt ]
  ++ [ sayR (prec3 v) (grX g - 6) (gY g v + 6) 14 (rgbC 175 160 150) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 40) 14 (rgbC 215 205 190), say yl (grX g) (grY g - 8) 14 (rgbC 215 205 190) ]

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1.2 then Line pts else thickPath w pts
  _ -> Blank

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> Float -> [(Float, Float)] -> Picture
dashed wd on off pts = Pictures (concat (zipWith seg pts (drop 1 pts)))
  where seg (x1, y1) (x2, y2) =
          let l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
              k = max 1 (ceiling (l / (on + off))) :: Int
              p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
              fr = on / (on + off)
          in [ lineW wd (p (fromIntegral i / fromIntegral k)) (p ((fromIntegral i + fr) / fromIntegral k)) | i <- [0 .. k - 1] ]

-- 凡例：items = [(色, 名前)]
legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures
  [ Pictures [Color (colOf c) (rectTL x (yy - 5) 14 10), say s (x + 22) (yy + 5) 14 (rgbC 220 210 195)]
  | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * fromIntegral k ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 右揃えで上下は中央（textAlign(RIGHT, CENTER)）、左揃えで上下は中央（textAlign(LEFT, CENTER)）
sayRM, sayLM :: String -> Float -> Float -> Float -> Color -> Picture
sayRM s x y size c = sayR s x (y + size * 0.4) size c
sayLM s x y size c = say s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 maxW で折り返す文字（左上 = (x, y)）
sayWrap :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayWrap s x y maxW size c = Pictures [ say l x (y + size + fromIntegral k * size * 1.3) size c | (k, l) <- zip [0 :: Int ..] (wrapText maxW size s) ]

wrapText :: Float -> Float -> String -> [String]
wrapText maxW size s = go (words s) ""
  where
    fits t = textW t size <= maxW
    go [] cur = [cur | not (null cur)]
    go (w : ws) "" = go ws w
    go (w : ws) cur = let t = cur ++ " " ++ w in if fits t then go ws t else cur : go (w : ws) ""

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
                     else thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分（細いときは gloss の線）
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW wt a b = if wt <= 1.2 then Line [a, b] else thickLine wt a b

-- ---- 数の道具
-- 有効数字 n 桁に丸めた数（toPrecision）
roundSig :: Int -> Double -> Double
roundSig n v0'
  | v == 0 = 0
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (n - 1 - e) :: Double
                in fromIntegral (round (v * k) :: Integer) / k
  where v = fin 0 v0'

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v = let r = roundSig 3 v
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else fmtNum 6 r

-- 小数点以下 d 桁に丸めた数（+v.toFixed(d)）
roundTo :: Int -> Double -> Double
roundTo d v = fromIntegral (jsRoundI (v * 10 ^ d)) / 10 ^ d

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e9) 1e9)

-- JavaScript の toExponential(1)（例 6.7e-9）
toExp1 :: Double -> String
toExp1 v
  | isNaN v || isInfinite v = "?"
  | v == 0 = "0.0e+0"
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          pick e = let n = floor (a / (10 ^^ (e - 1)) + 1 / 2) :: Integer in (n, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= 100 then pick (e0 + 1) else if n < 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= 100 then (n1 `div` 10, e1 + 1) else (n1, e1)
          ds = show n2
      in (if v < 0 then "-" else "") ++ take 1 ds ++ "." ++ drop 1 ds ++ "e" ++ (if e2 >= 0 then "+" else "-") ++ show (abs e2)

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = all (\v -> not (isNaN v || isInfinite v))

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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 220) (envH env - 12) 14 (if all snd rs then rgbC 140 215 170 else rgbC 255 90 110)
