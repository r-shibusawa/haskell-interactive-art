-- ============================================
-- 61-09 スペクトラルクラスタリング ─ 点を「近さのグラフ」にしてから分ける。三日月や同心円もきれいに分かれる
--   ① 近さ W_ij = exp(−|x_i − x_j|² / 2σ²)（近いほど 1、遠いと 0）② 正規化ラプラシアン L = I − D^(−1/2) W D^(−1/2)（D = 各点の重みの合計）
--   ③ L の小さい固有値の固有ベクトル（ヤコビ法で計算）を並べて新しい座標にする → そこで k-means
--   2 番目の固有ベクトル = フィードラーベクトル。値の順に点を並べかえると、W の行列がブロック（グループ）に分かれて見える
--   グラフの「切れ目」を探すので、ネットワークのコミュニティ分け（シリーズ 8 の 09）と同じ考え方
--   ヤコビ法は重いので、1 フレームに 1 回の「掃き出し」ずつ進める（計算中は前の答えを表示）
-- 操作：スライダ / ← → σ / D データ / K k-means とくらべる / R 新しいデータ / クリック 点を足す（200 点まで）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.List (foldl', sortBy)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Numeric (showEFloat)
import Data.List (foldl', sort, sortBy)
import Data.Bits (shiftR, xor, (.|.))
import Data.Word (Word32)
import Control.Monad (forM, forM_, replicateM, when, foldM)
import qualified Data.Map.Strict as M
import Data.List (foldl', sort, sortBy, nub)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nPts :: Int
nPts = 120                   -- ★ 点の数（ヤコビ法は n³ に比例して重くなる）

dKinds :: [String]
dKinds = ["moons", "circles", "blobs", "aniso"]

data World = World
  { wData   :: !DS
  , wDataNo :: !Int
  , wSeedD  :: !Int
  , wKm     :: !KM                  -- くらべる k-means
  , wShowKm :: !Bool
  , wSig    :: !Slider
  , wJob    :: !(Maybe Jac)         -- 計算中のヤコビ法（Nothing = 計算おわり）
  , wWm     :: !(U.Vector Double)   -- 近さ W（n × n）
  , wEig    :: !Eig                 -- いちばん新しい固有値・固有ベクトル
  , wLab    :: !(U.Vector Int)      -- スペクトラルクラスタリングのラベル
  , wOrder  :: !(U.Vector Int)      -- フィードラーベクトルの小さい順
  , wEdge   :: !Picture             -- 近さのグラフの画像（W が変わったときだけ作りなおす）
  , wMat    :: !Picture             -- 並べかえた W の画像
  , wTest   :: ![(String, Bool)] }

main :: IO ()
main = runSketch (sketch "61-09 Spectral clustering" (world0 1280 800))
  { skBack  = rgbC 22 24 34
  , skStart = \env -> do
      printSelfTest selfTest
      let w = finishAll env (world0 (envW env) (envH env))
      U.length (wLab w) `seq` pure w { wTest = selfTest }
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: Float -> Float -> World
world0 width height = newData env (World (DS V.empty U.empty 2) 0 91 (KM V.empty U.empty 0 [] 0 []) False
                                    (makeSlider (width - 360) 140 300 0.05 0.8 0.14 "sigma (scale of nearness)" 2)
                                    Nothing U.empty (Eig [] V.empty) U.empty U.empty Blank Blank [])
  where env = (envOf width height)

-- 初めの 1 回は「計算中」を見せずに最後まで計算する
finishAll :: Env -> World -> World
finishAll env w = case wJob w of
  Nothing -> w
  Just _ -> finishAll env (step env w)

envOf :: Float -> Float -> Env
envOf width height = Env width height (0, 0) False 0 0 mempty

viewOf :: Env -> G
viewOf env = fitView 40 100 (envW env * 0.42) (envH env * 0.6) 0 0 5.0

newData :: Env -> World -> World
newData env w = rerun env w { wData = d, wKm = km }
  where
    d = withSeed 1 (makeDataset (headOr "moons" (drop (wDataNo w) dKinds)) nPts (fromIntegral (wSeedD w)))
    km = withSeed 5 (kmeansBest (dsPts d) (dsK d) 10 True 100)

-- ★ 近さのグラフ W とラプラシアン L を作って、ヤコビ法を始める（k-means の種は 17 に固定して、毎回同じ答えに）
rerun :: Env -> World -> World
rerun env w = w { wJob = Just (jacobiStart n l), wWm = wm, wEdge = edgeImage (viewOf env) pts wm }
  where
    pts = dsPts (wData w)
    n = V.length pts
    (wm, _, l) = graphOf pts (slVal (wSig w))

-- 1 フレームに 1 回の掃き出し。終わったら固有ベクトルで k-means して、行列の画像を作る
step :: Env -> World -> World
step _ w = case wJob w of
  Nothing -> w
  Just j ->
    let j' = jacobiStep 1500 j
    in if not (jDone j') then w { wJob = Just j' } else
         let eig = fixSigns (jacobiResult j')
             n = jN j'
             k = dsK (wData w)
             (lab, _) = withSeed 17 (spectralClusterE n eig k)
             fv = vecAt eig 1 n
             order = U.fromList (sortBy (\a b -> compare (fv U.! a) (fv U.! b)) [0 .. n - 1])
         in w { wJob = Nothing, wEig = eig, wLab = lab, wOrder = order, wMat = matImage (wWm w) n order }

-- m 番目の固有ベクトル（なければ 0 の列）
vecAt :: Eig -> Int -> Int -> U.Vector Double
vecAt e m n = if m < V.length (eVecs e) then eVecs e V.! m else U.replicate n 0

draw :: Env -> World -> Picture
draw env w = view env $
  [ imageIn (pX vw) (pY vw) (pW vw) (pH vw) (wEdge w)
  , Color (grayC 80) (frameTL 1 (pX vw) (pY vw) (pW vw) (pH vw))
  , drawPts vw pts (Just lab) 8 255
  -- ---- W を並べかえた行列
  , imageIn mx 120 ms ms (wMat w)
  , Color (grayC 80) (frameTL 1 mx 120 ms ms)
  , say "W sorted by u2 (brightness = W^0.25)" mx 112 14 (grayC 205)
  -- ---- フィードラーベクトル（小さい順）
  , plotAxes g1 "sorted index" "Fiedler vector u2" [0, fromIntegral (n - 1)] [read2 lo, 0, read2 hi]
  , Color (rgbaC 255 255 255 50) (lineW 1 (pX g1, gY g1 0) (pX g1 + pW g1, gY g1 0)) ]
  ++ [ Color (palC (ixI (wLab w) i)) (circleAt (gX g1 (fromIntegral r)) (gY g1 (ixD fv i)) 5) | (r, i) <- zip [0 :: Int ..] (U.toList (wOrder w)) ]
  -- ---- 固有ベクトルの空間（u2, u3）
  ++ [ Color (grayC 80) (frameTL 1 (pX g2) (pY g2) (pW g2) (pH g2)) ]
  ++ [ Color (palC (ixI (wLab w) i)) (circleAt (gX g2 (fv U.! i)) (gY g2 (ixD u3 i)) 6) | i <- [0 .. U.length fv - 1] ]
  ++ [ say "eigenvector space (x = u2, y = u3)" (pX g2) (pY g2 - 8) 14 (grayC 205)
  -- ---- 固有値
     , drawSlider (wSig w)
     , say "smallest eigenvalues of L (Jacobi)" sx 200 14 (grayC 205) ]
  ++ concat [ [ Color (if m < dsK (wData w) then rgbC 255 190 80 else rgbC 110 120 150) (rectTL (sx + fromIntegral m * 50) (280 - hgt) 36 hgt)
              , sayC (fmt 4 v) (sx + fromIntegral m * 50 + 18) (if even m then 294 else 309) 14 (grayC 180) ]
            | (m, v) <- zip [0 :: Int ..] vals6, let hgt = max 2 (f2 (60 * v / vmax)) ]
  -- ---- 文字
  ++ [ say ("Spectral clustering   " ++ dataName (headOr "" (drop (wDataNo w) dKinds)) ++ "   sigma = " ++ fmt 2 (slVal (wSig w)) ++ "   ARI: spectral " ++ fmt 3 aS ++ "   k-means " ++ fmt 3 aK) 40 40 20 (grayC 240)
     , say ("Slider or <- -> sigma   D data   K left plot " ++ (if wShowKm w then "shows k-means" else "-> k-means") ++ "   R new data   click add point" ++ (if busy then "   (computing ...)" else "")) 40 66 15 (if busy then rgbC 255 210 130 else grayC 170)
     , sayWrap "Lines = nearness graph (pairs with W > 0.05, brighter = nearer). If k eigenvalues are near 0 and the next one is clearly larger (a gap), the graph is almost cut into k parts. Too small sigma: the graph falls apart; too large: everything is connected."
               40 (pY vw + pH vw + 16) (pW vw + 20) 14 (grayC 150)
     , drawSelfTest env (wTest w) ]
  where
    vw = viewOf env
    pts = dsPts (wData w)
    n = V.length pts
    busy = maybe False (const True) (wJob w)
    lab = if wShowKm w then kmLabels (wKm w) else wLab w
    eig = wEig w
    ne = V.length (eVecs eig)
    fv = vecAt eig 1 ne
    u3 = vecAt eig 2 ne
    mx = pX vw + pW vw + 50
    ms = min (envH env * 0.36) (envW env - mx - 420)
    lo = if U.null fv then 0 else U.minimum fv
    hi = if U.null fv then 0 else U.maximum fv
    g1 = G mx (120 + ms + 50) ms 120 0 (fromIntegral (max 1 (ne - 1))) lo hi
    sx = envW env - 360
    g2 = G sx 330 300 230 (lo * 1.1) (hi * 1.1) (minU u3 * 1.1) (maxU u3 * 1.1)
    vals6 = take 6 (eVals eig)
    vmax = foldl' max 1e-12 vals6
    aS = ariScore (wLab w) (dsTruth (wData w))
    aK = ariScore (kmLabels (wKm w)) (dsTruth (wData w))
    read2 v = fromIntegral (jsRoundI (v * 100)) / 100     -- +v.toFixed(2)
    minU v = if U.null v then 0 else U.minimum v
    maxU v = if U.null v then 0 else U.maximum v

ixI :: U.Vector Int -> Int -> Int
ixI v i = if i >= 0 && i < U.length v then v U.! i else 0

ixD :: U.Vector Double -> Int -> Double
ixD v i = if i >= 0 && i < U.length v then v U.! i else 0

-- 画像 pic（pixelImage、倍率 1）を左上 (x, y)、幅 w、高さ h に引きのばして置く
imageIn :: Float -> Float -> Float -> Float -> Picture -> Picture
imageIn x y w h pic = case pic of
  Scale _ _ (Bitmap bd) -> place bd (Bitmap bd)
  Bitmap bd -> place bd pic
  _ -> Blank
  where place bd b = let (bw, bh) = bitmapSize bd in Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral bw) (-(h / fromIntegral bh)) b)

-- 近さのグラフ（W > 0.05 の組を線で）を、半分の大きさの画像に描く（p5.js 版の edgeBuf）
edgeImage :: G -> Pts -> U.Vector Double -> Picture
edgeImage g pts wm = pixelImage bw bh 1 (\x y -> let o = 3 * (y * bw + x) in (q (buf U.! o), q (buf U.! (o + 1)), q (buf U.! (o + 2))))
  where
    n = V.length pts
    bw = max 1 (round (pW g / 2)) :: Int
    bh = max 1 (round (pH g / 2)) :: Int
    q v = fromIntegral (max 0 (min 255 (round v :: Int)))
    sp p = ((gX g (ptX p) - pX g) / 2, (gY g (ptY p) - pY g) / 2)
    buf = runST $ do
      b <- UM.generate (bw * bh * 3) (\i -> case i `mod` 3 of { 0 -> 22; 1 -> 24; _ -> 34 }) :: ST s (UM.MVector s Double)
      let plot !a !x !y = when (x >= 0 && x < bw && y >= 0 && y < bh) $ do
            let o = 3 * (y * bw + x)
            mapM_ (\(c, tgt) -> UM.unsafeModify b (\v -> v + a * (tgt - v)) (o + c)) [(0, 150), (1, 160), (2, 200)]
      sequence_ [ do let (xa, ya) = sp (pts V.! i)
                         (xb, yb) = sp (pts V.! j)
                         a = realToFrac ((30 + 120 * wij) / 255) :: Double
                         steps = max 1 (ceiling (max (abs (xb - xa)) (abs (yb - ya)))) :: Int
                     mapM_ (\s -> let t = fromIntegral s / fromIntegral steps in plot a (floor (xa + (xb - xa) * t)) (floor (ya + (yb - ya) * t))) [0 .. steps]
                | i <- [0 .. n - 1], j <- [i + 1 .. n - 1], let wij = wm U.! (i * n + j), wij > 0.05 ]
      U.freeze b

-- W を u2 の順に並べかえた行列（明るさ = W^0.25、対角は 1）
matImage :: U.Vector Double -> Int -> U.Vector Int -> Picture
matImage wm n order
  | n == 0 || U.length wm /= n * n || U.length order /= n = Blank
  | otherwise = pixelImage n n 1 $ \c r ->
      let v = if r == c then 1 else (wm U.! (order U.! r * n + order U.! c)) ** 0.25
      in (q (22 + 200 * v), q (24 + 150 * v), q (34 + 220 * v))
  where q v = fromIntegral (max 0 (min 255 (floor v :: Int)))

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        vw = viewOf env
    in case sliderPress mx my (wSig w) of
         Just s -> w { wSig = s }
         Nothing
           | inBox vw mx my && V.length (dsPts (wData w)) < 200 ->
               let q = pt2 (invX vw mx) (invY vw my)
                   d = appendDS (wData w) [(q, 0)]
                   km = wKm w
               in rerun env w { wData = d, wKm = km { kmLabels = kmLabels km `U.snoc` nearestCenter q (kmCenters km) } }
           | otherwise -> w
  EventMotion p | envDown env -> let (mx, _) = toP5 env p in w { wSig = sliderDrag mx (wSig w) }
  EventKey (MouseButton LeftButton) Up _ _
    | slDrag (wSig w) -> rerun env w { wSig = sliderRelease (wSig w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> nudge 0.02
  EventKey (SpecialKey KeyLeft) Down _ _ -> nudge (-0.02)
  EventKey (Char c) Down _ _
    | c `elem` "dD" -> newData env w { wDataNo = (wDataNo w + 1) `mod` length dKinds }
    | c `elem` "kK" -> w { wShowKm = not (wShowKm w) }
    | c `elem` "rR" -> newData env w { wSeedD = wSeedD w + 1 }
  _ -> w
  where
    nudge dv = let s = wSig w in rerun env w { wSig = s { slVal = clampv (slLo s) (slHi s) (slVal s + dv) } }

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("Jacobi: L v = lambda v (error " ++ expo1 res ++ "), eigenvectors orthonormal (error " ++ expo1 orth ++ ")", res < 1e-9 && orth < 1e-9)
  , ("eigenvalues of L lie in 0..2, the smallest ~ 0 with eigenvector ~ sqrt(degree)", all (\v -> v > -1e-9 && v < 2 + 1e-9) (eVals (emEig e)) && abs (headOr 1 (eVals (emEig e))) < 1e-9 && cosv > 1 - 1e-9)
  , ("graph cut into exactly 3 parts: exactly 3 zero eigenvalues (4th = " ++ fmt 3 (headOr 0 (drop 3 (eVals (emEig ef)))) ++ ")", length (filter (\v -> abs v < 1e-9) (eVals (emEig ef))) == 3)
  , ("moons: spectral ARI " ++ fmt 3 aS ++ " >= 0.95, k-means ARI " ++ fmt 3 aK ++ " < 0.6", aS >= 0.95 && aK < 0.6)
  , ("splitting by the sign of the Fiedler vector alone separates the moons (ARI " ++ fmt 3 aSign ++ " >= 0.95)", aSign >= 0.95)
  , ("circles: spectral ARI " ++ fmt 3 aC ++ " >= 0.95, k-means < 0.2", aC >= 0.95 && aCk < 0.2) ]
  where
    n = 40
    pts = withSeed 9 (V.fromList <$> mapM (\_ -> do { a <- rnd; b <- rnd; pure (pt2 (a * 2) (b * 2)) }) [1 .. n])
    e = spectralEmbed pts 0.4
    ev = emEig e
    lm = emL e
    res = maximum [ abs (s - (eVals ev !! m) * v U.! i) | m <- [0 .. n - 1], let v = eVecs ev V.! m, i <- [0 .. n - 1]
                  , let s = foldl' (\acc j -> acc + lm U.! (i * n + j) * v U.! j) 0 [0 .. n - 1] ]
    orth = maximum [ abs (s - (if m == q then 1 else 0)) | m <- [0 .. n - 1], q <- [0 .. n - 1]
                   , let s = foldl' (\acc i -> acc + (eVecs ev V.! m) U.! i * (eVecs ev V.! q) U.! i) 0 [0 .. n - 1] ]
    sq = U.map sqrt (emDeg e)
    ln = sqrt (U.foldl' (\s x -> s + x * x) 0 sq)
    cosv = abs (U.foldl' (+) 0 (U.zipWith (\v s -> v * s / ln) (eVecs ev V.! 0) sq))
    far = withSeed 10 (makeBlobs [Blob 10 (0, 0) (0.2, 0.2) 0, Blob 10 (30, 0) (0.2, 0.2) 0, Blob 10 (0, 30) (0.2, 0.2) 0])
    ef = spectralEmbed (dsPts far) 0.5
    mo = withSeed 1 (makeDataset "moons" 100 92)
    em = spectralEmbed (dsPts mo) 0.14
    (smLab, _) = withSeed 3 (spectralClusterE 100 (emEig em) 2)
    kmo = withSeed 3 (kmeansBest (dsPts mo) 2 10 True 100)
    aS = ariScore smLab (dsTruth mo)
    aK = ariScore (kmLabels kmo) (dsTruth mo)
    signLab = U.map (\v -> if v > 0 then 1 else 0) (eVecs (emEig em) V.! 1)
    aSign = ariScore signLab (dsTruth mo)
    ci = withSeed 1 (makeDataset "circles" 100 93)
    (scLab, _) = withSeed 3 (spectralClusterE 100 (emEig (spectralEmbed (dsPts ci) 0.14)) 2)
    kc = withSeed 3 (kmeansBest (dsPts ci) 2 10 True 100)
    aC = ariScore scLab (dsTruth ci)
    aCk = ariScore (kmLabels kc) (dsTruth ci)

expo1 :: Double -> String                    -- toExponential(1)
expo1 v = let s = showEFloat (Just 1) v "" in case break (== 'e') s of
  (m, 'e' : '-' : ex) -> m ++ "e-" ++ ex
  (m, 'e' : ex) -> m ++ "e+" ++ ex
  _ -> s

-- ---------- やってみよう ----------
-- 1. σ をスライダで小さく・大きくしてみよう。固有値の棒（オレンジ = 小さい k 個）とすき間はどう変わる？
-- 2. 右上の W の行列が「四角いブロック」に分かれているとき、クラスタはうまく分かれている
-- 3. K で k-means の結果とくらべよう。三日月・同心円では差がはっきり
-- 4. 固有ベクトルの空間（右下）では、三日月が 2 つの「点のかたまり」になっている。だから最後の k-means がうまくいく
-- 5. シリーズ 8 の 09（コミュニティ）のネットワークにも同じ方法が使える。W の代わりにつながりの行列

-- ---- スペクトラルクラスタリング（ヤコビ法を 1 回の掃き出しずつ進められるように、このスケッチの中に書いた版）
-- 近さ W（対角 0）、次数（各点の重みの合計）、正規化ラプラシアン L。どれも n × n を行ごとに並べた配列
graphOf :: Pts -> Double -> (U.Vector Double, U.Vector Double, U.Vector Double)
graphOf pts sigma = (w, deg, l)
  where
    n = V.length pts
    s2 = 2 * sigma * sigma
    w = U.generate (n * n) (\ij -> let (i, j) = ij `divMod` n in if i == j then 0 else exp (-sqd (pts V.! i) (pts V.! j) / s2))
    deg = U.generate n (\i -> U.foldl' (+) 0 (U.slice (i * n) n w))
    isq = U.map (\x -> 1 / sqrt (max x 1e-12)) deg
    l = U.generate (n * n) (\ij -> let (i, j) = ij `divMod` n in (if i == j then 1 else 0) - isq U.! i * (w U.! ij) * isq U.! j)

data Emb = Emb { emW :: !(U.Vector Double), emDeg :: !(U.Vector Double), emL :: !(U.Vector Double), emEig :: !Eig }

spectralEmbed :: Pts -> Double -> Emb        -- 全部まとめて計算する版（自己テスト用）
spectralEmbed pts sigma = Emb w deg l (fixSigns (jacobiResult (jacobiRun (jacobiStart (V.length pts) l))))
  where (w, deg, l) = graphOf pts sigma

fixSigns :: Eig -> Eig                      -- 固有ベクトルの向き（±）をそろえる：絶対値がいちばん大きい成分を + に
fixSigns e = e { eVecs = V.map fixSign (eVecs e) }
  where fixSign v = let m = U.foldl' (\mm x -> if abs x > abs mm then x else mm) 0 v in if m < 0 then U.map negate v else v

-- 小さい固有値の固有ベクトル k 本を並べ、行ごとに長さ 1 にそろえてから k-means（ン-ジョーダン-ワイス）
spectralClusterE :: Int -> Eig -> Int -> R (U.Vector Int, Pts)
spectralClusterE n eig k = do
  let vecs = eVecs eig
      rows = V.generate n (\i -> let r = U.fromList [ vecs V.! m U.! i | m <- [0 .. min k (V.length vecs) - 1] ]
                                     l0 = sqrt (U.foldl' (\s x -> s + x * x) 0 r)
                                     l = if l0 == 0 || isNaN l0 then 1 else l0
                                 in U.map (/ l) r)
  km <- kmeansBest rows k 8 True 100
  pure (kmLabels km, rows)

-- ---- 対称行列の固有値・固有ベクトル（ヤコビ法）。A は n × n を行ごとに並べた配列。小さい順に返す
--   重いので 1 回の「掃き出し」（全部の (p, q) を 1 回ずつ回転）ごとに分けられる：jacobiStart → jacobiSweep をくり返す → jacobiResult
data Jac = Jac { jN :: !Int, jA :: !(U.Vector Double), jV :: !(U.Vector Double), jFro :: !Double, jSweeps :: !Int, jP :: !Int, jDone :: !Bool }
-- jP = 掃き出しの途中なら次に回す行 p（0 = 次の掃き出しの初め）

data Eig = Eig { eVals :: ![Double], eVecs :: !(V.Vector (U.Vector Double)) }

jacobiStart :: Int -> U.Vector Double -> Jac
jacobiStart n a = Jac n a (U.generate (n * n) (\ij -> if ij `div` n == ij `mod` n then 1 else 0)) (U.foldl' (\s x -> s + x * x) 0 a) 0 0 (n <= 1)

-- 掃き出しを少しだけ進める：(p, q) の回転を budget 組ぶんくらい（行 p の切れ目まで）。最大 60 回の掃き出し、非対角がほぼ 0 になったら終わり
jacobiStep :: Int -> Jac -> Jac
jacobiStep budget j
  | jDone j = j
  | jP j == 0 && (jSweeps j >= 60 || off <= 1e-26 * jFro j || off == 0) = j { jDone = True }
  | otherwise = let (a', v') = rotateRows (jP j) pEnd
                in j { jA = a', jV = v', jP = if pEnd >= n then 0 else pEnd, jSweeps = if pEnd >= n then jSweeps j + 1 else jSweeps j }
  where
    n = jN j
    a0 = jA j
    off = sum [ let x = U.unsafeIndex a0 (i * n + k) in x * x | i <- [0 .. n - 1], k <- [i + 1 .. n - 1] ]
    -- 行 p0 から、回転の数が budget をこえるところまで
    pEnd = let go p c = if p >= n || (c >= budget && p > jP j) then p else go (p + 1) (c + n - 1 - p) in go (jP j) 0
    rotateRows p0 p1 = runST $ do
      a <- U.thaw a0
      v <- U.thaw (jV j)
      let pq !p !q
            | p >= p1 = pure ()
            | q >= n = pq (p + 1) (p + 2)
            | otherwise = do
                apq <- UM.unsafeRead a (p * n + q)
                when (abs apq >= 1e-300) $ do
                  aqq <- UM.unsafeRead a (q * n + q)
                  app <- UM.unsafeRead a (p * n + p)
                  let th = (aqq - app) / (2 * apq)
                      t = (if th >= 0 then 1 else -1) / (abs th + sqrt (th * th + 1))
                      c = 1 / sqrt (t * t + 1)
                      s = t * c
                  rotCols a n c s p q
                  rotRows a n c s p q
                  UM.unsafeWrite a (p * n + q) 0
                  UM.unsafeWrite a (q * n + p) 0
                  rotCols v n c s p q
                pq p (q + 1)
      pq p0 (p0 + 1)
      (,) <$> U.unsafeFreeze a <*> U.unsafeFreeze v

-- 1 回の掃き出しを全部
jacobiSweep :: Jac -> Jac
jacobiSweep = jacobiStep maxBound

-- 列 p と列 q を回す（k = 0〜n−1）
rotCols :: UM.MVector s Double -> Int -> Double -> Double -> Int -> Int -> ST s ()
rotCols m !n !c !s !p !q = go 0
  where
    go !k | k >= n = pure ()
          | otherwise = do
              x <- UM.unsafeRead m (k * n + p)
              y <- UM.unsafeRead m (k * n + q)
              UM.unsafeWrite m (k * n + p) (c * x - s * y)
              UM.unsafeWrite m (k * n + q) (s * x + c * y)
              go (k + 1)

-- 行 p と行 q を回す
rotRows :: UM.MVector s Double -> Int -> Double -> Double -> Int -> Int -> ST s ()
rotRows m !n !c !s !p !q = go 0
  where
    go !k | k >= n = pure ()
          | otherwise = do
              x <- UM.unsafeRead m (p * n + k)
              y <- UM.unsafeRead m (q * n + k)
              UM.unsafeWrite m (p * n + k) (c * x - s * y)
              UM.unsafeWrite m (q * n + k) (s * x + c * y)
              go (k + 1)

-- 終わるまで掃き出す
jacobiRun :: Jac -> Jac
jacobiRun j = if jDone j then j else jacobiRun (jacobiSweep j)

jacobiResult :: Jac -> Eig
jacobiResult j = Eig [ d U.! i | i <- idx ] (V.fromList [ U.generate n (\k -> jV j U.! (k * n + i)) | i <- idx ])
  where
    n = jN j
    d = U.generate n (\i -> jA j U.! (i * n + i))
    idx = sortBy (\i k -> compare (d U.! i) (d U.! k)) [0 .. n - 1]

-- ==================================================
-- ここから下は「クラスタリングの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。点は U.Vector Double（[x, y] や [r, g, b]）、
-- 点の集まりは V.Vector（p5.js 版の配列の配列）、ラベルは U.Vector Int（−1 = ノイズ）
-- ==================================================
-- ---- 共通：クラスタの色・距離（点は [x, y] や [r, g, b] のような U.Vector Double）
type P = U.Vector Double
type Pts = V.Vector P

pt2 :: Double -> Double -> P                -- 2 次元の点 [x, y]
pt2 x y = U.fromList [x, y]

ptX, ptY :: P -> Double                     -- x 座標・y 座標（なければ 0）
ptX p = if U.length p > 0 then U.unsafeIndex p 0 else 0
ptY p = if U.length p > 1 then U.unsafeIndex p 1 else 0

palette :: [(Int, Int, Int)]
palette = [(176, 140, 255), (110, 230, 190), (255, 190, 80), (110, 180, 255), (255, 120, 130), (200, 240, 110), (255, 150, 220), (90, 210, 220), (255, 150, 70), (190, 190, 255)]

palRGB :: Int -> (Int, Int, Int)            -- クラスタ k の色（k < 0 = ノイズ = 灰色）
palRGB k | k < 0 = (120, 120, 135)
         | otherwise = headOr (0, 0, 0) (drop (k `mod` length palette) palette)

palC :: Int -> Color
palC k = palCA k 255

palCA :: Int -> Int -> Color                -- 不透明度 a（0〜255）つき
palCA k a = let (r, g, b) = palRGB k in rgbaC r g b a

sqd :: P -> P -> Double                     -- 距離の 2 乗
sqd a b = go 0 0
  where
    n = min (U.length a) (U.length b)
    go !d !s | d >= n = s
             | otherwise = let e = U.unsafeIndex a d - U.unsafeIndex b d in go (d + 1) (s + e * e)
{-# INLINE sqd #-}

edist :: P -> P -> Double                   -- ユークリッド距離
edist a b = sqrt (sqd a b)

meanOf :: [Double] -> Double
meanOf [] = 0
meanOf xs = foldl' (+) 0 xs / fromIntegral (length xs)

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ。gauss の 2 つ目の値もとっておく）
data Rng = Rng !Word32 !(Maybe Double)

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s) Nothing

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s sp) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (fromIntegral (t2 `xor` (t2 `shiftR` 14)) :: Int) / 4294967296, Rng a sp)   -- Int を通すと速い

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> let (a, s1) = g s in runR (k a) s1)

-- 種 s で始めて結果だけ取り出す（seedRng(s) してから計算）
withSeed :: Word32 -> R a -> a
withSeed s m = fst (runR m (seedRng s))

seedR :: Word32 -> R ()                     -- 計算の途中で seedRng(s)
seedR s = R (\_ -> ((), seedRng s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int               -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

gauss :: R Double                           -- 標準正規分布（ボックス-ミュラー法）
gauss = R $ \(Rng s sp) -> case sp of
  Just g -> (g, Rng s Nothing)
  Nothing ->
    let (u0, r1) = rndStep (Rng s Nothing)
        (v, Rng s2 _) = rndStep r1
        u = 1 - u0
        r = sqrt (-2 * log u)
    in (r * cos (2 * pi * v), Rng s2 (Just (r * sin (2 * pi * v))))

-- ---- 合成データ（種つき）。どれも DS { 点, 正解のグループ番号, 正解のグループ数 } を返す
-- blob = 点の数、中心 (x, y)、(長い向きの標準偏差, 短い向き)、傾き（ラジアン）
data Blob = Blob { bN :: !Int, bC :: !(Double, Double), bSd :: !(Double, Double), bRot :: !Double }
data DS = DS { dsPts :: !Pts, dsTruth :: !(U.Vector Int), dsK :: !Int }

makeBlobs :: [Blob] -> R DS
makeBlobs specs = do
  groups <- forM (zip [0 ..] specs) $ \(k, s) -> do
    let cr = cos (bRot s)
        sr = sin (bRot s)
        (cx, cy) = bC s
        (s0, s1) = bSd s
    replicateM (bN s) $ do
      a <- (* s0) <$> gauss
      b <- (* s1) <$> gauss
      pure (pt2 (cx + cr * a - sr * b) (cy + sr * a + cr * b), k)
  let ps = concat groups
  pure (DS (V.fromList (map fst ps)) (U.fromList (map snd ps)) (length specs))

makeMoons :: Int -> Double -> R DS          -- 2 つの三日月（かみ合った半円）
makeMoons n noise = do
  ps <- forM [0 .. n - 1] $ \i -> do
    t <- (* pi) <$> rnd
    let k = i `mod` 2
        x = if k == 1 then 1 - cos t else cos t
        y = if k == 1 then 0.5 - sin t else sin t
    gx <- gauss
    gy <- gauss
    pure (pt2 ((x - 0.5) * 1.6 + gx * noise) ((y - 0.25) * 1.6 + gy * noise), k)
  pure (DS (V.fromList (map fst ps)) (U.fromList (map snd ps)) 2)

makeCircles :: Int -> Double -> Double -> R DS   -- 同心円（外の輪と内の輪。点の数は輪の長さに比例）
makeCircles n noise inner = do
  let nOut = jsRoundI (fromIntegral n * 1.3 / (1.3 + inner))
  ps <- forM [0 .. n - 1] $ \i -> do
    let k = if i < nOut then 0 else 1
        r = if k == 1 then inner else 1.3
    t <- (* (2 * pi)) <$> rnd
    gx <- gauss
    gy <- gauss
    pure (pt2 (r * cos t + gx * noise) (r * sin t + gy * noise), k)
  pure (DS (V.fromList (map fst ps)) (U.fromList (map snd ps)) 2)

dataName :: String -> String                -- 画面に出すデータの名前
dataName kind = case kind of
  "blobs" -> "blobs"
  "moons" -> "moons"
  "circles" -> "circles"
  "aniso" -> "elongated"
  "density" -> "different densities"
  "sizes" -> "different sizes"
  "noise" -> "blobs + outliers"
  "chain" -> "chained"
  _ -> kind

appendDS :: DS -> [(P, Int)] -> DS          -- 点を後ろに足す
appendDS d extra = d { dsPts = dsPts d V.++ V.fromList (map fst extra), dsTruth = dsTruth d U.++ U.fromList (map snd extra) }

makeDataset :: String -> Int -> Word32 -> R DS
makeDataset kind n seed = do
  seedR seed
  case kind of
    "moons" -> makeMoons n 0.07
    "circles" -> makeCircles n 0.05 0.55
    "aniso" -> let m = n `div` 3 in makeBlobs [Blob m (-0.6, 0.9) (0.9, 0.1) 0.5, Blob m (0, 0) (0.9, 0.1) 0.5, Blob (n - 2 * m) (0.6, -0.9) (0.9, 0.1) 0.5]
    "density" -> let m = floor (fromIntegral n * 0.45 :: Double) in makeBlobs [Blob m (-0.9, 0.3) (0.12, 0.12) 0, Blob m (-0.3, 0.3) (0.12, 0.12) 0, Blob (n - 2 * m) (0.9, -0.2) (0.65, 0.65) 0]
    "sizes" -> let m = floor (fromIntegral n * 0.12 :: Double) in makeBlobs [Blob (n - 2 * m) (-0.5, 0) (0.6, 0.6) 0, Blob m (0.85, 0.75) (0.1, 0.1) 0, Blob m (0.85, -0.75) (0.1, 0.1) 0]
    "chain" -> do
      let m = floor (fromIntegral n * 0.4 :: Double)
          rest = n - 2 * m
      d <- makeBlobs [Blob m (-1.2, 0) (0.25, 0.25) 0, Blob m (1.2, 0) (0.25, 0.25) 0]
      extra <- forM [0 .. rest - 1] $ \i -> do
        g <- gauss
        pure (pt2 (-0.8 + 1.6 * (fromIntegral i + 0.5) / fromIntegral rest) (0.04 * g), if fromIntegral i < (fromIntegral rest / 2 :: Double) then 0 else 1)
      pure (appendDS d extra)
    _ -> do
      let m = n `div` 3
      d <- makeBlobs [Blob m (-1, 0.7) (0.25, 0.25) 0, Blob m (1, 0.6) (0.3, 0.22) 0.4, Blob (n - 2 * m) (0, -0.9) (0.28, 0.28) 0]
      if kind == "noise"
        then do
          let q = floor (fromIntegral n * 0.08 :: Double)
          extra <- replicateM q $ do
            x <- rnd
            y <- rnd
            pure (pt2 (-2 + 4 * x) (-1.8 + 3.6 * y), -1)
          pure (appendDS d extra)
        else pure d

-- ---- k-means（ロイドのアルゴリズム）。目的関数 = 慣性 = Σ |x_i − 中心|²
nearestCenter :: P -> Pts -> Int
nearestCenter p cs = go 0 0 (1 / 0)
  where
    k = V.length cs
    go !j !b !bd
      | j >= k = b
      | otherwise = let d = sqd p (V.unsafeIndex cs j) in if d < bd then go (j + 1) j d else go (j + 1) b bd

assignLabels :: Pts -> Pts -> U.Vector Int  -- ① 割り当て
assignLabels pts cs = U.generate (V.length pts) (\i -> nearestCenter (V.unsafeIndex pts i) cs)

inertiaOf :: Pts -> Pts -> U.Vector Int -> Double
inertiaOf pts cs labels = U.ifoldl' (\s i l -> s + sqd (pts V.! i) (cs V.! l)) 0 (U.take (V.length pts) labels)

-- ② 更新（仲間の平均。仲間 0 なら動かさない）
updateCenters :: Pts -> U.Vector Int -> Pts -> Pts
updateCenters pts labels cs
  | V.null cs = cs
  | otherwise = V.generate k (\j -> let c = cnt U.! j in if c > 0 then U.generate dim (\d -> sums U.! (j * dim + d) / fromIntegral c) else cs V.! j)
  where
    k = V.length cs
    dim = U.length (V.head cs)
    cnt = U.accumulate (+) (U.replicate k (0 :: Int)) (U.map (\l -> (l, 1)) labels)
    sums = runST $ do
      s <- UM.replicate (k * dim) 0
      forM_ [0 .. V.length pts - 1] $ \i -> do
        let l = labels U.! i
            p = pts V.! i
        forM_ [0 .. min dim (U.length p) - 1] $ \d -> UM.unsafeModify s (+ U.unsafeIndex p d) (l * dim + d)
      U.freeze s

initRandomCenters :: Pts -> Int -> R Pts    -- ランダムに k 点えらぶ（重ならないように）
initRandomCenters pts k
  | V.null pts = pure (V.replicate k (pt2 0 0))
  | otherwise = go 0 (U.enumFromN 0 n) []
  where
    n = V.length pts
    go j idx out
      | j >= min k n = pure (V.fromList (reverse out ++ replicate (k - length out) (V.head pts)))
      | otherwise = do
          u <- rnd
          let r = j + floor (u * fromIntegral (n - j))
              idx' = idx U.// [(j, idx U.! r), (r, idx U.! j)]
          go (j + 1) idx' (pts V.! (idx' U.! j) : out)

-- k-means++：今ある中心から遠い点ほど（距離² に比例して）えらばれやすい
initPlusPlus :: Pts -> Int -> R Pts
initPlusPlus pts k
  | V.null pts = pure (V.replicate k (pt2 0 0))
  | otherwise = do
      u <- rnd
      let c0 = pts V.! min (n - 1) (floor (u * fromIntegral n))
      loop [c0] (U.generate n (\i -> sqd (pts V.! i) c0))
  where
    n = V.length pts
    loop out d2
      | length out >= k = pure (V.fromList (reverse out))
      | otherwise = do
          let tot = U.foldl' (+) 0 d2
          u <- rnd
          pick <- if tot > 0 then pure (scan (u * tot) 0) else (\v -> min (n - 1) (floor (v * fromIntegral n))) <$> rnd
          let c = pts V.! pick
          loop (c : out) (U.imap (\i v -> min v (sqd (pts V.! i) c)) d2)
      where scan r i | i >= n = n - 1
                     | r - d2 U.! i <= 0 = i
                     | otherwise = scan (r - d2 U.! i) (i + 1)

-- k-means の結果。hist = 半ステップ（割り当て・更新）ごとの慣性、all = やり直しごとの慣性
data KM = KM { kmCenters :: !Pts, kmLabels :: !(U.Vector Int), kmInertia :: !Double, kmHist :: ![Double], kmIters :: !Int, kmAll :: ![Double] }

-- 最後まで回す（maxIter 回まで。0 なら 100 回）
lloyd :: Pts -> Pts -> Int -> KM
lloyd pts c0 maxIter = go 0 c0 l0 [inertiaOf pts c0 l0]
  where
    mi = if maxIter <= 0 then 100 else maxIter
    l0 = assignLabels pts c0
    go it cs ls hist
      | it >= mi = finish it cs ls hist
      | otherwise =
          let nc = updateCenters pts ls cs
              h1 = inertiaOf pts nc ls
              nl = assignLabels pts nc
              h2 = inertiaOf pts nc nl
              hist' = h2 : h1 : hist
          in if nl == ls then finish (it + 1) nc nl hist' else go (it + 1) nc nl hist'
    finish it cs ls hist = KM cs ls (headOr 0 hist) (reverse hist) it []

-- restarts 回やり直して慣性がいちばん小さいものを選ぶ（usePP = k-means++ で始める）
kmeansBest :: Pts -> Int -> Int -> Bool -> Int -> R KM
kmeansBest pts k restarts usePP maxIter = do
  rs <- forM [1 .. max 1 restarts] $ \_ -> do
    c0 <- if usePP then initPlusPlus pts k else initRandomCenters pts k
    pure (lloyd pts c0 maxIter)
  let best = foldl' (\b r -> if kmInertia r < kmInertia b then r else b) (headOr (KM V.empty U.empty 0 [] 0 []) rs) (drop 1 rs)
  pure best { kmAll = map kmInertia rs }

-- ---- 正解との一致：分割表、純度、調整ランド指数（ARI）、正規化相互情報量（NMI）。ラベル −1（ノイズ）も 1 つのグループとして数える
-- 分割表 M（行 = a のグループ、列 = b のグループ。どちらも小さい番号順）
contingency :: U.Vector Int -> U.Vector Int -> ([[Int]], [Int], [Int])
contingency a b = ([ [ M.findWithDefault 0 (x, y) cnt | y <- cb ] | x <- ra ], ra, cb)
  where
    ra = M.keys (M.fromList [ (x, ()) | x <- U.toList a ])
    cb = M.keys (M.fromList [ (y, ()) | y <- U.toList b ])
    cnt = M.fromListWith (+) [ ((x, y), 1 :: Int) | (x, y) <- zip (U.toList a) (U.toList b) ]

comb2 :: Double -> Double
comb2 x = x * (x - 1) / 2

ariScore :: U.Vector Int -> U.Vector Int -> Double
ariScore a b
  | U.null a = 1
  | otherwise = if maxv - expv == 0 then 1 else (sij - expv) / (maxv - expv)
  where
    (mt, _, _) = contingency a b
    n = fromIntegral (U.length a)
    sij = foldl' (+) 0 [ comb2 (fromIntegral v) | r <- mt, v <- r ]
    sa = foldl' (+) 0 [ comb2 (fromIntegral (sum r)) | r <- mt ]
    sb = foldl' (+) 0 [ comb2 (fromIntegral v) | v <- foldr (zipWith (+)) (map (const 0) (headOr [] mt)) mt ]
    expv = sa * sb / max 1 (comb2 n)
    maxv = (sa + sb) / 2

nmiScore :: U.Vector Int -> U.Vector Int -> Double   -- NMI = 2·I(A; B) / (H(A) + H(B))
nmiScore a b
  | U.null a = 1
  | otherwise = let hs = h rs + h cs in if hs == 0 then 1 else clampv 0 1 (2 * mi / hs)
  where
    (mt, _, _) = contingency a b
    n = fromIntegral (U.length a) :: Double
    rs = [ fromIntegral (sum r) | r <- mt ] :: [Double]
    cs = map fromIntegral (foldr (zipWith (+)) (map (const 0) (headOr [] mt)) mt) :: [Double]
    h arr = negate (foldl' (\s v -> s + (if v > 0 then (v / n) * log (v / n) else 0)) 0 arr)
    mi = foldl' (+) 0 [ (v / n) * log (n * v / (ri * cj)) | (r, ri) <- zip mt rs, (vi, cj) <- zip r cs, let v = fromIntegral vi, v > 0 ]

purityScore :: U.Vector Int -> U.Vector Int -> Double
purityScore pred' truth
  | U.null pred' = 0
  | otherwise = let (mt, _, _) = contingency pred' truth in fromIntegral (sum (map (foldl' max 0) mt)) / fromIntegral (U.length pred')

-- ---- かんたんなスライダ（ドラッグで値を変える）。座標は p5.js と同じ（左上が原点）
data Slider = Slider { slX, slY, slW :: !Float, slLo, slHi, slVal :: !Double, slName :: !String, slDigits :: !Int, slDrag :: !Bool }

makeSlider :: Float -> Float -> Float -> Double -> Double -> Double -> String -> Int -> Slider
makeSlider x y w lo hi val name digits = Slider x y w lo hi val name digits False

drawSlider :: Slider -> Picture
drawSlider s = Pictures
  [ Color (grayC 90) (lineW 4 (slX s, slY s) (slX s + slW s, slY s))
  , Color (rgbC 176 140 255) (lineW 4 (slX s, slY s) (slX s + u * slW s, slY s))
  , Color (grayC (if slDrag s then 255 else 220)) (circleAt (slX s + u * slW s) (slY s) 16)
  , say (slName s ++ " = " ++ fmt (slDigits s) (slVal s)) (slX s) (slY s - 12) 14 (grayC 200) ]
  where u = f2 ((slVal s - slLo s) / (slHi s - slLo s))

-- 押した場所がスライダの上なら、つかんで値を変える（Nothing = スライダの外）
sliderPress :: Float -> Float -> Slider -> Maybe Slider
sliderPress mx my s
  | abs (my - slY s) < 14 && mx > slX s - 10 && mx < slX s + slW s + 10 = Just (sliderDrag mx s { slDrag = True })
  | otherwise = Nothing

sliderDrag :: Float -> Slider -> Slider
sliderDrag mx s
  | not (slDrag s) = s
  | otherwise = let v = slLo s + clampv 0 1 (realToFrac ((mx - slX s) / slW s)) * (slHi s - slLo s)
                in s { slVal = if slDigits s == 0 then jsRound v else v }

sliderRelease :: Slider -> Slider
sliderRelease s = s { slDrag = False }

-- ---- グラフと図の道具：G = 画面の四角（左上 x, y、幅、高さ）とデータの範囲（p5.js と同じ、左上が原点）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv (-0.02) 1.02 ((v - x0 g) / (x1 g - x0 g))) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv (-0.02) 1.02 ((v - y0 g) / (y1 g - y0 g))) * pH g

invX :: G -> Float -> Double                -- 画面の x → データの x
invX g sx = x0 g + realToFrac ((sx - pX g) / pW g) * (x1 g - x0 g)

invY :: G -> Float -> Double
invY g sy = y0 g + realToFrac ((pY g + pH g - sy) / pH g) * (y1 g - y0 g)

inBox :: G -> Float -> Float -> Bool
inBox g sx sy = sx >= pX g && sx <= pX g + pW g && sy >= pY g && sy <= pY g + pH g

-- 縦横の縮尺をそろえる（span = 短い辺に入るデータの幅、(cx, cy) = まん中）
fitView :: Float -> Float -> Float -> Float -> Double -> Double -> Double -> G
fitView x y w h cx cy sp = G x y w h (cx - hw) (cx + hw) (cy - hh) (cy + hh)
  where
    s = realToFrac (min w h) / sp
    hw = realToFrac w / 2 / s
    hh = realToFrac h / 2 / s

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (grayC 95) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 18) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 38) 14 (grayC 205), say yl (pX g) (pY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- クラスタの色で点を描く（ラベルがなければ白）。r = 直径、al = 不透明度（0〜255）
drawPts :: G -> Pts -> Maybe (U.Vector Int) -> Float -> Int -> Picture
drawPts g pts labels r al = Pictures
  [ Color (maybe (rgbaC 230 230 240 al) (\ls -> palCA (if i < U.length ls then ls U.! i else 0) al) labels) (circleAt (gX g (ptX p)) (gY g (ptY p)) r)
  | (i, p) <- zip [0 ..] (V.toList pts) ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

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
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double   -- clampv 下限 上限 値
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = let s = showFFloat (Just n) (fin 0 v) "" in if all (`elem` "-0.") s && take 1 s == "-" then drop 1 s else s

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
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (jsRoundI v)

jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = not . any isNaN

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下に小さく（view の中で使う）
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
