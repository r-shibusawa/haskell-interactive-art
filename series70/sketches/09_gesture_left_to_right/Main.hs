-- ============================================
-- 70-09 左から右へ進む HMM でジェスチャ認識 ─ マウスで描いた線を 24 本の短い矢印に分け、それぞれを 8 方向の記号（→ ↘ ↓ ↙ ← ↖ ↑ ↗）にする。
-- ジェスチャごと（丸・三角・Z・L）に 1 つずつ HMM を用意する。状態は 6 個で、「とどまる・次へ・1 つ飛ばす」しかできない（左から右へ。バキス型）。
-- 描いた記号の列を 4 つの HMM に入れて尤度 P(x | モデル) をくらべ、いちばん大きいジェスチャと判定する。
-- 音声認識で「単語ごとに HMM を作り、いちばん尤度の高い単語を答える」のと同じ考え方（状態 = 単語の中の音の順番）
-- 操作：ドラッグ 描く / スペース お手本を自動で描く / 1〜4 教えるジェスチャを選ぶ → T 今の線をそのお手本に追加して学習 / C 追加したお手本を忘れる
-- ※ 描きはじめの位置と向きも大事：丸は上から時計回り、三角は上から右下へ、Z は左上から、L は上から下へ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (Path, rose)
import Data.Array.Unboxed (UArray, accumArray, listArray, (!))
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', sortBy)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (floatToDigits)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)
import qualified Data.Vector as V
import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed.Mutable as MU
gNames :: [String]
gNames = ["circle", "triangle", "Z", "L"]      -- 丸・三角・Z・L

nSeg, nSt, nTrain :: Int
nSeg = 24                                     -- ★ 線を何本の矢印に分けるか
nSt = 6                                       -- ★ 1 つのジェスチャの状態の数
nTrain = 12                                   -- ★ はじめに自動で作るお手本の数（ジェスチャごと）

wobble, epsEm :: Double
wobble = 0.02                                 -- ★ 自動で作るお手本の「手のふるえ」の大きさ
epsEm = 0.05                                  -- ★ 出力の数に足す小さな数（1 度も出なかった方向でも確率 0 にしない）

dirNames :: [String]                          -- 8 方向（→ ↘ ↓ ↙ ← ↖ ↑ ↗）。画面は東西南北の頭文字（y は下向き）
dirNames = ["E", "SE", "S", "SW", "W", "NW", "N", "NE"]

type Pt = (Double, Double)

-- 判定の結果：いちばんのジェスチャ（−1 = 判定できない）と、4 つの対数尤度
data Verdict = Verdict { vBest :: !Int, vScores :: [Double] }

data World = World
  { wModels  :: [Hmm]
  , wSets    :: [[[Int]]]                     -- ジェスチャごとのお手本（記号の列）
  , wAdded   :: [Int]
  , wDrawing :: [(Float, Float)]              -- 描いている線（新しい順）
  , wStroke  :: Maybe [Pt]                    -- 最後の線
  , wSyms    :: Maybe [Int]
  , wVerdict :: Verdict
  , wTeach   :: !Int
  , wDown    :: !Bool
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "70-09 Gesture recognition with left-to-right HMMs" (world0 env1))
  { skBack  = col bgc
  , skStart = \env -> do
      printSelfTest selfTest
      let w = world0 env
      _ <- evaluate (sum (map (sumOf . concat . hEm) (wModels w)) + sumOf (vScores (wVerdict w)))
      pure w { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event }
  where env1 = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: Env -> World
world0 env = judge (demoStroke env (resetTraining (World [] [] [0, 0, 0, 0] [] Nothing Nothing (Verdict (-1) []) 0 False (rngOf 79) [])))

resetTraining :: World -> World
resetTraining w =
  let (sets, g1) = runWith (wRng w) (mapM (\c -> replicateR nTrain (fmap strokeToSymsJ (synthStroke c wobble))) [0 .. 3])
      (models, g2) = runWith g1 (mapM (\s -> fmap fst (trainClass s 15 epsEm)) sets)
  in w { wSets = sets, wAdded = [0, 0, 0, 0], wModels = models, wRng = g2 }

-- お手本の線は短すぎることがないので、記号の列は必ずある
strokeToSymsJ :: [Pt] -> [Int]
strokeToSymsJ = maybe [] id . strokeToSyms . Just

templatePts :: Int -> [Pt]                    -- お手本の形（0〜1 の座標、y は下向き）
templatePts c
  | c == 0 = [ (0.5 + 0.5 * cos a, 0.5 + 0.5 * sin a) | k <- [0 .. 32 :: Int], let a = -pi / 2 + (i2d k / 32) * 2 * pi ]
  | c == 1 = [(0.5, 0), (1, 1), (0, 1), (0.5, 0)]
  | c == 2 = [(0, 0), (1, 0), (0, 1), (1, 1)]
  | otherwise = [(0, 0), (0, 1), (0.8, 1)]

-- ★ お手本に「人が描いたようなゆれ」を足す：大きさ・かたむき・点のふるえ
synthStroke :: Int -> Double -> R [Pt]
synthStroke c wob = do
  let base = maybe [] id (resample (templatePts c) 60)
  sx0 <- rnd
  sy0 <- rnd
  r0 <- rnd
  let sx = 0.8 + 0.4 * sx0
      sy = 0.8 + 0.4 * sy0
      rot = (r0 - 0.5) * 0.35
      cs = cos rot
      sn = sin rot
  mapM (\(x, y) -> do
          let u = (x - 0.5) * sx
              v = (y - 0.5) * sy
          n1 <- randn
          n2 <- randn
          pure (u * cs - v * sn + wob * n1, u * sn + v * cs + wob * n2)) base

-- Math.hypot と同じ計算（いちばん大きい値で割って、カハンの足し算）
jsHypot :: Double -> Double -> Double
jsHypot a b =
  let xs = [abs a, abs b]
      mx = maximum xs
      go (s, comp) v = let n = v / mx
                           summand = n * n - comp
                           pre = s + summand
                       in (pre, (pre - s) - summand)
  in if isInfinite mx then posInf else if mx == 0 then 0 else sqrt (fst (foldl' go (0, 0) xs)) * mx

-- ★ 線を「長さが等しい n 本」に分け直す（速く描いた所も遅く描いた所も同じあつかいに）。短すぎる線は Nothing
resample :: [Pt] -> Int -> Maybe [Pt]
resample pts n
  | length pts < 2 = Nothing
  | not (total > 1e-9) = Nothing
  | otherwise = Just (go 0 0)
  where
    pv = V.fromList pts
    np = V.length pv
    accL = scanl (+) 0 [ jsHypot (x2 - x1) (y2 - y1) | ((x1, y1), (x2, y2)) <- zip pts (drop 1 pts) ]
    acc = U.fromList accL
    total = U.last acc
    go s k
      | s > n = []
      | otherwise =
          let target = (i2d s / i2d n) * total
              k' = advance k target
              seg = acc U.! (k' + 1) - acc U.! k'
              f = if seg > 0 then clampv ((target - acc U.! k') / seg) 0 1 else 0
              (xa, ya) = pv V.! k'
              (xb, yb) = pv V.! (k' + 1)
          in (xa + (xb - xa) * f, ya + (yb - ya) * f) : go (s + 1) k'
    advance k target = if k < np - 2 && acc U.! (k + 1) < target then advance (k + 1) target else k

-- ★ となりの点への向きを 8 方向の番号に（0 = 右、2 = 下、4 = 左、6 = 上）
dirSymbols :: [Pt] -> [Int]
dirSymbols pts = [ ((jsRound (atan2 (y2 - y1) (x2 - x1) / (pi / 4)) `rem` 8) + 8) `rem` 8 | ((x1, y1), (x2, y2)) <- zip pts (drop 1 pts) ]

strokeToSyms :: Maybe [Pt] -> Maybe [Int]
strokeToSyms mp = mp >>= \p -> fmap dirSymbols (resample p nSeg)

-- ★ 左から右への HMM の出発点：遷移は「とどまる 0.6・次へ 0.3・1 つ飛ばす 0.1」だけ。0 のところは EM をしても 0 のまま
bakisStart :: Int -> Int -> R Hmm
bakisStart n k = do
  rows <- mapM (\i -> do
                  let row = [ if j == i then 0.6 else if j == i + 1 then 0.3 else if j == i + 2 then 0.1 else 0 | j <- [0 .. n - 1] ]
                  e <- replicateR k (fmap (\u -> 1 + 0.2 * u) rnd)
                  pure (normRow row, normRow e)) [0 .. n - 1]
  pure (Hmm (1 : replicate (n - 1) 0) (map fst rows) (map snd rows))

-- 学習：(モデル, 各回の対数尤度)
trainClass :: [[Int]] -> Int -> Double -> R (Hmm, [Double])
trainClass seqs iters eps = do
  m0 <- bakisStart nSt 8
  let go' (m, lls) _ = let (m', ll) = baumWelchFast m seqs eps in m' `seq` (m', ll : lls)
      (mEnd, llsR) = foldl' go' (m0, []) [1 .. iters]
  pure (mEnd, reverse llsR)

-- 道具の baumWelchStep と同じ計算を、同じ順番の足し算で速くしたもの（配列を使う。結果はまったく同じ数になる）
baumWelchFast :: Hmm -> [[Int]] -> Double -> (Hmm, Double)
baumWelchFast m seqs eps = runST $ do
  g0 <- MU.replicate n 0
  trN <- MU.replicate (n * n) 0
  emN <- MU.replicate (n * k) eps
  llRef <- MU.replicate 1 0
  forM_ (filter (not . null) seqs) $ \xsL -> do
    let xs = U.fromList xsL
        len = U.length xs
        e t j = emA U.! (j * k + xs U.! t)
        fwd = go 0 Nothing [] [] 0
        go t prev ahs cs !ll
          | t >= len = (reverse ahs, reverse cs, ll)
          | otherwise =
              let pred' = case prev of
                    Nothing -> p0A
                    Just v -> U.generate n (\j -> U.ifoldl' (\acc i x -> if x == 0 then acc else acc + x * trA U.! (i * n + j)) 0 v)
                  row = U.imap (\j v -> v * e t j) pred'
                  c = U.foldl' (+) 0 row
              in if not (c > 0) then let r = U.replicate n (1 / i2d n) in go (t + 1) (Just r) (r : ahs) (0 : cs) negInf
                 else let r = U.map (/ c) row in go (t + 1) (Just r) (r : ahs) (c : cs) (if ll /= negInf then ll + log c else ll)
        (ahL, csL, logL) = fwd
        ahV = V.fromList ahL
        csU = U.fromList csL
        bwd t nb acc
          | t < 0 = acc
          | otherwise =
              let c0 = csU U.! (t + 1)
                  c = if c0 > 0 then c0 else 1
                  b = U.generate n (\i -> U.foldl' (+) 0 (U.generate n (\j -> trA U.! (i * n + j) * e (t + 1) j * nb U.! j)) / c)
              in bwd (t - 1) b (b : acc)
        bhV = V.fromList (bwd (len - 2) (U.replicate n 1) [U.replicate n 1])
        gam t = let g = U.zipWith (*) (ahV V.! t) (bhV V.! t)
                    sg = U.foldl' (+) 0 g
                in if sg > 1e-300 then U.map (/ sg) g else U.replicate n (1 / i2d n)
    ll0 <- MU.read llRef 0
    MU.write llRef 0 (ll0 + logL)
    when (logL /= negInf) $ do
      let g0v = gam 0
      forM_ [0 .. n - 1] $ \i -> MU.modify g0 (+ (g0v U.! i)) i
      forM_ [0 .. len - 1] $ \t -> do
        let gv = gam t
            x = xs U.! t
        forM_ [0 .. n - 1] $ \i -> MU.modify emN (+ (gv U.! i)) (i * k + x)
      forM_ [0 .. len - 2] $ \t -> do
        let ft = ahV V.! t
            b1 = bhV V.! (t + 1)
            c = csU U.! (t + 1)
        forM_ [0 .. n - 1] $ \i -> do
          let a = ft U.! i
          when (a /= 0) $ forM_ [0 .. n - 1] $ \j ->
            MU.modify trN (+ ((a * trA U.! (i * n + j) * e (t + 1) j * b1 U.! j) / c)) (i * n + j)
  g0F <- U.freeze g0
  trF <- U.freeze trN
  emF <- U.freeze emN
  ll <- MU.read llRef 0
  let g0l = U.toList g0F
      rowsOf v w = [ U.toList (U.slice (i * w) w v) | i <- [0 .. n - 1] ]
      p0' = if sumOf g0l > 0 then normRow g0l else hP0 m
      tr' = [ if sumOf r > 1e-12 then normRow r else old | (r, old) <- zip (rowsOf trF n) (hTr m) ]
      em' = [ if sumOf r > 1e-12 then normRow r else old | (r, old) <- zip (rowsOf emF k) (hEm m) ]
  pure (Hmm p0' tr' em', ll)
  where
    n = nStates m
    k = length (headOr [] (hEm m))
    p0A = U.fromList (hP0 m)
    trA = U.fromList (concat (hTr m))
    emA = U.fromList (concat (hEm m))

-- ★ 判定：4 つのモデルの対数尤度をくらべる。どのモデルでも出せない列なら −1
classify :: [Hmm] -> Maybe [Int] -> Verdict
classify models ms = case ms of
  Just syms | not (null syms) ->
    let scores = map (`hmmLogLik` syms) models
        b = argMax scores
    in Verdict (if at' scores b negInf > negInf then b else -1) scores
  _ -> Verdict (-1) (map (const negInf) models)

judge :: World -> World
judge w = let s = strokeToSyms (wStroke w) in w { wSyms = s, wVerdict = classify (wModels w) s }

-- お手本を 1 本、描く場所に合わせて作る
demoStroke :: Env -> World -> World
demoStroke env w =
  let ((c, pts), g) = runWith (wRng w) (do c' <- rndInt 0 3; p <- synthStroke c' wobble; pure (c', p))
      sz = realToFrac (min 260 (envH env * 0.32)) :: Double
      cx = realToFrac (60 + drawW env / 2)
      cy = realToFrac (120 + drawH env / 2)
  in c `seq` judge w { wStroke = Just [ (cx + x * sz, cy + y * sz) | (x, y) <- pts ], wDrawing = [], wRng = g }

drawW, drawH :: Env -> Float
drawW env = max 360 (envW env * 0.48)
drawH env = max 300 (envH env - 330)

softmaxOf :: [Double] -> [Double]
softmaxOf sc = let m = foldl' maxN negInf sc in if m == negInf then map (const 0) sc else normRow (map (\v -> exp (v - m)) sc)

draw :: Env -> World -> Picture
draw env w = view env
  [ hudTitle env "Gesture recognition with left-to-right HMMs"
      ("Drag to draw   Space auto-draw an example   1-4 choose gesture to teach (now: " ++ at' gNames (wTeach w) "" ++ ") -> T add this stroke as an example   C forget added examples")
  , panelBox ax ay aw ah
  , note "Draw here. Start matters: circle from the top, clockwise;" (ax + 12) (ay + 22) dimc 14
  , note "triangle from the top to the lower right; Z from the top left; L from the top down" (ax + 12) (ay + 42) dimc 14
  -- 描いた線：色 = いちばん尤度が高いモデルのビタビの状態（左から右へ進む）
  , case rs of
      Just ps -> Pictures [ Pictures [ Color (col (maybe dimc (\v -> stCol (at' v (i - 1) 0)) vp)) (lineW 5 (p2f a) (p2f b)) | (i, a, b) <- zip3 [1 :: Int ..] ps (drop 1 ps) ]
                          , let (sx0, sy0) = p2f (headOr (0, 0) ps) in Color (col ink) (circleAt sx0 sy0 12) ]
      Nothing -> Blank
  , if length (wDrawing w) > 1 then Color (col ink) (polyline 3 (wDrawing w)) else Blank
  -- 記号の列
  , note "The 8-direction symbols (24 of them) and their states" ax sy ink 14
  , case wSyms w of
      Just syms -> Pictures [ Pictures [ Color (col (maybe linec (\v -> stCol (at' v i 0)) vp)) (rectTL x (sy + 10) (scw - 3) 26)
                                       , dirArrow (x + scw / 2 - 1.5) (sy + 23) s ]
                            | (i, s) <- zip [0 :: Int ..] syms, let x = ax + i2f i * scw ]
      Nothing -> note "(the stroke is too short to turn into symbols)" ax (sy + 30) rose 14
  , Pictures [ Pictures [ Color (col (stCol s)) (rectTL (ax + i2f s * 40) (sy + 52) 14 14), note (show (s + 1)) (ax + i2f s * 40 + 18) (sy + 64) dimc 14 ] | s <- [0 .. nSt - 1] ]
  , note "= states 1-6 (left to right)" (ax + i2f nSt * 40 + 4) (sy + 64) dimc 14
  -- ---- 右：判定
  , note (if best >= 0 then "Verdict: " ++ at' gNames best "" else "Cannot decide") rx 150 (if best >= 0 then lan else rose) 30
  , Pictures [ Pictures [ note (at' gNames c "") rx (y + 18) (if c == wTeach w then lan else ink) 16
                        , Color (col linec) (rectTL (rx + 96) (y + 4) 194 18)
                        , Color (col (if c == best then lan else dimc)) (rectTL (rx + 96) (y + 4) (194 * f2 (at' post c 0)) 18)
                        , note ("logP " ++ (if null (vScores v) then "-" else fmtN (at' (vScores v) c 0) 1) ++ "  n=" ++ show (length (at' (wSets w) c []))
                                ++ (if at' (wAdded w) c 0 > 0 then " (+" ++ show (at' (wAdded w) c 0) ++ ")" else "")) (rx + 300) (y + 18) dimc 14 ]
             | c <- [0 .. 3], let y = 190 + i2f c * 44 ]
  , note "Bar = the 4 likelihoods compared (log P one larger = about 2.7x)" rx 384 dimc 14
  , note (at' gNames shown "" ++ " HMM: B (rows = states 1-6, cols = direction)") rx 420 ink 14
  , drawMatrix (hEm m) (rx + 40) 448 cw 24 (map show [1 .. 6 :: Int]) dirNames matOpt { moCol = lan, moNumbers = False }
  , note "A (lower left all 0 = no going back)" rx (448 + 6 * 24 + 30) ink 14
  , drawMatrix (hTr m) (rx + 40) (448 + 6 * 24 + 44) cw 22 (map show [1 .. 6 :: Int]) [] matOpt { moCol = rose }
  , drawSelfTest env (wTest w) ]
  where
    ax = 60
    ay = 120
    aw = drawW env
    ah = drawH env
    v = wVerdict w
    best = vBest v
    vp = if best >= 0 then Just (vtPath (viterbiLog bm (emTable bm (maybe [] id (wSyms w))))) else Nothing
    bm = at' (wModels w) best (Hmm [] [] [])
    rs = wStroke w >>= \p -> resample p nSeg
    sy = ay + ah + 26
    scw = min 30 (aw / 24)
    rx = ax + aw + 40
    post = if null (vScores v) then [0, 0, 0, 0] else softmaxOf (vScores v)
    shown = if best >= 0 then best else wTeach w
    m = at' (wModels w) shown (Hmm [] [] [])
    cw = min 40 ((envW env - rx - 80) / 8)
    p2f (x, y) = (realToFrac x, realToFrac y) :: (Float, Float)

-- 方向の記号を小さな矢印で（p5.js 版の → ↘ ↓ … の文字の代わり）
dirArrow :: Float -> Float -> Int -> Picture
dirArrow x y s = let a = i2f s * pi / 4
                     dx = 8 * cos a
                     dy = 8 * sin a
                 in arrowLine 2 (col bgc) (x - dx, y - dy) (x + dx, y + dy) 7

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        inside = mx >= 60 && mx <= 60 + drawW env && my >= 120 && my <= 120 + drawH env
    in w { wDown = inside, wDrawing = if inside then [(mx, my)] else [] }
  EventMotion pos | envDown env && wDown w && length (wDrawing w) < 2000 -> w { wDrawing = wDrawing w ++ [toP5 env pos] }
  EventKey (MouseButton LeftButton) Up _ _ ->
    let w' = if wDown w && length (wDrawing w) >= 2 then judge w { wStroke = Just [ (realToFrac x, realToFrac y) | (x, y) <- wDrawing w ] } else w
    in w' { wDrawing = [], wDown = False }
  EventKey (SpecialKey KeySpace) Down _ _ -> demoStroke env w
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> w { wTeach = fromEnum c - fromEnum '1' }
    | c == 't' || c == 'T', Just syms <- wSyms w ->
        let k = wTeach w
            set0 = at' (wSets w) k [] ++ [syms]
            set1 = if length set0 > 60 then take nTrain set0 ++ drop (nTrain + 1) set0 else set0
            ((m, _), g) = runWith (wRng w) (trainClass set1 15 epsEm)
        in judge w { wSets = setAt k set1 (wSets w), wAdded = modifyAt k (+ 1) (wAdded w), wModels = setAt k m (wModels w), wRng = g }
    | c == 'c' || c == 'C' -> judge (resetTraining w { wRng = rngOf 79 })
  _ -> w

-- ---------- やってみよう ----------
-- 1. 丸・三角・Z・L を自分で描いて、正しく判定される？ 丸を反時計回りに描くとどうなる？ なぜ？
-- 2. 線の色（状態）は、描きはじめから終わりまで順番に変わっていく。三角の「かど」と色の変わり目は合っている？
-- 3. 自分だけのジェスチャ（たとえば「V」）を 5 回描いて、4 キー → T で「L」のかわりに教えよう
-- 4. nSt を 2 にすると（状態が少ない）、どのジェスチャが見分けられなくなる？

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest = withSeed 790 $ do
  let rs = maybe [] id (resample [(0, 0), (10, 0), (10, 10)] 8)
      okLen = length rs == 9 && and [ near (jsHypot (x2 - x1) (y2 - y1)) 2.5 1e-9 | ((x1, y1), (x2, y2)) <- zip rs (drop 1 rs) ]
      okDirs = dirSymbols [(0, 0), (5, 0), (5, 5), (0, 5), (0, 0), (3, 3)] == [0, 2, 4, 6, 1]
  bk <- bakisStart 6 8
  let okNull = strokeToSyms (Just [(3, 3)]) == Nothing && strokeToSyms (Just [(3, 3), (3, 3), (3, 3)]) == Nothing && strokeToSyms Nothing == Nothing
               && vBest (classify [bk] Nothing) == -1
  sets <- mapM (\c -> replicateR nTrain (fmap strokeToSymsJ (synthStroke c wobble))) [0 .. 3]
  trained <- mapM (\s -> trainClass s 15 epsEm) sets
  let models = map fst trained
      okTri = all (\m -> and [ (j >= i && j <= i + 2) || v == 0 | (i, row) <- zip [0 :: Int ..] (hTr m), (j, v) <- zip [0 ..] row ]
                         && rowSumErr (hTr m) < 1e-9 && rowSumErr (hEm m) < 1e-9 && at' (hP0 m) 0 0 == 1) models
  (_, pureL) <- trainClass (at' sets 1 []) 15 0
  let okMono = and [ b >= a - 1e-9 | (a, b) <- zip pureL (drop 1 pureL) ]
  tests <- mapM (\c -> replicateR 15 (do
              st <- synthStroke c wobble
              let syms = strokeToSyms (Just st)
                  v = classify models syms
                  mc = at' models c (Hmm [] [] [])
                  vp = vtPath (viterbiLog mc (emTable mc (maybe [] id syms)))
              pure (vBest v == c, and (zipWith (<=) vp (drop 1 vp))))) [0 .. 3]
  let flat = concat tests
      right = length (filter fst flat)
      total = length flat
      okLR = all snd flat
  z <- synthStroke 2 0.02
  let mz = at' models 2 (Hmm [] [] [])
      fwdLL = hmmLogLik mz (maybe [] id (strokeToSyms (Just z)))
      revLL = hmmLogLik mz (maybe [] id (strokeToSyms (Just (reverse z))))
  junk <- replicateR 40 (do a <- rnd; b <- rnd; pure (a * 100, b * 100))
  let vj = classify models (strokeToSyms (Just junk))
  pure
    [ ("線を分け直す：9 個の点が等しい間かく（道のり 20 を 8 等分）", okLen && near (fst (at' rs 4 (0, 0))) 10 1e-9 && near (snd (at' rs 8 (0, 0))) 10 1e-9)
    , ("方向の記号：右 0・下 2・左 4・上 6・右下 1", okDirs)
    , ("短すぎる線（1 点・同じ点だけ）は記号にしない（null）、判定は −1 で止まらない", okNull)
    , ("学習のあとも遷移行列は上三角（とどまる・次へ・1 つ飛ばす だけ）、行の合計 = 1、いつも状態 1 から始まる", okTri)
    , ("バウム-ウェルチ（小さな数を足さない場合）：対数尤度は下がらない", okMono)
    , ("新しく作った 60 本のジェスチャ（各 15 本）を全部正しく判定：" ++ show right ++ " / " ++ show total, total == 60 && right == total)
    , ("ビタビの状態は左から右へしか進まない（60 本全部）", okLR)
    , ("Z を逆向きに描くと、Z のモデルの尤度は大きく下がる（順番も見ている）：" ++ fmtN fwdLL 1 ++ " → " ++ fmtN revLL 1, revLL < fwdLL - 10)
    , ("でたらめな線でも NaN にならず、どれかに判定される（尤度はどれも低い）", noNaN (vScores vj) && vBest vj >= 0 && foldl' maxN negInf (vScores vj) < fwdLL - 10) ]

-- ==================================================
-- ここから下は「HMM の道具」（中身は読まなくても OK。消さないでね）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 計算は Double（倍精度）、描くときだけ Float にする。分布 = リスト、行列 = 行のリスト
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（霧の夜と灯り：黄 = 見える観測、ばら色 = 隠れた状態）
type RGB = (Float, Float, Float)
type Vec = [Double]                           -- 分布・ベクトル
type Mat = [[Double]]                         -- 行列（行のリスト）

bgc, ink, lan, rose, aqua, lilac, leaf, tang, dimc, panelc, linec :: RGB
bgc = (27, 33, 44)                            -- 背景（霧の夜の青ねずみ色）
ink = (230, 234, 240)                         -- 文字（やわらかい白）
lan = (250, 208, 100)
rose = (236, 124, 150)
aqua = (110, 200, 210)
lilac = (176, 156, 236)
leaf = (150, 210, 130)
tang = (240, 160, 96)
dimc = (142, 154, 172)
panelc = (38, 46, 60)
linec = (76, 88, 108)

stCols :: [RGB]                               -- 状態の色
stCols = [rose, aqua, lan, lilac, leaf, tang, (200, 200, 210), (130, 150, 240)]

stCol :: Int -> RGB
stCol i = at' stCols (i `mod` 8) ink

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

clampF' :: Float -> Float -> Float -> Float
clampF' v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumOf :: [Double] -> Double                   -- 左から順に足す（JavaScript の for と同じ順番）
sumOf = foldl' (+) 0

isFin :: Double -> Bool
isFin v = not (isNaN v || isInfinite v)

negInf, posInf :: Double
negInf = -1 / 0
posInf = 1 / 0

f2 :: Double -> Float                         -- 描くときの Float（NaN は 0）
f2 v = if isFin v then realToFrac v else 0

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

-- JavaScript の Math.round（.5 は大きいほうへ）と Math.floor
jsRound :: Double -> Int
jsRound v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e15) 1e15)

-- Math.max と同じく NaN が混じったら NaN
maxN :: Double -> Double -> Double
maxN a b = if isNaN a || isNaN b then 0 / 0 else max a b

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

-- 行列の (i, j)（なければ 0）
at2 :: [[Double]] -> Int -> Int -> Double
at2 a i j = at' (at' a i []) j 0

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

-- リストの i 番目を f で変える
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

setAt :: Int -> a -> [a] -> [a]
setAt i v = modifyAt i (const v)

forceL :: [Double] -> [Double]
forceL xs = foldl' (flip seq) () xs `seq` xs

forceM :: [[Double]] -> [[Double]]
forceM xs = foldl' (\u r -> forceL r `seq` u) () xs `seq` xs

-- 最初にいちばん大きい値の番号（同点なら小さい番号）
argMax :: [Double] -> Int
argMax xs = fst (foldl' (\(bi, bv) (i, v) -> if v > bv then (i, v) else (bi, bv)) (0, headOr 0 xs) (zip [0 ..] xs))

-- 数を書く（p5.js 版の fmtN：toFixed(d)、"-0.00" は "0.00" に。∞ は画面では inf）
fmtN :: Double -> Int -> String
fmtN v d
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "∞" else "−∞"
  | otherwise = (if v < 0 && n > 0 then "-" else "") ++ fixedDigits n d
  where n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer

fixedDigits :: Integer -> Int -> String
fixedDigits n d =
  let ds = show n
      padded = replicate (d + 1 - length ds) '0' ++ ds
      (ip, fp) = splitAt (length padded - d) padded
  in if d > 0 then ip ++ "." ++ fp else ip

-- v.toFixed(d) そのまま（−0.0000 も残す）
toFixed :: Double -> Int -> String
toFixed v d
  | not (isFin v) = jsNum v
  | otherwise = (if v < 0 then "-" else "") ++ fixedDigits (floor (toRational (abs v) * 10 ^ d + 1 / 2)) d

-- 確率の表示：小さい数は 1.2e-5 の形で
fmtP :: Double -> String
fmtP v
  | isNaN v = "?"
  | v == 0 = "0"
  | abs v >= 0.001 = toFixed v (if v >= 0.1 then 3 else 4)
  | otherwise = toExp1 v

-- v.toExponential(1)（例 6.7e-9）
toExp1 :: Double -> String
toExp1 v
  | not (isFin v) = jsNum v
  | v == 0 = "0.0e+0"
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

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

rndOut :: Word32 -> Double
rndOut a =
  let t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296

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
seedRng s = R (\_ -> ((), rngOf s))

rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

-- 乱数の列 g で計算して、(結果, 進んだ列)
runWith :: Rng -> R a -> (a, Rng)
runWith g m = runR m g

rnd :: R Double                               -- 0〜1 の一様乱数
rnd = R (\(Rng s) -> let a = s + 0x6d2b79f5 in (rndOut a, Rng a))

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * i2d (b - a + 1)))

-- 標準正規分布（ボックス-ミュラー、u = 1 − rnd() なので log(0) にならない）
randn :: R Double
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- 確率 p !! i で i を返す（rnd() を 1 回だけ引く）
sampleFrom :: [Double] -> R Int
sampleFrom p = do
  u0 <- rnd
  let u = u0 * sumOf p
      go _ _ [] = length p - 1
      go i c (v : vs) = let c' = c + v in if u < c' then i else go (i + 1) c' vs
  pure (go 0 0 p)

-- n 回くり返して結果を順に集める（1 回ずつ状態を進める）
replicateR :: Int -> R a -> R [a]
replicateR n m = go n []
  where go k acc = if k <= 0 then pure (reverse acc) else do { !x <- m; go (k - 1) (x : acc) }

-- 乱数を使う左たたみ込み（JavaScript の for で状態を書きかえていく計算）
foldlR :: (b -> a -> R b) -> b -> [a] -> R b
foldlR f = go
  where go !acc [] = pure acc
        go !acc (x : xs) = f acc x >>= \a -> go a xs

-- ---- 隠れマルコフモデルの道具
-- モデル = Hmm { hP0 = はじめの分布 π, hTr = 遷移行列 A（hTr !! i !! j = 状態 i の次に j）, hEm = 出力行列 B（hEm !! j !! k = 状態 j が記号 k を出す） }
-- どの計算も「出力の表」eb !! t !! j = b_j(x_t)（時刻 t の観測を状態 j が出す確率）を受け取る → 記号でも数（正規分布）でも同じ道具が使える
data Hmm = Hmm { hP0 :: Vec, hTr :: Mat, hEm :: Mat }

nStates :: Hmm -> Int
nStates = length . hP0

lg :: Double -> Double                        -- log 0 は −∞（NaN にしない）
lg p = if p > 0 then log p else negInf

-- log(Σ e^a_i)：いちばん大きい値を先に引く。全部 −∞ なら −∞
logSumExp :: [Double] -> Double
logSumExp a =
  let m = foldl' (\b v -> if v > b then v else b) negInf a
  in if m == negInf then negInf else m + log (sumOf [ exp (v - m) | v <- a ])

-- 合計 1 に（全部 0 なら一様に）
normRow :: Vec -> Vec
normRow r = let s = sumOf r in if s > 1e-300 then map (/ s) r else map (const (1 / i2d (length r))) r

rowSumErr :: Mat -> Double
rowSumErr a = foldl' (\d r -> maxN d (abs (sumOf r - 1))) 0 a

-- 2 つの行列（またはベクトル）のいちばん大きなずれ（NaN が混じれば NaN）
maxAbsDiff :: Mat -> Mat -> Double
maxAbsDiff a b = foldl' maxN 0 (zipWith maxAbsDiffV a b)

maxAbsDiffV :: Vec -> Vec -> Double
maxAbsDiffV a b = foldl' (\d (x, y) -> maxN d (abs (x - y))) 0 (zip a b)

-- 横ベクトル × 行列：1 歩先の分布
vecMat :: Vec -> Mat -> Vec
vecMat v a = foldl' (\acc (x, row) -> if x == 0 then acc else forceL (zipWith (\c y -> c + x * y) acc row)) (replicate (length (headOr [] a)) 0) (zip v a)

emTable :: Hmm -> [Int] -> Mat                -- 記号の列 → 出力の表
emTable m xs = [ [ at' r x 0 | r <- hEm m ] | x <- xs ]

-- π, A, B からサイコロで列を作る：(隠れた状態, 観測)
hmmSample :: Hmm -> Int -> R ([Int], [Int])
hmmSample m n = do
  z0 <- sampleFrom (hP0 m)
  let go t z zs xs
        | t >= n = pure (reverse zs, reverse xs)
        | otherwise = do
            z' <- if t > 0 then sampleFrom (at' (hTr m) z []) else pure z
            x <- sampleFrom (at' (hEm m) z' [])
            go (t + 1) z' (z' : zs) (x : xs)
  go (0 :: Int) z0 [] []

-- ★ 前向き：α_t(j) = P(x_1..x_t, z_t = j) = b_j(x_t) Σ_i α_{t−1}(i) a_ij。尤度 P(x) = Σ_j α_T(j)
fwdPlain :: Hmm -> Mat -> (Mat, Double)
fwdPlain m eb = (al, if null al then 1 else sumOf (last al))
  where
    n = nStates m
    al = reverse (fst (foldl' step ([], True) eb))
    step (acc, first) e =
      let prev = headOr [] acc
          row = forceL [ s * at' e j 0 | j <- [0 .. n - 1]
                       , let s = if first then at' (hP0 m) j 0 else sumOf [ at' prev i 0 * at2 (hTr m) i j | i <- [0 .. n - 1] ] ]
      in (row : acc, False)

-- ★ 後ろ向き：β_t(i) = P(x_{t+1}..x_T | z_t = i) = Σ_j a_ij b_j(x_{t+1}) β_{t+1}(j)。尤度 = Σ_i π_i b_i(x_1) β_1(i)
bwdPlain :: Hmm -> Mat -> (Mat, Double)
bwdPlain m eb = (be, lik)
  where
    n = nStates m
    be = if null eb then [] else foldr step [replicate n 1] (drop 1 eb)
    step e acc = let nb = headOr [] acc
                 in forceL [ sumOf [ at2 (hTr m) i j * at' e j 0 * at' nb j 0 | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ] : acc
    lik = if null eb then 1 else sumOf [ at' (hP0 m) i 0 * at' (headOr [] eb) i 0 * at' (headOr [] be) i 0 | i <- [0 .. n - 1] ]

-- ★ スケーリングつきの前向き：毎時刻 合計 c_t で割る。fsAh !! t = P(z_t | x_1..x_t)（= フィルタ）、log P(x) = Σ log c_t
--   c_t = 0（ありえない観測）なら logLik = −∞ にして、その時刻は一様分布で先へ進む（NaN を作らない）
data FS = FS { fsAh :: [Vec], fsCs :: [Double], fsLogLik :: !Double }

fwdScaled :: Hmm -> Mat -> FS
fwdScaled m eb = let (ahR, csR, ll) = foldl' step ([], [], 0) eb in FS (reverse ahR) (reverse csR) ll
  where
    n = nStates m
    step (ah, cs, !ll) e =
      let pred' = if null ah then hP0 m else vecMat (headOr [] ah) (hTr m)
          row = zipWith (*) pred' e
          c = sumOf row
      in if not (c > 0) then (replicate n (1 / i2d n) : ah, 0 : cs, negInf)
         else (forceL (map (/ c) row) : ah, c : cs, if ll /= negInf then ll + log c else ll)

-- 同じ c_t で割った後ろ向き。γ_t(i) = ah !! t !! i × bh !! t !! i
bwdScaled :: Hmm -> Mat -> [Double] -> [Vec]
bwdScaled m eb cs = if null eb then [] else foldr step [replicate n 1] (zip (drop 1 eb) (drop 1 cs))
  where
    n = nStates m
    step (e, c0) acc =
      let nb = headOr [] acc
          c = if c0 > 0 then c0 else 1
      in forceL [ sumOf [ at2 (hTr m) i j * at' e j 0 * at' nb j 0 | j <- [0 .. n - 1] ] / c | i <- [0 .. n - 1] ] : acc

-- 対数のまま：log α_t(j) = log b_j(x_t) + logSumExp_i(log α_{t−1}(i) + log a_ij)
fwdLog :: Hmm -> Mat -> (Mat, Double)
fwdLog m eb = (la, if null la then 0 else logSumExp (last la))
  where
    n = nStates m
    lt = map (map lg) (hTr m)
    la = reverse (fst (foldl' step ([], True) eb))
    step (acc, first) e =
      let prev = headOr [] acc
          row = forceL [ s + lg (at' e j 0) | j <- [0 .. n - 1]
                       , let s = if first then lg (at' (hP0 m) j 0) else logSumExp [ at' prev i 0 + at2 lt i j | i <- [0 .. n - 1] ] ]
      in (row : acc, False)

-- ★ 平滑化：γ_t(i) = P(z_t = i | 全部の観測)。smFilt = フィルタ P(z_t | x_1..x_t)
data SM = SM { smFilt :: [Vec], smGam :: [Vec], smBh :: [Vec], smCs :: [Double], smLogLik :: !Double }

smoothAll :: Hmm -> Mat -> SM
smoothAll m eb =
  let f = fwdScaled m eb
      bh = bwdScaled m eb (fsCs f)
      gam = zipWith (\a b -> forceL (normRow (zipWith (*) a b))) (fsAh f) bh
  in SM (fsAh f) gam bh (fsCs f) (fsLogLik f)

-- ξ_t(i, j) = P(z_t = i, z_{t+1} = j | 全部の観測)
xiAt :: Hmm -> Mat -> SM -> Int -> Mat
xiAt m eb sm t =
  let n = nStates m
      c0 = at' (smCs sm) (t + 1) 0
      c = if c0 > 0 then c0 else 1
      ft = at' (smFilt sm) t []
      e1 = at' eb (t + 1) []
      b1 = at' (smBh sm) (t + 1) []
  in [ [ at' ft i 0 * at2 (hTr m) i j * at' e1 j 0 * at' b1 j 0 / c | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]

-- ★ ビタビ（対数）：vtDl !! t !! j = 時刻 t に状態 j で終わるいちばん良い道の log 確率、vtBp !! t !! j = そのとき 1 つ前にいた状態（戻り矢印、なければ −1）
data VT = VT { vtPath :: [Int], vtLogP :: !Double, vtDl :: [Vec], vtBp :: [[Int]] }

viterbiLog :: Hmm -> Mat -> VT
viterbiLog m eb = VT path logP dl bp
  where
    n = nStates m
    lt = map (map lg) (hTr m)
    (dlR, bpR) = foldl' step ([], []) eb
    dl = reverse dlR
    bp = reverse bpR
    step (ds, bs) e =
      let prev = headOr [] ds
          cells = [ if null ds then (lg (at' (hP0 m) j 0), -1)
                    else foldl' (\(b, a) i -> let v = at' prev i 0 + at2 lt i j in if v > b then (v, i) else (b, a)) (negInf, -1) [0 .. n - 1]
                  | j <- [0 .. n - 1] ]
          row = forceL [ b + lg (at' e j 0) | (j, (b, _)) <- zip [0 ..] cells ]
      in (row : ds, map snd cells : bs)
    lastRow = lastOr [] dl
    zEnd = argMax lastRow
    logP = if null eb then 0 else at' lastRow zEnd negInf
    path = if null eb then [] else reverse (goBack zEnd bpR)
    goBack _ [] = []
    goBack z (b : rest) = let p = at' b z (-1) in z : goBack (if p >= 0 then p else 0) rest

-- 1 本の道 zs と観測がいっしょに起きる log 確率
pathLogProb :: Hmm -> Mat -> [Int] -> Double
pathLogProb m eb zs = foldl' (\s (t, z, zp, e) -> s + ((if t == 0 then lg (at' (hP0 m) z 0) else lg (at2 (hTr m) zp z)) + lg (at' e z 0))) 0
                        (zip4' [0 :: Int ..] zs (0 : zs) eb)

zip4' :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
zip4' _ _ _ _ = []

postDecode :: [Vec] -> [Int]                  -- 事後復号：時刻ごとにいちばんありそうな状態
postDecode = map argMax

-- ★ 総当たり：N^T 本の道を全部ためす（短い列だけ）。尤度、γ、ξ、いちばん良い道
data BF = BF { bfLik :: !Double, bfGam :: [Vec], bfXi :: [Mat], bfBest :: [Int], bfBestP :: !Double, bfPaths :: !Int }

bruteAll :: Hmm -> Mat -> BF
bruteAll m eb = BF lik gam xi best bestP total
  where
    n = nStates m
    len = length eb
    total = n ^ len
    ebA = listArray ((0, 0), (max 0 (len - 1), max 0 (n - 1))) (concatMap (\r -> take n (r ++ repeat 0)) eb ++ repeat 0) :: UArray (Int, Int) Double
    p0A = listArray (0, max 0 (n - 1)) (hP0 m ++ repeat 0) :: UArray Int Double
    trA = listArray ((0, 0), (max 0 (n - 1), max 0 (n - 1))) (concatMap (\r -> take n (r ++ repeat 0)) (hTr m) ++ repeat 0) :: UArray (Int, Int) Double
    pathOf k = U.generate len (\t -> (k `div` (n ^ (len - 1 - t))) `mod` n)
    probOf zs = let go t p = if t >= len || not (p > 0) then p
                             else let z = zs U.! t in go (t + 1) (p * ((if t == 0 then p0A ! z else trA ! (zs U.! (t - 1), z)) * ebA ! (t, z)))
                in go 0 1
    paths = [ (zs, probOf zs) | k <- [0 .. total - 1], let zs = pathOf k ]
    (lik0, bestP, bestV) = foldl' (\(l, bp, bz) (zs, p) -> let (bp', bz') = if p > bp then (p, zs) else (bp, bz) in (if p == 0 then l else l + p, bp', bz')) (0, -1, U.empty) paths
    best = U.toList bestV
    gamA = accumArray (+) 0 ((0, 0), (max 0 (len - 1), max 0 (n - 1))) [ ((t, zs U.! t), p) | (zs, p) <- paths, p /= 0, t <- [0 .. len - 1] ] :: UArray (Int, Int) Double
    xiA = accumArray (+) 0 ((0, 0, 0), (max 0 (len - 2), max 0 (n - 1), max 0 (n - 1))) [ ((t - 1, zs U.! (t - 1), zs U.! t), p) | (zs, p) <- paths, p /= 0, t <- [1 .. len - 1] ] :: UArray (Int, Int, Int) Double
    lik = lik0
    dv v = if lik > 0 then v / lik else v
    gam = [ [ dv (gamA ! (t, j)) | j <- [0 .. n - 1] ] | t <- [0 .. len - 1] ]
    xi = [ [ [ dv (xiA ! (t, i, j)) | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ] | t <- [0 .. len - 2] ]

-- 定常分布 π A = π（なまけた連鎖 (π + πA)/2 をくり返す。周期があっても収束する）
stationaryOf :: Mat -> Vec
stationaryOf tr = normRow (go (0 :: Int) (replicate n (1 / i2d n)))
  where
    n = length tr
    go k v
      | k >= 4000 = v
      | otherwise =
          let w = vecMat v tr
              u = forceL (zipWith (\a b -> (a + b) / 2) v w)
              d = foldl' maxN 0 (zipWith (\a b -> abs (a - b)) u v)
          in if d < 1e-15 then u else go (k + 1) u

hmmLogLik :: Hmm -> [Int] -> Double           -- 記号の列の対数尤度 log P(x)
hmmLogLik m xs = fsLogLik (fwdScaled m (emTable m xs))

predictK :: Vec -> Mat -> Int -> Vec          -- k 歩先の予測：A を k 回掛ける
predictK belief tr k = iterate (`vecMat` tr) belief !! max 0 k

-- ---- バウム-ウェルチ（EM）の道具
-- でたらめな行（(0.05 + rnd())^sharp を合計 1 に。sharp = 0 は 1 と同じ）
randomRow :: Int -> Double -> R Vec
randomRow k sharp = do
  r <- replicateR k (fmap (\u -> (0.05 + u) ** (if sharp == 0 then 1 else sharp)) rnd)
  pure (normRow r)

-- でたらめなモデル：π のあと、状態ごとに A の行、B の行の順に引く
randomHmm :: Int -> Int -> R Hmm
randomHmm n k = do
  p0 <- randomRow n 1
  rows <- replicateR n (do a <- randomRow n 1; b <- randomRow k 1; pure (a, b))
  pure (Hmm p0 (map fst rows) (map snd rows))

-- ★ 1 回の EM：E ステップで γ（状態 i にいた期待回数）と ξ（i → j と動いた期待回数）を足し合わせ、M ステップで割り算する
--   seqs = 観測列のリスト。eps = 出力の数に足す小さな数（0 でもよい）。戻り値の対数尤度は「更新する前」のモデルのもの
baumWelchStep :: Hmm -> [[Int]] -> Double -> (Hmm, Double)
baumWelchStep m seqs eps = (Hmm p0' tr' em', logLik)
  where
    n = nStates m
    k = length (headOr [] (hEm m))
    start = (replicate n 0, replicate n (replicate n 0), replicate n (replicate k eps), 0)
    (g0, trNum, emNum, logLik) = foldl' oneSeq start seqs
    oneSeq acc@(g, tn, en, !ll) xs
      | null xs = acc
      | otherwise =
          let eb = emTable m xs
              sm = smoothAll m eb
              ll' = ll + smLogLik sm
          in if smLogLik sm == negInf then (g, tn, en, ll') else
               let g' = forceL (zipWith (+) g (headOr [] (smGam sm)))
                   en' = foldl' (\e (x, gm) -> forceM [ [ if s == x then v + at' gm i 0 else v | (s, v) <- zip [0 ..] row ] | (i, row) <- zip [0 ..] e ]) en (zip xs (smGam sm))
                   tn' = foldl' (\tacc (ft, e1, b1, c) ->
                                   forceM [ let a = at' ft i 0 in if a == 0 then row else
                                              [ v + (a * at2 (hTr m) i j * at' e1 j 0 * at' b1 j 0) / c | (j, v) <- zip [0 ..] row ]
                                          | (i, row) <- zip [0 ..] tacc ])
                                tn (zip4' (smFilt sm) (drop 1 eb) (drop 1 (smBh sm)) (drop 1 (smCs sm)))
               in (g', tn', en', ll')
    p0' = if sumOf g0 > 0 then normRow g0 else hP0 m
    tr' = [ if sumOf r > 1e-12 then normRow r else old | (r, old) <- zip trNum (hTr m) ]
    em' = [ if sumOf r > 1e-12 then normRow r else old | (r, old) <- zip emNum (hEm m) ]

permsOf :: Int -> [[Int]]                     -- 0..n−1 の並べかえを全部（辞書順）
permsOf n = go [0 .. n - 1]
  where go [] = [[]]
        go xs = [ x : rest | x <- xs, rest <- go (filter (/= x) xs) ]

-- ★ ラベルの入れかえ：cost(本物 i, 学習 p !! i) の合計が最小になる並べかえ p
bestPerm :: Int -> (Int -> Int -> Double) -> [Int]
bestPerm n cost = snd (foldl' (\(bc, bp) p -> let c = sumOf [ cost i (at' p i 0) | i <- [0 .. n - 1] ] in if c < bc then (c, p) else (bc, bp)) (posInf, [0 .. n - 1]) (permsOf n))

permuteHmm :: Hmm -> [Int] -> Hmm
permuteHmm m p = Hmm [ at' (hP0 m) i 0 | i <- p ] [ [ at2 (hTr m) i j | j <- p ] | i <- p ] [ at' (hEm m) i [] | i <- p ]

rowDist :: Vec -> Vec -> Double
rowDist a b = sumOf (zipWith (\x y -> abs (x - y)) a b)

-- ---- グラフの道具：G { 左上 x y, 幅 w, 高さ h, x の範囲 x0〜x1, y の範囲 y0〜y1 }
data G = G { gx, gy, gw, gh :: !Float, gx0, gx1, gy0, gy1 :: !Double }

gX :: G -> Double -> Float
gX g v = gx g + f2 (clampv ((v - gx0 g) / (gx1 g - gx0 g)) (-0.02) 1.02) * gw g

gY :: G -> Double -> Float
gY g v = gy g + gh g - f2 (clampv ((v - gy0 g) / (gy1 g - gy0 g)) (-0.02) 1.02) * gh g

-- 枠と目盛り（目盛りの数は有効数字 4 桁）、x 軸と y 軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures
  [ Color (col linec) (frameTL 1 (gx g) (gy g) (gw g) (gh g))
  , Pictures [ sayT (precN 4 v) (gX g v) (gy g + gh g + 4) 14 (col dimc) True | v <- xt ]
  , Pictures [ sayRM (precN 4 v) (gx g - 5) (gY g v) 14 (col dimc) | v <- yt ]
  , sayR xl (gx g + gw g) (gy g + gh g + 34) 14 (col ink)
  , say yl (gx g) (gy g - 8) 14 (col ink) ]

-- arr !! i を x = i (+ xOff) に。長いときは間引いて 600 点まで（from〜to−1 だけ）
drawCurve :: G -> [Double] -> RGB -> Float -> Double -> Picture
drawCurve g arr c sw xOff = drawCurveR g arr c sw xOff 0 (length arr)

drawCurveR :: G -> [Double] -> RGB -> Float -> Double -> Int -> Int -> Picture
drawCurveR g arr c sw xOff a b =
  let n = b - a
      stride = max 1 ((n + 599) `div` 600)
      pts = [ (gX g (i2d i + xOff), gY g v) | (i, v) <- zip [a ..] (take n (drop a arr)), (i - a) `mod` stride == 0, isFin v ]
  in Color (col c) (polyline sw pts)

-- 凡例：items = [(色, 名前)]
legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures [ Pictures [ Color (col c) (rectTL x (yy - 5) 16 10), sayLM s (x + 22) yy 14 (col ink) ] | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * i2f k ]

-- 行列を色つきの表で（明るい = 大きい）
data MatOpt = MatOpt { moDigits :: Int, moCol :: RGB, moTs :: Float, moHi :: Double, moNumbers :: Bool }

matOpt :: MatOpt
matOpt = MatOpt 2 aqua 14 1 True

drawMatrix :: Mat -> Float -> Float -> Float -> Float -> [String] -> [String] -> MatOpt -> Picture
drawMatrix a x y cw ch rowNames colNames o = Pictures
  [ Pictures [ Pictures [ Color (col (mixC t)) (rectTL (x + i2f j * cw) (y + i2f i * ch) (cw - 2) (ch - 2))
                        , if moNumbers o then sayM (fmtN v (moDigits o)) (x + i2f j * cw + cw / 2) (y + i2f i * ch + ch / 2) ts (col ink) else Blank ]
             | (i, row) <- zip [0 :: Int ..] a, (j, v) <- zip [0 :: Int ..] row, let t = f2 (clampv (abs v / moHi o) 0 1) ]
  , Pictures [ sayRM nm (x - 6) (y + i2f i * ch + ch / 2) ts (col dimc) | (i, nm) <- zip [0 :: Int ..] (take (length a) rowNames) ]
  , Pictures [ sayC nm (x + i2f j * cw + cw / 2) (y - 4) ts (col dimc) | (j, nm) <- zip [0 :: Int ..] (take (length (headOr [] a)) colNames) ] ]
  where
    ts = max 14 (moTs o)
    (pr, pg, pb) = panelc
    (cr, cg, cb) = moCol o
    mixC t = (pr + t * (cr * 0.75 - pr), pg + t * (cg * 0.75 - pg), pb + t * (cb * 0.75 - pb))

-- 状態の列を色の帯で（同じ色が続くところは 1 つの四角にまとめる）。色が Nothing なら描かない
drawBand :: [Int] -> Float -> Float -> Float -> Float -> (Int -> Maybe RGB) -> Picture
drawBand arr x y w h colOf = Pictures [ case colOf v of
                                          Just c -> Color (col c) (rectTL (x + i2f s / n * w) y (i2f (e - s) / n * w + 0.6) h)
                                          Nothing -> Blank
                                      | (s, e, v) <- runsOf arr ]
  where n = i2f (max 1 (length arr))

-- 同じ値が続く区間 (はじめ, 終わり, 値)
runsOf :: [Int] -> [(Int, Int, Int)]
runsOf = go 0
  where go _ [] = []
        go s (v : vs) = let k = length (takeWhile (== v) vs) in (s, s + k + 1, v) : go (s + k + 1) (drop k vs)

-- 矢印（太さ sw、頭の大きさ hs）
arrowLine :: Float -> Color -> (Float, Float) -> (Float, Float) -> Float -> Picture
arrowLine sw c (x1, y1) (x2, y2) hs =
  let l0 = hyp (x2 - x1) (y2 - y1)
      l = if l0 == 0 then 1 else l0
      ux = (x2 - x1) / l
      uy = (y2 - y1) / l
  in Color c (Pictures [ lineW sw (x1, y1) (x2 - ux * hs * 0.6, y2 - uy * hs * 0.6)
                       , Polygon [(x2, y2), (x2 - ux * hs - uy * hs * 0.5, y2 - uy * hs + ux * hs * 0.5), (x2 - ux * hs + uy * hs * 0.5, y2 - uy * hs - ux * hs * 0.5)] ])

hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 画面の文字は ASCII だけ（∞ や − などは近い ASCII に直す）
asciiText :: String -> String
asciiText = concatMap f
  where f c = case c of
          '∞' -> "inf"; '−' -> "-"; '×' -> "x"; '≈' -> "~"; '≤' -> "<="; '≥' -> ">="; '→' -> "->"; '←' -> "<-"
          '·' -> "."; '…' -> "..."; '✓' -> "OK"; '✗' -> "x"; 'π' -> "pi"; 'α' -> "a"; 'β' -> "b"; 'γ' -> "g"
          _ -> [c]

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c (asciiText s)))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 左揃え・右揃えで上下は中央（textAlign(LEFT, CENTER)、(RIGHT, CENTER)）
sayLM, sayRM :: String -> Float -> Float -> Float -> Color -> Picture
sayLM s x y size c = say s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c

-- 上が y の文字（textAlign(…, TOP)）。center = True なら中央揃え
sayT :: String -> Float -> Float -> Float -> Color -> Bool -> Picture
sayT s x y size c center = (if center then sayC else say) s x (y + size * 0.85) size c

-- p5.js 版の note(s, x, y, 色, 大きさ)：左揃え・BASELINE（大きさは 14 より小さくしない）
note :: String -> Float -> Float -> RGB -> Float -> Picture
note s x y c sz = say s x y (max 14 sz) (col c)

textW :: String -> Float -> Float             -- 文字の幅（だいたい）
textW s size = fromIntegral (length (asciiText s)) * size * 0.72

-- 幅 w に収まるように単語の切れ目で折り返す
wrapText :: Float -> Float -> String -> [String]
wrapText w size s = concatMap wrapLine (lines s)
  where
    wrapLine ln = go [] (words ln)
    go cur [] = [unwords (reverse cur) | not (null cur)]
    go cur (x : xs) = let cand = unwords (reverse (x : cur))
                      in if null cur || textW cand size <= w then go (x : cur) xs else unwords (reverse cur) : go [x] xs

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (max 0 d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (max 0 d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> thickPath w pts
  _ -> Blank

-- 点 (x, y) が長方形 (bx, by, bw, bh) の中か（左上を含み、右下を含まない）
inBox :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
inBox (mx, my) (bx, by, bw, bh) = mx >= bx && mx < bx + bw && my >= by && my < by + bh

-- パネル（p5.js 版の角の丸い長方形）
panelBox :: Float -> Float -> Float -> Float -> Picture
panelBox x y w h = Color (col panelc) (rectTL x y w h)

-- 左上の見出しと操作の説明（説明は幅に合わせて折り返す）
hudTitle :: Env -> String -> String -> Picture
hudTitle env t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (wrapText (envW env - 60) 14 s) ])

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = isFin a && abs (a - b) <= tol

noNaN :: [Double] -> Bool                     -- NaN がない（∞ はよい）
noNaN = not . any isNaN

noNaNM :: [[Double]] -> Bool
noNaNM = all noNaN

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  hSetEncoding stdout utf8
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下に小さく（p5.js 版の drawSelfTest）
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 210) (envH env - 14) 14 (if all snd rs then col leaf else col (255, 90, 90))
