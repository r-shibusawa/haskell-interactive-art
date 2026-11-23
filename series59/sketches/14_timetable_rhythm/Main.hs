-- ============================================
-- 59-14 時刻表のリズム ─ x(k+1) = A ⊗ x(k) の「できごと」を打楽器の音にする。周期 λ がテンポ、巡回性 c が「何拍で 1 まわりか」
-- 4 つのできごと（キック・スネア・ハイハット・タム）が輪（0 → 1 → 2 → 3 → 0）でつながり、それぞれ自分の休み時間（自己ループ）をもつ
--   いちばん遅い回路（臨界回路）がリズムを決める：自己ループなら毎回同じ形（c = 1）、輪全体なら 4 拍で 1 まわり（c = 4）
-- 操作：クリックで音 ON / 1・2・3 例 / ← → できごとを選ぶ / ↑↓ その休み時間（自己ループ）/ M 消音 / R 最初から
-- 音量は小さめから。ヘッドホンは特に注意（全体の音量 master は 0.2 以下）
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', sort, nub)
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

names :: [String]
names = ["Kick", "Snare", "Hi-hat", "Tom"]

-- ★ ring = 輪の辺 i → i+1 の時間、self = 自己ループ（0 はなし）、chord = 1 ⇔ 3 の辺（0 はなし）
data Preset = Preset { prName :: String, prRing :: [Int], prSelf :: [Int], prChord :: !Int, prIdx :: !Int }

presets :: [Preset]
presets =
  [ Preset "1: kick's rest is the bottleneck (c = 1)" [2, 3, 2, 3] [6, 4, 3, 4] 0 0
  , Preset "2: snare <-> tom round trip is the bottleneck (c = 2)" [2, 3, 2, 3] [4, 3, 0, 4] 5 1
  , Preset "3: the whole ring is the bottleneck (c = 4)" [5, 6, 5, 6] [4, 3, 4, 3] 0 2 ]

unitT :: Double
unitT = 0.11               -- ★ 時間 1 = 0.11 秒（テンポ）

master :: Double
master = 0.12              -- ★ 全体の音量（0.2 以下にしておく）

data Hit = Hit { hI :: !Int, hT :: !Double, hK :: !Int }

data World = World
  { wPr    :: Preset
  , wSel   :: !Int
  , wA     :: Mat
  , wLam   :: !Double
  , wCyc   :: !Int
  , wCi    :: Crit
  , wXk    :: Vec               -- 次に鳴らす x(k)
  , wK     :: !Int
  , wT0    :: !Double           -- 時刻の原点（クリックした時刻。0 = まだ）
  , wHits  :: [Hit]             -- 予約した音（新しい順。60 単位より古いものは消す）
  , wMuted :: !Bool
  , wAudio :: !Bool             -- 音 ON（クリックかキーで）
  , wPrev  :: !Double           -- 前のフレームの時刻（この間に来た音を鳴らす）
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "59-14 Timetable rhythm" (world0 0))
  { skBack  = rgbC 22 22 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (world0 0) { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: Double -> World
world0 now = loadPreset now 0 (World (headOr (Preset "" [] [] 0 0) presets) 0 [] 0 1 (Crit [] [] Nothing []) [] 0 0 [] False False 0 [])

buildA :: Preset -> Mat
buildA p = foldl' (\m (i, j, v) -> setM i j v m) (mpMat 4 4 negInf) (ringArcs ++ chordArcs)
  where
    ringArcs = concat [ [((i + 1) `mod` 4, i, fromIntegral r)] ++ [ (i, i, fromIntegral s) | s > 0 ]
                      | (i, r, s) <- zip3 [0 ..] (prRing p) (prSelf p) ]
    chordArcs = [ a | prChord p > 0, a <- [(3, 1, fromIntegral (prChord p)), (1, 3, fromIntegral (prChord p))] ]

-- 時刻（単位）：p5.js の (millis() − t0) / 1000 / UNIT
playTime :: Double -> World -> Double
playTime secs w = (secs - wT0 w) / unitT

loadPreset :: Double -> Int -> World -> World
loadPreset secs k w = rebuild secs w { wPr = headOr (wPr w) (drop k presets) }

rebuild :: Double -> World -> World
rebuild secs w = w { wA = a, wLam = lam, wCi = criticalInfo a lam, wCyc = cyclicity a lam
                   , wXk = replicate 4 (fromIntegral (ceiling (playTime secs w) :: Int)), wK = 0 }   -- いまの時刻からやり直す
  where a = buildA (wPr w)
        lam = karp a

-- ★ 先読み：次の x(k) が 0.4 秒先までに入ったら、全部の音を予約して k を 1 つ進める（1 フレームに 8 回まで）
schedule :: Double -> World -> World
schedule now w0 = go (0 :: Int) w0 { wHits = filter (\h -> hT h > now - 60) (wHits w0) }
  where
    go g w | g >= 8 || foldl' min posInf (wXk w) > now + 0.4 / unitT = w
           | otherwise = go (g + 1) w { wHits = reverse [ Hit i t (wK w) | (i, t) <- zip [0 ..] (wXk w) ] ++ wHits w
                                      , wXk = mpMatVec (wA w) (wXk w), wK = wK w + 1 }

step :: Env -> Float -> World -> IO World
step env _ w = do
  let secs = realToFrac (envTime env)
      now = playTime secs w
      w' = schedule now w
  -- 前のフレームからいままでの間に来た音を鳴らす（p5.js 版は Web Audio で先に予約する）
  when (wAudio w' && not (wMuted w')) $
    forM_ [ h | h <- wHits w', hT h > wPrev w, hT h <= now ] (drum . hI)
  pure w' { wPrev = now }

colsI :: [(Int, Int, Int)]
colsI = [(255, 170, 70), (110, 190, 255), (150, 230, 160), (220, 150, 255)]

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- ピアノロール（右はし = いま）
  concat [ [ Color (grayC (if i == wSel w then 40 else 32)) (rectTL gx (gy + fi * rh) gw (rh - 6))
           , sayR (nameOf i) (gx - 10) (gy + fi * rh + rh / 2 + 3) 15 (grayC 220) ]
         | i <- [0 .. 3], let fi = fromIntegral i ]
  ++ [ Color (grayC 60) (Line [(xOf (fromIntegral t), gy), (xOf (fromIntegral t), gy + 4 * rh)])
     | t <- [ceiling (now - wWin) .. floor now :: Int], t `mod` 10 == 0 ]
  ++ [ Color (colOf (hI h) 255) (circleAt (xOf (hT h)) (gy + fromIntegral (hI h) * rh + rh / 2 - 3) 14)
     | h <- wHits w, hT h <= now, hT h > now - wWin ]
  -- ---- 輪のグラフと光る楽器
  ++ concat [ let c = (j, i) `elem` ciArcs (wCi w)
                  col = if c then rgbC 255 170 70 else grayC 130
                  wt = if c then 3.5 else 1.5
                  (pic, lp) = if i == j
                                then let (x, y) = pP i; ox = (x - cx) * 0.45; oy = (y - cy) * 0.45
                                     in (ringAt (x + ox) (y + oy) 34 wt, (x + ox * 2.1, y + oy * 2.1))
                                else arrowTo wt (pP j) (pP i) 24 (if isEps (ix a j i) then 0 else 8)
              in [ Color col pic, sayM (mpFmt 0 aij) (fst lp) (snd lp) 14 (grayC 230) ]
            | i <- [0 .. 3], j <- [0 .. 3], let aij = ix a i j, not (isEps aij) ]
  ++ concat [ let recent = any (\h -> hI h == i && hT h <= now && hT h > now - 1.2) (wHits w)
                  (x, y) = pP i
                  d = if recent then 48 else 40
              in [ Color (if i == wSel w then rgbC 255 220 120 else black) (circleAt x y (d + (if i == wSel w then 6 else 2)))
                 , Color (colOf i (if recent then 255 else 110)) (circleAt x y d)
                 , sayM (headOr "?" (drop i ["Kick", "Snare", "Hat", "Tom"])) x y 14 (grayC 20) ]
            | i <- [0 .. 3] ]
  -- ---- 文字
  ++ [ say ("Timetable rhythm   " ++ drop 3 (prName (wPr w))) 40 40 20 (grayC 240)
     , say ((if wAudio w then (if wMuted w then "Muted (M)" else "Sound ON") else "Click for sound")
            ++ "   1 2 3 examples   <- -> choose event   Up/Down its rest (self-loop)   M mute   R restart") 40 66 15 (grayC 170)
     ] ++ [ say l tx (120 + fromIntegral k * 22) sz c | (k, (l, sz, c)) <- zip [0 :: Int ..] infoLines ] ++
     [ drawSelfTest env (wTest w) ]
  where
    a = wA w
    now = playTime (realToFrac (envTime env)) w
    wWin = 60
    gx = 140
    gw = envW env - 190
    gy = 380
    rh = 50
    xOf t = gx + f2 ((t - now + wWin) / wWin) * gw
    cx = 210
    cy = 240
    rr = 74
    pP i = let t = tau * fromIntegral i / 4 - pi / 2 in (cx + rr * cos t, cy + rr * sin t)
    tx = 420
    tw = envW env - tx - 40
    wrapL sz c str = [ (l, sz, c) | l <- wrapText tw sz str ]
    infoLines =
      wrapL 18 (rgbC 255 220 120) ("Period lambda = " ++ mpFmt 3 (wLam w) ++ " (= " ++ fmt 2 (wLam w * unitT) ++ " s)   cyclicity c = " ++ show (wCyc w)
            ++ " (same shape every " ++ show (wCyc w) ++ " periods = " ++ fmt 2 (fromIntegral (wCyc w) * wLam w * unitT) ++ " s)")
      ++ wrapL 15 (grayC 220) ("Rest of the chosen \"" ++ nameOf (wSel w) ++ "\" = " ++ (let s = vixI (prSelf (wPr w)) (wSel w) in if s > 0 then show s else "none (eps)")
                ++ ". Orange = critical circuit (what sets the rhythm)")
      ++ wrapL 15 (grayC 220) ("Critical arcs: " ++ joinWith ", " [ nameOf j ++ "->" ++ nameOf i | (j, i) <- ciArcs (wCi w) ])
      ++ wrapL 15 (grayC 170) ("x(k+1) = A (x) x(k): each event sounds a_ij after the latest of the events it waits for. Each dot below is one note. Now k = " ++ show (wK w))
    colOf i al = let (r, g, b) = headOr (255, 255, 255) (drop i colsI) in rgbaC r g b al

nameOf :: Int -> String
nameOf i = headOr "?" (drop i names)

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  (x : rest) -> x ++ concatMap (sep ++) rest

event :: Env -> Event -> World -> IO World
event env ev w0 = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    if wT0 w == 0 then rebuild secs w { wT0 = secs, wHits = [], wPrev = -0.05 } else w
  EventKey k Down _ _ -> key k w
  _ -> w0
  where
    secs = realToFrac (envTime env)
    w = w0 { wAudio = True }                         -- クリックでもキーでも音 ON（startAudio）
    setSelf d v = let p = wPr v
                      s = [ if i == wSel v then max 0 (min 12 (x + d)) else x | (i, x) <- zip [0 ..] (prSelf p) ]
                  in rebuild secs v { wPr = p { prSelf = s } }
    key k v = case k of
      Char c | c `elem` "123" -> loadPreset secs (fromEnum c - fromEnum '1') v
             | c `elem` "mM" -> v { wMuted = not (wMuted v) }
             | c `elem` "rR" -> loadPreset secs (prIdx (wPr v)) v { wHits = [] }
      SpecialKey KeyLeft -> v { wSel = (wSel v + 3) `mod` 4 }
      SpecialKey KeyRight -> v { wSel = (wSel v + 1) `mod` 4 }
      SpecialKey KeyUp -> setSelf 1 v
      SpecialKey KeyDown -> setSelf (-1) v
      _ -> v

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 14・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("the 3 examples are strongly connected, lambda = 6, 5, 5.5 (Karp = brute force)"
    , all isIrreducible as && ls == [6, 5, 5.5] && and (zipWith (\a l -> abs (bruteMaxMean a - l) < 1e-9) as ls))
  , ("cyclicity c = 1, 2, 4 (critical: kick self-loop, snare <-> tom, whole ring)", cs == [1, 2, 4])
  , ("the played rhythm (start 0, 1, 2, 3) has period = cyclicity and slope = lambda"
    , and (zipWith3 (\a l c -> let pp = powerPeriod a [0, 1, 2, 3] 300 12 in ppC pp == c && maybe False (\pl -> abs (pl - l) < 1e-9) (ppLam pp)) as ls cs))
  , ("no event time goes back (x(k+1) >= x(k)), no NaN -> notes are scheduled in order", all mono as)
  , ("random rests: still strongly connected, the visible period divides the cyclicity", and randomOk)
  , ("volume limit: master <= 0.2", master <= 0.2) ]
  where
    as = map buildA presets
    ls = map karp as
    cs = zipWith cyclicity as ls
    mono a = snd (foldl' (\(x, ok) _ -> let y = mpMatVec a x in (y, ok && and (zipWith (>=) y x) && noNaN y)) ([0, 0, 0, 0], True) [1 .. 100 :: Int])
    randomOk = withSeed 14 (mapM (const oneR) [1 .. 30 :: Int])
    oneR = do
      ring <- mapM (const (rndInt 1 7)) [1 .. 4 :: Int]
      self <- mapM (const (rndInt 0 9)) [1 .. 4 :: Int]
      ch <- rndInt 0 6
      let a = buildA (Preset "" ring self ch 0)
          l = karp a
          pp = powerPeriod a [0, 0, 0, 0] 400 24
      pure (isIrreducible a && ppC pp /= 0 && cyclicity a l `mod` ppC pp == 0)

-- ---------- やってみよう ----------
-- 1. 例 1 でキックを選び、↓ で休み時間を短くしていこう。どこでリズムの形が変わる？（臨界回路が入れかわる）
-- 2. 例 3（c = 4）は 4 拍で 1 まわり。何の楽器から順に鳴っている？
-- 3. 例 2 で、スネアの休み時間を ↑ で 6 にすると、c は 2 から何に変わる？
-- 4. unitT を小さくするとテンポが速くなる。λ と BPM（1 分あたりの拍）の関係は？
-- 5. drum の周波数や波形を変えて、自分の打楽器を作ろう。07 の生産ラインの機械を楽器にしてもいい

-- ---- 音の道具（Art.Sound の playTone で鳴らす）。音量の上限：全体の音量 master は 0.2 以下
-- 0 キック、1 スネア、2 ハイハット、3 タム。Web Audio の雑音とフィルターの代わりに、短い音を 1 つ鳴らす
drum :: Int -> IO ()
drum kind = case kind of
  0 -> tone 75 0.9 0.15 Sine          -- キック：低いサイン波
  3 -> tone 130 0.9 0.15 Sine         -- タム：少し高いサイン波
  1 -> tone 1800 0.35 0.06 Sawtooth   -- スネア：短いのこぎり波
  _ -> tone 7000 0.2 0.03 Square      -- ハイハット：とても短く高い音
  where tone f g d wv = playTone f (realToFrac (min 0.2 master * g)) d wv

-- ==================================================
-- ここから下は「マックスプラスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。ε = −∞ は Double の −Infinity で表し、
-- ⊗ は必ず mpMul を使う（ε があれば ε。−∞ + ∞ = NaN を作らない）
-- ==================================================
-- ---- マックスプラスの数：ε = −∞（「ゼロ」= 起きない・制約なし）、e = 0（「いち」= 待ち時間なし）
negInf, posInf :: Double
negInf = -1 / 0
posInf = 1 / 0

isEps :: Double -> Bool
isEps v = v == negInf

mpAdd :: Double -> Double -> Double          -- a ⊕ b = max(a, b)
mpAdd a b = if a >= b then a else b

mpMul :: Double -> Double -> Double          -- a ⊗ b = a + b（ε があれば ε）
mpMul a b = if a == negInf || b == negInf then negInf else a + b

-- ε を「eps」と表示（−Infinity を見せない）。d = 小数点以下の桁（toFixed のあと余分な 0 を消す）
mpFmt :: Int -> Double -> String
mpFmt d v
  | v == negInf = "eps"
  | v == posInf = "+inf"
  | isNaN v = "?"
  | otherwise = let s = stripZeros (showFFloat (Just d) v "")
                in if s == "-0" then "0" else s
  where stripZeros s | '.' `elem` s = let t = reverse (dropWhile (== '0') (reverse s))
                                      in if last' t == '.' then init t else t
                     | otherwise = s
        last' t = if null t then ' ' else last t

-- 小数 1 桁（p5 の mpFmt(v) と同じ）
mpF :: Double -> String
mpF = mpFmt 1
-- ---- マックスプラス行列（リストのリスト）。(A ⊗ B)_ij = max_k (a_ik + b_kj)
type Mat = [[Double]]
type Vec = [Double]

-- (i, j) の成分（範囲の外は ε）
ix :: Mat -> Int -> Int -> Double
ix a i j = vix (rowOf a i) j

rowOf :: Mat -> Int -> Vec
rowOf a i = case drop i a of
  (r : _) | i >= 0 -> r
  _ -> []

vix :: Vec -> Int -> Double
vix v j = case drop j v of
  (x : _) | j >= 0 -> x
  _ -> negInf

setM :: Int -> Int -> Double -> Mat -> Mat
setM i j x = zipWith (\r row -> if r == i then zipWith (\c v -> if c == j then x else v) [0 ..] row else row) [0 ..]

mpMat :: Int -> Int -> Double -> Mat
mpMat n m v = replicate n (replicate m v)

mpEye :: Int -> Mat                           -- 単位行列 E（対角 e = 0、他は ε）
mpEye n = [ [ if i == j then 0 else negInf | j <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]

transposeM :: Mat -> Mat
transposeM a = case a of
  [] -> []
  (r : _) -> [ map (`vix` j) a | j <- [0 .. length r - 1] ]

mpMatMul :: Mat -> Mat -> Mat
mpMatMul a b = let bt = transposeM b
               in [ [ foldl' mpAdd negInf (zipWith mpMul r c) | c <- bt ] | r <- a ]

mpMatAdd :: Mat -> Mat -> Mat
mpMatAdd = zipWith (zipWith mpAdd)

mpMatVec :: Mat -> Vec -> Vec
mpMatVec a x = [ foldl' mpAdd negInf (zipWith mpMul r x) | r <- a ]

mpPow :: Mat -> Int -> Mat
mpPow a k = foldl' (\p _ -> mpMatMul p a) (mpEye (length a)) [1 .. k]

-- A* = E ⊕ A ⊕ A² ⊕ … ⊕ A^(n−1)。正の回路があると A* は発散する → 2 つ目が True（tol は計算誤差のゆとり）
mpStar :: Double -> Mat -> (Mat, Bool)
mpStar tol a = (st, positive)
  where
    n = length a
    pows = take (max 1 (n - 1)) (iterate (`mpMatMul` a) a)      -- A, A², …, A^(n−1)（n = 1 なら A だけ）
    st = foldl' mpMatAdd (mpEye n) pows
    plus = foldl' mpMatAdd a (take (max 0 (n - 1)) (drop 1 (iterate (`mpMatMul` a) a)))   -- A ⊕ … ⊕ A^n
    positive = or [ ix plus i i > tol | i <- [0 .. n - 1] ]

-- 残余（いちばん大きい劣解）：A ⊗ x ≤ b を満たす最大の x。x_j = min_i (b_i − a_ij)（a_ij = ε の行は制約なし）
mpResid :: Mat -> Vec -> Vec
mpResid a b = [ foldl' min posInf [ if isEps bi then negInf else bi - aij
                                  | (r, bi) <- zip a b, let aij = vix r j, not (isEps aij) ]
              | j <- [0 .. m - 1] ]
  where m = case a of { (r : _) -> length r; [] -> 0 }

mpVecLeq :: Double -> Vec -> Vec -> Bool
mpVecLeq tol u v = and (zipWith (\a b -> isEps a || a <= b + tol) u v) && length u <= length v

mpVecEq :: Double -> Vec -> Vec -> Bool
mpVecEq tol u v = length u <= length v && and (zipWith (\a b -> (isEps a && isEps b) || (not (isEps a) && not (isEps b) && abs (a - b) <= tol)) u v)
-- ---- 固有値・臨界グラフ・周期（約束：a_ij = 「j が起きてから i が起きるまで」= 辺 j → i の重み、x(k+1) = A ⊗ x(k)）
-- カープのアルゴリズム：D_k(v) = ちょうど k 本の辺で v に着く最大重み。λ = max_v min_k (D_n(v) − D_k(v)) / (n − k)
karp :: Mat -> Double
karp a = foldl' max negInf
  [ m | v <- [0 .. n - 1], let dn = vix (vix' n) v, not (isEps dn)
      , let ms = [ (dn - dk) / fromIntegral (n - k) | k <- [0 .. n - 1], let dk = vix (vix' k) v, not (isEps dk) ]
      , not (null ms), let m = minimum ms ]
  where
    n = length a
    ds = take (n + 1) (iterate (mpMatVec a) (replicate n 0))
    vix' k = case drop k ds of { (d : _) -> d; [] -> [] }

-- 単純な回路（ノードは辺の向きの順、重み、平均）
data Cycle = Cycle { cyNodes :: [Int], cyW :: !Double, cyMean :: !Double }

-- 全部の単純な回路（小さいグラフ用の答え合わせ）
allCycles :: Mat -> [Cycle]
allCycles a = concat [ dfs s s [s] 0 | s <- [0 .. n - 1] ]
  where
    n = length a
    dfs s v path w = concat
      [ if u == s then [Cycle path (w + auv) ((w + auv) / fromIntegral (length path))]
        else if u `elem` path then [] else dfs s u (path ++ [u]) (w + auv)
      | u <- [s .. n - 1], let auv = ix a u v, not (isEps auv) ]

bruteMaxMean :: Mat -> Double
bruteMaxMean a = foldl' max negInf (map cyMean (allCycles a))

-- どのノードからどのノードへも行ける（強連結）
isIrreducible :: Mat -> Bool
isIrreducible a = n > 0 && reach True && reach False
  where
    n = length a
    reach fwd = go [0] [0]
      where go seen [] = length seen == n
            go seen (v : st) = let new = [ u | u <- [0 .. n - 1], not (isEps (if fwd then ix a u v else ix a v u)), u `notElem` seen ]
                               in go (seen ++ new) (reverse new ++ st)

mpShift :: Mat -> Double -> Mat                -- A − λ
mpShift a lam = map (map (\v -> if isEps v then negInf else v - lam)) a

-- 臨界グラフ：平均 λ の回路にのる辺とノード。固有ベクトル v = (A − λ)* の臨界ノード c の列
data Crit = Crit { ciNodes :: [Int], ciArcs :: [(Int, Int)], ciVec :: Maybe Vec, ciStar :: Mat }

criticalInfo :: Mat -> Double -> Crit
criticalInfo a lam = Crit nodes arcs vec bs
  where
    n = length a
    b = mpShift a lam
    bs = fst (mpStar 1e-7 b)
    arcs = [ (j, i) | i <- [0 .. n - 1], j <- [0 .. n - 1]
           , let bij = ix b i j, let bji = ix bs j i
           , not (isEps bij), not (isEps bji), abs (bij + bji) < 1e-7 ]
    nodes = sort (nub (concat [ [j, i] | (j, i) <- arcs ]))
    vec = case nodes of
      [] -> Nothing
      (c : _) -> let col = map (`vix` c) bs
                     mx = foldl' max negInf col
                 in Just (map (\v -> if isEps v then negInf else v - mx) col)

-- 巡回性：臨界グラフの強連結成分ごとに回路の長さの最大公約数、全体はその最小公倍数
cyclicity :: Mat -> Double -> Int
cyclicity a lam = go [] 1 (ciNodes ci)
  where
    ci = criticalInfo a lam
    arcs = ciArcs ci
    go _ cyc [] = cyc
    go done cyc (r : rs)
      | r `elem` done = go done cyc rs
      | otherwise =
          let lev = bfs [(r, 0)] [r]
              levOf u = lookup u lev
              comp = [ u | u <- ciNodes ci, levOf u /= Nothing, reachesBack arcs u r ]
              g = foldl' gcd 0 [ lj + 1 - li | (j, i) <- arcs, j `elem` comp, i `elem` comp
                                             , Just lj <- [levOf j], Just li <- [levOf i] ]
              cyc' = if g > 0 then lcm cyc g else cyc
          in go (done ++ comp) cyc' rs
    bfs lev [] = lev
    bfs lev (v : q) = let lv = maybe 0 id (lookup v lev)
                          new = nubOrd' [ i | (j, i) <- arcs, j == v ] lev
                          lev' = lev ++ [ (i, lv + 1) | i <- new ]
                      in bfs lev' (q ++ new)
    nubOrd' is lev = foldl' (\acc i -> if lookup i lev /= Nothing || i `elem` acc then acc else acc ++ [i]) [] is

reachesBack :: [(Int, Int)] -> Int -> Int -> Bool
reachesBack arcs u r = go [u] [u]
  where go _ [] = False
        go seen (v : q) | v == r = True
                        | otherwise = let new = foldl' (\acc i -> if i `elem` seen || i `elem` acc then acc else acc ++ [i]) [] [ i | (j, i) <- arcs, j == v ]
                                      in go (seen ++ new) (q ++ new)

-- べき乗法：x(k+1) = A ⊗ x(k) を回し、はじめて x(k0 + c) = c·λ ⊗ x(k0) になる (k0, c) を探す
--   見つからなければ ppK0 = −1、ppC = 0、ppLam = Nothing
data Period = Period { ppK0 :: !Int, ppC :: !Int, ppLam :: Maybe Double, ppXs :: [Vec] }

powerPeriod :: Mat -> Vec -> Int -> Int -> Period
powerPeriod a x0 kmax cmax = go 1 [x0]
  where
    go k rev
      | k > kmax = Period (-1) 0 Nothing (reverse rev)
      | otherwise =
          let xk = mpMatVec a (headOr [] rev)
              rev' = xk : rev
              hits = [ (c, d) | c <- [1 .. min k cmax], Just d <- [sameShift xk (headOr [] (drop c rev'))] ]
          in case hits of
               ((c, d) : _) -> Period (k - c) c (Just (d / fromIntegral c)) (reverse rev')
               [] -> go (k + 1) rev'
    -- a と b がどの成分も同じ d だけずれているか（ε は ε どうしだけ一致）
    sameShift xa xb = chk Nothing (zip xa xb)
      where chk d [] = d
            chk d ((p, q) : rest)
              | isEps p || isEps q = if p == q then chk d rest else Nothing
              | otherwise = let di = p - q in case d of
                  Nothing -> chk (Just di) rest
                  Just d0 -> if abs (di - d0) > 1e-9 then Nothing else chk d rest

-- 強連結なランダムグラフ：ハミルトン閉路（全ノードを 1 周）を必ず入れる（乱数を使う順は p5.js 版と同じ）
randomIrreducible :: Int -> Double -> Int -> Int -> R Mat
randomIrreducible n p wmin wmax = do
  perm <- shuffle [0 .. n - 1]
  let pv i = vixI perm (i `mod` n)
  ws <- mapM (const (rndInt wmin wmax)) [0 .. n - 1]
  let a0 = foldl' (\m (k, w) -> setM (pv (k + 1)) (pv k) (fromIntegral w) m) (mpMat n n negInf) (zip [0 ..] ws)
  foldM (\m (i, j) -> if isEps (ix m i j)
                        then do u <- rnd
                                if u < p then do w <- rndInt wmin wmax; pure (setM i j (fromIntegral w) m) else pure m
                        else pure m) a0 [ (i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1] ]
  where
    -- フィッシャー–イェーツ（i = n−1 から 1 へ、j = floor(rnd·(i+1))）
    shuffle xs = foldM (\ys i -> do u <- rnd
                                    let j = floor (u * fromIntegral (i + 1))
                                        yi = vixI ys i
                                        yj = vixI ys j
                                    pure [ if k == i then yj else if k == j then yi else y | (k, y) <- zip [0 ..] ys ])
                       xs [n - 1, n - 2 .. 1]

vixI :: [Int] -> Int -> Int
vixI xs k = case drop k xs of { (x : _) | k >= 0 -> x; _ -> 0 }
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

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

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))
-- ---- 矢印（ノードの半径 r の手前で止める）。bend で少し曲げる（往復の辺を分ける）。返り値の 2 つ目 = 数字を書く位置
arrowTo :: Float -> Point -> Point -> Float -> Float -> (Picture, Point)
arrowTo wt (xa, ya) (xb, yb) r bend = (Pictures [thickLine wt (ax, ay) (bx, by), Polygon [(bx, by), (hx - uy * 6, hy + ux * 6), (hx + uy * 6, hy - ux * 6)]], lp)
  where
    dx = xb - xa
    dy = yb - ya
    l = let d = sqrt (dx * dx + dy * dy) in if d == 0 then 1 else d
    ux = dx / l
    uy = dy / l
    ox = -uy * bend
    oy = ux * bend
    ax = xa + ux * r + ox
    ay = ya + uy * r + oy
    bx = xb - ux * r + ox
    by = yb - uy * r + oy
    hx = bx - ux * 12
    hy = by - uy * 12
    lp = ((ax + bx) / 2 + ox * 0.6, (ay + by) / 2 + oy * 0.6)
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
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

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

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
