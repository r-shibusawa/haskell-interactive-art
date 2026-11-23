-- ============================================
-- 62-02 手で木を作る ─ マウスで縦・横の線を引いて平面を分け、自分の「決定木」を作る
-- 1 本の線 = 1 つの質問「x₁ ≤ ○？」（縦）か「x₂ ≤ ○？」（横）。線はクリックした長方形の中だけに引かれる（= その葉を 2 つに分ける）
--   各長方形の色 = 多数決のクラス、数字 = 件数 [A|B] とジニ。右の表示：訓練の正解率と、ジニの重みつき合計（小さいほどよい）
--   A を押すと、同じ葉の数で CART（毎回いちばんジニが減る葉を割る）が引く線を破線で重ねる。勝てるかな？
-- 操作：クリック 線を引く / V 縦 ⇔ 横 / Z 1 本戻す / C 全部消す / A CART を重ねる / 1〜6 データの形 / R 新しいデータ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftR, xor, (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', intercalate, nub, sort, sortOn)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

nPts :: Int
nPts = 120                 -- ★ 点の数

flipP :: Double
flipP = 0.04               -- ★ ラベルのノイズ

-- 手で作る木のノード。hBox = (u0, u1, v0, v1) = このノードの長方形
data HNode = HNode
  { hId :: !Int, hDepth :: !Int, hIdx :: [Int], hBox :: !(Double, Double, Double, Double)
  , hCounts :: !(U.Vector Double), hN :: !Int, hImp :: !Double, hPred :: !Int
  , hLeaf :: !Bool, hFeat :: !Int, hThr :: !Double, hLeft :: HNode, hRight :: HNode }

data World = World
  { wKind  :: !Int          -- ★ データの形（4 = 箱）
  , wSeed  :: !Int
  , wData  :: !DataSet
  , wRoot  :: HNode         -- あなたの木
  , wUid   :: !Int
  , wUndo  :: [HNode]       -- 戻すための、前の木
  , wVert  :: !Bool         -- 線の向き（縦 = x1）
  , wShowC :: !Bool
  , wCart  :: HNode         -- 同じ葉の数の CART
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "62-02 Build a tree by hand" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = newData (World 4 2 (DataSet V.empty U.empty) h0 0 [] True False h0 [])
  where h0 = fst (makeLeaf (DataSet V.empty U.empty) [] fullBox 0 0)

fullBox :: (Double, Double, Double, Double)
fullBox = (0, 1, 0, 1)

newData :: World -> World
newData w = clearAll w { wData = fst (evalR (rngOf 1) (makeData2D (wKind w) nPts flipP (wSeed w))) }

clearAll :: World -> World
clearAll w = let d = wData w
                 (r, u) = makeLeaf d [0 .. V.length (dsX d) - 1] fullBox 0 0
             in updateCart w { wRoot = r, wUid = u, wUndo = [] }

updateCart :: World -> World
updateCart w = w { wCart = cartBestFirst (wData w) (length (leavesOf (wRoot w))) }

-- 葉を作る（返り値の 2 つ目 = 次の番号）
makeLeaf :: DataSet -> [Int] -> (Double, Double, Double, Double) -> Int -> Int -> (HNode, Int)
makeLeaf d idx box depth uid =
  let counts = U.accum (+) (U.replicate 2 0) [(dsY d U.! i, 1) | i <- idx]
      t = HNode uid depth idx box counts (length idx) (giniOf counts) (argMaxU counts) True 0 0 t t
  in (t, uid + 1)

-- ★ 番号 lid の葉を、特徴量 f のしきい値 th で 2 つに分ける（左 = th 以下）
splitLeaf :: DataSet -> Int -> Int -> Double -> (HNode, Int) -> (HNode, Int)
splitLeaf d lid f th (t, uid)
  | hLeaf t && hId t == lid =
      let l = filter (\i -> xAt (dsX d) i f <= th) (hIdx t)
          r = filter (\i -> xAt (dsX d) i f > th) (hIdx t)
          (u0, u1, v0, v1) = hBox t
          (lt, uid1) = makeLeaf d l (if f == 0 then (u0, th, v0, v1) else (u0, u1, v0, th)) (hDepth t + 1) uid
          (rt, uid2) = makeLeaf d r (if f == 0 then (th, u1, v0, v1) else (u0, u1, th, v1)) (hDepth t + 1) uid1
      in (t { hLeaf = False, hFeat = f, hThr = th, hLeft = lt, hRight = rt }, uid2)
  | hLeaf t = (t, uid)
  | otherwise = let (l, u1) = splitLeaf d lid f th (hLeft t, uid)
                    (r, u2) = splitLeaf d lid f th (hRight t, u1)
                in (t { hLeft = l, hRight = r }, u2)

leavesOf :: HNode -> [HNode]
leavesOf t = if hLeaf t then [t] else leavesOf (hLeft t) ++ leavesOf (hRight t)

findLeaf :: HNode -> Double -> Double -> HNode
findLeaf t u v
  | hLeaf t = t
  | otherwise = findLeaf (if (if hFeat t == 0 then u else v) <= hThr t then hLeft t else hRight t) u v

weightedGini :: HNode -> Double
weightedGini t = let ls = leavesOf t
                     n0 = sum (map hN ls)
                     n = fromIntegral (if n0 == 0 then 1 else n0)
                 in foldl' (+) 0 [(fromIntegral (hN l) / n) * hImp l | l <- ls]

handAcc :: HNode -> DataSet -> Double
handAcc t d = accuracyOf (\x -> hPred (findLeaf t (x U.! 0) (x U.! 1))) (dsX d) (dsY d)

-- ★ CART の「よいほうから」：葉の数が k になるまで、ジニがいちばん減る葉を割る
cartBestFirst :: DataSet -> Int -> HNode
cartBestFirst d k = go (makeLeaf d [0 .. V.length (dsX d) - 1] fullBox 0 0)
  where
    go (r, uid)
      | length (leavesOf r) >= k = r
      | otherwise =
          let cand l = let s = growTreeP defOpt { oMaxDepth = 1, oIdx = Just (U.fromList (hIdx l)), oNClass = 2 } (dsX d) (dsY d)
                       in if nLeaf s then Nothing else Just (l, s)
              pick b c = case (b, c) of
                (_, Nothing) -> b
                (Nothing, _) -> c
                (Just (_, bs), Just (_, cs)) -> if nDec cs > nDec bs + 1e-12 then c else b
          in case foldl' pick Nothing (map cand (leavesOf r)) of
               Nothing -> r
               Just (l, s) -> go (splitLeaf d (hId l) (nFeat s) (nThr s) (r, uid))

plotBox :: Env -> Box
plotBox env = let side = min (envH env - 200) (envW env * 0.5) in Box 40 100 side side

draw :: Env -> World -> Picture
draw env w = view env $
  -- 葉ごとの長方形
  [ Pictures (map leafRect (leavesOf (wRoot w)))
  -- 自分の線、CART の線（破線）
  , drawLines False (rgbC 255 240 150) 3 (wRoot w)
  , if wShowC w then drawLines True (rgbaC 255 255 255 200) 2 (wCart w) else Blank
  , drawPoints pb (dsX d) (dsY d) 9 Nothing
  , Color (rgbC 90 120 100) (frameTL 1 (bxX pb) (bxY pb) side side)
  -- マウスの位置に予告の線
  , if mu > 0 && mu < 1 && mv > 0 && mv < 1
      then let l = findLeaf (wRoot w) mu mv
               (u0, u1, v0, v1) = hBox l
               (ax, ay) = toPx pb u0 v0
               (bx', by) = toPx pb u1 v1
           in Color (rgbaC 255 255 255 120) (if wVert w then lineW 1.5 (mx, ay) (mx, by) else lineW 1.5 (ax, my) (bx', my))
      else Blank
  -- ---- 文字
  , say ("Build a tree by hand   data: " ++ shapeName (wKind w) ++ "   line: " ++ (if wVert w then "vertical (x1 <= ?)" else "horizontal (x2 <= ?)")) 40 40 20 (grayC 240)
  , say ("click draw a line   V vertical <-> horizontal   Z undo   C clear   A show CART" ++ (if wShowC w then " (ON)" else "") ++ "   1-6 data shape   R new data") 40 66 15 (grayC 170)
  , Color (rgbC 35 52 44) (rectTL px 100 pw 190)
  , say ("Your tree: " ++ show (length ls) ++ " leaves") (px + 20) 136 22 (rgbC 255 240 150)
  , say ("training accuracy " ++ fmtNum 1 (acc * 100) ++ "%") (px + 20) 170 17 (grayC 230)
  , say ("weighted Gini  sum (n_leaf / N) * Gini = " ++ fmtNum 4 wg) (px + 20) 198 17 (grayC 230)
  , sayWrap ("CART (same leaves): accuracy " ++ fmtNum 1 (cAcc * 100) ++ "%, Gini " ++ fmtNum 4 cg ++ (if wg <= cg + 1e-12 then "  -> you win (or tie)!" else ""))
      (px + 20) 212 (pw - 30) 16 (grayC 200)
  -- 自分の木を文字で
  , Pictures [ say s (px + 10) (320 + fromIntegral k * 20) 14 (grayC 200) | (k, s) <- zip [0 :: Int ..] (take 15 lns) ]
  , if length lns > 15 then say "..." (px + 10) (320 + 15 * 20) 14 (grayC 200) else Blank
  , sayWrap "Splitting a leaf never increases the weighted Gini. But with too many leaves you get a rectangle just for one noisy point (05)."
      px (envH env - 76) pw 14 (grayC 160)
  , drawSelfTest env (wTest w) ]
  where
    pb = plotBox env
    side = bxW pb
    d = wData w
    (mx, my) = mouseP env
    (mu, mv) = fromPx pb (mx, my)
    ls = leavesOf (wRoot w)
    px = bxX pb + side + 50
    pw = envW env - px - 40
    wg = weightedGini (wRoot w)
    acc = handAcc (wRoot w) d
    cAcc = handAcc (wCart w) d
    cg = weightedGini (wCart w)
    lns = listTree (wRoot w) ""
    leafRect l =
      let (u0, u1, v0, v1) = hBox l
          (ax, ay) = toPx pb u0 v1
          (bx', by) = toPx pb u1 v0
          c = if hN l > 0 then mixCol (hCounts l) (0.2 + 0.45 * (1 - 2 * hImp l)) else bgCol
      in Pictures [ Color (colOf c) (rectTL ax ay (bx' - ax) (by - ay))
                  , if bx' - ax > 90 && by - ay > 34 then sayC (countsStr (hCounts l) ++ " " ++ fmtNum 2 (hImp l)) ((ax + bx') / 2) (ay + 20) 14 (rgbaC 255 255 255 200) else Blank ]
    drawLines dash col wd t
      | hLeaf t = Blank
      | otherwise =
          let (u0, u1, v0, v1) = hBox t
              (p, q) = if hFeat t == 0 then (toPx pb (hThr t) v0, toPx pb (hThr t) v1) else (toPx pb u0 (hThr t), toPx pb u1 (hThr t))
          in Pictures [Color col (if dash then dashLine wd p q else lineW wd p q), drawLines dash col wd (hLeft t), drawLines dash col wd (hRight t)]

-- 破線（8 描いて 6 あける）
dashLine :: Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine wd (x1, y1) (x2, y2) =
  let len = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
      at' s = (x1 + (x2 - x1) * s / max 1e-6 len, y1 + (y2 - y1) * s / max 1e-6 len)
  in Pictures [lineW wd (at' s) (at' (min len (s + 8))) | s <- takeWhile (< len) [0, 14 ..]]

-- 木を文字で（if 文の形）
listTree :: HNode -> String -> [String]
listTree t ind
  | hLeaf t = [ind ++ "-> " ++ (if hPred t == 1 then "B" else "A") ++ "  " ++ countsStr (hCounts t)]
  | otherwise = (ind ++ "x" ++ show (hFeat t + 1) ++ " <= " ++ fmtNum 2 (hThr t) ++ " ?") : listTree (hLeft t) (ind ++ "   ") ++ listTree (hRight t) (ind ++ "   ")

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (u, v) = fromPx (plotBox env) (toP5 env pos)
    in if u <= 0 || u >= 1 || v <= 0 || v >= 1 then w
       else let l = findLeaf (wRoot w) u v
                (r, uid) = splitLeaf (wData w) (hId l) (if wVert w then 0 else 1) (if wVert w then u else v) (wRoot w, wUid w)
            in updateCart w { wRoot = r, wUid = uid, wUndo = wRoot w : wUndo w }
  EventKey (Char c) Down _ _
    | c `elem` "vV" -> w { wVert = not (wVert w) }
    | c `elem` "zZ" -> case wUndo w of
        (prev : rest) -> updateCart w { wRoot = prev, wUndo = rest }
        [] -> w
    | c `elem` "cC" -> clearAll w
    | c `elem` "aA" -> w { wShowC = not (wShowC w) }
    | c >= '1' && c <= '6' -> newData w { wKind = fromEnum c - fromEnum '1' }
    | c `elem` "rR" -> newData w { wSeed = wSeed w + 1 }
  _ -> w

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("割った 2 つの葉の件数の合計 = もとの葉の件数（12 回）", okC)
  , ("葉を割るたびに、ジニの重みつき合計は増えない", okM)
  , ("正解率 = 葉ごとの多数決の個数の合計 / N（1 点ずつ数えた値と一致）", near byLeaf (handAcc r12 dd) 1e-9)
  , ("CART の 1 本目のジニ " ++ fmtNum 4 (weightedGini c2) ++ " ≤ ランダムな 400 本のどれよりも小さいか同じ", weightedGini c2 <= bestHand + 1e-12)
  , ("となりの 2 点の間なら、どこで切っても中点で切っても同じ分け方", hN (hLeft a) == hN (hLeft b2) && near (weightedGini a) (weightedGini b2) 1e-9)
  , ("CART を葉 8 枚まで育てると、訓練の正解率 ≥ 1 枚（全部を多数決）のとき", handAcc (cartBestFirst dd 8) dd >= handAcc (cartBestFirst dd 1) dd) ]
  where
    dd = fst (evalR (rngOf 1) (makeData2D 4 120 0.04 77))
    all0 = [0 .. V.length (dsX dd) - 1]
    root0 = makeLeaf dd all0 fullBox 0 0
    -- ランダムに 12 回割る（seedRng(9) から）、続けて 400 本の 1 本だけの木
    ((r12, okC, okM, hands), _) = evalR (rngOf 1) $ do
      seedRng 9
      let one (st@(t, _), oc, om, prevG) _ = do
            let ls = leavesOf t
            q <- rnd
            let l = headOr t (drop (floor (q * fromIntegral (length ls))) ls)
            fr <- rnd
            let f = if fr < 0.5 then 0 else 1
                (u0, u1, v0, v1) = hBox l
            s <- rnd
            let th = if f == 0 then u0 + s * (u1 - u0) else v0 + s * (v1 - v0)
                st'@(t', _) = splitLeaf dd (hId l) f th st
                l' = findById (hId l) t'
                oc' = oc && hN (hLeft l') + hN (hRight l') == hN l' && (hCounts (hLeft l') U.! 1) + (hCounts (hRight l') U.! 1) == hCounts l' U.! 1
                g = weightedGini t'
            pure (st', oc', om && not (g > prevG + 1e-12), g)
      (st, oc, om, _) <- foldlM one (root0, True, True, weightedGini (fst root0)) [1 .. 12 :: Int]
      hs <- mapM (\k -> do
              th <- rnd
              pure (weightedGini (fst (splitLeaf dd 0 (k `mod` 2) th root0)))) [0 .. 399 :: Int]
      pure (fst st, oc, om, hs)
    byLeaf = foldl' (+) 0 [hCounts l U.! hPred l | l <- leavesOf r12] / fromIntegral (V.length (dsX dd))
    c2 = cartBestFirst dd 2
    bestHand = minOr posInf hands
    xs = sort (map (U.! 0) (V.toList (dsX dd)))
    x50 = headOr 0 (drop 50 xs)
    x51 = headOr 0 (drop 51 xs)
    a = fst (splitLeaf dd 0 0 (x50 + 0.1 * (x51 - x50)) root0)
    b2 = fst (splitLeaf dd 0 0 ((x50 + x51) / 2) root0)

findById :: Int -> HNode -> HNode
findById i t
  | hId t == i || hLeaf t = t
  | otherwise = let l = findById i (hLeft t) in if hId l == i then l else findById i (hRight t)

foldlM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldlM f z xs = case xs of
  [] -> pure z
  (x : r) -> f z x >>= \z' -> foldlM f z' r

-- ---------- やってみよう ----------
-- 1. 5（箱）のデータで、線 4 本以内に正解率 95% 以上をめざそう
-- 2. A で CART の線を重ねる。CART は「いま、いちばんジニが減る線」を選ぶだけ。先を読むと勝てることもある？
-- 3. 2（斜め）を手で分けると、線は何本くらい要る？ 斜めの線が引ければ 1 本なのに…（13）
-- 4. ノイズの点 1 個を囲むように線を引くと、正解率は上がる。でも新しいデータでは？（05）
-- 5. 右の木の表示（x1 <= … ?）は、そのまま if 文になる。自分の木を if 文で書いてみよう

-- ==================================================
-- ここから下は「決定木の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。点 x = U.Vector Double（x1, x2, …）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 小さな道具：はさむ・合計・いちばん大きい番号
type Pt = U.Vector Double                     -- 点（x1, x2, …）

pt2 :: Double -> Double -> Pt
pt2 u v = U.fromList [u, v]

-- データ：点 X とクラス y（0, 1, 2）
data DataSet = DataSet { dsX :: !(V.Vector Pt), dsY :: !(U.Vector Int) }

xAt :: V.Vector Pt -> Int -> Int -> Double    -- X[i][f]
xAt xs i f = (xs V.! i) U.! f

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else v

sumU :: U.Vector Double -> Double
sumU = U.foldl' (+) 0

-- いちばん大きい番号（同点なら小さい番号）
argMaxU :: U.Vector Double -> Int
argMaxU a = if U.null a then 0 else U.ifoldl' (\b i v -> if v > a U.! b then i else b) 0 a

maxU :: U.Vector Double -> Double
maxU a = if U.null a then 0 else U.maximum a

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- リストの最初（なければ d）
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

-- ---- 不純度：counts = 各クラスの個数（重みつきでもよい）
data Crit = Gini | Entropy | MisErr deriving (Eq, Show)

log2 :: Double -> Double
log2 q = log q / log 2

entropyP :: Double -> Double               -- 2 クラスのエントロピー（ビット）
entropyP p = if p <= 0 || p >= 1 then 0 else -p * log2 p - (1 - p) * log2 (1 - p)

entropyOf :: U.Vector Double -> Double
entropyOf counts = let n = sumU counts in if n <= 0 then 0 else
  max 0 (U.foldl' (\h c -> if c > 0 then let q = c / n in h - q * log2 q else h) 0 counts)

giniOf :: U.Vector Double -> Double
giniOf counts = let n = sumU counts in if n <= 0 then 0 else
  max 0 (U.foldl' (\s c -> s - (c / n) * (c / n)) 1 counts)

errOf :: U.Vector Double -> Double           -- 誤分類率
errOf counts = let n = sumU counts in if n <= 0 then 0 else max 0 (1 - maxU counts / n)

impurityOf :: Crit -> U.Vector Double -> Double
impurityOf crit = case crit of
  Entropy -> entropyOf
  MisErr -> errOf
  Gini -> giniOf

-- 分けたときの「不純度の減り」= 親 − (左の割合·左 + 右の割合·右)。Entropy なら情報利得
gainOf :: Crit -> U.Vector Double -> U.Vector Double -> U.Vector Double -> Double
gainOf crit parent left right =
  let n = sumU parent
      nl = sumU left
      nr = sumU right
  in if n <= 0 then 0 else impurityOf crit parent - (nl / n) * impurityOf crit left - (nr / n) * impurityOf crit right

cnt :: [Double] -> U.Vector Double
cnt = U.fromList

-- ---- CART（分類）：全部の特徴量 × 全部のしきい値（並べた値の中点）を調べて、不純度がいちばん減る分割を選ぶ
-- ノード：分類の木も回帰の木もこの形。葉の nLeft, nRight は使わない
data Node = Node
  { nLeaf   :: !Bool
  , nFeat   :: !Int                 -- 何番目の特徴量で切るか（0 = x1）
  , nThr    :: !Double              -- しきい値（x ≤ thr なら左）
  , nLeft   :: Node
  , nRight  :: Node
  , nCounts :: !(U.Vector Double)   -- 各クラスの個数（重みつき）
  , nN      :: !Int                 -- 点の数
  , nImp    :: !Double              -- 不純度
  , nPred   :: !Int                 -- 多数決のクラス
  , nDec    :: !Double              -- 分けたときの重みつきの不純度の減り（回帰木では SSE の減り）
  , nDepth  :: !Int
  , nId     :: !Int                 -- 行きがけ順の番号（0 = 根）
  , nCut    :: !Double              -- 剪定：この α 以上で葉として扱う（刈られないなら +∞）
  , nValue  :: !Double              -- 回帰木：葉の値（平均）
  , nSse    :: !Double              -- 回帰木：2 乗誤差
  }

nilNode :: Node
nilNode = Node True 0 0 nilNode nilNode U.empty 0 0 0 0 0 0 posInf 0 0

-- o = { maxDepth, minLeaf, crit, mtry（分割ごとに見る特徴量の数、0 = 全部）, w（点の重み）, idx（使う点の番号）, nClass（0 = 自動）}
data TreeOpt = TreeOpt
  { oMaxDepth :: !Int, oMinLeaf :: !Int, oCrit :: !Crit, oMtry :: !Int
  , oW :: !(Maybe (U.Vector Double)), oIdx :: !(Maybe (U.Vector Int)), oNClass :: !Int }

defOpt :: TreeOpt
defOpt = TreeOpt 40 1 Gini 0 Nothing Nothing 0

-- 育てるときの道具箱：cCols = 特徴量ごとの列、cW = 点の重み
data Ctx = Ctx { cCols :: !(V.Vector (U.Vector Double)), cY :: !(U.Vector Int), cW :: !(U.Vector Double)
               , cK :: !Int, cD :: !Int, cO :: !TreeOpt }

-- 木を育てる（mtry を使うときだけ乱数を引く）
-- 速くするため、根で特徴量ごとに点を 1 回だけ並べ、子にはその並びを分けて渡す（並べ直したのと同じ順になる）
growTree :: TreeOpt -> V.Vector Pt -> U.Vector Int -> R Node
growTree o xs ys = numberTree <$> growNode ctx idx0 ords0 0
  where
    k = if oNClass o > 0 then oNClass o else max 1 (if U.null ys then 1 else U.maximum ys + 1)
    d = if V.null xs then 0 else U.length (V.head xs)
    n = V.length xs
    idx0 = fromMaybe (U.enumFromN 0 n) (oIdx o)
    cols = V.generate d (\f -> U.generate n (\i -> xAt xs i f))
    ords0 = V.map (\col -> U.fromList (sortOn (col U.!) (U.toList idx0))) cols
    ctx = Ctx cols ys (fromMaybe (U.replicate n 1) (oW o)) k d o { oMinLeaf = max 1 (oMinLeaf o) }

-- 乱数を使わない木（mtry = 0）
growTreeP :: TreeOpt -> V.Vector Pt -> U.Vector Int -> Node
growTreeP o xs ys = fst (runR (growTree o { oMtry = 0 } xs ys) (Rng 1))

-- idx = このノードの点（重複あり）、ords = 特徴量ごとに値の順に並べた idx
growNode :: Ctx -> U.Vector Int -> V.Vector (U.Vector Int) -> Int -> R Node
growNode ctx idx ords depth = do
  let o = cO ctx
      counts = U.accumulate (+) (U.replicate (cK ctx) 0) (U.map (\i -> (cY ctx U.! i, cW ctx U.! i)) idx)
      node = nilNode { nLeaf = True, nCounts = counts, nN = U.length idx, nImp = impurityOf (oCrit o) counts
                     , nPred = argMaxU counts, nDepth = depth, nDec = 0 }
      kinds = U.length (U.filter (> 0) counts)
  if depth >= oMaxDepth o || U.length idx < 2 * oMinLeaf o || kinds <= 1 then pure node else do
    mb <- bestSplit ctx ords counts
    case mb of
      Nothing -> pure node
      Just (f, thr, dec) -> do
        let col = cCols ctx V.! f
            goL i = col U.! i <= thr
            (l, r) = U.partition goL idx
        lt <- growNode ctx l (V.map (U.filter goL) ords) (depth + 1)
        rt <- growNode ctx r (V.map (U.filter (not . goL)) ords) (depth + 1)
        pure node { nLeaf = False, nFeat = f, nThr = thr, nDec = dec, nLeft = lt, nRight = rt }

-- ★ いちばんよい分割：特徴量ごとに点を並べ、左から 1 つずつ動かしながら左右の個数を更新する
bestSplit :: Ctx -> V.Vector (U.Vector Int) -> U.Vector Double -> R (Maybe (Int, Double, Double))
bestSplit ctx ords counts = do
  let d = cD ctx
      mtry = oMtry (cO ctx)
  feats <- if mtry > 0 && mtry < d then U.toList <$> shuffleU (U.enumFromN 0 d) else pure [0 .. d - 1]   -- ランダムフォレスト：前の mtry 個だけを見る
  let first = if mtry > 0 then min mtry d else d
      best = splitOver ctx ords counts (take first feats)
  pure (if isNothing best && first < d then splitOver ctx ords counts (drop first feats) else best)   -- 切れる特徴量が 1 つもないときだけ残りを見る

splitOver :: Ctx -> V.Vector (U.Vector Int) -> U.Vector Double -> [Int] -> Maybe (Int, Double, Double)
splitOver ctx ords counts = foldl' feat Nothing
  where
    o = cO ctx
    crit = oCrit o
    minLeaf = oMinLeaf o
    w = sumU counts
    pImp = impurityOf crit counts
    feat best f =
      let ord = ords V.! f
          col = cCols ctx V.! f
          len = U.length ord
          go !k !lc !lw !b
            | k >= len - 1 = b
            | otherwise =
                let i = U.unsafeIndex ord k
                    wi = U.unsafeIndex (cW ctx) i
                    yi = U.unsafeIndex (cY ctx) i
                    lc' = U.unsafeUpd lc [(yi, U.unsafeIndex lc yi + wi)]
                    lw' = lw + wi
                    v0 = U.unsafeIndex col i
                    v1 = U.unsafeIndex col (U.unsafeIndex ord (k + 1))
                in if not (v1 > v0) || k + 1 < minLeaf || len - k - 1 < minLeaf then go (k + 1) lc' lw' b else
                   let rc = U.zipWith (-) counts lc'
                       dec = w * pImp - lw' * impurityOf crit lc' - (w - lw') * impurityOf crit rc
                       b' = case b of
                              Just (_, _, bd) | not (dec > bd + 1e-12) -> b
                              _ -> Just (f, (v0 + v1) / 2, dec)
                   in go (k + 1) lc' lw' b'
      in go 0 (U.replicate (cK ctx) 0) 0 best

-- 行きがけ順に番号をつけ直す（p5.js 版の node.id と同じ順）
numberTree :: Node -> Node
numberTree t0 = fst (go 0 t0)
  where
    go !n t | nLeaf t = (t { nId = n }, n + 1)
            | otherwise = let (l, n1) = go (n + 1) (nLeft t)
                              (r, n2) = go n1 (nRight t)
                          in l `seq` r `seq` (t { nId = n, nLeft = l, nRight = r }, n2)

-- 見せる深さ dLim、剪定の α = aLim で「葉として扱う」か
isCut :: Int -> Double -> Node -> Bool
isCut dLim aLim t = nLeaf t || nDepth t >= dLim || aLim >= nCut t

noD :: Int                                    -- 深さの制限なし
noD = maxBound

-- 根から葉までたどる。dLim = 見せる深さ、aLim = 剪定の α（制限なし = noD, negInf）
leafOfL :: Int -> Double -> Node -> Pt -> Node
leafOfL dLim aLim node x
  | isCut dLim aLim node = node
  | otherwise = leafOfL dLim aLim (if x U.! nFeat node <= nThr node then nLeft node else nRight node) x

leafOf :: Node -> Pt -> Node
leafOf = leafOfL noD negInf

treePredict :: Node -> Pt -> Int
treePredict t x = nPred (leafOf t x)

treePredictL :: Int -> Double -> Node -> Pt -> Int
treePredictL dl al t x = nPred (leafOfL dl al t x)

-- 見えているノードを全部（行きがけ順）
treeNodesL :: Int -> Double -> Node -> [Node]
treeNodesL dl al t = t : if isCut dl al t then [] else treeNodesL dl al (nLeft t) ++ treeNodesL dl al (nRight t)

treeNodes :: Node -> [Node]
treeNodes = treeNodesL noD negInf

countLeavesL :: Int -> Double -> Node -> Int
countLeavesL dl al t = length (filter (isCut dl al) (treeNodesL dl al t))

countLeaves :: Node -> Int
countLeaves = countLeavesL noD negInf

treeDepthL :: Int -> Double -> Node -> Int
treeDepthL dl al t = maxOr 0 (map nDepth (treeNodesL dl al t))

treeDepth :: Node -> Int
treeDepth = treeDepthL noD negInf

accuracyOf :: (Pt -> Int) -> V.Vector Pt -> U.Vector Int -> Double   -- fn(x) → クラス
accuracyOf fn xs ys = let n = V.length xs
                          ok = length (filter (\i -> fn (xs V.! i) == ys U.! i) [0 .. n - 1])
                      in if n > 0 then fromIntegral ok / fromIntegral n else 0

accuracyIdx :: (Pt -> Int) -> V.Vector Pt -> U.Vector Int -> [Int] -> Double   -- idx の点だけ
accuracyIdx fn xs ys ids = let ok = length (filter (\i -> fn (xs V.! i) == ys U.! i) ids)
                           in if null ids then 0 else fromIntegral ok / fromIntegral (length ids)

-- ---- 合成データ（[0, 1]² の 2 次元、種つき）。kind = 0 かたまり / 1 斜め / 2 円 / 3 XOR / 4 箱 / 5 しま
shapes :: [String]
shapes = ["blobs", "diagonal", "circle", "XOR", "box", "stripes"]

shapeName :: Int -> String
shapeName k = headOr "?" (drop k shapes)

sq2 :: Double -> Double
sq2 a = a * a

trueLabel :: Int -> Double -> Double -> Int
trueLabel kind u v = case kind of
  0 -> if sq2 (u - 0.32) + sq2 (v - 0.36) < sq2 (u - 0.68) + sq2 (v - 0.64) + 0.03 * sin (9 * u) then 0 else 1
  1 -> if u + 0.8 * v > 0.9 then 1 else 0
  2 -> if sq2 (u - 0.5) + sq2 (v - 0.5) < 0.09 then 1 else 0
  3 -> if (u > 0.5) /= (v > 0.5) then 1 else 0
  4 -> if u > 0.25 && u < 0.7 && v > 0.3 && v < 0.8 then 1 else 0
  _ -> if sin (10 * u + 3 * v) > 0 then 1 else 0

-- n 個、flip = ラベルをわざと入れかえる割合（ノイズ）
makeData2D :: Int -> Int -> Double -> Int -> R DataSet
makeData2D kind n flp seed = do
  seedRng seed
  ps <- mapM (\_ -> do
          u0 <- rnd
          v0 <- rnd
          let u = 0.02 + 0.96 * u0
              v = 0.02 + 0.96 * v0
              c = trueLabel kind u v
          r <- rnd
          pure (U.fromList [u, v], if r < flp then 1 - c else c)) [1 .. n]
  pure (DataSet (V.fromList (map fst ps)) (U.fromList (map snd ps)))

-- ---- 描く道具：クラスの色、領域の絵、点、数の書き方
type RGB = (Double, Double, Double)

clsCol :: Int -> RGB                          -- ★ クラス A・B・C の色
clsCol c = case c of
  0 -> (245, 140, 75)
  1 -> (85, 175, 240)
  2 -> (175, 225, 95)
  _ -> (200, 200, 200)

bgCol :: RGB                                  -- 背景（森の色）
bgCol = (20, 30, 26)

colOf :: RGB -> Color
colOf (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) 255

colA :: Int -> RGB -> Color
colA a (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) a

rnd8 :: Double -> Int
rnd8 v = max 0 (min 255 (round (fin 0 v)))

clsC :: Int -> Color
clsC = colOf . clsCol

-- クラスの割合 → 背景と混ぜた色（k = 色の濃さ）
mixCol :: U.Vector Double -> Double -> RGB
mixCol probs k =
  let s0 = sumU probs
      s = if s0 == 0 then 1 else s0
      add (r, g, b) (j, p) = let (cr, cg, cb) = clsCol j
                                 (br, bg, bb) = bgCol
                             in (r + (p / s) * (cr - br) * k, g + (p / s) * (cg - bg) * k, b + (p / s) * (cb - bb) * k)
  in foldl' add bgCol (zip [0 ..] (U.toList probs))

-- 幅 iw × 高さ ih の画像：全部の画素に fn u v → (r, g, b) を塗る（u は右向き、v は上向き、どちらも 0〜1）
paintImage :: Int -> Int -> (Double -> Double -> RGB) -> Picture
paintImage iw ih fn = pixelImage iw ih 1 (\px py ->
  let (r, g, b) = fn ((fromIntegral px + 0.5) / fromIntegral iw) (1 - (fromIntegral py + 0.5) / fromIntegral ih)
  in (w8 r, w8 g, w8 b))
  where w8 v = fromIntegral (rnd8 v) :: Word8

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（画面の道具の座標で）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

data Box = Box { bxX :: !Float, bxY :: !Float, bxW :: !Float, bxH :: !Float }

toPx :: Box -> Double -> Double -> (Float, Float)   -- [0,1]² → 画面
toPx bx u v = (bxX bx + f2 u * bxW bx, bxY bx + (1 - f2 v) * bxH bx)

fromPx :: Box -> (Float, Float) -> (Double, Double)
fromPx bx (px, py) = (realToFrac ((px - bxX bx) / bxW bx), realToFrac (1 - (py - bxY bx) / bxH bx))

-- 点を描く（直径 r、ws = 点ごとの大きさの倍率）
drawPoints :: Box -> V.Vector Pt -> U.Vector Int -> Float -> Maybe (U.Vector Double) -> Picture
drawPoints bx xs ys r ws = Pictures
  [ Pictures [Color (grayC 15) (circleAt px py (d + 2)), Color (clsC (ys U.! i)) (circleAt px py d)]
  | i <- [0 .. V.length xs - 1]
  , let (px, py) = toPx bx (xAt xs i 0) (xAt xs i 1)
        d = r * maybe 1 (\w -> f2 (w U.! i)) ws ]

-- 数を短く書く（toFixed(d) のあと余分な 0 を消す。p5.js 版の fmtNum）
-- toFixed と同じく、double のちょうどの値で丸め、ちょうど半分なら絶対値の大きいほうへ
fmtNum :: Int -> Double -> String
fmtNum d v
  | isNaN v || isInfinite v = "?"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = stripZeros (if d > 0 then ip ++ "." ++ fp else ip)
                in if n == 0 || v > 0 then body else '-' : body
  where stripZeros s | '.' `elem` s = let t = reverse (dropWhile (== '0') (reverse s))
                                      in if lastOr ' ' t == '.' then init t else t
                     | otherwise = s

-- 件数 [5|7] の形
countsStr :: U.Vector Double -> String
countsStr c = "[" ++ intercalate "|" (map (fmtNum 2) (U.toList c)) ++ "]"

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

-- 段落をいくつか上から順に（左上 = (x, y)、段落の間は gap）
sayParas :: [String] -> Float -> Float -> Float -> Float -> Float -> Color -> Picture
sayParas ps x y maxW size gap c = Pictures (go y ps)
  where go _ [] = []
        go yy (p : rest) = let n = length (wrapText maxW size p)
                           in sayWrap p x yy maxW size c : go (yy + fromIntegral n * size * 1.3 + gap) rest

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
jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e9) 1e9)

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
