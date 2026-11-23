-- ============================================
-- 62-00 20 の質問 ─ 心の中で動物を 1 つ決めて、コンピュータの「はい/いいえ」の質問に答えよう
-- コンピュータは、残っている候補を「なるべく半分ずつ」に分ける質問を選ぶ → 質問を重ねた「決定木」ができる
--   N 個を見分けるには、どう質問しても最低 log₂N 回（20 種類なら 4.32 回、いちばん長い道は 5 回以上）
--   悪い質問（1 匹だけ当てにいく）だと平均の回数がふえる。B で「よい質問 ⇔ 悪い質問」の木を比べよう
--   （シリーズ 10 の 09 も「決定木」：並べかえの比較の木で n log n 回の下限。こちらはデータから「学習する」決定木）
-- 操作：→ / Y はい / ← / N いいえ / スペース コンピュータが 1 匹選んで自動で答える / B よい ⇔ 悪い質問 / R 最初から
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

-- 質問（右の枠に出す長い文と、木に書く短い文）
questions, qShort :: [String]
questions = ["Is it a mammal?", "Does it fly?", "Does it live in water?", "Does it have 4 legs?", "Is it bigger than a person?", "Does it eat meat or insects?"
            , "Stripes or spots?", "Is it kept as a pet?", "Does it lay eggs?", "Is it active at night?", "Long neck or nose?"]
qShort = ["mammal?", "flies?", "water?", "4 legs?", "big?", "meat?", "pattern?", "pet?", "eggs?", "night?", "long neck?"]

-- ★ 自分で動物と答えを足してみよう（1 = はい、0 = いいえ。同じ行が 2 つあると見分けられない）
animals :: [(String, [Int])]
animals =
  [ ("Dog", [1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0]), ("Cat", [1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0])
  , ("Horse", [1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0]), ("Zebra", [1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0])
  , ("Elephant", [1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1]), ("Giraffe", [1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1])
  , ("Tiger", [1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0]), ("Lion", [1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0])
  , ("Bat", [1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0]), ("Whale", [1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0])
  , ("Dolphin", [1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1]), ("Hamster", [1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0])
  , ("Penguin", [0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0]), ("Eagle", [0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0])
  , ("Owl", [0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0]), ("Parakeet", [0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0])
  , ("Goldfish", [0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0]), ("Frog", [0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0])
  , ("Snake", [0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0]), ("Turtle", [0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0]) ]

nAnimals :: Int
nAnimals = length animals

animalName :: Int -> String
animalName i = maybe "?" fst (lookupAt i animals)

answerOf :: Int -> Int -> Int                -- 動物 i の質問 q の答え
answerOf i q = maybe 0 (\(_, a) -> headOr 0 (drop q a)) (lookupAt i animals)

lookupAt :: Int -> [a] -> Maybe a
lookupAt i xs = if i < 0 then Nothing else case drop i xs of
  (x : _) -> Just x
  [] -> Nothing

-- 質問の木のノード
data QNode = QNode
  { qId :: !Int, qDepth :: !Int, qCands :: [Int], qLeaf :: !Bool
  , qQ :: !Int, qLeft :: QNode, qRight :: QNode }   -- 左 = はい、右 = いいえ

data World = World
  { wGood   :: !Bool          -- よい質問の木か
  , wRoot   :: QNode
  , wCur    :: QNode          -- いまいるノード
  , wPath   :: [Int]          -- たどったノードの番号
  , wSecret :: !Int           -- コンピュータが選んだ動物（−1 = なし）
  , wAutoT  :: !Float
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "62-00 Twenty questions" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = rebuild (World True leaf0 leaf0 [] (-1) 0 [])
  where leaf0 = QNode 0 0 [] True 0 leaf0 leaf0

rebuild :: World -> World
rebuild w = restart w { wRoot = fst (buildQ [0 .. nAnimals - 1] 0 (wGood w) 0) }

restart :: World -> World
restart w = w { wCur = wRoot w, wPath = [qId (wRoot w)], wSecret = -1 }

-- ★ 質問の木を作る：候補 cands を、yes と no に分ける質問を選ぶ
-- よい質問 = 小さいほうの側がいちばん大きい（= 半分に近い = 情報利得が最大）、悪い質問 = 小さいほうがいちばん小さい（でも 0 ではない）
-- 返り値の 2 つ目 = 次に使う番号
buildQ :: [Int] -> Int -> Bool -> Int -> (QNode, Int)
buildQ cands depth good nid
  | length cands <= 1 || best < 0 = (node, nid + 1)            -- best < 0：見分けられない（同じ行がある）
  | otherwise =
      let (l, n1) = buildQ (filter (\i -> answerOf i best == 1) cands) (depth + 1) good (nid + 1)   -- 左 = はい
          (r, n2) = buildQ (filter (\i -> answerOf i best == 0) cands) (depth + 1) good n1           -- 右 = いいえ
      in (node { qLeaf = False, qQ = best, qLeft = l, qRight = r }, n2)
  where
    node = QNode nid depth cands True 0 node node
    k = length cands
    score q = let yes = length (filter (\i -> answerOf i q == 1) cands) in min yes (k - yes)
    pick (b, bs) q = let m = score q
                     in if m == 0 then (b, bs)                 -- 全員同じ答え → 何もわからない質問
                        else if (if good then m > bs else m < bs) then (q, m) else (b, bs)
    best = fst (foldl' pick (-1, if good then -1 else maxBound) [0 .. length questions - 1])

leavesOfQ :: QNode -> [QNode]
leavesOfQ t = if qLeaf t then [t] else leavesOfQ (qLeft t) ++ leavesOfQ (qRight t)

allNodes :: QNode -> [QNode]
allNodes t = t : if qLeaf t then [] else allNodes (qLeft t) ++ allNodes (qRight t)

avgDepth :: QNode -> Double
avgDepth t = let ls = leavesOfQ t in fromIntegral (sum (map qDepth ls)) / fromIntegral (max 1 (length ls))

answer :: Bool -> World -> World
answer yes w
  | qLeaf (wCur w) = w
  | otherwise = let c = if yes then qLeft (wCur w) else qRight (wCur w)
                in w { wCur = c, wPath = wPath w ++ [qId c] }

step :: Env -> Float -> World -> IO World
step env _ w
  | wSecret w >= 0 && not (qLeaf (wCur w)) && envTime env - wAutoT w > 0.8 =
      pure (answer (answerOf (wSecret w) (qQ (wCur w)) == 1) w { wAutoT = envTime env })
  | otherwise = pure w

hotC :: Color
hotC = rgbC 255 230 120

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 木（左から右へ。行 = 葉の順番、列 = 深さ）
  [ Pictures (map edge (filter (not . qLeaf) nodes))
  , Pictures (map node nodes)
  -- ---- いまの質問
  , say ("Twenty questions   " ++ (if wGood w then "good questions (split in half)" else "bad questions (guess one animal at a time)")) 40 40 20 (grayC 240)
  , say "-> / Y yes   <- / N no   Space computer picks and answers   B good <-> bad   R restart" 40 66 15 (grayC 170)
  , Color (rgbC 35 52 44) (rectTL px 110 pw 120)
  , if qLeaf cur
      then say (if length (qCands cur) == 1 then "Got it! Is it a " ++ animalName (headOr 0 (qCands cur)) ++ "?" else "Can't tell them apart...") (px + 20) 160 22 (rgbC 255 235 150)
      else say ("Q" ++ show (length (wPath w)) ++ ". " ++ headOr "?" (drop (qQ cur) questions)) (px + 20) 160 22 (rgbC 255 235 150)
  , say (if qLeaf cur then show (length (wPath w) - 1) ++ " questions to find it (R to play again)"
         else "yes -> / no <-" ++ (if wSecret w >= 0 then "  (computer answers as " ++ animalName (wSecret w) ++ ")" else "")) (px + 20) 205 15 (grayC 190)
  -- 残りの候補
  , say ("Candidates left " ++ show k ++ "  ->  at least log2 " ++ show k ++ " = " ++ fmtNum 2 h ++ " more") px 270 16 (grayC 220)
  , Pictures [ say (animalName i) (px + fromIntegral (i `mod` 4) * (pw / 4)) (300 + fromIntegral (i `div` 4) * 24) 15
                   (if i `elem` qCands cur then rgbC 175 225 95 else rgbC 90 105 95) | i <- [0 .. nAnimals - 1] ]
  , if qLeaf cur then Blank
      else let yes = length (filter (\i -> answerOf i (qQ cur) == 1) (qCands cur))
           in sayWrap ("This question: yes " ++ show yes ++ " / no " ++ show (k - yes) ++ " -> information gain "
                       ++ fmtNum 3 (entropyP (fromIntegral yes / fromIntegral (max 1 k))) ++ " bits (1 if exactly half)") px (ya - 16) pw 15 (grayC 200)
  , say ("This tree: average " ++ fmtNum 2 gd ++ " questions, longest path " ++ show maxD) px (ya + 60) 16 (grayC 220)
  , say ("Lower bound: log2 20 = " ++ fmtNum 2 (log2 20) ++ " (avg), ceil = " ++ show (ceiling (log2 20) :: Int) ++ " (longest)") px (ya + 86) 16 (grayC 220)
  , sayWrap "One yes/no answer gives at most 1 bit. Telling 20 apart needs log2 20 bits. Good questions always get answers close to 1 bit."
      px (ya + 104) pw 14 (grayC 160)
  , drawSelfTest env (wTest w) ]
  where
    root = wRoot w
    cur = wCur w
    ls = leavesOfQ root
    nodes = allNodes root
    maxD = maxOr 0 (map qDepth ls)
    tbx = 40
    tby = 110
    tbw = envW env * 0.56
    tbh = envH env - 170
    rowH = tbh / fromIntegral (max 1 (length ls))
    colW = (tbw - 110) / fromIntegral (max 1 maxD)
    leafPos = IM.fromList (zip (map qId ls) [0 :: Int ..])
    pos t | qLeaf t = fromIntegral (IM.findWithDefault 0 (qId t) leafPos)
          | otherwise = (pos (qLeft t) + pos (qRight t)) / 2 :: Float
    pP t = (tbx + fromIntegral (qDepth t) * colW, tby + (pos t + 0.5) * rowH)
    onPath = S.fromList (wPath w)
    edge t = Pictures
      [ let (ax, ay) = pP t
            (bx, by) = pP c
            hot = S.member (qId c) onPath
            col | hot = hotC
                | isL = rgbaC 120 190 120 150
                | otherwise = rgbaC 200 120 100 150
        in Color col (thickPath (if hot then 3.5 else 1.5) [(ax, ay), (ax + colW * 0.5, by), (bx, by)])
      | (c, isL) <- [(qLeft t, True), (qRight t, False)] ]
    node t =
      let (x, yy) = pP t
          hot = S.member (qId t) onPath
      in if qLeaf t
           then say (intercalate ", " (map animalName (qCands t))) (x + 6) (yy + 5) (min 14 (rowH * 0.8)) (if hot then hotC else grayC 220)
           else Pictures [ Color (if hot then hotC else rgbC 60 90 75) (circleAt x yy 9)
                         , if colW > 60 && qDepth t < 4 then say (headOr "?" (drop (qQ t) qShort)) (x + 6) (yy - 6) 14 (if hot then grayC 255 else grayC 170) else Blank ]
    px = tbx + tbw + 30
    pw = envW env - px - 30
    k = length (qCands cur)
    h = log2 (fromIntegral (max 1 k))
    ya = 300 + 5 * 24 + 20
    gd = avgDepth root

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (answer True w { wSecret = -1 })
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (answer False w { wSecret = -1 })
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    s <- randomRIO (0, nAnimals - 1)
    pure (restart w) { wSecret = s, wAutoT = envTime env }
  EventKey (Char c) Down _ _
    | c `elem` "yY" -> pure (answer True w { wSecret = -1 })
    | c `elem` "nN" -> pure (answer False w { wSecret = -1 })
    | c `elem` "bB" -> pure (rebuild w { wGood = not (wGood w) })
    | c `elem` "rR" -> pure (restart w)
  _ -> pure w

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("動物は 20 種類、答えの行はぜんぶちがう（同じ行だと見分けられない）", nAnimals == 20 && length (nub rows) == 20 && all (\(_, a) -> length a == length questions) animals)
  , ("よい木も悪い木も、葉はちょうど 20 枚で 1 匹ずつ", all (\t -> length (leavesOfQ t) == 20 && all (\l -> length (qCands l) == 1) (leavesOfQ t)) [g, b])
  , ("どの動物も、自分の答えで木をたどると自分にたどり着く", all (\i -> reach g i == i && reach b i == i) [0 .. nAnimals - 1])
  , ("平均の質問数 " ++ fmtNum 2 dg ++ " ≥ log₂20 = 4.32、いちばん長い道 " ++ show mg ++ " ≥ 5", dg >= log2 20 - 1e-9 && mg >= 5)
  , ("クラフトの等式：葉ごとの 2^(−深さ) の合計 = 1（2 分木）", near (sum [2 ** negate (fromIntegral (qDepth l)) | l <- leavesOfQ g]) 1 1e-9)
  , ("よい質問の木の平均 " ++ fmtNum 2 dg ++ " < 悪い質問の木の平均 " ++ fmtNum 2 (avgDepth b), dg < avgDepth b)
  , ("情報利得：半分ずつ（10 / 10）なら 1 ビット、1 / 19 なら 0.29 ビット", near (entropyP 0.5) 1 1e-9 && near (entropyP (1 / 20)) 0.2864 1e-4) ]
  where
    rows = map snd animals
    g = fst (buildQ [0 .. nAnimals - 1] 0 True 0)
    b = fst (buildQ [0 .. nAnimals - 1] 0 False 0)
    reach t i = if qLeaf t then headOr (-1) (qCands t) else reach (if answerOf i (qQ t) == 1 then qLeft t else qRight t) i
    dg = avgDepth g
    mg = maxOr 0 (map qDepth (leavesOfQ g))

-- ---------- やってみよう ----------
-- 1. 心の中で動物を決めて答えよう。何回で当たった？ log₂20 = 4.32 と比べよう
-- 2. B で悪い質問の木にすると、いちばん長い道は何回？ なぜ長くなる？
-- 3. animals に動物を 1 匹足してみよう（同じ行になったら、自己テストが FAIL になる → 質問を 1 つ足す）
-- 4. 32 種類なら最低何回？ 1000 種類なら？（2 の何乗か）
-- 5. 01 では、「半分に近い」をエントロピーと情報利得で数えて、データのしきい値選びに使う

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
