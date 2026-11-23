-- ============================================
-- 62-03 ID3 とカテゴリの表 ─ 「テニスをする？」の 14 日分の表から、情報利得で質問を選んで木を作る（ID3、クインラン）
-- 根：4 つの属性（天気・気温・湿度・風）それぞれで表を分けたときの情報利得を計算 → いちばん大きい属性で分ける → 枝ごとにくり返す
--   カテゴリの属性は「値の数だけ枝」が出る（しきい値ではない）。純粋になった枝は葉
--   利得比 = 利得 / 分割情報（値の分かれ方のエントロピー）。値の種類が多い属性（たとえば「日付」）をひいきしないための工夫（C4.5）
-- 操作：クリック 表のマスの値を変える / G 情報利得 ⇔ 利得比 / R もとの表に
-- 画面の文字：Outlook = 天気（Sunny 晴れ / Overcast くもり / Rain 雨）、Temp = 気温、Humidity = 湿度、Wind = 風、Play = テニスをする？
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

attrs :: [String]
attrs = ["Outlook", "Temp", "Humidity", "Wind"]

vals :: [[String]]
vals = [["Sunny", "Overcast", "Rain"], ["Hot", "Mild", "Cool"], ["High", "Normal"], ["Weak", "Strong"]]

playS :: [String]
playS = ["Yes", "No"]

-- [天気, 気温, 湿度, 風, テニス]（値は vals の番号、テニスは 0 = する、1 = しない）
tennis :: [[Int]]
tennis =
  [ [0, 0, 0, 0, 1], [0, 0, 0, 1, 1], [1, 0, 0, 0, 0], [2, 1, 0, 0, 0], [2, 2, 1, 0, 0], [2, 2, 1, 1, 1], [1, 2, 1, 1, 0]
  , [0, 1, 0, 0, 1], [0, 2, 1, 0, 0], [2, 1, 1, 0, 0], [0, 1, 1, 1, 0], [1, 1, 0, 1, 0], [1, 0, 1, 0, 0], [2, 1, 0, 1, 1] ]

cellW :: Float
cellW = 90                 -- ★ 表のマスの幅

tblX, tblY :: Float
tblX = 40
tblY = 120

nVals :: Int -> Int
nVals a = length (atL a vals)

atL :: Int -> [[a]] -> [a]
atL i xs = headOr [] (drop i xs)

valName :: Int -> Int -> String
valName a v = headOr "?" (drop v (atL a vals))

col :: Int -> [Int] -> Int                  -- 行 r の a 列目
col a r = headOr 0 (drop a r)

-- 属性ごとの数
data AStat = AStat { asA :: !Int, asGain :: !Double, asSI :: !Double, asRatio :: !Double }

-- ID3 の木のノード
data INode = INode
  { iRows :: [[Int]], iCounts :: !(U.Vector Double), iPred :: !Int, iDepth :: !Int, iLeaf :: !Bool
  , iAttr :: !Int, iGain :: !Double, iRatio :: !Double, iKids :: [INode] }

data World = World
  { wRows  :: [[Int]]
  , wRatio :: !Bool        -- 利得比で選ぶか
  , wRoot  :: INode
  , wStats :: [AStat]      -- 根での属性ごとの利得
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "62-03 ID3 and the tennis table" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = resetTable (World [] False (buildID3 [] [] False 0) [] [])

resetTable :: World -> World
resetTable w = rebuild w { wRows = tennis }

rebuild :: World -> World
rebuild w = w { wStats = attrStats (wRows w) [0 .. 3], wRoot = buildID3 (wRows w) [0 .. 3] (wRatio w) 0 }

countsOf :: [[Int]] -> U.Vector Double
countsOf rs = U.accum (+) (U.replicate 2 0) [(col 4 r, 1) | r <- rs]

-- ★ 属性 a で分けたときの情報利得と利得比
attrGain :: [[Int]] -> Int -> AStat
attrGain rs a =
  let h = entropyOf (countsOf rs)
      subs = [filter (\r -> col a r == v) rs | v <- [0 .. nVals a - 1]]
      remd = foldl' (\acc sub -> if null sub then acc else acc + (fromIntegral (length sub) / fromIntegral (length rs)) * entropyOf (countsOf sub)) 0 subs
      gain = h - remd
      si = entropyOf (U.fromList (map (fromIntegral . length) subs))   -- 分割情報 = 「どの枝に行くか」のエントロピー
  in AStat a gain si (if si > 1e-12 then gain / si else 0)

attrStats :: [[Int]] -> [Int] -> [AStat]
attrStats rs = map (attrGain rs)

-- ★ ID3：利得（か利得比）がいちばん大きい属性で分け、値ごとの枝でくり返す
buildID3 :: [[Int]] -> [Int] -> Bool -> Int -> INode
buildID3 rs as ratio depth
  | null rs || c U.! 0 == 0 || c U.! 1 == 0 || null as = node
  | otherwise =
      let st = attrStats rs as
          key s = if ratio then asRatio s else asGain s
          best = foldl' (\m s -> if key s > key m + 1e-12 then s else m) (headOr (AStat 0 0 0 0) st) (drop 1 st)
          ba = asA best
          kids = [ let k = buildID3 (filter (\r -> col ba r == v) rs) (filter (/= ba) as) ratio (depth + 1)
                   in if null (iRows k) then k { iPred = iPred node } else k        -- 行が 0 の枝は親の多数決
                 | v <- [0 .. nVals ba - 1] ]
      in node { iLeaf = False, iAttr = ba, iGain = asGain best, iRatio = asRatio best, iKids = kids }
  where
    c = countsOf rs
    node = INode rs c (if c U.! 1 > c U.! 0 then 1 else 0) depth True 0 0 0 []

id3Predict :: INode -> [Int] -> Int
id3Predict t r
  | iLeaf t = iPred t
  | otherwise = case drop (col (iAttr t) r) (iKids t) of
      (k : _) -> id3Predict k r
      [] -> iPred t

countID3Leaves :: INode -> Int
countID3Leaves t = if iLeaf t then 1 else sum (map countID3Leaves (iKids t))

cellH :: Env -> Float
cellH env = min 32 ((envH env - 190) / 15)

colWs :: [Float]
colWs = [36, cellW, cellW, cellW, cellW, cellW]

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 表
  [ Pictures [ Pictures [Color (rgbC 60 90 75) (rectTL x tblY (cw - 2) (ch - 2)), sayM hd (x + cw / 2) (tblY + ch / 2) 14 (grayC 240)]
             | (hd, cw, x) <- zip3 heads colWs xs0 ]
  , Pictures (concat (zipWith3 rowPic [0 :: Int ..] (wRows w) (tennis ++ repeat [])))
  , say ("Play " ++ show (cI 0) ++ " days / not " ++ show (cI 1) ++ " days   H = " ++ fmtNum 3 (entropyOf c) ++ " bits") tblX (tblY + 15.8 * ch) 15 (grayC 220)
  , say ("The tree predicts " ++ show acc ++ " / " ++ show (length (wRows w)) ++ " rows correctly") tblX (tblY + 15.8 * ch + 22) 15 (grayC 220)
  -- ---- 根での利得の棒グラフ
  , say ("Compare at the root: " ++ (if wRatio w then "gain ratio = gain / split info" else "information gain (bits)")) bx (gy - 10) 16 (grayC 240)
  , Pictures (zipWith bar [0 :: Int ..] (wStats w))
  , sayWrap ("If \"day\" (a different value on all 14 days) were an attribute: gain " ++ fmtNum 3 dayG
             ++ " (the largest! but useless); with gain ratio it drops to " ++ fmtNum 3 dayR) bx (gy + 128) bw 14 (grayC 150)
  -- ---- ID3 の木
  , drawID3 (wRatio w) (wRoot w) bx (gy + 200) bw (envH env - gy - 260)
  , say "ID3 and a categorical table   \"Play tennis?\" over 14 days" 40 40 20 (grayC 240)
  , say ("click change a cell (yellow frame = changed)   G gain <-> gain ratio (now " ++ (if wRatio w then "ratio" else "gain") ++ ")   R original table") 40 66 15 (grayC 170)
  , drawSelfTest env (wTest w) ]
  where
    ch = cellH env
    heads = "#" : attrs ++ ["Play"]
    xs0 = scanl (+) tblX colWs
    rowPic i r orig =
      let yy = tblY + fromIntegral (i + 1) * ch
      in sayM (show (i + 1)) (tblX + 18) (yy + ch / 2) 14 (grayC 170)
         : [ Pictures
               [ Color (if a == 4 then (if col 4 r == 0 then rgbC 60 110 60 else rgbC 110 60 55) else rgbC 32 46 40) (rectTL x yy (cellW - 2) (ch - 2))
               , if col a r /= col a orig then Color (rgbC 255 240 150) (frameTL 2 x yy (cellW - 2) (ch - 2)) else Blank
               , sayM (if a == 4 then headOr "?" (drop (col 4 r) playS) else valName a (col a r)) (x + cellW / 2) (yy + ch / 2) 14 (grayC 235) ]
           | (a, x) <- zip [0 .. 4] (drop 1 xs0) ]
    c = countsOf (wRows w)
    cI k = round (c U.! k) :: Int
    acc = length [() | r <- wRows w, id3Predict (wRoot w) r == col 4 r]
    bx = tblX + 36 + 5 * cellW + 50
    bw = envW env - bx - 40
    gy = 120
    key s = if wRatio w then asRatio s else asGain s
    best = argMaxU (U.fromList (map key (wStats w)))
    bar k s =
      let yy = gy + 10 + fromIntegral k * 30
          len = (bw - 360) * f2 (clampv (key s / 0.3) 0 1.1)
      in Pictures [ sayR (headOr "?" (drop (asA s) attrs)) (bx + 50) (yy + 16) 14 (grayC 200)
                  , Color (if k == best then rgbC 175 225 95 else rgbC 85 175 240) (rectTL (bx + 60) yy len 20)
                  , say ("gain " ++ fmtNum 4 (asGain s) ++ "  split " ++ fmtNum 3 (asSI s) ++ "  ratio " ++ fmtNum 4 (asRatio s)) (bx + 70 + len) (yy + 16) 14 (grayC 230) ]
    dayG = entropyOf c
    dayR = dayG / log2 (fromIntegral (length (wRows w)))

drawID3 :: Bool -> INode -> Float -> Float -> Float -> Float -> Picture
drawID3 ratio root x0 y0 w h = Pictures (walk proot ++ map box nodes)
  where
    -- 葉に左から番号をふり、親は最初と最後の子の真ん中
    place k t
      | iLeaf t = (k + 1, fromIntegral k, PNode t (fromIntegral k) [])
      | otherwise = let (k', ps) = foldl' (\(kk, acc) kid -> let (k2, _, p) = place kk kid in (k2, acc ++ [p])) (k, []) (iKids t)
                        firstP = maybe 0 pPos (headOrM ps)
                        lastP = maybe 0 pPos (headOrM (reverse ps))
                        p = (firstP + lastP) / 2
                    in (k', p, PNode t p ps)
    (leaves, _, proot) = place (0 :: Int) root
    nodes = flatten proot
    flatten p = p : concatMap flatten (pKids p)
    maxD = maxOr 0 (map (iDepth . pNode) nodes)
    cw = w / fromIntegral (max 1 leaves)
    rh = min 110 (h / fromIntegral (maxD + 1))
    pp p = (x0 + (pPos p + 0.5) * cw, y0 + fromIntegral (iDepth (pNode p)) * rh + 20)
    walk p = concat [ [ Color (rgbC 120 140 125) (lineW 1.5 (ax, ay + 18) (bx', by - 18))
                      , sayM (valName (iAttr (pNode p)) v) ((ax + bx') / 2) ((ay + by) / 2) 14 (rgbC 255 240 150) ] ++ walk k
                    | (v, k) <- zip [0 ..] (pKids p), let (ax, ay) = pp p, let (bx', by) = pp k ]
    box p =
      let t = pNode p
          (x, yy) = pp p
          bw = min 118 (cw - 6)
          leaf = iLeaf t
      in Pictures
           [ Color (if leaf then (if iPred t == 0 then rgbC 40 80 45 else rgbC 80 40 38) else rgbC 30 44 38) (rectTL (x - bw / 2) (yy - 18) bw 36)
           , Color (if leaf then (if iPred t == 0 then rgbC 120 200 120 else rgbC 230 120 110) else rgbC 110 130 115) (frameTL 1.5 (x - bw / 2) (yy - 18) bw 36)
           , sayM (if leaf then headOr "?" (drop (iPred t) playS) ++ (if null (iRows t) then " (0 rows)" else "") else headOr "?" (drop (iAttr t) attrs) ++ "?") x (yy - 8) 14 (grayC 240)
           , sayM (countsStr (iCounts t) ++ (if leaf then "" else " " ++ fmtNum 3 (if ratio then iRatio t else iGain t))) x (yy + 9) 14 (grayC 200) ]

data PNode = PNode { pNode :: INode, pPos :: !Float, pKids :: [PNode] }

headOrM :: [a] -> Maybe a
headOrM xs = case xs of
  (x : _) -> Just x
  [] -> Nothing

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 env pos
        i = floor ((my - tblY) / cellH env) - 1 :: Int
        xs0 = scanl (+) (tblX + 36) (replicate 5 cellW)
        hit = [a | (a, x) <- zip [0 .. 4] xs0, mx >= x && mx < x + cellW]
    in if i < 0 || i >= length (wRows w) then w else case hit of
         (a : _) -> rebuild w { wRows = [ if j == i then [ if b == a then (v + 1) `mod` (if a == 4 then 2 else nVals a) else v | (b, v) <- zip [0 ..] r ] else r
                                        | (j, r) <- zip [0 ..] (wRows w) ] }
         [] -> w
  EventKey (Char c) Down _ _
    | c `elem` "gG" -> rebuild w { wRatio = not (wRatio w) }
    | c `elem` "rR" -> resetTable w
  _ -> w

-- ---- 自己テスト（いつも「もとの表」tennis で）
selfTest :: [(String, Bool)]
selfTest =
  [ ("表は 14 行、する 9 日 / しない 5 日、H = 0.940 ビット", length tennis == 14 && c U.! 0 == 9 && c U.! 1 == 5 && near (entropyOf c) 0.9403 1e-4)
  , ("根の情報利得：天気 0.2467、湿度 0.1518、風 0.0481、気温 0.0292（±0.001）", near (g 0) 0.2467 1e-3 && near (g 2) 0.1518 1e-3 && near (g 3) 0.0481 1e-3 && near (g 1) 0.0292 1e-3)
  , ("根でいちばん利得が大きいのは「天気」", argMaxU (U.fromList (map asGain st)) == 0)
  , ("利得比：天気 0.247 / 1.577 = 0.156、湿度 0.152 / 1 = 0.152（天気がまだ 1 位）", near (asSI (s 0)) 1.5774 1e-3 && near (asRatio (s 0)) 0.1564 1e-3 && near (asRatio (s 2)) 0.1518 1e-3 && argMaxU (U.fromList (map asRatio st)) == 0)
  , ("ID3 の木：天気 → くもりは全部「する」、晴れ → 湿度、雨 → 風", iAttr t == 0 && not (iLeaf t) && iLeaf (kid 1) && iPred (kid 1) == 0 && iCounts (kid 1) U.! 1 == 0 && not (iLeaf (kid 0)) && iAttr (kid 0) == 2 && not (iLeaf (kid 2)) && iAttr (kid 2) == 3)
  , ("ID3 の木は 14 行を全部当てる。葉は 5 枚", all (\r -> id3Predict t r == col 4 r) tennis && countID3Leaves t == 5)
  , ("どの属性でも利得 ≥ 0、「日付」なら利得 = H = 0.940 で利得比 = 0.940 / log₂14 = 0.247", all (\x -> asGain x >= -1e-12) st && near (entropyOf c / log2 14) 0.247 1e-3) ]
  where
    c = countsOf tennis
    st = attrStats tennis [0 .. 3]
    s k = headOr (AStat 0 0 0 0) (drop k st)
    g k = asGain (s k)
    t = buildID3 tennis [0 .. 3] False 0
    kid k = headOr t (drop k (iKids t))

-- ---------- やってみよう ----------
-- 1. 根の棒グラフで、天気の利得 0.247 がいちばん大きいことを確かめよう。手で計算もしてみよう
-- 2. 「くもり」の日を 1 日「しない」に変える（Play の列をクリック）。木はどう変わる？
-- 3. G で利得比にすると、根の属性は変わる？ 値が 3 つある天気と、2 つの湿度で比べてみよう
-- 4. 表の値を変えて、木の葉が 10 枚以上になる表を作れる？
-- 5. 04 からは、カテゴリではなく連続の値（しきい値で分ける）の木＝CART

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
