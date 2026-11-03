-- ============================================
-- 07 ビットベクトルと要約 ─ van Emde Boas 木への準備
-- 0〜u−1 の整数の集合を、u 個の 0/1 で表す。「x の次に大きい要素（SUCCESSOR）」を 3 つの方法で探して比べる
-- 黄 = 端から順に見る O(u) / 水色 = 上に OR の 2 分木を重ねる O(lg u) / ピンク = √u 個ずつの「かたまり」と要約 O(√u)
-- マス目クリック = 入れる・消す / スペース = ランダムな x の SUCCESSOR / 数字 + Enter = x を指定 / ←→ = u（16・64・256）/ R = 並べ直す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits ((.|.))
import Data.Char (isDigit)
import Data.List (find)
import qualified Data.Vector.Unboxed as U

startU :: Int
startU = 16           -- ★ 全体の大きさ u（4 の累乗にすると √u が整数）

density :: Float
density = 0.15        -- ★ 最初に 1 にする割合

speed :: Float
speed = 0.18          -- ★ 1 歩の秒数

-- 見た場所：c = かたまりのマス、s = 要約のマス
data Look = LookC !Int | LookS !Int deriving Eq

-- 3 つの方法の探索の記録
data Seqs = Seqs { sNaive :: ![Int], sTree :: ![(Int, Int)], sClus :: ![Look], sAns :: !(Maybe Int), sTreeOK :: !Bool }

data World = World
  { wU     :: !Int
  , wBits  :: !(U.Vector Int)
  , wQ     :: !(Maybe Int)
  , wSeqs  :: !(Maybe Seqs)
  , wSi    :: !Int
  , wTimer :: !Float
  , wTyped :: !String
  }

main :: IO ()
main = runSketch (sketch "13-07 Bit Vector and Summary" (World startU (U.replicate startU 0) Nothing Nothing 0 0 ""))
  { skBack  = hsb 190 35 9
  , skStart = \_ -> resetWorld (World startU (U.replicate startU 0) Nothing Nothing 0 0 "")
  , skStep  = \_ dt w -> pure (stepWorld dt w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

resetWorld :: World -> IO World
resetWorld w = do
  bs <- replicateM (wU w) (randomRIO (0, 1 :: Float))
  pure w { wBits = U.fromList [if b < density then 1 else 0 | b <- bs], wSeqs = Nothing, wQ = Nothing }

sqrtOf :: Int -> Int
sqrtOf u = round (sqrt (fromIntegral u :: Float))

high, low :: Int -> Int -> Int
high s x = x `div` s
low s x = x `mod` s

index :: Int -> Int -> Int -> Int
index s h l = h * s + l

summary :: Int -> U.Vector Int -> U.Vector Int
summary s bits = U.generate s (\h -> U.foldl' (.|.) 0 (U.slice (h * s) s bits))

-- OR の 2 分木（0 段目がビットベクトル、上に向かって半分ずつ）
treeLevels :: U.Vector Int -> [U.Vector Int]
treeLevels v
  | U.length v <= 1 = [v]
  | otherwise = v : treeLevels (U.generate (U.length v `div` 2) (\i -> (v U.! (2 * i)) .|. (v U.! (2 * i + 1))))

-- 3 つの方法で SUCCESSOR(x) を探し、見た場所を記録する
query :: Int -> World -> World
query x0 w = w { wQ = Just x, wSi = 0, wTimer = 0, wSeqs = Just (Seqs naive tree clus ans (u <= 64)) }
  where
    u = wU w
    s = sqrtOf u
    bits = wBits w
    on i = bits U.! i /= 0
    x = max 0 (min (u - 1) x0)
    -- ① 端から順に
    upTo = takeWhile (not . on) [x + 1 .. u - 1]
    hit = find on [x + 1 .. u - 1]
    naive = upTo ++ maybe [] pure hit
    ans = hit
    -- ② OR の 2 分木：上って、右の兄弟が 1 なら下る
    levels = treeLevels bits
    levelOf lv = maybe U.empty id (safeAt levels lv)
    tree = (0, x) : climb 0 x
    top = length levels - 1
    climb lv i
      | lv >= top = []
      | even i && levelOf lv U.! (i + 1) /= 0 = (lv, i + 1) : descend lv (i + 1)
      | otherwise = (lv + 1, i `div` 2) : climb (lv + 1) (i `div` 2)
    descend lv i
      | lv <= 0 = []
      | otherwise =
          let lv' = lv - 1
              i' = if levelOf lv' U.! (2 * i) /= 0 then 2 * i else 2 * i + 1
          in (lv', i') : descend lv' i'
    -- ③ かたまりと要約
    sm = summary s bits
    h = high s x
    untilOn f [] = []
    untilOn f (a : as) = a : if f a then [] else untilOn f as
    inOwn = untilOn on [index s h l | l <- [low s x + 1 .. s - 1]]
    foundOwn = not (null inOwn) && on (last inOwn)
    sums = untilOn (\g -> sm U.! g /= 0) [h + 1 .. s - 1]
    foundSum = not (null sums) && sm U.! last sums /= 0
    clus = map LookC inOwn ++
      (if foundOwn then [] else
         map LookS sums ++
         (if foundSum then map LookC (untilOn on [index s (last sums) l | l <- [0 .. s - 1]]) else []))

stepWorld :: Float -> World -> World
stepWorld dt w = case wSeqs w of
  Nothing -> w
  Just _ ->
    let t = wTimer w + dt
    in if t > speed then w { wTimer = 0, wSi = wSi w + 1 } else w { wTimer = t }

-- ---------- マス目の場所 ----------
-- 1 列のマス：(x, y, 幅, 番号)
rowCells :: Env -> Int -> [(Float, Float, Float, Int)]
rowCells env u =
  let (cw, x0, rowY) = rowGeom env u
  in [ (x0 + fromIntegral i * cw, rowY - cw / 2, cw, i) | i <- [0 .. u - 1] ]

rowGeom :: Env -> Int -> (Float, Float, Float)
rowGeom env u =
  let cw = min 52 ((envW env - 80) / fromIntegral u)
  in (cw, envW env / 2 - fromIntegral u * cw / 2, if u <= 64 then 330 else 170)

-- かたまりのマス目の寸法：(y0, マスの大きさ, x0)
gridGeom :: Env -> Int -> (Float, Float, Float)
gridGeom env u =
  let (_, _, rowY) = rowGeom env u
      s = fromIntegral (sqrtOf u)
      gy0 = rowY + 90
      gh = envH env - gy0 - 120
  in (gy0, minimum [48, gh / s, (envW env - 400) / (s + 2)], 150)

gridCells :: Env -> Int -> [(Float, Float, Float, Int)]
gridCells env u =
  let s = sqrtOf u
      (gy0, gc, gx0) = gridGeom env u
  in [ (gx0 + fromIntegral (l + 1) * gc, gy0 + fromIntegral h * gc, gc - 4, index s h l) | h <- [0 .. s - 1], l <- [0 .. s - 1] ]

cellAt :: Env -> Int -> Point -> Maybe Int
cellAt env u (mx, my) =
  case [ i | (x, y, c, i) <- rowCells env u ++ gridCells env u, mx > x, mx < x + c, my > y, my < y + c ] of
    (i : _) -> Just i
    [] -> Nothing

-- ---------- 描画 ----------
drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (treePics ++ rowPics ++ gridPics ++ resultPics ++ headPics)
  where
    u = wU w
    s = sqrtOf u
    bits = wBits w
    on i = bits U.! i /= 0
    sm = summary s bits
    h = envH env
    si = wSi w
    (cw, x0, rowY) = rowGeom env u
    levels = treeLevels bits
    seqs = wSeqs w
    tvis = maybe [] (take (si + 1) . sTree) seqs
    nvis = maybe [] (take (si + 1) . sNaive) seqs
    cvis = maybe [] (take (si + 1) . sClus) seqs
    -- 上：1 列のビットベクトル + OR の木
    nLevels = length levels
    ly :: Int -> Float
    ly lv | lv == 0 = rowY - cw / 2
          | otherwise = rowY - 50 - fromIntegral (lv - 1) * (140 / fromIntegral (max 1 (nLevels - 2)))
    treePics
      | u > 64 = []
      | otherwise = concat
          [ [ lineP env 1 (hsb 190 20 40) (cx, y) (x0 + (2 * fromIntegral i + 0.5) * (nw / 2), ly (lv - 1))
            , lineP env 1 (hsb 190 20 40) (cx, y) (x0 + (2 * fromIntegral i + 1.5) * (nw / 2), ly (lv - 1))
            , discP env (if vis then hsb 185 80 100 else if v /= 0 then hsb 190 50 60 else hsb 190 15 22) (cx, y) (min 22 (nw * 0.7) / 2)
            , txtC env cx y 14 (hsb 0 0 100) (show v) ]
          | (lv, vec) <- drop 1 (zip [0 ..] levels)
          , let n = U.length vec
                nw = fromIntegral u * cw / fromIntegral n
                y = ly lv
          , (i, v) <- zip [0 :: Int ..] (U.toList vec)
          , let cx = x0 + (fromIntegral i + 0.5) * nw
                vis = (lv, i) `elem` tvis ]
    rowPics = concat
      [ [ fillBox env (if on i then hsb 190 60 70 else hsb 190 15 16) x y (cw - 2) (cw - 2)
        , frameBox env (if wQ w == Just i then hsb 0 0 100 else hsb 190 20 35) (if wQ w == Just i then 3 else 1) x y (cw - 2) (cw - 2) ]
        ++ [ fillBox env (hsba 55 90 100 0.6) x (rowY + cw / 2 + 2) (cw - 2) 6 | i `elem` nvis ]
        ++ [ fillBox env (hsb 185 80 100) x (rowY + cw / 2 + 10) (cw - 2) 6 | (0, i) `elem` tvis ]
        ++ [ txtC env (x + cw / 2 - 1) (rowY - 1) 14 (hsb 0 0 100) (show (bits U.! i)) | cw > 14 ]
        ++ [ txtC env (x + cw / 2) (rowY + cw / 2 + 28) 14 (hsb 0 0 55) (show i) | cw > 24 ]
      | (x, y, _, i) <- rowCells env u ]
    -- 下：√u × √u のかたまり + 要約
    (gy0, gc, gx0) = gridGeom env u
    gridPics =
      [ txt env (gx0 - 70) (gy0 - 22) 14 (hsb 330 50 95) "Summary"
      , txt env (gx0 + gc + 10) (gy0 - 22) 14 (hsb 0 0 80) "Clusters" ] ++ concat
      [ [ fillBox env (if sm U.! g /= 0 then hsb 330 60 70 else hsb 330 15 16) (gx0 - 70) y (gc - 4) (gc - 4)
        , frameBox env (if sv then hsb 330 80 100 else hsb 330 20 35) (if sv then 3 else 1) (gx0 - 70) y (gc - 4) (gc - 4) ]
        ++ [ txtC env (gx0 - 70 + gc / 2 - 2) (y + gc / 2 - 2) 14 (hsb 0 0 100) (show (sm U.! g)) ]
      | g <- [0 .. s - 1], let y = gy0 + fromIntegral g * gc, let sv = LookS g `elem` cvis ] ++ concat
      [ [ fillBox env (if on i then hsb 190 60 70 else hsb 190 15 16) x y c c
        , frameBox env (if isQ then hsb 0 0 100 else if cv then hsb 330 80 100 else hsb 190 20 35) (if isQ || cv then 3 else 1) x y c c ]
        ++ [ txtFit env (x + c / 2) (y + c / 2) 14 (c - 2) (hsb 0 0 100) (show i) | gc > 20 ]
      | (x, y, c, i) <- gridCells env u
      , let cv = LookC i `elem` cvis
            isQ = wQ w == Just i ]
    -- 結果と比較
    px = envW env - 600
    py = gy0
    doneOf a = min (si + 1) (length a)
    resultPics =
      (case seqs of
         Nothing -> []
         Just q ->
           [ txt env px py 18 (hsb 0 0 100)
               ("SUCCESSOR(" ++ maybe "" show (wQ w) ++ ") = " ++ maybe "none (NIL)" show (sAns q))
           , txt env px (py + 40) 15 (hsb 55 90 100) ("Yellow: scan " ++ show (doneOf (sNaive q)) ++ " cells (worst u = " ++ show u ++ ")")
           , txt env px (py + 66) 15 (hsb 185 80 100)
               (if sTreeOK q
                  then "Light blue: OR tree " ++ show (doneOf (sTree q)) ++ " steps (worst 2 lg u = " ++ show (2 * lg2 u) ++ ")"
                  else "Light blue: OR tree (shown when u <= 64)")
           , txt env px (py + 92) 15 (hsb 330 70 100) ("Pink: summary " ++ show (doneOf (sClus q)) ++ " cells (worst 3 sqrt u = " ++ show (3 * s) ++ ")")
           ]) ++
      (case cellAt env u (mouseXY env) of
         Just i -> [ txt env px (py + 140) 15 (hsb 0 0 90)
                       ("x = " ++ show i ++ ": high = " ++ show (high s i) ++ ", low = " ++ show (low s i) ++ "   (" ++ show (high s i) ++ " * " ++ show s ++ " + " ++ show (low s i) ++ ")") ]
         Nothing -> []) ++
      [ txt env px (py + 172) 14 (hsb 0 0 70) "All of these are still slow per operation."
      , txt env px (py + 192) 14 (hsb 0 0 70) "Build the summary recursively to get"
      , txt env px (py + 212) 14 (hsb 0 0 70) "O(lg lg u): see 08 and 09"
      ]
    headPics =
      [ txt env 24 18 26 (hsb 0 0 100) ("Bit vector and summary (u = " ++ show u ++ ", sqrt u = " ++ show s ++ ")")
      , txt env 24 56 14 (hsb 0 0 80) "Split x: high(x) = floor(x / sqrt u) is the cluster, low(x) = x mod sqrt u is the place in it."
      , txt env 24 76 14 (hsb 0 0 80) "x = index(high, low) = high * sqrt u + low"
      , txt env 24 (h - 74) 14 (hsb 0 0 60) ("Typing: " ++ (if null (wTyped w) then "-" else wTyped w))
      , txt env 24 (h - 52) 14 (hsb 0 0 60) "Click a cell = add or remove / Space = SUCCESSOR of a random x / Digits + Enter = choose x"
      , txt env 24 (h - 30) 14 (hsb 0 0 60) "Left, Right = u (16, 64, 256) / R = new bits"
      ]

lg2 :: Int -> Int
lg2 n = if n <= 1 then 0 else 1 + lg2 (n `div` 2)

-- ---------- マウスとキー ----------
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    pure $ case cellAt env (wU w) (mouseXY env) of
      Just i -> w { wBits = wBits w U.// [(i, 1 - wBits w U.! i)], wSeqs = Nothing, wQ = Nothing }
      Nothing -> w
  EventKey (SpecialKey KeyRight) Down _ _ -> resetWorld w { wU = if wU w == 16 then 64 else 256 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetWorld w { wU = if wU w == 256 then 64 else 16 }
  EventKey (Char c) Down _ _ | isDigit c -> pure w { wTyped = lastN 3 (wTyped w ++ [c]) }
  EventKey k Down _ _ | isBackKey k -> pure w { wTyped = take (length (wTyped w) - 1) (wTyped w) }
  EventKey (SpecialKey KeyEnter) Down _ _ | not (null (wTyped w)) ->
    pure (query (digitsToInt (wTyped w)) w) { wTyped = "" }
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    x <- randomRIO (0, wU w - 1)
    pure (query x w)
  EventKey (Char c) Down _ _ | c `elem` "rR" -> resetWorld w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. density を 0.02 にして SUCCESSOR を探そう。黄と水色とピンクの歩数の差はどう変わる？
-- 2. マス目をクリックして、3 つの方法それぞれの「最悪の場合」を作ってみよう
-- 3. マウスをマス目にのせて high と low を確かめよう。x = high * sqrt u + low になっている？

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左上が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx (gy - size) size c s

-- 中央揃えの文字（中心が x, y）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = let (gx, gy) = toG env x y in labelC gx gy size c s

-- 太さのある線分
lineP :: Env -> Float -> Color -> Point -> Point -> Picture
lineP env w c (x1, y1) (x2, y2) = Color c (thickLine w (toG env x1 y1) (toG env x2 y2))

-- 太さのある折れ線
pathP :: Env -> Float -> Color -> [Point] -> Picture
pathP env w c ps = Color c (thickPath w [toG env x y | (x, y) <- ps])

-- 塗りつぶした長方形（x, y は左上）
fillBox :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
fillBox env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 長方形の枠（x, y は左上、sw は線の太さ）
frameBox :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
frameBox env c sw x y w h = pathP env sw c [(x - sw / 2, y), (x + w + sw / 2, y), (x + w, y), (x + w, y + h), (x + w + sw / 2, y + h), (x - sw / 2, y + h), (x, y + h), (x, y)]

-- 塗りつぶした円（p は中心、r は半径）
discP :: Env -> Color -> Point -> Float -> Picture
discP env c (x, y) r = Color c (at (toG env x y) (disc (max 0.1 r)))

-- 数字の文字列を整数にする（空なら 0）
digitsToInt :: String -> Int
digitsToInt = foldl (\a c -> a * 10 + (fromEnum c - fromEnum '0')) 0

-- 文字列の後ろ n 文字
lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

-- 消すキーか（Mac の delete キーは KeyDelete になる）
isBackKey :: Key -> Bool
isBackKey k = k `elem` [SpecialKey KeyBackspace, SpecialKey KeyDelete, Char '\b', Char '\DEL']

-- 列の i 番目（なければ Nothing）
safeAt :: [a] -> Int -> Maybe a
safeAt xs i = if i < 0 then Nothing else case drop i xs of
  (x : _) -> Just x
  [] -> Nothing

-- マスの中に入る文字（幅 maxW を超えるときは横に縮める）
txtFit :: Env -> Float -> Float -> Float -> Float -> Color -> String -> Picture
txtFit env x y size maxW c s =
  let (gx, gy) = toG env x y
      w = fromIntegral (length s) * size * 0.9
      k = min 1 (maxW / max 1 w)
  in Translate gx gy (Scale k 1 (labelC 0 0 size c s))
