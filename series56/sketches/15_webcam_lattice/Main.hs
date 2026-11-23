-- ============================================
-- 56-15 カメラと束 📷 ─ 映像のブロックを「対象」、見た目の特徴を「属性」にして、概念束をその場で計算する
-- 160 × 120 の映像を 8 × 6 = 48 ブロックに分け、明るい・暗い・赤っぽい・青っぽい・縦のエッジ・動き の 6 つの属性を 0/1 で決める
-- 属性の部分集合 2^6 = 64 通りから概念（外延 = ブロックの集合、内包 = 属性の集合）を全部求め、概念束を描く（08 と同じ計算）
-- 操作：概念の丸をクリック → その外延のブロックを映像の上で強調 / ↑↓ 明るさのしきい値 / ← → 動きのしきい値 / V カメラ ⇔ テスト画像 / S 保存
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import qualified Data.Array as A
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.Bits (shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.List (findIndex, intercalate, sortBy, foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

cw, chh, bxN, byN :: Int
cw = 160                    -- 映像の大きさとブロックの数
chh = 120
bxN = 8
byN = 6

attrs :: [String]
attrs = ["bright", "dark", "reddish", "bluish", "v-edge", "motion"]

-- 概念：外延（ブロックの番号）と内包（属性のビットマスク）
data Concept = Concept { cExt :: [Int], cB :: !Int }

data World = World
  { wObjs    :: [Int]                  -- ブロックごとの属性（ビットマスク）
  , wConc    :: [Concept]
  , wLat     :: Maybe Poset
  , wAttrAt  :: [Int]                  -- 属性 a の概念 μ(a) の番号
  , wLum     :: !(U.Vector Float)      -- 前に計算したときの明るさ（動きを見るため）
  , wPx      :: !(U.Vector Word8)      -- いまの画像（RGB）
  , wPickB   :: !Int
  , wThrB    :: !Double
  , wThrM    :: !Double
  , wUseCam  :: !Bool
  , wLive    :: !Bool
  , wCam     :: Maybe Camera
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "56-15 Camera and concept lattices" world0)
  { skBack  = bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      cam <- startCamera cw chh
      pure world0 { wCam = Just cam, wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [] [] Nothing [] (U.replicate (cw * chh) 0) (testPixels 0) (-1) 0.55 0.05 True False Nothing []

-- ---- 画素 → ブロックの属性（ビットマスク）。明るさは Float32Array と同じく Float でしまっておく
blockAttrs :: U.Vector Word8 -> U.Vector Float -> Double -> Double -> ([Int], U.Vector Float)
blockAttrs px prevA tb tm = (map mask [0 .. bxN * byN - 1], lumA)
  where
    n = cw * chh
    g k = fromIntegral (U.unsafeIndex px k) :: Double
    lumD i = (0.299 * g (3 * i) + 0.587 * g (3 * i + 1) + 0.114 * g (3 * i + 2)) / 255
    lumA = U.generate n (realToFrac . lumD)
    blk i = let (y, x) = i `divMod` cw in (x * bxN) `div` cw + bxN * ((y * byN) `div` chh)
    -- ブロックごとの合計：明るさ・赤・青・横の差・前との差・画素数
    sums = U.accumulate (\(a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6) -> (a1 + b1, a2 + b2, a3 + b3, a4 + b4, a5 + b5, a6 + b6))
             (U.replicate (bxN * byN) (0, 0, 0, 0, 0, 0))
             (U.generate n (\i -> let l = lumD i
                                      r = g (3 * i); gg = g (3 * i + 1); b = g (3 * i + 2)
                                      x = i `mod` cw
                                  in (blk i, (l, (r - (gg + b) / 2) / 255, (b - (r + gg) / 2) / 255
                                             , if x > 0 then abs (l - realToFrac (lumA U.! (i - 1))) else 0
                                             , abs (l - realToFrac (prevA U.! i)), 1 :: Double))))
    mask k = let (l, rd, bl, ex, mv, c) = sums U.! k
             in (if l / c > tb then 1 else 0) .|. (if l / c < 0.3 then 2 else 0) .|. (if rd / c > 0.12 then 4 else 0)
                .|. (if bl / c > 0.1 then 8 else 0) .|. (if ex / c > 0.06 then 16 else 0) .|. (if mv / c > tm then 32 else 0)

-- ---- 概念：属性の部分集合 B → 外延 B′（ブロック）→ 内包 B″
conceptsOf :: [Int] -> Int -> [Concept]
conceptsOf ob nA = sortBy (comparing (length . cExt)) (reverse found)
  where
    full = 2 ^ nA - 1
    (found, _) = foldl' one ([], S.empty) [0 .. full]
    one (acc, seen) b =
      let ext = [ gI | (gI, m) <- zip [0 ..] ob, m .&. b == b ]
      in if S.member ext seen then (acc, seen)
         else (Concept ext (foldl' (\m gI -> m .&. getAt 0 ob gI) full ext) : acc, S.insert ext seen)

-- 外延の ⊆ ＝ 内包の ⊇ で並べる
latticeOf :: [Concept] -> Poset
latticeOf cs = (makePoset n (\i j -> bOf i .&. bOf j == bOf j) [ show (length (cExt c)) | c <- cs ]) { pHint = Just hint }
  where
    n = length cs
    ba = listArray (0, n - 1) (map cB cs) :: UArray Int Int
    bOf i = ba ! i
    dir = [-1, 1, -0.5, 0.5, -0.2, 0.25] :: [Double]
    hint i = sum [ d | (a, d) <- zip [0 ..] dir, (bOf i `shiftR` a) .&. 1 == 1 ]

-- テスト画像：明暗のグラデーション、動く赤い円、青い四角、縦じま（Uint8ClampedArray と同じく偶数への丸め）
testPixels :: Int -> U.Vector Word8
testPixels t = U.generate (cw * chh * 3) px
  where
    cx = 80 + 50 * sin (fromIntegral t * 0.03) :: Double
    cy = 60 + 30 * cos (fromIntegral t * 0.021)
    px k = let (i, c) = k `divMod` 3
               (y, x) = i `divMod` cw
               v = 40 + 170 * (fromIntegral x / fromIntegral cw) :: Double
               col | sqrt ((fromIntegral x - cx) ^ (2 :: Int) + (fromIntegral y - cy) ^ (2 :: Int)) < 16 = (220, 40, 40)
                   | y < 30 && x < 60 = if odd (x `shiftR` 2) then (230, 230, 230) else (20, 20, 20)
                   | x > 120 && y > 70 = (40, 60, 200)
                   | otherwise = (v, v, v)
               (r, g, b) = col
           in toByte (case c of { 0 -> r; 1 -> g; _ -> b })

toByte :: Double -> Word8
toByte v = fromIntegral (max 0 (min 255 (round v :: Int)))

step :: Env -> Float -> World -> IO World
step env _ w = do
  (live, fr) <- case wCam w of
    Just cam | wUseCam w -> do
      ok <- camLive cam
      if not ok then pure (False, B.empty) else do
        f <- cameraFrame cam
        pure (if B.length f >= cw * chh * 3 then (True, f) else (False, B.empty))
    _ -> pure (False, B.empty)
  let px | live = U.generate (cw * chh * 3) (BU.unsafeIndex fr)
         | otherwise = testPixels (envFrame env)
      w1 = w { wLive = live, wPx = px }
  -- 6 コマに 1 回、文脈と概念束を計算しなおす
  pure $ if envFrame env `mod` 6 == 1 || not (isJust (wLat w)) then recompute env w1 else w1

recompute :: Env -> World -> World
recompute env w = w { wObjs = ob, wConc = cs, wLat = Just lat, wAttrAt = aa, wLum = lum }
  where
    (ob, lum) = blockAttrs (wPx w) (wLum w) (wThrB w) (wThrM w)
    cs = conceptsOf ob (length attrs)
    lat = layoutHasse (latticeOf cs) (envW env * 0.56) 190 (envW env * 0.41) (envH env - 300) False
    aa = [ let b = foldl' (\m o -> if (o `shiftR` a) .&. 1 == 1 then m .&. o else m) 63 ob
           in maybe (-1) id (findIndex (\c -> cB c == b) cs) | a <- [0 .. length attrs - 1] ]

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 映像とブロック
  [ at (x0 + iw / 2, y0 + ih / 2) (Scale 1 (-1) (pixelImage cw chh sc (\x y -> let i = 3 * (x + cw * y) in (px U.! i, px U.! (i + 1), px U.! (i + 2))))) ]
  ++ concat [ let bx = x0 + fromIntegral (gI `mod` bxN) * bw
                  by = y0 + fromIntegral (gI `div` bxN) * bh
                  inE = gI `elem` ext
                  o = getAt 0 (wObjs w) gI
              in (if inE then [Color (rgbaC 90 225 215 110) (rectTL bx by bw bh), Color (rgbC 90 225 215) (frameTL 2 bx by bw bh)]
                         else [Color (rgbaC 255 255 255 40) (frameTL 1 bx by bw bh)])
                 ++ [ Color (getAt white dotCols a) (at (bx + 6 + fromIntegral a * 7, by + bh - 6) (disc 3)) | a <- [0 .. 5], (o `shiftR` a) .&. 1 == 1 ]
            | gI <- [0 .. bxN * byN - 1] ]
  -- ---- 概念束（ラベル = 外延のブロックの数、属性名は μ(m) の上に）
  ++ (case wLat w of
        Nothing -> []
        Just lat -> drawHasse lat hopt { oRad = 13, oFs = 14, oCol = \i -> Just (if i == ci then rgbC 90 225 215 else if ci >= 0 && leq lat i ci then rgbC 150 200 210 else rgbC 150 160 205) }
                    : [ let (x, y) = posOf lat k in sayC nm x (y - 4 - 14 * fromIntegral u) 14 (rgbC 255 220 140)
                      | (nm, k, u) <- attrLabels, k >= 0 ])
  -- ---- 文字
  ++ [ say ((if wLive w then "camera image" else "test image (V for camera)") ++ "   dots: bright, dark, red, blue, v-edge, motion") x0 ty 14 (grayC 200)
     , if ci >= 0
         then say ("chosen concept: intent {" ++ intercalate ", " [ nm | (a, nm) <- zip [0 :: Int ..] attrs, (wPickB w `shiftR` a) .&. 1 == 1 ] ++ "}   extent " ++ show (length ext) ++ " blocks (cyan)") x0 (ty + 24) 14 (rgbC 90 225 215)
         else say "Click a circle on the right -> its blocks light up in the image" x0 (ty + 24) 14 (grayC 175)
     , say ("brightness threshold " ++ fmt 2 (wThrB w) ++ " (Up/Down)   motion threshold " ++ fmt 2 (wThrM w) ++ " (Left/Right)") x0 (ty + 48) 14 (grayC 175)
     , say ("Camera and lattices   48 blocks x 6 attributes -> " ++ show (length (wConc w)) ++ " concepts (every 6 frames)") 20 36 18 (grayC 240)
     , say "Click circle: highlight extent   Up/Down brightness   Left/Right motion   V camera <-> test image   S save" 20 62 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    px = wPx w
    sc = min (envW env * 0.5 / fromIntegral cw) ((envH env - 260) / fromIntegral chh)
    x0 = 20
    y0 = 100
    iw = fromIntegral cw * sc
    ih = fromIntegral chh * sc
    bw = iw / fromIntegral bxN
    bh = ih / fromIntegral byN
    ty = y0 + ih + 30
    ci = maybe (-1) id (findIndex (\c -> cB c == wPickB w) (wConc w))
    ext = if ci >= 0 then cExt (getAt (Concept [] 0) (wConc w) ci) else []
    dotCols = [rgbC 255 255 200, rgbC 90 90 120, rgbC 255 80 80, rgbC 80 120 255, rgbC 255 255 255, rgbC 255 190 90]
    -- 同じ概念に属性がいくつ重なるか数えて、上に積む
    attrLabels = reverse (snd (foldl' (\(used, acc) (nm, k) -> let u = M.findWithDefault 0 k used + 1 :: Int
                                                                 in (M.insert k u used, (nm, k, u) : acc)) (M.empty, []) (zip attrs (wAttrAt w))))

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> pure $ case wLat w of
    Nothing -> w
    Just lat -> let i = nodeAt lat (toP5 env pos) 16
                    b = cB (getAt (Concept [] 0) (wConc w) i)
                in if i >= 0 then w { wPickB = if b == wPickB w then -1 else b } else w
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wThrB = min 0.9 (wThrB w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wThrB = max 0.3 (wThrB w - 0.05) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wThrM = min 0.3 (wThrM w + 0.01) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wThrM = max 0.01 (wThrM w - 0.01) }
  EventKey (Char c) Down _ _
    | c `elem` "vV" -> pure w { wUseCam = not (wUseCam w) }
    | c `elem` "sS" -> saveSvg "lattice_camera" env (draw env w) >> pure w
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 5 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("context 'i /= j' 4 x 4 -> 16 concepts (the subset lattice)", length (conceptsOf [14, 13, 11, 7] 4) == 16)
  , ("test image: bottom right bluish, top-left stripes v-edge, top right bright, bottom left dark"
    , oa (blk 150 110) .&. 8 /= 0 && oa (blk 10 10) .&. 16 /= 0 && oa (blk 150 10) .&. 1 /= 0 && oa (blk 5 110) .&. 2 /= 0)
  , ("every concept has A' = B and B' = A (" ++ show (length cs) ++ " concepts)"
    , and [ all (\gI -> oa gI .&. cB c == cB c) (cExt c) && and [ (m .&. cB c == cB c) == (gI `elem` cExt c) | (gI, m) <- zip [0 ..] ob ] | c <- cs ])
  , ("set of intents = block attribute sets closed under intersection (independent check)"
    , S.size closed == length cs && all (\c -> S.member (cB c) closed) cs)
  , ("the concept lattice is a lattice; extent of the meet = intersection of extents", okL) ]
  where
    (ob, _) = blockAttrs (testPixels 0) (U.replicate (cw * chh) 0) 0.55 0.05
    cs = conceptsOf ob 6
    oa gI = getAt 0 ob gI
    blk x y = (x * bxN) `div` cw + bxN * ((y * byN) `div` chh)
    -- 確かめ用：内包 = 対象の属性集合の共通部分（∩ で閉じる）
    closed = grow (S.fromList (63 : ob))
    grow s = let s' = S.union s (S.fromList [ a .&. b | a <- S.toList s, b <- S.toList s ]) in if S.size s' == S.size s then s else grow s'
    lat = latticeOf cs
    ca = A.listArray (0, length cs - 1) cs
    okL = isLattice lat && and [ let m = meetOf lat i j in m >= 0 && cExt (ca A.! m) == filter (`elem` cExt (ca A.! j)) (cExt (ca A.! i))
                               | i <- allOf lat, j <- allOf lat ]

-- ---------- やってみよう ----------
-- 1. 手をふると「動き」の属性が増える。概念束の形はどう変わる？
-- 2. 赤いものをカメラに向けて、「赤っぽい」（reddish）の概念をクリック。どのブロックが光る？
-- 3. ↑↓ で明るさのしきい値を変えると、概念の数は増える？ 減る？
-- 4. attrs に「緑っぽい」を足してみよう（blockAttrs に 1 行足す）。概念は最大 2^7 = 128 個
-- 5. S で保存。自分の部屋を「概念束」で説明する作品にしよう


-- ==================================================
-- ここから下は「束論の道具」（中身は読まなくても OK。消さないでね）
-- ==================================================
-- ---- 順序の表：R ! (i, j) = True は「i ≤ j」（n × n の表）
type Rel = UArray (Int, Int) Bool

-- 順序集合 P。pJ・pM は上限・下限の表（tables で作る）、pPos はハッセ図の位置
data Poset = Poset
  { pN    :: !Int
  , pR    :: !Rel
  , pCov  :: [(Int, Int)]
  , pRk   :: !(UArray Int Int)
  , pLab  :: A.Array Int String
  , pPos  :: A.Array Int (Float, Float)
  , pVal  :: [Int]
  , pHint :: Maybe (Int -> Double)
  , pJ, pM :: !(UArray (Int, Int) Int) }

relSize :: Rel -> Int
relSize r = let ((_, _), (n1, _)) = bounds r in n1 + 1

elemsU :: Rel -> [Bool]
elemsU r = [ r ! (i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1] ] where n = relSize r

assocsU :: Rel -> [((Int, Int), Bool)]
assocsU r = [ ((i, j), r ! (i, j)) | i <- [0 .. n - 1], j <- [0 .. n - 1] ] where n = relSize r

leq :: Poset -> Int -> Int -> Bool
leq p i j = pR p ! (i, j)

rankOf :: Poset -> Int -> Int
rankOf p i = if i >= 0 && i < pN p then pRk p ! i else 0

labOf :: Poset -> Int -> String
labOf p i = if i >= 0 && i < pN p then pLab p A.! i else "-"

posOf :: Poset -> Int -> (Float, Float)
posOf p i = let (lo, hi) = A.bounds (pPos p) in if i >= lo && i <= hi then pPos p A.! i else (0, 0)

makePoset :: Int -> (Int -> Int -> Bool) -> [String] -> Poset
makePoset n leqFn labs = posetFromRel (listArray ((0, 0), (n - 1, n - 1)) [ i == j || leqFn i j | i <- [0 .. n - 1], j <- [0 .. n - 1] ]) labs

-- labs が空なら 0, 1, 2, ... を名前にする
posetFromRel :: Rel -> [String] -> Poset
posetFromRel r labs = Poset n r cov (ranksOf n cov) (A.listArray (0, n - 1) (take n (labs ++ map show [length labs ..])))
                            (A.listArray (0, -1) []) [] Nothing emptyT emptyT
  where n = relSize r
        cov = coversOf r

emptyT :: UArray (Int, Int) Int
emptyT = listArray ((0, 0), (-1, -1)) []

-- 被覆の組から
fromCovers :: [String] -> [(Int, Int)] -> Poset
fromCovers labs cv = posetFromRel (closeRel (runSTUArray (do
  r <- newArray ((0, 0), (n - 1, n - 1)) False
  forM_ cv $ \(a, b) -> writeArray r (a, b) True
  pure r))) labs
  where n = length labs

-- 反射的・推移的閉包（ワーシャル法：k を経由して行けるなら i ≤ j）
closeRel :: Rel -> Rel
closeRel r0 = runSTUArray $ do
  let n = relSize r0
  c <- newArray ((0, 0), (n - 1, n - 1)) False
  forM_ [0 .. n - 1] $ \i -> forM_ [0 .. n - 1] $ \j -> writeArray c (i, j) (r0 ! (i, j) || i == j)
  forM_ [0 .. n - 1] $ \k -> forM_ [0 .. n - 1] $ \i -> do
    ik <- readArray c (i, k)
    when ik $ forM_ [0 .. n - 1] $ \j -> do
      kj <- readArray c (k, j)
      when kj (writeArray c (i, j) True)
  pure c

isReflexive, isAntisym, isTransitive, isPoset :: Rel -> Bool
isReflexive r = and [ r ! (i, i) | i <- [0 .. relSize r - 1] ]
isAntisym r = and [ not (r ! (i, j) && r ! (j, i)) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ] where n = relSize r
isTransitive r = and [ r ! (i, k) | i <- [0 .. n - 1], j <- [0 .. n - 1], r ! (i, j), k <- [0 .. n - 1], r ! (j, k) ] where n = relSize r
isPoset r = isReflexive r && isAntisym r && isTransitive r

-- 被覆 a ⋖ b：a < b で、間にはさまる c がない（推移的簡約 = ハッセ図の辺）
coversOf :: Rel -> [(Int, Int)]
coversOf r = [ (a, b) | a <- [0 .. n - 1], b <- [0 .. n - 1], lt a b, not (any (\c -> lt a c && lt c b) [0 .. n - 1]) ]
  where n = relSize r
        lt a b = a /= b && r ! (a, b) && not (r ! (b, a))

-- ランク = 下から c までの最長の鎖の長さ（最大 n 回くり返せば止まる）
ranksOf :: Int -> [(Int, Int)] -> UArray Int Int
ranksOf n cov = runSTUArray $ do
  rk <- newArray (0, n - 1) 0
  let pass t = when (t <= n) $ do
        ch <- foldM (\c (a, b) -> do
                       ra <- readArray rk a
                       rb <- readArray rk b
                       if rb < ra + 1 && ra < n then writeArray rk b (ra + 1) >> pure True else pure c) False cov
        when ch (pass (t + 1))
  pass 0
  pure rk

allOf :: Poset -> [Int]
allOf p = [0 .. pN p - 1]

upperOf, lowerOf :: Poset -> Int -> Int -> [Int]
upperOf p a b = [ c | c <- allOf p, leq p a c, leq p b c ]     -- 上界：a と b の両方より上
lowerOf p a b = [ c | c <- allOf p, leq p c a, leq p c b ]     -- 下界

-- 最小元・最大元（なければ −1）
leastOf, greatestOf :: Poset -> [Int] -> Int
leastOf p sx = case [ x | x <- sx, all (leq p x) sx ] of { (x : _) -> x; [] -> -1 }
greatestOf p sx = case [ x | x <- sx, all (\y -> leq p y x) sx ] of { (x : _) -> x; [] -> -1 }

-- 上限 a ∨ b = 最小上界、下限 a ∧ b = 最大下界（なければ −1）
joinOf, meetOf :: Poset -> Int -> Int -> Int
joinOf p a b = leastOf p (upperOf p a b)
meetOf p a b = greatestOf p (lowerOf p a b)

botOf, topOf :: Poset -> Int
botOf p = leastOf p (allOf p)
topOf p = greatestOf p (allOf p)

isLattice :: Poset -> Bool
isLattice p = pN p > 0 && and [ joinOf p a b >= 0 && meetOf p a b >= 0 | a <- allOf p, b <- [a + 1 .. pN p - 1] ]

-- 上限・下限の表（何度も使うので先に計算）
tables :: Poset -> Poset
tables p = p { pJ = mk joinOf, pM = mk meetOf }
  where n = pN p
        mk f = listArray ((0, 0), (n - 1, n - 1)) [ f p a b | a <- [0 .. n - 1], b <- [0 .. n - 1] ]

-- 表を引く（tables の前や範囲の外なら −1）
tJ, tM :: Poset -> Int -> Int -> Int
tJ p a b = tableAt (pJ p) a b
tM p a b = tableAt (pM p) a b

tableAt :: UArray (Int, Int) Int -> Int -> Int -> Int
tableAt t a b = let ((_, _), (n1, _)) = bounds t in if a >= 0 && b >= 0 && a <= n1 && b <= n1 then t ! (a, b) else -1

-- ---- よく使う束：約数の束（割り切る）と部分集合の束（含まれる）
lcmOf :: Int -> Int -> Int
lcmOf a b = (a `div` gcd a b) * b

divisorsOf :: Int -> [Int]
divisorsOf n = [ k | k <- [1 .. n], n `mod` k == 0 ]

primeFactors :: Int -> [(Int, Int)]
primeFactors = go 2
  where
    go p n | p * p > n = if n > 1 then [(n, 1)] else []
           | n `mod` p == 0 = let (e, m) = strip p n 0 in (p, e) : go (p + 1) m
           | otherwise = go (p + 1) n
    strip p n e = if n `mod` p == 0 then strip p (n `div` p) (e + 1) else (e, n)

expOf :: Int -> Int -> Int
expOf p v = if v > 0 && v `mod` p == 0 then 1 + expOf p (v `div` p) else 0

divisorPoset :: Int -> Poset
divisorPoset n = (makePoset (length ds) (\i j -> dv j `mod` dv i == 0) (map show ds)) { pVal = ds, pHint = Just hint }
  where
    ds = divisorsOf n
    da = listArray (0, length ds - 1) ds :: UArray Int Int
    dv i = da ! i
    ps = map fst (primeFactors n)
    dir = [-1, 1, -0.3, 0.55, -0.62] :: [Double]
    hint i = sum [ fromIntegral (expOf p (dv i)) * getAt 0 dir (t `mod` 5) | (t, p) <- zip [0 ..] ps ]

popc :: Int -> Int
popc m = if m <= 0 then 0 else (m .&. 1) + popc (m `shiftR` 1)

-- 部分集合の名前（空集合は {}）
setName :: Int -> Int -> String
setName m k = case [ c | (t, c) <- zip [0 .. k - 1] "abcdefg", (m `shiftR` t) .&. 1 == 1 ] of
  "" -> "{}"
  s -> s

powersetPoset :: Int -> Poset
powersetPoset k = (makePoset (2 ^ k) (\i j -> i .&. j == i) [ setName m k | m <- [0 .. 2 ^ k - 1] ])
                    { pVal = [0 .. 2 ^ k - 1], pHint = if k <= 4 then Just hint else Nothing }
  where dir = [-1, 1, -0.3, 0.55] :: [Double]
        hint i = sum [ fromIntegral ((i `shiftR` t) .&. 1) * d | (t, d) <- zip [0 ..] dir ]

-- ---- ハッセ図を描く：高さ = ランク、横 = hint（なければ下の被覆の平均の順）
layoutHasse :: Poset -> Float -> Float -> Float -> Float -> Bool -> Poset
layoutHasse p x y w h exact = p { pPos = A.listArray (0, n - 1) [ (x + w * f2 (pxF i), y + h - (if mr > 0 then fromIntegral (rankOf p i) / fromIntegral mr else 0.5) * h) | i <- [0 .. n - 1] ] }
  where
    n = pN p
    mr = maximum (0 : [ rankOf p i | i <- [0 .. n - 1] ])
    useHint = exact && isJust (pHint p)
    lvl r = [ i | i <- [0 .. n - 1], rankOf p i == r ]
    step :: A.Array Int Double -> Int -> A.Array Int Double
    step px r =
      let ids = lvl r
          v i = case pHint p of
            Just f -> f i
            Nothing -> let ss = [ px A.! a | (a, b) <- pCov p, b == i ]
                       in (if null ss then 0.5 else sum ss / fromIntegral (length ss)) + fromIntegral i * 1e-6
          px1 = px A.// [ (i, v i) | i <- ids ]
          sorted = sortBy (comparing (px1 A.!)) ids
          m = length sorted
      in if useHint then px1 else px1 A.// [ (i, (fromIntegral k + 0.5) / fromIntegral m) | (k, i) <- zip [0 :: Int ..] sorted ]
    pxRaw = foldl' step (A.listArray (0, n - 1) (replicate n 0.5)) [0 .. mr]
    vs = A.elems pxRaw
    lo = if null vs then 0 else minimum vs
    hi = if null vs then 0 else maximum vs
    pxF i | useHint = if hi > lo then 0.04 + 0.92 * (pxRaw A.! i - lo) / (hi - lo) else 0.5
          | otherwise = pxRaw A.! i

-- 描き方の指定：要素の色・辺の色（Nothing なら標準の色）、丸の半径、文字の大きさ
data HOpt = HOpt { oCol :: Int -> Maybe Color, oEcol :: Int -> Int -> Maybe Color, oRad :: Float, oFs :: Float }

hopt :: HOpt
hopt = HOpt (const Nothing) (\_ _ -> Nothing) 16 14

drawHasse :: Poset -> HOpt -> Picture
drawHasse p o = Pictures $
  [ Color (maybe (rgbC 96 108 168) id (oEcol o a b)) (thickLine 2 (posOf p a) (posOf p b)) | (a, b) <- pCov p ]
  ++ concat [ [ Color bgCol (at (posOf p i) (disc (oRad o + 1)))
              , Color (maybe (rgbC 226 232 248) id (oCol o i)) (at (posOf p i) (disc (oRad o - 1)))
              , let (px, py) = posOf p i in sayM (labOf p i) px (py + 1) (max 14 (oFs o)) bgCol ]
            | i <- allOf p ]

-- (mx, my) にいちばん近い要素（r より遠ければ −1）
nodeAt :: Poset -> (Float, Float) -> Float -> Int
nodeAt p (mx, my) r = fst (foldl' f (-1, r) (allOf p))
  where f (best, bd) i = let (x, y) = posOf p i
                             d = sqrt ((x - mx) * (x - mx) + (y - my) * (y - my))
                         in if d < bd then (i, d) else (best, bd)

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rnd :: Rng -> (Double, Rng)
rnd (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- ランダムな順番 o で、前 → 後ろに確率 p で辺 → 閉包
randomPoset :: Int -> Double -> Rng -> (Poset, Rng)
randomPoset n p r0 = (posetFromRel (closeRel rel) [], r2)
  where
    shuffle (o, r) i = let (u, r') = rnd r
                           j = floor (u * fromIntegral (i + 1))
                           oi = o A.! i
                           oj = o A.! j
                       in (o A.// [(i, oj), (j, oi)], r')
    (ord, r1) = foldl' shuffle (A.listArray (0, n - 1) [0 .. n - 1] :: A.Array Int Int, r0) [n - 1, n - 2 .. 1]
    pick (acc, r) (a, b) = let (u, r') = rnd r in (if u < p then (ord A.! a, ord A.! b) : acc else acc, r')
    (edges, r2) = foldl' pick ([], r1) [ (a, b) | a <- [0 .. n - 1], b <- [a + 1 .. n - 1] ]
    rel = runSTUArray $ do
      m <- newArray ((0, 0), (n - 1, n - 1)) False
      forM_ edges $ \e -> writeArray m e True
      pure m

-- p5.js 版と同じ順番：clampv v a b（v を a〜b に収める）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else v

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

f2 :: Double -> Float
f2 v = if isNaN v || isInfinite v then 0 else realToFrac v

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (if isNaN v || isInfinite v then 0 else v) ""

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- gloss のマウス座標 → 左上が原点の座標
toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

bgCol :: Color
bgCol = rgbC 18 20 44

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃え（y は文字の下の線）、sayM は (x, y) が文字の真ん中
sayC, sayR, sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c
sayM s x y size c = say s (x - textW s size / 2) (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- ---- 自己テストの道具
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
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 220 170 else rgbC 255 90 90)
