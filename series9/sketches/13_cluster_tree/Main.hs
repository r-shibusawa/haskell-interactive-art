-- ============================================
-- 13 クラスタの木（H0 のパーシステンス = 階層的クラスタリング）
-- 近い点どうしから順に合体させていく記録を「木（デンドログラム）」にする
-- 木をどの高さで切るかで、クラスタの数が決まる。長い枝 = はっきり分かれたグループ
-- マウス上下 = 切る高さ / 1〜3 でデータ / クリックで作り直す
-- ============================================
module Main (main) where

import Art
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', mapAccumL, sortOn)
import System.Random (mkStdGen, randomRs)

-- 合体の記録：どの 2 点をつないだか、その距離
data Merge = Merge { mA :: !Int, mB :: !Int, mD :: !Float }

data World = World
  { wPts    :: ![Point]
  , wMerges :: ![Merge]     -- 距離順
  , wKind   :: !Int
  , wSeed   :: !Int
  }

initialWorld :: World
initialWorld = World [] [] 1 1

main :: IO ()
main = runSketch (sketch "09-13 Cluster Tree" initialWorld)
  { skBack  = hsb 230 40 10
  , skStart = \env -> pure (makePoints env initialWorld)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

kindName :: Int -> String
kindName k = case k of
  1 -> "3 blobs"
  2 -> "2 close blobs"
  _ -> "Random"

makePoints :: Env -> World -> World
makePoints env w = w { wPts = pts, wMerges = singleLinkage pts }
  where
    pts = zipWith sample [0 :: Int ..] (take 40 (randomChunks (wSeed w) 6))
    cx = envW env * 0.3
    cy = envH env * 0.55
    bigR = min (envW env) (envH env) * 0.2
    sample i (ua : ub : g1 : g2 : g3 : g4 : _) = case wKind w of
      1 -> let a = tau * fromIntegral (i `mod` 3) / 3
           in (cx + cos a * bigR + gaussian g1 g2 * 22, cy + sin a * bigR + gaussian g3 g4 * 22)
      2 -> let s = if odd i then 1 else -1
           in (cx + s * bigR * 0.5 + gaussian g1 g2 * 25, cy + gaussian g3 g4 * 25)
      _ -> (cx + lerpF (-bigR * 1.3) (bigR * 1.3) ua, cy + lerpF (-bigR * 1.3) (bigR * 1.3) ub)
    sample _ _ = (cx, cy)

-- ★ 単連結法：全ペアを距離順に並べ、別のグループなら合体（= H0 の死）
singleLinkage :: [Point] -> [Merge]
singleLinkage pts = reverse (snd (foldl' step (ufNew is, []) pairs))
  where
    is = [0 .. length pts - 1]
    indexed = zip is pts
    pairs = sortOn mD [Merge i j (dist2 p q) | (i, p) <- indexed, (j, q) <- indexed, i < j]
    step (uf, out) m
      | ufFind uf (mA m) /= ufFind uf (mB m) = (ufUnion uf (mA m) (mB m), m : out)
      | otherwise = (uf, out)

-- 切る高さ d でのクラスタ番号（0, 1, 2, ... を出てきた順に）
clustersAt :: World -> Float -> [Int]
clustersAt w d = snd (mapAccumL number IM.empty is)
  where
    is = [0 .. length (wPts w) - 1]
    final = foldl' (\u m -> if mD m < d then ufUnion u (mA m) (mB m) else u) (ufNew is) (wMerges w)
    number ids i =
      let r = ufFind final i
      in case IM.lookup r ids of
           Just k  -> (ids, k)
           Nothing -> let k = IM.size ids in (IM.insert r k ids, k)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ -- 左：点（クラスタで色分け）
    Pictures [ Color (hsb (fromIntegral ((c * 47) `mod` 360)) 70 100) (at (g x y) (disc 5))
             | ((x, y), c) <- zip (wPts w) cl ]
  -- 右：デンドログラム風（H0 バーコードとして描く：各合体 = 1 本の棒が終わる）
  , Color (hsb 0 0 30) (Line [g bx (by + bh), g (bx + bw) (by + bh)])
  , Pictures (zipWith barPic [0 ..] ds)
  , Color (hsb 45 90 100) (thickLine 2 (g (bx + cutD / 250 * bw) by) (g (bx + cutD / 250 * bw) (by + bh)))
  , txt env bx (by - 30) 14 (hsb 0 0 60) "H0 bars (short to long): length = distance at merge."
  , txt env bx (by - 10) 14 (hsb 0 0 60) "Bars longer than the yellow line + 1 = clusters."
  , txt env 20 40 18 (hsb 0 0 100)
      ("Cut height " ++ show (round cutD :: Int) ++ " (mouse Y)  -> " ++ show nCl ++ " clusters  Data: " ++ kindName (wKind w) ++ " (1-3)  Click: new")
  , txt env 20 68 15 (hsb 0 0 60)
      "A big gap in the bar lengths is the natural cut. Three blobs: just 2 long bars stick out."
  ]
  where
    g = toG env
    cutD = mapC (snd (mouseXY env)) 0 (envH env) 250 5
    cl = clustersAt w cutD
    nCl = if null cl then 0 else maximumOf cl + 1
    maximumOf = foldl' max 0
    bx = envW env * 0.6
    bw = envW env - bx - 50
    by = 120
    bh = envH env - 220
    -- 棒：合体距離を短い順に棒として描く
    ds = map mD (wMerges w)
    nd = fromIntegral (max 1 (length ds)) :: Float
    barPic :: Float -> Float -> Picture
    barPic k d =
      let y = by + k / nd * bh
          col = if d < cutD then hsb 0 0 45 else hsb 200 80 100
      in Color col (thickLine (max 2 (bh / nd - 3)) (g bx y) (g (bx + min d 250 / 250 * bw) y))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> pure (makePoints env w { wKind = fromEnum c - fromEnum '0' })
  EventKey (MouseButton LeftButton) Down _ _ -> do
    s <- randomRIO (0, 9999)
    pure (makePoints env w { wSeed = s })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 3 つの塊で、クラスタ数が 3 になる高さの範囲は広い？（安定 = 本物のクラスタ）
-- 2. ランダム（3）だと、どの高さでも「それらしい」クラスタ数がない
-- 3. 「平均連結法」（グループの平均距離で合体）にすると結果は変わる？

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 範囲からはみ出さない remap（p5.js の map(v, a, b, c, d, true)）
mapC :: Float -> Float -> Float -> Float -> Float -> Float
mapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)

-- 種から決まる 0〜1 の乱数を k 個ずつ区切った列（p5.js の randomSeed + random）。
-- 同じ種なら毎回同じ点群になる
randomChunks :: Int -> Int -> [[Float]]
randomChunks seed k = go (randomRs (0, 1) (mkStdGen seed))
  where go us = let (a, b) = splitAt k us in a : go b

-- 0〜1 の乱数 2 つから正規分布の乱数を作る（p5.js の randomGaussian）
gaussian :: Float -> Float -> Float
gaussian u v = sqrt (-2 * log (max 1e-6 u)) * cos (tau * v)

-- union-find（つながったグループを管理する）
newtype UnionFind = UnionFind (IM.IntMap Int)

ufNew :: [Int] -> UnionFind
ufNew is = UnionFind (IM.fromList [(i, i) | i <- is])

ufFind :: UnionFind -> Int -> Int
ufFind uf@(UnionFind m) x = case IM.lookup x m of
  Just p | p /= x -> ufFind uf p
  _               -> x

ufUnion :: UnionFind -> Int -> Int -> UnionFind
ufUnion uf@(UnionFind m) a b =
  let ra = ufFind uf a
      rb = ufFind uf b
  in if ra == rb then uf else UnionFind (IM.insert ra rb m)
