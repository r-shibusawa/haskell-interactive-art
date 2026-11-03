-- ============================================
-- 01 点を太らせる（半径 r の円）
-- 各点のまわりに半径 r の円を置く。r を大きくすると円が重なり、
-- 「いくつの塊か」「穴はあるか」が、r によって変わっていく
-- マウス左右 = 半径 r / 1〜3 で形 / クリックで作り直す
-- ============================================
module Main (main) where

import Art
import qualified Data.IntMap.Strict as IM
import Data.List (elemIndex, foldl', nub)
import Data.Maybe (fromMaybe)
import System.Random (mkStdGen, randomRs)

data World = World
  { wPts   :: ![Point]
  , wShape :: !Int
  , wSeed  :: !Int
  }

main :: IO ()
main = runSketch (sketch "09-01 Balls" (World [] 1 1))
  { skBack  = hsb 230 40 10
  , skStart = \env -> pure (makePoints env (World [] 1 1))
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

makePoints :: Env -> World -> World
makePoints env w = w { wPts = map sample (take 40 (randomChunks (wSeed w) 7)) }
  where
    cx = envW env / 2
    cy = envH env / 2 + 30
    bigR = min (envW env) (envH env) * 0.25
    sample (ua : us : ur : g1 : g2 : g3 : g4 : _) =
      let a = ua * tau
          s = if us < 0.5 then -1 else 1
          (x, y) = case wShape w of
            1 -> (cx + cos a * bigR, cy + sin a * bigR)
            2 -> (cx + s * bigR * 1.1 + cos a * bigR * 0.6, cy + sin a * bigR * 0.6)
            _ -> let rr = bigR * sqrt ur in (cx + cos a * rr, cy + sin a * rr)
      in (x + gaussian g1 g2 * 8, y + gaussian g3 g4 * 8)
    sample _ = (cx, cy)

-- ★ 塊の数：円が重なる（距離 < 2r）点どうしを同じグループに（union-find）
-- 返り値は、各点が属するグループの代表の番号
components :: [Point] -> Float -> [Int]
components pts r = map (ufFind final) is
  where
    is = [0 .. length pts - 1]
    final = foldl' join (ufNew is)
      [(i, j) | (i, p) <- zip is pts, (j, q) <- zip is pts, i < j, dist2 p q < 2 * r]
    join uf (i, j) = ufUnion uf i j

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ -- 円（塊ごとに色）
    Pictures [ Color (hsba (fromIntegral ((idOf g * 53) `mod` 360)) 60 80 0.35) (at (toG env x y) (disc r))
             | ((x, y), g) <- zip pts group ]
  , Color (hsb 0 0 100) (Pictures [at (toG env x y) (disc 3.5) | (x, y) <- pts])
  , txt env 20 40 18 (hsb 0 0 100)
      ("r = " ++ show (round r :: Int) ++ " (mouse X)  Blobs = " ++ show (length ids) ++ "  1: circle  2: two circles  3: blob  Click: new")
  , txt env 20 68 15 (hsb 0 0 60)
      "Small r: all separate (40 blobs). Larger r: they merge, then holes fill. TDA records what happens at each r."
  ]
  where
    pts = wPts w
    r = mapC (fst (mouseXY env)) 0 (envW env) 2 160
    group = components pts r
    ids = nub group
    idOf g = fromMaybe 0 (elemIndex g ids)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '3' -> pure (makePoints env w { wShape = fromEnum c - fromEnum '0' })
  EventKey (MouseButton LeftButton) Down _ _ -> do
    s <- randomRIO (0, 9999)
    pure (makePoints env w { wSeed = s })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 塊の数が 1 になる r を見つけよう。形 1 と形 3 でちがう？
-- 2. 円（1）で、穴が「できて → 消える」r の範囲を目で読み取ろう（02 以降で計算する）
-- 3. 円の数が 2 つから 1 つになる r は、2 つの円の「すきま」の半分

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
