-- ============================================
-- 04 ランダムグラフと巨大成分（エルデシュ＝レーニ）
-- 点どうしを確率 p でつなぐ。p が小さいとバラバラ、ある値を超えると突然ほとんどがひとつながりに
-- マウス左右 = p / 色 = つながっている仲間（連結成分） / R で配置をやり直す
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.IntMap.Strict as IM
import Numeric (showFFloat)
import System.Random (mkStdGen, randoms)

-- 同じ seed なら同じ乱数の列になるので、p を動かしても線は「増える」だけ
newtype World = World { wSeed :: Int }

n :: Int
n = 80                     -- ★ 点の数

maxP :: Float
maxP = 0.06                -- ★ マウス右端の p

main :: IO ()
main = runSketch (sketch "08-04 Random Graph" (World 1))
  { skBack  = hsb 220 40 10
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ 確率 p でつなぐ
buildEdges :: Int -> Float -> [(Int, Int)]
buildEdges seed p =
  [ pair
  | (pair, r) <- zip [(i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1]] (randoms (mkStdGen seed) :: [Float])
  , r < p ]

-- ★ 連結成分：union-find（各点の「親」をたどると、その成分の代表に着く）
find :: IM.IntMap Int -> Int -> Int
find parent x =
  let px = IM.findWithDefault x x parent
  in if px == x then x else find parent px

union :: IM.IntMap Int -> (Int, Int) -> IM.IntMap Int
union parent (a, b) = IM.insert (find parent a) (find parent b) parent

-- 各点の成分番号（見つけた順に 0, 1, 2, ...）
components :: [(Int, Int)] -> IM.IntMap Int
components edges = snd (foldl' number (IM.empty, IM.empty) [0 .. n - 1])
  where
    parent = foldl' union (IM.fromList [(i, i) | i <- [0 .. n - 1]]) edges
    number (ids, comp) i =
      let r = find parent i
      in case IM.lookup r ids of
           Just k -> (ids, IM.insert i k comp)
           Nothing -> let k = IM.size ids in (IM.insert r k ids, IM.insert i k comp)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (map edgePic edges ++ concatMap nodePic [0 .. n - 1] ++ hud)
  where
    p = clampF 0 maxP (remap (fst (mouseXY env)) 0 (envW env) 0 maxP)
    edges = buildEdges (wSeed w) p
    comp = components edges
    compOf i = IM.findWithDefault 0 i comp
    sizes = IM.fromListWith (+) [(compOf i, 1 :: Int) | i <- [0 .. n - 1]]
    biggest = maximum (0 : IM.elems sizes)
    bigId = case [k | (k, s) <- IM.toAscList sizes, s == biggest] of
      (k : _) -> k
      [] -> 0
    inBig i = compOf i == bigId
    radius = min (envW env) (envH env) * 0.38
    pos i =
      let a = tau * fromIntegral i / fromIntegral n
      in toG env (envW env / 2 + cos a * radius) (envH env / 2 + 30 + sin a * radius)
    edgePic (a, b)
      | inBig a = Color (hsba 45 90 90 0.6) (thickLine 1.5 (pos a) (pos b))
      | otherwise = Color (hsba 200 40 50 0.6) (Line [pos a, pos b])
    nodePic i
      | inBig i = [Color (hsb 45 90 100) (at (pos i) (disc 7))]
      | otherwise = [Color (hsb (fromIntegral ((compOf i * 47) `mod` 360)) 50 60) (at (pos i) (disc 5))]
    avgDeg = 2 * fromIntegral (length edges) / fromIntegral n :: Float
    meterX = envW env - 260
    hud =
      [ txt env 20 32 18 (hsb 0 0 100)
          ("p = " ++ nf 3 p ++ " (mouse X)  Edges " ++ show (length edges) ++ "  Avg degree " ++ nf 2 avgDeg)
      , txt env 20 56 18 (hsb 0 0 100)
          ("Components " ++ show (IM.size sizes) ++ "  Largest " ++ show biggest ++ " / " ++ show n ++ "   R: new layout")
      , txt env 20 80 15 (hsb 0 0 60)
          ("Giant component appears when avg degree passes 1. Yellow = largest. Boundary p = 1/n = " ++ nf 3 (1 / fromIntegral n))
      -- メーター
      , Color (hsb 0 0 20) (rectTL env meterX 90 220 16)
      , Color (hsb 45 90 100) (rectTL env meterX 90 (220 * fromIntegral biggest / fromIntegral n) 16)
      , txt env meterX 126 14 (hsb 0 0 70) "Largest component share"
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char c) Down _ _ | c `elem` "rR" -> do
    s <- randomRIO (0, 9999)
    pure w { wSeed = s }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. p を 0 からゆっくり上げて、黄色が「突然」増える瞬間を見つける（平均次数 ≈ 1）
-- 2. n を 200 にすると境目の p は変わる？（1/n になる）
-- 3. 伝染病や噂は、平均次数が 1 を超えると大流行する。同じ現象

-- ---------- 道具 ----------

-- 左上が (x, y) の長方形（p5.js の rect）
rectTL :: Env -> Float -> Float -> Float -> Float -> Picture
rectTL env x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in rect gx gy w h

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 小数を d 桁で文字にする（p5.js の nf）
nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
