-- ============================================
-- 02 風にゆれる木
-- ランダムな枝 ＋ ノイズでゆっくりゆれる ＋ 花が咲く
-- クリックで新しい木が生える / S で保存
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import System.Random (StdGen, mkStdGen, randomR)

-- 木の形。種から 1 度だけ作って、あとは毎フレーム風をかけて描く
data Node
  = Flower !Float !Float !Float !Float   -- 枝の長さ、枝の曲がり、花の色のずれ、花の直径
  | Fork !Float !Float [Node]            -- 枝の長さ、枝の曲がり、その先の枝

data World = World
  { wHue  :: !Float    -- 花の色
  , wTree :: Node
  }

maxPrims :: Int
maxPrims = 7400        -- ★ 図形の数の上限（これより茂った木は別の種に替える）

main :: IO ()
main = runSketch (sketch "02-02 Wind Tree" (World 340 (makeTree 800 1)))  -- 種 1、花の色 340
  { skBack  = hsb 220 40 30
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (sky ++ [drawNode (wHue w) winds 0 (-h / 2) 0 0 (wTree w)])
  where
    ww = envW env
    h = envH env
    -- 空のグラデーション
    sky =
      [ Color (hsb (remap y 0 h 220 30) 40 (remap y 0 h 30 85)) (rect 0 (h / 2 - y - 2) ww 4.5)
      | y <- [0, 4 .. h] ]
    -- 風：ノイズで角度が少しずつ変わる（depth が深いほど大きくゆれる）
    t = fromIntegral (envFrame env) * 0.005
    winds = U.generate 24 (\d -> (noise2 t (fromIntegral d) - 0.5) * fromIntegral d * 0.08)  -- ★ 0.08 が風の強さ

-- 枝を 1 本描いて、その先を再帰で描く
drawNode :: Float -> U.Vector Float -> Float -> Float -> Float -> Int -> Node -> Picture
drawNode hue winds x y heading depth node = case node of
  Flower len turn dh size ->
    let (tip, pic) = stem len turn
    in Pictures [pic, at tip (Color (hsba (hue + dh) 60 100 0.8) (disc (size / 2)))]
  Fork len turn kids ->
    let (tip@(tx, ty), pic) = stem len turn
        h' = heading + turn + wind
    in Pictures (pic : map (drawNode hue winds tx ty h' (depth + 1)) kids)
  where
    d = fromIntegral depth
    wind = winds U.! min 23 depth
    wd = clampF 1 16 (remap d 0 9 16 1)
    c = hsb 25 60 (remap d 0 9 35 60)
    stem len turn =
      let h' = heading + turn + wind
          tip = (x + len * sin h', y + len * cos h')
          body = thickLine wd (x, y) tip
          pic = if depth <= 5 then Pictures [body, at tip (disc (wd / 2))] else body
      in (tip, Color c pic)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> do
    seed <- randomRIO (0, 99999)      -- 新しい種 → 新しい木
    hue <- randomRIO (0, 360)         -- 花の色もランダム
    pure w { wHue = hue, wTree = makeTree (envH env) seed }
  EventKey (Char c) Down _ _ | c == 's' || c == 'S' -> do
    saveSvg "tree" env (drawWorld env w)
    pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 風の強さ 0.08 を 0.3 にすると？（台風！）
-- 2. 花を disc から、rect や label（文字）に変えよう
-- 3. 枝の数が 3 になる確率 0.3 を変えて、木の茂り方を変えよう

-- ---------- 道具 ----------

-- 種から木を作る（同じ種なら同じ木）。茂りすぎた木は次の種に替える
makeTree :: Float -> Int -> Node
makeTree h seed
  | prims tree <= maxPrims = tree
  | otherwise = makeTree h (seed + 1)
  where tree = fst (growNode (h * 0.25) 0 (mkStdGen seed))

growNode :: Float -> Float -> StdGen -> (Node, StdGen)
growNode len turn g0
  | len < 8 =
      -- 枝の先に花を咲かせる
      let (dh, g1) = randomR (-20, 20) g0
          (size, g2) = randomR (6, 16) g1
      in (Flower len turn dh size, g2)
  | otherwise =
      -- 枝の数と角度をランダムに決める
      let (r, g1) = randomR (0, 1 :: Float) g0
          n = if r < 0.3 then 3 else 2 :: Int
          (kids, g2) = growKids n g1
      in (Fork len turn kids, g2)
  where
    growKids :: Int -> StdGen -> ([Node], StdGen)
    growKids 0 g = ([], g)
    growKids k g =
      let (a, g1) = randomR (-0.7, 0.7) g
          (ratio, g2) = randomR (0.6, 0.8) g1
          (kid, g3) = growNode (len * ratio) a g2
          (rest, g4) = growKids (k - 1) g3
      in (kid : rest, g4)

-- 描くときの図形の数
prims :: Node -> Int
prims (Flower {}) = 2
prims (Fork _ _ kids) = 1 + sum (map prims kids)
