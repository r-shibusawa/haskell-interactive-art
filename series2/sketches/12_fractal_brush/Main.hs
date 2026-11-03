-- ============================================
-- 12 フラクタルの森（自由制作のベース）
-- クリックした場所に木が生えて、音を鳴らしながら育つ
-- C で消す / S で保存
-- ============================================
-- ★ のついた数字を変えて、自分だけの森にしよう！
module Main (main) where

import Art
import System.Random (StdGen, mkStdGen, randomR)

-- 枝ごとのゆらぎと、その先の枝
data Node = Node !Float [Node]

data Tree = Tree
  { tX, tY    :: !Float
  , tSize     :: !Float
  , tMaxDepth :: !Int
  , tBranches :: !Int
  , tAngle    :: !Float
  , tRatio    :: !Float
  , tHue      :: !Float
  , tGrow     :: !Float
  , tShape    :: Node      -- 種から作った形（この木の形は毎回同じ）
  }

newtype World = World [Tree]   -- 古い木が先頭

notes :: [String]
notes = ["C3", "E3", "G3", "B3", "D4", "F#4", "A4", "C5"]   -- ★ 育つときの音

maxPrims :: Int
maxPrims = 7400   -- 森全体の図形の上限。超えたら古い木から消える

main :: IO ()
main = runSketch (sketch "02-12 Fractal Forest" (World []))
  { skBack  = hsb 230 40 8
  , skStep  = \_ _ (World ts) -> World <$> mapM growTree ts
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 育つ：depth が 0 から maxDepth まで少しずつ増える
growTree :: Tree -> IO Tree
growTree t = do
  let before = floor (tGrow t) :: Int
      grown = min (tGrow t + 0.08) (fromIntegral (tMaxDepth t))   -- ★ 育つ速さ
      after = floor grown :: Int
  -- 1 段育つごとに、高い音を鳴らす
  if after > before
    then playNote (noteAt after) 0.2 0.5 Triangle
    else pure ()
  pure t { tGrow = grown }

drawWorld :: Env -> World -> Picture
drawWorld env (World ts) = Pictures
  ( map drawTree ts
  ++ [ label (-envW env / 2 + 20) (envH env / 2 - 30) 16 white
         ("Click: plant a tree (" ++ show (length ts) ++ ")   C: clear   S: save") ] )

drawTree :: Tree -> Picture
drawTree t = Pictures (branch t (tX t) (tY t) 0 (tSize t) 0 (tShape t))

branch :: Tree -> Float -> Float -> Float -> Float -> Int -> Node -> [Picture]
branch t x y h len depth (Node _ kids)
  | d > tGrow t = []   -- まだ育っていない部分は描かない
  | partial < 1 = [stem]
  | depth == tMaxDepth t =
      [ stem, at tip (Color (hsba (wrapF 360 (tHue t + 180)) 70 100 0.8) (disc 4)) ]  -- ★ 花の色
  | otherwise = stem : concat (zipWith kid [0 ..] kids)
  where
    d = fromIntegral depth
    -- 育っている途中の枝は短く描く（にゅっと伸びる）
    partial = min 1 (tGrow t - d + 1)
    l = len * partial
    tip@(tx, ty) = (x + l * sin h, y + l * cos h)
    wd = remap d 0 (fromIntegral (tMaxDepth t)) 10 1
    c = hsb (wrapF 360 (tHue t + d * 12)) 60 (60 + d * 5)    -- ★ 色
    body = thickLine wd (x, y) tip
    stem = Color c (if depth <= 2 then Pictures [body, at tip (disc (wd / 2))] else body)
    n = fromIntegral (tBranches t) :: Float   -- ★ 枝の数
    kid :: Float -> Node -> [Picture]
    kid i node@(Node jitter _) =
      branch t tx ty (h + remap i 0 (n - 1) (-tAngle t) (tAngle t) + jitter) (len * tRatio t) (depth + 1) node

onEvent :: Env -> Event -> World -> IO World
onEvent env ev (World ts) = case ev of
  EventKey (MouseButton LeftButton) Down _ (mx, my) | my <= envH env / 2 - 44 -> do
    t <- plant mx my
    pure (World (limit (ts ++ [t])))
  EventKey (Char c) Down _ _
    | c == 'c' || c == 'C' -> pure (World [])
    | c == 's' || c == 'S' -> do
        saveSvg "forest" env (drawWorld env (World ts))
        pure (World ts)
  _ -> pure (World ts)

plant :: Float -> Float -> IO Tree
plant x y = do
  size <- randomRIO (50, 130)         -- ★ 幹の長さ
  depth <- randomRIO (5, 7)           -- ★ 深さ
  r <- randomRIO (0, 1 :: Float)
  angle <- randomRIO (0.3, 0.9)       -- ★ 枝の広がり
  ratio <- randomRIO (0.6, 0.75)      -- ★ 縮む割合
  hue <- randomRIO (0, 360)
  seed <- randomRIO (0, 99999)
  let branches = if r < 0.5 then 2 else 3
      shape = fst (makeShape branches depth 0 0 (mkStdGen seed))
  pure (Tree x y size depth branches angle ratio hue 0 shape)

-- ---------- 自由制作のアイデア ----------
-- ・枝の先を花ではなく、円の中に円（00）やコッホ（06）にしてみる
-- ・EventMotion で、ドラッグしながら森を植える
-- ・10 のマイクと組み合わせて、声で木が育つようにする
-- ・背景を 14 の山にして、山にフラクタルの森をつくる

-- ---------- 道具 ----------

noteAt :: Int -> String
noteAt i = case drop (i `mod` length notes) notes of
  (n : _) -> n
  [] -> "C4"

-- 枝ごとのゆらぎ（-0.15〜0.15）を種から作る
makeShape :: Int -> Int -> Int -> Float -> StdGen -> (Node, StdGen)
makeShape n maxDepth depth jitter g0
  | depth >= maxDepth = (Node jitter [], g0)
  | otherwise =
      let (kids, g1) = many n g0
      in (Node jitter kids, g1)
  where
    many :: Int -> StdGen -> ([Node], StdGen)
    many 0 g = ([], g)
    many k g =
      let (j, g1) = randomR (-0.15, 0.15) g
          (kid, g2) = makeShape n maxDepth (depth + 1) j g1
          (rest, g3) = many (k - 1) g2
      in (kid : rest, g3)

-- 1 本の木の図形の数（枝 ＋ 花 ＋ 太い枝の丸）
treePrims :: Tree -> Int
treePrims t = sum [n ^ d | d <- [0 .. m]] + n ^ m + 1 + n + n * n
  where
    n = tBranches t
    m = tMaxDepth t

-- 図形が多すぎたら古い木から消す
limit :: [Tree] -> [Tree]
limit ts
  | sum (map treePrims ts) <= maxPrims = ts
  | otherwise = limit (drop 1 ts)
