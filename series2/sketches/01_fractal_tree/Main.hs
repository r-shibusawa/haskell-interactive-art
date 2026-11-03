-- ============================================
-- 01 フラクタルツリー
-- 枝の先からまた枝が生える、を再帰でくり返す
-- 左右 = 枝の開く角度 / 上下 = 枝の縮む割合
-- クリックで 2本枝 ⇄ 3本枝
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

newtype World = World { wBranches :: Int }  -- 1 本の枝から何本に分かれるか

-- 1 本の枝：根もとの位置、向き（真上が 0、右回りが正）、長さ
data Twig = Twig !Float !Float !Float !Float

maxLines :: Int
maxLines = 7400   -- ★ 描く枝の数の上限（これ以上は 1 フレームで描ききれない）

main :: IO ()
main = runSketch (sketch "02-01 Fractal Tree" (World 2))
  { skBack  = hsb 210 40 12
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (map drawLevel levels ++ [info])
  where
    ww = envW env
    wh = envH env
    (mx, my) = envMouse env
    angle = remap (mx + ww / 2) 0 ww 0 (pi / 2)  -- 枝の角度 0°〜90°
    ratio = remap (wh / 2 - my) 0 wh 0.8 0.5     -- 枝がどれだけ短くなるか
    -- 画面の下の中央から、上に向かって幹を描く
    root = Twig 0 (-wh / 2) 0 (wh * 0.28)
    levels = grow (wBranches w) angle ratio maxLines 0 [root]
    info = label (-ww / 2 + 20) (wh / 2 - 30) 18 white
      ( "Angle " ++ show (round (angle * 180 / pi) :: Int) ++ " deg   Ratio " ++ nf2 ratio
        ++ "   Click: branches (" ++ show (wBranches w) ++ ")" )

-- 世代ごとに枝を増やしていく再帰。budget は残りの本数
-- （p5.js 版は枝を 1 本ずつ深く進むが、全体の本数をおさえるために世代ごとに進む）
grow :: Int -> Float -> Float -> Int -> Int -> [Twig] -> [(Int, [Twig])]
grow _ _ _ _ _ [] = []
grow n angle ratio budget depth twigs
  | count > budget = [(depth, thin budget count twigs)]   -- 入りきらない世代は間引いて終わり
  | otherwise = (depth, twigs) : grow n angle ratio (budget - count) (depth + 1) next
  where
    count = length twigs
    next = concatMap (children n angle ratio depth) twigs

-- 枝の先に生える枝
children :: Int -> Float -> Float -> Int -> Twig -> [Twig]
children n angle ratio depth t@(Twig _ _ h len)
  -- 枝が短くなりすぎたら終わり（終わる条件がないと無限に続いてしまう！）
  | len < 5 || depth > 11 = []
  | otherwise =
      [ Twig x y (h + angle) (len * ratio)     -- 右の枝
      , Twig x y (h - angle) (len * ratio) ]   -- 左の枝
      -- 3 本モードなら、まっすぐの枝も
      ++ [ Twig x y h (len * ratio * 0.8) | n == 3 ]
  where (x, y) = tipOf t

-- 同じ世代の枝は同じ色・同じ太さ
drawLevel :: (Int, [Twig]) -> Picture
drawLevel (depth, twigs) = Color c (Pictures (map (drawTwig depth wd) twigs))
  where
    d = fromIntegral depth
    wd = clampF 1 14 (remap d 0 10 14 1)
    c = hsb (remap d 0 10 30 130) 60 (remap d 0 10 60 100)  -- 茶色 → 緑

drawTwig :: Int -> Float -> Twig -> Picture
drawTwig depth wd t@(Twig x y _ _)
  | depth <= 5 = Pictures [thickLine wd (x, y) tip, at tip (disc (wd / 2))]  -- 太い枝は先を丸く
  | otherwise = thickLine wd (x, y) tip
  where tip = tipOf t

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    pure w { wBranches = if wBranches w == 2 then 3 else 2 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. マウスを動かして、木・雪の結晶・シダのような形を探そう
-- 2. 左右の角度をちがう値にすると？（h + angle と h - angle * 0.5）
-- 3. 枝の色を自分で決めよう（drawLevel で depth によって変える）

-- ---------- 道具 ----------

-- 枝の先の位置
tipOf :: Twig -> Point
tipOf (Twig x y h len) = (x + len * sin h, y + len * cos h)

-- n 本のうち、まんべんなく budget 本だけ残す
thin :: Int -> Int -> [a] -> [a]
thin budget n xs = [x | (i, x) <- zip [0 ..] xs, (i * budget) `mod` n < budget]

-- 小数 2 桁の文字列（p5.js の nf(x, 1, 2)）
nf2 :: Float -> String
nf2 x = showFFloat (Just 2) x ""
