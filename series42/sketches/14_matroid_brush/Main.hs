-- ============================================
-- 42-14 全域木ブラシ
-- ドラッグで点を撒くと、近い点どうしの「最小全域木（森）」がリアルタイムに伸びていく成長アート
--   点を足すたびにクラスカル法（06）をやり直す：軽い辺から順に、閉路ができなければ採用
--   新しく採用された辺はにゅっと伸びて現れ、不要になった辺は消える。色は辺の長さ（重み）
-- 操作：ドラッグ = 点を撒く / ↑↓ = つなぐ半径 / M = 最小 ⇄ 最大全域木 / P = 点の表示 / C = クリア / S = 保存
-- ============================================
module Main (main) where

import Art
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newListArray, readArray, writeArray)
import Data.List (sortOn)

radius0 :: Float
radius0 = 90             -- ★ この距離より近い点どうしだけ辺の候補にする

kNear :: Int
kNear = 6                -- ★ 1 点あたりの候補の本数

spacing :: Float
spacing = 9              -- ★ 点と点の最小間隔（ドラッグ中）

maxPts :: Int
maxPts = 900             -- ★ 点の上限

growFrames :: Float
growFrames = 18          -- ★ 辺が伸びきるまでのフレーム数

data Cand = Cand { cA, cB :: !Int, cW :: !Float }

data World = World
  { wPts     :: !(IM.IntMap Point)
  , wCand    :: ![Cand]                         -- 候補の辺
  , wTree    :: !(M.Map (Int, Int) (Float, Int))  -- 採用中の辺 (a, b) → (重み, 生まれたフレーム)
  , wRadius  :: !Float
  , wMaximum :: !Bool                           -- True = 最大全域木
  , wShowPts :: !Bool
  }

main :: IO ()
main = runSketch (sketch "42-14 Spanning tree brush" world0)
  { skBack  = hsb 260 35 7
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World IM.empty [] M.empty radius0 False True

-- 点 i から、それより前の点への辺の候補（近い順に kNear 本まで）
candsOf :: Float -> IM.IntMap Point -> Int -> Point -> [Cand]
candsOf radius pts i p =
  let near = [ Cand j i d | (j, q) <- IM.toAscList pts, j < i, let d = dist2 p q, d < radius ]
  in take kNear (sortOn cW near)

-- ★ 点を 1 つ足す：近い点への辺を候補に加えて、木を作り直す
addPoint :: Int -> Point -> World -> World
addPoint frame p w
  | IM.size (wPts w) >= maxPts = w
  | any (\q -> dist2 p q < spacing) (IM.elems (wPts w)) = w
  | otherwise =
      let i = IM.size (wPts w)
          cs = candsOf (wRadius w) (wPts w) i p
      in rebuild frame w { wPts = IM.insert i p (wPts w), wCand = wCand w ++ cs }

rebuild :: Int -> World -> World
rebuild frame w =
  let order = sortOn (\c -> if wMaximum w then negate (cW c) else cW c) (wCand w)
      kept = kruskal (IM.size (wPts w)) cA cB order
      entry c = case M.lookup (cA c, cB c) (wTree w) of
        Just old -> ((cA c, cB c), old)
        Nothing -> ((cA c, cB c), (cW c, frame))
  in w { wTree = M.fromList (map entry kept) }

-- 半径を変えたら候補の辺を全部作り直す
recand :: Int -> World -> World
recand frame w =
  rebuild frame w { wCand = concat [ candsOf (wRadius w) (wPts w) i p | (i, p) <- IM.toAscList (wPts w) ] }

step :: Env -> Float -> World -> IO World
step env _ w
  | envDown env && my > 90 = do
      jx <- randomRIO (-4, 4)
      jy <- randomRIO (-4, 4)
      pure (addPoint (envFrame env) (mx + jx, my + jy) w)
  | otherwise = pure w
  where (mx, my) = mouseP env

draw :: Env -> World -> Picture
draw env w =
  let radius = wRadius w
      edgePic ((a, b), (wgt, born)) =
        let p@(px, py) = IM.findWithDefault (0, 0) a (wPts w)
            (qx, qy) = IM.findWithDefault (0, 0) b (wPts w)
            t0 = clampF 0 1 (fromIntegral (envFrame env - born) / growFrames)
            t = 1 - (1 - t0) * (1 - t0)    -- ゆっくり止まる
            hue = remap wgt 0 radius 300 120
            th = max 0.5 (remap wgt 0 radius 4 1.2)
        in seg env th (hsba hue 70 100 0.9) p (lerpF px qx t, lerpF py qy t)
      nPts = IM.size (wPts w)
      nTree = M.size (wTree w)
      ptPics = if wShowPts w then map (dot env 1.5 (withA 0.7 white)) (IM.elems (wPts w)) else []
  in Pictures $
       map edgePic (M.toList (wTree w)) ++ ptPics ++
       [ txt env 20 40 16 white ("Drag: scatter  Up/Down: radius " ++ show (roundI radius) ++ "  M: " ++ (if wMaximum w then "maximum" else "minimum") ++ " tree  P: points " ++ (if wShowPts w then "ON" else "off") ++ "  C: clear  S: save")
       , txt env 20 66 14 (gray 0.6) ("Points " ++ show nPts ++ " / " ++ show maxPts ++ "  edges " ++ show nTree ++ "  trees " ++ show (nPts - nTree) ++ "  (edges = points - trees: the rank)" ++ (if nPts >= maxPts then "  Full! C to clear" else ""))
       ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (recand frame w { wRadius = min 300 (wRadius w + 15) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (recand frame w { wRadius = max 20 (wRadius w - 15) })
  EventKey (Char c) Down _ _
    | c `elem` "mM" -> pure (rebuild frame w { wMaximum = not (wMaximum w), wTree = M.empty })
    | c `elem` "pP" -> pure w { wShowPts = not (wShowPts w) }
    | c `elem` "cC" -> pure w { wPts = IM.empty, wCand = [], wTree = M.empty }
    | c `elem` "sS" -> w <$ saveSvg "matroid_brush" env (draw env w)
  _ -> pure w
  where frame = envFrame env

-- ---------- やってみよう ----------
-- 1. ゆっくり 1 本の線を描く → 木は線になる。ぐるっと輪を描く → 閉路はできない（どこか 1 か所が切れる）
-- 2. M で最大全域木にする。同じ点なのに全然ちがう形になる。どちらも「基」だが重みがちがう
-- 3. 色を「辺の長さ」でなく「描いた順」や「木の番号（union-find の根）」にしてみよう
-- 4. 点を消すブラシを作る（右クリックで近くの点を削除 → wCand から外して rebuild）。木はどう変わる？

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標（中央が原点、y は上向き）へ
sp :: Env -> Point -> Point
sp env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点の座標で）
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 文字（左下を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sp env (x, y) in label a b size c s

-- 太さ w の線分
seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c p q = Color c (thickLine w (sp env p) (sp env q))

-- 塗りつぶしの丸（半径 r）
dot :: Env -> Float -> Color -> Point -> Picture
dot env r c p = Color c (at (sp env p) (disc r))

-- 四捨五入（Math.round）
roundI :: Float -> Int
roundI x = floor (x + 0.5)

-- union-find を ST の配列で（辺が多いので速い版）。候補の辺を順に見て、閉路を作らないものだけ残す
kruskal :: Int -> (e -> Int) -> (e -> Int) -> [e] -> [e]
kruskal n ea eb es = runST $ do
  parent <- newListArray (0, max 0 (n - 1)) [0 .. max 0 (n - 1)] :: ST s (STUArray s Int Int)
  let find x = do
        p <- readArray parent x
        if p == x then pure x else do
          g <- readArray parent p
          writeArray parent x g        -- 道を縮める
          find g
      go acc [] = pure (reverse acc)
      go acc (e : r) = do
        ra <- find (ea e)
        rb <- find (eb e)
        if ra == rb then go acc r else do
          writeArray parent ra rb
          go (e : acc) r
  go [] es
