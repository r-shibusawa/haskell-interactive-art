-- ============================================
-- 08 RRT ─ ランダムに伸びる木で道を探す
-- マス目に切らなくても道が探せる。ランダムな点を打ち、木のいちばん近い節からそこへ少し伸ばす。これだけ
--   木は「まだ行っていない広い場所」へ自然に伸びていく（探索の偏りが空間の広さに比例）
--   RRT*：新しい節の親を「近くでいちばんコストが小さい節」に選び直し、周りの節もつなぎ直す（rewire）→ 道がだんだん短くなる
-- スペース = 伸ばす／止める / クリック = ゴール / ↑↓ = ゴールへの偏り / S = RRT ⇄ RRT* / R = 地図をつくり直す / C = 木を消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, replicateM)
import Data.Char (toLower)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Text.Printf (printf)

stepLen, goalRadius, rewireR :: Float
stepLen = 22              -- ★ 1 回に伸ばす長さ
goalRadius = 18
rewireR = 60              -- ★ RRT* でつなぎ直す半径

maxNodes, perFrame :: Int
maxNodes = 3000           -- ★ 節の上限
perFrame = 6              -- ★ 1 フレームで試す回数

data Node = Node { nX :: !Float, nY :: !Float, nParent :: !Int, nCost :: !Float }
data Box = Box !Float !Float !Float !Float    -- 左上 x, y, 幅, 高さ

data World = World
  { wObs     :: ![Box]
  , wNodes   :: !(IM.IntMap Node)   -- 番号 → 節
  , wStartP  :: !Point
  , wGoal    :: !Point
  , wBias    :: !Float    -- ★ ゴールをサンプルする確率
  , wStar    :: !Bool     -- RRT* にするか
  , wRunning :: !Bool
  , wBest    :: !Int      -- ゴールに届いた節（最良）。なければ -1
  }

main :: IO ()
main = runSketch (sketch "21-08 RRT" world0)
  { skBack  = hsb 215 35 12
  , skStart = \_ -> makeMap world0
  , skStep  = \_ _ w -> step w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = resetTree (World [] IM.empty (80, height / 2) (width - 100, height / 2) 0.05 False False (-1))

makeMap :: World -> IO World
makeMap w = do
  obs <- replicateM 9 (Box <$> randomRIO (width * 0.2, width * 0.8) <*> randomRIO (110, height - 120)
                           <*> randomRIO (30, 110) <*> randomRIO (30, 140))
  pure (resetTree w { wObs = obs, wStartP = (80, height / 2), wGoal = (width - 100, height / 2) })

resetTree :: World -> World
resetTree w =
  let (sx, sy) = wStartP w
  in w { wNodes = IM.singleton 0 (Node sx sy (-1) 0), wBest = -1 }

inObstacle :: World -> Point -> Bool
inObstacle w (x, y) =
  x < 0 || y < 100 || x > width || y > height
  || any (\(Box ox oy ow oh) -> x > ox && x < ox + ow && y > oy && y < oy + oh) (wObs w)

-- 線分が障害物にぶつかるか（数点をサンプルして調べる）
blocked :: World -> Point -> Point -> Bool
blocked w a@(ax, ay) b@(bx, by) =
  let n = max 2 (ceiling (dist2 a b / 6)) :: Int
      k i = fromIntegral i / fromIntegral n
  in any (\i -> inObstacle w (lerpF ax bx (k i), lerpF ay by (k i))) [0 .. n]

posOf :: Node -> Point
posOf n = (nX n, nY n)

-- いちばん近い節の番号
nearest :: World -> Point -> Int
nearest w (px, py) = fst (IM.foldlWithKey' pick (0, 1 / 0) (wNodes w))
  where
    pick best@(_, bd) i n =
      let dx = nX n - px
          dy = nY n - py
          d = dx * dx + dy * dy
      in if d < bd then (i, d) else best

-- ★ RRT の 1 回：サンプル → いちばん近い節 → そこから stepLen だけ伸ばす
grow :: World -> IO World
grow w
  | IM.size (wNodes w) >= maxNodes = pure w { wRunning = False }
  | otherwise = do
      u <- randomRIO (0, 1)
      s <- if u < wBias w then pure (wGoal w)
           else (,) <$> randomRIO (0, width) <*> randomRIO (100, height)
      pure (extend s w)

extend :: Point -> World -> World
extend s@(sx, sy) w = case IM.lookup ni (wNodes w) of
  Nothing -> w
  Just nn
    | d < 1 || blocked w (posOf nn) q -> w
    | otherwise -> addNode ni q (nCost nn + ext) w
    where
      d = dist2 s (posOf nn)
      ext = min stepLen d
      q = (nX nn + (sx - nX nn) / d * ext, nY nn + (sy - nY nn) / d * ext)
  where ni = nearest w s

addNode :: Int -> Point -> Float -> World -> World
addNode ni0 q@(qx, qy) cost0 w =
  let nodes0 = wNodes w
      -- ★ RRT*：半径内の節から「コストが最小になる親」を選ぶ
      near | wStar w = [ (i, n, dd) | (i, n) <- IM.toList nodes0, let dd = dist2 (posOf n) q, dd < rewireR ]
           | otherwise = []
      choose (c, p) (i, n, dd)
        | nCost n + dd < c && not (blocked w (posOf n) q) = (nCost n + dd, i)
        | otherwise = (c, p)
      (cost, parent) = foldl' choose (cost0, ni0) near
      qi = IM.size nodes0
      nodes1 = IM.insert qi (Node qx qy parent cost) nodes0
      -- rewire：新しい節を経由したほうが近道になる節は親をつけ替える
      rewire m (i, n, dd)
        | cost + dd < nCost n && not (blocked w q (posOf n)) = IM.insert i n { nParent = qi, nCost = cost + dd } m
        | otherwise = m
      nodes2 = foldl' rewire nodes1 near
      reached = dist2 q (wGoal w) < goalRadius
      bestCost = maybe (1 / 0) nCost (IM.lookup (wBest w) nodes2)
  in w { wNodes = nodes2
       , wBest = if reached && (wBest w < 0 || cost < bestCost) then qi else wBest w
       , wRunning = wRunning w && not (reached && not (wStar w)) }

step :: World -> IO World
step w0 = do
  w1 <- if wRunning w0
          then foldM (\w _ -> grow w) w0 [1 .. perFrame]
          else pure w0
  pure (recheckBest w1)

-- RRT* は rewire でコストが変わるので、ゴール付近の最良節を取り直す
recheckBest :: World -> World
recheckBest w
  | wStar w && wBest w >= 0 = w { wBest = fst (IM.foldlWithKey' pick (wBest w, c0) (wNodes w)) }
  | otherwise = w
  where
    c0 = maybe (1 / 0) nCost (IM.lookup (wBest w) (wNodes w))
    pick best@(_, bc) i n
      | dist2 (posOf n) (wGoal w) < goalRadius && nCost n < bc = (i, nCost n)
      | otherwise = best

-- 節 i から根までの点の列（念のため節の数を上限にする）
pathFrom :: World -> Int -> [Point]
pathFrom w = go (IM.size (wNodes w))
  where
    go :: Int -> Int -> [Point]
    go 0 _ = []
    go k i = case IM.lookup i (wNodes w) of
      Nothing -> []
      Just n -> posOf n : go (k - 1) (nParent n)

drawAll :: World -> Picture
drawAll w =
  let nodes = wNodes w
      edge n = case IM.lookup (nParent n) nodes of
        Just p -> Line [posOf p, posOf n]
        Nothing -> Blank
      goal = wGoal w
      best = IM.lookup (wBest w) nodes
      boxPic (Box x y bw bh) = boxAt x y bw bh
  in Pictures
  [ view
      [ Color (hsb 0 60 70) (Pictures (map boxPic (wObs w)))
      , Color (hsbA 200 50 80 50) (Pictures (map edge (IM.elems nodes)))    -- 木
      , if wBest w >= 0 then Color (hsb 45 90 100) (thickPath 4 (pathFrom w (wBest w))) else Blank   -- 道
      , Color (hsb 170 80 100) (at (wStartP w) (disc 10))
      , Color (hsbA 120 80 100 40) (at goal (disc goalRadius))
      , Color (hsb 120 80 100) (at goal (disc 8))
      ]
  , txt 20 34 16 (hsb 0 0 100) ("Space: " ++ (if wRunning w then "stop" else "grow") ++ "   " ++ (if wStar w then "RRT*" else "RRT") ++ " (S)   Goal bias "
                                ++ nf 2 (wBias w) ++ " (Up/Down)   Nodes " ++ show (IM.size nodes)
                                ++ maybe "" (\n -> "   Path length " ++ nf 0 (nCost n)) best)
  , txt 20 58 14 (hsb 0 0 100) "Click: goal   R: new map   C: clear tree"
  , txt 20 78 14 (hsb 0 0 60) "RRT stops when it arrives (rough path). RRT* keeps growing and the path gets shorter. Bias 0 fills space evenly."
  ]

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let m@(_, my) = toP5 pos
    in pure (if my < 90 || inObstacle w m then w else w { wGoal = m, wBest = -1 })
  EventKey (SpecialKey k) Down _ _ -> case k of
    KeySpace -> pure w { wRunning = not (wRunning w) }
    KeyUp -> pure w { wBias = min 0.5 (wBias w + 0.05) }
    KeyDown -> pure w { wBias = max 0 (wBias w - 0.05) }
    _ -> pure w
  EventKey (Char c) Down _ _ -> case toLower c of
    's' -> pure (resetTree w { wStar = not (wStar w), wRunning = True })
    'r' -> makeMap w
    'c' -> pure (resetTree w)
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 偏り 0 と 0.5 で、ゴールに届くまでの節の数を比べよう。偏りが大きいと障害物の裏で詰まることがある
-- 2. RRT* を 3000 節まで伸ばして、道の長さが下がっていくのを見る。rewireR を 120 にすると？
-- 3. stepLen を 5 にすると細かいが遅い。60 だと障害物をすり抜ける？（blocked の 6 px 刻みを見る）
-- 4. 狭い隙間（障害物を 2 つ並べる）を通る道を RRT はなかなか見つけない。なぜ？


-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- マウスの位置（gloss の座標：中央が原点、y は上向き）を p5 と同じ座標（左上が原点、y は下向き）に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は p5 の text と同じ左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 左上の角が (x, y) の長方形（p5 の rect）
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y bw bh = rect (x + bw / 2) (y + bh / 2) bw bh

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 小数点以下 d 桁の文字列（p5 の nf）
nf :: Int -> Float -> String
nf d = printf ("%." ++ show d ++ "f")
