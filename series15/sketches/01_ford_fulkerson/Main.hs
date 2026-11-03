-- ============================================
-- 01 フォード＝ファルカーソン法
-- 「残余ネットワーク」= あとどれだけ流せるか（順向き）と、どれだけ戻せるか（逆向き）の地図
-- 残余ネットワークで s から t への道（増加道）を探し、いちばん細いところの量だけ流す。道がなくなったら最大
-- スペース = 1 手ずつ / A = 自動 / G = フローと残余ネットワークを切りかえ / 1：お手本 / 2：ランダム / R = 最初から
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.List (intercalate)
import qualified Data.IntSet as IS

data Node = Node { nName :: String, nFx :: !Float, nFy :: !Float }

data Edge = Edge { eU :: !Int, eV :: !Int, eC :: !Int, eF :: !Int }

-- 残余ネットワークの矢印（dir = +1 順向き、-1 逆向き）
data RArc = RArc { aE :: !Int, aDir :: !Int, aFrom :: !Int, aTo :: !Int, aR :: !Int }

data Phase = Find | Augment | Finished deriving Eq

data World = World
  { wNodes :: [Node]
  , wEdges :: [Edge]
  , wPhase :: !Phase
  , wPath :: [RArc]         -- 増加道
  , wBottleneck :: !Int
  , wResidual :: !Bool      -- 残余ネットワークを表示中か
  , wAuto :: !Bool
  , wTimer :: !Int
  , wLog :: [String]
  , wReach :: [Bool]
  }

autoEvery :: Int
autoEvery = 40              -- ★ 自動のとき何フレームごとに 1 手進めるか

main :: IO ()
main = runSketch (sketch "15-01 Ford-Fulkerson" (preset1 world0))
  { skBack  = hsb 230 40 10
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

world0 :: World
world0 = World [] [] Find [] 0 True False 0 [] []

preset1 :: World -> World
preset1 w = reset w
  { wNodes = [Node "s" 0.1 0.55, Node "a" 0.33 0.32, Node "b" 0.33 0.78, Node "c" 0.62 0.32, Node "d" 0.62 0.78, Node "t" 0.85 0.55]
  , wEdges = [Edge u v c 0 | (u, v, c) <- [(0, 1, 10), (0, 2, 12), (1, 2, 3), (1, 3, 9), (1, 4, 4), (2, 4, 11), (4, 3, 5), (3, 5, 15), (4, 5, 9)]]
  }

-- ランダムなネットワーク（真ん中の 8 点は 3 列に、少しずらして並べる）
preset2 :: World -> IO World
preset2 w = do
  mids <- mapM midNode (zip [0 ..] "abcdefgh")
  let ns = [Node "s" 0.08 0.55] ++ mids ++ [Node "t" 0.88 0.55]
      xOf i = nFx (ns !! i)
      add es (u, v)
        | u == v || any (\e -> (eU e == u && eV e == v) || (eU e == v && eV e == u)) es = pure es
        | otherwise = do
            c <- randomRIO (2, 11)
            pure (es ++ [Edge u v c 0])
      addMaybe es (i, j)
        | abs (xOf i - xOf j) >= 0.25 = pure es
        | otherwise = do
            r <- randomRIO (0, 1 :: Float)
            if r < 0.4
              then add es (if xOf i <= xOf j then (i, j) else (j, i))
              else pure es
  es1 <- foldM add [] ([(0, i) | i <- [1 .. 3]] ++ [(i, 9) | i <- [6 .. 8]])
  es2 <- foldM addMaybe es1 [(i, j) | i <- [1 .. 8], j <- [i + 1 .. 8]]
  pure (reset w { wNodes = ns, wEdges = es2 })
  where
    midNode :: (Int, Char) -> IO Node
    midNode (i, ch) = do
      dx <- randomRIO (-0.03, 0.03)
      dy <- randomRIO (-0.04, 0.04)
      pure (Node [ch] (0.27 + fromIntegral (i `div` 3) * 0.2 + dx) (0.28 + fromIntegral (i `mod` 3) * 0.25 + dy))

reset :: World -> World
reset w = w
  { wEdges = [e { eF = 0 } | e <- wEdges w]
  , wPhase = Find, wPath = [], wLog = [], wReach = [], wAuto = False }

-- 残余容量のある向き（順向き c-f、逆向き f）をすべて並べる
residualArcs :: [Edge] -> [RArc]
residualArcs es = concat
  [ [RArc i 1 (eU e) (eV e) (eC e - eF e) | eC e - eF e > 0] ++ [RArc i (-1) (eV e) (eU e) (eF e) | eF e > 0]
  | (i, e) <- zip [0 ..] es ]

-- 深さ優先探索で増加道を 1 本探す（フォード＝ファルカーソン法は探し方を決めていない）
-- 見つからなければ、s から行ける点の集合を返す
findPathDFS :: World -> Either IS.IntSet [RArc]
findPathDFS w =
  case dfs 0 IS.empty of
    (Just p, _) -> Right p
    (Nothing, seen) -> Left seen
  where
    arcs = residualArcs (wEdges w)
    t = length (wNodes w) - 1
    dfs v seen
      | v == t = (Just [], seen)
      | otherwise = go [a | a <- arcs, aFrom a == v] (IS.insert v seen)
    go [] s = (Nothing, s)
    go (a : r) s
      | IS.member (aTo a) s = go r s
      | otherwise = case dfs (aTo a) s of
          (Just p, s') -> (Just (a : p), s')
          (Nothing, s') -> go r s'

advance :: World -> World
advance w = case wPhase w of
  Find -> case findPathDFS w of
    Left seen -> w
      { wPhase = Finished, wAuto = False
      , wReach = [IS.member i seen || i == 0 | i <- [0 .. length (wNodes w) - 1]] }
    Right p -> w { wPath = p, wBottleneck = foldr (min . aR) maxBound p, wPhase = Augment }
  Augment ->
    let b = wBottleneck w
        apply e0 a = [ if i == aE a then e { eF = eF e + aDir a * b } else e | (i, e) <- zip [0 ..] e0 ]
        names = map (nName . (wNodes w !!) . aFrom) (wPath w)
        entry = intercalate "->" names ++ "->t  +" ++ show b
        logged = wLog w ++ [entry]
    in w { wEdges = foldl apply (wEdges w) (wPath w)
         , wLog = drop (length logged - 9) logged
         , wPath = [], wPhase = Find }
  Finished -> w

value :: World -> Int
value w = sum [ (if eU e == 0 then eF e else 0) - (if eV e == 0 then eF e else 0) | e <- wEdges w ]

stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wAuto w =
      let t = wTimer w + 1
          w' = w { wTimer = t }
      in pure (if t `mod` autoEvery == 0 then advance w' else w')
  | otherwise = pure w

nodePos :: Env -> Node -> Point
nodePos env n = (envW env * nFx n, envH env * nFy n)

drawW :: Env -> World -> Picture
drawW env w =
  let ns = wNodes w
      lastI = length ns - 1
      clouds
        | wPhase w == Finished =
            -- 最小カット：s から残余ネットワークでたどれる点（S 側）を塗る
            [dot env (hsba 140 60 60 0.25) (nodePos env n) 120 | (n, True) <- zip ns (wReach w)]
        | otherwise = []
      lines'
        | wResidual w = map (drawArc env ns False) (residualArcs (wEdges w))
        | otherwise = map (drawFlowEdge env ns) (wEdges w)
      nodePic (i, n) =
        let p@(x, y) = nodePos env n
            c | i == 0 = hsb 140 70 85
              | i == lastI = hsb 20 75 95
              | otherwise = hsb 230 15 88
        in Pictures [dot env c p 50, txtC env x y 17 (lum 10) (nName n)]
      msg = case wPhase w of
        Find -> "Next: find an s-t path in the residual network (DFS)"
        Augment -> "Augmenting path (yellow). Narrowest part = " ++ show (wBottleneck w) ++ ", push that much"
        Finished -> "No path -> max flow! Green = reachable from s. Green-to-outside pipes = min cut (all full)"
  in Pictures
       [ Pictures clouds
       , Pictures lines'
       , Pictures (map (drawArc env ns True) (wPath w))
       , Pictures (map nodePic (zip [0 :: Int ..] ns))
       , txt env 20 36 17 (lum 100)
           ("Space: 1 step  A: auto  G: " ++ (if wResidual w then "showing residual" else "showing flow") ++ "  1: example  2: random  R: restart")
       , txt env 20 62 15 (lum 65)
           (if wResidual w
              then "Blue = can still push forward (c - f)   Orange = can undo backward (f). Backward arcs cancel earlier flow"
              else "Numbers = flow / capacity")
       , txt env 20 (envH env - 64) 18 (hsb 45 80 100) msg
       , txt env 20 (envH env - 26) 26 (lum 100) ("Flow value = " ++ show (value w))
       , Pictures [ txt env (envW env - 340) (100 + fromIntegral k * 22) 14 (lum 70) s | (k, s) <- zip [0 :: Int ..] (wLog w) ]
       ]

-- 点から点への矢印。戻り値は図と、数字を置く場所
arrowP :: Env -> Point -> Point -> Float -> Float -> Color -> (Picture, Point)
arrowP env (x1, y1) (x2, y2) off wd col =
  let ang = atan2 (y2 - y1) (x2 - x1)
      ox = -sin ang * off
      oy = cos ang * off
      a@(ax, ay) = (x1 + cos ang * 28 + ox, y1 + sin ang * 28 + oy)
      b@(bx, by) = (x2 - cos ang * 30 + ox, y2 - sin ang * 30 + oy)
  in (Pictures [seg env col wd a b, tip env col b ang 14 0.45], ((ax + bx) / 2 + ox * 0.9, (ay + by) / 2 + oy * 0.9))

edgeLabel :: Env -> Point -> String -> Color -> Picture
edgeLabel env (mx, my) s col = Pictures
  [ boxC env (hsba 230 40 12 0.85) mx my (14 + fromIntegral (length s) * 10) 22
  , txtC env mx my 14 col s ]

drawArc :: Env -> [Node] -> Bool -> RArc -> Picture
drawArc env ns hi a =
  let col | hi = hsb 50 90 100
          | aDir a > 0 = hsb 200 70 90
          | otherwise = hsb 25 80 95
      (pic, m) = arrowP env (nodePos env (ns !! aFrom a)) (nodePos env (ns !! aTo a)) 9
                   (if hi then 6 else 1.5 + fromIntegral (aR a) * 0.35) col
  in Pictures [pic, if hi then Blank else edgeLabel env m (show (aR a)) col]

drawFlowEdge :: Env -> [Node] -> Edge -> Picture
drawFlowEdge env ns e =
  let full = eF e == eC e
      col | full = hsb 45 90 100
          | eF e > 0 = hsb 195 80 95
          | otherwise = hsb 230 15 40
      (pic, m) = arrowP env (nodePos env (ns !! eU e)) (nodePos env (ns !! eV e)) 0
                   (2 + fromIntegral (eF e) * 0.6) col
  in Pictures [pic, edgeLabel env m (show (eF e) ++ "/" ++ show (eC e)) (if full then hsb 45 90 100 else lum 95)]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (advance w)
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "gG" -> pure w { wResidual = not (wResidual w) }
    | c == '1' -> pure (preset1 w)
    | c == '2' -> preset2 w
    | c `elem` "rR" -> pure (reset w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. G で残余ネットワークとフローを見比べる。逆向きの橙の矢印を使った増加道はどれ？
-- 2. お手本の最大フローは 21。最後に緑から外へ出る線の容量を足すと？（最大フロー最小カット定理）
-- 3. findPathDFS の探す順番を変えると、道の選び方と回数が変わる（02 で比べる）

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- 中心 (x, y)、幅 w、高さ h の塗りつぶし（rectMode(CENTER) の rect）
boxC :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
boxC env c x y w h = Color c (rect (gx env x) (gy env y) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 中心 (x, y)、直径 d の円（p5.js の circle）
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

-- 矢じり：先端 (x, y)、向き ang、長さ s、開き a
tip :: Env -> Color -> Point -> Float -> Float -> Float -> Picture
tip env c (x, y) ang s a = Color c (Polygon (map (pt env)
  [(x, y), (x - cos (ang - a) * s, y - sin (ang - a) * s), (x - cos (ang + a) * s, y - sin (ang + a) * s)]))

-- 文字：txt は左下が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = label (gx env x - fromIntegral (length str) * s * 0.72) (gy env y) s c str

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0
