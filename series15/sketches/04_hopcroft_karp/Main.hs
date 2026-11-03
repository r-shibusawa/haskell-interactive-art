-- ============================================
-- 04 ホップクロフト＝カープ法
-- 03 では増加道を 1 本ずつ見つけていた。ここでは 1 回の「フェーズ」で
-- ① 幅優先探索で空いている左の点から層（距離）をつける ② 最短の増加道を、点がかぶらないように何本も同時に見つける ③ まとめて反転
-- フェーズの回数は O(√V) で済む → 全体 O(E√V)
-- スペース = 1 手 / A = 自動 / 上下 = 点の数 / R = ランダム
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM)

-- 増加道の 1 本の線。(左 i, 右 j, 今ペアの線か)
type Link = (Int, Int, Bool)

data Phase = Bfs | Paths | Flip | Finished deriving Eq

data World = World
  { wN :: !Int
  , wAdj :: [[Int]]
  , wMatchL :: [Int]
  , wMatchR :: [Int]
  , wDistL :: [Int]         -- 層（inf = まだ届いていない）
  , wDistR :: [Int]
  , wMaxLayer :: !Int
  , wPaths :: [[Link]]
  , wPhase :: !Phase
  , wPhases :: !Int
  , wAugments :: !Int
  , wAuto :: !Bool
  , wTimer :: !Int
  , wMessage :: String
  }

inf :: Int
inf = 1000000000

greedyChance :: Float
greedyChance = 0.6           -- ★ 最初に欲張りでマッチさせる確率（0 にすると？）

main :: IO ()
main = runSketch (sketch "15-04 Hopcroft-Karp" (world0 12))
  { skBack  = hsb 275 30 9
  , skStart = \_ -> randomGraph (world0 12)
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

world0 :: Int -> World
world0 n = World n (replicate n []) (replicate n (-1)) (replicate n (-1)) (replicate n inf) (replicate n inf) inf [] Bfs 0 0 False 0 ""

randomGraph :: World -> IO World
randomGraph w = do
  let n = wN w
      nf = fromIntegral n :: Float
      -- ★ 2.6 / n = 線を引く確率（1 人あたり平均 2.6 本）
      linked i j = do
        r <- randomRIO (0, 1)
        if r < 2.6 / nf
          then pure True
          else if i == j then (< (0.5 :: Float)) <$> randomRIO (0, 1) else pure False
  adj <- forM [0 .. n - 1] $ \i -> do
    bs <- mapM (linked i) [0 .. n - 1]
    case [j | (j, True) <- zip [0 ..] bs] of
      [] -> (: []) <$> randomRIO (0, n - 1)
      a -> pure a
  -- 最初に「欲張り」で少しだけマッチさせておく（こうすると長い増加道が出てくる）
  let greedy (l, r) (i, j)
        | l !! i >= 0 || r !! j >= 0 = pure (l, r)
        | otherwise = do
            x <- randomRIO (0, 1)
            pure (if x < greedyChance then (setAt i j l, setAt j i r) else (l, r))
  (l, r) <- foldM greedy (replicate n (-1), replicate n (-1)) [(i, j) | (i, a) <- zip [0 ..] adj, j <- a]
  pure (world0 n)
    { wAdj = adj, wMatchL = l, wMatchR = r, wAuto = wAuto w
    , wTimer = wTimer w, wMessage = "Space: layering for phase 1 (BFS)" }

-- 幅優先探索で層をつける。戻り値は (distL, distR, 最短の増加道の長さ)
bfsLayers :: World -> ([Int], [Int], Int)
bfsLayers w = go free distL0 (replicate n inf) inf
  where
    n = wN w
    mL = wMatchL w
    mR = wMatchR w
    free = [i | (i, m) <- zip [0 ..] mL, m < 0]
    distL0 = [if m < 0 then 0 else inf | m <- mL]
    go [] dl dr ml = (dl, dr, ml)
    go (i : q) dl dr ml
      | dl !! i >= ml = go q dl dr ml
      | otherwise = scan i (wAdj w !! i) q dl dr ml
    scan _ [] q dl dr ml = go q dl dr ml
    scan i (j : js) q dl dr ml
      | mL !! i == j || dr !! j < inf = scan i js q dl dr ml
      | otherwise =
          let d = dl !! i + 1
              dr' = setAt j d dr
              k = mR !! j
          in if k < 0
               then scan i js q dl dr' (min ml d)
               else if dl !! k == inf
                 then scan i js (q ++ [k]) (setAt k (d + 1) dl) dr' ml
                 else scan i js q dl dr' ml

-- 深さ優先探索の途中経過
data Dfs = Dfs { dUsedR :: [Bool], dDeadL :: [Bool], dPaths :: [[Link]] }

-- 層に沿って、点がかぶらない最短の増加道を集める（深さ優先）
collectPaths :: World -> [[Link]]
collectPaths w = reverse (dPaths (foldl start (Dfs (replicate n False) (replicate n False) []) [0 .. n - 1]))
  where
    n = wN w
    mL = wMatchL w
    mR = wMatchR w
    dl = wDistL w
    dr = wDistR w
    start st i
      | mL !! i < 0 = snd (dfs i [] st)
      | otherwise = st
    dfs :: Int -> [Link] -> Dfs -> (Bool, Dfs)
    dfs i acc st = tryAll i acc (wAdj w !! i) st
    tryAll i _ [] st = (False, st { dDeadL = setAt i True (dDeadL st) })
    tryAll i acc (j : js) st
      | dUsedR st !! j || mL !! i == j || dr !! j /= dl !! i + 1 = tryAll i acc js st
      | otherwise =
          let st1 = st { dUsedR = setAt j True (dUsedR st) }
              k = mR !! j
          in if k < 0
               then if dr !! j == wMaxLayer w
                      then (True, st1 { dPaths = (acc ++ [(i, j, False)]) : dPaths st1 })
                      else tryAll i acc js st1
               else if dDeadL st1 !! k
                 then tryAll i acc js st1
                 else case dfs k (acc ++ [(i, j, False), (k, j, True)]) st1 of
                   (True, st2) -> (True, st2)
                   (False, st2) -> tryAll i acc js st2

matchingSize :: World -> Int
matchingSize w = length (filter (>= 0) (wMatchL w))

advance :: World -> World
advance w = case wPhase w of
  Bfs ->
    let (dl, dr, ml) = bfsLayers w
    in if ml >= inf
         then w { wDistL = dl, wDistR = dr, wMaxLayer = ml, wPhase = Finished, wAuto = False
                , wMessage = "No more augmenting paths -> maximum matching " ++ show (matchingSize w) ++ " pairs (" ++ show (wPhases w) ++ " phases)" }
         else
           let ph = wPhases w + 1
           in w { wDistL = dl, wDistR = dr, wMaxLayer = ml, wPhases = ph, wPhase = Paths
                , wMessage = "Phase " ++ show ph ++ ": numbers = layers (distance). Shortest augmenting path length = " ++ show ml }
  Paths ->
    let ps = collectPaths w
    in w { wPaths = ps, wPhase = Flip
         , wMessage = "Found " ++ show (length ps) ++ " disjoint shortest augmenting paths at once (colors)" }
  Flip ->
    let flipOne (l, r) p =
          let clear (a, b) (i, j, m) = if m then (setAt i (-1) a, setAt j (-1) b) else (a, b)
              pair (a, b) (i, j, m) = if m then (a, b) else (setAt i j a, setAt j i b)
          in foldl pair (foldl clear (l, r) p) p
        (l1, r1) = foldl flipOne (wMatchL w, wMatchR w) (wPaths w)
        w1 = w { wMatchL = l1, wMatchR = r1, wAugments = wAugments w + length (wPaths w) }
        n = wN w
    in w1 { wMessage = "Flipped " ++ show (length (wPaths w)) ++ " paths -> " ++ show (matchingSize w1) ++ " pairs. Next phase"
          , wPaths = [], wDistL = replicate n inf, wDistR = replicate n inf, wPhase = Bfs }
  Finished -> w

stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wAuto w =
      let t = wTimer w + 1
          w' = w { wTimer = t }
      in pure (if t `mod` 50 == 0 then advance w' else w')
  | otherwise = pure w

drawW :: Env -> World -> Picture
drawW env w =
  let n = wN w
      lx = envW env * 0.32
      rx = envW env * 0.68
      yy :: Int -> Float
      yy i = remap (fromIntegral i) 0 (fromIntegral (n - 1)) (envH env * 0.15) (envH env * 0.86)
      links = [ seg env (if m then hsb 195 80 100 else hsb 275 15 40) (if m then 6 else 1.5) (lx, yy i) (rx, yy j)
              | (i, a) <- zip [0 ..] (wAdj w), j <- a, let m = wMatchL w !! i == j ]
      pathPics = [ (if m then dashed else seg) env (hsb (wrapF 360 (fromIntegral k * 67 + 40)) 85 100) 5 (lx, yy i) (rx, yy j)
                 | (k, p) <- zip [0 :: Int ..] (wPaths w), (i, j, m) <- p ]
      dots xs matches dists names side =
        [ drawDot env x (yy i) (names ++ show i) (m >= 0) d side
        | (i, m, d) <- zip3 [0 ..] matches dists, x <- [xs] ]
  in Pictures
       [ Pictures links
       , Pictures pathPics
       , Pictures (dots lx (wMatchL w) (wDistL w) "L" (-34))
       , Pictures (dots rx (wMatchR w) (wDistR w) "R" 34)
       , txt env 20 36 18 (lum 100) ("Space: 1 step  A: auto  Up/Down: nodes " ++ show n ++ "  R: random")
       , txt env 20 62 15 (lum 65) "Blue = matching. Numbers = layer (distance from free left nodes). Dashed = pair to undo"
       , txt env 20 (envH env - 26) 18 (hsb 45 80 100) (wMessage w)
       , txtR env (envW env - 30) 110 22 (lum 100) ("Matching " ++ show (matchingSize w) ++ " pairs")
       , txtR env (envW env - 30) 140 17 (lum 75) ("Phases " ++ show (wPhases w))
       , txtR env (envW env - 30) 166 17 (lum 75) ("Paths " ++ show (wAugments w))
       , txtR env (envW env - 30) 192 15 (lum 75) "In 03: one path = 1 round"
       ]

drawDot :: Env -> Float -> Float -> String -> Bool -> Int -> Float -> Picture
drawDot env x y s on d side = Pictures
  [ dot env (if on then hsb 195 60 95 else hsb 275 10 85) (x, y) 34
  , txtC env x y 14 (lum 10) s
  , if d < inf then txtC env (x + side) y 16 (hsb 45 90 100) (show d) else Blank
  ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (advance w)
  EventKey (SpecialKey KeyUp) Down _ _ -> randomGraph w { wN = min 20 (wN w + 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> randomGraph w { wN = max 4 (wN w - 2) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> randomGraph w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 点を 20 にして、フェーズ回数と増加道の本数を比べる（フェーズ ≪ 本数）
-- 2. フェーズが進むほど、最短の増加道は長くなる（層の数字が大きくなる）
-- 3. 最初の「欲張り」マッチ（greedyChance）を 0 にすると？

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 点線（実線 10、すき間 8 をくり返す）
dashed :: Env -> Color -> Float -> Point -> Point -> Picture
dashed env c t (x1, y1) (x2, y2) =
  let l = dist2 (x1, y1) (x2, y2)
      at' d = let q = d / max 1e-6 l in (lerpF x1 x2 q, lerpF y1 y2 q)
  in Pictures [seg env c t (at' d) (at' (min l (d + 10))) | d <- takeWhile (< l) [0, 18 ..]]

-- 中心 (x, y)、直径 d の円（p5.js の circle）
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

-- 文字：txt は左下が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = label (gx env x - fromIntegral (length str) * s * 0.72) (gy env y) s c str

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0

-- リストの i 番目だけ置きかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]
