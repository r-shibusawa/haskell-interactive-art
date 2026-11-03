-- ============================================
-- 02 エドモンズ＝カープ法と最小カット
-- 道の「選び方」で回数が大違い。左：運の悪い選び方（真ん中の細い線を毎回通る）→ 2C 回
-- 右：幅優先探索で「いちばん短い道」を選ぶ（エドモンズ＝カープ法）→ 2 回。どんな地図でも O(VE²) 回で終わる
-- スペース = 再生・停止 / 上下 = 容量 C を変える / R = 最初から
-- 終わると右に最小カット（s 側と t 側を分ける線）が出る。カットの容量 = 最大フロー
-- ============================================
module Main (main) where

import Art
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Maybe (fromMaybe)

capacities :: [Int]
capacities = [10, 100, 1000, 10000]  -- ★ 上下キーで選べる容量 C

data Edge = Edge { eU :: !Int, eV :: !Int, eC :: !Int, eF :: !Int }

data RArc = RArc { aE :: !Int, aDir :: !Int, aFrom :: !Int, aTo :: !Int, aR :: !Int }

data Net = Net
  { nCx :: !Float            -- 中心の x（画面の幅に対する割合）
  , nBad :: !Bool            -- 運の悪い選び方をするか
  , nEdges :: [Edge]
  , nIters :: !Int
  , nDone :: !Bool
  , nLast :: [RArc]
  , nReach :: [Bool]
  }

data World = World { wLeft :: !Net, wRight :: !Net, wCi :: !Int, wPlaying :: !Bool }

main :: IO ()
main = runSketch (sketch "15-02 Edmonds-Karp" (reset 1))
  { skBack  = hsb 240 35 9
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

capOf :: Int -> Int
capOf i = case drop i capacities of
  (c : _) -> c
  [] -> 10

makeNet :: Int -> Float -> Bool -> Net
makeNet ci cx bad =
  let c = capOf ci
  in Net cx bad [Edge 0 1 c 0, Edge 0 2 c 0, Edge 1 2 1 0, Edge 1 3 c 0, Edge 2 3 c 0] 0 False [] []

reset :: Int -> World
reset ci = World (makeNet ci 0.27 True) (makeNet ci 0.73 False) ci False

arcs :: Net -> [RArc]
arcs net = concat
  [ [RArc i 1 (eU e) (eV e) (eC e - eF e) | eC e - eF e > 0] ++ [RArc i (-1) (eV e) (eU e) (eF e) | eF e > 0]
  | (i, e) <- zip [0 ..] (nEdges net) ]

-- 幅優先探索：辺の数がいちばん少ない増加道。戻り値は（道、s から行ける点）
bfsPath :: Net -> (Maybe [RArc], [Bool])
bfsPath net =
  let as = arcs net
      go [] seen prev = (seen, prev)
      go (v : q) seen prev =
        let (seen', prev', new) = foldl visit (seen, prev, []) [a | a <- as, aFrom a == v]
        in go (q ++ reverse new) seen' prev'
      visit (seen, prev, new) a
        | IS.member (aTo a) seen = (seen, prev, new)
        | otherwise = (IS.insert (aTo a) seen, IM.insert (aTo a) a prev, aTo a : new)
      (seenF, prevF) = go [0] (IS.singleton 0) IM.empty
      reach = [IS.member i seenF | i <- [0 .. 3]]
      back v acc
        | v == 0 = acc
        | otherwise = case IM.lookup v prevF of
            Just a -> back (aFrom a) (a : acc)
            Nothing -> acc
  in (if IS.member 3 seenF then Just (back 3 []) else Nothing, reach)

-- 運の悪い選び方：真ん中の線（a→b か、その逆向き）を必ず使う道を選ぶ
badPath :: Net -> Maybe [RArc]
badPath net =
  let as = arcs net
      find' seq' = mapM (\(p, q) -> firstOf [a | a <- as, aFrom a == p, aTo a == q]) (zip seq' (drop 1 seq'))
      firstOf (a : _) = Just a
      firstOf [] = Nothing
  in case find' [0, 1, 2, 3] of
       Just p -> Just p
       Nothing -> case find' [0, 2, 1, 3] of
         Just p -> Just p
         Nothing -> fst (bfsPath net)

augmentOnce :: Net -> Net
augmentOnce net
  | nDone net = net
  | otherwise =
      let p = if nBad net then badPath net else fst (bfsPath net)
      in case p of
           Nothing -> net { nDone = True, nReach = snd (bfsPath net) }
           Just path ->
             let b = foldr (min . aR) maxBound path
                 apply es a = [if i == aE a then e { eF = eF e + aDir a * b } else e | (i, e) <- zip [0 ..] es]
             in net { nEdges = foldl apply (nEdges net) path, nIters = nIters net + 1, nLast = path }

value :: Net -> Int
value net = sum [eF e | e <- nEdges net, eU e == 0]

stepW :: Env -> Float -> World -> IO World
stepW env _ w
  | wPlaying w =
      -- 運の悪い方は回数が多いので、1 フレームに何回か進める
      let k = max 1 (capOf (wCi w) `div` 200)
          r = if envFrame env `mod` 20 == 0 then augmentOnce (wRight w) else wRight w
          l = iterate augmentOnce (wLeft w) !! k
      in pure w { wLeft = l, wRight = r, wPlaying = not (nDone l && nDone r) }
  | otherwise = pure w

drawW :: Env -> World -> Picture
drawW env w =
  let c = capOf (wCi w)
  in Pictures
       [ seg env (lum 30) 1 (envW env / 2, 90) (envW env / 2, envH env - 20)
       , drawNet env c (wLeft w) "Unlucky choice (DFS through the middle)" (hsb 0 70 95)
       , drawNet env c (wRight w) "Edmonds-Karp (BFS = shortest path)" (hsb 160 70 90)
       , txt env 20 36 18 (lum 100)
           ("Space: " ++ (if wPlaying w then "stop" else "play") ++ "   Up/Down: capacity C = " ++ show c ++ "   R: restart")
       , txt env 20 62 15 (lum 65) "The middle pipe has capacity 1. Going back and forth over it adds only 1 each time"
       ]

drawNet :: Env -> Int -> Net -> String -> Color -> Picture
drawNet env c net title col =
  let cx = envW env * nCx net
      hw = envW env * 0.2
      hh = envH env * 0.22
      cy = envH env * 0.5
      ps = [(cx - hw, cy), (cx, cy - hh), (cx, cy + hh), (cx + hw, cy)]
      names = ["s", "a", "b", "t"]
      reachAt i = case drop i (nReach net) of
        (b : _) -> b
        [] -> False
      -- 最小カット：s 側（残余ネットワークで s から行ける点）を緑の雲で囲む
      clouds = if nDone net then [dot env (hsba 140 60 60 0.25) p 130 | (i, p) <- zip [0 ..] ps, reachAt i] else []
      edgePic (i, e) =
        let (ax0, ay0) = ps !! eU e
            (bx0, by0) = ps !! eV e
            onPath = any ((== i) . aE) (nLast net) && not (nDone net)
            cut = nDone net && reachAt (eU e) && not (reachAt (eV e))
            ang = atan2 (by0 - ay0) (bx0 - ax0)
            a@(ax, ay) = (ax0 + cos ang * 28, ay0 + sin ang * 28)
            b@(bx, by) = (bx0 - cos ang * 30, by0 - sin ang * 30)
            r = fromIntegral (eF e) / fromIntegral (eC e) :: Float
            thin = eC e == 1
            flowC | cut = hsb 45 90 100
                  | onPath = hsb 50 90 100
                  | otherwise = hsb 195 80 95
            q = max r 0.02
            mx = (ax + bx) / 2 - sin ang * 20
            my = (ay + by) / 2 + cos ang * 20
        in Pictures
             [ seg env (hsb 240 15 30) (if thin then 3 else 14) a b
             , if eF e > 0 || cut
                 then seg env flowC (if thin then 3 else 2 + 12 * r) a
                        (if cut then b else (lerpF ax bx q, lerpF ay by q))
                 else Blank
             , tip env (lum 85) b ang 14 0.45
             , txtC env mx my 14 (if cut then hsb 45 90 100 else lum 95) (show (eF e) ++ "/" ++ show (eC e))
             ]
      nodePic (p@(x, y), s) =
        let fc | s == "s" = hsb 140 70 85
               | s == "t" = hsb 20 75 95
               | otherwise = hsb 240 15 88
        in Pictures [dot env fc p 50, txtC env x y 18 (lum 10) s]
      h = envH env
  in Pictures
       [ Pictures clouds
       , Pictures (map edgePic (zip [0 ..] (nEdges net)))
       , Pictures (map nodePic (zip ps names))
       , txtC env cx 120 18 col title
       , txtC env cx (h - 122) 40 (lum 100) (show (nIters net) ++ " rounds")
       , txtC env cx (h - 80) 17 (lum 80)
           ("Flow value " ++ show (value net) ++ (if nDone net then "  (max = 2C = " ++ show (2 * c) ++ ")" else ""))
       , if nDone net
           then txtC env cx (h - 50) 16 (hsb 45 90 100) ("Yellow = min cut. Capacity total " ++ show (2 * c) ++ " = max flow")
           else Blank
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> reset (min (length capacities - 1) (wCi w + 1))
  EventKey (SpecialKey KeyDown) Down _ _ -> reset (max 0 (wCi w - 1))
  EventKey (Char c) Down _ _ | c `elem` "rR" -> reset (wCi w)
  _ -> w

-- ---------- やってみよう ----------
-- 1. C を 10000 にする。左は 20000 回、右は 2 回。容量が無理数だと左は終わらないことさえある
-- 2. 最小カットの線（黄）の容量を足すと、最大フローと必ず同じになる（最大フロー最小カット定理）
-- 3. makeNet の真ん中の線の容量を 1 から c に変えると、左の回数はどうなる？

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

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
