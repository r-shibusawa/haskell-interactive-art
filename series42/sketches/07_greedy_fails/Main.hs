-- ============================================
-- 42-07 貪欲法が失敗する構造
-- 06 と同じ貪欲法（価値の高い順に、入れられるなら入れる）を「マトロイドでない集合族」に使うと最適解を逃す
--   1：ナップサック（重さの合計 ≤ 容量 なら独立）　2：マッチング（辺が頂点を共有しなければ独立）
--   どちらも (I1)(I2) は満たすが (I3) 交換公理が壊れている。その「壊れている場所」を右下に表示
-- 操作：1・2 = 問題を切替 / スペース = 1 ステップ / A = 自動 / R = 最初から（重みをランダムに） / ↑↓ = 容量（1 のとき）
-- ============================================
module Main (main) where

import Art
import Data.Bits (testBit)
import Data.List (intercalate, nub, sortOn)

capacity0 :: Int
capacity0 = 10           -- ★ ナップサックの容量

data Elem = Elem { eName :: String, eWt, eVal, eA, eB :: !Int }

data St = Untried | Taken | Dropped deriving (Eq)

data World = World
  { wKind  :: !Int           -- 1 = ナップサック, 2 = マッチング
  , wElems :: ![Elem]
  , wCap   :: !Int
  , wOrder :: ![Int]
  , wStepI :: !Int
  , wStat  :: ![St]
  , wAuto  :: !Bool
  , wTimer :: !Int
  , wBestV :: !Int
  , wBestS :: ![Int]
  , wViol  :: !(Maybe ([Int], [Int]))   -- 交換公理の反例 (A, B)
  }

main :: IO ()
main = runSketch (sketch "42-07 When greedy fails" world0)
  { skBack  = hsb 340 25 9
  , skStart = \_ -> build world0
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 [] capacity0 [] 0 [] False 0 0 [] Nothing

-- ★ マッチング用：6 頂点の輪＋対角線のグラフ。辺が要素
matchEdges :: [(Int, Int)]
matchEdges = [(0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, 0), (0, 3), (1, 4)]

matchNodes :: Env -> [Point]
matchNodes env =
  let cx = envW env * 0.3
      cy = envH env * 0.55
      r = min (envW env) (envH env) * 0.24
      a i = tau * fromIntegral i / 6 - pi / 2
  in [ (cx + r * cos (a i), cy + r * sin (a i)) | i <- [0 .. 5 :: Int] ]

build :: World -> IO World
build w = do
  es <- if wKind w == 1
    then mapM (\i -> do    -- ★ 品物：重さ、価値
                 wt <- randomRIO (2, 7)
                 v <- randomRIO (3, 14)
                 pure (Elem ("item" ++ show (i + 1 :: Int)) wt v 0 0)) [0 .. 5]
    else mapM (\(i, (a, b)) -> do
                 v <- randomRIO (3, 14)
                 pure (Elem ("e" ++ show (i + 1 :: Int)) 1 v a b)) (zip [0 :: Int ..] matchEdges)
  pure (reset w { wElems = es })

-- ★ 独立かどうか（この判定だけが 06 とちがう）
isIndep :: World -> [Int] -> Bool
isIndep w sub =
  let el = pick (Elem "" 0 0 0 0) (wElems w)
  in if wKind w == 1
       then sum (map (eWt . el) sub) <= wCap w
       else let vs = concatMap (\i -> [eA (el i), eB (el i)]) sub in length (nub vs) == length vs

valueOf :: World -> [Int] -> Int
valueOf w = sum . map (eVal . pick (Elem "" 0 0 0 0) (wElems w))

reset :: World -> World
reset w =
  let es = wElems w
      n = length es
      subs = [ [ i | i <- [0 .. n - 1], testBit m i ] | m <- [0 .. (2 :: Int) ^ n - 1] ]
      -- 全列挙で最適解
      indeps = filter (isIndep w) subs
      (bestV, bestS) = foldl' (\(bv, bs) s -> let v = valueOf w s in if v > bv then (v, s) else (bv, bs)) (-1, []) indeps
      -- 交換公理の反例：|A| < |B| なのに B のどれを足しても従属になる A
      broken a b = not (any (\e -> e `notElem` a && isIndep w (a ++ [e])) b)
      better cur (a, b) = case cur of
        Just (x, y) | length a + length b >= length x + length y -> cur
        _ -> Just (a, b)
      viol = foldl' better Nothing
               [ (a, b) | a <- indeps, b <- indeps, length b > length a, broken a b ]
  in w { wOrder = sortOn (\i -> negate (eVal (pick (Elem "" 0 0 0 0) es i))) [0 .. n - 1]
       , wStat = map (const Untried) es
       , wStepI = 0, wAuto = False, wBestV = bestV, wBestS = bestS, wViol = viol }

greedyStep :: World -> World
greedyStep w
  | wStepI w >= length (wOrder w) = w
  | otherwise =
      let i = pick 0 (wOrder w) (wStepI w)
          chosen = [ j | j <- take (wStepI w) (wOrder w), pick Untried (wStat w) j == Taken ]
          ok = isIndep w (chosen ++ [i])
      in w { wStat = setAt i (const (if ok then Taken else Dropped)) (wStat w), wStepI = wStepI w + 1 }

step :: World -> World
step w
  | wAuto w =
      let t = wTimer w + 1
          w1 = w { wTimer = t }
      in if t `mod` 25 == 0 then greedyStep w1 else w1
  | otherwise = w

draw :: Env -> World -> Picture
draw env w =
  let es = wElems w
      n = length es
      st = pick Untried (wStat w)
      chosen = [ i | i <- [0 .. n - 1], st i == Taken ]
      done = wStepI w >= length (wOrder w)
      total = valueOf w chosen
      el = pick (Elem "" 0 0 0 0 ) es
      names = intercalate ", " . map (eName . el)
      px = envW env - 460
      py = 120
      row k i =
        let e = el i
            s = st i
            isCur = k == wStepI w
            c | s == Taken = hsb 130 75 100
              | s == Dropped = hsb 5 75 70
              | isCur = hsb 50 75 100
              | k > wStepI w = gray 0.6
              | otherwise = white
        in txt env px (py + 96 + fromIntegral k * 22) 14 c
             ((if isCur && not done then "> " else "   ") ++ eName e ++ "  val " ++ pad2 (eVal e)
               ++ (if wKind w == 1 then "  wt " ++ show (eWt e) else "  " ++ show (eA e + 1) ++ "-" ++ show (eB e + 1))
               ++ "  " ++ (case s of { Taken -> "taken"; Dropped -> "no fit: dropped"; Untried -> "" }))
      vy = py + 96 + fromIntegral (length (wOrder w)) * 22 + 22
      violText = case wViol w of
        Just (a, b) ->
          [ (14, gray 0.8, "A = {" ++ names a ++ "}")
          , (14, gray 0.8, "B = {" ++ names b ++ "}")
          , (14, gray 0.8, "|A| < |B|, yet no element of B fits A.")
          , (14, gray 0.8, if wKind w == 1 then "-> 2 light items vs 1 heavy one"
                                           else "-> 1 middle edge vs 2 side edges") ]
        Nothing -> [(14, gray 0.8, "No counterexample with these weights")]
  in Pictures $
       [ if wKind w == 1 then drawKnapsack env w chosen else drawMatching env w chosen
       , txt env 20 40 16 white ("1: knapsack  2: matching  Space: 1 step  A: auto " ++ (if wAuto w then "ON" else "off") ++ "  R: restart"
                                  ++ (if wKind w == 1 then "  Up/Down: capacity " ++ show (wCap w) else ""))
       , txt env 20 66 14 (gray 0.6) "Same greedy as 06: highest value first, take if it fits. But this family is not a matroid."
       , box env (withA 0.08 white) (px - 20) (py - 40) 470 (envH env - py + 20)
       , stack env px py
           [ (18, hsb (if done then (if total == wBestV w then 130 else 5) else 50) 80 100,
               if done then (if total == wBestV w then "Optimal this time" else "Greedy missed the optimum")
                       else "By value (" ++ show (wStepI w) ++ " / " ++ show n ++ ")")
           , (15, white, "Greedy value = " ++ show total ++ "   best = " ++ show (wBestV w))
           , (14, gray 0.7, "Best: {" ++ names (wBestS w) ++ "}") ] ] ++
       zipWith row [0 ..] (wOrder w) ++
       [ stack env px vy
           ([ (16, hsb 5 80 100, "Where (I3) exchange breaks") ] ++ violText ++
            [ (14, gray 0.6, "Matroid: maximal sets have equal size,")
            , (14, gray 0.6, "and greedy is always optimal (see 08).") ]) ]

drawKnapsack :: Env -> World -> [Int] -> Picture
drawKnapsack env w chosen =
  let x0 = 60
      y0 = envH env * 0.55
      bw = 70
      barW = envW env - 540
      el = pick (Elem "" 0 0 0 0) (wElems w)
      used = sum (map (eWt . el) chosen)
      ratio = min 1 (fromIntegral used / fromIntegral (wCap w))
      item i e =
        let on = i `elem` chosen
            s = pick Untried (wStat w) i
            h = fromIntegral (eWt e) * 14
            x = x0 + fromIntegral i * (bw + 30)
            border = if s == Dropped then hsb 5 70 70 else gray 0.6
            cx = x + bw / 2
        in Pictures
             [ box env border (x - 1) (y0 + 100 - h - 1) (bw + 2) (h + 2)
             , box env (if on then hsb 130 70 75 else gray 0.3) x (y0 + 100 - h) bw h
             , txtC env cx (y0 + 118) 14 white (eName e)
             , txtC env cx (y0 + 100 - h - 32) 14 white ("wt " ++ show (eWt e))
             , txtC env cx (y0 + 100 - h - 14) 14 (hsb 50 80 100) ("val " ++ show (eVal e)) ]
  in Pictures $
       [ box env (gray 0.25) x0 (y0 + 160) barW 30
       , box env (hsb 130 70 80) x0 (y0 + 160) (barW * ratio) 30
       , txt env x0 (y0 + 215) 15 white ("Total weight " ++ show used ++ " / capacity " ++ show (wCap w)) ] ++
       zipWith item [0 :: Int ..] (wElems w)

drawMatching :: Env -> World -> [Int] -> Picture
drawMatching env w chosen =
  let ns = matchNodes env
      node = pick (0, 0) ns
      el = pick (Elem "" 0 0 0 0) (wElems w)
      usedV = concatMap (\i -> [eA (el i), eB (el i)]) chosen
      edgePic i e =
        let p = node (eA e)
            q = node (eB e)
            on = i `elem` chosen
            s = pick Untried (wStat w) i
            linePic | on = seg env 8 (hsb 130 85 100) p q
                    | s == Dropped = dashSeg env 2 6 6 (hsba 5 70 70 0.6) p q
                    | otherwise = seg env 2.5 (gray 0.45) p q
            (mx, my) = ((fst p + fst q) / 2, (snd p + snd q) / 2)
        in Pictures [ linePic, txtC env (mx + 14) (my - 12) 14 (hsb (if on then 130 else 50) 70 100) (eName e ++ " (" ++ show (eVal e) ++ ")") ]
      nodePic i p =
        let u = i `elem` usedV
        in Pictures [ ball env 15 (hsb (if u then 130 else 340) (if u then 60 else 30) 85) p
                    , txtC env (fst p) (snd p) 14 (gray 0.1) (show (i + 1)) ]
  in Pictures (zipWith edgePic [0 :: Int ..] (wElems w) ++ zipWith nodePic [0 :: Int ..] ns)

event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (greedyStep w)
  EventKey (SpecialKey KeyUp) Down _ _ | wKind w == 1 -> pure (reset w { wCap = min 30 (wCap w + 1) })
  EventKey (SpecialKey KeyDown) Down _ _ | wKind w == 1 -> pure (reset w { wCap = max 2 (wCap w - 1) })
  EventKey (Char c) Down _ _
    | c == '1' -> build w { wKind = 1 }
    | c == '2' -> build w { wKind = 2 }
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> build w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. ナップサックで R を何回か押し、貪欲が失敗する回と成功する回の割合を感じる。容量を大きくすると？
-- 2. 「価値／重さ」の順に並べ替えるとどうなる？（reset の sortOn を v / w に）。それでも失敗する例を探す
-- 3. マッチングで、真ん中の対角線 e7・e8 の価値を高くすると貪欲法がだまされる。ノードを増やしたら？
-- 4. どちらも「極大なのに大きさがちがう独立集合」がある。これがマトロイドでない印。03 の見本 4 と同じ

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標（中央が原点、y は上向き）へ
sp :: Env -> Point -> Point
sp env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- 文字（左下を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sp env (x, y) in label a b size c s

-- 文字（中央を指定）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = let (a, b) = sp env (x, y) in labelC a b size c s

-- 上から順に文字を並べる：(大きさ, 色, 文字)。空の文字は行間だけあける
stack :: Env -> Float -> Float -> [(Float, Color, String)] -> Picture
stack env x y0 rows = Pictures (go y0 rows)
  where go _ [] = []
        go y ((sz, c, s) : r) = txt env x y sz c s : go (y + sz * 1.5) r

-- 太さ w の線分
seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c p q = Color c (thickLine w (sp env p) (sp env q))

-- 破線（on 描いて off あける）
dashSeg :: Env -> Float -> Float -> Float -> Color -> Point -> Point -> Picture
dashSeg env w on off c p@(x1, y1) q@(x2, y2) =
  let l = dist2 p q
      n = floor (l / (on + off)) :: Int
      pt d = (x1 + (x2 - x1) * d / max 1e-6 l, y1 + (y2 - y1) * d / max 1e-6 l)
  in Pictures [ seg env w c (pt d) (pt (min l (d + on))) | k <- [0 .. n], let d = fromIntegral k * (on + off), d < l ]

-- 白いふちつきの丸（半径 r）
ball :: Env -> Float -> Color -> Point -> Picture
ball env r c p = at (sp env p) (Pictures [Color white (disc (r + 1)), Color c (disc r)])

-- 塗りつぶしの長方形（左上の角 (x, y)、幅 w、高さ h）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (a, b) = sp env (x + w / 2, y + h / 2) in Color c (rect a b w h)

-- リストの i 番目（範囲外なら d を返す安全版）
pick :: a -> [a] -> Int -> a
pick d xs i
  | i < 0 = d
  | otherwise = case drop i xs of { (x : _) -> x; [] -> d }

-- リストの i 番目だけ f で変える
setAt :: Int -> (a -> a) -> [a] -> [a]
setAt i f xs = [ if k == i then f x else x | (k, x) <- zip [0 ..] xs ]

-- 2 桁にそろえる（p5.js の nf(n, 2)）
pad2 :: Int -> String
pad2 n = (if n < 10 then "0" else "") ++ show n
