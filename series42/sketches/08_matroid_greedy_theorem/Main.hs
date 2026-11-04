-- ============================================
-- 42-08 貪欲法の定理
-- 同じ集合族にランダムな重みを何度も与え、「貪欲解」と「全列挙の最適解」を比べて統計をとる
--   定理：集合族が マトロイド ⇔ どんな重みでも貪欲法が最適（Rado–Edmonds）
--   1：グラフ的マトロイド（K4 の全域木）は 100% 一致。2：ナップサック、3：マッチング は一致しない回が出る
-- 操作：スペース = 1 回試す / A = 自動（毎フレーム 10 回） / 1〜3 = 集合族を切替 / R = 統計をリセット
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Bits (testBit)
import Data.List (intercalate, nub, sortOn)
import Numeric (showFFloat)
import qualified Data.IntMap.Strict as IM

capacity :: Int
capacity = 10            -- ★ ナップサックの容量

data Elem = Elem { eName :: String, eWt, eVal, eA, eB :: !Int }

-- 直近の試行
data Last = Last { lGreedy, lBest :: ![Int], lGV, lBV :: !Int }

data World = World
  { wKind    :: !Int     -- 1 = K4 の森, 2 = ナップサック, 3 = マッチング
  , wElems   :: ![Elem]
  , wTrials  :: !Int
  , wMatches :: !Int
  , wBins    :: ![Int]   -- 貪欲／最適 の比のヒストグラム（0.5〜1.0、20 本）
  , wAuto    :: !Bool
  , wLast    :: !(Maybe Last)
  }

main :: IO ()
main = runSketch (sketch "42-08 Greedy theorem" (build world0))
  { skBack  = hsb 50 25 9
  , skStep  = \_ _ w -> if wAuto w then foldM (\x _ -> trial x) w [1 .. 10 :: Int] else pure w
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 [] 0 0 (replicate 20 0) False Nothing

noElem :: Elem
noElem = Elem "" 0 0 0 0

build :: World -> World
build w = resetStats w { wElems = case wKind w of
  1 -> [ Elem ("e" ++ show k) 0 0 a b   -- K4：4 頂点、6 辺。独立 = 閉路なし
       | (k, (a, b)) <- zip [1 :: Int ..] [ (a, b) | a <- [0 .. 3], b <- [a + 1 .. 3] ] ]
  2 -> [ Elem ("item" ++ show k) wt 0 0 0  -- ナップサック：6 品。重さは固定、価値がランダム
       | (k, wt) <- zip [1 :: Int ..] [2, 3, 4, 5, 6, 7] ]
  _ -> [ Elem ("e" ++ show k) 0 0 a b     -- マッチング：6 頂点の輪＋対角線 2 本
       | (k, (a, b)) <- zip [1 :: Int ..] [(0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, 0), (0, 3), (1, 4)] ] }

resetStats :: World -> World
resetStats w = w { wTrials = 0, wMatches = 0, wBins = replicate 20 0, wLast = Nothing, wAuto = False }

graphNodes :: Env -> Int -> [Point]
graphNodes env kind =
  let cx = envW env * 0.25
      cy = envH env * 0.6
  in if kind == 1
       then [ (cx + 110 * cos a, cy + 110 * sin a) | i <- [0 .. 3 :: Int], let a = tau * fromIntegral i / 4 + pi / 4 ]
       else [ (cx + 120 * cos a, cy + 120 * sin a) | i <- [0 .. 5 :: Int], let a = tau * fromIntegral i / 6 - pi / 2 ]

-- ★ 独立の判定（3 つの集合族）
isIndep :: World -> [Int] -> Bool
isIndep w sub =
  let el = pick noElem (wElems w)
  in case wKind w of
       1 -> isForest 4 (map (\i -> (eA (el i), eB (el i))) sub)
       2 -> sum (map (eWt . el) sub) <= capacity
       _ -> let vs = concatMap (\i -> [eA (el i), eB (el i)]) sub in length (nub vs) == length vs

-- ★ 1 回の試行：ランダムな価値 → 貪欲法と全列挙
trial :: World -> IO World
trial w0 = do
  es <- mapM (\e -> (\v -> e { eVal = v }) <$> randomRIO (1, 19)) (wElems w0)
  let w = w0 { wElems = es }
      n = length es
      val i = eVal (pick noElem es i)
      valueOf = sum . map val
      order = sortOn (negate . val) [0 .. n - 1]
      greedy = foldl' (\g i -> if isIndep w (g ++ [i]) then g ++ [i] else g) [] order
      subs = [ [ i | i <- [0 .. n - 1], testBit m i ] | m <- [0 .. (2 :: Int) ^ n - 1] ]
      (bestV, best) = foldl' (\(bv, bs) s -> if isIndep w s && valueOf s > bv then (valueOf s, s) else (bv, bs)) (-1, []) subs
      gv = valueOf greedy
      ratio = if bestV > 0 then fromIntegral gv / fromIntegral bestV else 1 :: Float
      bin = max 0 (min 19 (floor (remap ratio 0.5 1.0001 0 20))) :: Int
  pure w { wTrials = wTrials w + 1
         , wMatches = wMatches w + (if gv == bestV then 1 else 0)
         , wBins = setAt bin (+ 1) (wBins w)
         , wLast = Just (Last greedy best gv bestV) }

draw :: Env -> World -> Picture
draw env w =
  let trials = wTrials w
      px = envW env * 0.5
      py = 130
      pw = envW env - px - 60
      ph = 300
      rate = if trials > 0 then fromIntegral (wMatches w) / fromIntegral trials else 0 :: Float
      titleC | trials == 0 = white
             | rate == 1 = hsb 130 80 100
             | otherwise = hsb 5 80 100
      -- ヒストグラム
      hy = py + 60
      hh = ph - 80
      mx = foldl' max 1 (wBins w)
      bw = pw / 20
      bar b c =
        let h = fromIntegral c / fromIntegral mx * hh
            x = px + fromIntegral b * bw
            lift = if odd b then 16 else 0     -- 数字が重ならないよう 1 本おきに上へ
        in Pictures $
             [ box env (hsb (if b == 19 then 130 else 5) 70 90) (x + 2) (hy + hh - h) (bw - 4) h ] ++
             [ txtC env (x + bw / 2) (hy + hh - h - 10 - lift) 14 white (show c) | c > 0 ]
      names = intercalate " " . map (eName . pick noElem (wElems w))
      lastRows = case wLast w of
        Just l ->
          [ (15, white, "Last greedy {" ++ names (lGreedy l) ++ "} = " ++ show (lGV l))
          , (15, white, "Last best {" ++ names (lBest l) ++ "} = " ++ show (lBV l) ++ (if lGV l == lBV l then "  match" else "  [x]")) ]
        Nothing -> [ (15, white, ""), (15, white, "") ]
  in Pictures $
       [ if wKind w == 2 then drawKnapsack env w else drawGraph env w
       , txt env 20 40 16 white ("1: K4 trees (matroid)  2: knapsack  3: matching  Space: once  A: auto " ++ (if wAuto w then "ON" else "off") ++ "  R: reset")
       , txt env 20 66 14 (gray 0.6) "Random values: greedy vs brute-force best. Green = greedy's choice, yellow = the optimum."
       , box env (withA 0.08 white) (px - 20) (py - 40) (pw + 40) (ph + 250)
       , stack env px py
           [ (22, titleC, if trials > 0
                            then "Match " ++ f1 (rate * 100) ++ "%  (" ++ show (wMatches w) ++ " / " ++ show trials ++ ")"
                            else "Press space to try")
           , (14, gray 0.7, if wKind w == 1 then "A matroid: always matches (theorem)" else "Some misses -> not a matroid") ]
       , seg env 1 (gray 0.35) (px, hy + hh) (px + pw, hy + hh) ] ++
       zipWith bar [0 :: Int ..] (wBins w) ++
       [ txt env px (hy + hh + 20) 14 (gray 0.7) "greedy/best = 0.5"
       , txt env (px + pw - 100) (hy + hh + 20) 14 (gray 0.7) "1.0 (match)"
       , stack env px (hy + hh + 50)
           (lastRows ++
            [ (6, white, "")
            , (14, gray 0.7, "Theorem (Rado 1957 / Edmonds 1971): for a family")
            , (14, gray 0.7, "with (I1)(I2), greedy gives a maximum-weight")
            , (14, gray 0.7, "independent set for all weights <=> it is a matroid") ])
       ]

drawGraph :: Env -> World -> Picture
drawGraph env w =
  let ns = graphNodes env (wKind w)
      node = pick (0, 0) ns
      g = maybe [] lGreedy (wLast w)
      b = maybe [] lBest (wLast w)
      edgePic i e =
        let p = node (eA e)
            q = node (eB e)
            inG = i `elem` g
            (mx, my) = ((fst p + fst q) / 2, (snd p + snd q) / 2)
        in Pictures $
             [ seg env 14 (hsb 50 90 100) p q | i `elem` b ] ++
             [ seg env (if inG then 6 else 2.5) (if inG then hsb 130 85 100 else gray 0.4) p q
             , txtC env (mx + 12) (my - 12) 14 white (eName e ++ (case wLast w of { Just _ -> " (" ++ show (eVal e) ++ ")"; Nothing -> "" })) ]
  in Pictures (zipWith edgePic [0 :: Int ..] (wElems w) ++ map (ball env 13 (hsb 50 40 85)) ns)

drawKnapsack :: Env -> World -> Picture
drawKnapsack env w =
  let x0 = 40
      y0 = envH env * 0.6
      bw = 66
      g = maybe [] lGreedy (wLast w)
      b = maybe [] lBest (wLast w)
      item i e =
        let h = fromIntegral (eWt e) * 16
            x = x0 + fromIntegral i * (bw + 26)
            cx = x + bw / 2
            inG = i `elem` g
        in Pictures $
             [ Pictures [ box env (hsb 50 90 100) (x - 8) (y0 - h - 8) (bw + 16) (h + 16)
                        , box env (hsb 50 25 9) (x - 4) (y0 - h - 4) (bw + 8) (h + 8) ] | i `elem` b ] ++
             [ box env (gray 0.6) (x - 1) (y0 - h - 1) (bw + 2) (h + 2)
             , box env (if inG then hsb 130 70 75 else gray 0.3) x (y0 - h) bw h
             , txtC env cx (y0 + 18) 14 white (eName e)
             , txtC env cx (y0 - h - 34) 14 white ("wt " ++ show (eWt e)) ] ++
             [ txtC env cx (y0 - h - 16) 14 (hsb 50 80 100) ("val " ++ show (eVal e)) | Just _ <- [wLast w] ]
  in Pictures (zipWith item [0 :: Int ..] (wElems w) ++ [txt env x0 (y0 + 46) 14 (gray 0.7) ("Capacity " ++ show capacity)])

event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> trial w
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> pure (resetStats w)
    | c `elem` "123" -> pure (build w { wKind = fromEnum c - fromEnum '0' })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 1 を自動で 1000 回まわして 100% を確かめる。2・3 は何%くらい？ 容量（capacity）を変えると？
-- 2. 価値を 1〜19 でなく 1〜3 にすると（trial の randomRIO）、同点が増えて一致率はどう変わる？
-- 3. K4 でなく 01 のランダムなグラフにしても 100% になるか試す（wElems と isIndep を 01 から持ってくる）
-- 4. 「貪欲法が最適 ⇔ マトロイド」。逆向き（最適なら必ずマトロイド）は、反例から交換公理の違反を作る証明

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

-- 小数 1 桁の文字列
f1 :: Float -> String
f1 x = showFFloat (Just 1) x ""

-- union-find：親の表を IntMap で持つ
ufNew :: Int -> IM.IntMap Int
ufNew n = IM.fromDistinctAscList [ (i, i) | i <- [0 .. n - 1] ]

ufFind :: IM.IntMap Int -> Int -> Int
ufFind uf x = case IM.lookup x uf of
  Just p | p /= x -> ufFind uf p
  _ -> x

-- 2 つの木をつなぐ。もう同じ木なら Nothing（この辺で閉路ができる）
ufUnion :: IM.IntMap Int -> (Int, Int) -> Maybe (IM.IntMap Int)
ufUnion uf (a, b) =
  let ra = ufFind uf a
      rb = ufFind uf b
  in if ra == rb then Nothing else Just (IM.insert ra rb uf)

-- 辺のリストが森か（閉路がないか）
isForest :: Int -> [(Int, Int)] -> Bool
isForest n es = case foldM ufUnion (ufNew n) es of
  Just _ -> True
  Nothing -> False
