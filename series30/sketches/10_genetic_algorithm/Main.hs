-- ============================================
-- 10 遺伝的アルゴリズム（GA）─ 予算内で投資プロジェクトを選ぶ
-- 30 個のプロジェクトにはそれぞれ費用と効果がある。予算を超えずに効果の合計を最大にする組み合わせは？（2^30 ≒ 10 億通り）
--   1 つの答え = 0/1 の並び（遺伝子）。良い答えほど親に選ばれ、交叉（混ぜる）と突然変異（ときどき反転）で次の世代をつくる
-- スペース = 一時停止 / ↑↓ = 突然変異率 / ←→ = 集団の大きさ / C = 交叉 ON/OFF / E = エリート保存 / F = 早送り / R = やり直し / N = 新しい問題
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Numeric (showFFloat)
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (foldl', mapAccumL, sortBy)
import Data.Ord (comparing)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Random (StdGen, newStdGen, randomR)

nItems :: Int
nItems = 30                            -- ★ プロジェクトの数

-- | プロジェクト：費用 w、効果 v、色
data Item = Item { iW, iV :: !Int, iHue :: !Float }

-- | 個体：遺伝子（0/1）と効果の合計
data Ind = Ind { indG :: !(U.Vector Int), indFit :: !Int }

data World = World
  { wItems :: ![Item]
  , wWs, wVs :: !(U.Vector Int)        -- 費用と効果（速く読むため）
  , wOrder :: ![Int]                   -- 効果 / 費用 が悪い順（修復で使う）
  , wBudget :: !Int
  , wPopSize :: !Int                   -- ★ 集団の大きさ（個体の数）
  , wMutRate :: !Float                 -- ★ 1 ビットあたりの突然変異率
  , wUseCross, wElitism, wFast, wPaused :: !Bool
  , wPop :: !(V.Vector Ind)            -- 良い順に並べた集団
  , wGenN :: !Int
  , wLogBest, wLogMean :: !(Seq.Seq Float)   -- 成績の記録（600 世代分）
  , wDpBest, wGreedyBest :: !Int
  , wGen :: !StdGen
  }

main :: IO ()
main = runSketch (sketch "30-10 Genetic Algorithm" (error "skStart makes the world"))
  { skBack  = hsb 225 30 12
  , skStart = \_ -> newProblem . emptyWorld <$> newStdGen
  , skStep  = \_ _ w -> pure (if wPaused w then w else iterateN (if wFast w then 10 else 1) nextGeneration w)
  , skDraw  = drawAll
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ !x = x
iterateN n f !x = iterateN (n - 1) f (f x)

emptyWorld :: StdGen -> World
emptyWorld = World [] U.empty U.empty [] 0 60 0.02 True True False False V.empty 0 Seq.empty Seq.empty 0 0

-- 乱数を 1 つ
rnd :: StdGen -> (Float, StdGen)
rnd = randomR (0, 1)

newProblem :: World -> World
newProblem w =
  let mk g i =
        let (cw, g1) = randomR (5, 40 :: Int) g               -- ★ 費用 w
            (r1, g2) = randomR (0.5, 1.8 :: Float) g1          -- ★ 効果 v
            (r2, g3) = randomR (-5, 5 :: Float) g2
            v = max 1 (floor (fromIntegral cw * r1 + r2))
        in (g3, Item cw v (wrapF 360 (fromIntegral (i * 137))))
      (gen, items) = mapAccumL mk (wGen w) [0 .. nItems - 1]
      ws = U.fromList (map iW items)
      vs = U.fromList (map iV items)
      budget = floor (fromIntegral (U.sum ws) * (0.35 :: Float))     -- ★ 予算 = 費用の合計の 35%
      ratio i = fromIntegral (vs U.! i) / fromIntegral (ws U.! i) :: Float
      order = sortBy (comparing ratio) [0 .. nItems - 1]
  in restart w { wItems = items, wWs = ws, wVs = vs, wOrder = order, wBudget = budget
               , wDpBest = solveDP budget items, wGreedyBest = solveGreedy budget ws vs (reverse order), wGen = gen }

restart :: World -> World
restart w =
  let mkInd g _ =
        let (g1, bits) = mapAccumL (\h _ -> let (u, h1) = rnd h in (h1, if u < 0.3 then 1 else 0)) g [1 .. nItems]
        in (g1, makeInd w (U.fromList bits))
      (gen, inds) = mapAccumL mkInd (wGen w) [1 .. wPopSize w]
  in w { wPop = sortPop inds, wGenN = 0, wLogBest = Seq.empty, wLogMean = Seq.empty, wGen = gen }

sortPop :: [Ind] -> V.Vector Ind
sortPop = V.fromList . sortBy (comparing (negate . indFit))

weightOf :: World -> U.Vector Int -> Int
weightOf w g = U.sum (U.zipWith (*) g (wWs w))

fitness :: World -> U.Vector Int -> Int
fitness w g = U.sum (U.zipWith (*) g (wVs w))

makeInd :: World -> U.Vector Int -> Ind
makeInd w g0 = let g = repair w g0 in Ind g (fitness w g)

-- ★ 予算オーバーなら、効果 / 費用 が一番悪いものから外す（修復）
repair :: World -> U.Vector Int -> U.Vector Int
repair w g0 =
  let go (g, wt) i
        | wt > wBudget w && g U.! i == 1 = (g U.// [(i, 0)], wt - wWs w U.! i)
        | otherwise = (g, wt)
  in fst (foldl' go (g0, weightOf w g0) (wOrder w))

-- ★ トーナメント選択：ランダムに 3 個体を選んで一番良いものを親に
pickParent :: V.Vector Ind -> StdGen -> (U.Vector Int, StdGen)
pickParent pop g0 =
  let n = V.length pop
      (a, g1) = randomR (0, n - 1) g0
      (b, g2) = randomR (0, n - 1) g1
      (c, g3) = randomR (0, n - 1) g2
      better x y = if indFit (pop V.! y) > indFit (pop V.! x) then y else x
  in (indG (pop V.! better (better a b) c), g3)

-- 子を 1 つつくる
makeChild :: World -> StdGen -> Ind
          -> (StdGen, Ind)
makeChild w@World{..} g0 _ =
  let (p1, g1) = pickParent wPop g0
      (p2, g2) = pickParent wPop g1
      (cut, g3) = randomR (1, nItems - 1) g2
      child0 = if wUseCross then U.take cut p1 U.++ U.drop cut p2 else p1   -- 一点交叉
      flipBit h b = let (u, h1) = rnd h in (h1, if u < wMutRate then 1 - b else b)   -- 突然変異
      (g4, bits) = mapAccumL flipBit g3 (U.toList child0)
  in (g4, makeInd w (U.fromList bits))

-- ★ 1 世代すすめる
nextGeneration :: World -> World
nextGeneration w@World{..}
  | V.null wPop = w
  | otherwise =
      let elite = [V.head wPop | wElitism]
          nNew = wPopSize - length elite
          (gen, kids) = mapAccumL (makeChild w) wGen (replicate nNew (V.head wPop))
          pop = sortPop (elite ++ kids)
          best = fromIntegral (indFit (V.head pop)) :: Float
          mean = fromIntegral (V.sum (V.map indFit pop)) / fromIntegral (V.length pop)
          trim q = if Seq.length q > 600 then Seq.drop 1 q else q
      in w { wPop = pop, wGen = gen, wGenN = wGenN + 1, wLogBest = trim (wLogBest Seq.|> best), wLogMean = trim (wLogMean Seq.|> mean) }

drawAll :: Env -> World -> Picture
drawAll env w@World{..} = screen env $ Pictures [popPic, shareBars, bestPic, graph, explain, hud]
  where
    wd = envW env
    ht = envH env
    fI = fromIntegral :: Int -> Float
    best = if V.null wPop then Ind (U.replicate nItems 0) 0 else V.head wPop
    nPop = V.length wPop

    -- 左：集団（行 = 個体、上ほど良い。列 = プロジェクト）
    gx = 40
    gy = 110
    gw = wd * 0.42
    gh = ht - 260
    cw = gw / fI nItems
    rh = gh / fI (max 1 nPop)
    hues = U.fromList (map iHue wItems)
    popPic = Pictures $
      [ box (hsb 225 25 20) (gx + fI i * cw) gy (cw - 1) gh | i <- [0 .. nItems - 1] ]
      ++ [ box (hsb (hues U.! i) 60 90) (gx + fI i * cw) (gy + fI r * rh) (cw - 1) (max 1 (rh - (if rh > 4 then 1 else 0)))
         | (r, ind) <- zip [0 ..] (V.toList wPop), i <- [0 .. nItems - 1], indG ind U.! i == 1 ]
      ++ [ txt gx (gy - 12) 14 (gr 70) ("Population " ++ show wPopSize ++ " (top = best). Color = chosen project") ]

    -- 列ごとの「選ばれている割合」（多様性が消えると 0% か 100% に固まる）
    shares = [ fI (V.sum (V.map (\p -> indG p U.! i) wPop)) / fI (max 1 nPop) | i <- [0 .. nItems - 1] ]
    diversity = sum [f * (1 - f) * 4 / fI nItems | f <- shares]
    shareBars = Pictures $
      [ box (hsb (hues U.! i) 60 90) (gx + fI i * cw) (gy + gh + 10 + (1 - f) * 40) (cw - 1) (f * 40) | (i, f) <- zip [0 ..] shares ]
      ++ [ txt gx (gy + gh + 68) 14 (gr 60) ("Share chosen per project   diversity " ++ nf 2 diversity ++ " (0 = all same)") ]

    -- 一番良い個体
    bestPic = txt gx (gy + gh + 92) 14 (gr 85) ("Best: value " ++ show (indFit best) ++ "   cost " ++ show (weightOf w (indG best)) ++ " / budget " ++ show wBudget)

    -- 右：成績のグラフ
    px = gx + gw + 60
    pw = wd - px - 40
    py = 120
    ph = ht * 0.42
    dp = fI wDpBest
    vmax = dp * 1.05
    vmin = dp * 0.5
    yV v = py + ph - clampF 0 1 ((v - vmin) / max 1e-6 (vmax - vmin)) * ph
    curve t c q = path t c [ (px + fI k / 599 * pw, yV v) | (k, v) <- zip [0 ..] (toList q) ]
    graph = Pictures
      [ seg 1 (hsba 0 0 100 0.25) (px, py + ph) (px + pw, py + ph)
      , seg 1 (hsba 0 0 100 0.25) (px, py) (px, py + ph)
      , dash 1 (hsb 140 80 100) (px, yV dp) (px + pw, yV dp)
      , dash 1 (hsb 30 70 100) (px, yV (fI wGreedyBest)) (px + pw, yV (fI wGreedyBest))
      , curve 1.5 (hsb 200 50 80) wLogMean
      , curve 2.5 (hsb 50 90 100) wLogBest
      , txtR (px + pw) (yV dp - 6) 14 (hsb 140 80 100) ("True optimum (DP) " ++ show wDpBest)
      , txtR (px + pw) (yV (fI wGreedyBest) + 18) 14 (hsb 30 70 100) ("Greedy (v/w order) " ++ show wGreedyBest)
      , txt (px + 6) (py - 8) 14 (hsb 50 90 100) ("Best " ++ show (indFit best))
      , txt (px + 110) (py - 8) 14 (hsb 200 50 80) "Mean"
      ]

    -- 説明
    ty = py + ph + 50
    rows =
      [ "Gen " ++ show wGenN ++ "  Best " ++ show (indFit best) ++ " / Opt " ++ show wDpBest ++ " (" ++ nf 1 (100 * fI (indFit best) / max 1 dp) ++ "%)"
          ++ (if indFit best == wDpBest then "  REACHED" else "")
      , "Mutation " ++ nf 3 wMutRate ++ " (about " ++ nf 1 (wMutRate * fI nItems) ++ " bits flip per individual)"
      , if wUseCross then "Crossover ON (cut 2 parents at one point and join)" else "Crossover OFF (copy of a parent + mutation only)"
      , if wElitism then "Elitism ON (the best goes straight to the next gen)" else "Elitism OFF (even the best can vanish)"
      , ""
      , "Low mutation: all alike, then stuck (premature convergence)"
      , "High mutation: close to random search"
      ]
    explain = Pictures [ txt px (ty + i * 24) 15 (gr 85) s | (i, s) <- zip [0 ..] rows ]

    hud = Pictures
      [ txt 20 40 16 white ("Space: " ++ (if wPaused then "resume" else "pause") ++ "  U/D mutation  L/R pop " ++ show wPopSize ++ "  C: cross " ++ onOff wUseCross
          ++ "  E: elite " ++ onOff wElitism ++ "  F: fast " ++ onOff wFast ++ "  R: restart  N: new")
      , txt 20 68 14 (gr 60) ("Budget " ++ show wBudget ++ ": maximize total value. GA does not guarantee the optimum, but needs no inside knowledge.")
      ]

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- キー
onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wMutRate = min 0.3 (wMutRate w * 1.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wMutRate = max 0.001 (wMutRate w / 1.5) }
  EventKey (SpecialKey KeyRight) Down _ _ -> restart w { wPopSize = min 200 (wPopSize w + 20) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> restart w { wPopSize = max 10 (wPopSize w - 10) }
  EventKey (Char c) Down _ _ -> case toLower c of
    'c' -> w { wUseCross = not (wUseCross w) }
    'e' -> w { wElitism = not (wElitism w) }
    'f' -> w { wFast = not (wFast w) }
    'r' -> restart w
    'n' -> newProblem w
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 突然変異率を 0.001 と 0.2 にして、多様性と最良の伸び方を比べよう
-- 2. 交叉 OFF だと最適に届くまでの世代はどう変わる？ 交叉は何の役に立っている？
-- 3. エリート保存 OFF だと最良の線がギザギザになる。なぜ？
-- 4. 修復（repair）をやめて、予算オーバーの個体の効果を 0 にする（罰金）方式に変えるとどうなる？


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標で描くための変換
screen :: Env -> Picture -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2)

-- 文字：(x, y) は左端のベースライン
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label x (negate y)

-- 文字：右端を x にそろえる
txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtR x y size = \c s -> label (x - fromIntegral (length s) * size * 0.72) (negate y) size c s

-- 塗りつぶした長方形：左上 (x, y)、幅 w、高さ h
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (rect (x + w / 2) (negate (y + h / 2)) w h)

-- 線分（太さ t）
seg :: Float -> Color -> Point -> Point -> Picture
seg t c (x1, y1) (x2, y2) = Color c (thickLine t (x1, negate y1) (x2, negate y2))

-- 折れ線（太さ t）
path :: Float -> Color -> [Point] -> Picture
path t c ps = Color c (thickPath t [(x, negate y) | (x, y) <- ps])

-- 点線（8 描いて 6 空ける）
dash :: Float -> Color -> Point -> Point -> Picture
dash t c (x1, y1) (x2, y2) =
  let l = dist2 (x1, y1) (x2, y2)
      pt d = (x1 + (x2 - x1) * d / l, y1 + (y2 - y1) * d / l)
      ds = takeWhile (< l) [0, 14 ..]
  in if l < 1 then Blank else Pictures [seg t c (pt d) (pt (min l (d + 8))) | d <- ds]

-- 灰色（明るさ 0〜100）
gr :: Float -> Color
gr = hsb 0 0

-- 小数点以下 d 桁の文字列（p5.js の nf）
nf :: RealFloat a => Int -> a -> String
nf d x
  | d <= 0 = show (round x :: Int)
  | otherwise = showFFloat (Just d) x ""

-- 答え合わせ：動的計画法（→ シリーズ 12）
solveDP :: Int -> [Item] -> Int
solveDP budget items =
  let start = U.replicate (budget + 1) 0 :: U.Vector Int
      add best Item{..} = U.generate (budget + 1) $ \c ->
        if c >= iW then max (best U.! c) (best U.! (c - iW) + iV) else best U.! c
  in U.last (foldl' add start items)

-- 答え合わせ：貪欲法（効果 / 費用 の良い順に入るだけ入れる）
solveGreedy :: Int -> U.Vector Int -> U.Vector Int -> [Int] -> Int
solveGreedy budget ws vs order =
  snd (foldl' (\(cw, cv) i -> if cw + ws U.! i <= budget then (cw + ws U.! i, cv + vs U.! i) else (cw, cv)) (0, 0) order)
