-- ============================================
-- 13 ゲーム理論 ─ ミニマックスと囚人のジレンマ
-- 相手も考えて動くときの意思決定。1：ゼロサムゲーム（自分の得 = 相手の損）は「混合戦略」を図で解く
--   行プレイヤーは確率 p で上の手を出す。相手が一番困らせる手を選んでも得られる量（下側の折れ線）を最大にする p がミニマックス解
-- 2：囚人のジレンマを 200 回くり返すと？ いろいろな戦略の総当たり戦と、強い戦略が増える「進化」
-- 1・2 = 切りかえ / 1 のとき：マスをクリック = +1（Shift + クリック = −1）、R = ランダムな表 / 2 のとき：スペース = 対戦、←→ = ミスの確率、E = 進化
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Numeric (showFFloat)
import Control.Monad (replicateM)
import Data.Array
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import qualified Data.Sequence as Seq
import System.Random (StdGen, newStdGen, randomR)

rounds :: Int
rounds = 200                           -- ★ くり返す回数

-- ★ 自分の得点（自分の手・相手の手）。True = 協力
pay :: Bool -> Bool -> Float
pay True True = 3
pay True False = 0
pay False True = 5
pay False False = 1

-- | 戦略：名前と「次の手」。履歴は新しい順。coin はコイン投げの結果
data Strategy = Strategy { stName :: String, stMove :: [Bool] -> [Bool] -> Bool -> Bool }

strategies :: [Strategy]
strategies =
  [ Strategy "Always C" (\_ _ _ -> True)
  , Strategy "Always D" (\_ _ _ -> False)
  , Strategy "Tit for tat" (\_ op _ -> case op of { o : _ -> o; [] -> True })
  , Strategy "Trigger" (\_ op _ -> and op)
  , Strategy "Random" (\_ _ coin -> coin)
  , Strategy "Pavlov" (\me op _ -> case (me, op) of { (m : _, o : _) -> m == o; _ -> True })
  , Strategy "Forgiving" (\_ op _ -> case op of { False : False : _ -> False; _ -> True })
  ]

nS :: Int
nS = length strategies

stratAt :: Array Int Strategy
stratAt = listArray (0, nS - 1) strategies

data World = World
  { wView :: !Int
  , wA :: ![[Int]]                     -- ★ 行プレイヤーの得点（列プレイヤーはその逆）
  , wNoise :: !Float                   -- ミスの確率
  , wM :: !(Array (Int, Int) Float)    -- 総当たりの平均得点
  , wTotals :: ![Float]
  , wShares :: ![Float]
  , wShareLog :: !(Seq.Seq [Float])    -- 割合の記録（400 世代分）
  , wEvolving :: !Bool
  , wGen :: !StdGen
  }

main :: IO ()
main = runSketch (sketch "30-13 Game Theory" (error "skStart makes the world"))
  { skBack  = hsb 225 30 12
  , skStart = \_ -> tournament . start <$> newStdGen
  , skStep  = \_ _ w -> pure (if wView w == 2 && wEvolving w then evolveStep w else w)
  , skDraw  = drawAll
  , skEvent = \env ev w -> onEvent env ev w
  }
  where
    start = World 1 [[3, -1, 2, 0], [-2, 4, -1, 1]] 0 (listArray ((0, 0), (nS - 1, nS - 1)) (repeat 0)) [] [] Seq.empty False

---------------------------------------------------------------- 1：ゼロサムゲーム

row0, row1 :: [[Int]] -> [Int]
row0 a = case a of { r : _ -> r; [] -> [] }
row1 a = case a of { _ : r : _ -> r; _ -> [] }

rowValue :: Float -> Int -> Int -> Float
rowValue p a0 a1 = p * fromIntegral a0 + (1 - p) * fromIntegral a1

lowerEnv :: [[Int]] -> Float -> Float
lowerEnv a p = minimum (1 / 0 : zipWith (rowValue p) (row0 a) (row1 a))

-- ★ 下側の折れ線の一番高い点：p = 0, 1 と、直線どうしの交点だけ調べればよい
solveGame :: [[Int]] -> (Float, Float)
solveGame a =
  let cols = zip (row0 a) (row1 a)
      ic = zip [0 :: Int ..] cols
      cross = [ p | (i, (a0, a1)) <- ic, (j, (b0, b1)) <- ic, i < j
              , let den = fromIntegral ((a0 - a1) - (b0 - b1)) :: Float
              , abs den > 1e-9
              , let p = fromIntegral (b1 - a1) / den, p > 0, p < 1 ]
      pick (bp, bv) p = let v = lowerEnv a p in if v > bv + 1e-6 then (p, v) else (bp, bv)
  in foldl' pick (0, -1 / 0) (0 : 1 : cross)

cellX, cellY :: Int -> Float
cellX j = 80 + fromIntegral j * 90
cellY i = 150 + fromIntegral i * 70

colName :: Int -> String
colName j = "col " ++ take 1 (drop j "abcdef")

drawZeroSum :: Env -> World -> Picture
drawZeroSum env World{..} = Pictures [table, info, chart]
  where
    wd = envW env
    ht = envH env
    a0 = row0 wA
    a1 = row1 wA
    n = length a0
    (solP, solV) = solveGame wA
    table = Pictures $
      [ txtC (cellX j + 43) 130 14 (gr 80) (colName j) | j <- [0 .. n - 1] ]
      ++ [ txtC 42 (cellY i + 33) 14 (gr 80) nm | (i, nm) <- [(0, "Top"), (1, "Bot")] ]
      ++ concat
        [ [ box (if v >= 0 then hsb 140 50 (30 + fromIntegral v * 6) else hsb 0 50 (30 - fromIntegral v * 6)) (cellX j) (cellY i) 86 66
          , txtC (cellX j + 43) (cellY i + 33) 24 white (show v) ]
        | (i, r) <- zip [0 ..] [a0, a1], (j, v) <- zip [0 ..] r ]
    -- 鞍点（純粋戦略の解）があるか
    maximin = max (minimum (maxBound : a0)) (minimum (maxBound : a1))
    minimax = minimum (maxBound : zipWith max a0 a1)
    ty = 330
    info = Pictures
      [ txt 60 ty 15 (gr 85) ("Row maximin = " ++ show maximin ++ "   Column minimax = " ++ show minimax)
      , txt 60 (ty + 26) 15 (gr 85) (if maximin == minimax then "-> equal: saddle point; a pure strategy solves it" else "-> differ: fixed moves get read; mix them (mixed strategy)")
      , txt 60 (ty + 70) 16 (hsb 50 90 100) ("Best: Top " ++ nf 1 (100 * solP) ++ "%, Bot " ++ nf 1 (100 * (1 - solP)) ++ "%  value v = " ++ nf 3 solV)
      , txt 60 (ty + 100) 14 (gr 65) "Whatever column the opponent picks, you average at least v."
      , txt 60 (ty + 122) 14 (gr 65) "The opponent can hold you to v as well (minimax theorem)."
      , txt 60 (ty + 152) 14 (gr 65) "Real examples: penalty kicks (left or right?),"
      , txt 60 (ty + 174) 14 (gr 65) "surprise inspections, rock-paper-scissors."
      ]
    -- 右：図解
    gx = wd * 0.5
    gy = 140
    gw = wd - gx - 60
    gh = ht - 260
    vals = a0 ++ a1
    hi = fromIntegral (maximum (0 : vals)) + 1 :: Float
    lo = fromIntegral (minimum (0 : vals)) - 1 :: Float
    xP p = gx + p * gw
    yV v = gy + gh - (v - lo) / (hi - lo) * gh
    colHue j = wrapF 360 (fromIntegral j * 80 + 190)
    chart = Pictures $
      [ seg 1 (hsba 0 0 100 0.25) (gx, yV 0) (gx + gw, yV 0)
      , seg 1 (hsba 0 0 100 0.25) (gx, gy) (gx, gy + gh)
      , seg 1 (hsba 0 0 100 0.25) (gx + gw, gy) (gx + gw, gy + gh) ]
      ++ concat
        [ [ seg 2 (hsba (colHue j) 60 90 0.7) (xP 0, yV (fromIntegral v1)) (xP 1, yV (fromIntegral v0))
          , txt (xP 1 + 6) (yV (fromIntegral v0) + 4) 14 (hsb (colHue j) 60 90) (colName j) ]
        | (j, v0, v1) <- zip3 [0 ..] a0 a1 ]
      ++ [ path 4 (hsb 50 90 100) [ (xP p, yV (lowerEnv wA p)) | k <- [0 .. 200 :: Int], let p = fromIntegral k * 0.005 ]
         , dot (hsb 50 90 100) (xP solP) (yV solV) 14
         , dash 1 (hsba 50 90 100 0.6) (xP solP, yV solV) (xP solP, gy + gh)
         , txtC (gx + gw / 2) (gy + gh + 18) 14 (gr 70) "Probability of Top p ->"
         , txt (gx - 20) (gy + gh + 44) 14 (gr 70) "p = 0 (always Bot)"
         , txtR (gx + gw + 40) (gy + gh + 44) 14 (gr 70) "p = 1 (always Top)"
         , txt gx (gy - 32) 14 (gr 70) "Each line = my mean score if the opponent plays that column."
         , txt gx (gy - 12) 14 (gr 70) "Yellow = my score when they pick the worst column for me."
         ]

---------------------------------------------------------------- 2：囚人のジレンマ

-- 2 つの戦略を rounds 回戦わせる（戻り値は 1 回あたりの得点）
playMatch :: Float -> StdGen -> Int -> Int -> ((Float, Float), StdGen)
playMatch noise g0 a b = go rounds [] [] 0 0 g0
  where
    fa = stMove (stratAt ! a)
    fb = stMove (stratAt ! b)
    go :: Int -> [Bool] -> [Bool] -> Float -> Float -> StdGen -> ((Float, Float), StdGen)
    go 0 _ _ !sa !sb g = ((sa / fromIntegral rounds, sb / fromIntegral rounds), g)
    go t ha hb !sa !sb g =
      let (c1, g1) = randomR (0, 1 :: Float) g
          (c2, g2) = randomR (0, 1 :: Float) g1
          (n1, g3) = randomR (0, 1 :: Float) g2
          (n2, g4) = randomR (0, 1 :: Float) g3
          ma0 = fa ha hb (c1 < 0.5)
          mb0 = fb hb ha (c2 < 0.5)
          ma = if n1 < noise then not ma0 else ma0       -- ★ ミス（手がすべる）
          mb = if n2 < noise then not mb0 else mb0
      in go (t - 1) (ma : ha) (mb : hb) (sa + pay ma mb) (sb + pay mb ma) g4

-- 総当たり戦
tournament :: World -> World
tournament w =
  let pairs = [(i, j) | i <- [0 .. nS - 1], j <- [i .. nS - 1]]
      matchUp (g, acc) (i, j) =
        let ((ra, rb), g1) = playMatch (wNoise w) g i j
        in if i == j then (g1, ((i, i), (ra + rb) / 2) : acc)
           else (g1, ((i, j), ra) : ((j, i), rb) : acc)
      (gen, entries) = foldl' matchUp (wGen w, []) pairs
      m = array ((0, 0), (nS - 1, nS - 1)) entries
      totals = [ sum [m ! (i, j) | j <- [0 .. nS - 1]] / fromIntegral nS | i <- [0 .. nS - 1] ]
      shares = replicate nS (1 / fromIntegral nS)
  in foldr seq () totals `seq` w { wGen = gen, wM = m, wTotals = totals, wShares = shares, wShareLog = Seq.singleton shares }

-- ★ 進化（レプリケーター・ダイナミクス）：平均より稼ぐ戦略が増える
evolveStep :: World -> World
evolveStep w@World{..} =
  let fit = [ sum (zipWith (\j s -> s * wM ! (i, j)) [0 ..] wShares) | i <- [0 .. nS - 1] ]
      avg = sum (zipWith (*) wShares fit)
      shares = zipWith (\s f -> s * f / max 1e-9 avg) wShares fit
      l = wShareLog Seq.|> shares
  in foldr seq () shares `seq` w { wShares = shares, wShareLog = if Seq.length l > 400 then Seq.drop 1 l else l }

stratHue :: Int -> Float
stratHue i = wrapF 360 (fromIntegral (i * 51))

drawPD :: Env -> World -> Picture
drawPD env World{..} = Pictures [matrix, ranking, evolution, foot]
  where
    wd = envW env
    ht = envH env
    fI = fromIntegral :: Int -> Float
    cs = min 70 ((wd * 0.45 - 160) / fI nS)
    tx = 170
    ty = 150
    rowH = cs * 0.6
    name i = stName (stratAt ! i)
    matrix = Pictures $ concat
      [ [ txtC (tx + fI i * cs + cs / 2) (ty - 16) 14 (hsb (stratHue i) 60 95) (show (i + 1))
        , txtR (tx - 10) (ty + fI i * rowH + rowH / 2 + 5) 14 (hsb (stratHue i) 60 95) (show (i + 1) ++ ". " ++ name i) ]
        ++ concat
          [ let v = wM ! (i, j)
            in [ box (hsb (remap v 0 5 0 140) 60 (25 + v * 12)) (tx + fI j * cs) (ty + fI i * rowH) (cs - 2) (rowH - 2)
               , txtC (tx + fI j * cs + cs / 2) (ty + fI i * rowH + rowH / 2) 14 white (nf 2 v) ]
          | j <- [0 .. nS - 1] ]
      | i <- [0 .. nS - 1] ]
    by = ty + fI nS * rowH + 40
    order = sortBy (comparing (negate . snd)) (zip [0 ..] wTotals)
    ranking = Pictures $
      txt 40 by 14 (gr 80) "Round-robin mean score (per round)"
      : concat
        [ let y = by + 14 + fI k * 26
              len = t / 5 * (wd * 0.3)
          in [ box (hsb (stratHue i) 60 95) 170 y len 20
             , txt 40 (y + 15) 14 (gr 90) (show (k + 1) ++ ". " ++ name i)
             , txt (176 + len) (y + 15) 14 (gr 90) (nf 2 t) ]
        | (k, (i, t)) <- zip [0 ..] order ]
    -- 右：進化
    gx = wd * 0.55
    gy = 150
    gw = wd - gx - 60
    gh = ht - 300
    logL = toList wShareLog
    evolution = Pictures $
      [ txt gx (gy - 16) 14 (gr 80) ("Evolution (E): high scorers grow   Gen " ++ show (length logL - 1))
      , seg 1 (hsba 0 0 100 0.25) (gx, gy + gh) (gx + gw, gy + gh)
      , seg 1 (hsba 0 0 100 0.25) (gx, gy) (gx, gy + gh) ]
      ++ [ path 2.5 (hsb (stratHue i) 60 95)
             [ (gx + fI k / 399 * gw, gy + gh - v * gh) | (k, s) <- zip [0 ..] logL, v <- take 1 (drop i s) ]
         | i <- [0 .. nS - 1] ]
      ++ [ txt (gx + fI (i `mod` 3) * (gw / 3)) (gy + gh + 30 + fI (i `div` 3) * 22) 14 (hsb (stratHue i) 60 95) (name i ++ " " ++ nf 1 (100 * s))
         | (i, s) <- zip [0 ..] wShares ]
    foot = txt 40 (ht - 50) 14 (gr 65) "Payoff: both C 3/3, both D 1/1, lone D 5/0. One game favors D, but when repeated?"

drawAll :: Env -> World -> Picture
drawAll env w@World{..} = screen env $ Pictures
  [ if wView == 1 then drawZeroSum env w else drawPD env w
  , txt 20 40 16 white $ if wView == 1
      then "1: zero-sum game (2: Prisoner's Dilemma)   Click cell: +1, Shift+click: -1   R: random table"
      else "2: Prisoner's Dilemma (1: zero-sum)   Space: replay   L/R error rate " ++ nf 2 wNoise ++ "   E: evolve " ++ (if wEvolving then "ON" else "off")
  , txt 20 68 14 (gr 60) $ if wView == 1
      then "Numbers = row player's score = column player's loss. You pick Top or Bot, the opponent picks a to d."
      else "7 strategies, round robin (" ++ show rounds ++ " rounds each). Cell = row strategy's mean score against the column number."
  ]

-- キーとマウス
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down mods _ | wView w == 1 ->
    let (mx, my) = mouseP env
        d = if shift mods == Down then -1 else 1
        hit i j = mx > cellX j && mx < cellX j + 86 && my > cellY i && my < cellY i + 66
        bump i j v = if hit i j then max (-9) (min 9 (v + d)) else v
    in pure w { wA = [ [bump i j v | (j, v) <- zip [0 ..] r] | (i, r) <- zip [0 ..] (wA w) ] }
  EventKey (Char '1') Down _ _ -> pure w { wView = 1 }
  EventKey (Char '2') Down _ _ -> pure w { wView = 2 }
  EventKey (Char c) Down _ _ | wView w == 1 && toLower c == 'r' -> do
    rows <- replicateM 2 (replicateM (length (row0 (wA w))) (randomRIO (-5, 5)))
    pure w { wA = rows }
  EventKey (Char c) Down _ _ | wView w == 2 && toLower c == 'e' ->
    let shares = replicate nS (1 / fromIntegral nS)
    in pure $ if wEvolving w
         then w { wEvolving = False }
         else w { wShares = shares, wShareLog = Seq.singleton shares, wEvolving = True }
  EventKey (SpecialKey KeySpace) Down _ _ | wView w == 2 -> pure (tournament w) { wEvolving = False }
  EventKey (SpecialKey KeyRight) Down _ _ | wView w == 2 -> pure (tournament w { wNoise = min 0.3 (round2 (wNoise w + 0.01)) })
  EventKey (SpecialKey KeyLeft) Down _ _ | wView w == 2 -> pure (tournament w { wNoise = max 0 (round2 (wNoise w - 0.01)) })
  _ -> pure w

round2 :: Float -> Float
round2 v = fromIntegral (round (v * 100) :: Int) / 100

-- ---------- やってみよう ----------
-- 1. 表を「じゃんけん」っぽく（勝ち 1・負け −1）にすると最適な混ぜ方は？ 列を 3 つにして試そう（wA の各行を 3 個に）
-- 2. 鞍点のある表をつくろう。そのとき黄色の折れ線の頂上はどこにある？
-- 3. 囚人のジレンマでミスの確率を 0.05 にすると、しっぺ返しとトリガーの順位はどう変わる？ パブロフは？
-- 4. 自分の戦略を strategies に足そう（例：最初の 10 回は協力、あとは相手の協力率で決める）


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標で描くための変換
screen :: Env -> Picture -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2)

-- マウスの位置（左上が原点、y は下向き）
mouseP :: Env -> Point
mouseP = toP5 <*> envMouse

-- gloss の座標 → 左上が原点の座標
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字：(x, y) は左端のベースライン
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label x (negate y)

-- 文字：(x, y) が文字の中心
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y = labelC x (negate y)

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

-- 塗りつぶした円：中心 (x, y)、直径 d
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (at (x, negate y) (disc (d / 2)))

-- 灰色（明るさ 0〜100）
gr :: Float -> Color
gr = hsb 0 0

-- 小数点以下 d 桁の文字列（p5.js の nf）
nf :: RealFloat a => Int -> a -> String
nf d x
  | d <= 0 = show (round x :: Int)
  | otherwise = showFFloat (Just d) x ""

