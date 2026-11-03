-- ============================================
-- 04 割当とスケジューリング ─ ジョンソン法
-- 印刷所の仕事（ジョブ）は全部「印刷機 M1 → 製本機 M2」の順に通る。どの順番で流すと全部終わる時刻（メイクスパン）が一番早い？
--   ジョンソン法（1954）：M1 が短い仕事は前へ（短い順）、M2 が短い仕事は後ろへ（長い順）。2 台なら必ず最適になる
-- ドラッグ = 下の札を並べかえる / J = ジョンソン法 / X = ばらばらに / ↑↓ = 仕事の数 / R = 新しい仕事
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Numeric (showFFloat)
import Control.Monad (replicateM)
import Data.Array (Array, accumArray, elems, listArray, (!))
import Data.Char (toLower)
import Data.List (foldl', permutations, sortBy)
import Data.Ord (comparing)

-- | 仕事：M1 の時間、M2 の時間、色
data Job = Job { jA, jB :: !Int, jHue :: !Float }

-- | バーの位置
data Bar = Bar { bK, bS1, bE1, bS2, bE2 :: !Int }

-- | 全部の順番を試した結果
data Stats = Stats { stTotal, stBest, stWorst :: !Int, stMean :: !Float, stCount :: [Int] }

data World = World
  { wN :: !Int                         -- ★ 仕事の数（4〜8）
  , wJobs :: !(Array Int Job)
  , wSeq :: ![Int]                     -- 流す順番
  , wDragK :: !Int                     -- ドラッグ中の札（-1 = なし）
  , wDragX :: !Float
  , wStats :: !Stats
  , wSteps :: ![String]                -- ジョンソン法の説明
  }

main :: IO ()
main = runSketch (sketch "30-04 Johnson's Rule" (error "skStart makes the world"))
  { skBack  = hsb 225 30 12
  , skStart = \_ -> newJobs 6
  , skDraw  = drawAll
  , skEvent = onEvent
  }

newJobs :: Int -> IO World
newJobs n = do
  ts <- replicateM n ((,) <$> randomRIO (1, 9) <*> randomRIO (1, 9))     -- ★ 処理時間
  let hueOf i = wrapF 360 (fromIntegral i * 360 / fromIntegral n + 20)
      jobs = listArray (0, n - 1) [Job a b (hueOf i) | (i, (a, b)) <- zip [0 :: Int ..] ts]
  pure (World n jobs [0 .. n - 1] (-1) 0 (enumerateAll n jobs) [])

-- ★ メイクスパン：M1 は詰めて流す。M2 は「M1 が終わった」かつ「M2 が空いた」ときに始める
schedule :: Array Int Job -> [Int] -> (Int, [Bar])
schedule jobs order =
  let go (t1, t2, bars) k =
        let e1 = t1 + jA (jobs ! k)
            s2 = max t2 e1
            e2 = s2 + jB (jobs ! k)
        in (e1, e2, Bar k t1 e1 s2 e2 : bars)
      (_, sp, bs) = foldl' go (0, 0, []) order
  in (sp, reverse bs)

-- メイクスパンだけを速く求める
spanOf :: Array Int Job -> [Int] -> Int
spanOf jobs = snd . foldl' step (0, 0)
  where
    step (!t1, !t2) k =
      let e1 = t1 + jA (jobs ! k)
      in (e1, max t2 e1 + jB (jobs ! k))

-- ★ ジョンソン法
johnson :: World -> World
johnson w@World{..} =
  let job k = wJobs ! k
      idx = sortBy (comparing (\k -> min (jA (job k)) (jB (job k)))) [0 .. wN - 1]
      go (front, back, steps) k
        | jA (job k) <= jB (job k) = (front ++ [k], back, steps ++ [line k "M1" (jA (job k)) "<=" "M2" (jB (job k)) "front"])
        | otherwise = (front, k : back, steps ++ [line k "M2" (jB (job k)) "<" "M1" (jA (job k)) "back"])
      line k m1 a op m2 b dest = "J" ++ show (k + 1) ++ ": " ++ m1 ++ " " ++ show a ++ " " ++ op ++ " " ++ m2 ++ " " ++ show b ++ " -> " ++ dest
      (fr, bk, st) = foldl' go ([], [], []) idx
  in w { wSeq = fr ++ bk, wSteps = st }

-- 全部の順番（n!）を試す ─ 8 個で 40320 通り
enumerateAll :: Int -> Array Int Job -> Stats
enumerateAll n jobs =
  let spans = map (spanOf jobs) (permutations [0 .. n - 1])
      (!total, !best, !worst, !sumS) =
        foldl' (\(!c, !b, !wo, !s) x -> (c + 1, min b x, max wo x, s + x)) (0 :: Int, maxBound :: Int, 0 :: Int, 0 :: Int) spans
      cnt = accumArray (+) 0 (best, worst) [(x, 1) | x <- spans] :: Array Int Int
      !counts = forceList (elems cnt)
  in Stats total best worst (fromIntegral sumS / fromIntegral total) counts

forceList :: [Int] -> [Int]
forceList xs = foldl' (flip seq) () xs `seq` xs

-- 札の位置
cardW :: Env -> Int -> Float
cardW env n = min 110 ((envW env - 80) / fromIntegral n - 10)

cardX :: Env -> Int -> Int -> Float
cardX env n k = 40 + fromIntegral k * (cardW env n + 10)

cardY :: Env -> Float
cardY env = envH env - 150

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures [gantt, hist, method, cards, hud]
  where
    wd = envW env
    ht = envH env
    Stats{..} = wStats
    (sp, bars) = schedule wJobs wSeq
    gx = 120
    gw = wd - gx - 60
    unit = gw / fromIntegral (max 1 (stWorst + 2))
    tx t = gx + fromIntegral t * unit
    r0 = 150
    r1 = 230

    -- ガントチャート
    gantt = Pictures $
      [ txt 30 (r0 + 30) 16 (gr 85) "M1 print", txt 30 (r1 + 30) 16 (gr 85) "M2 bind" ]
      ++ [ seg 1 (hsba 0 0 100 0.12) (tx t, r0 - 10) (tx t, r1 + 60) | t <- [0 .. stWorst + 2] ]
      ++ [ txtC (tx t) (r1 + 72) 14 (gr 50) (show t) | t <- [0, 5 .. stWorst + 2] ]
      ++ concatMap barPic bars
      ++ idleBoxes
      ++ [ seg 3 (hsb 50 90 100) (tx sp, r0 - 20) (tx sp, r1 + 60)
         , dash 2 (hsb 140 80 100) (tx stBest, r0 - 20) (tx stBest, r1 + 60)
         , txtR (tx sp - 6) (r0 - 14) 16 (hsb 50 90 100) ("Makespan " ++ show sp)
         , txt (tx stBest + 6) (r1 + 92) 14 (hsb 140 70 100) ("Best " ++ show stBest) ]
    barPic Bar{..} =
      let jb = wJobs ! bK
          c = hsb (jHue jb) 60 90
          name = "J" ++ show (bK + 1)
          bw a b = fromIntegral (b - a) * unit
          lbl a b y = if bw a b >= 18 then [txtC (tx a + bw a b / 2) y 14 (gr 10) name] else []
      in [ box c (tx bS1) r0 (bw bS1 bE1 - 1) 50, box c (tx bS2) r1 (bw bS2 bE2 - 1) 50 ]
         ++ lbl bS1 bE1 (r0 + 18) ++ lbl bS2 bE2 (r1 + 18)
    -- M2 の待ち時間（アイドル）
    idleOf (prev, acc, pics) Bar{..}
      | bS2 > prev = (bE2, acc + bS2 - prev, box (hsba 0 70 80 0.35) (tx prev) r1 (fromIntegral (bS2 - prev) * unit) 50 : pics)
      | otherwise = (bE2, acc, pics)
    (_, idle, idleBoxes) = foldl' idleOf (0 :: Int, 0 :: Int, []) bars

    -- ヒストグラム：全部の順番のメイクスパン
    hx = 40
    hy = 360
    hw = wd * 0.5
    hh = ht - hy - 200
    nb = length stCount
    cmax = maximum (1 : stCount)
    bw = hw / fromIntegral (max 1 nb)
    labelEvery = max 1 (ceiling (26 / bw)) :: Int
    nBest = case stCount of
      c : _ -> c
      [] -> 0
    hist = Pictures $
      [ txt hx (hy - 10) 14 (gr 80) ("All " ++ show stTotal ++ " orders by makespan (left = better)") ]
      ++ concat
        [ [ let h = fromIntegral c / fromIntegral cmax * hh
                v = stBest + i
                col | v == sp = hsb 50 90 100
                    | v == stBest = hsb 140 60 80
                    | otherwise = hsb 220 30 50
            in box col (hx + fromIntegral i * bw) (hy + hh - h) (bw - 2) h ]
          ++ [ txtC (hx + (fromIntegral i + 0.5) * bw) (hy + hh + 12) 14 (gr 60) (show (stBest + i)) | i `mod` labelEvery == 0 ]
        | (i, c) <- zip [0 ..] stCount ]
      ++ [ txt hx (hy + hh + 34) 14 (gr 70)
             ("Best orders: " ++ show nBest ++ " (" ++ nf 1 (100 * fromIntegral nBest / fromIntegral stTotal :: Float) ++ "%). Random order mean " ++ nf 1 stMean) ]

    -- 右：ジョンソン法の説明
    jx = hx + hw + 50
    method = Pictures $
      [ txt jx hy 16 (gr 90) "Johnson's rule"
      , txt jx (hy + 24) 14 (gr 65) "1) Find the shortest time on M1 or M2"
      , txt jx (hy + 44) 14 (gr 65) "2) If on M1 place from the front, if on M2 from the back"
      , txt jx (hy + 64) 14 (gr 65) "3) Remove that job and repeat" ]
      ++ [ txt jx (hy + 92 + fromIntegral i * 20) 14 (gr 85) (show (i + 1) ++ ". " ++ s) | (i, s) <- zip [0 :: Int ..] wSteps ]
      ++ [ txt jx (hy + hh + 34) 14 (gr 60) ("Red on M2 = bindery idle time " ++ show idle) ]

    -- 札（ドラッグで並べかえ）
    (mx, _) = mouseP env
    cards = Pictures $
      [ drawCard k (cardX env wN i) (cardY env) | (i, k) <- zip [0 ..] wSeq, i /= wDragK ]
      ++ [ drawCard k (mx - wDragX) (cardY env - 10) | (i, k) <- zip [0 ..] wSeq, i == wDragK ]
    drawCard k x y =
      let jb = wJobs ! k
          w = cardW env wN
      in Pictures
        [ box (hsb (jHue jb) 50 85) x y w 90
        , txtC (x + w / 2) (y + 18) 18 (gr 10) ("J" ++ show (k + 1))
        , txtC (x + w / 2) (y + 50) 14 (gr 10) ("M1 " ++ show (jA jb))
        , txtC (x + w / 2) (y + 72) 14 (gr 10) ("M2 " ++ show (jB jb))
        ]

    hud = Pictures
      [ txt 20 40 16 white ("Drag: reorder cards   J: Johnson   X: shuffle   U/D: jobs " ++ show wN ++ "   R: new jobs")
      , txt 20 68 14 (gr 60) ("Makespan " ++ show sp ++ "  Best " ++ show stBest ++ (if sp == stBest then "  OPTIMAL!" else "  (" ++ show (sp - stBest) ++ " to go)")
          ++ "    Yellow line = all done, green dashes = earliest possible")
      ]

-- キーとマウス
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w@World{..} = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
        cw = cardW env wN
        hits = [ k | k <- [0 .. wN - 1], mx > cardX env wN k, mx < cardX env wN k + cw, my > cardY env, my < cardY env + 90 ]
    in pure $ case reverse hits of
         k : _ -> w { wDragK = k, wDragX = mx - cardX env wN k }
         [] -> w
  EventKey (MouseButton LeftButton) Up _ _
    | wDragK >= 0 ->
        let (mx, _) = mouseP env
            cw = cardW env wN
            slot = max 0 (min (length wSeq - 1) (round ((mx - wDragX - cardX env wN 0) / (cw + 10))))
            item = take 1 (drop wDragK wSeq)
            rest = take wDragK wSeq ++ drop (wDragK + 1) wSeq
        in pure w { wSeq = take slot rest ++ item ++ drop slot rest, wDragK = -1, wSteps = [] }
  EventKey (Char c) Down _ _ -> case toLower c of
    'j' -> pure (johnson w)
    'x' -> do
      s <- shuffleIO wSeq
      pure w { wSeq = s, wSteps = [] }
    'r' -> newJobs wN
    _ -> pure w
  EventKey (SpecialKey KeyUp) Down _ _ | wN < 8 -> newJobs (wN + 1)
  EventKey (SpecialKey KeyDown) Down _ _ | wN > 4 -> newJobs (wN - 1)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. J を押す前に、自分で並べかえて最適（緑の点線）に届くか挑戦しよう
-- 2. ヒストグラムを見ると「最適の順番」は何 % ある？ 仕事が 8 個になると全部試すのは大変（40320 通り）
-- 3. 機械を 3 台にすると、ジョンソン法はもう最適を保証しない。schedule に M3 を足して試そう
-- 4. 最初の仕事の M1 と最後の仕事の M2 の間は必ず待つ。だから「M1 が短い仕事を先に」がよい理由を説明しよう


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

-- 並びをランダムに混ぜる（p5.js の shuffle）
shuffleIO :: [a] -> IO [a]
shuffleIO xs = do
  keys <- mapM (const (randomRIO (0, 1 :: Float))) xs
  pure (map snd (sortBy (comparing fst) (zip keys xs)))

