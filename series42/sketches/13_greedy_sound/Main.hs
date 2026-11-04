-- ============================================
-- 42-13 貪欲法を聴く
-- クラスカル法（06）を音にする：辺を軽い順に試し、採用（独立）なら明るい音、閉路（従属）なら不協和音
--   音の高さは辺の重み（短い辺ほど高い音）。完成した全域木は P で重み順にメロディとして鳴らす
--   自由制作のベース：音色・テンポ・「何を音にするか」を変えて自分の貪欲法を作ろう
-- 操作：スペース = 1 ステップ / A = 自動 / P = 木を鳴らす / R = 最初から / N = 新しいグラフ / 1〜3 = 音色
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.List (foldl', sortOn)
import qualified Data.IntMap.Strict as IM

nNode :: Int
nNode = 8                -- ★ ノードの数

tempo :: Int
tempo = 22               -- ★ 自動のとき何フレームに 1 本試すか

noteGap :: Int
noteGap = 14             -- ★ メロディの音の間隔（フレーム）

data Edge = Edge { eA, eB, eW :: !Int }

data St = Untried | Taken | Dropped deriving (Eq)

-- 採用の波紋
data Ripple = Ripple { rPos :: !Point, rT :: !Float, rOk :: !Bool }

-- 木を鳴らす
data Melody = Melody { mList :: ![Int], mK :: !Int, mT :: !Int }

data World = World
  { wNodes   :: ![Point]
  , wEdges   :: ![Edge]
  , wOrder   :: ![Int]
  , wStepI   :: !Int
  , wStatus  :: ![St]
  , wAuto    :: !Bool
  , wTimer   :: !Int
  , wWave    :: !Wave        -- ★ 音色（1 = Triangle, 2 = Sine, 3 = Square）
  , wRipples :: ![Ripple]
  , wMelody  :: !(Maybe Melody)
  }

main :: IO ()
main = runSketch (sketch "42-13 Listening to greedy" world0)
  { skBack  = hsb 330 30 8
  , skStart = \env -> newGraph env world0
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [] [] [] 0 [] False 0 Triangle [] Nothing

noEdge :: Edge
noEdge = Edge 0 0 0

ends :: Edge -> (Int, Int)
ends e = (eA e, eB e)

newGraph :: Env -> World -> IO World
newGraph env w = do
  let place :: Int -> [Point] -> IO [Point]
      place tries ps
        | length ps >= nNode || tries <= 0 = pure ps
        | otherwise = do
            x <- randomRIO (80, envW env - 420)
            y <- randomRIO (130, envH env - 60)
            place (tries - 1) (if all (\q -> dist2 (x, y) q > 120) ps then ps ++ [(x, y)] else ps)
  ns <- place 500 []
  let n = length ns
      node = pick (0, 0) ns
      weight a b = roundI (dist2 (node a) (node b) / 10)
      limit = min (envW env) (envH env) * 0.42
      es1 = [ Edge i j (weight i j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1], dist2 (node i) (node j) < limit ]
      -- つながるまで足す
      link (uf, es) i
        | ufFind uf i /= ufFind uf 0 = (maybe uf id (ufUnion uf (0, i)), es ++ [Edge 0 i (weight 0 i)])
        | otherwise = (uf, es)
      (_, es2) = foldl' link (ufAll n (map ends es1), es1) [1 .. n - 1]
  pure (reset w { wNodes = ns, wEdges = es2 })

reset :: World -> World
reset w =
  let es = wEdges w
  in w { wOrder = sortOn (\i -> eW (pick noEdge es i)) [0 .. length es - 1]
       , wStatus = map (const Untried) es
       , wStepI = 0, wAuto = False, wMelody = Nothing, wRipples = [] }

-- ★ 重み → 周波数（短い辺ほど高い音。ペンタトニックに丸める）
freqOf :: Int -> Float
freqOf w =
  let scaleNotes = [0, 2, 4, 7, 9] :: [Int]
      idx = max 0 (min 14 (roundI (remap (fromIntegral w) 10 60 14 0)))
      semis = pick 0 scaleNotes (idx `mod` 5) + 12 * (idx `div` 5)
  in 220 * 2 ** (fromIntegral semis / 12)

midOf :: World -> Edge -> Point
midOf w e =
  let (ax, ay) = pick (0, 0) (wNodes w) (eA e)
      (bx, by) = pick (0, 0) (wNodes w) (eB e)
  in ((ax + bx) / 2, (ay + by) / 2)

kruskalStep :: World -> IO World
kruskalStep w
  | wStepI w >= length (wOrder w) = pure w
  | otherwise = do
      let es = wEdges w
          i = pick 0 (wOrder w) (wStepI w)
          e = pick noEdge es i
          chosen = [ j | j <- take (wStepI w) (wOrder w), pick Untried (wStatus w) j == Taken ]
          ok = isForest (length (wNodes w)) (map (ends . pick noEdge es) (chosen ++ [i]))
          f = freqOf (eW e)
      if ok then playTone f 0.25 0.35 (wWave w)
            else do playTone f 0.25 0.4 Sawtooth          -- 近い 2 音 = うなり（不協和）
                    playTone (f * 1.06) 0.25 0.4 Sawtooth
      pure w { wStatus = setAt i (const (if ok then Taken else Dropped)) (wStatus w)
             , wRipples = Ripple (midOf w e) 0 ok : wRipples w
             , wStepI = wStepI w + 1 }

playTree :: World -> World
playTree w =
  let list = [ i | i <- wOrder w, pick Untried (wStatus w) i == Taken ]
  in if null list then w else w { wMelody = Just (Melody list 0 0) }

step :: Env -> Float -> World -> IO World
step _ _ w0 = do
  -- 自動
  w1 <- if wAuto w0
          then do
            let t = wTimer w0 + 1
                wt = w0 { wTimer = t }
            if t `mod` tempo == 0
              then do w' <- kruskalStep wt
                      pure (if wStepI w' >= length (wOrder w') then w' { wAuto = False } else w')
              else pure wt
          else pure w0
  -- メロディ
  w2 <- case wMelody w1 of
    Just m
      | mT m `mod` noteGap == 0 ->
          case drop (mK m) (mList m) of
            [] -> pure w1 { wMelody = Nothing }
            (i : _) -> do
              let e = pick noEdge (wEdges w1) i
              playTone (freqOf (eW e)) 0.25 0.3 (wWave w1)
              pure w1 { wMelody = Just m { mK = mK m + 1, mT = mT m + 1 }
                      , wRipples = Ripple (midOf w1 e) 0 True : wRipples w1 }
      | otherwise -> pure w1 { wMelody = Just m { mT = mT m + 1 } }
    Nothing -> pure w1
  -- 波紋
  pure w2 { wRipples = [ r { rT = rT r + 1 / 30 } | r <- wRipples w2, rT r + 1 / 30 < 1 ] }

waveName :: Wave -> String
waveName Triangle = "triangle"
waveName Sine = "sine"
waveName Square = "square"
waveName Sawtooth = "sawtooth"

draw :: Env -> World -> Picture
draw env w =
  let es = wEdges w
      st = pick Untried (wStatus w)
      node = pick (0, 0) (wNodes w)
      nOk = length (filter (== Taken) (wStatus w))
      nNg = length (filter (== Dropped) (wStatus w))
      done = nOk == length (wNodes w) - 1
      nOrder = length (wOrder w)
      current = if wStepI w < nOrder && not done then pick (-1) (wOrder w) (wStepI w) else -1
      playing = case wMelody w of
        Just m | mK m > 0 -> pick (-1) (mList m) (mK m - 1)
        _ -> -1
      edgePic i e =
        let p = node (eA e)
            q = node (eB e)
        in if i == playing then seg env 10 (hsb 50 90 100) p q
           else case st i of
             Taken -> seg env 6 (hsb 130 85 100) p q
             Dropped -> dashSeg env 1.5 5 7 (hsba 5 70 60 0.5) p q
             Untried | i == current -> seg env 3 (hsb 50 90 100) p q
                     | otherwise -> seg env 1.5 (gray 0.35) p q
      ripplePic r = Color (hsba (if rOk r then 130 else 5) 80 100 (1 - rT r))
                      (at (sp env (rPos r)) (ring ((20 + 120 * rT r) / 2) 3))
      px = envW env - 380
      py = 120
      rowH = 20
      maxRows = floor ((envH env - py - 100) / rowH) :: Int
      row k i =
        let e = pick noEdge es i
            s = st i
            isCur = k == wStepI w
            c | s == Taken = hsb 130 75 100
              | s == Dropped = hsb 5 75 65
              | isCur = hsb 50 75 100
              | k > wStepI w = gray 0.55
              | otherwise = white
        in txt env px (py + 68 + fromIntegral k * rowH) 14 c
             ((if isCur && not done then "> " else "   ") ++ pad2 (eW e) ++ "  " ++ show (eA e + 1) ++ "-" ++ show (eB e + 1)
               ++ "  " ++ show (roundI (freqOf (eW e))) ++ " Hz  "
               ++ (case s of { Taken -> "taken"; Dropped -> "cycle"; Untried -> "" }))
  in Pictures $
       zipWith edgePic [0 ..] es ++ map ripplePic (wRipples w) ++
       map (ball env 12 (hsb 330 40 85)) (wNodes w) ++
       [ txt env 20 40 16 white ("Space: 1 step  A: auto " ++ (if wAuto w then "ON" else "off") ++ "  P: play tree  R: restart  N: new graph  1-3: wave")
       , txt env 20 66 14 (gray 0.6) "Taken (independent) = bright note. Cycle (dependent) = beating clash. Shorter edge = higher note."
       , box env (withA 0.08 white) (px - 20) (py - 40) 390 (envH env - py + 20)
       , stack env px py
           [ (18, hsb (if done then 130 else 50) 80 100,
               if done then "Tree done! P to play" else show (wStepI w) ++ " / " ++ show (length es) ++ " edges tried")
           , (14, white, "Taken " ++ show nOk ++ "  dropped " ++ show nNg)
           , (14, white, "Wave: " ++ waveName (wWave w)) ] ] ++
       zipWith row [0 ..] (take maxRows (wOrder w)) ++
       [ txt env px (py + 68 + fromIntegral maxRows * rowH) 14 (gray 0.5) ("... " ++ show (nOrder - maxRows) ++ " more") | nOrder > maxRows ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> kruskalStep w
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "pP" -> pure (playTree w)
    | c `elem` "rR" -> pure (reset w)
    | c `elem` "nN" -> newGraph env w
    | c == '1' -> pure w { wWave = Triangle }
    | c == '2' -> pure w { wWave = Sine }
    | c == '3' -> pure w { wWave = Square }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. tempo を 8 にして自動再生。採用の音と閉路の音が「だんだん閉路ばかり」になるのが聞こえる？
-- 2. freqOf を「重い辺ほど高い音」に変える。木の完成までの音が上がっていく形になる
-- 3. 閉路のとき、その閉路の辺を全部短く鳴らす（01 の update の閉路の求め方を使う）
-- 4. 06 のドラッグを足して、ノードを動かしながら木が変わる音を聴く。14 のブラシと合体しても楽しい

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標（中央が原点、y は上向き）へ
sp :: Env -> Point -> Point
sp env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- 文字（左下を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sp env (x, y) in label a b size c s

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

-- 四捨五入（Math.round）
roundI :: Float -> Int
roundI x = floor (x + 0.5)

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

-- 辺を全部つないだ union-find
ufAll :: Int -> [(Int, Int)] -> IM.IntMap Int
ufAll n = foldl' (\uf e -> maybe uf id (ufUnion uf e)) (ufNew n)

-- 辺のリストが森か（閉路がないか）
isForest :: Int -> [(Int, Int)] -> Bool
isForest n es = case foldM ufUnion (ufNew n) es of
  Just _ -> True
  Nothing -> False
