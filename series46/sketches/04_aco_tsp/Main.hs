-- ============================================
-- 46-04 アリコロニー最適化で巡回セールスマン問題（ACO / TSP）
-- 全部の都市を 1 回ずつ回って戻る一番短い道は？ アリは「近い都市」と「フェロモンの濃い道」を確率で選んで 1 周する
-- 短い巡回路ほどフェロモンを多く置き、全体は少しずつ蒸発する。線の太さ = フェロモンの量、赤い線 = これまでの最良の巡回路
-- 操作：クリック = 都市を追加 / 1 = 円の上の都市（最適解がわかる）/ 2 = ランダムな都市 / C = 全部消す
--       ↑↓ = β（距離の重視）/ ←→ = 蒸発率 ρ / A = α（フェロモンの重視）/ O = 最良路を 2-opt で磨く / R = フェロモンをリセット / スペース = 一時停止
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (ST)
import Data.Bits (shiftL, shiftR, xor)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word (Word64)
import Numeric (showFFloat)

elite :: Double
elite = 3                    -- ★ 最良の巡回路に追加で置くフェロモンの倍率（エリート戦略）

itersPerFrame :: Int
itersPerFrame = 1            -- ★ 1 フレームに何回まわすか

data World = World
  { wAlpha    :: !Double                 -- α：フェロモンをどれだけ信じるか
  , wBeta     :: !Double                 -- β：距離の近さをどれだけ重視するか
  , wRho      :: !Double                 -- ρ：1 回ごとの蒸発率
  , wCx, wCy  :: !(U.Vector Float)       -- 都市の位置（p5.js と同じ画面の座標）
  , wD        :: !(U.Vector Double)      -- 距離の表（n × n）
  , wTau      :: !(U.Vector Double)      -- フェロモンの表（n × n）
  , wBest     :: !(U.Vector Int)         -- これまでの最良の巡回路
  , wBestLen  :: !Double
  , wIter     :: !Int
  , wHist     :: ![Double]               -- 最良の長さの記録（新しい順、2000 個を超えたら間引く）
  , wHistIt   :: ![Double]               -- その回の最良の長さの記録
  , wHistN    :: !Int
  , wTwoOpt   :: !Double                 -- 2-opt だけの最良（比べる相手）
  , wKnown    :: !Double                 -- わかっている最適解（0 = わからない）
  , wLayout   :: !String
  , wPaused   :: !Bool
  , wPolished :: !Bool
  , wSeed     :: !Seed
  }

world0 :: World
world0 = World 1 3 0.1 U.empty U.empty U.empty U.empty U.empty inf 0 [] [] 0 0 0 "" False False 1

inf :: Double
inf = 1 / 0

main :: IO ()
main = runSketch (sketch "46-04 Ant colony optimisation (TSP)" world0)
  { skBack  = hsb 268 40 13
  , skStart = \env -> do
      s <- randomRIO (1, 2 ^ (62 :: Int))
      pure (makeCircle env 24 world0 { wSeed = s })
  , skStep  = \_ _ w -> pure (if wPaused w || nCities w < 3 then w else iterate acoIteration w !! itersPerFrame)
  , skDraw  = draw
  , skEvent = event
  }

nCities :: World -> Int
nCities = U.length . wCx

-- 都市を置く枠（左上 x, y、幅、高さ）
areaBox :: Env -> (Float, Float, Float, Float)
areaBox env = (30, 90, envW env * 0.62 - 30, envH env - 120)

makeCircle :: Env -> Int -> World -> World
makeCircle env n w = rebuild w { wCx = U.map fst ps, wCy = U.map snd ps, wLayout = show n ++ " cities on a circle"
                               , wKnown = 2 * fromIntegral n * realToFrac r * sin (pi / fromIntegral n), wSeed = nextSeed s }
  where
    (bx, by, bw, bh) = areaBox env
    r = min bw bh * 0.42
    s = wSeed w
    -- 番号の順に回れば正解、とならないように並びを混ぜる
    order = shuffleV s (U.enumFromN 0 n :: U.Vector Int)
    ps = U.map (\i -> let a = fromIntegral i / fromIntegral n * tau in (bx + bw / 2 + r * cos a, by + bh / 2 + r * sin a)) order

makeRandom :: Env -> Int -> World -> World
makeRandom env n w = rebuild w { wCx = U.generate n (\i -> bx + 20 + hashRand s (2 * i) * (bw - 40))
                               , wCy = U.generate n (\i -> by + 20 + hashRand s (2 * i + 1) * (bh - 40))
                               , wLayout = show n ++ " random cities", wKnown = 0, wSeed = nextSeed s }
  where
    (bx, by, bw, bh) = areaBox env
    s = wSeed w

-- 都市が変わったら距離の表を作り直して最初から
rebuild :: World -> World
rebuild w
  | n < 3 = base { wTwoOpt = 0, wTau = U.replicate (n * n) 1 }
  | otherwise = base { wTwoOpt = two, wTau = U.replicate (n * n) (tau0 * 10), wSeed = iterate nextSeed s !! 5 }
  where
    n = nCities w
    d = U.generate (n * n) (\k -> let (i, j) = k `divMod` n
                                      dx = realToFrac (wCx w U.! i - wCx w U.! j)
                                      dy = realToFrac (wCy w U.! i - wCy w U.! j)
                                  in max 1e-3 (sqrt (dx * dx + dy * dy)))
    base = w { wD = d, wBest = U.empty, wBestLen = inf, wIter = 0, wHist = [], wHistIt = [], wHistN = 0, wPolished = False }
    s = wSeed w
    -- 比べる相手：2-opt だけで作った巡回路（ランダムな順番から 5 回やって一番よいもの）
    two = minimum [ tourLen d n (twoOpt d n (shuffleV sk (U.enumFromN 0 n))) | sk <- take 5 (iterate nextSeed s) ]
    tau0 = 1 / (fromIntegral n * two)

-- ★ ACO の 1 回分：アリ n 匹がそれぞれ 1 周 → 蒸発 → フェロモンを置く
acoIteration :: World -> World
acoIteration w = w { wTau = tau', wBest = best', wBestLen = bestLen', wIter = wIter w + 1, wSeed = nextSeed s
                   , wHist = h1, wHistIt = h2, wHistN = hn }
  where
    n = nCities w
    d = wD w
    s = wSeed w
    rho = wRho w
    wm = U.generate (n * n) (\k -> (wTau w U.! k) ** wAlpha w * (1 / d U.! k) ** wBeta w)
    tours = [ let t = antTour wm n s a in (t, tourLen d n t) | a <- [0 .. n - 1] ]
    (bestLen', best') = foldl' (\(bl, bt) (t, l) -> if l < bl then (l, t) else (bl, bt)) (wBestLen w, wBest w) tours
    itBest = foldl' min inf (map snd tours)
    tau' = U.modify (\m -> do
             forM_ [0 .. n * n - 1] $ M.modify m (* (1 - rho))                   -- 蒸発
             forM_ tours $ \(t, l) -> deposit m n t (1 / l)                       -- 短いほど多く置く
             deposit m n best' (elite / bestLen')
             forM_ [0 .. n * n - 1] $ M.modify m (\v -> min 1e6 (max 1e-12 v)))   -- 念のため範囲に収める
           (wTau w)
    (h1, h2, hn) =
      let a = bestLen' : wHist w
          b = itBest : wHistIt w
          k = wHistN w + 1
      in if k > 2000 then (thin a, thin b, length (thin a)) else (a, b, k)    -- 長くなったら間引く
    thin xs = reverse [ v | (i, v) <- zip [0 :: Int ..] (reverse xs), even i ]

-- アリ a の 1 周：まだ行っていない都市 j を重み τ^α (1/d)^β に比例する確率で選ぶ
antTour :: U.Vector Double -> Int -> Seed -> Int -> U.Vector Int
antTour wm n s a = U.create $ do
  vis <- M.replicate n False
  t <- M.new n
  M.write vis a True
  M.write t 0 a
  let pick cur r j lastJ
        | j >= n = pure lastJ
        | otherwise = do
            v <- M.read vis j
            if v then pick cur r (j + 1) lastJ
            else let r' = r - wm U.! (cur * n + j)
                 in if r' <= 0 then pure j else pick cur r' (j + 1) j
      total cur = foldM (\acc j -> do v <- M.read vis j; pure (if v then acc else acc + wm U.! (cur * n + j))) 0 [0 .. n - 1]
      go cur st
        | st >= n = pure ()
        | otherwise = do
            tot <- total cur
            let r = realToFrac (hashRand s (a * n + st)) * tot
            nxt0 <- pick cur r 0 (-1)
            let nxt = max 0 nxt0
            M.write vis nxt True
            M.write t st nxt
            go nxt (st + 1)
  go a 1
  pure t

deposit :: M.MVector st Double -> Int -> U.Vector Int -> Double -> ST st ()
deposit m n t amount = when (U.length t >= 2) $
  forM_ [0 .. U.length t - 1] $ \k -> do
    let a = t U.! k
        b = t U.! ((k + 1) `mod` U.length t)
    M.modify m (+ amount) (a * n + b)
    M.modify m (+ amount) (b * n + a)

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (hsbA 0 0 0 25) (rect (bx + bw / 2) (by + bh / 2) bw bh) ]
  -- ---- フェロモン：濃さを 6 段階に分けて、段階ごとにまとめて描く
  ++ (if n >= 3 then [ Color (hsbA 45 70 100 (12 + fromIntegral lv * 12))
                              (Pictures [ thickLine (fromIntegral lv * 1.3) (city i) (city j) | (i, j) <- es ])
                     | lv <- [1 .. 6 :: Int], let es = edgesAt lv, not (null es) ]
      else [])
  ++ [ path 2.5 (hsb 0 85 100) (map city (U.toList bt ++ take 1 (U.toList bt))) | let bt = wBest w, U.length bt >= 3 ]
  ++ [ dot (gray 1) x y 9 | (x, y) <- U.toList (U.zip (wCx w) (wCy w)) ]
  -- ---- 右：数字と収束グラフ
  ++ [ txt gx (110 + fromIntegral i * 26) 15 (gray 1) l | (i, l) <- zip [0 :: Int ..] info ]
  ++ [ Color (hsbA 0 0 0 35) (rect (gx + gw / 2) (py + ph / 2) gw ph)
     , txt gx (py - 8) 14 (hsb 0 0 85) "red = best so far, faint = this round, green = ref" ]
  ++ plot
  ++ [ txt gx (py + ph + 30) 14 (hsb 0 0 70) "P(choose j) ~ tau^alpha * (1/dist)^beta"
     , txt gx (py + ph + 50) 14 (hsb 0 0 70) "tau <- (1 - rho) tau + sum 1/L"
     , txt gx (py + ph + 70) 14 (hsb 0 0 70) "(shorter tours lay more pheromone)"
     , txt 20 36 17 (gray 1) ("A: alpha = " ++ fmtD 1 (wAlpha w) ++ "   Up/Down: beta = " ++ fmtD 1 (wBeta w)
                              ++ "   Left/Right: rho = " ++ fmtD 2 (wRho w) ++ "   O: 2-opt   R: reset pheromone   Space: "
                              ++ (if wPaused w then "paused" else "running"))
     , txt 20 60 14 (hsb 0 0 70) "Click: add a city (max 80)   1: cities on a circle   2: random   C: clear all" ]
  where
    n = nCities w
    (bx, by, bw, bh) = areaBox env
    city i = (wCx w U.! i, wCy w U.! i)
    tmax = U.foldl' max 1e-12 (wTau w)
    edgesAt lv = [ (i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1]
                          , let q = wTau w U.! (i * n + j) / tmax
                          , ceiling (q * 6) == lv, q > 0.04 ]
    gx = envW env * 0.65
    gw = envW env - gx - 30
    py = 280
    ph = min 220 (envH env - py - 80)
    info = [ "Layout: " ++ wLayout w
           , "Iteration " ++ show (wIter w) ++ " (" ++ show n ++ " ants per round)"
           , "Best tour: " ++ (if isInfinite (wBestLen w) then "-" else fmtD 1 (wBestLen w)) ++ (if wPolished w then " (2-opt polished)" else "")
           , "2-opt only (best of 5): " ++ (if wTwoOpt w > 0 then fmtD 1 (wTwoOpt w) else "-")
           , if wKnown w > 0
               then "Optimum (n-gon): " ++ fmtD 1 (wKnown w) ++ "  -> gap " ++ fmtD 2 (max 0 ((wBestLen w - wKnown w) / wKnown w * 100)) ++ " %"
               else "Optimum: unknown (need something to compare)" ]
    plot
      | wHistN w > 1 =
          let ref = if wKnown w > 0 then wKnown w else wTwoOpt w
              lo = ref * 0.97
              itOld = reverse (wHistIt w)
              hi = foldl' max (ref * 1.3) (take 20 itOld)
              yv v = py + ph - clampF 0 1 (realToFrac ((v - lo) / (hi - lo))) * ph
              line' c wd xs = let m = length xs in path wd c [ (gx + fromIntegral i / fromIntegral (max 1 (m - 1)) * gw, yv v) | (i, v) <- zip [0 :: Int ..] xs ]
          in [ seg 1.5 (hsb 120 60 90) (gx, yv ref) (gx + gw, yv ref)
             , line' (hsbA 0 0 100 30) 1 itOld
             , line' (hsb 0 85 100) 2 (reverse (wHist w)) ]
      | otherwise = []

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ (mx, my) ->
    let x = mx + envW env / 2
        y = envH env / 2 - my
        (bx, by, bw, bh) = areaBox env
    in if x > bx && x < bx + bw && y > by && y < by + bh && nCities w < 80
         then rebuild w { wCx = U.snoc (wCx w) x, wCy = U.snoc (wCy w) y
                        , wLayout = show (nCities w + 1) ++ " cities (placed by you)", wKnown = 0 }
         else w
  EventKey (Char '1') Down _ _ -> makeCircle env 24 w
  EventKey (Char '2') Down _ _ -> makeRandom env 40 w
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> rebuild w { wCx = U.empty, wCy = U.empty, wLayout = "empty (click to place cities)", wKnown = 0 }
    | c `elem` "aA" -> w { wAlpha = if wAlpha w >= 2 then 0 else wAlpha w + 0.5 }
    | c `elem` "oO" -> if U.length (wBest w) >= 4
                         then let t = twoOpt (wD w) (nCities w) (wBest w)
                              in w { wBest = t, wBestLen = tourLen (wD w) (nCities w) t, wPolished = True }
                         else w
    | c `elem` "rR" -> rebuild w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wBeta = min 8 (wBeta w + 0.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wBeta = max 0 (wBeta w - 0.5) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wRho = min 0.9 (wRho w + 0.02) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wRho = max 0.01 (wRho w - 0.02) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1（円の上の 24 都市）で、最適解との差が 0 % になるまで何回かかる？ R で何回やっても同じ？
-- 2. β = 0 にすると距離を見ないで選ぶ。β = 8 だと「近い所へ行くだけ」の貪欲法に近づく。どちらも苦手な理由は？
-- 3. ρ を 0.5 にすると忘れっぽいアリ。速く収束するけど、悪い答えで固まる（早すぎる収束）ことが増える
-- 4. 2（ランダム 40 都市）で ACO の最良と「2-opt だけ」を比べよう。O で ACO の答えを 2-opt で磨くと、さらに短くなる？
-- 5. 自分で都市を置いて、人間の目で考えた最短の回り方と ACO の答えを比べてみよう（elite を 0 にすると？）

-- ---------- 道具 ----------
-- 巡回路の長さ（n 都市の距離の表 d）
tourLen :: U.Vector Double -> Int -> U.Vector Int -> Double
tourLen d n t
  | U.null t = 0
  | otherwise = U.sum (U.imap (\i a -> d U.! (a * n + t U.! ((i + 1) `mod` U.length t))) t)

-- 2-opt：2 本の辺を入れかえて短くなる限り続ける（交差がほどける）
twoOpt :: U.Vector Double -> Int -> U.Vector Int -> U.Vector Int
twoOpt d n = U.modify (\t -> loop t (0 :: Int))
  where
    loop t guard = when (guard < 200) $ do
      improved <- pass t
      when improved (loop t (guard + 1))
    pass t = do
      let m = M.length t
      foldM (\imp (i, j) ->
               if i == 0 && j == m - 1 then pure imp else do
                 a <- M.read t i
                 b <- M.read t (i + 1)
                 c <- M.read t j
                 e <- M.read t ((j + 1) `mod` m)
                 let dd p q = d U.! (p * n + q)
                     delta = dd a c + dd b e - dd a b - dd c e
                 if delta < -1e-9 then reverseSeg t (i + 1) j >> pure True else pure imp)
            False [ (i, j) | i <- [0 .. m - 2], j <- [i + 2 .. m - 1] ]
    reverseSeg t i j = when (i < j) (M.swap t i j >> reverseSeg t (i + 1) (j - 1))

-- 並びをランダムに混ぜる（フィッシャー–イェーツ）
shuffleV :: U.Unbox a => Seed -> U.Vector a -> U.Vector a
shuffleV s = U.modify (\v -> forM_ [M.length v - 1, M.length v - 2 .. 1] $ \i ->
                         M.swap v i (min i (floor (hashRand s i * fromIntegral (i + 1)))))

-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = if w <= 1 then Color c (Line ps) else Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmtD :: Int -> Double -> String
fmtD n v = showFFloat (Just n) v ""

-- 乱数（xorshift64）。種を World に入れておけば skStep は純粋なまま
type Seed = Word64

nextSeed :: Seed -> Seed
nextSeed s0 = let s1 = s0 `xor` (s0 `shiftL` 13)
                  s2 = s1 `xor` (s1 `shiftR` 7)
              in s2 `xor` (s2 `shiftL` 17)

-- 種 s と番号 i から決まる 0〜1 の乱数（たくさんの粒子に一度に配るとき用）
hashRand :: Seed -> Int -> Float
hashRand s i =
  let z0 = s + fromIntegral i * 0x9E3779B97F4A7C15
      z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
      z3 = z2 `xor` (z2 `shiftR` 31)
  in fromIntegral (z3 `shiftR` 40) / 16777216
