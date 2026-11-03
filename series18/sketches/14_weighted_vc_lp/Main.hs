-- ============================================
-- 14 重みつき頂点被覆：線形計画で緩めて、丸める
-- 点に重さ（費用）がある。費用の合計が最小の頂点被覆は NP 困難
-- (1) 「取る = 1／取らない = 0」を「0〜1 の小数 x」に緩める（線形計画法：各線で x_u + x_v ≥ 1）→ 多項式時間で解ける
-- (2) x ≥ 1/2 の点を全部取る（丸め）→ どの線も片方は 1/2 以上なので被覆になり、費用は LP の 2 倍以下 ≤ 最適の 2 倍
-- 実はこの LP の最適解は 0, 1/2, 1 だけで作れる（だからここでは 3^n 通りを調べて LP を解いている）
-- スペース：解く / 点をクリック：重さを 1→2→…→6 / R：ランダム / 1：三角形（LP が 1/2 だらけになる例）
-- ============================================
module Main (main) where

import Art
import Control.Monad (filterM, forM)
import Data.List (findIndex)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

nPoints :: Int
nPoints = 9         -- ★ 点の数（LP は 3^n、整数の最適は 2^n 通りを調べる）

data Node = Node { nX :: !Float, nY :: !Float, nW :: !Int }

-- 解：x は 2 倍した値（0, 1, 2 = 0, 1/2, 1）
data Sol = Sol { solX :: !(U.Vector Int), solCost :: !Float }

-- ジェネレーターが進める状態
data S = S
  { sLp   :: Maybe Sol
  , sIp   :: Maybe Sol
  , sProg :: !Float
  }

data World = World
  { wNodes :: [Node]
  , wEdges :: [(Int, Int)]
  , wS     :: !S
  , wRest  :: [S]
  , wGen   :: !Bool
  }

emptyS :: S
emptyS = S Nothing Nothing 1

main :: IO ()
main = runSketch (sketch "18-14 LP Relax and Round" (World [] [] emptyS [] False))
  { skBack  = hsb 190 30 9
  , skStart = randomGraph
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

randomGraph :: Env -> IO World
randomGraph env = do
  let n = nPoints
      cx = envW env / 2
      cy = envH env * 0.55
      r = min (envW env * 0.3) (envH env * 0.32)
  nodes <- forM [0 .. n - 1] $ \i -> do
    fx <- randomRIO (0.6, 1)
    fy <- randomRIO (0.6, 1)
    wt <- randomRIO (1, 6)
    let a = tau * fromIntegral i / fromIntegral n
    pure (Node (cx + cos a * r * fx) (cy + sin a * r * fy) wt)
  es <- filterM (const (chance 0.3)) [(i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1]]
  pure (World nodes (if null es then [(0, 1)] else es) emptyS [] False)

triangles :: Env -> World
triangles env =
  let cx = envW env / 2
      cy = envH env * 0.55
      r = min (envW env) (envH env) * 0.28
      nodes =
        [ Node (cx + cos a * r + cos b * r * 0.35) (cy + sin a * r + sin b * r * 0.35) 1
        | t <- [0 .. 2 :: Int], k <- [0 .. 2 :: Int]
        , let a = tau * fromIntegral t / 3
        , let b = a + tau * fromIntegral k / 3 + pi / 2 ]
      es = concat [[(3 * t, 3 * t + 1), (3 * t + 1, 3 * t + 2), (3 * t, 3 * t + 2)] | t <- [0 .. 2]]
  in World nodes es emptyS [] False

reset :: World -> World
reset w = w { wS = emptyS, wRest = [], wGen = False }

solveAll :: [Node] -> [(Int, Int)] -> Gen S ()
solveAll nodes es =
  let n = length nodes
      weights = U.fromList (map (fromIntegral . nW) nodes) :: U.Vector Float
      total = 3 ^ n :: Int
      decode code = U.unfoldrN n (\c -> Just (c `mod` 3, c `div` 3)) code
      feasible x = all (\(a, b) -> x U.! a + x U.! b >= 2) es
      cost x = U.sum (U.zipWith (\v wt -> fromIntegral v / 2 * wt) x weights)
      go code best bestI
        | code >= total = modify (\s -> s { sLp = best, sIp = bestI })
        | otherwise = do
            let x = decode code
                (best', bestI') =
                  if feasible x
                    then
                      let c = cost x
                          b1 = case best of
                            Just b | c >= solCost b - 1e-6 -> best
                            _ -> Just (Sol x c)
                          b2 = case bestI of
                            Just b | c >= solCost b -> bestI
                            _ | U.all (/= 1) x -> Just (Sol x c)
                              | otherwise -> bestI
                      in (b1, b2)
                    else (best, bestI)
            if code `mod` 400 == 0
              then do
                modify (\s -> s { sLp = best', sProg = fromIntegral code / fromIntegral total })
                yield
              else pure ()
            go (code + 1) best' bestI'
  in go 0 Nothing Nothing

-- 1 フレームに 1 回 yield まで進める（400 通りずつ）
stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wGen w = pure $ case wRest w of
      (s : r) -> w { wS = s, wRest = r }
      [] -> w { wGen = False }
  | otherwise = pure w

-- 点の中の「水位」：下から v の高さまで塗る
arcPoly :: Float -> Float -> Float -> Float -> [Point]
arcPoly x y r v =
  [ (x + r * cos a, y + r * sin a)
  | k <- [0 .. 32 :: Int]
  , let a = pi / 2 - pi * v + 2 * pi * v * fromIntegral k / 32 ]

showVal :: Int -> String
showVal v = case v of
  1 -> "1/2"
  2 -> "1"
  _ -> "0"

-- 整数なら小数点なしで表示
showNum :: Float -> String
showNum v
  | v == fromIntegral (round v :: Int) = show (round v :: Int)
  | otherwise = fix 1 v

drawW :: Env -> World -> Picture
drawW env w =
  let s = wS w
      gen = wGen w
      lp = sLp s
      ip = sIp s
      valueAt sol i = solX sol U.! i
      rounded i = maybe False (\l -> valueAt l i >= 1) lp
      posOf i = case drop i (wNodes w) of
        (p : _) -> (nX p, nY p)
        [] -> (0, 0)
      edge (a, b) = seg env (hsba 0 0 100 0.35) 2 (posOf a) (posOf b)
      node (i, p) =
        let d = 26 + fromIntegral (nW p) * 7
            pos@(x, y) = (nX p, nY p)
            v = maybe 0 (`valueAt` i) lp
            level = fromIntegral v / 2 :: Float
            water
              | v == 2 = dot env (hsb 45 85 100) pos d
              | v == 1 = Color (hsb 190 70 100) (Polygon (map (pt env) (arcPoly x y (d / 2) level)))
              | otherwise = Blank
            ipRing = case ip of
              Just q | not gen && valueAt q i == 2 ->
                let r = (d + 14) / 2
                    arcOf k =
                      let a0 = tau * fromIntegral (k :: Int) / 12
                          a1 = a0 + tau / 12 * 0.6
                      in seg env (hsb 280 70 100) 3 (x + r * cos a0, y + r * sin a0) (x + r * cos a1, y + r * sin a1)
                in Pictures (map arcOf [0 .. 11])
              _ -> Blank
            roundRing
              | rounded i && not gen = Color (hsb 130 80 100) (at (pt env pos) (ring ((d + 4) / 2) 4))
              | otherwise = Blank
            valueText = case lp of
              Just _ -> txtC env x y 15 (hsb 0 0 100) (showVal v)
              Nothing -> Blank
        in Pictures
             [ dot env (hsb 0 0 25) pos d
             , water
             , ipRing
             , roundRing
             , txtC env x (y - d / 2 - 14) 14 (hsb 0 0 100) ("w " ++ show (nW p))
             , valueText
             ]
      progressLine
        | gen = txt env 20 68 18 (hsb 45 80 100) ("Solving the LP... " ++ show (round (sProg s * 100) :: Int) ++ "%")
        | otherwise = Blank
      results = case lp of
        Just l | not gen ->
          let rc = sum [nW p | (i, p) <- zip [0 ..] (wNodes w), rounded i]
              ipS = maybe "?" (showNum . solCost) ip
              c = fix 1 (solCost l)
          in [ txt env 20 70 18 (hsb 190 70 100) ("1) LP (fractions allowed) min cost = " ++ c ++ "   (blue = 1/2, yellow = 1)")
             , txt env 20 98 18 (hsb 280 60 100) ("    True optimum (0/1 only) = " ++ ipS ++ "   (purple dashes)")
             , txt env 20 126 18 (hsb 130 70 100)
                 ("2) Take all >= 1/2 = " ++ show rc ++ " (green ring):  " ++ c ++ " <= " ++ ipS ++ " <= " ++ show rc
                   ++ " <= 2 x " ++ c ++ " = " ++ fix 1 (2 * solCost l))
             ]
        _ -> []
  in Pictures
       ( map edge (wEdges w)
      ++ map node (zip [0 ..] (wNodes w))
      ++ [ txt env 20 34 16 (hsb 0 0 100) "Space: solve   Click a point: change weight   R: random   1: three triangles"
         , progressLine
         ]
      ++ results
      ++ [ txt env 20 (envH env - 44) 15 (hsb 0 0 60) "The LP is a relaxation, so LP <= optimum. Rounding at most doubles each value: a 2-approximation."
         , txt env 20 (envH env - 20) 15 (hsb 0 0 60) "One triangle: LP = 1.5, optimum = 2 (the integrality gap)."
         ] )

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let m = mouseP5 env p
        hitNode q = dist2 m (nX q, nY q) < 20 + fromIntegral (nW q) * 4
    in pure $ case findIndex hitNode (wNodes w) of
         Just k -> reset w { wNodes = [if i == k then q { nW = nW q `mod` 6 + 1 } else q | (i, q) <- zip [0 ..] (wNodes w)] }
         Nothing -> w
  EventKey (SpecialKey KeySpace) Down _ _
    | not (wGen w) -> pure w { wRest = steps (solveAll (wNodes w) (wEdges w) >> yield) (wS w) { sProg = 1 }, wGen = True }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> randomGraph env
    | c == '1' -> pure (triangles env)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 三角形（1）で LP の値が全部 1/2 になるのを確かめよう。丸めると全部取る（費用 9、最適 6）
-- 2. 重さを極端に（6 と 1）してみよう。LP はどこに 1 を置く？
-- 3. 丸めのしきい値を 1/2 以外（例えば 0.6）にすると、被覆にならないことがある。なぜ？

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- gloss のマウス座標を p5.js の座標に直す
mouseP5 :: Env -> Point -> Point
mouseP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 直径 d の円（p5.js の circle）
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

-- 文字：左端の下が (x, y)
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str

-- 文字：中心が (x, y)
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = labelC (gx env x) (gy env y) s c str

-- ---------- 道具：その他 ----------
-- 確率 p で True
chance :: Float -> IO Bool
chance p = (< p) <$> randomRIO (0, 1)

-- 小数点以下 d 桁で表示（p5.js の nf）
fix :: RealFloat a => Int -> a -> String
fix d v = showFFloat (Just d) v ""

-- ---------- 道具：ジェネレーター（function*）の代わり ----------
-- yield のたびに状態を 1 つ流す計算。結果は「状態の遅延リスト」になり、
-- World がその残りを持っていて、1 手進めるたびに先頭を 1 つ取る。
newtype Gen s a = Gen { unGen :: s -> (a -> s -> [s]) -> [s] }

instance Functor (Gen s) where
  fmap f (Gen g) = Gen (\s k -> g s (k . f))

instance Applicative (Gen s) where
  pure x = Gen (\s k -> k x s)
  Gen f <*> Gen g = Gen (\s k -> f s (\h s' -> g s' (k . h)))

instance Monad (Gen s) where
  Gen g >>= f = Gen (\s k -> g s (\x s' -> unGen (f x) s' k))

yield :: Gen s ()
yield = Gen (\s k -> s : k () s)

modify :: (s -> s) -> Gen s ()
modify f = Gen (\s k -> k () $! f s)

steps :: Gen s () -> s -> [s]
steps (Gen g) s0 = g s0 (\_ _ -> [])
