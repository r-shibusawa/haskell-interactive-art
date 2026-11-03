-- ============================================
-- 07 巡回セールスマンを総当たりで解く
-- 都市を全部 1 回ずつ回って戻る、いちばん短い道。都市 n 個で巡回路は (n−1)!/2 通り
-- 総当たりは必ず正解。でも 1 都市増えるごとに時間が n 倍に
-- H モード：ハミルトン閉路 → TSP の還元。グラフの線は費用 0、線がない所は費用 1 として TSP を解く。最短 0 ⇔ ハミルトン閉路あり
-- クリック：都市を追加 / スペース：探索開始・停止 / 上下キー：速さ / R：ランダム / C：消す / H：還元モード切り替え
-- ============================================
module Main (main) where

import Art
import Control.Monad (filterM, replicateM)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

maxCities :: Int
maxCities = 10      -- ★ これ以上は待ちきれない

data World = World
  { wCities  :: [Point]
  , wGEdges  :: !(Set.Set (Int, Int))
  , wHam     :: !Bool
  , wCost    :: !(U.Vector Float)    -- 都市の間の費用（n × n の表）
  , wPerm    :: [Int]
  , wBest    :: Maybe [Int]
  , wBestLen :: !Float
  , wCount   :: !Int
  , wTotal   :: !Int
  , wRunning :: !Bool
  , wDone    :: !Bool
  , wSpeed   :: !Int
  }

main :: IO ()
main = runSketch (sketch "18-07 TSP Brute Force" emptyWorld)
  { skBack  = hsb 30 30 9
  , skStart = \env -> randomCities env 7 emptyWorld
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

emptyWorld :: World
emptyWorld = World [] Set.empty False U.empty [] Nothing 0 0 1 False False 20

randomCities :: Env -> Int -> World -> IO World
randomCities env n w = do
  cs <- replicateM n $ do
    x <- randomRIO (100, envW env - 100)
    y <- randomRIO (150, envH env - 80)
    pure (x, y)
  makeGraph env w { wCities = cs }

-- H モード用のグラフ（近い都市どうしをつなぐ）
makeGraph :: Env -> World -> IO World
makeGraph env w = do
  let cs = zip [0 ..] (wCities w)
      limit = min (envW env) (envH env) * 0.38
  es <- filterM (const (chance 0.75))
          [(i, j) | (i, a) <- cs, (j, b) <- cs, i < j, dist2 a b < limit]
  pure (reset w { wGEdges = Set.fromList es })

reset :: World -> World
reset w =
  let cs = wCities w
      n = length cs
      cost (i, a) (j, b)
        | wHam w = if Set.member (min i j, max i j) (wGEdges w) then 0 else 1
        | otherwise = dist2 a b
      indexed = zip [0 ..] cs
  in w { wCost = U.fromList [cost a b | a <- indexed, b <- indexed]
       , wPerm = [0 .. n - 1]
       , wBest = Nothing, wBestLen = 0, wCount = 0, wDone = False, wRunning = False
       , wTotal = product [2 .. n - 1] }   -- (n−1)!

tourLen :: U.Vector Float -> Int -> [Int] -> Float
tourLen cost n t = case t of
  [] -> 0
  (t0 : _) ->
    let go !acc (a : r@(b : _)) = go (acc + U.unsafeIndex cost (a * n + b)) r
        go !acc [a] = acc + U.unsafeIndex cost (a * n + t0)
        go !acc [] = acc
    in go 0 t

-- 都市 0 を固定し、残りを辞書順に並べかえる
nextPerm :: [Int] -> Maybe [Int]
nextPerm t = case t of
  (t0 : r) -> (t0 :) <$> nextTail r
  [] -> Nothing

nextTail :: [Int] -> Maybe [Int]
nextTail xs = split [] (reverse xs)
  where
    -- 後ろから見て、初めて小さくなる所（pivot）を探す。acc は後ろの部分（小さい順に並ぶ）
    split acc (a : b : rest)
      | b < a = Just (rebuild (reverse (a : acc)) b rest)
      | otherwise = split (a : acc) (b : rest)
    split _ _ = Nothing
    rebuild suffix pivot prefixRev =
      let (small, large) = span (< pivot) suffix
      in case large of
           (g : more) -> reverse prefixRev ++ [g] ++ small ++ [pivot] ++ more
           [] -> reverse prefixRev ++ [pivot] ++ suffix

runBatch :: Int -> World -> World
runBatch k w
  | k <= 0 = w
  | otherwise =
      let n = length (wCities w)
          perm = wPerm w
          l = tourLen (wCost w) n perm
          better = case wBest w of
            Nothing -> True
            Just _ -> l < wBestLen w - 1e-9
          w1 = (if better then w { wBestLen = l, wBest = Just perm } else w) { wCount = wCount w + 1 }
      in case nextPerm perm of
           Nothing -> w1 { wDone = True, wRunning = False }
           Just p -> runBatch (k - 1) w1 { wPerm = p }

stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wRunning w && length (wCities w) >= 3 = pure (runBatch (wSpeed w) w)
  | otherwise = pure w

drawW :: Env -> World -> Picture
drawW env w =
  let cities = zip [0 :: Int ..] (wCities w)
      posC i = concat [[p] | (k, p) <- cities, k == i]
      n = length cities
      running = wRunning w && n >= 3
      tour c th t =
        let ps = concatMap posC t
        in case ps of
             (p0 : _ : _) -> Color c (thickPath th (map (pt env) (ps ++ [p0])))
             _ -> Blank
      gEdge (a, b) = case (posC a, posC b) of
        ([p], [q]) -> seg env (hsba 120 50 70 0.6) 3 p q
        _ -> Blank
      city (i, p@(x, y)) = Pictures
        [ dot env (if i == 0 then hsb 0 80 100 else hsb 200 60 100) p 22
        , txtC env x y 14 (hsb 0 0 10) (show i)
        ]
      pct = if wTotal w > 0 then fix 1 (100 * fromIntegral (wCount w) / fromIntegral (wTotal w) :: Float) else "0"
      bestLine = case wBest w of
        Nothing -> Blank
        Just _ ->
          txt env 20 98 20 (hsb 0 0 85)
            ((if wHam w then "Min cost " ++ show (round (wBestLen w) :: Int) else "Shortest " ++ fix 1 (wBestLen w))
              ++ (if wDone w then " (final!)" else " (so far)"))
      hamLine
        | wHam w && wDone w =
            let zero = wBestLen w == 0
            in txt env 20 132 18 (if zero then hsb 120 80 100 else hsb 0 80 100)
                 (if zero then "Cost 0 tour = uses only green lines = a Hamiltonian cycle exists!"
                          else "Cost 0 is impossible: this graph has no Hamiltonian cycle")
        | otherwise = Blank
      sec = fromIntegral (wTotal w) / (fromIntegral (wSpeed w) * 60) :: Float
  in Pictures
       ( (if wHam w then map gEdge (Set.toList (wGEdges w)) else [])
      ++ [ if running then tour (hsba 0 0 100 0.25) 1.5 (wPerm w) else Blank
         , maybe Blank (tour (hsb 45 90 100) 5) (wBest w)
         ]
      ++ map city cities
      ++ [ txt env 20 34 15 (hsb 0 0 100) "Click: add city   Space: search   Up/Down: speed   R: random   C: clear   H: Hamiltonian -> TSP mode"
         , txt env 20 68 18 (hsb 45 80 100)
             (show n ++ " cities   tours " ++ commas (wCount w) ++ " / " ++ commas (wTotal w) ++ " (" ++ pct ++ "%)   speed " ++ show (wSpeed w) ++ "/frame")
         , bestLine
         , hamLine
         , txt env 20 (envH env - 22) 15 (hsb 0 0 60)
             ("At this speed, about " ++ fmtSec sec ++ " in all. One more city: x" ++ show n ++ ". 15 cities: over 10^10 tours")
         ] )

fmtSec :: Float -> String
fmtSec s
  | s < 60 = fix 1 s ++ " s"
  | s < 3600 = fix 1 (s / 60) ++ " min"
  | s < 86400 * 2 = fix 1 (s / 3600) ++ " hours"
  | otherwise = fix 0 (s / 86400) ++ " days"

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let m@(_, my) = mouseP5 env p
    in if my < 110 || length (wCities w) >= maxCities
         then pure w
         else makeGraph env w { wCities = wCities w ++ [m] }
  EventKey (SpecialKey KeySpace) Down _ _ ->
    let w1 = if wDone w then reset w else w
    in pure w1 { wRunning = not (wRunning w1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 5000 (wSpeed w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 1 (wSpeed w `div` 2) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> randomCities env 7 w
    | c `elem` "cC" -> pure (reset w { wCities = [], wGEdges = Set.empty })
    | c `elem` "hH" -> pure (reset w { wHam = not (wHam w) })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 都市 8・9・10 個で、全部調べるのにかかる時間を比べよう
-- 2. 暫定の最短路は、全体の何 % 調べたところで見つかることが多い？
-- 3. H モードで、ハミルトン閉路がないグラフ（R を何度か押す）を探そう

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

-- 3 桁ごとにカンマを入れる（toLocaleString）
commas :: Int -> String
commas v
  | v < 0 = '-' : commas (negate v)
  | v < 1000 = show v
  | otherwise = commas (v `div` 1000) ++ "," ++ drop 1 (show (1000 + v `mod` 1000))
