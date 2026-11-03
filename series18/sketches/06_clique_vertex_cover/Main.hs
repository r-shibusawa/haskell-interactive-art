-- ============================================
-- 06 クリーク ↔ 頂点被覆（補グラフで裏返す）
-- 左：グラフ G。右：補グラフ G-bar（G で線が「ない」所にだけ線を引いたもの）
-- C が G のクリーク（全員つながっている）⇔ 残り V − C が補グラフの頂点被覆（どの線も少なくとも片方の端を含む）
-- だから「大きさ k のクリーク」と「大きさ n − k の頂点被覆」は同じ問題（CLIQUE から VERTEX-COVER への還元）
-- 点をクリック：C に入れる・外す / 左のグラフで点から点へドラッグ：線をつける・消す
-- スペース：最大クリーク（＝最小頂点被覆）を総当たり / R：ランダムなグラフ / C：選択を消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (filterM)
import Data.Bits (shiftL, testBit)
import Data.List (find, intercalate)
import qualified Data.Set as Set

n :: Int
n = 8               -- ★ 点の数（総当たりは 2^n 通り）

edgeP :: Float
edgeP = 0.5         -- ★ R で線を引く確率

allPairs :: [(Int, Int)]
allPairs = [(i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1]]

data World = World
  { wA    :: !(Set.Set (Int, Int))   -- 線の集合（小さい番号を先に）
  , wInC  :: [Bool]
  , wDrag :: !Int                    -- ドラッグを始めた点（-1 = なし）
  , wBrute :: !Int                   -- 総当たりの番号（-1 = 休み）
  , wBest :: Maybe ([Bool], Int)
  }

main :: IO ()
main = runSketch (sketch "18-06 Clique and Vertex Cover" (World Set.empty (replicate n False) (-1) (-1) Nothing))
  { skBack  = hsb 300 25 9
  , skStart = const randomGraph
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

randomGraph :: IO World
randomGraph = do
  es <- filterM (const (chance edgeP)) allPairs
  pure (World (Set.fromList es) (replicate n False) (-1) (-1) Nothing)

hasEdge :: World -> Int -> Int -> Bool
hasEdge w i j = Set.member (min i j, max i j) (wA w)

center :: Env -> Int -> (Float, Float, Float)
center env side =
  ( envW env * (if side == 0 then 0.27 else 0.73)
  , envH env * 0.55
  , min (envW env * 0.2) (envH env * 0.3) )

xy :: Env -> Int -> Int -> Point
xy env side i =
  let (cx, cy, r) = center env side
      a = tau * fromIntegral i / fromIntegral n - pi / 2
  in (cx + cos a * r, cy + sin a * r)

at' :: [Bool] -> Int -> Bool
at' s i = or (take 1 (drop i s))

isClique :: World -> [Bool] -> Bool
isClique w s = and [hasEdge w i j | (i, j) <- allPairs, at' s i, at' s j]

isCover :: World -> [Bool] -> Bool
isCover w s = and [at' s i || at' s j | (i, j) <- allPairs, not (hasEdge w i j)]

-- 総当たり：1 フレームに 4 通り
bruteOne :: World -> World
bruteOne w
  | wBrute w < 0 || wBrute w >= total = w
  | otherwise =
      let b = wBrute w
          s = [testBit b i | i <- [0 .. n - 1]]
          cnt = length (filter id s)
          better = isClique w s && maybe True ((cnt >) . snd) (wBest w)
      in w { wBrute = b + 1, wInC = s, wBest = if better then Just (s, cnt) else wBest w }
  where total = 1 `shiftL` n

stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wBrute w < 0 = pure w
  | otherwise =
      let w1 = iterate bruteOne w !! 4
      in pure $ if wBrute w1 >= 1 `shiftL` n
           then w1 { wBrute = -1, wInC = maybe (wInC w1) fst (wBest w1) }
           else w1

drawW :: Env -> World -> Picture
drawW env w =
  let inC = wInC w
      cover = map not inC
      cq = isClique w inC
      cv = isCover w cover
      edgesOf side =
        [ if side == 0
            then
              let hot = at' inC i && at' inC j
              in seg env (if hot then hsb 50 90 100 else hsba 0 0 100 0.25) (if hot then 4 else 2) a b
            else
              let ok = at' cover i || at' cover j
              in seg env (if ok then hsba 190 70 100 0.7 else hsb 0 90 100) (if ok then 2.5 else 5) a b
        | (i, j) <- allPairs
        , (side == 0) == hasEdge w i j
        , let a = xy env side i
        , let b = xy env side j ]
      nodesOf side =
        [ Pictures
            [ dot env (hsb (if side == 0 then 50 else 190) (if on then 85 else 15) (if on then 100 else 45)) p (if on then 44 else 36)
            , txtC env x y 15 (hsb 0 0 10) (show i)
            ]
        | i <- [0 .. n - 1]
        , let p@(x, y) = xy env side i
        , let on = at' (if side == 0 then inC else cover) i ]
      dragLine
        | wDrag w >= 0 = seg env (hsb 0 0 100) 2 (xy env 0 (wDrag w)) (mouseP5 env (envMouse env))
        | otherwise = Blank
      k = length (filter id inC)
      members = [show i | (i, True) <- zip [0 :: Int ..] inC]
      (c0x, c0y, c0r) = center env 0
      (c1x, c1y, c1r) = center env 1
      good = hsb 120 80 100
      bad = hsb 0 80 100
      status
        | wBrute w >= 0 =
            txt env 20 90 16 (hsb 45 80 100) ("Brute force... " ++ show (wBrute w) ++ " / " ++ show (1 `shiftL` n :: Int))
        | Just (_, c) <- wBest w =
            txt env 20 90 16 (hsb 45 80 100) ("Max clique " ++ show c ++ " <=> min vertex cover " ++ show (n - c))
        | otherwise = Blank
  in Pictures
       ( edgesOf 0 ++ nodesOf 0 ++ edgesOf 1 ++ nodesOf 1
      ++ [ dragLine
         , txtC env c0x (c0y + c0r + 60) 18 (hsb 50 80 100) ("G: C = {" ++ intercalate "," members ++ "}")
         , txtC env c1x (c1y + c1r + 60) 18 (hsb 190 70 100) ("Complement: V - C (" ++ show (n - k) ++ ")")
         , txtC env c0x (c0y + c0r + 92) 18 (if cq then good else bad)
             (if cq then "Clique of size " ++ show k ++ " (OK)" else "Not a clique (X)")
         , txtC env c1x (c1y + c1r + 92) 18 (if cv then good else bad)
             (if cv then "Vertex cover of size " ++ show (n - k) ++ " (OK)" else "Not a cover: red edges (X)")
         , txt env 20 34 15 (hsb 0 0 100) "Click a point: in/out of C   Drag on the left: toggle edge   Space: brute force   R: random   C: clear"
         , txt env 20 62 16 (hsb 0 0 65) "OK and X always change together. Solve one fast and you solve the other fast."
         , status
         ] )

hit :: Env -> Point -> Maybe (Int, Int)
hit env m = find (\(side, i) -> dist2 m (xy env side i) < 22) [(s, i) | s <- [0, 1], i <- [0 .. n - 1]]

toggleC :: Int -> World -> World
toggleC i w = w { wInC = [if k == i then not b else b | (k, b) <- zip [0 ..] (wInC w)], wBest = Nothing }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    pure $ case hit env (mouseP5 env p) of
      Just (0, i) -> w { wDrag = i }
      Just (_, i) -> (toggleC i w) { wDrag = -1 }
      Nothing -> w { wDrag = -1 }
  EventKey (MouseButton LeftButton) Up _ p
    | wDrag w >= 0 ->
        let from = wDrag w
            w1 = w { wDrag = -1 }
        in pure $ case hit env (mouseP5 env p) of
             Just (0, i)
               | i == from -> toggleC i w1
               | otherwise ->
                   let e = (min from i, max from i)
                       a = wA w1
                   in w1 { wA = if Set.member e a then Set.delete e a else Set.insert e a, wBest = Nothing }
             _ -> w1
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wBrute = 0, wBest = Nothing }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> randomGraph
    | c `elem` "cC" -> pure w { wInC = replicate n False, wBest = Nothing, wBrute = -1 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 左で 3 人組のクリークを選ぶ。右で残りの点が赤い線を全部消しているか確かめよう
-- 2. 全員を選ぶ（C = V）と右はどうなる？ 誰も選ばないと？
-- 3. n を 12 にして総当たり。何通り調べる？ 時間は？

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
