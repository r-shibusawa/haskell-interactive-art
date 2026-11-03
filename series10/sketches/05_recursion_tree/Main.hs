-- ============================================
-- 05 再帰木とマスター定理
-- 分割統治の計算時間 T(n) = a·T(n/b) + f(n) を「木」で描く。各段の横幅 = その段の仕事量
-- 段ごとに仕事が減る → 根（いちばん上）が支配。同じ → 段の数 log n 倍。増える → 葉（いちばん下）が支配
-- これがマスター定理の 3 つの場合。5・6 は分け方が不ぞろいな場合（アクラ＝バッジの方法）
-- ←→：a（いくつに分けるか）　↑↓：b（何分の 1 にするか）　1〜4：f(n) = 1, n, n^2, n^3
-- 5：T(n/3)+T(2n/3)+n　6：T(n/2)+T(n/4)+n　0：a・b の設定にもどる
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt)
import Data.List (foldl', intercalate, sortBy)
import qualified Data.Map.Strict as M
import Numeric (showFFloat)

bigN :: Double
bigN = 4096                -- ★ 木を描くときの n

data World = World
  { wA  :: !Int            -- ★ T(n) = A·T(n/B) + n^D
  , wB  :: !Int
  , wD  :: !Int
  , wAb :: !Int            -- 0 = マスター定理、5・6 = 不ぞろいの分割
  }

-- 「同じ大きさの問題が count 個」
data Node = Node { ndSize :: !Double, ndCount :: !Double }

main :: IO ()
main = runSketch (sketch "10-05 Recursion Tree" (World 2 2 1 0))
  { skBack  = hsb 285 30 10
  , skDraw  = drawW
  , skEvent = onEvent
  }

f :: World -> Double -> Double
f w sz = max sz 1 ** fromIntegral (wD w)

partsOf :: Int -> [Double]
partsOf ab = if ab == 5 then [1 / 3, 2 / 3] else [1 / 2, 1 / 4]

-- 段ごとのノード（大きさの列）を作る。数が多すぎるときは「同じ大きさが count 個」でまとめる
levelsOf :: World -> [[Node]]
levelsOf w
  | wAb w == 0 =
      let go :: Int -> Double -> [[Node]]
          go depth size
            | size >= 1 && depth < 14 =
                [Node size (fromIntegral (wA w) ^ depth)] : go (depth + 1) (size / fromIntegral (wB w))
            | otherwise = []
      in go 0 bigN
  | otherwise =
      let parts = partsOf (wAb w)
          go :: Int -> [Node] -> [[Node]]
          go depth cur
            | depth >= 12 || null cur = []
            | otherwise = cur : go (depth + 1) (nextLevel parts cur)
      in go 0 [Node bigN 1]

-- 次の段：同じ大きさ（小数 6 けたまで同じ）のノードをまとめる
nextLevel :: [Double] -> [Node] -> [Node]
nextLevel parts cur =
  let add m (Node s c) = M.insertWith merge (round (s * 1e6) :: Int) (Node s c) m
      merge (Node s c1) (Node _ c2) = Node s (c1 + c2)
      children = [Node (ndSize nd * p) (ndCount nd) | nd <- cur, p <- parts, ndSize nd * p >= 1]
      table = foldl' add M.empty children
  in sortBy (\x y -> compare (ndSize y) (ndSize x)) (M.elems table)

levelCost :: World -> [Node] -> Double
levelCost w = foldl' (\s nd -> s + ndCount nd * unitCost w nd) 0

unitCost :: World -> Node -> Double
unitCost w nd = if wAb w /= 0 then ndSize nd else f w (ndSize nd)

-- 段の中の各ノードの幅（多いときは最大 256 個に区切って描く）
segsOf :: World -> Double -> Double -> [Node] -> [Double]
segsOf w maxC wd = go (0 :: Int)
  where
    go _ [] = []
    go acc (nd : rest)
      | acc > 512 = []
      | otherwise =
          let k = min (ndCount nd) 256
              ki = round k :: Int
              one = unitCost w nd * ndCount nd / k / maxC * wd
          in replicate ki one ++ go (acc + ki) rest

drawW :: Env -> World -> Picture
drawW env w =
  let levels = levelsOf w
      costs = map (levelCost w) levels
      maxC = foldl' max 1e-9 costs
      l = 60
      wd = envW env - 420
      t = 140
      rowH = min 48 ((envH env - t - 60) / fromIntegral (max 1 (length levels)))
      level (d, lv, cost) =
        let y = t + fromIntegral d * rowH
            total = realToFrac (cost / maxC) * wd
            x0 = l + (wd - total) / 2
            sws = segsOf w maxC (realToFrac wd) lv
            xs = scanl (+) x0 (map realToFrac sws)
            c = hsb (wrapF 360 (200 + fromIntegral d * 14)) 60 90
            seg1 (x, sw0) =
              let sw = realToFrac sw0 :: Float
              in box env c x y (max 0.5 (sw - (if sw > 4 then 1.5 else 0))) (rowH - 10)
            cnt = foldl' (\a nd -> a + ndCount nd) 0 lv
            yc = y + (rowH - 10) / 2 + 7
        in Pictures
             [ Color c (Pictures (map seg1 (zip xs sws)))
             , txt env (l + wd + 20) yc 14 (hsb 0 0 75) ("depth " ++ show (d :: Int) ++ ": " ++ fmt cnt)
             , txt env (l + wd + 175) yc 14 (hsb 0 0 75) ("work " ++ fmt cost)
             ]
      (ans, why) = verdict w
  in Pictures
       [ Pictures (map level (zip3 [0 ..] levels costs))
       , txt env 20 40 22 (hsb 0 0 100) (formula w ++ "   ->   " ++ ans)
       , txt env 20 68 16 (hsb 50 80 100) why
       , txt env 20 94 14 (hsb 0 0 60)
           "Left/Right a   Up/Down b   1-4 f(n) = n^0..n^3   5 6 uneven split   0 back   (bar width = total work of that level)"
       , txt env l (envH env - 26) 14 (hsb 0 0 70)
           ("Total time = sum of all levels = " ++ fmt (sum costs) ++ "  (n = " ++ show (round bigN :: Int) ++ ")")
       ]

fName :: Int -> String
fName d = case d of
  0 -> "1"
  1 -> "n"
  2 -> "n^2"
  _ -> "n^3"

formula :: World -> String
formula w = case wAb w of
  5 -> "T(n) = T(n/3) + T(2n/3) + n"
  6 -> "T(n) = T(n/2) + T(n/4) + n"
  _ -> "T(n) = " ++ (if wA w == 1 then "" else show (wA w)) ++ "T(n/" ++ show (wB w) ++ ") + " ++ fName (wD w)

verdict :: World -> (String, String)
verdict w
  | wAb w /= 0 =
      -- Σ b_i^p = 1 を満たす p を二分法で求める
      let parts = partsOf (wAb w)
          bisect :: Int -> Double -> Double -> Double
          bisect 0 lo hi = (lo + hi) / 2
          bisect i lo hi =
            let p = (lo + hi) / 2
                s = sum (map (** p) parts)
            in if s > 1 then bisect (i - 1) p hi else bisect (i - 1) lo p
          p' = bisect 60 (-2) 4
      in if abs (p' - 1) < 1e-6
           then ("Theta(n log n)", "Akra-Bazzi: sum b_i^p = 1 gives p = 1, same as f(n) = n -> each level is about n, times log n levels")
           else if p' < 1
             then ("Theta(n)", "Akra-Bazzi: p = " ++ fixed 3 p' ++ " < 1 -> work shrinks each level (geometric), the root's n dominates")
             else ("Theta(n^" ++ fixed 2 p' ++ ")", "Akra-Bazzi: p = " ++ fixed 3 p' ++ " > 1 -> the leaves dominate")
  | otherwise =
      let c = log (fromIntegral (wA w)) / log (fromIntegral (wB w)) :: Double
          cs = fixed 2 c
          d = wD w
          df = fromIntegral d :: Double
      in if abs (c - df) < 1e-9
           then ( if d == 0 then "Theta(log n)" else "Theta(n" ++ (if d == 1 then "" else "^" ++ show d) ++ " log n)"
                , "Case 2: log_b a = " ++ cs ++ " equals degree " ++ show d ++ " of f -> every level has equal work, times log n levels" )
           else if c > df
             then ( "Theta(n^" ++ cs ++ ")"
                  , "Case 1: log_b a = " ++ cs ++ " > " ++ show d ++ " -> lower levels do more, the n^" ++ cs ++ " leaves dominate" )
             else ( "Theta(" ++ fName d ++ ")"
                  , "Case 3: log_b a = " ++ cs ++ " < " ++ show d ++ " -> lower levels do less, the root's f(n) dominates" )

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wA = min 9 (wA w + 1), wAb = 0 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wA = max 1 (wA w - 1), wAb = 0 }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wB = min 4 (wB w + 1), wAb = 0 }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wB = max 2 (wB w - 1), wAb = 0 }
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> pure w { wD = digitToInt c - 1, wAb = 0 }
    | c `elem` "56" -> pure w { wAb = digitToInt c, wD = 1 }
    | c == '0' -> pure w { wAb = 0 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. マージソート T(n) = 2T(n/2) + n、二分探索 T(n) = T(n/2) + 1、シュトラッセン 7T(n/2) + n^2 を作って答えを確かめる
-- 2. 8T(n/2) + n^2 は場合いくつ？ 横幅の変わり方で説明しよう
-- 3. 5 の T(n/3) + T(2n/3) + n は、木の左右で深さがちがう。それでも各段の仕事が n になる理由は？

-- ---------- 道具：数の表示 ----------
fixed :: Int -> Double -> String
fixed d v = showFFloat (Just d) v ""

fmt :: Double -> String
fmt v
  | v >= 1e6 =
      let e = floor (logBase 10 v + 1e-9) :: Int   -- 1e-9 は計算誤差の補正
      in fixed 1 (v / 10 ** fromIntegral e) ++ "e" ++ show e
  | otherwise = comma (round v)

comma :: Int -> String
comma v = intercalate "," (reverse (map reverse (chunks (reverse (show v)))))
  where
    chunks [] = []
    chunks s = let (a, b) = splitAt 3 s in a : chunks b

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> (Float, Float) -> (Float, Float)
pt env (x, y) = (gx env x, gy env y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> (Float, Float) -> (Float, Float) -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 文字：txt は左下が (x, y)、txtC は中心が (x, y)
txt, txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
