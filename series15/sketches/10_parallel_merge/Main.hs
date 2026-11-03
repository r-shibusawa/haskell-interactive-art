-- ============================================
-- 10 並列マージと並列マージソート
-- ふつうのマージは先頭から 1 つずつ → n ステップかかり、並列にできない
-- 並列マージ：長い方の真ん中 x を選び、短い方で x が入る場所を二分探索 → x の最終位置が決まる
-- 左右の 2 つの小さなマージは独立なので、同時に（spawn）できる。深さは O(lg n)、スパンは Θ(lg² n)
-- スペース = 1 段進む / A = 自動 / 上下 = 要素数 / R = 新しい数
-- ============================================
module Main (main) where

import Art
import Data.List (sort)
import qualified Data.Vector.Unboxed as U

-- 再帰呼び出しの記録
data CallR = CallR
  { cDepth :: !Int, cP1 :: !Int, cR1 :: !Int, cP2 :: !Int, cR2 :: !Int
  , cQ1 :: !Int, cQ2 :: !Int, cQ3 :: !Int }

data World = World
  { wN :: !Int
  , wT :: !(U.Vector Int)
  , wOut :: !(U.Vector Int)      -- まだ決まっていない場所は -1
  , wCalls :: [CallR]
  , wMaxDepth :: !Int
  , wLevel :: !Int
  , wAuto :: !Bool
  , wTimer :: !Int
  , wDepthByQ1 :: !(U.Vector Int)  -- その要素が真ん中に選ばれる段
  , wDepthByQ3 :: !(U.Vector Int)  -- その出力位置が決まる段
  }

main :: IO ()
main = runSketch (sketch "15-10 Parallel Merge" (World 32 U.empty U.empty [] 0 (-1) False 0 U.empty U.empty))
  { skBack  = hsb 100 25 9
  , skStart = \_ -> rebuild (World 32 U.empty U.empty [] 0 (-1) False 0 U.empty U.empty)   -- ★ 最初の要素数
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

rebuild :: World -> IO World
rebuild w = do
  let n = wN w
      nf' = fromIntegral n :: Float
  d <- randomRIO (-nf' / 6, nf' / 6)
  let h = n `div` 2 + floor d   -- ★ 片方の列の長さ（1 にするとどうなる？）
  a <- mapM (const (randomRIO (1, 99))) [1 .. h]
  b <- mapM (const (randomRIO (1, 99))) [1 .. n - h]
  let t = U.fromList (sort a ++ sort b)
      calls = pmerge t 0 (h - 1) h (n - 1) 0 0
      depthAt f = U.replicate n 0 U.// [(f c, cDepth c) | c <- calls]
  pure w { wT = t, wOut = U.replicate n (-1), wCalls = calls
         , wMaxDepth = foldr (max . cDepth) 0 calls, wLevel = -1, wAuto = False
         , wDepthByQ1 = depthAt cQ1, wDepthByQ3 = depthAt cQ3 }

-- T[p1..r1] と T[p2..r2]（どちらも整列済み）を出力の p3 以降にマージする。実際の計算は再帰の記録だけ
pmerge :: U.Vector Int -> Int -> Int -> Int -> Int -> Int -> Int -> [CallR]
pmerge t p1' r1' p2' r2' p3 depth =
  let longer = r1' - p1' >= r2' - p2'
      (p1, r1, p2, r2) = if longer then (p1', r1', p2', r2') else (p2', r2', p1', r1')
      n1 = r1 - p1 + 1
      q1 = (p1 + r1) `div` 2
      q2 = binarySearch t (t U.! q1) p2 r2
      q3 = p3 + (q1 - p1) + (q2 - p2)
  in if n1 <= 0 then [] else
       CallR depth p1 r1 p2 r2 q1 q2 q3
       : pmerge t p1 (q1 - 1) p2 (q2 - 1) p3 (depth + 1)      -- spawn：左側
       ++ pmerge t (q1 + 1) r1 q2 r2 (q3 + 1) (depth + 1)     -- 右側（同時に動ける）

-- T[p..r] で x 以上になる最初の位置
binarySearch :: U.Vector Int -> Int -> Int -> Int -> Int
binarySearch t x p r = go p (max p (r + 1))
  where
    go low high
      | low >= high = high
      | otherwise =
          let mid = (low + high) `div` 2
          in if x <= t U.! mid then go low mid else go (mid + 1) high

advance :: World -> World
advance w
  | wLevel w >= wMaxDepth w = w { wAuto = False }
  | otherwise =
      let l = wLevel w + 1
      in w { wLevel = l, wOut = wOut w U.// [(cQ3 c, wT w U.! cQ1 c) | c <- wCalls w, cDepth c == l] }

stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wAuto w =
      let t = wTimer w + 1
          w' = w { wTimer = t }
      in pure (if t `mod` 40 == 0 then advance w' else w')
  | otherwise = pure w

drawW :: Env -> World -> Picture
drawW env w =
  let n = wN w
      fi' :: Int -> Float
      fi' = fromIntegral
      x0 = 60
      bw = (envW env - 120) / fi' n
      level = wLevel w
      yIn = 300
      yOut = 560
      showNums = bw >= 30          -- 数字が入る幅があるときだけ書く
      cur = [c | c <- wCalls w, cDepth c == level]
      inFirst i = case wCalls w of
        (h : _) -> i >= min (cP1 h) (cP2 h) && i <= (if cP1 h < cP2 h then cR1 h else cR2 h)
        [] -> False
      inBar i =
        let d = wDepthByQ1 w U.! i
            v = wT w U.! i
            hh = fi' v * 1.6
            c | d == level = hsb 50 90 100
              | d < level = lum 25
              | inFirst i = hsb 195 60 85
              | otherwise = hsb 330 50 90
        in Pictures
             [ box env c (x0 + fi' i * bw + 1) (yIn - hh) (bw - 2) hh
             , if showNums then txtC env (x0 + (fi' i + 0.5) * bw) (yIn + 12) 14 (lum 80) (show v) else Blank ]
      -- 今の段の部分問題：短い方での二分探索の位置（線）
      callPic c =
        let x = x0 + fi' (cQ2 c) * bw
        in Pictures
             [ seg env (hsb 50 90 100) 2 (x, yIn - 170) (x, yIn + 20)
             , wire env (hsb 50 60 70) (x0 + fi' (cP1 c) * bw) (yIn - 172) (fi' (cR1 c - cP1 c + 1) * bw) 4
             , if cR2 c >= cP2 c
                 then wire env (hsb 50 60 70) (x0 + fi' (cP2 c) * bw) (yIn - 172) (fi' (cR2 c - cP2 c + 1) * bw) 4
                 else Blank ]
      outBar i =
        let v = wOut w U.! i
            d = wDepthByQ3 w U.! i
            hh = fi' v * 1.6
        in if v < 0
             then box env (lum 18) (x0 + fi' i * bw + 1) (yOut - 10) (bw - 2) 10
             else Pictures
               [ box env (if d == level then hsb 50 90 100 else hsb (wrapF 360 (fi' d * 45)) 60 90) (x0 + fi' i * bw + 1) (yOut - hh) (bw - 2) hh
               , if showNums then txtC env (x0 + (fi' i + 0.5) * bw) (yOut + 12) 14 (lum 80) (show v) else Blank ]
      -- 段ごとの数
      baseY = yOut + 92
      depthBar d =
        let k = length [c | c <- wCalls w, cDepth c == d]
            bh = min 60 (fi' k * 3)
            x = x0 + fi' d * 46
        in Pictures
             [ box env (if d <= level then hsb (wrapF 360 (fi' d * 45)) 60 90 else lum 30) x (baseY - bh) 40 bh
             , txtC env (x + 20) (baseY + 12) 14 (lum 70) (show k) ]
      lg = logBase 2 (fi' n)
      h = envH env
  in Pictures
       [ Pictures (map inBar [0 .. U.length (wT w) - 1])
       , Pictures (map callPic cur)
       , Pictures (map outBar [0 .. U.length (wOut w) - 1])
       , Pictures (map depthBar [0 .. wMaxDepth w])
       , txt env (x0 + fi' (wMaxDepth w + 1) * 46 + 10) baseY 14 (lum 70) "<- elements fixed at once per level"
       , txt env 20 36 18 (lum 100) ("Space: 1 level  A: auto  Up/Down: n = " ++ show n ++ "  R: new numbers")
       , txt env 20 62 15 (lum 65) "Blue, pink = two sorted runs. Yellow = middle of the longer run and its binary search spot in the shorter"
       , txt env 20 (h - 62) 19 (lum 100)
           ("Level " ++ show (level + 1) ++ " / " ++ show (wMaxDepth w + 1) ++ "  (a plain merge takes " ++ show n ++ " steps)")
       , txt env 20 (h - 38) 14 (lum 80)
           ("Parallel merge: work Theta(n), span Theta(lg^2 n) = about " ++ show (round (lg * lg) :: Int)
             ++ "    Parallel merge sort: work Theta(n lg n), span Theta(lg^3 n)")
       , txt env 20 (h - 16) 14 (hsb 45 80 100) "Each level halves the longer run and the total shrinks to at most 3/4 -> O(lg n) levels"
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (advance w)
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild w { wN = min 128 (wN w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild w { wN = max 8 (wN w `div` 2) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> rebuild w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. n を 2 倍にしても、段の数は少ししか増えない（ふつうのマージは 2 倍）
-- 2. 片方の列がとても短いときはどうなる？（rebuild の h を 1 にしてみる）
-- 3. これをマージソートの各段で使うと並列マージソート。07 の DAG のように描いてみよう

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 左上 (x, y)、幅 w、高さ h の枠線
wire :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
wire env c x y w h = Color c (rectLine (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 文字：txt は左下が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = label (gx env x - fromIntegral (length str) * s * 0.72) (gy env y) s c str

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0
