-- ============================================
-- 07 優先度つきキュー
-- 「いちばん急ぎの仕事」をすぐ取り出せる箱。中身は最大ヒープ（06）
-- INSERT：末尾に入れて、親より大きければ上へ（Θ(log n)）
-- EXTRACT-MAX：根を取り出し、末尾を根に置いて沈める（Θ(log n)）
-- INCREASE-KEY：値を大きくして上へ。病院の救急、OS のタスク、イベントのシミュレーションで使う
-- クリック：ランダムな仕事を追加　スペース：最大を取り出す　ノードをクリック：+30（INCREASE-KEY）
-- A：自動（ランダムに来て、順に処理）　C：空にする
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import qualified Data.Vector as V

-- 仕事：優先度と名前
data Task = Task { tP :: !Int, tName :: !String }

data World = World
  { wHeap  :: !(V.Vector Task)
  , wLog   :: ![String]        -- 取り出した順（新しいものが先頭）
  , wAuto  :: !Bool
  , wAnim  :: ![[Int]]         -- 入れかえの記録（新しいものが先頭）
  , wTimer :: !Int
  }

names :: V.Vector String
names = V.fromList ["Mail", "Meet", "Fix", "Call", "Docs", "Talk", "Test", "Ship", "Calc", "Print", "Chat", "Wash"]

main :: IO ()
main = runSketch (sketch "10-07 Priority Queue" (World V.empty [] False [] 0))
  { skBack  = hsb 170 30 10
  , skStart = \_ -> do
      w <- foldM (\w _ -> randomPriority >>= \p -> insert p w) (World V.empty [] False [] 0) [1 .. 7 :: Int]
      pure w { wAnim = [] }
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

randomPriority :: IO Int
randomPriority = randomRIO (1, 99)

parent :: Int -> Int
parent i = (i - 1) `div` 2

swapAt :: Int -> Int -> V.Vector Task -> V.Vector Task
swapAt i j h = h V.// [(i, h V.! j), (j, h V.! i)]

-- 親より大きければ上へ（入れかえの記録も返す）
siftUp :: Int -> V.Vector Task -> [[Int]] -> (V.Vector Task, [[Int]])
siftUp i h an
  | i > 0 && tP (h V.! parent i) < tP (h V.! i) =
      siftUp (parent i) (swapAt i (parent i) h) ([i, parent i] : an)
  | otherwise = (h, an)

-- 小さい子より小さければ下へ
siftDown :: Int -> V.Vector Task -> [[Int]] -> (V.Vector Task, [[Int]])
siftDown i h an =
  let l = 2 * i + 1
      r = 2 * i + 2
      len = V.length h
      m1 = if l < len && tP (h V.! l) > tP (h V.! i) then l else i
      m2 = if r < len && tP (h V.! r) > tP (h V.! m1) then r else m1
  in if m2 == i then (h, an) else siftDown m2 (swapAt i m2 h) ([i, m2] : an)

insert :: Int -> World -> IO World
insert p w = do
  k <- randomRIO (0, V.length names - 1)
  let h = V.snoc (wHeap w) (Task p (names V.! k))
      i = V.length h - 1
      (h', an) = siftUp i h ([i] : wAnim w)
  pure w { wHeap = h', wAnim = an }

extractMax :: World -> World
extractMax w
  | V.null h = w { wLog = take 12 ("(empty)" : wLog w) }
  | otherwise =
      let top = V.head h
          lastT = V.last h
          rest = V.init h
          (h', an)
            | V.null rest = (rest, wAnim w)
            | otherwise = siftDown 0 (rest V.// [(0, lastT)]) (wAnim w)
      in w { wHeap = h', wAnim = an
           , wLog = take 12 (("Done: " ++ tName top ++ " (priority " ++ show (tP top) ++ ")") : wLog w) }
  where h = wHeap w

increaseKey :: Int -> Int -> World -> World
increaseKey i delta w =
  let h = wHeap w
      t = h V.! i
      h1 = h V.// [(i, t { tP = min 999 (tP t + delta) })]
      (h2, an) = siftUp i h1 (wAnim w)
  in w { wHeap = h2, wAnim = an }

-- i 番目のノードは木の何段目か
depthOf :: Int -> Int
depthOf i = length (takeWhile (<= i + 1) (iterate (* 2) 2))

pos :: Env -> Int -> (Float, Float)
pos env i =
  let depth = depthOf i
      cnt = 2 ^ depth :: Int
      idx = i - (cnt - 1)
      wd = envW env - 420
  in (40 + wd * (fromIntegral idx + 0.5) / fromIntegral cnt, 150 + fromIntegral depth * 80)

stepW :: Env -> Float -> World -> IO World
stepW env _ w0 = do
  w1 <- if wAuto w0 then autoStep w0 { wTimer = wTimer w0 + 1 } else pure w0
  pure (if envFrame env `mod` 20 == 0 then w1 { wAnim = [] } else w1)

-- ★ 40 と 55 を変えると、来る速さと処理の速さが変わる
autoStep :: World -> IO World
autoStep w = do
  r <- randomRIO (0, 1 :: Float)
  w1 <- if wTimer w `mod` 40 == 0 && r < 0.6
          then randomPriority >>= \p -> insert p w
          else pure w
  let w2 = if wTimer w1 `mod` 55 == 0 then extractMax w1 else w1
  pure (if V.length (wHeap w2) > 30 then extractMax w2 else w2)

drawW :: Env -> World -> Picture
drawW env w =
  let h = wHeap w
      len = V.length h
      flash = case wAnim w of
        (x : _) -> x
        [] -> []
      edge i = seg env (hsb 0 0 40) 2 (pos env (parent i)) (pos env i)
      node i =
        let q@(x, y) = pos env i
            t = h V.! i
            d = 4 - min 3 (depthOf i)
            sz = 22 + fromIntegral d * 8 :: Float
            hue = max 0 (remap (fromIntegral (tP t)) 0 120 200 0)
            c = if i `elem` flash then hsb 45 100 100 else hsb hue 70 95
            ink = hsb 0 0 10
        in Pictures
             [ Color c (at (pt env q) (disc (sz * 0.8)))
             , txtC env x (y - 9) (14 + fromIntegral d * 0.5) ink (show (tP t))
             , txtC env x (y + 9) 14 ink (tName t) ]
      x0 = envW env - 360
      logLine (i, str) =
        txt env x0 (180 + fromIntegral i * 24) 14 (hsb 0 0 (90 - fromIntegral (i :: Int) * 5)) str
      maxText = if V.null h then "-" else show (tP (V.head h))
      help = hsb 0 0 65
  in Pictures
       [ Pictures (map edge [1 .. len - 1])
       , Pictures (map node [0 .. len - 1])
       , txt env x0 150 16 (hsb 0 0 100) "Extraction order (largest first)"
       , Pictures (map logLine (zip [0 ..] (wLog w)))
       , txt env 20 40 22 (hsb 0 0 100)
           ("Priority queue   size " ++ show len ++ "   max = " ++ maxText
             ++ (if wAuto w then "   auto running" else ""))
       , txt env 20 68 15 help "Click empty space: add   Space: take max   Click a node: +30   A: auto   C: clear"
       , txt env 20 92 15 help "Every operation takes at most log2 n swaps. You learn the current max faster than a full sort"
       ]

-- マウスの位置を p5.js の座標に直して、ノードに当たったか調べる
onClick :: Env -> (Float, Float) -> World -> IO World
onClick env (mx, my) w =
  let mouse = (mx + envW env / 2, envH env / 2 - my)
      hits = [i | i <- [0 .. V.length (wHeap w) - 1], dist2 mouse (pos env i) < 26]
  in case hits of
       (i : _) -> pure (increaseKey i 30 w)
       [] | snd mouse > 110 && V.length (wHeap w) < 31 -> randomPriority >>= \p -> insert p w
          | otherwise -> pure w

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p -> onClick env p w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (extractMax w)
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "cC" -> pure w { wHeap = V.empty, wLog = [] }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 10 個入れてから全部取り出すと、大きい順に並ぶ（= ヒープソート）
-- 2. 優先度の小さい順に取り出す「最小ヒープ」にするには？（ダイクストラ法で使う）
-- 3. 自動運転で、来る速さと処理の速さを変えて、行列が伸びるか見てみよう

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
