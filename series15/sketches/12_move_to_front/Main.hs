-- ============================================
-- 12 前に移動（Move-To-Front）リスト
-- 連結リストで探しものをすると、前から k 番目なら k 回たどる（コスト k）
-- 次に何を探されるか分からない（オンライン）。見つけたものを先頭に移す MTF は、未来を知る最善の方法の 4 倍以内（4-競合）
-- 他の戦略：1 つだけ前に（転置）、よく使う順に並べる（頻度）、最初から確率の高い順に固定（静的・未来の分布を知っている）
-- スペース = 再生・停止 / 1〜4 = 要求の出し方 / 項目をクリック = その項目を要求 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.List (delete, elemIndex, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

items :: String
items = "ABCDEFGHIJ"

names :: [String]
names = ["Move to front (MTF)", "Swap forward (transpose)", "By use count (frequency)", "Fixed by probability (static)"]

hues :: [Float]
hues = [45, 200, 300, 140]

data World = World
  { wLists :: [String]
  , wCosts :: [Int]
  , wCounts :: !(Map.Map Char Int)
  , wGen :: !Int
  , wPlaying :: !Bool
  , wTimer :: !Int
  , wLast :: Maybe Char
  , wReqs :: !Int
  , wProbs :: [Float]
  , wShift :: !Int
  }

main :: IO ()
main = runSketch (sketch "15-12 Move to Front" (World (replicate 4 items) [0, 0, 0, 0] Map.empty 2 False 0 Nothing 0 (map (const 1) items) 0))
  { skBack  = hsb 45 20 9
  , skStart = \_ -> reset (World [] [] Map.empty 2 False 0 Nothing 0 [] 0)
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

itemIndex :: Char -> Int
itemIndex c = fromMaybe 0 (elemIndex c items)

reset :: World -> IO World
reset w = do
  let probs = [if wGen w == 2 then 1 / fromIntegral (i + 1) else 1 | i <- [0 .. length items - 1]] :: [Float]
      probOf c = probs !! itemIndex c
  order <- shuffled items
  -- 静的：確率の高い順（未来の分布を知っているずるい戦略）
  let staticList = sortBy (\a b -> compare (probOf b) (probOf a)) items
  pure w { wProbs = probs, wLists = [order, order, order, staticList], wCosts = [0, 0, 0, 0]
         , wCounts = Map.fromList [(x, 0) | x <- items], wLast = Nothing, wReqs = 0, wShift = 0 }

-- 次の要求を決める（1：均等 2：ジップ分布 3：局所性 4：MTF への意地悪）
nextRequest :: World -> IO (Char, Int)
nextRequest w = case wGen w of
  4 -> case wLists w of
    (l : _) | not (null l) -> pure (last l, wShift w)   -- 意地悪：MTF のいちばん後ろを要求
    _ -> pure ('A', wShift w)
  3 -> do                                               -- 局所性：しばらく同じ 3 つばかり
    s <- if wReqs w `mod` 30 == 0 then randomRIO (0, length items - 1) else pure (wShift w)
    d <- randomRIO (0, 2)
    pure (items !! ((s + d) `mod` length items), s)
  _ -> do
    r <- randomRIO (0, sum (wProbs w))
    let go q ((c, p) : rest)
          | q - p < 0 = c
          | otherwise = go (q - p) rest
        go _ [] = 'A'
    pure (go r (zip items (wProbs w)), wShift w)

-- 要求 x に 4 つの戦略がそれぞれ答える
request :: Char -> World -> World
request x w =
  let counts = Map.insertWith (+) x 1 (wCounts w)
      countOf c = Map.findWithDefault 0 c counts
      serve k l =
        let pos = fromMaybe 0 (elemIndex x l)
        in case k :: Int of
             0 -> x : delete x l                 -- 先頭へ
             1 | pos > 0 -> swapPrev pos l       -- 1 つ前と交換
             2 -> sortBy (\a b -> compare (countOf b) (countOf a)) l  -- 使った回数の順
             _ -> l
      swapPrev pos l = [ if q == pos - 1 then l !! pos else if q == pos then l !! (pos - 1) else c
                       | (q, c) <- zip [0 ..] l ]
      costOf l = fromMaybe 0 (elemIndex x l) + 1
  in w { wCounts = counts
       , wCosts = zipWith (\c l -> c + costOf l) (wCosts w) (wLists w)
       , wLists = zipWith serve [0 ..] (wLists w)
       , wLast = Just x
       , wReqs = wReqs w + 1 }

stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wPlaying w =
      let t = wTimer w + 1
          w' = w { wTimer = t }
      in if t `mod` 10 == 0
           then do
             (x, s) <- nextRequest w'
             pure (request x w' { wShift = s })
           else pure w'
  | otherwise = pure w

drawW :: Env -> World -> Picture
drawW env w =
  let colW = (envW env - 80) / 4
      maxCost = max 1 (foldr max 0 (wCosts w))
      maxProb = foldr max 0.0001 (wProbs w)
      probOf c = case drop (itemIndex c) (wProbs w) of
        (p : _) -> p
        [] -> 1
      h = envH env
      column (k, (l, c)) =
        let x0 = 40 + fromIntegral k * colW
            hk = hues !! k
            itemPic (i, it) =
              let y = 140 + fromIntegral i * 40
                  hit = Just it == wLast w
              in Pictures
                   [ box env (if hit then hsb hk 80 100 else hsb 45 10 (25 + 40 * (probOf it / maxProb))) x0 y 54 32
                   , txtC env (x0 + 27) (y + 16) 17 (lum (if hit then 10 else 90)) [it]
                   , txtC env (x0 - 14) (y + 16) 14 (lum 50) (show (i + 1 :: Int)) ]
            -- 累計コストの棒
            bh = fromIntegral c / fromIntegral maxCost * 220 :: Float
            avg :: String
            avg = if wReqs w > 0
                    then printf "avg %.2f" (fromIntegral c / fromIntegral (wReqs w) :: Float)
                    else "avg -"
        in Pictures
             [ txt env x0 120 15 (hsb hk 60 95) (names !! k)
             , Pictures (map itemPic (zip [0 :: Int ..] l))
             , box env (hsb hk 60 80) (x0 + 90) (560 - bh) 50 bh
             , txtC env (x0 + 115) (548 - bh) 16 (lum 100) (show c)
             , txtC env (x0 + 115) 578 14 (lum 70) avg ]
      lastS = maybe "" (\c -> "   last request: " ++ [c]) (wLast w)
      advice = case wGen w of
        3 -> "With locality MTF is strong. A fixed order cannot follow what is in use right now"
        4 -> "Adversary: MTF pays " ++ show (length items) ++ " every time, but the best method suffers too, so the ratio stays within 4x"
        _ -> "With skew, the (cheating) static order is best. MTF does nearly as well without knowing the distribution"
  in Pictures
       [ Pictures (map column (zip [0 :: Int ..] (zip (wLists w) (wCosts w))))
       , txt env 20 36 16 (lum 100)
           ("Space: " ++ (if wPlaying w then "stop" else "play") ++ "  1: uniform  2: skewed (Zipf)  3: locality  4: worst for MTF  Click item: request  R")
       , txt env 20 62 15 (lum 65) "Cost = items walked until found (position from the front). Brighter items are requested more often"
       , txt env 20 (h - 64) 18 (lum 90) ("Requests " ++ show (wReqs w) ++ lastS)
       , txt env 20 (h - 30) 15 (hsb 45 80 100) advice
       ]

-- 項目のクリック：その項目を要求する
click :: Env -> World -> World
click env w =
  let (mx, my) = mouseP env
      colW = (envW env - 80) / 4
      i = floor ((my - 140) / 40) :: Int
      hits = [ c
             | (k, l) <- zip [0 :: Int ..] (wLists w)
             , let x0 = 40 + fromIntegral k * colW
             , mx > x0, mx < x0 + 54
             , i >= 0
             , c <- take 1 (drop i l) ]
  in case hits of
       (c : _) -> request c w
       [] -> w

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (click env w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> reset w { wGen = fromEnum c - fromEnum '0' }
    | c `elem` "rR" -> reset w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 3（局所性）と 2（偏り）で、どの戦略が勝つか比べる
-- 2. 4（意地悪）：MTF のコストは毎回 10。他の戦略はどうなる？
-- 3. ブラウザの「最近使ったファイル」、キャッシュの LRU（13）も同じ考え方

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 文字：txt は左下が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = label (gx env x - fromIntegral (length str) * s * 0.72) (gy env y) s c str

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0

-- フィッシャー＝イェーツのシャッフル
shuffled :: [a] -> IO [a]
shuffled xs = go (length xs - 1) xs
  where
    go i a
      | i <= 0 = pure a
      | otherwise = do
          k <- randomRIO (0, i)
          go (i - 1) (swapAt i k a)
    swapAt i k a = [pick q x | (q, x) <- zip [0 ..] a]
      where pick q x | q == i = a !! k
                     | q == k = a !! i
                     | otherwise = x
