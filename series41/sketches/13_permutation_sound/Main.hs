-- ============================================
-- 41-13 順列を聴く（チェンジリンギング）
-- 音階 n 音の順列をすべて順番に鳴らす。イギリスの教会の鐘「チェンジリンギング」は、
-- 隣どうしの入れかえだけで n! 通りの並び（チェンジ）を全部鳴らす数学的な音楽
-- 転倒数（並びの「乱れ」の数）が多いほど音色がざらつく
-- 操作：クリック = 開始／停止 / ↑↓ = 音の数 n（3〜6）/ M = 隣の入れかえ（SJT）と辞書順を切り替え / マウス左右 = 速さ
-- ============================================
module Main (main) where

import Art
import Control.Monad (when)
import Data.Array
import Data.List (elemIndex)

scaleHz :: [Float]
scaleHz = [261.6, 293.7, 329.6, 392.0, 440.0, 523.3]   -- ★ ドレミソラド（ペンタトニック）

noteNames :: [String]
noteNames = ["Do", "Re", "Mi", "So", "La", "Do"]

data World = World
  { wN       :: !Int                  -- ★ 音の数
  , wRows    :: !(Array Int [Int])    -- すべての順列（鳴らす順）
  , wSJT     :: !Bool                 -- True: 隣どうしの入れかえだけ（Steinhaus–Johnson–Trotter）
  , wRow     :: !Int                  -- いま鳴らしている行
  , wNote    :: !Int                  -- いま鳴らしている音
  , wPlaying :: !Bool
  , wTimer   :: !Float
  }

main :: IO ()
main = runSketch (sketch "41-13 Listening to permutations" (rebuild (World 4 (toRows [[]]) True 0 0 False 0)))
  { skBack  = hsb 230 28 11
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

toRows :: [[Int]] -> Array Int [Int]
toRows xs = listArray (0, length xs - 1) xs

rebuild :: World -> World
rebuild w = w
  { wRows = toRows (if wSJT w then sjtPermutations (wN w) else lexPermutations (wN w))
  , wRow = 0, wNote = 0, wTimer = 0 }

-- ★ SJT：いちばん大きな「動ける」数を、その向きに 1 つ動かす（隣との入れかえだけ）
--   (値, 向き) のリストで持つ。向きは −1（左）か +1（右）
sjtPermutations :: Int -> [[Int]]
sjtPermutations k = go [(i, -1) | i <- [0 .. k - 1]]
  where
    go st = map fst st : case mobile st of
      Nothing -> []
      Just (pos, val) ->
        let moved = moveAt pos st
            flipped = [if v > val then (v, negate d) else (v, d) | (v, d) <- moved]
        in go flipped
    -- いちばん大きな動ける数の (位置, 値)
    mobile st =
      let arr = zip [0 ..] st
          ok (i, (v, d)) = case drop (i + d) st of
            ((u, _) : _) | i + d >= 0 -> u < v
            _ -> False
          cands = [(v, i) | c@(i, (v, _)) <- arr, ok c]
      in case cands of
           [] -> Nothing
           _ -> let (v, i) = maximum cands in Just (i, v)
    -- 位置 pos の要素を向きの方へ 1 つ動かす（向きも一緒に動く）
    moveAt pos st =
      let (_, d) = indexOr (0, 0) st pos
          j = pos + d
          a = indexOr (0, 0) st pos
          b = indexOr (0, 0) st j
      in [if i == pos then b else if i == j then a else x | (i, x) <- zip [0 ..] st]

-- 辞書順：後ろから「上がる所」を探して入れ替え、残りを反転
lexPermutations :: Int -> [[Int]]
lexPermutations k = go [0 .. k - 1]
  where
    go p = p : maybe [] go (nextPerm p)

nextPerm :: [Int] -> Maybe [Int]
nextPerm p =
  let rp = reverse p
      -- 後ろから見て、はじめて下がる所を探す
      findRise seen (x : y : rest)
        | y < x = Just (reverse (x : seen), y, rest)
        | otherwise = findRise (x : seen) (y : rest)
      findRise _ _ = Nothing
  in case findRise [] rp of
       Nothing -> Nothing
       Just (tailAsc, pivot, restRev) ->
         -- tailAsc は後ろの降順部分を上がる順に並べたもの
         let bigger = filter (> pivot) tailAsc
             swapVal = foldr min maxBound bigger
             newTail = map (\v -> if v == swapVal then pivot else v) tailAsc
         in Just (reverse restRev ++ [swapVal] ++ newTail)

-- 転倒数：i < j なのに p[i] > p[j] のペアの数
inversions :: [Int] -> Int
inversions p = case p of
  [] -> 0
  (x : xs) -> length (filter (x >) xs) + inversions xs

currentRow :: World -> [Int]
currentRow w = wRows w ! wRow w

speedOf :: Env -> Float
speedOf env = clampF 3 14 (remap (fst (mouseP env)) 0 (envW env) 3 14)   -- ★ 1 秒あたりの音の数

step :: Env -> Float -> World -> IO World
step env dt w
  | not (wPlaying w) = pure w
  | wTimer w + dt < 1 / speed = pure w { wTimer = wTimer w + dt }
  | otherwise = do
      -- ★ 転倒数で音色：乱れが多いほどざらついた波形に
      let row = currentRow w
          inv = inversions row
          freq = indexOr 440 scaleHz (indexOr 0 row (wNote w))
      playTone freq 0.25 (0.9 / speed + 0.1) (if inv > wN w then Sawtooth else Triangle)
      let note = wNote w + 1
          (_, top) = bounds (wRows w)
      pure $ if note >= wN w
        then w { wTimer = 0, wNote = 0, wRow = (wRow w + 1) `mod` (top + 1) }
        else w { wTimer = 0, wNote = note }
  where speed = speedOf env

draw :: Env -> World -> Picture
draw env w =
  let n = wN w
      row = currentRow w
      inv = inversions row
      (_, top) = bounds (wRows w)
      count = top + 1
      noteHue v = fromIntegral v * 300 / fromIntegral n + 20
      -- 各音の「道」：いまの行のまわりを表示
      lx = 60
      cellW = 48
      rowH = 22
      visible = floor ((envH env - 200) / rowH) :: Int
      first = max 0 (min (wRow w - visible `div` 2) (count - visible))
      rowsShown = [first .. min (count - 1) (first + visible - 1)]
      rowPic r =
        let y = 140 + fromIntegral (r - first) * rowH
            values = wRows w ! r
            nextR = r + 1 < count && r + 1 < first + visible
            nextValues = if nextR then wRows w ! (r + 1) else []
            cell (i, v) =
              let cur = r == wRow w && i == wNote w
                  x = lx + fromIntegral i * cellW
                  linkPic = case elemIndex v nextValues of
                    Just ni ->
                      let c = hsba (noteHue v) 70 70 0.6
                          a = toG env x (y + 9)
                          b = toG env (lx + fromIntegral ni * cellW) (y + rowH - 9)
                      in Color c (if v == 0 then thickLine 3 a b else Line [a, b])
                    Nothing -> Blank
              in Pictures
                   [ txtM env x y (if cur then 18 else 14) (hsb (noteHue v) 70 (if cur then 100 else 80)) (show (v + 1))
                   , linkPic ]
        in Pictures
             ( (if r == wRow w then box env (hsba 0 0 100 0.1) (lx - 30) (y - rowH / 2) (fromIntegral n * cellW + 80) rowH else Blank)
             : txtM env (lx - 26) y 14 (hsb 0 0 50) (show (r + 1))
             : map cell (zip [0 :: Int ..] values) )
      -- 右：いまの行を大きく、転倒数
      px = envW env * 0.5
      py = 160
      bigBar (i, v) =
        let h = 30 + fromIntegral v * 25
            x = px + fromIntegral i * 70
            cur = i == wNote w && wPlaying w
        in Pictures
             [ box env (hsb (noteHue v) 70 95) x (py + 160 - h) 56 h
             , if cur then box env (hsba 0 0 100 0.4) (x - 4) (py + 160 - h - 4) 64 (h + 8) else Blank
             , txtM env (x + 28) (py + 146) 16 (hsb 0 0 10) (indexOr "" noteNames v) ]
      plain dy s = txt env px (py + dy) 14 (hsb 0 0 70) s
  in Pictures
       [ Pictures (map rowPic rowsShown)
       , txt env px py 20 (hsb 0 0 100)
           ("Row " ++ show (wRow w + 1) ++ " / " ++ show count ++ " (" ++ show n ++ "! = " ++ show count ++ ")")
       , Pictures (map bigBar (zip [0 :: Int ..] row))
       , txt env px (py + 200) 18 (hsb 50 80 100)
           ("Inversions " ++ show inv ++ " / " ++ show (n * (n - 1) `div` 2)
            ++ " (tone: " ++ (if inv > n then "sawtooth" else "triangle") ++ ")")
       , plain 226 "Inversions = pairs where the earlier note is higher."
       , plain 246 "0 = rising scale, max n(n-1)/2 = falling scale"
       , plain 270 (if wSJT w then "SJT (plain changes): only 2 neighbors swap, so" else "Lexicographic: tidy order, but distant notes")
       , plain 290 (if wSJT w then "bell ringers can change order without strain." else "may swap all at once.")
       , plain 314 "Change ringing: n = 6 (720 rows) takes 30 minutes,"
       , plain 334 "n = 8 (40320 rows) takes 20 hours."
       , txt env 20 40 16 (hsb 0 0 100)
           ((if wPlaying w then "Playing (click: stop)" else "Click: play") ++ "   n = " ++ show n ++ " (Up/Down)   M: "
            ++ (if wSJT w then "neighbor swaps (SJT)" else "lexicographic") ++ "   Mouse X: speed")
       , txt env 20 68 14 (hsb 0 0 60)
           "Plays all n! orders once. Left: each note's path. With neighbor swaps the lines braid smoothly."
       ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyUp) Down _ _ | wN w < 6 -> rebuild w { wN = wN w + 1 }
  EventKey (SpecialKey KeyDown) Down _ _ | wN w > 3 -> rebuild w { wN = wN w - 1 }
  EventKey (Char c) Down _ _ | c `elem` "mM" -> rebuild w { wSJT = not (wSJT w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. n = 3 の 6 行を聴いて、SJT と辞書順のちがいを耳で感じよう（SJT は 1 音ずつしか変わらない）
-- 2. scaleHz を長調の 7 音や、好きな和音の構成音に変えてみよう
-- 3. 転倒数で音の長さや音量を変えると、乱れの「波」が聞こえる。playTone の引数を書きかえよう
-- 4. 行ごとに「最初の音だけ」鳴らすと、n = 4 で 24 音のメロディになる。どんな旋律？

-- ---------- 道具 ----------
-- 範囲外なら既定の値を返す取り出し
indexOr :: a -> [a] -> Int -> a
indexOr d xs i
  | i < 0 = d
  | otherwise = case drop i xs of
      (x : _) -> x
      [] -> d

-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js の座標で
mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左上が (x, y) の長方形
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 左下が (x, y) の文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 中心が (x, y) の文字
txtM :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtM env x y size c s = let (gx, gy) = toG env x y in labelC gx gy size c s
