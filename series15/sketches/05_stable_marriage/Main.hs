-- ============================================
-- 05 安定マッチング（ゲール＝シャプレー法）
-- 左の人（申し込む側）と右の人（受ける側）がそれぞれ好きな順番を持っている
-- 空いている人が、まだ断られていない中で一番好きな相手に申し込む。受ける側は今の相手より好きなら乗りかえる
-- 終わると「おたがい今の相手より好き同士」のペア（ブロッキングペア）が 1 つもない = 安定
-- スペース = 1 手 / A = 自動 / S = 申し込む側を入れかえ / 上下 = 人数 / R = 好みをランダム
-- ============================================
module Main (main) where

import Art
import Data.List (elemIndex, findIndex)
import Data.Maybe (fromMaybe)

data LastTry = LastTry { lI :: !Int, lJ :: !Int, lOk :: !Bool, lDumped :: Maybe Int }

data World = World
  { wN :: !Int
  , wPrefP :: [[Int]]       -- wPrefP !! i = 申し込む側 i の好きな順
  , wPrefQ :: [[Int]]       -- wPrefQ !! j = 受ける側 j の好きな順
  , wNext :: [Int]
  , wPartnerP :: [Int]
  , wPartnerQ :: [Int]
  , wProposals :: !Int
  , wLast :: Maybe LastTry
  , wAuto :: !Bool
  , wTimer :: !Int
  , wSwapped :: !Bool
  }

main :: IO ()
main = runSketch (sketch "15-05 Stable Matching" (restart (World 0 [] [] [] [] [] 0 Nothing False 0 False)))
  { skBack  = hsb 330 25 10
  , skStart = \_ -> randomPrefs (restart (World 6 [] [] [] [] [] 0 Nothing False 0 False))  -- ★ 最初の人数
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

-- 申し込む側の名前 A, B, …  受ける側の名前 1, 2, …
nameA, nameB :: Int -> String
nameA i = [toEnum (fromEnum 'A' + i)]
nameB j = show (j + 1)

randomPrefs :: World -> IO World
randomPrefs w = do
  let n = wN w
  ps <- mapM (const (shuffled [0 .. n - 1])) [1 .. n]
  qs <- mapM (const (shuffled [0 .. n - 1])) [1 .. n]
  pure (restart w { wPrefP = ps, wPrefQ = qs, wSwapped = False })

restart :: World -> World
restart w =
  let n = wN w
  in w { wNext = replicate n 0, wPartnerP = replicate n (-1), wPartnerQ = replicate n (-1)
       , wProposals = 0, wLast = Nothing }

-- 受ける側 j の中での i の順位（小さいほど好き）
rankQ :: World -> Int -> Int -> Int
rankQ w j i = fromMaybe 0 (elemIndex i (wPrefQ w !! j))

rankP :: World -> Int -> Int -> Int
rankP w i j = fromMaybe 0 (elemIndex j (wPrefP w !! i))

proposeOnce :: World -> World
proposeOnce w =
  let n = wN w
      cand = findIndex (\(p, k) -> p < 0 && k < n) (zip (wPartnerP w) (wNext w))
  in case cand of
       Nothing -> w { wAuto = False }
       Just i ->
         let j = wPrefP w !! i !! (wNext w !! i)
             cur = wPartnerQ w !! j
             w1 = w { wNext = setAt i (wNext w !! i + 1) (wNext w), wProposals = wProposals w + 1 }
             accept q = q { wPartnerQ = setAt j i (wPartnerQ q), wPartnerP = setAt i j (wPartnerP q) }
         in if cur < 0
              then (accept w1) { wLast = Just (LastTry i j True Nothing) }
              else if rankQ w j i < rankQ w j cur
                then (accept w1 { wPartnerP = setAt cur (-1) (wPartnerP w1) }) { wLast = Just (LastTry i j True (Just cur)) }
                else w1 { wLast = Just (LastTry i j False Nothing) }

finished :: World -> Bool
finished w = all (>= 0) (wPartnerP w)

-- ブロッキングペアを数える：i は今の相手より j が好き、j も今の相手より i が好き
blocking :: World -> [(Int, Int)]
blocking w =
  [ (i, j)
  | i <- [0 .. n - 1], j <- [0 .. n - 1]
  , let pi' = wPartnerP w !! i
  , let qj = wPartnerQ w !! j
  , pi' /= j, pi' >= 0, qj >= 0
  , rankP w i j < rankP w i pi', rankQ w j i < rankQ w j qj ]
  where n = wN w

-- 相手の順位の合計
satisfactionP, satisfactionQ :: World -> Int
satisfactionP w = sum [rankP w i p + 1 | (i, p) <- zip [0 ..] (wPartnerP w), p >= 0]
satisfactionQ w = sum [rankQ w j q + 1 | (j, q) <- zip [0 ..] (wPartnerQ w), q >= 0]

stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wAuto w =
      let t = wTimer w + 1
          w' = w { wTimer = t }
      in pure (if t `mod` 30 == 0 then proposeOnce w' else w')
  | otherwise = pure w

drawW :: Env -> World -> Picture
drawW env w =
  let n = wN w
      ax = envW env * 0.38
      bx = envW env * 0.62
      h = envH env
      py :: Int -> Float
      py i = remap (fromIntegral i) 0 (fromIntegral (n - 1)) (h * 0.2) (h * 0.82)
      a = if wSwapped w then nameB else nameA
      b = if wSwapped w then nameA else nameB
      idx = [0 .. n - 1]
      pink = hsb 330 70 100
      pairLines = [seg env pink 6 (ax, py i) (bx, py p) | (i, p) <- zip [0 ..] (wPartnerP w), p >= 0]
      lastLine = case wLast w of
        Just l -> dashed env (if lOk l then hsb 140 70 95 else lum 60) 3 (ax, py (lI l)) (bx, py (lJ l))
        Nothing -> Blank
      bl = if finished w then blocking w else []
      blLines = [seg env (hsb 0 90 100) 3 (ax, py i) (bx, py j) | (i, j) <- bl]
      chip x y c s = Pictures [boxC env c x y 26 26, txtC env x y 14 (lum 10) s]
      row i =
        let y = py i
            pa = wPartnerP w !! i
            qa = wPartnerQ w !! i
            chipP k =
              let j = wPrefP w !! i !! k
                  x = ax - 60 - fromIntegral (n - k) * 30
                  c | k < wNext w !! i && pa /= j = lum 25
                    | pa == j = pink
                    | otherwise = hsb 30 20 85
              in chip x y c (b j)
            chipQ k =
              let p = wPrefQ w !! i !! k
                  x = bx + 60 + fromIntegral k * 30
              in chip x y (if qa == p then pink else hsb 200 15 85) (a p)
        in Pictures
             [ dot env (if pa >= 0 then hsb 200 50 95 else lum 70) (ax, y) 44
             , txtC env ax y 18 (lum 10) (a i)
             , Pictures (map chipP idx)
             , dot env (if qa >= 0 then hsb 30 60 95 else lum 70) (bx, y) 44
             , txtC env bx y 18 (lum 10) (b i)
             , Pictures (map chipQ idx) ]
      msg | finished w =
              "Done! " ++ show (wProposals w) ++ " proposals (max n^2 = " ++ show (n * n) ++ "). Blocking pairs "
                ++ show (length bl) ++ " -> " ++ (if null bl then "stable" else "unstable")
          | otherwise = case wLast w of
              Nothing -> "Space: a free person on the left proposes"
              Just l -> a (lI l) ++ " -> " ++ b (lJ l) ++ ": "
                ++ (if lOk l
                      then "OK" ++ maybe "" (\d -> " (" ++ a d ++ " is dumped and becomes free)") (lDumped l)
                      else "Sorry (prefers the current partner)")
  in if n <= 0 then Blank else Pictures
       [ Pictures pairLines, lastLine, Pictures blLines
       , Pictures (map row idx)
       , txt env 20 36 18 (lum 100) ("Space: 1 step  A: auto  S: swap proposing side  Up/Down: people " ++ show n ++ "  R: random prefs")
       , txt env 20 62 15 (lum 65) "Chips = preference order (left = favorite). Dark chip = rejected. Pink = current partner"
       , txt env 20 (h - 56) 18 (hsb 45 80 100) msg
       , txt env 20 (h - 24) 15 (lum 80)
           ("Sum of partner ranks (smaller = happier)   proposers " ++ show (satisfactionP w) ++ "   receivers "
             ++ show (satisfactionQ w) ++ "  -> proposers gain")
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (proposeOnce w)
  EventKey (SpecialKey KeyUp) Down _ _ -> randomPrefs w { wN = min 10 (wN w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> randomPrefs w { wN = max 3 (wN w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "sS" -> pure (restart w { wPrefP = wPrefQ w, wPrefQ = wPrefP w, wSwapped = not (wSwapped w) })
    | c `elem` "rR" -> randomPrefs w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. S で申し込む側を入れかえる。結果のペアと「満足度」はどう変わる？（申し込む側に最適な安定マッチング）
-- 2. 申し込む順番（findIndex でいちばん上の人から）を変えても、結果は同じになる
-- 3. 研修医と病院のマッチング、学校の選択制で実際に使われている（2012 年ノーベル経済学賞）

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- 中心 (x, y)、幅 w、高さ h の塗りつぶし（rectMode(CENTER) の rect）
boxC :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
boxC env c x y w h = Color c (rect (gx env x) (gy env y) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 点線（実線 10、すき間 8 をくり返す）
dashed :: Env -> Color -> Float -> Point -> Point -> Picture
dashed env c t (x1, y1) (x2, y2) =
  let l = dist2 (x1, y1) (x2, y2)
      at' d = let q = d / max 1e-6 l in (lerpF x1 x2 q, lerpF y1 y2 q)
  in Pictures [seg env c t (at' d) (at' (min l (d + 10))) | d <- takeWhile (< l) [0, 18 ..]]

-- 中心 (x, y)、直径 d の円（p5.js の circle）
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

-- 文字：txt は左下が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = label (gx env x - fromIntegral (length str) * s * 0.72) (gy env y) s c str

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0

-- リストの i 番目だけ置きかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]

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
