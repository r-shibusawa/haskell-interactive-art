-- ============================================
-- 14 雇用問題とランダムな並べかえ
-- 1：雇用問題。候補者が 1 人ずつ来て、今までで一番良ければ雇う（そのたびに費用）。何回雇う？
--    悪い順に来ると毎回雇う（n 回）。ランダムな順なら平均 1 + 1/2 + … + 1/n ≈ ln n 回（指示確率変数）
-- 2：かたよりのないシャッフル。RANDOMIZE-IN-PLACE（i 番目を i〜末尾のどれかと交換）は 6 通りが同じ確率。
--    「どれとでも交換」するまちがった版は、かたよる
-- 3：オンライン雇用。最初の k 人は見るだけ、その後は「今までより良い人」で即決。
--    k = n/e（約 37%）のとき、最高の人を選べる確率が最大（約 1/e）
-- 1〜3：モード　スペース／クリック：1 回試す　A：1000 回まとめて試す　R：リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt)
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)
import System.Random (StdGen, mkStdGen, newStdGen, randomR)

n :: Int
n = 20              -- ★ 候補者の人数

data World = World
  { wGen    :: !StdGen
  , wMode   :: !Int
  , wTrials :: !Int
  , wHist   :: !(U.Vector Int)          -- 雇った回数のヒストグラム
  , wLast   :: !(Maybe (U.Vector Int, [Int]))  -- この回の並びと、雇った人
  , wPerm   :: !(M.Map String Int)      -- RANDOMIZE-IN-PLACE の結果
  , wSort   :: !(M.Map String Int)      -- PERMUTE-BY-SORTING の結果
  , wWrong  :: !(M.Map String Int)      -- まちがった版の結果
  , wWin    :: !(U.Vector Int)          -- k ごとの成功回数
  , wTries  :: !(U.Vector Int)          -- k ごとの試行回数
  }

main :: IO ()
main = runSketch (sketch "10-14 Hiring and Shuffling" (emptyWorld (mkStdGen 1)))
  { skBack  = hsb 30 25 9
  , skStart = \_ -> emptyWorld <$> newStdGen
  , skDraw  = drawW
  , skEvent = onEvent
  }

emptyWorld :: StdGen -> World
emptyWorld g = resetStats (World g 1 0 U.empty Nothing M.empty M.empty M.empty U.empty U.empty)

resetStats :: World -> World
resetStats w = w
  { wTrials = 0, wHist = U.replicate (n + 1) 0, wLast = Nothing
  , wPerm = M.empty, wSort = M.empty, wWrong = M.empty
  , wWin = U.replicate n 0, wTries = U.replicate n 0 }

-- 乱数を 1 つ引いて i と入れかえる（lo は交換先の下限を決める関数）
swapWith :: (Int -> Int) -> (U.Vector Int, StdGen) -> Int -> (U.Vector Int, StdGen)
swapWith lo (v, g) i =
  let (j, g') = randomR (lo i, U.length v - 1) g
  in (v U.// [(i, v U.! j), (j, v U.! i)], g')

-- その場でシャッフル（フィッシャー＝イェーツ）
randomizeInPlace :: StdGen -> U.Vector Int -> (U.Vector Int, StdGen)
randomizeInPlace g v = foldl' (swapWith id) (v, g) [0 .. U.length v - 1]

-- まちがった版：どれとでも交換（かたよる）
wrongShuffle :: StdGen -> U.Vector Int -> (U.Vector Int, StdGen)
wrongShuffle g v = foldl' (swapWith (const 0)) (v, g) [0 .. U.length v - 1]

-- 番号をつけて並べかえるシャッフル：ランダムな番号（1〜n^3）をつけて番号順に
permuteBySorting :: StdGen -> U.Vector Int -> (U.Vector Int, StdGen)
permuteBySorting g v =
  let len = U.length v
      draw (acc, g0) x =
        let (p, g1) = randomR (1, len * len * len) g0
        in ((p, x) : acc, g1)
      (pairs, g') = foldl' draw ([], g) (U.toList v)
  in (U.fromList (map snd (sortOn fst (reverse pairs))), g')

keyOf :: U.Vector Int -> String
keyOf = concatMap show . U.toList

candidates :: U.Vector Int
candidates = U.enumFromTo 1 n

bump :: String -> M.Map String Int -> M.Map String Int
bump k = M.insertWith (+) k 1

trial :: World -> World
trial w0 =
  let w = w0 { wTrials = wTrials w0 + 1 }
      g0 = wGen w
  in case wMode w of
       1 ->
         let (q, g1) = randomizeInPlace g0 candidates
             hires = hiresOf q
         in w { wGen = g1
              , wHist = U.accum (+) (wHist w) [(length hires, 1)]
              , wLast = Just (q, hires) }
       2 ->
         let three = U.fromList [1, 2, 3]
             (p, g1) = randomizeInPlace g0 three
             (x, g2) = wrongShuffle g1 three
             (o, g3) = permuteBySorting g2 three
         in w { wGen = g3, wPerm = bump (keyOf p) (wPerm w)
              , wSort = bump (keyOf o) (wSort w), wWrong = bump (keyOf x) (wWrong w) }
       _ ->
         let (q, g1) = randomizeInPlace g0 candidates
             wins = [(k, 1) | k <- [0 .. n - 1], q U.! pickFor q k == n]
         in w { wGen = g1, wWin = U.accum (+) (wWin w) wins
              , wTries = U.map (+ 1) (wTries w), wLast = Just (q, []) }

-- 今までで一番良ければ雇う
hiresOf :: U.Vector Int -> [Int]
hiresOf q = reverse (snd (U.ifoldl' step (0, []) q))
  where
    step (best, hs) i v
      | v > best = (v, i : hs)
      | otherwise = (best, hs)

-- 最初の k 人を見送ったあと、その時点の最高より良い最初の人（いなければ最後の人）
pickFor :: U.Vector Int -> Int -> Int
pickFor q k =
  let seen = U.foldl' max 0 (U.take k q)
  in maybe (n - 1) (+ k) (U.findIndex (> seen) (U.drop k q))

drawW :: Env -> World -> Picture
drawW env w =
  let l = 60
      wd = envW env - 120
      hg = envH env
      help = hsb 0 0 65
      body = case wMode w of
        1 -> mode1
        2 -> mode2
        _ -> mode3
      mode1 =
        let lastView = case wLast w of
              Just (q, hires) ->
                let bw = wd / fromIntegral n
                    bar i =
                      let hired = i `elem` hires
                          h = fromIntegral (q U.! i) * 8
                          x = l + fromIntegral i * bw
                      in Pictures
                           [ box env (if hired then hsb 140 70 90 else hsb 30 20 40) (x + 3) (300 - h) (bw - 6) h
                           , if hired then txtC env (x + bw / 2) 314 14 (hsb 140 80 100) "hire" else Blank ]
                in Pictures
                     ( txt env l 130 15 (hsb 0 0 80)
                         ("This run: hired " ++ show (length hires) ++ " times (bar height = skill, arrivals left to right)")
                     : map bar [0 .. n - 1] )
              Nothing -> Blank
            -- ヒストグラム
            hist = wHist w
            maxH = max 1 (U.maximum hist)
            hw = wd / fromIntegral (n + 1)
            by = hg - 80
            hh = hg - 460
            histBar k =
              let h = fromIntegral (hist U.! k) / fromIntegral maxH * hh
                  x = l + fromIntegral k * hw
              in Pictures
                   [ box env (hsb 200 60 85) (x + 2) (by - h) (hw - 4) h
                   , txtC env (x + hw / 2) (by + 12) 14 (hsb 0 0 55) (show k) ]
            hN = sum [1 / fromIntegral i | i <- [1 .. n]] :: Float
            mean = if wTrials w > 0
                     then fromIntegral (U.sum (U.imap (*) hist)) / fromIntegral (wTrials w)
                     else 0 :: Float
            xm = l + hN * hw + hw / 2
        in Pictures
             [ lastView
             , Pictures (map histBar [0 .. n])
             , seg env (hsb 45 90 100) 2 (xm, by - hh - 10) (xm, by)
             , txt env l (by - hh - 20) 15 (hsb 45 90 100)
                 ("Theory H_n = 1 + 1/2 + ... + 1/" ++ show n ++ " = " ++ fixed 2 hN
                   ++ " (ln n = " ++ fixed 2 (log (fromIntegral n)) ++ ")   actual mean " ++ fixed 2 mean
                   ++ " (" ++ show (wTrials w) ++ " runs)")
             ]
      mode2 =
        let ps = ["123", "132", "213", "231", "312", "321"]
            bw = wd / 21
            top = 195     -- 棒が見出しに重ならないように
            unit = (hg - 330) * 3
            side (i, cnt, title) =
              let x0 = l + fromIntegral (i :: Int) * wd / 3
                  tot = fromIntegral (max 1 (wTrials w)) :: Float
                  barC = if i < 2 then hsb (140 + fromIntegral i * 50) 60 85 else hsb 0 60 85
                  bar (k, p) =
                    let v = fromIntegral (M.findWithDefault 0 p cnt) / tot
                        h = min (v * unit) (hg - 100 - top)
                        x = x0 + fromIntegral (k :: Int) * bw
                    in Pictures
                         [ box env barC (x + 4) (hg - 100 - h) (bw - 8) h
                         , txtC env (x + bw / 2) (hg - 84) 14 (hsb 0 0 80) p
                         , txtC env (x + bw / 2) (hg - 62) 14 (hsb 0 0 80) (fixed 1 (v * 100) ++ "%") ]
                  y = hg - 100 - (1 / 6) * unit
              in Pictures
                   ( txt env x0 150 16 (hsb 0 0 90) title
                   : seg env (hsb 45 90 100) 1 (x0, y) (x0 + 6 * bw, y)
                   : map bar (zip [0 ..] ps) )
        in Pictures
             [ Pictures (map side [ (0, wPerm w, "RANDOMIZE-IN-PLACE (correct)")
                                  , (1, wSort w, "PERMUTE-BY-SORTING (correct)")
                                  , (2, wWrong w, "swap with anyone (wrong)") ])
             , txt env l 180 14 (hsb 45 90 100)
                 "Yellow = 1/6 (16.7%). The wrong one splits 3^3 = 27 outcomes into 6 cases, so it cannot be even"
             ]
      mode3 =
        let gh = hg - 330
            by = hg - 90
            maxBar = by - 190
            bw = wd / fromIntegral n
            rate k = if wTries w U.! k > 0
                       then fromIntegral (wWin w U.! k) / fromIntegral (wTries w U.! k)
                       else 0 :: Float
            nOverE = fromIntegral n / exp 1 :: Float
            bar k =
              let h = min (rate k * gh * 2) maxBar
                  x = l + fromIntegral k * bw
                  c = if abs (fromIntegral k - nOverE) < 1 then hsb 45 90 100 else hsb 260 50 80
              in Pictures
                   [ box env c (x + 3) (by - h) (bw - 6) h
                   , txtC env (x + bw / 2) (by + 12) 14 (hsb 0 0 55) (show k) ]
            bestK = foldl' (\b k -> if wWin w U.! k > wWin w U.! b then k else b) 0 [1 .. n - 1]
        in Pictures
             [ seg env (hsb 0 0 40) 1 (l, by) (l + wd, by)
             , Pictures (map bar [0 .. n - 1])
             , txt env l 140 15 (hsb 0 0 85)
                 ("x: skipped first k   y: share that got the best one (" ++ show (wTrials w)
                   ++ " runs)   yellow: k = n/e = " ++ fixed 1 nOverE)
             , if wTrials w > 0
                 then txt env l 164 15 (hsb 0 0 85)
                        ("Best k = " ++ show bestK ++ "   success " ++ fixed 1 (rate bestK * 100) ++ "% (1/e = 36.8%)")
                 else Blank
             ]
      title = case wMode w of
        1 -> "Hiring problem: how many hires?"
        2 -> "Unbiased shuffle (3 cards, 6 orders)"
        _ -> "Online hiring: how many to skip?"
      note = case wMode w of
        1 -> "Hires = sum of indicators \"person i is the best so far\". Its chance is 1/i, so the mean is H_n"
        2 -> "A randomized algorithm is fast on average for any input order (same idea as quicksort)"
        _ -> "Also called the secretary problem. Skip too many and miss the best; too few and nothing to compare"
  in Pictures
       [ body
       , txt env 20 40 22 (hsb 0 0 100) (title ++ "   runs " ++ show (wTrials w))
       , txt env 20 68 15 help "1-3: mode   Space or click: one run   A: 1000 runs at once   R: reset"
       , txt env 20 92 15 help note
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (trial w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (trial w)
  EventKey (Char c) Down _ _
    | c `elem` "123" -> pure (resetStats w { wMode = digitToInt c })
    | c `elem` "aA" -> pure (foldl' (\v _ -> trial v) w [1 .. 1000 :: Int])
    | c `elem` "rR" -> pure (resetStats w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. n = 100 にして、平均の雇用回数が ln 100 ≈ 4.6 に近づくか確かめる
-- 2. PERMUTE-BY-SORTING の番号の範囲を 1〜n^3 から 1〜2 にすると、同じ番号が出てかたよる。確かめよう
-- 3. オンライン雇用で「2 番目に良い人まで OK」なら、最適な k はどう変わる？

-- ---------- 道具：小数の表示 ----------
fixed :: Int -> Float -> String
fixed d v = showFFloat (Just d) v ""

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
