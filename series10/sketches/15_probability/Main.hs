-- ============================================
-- 15 確率の直感をきたえる（誕生日・ボールと箱・連）
-- 乱択アルゴリズムの解析でよく使う 3 つの例
-- 1：誕生日のパラドックス。何人集まると「同じ誕生日の 2 人」がいる確率が 50% を超える？ → たった 23 人
--    （ハッシュ表の衝突と同じ計算）
-- 2：ボールと箱。b 個の箱にランダムに投げて、全部の箱が埋まるまで何個？ → 約 b ln b 個（クーポン収集）
-- 3：連（ストリーク）。コインを n 回投げたとき、表が続く最長の長さは？ → 約 log2 n
-- 1〜3：モード　スペース／クリック：1 つ進める　A：1000 回まとめて試す　R：リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)
import System.Random (StdGen, mkStdGen, newStdGen, randomR)

bins, flips :: Int
bins = 30           -- ★ 2 の箱の数
flips = 256         -- ★ 3 のコインを投げる回数

data World = World
  { wGen    :: !StdGen
  , wMode   :: !Int
  , wHist   :: !(IM.IntMap Int)
  , wTrials :: !Int
  , wDays   :: !(U.Vector Int)    -- 1 年の各日の人数
  , wPeople :: !Int
  , wFound  :: !Int               -- 同じ誕生日の日（-1 = まだ）
  , wBox    :: !(U.Vector Int)
  , wBalls  :: !Int
  , wCoins  :: ![Bool]            -- 新しいものが先頭
  , wBestS  :: !Int               -- 最長の連の始まり
  , wBestL  :: !Int               -- 最長の連の長さ
  }

main :: IO ()
main = runSketch (sketch "10-15 Probability Intuition" (emptyWorld (mkStdGen 1)))
  { skBack  = hsb 190 25 9
  , skStart = \_ -> emptyWorld <$> newStdGen
  , skDraw  = drawW
  , skEvent = onEvent
  }

emptyWorld :: StdGen -> World
emptyWorld g = resetAll (World g 1 IM.empty 0 U.empty 0 (-1) U.empty 0 [] 0 0)

resetAll :: World -> World
resetAll w = resetOne w { wHist = IM.empty, wTrials = 0 }

resetOne :: World -> World
resetOne w = w
  { wDays = U.replicate 365 0, wPeople = 0, wFound = -1
  , wBox = U.replicate bins 0, wBalls = 0
  , wCoins = [], wBestS = 0, wBestL = 0 }

record :: Int -> World -> World
record v w = w { wHist = IM.insertWith (+) v 1 (wHist w), wTrials = wTrials w + 1 }

-- 1 つ進める（スペース／クリック）
addOne :: World -> World
addOne w = case wMode w of
  1 | wFound w >= 0 -> resetOne w
    | otherwise ->
        let (d, g) = randomR (0, 364) (wGen w)
            count = wDays w U.! d + 1
            people = wPeople w + 1
            w1 = w { wGen = g, wDays = wDays w U.// [(d, count)], wPeople = people }
        in if count == 2 then record people w1 { wFound = d } else w1
  2 | U.all (> 0) (wBox w) -> resetOne w
    | otherwise ->
        let (b, g) = randomR (0, bins - 1) (wGen w)
            box' = wBox w U.// [(b, wBox w U.! b + 1)]
            balls = wBalls w + 1
            w1 = w { wGen = g, wBox = box', wBalls = balls }
        in if U.all (> 0) box' then record balls w1 else w1
  _ | length (wCoins w) >= flips -> resetOne w
    | otherwise ->
        let room = min 16 (flips - length (wCoins w))
            (new, g) = flipCoins room (wGen w) []
            coins = new ++ wCoins w
            (s, l) = longest (reverse coins)
            w1 = w { wGen = g, wCoins = coins, wBestS = s, wBestL = l }
        in if length coins >= flips then record l w1 else w1

flipCoins :: Int -> StdGen -> [Bool] -> ([Bool], StdGen)
flipCoins 0 g acc = (acc, g)
flipCoins k g acc =
  let (c, g') = randomR (False, True) g
  in flipCoins (k - 1) g' (c : acc)

-- 表の最長の連：(始まり, 長さ)
longest :: [Bool] -> (Int, Int)
longest cs =
  let step (bs, bl, run, i) c =
        let run' = if c then run + 1 else 0
        in if run' > bl then (i - run' + 1, run', run', i + 1) else (bs, bl, run', i + 1)
      (s, l, _, _) = foldl' step (0, 0, 0 :: Int, 0 :: Int) cs
  in (s, l)

-- まとめて試すための、速い 1 回
fastTrial :: World -> World
fastTrial w = case wMode w of
  1 -> let (k, g) = birthday (wGen w) IS.empty 1 in record k w { wGen = g }
  2 -> let (k, g) = coupons (wGen w) IS.empty 0 in record k w { wGen = g }
  _ -> let (l, g) = streak (wGen w) flips 0 0 in record l w { wGen = g }

birthday :: StdGen -> IS.IntSet -> Int -> (Int, StdGen)
birthday g seen k =
  let (d, g') = randomR (0, 364 :: Int) g
  in if IS.member d seen then (k, g') else birthday g' (IS.insert d seen) (k + 1)

coupons :: StdGen -> IS.IntSet -> Int -> (Int, StdGen)
coupons g filled k
  | IS.size filled >= bins = (k, g)
  | otherwise =
      let (b, g') = randomR (0, bins - 1) g
      in coupons g' (IS.insert b filled) (k + 1)

streak :: StdGen -> Int -> Int -> Int -> (Int, StdGen)
streak g 0 _ best = (best, g)
streak g left run best =
  let (c, g') = randomR (False, True) g
      run' = if c then run + 1 else 0
  in run' `seq` streak g' (left - 1) run' (max best run')

monthLengths :: [Int]
monthLengths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

-- 0 始まりの日 → (0 始まりの月, 月の中の日)
monthDay :: Int -> (Int, Int)
monthDay d0 = go 0 d0 monthLengths
  where
    go m d (ml : rest) | d >= ml = go (m + 1) (d - ml) rest
    go m d _ = (m, d)

hN :: Int -> Float
hN b = sum [1 / fromIntegral i | i <- [1 .. b]]

drawW :: Env -> World -> Picture
drawW env w =
  let l = 60
      wd = envW env - 120
      hg = envH env
      topH = hg * 0.42
      top = case wMode w of
        1 -> mode1
        2 -> mode2
        _ -> mode3
      mode1 =
        let cw = wd / 31
            ch = (topH - 40) / 12
            cell d =
              let (m, dd) = monthDay d
                  v = wDays w U.! d
                  c | v == 0 = hsb 190 20 20
                    | v == 1 = hsb 140 60 80
                    | otherwise = hsb 0 80 100
              in box env c (l + fromIntegral dd * cw + 1) (120 + fromIntegral m * ch + 1) (cw - 2) (ch - 2)
            p = product [fromIntegral (365 - k) / 365 | k <- [0 .. wPeople w - 1]] :: Float
            foundText
              | wFound w >= 0 = "   -> two people in month " ++ show (fst (monthDay (wFound w)) + 1) ++ "!"
              | otherwise = ""
        in Pictures
             [ Pictures (map cell [0 .. 364])
             , txt env l 112 16 (hsb 0 0 90)
                 (show (wPeople w) ++ " people   chance of a shared birthday = " ++ fixed 1 ((1 - p) * 100) ++ "%" ++ foundText)
             ]
      mode2 =
        let bw = wd / fromIntegral bins
            mx = max 4 (U.foldl' max 0 (wBox w))
            bar i =
              let v = wBox w U.! i
                  h = fromIntegral v / fromIntegral mx * (topH - 60)
                  c = if v > 0 then hsb (30 + fromIntegral i * 8) 60 85 else hsb 0 70 40
              in box env c (l + fromIntegral i * bw + 3) (120 + topH - 60 - h) (bw - 6) (max 3 h)
            empties = U.length (U.filter (== 0) (wBox w))
        in Pictures
             [ Pictures (map bar [0 .. bins - 1])
             , txt env l 112 16 (hsb 0 0 90)
                 (show (wBalls w) ++ " thrown   empty boxes " ++ show empties
                   ++ "   (expected b H_b = b ln b = " ++ fixed 1 (fromIntegral bins * hN bins) ++ ")")
             ]
      mode3 =
        let per = 64 :: Int
            cw = wd / fromIntegral per
            rows = ceiling (fromIntegral flips / fromIntegral per :: Float) :: Int
            ch = min 28 ((topH - 40) / fromIntegral rows)
            coins = reverse (wCoins w)
            dot' (i, c) =
              let inBest = i >= wBestS w && i < wBestS w + wBestL w
                  col | c && inBest = hsb 45 100 100
                      | c = hsb 45 50 70
                      | otherwise = hsb 220 30 35
              in Color col (at (pt env ( l + fromIntegral (i `mod` per) * cw + cw / 2
                                       , 130 + fromIntegral (i `div` per) * ch + ch / 2 ))
                               (disc (min cw ch * 0.4)))
            count = length coins
        in Pictures
             [ Pictures (map dot' (zip [0 ..] coins))
             , txt env l 112 16 (hsb 0 0 90)
                 (show count ++ " flips (yellow = heads)   longest run " ++ show (wBestL w)
                   ++ "   (log2 n = " ++ fixed 1 (logBase 2 (fromIntegral (max 2 count))) ++ ")")
             ]
      -- ヒストグラム
      by = hg - 50
      hh = hg - topH - 190
      hist = wHist w
      histogram = case (IM.lookupMin hist, IM.lookupMax hist) of
        (Just (lo, _), Just (hi, _)) ->
          let mxc = maximum (1 : IM.elems hist)
              bw = wd / fromIntegral (hi - lo + 1)
              every = head' [s | s <- [1, 5, 10, 20, 50, 100], bw * fromIntegral s >= 34]
              bar k =
                let c = IM.findWithDefault 0 k hist
                    h = fromIntegral c / fromIntegral mxc * hh
                    x = l + fromIntegral (k - lo) * bw
                in Pictures
                     [ box env (hsb 260 50 80) (x + 1) (by - h) (max 1 (bw - 2)) h
                     , if k `mod` every == 0
                         then txtC env (x + bw / 2) (by + 12) 14 (hsb 0 0 55) (show k)
                         else Blank ]
              total = IM.foldlWithKey' (\s k c -> s + k * c) 0 hist
              mean = fromIntegral total / fromIntegral (max 1 (wTrials w)) :: Float
              theory = case wMode w of
                1 -> "theory: 50% at about 23, mean about 24.6"
                2 -> "theory: b H_b = " ++ fixed 1 (fromIntegral bins * hN bins)
                _ -> "theory: about log2 n = " ++ fixed 0 (logBase 2 (fromIntegral flips)) ++ " (+- a few)"
          in Pictures
               [ Pictures (map bar [lo .. hi])
               , txt env l (by - hh - 14) 15 (hsb 45 90 100)
                   ("Histogram (" ++ show (wTrials w) ++ " runs)   mean " ++ fixed 2 mean ++ "   " ++ theory)
               ]
        _ -> Blank
      title = case wMode w of
        1 -> "Birthday paradox"
        2 -> "Balls and bins (until all are filled)"
        _ -> "Coin streaks (longest run of heads)"
  in Pictures
       [ top
       , histogram
       , txt env 20 40 22 (hsb 0 0 100) title
       , txt env 20 68 15 (hsb 0 0 65) "1-3: mode   Space or click: one step   A: 1000 runs at once   R: reset"
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (addOne w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (addOne w)
  EventKey (Char c) Down _ _
    | c `elem` "123" -> pure (resetAll w { wMode = digitToInt c })
    | c `elem` "aA" -> pure (foldl' (\v _ -> fastTrial v) w [1 .. 1000 :: Int])
    | c `elem` "rR" -> pure (resetAll w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 誕生日を「1 年 = 1000 日」にすると、50% になる人数は何人？（約 √(2·1000·ln 2)）
-- 2. 箱の数 bins を 2 倍にすると、必要なボールは 2 倍より多い？少ない？
-- 3. 人が「ランダムっぽく」書いた表裏の列は、最長の連が短すぎることが多い。自分で 64 個書いて比べよう

-- ---------- 道具：リストの先頭（空なら 100）と小数の表示 ----------
head' :: [Int] -> Int
head' (x : _) = x
head' [] = 100

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
