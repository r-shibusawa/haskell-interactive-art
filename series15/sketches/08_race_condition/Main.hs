-- ============================================
-- 08 並列ループと競合（レース）
-- x = x + 1 は、実は 3 つの命令：① r = x（読む）② r = r + 1 ③ x = r（書く）
-- 2 人が同時にやると、命令の順番しだいで x が 2 にならず 1 になる（決定性レース）
-- 並列ループ parallel for で合計を 1 つの変数に足すと、足したはずの分が消える → 各自で足してから最後にまとめる（リダクション）
-- 1：2 人の x++ / 2：並列ループの合計 / スペース = 1 命令 / A = 自動 / T = 1000 回試す / R = やり直し / S = リダクション / 左右 = 人数（2）
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import System.Random (StdGen, newStdGen, randomR)
import Text.Printf (printf)

data Thread = Thread
  { tPc :: !Int
  , tR :: !Int
  , tRest :: [Int]       -- まだ足していない数
  , tLocal :: !Int
  , tFlushed :: !Bool
  }

-- 1 回の実行の状態
data Run = Run { rThreads :: [Thread], rShared :: !Int, rLog :: [(Int, String)] }

data World = World
  { wMode :: !Int
  , wRun :: !Run
  , wHist :: !(Map.Map Int Int)
  , wAuto :: !Bool
  , wTimer :: !Int
  , wWorkers :: !Int
  , wReduce :: !Bool
  }

total :: Int
total = 40                -- ★ 並列ループで足す数の個数 N

main :: IO ()
main = runSketch (sketch "15-08 Race Condition" (World 1 (start 1 4) Map.empty False 0 4 False))
  { skBack  = hsb 0 30 9
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

start :: Int -> Int -> Run
start mode workers
  | mode == 1 = Run (replicate 2 (thread [1])) 0 []
  | otherwise = Run [thread [k + 1, k + 1 + workers .. total] | k <- [0 .. workers - 1]] 0 []
  where thread items = Thread 0 0 items 0 False

restart :: World -> World
restart w = w { wRun = start (wMode w) (wWorkers w) }

isReduce :: World -> Bool
isReduce w = wReduce w && wMode w == 2

finishedThread :: Bool -> Thread -> Bool
finishedThread red t
  | red = null (tRest t) && tFlushed t
  | otherwise = null (tRest t)

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]

-- 1 命令だけ進める（どの人が進むかはランダム = OS まかせ）。全員終わっていれば Nothing
oneOp :: Bool -> Bool -> StdGen -> Run -> Maybe (Run, StdGen)
oneOp red keepLog g run =
  case [k | (k, t) <- zip [0 ..] (rThreads run), not (finishedThread red t)] of
    [] -> Nothing
    cand ->
      let (q, g') = randomR (0, length cand - 1) g
          k = cand !! q
          t = rThreads run !! k
          v = case tRest t of
            (x : _) -> x
            [] -> 0
          note s = if keepLog then take 12 ((k, s) : rLog run) else []
          put t' sh s = Just (run { rThreads = setAt k t' (rThreads run), rShared = sh, rLog = note s }, g')
          sh0 = rShared run
      in if red
           -- リダクション：自分の local に足すだけ（共有しない）。最後に 1 人ずつまとめる
           then case tRest t of
             (x : r) -> put t { tLocal = tLocal t + x, tRest = r } sh0 ("local += " ++ show x)
             [] -> put t { tFlushed = True } (sh0 + tLocal t) ("merge +" ++ show (tLocal t))
           else
             let pc' = (tPc t + 1) `mod` 3
             in case tPc t of
                  0 -> put t { tR = sh0, tPc = pc' } sh0 ("r = x (" ++ show sh0 ++ ")")
                  1 -> put t { tR = tR t + v, tPc = pc' } sh0 ("r = r + " ++ show v ++ " (" ++ show (tR t + v) ++ ")")
                  _ -> put t { tPc = pc', tRest = drop 1 (tRest t) } (tR t) ("x = r (" ++ show (tR t) ++ ")")

expected :: World -> Int
expected w = if wMode w == 1 then 2 else total * (total + 1) `div` 2

-- 1 回の実行を最後まで走らせて、結果 x を返す
trial :: Bool -> Run -> StdGen -> (Int, StdGen)
trial red = go
  where
    go r g = case oneOp red False g r of
      Nothing -> (rShared r, g)
      Just (r', g') -> go r' g'

-- ★ 試す回数
trialCount :: Int
trialCount = 1000

runTrials :: World -> StdGen -> World
runTrials w g0 =
  let r0 = start (wMode w) (wWorkers w)
      red = isReduce w
      one (h, g) _ =
        let (x, g') = trial red r0 g
        in (Map.insertWith (+) x 1 h, g')
      (hist, _) = foldl' one (wHist w, g0) [1 .. trialCount]
  in restart w { wHist = hist }

stepOne :: World -> IO World
stepOne w = do
  g <- newStdGen
  pure $ case oneOp (isReduce w) True g (wRun w) of
    Nothing -> w { wAuto = False }
    Just (r, _) -> w { wRun = r }

stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wAuto w =
      let t = wTimer w + 1
          w' = w { wTimer = t }
      in if t `mod` (if wMode w == 1 then 30 else 4) == 0 then stepOne w' else pure w'
  | otherwise = pure w

drawW :: Env -> World -> Picture
drawW env w =
  let run = wRun w
      ths = rThreads run
      nT = length ths
      red = isReduce w
      shared = rShared run
      done = all (finishedThread red) ths
      expct = expected w
      cx = envW env / 2
      h = envH env
      fi' :: Int -> Float
      fi' = fromIntegral
      tw = min 220 ((envW env - 80) / fi' nT - 16)
      hueOf k = fi' k * 360 / fi' nT
      threadPic (k, t) =
        let x = 40 + fi' k * (tw + 16)
            y = 220
            mid = x + tw / 2
            next = case tRest t of
              (v : _) -> show v
              [] -> ""
            ins = zip [0 :: Int ..] ["1: r = x", "2: r = r + " ++ next, "3: x = r"]
            insPic (q, s) = txtC env mid (y + 80 + fi' q * 20) 14
              (if tPc t == q && not (finishedThread red t) then hsb 50 90 100 else lum 55) s
        in Pictures
             ( box env (hsb (hueOf k) 50 30) x y tw 150
             : txtC env mid (y + 20) 16 (hsb (hueOf k) 60 100) ("Person " ++ show (k + 1))
             : txtC env mid (y + 138) 14 (lum 60) (show (length (tRest t)) ++ " left")
             : (if red
                  then [txtC env mid (y + 60) 14 (lum 90) ("local = " ++ show (tLocal t))]
                  else txtC env mid (y + 50) 14 (lum 90) ("register r = " ++ show (tR t)) : map insPic ins) )
      -- 実行の記録（タイムライン）
      timeline = [ txt env (40 + fi' ((k * 140) `mod` (round (envW env) - 260))) (405 + fi' q * 20) 14
                     (hsb (hueOf k) 60 100) ("P" ++ show (k + 1) ++ ": " ++ s)
                 | (q, (k, s)) <- zip [0 :: Int ..] (reverse (rLog run)) ]
      -- ヒストグラム
      hist = wHist w
      kvs = Map.toAscList hist
      totalRuns = sum (map snd kvs)
      histPic
        | totalRuns == 0 = Blank
        | otherwise =
            let gx' = envW env - 360
                gy' = 420
                gw = 320
                gh = 180
                bw = gw / fi' (length kvs)
                mx = foldr (max . snd) 1 kvs
                bar (q, (k, c)) =
                  let bh = fi' c / fi' mx * (gh - 40)
                      bx = gx' + fi' q * bw
                  in Pictures
                       [ box env (if k == expct then hsb 140 60 90 else hsb 0 70 95) (bx + 2) (gy' + gh - 20 - bh) (max 1 (bw - 4)) bh
                       , if length kvs <= 8 then txtC env (bx + bw / 2) (gy' + gh - 10) 14 (lum 90) (show k) else Blank ]
                rate = 100 * fi' (fromMaybe 0 (Map.lookup expct hist)) / fi' totalRuns
            in Pictures
                 [ box env (hsb 0 20 15) gx' gy' gw gh
                 , Pictures (map bar (zip [0 :: Int ..] kvs))
                 , txt env (gx' - 40) (gy' - 8) 14 (lum 80)
                     (show totalRuns ++ " runs (green = correct " ++ show expct ++ ")  correct " ++ printf "%.1f" rate ++ "%")
                 ]
      modeHint
        | wMode w == 2 = "  S: reduce " ++ (if wReduce w then "ON" else "OFF") ++ "  L/R: workers " ++ show (wWorkers w)
        | otherwise = ""
  in Pictures
       [ box env (if done && shared /= expct then hsb 0 80 90 else hsb 45 80 95) (cx - 90) 100 180 80
       , txtC env cx 118 16 (lum 10) "shared x"
       , txtC env cx 155 32 (lum 10) (show shared)
       , Pictures (map threadPic (zip [0 ..] ths))
       , Pictures timeline
       , histPic
       , txt env 20 36 15 (lum 100)
           ("1: two x++  2: parallel sum 1.." ++ show total ++ "  Space: 1 op  A: auto  T: try " ++ show trialCount ++ "  R: redo" ++ modeHint)
       , txt env 20 62 15 (lum 65)
           (if wMode w == 1
              then "Correct is 2. In the order 1 1 2 2 3 3 both read 0 and write 1 -> x = 1"
              else "parallel for i = 1.." ++ show total ++ ": x = x + i. Correct is " ++ show expct ++ ". With S (reduction) it is always right")
       , if done
           then txt env 20 (h - 26) 22 (if shared == expct then hsb 140 60 95 else hsb 0 70 100)
                  ("Result x = " ++ show shared ++ (if shared == expct then "  correct" else "  wrong! (" ++ show (expct - shared) ++ " lost)"))
           else Blank
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> stepOne w
  EventKey (SpecialKey KeyRight) Down _ _ | wMode w == 2 -> pure (restart w { wWorkers = min 6 (wWorkers w + 1), wHist = Map.empty })
  EventKey (SpecialKey KeyLeft) Down _ _ | wMode w == 2 -> pure (restart w { wWorkers = max 2 (wWorkers w - 1), wHist = Map.empty })
  EventKey (Char c) Down _ _
    | c == '1' -> pure (restart w { wMode = 1, wHist = Map.empty })
    | c == '2' -> pure (restart w { wMode = 2, wHist = Map.empty })
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "tT" -> runTrials w <$> newStdGen
    | c `elem` "rR" -> pure (restart w)
    | c `elem` "sS" -> pure (restart w { wReduce = not (wReduce w), wHist = Map.empty })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 1 で T を押す。正しい 2 になるのは何 % ？
-- 2. 2 で人数を増やすと、正解率はどうなる？ S（リダクション）にすると？
-- 3. 本物の並列プログラムでは「ロック」や「アトミック命令」でも防げるが、待ち時間が生まれる

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

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
