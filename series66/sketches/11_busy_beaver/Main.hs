-- ============================================
-- 66-11 忙しいビーバーと停止問題 ─ 状態が n 個の機械で、空のテープから始めて「止まるもののうち」いちばんたくさん 1 を書くのは？
-- 記号は 0 と 1、止まる状態 H への最後の 1 手も数える。2 状態は 1 が 4 個（6 手）、4 状態は 13 個（107 手）。
-- 3 状態では「1 の数の王者」（6 個・14 手）と「手数の王者」（21 手・1 は 5 個）が別の機械。
-- 2 状態の機械 20736 個を全部ためすと：止まる / 止まらない（同じ様子のくり返し・ずれながらのくり返しを見つけた）/ 判定できない（手数の上限まで）
-- 操作：1〜4 王者を選ぶ / スペース 再生・一時停止 / ↑↓ 判定の手数の上限 / R 全部ためすのをやり直す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art hiding (cyan, rose, violet)
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.Bits (shiftR, xor, (.&.), (.|.))
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32, Word8)
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- ★ (名前, 自己テスト用の名前, 機械（A0:1RB = 状態 A で 0 を読んだら 1 を書いて右へ、B へ）, 1 の数, 手数)
champs :: [(String, String, String, Int, Int)]
champs =
  [ ("2 states: champion", "2 状態の王者", "A0:1RB A1:1LB B0:1LA B1:1RH", 4, 6)
  , ("3 states: most ones", "3 状態：1 の数の王者", "A0:1RB A1:1RH B0:0RC B1:1RB C0:1LC C1:1LA", 6, 14)
  , ("3 states: most steps", "3 状態：手数の王者", "A0:1RB A1:1RH B0:1LB B1:0RC C0:1LC C1:1LA", 5, 21)
  , ("4 states: champion", "4 状態の王者", "A0:1RB A1:1LB B0:1LA B1:0LC C0:1RH C1:1LD D0:1RD D1:0RA", 13, 107) ]

champOf :: Int -> (String, String, String, Int, Int)
champOf k = at' champs k (headOr ("", "", "", 0, 0) champs)

codeOf :: Int -> String
codeOf k = let (_, _, c, _, _) = champOf k in c

nMachines :: Int
nMachines = 20736                             -- 12^4（2 状態の機械の数）

-- 1 手ごとのテープ（時空図用）
data Row = Row { rTape :: M.Map Int Char, rHead :: !Int, rQ :: String }

data Kind = KHalt | KLoop | KShift | KUnknown deriving (Eq, Show)
data Res = Res { resKind :: !Kind, resOnes :: !Int, resSteps :: !Int }

data World = World
  { wChamp   :: !Int
  , wRows    :: [Row]                         -- 王者の 1 手ごとの様子
  , wNRows   :: !Int
  , wLo      :: !Int                          -- 時空図の左右のはし
  , wHi      :: !Int
  , wShown   :: !Int                          -- 何行目まで見せたか
  , wPlaying :: !Bool
  , wBudget  :: !Int                          -- 判定の手数の上限
  , wIdx     :: !Int                          -- 全部ためす：いま何個目
  , wCounts  :: (Int, Int, Int, Int)          -- 止まる・くり返し・ずれながら・判定できない
  , wBestO   :: !Int
  , wBestS   :: !Int
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "66-11 Busy beaver and the halting problem" world0)
  { skBack  = col bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      let w = loadChamp 0 world0
      wNRows w `seq` pure w { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event }

world0 :: World
world0 = restartEnum (World 0 [] 0 0 0 1 True 40 0 (0, 0, 0, 0) 0 0 [])

-- 1 手ごとのテープを全部記録しておく（時空図用）
loadChamp :: Int -> World -> World
loadChamp k w =
  let m = tmFromCode (codeOf k)
      go c acc = let acc' = Row (cTape c) (cHead c) (cQ c) : acc
                 in if not (cHalted c) && cSteps c < 500 then go (snd (tmStep m c)) acc' else reverse acc'
      rows = go (tmStart m "" 0) []
      ks r = rHead r : M.keys (rTape r)
      lo = minimum (0 : concatMap ks rows)
      hi = maximum (0 : concatMap ks rows)
  in w { wChamp = k, wRows = rows, wNRows = length rows, wLo = lo, wHi = hi, wShown = 1, wPlaying = True }

restartEnum :: World -> World
restartEnum w = w { wIdx = 0, wCounts = (0, 0, 0, 0), wBestO = 0, wBestS = 0 }

-- ---- 2 状態の機械を全部ためす（速いシミュレータ：テープは配列）
-- 1 つの遷移の選び方 12 通り：書く 0/1 × 左右 × 次 A/B/H
optOf :: Int -> (Word8, Int, Int)
optOf k = let (w, r) = k `divMod` 6
              (dk, nx) = r `divMod` 3
          in (fromIntegral w, if dk == 0 then -1 else 1, nx)

-- ずれながらくり返すかを調べるための記録
data Rec = Rec { rcQ :: !Int, rcDir :: !Int, rcPos :: !Int, rcFar :: !Int, rcBase :: !Int, rcSnap :: UArray Int Word8 }

classify :: Int -> Int -> Res
classify idx maxSteps = runST $ do
  let tr = map optOf [idx `mod` 12, (idx `div` 12) `mod` 12, (idx `div` 144) `mod` 12, (idx `div` 1728) `mod` 12]   -- A0 A1 B0 B1
  tape <- newTape
  save <- newTape
  let rd i = if i < 0 || i > 2047 then pure 255 else readArray tape i   -- はみ出し = どの記号とも違う
      sameRange lo hi = allM [lo .. hi] (\i -> (==) <$> readArray tape i <*> readArray save i)
      -- 前の記録と「状態」と「そこから戻って読んだ範囲のテープ」が同じか
      shiftMatch q dir hd rc
        | rcQ rc /= q || rcDir rc /= dir = pure False
        | otherwise =
            let is = takeWhile (/= rcFar rc - dir) (iterate (subtract dir) (rcPos rc))
                (b0, b1) = bounds (rcSnap rc)
                old i = let j = i - rcBase rc in if j >= b0 && j <= b1 then rcSnap rc ! j else 0
            in allM is (\i -> (== old i) <$> rd (hd + (i - rcPos rc)))
      loop !s !hd !q !lo !hi !sq !sh !slo !shi recs
        | s > maxSteps = pure (Res KUnknown 0 maxSteps)
        | otherwise = do
            -- ずれながらくり返す：書いたことのない端のマスに来たら記録して、前の記録とくらべる
            (found, recs1) <-
              if hd < lo || hd > hi
                then do
                  let dir = if hd < lo then -1 else 1
                  f <- anyM recs (shiftMatch q dir hd)
                  if f then pure (True, recs) else do
                    let a = min lo hd
                        b = max hi hd
                    snapL <- mapM (readArray tape) [a .. b]
                    let rc = Rec q dir hd hd a (listArray (0, b - a) snapL)
                        rs = recs ++ [rc]
                    pure (False, if length rs > 8 then drop 1 rs else rs)
                else pure (False, recs)
            if found then pure (Res KShift 0 (s - 1)) else do
              cur <- readArray tape hd
              let (w, d, nx) = at' tr (q * 2 + fromIntegral cur) (0, 1, 2)
              writeArray tape hd w
              let lo1 = min lo hd
                  hi1 = max hi hd
                  hd1 = hd + d
              if hd1 < 1 || hd1 > 2046 then pure (Res KUnknown 0 s) else do
                let recs2 = [ rc { rcFar = if rcDir rc < 0 then max (rcFar rc) hd1 else min (rcFar rc) hd1 } | rc <- recs1 ]   -- どこまで戻ったか
                if nx == 2
                  then do
                    vs <- mapM (readArray tape) [lo1 .. hi1]
                    pure (Res KHalt (sum (map fromIntegral vs)) s)
                  else do
                    -- くり返し：ブレントの方法で 1, 2, 4, 8 … 手目の様子を保存し、まったく同じ様子にもどったら止まらない
                    same <- if nx == sq && hd1 == sh && lo1 == slo && hi1 == shi then sameRange lo1 hi1 else pure False
                    if same then pure (Res KLoop 0 s) else
                      if s .&. (s - 1) == 0
                        then do
                          forM_ [lo1 .. hi1] (\i -> readArray tape i >>= writeArray save i)
                          loop (s + 1) hd1 nx lo1 hi1 nx hd1 lo1 hi1 recs2
                        else loop (s + 1) hd1 nx lo1 hi1 sq sh slo shi recs2
  loop 1 1024 0 1024 1024 (-1) 0 0 0 []

newTape :: ST s (STUArray s Int Word8)
newTape = newArray (0, 2047) 0

allM :: Monad m => [a] -> (a -> m Bool) -> m Bool
allM xs f = case xs of
  [] -> pure True
  (x : rest) -> do ok <- f x; if ok then allM rest f else pure False

anyM :: Monad m => [a] -> (a -> m Bool) -> m Bool
anyM xs f = case xs of
  [] -> pure False
  (x : rest) -> do ok <- f x; if ok then pure True else anyM rest f

-- 1 フレームに 4 ミリ秒だけ進める（60fps のまま）
step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let w1 = if wPlaying w0 && envFrame env `mod` 3 == 0 && wShown w0 < wNRows w0 then w0 { wShown = wShown w0 + 1 } else w0
  t0 <- getMonotonicTime
  let chunk w = foldl' one w [1 .. 64 :: Int]
      one w _ = if wIdx w >= nMachines then w else
        let r = classify (wIdx w) (wBudget w)
            (h, l, s, u) = wCounts w
            cnt = case resKind r of
              KHalt -> (h + 1, l, s, u)
              KLoop -> (h, l + 1, s, u)
              KShift -> (h, l, s + 1, u)
              KUnknown -> (h, l, s, u + 1)
            isH = resKind r == KHalt
        in cnt `seq` w { wIdx = wIdx w + 1, wCounts = cnt
                        , wBestO = if isH then max (wBestO w) (resOnes r) else wBestO w
                        , wBestS = if isH then max (wBestS w) (resSteps r) else wBestS w }
      go w = if wIdx w >= nMachines then pure w else do
        let !w' = chunk w
        t <- getMonotonicTime
        if t - t0 < 0.004 then go w' else pure w'
  go w1

draw :: Env -> World -> Picture
draw env w = view env
  [ hudTitle ("Busy beaver and the halting problem: " ++ cname)
             ("1-4 pick a champion   Space play / pause   Up/Down step limit (" ++ show (wBudget w) ++ ")   R restart the full search")
  -- ---- 王者の機械：状態表と時空図
  , say "State table" 30 100 15 (col ink)
  , Pictures [ say (q ++ [s] ++ " -> " ++ maybe "?" (\(a, b, c) -> [a, b] ++ c) (M.lookup (q, s) (tmRules m))) (30 + i2f j * 110) (124 + i2f i * 22) 14
                   (col (if rQ cur == q && M.findWithDefault '0' (rHead cur) (rTape cur) == s && not done then amber else dimc))
             | (i, q) <- zip [0 :: Int ..] (tmStates m), (j, s) <- zip [0 :: Int ..] "01" ]
  , say ("steps " ++ show (wShown w - 1) ++ "   ones " ++ show ones ++ (if done then "   halted (" ++ show cOnes ++ " ones, " ++ show cSteps' ++ " steps)" else ""))
        30 (124 + i2f nst * 22 + 20) 18 (col (if done then lime else ink))
  , Pictures [ Pictures (Pictures [ Color (col (if M.lookup i (rTape r) == Just '1' then amber else (22, 40, 32)))
                                           (rectTL (30 + i2f (i - wLo w) * cw) (gy + i2f k * ch) (cw - 1) (max 1 (ch - (if ch > 3 then 1 else 0))))
                                  | i <- [wLo w .. wHi w] ]
                         : [Color (col cyan) (rectTL (30 + i2f (rHead r - wLo w) * cw + cw * 0.3) (gy + i2f k * ch) (cw * 0.4) (max 1 (ch - 1)))])
             | (k, r) <- zip [0 :: Int ..] (take (wShown w) (wRows w)) ]
  -- ---- 2 状態の機械を全部ためす
  , say ("All 2-state machines: " ++ show (wIdx w) ++ " / " ++ show nMachines ++ "  (step limit " ++ show (wBudget w) ++ ")") px py 15 (col ink)
  , Pictures [ let y = py + 28 + i2f i * 30
                   bw = max 1 (i2f cnt / i2f nMachines * (envW env - px - 60))
                   txt = lab ++ "  " ++ show cnt
               in Pictures [ Color (colA 200 c) (rectTL px y bw 20)
                           , say txt (min (px + bw + 8) (envW env - 20 - textW txt 14 * 1.2)) (y + 15) 14 (col ink) ]
             | (i, (lab, cnt, c)) <- zip [0 :: Int ..] [("halts", ch0, lime), ("loops (same configuration)", ch1, cyan), ("loops (shifting)", ch2, violet), ("undecided (hit the limit)", ch3, rose)] ]
  , say ("Halting ones: most 1s " ++ show (wBestO w) ++ ", most steps " ++ show (wBestS w)) px (py + 156) 14 (col dimc)
  , say (if wIdx w >= nMachines then "(= the 2-state champion: 4 ones, 6 steps)" else "(still searching ...)") px (py + 176) 14 (col dimc)
  , say "A smaller limit gives more 'undecided'." px (py + 196) 14 (col dimc)
  , say "No limit and no trick can decide every machine:" px (py + 216) 14 (col dimc)
  , drawDiagonal px (py + 250) (envW env - px - 40) (envH env - py - 400)
  , drawSelfTest env (wTest w) ]
  where
    (cname, _, _, cOnes, cSteps') = champOf (wChamp w)
    m = tmFromCode (codeOf (wChamp w))
    nst = length (tmStates m)
    cur = at' (wRows w) (wShown w - 1) (Row M.empty 0 "A")
    done = wShown w >= wNRows w
    ones = M.size (M.filter (== '1') (rTape cur))
    gy = 124 + i2f nst * 22 + 44
    gh = envH env - gy - 40
    ch = min 24 (gh / i2f (max 1 (wNRows w)))
    cw = min 40 ((envW env * 0.5 - 60) / i2f (wHi w - wLo w + 1))
    px = envW env * 0.56
    py = 100
    (ch0, ch1, ch2, ch3) = wCounts w

-- 停止問題の対角線論法の図（H = 止まる、oo = 止まらない）
drawDiagonal :: Float -> Float -> Float -> Float -> Picture
drawDiagonal x y w h = Pictures
  [ say "Halting problem: suppose a machine could decide halting ..." x y 15 (col ink)
  , Pictures [ Pictures (sayM ("M" ++ show (i + 1)) (x + 30) (y + 30 + i2f (i + 1) * cs) 14 (col dimc)
                         : sayM ("in" ++ show (i + 1)) (x + 70 + i2f i * cs) (y + 30) 14 (col dimc)
                         : [ Pictures [ Color (col (if i == j then amber else panel)) (rectTL (x + 70 + i2f j * cs - cs / 2 + 1) (y + 30 + i2f (i + 1) * cs - cs / 2 + 1) (cs - 2) (cs - 2))
                                      , sayM (if v == 1 then "H" else "oo") (x + 70 + i2f j * cs) (y + 30 + i2f (i + 1) * cs) 14 (col (if i == j then (10, 20, 15) else ink)) ]
                           | (j, v) <- zip [0 ..] row ])
             | (i, row) <- zip [0 :: Int ..] pat ]
  , sayM "Anti" (x + 30) ly 14 (col rose)
  , Pictures [ Pictures [ Color (colA 80 rose) (rectTL (x + 70 + i2f j * cs - cs / 2 + 1) (ly - cs / 2 + 1) (cs - 2) (cs - 2))
                        , sayM (if at' (at' pat j []) j 0 == (1 :: Int) then "oo" else "H") (x + 70 + i2f j * cs) ly 14 (col ink) ]
             | j <- [0 .. n - 1] ]
  , Pictures [ say t x (ly + cs * 0.8 + 14 + i2f k * 18) 14 (col dimc) | (k, t) <- zip [0 :: Int ..] expl ] ]
  where
    n = 6 :: Int
    cs = minimum [36, h / i2f (n + 2), w / i2f (n + 4)]
    pat = [[1, 0, 1, 1, 0, 1], [0, 0, 1, 0, 1, 1], [1, 1, 1, 0, 0, 0], [0, 1, 0, 1, 1, 0], [1, 1, 0, 0, 0, 1], [0, 0, 1, 1, 1, 1]]
    ly = y + 30 + i2f (n + 1) * cs
    expl = [ "Build 'Anti': flip every answer on the diagonal"
           , "(each machine run on itself). Anti differs from"
           , "every row, so it is not in the table. But Anti is"
           , "a machine too, so it must be. Contradiction! No"
           , "halting decider works for all machines (Turing 1936)" ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> loadChamp (fromEnum c - fromEnum '1') w
    | isKey 'r' c -> restartEnum w
  EventKey (SpecialKey KeySpace) Down _ _ -> (if wShown w >= wNRows w then w { wShown = 1 } else w) { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> restartEnum w { wBudget = min 200 (wBudget w + 10) }
  EventKey (SpecialKey KeyDown) Down _ _ -> restartEnum w { wBudget = max 5 (wBudget w - 5) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 4 で 4 状態の王者を再生しよう。107 手のあいだ、ヘッドはどんな形で行ったり来たりする？（時空図）
-- 2. ↓ で上限を 5 や 10 にすると「判定できない」が出てくる。上限を上げると減るが、状態が増えると……
-- 3. 5 状態の王者は 4098 個の 1 を 47176870 手で書く。6 状態の答えは今でもわかっていない
-- 4. champs の機械を 1 か所だけ変えてみよう（例：B1:1RH を B1:1LH に）。1 の数と手数はどうなる？
-- 5. 「くり返し」「ずれながらくり返す」の判定は確実。でも「そのうち止まるかも」としか言えない機械はいつまでも判定できない。対角線の図と見くらべよう

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ (jn ++ "：1 の数 " ++ show (tmCount c '1') ++ "、手数 " ++ show (cSteps c) ++ "（期待 " ++ show o ++ " 個・" ++ show st ++ " 手）", cHalted c && tmCount c '1' == o && cSteps c == st)
  | (_, jn, code, o, st) <- champs, let c = tmRun (tmFromCode code) "" 1000 0 ]
  ++
  [ ("2 状態の機械 20736 個（上限 100 手）：止まるものの最大は 1 が " ++ show bo ++ " 個・" ++ show bs ++ " 手（= 王者）", bo == 4 && bs == 6)
  , ("分類の合計 = 20736、上限 100 手で判定できないのは " ++ show nUnk ++ " 個だけ（2000 手まで動かしてもどれも止まらない）"
    , length rs == nMachines && nUnk == 12 && all (\i -> resKind (classify i 2000) /= KHalt) unk)
  , ("「止まらない」と判定した機械は、1500 手まで動かしても本当に止まらない（" ++ show nSure ++ " 個を抜き取り）", okSure && nSure > 200) ]
  where
    rs = [ classify i 100 | i <- [0 .. nMachines - 1] ]
    halts = [ r | r <- rs, resKind r == KHalt ]
    bo = maxOr 0 (map resOnes halts)
    bs = maxOr 0 (map resSteps halts)
    unk = [ i | (i, r) <- zip [0 ..] rs, resKind r == KUnknown ]
    nUnk = length unk
    sure = [ i | i <- [0, 37 .. nMachines - 1], let k = resKind (classify i 100), k == KLoop || k == KShift ]
    nSure = length sure
    okSure = all (\i -> resKind (classify i 1500) /= KHalt) sure

-- ==================================================
-- ここから下は「オートマトンの道具」（中身は読まなくても OK。消さないでね）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具
type RGB = (Float, Float, Float)

bgCol, ink, amber, cyan, lime, rose, violet, dimc, panel, wire :: RGB
bgCol = (9, 20, 15)                           -- 背景（回路基板の黒緑）
ink = (226, 240, 228)                         -- 文字（蛍光の白）
amber = (255, 170, 50)
cyan = (70, 215, 230)
lime = (140, 230, 120)
rose = (255, 100, 120)
violet = (185, 150, 255)
dimc = (125, 155, 138)
panel = (18, 36, 28)
wire = (44, 74, 58)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

scaleRGB :: Float -> RGB -> RGB
scaleRGB k (r, g, b) = (r * k, g * k, b * k)

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

clampI :: Int -> Int -> Int -> Int
clampI v a b = max a (min b v)

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

f2 :: Double -> Float                         -- 描くときの Float（NaN は 0）
f2 v = if isNaN v || isInfinite v then 0 else realToFrac v

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- リストの i 番目を f で変える
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

setAt :: Int -> a -> [a] -> [a]
setAt i v = modifyAt i (const v)

-- i 番目を取りのぞく（splice(i, 1)）
removeAt :: Int -> [a] -> [a]
removeAt i xs = [x | (k, x) <- zip [0 ..] xs, k /= i]

-- 長さ √(a² + b²)（Math.hypot）
hyp :: Float -> Float -> Float
hyp a b = sqrt (a * a + b * b)

-- 文字の入力：allowed に入っている 1 文字だけ足す（長さの上限つき。p5.js 版の typeInto）
typeInto :: String -> Char -> String -> Int -> String
typeInto s ch allowed maxLen = if ch `elem` allowed && length s < maxLen then s ++ [ch] else s

dropLast :: [a] -> [a]                        -- s.slice(0, -1)
dropLast xs = if null xs then xs else init xs

-- キーの名前をそろえる（gloss では スペース・Enter・Backspace・Tab が SpecialKey で来る）
data TypedKey = TChar Char | TBack | TEnter | TTab | TOther

typedKey :: Key -> TypedKey
typedKey k = case k of
  Char '\b' -> TBack
  Char '\DEL' -> TBack
  Char '\t' -> TTab
  Char '\r' -> TEnter
  Char '\n' -> TEnter
  Char c -> TChar c
  SpecialKey KeySpace -> TChar ' '
  SpecialKey KeyBackspace -> TBack
  SpecialKey KeyDelete -> TBack               -- Mac の delete キー
  SpecialKey KeyEnter -> TEnter
  SpecialKey KeyPadEnter -> TEnter
  SpecialKey KeyTab -> TTab
  _ -> TOther

-- 文字（大文字・小文字のどちらでも）か
isKey :: Char -> Char -> Bool
isKey k c = c == k || c == toUpperC k

toUpperC :: Char -> Char
toUpperC c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

nubOrd :: Ord a => [a] -> [a]                 -- 重なりを除く（最初の順を保つ）
nubOrd = go S.empty
  where go _ [] = []
        go seen (x : xs) = if S.member x seen then go seen xs else x : go (S.insert x seen) xs

-- 厳密にリストを評価する（遅延評価の「つけ」をためない）
forceL :: [a] -> [a]
forceL xs = foldl' (flip seq) () xs `seq` xs

-- ---- 数の書き方
-- fmtN(v, d)：小数点以下 d 桁（toFixed と同じく double のちょうどの値で丸め、半分なら大きいほうへ）
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = if d > 0 then ip ++ "." ++ fp else ip
                in if n == 0 || v > 0 then body else '-' : body

-- JavaScript の Math.round（ちょうど .5 は大きいほうへ）
jsRound :: Double -> Integer
jsRound v = let r = floor v in if v - fromIntegral r >= 0.5 then r + 1 else r

showD :: Double -> String                     -- JavaScript の String(v) に近い書き方（0.12、2）
showD v = if v == fromIntegral (jsRound v) && abs v < 1e15 then show (jsRound v) else showFFloat Nothing v ""

-- n を 2 進数の文字列に（n.toString(2)）
toBin :: Int -> String
toBin n = if n <= 0 then "0" else reverse (go n)
  where go 0 = ""
        go k = (if odd k then '1' else '0') : go (k `div` 2)

-- 2 進数の文字列を数に（parseInt(s, 2)。0 と 1 でない文字で止まる）
fromBin :: String -> Int
fromBin = foldl' (\a c -> 2 * a + (if c == '1' then 1 else 0)) 0 . takeWhile (`elem` "01")

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：種 s の列（0 なら 1）
rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 計算を種 s から実行して、結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

rndPick :: String -> R Char                   -- 文字の中から 1 つ
rndPick cs = do u <- rnd; pure (at' cs (floor (u * fromIntegral (length cs))) ' ')

rndString :: String -> Int -> R String        -- alpha の文字で長さ len の文字列
rndString alpha len = mapM (const (rndPick alpha)) [1 .. len]

-- n 回くり返して結果を順に集める（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- ---- 有限オートマトンの道具
-- DFA = 状態の数・記号・遷移（dTr !! q で、記号 → 次の状態）・開始・受理
--   遷移がない記号 = 「行き止まり」（そこで不受理が決まる）
-- NFA = 遷移が「次の状態のリスト」。記号 epsSym が ε 遷移
type Sym = Char

data DFA = DFA { dN :: !Int, dAlpha :: [Sym], dTr :: [M.Map Sym Int], dStart :: !Int, dAcc :: [Bool] }
data NFA = NFA { nN :: !Int, nAlpha :: [Sym], nTr :: [M.Map Sym [Int]], nStart :: !Int, nAcc :: [Bool] }

epsSym :: Sym                                 -- ε（文字を読まずに進む）
epsSym = '\0'

mkDfa :: [Sym] -> [[(Sym, Int)]] -> Int -> [Bool] -> DFA
mkDfa alpha rows st acc = DFA (length rows) alpha (map M.fromList rows) st acc

mkNfa :: [Sym] -> [[(Sym, [Int])]] -> Int -> [Bool] -> NFA
mkNfa alpha rows st acc = NFA (length rows) alpha (map M.fromList rows) st acc

dfaNext :: DFA -> Int -> Sym -> Int
dfaNext d q c
  | q < 0 = -1
  | otherwise = case M.lookup c (at' (dTr d) q M.empty) of
      Just t | t >= 0 && t < dN d -> t
      _ -> -1

-- たどった状態の列（-1 = 行き止まり）
dfaTrace :: DFA -> String -> [Int]
dfaTrace d = scanl (dfaNext d) (dStart d)

dfaAccepts :: DFA -> String -> Bool
dfaAccepts d s = go (dStart d) s
  where go q [] = q >= 0 && at' (dAcc d) q False
        go q (c : cs) = let t = dfaNext d q c in t >= 0 && go t cs

-- 行き止まりを「ごみ箱」状態にまとめて、全部の遷移をそろえる（返り値の 2 つ目 = ごみ箱の番号、なければ −1）
completeDfaS :: DFA -> (DFA, Int)
completeDfaS d =
  let missing = or [ dfaNext d q c < 0 | q <- [0 .. dN d - 1], c <- dAlpha d ]
      sink = dN d
      fillRow q = foldl' (\m c -> if dfaNext d q c < 0 then M.insert c sink m else m) (at' (dTr d) q M.empty) (dAlpha d)
  in if not missing then (d, -1) else
       ( d { dN = dN d + 1
           , dTr = [ fillRow q | q <- [0 .. dN d - 1] ] ++ [M.fromList [ (c, sink) | c <- dAlpha d ]]
           , dAcc = take (dN d) (dAcc d ++ repeat False) ++ [False] }
       , sink )

completeDfa :: DFA -> DFA
completeDfa = fst . completeDfaS

complementDfa :: DFA -> DFA                   -- 受理と不受理を入れかえる
complementDfa d = let e = completeDfa d in e { dAcc = map not (dAcc e) }

data Op = OpAnd | OpOr | OpDiff | OpXor deriving (Eq, Show)

opFun :: Op -> Bool -> Bool -> Bool
opFun o x y = case o of
  OpAnd -> x && y
  OpOr -> x || y
  OpDiff -> x && not y
  OpXor -> x /= y

data Product = Product { prD :: DFA, prPairs :: [(Int, Int)], prLeft :: DFA, prRight :: DFA }

-- 積の構成：状態 = (A の状態, B の状態) の組。たどれる組だけ、見つけた順に番号をつける
productDfa :: DFA -> DFA -> Op -> Product
productDfa a0 b0 o =
  let a = completeDfa a0
      b = completeDfa b0
      alpha = filter (`elem` dAlpha b) (dAlpha a)
      start = (dStart a, dStart b)
      loop h ids prs n rows
        | h >= n = (prs, reverse rows)
        | otherwise =
            let (p, q) = IM.findWithDefault (0, 0) h prs
                addSym (ids1, prs1, n1, row) c =
                  let t = (dfaNext a p c, dfaNext b q c)
                  in case M.lookup t ids1 of
                       Just k -> (ids1, prs1, n1, M.insert c k row)
                       Nothing -> (M.insert t n1 ids1, IM.insert n1 t prs1, n1 + 1, M.insert c n1 row)
                (ids', prs', n', row') = foldl' addSym (ids, prs, n, M.empty) alpha
            in loop (h + 1) ids' prs' n' (row' : rows)
      (pairsM, trs) = loop 0 (M.singleton start 0) (IM.singleton 0 start) 1 []
      pairs = IM.elems pairsM
      accOf (p, q) = opFun o (at' (dAcc a) p False) (at' (dAcc b) q False)
  in Product (DFA (length pairs) alpha trs 0 (map accOf pairs)) pairs a b

-- 幅優先探索：受理状態までの一番短い文字列（Nothing = 空言語）
dfaShortestAccepted :: DFA -> Maybe String
dfaShortestAccepted d
  | dStart d < 0 || dStart d >= dN d = Nothing
  | otherwise = go [dStart d] [] (IM.singleton (dStart d) "")
  where
    go [] [] _ = Nothing
    go [] next seen = go (reverse next) [] seen
    go (q : qs) next seen
      | at' (dAcc d) q False = Just (reverse (IM.findWithDefault "" q seen))
      | otherwise =
          let path = IM.findWithDefault "" q seen
              (next', seen') = foldl' (\(nx, sn) c -> let t = dfaNext d q c in
                                          if t >= 0 && not (IM.member t sn) then (t : nx, IM.insert t (c : path) sn) else (nx, sn))
                                      (next, seen) (dAlpha d)
          in go qs next' seen'

-- Nothing なら同じ言語、そうでなければ反例
dfaEquivalent :: DFA -> DFA -> Maybe String
dfaEquivalent a b = dfaShortestAccepted (prD (productDfa a b OpXor))

-- スタートからたどれない状態を消す（2 つ目 = 新しい番号 → 元の番号）
reachableDfa :: DFA -> (DFA, [Int])
reachableDfa d =
  let go h order idx
        | h >= length order = (order, idx)
        | otherwise =
            let q = at' order h 0
                (order', idx') = foldl' (\(o, ix) c -> let t = dfaNext d q c in
                                            if t >= 0 && not (IM.member t ix) then (o ++ [t], IM.insert t (length o) ix) else (o, ix))
                                        (order, idx) (dAlpha d)
            in go (h + 1) order' idx'
      (ord', idxM) = go 0 [dStart d] (IM.singleton (dStart d) 0)
      row q = M.fromList [ (c, IM.findWithDefault 0 t idxM) | c <- dAlpha d, let t = dfaNext d q c, t >= 0 ]
  in (DFA (length ord') (dAlpha d) (map row ord') 0 (map (\q -> at' (dAcc d) q False) ord'), ord')

data Mini = Mini { mWork :: DFA, mOld :: [Int], mRounds :: [[Int]], mBlockOf :: [Int], mMin :: DFA }

-- 見つけた順に 0, 1, 2, … と番号をつけなおす
renum :: Ord k => [k] -> [Int]
renum keys = reverse (snd (foldl' (\(m, out) k -> case M.lookup k m of
                                     Just v -> (m, v : out)
                                     Nothing -> let v = M.size m in (M.insert k v m, v : out)) (M.empty, []) keys))

-- ムーアの分割の精密化：「区別できない状態」を同じブロックに
minimizeMoore :: DFA -> Mini
minimizeMoore d =
  let (e, old) = reachableDfa (completeDfa d)
      blk0 = renum (map (\v -> if v then 1 else 0 :: Int) (dAcc e))
      nDistinct = S.size . S.fromList
      refine blk rounds =
        let keys = [ (b, [ at' blk (dfaNext e q c) 0 | c <- dAlpha e ]) | (q, b) <- zip [0 ..] blk ]
            nb = renum keys
        in if nDistinct nb == nDistinct blk then (blk, reverse rounds) else refine nb (nb : rounds)
      (blk, rounds) = refine blk0 [blk0]
      k = nDistinct blk
      accs = IM.fromList [ (b, at' (dAcc e) q False) | (q, b) <- zip [0 ..] blk ]
      trs = IM.fromListWith M.union [ (b, M.fromList [ (c, at' blk (dfaNext e q c) 0) | c <- dAlpha e ]) | (q, b) <- zip [0 ..] blk ]
      mn = DFA k (dAlpha e) [ IM.findWithDefault M.empty b trs | b <- [0 .. k - 1] ] (at' blk (dStart e) 0) [ IM.findWithDefault False b accs | b <- [0 .. k - 1] ]
  in Mini e old rounds blk mn

-- 長さ 0〜maxLen の文字列を全部（短い順）
allStrings :: [Sym] -> Int -> [String]
allStrings alpha maxLen = concat (take (maxLen + 1) (iterate (\layer -> [ s ++ [c] | s <- layer, c <- alpha ]) [""]))

nfaRow :: NFA -> Int -> Sym -> [Int]
nfaRow nfa q c = M.findWithDefault [] c (at' (nTr nfa) q M.empty)

-- ε 遷移だけでたどれる状態を全部足す（小さい順）
epsClosure :: NFA -> [Int] -> [Int]
epsClosure nfa states = S.toAscList (go (S.fromList states) states)
  where go seen [] = seen
        go seen (q : todo) = let new = [ t | t <- nfaRow nfa q epsSym, not (S.member t seen) ]
                             in go (foldl' (flip S.insert) seen new) (new ++ todo)

nfaMove :: NFA -> [Int] -> Sym -> [Int]
nfaMove nfa set c = epsClosure nfa (S.toList (S.fromList (concatMap (\q -> nfaRow nfa q c) set)))

nfaTrace :: NFA -> String -> [[Int]]
nfaTrace nfa = scanl (nfaMove nfa) (epsClosure nfa [nStart nfa])

nfaAccepts :: NFA -> String -> Bool
nfaAccepts nfa s = go (epsClosure nfa [nStart nfa]) s
  where go cur [] = any (\q -> at' (nAcc nfa) q False) cur
        go cur (c : cs) = let nx = nfaMove nfa cur c in not (null nx) && go nx cs

data Subset = Subset { sSets :: [[Int]], sRows :: [(Int, M.Map Sym Int)], sDfa :: DFA }

-- 部分集合構成：DFA の 1 状態 = NFA の状態の集合（たどれる集合だけ、見つけた順に番号）
subsetConstruct :: NFA -> Int -> Subset
subsetConstruct nfa maxStates =
  let first = epsClosure nfa [nStart nfa]
      loop h sets ids n rows
        | h >= n || n > maxStates = (sets, reverse rows)
        | otherwise =
            let cur = IM.findWithDefault [] h sets
                addSym (sets1, ids1, n1, row) c =
                  let t = nfaMove nfa cur c
                  in case M.lookup t ids1 of
                       Just k -> (sets1, ids1, n1, M.insert c k row)
                       Nothing -> (IM.insert n1 t sets1, M.insert t n1 ids1, n1 + 1, M.insert c n1 row)
                (sets', ids', n', row') = foldl' addSym (sets, ids, n, M.empty) (nAlpha nfa)
            in loop (h + 1) sets' ids' n' ((h, row') : rows)
      (setsM, rows) = loop 0 (IM.singleton 0 first) (M.singleton first 0) 1 []
      sets = IM.elems setsM
      dfa = DFA (length sets) (nAlpha nfa) (map snd rows) 0 [ any (\q -> at' (nAcc nfa) q False) st | st <- sets ]
  in Subset sets rows dfa

setName :: [Int] -> String
setName st = "{" ++ joinWith "," (map show st) ++ "}"

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  _ -> foldr1 (\a b -> a ++ sep ++ b) xs

-- ---- 状態遷移図を描く道具（円 = 状態、二重丸 = 受理、矢印 = 遷移。曲線は折れ線で描く）
data Pos = Pos { pX :: !Float, pY :: !Float, pLoop :: Maybe Float }   -- 位置と自己ループの向き

-- 辺 = 出る状態・入る状態・ラベル・記号のリスト
data Edge = Edge { eFrom :: !Int, eTo :: !Int, eLabel :: String, eSyms :: [String] }

epsLabel :: String                            -- ε の矢印のラベル（画面は ASCII だけなので "eps"）
epsLabel = "eps"

-- 2 次ベジェ曲線 → 折れ線の点
bezPts :: Float -> Float -> Float -> Float -> Float -> Float -> Int -> [(Float, Float)]
bezPts x0 y0 cx cy x1 y1 n =
  [ (u * u * x0 + 2 * u * t * cx + t * t * x1, u * u * y0 + 2 * u * t * cy + t * t * y1) | i <- [0 .. n], let t = i2f i / i2f n, let u = 1 - t ]

-- 3 次ベジェ曲線 → 折れ線の点（自己ループ用）
cubicPts :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Int -> [(Float, Float)]
cubicPts (p0x, p0y) (p1x, p1y) (p2x, p2y) (p3x, p3y) n =
  [ (a * p0x + b * p1x + c * p2x + d * p3x, a * p0y + b * p1y + c * p2y + d * p3y)
  | i <- [0 .. n], let t = i2f i / i2f n, let u = 1 - t, let a = u * u * u, let b = 3 * u * u * t, let c = 3 * u * t * t, let d = t * t * t ]

-- 折れ線 + 先の三角（dashed = 点線：1 つおきの区間だけ描く）
arrowLine :: [(Float, Float)] -> RGB -> Float -> Bool -> Picture
arrowLine pts c w dashed = case reverse pts of
  ((x1, y1) : (x0, y0) : _) ->
    let l0 = hyp (x1 - x0) (y1 - y0)
        l = if l0 == 0 then 1 else l0
        ux = (x1 - x0) / l
        uy = (y1 - y0) / l
        s = 7 + w * 1.5
        body = if dashed then Pictures [ lineW w p q | (k, p, q) <- zip3 [0 :: Int ..] pts (drop 1 pts), even k ] else polyline w pts
        tip = Polygon [(x1, y1), (x1 - ux * s - uy * s * 0.5, y1 - uy * s + ux * s * 0.5), (x1 - ux * s + uy * s * 0.5, y1 - uy * s - ux * s * 0.5)]
    in Color (col c) (Pictures [body, tip])
  _ -> Blank

-- 状態ごとの「ラベル → 行き先のリスト」から辺のリスト（同じ向きの辺はラベルをまとめる。p5.js 版の edgeList）
edgesFrom :: [[(String, [Int])]] -> [Edge]
edgesFrom rows =
  let n = length rows
      items = [ ((q, t), lab) | (q, row) <- zip [0 ..] rows, (lab, ts) <- row, t <- ts, t >= 0, t < n ]
      keys = nubOrd (map fst items)
      symsOf k = [ lab | (k', lab) <- items, k' == k ]
      sortSyms = sortOn (\s -> (s == epsLabel, s))
  in [ Edge q t (joinWith "," (sortSyms (symsOf (q, t)))) (symsOf (q, t)) | (q, t) <- keys ]

symLabel :: Sym -> String
symLabel c = if c == epsSym then epsLabel else [c]

dfaEdges :: DFA -> [Edge]
dfaEdges d = edgesFrom [ [ (symLabel c, [t]) | (c, t) <- M.toList row ] | row <- take (dN d) (dTr d ++ repeat M.empty) ]

nfaEdges :: NFA -> [Edge]
nfaEdges m = edgesFrom [ [ (symLabel c, ts) | (c, ts) <- M.toList row ] | row <- take (nN m) (nTr m ++ repeat M.empty) ]

-- 1 本の辺の折れ線の点とラベルの位置
edgeGeometry :: Edge -> Pos -> Pos -> Float -> Bool -> ([(Float, Float)], (Float, Float))
edgeGeometry e a b r both
  | eFrom e == eTo e =                         -- 自己ループ：中心から外向き（または上）に輪
      let ang = maybe (-pi / 2) id (pLoop a)
          ux = cos ang
          uy = sin ang
          px = -uy
          py = ux
          s0 = (pX a + (ux * 0.6 + px * 0.8) * r, pY a + (uy * 0.6 + py * 0.8) * r)
          s1 = (pX a + (ux * 0.6 - px * 0.8) * r, pY a + (uy * 0.6 - py * 0.8) * r)
          pts = cubicPts s0 (fst s0 + (ux * 2.2 + px * 0.9) * r, snd s0 + (uy * 2.2 + py * 0.9) * r)
                            (fst s1 + (ux * 2.2 - px * 0.9) * r, snd s1 + (uy * 2.2 - py * 0.9) * r) s1 18
      in (pts, (pX a + ux * 2.35 * r, pY a + uy * 2.35 * r))
  | otherwise =
      let dx = pX b - pX a
          dy = pY b - pY a
          l0 = hyp dx dy
          l = if l0 == 0 then 1 else l0
          ux = dx / l
          uy = dy / l
          bend = if both then 0.22 * l else 0
          mx = (pX a + pX b) / 2 - uy * bend
          my = (pY a + pY b) / 2 + ux * bend
          d0x = mx - pX a
          d0y = my - pY a
          l0' = nz (hyp d0x d0y)
          d1x = mx - pX b
          d1y = my - pY b
          l1 = nz (hyp d1x d1y)
          pts = bezPts (pX a + (d0x / l0') * r) (pY a + (d0y / l0') * r) mx my (pX b + (d1x / l1) * (r + 2)) (pY b + (d1y / l1) * (r + 2)) 16
          (midX, midY) = at' pts 8 (mx, my)
      in (pts, (midX - uy * 13, midY + ux * 13))
  where nz v = if v == 0 then 1 else v

-- 図の描き方の選択（p5.js 版の opt）
data MOpt = MOpt
  { oR :: Float                               -- 円の半径
  , oNames :: Maybe [String]                  -- 状態の名前（Nothing なら q0, q1, …）
  , oHi :: IM.IntMap RGB                      -- 色をつける状態
  , oEdgeHi :: Edge -> Maybe RGB              -- 色をつける辺
  , oStart :: Bool                            -- 開始の矢印を描くか
  , oLabels :: Bool                           -- 辺のラベルを描くか
  , oLabelSize :: Float
  , oNameSize :: Maybe Float
  , oEdgeCol :: RGB
  , oLabelCol :: RGB
  , oStartCol :: RGB
  , oStartAng :: Maybe Float }

defOpt :: MOpt
defOpt = MOpt 22 Nothing IM.empty (const Nothing) True True 14 Nothing (120, 160, 140) (200, 225, 210) ink Nothing

-- 状態の数 n・開始・受理・辺・位置から図を描く
drawMachine :: MOpt -> Int -> Int -> [Bool] -> [Edge] -> [Pos] -> Picture
drawMachine opt n start acc edges pos = Pictures (edgePics ++ [startPic] ++ statePics)
  where
    r = oR opt
    has = S.fromList [ (eFrom e, eTo e) | e <- edges ]
    posOf q = if q >= 0 then at' (map Just pos) q Nothing else Nothing
    edgePics =
      [ Pictures [ arrowLine pts (maybe (oEdgeCol opt) id hc) (if hc == Nothing then 1.8 else 3.5) (eLabel e == epsLabel && hc == Nothing)
                 , if oLabels opt || hc /= Nothing then sayM (eLabel e) lx ly (max 14 (oLabelSize opt)) (col (maybe (oLabelCol opt) id hc)) else Blank ]
      | e <- edges, Just a <- [posOf (eFrom e)], Just b <- [posOf (eTo e)]
      , let (pts, (lx, ly)) = edgeGeometry e a b r (eFrom e /= eTo e && S.member (eTo e, eFrom e) has)
      , let hc = oEdgeHi opt e ]
    startPic = case posOf start of
      Just p | oStart opt && start < n ->
        let sa = case oStartAng opt of
              Just v -> v
              Nothing -> case pLoop p of
                Just lp | cos lp < -0.5 -> -2.2
                _ -> pi
            ux = cos sa
            uy = sin sa
        in arrowLine [(pX p + ux * (r + 34), pY p + uy * (r + 34)), (pX p + ux * (r + 2), pY p + uy * (r + 2))] (oStartCol opt) 2 False
      _ -> Blank
    statePics =
      [ Pictures [ Color (col (maybe panel (scaleRGB 0.35) hc)) (circleAt (pX p) (pY p) (2 * r))
                 , Color (col (maybe (150, 190, 170) id hc)) (ringAt (pX p) (pY p) (2 * r) sw)
                 , if at' acc q False then Color (col (maybe (150, 190, 170) id hc)) (ringAt (pX p) (pY p) (2 * r - 9) sw) else Blank
                 , sayM nm (pX p) (pY p + 1) (max 14 (maybe (min 16 (r * 0.75)) id (oNameSize opt))) (col (maybe ink (const (255, 255, 255)) hc)) ]
      | q <- [0 .. n - 1], Just p <- [posOf q]
      , let hc = IM.lookup q (oHi opt)
      , let sw = if hc == Nothing then 2 else 3.5
      , let nm = maybe ("q" ++ show q) (\ns -> at' ns q "") (oNames opt) ]

drawDfa :: MOpt -> DFA -> [Pos] -> Picture
drawDfa opt d = drawMachine opt (dN d) (dStart d) (dAcc d) (dfaEdges d)

drawNfa :: MOpt -> NFA -> [Pos] -> Picture
drawNfa opt m = drawMachine opt (nN m) (nStart m) (nAcc m) (nfaEdges m)

-- 円の上に並べる（自己ループは外向き）
circleLayout :: Int -> Float -> Float -> Float -> Float -> [Pos]
circleLayout n cx cy rad a0 =
  [ Pos (cx + rad * cos a) (cy + rad * sin a) (Just a) | i <- [0 .. n - 1], let a = a0 + 2 * pi * i2f i / i2f (max 1 n) ]

-- スタートからの距離（幅優先）で左 → 右に並べる
layerLayout :: Int -> Int -> [Edge] -> Float -> Float -> Float -> Float -> [Pos]
layerLayout n start edges x y w h
  | n <= 0 = []
  | otherwise =
      let st = clampI start 0 (n - 1)
          bfs i order depth
            | i >= length order = (order, depth)
            | otherwise =
                let q = at' order i 0
                    dq = IM.findWithDefault 0 q depth
                    (order', depth') = foldl' (\(o, dp) e -> if eFrom e == q && not (IM.member (eTo e) dp) && eTo e >= 0 && eTo e < n
                                                               then (o ++ [eTo e], IM.insert (eTo e) (dq + 1) dp) else (o, dp)) (order, depth) edges
                in bfs (i + 1) order' depth'
          (order0, depth0) = bfs 0 [st] (IM.singleton st 0)
          maxD0 = maxOr 0 (IM.elems depth0)
          rest = [ q | q <- [0 .. n - 1], not (IM.member q depth0) ]
          depth = foldl' (\dp q -> IM.insert q (maxD0 + 1) dp) depth0 rest
          order = order0 ++ rest
          maxD = maxOr 0 (IM.elems depth)
          cols = IM.fromListWith (flip (++)) [ (IM.findWithDefault 0 q depth, [q]) | q <- order ]
          posM = IM.fromList [ (q, Pos (x + (if maxD > 0 then i2f d / i2f maxD * w else w / 2)) (y + i2f (k + 1) / i2f (length cs + 1) * h) (Just (-pi / 2)))
                             | (d, cs) <- IM.toList cols, (k, q) <- zip [0 :: Int ..] cs ]
      in [ IM.findWithDefault (Pos x y Nothing) q posM | q <- [0 .. n - 1] ]

-- (x, y) の上にある状態（後ろから調べる。なければ −1）
hitState :: [Pos] -> Float -> Float -> Float -> Int
hitState pos x y r = headOr (-1) [ q | (q, p) <- reverse (zip [0 ..] pos), hyp (pX p - x) (pY p - y) < r ]

-- ---- チューリング機械の道具
-- 機械 = 状態・記号・空白・規則（(状態, 読んだ記号) → (書く記号, 向き 'L' 'R', 次の状態)）・開始・止まる状態
--   規則がないときも止まる（「止まる」= 規則がない or 状態 H）
data TM = TM { tmStates :: [String], tmSyms :: [Char], tmBlank :: Char, tmRules :: M.Map (String, Char) (Char, Char, String), tmStartQ :: String, tmHalt :: String }

data Cfg = Cfg { cTape :: M.Map Int Char, cHead :: !Int, cQ :: String, cSteps :: !Int, cHalted :: !Bool, cLo :: !Int, cHi :: !Int }

tmStart :: TM -> String -> Int -> Cfg
tmStart m s h = Cfg (M.fromList [ (i, c) | (i, c) <- zip [0 ..] s, c /= tmBlank m ]) h (tmStartQ m) 0 False 0 (max 0 (length s - 1))

tmRead :: Cfg -> Int -> Char -> Char
tmRead cfg i blank = M.findWithDefault blank i (cTape cfg)

-- 1 手：読む → 書く → 動く → 状態を変える（動いたら True）
tmStep :: TM -> Cfg -> (Bool, Cfg)
tmStep m cfg
  | cHalted cfg = (False, cfg)
  | otherwise = case M.lookup (cQ cfg, tmRead cfg (cHead cfg) (tmBlank m)) (tmRules m) of
      Just (w, d, nq) | cQ cfg /= tmHalt m ->
        let tape = if w == tmBlank m then M.delete (cHead cfg) (cTape cfg) else M.insert (cHead cfg) w (cTape cfg)
            h = cHead cfg + (if d == 'R' then 1 else if d == 'L' then -1 else 0)
        in (True, cfg { cTape = tape, cHead = h, cQ = nq, cSteps = cSteps cfg + 1, cLo = min (cLo cfg) h, cHi = max (cHi cfg) h, cHalted = nq == tmHalt m })
      _ -> (False, cfg { cHalted = True })

tmRun :: TM -> String -> Int -> Int -> Cfg
tmRun m s maxSteps h = go (tmStart m s h)
  where go c = if cHalted c || cSteps c >= maxSteps then c else go (snd (tmStep m c))

tmTape :: Cfg -> Char -> Int -> Int -> String
tmTape cfg blank lo hi = [ tmRead cfg i blank | i <- [lo .. hi] ]

-- 空白でない部分だけの文字列
tmContent :: Cfg -> Char -> String
tmContent cfg blank = case (M.lookupMin (cTape cfg), M.lookupMax (cTape cfg)) of
  (Just (lo, _), Just (hi, _)) -> tmTape cfg blank lo hi
  _ -> ""

tmCount :: Cfg -> Char -> Int
tmCount cfg sym = M.size (M.filter (== sym) (cTape cfg))

-- 'A0:1RB A1:1LB B0:1LA B1:1RH' のような短い書き方から
tmFromCode :: String -> TM
tmFromCode code =
  let parts = words code
      rule p = case break (== ':') p of
        (k, ':' : (w : d : nq)) | not (null k) -> Just ((init k, last k), (w, d, nq))
        _ -> Nothing
      rs = [ r | Just r <- map rule parts ]
  in TM (nubOrd [ q | ((q, _), _) <- rs ]) "01" '0' (M.fromList rs) "A" "H"

-- テープのマスとヘッド（x, y = 左上、cw = マスの幅、ncell マス、center = 真ん中のマスの番号、cols = 記号の色）
drawTape :: Cfg -> Char -> Float -> Float -> Float -> Int -> Int -> (Char -> Maybe RGB) -> Picture
drawTape cfg blank x y cw ncell center cols =
  let lo = center - ncell `div` 2
      ts = min 22 (cw * 0.6)
  in Pictures
       [ Pictures [ Color (col (maybe panel id (cols v))) (rectTL cx y (cw - 2) (cw * 1.2))
                  , Color (col wire) (frameTL 1 cx y (cw - 2) (cw * 1.2))
                  , if v == blank then Blank else sayM [v] (cx + cw / 2 - 1) (y + cw * 0.6) (max 14 ts) (col ink)
                  , if i == cHead cfg
                      then Pictures [ Color (col amber) (Polygon [(cx + cw / 2 - 1, y - 4), (cx + cw / 2 - 10, y - 18), (cx + cw / 2 + 8, y - 18)])
                                    , Color (col amber) (frameTL 3 cx y (cw - 2) (cw * 1.2)) ]
                      else Blank ]
       | k <- [0 .. ncell - 1], let i = lo + k, let v = tmRead cfg i blank, let cx = x + i2f k * cw ]

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 左揃え・右揃えで上下は中央（textAlign(LEFT, CENTER)、(RIGHT, CENTER)）
sayLM, sayRM :: String -> Float -> Float -> Float -> Color -> Picture
sayLM s x y size c = say s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (max 0 d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (max 0 d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> thickPath w pts
  _ -> Blank

-- 点線（長さ on の線と off のすき間をくり返す）
dashLine :: Float -> Float -> Float -> (Float, Float) -> (Float, Float) -> Picture
dashLine w on off (x1, y1) (x2, y2) =
  let l = hyp (x2 - x1) (y2 - y1)
      n = floor (l / (on + off)) :: Int
      ux = if l == 0 then 0 else (x2 - x1) / l
      uy = if l == 0 then 0 else (y2 - y1) / l
  in Pictures [ lineW w (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + on), y1 + uy * min l (s + on)) | k <- [0 .. n], let s = i2f k * (on + off), s < l ]

-- 左上のタイトルと操作の説明（説明は \n で改行、行の間 19）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- ---- 自己テストの道具
-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 右下の小さな表示
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 230) (envH env - 14) 14 (if all snd rs then col lime else col rose)
