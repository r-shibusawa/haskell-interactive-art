-- ============================================
-- 41-12 帽子問題と確率
-- n 人が帽子を預け、ランダムに返されたとき「誰も自分の帽子を受け取らない」確率は？
-- 何万回も試して数える（シミュレーション）と、理論値 D_n/n! に近づき、n が大きいと 1/e ≈ 0.3679 に
-- 操作：↑↓ = 人数 n（2〜10）/ スペース = 一時停止 / R = やり直し / マウス左右 = 1 フレームの試行回数
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM_)
import Control.Monad.ST (ST, runST)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Numeric (showFFloat)
import System.Random (StdGen, newStdGen, randomR)

data World = World
  { wN         :: !Int             -- ★ 人数
  , wTrials    :: !Int             -- 試行回数
  , wSuccess   :: !Int             -- 誰も自分の帽子でなかった回数
  , wFixedHist :: !(U.Vector Int)  -- 「自分の帽子だった人数」の分布
  , wPaused    :: !Bool
  , wSeries    :: ![Float]         -- 推定値の推移（新しい順、400 個まで）
  , wShown     :: !(U.Vector Int)  -- 画面に描く 1 回ぶんの配り方
  , wShowTimer :: !Float
  , wGen       :: !StdGen          -- 乱数の種
  }

main :: IO ()
main = runSketch (sketch "41-12 Hat problem" (error "skStart"))
  { skBack  = hsb 226 28 11
  , skStart = \_ -> do
      g <- newStdGen
      pure (reset (World 5 0 0 U.empty False [] U.empty 0 g))
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

reset :: World -> World
reset w =
  let (p, g) = randomPerm (wN w) (wGen w)
  in w { wTrials = 0, wSuccess = 0, wSeries = [], wFixedHist = U.replicate (wN w + 1) 0, wShown = p, wGen = g }

-- ★ Fisher–Yates：0..k-1 をランダムに並べかえる（どの順列も同じ確率）
randomPerm :: Int -> StdGen -> (U.Vector Int, StdGen)
randomPerm k g0 = runST $ do
  p <- U.thaw (U.enumFromN 0 k)
  g1 <- foldM (shuffleStep p) g0 [k - 1, k - 2 .. 1]
  v <- U.unsafeFreeze p
  pure (v, g1)

shuffleStep :: M.MVector s Int -> StdGen -> Int -> ST s StdGen
shuffleStep p g i = do
  let (j, g') = randomR (0, i) g
  M.swap p i j
  pure g'

countFixed :: U.Vector Int -> Int
countFixed = U.ifoldl' (\c i v -> if i == v then c + 1 else c) 0

-- 試行を perFrame 回まとめて行う
runTrials :: Int -> World -> World
runTrials perFrame w =
  let (hist, succ', g') = runST $ do
        h <- U.thaw (wFixedHist w)
        (s, g2) <- trialLoop (wN w) h perFrame (wSuccess w) (wGen w)
        v <- U.unsafeFreeze h
        pure (v, s, g2)
  in w { wFixedHist = hist, wSuccess = succ', wGen = g', wTrials = wTrials w + perFrame }

trialLoop :: Int -> M.MVector s Int -> Int -> Int -> StdGen -> ST s (Int, StdGen)
trialLoop _ _ 0 !s g = pure (s, g)
trialLoop n h t !s g = do
  let (p, g1) = randomPerm n g
      fixed = countFixed p
  M.modify h (+ 1) fixed
  trialLoop n h (t - 1) (if fixed == 0 then s + 1 else s) g1

perFrameOf :: Env -> Int
perFrameOf env = round (clampF 1 400 (remap (fst (mouseP env)) 0 (envW env) 1 400))   -- ★ 1 フレームの試行回数

step :: Env -> Float -> World -> IO World
step env dt w
  | wPaused w = pure w
  | otherwise =
      let w1 = runTrials (perFrameOf env) w
          w2 = if wTrials w1 > 0 && even (envFrame env)
                 then w1 { wSeries = take 400 (fromIntegral (wSuccess w1) / fromIntegral (wTrials w1) : wSeries w1) }
                 else w1
          t = wShowTimer w2 + dt
      in pure $
           if t > 0.6
             then let (p, g) = randomPerm (wN w2) (wGen w2) in w2 { wShowTimer = 0, wShown = p, wGen = g }
             else w2 { wShowTimer = t }

derangements :: Int -> Int
derangements k = go k 1 0 1
  where
    -- d0 = D_(i-1), d1 = D_i
    go :: Int -> Int -> Int -> Int -> Int
    go m d0 d1 i
      | m == 0 = 1
      | i >= m = d1
      | otherwise = go m d1 (i * (d1 + d0)) (i + 1)

factorial :: Int -> Int
factorial k = product [1 .. k]

draw :: Env -> World -> Picture
draw env w =
  let n = wN w
      wd = envW env
      hg = envH env
      est = if wTrials w > 0 then fromIntegral (wSuccess w) / fromIntegral (wTrials w) else 0 :: Float
      theory = fromIntegral (derangements n) / fromIntegral (factorial n) :: Float
      -- 左上：1 回ぶんの配り方（人 i → 帽子 shown[i]）
      hx = 60
      hy = 150
      gap = min 80 (wd * 0.45 / fromIntegral n)
      shown = U.toList (wShown w)
      person (i, s) =
        let x = hx + fromIntegral i * gap
            own = s == i
        in Pictures
             [ Color (hsb (fromIntegral i * 360 / fromIntegral n) 60 95) (hat env x hy)
             , txtC env x (hy + 24) 14 (hsb 0 0 100) ("H" ++ show (i + 1))
             , at (toG env x (hy + 150)) (Color (if own then hsb 20 80 100 else hsb 0 0 80) (disc 15))
             , txtC env x (hy + 185) 14 (hsb 0 0 100) ("P" ++ show (i + 1)) ]
      -- 線：人 i が受け取った帽子 shown[i]
      link (i, s) =
        let x1 = hx + fromIntegral s * gap
            x2 = hx + fromIntegral i * gap
            own = s == i
        in Color (if own then hsb 20 80 100 else hsba 180 80 100 0.6)
                 (thickLine (if own then 4 else 2) (toG env x1 (hy + 8)) (toG env x2 (hy + 134)))
      pairs = zip [0 :: Int ..] shown
      fixedNow = countFixed (wShown w)
      -- 右上：数値
      px = wd * 0.55
      py = 130
      -- 下：推定値の推移（線）
      gx = 60
      gy = hg - 60
      gw = wd * 0.45
      gh = 180
      levelY v = gy - gh * v / 0.6
      curve = [ toG env (gx + fromIntegral i / 400 * gw) (levelY (min 0.6 v))
              | (i, v) <- zip [0 :: Int ..] (reverse (wSeries w)) ]
      -- 右下：自分の帽子だった人数の分布（ポアソン分布 λ=1 に近づく）
      bx = wd * 0.55
      by = hg - 60
      bw = wd * 0.4 / fromIntegral (n + 1)
      bh = 180
      trials1 = max 1 (wTrials w)
      bar (k, c) =
        let ratio = fromIntegral c / fromIntegral trials1 :: Float
            hgt = min bh (ratio * bh / 0.5)
            x = bx + fromIntegral k * bw
            cx = x + bw / 2
            s = drop 1 (fix3 ratio)      -- 0.368 → .368（棒の幅に入るように）
        in Pictures
             [ box env (hsb (if k == (0 :: Int) then 130 else 200) 70 95) (x + 3) (by - hgt) (bw - 6) hgt
             , txtC env cx (by + 18) 14 (hsb 0 0 80) (show k)
             , if wTrials w > 0 then txtC env cx (by - hgt - 4) 14 (hsb 0 0 80) s else Blank ]
  in Pictures
       [ Pictures (map link pairs)
       , Pictures (map person pairs)
       , txt env (hx - 40) (hy - 44) 14 (hsb 0 0 60) "H = hat, P = person. Lines: who got which hat."
       , txt env (hx - 40) (hy + 220) 16 (hsb (if fixedNow == 0 then 130 else 20) 80 100)
           (if fixedNow == 0 then "Nobody has their own hat (a derangement!)"
                             else "People with their own hat: " ++ show fixedNow ++ " (orange)")
       , txt env px py 16 (hsb 0 0 100)
           ("Trials " ++ commas (wTrials w) ++ " (" ++ show (perFrameOf env) ++ " per frame: mouse X)")
       , txt env px (py + 50) 30 (hsb 50 80 100) ("Estimate " ++ fix4 est)
       , txt env px (py + 82) 15 (hsb 0 0 80)
           ("Theory D_" ++ show n ++ "/" ++ show n ++ "! = " ++ show (derangements n) ++ "/" ++ show (factorial n) ++ " = " ++ fix4 theory)
       , txt env px (py + 104) 15 (hsb 0 0 80) ("1/e = " ++ fix4 (exp (-1)))
       , txt env px (py + 128) 15 (hsb 0 0 80) "D_n = (n-1)(D_(n-1) + D_(n-2))"
       , txt env px (py + 150) 15 (hsb 0 0 80) "D_n = n! sum (-1)^k/k! (inclusion-exclusion)"
       -- 推移のグラフ
       , box env (hsba 0 0 100 0.06) gx (gy - gh) gw gh
       , Color (hsb 0 0 80) (Line [toG env gx (levelY theory), toG env (gx + gw) (levelY theory)])
       , Color (hsb 0 0 45) (Line [toG env gx (levelY (exp (-1))), toG env (gx + gw) (levelY (exp (-1)))])
       , Color (hsb 50 80 100) (thickPath 2 curve)
       , txt env gx (gy - gh - 8) 14 (hsb 0 0 70) "Estimate (0-0.6): white = theory, gray = 1/e"
       -- 分布のグラフ
       , Pictures (map bar (zip [0 ..] (U.toList (wFixedHist w))))
       , txt env bx (by - bh - 30) 14 (hsb 0 0 70) "How many got their own hat"
       , txt env bx (by - bh - 10) 14 (hsb 0 0 70) "Large n, e^-1/k!: .368 .368 .184 .061 ..."
       , txt env 20 40 18 (hsb 0 0 100)
           ("n = " ++ show n ++ " (Up/Down)   Space: " ++ (if wPaused w then "resume" else "pause") ++ "   R: restart")
       , txt env 20 68 14 (hsb 0 0 60)
           "D_n of n! handouts give nobody their own hat. n=2: 1/2, n=3: 1/3, n=4: 3/8, ... approaching 1/e."
       ]

-- 帽子（半分の楕円とつば）
hat :: Env -> Float -> Float -> Picture
hat env x y =
  let (gx, gy) = toG env x y
      dome = [(gx + 20 * cos a, gy + 15 * sin a) | i <- [0 .. 16 :: Int], let a = fromIntegral i * pi / 16]
  in Pictures [Polygon dome, rect gx (gy - 1) 52 6]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ | wN w < 10 -> reset w { wN = wN w + 1 }
  EventKey (SpecialKey KeyDown) Down _ _ | wN w > 2 -> reset w { wN = wN w - 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _ | c `elem` "rR" -> reset w
  _ -> w

-- ---------- やってみよう ----------
-- 1. n = 2, 3, 4 で推定値が 0.5, 0.333, 0.375 に落ち着くのを見る。n = 4 で 1/e との差はもうわずか
-- 2. 「ちょうど 1 人だけ自分の帽子」の確率は？ 分布の棒 1 を読む（n 大で 1/e と同じ！）
-- 3. 試行を 10 万回まで増やしたとき、推定値のふらつき幅はどれくらい？（√(p(1−p)/N) が目安）
-- 4. 帽子が「2 つ入れかわっただけ」（2 人だけ交換、あとは自分の）になる確率は C(n,2)/n!。数えて確かめよう

-- ---------- 道具 ----------
fix3, fix4 :: Float -> String
fix3 v = showFFloat (Just 3) v ""
fix4 v = showFFloat (Just 4) v ""

-- 3 桁ごとにカンマを入れる
commas :: Int -> String
commas v = reverse (go (reverse (show v)))
  where
    go s = case splitAt 3 s of
      (a, []) -> a
      (a, b) -> a ++ "," ++ go b

-- 文字の幅のおおよその見積もり
textW :: Float -> String -> Float
textW size s = fromIntegral (length s) * size * 0.85

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

-- 横は中央ぞろえ、y は文字の下
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = txt env (x - textW size s / 2) y size c s
