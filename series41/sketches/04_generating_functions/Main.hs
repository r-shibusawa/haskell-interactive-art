-- ============================================
-- 41-04 母関数で数える
-- 「コインで金額 n をつくる方法の数」は、多項式のかけ算で全部まとめて出せる
--   (1 + x + x^2 + …)(1 + x^5 + x^10 + …)… の x^n の係数 = n 円のつくり方
-- 操作：1〜6 = コインの種類を ON/OFF（1, 2, 5, 10, 20, 50）/ P = 分割数モード（1〜N 全部の数）
--       スペース = もう一度アニメ / ↑↓ = 最大金額
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M

coinKinds :: [Int]
coinKinds = [1, 2, 5, 10, 20, 50]

data World = World
  { wMax     :: !Int             -- ★ 最大の金額（x^N まで計算）
  , wOn      :: ![Bool]          -- ★ 使うコイン
  , wPart    :: !Bool            -- 分割数モード
  , wFactors :: ![Int]           -- 使うコインの額面（順にかけていく）
  , wPoly    :: !(U.Vector Int)  -- いまの多項式の係数
  , wStage   :: !Int             -- いくつ目の因子までかけたか
  , wT       :: !Float           -- アニメの時間
  }

main :: IO ()
main = runSketch (sketch "41-04 Generating functions" (rebuild (World 40 [True, True, True, False, False, False] False [] U.empty 0 0)))
  { skBack  = hsb 230 28 11
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

rebuild :: World -> World
rebuild w =
  let n = wMax w
      fs | wPart w = [1 .. n]
         | otherwise = [c | (c, True) <- zip coinKinds (wOn w)]
  in w { wFactors = fs
       , wPoly = U.generate (n + 1) (\i -> if i == 0 then 1 else 0)   -- 何もかけていない状態 = 1
       , wStage = 0, wT = 0 }

-- ★ 1 つの因子 1/(1 − x^c) = 1 + x^c + x^2c + … をかける
--   係数の配列に対して poly[n] += poly[n − c]（小さい n から順に）
multiplyFactor :: Int -> U.Vector Int -> U.Vector Int
multiplyFactor c = U.modify $ \p ->
  forM_ [c .. M.length p - 1] $ \i -> do
    a <- M.read p (i - c)
    b <- M.read p i
    M.write p i (a + b)

step :: Env -> Float -> World -> IO World
step _ dt w = pure $ case drop (wStage w) (wFactors w) of
  [] -> w
  (c : _) ->
    let t = wT w + dt
        interval = if wPart w then 0.12 else 0.9   -- ★ 次の因子をかけるまでの秒数
    in if t > interval
         then w { wT = 0, wPoly = multiplyFactor c (wPoly w), wStage = wStage w + 1 }
         else w { wT = t }

draw :: Env -> World -> Picture
draw env w =
  let n = wMax w
      x0 = 60
      y0 = envH env - 90
      bw = (envW env - 120) / fromIntegral (n + 1)
      maxV = U.foldl' max 1 (wPoly w)
      hMax = envH env - 320
      lastF = if wStage w > 0 then headOr 0 (drop (wStage w - 1) (wFactors w)) else 0
      bar (i, v) =
        let -- 対数で高さを決める（大きい数も見える）
            hgt = log (fromIntegral v + 1) / log (fromIntegral maxV + 1) * hMax :: Float
            fresh = lastF > 0 && i >= lastF
            x = x0 + fromIntegral i * bw
            cx = x + bw / 2
            s = show v
            -- 棒の幅に入らない数字は 90 度回して書く
            value
              | v <= 0 = Blank
              | textW 14 s <= bw = txtC env cx (y0 - hgt - 4) 14 (hsb 0 0 75) s
              | hgt > textW 14 s + 6 = vertical env (cx + 6) (y0 - hgt + textW 14 s + 3) (hsb 0 0 8) s
              | otherwise = vertical env (cx + 6) (y0 - hgt - 4) (hsb 0 0 75) s
            axis = if i `mod` 5 == 0 then txtC env cx (y0 + 20) 14 (hsb 0 0 55) (show i) else Blank
        in Pictures
             [ box env (hsb (if fresh then 40 else 190) 70 (if v > 0 then 95 else 25)) x (y0 - hgt) (bw - 2) hgt
             , value, axis ]
      px = 40
      py = 130
      polyLines
        | wPart w =
            [ "Partition numbers p(n): all coins 1, 2, 3, ...  G(x) = product of 1/(1 - x^k)"
            , "Factors done: " ++ show (wStage w) ++ " / " ++ show (length (wFactors w)) ++ " (k = 1 ... " ++ show (wStage w) ++ ")"
            , "p(" ++ show n ++ ") = " ++ show (wPoly w U.! n) ++ "   (p(5)=7, p(10)=42, p(20)=627, p(100)=190,569,292)" ]
        | otherwise =
            let fs = [ (if i < wStage w then "[" else "") ++ "1/(1-x^" ++ show c ++ ")" ++ (if i < wStage w then "]" else "")
                     | (i, c) <- zip [0 ..] (wFactors w) ]
            in [ "Coins: " ++ (if null (wFactors w) then "(none: choose with 1-6)" else intercalate "," (map show (wFactors w)))
                 ++ "   G(x) = " ++ (if null fs then "1" else unwords fs)
               , "Factors done: " ++ show (wStage w) ++ " / " ++ show (length (wFactors w)) ++ "   [ ] = already multiplied"
               , "Ways to make " ++ show n ++ ": " ++ show (wPoly w U.! n) ++ " (just read the coefficient!)" ]
      coinsOn = unwords [(if o then "*" else "o") ++ show c | (c, o) <- zip coinKinds (wOn w)]
  in Pictures
       [ Pictures (map bar (zip [0 :: Int ..] (U.toList (wPoly w))))
       , Color (hsb 0 0 40) (Line [toG env x0 y0, toG env (x0 + fromIntegral (n + 1) * bw) y0])
       , Pictures [txt env px (py + fromIntegral i * 24) 15 (hsb 0 0 100) s | (i, s) <- zip [0 :: Int ..] polyLines]
       , if wStage w > 0
           then txt env px (py + 76) 15 (hsb 40 70 95)
                  ("Last: times (1 + x^" ++ show lastF ++ " + x^" ++ show (2 * lastF)
                   ++ " + ...): c[n] += c[n-" ++ show lastF ++ "] (yellow bars changed)")
           else Blank
       , txt env 20 40 18 (hsb 0 0 100)
           ((if wPart w then "Partition mode (P: back)" else "Coins (1-6): " ++ coinsOn)
            ++ "   N = " ++ show n ++ " (Up/Down)   Space: replay")
       , txt env 20 68 14 (hsb 0 0 60)
           "Bar height is logarithmic. Each factor lets you use one more coin, so the number of ways grows."
       , txt env x0 (y0 + 42) 14 (hsb 0 0 55) "amount n ->"
       ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '6' ->
        let k = fromEnum c - fromEnum '1'
        in rebuild w { wOn = [if i == k then not o else o | (i, o) <- zip [0 ..] (wOn w)], wPart = False }
    | c `elem` "pP" -> rebuild w { wPart = not (wPart w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> rebuild w
  EventKey (SpecialKey KeyUp) Down _ _ | wMax w < 80 -> rebuild w { wMax = wMax w + 10 }
  EventKey (SpecialKey KeyDown) Down _ _ | wMax w > 10 -> rebuild w { wMax = wMax w - 10 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1 円と 2 円だけなら、n 円のつくり方は floor(n/2)+1。グラフで確かめよう
-- 2. 1, 5, 10 円で 30 円のつくり方は？ 手で数えてから係数と比べよう
-- 3. 「各コインは 1 回まで」にするには因子を (1 + x^c) に変える：multiplyFactor を i を大きい方から回す形に
-- 4. 分割数 p(n) は「奇数だけの分割」と「相異なる数の分割」が同じ数（オイラー）。因子を変えて確かめよう

-- ---------- 道具 ----------
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

-- 文字の幅のおおよその見積もり
textW :: Float -> String -> Float
textW size s = fromIntegral (length s) * size * 0.85

-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- 左上が (x, y) の長方形
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 左下が (x, y) の文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 横は中央ぞろえ、y は文字の下
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = txt env (x - textW size s / 2) y size c s

-- 下から上へ読む文字（(x, y) が書き出しの位置）
vertical :: Env -> Float -> Float -> Color -> String -> Picture
vertical env x y c s = at (toG env x y) (Rotate (-90) (label 0 0 14 c s))
