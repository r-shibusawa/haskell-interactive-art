-- ============================================
-- 41-07 包除原理
-- |A ∪ B ∪ C| = |A| + |B| + |C| − |A∩B| − |B∩C| − |A∩C| + |A∩B∩C|
-- 重なりを「足しすぎては引き、引きすぎては足す」。点を数えて確かめる
-- 操作：3 / 4 = 集合の数 / ドラッグ = 円を動かす / スペース = 項を 1 つずつ足し引き / A = 自動 / D = 攪乱順列 D_n の表
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Bits (popCount, shiftL, testBit, (.&.), (.|.))
import Data.List (foldl', intercalate, sortOn)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

pointCount :: Int
pointCount = 500           -- ★ 点の数

data Circ = Circ { cX, cY, cR, cHue :: !Float }

data World = World
  { wPts      :: !(U.Vector (Float, Float))   -- 散らばった点
  , wCircles  :: ![Circ]
  , wK        :: !Int                         -- ★ 集合の数（3 か 4）
  , wTerm     :: !Int                         -- 何項目まで足したか
  , wAuto     :: !Bool
  , wTick     :: !Float
  , wDragging :: !Int
  , wDerange  :: !Bool
  , wLast     :: !Point                       -- 前フレームのマウス位置
  }

data Term = Term { tMask, tSize, tSign, tCount :: !Int }

world0 :: World
world0 = World U.empty [] 3 0 True 0 (-1) False (0, 0)

main :: IO ()
main = runSketch (sketch "41-07 Inclusion-exclusion" world0)
  { skBack  = hsb 222 28 11
  , skStart = start
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

start :: Env -> IO World
start env = do
  ps <- replicateM pointCount $ do
    x <- randomRIO (0, 1)
    y <- randomRIO (0, 1)
    pure (40 + x * (envW env * 0.6), 110 + y * (envH env - 150))
  pure (rebuild env world0 { wPts = U.fromList ps })

rebuild :: Env -> World -> World
rebuild env w =
  let k = wK w
      cx = envW env * 0.33
      cy = envH env * 0.55
      big = min (envW env) (envH env) * 0.14
      one i =
        let a = fromIntegral i * tau / fromIntegral k - pi / 2
        in Circ (cx + cos a * big * 0.9) (cy + sin a * big * 0.9) (big * 1.15) (fromIntegral i * 360 / fromIntegral k + 20)
  in w { wCircles = map one [0 .. k - 1 :: Int], wTerm = 0 }

-- 点 p がどの円に入っているか（ビットマスク）
membership :: [Circ] -> (Float, Float) -> Int
membership cs (x, y) = foldl' add 0 (zip [0 ..] cs)
  where
    add m (i, c)
      | (x - cX c) * (x - cX c) + (y - cY c) * (y - cY c) < cR c * cR c = m .|. (1 `shiftL` i)
      | otherwise = m

memberships :: World -> U.Vector Int
memberships w = U.map (membership (wCircles w)) (wPts w)

-- ★ 包除原理の各項：部分集合 S（サイズ順）ごとに (−1)^(|S|−1) × |∩S|
terms :: Int -> U.Vector Int -> [Term]
terms k mem =
  [ Term s (popCount s) (if odd (popCount s) then 1 else -1) (U.length (U.filter (\m -> m .&. s == s) mem))
  | s <- sortOn (\a -> (popCount a, a)) [1 .. (1 `shiftL` k) - 1 :: Int] ]

termCount :: Int -> Int
termCount k = (1 `shiftL` k) - 1

maskName :: Int -> Int -> String
maskName k s = intercalate "n" [[c] | (i, c) <- zip [0 .. k - 1] "ABCD", testBit s i]

step :: Env -> Float -> World -> IO World
step _ dt w
  | not (wAuto w) = pure w
  | wTick w + dt > 0.7 = pure w { wTick = 0, wTerm = (wTerm w + 1) `mod` (termCount (wK w) + 1) }
  | otherwise = pure w { wTick = wTick w + dt }

draw :: Env -> World -> Picture
draw env w =
  let k = wK w
      mem = memberships w
      ts = terms k mem
      union = U.length (U.filter (/= 0) mem)
      circlePic (i, c) =
        Pictures
          [ at (toG env (cX c) (cY c)) (Color (hsba (cHue c) 70 90 0.18) (disc (cR c)))
          , at (toG env (cX c) (cY c)) (Color (hsb (cHue c) 70 95) (ring (cR c) 2))
          , txt env (cX c - 8) (cY c - cR c + 26) 20 (hsb (cHue c) 70 100) [pickChar i] ]
      pointPic (x, y) m
        | m /= 0 = at (toG env x y) (Color (hsb 50 80 100) (disc 2.5))
        | otherwise = at (toG env x y) (Color (hsb 0 0 40) (disc 1.5))
      px = envW env * 0.64
      py = 120
      -- 項を 1 行ずつ。合計と y 座標を持ちながら並べる
      rows = go 0 0 (py + 66) ts
      go :: Int -> Int -> Float -> [Term] -> ([Picture], Int, Float)
      go _ total y [] = ([], total, y)
      go i total y (t : rest) =
        let active = i < wTerm w
            total1 = if active then total + tSign t * tCount t else total
            c | active = hsb (if tSign t > 0 then 130 else 10) 70 95
              | otherwise = hsb 0 0 45
            line1 = txt env px y 16 c
                      ((if tSign t > 0 then "+ " else "- ") ++ "|" ++ maskName k (tMask t) ++ "| = " ++ show (tCount t))
            line2 = if active then txt env (px + 200) y 16 c ("-> " ++ show total1) else Blank
            line3 = if i == wTerm w - 1 then txt env (px + 300) y 16 (hsb 50 80 100) "<" else Blank
            gapNext = case rest of
              (t2 : _) | tSize t2 /= tSize t -> 8
              _ -> 0
            (ps, tot, yEnd) = go (i + 1) total1 (y + 22 + gapNext) rest
        in (line1 : line2 : line3 : ps, tot, yEnd)
      (rowPics, total, yEnd) = rows
      nterm = termCount k
      note | wTerm w == 0 = ""
           | wTerm w <= k = "Adding singles counts overlaps twice"
           | wTerm w <= k + k * (k - 1) `div` 2 = "Subtract pairs -> triples removed too often"
           | otherwise = "Add triples back (with 4 sets, subtract again)"
  in Pictures
       [ Pictures (map circlePic (zip [0 :: Int ..] (wCircles w)))
       , Pictures (zipWith pointPic (U.toList (wPts w)) (U.toList mem))
       , box env (hsba 0 0 100 0.08) (px - 16) (py - 30) (envW env - px + 6) (envH env - py - 30)
       , txt env px py 22 (hsb 0 0 100)
           ("|A u B u C" ++ (if k == 4 then " u D" else "") ++ "|: count it")
       , txt env px (py + 28) 15 (hsb 0 0 70) ("Union points counted directly: " ++ show union)
       , Pictures rowPics
       , txt env px (yEnd + 16) 18 (hsb 0 0 100)
           ("Total: " ++ show total ++
            (if wTerm w == nterm
               then "  = direct " ++ show union ++ (if total == union then " OK" else " NG")
               else "  (not finished)"))
       , if null note then Blank else txt env px (yEnd + 40) 14 (hsb 0 0 60) note
       , if wDerange w then drawDerangement env px (yEnd + 64) else Blank
       , txt env 20 40 16 (hsb 0 0 100)
           ("Sets " ++ show k ++ " (3 / 4)   Drag: move a circle   Space: next term   A: auto "
            ++ onOff (wAuto w) ++ "   D: derangements " ++ onOff (wDerange w))
       , txt env 20 68 14 (hsb 0 0 60)
           "Yellow = inside some circle (the union). Fixing over- and under-counting in turn matches exactly at the end."
       ]

-- 攪乱順列：D_n = n! Σ_{i=0}^{n} (−1)^i / i!　（包除原理：「i 番が固定」の集合の和集合を n! から引く）
drawDerangement :: Env -> Float -> Float -> Picture
drawDerangement env px py0 =
  let py = min py0 (envH env - 200)
      ds = derangements
      fs = scanl (*) 1 [1 .. 8 :: Int]
      ratio i = showFFloat (Just 3) (fromIntegral (at' ds i) / fromIntegral (at' fs i) :: Float) ""
      cell i = show i ++ ": " ++ show (at' ds i) ++ "  " ++ ratio i
      row i = cell i ++ "    " ++ cell (i + 4)
  in Pictures
       ( txt env px py 15 (hsb 0 0 100) "Derangements D_n: nobody in their own seat"
       : txt env px (py + 20) 14 (hsb 0 0 70) "D_n = n! - |permutations with a fixed point|"
       : [ txt env px (py + 40 + fromIntegral j * 18) 14 (hsb 0 0 70) (row (j + 1)) | j <- [0 .. 3 :: Int] ]
      ++ [ txt env px (py + 118) 14 (hsb 0 0 70) "n: D_n  D_n/n!. D_n = n!(1 - 1/1! + 1/2! - ...)"
         , txt env px (py + 138) 14 (hsb 0 0 70) "-> 1/e = 0.368 (see sketch 12)" ] )

derangements :: [Int]
derangements = 1 : 0 : zipWith3 (\i a b -> i * (a + b)) [1 ..] (drop 1 derangements) derangements

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c == '3' && wK w /= 3 -> rebuild env w { wK = 3 }
    | c == '4' && wK w /= 4 -> rebuild env w { wK = 4 }
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "dD" -> w { wDerange = not (wDerange w) }
  EventKey (SpecialKey KeySpace) Down _ _ ->
    w { wAuto = False, wTerm = (wTerm w + 1) `mod` (termCount (wK w) + 1) }
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
        hits = [i | (i, c) <- zip [0 ..] (wCircles w), (cX c - mx) * (cX c - mx) + (cY c - my) * (cY c - my) < cR c * cR c]
    in w { wLast = (mx, my), wDragging = case reverse hits of { (i : _) -> i; [] -> -1 } }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDragging = -1 }
  EventMotion _ ->
    let (mx, my) = mouseP env
        (lx, ly) = wLast w
        move (i, c) = if i == wDragging w then c { cX = cX c + mx - lx, cY = cY c + my - ly } else c
    in w { wLast = (mx, my), wCircles = map move (zip [0 ..] (wCircles w)) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 円を全部離すと、2 個以上の重なりの項がすべて 0 になる。重ねるほど項が効いてくる
-- 2. 3 つの円を完全に重ねると |A∪B∪C| = |A|。式の右辺がどう打ち消し合うか追ってみよう
-- 3. 「1〜100 のうち 2 でも 3 でも 5 でも割り切れない数」は包除原理で数えられる。点を整数に置きかえて数えよう
-- 4. 攪乱順列 D_n/n! が 1/e に近づく理由：e^x のテイラー展開 Σ x^n/n! に x = −1 を入れた形

-- ---------- 道具 ----------
onOff :: Bool -> String
onOff b = if b then "ON" else "off"

pickChar :: Int -> Char
pickChar i = case drop i "ABCD" of
  (c : _) -> c
  [] -> '?'

-- 範囲外なら 0 を返す取り出し
at' :: [Int] -> Int -> Int
at' xs i = case drop i xs of
  (x : _) -> x
  [] -> 0

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
