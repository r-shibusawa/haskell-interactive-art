-- ============================================
-- 41-03 パスカルの三角形
-- 両端は 1、あとは「左上 ＋ 右上」。並ぶ数は二項係数 C(n,k)
-- m で割った余りで色を塗ると、シェルピンスキーのギャスケットのような模様が浮かび上がる
-- 操作：↑↓ = 割る数 m（2〜9）/ ←→ = 行数 / H = 性質のハイライトを順に切り替え / N = 数字の表示
-- ============================================
module Main (main) where

import Art
import Data.Array
import Data.List (intercalate)

startRows, startM :: Int
startRows = 24         -- ★ 行数（8〜48）
startM = 2             -- ★ 割る数（余りで色分け）

hlNames :: [String]
hlNames =
  [ "none", "diagonal k=2: triangular numbers 1,3,6,10,..."
  , "shallow diagonals: Fibonacci 1,1,2,3,5,...", "row sum = 2^n"
  , "hockey stick: a diagonal adds up to the next number" ]

data World = World
  { wRows, wM :: !Int
  , wTri      :: !(Array (Int, Int) Int)   -- C(n,k) mod m
  , wReal     :: !(Array (Int, Int) Int)   -- 実際の値
  , wHl       :: !Int                      -- ハイライトの種類 0〜4
  , wShowNum  :: !Bool
  , wFibIdx   :: !Int                      -- フィボナッチ・ホッケーのアニメ用
  , wLayer    :: Picture                   -- 三角形の絵（作り直すまで使い回す）
  }

main :: IO ()
main = runSketch (sketch "41-03 Pascal's triangle" world0)
  { skBack  = hsb 225 30 10
  , skStart = \env -> pure (rebuild env world0)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World startRows startM (table startRows startM) (table startRows 0) 0 True 0 Blank

-- ★ 余りだけ持てば大きな数にならない：(a + b) mod m（md = 0 なら実際の値）
table :: Int -> Int -> Array (Int, Int) Int
table rows md = t
  where
    reduce v = if md == 0 then v else v `mod` md
    t = array ((0, 0), (rows - 1, rows - 1))
          [ ((r, k), if k == 0 || k == r then reduce 1 else reduce (t ! (r - 1, k - 1) + t ! (r - 1, k)))
          | r <- [0 .. rows - 1], k <- [0 .. r] ]

rebuild :: Env -> World -> World
rebuild env w =
  let w1 = w { wTri = table (wRows w) (wM w), wReal = table (wRows w) 0 }
  in w1 { wLayer = drawTriangle env w1 }

cellPos :: Env -> World -> Int -> Int -> (Float, Float, Float)
cellPos env w r k =
  let rows = fromIntegral (wRows w)
      s = min ((envW env - 40) / (rows + 1)) ((envH env - 190) / rows)
  in (envW env / 2 + (fromIntegral k - fromIntegral r / 2) * s, 160 + fromIntegral r * s * 0.92, s)

-- 数字が入る幅があるときだけ書く（文字の大きさは 14 より小さくしない）
numberText :: Float -> Int -> String
numberText s val =
  let str = show val
      fits = fromIntegral (length str) * 14.7 <= s * 0.95
  in if fits then str else ".."

drawTriangle :: Env -> World -> Picture
drawTriangle env w =
  let cell (r, k) =
        let (x, y, s) = cellPos env w r k
            v = wTri w ! (r, k)
            c = if v == 0 then hsb 0 0 18
                else hsb (fromIntegral (v - 1) * 360 / fromIntegral (max 1 (wM w - 1)) * 0.8 + 20) 70 95
            num = if wShowNum w && wRows w <= 20
                    then txtM env x y 14 (hsb 0 0 (if v == 0 then 50 else 12)) (numberText s (wReal w ! (r, k)))
                    else Blank
        in Pictures [at (toG env x y) (Color c (disc (s * 0.44))), num]
  in Pictures (map cell [(r, k) | r <- [0 .. wRows w - 1], k <- [0 .. r]])

step :: Env -> Float -> World -> IO World
step env _ w
  | envFrame env `mod` 30 == 0 = pure w { wFibIdx = (wFibIdx w + 1) `mod` min (wRows w) 16 }
  | otherwise = pure w

-- 枠線で囲む
mark :: Env -> World -> Float -> Float -> Int -> Int -> Picture
mark env w sc wd r k =
  let (x, y, s) = cellPos env w r k
  in at (toG env x y) (ring (s * sc / 2) wd)

-- ハイライト：(絵, 説明)
highlight :: Env -> World -> (Picture, String)
highlight env w =
  let rows = wRows w
      fi = wFibIdx w
      real r k = wReal w ! (r, k)
  in case wHl w of
    1 -> let rs = [2 .. rows - 1]
             vals = take 8 [real r 2 | r <- rs]
         in ( Color (hsb 60 90 100) (Pictures [mark env w 1 3 r 2 | r <- rs])
            , "Triangular numbers: " ++ commaList vals ++ " ...  C(n,2) = n(n-1)/2" )
    2 -> let d = fi + 2
             cells = [(d - k, k) | k <- [0 .. d `div` 2], d - k < rows]
             total = sum [real r k | (r, k) <- cells]
         in ( Color (hsb 130 90 100) (Pictures [mark env w 1 3 r k | (r, k) <- cells])
            , "Shallow diagonal " ++ show (d - 1) ++ ": sum = " ++ show total
              ++ "   Fibonacci: " ++ commaList (take 10 fibs) ++ " ..." )
    3 -> let r = min (rows - 1) (fi + 1)   -- 行が足りないときは最後の行にとどめる
             total = sum [real r k | k <- [0 .. r]]
         in ( Color (hsb 300 70 100) (Pictures [mark env w 1 3 r k | k <- [0 .. r]])
            , "Row " ++ show r ++ " sum = " ++ show total ++ " = 2^" ++ show r ++ " (all ways to choose from n)" )
    4 -> let kk = 1 + fi `mod` 3
             len = 4 + fi `mod` 4
             rs = [kk + i | i <- [0 .. len - 1], kk + i < rows]
             total = sum [real r kk | r <- rs]
             r2 = kk + len
         in ( Pictures
                [ Color (hsb 20 90 100) (Pictures [mark env w 1 3 r kk | r <- rs])
                , if r2 < rows then Color (hsb 50 90 100) (mark env w 1.1 5 r2 (kk + 1)) else Blank ]
            , "Top " ++ show len ++ " of the diagonal add to " ++ show total ++ " -> next number C("
              ++ show r2 ++ "," ++ show (kk + 1) ++ ")"
              ++ (if r2 < rows then " = " ++ show (real r2 (kk + 1)) else "") )
    _ -> (Blank, "")

draw :: Env -> World -> Picture
draw env w =
  let (hlPic, msg) = highlight env w
      m = wM w
  in Pictures
       [ wLayer w
       , hlPic
       , txt env 20 34 18 (hsb 0 0 100)
           ("mod " ++ show m ++ " (Up/Down)   rows " ++ show (wRows w) ++ " (Left/Right)   H: highlight   N: numbers "
            ++ (if wShowNum w then "ON" else "off"))
       , txt env 20 60 14 (hsb 0 0 60)
           (if m == 2
              then "mod 2: odd numbers colored = Sierpinski gasket (a fractal). Big triangles close at 2^k rows."
              else "mod " ++ show m ++ ": remainder 0 is dark, others colored. For a prime p, big triangles at p^k rows (Lucas).")
       , txt env 20 84 14 (hsb 0 0 75) ("Highlight: " ++ pick (wHl w) hlNames)
       , if null msg then Blank else txt env 20 108 14 (hsb 55 80 100) msg
       ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ | wM w < 9 -> rebuild env w { wM = wM w + 1 }
  EventKey (SpecialKey KeyDown) Down _ _ | wM w > 2 -> rebuild env w { wM = wM w - 1 }
  EventKey (SpecialKey KeyRight) Down _ _ | wRows w < 48 -> resize (wRows w + 4)
  EventKey (SpecialKey KeyLeft) Down _ _ | wRows w > 8 -> resize (wRows w - 4)
  EventKey (Char c) Down _ _
    | c `elem` "hH" -> w { wHl = (wHl w + 1) `mod` length hlNames }
    | c `elem` "nN" -> let w1 = w { wShowNum = not (wShowNum w) } in w1 { wLayer = drawTriangle env w1 }
  _ -> w
  where
    resize r = rebuild env w { wRows = r, wFibIdx = wFibIdx w `mod` min r 16 }

-- ---------- やってみよう ----------
-- 1. mod 2 で行数を 16 → 32 にすると模様が「入れ子」になる。3 で割ると？ 5 で割ると？
-- 2. H を押して、三角数・フィボナッチ・2^n・ホッケースティックを順に確かめよう
-- 3. 行 n がすべて奇数（mod 2 で全部色つき）になるのはどんな n？（n = 2^k − 1）
-- 4. 2 行上の数との関係（C(n,k) = C(n−2,k−2) + 2C(n−2,k−1) + C(n−2,k)）を描いてみよう

-- ---------- 道具 ----------
fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (drop 1 fibs)

commaList :: [Int] -> String
commaList = intercalate ", " . map show

-- 範囲外でも止まらない取り出し
pick :: Int -> [String] -> String
pick i xs = case drop i xs of
  (x : _) -> x
  [] -> ""

-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 中心が (x, y) の文字（数字の幅に合わせて中央に置く）
txtM :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtM env x y size c s =
  let (gx, gy) = toG env x y
  in label (gx - fromIntegral (length s) * size * 0.5) (gy - size / 2) size c s
