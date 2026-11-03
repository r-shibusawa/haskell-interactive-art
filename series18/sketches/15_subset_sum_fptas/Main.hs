-- ============================================
-- 15 部分和の近似スキーム（FPTAS）
-- 厳密に解く：「作れる合計のリスト」に数を 1 つずつ足して合流させる。リストは最大 2^n 個にふくらむ
-- 近似で解く：毎回リストを「間引く」。値が (1+d) 倍以内の仲間は 1 つにまとめる
-- 誤差 e を決めると d = e / 2n。答えは最適の (1 − e) 倍以上、時間は n と 1/e の多項式 → 完全多項式時間近似スキーム
-- 上下キー：e を変える / スペース：1 段ずつ進める・自動 / R：新しい数 / 1〜3：お手本
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM)
import Data.List (intercalate)
import Numeric (showFFloat)

nNums :: Int
nNums = 14          -- ★ 数の個数

-- 1 段分のリストの要約。画面の 1 ピクセル幅に何個もあるので、続いた範囲（0〜1 の割合）にまとめておく
data Row = Row
  { rLenE  :: !Int
  , rLenA  :: !Int
  , rMaxE  :: !Int
  , rMaxA  :: !Int
  , rRunsE :: [(Float, Float)]
  , rRunsA :: [(Float, Float)]
  }

data World = World
  { wNums  :: [Int]
  , wT     :: !Int
  , wEps   :: !Double    -- ★ 許す誤差（p5.js の ε）
  , wRows  :: [Row]
  , wShown :: !Int
  , wAuto  :: !Bool
  }

main :: IO ()
main = runSketch (sketch "18-15 Subset Sum FPTAS" (preset1 (World [] 0 0.2 [] 0 False)))
  { skBack  = hsb 20 30 9
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

-- 小さな例（e = 0.4 で 389 が出る。最適は 396）
preset1 :: World -> World
preset1 w = recompute w { wNums = [88, 152, 95, 149, 207], wT = 400, wEps = 0.4 }

preset2 :: World -> IO World
preset2 w = do
  ns <- forM [1 .. nNums] (const (randomRIO (100, 999)))
  pure (recompute w { wNums = ns, wT = floor (fromIntegral (sum ns) * 0.45 :: Double) })

preset3 :: World -> IO World
preset3 w = do
  ns <- forM [0 .. nNums - 1] (\i -> ((2 ^ i) +) <$> randomRIO (0, 2))
  pure (recompute w { wNums = ns, wT = floor (fromIntegral (sum ns) * 0.6 :: Double) })

-- 小さい順の 2 つのリストを、同じ値を 1 つにして合流させる
mergeLists :: [Int] -> [Int] -> [Int]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists xa@(x : xs) ya@(y : ys)
  | x < y = x : mergeLists xs ya
  | x > y = y : mergeLists xa ys
  | otherwise = x : mergeLists xs ys

-- 間引く：前に残した値の (1+d) 倍を超えたものだけ残す
trim :: Double -> [Int] -> [Int]
trim _ [] = []
trim d (l0 : ls) = l0 : go l0 ls
  where
    go _ [] = []
    go lastV (v : vs)
      | fromIntegral v > fromIntegral lastV * (1 + d) = v : go v vs
      | otherwise = go lastV vs

recompute :: World -> World
recompute w =
  let nums = wNums w
      t = wT w
      d = wEps w / (2 * fromIntegral (length nums))
      stepE e num = takeWhile (<= t) (mergeLists e (map (+ num) e))
      stepA a num = takeWhile (<= t) (trim d (mergeLists a (map (+ num) a)))
      listsE = scanl stepE [0] nums
      listsA = scanl stepA [0] nums
      n = length nums
  in w { wRows = zipWith (mkRow t) listsE listsA, wShown = min (wShown w) n }

mkRow :: Int -> [Int] -> [Int] -> Row
mkRow t e a = Row (length e) (length a) (lastOr e) (lastOr a) (runs t e) (runs t a)
  where lastOr xs = foldl (\_ v -> v) 0 xs

-- 値を 1/1000 の幅に丸め、つながった範囲にまとめる（四角の数を減らす）
runs :: Int -> [Int] -> [(Float, Float)]
runs t vs =
  let cell v = floor (1000 * fromIntegral v / fromIntegral (max 1 t) :: Float) :: Int
      go [] = []
      go (c : cs) = grow c c cs
      grow a b (c : cs)
        | c <= b + 2 = grow a c cs
      grow a b cs = (toF a, toF b) : go cs
      toF c = fromIntegral c / 1000
  in go (map cell vs)

stepW :: Env -> Float -> World -> IO World
stepW env _ w
  | wAuto w && envFrame env `mod` 30 == 0 =
      let s = wShown w + 1
          n = length (wNums w)
      in pure $ if s > n then w { wShown = n, wAuto = False } else w { wShown = s }
  | otherwise = pure w

-- 文字を単語の切れ目で折り返す
wrapText :: Int -> String -> [String]
wrapText maxChars = go "" . words
  where
    go cur [] = [cur | not (null cur)]
    go cur (x : xs)
      | null cur = go x xs
      | length cur + 1 + length x > maxChars = cur : go x xs
      | otherwise = go (cur ++ " " ++ x) xs

drawW :: Env -> World -> Picture
drawW env w =
  let nums = wNums w
      n = length nums
      shown = wShown w
      eps = wEps w
      x0 = 150
      x1 = envW env - 230
      y0 = 120
      rh = min 48 ((envH env - 230) / fromIntegral (n + 1))
      span' = x1 - x0
      headerText =
        "Numbers {" ++ intercalate ", " (map show nums) ++ "}  target t = " ++ show (wT w)
          ++ "  e = " ++ fix 2 eps ++ "  d = e/2n = " ++ fix 4 (eps / (2 * fromIntegral (max 1 n)))
      headerLines = wrapText (floor ((envW env - 40) / (15 * 0.8))) headerText
      header = [txt env 20 (68 + fromIntegral i * 22) 15 (hsb 45 85 100) s | (i, s) <- zip [0 :: Int ..] headerLines]
      rowPic (i, r) =
        let y = y0 + fromIntegral (i :: Int) * rh
            name = if i == 0 then "L0" else "+" ++ concatMap show (take 1 (drop (i - 1) nums))
            barE (a, b) = box env (hsba 200 50 100 0.7) (x0 + a * span' - 1) (y + 5) ((b - a) * span' + 2) ((rh - 10) / 2)
            barA (a, b) = box env (hsb 45 90 100) (x0 + a * span' - 1.5) (y + rh / 2) ((b - a) * span' + 3) ((rh - 10) / 2)
        in Pictures
             ( txtR env (x0 - 10) (y + rh / 2) 14 (hsb 0 0 70) name
             : box env (hsb 0 0 15) x0 (y + 3) span' (rh - 6)
             : txtM env (x1 + 10) (y + rh / 2 - 9) 14 (hsb 200 50 100) ("exact " ++ show (rLenE r))
             : txtM env (x1 + 10) (y + rh / 2 + 9) 14 (hsb 45 90 100) ("approx " ++ show (rLenA r))
             : map barE (rRunsE r) ++ map barA (rRunsA r) )
      visible = take (shown + 1) (wRows w)
      yEnd = y0 + fromIntegral (shown + 1) * rh
      bottomPart = case drop n visible of
        (r : _) | shown == n ->
          let y = envH env - 56
              be = rMaxE r
              ba = rMaxA r
              totalE = sum (map rLenE (wRows w))
              totalA = sum (map rLenA (wRows w))
              ratio = fromIntegral ba / fromIntegral (max 1 be) :: Double
          in [ txt env 20 y 18 (hsb 200 50 100) ("Exact max = " ++ show be ++ " (lists total " ++ show totalE ++ ")")
             , txt env 20 (y + 30) 18 (hsb 45 90 100)
                 ("Approx max = " ++ show ba ++ " (lists total " ++ show totalA ++ ")  ratio " ++ fix 4 ratio ++ " >= 1 - e = " ++ fix 2 (1 - eps))
             ]
        _ ->
          [ txt env 20 (envH env - 48) 15 (hsb 0 0 60) "Blue: all reachable sums (exact). Yellow: the trimmed list."
          , txt env 20 (envH env - 24) 15 (hsb 0 0 60) "Near values merge into one; the error grows by at most (1+d) per row."
          ]
  in Pictures
       ( header
      ++ map rowPic (zip [0 ..] visible)
      ++ [ seg env (hsb 0 0 100) 2 (x1, y0) (x1, yEnd)
         , txtC env x1 (yEnd + 14) 14 (hsb 0 0 100) "t"
         , txt env 20 34 14 (hsb 0 0 100) "Up/Down: e (error)   Space: 1 row (then auto)   R: new   1: small   2: random 14   3: near powers of 2"
         ]
      ++ bottomPart )

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (recompute w { wEps = min 0.9 (wEps w + 0.05) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (recompute w { wEps = max 0.01 (wEps w - 0.05) })
  EventKey (SpecialKey KeySpace) Down _ _
    | wShown w >= length (wNums w) -> pure w { wShown = 0, wAuto = False }
    | wShown w > 0 -> pure w { wAuto = not (wAuto w) }
    | otherwise -> pure w { wShown = 1 }
  EventKey (Char c) Down _ _
    | c `elem` "rR2" -> preset2 w
    | c == '1' -> pure (preset1 w)
    | c == '3' -> preset3 w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 小さな例（1）で e = 0.40 にすると答えは 389（最適 396）。e を小さくすると？
-- 2. 3（2 のべき乗に近い数）では厳密リストがほぼ 2 倍ずつふくらむ。近似リストは？
-- 3. e を 0.01 にするとリストは長くなるが、それでも n^2 / e くらいに収まる

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 文字：左端の下が (x, y)
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str

-- 文字：中心が (x, y)
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = labelC (gx env x) (gy env y) s c str

-- 文字：左端の上下中央が (x, y)
txtM :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtM env x y s c str = label (gx env x) (gy env y - s / 2) s c str

-- 文字：右端の上下中央が (x, y)
txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtR env x y s c str =
  label (gx env x - fromIntegral (length str) * s * 0.72) (gy env y - s / 2) s c str

-- ---------- 道具：その他 ----------
-- 小数点以下 d 桁で表示（p5.js の nf）
fix :: RealFloat a => Int -> a -> String
fix d v = showFFloat (Just d) v ""
