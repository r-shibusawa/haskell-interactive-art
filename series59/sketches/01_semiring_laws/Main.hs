-- ============================================
-- 59-01 半環のルール ─ ⊕ と ⊗ は、ふつうの + と × と同じルールをどこまで守る？
-- 交換・結合・分配法則、ゼロ（⊕ の単位元）といち（⊗ の単位元）は 4 つとも成り立つ → 「半環」
--   でも (max, +) には ⊕ の逆元がない（a ⊕ x = ε となる x がない ＝ 引き算ができない）。そのかわり a ⊕ a = a（冪等）
--   表は各ルールを 300 組のランダムな数（ε や ±∞ も入れて）で確かめた結果
--   画面では ⊕ を (+)、⊗ を (x)、ε を eps、+∞ を +inf と書く
-- 操作：1〜4 半環を選ぶ / ← → a・b・c を選ぶ / ↑↓ その値を変える / E その値を「ゼロ」に / スペース 別の乱数で表を作り直す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (foldM, replicateM)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl')
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

-- ★ 半環：名前、⊕、⊗、ゼロ、いち、冪等か
data Semiring = Semiring
  { srName :: String
  , srAdd, srMul :: Double -> Double -> Double
  , srZero, srOne :: !Double
  , srIdem :: !Bool }

semirings :: [Semiring]
semirings =
  [ Semiring "(+, x) ordinary" (+) (*) 0 1 False
  , Semiring "(max, +)" mpAdd mpMul negInf 0 True
  , Semiring "(min, +)" (\a b -> if a <= b then a else b) (\a b -> if a == posInf || b == posInf then posInf else a + b) posInf 0 True
  , Semiring "(max, min)" (\a b -> if a >= b then a else b) (\a b -> if a <= b then a else b) negInf posInf True ]

-- ★ 確かめるルール：(短い名前, 式, 両辺を計算する関数)。逆元のルールは「ある = 1、ない = 0」を両辺にする
data Law = Law { lwShort :: String, lwForm :: String, lwF :: Semiring -> Double -> Double -> Double -> (Double, Double) }

laws :: [Law]
laws =
  [ Law "(+) commutative" "a (+) b = b (+) a" (\s a b _ -> (srAdd s a b, srAdd s b a))
  , Law "(+) associative" "(a(+)b)(+)c = a(+)(b(+)c)" (\s a b c -> (srAdd s (srAdd s a b) c, srAdd s a (srAdd s b c)))
  , Law "(x) commutative" "a (x) b = b (x) a" (\s a b _ -> (srMul s a b, srMul s b a))
  , Law "(x) associative" "(a(x)b)(x)c = a(x)(b(x)c)" (\s a b c -> (srMul s (srMul s a b) c, srMul s a (srMul s b c)))
  , Law "distributive" "a(x)(b(+)c) = a(x)b (+) a(x)c" (\s a b c -> (srMul s a (srAdd s b c), srAdd s (srMul s a b) (srMul s a c)))
  , Law "zero" "a (+) zero = a" (\s a _ _ -> (srAdd s a (srZero s), a))
  , Law "zero absorbs" "a (x) zero = zero" (\s a _ _ -> (srMul s a (srZero s), srZero s))
  , Law "one" "a (x) one = a" (\s a _ _ -> (srMul s a (srOne s), a))
  , Law "idempotent" "a (+) a = a" (\s a _ _ -> (srAdd s a a, a))
  , Law "(+) inverse" "some x: a (+) x = zero" (\s a _ _ -> (b2d (hasAddInverse s a), 1))
  , Law "(x) inverse" "a (x) x = one (a /= zero)" (\s a _ _ -> (b2d (a == srZero s || hasMulInverse s a), 1)) ]
  where b2d t = if t then 1 else 0

data World = World
  { wCur   :: !Int          -- いまの半環
  , wVals  :: [Double]      -- ★ a, b, c
  , wSel   :: !Int          -- 選んでいる数
  , wTable :: [[Int]]       -- 半環 × ルールで、300 組のうち成り立った数
  , wSeed  :: !Word32       -- 表の乱数の種
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "59-01 Semiring laws" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 [3, 5, negInf] 0 (buildTable 1) 1 []

-- 引き算：候補 x を順に試す（ふつうなら x = −a。max・min は x を何にしても a ⊕ x ≥ a なのでゼロにならない）
hasAddInverse :: Semiring -> Double -> Bool
hasAddInverse s a = any (\x -> srAdd s a x == srZero s) [-a, srZero s, srOne s, 0, -1e9, 1e9, negInf, posInf]

hasMulInverse :: Semiring -> Double -> Bool
hasMulInverse s a = any (\x -> let v = srMul s a x in v == srOne s || (not (isInfinite v || isNaN v) && abs (v - srOne s) < 1e-12))
                        [-a, 1 / a, srOne s, srZero s]

-- 整数（小数の誤差を避ける）とゼロ・いち
sample :: Semiring -> R Double
sample s = do
  u <- rnd
  if u < 0.12 then pure (srZero s) else if u < 0.2 then pure (srOne s) else fromIntegral <$> rndInt (-9) 9

lawHolds :: Semiring -> Law -> Double -> Double -> Double -> Bool
lawHolds s law a b c = let (p, q) = lwF law s a b c in p == q

-- 種 seed で、半環ごとに 300 組（a, b, c の順に引く）
buildTable :: Word32 -> [[Int]]
buildTable seed = withSeed seed (mapM one semirings)
  where
    one s = do
      trips <- replicateM 300 ((,,) <$> sample s <*> sample s <*> sample s)
      pure [ length [ () | (a, b, c) <- trips, lawHolds s law a b c ] | law <- laws ]

-- ゼロ（0 でないとき）は zero(...) と表示
fmtS :: Semiring -> Double -> String
fmtS s v | v == srZero s && srZero s /= 0 = "zero(" ++ mpF v ++ ")"
         | v == posInf = "+inf"
         | otherwise = mpFmt 0 v

semi :: Int -> Semiring
semi k = headOr (head' semirings) (drop k semirings)
  where head' xs = case xs of { (x : _) -> x; [] -> Semiring "" mpAdd mpMul negInf 0 True }

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Semiring laws   now: " ++ srName s ++ "   zero = " ++ mpF (srZero s) ++ ", one = " ++ mpF (srOne s)) x0 40 20 (grayC 240)
  , say ("1-4 semiring   <- -> pick a, b, c   Up/Down value   E make it zero   Space new random table (seed " ++ show (wSeed w) ++ ")") x0 66 15 (grayC 170) ]
  -- ---- 表
  ++ [ say (srName sr) (x0 + 470 + fromIntegral si * cw) y0 15 (if si == wCur w then rgbC 255 200 90 else grayC 200) | (si, sr) <- zip [0 :: Int ..] semirings ]
  ++ concat
     [ [ Color (grayC (if odd li then 30 else 36)) (rectTL (x0 - 8) (y - 20) (470 + 4 * cw) rh)
       , say (lwShort law ++ "  " ++ lwForm law) x0 y 15 (grayC 220) ]
       ++ [ say ((if n == 300 then "OK " else "NO ") ++ show n ++ " / 300") (x0 + 470 + fromIntegral si * cw) y 15
                (if n == 300 then rgbC 120 210 150 else if n == 0 then rgbC 255 120 100 else rgbC 255 200 90)
          | (si, row) <- zip [0 :: Int ..] (wTable w), let n = headOr 0 (drop li row) ]
     | (li, law) <- zip [0 :: Int ..] laws, let y = y0 + 30 + fromIntegral li * rh ]
  -- ---- いまの a, b, c で 1 つずつ計算
  ++ [ say ("Now: " ++ joinWith "     " [ (if i == wSel w then "> " else "") ++ nm ++ " = " ++ fmtS s v | (i, nm, v) <- zip3 [0 :: Int ..] ["a", "b", "c"] (wVals w) ]) x0 yy 18 (grayC 240) ]
  ++ [ say (lwShort law ++ ": " ++ fmtS s p ++ (if p == q then " = " else " /= ") ++ fmtS s q)
           (x0 + fromIntegral col * 560) (yy + 28 + fromIntegral row * 22) 15 (if p == q then rgbC 160 220 180 else rgbC 255 140 120)
     | (li, law) <- zip [0 :: Int ..] (take 9 laws)
     , let (p, q) = lwF law s a b c
           col = if li < 5 then 0 else 1 :: Int
           row = if li < 5 then li else li - 5 ]
  ++ [ sayWrap (if srIdem s then "In an idempotent semiring a (+) x >= a (max), so a (+) x never becomes zero -> no subtraction. Adding cannot be undone = waiting time cannot be taken back."
                else "Ordinary numbers have subtraction (-a). But they are not idempotent: " ++ fmtS s a ++ " + " ++ fmtS s a ++ " = " ++ fmtS s (a + a))
               x0 (yy + 28 + 5 * 22) (envW env - 80) 16 (rgbC 255 220 120)
     , drawSelfTest env (wTest w) ]
  where
    s = semi (wCur w)
    x0 = 40
    y0 = 104
    cw = min 190 ((envW env - 520) / 4)
    rh = 28
    yy = y0 + 60 + fromIntegral (length laws) * rh
    (a, b, c) = case wVals w of { [p, q, r] -> (p, q, r); _ -> (0, 0, 0) }

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  (x : rest) -> x ++ concatMap (sep ++) rest

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char ch) Down _ _
    | ch `elem` "1234" -> w { wCur = fromEnum ch - fromEnum '1' }
    | ch `elem` "eE" -> setV (srZero s)
    | otherwise -> w
  EventKey (SpecialKey KeySpace) Down _ _ -> let sd = wSeed w + 1 in w { wSeed = sd, wTable = buildTable sd }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSel = (wSel w + 2) `mod` 3 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSel = (wSel w + 1) `mod` 3 }
  EventKey (SpecialKey KeyUp) Down _ _ -> setV (if isInfinite v then 0 else min 9 (v + 1))
  EventKey (SpecialKey KeyDown) Down _ _ -> setV (if isInfinite v then 0 else max (-9) (v - 1))
  _ -> w
  where
    s = semi (wCur w)
    v = headOr 0 (drop (wSel w) (wVals w))
    setV x = w { wVals = [ if i == wSel w then x else y | (i, y) <- zip [0 ..] (wVals w) ] }

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種）
selfTest :: [(String, Bool)]
selfTest =
  [ ("(max, +): commutative, associative, distributive, zero, absorbing, one, idempotent hold for all 500 triples (with eps)", first9 1)
  , ("(min, +) and (max, min) satisfy the same 9 laws (with +inf)", first9 2 && first9 3)
  , ("(+, x) is not idempotent (some a + a /= a) but the other 8 laws hold", count 0 8 3 < 500 && all (\li -> count 0 li 11 == 500) [0 .. 7])
  , ("(max, +) has no (+) inverse: for a /= eps no x gives a (+) x = eps", not (any (hasAddInverse mp) [-9, -1, 0, 4, 9]) && hasAddInverse (semi 0) 4)
  , ("(max, +) can divide (semifield): a (x) (-a) = e = 0", all (\x -> mpMul x (-x) == 0) [-9, -1, 0, 4, 9])
  , ("no NaN in any semiring for combinations of zero, one and numbers", ok6) ]
  where
    mp = semi 1
    count si li seed = let s = semi si
                           law = headOr (Law "" "" (\_ _ _ _ -> (0, 1))) (drop li laws)
                       in withSeed seed (length . filter id <$> replicateM 500 (lawHolds s law <$> sample s <*> sample s <*> sample s))
    first9 si = all (\li -> count si li (fromIntegral (7 + li)) == 500) [0 .. 8]
    ok6 = and [ noNaN [srAdd s a b, srMul s a b] | s <- semirings, a <- [srZero s, srOne s, -3, 5], b <- [srZero s, srOne s, 2] ]

-- ---------- やってみよう ----------
-- 1. 2 を押して (max, +) にし、a = 3, b = 3 で冪等 a ⊕ a = a を確かめよう。1 のふつうの数ではどうなる？
-- 2. E で a をゼロ（ε）にして、「ゼロは吸収」a ⊗ ε = ε を確かめよう。待ち合わせで言うと？
-- 3. (max, min) の「いち」は +∞。a ⊗ +∞ = min(a, +∞) = a になるのはなぜ？（道の「太さ」の計算に使う）
-- 4. 引き算がないのに、なぜ方程式が解ける？ → 05 の「残余」（いちばん大きい劣解）
-- 5. laws に「a ⊕ (b ⊗ c) = (a ⊕ b) ⊗ (a ⊕ c)」を足してみよう（Law "..." "..." (\s a b c -> (..., ...))）。どの半環で成り立つ？

-- ==================================================
-- ここから下は「マックスプラスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。ε = −∞ は Double の −Infinity で表し、
-- ⊗ は必ず mpMul を使う（ε があれば ε。−∞ + ∞ = NaN を作らない）
-- ==================================================
-- ---- マックスプラスの数：ε = −∞（「ゼロ」= 起きない・制約なし）、e = 0（「いち」= 待ち時間なし）
negInf, posInf :: Double
negInf = -1 / 0
posInf = 1 / 0

isEps :: Double -> Bool
isEps v = v == negInf

mpAdd :: Double -> Double -> Double          -- a ⊕ b = max(a, b)
mpAdd a b = if a >= b then a else b

mpMul :: Double -> Double -> Double          -- a ⊗ b = a + b（ε があれば ε）
mpMul a b = if a == negInf || b == negInf then negInf else a + b

-- ε を「eps」と表示（−Infinity を見せない）。d = 小数点以下の桁（toFixed のあと余分な 0 を消す）
mpFmt :: Int -> Double -> String
mpFmt d v
  | v == negInf = "eps"
  | v == posInf = "+inf"
  | isNaN v = "?"
  | otherwise = let s = stripZeros (showFFloat (Just d) v "")
                in if s == "-0" then "0" else s
  where stripZeros s | '.' `elem` s = let t = reverse (dropWhile (== '0') (reverse s))
                                      in if last' t == '.' then init t else t
                     | otherwise = s
        last' t = if null t then ' ' else last t

-- 小数 1 桁（p5 の mpFmt(v) と同じ）
mpF :: Double -> String
mpF = mpFmt 1
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> let (a, s1) = g s in runR (k a) s1)

-- 種 s で始めて結果だけ取り出す（seedRng(s) してから計算）
withSeed :: Word32 -> R a -> a
withSeed s m = fst (runR m (seedRng s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 maxW で折り返す文字（左上 = (x, y)）
sayWrap :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayWrap s x y maxW size c = Pictures [ say l x (y + size + fromIntegral k * size * 1.3) size c | (k, l) <- zip [0 :: Int ..] (wrapText maxW size s) ]

wrapText :: Float -> Float -> String -> [String]
wrapText maxW size s = go (words s) ""
  where
    fits t = textW t size <= maxW
    go [] cur = [cur | not (null cur)]
    go (w : ws) "" = go ws w
    go (w : ws) cur = let t = cur ++ " " ++ w in if fits t then go ws t else cur : go (w : ws) ""

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine
-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 有効数字 n 桁に丸めた数（toPrecision）
roundSig :: Int -> Double -> Double
roundSig n v0'
  | v == 0 = 0
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (n - 1 - e) :: Double
                in fromIntegral (round (v * k) :: Integer) / k
  where v = fin 0 v0'

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v = let r = roundSig 3 v
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (jsRoundI v)

jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv (-1e9) 1e9 v)

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d
-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

noNaN :: [Double] -> Bool
noNaN = not . any isNaN

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 210 150 else rgbC 255 90 90)
