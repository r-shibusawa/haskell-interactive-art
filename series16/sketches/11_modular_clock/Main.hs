-- ============================================
-- 11 mod n の時計：群・部分群・合同式
-- 0 〜 n−1 を時計の文字盤に並べる。足し算・掛け算は n で割った余り
-- 1：かけ算の表（k → a·k mod n の弦）と Zₙ*（n と互いに素な数＝逆数がある数）
-- 2：累乗 1, a, a², … で一周する部分群。長さ = 位数。全部まわれば「原始根」
-- 3：一次合同式 ax ≡ b (mod n) の解。d = gcd(a, n) が b を割れば解が d 個
-- ←→：n を変える / ↑↓：a を変える / 数字をクリック：2 では a、3 では b にする
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt)
import qualified Data.IntSet as IS
import Data.List (intercalate, sort)

data World = World
  { wN    :: !Int   -- ★ 最初の値
  , wA    :: !Int
  , wB    :: !Int
  , wMode :: !Int
  , wAnim :: !Float
  }

main :: IO ()
main = runSketch (sketch "16-11 Modular Clock" (World 19 2 4 2 0))
  { skBack  = hsb 265 45 10
  , skStep  = \_ _ w -> pure w { wAnim = wAnim w + 0.04 }
  , skDraw  = drawW
  , skEvent = onEvent
  }

phi :: Int -> Int
phi m = length [k | k <- [1 .. m], gcd k m == 1]

-- 1, g, g², … を、同じ数が出るまで
powers :: Int -> Int -> [Int]
powers n g = go (1 `mod` n) IS.empty
  where
    go v seen
      | IS.member v seen = []
      | otherwise = v : go (v * g `mod` n) (IS.insert v seen)

-- 拡張ユークリッド：(d, x, y)
extGcd :: Int -> Int -> (Int, Int, Int)
extGcd p q
  | q == 0 = (p, 1, 0)
  | otherwise = let (d, x, y) = extGcd q (p `mod` q) in (d, y, x - (p `div` q) * y)

invMod :: Int -> Int -> Int
invMod n x = let (_, e, _) = extGcd x n in e `mod` n

center :: Env -> (Float, Float, Float)
center env = (envW env * 0.33, envH env * 0.57, min (envW env * 0.27) (envH env * 0.36))

drawW :: Env -> World -> Picture
drawW env w =
  let n = wN w
      a = wA w
      b = wB w
      nf = fromIntegral n :: Float
      (cx, cy, r) = center env
      pAt k = let t = -pi / 2 + tau * fromIntegral (k :: Int) / nf in (cx + cos t * r, cy + sin t * r)
      units = IS.fromList [k | k <- [1 .. n - 1], gcd k n == 1]
      s1 = show
      (extra, hl, msgs) = case wMode w of
        1 ->
          ( [ seg env (hsba (fromIntegral k * 360 / nf) 60 90 0.6) 1.5 (pAt k) (pAt (a * k `mod` n)) | k <- [0 .. n - 1] ]
          , units
          , [ "k -> " ++ s1 a ++ "*k mod " ++ s1 n ++ " chords (the pattern of the multiplication table)"
            , "Z_n* = numbers coprime to n = numbers with an inverse (bright): " ++ s1 (phi n) ++ " of them (Euler phi(" ++ s1 n ++ ") = " ++ s1 (phi n) ++ ")"
            , if gcd a n == 1
                then s1 a ++ " is coprime: chords are 1-to-1 (a permutation). Inverse of " ++ s1 a ++ " is " ++ s1 (invMod n a)
                       ++ " (" ++ s1 a ++ "*" ++ s1 (invMod n a) ++ " = 1)"
                else s1 a ++ " shares factor " ++ s1 (gcd a n) ++ " with " ++ s1 n ++ ": some chords meet at the same point (no inverse)"
            ] )
        2 ->
          let out = powers n a
              m = length out
              shown = min m (floor (wAnim w * 2) `mod` (m + 4))
              pathPts = [pAt (nthI out (i `mod` m)) | i <- [0 .. shown], m > 0]
              isPrim = gcd a n == 1 && m == phi n
              prims = [g | g <- [1 .. n - 1], gcd g n == 1, length (powers n g) == phi n]
              firstV = case out of
                (v : _) -> v
                [] -> 1
          in ( [Color (hsb 45 90 100) (thickPath 3 (map (pt env) pathPts))]
             , IS.fromList out
             , [ s1 a ++ " powers: " ++ intercalate " -> " (map s1 (take 16 out)) ++ (if m > 16 then " ..." else "") ++ " -> " ++ s1 firstV
               , if gcd a n /= 1
                   then s1 a ++ " and " ++ s1 n ++ " are not coprime, so it never returns to 1 (outside Z_n*)"
                   else "Order ord(" ++ s1 a ++ ") = " ++ s1 m ++ ". Subgroup size " ++ s1 m ++ " divides |Z_n*| = " ++ s1 (phi n) ++ " (Lagrange's theorem)"
               , if isPrim then "* " ++ s1 a ++ " is a primitive root! Its powers visit all of Z_n* (Z_n* is cyclic)" else "Not a primitive root."
               , "Primitive roots mod " ++ s1 n ++ ": " ++ (if null prims then "none" else intercalate ", " (map s1 prims)) ++ " (a prime p always has them)"
               , if gcd a n == 1 then "Euler: " ++ s1 a ++ "^phi(n) = 1. If n is prime, Fermat's little theorem: " ++ s1 a ++ "^(n-1) = 1" else ""
               ] )
        _ ->
          let d = gcd a n
              chords = [ let on = a * x `mod` n == b `mod` n
                             (x1, y1) = pAt x
                             (x2, y2) = pAt (a * x `mod` n)
                         in seg env (if on then hsb 45 90 100 else hsba 0 0 50 0.4) (if on then 3 else 1)
                              (x1, y1) (lerpF x1 x2 0.97, lerpF y1 y2 0.97)
                       | x <- [0 .. n - 1] ]
              sols0 = [x | x <- [0 .. n - 1], a * x `mod` n == b `mod` n]
              (_, ex, ey) = extGcd a n
              x0 = (ex * (b `div` d)) `mod` n
              sols = sort [(x0 + i * (n `div` d)) `mod` n | i <- [0 .. d - 1]]
              base = [ s1 a ++ "x = " ++ s1 b ++ " (mod " ++ s1 n ++ "). Arrows x -> " ++ s1 a ++ "x mod " ++ s1 n ++ " that reach " ++ s1 b ++ " (green ring) are solutions"
                     , "d = gcd(" ++ s1 a ++ ", " ++ s1 n ++ ") = " ++ s1 d ]
              more
                | b `mod` d == 0 =
                    [ "d divides " ++ s1 b ++ ": exactly " ++ s1 d ++ " solutions: x = " ++ intercalate ", " (map s1 sols)
                    , "How: extended Euclid gives " ++ s1 a ++ "*(" ++ s1 ex ++ ") + " ++ s1 n ++ "*(" ++ s1 ey ++ ") = " ++ s1 d
                        ++ ". x0 = " ++ s1 ex ++ "*(" ++ s1 b ++ "/" ++ s1 d ++ ") mod " ++ s1 n ++ ", then every n/d = " ++ s1 (n `div` d) ]
                | otherwise = ["d = " ++ s1 d ++ " does not divide " ++ s1 b ++ ": no solution (" ++ s1 a ++ "x is always a multiple of " ++ s1 d ++ ")"]
          in (chords, IS.fromList sols0, base ++ more)
      numR = max 14 (min 22 (r * 2.6 / nf))
      numbers = [ let (x, y) = pAt k
                      on = IS.member k hl
                      fillC | on = hsb (fromIntegral k * 360 / nf) 70 95
                            | IS.member k units = hsb 0 0 45
                            | otherwise = hsb 0 0 22
                  in Pictures [ dot env fillC x y (numR * 2)
                              , txtC env x y (max 14 (numR * 0.9)) (if on then hsb 0 0 8 else hsb 0 0 90) (show k) ]
                | k <- [0 .. n - 1] ]
      px = envW env * 0.6
      pw = envW env - px - 30
  in Pictures
       [ txt env 30 50 26 (hsb 0 0 92) ("The mod " ++ show n ++ " clock")
       , txt env 30 80 15 (hsb 0 0 60) ("1: mult table  2: powers, subgroup  3: congruence  L/R: n = " ++ show n ++ "  Up/Down: a = " ++ show a
                                         ++ "  Click number: pick " ++ (if wMode w == 3 then "b" else "a"))
       , hoop env (hsb 0 0 25) 1.5 cx cy (r * 2)
       , Pictures extra
       , Pictures numbers
       , if wMode w == 3 then uncurry (\bx by -> hoop env (hsb 150 70 95) 4 bx by (numR * 2 + 10)) (pAt b) else Blank
       , stack env px 120 pw [(16, hsb 0 0 88, 12, m) | m <- msgs, not (null m)]
       , footer env px pw 14 (hsb 0 0 55)
           "If n is prime, everything except 0 is in Z_n* (you can divide). If composite, numbers sharing a factor have no inverse. RSA (13): only someone who knows phi(n) can compute the inverse."
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (cx, cy, r) = center env
        n = wN w
        m = mouseP env
        posOf k = let t = -pi / 2 + tau * fromIntegral k / fromIntegral n in (cx + cos t * r, cy + sin t * r)
        hits = [k | k <- [0 .. n - 1], dist2 m (posOf k) < 20]
        pick w' k = if wMode w' == 3 then w' { wB = k } else w' { wA = k, wAnim = 0 }
    in foldl pick w hits
  EventKey (Char c) Down _ _
    | c `elem` "123" -> w { wMode = digitToInt c, wAnim = 0 }
  EventKey (SpecialKey KeyRight) Down _ _ -> setN (min 60 (wN w + 1)) w
  EventKey (SpecialKey KeyLeft) Down _ _ -> setN (max 2 (wN w - 1)) w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wA = (wA w + 1) `mod` wN w, wAnim = 0 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wA = (wA w - 1 + wN w) `mod` wN w, wAnim = 0 }
  _ -> w

setN :: Int -> World -> World
setN n w = w { wN = n, wA = wA w `mod` n, wB = wB w `mod` n }

-- ---------- やってみよう ----------
-- 1. 2 で n = 19（素数）の原始根を全部見つける。いくつある？（φ(18) = 6 個）
-- 2. 1 で n = 200 にしたいときは setN の上限と World の n を書きかえる。a = 2, 3, … で模様（カーディオイドなど）
-- 3. 3 で n = 12, a = 8 にして、b をいろいろ変える。解がある b はどんな数？

-- ---------- 道具 ----------
nthI :: [Int] -> Int -> Int
nthI xs i = case drop i xs of
  (x : _) -> x
  [] -> 0

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> (Float, Float) -> (Float, Float)
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> (Float, Float) -> (Float, Float) -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 長方形の枠線（太さ t）
frame :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
frame env c t x y w h = Pictures
  [ seg env c t (x, y) (x + w, y), seg env c t (x + w, y) (x + w, y + h)
  , seg env c t (x + w, y + h) (x, y + h), seg env c t (x, y + h) (x, y) ]

-- 直径 d の円（p5.js の circle）
dot :: Env -> Color -> Float -> Float -> Float -> Picture
dot env c x y d = Color c (at (pt env (x, y)) (disc (d / 2)))

-- 直径 d の円の輪郭（太さ t）
hoop :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
hoop env c t x y d = Color c (at (pt env (x, y)) (ring (d / 2) t))

-- 文字：txt は左下が (x, y)、txtT は左上が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtT, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtT env x y s c str = txt env x (y + s) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = txt env (x - fromIntegral (length str) * s * 0.78) y s c str

-- ---------- 道具：文字の折り返し（文字の幅はおよそ 0.85 × 大きさ） ----------
wrapText :: Float -> Float -> String -> [String]
wrapText s w = concatMap one . lines
  where
    limit = max 8 (floor (w / (s * 0.85))) :: Int
    one l = case words l of
      [] -> [""]
      (x : xs) -> go x xs
    go cur [] = [cur]
    go cur (x : xs)
      | length cur + 1 + length x <= limit = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs

lineH :: Float -> Float
lineH s = s * 1.35

-- 左上 (x, y) から幅 w で折り返して書く
para :: Env -> Float -> Float -> Float -> Color -> Float -> String -> Picture
para env x y s c w str = Pictures
  [ txtT env x (y + fromIntegral i * lineH s) s c l | (i, l) <- zip [0 :: Int ..] (wrapText s w str) ]

-- 折り返したときの高さ
paraH :: Float -> Float -> String -> Float
paraH s w str = fromIntegral (length (wrapText s w str)) * lineH s

-- 上から順に積む：(大きさ, 色, 下のすきま, 文字)
stack :: Env -> Float -> Float -> Float -> [(Float, Color, Float, String)] -> Picture
stack env x y0 w = Pictures . go y0
  where
    go _ [] = []
    go y ((s, c, gap, str) : rest) = para env x y s c w str : go (y + paraH s w str + gap) rest

-- 画面の下の端にそろえて書く
footer :: Env -> Float -> Float -> Float -> Color -> String -> Picture
footer env x w s c str = para env x (envH env - 14 - paraH s w str) s c w str
