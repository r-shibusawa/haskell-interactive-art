-- ============================================
-- 57-13 八元数とファノ平面 ─ 7 つの虚数単位 e1〜e7 の掛け算
-- 7 点 7 本の線（円も 1 本の線）。同じ線の上の 3 つは e_a e_b = e_c（矢印の向きなら ＋、逆なら −）
-- 表の積（fanoMul）と、ケーリー-ディクソン構成の積（cdMul、12）が完全に一致することを確かめる
-- 操作：点をクリック 1 つ目 a、2 つ目 b で積、3 つ目 c で (ab)c と a(bc) / R ランダムな八元数を取りかえ / 1〜7 線を光らせる / 0 消す / ← → 線を順に / スペース 選び直し
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Array (accumArray, elems)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate, sort)
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

-- 点の位置：三角形の頂点・辺の中点・重心（ラベルは fanoLines がこの図の線になるように選んだ）
posLabel :: [Int]
posLabel = [1, 2, 4, 6, 3, 5, 7]       -- 上・左下・右下・下の中点・左の中点・右の中点・重心

data World = World
  { wPicked :: [Int]                   -- 選んだ点（最大 3 つ）
  , wLit    :: !Int                    -- 光らせる線（−1 でなし）
  , wXYZ    :: !(CD, CD, CD)           -- ランダムな八元数 x, y, z
  , wRng    :: !Rng
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "57-13 Octonions and the Fano plane" (start []))
  { skBack  = rgbC 16 18 34
  , skStart = \_ -> do
      printSelfTest selfTest
      pure (start selfTest)
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

-- 自己テストのあと seedRng(77) で x, y, z を作る
start :: [(String, Bool)] -> World
start t = let (xyz, r) = runGen newRandom (seedRng 77) in World [] (-1) xyz r t

newRandom :: Gen (CD, CD, CD)
newRandom = (,,) <$> cdRandom 8 <*> cdRandom 8 <*> cdRandom 8

-- 図の大きさ（中心 cx, cy と半径 rad）
geom :: Env -> (Float, Float, Float)
geom env = (min (envW env * 0.27) 380, envH env * 0.55, min 260 (envH env * 0.33))

ptPos :: Env -> Int -> (Float, Float)
ptPos env lab =
  let (cx, cy, rad) = geom env
      t = (cx, cy - rad); bl = (cx - rad * 0.866, cy + rad / 2); br = (cx + rad * 0.866, cy + rad / 2)
      mid (a, b) (c, d) = ((a + c) / 2, (b + d) / 2)
      s = length (takeWhile (/= lab) posLabel)
  in getAt (cx, cy) [t, bl, br, mid bl br, mid t bl, mid t br, (cx, cy)] s

onCircle :: [Int] -> Bool
onCircle = all (`elem` [6, 3, 5])

-- 0 でない成分だけを「−e5」「0.50 + 1.20e3」のように
octName :: CD -> String
octName v = case [ (x, a) | (a, x) <- zip [0 :: Int ..] v, abs x > 1e-12 ] of
  [] -> "0"
  nz -> unwords [ (if x < 0 then "-" else if k > 0 then "+" else "")
                  ++ (if abs (abs x - 1) < 1e-12 && a /= 0 then "" else fmt (if x == fromIntegral (round x :: Integer) then 0 else 2) (abs x))
                  ++ (if a /= 0 then "e" ++ show a else "")
                | (k, (x, a)) <- zip [0 :: Int ..] nz ]

dd :: (Float, Float) -> (Float, Float) -> Float
dd (a, b) (c, d) = sqrt ((a - c) * (a - c) + (b - d) * (b - d))

-- 矢じり（(x, y) で向き ang）
arrowHead :: Float -> Float -> Float -> Color -> Picture
arrowHead x y ang col = Color col (Polygon [ rot (10, 0), rot (-7, -7), rot (-7, 7) ])
  where rot (px, py) = (x + px * cos ang - py * sin ang, y + px * sin ang + py * cos ang)

draw :: Env -> World -> Picture
draw env w = view env $
  -- 7 本の線（向き付き）
  concat [ fanoLine n l | (n, l) <- zip [0 :: Int ..] fanoLines ]
  ++ concat [ let p@(px, py) = ptPos env a
                  k = maybe (-1) id (lookup a (zip pk [0 :: Int ..]))
                  fillC = case k of { 0 -> rgbC 110 170 255; 1 -> rgbC 255 150 90; 2 -> rgbC 150 230 150; _ -> rgbC 30 36 66 }
              in [ Color fillC (at p (disc 25)), Color white (circleLine (if k >= 0 then 3 else 1) px py 25)
                 , sayCC ("e" ++ show a) px py 20 white ]
            | a <- [1 .. 7] ]
  -- ---- 選んだ単位の積
  ++ picks
  -- ---- ランダムな八元数で確かめる
  ++ [ say "Random octonions x, y, z (R for new ones)" tx (ty + 210) 17 (rgbC 255 210 110) ]
  ++ concat [ [ say a tx yy 15 (grayC 190), say b (tx + 190) yy 15 (grayC 235), say c (tx + 300) yy 15 (grayC 170) ]
            | (n, (a, b, c)) <- zip [0 :: Int ..] rows, let yy = ty + 240 + fromIntegral n * 26 ]
  ++ [ sayBox "The norm rule |xy| = |x||y| holds (so we can divide by a nonzero octonion). One wrong sign in the table breaks it."
              tx (ty + 390) (envW env - tx - 20) 14 (grayC 170)
     , say "Octonions and the Fano plane: 7 lines = 7 copies of the quaternions" 20 36 20 (grayC 240)
     , say "Click points: a, b (product), c (associativity), a 4th starts again   R: new x, y, z   1-7: light a line   0: off" 20 62 15 (grayC 175)
     , say "A line (a, b, c) works like ij = k: ea eb = ec, eb ec = ea, ec ea = eb. Against the arrow -> minus" 20 86 15 (grayC 175)
     , drawSelfTest env (wTest w) ]
  where
    (cx, cy, rad) = geom env
    pk = wPicked w
    tx = envW env * 0.55; ty = 130
    fanoLine n l =
      let on = n == wLit w || (case pk of
                                 (a : b : _) -> a `elem` l && b `elem` l
                                 _ -> False)
          col = if on then rgbC 255 205 90 else rgbaC 110 150 220 170
          wt = if on then 4 else 2
      in if onCircle l
           then Color col (circleLine wt cx (cy + rad / 4) (rad / 2))      -- 内接円（半径 = rad/2）
                : [ arrowHead (cx + (rad / 2) * cos am) (cy + rad / 4 + (rad / 2) * sin am) (am + (if d > 0 then pi / 2 else -pi / 2)) col
                  | t <- [0 .. 2]                                         -- a → b → c の向きに円の上の矢印
                  , let (p0x, p0y) = ptPos env (getAt 0 l t)
                        (p1x, p1y) = ptPos env (getAt 0 l ((t + 1) `mod` 3))
                        a0 = atan2 (p0y - cy - rad / 4) (p0x - cx)
                        a1 = atan2 (p1y - cy - rad / 4) (p1x - cx)
                        d = atan2 (sin (a1 - a0)) (cos (a1 - a0))
                        am = a0 + d / 2 ]
           else
             -- 両はしの 2 点を結ぶ。矢印は a → b → c → a の向き（となり合う 2 点の間だけ）
             let ps = map (ptPos env) l
                 p i = getAt (0, 0) ps i
                 (e0, e1) = foldl1 (\m e -> if dd (p (fst e)) (p (snd e)) > dd (p (fst m)) (p (snd m)) then e else m) [(0, 1), (1, 2), (0, 2)]
             in Color col (thickLine wt (p e0) (p e1))
                : [ arrowHead ((ax + bx) / 2) ((ay + by) / 2) (atan2 (by - ay) (bx - ax)) col
                  | t <- [0 .. 2], let (ax, ay) = p t, let (bx, by) = p ((t + 1) `mod` 3), dd (ax, ay) (bx, by) <= rad * 1.2 ]
    picks = case pk of
      (a : b : rest) ->
        let ea = cdBasis 8 a 1; eb = cdBasis 8 b 1
            ab = fanoMul ea eb; ab2 = cdMul ea eb; ba = fanoMul eb ea
            nm i = "e" ++ show i
        in [ say (nm a ++ " " ++ nm b ++ " = " ++ octName ab ++ "   (table)") tx ty 18 (grayC 240)
           , say (nm a ++ " " ++ nm b ++ " = " ++ octName ab2 ++ "   (Cayley-Dickson)") tx (ty + 26) 18 (grayC 240)
           , say (nm b ++ " " ++ nm a ++ " = " ++ octName ba ++ (if a /= b then "   (opposite sign)" else "")) tx (ty + 52) 18 (rgbC 255 150 150) ]
           ++ (case rest of
                 (c : _) ->
                   let ec = cdBasis 8 c 1
                       l = fanoMul (fanoMul ea eb) ec
                       r = fanoMul ea (fanoMul eb ec)
                       same = cdMaxAbs (cdSub l r) < 1e-12
                   in [ say ("(" ++ nm a ++ " " ++ nm b ++ ") " ++ nm c ++ " = " ++ octName l) tx (ty + 90) 18 (grayC 240)
                      , say (nm a ++ " (" ++ nm b ++ " " ++ nm c ++ ") = " ++ octName r) tx (ty + 116) 18 (grayC 240)
                      , say (if same then "-> same (one line, or a repeated unit)" else "-> opposite sign! not associative")
                            tx (ty + 142) 18 (if same then rgbC 130 230 150 else rgbC 255 150 150) ]
                 [] -> [])
      _ -> [ sayBox "Click 2 points for the product, a 3rd one to compare (ab)c and a(bc)" tx (ty - 16) (envW env - tx - 20) 16 (grayC 170) ]
    (x, y, z) = wXYZ w
    xy = cdMul x y
    rows = [ ("|xy|", fmt 6 (cdNorm xy), "|x||y| " ++ fmt 6 (cdNorm x * cdNorm y))
           , ("table - CD", expo 1 (cdMaxAbs (cdSub (fanoMul x y) xy)), "")
           , ("(xy)z - x(yz)", fmt 4 (cdNorm (cdAssoc x y z)), "<- not 0 (non-assoc.)")
           , ("(xx)y - x(xy)", expo 1 (cdNorm (cdAssoc x x y)), "<- alternative")
           , ("assoc. (x, y, xy)", expo 1 (cdNorm (cdAssoc x y xy)), "<- Artin's theorem") ]

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP5 env
        hit = [ a | a <- [1 .. 7], dd (ptPos env a) (realToFrac mx, realToFrac my) < 30 ]
    in case hit of
         (a : _) -> w { wPicked = (if length (wPicked w) >= 3 then [] else wPicked w) ++ [a] }
         [] -> w
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> let (xyz, r) = runGen newRandom (wRng w) in w { wXYZ = xyz, wRng = r }
    | c >= '1' && c <= '7' -> w { wLit = fromEnum c - fromEnum '1' }
    | c == '0' -> w { wLit = -1 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wLit = (wLit w + 1) `rem` 7 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wLit = (wLit w + 6) `rem` 7 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPicked = [] }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 7 項目・同じ種・同じ許容誤差）
selfTest :: [(String, Bool)]
selfTest =
  [ ("the 7 oriented lines derived from the cdMul rule = fanoLines", all (`elem` found) want && all (`elem` want) found)
  , ("table product (fanoMul) = Cayley-Dickson product (cdMul): 200 trials, error " ++ expo 1 e1, e1 < 1e-12)
  , ("norm of products |xy| = |x||y| (200 trials, relative 1e-9)", e2 < 1e-9)
  , ("associativity fails: (e1e2)e4 = -e1(e2e4), random associators are /= 0"
    , cdMaxAbs (cdAdd (cdMul (cdMul u1 u2) u4) (cdMul u1 (cdMul u2 u4))) == 0 && cdMaxAbs (cdMul (cdMul u1 u2) u4) == 1 && e5 > 0.1)
  , ("alternative: (xx)y = x(xy), (xy)y = x(yy)", e3 < 1e-11)
  , ("Artin's theorem: elements built from x and y associate ((x, y, xy) = 0 etc.)", e4 < 1e-10)
  , ("ea^2 = -1, and ea eb = -eb ea when a /= b", sq && anti) ]
  where
    -- cdMul から向き付きの線を求め直して、fanoLines と比べる
    found = [ [getAt 0 t m, getAt 0 t ((m + 1) `mod` 3), getAt 0 t ((m + 2) `mod` 3)]
            | a <- [1 .. 7], b <- [1 .. 7], a /= b
            , let r = cdMul (cdBasis 8 a 1) (cdBasis 8 b 1)
            , (c, rc) <- take 1 [ (i, v) | (i, v) <- zip [0 ..] r, v /= 0 ]
            , rc == 1
            , let t = [a, b, c]
            , let m = length (takeWhile (/= minimum t) t) ]
    want = fanoLines
    trials = evalGen 13 (replicateM 200 newRandom)
    mx = maximum . (0 :)
    e1 = mx [ cdMaxAbs (cdSub (fanoMul x y) (cdMul x y)) | (x, y, _) <- trials ]
    e2 = mx [ abs (cdNorm (cdMul x y) / (cdNorm x * cdNorm y) - 1) | (x, y, _) <- trials ]
    e3 = mx [ max (cdMaxAbs (cdAssoc x x y)) (cdMaxAbs (cdAssoc x y y)) | (x, y, _) <- trials ]
    e4 = mx [ maximum [cdMaxAbs (cdAssoc x y (cdMul x y)), cdMaxAbs (cdAssoc (cdMul y x) x y), cdMaxAbs (cdAssoc (cdMul x x) y x)] | (x, y, _) <- trials ]
    e5 = mx [ cdNorm (cdAssoc x y z) | (x, y, z) <- trials ]
    u1 = cdBasis 8 1 1; u2 = cdBasis 8 2 1; u4 = cdBasis 8 4 1
    e i = cdBasis 8 i 1
    sq = and [ cdMaxAbs (cdAdd (fanoMul (e a) (e a)) (e 0)) == 0 | a <- [1 .. 7] ]
    anti = and [ cdMaxAbs (cdAdd (fanoMul (e a) (e b)) (fanoMul (e b) (e a))) == 0 | a <- [1 .. 7], b <- [1 .. 7], a /= b ]

-- ---------- やってみよう ----------
-- 1. e1 → e2 とクリック。結果は？ e2 → e1 では？ 同じ線の上の 3 つが「1 組の i, j, k」になっていることを確かめよう
-- 2. 同じ線にない 3 つ（例 e1, e2, e4）をクリック。(ab)c と a(bc) はどうなる？ 同じ線の上の 3 つでは？
-- 3. fanoLines の 1 本の向きを逆にすると（例 [1, 2, 3] → [1, 3, 2]）、自己テストのどれが FAIL になる？ |xy| = |x||y| は？
-- 4. R で何回取りかえても、交代的・アルティンの行は 0 のまま？ (xy)z − x(yz) は？
-- 5. 1 本の線の 3 つと 1 で「四元数」ができる。7 本の線 → 四元数のコピーが 7 つ入っている

-- ==================================================
-- ここから下は「四元数の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）。自己テストで使う
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rnd :: Rng -> (Double, Rng)
rnd (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 標準正規分布（ボックス＝ミュラー法）
rndNormal :: Rng -> (Double, Rng)
rndNormal r0 = let (a, r1) = rnd r0
                   (v, r2) = rnd r1
                   u = 1 - a
               in (sqrt (-2 * log u) * cos (2 * pi * v), r2)

-- 乱数を引きながら計算する小さなモナド（JavaScript と同じく左から順に引く）
newtype Gen a = Gen { runGen :: Rng -> (a, Rng) }

instance Functor Gen where
  fmap f (Gen g) = Gen (\r -> case g r of (a, r1) -> (f a, r1))

instance Applicative Gen where
  pure a = Gen (\r -> (a, r))
  Gen f <*> Gen g = Gen (\r -> case f r of
                                 (h, !r1) -> case g r1 of (a, !r2) -> (h a, r2))

instance Monad Gen where
  Gen g >>= k = Gen (\r -> case g r of (a, !r1) -> runGen (k a) r1)

rndG, rndNormalG :: Gen Double
rndG = Gen rnd
rndNormalG = Gen rndNormal

-- 種 s から始めて m を計算する
evalGen :: Word32 -> Gen a -> a
evalGen s m = fst (runGen m (seedRng s))

clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else v

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

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

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標に
mouseP5 :: Env -> (Double, Double)
mouseP5 env = let (x, y) = envMouse env in (realToFrac (x + envW env / 2), realToFrac (envH env / 2 - y))

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC r g b a = makeColorI r g b (max 0 (min 255 a))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 0〜255 の小数で色を作る（明るさを掛けた色など）
rgbD :: Double -> Double -> Double -> Double -> Color
rgbD r g b a = makeColor (c r) (c g) (c b) (c a)
  where c v = f2 (clampv (v / 255) 0 1)

-- 左下が (x, y) の文字（y は文字の下の線 = p5.js の BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央（textAlign(CENTER, CENTER)）
sayCC :: String -> Float -> Float -> Float -> Color -> Picture
sayCC s x y size c = sayC s x (y + size * 0.36) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 w の箱に折り返して書く（p5.js の text(s, x, y, w, h)。(x, y) は箱の左上）
sayBox :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayBox s x y w size c = Pictures [ say ln x (y + size + fromIntegral k * size * 1.25) size c | (k, ln) <- zip [0 :: Int ..] (wrapWords (words s)) ]
  where
    wrapWords [] = []
    wrapWords (h : t) = go h t
    go cur [] = [cur]
    go cur (x1 : xs)
      | textW (cur ++ " " ++ x1) size <= w = go (cur ++ " " ++ x1) xs
      | otherwise = cur : go x1 xs

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 中心 (x, y)、半径 r の円の線（太さ wt）
circleLine :: Float -> Float -> Float -> Float -> Picture
circleLine wt x y r = Translate x y (ThickCircle r wt)

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- ---- 数の道具
-- Math.hypot（JavaScript と同じ計算：最大値で割ってカハンの足し算）
hypotL :: [Double] -> Double
hypotL xs =
  let as = map abs xs
      m = maximum (0 : as)
      go (!s, !c) v = let n = v / m
                          summand = n * n - c
                          pre = s + summand
                      in (pre, (pre - s) - summand)
  in if any isInfinite as then 1 / 0 else if any isNaN as then 0 / 0 else if m == 0 then 0
     else sqrt (fst (foldl' go (0, 0) as)) * m

hypot3 :: Double -> Double -> Double -> Double
hypot3 a b c = hypotL [a, b, c]

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- JavaScript の % （0 の方へ切り捨てた余り）
jsMod :: Double -> Double -> Double
jsMod a b = a - b * fromIntegral (truncate (a / b) :: Integer)

-- Math.round（.5 は上へ）
jsRound :: Double -> Double
jsRound v = fromIntegral (floor (fin 0 v + 0.5) :: Integer)

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v0 = let v = fin 0 v0
               s = showFFloat (Just n) v ""
           in if all (`elem` "-0.") s then dropWhile (== '-') s else s

-- 指数で書く（toExponential(n)、例 1.2e-16）
expo :: Int -> Double -> String
expo n v0
  | v == 0 = fmt n 0 ++ "e+0"
  | otherwise = let s = showEFloat (Just n) v ""
                    (m, e) = break (== 'e') s
                    e' = drop 1 e
                in m ++ "e" ++ (if take 1 e' == "-" then e' else '+' : e')
  where v = fin 0 v0
-- ---- ケーリー-ディクソン構成：長さ 2ⁿ のリスト = 「数の 2 つ組」(a, b)
--   積の約束 (a, b)(c, d) = (ac − d* b, da + b c*)、共役 (a, b)* = (a*, −b)
--   長さ 1 = 実数、2 = 複素数、4 = 四元数（qMul と一致）、8 = 八元数、16 = 十六元数
type CD = [Double]

cdConj :: CD -> CD
cdConj x = case x of
  [v] -> [v]
  _ -> let h = length x `div` 2
           (a, b) = splitAt h x
       in cdConj a ++ map negate b

cdMul :: CD -> CD -> CD
cdMul x y = case (x, y) of
  ([u], v : _) -> [u * v]
  _ -> let h = length x `div` 2
           (a, b) = splitAt h x
           (c, d) = splitAt h y
           ac = cdMul a c
           db = cdMul (cdConj d) b
           da = cdMul d a
           bc = cdMul b (cdConj c)
       in zipWith (-) ac db ++ zipWith (+) da bc

cdAdd, cdSub :: CD -> CD -> CD
cdAdd = zipWith (+)
cdSub = zipWith (-)

cdNorm :: CD -> Double
cdNorm x = sqrt (foldl' (\s v -> s + v * v) 0 x)

cdBasis :: Int -> Int -> Double -> CD  -- s · e_a（長さ n）
cdBasis n a s = [ if t == a then s else 0 | t <- [0 .. n - 1] ]

cdRandom :: Int -> Gen CD
cdRandom n = replicateM n rndNormalG

cdMaxAbs :: CD -> Double
cdMaxAbs = foldl' (\m v -> max m (abs v)) 0

cdAssoc :: CD -> CD -> CD -> CD        -- 結合子 (xy)z − x(yz)
cdAssoc x y z = cdSub (cdMul (cdMul x y) z) (cdMul x (cdMul y z))
-- ---- ファノ平面の掛け算表（上の cdMul の約束から求めた 7 本の向き付きの線）
--   (a, b, c) は e_a e_b = e_c、e_b e_c = e_a、e_c e_a = e_b（逆順はマイナス）、e_a² = −1
fanoLines :: [[Int]]
fanoLines = [[1, 2, 3], [1, 4, 5], [1, 7, 6], [2, 4, 6], [2, 5, 7], [3, 4, 7], [3, 6, 5]]

-- e_a e_b = s · e_c を (s, c) で返す（a, b は 0〜7、0 = 実数の 1）
fanoUnit :: Int -> Int -> (Double, Int)
fanoUnit a b
  | a == 0 = (1, b)
  | b == 0 = (1, a)
  | a == b = (-1, 0)
  | otherwise = case [ (if (ib - ia + 3) `mod` 3 == 1 then 1 else -1, getAt 0 l (3 - ia - ib))
                     | l <- fanoLines, Just ia <- [lookup a (zip l [0 ..])], Just ib <- [lookup b (zip l [0 ..])] ] of
      (r : _) -> r
      [] -> (0, 0)

-- 八元数の積を表から（双線形にのばす）
fanoMul :: CD -> CD -> CD
fanoMul x y = elems (accumArray (+) 0 (0, 7)
  [ (c, s * xa * yb) | (a, xa) <- zip [0 ..] x, xa /= 0, (b, yb) <- zip [0 ..] y, yb /= 0, let (s, c) = fanoUnit a b ])
