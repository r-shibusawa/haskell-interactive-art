-- ============================================
-- 43-06 ルジャンドル変換
-- 凸関数 f に対して  f*(p) = sup_x ( p x − f(x) )
--   「傾き p の直線 y = px をどこまで下げれば f に接するか」：接線の切片が −f*(p)
--   f** = f（凸なら元に戻る）。凸でない f は f** が「凸包」になる
-- 操作：マウス左右 = p / 1〜6 = 関数 / D = f** を重ねる / N = ノイズ凸関数を作り直す
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

xR, pR :: Float
xR = 3                 -- ★ x の範囲 ±xR
pR = 4                 -- ★ p の範囲 ±pR

nS :: Int
nS = 240               -- ★ 標本の数

-- 標本点
xs, ps :: U.Vector Float
xs = U.generate nS (\i -> -xR + 2 * xR * fromIntegral i / fromIntegral (nS - 1))
ps = U.generate nS (\i -> -pR + 2 * pR * fromIntegral i / fromIntegral (nS - 1))

data World = World
  { wNo     :: !Int
  , wDD     :: !Bool
  , wNoise  :: !(U.Vector Float)  -- ノイズ凸関数の値
  , wFx     :: !(U.Vector Float)  -- f(x)
  , wFstar  :: !(U.Vector Float)  -- f*(p)
  , wFdd    :: !(U.Vector Float)  -- f**(x)
  }

main :: IO ()
main = runSketch (sketch "43-06 Legendre transform" (rebuild (World 1 False (U.replicate nS 0) U.empty U.empty U.empty)))
  { skBack  = hsb 250 40 10
  , skStart = \_ -> do
      noise <- makeNoiseF
      pure (rebuild (World 1 False noise U.empty U.empty U.empty))
  , skDraw  = draw
  , skEvent = event
  }

fname :: Int -> String
fname k = case k of
  1 -> "x^2/2"
  2 -> "e^x"
  3 -> "x^4/4"
  4 -> "|x|"
  5 -> "noisy convex"
  _ -> "x^4/4 - x^2 (not convex)"

-- ★ 関数の定義
fval :: Int -> U.Vector Float -> Float -> Float
fval k noise x = case k of
  1 -> x * x / 2
  2 -> exp x
  3 -> x ^ (4 :: Int) / 4
  4 -> abs x
  5 -> noise U.! max 0 (min (nS - 1) (round ((x + xR) / (2 * xR) * fromIntegral (nS - 1))))
  _ -> x ^ (4 :: Int) / 4 - x * x

-- f'' > 0 のランダムな値を 2 回積分して凸関数を作る
makeNoiseF :: IO (U.Vector Float)
makeNoiseF = do
  d2 <- forM [1 .. nS - 1] $ \i -> do
    a <- randomRIO (0, 1)
    b <- randomRIO (0, 1)
    pure (0.2 + 2.5 * sin (fromIntegral i * 0.37 + a * 0.3) ^ (2 :: Int) * b :: Float)
  let h = 2 * xR / fromIntegral (nS - 1)
      d1 = scanl (\s v -> s + v * h) 0 d2
      mid = case drop (nS `div` 2) d1 of
        (v : _) -> v
        _ -> 0
      f = scanl (\s v -> s + (v - mid) * h) 0 (drop 1 d1)
      mn = foldl' min 1e30 f
  pure (U.fromList (map (subtract mn) f))

rebuild :: World -> World
rebuild w =
  let fx = U.map (fval (wNo w) (wNoise w)) xs
      fstar = U.map (\p -> fst (conj fx p)) ps
      fdd = U.map (\x -> U.maximum (U.zipWith (\p v -> p * x - v) ps fstar)) xs
  in w { wFx = fx, wFstar = fstar, wFdd = fdd }

-- ★ 共役：sup_x (p x − f(x)) を標本から求める（値と番号）
conj :: U.Vector Float -> Float -> (Float, Int)
conj fx p = U.ifoldl' best (-1 / 0, 0) (U.zip xs fx)
  where
    best b@(bv, _) i (x, f) =
      let v = p * x - f in if v > bv then (v, i) else b

panels :: Env -> (Graph, Graph)
panels env =
  let w = (envW env - 60) / 2
      h = envH env - 150
  in (Graph (Panel 20 100 w h) xR 6, Graph (Panel (40 + w) 100 w h) pR 10)

-- 枠と横の範囲、縦の尺度
data Graph = Graph { gP :: !Panel, gX :: !Float, gY :: !Float }

toS :: Graph -> Float -> Float -> Point
toS g x y =
  let p = gP g
  in (px p + (x + gX g) / (2 * gX g) * pw p, py p + ph p * 0.75 - y / gY g * ph p * 0.65)

-- 表示できる y の範囲に収める
inY :: Graph -> Float -> Float
inY g = clampF (-(gY g) * 0.36) (gY g * 0.78)

drawFrame :: Graph -> String -> Picture
drawFrame g title =
  let p = gP g
      (ox, oy) = toS g 0 0
  in Pictures
       [ box (hsb 0 0 35) (px p) (py p) (pw p) (ph p)
       , seg 1 (hsb 0 0 30) (px p, oy) (px p + pw p, oy)
       , seg 1 (hsb 0 0 30) (ox, py p) (ox, py p + ph p)
       , txt (px p + 10) (py p + 22) 14 (hsb 0 0 70) title ]

-- 曲線を描く（枠の外に出た部分は切って描かない）
plot :: Graph -> U.Vector Float -> U.Vector Float -> Float -> Float -> Bool -> Picture
plot g xv yv hue w dashed = Pictures (map one (runs (U.toList (U.zip xv yv))))
  where
    ok y = not (isNaN y || isInfinite y) && y > -(gY g) * 0.36 && y < gY g * 0.78
    runs [] = []
    runs l = case span (ok . snd) l of
      ([], rest) -> runs (drop 1 rest)
      (good, rest) -> good : runs (drop 1 rest)
    one r = let pts = [toS g x y | (x, y) <- r] in
      if dashed then dashPath w (hsb hue 70 100) 6 6 pts else path w (hsb hue 70 100) pts

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawFrame l ("f(x) = " ++ fname no ++ " and y = px  (p = " ++ fmt 2 p ++ ")")
  , drawFrame r "f*(p) = sup( px - f(x) )"
  -- 左：f、直線 px、接線 px − f*(p)
  , plot l xs (wFx w) 10 3 False ]
  ++ [ plot l xs (wFdd w) 55 3 True | wDD w ]
  ++ [ case clipSeg lp (toS l (-xR) (-p * xR)) (toS l xR (p * xR)) of
         Just (a, b) -> dashPath 1.5 (hsbA 180 60 100 50) 4 6 [a, b]
         Nothing -> Blank
     , case clipSeg lp (toS l (-xR) (-p * xR - val)) (toS l xR (p * xR - val)) of
         Just (a, b) -> seg 2.5 (hsb 180 70 100) a b
         Nothing -> Blank
     -- x* と縦のギャップ、切片
     , seg 3 (hsb 55 90 100) (sxx, sxy) (slx, sly)
     , dot (hsb 55 90 100) sxx sxy 12
     , dot (hsb 180 70 100) icx icy 10
     , fit lp (icx + 10) (min (py lp + ph lp - 6) (icy + (if val > 0 then 18 else -8))) ("intercept = -f*(p) = " ++ fmt 2 (-val))
     , fit lp (sxx + 10) (sxy - 10) ("x* = " ++ fmt 2 xStar ++ (if edge then " (edge: sup beyond range, f* -> inf)" else ""))
     , fit lp (slx + 10) (if abs (sly + 16 - (sxy - 10)) < 20 then sxy + 12 else sly + 16) ("max of px - f(x) = " ++ fmt 2 val)
     -- 右：f* と現在の p
     , plot r ps (wFstar w) 180 3 False
     , seg 1 (hsbA 180 60 100 50) (spx, py rp) (spx, py rp + ph rp)
     , dot (hsb 55 90 100) spx spy 12
     , fit rp (spx + 10) (spy - 10) ("f*(" ++ fmt 2 p ++ ") = " ++ fmt 2 val) ]
  ++ [ txt (px rp + 10) (py rp + ph rp - 12) 14 (hsb 0 0 70) s | Just s <- [note no] ]
  ++ [ txt 20 32 18 (hsb 0 0 100) ("f = " ++ fname no ++ " (1-6)   p = " ++ fmt 2 p ++ " (mouse)   D: f** " ++ (if wDD w then "ON (yellow dashes)" else "off") ++ "   N: new noise")
     , txt 20 58 14 (hsb 0 0 60) "Push a line of slope p up against f from below: the height where it touches is -f*(p). Points in x become slopes in p."
     , txt 20 78 14 (hsb 0 0 60) "If f is convex, f** = f. If not, f** is its convex hull." ]
  where
    (l, r) = panels env
    lp = gP l
    rp = gP r
    no = wNo w
    (mx, _) = mouseP env
    p | mx > px rp = remap (clampF (px rp) (px rp + pw rp) mx) (px rp) (px rp + pw rp) (-pR) pR
      | otherwise = remap (clampF (px lp) (px lp + pw lp) mx) (px lp) (px lp + pw lp) (-pR) pR
    (val, bi) = conj (wFx w) p
    xStar = xs U.! bi
    edge = bi == 0 || bi == nS - 1
    (sxx, sxy) = toS l xStar (inY l (fval no (wNoise w) xStar))
    (slx, sly) = toS l xStar (inY l (p * xStar))
    (icx, icy) = toS l 0 (inY l (-val))
    (spx, spy) = toS r p (inY r val)
    -- 文字が枠の右へはみ出さないように左へ寄せる
    fit pan x y s = txt (min x (px pan + pw pan - 4 - 10 * fromIntegral (length s))) y 14 (hsb 0 0 100) s

note :: Int -> Maybe String
note k = case k of
  4 -> Just "The conjugate of |x| is 0 for |p| <= 1 and inf outside (a wall)"
  1 -> Just "The conjugate of x^2/2 is p^2/2: self-dual"
  2 -> Just "The conjugate of e^x is p ln p - p (p > 0): entropy form"
  _ -> Nothing

event :: Env -> Event -> World -> IO World
event _ ev w = case ev of
  EventKey (Char k) Down _ _
    | k `elem` "123456" -> pure (rebuild w { wNo = fromEnum k - fromEnum '0' })
    | k `elem` "dD" -> pure w { wDD = not (wDD w) }
    | k `elem` "nN" -> do
        noise <- makeNoiseF
        let w' = w { wNoise = noise }
        pure (if wNo w == 5 then rebuild w' else w')
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. f = x²/2 で p を動かし、接点 x* = p になることを確かめる。f* も x²/2 と同じ形
-- 2. |x|（4）で p を ±1 の外へ出すと sup が「端」になる。f* が ∞ になる意味は？
-- 3. 6（凸でない）で D を押す。f** が f の下側をまっすぐ埋める（凸包）のはなぜ？
-- 4. 物理：f = 運動エネルギー mv²/2 の共役は p²/2m（ラグランジアン → ハミルトニアン）。fval に m を入れてみよう

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 左上が (x, y) の長方形の枠
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 点線の折れ線（on だけ描いて off だけ空ける）
dashPath :: Float -> Color -> Float -> Float -> [Point] -> Picture
dashPath w c on off pts = Pictures (walk on True pts)
  where
    walk left drawing (a : b : rest)
      | l <= left = [seg w c a b | drawing] ++ walk (left - l) drawing (b : rest)
      | otherwise = [seg w c a m | drawing] ++ walk (if drawing then off else on) (not drawing) (m : b : rest)
      where
        l = dist2 a b
        t = left / l
        m = (lerpF (fst a) (fst b) t, lerpF (snd a) (snd b) t)
    walk _ _ _ = []

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: RealFloat a => Int -> a -> String
fmt n v = showFFloat (Just n) v ""

-- 画面の枠（左上の x, y、幅、高さ）
data Panel = Panel { px :: !Float, py :: !Float, pw :: !Float, ph :: !Float }

-- 線分を枠で切る（Liang–Barsky）
clipSeg :: Panel -> Point -> Point -> Maybe (Point, Point)
clipSeg p (x1, y1) (x2, y2) = go 0 1 tests
  where
    dx = x2 - x1
    dy = y2 - y1
    tests = [(-dx, x1 - px p), (dx, px p + pw p - x1), (-dy, y1 - py p), (dy, py p + ph p - y1)]
    go t0 t1 [] = Just ((x1 + t0 * dx, y1 + t0 * dy), (x1 + t1 * dx, y1 + t1 * dy))
    go t0 t1 ((a, q) : rest)
      | a == 0 = if q < 0 then Nothing else go t0 t1 rest
      | a < 0 = if r > t1 then Nothing else go (max t0 r) t1 rest
      | otherwise = if r < t0 then Nothing else go t0 (min t1 r) rest
      where r = q / a
