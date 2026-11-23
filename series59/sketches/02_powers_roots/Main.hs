-- ============================================
-- 59-02 冪と「分数の冪」 ─ a^{⊗n} = a ⊗ a ⊗ … ⊗ a = n·a、a^{⊗1/n} = a / n（⊗ の n 乗根 = n で割る = 平均）
-- 1 周 k 本の回路の重みの合計 w を ⊗ の k 乗根にすると「1 本あたりの平均」 → 08 の固有値（サイクル時間）の正体
-- 右：多項式 p(x) = ⊕ c_k ⊗ x^{⊗k} = max_k (c_k + k·x) は折れ線（凸）。ホーナー法なら ⊗ は 4 回で計算できる
--   （折れ線の形・「根」の話はシリーズ 38 トロピカル幾何学。ここでは計算の道具として使う）
--   画面では ⊕ を (+)、⊗ を (x)、ε を eps、a^{⊗n} を a^n と書く
-- 操作：↑↓ a / ← → n / 右のグラフ：左はしの ● を上下にドラッグ = 係数 c_k、クリックで選んで E = ε / R ランダム
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

ringW :: [Double]
ringW = [3, 7, 2, 8]          -- ★ 回路の辺の重み（4 本で 1 周）

data World = World
  { wA     :: !Double         -- ★ 底 a
  , wN     :: !Int            -- ★ 指数 n
  , wCoef  :: [Double]        -- ★ p(x) の係数 c_0 … c_4（ε にもできる）
  , wSelK  :: !Int
  , wDragK :: !Int            -- ドラッグ中の係数（−1 = なし）
  , wTest  :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "59-02 Powers and roots" world0)
  { skBack  = rgbC 22 22 28
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 3 4 [4, 2, 1, 1.5, -1] 0 (-1) []

-- ★ ⊗ のべき：n 回 ⊗ する（ε の 0 乗は e = 0）
powRepeat :: Double -> Int -> Double
powRepeat a n = foldl' (\p _ -> mpMul p a) 0 [1 .. n]

-- ★ くり返し 2 乗法：a^{⊗n} を約 2·log₂n 回の ⊗ で（2 つ目 = ⊗ の回数）
powFast :: Double -> Int -> (Double, Int)
powFast a n0 = go 0 a 0 n0
  where
    go !res !base !ops !n
      | n <= 0 = (res, ops)
      | otherwise =
          let (res', ops1) = if odd n then (mpMul res base, ops + 1) else (res, ops)
              n' = n `div` 2
              (base', ops2) = if n' > 0 then (mpMul base base, ops1 + 1) else (base, ops1)
          in go res' base' ops2 n'

powFrac :: Double -> Int -> Double            -- a^{⊗1/n}（n ≥ 1）
powFrac a n = if isEps a then negInf else a / fromIntegral n

polyDirect :: [Double] -> Double -> Double
polyDirect c x = foldl' mpAdd negInf [ mpMul ck (fromIntegral k * x) | (k, ck) <- zip [0 :: Int ..] c ]

polyHorner :: [Double] -> Double -> Double
polyHorner c x = case reverse c of
  [] -> negInf
  (cl : rest) -> foldl' (\s ck -> mpAdd ck (mpMul x s)) cl rest

-- いちばん大きい項の番号（なければ −1）
winner :: [Double] -> Double -> Int
winner c x = fst (foldl' (\(b, v) (k, ck) -> let t = mpMul ck (fromIntegral k * x) in if t > v + 1e-12 then (k, t) else (b, v)) (-1, negInf) (zip [0 ..] c))

-- 右のグラフ（p5 の gP）
gPOf :: Env -> G
gPOf env = G (envW env * 0.52) 110 (envW env * 0.44) (envH env - 270) (-4) 4 (-8) 16

colsK :: [(Int, Int, Int)]
colsK = [(255, 170, 70), (120, 200, 255), (150, 230, 160), (255, 220, 120), (220, 150, 255)]

colK :: Int -> Int -> Color
colK k a = let (r, g, b) = headOr (255, 255, 255) (drop k colsK) in rgbaC r g b a

handleXY :: G -> Int -> Double -> (Float, Float)
handleXY gp k ck = (gX gp 0 - 24 - 22 * fromIntegral k, if isEps ck then pY gp + pH gp + 22 else gY gp ck)

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Powers and fractional powers   a = " ++ mpF a ++ ", n = " ++ show n) 40 40 20 (grayC 240)
  , say "Up/Down a   <- -> n   right: drag a dot = coefficient, click to select, E = eps   R random" 40 66 15 (grayC 170) ]
  -- ---- 左：a^{⊗t} の階段（t = 0 … n）
  ++ [ plotAxes g "exponent t" "a^t = t*a" [0, 2, 4, 6, 8] (map jsRound [y0 g, 0, y1 g]) ]
  ++ [ Color (if t <= n then rgbC 255 170 70 else rgbaC 255 170 70 60) (rectTL (gX g (fromIntegral t) - 10) (min yv yz) 20 (abs (yv - yz)))
     | t <- [0 .. 8], let yv = gY g (powRepeat a t), let yz = gY g 0 ]
  ++ [ say ("a^" ++ show n ++ " = " ++ joinWith " (x) " (replicate (max 1 n) (mpF a)) ++ " = " ++ mpF (powRepeat a n) ++ "   (= " ++ show n ++ " x " ++ mpF a ++ ")") 40 ty 18 (grayC 240)
     , say ("Repeated squaring: " ++ mpF (fst pf) ++ " (" ++ show (snd pf) ++ " (x) ops; multiplying one by one: " ++ show (max 0 (n - 1)) ++ ")") 40 (ty + 28) 16 (grayC 200)
     , say ("a^(1/" ++ show n1 ++ ") = " ++ mpF a ++ " / " ++ show n1 ++ " = " ++ mpFmt 3 (powFrac a n1) ++ "   -> to the power " ++ show n1 ++ ": " ++ mpFmt 3 (powRepeat (powFrac a n1) n1)) 40 (ty + 62) 18 (rgbC 255 220 120) ]
  -- ---- 回路の平均
  ++ [ Color (rgbC 120 200 255) (ringAt cx cy (2 * rr) 2) ]
  ++ concat [ [ Color (rgbC 120 200 255) (circleAt (cx + rr * cos t) (cy + rr * sin t) 14)
              , sayM (mpFmt 0 wk) (cx + (rr + 18) * cos t2) (cy + (rr + 18) * sin t2) 14 (grayC 230) ]
            | (k, wk) <- zip [0 :: Int ..] ringW
            , let m = fromIntegral (length ringW)
                  t = fromIntegral k / m * tau - pi / 2
                  t2 = (fromIntegral k + 0.5) / m * tau - pi / 2 ]
  ++ [ say ("One lap: w = " ++ joinWith "(x)" (map (mpFmt 0) ringW) ++ " = " ++ mpFmt 0 wr) (cx + 110) (cy - 20) 15 (grayC 220)
     , say ("w^(1/" ++ show (length ringW) ++ ") = " ++ mpFmt 0 wr ++ "/" ++ show (length ringW) ++ " = " ++ mpFmt 2 (powFrac wr (length ringW)) ++ " = mean per arc") (cx + 110) (cy + 8) 15 (rgbC 120 200 255)
     , say "(= eigenvalue lambda, sketch 08)" (cx + 110) (cy + 32) 15 (rgbC 120 200 255) ]
  -- ---- 右：p(x) = max_k (c_k + k x)
  ++ [ plotAxes gp "x" "p(x) = max_k (c_k + k*x)" [-4, -2, 0, 2, 4] [-8, 0, 8, 16]
     , Color (grayC 70) (Line [(gX gp 0, pY gp), (gX gp 0, pY gp + pH gp)]) ]
  ++ [ Color (colK k 110) (lineW 1.5 (gX gp (-4), gY gp (ck - 4 * fromIntegral k)) (gX gp 4, gY gp (ck + 4 * fromIntegral k)))
     | (k, ck) <- zip [0 ..] coef, not (isEps ck) ]
  ++ [ Color (grayC 250) (polyline 4 [ (gX gp x, gY gp v) | i <- [0 .. 160 :: Int], let x = -4 + 8 * fromIntegral i / 160, let v = polyDirect coef x, not (isEps v) ]) ]
  ++ concat [ [ Color (colK k (if isEps ck then 80 else 255)) (circleAt hx hy (if k == wSelK w then 18 else 13))
              , sayC ("c" ++ show k) hx (hy - 10) 14 (grayC 230) ]
            | (k, ck) <- zip [0 ..] coef, let (hx, hy) = handleXY gp k ck ]
  ++ [ say ("x = " ++ fmt 2 mx ++ ": max directly = " ++ mpFmt 3 (polyDirect coef mx) ++ "   Horner = " ++ mpFmt 3 (polyHorner coef mx)
            ++ (if kw >= 0 then "   (winning term k = " ++ show kw ++ ")" else "")) (pX gp) (pY gp + pH gp + 58) 16 (grayC 240)
     , say "Horner: c0 (+) x(x)(c1 (+) x(x)(c2 (+) x(x)(c3 (+) x(x)c4)))" (pX gp) (pY gp + pH gp + 82) 14 (grayC 170)
     , say ("Coefficients: " ++ joinWith ", " [ "c" ++ show i ++ " = " ++ mpF c | (i, c) <- zip [0 :: Int ..] coef ]) (pX gp) (pY gp + pH gp + 104) 14 (grayC 170)
     , drawSelfTest env (wTest w) ]
  where
    a = wA w
    n = wN w
    n1 = max 1 n
    coef = wCoef w
    g = G 70 110 (envW env * 0.36) 230 0 8 (min 0 (8 * a) - 1) (max 0 (8 * a) + 1)
    pf = powFast a n
    ty = pY g + pH g + 70
    cx = 160
    cy = ty + 170
    rr = 70
    wr = foldl' mpMul 0 ringW
    gp = gPOf env
    (mxp, _) = mouseP env
    mx = clampv (-4) 4 (realToFrac ((mxp - pX gp) / pW gp) * 8 - 4)
    kw = winner coef mx

joinWith :: String -> [String] -> String
joinWith sep xs = case xs of
  [] -> ""
  (x : rest) -> x ++ concatMap (sep ++) rest

handleAt :: Env -> World -> (Float, Float) -> Int
handleAt env w (mx, my) = headOr (-1) [ k | (k, ck) <- zip [0 ..] (wCoef w), let (hx, hy) = handleXY (gPOf env) k ck
                                          , sqrt ((mx - hx) * (mx - hx) + (my - hy) * (my - hy)) < 12 ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let k = handleAt env w (toP5 env p) in pure (if k >= 0 then w { wDragK = k, wSelK = k } else w { wDragK = k })
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDragK = -1 }
  EventMotion p | wDragK w >= 0 ->
    let gp = gPOf env
        (_, my) = toP5 env p
        v = jsRound (clampv (-8) 16 (y0 gp + realToFrac ((pY gp + pH gp - my) / pH gp) * (y1 gp - y0 gp)) * 2) / 2
    in pure w { wCoef = [ if i == wDragK w then v else c | (i, c) <- zip [0 ..] (wCoef w) ] }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wA = min 10 (wA w + 0.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wA = max (-5) (wA w - 0.5) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wN = min 8 (wN w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wN = max 0 (wN w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "eE" -> pure w { wCoef = [ if i == wSelK w then (if isEps v then 0 else negInf) else v | (i, v) <- zip [0 ..] (wCoef w) ] }
    | c `elem` "rR" -> do
        cs <- mapM (const (randomRIO (0, 1 :: Double))) (wCoef w)
        pure w { wCoef = map (\u -> jsRound (u * 16 - 6)) cs }
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種 2・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("a^n = n*a (repeated and repeated squaring, 200 cases)", all (\(o, _, _, _) -> o) r1 && snd (powFast 3 13) <= 7)
  , ("fractional power: (a^(1/n))^n = a, eps^n = eps (n >= 1), eps^0 = e = 0", all (\(_, o, _, _) -> o) r1 && isEps (powRepeat negInf 3) && powRepeat negInf 0 == 0)
  , ("'dream formula' (a (+) b)^n = a^n (+) b^n (false for ordinary numbers) and (a (x) b)^n = a^n (x) b^n"
    , all (\(_, _, o3, o4) -> o3 && o4) r1 && (2 + 3) ^ (2 :: Int) /= (2 ^ (2 :: Int) + 3 ^ (2 :: Int) :: Int))
  , ("Horner = direct max (coefficients with eps, 100 cases)", all fst r2)
  , ("p(x) is convex: p((x+y)/2) <= (p(x) + p(y))/2", all snd r2)
  , ("cycle 3, 7, 2, 8: (x) 4th root of weight 20 = mean 5", powFrac (foldl' mpMul 0 ringW) 4 == 5) ]
  where
    (r1, r2) = withSeed 2 $ do
      a1 <- replicateM 200 $ do
        a <- fromIntegral <$> rndInt (-20) 20
        b <- fromIntegral <$> rndInt (-20) 20
        n <- rndInt 0 12
        let na = fromIntegral n * a
        pure ( powRepeat a n == na && fst (powFast a n) == na
             , not (n > 0 && not (near (powRepeat (powFrac a n) n) a 1e-9))
             , powRepeat (mpAdd a b) n == mpAdd (powRepeat a n) (powRepeat b n)
             , powRepeat (mpMul a b) n == mpMul (powRepeat a n) (powRepeat b n) )
      a2 <- replicateM 100 $ do
        c <- replicateM 5 (do u <- rnd; if u < 0.2 then pure negInf else fromIntegral <$> rndInt (-9) 9)
        x <- (\u -> u * 8 - 4) <$> rnd
        y <- (\u -> u * 8 - 4) <$> rnd
        let okH = not ((not (isEps (polyDirect c x)) || not (isEps (polyHorner c x))) && not (near (polyDirect c x) (polyHorner c x) 1e-9))
            m = polyDirect c ((x + y) / 2)
            okC = not (not (isEps m) && m > (polyDirect c x + polyDirect c y) / 2 + 1e-9)
        pure (okH, okC)
      pure (a1, a2)

-- ---------- やってみよう ----------
-- 1. a を負にすると a^{⊗n} の階段はどうなる？ a = 0（= e）なら？
-- 2. n = 8 で、くり返し 2 乗法の ⊗ の回数はいくつ？ powFast 3 1000 なら？（GHCi で試せる）
-- 3. 回路の重み ringW を変えて、⊗ の 4 乗根（平均）を確かめよう。いちばん遅い回路が全体の速さを決める（08）
-- 4. 右のグラフで c2 を ε にすると、折れ線のどこが変わる？
-- 5. 「夢の公式」(a ⊕ b)^{⊗2} = a^{⊗2} ⊕ b^{⊗2} を、ふつうの数 (a + b)² と比べよう

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
-- ---- グラフの道具（左上が原点、y は下向き）
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv (-0.02) 1.02 ((v - x0 g) / (x1 g - x0 g))) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv (-0.02) 1.02 ((v - y0 g) / (y1 g - y0 g))) * pH g

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (grayC 95) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 18) 14 (grayC 165) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (grayC 165) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 38) 14 (grayC 205), say yl (pX g) (pY g - 8) 14 (grayC 205) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank
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
