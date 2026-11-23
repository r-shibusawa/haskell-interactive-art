-- ============================================
-- 63-15 体で報酬を与える 📷 ─ 画面の生き物が 4 つの動き方から 1 つを選んで 2 秒間やってみせる。
-- その間にカメラの前で手を振ると、動きの量（前のフレームとの差 = フレーム差分）がそのまま報酬になる。
-- 生き物は「勾配バンディット」で学ぶ：好み H を持ち、確率 π = ソフトマックス(H) で選ぶ。
-- 報酬が平均より大きければ選んだ動きの H を上げ、ほかを下げる：H_a ← H_a + α (R − R_bar) (1 − π_a)
-- カメラが使えないときは、2 秒の間のマウスのクリックが報酬（3 回で満点）
-- 操作：↑↓ 感度 / R 学習をやり直す / C カメラ ⇔ マウス / S 画面を保存
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Bits (shiftR, xor, (.|.))
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showFFloat)
import System.Environment (lookupEnv)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

acts :: [String]
acts = ["hop (gold)", "spin (cyan)", "wobble (pink)", "stretch (violet)"]

actCols :: [Color]
actCols = [goldC, cyanC, pinkC, violetC]

trialFrames :: Int
trialFrames = 120                      -- ★ 1 回の長さ（60 フレーム = 1 秒）

alphaG :: Double
alphaG = 0.4                           -- ★ 学習率

camW, camH :: Int
camW = 64                              -- カメラを小さくして差分をとる
camH = 48

data World = World
  { wPrefs     :: !(U.Vector Double)   -- 好み H
  , wRBar      :: !Double              -- これまでの報酬の平均（ベースライン）
  , wNTrials   :: !Int
  , wAct       :: !Int                 -- いまの動き
  , wTrialT    :: !Int                 -- 1 回の中のフレーム
  , wMotionSum :: !Double
  , wClicks    :: !Int
  , wLog       :: !(Seq.Seq (Int, Double))   -- (動き, 報酬)
  , wSens      :: !Int                 -- ★ 感度（動きの量に掛ける数）
  , wUseCam    :: !Bool
  , wLive      :: !Bool                -- カメラの映像が来ているか（C でマウスにしたら False）
  , wLastMot   :: !Double
  , wPrevGray  :: !(Maybe (U.Vector Double))
  , wFrame     :: !(Maybe BS.ByteString)
  , wCam       :: Maybe Camera
  , wRng       :: !Rng
  , wTest      :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "63-15 Reward by body (webcam)" (setup Nothing []))
  { skBack  = bgC
  , skStart = \_ -> do
      let rs = selfTest
      printSelfTest rs
      cam <- startCamera camW camH       -- 左右反転した映像
      pure (setup (Just cam) rs)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

setup :: Maybe Camera -> [(String, Bool)] -> World
setup cam rs = resetLearning (World (U.replicate 4 0) 0 0 0 0 0 0 Seq.empty 6 True False 0 Nothing Nothing cam (seedRng 15) rs)

resetLearning :: World -> World
resetLearning w = startTrial w { wPrefs = U.replicate (length acts) 0, wRBar = 0, wNTrials = 0, wLog = Seq.empty }

probsOf :: World -> [Double]
probsOf w = softmaxArr (U.toList (wPrefs w)) 1

startTrial :: World -> World
startTrial w = let (a, g) = runWith (wRng w) (sampleFrom (probsOf w))
               in w { wAct = a, wTrialT = 0, wMotionSum = 0, wClicks = 0, wRng = g }

-- ★ 勾配バンディットの更新（base = R_bar = これまでの報酬の平均 = ベースライン）
banditUpdate :: U.Vector Double -> Double -> Int -> Double -> U.Vector Double
banditUpdate h base a r = U.imap (\b hb -> clampv (hb + alphaG * (r - base) * ((if b == a then 1 else 0) - idx 0 p b)) (-20) 20) h
  where p = softmaxArr (U.toList h) 1

-- ★ フレーム差分：前のフレームとの明るさの差の平均（0〜1）
frameDiff :: U.Vector Double -> U.Vector Double -> Double
frameDiff a b = if U.null a then 0 else U.sum (U.zipWith (\x y -> abs (x - y)) a b) / fromIntegral (U.length a) / 255

-- カメラの絵を明るさ（0〜255）の列に
grayOf :: BS.ByteString -> U.Vector Double
grayOf f = U.generate (camW * camH) (\i -> let c k = fromIntegral (BSU.unsafeIndex f (3 * i + k)) in (c 0 + c 1 + c 2) / 3)

step :: Env -> Float -> World -> IO World
step _ _ w0 = do
  live <- maybe (pure False) camLive (wCam w0)
  let useLive = wUseCam w0 && live
  (motion, w1) <- case wCam w0 of
    Just cam | useLive -> do
      f <- cameraFrame cam
      if BS.length f < camW * camH * 3 then pure (0, w0 { wLive = useLive }) else do
        let g = grayOf f
            m = maybe 0 (frameDiff g) (wPrevGray w0)
        pure (m, w0 { wPrevGray = Just g, wFrame = Just f, wLive = useLive })
    _ -> pure (0, w0 { wLive = useLive })
  let w2 = w1 { wLastMot = motion, wMotionSum = wMotionSum w1 + motion, wTrialT = wTrialT w1 + 1 }
  pure $ if wTrialT w2 < trialFrames then w2 else   -- 1 回おわり → 報酬を計算して学習
    let r = if useLive then clampv (wMotionSum w2 / fromIntegral trialFrames * fromIntegral (wSens w2) * 10) 0 1
            else clampv (fromIntegral (wClicks w2) / 3) 0 1
        n = wNTrials w2 + 1
        l = let s = wLog w2 Seq.|> (wAct w2, r) in if Seq.length s > 3000 then Seq.drop 1 s else s
    in startTrial w2 { wNTrials = n, wPrefs = banditUpdate (wPrefs w2) (wRBar w2) (wAct w2) r
                     , wRBar = wRBar w2 + (r - wRBar w2) / fromIntegral n, wLog = l }

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("Reward by body: trial " ++ show (wNTrials w) ++ "   now = " ++ idx "" acts (wAct w)) 40 42 22 (grayC 240)
  , say ((if wLive w then "Wave your hand at the camera = reward" else "Mouse clicks = reward (no camera)") ++ "   Up/Down sensitivity " ++ show (wSens w)
         ++ "   R restart   C camera <-> mouse   S save") 40 68 15 (rgbC 170 176 210) ]
  -- ---- 生き物
  ++ [ Translate cx cy (Translate bx by (Rotate rotDeg body)) ]
  -- 1 回の残り時間
  ++ [ Color (rgbC 60 66 110) (rectTL (cx - 150) (cy + sz + 20) 300 10)
     , Color col (rectTL (cx - 150) (cy + sz + 20) (300 * fromIntegral (wTrialT w) / fromIntegral trialFrames) 10) ]
  -- ---- カメラと動きの量
  ++ (case (wLive w, wFrame w, wCam w) of
        (True, Just f, Just cam) -> [ Translate (vx + vw / 2) (vy + vh / 2) (Scale 1 (-1) (camImage cam f 3)) ]
        _ -> [ Color (rgbC 30 34 70) (rectTL vx vy vw vh)
             , sayM "No camera:" (vx + vw / 2) (vy + vh / 2 - 22) 14 (grayC 200)
             , sayM "click = reward" (vx + vw / 2) (vy + vh / 2) 14 (grayC 200)
             , sayM ("(now " ++ show (wClicks w) ++ " clicks)") (vx + vw / 2) (vy + vh / 2 + 22) 14 (grayC 200) ])
  ++ [ say ("motion " ++ fmtN 2 (wLastMot w * fromIntegral (wSens w) * 10)) (vx + vw + 20) (vy + 20) 14 (grayC 230)
     , Color limeC (rectTL (vx + vw + 20) (vy + 30) (f2 (clampv (wLastMot w * fromIntegral (wSens w) * 10) 0 1) * 160) 12) ]
  -- 確率
  ++ [ say "choice probability pi (softmax of H)" vx py 15 (grayC 230) ]
  ++ concat [ [ Color c (rectTL vx (py + 14 + fromIntegral k * 30) (f2 v * 300) 20)
              , say (nm ++ " " ++ show (jsRoundI (100 * v)) ++ "%") (vx + 310) (py + 29 + fromIntegral k * 30) 14 (grayC 230) ]
            | (k, v, c, nm) <- zip4' [0 :: Int ..] (probsOf w) actCols acts ]
  -- 報酬の履歴
  ++ [ plotAxes g "trial" "reward (colour = the move of that trial)" [0, x1 g] [0, 0.5, 1] ]
  ++ [ Color (idx white actCols a) (circleAt (gX g (fromIntegral i + 0.5)) (gY g r) 8) | (i, (a, r)) <- zip [0 :: Int ..] (toList (wLog w)) ]
  ++ [ Color (withA (120 / 255) white) (lineW 1 (pX g, gY g (wRBar w)) (pX g + pW g, gY g (wRBar w)))
     , say "white line = mean reward R_bar" (pX g) (pY g + pH g + 46) 14 (rgbC 170 176 210)
     , drawSelfTest env (wTest w) ]
  where
    cx = envW env * 0.3
    cy = envH env * 0.42
    t = fromIntegral (envFrame env) / 60 :: Float
    sz = min (envW env) (envH env) * 0.16
    a = wAct w
    col = idx white actCols a
    bx = if a == 2 then sin (t * 40) * 6 else 0
    by = if a == 0 then - abs (sin (t * 6)) * sz * 0.6 else 0
    rotDeg = if a == 1 then - (t * 4) * 180 / pi else 0     -- p5 の rotate（画面で時計回り）
    sx = if a == 3 then 1 + 0.35 * sin (t * 5) else 1
    sy = if a == 3 then 1 - 0.3 * sin (t * 5) else 1
    body = Pictures
      [ Color col (Scale sx sy (disc (sz / 2)))
      , Color (grayC 20) (circleAt (-sz * 0.18) (-sz * 0.08) (sz * 0.12))
      , Color (grayC 20) (circleAt (sz * 0.18) (-sz * 0.08) (sz * 0.12))
      , Color (grayC 20) (thickPath 3 [ (sz * 0.15 * cos th, sz * 0.08 + sz * 0.09 * sin th) | k <- [0 .. 16 :: Int], let th = 0.2 + (pi - 0.4) * fromIntegral k / 16 ]) ]
    vx = envW env * 0.58
    vy = 100
    vw = 192
    vh = 144
    py = vy + vh + 40
    g = G vx (py + 170) (envW env - vx - 50) (envH env - (py + 170) - 70) 0 (fromIntegral (max 20 (Seq.length (wLog w)))) 0 1
    zip4' (p : ps) (q : qs) (r : rs) (s : ss) = (p, q, r, s) : zip4' ps qs rs ss
    zip4' _ _ _ _ = []

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wClicks = wClicks w + 1 }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSens = min 30 (wSens w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSens = max 1 (wSens w - 1) }
  EventKey (Char ch) Down _ _
    | ch `elem` "rR" -> pure (resetLearning w)
    | ch `elem` "cC" -> pure w { wUseCam = not (wUseCam w) }
    | ch `elem` "sS" -> do saveSvg "human_reward" env (draw env w); pure w
  _ -> pure w

-- ---- 自己テスト（p5.js 版と同じ 4 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("learns the move with more reward (wobble, mean 0.8) in 500 trials: pi = " ++ show (jsRoundI (100 * idx 0 p 2)) ++ "% > 80%", idx 0 p 2 > 0.8)
  , ("probabilities add up to 1, no NaN", near (sum p) 1 1e-12 && noNaN (U.toList h))
  , ("a better-than-average reward raises H of the chosen move and lowers the others (sum of H unchanged)"
    , h2 U.! 1 > -0.2 && h2 U.! 0 < 0.5 && near (U.sum h2) s0 1e-12)
  , ("frame difference: same image gives 0, a quarter changing by 127.5 gives 0.125", frameDiff fa fa == 0 && near (frameDiff fa fb) 0.125 1e-6) ]
  where
    (h, _) = withSeed 1 (foldM one (U.replicate 4 0, 0) [1 .. 500 :: Int])
    one (hh, base) n = do
      a <- sampleFrom (softmaxArr (U.toList hh) 1)
      z <- randn
      let r = clampv ((if a == 2 then 0.8 else 0.2) + 0.15 * z) 0 1
      pure (banditUpdate hh base a r, base + (r - base) / fromIntegral n)
    p = softmaxArr (U.toList h) 1
    h20 = U.fromList [0.5, -0.2, 0.1, 0]
    s0 = U.sum h20
    h2 = banditUpdate h20 0.3 1 0.9
    fa = U.replicate 100 80
    fb = U.imap (\i v -> if i < 25 then 80 + 255 * 0.5 else v) fa

-- ---------- やってみよう ----------
-- 1. 「くるくる（spin）」のときだけ手を振って、何回で確率が 8 割をこえるか数えよう
-- 2. 途中で好みを変えると、生き物はついてくる？（勾配バンディットは確率がかたよると試さなくなる）
-- 3. alphaG を 0.1 と 1.0 でくらべよう
-- 4. 動きを 1 つ足そう（acts と actCols と、描くところに 1 つずつ）
-- 5. 人がほめると学ぶのは、大きな言語モデルの「人のフィードバックによる強化学習」と同じ考え方。どんな注意が要る？（13 の報酬のずる）

-- ==================================================
-- ここから下は「強化学習の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。乱数は p5.js 版と同じ mulberry32 で、同じ順に引く
-- ==================================================
-- ---- 色と小さな道具
bgC, goldC, cyanC, pinkC, limeC, violetC :: Color
bgC = rgbC 14 16 34                     -- 背景（アーケードの紺）
goldC = rgbC 255 196 60
cyanC = rgbC 90 215 245
pinkC = rgbC 245 110 170
limeC = rgbC 160 230 110
violetC = rgbC 175 140 255

negInf, posInf :: Double
negInf = -1 / 0
posInf = 1 / 0

-- v を a〜b に（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

-- いちばん大きい値の番号（同点はランダムに 1 つ。乱数を 1 つ使う）
argmaxR :: [Double] -> R Int
argmaxR q = case reverse best of
    [] -> pure 0
    bs -> do u <- rnd; pure (idx 0 bs (floor (u * fromIntegral (length bs))))
  where
    (_, best) = foldl' step (negInf, []) (zip [0 ..] q)
    step (m, b) (i, v)
      | v > m + 1e-12 = (v, [i])
      | abs (v - m) <= 1e-12 = (m, i : b)
      | otherwise = (m, b)

-- 同点は若い番号
argmaxFirst :: [Double] -> Int
argmaxFirst q = fst (foldl' (\(b, bv) (i, v) -> if v > bv then (i, v) else (b, bv)) (0, idx 0 q 0) (zip [0 ..] q))

maxOf :: [Double] -> Double
maxOf = foldl' (\m v -> if v > m then v else m) negInf

-- ε の確率でランダム、ほかは貪欲
epsGreedy :: [Double] -> Double -> R Int
epsGreedy q eps = do
  u <- rnd
  if u < eps then (\v -> floor (v * fromIntegral (length q))) <$> rnd else argmaxR q

-- ボルツマン分布 exp(h/τ) / Σ（大きい数でもあふれないように最大値を引く）
softmaxArr :: [Double] -> Double -> [Double]
softmaxArr h tau = map (/ s) e
  where
    m = maxOf h
    t = if tau == 0 || isNaN tau then 1 else tau
    e = map (\v -> exp ((v - m) / t)) h
    s = foldl' (+) 0 e

sampleFrom :: [Double] -> R Int
sampleFrom p = do
  u <- rnd
  let go _ _ [] = length p - 1
      go i c (x : xs) = let c' = c + x in if u < c' then i else go (i + 1) c' xs
  pure (go 0 0 p)

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else foldl' (+) 0 a / fromIntegral (length a)

-- 小数点以下 d 桁（toFixed）。−0.00 は 0.00 に、NaN は ?
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let s = showFFloat (Just d) v ""
                in if all (`elem` "-0.") s && take 1 s == "-" then drop 1 s else s

-- リストの i 番目（なければ d）
idx :: a -> [a] -> Int -> a
idx d xs i = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d
-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
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

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態はすぐに計算する（正格）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- 種 s で始めて結果だけ取り出す（seedRng(s) してから計算）
withSeed :: Word32 -> R a -> a
withSeed s m = fst (runR m (seedRng s))

-- 今の乱数の状態で計算して、(結果, 次の状態)
runWith :: Rng -> R a -> (a, Rng)
runWith s m = runR m s

rnd :: R Double
rnd = R (\(Rng s) -> case rndStep (Rng s) of (u, s') -> (u, s'))

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

randn :: R Double                             -- 標準正規分布（ボックス-ミュラー）
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))
-- ---- グラフの道具（左上が原点、y は下向き）：G 左 上 幅 高さ x0 x1 y0 y1
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv ((v - x0 g) / (x1 g - x0 g)) (-0.02) 1.02) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv ((v - y0 g) / (y1 g - y0 g)) (-0.02) 1.02) * pH g

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgbC 70 76 110) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 18) 14 (rgbC 150 156 190) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (rgbC 150 156 190) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 38) 14 (rgbC 205 210 235), say yl (pX g) (pY g - 8) 14 (rgbC 205 210 235) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- 直前 k 個の平均（ギザギザの学習曲線をならす）
smoothArr :: Seq.Seq Double -> Int -> Seq.Seq Double
smoothArr a k = Seq.fromList (go 0 0 (toList a))
  where
    arr = a
    go _ _ [] = []
    go !i !s (v : vs) = let s1 = s + v - (if i >= k then Seq.index arr (i - k) else 0)
                        in s1 / fromIntegral (min (i + 1) k) : go (i + 1) s1 vs

-- arr の i 番目を x = i + xOff に。長いときは間引いて 600 点まで（色 col、太さ sw）
drawCurve :: G -> Seq.Seq Double -> Color -> Float -> Double -> Picture
drawCurve g arr col sw xOff = Color col (polyline sw pts)
  where
    n = Seq.length arr
    stride = max 1 ((n + 599) `div` 600)
    pt i = (gX g (fromIntegral i + xOff), gY g (Seq.index arr i))
    pts = [ pt i | i <- [0, stride .. n - 1] ] ++ [ pt (n - 1) | n > 1, (n - 1) `mod` stride /= 0 ]

-- 色の見本と名前（items = [(色, 名前)]）
legend :: Float -> Float -> [(Color, String)] -> Picture
legend x y items = Pictures (concat [ [Color c (rectTL x (yy - 5) 16 10), say s (x + 22) (yy + 5) 14 (rgbC 220 225 245)]
                                    | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * fromIntegral k ])

-- 長さ cap までの列に追加（古いものから捨てる。メモリを増やし続けない）
pushCap :: Int -> Double -> Seq.Seq Double -> Seq.Seq Double
pushCap cap !v s = let s' = s Seq.|> v in if Seq.length s' > cap then Seq.drop 1 s' else s'
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

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

-- 点が (x, y) の長方形の中にあるか
inBox :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
inBox (mx, my) (x, y, w, h) = mx > x && mx < x + w && my > y && my < y + h
-- ---- 数の道具
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt = fmtN

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
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e9) 1e9)

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d
-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- NaN も ∞ もない
noNaN :: [Double] -> Bool
noNaN = all (\v -> not (isNaN v || isInfinite v))

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 画面の右下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 225 200 else rgbC 255 90 120)
