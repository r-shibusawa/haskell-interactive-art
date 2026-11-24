-- ============================================
-- 64-15 カメラ映像で重み付き多数決 📷 ─ 単純な 6 つの「検出器」が、画素ごとに「前景（手前のもの）か？」を投票する
-- 明るさ・赤み・動き（前のフレームとの差）・エッジ・まん中・背景との差（ゆっくり覚えた背景と比べる）。どれも単独ではまちがいが多い
--   重み付き多数決：重み w の合計が大きい方の意見に従う。クリックで正解を教えると、まちがえた検出器の重みを (1 − ε) 倍にする
--   自分のまちがい ≤ 2(1 + ε)·m* + 2 ln n / ε（m* = いちばん良い検出器のまちがい、n = 6）← series17/11 の乗算型重み更新法と同じ式
--   カメラがなければテスト画像（動く人形が前景）。T でテスト画像の正解を自動で教える
-- 操作：1 / 2 クリックで教えるのは「前景」/「背景」 / クリック 正解を教える / T 自動で教える / ↑↓ ε / C 重みを戻す / V カメラ ⇔ テスト画像 / S 保存
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (runST)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate, sortBy, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import GHC.Clock (getMonotonicTime)
import System.Environment (lookupEnv)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
camW', camH' :: Int
camW' = 160                    -- 映像の大きさ（160 × 120）
camH' = 120

nDet :: Int
nDet = 6

detNames :: [String]           -- 画面に出す名前
detNames = ["brightness", "redness", "motion", "edge", "center", "bg diff"]

detNamesJ :: [String]          -- 自己テストの文に入れる名前（p5.js 版と同じ）
detNamesJ = ["明るさ", "赤み", "動き", "エッジ", "まん中", "背景との差"]

detCol :: Int -> RGB
detCol k = headOr ink (drop k [(236, 214, 160), (240, 120, 100), (64, 186, 176), (110, 165, 240), (190, 135, 235), (150, 210, 95)])

-- 6 つの検出器の票（画素ごとに True/False）と明るさ
data Det = Det { dLum :: !(U.Vector Float), dV :: ![U.Vector Bool] }

data World = World
  { wEps      :: !Double         -- ★ まちがえた検出器の重みを減らす割合
  , wTeachFg  :: !Int            -- クリックで教えるのは 1 = 前景 / 0 = 背景
  , wAuto     :: !Bool
  , wUseCam   :: !Bool
  , wCam      :: Maybe Camera
  , wLive     :: !Bool
  , wTFrame   :: !Int
  , wPx       :: !(U.Vector Word8)   -- いまの画像（R, G, B を順に）
  , wPrev     :: !(Maybe (U.Vector Float))   -- 前のフレームの明るさ
  , wBg       :: !(Maybe (U.Vector Float))   -- ゆっくり覚えた背景
  , wDet      :: !Det
  , wWts      :: ![Double]
  , wDetMiss  :: ![Int]
  , wAlgMiss  :: !Int
  , wTaught   :: !Int
  , wClicks   :: ![(Int, Int)]   -- (画素の番号, 正解)、40 まで
  , wRng      :: !Rng            -- 自動で教える画素を選ぶ乱数（自己テストの続きから）
  , wTest     :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "64-15 Camera weighted majority" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      let (rs, g) = selfTestAll
      printSelfTest rs
      cam <- startCamera camW' camH'     -- 左右反転した映像（p5.js の flipped: true）
      pure world0 { wCam = Just cam, wRng = g, wTest = rs }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = resetWeights (World 0.25 1 True True Nothing False 0 (testFrame 0) Nothing Nothing (detectAll (testFrame 0) Nothing Nothing) [] [] 0 0 [] (rngOf 1) [])

resetWeights :: World -> World
resetWeights w = w { wWts = replicate nDet 1, wDetMiss = replicate nDet 0, wAlgMiss = 0, wTaught = 0, wClicks = [] }

-- テスト画像（t = 時刻）：暗い部屋、明るいランプ（背景）、しまもようの壁（背景）、左右に動く人形（前景：赤っぽい顔 + 青い服）
figureAt :: Int -> (Double, Double)
figureAt t = (80 + 45 * sin (fromIntegral t * 0.03), 62)

isFigure :: Int -> Int -> Int -> Bool
isFigure xi yi t = let (fx, fy) = figureAt t
                       x = fromIntegral xi
                       y = fromIntegral yi
                   in (x - fx) * (x - fx) + (y - fy + 24) * (y - fy + 24) < 13 * 13 || (abs (x - fx) < 15 && y > fy - 12 && y < fy + 40)

testFrame :: Int -> U.Vector Word8
testFrame t = U.generate (camW' * camH' * 3) px
  where
    (fx, fy) = figureAt t
    -- Uint8ClampedArray と同じ：0〜255 にはさんで、ちょうど半分は偶数へ丸める
    w8 v = fromIntegral (round (max 0 (min 255 v)) :: Int) :: Word8
    px j = let (i, q) = j `quotRem` 3
               (yi, xi) = i `quotRem` camW'
               x = fromIntegral xi :: Double
               y = fromIntegral yi :: Double
               c0 = [50 + 0.2 * y, 55 + 0.15 * x, 75]
               c1 = if (x - 25) * (x - 25) + (y - 22) * (y - 22) < 14 * 14 then [250, 240, 190] else c0   -- ランプ
               c2 = if xi > 125 && (xi `div` 4) `mod` 2 == 0 then [150, 150, 160] else c1            -- しまの壁
               c3 | (x - fx) * (x - fx) + (y - fy + 24) * (y - fy + 24) < 13 * 13 = [225, 150, 120]  -- 顔
                  | abs (x - fx) < 15 && y > fy - 12 && y < fy + 40 = [40, 70, 160]                   -- 服
                  | otherwise = c2
           in w8 (headOr 0 (drop q c3))

-- ★ 6 つの検出器の票。prev = 前のフレームの明るさ、bg = 覚えた背景
detectAll :: U.Vector Word8 -> Maybe (U.Vector Float) -> Maybe (U.Vector Float) -> Det
detectAll px prev bg = Det lum [v0, v1, v2, v3, v4, v5]
  where
    n = camW' * camH'
    cD i = fromIntegral (px U.! i) :: Double
    lum = U.generate n (\i -> realToFrac ((0.3 * cD (3 * i) + 0.59 * cD (3 * i + 1) + 0.11 * cD (3 * i + 2)) / 255) :: Float)
    l i = realToFrac (lum U.! i) :: Double
    v0 = U.generate n (\i -> l i > 0.55)                                          -- ★ しきい値
    v1 = U.generate n (\i -> let r = cD (3 * i) / 255
                                 g = cD (3 * i + 1) / 255
                                 b = cD (3 * i + 2) / 255
                             in r - (g + b) / 2 > 0.12)
    v2 = case prev of
      Nothing -> U.replicate n False
      Just p -> U.generate n (\i -> abs (l i - realToFrac (p U.! i)) > 0.06)
    v3 = U.generate n (\i -> let (y, x) = i `quotRem` camW'
                                 gx = if x > 0 && x < camW' - 1 then l (i + 1) - l (i - 1) else 0
                                 gy = if y > 0 && y < camH' - 1 then l (i + camW') - l (i - camW') else 0
                             in abs gx + abs gy > 0.15)
    v4 = U.generate n (\i -> let (y, x) = i `quotRem` camW'
                                 a = (fromIntegral x - fromIntegral camW' / 2) / fromIntegral camW' :: Double
                                 b = (fromIntegral y - fromIntegral camH' / 2) / fromIntegral camH'
                             in a * a + b * b < 0.06)
    v5 = case bg of
      Nothing -> U.replicate n False
      Just bgv -> U.generate n (\i -> abs (l i - realToFrac (bgv U.! i)) > 0.1)

-- ゆっくり覚える背景
updateBg :: Maybe (U.Vector Float) -> U.Vector Float -> U.Vector Float
updateBg Nothing lum = lum
updateBg (Just bg) lum = U.zipWith (\b l -> let bd = realToFrac b :: Double in realToFrac (bd + 0.01 * (realToFrac l - bd))) bg lum

-- ★ 重み付き多数決：前景の票の重み > 全体の半分 なら前景
wmVote :: [Double] -> [Int] -> Int
wmVote w d = let (s, tot) = foldl' (\(!a, !t) (wk, dk) -> (if dk /= 0 then a + wk else a, t + wk)) (0, 0) (zip w d)
             in if 2 * s > tot then 1 else 0

-- ★ 正解を 1 つ教える：まちがえた検出器の重みを (1 − ε) 倍（小さくなりすぎたら全体を割り直す）
teach :: [Double] -> [Int] -> Int -> Double -> ([Double], Bool)
teach w d truth e =
  let alg = wmVote w d
      out = zipWith (\v dk -> if dk /= truth then v * (1 - e) else v) w d
      mx = maxOr 0 out
  in (if mx < 1e-100 then map (/ mx) out else out, alg /= truth)

votesAt :: Det -> Int -> [Int]
votesAt det i = [ if v U.! i then 1 else 0 | v <- dV det ]

teachAt :: Int -> Int -> World -> World
teachAt i truth w =
  let d = votesAt (wDet w) i
      (w', wrong) = teach (wWts w) d truth (wEps w)
  in w { wWts = w', wDetMiss = zipWith (\m dk -> if dk /= truth then m + 1 else m) (wDetMiss w) d
       , wAlgMiss = wAlgMiss w + (if wrong then 1 else 0), wTaught = wTaught w + 1
       , wClicks = lastN 40 (wClicks w ++ [(i, truth)]) }
  where lastN k xs = drop (length xs - k) xs

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let w = w0 { wTFrame = wTFrame w0 + 1 }
  fr <- case wCam w of
    Just cam | wUseCam w -> do
      ok <- camLive cam
      if not ok then pure Nothing else do
        f <- cameraFrame cam
        pure (if B.length f >= camW' * camH' * 3 then Just f else Nothing)
    _ -> pure Nothing
  let (px, live) = case fr of
        Just f -> (U.generate (camW' * camH' * 3) (BU.unsafeIndex f), True)
        Nothing -> (testFrame (wTFrame w), False)
      det = detectAll px (wPrev w) (wBg w)
      w1 = w { wPx = px, wLive = live, wDet = det, wPrev = Just (dLum det), wBg = Just (updateBg (wBg w) (dLum det)) }
      w2 = if not live && wAuto w1 && envFrame env `mod` 6 == 0 && wTaught w1 < 400
             then let (u, g) = rndStep (wRng w1)
                      i = floor (u * fromIntegral (camW' * camH'))
                  in teachAt i (if isFigure (i `mod` camW') (i `div` camW') (wTFrame w1) then 1 else 0) w1 { wRng = g }
             else w1
  _ <- evaluate (U.length (dLum det))
  pure w2

-- 映像と、重み付き多数決の結果の箱
boxes :: Env -> (Float, Box, Box)
boxes env = let s = min ((envW env - 140) / (2 * fromIntegral camW')) ((envH env - 420) / fromIntegral camH')
            in (s, Box 40 100 (fromIntegral camW' * s) (fromIntegral camH' * s), Box (80 + fromIntegral camW' * s) 100 (fromIntegral camW' * s) (fromIntegral camH' * s))

draw :: Env -> World -> Picture
draw env w = view env
  [ imageBox (bxX vb) (bxY vb) (bxW vb) (bxH vb) camW' camH' srcPic
  , imageBox (bxX ob) (bxY ob) (bxW ob) (bxH ob) camW' camH' outPic
  , Pictures [ Color (colOf (if t == 1 then teal else wine)) (ringAt (bxX b + (fromIntegral (i `mod` camW') + 0.5) * s) (bxY b + (fromIntegral (i `div` camW') + 0.5) * s) 9 2)
             | (i, t) <- wClicks w, b <- [vb, ob] ]
  , say (if wLive w then "camera" else "test image (the moving doll is foreground)") (bxX vb) (bxY vb - 8) 14 (colOf ink)
  , say "weighted majority: teal = foreground" (bxX ob) (bxY ob - 8) 14 (colOf ink)
  -- ---- 下：6 つの検出器と重み
  , Pictures [ Pictures [ imageBox x ty tw th (camW' `div` 2) (camH' `div` 2) (thumb k)
                        , say (headOr "" (drop k detNames)) x (ty + th + 18) 14 (colOf (detCol k))
                        , Color (rgbC 60 45 50) (rectTL x (ty + th + 26) tw 12)
                        , Color (colOf (detCol k)) (rectTL x (ty + th + 26) (tw * f2 (wk / maxW)) 12)
                        , say ("weight " ++ fmtNum 3 (wk / tot)) x (ty + th + 58) 14 (rgbC 200 185 170)
                        , say ("misses " ++ show mk) x (ty + th + 78) 14 (rgbC 200 185 170) ]
             | (k, wk, mk) <- zip3 [0 ..] (wWts w) (wDetMiss w), let x = 40 + fromIntegral k * (tw + 16) ]
  , say ("taught " ++ show (wTaught w) ++ "   majority misses " ++ show (wAlgMiss w) ++ " <= 2(1 + eps)*" ++ show mStar ++ " + 2 ln 6 / eps = " ++ fmtNum 1 bound
         ++ "   (best detector: " ++ headOr "" (drop (argMinL (map fromIntegral (wDetMiss w))) detNames) ++ ")") 40 (envH env - 40) 15 (colOf ink)
  -- ---- 文字
  , say ("Camera weighted majority   eps = " ++ fmtNum 2 (wEps w) ++ "   click teaches \"" ++ (if wTeachFg w == 1 then "foreground" else "background") ++ "\"") 40 40 20 (colOf ink)
  , say ("1 fg / 2 bg   click teach   T auto" ++ (if wAuto w then " (ON)" else " (OFF)") ++ "   Up/Down eps   C reset   V camera/test   S save") 40 66 15 (rgbC 170 155 145)
  , drawSelfTest env (wTest w) ]
  where
    (s, vb, ob) = boxes env
    det = wDet w
    px = wPx w
    tot = sumL (wWts w)
    maxW = maxOr 1 (wWts w)
    mStar = minOr 0 (wDetMiss w)
    bound = 2 * (1 + wEps w) * fromIntegral mStar + (2 * log (fromIntegral nDet)) / wEps w
    srcPic = pixelImage camW' camH' 1 (\x y -> let i = 3 * (x + camW' * y) in (px U.! i, px U.! (i + 1), px U.! (i + 2)))
    -- wmVote と同じ（速くするために展開）
    outPic = pixelImage camW' camH' 1 (\x y ->
               let i = x + camW' * y
                   fgW = foldl' (\a (wk, v) -> if v U.! i then a + wk else a) 0 (zip (wWts w) (dV det))
                   gray = realToFrac (dLum det U.! i) * 255 :: Double
                   (cr, cg, cb) = clsCol 1
                   q v = fromIntegral (rnd8 v) :: Word8
               in if 2 * fgW > tot then (q (cr * 0.7 + gray * 0.3), q (cg * 0.7 + gray * 0.3), q (cb * 0.7 + gray * 0.3))
                  else (q (gray * 0.35), q (gray * 0.35), q (gray * 0.35)))
    -- 検出器の絵は半分の大きさ（80 × 60）で作る
    thumb k = let v = headOr U.empty (drop k (dV det))
                  (r, g, b) = detCol k
                  on = (fromIntegral (rnd8 r), fromIntegral (rnd8 g), fromIntegral (rnd8 b))
              in pixelImage (camW' `div` 2) (camH' `div` 2) 1 (\x y -> if v U.! (2 * x + camW' * 2 * y) then on else (25, 14, 20))
    ty = bxY vb + bxH vb + 30
    tw = min 130 ((envW env - 80) / fromIntegral nDet - 16)
    th = tw * fromIntegral camH' / fromIntegral camW'

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        (_, vb, ob) = boxes env
        hit = [ floor (u * fromIntegral camW') + camW' * floor (v * fromIntegral camH')
              | b <- [vb, ob], let u = (mx - bxX b) / bxW b, let v = (my - bxY b) / bxH b, u >= 0, u < 1, v >= 0, v < 1 ]
    in pure (case hit of
               (i : _) -> teachAt i (wTeachFg w) w
               [] -> w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wEps = min 0.5 (wEps w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wEps = max 0.05 (wEps w - 0.05) }
  EventKey (Char c) Down _ _
    | c == '1' -> pure w { wTeachFg = 1 }
    | c == '2' -> pure w { wTeachFg = 0 }
    | c == 't' || c == 'T' -> pure w { wAuto = not (wAuto w) }
    | c == 'c' || c == 'C' -> pure (resetWeights w)
    | c == 'v' || c == 'V' -> pure w { wUseCam = not (wUseCam w), wPrev = Nothing, wBg = Nothing }
    | c == 's' || c == 'S' -> do
        saveSvg "weighted_majority" env (draw env w)
        pure w
  _ -> pure w

-- ---- 自己テスト（結果と、そのあとの乱数の状態）
selfTest :: [(String, Bool)]
selfTest = fst selfTestAll

selfTestAll :: ([(String, Bool)], Rng)
selfTestAll =
  ( [ ("教えると、まちがえた検出器（2・4・6）だけ重みが × 0.75、当たった検出器はそのまま", and (zipWith3 (\v dk w0k -> near v (if dk /= 1 then w0k * 0.75 else w0k) 1e-15) r1 d1 w0))
    , ("重み付き多数決 = 前景の票の重み > 背景の票の重み（300 通り）。重みが全部 1 ならふつうの多数決（3 対 3 は背景）"
      , okV && wmVote (replicate 6 1) [1, 1, 1, 0, 0, 0] == 0 && wmVote (replicate 6 1) [1, 1, 1, 1, 0, 0] == 1)
    , ("テスト画像：160 × 120、NaN なし。人形の顔は「赤み」、ランプは「明るさ」、動いた人形のふちは「動き」が反応する"
      , U.length f0 == camW' * camH' * 3 && noNaN (map realToFrac (U.toList (dLum dt1)))
        && (dV dt1 !! 1) U.! (jsRoundI fpx + camW' * jsRoundI (fpy - 24)) && (dV dt1 !! 0) U.! (25 + camW' * 22)
        && or [ v && isFigure (i `mod` camW') (i `div` camW') 10 | (i, v) <- zip [0 ..] (U.toList (dV dt1 !! 2)) ])
    , ("テスト画像で 370 回教える：多数決のまちがい " ++ show alg ++ " ≤ 上限 2(1 + ε)m* + 2 ln 6/ε = " ++ fmtNum 1 bd ++ "（m* = " ++ show ms ++ "）", fromIntegral alg <= bd)
    , ("いちばんまちがいの少ない検出器（" ++ headOr "" (drop iBest detNamesJ) ++ "）の重みがいちばん大きい。重みはすべて正で有限"
      , argMaxL wEnd == iBest && all (\v -> v > 0 && not (isInfinite v)) wEnd)
    , ("カメラを使わないとき（V）はテスト画像に切りかわる（大きさ 160 × 120）", U.length (testFrame 0) == camW' * camH' * 3) ]
  , gEnd )
  where
    w0 = [1, 0.5, 0.25, 1, 1, 1]
    d1 = [1, 0, 1, 0, 1, 0]
    (r1, _) = teach w0 d1 1 0.25
    okV = withSeed 4 (and <$> mapM (const oneV) [1 .. 300 :: Int])
    oneV = do
      ws <- mapM (const rnd) [1 .. 6 :: Int]
      dd <- mapM (const (do u <- rnd; pure (if u < 0.5 then 1 else 0))) [1 .. 6 :: Int]
      let fgW = foldl' (\a (wk, v) -> if v /= 0 then a + wk else a) 0 (zip ws dd)
      pure (wmVote ws dd == (if fgW > sumL ws - fgW then 1 else 0))
    f0 = testFrame 0
    dt0 = detectAll f0 Nothing Nothing
    dt1 = detectAll (testFrame 10) (Just (dLum dt0)) (Just (dLum dt0))
    (fpx, fpy) = figureAt 10
    -- 400 フレーム（最初の 30 は背景を覚えるまで待つ）
    loop :: Int -> Maybe (U.Vector Float) -> Maybe (U.Vector Float) -> [Double] -> [Int] -> Int -> Rng -> ([Double], [Int], Int, Rng)
    loop t prev bg w miss al g
      | t >= 400 = (w, miss, al, g)
      | otherwise =
          let det = detectAll (testFrame t) prev bg
              prev' = Just (dLum det)
              bg' = Just (updateBg bg (dLum det))
          in if t < 30 then loop (t + 1) prev' bg' w miss al g else
             let (u, g') = rndStep g
                 i = floor (u * fromIntegral (camW' * camH'))
                 truth = if isFigure (i `mod` camW') (i `div` camW') t then 1 else 0
                 dd = votesAt det i
                 (w', wrong) = teach w dd truth 0.25
             in loop (t + 1) prev' bg' w' (zipWith (\m dk -> if dk /= truth then m + 1 else m) miss dd) (if wrong then al + 1 else al) g'
    (wEnd, miss, alg, gEnd) = loop 0 Nothing Nothing (replicate nDet 1) (replicate nDet 0) 0 (rngOf 8)
    ms = minOr 0 miss
    iBest = argMinL (map fromIntegral miss)
    bd = 2 * 1.25 * fromIntegral ms + (2 * log (fromIntegral nDet)) / 0.25 :: Double

-- ---------- やってみよう ----------
-- 1. T を切って、自分でクリックして教えよう（1 前景 / 2 背景）。何回教えると塗り分けが落ち着く？
-- 2. ↑↓ で ε を大きくすると、重みの変わり方は？ 速く決まるが、たまたまのまちがいにも振り回される
-- 3. カメラでは、どの検出器が強い？ 部屋の明るさ・服の色・動き方で答えが変わる
-- 4. 検出器を 1 つ自分で作ろう（例：青み b − (r + g)/2 > 0.1）。nDet と detNames も増やす
-- 5. series17/11 では天気予報の専門家に同じ更新をしている。「誰が当たるか分からないとき」の強い作戦。S で保存

-- ==================================================
-- ここから下は「アンサンブル学習の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。点 x = U.Vector Double（x1, x2, …）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 小さな道具：はさむ・合計・平均・いちばん大きい番号・数の表示
type Pt = U.Vector Double                     -- 点（x1, x2, …）

pt2 :: Double -> Double -> Pt
pt2 u v = U.fromList [u, v]

-- データ：点 X とクラス y（0, 1）
data DataSet = DataSet { dsX :: !(V.Vector Pt), dsY :: !(U.Vector Int) }

xAt :: V.Vector Pt -> Int -> Int -> Double    -- X[i][f]
xAt xs i f = (xs V.! i) U.! f

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

-- はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumL :: [Double] -> Double
sumL = foldl' (+) 0

sumU :: U.Vector Double -> Double
sumU = U.foldl' (+) 0

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else sumL a / fromIntegral (length a)

-- ÷n の分散
varOf :: [Double] -> Double
varOf a = let m = meanOf a in if null a then 0 else foldl' (\s v -> s + (v - m) * (v - m)) 0 a / fromIntegral (length a)

-- いちばん大きい番号（同点なら小さい番号）
argMaxL :: [Double] -> Int
argMaxL [] = 0
argMaxL (a0 : as) = go 1 0 a0 as
  where go _ b _ [] = b
        go !i !b !bv (v : vs) = if v > bv then go (i + 1) i v vs else go (i + 1) b bv vs

-- いちばん小さい値の最初の番号（indexOf(Math.min(...))）
argMinL :: [Double] -> Int
argMinL = argMaxL . map negate

sigm :: Double -> Double
sigm z = 1 / (1 + exp (-(clampv z (-40) 40)))

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- 数を短く書く（p5.js 版の fmtNum：toFixed(d) のあと余分な 0 を消す。0.000001 より小さいと 1e-7 の形）
fmtNum :: Int -> Double -> String
fmtNum d v
  | isNaN v || isInfinite v = "?"
  | n == 0 = "0"
  | otherwise = (if v < 0 then "-" else "") ++ body
  where
    n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
    ds = show n
    padded = replicate (d + 1 - length ds) '0' ++ ds
    (ip, fp) = splitAt (length padded - d) padded
    tiny = fromIntegral n / (10 ^^ d :: Double) < 1e-6
    sig = reverse (dropWhile (== '0') (reverse ds))
    ex = length ds - 1 - d
    body | tiny = take 1 sig ++ (if length sig > 1 then "." ++ drop 1 sig else "") ++ "e" ++ show ex
         | d > 0 = stripZeros (ip ++ "." ++ fp)
         | otherwise = ip
    stripZeros s = let t = reverse (dropWhile (== '0') (reverse s)) in if lastOr ' ' t == '.' then init t else t

pctStr :: Double -> String
pctStr v = fmtNum 1 (100 * v) ++ "%"

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：列を種 s から始め直す（0 なら 1）
seedRng :: Int -> R ()
seedRng s = R (\_ -> let w = fromIntegral s :: Word32 in ((), Rng (if w == 0 then 1 else w)))

rngOf :: Int -> Rng
rngOf s = snd (runR (seedRng s) (Rng 1))

-- 計算を実行して、結果と次の状態
evalR :: Rng -> R a -> (a, Rng)
evalR g m = runR m g

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

rndGauss :: R Double                          -- 標準正規分布（ボックス-ミュラー）
rndGauss = do
  u0 <- rnd
  v <- rnd
  let u = max 1e-12 u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- フィッシャー-イェーツのシャッフル（後ろから i と 0〜i のどこかを入れかえる）
shuffleU :: U.Unbox a => U.Vector a -> R (U.Vector a)
shuffleU v = do
  let n = U.length v
  js <- mapM (\i -> do u <- rnd; pure (i, floor (u * fromIntegral (i + 1)))) [n - 1, n - 2 .. 1]
  pure (U.modify (\mv -> mapM_ (\(i, j) -> MU.swap mv i j) js) v)

-- ブートストラップ：0〜n−1 を重複ありで n 個
bootIdx :: Int -> R (U.Vector Int)
bootIdx n = U.fromList <$> mapM (const (do u <- rnd; pure (floor (u * fromIntegral n)))) [1 .. n]

-- ---- 描く道具：色（オーケストラ：えんじ色の黒、象牙色、金管の金、弦の青緑）、領域の絵、点
type RGB = (Double, Double, Double)

bgCol, ink, gold, teal, wine :: RGB
bgCol = (24, 11, 16)                          -- 背景（えんじ色の黒）
ink = (242, 233, 214)
gold = (226, 176, 74)
teal = (64, 186, 176)
wine = (196, 72, 96)

clsCol :: Int -> RGB                          -- ★ クラス 0（金）・クラス 1（青緑）の色
clsCol c = if c == 0 then (226, 170, 70) else (60, 182, 176)

colOf :: RGB -> Color
colOf (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) 255

colA :: Int -> RGB -> Color
colA a (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) a

rnd8 :: Double -> Int
rnd8 v = max 0 (min 255 (round (fin 0 v)))

-- クラス 1 の確率 → 背景と混ぜた色（k = 濃さ）
probCol :: Double -> Double -> RGB
probCol p k = let a = clampv k 0 1 * abs (2 * p - 1) + 0.12
                  (cr, cg, cb) = clsCol (if p > 0.5 then 1 else 0)
                  (br, bg, bb) = bgCol
              in (br + (cr - br) * a, bg + (cg - bg) * a, bb + (cb - bb) * a)

-- 幅 iw × 高さ ih の画像：全部の画素に fn u v → (r, g, b) を塗る（u は右向き、v は上向き、どちらも 0〜1）
paintImage :: Int -> Int -> (Double -> Double -> RGB) -> Picture
paintImage iw ih fn = pixelImage iw ih 1 (\px py ->
  let (r, g, b) = fn ((fromIntegral px + 0.5) / fromIntegral iw) (1 - (fromIntegral py + 0.5) / fromIntegral ih)
  in (w8 r, w8 g, w8 b))
  where w8 v = fromIntegral (rnd8 v) :: Word8

-- マス目の値（行 j = 上から、列 i = 左から）を色にした画像
gridImage :: Int -> (Int -> RGB) -> Picture
gridImage cells fn = pixelImage cells cells 1 (\i j -> let (r, g, b) = fn (i + cells * j) in (w8 r, w8 g, w8 b))
  where w8 v = fromIntegral (rnd8 v) :: Word8

-- マス目の中心の点（p5.js 版の [(i + 0.5)/CELLS, 1 − (j + 0.5)/CELLS]）
cellPt :: Int -> Int -> Pt
cellPt cells q = let (j, i) = q `quotRem` cells
                 in pt2 ((fromIntegral i + 0.5) / fromIntegral cells) (1 - (fromIntegral j + 0.5) / fromIntegral cells)

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（画面の道具の座標で）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

data Box = Box { bxX :: !Float, bxY :: !Float, bxW :: !Float, bxH :: !Float }

toPx :: Box -> Double -> Double -> (Float, Float)   -- [0,1]² → 画面
toPx bx u v = (bxX bx + f2 u * bxW bx, bxY bx + (1 - f2 v) * bxH bx)

fromPx :: Box -> (Float, Float) -> (Double, Double)
fromPx bx (px, py) = (realToFrac ((px - bxX bx) / bxW bx), realToFrac (1 - (py - bxY bx) / bxH bx))

-- 点を描く（直径 r、ふちは黒）
drawPoints :: Box -> V.Vector Pt -> U.Vector Int -> Float -> Picture
drawPoints bx xs ys r = Pictures
  [ Pictures [Color (grayC 10) (circleAt px py (r + 2)), Color (colOf (clsCol (ys U.! i))) (circleAt px py r)]
  | i <- [0 .. V.length xs - 1]
  , let (px, py) = toPx bx (xAt xs i 0) (xAt xs i 1) ]

frameBox :: Box -> RGB -> Picture
frameBox bx c = Color (colOf c) (frameTL 1 (bxX bx) (bxY bx) (bxW bx) (bxH bx))

frameCol :: RGB                               -- 枠のふつうの色
frameCol = (110, 80, 85)

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

-- 右揃えで上下は中央（textAlign(RIGHT, CENTER)）、左揃えで上下は中央（textAlign(LEFT, CENTER)）
sayRM, sayLM :: String -> Float -> Float -> Float -> Color -> Picture
sayRM s x y size c = sayR s x (y + size * 0.4) size c
sayLM s x y size c = say s x (y + size * 0.4) size c

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
frameTL wt x y w h = if wt <= 1.2 then Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
                     else thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分（細いときは gloss の線）
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW wt a b = if wt <= 1.2 then Line [a, b] else thickLine wt a b

-- ---- 数の道具
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
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else fmtNum 6 r

-- 小数点以下 d 桁に丸めた数（+v.toFixed(d)）
roundTo :: Int -> Double -> Double
roundTo d v = fromIntegral (jsRoundI (v * 10 ^ d)) / 10 ^ d

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e9) 1e9)

-- JavaScript の toExponential(1)（例 6.7e-9）
toExp1 :: Double -> String
toExp1 v
  | isNaN v || isInfinite v = "?"
  | v == 0 = "0.0e+0"
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          pick e = let n = floor (a / (10 ^^ (e - 1)) + 1 / 2) :: Integer in (n, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= 100 then pick (e0 + 1) else if n < 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= 100 then (n1 `div` 10, e1 + 1) else (n1, e1)
          ds = show n2
      in (if v < 0 then "-" else "") ++ take 1 ds ++ "." ++ drop 1 ds ++ "e" ++ (if e2 >= 0 then "+" else "-") ++ show (abs e2)

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

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

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 220) (envH env - 12) 14 (if all snd rs then rgbC 140 215 170 else rgbC 255 90 110)
