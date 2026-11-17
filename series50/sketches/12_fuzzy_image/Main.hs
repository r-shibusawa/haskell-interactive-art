-- ============================================
-- 50-12 ファジィ画像処理（コントラスト強調とエッジ検出）
-- 画素の明るさを「明るい」の所属度 μ（0〜1）にする → INT 演算子で 0.5 より上はより明るく、下はより暗く → 画素に戻す
--   INT(μ) = 2μ²（μ ≤ 0.5）、1 − 2(1 − μ)²（μ > 0.5）。エッジ：「となりとの差 dx が 0 に近い かつ dy が 0 に近い」でなければ縁
-- 操作：↑↓ = INT を何回かけるか / ←→ = 「差が 0 に近い」の幅 t / E = エッジを元画像・強調後のどちらで測るか / N = 別のテスト画像
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import System.Environment (lookupEnv)
import Numeric (showFFloat)

imgW, imgH :: Int
imgW = 240                 -- 画像の大きさ（小さめにして計算を軽く）
imgH = 150

data World = World
  { wNInt    :: !Int          -- ★ INT 演算子をくり返す回数（最初は 2）
  , wTEdge   :: !Double       -- ★ 「差が 0 に近い」の三角形の半分の幅（最初は 0.12）
  , wEdgeEnh :: !Bool         -- エッジを強調後の画像で測るか
  , wSeed    :: !Int          -- 次のテスト画像の乱数の種
  , wG0      :: U.Vector Double   -- 元の明るさ 0〜255
  , wMuA     :: U.Vector Double   -- μ（明るい の所属度）
  , wMuB     :: U.Vector Double   -- INT を nInt 回かけた μ
  , wEdge    :: U.Vector Double   -- エッジの度合い
  , wImA, wImB, wImE :: Picture   -- 3 枚の画像（計算しなおしたときだけ作る）
  , wHA, wHB :: [Int]             -- ヒストグラム（32 段）
  , wTest    :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "50-12 Fuzzy image processing" w0)
  { skBack  = hsb 250 18 14
  , skStart = \_ -> do
      printSelfTest selfTest
      pure w0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }
  where w0 = makeTestImage (World 2 0.12 False 1 U.empty U.empty U.empty U.empty Blank Blank Blank [] [] [])

-- ぼんやりしたテスト画像（コントラストが低い：明るさ 85〜165 くらい）
-- 縦のグラデーション、円と角の丸い長方形 9 個、「FUZZY」の文字、最後に少しノイズ
makeTestImage :: World -> World
makeTestImage w = recompute w { wG0 = g0, wSeed = wSeed w + 1 }
  where
    (shapes, r1) = takeShapes (9 :: Int) (lcg (wSeed w))
    (tx, ty, r2) = case r1 of
      (a : b : rest) -> (20 + a * 80, 60 + b * 60, rest)
      _ -> (20, 60, [])
    noise = U.fromListN (imgW * imgH) (r2 ++ repeat 0.5)
    takeShapes 0 rs = ([], rs)
    takeShapes k (f : s0 : c : x : y : rs) =
      let s = 20 + s0 * 60
          sh = (jsRound (85 + f * 80), c < 0.5, x * fromIntegral imgW, y * fromIntegral imgH, s)
          (more, rest) = takeShapes (k - 1) rs
      in (sh : more, rest)
    takeShapes _ rs = ([], rs)
    base px py =
      let x = fromIntegral px + 0.5
          y = fromIntegral py + 0.5
          grad = fromIntegral (jsRound (100 + fromIntegral py / fromIntegral imgH * 40))
          withShapes = foldl' (\g (col, isCircle, sx, sy, s) ->
                                 if (if isCircle then inCircle sx sy s x y else inRoundRect sx sy s (s * 0.6) 4 x y)
                                   then fromIntegral col else g) grad shapes
      in if inText tx ty x y then 150 else withShapes
    g0 = U.generate (imgW * imgH) (\i -> let (py, px) = i `divMod` imgW
                                         in clampv 0 255 (base px py + (noise U.! i - 0.5) * 10))

inCircle :: Double -> Double -> Double -> Double -> Double -> Bool
inCircle cx cy d x y = (x - cx) ^ (2 :: Int) + (y - cy) ^ (2 :: Int) <= (d / 2) ^ (2 :: Int)

-- 左上 (rx, ry)、幅 w、高さ h、角の半径 r の長方形の中か
inRoundRect :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Bool
inRoundRect rx ry w h r x y
  | x < rx || x > rx + w || y < ry || y > ry + h = False
  | otherwise = let qx = max 0 (max (rx + r - x) (x - (rx + w - r)))
                    qy = max 0 (max (ry + r - y) (y - (ry + h - r)))
                in qx * qx + qy * qy <= r * r

-- 「FUZZY」の文字（5 × 7 の点の字を 3 倍。左下が (tx, ty)）
inText :: Double -> Double -> Double -> Double -> Bool
inText tx ty x y =
  let cx = floor ((x - tx) / 3) :: Int
      cy = floor ((y - (ty - 21)) / 3) :: Int
      ch = cx `div` 6
      col = cx `mod` 6
  in x >= tx && y >= ty - 21 && cy < 7 && ch < 5 && col < 5
     && getAt ' ' (getAt "" (getAt [] font ch) cy) col == '#'
  where
    fF = ["#####", "#....", "#....", "####.", "#....", "#....", "#...."]
    fU = ["#...#", "#...#", "#...#", "#...#", "#...#", "#...#", ".###."]
    fZ = ["#####", "....#", "...#.", "..#..", ".#...", "#....", "#####"]
    fY = ["#...#", "#...#", ".#.#.", "..#..", "..#..", "..#..", "..#.."]
    font = [fF, fU, fZ, fZ, fY]

-- パラメータが変わったときだけ計算する
recompute :: World -> World
recompute w = w { wMuA = muA, wMuB = muB, wEdge = e
                , wImA = toImage muA, wImB = toImage muB, wImE = toImage e
                , wHA = histo muA, wHB = histo muB }
  where
    muA = fuzzify (wG0 w)
    muB = U.map (\m -> intN m (wNInt w)) muA
    e = fuzzyEdge (if wEdgeEnh w then muB else muA) imgW imgH (wTEdge w)

toImage :: U.Vector Double -> Picture
toImage arr = pixelImage imgW imgH 1 (\x y -> let v = fromIntegral (max 0 (min 255 (round (arr U.! (y * imgW + x) * 255) :: Int))) in (v, v, v))

histo :: U.Vector Double -> [Int]
histo arr = U.toList (U.accumulate (+) (U.replicate 32 0) (U.map (\v -> (min 31 (floor (v * 32)), 1)) arr))

draw :: Env -> World -> Picture
draw env w = view env $
  concat [ [ Translate (x + iw / 2) (y0 + ih / 2) (Scale sc (-sc) im)
           , txt x (y0 - 8) 15 (hsb 0 0 90) nm ]
         | (x, im, nm) <- zip3 xs [wImA w, wImB w, wImE w] names ]
  ++ drawHist (wHA w) x0 hy iw hh (hsb 260 30 90) ++ drawHist (wHB w) x1 hy iw hh (hsb 20 45 100)
     -- INT の曲線
  ++ [ Color (hsba 0 0 100 0.3) (rectLine (x2 + cw / 2) (hy + hh / 2) cw hh)
     , Color (hsba 0 0 100 0.25) (Line [(x2, hy + hh), (x2 + cw, hy)])
     , Color (hsb 175 60 80) (thickPath 2.5 [ (x2 + realToFrac m * cw, hy + hh - realToFrac (intN m (wNInt w)) * hh)
                                            | i <- [0 .. 50 :: Int], let m = fromIntegral i / 50 ])
     , txt (x2 + cw + 8) (hy + 16) 14 (hsb 0 0 70) ("mu -> INT^" ++ show (wNInt w) ++ "(mu)")
     , txt (x2 + cw + 8) (hy + 36) 14 (hsb 0 0 70) "0.5 stays put" ]
  ++ mouseInfo
  ++ [ txt 20 (iy + 26) 14 (hsb 0 0 70) ("Edge = 1 - min(zero(dx), zero(dy)), zero(d) = triangle (-t, 0, t), t = " ++ fmt 2 (wTEdge w)
                                         ++ ". Difference >= t: fully edge (1)")
     , txt 20 36 18 (hsb 0 0 95) ("Up/Down: INT count " ++ show (wNInt w) ++ "   Left/Right: width t = " ++ fmt 2 (wTEdge w)
                                  ++ "   E: edge source   N: new image")
     , txt 20 62 15 (hsb 0 0 70) "mu = (b - min) / (max - min). INT pushes values above 0.5 toward 1, below toward 0 -> histogram splits"
     , txt 20 (envH env - 20) 14 (if all snd (wTest w) then hsb 160 50 90 else hsb 0 80 100) (selfTestLine (wTest w)) ]
  where
    sc = min ((envW env - 80) / (3 * fromIntegral imgW)) (envH env * 0.5 / fromIntegral imgH)
    iw = fromIntegral imgW * sc
    ih = fromIntegral imgH * sc
    y0 = 110
    x0 = 20
    x1 = 40 + iw
    x2 = 60 + 2 * iw
    xs = [x0, x1, x2]
    names = [ "Original (mu = membership in bright)", "Enhanced: INT x " ++ show (wNInt w)
            , "Fuzzy edges (measured on " ++ (if wEdgeEnh w then "enhanced" else "original") ++ ")" ]
    hy = y0 + ih + 20
    hh = envH env * 0.16
    cw = min iw (hh * 1.6)
    iy = hy + hh + 34
    (mx, my) = p5Mouse env
    hits = [ (x, px, py) | x <- xs
           , let px = floor ((mx - x) / sc) :: Int
           , let py = floor ((my - y0) / sc) :: Int
           , px >= 0, px < imgW, py >= 0, py < imgH ]
    mouseInfo = case hits of
      ((x, px, py) : _) | U.length (wG0 w) == imgW * imgH ->
        let k = py * imgW + px
        in [ txt 20 iy 16 (hsb 0 0 95) ("Pixel (" ++ show px ++ ", " ++ show py ++ ")  brightness " ++ show (jsRound (wG0 w U.! k))
                                        ++ " -> mu = " ++ fmt 3 (wMuA w U.! k) ++ "  -> INT^" ++ show (wNInt w) ++ " = " ++ fmt 3 (wMuB w U.! k)
                                        ++ "  edge degree = " ++ fmt 3 (wEdge w U.! k))
           , Color (hsb 20 45 100) (rectLine (x + fromIntegral px * sc) (y0 + fromIntegral py * sc) 8 8) ]
      _ -> []

drawHist :: [Int] -> Float -> Float -> Float -> Float -> Color -> [Picture]
drawHist h x y w hh c =
  [ Color (hsba 0 0 100 0.3) (rectLine (x + w / 2) (y + hh / 2) w hh) ]
  ++ [ Color c (rectTL (x + fromIntegral i * bw) (y + hh - f * hh) (bw - 1) (f * hh))
     | (i, v) <- zip [0 :: Int ..] h, let f = fromIntegral v / mx ]
  ++ [ txt x (y + hh + 16) 14 (hsb 0 0 70) "0", txt (x + w / 2) (y + hh + 16) 14 (hsb 0 0 70) "mu"
     , txt (x + w - 8) (y + hh + 16) 14 (hsb 0 0 70) "1" ]
  where
    mx = fromIntegral (foldl' max 1 h) :: Float
    bw = w / fromIntegral (max 1 (length h))

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> recompute w { wNInt = min 6 (wNInt w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> recompute w { wNInt = max 0 (wNInt w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> recompute w { wTEdge = min 0.5 (wTEdge w + 0.02) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> recompute w { wTEdge = max 0.02 (wTEdge w - 0.02) }
  EventKey (Char c) Down _ _
    | c `elem` "eE" -> recompute w { wEdgeEnh = not (wEdgeEnh w) }
    | c `elem` "nN" -> makeTestImage w
  _ -> w

-- ---- 自己テスト：手で計算できる値と比べる
selfTest :: [(String, Bool)]
selfTest =
  [ ("INT(0.25) = 2 * 0.25^2 = 0.125", near (intOp 0.25) 0.125)
  , ("INT(0.75) = 1 - 2 * 0.25^2 = 0.875", near (intOp 0.75) 0.875)
  , ("INT(0.5) = 0.5, INT(0) = 0, INT(1) = 1 (fixed points)", near (intOp 0.5) 0.5 && intOp 0 == 0 && intOp 1 == 1)
  , ("INT keeps the order (monotone)", and [intN (fromIntegral i / 100) 3 >= intN (fromIntegral (i - 1) / 100) 3 | i <- [1 .. 100 :: Int]])
  , ("darkest pixel -> mu = 0, brightest -> 1, middle -> 0.5", U.toList (fuzzify (U.fromList [50, 100, 150])) == [0, 0.5, 1])
  , ("a flat image has edge 0 everywhere", U.all (== 0) (fuzzyEdge (U.replicate 25 0.4) 5 5 0.1))
  , ("1 on a black/white border, 0 away from it", e U.! (2 * 5 + 1) == 1 && e U.! (2 * 5 + 3) == 0)
  , ("neighbour difference dx = t/2 -> zero = 0.5 -> edge 0.5",
       near (fuzzyEdge (U.fromList [0, 0.025, 0.05, 0, 0.025, 0.05, 0, 0.025, 0.05]) 3 3 0.1 U.! 4) 0.5) ]
  where
    st = U.fromList [if x < 2 then 0 else 1 | _y <- [0 .. 4 :: Int], x <- [0 .. 4 :: Int]]
    e = fuzzyEdge st 5 5 0.1

-- ---------- やってみよう ----------
-- 1. ↑ で INT の回数を増やす。ヒストグラムが真ん中から両端へ分かれていく。何回で白黒の 2 値に近くなる？
-- 2. INT は 0.5 をさかいに分ける。fuzzify の最小・最大を変えて、さかい目の明るさを変えてみよう
-- 3. ←→ で t を変える。小さいと細かい模様（ノイズ）まで縁になる、大きいと太い縁だけ残る
-- 4. E で、強調した画像でエッジを測る。縁がくっきりする？
-- 5. fuzzyEdge の zero(d) を三角形（trimf）からガウス（gaussmf）に変えると、エッジの見え方はどう変わる？

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点、y は下向き）で
p5Mouse :: Env -> (Float, Float)
p5Mouse env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 左上が (x, y) の塗りつぶした長方形（p5.js の rect）
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 小数点以下 n 桁で書く（p5.js の toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) v ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Int
jsRound v = floor (v + 0.5)

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- ==================================================
-- ここから下は「ファジィの道具」（中身は読まなくても OK。消さないでね）
-- 計算はすべて Double（倍精度）で行う
-- ==================================================
-- ---- メンバーシップ関数（所属度 μ を 0〜1 で返す）
trimf :: Double -> Double -> Double -> Double -> Double      -- 三角：a で 0、b で 1、c で 0
trimf x a b c
  | x < a || x > c = 0
  | x == b = 1
  | x < b = if b > a then (x - a) / (b - a) else 1
  | otherwise = if c > b then (c - x) / (c - b) else 1

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- ---- 自己テストの道具
near :: Double -> Double -> Bool
near a b = nearTol 1e-9 a b

nearTol :: Double -> Double -> Double -> Bool
nearTol tol a b = abs (a - b) <= tol

-- 結果を端末に出す（テスト中は 1 行のまとめと失敗した項目だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK  " else "NG  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "NG ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- ---- ファジィ画像処理
-- 明るさ → 「明るい」の所属度（いちばん暗い画素が 0、いちばん明るい画素が 1）
fuzzify :: U.Vector Double -> U.Vector Double
fuzzify g
  | U.null g = g
  | otherwise = U.map (\v -> if hi > lo then (v - lo) / (hi - lo) else 0) g
  where
    lo = U.minimum g
    hi = U.maximum g

-- INT 演算子（強調）：0.5 より下は 2μ²、上は 1 − 2(1 − μ)²
intOp :: Double -> Double
intOp m = if m <= 0.5 then 2 * m * m else 1 - 2 * (1 - m) * (1 - m)

intN :: Double -> Int -> Double
intN m n = iterate intOp m !! max 0 n

-- ファジィなエッジ：規則「dx が 0 に近い かつ dy が 0 に近い ならば 縁ではない」の否定
fuzzyEdge :: U.Vector Double -> Int -> Int -> Double -> U.Vector Double
fuzzyEdge mu w h t
  | U.length mu < w * h = U.replicate (w * h) 0
  | otherwise = U.generate (w * h) $ \k ->
      let (y, x) = k `divMod` w
          dx = mu U.! (y * w + min (w - 1) (x + 1)) - mu U.! (y * w + max 0 (x - 1))
          dy = mu U.! (min (h - 1) (y + 1) * w + x) - mu U.! (max 0 (y - 1) * w + x)
      in 1 - min (trimf dx (-t) 0 t) (trimf dy (-t) 0 t)

-- 決まった順番の乱数（毎回同じ画像を作れる）。0〜1 の無限の列
lcg :: Int -> [Double]
lcg s = map (\x -> fromIntegral x / 233280) (drop 1 (iterate (\x -> (x * 9301 + 49297) `mod` 233280) (s * 9301 + 49297)))
