-- ============================================
-- 51-13 固有形（フォン・フェルスター）
-- 同じ操作を、その結果にまたくり返す：x → Op(x) → Op(Op(x)) → … すると、はじめの形によらず決まった「固有のふるまい」に落ちつく
-- 1：数 x → cos x（電卓で cos を押し続ける。答えは 0.739085…） 2：形 → となりと平均して大きさをそろえる（だ円になる）
-- 3：形 → 半分に縮めた 3 つのコピー（シェルピンスキーの三角形になる）
-- 操作：1〜3 = 操作 / ドラッグ = はじめの形を描く（1 ではクリックで x₀） / スペース = 1 回 / A = 自動 / R = はじめに戻す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

nP :: Int
nP = 64              -- ★ 形の点の数

maxDists :: Int
maxDists = 1500      -- 変化 d の記録は新しい方からこの数まで

type Pt = (Double, Double)

data World = World
  { wMode    :: !Int
  , wX0      :: !Double          -- ★ 1 のはじめの数
  , wXs      :: [Double]         -- 1 の数の列（古い順、60 個まで）
  , wShape0  :: [Pt]             -- はじめの形
  , wCur     :: [Pt]             -- 2 のいまの形
  , wIfs     :: U.Vector Pt      -- 3 の点の集まり（0〜1 の座標）
  , wImg     :: Maybe Picture    -- 3 の点を塗った画像（点が変わったら作りなおす）
  , wDists   :: [Double]         -- 1 回ごとの変化 d（新しい順）
  , wIter    :: !Int
  , wAuto    :: !Bool
  , wDrawing :: Maybe [(Float, Float)]   -- 描いている線（画面の座標、新しい順）
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "51-13 Eigenform" world0)
  { skBack  = col (24, 35, 10) 100
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = restart (World 1 2.5 [] defaultShape [] U.empty Nothing [] 0 True Nothing [])

defaultShape :: [Pt]
defaultShape = [ (r * cos a + 0.3, 0.7 * r * sin a)
               | i <- [0 .. nP - 1], let a = fromIntegral i / fromIntegral nP * 2 * pi
                                         r = 1 + 0.45 * sin (5 * a) + 0.25 * cos (3 * a) ]

restart :: World -> World
restart w = w { wIter = 0, wDists = [], wXs = [wX0 w], wCur = wShape0 w
              , wIfs = U.fromList [ ((x + 1.6) / 3.2, (y + 1.6) / 3.2) | (x, y) <- wShape0 w ], wImg = Nothing }

-- ---- 操作 1：x → cos x
opCos :: Double -> Double
opCos = cos

-- ---- 操作 2：となり 2 つずつと平均（5 点平均）→ 重心を 0、大きさ（RMS 半径）を 1 に
opSmooth :: [Pt] -> [Pt]
opSmooth ps = normalizeShape [ avg i | i <- [0 .. n - 1] ]
  where
    v = U.fromList ps
    n = U.length v
    avg i = foldl' (\(!sx, !sy) d -> let (x, y) = v U.! ((i + d + n) `mod` n) in (sx + x / 5, sy + y / 5)) (0, 0) [-2 .. 2]

normalizeShape :: [Pt] -> [Pt]
normalizeShape qs = [ ((x - cx) / r, (y - cy) / r) | (x, y) <- qs ]
  where
    n = fromIntegral (max 1 (length qs))
    cx = sum [ x / n | (x, _) <- qs ]
    cy = sum [ y / n | (_, y) <- qs ]
    r0 = sqrt (sum [ (x - cx) ^ (2 :: Int) + (y - cy) ^ (2 :: Int) | (x, y) <- qs ] / n)
    r = if r0 == 0 || isNaN r0 then 1 else r0

-- ---- 操作 3：点の集まりを、半分に縮めて 3 か所へコピー（ハッチンソンの作用素）
ifsT :: [Pt]
ifsT = [(0, 0), (0.5, 0), (0.25, 0.433)]

opIFS :: U.Vector Pt -> U.Vector Pt
opIFS ps = U.concat [ U.map (\(x, y) -> (x / 2 + tx, y / 2 + ty)) ps | (tx, ty) <- ifsT ]

-- ---- 距離
rmsDist :: [Pt] -> [Pt] -> Double
rmsDist ps qs = sqrt (sum (zipWith (\(a, b) (c, d) -> (a - c) ^ (2 :: Int) + (b - d) ^ (2 :: Int)) ps qs) / fromIntegral (max 1 (length ps)))

hausdorff :: U.Vector Pt -> U.Vector Pt -> Double
hausdorff p q = sqrt (max (dir p q) (dir q p))
  where
    -- A のどの点も、B のいちばん近い点までの距離（の 2 乗）の最大
    dir a b =
      let (bxs, bys) = U.unzip b
          nb = U.length b
          nearest !ax !ay = go 0 1e18
            where go !j !best | j >= nb = best
                              | otherwise = let dx = ax - U.unsafeIndex bxs j
                                                dy = ay - U.unsafeIndex bys j
                                                d = dx * dx + dy * dy
                                            in go (j + 1) (if d < best then d else best)
          (axs, ays) = U.unzip a
          na = U.length a
          outer !i !m | i >= na = m
                      | otherwise = let d = nearest (U.unsafeIndex axs i) (U.unsafeIndex ays i)
                                    in outer (i + 1) (if d > m then d else m)
      in outer (0 :: Int) (0 :: Double)

oneIter :: World -> World
oneIter w = case wMode w of
  1 -> let x = last' (wX0 w) (wXs w)
           y = opCos x
       in done w { wDists = addD (abs (y - x)), wXs = lastN 60 (wXs w ++ [y]) }
  2 -> let n = opSmooth (wCur w)
       in done w { wDists = addD (rmsDist n (wCur w)), wCur = n }
  _ | wIter w >= 7 -> w
    | otherwise ->
        let n = opIFS (wIfs w)
            d = if fromIntegral (U.length n) * fromIntegral (U.length (wIfs w)) < (3e7 :: Double) then hausdorff n (wIfs w) else 0 / 0
        in done w { wDists = addD d, wIfs = n, wImg = Nothing }
  where
    addD d = take maxDists (d : wDists w)
    done v = v { wIter = wIter v + 1 }

step :: Env -> Float -> World -> IO World
step env _ w = do
  let per = if wMode w == 3 then 40 else if wMode w == 1 then 20 else 2
      w1 = if wAuto w && envFrame env `mod` per == 0 then oneIter w else w
  pure $ if wMode w1 == 3 && null (wImg w1) then w1 { wImg = Just (ifsImage env (wIfs w1)) } else w1

-- 左の絵の場所
panel :: Env -> (Float, Float, Float, Float)
panel env = (20, 90, envW env * 0.58, envH env - 140)

-- 3 の点の集まりを、縮めたピクセル画像に塗る（8 万画素以下）
ifsImage :: Env -> U.Vector Pt -> Picture
ifsImage env ps = at (ax + aw / 2, ay + ah / 2) (Scale 1 (-1) (pixelImage iw ih k px))
  where
    (ax, ay, aw, ah) = panel env
    k = max 1 (sqrt (aw * ah / 78000))
    iw = max 1 (floor (aw / k))
    ih = max 1 (floor (ah / k))
    s = min aw ah * 0.9
    ds = [0 :: Int]       -- 1 画素（約 2.5 ピクセル）の点。p5.js 版は 5000 点以下で太さ 3、それより多いと 1
    on = U.create $ do
      v <- MU.replicate (iw * ih) False
      U.forM_ ps $ \(x, y) -> do
        let cx = floor ((aw / 2 - s / 2 + f2 x * s) / k)
            cy = floor ((ah / 2 + s * 0.4 - f2 y * s) / k)
        sequence_ [ MU.write v (yy * iw + xx) True | dx <- ds, dy <- ds, let xx = cx + dx, let yy = cy + dy, xx >= 0, yy >= 0, xx < iw, yy < ih ]
      pure v
    (br, bg, bb) = (33, 30, 25)       -- 枠の色 HSB(28, 25, 13) をおよそ RGB に
    px x y = if on U.! (y * iw + x) then (255, 140, 40) else (br, bg, bb)

-- だ円以外の成分の割合：Σ_{k ≠ ±1} |c_k|² ÷ Σ |c_k|²（重心 0 なので c_0 = 0）
nonEllipse :: [Pt] -> Double
nonEllipse ps = if tot > 0 then 1 - ell / tot else 0
  where
    v = U.fromList ps
    n = U.length v
    es = [ (k, re * re + im * im)
         | k <- [0 .. n - 1]
         , let (re, im) = U.ifoldl' (\(!r, !i) j (x, y) -> let a = -2 * pi * fromIntegral k * fromIntegral j / fromIntegral n
                                                          in (r + x * cos a - y * sin a, i + x * sin a + y * cos a)) (0, 0) v ]
    tot = sum (map snd es)
    ell = sum [ e | (k, e) <- es, k == 1 || k == n - 1 ]

draw :: Env -> World -> Picture
draw env w = view env $
  [ say "Eigenform - repeat one operation: it settles, whatever the start" 20 30 20 cTxt
  , say ("1-3 operation (now " ++ show (wMode w) ++ ": " ++ getAt "" names (wMode w) ++ ")") 20 54 15 cSub
  , say ((if wMode w == 1 then "Click: x0" else "Drag: draw a shape") ++ "   Space: once   A: auto " ++ (if wAuto w then "ON" else "OFF") ++ "   R: reset") 20 76 15 cSub
  , Color (hsb 28 25 13) (rectTL ax ay aw ah) ]
  ++ body
  ++ [ Color (col cScope 100) (thickPath 2 (reverse d)) | Just d@(_ : _ : _) <- [wDrawing w] ]
  ++ [ say ("Iterations " ++ show (wIter w)) hx hy 18 cTxt
     , say ("Change in one step d = " ++ maybe "-" (\v -> showEFloat (Just 3) v "") lastD) hx (hy + 28) 15 cTube ]
  ++ info
  ++ [ plotFrame hx gy gw gh "log10 d (-12 to 1): going down = closer"
     , plotLine (reverse (map (\d -> logBase 10 (max d 1e-13)) (wDists w))) hx gy gw gh (-12) 1 cTube (max 30 (length (wDists w))) 2
     , say note1 hx (envH env - 42) 14 cSub
     , say note2 hx (envH env - 22) 14 cSub
     , drawSelfTest env (wTest w) ]
  where
    names = ["", "x -> cos x", "shape -> average neighbours, rescale", "shape -> 3 half-size copies"]
    (ax, ay, aw, ah) = panel env
    hx = ax + aw + 30
    hy = 110
    gw = envW env - hx - 30
    gy = hy + 100
    gh = envH env - gy - 64
    lastD = case wDists w of
      (d : _) | not (isNaN d || isInfinite d) -> Just d
      _ -> Nothing
    body = case wMode w of
      1 -> cobweb ax ay aw ah (wXs w)
      2 -> let sc = min aw ah / 5
               cx = ax + aw / 2
               cy = ay + ah / 2
               pt (x, y) = (cx + f2 x * sc, cy + f2 y * sc)
               closed ps = map pt (ps ++ take 1 ps)
           in [ Color (col cSub 40) (thickPath 1.5 (closed (normalizeShape (wShape0 w))))
              , Color (col cTube 100) (thickPath 3 (closed (wCur w))) ]
      _ -> maybe [] (: []) (wImg w)
    info = case wMode w of
      1 -> [ say ("x = " ++ fmt 10 (last' (wX0 w) (wXs w))) hx (hy + 54) 14 cScope
           , say "(fixed point cos x* = x* = 0.7390851332)" hx (hy + 74) 14 cScope ]
      2 -> [ say "Fourier: share of parts other than +-1" hx (hy + 54) 14 cScope
           , say ("  = " ++ showEFloat (Just 2) (nonEllipse (wCur w)) "") hx (hy + 74) 14 cScope ]
      _ -> [ say ("Points " ++ show (U.length (wIfs w)) ++ " (x3 per step, up to 7)") hx (hy + 54) 14 cScope
           , say "distance = Hausdorff distance" hx (hy + 74) 14 cScope ]
    (note1, note2) = case wMode w of
      1 -> ("cos on [0.54, 1]: slope < sin 1 = 0.84", "-> shrinks by x0.84 or less each time")
      2 -> ("Fine wiggles vanish fastest; only the", "ellipse (the slowest part) remains")
      _ -> ("Distance at least halves each step:", "the start is forgotten -> triangle")

-- クモの巣図：y = cos x と y = x、くり返しの道すじ
cobweb :: Float -> Float -> Float -> Float -> [Double] -> [Picture]
cobweb ax ay aw ah xs =
  [ Color (col cSub 40) (Line [(fx (-3.2), fy 0), (fx 3.2, fy 0)])
  , Color (col cSub 40) (Line [(fx 0, fy (-1.6)), (fx 0, fy 1.6)])
  , Color (col cBrass 100) (thickLine 1.5 (fx (-1.6), fy (-1.6)) (fx 1.6, fy 1.6))
  , Color (col cScope 100) (thickPath 2.5 [ (fx v, fy (cos v)) | i <- [0 .. 128 :: Int], let v = -3.2 + 0.05 * fromIntegral i ])
  , Color (col cTube 100) (thickPath 2 path)
  , say "y = cos x (green), y = x (yellow), orange = path (cobweb)" (ax + 10) (ay + 22) 14 cSub ]
  where
    fx v = ax + f2 ((v + 3.2) / 6.4) * aw
    fy v = ay + ah - f2 ((v + 1.6) / 3.2) * ah
    path = case xs of
      (x0 : _) -> (fx x0, fy 0) : concat [ [(fx a, fy b), (fx b, fy b)] | (a, b) <- zip xs (drop 1 xs) ]
      [] -> []

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "123" -> restart w { wMode = fromEnum c - fromEnum '0' }
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> restart w
  EventKey (SpecialKey KeySpace) Down _ _ -> oneIter w
  EventKey (MouseButton LeftButton) Down _ _
    | wMode w == 1 -> if mx < 20 + envW env * 0.58 && my > 90
                        then restart w { wX0 = clampv (-3.2) 3.2 (realToFrac ((mx - 20) / (envW env * 0.58)) * 6.4 - 3.2) }
                        else w
    | otherwise -> w { wDrawing = Just [(mx, my)] }
  EventMotion _ -> case wDrawing w of
    Just d | length d < 4000 -> w { wDrawing = Just ((mx, my) : d) }
    _ -> w
  EventKey (MouseButton LeftButton) Up _ _ -> case wDrawing w of
    Just d | length d > 8 ->
      restart w { wDrawing = Nothing
                , wShape0 = resample [ (realToFrac ((x - envW env * 0.3) / 120), realToFrac ((y - envH env * 0.5) / 120)) | (x, y) <- reverse d ] nP }
    _ -> w { wDrawing = Nothing }
  _ -> w
  where (mx, my) = p5Mouse env

-- 描いた線を、長さにそって等間隔の n 点にする
resample :: [Pt] -> Int -> [Pt]
resample path n = [ pick (fromIntegral k / fromIntegral n * tot) | k <- [0 .. n - 1] ]
  where
    pv = U.fromList path
    m = U.length pv
    lens = U.scanl' (+) 0 (U.zipWith (\(a, b) (c, d) -> sqrt ((c - a) ^ (2 :: Int) + (d - b) ^ (2 :: Int))) pv (U.drop 1 pv))
    tot0 = U.last lens
    tot = if tot0 == 0 then 1 else tot0
    pick s = let j = seek 0
                 lj = lens U.! j
                 dl = lens U.! (j + 1) - lj
                 f = (s - lj) / (if dl == 0 then 1 else dl)
                 (x0, y0) = pv U.! j
                 (x1, y1) = pv U.! (j + 1)
             in (x0 + f * (x1 - x0), y0 + f * (y1 - y0))
      where seek j = if j < m - 2 && lens U.! (j + 1) < s then seek (j + 1) else j

-- ---- 自己テスト（p5.js 版と同じ 5 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("cos 100 times: from x0 = 0, 1, 3, -2 always 0.7390851332 (error < 1e-12)"
    , and [ near (iterate opCos s !! 100) 0.7390851332151607 1e-12 | s <- [0, 1, 3, -2] ])
  , ("from step 2 on, d shrinks every time (contraction), ratio <= sin 1 = 0.841"
    , mono && and [ getAt 0 d (k + 3) <= 0.8415 * v + 1e-15 | (k, v) <- zip [0 ..] (take 18 (drop 2 d)) ])
  , ("average 600 times: any shape -> ellipse (other parts " ++ showEFloat (Just 1) ne "" ++ " < 1e-6)", ne < 1e-6)
  , ("rescale: centroid 0, RMS radius 1"
    , near (sum (map fst q)) 0 1e-12 && near (sqrt (sum [ x * x + y * y | (x, y) <- q ] / fromIntegral nP)) 1 1e-12)
  , ("3 copies: Hausdorff distance at least halves (" ++ joinA (map (fmt 3) h) ++ ")"
    , and [ k == 0 || v <= getAt 0 h (k - 1) / 2 + 1e-12 | (k, v) <- zip [0 :: Int ..] h ]) ]
  where
    xs40 = take 41 (iterate opCos 3)
    d = zipWith (\a b -> abs (b - a)) xs40 (drop 1 xs40)
    mono = and [ not (not (getAt 0 d (k + 1) < getAt 0 d k) && getAt 0 d k > 1e-15) | k <- [2 .. length d - 2] ]
    p600 = iterate opSmooth defaultShape !! 600
    ne = nonEllipse p600
    q = normalizeShape defaultShape
    a0 = U.fromList [ ((x + 1.6) / 3.2, (y + 1.6) / 3.2) | (x, y) <- take 16 defaultShape ]
    h = go (4 :: Int) a0
      where go 0 _ = []
            go k a = let b = opIFS a in hausdorff b a : go (k - 1) b
    joinA [] = ""
    joinA [x] = x
    joinA (x : r) = x ++ " -> " ++ joinA r

-- ---------- やってみよう ----------
-- 1. 1 でクリックして x₀ をいろいろ変えよう。いつも同じ数に落ちつく？ 電卓で cos を押し続けても確かめられる（ラジアンで）
-- 2. 2 でぐちゃぐちゃな形を描いてみよう。最後のだ円の形は、描いた形の何で決まる？
-- 3. 3 で丸・線・文字を描いても、最後は同じ三角形になる？「はじめの形を忘れる」とはどういうこと？
-- 4. ifsT の 3 つの位置を 4 つに変えると、固有形はどうなる？
-- 5. フォン・フェルスターは「ものとは、くり返しの固有のふるまいである」と考えた。言葉や習慣で同じことが起きる例は？

-- ==================================================
-- ここから下は「サイバネティクスの道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で
p5Mouse :: Env -> (Float, Float)
p5Mouse env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 色は (色相, 彩度, 明るさ)。透明度は 0〜100
type HSB = (Float, Float, Float)

cTxt, cSub, cScope, cTube, cBrass, cAlarm :: HSB
cTxt = (40, 12, 96)
cSub = (35, 18, 72)
cScope = (135, 70, 92)
cTube = (24, 85, 100)
cBrass = (42, 55, 88)
cAlarm = (0, 80, 100)

col :: HSB -> Float -> Color
col (h, s, b) a = hsba h s b (a / 100)

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> HSB -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size (col c 100) s))

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- オシロスコープ風の枠
plotFrame :: Float -> Float -> Float -> Float -> String -> Picture
plotFrame x y w h title = Pictures $
  [ Color (hsba 130 40 6 0.9) (rectTL x y w h) ]
  ++ [ Color (hsba 130 40 40 0.4) (Line [(x, y + h * k / 4), (x + w, y + h * k / 4)]) | k <- [1, 2, 3] ]
  ++ [ Color (hsb 42 55 60) (frameTL 1 x y w h) ]
  ++ [ say title (x + 8) (y + 18) 14 cSub | not (null title) ]

-- arr の値を lo〜hi の範囲で、枠 (x, y, w, h) に折れ線で描く（n = 横に並べる点の数）
plotLine :: [Double] -> Float -> Float -> Float -> Float -> Double -> Double -> HSB -> Int -> Float -> Picture
plotLine arr x y w h lo hi c n wt
  | length arr < 2 = Blank
  | otherwise = Color (col c 100) (thickPath wt [ (x + fromIntegral i / fromIntegral (n - 1) * w, y + h - f2 (clampv 0 1 ((fin lo v - lo) / (hi - lo))) * h)
                                              | (i, v) <- zip [0 :: Int ..] arr ])

-- ---- 数の道具
clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- 最後の要素（なければ d）
last' :: a -> [a] -> a
last' d [] = d
last' _ xs = last xs

-- 後ろの n 個
lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

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
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 16) 14 (if all snd rs then cScope else cAlarm)
