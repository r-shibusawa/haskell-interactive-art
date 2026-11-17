{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 48-08 ヘルマン格子ときらめき格子 ─ 受容野を計算で確かめる
-- 白い道の交差点に、ぼんやり灰色の点が見える（見つめた所は消える）。きらめき格子では白い円の中に黒い点がちらつく
-- 目の網膜の細胞は「中心 − まわり」（中心-周辺型の受容野）で反応する。交差点はまわりが明るいぶん反応が弱い
-- 右の画像：左の図に DoG フィルタ（ガウスぼかし 2 つの差）を本当にかけた結果。交差点が暗くなっている
-- 操作：1・2 = ヘルマン / きらめき / ↑↓ = 道の幅 / ←→ = 受容野の大きさ σ / T またはマウスを押す = 真実（画素の値を表示）
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Control.Monad (forM_)
import Control.Exception (evaluate)
import Data.Word (Word8)
import System.Environment (lookupEnv)
import Text.Printf (printf)

nSq :: Int
nSq = 6                      -- ★ 黒い四角の数（1 辺）

res :: Int
res = 2                      -- 右の計算は 2 px を 1 画素にまとめて行う

-- DoG の計算結果（パラメータを変えたときだけ作り直す）
--   dWork：計算を 16 行ずつに分けた仕事の列。1 フレームに少しずつ進めるので、キーを押しても画面が止まらない
data DoG = DoG { dW :: !Int, dV :: U.Vector Double, dImg :: Shape, dWork :: [Double] }

data World = World
  { wWhich  :: !Int            -- 1 = ヘルマン、2 = きらめき
  , wStreet :: !Float          -- ★ 道の幅（ピッチに対する割合）
  , wSigma  :: !Double         -- ★ 受容野の中心の大きさ σ（右の計算用の画素）。まわりは 3 倍
  , wTruth  :: !Bool
  , wDoG    :: DoG             -- いま表示している結果
  , wNext   :: Maybe DoG       -- 計算中の結果（できたら wDoG と入れかえる）
  , wTest   :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "48-08 Hermann grid" (computeNow env1 world0))
  { skBack  = g' 240
  , skStart = \env -> do
      let w = computeNow env world0
          rs = selfTest env w
      printSelfTest rs
      _ <- evaluate (U.sum (dV (wDoG w)))          -- 最初の結果は起動時に計算しておく
      _ <- evaluate (forceImg (dImg (wDoG w)))
      pure w { wTest = rs }
  , skStep  = \_ _ w -> pure (stepDoG w)
  , skDraw  = draw
  , skEvent = event
  }
  where env1 = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: World
world0 = World 1 0.22 2.0 False (DoG 0 U.empty (Box (0, 0, 0) 0 0 0 0) []) Nothing []

-- 図の大きさ：パネルの左上 (x0, y0)、一辺 P、ピッチ、道の幅 st、四角の一辺 sq、右の画像の左 xR
data Geom = Geom { gx0, gy0, gP, gPitch, gSt, gSq, gxR :: !Float }

geom :: Env -> World -> Geom
geom env w = Geom 30 100 p pitch st (pitch - st) (30 + p + 40)
  where
    p = fromIntegral (jsRound (min (envW env * 0.44) (envH env - 170)))
    pitch = p / (fromIntegral nSq + 0.5)
    st = fromIntegral (jsRound (pitch * wStreet w))

-- 図の明るさ（0〜255）：パネルの中の座標 (x, y) → 値。四角・道・交差点の円
lumAt :: Geom -> Int -> Float -> Float -> Int
lumAt gm which x y
  | inSq = 0
  | which == 1 = 255
  | otherwise = if dist2 (u, v) (cxu, cyv) < gSt gm * 0.62 then 255 else 110   -- きらめき格子：灰色の道と、交差点の白い円
  where
    u = x - gSt gm
    v = y - gSt gm
    p = gPitch gm
    iu = floor (u / p) :: Int
    iv = floor (v / p) :: Int
    fu = u - fromIntegral iu * p
    fv = v - fromIntegral iv * p
    inSq = iu >= 0 && iv >= 0 && iu < nSq && iv < nSq && fu < gSq gm && fv < gSq gm
    cxu = fromIntegral (jsRound (u / p)) * p - gSt gm / 2
    cyv = fromIntegral (jsRound (v / p)) * p - gSt gm / 2

-- 交差点と、道の真ん中（となりの交差点との中点）の位置（パネル内の座標）
probes :: Geom -> (Point, Point)
probes gm = ((c, c), (c, c + gPitch gm / 2))
  where c = gSt gm + fromIntegral (nSq `div` 2) * gPitch gm - gSt gm / 2

-- DoG = 中心のガウスぼかし − まわりのガウスぼかし（重みはどちらも合計 1）
gaussKernel :: Double -> U.Vector Double
gaussKernel s = U.map (/ U.sum k) k
  where
    r = ceiling (3 * s) :: Int
    k = U.fromList [ exp (negate (fromIntegral (i * i)) / (2 * s * s)) | i <- [-r .. r] ]

-- 横 → 縦の順にぼかす（はしは端の画素をくり返す）。返すのは結果と、16 行ずつの仕事の列
blurJob :: U.Vector Double -> Int -> Int -> U.Vector Double -> (U.Vector Double, [Double])
blurJob src w h k = (U.concat vs, map U.sum hs ++ map U.sum vs)
  where
    r = (U.length k - 1) `div` 2
    chunks = [ (y0, min h (y0 + 16)) | y0 <- [0, 16 .. h - 1] ]
    hs = [ rows True src a b | (a, b) <- chunks ]
    hFull = U.concat hs
    vs = [ rows False hFull a b | (a, b) <- chunks ]
    -- 行 a〜b−1 を計算する。horiz = True なら横、False なら縦に重みをかけて足す
    rows :: Bool -> U.Vector Double -> Int -> Int -> U.Vector Double
    rows horiz s a b = U.create $ do
      out <- MU.new ((b - a) * w)
      forM_ [a .. b - 1] $ \y -> forM_ [0 .. w - 1] $ \x -> do
        let go :: Int -> Double -> Double
            go !i !acc
              | i > r = acc
              | otherwise =
                  let j = if horiz then y * w + max 0 (min (w - 1) (x + i)) else max 0 (min (h - 1) (y + i)) * w + x
                  in go (i + 1) (acc + U.unsafeIndex k (i + r) * U.unsafeIndex s j)
        MU.unsafeWrite out ((y - a) * w + x) (go (-r) 0)
      pure out

blur :: U.Vector Double -> Int -> Int -> U.Vector Double -> U.Vector Double
blur src w h k = fst (blurJob src w h k)

-- 新しいパラメータで計算を始める（中身は stepDoG で少しずつ進む）
computeDoG :: Env -> World -> World
computeDoG env w = w { wNext = Just (DoG n v im (workC ++ workS ++ [U.sum v, forceImg im])) }
  where
    im = img (gxR gm) (gy0 gm) k n n pix
    gm = geom env w
    n = floor (gP gm / fromIntegral res)
    src = U.generate (n * n) (\j -> let (y, x) = j `divMod` n in fromIntegral (lumAt gm (wWhich w) ((fromIntegral x + 0.5) * fromIntegral res) ((fromIntegral y + 0.5) * fromIntegral res)))
    (c, workC) = blurJob src n n (gaussKernel (wSigma w))
    (s, workS) = blurJob src n n (gaussKernel (wSigma w * 3))
    v = U.zipWith (-) c s
    k = gP gm / fromIntegral n
    pix x y = let q = max 0 (min 255 (round (128 + U.unsafeIndex v (y * n + x) * 1.6) :: Int)) in (q, q, q)

-- 1 フレームに仕事を 4 つずつ進め、終わったら表示を入れかえる
stepDoG :: World -> World
stepDoG w = case wNext w of
  Nothing -> w
  Just d -> case dWork d of
    [] -> w { wDoG = d, wNext = Nothing }
    ws -> let (now, later) = splitAt 4 ws in foldr seq () now `seq` w { wNext = Just d { dWork = later } }

-- すぐに全部計算する（起動時・自己テスト用）
computeNow :: Env -> World -> World
computeNow env w = case wNext w' of
  Just d -> w' { wDoG = d, wNext = Nothing }
  Nothing -> w'
  where w' = computeDoG env w

dogAt :: DoG -> Point -> Double
dogAt d (x, y) =
  let i = floor (y / fromIntegral res) * dW d + floor (x / fromIntegral res)
  in if i >= 0 && i < U.length (dV d) then dV d U.! i else 0

-- ---- 図：黒い四角（と、きらめき格子の白い円）、右に DoG の画像、赤・青の輪、上下の帯
scene :: Env -> World -> Bool -> [Shape]
scene env w truth =
  [ Box (0, 0, 0) (r (gx0 gm + gSt gm + fromIntegral i * gPitch gm)) (r (gy0 gm + gSt gm + fromIntegral j * gPitch gm)) (r (gSq gm)) (r (gSq gm))
  | i <- [0 .. nSq - 1], j <- [0 .. nSq - 1] ]
  ++ (if wWhich w == 2
        then [ Dot (255, 255, 255) (gx0 gm + gSt gm + fromIntegral i * gPitch gm - gSt gm / 2) (gy0 gm + gSt gm + fromIntegral j * gPitch gm - gSt gm / 2) (gSt gm * 0.62)
             | i <- [0 .. nSq], j <- [0 .. nSq] ]
        else [])
  ++ [ dImg (wDoG w) ]
  ++ concat [ Circ c 2 (gxR gm + px) (gy0 gm + py) 9 : [ Circ c 2 (gx0 gm + px) (gy0 gm + py) 9 | truth ]
            | ((px, py), c) <- [(cross, (230, 40, 40)), (street, (40, 110, 230))] ]
  ++ [ Box (240, 240, 240) 0 0 (envW env) 86, Box (240, 240, 240) 0 (envH env - 60) (envW env) 60 ]
  where
    gm = geom env w
    (cross, street) = probes gm
    r = fromIntegral . jsRound

bgOf :: World -> RGB
bgOf w = g (if wWhich w == 1 then 255 else 110)

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (col (bgOf w)) (rect (envW env / 2) (envH env / 2) (envW env) (envH env))
  , render sc
  , txt 20 26 18 (g' 20) ("1/2: " ++ (if wWhich w == 1 then "Hermann grid" else "Scintillating grid") ++ "   Up/Down: street " ++ show (round (gSt gm) :: Int)
                          ++ " px   Left/Right: sigma " ++ printf "%.1f (surround %.1f)" (wSigma w) (3 * wSigma w) ++ "   T: truth")
  , txt 20 50 15 (g' 60) (printf "Right = DoG filter applied to the left picture (retina response). Red = crossing %.1f, blue = street %.1f" rc rs)
  , txt 20 72 15 (g' 60) (printf "-> the crossing responds %.1f weaker" (rs - rc) ++ maybe "" (const "   (computing the new filter ...)") (wNext w)) ]
  ++ (if truth
        then let (a, b) = (readAt cross, readAt street)
             in [ txt 20 (envH env - 36) 18 (g' 20) ("Real pixels of the left picture: crossing " ++ showRGB a ++ "  street " ++ showRGB b ++ (if a == b then "  -> same" else "")) ]
        else [])
  ++ [ txt 20 (envH env - 12) 14 (testColour (wTest w)) (selfTestLine (wTest w)) ]
  where
    truth = wTruth w || envDown env
    gm = geom env w
    sc = scene env w truth
    (cross, street) = probes gm
    rc = dogAt (wDoG w) cross
    rs = dogAt (wDoG w) street
    readAt (px, py) = readPix (bgOf w) sc (jsRound (gx0 gm + px)) (jsRound (gy0 gm + py))

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "12" -> computeDoG env w { wWhich = fromEnum c - fromEnum '0' }
    | c `elem` "tT" -> w { wTruth = not (wTruth w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> computeDoG env w { wStreet = min 0.5 (wStreet w + 0.03) }
  EventKey (SpecialKey KeyDown) Down _ _ -> computeDoG env w { wStreet = max 0.07 (wStreet w - 0.03) }
  EventKey (SpecialKey KeyRight) Down _ _ -> computeDoG env w { wSigma = min 8 (wSigma w + 0.5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> computeDoG env w { wSigma = max 0.5 (wSigma w - 0.5) }
  EventResize _ -> computeDoG env w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 道を太く（↑）すると灰色の点は弱くなる？ 右の「交差点 − 道」の差も小さくなる？
-- 2. σ を道の幅にくらべて大きく・小さくすると、右の画像の交差点の暗さはどう変わる？
-- 3. 交差点を 1 つ見つめると、そこだけ点が消える。目の中心（中心窩）は受容野が小さいから
-- 4. きらめき格子（2）の黒い点は、DoG でも説明できる？ できない部分もある（研究中）
-- 5. nSq を 12 にして、四角を細かくしよう

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 灰色（0〜255）
g :: Int -> RGB
g v = (v, v, v)

g' :: Int -> Color
g' v = rgb255 v v v

col :: RGB -> Color
col (r, gg, b) = rgb255 r gg b

showRGB :: RGB -> String
showRGB (r, gg, b) = "(" ++ show r ++ "," ++ show gg ++ "," ++ show b ++ ")"

-- JavaScript の Math.round と同じ（0.5 は上へ）
jsRound :: Float -> Int
jsRound x = floor (x + 0.5)

-- ==================================================
-- ここから下は「錯視の道具」（画素を読む・自己テスト。中身は読まなくても OK。消さないでね）
-- ==================================================
-- ---- 図は「形のリスト」で作る。同じリストから、画面の絵（render）と「画素を読む」関数（readPix）の両方を作る
--   座標は p5.js と同じ（左上が原点、y は下向き）。色は 0〜255 の整数なので、ぴったり同じ値かどうかを比べられる
type RGB = (Int, Int, Int)

data Shape
  = Box !RGB !Float !Float !Float !Float          -- 左上 (x, y)・幅・高さ の長方形
  | Dot !RGB !Float !Float !Float                 -- 中心 (x, y)・半径 の円
  | Circ !RGB !Float !Float !Float !Float         -- 太さ w の円の線：中心 (x, y)・半径
  | Img !Float !Float !Float !Int !Int (Int -> Int -> RGB) Picture  -- 左上 (x, y)・倍率・幅・高さ の画像（絵は作っておく）

-- 画像の形を作る（絵は 1 回だけ作って覚えておく）
img :: Float -> Float -> Float -> Int -> Int -> (Int -> Int -> RGB) -> Shape
img x y k w h f = Img x y k w h f pic
  where inner = pixelImage w h k (\i j -> let (a, b, c) = f i j in (w8 a, w8 b, w8 c))
        pic = inner `seq` Translate (x + fromIntegral w * k / 2) (y + fromIntegral h * k / 2) (Scale 1 (-1) inner)
        w8 v = fromIntegral (max 0 (min 255 v)) :: Word8

-- 形のリスト → 画面の絵
render :: [Shape] -> Picture
render = Pictures . map one
  where
    one sh = case sh of
      Box c x y w h -> Color (col c) (rect (x + w / 2) (y + h / 2) w h)
      Dot c x y r -> Color (col c) (at (x, y) (disc r))
      Circ c w x y r -> Color (col c) (at (x, y) (ring r w))
      Img _ _ _ _ _ _ pic -> pic

-- 画像の絵を作っておく（仕事の 1 つとして数える）
forceImg :: Shape -> Double
forceImg sh = case sh of
  Img _ _ _ _ _ _ pic -> pic `seq` 0
  _ -> 0

-- 形が点 p をおおっていれば、その色
covers :: Shape -> Point -> Maybe RGB
covers sh p@(px, py) = case sh of
  Box c x y w h -> hit c (px >= x && px < x + w && py >= y && py < y + h)
  Dot c x y r -> hit c (dist2 p (x, y) <= r)
  Circ c w x y r -> hit c (abs (dist2 p (x, y) - r) <= w / 2)
  Img x y k w h f _ ->
    let i = floor ((px - x) / k)
        j = floor ((py - y) / k)
    in if px >= x && py >= y && i < w && j < h then Just (f i j) else Nothing
  where hit c ok = if ok then Just c else Nothing

-- 背景 bg の上に形を順に重ねたときの、点 p の色
colourAt :: RGB -> [Shape] -> Point -> RGB
colourAt bg shs p = foldl' (\d sh -> maybe d id (covers sh p)) bg shs

-- 画素 (x, y) の色（画素の真ん中を読む。p5.js の readPix）
readPix :: RGB -> [Shape] -> Int -> Int -> RGB
readPix bg shs x y = colourAt bg shs (fromIntegral x + 0.5, fromIntegral y + 0.5)

-- ---- 自己テスト：交差点と道が本当に同じ白か（画素）、DoG の計算が正しいか
selfTest :: Env -> World -> [(String, Bool)]
selfTest env w0 =
  [ ("Hermann: crossing " ++ showRGB a ++ " and street middle " ++ showRGB b ++ " are the same white 255", a == b && a == g 255)
  , ("drawn picture = picture used for the filter (lumAt): crossing, street, inside a square"
    , lumAt gm 1 cx cy == 255 && lumAt gm 1 sx sy == 255 && lumAt gm 1 mid mid == 0
      && readPix (bgOf w1) sc1 (jsRound (gx0 gm + mid)) (jsRound (gy0 gm + mid)) == g 0)
  , ("Gaussian weights sum to 1 (centre and surround)", abs (U.sum (gaussKernel 2) - 1) < 1e-6 && abs (U.sum (gaussKernel 6) - 1) < 1e-6)
  , ("DoG of a flat surface is 0 (no response to brightness itself)", U.all (\x -> abs x < 1e-3) d0)
  , (printf "DoG response: crossing %.1f < street middle %.1f" rc rs, rc < rs)
  , ("scintillating: crossing disc is white " ++ show c ++ ", street is grey " ++ show s ++ " (no black dots drawn anywhere)", c == 255 && s == 110) ]
  where
    w1 = computeNow env w0 { wWhich = 1 }
    w2 = computeNow env w0 { wWhich = 2 }
    gm = geom env w0
    ((cx, cy), (sx, sy)) = probes gm
    mid = gSt gm + gSq gm / 2
    sc1 = scene env w1 False
    sc2 = scene env w2 False
    at1 bg sc (x, y) = readPix bg sc (jsRound (gx0 gm + x)) (jsRound (gy0 gm + y))
    a = at1 (bgOf w1) sc1 (cx, cy)
    b = at1 (bgOf w1) sc1 (sx, sy)
    (c, _, _) = at1 (bgOf w2) sc2 (cx, cy)
    (s, _, _) = at1 (bgOf w2) sc2 (sx, sy)
    flat = U.replicate 400 200
    d0 = U.zipWith (-) (blur flat 20 20 (gaussKernel 1.5)) (blur flat 20 20 (gaussKernel 4.5))
    rc = dogAt (wDoG w1) (cx, cy)
    rs = dogAt (wDoG w1) (sx, sy)

-- 結果を端末に出す（テスト中は 1 行のまとめと失敗した項目だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(s, ok) -> putStrLn ((if ok then "OK  " else "NG  ") ++ s)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if not (null rs) && all snd rs then "OK " else "NG ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

testColour :: [(String, Bool)] -> Color
testColour rs = if not (null rs) && all snd rs then rgb255 30 130 60 else rgb255 200 40 40
