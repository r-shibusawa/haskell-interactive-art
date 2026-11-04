-- ============================================
-- 39-15 ウェブカメラの射影補正（スキャン）
-- 映像の中の紙・本・画面の 4 隅を選ぶと、それを正面から見た長方形に直す（書類スキャンアプリと同じ仕組み）
--   出力の各画素 (u, v) → ホモグラフィ H で映像の座標 (x, y) → その色を取ってくる（逆ホモグラフィで画素をサンプル）
--   映像は小さく縮めて（320×240）計算するので 60 fps で動く。カメラが使えないときは代わりのチェッカー画像
-- 映像の 4 隅をドラッグ / 1 = 出力を A4 の比率 / 2 = 正方形 / 3 = 横長 / R = 隅をリセット（カメラの許可が必要）
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Word (Word8)
import Numeric (showFFloat)

vw, vh :: Int
vw = 320       -- ★ 映像の大きさ（小さいほど速い）
vh = 240

vwF, vhF :: Float
vwF = fromIntegral vw
vhF = fromIntegral vh

unitSquare :: [Point]
unitSquare = [(0, 0), (1, 0), (1, 1), (0, 1)]

data World = World
  { wCam     :: !(Maybe Camera)
  , wLive    :: !Bool           -- 本物のカメラ映像が来ているか
  , wFrame   :: !B.ByteString   -- 最新の映像（RGB）
  , wCorners :: ![Point]        -- 映像の中の 4 隅（映像の px 座標）
  , wDrag    :: !Int
  , wOW, wOH :: !Int            -- ★ 出力の大きさ（px）
  }

corners0 :: [Point]
corners0 = [(vwF * 0.25, vhF * 0.2), (vwF * 0.78, vhF * 0.15), (vwF * 0.85, vhF * 0.85), (vwF * 0.2, vhF * 0.8)]

main :: IO ()
main = runSketch (sketch "39-15 Webcam perspective correction" world0)
  { skBack  = hsb 30 35 11
  , skStart = \_ -> do
      cam <- startCamera vw vh
      pure world0 { wCam = Just cam }
  , skStep  = step
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

world0 :: World
world0 = World Nothing False B.empty corners0 (-1) 236 334

-- 映像の表示位置 (x, y, 倍率)
videoRect :: Env -> (Float, Float, Float)
videoRect env = (30, 110, min (envW env * 0.55 / vwF) ((envH env - 160) / vhF))

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let (vx, vy, s) = videoRect env
      (mx, my) = mouseP5 env
      w = if wDrag w0 >= 0
            then w0 { wCorners = setAt (wDrag w0) (clampF 0 (vwF - 1) ((mx - vx) / s), clampF 0 (vhF - 1) ((my - vy) / s)) (wCorners w0) }
            else w0
  case wCam w of
    Nothing -> pure w
    Just cam -> do
      live <- camLive cam
      frame <- if live then cameraFrame cam else pure B.empty
      pure w { wLive = live && B.length frame >= vw * vh * 3, wFrame = frame }

-- カメラが使えないときの絵：ななめに置いたチェッカーの 4 隅
fakeCorners :: Env -> [Point]
fakeCorners env =
  let t = envTime env * 0.4
  in [(80 + 20 * sin t, 60), (250, 40 + 15 * cos t), (270, 200), (60, 190)]

fakeColor :: Int -> Int -> Color
fakeColor i j = if odd (i + j) then hsb 45 70 95 else hsb 0 0 15

-- 代わりの絵を色のかたまり（四角形）として描く
fakePicture :: Env -> Picture
fakePicture env = case homographyFrom4 unitSquare (fakeCorners env) of
  Nothing -> background
  Just hf -> Pictures $ background :
    [ fillQuad (fakeColor i j) c
    | i <- [0 .. 5], j <- [0 .. 7]
    , let p a b = applyH hf (fromIntegral a / 6, fromIntegral b / 8)
    , Just c <- [sequence [p i j, p (i + 1) j, p (i + 1) (j + 1), p i (j + 1)]] ]
  where background = fillRect (hsb 220 30 30) 0 0 vwF vhF

rgbOf :: Color -> (Word8, Word8, Word8)
rgbOf c = let (r, g, b, _) = rgbaOfColor c in (q r, q g, q b)
  where q v = fromIntegral (round (v * 255) :: Int)

-- 映像の座標 (x, y) の色を返す関数を作る
sampler :: Env -> World -> Int -> Int -> (Word8, Word8, Word8)
sampler env w
  | wLive w = \x y ->
      let f = wFrame w
          o = (x + y * vw) * 3
      in (B.index f o, B.index f (o + 1), B.index f (o + 2))
  | otherwise = case homographyFrom4 unitSquare (fakeCorners env) >>= mat3Inv of
      Nothing -> \_ _ -> backRGB
      Just (M3 (H3 a b c) (H3 d e f) (H3 g h i)) -> \x y ->
        let px = fromIntegral x
            py = fromIntegral y
            q = g * px + h * py + i
            u = (a * px + b * py + c) / q
            v = (d * px + e * py + f) / q
        in if q /= 0 && u >= 0 && u < 1 && v >= 0 && v < 1
             then if odd (floor (u * 6) + floor (v * 8) :: Int) then cellA else cellB
             else backRGB
  where
    backRGB = rgbOf (hsb 220 30 30)
    cellA = rgbOf (fakeColor 1 0)
    cellB = rgbOf (fakeColor 0 0)

draw :: Env -> World -> Picture
draw env w = view env $
  -- 左：映像と 4 隅
  [ case (wLive w, wCam w) of
      (True, Just cam) -> Translate (vx + vwF * s / 2) (vy + vhF * s / 2) (Scale 1 (-1) (camImage cam (wFrame w) s))
      _ -> Translate vx vy (Scale s s (fakePicture env))
  , closedPath 2 (hsb 45 90 100) sc ]
  ++ concat [ [dotAt (hsb 45 90 100) p 18, txt (fst p - 4) (snd p + 5) 14 (hsb 0 0 10) (show i)] | (i, p) <- zip [1 :: Int ..] sc ]
  -- 右：出力
  ++ [ maybe Blank output mh
     , strokeRect (hsba 0 0 100 0.6) ox vy (fromIntegral ow * k) (fromIntegral oh * k)
     , txt ox (vy - 10) 14 white ("Output " ++ show ow ++ "x" ++ show oh ++ "  (1: A4  2: square  3: wide)")
     , txt vx (vy - 10) 14 white (if wLive w then "Camera" else "No camera: substitute picture (checker)")
     , txt 20 28 18 white "Drag the 4 corners of paper or a book (1 2 3 4 = TL TR BR BL)   R: reset corners"
     , txt 20 50 14 (gray 0.65) "Each output pixel is mapped by the homography to the video and takes that color (no holes this way)."
     , txt 20 70 14 (gray 0.65) (case mh of
         Just (M3 _ _ (H3 a b _)) -> "H row 3 = [" ++ nf 3 (d2f a) ++ ", " ++ nf 3 (d2f b) ++ ", 1]"
         Nothing -> "") ]
  where
    (vx, vy, s) = videoRect env
    ow = wOW w
    oh = wOH w
    sc = [(vx + x * s, vy + y * s) | (x, y) <- wCorners w]
    ox = vx + vwF * s + 40
    maxW = envW env - ox - 30
    maxH = envH env - 160
    k = min (maxW / fromIntegral ow) (maxH / fromIntegral oh)
    -- ★ 出力の正規化座標 (u, v) ∈ [0,1]² → 映像の座標。H は「出力 → 映像」の向き（逆ホモグラフィ）
    mh = homographyFrom4 unitSquare (wCorners w)
    sample = sampler env w
    output (M3 (H3 a b c) (H3 d e f) (H3 g h i)) =
      let fw = fromIntegral ow :: Double
          fh = fromIntegral oh :: Double
          dark' = (40, 40, 40) :: (Word8, Word8, Word8)
          px x y =
            let u = fromIntegral x / fw
                v = fromIntegral y / fh
                q = g * u + h * v + i
                sx = (a * u + b * v + c) / q
                sy = (d * u + e * v + f) / q
            in if q > 0 && sx >= 0 && sx < fromIntegral vw && sy >= 0 && sy < fromIntegral vh
                 then sample (floor sx) (floor sy)
                 else dark'
      in Translate (ox + fromIntegral ow * k / 2) (vy + fromIntegral oh * k / 2)
           (Scale 1 (-1) (pixelImage ow oh k px))

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (vx, vy, s) = videoRect env
        sc = [(vx + x * s, vy + y * s) | (x, y) <- wCorners w]
    in w { wDrag = nearestHandle sc (mouseP5 env) 30 }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventKey (Char '1') Down _ _ -> w { wOW = 236, wOH = 334 }
  EventKey (Char '2') Down _ _ -> w { wOW = 280, wOH = 280 }
  EventKey (Char '3') Down _ _ -> w { wOW = 342, wOH = 228 }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> w { wCorners = corners0 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 名刺・本の表紙・スマホの画面をななめに映して、4 隅を合わせる。文字が読めるようになる？
-- 2. 4 隅の順番を 1→2→3→4 でなく回転させると出力はどうなる？ 裏返し（1↔2）にすると？
-- 3. 最近傍（floor）でなく、まわり 4 画素の平均（バイリニア）にすると滑らかになる。重くなる？
-- 4. 逆に「正面の絵をななめに貼る」（AR 風）：H の向きを逆にして、映像の上に自分の絵を貼りつけてみよう

-- ==================================================
-- ここから下は「道具」：p5.js と同じ座標（左上が原点、y は下向き）で描く
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- 図全体を「左上が原点、y は下向き」の座標に置く
view :: Env -> [Picture] -> Picture
view env ps = Translate (negate (envW env) / 2) (envH env / 2) (Scale 1 (-1) (Pictures ps))

-- gloss の座標 → p5.js の座標
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

mouseP5 :: Env -> Point
mouseP5 env = toP5 env (envMouse env)

-- 文字（(x, y) が左下。y が下向きなので文字だけ裏返して戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 塗りつぶした丸（d は直径。p5.js の circle と同じ）
dotAt :: Color -> Point -> Float -> Picture
dotAt c p d = Color c (at p (disc (d / 2)))

-- 輪（d は直径、w は線の太さ）
ringAt :: Color -> Point -> Float -> Float -> Picture
ringAt c p d w
  | w <= 1 = Color c (at p (Circle (d / 2)))
  | otherwise = Color c (at p (ring (d / 2) w))

-- 線分（太さ 1 以下は細い線）
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b
  | w <= 0 = Blank
  | w <= 1 = Color c (Line [a, b])
  | otherwise = Color c (thickLine w a b)

-- 閉じた折れ線
closedPath :: Float -> Color -> [Point] -> Picture
closedPath _ _ [] = Blank
closedPath w c ps@(p : rest) = Pictures (zipWith (seg w c) ps (rest ++ [p]))

-- 左上 (x, y)、幅 w、高さ h の長方形
fillRect :: Color -> Float -> Float -> Float -> Float -> Picture
fillRect c x y w h = Color c (Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)])

strokeRect :: Color -> Float -> Float -> Float -> Float -> Picture
strokeRect c x y w h = closedPath 1 c [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- 四角形を塗る。gloss の Polygon はへこんだ形を正しく塗れないので、
-- へこみ・ねじれ（8 の字）のときは三角形に分ける
fillQuad :: Color -> [Point] -> Picture
fillQuad col [a, b, c, d]
  | sameSign [o a b c, o b c d, o c d a, o d a b] = Color col (Polygon [a, b, c, d])
  | o a b c * o a c d > 0 = tris [[a, b, c], [a, c, d]]
  | o b c d * o b d a > 0 = tris [[b, c, d], [b, d, a]]
  | otherwise = case (crossAt a b c d, crossAt b c d a) of
      (Just x, _) -> tris [[x, b, c], [x, d, a]]
      (_, Just x) -> tris [[a, b, x], [c, d, x]]
      _ -> Blank
  where
    o (ax, ay) (bx, by) (cx, cy) = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
    sameSign vs = all (> 0) vs || all (< 0) vs
    tris ts = Color col (Pictures (map Polygon ts))
    -- 線分 pq と線分 rs の交点
    crossAt (px, py) (qx, qy) (rx, ry) (sx, sy) =
      let dx1 = qx - px; dy1 = qy - py; dx2 = sx - rx; dy2 = sy - ry
          den = dx1 * dy2 - dy1 * dx2
          t = ((rx - px) * dy2 - (ry - py) * dx2) / den
          u = ((rx - px) * dy1 - (ry - py) * dx1) / den
      in if abs den < 1e-6 || t < 0 || t > 1 || u < 0 || u > 1 then Nothing
         else Just (px + t * dx1, py + t * dy1)
fillQuad _ _ = Blank

-- リストの i 番目を置きかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]

-- 小数 d 桁の文字列（p5.js の nf に相当）
nf :: Int -> Float -> String
nf d v
  | isNaN v || isInfinite v = "-"
  | d <= 0 = show (round v :: Int)
  | otherwise = showFFloat (Just d) v ""

-- ==================================================
-- ここから下は「射影幾何の道具」（同次座標・外積・行列・ホモグラフィ）
-- 小さな差（行列式 ≈ 0 など）を見るので、計算は Double で行う
-- ==================================================

-- 同次座標の点 [x, y, w]、または直線 [a, b, c]（ax + by + cw = 0）
data H3 = H3 !Double !Double !Double

f2d :: Float -> Double
f2d = realToFrac

d2f :: Double -> Float
d2f = realToFrac

-- w = 1 のふつうの点
hPt :: Point -> H3
hPt (x, y) = H3 (f2d x) (f2d y) 1

-- 外積。2 点 → それを通る直線 ／ 2 直線 → その交点。どちらも同じ式！
hCross :: H3 -> H3 -> H3
hCross (H3 a b c) (H3 d e f) = H3 (b * f - c * e) (c * d - a * f) (a * e - b * d)

-- 内積。点と直線で 0 なら「点が直線の上にある」
hDot :: H3 -> H3 -> Double
hDot (H3 a b c) (H3 d e f) = a * d + b * e + c * f

-- 3 つの点（または直線）の行列式。0 なら共線（共点）
hDet :: H3 -> H3 -> H3 -> Double
hDet p q r = hDot p (hCross q r)

-- 長さを 1 にそろえる（表示用。射影的には同じ点・同じ直線）
hUnit :: H3 -> H3
hUnit p@(H3 a b c) =
  let n = sqrt (hDot p p)
      m = if n == 0 then 1 else n
  in H3 (a / m) (b / m) (c / m)

-- 無限遠（または画面からはるか遠く）にあるか
isFar :: H3 -> Bool
isFar (H3 x y w) = abs w * 1e5 < sqrt (x * x + y * y)

-- 同次座標 → 画面の点（無限遠なら Nothing）
toXY :: H3 -> Maybe Point
toXY p@(H3 x y w)
  | isFar p || w == 0 = Nothing
  | otherwise = Just (d2f (x / w), d2f (y / w))

-- 2 点を通る直線
lineThrough :: Point -> Point -> H3
lineThrough a b = hCross (hPt a) (hPt b)

-- 1 直線上の 4 点の複比 (P1,P2;P3,P4)。a, b はその直線上の基準 2 点（無限遠点があっても計算できる）
crossRatioH :: H3 -> H3 -> H3 -> H3 -> H3 -> H3 -> Maybe Double
crossRatioH p1 p2 p3 p4 a b =
  let n = hCross a b
      nn = let v = hDot n n in if v == 0 then 1 else v
      lm p = (hDot (hCross p b) n / nn, hDot (hCross a p) n / nn)
      (l1, m1) = lm p1
      (l2, m2) = lm p2
      (l3, m3) = lm p3
      (l4, m4) = lm p4
      num = (l1 * m3 - l3 * m1) * (l2 * m4 - l4 * m2)
      den = (l2 * m3 - l3 * m2) * (l1 * m4 - l4 * m1)
  in if abs den < 1e-12 then Nothing else Just (num / den)

-- 枠（左, 上, 右, 下）
type Frame = (Float, Float, Float, Float)

screenBox :: Env -> Frame
screenBox env = (0, 0, envW env, envH env)

-- 無限遠点の矢印を置く枠（上の説明文と下の文字にかからないようにする）
arrowBox :: Env -> Frame
arrowBox env = (0, 90, envW env, envH env - 30)

-- 直線 [a, b, c] が枠を横切る両端
hLineEnds :: Frame -> H3 -> Maybe (Point, Point)
hLineEnds (x0, y0, x1, y1) l =
  let edges = [H3 1 0 (f2d (-x0)), H3 1 0 (f2d (-x1)), H3 0 1 (f2d (-y0)), H3 0 1 (f2d (-y1))]
      ok (x, y) = x > x0 - 1 && x < x1 + 1 && y > y0 - 1 && y < y1 + 1
      cand = [p | e <- edges, Just p <- [toXY (hCross l e)], ok p]
      pairs = [(p, q) | (i, p) <- zip [0 :: Int ..] cand, (j, q) <- zip [0 ..] cand, i < j]
  in case pairs of
       [] -> Nothing
       _ -> Just (maximumBy (comparing (uncurry dist2)) pairs)

-- 直線を枠いっぱいに描く
hLine :: Float -> Color -> Frame -> H3 -> Picture
hLine w c box l = maybe Blank (uncurry (seg w c)) (hLineEnds box l)

-- 点を描く。ふつうの点なら丸、無限遠点なら枠のふちに「方向の矢印」（向かい合う 2 か所 = 同じ 1 点）
hPoint :: Frame -> Float -> String -> Color -> Color -> Float -> H3 -> Picture
hPoint (x0, y0, x1, y1) r lab fillC strokeC sw p@(H3 hx hy _) = case toXY p of
  Just q@(qx, qy) -> Pictures
    [ dotAt fillC q r
    , if sw > 0 then ringAt strokeC q r sw else Blank
    , if null lab then Blank else txt (qx + r * 0.7) (qy - r * 0.7) 15 strokeC lab ]
  Nothing
    | m == 0 -> Blank
    | otherwise -> Pictures (concatMap arrowAt [1, -1])
  where
    m = sqrt (hx * hx + hy * hy)
    dx = d2f (hx / m)
    dy = d2f (hy / m)
    cx = (x0 + x1) / 2
    cy = (y0 + y1) / 2
    arrowAt s =
      let ux = dx * s
          uy = dy * s
          ts = [(x1 - 30 - cx) / ux | ux > 1e-9] ++ [(x0 + 30 - cx) / ux | ux < -1e-9]
            ++ [(y1 - 30 - cy) / uy | uy > 1e-9] ++ [(y0 + 30 - cy) / uy | uy < -1e-9]
          t = foldr min 1e6 ts
          x = cx + ux * t
          y = cy + uy * t
          rot (u, v) = (x + u * ux - v * uy, y + u * uy + v * ux)
          tip = rot (r, 0)
      in [ seg sw strokeC (rot (-r * 2.5, 0)) tip
         , seg sw strokeC tip (rot (0, -r * 0.8))
         , seg sw strokeC tip (rot (0, r * 0.8))
         , if null lab then Blank else txt (x - r * 2) (y - r) 15 strokeC (lab ++ " inf") ]

-- 3×3 行列（行を 3 つ並べたもの）の道具
data M3 = M3 !H3 !H3 !H3

mat3Vec :: M3 -> H3 -> H3
mat3Vec (M3 a b c) v = H3 (hDot a v) (hDot b v) (hDot c v)

mat3T :: M3 -> M3
mat3T (M3 (H3 a b c) (H3 d e f) (H3 g h i)) = M3 (H3 a d g) (H3 b e h) (H3 c f i)

mat3Mul :: M3 -> M3 -> M3
mat3Mul (M3 a b c) m =
  let M3 p q r = mat3T m
      row v = H3 (hDot v p) (hDot v q) (hDot v r)
  in M3 (row a) (row b) (row c)

mat3Inv :: M3 -> Maybe M3
mat3Inv (M3 (H3 a b c) (H3 d e f) (H3 g h i)) =
  let ca = e * i - f * h
      cb = negate (d * i - f * g)
      cc = d * h - e * g
      det = a * ca + b * cb + c * cc
  in if abs det < 1e-12 then Nothing else Just $ M3
       (H3 (ca / det) (negate (b * i - c * h) / det) ((b * f - c * e) / det))
       (H3 (cb / det) ((a * i - c * g) / det) (negate (a * f - c * d) / det))
       (H3 (cc / det) (negate (a * h - b * g) / det) ((a * e - b * d) / det))

-- ガウスの消去法（部分ピボット）で A x = b を解く。解けなければ Nothing
solveLinear :: [[Double]] -> [Double] -> Maybe [Double]
solveLinear a b = go 0 (zipWith (\r v -> r ++ [v]) a b)
  where
    n = length b
    go c m
      | c >= n = Just (zipWith (\i r -> (r !! n) / (r !! i)) [0 ..] m)
      | abs (m !! p !! c) < 1e-12 = Nothing
      | otherwise = go (c + 1) [if i == c then pivot else elim i r | (i, r) <- zip [0 ..] swapped]
      where
        p = maximumBy (comparing (\r -> abs (m !! r !! c))) [c .. n - 1]
        swapped = setAt c (m !! p) (setAt p (m !! c) m)
        pivot = swapped !! c
        elim i r =
          let f = (r !! c) / (pivot !! c)
          in zipWith3 (\k x y -> if k < c then x else x - f * y) [0 :: Int ..] r pivot

-- 4 点の対応 src[i] → dst[i] からホモグラフィ 3×3 を求める（h33 = 1 に固定、8 本の連立方程式）
homographyFrom4 :: [Point] -> [Point] -> Maybe M3
homographyFrom4 src dst =
  let rows = concat (zipWith eqs (map dbl src) (map dbl dst))
      dbl (x, y) = (f2d x, f2d y)
      eqs (x, y) (u, v) =
        [ ([x, y, 1, 0, 0, 0, -u * x, -u * y], u)
        , ([0, 0, 0, x, y, 1, -v * x, -v * y], v) ]
  in case solveLinear (map fst rows) (map snd rows) of
       Just [h0, h1, h2, h3, h4, h5, h6, h7] -> Just (M3 (H3 h0 h1 h2) (H3 h3 h4 h5) (H3 h6 h7 1))
       _ -> Nothing

-- ホモグラフィで点を写す（無限遠へ飛ぶなら Nothing）
applyH :: M3 -> Point -> Maybe Point
applyH h p = toXY (mat3Vec h (hPt p))

-- つかめる点のリストからマウスに近いものの番号。半径 r 以内になければ -1
nearestHandle :: [Point] -> Point -> Float -> Int
nearestHandle ps m r = fst (foldl pick (-1, r) (zip [0 ..] ps))
  where pick (bi, bd) (i, p) = let d = dist2 p m in if d < bd then (i, d) else (bi, bd)
