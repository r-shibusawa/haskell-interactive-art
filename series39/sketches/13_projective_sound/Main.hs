-- ============================================
-- 39-13 複比を聴く
-- 直線 l 上の 4 点を O から直線 m に射影する（04 と同じ図）。点の「位置」を音程にすると射影で音は変わるが、
--   「複比」を音程にすると射影しても同じ音 ─ 不変量は「同じ音」として聞こえる
--   スペース：l の 4 点の音 → m の 4 点の音（ちがう） → l の複比の音 と m の複比の音（同じ）
-- スペース = 再生 / H = 複比の音を鳴らし続ける（ドラッグしながら聴く） / 点・O・直線の端をドラッグ / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (maximumBy)
import Data.Ord (comparing)
import Numeric (showFFloat)

tt0 :: [Float]
tt0 = [0.15, 0.4, 0.55, 0.85]    -- ★ l 上の 4 点の位置

baseF, humPeriod :: Float
baseF = 220       -- ★ 音の基準（Hz）
humPeriod = 0.3   -- ★ H の音をくり返す間隔（秒）

-- 再生する音：周波数、長さ、どの棒か（'l', 'm', 'c'）、番号
data Note = Note !Float !Float !Char !Int

data World = World
  { wLa, wLb, wMa, wMb, wO :: !Point
  , wTT    :: ![Float]
  , wDrag  :: !Int
  , wQueue :: ![Note]      -- これから鳴らす音
  , wCur   :: !(Maybe Note)   -- いま鳴っている音
  , wPlay  :: !Bool
  , wNext  :: !Float       -- 次の音の時刻
  , wHum   :: !Bool
  , wHumAt :: !Float
  }

main :: IO ()
main = runSketch (sketch "39-13 Hearing the cross-ratio" (mk (1, 0) (2, 0) (0, 1) (1, 1) (0, 0)))
  { skBack  = hsb 30 35 11
  , skStart = \env -> pure (resetAll env (mk (1, 0) (2, 0) (0, 1) (1, 1) (0, 0)))
  , skStep  = step
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

mk :: Point -> Point -> Point -> Point -> Point -> World
mk a b c d o = World a b c d o tt0 (-1) [] Nothing False 0 False 0

resetAll :: Env -> World -> World
resetAll env w =
  let sw = envW env
      sh = envH env
  in w { wLa = (sw * 0.1, sh * 0.72), wLb = (sw * 0.9, sh * 0.72), wMa = (sw * 0.18, sh * 0.42)
       , wMb = (sw * 0.82, sh * 0.22), wO = (sw * 0.5, sh * 0.1), wTT = tt0 }

onL :: World -> Float -> Point
onL w t = let (ax, ay) = wLa w; (bx, by) = wLb w in (ax + (bx - ax) * t, ay + (by - ay) * t)

handles :: World -> [Point]
handles w = map (onL w) (wTT w) ++ [wO w, wLa w, wLb w, wMa w, wMb w]

-- ★ 位置 → 音程：直線の端 a からの距離（px）を半音に
posFreq :: Float -> Float
posFreq d = baseF * 2 ** clampF 0 2.5 (d / 300)

-- ★ 複比 → 音程：|複比| の対数を半音に（複比が同じなら同じ音）
crFreq :: Maybe Double -> Float
crFreq Nothing = baseF
crFreq (Just cr) = baseF * 2 ** clampF 0 3 (logBase 2 (abs (d2f cr) + 1e-6) * 0.5 + 1)

-- 図から計算する値をまとめる
data Calc = Calc
  { cPs  :: [Point]
  , cQs  :: [H3]            -- 射影した点
  , cCrL :: Maybe Double
  , cCrM :: Maybe Double
  , cFL  :: [Float]
  , cFM  :: [Float]
  }

calc :: World -> Calc
calc w =
  let ps = map (onL w) (wTT w)
      m = lineThrough (wMa w) (wMb w)
      qs = map (\p -> hCross (lineThrough (wO w) p) m) ps
      cr xs a b = case xs of
        [p1, p2, p3, p4] -> crossRatioH p1 p2 p3 p4 (hPt a) (hPt b)
        _ -> Nothing
  in Calc ps qs (cr (map hPt ps) (wLa w) (wLb w)) (cr qs (wMa w) (wMb w))
       (map (posFreq . dist2 (wLa w)) ps)
       (map (maybe (baseF * 8) (posFreq . dist2 (wMa w)) . toXY) qs)

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let now = envTime env
      w = drag env w0
      c = calc w
  -- 再生
  w1 <- if wPlay w && now >= wNext w
          then case wQueue w of
            n@(Note f dur _ _) : rest -> do
              playTone (clampF 40 4000 f) 0.3 dur Triangle
              pure w { wQueue = rest, wCur = Just n, wNext = now + dur + 0.08 }
            [] -> pure w { wPlay = False, wCur = Nothing }
          else pure w
  -- 鳴らしっぱなしの代わりに、短い音をくり返す
  if wHum w1 && now >= wHumAt w1
    then do
      playTone (clampF 40 4000 (crFreq (cCrL c))) 0.12 (humPeriod + 0.05) Triangle
      pure w1 { wHumAt = now + humPeriod }
    else pure w1

drag :: Env -> World -> World
drag env w
  | i < 0 = w
  | i < 4 =
      let (ax, ay) = wLa w
          (bx, by) = wLb w
          vx = bx - ax
          vy = by - ay
          l2 = let v = vx * vx + vy * vy in if v == 0 then 1 else v
      in w { wTT = setAt i (clampF (-0.3) 1.3 (((mx - ax) * vx + (my - ay) * vy) / l2)) (wTT w) }
  | i == 4 = w { wO = m }
  | i == 5 = w { wLa = m }
  | i == 6 = w { wLb = m }
  | i == 7 = w { wMa = m }
  | otherwise = w { wMb = m }
  where
    i = wDrag w
    m@(mx, my) = mouseP5 env

draw :: Env -> World -> Picture
draw env w = view env $
  -- 図
  [ hLine 2 (hsba 200 60 95 0.7) full (lineThrough (wLa w) (wLb w))
  , hLine 2 (hsba 15 75 95 0.7) full (lineThrough (wMa w) (wMb w)) ]
  ++ [ seg 1 (hsba 45 80 100 0.35) o (px + (px - ox) * 3, py + (py - oy) * 3) | (px, py) <- cPs c ]
  ++ concat [ [ dotAt (hsb 200 60 100) p (if isOn 'l' i then 26 else 14)
              , txt (fst p - 5) (snd p + 28) 16 white n
              , hPoint (arrowBox env) (if isOn 'm' i then 26 else 14) (n ++ "'") (hsb 15 75 100) (hsb 15 75 100) 2 q ]
            | (i, p, q, n) <- zip4' [0 ..] (cPs c) (cQs c) ["A", "B", "C", "D"] ]
  ++ [ dotAt (hsb 45 90 100) o 16, txt (ox + 12) (oy - 8) 16 white "O" ]
  ++ [ dotAt (gray 0.6) h 8 | h <- [wLa w, wLb w, wMa w, wMb w] ]
  -- 音の列（ピアノロール風）
  ++ [ fillRect (hsba 0 0 100 0.08) (bx - 16) (by - 30) (bw + 32) (bh + 50)
     , txt bx (by - 8) 14 white "Pitch: l ABCD | m A'B'C'D' | cross-ratio" ]
  ++ concat [ let x = bx + fromIntegral k * 44 + (if k >= 4 then 10 else 0) + (if k >= 8 then 10 else 0)
                  hgt = clampF 0 1 (logBase 2 (f / baseF) / 3) * bh
                  hue = case who of { 'l' -> 200; 'm' -> 15; _ -> 45 }
              in [ fillRect (hsb hue 70 (if isOn who i then 100 else 70)) x (by + bh - hgt) 32 hgt
                 , txt x (by + bh + 16) 14 white (nf 0 f) ]
            | (k, (f, who, i)) <- zip [0 :: Int ..] bars ]
  ++ [ txt 20 28 18 white ("Space: play   H: cross-ratio tone " ++ (if wHum w then "ON" else "off") ++ "   R: reset   Cross-ratio l = " ++ fmt (cCrL c) ++ "  m = " ++ fmt (cCrM c))
     , txt 20 50 14 (gray 0.8) ("Tones " ++ nf 0 (crFreq (cCrL c)) ++ " Hz / " ++ nf 0 (crFreq (cCrM c)) ++ " Hz (same).   Drag points, O and line ends.")
     , txt 20 70 14 (gray 0.65) "Blue/red tones change; yellow (cross-ratio) stays." ]
  where
    full = screenBox env
    c = calc w
    o@(ox, oy) = wO w
    bx = envW env - 470
    by = 110
    bw = 440
    bh = 130
    isOn who i = case wCur w of
      Just (Note _ _ a b) -> a == who && b == i
      Nothing -> False
    bars = zipWith (\f i -> (f, 'l', i)) (cFL c) [0 ..] ++ zipWith (\f i -> (f, 'm', i)) (cFM c) [0 ..]
        ++ [(crFreq (cCrL c), 'c', 0), (crFreq (cCrM c), 'c', 1)]

zip4' :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
zip4' _ _ _ _ = []

fmt :: Maybe Double -> String
fmt Nothing = "-"
fmt (Just v) = if abs v > 1e4 then "inf" else nf 3 (d2f v)

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w { wDrag = nearestHandle (handles w) (mouseP5 env) 26 }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventKey (SpecialKey KeySpace) Down _ _ ->
    let c = calc w
        notes = zipWith (\f i -> Note f 0.25 'l' i) (cFL c) [0 ..]
             ++ zipWith (\f i -> Note f 0.25 'm' i) (cFM c) [0 ..]
             ++ [Note (crFreq (cCrL c)) 0.6 'c' 0, Note (crFreq (cCrM c)) 0.6 'c' 1]
    in w { wQueue = notes, wCur = Nothing, wPlay = True, wNext = envTime env + 0.3 }
  EventKey (Char ch) Down _ _
    | ch `elem` "hH" -> w { wHum = not (wHum w), wHumAt = envTime env }
    | ch `elem` "rR" -> resetAll env w
  _ -> w

-- ---------- やってみよう ----------
-- 1. O を動かすと青の音は変わらず赤の音だけ変わる。黄は両方変わらない。なぜ青は変わらない？
-- 2. crFreq を「複比そのもの × 100 Hz」に変えてみる。複比が負のときはどうする？
-- 3. 4 点の順番を変えた複比（1−λ, 1/λ…）を和音にして鳴らす（6 種類の値）
-- 4. 調和列点（複比 −1、09）のとき決まった音が鳴る「調和アラーム」を作ろう

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
