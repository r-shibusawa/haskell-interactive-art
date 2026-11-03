-- ============================================
-- 00 ゴムの幾何学：同相
-- トポロジーでは図形をゴムのように伸ばす・曲げる・縮めるのは OK、「切る」「貼る」はダメ
--   穴の数が同じ図形どうしは、なめらかに変形できる ＝「同相」。コーヒーカップとドーナツは同じ形！
-- 0 / 1 / 2 = 穴の数のグループ / ←→ = となりの形へ変形 / スペース = 自動で次々変形 / ドラッグ = ゴムを引っぱる
-- ============================================
module Main (main) where

import Art
import Data.Array (accumArray, assocs, listArray, (!))
import Data.List (foldl', sort)
import Data.Maybe (fromMaybe)

nPts :: Int
nPts = 160               -- ★ 輪郭 1 本あたりの点の数

morphSpeed, pullSigma :: Float
morphSpeed = 0.018       -- ★ 変形の速さ
pullSigma = 0.12         -- ★ 引っぱるときに動く範囲（形の大きさを 1 として）

width, height, sS, ox, oy :: Float
width = 1280
height = 800
sS = min (width * 0.5) (height * 0.72)   -- 形を描く枠の大きさ
ox = 60                                  -- 枠の左上
oy = (height - sS) / 2 + 30

type P = (Float, Float)

-- | 形 ＝ 外側の輪郭と、穴の輪郭のリスト
data Shape = Shape { sOuter :: [P], sHoles :: [[P]] }

-- | 引っぱり（形の座標）
data Pull = Pull { plX, plY, plDx, plDy, plLife :: !Float, plDrag :: !Bool }

data World = World
  { wGi, wSi :: !Int            -- いまのグループと形の番号
  , wFrom, wTo :: !Shape
  , wT :: !Float                -- 変形の進み具合（0〜1）
  , wAuto :: !Bool
  , wTimer :: !Int
  , wPulls :: ![Pull]
  , wMsg :: !String
  , wMsgLife :: !Int
  }

main :: IO ()
main = runSketch (sketch "27-00 Rubber Geometry: Homeomorphism" world0)
  { skBack  = bgCol
  , skStep  = \env _ w -> pure (stepWorld (toP5 (envMouse env)) w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

bgCol :: Color
bgCol = hsb 250 30 12

world0 :: World
world0 = let s0 = shapeAt 1 0 in World 1 0 s0 s0 1 False 0 [] "" 0

---------------------------------------------------------------- 動き

goTo :: Int -> Int -> World -> World
goTo g s w
  | g /= wGi w =
      let s0 = shapeAt g 0
      in w { wMsg = "Holes " ++ show (wGi w) ++ " -> " ++ show g ++ ": impossible with rubber. You must cut or glue!"
           , wMsgLife = 180, wGi = g, wSi = 0, wFrom = s0, wTo = s0, wT = 1 }
  | otherwise =
      let n = length (groupOf g)
          si = (s + n) `mod` n
      in w { wSi = si, wFrom = currentRings w, wTo = shapeAt g si, wT = 0 }

-- | いま画面にある形（変形の途中）＝ 2 つの形の点を混ぜる
currentRings :: World -> Shape
currentRings w =
  let t = wT w
      e = t * t * (3 - 2 * t)      -- なめらかに始まってなめらかに止まる
      mixRing = zipWith (\(ax, ay) (bx, by) -> (ax + (bx - ax) * e, ay + (by - ay) * e))
  in Shape (mixRing (sOuter (wFrom w)) (sOuter (wTo w))) (zipWith mixRing (sHoles (wFrom w)) (sHoles (wTo w)))

stepWorld :: P -> World -> World
stepWorld (mx, my) w0 =
  let w1 = if wT w0 < 1 then w0 { wT = min 1 (wT w0 + morphSpeed) } else w0
      w2 | wAuto w1 && wT w1 >= 1 =
             if wTimer w1 + 1 > 50 then goTo (wGi w1) (wSi w1 + 1) w1 { wTimer = 0 } else w1 { wTimer = wTimer w1 + 1 }
         | otherwise = w1
      upd p | plDrag p = p { plDx = (mx - ox) / sS - plX p, plDy = (my - oy) / sS - plY p }
            | otherwise = p { plLife = plLife p * 0.93 }     -- 離すとゴムのように戻る
      pulls = filter (\p -> plLife p > 0.01) (map upd (wPulls w2))
  in w2 { wPulls = pulls, wMsgLife = max 0 (wMsgLife w2 - 1) }

-- | ★ 引っぱり ＝ ガウス関数の形のなめらかなずらし（連続なので形のつながりは変わらない）
warp :: [Pull] -> P -> P
warp pulls (px, py) = foldl' add (px, py) pulls
  where
    add (x, y) q =
      let dx = px - plX q
          dy = py - plY q
          k = exp (-(dx * dx + dy * dy) / (pullSigma * pullSigma)) * plLife q
      in (x + plDx q * k, y + plDy q * k)

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "012" -> goTo (fromEnum c - fromEnum '0') 0 w
  EventKey (SpecialKey KeyRight) Down _ _ -> goTo (wGi w) (wSi w + 1) w
  EventKey (SpecialKey KeyLeft) Down _ _ -> goTo (wGi w) (wSi w - 1) w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w), wTimer = 0 }
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 pos
        x = (mx - ox) / sS
        y = (my - oy) / sS
    in if x > -0.2 && x < 1.2 && y > -0.2 && y < 1.2
         then w { wPulls = wPulls w ++ [Pull x y 0 0 1 True] }
         else w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wPulls = map (\p -> p { plDrag = False }) (wPulls w) }
  _ -> w

---------------------------------------------------------------- 描画

drawWorld :: World -> Picture
drawWorld w =
  let r = currentRings w
      gi = wGi w
      scr = shapeToScreen (wPulls w)
      name = groupNames gi (wSi w)
      white100 = gray 1
      gray60 = gray 0.6
  in Pictures $
       [ Color (hsb 250 20 30) (let (x, y) = pt (ox + sS / 2, oy + sS / 2) in rectLine x y sS sS)
       -- ★ 形を描く：外側の輪郭を塗って、穴を背景色で抜く
       , scanFill (hsb (30 + fromIntegral gi * 60) 70 95) (scr (sOuter r)) ]
       ++ [scanFill bgCol (scr h) | h <- sHoles r]
       ++ [Color (hsba 0 0 100 0.7) (closedPath 2 (scr h)) | h <- sOuter r : sHoles r]
       -- 引っぱっている所
       ++ [ Color (hsba 50 90 100 (0.8 * plLife p))
              (thickLine 2 (pt (ox + plX p * sS, oy + plY p * sS))
                           (pt (ox + (plX p + plDx p * plLife p) * sS, oy + (plY p + plDy p * plLife p) * sS)))
          | p <- wPulls w ]
       ++ [ txt ox (oy - 22) 36 white100 name
          , txt 20 40 16 white100 ("0 1 2: hole group (now " ++ show gi ++ ")   <- ->: morph to next   Space: auto " ++ (if wAuto w then "ON" else "off") ++ "   Drag: pull")
          , txt 20 68 14 gray60 "Same group (same number of holes) = can morph without cutting = homeomorphic. Only connections count."
          ]
       ++ [ txt ox (oy + sS + 40) 18 (hsba 0 80 100 (min 100 (fromIntegral (wMsgLife w)) / 100)) (wMsg w) | wMsgLife w > 0 ]
       ++ [drawAlphabet gi (ox + sS + 60) (oy + 10)]

shapeToScreen :: [Pull] -> [P] -> [P]
shapeToScreen pulls = map (\p -> let (x, y) = warp pulls p in (ox + x * sS, oy + y * sS))

-- | アルファベット（太い文字＝面として）を穴の数で分類
drawAlphabet :: Int -> Float -> Float -> Picture
drawAlphabet gi x0 y0 =
  let holesOf c | c == 'B' = 2 :: Int
                | c `elem` "ADOPQR" = 1
                | otherwise = 0
      colW = max 40 (min 56 ((width - x0 - 30) / 7))
      letter i =
        let ch = toEnum (65 + i) :: Char
            h = holesOf ch
            on = h == gi
            col = hsb (30 + fromIntegral h * 60) (if on then 80 else 30) (if on then 100 else 55)
            x = x0 + fromIntegral (i `mod` 7) * colW
            y = y0 + 60 + fromIntegral (i `div` 7) * colW * 1.05
        in Pictures [txt (x + d) y (colW * 0.7) col [ch] | d <- [0, 0.8, 1.6]]   -- 太字のかわりに少しずらして重ねる
      legend h =
        let x = x0 + fromIntegral h * 120
            y = y0 + 80 + 4 * colW * 1.05
            (gx, gy) = pt (x + 6, y - 6)
            c = hsb (30 + fromIntegral h * 60) 70 95
        in Pictures [Color c (rect gx gy 12 12), txt (x + 18) y 14 c (show (h :: Int) ++ (if h == 1 then " hole" else " holes"))]
      yE = y0 + 110 + 4 * colW * 1.05
  in Pictures $
       [txt x0 y0 14 (gray 1) "Letters by number of holes (thick letter = surface)"]
       ++ map letter [0 .. 25]
       ++ map legend [0, 1, 2]
       ++ [ txt x0 yE 14 (gray 0.6) "As thin lines the classes are finer:"
          , txt x0 (yE + 22) 14 (gray 0.6) "T and I differ (T has a branch point)." ]

-- ---------- やってみよう ----------
-- 1. buildShapes に自分の形（たとえば穴 1 つの「Q」や「R」）を追加して、ドーナツから変形させよう
-- 2. 取っ手 2 つのカップと「8」が同相なのを確かめる。穴 3 つのグループ（プレッツェル）もつくれる？
-- 3. pullSigma を小さくして思いきり引っぱると、輪郭が重なる。それは「同相写像」と言える？
-- 4. 文字を「線」と見たとき、X と Y、E と F は同相？ 枝分かれ点の数を数えてみよう

-- ==================================================
-- ここから下は「道具」（座標の変換、塗りつぶし、形をつくる）
-- ==================================================

-- | p5.js の座標（左上が原点、y は下向き）→ gloss の座標
pt :: P -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

-- | gloss の座標 → p5.js の座標
toP5 :: Point -> P
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = let (gx, gy) = pt (x, y) in label gx gy s c str

closedPath :: Float -> [P] -> Picture
closedPath _ [] = Blank
closedPath w ps@(p0 : _) = thickPath w (map pt (ps ++ [p0]))

-- | へこみのある多角形の塗りつぶし（gloss の Polygon は凸形しか正しく塗れない）。
--   横の細い帯ごとに辺との交点を求め、奇数番目から偶数番目の交点の間を塗る。
scanFill :: Color -> [P] -> Picture
scanFill _ [] = Blank
scanFill c ps@(p0 : ps') = Color c (Pictures [ span1 r a b | (r, xs) <- assocs table, (a, b) <- pairs (sort xs) ])
  where
    step = 2 :: Float
    rows = floor (height / step) - 1 :: Int
    edges = zip ps (ps' ++ [p0])
    hits = concatMap edgeHits edges
    edgeHits ((ax, ay), (bx, by))
      | ay == by = []
      | otherwise =
          let lo = max 0 (ceiling (min ay by / step - 0.5)) :: Int
              hi = min rows (ceiling (max ay by / step - 0.5) - 1) :: Int
          in [ (r, ax + (bx - ax) * (y - ay) / (by - ay)) | r <- [lo .. hi], let y = (fromIntegral r + 0.5) * step ]
    table = accumArray (flip (:)) [] (0, rows) hits
    span1 r a b =
      let (gx, gy) = pt ((a + b) / 2, (fromIntegral r + 0.5) * step)
      in rect gx gy (b - a) step
    pairs (a : b : more) = (a, b) : pairs more
    pairs _ = []

groups :: [[(String, Shape)]]
groups = shapeTable

groupOf :: Int -> [(String, Shape)]
groupOf g = fromMaybe [] (nth g groups)

shapeAt :: Int -> Int -> Shape
shapeAt g s = maybe (Shape [] []) snd (nth s (groupOf g))

groupNames :: Int -> Int -> String
groupNames g s = maybe "" fst (nth s (groupOf g))

nth :: Int -> [a] -> Maybe a
nth i xs = case drop i xs of
  (x : _) | i >= 0 -> Just x
  _ -> Nothing

arcPts :: Float -> Float -> Float -> Float -> Float -> Float -> Int -> [P]
arcPts cx cy rx ry a0 a1 n =
  [ (cx + rx * cos a, cy + ry * sin a)
  | i <- [0 .. n], let a = a0 + (a1 - a0) * fromIntegral i / fromIntegral n ]

hPi :: Float
hPi = pi / 2

cup :: Bool -> ([P], [[P]])
cup two =
  let o = [(0.3, 0.22), (0.66, 0.22), (0.66, 0.32)] ++ arcPts 0.66 0.5 0.2 0.18 (-hPi) hPi 30 ++ [(0.66, 0.85), (0.3, 0.85)]
          ++ (if two then arcPts 0.3 0.5 0.2 0.18 hPi (3 * hPi) 30 else [])
      h1 = (0.66, 0.42) : arcPts 0.66 0.5 0.1 0.08 (-hPi) hPi 20
      h2 = (0.3, 0.58) : arcPts 0.3 0.5 0.1 0.08 hPi (3 * hPi) 20
  in (o, if two then [h2, h1] else [h1])

mk :: String -> ([P], [[P]]) -> (String, Shape)
mk n (o, hs) = (n, Shape (resample o) (map resample hs))

shapeTable :: [[(String, Shape)]]
shapeTable =
  [ [ mk "Disk" (arcPts 0.5 0.5 0.42 0.42 0 tau 60, [])
    , mk "I" ([(0.4, 0.06), (0.6, 0.06), (0.6, 0.94), (0.4, 0.94)], [])
    , mk "L" ([(0.25, 0.06), (0.45, 0.06), (0.45, 0.74), (0.8, 0.74), (0.8, 0.94), (0.25, 0.94)], [])
    , mk "T" ([(0.1, 0.06), (0.9, 0.06), (0.9, 0.26), (0.6, 0.26), (0.6, 0.94), (0.4, 0.94), (0.4, 0.26), (0.1, 0.26)], [])
    , mk "C" (arcPts 0.5 0.5 0.44 0.44 0.7 (tau - 0.7) 40 ++ arcPts 0.5 0.5 0.24 0.24 (tau - 0.7) 0.7 30, [])
    ]
  , [ mk "Donut" (arcPts 0.5 0.5 0.45 0.45 0 tau 60, [arcPts 0.5 0.5 0.17 0.17 0 tau 40])
    , mk "Coffee cup" (cup False)
    , mk "O" (arcPts 0.5 0.5 0.36 0.45 0 tau 60, [arcPts 0.5 0.5 0.18 0.27 0 tau 40])
    , mk "D" ((0.22, 0.08) : arcPts 0.5 0.5 0.42 0.42 (-hPi) hPi 40 ++ [(0.22, 0.92)],
              [(0.4, 0.26) : arcPts 0.5 0.5 0.24 0.24 (-hPi) hPi 30 ++ [(0.4, 0.74)]])
    , mk "P" ([(0.2, 0.95), (0.2, 0.05)] ++ arcPts 0.5 0.3 0.25 0.25 (-hPi) hPi 30 ++ [(0.4, 0.55), (0.4, 0.95)],
              [(0.4, 0.18) : arcPts 0.5 0.3 0.12 0.12 (-hPi) hPi 20 ++ [(0.4, 0.42)]])
    , mk "A" ([(0.1, 0.95), (0.42, 0.05), (0.58, 0.05), (0.9, 0.95), (0.7, 0.95), (0.62, 0.72), (0.38, 0.72), (0.3, 0.95)],
              [[(0.5, 0.28), (0.58, 0.55), (0.42, 0.55)]])
    ]
  , [ mk "8" (eight, [arcPts 0.5 0.28 0.1 0.1 0 tau 30, arcPts 0.5 0.72 0.1 0.1 0 tau 30])
    , mk "B" ((0.2, 0.05) : arcPts 0.5 0.26 0.21 0.21 (-hPi) hPi 30 ++ arcPts 0.5 0.71 0.24 0.24 (-hPi) hPi 30 ++ [(0.2, 0.95)],
              [ (0.38, 0.16) : arcPts 0.5 0.26 0.1 0.1 (-hPi) hPi 20 ++ [(0.38, 0.36)]
              , (0.38, 0.58) : arcPts 0.5 0.71 0.13 0.13 (-hPi) hPi 20 ++ [(0.38, 0.84)] ])
    , mk "Two-handled cup" (cup True)
    ]
  ]
  where
    eight = [ (0.5 + 0.32 * sin t * (0.45 + 0.55 * c * c), 0.5 - 0.46 * c)
            | i <- [0 .. 79 :: Int], let t = fromIntegral i / 80 * tau, let c = cos t ]

-- | 輪郭を弧長で N 等分し、向き（時計回り）と始点（いちばん上の点）をそろえる
resample :: [P] -> [P]
resample [] = []
resample raw =
  let area = sum (zipWith (\(ax, ay) (bx, by) -> ax * by - bx * ay) raw (rotate1 raw))
      ps = if area < 0 then reverse raw else raw
      m = length ps
      arr = listArray (0, m - 1) ps
      segLens = zipWith dist2 ps (rotate1 ps)
      lens = scanl (+) 0 segLens
      lenArr = listArray (0, m) lens
      total = lenArr ! m
      -- d は増えていくので j は戻らない
      go _ [] = []
      go j (d : ds)
        | j < m - 1 && lenArr ! (j + 1) < d = go (j + 1) (d : ds)
        | otherwise =
            let (ax, ay) = arr ! j
                (bx, by) = arr ! ((j + 1) `mod` m)
                den = lenArr ! (j + 1) - lenArr ! j
                u = (d - lenArr ! j) / (if den == 0 then 1 else den)
            in (ax + (bx - ax) * u, ay + (by - ay) * u) : go j ds
      out = go (0 :: Int) [fromIntegral k / fromIntegral nPts * total | k <- [0 .. nPts - 1]]
      top = fst (foldl' (\(bi, by) (i, (_, y)) -> if y < by - 1e-6 then (i, y) else (bi, by))
                        (0 :: Int, maybe 0 snd (nth 0 out)) (zip [0 ..] out))
  in drop top out ++ take top out

rotate1 :: [a] -> [a]
rotate1 [] = []
rotate1 (x : xs) = xs ++ [x]
