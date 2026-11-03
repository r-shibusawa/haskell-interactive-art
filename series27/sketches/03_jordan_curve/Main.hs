-- ============================================
-- 03 ジョルダン曲線定理
-- 自分と交わらない閉じた曲線は、平面を「内側」と「外側」のちょうど 2 つに分ける（当たり前に見えて証明はむずかしい）
--   ある点が内側かどうかは、そこから半直線を出して曲線と交わる回数を数えればわかる：奇数 → 内側、偶数 → 外側
-- ドラッグ = 閉じた曲線を描く / クリック = 調べる点を置く / ←→ = 半直線の向き / 1 = 迷路のようなうずまき / 2 = ぐにゃぐにゃの円 / F = 内側を塗る / C = 消す
-- ============================================
module Main (main) where

import Art
import Data.Array (accumArray, assocs)
import Data.List (sort, sortOn)

fillRes :: Float
fillRes = 6          -- ★ 塗りの細かさ（px）

maxDraw :: Int
maxDraw = 3000       -- 描く曲線の点の数の上限

width, height :: Float
width = 1280
height = 800

type P = (Float, Float)

data World = World
  { wCurve :: ![P]              -- 閉じた曲線（折れ線）
  , wDrawing :: ![P]            -- 描いている途中の線（新しい順）
  , wDrawN :: !Int
  , wIsDrawing :: !Bool
  , wProbe :: !P                -- 調べる点
  , wAngle :: !Float            -- ★ 半直線の向き（ラジアン）
  , wShowFill :: !Bool
  , wFill :: Picture            -- 内側の塗り。曲線を変えたときに作り直す（必要になるまで計算されない）
  }

main :: IO ()
main = runSketch (sketch "27-03 Jordan Curve Theorem" world0)
  { skBack  = hsb 30 25 12
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = setCurve spiralPreset (World [] [] 0 False (width / 2 + 20, height / 2 + 30) 0 False Blank)

setCurve :: [P] -> World -> World
setCurve c w = w { wCurve = c, wFill = makeFill c }

-- | うずまきの帯（外側のうずと内側のうずをつないだ 1 本の閉曲線）
spiralPreset :: [P]
spiralPreset =
  let cx = width / 2
      cy = height / 2 + 30
      a = min width height * 0.03
      b = min width height * 0.022
      gap = b * 2.6
      ts = takeWhile (<= 5 * pi) [fromIntegral k * 0.05 | k <- [0 :: Int ..]]
      outPts = [ (cx + r * cos t, cy + r * sin t) | t <- ts, let r = a + b * t ]
      inPts = [ (cx + (r + gap) * cos t, cy + (r + gap) * sin t) | t <- ts, let r = a + b * t ]
  in outPts ++ reverse inPts

blobPreset :: [P]
blobPreset =
  let cx = width / 2
      cy = height / 2 + 30
      rr = min width height * 0.3
      ts = takeWhile (< tau) [fromIntegral k * 0.02 | k <- [0 :: Int ..]]
  in [ (cx + r * cos t, cy + r * sin t) | t <- ts, let r = rr * (1 + 0.35 * sin (3 * t) + 0.2 * sin (7 * t + 1)) ]

-- | ★ 半直線（点 p から角度 a の方向）と曲線の交点をすべて求める。結果は (x, y, 距離 t)
rayHits :: [P] -> P -> Float -> [(Float, Float, Float)]
rayHits curve (px, py) a =
  let dx = cos a
      dy = sin a
      hit ((ax, ay), (bx, by))
        | abs den < 1e-9 = []
        | t > 0 && u >= 0 && u < 1 = [(px + dx * t, py + dy * t, t)]
        | otherwise = []
        where
          ex = bx - ax
          ey = by - ay
          den = dx * ey - dy * ex
          t = ((ax - px) * ey - (ay - py) * ex) / den      -- 半直線の上の位置
          u = ((ax - px) * dy - (ay - py) * dx) / den      -- 線分の上の位置（0〜1）
  in concatMap hit (zip curve (rotate1 curve))

---------------------------------------------------------------- 動き

stepWorld :: Env -> World -> World
stepWorld env w =
  let d1 = if isDown env (SpecialKey KeyLeft) then -0.02 else 0
      d2 = if isDown env (SpecialKey KeyRight) then 0.02 else 0
  in w { wAngle = wAngle w + d1 + d2 }

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    w { wDrawing = [toP5 pos], wDrawN = 1, wIsDrawing = True }
  EventMotion pos | wIsDrawing w ->
    let m = toP5 pos
    in case wDrawing w of
         (l : _) | dist2 m l > 4 && wDrawN w < maxDraw -> w { wDrawing = m : wDrawing w, wDrawN = wDrawN w + 1 }
         _ -> w
  EventKey (MouseButton LeftButton) Up _ pos ->
    let m@(_, my) = toP5 pos
        w1 | wDrawN w > 10 = setCurve (reverse (wDrawing w)) w
           | my > 90 = w { wProbe = m }           -- 短いドラッグ ＝ クリック
           | otherwise = w
    in w1 { wIsDrawing = False, wDrawing = [], wDrawN = 0 }
  EventKey (Char '1') Down _ _ -> setCurve spiralPreset w
  EventKey (Char '2') Down _ _ -> setCurve blobPreset w
  EventKey (Char c) Down _ _
    | c `elem` "fF" -> w { wShowFill = not (wShowFill w) }
    | c `elem` "cC" -> setCurve [] w
  _ -> w

---------------------------------------------------------------- 描画

drawWorld :: World -> Picture
drawWorld w =
  let curve = wCurve w
      hasCurve = length (take 3 curve) > 2
      curvePic = Color (hsb 30 80 100) (closed 3 curve)
      drawingPic = if wIsDrawing w && wDrawN w > 1
                     then Color (hsb 50 60 100) (thickPath 3 (map pt (wDrawing w)))
                     else Blank
      probePic
        | not hasCurve = Blank
        | otherwise =
            let (px, py) = wProbe w
                a = wAngle w
                hits = sortOn (\(_, _, t) -> t) (rayHits curve (px, py) a)
                n = length hits
                isIn = odd n
                col = hsb (if isIn then 140 else 0) 70 100
                far = max width height * 1.5
                info = "Crossings " ++ show n ++ " -> " ++ (if isIn then "odd: INSIDE" else "even: OUTSIDE")
            in Pictures $
                 [ Color col (thickLine 2 (pt (px, py)) (pt (px + cos a * far, py + sin a * far))) ]
                 ++ concat [ [ at (pt (x, y)) (Color (hsb 50 90 100) (disc 9))
                             , let (gx, gy) = pt (x, y) in labelC gx gy 14 black (show k) ]
                           | (k, (x, y, _)) <- zip [1 :: Int ..] hits ]
                 ++ [ at (pt (px, py)) (Color col (disc 8))
                    , txt 20 (height - 40) 26 col info ]
  in Pictures
       [ if wShowFill w then wFill w else Blank
       , if hasCurve then curvePic else Blank
       , drawingPic
       , probePic
       , txt 20 40 16 (gray 1) ("Drag: draw curve   Click: probe   <- ->: ray angle   1: spiral   2: blob   F: fill "
                                ++ (if wShowFill w then "ON" else "off") ++ "   C: clear")
       , txt 20 68 14 (gray 0.6) "Is the spiral's center inside? Count crossings instead of tracing. Turning the ray keeps odd/even."
       ]

-- | 内側を塗る：マスの中心が内側かを偶奇で調べる（p5.js の inside() を全部のマスで呼ぶのと同じ結果を、行ごとにまとめて求める）
makeFill :: [P] -> Picture
makeFill curve = Color (hsba 140 50 45 0.7) (Pictures
  [ rect gx gy (fromIntegral (i1 - i0) * fillRes) fillRes
  | (r, xs) <- assocs table
  , (a, b) <- pairs (sort xs)
  , let i0 = ceiling (a / fillRes - 0.5) :: Int       -- 中心が a より右の最初のマス
  , let i1 = ceiling (b / fillRes - 0.5) :: Int       -- 中心が b より右の最初のマス
  , i1 > i0
  , let (gx, gy) = pt ((fromIntegral i0 + fromIntegral i1) / 2 * fillRes, (fromIntegral r + 0.5) * fillRes) ])
  where
    rows = ceiling (height / fillRes) - 1 :: Int
    hits = concatMap edgeHits (zip curve (rotate1 curve))
    edgeHits ((ax, ay), (bx, by))
      | ay == by = []
      | otherwise =
          let lo = max 0 (ceiling (min ay by / fillRes - 0.5)) :: Int
              hi = min rows (ceiling (max ay by / fillRes - 0.5) - 1) :: Int
          in [ (r, ax + (bx - ax) * (y - ay) / (by - ay)) | r <- [lo .. hi], let y = (fromIntegral r + 0.5) * fillRes ]
    table = accumArray (flip (:)) [] (0, rows) hits
    pairs (a : b : more) = (a, b) : pairs more
    pairs _ = []

-- ---------- やってみよう ----------
-- 1. うずまきの巻き数（5 * pi）を増やして、中心が内側か外側かを予想してから点を置いてみよう
-- 2. 自分と交わる「8 の字」を描くと、内側・外側はどうなる？（偶奇規則は「交わり方」で答えを決める）
-- 3. 半直線が曲線の角（頂点）をちょうど通るとき、数え間違えないための工夫を考えよう
-- 4. 偶奇の判定は塗りつぶしソフトの「バケツ」や、ゲームの当たり判定でも使われている。マウスが内側なら色を変える遊びをつくろう

-- ==================================================
-- ここから下は「道具」（座標の変換など）
-- ==================================================

pt :: P -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

toP5 :: Point -> P
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = let (gx, gy) = pt (x, y) in label gx gy s c str

rotate1 :: [a] -> [a]
rotate1 [] = []
rotate1 (x : xs) = xs ++ [x]

closed :: Float -> [P] -> Picture
closed w ps = thickPath w (map pt (ps ++ take 1 ps))
