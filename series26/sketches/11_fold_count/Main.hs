-- ============================================
-- 11 折り紙で数を数える：紙を何回折れる？
-- 半分に折るたびに厚さは 2 倍：n 回で 2ⁿ 枚。0.1 mm の紙でも 42 回で月に届く計算
--   でも実際は折り目の丸みで長さが足りなくなる。同じ向きに n 回折るのに必要な長さ（ギャリバンの式）
--   L = (π t / 6)(2ⁿ + 4)(2ⁿ − 1)
-- ←→ = 折る回数 n / スライダー（ドラッグ）= 紙の厚さ t・長さ L / ↑↓ = 厚さ / Z・X = 長さ
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl')
import Numeric (showFFloat)

data World = World
  { wN     :: !Int
  , wLogT  :: !Double     -- ★ 紙の厚さ（m）の log。0.1 mm
  , wLogL  :: !Double     -- ★ 紙の長さ（m）の log。A4 の長い辺
  , wDragS :: !Int        -- 動かしているスライダー（-1 = なし）
  }

marks :: [(Double, String)]
marks =
  [ (0.1, "Palm"), (1.7, "Person"), (333, "Tokyo Tower"), (3776, "Mt. Fuji"), (8849, "Everest")
  , (4e5, "Space Station (400 km)"), (3.844e8, "Moon (384,000 km)"), (1.496e11, "Sun (150 million km)") ]

logTmin, logTmax, logLmin, logLmax :: Double
logTmin = logBase 10 0.02e-3
logTmax = logBase 10 1e-3
logLmin = -1
logLmax = 4

main :: IO ()
main = runSketch (sketch "26-11 How Many Folds?" (World 4 (logBase 10 0.1e-3) (logBase 10 0.297) (-1)))
  { skBack  = rgb255 34 32 40
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ ギャリバンの式：同じ向きに n 回折るのに必要な長さ
needLength :: Double -> Int -> Double
needLength t k = let nn = 2 ^^ k in (pi * t / 6) * (nn + 4) * (nn - 1)

maxFolds :: Double -> Double -> Int
maxFolds t l = go 0
  where go k | k < 60 && needLength t (k + 1) <= l = go (k + 1)
             | otherwise = k

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let n = wN w
      wd = envW env
      ht = envH env
      t = 10 ** wLogT w
      l = 10 ** wLogL w
      twoN = 2 ^^ n :: Double
      thick = t * twoN
      need = needLength t n
      nmax = maxFolds t l
      ok = need <= l
      -- 左：断面図
      bx = 170
      by = ht * 0.62
      bw = max 120 (wd * 0.45 - 340)    -- 左右に折り目の丸みのぶん余白
      (rcx, rcy) = sc env (bx + bw / 2, by - 140)
      stack
        | n <= 7 = [drawStack env n bx by bw (min 10 (280 / realToFrac twoN))]
        | otherwise =
            [ Color (rgb255 246 240 226) (rect rcx rcy bw 280)
            , txt env (bx + 12) (by - 150) 14 (gray 0.24) (fmtNum twoN ++ " layers")
            , txt env (bx + 12) (by - 128) 14 (gray 0.24) "(too fine to draw)" ]
      leftText =
        [ txt env 60 (by + 40) 20 white ("Layers 2^" ++ show n ++ " = " ++ fmtNum twoN)
        , txt env 60 (by + 70) 20 white ("Thickness " ++ fmtLen thick)
        , txt env 60 (by + 100) 20 (if ok then rgb255 80 220 140 else rgb255 255 110 90) ("Length needed " ++ fmtLen need ++ (if ok then " (enough)" else " (not enough!)"))
        , txt env 60 (by + 130) 20 (rgb255 255 190 40) ("Max folds for this paper: " ++ show nmax)
        ]
      -- 右：厚さのグラフ（対数）
      gx = wd * 0.45
      gy = ht - 90
      gw = wd * 0.5
      gh = ht - 230
      ymin = -5 :: Double
      ymax = 12 :: Double
      yOf v = gy - gh * realToFrac ((logBase 10 v - ymin) / (ymax - ymin))
      xOf :: Int -> Float
      xOf k = gx + gw * fromIntegral k / 50
      box = (gx, gy - gh, gx + gw, gy)
      axes =
        [ Color (gray 0.31) (thickLine 1 (sc env (gx, gy)) (sc env (gx + gw, gy)))
        , Color (gray 0.31) (thickLine 1 (sc env (gx, gy)) (sc env (gx, gy - gh))) ]
        ++ [ Color (gray 0.24) (thickLine 1 (sc env (gx, yOf (10 ** e))) (sc env (gx + gw, yOf (10 ** e)))) | e <- [ymin .. ymax] ]
      markPics = concat
        [ let k = ceiling (logBase 2 (v / t)) :: Int
              y = yOf v
              -- 近い目盛りの文字が重ならないように 2 列に分ける
              lx = gx + (if odd i then 300 else 6)
          in [ Color (rgba255 120 160 220 120) (dashed 1 [6, 6] (sc env (gx, y)) (sc env (gx + gw, y)))
             , txt env lx (y - 4) 14 (rgb255 150 190 255) (name ++ " -> " ++ show k ++ " folds") ]
        | (i, (v, name)) <- zip [0 :: Int ..] marks ]
      -- グラフの枠の中だけ描く
      curve col wid pts = Color col (Pictures
        [ thickLine wid (sc env a') (sc env b')
        | (a, b) <- zip pts (drop 1 pts), Just (a', b') <- [clipSeg box a b] ])
      thickCurve = curve (rgb255 255 190 40) 2.5 [(xOf k, yOf (t * 2 ^^ k)) | k <- [0 .. 50]]
      needCurve = curve (rgb255 230 90 70) 2 [(xOf k, yOf (needLength t k)) | k <- [1 .. 50]]
      lenLine = case clipSeg box (gx, yOf l) (gx + gw, yOf l) of
        Just (a, b) -> [Color (rgba255 230 90 70 140) (dashed 1.5 [3, 5] (sc env a) (sc env b))]
        Nothing -> []
      plots =
        [ thickCurve, needCurve ] ++ lenLine ++
        [ Color white (at (sc env (xOf n, yOf thick)) (disc 6))
        , txt env (xOf 36) (yOf (t * 2 ^^ (36 :: Int)) + 20) 14 (rgb255 255 190 40) "Thickness t*2^n"
        , txt env (xOf 16 - 190) (yOf (needLength t 16) - 26) 14 (rgb255 230 90 70) "Length needed L(n)"
        , txt env (gx + gw - 230) (yOf l - 6) 14 (rgba255 230 90 70 200) ("Paper length " ++ fmtLen l)
        , txt env (gx + gw + 8) (gy + 4) 14 (gray 0.67) "n ->"
        ]
        ++ [ txt env (xOf k - 6) (gy + 20) 14 (gray 0.67) (show k) | k <- [0, 10 .. 50] ]
      sliders =
        [ slider env w 0 (ht - 100) ("Paper thickness t = " ++ fmtLen t) ((wLogT w - logTmin) / (logTmax - logTmin))
        , slider env w 1 (ht - 50) ("Paper length L = " ++ fmtLen l) ((wLogL w - logLmin) / (logLmax - logLmin))
        , txt env 20 (ht - 14) 14 (gray 0.59) "In 2002, high-school student Britney Gallivan folded 1.2 km of toilet paper 12 times."
        ]
      hud =
        [ txt env 20 40 18 white ("Left/Right: folds n = " ++ show n ++ "   Sliders: thickness, length (also Up/Down, Z/X)")
        , txt env 20 68 15 (gray 0.67) "Left: side view after n folds (outer folds use up more paper). Right: thickness graph (each line = x10)."
        ]
  in Pictures (stack ++ leftText ++ axes ++ markPics ++ plots ++ sliders ++ hud)

-- 断面図：2ⁿ 本の層を、つながる端で半円でつなぐ（同じ向きに折りつづけたときの重なり方を計算）
drawStack :: Env -> Int -> Float -> Float -> Float -> Float -> Picture
drawStack env k x y w gap =
  let nn = 2 ^ k :: Int
      start = [(fromIntegral i, fromIntegral i + 1, 0) | i <- [0 .. nn - 1]] :: [(Float, Float, Int)]
      foldOnce (segs, wid, maxZ) _ =
        let m = wid / 2
            f s@(x0, x1, z)
              | x0 >= m - 1e-6 && x1 >= m - 1e-6 = (2 * m - x0, 2 * m - x1, 2 * maxZ + 1 - z)
              | otherwise = s
        in (map f segs, m, 2 * maxZ + 1)
      (segsF, _, _) = foldl' foldOnce (start, fromIntegral nn, 0 :: Int) [1 .. k]
      yz z = y - fromIntegral z * gap
      sw = max 1 (gap * 0.4)
      colAt i =
        let u = fromIntegral i / fromIntegral (max 1 (nn - 1)) :: Float
        in makeColor (lerpF 246 226 u / 255) (lerpF 240 84 u / 255) (lerpF 226 72 u / 255) 1
      arcPts cx cy r a0 a1 = [ sc env (cx + r * cos a, cy + r * sin a)
                             | j <- [0 .. 12 :: Int], let a = a0 + (a1 - a0) * fromIntegral j / 12 ]
      one (i, (_, x1, z), next) =
        let c = colAt i
            y1 = yz z
            flat = thickLine sw (sc env (x, y1)) (sc env (x + w, y1))
            joint = case next of
              Nothing -> Blank
              Just (_, _, z2) ->
                let y2 = yz z2
                    r = abs (y1 - y2) / 2
                    my = (y1 + y2) / 2
                in if x1 > 0.5     -- 共有する端が右か左か
                     then thickPath sw (arcPts (x + w) my r (-(pi / 2)) (pi / 2))
                     else thickPath sw (arcPts x my r (pi / 2) (3 * pi / 2))
        in Color c (Pictures [flat, joint])
      nexts = map Just (drop 1 segsF) ++ [Nothing]
  in Pictures (map one (zip3 [0 :: Int ..] segsF nexts))

slider :: Env -> World -> Int -> Float -> String -> Double -> Picture
slider env w i y name v =
  let wd = envW env * 0.3
      x = 60
      px = x + wd * clampF 0 1 (realToFrac v)
  in Pictures
       [ Color (gray 0.35) (thickLine 4 (sc env (x, y)) (sc env (x + wd, y)))
       , Color (if wDragS w == i then rgb255 255 190 40 else white) (at (sc env (px, y)) (disc 9))
       , txt env x (y - 14) 15 (gray 0.86) name
       ]

setSlider :: Env -> Int -> World -> World
setSlider env i w =
  let (mx, _) = mouseXY env
      v = realToFrac (clampF 0 1 ((mx - 60) / (envW env * 0.3))) :: Double
  in if i == 0 then w { wLogT = logTmin + v * (logTmax - logTmin) }
     else w { wLogL = logLmin + v * (logLmax - logLmin) }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseXY env
        lim = 60 + envW env * 0.3 + 10
        hit | abs (my - (envH env - 100)) < 16 && mx < lim = 0
            | abs (my - (envH env - 50)) < 16 && mx < lim = 1
            | otherwise = -1
    in if hit >= 0 then setSlider env hit w { wDragS = hit } else w { wDragS = hit }
  EventMotion _ | wDragS w >= 0 -> setSlider env (wDragS w) w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDragS = -1 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wN = min 50 (wN w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wN = max 0 (wN w - 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wLogT = min logTmax (wLogT w + 0.1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wLogT = max logTmin (wLogT w - 0.1) }
  EventKey (Char c) Down _ _ -> case toLower c of
    'x' -> w { wLogL = min 4 (wLogL w + 0.2) }
    'z' -> w { wLogL = max (-1) (wLogL w - 0.2) }
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 0.1 mm の紙を何回折れば富士山・月に届く？ グラフの目盛りで読もう（倍々の速さ = 指数関数）
-- 2. A4（長さ 0.297 m）で折れる最大回数は？ 本物の紙で試して、式の答えと比べよう
-- 3. 12 回折るのに必要な長さは？ ギャリバンの 1.2 km と比べよう。13 回には何 km いる？
-- 4. 毎回向きを変えて（縦・横交互に）折ると、必要な紙はどう変わる？ 考えてみよう

-- ==================================================
-- ここから下は「数を読みやすくする道具」と画面の道具
-- ==================================================

fmtLen :: Double -> String
fmtLen m
  | m < 1e-2 = nf 2 (m * 1000) ++ " mm"
  | m < 1 = nf 1 (m * 100) ++ " cm"
  | m < 1000 = nf 1 m ++ " m"
  | m < 1e8 = fmtNum (m / 1000) ++ " km"
  | otherwise = nf 2 (m / 1000 / 1e8) ++ " x10^8 km"

-- 3 桁ごとにコンマを入れる
fmtNum :: Double -> String
fmtNum v = reverse (go (reverse (show (round v :: Integer))))
  where
    go s = case splitAt 3 s of
      (a, []) -> a
      (a, b) -> a ++ "," ++ go b

nf :: Int -> Double -> String
nf d v = showFFloat (Just d) v ""

sc :: Env -> Point -> Point
sc env (x, y) = (x - envW env / 2, envH env / 2 - y)

mouseXY :: Env -> Point
mouseXY env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sc env (x, y) in label a b size c s

-- 線分を長方形の中に切りつめる（リャン・バースキーの方法）
clipSeg :: (Float, Float, Float, Float) -> Point -> Point -> Maybe (Point, Point)
clipSeg (x0, y0, x1, y1) (ax, ay) (bx, by) =
  let dx = bx - ax
      dy = by - ay
      edge (t0, t1) (p, q)
        | p == 0 = if q < 0 then (1, 0) else (t0, t1)
        | p < 0 = (max t0 (q / p), t1)
        | otherwise = (t0, min t1 (q / p))
      (ta, tb) = foldl edge (0, 1) [(-dx, ax - x0), (dx, x1 - ax), (-dy, ay - y0), (dy, y1 - ay)]
  in if ta > tb then Nothing
     else Just ((ax + dx * ta, ay + dy * ta), (ax + dx * tb, ay + dy * tb))

dashed :: Float -> [Float] -> Point -> Point -> Picture
dashed w pat a@(ax, ay) (bx, by) =
  let l = dist2 a (bx, by)
      pt s = (ax + (bx - ax) * s / l, ay + (by - ay) * s / l)
      go s (on : off : rest)
        | s >= l = []
        | otherwise = thickLine w (pt s) (pt (min l (s + on))) : go (s + on + off) rest
      go _ _ = []
  in if l < 1e-6 then Blank else Pictures (go 0 (cycle pat))
