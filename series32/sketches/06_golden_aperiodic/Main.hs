-- ============================================
-- 06 黄金比と非周期性
-- ペンローズ・タイルの枚数の比は黄金比 φ に近づく。φ は無理数だから、模様は決してくり返さない
--   1 次元版 = フィボナッチ語（L → LS、S → L）。格子を「無理数の傾き」で切ると同じ並びが出てくる
--   くり返さないのに「秩序」がある証拠 = 回折像（光を当てた時の模様）にくっきりした点が 10 回対称に並ぶ
-- ↑↓ = フィボナッチ語の世代 / ドラッグ左右 = 切る傾き（P で 1/φ に戻す）/ 1・2・3 = 回折：ペンローズ・正方格子・ランダム / S = 保存
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', sort)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)
import System.Random (mkStdGen, randomR, StdGen)

phi :: Double
phi = (1 + sqrt 5) / 2

data World = World
  { wGen    :: !Int      -- ★ フィボナッチ語の世代
  , wSlope  :: !Double   -- ★ 格子を切る直線の傾き（1/φ）
  , wSource :: !Int      -- 回折に使う点：1 = ペンローズ、2 = 正方格子、3 = ランダム
  , wDiff   :: Picture   -- 回折像（96 × 96）
  }

main :: IO ()
main = runSketch (sketch "32-06 Golden Ratio & Aperiodicity" (World 7 (1 / phi) 1 (computeDiffraction 1)))
  { skBack  = rgb255 16 18 26
  , skDraw  = drawAll
  , skEvent = onEvent
  }

drawAll :: Env -> World -> Picture
drawAll env w =
  let pw = envW env / 3
      h = envH env
  in Pictures
    [ drawCounts env 20 110 (pw - 40)
    , drawCutProject env w (pw + 10) 110 (pw - 20) (h - 200)
    , drawDiffraction env w (2 * pw + 10) 110 (pw - 30)
    , txt env 20 38 20 white "Golden ratio & aperiodicity: never repeats, yet ordered"
    , txt env 20 66 15 (gray 0.75)
        ("Left: counts & ratio   Center: cut the lattice at slope " ++ nf 4 (wSlope w) ++ " (drag, P = 1/phi)   Right: diffraction (1,2,3)")
    ]

-- ---- 左：太い / 細いひし形（の半分の三角形）の枚数 ----
drawCounts :: Env -> Float -> Float -> Float -> Picture
drawCounts env x y w =
  let -- ★ 分割のルール：細い → 細い 1 + 太い 1、太い → 細い 1 + 太い 2
      rows = take 13 (iterate (\(a, b) -> (a + b, a + 2 * b)) (1, 0)) :: [(Int, Int)]
      nRows = fromIntegral (length rows) :: Float
      row (n, (a, b)) =
        let yy = y + fromIntegral n * 26
            tot = fromIntegral (a + b) :: Float
            w1 = (w - 140) * fromIntegral a / tot
            w2 = (w - 140) * fromIntegral b / tot
        in Pictures
          [ txt env x (yy + 12) 14 (gray 0.6) (show (n :: Int))
          , onP5 env (Pictures
              [ Color (rgb255 250 190 80) (box (x + 24) yy w1 16)
              , Color (rgb255 70 130 210) (box (x + 24 + w1) yy w2 16) ])
          , txt env (x + w - 100) (yy + 12) 14 (gray 0.86) (if a > 0 then nf 5 (fromIntegral b / fromIntegral a :: Double) else "-")
          ]
  in Pictures
    ( txt env x (y - 10) 14 white "n  thin (yellow) / thick (blue)  ratio"
    : txt env x (y + nRows * 26 + 20) 14 (rgb255 255 220 120) ("-> phi = " ++ nf 5 phi ++ "... (irrational)")
    : txt env x (y + nRows * 26 + 46) 14 (gray 0.67) "Irrational ratio: repeating a few"
    : txt env x (y + nRows * 26 + 66) 14 (gray 0.67) "tiles periodically cannot make this"
    : map row (zip [0 ..] rows) )

-- ---- 中：切断と射影（cut and project）とフィボナッチ語 ----
drawCutProject :: Env -> World -> Float -> Float -> Float -> Float -> Picture
drawCutProject env world x y w h =
  let cs = 26
      ox = x + 20
      oy = y + h * 0.55
      slope = wSlope world
      c = 1 / sqrt (1 + slope * slope)
      s = slope * c
      win = c + s                               -- ★ 帯の幅 = 単位正方形の影の長さ
      points = [ (i, j, perp >= -1e-9 && perp < win - 1e-9, fromIntegral i * c + fromIntegral j * s)
               | i <- [0 .. 59 :: Int], j <- [-3 .. 69 :: Int]
               , let perp = negate (fromIntegral i) * s + fromIntegral j * c ]   -- 直線からの（符号つき）距離
      pts = sort [ d | (_, _, True, d) <- points ]   -- 直線上に写した位置
      dots = [ at (px, py) (if inside then Color (rgb255 255 220 120) (disc 4) else Color (rgb255 70 74 90) (disc 2))
             | (i, j, inside, _) <- points
             , let px = ox + fromIntegral i * cs
                   py = oy - fromIntegral j * cs
             , px <= x + w && py >= y && py <= y + h - 120 ]
      -- 帯（2 本の平行線）
      sl = realToFrac slope :: Float
      bigL = min (w - 30) ((oy - y) / max sl 0.01)
      off = realToFrac (win / c) * cs
      band = Color (rgba255 120 200 255 140) (Pictures
        [ thickLine 1.5 (ox, oy) (ox + bigL, oy - bigL * sl)
        , thickLine 1.5 (ox, oy - off) (ox + bigL, oy - bigL * sl - off) ])
      -- 写した点の間隔：長い L と短い S の 2 種類だけ
      big = max c s
      small = min c s
      word = take 34 [ if abs (d - big) < abs (d - small) then 'L' else 'S' | (a, b) <- zip pts (drop 1 pts), let d = b - a ]
      fw = fibWord (wGen world)
      cnt ch = length (filter (== ch) fw)
  in Pictures
    [ onP5 env (Pictures (band : dots))
    , txt env x (y - 10) 14 white "Yellow: points in the band. Gaps: L, S"
    , txt env x (y + h - 58) 14 white ("Cut: " ++ word)
    , txt env x (y + h - 34) 14 white ("Fib: " ++ take 34 fw)
    , txt env x (y + h - 10) 14 (gray 0.67)
        ("Gen " ++ show (wGen world) ++ " (Up/Down)  length " ++ show (length fw) ++ "  L/S = " ++ nf 4 (fromIntegral (cnt 'L') / fromIntegral (max 1 (cnt 'S')) :: Double))
    , onP5 env (drawWordStrip fw x (y + h - 100) w)
    ]

drawWordStrip :: String -> Float -> Float -> Float -> Picture
drawWordStrip wd x y w =
  let phiF = realToFrac phi :: Float
      lenOf ch = if ch == 'L' then phiF else 1
      total = sum (map lenOf wd)
      k = min 1 (w / (total * 14)) * 14
      go _ [] = []
      go px (ch : rest)
        | px + l > x + w = []
        | otherwise = Color (if ch == 'L' then rgb255 250 190 80 else rgb255 70 130 210) (box px y (l - 1) 18) : go (px + l) rest
        where l = lenOf ch * k
  in Pictures (go x wd)

-- ★ フィボナッチ語：L → LS、S → L をくり返す
fibWord :: Int -> String
fibWord g = foldl' (\wd _ -> concatMap (\ch -> if ch == 'L' then "LS" else "L") wd) "L" [1 .. g]

-- ---- 右：回折像 ----
drawDiffraction :: Env -> World -> Float -> Float -> Float -> Picture
drawDiffraction env world x y w =
  let k = w / 96
      name = case wSource world of
        1 -> "Penrose vertices"
        2 -> "Square lattice"
        _ -> "Random points"
      msg = case wSource world of
        1 -> ["Sharp spots in 10-fold symmetry: order!", "Impossible for a periodic crystal", "(a quasicrystal)"]
        2 -> ["Spots on a square grid: periodic crystal"]
        _ -> ["Only a blurry fog: no order"]
  in Pictures
    ( at (x + w / 2 - envW env / 2, envH env / 2 - y - w / 2) (Scale k k (wDiff world))
    : onP5 env (Color (gray 0.31) (rectLine (x + w / 2) (y + w / 2) w w))
    : txt env x (y - 10) 14 white (name ++ ": diffraction (1,2,3)")
    : [ txt env x (y + w + 24 + fromIntegral i * 20) 14 (gray 0.67) m | (i, m) <- zip [0 :: Int ..] msg ] )

-- 点の集まりに光を当てた時の明るさ |Σ exp(i k·x)|²（重い計算なので 1 回だけ）
-- cos, sin を毎回計算せず、k の値ごとの表を掛け合わせて速くしている
computeDiffraction :: Int -> Picture
computeDiffraction source =
  let ps = case source of
        1 -> penroseVertices 6 9
        2 -> [ (fromIntegral i, fromIntegral j) | i <- [-9 .. 9 :: Int], j <- [-9 .. 9 :: Int], i * i + j * j < 81 ]
        _ -> randomPoints 300 (mkStdGen 3)
      n = 96 :: Int                -- ★ 解像度
      kmax = 13 :: Double          -- ★ k の範囲
      kOf a = (fromIntegral a / fromIntegral (n - 1) * 2 - 1) * kmax
      kv = U.generate n kOf
      nq = length ps
      wgt = U.fromList [ exp (negate (x * x + y * y) / 40) | (x, y) <- ps ]   -- 端をぼかして、にじみを減らす
      xs = U.fromList (map fst ps)
      ys = U.fromList (map snd ps)
      table f coords = U.generate (nq * n) (\i -> f (U.unsafeIndex kv (i `mod` n) * U.unsafeIndex coords (i `div` n)))
      cx = table cos xs
      sx = table sin xs
      cy = table cos ys
      sy = table sin ys
      value :: Int -> Int -> Double
      value !a !b = go 0 0 0
        where
          go !q !re !im
            | q >= nq = re * re + im * im
            | otherwise =
                let ca = U.unsafeIndex cx (q * n + a)
                    sa = U.unsafeIndex sx (q * n + a)
                    cb = U.unsafeIndex cy (q * n + b)
                    sb = U.unsafeIndex sy (q * n + b)
                    g = U.unsafeIndex wgt q
                in go (q + 1) (re + g * (ca * cb - sa * sb)) (im + g * (sa * cb + ca * sb))
      -- k と −k の明るさは同じなので、半分だけ計算する
      half = (n * n + 1) `div` 2
      vals = U.generate half (\i -> value (i `div` n) (i `mod` n))
      valAt i = U.unsafeIndex vals (if i < half then i else n * n - 1 - i)
      mx = max 1e-12 (U.foldl' max 0 vals)
      pixel a b =
        let br = (valAt (a * n + b) / mx) ** 0.35 * 255
        in (toByte br, toByte (br * 0.9), toByte (min 255 (br * 0.6 + 40)))
      toByte v = fromIntegral (max 0 (min 255 (floor v :: Int)))
  in vals `seq` pixelImage n n 1 pixel

randomPoints :: Int -> StdGen -> [(Double, Double)]
randomPoints 0 _ = []
randomPoints k g =
  let (u, g1) = randomR (0, 1) g
      (a, g2) = randomR (0, 2 * pi) g1
      r = 9 * sqrt u
  in (r * cos a, r * sin a) : randomPoints (k - 1) g2

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _   -> pure w { wGen = min 14 (wGen w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wGen = max 1 (wGen w - 1) }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> let s = fromEnum c - 48 in pure w { wSource = s, wDiff = computeDiffraction s }
    | c == 'p' || c == 'P' -> pure w { wSlope = 1 / phi }
    | c == 's' || c == 'S' -> saveSvg "golden" env (drawAll env w) >> pure w
  EventKey (MouseButton LeftButton) Down _ (mx, _) -> pure (slopeFrom env mx w)
  EventMotion (mx, _) | envDown env -> pure (slopeFrom env mx w)
  _ -> pure w

slopeFrom :: Env -> Float -> World -> World
slopeFrom env mx w =
  let x = mx + envW env / 2
      s = clampF 0.2 1.2 (remap x 0 (envW env) 0.2 1.2)
  in w { wSlope = realToFrac s }

-- ---------- やってみよう ----------
-- 1. ドラッグで傾きを 0.5（= 1/2）や 0.6（= 3/5）に近づけると、切った並びがくり返しになる。1/φ では？
-- 2. フィボナッチ語の長さは 1, 2, 3, 5, 8, 13, … 。L ÷ S が φ に近づくのを確かめよう
-- 3. 回折像 1 と 2 を比べよう。10 回対称の点の並びは、1984 年に本物の合金（準結晶）で見つかった
-- 4. 左の表の数を足し算でたどると、フィボナッチ数列が隠れている。見つけよう


-- ==================================================
-- ここから下は「道具」（座標と、05 と同じ分割で頂点を作るペンローズの道具）
-- ==================================================
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

nf :: Int -> Double -> String
nf d x = showFFloat (Just d) x ""

type D2 = (Double, Double)
data Tri = Tri !Int !D2 !D2 !D2

penroseVertices :: Int -> Double -> [D2]
penroseVertices lv radius =
  let r = phi ^ lv
      pt k = (r * cos (fromIntegral k * pi / 10), r * sin (fromIntegral k * pi / 10))
      wheel i = if even i then Tri 0 (0, 0) (pt (2 * i + 1)) (pt (2 * i - 1))
                else Tri 0 (0, 0) (pt (2 * i - 1)) (pt (2 * i + 1 :: Int))
      l2 (ax, ay) (bx, by) t = (ax + (bx - ax) * t, ay + (by - ay) * t)
      one (Tri 0 a b c) = let p = l2 a b (1 / phi) in [Tri 0 c p b, Tri 1 p c a]
      one (Tri _ a b c) =
        let q = l2 b a (1 / phi)
            s = l2 b c (1 / phi)
        in [Tri 1 s c a, Tri 1 q s b, Tri 0 s q a]
      tris = foldr (\_ t -> concatMap one t) (map wheel [0 .. 9 :: Int]) [1 .. lv]
      addV m v@(x, y)
        | x * x + y * y > radius * radius = m
        | otherwise = M.insert (round (x * 1000) :: Int, round (y * 1000) :: Int) v m
      seen = foldl' (\m (Tri _ a b c) -> addV (addV (addV m a) b) c) M.empty tris
  in M.elems seen
