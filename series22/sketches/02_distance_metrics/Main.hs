-- ============================================
-- 02 距離を変えると形が変わる
-- 「近い」の測り方（距離）を変えると、同じサイトでもボロノイ図の形が変わる
--   ユークリッド √(dx² + dy²)・マンハッタン |dx| + |dy|・チェビシェフ max(|dx|, |dy|)・ミンコフスキー (|dx|^p + |dy|^p)^(1/p)
-- 1〜4 = 距離の種類 / ←→ = p（ミンコフスキー）/ ドラッグ = 動かす / クリック = 追加 / R = ランダム / C = 消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (findIndex, foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Text.Printf (printf)

res :: Float
res = 5                   -- ★ 計算の細かさ

gw, gh :: Int
gw = ceiling (width / res)
gh = ceiling (height / res)

data World = World
  { wSites  :: ![Point]   -- サイト（座標は p5 と同じ：左上が原点、y は下向き）
  , wMetric :: !Int       -- 1 ユークリッド 2 マンハッタン 3 チェビシェフ 4 ミンコフスキー
  , wP      :: !Float     -- ★ ミンコフスキーの p（1 = マンハッタン、2 = ユークリッド、大きいほどチェビシェフ）
  , wDrag   :: !Int
  , wDirty  :: !Bool
  , wImg    :: !Picture
  }

main :: IO ()
main = runSketch (sketch "22-02 Distance Metrics" world0)
  { skStart = \_ -> randomSites 14 world0
  , skStep  = \_ _ w -> pure (if wDirty w then w { wImg = paintBuffer w, wDirty = False } else w)
  , skDraw  = \env w -> drawAll (toP5 (envMouse env)) w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World [] 1 3 (-1) True Blank

randomSites :: Int -> World -> IO World
randomSites n w = do
  ss <- replicateM n ((,) <$> randomRIO (60, width - 60) <*> randomRIO (120, height - 40))
  pure w { wSites = ss, wDirty = True }

metricName :: Int -> String
metricName m = case m of
  1 -> "Euclidean"
  2 -> "Manhattan (city blocks)"
  3 -> "Chebyshev (king moves)"
  _ -> "Minkowski"

-- ★ 距離の定義（ここを書きかえると新しい形のボロノイ図）
metricDist :: Int -> Float -> Float -> Float -> Float
metricDist metric p dx0 dy0 = case metric of
  1 -> sqrt (dx * dx + dy * dy)
  2 -> dx + dy
  3 -> max dx dy
  -- ミンコフスキー：大きい方で割ってから p 乗すると、桁あふれしない（大きい方は 1 になるので累乗は 1 回で済む）
  _ | m == 0 -> 0
    | otherwise -> m * (1 + (min dx dy / m) ** p) ** (1 / p)
  where
    dx = abs dx0
    dy = abs dy0
    m = max dx dy

paintBuffer :: World -> Picture
paintBuffer w = pixelImage gw gh res pixel
  where
    metric = wMetric w
    p = wP w
    xs = U.fromList (map fst (wSites w))
    ys = U.fromList (map snd (wSites w))
    n = U.length xs
    cols = U.generate n (\i -> hsbToRgb (wrapF 360 (fromIntegral i * 137.5)) 45 85)
    -- どの距離でも max(|dx|, |dy|) 以上なので、それが今の最小より大きければ累乗を計算しなくてよい
    labelAt gx gy =
      let x = (fromIntegral gx + 0.5) * res
          y = (fromIntegral gy + 0.5) * res
          go !i !best !bd
            | i >= n = best
            | otherwise =
                let dx = x - U.unsafeIndex xs i
                    dy = y - U.unsafeIndex ys i
                in if max (abs dx) (abs dy) >= bd then go (i + 1) best bd else
                   let d = metricDist metric p dx dy
                   in if d < bd then go (i + 1) i d else go (i + 1) best bd
      in go 0 (-1) (1 / 0) :: Int
    lab = U.generate (gw * gh) (\i -> let (gy, gx) = i `divMod` gw in labelAt gx gy)
    -- となりとラベルが違う画素 = 境界（暗く）
    pixel gx gy =
      let i = gx + gy * gw
          l = U.unsafeIndex lab i
          edge = (gx + 1 < gw && U.unsafeIndex lab (i + 1) /= l) || (gy + 1 < gh && U.unsafeIndex lab (i + gw) /= l)
          (r, g, b) = if l >= 0 then U.unsafeIndex cols l else (25, 25, 25)
          f = if edge then 0.2 else 1
      in (w8 (r * f), w8 (g * f), w8 (b * f))

-- 「距離 = r の点の集まり」（その距離での“円”）をかく
unitBall :: World -> Float -> Point -> Float -> Picture
unitBall w t (cx, cy) r = outline t
  [ (cx + ux / d * r, cy + uy / d * r)
  | k <- [0 .. 119 :: Int]
  , let a = fromIntegral k / 120 * tau
        ux = cos a
        uy = sin a
        d = metricDist (wMetric w) (wP w) ux uy ]

drawAll :: Point -> World -> Picture
drawAll m@(mx, my) w = Pictures
  [ wImg w
  , view (concatMap siteDot ss ++ mouseBall ++
      [ Color (hsbA 0 0 0 55) (boxAt 20 (height - 170) 150 150)
      , Color (hsb 50 90 100) (unitBall w 3 (95, height - 95) 60)
      ])
  , txt 28 (height - 157) 14 (gray 1) "Distance = 1"
  , view [Color (hsbA 0 0 0 55) (boxAt 0 0 width 84)]
  , txt 20 30 16 (gray 1) ("1-4: " ++ metricName (wMetric w)
      ++ (if wMetric w == 4 then "  p = " ++ printf "%.2f" (wP w) ++ " (Left/Right)" else "")
      ++ "  Drag: move  Click: add  R: random  C: clear")
  , txt 20 52 14 (gray 0.8) "The mouse shape is the \"circle\" up to the nearest site: diamond in Manhattan, square in Chebyshev,"
  , txt 20 74 14 (gray 0.8) "so boundaries become lines at 45 degrees or horizontal/vertical. Try p = 1, 2, 8 with the arrow keys."
  ]
  where
    ss = wSites w
    siteDot s = [dotAt (hsb 0 0 10) s 13, dotAt (gray 1) s 9]
    -- マウスから最寄りのサイトまでの距離で“円”をかく：円の上にサイトが乗っている
    (best, bd) = foldl' pick (-1 :: Int, 1 / 0) (zip [0 ..] ss)
    pick (bi, bv) (i, (x, y)) =
      let d = metricDist (wMetric w) (wP w) (mx - x) (my - y)
      in if d < bv then (i, d) else (bi, bv)
    mouseBall = case nth ss best of
      Just s | my > 90 ->
        [ Color (gray 1) (unitBall w 2 m bd)
        , Color (hsbA 0 0 100 50) (thickLine 2 m s) ]
      _ -> []

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> pure (press (toP5 pos) w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion pos | wDrag w >= 0 ->
    let (x, y) = toP5 pos
    in pure w { wSites = setAt (wDrag w) (clampF 0 width x, clampF 90 height y) (wSites w), wDirty = True }
  EventKey (SpecialKey KeyRight) Down _ _ ->
    let p = wP w in pure w { wP = min 12 (if p < 1 then p + 0.1 else p + 0.25), wMetric = 4, wDirty = True }
  EventKey (SpecialKey KeyLeft) Down _ _ ->
    let p = wP w in pure w { wP = max 0.5 (if p <= 1 then p - 0.1 else p - 0.25), wMetric = 4, wDirty = True }
  EventKey (Char c) Down _ _ -> case toLower c of
    '1' -> pure w { wMetric = 1, wDirty = True }
    '2' -> pure w { wMetric = 2, wDirty = True }
    '3' -> pure w { wMetric = 3, wDirty = True }
    '4' -> pure w { wMetric = 4, wDirty = True }
    'r' -> do n <- randomRIO (8, 21)
              randomSites n w
    'c' -> pure w { wSites = [], wDirty = True }
    _ -> pure w
  _ -> pure w

press :: Point -> World -> World
press m@(_, my) w
  | my < 90 = w
  | otherwise = case findIndex (\s -> dist2 m s < 14) (wSites w) of
      Just i -> w { wDrag = i }
      Nothing -> w { wSites = wSites w ++ [m], wDirty = True }

-- ---------- やってみよう ----------
-- 1. p = 1 と マンハッタン、p = 2 と ユークリッド、p = 12 と チェビシェフ を見比べよう
-- 2. p を 1 より小さく（0.5）すると“円”がへこむ（星形）。これはもう「距離」と呼べない（三角不等式が成り立たない）。セルはどうなる？
-- 3. metricDist を dx * 2 + dy のように「横に長い」距離にすると、セルはどちらに伸びる？
-- 4. マンハッタン距離で、2 つのサイトを斜め 45° に並べると境界に「面」ができることがある。探してみよう

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- マウスの位置（gloss の座標：中央が原点、y は上向き）を p5 と同じ座標に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は p5 の text と同じ左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y bw bh = rect (x + bw / 2) (y + bh / 2) bw bh

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 直径 d の円（p5 の circle と同じく直径で書く）
dotAt :: Color -> Point -> Float -> Picture
dotAt c p d = Color c (at p (disc (d / 2)))

-- 閉じた多角形の太い枠
outline :: Float -> [Point] -> Picture
outline t ps = thickPath t (ps ++ take 1 ps)

-- 安全な添字アクセス
nth :: [a] -> Int -> Maybe a
nth xs i | i < 0 = Nothing
         | otherwise = case drop i xs of
             (x : _) -> Just x
             [] -> Nothing

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]

-- 色の変換（HSB → RGB 0〜255）
hsbToRgb :: Float -> Float -> Float -> (Float, Float, Float)
hsbToRgb h s0 b0 = (f 5, f 3, f 1)
  where
    s = s0 / 100
    b = b0 / 100
    f :: Float -> Float
    f n = let k = wrapF 6 (n + h / 60) in 255 * b * (1 - s * max 0 (min k (min (4 - k) 1)))

w8 :: Float -> Word8
w8 v = fromIntegral (max 0 (min 255 (truncate (v + 0.5) :: Int)))
