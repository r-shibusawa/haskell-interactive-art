-- ============================================
-- 08 重みつきボロノイ
-- サイトに「重み w（強さ）」をつけて、距離の測り方を変える。強いサイトほど広いなわばりを持つ
--   1 加法（アポロニウス）d − w：境界は双曲線 / 2 乗法 d ÷ w：境界は円（アポロニウスの円）/ 3 パワー図 d² − w²：境界は直線
-- 1〜3 = 種類 / ドラッグ = 動かす / Shift+ドラッグ・ホイール・↑↓ = マウスの近くのサイトの重み / クリック = 追加 / R = ランダムな重み / E = 重みをそろえる / C = 消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM, replicateM)
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Text.Printf (printf)

res :: Float
res = 5                   -- ★ 計算の細かさ

gw, gh :: Int
gw = ceiling (width / res)
gh = ceiling (height / res)

data Site = Site { sX, sY, sW :: !Float }

data World = World
  { wSites  :: ![Site]    -- サイト（座標は p5 と同じ：左上が原点、y は下向き）
  , wMode   :: !Int
  , wDrag   :: !Int
  , wWeight :: !Int       -- Shift+ドラッグで重みを変えているサイト
  , wDragY  :: !Float
  , wStartW :: !Float
  , wDirty  :: !Bool
  , wImg    :: !Picture
  }

main :: IO ()
main = runSketch (sketch "22-08 Weighted Voronoi" world0)
  { skStart = \_ -> do
      ss <- replicateM 12 (Site <$> randomRIO (80, width - 80) <*> randomRIO (140, height - 60) <*> randomRIO (10, 90))
      pure world0 { wSites = ss }
  , skStep  = \_ _ w -> pure (if wDirty w then w { wImg = paintBuffer w, wDirty = False } else w)
  , skDraw  = \env w -> drawAll (toP5 (envMouse env)) w
  , skEvent = \env ev w -> onEvent env ev w
  }

world0 :: World
world0 = World [] 1 (-1) (-1) 0 0 True Blank

modeName :: Int -> String
modeName m = case m of
  1 -> "Additive (Apollonius) d - w"
  2 -> "Multiplicative d / w"
  _ -> "Power diagram d^2 - w^2"

-- ★ 重みつきの「距離」。小さいサイトが勝つ
wdist :: Int -> Float -> Float -> Float -> Float -> Float -> Float
wdist mode sx sy sw x y = case mode of
  1 -> sqrt d2 - sw
  2 -> sqrt d2 / (sw * 0.02)   -- w = 50 で 1 倍
  _ -> d2 - sw * sw
  where d2 = (x - sx) * (x - sx) + (y - sy) * (y - sy)

paintBuffer :: World -> Picture
paintBuffer w = pixelImage gw gh res pixel
  where
    mode = wMode w
    xs = U.fromList (map sX (wSites w))
    ys = U.fromList (map sY (wSites w))
    ws = U.fromList (map sW (wSites w))
    n = U.length xs
    cols = U.generate n (\i -> hsbToRgb (wrapF 360 (fromIntegral i * 137.5)) 45 85)
    labelAt i =
      let (gy, gx) = i `divMod` gw
          x = (fromIntegral gx + 0.5) * res
          y = (fromIntegral gy + 0.5) * res
          go !k !best !bd
            | k >= n = best
            | otherwise =
                let d = wdist mode (U.unsafeIndex xs k) (U.unsafeIndex ys k) (U.unsafeIndex ws k) x y
                in if d < bd then go (k + 1) k d else go (k + 1) best bd
      in go 0 (-1) (1 / 0) :: Int
    lab = U.generate (gw * gh) labelAt
    pixel gx gy =
      let i = gx + gy * gw
          l = U.unsafeIndex lab i
          edge = (gx + 1 < gw && U.unsafeIndex lab (i + 1) /= l) || (gy + 1 < gh && U.unsafeIndex lab (i + gw) /= l)
          (r, g, b) = if l >= 0 then U.unsafeIndex cols l else (25, 25, 25)
          f = if edge then 0.15 else 1
      in (w8 (r * f), w8 (g * f), w8 (b * f))

-- 半径 r より近いサイトのうち、いちばん近いものの番号（なければ -1）
nearestIdx :: [Site] -> Point -> Float -> Int
nearestIdx ss m r = fst (foldl' pick (-1, r) (zip [0 ..] ss))
  where
    pick (bi, bd) (i, s) =
      let d = dist2 m (sX s, sY s)
      in if d < bd then (i, d) else (bi, bd)

drawAll :: Point -> World -> Picture
drawAll m w = Pictures
  [ wImg w
  , view (concat (zipWith sitePic [0 ..] (wSites w)))
  , Pictures [txt (sX s + 8) (sY s - 8) 14 (hsb 0 0 10) ("w=" ++ printf "%.0f" (sW s)) | s <- wSites w]
  , view [Color (hsbA 0 0 0 55) (boxAt 0 0 width 84)]
  , txt 20 30 16 (gray 1) ("1-3: " ++ modeName (wMode w) ++ "  Drag: move  Click: add  R: random weights  E: equal  C: clear")
  , txt 20 52 14 (gray 0.8) "Shift+drag / wheel / Up,Down: weight of the site near the mouse. Circle size = weight w."
  , txt 20 74 14 (gray 0.8) tip
  ]
  where
    hov = nearestIdx (wSites w) m 200
    sitePic i s =
      let p = (sX s, sY s)
          isH = i == hov
      in [ Color (hsbA 0 0 100 (if isH then 90 else 40)) (at p (ring (sW s) (if isH then 2.5 else 1.5)))   -- 重みを円の大きさで
         , dotAt (hsb 0 0 10) p 13, dotAt (gray 1) p 9 ]
    tip = case wMode w of
      1 -> "Boundaries are hyperbolas: crystals growing with a time lag. All weights equal (E) = ordinary Voronoi."
      2 -> "Boundaries are circles: a weak site near a strong one becomes an island. E = ordinary Voronoi."
      _ -> "Power diagram: boundaries stay straight. Too small a weight makes a cell vanish. E = ordinary Voronoi."

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down mods pos -> pure (press (shift mods == Down) (toP5 pos) w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1, wWeight = -1 }
  EventKey (MouseButton WheelUp) Down _ _ -> pure (changeW near 5)
  EventKey (MouseButton WheelDown) Down _ _ -> pure (changeW near (-5))
  EventMotion pos
    | wWeight w >= 0 ->
        let (_, y) = toP5 pos
            nw = clampF 5 200 (wStartW w + (wDragY w - y) * 0.6)
        in pure w { wSites = modAt (wWeight w) (\s -> s { sW = nw }) (wSites w), wDirty = True }
    | wDrag w >= 0 ->
        let (x, y) = toP5 pos
        in pure w { wSites = modAt (wDrag w) (\s -> s { sX = clampF 0 width x, sY = clampF 90 height y }) (wSites w), wDirty = True }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (changeW near 8)
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (changeW near (-8))
  EventKey (Char c) Down _ _ -> case toLower c of
    '1' -> pure w { wMode = 1, wDirty = True }
    '2' -> pure w { wMode = 2, wDirty = True }
    '3' -> pure w { wMode = 3, wDirty = True }
    'r' -> do ss <- forM (wSites w) (\s -> (\v -> s { sW = v }) <$> randomRIO (5, 120))
              pure w { wSites = ss, wDirty = True }
    'e' -> pure w { wSites = map (\s -> s { sW = 40 }) (wSites w), wDirty = True }
    'c' -> pure w { wSites = [], wDirty = True, wDrag = -1, wWeight = -1 }
    _ -> pure w
  _ -> pure w
  where
    near = nearestIdx (wSites w) (toP5 (envMouse env)) 200
    changeW i d
      | i >= 0 = w { wSites = modAt i (\s -> s { sW = clampF 5 200 (sW s + d) }) (wSites w), wDirty = True }
      | otherwise = w

press :: Bool -> Point -> World -> World
press shifted m@(x, y) w
  | y < 90 = w
  | shifted =
      let k = nearestIdx ss m 200
      in w { wWeight = k, wDragY = y, wStartW = maybe 0 sW (nth ss k) }
  | i >= 0 = w { wDrag = i }
  | otherwise = w { wSites = ss ++ [Site x y 40], wDirty = True }
  where
    ss = wSites w
    i = nearestIdx ss m 14

modAt :: Int -> (a -> a) -> [a] -> [a]
modAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

-- ---------- やってみよう ----------
-- 1. E で重みをそろえてから、1 つだけ重みを大きくしよう。3 つの種類で境界の形はどう違う？
-- 2. 乗法（2）で、強いサイトの近くに弱いサイトを置くと「島」になる。島の境界が円になることを確かめよう
-- 3. パワー図（3）で重みを小さくしていくと、セルが消える。消える瞬間はどんなとき？
-- 4. 重みを「人口」、サイトを「駅」と思って、駅の利用圏の地図をつくってみよう

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

-- 安全な添字アクセス
nth :: [a] -> Int -> Maybe a
nth xs i | i < 0 = Nothing
         | otherwise = case drop i xs of
             (x : _) -> Just x
             [] -> Nothing

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
