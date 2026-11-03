-- ============================================
-- 00 最寄りの点で塗り分ける
-- 画面に「サイト（種の点）」を置き、すべての場所を「いちばん近いサイト」の色で塗る ＝ ボロノイ図
--   同じ色の領域 = そのサイトの「なわばり」（セル）。境目は 2 つのサイトからちょうど同じ距離の場所
-- クリック = サイトを追加 / ドラッグ = サイトを動かす / D = 距離の濃淡 / E = 境界線 / R = ランダム / C = 全部消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Text.Printf (printf)

res :: Float
res = 5                   -- ★ 塗り分けの細かさ（何画素ずつまとめて計算するか。小さいほど細かく重い）

gw, gh :: Int
gw = ceiling (width / res)
gh = ceiling (height / res)

data World = World
  { wSites :: ![Point]    -- サイト（座標は p5 と同じ：左上が原点、y は下向き）
  , wShade :: !Bool       -- 距離で濃淡をつける
  , wEdges :: !Bool       -- 境界線をかく
  , wDrag  :: !Int        -- 動かしているサイトの番号（なければ -1）
  , wDirty :: !Bool       -- サイトが変わったときだけ塗り直す
  , wImg   :: !Picture    -- 塗り終わった絵
  }

main :: IO ()
main = runSketch (sketch "22-00 Nearest Site" world0)
  { skStart = \_ -> randomSites 12 world0
  , skStep  = \_ _ w -> pure (if wDirty w then w { wImg = paintBuffer w, wDirty = False } else w)
  , skDraw  = \env w -> drawAll (toP5 (envMouse env)) w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World [] False True (-1) True Blank

randomSites :: Int -> World -> IO World
randomSites n w = do
  ss <- replicateM n ((,) <$> randomRIO (60, width - 60) <*> randomRIO (110, height - 40))
  pure w { wSites = ss, wDirty = True }

-- ★ 色の決め方：番号 × 137.5°（黄金角）で色相をずらすと、となり同士が似た色になりにくい
siteColor :: Int -> (Float, Float, Float)
siteColor i = hsbToRgb (wrapF 360 (fromIntegral i * 137.5)) 45 85

-- すべての画素について「いちばん近いサイト」と「2 番目に近いサイト」を探して塗る
paintBuffer :: World -> Picture
paintBuffer w = pixelImage gw gh res pixel
  where
    xs = U.fromList (map fst (wSites w))
    ys = U.fromList (map snd (wSites w))
    n = U.length xs
    pal = U.generate n siteColor
    pixel gx gy =
      let x = (fromIntegral gx + 0.5) * res
          y = (fromIntegral gy + 0.5) * res
          go !i !best !d1 !d2
            | i >= n = (best, d1, d2)
            | otherwise =
                let dx = x - U.unsafeIndex xs i
                    dy = y - U.unsafeIndex ys i
                    d = dx * dx + dy * dy
                in if d < d1 then go (i + 1) i d d1
                   else if d < d2 then go (i + 1) best d1 d
                   else go (i + 1) best d1 d2
          (b, a1, a2) = go 0 (-1 :: Int) (1 / 0) (1 / 0)
      in if b < 0 then (25, 25, 25) else
         let (r, g, bl) = U.unsafeIndex pal b
             s1 = sqrt a1
             base = if wShade w then max 0.25 (1 - s1 / 350) else 1      -- ★ 遠いほど暗く
             f = if wEdges w && sqrt a2 - s1 < res * 0.8 then 0.15 else base  -- 1 番と 2 番がほぼ同じ距離 = 境界
         in (w8 (r * f), w8 (g * f), w8 (bl * f))

-- いちばん近いサイトの番号と距離（サイトがなければ番号は -1）
nearestSite :: [Point] -> Point -> (Int, Float)
nearestSite ss (x, y) =
  let (i, d2) = foldl' pick (-1, 1 / 0) (zip [0 ..] ss)
      pick (bi, bd) (k, (sx, sy)) =
        let d = (x - sx) * (x - sx) + (y - sy) * (y - sy)
        in if d < bd then (k, d) else (bi, bd)
  in (i, sqrt d2)

drawAll :: Point -> World -> Picture
drawAll m@(mx, my) w = Pictures
  [ wImg w
  , view (map siteDot (wSites w))
  , mouseInfo
  , view [Color (hsbA 0 0 0 55) (boxAt 0 0 width 84)]
  , txt 20 30 16 (gray 1) ("Click: add  Drag: move  D: shade " ++ onOff (wShade w) ++ "  E: edges " ++ onOff (wEdges w)
                           ++ "  R: random  C: clear  Sites " ++ show (length (wSites w)))
  , txt 20 52 14 (gray 0.8) "Every place takes the color of its nearest site. No other site is inside the mouse circle."
  , txt 20 74 14 (gray 0.8) "A boundary is the set of places at equal distance from two sites."
  ]
  where
    siteDot p = Pictures [Color (hsb 0 0 10) (at p (disc 7)), Color (gray 1) (at p (disc 5))]
    (ni, nd) = nearestSite (wSites w) m
    -- マウスの最寄りのサイト：線と「空の円」（この円の中にはほかのサイトが 1 つもない）
    mouseInfo = case nth (wSites w) ni of
      Just s | my > 90 -> Pictures
        [ view
            [ Color (gray 1) (thickLine 2 m s)
            , Color (hsbA 0 0 100 60) (at m (ring nd 1.5))
            , Color (gray 1) (at m (disc 4))
            ]
        , txt (mx + 10) (my - 10) 14 (hsb 0 0 10) ("#" ++ show ni ++ "  " ++ printf "%.0f" nd ++ "px")
        ]
      _ -> Blank

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> pure (press (toP5 pos) w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion pos | wDrag w >= 0 ->
    let (x, y) = toP5 pos
        p = (clampF 0 width x, clampF 90 height y)
    in pure w { wSites = setAt (wDrag w) p (wSites w), wDirty = True }
  EventKey (Char c) Down _ _ -> case toLower c of
    'd' -> pure w { wShade = not (wShade w), wDirty = True }
    'e' -> pure w { wEdges = not (wEdges w), wDirty = True }
    'r' -> do n <- randomRIO (8, 24)
              randomSites n w
    'c' -> pure w { wSites = [], wDirty = True }
    _ -> pure w
  _ -> pure w

press :: Point -> World -> World
press m@(_, my) w
  | my < 90 = w
  | ni >= 0 && nd < 14 = w { wDrag = ni }
  | otherwise = w { wSites = wSites w ++ [m], wDirty = True }
  where (ni, nd) = nearestSite (wSites w) m

-- ---------- やってみよう ----------
-- 1. サイトを 2 個だけにしてみよう。境界はどんな線？ 3 個なら？（境界が 1 点で交わる）
-- 2. サイトを 1 列にきれいに並べると、セルはどんな形になる？ 格子状（碁盤の目）や六角形状に並べたら？
-- 3. res を 2 にすると細かくなるが重くなる。res を 20 にするとどう見える？
-- 4. siteColor を「サイトの y 座標で色を決める」に変えると、地図のような絵になる


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

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

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
