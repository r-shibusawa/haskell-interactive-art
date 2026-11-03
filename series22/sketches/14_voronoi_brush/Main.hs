-- ============================================
-- 14 ボロノイ・ブラシ（自由制作のベース）
-- マウスでなぞるとサイトがまかれ、その場でボロノイ図の絵になる。サイトを置くたびに音が鳴る
--   L でロイドの緩和（06）をかけると、ばらばらのセルがそろっていく。色・描き方を変えて自分の作品に
-- ドラッグ = サイトをまく / 1〜4 = 配色 / W = 描き方 / L = ロイド ON/OFF / ↑↓ = まく間隔 / S = 保存 / C = 全部消す（最初のクリックで音もスタート）
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM, when)
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U

maxSites :: Int
maxSites = 300            -- ★ サイトの上限（多いと重い）

-- 絵を描いておく画用紙の大きさ（画面の y = 90 から下）
artW, artH :: Float
artW = width
artH = height - 90

styleNames :: [String]
styleNames = ["Fill", "Stained glass", "Gradient", "Lines only"]

-- ★ 配色（好きな色に変えよう）
palettes :: [[Color]]
palettes =
  [ map hex [0x264653, 0x2a9d8f, 0xe9c46a, 0xf4a261, 0xe76f51]
  , map hex [0x0b132b, 0x1c2541, 0x3a506b, 0x5bc0be, 0xa8dadc]
  , map hex [0xffbe0b, 0xfb5607, 0xff006e, 0x8338ec, 0x3a86ff]
  , map hex [0xf1faee, 0xe0e1dd, 0xc9ada7, 0x9a8c98, 0x4a4e69]
  ]

penta :: U.Vector Int
penta = U.fromList [0, 2, 4, 7, 9]

data Site = Site { sX, sY :: !Float, sC :: !Int }

data World = World
  { wSites :: ![Site]     -- サイト（座標は画用紙の中：左上が原点、y は下向き）
  , wCells :: ![[Point]]
  , wGap   :: !Float      -- ★ まく間隔（px）
  , wLloyd :: !Bool
  , wStyle :: !Int
  , wPal   :: !Int
  , wArt   :: !Picture    -- 描き終わった絵
  , wDirty :: !Bool
  , wLast  :: !(Maybe Point)
  , wAudio :: !Bool       -- 音がスタートしたか
  }

main :: IO ()
main = runSketch (sketch "22-14 Voronoi Brush" world0)
  { skBack  = hsb 230 25 12
  , skStart = \_ -> do
      ss <- replicateM 40 (Site <$> randomRIO (0, width) <*> randomRIO (0, artH) <*> randomRIO (0, 4))
      pure world0 { wSites = ss }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = \env w -> drawAll env w
  , skEvent = \env ev w -> onEvent env ev w
  }

world0 :: World
world0 = World [] [] 28 False 0 0 Blank True Nothing False

step :: World -> World
step w0' =
  let w = if wLloyd w0' && not (null (wCells w0')) then lloydStep w0' else w0'
  in if wDirty w then rebuild w else w

rebuild :: World -> World
rebuild w = w { wCells = cells, wArt = art, wDirty = False }
  where
    points = map (\s -> (sX s, sY s)) (wSites w)
    cells = map (\p -> voronoiCell points p 0 0 artW artH) points
    pal = case nth palettes (wPal w) of
      Just p -> p
      Nothing -> [gray 1]
    art = Pictures (concat (zipWith cellPic (wSites w) cells))
    cellPic s ps
      | length ps < 3 = []
      | otherwise =
          let col = case nth pal (sC s `mod` length pal) of
                Just c -> c
                Nothing -> gray 1
              cen = polyCentroid ps
              shrink k = map (\(x, y) -> (fst cen + (x - fst cen) * k, snd cen + (y - snd cen) * k)) ps
          in case wStyle w of
               0 -> [Color col (Polygon ps), Color (gray (15 / 255)) (outline 1.5 ps)]        -- 塗り
               1 -> [ Color col (Polygon ps)                                                  -- ステンドグラス：太い黒い枠
                    , Color (gray (10 / 255)) (outline 7 ps)
                    , Color (makeColor 1 1 1 (60 / 255)) (outline 2 (shrink 0.6)) ]
               2 -> [ Color (mixWhite col (fromIntegral k * 0.12)) (Polygon (shrink (1 - fromIntegral k * 0.18)))  -- グラデーション：中心へ縮めた多角形を重ねる（F1 のような見え方）
                    | k <- [0 .. 4 :: Int] ]
               _ -> [Color col (outline 2 (shrink 0.9))]                                      -- 線だけ

lloydStep :: World -> World
lloydStep w = w { wSites = zipWith move (wSites w) (wCells w), wDirty = True }
  where
    move s c
      | length c < 3 = s
      | otherwise = let (cx, cy) = polyCentroid c in s { sX = lerpF (sX s) cx 0.5, sY = lerpF (sY s) cy 0.5 }

drawAll :: Env -> World -> Picture
drawAll env w = Pictures
  [ scene w
  , view [Color (hsbA 0 0 100 60) (at (toP5 (envMouse env)) (ring (wGap w / 2) 1.5))]
  , view [Color (hsb 230 25 12) (boxAt 0 0 width 90)]
  , txt 20 30 16 (gray 1) ((if wAudio w then "" else "Click starts sound.  ") ++ "Drag: scatter  1-4: palette  W: " ++ styleName ++ "  L: Lloyd " ++ onOff (wLloyd w)
      ++ "  Up/Down: gap " ++ show (round (wGap w) :: Int) ++ "  S: save  C: clear  Sites " ++ show (length (wSites w)) ++ " / " ++ show maxSites)
  , txt 20 52 14 (hsb 0 0 65) "Colors are picked at random as you scatter. Toggle L on and off rather than holding it."
  , txt 20 74 14 (hsb 0 0 65) "When the cells even out, change the style with W and save with S."
  ]
  where
    styleName = case nth styleNames (wStyle w) of
      Just s -> s
      Nothing -> ""

-- 画用紙の絵（濃い灰色の地 + セル）を画面に置く
scene :: World -> Picture
scene w = view
  [ Color (gray (15 / 255)) (boxAt 0 90 artW artH)
  , Translate 0 90 (wArt w) ]

-- なぞった所にサイトをまく
paint :: Point -> World -> IO World
paint (mx, my) w
  | my < 90 || length (wSites w) >= maxSites = pure w
  | tooClose = pure w
  | otherwise = do
      dx <- randomRIO (-0.5, 0.5)
      c <- randomRIO (0, 4)
      -- ★ 置いた場所で音：高さ = 上ほど高い、左右 = 音色
      let k = max 0 (min 14 (floor (remap y artH 0 0 15))) :: Int
          freq = 220 * 2 ** (fromIntegral (12 * (k `div` 5) + penta U.! (k `mod` 5)) / 12)
      when (wAudio w) $ playTone (clampF 30 4000 freq) 0.15 0.4 (if mx < width / 2 then Triangle else Sine)
      pure w { wSites = wSites w ++ [Site (mx + dx) y c], wLast = Just (mx, y), wDirty = True }
  where
    y = my - 90
    tooClose = case wLast w of
      Just l -> dist2 (mx, y) l < wGap w
      Nothing -> False

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> paint (toP5 pos) w { wAudio = True, wLast = Nothing }
  EventMotion pos | envDown env -> paint (toP5 pos) w
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wGap = min 120 (wGap w + 4) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wGap = max 6 (wGap w - 4) }
  EventKey (Char c) Down _ _ -> case toLower c of
    '1' -> pure w { wPal = 0, wDirty = True }
    '2' -> pure w { wPal = 1, wDirty = True }
    '3' -> pure w { wPal = 2, wDirty = True }
    '4' -> pure w { wPal = 3, wDirty = True }
    'w' -> pure w { wStyle = (wStyle w + 1) `mod` length styleNames, wDirty = True }
    'l' -> pure w { wLloyd = not (wLloyd w) }
    's' -> do saveSvg "voronoi_brush" env (drawAll env w)
              pure w
    'c' -> pure w { wSites = [], wCells = [], wDirty = True }
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. palettes に自分の配色を足そう（coolors.co などで 5 色選ぶ）
-- 2. サイトの色を「置いた場所の y」や「置いた時刻」で決めると、グラデーションの絵になる
-- 3. 08 の重みつきボロノイのように、ブラシの速さで「重み」を変えるとどうなる？
-- 4. 10 のウォーリーノイズの描き方（F2 − F1 で線）を新しい style として足そう

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

-- 閉じた多角形の太い枠
outline :: Float -> [Point] -> Picture
outline t ps = thickPath t (ps ++ take 1 ps)

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- 安全な添字アクセス
nth :: [a] -> Int -> Maybe a
nth xs i | i < 0 = Nothing
         | otherwise = case drop i xs of
             (x : _) -> Just x
             [] -> Nothing

-- ==================================================
-- ボロノイ図の道具
--   voronoiCell sites a x0 y0 x1 y1 … サイト a の領域（多角形）。長方形を垂直二等分線で切っていく
--   サイトが多いときのために、セルのどの頂点よりも遠い二等分線（セルに届かない）は切らずに飛ばす
-- ==================================================
voronoiCell :: [Point] -> Point -> Float -> Float -> Float -> Float -> [Point]
voronoiCell sites a@(ax, ay) x0 y0 x1 y1 = fst (foldl' cut (start, reach start) sites)
  where
    start = [(x0, y0), (x1, y0), (x1, y1), (x0, y1)]
    -- a からいちばん遠い頂点までの距離² × 4（これより遠いサイトの二等分線はセルに届かない）
    reach ps = 4 * foldl' (\m (x, y) -> max m ((x - ax) * (x - ax) + (y - ay) * (y - ay))) 0 ps
    cut acc@(poly, r) b@(bx, by)
      | b == a || null (drop 2 poly) = acc
      | (bx - ax) * (bx - ax) + (by - ay) * (by - ay) > r = acc
      | otherwise =
          -- a の側 ＝ (b − a)・p ≦ (|b|² − |a|²) / 2　（a と b の垂直二等分線で切る）
          let poly' = clipHalfPlane poly (bx - ax) (by - ay) ((bx * bx + by * by - ax * ax - ay * ay) / 2)
          in (poly', reach poly')

-- nx·x + ny·y ≦ c の側だけ残す（Sutherland–Hodgman の 1 本分）
clipHalfPlane :: [Point] -> Float -> Float -> Float -> [Point]
clipHalfPlane poly nx ny c = concat (zipWith edge poly (drop 1 poly ++ take 1 poly))
  where
    side (x, y) = nx * x + ny * y - c
    edge p@(px, py) q@(qx, qy) =
      let dp = side p
          dq = side q
          t = dp / (dp - dq)
      in [p | dp <= 0] ++ [(px + (qx - px) * t, py + (qy - py) * t) | (dp < 0 && dq > 0) || (dp > 0 && dq < 0)]

-- 多角形の重心（Float の桁落ちを防ぐため、最初の頂点からの相対座標で計算する）
polyCentroid :: [Point] -> Point
polyCentroid [] = (0, 0)
polyCentroid poly@((ox, oy) : _)
  | abs a < 1e-6 = (sum (map fst poly) / n, sum (map snd poly) / n)
  | otherwise = (ox + cx / (3 * a), oy + cy / (3 * a))
  where
    n = fromIntegral (length poly)
    (a, cx, cy) = foldl' add (0, 0, 0) (zip poly (drop 1 poly ++ take 1 poly))
    add (!s, !sx, !sy) ((px0, py0), (qx0, qy0)) =
      let px = px0 - ox
          py = py0 - oy
          qx = qx0 - ox
          qy = qy0 - oy
          cr = px * qy - qx * py
      in (s + cr, sx + (px + qx) * cr, sy + (py + qy) * cr)

-- 0xRRGGBB を色にする
hex :: Int -> Color
hex v = rgb255 (v `div` 65536) (v `div` 256 `mod` 256) (v `mod` 256)

-- 白へ t の割合だけ近づける（p5 の lerpColor）
mixWhite :: Color -> Float -> Color
mixWhite c t = let (r, g, b, _) = rgbaOfColor c in makeColor (lerpF r 1 t) (lerpF g 1 t) (lerpF b 1 t) 1
