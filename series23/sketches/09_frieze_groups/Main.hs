-- ============================================
-- 09 フリーズ群（7 種類）
-- 帯（フリーズ）状にくり返す模様の対称性は、ちょうど 7 種類しかない
--   並進に、鏡映（たて・よこ）、180° 回転、すべり鏡映をどう組み合わせるかで決まる
-- 左の枠の中でドラッグ = 模様を描く / 1〜7 = その帯の対称性（鏡・回転の中心）を表示 / C = 消す / P = 例の模様
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word (Word8)

-- ★ 7 つのフリーズ群：周期 1 の中のコピー（x' = a x + e, y' = d y）と名前
data Frieze = Frieze { fName :: String, fNick :: String, fGens :: String, fMaps :: [(Float, Float, Float)] }

friezes :: [Frieze]
friezes =
  [ Frieze "p1" "hop" "translation only" [(1, 1, 0)]
  , Frieze "p11g" "step" "translation + glide" [(1, 1, 0), (1, -1, 0.5)]
  , Frieze "p1m1" "sidle" "translation + vertical mirror" [(1, 1, 0), (-1, 1, 0)]
  , Frieze "p2" "spinning hop" "translation + 180 deg rotation" [(1, 1, 0), (-1, -1, 0)]
  , Frieze "p2mg" "spinning sidle" "vertical mirror + glide (-> 180 rot)" [(1, 1, 0), (-1, 1, 0), (1, -1, 0.5), (-1, -1, 0.5)]
  , Frieze "p11m" "jump" "translation + horizontal mirror" [(1, 1, 0), (1, -1, 0)]
  , Frieze "p2mm" "spinning jump" "both mirrors (-> 180 rot)" [(1, 1, 0), (-1, 1, 0), (1, -1, 0), (-1, -1, 0)]
  ]

-- 模様の線分（x: 0〜0.5, y: -0.5〜0.5 の座標）と色相
data Seg = Seg !Float !Float !Float !Float !Float

data World = World
  { wSegs  :: ![Seg]             -- 新しい順
  , wCount :: !Int
  , wBuf   :: !(U.Vector Word8)  -- 1 周期ぶんの 7 本の帯（R, G, B の順）
  , wTiles :: !Picture           -- 横にしきつめた帯
  , wPad   :: !Picture           -- 左の枠の中の線
  , wDirty :: !Bool
  , wSel   :: !Int               -- 対称性を表示する帯（なければ -1）
  , wHue   :: !Float
  , wPrev  :: !Point             -- 前のマウス位置
  }

-- 配置：帯の高さと周期は整数のピクセルにすると、すきまなく並ぶ
padX, padY, padS, stripX, layerY, stripH, period :: Float
padX = 30
padY = 130
padS = min (width * 0.24) (height * 0.5)
stripX = padX + padS + 50
layerY = 100
stripH = fromIntegral stripHI
period = fromIntegral periodI

stripHI, periodI :: Int
stripHI = floor ((height - 120) / 7)
periodI = round (stripH * 1.1)    -- ★ 周期の長さ（px）

maxSegs :: Int
maxSegs = 2000

main :: IO ()
main = runSketch (sketch "23-09 Frieze Groups" (refresh (preset world0)))
  { skBack  = hsb 230 25 10
  , skStep  = \_ _ w -> pure (if wDirty w then refresh w else w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = onEvent
  }

world0 :: World
world0 = World [] 0 U.empty Blank Blank True (-1) 0 (0, 0)

-- 例：「F」に似た非対称な模様
preset :: World -> World
preset w =
  let lines' = [(0.1, 0.4, 0.1, -0.4), (0.1, -0.4, 0.42, -0.4), (0.1, -0.05, 0.32, -0.05), (0.32, -0.05, 0.4, 0.2)]
      segs = [ Seg a b c d (wHue w + 25 * i) | (i, (a, b, c, d)) <- zip [0 ..] lines' ]
  in repaint w { wSegs = reverse segs, wCount = length segs, wHue = wHue w + 100 }

rgb8 :: Color -> (Word8, Word8, Word8)
rgb8 c = let (r, g, b, _) = rgbaOfColor c in (q r, q g, q b)
  where q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))

-- 7 本の帯を全部描き直す
repaint :: World -> World
repaint w =
  let total = periodI * stripHI * 7 * 3
      stripColor y = rgb8 (hsb (if (y `div` stripHI) `mod` 2 == 1 then 220 else 225) 20 16)
      px i = let (y, _) = (i `div` 3) `divMod` periodI
                 (r, g, b) = stripColor (y `min` (stripHI * 7 - 1))
             in case i `mod` 3 of
                  0 -> r
                  1 -> g
                  _ -> b
      base = U.generate total px
  in w { wBuf = U.modify (\m -> forM_ (reverse (wSegs w)) (drawSeg m)) base
       , wPad = padPicture (wSegs w), wDirty = True }

padPicture :: [Seg] -> Picture
padPicture segs = Pictures
  [ Color (hsb (wrapF 360 h) 70 100)
      (thickLine 3 (pt (padX + a * 2 * padS, padY + (b + 0.5) * padS)) (pt (padX + c * 2 * padS, padY + (d + 0.5) * padS)))
  | Seg a b c d h <- reverse segs ]

-- 線分 1 本を 7 本の帯すべてに描く
drawSeg :: M.MVector s Word8 -> Seg -> ST s ()
drawSeg buf (Seg x1 y1 x2 y2 h) = do
  let col = rgb8 (hsb (wrapF 360 h) 70 100)
      sc = stripH * 0.8
  forM_ (zip [0 :: Int ..] friezes) $ \(i, f) -> do
    let cy = fromIntegral i * stripH + stripH / 2
    forM_ (fMaps f) $ \(a, d, e) ->
      forM_ [-1, 0, 1, 2] $ \k -> do      -- はみ出した分も 1 周期の中に入れる
        let fx x = (a * x + e + k) * period
            fy y = cy + d * y * sc
        plotLine col buf (fx x1) (fy y1) (fx x2) (fy y2)

-- 太さ 3 の線分をなめらかにピクセルへ描く
plotLine :: (Word8, Word8, Word8) -> M.MVector s Word8 -> Float -> Float -> Float -> Float -> ST s ()
plotLine (cr, cg, cb) buf !x1 !y1 !x2 !y2 =
  when (x0 <= xN && y0 <= yN) (goY y0)
  where
    bw = periodI
    bh = stripHI * 7
    lw = 3 :: Float
    pad = lw / 2 + 1
    x0 = max 0 (floor (min x1 x2 - pad)) :: Int
    xN = min (bw - 1) (ceiling (max x1 x2 + pad)) :: Int
    y0 = max 0 (floor (min y1 y2 - pad)) :: Int
    yN = min (bh - 1) (ceiling (max y1 y2 + pad)) :: Int
    vx = x2 - x1
    vy = y2 - y1
    len2 = max 1e-6 (vx * vx + vy * vy)
    goY !y | y > yN = pure ()
           | otherwise = goX y x0 >> goY (y + 1)
    goX !y !x
      | x > xN = pure ()
      | otherwise = do
          let px = fromIntegral x + 0.5
              py = fromIntegral y + 0.5
              t = clampF 0 1 (((px - x1) * vx + (py - y1) * vy) / len2)
              ex = px - (x1 + t * vx)
              ey = py - (y1 + t * vy)
              cov = clampF 0 1 (lw / 2 + 0.5 - sqrt (ex * ex + ey * ey))
          when (cov > 0) $ do
            let i = (y * bw + x) * 3
            blend i cr cov
            blend (i + 1) cg cov
            blend (i + 2) cb cov
          goX y (x + 1)
    blend i c cov = do
      old <- M.unsafeRead buf i
      M.unsafeWrite buf i (round (fromIntegral old * (1 - cov) + fromIntegral c * cov :: Float))

-- バッファから絵を作る（1 周期ぶんの画像を横に並べる）
refresh :: World -> World
refresh w =
  let buf = wBuf w
      bh = stripHI * 7
      f x y = let i = (y * periodI + x) * 3
              in (U.unsafeIndex buf i, U.unsafeIndex buf (i + 1), U.unsafeIndex buf (i + 2))
      img = pixelImage periodI bh 1 f
      n = ceiling ((width - stripX) / period) :: Int
      tiles = [ at (pt (stripX + fromIntegral k * period + period / 2, layerY + fromIntegral bh / 2)) img
              | k <- [0 .. n - 1] ]
  in w { wTiles = Pictures tiles, wDirty = False }

drawWorld :: World -> Picture
drawWorld w = Pictures ([wTiles w] ++ padFrame ++ [wPad w] ++ names ++ sym ++ hud)
  where
    white60 = makeColor 1 1 1 0.6
    padFrame =
      [ Color white60 (Pictures (map (\(a, b) -> thickLine 2 (pt a) (pt b))
          [ ((padX, padY), (padX + padS, padY)), ((padX + padS, padY), (padX + padS, padY + padS))
          , ((padX + padS, padY + padS), (padX, padY + padS)), ((padX, padY + padS), (padX, padY)) ]))
      , Color (makeColor 1 1 1 0.25) (thickLine 2 (pt (padX, padY + padS / 2)) (pt (padX + padS, padY + padS / 2)))
      ]
    -- 帯の名前
    names = concat
      [ let y = layerY + fromIntegral i * stripH
            s = show (i + 1) ++ "  " ++ fName f ++ " (" ++ fNick f ++ ")  " ++ fGens f
        in [ box stripX (y + 2) (fromIntegral (length s) * 14 * 0.72 + 14) 24 (makeColor 0 0 0 0.55)
           , txt (stripX + 6) (y + 19) 14 (if i == wSel w then hsb 50 80 100 else white) s ]
      | (i, f) <- zip [0 :: Int ..] friezes ]
    sym = case drop (wSel w) friezes of
      (f : _) | wSel w >= 0 -> drawSymmetry (wSel w) f
      _ -> []
    selName = case drop (wSel w) friezes of
      (f : _) | wSel w >= 0 -> fName f
      _ -> "none"
    hud =
      [ txt 20 36 18 white ("Drag in the left box: draw   1-7: show symmetries (" ++ selName ++ ")   C: clear   P: example")
      , txt 20 62 15 (gray 0.7) "Exactly 7 symmetry types of strip patterns. The nicknames describe footprints (Conway)"
      , txt padX (padY + padS + 26) 14 (gray 0.8) "This box is the left half of one period."
      , txt padX (padY + padS + 48) 14 (gray 0.8) "Center line = axis of the strip"
      , txt padX (padY + padS + 70) 14 (gray 0.8) "(horizontal mirror, glide axis)"
      ]

-- えらんだ帯の上に、鏡（実線）・すべり鏡映の軸（点線）・回転の中心（◆）を描く
drawSymmetry :: Int -> Frieze -> [Picture]
drawSymmetry i f = horiz ++ glide ++ concatMap vertical ks
  where
    cy = layerY + fromIntegral i * stripH + stripH / 2
    has t = t `elem` fMaps f
    yellow = hsb 50 90 100
    horiz = [ Color yellow (thickLine 2 (pt (stripX, cy)) (pt (width, cy))) | has (1, -1, 0) ]
    glide = [ dashed 2 yellow (stripX, cy) (width, cy) | has (1, -1, 0.5) ]
    ks = takeWhile (\k -> stripX + fromIntegral k * period / 2 < width) [0 :: Int ..]
    rot :: Float
    rot | has (-1, -1, 0) = 0
        | has (-1, -1, 0.5) = 0.25
        | otherwise = -1
    vertical k =
      let x = stripX + fromIntegral k * period / 2
          rx = stripX + (fromIntegral k * 0.5 + rot) * period
      in [ Color (hsb 170 80 100) (thickLine 2 (pt (x, cy - stripH / 2)) (pt (x, cy + stripH / 2))) | has (-1, 1, 0) ]
         ++ [ Color (hsb 330 80 100) (Polygon (map pt [(rx, cy - 7), (rx + 7, cy), (rx, cy + 7), (rx - 7, cy)])) | rot >= 0 ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p -> w { wPrev = toP5 p }
  EventMotion p | envDown env -> dragTo (toP5 p) w
                | otherwise -> w { wPrev = toP5 p }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '7' ->
        let i = fromEnum c - fromEnum '1'
        in w { wSel = if wSel w == i then -1 else i }
    | c == 'c' || c == 'C' -> repaint w { wSegs = [], wCount = 0 }
    | c == 'p' || c == 'P' -> preset w
  _ -> w

dragTo :: Point -> World -> World
dragTo (mx, my) w
  | mx < padX || mx > padX + padS || my < padY || my > padY + padS = w { wPrev = (mx, my) }
  | dist2 (mx, my) (px, py) < 2 || wCount w >= maxSegs = w
  | otherwise =
      let f x y = ((x - padX) / padS / 2, (y - padY) / padS - 0.5)
          (a, b) = f px py
          (c, d) = f mx my
          s = Seg a b c d (wHue w)
          segs = s : wSegs w
      in w { wSegs = segs, wCount = wCount w + 1, wHue = wHue w + 2
           , wBuf = U.modify (\m -> drawSeg m s) (wBuf w)
           , wPad = padPicture segs, wDirty = True, wPrev = (mx, my) }
  where (px, py) = wPrev w

-- ---------- やってみよう ----------
-- 1. 「F」の模様で 7 本を見比べ、それぞれ F がどう並んでいるか言葉で説明しよう
-- 2. 左右対称な模様（例：「A」）を描くと、いくつかの帯が同じに見える。なぜ？
-- 3. 身のまわりの帯模様（ふすま、着物、建物の飾り）は 7 種類のどれ？
-- 4. 2 つの鏡映を合わせると 180° 回転になる（p2mm の ◆）。1〜7 で確かめよう

-- ---------- 道具 ----------
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Point -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

-- gloss の座標 → p5 の座標（マウス用）
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)

dashed :: Float -> Color -> Point -> Point -> Picture
dashed w c (x1, y1) (x2, y2) =
  let l = dist2 (x1, y1) (x2, y2)
      n = floor (l / 18) :: Int
      p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
  in Color c (Pictures
       [ thickLine w (pt (p (18 * fromIntegral i / l))) (pt (p (min 1 ((18 * fromIntegral i + 10) / l))))
       | i <- [0 .. n] ])
