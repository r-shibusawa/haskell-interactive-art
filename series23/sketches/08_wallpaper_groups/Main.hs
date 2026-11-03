-- ============================================
-- 08 壁紙模様（17 の壁紙群のうち 9 つ）
-- マウスで描いた線が、対称性（並進・回転・鏡映・すべり鏡映）で平面いっぱいにコピーされる
--   平面をくり返し埋める模様の対称性はちょうど 17 種類しかない（壁紙群）
-- ドラッグ = 描く / 1〜9 = p1 p2 pm pg cm p4 p4m p3 p6m / ↑↓ = 単位の大きさ / G = 単位の枠 / C = 消す / S = 保存
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word (Word8)

-- コピーの作り方（x' = a x + b y + e, y' = c x + d y + f）
data Aff = Aff !Float !Float !Float !Float !Float !Float

-- 壁紙群：周期の長方形 W×H（単位の大きさ = 1）と、その中のコピー
data Grp = Grp { gName :: String, gW :: !Float, gH :: !Float, gMaps :: [Aff], gNote :: String }

r3 :: Float
r3 = sqrt 3

identity :: Aff
identity = Aff 1 0 0 1 0 0

rotations :: Int -> [Aff]
rotations m = [ Aff c (-s) s c 0 0 | i <- [0 .. m - 1]
              , let a = fromIntegral i * tau / fromIntegral m, let c = cos a, let s = sin a ]

-- 先に変換してから x を反転
mirrorX :: Aff -> Aff
mirrorX (Aff a b c d e f) = Aff (-a) (-b) c d (-e) f

withCenter :: [Aff] -> [Aff]
withCenter ms = ms ++ [Aff a b c d (e + 0.5) (f + r3 / 2) | Aff a b c d e f <- ms]

-- ★ 壁紙群の一覧（残りの 8 種類もここに足せる）
groups :: [Grp]
groups =
  [ Grp "p1" 1 1 [identity] "translation only"
  , Grp "p2" 1 1 [identity, Aff (-1) 0 0 (-1) 0 0] "180 deg rotation"
  , Grp "pm" 1 1 [identity, Aff (-1) 0 0 1 0 0] "mirror (vertical axis)"
  , Grp "pg" 1 1 [identity, Aff (-1) 0 0 1 0 0.5] "glide reflection (mirror, then shift by half)"
  , Grp "cm" 1 1 [identity, Aff (-1) 0 0 1 0 0, Aff 1 0 0 1 0.5 0.5, Aff (-1) 0 0 1 0.5 0.5] "mirror + rhombic lattice"
  , Grp "p4" 1 1 (rotations 4) "90 deg rotation"
  , Grp "p4m" 1 1 (rotations 4 ++ map mirrorX (rotations 4)) "90 deg rotation + mirror (symmetry of a square)"
  , Grp "p3" 1 r3 (withCenter (rotations 3)) "120 deg rotation (hexagonal lattice)"
  , Grp "p6m" 1 r3 (withCenter (rotations 6 ++ map mirrorX (rotations 6))) "60 deg rotation + mirror (symmetry of a snowflake)"
  ]

-- 描いた線分（単位の座標）と色相
data Seg = Seg !Float !Float !Float !Float !Float

data World = World
  { wGroup :: !Int              -- 壁紙群の番号
  , wUnit  :: !Float            -- ★ 単位の大きさ（px）
  , wSegs  :: ![Seg]            -- 描いた線分（新しい順）
  , wCount :: !Int
  , wGrid  :: !Bool
  , wHue   :: !Float
  , wBuf   :: !(U.Vector Word8) -- 周期の長方形 1 枚分の絵（R, G, B の順）
  , wTiles :: !Picture          -- 画面全体にしきつめた絵
  , wDirty :: !Bool             -- 絵を作り直す必要があるか
  , wPrev  :: !Point            -- 前のマウス位置（p5 の pmouseX, pmouseY）
  }

maxSegs :: Int
maxSegs = 3000

main :: IO ()
main = runSketch (sketch "23-08 Wallpaper Groups" world0)
  { skBack  = rgb255 0 0 0
  , skStep  = \_ _ w -> pure (if wDirty w then refresh w else w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = onEvent
  }

world0 :: World
world0 = refresh (World 6 150 (reverse presetMotif) (length presetMotif) True 0 U.empty Blank True (0, 0))

-- 最初から入っている非対称な模様（C で消して自分で描こう）
presetMotif :: [Seg]
presetMotif =
  let ps = [ (0.12 + 0.3 * t, 0.1 + 0.25 * t * t) | i <- [0 .. 19 :: Int], let t = fromIntegral i * 0.05 ]
  in [ Seg a b c d (20 + fromIntegral i * 8) | (i, (a, b), (c, d)) <- zip3 [1 :: Int ..] ps (drop 1 ps) ]
     ++ [Seg 0.42 0.35 0.3 0.42 200, Seg 0.3 0.42 0.36 0.2 200]

currentGroup :: World -> Grp
currentGroup w = case drop (wGroup w) groups of
  (g : _) -> g
  [] -> Grp "p1" 1 1 [identity] ""

-- 周期の長方形の大きさ（画面のピクセル）
cellSize :: World -> (Int, Int)
cellSize w =
  let g = currentGroup w
  in (max 4 (round (gW g * wUnit w)), max 4 (round (gH g * wUnit w)))

-- 絵のピクセル数（8 万画素を超えるときは粗くする）
bufSize :: World -> (Int, Int, Float)
bufSize w =
  let (cw, ch) = cellSize w
      rk = max 1 (sqrt (fromIntegral (cw * ch) / 80000)) :: Float
  in (ceiling (fromIntegral cw / rk), ceiling (fromIntegral ch / rk), rk)

background :: (Word8, Word8, Word8)
background = rgb8 (hsb 230 25 14)

rgb8 :: Color -> (Word8, Word8, Word8)
rgb8 c = let (r, g, b, _) = rgbaOfColor c in (q r, q g, q b)
  where q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))

-- 周期の長方形を全部描き直す
repaint :: World -> World
repaint w =
  let (bw, bh, rk) = bufSize w
      g = currentGroup w
      (br, bg, bb) = background
      blankBuf = U.generate (bw * bh * 3) (\i -> case i `mod` 3 of
        0 -> br
        1 -> bg
        _ -> bb)
      segsOld = reverse (wSegs w)
  in w { wBuf = U.modify (\m -> forM_ segsOld (drawSeg g bw bh rk m)) blankBuf, wDirty = True }

-- 1 本の線分を、すべてのコピー × となりの周期（はみ出し分）に描く
drawSeg :: Grp -> Int -> Int -> Float -> M.MVector s Word8 -> Seg -> ST s ()
drawSeg g bw bh rk buf (Seg sx1 sy1 sx2 sy2 hue) = do
  let sx = fromIntegral bw / gW g
      sy = fromIntegral bh / gH g
      col = rgb8 (hsb (wrapF 360 hue) 70 100)
  forM_ (gMaps g) $ \(Aff a b c d e f) -> do
    let x1 = a * sx1 + b * sy1 + e
        y1 = c * sx1 + d * sy1 + f
        x2 = a * sx2 + b * sy2 + e
        y2 = c * sx2 + d * sy2 + f
        -- 周期の長方形の中に寄せてから、まわり 3×3 にも描く
        ox = fromIntegral (floor (x1 / gW g) :: Int) * gW g
        oy = fromIntegral (floor (y1 / gH g) :: Int) * gH g
    forM_ [-1, 0, 1] $ \i -> forM_ [-1, 0, 1] $ \j -> do
      let dx = i * gW g - ox
          dy = j * gH g - oy
      plotLine bw bh (3 / rk) col buf ((x1 + dx) * sx) ((y1 + dy) * sy) ((x2 + dx) * sx) ((y2 + dy) * sy)

-- 太さ lw の線分をなめらかにピクセルへ描く（線分の周りの四角だけ調べる）
plotLine :: Int -> Int -> Float -> (Word8, Word8, Word8) -> M.MVector s Word8
         -> Float -> Float -> Float -> Float -> ST s ()
plotLine bw bh lw (cr, cg, cb) buf !x1 !y1 !x2 !y2 =
  when (x0 <= xN && y0 <= yN) (goY y0)
  where
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

-- バッファから、しきつめる絵を作る。小さな単位は何枚か並べた 1 枚にして、画像の数を減らす
refresh :: World -> World
refresh w0 =
  let w = if U.null (wBuf w0) then repaint w0 else w0
      (cw, ch) = cellSize w
      (bw, bh, rk) = bufSize w
      rx = if rk > 1 then 1 else max 1 (200 `div` cw)
      ry = if rk > 1 then 1 else max 1 (200 `div` ch)
      buf = wBuf w
      f x y = let i = ((y `mod` bh) * bw + x `mod` bw) * 3
              in (U.unsafeIndex buf i, U.unsafeIndex buf (i + 1), U.unsafeIndex buf (i + 2))
      ibw = bw * rx
      ibh = bh * ry
      tw = fromIntegral (cw * rx) :: Float
      th = fromIntegral (ch * ry) :: Float
      img = Scale (tw / fromIntegral ibw) (th / fromIntegral ibh) (pixelImage ibw ibh 1 f)
      tiles = [ at (pt (x + tw / 2, y + th / 2)) img
              | y <- takeWhile (< height) [0, th ..], x <- takeWhile (< width) [0, tw ..] ]
  in w { wTiles = Pictures tiles, wDirty = False }

drawWorld :: World -> Picture
drawWorld w = Pictures ([wTiles w] ++ grid ++ hud)
  where
    g = currentGroup w
    (cw, ch) = cellSize w
    line' a b = Color (makeColor 1 1 1 0.25) (Line [pt a, pt b])
    grid | wGrid w = [ line' (0, y) (width, y) | y <- takeWhile (< height) [0, fromIntegral ch ..] ]
                  ++ [ line' (x, 0) (x, height) | x <- takeWhile (< width) [0, fromIntegral cw ..] ]
         | otherwise = []
    names = intercalate "  " [ show i ++ " " ++ gName gg | (i, gg) <- zip [1 :: Int ..] groups ]
    hud =
      [ box 0 0 width 104 (makeColor 0 0 0 0.6)
      , txt 20 26 16 white "Drag: draw   Up/Down: size   G: grid   C: clear   S: save"
      , txt 20 50 16 white ("1-9: " ++ names)
      , txt 20 90 26 (hsb 50 80 100) (gName g)
      , txt 90 88 15 (gray 0.9) (gNote g ++ "  |  copies in one period: " ++ show (length (gMaps g)) ++ "  |  17 plane patterns in all")
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p -> pure w { wPrev = toP5 p }
  EventMotion p | envDown env -> pure (dragTo (toP5 p) w)
                | otherwise -> pure w { wPrev = toP5 p }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '9' -> pure (repaint w { wGroup = fromEnum c - fromEnum '1' })
    | c == 'g' || c == 'G' -> pure w { wGrid = not (wGrid w) }
    | c == 'c' || c == 'C' -> pure (repaint w { wSegs = [], wCount = 0 })
    | c == 's' || c == 'S' -> do
        saveSvg ("wallpaper_" ++ gName (currentGroup w)) env (drawWorld w)
        pure w
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (repaint w { wUnit = min 400 (wUnit w * 1.15) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (repaint w { wUnit = max 50 (wUnit w / 1.15) })
  _ -> pure w

-- ドラッグ：線分を足して、絵にも描き足す
dragTo :: Point -> World -> World
dragTo (mx, my) w
  | my < 104 = w { wPrev = (mx, my) }
  | dist2 (mx, my) (px, py) < 2 || wCount w >= maxSegs = w
  | otherwise =
      let g = currentGroup w
          (cw, ch) = cellSize w
          sx = fromIntegral cw / gW g
          sy = fromIntegral ch / gH g
          s = Seg (px / sx) (py / sy) (mx / sx) (my / sy) (wHue w)
          (bw, bh, rk) = bufSize w
      in w { wSegs = s : wSegs w, wCount = wCount w + 1, wHue = wrapF 360 (wHue w + 1.5)
           , wBuf = U.modify (\m -> drawSeg g bw bh rk m s) (wBuf w)
           , wDirty = True, wPrev = (mx, my) }
  where (px, py) = wPrev w

-- ---------- やってみよう ----------
-- 1. 同じ線のまま 1〜9 を切りかえて、どこに鏡・回転の中心があるか探そう
-- 2. pm と pg の違いは？（鏡に映すだけか、映して半分ずらすか）。足あとを描くと pg がよくわかる
-- 3. p3 と p6m の周期は正方形でなく 1 × √3 の長方形（六角形の格子）。G で枠を見よう
-- 4. 残りの 8 種類（pmm, pmg, pgg, cmm, p4g, p3m1, p31m, p6）を groups に足してみよう
-- 5. エッシャーのように「すき間なく埋まる形」を描けるかな？ S で保存

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
