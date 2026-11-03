-- ============================================
-- 15 カメラでモザイク 📷
-- カメラの映像を、タイリングのモザイクで描き直す。1 枚のタイルの色 = その中心の映像の色
--   六角形・三角形・ペンローズのひし形・トルシェ（明るさで向きが変わる）で見え方がどう変わるか比べよう
-- 1 = 六角形 2 = 三角形 3 = ペンローズ 4 = トルシェ / ↑↓ = タイルの大きさ / G = すき間 / スペース = 映像を止める / S = 保存
-- （カメラが使えないときは、動く模様の画像で代わりに動きます）
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import Data.List (foldl', sort)
import qualified Data.Map.Strict as M
import Data.Word (Word8)

vw, vh :: Int
vw = 160                -- カメラの解像度（小さいほど速い）
vh = 120

data Tile = Tile { tPts :: [Point], tGap :: [Point], tCx, tCy :: !Float }

data World = World
  { wMode   :: !Int     -- ★ タイリングの種類
  , wSize   :: !Float   -- ★ タイルの大きさ（ピクセル）
  , wGapOn  :: !Bool
  , wFrozen :: !Bool
  , wCam    :: Maybe Camera
  , wLive   :: !Bool
  , wFrame  :: !B.ByteString
  , wTiles  :: [Tile]
  , wCols, wRows :: !Int      -- トルシェの列数・行数
  , wW4     :: !Float         -- トルシェの 1 枚の大きさ
  }

world0 :: World
world0 = World 1 22 True False Nothing False B.empty [] 0 0 1

main :: IO ()
main = runSketch (sketch "32-15 Webcam Mosaic" world0)
  { skBack  = gray 0.05
  , skStart = \env -> do
      cam <- startCamera vw vh
      pure (build env world0 { wCam = Just cam })
  , skStep  = stepWorld
  , skDraw  = drawAll
  , skEvent = onEvent
  }

stepWorld :: Env -> Float -> World -> IO World
stepWorld _ _ w = case wCam w of
  Just cam | not (wFrozen w) -> do
    f <- cameraFrame cam
    live <- camLive cam
    pure w { wFrame = f, wLive = live }
  _ -> pure w

-- ★ タイルの形を作る（画面の座標で）
build :: Env -> World -> World
build env w =
  let s = wSize w
      wd = envW env
      h = envH env
      s3 = sqrt 3
  in case wMode w of
       1 ->
         let tw = s3 * s
             th = 1.5 * s
         in w { wTiles =
                  [ polyTile (fromIntegral c * tw + (if odd r then tw / 2 else 0)) (fromIntegral r * th) s 6 (pi / 6)
                  | r <- takeWhile (\r -> fromIntegral r * th < h + s) [-1 :: Int ..]
                  , c <- takeWhile (\c -> fromIntegral c * tw < wd + tw) [-1 :: Int ..] ] }
       2 ->
         let th = s3 / 2 * s * 1.6
             tw = s * 1.6
         in w { wTiles =
                  [ mkTile pts x (y + (if up then 2 * th / 3 else th / 3))
                  | r <- takeWhile (\r -> fromIntegral r * th < h + th) [-1 :: Int ..]
                  , c <- takeWhile (\c -> fromIntegral c * tw / 2 < wd + tw) [-2 :: Int ..]
                  , let up = even (c + r)
                        x = fromIntegral c * tw / 2
                        y = fromIntegral r * th
                        pts = if up then [(x - tw / 2, y + th), (x + tw / 2, y + th), (x, y)]
                              else [(x - tw / 2, y), (x + tw / 2, y), (x, y + th)] ] }
       3 -> w { wTiles = penroseTiles wd h (s * 1.4) }
       _ ->
         let tw = s * 1.6
         in w { wTiles = [], wW4 = tw, wCols = ceiling ((wd + tw) / tw), wRows = ceiling ((h + tw) / tw) }

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      px = wFrame w
      ok = B.length px >= vw * vh * 3
      -- タイルの中心 → 映像の画素
      sample x y =
        let u = max 0 (min (vw - 1) (floor (x / wd * fromIntegral vw)))
            v = max 0 (min (vh - 1) (floor (y / h * fromIntegral vh)))
            i = 3 * (v * vw + u)
        in if ok then (B.index px i, B.index px (i + 1), B.index px (i + 2)) else (0, 0, 0)
      fillOf (r, g, b) = makeColorI (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
      tiles = [ Color (fillOf (sample (tCx t) (tCy t))) (Polygon (if wGapOn w then tGap t else tPts t)) | t <- wTiles w ]
      name = case wMode w of
        1 -> "Hexagon"
        2 -> "Triangle"
        3 -> "Penrose rhombus"
        _ -> "Truchet (brightness = direction)"
      nTiles = if wMode w == 4 then wCols w * wRows w else length (wTiles w)
      srcMsg = if wLive w then "Camera" else "No camera -> demo pattern"
      body | wMode w == 4 = drawTruchet env w sample
           | otherwise = onP5 env (Pictures tiles)
  in Pictures
    [ body
    , onP5 env (Color (rgba255 0 0 0 170) (box 10 10 700 70))
    , txt env 22 38 19 white (name ++ " mosaic (1-4)   " ++ show nTiles ++ " tiles")
    , txt env 22 64 15 (gray 0.78) (srcMsg ++ "   Up/Down size " ++ show (round (wSize w) :: Int) ++ "   G gaps   Space freeze")
    ]

-- トルシェ：明るい所と暗い所で 4 分の 1 円の向きを変え、暗いほど線を太く
drawTruchet :: Env -> World -> (Float -> Float -> (Word8, Word8, Word8)) -> Picture
drawTruchet env w sample =
  let tw = wW4 w
      cols = wCols w
      rows = wRows w
      center i j = (fromIntegral i * tw + tw / 2, fromIntegral j * tw + tw / 2)
      -- 地の色は 1 枚 = 1 画素の画像にして拡大する
      dim v = floor (fromIntegral v * 0.35 :: Float)
      ground = pixelImage cols rows tw (\i j ->
        let (cx, cy) = center i j
            (r, g, b) = sample cx cy
        in (dim r, dim g, dim b))
      gx = fromIntegral cols * tw / 2 - envW env / 2
      gy = envH env / 2 - fromIntegral rows * tw / 2
      h = tw / 2
      arcsOf i j =
        let (cx, cy) = center i j
            (r, g, b) = sample cx cy
            br = (fromIntegral r + fromIntegral g + fromIntegral b) / 3 :: Float
            th = max 1 (tw * (255 - br) / 255 * 0.45 + 1)
            col = makeColorI (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
            arcAt (x, y) a0 a1 = Translate x y (ThickArc a0 a1 h th)
        in Color col (Pictures
             (if br > 110
                then [arcAt (cx - h, cy - h) 0 90, arcAt (cx + h, cy + h) 180 270]
                else [arcAt (cx + h, cy - h) 90 180, arcAt (cx - h, cy + h) 270 360]))
  in Pictures
    [ Translate gx gy ground
    , onP5 env (Pictures [ arcsOf i j | j <- [0 .. rows - 1], i <- [0 .. cols - 1] ])
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (build env w { wSize = min 80 (wSize w + 4) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (build env w { wSize = max 10 (wSize w - 4) })
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wFrozen = not (wFrozen w) }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> pure (build env w { wMode = fromEnum c - 48 })
    | c == 'g' || c == 'G' -> pure w { wGapOn = not (wGapOn w) }
    | c == 's' || c == 'S' -> saveSvg "mosaic" env (drawAll env w) >> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 同じ大きさで 1〜3 を切りかえ、顔がいちばん分かりやすいタイリングはどれ？
-- 2. ↑ でタイルを大きくしていくと、どこで「誰だか分からなく」なる？
-- 3. トルシェ（4）は明るさで向きが変わる。手を動かすと模様の流れが変わるのを楽しもう
-- 4. 色を中心の 1 画素ではなく、タイルの中の平均にすると？（ヒント：まわりの 4 画素を足して 4 で割る）


-- ==================================================
-- ここから下は「タイルの道具」
-- ==================================================
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

mkTile :: [Point] -> Float -> Float -> Tile
mkTile pts cx cy = Tile pts (shrink 0.75 cx cy pts) cx cy

polyTile :: Float -> Float -> Float -> Int -> Float -> Tile
polyTile cx cy r n rot =
  mkTile [ (cx + r * cos a, cy + r * sin a) | k <- [0 .. n - 1], let a = rot + tau * fromIntegral k / fromIntegral n ] cx cy

-- すき間の太さ 1.5 の代わりに、中心へ向かって少し縮める
shrink :: Float -> Float -> Float -> [Point] -> [Point]
shrink d cx cy pts =
  let edgeDist ((ax, ay), (bx, by)) =
        let l = max 1e-6 (sqrt ((bx - ax) * (bx - ax) + (by - ay) * (by - ay)))
        in abs ((bx - ax) * (cy - ay) - (by - ay) * (cx - ax)) / l
      ap = case map edgeDist (zip pts (drop 1 pts ++ take 1 pts)) of
        (v : _) -> max 1 v
        [] -> 1
      k = max 0 (1 - d / ap)
  in [ (cx + (x - cx) * k, cy + (y - cy) * k) | (x, y) <- pts ]

type D2 = (Double, Double)
data Tri = Tri !Int !D2 !D2 !D2

-- ペンローズ（05 と同じ分割）。辺の長さがだいたい edge になるまで分割する
penroseTiles :: Float -> Float -> Float -> [Tile]
penroseTiles wf hf edge =
  let phi = (1 + sqrt 5) / 2 :: Double
      wd = realToFrac wf :: Double
      h = realToFrac hf :: Double
      r = sqrt (wd * wd + h * h) * 0.55
      lv = max 1 (round (log (r / realToFrac edge) / log phi)) :: Int
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
      -- 三角形 2 枚（辺 B−C を共有）を 1 枚のひし形にまとめる
      rnd (x, y) = (round (x * 10) :: Int, round (y * 10) :: Int)
      keyOf c b d = (c, sort [rnd b, rnd d])
      m = foldl' (\acc (Tri c a b d) -> M.insertWith (\_ (as, b0, d0) -> (a : as, b0, d0)) (keyOf c b d) ([a], b, d) acc) M.empty tris
      toTile (as, b, d) =
        let raw = case as of
              [a0, a1] -> [a0, b, a1, d]
              (a0 : _) -> [a0, b, d]
              [] -> [b, d]
            pts = [ (realToFrac (x + wd / 2), realToFrac (y + h / 2)) | (x, y) <- raw ] :: [Point]
            n = fromIntegral (length pts)
            cx = sum (map fst pts) / n
            cy = sum (map snd pts) / n
        in if cx < -60 || cx > wf + 60 || cy < -60 || cy > hf + 60 then [] else [mkTile pts cx cy]
  in concatMap toTile (M.elems m)
