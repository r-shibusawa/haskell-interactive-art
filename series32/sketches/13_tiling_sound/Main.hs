-- ============================================
-- 13 タイリングを聞く 🔊
-- 光る点がタイリングの上をまっすぐ進み、新しいタイルに入るたびに音が鳴る
--   タイルの形 → 音の高さの範囲、向き → 音階のどの音か
--   正方形・アルキメデス（周期的）はリズムがくり返す。ペンローズは「長い・短い」が くり返さずに混ざる
-- クリック = 音を出す / 1 = 正方形 2 = 3.4.6.4 3 = ペンローズ / マウス上下 = 進む向き / ↑↓ = 速さ / スペース = 止める / S = 保存
-- ============================================
module Main (main) where

import Art
import Control.Monad (when)
import Data.List (foldl', sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

data Poly = Poly { pPts :: [Point], pKind :: !Int, pDir :: !Int }

data World = World
  { wWhich   :: !Int       -- ★ 1 = 正方形、2 = 3.4.6.4、3 = ペンローズ
  , wSpeed   :: !Float     -- ★ 1 フレームに進むピクセル数
  , wPaused  :: !Bool
  , wPolys   :: V.Vector Poly
  , wBuckets :: M.Map (Int, Int) [Int]
  , wLayer   :: Picture
  , wPx, wPy :: !Float
  , wEvents  :: [(Int, Int)]   -- 鳴った音の記録 (フレーム, 形)。先頭が最新
  , wLastHit :: !Int
  , wAudio   :: !Bool
  }

world0 :: World
world0 = World 3 3 False V.empty M.empty Blank 0 0 [] (-1) False

main :: IO ()
main = runSketch (sketch "32-13 Listening to Tilings" world0)
  { skBack  = rgb255 18 20 28
  , skStart = \env -> pure (build env world0)
  , skStep  = stepWorld
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- ★ 音階（ペンタトニック）
scaleHz :: V.Vector Float
scaleHz = V.fromList [220, 247, 277, 330, 370, 440, 494, 554, 659, 740, 880, 988]

kindCol :: Int -> (Float, Float, Float)
kindCol k = case k of
  0 -> (250, 190, 80)
  1 -> (70, 140, 220)
  2 -> (140, 220, 150)
  _ -> (240, 120, 120)

toColor :: Float -> (Float, Float, Float) -> Color
toColor s (r, g, b) = makeColor (r * s / 255) (g * s / 255) (b * s / 255) 1

build :: Env -> World -> World
build env w =
  let wd = envW env
      h = envH env
      s = 56
      polys = case wWhich w of
        1 -> [ Poly [(x, y), (x + s, y), (x + s, y + s), (x, y + s)] 0 0
             | x <- takeWhile (< wd + s) [negate s, 0 ..], y <- takeWhile (< h + s) [negate s, 0 ..] ]
        2 -> makeArch wd h
        _ -> makePenrose wd h
      indexed = zip [0 ..] polys
      cellsOf p =
        let xs = map fst (pPts p)
            ys = map snd (pPts p)
            b v = floor (v / 64) :: Int
        in [ (bx, by) | bx <- [b (foldl' min 1e9 xs) .. b (foldl' max (-1e9) xs)]
                      , by <- [b (foldl' min 1e9 ys) .. b (foldl' max (-1e9) ys)] ]
      buckets = M.fromListWith (flip (++)) [ (c, [i]) | (i, p) <- indexed, c <- cellsOf p ]
      -- 線の太さ 1.5 の代わりに、少し小さく塗って背景を見せる
      tilePic p = Color (toColor (0.55 + 0.1 * fromIntegral (pDir p `mod` 5)) (kindCol (pKind p))) (Polygon (shrink 0.75 (pPts p)))
  in w { wPolys = V.fromList polys, wBuckets = buckets
       , wLayer = Pictures (map tilePic polys)
       , wPx = 0, wPy = h / 2, wEvents = [] , wLastHit = -1 }

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | wPaused w = pure w
  | otherwise = do
      let heading = headingOf env
          wd = envW env
          h = envH env
          x0 = wPx w + cos heading * wSpeed w
          y0 = wPy w + sin heading * wSpeed w
          x1 = if x0 > wd then x0 - wd else x0
          y1 | y0 < 100 = y0 + h - 200
             | y0 > h - 100 = y0 - (h - 200)
             | otherwise = y0
          hit = findPoly w x1 y1
          w1 = w { wPx = x1, wPy = y1 }
      if hit >= 0 && hit /= wLastHit w
        then case wPolys w V.!? hit of
          Just p -> do
            when (wAudio w) (playTile p)
            pure w1 { wLastHit = hit, wEvents = take 200 ((envFrame env, pKind p) : wEvents w) }
          Nothing -> pure w1
        else pure w1

headingOf :: Env -> Float
headingOf env =
  let my = envH env / 2 - snd (envMouse env)
  in clampF (-0.6) 0.6 (remap my 0 (envH env) (-0.6) 0.6)

-- ★ タイル → 音：形で高さの範囲、向きで音階の位置
playTile :: Poly -> IO ()
playTile p = do
  let baseIdx = case pKind p of { 0 -> 3; 1 -> 0; 2 -> 6; _ -> 8 } :: Int
      f = scaleHz V.! ((baseIdx + pDir p) `mod` V.length scaleHz)
      vol = if pKind p == 1 then 0.25 else 0.15
      wave = case pKind p of { 0 -> Triangle; 1 -> Sine; 2 -> Square; _ -> Sawtooth }
  playTone f vol 0.35 wave

drawAll :: Env -> World -> Picture
drawAll env w =
  let heading = headingOf env
      px = wPx w
      py = wPy w
      current = case wPolys w V.!? wLastHit w of
        Just p | wLastHit w >= 0 -> Color white (thickPath 4 (pPts p ++ take 1 (pPts p)))
        _ -> Blank
      title = case wWhich w of
        1 -> "Squares (periodic)"
        2 -> "3.4.6.4 (periodic)"
        _ -> "Penrose (aperiodic)"
      sub = if wAudio w
        then "Mouse up/down: direction. Up/Down keys: speed " ++ show (round (wSpeed w) :: Int) ++ ". Bottom strip = rhythm"
        else "Click to turn on the sound"
  in Pictures
    [ onP5 env (Pictures
        [ wLayer w
        , current
        , at (px, py) (Color white (disc 7))
        , Color (rgba255 255 255 255 120) (Line [(px, py), (px + cos heading * 60, py + sin heading * 60)])
        , Color (rgba255 0 0 0 170) (box 10 10 720 74) ])
    , drawTimeline env w
    , txt env 22 38 19 white (title ++ "   1-3 to switch")
    , txt env 22 66 15 (gray 0.78) sub
    ]

-- 下の帯：時間の流れ（右が今）。タイルの色の縦線 = 1 音
drawTimeline :: Env -> World -> Picture
drawTimeline env w =
  let wd = envW env
      y = envH env - 70
      h = 50
      span' = 360 :: Int
      bars = [ Color (toColor 1 (kindCol k)) (box x y 4 h)
             | (f, k) <- wEvents w
             , let age = envFrame env - f
             , age <= span'
             , let x = wd - 20 - fromIntegral age / fromIntegral span' * (wd - 40) ]
      -- 音と音の間隔（短い / 長い）を数える
      frames = map fst (wEvents w)
      gaps = take 30 (zipWith (-) frames (drop 1 frames))
      kinds = S.size (S.fromList (map (\g -> (g + 1) `div` 2) gaps))
  in Pictures
    [ onP5 env (Pictures (Color (rgba255 0 0 0 180) (box 0 (y - 10) wd (h + 20)) : bars))
    , txt env 20 (y - 16) 14 (gray 0.86) ("Kinds of gaps in the last 30 notes: about " ++ show kinds)
    ]

findPoly :: World -> Float -> Float -> Int
findPoly w x y =
  let list = M.findWithDefault [] (floor (x / 64), floor (y / 64)) (wBuckets w)
      hits = [ i | i <- list, Just p <- [wPolys w V.!? i], insidePoly x y (pPts p) ]
  in case hits of
       (i : _) -> i
       [] -> -1

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w0 = case ev of
  EventKey (MouseButton _) Down _ _ -> pure w0 { wAudio = True }
  EventKey k Down _ _ -> keyDown env k w0 { wAudio = True }
  _ -> pure w0

keyDown :: Env -> Key -> World -> IO World
keyDown env k w = case k of
  Char c
    | c >= '1' && c <= '3' -> pure (build env w { wWhich = fromEnum c - 48 })
    | c == 's' || c == 'S' -> saveSvg "tiling_sound" env (drawAll env w) >> pure w
  SpecialKey KeyUp -> pure w { wSpeed = min 12 (wSpeed w + 1) }
  SpecialKey KeyDown -> pure w { wSpeed = max 1 (wSpeed w - 1) }
  SpecialKey KeySpace -> pure w { wPaused = not (wPaused w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 正方形で向きを水平（マウスを真ん中）にすると、完全に一定のリズム。少し傾けると？
-- 2. ペンローズの帯を見ると、間隔は「長い」と「短い」がくり返さずに混ざる（06 のフィボナッチ語と同じ）
-- 3. scaleHz を別の音階に変えて、タイリングで作曲しよう
-- 4. playTile を改造し、タイルの面積で音の長さを変えよう


-- ==================================================
-- ここから下は「タイリングの道具」
-- ==================================================
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

-- 中心へ向かって少し縮める（どの辺も中心から同じ距離の形なら、すき間は同じ太さになる）
shrink :: Float -> [Point] -> [Point]
shrink d pts =
  let n = fromIntegral (length pts) :: Float
      cx = sum (map fst pts) / n
      cy = sum (map snd pts) / n
      edgeDist ((ax, ay), (bx, by)) =
        let l = max 1e-6 (sqrt ((bx - ax) * (bx - ax) + (by - ay) * (by - ay)))
        in abs ((bx - ax) * (cy - ay) - (by - ay) * (cx - ax)) / l
      ap = case map edgeDist (zip pts (drop 1 pts ++ take 1 pts)) of
        (v : _) -> max 1 v
        [] -> 1
      k = max 0 (1 - d / ap)
  in [ (cx + (x - cx) * k, cy + (y - cy) * k) | (x, y) <- pts ]

insidePoly :: Float -> Float -> [Point] -> Bool
insidePoly x y poly = odd (length (filter crosses (zip poly (drop 1 poly ++ take 1 poly))))
  where
    crosses ((ax, ay), (bx, by)) = (ay > y) /= (by > y) && x < (bx - ax) * (y - ay) / (by - ay) + ax

-- 3.4.6.4：六角形を格子に置き、間に正方形と三角形
makeArch :: Float -> Float -> [Poly]
makeArch wd h =
  let s = 34
      s3 = sqrt 3
      l = s3 + 1
      motif = [ (0, 0, 6, pi / 6, 2), (l / 2, 0, 4, pi / 4, 1), (l / 4, l * s3 / 4, 4, pi / 4 + pi / 3, 1)
              , (3 * l / 4, l * s3 / 4, 4, pi / 4 + 2 * pi / 3, 1), (l / 2, l * s3 / 6, 3, pi / 2, 0), (l, l * s3 / 3, 3, negate pi / 2, 0) ]
             :: [(Float, Float, Int, Float, Int)]
  in [ Poly [ (cx + r * cos (rot + tau * fromIntegral k / fromIntegral n), cy + r * sin (rot + tau * fromIntegral k / fromIntegral n)) | k <- [0 .. n - 1] ] kind (m `mod` 3)
     | i <- [-12 .. 12 :: Int], j <- [-8 .. 8 :: Int]
     , (m, (x, y, n, rot, kind)) <- zip [0 :: Int ..] motif
     , let cx = wd / 2 + (x + fromIntegral i * l + fromIntegral j * l / 2) * s
           cy = h / 2 + (y + fromIntegral j * l * s3 / 2) * s
     , cx >= -60 && cx <= wd + 60 && cy >= -60 && cy <= h + 60
     , let r = s / (2 * sin (pi / fromIntegral n)) ]

type D2 = (Double, Double)
data Tri = Tri !Int !D2 !D2 !D2

-- ペンローズ：05 と同じ分割。三角形 2 枚を 1 枚のひし形にまとめる
makePenrose :: Float -> Float -> [Poly]
makePenrose wf hf =
  let phi = (1 + sqrt 5) / 2 :: Double
      wd = realToFrac wf :: Double
      h = realToFrac hf :: Double
      r = sqrt (wd * wd + h * h) * 0.6
      pt k = (r * cos (fromIntegral k * pi / 10), r * sin (fromIntegral k * pi / 10))
      wheel i = if even i then Tri 0 (0, 0) (pt (2 * i + 1)) (pt (2 * i - 1))
                else Tri 0 (0, 0) (pt (2 * i - 1)) (pt (2 * i + 1 :: Int))
      l2 (ax, ay) (bx, by) t = (ax + (bx - ax) * t, ay + (by - ay) * t)
      one (Tri 0 a b c) = let p = l2 a b (1 / phi) in [Tri 0 c p b, Tri 1 p c a]
      one (Tri _ a b c) =
        let q = l2 b a (1 / phi)
            s = l2 b c (1 / phi)
        in [Tri 1 s c a, Tri 1 q s b, Tri 0 s q a]
      tris = foldr (\_ t -> concatMap one t) (map wheel [0 .. 9 :: Int]) [1 .. 6 :: Int]
      keyOf c b d = (c, sort [rnd b, rnd d])
      rnd (x, y) = (round (x * 10) :: Int, round (y * 10) :: Int)
      half = foldl' (\m (Tri c a b d) -> M.insertWith (\_ (as, b0, d0, c0) -> (a : as, b0, d0, c0)) (keyOf c b d) ([a], b, d, c) m) M.empty tris
      toPoly (as, b, d, c) =
        let raw = case as of
              [a0, a1] -> [a0, b, a1, d]
              (a0 : _) -> [a0, b, d]
              [] -> [b, d]
            pts = [ (realToFrac (x + wd / 2), realToFrac (y + h / 2)) | (x, y) <- raw ] :: [Point]
            out (x, y) = x < -50 || x > wf + 50 || y < -50 || y > hf + 50
            ang = atan2 (snd d - snd b) (fst d - fst b)
            dir = (round (ang / (pi / 5)) :: Int) `mod` 5    -- ひし形の向きは 5 通り
        in if all out pts then [] else [Poly pts (if c == 0 then 3 else 1) dir]
  in concatMap toPoly (M.elems half)
