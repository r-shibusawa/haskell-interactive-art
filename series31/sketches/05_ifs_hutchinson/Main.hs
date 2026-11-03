-- ============================================
-- 05 反復関数系（IFS）と自己相似図形
-- 縮小写像を何枚か用意し、図形 A を全部で写して合わせる：H(A) = w₁(A) ∪ w₂(A) ∪ … （ハッチンソン作用素）
--   H は「図形の空間」の上の縮小写像 → 02 の定理で不動点（アトラクタ）がただ 1 つ。どんな図形から始めても同じ形に着く
-- スペース = H を 1 回かける / A = 自動 / 1〜4 = 写像の組 / ドラッグ = 出発の図形を描き足す / B = 出発の図形を変える / C = 消す / G = カオスゲーム / R = 最初から
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (showEFloat, showFFloat)

-- 写像 (a, b, c, d, e, f)：x' = ax + by + e、y' = cx + dy + f
type Aff = (Float, Float, Float, Float, Float, Float)

data Preset = Preset
  { pName :: String
  , pView :: (Float, Float, Float, Float)
  , pMaps :: [Aff]
  }

presetOf :: Int -> Preset
presetOf 0 = Preset "Sierpinski triangle" (-0.05, 1.05, -0.1, 1.0) [(0.5, 0, 0, 0.5, 0, 0), (0.5, 0, 0, 0.5, 0.5, 0), (0.5, 0, 0, 0.5, 0.25, 0.433)]
presetOf 1 = Preset "Barnsley fern" (-3.2, 3.2, -0.3, 10.3) [(0, 0, 0, 0.16, 0, 0), (0.85, 0.04, -0.04, 0.85, 0, 1.6), (0.2, -0.26, 0.23, 0.22, 0, 1.6), (-0.15, 0.28, 0.26, 0.24, 0, 0.44)]
presetOf 2 = Preset "Dragon curve" (-0.45, 1.25, -0.6, 1.1) [(0.5, -0.5, 0.5, 0.5, 0, 0), (-0.5, -0.5, 0.5, -0.5, 1, 0)]
presetOf _ = Preset "Sierpinski carpet" (-0.05, 1.05, -0.05, 1.05)
  -- カーペット：3×3 のうち真ん中以外の 8 枚
  [ (1 / 3, 0, 0, 1 / 3, fromIntegral i / 3, fromIntegral j / 3) | i <- [0 .. 2 :: Int], j <- [0 .. 2 :: Int], i /= 1 || j /= 1 ]

maxPts, maxStart, res, thumbRes :: Int
maxPts = 60000       -- ★ 点の数の上限（多いほどきれい・重い）
maxStart = 20000     -- 描き足す点の上限
res = 275            -- ★ 図形の画像の解像度（2 倍に拡大して表示する）
thumbRes = 75        -- サムネイルの解像度（2 倍に拡大）

data World = World
  { wPre    :: !Int
  , wStart  :: ![(Float, Float)]      -- 出発の図形（0〜1 の正規化座標の点）
  , wKind   :: !Int
  , wXs     :: !(U.Vector Float)      -- 今の図形 Hⁿ(A) の点
  , wYs     :: !(U.Vector Float)
  , wCs     :: !(U.Vector Int)        -- どの写像から来たか（-1 = 出発の図形）
  , wN      :: !Int
  , wAuto   :: !Bool
  , wChaos  :: !Bool
  , wView   :: !(U.Vector Word32)     -- 今の図形の画像
  , wPic    :: !Picture
  , wThumbs :: ![Picture]             -- これまでの図形（古い順、6 枚まで）
  , wDots   :: !(U.Vector Float)      -- カオスゲームの点の濃さ（0〜1）
  , wCx     :: !Float
  , wCy     :: !Float
  , wRng    :: !Word32
  , wLast   :: !(Maybe (Float, Float))
  }

main :: IO ()
main = runSketch (sketch "31-05 IFS and Hutchinson" world0)
  { skBack  = hsb 230 30 11
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = makeStart World
  { wPre = 0, wStart = [], wKind = 0, wXs = U.empty, wYs = U.empty, wCs = U.empty, wN = 0
  , wAuto = False, wChaos = False, wView = U.empty, wPic = Blank, wThumbs = []
  , wDots = U.replicate (res * res) 0, wCx = 0.3, wCy = 0.3, wRng = 2463534242, wLast = Nothing }

-- 出発の図形：0 = 正方形の枠、1 = 円、2 = 顔、3 = 1 点
makeStart :: World -> World
makeStart w = resetShape w { wStart = shape (wKind w) }
  where
    shape :: Int -> [(Float, Float)]
    shape 0 = [ side e s | i <- [0 .. 399 :: Int], let (e, r) = i `divMod` 100, let s = fromIntegral r / 100 ]
    shape 1 = [ (0.5 + 0.3 * cos a, 0.5 + 0.3 * sin a) | i <- [0 .. 399 :: Int], let a = fromIntegral i / 400 * tau ]
    shape 2 = [ (0.5 + 0.35 * cos a, 0.5 + 0.35 * sin a) | i <- [0 .. 299 :: Int], let a = fromIntegral i / 300 * tau ]
           ++ [ (0.5 + 0.2 * cos a, 0.5 + 0.2 * sin a) | i <- [0 .. 59 :: Int], let a = pi * (1.2 + 0.6 * fromIntegral i / 60) ]
           ++ concat [ [(0.38 + 0.04 * cos a, 0.62 + 0.04 * sin a), (0.62 + 0.04 * cos a, 0.62 + 0.04 * sin a)]
                     | i <- [0 .. 39 :: Int], let a = fromIntegral i / 40 * tau ]
    shape _ = [(0.8, 0.2)]
    side :: Int -> Float -> (Float, Float)
    side 0 s = (s, 0)
    side 1 s = (1, s)
    side 2 s = (1 - s, 1)
    side _ s = (0, 1 - s)

toWorld :: Preset -> (Float, Float) -> (Float, Float)
toWorld p (u, v) = let (x0, x1, y0, y1) = pView p in (lerpF x0 x1 u, lerpF y0 y1 v)

resetShape :: World -> World
resetShape w =
  let p = presetOf (wPre w)
      pts = map (toWorld p) (wStart w)
      xs = U.fromList (map fst pts)
      ys = U.fromList (map snd pts)
  in render w { wN = 0, wThumbs = [], wXs = xs, wYs = ys, wCs = U.replicate (U.length xs) (-1)
              , wDots = U.replicate (res * res) 0 }

-- ★ ハッチンソン作用素：全部の点を全部の写像で写して合わせる
applyH :: World -> World
applyH w =
  let maps = pMaps (presetOf (wPre w))
      xs = wXs w
      ys = wYs w
      nx = U.concat [ U.zipWith (\x y -> a * x + b * y + e) xs ys | (a, b, _, _, e, _) <- maps ]
      ny = U.concat [ U.zipWith (\x y -> c * x + d * y + f) xs ys | (_, _, c, d, _, f) <- maps ]
      nc = U.concat [ U.replicate (U.length xs) m | (m, _) <- zip [0 ..] maps ]
      total = U.length nx
      -- 多すぎたらランダムに間引く（形はほとんど変わらない）
      keep = fromIntegral maxPts / fromIntegral total :: Float
      seed = rnd (wRng w)
      kept i = rnd01 (rnd (rnd (seed `xor` (fromIntegral i * 2654435761)))) < keep
      thin :: U.Unbox a => U.Vector a -> U.Vector a
      thin v = if total > maxPts then U.ifilter (\i _ -> kept i) v else v
  in render w { wXs = thin nx, wYs = thin ny, wCs = thin nc, wN = wN w + 1, wRng = seed }

-- 点を画像に打って、サムネイルも 1 枚足す
render :: World -> World
render w =
  let (x0, x1, y0, y1) = pView (presetOf (wPre w))
      rf = fromIntegral res :: Float
      bg = pack 16 18 30
      white' = pack 255 255 255
      palette = U.fromList [ let (r, g, b) = hsbToRgb (wrapF 360 (fromIntegral m * 70 + 180)) 60 100 in pack r g b | m <- [0 .. 7 :: Int] ]
      view = U.create $ do
        m <- MU.replicate (res * res) bg
        forM_ [0 .. U.length (wXs w) - 1] $ \i -> do
          let px = floor ((wXs w U.! i - x0) / (x1 - x0) * rf) :: Int
              py = floor (rf - (wYs w U.! i - y0) / (y1 - y0) * rf) :: Int
              c = wCs w U.! i
          when (px >= 0 && py >= 0 && px < res && py < res) $
            MU.unsafeWrite m (py * res + px) (if c < 0 then white' else palette U.! c)
        pure m
      thumb = makeThumb view
      thumbs = wThumbs w ++ [thumb]
  in w { wView = view
       , wPic = imagePicture view
       , wThumbs = drop (length thumbs - 6) thumbs }

imagePicture :: U.Vector Word32 -> Picture
imagePicture view = pixelImage res res 2 (\x y -> unpack (view U.! (y * res + x)))

-- サムネイル：小さなブロックの中でいちばん明るい画素を選ぶ（細い線が消えないように）
makeThumb :: U.Vector Word32 -> Picture
makeThumb view = pic `seq` pic
  where
    n = res `div` thumbRes
    pic = pixelImage thumbRes thumbRes 2 $ \x y ->
      let bx = x * res `div` thumbRes
          by = y * res `div` thumbRes
          vals = [ view U.! ((by + j) * res + bx + i) | j <- [0 .. n - 1], i <- [0 .. n - 1] ]
          bright v = let (r, g, b) = unpack v in fromIntegral r + fromIntegral g + fromIntegral b :: Int
          best = foldl' (\a v -> if bright v > bright a then v else a) (pack 16 18 30) vals
      in unpack best

-- 各写像の縮小率（行列の最大特異値）のうち最大のもの
contraction :: Preset -> Float
contraction p = foldl' max 0
  [ sqrt ((s1 + sqrt (max 0 (s1 * s1 - 4 * det * det))) / 2)
  | (a, b, c, d, _, _) <- pMaps p, let s1 = a * a + b * b + c * c + d * d, let det = a * d - b * c ]

-- カオスゲーム：1 点だけを、ランダムに選んだ写像で何度も動かす。それでもアトラクタが浮かぶ
runChaos :: World -> World
runChaos w = runST $ do
  buf <- U.thaw (wDots w)
  (cx, cy, seed) <- loop buf (1500 :: Int) (wCx w) (wCy w) (wRng w)
  dots <- U.unsafeFreeze buf
  pure w { wDots = dots, wCx = cx, wCy = cy, wRng = seed }
  where
    p = presetOf (wPre w)
    maps = U.fromList (pMaps p)
    cnt = U.length maps
    (x0, x1, y0, y1) = pView p
    rf = fromIntegral res :: Float
    loop :: MU.STVector s Float -> Int -> Float -> Float -> Word32 -> ST s (Float, Float, Word32)
    loop _ 0 !cx !cy !s = pure (cx, cy, s)
    loop buf k !cx !cy !s = do
      let s1 = rnd s
          r = rnd01 s1
          m | wPre w == 1 = if r < 0.01 then 0 else if r < 0.86 then 1 else if r < 0.93 then 2 else 3   -- シダは確率つき
            | otherwise = min (cnt - 1) (floor (r * fromIntegral cnt))
          (a, b, c, d, e, f) = maps U.! m
          nx = a * cx + b * cy + e
          ny = c * cx + d * cy + f
          px = floor ((nx - x0) / (x1 - x0) * rf) :: Int
          py = floor (rf - (ny - y0) / (y1 - y0) * rf) :: Int
      when (px >= 0 && py >= 0 && px < res && py < res) $ do
        let i = py * res + px
        v <- MU.unsafeRead buf i
        MU.unsafeWrite buf i (v + (1 - v) * 0.35)
      loop buf (k - 1) nx ny s1

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w =
  let w1 = if wAuto w && envFrame env `mod` 45 == 0 && wN w < 12 then applyH w else w
  in pure (if wChaos w1 then runChaos w1 else w1)

-- レイアウト
vx0, vy0 :: Float
vx0 = 40
vy0 = 100

viewSize :: Float
viewSize = fromIntegral res * 2

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $
  [ imageAt (vx0 + v / 2) (vy0 + v / 2) main'
  , box (hsb 0 0 40) vx0 vy0 v v
  , txt tx (vy0 - 8) 14 (hsb 0 0 70) "Shapes so far (top left is oldest)"
  ]
  ++ concat [ [ imageAt (x + tw / 2) (y + tw / 2) t
              , txt x (y + tw + 16) 14 (hsb 0 0 70) ("H^" ++ show (wN w - cntT + 1 + i) ++ "(A)") ]
            | (i, t) <- zip [0 ..] (wThumbs w)
            , let x = tx + fromIntegral (i `mod` 3) * (tw + 10)
            , let y = vy0 + fromIntegral (i `div` 3) * (tw + 30) ]
  ++ [ txt tx ty 15 white (show (length (pMaps p)) ++ " maps, contraction r = " ++ nf 3 r)
     , txt tx (ty + 26) 15 white ("d(H^n(A), attractor) <= r^n d(A, attractor),  r^n = " ++ showEFloat (Just 2) (r ^ wN w) "")
     , txt tx (ty + 52) 14 (hsb 0 0 60) "d = Hausdorff distance (farthest gap between two shapes)"
     , txt tx (ty + 76) 14 (hsb 0 0 60) ("Points: " ++ show (U.length (wXs w)) ++ (if fromIntegral (U.length (wXs w)) >= (fromIntegral maxPts * 0.9 :: Float) then " (thinning)" else ""))
     , txt 20 34 15 white (show (wPre w + 1) ++ ": " ++ pName p ++ "  n = " ++ show (wN w) ++ "  Space: H  A: auto " ++ onOff (wAuto w) ++ "  Drag: draw  B: start shape  C: clear  G: chaos " ++ onOff (wChaos w) ++ "  R: restart")
     , txt 20 62 14 (hsb 0 0 60) "White = start shape A. Color = which map it came through. Square, face or a single point: all end at the same shape (attractor)."
     ]
  where
    p = presetOf (wPre w)
    v = viewSize
    r = contraction p
    tx = vx0 + v + 40
    tw = min 150 (fromIntegral thumbRes * 2)
    ty = vy0 + 2 * (tw + 30) + 20
    cntT = length (wThumbs w)
    onOff b = if b then "ON" else "off"
    yellow' = (242, 216, 26) :: (Float, Float, Float)
    -- カオスゲームの点は図形の上に重ねて表示する
    main'
      | wChaos w =
          pixelImage res res 2 $ \x y ->
            let i = y * res + x
                a = wDots w U.! i
                (r0, g0, b0) = unpack (wView w U.! i)
                mix c0 c1 = round (fromIntegral c0 + (c1 - fromIntegral c0) * a) :: Word8
                (yr, yg, yb) = yellow'
            in if a == 0 then (r0, g0, b0) else (mix r0 yr, mix g0 yg, mix b0 yb)
      | otherwise = wPic w

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventMotion _ | envDown env -> pure (dragTo env w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wLast = Nothing }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (applyH w)
  EventKey (Char c) Down _ _
    | toLower c == 'a' -> pure w { wAuto = not (wAuto w) }
    | c >= '1' && c <= '4' -> pure (resetShape w { wPre = fromEnum c - fromEnum '1' })
    | toLower c == 'b' -> pure (makeStart w { wKind = (wKind w + 1) `mod` 4 })
    | toLower c == 'c' -> pure (resetShape w { wStart = [] })
    | toLower c == 'g' -> pure w { wChaos = not (wChaos w), wDots = U.replicate (res * res) 0 }
    | toLower c == 'r' -> pure (resetShape w)
  _ -> pure w

-- ドラッグで出発の図形に点を描き足す
dragTo :: Env -> World -> World
dragTo env w
  | mx < vx0 || mx > vx0 + v || my < vy0 || my > vy0 + v = w { wLast = Nothing }
  | otherwise =
      let u = (mx - vx0) / v
          t = 1 - (my - vy0) / v
          added = case wLast w of
            Just l@(lu, lt) ->
              let steps = ceiling (dist2 (u, t) l * 300) :: Int
              in [ (lerpF lu u k, lerpF lt t k) | i <- [1 .. steps], let k = fromIntegral i / fromIntegral steps ]
            Nothing -> [(u, t)]
      in resetShape w { wStart = take maxStart (wStart w ++ added), wLast = Just (u, t) }
  where
    (mx, my) = mouseP env
    v = viewSize

-- ---------- やってみよう ----------
-- 1. B で出発の図形を正方形 → 円 → 顔 → 1 点と変えて、n = 8 の形を比べよう。全部同じになる理由は？
-- 2. シェルピンスキーの 3 枚目の写像の 0.433 を 0.6 にすると形はどう変わる？
-- 3. 写像の 0.5 を 0.7 にすると、写した 3 つが重なる。縮小率 r が大きいと何回で「着く」？
-- 4. presetOf に自分の写像を足して、オリジナルの自己相似図形をつくろう（14 の不動点ブラシで手で作れる）

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5 と同じ座標（左上が原点、y は下向き）で描くための枠
screen :: Env -> [Picture] -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 文字（y 軸を反転してあるので、文字だけ元の向きに戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = Translate x y (Scale 1 (-1) (label 0 0 s c str))

mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

ln :: Float -> Color -> Point -> Point -> Picture
ln w c a b = Color c (thickLine w a b)

-- 塗りつぶした円（d は直径）
spot :: Color -> Point -> Float -> Picture
spot c (x, y) d = Color c (Translate x y (disc (d / 2)))

-- 輪（d は直径、w は太さ）
hoop :: Float -> Color -> Point -> Float -> Picture
hoop w c (x, y) d = Color c (Translate x y (ring (d / 2) w))

-- 左上 (x, y)、幅 w、高さ h の枠
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし
fillBox :: Color -> Float -> Float -> Float -> Float -> Picture
fillBox c x y w h = Color c (rect (x + w / 2) (y + h / 2) w h)

nf :: RealFloat a => Int -> a -> String
nf d v = showFFloat (Just d) v ""

-- 画像を中心 (x, y) に置く（y 軸の反転を打ち消す）
imageAt :: Float -> Float -> Picture -> Picture
imageAt x y = Translate x y . Scale 1 (-1)

-- 速い乱数（xorshift）
rnd :: Word32 -> Word32
rnd s0 = let s1 = s0 `xor` (s0 `shiftL` 13)
             s2 = s1 `xor` (s1 `shiftR` 17)
         in s2 `xor` (s2 `shiftL` 5)

rnd01 :: Word32 -> Float
rnd01 s = fromIntegral (s `shiftR` 8) / 16777216

pack :: Word8 -> Word8 -> Word8 -> Word32
pack r g b = (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

unpack :: Word32 -> (Word8, Word8, Word8)
unpack v = (fromIntegral (v `shiftR` 16), fromIntegral ((v `shiftR` 8) .&. 255), fromIntegral (v .&. 255))

hsbToRgb :: Float -> Float -> Float -> (Word8, Word8, Word8)
hsbToRgb h s b =
  let (r, g, bl, _) = rgbaOfColor (hsb h s b)
      q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))
  in (q r, q g, q bl)
