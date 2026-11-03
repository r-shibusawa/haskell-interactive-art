-- ============================================
-- 14 シームカービング：DP で絵を「賢く」縮める（自由制作のベース）
-- 上から下へ 1 本ずつ「目立たない筋（シーム）」を探して取り除くと、大事なものの形を保ったまま横幅が縮む
-- M[y][x] = エネルギー[y][x] + min( M[y-1][x-1], M[y-1][x], M[y-1][x+1] )　─ 1 行上の 3 マスから来る最小コスト
-- スペース = 自動で削る / → = 1 本削る / ドラッグ = ブラシ（1 守る 2 消す 3 絵の具）/ E = エネルギー表示 / R = 新しい絵
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

iw, ih :: Int
iw = 220       -- ★ 絵の大きさ（ピクセル）。大きいと重くなる
ih = 140

brushSize :: Int
brushSize = 6  -- ★ ブラシの太さ

carveEvery :: Int
carveEvery = 3 -- ★ 自動のとき何フレームごとに 1 本削るか

data World = World
  { wPix     :: !(U.Vector Int)   -- 色（赤・緑・青を 1 つの整数に詰めたもの）。w × ih 個
  , wMask    :: !(U.Vector Int)   -- 1 = 守る、-1 = 消す、0 = なし
  , wWidth   :: !Int              -- いまの横幅
  , wSeam    :: !(U.Vector Int)   -- 次に削るシーム（各行の x）。空なら削れない
  , wCost    :: !Float
  , wAuto    :: !Bool
  , wEnergy  :: !Bool
  , wBrush   :: !Int
  , wRemoved :: !Int
  , wHue     :: !Float
  , wImg     :: Picture           -- 画面に出す絵（変わったときだけ作り直す。p5.js 版の dirty と同じ役目）
  }

main :: IO ()
main = runSketch (sketch "12-14 Seam carving" (paintScene 0))
  { skBack  = cBg
  , skStart = \_ -> newScene
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

packRGB :: Float -> Float -> Float -> Int
packRGB r g b = (q r `shiftL` 16) .|. (q g `shiftL` 8) .|. q b
  where q v = max 0 (min 255 (round v)) :: Int

redOf, greenOf, blueOf :: Int -> Int
redOf c = (c `shiftR` 16) .&. 255
greenOf c = (c `shiftR` 8) .&. 255
blueOf c = c .&. 255

-- ★ ここを書きかえて自分の絵をつくろう（色を返す関数）。s はノイズをずらす乱数
sceneColor :: Float -> Int -> Int -> Int
sceneColor s xi yi
  | y > h * 0.86 = packRGB (40 + 30 * noise2 (x * 0.2 + s) (y * 0.2)) 90 60                 -- 草地
  | tree = packRGB 30 (110 + 40 * noise2 (x * 0.3 + s) (y * 0.3)) 60                         -- 木
  | y > mount = packRGB (70 + 30 * v) (60 + 20 * noise2 (x * 0.05 + s) (y * 0.05) * 3) 90    -- 山
  | sun = packRGB 255 200 90                                                                 -- 太陽
  | otherwise = packRGB (40 + 120 * v) (60 + 80 * v) (140 + 60 * v)                          -- 夕焼けのグラデーション
  where
    x = i2f xi
    y = i2f yi
    w = i2f iw
    h = i2f ih
    u = x / w
    v = y / h
    sun = dist2 (x, y) (w * 0.72, h * 0.32) < h * 0.12
    mount = h * (0.62 - 0.18 * noise2 (u * 3 + s) 0.5)                                       -- 山の稜線
    tree = abs (i2f (xi `mod` 37) - 18) < 4 - (y - h * 0.6) * 0.05
           && y > h * 0.55 && y < h * 0.86 && xi `mod` 74 < 37

paintScene :: Float -> World
paintScene s = refresh $ findSeam World
  { wPix = U.generate (iw * ih) (\i -> let (y, x) = i `quotRem` iw in sceneColor s x y)
  , wMask = U.replicate (iw * ih) 0
  , wWidth = iw, wSeam = U.empty, wCost = 0, wAuto = False, wEnergy = False
  , wBrush = 1, wRemoved = 0, wHue = 0, wImg = Blank }

newScene :: IO World
newScene = paintScene <$> randomRIO (0, 200)

-- 明るさの変化が大きいほどエネルギーが高い（守るマスクは +2000、消すマスクは −2000）
energyAt :: World -> Int -> Int -> Float
energyAt wd x y =
  let w = wWidth wd
      p xx yy = wPix wd U.! (yy * w + xx)
      l = p (max 0 (x - 1)) y
      r = p (min (w - 1) (x + 1)) y
      a = p x (max 0 (y - 1))
      b = p x (min (ih - 1) (y + 1))
      d f g h = abs (f g - f h)
      e = d redOf l r + d greenOf l r + d blueOf l r + d redOf a b + d greenOf a b + d blueOf a b
  in i2f (e + (wMask wd U.! (y * w + x)) * 2000)

-- ★ DP：上の行から順に、そのマスまでの最小コスト M を埋めて、一番下の最小から逆にたどる
findSeam :: World -> World
findSeam wd
  | w < 3 = wd { wSeam = U.empty }
  | otherwise = wd { wSeam = U.fromList (back (ih - 1) bottom []), wCost = m U.! ((ih - 1) * w + bottom) }
  where
    w = wWidth wd
    m :: U.Vector Float
    m = U.constructN (w * ih) $ \prev ->
      let i = U.length prev
          (y, x) = i `quotRem` w
          e = energyAt wd x y
      in if y == 0 then e
         else let up dx = prev U.! ((y - 1) * w + x + dx)
              in e + snd (bestFrom up x)
    -- 1 行上の 3 マスのうち最小のもの（(ずれ, 値) を返す）
    bestFrom :: (Int -> Float) -> Int -> (Int, Float)
    bestFrom up x =
      let s0 = (0, up 0)
          s1 = if x > 0 && up (-1) < snd s0 then (-1, up (-1)) else s0
      in if x < w - 1 && up 1 < snd s1 then (1, up 1) else s1
    bottom = U.minIndex (U.slice ((ih - 1) * w) w m)
    back :: Int -> Int -> [Int] -> [Int]
    back y x acc
      | y <= 0 = x : acc
      | otherwise =
          let up dx = m U.! ((y - 1) * w + x + dx)
          in back (y - 1) (x + fst (bestFrom up x)) (x : acc)

-- シームを 1 本取り除く
carve :: World -> IO World
carve wd
  | U.null (wSeam wd) || w <= 20 = pure wd { wAuto = False }
  | otherwise = do
      playTone (200 + 600 / (1 + wCost wd / i2f (ih * 60))) 0.12 0.08 Sine
      pure (refresh (findSeam wd { wPix = removeSeam (wPix wd), wMask = removeSeam (wMask wd)
                                 , wWidth = w - 1, wRemoved = wRemoved wd + 1 }))
  where
    w = wWidth wd
    removeSeam = U.ifilter (\i _ -> let (y, x) = i `quotRem` w in x /= wSeam wd U.! y)

step :: Env -> Float -> World -> IO World
step env _ w
  | wAuto w && envFrame env `mod` carveEvery == 0 = carve w
  | otherwise = pure w

-- 絵の位置と倍率
scaleOf :: Env -> Float
scaleOf env = min ((envW env - 64) / i2f iw) ((envH env - 200) / i2f ih)

originX, originY :: Float
originX = 32
originY = 100

paintAt :: Env -> World -> World
paintAt env wd
  | cx < 0 || cy < 0 || cx >= w || cy >= ih = wd
  | otherwise = refresh $ case wBrush wd of
      1 -> wd { wHue = hue, wMask = wMask wd U.// [(i, 1) | i <- spots] }
      2 -> wd { wHue = hue, wMask = wMask wd U.// [(i, -1) | i <- spots] }
      _ -> wd { wHue = hue, wPix = wPix wd U.// [(i, paint) | i <- spots] }
  where
    w = wWidth wd
    sc = scaleOf env
    (mx, my) = mouseP env
    cx = floor ((mx - originX) / sc) :: Int
    cy = floor ((my - originY) / sc) :: Int
    hue = wrapF 360 (wHue wd + 2)
    spots =
      [ y * w + x
      | y <- [max 0 (cy - brushSize) .. min ih (cy + brushSize) - 1]
      , x <- [max 0 (cx - brushSize) .. min w (cx + brushSize) - 1]
      , dist2 (i2f x, i2f y) (i2f cx, i2f cy) <= i2f brushSize ]
    (cr, cg, cb, _) = rgbaOfColor (hsb hue 80 100)
    paint = packRGB (cr * 255) (cg * 255) (cb * 255)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (paintAt env w)
  EventMotion _ | envDown env -> pure (paintAt env w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure (findSeam w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wAuto = not (wAuto w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> carve w
  EventKey (Char c) Down _ _
    | c `elem` "123" -> pure w { wBrush = fromEnum c - fromEnum '0' }
    | c `elem` "eE" -> pure (refresh w { wEnergy = not (wEnergy w) })
    | c `elem` "rR" -> do
        s <- newScene
        pure (refresh s { wBrush = wBrush w, wEnergy = wEnergy w })
  _ -> pure w

-- 絵をピクセル画像に。マスクは色を重ねて見せる
refresh :: World -> World
refresh wd = wd { wImg = pixelImage w ih 1 pixel }
  where
    w = wWidth wd
    q :: Float -> Int
    q = max 0 . min 255 . round
    pixel x y =
      let i = y * w + x
          c = wPix wd U.! i
          mk = wMask wd U.! i
          rr = redOf c
          gg = greenOf c
          bb = blueOf c
          (r1, g1, b1)
            | wEnergy wd = let e = q (energyAt wd x y * 0.8) in (e, e, e)
            | otherwise = (rr, gg, bb)
          blend a t = q (lerpF (i2f a) t alpha)
          alpha = if mk > 0 then 90 / 255 else 110 / 255
      in if mk > 0 then (fromIntegral (blend r1 163), fromIntegral (blend g1 230), fromIntegral (blend b1 53))
         else if mk < 0 then (fromIntegral (blend r1 255), fromIntegral (blend g1 107), fromIntegral (blend b1 139))
         else (fromIntegral r1, fromIntegral g1, fromIntegral b1)

-- 絵を決まった位置に、決まった倍率で置く
imagePic :: Env -> World -> Picture
imagePic env wd =
  Translate (originX + i2f (wWidth wd) * sc / 2) (negate (originY + i2f ih * sc / 2)) (Scale sc sc (wImg wd))
  where sc = scaleOf env

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env
  ( [ txt 26 cFg 32 24 "Seam carving: find a quiet seam with DP and remove it"
    , txt 15 cMuted 32 60 "Space: auto   Right: 1 seam   Drag: brush (1 protect, 2 erase, 3 paint)   E: energy   R: new picture"
    , outline cGrid 1 (originX - 1) (originY - 1) (i2f iw * sc + 2) (i2f ih * sc + 2)
    , imagePic env w ]
  -- 次に削るシーム
  ++ [ pathL cRasp (max 2 sc) [(originX + (i2f x + 0.5) * sc, originY + (i2f y + 0.5) * sc) | (y, x) <- zip [0 :: Int ..] (U.toList (wSeam w))]
     | not (U.null (wSeam w)) ]
  ++ [ txt 16 cFg 32 (bottom + 16) ("Width " ++ show (wWidth w) ++ " / " ++ show iw ++ "   Seams removed " ++ show (wRemoved w) ++ "   Brush: " ++ brushName)
     , txt 14 cMuted 32 (bottom + 42) "Areas painted with Erase attract the seams, so they vanish from the picture (object removal)" ] )
  where
    sc = scaleOf env
    bottom = originY + i2f ih * sc
    brushName = case wBrush w of
      1 -> "protect (green: energy +)"
      2 -> "erase (red: energy -)"
      _ -> "paint"

-- ---------- やってみよう ----------
-- 1. sceneColor を書きかえて、自分の絵を縮める
-- 2. energyAt を「横の変化だけ」にすると、シームの通り方はどう変わる？
-- 3. 太陽を「守る」で塗ってから削る。「消す」で塗ると？

-- ---------- 道具：色と、左上が原点の座標で描く関数（p5.js の座標のまま書ける） ----------

cBg, cPanel, cFg, cMuted, cRasp, cCyan, cAmber, cLime, cGrid :: Color
cBg    = rgb255 0x16 0x11 0x1c
cPanel = rgb255 0x23 0x1a 0x2b
cFg    = rgb255 0xf3 0xea 0xf0
cMuted = rgb255 0x9a 0x8a 0xa3
cRasp  = rgb255 0xff 0x6b 0x8b
cCyan  = rgb255 0x5e 0xe6 0xe0
cAmber = rgb255 0xff 0xc8 0x57
cLime  = rgb255 0xa3 0xe6 0x35
cGrid  = rgb255 0x3a 0x2f 0x44

-- | 画面全体を「左上が原点」の座標に移す（中の図形は y を下向きで渡す）
screen :: Env -> [Picture] -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Pictures

-- | マウスの位置を左上原点の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- | 角を落とした四角の頂点（p5.js の rect の角丸の代わり）
cornerPts :: Float -> Float -> Float -> Float -> [Point]
cornerPts x y w h =
  let r = min 5 (min (abs w) (abs h) / 3)
  in [ (x + r, -y), (x + w - r, -y), (x + w, -y - r), (x + w, -y - h + r)
     , (x + w - r, -y - h), (x + r, -y - h), (x, -y - h + r), (x, -y - r) ]

-- | 四角の枠線（太さ t）
outline :: Color -> Float -> Float -> Float -> Float -> Float -> Picture
outline c t x y w h = case cornerPts x y w h of
  ps@(p : _) | t <= 1.2  -> Color c (Line (ps ++ [p]))
             | otherwise -> Color c (thickPath t (ps ++ [p]))
  [] -> Blank

-- | 折れ線（太さ t）
pathL :: Color -> Float -> [Point] -> Picture
pathL c t ps = Color c (thickPath t [(x, -y) | (x, y) <- ps])

-- | 文字：(x, y) を左上にして書く
txt :: Float -> Color -> Float -> Float -> String -> Picture
txt size c x y = label x (-y - size) size c

-- | 整数を小数に
i2f :: Int -> Float
i2f = fromIntegral
