{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 13 多様体を聞く（トーラスの上の直線）
-- 正方形の上下・左右を貼るとトーラス。正方形の上の傾き s の直線は、トーラスに巻きつく「平らなトーラスの測地線」になる
--   s が分数 p/q なら q 周・p 周で閉じる（メロディがくり返す）。s が無理数なら永遠に閉じず、表面を埋めつくす
-- クリック = 音をスタート / 1〜4 = 分数の傾き（1, 1/2, 2/3, 3/5）/ 5〜8 = 無理数（√2, 黄金比, π, e）/ ←→ = 傾きを少し変える / Q = 音階にそろえる / ↑↓ = 速さ / M = 消音
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

baseF :: Float
baseF = 220                -- ★ いちばん低い音（Hz）

noteEvery :: Int
noteEvery = 15             -- ★ 音を鳴らす間隔（フレーム）。音は短い音のくり返しで鳴らす

-- 正方形の軌跡をためる画像（240 × 240 を 1.5 倍で 360 px に）
sqN :: Int
sqN = 240

data World = World
  { wSlope :: !Double             -- 傾き s
  , wFrac :: !(Maybe (Int, Int))  -- 分数なら (p, q)
  , wName :: String
  , wX, wY, wUx :: !Double        -- 正方形の上の位置と、ほどいた x
  , wSpeed :: !Double             -- ★ 進む速さ
  , wQuant, wMuted, wAudio :: !Bool
  , wTrail :: [(Float, Float)]    -- 新しい順に最大 3000 点
  , wSq :: !(U.Vector Word8)      -- 正方形の軌跡（通った画素 = 1）
  , wVisited :: !(U.Vector Word8) -- 40 × 40 のマス
  , wYaw :: !Float
  }

main :: IO ()
main = runSketch (sketch "28-13 Torus Geodesic Sound" (resetPath (World (2 / 3) (Just (2, 3)) "2/3" 0 0 0 0.004 True False False [] U.empty U.empty 0.3)))
  { skBack  = rgb255 14 16 26
  , skStep  = step
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

tiltT :: Float
tiltT = 0.9

resetPath :: World -> World
resetPath w = w { wX = 0.1, wY = 0.1, wUx = 0, wTrail = []
                , wSq = U.replicate (sqN * sqN) 0, wVisited = U.replicate 1600 0 }

setSlope :: Double -> Maybe (Int, Int) -> String -> World -> World
setSlope s f nm w = resetPath w { wSlope = s, wFrac = f, wName = nm }

-- トーラスの点（正方形の (x, y) を角度 2πx, 2πy に）
torusPt :: Float -> Float -> V3
torusPt a b =
  let u = tau * a
      v = tau * b
  in V3 ((1 + 0.42 * cos v) * cos u) ((1 + 0.42 * cos v) * sin u) (0.42 * sin v)

proj :: Float -> V3 -> Point
proj yaw (V3 x y z) =
  let c = cos yaw
      s = sin yaw
      x1 = c * x + s * y
      y1 = -s * x + c * y
  in (x1, y1 * sin tiltT + z * cos tiltT)

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  -- ★ 傾き s の直線に沿って進む（はみ出したら反対側から出てくる = 貼り合わせ）
  let s = wSlope w0
      dx = wSpeed w0 / sqrt (1 + s * s) / 3
      sub (!x, !y, !ux, segs, tr, vis) _ =
        let x1 = x + dx
            y1 = y + dx * s
            wrappedX = x1 >= 1
            wrappedY = y1 >= 1 || y1 < 0
            x2 = if wrappedX then x1 - 1 else x1
            y2 | y1 >= 1 = y1 - 1
               | y1 < 0 = y1 + 1
               | otherwise = y1
            segs' = if wrappedX || wrappedY then segs else (x, y, x2, y2) : segs
            cellV = min 39 (floor (y2 * 40)) * 40 + min 39 (floor (x2 * 40)) :: Int
        in (x2, y2, ux + dx, segs', (realToFrac x2, realToFrac y2) : tr, cellV : vis)
      (nx, ny, nu, segs, tr1, vis1) = foldl sub (wX w0, wY w0, wUx w0, [], [], []) [1 :: Int, 2, 3]
      sq = U.modify (\m -> forM_ segs $ \(ax, ay, bx, by) -> do
             -- 線分を画素に打つ
             let n = sqrt ((bx - ax) ^ (2 :: Int) + (by - ay) ^ (2 :: Int)) * fromIntegral sqN
                 k = max 1 (ceiling n) :: Int
             forM_ [0 .. k] $ \i -> do
               let t = fromIntegral i / fromIntegral k
                   x = ax + (bx - ax) * t
                   y = ay + (by - ay) * t
                   cx = max 0 (min (sqN - 1) (floor (x * fromIntegral sqN)))
                   cy = max 0 (min (sqN - 1) (floor ((1 - y) * fromIntegral sqN)))
               MU.unsafeWrite m (cy * sqN + cx) 1) (wSq w0)
      vis = U.modify (\m -> forM_ vis1 (\c -> MU.unsafeWrite m c 1)) (wVisited w0)
      w = w0 { wX = nx, wY = ny, wUx = nu, wTrail = forceList (take 3000 (tr1 ++ wTrail w0))
             , wSq = sq, wVisited = vis, wYaw = wYaw w0 + 0.003 }
  -- 音：x → 1 つ目の音、y → 2 つ目の音（短い音のくり返し）
  when (wAudio w && not (wMuted w) && envFrame env `mod` noteEvery == 0) $ do
    let dur = fromIntegral noteEvery / 60 * 1.1
    playTone (toFreq w nx) 0.12 dur Sine
    playTone (toFreq w ny * 1.5) 0.1 dur Triangle
  pure w

forceList :: [(Float, Float)] -> [(Float, Float)]
forceList xs = foldr seq xs xs

-- 0〜1 → 音の高さ（★ 2 オクターブ。Q なら五音音階にそろえる）
toFreq :: World -> Double -> Float
toFreq w t =
  let semis = realToFrac t * 24 :: Float
      scale5 = [0, 2, 4, 7, 9] :: [Float]
      o = fromIntegral (floor (semis / 12) :: Int) :: Float
      r = semis - o * 12
      best = maximum (0 : filter (<= r) scale5)
      s = if wQuant w then o * 12 + best else semis
  in baseF * 2 ** (s / 12)

event :: Event -> World -> World
event ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w { wAudio = True }
  EventKey (Char c) Down _ _ -> case toLower c of
    '1' -> setSlope 1 (Just (1, 1)) "1" w
    '2' -> setSlope (1 / 2) (Just (1, 2)) "1/2" w
    '3' -> setSlope (2 / 3) (Just (2, 3)) "2/3" w
    '4' -> setSlope (3 / 5) (Just (3, 5)) "3/5" w
    '5' -> setSlope (sqrt 2) Nothing "sqrt 2" w
    '6' -> setSlope ((1 + sqrt 5) / 2) Nothing "golden ratio (1+sqrt 5)/2" w
    '7' -> setSlope pi Nothing "pi" w
    '8' -> setSlope (exp 1) Nothing "e" w
    'q' -> w { wQuant = not (wQuant w) }
    'm' -> w { wMuted = not (wMuted w) }
    _ -> w
  EventKey (SpecialKey KeyLeft) Down _ _ -> setSlope (max 0.01 (wSlope w - 0.01)) Nothing "hand-tuned" w
  EventKey (SpecialKey KeyRight) Down _ _ -> setSlope (min 5 (wSlope w + 0.01)) Nothing "hand-tuned" w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wSpeed = min 0.03 (wSpeed w * 1.4) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wSpeed = max 0.0005 (wSpeed w / 1.4) }
  _ -> w

-- 軌跡を 100 点ずつの塊に分ける（新しいほど明るく）。正方形の端をまたぐ所で線を切る
trailPics :: (Float -> Float -> Point) -> [(Float, Float)] -> [Picture]
trailPics toS trail =
  let pts = reverse trail
      n = length pts
      chunks k xs = case splitAt k xs of
        ([], _) -> []
        (a, b) -> a : chunks k b
      gold a = withA (a / 255) (rgb255 255 210 90)
      strip (i, ps) =
        let c0 = i * 100 + 1
            al = 40 + 215 * fromIntegral c0 / fromIntegral (max 1 n)
        in Color (gold al) (Pictures (zipWith seg ps (drop 1 ps)))
      seg (ax, ay) (bx, by)
        | abs (ax - bx) > 0.5 || abs (ay - by) > 0.5 = Blank
        | otherwise = thickLine 2 (toS ax ay) (toS bx by)
      -- 3000 点を 2 点おきにして描く（太い線の数を減らす）
      every2 (a : _ : r) = a : every2 r
      every2 r = r
  in map strip (zip [0 :: Int ..] (chunks 50 (every2 pts)))

draw :: Env -> World -> Picture
draw env w =
  let ox = 60
      oy = 140
      size = 360 :: Float
      hg = envH env
      -- 左：正方形（貼り合わせの印つき）
      sqCenter = px env (ox + size / 2) (oy + size / 2)
      sqData = wSq w
      gold = rgb255 255 210 90
      sqImg = at sqCenter (pixelImage sqN sqN (size / fromIntegral sqN) (\x y ->
                if U.unsafeIndex sqData (y * sqN + x) /= 0 then (255, 210, 90) else (20, 24, 40)))
      vLine c x = Color c (thickLine 5 (px env x oy) (px env x (oy + size)))
      hLine c y = Color c (thickLine 5 (px env ox y) (px env (ox + size) y))
      redC = rgb255 255 110 110
      blueC = rgb255 110 180 255
      xF = realToFrac (wX w) :: Float
      yF = realToFrac (wY w) :: Float
      -- 右：トーラス（線の軌跡）
      cx = envW env * 0.68
      cy = hg * 0.48
      k = min (envW env * 0.2) (hg * 0.28)
      toS a b = let (p, q) = proj (wYaw w) (torusPt a b) in px env (cx + p * k) (cy - q * k)
      skeleton = [ Color (rgba255 150 170 230 40) (Line [toS (fromIntegral i / 24) (fromIntegral j / 40) | j <- [0 .. 40 :: Int]])
                 | i <- [0 .. 23 :: Int] ]
      f1 = toFreq w (wX w)
      f2' = toFreq w (wY w) * 1.5
      cover = fromIntegral (U.sum (U.map fromIntegral (wVisited w) :: U.Vector Int)) / 1600 :: Float
      msg = case wFrac w of
        Just (p, q)
          | wUx w >= fromIntegral q -> "Closed! " ++ show q ++ " laps across, " ++ show p ++ " laps up, back to the start (periodic melody)"
          | otherwise -> show q ++ " laps across will close it... now " ++ nf 2 (wUx w) ++ " laps"
        Nothing -> "Irrational: never closes. Visited " ++ nf 1 (cover * 100) ++ "% of the surface cells (slowly toward 100%)"
      onOff b = if b then "ON" else "off"
  in Pictures $
     [ sqImg
     , vLine redC ox, vLine redC (ox + size), hLine blueC oy, hLine blueC (oy + size)
     , at (px env (ox + xF * size) (oy + size - yF * size)) (Color white (disc 6))
     , txt env ox (oy + size + 30) 14 (gray 0.78) "Glue red to red and blue to blue -> torus"
     ] ++ skeleton ++ trailPics toS (wTrail w) ++
     [ at (toS xF yF) (Color white (disc 6))
     , txt env 20 34 16 white ((if wAudio w then "Sound on" else "Click to start sound") ++ "   1-4: fractions  5-8: irrationals   L/R: nudge   Q: scale " ++ onOff (wQuant w) ++ "   U/D: speed   M: mute")
     , txt env 20 60 14 (gray 0.67) "x -> pitch of sound 1, y -> pitch of sound 2. A closed line repeats its melody; an open line never repeats."
     , txt env ox (hg - 80) 19 gold ("Slope s = " ++ wName w ++ " = " ++ nf 5 (wSlope w))
     , txt env ox (hg - 50) 15 (gray 0.86) msg
     , txt env ox (hg - 24) 14 (gray 0.59) ("Sound: " ++ nf 0 f1 ++ " Hz / " ++ nf 0 f2' ++ " Hz")
     ]

-- ---------- やってみよう ----------
-- 1. 2/3 と 3/5 でメロディの長さ（周期）を比べよう。q 周で閉じるので、q が大きいほど長いメロディ
-- 2. √2 にして 1 分待つ。正方形の上の線はどこまで埋まる？（無理数の直線はトーラスの上で稠密）
-- 3. ←→ で 2/3 = 0.66667 から少しだけずらすと、ほとんど閉じそうで閉じない。耳で「ずれ」を聞こう
-- 4. 音色（playTone の Sine, Triangle）を Square などに。noteEvery を変えてリズムも変えよう

---------------------------------------------------------------- 道具
-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
px :: Env -> Float -> Float -> Point
px env x y = (x - envW env / 2, envH env / 2 - y)

-- 文字（p5 の text と同じく左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (a, b) = px env x y in label a b s c str

-- 左上が (x, y) の塗りつぶし長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (a, b) = px env (x + w / 2) (y + h / 2) in Color c (rect a b w h)

-- p5 の nf：小数点以下 d 桁の文字列
nf :: RealFloat a => Int -> a -> String
nf d x | isNaN x = "--"
       | isInfinite x = if x > 0 then "inf" else "-inf"
       | otherwise = showFFloat (Just d) x ""

degF :: Float -> Float
degF x = x * 180 / pi

-- JavaScript の % と同じ（符号は x に合わせる）
jsMod :: Float -> Float -> Float
jsMod x m = x - m * fromIntegral (truncate (x / m) :: Int)
