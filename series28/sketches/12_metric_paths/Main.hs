{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 12 リーマン計量で距離を変える
-- 「場所によって 1 歩の重さがちがう」平面：ds = c(x, y) × |dx|（c が大きい所は進みにくい = 遠い）。これがリーマン計量の一例
--   最短の道（測地線）は直線でなく、重い所をよけて曲がる。光が水やガラスで曲がる（屈折）のと同じしくみ
-- ドラッグ = 重い地面を塗る / Shift + ドラッグ = 軽い道を塗る / S・G をドラッグ = 出発・到着 / 1 = レンズ 2 = 双曲（上半平面）3 = 山 / C = 平らに / W = 等距離線 / ↑↓ = 筆の大きさ
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

cs :: Float
cs = 10                    -- ★ 1 マスの大きさ（px）。小さいほど細かいが重い

top0 :: Float
top0 = 90

-- マスの数（ウィンドウ 1280 × 800 のとき）
gw, gh :: Int
gw = floor (1280 / cs)
gh = floor ((800 - top0 - 60) / cs)

idx :: Int -> Int -> Int
idx i j = j * gw + i

-- 16 方向に進める
nb :: [(Int, Int)]
nb = [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1), (1, 2), (2, 1), (-1, 2), (-2, 1), (1, -2), (2, -1), (-1, -2), (-2, -1)]

data Grab = GrabNone | GrabS | GrabG | GrabPaint deriving Eq

data World = World
  { wCost :: !(U.Vector Float)     -- マスごとの重さ c
  , wDist :: !(U.Vector Double)    -- 出発点からの距離
  , wPrev :: !(U.Vector Int)       -- ひとつ前のマス
  , wS, wG :: !(Int, Int)          -- 出発・到着（マスの番号 (i, j)）
  , wBrush :: !Int                 -- ★ 筆の半径（マス）
  , wWaves, wDirty :: !Bool
  , wGrab :: !Grab
  , wLighter :: !Bool              -- Shift を押して塗り始めたか
  , wPath :: [(Int, Int)]
  , wImg :: Picture                -- 重さと等距離線の画像（変わったときだけ作り直す）
  }

main :: IO ()
main = runSketch (sketch "28-12 Metric Paths" initial)
  { skBack  = rgb255 14 16 26
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }
  where
    initial = solve (preset 1 (World (U.replicate (gw * gh) 1) U.empty U.empty (0, 0) (0, 0) 4 True True GrabNone False [] Blank))

preset :: Int -> World -> World
preset k w =
  let fgw = fromIntegral gw :: Float
      fgh = fromIntegral gh :: Float
      costAt n =
        let (j, i) = n `divMod` gw
            fi = fromIntegral i :: Float
            fj = fromIntegral j :: Float
            dx = (fi - fgw / 2) / fgh
            dy = (fj - fgh / 2) / fgh
        in case k of
             1 -> if dx * dx + dy * dy < 0.09 then 2.5 else 1             -- ガラスのレンズ（重い円）
             2 -> min 30 (0.25 / ((fgh - fj) / fgh))                      -- ★ 上半平面 ds = |dx| / y（下ほど重い）
             _ -> 1 + 4 * exp (-((dx * dx + dy * dy) / 0.02))             -- 山
      fl v = floor v :: Int
      (s, g) = if k == 2
        then ((fl (fgw * 0.2), fl (fgh * 0.75)), (fl (fgw * 0.8), fl (fgh * 0.75)))
        else ((fl (fgw * 0.12), fl (fgh * 0.5)), (fl (fgw * 0.88), fl (fgh * 0.5)))
  in w { wCost = U.generate (gw * gh) costAt, wS = s, wG = g, wDirty = True }

-- ★ ダイクストラ法：出発点から全マスへの最短距離（計量 c で測った長さ）
dijkstra :: U.Vector Float -> Int -> (U.Vector Double, U.Vector Int)
dijkstra cost s = runST $ do
  let n = gw * gh
      cap = n * 16 + 1
  dist <- MU.replicate n (1 / 0 :: Double)
  prev <- MU.replicate n (-1 :: Int)
  hk <- MU.new cap
  hv <- MU.new cap
  MU.write dist s 0
  MU.write hk 0 (0 :: Double)
  MU.write hv 0 s
  dijkstraLoop cost dist prev hk hv 1
  d <- U.unsafeFreeze dist
  p <- U.unsafeFreeze prev
  pure (d, p)

neighbors :: U.Vector (Int, Int, Double)
neighbors = U.fromList [ (a, b, sqrt (fromIntegral (a * a + b * b))) | (a, b) <- nb ]

dijkstraLoop :: U.Vector Float -> MU.STVector s Double -> MU.STVector s Int -> MU.STVector s Double -> MU.STVector s Int -> Int -> ST s ()
dijkstraLoop cost dist prev hk hv = go
  where
    go 0 = pure ()
    go sz = do
      (d, u, sz1) <- heapPop hk hv sz
      du <- MU.unsafeRead dist u
      if d > du then go sz1 else do
        let (uj, ui) = u `divMod` gw
            cu = realToFrac (U.unsafeIndex cost u) :: Double
            relax !k !acc
              | k >= U.length neighbors = pure acc
              | otherwise = do
                  let (di, dj, len) = U.unsafeIndex neighbors k
                      vi = ui + di
                      vj = uj + dj
                  if vi < 0 || vj < 0 || vi >= gw || vj >= gh then relax (k + 1) acc else do
                    let v = idx vi vj
                        -- 辺の長さ × 平均の重さ
                        nd = d + len * 0.5 * (cu + realToFrac (U.unsafeIndex cost v))
                    dv <- MU.unsafeRead dist v
                    if nd < dv
                      then do
                        MU.unsafeWrite dist v nd
                        MU.unsafeWrite prev v u
                        acc' <- heapPush hk hv acc nd v
                        relax (k + 1) acc'
                      else relax (k + 1) acc
        relax 0 sz1 >>= go

-- 距離と道を計算し直す
solve :: World -> World
solve w =
  let (d, p) = dijkstra (wCost w) (uncurry idx (wS w))
      walk v | v < 0 = []
             | otherwise = let (j, i) = v `divMod` gw in (i, j) : walk (U.unsafeIndex p v)
      w1 = w { wDist = d, wPrev = p, wPath = walk (uncurry idx (wG w)), wDirty = False }
  in w1 { wImg = makeImage w1 }

step :: Env -> World -> World
step env w0 =
  let w1 = if envDown env && wGrab w0 == GrabPaint then paint env w0 else w0
  in if wDirty w1 && envFrame env `mod` 4 == 0 then solve w1 else w1

-- 重さ → 色、等距離線を重ねた画像
makeImage :: World -> Picture
makeImage w =
  let geo = U.unsafeIndex (wDist w) (uncurry idx (wG w))
      base = if geo == 0 || isNaN geo then 100 else geo
      wave = if wWaves w then max 8 (base / 14) else 0
      cost = wCost w
      dist = wDist w
      px8 :: Float -> Word8
      px8 = truncate . max 0 . min 255
      pixel i j =
        let k = idx i j
            c = U.unsafeIndex cost k
            t = clampF 0 1 (log c / log 30 + 0.35)
            r = 30 + 200 * t
            g = 40 + 120 * t
            b = 70 + 40 * (1 - t)
            d = U.unsafeIndex dist k
            band = wave > 0 && not (isInfinite d) && even (floor (d / wave) :: Int)
        in if band then (px8 (r * 0.75), px8 (g * 0.85), px8 (b * 1.15)) else (px8 r, px8 g, px8 b)
  in pixelImage gw gh cs pixel

-- 筆で塗る
paint :: Env -> World -> World
paint env w =
  let (mx, my) = mouseP5 env
      ci = floor (mx / cs) :: Int
      cj = floor ((my - top0) / cs) :: Int
      b = wBrush w
      lighter = wLighter w || isDown env (SpecialKey KeyShiftL) || isDown env (SpecialKey KeyShiftR)
      cost = U.modify (\m ->
        forM_ [max 0 (cj - b) .. min (gh - 1) (cj + b)] $ \j ->
          forM_ [max 0 (ci - b) .. min (gw - 1) (ci + b)] $ \i -> do
            let d2 = fromIntegral ((i - ci) * (i - ci) + (j - cj) * (j - cj)) :: Float
                wt = exp (-d2 / (fromIntegral (b * b) * 0.5))
                k = idx i j
            c <- MU.unsafeRead m k
            -- ★ 重さの範囲 0.2〜30
            MU.unsafeWrite m k (clampF 0.2 30 (c * (if lighter then 1 - 0.15 * wt else 1 + 0.25 * wt)))) (wCost w)
  in w { wCost = cost, wDirty = True }

mouseP5 :: Env -> Point
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down mods _ ->
    let (mx, my) = mouseP5 env
        gx = fromIntegral (floor (mx / cs) :: Int) :: Float
        gy = fromIntegral (floor ((my - top0) / cs) :: Int) :: Float
        near (i, j) = sqrt ((gx - fromIntegral i) ^ (2 :: Int) + (gy - fromIntegral j) ^ (2 :: Int)) < 2.5
        g | near (wS w) = GrabS
          | near (wG w) = GrabG
          | my > top0 = GrabPaint
          | otherwise = GrabNone
    in w { wGrab = g, wLighter = shift mods == Down }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wGrab = GrabNone }
  EventMotion _ | envDown env ->
    let (mx, my) = mouseP5 env
        p = (max 0 (min (gw - 1) (floor (mx / cs))), max 0 (min (gh - 1) (floor ((my - top0) / cs))))
    in case wGrab w of
         GrabS -> w { wS = p, wDirty = True }
         GrabG -> w { wG = p, wDirty = True }
         _ -> w
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> preset (fromEnum c - fromEnum '0') w
    | toLower c == 'c' -> w { wCost = U.replicate (gw * gh) 1, wDirty = True }
    | toLower c == 'w' -> let w1 = w { wWaves = not (wWaves w) } in w1 { wImg = makeImage w1 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wBrush = min 15 (wBrush w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wBrush = max 1 (wBrush w - 1) }
  _ -> w

draw :: Env -> World -> Picture
draw env w =
  let cell (i, j) = ((fromIntegral i + 0.5) * cs, top0 + (fromIntegral j + 0.5) * cs)
      toS (x, y) = px env x y
      imgPos = toS (fromIntegral gw * cs / 2, top0 + fromIntegral gh * cs / 2)
      (sx, sy) = cell (wS w)
      (gx, gy) = cell (wG w)
      -- 最短路（測地線）
      pathPic = Color (rgb255 255 230 90) (thickPath 4 (map (toS . cell) (wPath w)))
      -- 直線（点線）
      len = dist2 (sx, sy) (gx, gy)
      dashN = floor (len / 14) :: Int
      dashes = [ thickLine 2 (toS (lerpF sx gx (a / len), lerpF sy gy (a / len))) (toS (lerpF sx gx (b / len), lerpF sy gy (b / len)))
               | k <- [0 .. dashN], let a = fromIntegral k * 14, let b = min len (a + 8), b > a ]
      -- 直線の、この計量での長さ
      (si, sj) = wS w
      (gi, gj) = wG w
      fi = fromIntegral :: Int -> Float
      n = 400 :: Int
      hypot = sqrt (fi ((gi - si) * (gi - si) + (gj - sj) * (gj - sj)))
      straightLen = sum
        [ U.unsafeIndex (wCost w) (idx (max 0 (min (gw - 1) i)) (max 0 (min (gh - 1) j))) * hypot / fi n
        | k <- [0 .. n - 1]
        , let t = (fi k + 0.5) / fi n
              i = floor (fi si + fi (gi - si) * t + 0.5) :: Int
              j = floor (fi sj + fi (gj - sj) * t + 0.5) :: Int ]
      geo = realToFrac (U.unsafeIndex (wDist w) (uncurry idx (wG w))) :: Float
      mark (x, y) c nm =
        let (a, b) = toS (x, y)
        in Pictures [at (a, b) (Color c (disc 10)), labelC a b 14 black nm]
      hg = envH env
  in Pictures
     [ at imgPos (wImg w)
     , pathPic
     , Color (withA (160 / 255) white) (Pictures dashes)
     , mark (sx, sy) (rgb255 120 255 160) "S"
     , mark (gx, gy) (rgb255 255 120 160) "G"
     , at (envMouse env) (Color (withA (120 / 255) white) (Circle (fromIntegral (wBrush w) * cs)))
     , txt env 20 28 16 white ("Drag: heavy ground   Shift+drag: light road   Drag S, G   1: lens  2: hyperbolic  3: hill   C: flat")
     , txt env 20 50 16 white ("W: wavefronts " ++ (if wWaves w then "ON" else "off") ++ "   U/D: brush")
     , txt env 20 74 14 (gray 0.67) "Bright = heavy (each step is long). Yellow = shortest path (geodesic), white dashes = straight line. Bands = equal distance"
     , txt env 20 (hg - 24) 16 (rgb255 255 230 90)
         ("Geodesic length = " ++ nf 1 geo ++ "    straight line (same metric) = " ++ nf 1 straightLen ++ "    -> straight is " ++ nf 1 ((straightLen / geo - 1) * 100) ++ "% longer")
     ]

-- ---------- やってみよう ----------
-- 1. 例 1（レンズ）：S と G をレンズの上下にずらすと、道がレンズの中で折れ曲がる（スネルの法則）
-- 2. 例 2（双曲）：同じ高さの 2 点を結ぶ最短の道は半円！ これは 09 の双曲平面のもう 1 つの地図（上半平面）
-- 3. 等距離線（W）の波面と最短の道は、いつも直角に交わる。確かめよう
-- 4. nb を 4 方向（上下左右）だけにすると、平らな所の最短の道はどう見える？（マス目の計量）

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

---------------------------------------------------------------- ヒープの道具（いちばん小さいものを速く取り出す）
heapPush :: MU.STVector s Double -> MU.STVector s Int -> Int -> Double -> Int -> ST s Int
heapPush hk hv sz key val = do
  let up !i
        | i <= 0 = pure i
        | otherwise = do
            let p = (i - 1) `div` 2
            kp <- MU.unsafeRead hk p
            if kp <= key then pure i else do
              vp <- MU.unsafeRead hv p
              MU.unsafeWrite hk i kp
              MU.unsafeWrite hv i vp
              up p
  i <- up sz
  MU.unsafeWrite hk i key
  MU.unsafeWrite hv i val
  pure (sz + 1)

heapPop :: MU.STVector s Double -> MU.STVector s Int -> Int -> ST s (Double, Int, Int)
heapPop hk hv sz = do
  topK <- MU.unsafeRead hk 0
  topV <- MU.unsafeRead hv 0
  let n = sz - 1
  lastK <- MU.unsafeRead hk n
  lastV <- MU.unsafeRead hv n
  let down !i = do
        let l = 2 * i + 1
            r = l + 1
        if l >= n then pure i else do
          kl <- MU.unsafeRead hk l
          kr <- if r < n then MU.unsafeRead hk r else pure (1 / 0)
          let c = if kr < kl then r else l
              kc = min kl kr
          if kc >= lastK then pure i else do
            vc <- MU.unsafeRead hv c
            MU.unsafeWrite hk i kc
            MU.unsafeWrite hv i vc
            down c
  when (n > 0) $ do
    i <- down 0
    MU.unsafeWrite hk i lastK
    MU.unsafeWrite hv i lastV
  pure (topK, topV, n)
