-- ============================================
-- 04 2 次元の衝突（ビリヤード）
-- たくさんの円盤が弾性衝突をくり返す。最初は全員同じ速さでも、速さの分布はやがてマクスウェル＝ボルツマン分布になる
--   運動量の合計（黄色い矢印）とエネルギーの合計は、ぶつかっても変わらない
-- スペース = やり直し（全員同じ速さ）/ ↑↓ = 円盤の数 / P = 周期境界 ↔ 壁 / M = 重い円盤を混ぜる / クリック = 速い円盤を追加
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Text.Printf (printf)

radius0, speed0 :: Float
radius0 = 7     -- ★ 円盤の半径 [px]
speed0 = 120    -- ★ 最初の速さ [px/s]

bins, subs :: Int
bins = 30
subs = 2

-- 全部の円盤（高速に計算するため、種類ごとに 1 本の配列にする）
data World = World
  { wN        :: !Int                 -- ★ 円盤の数
  , wPeriodic :: !Bool                -- 周期境界（右から出たら左から入る）なら運動量も保存
  , wHeavy    :: !Bool
  , wX, wY, wVx, wVy, wM, wR :: !(U.Vector Float)
  , wHist     :: !(U.Vector Float)    -- 速さの分布（少し時間平均）
  }

main :: IO ()
main = runSketch (sketch "24-04 Billiards in 2D" world0)
  { skBack  = hsb 200 30 10
  , skStart = \_ -> restart world0
  , skStep  = \_ _ w -> stepWorld w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World 260 True False e e e e e e (U.replicate bins 0)
  where e = U.empty

arenaX, arenaY, arenaW, arenaH :: Float
arenaX = width * 0.03
arenaY = 100
arenaW = width * 0.44
arenaH = height - 150

-- 円盤を重ならないように置きなおす
restart :: World -> IO World
restart w = do
  let n = wN w
      place :: Int -> Int -> [(Float, Float, Float, Float, Float, Float)] -> IO [(Float, Float, Float, Float, Float, Float)]
      place !count !tries acc
        | count >= n || tries >= 20000 = pure (reverse acc)
        | otherwise = do
            x <- randomRIO (arenaX + radius0, arenaX + arenaW - radius0)
            y <- randomRIO (arenaY + radius0, arenaY + arenaH - radius0)
            let lim = (2 * radius0 + 1) * (2 * radius0 + 1)
                hit = any (\(a, b, _, _, _, _) -> (a - x) * (a - x) + (b - y) * (b - y) < lim) acc
            if hit then place count (tries + 1) acc else do
              a <- randomRIO (0, tau)
              let m = if wHeavy w && count `mod` 5 == 0 then 4 else 1
                  r = if m > 1 then radius0 * 1.6 else radius0
              place (count + 1) (tries + 1) ((x, y, speed0 * cos a, speed0 * sin a, m, r) : acc)
  ds <- place 0 0 []
  let col f = U.fromList (map f ds)
  pure w { wX = col (\(a, _, _, _, _, _) -> a), wY = col (\(_, a, _, _, _, _) -> a)
         , wVx = col (\(_, _, a, _, _, _) -> a), wVy = col (\(_, _, _, a, _, _) -> a)
         , wM = col (\(_, _, _, _, a, _) -> a), wR = col (\(_, _, _, _, _, a) -> a)
         , wHist = U.replicate bins 0 }

stepWorld :: World -> IO World
stepWorld w = do
  let n = U.length (wX w)
      h = 1 / 60 / fromIntegral subs :: Float
  xs <- U.thaw (wX w)
  ys <- U.thaw (wY w)
  vxs <- U.thaw (wVx w)
  vys <- U.thaw (wVy w)
  forM_ [1 .. subs] $ \_ -> do
    forM_ [0 .. n - 1] $ \i -> do
      vx <- M.unsafeRead vxs i
      vy <- M.unsafeRead vys i
      x0 <- (+ vx * h) <$> M.unsafeRead xs i
      y0 <- (+ vy * h) <$> M.unsafeRead ys i
      if wPeriodic w
        then do
          let x1 = if x0 < arenaX then x0 + arenaW else x0
              x2 = if x1 >= arenaX + arenaW then x1 - arenaW else x1
              y1 = if y0 < arenaY then y0 + arenaH else y0
              y2 = if y1 >= arenaY + arenaH then y1 - arenaH else y1
          M.unsafeWrite xs i x2
          M.unsafeWrite ys i y2
        else do
          let r = U.unsafeIndex (wR w) i
              wall lo hi p v | p < lo = (lo, abs v)
                             | p > hi = (hi, -abs v)
                             | otherwise = (p, v)
              (x2, vx2) = wall (arenaX + r) (arenaX + arenaW - r) x0 vx
              (y2, vy2) = wall (arenaY + r) (arenaY + arenaH - r) y0 vy
          M.unsafeWrite xs i x2
          M.unsafeWrite ys i y2
          M.unsafeWrite vxs i vx2
          M.unsafeWrite vys i vy2
    collideAll (wM w) (wR w) xs ys vxs vys
  x' <- U.unsafeFreeze xs
  y' <- U.unsafeFreeze ys
  vx' <- U.unsafeFreeze vxs
  vy' <- U.unsafeFreeze vys
  let w1 = w { wX = x', wY = y', wVx = vx', wVy = vy' }
      counts = U.accum (+) (U.replicate bins 0)
        [ (b, 1)
        | i <- [0 .. n - 1], U.unsafeIndex (wM w) i == 1
        , let b = floor (speedOf w1 i / vmaxH * fromIntegral bins) :: Int, b >= 0, b < bins ]
  pure w1 { wHist = U.zipWith (\a c -> a * 0.95 + c * 0.05) (wHist w) counts }

vmaxH :: Float
vmaxH = speed0 * 3

speedOf :: World -> Int -> Float
speedOf w i = let a = U.unsafeIndex (wVx w) i
                  b = U.unsafeIndex (wVy w) i
              in sqrt (a * a + b * b)

-- ★ 2 次元の弾性衝突：中心を結ぶ方向の速度成分だけを交換する（グリッドで近くだけ調べる）
collideAll :: U.Vector Float -> U.Vector Float
           -> M.IOVector Float -> M.IOVector Float -> M.IOVector Float -> M.IOVector Float -> IO ()
collideAll ms rs xs ys vxs vys = do
  let n = U.length ms
      cell = radius0 * 4
      cols = 50 :: Int
      rows = 34 :: Int
      cellOf :: Float -> Float -> (Int, Int)
      cellOf x y = (floor (x / cell) + 1, floor (y / cell) + 1)
      idx gx gy = max 0 (min (cols - 1) gx) * rows + max 0 (min (rows - 1) gy)
  -- 各マスの先頭と「次の円盤」をつなぐリスト
  heads <- M.replicate (cols * rows) (-1 :: Int)
  nexts <- M.replicate n (-1 :: Int)
  forM_ [n - 1, n - 2 .. 0] $ \i -> do
    x <- M.unsafeRead xs i
    y <- M.unsafeRead ys i
    let (gx, gy) = cellOf x y
        k = idx gx gy
    M.unsafeRead heads k >>= M.unsafeWrite nexts i
    M.unsafeWrite heads k i
  forM_ [0 .. n - 1] $ \i -> do
    x0 <- M.unsafeRead xs i
    y0 <- M.unsafeRead ys i
    let (gx, gy) = cellOf x0 y0
    forM_ [-1, 0, 1] $ \ox -> forM_ [-1, 0, 1] $ \oy -> do
      let cgx = gx + ox
          cgy = gy + oy
      when (cgx >= 0 && cgx < cols && cgy >= 0 && cgy < rows) $ do
        let walk j | j < 0 = pure ()
                   | otherwise = do
                       when (j > i) (pairHit i j)
                       M.unsafeRead nexts j >>= walk
        M.unsafeRead heads (idx cgx cgy) >>= walk
  where
    pairHit i j = do
      ax <- M.unsafeRead xs i
      ay <- M.unsafeRead ys i
      bx <- M.unsafeRead xs j
      by <- M.unsafeRead ys j
      let dx = bx - ax
          dy = by - ay
          rr = U.unsafeIndex rs i + U.unsafeIndex rs j
          d2 = dx * dx + dy * dy
      when (d2 < rr * rr && d2 /= 0) $ do
        let dd = sqrt d2
            nx = dx / dd
            ny = dy / dd
            over = (rr - dd) / 2
        M.unsafeWrite xs i (ax - nx * over)
        M.unsafeWrite ys i (ay - ny * over)
        M.unsafeWrite xs j (bx + nx * over)
        M.unsafeWrite ys j (by + ny * over)
        avx <- M.unsafeRead vxs i
        avy <- M.unsafeRead vys i
        bvx <- M.unsafeRead vxs j
        bvy <- M.unsafeRead vys j
        let rel = (bvx - avx) * nx + (bvy - avy) * ny      -- 近づく速さ（負なら近づいている）
        when (rel < 0) $ do
          let ma = U.unsafeIndex ms i
              mb = U.unsafeIndex ms j
              j' = (2 * rel) / (1 / ma + 1 / mb)           -- 弾性衝突の力積
          M.unsafeWrite vxs i (avx + (j' / ma) * nx)
          M.unsafeWrite vys i (avy + (j' / ma) * ny)
          M.unsafeWrite vxs j (bvx - (j' / mb) * nx)
          M.unsafeWrite vys j (bvy - (j' / mb) * ny)

drawAll :: World -> Picture
drawAll w = Pictures
  [ view ( Color (hsb 0 0 6) (boxAt arenaX arenaY arenaW arenaH)
         : [ Color (hsb (remap (clampF 0 (speed0 * 2.5) (speedOf w i)) 0 (speed0 * 2.5) 230 0) 70 100)
               (at (U.unsafeIndex (wX w) i, U.unsafeIndex (wY w) i) (disc (U.unsafeIndex (wR w) i)))
           | i <- [0 .. n - 1] ]
        ++ [ if wPeriodic w
               then Color (hsb 0 0 30) (rectLine (arenaX + arenaW / 2) (arenaY + arenaH / 2) arenaW arenaH)
               else Color (hsb 0 0 80) (thickRect arenaX arenaY arenaW arenaH)
             -- ★ 運動量の合計（矢印、長さは 1 個あたりに直して表示）
           , Color (hsb 55 100 100) (thickLine 5 (cx, cy) tip)
           , Color (hsb 55 100 100) (at tip (disc 6)) ] )
  , drawHistogram w
  , txt 20 36 17 (gray 1) ("Disks " ++ show n ++ " (Up/Down)   " ++ (if wPeriodic w then "Periodic" else "Walls") ++ " (P)   Heavy "
                           ++ (if wHeavy w then "ON" else "OFF") ++ " (M)   Space: restart   Click: fast disk")
  , txt 20 66 14 (hsb 0 0 65) ("Total momentum (" ++ nf 0 px ++ ", " ++ nf 0 py ++ ")   Total energy " ++ nf 1 (ke / 1000)
                               ++ " k   (with walls, momentum changes at each hit; energy does not)")
  ]
  where
    n = U.length (wX w)
    ms = wM w
    px = U.sum (U.zipWith (*) ms (wVx w))
    py = U.sum (U.zipWith (*) ms (wVy w))
    ke = foldl' (\s i -> let v = speedOf w i in s + 0.5 * U.unsafeIndex ms i * v * v) 0 [0 .. n - 1]
    total = max 1 (U.sum ms)
    cx = arenaX + arenaW / 2
    cy = arenaY + arenaH / 2
    tip = (cx + px / total * 3, cy + py / total * 3)

-- 右：速さのヒストグラムと 2 次元のマクスウェル＝ボルツマン分布
drawHistogram :: World -> Picture
drawHistogram w = Pictures
  [ view ( Color (hsb 0 0 14) (boxAt gx gy gw gh)
         : [ Color (hsb (clampF 0 230 (remap (fromIntegral i) 0 (fromIntegral bins / 2.5) 230 0)) 60 90)
               (boxAt (gx + 20 + fromIntegral i * bw) (gy + gh - 30 - hh) (bw - 2) hh)
           | i <- [0 .. bins - 1], let hh = U.unsafeIndex (wHist w) i / peak * (gh - 60) ]
        ++ [ Color (hsb 55 90 100) (thickPath 2.5
               [ (gx + 20 + (v / vmaxH) * fromIntegral bins * bw, gy + gh - 30 - f / peak * (gh - 60))
               | i <- [0 .. 120 :: Int]
               , let v = fromIntegral i / 120 * vmaxH
               , let f = (v / s2) * exp (-v * v / (2 * s2)) * lightN * (vmaxH / fromIntegral bins) ])
           , Color (hsb 0 0 80) (dashPath 1 4 5 [(x0, gy + 30), (x0, gy + gh - 30)]) ] )
  , txt (gx + 8) (gy + 20) 14 (hsb 0 0 70) "Speed of the light disks. Dashed = starting speed."
  , txt (gx + 8) (gy + 38) 14 (hsb 0 0 70) "Yellow = Maxwell-Boltzmann distribution in 2D."
  , txtC (gx + gw / 2) (gy + gh - 8) 14 (hsb 0 0 70) "speed ->"
  ]
  where
    gx = width * 0.52
    gy = height * 0.16
    gw = width * 0.44
    gh = height * 0.6
    bw = (gw - 40) / fromIntegral bins
    peak = max 4 (U.maximum (wHist w) * 1.3)
    light = [i | i <- [0 .. U.length (wM w) - 1], U.unsafeIndex (wM w) i == 1]
    lightN = fromIntegral (length light) :: Float
    -- 理論：f(v) = (v/σ²) exp(−v²/2σ²)、σ² = kT/m（平均の運動エネルギーから）
    mvv = foldl' (\s i -> let v = speedOf w i in s + v * v) 0 light / max 1 lightN
    s2 = max 1e-3 (mvv / 2)
    x0 = gx + 20 + (speed0 / vmaxH) * fromIntegral bins * bw

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> do
    let (mx, my) = toP5 pos
    if mx > arenaX && mx < arenaX + arenaW && my > arenaY && my < arenaY + arenaH
      then do
        a <- randomRIO (0, tau)
        pure (addDisk mx my (speed0 * 4 * cos a) (speed0 * 4 * sin a) w)
      else pure w
  EventKey (SpecialKey KeySpace) Down _ _ -> restart w
  EventKey (SpecialKey KeyUp) Down _ _ -> restart w { wN = min (wN w + 60) 500 }
  EventKey (SpecialKey KeyDown) Down _ _ -> restart w { wN = max (wN w - 60) 20 }
  EventKey (Char c) Down _ _ -> case toLower c of
    'p' -> pure w { wPeriodic = not (wPeriodic w) }
    'm' -> restart w { wHeavy = not (wHeavy w) }
    _ -> pure w
  _ -> pure w

addDisk :: Float -> Float -> Float -> Float -> World -> World
addDisk x y vx vy w =
  let cap v = if U.length v > 600 then U.drop 1 v else v     -- 600 個を超えたら古いものから消す
      push v a = cap (U.snoc v a)
  in w { wX = push (wX w) x, wY = push (wY w) y, wVx = push (wVx w) vx, wVy = push (wVy w) vy
       , wM = push (wM w) 1, wR = push (wR w) radius0 }

-- ---------- やってみよう ----------
-- 1. スペースでやり直して、全員同じ速さ（点線に集中）から黄色い曲線になるまでの時間を見る
-- 2. 円盤を減らす（20 個）と分布になるまで時間がかかる。なぜ？（ぶつかる回数）
-- 3. M で重い円盤を混ぜる。重い円盤は遅く、軽い円盤は速くなる。でも ½mv² の平均は同じ（エネルギー等分配）
-- 4. 1 個だけ速い円盤をクリックで入れると、その速さは全体に広がっていく（熱が伝わる）


-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- マウスの位置（gloss の座標）を p5 と同じ座標（左上が原点、y は下向き）に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は左下の位置（p5 の座標）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y s c str = txt (x - fromIntegral (length str) * s * 0.36) y s c str

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y w h = rect (x + w / 2) (y + h / 2) w h

-- 太さ 3 の枠
thickRect :: Float -> Float -> Float -> Float -> Picture
thickRect x y w h = thickPath 3 [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]

nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

-- 点線：線の長さ on、すきま off
dashPath :: Float -> Float -> Float -> [Point] -> Picture
dashPath w on off = Pictures . go True on
  where
    go draw left (a : b : rest)
      | l <= left = [thickLine w a b | draw] ++ go draw (left - l) (b : rest)
      | otherwise =
          let u = left / l
              m = (lerpF (fst a) (fst b) u, lerpF (snd a) (snd b) u)
          in [thickLine w a m | draw] ++ go (not draw) (if draw then off else on) (m : b : rest)
      where l = dist2 a b
    go _ _ _ = []
