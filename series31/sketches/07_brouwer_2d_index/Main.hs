-- ============================================
-- 07 ブラウワーの不動点定理（2 次元）─ 回転数で不動点を追いつめる
-- 正方形を自分の中へ連続に写す f には不動点がある。ずれのベクトル v(p) = f(p) − p を考える
--   輪の上で v が何回まわるか（指数・回転数）が 0 でなければ、輪の中に不動点がある。外周では v は必ず内向き → 回転数 1
--   06 の二分法の 2 次元版：正方形を 4 つに割り、回転数が 0 でない小正方形だけ残す
-- ドラッグ = 白丸（ゆがみの根元）と矢じり（ゆがみの向き）/ ←→ = 回転 / ↑↓ = 輪の大きさ / スペース = 4 分割して絞る / R = 絞り込みをやり直す / A = 矢印 / N = 新しいゆがみ
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Numeric (showEFloat, showFFloat)

-- 細かく絞り込むので計算は Double で行う
data Handle' = Handle' { hAx, hAy, hVx, hVy :: !Double }
data Cell = Cell { cX, cY, cW :: !Double }

data World = World
  { wTheta   :: !Double      -- ★ 全体の回転
  , wHandles :: ![Handle']
  , wLoopR   :: !Double      -- マウスの輪の半径
  , wArrows  :: !Bool
  , wCells   :: ![Cell]      -- 残っている小正方形
  , wLevel   :: !Int
  , wField   :: !(U.Vector (Word8, Word8, Word8))  -- 背景の画像
  , wDrag    :: !(Maybe (Int, Bool))               -- つかんだ取っ手の番号、矢じりか
  }

shrink, sigma :: Double
shrink = 0.92                -- ★ 全体の縮み
sigma = 0.45                 -- ★ ゆがみの広がり

fieldN :: Int
fieldN = 90                  -- 背景の画像の解像度

main :: IO ()
main = runSketch (sketch "31-07 Brouwer 2D Index" (refresh (World 1.2 [] 0.25 True [] 0 U.empty Nothing)))
  { skBack  = hsb 230 30 11
  , skStart = \_ -> newHandles (World 1.2 [] 0.25 True [] 0 U.empty Nothing)
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

newHandles :: World -> IO World
newHandles w = do
  hs <- replicateM 3 (Handle' <$> randomRIO (-0.7, 0.7) <*> randomRIO (-0.7, 0.7) <*> randomRIO (-0.6, 0.6) <*> randomRIO (-0.6, 0.6))
  pure (refresh w { wHandles = hs })

-- 絞り込みをやり直して、背景も描き直す
refresh :: World -> World
refresh w = w { wCells = [Cell (-1) (-1) 2], wLevel = 0, wField = buildField w }

-- ★ 写像 f：回して少し縮め、白丸のまわりを矢じりの向きに押す。最後に正方形の中に押し戻す（連続）
mapF :: World -> Double -> Double -> (Double, Double)
mapF w x y =
  let c = cos (wTheta w)
      s = sin (wTheta w)
      nx0 = shrink * (c * x - s * y)
      ny0 = shrink * (s * x + c * y)
      push (!nx, !ny) h =
        let dx = x - hAx h
            dy = y - hAy h
            k = exp (negate (dx * dx + dy * dy) / (sigma * sigma))
        in (nx + hVx h * k, ny + hVy h * k)
      (nx1, ny1) = foldl' push (nx0, ny0) (wHandles w)
  in (clampD (-1) 1 nx1, clampD (-1) 1 ny1)

vOf :: World -> Double -> Double -> (Double, Double)
vOf w x y = let (a, b) = mapF w x y in (a - x, b - y)

-- 閉じた折れ線の上で v が何回まわるか（回転数）
windingOf :: World -> [(Double, Double)] -> Int
windingOf _ [] = 0
windingOf w pts@(p0 : _) =
  let angle (x, y) = let (dx, dy) = vOf w x y in atan2 dy dx
      angs = map angle (pts ++ [p0])
      total = sum (zipWith diffAngle angs (drop 1 angs))
  in round (total / (2 * pi))
  where
    diffAngle a b = wrap (b - a)
    wrap d | d > pi = wrap (d - 2 * pi)
           | d < negate pi = wrap (d + 2 * pi)
           | otherwise = d

squarePts :: Cell -> Int -> [(Double, Double)]
squarePts (Cell x y cw) m =
  [ (x + cw * t i, y) | i <- r ] ++ [ (x + cw, y + cw * t i) | i <- r ]
  ++ [ (x + cw * (1 - t i), y + cw) | i <- r ] ++ [ (x, y + cw * (1 - t i)) | i <- r ]
  where
    r = [0 .. m - 1]
    t i = fromIntegral i / fromIntegral m

-- ★ 4 分割して、回転数が 0 でない小正方形だけ残す
subdivide :: World -> World
subdivide w =
  let next = [ q | Cell x y cw <- wCells w, let h = cw / 2
                 , q <- [Cell x y h, Cell (x + h) y h, Cell x (y + h) h, Cell (x + h) (y + h) h]
                 , windingOf w (squarePts q 16) /= 0 ]
  in if not (null next) && length next < 300 then w { wCells = next, wLevel = wLevel w + 1 } else w

-- 背景：v の向きを色相、大きさを明るさで（小さい画像に描いて拡大）
buildField :: World -> U.Vector (Word8, Word8, Word8)
buildField w = U.generate (fieldN * fieldN) $ \k ->
  let (j, i) = k `divMod` fieldN
      n = fromIntegral fieldN :: Double
      x = -1 + 2 * (fromIntegral i + 0.5) / n
      y = 1 - 2 * (fromIntegral j + 0.5) / n
      (dx, dy) = vOf w x y
      hue = realToFrac (atan2 dy dx / (2 * pi) * 360 + 360)
      m = sqrt (dx * dx + dy * dy)
  in hsbToRgb hue 60 (15 + 70 * realToFrac (min 1 (m * 2.5)))

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | isDown env (SpecialKey KeyLeft) = pure (refresh w { wTheta = wTheta w - 0.02 })
  | isDown env (SpecialKey KeyRight) = pure (refresh w { wTheta = wTheta w + 0.02 })
  | otherwise = pure w

-- レイアウト
ox, oy :: Float
ox = 40
oy = 105

sizeOf :: Env -> Float
sizeOf env = min (envH env - 150) (envW env * 0.55)

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $
  [ Translate (ox + s / 2) (oy + s / 2) (Scale (s / fromIntegral fieldN) (negate s / fromIntegral fieldN)
      (pixelImage fieldN fieldN 1 (\x y -> wField w U.! (y * fieldN + x)))) ]
  ++ arrows
  ++ [ Color white (thickPath 2 (cellPath c)) | c <- wCells w ]
  ++ concat [ [ ln 2 white (toS (hAx h, hAy h)) (toS (hAx h + hVx h, hAy h + hVy h))
              , spot white (toS (hAx h, hAy h)) 14
              , spot (hsb 45 90 100) (toS (hAx h + hVx h, hAy h + hVy h)) 11 ] | h <- wHandles w ]
  ++ loopPart
  ++ [ txt 20 34 15 white ("theta = " ++ nf 2 (wTheta w) ++ " (Left/Right)  loop radius " ++ nf 2 (wLoopR w) ++ " (Up/Down)  Space: split in 4 (" ++ show (wLevel w) ++ " times, " ++ show (length (wCells w)) ++ " left)  R: redo  A: arrows  N: new warp")
     , txt 20 60 14 (hsb 0 0 60) ("Color = direction of the shift v = f(p) - p (darker = smaller). The dark point where all colors meet is a fixed point.")
     , txt 20 80 14 (hsb 0 0 60) ("Winding number around the outer edge = " ++ show outer ++ " (v points inward on the edge).")
     ]
  ++ best
  ++ [ txt tx (hh - 60) 14 (hsb 0 0 60) "Sketch 06 kept the half where the sign changes. In 2D, keep the"
     , txt tx (hh - 40) 14 (hsb 0 0 60) "quarters whose winding number is not 0. A loop with a nonzero"
     , txt tx (hh - 20) 14 (hsb 0 0 60) "winding number always contains a fixed point."
     ]
  where
    s = sizeOf env
    hh = envH env
    tx = ox + s + 40
    toS :: (Double, Double) -> Point
    toS (x, y) = (ox + realToFrac ((x + 1) / 2) * s, oy + realToFrac ((1 - y) / 2) * s)
    (mx0, my0) = mouseP env
    mx = realToFrac ((mx0 - ox) / s * 2 - 1) :: Double
    my = realToFrac (1 - (my0 - oy) / s * 2) :: Double
    noDrag = case wDrag w of { Nothing -> True; _ -> False }
    inside = abs mx <= 1.2 && abs my <= 1.2 && noDrag
    loopPts = [ (mx + wLoopR w * cos t, my + wLoopR w * sin t) | i <- [0 .. 89 :: Int], let t = fromIntegral i / 90 * 2 * pi ]
    -- 矢印：p から f(p) へ
    arrows
      | wArrows w = concat
          [ [ Color (hsba 0 0 100 0.55) (Line [toS (x, y), toS (ex, ey)]), spot (hsba 0 0 100 0.7) (toS (ex, ey)) 3 ]
          | i <- [0 .. 17 :: Int], j <- [0 .. 17 :: Int]
          , let x = -1 + (fromIntegral i + 0.5) / 9, let y = -1 + (fromIntegral j + 0.5) / 9
          , let (qx, qy) = mapF w x y, let ex = lerpD x qx 0.5, let ey = lerpD y qy 0.5 ]
      | otherwise = []
    cellPath c = let x0 = ox + realToFrac ((cX c + 1) / 2) * s
                     y0 = oy + realToFrac ((1 - (cY c + cW c)) / 2) * s
                     l = realToFrac (cW c / 2) * s
                 in [(x0, y0), (x0 + l, y0), (x0 + l, y0 + l), (x0, y0 + l), (x0, y0)]
    outer = windingOf w (squarePts (Cell (-1) (-1) 2) 60)
    -- マウスの輪と回転数
    loopPart
      | inside =
          let idx = windingOf w loopPts
              (lx, ly) = toS (mx, my)
              d = realToFrac (wLoopR w) * s
          in [ hoop 3 (if idx == 0 then white else hsb 45 90 100) (lx, ly) d
             , txt (lx + d / 2 + 8) ly 16 white ("winding " ++ show idx)
             ] ++ drawCompass env w s loopPts idx
      | otherwise = []
    best = case wCells w of
      (c : _) | wLevel w > 0 ->
        [ txt tx (hh - 88) 15 white ("Fixed point near (" ++ nf 5 (cX c + cW c / 2) ++ ", " ++ nf 5 (cY c + cW c / 2) ++ "), width " ++ showEFloat (Just 1) (cW c) "") ]
      _ -> []

-- 右：輪を 1 周するあいだの v の向き（羅針盤）
drawCompass :: Env -> World -> Float -> [(Double, Double)] -> Int -> [Picture]
drawCompass env w s pts idx
  | cxp + r > envW env = []
  | otherwise =
      [ hoop 1 (hsb 0 0 40) (cxp, cyp) (r * 2) ]
      ++ [ spot (hsb (fromIntegral i / n * 300) 70 100) (cxp + rr * cos a, cyp - rr * sin a) 6
         | (i, (x, y)) <- zip [0 :: Int ..] pts
         , let (dx, dy) = vOf w x y
         , let a = realToFrac (atan2 dy dx) :: Float
         , let rr = r * (0.55 + 0.4 * fromIntegral i / n) ]
      ++ [ txt (cxp - r) (cyp - r - 20) 15 white "Direction of v along one trip around the loop"
         , txt (cxp - 60) (cyp + r + 30) 15 white ("-> turned " ++ show idx ++ " times")
         , txt (cxp - r) (cyp + r + 54) 14 (hsb 0 0 60) "(color = position on the loop; spirals outward)"
         ]
  where
    cxp = ox + s + 40 + 150
    cyp = oy + 170
    r = 120
    n = fromIntegral (length pts) :: Float

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let hit acc (i, h)
          | dist2 m (toS (hAx h + hVx h, hAy h + hVy h)) < 12 = Just (i, True)
          | dist2 m (toS (hAx h, hAy h)) < 12 = Just (i, False)
          | otherwise = acc
    in pure w { wDrag = foldl' hit Nothing (zip [0 ..] (wHandles w)) }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = Nothing }
  EventMotion _ -> case wDrag w of
    Nothing -> pure w
    Just (i, tip) ->
      let (mx, my) = m
          s = sizeOf env
          x = clampD (-1) 1 (realToFrac ((mx - ox) / s * 2 - 1))
          y = clampD (-1) 1 (realToFrac (1 - (my - oy) / s * 2))
          upd h | tip = h { hVx = x - hAx h, hVy = y - hAy h }
                | otherwise = h { hAx = x, hAy = y }
          hs = [ if k == i then upd h else h | (k, h) <- zip [0 ..] (wHandles w) ]
      in pure (refresh w { wHandles = hs })
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (subdivide w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wLoopR = min 1.2 (wLoopR w + 0.03) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wLoopR = max 0.03 (wLoopR w - 0.03) }
  EventKey (Char c) Down _ _
    | toLower c == 'r' -> pure (refresh w)
    | toLower c == 'a' -> pure w { wArrows = not (wArrows w) }
    | toLower c == 'n' -> newHandles w
  _ -> pure w
  where
    m = mouseP env
    toS :: (Double, Double) -> Point
    toS (x, y) = let s = sizeOf env in (ox + realToFrac ((x + 1) / 2) * s, oy + realToFrac ((1 - y) / 2) * s)

-- ---------- やってみよう ----------
-- 1. 輪を不動点のまわりに置くと回転数 1（または −1）、何もない所では 0。輪をゆっくり不動点に近づけて、数が変わる瞬間を見よう
-- 2. スペースを 10 回押すと幅は 2/1024。小正方形が 2 か所以上に分かれたら、不動点がいくつある？
-- 3. 回転数 −1 の不動点（鞍点のような向き）を作れるか。取っ手を動かして探そう
-- 4. mapF の clampD を外すと外周の回転数は 1 のままか？ 正方形からはみ出す写像では定理が使えない理由を考えよう

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

clampD :: Double -> Double -> Double -> Double
clampD lo hi = max lo . min hi

lerpD :: Double -> Double -> Double -> Double
lerpD a b t = a + (b - a) * t

hsbToRgb :: Float -> Float -> Float -> (Word8, Word8, Word8)
hsbToRgb h s b =
  let (r, g, bl, _) = rgbaOfColor (hsb h s b)
      q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))
  in (q r, q g, q bl)
