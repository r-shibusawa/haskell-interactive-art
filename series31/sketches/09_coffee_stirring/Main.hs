-- ============================================
-- 09 コーヒーをかき混ぜる（と毛玉の定理）
-- カップのコーヒーを（こぼさず・ちぎらず）かき混ぜると、どこか 1 点は必ず元の場所に戻ってくる（ブラウワー）
--   粒の色 = 最初の場所（角度 → 色相、中心からの距離 → 濃さ）。かき混ぜ写像を何回重ねても不動点はある
-- スペース = 1 回かき混ぜる / ドラッグ = スプーンの通り道を描き直す / ←→ = うずの強さ / A = ずれの矢印 / O = 元の場所への線 / R = やり直し
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Numeric (showEFloat, showFFloat)

-- 1 回のかき混ぜの設定（スプーンの区間 (x, y, dx, dy) とうずの強さ）
data Stir = Stir { stSegs :: ![(Float, Float, Float, Float)], stTwist :: !Float, stBase :: !Float }

data Fixed = Fixed { fxX, fxY, fxErr :: !Float }

data World = World
  { wOx, wOy, wX, wY, wSx, wSy, wTx, wTy :: !(U.Vector Float)  -- 粒：元の場所、今の場所、動く前と後
  , wColors  :: ![Color]              -- 粒の色
  , wSpoon   :: ![(Float, Float)]     -- スプーンの通り道（単位円の座標）
  , wStirs   :: ![Stir]               -- これまでのかき混ぜ（古い順）
  , wAnim    :: !Float                -- 0→1 でアニメーション
  , wFixed   :: !(Maybe Fixed)
  , wArrows  :: ![(Float, Float, Float, Float)]
  , wShowA   :: !Bool
  , wShowO   :: !Bool
  , wTwist   :: !Float
  , wDrawing :: !Bool
  }

particles :: Int
particles = 2400             -- ★ 粒の数

base0, sigma :: Float
base0 = 0.5                  -- ★ カップ全体の回転
sigma = 0.22                 -- ★ スプーンの影響の広さ

main :: IO ()
main = do
  w0 <- reset (World e e e e e e e e [] spoon0 [] 1 Nothing [] False False 2.2 False)
  runSketch (sketch "31-09 Stirring Coffee" w0)
    { skBack  = hsb 230 30 11
    , skStep  = stepWorld
    , skDraw  = drawWorld
    , skEvent = onEvent
    }
  where e = U.empty

spoon0 :: [(Float, Float)]
spoon0 = [ (-0.7 + 1.4 * t, 0.35 * sin (t * 2 * pi)) | i <- [0 .. 25 :: Int], let t = fromIntegral i * 0.04 ]

reset :: World -> IO World
reset w = do
  pts <- replicateM particles $ do
    u <- randomRIO (0, 1)
    a <- randomRIO (0, tau)
    let r = sqrt u * 0.98 :: Float
    pure (r * cos a, r * sin a)
  let xs = U.fromList (map fst pts)
      ys = U.fromList (map snd pts)
      colorOf (x, y) = hsb (wrapF 360 (atan2 y x / tau * 360 + 360)) (30 + 60 * norm2 (x, y)) 95
  pure w { wOx = xs, wOy = ys, wX = xs, wY = ys, wSx = xs, wSy = ys, wTx = xs, wTy = ys
         , wColors = map colorOf pts, wStirs = [], wFixed = Nothing, wArrows = [], wAnim = 1 }

-- ★ 1 回のかき混ぜ：スプーンで押す（流れに沿って少しずつ動かす）→ うずで回す。カップの外には出さない
stirOnce :: Stir -> (Float, Float) -> (Float, Float)
stirOnce st (x0, y0) =
  let push :: Int -> Float -> Float -> (Float, Float)
      push 0 !x !y = (x, y)
      push k !x !y =
        let (dx, dy) = foldl' (\(!ax, !ay) (sx, sy, ddx, ddy) ->
                                  let w = exp (negate ((x - sx) * (x - sx) + (y - sy) * (y - sy)) / (sigma * sigma))
                                  in (ax + ddx * w, ay + ddy * w)) (0, 0) (stSegs st)
            x1 = x + dx * 0.5
            y1 = y + dy * 0.5
            r = sqrt (x1 * x1 + y1 * y1)
        in if r > 0.99 then push (k - 1) (x1 * 0.99 / r) (y1 * 0.99 / r) else push (k - 1) x1 y1
      (xe, ye) = push 6 x0 y0
      r2 = xe * xe + ye * ye
      ang = stBase st + stTwist st * (1 - r2)
      c = cos ang
      s = sin ang
  in (c * xe - s * ye, s * xe + c * ye)

total :: [Stir] -> (Float, Float) -> (Float, Float)
total stirs p = foldl' (flip stirOnce) p stirs

-- 不動点：格子で |M(p) − p| の小さい点を探し、まわりを細かく探して仕上げる
findFixed :: [Stir] -> (Fixed, [(Float, Float, Float, Float)])
findFixed stirs =
  let g = if length stirs > 4 then 30 else 44 :: Int
      gf = fromIntegral g :: Float
      cells = [ (x, y) | i <- [0 .. g - 1], j <- [0 .. g - 1]
                       , let x = -1 + 2 * (fromIntegral i + 0.5) / gf
                       , let y = -1 + 2 * (fromIntegral j + 0.5) / gf
                       , x * x + y * y <= 0.97 ]
      devOf p = dist2 (total stirs p) p
      (b0, d0) = foldl' (\(b, bd) p -> let d = devOf p in if d < bd then (p, d) else (b, bd)) ((0, 0), 1e9) cells
      refine :: Int -> Float -> (Float, Float) -> Float -> ((Float, Float), Float)
      refine 0 _ b bd = (b, bd)
      refine it h b@(bx, by) bd =
        let cand = [ (bx + dx, by + dy) | (dx, dy) <- [(h, 0), (-h, 0), (0, h), (0, -h), (h, h), (-h, -h), (h, -h), (-h, h)]
                                        , (bx + dx) * (bx + dx) + (by + dy) * (by + dy) <= 0.98 ]
            -- p5 版と同じく、見つかるたびに次の候補の基準を更新する
            (b1, d1) = foldl' (\(bb, dd) p -> let d = devOf p in if d < dd then (p, d) else (bb, dd)) (b, bd) cand
        in if d1 < bd then refine (it - 1) h b1 d1 else refine (it - 1) (h / 2) b bd
      ((fx, fy), err) = refine 40 (2 / gf) b0 d0
      arrows = [ (x, y, qx, qy)
               | i <- [0 .. 14 :: Int], j <- [0 .. 14 :: Int]
               , let x = -1 + 2 * (fromIntegral i + 0.5) / 15
               , let y = -1 + 2 * (fromIntegral j + 0.5) / 15
               , x * x + y * y < 0.95
               , let (qx, qy) = total stirs (x, y) ]
  in (Fixed fx fy err, arrows)

doStir :: World -> World
doStir w
  | length (wStirs w) >= 12 || wAnim w < 1 = w
  | otherwise =
      let segs = zipWith (\(ax, ay) (bx, by) -> (ax, ay, bx - ax, by - ay)) (wSpoon w) (drop 1 (wSpoon w))
          st = Stir segs (wTwist w) base0
          stirs = wStirs w ++ [st]
          moved = U.zipWith (\x y -> stirOnce st (x, y)) (wX w) (wY w)
          (fx, arrows) = findFixed stirs
      in w { wStirs = stirs, wSx = wX w, wSy = wY w
           , wTx = U.map fst moved, wTy = U.map snd moved
           , wAnim = 0, wFixed = Just fx, wArrows = arrows }

stepWorld :: Env -> Float -> World -> IO World
stepWorld _ _ w
  | wAnim w < 1 =
      let a = min 1 (wAnim w + 0.025)
          e = a * a * (3 - 2 * a)
      in pure w { wAnim = a
                , wX = U.zipWith (\s t -> lerpF s t e) (wSx w) (wTx w)
                , wY = U.zipWith (\s t -> lerpF s t e) (wSy w) (wTy w) }
  | otherwise = pure w

-- レイアウト
radiusR, cupX, cupY :: Env -> Float
radiusR env = min (envW env * 0.3) ((envH env - 160) / 2)
cupX env = 60 + radiusR env
cupY env = 110 + radiusR env

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $
  [ spot (hsb 25 60 22) (cx, cy) (r * 2 + 24)
  , spot (hsb 25 50 14) (cx, cy) (r * 2)
  ]
  -- 粒：色 = 最初の場所
  ++ zipWith drawPart [0 ..] (wColors w)
  ++ [ Color (hsba 0 0 100 0.25) (Line [toS (wOx w U.! i, wOy w U.! i), toS (wX w U.! i, wY w U.! i)])
     | wShowO w, i <- [0, 12 .. U.length (wX w) - 1] ]
  ++ [ Color (hsba 0 0 100 0.7) (thickLine 1.5 (toS (x, y)) (toS (lerpF x qx 0.5, lerpF y qy 0.5)))
     | wShowA w, wAnim w >= 1, (x, y, qx, qy) <- wArrows w ]
  -- スプーンの通り道
  ++ [ Color (hsba 0 0 100 0.6) (dashed 6 6 2 (map toS (wSpoon w))) | length (wSpoon w) > 1 ]
  ++ spoonHead
  -- 不動点
  ++ (case wFixed w of
        Just f | wAnim w >= 1 ->
          let (fx, fy) = toS (fxX f, fxY f)
          in [ hoop 3 white (fx, fy) 30, ln 3 white (fx - 22, fy) (fx + 22, fy), ln 3 white (fx, fy - 22) (fx, fy + 22) ]
        _ -> [])
  ++ [ txt 20 34 15 white ("Stirs: " ++ show cnt ++ "   Space: stir   Drag: spoon path   twist " ++ nf 1 (wTwist w) ++ " (Left/Right)   A: arrows   O: lines to origin   R: restart")
     , txt 20 62 14 (hsb 0 0 60) ("White cross = point that returns to its place after all " ++ show cnt ++ " stirs (fixed point). White grains = grains that happen to be near home.")
     ]
  ++ [ txt tx (130 + fromIntegral i * 26) 14 (hsb 0 0 80) l | (i, l) <- zip [0 :: Int ..] notes ]
  where
    r = radiusR env
    cx = cupX env
    cy = cupY env
    toS (x, y) = (cx + x * r, cy - y * r)
    cnt = length (wStirs w)
    drawPart i c =
      let x = wX w U.! i
          y = wY w U.! i
          back = cnt > 0 && dist2 (x, y) (wOx w U.! i, wOy w U.! i) < 0.04
          (px, py) = toS (x, y)
      in Color (if back then white else c) (Translate px py (disc 2.5))
    spoonHead = case reverse (wSpoon w) of
      (b : a : _) ->
        let (bx, by) = toS b
            (ax, ay) = toS a
            ang = atan2 (by - ay) (bx - ax)
            rotP (dx, dy) = (bx + dx * cos ang - dy * sin ang, by + dx * sin ang + dy * cos ang)
        in [ Color (hsba 0 0 100 0.7) (Polygon (map rotP [(0, 0), (-14, -7), (-14, 7)])) ]
      _ -> []
    tx = cx + r + 60
    notes =
      [ "A stir = a continuous map from cup (disc) to cup"
      , "-> Brouwer: some point must stay put"
      , (case wFixed w of
           Just f -> "Fixed point: (" ++ nf 3 (fxX f) ++ ", " ++ nf 3 (fxY f) ++ ")  error " ++ showEFloat (Just 1) (fxErr f) ""
           Nothing -> "(not stirred yet)")
      , ""
      , "If you may tear (discontinuous), you can escape:"
      , "cut it in half and swap, and every point moves"
      , ""
      , "Hairy ball theorem (series 27, sketch 12)"
      , "- Comb the hair on a ball: a cowlick must appear"
      , "  (a point where the direction field is 0: v = 0)"
      , "- Earth's wind: somewhere there is no wind"
      , "- Think of the coffee surface as a sphere: stirring"
      , "  leaves a point unmoved, seen from another angle"
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | dist2 m (cupX env, cupY env) < r -> pure w { wDrawing = True, wSpoon = [((mx - cupX env) / r, (cupY env - my) / r)] }
    | otherwise -> pure w
  EventMotion _ | wDrawing w ->
    let p = (clampF (-0.98) 0.98 ((mx - cupX env) / r), clampF (-0.98) 0.98 ((cupY env - my) / r))
        lastP = foldl (\_ q -> q) p (wSpoon w)
    in pure (if dist2 p lastP > 0.06 && length (wSpoon w) < 40 then w { wSpoon = wSpoon w ++ [p] } else w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrawing = False }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (doStir w)
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wTwist = max (-4) (wTwist w - 0.2) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wTwist = min 4 (wTwist w + 0.2) }
  EventKey (Char c) Down _ _
    | toLower c == 'a' -> pure w { wShowA = not (wShowA w) }
    | toLower c == 'o' -> pure w { wShowO = not (wShowO w) }
    | toLower c == 'r' -> reset w
  _ -> pure w
  where
    m@(mx, my) = mouseP env
    r = radiusR env

-- ---------- やってみよう ----------
-- 1. スペースを何回か押して、白い十字のまわりの粒の色が「元の色」に近いことを確かめよう
-- 2. スプーンの道を中心を通る直線にすると不動点はどこへ行く？ ぐるっと 1 周の道にすると？
-- 3. twist = 0、base0 = 0 にしてスプーンだけでかき混ぜる。不動点は何個できる？（A の矢印で探す）
-- 4. stirOnce の最後に「左右を入れ替える」（x → −x にして半分ずらす）と連続でなくなる。不動点は消える？

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

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> Float -> [Point] -> Picture
dashed on off w ps = Pictures (go True on ps)
  where
    go draw left (a : b : rest) =
      let l = dist2 a b
      in if l <= left
           then [thickLine w a b | draw] ++ go draw (left - l) (b : rest)
           else let t = left / l
                    m = (lerpF (fst a) (fst b) t, lerpF (snd a) (snd b) t)
                in [thickLine w a m | draw] ++ go (not draw) (if draw then off else on) (m : b : rest)
    go _ _ _ = []
