-- ============================================
-- 00 平面鏡と反射の法則
-- 光源（マウス）から扇状に光線を出し、鏡に当たると「入射角 = 反射角」で跳ね返る
--   反射の式： d' = d − 2 (d·n) n   （d は光の向き、n は鏡の法線ベクトル）
-- マウス = 光源 / 鏡の端点をドラッグ = 鏡の角度 / ↑↓ = 光線の本数 / ←→ = 扇の向き / M = 鏡を追加 / D = 最後の鏡を消す / V = 鏡像 / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showFFloat)

spread :: Float
spread = 50                -- ★ 扇の広がり（度）

maxBounce :: Int
maxBounce = 6              -- ★ 反射の最大回数

data Mirror = Mirror !Point !Point   -- 鏡の両端

-- 最初に鏡に当たったときの情報
data Hit = Hit
  { hX, hY   :: !Float     -- 当たった場所
  , hNx, hNy :: !Float     -- 鏡の法線（長さ 1）
  , hDx, hDy :: !Float     -- 入射の向き
  , hDot     :: !Float     -- d·n
  }

data World = World
  { wRays    :: !Int              -- ★ 光線の本数
  , wAim     :: !Float            -- 扇の向きのずらし（ラジアン）
  , wShow    :: !Bool             -- 鏡像（虚像）を表示
  , wMirrors :: ![Mirror]         -- 鏡の一覧
  , wDrag    :: !(Maybe (Int, Bool))  -- つかんでいる端点（鏡の番号、a 側か）
  , wSrc     :: !Point            -- 光源
  }

resetMirrors :: Env -> World -> World
resetMirrors env w = w
  { wMirrors =
      [ Mirror (wd * 0.62, h * 0.25) (wd * 0.72, h * 0.75)
      , Mirror (wd * 0.2, h * 0.85) (wd * 0.55, h * 0.9) ]
  , wAim = 0 }
  where wd = envW env
        h = envH env

main :: IO ()
main = runSketch (sketch "36-00 Plane Mirrors and Reflection" (World 15 0 True [] Nothing (300, 400)))
  { skBack  = hsb 230 40 8
  , skStart = \env -> pure (resetMirrors env (World 15 0 True [] Nothing (300, 400)))
  , skStep  = \env _ w -> pure (case wDrag w of
      Nothing -> w { wSrc = mouseP5 env }
      Just _  -> w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- 光線（p から向き d）と線分 a-b の交点。t = 光線の進んだ距離
raySeg :: Point -> Point -> Mirror -> Maybe Float
raySeg (px, py) (dx, dy) (Mirror (ax, ay) (bx, by))
  | abs den < 1e-9 = Nothing
  | t > 1e-3 && s >= 0 && s <= 1 = Just t
  | otherwise = Nothing
  where
    ex = bx - ax
    ey = by - ay
    den = dx * ey - dy * ex
    t = ((ax - px) * ey - (ay - py) * ex) / den
    s = ((ax - px) * dy - (ay - py) * dx) / den

-- いちばん近い鏡
nearestMirror :: [Mirror] -> Point -> Point -> Maybe (Float, Mirror)
nearestMirror ms p d = foldl' pick Nothing ms
  where
    pick best m = case (raySeg p d m, best) of
      (Nothing, _) -> best
      (Just t, Nothing) -> Just (t, m)
      (Just t, Just (t0, _)) -> if t < t0 then Just (t, m) else best

-- ★ 光線を追跡する：鏡に当たるたびに反射。点の列と、最初に当たった鏡の情報を返す
traceRay :: [Mirror] -> Point -> Point -> ([Point], Maybe Hit)
traceRay ms p0 d0 = go maxBounce p0 d0 [p0] Nothing
  where
    finish (px, py) (dx, dy) acc first = (reverse ((px + dx * 3000, py + dy * 3000) : acc), first)   -- 最後は画面の外まで伸ばす
    go :: Int -> Point -> Point -> [Point] -> Maybe Hit -> ([Point], Maybe Hit)
    go 0 p d acc first = finish p d acc first
    go k p@(px, py) d@(dx, dy) acc first = case nearestMirror ms p d of
      Nothing -> finish p d acc first
      Just (t, m) ->
        let hx = px + dx * t
            hy = py + dy * t
            (nx, ny) = normalOf m
            dp = dx * nx + dy * ny
            hit = Hit hx hy nx ny dx dy dp
            first' = maybe (Just hit) Just first
            -- 反射： d' = d − 2(d·n)n
            d' = (dx - 2 * dp * nx, dy - 2 * dp * ny)
        in go (k - 1) (hx, hy) d' ((hx, hy) : acc) first'

-- 鏡の法線（長さ 1）
normalOf :: Mirror -> Point
normalOf (Mirror (ax, ay) (bx, by)) =
  let mx = bx - ax
      my = by - ay
      len = max 1e-6 (norm2 (mx, my))
  in (-my / len, mx / len)

-- 点 p を鏡 m で折り返した位置
reflectPoint :: Point -> Mirror -> Point
reflectPoint (x, y) (Mirror (ax, ay) (bx, by)) =
  let mx = bx - ax
      my = by - ay
      len2 = max 1e-6 (mx * mx + my * my)
      t = ((x - ax) * mx + (y - ay) * my) / len2
      fx = ax + mx * t       -- 足
      fy = ay + my * t
  in (2 * fx - x, 2 * fy - y)

drawAll :: Env -> World -> Picture
drawAll env w =
  let ms = wMirrors w
      src@(sx, sy) = wSrc w
      -- 扇の中心方向：いちばん近い鏡の真ん中へ向ける（←→ でずらせる）
      centers = [ ((ax + bx) / 2, (ay + by) / 2) | Mirror (ax, ay) (bx, by) <- ms ]
      near = foldl' (\b c -> case b of
                        Nothing -> Just c
                        Just c0 -> if dist2 c src < dist2 c0 src then Just c else b) Nothing centers
      aim = case near of
        Just (cx, cy) -> atan2 (cy - sy) (cx - sx) + wAim w
        Nothing -> wAim w
      half = spread * pi / 180 / 2
      n = wRays w
      thetaOf i | n == 1 = aim
                | otherwise = aim - half + 2 * half * fromIntegral i / fromIntegral (n - 1)
      traced = [ (i, traceRay ms src (cos th, sin th)) | i <- [0 .. n - 1], let th = thetaOf i ]
      central = [ f | (i, (_, Just f)) <- traced, i == n `div` 2 ]
      -- 鏡像（光源を鏡で折り返した位置）
      images = if not (wShow w) then [] else concat
        [ [ Color (hsbA 0 0 100 25) (dashed src im)
          , Color (hsbA 45 80 100 50) (at im (ring 7 1.5)) ]
        | m <- ms, let im = reflectPoint src m ]
      rays = [ Color (hsbA 45 60 100 55) (thickPath 1.5 pts) | (_, (pts, _)) <- traced ]
  in Pictures
    [ onP5 env (Pictures
        ( images ++ rays ++ map drawMirror ms ++ concatMap centralMarks central ++
        [ Color (hsb 45 20 100) (at src (disc 8))
        , Color (hsbA 45 60 100 30) (at src (disc 20)) ]))
    , Pictures [ txt env (fx + nx * 130 - 60) (fy + ny * 130) 16 white
                   ("Incidence " ++ nf 1 deg ++ " deg  Reflection " ++ nf 1 deg ++ " deg")
               | f <- central
               , let (fx, fy) = (hX f, hY f)
                     sgn = if hDot f > 0 then -1 else 1
                     nx = hNx f * sgn
                     ny = hNy f * sgn
                     deg = acos (min 1 (abs (hDot f))) * 180 / pi ]
    , txt env 20 40 18 white
        ("Rays " ++ show n ++ " (Up/Down)  Fan (L/R)  Mirrors " ++ show (length ms)
         ++ " (M add, D del)  V: image " ++ onOff (wShow w) ++ "  R: reset")
    , txt env 20 68 15 (hsb 0 0 60)
        "Drag a white end point to turn a mirror. Incidence = reflection, measured from the normal."
    , txt env 20 92 15 (hsb 0 0 60) "Yellow circle: the source seen in the mirror (virtual image)."
    ]

-- 鏡（本体、裏側のハッチング、端点）
drawMirror :: Mirror -> Picture
drawMirror m@(Mirror a@(ax, ay) b@(bx, by)) =
  let mx = bx - ax
      my = by - ay
      len = max 1e-6 (norm2 (mx, my))
      (nx, ny) = normalOf m
      steps = floor (len / 10) :: Int
      hatch = [ Line [ (hx, hy), (hx + nx * 8 + mx / len * 6, hy + ny * 8 + my / len * 6) ]
              | i <- [0 .. steps]
              , let s = fromIntegral i * 10 / len
                    hx = ax + mx * s
                    hy = ay + my * s ]
  in Pictures
    [ Color (hsb 190 20 100) (thickLine 5 a b)
    , Color (hsbA 190 20 100 50) (Pictures hatch)
    , Color white (at a (disc 5))
    , Color white (at b (disc 5)) ]

-- 中央の光線の入射角・反射角を描く
centralMarks :: Hit -> [Picture]
centralMarks f =
  let sgn = if hDot f > 0 then -1 else 1 :: Float      -- 法線を光の来た側へ向ける
      nx = hNx f * sgn
      ny = hNy f * sgn
      p = (hX f, hY f)
      an = atan2 ny nx
      ain = atan2 (negate (hDy f)) (negate (hDx f))    -- 入射光が来た向き
      aout = atan2 (hDy f - 2 * hDot f * hNy f) (hDx f - 2 * hDot f * hNx f)
  in [ Color (hsbA 0 0 100 60) (dashed p (hX f + nx * 120, hY f + ny * 120))
     , Color (hsb 45 90 100) (arcBetween p 70 an ain)
     , Color (hsb 170 80 100) (arcBetween p 90 an aout) ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wDrag = grab }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = Nothing }
  EventMotion _ -> pure (case wDrag w of
    Just (i, isA) | envDown env -> w { wMirrors = moveEnd i isA (wMirrors w) }
    _ -> w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wRays = min 120 (wRays w + 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wRays = max 1 (wRays w - 2) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wAim = wAim w - 0.1 }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wAim = wAim w + 0.1 }
  EventKey (Char c) Down _ _
    | c `elem` "mM" -> do
        cx <- randomRIO (envW env * 0.2, envW env * 0.8)
        cy <- randomRIO (envH env * 0.2, envH env * 0.8)
        a <- randomRIO (0, tau)
        let len = 140
            new = Mirror (cx - cos a * len, cy - sin a * len) (cx + cos a * len, cy + sin a * len)
        pure w { wMirrors = wMirrors w ++ [new] }
    | c `elem` "dD" && length (wMirrors w) > 1 -> pure w { wMirrors = init (wMirrors w), wDrag = Nothing }
    | c `elem` "vV" -> pure w { wShow = not (wShow w) }
    | c `elem` "rR" -> pure (resetMirrors env w) { wDrag = Nothing }
  _ -> pure w
  where
    mouse = mouseP5 env
    -- 最初に見つかった端点をつかむ
    grab = case [ (i, isA)
                | (i, Mirror a b) <- zip [0 ..] (wMirrors w)
                , (isA, p) <- [(True, a), (False, b)]
                , dist2 mouse p < 16 ] of
      (g : _) -> Just g
      [] -> Nothing
    moveEnd i isA ms =
      [ if j /= i then m
        else if isA then Mirror mouse b else Mirror a mouse
      | (j, m@(Mirror a b)) <- zip [0 ..] ms ]

-- ---------- やってみよう ----------
-- 1. 鏡を回して、入射角と反射角がいつも同じになることを確かめよう（法線からはかる）
-- 2. 2 枚の鏡を 90° に組むと、光はどこから来ても元の向きへ戻る（コーナーリフレクター）。作ってみよう
-- 3. 黄色の丸（虚像）はどこにある？ 鏡を挟んで光源と対称な位置 → 鏡の中に見える「像」の正体
-- 4. maxBounce を 30 にして、向かい合わせの 2 枚の鏡で光を閉じ込めよう


-- ==================================================
-- ここから下は「道具」（座標の変換・色・数字・破線）
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を画面に合わせる
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- p5.js の text(s, x, y) と同じ位置に文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

-- マウスの位置（左上が原点、y は下向き）
mouseP5 :: Env -> Point
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 透明度を 0〜100 で書く HSB（p5.js と同じ）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- 2 つの角度の間の弧
arcBetween :: Point -> Float -> Float -> Float -> Picture
arcBetween (x, y) r a1 a2 =
  let d = atan2 (sin (a2 - a1)) (cos (a2 - a1))
  in thickPath 2 [ (x + r * cos a, y + r * sin a) | k <- [0 .. 16 :: Int], let a = a1 + d * fromIntegral k / 16 ]

-- 点線
dashed :: Point -> Point -> Picture
dashed (x1, y1) (x2, y2) =
  let n = max 1 (floor (dist2 (x1, y1) (x2, y2) / 12)) :: Int
      f i = fromIntegral i / fromIntegral n
      pt t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
  in Pictures [ Line [pt (f i), pt (f (i + 1))] | i <- [0, 2 .. n - 1] ]
