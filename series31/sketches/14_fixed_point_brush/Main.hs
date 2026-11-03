-- ============================================
-- 14 不動点ブラシ 🔊（自由制作のベース）
-- 枠（平行四辺形）1 つ = 縮小写像 1 つ。大きい正方形をその枠へ写す。枠を動かすと、IFS のアトラクタ（05 の不動点）がその場で描き直される
--   自分で描いた線も、枠の写像で何重にも写されてアトラクタに吸い込まれる。カオスゲームの 1 歩ごとに、選ばれた枠の音が鳴る
-- クリックで音をスタート / 枠の丸をドラッグ（●＝移動、○＝角）/ 空いた所をドラッグ = 線を描く / N = 枠を足す / X = 枠を消す / 1〜3 = 見本 / ↑↓ = 線を写す深さ / H = 枠を隠す / M = 音 / C = 線を消す / S = 保存
-- ============================================
module Main (main) where

import Art
import Control.Monad (when)
import Data.Bits (shiftL, shiftR, xor)
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import Numeric (showFFloat)

-- 枠：単位正方形の (0,0), (1,0), (0,1) の行き先
data Frame = Frame { fO, fX, fY :: !(Float, Float) }

data World = World
  { wMaps    :: ![Frame]
  , wStrokes :: ![[(Float, Float)]]   -- 描いた線（単位正方形の座標、古い順）
  , wDepth   :: !Int                  -- ★ 線を何回写すか
  , wAccR, wAccG, wAccB :: !(MU.IOVector Float)   -- アトラクタの明るさをためる
  , wImgR, wImgG, wImgB :: !(U.Vector Float)      -- その写し（描画用）
  , wMaxS    :: !Float
  , wPx, wPy :: !Float
  , wRng     :: !Word32
  , wGrab    :: !(Maybe (Int, Char))  -- つかんだ枠と丸（'o' 'x' 'y'）
  , wDrawing :: !Bool
  , wShowF   :: !Bool
  , wMute    :: !Bool
  , wAudio   :: !Bool
  , wNextN   :: !Float
  , wHead    :: !(Float, Float)       -- 音のカオスゲームの点
  , wHeadM   :: !Int
  }

res, iterPerFrame, maxStrokes :: Int
res = 280                  -- ★ アトラクタの解像度
iterPerFrame = 20000       -- ★ 1 フレームのカオスゲームの回数
maxStrokes = 24            -- 覚えておく線の本数（古い線から消える）

main :: IO ()
main = do
  w0 <- makeWorld
  runSketch (sketch "31-14 Fixed Point Brush" w0)
    { skBack  = hsb 230 30 11
    , skStep  = stepWorld
    , skDraw  = drawWorld
    , skEvent = onEvent
    }

makeWorld :: IO World
makeWorld = do
  r <- MU.replicate (res * res) 0
  g <- MU.replicate (res * res) 0
  b <- MU.replicate (res * res) 0
  let e = U.replicate (res * res) 0
  pure World
    { wMaps = presetOf 1, wStrokes = [], wDepth = 3, wAccR = r, wAccG = g, wAccB = b
    , wImgR = e, wImgG = e, wImgB = e, wMaxS = 1, wPx = 0.5, wPy = 0.5, wRng = 88172645
    , wGrab = Nothing, wDrawing = False, wShowF = True, wMute = False, wAudio = False
    , wNextN = 0, wHead = (0.5, 0.5), wHeadM = 0 }

presetOf :: Int -> [Frame]
presetOf 1 = [Frame (0, 0) (0.5, 0) (0, 0.5), Frame (0.5, 0) (1, 0) (0.5, 0.5), Frame (0.25, 0.5) (0.75, 0.5) (0.25, 1)]
presetOf 2 = [Frame (0.46, 0) (0.54, 0) (0.46, 0.45), Frame (0.5, 0.35) (0.85, 0.6) (0.25, 0.7), Frame (0.5, 0.35) (0.75, 0.0) (0.15, 0.6), Frame (0.3, 0.45) (0.7, 0.45) (0.3, 0.85)]
presetOf _ = [Frame (0.15, 0.1) (0.85, 0.2) (0.05, 0.8), Frame (0.75, 0.7) (0.95, 0.75) (0.7, 0.92)]

clearAcc :: World -> IO ()
clearAcc w = MU.set (wAccR w) 0 >> MU.set (wAccG w) 0 >> MU.set (wAccB w) 0

-- ★ 写像 w_i(u, v) = o + u·(x − o) + v·(y − o)
wmap :: Frame -> (Float, Float) -> (Float, Float)
wmap (Frame (ox, oy) (xx, xy) (yx, yy)) (u, v) =
  (ox + u * (xx - ox) + v * (yx - ox), oy + u * (xy - oy) + v * (yy - oy))

ratio :: Frame -> Float
ratio (Frame (ox, oy) (xx, xy) (yx, yy)) =
  let a = xx - ox
      c = xy - oy
      b = yx - ox
      d = yy - oy
      s1 = a * a + b * b + c * c + d * d
      det = a * d - b * c
  in sqrt ((s1 + sqrt (max 0 (s1 * s1 - 4 * det * det))) / 2)

mapHue :: Int -> Float
mapHue i = wrapF 360 (fromIntegral i * 57 + 190)

-- カオスゲーム：点をランダムな枠で写し続けて、通った画素を明るくする
runChaos :: World -> IO World
runChaos w = do
  let n = length (wMaps w)
      flat = U.fromList (concat
        [ [ox, oy, xx - ox, xy - oy, yx - ox, yy - oy, r, g, b]
        | (i, Frame (ox, oy) (xx, xy) (yx, yy)) <- zip [0 ..] (wMaps w)
        , let (cr, cg, cb, _) = rgbaOfColor (hsb (mapHue i) 60 100)
        , let r = cr * 255, let g = cg * 255, let b = cb * 255 ])
      rf = fromIntegral res :: Float
      accR = wAccR w
      accG = wAccG w
      accB = wAccB w
      loop :: Int -> Float -> Float -> Word32 -> IO (Float, Float, Word32)
      loop !k !px !py !s
        | k >= iterPerFrame = pure (px, py, s)
        | otherwise = do
            let s1 = rnd s
                i = min (n - 1) (floor (rnd01 s1 * fromIntegral n))
                o = i * 9
                at' j = U.unsafeIndex flat (o + j)
                qx = at' 0 + px * at' 2 + py * at' 4
                qy = at' 1 + px * at' 3 + py * at' 5
            if not (abs qx < 3 && abs qy < 3)
              then loop (k + 1) 0.5 0.5 s1
              else do
                let bx = floor (qx * rf) :: Int
                    by = floor ((1 - qy) * rf) :: Int
                when (k >= 20 && bx >= 0 && by >= 0 && bx < res && by < res) $ do
                  let p = by * res + bx
                  MU.unsafeModify accR (+ at' 6) p
                  MU.unsafeModify accG (+ at' 7) p
                  MU.unsafeModify accB (+ at' 8) p
                loop (k + 1) qx qy s1
  (px, py, seed) <- if n == 0 then pure (wPx w, wPy w, wRng w) else loop 0 (wPx w) (wPy w) (wRng w)
  r <- U.freeze accR
  g <- U.freeze accG
  b <- U.freeze accB
  let maxS = goMax 0 1
      goMax :: Int -> Float -> Float
      goMax !p !mx
        | p >= res * res = mx
        | otherwise = goMax (p + 7) (max mx (U.unsafeIndex r p + U.unsafeIndex g p + U.unsafeIndex b p))
  pure w { wPx = px, wPy = py, wRng = seed, wImgR = r, wImgG = g, wImgB = b, wMaxS = maxS }

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  w <- runChaos w0
  let n = length (wMaps w)
  -- 音：カオスゲームを 1 歩ずつゆっくり進めて、選ばれた枠の音を鳴らす
  if not (wMute w) && wAudio w && envTime env > wNextN w && n > 0
    then do
      let s1 = rnd (wRng w)
          i = min (n - 1) (floor (rnd01 s1 * fromIntegral n))
          q@(qx, qy) = case drop i (wMaps w) of
            (m : _) -> wmap m (wHead w)
            [] -> wHead w
          hd@(_, hy) = if abs qx < 3 && abs qy < 3 then q else (0.5, 0.5)
          oct = floor (hy * 2) :: Int
          f = 220 * 2 ** (fromIntegral (scaleOf i + 12 * oct) / 12) :: Float
      playTone (clampF 50 3000 f) 0.2 0.4 Sine
      pure w { wRng = s1, wHead = hd, wHeadM = i, wNextN = envTime env + 0.16 }
    else pure w

-- ★ 枠ごとの音（短調の五音音階）
scaleOf :: Int -> Int
scaleOf i = case drop (i `mod` 6) [0, 3, 5, 7, 10, 12] of
  (s : _) -> s
  [] -> 0

-- レイアウト
bx0, by0 :: Float
bx0 = 40
by0 = 100

sizeD :: Env -> Float
sizeD env = min (envW env * 0.62) (envH env - 140)

toUV :: Env -> (Float, Float)
toUV env = let (mx, my) = mouseP env
               d = sizeD env
           in ((mx - bx0) / d, 1 - (my - by0) / d)

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $
  [ Translate (bx0 + d / 2) (by0 + d / 2) (Scale (d / fromIntegral res) (negate d / fromIntegral res) attractor)
  , box (hsb 0 0 40) bx0 by0 d d ]
  ++ drawStrokes env w
  ++ (if wShowF w then concat (zipWith (drawFrame env) [0 ..] (wMaps w)) else [])
  ++ [ spot (hsb (mapHue (wHeadM w)) 40 100) (toS (wHead w)) 12
     , txt tx 130 15 white ("Frames: " ++ show (length (wMaps w)) ++ "   stroke depth: " ++ show (wDepth w))
     ]
  ++ [ txt tx (164 + fromIntegral i * 24) 15 (hsb (mapHue i) 60 100) ("Frame " ++ show (i + 1) ++ ": ratio " ++ nf 2 r ++ (if r >= 1 then "  <- 1 or more! not a contraction" else ""))
     | (i, m) <- zip [0 ..] (wMaps w), let r = ratio m ]
  ++ [ txt tx (164 + fromIntegral (length (wMaps w)) * 24 + 20 + fromIntegral i * 22) 14 (hsb 0 0 70) l | (i, l) <- zip [0 :: Int ..] notes ]
  ++ [ txt 20 34 15 white (sound ++ "   N: add frame   X: remove frame   1-3: presets   Up/Down: depth   H: frames   M: sound   C: clear strokes   S: save")
     , txt 20 62 14 (hsb 0 0 60) "Attractor = the shape unchanged by all the frames together (fixed point of the Hutchinson operator). Color = last frame used."
     ]
  where
    d = sizeD env
    tx = bx0 + d + 40
    toS (u, v) = (bx0 + u * d, by0 + (1 - v) * d)
    sound | not (wAudio w) = "Click to start sound"
          | wMute w = "Sound: off"
          | otherwise = "Sound on"
    notes =
      [ "Drag a filled dot: move the frame"
      , "Drag an open dot: move a corner (rotate/stretch)"
      , "Drag empty space: draw a stroke (white)"
      , "Strokes are copied by all frames, " ++ show (wDepth w) ++ " times"
      , "-> more depth: drawn into the attractor's shape"
      , ""
      , "Ideas: tree, snowflake, spiral, your initial"
      , "If all ratios are below 1, the shape is unique"
      ]
    sc = 255 / log (1 + wMaxS w / 3)
    q :: Float -> Word8
    q v = fromIntegral (min 255 (floor v :: Int))
    attractor = pixelImage res res 1 $ \x y ->
      let p = y * res + x
          r = U.unsafeIndex (wImgR w) p
          g = U.unsafeIndex (wImgG w) p
          b = U.unsafeIndex (wImgB w) p
          s = r + g + b
      in if s == 0 then (14, 16, 28)
         else let l = log (1 + s / 3) * sc / s * 3 in (q (r * l), q (g * l), q (b * l))

drawFrame :: Env -> Int -> Frame -> [Picture]
drawFrame env i m@(Frame o@(ox, oy) x@(xx, xy) y@(yx, yy)) =
  [ Color frameCol (thickPath 2 [toS o, toS x, toS (xx + yx - ox, xy + yy - oy), toS y, toS o])
  , Color frameCol (thickLine 5 (toS o) (toS x))   -- 向きがわかるように、元の正方形の「下の辺」を太く
  , spot c (toS o) 14
  , hoop 2 c (toS x) 12
  , hoop 2 c (toS y) 12
  ]
  where
    d = sizeD env
    toS (u, v) = (bx0 + u * d, by0 + (1 - v) * d)
    bad = ratio m >= 1
    frameCol = if bad then hsb 0 90 100 else hsb (mapHue i) 60 100
    c = hsb (mapHue i) 60 100

-- 線をすべての合成写像で写して描く（深さ wDepth まで、多すぎるときは打ち切り）
drawStrokes :: Env -> World -> [Picture]
drawStrokes env w
  | null (wStrokes w) = []
  | otherwise =
      [ Color col (Line [ toS (applyM mat p) | p <- thin s ]) | mat <- mats, s <- strokes ]
      ++ (if wDepth w > 0 then [ Color (hsba 0 0 100 0.4) (Line (map toS s)) | s <- strokes ] else [])
  where
    d = sizeD env
    toS (u, v) = (bx0 + u * d, by0 + (1 - v) * d)
    strokes = filter (\s -> length s >= 2) (wStrokes w)
    maps = wMaps w
    grow :: Int -> [Maybe Frame] -> [Maybe Frame]
    grow 0 ms = ms
    grow k ms
      | null maps || length ms * length maps > 400 = ms
      | otherwise = grow (k - 1) [ compose mm m | mm <- ms, m <- maps ]
    compose Nothing m = Just m
    compose (Just mm) m = Just (Frame (wmap mm (fO m)) (wmap mm (fX m)) (wmap mm (fY m)))
    mats = grow (wDepth w) [Nothing]
    applyM Nothing p = p
    applyM (Just m) p = wmap m p
    col = hsba 0 0 100 (max 12 (90 / sqrt (fromIntegral (length mats))) / 100)
    thin s = let st = max 1 (length s `div` 40) in everyNth st s

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseP env
        d = sizeD env
        toS (u, v) = (bx0 + u * d, by0 + (1 - v) * d)
        (u0, v0) = toUV env
        hits = if wShowF w
          then [ (i, k) | (i, f) <- reverse (zip [0 ..] (wMaps w))
                        , (k, p) <- [('x', fX f), ('y', fY f), ('o', fO f)]
                        , dist2 m (toS p) < 12 ]
          else []
    in pure $ case hits of
         (h : _) -> w { wAudio = True, wGrab = Just h }
         [] | u0 >= 0 && u0 <= 1 && v0 >= 0 && v0 <= 1 ->
                let ss = wStrokes w ++ [[(u0, v0)]]
                in w { wAudio = True, wGrab = Nothing, wDrawing = True, wStrokes = drop (length ss - maxStrokes) ss }
            | otherwise -> w { wAudio = True, wGrab = Nothing }
  EventMotion _ -> case wGrab w of
    Just (i, k) -> do
      let (u, v) = toUV env
          move f = case k of
            'o' -> let (ox, oy) = fO f
                       du = u - ox
                       dv = v - oy
                       sh (a, b) = (a + du, b + dv)
                   in Frame (sh (fO f)) (sh (fX f)) (sh (fY f))
            'x' -> f { fX = (u, v) }
            _ -> f { fY = (u, v) }
      clearAcc w
      pure w { wMaps = [ if j == i then move f else f | (j, f) <- zip [0 ..] (wMaps w) ] }
    Nothing
      | wDrawing w ->
          let (u, v) = toUV env
              addTo s =
                let l = foldl (\_ p -> p) (u, v) s
                in if dist2 (u, v) l > 0.005 && length s < 300 then s ++ [(clampF 0 1 u, clampF 0 1 v)] else s
              ss = wStrokes w
          in pure w { wStrokes = take (length ss - 1) ss ++ map addTo (drop (length ss - 1) ss) }
      | otherwise -> pure w
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wGrab = Nothing, wDrawing = False }
  EventKey k Down _ _ -> keyDown env k w { wAudio = True }
  _ -> pure w

keyDown :: Env -> Key -> World -> IO World
keyDown env k w = case k of
  SpecialKey KeyUp -> pure w { wDepth = min 6 (wDepth w + 1) }
  SpecialKey KeyDown -> pure w { wDepth = max 0 (wDepth w - 1) }
  Char c
    | c >= '1' && c <= '3' -> clearAcc w >> pure w { wMaps = presetOf (fromEnum c - fromEnum '0') }
    | toLower c == 'n' && length (wMaps w) < 6 -> do
        cu <- randomRIO (0.2, 0.6)
        cv <- randomRIO (0.2, 0.6)
        clearAcc w
        pure w { wMaps = wMaps w ++ [Frame (cu, cv) (cu + 0.3, cv + 0.05) (cu - 0.05, cv + 0.3)] }
    | toLower c == 'x' && length (wMaps w) > 1 -> do
        clearAcc w
        pure w { wMaps = take (length (wMaps w) - 1) (wMaps w) }
    | toLower c == 'h' -> pure w { wShowF = not (wShowF w) }
    | toLower c == 'm' -> pure w { wMute = not (wMute w) }
    | toLower c == 'c' -> pure w { wStrokes = [] }
    | toLower c == 's' -> saveSvg "fixed_point_brush" env (drawWorld env w) >> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 見本 1（シェルピンスキー）の上の枠を少し回すだけで、形がどう変わる？ 3 つの枠を全部同じ向きに回すと？
-- 2. 枠の ○ を外へ引っぱって縮小率を 1 より大きくすると、アトラクタはどうなる？（赤い警告）
-- 3. 線で顔を描いて深さを 0 → 6 と上げよう。どの絵から始めても最後は同じ形 = 不動点
-- 4. scaleOf や playTone の音色を変えて、形ごとに違う音楽にしよう。枠が多い形ほど音の種類も増える

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

-- 速い乱数（xorshift）
rnd :: Word32 -> Word32
rnd s0 = let s1 = s0 `xor` (s0 `shiftL` 13)
             s2 = s1 `xor` (s1 `shiftR` 17)
         in s2 `xor` (s2 `shiftL` 5)

rnd01 :: Word32 -> Float
rnd01 s = fromIntegral (s `shiftR` 8) / 16777216

everyNth :: Int -> [a] -> [a]
everyNth _ [] = []
everyNth n (x : xs) = x : everyNth n (drop (n - 1) xs)
