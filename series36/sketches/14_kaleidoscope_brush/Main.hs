-- ============================================
-- 14 万華鏡ブラシ
-- 鏡を N 枚、放射状に組んだ万華鏡。描いた線が N 回対称に映り、鏡なので裏返しの像も出る（2N 個のコピー）
--   1 本の線を描く → 中心まわりに 360°/N ずつ回した位置にも描く → さらに鏡で折り返した位置にも描く
-- ドラッグ = 描く / ↑↓ = 鏡の枚数 N / M = 鏡の折り返し ON/off / 1〜3 = ブラシ / W = 鏡の線を見る / C = 消す / S = 保存
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)

-- 絵をためておく層（1280 × 800 の絵を cell ピクセルごとに 1 マス、R, G, B を 0〜1 の小数で）
gw, gh :: Int
gw = 356
gh = 222

cell, canvasW, canvasH :: Float
cell = 3.6                 -- ★ 層のマスの大きさ（px）
canvasW = 1280
canvasH = 800

data World = World
  { wN      :: !Int        -- ★ 鏡の枚数（対称の数）
  , wMirror :: !Bool       -- 折り返し（鏡像）も描く
  , wBrush  :: !Int        -- 1 線、2 粒、3 光
  , wWedge  :: !Bool
  , wHue    :: !Float
  , wPrev   :: !Point      -- 1 フレーム前のマウス
  , wLayer  :: !(U.Vector Float)
  }

blankLayer :: U.Vector Float
blankLayer = U.generate (gw * gh * 3) (\i -> rgbOf (hsb 230 40 6) (i `mod` 3))

rgbOf :: Color -> Int -> Float
rgbOf c k = let (r, g, b, _) = rgbaOfColor c in case k of { 0 -> r; 1 -> g; _ -> b }

main :: IO ()
main = runSketch (sketch "36-14 Kaleidoscope Brush" (World 6 True 1 False 0 (0, 0) blankLayer))
  { skBack  = hsb 230 40 6
  , skStep  = stepWorld
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- ★ 点 (x, y) を万華鏡の 2N 個の像に写す（中心まわりの回転 + 鏡の折り返し）
images :: World -> Point -> [Point]
images w (x, y) =
  let cx = canvasW / 2
      cy = canvasH / 2
      dx = x - cx
      dy = y - cy
      r = norm2 (dx, dy)
      a = atan2 dy dx
      n = wN w
      pt ang = (cx + r * cos ang, cy + r * sin ang)
  in concat [ pt (a + rot) : [ pt (negate a + rot) | wMirror w ]   -- 鏡像：角度を反転
            | k <- [0 .. n - 1], let rot = tau * fromIntegral k / fromIntegral n ]

-- 1 回分のスタンプ
data Stamp
  = Capsule !Point !Point !Float !Color    -- 太さのある線（両端、太さ、色）
  | Dot !Point !Float !Color               -- 丸（中心、半径、色）

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  let w = w0 { wHue = wrapF 360 (wHue w0 + 0.4), wPrev = mouse }
      prev = wPrev w0
      mouse = mouseCanvas env
      v = dist2 prev mouse
      hue = wrapF 360 (wHue w + v * 2)
  if not (envDown env) || v == 0 then pure w else do
    let as = images w0 prev
        bs = images w0 mouse
        pairs = zip as bs
    stamps <- case wBrush w0 of
      1 -> pure [ Capsule a b (max 1.5 (14 - v * 0.5)) (hsba hue 70 100 0.9) | (a, b) <- pairs ]
      2 -> concat <$> mapM (grains hue) pairs
      _ -> pure (concat
             [ [ Dot b ((10 + fromIntegral k * 10 + v) / 2) (hsba hue 50 100 0.06) | k <- [4, 3, 2, 1 :: Int] ]
               ++ [ Dot b 2 (hsba 0 0 100 0.6) ]
             | (_, b) <- pairs ])
    pure w { wLayer = U.modify (\m -> forM_ stamps (paint m)) (wLayer w) }
  where
    -- 粒のブラシ：線の途中に小さな丸を 3 つ散らす
    grains hue ((ax, ay), (bx, by)) = mapM (\k -> do
      let t = fromIntegral (k :: Int) / 3
      ox <- randomRIO (-4, 4)
      oy <- randomRIO (-4, 4)
      d <- randomRIO (2, 7)
      pure (Dot (ax + (bx - ax) * t + ox, ay + (by - ay) * t + oy) (d / 2) (hsba hue 60 100 0.8))) [0, 1, 2]

-- 層にスタンプを塗る（半透明の色を重ねる）
paint :: MU.MVector s Float -> Stamp -> ST s ()
paint m stamp = case stamp of
  Capsule a b wd c -> fill (bounds [a, b] (wd / 2)) (wd / 2) (\p -> segDist p a b) c
  Dot p r c -> fill (bounds [p] r) r (\q -> dist2 q p) c
  where
    bounds ps r =
      ( cellOf (foldr min 1e9 (map fst ps) - r - cell), cellOf (foldr max (-1e9) (map fst ps) + r + cell)
      , cellOf (foldr min 1e9 (map snd ps) - r - cell), cellOf (foldr max (-1e9) (map snd ps) + r + cell) )
    cellOf v = floor (v / cell) :: Int
    -- マスより細い線でも切れないように、形がマスを覆う割合で薄めて塗る
    fill (i0, i1, j0, j1) r distTo c =
      let (cr, cg, cb, a) = rgbaOfColor c
      in forM_ [max 0 j0 .. min (gh - 1) j1] (\j ->
           forM_ [max 0 i0 .. min (gw - 1) i1] (\i -> do
             let p = ((fromIntegral i + 0.5) * cell, (fromIntegral j + 0.5) * cell)
                 cover = clampF 0 1 ((r - distTo p) / cell + 0.5)
                 k = (j * gw + i) * 3
                 mix o v = MU.unsafeModify m (\old -> old + (v - old) * a * cover) o
             if cover > 0
               then mix k cr >> mix (k + 1) cg >> mix (k + 2) cb
               else pure ()))

-- 点と線分の距離
segDist :: Point -> Point -> Point -> Float
segDist (px, py) (ax, ay) (bx, by) =
  let ex = bx - ax
      ey = by - ay
      t = clampF 0 1 (((px - ax) * ex + (py - ay) * ey) / max 1e-6 (ex * ex + ey * ey))
  in norm2 (px - ax - ex * t, py - ay - ey * t)

drawAll :: Env -> World -> Picture
drawAll env w =
  let layer = wLayer w
      q :: Float -> Word8
      q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))
      pixel i j =
        let k = (j * gw + i) * 3
        in (q (U.unsafeIndex layer k), q (U.unsafeIndex layer (k + 1)), q (U.unsafeIndex layer (k + 2)))
      n = wN w
      cx = canvasW / 2
      cy = canvasH / 2
      ray a = Line [(cx, cy), (cx + 2000 * cos a, cy + 2000 * sin a)]
      wedges = if not (wWedge w) then [] else concat
        [ Color (hsbA 0 0 100 25) (ray a)
          : [ Color (hsbA 0 0 100 10) (ray (a + pi / fromIntegral n)) | wMirror w ]
        | k <- [0 .. n - 1], let a = tau * fromIntegral k / fromIntegral n ]
      marks = [ Color (hsbA 0 0 100 50) (at p (disc 2.5)) | p <- images w (mouseCanvas env) ]
      brushName = case wBrush w of { 1 -> "line"; 2 -> "grains"; _ -> "glow" }
  in Pictures
    [ pixelImage gw gh cell pixel
    , Translate (negate canvasW / 2) (canvasH / 2) (Scale 1 (-1) (Pictures (wedges ++ marks)))
    , txt env 20 40 18 white
        ("Mirrors " ++ show n ++ " (Up/Down)  Fold " ++ onOff (wMirror w) ++ " (M)  Brush " ++ brushName
         ++ " (1-3)  W: mirror lines  C: clear  S: save")
    , txt env 20 68 15 (hsb 0 0 70)
        ("Drag to draw. One stroke becomes " ++ show (if wMirror w then 2 * n else n)
         ++ ". Fast strokes are thin, slow strokes are thick.")
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wN = min 24 (wN w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wN = max 1 (wN w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "mM" -> pure w { wMirror = not (wMirror w) }
    | c `elem` "123" -> pure w { wBrush = fromEnum c - 48 }
    | c `elem` "wW" -> pure w { wWedge = not (wWedge w) }
    | c `elem` "cC" -> pure w { wLayer = blankLayer }
    | c `elem` "sS" -> saveSvg "kaleidoscope" env (drawAll env w) >> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. N = 1 で折り返し ON にすると、ただの左右対称。N = 2 で上下左右。本物の万華鏡は鏡 3 枚（N = 3）
-- 2. 鏡の線（W）を出して、1 つの扇形の中だけに描く。他の扇形は「鏡の中」
-- 3. 中心からの距離 r を「r の 2 乗」や「sin(r)」に変えて写すと、鏡が曲がった万華鏡になる
-- 4. 色を時間でなく「中心からの距離」で決めると、同心円の虹になる。自由制作のベースに


-- ==================================================
-- ここから下は「道具」（座標の変換・色・数字）
-- ==================================================
-- 絵の座標（左上が原点、y は下向き）でのマウス
mouseCanvas :: Env -> Point
mouseCanvas env = let (x, y) = envMouse env in (x + canvasW / 2, canvasH / 2 - y)

-- p5.js の text(s, x, y) と同じ位置に文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

-- 透明度を 0〜100 で書く HSB（p5.js と同じ）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

onOff :: Bool -> String
onOff b = if b then "ON" else "off"
