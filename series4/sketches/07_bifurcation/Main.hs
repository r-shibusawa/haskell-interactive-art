-- ============================================
-- 07 分岐図（カオスへの地図）
-- 横軸 r、縦軸「落ち着いた先の値」。00 の実験を全部の r について一度に描く
-- 1本 → 2本 → 4本 → … → カオス。ズームすると同じ形がまた現れる（フラクタル！）
-- ドラッグで範囲を選んでズーム / R でリセット / S で保存
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Numeric (showFFloat)

-- ズームを深くしても計算が荒れないよう、範囲と x は Double で持つ
data World = World
  { wRMin, wRMax, wXMin, wXMax :: !Double
  , wDrag :: !(Maybe Point)      -- ドラッグを始めた位置（p5.js 風の座標）
  , wCols :: !Int                -- 何列まで描いたか（少しずつ描く）
  , wBuf  :: !(U.Vector Float)   -- 点を打ちためる画像（1 ピクセルに R, G, B の 3 つ）
  , wImg  :: Picture             -- 画像の絵（列が増えたときだけ作り直す）
  }

colsPerFrame :: Int
colsPerFrame = 12                -- ★ 1フレームに描く列の数

settle, record :: Int
settle = 300                     -- ★ 落ち着くまで待つ回数
record = 200                     -- ★ 点を打つ回数

bufW, bufH :: Int
bufW = 360                       -- 画像の大きさ（360 × 220 = 79200 画素）
bufH = 220

topBar :: Float
topBar = 50                      -- 画面の上の文字のための余白

pointAlpha :: Float
pointAlpha = 0.09                -- ★ 点 1 つの濃さ

backColor :: Color
backColor = hsb 240 40 8

main :: IO ()
main = runSketch (sketch "04-07 Bifurcation Diagram" (restart (World 2.5 4.0 0 1 Nothing 0 U.empty Blank)))
  { skBack  = backColor
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

restart :: World -> World
restart w = withImage w { wCols = 0, wBuf = blankBuf }

blankBuf :: U.Vector Float
blankBuf = U.generate (3 * bufW * bufH) (\i -> case i `mod` 3 of { 0 -> br; 1 -> bg; _ -> bb })
  where (br, bg, bb, _) = rgbaOfColor backColor

withImage :: World -> World
withImage w = w { wImg = pixelImage bufW bufH 1 pixel }
  where
    buf = wBuf w
    pixel x y =
      let i = 3 * (y * bufW + x)
      in (byte (U.unsafeIndex buf i), byte (U.unsafeIndex buf (i + 1)), byte (U.unsafeIndex buf (i + 2)))
    byte v = fromIntegral (truncate (v * 255 + 0.5) :: Int)

-- 1 フレームぶんの列を描く。列の数え方は画面の横幅（p5.js と同じ）で、
-- 点は小さな画像にまとめて打つ
step :: Env -> World -> World
step env w
  | wCols w >= width = w
  | otherwise = withImage w { wCols = wCols w + length cols, wBuf = buf }
  where
    width = round (envW env) :: Int
    cols = take colsPerFrame [wCols w .. width - 1]
    buf = runST $ do
      mv <- U.thaw (wBuf w)
      forM_ cols (drawColumn env w mv)
      U.unsafeFreeze mv

drawColumn :: Env -> World -> M.MVector s Float -> Int -> ST s ()
drawColumn env w mv col = do
  let r = lerpD (wRMin w) (wRMax w) (fromIntegral col / realToFrac (envW env))
      hue = 180 + 140 * toF ((r - wRMin w) / (wRMax w - wRMin w))
      (cr, cg, cb, _) = rgbaOfColor (hsb hue 60 100)
      bx = min (bufW - 1) (floor (fromIntegral col / envW env * fromIntegral bufW))
      plot x = do
        -- 落ち着いた先を点で打つ（x の範囲の外は打たない）
        let t = (x - wXMin w) / (wXMax w - wXMin w)      -- 0 が下、1 が上
            by = floor ((1 - t) * fromIntegral bufH) :: Int
        when (t >= 0 && t <= 1) $ do
          let i = 3 * (min (bufH - 1) by * bufW + bx)
          blend mv i cr
          blend mv (i + 1) cg
          blend mv (i + 2) cb
      go (0 :: Int) _ = pure ()
      go n x = do
        let x' = r * x * (1 - x)
        plot x'
        go (n - 1) x'
      waited = iterateN settle (\x -> r * x * (1 - x)) 0.5   -- 落ち着くまで待つ
  go record waited

blend :: M.MVector s Float -> Int -> Float -> ST s ()
blend mv k v = M.modify mv (\old -> old + (v - old) * pointAlpha) k

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Translate 0 (-topBar / 2)
      (Scale (envW env / fromIntegral bufW) ((envH env - topBar) / fromIntegral bufH) (wImg w))
  , dragBox
  , txt env 20 32 18 white
      ("r: " ++ fmt 4 (wRMin w) ++ " - " ++ fmt 4 (wRMax w) ++ "   x: " ++ fmt 3 (wXMin w) ++ " - " ++ fmt 3 (wXMax w)
        ++ "   Drag: zoom   R: reset   S: save")
  , txt env (min (mx + 10) (envW env - 130)) (envH env - 10) 14 (hsb 0 0 70)
      ("r = " ++ fmt 4 (lerpD (wRMin w) (wRMax w) (realToFrac (mx / envW env))))
  ]
  where
    (mx, my) = mouseP5 env
    -- ドラッグ中の範囲
    dragBox = case wDrag w of
      Nothing -> Blank
      Just (sx, sy) -> Color (hsb 40 90 100)
        (thickPath 2 [sc env sx sy, sc env mx sy, sc env mx my, sc env sx my, sc env sx sy])

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | my > topBar -> pure w { wDrag = Just (mx, my) }
  EventKey (MouseButton LeftButton) Up _ _ -> pure (finishDrag env w)
  EventKey (Char c) Down _ _
    | c == 'r' || c == 'R' -> pure (restart w { wRMin = 2.5, wRMax = 4.0, wXMin = 0, wXMax = 1 })
    | c == 's' || c == 'S' -> saveSvg "bifurcation" env (drawWorld env w) >> pure w
  _ -> pure w
  where (mx, my) = mouseP5 env

-- ドラッグを離したら、選んだ範囲にズームする
finishDrag :: Env -> World -> World
finishDrag env w = case wDrag w of
  Nothing -> w
  Just (sx, sy) ->
    let (mx, my) = mouseP5 env
        x1 = min sx mx; x2 = max sx mx
        y1 = min sy my; y2 = max sy my
        rAt x = lerpD (wRMin w) (wRMax w) (realToFrac (x / envW env))
        xAt y = lerpD (wXMin w) (wXMax w) (realToFrac ((envH env - y) / (envH env - topBar)))
    in if x2 - x1 > 10 && y2 - y1 > 10
         then restart w { wRMin = rAt x1, wRMax = rAt x2, wXMin = xAt y2, wXMax = xAt y1, wDrag = Nothing }
         else w { wDrag = Nothing }

-- ---------- やってみよう ----------
-- 1. 1本が2本に分かれる r（約 3.0）、2本が4本に分かれる r（約 3.449）を読み取ろう
-- 2. r ≈ 3.83 の「白い窓」をズーム。中に小さな分岐図がまるごと入っている
-- 3. 分岐の間隔がだんだん狭くなる比率は、どんな式でも約 4.669（ファイゲンバウム定数）

-- ---------- 道具 ----------

-- f を n 回くり返す
iterateN :: Int -> (Double -> Double) -> Double -> Double
iterateN n f !x
  | n <= 0 = x
  | otherwise = iterateN (n - 1) f (f x)

lerpD :: Double -> Double -> Double -> Double
lerpD a b t = a + (b - a) * t

toF :: Double -> Float
toF = realToFrac

-- p5.js 風の座標（左上が原点、y は下向き）を gloss の座標に直す
sc :: Env -> Float -> Float -> Point
sc env x y = (x - envW env / 2, envH env / 2 - y)

mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size col s = let (gx, gy) = sc env x y in label gx gy size col s

fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) v ""
