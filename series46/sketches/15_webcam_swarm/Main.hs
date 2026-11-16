-- ============================================
-- 46-15 カメラの光に集まる群れ 📷（おまけ）
-- カメラ映像の「明るい所」または「動いた所」をエサの場にして、800 匹の群れがエサの濃い方へ集まり、通った跡で絵を描く
-- 1 匹 1 匹はエサの坂（勾配）を登るのと、近すぎる仲間から離れるだけ。カメラが使えないときはマウスの周りがエサになる
-- 操作：1 = 明るい所 / 2 = 動いた所 / ↑↓ = 群れの数 / C = 絵を消す / F = 跡を少しずつ消す / S = 画像を保存（SVG）
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.Bits (shiftL, shiftR, xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word (Word64, Word8)
import Numeric (showFFloat)

vw, vh :: Int
vw = 160                     -- カメラ（エサの場）の解像度
vh = 120

climb, sepR, maxSp, jitter :: Float
climb = 2.2                  -- ★ エサの坂を登る強さ
sepR = 7                     -- ★ この距離より近い仲間からは離れる（px）
maxSp = 3.0                  -- ★ 速さの上限
jitter = 0.35                -- ★ ふらつき

cellSize :: Float
cellSize = 14                -- 空間ハッシュのマス（px）

artW, artH :: Int
artW = 320                   -- 絵（跡）を描きためる画像の大きさ。画面の 1/4 の解像度
artH = 200

data World = World
  { wCam      :: !(Maybe Camera)
  , wFrame    :: !B.ByteString
  , wUsingCam :: !Bool
  , wFood     :: !(U.Vector Float)     -- エサの場（vw × vh、0〜1）
  , wPrevGray :: !(U.Vector Float)     -- 前のフレームの明るさ（動きを測る）
  , wN        :: !Int                  -- 群れの数
  , wAx, wAy  :: !(U.Vector Float)     -- 位置（px、左上が原点）
  , wVx, wVy  :: !(U.Vector Float)     -- 速度
  , wMode     :: !Int                  -- 1 = 明るさ、2 = 動き
  , wFading   :: !Bool
  , wArt      :: !(U.Vector Float)     -- 絵（artW × artH × RGB、0〜1）
  , wW, wH    :: !Float                -- 画面の大きさ（群れを作ったときの値）
  , wSeed     :: !Seed
  }

world0 :: World
world0 = World Nothing B.empty False (U.replicate (vw * vh) 0) U.empty 800 U.empty U.empty U.empty U.empty 1 False blankArt 1280 800 1

main :: IO ()
main = runSketch (sketch "46-15 Webcam swarm" world0)
  { skBack  = paperC
  , skStart = \env -> do
      cam <- startCamera vw vh
      s <- randomRIO (1, 2 ^ (62 :: Int))
      pure (makeAgents env world0 { wCam = Just cam, wSeed = s })
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- 紙の色（p5.js の art.background(260, 50, 8)）
paperC :: Color
paperC = hsb 260 50 8

blankArt :: U.Vector Float
blankArt = let (r, g, b, _) = rgbaOfColor paperC in U.generate (artW * artH * 3) (\i -> [r, g, b] !! (i `mod` 3))

makeAgents :: Env -> World -> World
makeAgents env w = w { wAx = U.generate n (\i -> hashRand s (2 * i) * sw), wAy = U.generate n (\i -> hashRand s (2 * i + 1) * sh)
                     , wVx = U.replicate n 0, wVy = U.replicate n 0, wW = sw, wH = sh, wSeed = nextSeed s }
  where
    n = wN w
    s = wSeed w
    sw = envW env
    sh = envH env

-- (x, y)（画面の px）のエサの濃さ
foodAt :: World -> Float -> Float -> Float
foodAt w x y =
  let cx = max 0 (min (vw - 1) (floor (x / wW w * fromIntegral vw)))
      cy = max 0 (min (vh - 1) (floor (y / wH w * fromIntegral vh)))
  in wFood w U.! (cy * vw + cx)

-- ★ エサの場を作る：カメラの明るさ（1）か、前のフレームとの差＝動き（2）。カメラがなければマウスの周り
updateFood :: Env -> World -> World
updateFood env w
  | wUsingCam w =
      let f = wFrame w
          gray = U.generate (vw * vh) (\i -> let g o = fromIntegral (BU.unsafeIndex f (i * 3 + o)) :: Float
                                             in (g 0 + g 1 + g 2) / 765)
          prev = wPrevGray w
          food
            | wMode w == 1 = U.map (^ (3 :: Int)) gray                                  -- 明るい所ほど強く
            | otherwise = U.generate (vw * vh) (\i -> max (wFood w U.! i * 0.9)          -- 動いた所（少し残る）
                            (if U.length prev == vw * vh then min (abs (gray U.! i - prev U.! i) * 5) 1 else 0))
      in w { wFood = blurFood food, wPrevGray = gray }
  | otherwise =
      let (mx0, my0) = envMouse env
          mx = (mx0 + envW env / 2) / envW env * fromIntegral vw
          my = (envH env / 2 - my0) / envH env * fromIntegral vh
          food = U.generate (vw * vh) (\i -> let (y, x) = i `divMod` vw
                                                 dx = fromIntegral x - mx
                                                 dy = fromIntegral y - my
                                             in exp (-(dx * dx + dy * dy) / 120))
      in w { wFood = blurFood food }

-- ぼかして、遠くからでもエサの方向がわかるようにする（横 7 マス → 縦 7 マスの平均を 2 回）
blurFood :: U.Vector Float -> U.Vector Float
blurFood f0 = if fmax > 0.02 then U.map (/ fmax) f2 else f2   -- いちばん濃い所が 1 になるようにそろえる
  where
    sum7 g = g (-3) + g (-2) + g (-1) + g 0 + g 1 + g 2 + g 3
    hpass f = U.generate (vw * vh) (\i -> let (y, x) = i `divMod` vw
                                          in sum7 (\d -> U.unsafeIndex f (y * vw + max 0 (min (vw - 1) (x + d)))) / 7)
    vpass f = U.generate (vw * vh) (\i -> let (y, x) = i `divMod` vw
                                          in sum7 (\d -> U.unsafeIndex f (max 0 (min (vh - 1) (y + d)) * vw + x)) / 7)
    once = vpass . hpass
    f2 = once (once f0)
    fmax = U.maximum (U.cons 0 f2)

-- ★ 1 匹ずつ：エサの坂を登る ＋ 近すぎる仲間から離れる ＋ ふらつき
moveAgents :: World -> World
moveAgents w = w { wAx = ax', wAy = ay', wVx = U.map fst vs, wVy = U.map snd vs, wSeed = nextSeed s }
  where
    s = wSeed w
    ax = wAx w
    ay = wAy w
    n = U.length ax
    cols = ceiling (wW w / cellSize) + 1
    rows = ceiling (wH w / cellSize) + 1
    cellXY i = (floor (ax U.! i / cellSize), floor (ay U.! i / cellSize)) :: (Int, Int)
    grid = buildGrid (cols * rows) (U.generate n (\i -> let (cx, cy) = cellXY i in cx + cy * cols))
    gs = wW w / fromIntegral vw
    newV i =
      let x = ax U.! i
          y = ay U.! i
          -- エサの坂（となりのマスとの差）
          gx = (foodAt w (x + gs) y - foodAt w (x - gs) y) * 20
          gy = (foodAt w x (y + gs) - foodAt w x (y - gs)) * 20
          (cx, cy) = cellXY i
          cells = [ c | dy <- [-1, 0, 1], dx <- [-1, 0, 1], let c = cx + dx + (cy + dy) * cols, c >= 0, c < cols * rows ]
          push (fx, fy) j
            | j == i = (fx, fy)
            | d > 0.01 && d < sepR = (fx + ex / d * (1 - d / sepR) * 1.5, fy + ey / d * (1 - d / sepR) * 1.5)
            | otherwise = (fx, fy)
            where
              ex = x - ax U.! j
              ey = y - ay U.! j
              d = sqrt (ex * ex + ey * ey)
          (fx, fy) = foldl' (\acc c -> U.foldl' push acc (cellMembers grid c)) (clampF (-1) 1 gx * climb, clampF (-1) 1 gy * climb) cells
          vx1 = (wVx w U.! i + fx * 0.3 + (hashRand s (2 * i) - 0.5) * jitter) * 0.94
          vy1 = (wVy w U.! i + fy * 0.3 + (hashRand s (2 * i + 1) - 0.5) * jitter) * 0.94
          sp = sqrt (vx1 * vx1 + vy1 * vy1)
      in if sp > maxSp then (vx1 * maxSp / sp, vy1 * maxSp / sp) else (vx1, vy1)
    vs = U.generate n newV
    ax' = U.imap (\i x -> clampF 0 (wW w - 1) (x + fst (vs U.! i))) ax
    ay' = U.imap (\i y -> clampF 0 (wH w - 1) (y + snd (vs U.! i))) ay

-- ---- 通った跡を絵に描く（色は時間とともに変わる）。前の位置 → 今の位置を 3 点で塗り重ねる
paint :: Int -> World -> World -> World
paint frame old w = w { wArt = U.modify stamp faded }
  where
    faded = if wFading w then U.zipWith (\v b -> v + (b - v) * 0.04) (wArt w) blankArt else wArt w   -- 紙の色を薄く重ねる
    (cr, cg, cb, _) = rgbaOfColor (hsb (wrapF 360 (fromIntegral frame * 0.4)) 60 100)
    kx = fromIntegral artW / wW w
    ky = fromIntegral artH / wH w
    sameLen = U.length (wAx old) == U.length (wAx w)
    stamp buf = forM_ [0 .. U.length (wAx w) - 1] $ \i -> do
      let x1 = wAx w U.! i
          y1 = wAy w U.! i
          (x0, y0) = if sameLen then (wAx old U.! i, wAy old U.! i) else (x1, y1)
      forM_ [0, 0.5, 1 :: Float] $ \t -> do
        let px = floor (lerpF x0 x1 t * kx) :: Int
            py = floor (lerpF y0 y1 t * ky) :: Int
        when (px >= 0 && px < artW && py >= 0 && py < artH) $ do
          let o = (py * artW + px) * 3
              mix k c = M.modify buf (\v -> v + (c - v) * 0.25) (o + k)
          mix 0 cr
          mix 1 cg
          mix 2 cb

step :: Env -> Float -> World -> IO World
step env _ w = do
  live <- maybe (pure False) camLive (wCam w)
  frame <- case wCam w of
    Just c | live -> cameraFrame c
    _ -> pure B.empty
  let usingCam = live && B.length frame >= vw * vh * 3
      w1 = updateFood env w { wFrame = frame, wUsingCam = usingCam }
      w2 = moveAgents w1
  pure (paint (envFrame env) w1 w2)

draw :: Env -> World -> Picture
draw env w = Pictures
  [ Scale (envW env / fromIntegral artW) (envH env / fromIntegral artH)
      (pixelImage artW artH 1 (\x y -> let o = (y * artW + x) * 3 in (byte (U.unsafeIndex a o), byte (U.unsafeIndex a (o + 1)), byte (U.unsafeIndex a (o + 2)))))
  , view env $
      -- ---- 群れの点
      [ Color (hsbA 0 0 100 80) (Pictures [ Translate x y (disc 1) | (x, y) <- zip (U.toList (wAx w)) (U.toList (wAy w)) ]) ]
      -- ---- 右上：カメラとエサの場
      ++ [ if wUsingCam w then Blank else Color (hsb 0 0 15) (rect (px0 + pw / 2) (py0 + ph / 2) pw ph) ]
  , case wCam w of
      Just c | wUsingCam w -> Translate (px0 + pw / 2 - envW env / 2) (envH env / 2 - py0 - ph / 2)
                                        (camImage c (wFrame w) (pw / fromIntegral vw))
      _ -> Blank
  , view env $
      [ Color (hsbA 50 90 100 (min (f * 120) 80)) (rect (px0 + fromIntegral x / fromIntegral vw * pw + 3.5) (py0 + fromIntegral y / fromIntegral vh * ph + 3.5) 7 7)
      | y <- [0, 6 .. vh - 1], x <- [0, 6 .. vw - 1], let f = wFood w U.! (y * vw + x), f >= 0.05 ]
      ++ [ Color (hsbA 0 0 0 55) (rect (10 + 1040 / 2) (10 + 62 / 2) 1040 62)
         , txt 20 34 16 (hsb 0 0 100) ("1: bright  2: motion (now: " ++ (if wMode w == 1 then "bright" else "motion") ++ ")   Up/Down: "
                                       ++ show (wN w) ++ " agents   C: clear   F: fade " ++ (if wFading w then "ON" else "off") ++ "   S: save")
         , txt 20 60 14 (hsb 0 0 75) ((if wUsingCam w then "Camera on" else "No camera -> food around the mouse")
                                      ++ ".  On food: " ++ fmt 0 (fromIntegral near / fromIntegral (max 1 nq) * 100) ++ " %  (yellow = food field)") ]
  ]
  where
    a = wArt w
    pw = 200
    ph = 150
    px0 = envW env - pw - 16
    py0 = 16
    idx = [0, 4 .. U.length (wAx w) - 1]
    nq = length idx
    near = length [ () | i <- idx, foodAt w (wAx w U.! i) (wAy w U.! i) > 0.1 ]

-- 0〜1 の明るさを 0〜255 のバイトに
byte :: Float -> Word8
byte v = fromIntegral (truncate (clampF 0 1 v * 255 + 0.5) :: Int)

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (Char '1') Down _ _ -> pure w { wMode = 1 }
  EventKey (Char '2') Down _ _ -> pure w { wMode = 2, wFood = U.replicate (vw * vh) 0 }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (makeAgents env w { wN = min 2000 (wN w + 200) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (makeAgents env w { wN = max 200 (wN w - 200) })
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> pure w { wArt = blankArt }
    | c `elem` "fF" -> pure w { wFading = not (wFading w) }
    | c `elem` "sS" -> saveSvg "swarm_painting" env (draw env w) >> pure w
  EventResize _ -> pure (makeAgents env w { wArt = blankArt })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 部屋を暗くして、スマホのライトをカメラに向けて動かそう。群れが光を追いかけて線を描く
-- 2. 2（動き）にして手を振ると、振った所に群れが集まる。止まるとエサが消えて群れは散る
-- 3. sepR を 0 にすると全員が 1 点に集まる。15 にすると広がって「面」を塗る。描ける絵がどう変わる？
-- 4. climb を 0.5 にすると、ふらつきが勝ってエサを見つけにくい。jitter とのバランスを探そう
-- 5. 群れは「エサの坂」しか知らないのに、映像の形をなぞる。これも 1 匹ずつの単純なルールから生まれる全体の形

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = if w <= 1 then Color c (Line ps) else Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""

-- グラフ：記録 hist（新しい順、300 個まで）を左上 (x, y)、幅 w、高さ h の枠に lo〜hi で描く
drawPlot :: [Float] -> Float -> Float -> Float -> Float -> Float -> Float -> Color -> String -> Picture
drawPlot hist x y w h lo hi c title = Pictures
  [ Color (hsbA 0 0 0 35) (rect (x + w / 2) (y + h / 2) w h)
  , txt x (y - 8) 14 (hsb 0 0 85) title
  , if length hist < 2 then Blank
    else path 2 c [ (x + fromIntegral i / 299 * w, y + h - clampF 0 1 ((v - lo) / (hi - lo)) * h)
                  | (i, v) <- zip [0 :: Int ..] (reverse hist) ] ]

-- 乱数（xorshift64）。種を World に入れておけば skStep は純粋なまま
type Seed = Word64

nextSeed :: Seed -> Seed
nextSeed s0 = let s1 = s0 `xor` (s0 `shiftL` 13)
                  s2 = s1 `xor` (s1 `shiftR` 7)
              in s2 `xor` (s2 `shiftL` 17)

-- 種 s と番号 i から決まる 0〜1 の乱数（たくさんの粒子に一度に配るとき用）
hashRand :: Seed -> Int -> Float
hashRand s i =
  let z0 = s + fromIntegral i * 0x9E3779B97F4A7C15
      z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
      z3 = z2 `xor` (z2 `shiftR` 31)
  in fromIntegral (z3 `shiftR` 40) / 16777216

-- 空間ハッシュ：粒子 i の入るマス cells!i から「マスごとの粒子の並び」を作る（数えて並べる）
data Grid = Grid !(U.Vector Int) !(U.Vector Int)   -- マス c の粒子は order[start[c] .. start[c+1]-1]

buildGrid :: Int -> U.Vector Int -> Grid
buildGrid nc cells = Grid starts order
  where
    counts = U.accumulate (+) (U.replicate nc 0) (U.map (\c -> (c, 1)) cells)
    starts = U.scanl (+) 0 counts
    order = U.create $ do
      pos <- U.thaw (U.take nc starts)
      out <- M.new (U.length cells)
      forM_ [0 .. U.length cells - 1] $ \i -> do
        let c = cells U.! i
        p <- M.read pos c
        M.write out p i
        M.write pos c (p + 1)
      pure out

-- 周期境界での差：−L/2〜L/2 に入れる（Math.round を使うより速い）
wrapD :: Float -> Float -> Float
wrapD l d
  | d > l / 2 = d - l
  | d < -l / 2 = d + l
  | otherwise = d

cellMembers :: Grid -> Int -> U.Vector Int
cellMembers (Grid starts order) c = U.slice (starts U.! c) (starts U.! (c + 1) - starts U.! c) order
