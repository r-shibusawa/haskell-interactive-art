-- ============================================
-- 01 逆行列：変換を「もとに戻す」行列
-- 左：2×2 行列 A は平面の変形。2 本の矢印（基底）をドラッグして A を決める。I で A⁻¹ をかけて元に戻す
-- 右：3×3 の [A | I] を掃き出して [I | A⁻¹] にする（LUP 分解を 1 回して右辺を n 回解くやり方でも、中身は同じ）
-- ドラッグ：矢印の先 / I：逆変換アニメ / スペース：掃き出し 1 ステップ / A：自動 / R：やり直し
-- 1：整数で割り切れる例  2：ランダム  3：特異行列（逆行列なし）
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (digitToInt, toLower)
import Numeric (showFFloat)

unitPx :: Float
unitPx = 60   -- ★ 左の方眼 1 マスのピクセル数

speed :: Int
speed = 40    -- ★ 自動再生の間隔（フレーム）

data Hi = HNone | HBad !Int | HRows !Int !Int | HRow !Int | HColumn !Int | HInv

-- 掃き出し法の状態
data S = S
  { sG    :: ![[Double]]
  , sHi   :: !Hi
  , sMsg  :: !String
  , sDone :: !Bool
  }

data World = World
  { wA, wB, wC, wD :: !Float      -- A = [[a, b], [c, d]]
  , wDrag   :: !(Maybe Char)
  , wBack   :: !Float
  , wBackT  :: !Float
  , wS      :: !S
  , wRest   :: [S]
  , wAuto   :: !Bool
  , wTimer  :: !Int
  , wPreset :: !Int
  }

main :: IO ()
main = runSketch (sketch "16-01 Matrix Inverse" (loadPure 1 pre1 start))
  { skBack  = bgC
  , skStep  = \_ _ w -> pure (stepW w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

start :: World
start = World 1.4 0.5 0.3 1.1 Nothing 0 0 (S [] HNone "" False) [] False 0 1

pre1, pre3 :: [[Double]]
pre1 = [[2, 1, 1], [1, 3, 2], [1, 0, 0]]
pre3 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

loadPure :: Int -> [[Double]] -> World -> World
loadPure k a w =
  let g = [ r ++ [if i == j then 1 else 0 | j <- [0 .. 2 :: Int]] | (i, r) <- zip [0 ..] a ]
      s0 = S g HNone "Space: sweep one step at a time" False
  in w { wPreset = k, wS = s0, wRest = steps gaussJordan s0 }

loadG :: Int -> World -> IO World
loadG k w = case k of
  1 -> pure (loadPure 1 pre1 w)
  3 -> pure (loadPure 3 pre3 w)
  _ -> do
    a <- replicateM 3 (replicateM 3 (fromIntegral <$> randomRIO (-5, 5 :: Int)))
    pure (loadPure 2 a w)

-- 掃き出し法：yield のたびに画面が 1 つ進む
gaussJordan :: Gen S ()
gaussJordan = go 0
  where
    go :: Int -> Gen S ()
    go k
      | k >= 3 = do
          modify (\s -> s { sHi = HInv, sDone = True
                          , sMsg = "The left side is now I, so the right half is A^-1. The same operations were applied to I, so A^-1 comes out." })
          yield
      | otherwise = do
          g <- gets sG
          let at2 i j = g !! i !! j
              p = foldl (\b i -> if abs (at2 i k) > abs (at2 b k) then i else b) k [k + 1 .. 2]
          if abs (at2 p k) < 1e-9
            then do
              modify (\s -> s { sHi = HBad k, sDone = True
                              , sMsg = "Column " ++ show (k + 1) ++ " is all 0: singular. There is no inverse (det = 0)." })
              yield
            else do
              if p /= k
                then do
                  modify (\s -> s { sG = swapL k p (sG s), sHi = HRows k p
                                  , sMsg = "Use the row with the largest value as pivot: swap rows " ++ show (k + 1) ++ " and " ++ show (p + 1) })
                  yield
                else pure ()
              g1 <- gets sG
              let pv = g1 !! k !! k
              modify (\s -> s { sG = [if i == k then map (/ pv) r else r | (i, r) <- zip [0 ..] (sG s)], sHi = HRow k
                              , sMsg = "Divide row " ++ show (k + 1) ++ " by " ++ fmt pv ++ " so the pivot becomes 1" })
              yield
              modify (\s -> s { sG = eliminate k (sG s), sHi = HColumn k
                              , sMsg = "Subtract multiples of row " ++ show (k + 1) ++ " from the other rows: column " ++ show (k + 1) ++ " becomes 0 except the pivot" })
              yield
              go (k + 1)

eliminate :: Int -> [[Double]] -> [[Double]]
eliminate k g =
  let top = g !! k
  in [ if i == k then r else let f = r !! k in zipWith (\v t -> v - f * t) r top | (i, r) <- zip [0 ..] g ]

swapL :: Int -> Int -> [a] -> [a]
swapL a b xs = [pick i x | (i, x) <- zip [0 ..] xs]
  where pick i x | i == a = xs !! b
                 | i == b = xs !! a
                 | otherwise = x

advance :: World -> World
advance w
  | sDone (wS w) = w
  | otherwise = case wRest w of
      [] -> w { wS = (wS w) { sDone = True } }
      (s : r) -> w { wS = s, wRest = r }

stepW :: World -> World
stepW w0 =
  let running = wAuto w0 && not (sDone (wS w0))
      timer = if running then wTimer w0 + 1 else wTimer w0
      w1 = w0 { wTimer = timer, wBack = lerpF (wBack w0) (wBackT w0) 0.06 }
  in if running && timer `mod` speed == 0 then advance w1 else w1

-- A の方眼の中心
center :: Env -> (Float, Float)
center env = (envW env * 0.25, envH env * 0.55)

drawW :: Env -> World -> Picture
drawW env w = Pictures
  [ txt env 30 50 26 (g8 235) "Matrix inverse: undoing a transformation"
  , txt env 30 80 15 (g8 150) ("Left: drag arrow tips  I: apply A^-1  |  Right: Space: 1 step  A: auto"
                                ++ (if wAuto w then " (on)" else "") ++ "  R: redo  1-3: samples")
  , drawPlane env w
  , drawGauss env w (envW env * 0.52) 140
  ]

drawPlane :: Env -> World -> Picture
drawPlane env w =
  let (cx, cy) = center env
      a = wA w
      b = wB w
      c = wC w
      d = wD w
      det = a * d - b * c
      bk = wBack w
      -- back = 0 なら A、back = 1 なら単位行列
      ma = lerpF a 1 bk
      mb = lerpF b 0 bk
      mc = lerpF c 0 bk
      md = lerpF d 1 bk
      tr u v = (cx + (ma * u + mb * v) * unitPx, cy - (mc * u + md * v) * unitPx)
      p2 (u, v) = pt env (tr u v)
      l k = k * unitPx
      gridC = rgb255 45 38 62
      grid = [ Pictures [ seg env gridC 1 (cx + l k, cy - l 5) (cx + l k, cy + l 5)
                        , seg env gridC 1 (cx - l 5, cy + l k) (cx + l 5, cy + l k) ] | k <- [-5 .. 5] ]
      warpC = rgba255 255 95 109 90
      warp = [ Pictures [ seg env warpC 1 (tr k (-4)) (tr k 4), seg env warpC 1 (tr (-4) k) (tr 4 k) ] | k <- [-4 .. 4] ]
      -- F の形は凹んでいるので、長方形 3 つに分けて塗る
      s8 (u, v) = p2 (u * 0.8, v * 0.8)
      quad (u0, v0, u1, v1) = Color (rgba255 78 225 190 150) (Polygon (map s8 [(u0, v0), (u1, v0), (u1, v1), (u0, v1)]))
      arrowTip col lbl x0 y0 =
        let s = 1 - bk
            x1 = x0 * s + (if bk > 0.02 && lbl == "i" then bk * unitPx else 0)
            y1 = y0 * s + (if bk > 0.02 && lbl == "j" then negate (bk * unitPx) else 0)
        in Pictures [ seg env col 4 (cx, cy) (cx + x1, cy + y1)
                    , dot env col (cx + x1) (cy + y1) 20
                    , txtC env (cx + x1) (cy + y1) 14 bgC lbl ]
      ox = 30
      oy = envH env - 196
      pw = envW env * 0.45
      detBad = abs det < 0.05
      da = realToFrac d / realToFrac det :: Double
      db = realToFrac (negate b) / realToFrac det :: Double
      dc = realToFrac (negate c) / realToFrac det :: Double
      dd = realToFrac a / realToFrac det :: Double
      fm v = fmt (realToFrac v)
  in Pictures
       [ Pictures grid
       , Pictures warp
       , Color (rgba255 255 200 90 70) (Polygon (map p2 [(0, 0), (1, 0), (1, 1), (0, 1)]))
       , Pictures (map quad [(0, 0, 0.7, 3), (0.7, 2.4, 2, 3), (0.7, 1.2, 1.6, 1.8)])
       , Color mint (thickPath 2 (map s8 (shapeF ++ take 1 shapeF)))
       , arrowTip coral "i" (a * unitPx) (negate c * unitPx)
       , arrowTip amber "j" (b * unitPx) (negate d * unitPx)
       , box env panelC ox oy pw 186
       , txtT env (ox + 16) (oy + 10) 16 (g8 235) ("A = [ " ++ fm a ++ "  " ++ fm b ++ " ; " ++ fm c ++ "  " ++ fm d ++ " ]")
       , txtT env (ox + 16) (oy + 32) 16 (g8 235) ("det A = ad - bc = " ++ fm det)
       , if detBad
           then para env (ox + 16) (oy + 58) 16 (rgb255 255 90 90) (pw - 30) "det is near 0: the plane is squashed onto a line, so it cannot be undone (no inverse)"
           else Pictures
             [ txtT env (ox + 16) (oy + 58) 16 mint "A^-1 = (1/det) [ d  -b ; -c  a ]"
             , txtT env (ox + 16) (oy + 80) 16 mint ("     = [ " ++ fmt da ++ "  " ++ fmt db ++ " ; " ++ fmt dc ++ "  " ++ fmt dd ++ " ]")
             ]
       , para env (ox + 16) (oy + 108) 14 (g8 150) (pw - 30)
           "Yellow area = |det A|. A mirrored F: det < 0.\nI applies A^-1: the grid and F go back."
       ]

drawGauss :: Env -> World -> Float -> Float -> Picture
drawGauss env w ox oy =
  let s = wS w
      g = sG s
      hi = sHi s
      cs = min 70 ((envW env - ox - 40) / 6.6)
      cellPic i j v =
        let px = ox + fromIntegral j * cs + (if j >= 3 then cs * 0.5 else 0)
            py = oy + fromIntegral i * cs
            base = if j < 3 then cellC else rgb255 30 60 58
            col = case hi of
              HRow r | r == i -> rgba255 255 200 90 180
              HColumn k | j == k -> rgba255 255 95 109 160
              HRows p q | i == p || i == q -> rgb255 120 100 200
              HInv | j >= 3 -> rgba255 78 225 190 200
              HBad k | j == k && i >= k -> rgb255 200 50 50
              _ -> base
        in Pictures [ box env col (px + 1.5) (py + 1.5) (cs - 3) (cs - 3)
                    , txtC env (px + cs / 2) (py + cs / 2) (max 14 (cs * 0.27)) (g8 240) (fmt v) ]
      my = oy + 3 * cs + 30
      mw = envW env - ox - 30
  in Pictures
       [ txt env ox (oy - 14) 18 (g8 200) "[ A | I ]  ->  [ I | A^-1 ]  (sweep out)"
       , Pictures [cellPic i j v | (i, r) <- zip [0 :: Int ..] g, (j, v) <- zip [0 :: Int ..] r]
       , seg env (g8 200) 2 (ox + 3.25 * cs, oy - 4) (ox + 3.25 * cs, oy + 3 * cs + 4)
       , box env panelC ox my mw 90
       , para env (ox + 14) (my + 12) 16 (if sDone s then mint else g8 235) (mw - 28) (sMsg s)
       , para env ox (my + 110) 14 (g8 150) (mw - 10)
           "Column i of A^-1 solves Ax = e_i. You could also factor with LUP once, then solve for e1, e2, ... (Theta(n^3) in total).\nInverting costs the same as matrix multiplication: if one gets faster, so does the other.\nTo actually solve Ax = b, LUP directly is faster and more accurate than building the inverse."
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (cx, cy) = center env
        m = mouseP env
        near p = dist2 m p < 20
        hit | near (cx + wA w * unitPx, cy - wC w * unitPx) = Just 'i'
            | near (cx + wB w * unitPx, cy - wD w * unitPx) = Just 'j'
            | otherwise = Nothing
    in pure $ case hit of
         Just _ -> w { wDrag = hit, wBack = 0, wBackT = 0 }
         Nothing -> w
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = Nothing }
  EventMotion _ -> pure $ case wDrag w of
    Nothing -> w
    Just which ->
      let (cx, cy) = center env
          (mx, my) = mouseP env
          r10 v = fromIntegral (round (v * 10) :: Int) / 10
          u = r10 ((mx - cx) / unitPx)
          v = r10 (negate (my - cy) / unitPx)
      in if which == 'i' then w { wA = u, wC = v } else w { wB = u, wD = v }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (advance w)
  EventKey (Char c0) Down _ _ -> case toLower c0 of
    'i' | abs (wA w * wD w - wB w * wC w) > 0.05 -> pure w { wBackT = if wBackT w /= 0 then 0 else 1 }
    'a' -> pure w { wAuto = not (wAuto w) }
    'r' -> loadG (wPreset w) w
    c | c `elem` "123" -> loadG (digitToInt c) w
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 矢印 i と j を同じ向きにそろえると？（det = 0、F が線につぶれる）
-- 2. i と j を入れかえると F は鏡文字になる。det の符号を見る
-- 3. 右の 1 で、A⁻¹ が整数になる理由を det から考える（det = −1）

-- ---------- 道具 ----------
-- F の形の頂点
shapeF :: [(Float, Float)]
shapeF = [(0, 0), (0, 3), (2, 3), (2, 2.4), (0.7, 2.4), (0.7, 1.8), (1.6, 1.8), (1.6, 1.2), (0.7, 1.2), (0.7, 0)]

fmt :: Double -> String
fmt v
  | abs (v - fromIntegral (floor (v + 0.5) :: Int)) < 1e-9 = show (floor (v + 0.5) :: Int)
  | otherwise = showFFloat (Just 2) v ""

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> (Float, Float) -> (Float, Float)
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> (Float, Float) -> (Float, Float) -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 長方形の枠線（太さ t）
frame :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
frame env c t x y w h = Pictures
  [ seg env c t (x, y) (x + w, y), seg env c t (x + w, y) (x + w, y + h)
  , seg env c t (x + w, y + h) (x, y + h), seg env c t (x, y + h) (x, y) ]

-- 直径 d の円（p5.js の circle）
dot :: Env -> Color -> Float -> Float -> Float -> Picture
dot env c x y d = Color c (at (pt env (x, y)) (disc (d / 2)))

-- 直径 d の円の輪郭（太さ t）
hoop :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
hoop env c t x y d = Color c (at (pt env (x, y)) (ring (d / 2) t))

-- 文字：txt は左下が (x, y)、txtT は左上が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtT, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtT env x y s c str = txt env x (y + s) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = txt env (x - fromIntegral (length str) * s * 0.78) y s c str

-- ---------- 道具：文字の折り返し（文字の幅はおよそ 0.85 × 大きさ） ----------
wrapText :: Float -> Float -> String -> [String]
wrapText s w = concatMap one . lines
  where
    limit = max 8 (floor (w / (s * 0.85))) :: Int
    one l = case words l of
      [] -> [""]
      (x : xs) -> go x xs
    go cur [] = [cur]
    go cur (x : xs)
      | length cur + 1 + length x <= limit = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs

lineH :: Float -> Float
lineH s = s * 1.35

-- 左上 (x, y) から幅 w で折り返して書く
para :: Env -> Float -> Float -> Float -> Color -> Float -> String -> Picture
para env x y s c w str = Pictures
  [ txtT env x (y + fromIntegral i * lineH s) s c l | (i, l) <- zip [0 :: Int ..] (wrapText s w str) ]

-- 折り返したときの高さ
paraH :: Float -> Float -> String -> Float
paraH s w str = fromIntegral (length (wrapText s w str)) * lineH s

-- 上から順に積む：(大きさ, 色, 下のすきま, 文字)
stack :: Env -> Float -> Float -> Float -> [(Float, Color, Float, String)] -> Picture
stack env x y0 w = Pictures . go y0
  where
    go _ [] = []
    go y ((s, c, gap, str) : rest) = para env x y s c w str : go (y + paraH s w str + gap) rest

-- 画面の下の端にそろえて書く
footer :: Env -> Float -> Float -> Float -> Color -> String -> Picture
footer env x w s c str = para env x (envH env - 14 - paraH s w str) s c w str

-- ---------- 道具：色 ----------
bgC, panelC, plotC, cellC, mint, coral, amber :: Color
bgC    = rgb255 18 14 26
panelC = rgb255 32 26 44
plotC  = rgb255 28 22 38
cellC  = rgb255 40 34 56
mint   = rgb255 78 225 190
coral  = rgb255 255 95 109
amber  = rgb255 255 200 90

-- 0〜255 の灰色
g8 :: Int -> Color
g8 v = rgb255 v v v

-- ---------- 道具：1 手ずつ止まる計算（p5.js の function* の代わり） ----------
-- yield のたびに状態を 1 つ流す計算。結果は「状態の遅延リスト」になり、
-- 画面側はリストを 1 つずつ取り出して進める。
newtype Gen s a = Gen { unGen :: s -> (a -> s -> [s]) -> [s] }

instance Functor (Gen s) where
  fmap f (Gen g) = Gen (\s k -> g s (k . f))

instance Applicative (Gen s) where
  pure x = Gen (\s k -> k x s)
  Gen f <*> Gen g = Gen (\s k -> f s (\h s' -> g s' (k . h)))

instance Monad (Gen s) where
  Gen g >>= f = Gen (\s k -> g s (\x s' -> unGen (f x) s' k))

yield :: Gen s ()
yield = Gen (\s k -> s : k () s)

gets :: (s -> a) -> Gen s a
gets f = Gen (\s k -> k (f s) s)

modify :: (s -> s) -> Gen s ()
modify f = Gen (\s k -> k () $! f s)

steps :: Gen s () -> s -> [s]
steps (Gen g) s0 = g s0 (\_ _ -> [])
