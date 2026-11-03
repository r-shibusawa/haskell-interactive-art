-- ============================================
-- 10 k 平均法（k-means クラスタリング）
-- 中心を k 個置き、2 つの手順をくり返すだけ
-- ① 割り当て：各点をいちばん近い中心の仲間にする
-- ② 更新：各中心を仲間の点の平均（重心）に動かす
-- 誤差（各点と中心の距離の 2 乗の合計）は必ず減り、いつか止まる。ただし初めの置き方しだい
-- 操作：クリック = 点をまく / 中心をドラッグ / スペース = 再生・停止 / → = 1 手順
--       ↑↓ = k を変える / I = ランダムに置き直す / P = k-means++ で置き直す
--       R = 新しいデータ / 1〜3 = お手本（かたまり・輪・細長い）
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM, replicateM)
import Data.List (foldl', minimumBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
import Numeric (showFFloat)

speed :: Int
speed = 40              -- ★ 何フレームに 1 手順

maxPoints :: Int
maxPoints = 1200        -- 点の数の上限（これ以上はまかない）

data World = World
  { wPts    :: ![Point]
  , wK      :: !Int       -- ★ クラスタの数
  , wCents  :: ![Point]   -- 中心
  , wShown  :: ![Point]   -- 表示用の中心（なめらかに動かす）
  , wLabels :: ![Int]     -- 各点の仲間（-1 = まだ）
  , wAssign :: !Bool      -- 次の手順が ① 割り当てか
  , wIter   :: !Int
  , wSse    :: ![Float]   -- 誤差の履歴（新しい順）
  , wDone   :: !Bool      -- 収束した
  , wAuto   :: !Bool
  , wDrag   :: !Int
  , wPreset :: !Int
  }

main :: IO ()
main = runSketch (sketch "17-10 k-means" initialWorld)
  { skBack  = hsb 255 40 10
  , skStart = \env -> makeData env 1 initialWorld
  , skStep  = \env _ w -> pure (stepW env w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialWorld :: World
initialWorld = World [] 4 [] [] [] True 0 [] False True (-1) 1

makeData :: Env -> Int -> World -> IO World
makeData env k w = do
  let bigW = envW env - 300
      hh = envH env
  ps <- case k of
    2 -> forM [0 .. 259 :: Int] $ \i -> do     -- 輪と真ん中：k-means が苦手な形
      a <- randomRIO (0, tau)
      g <- gaussian
      let r = if i < 200 then 220 + g * 12 else g * 30
      pure (bigW / 2 + cos a * r, hh * 0.57 + sin a * r * 0.8)
    3 -> concat <$> forM [0 .. 2 :: Int] (\c -> replicateM 80 $ do    -- 細長いかたまり
      u <- randomRIO (0, bigW * 0.6)
      g <- gaussian
      pure (bigW * 0.2 + u, hh * (0.35 + fromIntegral c * 0.2) + g * 14))
    _ -> concat <$> replicateM 4 (do
      cx <- randomRIO (bigW * 0.2, bigW * 0.8)
      cy <- randomRIO (hh * 0.3, hh * 0.85)
      replicateM 70 ((\a b -> (cx + a * 45, cy + b * 45)) <$> gaussian <*> gaussian))
  initCenters False w { wPreset = k, wPts = ps }

initCenters :: Bool -> World -> IO World
initCenters plusplus w
  | null pts = pure w { wCents = [], wShown = [] }
  | otherwise = do
      let pv = V.fromList pts
          anyPoint = (pv V.!) <$> randomRIO (0, V.length pv - 1)
      cents <- if not plusplus
        then replicateM (wK w) anyPoint
        else do
          -- ★ k-means++：すでにある中心から遠い点ほど選ばれやすい
          first <- anyPoint
          let grow cs
                | length cs >= wK w = pure (reverse cs)
                | otherwise = do
                    let ws = [ minimum [sq2 p c | c <- cs] | p <- pts ]
                    r <- randomRIO (0, sum ws)
                    grow ((pv V.! pick r ws 0) : cs)
          grow [first]
      pure w { wCents = cents, wShown = cents, wLabels = map (const (-1)) pts
             , wAssign = True, wIter = 0, wSse = [], wDone = False }
  where pts = wPts w

-- 重みに比例して番号を選ぶ
pick :: Float -> [Float] -> Int -> Int
pick _ [] i = i
pick _ [_] i = i
pick r (x : xs) i
  | r - x > 0 = pick (r - x) xs (i + 1)
  | otherwise = i

sq2 :: Point -> Point -> Float
sq2 (a, b) (c, d) = (a - c) * (a - c) + (b - d) * (b - d)

nearest :: [Point] -> Point -> Int
nearest cents p = fst (minimumBy (comparing snd) ((0, 1 / 0) : zip [0 ..] (map (sq2 p) cents)))

sse :: World -> Float
sse w =
  let cv = V.fromList (wCents w)
  in foldl' (\s (p, l) -> if l >= 0 then s + sq2 p (cv V.! l) else s) 0 (zip (wPts w) (wLabels w))

-- ★ 1 手順進める
lloydStep :: World -> World
lloydStep w
  | null (wPts w) || wDone w = w
  | wAssign w =
      let labels = map (nearest (wCents w)) (wPts w)
          changed = length (filter id (zipWith (/=) labels (wLabels w)))
          w1 = w { wLabels = labels }
      in w1 { wDone = changed == 0 && wIter w > 0
            , wSse = take 600 (sse w1 : wSse w), wAssign = False }
  | otherwise =
      let mean j old =
            let mine = [p | (p, l) <- zip (wPts w) (wLabels w), l == j]
                c = fromIntegral (length mine)
            in if null mine then old
               else (sum (map fst mine) / c, sum (map snd mine) / c)
      in w { wCents = zipWith mean [0 ..] (wCents w), wAssign = True, wIter = wIter w + 1 }

stepW :: Env -> World -> World
stepW env w0 =
  let w = if wAuto w0 && envFrame env `mod` speed == 0 then lloydStep w0 else w0
      ease (a, b) (c, d) = (lerpF a c 0.12, lerpF b d 0.12)
  in w { wShown = zipWith ease (wShown w) (wCents w) }

hueOf :: World -> Int -> Float
hueOf w j = fromIntegral j * 360 / fromIntegral (wK w) + 20

drawW :: Env -> World -> Picture
drawW env w =
  let ww = envW env
      hh = envH env
      shown = wShown w
      sv = V.fromList shown
      g = 18
      cols = V.fromList [hsba (hueOf w j) 60 40 0.22 | j <- [0 .. wK w - 1]]
      -- ボロノイ（どの中心に近いか）を薄く塗る
      voronoi
        | null shown = []
        | otherwise =
            [ box env (cols V.! nearest shown (x, y)) x y g g
            | x <- takeWhile (< ww - 280) [0, g ..], y <- takeWhile (< hh) [100, 100 + g ..] ]
      -- 点と中心を結ぶ線
      links = [ Color (hsba (hueOf w l) 60 90 0.18) (Line [pt env p, pt env (sv V.! l)])
              | (p, l) <- zip (wPts w) (wLabels w), l >= 0 ]
      dots = [ dot env (if l >= 0 then hsb (hueOf w l) 60 100 else hsb 0 0 80) p 6
             | (p, l) <- zip (wPts w) (wLabels w) ]
      centre (j, (cx, cy)) =
        let r = 14.1
            dia = [(cx, cy - r), (cx + r, cy), (cx, cy + r), (cx - r, cy)]
        in Pictures [ fillPoly env (hsb (hueOf w j) 70 100) dia
                    , path env (hsb 0 0 100) 3 (dia ++ take 1 dia) ]
      -- パネル：誤差のグラフ
      px = ww - 270
      hist = reverse (wSse w)
      graph = case hist of
        [] -> []
        _ ->
          let mx = maximum hist
              mn = minimum hist
              n = length hist
              vx i = px + 20 + fromIntegral (i :: Int) / fromIntegral (max (n - 1) 1) * 210
              vy v = 220 - (v - mn) / max (mx - mn) 1 * 140
          in [ path env (hsb 300 40 100) 2 [(vx i, vy v) | (i, v) <- zip [0 ..] hist]
             , txt env (px + 18) 236 14 (hsb 0 0 90) ("now " ++ show (round (last hist) :: Int)) ]
      info = [ "k = " ++ show (wK w) ++ "   iterations " ++ show (wIter w)
             , "Next: " ++ (if wAssign w then "(1) assign" else "(2) to centroid") ]
             ++ ["Converged: no change" | wDone w]
  in Pictures
       [ Pictures voronoi
       , Pictures links
       , Pictures dots
       , Pictures (map centre (zip [0 ..] shown))
       , box env (hsb 255 30 18) px 20 250 300
       , txt env (px + 18) 36 17 (hsb 0 0 95) "Error (sum of dist^2)"
       , Pictures graph
       , Pictures [ txt env (px + 18) (258 + fromIntegral k * 20) 14 (hsb 0 0 90) l | (k, l) <- zip [0 :: Int ..] info ]
       , txt env 30 24 22 (hsb 0 0 95) "k-means: assign, then move to the centroid"
       , txt env 30 56 14 (hsb 0 0 60) "Click add points  Drag a center  Space play/stop  Right 1 step  Up/Down k"
       , txt env 30 76 14 (hsb 0 0 60) "I random init  P k-means++  R new data  1-3 examples (blobs / ring / long)"
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let near = [j | (j, c) <- zip [0 ..] (wShown w), dist2 m c < 16]
        d = foldl (\_ j -> j) (-1) near
    in if d < 0 && my > 100 && mx < envW env - 280 && length (wPts w) < maxPoints
         then do
           new <- replicateM 15 ((\a b -> (mx + a * 25, my + b * 25)) <$> gaussian <*> gaussian)
           pure w { wDrag = d, wPts = wPts w ++ new, wLabels = wLabels w ++ map (const (-1)) new, wDone = False }
         else pure w { wDrag = d }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 ->
    let put xs = [if j == wDrag w then m else c | (j, c) <- zip [0 ..] xs]
    in pure w { wCents = put (wCents w), wShown = put (wShown w), wDone = False, wAssign = True }
  EventResize _ -> makeData env (wPreset w) w { wDrag = -1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wAuto = not (wAuto w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (lloydStep w)
  EventKey (SpecialKey KeyUp) Down _ _ -> initCenters True w { wK = min (wK w + 1) 8, wDrag = -1 }
  EventKey (SpecialKey KeyDown) Down _ _ -> initCenters True w { wK = max (wK w - 1) 1, wDrag = -1 }
  EventKey (Char c) Down _ _
    | c `elem` "iI" -> initCenters False w { wDrag = -1 }
    | c `elem` "pP" -> initCenters True w { wDrag = -1 }
    | c `elem` "rR" -> makeData env (wPreset w) w { wDrag = -1 }
    | c `elem` "123" -> makeData env (fromEnum c - fromEnum '0') w { wDrag = -1 }
  _ -> pure w
  where m@(mx, my) = mouse env

-- ---------- やってみよう ----------
-- 1. I と P を何度か押して、最後の誤差を比べる（初めの置き方しだいで変わる）
-- 2. お手本 2（輪）で k を増やしても、輪と真ん中がうまく分かれないのはなぜ？
-- 3. 中心を遠くへドラッグして、どう戻ってくるか見る

-- ---------- 道具：乱数 ----------
-- 正規分布の乱数（p5.js の randomGaussian）。ボックス＝ミュラー法
gaussian :: IO Float
gaussian = do
  u <- randomRIO (1e-6, 1)
  v <- randomRIO (0, 1)
  pure (sqrt (-2 * log u) * cos (tau * v))

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置を p5.js の座標で
mouse :: Env -> Point
mouse env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の枠
outline :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
outline env c t x y w h = Color c (thickPath t (map (pt env) [(x - t / 2, y), (x + w + t / 2, y), (x + w, y), (x + w, y + h), (x + w + t / 2, y + h), (x - t / 2, y + h), (x, y + h), (x, y)]))

-- 太さ t の線分（p5.js の line）と折れ線
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

path :: Env -> Color -> Float -> [Point] -> Picture
path env c t ps = Color c (thickPath t (map (pt env) ps))

-- 点線
dashed :: Env -> Color -> Float -> Float -> Point -> Point -> Picture
dashed env c t d a@(x1, y1) b@(x2, y2) =
  let l = dist2 a b
      k = floor (l / (2 * d)) :: Int
      f s = (x1 + (x2 - x1) * s / max l 1e-6, y1 + (y2 - y1) * s / max l 1e-6)
  in Pictures [seg env c t (f (2 * d * fromIntegral i)) (f (min l (2 * d * fromIntegral i + d))) | i <- [0 .. k]]

-- 直径 d の円（p5.js の circle）と、太さ t の輪
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

hoop :: Env -> Color -> Float -> Point -> Float -> Picture
hoop env c t p d = Color c (at (pt env p) (ring (d / 2) t))

-- 塗りつぶしの多角形（凸形のみ）
fillPoly :: Env -> Color -> [Point] -> Picture
fillPoly env c ps = Color c (Polygon (map (pt env) ps))

-- 文字：txt は左上が (x, y)、txtM は左端で上下中央、txtC は中心が (x, y)
txt, txtM, txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env (y + s)) s c str
txtM env x y s c str = label (gx env x) (gy env (y + s / 2)) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str

-- 小数点以下 d 桁（JavaScript の toFixed）
fix :: Int -> Float -> String
fix d v = showFFloat (Just d) v ""

