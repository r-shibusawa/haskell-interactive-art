-- ============================================
-- 08 凸包：ジャービスの行進（ギフト包装法）
-- いちばん下の点から、ひもを巻きつけるように「いちばん右側に見える点」を次々選ぶ
-- 1 頂点を決めるのに全部の点を調べるので、手間は n × h（h = 凸包の頂点数）
-- h が小さいとグレアム走査より速く、全部が凸包に乗ると（円）遅い
-- 操作：クリック = 点を追加 / ドラッグ = 動かす / スペース = 再生・停止 / → = 1 回調べる
--       R = ランダム / C = 全部消す / ↑↓ = 速さ / 1〜4 = お手本（ランダム・円・かたまり・三角形）
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

nPoints :: Int
nPoints = 30            -- ★ ランダムに置く点の数

type Pts = U.Vector (Float, Float)

data S = S
  { sHull  :: ![Int]    -- ここまでの凸包（古い順）
  , sCur   :: !Int
  , sCand  :: !Int
  , sProbe :: !Int
  , sTests :: !Int
  , sMsg   :: String
  }

data World = World
  { wPts   :: !Pts
  , wPerF  :: !Int      -- ★ 1 フレームに何回調べるか（↑↓）
  , wAuto  :: !Bool
  , wS     :: !S
  , wGen   :: [S]
  , wDrag  :: !Int
  }

main :: IO ()
main = runSketch (sketch "17-08 Jarvis march" initialWorld)
  { skBack  = cBg
  , skStart = \env -> preset env 1 initialWorld
  , skStep  = \_ _ w -> pure (if wAuto w then iterate advance w !! wPerF w else w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

emptyS :: String -> S
emptyS = S [] (-1) (-1) (-1) 0

initialWorld :: World
initialWorld = World U.empty 1 True (emptyS "") [] (-1)

preset :: Env -> Int -> World -> IO World
preset env k w = do
  let bigW = envW env - 280
      cx = bigW / 2
      cy = envH env / 2 + 30
      r = min bigW (envH env) * 0.36
      n = nPoints
  ps <- forM [0 .. n - 1] $ \i -> do
    let fi = fromIntegral i
    case k of
      2 -> let a = tau * fi / fromIntegral n in pure (cx + cos a * r, cy + sin a * r)   -- h = n（いちばん遅い）
      3 -> do
        a <- gaussian
        b <- gaussian
        pure (cx + a * r * 0.3, cy + b * r * 0.3)
      4 | i < 3 ->                                                                    -- h = 3（いちばん速い）
            let a = fi * tau / 3 - pi / 2 in pure (cx + cos a * r * 1.1, cy + sin a * r * 1.1)
        | otherwise -> do
            a <- randomRIO (-r, r)
            b <- randomRIO (-r, r)
            pure (cx + a * 0.4, cy + b * 0.4)
      _ -> do
        a <- randomRIO (-r, r)
        b <- randomRIO (-r, r)
        pure (cx + a * 1.2, cy + b)
  pure (restart w { wPts = U.fromList ps })

-- + なら左回り
turn :: Point -> Point -> Point -> Float
turn (ox, oy) (ax, ay) (bx, by) = (ax - ox) * (oy - by) - (oy - ay) * (bx - ox)

d2 :: Point -> Point -> Float
d2 (ax, ay) (bx, by) = (ax - bx) * (ax - bx) + (ay - by) * (ay - by)

restart :: World -> World
restart w =
  let ok = U.length (wPts w) >= 3
      s0 = emptyS (if ok then "" else "Place at least 3 points (click)")
  in w { wS = s0, wGen = if ok then steps (jarvis (wPts w)) s0 else [] }

-- ★ ギフト包装法
jarvis :: Pts -> Gen S ()
jarvis pts = do
  let n = U.length pts
      p i = pts U.! i
      lower a b = let (ax, ay) = p a
                      (bx, by) = p b
                  in ay > by || (ay == by && ax < bx)
      start = foldl (\b i -> if lower i b then i else b) 0 [1 .. n - 1]
      probeAll cur j
        | j >= n = pure ()
        | j == cur = probeAll cur (j + 1)
        | otherwise = do
            cand <- gets sCand
            let c = turn (p cur) (p cand) (p j)
                -- j が cur→cand の右側にある（または一直線でもっと遠い）なら、j の方が「外側」
                cand' = if c < 0 || (c == 0 && d2 (p cur) (p j) > d2 (p cur) (p cand)) then j else cand
            modify (\s -> s { sProbe = j, sTests = sTests s + 1, sCand = cand'
                            , sMsg = "Vertex " ++ show (length (sHull s)) ++ ": testing point " ++ show j
                                       ++ " (candidate: point " ++ show cand' ++ ")" })
            yield
            probeAll cur (j + 1)
      wrap cur = do
        modify (\s -> s { sCur = cur, sHull = sHull s ++ [cur], sCand = (cur + 1) `mod` n })
        probeAll cur 0
        cand <- gets sCand
        hullLen <- gets (length . sHull)
        modify (\s -> s { sCur = cand })
        if hullLen > n || cand == start then pure () else wrap cand    -- hullLen > n は念のため
  wrap start
  modify (\s ->
    let h = length (sHull s)
    in s { sProbe = -1, sCand = -1
         , sMsg = "Done! h = " ++ show h ++ ", tests " ++ show (sTests s) ++ " (about n x h = "
                    ++ show n ++ " x " ++ show h ++ " = " ++ show (n * h) ++ ")" })
  yield

advance :: World -> World
advance w = case wGen w of
  [] -> w
  (s : r) -> w { wS = s, wGen = r }

drawW :: Env -> World -> Picture
drawW env w =
  let ww = envW env
      pts = wPts w
      s = wS w
      p i = pts U.! i
      genOn = not (null (wGen w))
      hull = sHull s
      cur = sCur s
      cand = sCand s
      probe = sProbe s
      -- 凸包（ここまで）
      hullLine = case map p hull of
        [] -> Blank
        ps@(p1 : _) -> path env cOrchid 3 (if genOn then ps ++ [p cur] else ps ++ [p1])
      -- ひも：いまの点 → 候補、いまの点 → 調べている点
      string'
        | genOn && cur >= 0 && cand >= 0 =
            let c@(cx, cy) = p cur
                (dx, dy) = p cand
                l = max 1e-6 (dist2 c (dx, dy))
                far = (cx + (dx - cx) / l * 2000, cy + (dy - cy) / l * 2000)
            in Pictures
                 [ seg env cGold 3 c far
                 , if probe >= 0 then seg env cMint 1.5 c (p probe) else Blank ]
        | otherwise = Blank
      pointPic i =
        let inHull = i `elem` hull
            c | i == cur && genOn = cGold
              | i == cand = cGold
              | inHull = cOrchid
              | i == probe = cMint
              | otherwise = cFg
        in dot env c (p i) (if inHull || i == cand then 13 else 9)
      -- パネル
      px = ww - panelW - 20
      py = panelY
      n = U.length pts
      h = length hull
      lines' = [ "n = " ++ show n, "h = " ++ show h, "Tests: " ++ show (sTests s), ""
               , "Jarvis ~ n*h"
               , "Graham ~ n*log2 n = " ++ show (round (fromIntegral n * logBase 2 (fromIntegral (max n 2) :: Float)) :: Int) ]
  in Pictures
       [ hullLine
       , string'
       , Pictures (map pointPic [0 .. n - 1])
       , box env cPanel px py panelW 250
       , txt env (px + 18) (py + 16) 17 cFg "Comparing the work"
       , Pictures [ txt env (px + 18) (py + 50 + fromIntegral k * 22) 15 cFg l | (k, l) <- zip [0 :: Int ..] lines' ]
       , txt env (px + 18) (py + 190) 14 cDim "Circle (2): h = n, so n^2"
       , txt env (px + 18) (py + 212) 14 cDim "Triangle (4): h = 3, so 3n"
       , txt env 30 24 22 cFg "Convex hull: Jarvis march (wrap a string)"
       , txt env 30 56 14 cDim ("Click add  Drag move  Space play/stop  Right 1 test  Up/Down speed (" ++ show (wPerF w)
           ++ ")  R random  C clear  1-4 examples")
       , txt env 30 84 17 cGold (sMsg s)
       ]

-- パネルの大きさ（文字が入るように p5.js 版より少し広く、低い位置に置く）
panelW, panelY :: Float
panelW = 280
panelY = 110

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | inPanel -> pure w
    | otherwise ->
        let near = [i | i <- [0 .. U.length (wPts w) - 1], dist2 m (wPts w U.! i) < 12]
        in pure w { wDrag = foldl (\_ i -> i) (-1) near }
  EventKey (MouseButton LeftButton) Up _ _
    | inPanel -> pure w { wDrag = -1 }
    | wDrag w < 0 && my > 110 -> pure (restart w { wPts = U.snoc (wPts w) m })
    | otherwise -> pure w { wDrag = -1 }
  EventMotion _
    | wDrag w >= 0 -> pure (restart w { wPts = wPts w U.// [(wDrag w, m)] })
  EventResize _ -> preset env 1 w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wAuto = not (wAuto w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (advance w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wPerF = min (wPerF w * 2) 64 }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wPerF = max (wPerF w `div` 2) 1 }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> preset env 1 w
    | c `elem` "cC" -> pure (restart w { wPts = U.empty, wDrag = -1 })
    | c `elem` "1234" -> preset env (fromEnum c - fromEnum '0') w
  _ -> pure w
  where
    m@(mx, my) = mouse env
    inPanel = mx > envW env - panelW - 20 && my < panelY + 260

-- ---------- やってみよう ----------
-- 1. お手本 2（円）と 4（三角形）で、調べた回数を比べる
-- 2. 点の数 nPoints を 100 にすると、グレアム走査との差はどう変わる？
-- 3. c < 0 を c > 0 にすると、ひもはどちら回りに巻きつく？

-- ---------- 道具：乱数 ----------
-- 正規分布の乱数（p5.js の randomGaussian）。ボックス＝ミュラー法
gaussian :: IO Float
gaussian = do
  u <- randomRIO (1e-6, 1)
  v <- randomRIO (0, 1)
  pure (sqrt (-2 * log u) * cos (tau * v))

-- ---------- 道具：ジェネレーター（function*）の代わり ----------
-- yield のたびに状態を 1 つ流す計算。結果は「状態の遅延リスト」になり、
-- World がその残りを持っていて、1 手進めるたびに先頭を 1 つ取る。
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

-- ---------- 道具：色 ----------
cBg, cPanel, cFg, cDim, cOrchid, cMint, cGold, cBad, cEdge, cGreen, cRed, cAmber, cBlue :: Color
cBg     = rgb255 0x14 0x10 0x1f
cPanel  = rgb255 0x22 0x1a 0x33
cFg     = rgb255 0xe9 0xe4 0xf5
cDim    = rgb255 0x8c 0x84 0xa3
cOrchid = rgb255 0xf0 0xa3 0xff
cMint   = rgb255 0x6f 0xe3 0xd0
cGold   = rgb255 0xff 0xcf 0x6b
cBad    = rgb255 0xff 0x6b 0x81
cEdge   = rgb255 0x3a 0x2f 0x52
cGreen  = rgb255 0x2f 0x5e 0x57
cRed    = rgb255 0x5e 0x2a 0x3a
cAmber  = rgb255 0x5a 0x4a 0x1f
cBlue   = rgb255 0x2f 0x4a 0x5e

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

