-- ============================================
-- 40-10 曲線短縮流 ─ どんな閉曲線も丸くなって消える（Grayson の定理）
-- 曲線の各点を曲率 κ に比例して法線方向へ動かす：∂p/∂t = κ n
--   等間隔の点列なら κ n ≈ (p_{i−1} − 2p_i + p_{i+1}) / h²（ラプラシアン）なので、各点を隣の平均に近づけるだけ
--   出っぱりは引っこみ、へこみは出る。長さは単調に減り、等周比 L²/(4πA) は 1（円）に近づき、最後は 1 点に消える
-- 操作：ドラッグ = 閉曲線を描く / 1〜4 = プリセット / ↑↓ = 速さ / スペース = 一時停止 / R = 最初から / S = 今の形を残す
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

gridH :: Float
gridH = 5                 -- ★ 点の間隔（px）

sigma :: Float
sigma = 0.6               -- ★ 1 歩の強さ（0〜1、1 に近いほど速いが荒い）

-- | グラフの 1 点：長さ、面積、等周比
data Hist = Hist !Float !Float !Float

data World = World
  { wPts, wInit :: !(U.Vector Point)
  , wRaw :: ![Point]              -- 描いている途中の線（新しい順）
  , wDrawing, wPaused, wGone :: !Bool
  , wHist :: !(Seq.Seq Hist)      -- 最大 600 個
  , wSnaps :: !(Seq.Seq (U.Vector Point))
  , wStepN :: !Int
  , wPerFrame :: !Int             -- ↑↓ で変わる（1 フレームの歩数）
  , wFrame :: !Int }

main :: IO ()
main = runSketch (sketch "40-10 Curve shortening flow" world0)
  { skBack  = hsb 160 25 10
  , skStart = \env -> pure (setPreset env 1 world0)
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World U.empty U.empty [] False False False Seq.empty Seq.empty 0 4 0

-- ★ プリセットの閉曲線
presetPts :: Env -> Int -> [Point]
presetPts env no =
  let cx = envW env * 0.5
      cy = envH env * 0.42
      s = min (envW env) (envH env) * 0.2
      polar r t = (cx + r * cos t, cy + r * sin t)
      one t = case no of
        1 -> polar (s * (1 + 0.55 * cos (5 * t))) t
        2 -> (cx + 2.2 * s * cos t, cy + s * sin t * (0.35 + 0.65 * cos t * cos t))
        3 -> polar (s * (1 + 0.3 * sin (3 * t + 1) + 0.25 * cos (7 * t) + 0.15 * sin (11 * t + 2))) t
        _ -> (cx + 1.8 * s * cos t, cy + 0.7 * s * sin t)
  in [one (fromIntegral i / 400 * tau) | i <- [0 .. 399 :: Int]]

setPreset :: Env -> Int -> World -> World
setPreset env no w = restart w { wInit = U.fromList (resample gridH True (presetPts env no)) }

restart :: World -> World
restart w = w { wPts = wInit w, wHist = Seq.empty, wSnaps = Seq.empty, wStepN = 0, wGone = False }

-- 曲線短縮流の 1 歩：各点を隣の平均に σ だけ近づける（= κ n 方向に動かす）
flowStep :: World -> World
flowStep w =
  let p = wPts w
      n = U.length p
      q = U.generate n $ \i ->
            let (ax, ay) = p U.! ((i - 1 + n) `mod` n)
                (bx, by) = p U.! ((i + 1) `mod` n)
                (x, y) = p U.! i
            in (x + sigma / 2 * (ax - 2 * x + bx), y + sigma / 2 * (ay - 2 * y + by))
      p' = U.fromList (resample gridH True (U.toList q))    -- 点が偏らないように打ち直す（長さが減ると点も減る）
  in w { wPts = p', wStepN = wStepN w + 1, wGone = wGone w || U.length p' < 8 }

curveLength :: U.Vector Point -> Float
curveLength p =
  let n = U.length p
  in foldl' (\s i -> s + dist2 (p U.! i) (p U.! ((i + 1) `mod` n))) 0 [0 .. n - 1]

curveArea :: U.Vector Point -> Float
curveArea p =
  let n = U.length p
      term s i = let (ax, ay) = p U.! i
                     (bx, by) = p U.! ((i + 1) `mod` n)
                 in s + ax * by - bx * ay
  in abs (foldl' term 0 [0 .. n - 1]) / 2

ratioOf :: Float -> Float -> Float
ratioOf l a = if a > 1 then l * l / (4 * pi * a) else 1

step :: Env -> World -> World
step env w0 =
  let key k = isDown env (SpecialKey k)
      pf1 = if key KeyUp then min 40 (wPerFrame w0 + 1) else wPerFrame w0
      pf2 = if key KeyDown then max 1 (pf1 - 1) else pf1
      frame = wFrame w0 + 1
      w1 = w0 { wPerFrame = pf2, wFrame = frame }
      run :: Int -> World -> World
      run 0 w = w
      run k w = if wGone w then w else run (k - 1) (flowStep w)
  in if wPaused w1 || wDrawing w1 || wGone w1 then w1 else
     let w2 = run pf2 w1
         l = curveLength (wPts w2)
         a = curveArea (wPts w2)
         h1 = if even frame then wHist w2 Seq.|> Hist l a (ratioOf l a) else wHist w2
     in w2 { wHist = if Seq.length h1 > 600 then Seq.drop 1 h1 else h1 }

closedShape :: Env -> Color -> Float -> U.Vector Point -> Picture
closedShape env c w p = case U.toList p of
  ps@(a : _ : _) -> path2 env c w (ps ++ [a])
  _ -> Blank

draw :: Env -> World -> Picture
draw env w =
  let pts = wPts w
      n = U.length pts
      -- 今の曲線（曲率の符号で色分け。プリセットの向きでは出っぱりが青、へこみが赤。逆向きに描くと色も逆）
      curve = [ seg2 env (hsb (if kk > 0 then 215 else 5) (min 100 (abs kk * 100)) 100) 3.5 p b
              | i <- [0 .. n - 1]
              , let (ax, ay) = pts U.! ((i - 1 + n) `mod` n)
              , let p@(x, y) = pts U.! i
              , let b@(bx, by) = pts U.! ((i + 1) `mod` n)
              , let lx = ax - 2 * x + bx
              , let ly = ay - 2 * y + by
              , let k = (bx - ax) * ly - (by - ay) * lx          -- 符号つき曲率（向きに対して左か右か）
              , let kk = k / (gridH * gridH * gridH) * 40 ]
      l = curveLength pts
      a = curveArea pts
      g = hsb 0 0 65
  in Pictures $
       -- 最初の形（薄く）と残した形
       [closedShape env (hsba 0 0 100 0.18) 1.5 (wInit w)] ++
       map (closedShape env (hsba 45 70 100 0.35) 1.5) (toList (wSnaps w)) ++
       (if wGone w then [] else curve) ++
       [path2 env (hsba 0 0 100 0.7) 2 (wRaw w) | wDrawing w] ++
       [graph env w] ++
       [ txt env 20 40 16 white ("drag: draw a closed curve   1-4: preset   Up/Down: speed " ++ show (wPerFrame w) ++ " steps/frame")
       , txt env 20 62 16 white ("Space: " ++ (if wPaused w then "resume" else "pause") ++ "   R: restart   S: keep this shape")
       , txt env 20 86 14 g ("dp/dt = k n  (move each point toward the mean of its neighbors)   steps " ++ show (wStepN w) ++ "   points " ++ show n)
       , txt env 20 106 14 g ("length L = " ++ nf 0 l ++ "   area A = " ++ nf 0 a ++ "   isoperimetric ratio L^2/(4 pi A) = " ++ nf 3 (ratioOf l a))
       ] ++
       [ txt env 20 142 22 (hsb 45 90 100) "Gone! It rounded out, then shrank to a point (Grayson 1987). R to restart" | wGone w ]

graph :: Env -> World -> Picture
graph env w =
  let gx = 20
      gy = envH env - 170
      gw = envW env - 40
      gh = 140
      back = box env gx gy gw gh (hsba 0 0 100 0.06)
      hist = toList (wHist w)
  in case hist of
       (Hist l0' a0' _ : _ : _) ->
         let l0 = max l0' 1
             a0 = max a0' 1
             rmax = foldl' (\m (Hist _ _ r) -> max m r) 1.05 hist
             xAt i = gx + fromIntegral (i :: Int) / 600 * gw
             yAt v = gy + gh - v * (gh - 20)
             plot sel = [ (xAt i, yAt (sel h)) | (i, h) <- zip [0 ..] hist ]
             Hist _ _ rNow = foldl' (\_ h -> h) (Hist 0 0 1) hist
         in Pictures
              [ back
              , path2 env (hsb 45 70 100) 2 (plot (\(Hist l _ _) -> l / l0))
              , path2 env (hsb 130 70 100) 2 (plot (\(Hist _ a _) -> a / a0))
              , path2 env (hsb 300 60 100) 2 (plot (\(Hist _ _ r) -> (r - 1) / (rmax - 1)))
              , txt env (gx + 10) (gy + 18) 14 (hsb 45 70 100) "length L/L0"
              , txt env (gx + 130) (gy + 18) 14 (hsb 130 70 100) "area A/A0"
              , txt env (gx + 240) (gy + 18) 14 (hsb 300 60 100) ("ratio L^2/(4 pi A) - 1 (0 = circle)   now " ++ nf 3 rNow)
              ]
       _ -> back

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos
    | snd (fromG env pos) < envH env - 180 -> w { wDrawing = True, wRaw = [fromG env pos] }
  EventKey (MouseButton LeftButton) Up _ _ | wDrawing w ->
    let w1 = w { wDrawing = False }
        new = U.fromList (resample gridH True (reverse (wRaw w)))
    in if length (wRaw w) < 15 || U.length new < 8 then w1 else restart w1 { wInit = new }
  EventMotion pos | wDrawing w -> case wRaw w of
    (l : _) | dist2 (fromG env pos) l > 3 -> w { wRaw = take 4000 (fromG env pos : wRaw w) }
    _ -> w
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> setPreset env (fromEnum c - fromEnum '0') w
    | c == 'r' || c == 'R' -> restart w
    | c == 's' || c == 'S' ->
        let s0 = if Seq.length (wSnaps w) > 12 then Seq.drop 1 (wSnaps w) else wSnaps w
        in w { wSnaps = s0 Seq.|> wPts w }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. ダンベル（2）は真ん中がくびれて切れそうに見えるが切れない（平面では必ず丸くなる）。3D の曲面（09）との違い
-- 2. 等周比のグラフが単調に 1 に近づくのを見よう。どの形がいちばん時間がかかる？
-- 3. 自分で「8 の字」のように交差した曲線を描くとどうなる？（Grayson の定理は単純閉曲線だけ）
-- 4. sigma を 1.2 にすると不安定になってギザギザが育つ。なぜ？（熱方程式の安定条件）

-- ==================================================
-- ここから下は「曲線の道具」です（等間隔に打ち直す）
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- | 折れ線を間隔 h の点列に打ち直す
resample :: Float -> Bool -> [Point] -> [Point]
resample _ _ [] = []
resample h isClosed src@(p0 : _) = trimEnd (p0 : walk 0 (zip src (drop 1 src ++ [p0 | isClosed])))
  where
    walk _ [] = []
    walk acc (((ax, ay), (bx, by)) : rest)
      | d < 1e-9 = walk acc rest
      | otherwise = inner acc 0
      where
        d = sqrt ((bx - ax) * (bx - ax) + (by - ay) * (by - ay))
        inner a t
          | a + (d - t) >= h =
              let t' = t + (h - a)
              in (ax + (bx - ax) * (t' / d), ay + (by - ay) * (t' / d)) : inner 0 t'
          | otherwise = walk (a + d - t) rest
    -- 閉じた曲線で、最後の点が最初の点に近すぎたら捨てる
    trimEnd out = case reverse out of
      (l : more@(_ : _)) | isClosed && dist2 p0 l < h * 0.5 -> reverse more
      _ -> out

-- ==================================================
-- ここから下は「座標と文字の道具」です
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- | p5.js の座標（左上が原点、y は下向き）→ gloss の座標（中央が原点、y は上向き）
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- | gloss の座標 → p5.js の座標（マウスの位置を読むときに使う）
fromG :: Env -> Point -> Point
fromG env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- | p5.js の座標で文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str

-- | p5.js の座標で長方形を塗る（(x, y) は左上）
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (gx, gy) = toG env (x + w / 2, y + h / 2) in Color c (rect gx gy w h)

-- | p5.js の座標で線分を引く（w は太さ、1 以下なら細い線）
seg2 :: Env -> Color -> Float -> Point -> Point -> Picture
seg2 env c w a b
  | w <= 1 = Color c (Line [toG env a, toG env b])
  | otherwise = Color c (thickLine w (toG env a) (toG env b))

-- | p5.js の座標で折れ線を引く（色は 1 色）
path2 :: Env -> Color -> Float -> [Point] -> Picture
path2 env c w ps
  | w <= 1 = Color c (Line (map (toG env) ps))
  | otherwise = Color c (thickPath w (map (toG env) ps))

-- | p5.js の座標で点（塗りつぶした円）を打つ。r は半径
dotAt :: Env -> Color -> Float -> Point -> Picture
dotAt env c r p = at (toG env p) (Color c (disc r))

-- | p5.js の座標で輪を描く。r は半径、w は線の太さ
ringAt :: Env -> Color -> Float -> Float -> Point -> Picture
ringAt env c r w p
  | w <= 1 = at (toG env p) (Color c (Circle r))
  | otherwise = at (toG env p) (Color c (ring r w))

-- | 小数 n 桁の文字列（p5.js の nf に相当）
nf :: Int -> Float -> String
nf n x = showFFloat (Just n) x ""
