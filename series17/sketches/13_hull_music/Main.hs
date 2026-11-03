-- ============================================
-- 13 凸包を聞く（自由制作のベース）🔊
-- 置いた点の凸包を毎フレーム計算し（アンドリューの単調連鎖法 = グレアム走査の仲間）、
-- 光の粒が凸包のふちを一周しながら、頂点を通るたびに音を鳴らす。高さ = 頂点の上下の位置
-- 中の点は、中心から回るレーダーが通ると小さく鳴る。点を動かすと、形も曲も変わる
-- 操作：クリック = 点を置く / ドラッグ = 動かす
--       C = 全部消す / R = ランダム / S = 音のオン・オフ / 1〜3 = 音階 / ↑↓ = 速さ
-- ★ 自由制作：音階・音色・速さ・色・形のルールを変えて、自分の「凸包楽器」をつくろう
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, replicateM, when)
import Data.List (sortOn)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

scaleOf :: Int -> [Int]
scaleOf 2 = [0, 3, 5, 7, 10]    -- 短調ペンタトニック
scaleOf 3 = [0, 1, 5, 7, 8]     -- 都節音階
scaleOf _ = [0, 2, 4, 7, 9]     -- ★ 長調ペンタトニック

baseMidi :: Int
baseMidi = 57           -- ★ いちばん下の音（MIDI 番号）

wave :: Wave
wave = Triangle         -- ★ 音色：Sine Triangle Square Sawtooth

data Flash = Flash { fP :: !Point, fHue :: !Float, fLife :: !Float }

data World = World
  { wPts     :: !(U.Vector (Float, Float))
  , wHull    :: ![Int]
  , wScale   :: !Int
  , wSpeed   :: !Float      -- ★ 光の粒の速さ（1 フレームに何ピクセル）
  , wSound   :: !Bool
  , wHead    :: !Float      -- 周の上の位置（ピクセル）
  , wRadar   :: !Float      -- レーダーの角度
  , wCenter  :: !Point      -- レーダーの中心
  , wFlashes :: ![Flash]    -- 鳴った点の光
  , wDrag    :: !Int
  , wSpark   :: ![Point]    -- 光の粒の残像（新しい順）
  , wSweep   :: ![Float]    -- レーダーの残像（新しい順）
  }

main :: IO ()
main = runSketch (sketch "17-13 Hear the convex hull" initialWorld)
  { skBack  = hsb 250 50 8
  , skStart = \env -> do
      ps <- randomPts 12 (envW env * 0.2) (envW env * 0.8) (envH env * 0.25) (envH env * 0.85)
      pure initialWorld { wPts = ps }
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialWorld :: World
initialWorld = World U.empty [] 1 3 True 0 0 (0, 0) [] (-1) [] []

randomPts :: Int -> Float -> Float -> Float -> Float -> IO (U.Vector (Float, Float))
randomPts n x0 x1 y0 y1 = U.fromList <$> replicateM n ((,) <$> randomRIO (x0, x1) <*> randomRIO (y0, y1))

-- ★ 凸包（単調連鎖法）：x で並べて、下側と上側を「左に曲がる」だけ残して作る
turn :: Point -> Point -> Point -> Float
turn (ox, oy) (ax, ay) (bx, by) = (ax - ox) * (by - oy) - (ay - oy) * (bx - ox)

convexHull :: U.Vector (Float, Float) -> [Int]
convexHull ps
  | n < 3 = [0 .. n - 1]
  | otherwise = reverse (drop 1 (chain idx)) ++ reverse (drop 1 (chain (reverse idx)))
  where
    n = U.length ps
    idx = sortOn (ps U.!) [0 .. n - 1]
    -- スタック（先頭が一番上）に順に積む
    chain = foldl add []
    add (b : a : rest) i
      | turn (ps U.! a) (ps U.! b) (ps U.! i) <= 0 = add (a : rest) i
    add st i = i : st

-- 上にあるほど高い音
pitchOf :: Env -> World -> Point -> Int
pitchOf env w (_, y) =
  let sc = scaleOf (wScale w)
      len = length sc
      k0 = floor (remap y (envH env) 0 0 (fromIntegral (len * 3))) :: Int
      k = max 0 (min (len * 3 - 1) k0)
      note = case drop (k `mod` len) sc of
        (v : _) -> v
        [] -> 0
  in baseMidi + 12 * (k `div` len) + note

-- 点 i を鳴らす
ring' :: Env -> Float -> Float -> World -> Int -> IO World
ring' env vol dur w i = do
  let p = wPts w U.! i
      onHull = i `elem` wHull w
  when (wSound w) (playTone (midiFreq (pitchOf env w p)) vol dur (if onHull then wave else Sine))
  pure w { wFlashes = Flash p (if onHull then 290 else 170) 100 : wFlashes w }

stepW :: Env -> Float -> World -> IO World
stepW env _ w0 = do
  let hull = convexHull (wPts w0)
      w1 = w0 { wHull = hull }
  w2 <- if length hull >= 2 then moveSpark env w1 else pure w1 { wSpark = drop 1 (wSpark w1) }
  w3 <- if length hull >= 3 then sweepRadar env w2 else pure w2 { wSweep = [] }
  pure w3 { wFlashes = [f { fLife = fLife f - 3 } | f <- wFlashes w3, fLife f > 0] }

-- 光の粒がふちを進む
moveSpark :: Env -> World -> IO World
moveSpark env w = do
  let hull = wHull w
      p i = wPts w U.! i
      nextOf = drop 1 hull ++ take 1 hull
      lens = zipWith (\a b -> dist2 (p a) (p b)) hull nextOf
      total = sum lens
      m = max total 1
      before = wrapF m (wHead w)
      headNew = wHead w + wSpeed w
      after = wrapF m headNew
      starts = scanl (+) 0 lens        -- 各頂点の周上の位置
      crossed v
        | before <= after = before < v && v <= after
        | otherwise = v > before || v <= after
      -- 頂点を通過したら鳴らす
      hits = [i | (i, v) <- zip hull starts, crossed v]
  w1 <- foldM (ring' env 0.28 0.5) w hits
  -- 粒の位置
  let edges = zip3 (zip hull nextOf) lens starts
      spark = case [e | e@(_, l, s) <- edges, after <= s + l] of
        (((a, b), l, s) : _) ->
          let t = (after - s) / max l 1e-6
              (ax, ay) = p a
              (bx, by) = p b
          in [(lerpF ax bx t, lerpF ay by t)]
        [] -> []
  pure w1 { wHead = headNew, wSpark = take 8 (spark ++ wSpark w1) }

-- レーダー：中の点を小さく鳴らす
sweepRadar :: Env -> World -> IO World
sweepRadar env w = do
  let hull = wHull w
      p i = wPts w U.! i
      count = fromIntegral (length hull)
      cx = sum (map (fst . p) hull) / count
      cy = sum (map (snd . p) hull) / count
      prev = wRadar w
      radar = wrapF tau (prev + 0.02)
      angleOf (x, y) = wrapF tau (atan2 (y - cy) (x - cx))
      passed a
        | prev < radar = prev < a && a <= radar
        | otherwise = a > prev || a <= radar
      inner = [i | i <- [0 .. U.length (wPts w) - 1], i `notElem` hull, passed (angleOf (p i))]
  w1 <- foldM (ring' env 0.08 0.25) w inner
  pure w1 { wRadar = radar, wCenter = (cx, cy), wSweep = take 6 (radar : wSweep w1) }

drawW :: Env -> World -> Picture
drawW env w =
  let pts = wPts w
      p i = pts U.! i
      hull = wHull w
      hullPts = map p hull
      n = U.length pts
      -- 凸包の面
      face
        | length hull >= 3 = Pictures
            [ fillPoly env (hsba 290 50 60 0.12) hullPts
            , path env (hsba 290 40 100 0.8) 2 (hullPts ++ take 1 hullPts) ]
        | otherwise = Blank
      -- 光の粒（p5.js の残像の代わりに、少し前の位置も薄く描く）
      sparkPics =
        [ Pictures [ dot env (hsba 50 60 100 (a * 0.25)) q 40, dot env (hsba 50 60 100 a) q 16 ]
        | (k, q) <- reverse (zip [0 :: Int ..] (wSpark w)), let a = 0.65 ^ k ]
      (cx, cy) = wCenter w
      radarPics =
        [ seg env (hsba 170 60 100 (0.3 * 0.65 ^ k)) 1 (cx, cy) (cx + cos r * 2000, cy + sin r * 2000)
        | (k, r) <- zip [0 :: Int ..] (wSweep w) ]
      pointPic i =
        let on = i `elem` hull
        in dot env (if on then hsb 290 50 100 else hsb 170 50 90) (p i) (if on then 14 else 9)
      flashPic (Flash q h life) = hoop env (hsba h 60 100 (clampF 0 1 (life / 100))) 2 q (110 - life)
      scaleName = case wScale w of
        2 -> "minor"
        3 -> "miyako-bushi"
        _ -> "major"
  in Pictures
       [ face
       , Pictures radarPics
       , Pictures sparkPics
       , Pictures (map pointPic [0 .. n - 1])
       , Pictures (map flashPic (wFlashes w))
       , txt env 30 24 22 (hsb 0 0 95) "Hear the convex hull"
       , txt env 30 56 14 (hsb 0 0 65) ("Click place  Drag move  C clear  R random  S sound " ++ (if wSound w then "ON" else "OFF")
           ++ "  1-3 scale (" ++ scaleName ++ ")  Up/Down speed")
       , txt env 30 78 14 (hsb 0 0 65) ("Points " ++ show n ++ " / hull vertices " ++ show (length hull))
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let near = [i | i <- [0 .. U.length (wPts w) - 1], dist2 m (wPts w U.! i) < 12]
        d = foldl (\_ i -> i) (-1) near
    in pure (if d < 0 && my > 90 then w { wDrag = d, wPts = U.snoc (wPts w) m } else w { wDrag = d })
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = -1 }
  EventMotion _
    | wDrag w >= 0 -> pure w { wPts = wPts w U.// [(wDrag w, m)] }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min (wSpeed w + 1) 20 }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max (wSpeed w - 1) 1 }
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> pure w { wPts = U.empty, wHull = [], wDrag = -1 }
    | c `elem` "rR" -> do
        ps <- randomPts 14 (envW env * 0.15) (envW env * 0.85) (envH env * 0.25) (envH env * 0.9)
        pure w { wPts = ps, wHull = [], wDrag = -1 }
    | c `elem` "sS" -> pure w { wSound = not (wSound w) }
    | c `elem` "123" -> pure w { wScale = fromEnum c - fromEnum '0' }
  _ -> pure w
  where m@(_, my) = mouse env

-- ---------- やってみよう ----------
-- 1. scaleOf の数字を変えて、自分の音階をつくる
-- 2. wave を Square や Sawtooth にして、音色を変える
-- 3. 頂点の x 座標で音量を変えると、どう聞こえる？ 自分の「凸包楽器」をつくろう

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

