-- ============================================
-- 40-13 曲率を聴く（音：まずクリック）
-- マウスで描いた曲線の上を再生ヘッドが走り、その点の曲率 |κ| で音の高さが決まる（急カーブほど高い音）
--   κ の符号（左曲がり・右曲がり）で音色を変え、まっすぐな所は低いドローン。曲線が「メロディ」になる
--   曲率は 00 と同じ差分の式：κ = (x'y'' − y'x'') / (x'² + y'²)^{3/2}
-- 操作：ドラッグ = 曲線を描く / 1〜3 = プリセット / ←→ = テンポ / Q = 音階に丸める / スペース = 一時停止 / C = 消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (when)
import Data.List (sort)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

gridH :: Float
gridH = 6                 -- ★ 点の間隔（px）

baseFreq :: Float
baseFreq = 110            -- ★ まっすぐな所の音の高さ（Hz）

octaves :: Float
octaves = 3               -- ★ 急カーブで上がる最大の幅（オクターブ）

pentatonic :: [Float]
pentatonic = [0, 2, 4, 7, 9]

notePeriod :: Int
notePeriod = 12           -- ★ 音を鳴らす間隔（フレーム）。連続音の代わりに短い音を並べる

noteVol :: Float
noteVol = 0.2             -- ★ 音量

data World = World
  { wPts :: !(U.Vector Point)
  , wKappa :: !(U.Vector Float)
  , wRaw :: ![Point]
  , wDrawing, wClosed, wPaused, wQuant, wAudio :: !Bool
  , wHead, wSpeed, wKRef :: !Float
  , wFrame :: !Int }

main :: IO ()
main = runSketch (sketch "40-13 Listening to curvature" world0)
  { skBack  = hsb 300 25 10
  , skStart = \env -> pure (setPreset env 1 world0)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World U.empty U.empty [] False True False True False 0 1.5 0.03 0

presetPts :: Env -> Int -> [Point]
presetPts env no =
  let cx = envW env * 0.5
      cy = envH env * 0.42
      s = min (envW env) (envH env) * 0.2
      one i =
        let f = fromIntegral (i :: Int) / 500 :: Float
            t = f * tau
        in case no of
             1 -> let r = s * 0.15 + s * 1.3 * f in (cx + r * cos (3 * t), cy + r * sin (3 * t))
             2 -> ( cx + s * 0.1 * 16 * sin t * sin t * sin t
                  , cy - s * 0.1 * (13 * cos t - 5 * cos (2 * t) - 2 * cos (3 * t) - cos (4 * t)) )
             _ -> (cx + 1.8 * s * sin (3 * t + 1), cy + 0.9 * s * sin (4 * t))
  in map one [0 .. 500]

setCurve :: Bool -> [Point] -> World -> World
setCurve closed ps w =
  let pts = U.fromList ps
      kap = curvatureOf gridH closed pts
      a = U.fromList (sort (map abs (U.toList kap)))
      i = floor (fromIntegral (U.length a) * (0.95 :: Float))
      v = if U.null a then 0 else a U.! min i (U.length a - 1)
  in w { wClosed = closed, wPts = pts, wKappa = kap, wKRef = max (if v == 0 then 0.02 else v) 0.004, wHead = 0 }

setPreset :: Env -> Int -> World -> World
setPreset env no = let closed = no /= 1 in setCurve closed (resample gridH closed (presetPts env no))

-- 曲率 → 周波数（|κ| が大きいほど高く、Q で五音音階に丸める）
freqOf :: World -> Float -> Float
freqOf w k =
  let u = min 1 (abs k / wKRef w)
      semis = u * 12 * octaves
      o = fromIntegral (floor (semis / 12) :: Int) :: Float
      r = semis - o * 12
      best = foldl (\p q -> if abs (q - r) < abs (p - r) then q else p) 0 pentatonic
      s = if wQuant w then o * 12 + best else semis
  in baseFreq * 2 ** (s / 12)

curIndex :: World -> Int
curIndex w = min (U.length (wPts w) - 1) (floor (wHead w))

curK :: World -> Float
curK w = let i = curIndex w in if i >= 0 && i < U.length (wKappa w) then wKappa w U.! i else 0

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let key k = isDown env (SpecialKey k)
      s1 = if key KeyLeft then max 0.3 (wSpeed w0 - 0.03) else wSpeed w0
      s2 = if key KeyRight then min 8 (s1 + 0.03) else s1
      n = fromIntegral (U.length (wPts w0)) :: Float
      h0 = wHead w0 + s2
      moving = not (wPaused w0) && not (wDrawing w0) && U.length (wPts w0) > 3
      h1 | not moving = wHead w0
         | h0 >= n = if wClosed w0 then h0 - n else 0
         | otherwise = h0
      w1 = w0 { wSpeed = s2, wHead = h1, wFrame = wFrame w0 + 1 }
      k = curK w1
      f = freqOf w1 k
  -- 音の更新（左曲がりは三角波、右曲がりは 1 オクターブ下のサイン波）
  when (wAudio w1 && not (wPaused w1) && not (wDrawing w1) && wFrame w1 `mod` notePeriod == 0) $
    if k > 0 then playTone f noteVol 0.22 Triangle else playTone (f * 0.5) noteVol 0.22 Sine
  pure w1

draw :: Env -> World -> Picture
draw env w =
  let pts = wPts w
      kap = wKappa w
      n = U.length pts
      i = curIndex w
      k = curK w
      f = freqOf w k
      kRef = wKRef w
      -- 曲線（曲率で色、再生済みの所は明るく）
      curve = [ seg2 env (hsb (if kap U.! j > 0 then 25 else 200) (40 + 60 * kk) (if j <= i then 100 else 55)) 3 (pts U.! j) (pts U.! ((j + 1) `mod` n))
              | j <- [0 .. n - (if wClosed w then 1 else 2)], let kk = min 1 (abs (kap U.! j) / kRef) ]
      -- 再生ヘッド：接触円の大きさで輪を描く（高い音ほど小さい輪）
      headPart = if n <= 3 then [] else
        let p = pts U.! max 0 i
            r = min (1 / max (abs k) 1e-6) 300
        in [ ringAt env (hsba 45 90 100 0.6) r 2 p
           , dotAt env (hsb 45 90 100) (7 + 5 * min 1 (abs k / kRef)) p ]
      -- 音の高さのバー
      bx = 20
      by = envH env - 60
      bw = envW env - 40
      u = logBase 2 (f / baseFreq) / octaves
      barCol = hsb (if k > 0 then 25 else 200) 80 100
      g = hsb 0 0 65
  in Pictures $
       curve ++
       [path2 env (hsba 0 0 100 0.7) 2 (wRaw w) | wDrawing w] ++
       headPart ++
       [ box env bx by bw 24 (hsba 0 0 100 0.08)
       , box env bx by (bw * clampF 0 1 u) 24 barCol
       , txt env (bx + 10) (by - 8) 15 white (nf 0 f ++ " Hz   |k| = " ++ nf 4 (abs k) ++ "  (reference " ++ nf 4 kRef ++ ")") ] ++
       [ txt env 20 40 16 white ((if wAudio w then "" else "> click first (sound on)   ") ++ "drag: draw   1-3: preset   <- ->: tempo " ++ nf 1 (wSpeed w))
       , txt env 20 62 16 white ("Q: round to scale " ++ (if wQuant w then "ON" else "off") ++ "   Space: " ++ (if wPaused w then "resume" else "pause") ++ "   C: clear")
       , txt env 20 86 14 g "orange = turns left (triangle wave)   blue = turns right (sine wave)"
       , txt env 20 106 14 g ("sharper curve = higher pitch (up to " ++ nf 0 octaves ++ " octaves)   yellow ring = osculating circle")
       ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let m = fromG env pos
        w1 = w { wAudio = True }
    in if snd m < envH env - 80 then w1 { wDrawing = True, wRaw = [m] } else w1
  EventKey (MouseButton LeftButton) Up _ _ | wDrawing w -> finish w { wDrawing = False }
  EventMotion pos | wDrawing w -> case wRaw w of
    (l : _) | dist2 (fromG env pos) l > 3 -> w { wRaw = take 4000 (fromG env pos : wRaw w) }
    _ -> w
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> setPreset env (fromEnum c - fromEnum '0') w { wAudio = True }
    | c == 'q' || c == 'Q' -> w { wQuant = not (wQuant w), wAudio = True }
    | c == 'c' || c == 'C' -> w { wPts = U.empty, wKappa = U.empty, wHead = 0, wAudio = True }
    | otherwise -> w { wAudio = True }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w), wAudio = True }
  EventKey (SpecialKey _) Down _ _ -> w { wAudio = True }
  _ -> w

-- 描き終わったら：なめらかにして、打ち直して、曲率を計算する
finish :: World -> World
finish w =
  let raw = reverse (wRaw w)
  in case (raw, wRaw w) of
       (a : _, l : _) | length raw >= 12 ->
         let closed = dist2 a l < 60
         in setCurve closed (smoothPoly 2 closed (resample gridH closed (smoothPoly 3 closed raw))) w
       _ -> w

-- ---------- やってみよう ----------
-- 1. まっすぐ描くと低いドローン、小さい渦を描くと高い音。「音の高さ」で曲線の形を当てるゲームをしよう
-- 2. baseFreq や octaves、pentatonic（音階）を変えてみよう。pentatonic = [0, 3, 5, 7, 10] は短調っぽい
-- 3. notePeriod を小さくすると音が細かくなる、大きくするとゆったりする。曲率の符号で波形を Square に変えると？
-- 4. 02 の縮閉線や 10 の曲線短縮流（丸くなる = 音が一定になる）と合体してみよう

-- ==================================================
-- ここから下は「曲線の道具」です（等間隔に打ち直す・なめらかにする・曲率を差分で求める）
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

-- | 3 点平均を n 回かけてなめらかにする（開いた曲線は端を固定）
smoothPoly :: Int -> Bool -> [Point] -> [Point]
smoothPoly n isClosed p0 = U.toList (iterate once (U.fromList p0) !! max 0 n)
  where
    once q =
      let m = U.length q
      in U.generate m $ \i ->
           if not isClosed && (i == 0 || i == m - 1) then q U.! i else
           let (ax, ay) = q U.! ((i - 1 + m) `mod` m)
               (bx, by) = q U.! ((i + 1) `mod` m)
               (x, y) = q U.! i
           in ((ax + 2 * x + bx) / 4, (ay + 2 * y + by) / 4)

-- | 等間隔の点列の曲率（中心差分）。p5.js の座標は y が下向きなので符号を反転して「左曲がり = +」にする
curvatureOf :: Float -> Bool -> U.Vector Point -> U.Vector Float
curvatureOf h isClosed p = U.generate n at'
  where
    n = U.length p
    at' i =
      let im = if isClosed then (i - 1 + n) `mod` n else max (i - 1) 0
          ip = if isClosed then (i + 1) `mod` n else min (i + 1) (n - 1)
          (xm, ym) = p U.! im
          (xp, yp) = p U.! ip
          (x, y) = p U.! i
          x1 = (xp - xm) / (2 * h)
          y1 = (yp - ym) / (2 * h)
          x2 = (xp - 2 * x + xm) / (h * h)
          y2 = (yp - 2 * y + ym) / (h * h)
          s = x1 * x1 + y1 * y1
          den = s * sqrt s
      in if den > 1e-9 then negate (x1 * y2 - y1 * x2) / den else 0

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
