-- ============================================
-- 46-14 群れの音楽 🔊
-- 1 = ホタルの合奏：24 匹のホタルがそれぞれ自分の音を持つ。光ったら鳴る。パルス結合（02 と同じ）で同期が進むと、バラバラの音が和音のリズムにそろう
-- 2 = 粒子群の旋律：PSO（05 と同じ）を 1 回まわすごとに、粒子の高さ（y）で音程を決めてアルペジオで鳴らす。群れが答えに集まると音もそろう
-- 操作：1・2 = モード / ↑↓ = 結合 ε（1）・テンポ（2）/ R = やり直し / M = 消音（クリックしなくても音が出る）
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Data.Bits (shiftL, shiftR, xor)
import Data.List (foldl', mapAccumL, sortOn)
import Data.Word (Word64)
import Numeric (showEFloat, showFFloat)

volume :: Float
volume = 0.18                -- ★ 音の大きさ（0〜0.4）

-- 五音音階（ラ ド レ ミ ソ）を 3 オクターブ
scaleSteps :: [Int]
scaleSteps = [0, 3, 5, 7, 10]

scaleFreq :: Int -> Float
scaleFreq k = let (o, s) = k `divMod` 5 in 220 * 2 ** (fromIntegral o + fromIntegral (scaleSteps !! s) / 12)

-- ---- モード 1：ホタル
nFF :: Int
nFF = 24                     -- ★ ホタルの数

curveB, period :: Float
curveB = 3                   -- ★ 曲がり具合
period = 1.6                 -- ★ 周期（秒）

-- ---- モード 2：PSO（Rastrigin）
nP :: Int
nP = 10                      -- ★ 粒子の数

wIn, c1, c2, lo, hi :: Float
wIn = 0.72                   -- ★ PSO の係数（慣性）
c1 = 1.49                    -- ★ 自分の最良点へ引く強さ
c2 = 1.49                    -- ★ 群れの最良点へ引く強さ
lo = -5.12
hi = 5.12

data Fly = Fly { fPh :: !Float, fGlow :: !Float, fNote :: !Int }

data Part = Part { pX, pY, pVx, pVy, pBx, pBy, pBv :: !Float }

data Ripple = Ripple !Float !Float !Float !Float !Float    -- x, y, 半径, 濃さ, 色相

-- 鳴らす予定の音：時刻、周波数、音量、長さ、波形
data Pending = Pending !Float !Float !Float !Float !Wave

data World = World
  { wMode     :: !Int
  , wMuted    :: !Bool
  , wEps      :: !Float            -- 結合 ε
  , wFlies    :: ![Fly]
  , wRHist    :: ![Float]          -- 新しい順、300 個まで
  , wNow      :: !Int              -- 同時に鳴った数
  , wParts    :: ![Part]
  , wGBest    :: !(Float, Float)
  , wGBestVal :: !Float
  , wIter     :: !Int
  , wBestHist :: ![Float]
  , wBeat     :: !Int              -- 1 回の PSO を何フレームごとに行うか（テンポ）
  , wRipples  :: ![Ripple]
  , wPending  :: ![Pending]
  , wSeed     :: !Seed
  }

main :: IO ()
main = runSketch (sketch "46-14 Swarm music" (start 1))
  { skBack  = hsb 265 45 10
  , skStart = \_ -> start <$> randomRIO (1, 2 ^ (62 :: Int))
  , skStep  = stepIO
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

start :: Seed -> World
start s = resetPSO (resetFireflies (World 1 False 0.06 [] [] 0 [] (0, 0) 1e30 0 [] 24 [] [] s))

resetFireflies :: World -> World
resetFireflies w = w { wFlies = [ Fly (hashRand (wSeed w) i) 0 (i `mod` 15) | i <- [0 .. nFF - 1] ], wRHist = [], wSeed = nextSeed (wSeed w) }

rastrigin :: Float -> Float -> Float
rastrigin x y = 20 + x * x - 10 * cos (tau * x) + y * y - 10 * cos (tau * y)

resetPSO :: World -> World
resetPSO w = w { wParts = ps, wGBest = g, wGBestVal = gv, wIter = 0, wBestHist = [], wSeed = nextSeed s }
  where
    s = wSeed w
    ps = [ Part x y 0 0 x y (rastrigin x y) | i <- [0 .. nP - 1]
         , let x = lo + hashRand s (2 * i) * (hi - lo), let y = lo + hashRand s (2 * i + 1) * (hi - lo) ]
    (g, gv) = foldl' (\(gb, bv) p -> if pBv p < bv then ((pBx p, pBy p), pBv p) else (gb, bv)) ((0, 0), 1e30) ps

fX :: Float -> Float
fX ph = log (1 + (exp curveB - 1) * ph) / curveB

fInv :: Float -> Float
fInv x = (exp (curveB * x) - 1) / (exp curveB - 1)

-- 円の上のホタル i の位置
flyPos :: Env -> Int -> Point
flyPos env i = let (cx, cy, rr) = circleLayout env
                   a = fromIntegral i / fromIntegral nFF * tau
               in (cx + rr * cos a, cy + rr * sin a)

circleLayout :: Env -> (Float, Float, Float)
circleLayout env = (envW env * 0.36, envH env * 0.55, min (envW env) (envH env) * 0.3)

-- 音を鳴らすところだけ IO。残りは純粋な step
stepIO :: Env -> Float -> World -> IO World
stepIO env _ w0 = do
  let w1 = step env w0
      t = envTime env
      (due, later) = foldr (\p@(Pending tp _ _ _ _) (a, b) -> if tp <= t then (p : a, b) else (a, p : b)) ([], []) (wPending w1)
  forM_ due $ \(Pending _ f v d wv) -> playTone f v d wv
  pure w1 { wPending = later }

step :: Env -> World -> World
step env w0 = w1 { wRipples = [ Ripple x y (r + 4) (a * 0.93) h | Ripple x y r a h <- wRipples w1, a * 0.93 > 0.03 ] }
  where w1 = if wMode w0 == 1 then stepFlies env w0 else stepPSO env w0

-- ★ パルス結合（全員が全員を見る）
stepFlies :: Env -> World -> World
stepFlies env w = w { wFlies = flies', wRHist = take 300 (r : wRHist w), wNow = length now
                    , wRipples = wRipples w ++ ripples, wPending = wPending w ++ notes }
  where
    advanced = [ f { fPh = fPh f + 1 / 60 / period } | f <- wFlies w ]
    phs0 = map fPh advanced
    fired0 = reverse [ i | (i, p) <- zip [0 ..] phs0, p >= 1 ]
    (phs, done) = cascade fired0 phs0 (replicate nFF False)
    cascade [] ps ds = (ps, ds)
    cascade (i : rest) ps ds
      | ds !! i = cascade rest ps ds
      | otherwise =
          let ds' = setAt i True ds
              ps1 = setAt i 0 ps
              (pushed, ps2) = foldl' push ([], ps1) (zip [0 ..] ds')
              push (acc, ps') (j, dj)
                | dj = (acc, ps')
                | otherwise = let x = fX (ps' !! j) + wEps w
                              in if x >= 1 then (j : acc, ps') else (acc, setAt j (fInv x) ps')
          in cascade (pushed ++ rest) ps2 ds'
    flies' = [ f { fPh = p, fGlow = if d then 1 else fGlow f * 0.88 } | (f, p, d) <- zip3 advanced phs done ]
    -- 同時に光ったホタルの音をまとめて鳴らす（多すぎるときは音量を下げる）
    now = [ i | (i, d) <- zip [0 ..] done, d ]
    vol = if wMuted w then 0 else volume / sqrt (max 1 (fromIntegral (length now)))
    t = envTime env
    notes = [ Pending t (scaleFreq (fNote (flies' !! i))) vol 0.35 Triangle | i <- take 8 now, vol > 0 ]
    ripples = [ Ripple x y 10 0.8 (50 + fromIntegral (fNote (flies' !! i)) * 15) | i <- take 8 now, let (x, y) = flyPos env i ]
    sx = sum [ cos (p * tau) | p <- phs ]
    sy = sum [ sin (p * tau) | p <- phs ]
    r = sqrt (sx * sx + sy * sy) / fromIntegral nFF

-- ★ PSO を 1 回まわす（beat フレームごと）
stepPSO :: Env -> World -> World
stepPSO env w
  | envFrame env `mod` wBeat w /= 0 = w
  | otherwise = w { wParts = ps', wGBest = g, wGBestVal = gv, wIter = wIter w + 1, wSeed = s'
                  , wBestHist = take 300 (logBase 10 (max gv 1e-12) : wBestHist w), wPending = wPending w ++ notes }
  where
    (gx, gy) = wGBest w
    (s', ps') = mapAccumL move (wSeed w) (wParts w)
    move s p =
      let (r1, sA) = rand01 s
          (r2, sB) = rand01 sA
          vx = clampF (-2) 2 (wIn * pVx p + c1 * r1 * (pBx p - pX p) + c2 * r2 * (gx - pX p))
          vy = clampF (-2) 2 (wIn * pVy p + c1 * r1 * (pBy p - pY p) + c2 * r2 * (gy - pY p))
          x = clampF lo hi (pX p + vx)
          y = clampF lo hi (pY p + vy)
          v = rastrigin x y
      in (sB, if v < pBv p then Part x y vx vy x y v else p { pX = x, pY = y, pVx = vx, pVy = vy })
    (g, gv) = foldl' (\(gb, bv) p -> if pBv p < bv then ((pBx p, pBy p), pBv p) else (gb, bv)) (wGBest w, wGBestVal w) ps'
    -- 粒子の高さ y → 音程（上ほど高い）。左の粒子から順にアルペジオ
    t = envTime env
    notes = [ Pending (t + fromIntegral k * 0.06) (scaleFreq (noteOf p)) (volume * 0.6) 0.25 Sine
            | not (wMuted w), (k, p) <- zip [0 :: Int ..] (sortOn pX ps') ]

noteOf :: Part -> Int
noteOf p = round ((pY p - lo) / (hi - lo) * 14)

draw :: Env -> World -> Picture
draw env w = view env $
  (if wMode w == 1 then drawFlies env w else drawPSO env w)
  ++ [ Color (hsbA h 70 100 (a * 100)) (Translate x y (ring (r / 2) 2)) | Ripple x y r a h <- wRipples w ]
  ++ [ txt 20 36 18 (hsb 0 0 100) ("1: firefly ensemble   2: swarm melody   Up/Down: "
                                   ++ (if wMode w == 1 then "coupling eps = " ++ fmt 3 (wEps w) else "tempo (" ++ show (wBeat w) ++ " frames / step)")
                                   ++ "   R: restart   M: " ++ (if wMuted w then "muted" else "sound on"))
     , txt 20 60 14 (hsb 0 0 70) "Sound is on (no click needed)" ]

drawFlies :: Env -> World -> [Picture]
drawFlies env w =
  concat [ [ dot (hsb (50 + fromIntegral (fNote f) * 15) 70 (30 + 70 * g)) x y (16 + 22 * g)
           , txt (x - 14) (y + 32) 14 (hsb 0 0 80) (show (round (scaleFreq (fNote f)) :: Int))
             -- 位相の針
           , seg 1 (hsbA 0 0 100 40) (cx, cy) (cx + 0.3 * rr * cos (fPh f * tau), cy + 0.3 * rr * sin (fPh f * tau)) ]
         | (i, f) <- zip [0 ..] (wFlies w), let (x, y) = flyPos env i, let g = fGlow f ]
  ++ [ drawPlot (wRHist w) px 120 320 100 0 1 (hsb 50 80 100) ("Sync r = " ++ fmt 3 r ++ " (1 = all together)")
     , txt px 260 14 (hsb 0 0 75) ("Notes sounding together: " ++ show (wNow w))
     , txt px 290 14 (hsb 0 0 75) "Centre needles = each phase. When they"
     , txt px 312 14 (hsb 0 0 75) "line up, the 24 notes sound as one chord"
     , txt px 342 14 (hsb 0 0 75) "eps = 0 keeps them apart (polyrhythm)" ]
  where
    (cx, cy, rr) = circleLayout env
    px = envW env - 360
    r = case wRHist w of
          (v : _) -> v
          [] -> 0

drawPSO :: Env -> World -> [Picture]
drawPSO env w =
  [ Color (hsbA 0 0 0 30) (rect (ox + sz / 2) (oy + sz / 2) sz sz) ]
  ++ [ seg 1 (hsbA 0 0 100 10) (ox, y) (ox + sz, y) | k <- [0 .. 14 :: Int], let y = oy + sz - fromIntegral k / 14 * sz ]   -- 音程の線
  ++ [ txt (ox + sz + 6) (oy + sz - fromIntegral k / 14 * sz + 5) 14 (hsb 0 0 60) (show (round (scaleFreq k) :: Int) ++ " Hz") | k <- [0, 5, 10] ]
  ++ [ dot (hsb (50 + fromIntegral (noteOf p) * 15) 70 100) (sxF (pX p)) (syF (pY p)) 14 | p <- wParts w ]
  ++ [ dot (hsb 0 90 100) (sxF (fst (wGBest w))) (syF (snd (wGBest w))) 8
     , Color (hsb 120 70 100) (Translate (sxF 0) (syF 0) (ring 10 2))
     , drawPlot (wBestHist w) gx 120 (envW env - gx - 30) 120 (-12) 2 (hsb 0 85 100) "Rastrigin best log10 f (true minimum 0 = green ring)"
     , txt gx 280 15 (hsb 0 0 90) ("Iteration " ++ show (wIter w) ++ ", best f = "
                                   ++ (if wGBestVal w < 1e-4 then showEFloat (Just 2) (wGBestVal w) "" else fmt 4 (wGBestVal w)))
     , txt gx 312 14 (hsb 0 0 70) "Particle height = pitch (each line = one pentatonic note)"
     , txt gx 336 14 (hsb 0 0 70) "When the swarm gathers, all 10 notes match"
     , txt gx 360 14 (hsb 0 0 70) "Particles stuck in a local minimum keep a different note" ]
  where
    sz = min (envH env - 120) (envW env * 0.55)
    ox = 30
    oy = 90
    sxF x = ox + (x - lo) / (hi - lo) * sz
    syF y = oy + sz - (y - lo) / (hi - lo) * sz
    gx = ox + sz + 70

event :: Event -> World -> World
event ev w = case ev of
  EventKey (Char '1') Down _ _ -> w { wMode = 1 }
  EventKey (Char '2') Down _ _ -> w { wMode = 2 }
  EventKey (SpecialKey KeyUp) Down _ _
    | wMode w == 1 -> w { wEps = min 0.3 (wEps w + 0.01) }
    | otherwise -> w { wBeat = max 8 (wBeat w - 4) }
  EventKey (SpecialKey KeyDown) Down _ _
    | wMode w == 1 -> w { wEps = max 0 (wEps w - 0.01) }
    | otherwise -> w { wBeat = min 60 (wBeat w + 4) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> if wMode w == 1 then resetFireflies w else resetPSO w
    | c `elem` "mM" -> w { wMuted = not (wMuted w), wPending = [] }
  _ -> w

-- ---------- やってみよう ----------
-- 1. モード 1 で ε = 0 にするとバラバラな音（でも周期は同じなので、ずっと同じパターン）。ε を上げると何秒でそろう？
-- 2. period を 0.5 にするとテンポが速くなる。scaleSteps を [0, 2, 4, 7, 9]（ド レ ミ ソ ラ）に変えると明るい響きに
-- 3. モード 2 で R を何回か。答え（0）に集まると同じ音になる。途中で止まると 2 つの音が残る（局所解の音）
-- 4. wIn（慣性）を 0.95 にすると、粒子がなかなか止まらず、音もいつまでも動き回る
-- 5. Pending の Sine を Square や Sawtooth に変えて、音色を比べよう

-- ---------- 道具 ----------
-- リストの i 番目を v に置きかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [ if k == i then v else x | (k, x) <- zip [0 ..] xs ]

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
drawPlot hist x y w h lo' hi' c title = Pictures
  [ Color (hsbA 0 0 0 35) (rect (x + w / 2) (y + h / 2) w h)
  , txt x (y - 8) 14 (hsb 0 0 85) title
  , if length hist < 2 then Blank
    else path 2 c [ (x + fromIntegral i / 299 * w, y + h - clampF 0 1 ((v - lo') / (hi' - lo')) * h)
                  | (i, v) <- zip [0 :: Int ..] (reverse hist) ] ]

-- 乱数（xorshift64）。種を World に入れておけば skStep は純粋なまま
type Seed = Word64

nextSeed :: Seed -> Seed
nextSeed s0 = let s1 = s0 `xor` (s0 `shiftL` 13)
                  s2 = s1 `xor` (s1 `shiftR` 7)
              in s2 `xor` (s2 `shiftL` 17)

-- 0〜1 の乱数と次の種
rand01 :: Seed -> (Float, Seed)
rand01 s = let s' = nextSeed s in (fromIntegral (s' `shiftR` 40) / 16777216, s')

-- 種 s と番号 i から決まる 0〜1 の乱数（たくさんの粒子に一度に配るとき用）
hashRand :: Seed -> Int -> Float
hashRand s i =
  let z0 = s + fromIntegral i * 0x9E3779B97F4A7C15
      z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
      z3 = z2 `xor` (z2 `shiftR` 31)
  in fromIntegral (z3 `shiftR` 40) / 16777216
