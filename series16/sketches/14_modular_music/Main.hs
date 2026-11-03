-- ============================================
-- 14 累乗の音楽：gᵏ mod p を演奏する（自由制作のベース）
-- 1, g, g², g³, … を素数 p で割った余りは、いつか 1 に戻ってくり返す
-- g が原始根なら 1 〜 p−1 を全部まわってから戻る（最長のメロディ）。そうでなければ短いループ
-- 余りの数を音階の音に割り当てて鳴らす。円の上の線は、いま鳴っている数の動き
-- スペース：再生・停止 / ←→：素数 p / ↑↓：g / 1〜3：音階 / V：2 声目（g² の旋律） / B：ベース
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, when)
import Data.Char (digitToInt, toLower)
import Data.List (intercalate)
import qualified Data.IntSet as IS

tempo :: Float
tempo = 0.18          -- ★ 1 音の長さ（秒）

waveType :: Wave
waveType = Triangle   -- ★ 音色：Sine Square Sawtooth Triangle

primes :: [Int]
primes = [5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43]

-- ★ ペンタトニック・長音階・都節
scales :: Int -> [Int]
scales 1 = [0, 2, 4, 7, 9]
scales 2 = [0, 2, 4, 5, 7, 9, 11]
scales _ = [0, 1, 5, 7, 8]

scaleName :: Int -> String
scaleName 1 = "pentatonic"
scaleName 2 = "major"
scaleName _ = "miyako-bushi"

data World = World
  { wPi      :: !Int
  , wG       :: !Int
  , wScale   :: !Int
  , wPlaying :: !Bool
  , wVoice2  :: !Bool
  , wBass    :: !Bool
  , wK       :: !Int
  , wAcc     :: !Float
  , wTrail   :: ![Int]   -- 鳴った数（新しいものが後ろ、最大 40）
  , wFlash   :: !Float
  , wSeq     :: ![Int]
  }

main :: IO ()
main = runSketch (sketch "16-14 Modular Music" (rebuild (World 5 3 1 False False True 0 0 [] 0 [])))
  { skBack  = hsb 250 50 8
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

primeOf :: World -> Int
primeOf w = case drop (wPi w) primes of
  (p : _) -> p
  [] -> 43

-- 1, g, g², … を 1 に戻るまで
rebuild :: World -> World
rebuild w =
  let p = primeOf w
      g = max 2 (min (p - 1) (wG w))
      go v acc
        | v == 1 && not (null acc) = reverse acc
        | length acc >= p = reverse acc
        | otherwise = go (v * g `mod` p) (v : acc)
  in w { wG = g, wSeq = go 1 [], wK = 0, wTrail = [] }

isPrimitive :: Int -> Int -> Bool
isPrimitive x p = order 1 (x `mod` p) == p - 1
  where
    order c v
      | v == 1 = c
      | c > p = -1
      | otherwise = order (c + 1 :: Int) (v * x `mod` p)

-- 3 オクターブの中に収める
noteOf :: Int -> Int -> Float
noteOf sc v =
  let s = scales sc
      len = length s
      i = (v - 1) `mod` (len * 3)
      semi = (case drop (i `mod` len) s of
                (x : _) -> x
                [] -> 0) + 12 * (i `div` len)
  in 220 * 2 ** (fromIntegral semi / 12)

-- 時間がたったら次の音を鳴らす
stepW :: Env -> Float -> World -> IO World
stepW _ dt w0 = do
  let w1 = w0 { wFlash = wFlash w0 * 0.9 }
  if not (wPlaying w1) then pure w1 else do
    let acc = wAcc w1 + dt
        count = floor (acc / tempo) :: Int
        w2 = w1 { wAcc = acc - fromIntegral count * tempo }
    foldM (\w _ -> playOne w) w2 [1 .. min 4 count]

playOne :: World -> IO World
playOne w = do
  let sq = wSeq w
      len = max 1 (length sq)
      v = case drop (wK w `mod` len) sq of
        (x : _) -> x
        [] -> 1
      p = primeOf w
      v2 = let r = v * v `mod` p in if r == 0 then 1 else r
  playTone (noteOf (wScale w) v) 0.22 (tempo * 1.6) waveType
  when (wVoice2 w) (playTone (noteOf (wScale w) v2 * 0.5) 0.12 (tempo * 1.4) Sine)
  when (wBass w && wK w `mod` len == 0) (playTone 55 0.35 (tempo * fromIntegral len * 0.9) Sine)
  let tr = wTrail w ++ [v]
  pure w { wK = wK w + 1, wFlash = 1, wTrail = drop (length tr - 40) tr }

drawW :: Env -> World -> Picture
drawW env w =
  let p = primeOf w
      pf = fromIntegral p :: Float
      g = wG w
      sq = wSeq w
      prim = isPrimitive g p
      cx = envW env * 0.36
      cy = envH env * 0.56
      r = min (envW env * 0.27) (envH env * 0.34)
      posOf v = let t = -pi / 2 + tau * fromIntegral (v - 1) / (pf - 1) in (cx + cos t * r, cy + sin t * r)
      tr = wTrail w
      nt = fromIntegral (length tr) :: Float
      -- 軌跡
      trailP = [ let age = fromIntegral i / nt
                 in seg env (hsba (fromIntegral b * 360 / pf) 70 100 (age * 0.9)) (1 + age * 4) (posOf a) (posOf b)
               | (i, a, b) <- zip3 [1 :: Int ..] tr (drop 1 tr) ]
      inSeq = IS.fromList sq
      lastV = foldl (\_ x -> x) (-1) tr
      -- 数の丸
      numR = max 14 (min 24 (r * 2.4 / pf))
      circles = [ let (x, y) = posOf v
                      rr = numR * (if v == lastV then 1 + wFlash w * 0.8 else 1)
                      on = IS.member v inSeq
                  in Pictures
                       [ dot env (if on then hsb (fromIntegral v * 360 / pf) 65 95 else hsb 0 0 25) x y (rr * 2)
                       , txtC env x y (max 14 (numR * 0.8)) (hsb 0 0 (if on then 8 else 70)) (show v) ]
                | v <- [1 .. p - 1] ]
      px = envW env * 0.68
      pw = envW env - px - 30
      cs = min 34 (pw / 8)
      y0 = 150
      seqCell (i, v) =
        let x = px + fromIntegral (i `mod` 8) * cs
            y = y0 + fromIntegral (i `div` 8) * cs
            on = wPlaying w && (wK w - 1) `mod` max 1 (length sq) == i
        in Pictures
             [ box env (if on then hsb 50 90 100 else hsb (fromIntegral v * 360 / pf) 50 60) x y (cs - 3) (cs - 3)
             , txtC env (x + cs / 2 - 1) (y + cs / 2 - 1) 14 (hsb 0 0 8) (show v) ]
      rowsN = (length sq + 7) `div` 8
      y1 = y0 + fromIntegral rowsN * cs + 20
      prims = [x | x <- [2 .. p - 1], isPrimitive x p]
      divisors m = [x | x <- [1 .. m], m `mod` x == 0]
      list = intercalate ", " . map show
  in Pictures
       [ txt env 30 50 26 (hsb 0 0 92) ("Music of powers: " ++ show g ++ "^k mod " ++ show p)
       , txt env 30 80 15 (hsb 0 0 60) ("Space: " ++ (if wPlaying w then "stop" else "play") ++ "  L/R: p = " ++ show p ++ "  Up/Down: g = " ++ show g
                                         ++ "  1-3: scale (" ++ scaleName (wScale w) ++ ")  V: voice 2 " ++ onOff (wVoice2 w) ++ "  B: bass " ++ onOff (wBass w))
       , Pictures trailP
       , Pictures circles
       , txtC env cx (cy - 12) 22 (if prim then hsb 50 80 100 else hsb 0 0 80)
           (if prim then "Primitive root!" else "Loop of " ++ show (length sq))
       , txtC env cx (cy + 16) 14 (hsb 0 0 65) ("order = " ++ show (length sq))
       , txtT env px 120 17 (hsb 0 0 90) "Sequence (until back to 1)"
       , Pictures (map seqCell (zip [0 ..] sq))
       , stack env px y1 pw
           [ (15, hsb 0 0 85, 10, "Primitive roots mod " ++ show p ++ ": " ++ list prims)
           , (15, hsb 0 0 85, 10, "The order always divides p-1 = " ++ show (p - 1) ++ " (" ++ list (divisors (p - 1)) ++ ")")
           , (14, hsb 0 0 60, 0, "Discrete log: finding k with g^k mod p = target is hard, though powers are fast. Diffie-Hellman and elliptic curve crypto use this one-way street.\n\nMake your own: change the timbre, tempo, scales and voice 2 rule marked with a star.")
           ]
       ]

onOff :: Bool -> String
onOff b = if b then "ON" else "OFF"

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w), wAcc = tempo }
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild w { wPi = min (length primes - 1) (wPi w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild w { wPi = max 0 (wPi w - 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild w { wG = if wG w + 1 >= primeOf w then 2 else wG w + 1 }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild w { wG = if wG w - 1 < 2 then primeOf w - 1 else wG w - 1 }
  EventKey (Char c0) Down _ _ -> case toLower c0 of
    c | c `elem` "123" -> w { wScale = digitToInt c }
    'v' -> w { wVoice2 = not (wVoice2 w) }
    'b' -> w { wBass = not (wBass w) }
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. p = 13 で g = 2（原始根）と g = 3（位数 3）を聞き比べる
-- 2. g = p−1 にすると？（位数 2：1 と p−1 を行ったり来たり）
-- 3. 2 声目を g² ではなく「v + 5 mod p」にするなど、自分のルールで伴奏を作る

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
