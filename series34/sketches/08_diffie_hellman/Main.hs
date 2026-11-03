-- ============================================
-- 08 ディフィー＝ヘルマン鍵共有
-- 会ったことのない 2 人が、盗み聞きされている通信だけで「同じ秘密の鍵」をつくる方法。
--   絵の具：混ぜるのは簡単、混ざった色から元の色を取り出すのは難しい
--   数：A = g^a mod p は計算が速い。A から a を探す（離散対数）のは総当たりしかない
-- ↑↓ = 素数 p の大きさ / A・B = アリス・ボブの秘密を選び直す / スペース = イブの総当たり開始・停止 / ←→ = イブの速さ
-- ※ 教育用（実際は 2048 ビット以上の p か楕円曲線を使う）
-- ============================================
module Main (main) where

import Art

primes :: [Int]
primes = [23, 101, 1019, 10007, 100003, 999983]   -- ★ 選べる素数 p

-- 絵の具（RGB、0〜255）
type Paint = (Float, Float, Float)

common :: Paint
common = (250, 220, 60)

data World = World
  { pIdx     :: !Int
  , pP       :: !Int
  , gG       :: !Int
  , secA     :: !Int
  , secB     :: !Int
  , pubA     :: !Int
  , pubB     :: !Int
  , shared   :: !Int
  , eveX     :: !Int
  , eveRun   :: !Bool
  , eveFound :: !Int      -- -1 = まだ、0 = 見つからず、正 = 見つけた a
  , eveSpeed :: !Int      -- ★ イブが 1 フレームに試す回数
  , paintA   :: !Paint
  , paintB   :: !Paint
  }

main :: IO ()
main = do
  w0 <- newParams 1 (World 1 23 5 2 2 1 1 1 0 False (-1) 20 common common)
  runSketch (sketch "34-08 Diffie-Hellman Key Exchange" w0)
    { skBack  = rgb255 18 16 22
    , skStep  = \_ _ w -> pure (eveStep w)
    , skDraw  = drawWorld
    , skEvent = onEvent
    }

newParams :: Int -> World -> IO World
newParams i w =
  let p = case drop i primes of { (v : _) -> v; [] -> 23 }
  in newSecrets w { pIdx = i, pP = p, gG = primitiveRoot p }

newSecrets :: World -> IO World
newSecrets w = do
  let p = pP w
      g = gG w
  a <- (2 +) <$> randomRIO (0, p - 4)           -- ★ アリスの秘密（誰にも送らない）
  b <- (2 +) <$> randomRIO (0, p - 4)           -- ★ ボブの秘密
  ar <- randomRIO (0, 99)
  ag <- randomRIO (0, 79)
  ab <- randomRIO (0, 119)
  br <- randomRIO (0, 79)
  bg <- randomRIO (0, 139)
  bb <- randomRIO (0, 99)
  let pa = modPow g a p                         -- 公開してよい値
      pb = modPow g b p
  pure w { secA = a, secB = b, pubA = pa, pubB = pb
         , shared = modPow pb a p               -- アリスの計算。ボブは modPow pa b p で同じ値になる
         , paintA = (150 + fi ar, fi ag, fi ab)
         , paintB = (fi br, fi bg, 150 + fi bb)
         , eveX = 0, eveFound = -1 }
  where fi = fromIntegral :: Int -> Float

-- イブの総当たり：g^x = pubA となる x を 1 から順に探す
eveStep :: World -> World
eveStep w
  | eveRun w && eveFound w < 0 = go (eveSpeed w) (eveX w)
  | otherwise = w
  where
    go :: Int -> Int -> World
    go 0 x = w { eveX = x }
    go n x =
      let x' = x + 1
      in if modPow (gG w) x' (pP w) == pubA w then w { eveX = x', eveFound = x' }
         else if x' >= pP w then w { eveX = x', eveFound = 0 }
         else go (n - 1) x'

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (channel : header ++ panels ++ paints ++ numbers ++ eve ++ result)
  where
    sw = envW env
    sh = envH env
    cw = sw / 3
    top = 90
    p = pP w
    bobKey = modPow (pubA w) (secB w) p
    mixA = mixPaint common (paintA w)
    mixB = mixPaint common (paintB w)
    good = rgb255 120 230 150
    amber = rgb255 255 200 90
    header =
      [ txt env 24 34 18 (gray 0.94)
          ("Diffie-Hellman  Up/Down p  A/B new secrets  Space Eve brute force  Left/Right Eve speed " ++ show (eveSpeed w) ++ "/frame")
      , txt env 24 56 14 (gray 0.6) "Gray band = the public channel everyone sees (Eve too)  | educational"
      ]
    -- 中央の通信路
    channel = box env (cw + 10) (top + 30) (cw - 20) (sh - top - 60) (rgba255 255 255 255 14)
    -- 3 人のパネル
    panels =
      [ txtC env (cw * 0.5) (top + 10) 20 (rgb255 255 140 160) "Alice"
      , txtC env (cw * 1.5) (top + 10) 20 (gray 0.7) "Eve (eavesdropper)"
      , txtC env (cw * 2.5) (top + 10) 20 (rgb255 130 180 255) "Bob"
      ]
    -- ---- 絵の具の段
    py = top + 70
    paints =
      [ caption env (cw * 0.5) (py - 16) "Common color + secret color"
      , blob env (cw * 0.25) (py + 30) common False 40
      , blob env (cw * 0.5) (py + 30) (paintA w) True 40
      , blob env (cw * 0.75) (py + 30) mixA False 40
      , blob env (cw * 2.25) (py + 30) mixB False 40
      , blob env (cw * 2.5) (py + 30) (paintB w) True 40
      , blob env (cw * 2.75) (py + 30) common False 40
      , caption env (cw * 1.5) (py - 16) "Visible: common color, 2 mixed colors"
      , blob env (cw * 1.3) (py + 30) common False 40
      , blob env (cw * 1.5) (py + 30) mixA False 40
      , blob env (cw * 1.7) (py + 30) mixB False 40
      , caption env (cw * 0.5) (py + 90) "Other's mix + my secret"
      , blob env (cw * 0.5) (py + 130) (mixPaint mixB (paintA w)) False 54
      , caption env (cw * 2.5) (py + 90) "Other's mix + my secret"
      , blob env (cw * 2.5) (py + 130) (mixPaint mixA (paintB w)) False 54
      , caption env (cw * 1.5) (py + 120) "Mixing these never gives both secrets"
      ]
    -- ---- 数の段
    ny = py + 220
    linesA = [ "Secret a = " ++ show (secA w), "A = g^a mod p", "  = " ++ show (pubA w) ++ "  -> send"
             , "Received B = " ++ show (pubB w), "B^a mod p = " ++ show (shared w) ]
    linesB = [ "Secret b = " ++ show (secB w), "B = g^b mod p", "  = " ++ show (pubB w) ++ "  -> send"
             , "Received A = " ++ show (pubA w), "A^b mod p = " ++ show bobKey ]
    lineCol :: Int -> Color
    lineCol i | i == 0 = amber
              | i == 4 = good
              | otherwise = gray 0.86
    numbers =
      txtC env (sw / 2) (ny - 20) 17 (gray 0.9) ("Public: p = " ++ show p ++ "   g = " ++ show (gG w))
      : concat
        [ [ txt env 30 (ny + 10 + fromIntegral i * 26) 16 (lineCol i) a
          , txt env (cw * 2 + 30) (ny + 10 + fromIntegral i * 26) 16 (lineCol i) b ]
        | (i, a, b) <- zip3 [0 ..] linesA linesB ]
    -- イブの欄
    ex = cw + 24
    progress = min 1 (fromIntegral (eveX w) / fromIntegral (max 1 (p - 1))) :: Float
    eve =
      [ txt env ex (ny + 10) 14 (gray 0.78) ("Visible: p, g, A = " ++ show (pubA w) ++ ", B = " ++ show (pubB w))
      , txt env ex (ny + 36) 14 (gray 0.78) "Brute force x with g^x mod p = A"
      , txt env ex (ny + 62) 15 (if eveFound w > 0 then rgb255 255 110 110 else amber)
          (if eveFound w > 0
             then "Found! a = " ++ show (eveFound w) ++ " (" ++ show (eveX w) ++ " tries)"
             else "Tried x: " ++ show (eveX w) ++ " / " ++ show (p - 1) ++ (if eveRun w then " ..." else " (Space to start)"))
      , box env ex (ny + 76) (cw - 48) 12 (gray 0.24)
      , box env ex (ny + 76) ((cw - 48) * progress) 12 amber
      , txt env ex (ny + 112) 14 (gray 0.6) "10x bigger p -> 10x worst-case work."
      , txt env ex (ny + 134) 14 (gray 0.6) "Real p has 600+ digits:"
      , txt env ex (ny + 156) 14 (gray 0.6) "no computer in the universe could finish"
      ]
    result =
      [ txt env 30 (sh - 30) 17 good
          ("Shared key: " ++ show (shared w) ++ (if shared w == bobKey then " (both the same: OK)" else "")) ]

caption :: Env -> Float -> Float -> String -> Picture
caption env x y = txtC env x y 14 (gray 0.67)

blob :: Env -> Float -> Float -> Paint -> Bool -> Float -> Picture
blob env x y (r, g, b) secret d = Pictures
  [ at (toS env (x, y)) (Color (makeColor (r / 255) (g / 255) (b / 255) 1) (disc (d / 2)))
  , if secret then txtC env x (y + 38) 14 white "secret" else Blank
  ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _    -> newParams (min (length primes - 1) (pIdx w + 1)) w
  EventKey (SpecialKey KeyDown) Down _ _  -> newParams (max 0 (pIdx w - 1)) w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { eveRun = not (eveRun w) }
  EventKey (SpecialKey KeyLeft) Down _ _  -> pure w { eveSpeed = max 1 (eveSpeed w `div` 2) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { eveSpeed = min 5000 (eveSpeed w * 2) }
  EventKey (Char c) Down _ _ | c `elem` "aAbB" -> newSecrets w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. p = 23 だとイブは一瞬。p = 999983 で速さ 20 だと何秒かかる？ 速さを上げると？
-- 2. 紙で p = 23, g = 5, a = 6, b = 15 を計算して、2 人の鍵が同じになるのを確かめよう
-- 3. イブが「なりすまし」（アリスにはボブのふり、ボブにはアリスのふり）をしたら？ → 11 のデジタル署名が必要
-- 4. 共有した鍵を 06 のブロック暗号の鍵にすれば、そのあとは速い共通鍵暗号で話せる

-- ==================================================
-- ここから下は「道具」（べき乗の余り・原始根・絵の具を混ぜる）
-- ==================================================
-- g^e mod m を「2 乗をくり返す」方法で速く計算
modPow :: Int -> Int -> Int -> Int
modPow base e0 m = go 1 (base `mod` m) e0
  where
    go !r !_ 0 = r
    go !r !x e = go (if odd e then r * x `mod` m else r) (x * x `mod` m) (e `div` 2)

-- 原始根：g, g², g³, … が 1〜p−1 を全部まわる g
primitiveRoot :: Int -> Int
primitiveRoot pp =
  case [c | c <- [2 .. pp - 1], all (\f -> modPow c ((pp - 1) `div` f) pp /= 1) factors] of
    (c : _) -> c
    [] -> 2
  where
    factors = primeFactors (pp - 1) 2
    primeFactors n f
      | f * f > n = [n | n > 1]
      | n `mod` f == 0 = f : primeFactors (strip n f) (f + 1)
      | otherwise = primeFactors n (f + 1)
    strip n f = if n `mod` f == 0 then strip (n `div` f) f else n

mixPaint :: Paint -> Paint -> Paint
mixPaint (a, b, c) (d, e, f) = ((a + d) / 2, (b + e) / 2, (c + f) / 2)

-- p5.js と同じ「左上が原点・y は下向き」の座標を gloss の座標に直す
toS :: Env -> Point -> Point
toS e (x, y) = (x - envW e / 2, envH e / 2 - y)

-- 文字（(x, y) は左端の下）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt e x y s c str = let (a, b) = toS e (x, y) in label a b s c str

-- 中央揃えの文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC e x y s c str = txt e (x - fromIntegral (length str) * s * 0.36) y s c str

-- 左上が (x, y) の長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box e x y bw bh c = let (a, b) = toS e (x + bw / 2, y + bh / 2) in Color c (rect a b bw bh)
