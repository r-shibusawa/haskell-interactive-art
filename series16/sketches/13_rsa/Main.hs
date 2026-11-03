-- ============================================
-- 13 RSA 暗号：だれでも閉められて、1 人だけが開けられる鍵
-- 2 つの素数 p, q → n = pq、φ(n) = (p−1)(q−1)、e と互いに素な公開鍵 e、秘密鍵 d = e⁻¹ mod φ(n)
-- 暗号化 C = Mᵉ mod n、復号 M = Cᵈ mod n。累乗は「くり返し 2 乗法」で速く計算
-- 上の欄をクリックして文を入力 / 1：p=61, q=53（英数字）  2：p=251, q=263（日本語も OK）  3：ランダムな素数
-- スペース：一時停止 / F：n を素因数分解して攻撃してみる
-- ============================================
module Main (main) where

import Art
import Data.Char (chr, ord, toLower)
import Numeric (showHex, showIntAtBase)

speed :: Int
speed = 50 -- ★ 1 文字を暗号化する間隔（フレーム）

data World = World
  { wP, wQ, wNn, wPh, wE, wD :: !Integer
  , wInp    :: !String
  , wFocus  :: !Bool    -- 入力欄を選んでいるか
  , wT      :: !Int
  , wPaused :: !Bool
  , wAttack :: !(Maybe (Integer, Int))
  , wScr    :: !Integer -- 暗号文がかき混ざって見える間の乱数
  }

main :: IO ()
main = runSketch (sketch "16-13 RSA" (setKeysPure 251 263 start))
  { skBack  = bgC
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

start :: World
start = World 0 0 1 1 3 1 "Hello, RSA! \12371\12435\12395\12385\12399" False 0 False Nothing 0

isPrime :: Int -> Bool
isPrime x = x >= 2 && all (\k -> x `mod` k /= 0) (takeWhile (\k -> k * k <= x) [2 ..])

-- くり返し 2 乗法
modPow :: Integer -> Integer -> Integer -> Integer
modPow a0 b0 m = go (a0 `mod` m) b0 1
  where
    go _ 0 r = r
    go a b r = go (a * a `mod` m) (b `div` 2) (if odd b then r * a `mod` m else r)

modInv :: Integer -> Integer -> Integer
modInv a m = go a m 1 0
  where
    go r0 r1 s0 s1
      | r1 == 0 = s0 `mod` m
      | otherwise = let q = r0 `div` r1 in go r1 (r0 - q * r1) s1 (s0 - q * s1)

setKeysPure :: Integer -> Integer -> World -> World
setKeysPure p q w =
  let n = p * q
      ph = (p - 1) * (q - 1)
      e = case [c | c <- [3, 5, 17, 257, 65537], c < ph, gcd c ph == 1] of
        (c : _) -> c
        [] -> wE w
  in w { wP = p, wQ = q, wNn = n, wPh = ph, wE = e, wD = modInv e ph, wT = 0, wAttack = Nothing }

setKeys :: Int -> World -> IO World
setKeys k w = case k of
  1 -> pure (setKeysPure 61 53 w)
  2 -> pure (setKeysPure 251 263 w)
  _ -> do
    let pick = do
          x <- randomRIO (260, 899)
          if isPrime x then pure x else pick
        pickOther a = do
          b <- pick
          if b == a then pickOther a else pure b
    p <- pick
    q <- pickOther p
    pure (setKeysPure (fromIntegral p) (fromIntegral q) w)

stepW :: Env -> Float -> World -> IO World
stepW _ _ w = do
  r <- randomRIO (0, wNn w - 1)
  pure w { wT = if wPaused w then wT w else wT w + 1, wScr = r }

-- 入力欄の位置
inX, inY, inW, inH :: Float
inX = 30
inY = 96
inW = 520
inH = 26

drawW :: Env -> World -> Picture
drawW env w =
  let n = wNn w
      e = wE w
      d = wD w
      ph = wPh w
      t = wT w
      chars = take 18 (wInp w)
      shown = min (length chars) (t `div` speed + 1)
      x0 = 30
      y0 = 152
      rh = min 30 ((envH env - y0 - 40) / fromIntegral (max 1 (length chars)))
      -- 文字の行：文字 / コード / 暗号文 / 復号
      colC = x0 + 2
      colM = x0 + 86
      colE = x0 + 160
      colB = x0 + 262
      rowPic (i, ch) =
        let m = fromIntegral (ord ch) :: Integer
            yy = y0 + 24 + fromIntegral (i :: Int) * rh
            ym = yy + (rh - 4) / 2
            base = [ box env cellC colC yy 80 (rh - 4)
                   , txtC env (colC + 40) ym 14 (g8 235) (glyph ch)
                   , txtC env (colM + 35) ym 14 (g8 150) (show m) ]
            more
              | i >= shown = []
              | m >= n = [txt env colE (ym + 6) 14 (rgb255 255 110 110) "M >= n: use key 2"]
              | otherwise =
                  let c = modPow m e n
                      back = modPow c d n
                      age = t - i * speed
                      scramble = fromIntegral age < (fromIntegral speed * 0.6 :: Float)
                      shownC = if scramble then (wScr w * (fromIntegral i + 7)) `mod` n else c
                  in [ box env (rgb255 110 40 55) colE yy 98 (rh - 4)
                     , txtC env (colE + 49) ym 14 amber (show shownC) ]
                     ++ (if fromIntegral age > (fromIntegral speed * 0.6 :: Float)
                           then [ box env (rgb255 30 90 80) colB yy 80 (rh - 4)
                                , txtC env (colB + 40) ym 14 mint (glyph (chr (fromIntegral back))) ]
                           else [])
        in Pictures (base ++ more)
      curIdx = last' (-1) [i | (i, ch) <- zip [0 ..] chars, i < shown, fromIntegral (ord ch) < n]
      -- くり返し 2 乗法の表（いまの文字の復号）
      x1 = 396
      table
        | curIdx >= 0 =
            let ch = nthC chars curIdx
                m = fromIntegral (ord ch) :: Integer
                c = modPow m e n
                bits = showIntAtBase 2 (\v -> if v == 0 then '0' else '1') d ""
                title = "Decrypt '" ++ glyph ch ++ "': " ++ show c ++ "^" ++ show d ++ " mod " ++ show n
                ty0 = 152 + paraH 15 280 title + 6
                ty1 = ty0 + 22
                nb = length bits
                r2 = min 22 ((envH env - ty1 - 60) / fromIntegral nb)
                rows = drop 1 (scanl (\(cc, dd, _) b ->
                                 let dd2 = dd * dd `mod` n
                                 in if b == '1' then (cc * 2 + 1, dd2 * c `mod` n, b) else (cc * 2, dd2, b))
                               (0 :: Integer, 1 :: Integer, '0') bits)
                colsX = [x1, x1 + 40, x1 + 74, x1 + 170]
                rowLine (i, (cc, dd, b)) =
                  let y = ty1 + fromIntegral (i :: Int) * r2
                      col = if b == '1' then amber else g8 190
                      cellsT = [show (nb - 1 - i), [b], show cc, show dd]
                  in Pictures [txtT env xx y 14 col s | (xx, s) <- zip colsX cellsT]
                ones = length (filter (== '1') bits)
            in Pictures
                 [ para env x1 152 15 (g8 235) 280 title
                 , Pictures [txtT env xx ty0 14 (g8 150) s | (xx, s) <- zip colsX ["i", "b", "c", "d = C^c"]]
                 , Pictures (map rowLine (zip [0 ..] rows))
                 , para env x1 (ty1 + fromIntegral nb * r2 + 4) 14 (g8 150) 280
                     (show nb ++ " squarings, " ++ show ones ++ " multiplications (naive: " ++ show d ++ ")")
                 ]
        | otherwise = Blank
      -- 鍵
      kx = envW env * 0.55
      ky = 110
      kw = envW env - kx - 30
      ay = ky + 210
      -- 攻撃
      attackP = case wAttack w of
        Just (p, tries) ->
          let q = n `div` p
          in stack env kx (ay + 44) (kw - 10)
               [ (14, amber, 10, "Trial division of n = " ++ show n ++ ": found p = " ++ show p ++ " after " ++ show tries ++ " tries")
               , (14, coral, 10, "So q = " ++ show q ++ ", then phi(n) and d = " ++ show (modInv e ((p - 1) * (q - 1))) ++ " can be computed!")
               , (14, g8 150, 0, "If n has 617 digits (2048 bits), trial division would outlast the universe. Fast methods (Pollard rho in 15) cannot do it either: this is why RSA is safe")
               ]
        Nothing -> para env kx (ay + 44) 14 (g8 150) (kw - 10) "Press F: try to steal the secret key from the public n alone"
      field = reverse (take 44 (reverse (wInp w)))
  in Pictures
       [ txt env 30 50 26 (g8 235) "RSA: lock with the public key, open with the secret key"
       , txt env 30 80 15 (g8 150) ("Click the box to type  1: p=61, q=53  2: p=251, q=263  3: random  Space: "
                                     ++ (if wPaused w then "resume" else "pause") ++ "  F: attack")
       -- 入力欄
       , box env (rgb255 34 26 46) inX inY inW inH
       , frame env (if wFocus w then amber else g8 85) 1 inX inY inW inH
       , txt env (inX + 6) (inY + 20) 15 (g8 238) (concatMap glyphS field ++ (if wFocus w then "_" else ""))
       -- 文字ごとの暗号化
       , txtT env colC y0 14 (g8 170) "char"
       , txtT env colM y0 14 (g8 170) "M"
       , txtT env colE y0 14 (g8 170) "C = M^e"
       , txtT env colB y0 14 (g8 170) "C^d"
       , Pictures (map rowPic (zip [0 ..] chars))
       , table
       -- 鍵
       , box env panelC (kx - 10) ky (kw + 10) 190
       , txtT env kx (ky + 14) 16 coral ("Secret: p = " ++ show (wP w) ++ ",  q = " ++ show (wQ w))
       , txtT env kx (ky + 42) 16 (g8 200) ("phi(n) = (p-1)(q-1) = " ++ show ph)
       , txtT env kx (ky + 74) 16 mint ("Public key (e, n) = (" ++ show e ++ ", " ++ show n ++ ")")
       , txtT env kx (ky + 102) 16 coral ("Secret key d = e^-1 mod phi(n) = " ++ show d)
       , txtT env kx (ky + 132) 14 (g8 150) ("Check: e*d mod phi(n) = " ++ show (e * d `mod` ph) ++ " (ext. Euclid: 10)")
       , txtT env kx (ky + 156) 14 (g8 150) "n is public; without p, q no phi(n), no d"
       , box env panelC (kx - 10) ay (kw + 10) (envH env - ay - 30)
       , txtT env kx (ay + 14) 16 (g8 235) "The attacker's view (F)"
       , attackP
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w
  | wFocus w = case ev of
      EventKey (MouseButton LeftButton) Down _ _ -> pure w { wFocus = onBox }
      EventKey (SpecialKey KeySpace) Down _ _ -> typed ' '
      EventKey (SpecialKey KeyBackspace) Down _ _ -> erase
      EventKey (SpecialKey KeyDelete) Down _ _ -> erase
      EventKey (SpecialKey KeyEnter) Down _ _ -> pure w { wFocus = False }
      EventKey (Char c) Down _ _
        | c == '\b' || c == '\DEL' -> erase
        | c >= ' ' -> typed c
      _ -> pure w
  | otherwise = case ev of
      EventKey (MouseButton LeftButton) Down _ _ -> pure w { wFocus = onBox }
      EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
      EventKey (Char c0) Down _ _ -> case toLower c0 of
        '1' -> setKeys 1 w
        '2' -> setKeys 2 w
        '3' -> setKeys 3 w
        'f' -> pure w { wAttack = Just (trial (wNn w)) }
        _ -> pure w
      _ -> pure w
  where
    onBox = let (mx, my) = mouseP env in mx >= inX && mx <= inX + inW && my >= inY && my <= inY + inH
    typed c = pure w { wInp = take 64 (wInp w ++ [c]), wT = 0 }
    erase = pure w { wInp = take (length (wInp w) - 1) (wInp w), wT = 0 }

-- 試し割りで n を割る数を探す
trial :: Integer -> (Integer, Int)
trial n = go 2 0
  where
    go k tries
      | n `mod` k == 0 = (k, tries + 1)
      | otherwise = go (k + 1) (tries + 1)

-- ---------- やってみよう ----------
-- 1. 同じ文字は同じ暗号文になる → 文字の出現回数で解読されてしまう。実際の RSA は乱数を混ぜる（パディング）
-- 2. 1 の鍵で日本語を入れると M ≥ n になる。なぜダメ？（mod n で別の数と区別できない）
-- 3. e と d を入れかえると「署名」になる：秘密鍵で作って、公開鍵でだれでも確かめられる

-- ---------- 道具 ----------
-- 画面に書ける形にする（日本語はフォントにないので U+3053 のように文字コードで書く）
glyph :: Char -> String
glyph c
  | c >= ' ' && c <= '~' = [c]
  | otherwise = "U+" ++ showHex (ord c) ""

-- 入力欄では短く ? で見せる
glyphS :: Char -> String
glyphS c = if c >= ' ' && c <= '~' then [c] else "?"

last' :: Int -> [Int] -> Int
last' d = foldl (\_ x -> x) d

nthC :: String -> Int -> Char
nthC s i = case drop i s of
  (c : _) -> c
  [] -> ' '

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
