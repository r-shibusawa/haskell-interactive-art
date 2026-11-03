-- ============================================
-- 09 RSA を手で
-- 2 つの素数 p, q → n = p·q（公開）、φ = (p−1)(q−1)（秘密）
-- e を選び、e·d ≡ 1 (mod φ) となる d を「拡張ユークリッドの互除法」で求める
--   暗号化 c = m^e mod n（誰でもできる）／ 復号 m = c^d mod n（d を持つ人だけ）
-- n を素因数分解できれば φ も d も分かる → 大きな素数だから安全
-- ←→ = p / ↑↓ = q / E = 次の e / M = メッセージを変える / F = n の素因数分解（攻撃）を開始
-- ※ 教育用（実際の n は 600 桁以上）
-- ============================================
module Main (main) where

import Art
import Data.Char (chr, ord)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

primeTable :: U.Vector Int
primeTable = U.fromList
  [11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131]

wordsList :: [String]
wordsList = ["HELLO", "P5JS", "SECRET", "CAT", "RSA"]   -- ★ 送るメッセージ（1 文字ずつ数にする）

-- 拡張ユークリッドの表の 1 行（r, 商, t）
data Row = Row !Int !(Maybe Int) !Int

data World = World
  { pi'    :: !Int     -- 素数 p の番号
  , qi     :: !Int     -- 素数 q の番号
  , eIdx   :: !Int
  , wIdx   :: !Int
  , pp     :: !Int
  , qq     :: !Int
  , nN     :: !Int
  , phi    :: !Int
  , eE     :: !Int
  , dD     :: !Int
  , steps  :: ![Row]
  , attack :: !Bool
  , tryDiv :: !Int
  , foundP :: !Int
  }

main :: IO ()
main = runSketch (sketch "34-09 RSA by Hand" (makeKeys (World 6 9 0 0 0 0 1 1 3 1 [] False 2 0)))
  { skBack  = rgb255 16 14 24
  , skStep  = \env _ w -> pure (attackStep env w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

makeKeys :: World -> World
makeKeys w = w { pp = p, qq = q, nN = p * q, phi = ph, eE = e, dD = d, steps = st
               , attack = False, tryDiv = 2, foundP = 0 }
  where
    p = primeTable U.! pi' w
    q = primeTable U.! qi w
    ph = (p - 1) * (q - 1)
    -- e の候補：φ と互いに素なもの（最大公約数が 1）
    cands = [c | c <- [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 257], c < ph, gcd c ph == 1]
    e = case drop (eIdx w `mod` max 1 (length cands)) cands of
          (c : _) -> c
          [] -> 3
    (x, st) = extEuclid e ph
    d = x `mod` ph

-- 4 フレームに 1 つずつ割ってみる（ゆっくり見せる）
attackStep :: Env -> World -> World
attackStep env w
  | attack w && foundP w == 0 && envFrame env `mod` 4 == 0 =
      if nN w `mod` tryDiv w == 0 then w { foundP = tryDiv w } else w { tryDiv = tryDiv w + 1 }
  | otherwise = w

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (header ++ part1 ++ part2 ++ part3 ++ part4)
  where
    sw = envW env
    n = nN w
    e = eE w
    d = dD w
    blue' = rgb255 130 200 255
    red' = rgb255 255 150 150
    green' = rgb255 120 230 150
    amber = rgb255 255 200 90
    alarm = rgb255 255 110 110
    header =
      [ txt env 24 34 18 (gray 0.94)
          ("RSA by hand  Left/Right p = " ++ show (pp w) ++ "  Up/Down q = " ++ show (qq w) ++ "  E next e  M message  F factor attack")
      , txt env 24 56 14 (if pp w == qq w then alarm else gray 0.6)
          (if pp w == qq w then "p and q must be different primes!   Public key = (n, e), private key = d | educational"
                           else "Public key = (n, e), private key = d | educational")
      ]
    c1 = 24
    c2 = sw * 0.36
    c3 = sw * 0.68
    y0 = 100

    -- ---- 1. 鍵をつくる
    part1 =
      [ headTxt env c1 y0 "(1) Make the keys" ]
      ++ linesAt env c1 (y0 + 30)
           [ ("p = " ++ show (pp w) ++ ", q = " ++ show (qq w), gray 0.86)
           , ("n = p*q = " ++ show n ++ " (public)", blue')
           , ("phi = (p-1)(q-1) = " ++ show (phi w) ++ " (secret)", red')
           , ("e = " ++ show e ++ " (public, gcd(e, phi) = 1)", blue')
           , ("d = " ++ show d ++ " (secret)", red')
           , ("Check: e*d mod phi = " ++ show (e * d `mod` phi w), green')
           ]
      -- 拡張ユークリッドの表
      ++ [ headTxt env c1 (y0 + 220) "Extended Euclid (phi, e -> d)"
         , numR env (c1 + 70) (y0 + 250) (gray 0.67) "r"
         , numR env (c1 + 150) (y0 + 250) (gray 0.67) "quot"
         , numR env (c1 + 240) (y0 + 250) (gray 0.67) "t"
         ]
      ++ concat
           [ [ numR env (c1 + 70) y col (show r)
             , numR env (c1 + 150) y col (maybe "" show mq)
             , numR env (c1 + 240) y col (show t) ]
           | (i, Row r mq t) <- zip [0 :: Int ..] shown
           , let y = y0 + 274 + fromIntegral i * 20
                 col = if i == length (steps w) - 1 then amber else gray 0.78 ]
      ++ [ txt env c1 (y0 + 274 + fromIntegral (length shown) * 20 + 10) 14 (gray 0.6) "t in the row where r = 1 is d (add phi if negative)" ]
    shown = take 9 (steps w)

    -- ---- 2. 暗号化と復号
    word = case drop (wIdx w) wordsList of
      (s : _) -> s
      [] -> ""
    cipherNums = [modPow (ord ch) e n | ch <- word]
    colX k = c2 + k
    part2 =
      [ headTxt env c2 y0 "(2) Encrypt / decrypt (char codes)"
      , txt env (colX 0) (y0 + 58) 15 (gray 0.63) "Char"
      , txt env (colX 60) (y0 + 58) 15 (gray 0.63) "m"
      , txt env (colX 110) (y0 + 58) 15 (gray 0.63) "c = m^e mod n"
      , txt env (colX 280) (y0 + 58) 15 (gray 0.63) "c^d mod n"
      ]
      ++ concat
           [ [ txt env (colX 0) y 15 (gray 0.9) [ch]
             , txt env (colX 60) y 15 (gray 0.9) (show (ord ch))
             , txt env (colX 110) y 15 amber (show c)
             , txt env (colX 280) y 15 green' [charOf (modPow c d n)] ]
           | (r, ch, c) <- zip3 [1 :: Int ..] word cipherNums
           , let y = y0 + 58 + fromIntegral r * 24 ]

    -- ---- 3. 署名
    hsh = foldl' (\h ch -> (h * 31 + ord ch) `mod` n) 0 word
    sig = modPow hsh d n
    back = modPow sig e n
    sy = y0 + 70 + fromIntegral (length word + 1) * 24
    part3 =
      headTxt env c2 sy "(3) Sign (the other way around)"
      : linesAt env c2 (sy + 30)
          [ ("Hash h = " ++ show hsh ++ " (from \"" ++ word ++ "\")", gray 0.86)
          , ("Signature s = h^d mod n = " ++ show sig ++ " (d only)", red')
          , ("Verify s^e mod n = " ++ show back ++ (if back == hsh then "  = h OK" else "  NG"), green')
          ]

    -- ---- 4. 攻撃
    part4 =
      [ headTxt env c3 y0 ("(4) Attacker: factor n = " ++ show n)
      , txt env c3 (y0 + 34) 15 (gray 0.78) attackLine
      ]
      ++ stolenLines
      ++ [ txt env c3 (y0 + 190 + fromIntegral i * 22) 14 (gray 0.6) s
         | (i, s) <- zip [0 :: Int ..]
             [ "If n has 617 digits (2048 bits),"
             , "sqrt(n) has about 300 digits."
             , "The fastest known methods would still"
             , "take billions of years."
             , "-> Multiplying is easy, factoring is hard:"
             , "   that is what keeps RSA safe." ] ]
    attackLine
      | not (attack w) = "F to start (divide by 2, 3, 4, ...)"
      | foundP w > 0 = show n ++ " = " ++ show (foundP w) ++ " x " ++ show (n `div` foundP w)
      | otherwise = show n ++ " / " ++ show (tryDiv w) ++ " = "
          ++ printf "%.2f" (fromIntegral n / fromIntegral (tryDiv w) :: Float) ++ " ..."
    stolenLines
      | foundP w == 0 = []
      | otherwise =
          let f = foundP w
              ph2 = (f - 1) * (n `div` f - 1)
              d2 = fst (extEuclid e ph2) `mod` max 1 ph2
              stolen = [charOf (modPow c d2 n) | c <- cipherNums]
          in linesAt env c3 (y0 + 64)
               [ ("phi = " ++ show ph2 ++ " -> d = " ++ show d2 ++ " exposed", alarm)
               , ("Stolen text: \"" ++ stolen ++ "\"", alarm)
               , ("Divided up to " ++ show (tryDiv w), gray 0.67)
               , ("(checking up to sqrt(n) = " ++ show (isqrt n) ++ " is enough)", gray 0.67)
               ]

headTxt :: Env -> Float -> Float -> String -> Picture
headTxt env x y = txt env x y 17 (rgb255 255 220 120)

linesAt :: Env -> Float -> Float -> [(String, Color)] -> [Picture]
linesAt env x y items = [txt env x (y + fromIntegral i * 26) 15 c s | (i, (s, c)) <- zip [0 :: Int ..] items]

-- 右端をそろえて書く（数字の幅はほぼ 0.62 × 大きさ）
numR :: Env -> Float -> Float -> Color -> String -> Picture
numR env x y c s = txt env (x - fromIntegral (length s) * 14 * 0.62) y 14 c s

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey k) Down _ _ -> case k of
    KeyLeft  -> makeKeys w { pi' = max 0 (pi' w - 1) }
    KeyRight -> makeKeys w { pi' = min (U.length primeTable - 1) (pi' w + 1) }
    KeyDown  -> makeKeys w { qi = max 0 (qi w - 1) }
    KeyUp    -> makeKeys w { qi = min (U.length primeTable - 1) (qi w + 1) }
    _ -> makeKeys w
  EventKey (Char c) Down _ _
    | c `elem` "eE" -> makeKeys w { eIdx = eIdx w + 1 }
    | c `elem` "mM" -> w { wIdx = (wIdx w + 1) `mod` length wordsList }
    | c `elem` "fF" -> (makeKeys w) { attack = not (attack w) }
    | otherwise -> makeKeys w      -- p5.js 版と同じく、ほかのキーでも攻撃はリセットされる
  _ -> w

-- ---------- やってみよう ----------
-- 1. p = 61, q = 53, e = 17 のとき d はいくつ？ 表を紙で追いかけてみよう（答え 2753）
-- 2. 同じ文字は同じ暗号文になる（"HELLO" の L）。これは 06 の ECB と同じ弱点。どう直す？
-- 3. 署名の h を 1 だけ変えると、検証はどうなる？（→ 11 のデジタル署名）
-- 4. primeTable にもっと大きな素数を足すと、攻撃にかかる時間はどう伸びる？

-- ==================================================
-- ここから下は「道具」（拡張ユークリッド・べき乗の余り）
-- ==================================================
-- a·x ≡ 1 (mod m) となる x を求め、途中の表（r, 商, t）も返す
extEuclid :: Int -> Int -> (Int, [Row])
extEuclid a m = go m a 0 1 [Row a Nothing 1, Row m Nothing 0]
  where
    go r0 r1 t0 t1 acc
      | r1 > 1 =
          let qt = r0 `div` r1
              r2 = r0 - qt * r1
              t2 = t0 - qt * t1
          in go r1 r2 t1 t2 (Row r2 (Just qt) t2 : acc)
      | otherwise = (t1, reverse acc)

modPow :: Int -> Int -> Int -> Int
modPow base e0 m = go 1 (base `mod` m) e0
  where
    go !r !_ 0 = r
    go !r !x e = go (if odd e then r * x `mod` m else r) (x * x `mod` m) (e `div` 2)

charOf :: Int -> Char
charOf v = if v >= 32 && v < 127 then chr v else '?'

isqrt :: Int -> Int
isqrt v = floor (sqrt (fromIntegral v :: Float))

-- p5.js と同じ「左上が原点・y は下向き」の座標を gloss の座標に直す
toS :: Env -> Point -> Point
toS e (x, y) = (x - envW e / 2, envH e / 2 - y)

-- 文字（(x, y) は左端の下）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt e x y s c str = let (a, b) = toS e (x, y) in label a b s c str
