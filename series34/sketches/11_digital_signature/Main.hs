-- ============================================
-- 11 デジタル署名と改ざん検知
-- 送る人：メッセージのハッシュ h を秘密鍵 d で「署名」s = h^d mod n
-- 受け取る人：公開鍵 e で s^e mod n を計算し、届いたメッセージのハッシュと比べる。一致 → 本物 ✓
-- 1 文字でも書きかえるとハッシュが変わり、検証が失敗する ✗（署名を作れるのは d を持つ本人だけ）
-- 文字を打つ = 届いたメッセージを書きかえる（改ざん）/ Backspace = 消す / Enter = 今の文に署名し直す / ↑↓ = 署名の数字をいじる / 1 = 元に戻す
-- ※ 教育用（09 と同じおもちゃの RSA。実際は 2048 ビット以上と SHA-256）
-- ============================================
module Main (main) where

import Art
import Data.Bits (xor)
import Data.Char (ord, toUpper)
import Data.List (foldl')
import Data.Word (Word32)

p1, q1, modN, phiN, pubE, privD :: Int
p1 = 1009                  -- ★ 秘密の素数
q1 = 1013
modN = p1 * q1
phiN = (p1 - 1) * (q1 - 1)
pubE = 17
privD = modInverse pubE phiN

startMessage :: String
startMessage = "PAY BOB 100 YEN"      -- ★ 署名した元のメッセージ

data World = World
  { original  :: !String
  , received  :: !String
  , signature :: !Int
  , flash     :: !Int
  }

main :: IO ()
main = runSketch (sketch "34-11 Digital Signature" (signMessage startMessage))
  { skBack  = rgb255 14 18 24
  , skStep  = \_ _ w -> pure w { flash = max 0 (flash w - 1) }
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

signMessage :: String -> World
signMessage s = World s s (modPow (toyHash s) privD modN) 30   -- ★ 署名：秘密鍵 d を使う

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (header ++ boxes ++ alicePart ++ channelPart ++ bobPart ++ verdict)
  where
    sw = envW env
    sh = envH env
    white' = gray 0.86
    sigCol = rgb255 255 150 150
    amber = rgb255 255 200 90
    green' = rgb255 120 230 150
    bad = rgb255 255 100 100
    header =
      [ txt env 24 34 18 (gray 0.94) "Digital signature  type = tamper, Backspace  Enter re-sign  Up/Down nudge signature  1 reset"
      , txt env 24 56 14 (gray 0.6)
          ("Public key (n = " ++ show modN ++ ", e = " ++ show pubE ++ ")   Private key d = " ++ show privD ++ " (only Alice knows) | educational")
      ]
    hRecv = toyHash (received w)
    hSig = modPow (signature w) pubE modN      -- ★ 検証：公開鍵 e で戻す
    ok = hRecv == hSig
    -- ---- 3 つの箱：アリス → 通信路 → ボブ
    bw = (sw - 120) / 3
    by = 110
    bh = sh * 0.42
    boxes =
      [ box3 env 30 by bw bh "Alice (sender)" (rgb255 255 140 160)
      , box3 env (60 + bw) by bw bh "Channel (anyone can rewrite)" (gray 0.63)
      , box3 env (90 + 2 * bw) by bw bh "Bob (receiver)" (rgb255 130 180 255)
      , flow (30 + bw) (60 + bw)
      , flow (60 + 2 * bw) (90 + 2 * bw)
      ]
    flow a b = Color (gray 0.78) (arrow 3 (toS env (a + 4, by + bh / 2)) (toS env (b - 4, by + bh / 2)))
    y = by + 60
    x1 = 46
    pitch = 11
    alicePart =
      [ txt env x1 y 15 white' "Message"
      , mono env x1 (y + 24) 14 pitch white ("\"" ++ original w ++ "\"")
      , txt env x1 (y + 60) 15 white' ("Hash h = " ++ show (toyHash (original w)))
      , txt env x1 (y + 96) 15 sigCol "Signature s = h^d mod n"
      , txt env x1 (y + 120) 15 sigCol ("  = " ++ show (signature w))
      , if flash w > 0 then txt env x1 (y + 156) 15 (rgba255 255 220 120 (min 255 (flash w * 8))) "Signed" else Blank
      ]
    x2 = 76 + bw
    cursor = if envFrame env `mod` 60 < 30 then "_" else ""
    -- 違う文字に印
    changed = [ i | (i, c) <- zip [0 ..] (received w), Just c /= lookup' i (original w) ]
    lookup' :: Int -> String -> Maybe Char
    lookup' i s = case drop i s of { (c : _) -> Just c; [] -> Nothing }
    channelPart =
      [ txt env x2 y 15 white' "Received text (type to tamper)"
      , mono env x2 (y + 24) 14 pitch (if received w == original w then white else amber)
          ("\"" ++ received w ++ cursor ++ "\"")
      , txt env x2 (y + 96) 15 white' "Signature s"
      , txt env x2 (y + 120) 15 sigCol ("  = " ++ show (signature w))
      ]
      ++ [ box env (x2 + fromIntegral (i + 1) * pitch) (y + 30) (pitch - 1) 3 amber | i <- changed ]
    x3 = 106 + 2 * bw
    bobPart =
      [ txt env x3 y 15 white' "1. Hash of received text"
      , txt env x3 (y + 24) 15 white ("  h' = " ++ show hRecv)
      , txt env x3 (y + 60) 15 white' "2. Undo s with public key"
      , txt env x3 (y + 84) 15 white ("  s^e mod n = " ++ show hSig)
      , txt env x3 (y + 120) 15 white' "3. Compare"
      , txt env x3 (y + 144) 15 (if ok then green' else bad)
          (if ok then "  Match -> genuine" else "  Mismatch -> tampered or forged")
      ]
    -- ---- 大きな ✓ / ✗
    cx = sw / 2
    cy = by + bh + (sh - by - bh) / 2 - 10
    r = min 120 ((sh - by - bh) * 0.35)
    col = if ok then green' else bad
    stroke a b = Color col (thickLine 16 (toS env a) (toS env b))
    verdict =
      [ at (toS env (cx, cy)) (Color col (ring r 16)) ]
      ++ (if ok
            then [ stroke (cx - r * 0.45, cy) (cx - r * 0.1, cy + r * 0.38)
                 , stroke (cx - r * 0.1, cy + r * 0.38) (cx + r * 0.5, cy - r * 0.35) ]
            else [ stroke (cx - r * 0.4, cy - r * 0.4) (cx + r * 0.4, cy + r * 0.4)
                 , stroke (cx + r * 0.4, cy - r * 0.4) (cx - r * 0.4, cy + r * 0.4) ])
      ++ [ txtC env cx (sh - 24) 15 (gray 0.67)
             (if ok then "Signature valid: Alice wrote it, and not one letter changed"
                    else "The tamperer does not know d, so cannot make a valid signature") ]

box3 :: Env -> Float -> Float -> Float -> Float -> String -> Color -> Picture
box3 env x y w h title col = Pictures
  [ box env x y w h (rgb255 26 32 42)
  , txt env (x + 16) (y + 28) 17 col title
  ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyEnter) Down _ _ -> signMessage (received w)
  EventKey (Char '\r') Down _ _ -> signMessage (received w)
  EventKey (SpecialKey KeyBackspace) Down _ _ -> backspace
  EventKey (SpecialKey KeyDelete) Down _ _    -> backspace   -- Mac の delete キー
  EventKey (Char '\b') Down _ _   -> backspace
  EventKey (Char '\DEL') Down _ _ -> backspace
  EventKey (SpecialKey KeyUp) Down _ _   -> w { signature = (signature w + 1) `mod` modN }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { signature = (signature w + modN - 1) `mod` modN }
  EventKey (Char '1') Down _ _ -> signMessage startMessage
  EventKey (SpecialKey KeySpace) Down _ _ -> typed ' '
  EventKey (Char c) Down _ _ -> typed c
  _ -> w
  where
    backspace = w { received = take (length (received w) - 1) (received w) }
    typed c | length (received w) < 28 && c >= ' ' && c <= '~' = w { received = received w ++ [toUpper c] }
            | otherwise = w

-- ---------- やってみよう ----------
-- 1. 100 を 900 に書きかえると ✗。戻すと ✓ に戻る。ハッシュは同じ文なら必ず同じ値
-- 2. ↑↓ で署名の数字を 1 つずらすと？ 正しい署名を偶然当てる確率は約 1/n
-- 3. Enter は「アリスが署名し直す」操作。改ざんした人がこれをできないのはなぜ？
-- 4. toyHash を「文字コードの合計」にすると、文字を並べかえても ✓ になってしまう。試してみよう

-- ==================================================
-- ここから下は「道具」（ハッシュ・べき乗の余り・逆数）
-- ==================================================
-- おもちゃのハッシュ（07 の FNV-1a を n で割った余り）
toyHash :: String -> Int
toyHash s =
  let h = foldl' (\v c -> (v `xor` fromIntegral (ord c)) * 0x01000193) (0x811C9DC5 :: Word32) s
  in fromIntegral h `mod` modN

-- n ≈ 10^6 なので掛け算は 10^12（Int で安全）
modPow :: Int -> Int -> Int -> Int
modPow base e0 m = go 1 (base `mod` m) e0
  where
    go !r !_ 0 = r
    go !r !x e = go (if odd e then r * x `mod` m else r) (x * x `mod` m) (e `div` 2)

modInverse :: Int -> Int -> Int
modInverse a m = go m a 0 1
  where
    go r0 r1 t0 t1
      | r1 > 0 = let q = r0 `div` r1 in go r1 (r0 - q * r1) t1 (t0 - q * t1)
      | otherwise = t0 `mod` m

-- p5.js と同じ「左上が原点・y は下向き」の座標を gloss の座標に直す
toS :: Env -> Point -> Point
toS e (x, y) = (x - envW e / 2, envH e / 2 - y)

-- 文字（(x, y) は左端の下）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt e x y s c str = let (a, b) = toS e (x, y) in label a b s c str

-- 中央揃えの文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC e x y s c str = txt e (x - fromIntegral (length str) * s * 0.36) y s c str

-- 1 文字ずつ等間隔に並べる（直した文字の下に印をつけるため）
mono :: Env -> Float -> Float -> Float -> Float -> Color -> String -> Picture
mono e x y s pitch c str =
  Pictures [txt e (x + fromIntegral i * pitch) y s c [ch] | (i, ch) <- zip [0 :: Int ..] str, ch /= ' ']

-- 左上が (x, y) の長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box e x y bw bh c = let (a, b) = toS e (x + bw / 2, y + bh / 2) in Color c (rect a b bw bh)
