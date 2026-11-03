-- ============================================
-- 14 暗号を聞く 🔊
-- データの 1 バイト = 1 つの音。平文のメロディはくり返しがあって「曲」に聞こえる。
--   ECB で暗号化：同じ 2 バイトのかたまりは同じ暗号文 → くり返しがそのまま聞こえてしまう（06 の輪郭と同じ）
--   CTR で暗号化：毎回違うカウンタを混ぜる → ランダムな音の列。耳でも区別できない = よい暗号
-- クリック = 音を出す / 1 = 平文 2 = ECB 3 = CTR / ←→ = テンポ / ↑↓ = 鍵を変える / W = 音色 / スペース = 一時停止
-- 自由制作のベース：phrase を自分の曲に変えてみよう
-- ※ 教育用（05 と同じおもちゃの 16 ビット暗号）
-- ============================================
module Main (main) where

import Art
import Control.Monad (when)
import Data.Bits (shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.Char (toUpper)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Numeric (showHex)

-- ★ 平文のメロディ（0〜14 = ペンタトニック音階の段）。2 つずつ組になって 1 ブロック
phrase :: [Int]
phrase = [0, 2, 4, 2, 0, 2, 4, 7, 5, 4, 2, 4, 0, 0, 2, 0]

melody :: [Int]
melody = phrase ++ phrase ++ phrase ++ take 12 phrase ++ [7, 5, 4, 0]

scaleSemis :: U.Vector Int
scaleSemis = U.fromList [0, 2, 4, 7, 9]        -- ペンタトニック（ド レ ミ ソ ラ）

sbox :: U.Vector Int
sbox = U.fromList [0xC, 0x5, 0x6, 0xB, 0x9, 0x0, 0xA, 0xD, 0x3, 0xE, 0xF, 0x8, 0x4, 0x7, 0x1, 0x2]

data World = World
  { modeNo    :: !Int
  , tempo     :: !Int             -- ★ 1 分あたりの音の数
  , cipherKey :: !Int
  , wave      :: !Wave
  , playing   :: !Bool
  , audioOn   :: !Bool            -- クリックかキーで音を出し始める
  , idx       :: !Int
  , lastNote  :: !Float
  , bytes     :: !(U.Vector Int)
  }

main :: IO ()
main = runSketch (sketch "34-14 Hear the Cipher" (rebuild (World 1 160 0x2B7E Triangle True False 0 0 U.empty)))
  { skBack  = hsb 230 30 10
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ モードに合わせてバイト列をつくる
rebuild :: World -> World
rebuild w = w { bytes = U.fromList (concat (zipWith toBytes [0 ..] (pairs melody))) }
  where
    pairs (a : b : r) = ((a `shiftL` 8) .|. b) : pairs r
    pairs _ = []
    toBytes :: Int -> Int -> [Int]
    toBytes i block =
      let c = case modeNo w of
                2 -> enc (cipherKey w) block                                       -- ECB
                3 -> block `xor` enc (cipherKey w) ((0x1000 + i) .&. 0xFFFF)       -- CTR
                _ -> block
      in [(c `shiftR` 8) .&. 0xFF, c .&. 0xFF]

-- バイト → 音の高さ（Hz）。15 段のペンタトニックに折りたたむ
byteFreq :: Int -> Float
byteFreq b =
  let step = b `mod` 15
      semi = scaleSemis U.! (step `mod` 5) + 12 * (step `div` 5)
  in 220 * 2 ** (fromIntegral semi / 12)

-- 再生
stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | playing w && audioOn w && envTime env - lastNote w > interval && U.length (bytes w) > 0 = do
      playTone (byteFreq (bytes w U.! idx w)) 0.25 (interval * 0.9) (wave w)
      pure w { lastNote = envTime env, idx = (idx w + 1) `mod` U.length (bytes w) }
  | otherwise = pure w
  where interval = 60 / fromIntegral (tempo w)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (header ++ cells ++ roll)
  where
    sw = envW env
    sh = envH env
    bs = bytes w
    nB = U.length bs
    current = (idx w + nB - 1) `mod` max 1 nB
    (name, note) = case modeNo w of
      2 -> ("ECB encrypted", "Same block -> same note group. Repetition is audible (bad)")
      3 -> ("CTR encrypted", "Different every time. The repetition is gone (good)")
      _ -> ("Plain (melody)", "It repeats, so it sounds like a tune")
    header =
      [ txt env 24 28 18 (hsb 0 0 95) "Hear the cipher   click = sound   1 plain  2 ECB  3 CTR"
      , txt env 24 50 14 (hsb 0 0 80)
          ("Left/Right tempo " ++ show (tempo w) ++ "   Up/Down key " ++ hex4 (cipherKey w)
            ++ "   W wave " ++ waveName (wave w) ++ "   Space " ++ (if playing w then "stop" else "play"))
      , txt env 24 70 14 (hsb 0 0 60)
          (if audioOn w then "From top left: 1 cell = 1 byte = 1 note. Same color = same pitch | educational"
                        else ">> Click once (or press a key) to start the sound")
      , txt env 24 100 20 (if modeNo w == 2 then hsb 0 60 100 else hsb 140 50 90)
          (show (modeNo w) ++ ". " ++ name ++ "   " ++ note)
      ]
    -- バイトのマス目（16 個 × 4 段）
    cols = 16 :: Int
    cw = min 70 ((sw - 48) / fromIntegral cols)
    top = 120
    cells = concat
      [ [ box env (x + 2) (y + 2) (cw - 4) (cw - 4) (hsb (fromIntegral step * 24) 70 (40 + fromIntegral step * 4)) ]
        ++ [ outline env x y cw cw 3 white | i == current && audioOn w ]
        ++ [ txt env (x + 8) (y + 20) 14 (hsba 0 0 100 0.8) (hex2 b) ]
        ++ [ box env (x + 4) (y + cw - 8) (cw * 2 - 8) 3 (hsba 0 0 100 0.3) | even i ]   -- 2 バイト = 1 ブロック
      | (i, b) <- zip [0 :: Int ..] (U.toList bs)
      , let (r, c) = i `divMod` cols
            x = 24 + fromIntegral c * cw
            y = top + fromIntegral r * cw
            step = b `mod` 15 ]
    -- 音の高さのグラフ（ピアノロール）
    nRows = (nB + cols - 1) `div` cols
    gy = top + fromIntegral nRows * cw + 24
    gh = sh - gy - 24
    bw = (sw - 48) / fromIntegral (max 1 nB)
    roll =
      box env 24 gy (sw - 48) gh (hsba 0 0 100 0.06)
      : [ box env (24 + fromIntegral i * bw) (gy + gh - fromIntegral (step + 1) / 15 * (gh - 10)) (bw - 1) 8
            (hsb (fromIntegral step * 24) 70 (if i == current then 100 else 75))
        | (i, b) <- zip [0 :: Int ..] (U.toList bs)
        , let step = b `mod` 15 ]
      ++ [ txt env 30 (gy + 18) 14 (hsb 0 0 60) "Pitch (across = time)" ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { audioOn = True }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> pure (rebuild on { modeNo = fromEnum c - fromEnum '0' })
    | c `elem` "wW" -> pure on { wave = nextWave (wave w) }
    | otherwise -> pure on
  EventKey (SpecialKey k) Down _ _ -> case k of
    KeyLeft  -> pure on { tempo = max 60 (tempo w - 20) }
    KeyRight -> pure on { tempo = min 480 (tempo w + 20) }
    KeyUp    -> newKey
    KeyDown  -> newKey
    KeySpace -> pure on { playing = not (playing w) }
    _ -> pure on
  _ -> pure w
  where
    on = w { audioOn = True }
    newKey = (\v -> rebuild on { cipherKey = v }) <$> randomRIO (0, 65535)

-- ---------- やってみよう ----------
-- 1. 1 → 2 → 3 の順に聞き比べよう。ECB で「同じフレーズがまた来た」と分かる？
-- 2. phrase を自分のメロディに変えて、CTR にしたらどれくらい「別物」になるか聞いてみよう
-- 3. byteFreq を変えて、バイトを和音やリズム（音の長さ）にも割り当ててみよう
-- 4. 作品のアイデア：自分の名前をハッシュ（07）して、その 32 ビットで 1 小節の曲をつくる

-- ==================================================
-- ここから下は「道具」（おもちゃの暗号と表示の部品。音は Art.Sound の playTone）
-- ==================================================
subst :: Int -> Int
subst x = foldl' (\y i -> y .|. ((sbox U.! ((x `shiftR` (4 * i)) .&. 0xF)) `shiftL` (4 * i))) 0 [0 .. 3]

permute :: Int -> Int
permute x = foldl' (\y b -> if testBit x (15 - b) then y .|. (1 `shiftL` (15 - ((b `mod` 4) * 4 + (b `shiftR` 2)))) else y) 0 [0 .. 15]

roundKey :: Int -> Int -> Int
roundKey k r =
  let s = (3 * r) `mod` 16
  in (((k `shiftL` s) .|. (k `shiftR` (16 - s))) `xor` (0x1357 * (r + 1))) .&. 0xFFFF

enc :: Int -> Int -> Int
enc k x0 = foldl' oneRound x0 [0 .. 3] `xor` roundKey k 4
  where
    oneRound x r =
      let b = subst (x `xor` roundKey k r)
      in if r < 3 then permute b else b

nextWave :: Wave -> Wave
nextWave Sine = Triangle
nextWave Triangle = Square
nextWave Square = Sawtooth
nextWave Sawtooth = Sine

waveName :: Wave -> String
waveName Sine = "sine"
waveName Triangle = "triangle"
waveName Square = "square"
waveName Sawtooth = "sawtooth"

hexPad :: Int -> Int -> String
hexPad n v = let s = map toUpper (showHex v "") in replicate (n - length s) '0' ++ s

hex2, hex4 :: Int -> String
hex2 = hexPad 2
hex4 v = "0x" ++ hexPad 4 v

-- 太さのある枠
outline :: Env -> Float -> Float -> Float -> Float -> Float -> Color -> Picture
outline env x y w h t c = Color c (thickPath t (map (toS env) [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]))

-- p5.js と同じ「左上が原点・y は下向き」の座標を gloss の座標に直す
toS :: Env -> Point -> Point
toS e (x, y) = (x - envW e / 2, envH e / 2 - y)

-- 文字（(x, y) は左端の下）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt e x y s c str = let (a, b) = toS e (x, y) in label a b s c str

-- 左上が (x, y) の長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box e x y bw bh c = let (a, b) = toS e (x + bw / 2, y + bh / 2) in Color c (rect a b bw bh)
