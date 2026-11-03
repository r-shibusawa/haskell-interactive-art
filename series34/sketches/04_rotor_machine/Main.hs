-- ============================================
-- 04 エニグマ風ローター（概念モデル）
-- 3 つのローター（文字の入れかえ）と反射板。キーを押すたびに右のローターが 1 つ回るので、
-- 同じ文字でも毎回ちがう文字になる。反射板のおかげで「同じ設定にもどせば、同じ操作で復号できる」
-- 配線はこのスケッチ用のランダムなもの（歴史上のエニグマとは違う）
-- 文字を打つ = 暗号化 / ↑↓ = ローターを選ぶ / ←→ = 開始位置を変える / Backspace = 開始位置に戻してテープを消す
-- ※ 教育用。実際の安全な暗号には使わない
-- ============================================
module Main (main) where

import Art
import Data.Char (chr, isAsciiLower, isAsciiUpper, ord, toUpper)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as U
import System.Random (StdGen, mkStdGen, randomR)

wiringSeed :: Int
wiringSeed = 7                     -- ★ 配線を決める種（変えると別の機械になる）

notch :: [Int]
notch = [17, 5, 22]                -- ★ この位置を通過すると左隣のローターが 1 つ回る

-- ローター位置（左・中・右）
data Pos = Pos !Int !Int !Int

data World = World
  { startPos :: !Pos               -- 開始位置
  , pos      :: !Pos
  , sel      :: !Int               -- 選んでいるローター
  , tapeIn   :: !String            -- 新しい文字が先頭（表示のときに逆順にする）
  , tapeOut  :: !String
  , lastPath :: ![Int]             -- 最後に押した文字の通り道（8 個、なければ空）
  , lampOn   :: !Int
  , lampTime :: !Float
  }

main :: IO ()
main = runSketch (sketch "34-04 Rotor Machine" (World p0 p0 2 "" "" [] (-1) 0))
  { skBack  = rgb255 20 18 16
  , skDraw  = drawWorld
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }
  where p0 = Pos 0 0 0

-- 配線：種から決まる 3 つのローターと、26 文字を 13 組のペアにした反射板
rotorL, rotorM, rotorR, reflector :: U.Vector Int
(rotorL, rotorM, rotorR, reflector) =
  let g0 = mkStdGen wiringSeed
      (a, g1) = shufflePure [0 .. 25] g0
      (b, g2) = shufflePure [0 .. 25] g1
      (c, g3) = shufflePure [0 .. 25] g2
      (o, _) = shufflePure [0 .. 25] g3
      pairs (x : y : r) = (x, y) : (y, x) : pairs r
      pairs _ = []
  in (U.fromList a, U.fromList b, U.fromList c, U.replicate 26 0 U.// pairs o)

rotorOf :: Int -> U.Vector Int
rotorOf 0 = rotorL
rotorOf 1 = rotorM
rotorOf _ = rotorR

posOf :: Pos -> Int -> Int
posOf (Pos a _ _) 0 = a
posOf (Pos _ b _) 1 = b
posOf (Pos _ _ c) _ = c

-- ★ ローターの段送り：右は毎回、中と左は「ノッチ」を通過したとき
stepRotors :: Pos -> Pos
stepRotors (Pos a b c) =
  let midTurn = c == notchOf 2
      leftTurn = b == notchOf 1 && midTurn
      inc t v = if t then (v + 1) `mod` 26 else v
  in Pos (inc leftTurn a) (inc midTurn b) ((c + 1) `mod` 26)
  where notchOf i = case drop i notch of { (n : _) -> n; [] -> 0 }

-- ローター r を通す（forward = 右→左、そうでなければ帰り道）
through :: Pos -> Int -> Int -> Bool -> Int
through ps r c forward
  | forward = (rotorOf r U.! ((c + p) `mod` 26) - p + 26) `mod` 26
  | otherwise = (fromMaybe 0 (U.elemIndex ((c + p) `mod` 26) (rotorOf r)) - p + 26) `mod` 26
  where p = posOf ps r

-- ★ 1 文字の暗号化：回す → 右→中→左 → 反射 → 左→中→右。通り道も返す
encryptLetter :: Pos -> Int -> (Pos, [Int])
encryptLetter ps0 c =
  let ps = stepRotors ps0
      a1 = through ps 2 c True
      a2 = through ps 1 a1 True
      a3 = through ps 0 a2 True
      m = reflector U.! a3
      b1 = through ps 0 m False
      b2 = through ps 1 b1 False
      b3 = through ps 2 b2 False
  in (ps, [c, a1, a2, a3, m, b1, b2, b3])

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (header ++ columns ++ letters ++ path ++ tape)
  where
    sw = envW env
    sh = envH env
    header =
      [ txt env 24 34 18 (gray 0.94) "Rotor machine (concept)  type = encrypt  Up/Down rotor  Left/Right start  Backspace reset"
      , txt env 24 56 14 (gray 0.6) "Wiring is random for this sketch. Type the ciphertext from the same start to get the plaintext back | educational"
      ]
    -- 列：反射板 | 左 | 中 | 右 | 入出力
    top = 110
    rowH = (sh - top - 190) / 26
    colW = min 170 ((sw - 120) / 5)
    colX i = 60 + fromIntegral (i :: Int) * colW * 1.15
    names = ["Reflector", "Left", "Middle", "Right", "Input / Lamp"]
    columns = concat
      [ [ txtC env (colX i + colW / 2) (top - 14) 15 (if i - 1 == sel w then yellow' else gray 0.78)
            (nm ++ (if i >= 1 && i <= 3 then "  " ++ [letterOf (posOf (pos w) (i - 1))] else ""))
        , box env (colX i) top colW (rowH * 26) (if i - 1 == sel w then rgb255 60 52 30 else rgb255 36 34 32)
        ]
      | (i, nm) <- zip [0 ..] names ]
    yellow' = rgb255 255 210 90
    -- 各列の 26 行の文字（ローターは回った分だけずれて見える）
    size = max 12 (min 14 (rowH * 0.8))
    letters = concat
      [ [ txt env (colX i + 6) y size (gray 0.43) [letterOf ((j + posOf (pos w) (i - 1)) `mod` 26)] | i <- [1 .. 3] ]
        ++ [ txt env (colX 4 + colW / 2 - 5) y size
               (if j == lampOn w && envTime env - lampTime w < 0.6 then rgb255 255 230 120 else gray 0.6) [letterOf j] ]
      | j <- [0 .. 25]
      , let y = top + fromIntegral j * rowH + rowH * 0.75 ]

    -- 最後に押した文字の通り道
    path = case lastPath w of
      [p0, p1, p2, p3, p4, p5, p6, p7] ->
        let yOf j = top + fromIntegral j * rowH + rowH / 2
            pR i = colX i + colW
            l0 = colX 0
            go = [(colX 4, p0), (pR 3, p0), (colX 3, p1), (pR 2, p1), (colX 2, p2), (pR 1, p2), (colX 1, p3), (pR 0, p3)]
            back = [(pR 0, p4), (colX 1, p4), (pR 1, p5), (colX 2, p5), (pR 2, p6), (colX 3, p6), (pR 3, p7), (colX 4, p7)]
            route dy = map (\(x, j) -> toS env (x, yOf j + dy))
        in [ Color (rgb255 120 200 255) (thickPath 3 (route 0 go))     -- 行き（右 → 左）
           , Color (rgb255 255 150 220) (thickPath 3 (map (toS env)    -- 反射板の中
               [(pR 0, yOf p3), (l0 + colW * 0.4, yOf p3), (l0 + colW * 0.4, yOf p4), (pR 0, yOf p4)]))
           , Color (rgb255 255 200 90) (thickPath 3 (route 2 back))    -- 帰り（左 → 右）
           ]
      _ -> []

    -- テープ
    ty = sh - 140
    Pos s0 s1 s2 = startPos w
    tape =
      [ box env 24 ty (sw - 48) 110 (rgb255 40 36 32)
      , txt env 44 (ty + 38) 20 (rgb255 120 200 255) "In"
      , mono env 110 (ty + 38) 20 17 (rgb255 120 200 255) (reverse (take 48 (tapeIn w)))
      , txt env 44 (ty + 72) 20 (rgb255 255 200 90) "Out"
      , mono env 110 (ty + 72) 20 17 (rgb255 255 200 90) (reverse (take 48 (tapeOut w)))
      , txt env 44 (ty + 100) 14 (gray 0.6)
          ("Start " ++ map letterOf [s0, s1, s2] ++ " | Note: a letter never maps to itself (reflector), a clue that helped break the real Enigma")
      ]

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _    -> w { sel = (sel w + 2) `mod` 3 }
  EventKey (SpecialKey KeyDown) Down _ _  -> w { sel = (sel w + 1) `mod` 3 }
  EventKey (SpecialKey KeyLeft) Down _ _  -> reset w { startPos = bump 25 }
  EventKey (SpecialKey KeyRight) Down _ _ -> reset w { startPos = bump 1 }
  EventKey (SpecialKey KeyBackspace) Down _ _ -> reset w
  EventKey (SpecialKey KeyDelete) Down _ _    -> reset w   -- Mac の delete キー
  EventKey (Char '\b') Down _ _   -> reset w
  EventKey (Char '\DEL') Down _ _ -> reset w
  EventKey (Char c) Down _ _
    | isAsciiUpper c || isAsciiLower c ->
        let up = toUpper c
            (ps, route) = encryptLetter (pos w) (ord up - 65)
            out = case drop 7 route of { (o : _) -> o; [] -> 0 }
        in w { pos = ps, lastPath = route
             , tapeIn = take 200 (up : tapeIn w)     -- 長くなりすぎないように
             , tapeOut = take 200 (letterOf out : tapeOut w)
             , lampOn = out, lampTime = envTime env }
  _ -> w
  where
    bump d = case (startPos w, sel w) of
      (Pos a b c, 0) -> Pos ((a + d) `mod` 26) b c
      (Pos a b c, 1) -> Pos a ((b + d) `mod` 26) c
      (Pos a b c, _) -> Pos a b ((c + d) `mod` 26)

reset :: World -> World
reset w = w { pos = startPos w, tapeIn = "", tapeOut = "", lastPath = [] }

-- ---------- やってみよう ----------
-- 1. AAAAA と打つと出力はどうなる？ 同じ文字が毎回ちがう文字になるのを確かめよう
-- 2. 出力をメモ → Backspace → メモした文字を打つと、元の文に戻る（暗号化と復号が同じ操作）
-- 3. 開始位置は 26³ = 17576 通り。現代のコンピュータなら一瞬で総当たりできる。何通りなら安全？
-- 4. 入力と出力の間に「プラグボード」（文字のペアを入れかえる）を足して、鍵の数を増やそう

-- ==================================================
-- ここから下は「道具」（種から決まるシャッフル、文字、座標）
-- ==================================================
shufflePure :: [Int] -> StdGen -> ([Int], StdGen)
shufflePure [] g = ([], g)
shufflePure xs g =
  let (i, g1) = randomR (0, length xs - 1) g
      (a, rest) = splitAt i xs
  in case rest of
       (x : b) -> let (r, g2) = shufflePure (a ++ b) g1 in (x : r, g2)
       [] -> (a, g1)

letterOf :: Int -> Char
letterOf i = chr (65 + i)

-- p5.js と同じ「左上が原点・y は下向き」の座標を gloss の座標に直す
toS :: Env -> Point -> Point
toS e (x, y) = (x - envW e / 2, envH e / 2 - y)

-- 文字（(x, y) は左端の下）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt e x y s c str = let (a, b) = toS e (x, y) in label a b s c str

-- 中央揃えの文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC e x y s c str = txt e (x - fromIntegral (length str) * s * 0.36) y s c str

-- 1 文字ずつ等間隔に並べる
mono :: Env -> Float -> Float -> Float -> Float -> Color -> String -> Picture
mono e x y s pitch c str =
  Pictures [txt e (x + fromIntegral i * pitch) y s c [ch] | (i, ch) <- zip [0 :: Int ..] str]

-- 左上が (x, y) の長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box e x y bw bh c = let (a, b) = toS e (x + bw / 2, y + bh / 2) in Color c (rect a b bw bh)
