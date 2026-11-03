-- ============================================
-- 12 ルービックキューブ ③ 交換子と共役
-- 交換子 [A, B] = A B A⁻¹ B⁻¹：A と B が重なる所だけが動く → ほんの数個だけ入れかえる手順ができる
-- 共役 X A X⁻¹：X で準備 → A → X をもとにもどす。A の効果を「別の場所」に移す
--   実行するのは X · [A, B] · X⁻¹（B が空なら X A X⁻¹）
-- 1〜6 = 例 / Tab = 編集する欄（A → B → X）/ u d l r f b（Shift で '）= 欄に手を足す / Backspace = 消す
-- スペース = 1 手ずつ実行（動いた所を強調）/ 0 = リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (toUpper)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ★ (A, B, X, 説明)
presets :: [(String, String, String, String)]
presets =
  [ ("R", "U", "", "[R, U]: the famous R U R' U'. Only 7 pieces move")
  , ("R U R'", "D", "", "Corner-only 3-cycle (A moves just one corner of the D face)")
  , ("U2 R2 U2", "F2", "", "Edge-only 3-cycle")
  , ("U", "", "R", "Conjugate R U R': same 'shape' of motion as U, somewhere else")
  , ("R U R'", "D", "L", "The 3-cycle of example 2 conjugated by L -> swaps other corners")
  , ("R", "U", "F", "F [R, U] F': moves only the top layer (one of the OLL algorithms)")
  ]

data World = World
  { wA, wB, wX :: ![Move]
  , wSlot   :: !Int        -- 0 = A, 1 = B, 2 = X
  , wNote   :: !String
  , wCube   :: ![St]
  , wBefore :: !(Map.Map P Int)  -- 実行前の各マスの色
  , wPlan   :: ![Move]     -- 実行中の手の列
  , wPi     :: !Int        -- 実行中の位置
  , wTimer  :: !Int
  , wHi     :: !(Set.Set (Int, Int, Int))  -- 強調するマス
  , wMoved  :: !Int
  }

stepFrames :: Int
stepFrames = 14          -- ★ 1 手あたりのフレーム数

usePreset :: Int -> World -> World
usePreset i w =
  let (a, b, x, note) = presets !! (i `mod` length presets)
  in w { wA = words a, wB = words b, wX = words x, wNote = note
       , wCube = newCube, wPlan = [], wHi = Set.empty, wMoved = 0 }

world0 :: World
world0 = usePreset 0 (World [] [] [] 0 "" newCube Map.empty [] 0 0 Set.empty 0)

main :: IO ()
main = runSketch (sketch "23-12 Rubik Commutator" world0)
  { skBack  = rgb255 24 28 34
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- X · [A, B] · X⁻¹（B が空なら X A X⁻¹）
expression :: World -> [Move]
expression w =
  let core | null (wB w) = wA w
           | otherwise = wA w ++ wB w ++ invSeq (wA w) ++ invSeq (wB w)
  in wX w ++ core ++ invSeq (wX w)

-- 1 手ずつ実行
step :: World -> World
step w = case drop (wPi w) (wPlan w) of
  [] -> w
  (m : rest)
    | wTimer w + 1 < stepFrames -> w { wTimer = wTimer w + 1 }
    | otherwise ->
        let w' = w { wTimer = 0, wCube = doMove m (wCube w), wPi = wPi w + 1 }
        in if null rest then markChanged w' else w'

-- 実行の前後でステッカーの色が変わったマスと、そのキュービーを数える
markChanged :: World -> World
markChanged w =
  let changed = [s | s <- wCube w, Map.lookup (sP s) (wBefore w) /= Just (sC s)]
      cell s = netCell (sP s) (sN s)
      cubie s = let P a b c = sP s
                    P d e f = sN s
                in P (a - d) (b - e) (c - f)
  in w { wHi = Set.fromList (map cell changed), wMoved = Set.size (Set.fromList (map cubie changed)) }

drawWorld :: World -> Picture
drawWorld w = Pictures ([drawNet (wCube w) x0 y0 cs (if finished then wHi w else Set.empty)] ++ hud ++ slots ++ expr)
  where
    cs = min (width * 0.52 / 12) ((height - 260) / 9)
    x0 = 30 :: Float
    y0 = 150 :: Float
    plan = wPlan w
    finished = wPi w >= length plan
    hud =
      [ txt 20 36 16 white "1-6: examples  Tab: edit slot  u d l r f b (Shift = '): add move  Backspace: erase  Space: run  0: reset"
      , txt 20 62 14 (gray (170 / 255)) "[A, B] = A B A^-1 B^-1. If A and B barely overlap, only the few shared pieces move: a building block of cube solving"
      , txt 20 100 17 (rgb255 255 210 90) (wNote w)
      ]
    px = x0 + cs * 12 + 40
    py = 140 :: Float
    slots = concat
      [ let y = py + fromIntegral i * 70
            on = i == wSlot w
            s = nm ++ " = " ++ (if null v then "(empty)" else unwords v)
        in [ box (px - 10) (y - 30) (width - px - 20) 56 (if on then rgba255 255 0 200 60 else rgba255 255 255 255 18)
           , txt px (y + 8) (if length s > 26 then 16 else 26) (if on then rgb255 255 120 230 else white) (take 44 s) ]
      | (i, nm, v) <- zip3 [0 :: Int ..] ["A", "B", "X"] [wA w, wB w, wX w] ]
    -- 展開した手順。実行中の手を強調
    ex = if null plan then expression w else plan
    yTitle = py + 230
    title | not (null (wX w)) = if not (null (wB w)) then "X [A, B] X^-1 =" else "X A X^-1 ="
          | not (null (wB w)) = "[A, B] = A B A^-1 B^-1 ="
          | otherwise = "A ="
    (tokens, yLast) = layoutMoves px (yTitle + 36) (zip [0 ..] ex)
    expr =
      [ txt px yTitle 16 (gray 0.78) title ]
      ++ [ txt x y 24 (moveColor i) m | (x, y, i, m) <- tokens ]
      ++ [ txt px (yLast + 36) 16 white (show (length ex) ++ " moves") ]
      ++ (if not (null plan) && finished
            then [ txt px (yLast + 86) 30 (rgb255 120 230 160) ("Cubies moved: " ++ show (wMoved w))
                 , txt px (yLast + 112) 15 (gray (170 / 255)) "Framed cells changed color between start and finish" ]
            else [])
    moveColor i | not (null plan) && i == wPi w = rgb255 255 0 200
                | not (null plan) && i < wPi w = gray (130 / 255)
                | otherwise = white

-- 手を横に並べ、右端で折り返す（画面の下にはみ出す分は描かない）
layoutMoves :: Float -> Float -> [(Int, Move)] -> ([(Float, Float, Int, Move)], Float)
layoutMoves x0 y0 = go x0 y0
  where
    go _ y [] = ([], y)
    go x y ((i, m) : r)
      | x + w > width - 20 && y + 32 > 590 = ([], y)
      | x + w > width - 20 = go x0 (y + 32) ((i, m) : r)
      | otherwise = let (rest, yEnd) = go (x + w) y r in ((x, y, i, m) : rest, yEnd)
      where w = fromIntegral (length m) * 17 + 14

onEvent :: Event -> World -> World
onEvent (EventKey k Down _ _) w = case k of
  Char c
    | c >= '1' && c <= '6' -> usePreset (fromEnum c - fromEnum '1') w
    | c == '\t' -> nextSlot
    | Just m <- keyMove c -> (editSlot (++ [m]) w) { wPlan = [], wHi = Set.empty, wNote = "" }
    | c == '\b' || c == '\DEL' -> eraseLast
    | c == '0' -> w { wCube = newCube, wPlan = [], wHi = Set.empty }
  SpecialKey KeyTab -> nextSlot
  SpecialKey KeyBackspace -> eraseLast
  SpecialKey KeyDelete -> eraseLast
  SpecialKey KeySpace | wPi w >= length (wPlan w) ->
    w { wBefore = Map.fromList [(sP s, sC s) | s <- wCube w]
      , wPlan = expression w, wPi = 0, wTimer = 0, wHi = Set.empty }
  _ -> w
  where
    nextSlot = w { wSlot = (wSlot w + 1) `mod` 3 }
    eraseLast = (editSlot (reverse . drop 1 . reverse) w) { wPlan = [], wHi = Set.empty }
onEvent _ w = w

-- えらんでいる欄を書きかえる
editSlot :: ([Move] -> [Move]) -> World -> World
editSlot f w = case wSlot w of
  0 -> w { wA = f (wA w) }
  1 -> w { wB = f (wB w) }
  _ -> w { wX = f (wX w) }

-- ---------- やってみよう ----------
-- 1. 例 1 の [R, U] を 6 回実行すると元どおり（11 の位数と同じ）
-- 2. 例 2 を 3 回実行 → 3 サイクルなので元どおり。2 回なら逆向きの 3 サイクル
-- 3. A と B がまったく重ならない（例：A = R, B = L）と [A, B] は何もしない = 可換
-- 4. 例 5 の X を変えて、3 サイクルを好きな 3 つの角に「移動」させよう（これが解法のコツ）
-- 5. 自分で A と B を探して、動くキュービーがいちばん少ない交換子を見つけよう

-- ---------- 道具 ----------
width, height :: Float
width = 1280
height = 800

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y = labelC (x - width / 2) (height / 2 - y)

box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)


-- ==================================================
-- ここから下は「キューブの道具」（10〜13 で共通の仕組み）
-- ==================================================
-- 記号：U=上 D=下 R=右 L=左 F=前 B=後ろ。その面を正面から見て「時計回り」に 90° 回す
--       R' は反時計回り（R の逆操作）、R2 は 180°（R を 2 回）
-- 座標は 2 倍して整数に：小さな立方体（キュービー）の中心は -2, 0, 2、ステッカーは中心 + 法線（面は ±3）
-- 回転は 90° の整数回転だけ → 何回回しても誤差がたまらない

-- 整数の 3 次元座標
data P = P !Int !Int !Int deriving (Eq, Ord)

-- ステッカー 1 枚：今の位置と法線、色、最初の位置と法線
data St = St { sP :: !P, sN :: !P, sC :: !Int, sHome :: !P, sHn :: !P }

type Move = String

-- 面の [軸, 層, 向き]
face :: Char -> (Int, Int, Int)
face 'U' = (1, 1, -1)
face 'D' = (1, -1, 1)
face 'R' = (0, 1, -1)
face 'L' = (0, -1, 1)
face 'F' = (2, 1, -1)
face _   = (2, -1, 1)

-- U D R L F B（白 黄 赤 橙 緑 青）
faceColor :: Int -> Color
faceColor c = case c of
  0 -> rgb255 0xff 0xff 0xff
  1 -> rgb255 0xff 0xd5 0x00
  2 -> rgb255 0xc4 0x1e 0x3a
  3 -> rgb255 0xff 0x58 0x00
  4 -> rgb255 0x00 0x9e 0x60
  _ -> rgb255 0x00 0x51 0xba

comp :: Int -> P -> Int
comp 0 (P x _ _) = x
comp 1 (P _ y _) = y
comp _ (P _ _ z) = z

newCube :: [St]
newCube =
  [ St p n (faceIndex n) p n
  | x <- [-3 .. 3], y <- [-3 .. 3], z <- [-3 .. 3]
  , let vs = [x, y, z]
  , length (filter ((== 3) . abs) vs) == 1
  , length (filter (\v -> v == 0 || abs v == 2) vs) == 2
  , let p = P x y z
  , let n = P (sgn3 x) (sgn3 y) (sgn3 z) ]
  where sgn3 v = if abs v == 3 then signum v else 0

faceIndex :: P -> Int
faceIndex (P x y z)
  | y == 1 = 0
  | y == -1 = 1
  | x == 1 = 2
  | x == -1 = 3
  | z == 1 = 4
  | otherwise = 5

-- 軸 ax のまわりに 90°（d = +1 で右手系の正の向き、-1 で逆）
rot90 :: Int -> Int -> P -> P
rot90 0 d (P x y z) = if d > 0 then P x (-z) y else P x z (-y)
rot90 1 d (P x y z) = if d > 0 then P z y (-x) else P (-z) y x
rot90 _ d (P x y z) = if d > 0 then P (-y) x z else P y (-x) z

-- 手を「面」と「回数」に分ける
parseMove :: Move -> (Char, Int)
parseMove (f : '2' : _) = (f, 2)
parseMove (f : _ : _) = (f, 3)
parseMove (f : _) = (f, 1)
parseMove [] = ('U', 0)

inLayer :: St -> Char -> Bool
inLayer s f = let (ax, layer, _) = face f in comp ax (sP s) * layer >= 2

turn :: Char -> Int -> [St] -> [St]
turn f times st
  | times <= 0 = st
  | otherwise = turn f (times - 1) (map one st)
  where
    (ax, _, d) = face f
    one s | inLayer s f = s { sP = rot90 ax d (sP s), sN = rot90 ax d (sN s) }
          | otherwise = s

doMove :: Move -> [St] -> [St]
doMove m = let (f, t) = parseMove m in turn f t

doSeq :: [Move] -> [St] -> [St]
doSeq seq' st = foldl (flip doMove) st seq'

invMove :: Move -> Move
invMove [f] = [f, '\'']
invMove [f, '\''] = [f]
invMove m = m

invSeq :: [Move] -> [Move]
invSeq = reverse . map invMove

isSolved :: [St] -> Bool
isSolved = all (\s -> sC s == faceIndex (sN s))

-- 'u' → "U"、'U'（Shift）→ "U'"。回す手のキーでなければ Nothing
keyMove :: Char -> Maybe Move
keyMove c
  | c `elem` "udlrfb" = Just [toUpper c]
  | c `elem` "UDLRFB" = Just [c, '\'']
  | otherwise = Nothing

-- ランダムな 1 手
randomFace :: IO Char
randomFace = ("UDLRFB" !!) <$> randomRIO (0, 5)

randomSuffix :: IO String
randomSuffix = (["", "'", "2"] !!) <$> randomRIO (0, 2)

-- ステッカー → 展開図の (面, 行, 列)
netCell :: P -> P -> (Int, Int, Int)
netCell (P x y z) n =
  let f = faceIndex n
      (r, c) = case f of
        0 -> ((z + 2) `div` 2, (x + 2) `div` 2)
        1 -> ((2 - z) `div` 2, (x + 2) `div` 2)
        2 -> ((2 - y) `div` 2, (2 - z) `div` 2)
        3 -> ((2 - y) `div` 2, (z + 2) `div` 2)
        4 -> ((2 - y) `div` 2, (x + 2) `div` 2)
        _ -> ((2 - y) `div` 2, (2 - x) `div` 2)
  in (f, r, c)

-- 展開図での各面の位置（マス）
netPos :: Int -> (Float, Float)
netPos f = case f of
  0 -> (3, 0)
  1 -> (3, 6)
  2 -> (6, 3)
  3 -> (0, 3)
  4 -> (3, 3)
  _ -> (9, 3)

-- 展開図を描く（hi = 強調するマスの集合）
drawNet :: [St] -> Float -> Float -> Float -> Set.Set (Int, Int, Int) -> Picture
drawNet st x0 y0 cs hi = Pictures (concatMap sticker st ++ names)
  where
    sticker s =
      let cell@(f, r, c) = netCell (sP s) (sN s)
          (nx, ny) = netPos f
          x = x0 + (nx + fromIntegral c) * cs
          y = y0 + (ny + fromIntegral r) * cs
      in [ box x y cs cs (gray (20 / 255))
         , box (x + 1) (y + 1) (cs - 2) (cs - 2) (faceColor (sC s)) ]
         ++ (if Set.member cell hi then frameBox (x + 3) (y + 3) (cs - 6) (cs - 6) 4 (rgb255 255 0 200) else [])
    -- 面の名前は中心のマスに（中心のステッカーは面の回転では動かない）
    names =
      [ let (nx, ny) = netPos f
        in txtC (x0 + (nx + 1.5) * cs) (y0 + (ny + 1.5) * cs) (cs * 0.42) (makeColorI 0 0 0 110) nm
      | (f, nm) <- zip [0 ..] ["U", "D", "R", "L", "F", "B"] ]

-- 四角い枠
frameBox :: Float -> Float -> Float -> Float -> Float -> Color -> [Picture]
frameBox x y w h t c =
  [ box x y w t c, box x (y + h - t) w t c, box x y t h c, box (x + w - t) y t h c ]
