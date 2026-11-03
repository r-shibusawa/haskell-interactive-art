-- ============================================
-- 11 ルービックキューブ ② 操作の位数
-- 手順（例：R U）を何回もくり返すと、必ずいつか元にもどる。その回数が「位数」
--   位数 = ステッカーの動き（置換）を巡回に分けたときの、長さの最小公倍数
-- 1〜6 = 手順の例 / u d l r f b（Shift で '）= 手順を自分でつくる / Backspace = 1 手消す / スペース = 1 回だけ適用
-- A = 元にもどるまで自動でくり返す / ↑↓ = 自動の速さ / V = 動きの矢印 / 0 = リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (toUpper)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ★ 手順の例
presets :: [[Move]]
presets = map words ["R", "R U", "R U R' U'", "R2 U2", "R U'", "R U2 D' B D'"]

-- 1 回分の置換の分析
data Info = Info
  { iOrder  :: !Int
  , iLines  :: ![String]
  , iLens   :: ![Int]
  , iArrows :: ![(P, P, P, P)]  -- 動くステッカー（元の位置と法線、行き先と法線）
  }

data World = World
  { wSeq    :: ![Move]
  , wCube   :: ![St]    -- 今の状態
  , wReps   :: !Int     -- くり返した回数
  , wAuto   :: !Bool
  , wSpeed  :: !Int     -- ★ 1 フレームに何回くり返すか（↑↓）
  , wInfo   :: !Info
  , wArrows :: !Bool
  , wFlash  :: !Int
  , wDone   :: !String
  }

resetAll :: World -> World
resetAll w = w { wCube = newCube, wReps = 0, wAuto = False, wDone = "", wInfo = analyze (wSeq w) }

world0 :: World
world0 = resetAll (World (words "R U") newCube 0 False 1 (Info 1 [] [1] []) True 0 "")

main :: IO ()
main = runSketch (sketch "23-11 Rubik Order" world0)
  { skBack  = rgb255 26 26 36
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

step :: World -> World
step w0 =
  let w1 = if wFlash w0 > 0 then w0 { wFlash = wFlash w0 - 1 } else w0
      go :: Int -> World -> World
      go 0 v = v
      go i v | wAuto v = go (i - 1) (applyOnce v)
             | otherwise = v
  in if wAuto w1 && not (null (wSeq w1)) then go (wSpeed w1) w1 else w1

-- 1 回だけ手順を適用して、そろったか調べる
applyOnce :: World -> World
applyOnce w
  | null (wSeq w) = w
  | isSolved c = w { wCube = c, wReps = r, wAuto = False, wDone = show r ++ " times: back to start!", wFlash = 30 }
  | otherwise = w { wCube = c, wReps = r, wDone = "" }
  where
    c = doSeq (wSeq w) (wCube w)
    r = wReps w + 1

-- 手順 1 回分を、ステッカーの置換・キュービーの巡回として分析
analyze :: [Move] -> Info
analyze s =
  let st = doSeq s newCube
      to = Map.fromList [(sHome t, sP t) | t <- st]             -- ステッカーの元の場所 → 行き先
      -- ステッカーの巡回の長さ
      stLen = fst (foldl' stickerCycle (Map.empty, Set.empty) (map sHome st))
      stickerCycle (m, seen) k
        | Set.member k seen = (m, seen)
        | otherwise =
            let cyc = walk to k
            in (foldl' (\mm c -> Map.insert c (length cyc) mm) m cyc, foldl' (flip Set.insert) seen cyc)
      -- キュービー（中心 = ステッカー − 法線）の巡回
      cto = Map.fromList [(minusP (sHome t) (sHn t), minusP (sP t) (sN t)) | t <- st]
      rep = Map.fromList [(minusP (sHome t) (sHn t), sHome t) | t <- st]
      cubies = dedupe [minusP (sHome t) (sHn t) | t <- st]      -- ステッカーの順に
      (rowsRev, _) = foldl' cubieCycle ([], Set.empty) cubies
      cubieCycle (acc, seen) h
        | Set.member h seen = (acc, seen)
        | otherwise =
            let cyc = walk cto h
                seen' = foldl' (flip Set.insert) seen cyc
                eff = Map.findWithDefault 1 (Map.findWithDefault h h rep) stLen
            in if eff == 1 then (acc, seen') else
               let P hx hy hz = h
                   isCorner = length (filter (/= 0) [hx, hy, hz]) == 3
                   kind = if isCorner then "corner" else "edge"
                   tw = eff `div` length cyc
                   twist | tw > 1 = " x" ++ show tw ++ (if isCorner then " twist" else " flip")
                         | otherwise = ""
                   line' = kind ++ " (" ++ show (length cyc) ++ ")" ++ twist ++ ": " ++ intercalate " -> " (map cubieName cyc)
               in ((line', eff) : acc, seen')
      rows = reverse rowsRev
      lens = map snd rows
      order = foldl' lcm 1 lens
  in Info order (map fst rows) (if null lens then [1] else lens)
       [(sHome t, sHn t, sP t, sN t) | t <- st, sHome t /= sP t]

-- k から写像をたどって一周する
walk :: Map.Map P P -> P -> [P]
walk m k = k : takeWhile (/= k) (drop 1 (iterate (\c -> Map.findWithDefault k c m) k))

dedupe :: [P] -> [P]
dedupe = go Set.empty
  where go _ [] = []
        go s (x : r) | Set.member x s = go s r
                     | otherwise = x : go (Set.insert x s) r

minusP :: P -> P -> P
minusP (P a b c) (P d e f) = P (a - d) (b - e) (c - f)

cubieName :: P -> String
cubieName (P x y z) =
  (if y > 0 then "U" else if y < 0 then "D" else "")
  ++ (if z > 0 then "F" else if z < 0 then "B" else "")
  ++ (if x > 0 then "R" else if x < 0 then "L" else "")

drawWorld :: World -> Picture
drawWorld w = Pictures ([drawNet (wCube w) x0 y0 cs Set.empty] ++ arrows ++ flash ++ hud)
  where
    cs = min (width * 0.56 / 12) ((height - 200) / 9)
    x0 = 30 :: Float
    y0 = 130 :: Float
    info = wInfo w
    arrows | wArrows w = concatMap arrowOf (iArrows info)
           | otherwise = []
    -- 展開図の上に「このステッカーはここへ行く」矢印
    arrowOf (hp, hn, p, n) =
      let a@(ax, ay) = cellCenter hp hn
          b@(bx, by) = cellCenter p n
          ang = atan2 (by - ay) (bx - ax)
      in [ Color (rgba255 255 0 200 150) (thickLine 2 (pt a) (pt b))
         , Color (rgba255 255 0 200 200) (Polygon (map pt
             [ b, (bx - 9 * cos (ang - 0.4), by - 9 * sin (ang - 0.4)), (bx - 9 * cos (ang + 0.4), by - 9 * sin (ang + 0.4)) ])) ]
    cellCenter p n =
      let (f, r, c) = netCell p n
          (nx, ny) = netPos f
      in (x0 + (nx + fromIntegral c + 0.5) * cs, y0 + (ny + fromIntegral r + 0.5) * cs)
    flash = [ box x0 y0 (cs * 12) (cs * 9) (rgba255 255 255 255 (wFlash w * 6)) | wFlash w > 0 ]
    -- 文字
    px = x0 + cs * 12 + 40
    py = 120 :: Float
    seqStr = if null (wSeq w) then "(empty)" else unwords (wSeq w)
    seqRows | length seqStr <= 19 = [txt px (py + 40) 34 white seqStr]
            | otherwise = [ txt px (py + 34 + fromIntegral i * 26) 20 white s
                          | (i, s) <- zip [0 :: Int ..] (take 3 (wrapAt 30 seqStr)) ]
    detail = concatMap (wrapAt 44) (iLines info)
    shownDetail = take 12 detail
    yDetail = py + 354
    yEnd = yDetail + fromIntegral (length shownDetail + (if length detail > 12 then 1 else 0)) * 20
    hud =
      [ txt 20 28 16 white "1-6: examples   u d l r f b (Shift = '): build a sequence   Backspace: erase   Space: apply once"
      , txt 20 50 16 white ("A: auto (" ++ (if wAuto w then "ON" else "off") ++ ")   Up/Down: speed " ++ show (wSpeed w) ++ "   V: arrows   0: reset")
      , txt 20 76 15 (gray (170 / 255)) "A cube has finitely many states, so repeating a sequence must return to the start. How many times? Its order"
      , txt px py 16 white "Sequence"
      ] ++ seqRows ++
      [ txt px (py + 110) 16 (rgb255 255 210 90) "Times repeated"
      , txt px (py + 166) 56 (rgb255 255 210 90) (show (wReps w))
      , txt px (py + 204) 22 (rgb255 120 230 160) (wDone w)
      , txt px (py + 250) 16 white "Order (computed)"
      , txt px (py + 294) 40 (rgb255 130 200 255) (show (iOrder info))
      , txt px (py + 330) 15 (gray 0.78) "Cubie cycles moved by one application:"
      ] ++
      [ txt px (yDetail + fromIntegral i * 20) 14 (gray 0.78) s | (i, s) <- zip [0 :: Int ..] shownDetail ] ++
      [ txt px (yDetail + 12 * 20) 14 (gray 0.78) "..." | length detail > 12 ] ++
      [ txt px (yEnd + 6) 14 (gray (170 / 255)) ("Order = lcm(" ++ intercalate ", " (map show (iLens info)) ++ ") = " ++ show (iOrder info))
      , txt px (yEnd + 26) 14 (gray (170 / 255)) "Twists (x3) and flips (x2) make cycles longer"
      ]

onEvent :: Event -> World -> World
onEvent (EventKey k Down _ _) w = case k of
  Char c
    | c >= '1' && c <= '6' -> resetAll w { wSeq = presets !! (fromEnum c - fromEnum '1') }
    | Just m <- keyMove c -> resetAll w { wSeq = wSeq w ++ [m] }
    | c == '\b' || c == '\DEL' -> eraseLast
    | c == 'a' || c == 'A' -> w { wAuto = not (wAuto w), wDone = "" }
    | c == 'v' || c == 'V' -> w { wArrows = not (wArrows w) }
    | c == '0' -> resetAll w
  SpecialKey KeyBackspace -> eraseLast
  SpecialKey KeyDelete -> eraseLast
  SpecialKey KeySpace -> applyOnce w
  SpecialKey KeyUp -> w { wSpeed = min 64 (wSpeed w * 2) }
  SpecialKey KeyDown -> w { wSpeed = max 1 (wSpeed w `div` 2) }
  _ -> w
  where eraseLast = resetAll w { wSeq = reverse (drop 1 (reverse (wSeq w))) }
onEvent _ w = w

-- ---------- やってみよう ----------
-- 1. 予想してから A：R は 4、R U は？ R U R' U' は？（答え：105 と 6）
-- 2. 6 の R U2 D' B D' は 1260 回！ ↑ で速くして確かめよう（3×3×3 で最大級の位数）
-- 3. R2 U2 の巡回を見る。辺の 3 サイクル × 2 と角の 2 サイクル… lcm で位数が決まる
-- 4. 自分の手順で位数がいちばん大きいものを探そう。位数が奇数になる手順は？
-- 5. 位数 n の手順を n − 1 回くり返すと、その手順の「逆」になる（A⁻¹ = Aⁿ⁻¹）

-- ---------- 道具 ----------
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Point -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y = labelC (x - width / 2) (height / 2 - y)

box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)

-- 長い文字列を、1 行 m 文字くらいで折り返す
wrapAt :: Int -> String -> [String]
wrapAt m = go "" . words
  where
    go cur [] = [cur | not (null cur)]
    go "" (x : r) = go x r
    go cur (x : r)
      | length cur + 1 + length x > m = cur : go x r
      | otherwise = go (cur ++ " " ++ x) r

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
