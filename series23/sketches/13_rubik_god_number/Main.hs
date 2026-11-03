-- ============================================
-- 13 ルービックキューブ ④ 状態の数と神の数
-- 回して作れる状態は 8!·3⁷·12!·2¹¹ / 2 = 43,252,003,274,489,856,000 通り（Haskell の Integer で計算）
-- ばらして組み立て直すと 12 倍の状態がつくれるが、そのうち 11/12 は回すだけでは絶対にそろわない。3 つの「保存量」のせい
-- u d l r f b（Shift で '）= 回す / スペース = かき混ぜる / 0 = リセット
-- T = 角を 1 個だけひねる / E = 辺を 1 個だけ裏返す / W = 辺を 2 個だけ入れかえる（どれも「反則」）
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toUpper)
import Data.List (elemIndex, findIndex, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data World = World
  { wCube    :: ![St]
  , wIllegal :: ![String]  -- やった反則の記録（新しい順）
  }

main :: IO ()
main = runSketch (sketch "23-13 Rubik God Number" (World newCube []))
  { skBack  = rgb255 20 24 30
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \_ ev w -> onEvent ev w
  }

-- 階乗（Integer なのでいくらでも大きな数が正確に計算できる）
fact :: Integer -> Integer
fact n = product [1 .. n]

-- カンマ区切りの文字列にする
fmt :: Integer -> String
fmt v = reverse (go (reverse (show v)))
  where go s = case splitAt 3 s of
          (a, []) -> a
          (a, b) -> a ++ "," ++ go b

drawWorld :: World -> Picture
drawWorld w = Pictures ([drawNet (wCube w) x0 y0 cs Set.empty] ++ hud ++ rowsPic ++ verdict ++ panel)
  where
    cs = min (width * 0.46 / 12) ((height - 300) / 9)
    x0 = 30 :: Float
    y0 = 110 :: Float
    Inv twist flipSum cp ep = invariants (wCube w)
    green' = rgb255 120 230 160
    red' = rgb255 255 110 110
    pyL = y0 + cs * 9 + 40
    parName v = if v /= (0 :: Int) then "odd" else "even"
    rows =
      [ ("corner twist sum mod 3", show twist, twist == 0)
      , ("edge flip sum mod 2", show flipSum, flipSum == 0)
      , ("corner parity / edge parity", parName cp ++ " / " ++ parName ep, cp == ep) ]
    ok = all (\(_, _, b) -> b) rows
    hud =
      [ txt 20 36 16 white "u d l r f b (Shift = '): turn   Space: scramble   0: reset   T: twist corner   E: flip edge   W: swap 2 edges"
      , txt 20 62 14 (gray (170 / 255)) "Plain turns never change the 3 values below (invariants). Cheats change them -> turning alone can no longer solve it"
      ]
    rowsPic =
      [ txt 30 (pyL + fromIntegral i * 32) 18 (if b then green' else red') ((if b then "O " else "X ") ++ nm ++ " = " ++ v)
      | (i, (nm, v, b)) <- zip [0 :: Int ..] rows ]
    verdict =
      [ txt 30 (pyL + 112) 26 (if ok then green' else red')
          (if ok then (if isSolved (wCube w) then "Solved" else "Solvable by turning") else "Turning alone can NEVER solve this!") ]
      ++ [ txt 30 (pyL + 140) 15 (gray (170 / 255)) ("Cheats: " ++ unwords (reverse (take 10 (wIllegal w)))) | not (null (wIllegal w)) ]
    -- 状態の数（Integer）
    px = x0 + cs * 12 + 50
    corners = fact 8 * 3 ^ (7 :: Int)
    edges = fact 12 * 2 ^ (11 :: Int)
    total = corners * edges `div` 2
    allStates = fact 8 * 3 ^ (8 :: Int) * fact 12 * 2 ^ (12 :: Int)
    light = gray 0.78
    panel =
      [ txt px 110 20 white "Number of states reachable by turning"
      , txt px 146 14 light ("8 corners: orders 8! = " ++ fmt (fact 8) ++ " x orientations 3^7 = " ++ fmt (3 ^ (7 :: Int)))
      , txt px 172 14 light ("12 edges: orders 12! = " ++ fmt (fact 12) ++ " x flips 2^11 = " ++ fmt (2 ^ (11 :: Int)))
      , txt px 198 14 light "Not 3^8 and 2^12: the last piece's direction is decided by the rest"
      , txt px 224 14 light "/ 2: corner parity and edge parity are always equal"
      , txt px 264 15 (rgb255 255 210 90) "8! * 3^7 * 12! * 2^11 / 2 ="
      , txt px 302 30 (rgb255 255 210 90) (fmt total)
      , txt px 332 16 light "About 4.3 x 10^19. At one per second: over a trillion years"
      , txt px 372 16 light "Take it apart and reassemble:"
      , txt px 396 16 light (fmt allStates ++ " states")
      , txt px 436 16 light "-> exactly 12 times (= 3 x 2 x 2)."
      , txt px 460 16 light "Only 1/12 of those can be solved"
      , txt px 510 19 (rgb255 130 200 255) "God's number = 20"
      ] ++
      [ txt px (536 + fromIntegral i * 20) 15 light s
      | (i, s) <- zip [0 :: Int ..] (wrapAt 54 "Any state can be solved within 20 moves (a half turn counts as 1), and some states need all 20 (computer proof, 2010)") ]

-- 3 つの保存量
data Inv = Inv !Int !Int !Int !Int

-- 3 つの保存量を、ステッカーの色から計算する
invariants :: [St] -> Inv
invariants st =
  let col = Map.fromList [(sP s, sC s) | s <- st]
      colAt p = Map.findWithDefault 0 p col
      ud c = c == 0 || c == 1            -- 白・黄
      fb c = c == 4 || c == 5            -- 緑・青
      -- 角：U/D 向きのステッカーから時計回りに並べ、白か黄がどこにあるか（0, 1, 2）
      cornerData =
        [ (fromMaybe 0 (findIndex ud cs), sort cs)
        | sx <- [-1, 1], sy <- [-1, 1], sz <- [-1, 1]
        , let c = P (2 * sx) (2 * sy) (2 * sz)
              n0 = P 0 sy 0
              n1 = P sx 0 0
              n2 = P 0 0 sz
              ns = if sx * sy * sz < 0 then [n0, n2, n1] else [n0, n1, n2]  -- 向きをそろえる（外から見て同じ回り）
              cs = map (colAt . plusP c) ns ]
      -- 辺：基準のマス（U/D 面、なければ F/B 面）に白・黄（なければ緑・青）があれば 0
      edgeData =
        [ (if ud a || (not (ud b) && fb a) then 0 else 1 :: Int, sort [a, b])
        | (c, nRef, nOther) <- edgeSlots
        , let a = colAt (plusP c nRef)
              b = colAt (plusP c nOther) ]
  in Inv (sum (map fst cornerData) `mod` 3) (sum (map fst edgeData) `mod` 2)
         (parityOf homeCorners (map snd cornerData)) (parityOf homeEdges (map snd edgeData))

plusP :: P -> P -> P
plusP (P a b c) (P d e f) = P (a + d) (b + e) (c + f)

edgeSlots :: [(P, P, P)]
edgeSlots =
  [ if ref == (0 :: Int) then (c, n1, n2) else (c, n2, n1)
  | a <- [0, 1, 2 :: Int], s1 <- [-1, 1], s2 <- [-1, 1]
  , let ax0 = (a + 1) `mod` 3
        ax1 = (a + 2) `mod` 3
        c = setComp ax0 (2 * s1) (setComp ax1 (2 * s2) (P 0 0 0))
        n1 = setComp ax0 s1 (P 0 0 0)
        n2 = setComp ax1 s2 (P 0 0 0)
        -- 基準：y の面があれば y、なければ z
        ref | ax0 == 1 || ax1 == 1 = if ax0 == 1 then 0 else 1
            | otherwise = if ax0 == 2 then 0 else 1 ]

setComp :: Int -> Int -> P -> P
setComp 0 v (P _ y z) = P v y z
setComp 1 v (P x _ z) = P x v z
setComp _ v (P x y _) = P x y v

-- そろった状態での各位置のピース（色の組）
homeCorners, homeEdges :: [[Int]]
homeCorners =
  [ sort (map (colHome . plusP (P (2 * sx) (2 * sy) (2 * sz))) [P sx 0 0, P 0 sy 0, P 0 0 sz])
  | sx <- [-1, 1], sy <- [-1, 1], sz <- [-1, 1] ]
homeEdges = [ sort [colHome (plusP c n1), colHome (plusP c n2)] | (c, n1, n2) <- edgeSlots ]

colHome :: P -> Int
colHome p = Map.findWithDefault 0 p (Map.fromList [(sP s, sC s) | s <- newCube])

-- 「どの位置にどのピースがあるか」の置換の偶奇（巡回の数から）
parityOf :: [[Int]] -> [[Int]] -> Int
parityOf home cur =
  let perm = Map.fromList (zip [0 ..] (map (\k -> fromMaybe (-1) (elemIndex k home)) cur))
      n = length cur
      go seen i cnt
        | i >= n = cnt
        | Set.member i seen = go seen (i + 1) cnt
        | Map.findWithDefault (-1) i perm < 0 = go seen (i + 1) cnt
        | otherwise = go (mark seen i) (i + 1) (cnt + 1)
      mark seen j
        | j < 0 || Set.member j seen = seen
        | otherwise = mark (Set.insert j seen) (Map.findWithDefault (-1) j perm)
  in (n - go (Set.empty :: Set.Set Int) (0 :: Int) (0 :: Int)) `mod` 2

-- 反則：位置 p のステッカーの色を直接書きかえる
colorSwap :: [(P, P)] -> [St] -> [St]
colorSwap pairs st =
  let col = Map.fromList [(sP s, sC s) | s <- st]
      newCol = Map.fromList [(to, Map.findWithDefault 0 from col) | (from, to) <- pairs]
  in [s { sC = Map.findWithDefault (sC s) (sP s) newCol } | s <- st]

-- UFR の角の 3 色を回す
twistCorner :: [St] -> [St]
twistCorner = colorSwap [(c, a), (a, b), (b, c)]
  where a = P 2 3 2
        b = P 3 2 2
        c = P 2 2 3

-- UF の辺の 2 色を入れかえる
flipEdge :: [St] -> [St]
flipEdge = colorSwap [(a, b), (b, a)]
  where a = P 0 3 2
        b = P 0 2 3

-- UF と UR の辺を入れかえる
swapEdges :: [St] -> [St]
swapEdges = colorSwap [(a1, b1), (b1, a1), (a2, b2), (b2, a2)]
  where a1 = P 0 3 2
        a2 = P 0 2 3
        b1 = P 2 3 0
        b2 = P 3 2 0

onEvent :: Event -> World -> IO World
onEvent (EventKey k Down _ _) w = case k of
  Char c
    | Just m <- keyMove c -> pure w { wCube = doMove m (wCube w) }
    | c == 't' || c == 'T' -> pure (cheat twistCorner "twist")
    | c == 'e' || c == 'E' -> pure (cheat flipEdge "flip")
    | c == 'w' || c == 'W' -> pure (cheat swapEdges "swap")
    | c == '0' -> pure (World newCube [])
  SpecialKey KeySpace -> do
    ms <- replicateM 25 ((:) <$> randomFace <*> randomSuffix)
    pure w { wCube = doSeq ms (wCube w) }
  _ -> pure w
  where cheat f nm = w { wCube = f (wCube w), wIllegal = nm : take 30 (wIllegal w) }
onEvent _ w = pure w

-- ---------- やってみよう ----------
-- 1. T を押してから好きなだけ回す → ねじれの合計はずっと 1 のまま。T を 3 回でまた ○ になる
-- 2. E と W を両方やると？ 2 つの × が同時に ○ にもどるか確かめよう
-- 3. 2×2×2 キューブなら？ 角だけなので 8!·3⁷ / 24（全体の向き）= 3,674,160 通り。Integer で計算してみよう
-- 4. 「1 手で行ける状態は 18 通り」。n 手で行ける状態は最大 18·15ⁿ⁻¹。何手で 4.3×10¹⁹ を超える？（神の数の下限の考え方）

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
