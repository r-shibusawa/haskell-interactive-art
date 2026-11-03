-- ============================================
-- 10 ルービックキューブ ① 3D キューブを回す
-- 3×3×3 のキューブを 6 つの面の回転（U D L R F B）だけで動かす。回転は 90° の整数回転なのでずれない
--   記号：U=上 D=下 L=左 R=右 F=前 B=後ろ を時計回り、' は反時計回り（逆操作）
-- u d l r f b = 時計回り / Shift + 同じキー（大文字）= 反時計回り / スペース = 20 手でかき混ぜる
-- Backspace か Z = 1 手もどす / 0 = そろった状態にもどす / マウスドラッグ = 視点を回す / ホイール = 近づく・離れる
-- ============================================
module Main (main) where

import Art
import Data.Char (toUpper)

turnFrames, uPx :: Float
turnFrames = 12          -- ★ 1 手にかけるフレーム数（小さいほど速い）
uPx = 54                 -- ★ 1 単位の大きさ（px）。キュービー 1 個 = 2 単位

scrambleLen :: Int
scrambleLen = 20         -- ★ かき混ぜる手数

data World = World
  { wCube  :: ![St]                  -- 54 枚のステッカー（下の「キューブの道具」）
  , wQueue :: ![Move]                -- これから回す手（アニメーション待ち）
  , wAnim  :: !(Maybe (Move, Float)) -- 今回している手と進み具合 0〜1
  , wHist  :: ![Move]                -- 回した手の記録（新しい順）
  , wYaw   :: !Float                 -- 視点（左右）
  , wPitch :: !Float                 -- 視点（上下）
  , wDist  :: !Float                 -- カメラまでの距離
  , wDrag  :: !(Maybe Point)         -- ドラッグ中の前のマウス位置
  }

-- p5 版のカメラ (520, 430, 640) から原点を見たときと同じ向き
world0 :: World
world0 = World newCube [] Nothing [] (atan2 520 640) (atan2 430 (sqrt (520 * 520 + 640 * 640)))
               (sqrt (520 * 520 + 430 * 430 + 640 * 640)) Nothing

main :: IO ()
main = runSketch (sketch "23-10 Rubik 3D" world0)
  { skBack  = rgb255 22 24 32
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = onEvent
  }

-- アニメーションを進める（待ちが多いときは速く）
step :: World -> World
step w0 =
  let w1 = case (wAnim w0, wQueue w0) of
        (Nothing, m : r) -> w0 { wAnim = Just (m, 0), wQueue = r }
        _ -> w0
  in case wAnim w1 of
       Nothing -> w1
       Just (m, t) ->
         let t' = t + (if length (wQueue w1) > 2 then 3 else 1) / turnFrames
         in if t' >= 1
              then w1 { wCube = doMove m (wCube w1), wAnim = Nothing }  -- ここで整数の回転を本当に適用
              else w1 { wAnim = Just (m, t') }

-- 1 枚の四角形（立体の座標の 4 点と色）と、その中心と外向きの法線
data Quad = Quad [V3] !Color !V3 !V3

toV3 :: P -> V3
toV3 (P x y z) = V3 (fromIntegral x) (fromIntegral y) (fromIntegral z)

-- 中心 p・法線 n・半分の大きさ h の正方形を、軸 ax まわりに ang 回す
quadAt :: P -> P -> Float -> Float -> Float -> Int -> Color -> Quad
quadAt p n@(P nx ny _) h lift ang ax col =
  let na | nx /= 0 = 0
         | ny /= 0 = 1
         | otherwise = 2 :: Int
      unitOn i = case i `mod` 3 of
        0 -> V3 1 0 0
        1 -> V3 0 1 0
        _ -> V3 0 0 1
      u = unitOn (na + 1)
      v = unitOn (na + 2)
      nv = toV3 n
      c = add3 (toV3 p) (scale3 lift nv)
      corner a b = add3 c (add3 (scale3 (a * h) u) (scale3 (b * h) v))
      tf = turnFloat ax ang
  in Quad (map tf [corner (-1) (-1), corner 1 (-1), corner 1 1, corner (-1) 1]) col (tf c) (tf nv)

-- 見た目だけの回転（小数）。rot90 と同じ向き
turnFloat :: Int -> Float -> V3 -> V3
turnFloat ax a q
  | ax < 0 || a == 0 = q
  | ax == 0 = rotX a q
  | ax == 1 = rotY a q
  | otherwise = rotZ a q

drawWorld :: World -> Picture
drawWorld w = Pictures [cubePicture w, hud w]

-- 今回している層だけ、見た目の上で回して描く。
-- 回る層と止まっている部分はどちらも凸な立体で、切れ目の平面で分かれている。
-- だから「裏向きの面を捨てる」と「カメラから遠いかたまりを先に描く」だけで正しく重なる（画家のアルゴリズム）
cubePicture :: World -> Picture
cubePicture w = Pictures (concatMap (map drawQuad . filter visible) ordered)
  where
    d = wDist w
    view = rotX (wPitch w) . rotY (-wYaw w)    -- 立体の座標 → カメラの座標
    cam = V3 0 0 d
    screen v =
      let (x, y) = project d (scale3 uPx (view v))
          k = 800 / d                           -- p5 の標準カメラと同じ画角
      in (x * k, y * k)
    visible (Quad _ _ c n) = dot3 (view n) (sub3 cam (scale3 uPx (view c))) > 0
    drawQuad (Quad ps col _ _) = Color col (Polygon (map screen ps))
    black1 = rgb255 12 12 14
    stickerQuads a ax s =
      [ quadAt (sP s) (sN s) 1 0 a ax black1                     -- 黒い台紙
      , quadAt (sP s) (sN s) 0.86 0.03 a ax (faceColor (sC s)) ] -- 色のステッカー
    ordered = case wAnim w of
      Nothing -> [concatMap (stickerQuads 0 (-1)) (wCube w)]
      Just (m, t) ->
        let (f, times) = parseMove m
            (a, layer, dir) = face f
            e = t * t * (3 - 2 * t)                        -- なめらかに加速・減速
            k | times == 2 = 2
              | times == 3 = -1
              | otherwise = 1 :: Float
            ang = fromIntegral dir * k * pi / 2 * e
            capP = setComp a layer (P 0 0 0)
            capCol = rgb255 8 8 10
            -- 回転中は、切れ目に黒いふたを 2 枚（回る側と止まっている側）
            moving = quadAt capP (setComp a (-layer) (P 0 0 0)) 3 0 ang a capCol
                     : concatMap (stickerQuads ang a) (filter (`inLayer` f) (wCube w))
            still = quadAt capP (setComp a layer (P 0 0 0)) 3 0 0 a capCol
                    : concatMap (stickerQuads 0 a) (filter (not . (`inLayer` f)) (wCube w))
            -- カメラが回る層の側にあれば、止まっている部分を先に描く
            camInCube = rotY (wYaw w) (rotX (-wPitch w) cam)
            camSide = (axisOf a camInCube / uPx - fromIntegral layer) * fromIntegral layer
        in if camSide > 0 then [still, moving] else [moving, still]

setComp :: Int -> Int -> P -> P
setComp 0 v (P _ y z) = P v y z
setComp 1 v (P x _ z) = P x v z
setComp _ v (P x y _) = P x y v

axisOf :: Int -> V3 -> Float
axisOf 0 (V3 x _ _) = x
axisOf 1 (V3 _ y _) = y
axisOf _ (V3 _ _ z) = z

hud :: World -> Picture
hud w = Pictures $
  [ txt 20 36 16 white "u d l r f b: clockwise  Shift: reverse (')  Space: scramble  Backspace/Z: undo  0: reset  Drag: view"
  , txt 20 62 15 (gray (170 / 255)) "U D L R F B = up down left right front back: turn 90 deg clockwise facing it. R' = reverse, R2 = twice"
  , txt 20 (height - 90) 16 white ("Moves " ++ show (length (wHist w)) ++ (if length (wHist w) > 36 then " (latest 36)" else ""))
  ] ++
  [ txt 20 (height - 58 + fromIntegral i * 26) 18 white s
  | (i, s) <- zip [0 :: Int ..] (wrapAt 66 (unwords (reverse (take 36 (wHist w))))) ] ++
  [ txt (width - 280) (height - 46) 30 (rgb255 120 230 160) (if null (wHist w) then "Solved state" else "Solved!")
  | isSolved (wCube w), null (wQueue w), Nothing <- [wAnim w] ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char c) Down _ _
    | Just m <- keyMove c -> pure w { wQueue = wQueue w ++ [m], wHist = m : wHist w }
    | c == 'z' || c == 'Z' || c == '\b' || c == '\DEL' -> pure (undo w)
    | c == '0' -> pure w { wCube = newCube, wQueue = [], wAnim = Nothing, wHist = [] }
  EventKey (SpecialKey KeyBackspace) Down _ _ -> pure (undo w)
  EventKey (SpecialKey KeyDelete) Down _ _ -> pure (undo w)
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    ms <- scramble scrambleLen ' '
    pure w { wQueue = wQueue w ++ ms, wHist = reverse ms ++ wHist w }
  EventKey (MouseButton LeftButton) Down _ p -> pure w { wDrag = Just p }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = Nothing }
  EventKey (MouseButton WheelUp) Down _ _ -> pure w { wDist = max 500 (wDist w * 0.95) }
  EventKey (MouseButton WheelDown) Down _ _ -> pure w { wDist = min 2500 (wDist w / 0.95) }
  EventMotion p@(x, y) -> pure $ case wDrag w of
    Just (x0, y0) -> w { wYaw = wYaw w - (x - x0) * 0.008
                       , wPitch = clampF (-1.5) 1.5 (wPitch w - (y - y0) * 0.008)
                       , wDrag = Just p }
    Nothing -> w
  _ -> pure w

-- 同じ面を続けないように、かき混ぜる手をつくる
scramble :: Int -> Char -> IO [Move]
scramble 0 _ = pure []
scramble n lastF = do
  f <- randomFace
  if f == lastF then scramble n lastF else do
    s <- randomSuffix
    ((f : s) :) <$> scramble (n - 1) f

-- 1 手もどす
undo :: World -> World
undo w = case wHist w of
  [] -> w
  m : r
    | take 1 (reverse (wQueue w)) == [m] -> w { wHist = r, wQueue = reverse (drop 1 (reverse (wQueue w))) } -- まだ回していなければ取り消すだけ
    | otherwise -> w { wHist = r, wQueue = wQueue w ++ [invMove m] }   -- 回した後なら逆の手を回す

-- ---------- やってみよう ----------
-- 1. R を 4 回押すと元どおり。では R U を何回くり返すと元にもどる？（答えは 11 で）
-- 2. R U R' U' を 6 回。途中の形を観察しよう（交換子 → 12）
-- 3. スペースでかき混ぜて、Backspace を連打すると逆の手順で元にもどる：(AB)⁻¹ = B⁻¹A⁻¹
-- 4. turnFrames を 3 にすると超高速。uPx を変えて大きさを変えよう
-- 5. 中段（M 層）を回す手を追加するには？ inLayer の条件 |座標| ≥ 2 を「= 0」にした層を回す

-- ---------- 道具 ----------
width, height :: Float
width = 1280
height = 800

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

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
