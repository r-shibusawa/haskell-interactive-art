-- ============================================
-- 13 モルフォロジーを聞く
-- 描いた形を「収縮 → 収縮 → … → 膨張 → 膨張 → …」と脈打たせ、面積を音の高さと大きさにする
-- 形がちぎれて塊の数がふえた瞬間にポンと鳴る。細い所の多い形ほど「にぎやかな曲」になる
-- スペース（またはマス目の外をクリック）で再生／停止 / ドラッグで描く / 1〜3 でお手本 / マウス左右 = テンポ / 左右で収縮の深さ / C で消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, when)
import Control.Monad.ST (runST)
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

cols, rows :: Int
cols = 96
rows = 64

ox, oy :: Float
ox = 20
oy = 110

data World = World
  { wOrig      :: !Grid
  , wImg       :: !Grid
  , wDepth     :: !Int     -- ★ 何回収縮してから戻るか
  , wPhase     :: !Int     -- 0 = 収縮中, 1 = 膨張中
  , wK         :: !Int
  , wPlaying   :: !Bool
  , wLastStep  :: !Float   -- 最後に動いた時刻（秒）
  , wLastComps :: !Int
  , wHistory   :: ![Int]   -- 面積の履歴（新しい順、最大 120 個）
  , wMaxArea   :: !Int
  }

main :: IO ()
main = runSketch (sketch "19-13 Hearing Morphology" (preset 1 world0))
  { skBack  = hsb 170 45 9
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

world0 :: World
world0 = World e e 6 0 0 False 0 0 [] 1
  where e = U.replicate (cols * rows) False

se :: [(Int, Int)]
se = makeSE Disk 1  -- ★ 1 回あたりの SE

preset :: Int -> World -> World
preset n w = restart w { wOrig = img }
  where
    img = case n of
      1 -> textToGrid "on" 60  -- 漢字の「音」（道具の glyphs を見よ）
      2 -> gridFrom $ \x y ->
             distI x y 28 32 < 15 || distI x y 62 32 < 15 || (x > 28 && x < 62 && abs (y - 32) < 3)
      _ -> gridFrom $ \x y ->
             let d = distI x y 48 32 in (d < 24 && d > 14) || (abs (x - 48) < 2 && y > 8 && y < 56)

restart :: World -> World
restart w = w
  { wImg = wOrig w, wMaxArea = max 1 (areaOf (wOrig w)), wPhase = 0, wK = 0, wHistory = []
  , wLastComps = countComponents (wOrig w) }

-- テンポ（ミリ秒）
tempoOf :: Env -> Float
tempoOf env = clampF 60 350 (remap (fst (mouseXY env)) 0 (envW env) 350 60)  -- ★ テンポ

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | wPlaying w && (envTime env - wLastStep w) * 1000 > tempo = stepMorph tempo w { wLastStep = envTime env }
  | otherwise = pure w
  where tempo = tempoOf env

-- ★ 1 ステップ：収縮を depth 回、そのあと膨張を depth 回、くり返す
stepMorph :: Float -> World -> IO World
stepMorph tempo w = do
  let w1 | wPhase w == 0 =
             let img = erodeImg se (wImg w)
                 k = wK w + 1
             in w { wImg = img, wK = k, wPhase = if k >= wDepth w || areaOf img == 0 then 1 else 0 }
         | otherwise =
             let k = wK w - 1
             in if k <= 0 then w { wImg = wOrig w, wK = k, wPhase = 0 }
                else w { wImg = dilateImg se (wImg w), wK = k }
      area = areaOf (wImg w1)
      comps = countComponents (wImg w1)
      ratio = fi area / fi (wMaxArea w1)
  -- ★ 音：面積 → 高さ（3 オクターブ）と大きさ。塊がふえたらポン
  let f = 110 * 2 ** (ratio * 3)
      vol = 0.02 + 0.25 * sqrt ratio
  playTone (fi (round f)) (fi (round (vol * 50)) / 50) (fi (round (tempo * 1.3 / 20)) * 20 / 1000) Triangle
  when (comps > wLastComps w1) $ playTone (880 + fi comps * 60) 0.25 0.25 Sine
  pure w1 { wHistory = take 120 (area : wHistory w1), wLastComps = comps }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ cellsPic env c ox oy panelC cellColor
  , txt env px (ty + 20) 34 (lum 100) (if wPhase w == 0 then "Erode " ++ show (wK w) else "Dilate " ++ show (wDepth w - wK w))
  , txt env px (ty + 54) 15 (lum 100) ("Area " ++ show area ++ " / " ++ show (wMaxArea w) ++ "   Blobs " ++ show (wLastComps w))
  , note 90 "Pitch = area (bigger is higher)"
  , note 114 "Volume = square root of area"
  , note 138 "Blobs increase = blip (high note)"
  -- 面積の履歴
  , box env (hsb 170 30 14) px gy pw gh
  , Color (hsb 45 90 100) (thickPath 2 historyPts)
  , txt env (px + 6) (gy + gh + 20) 14 (lum 60) "Area over time ->"
  , txt env px (gy + gh + 50) 14 (lum 60) "Erode then dilate = opening"
  , txt env px (gy + gh + 74) 14 (lum 60) "Thin parts do not come back"
  , txt env px (gy + gh + 92) 14 (lum 60) "(they stay dark green)"
  , txt env 20 40 15 (lum 100)
      ((if wPlaying w then "Playing (Space: stop)" else "Space: play (or click outside the grid)")
       ++ "   Tempo (mouse X): " ++ show (round tempo :: Int) ++ " ms   Left/Right: depth " ++ show (wDepth w) ++ "   1-3: samples   Drag: draw   C: clear")
  , txt env 20 68 14 (lum 60)
      "Red = eroding, green = dilating, dark green = original. Thick shapes play low and slow; thin shapes vanish with many blips."
  ]
  where
    c = cellOf env
    img = wImg w
    area = areaOf img
    tempo = tempoOf env
    liveRGB = if wPhase w == 0 then redRGB else greenRGB
    cellColor x y
      | img U.! i = liveRGB
      | wOrig w U.! i = shadeRGB
      | otherwise = offRGB
      where i = idx x y
    px = ox + fi cols * c + 40
    ty = oy + 10
    pw = envW env - px - 30
    gy = ty + 170
    gh = 160
    note dy s = txt env px (ty + dy) 14 (lum 60) s
    historyPts =
      [ pt env (px + (fi i / 119) * pw) (gy + gh - (fi a / fi (wMaxArea w)) * (gh - 8) - 4)
      | (i, a) <- zip [0 :: Int ..] (reverse (wHistory w)) ]

-- 塊（8 近傍）の数
countComponents :: Grid -> Int
countComponents a = runST $ do
  seen <- MU.replicate (U.length a) False
  let flood [] = pure ()
      flood (i : stack) = do
        let x = i `rem` cols
            y = i `quot` cols
        stack' <- foldM (\acc j -> do
                    s <- MU.read seen j
                    if a U.! j && not s then MU.write seen j True >> pure (j : acc) else pure acc)
                  stack
                  [ idx nx ny | dy <- [-1, 0, 1], dx <- [-1, 0, 1]
                  , let nx = x + dx, let ny = y + dy, inGrid nx ny ]
        flood stack'
      start n s = do
        v <- MU.read seen s
        if a U.! s && not v then MU.write seen s True >> flood [s] >> pure (n + 1) else pure n
  foldM start (0 :: Int) [0 .. U.length a - 1]

-- 描く。マス目の外なら False を返す
paint :: Env -> World -> Maybe World
paint env w
  | not (inGrid gx gy) = Nothing
  | otherwise =
      let on = [(i, True) | i <- brush (gx, gy) 2 5]
          orig = wOrig w U.// on
      in Just w { wOrig = orig, wImg = wImg w U.// on, wMaxArea = max 1 (areaOf orig) }
  where (gx, gy) = cellAt env (cellOf env) ox oy

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    maybe w { wPlaying = not (wPlaying w) } id (paint env w)  -- マス目の外をクリック = 再生／停止
  EventMotion _ | envDown env -> maybe w id (paint env w)
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wDepth = max 1 (wDepth w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wDepth = min 20 (wDepth w + 1) }
  EventKey (Char ch) Down _ _ -> case toLower ch of
    '1' -> preset 1 w
    '2' -> preset 2 w
    '3' -> preset 3 w
    'c' -> restart w { wOrig = U.map (const False) (wOrig w) }
    _   -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 「音」「太い丸」「細い線」で聞き比べる。ポンがたくさん鳴るのはどれ？
-- 2. 深さを 20 にして、面積のグラフが 0 まで落ちる形と落ちない形を作る
-- 3. 塊の数 wLastComps を和音の数にする（塊ごとに 1 つの音を鳴らす。playChord が使える）

-- ==================================================
-- ここから下は「道具」（座標、描画、構造要素、膨張・収縮、文字の画像）
-- （中身は読まなくても OK）
-- ==================================================
cellOf :: Env -> Float
cellOf env = min ((envW env - 400) / fi cols) ((envH env - 140) / fi rows)

panelC :: Color
panelC = hsb 170 30 16

offRGB, redRGB, greenRGB, shadeRGB :: (Float, Float, Float)
offRGB = rgbOf panelC
redRGB = rgbOf (hsb 350 60 95)
greenRGB = rgbOf (hsb 150 60 95)
shadeRGB = rgbOf (hsb 150 40 30)

-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

-- 塗りつぶした長方形（左上が (x, y)。p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0

fi :: Int -> Float
fi = fromIntegral

-- 色を pixelImageF 用の (赤, 緑, 青) にする
rgbOf :: Color -> (Float, Float, Float)
rgbOf c = let (r, g, b, _) = rgbaOfColor c in (r, g, b)

-- マス目全体を 1 枚の画像にする。f x y がマスの色。1 マスずつ長方形を描くより速い
imagePic :: Env -> Float -> Float -> Float -> (Int -> Int -> (Float, Float, Float)) -> Picture
imagePic env c x0 y0 f =
  at (pt env (x0 + fi cols * c / 2) (y0 + fi rows * c / 2)) (pixelImageF cols rows c f)

-- 1 ピクセルのすき間のあるマス目（p5.js の rect(x, y, cell - 1, cell - 1)）。
-- 画像の上に、すき間の色の線を重ねて描く
cellsPic :: Env -> Float -> Float -> Float -> Color -> (Int -> Int -> (Float, Float, Float)) -> Picture
cellsPic env c x0 y0 gapC f = Pictures
  [ imagePic env c x0 y0 f
  , Color gapC (Pictures
      ( [Line [pt env (x0 + fi i * c - 0.5) y0, pt env (x0 + fi i * c - 0.5) (y0 + fi rows * c)] | i <- [1 .. cols - 1]]
     ++ [Line [pt env x0 (y0 + fi j * c - 0.5), pt env (x0 + fi cols * c) (y0 + fi j * c - 0.5)] | j <- [1 .. rows - 1]]))
  ]

-- 二値画像：長さ cols × rows の Bool の並び（True = 前景）
type Grid = U.Vector Bool

idx :: Int -> Int -> Int
idx x y = x + y * cols

gridFrom :: (Int -> Int -> Bool) -> Grid
gridFrom f = U.generate (cols * rows) (\i -> f (i `rem` cols) (i `quot` cols))

-- 前景のマスの数
areaOf :: Grid -> Int
areaOf = U.length . U.filter id

-- (x, y) と (cx, cy) の距離（p5.js の dist）
distI :: Int -> Int -> Float -> Float -> Float
distI x y cx cy = dist2 (fi x, fi y) (cx, cy)

-- マウスのいるマスの番号
cellAt :: Env -> Float -> Float -> Float -> (Int, Int)
cellAt env c x0 y0 =
  let (mx, my) = mouseXY env in (floor ((mx - x0) / c), floor ((my - y0) / c))

inGrid :: Int -> Int -> Bool
inGrid x y = x >= 0 && y >= 0 && x < cols && y < rows

-- (gx, gy) のまわり半径 r の、dx² + dy² <= lim を満たすマスの番号
brush :: (Int, Int) -> Int -> Int -> [Int]
brush (gx, gy) r lim =
  [ idx x y | dy <- [-r .. r], dx <- [-r .. r], dx * dx + dy * dy <= lim
  , let x = gx + dx, let y = gy + dy, inGrid x y ]

-- 構造要素：原点からのずれ (dx, dy) のリスト
data Kind = Box | Cross | Disk deriving (Eq)

makeSE :: Kind -> Int -> [(Int, Int)]
makeSE kind r =
  [ (dx, dy) | dy <- [-r .. r], dx <- [-r .. r], inSE dx dy ]
  where
    inSE dx dy = case kind of
      Box -> True
      Cross -> dx == 0 || dy == 0
      Disk -> fi (dx * dx + dy * dy) <= fi (r * r) + fi r * 0.5

-- 膨張と収縮。画像の外側は端のマスと同じ値とみなす
--   膨張：SE のどれか 1 マスでも前景に当たれば ON
--   収縮：SE の全部のマスが前景に入れば ON
dilateImg, erodeImg :: [(Int, Int)] -> Grid -> Grid
dilateImg se a = U.generate (U.length a) (\i -> any (onAt a i) se)
erodeImg se a = U.generate (U.length a) (\i -> all (onAt a i) se)

onAt :: Grid -> Int -> (Int, Int) -> Bool
onAt a i (dx, dy) =
  let nx = max 0 (min (cols - 1) (i `rem` cols + dx))
      ny = max 0 (min (rows - 1) (i `quot` cols + dy))
  in a U.! (nx + ny * cols)

-- 文字をマス目の画像にする。gloss には文字を画像に焼く機能がないので、
-- 文字の形を太い折れ線で作ってある（px は文字の大きさ）
textToGrid :: String -> Float -> Grid
textToGrid str px = gridFrom (\x y -> any (near (fi x, fi y)) segs)
  where
    k = px / 56
    (halfW, strokes) = glyphs str
    place (ax, ay) = (fi cols / 2 + k * ax, fi rows / 2 + k * ay)
    segs = [ (place a, place b) | p <- strokes, (a, b) <- zip p (drop 1 p) ]
    near q (a, b) = segDist q a b < halfW * k

-- 点 q と線分 ab の距離
segDist :: Point -> Point -> Point -> Float
segDist (qx, qy) (ax, ay) (bx, by) =
  let dx = bx - ax
      dy = by - ay
      l2 = max 1e-6 (dx * dx + dy * dy)
      t = clampF 0 1 (((qx - ax) * dx + (qy - ay) * dy) / l2)
  in dist2 (qx, qy) (ax + t * dx, ay + t * dy)

-- 円弧を折れ線にする（中心、半径 rx, ry、開始角、終了角。角度は度で、y は下向き）
arcPts :: Point -> Float -> Float -> Float -> Float -> [Point]
arcPts (cx, cy) rx ry a0 a1 =
  [ (cx + rx * cos t, cy + ry * sin t)
  | i <- [0 .. 16 :: Int], let t = (a0 + (a1 - a0) * fi i / 16) * pi / 180 ]

shiftX :: Float -> [[Point]] -> [[Point]]
shiftX d = map (map (\(x, y) -> (x + d, y)))

-- 文字ごとの線の太さの半分と、折れ線（中心が原点、大きさ 56 のときの座標）
glyphs :: String -> (Float, [[Point]])
glyphs s = case s of
  "p5" -> (4, shiftX (-17) glyphP ++ shiftX 17 glyph5)
  "ab" -> (4, shiftX (-14) glyphA ++ shiftX 14 glyphB)
  _    -> (2.6, glyphOn)
  where
    glyphP = [[(-10, -11), (-10, 24)], arcPts (1, -0.5) 10.5 10.5 0 360]
    glyph5 = [ [(11, -21), (-6, -21), (-8, -7)]
             , arcPts (-1, 0) 12 10.5 (-125) 150 ]
    glyphA = [ arcPts (0, -4) 9 9 (-160) 0, [(9, -4), (9, 11)]
             , arcPts (0, 4.5) 9 6 0 360 ]
    glyphB = [[(-10, -22), (-10, 11)], arcPts (1, -0.5) 10.5 10.5 0 360]
    -- 「音」= 立 + 日
    glyphOn = [ [(0, -24), (0, -19)], [(-15, -18), (15, -18)]
              , [(-8, -14), (-5, -6)], [(8, -14), (5, -6)], [(-23, -5), (23, -5)]
              , [(-13, 1), (13, 1), (13, 24), (-13, 24), (-13, 1)], [(-13, 12.5), (13, 12.5)] ]
