-- ============================================
-- 14 モース・ブラシ ─ 地形を描くと流域と臨界点が浮かび上がる
-- マウスで地形を盛り上げ（Shift で掘り）、そのたびに離散勾配場を計算しなおす
--   極小ごとの流域（09）を色で塗り分け、分水嶺を暗い線で、臨界点を記号で描くアート。自由制作のベース
-- ドラッグ = 盛り上げる / Shift+ドラッグ = 掘る / 1〜3 = 配色 / ↑↓ = ブラシの大きさ / H = 記号の表示 / C = 平らに / S = 画像を保存
-- ============================================
module Main (main) where

import Art
import Art
import Control.Monad (forM_)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', mapAccumL, nub, sort, sortBy, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

nx, ny :: Int
nx = 80             -- ★ 地形の解像度
ny = 50

cs :: Float
cs = max (scrW / fi nx) (scrH / fi ny)

cx :: Cx
cx = makeGridComplex nx ny Plain (\_ _ -> True)

pos :: U.Vector Point
pos = gridPositions cx 0 0 cs

data World = World
  { wH       :: !(U.Vector Float)
  , wPair    :: !(U.Vector Int)
  , wPic     :: Picture
  , wBrush   :: !Int
  , wPalette :: !Int
  , wMarks   :: !Bool
  , wDirty   :: !Bool
  , wLast    :: !Float
  , wShift   :: !Bool
  }

empty :: World
empty = World U.empty (U.replicate (nCells cx) (-1)) Blank 6 1 True True (-1) False

main :: IO ()
main = runSketch (sketch "37-14 Morse Brush" empty)
  { skBack  = hsb 225 30 8
  , skStart = \_ -> forceCx cx `seq` pos `seq` flatten empty
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

flatten :: World -> IO World
flatten w = do
  s <- newSeed
  let n = pnoise 2 0.3 s
      h = heightFrom cx (\i j -> 0.1 * n (fi i * 0.05) (fi j * 0.05) + 0.00001 * (fi i * 3 + fi j))
  pure w { wH = h, wDirty = True }

-- ★ 配色：流域の番号 → 色相、高さ → 明るさ
faceColor :: Int -> Int -> Float -> Bool -> (Float, Float, Float)
faceColor paletteNo b v edge = case paletteNo of
  1 -> (hue, if edge then 10 else 50, 25 + v * 60)                       -- 虹色の流域
  2 -> (wrapF 60 hue + 180, if edge then 10 else 70, 15 + v * 75)        -- 青〜紫の夜
  _ -> (20 + wrapF 40 hue, if edge then 5 else 60, 20 + v * 70)          -- 砂と土
  where hue = wrapF 360 (fi (b `mod` 4000) * 137.5)

computeField :: Float -> World -> World
computeField t w = w { wPair = pair, wPic = Pictures (image : marks), wDirty = False, wLast = t }
  where
    h = wH w
    pair = gradientFromHeight cx h
    bs = basins cx pair
    lo = U.minimum h
    hi = U.maximum h + 1e-6
    image = stretch 0 0 (fi nx * cs) (fi ny * cs) nx ny $ \i j ->
      let vs = cVerts (cell cx (fid cx i j))
          v = (sum (map (h U.!) vs) / 4 - lo) / (hi - lo)
          b = case vs of
            (q : _) -> bs U.! q
            [] -> 0
          edge = any (\q -> bs U.! q /= b) vs
          (hh, ss, bb) = faceColor (wPalette w) b v edge
      in bytes (hsb hh ss (if edge then bb * 0.45 else bb))
    marks | wMarks w = concat [ markOf c | c <- [0 .. nCells cx - 1], pair U.! c < 0 ]
          | otherwise = []
    markOf c =
      let p@(x, y) = pos U.! c
      in case cDim (cell cx c) of
           0 -> [dotC (hsba 0 0 100 0.85) p 7]
           1 -> let col = hsba 85 80 100 0.9
                in [seg 2 col (x - 5, y - 5) (x + 5, y + 5), seg 2 col (x - 5, y + 5) (x + 5, y - 5)]
           _ -> [ringC (hsba 12 80 100 0.9) p 14 2.5]

-- ★ ブラシ：ガウス関数で盛り上げる／掘る
paint :: Point -> World -> World
paint (mx, my) w = w { wH = wH w U.// ups, wDirty = True }
  where
    ci = mx / cs
    cj = my / cs
    br = fi (wBrush w)
    sgn = if wShift w then -1 else 1
    ups = [ (v, clampF (-3) 3 (wH w U.! v + sgn * 0.04 * exp (-d2)))
          | i <- [max 0 (floor (ci - br * 2)) .. min nx (ceiling (ci + br * 2))]
          , j <- [max 0 (floor (cj - br * 2)) .. min ny (ceiling (cj + br * 2))]
          , let d2 = ((fi i - ci) ^ (2 :: Int) + (fi j - cj) ^ (2 :: Int)) / (br * br)
          , let v = vid cx i j ]

stepWorld :: Env -> World -> World
stepWorld env w
  | wDirty w && envTime env - wLast w > 0.05 = computeField (envTime env) w
  | otherwise = w

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $ Pictures $
  [ wPic w, ringC (hsba 0 0 100 0.6) (mouseP env) (fi (wBrush w) * cs * 2) 1 ]
  ++ (if wMarks w then
        [ box (makeColor 0 0 0 0.45) 0 0 scrW 64
        , txt 20 28 18 white ("Min " ++ show m0 ++ " (basins)  saddle " ++ show m1 ++ "  max " ++ show m2 ++ "   m0 - m1 + m2 = " ++ show (m0 - m1 + m2))
        , txt 20 52 14 (gray 0.8) ("Drag: raise  Shift+drag: dig  1-3: palette  Up/Dn: brush " ++ show (wBrush w) ++ "  H: hide  C: flatten  S: save   dot = min  x = saddle  ring = max  dark = watershed")
        ] else [])
  where (m0, m1, m2) = countCritical cx (wPair w)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventMotion _ | envDown env -> pure (paint m w)
  EventKey (MouseButton LeftButton) Down md _ -> pure (paint m w { wShift = shift md == Down })
  EventKey (Char c) Down _ _
    | c `elem` "123" -> pure w { wPalette = fromEnum c - fromEnum '0', wDirty = True }
    | c `elem` "hH" -> pure w { wMarks = not (wMarks w), wDirty = True }
    | c `elem` "cC" -> flatten w
    | c `elem` "sS" -> saveSvg "morse_brush" env (drawWorld env w) >> pure w
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wBrush = min 16 (wBrush w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wBrush = max 2 (wBrush w - 1) }
  _ -> pure w
  where m = mouseP env

-- ---------- やってみよう ----------
-- 1. 山を 1 つ描くと流域はどう分かれる？ 山脈（線）を描くと？ 分水嶺が線の上に来るか確かめよう
-- 2. 穴（Shift）を 3 つ掘ると流域が 3 つになる。間の鞍点（×）を掘って 2 つにつなげよう
-- 3. faceColor を自分の配色にしよう（流域の番号 b で色相、高さ v で明るさ、edge で境界）
-- 4. 記号を消して（H）、流域の色だけの抽象画にする。wBrush を 2 にして細かい「模様」を描こう

-- ---------- 離散モース理論の道具 ----------

-- p5.js と同じ座標（左上が原点、y は下向き）で描くための小道具。screen で全体を画面に合わせる
scrW, scrH :: Float
scrW = 1280
scrH = 800

screen :: Env -> Picture -> Picture
screen env = Translate (-envW env / 2) (envH env / 2)

toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

mouseP :: Env -> Point
mouseP env = toP5 env (envMouse env)

fl :: Point -> Point
fl (x, y) = (x, -y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label x (-y)

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (thickLine w (fl a) (fl b))

-- 太さ w の折れ線
polyL :: Float -> Color -> [Point] -> Picture
polyL w c ps = Color c (thickPath w (map fl ps))

-- 左上 (x, y)、幅 w、高さ h の長方形
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (rect (x + w / 2) (-(y + h / 2)) w h)

-- 直径 d の円（p5.js の circle）
dotC :: Color -> Point -> Float -> Picture
dotC c p d = Color c (at (fl p) (disc (d / 2)))

-- 直径 d、線の太さ w の輪
ringC :: Color -> Point -> Float -> Float -> Picture
ringC c p d w = Color c (at (fl p) (ring (d / 2) w))

fmt :: Int -> Float -> String
fmt n x = showFFloat (Just n) x ""

fi :: Int -> Float
fi = fromIntegral

-- p5.js の noise に近いノイズ。oct = 重ねる回数、fall = 減衰（noiseDetail）、s = ずらす量（noiseSeed の代わり）
pnoise :: Int -> Float -> Float -> Float -> Float -> Float
pnoise oct fall s x y = go oct 1 fall 0
  where
    go :: Int -> Float -> Float -> Float -> Float
    go 0 _ _ acc = acc
    go k f a acc = go (k - 1) (f * 2) (a * fall) (acc + a * clampF 0 1 (0.5 + 2.2 * (noise3 (x * f + s) (y * f + s * 0.61) (0.5 + s * 0.37) - 0.5)))   -- 2.2 倍：p5.js の noise と同じくらいの幅にする

-- noiseDetail を変えていないときの noise
noiseP :: Float -> Float -> Float -> Float
noiseP = pnoise 4 0.5

newSeed :: IO Float
newSeed = randomRIO (0, 200)

-- 格子（nx × ny マス）の胞体複体。Ident は辺の同一視、mask i j が False のマス（面）は入れない
--   胞体は 頂点 → 辺 → 面 の順に番号がつく。cxV・cxEh（横の辺）・cxEv（縦の辺）・cxF = 格子の位置 → 胞体番号（なければ -1）
data Ident = Plain | TorusGlue | SphereGlue | KleinGlue deriving Eq

data Cell = Cell
  { cDim     :: !Int
  , cVerts   :: ![Int]
  , cFaces   :: ![Int]
  , cCofaces :: ![Int]
  , cNcof    :: !Int     -- 面に接する回数（折り返しは 2 回）
  }

data Cx = Cx
  { cxNx, cxNy :: !Int
  , cxCells :: !(V.Vector Cell)
  , cxV, cxEh, cxEv, cxF :: !(U.Vector Int)
  }

type CellKey = (Char, Int, Int)
data Build = Build !(M.Map CellKey Int) [(Int, [Int])]

cell :: Cx -> Int -> Cell
cell cx c = cxCells cx V.! c

nCells :: Cx -> Int
nCells = V.length . cxCells

gridAt :: Cx -> U.Vector Int -> Int -> Int -> Int
gridAt cx t i j = t U.! (i * (cxNy cx + 1) + j)

vid, ehAt, evAt, fid :: Cx -> Int -> Int -> Int
vid cx = gridAt cx (cxV cx)
ehAt cx = gridAt cx (cxEh cx)
evAt cx = gridAt cx (cxEv cx)
fid cx = gridAt cx (cxF cx)

getCell :: CellKey -> Int -> [Int] -> Build -> (Build, Int)
getCell k d vs b@(Build m cs) = case M.lookup k m of
  Just c  -> (b, c)
  Nothing -> let c = M.size m in (Build (M.insert k c m) ((d, vs) : cs), c)

-- 起動のときに複体を作りきっておく（最初のフレームで止まらないように）
forceCx :: Cx -> Int
forceCx = V.foldl' (\a c -> a + cDim c + length (cVerts c) + length (cFaces c) + length (cCofaces c)) 0 . cxCells

makeGridComplex :: Int -> Int -> Ident -> (Int -> Int -> Bool) -> Cx
makeGridComplex nx ny ident mask = Cx nx ny (V.fromList cells) vT ehT evT fT
  where
    grid = [(i, j) | i <- [0 .. nx], j <- [0 .. ny]]
    keep i j = i >= 0 && j >= 0 && i < nx && j < ny && mask i j
    canon i j = case ident of
      TorusGlue -> (i `mod` nx, j `mod` ny)
      KleinGlue -> let (a, b) = if i == nx then (0, ny - j) else (i, j) in (a, if b == ny then 0 else b)
      SphereGlue | j == 0 -> (0, i)
                 | i == nx -> (j, ny)
      _ -> (i, j)
    -- 辺は「格子のどの位置の辺か」で同一視する（同じ 2 頂点をもつ別の辺もあるので頂点の組では区別しない）
    canonE i j d = case ident of
      TorusGlue -> (d, i `mod` nx, j `mod` ny)
      KleinGlue | d == 'h' && j == ny -> (d, i, 0)
                | d == 'v' && i == nx -> (d, 0, ny - j - 1)
      SphereGlue | d == 'h' && j == 0 -> ('v', 0, i)
                 | d == 'v' && i == nx -> ('h', j, ny)
      _ -> (d, i, j)
    addV b (i, j)
      | keep i j || keep (i - 1) j || keep i (j - 1) || keep (i - 1) (j - 1) =
          let (a, c) = canon i j in getCell ('p', a, c) 0 [] b
      | otherwise = (b, -1)
    (b1, vids) = mapAccumL addV (Build M.empty []) grid
    vT = U.fromList vids
    ix i j = i * (ny + 1) + j
    vAt i j = vT U.! ix i j
    edge k a c b
      | a < 0 || c < 0 || a == c = (b, -1)
      | otherwise = getCell k 1 [min a c, max a c] b
    addE b (i, j) =
      let (b', h) | i < nx && (keep i j || keep i (j - 1)) = edge (canonE i j 'h') (vAt i j) (vAt (i + 1) j) b
                  | otherwise = (b, -1)
          (b'', e) | j < ny && (keep i j || keep (i - 1) j) = edge (canonE i j 'v') (vAt i j) (vAt i (j + 1)) b'
                   | otherwise = (b', -1)
      in (b'', (h, e))
    (b2, es) = mapAccumL addE b1 grid
    ehT = U.fromList (map fst es)
    evT = U.fromList (map snd es)
    addF b (i, j)
      | i < nx && j < ny && keep i j =
          getCell ('f', i, j) 2 (sort (nub [vAt i j, vAt (i + 1) j, vAt i (j + 1), vAt (i + 1) (j + 1)])) b
      | otherwise = (b, -1)
    (Build _ rev, fs) = mapAccumL addF b2 grid
    fT = U.fromList fs
    faceEdges i j = filter (>= 0) [ehT U.! ix i j, ehT U.! ix i (j + 1), evT U.! ix i j, evT U.! ix (i + 1) j]
    faceList = [(f, faceEdges i j) | ((i, j), f) <- zip grid fs, f >= 0]
    faceMap = IM.fromList [(f, nub e) | (f, e) <- faceList]
    ncof = IM.fromListWith (+) [(e, 1 :: Int) | (_, l) <- faceList, e <- l]
    raw = zip [0 ..] (reverse rev)
    facesOf (c, (d, vs)) = case d of
      0 -> []
      1 -> vs
      _ -> IM.findWithDefault [] c faceMap
    cof = IM.fromListWith (\new old -> old ++ new) [(f, [c]) | r@(c, _) <- raw, f <- facesOf r]
    cells = [ Cell d (if d == 0 then [c] else vs) (facesOf r) (IM.findWithDefault [] c cof) (IM.findWithDefault 0 c ncof)
            | r@(c, (d, vs)) <- raw ]

-- 頂点の数
nVerts :: Cx -> Int
nVerts cx = V.length (V.filter ((== 0) . cDim) (cxCells cx))

-- 格子の位置 → 高さ から、頂点ごとの高さ h をつくる（穴の頂点はとばす）
heightFrom :: Cx -> (Int -> Int -> Float) -> U.Vector Float
heightFrom cx f = U.replicate (nVerts cx) 0 U.// assocs
  where assocs = [ (v, f i j) | i <- [0 .. cxNx cx], j <- [0 .. cxNy cx], let v = vid cx i j, v >= 0 ]

-- 高さ関数 h[頂点] から離散勾配場（ペアリング）をつくる（lower star 法：Robins ら 2011）
--   返り値 pair[胞体] = 矢印の相手の胞体番号、臨界なら -1
gradientFromHeight :: Cx -> U.Vector Float -> U.Vector Int
gradientFromHeight cx h = U.create $ do
  p <- MU.replicate (nCells cx) (-1)
  forM_ [c | c <- [0 .. nCells cx - 1], cDim (cell cx c) == 0] $ \x ->
    forM_ (lowerStarPairs cx h x) $ \(a, b) -> MU.write p a b >> MU.write p b a
  pure p

-- 胞体の「値」= 頂点の高さを大きい順に並べた列（同じ高さは頂点番号で区別）
gval :: Cx -> U.Vector Float -> Int -> [(Float, Int)]
gval cx h c = sortBy (flip compare) [(h U.! v, v) | v <- cVerts (cell cx c)]

-- いちばん高い頂点
topVert :: Cx -> U.Vector Float -> Int -> Int
topVert cx h c = snd (maximum ((-1 / 0, -1) : [(h U.! v, v) | v <- cVerts (cell cx c)]))

data Star = Star { stDone :: !IS.IntSet, stPq0 :: [Int], stPq1 :: [Int], stPairs :: [(Int, Int)] }

-- x の lower star（x がいちばん高い頂点である胞体）の中だけで、矢印のペアを決める
lowerStarPairs :: Cx -> U.Vector Float -> Int -> [(Int, Int)]
lowerStarPairs cx h x = case sortOn gv (filter ((== 1) . cDim . cell cx) star) of
  [] -> []                                             -- 臨界な頂点（極小）
  (delta : rest) ->                                    -- いちばん低い辺と x をペアに
    stPairs (run (addCof delta (Star (IS.singleton delta) rest [] [(x, delta)])))
  where
    gv = gval cx h
    star = nub (concat [ e : [f | f <- cCofaces (cell cx e), topVert cx h f == x]
                       | e <- cCofaces (cell cx x), topVert cx h e == x ])
    inL = IS.fromList star
    unpaired s a = [b | b <- cFaces (cell cx a), IS.member b inL, not (IS.member b (stDone s))]
    addCof a s0 = foldl' add s0 (cCofaces (cell cx a))
      where add s b
              | IS.member b inL && not (IS.member b (stDone s)) && length (unpaired s b) == 1 && b `notElem` stPq1 s =
                  s { stPq1 = stPq1 s ++ [b] }
              | otherwise = s
    run s = case (stPq0 s, stPq1 s) of
      ([], []) -> s
      _ -> run (popCrit (drain s))
    drain s = case sortOn gv (stPq1 s) of
      [] -> s
      (a : more) -> case unpaired s a of
        [] -> drain s { stPq1 = more, stPq0 = stPq0 s ++ [a] }
        (b : _) -> drain (addCof b (addCof a s
                     { stPq1 = more, stPq0 = filter (/= b) (stPq0 s)
                     , stDone = IS.insert a (IS.insert b (stDone s)), stPairs = (a, b) : stPairs s }))
    popCrit s = case sortOn gv (stPq0 s) of            -- g は臨界
      [] -> s
      (g : more) -> addCof g s { stPq0 = more, stDone = IS.insert g (stDone s) }

-- 臨界胞体の数 (m0, m1, m2)
countCritical :: Cx -> U.Vector Int -> (Int, Int, Int)
countCritical cx pair = (cnt 0, cnt 1, cnt 2)
  where cnt d = length [() | c <- [0 .. nCells cx - 1], pair U.! c < 0, cDim (cell cx c) == d]

showM :: (Int, Int, Int) -> String
showM (a, b, c) = "(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"

-- 各胞体の描く位置（頂点 = 格子点、辺 = 中点、面 = 中心。同一視された胞体は最初の位置）
gridPositions :: Cx -> Float -> Float -> Float -> U.Vector Point
gridPositions cx ox oy cs = U.replicate (nCells cx) (0, 0) U.// reverse puts
  where
    f = fromIntegral :: Int -> Float
    puts = concat
      [ [ (c, p) | (c, p) <- [ (vid cx i j, (ox + f i * cs, oy + f j * cs))
                             , (ehAt cx i j, (ox + (f i + 0.5) * cs, oy + f j * cs))
                             , (evAt cx i j, (ox + f i * cs, oy + (f j + 0.5) * cs))
                             , (fid cx i j, (ox + (f i + 0.5) * cs, oy + (f j + 0.5) * cs)) ], c >= 0 ]
      | i <- [0 .. cxNx cx], j <- [0 .. cxNy cx] ]

-- 流域：各頂点を矢印（頂点 → 辺 → 反対側の頂点）にそって流して、着いた極小の番号を入れる
basins :: Cx -> U.Vector Int -> U.Vector Int
basins cx pair = U.create $ do
  bs <- MU.replicate (nCells cx) (-1)
  let other e c = case cVerts (cell cx e) of
        [a, b] | a == c -> b
               | otherwise -> a
        _ -> c
      flow c trail = do
        b <- MU.read bs c
        if b >= 0 then pure (b, trail)
        else if pair U.! c >= 0 then flow (other (pair U.! c) c) (c : trail)
        else pure (c, c : trail)
  forM_ [c | c <- [0 .. nCells cx - 1], cDim (cell cx c) == 0] $ \v -> do
    (b, trail) <- flow v []
    forM_ trail (\t -> MU.write bs t b)
  pure bs

-- ピクセル画像を左上 (x, y)、幅 ww、高さ hh に引きのばして置く
stretch :: Float -> Float -> Float -> Float -> Int -> Int -> (Int -> Int -> (Word8, Word8, Word8)) -> Picture
stretch x y ww hh w h f =
  Translate (x + ww / 2) (-(y + hh / 2)) (Scale (ww / fromIntegral w) (hh / fromIntegral h) (pixelImage w h 1 f))

bytes :: Color -> (Word8, Word8, Word8)
bytes c = let (r, g, b, _) = rgbaOfColor c in (q r, q g, q b)
  where q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))
