-- ============================================
-- 11 画像の離散勾配 ─ 稜線と谷線を取り出す
-- 画像の明るさを高さ関数とみなすと、離散モース理論がそのまま使える
--   鞍点から極大へ登る V-パス = 稜線（明るい筋）、鞍点から極小へ降りる V-パス = 谷線（暗い筋）
--   指紋風の縞では縞の中心が稜線に、ノイズ画像では明るい塊の背骨が稜線になる。しきい値 ε 以下のペアはキャンセル（10）してから線を引く
-- 1/2 = 画像（縞・ノイズ＋図形）/ ↑↓ = しきい値 ε / R = 稜線 / V = 谷線 / I = 明暗を反転 / N = 別の画像
-- ============================================
module Main (main) where

import Art
import Art
import Control.Monad (forM_)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', mapAccumL, nub, sort, sortBy, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

nx, ny :: Int
nx = 96             -- ★ 画像の大きさ（頂点の数）
ny = 64

ox, oy, cs :: Float
ox = 30
oy = 100
cs = min ((scrW - 60) / fi nx) ((scrH - 150) / fi ny)

cx :: Cx
cx = makeGridComplex nx ny Plain (\_ _ -> True)

pos :: U.Vector Point
pos = gridPositions cx ox oy cs

data World = World
  { wImgNo  :: !Int
  , wInv    :: !Bool
  , wEps    :: !Float
  , wRidge  :: !Bool
  , wValley :: !Bool
  , wH      :: !(U.Vector Float)
  , wBase   :: !(U.Vector Int)
  , wPair   :: !(U.Vector Int)
  , wSeq    :: [Cancel]
  , wJob    :: Maybe SeqJob   -- 計算中のキャンセルの列（1 フレームに少しずつ進める）
  , wWait   :: !Int           -- あと何フレーム待ってから次の段階へ進むか
  , wImg    :: Picture     -- 画像
  , wLines  :: Picture     -- 線（p5.js の pg）
  , wCounts :: !(Int, Int, Int)
  }

empty :: World
empty = World 1 False 0.08 True True U.empty neg neg [] Nothing 0 Blank Blank (0, 0, 0)
  where neg = U.replicate (nCells cx) (-1)

main :: IO ()
main = runSketch (sketch "37-11 Image Ridges" empty)
  { skBack  = hsb 225 30 8
  , skStart = \_ -> forceCx cx `seq` pos `seq` makeImage empty
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ 合成画像：1 = 指紋風の縞（sin を曲げる）、2 = ノイズ + 明るい図形
makeImage :: World -> IO World
makeImage w = do
  s <- newSeed
  ph <- randomRIO (0, 100)
  let n = noiseP s
      value i j
        | wImgNo w == 1 =
            let warp = 6 * n (x * 0.03 + ph) (y * 0.03)
                cxr = x - fi nx / 2
                cyr = y - fi ny / 2
            in 0.5 + 0.5 * sin (0.55 * sqrt (cxr * cxr + 1.4 * cyr * cyr) + warp + 2 * n (x * 0.05) (y * 0.05 + ph)) * (0.7 + 0.3 * n (x * 0.2) (y * 0.2))
        | otherwise =
            0.35 * n (x * 0.12 + ph) (y * 0.12)
            + sum [ 0.7 * exp (-(d * d) / (r * r))
                  | (sx, sy, r) <- [(fi nx * 0.3, fi ny * 0.4, 14), (fi nx * 0.7, fi ny * 0.35, 10), (fi nx * 0.55, fi ny * 0.72, 12)]
                  , let d = dist2 (x, y) (sx, sy) ]
            + 0.25 * exp (-((abs (y - fi ny * 0.6 - 10 * sin (x * 0.1)) / 2.5) ^ (2 :: Int))) * (if i > 8 && i < nx - 8 then 1 else 0)
        where x = fi i
              y = fi j
      shade i j = let v = value i j in if wInv w then 1 - v else v
      h = heightFrom cx (\i j -> shade i j + 0.00001 * (fi i * 3 + fi j))
      base = gradientFromHeight cx h
      grays = U.generate ((nx + 1) * (ny + 1)) (\k -> let (j, i) = k `divMod` (nx + 1) in clampF 0 1 (shade i j) * 235 + 10)
      image = stretch ox oy (fi nx * cs) (fi ny * cs) (nx + 1) (ny + 1) $ \i j ->
        let g = fromIntegral (floor (grays U.! (i + j * (nx + 1))) :: Int) in (g, g, g)
  pure w { wH = h, wBase = base, wPair = base, wSeq = [], wJob = Just (seqInit cx h base), wWait = 1, wImg = image, wLines = Blank }

-- ★ キャンセルの列を作る（10 と同じ）。高さの差 0.6 までのペアを、止まらないように 1 フレーム 8 回ずつ
--   パスの下調べ（seqInit）と最後の描きなおしも、それぞれ別のフレームで行う
stepWorld :: World -> World
stepWorld w = case wJob w of
  Nothing -> w
  Just j
    | wWait w > 0 -> jEnd j `seq` w { wWait = wWait w - 1 }
    | jEnd j -> render w { wSeq = jobResult j, wJob = Nothing }
    | otherwise -> w { wJob = Just (seqAdvance cx (wH w) 0.6 8 j) }

-- 計算中は描きなおさない
refresh :: World -> World
refresh w = maybe (render w) (const w) (wJob w)

-- ★ ε 以下のペアをキャンセルしてから、残った鞍点ごとに降りるパス（谷線）と登るパス（稜線）を描く
render :: World -> World
render w = w { wPair = pair, wLines = pic, wCounts = (length saddles, length ups, length downs) }
  where
    pair = applyEps (wEps w) (wBase w) (wSeq w)
    saddles = [c | c <- [0 .. nCells cx - 1], pair U.! c < 0, cDim (cell cx c) == 1]
    downs = if wValley w then concatMap (take 200 . descendPaths cx pair 4000) saddles else []
    ups = if wRidge w then concatMap (take 200 . ascendPaths cx pair 4000) saddles else []
    pic = Pictures $
      [ seg 2 (hsba 272 80 100 0.9) a b | (a, b) <- uniqueSegs pos downs ]
      ++ [ seg 2 (hsba 12 90 100 0.9) a b | (a, b) <- uniqueSegs pos ups ]
      ++ [ dotC (hsb 85 90 100) (pos U.! c) 6 | c <- saddles ]

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $ Pictures
  [ wImg w
  , wLines w
  , txt ox 40 18 white ((if wImgNo w == 1 then "Stripes" else "Noise+shapes") ++ " m = " ++ showM (countCritical cx (wBase w))
      ++ " -> " ++ showM (countCritical cx (wPair w)) ++ "  eps = " ++ fmt 2 (wEps w)
      ++ maybe ("  saddles " ++ show ns ++ " ridges " ++ show nr ++ " valleys " ++ show nv) (const "  computing...") (wJob w))
  , txt ox 66 15 (gray 0.65) ("1/2: image  Up/Dn: eps  R: ridges " ++ onOff (wRidge w) ++ "  V: valleys " ++ onOff (wValley w)
      ++ "  I: invert  N: new   red = ridge  purple = valley  lime = saddle")
  ]
  where (ns, nr, nv) = wCounts w

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (refresh w { wEps = min 1 (wEps w + 0.02) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (refresh w { wEps = max 0 (wEps w - 0.02) })
  EventKey (Char c) Down _ _
    | c `elem` "12" -> makeImage w { wImgNo = fromEnum c - fromEnum '0' }
    | c `elem` "rR" -> pure (refresh w { wRidge = not (wRidge w) })
    | c `elem` "vV" -> pure (refresh w { wValley = not (wValley w) })
    | c `elem` "iI" -> makeImage w { wInv = not (wInv w) }
    | c `elem` "nN" -> makeImage w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 縞（1）で ε を上げていくと、縞の中心を通る長い稜線だけが残る。縞が途切れる所（指紋の「端点」）はどう見える？
-- 2. 谷線だけ表示して反転（I）すると、稜線と入れかわる。離散モース理論では極大と極小は対称
-- 3. ノイズ画像（2）の曲がった明るい線は 1 本の稜線としてつながる？ ε をいくつにすればつながる？
-- 4. makeImage の図形を自分で足して（円・線・文字の形）、稜線がどう出るか試そう

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

-- 臨界胞体 c（次元 k）から矢印をたどって下る V-パスをすべて集める（次元 k-1 の臨界胞体で終わる）
descendPaths, ascendPaths :: Cx -> U.Vector Int -> Int -> Int -> [[Int]]
descendPaths = walkPaths cFaces
-- 臨界胞体 c から矢印を逆にたどって上る V-パス（次元 k+1 の臨界胞体で終わる）
ascendPaths = walkPaths cCofaces

-- 閉路に出会ったパスはそこで打ち切る。maxLen = パスの長さの上限（p5.js の省略時は 4000）
walkPaths :: (Cell -> [Int]) -> Cx -> U.Vector Int -> Int -> Int -> [[Int]]
walkPaths next cx pair maxLen c0 = walk c0 [c0] (IS.singleton c0) 1
  where
    walk a rev onPath len = concatMap step (next (cell cx a))
      where
        step b
          | b == pair U.! a = []
          | p < 0 = [reverse (b : rev)]
          | cDim (cell cx p) /= cDim (cell cx a) = []
          | IS.member p onPath = [reverse (p : b : rev)]
          | len < maxLen = walk p (p : b : rev) (IS.insert p onPath) (len + 2)
          | otherwise = []
          where p = pair U.! b

-- パスの最後の胞体
pathEnd :: [Int] -> Int
pathEnd = foldl' (\_ x -> x) (-1)

-- キャンセル：パス c0, c1, c2, … の矢印を (c0,c1), (c2,c3), … に組み替える（向きが逆になる）
cancelUpdates :: [Int] -> [(Int, Int)]
cancelUpdates (a : b : rest) = (a, b) : (b, a) : cancelUpdates rest
cancelUpdates _ = []

cancelPath :: U.Vector Int -> [Int] -> U.Vector Int
cancelPath pair path = pair U.// cancelUpdates path

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

-- キャンセルの列。パーシステンス最小のペアを消す、をできなくなるまでくり返す貪欲法
--   p5.js は毎回すべての臨界胞体からパスをたどり直すが、ここでは結果を覚えておき、
--   組み替えた胞体にふれるものだけ計算しなおす（結果は同じで、ずっと速い）
data Cancel = Cancel { sPath :: [Int], sPers :: !Float, sDim :: !Int, sBirth :: !Float, sDeath :: !Float }

data Info = Info { iBest :: !(Maybe Cancel), iTouched :: !IS.IntSet }

cellHeight :: Cx -> U.Vector Float -> Int -> Float
cellHeight cx h c = foldl' (\m v -> max m (h U.! v)) (-1 / 0) (cVerts (cell cx c))

better :: Maybe Cancel -> Cancel -> Maybe Cancel
better (Just b) s | sPers s >= sPers b = Just b
better _ s = Just s

cellInfo :: Cx -> U.Vector Float -> U.Vector Int -> Int -> Info
cellInfo cx h pair c = Info best touched
  where
    paths = descendPaths cx pair 4000 c
    ends = IM.fromListWith (++) [(pathEnd p, [p]) | p <- paths]
    hc = cellHeight cx h c
    d = cDim (cell cx c)
    best = foldl' better Nothing
      [ Cancel p (hc - he) (d - 1) he hc | (e, [p]) <- IM.toAscList ends, let he = cellHeight cx h e ]
    onPaths = c : concat paths
    touched = IS.fromList (onPaths ++ concatMap (cFaces . cell cx) onPaths)

-- 途中の状態。少しずつ進められるので、計算を何フレームかに分けることもできる
data SeqJob = SeqJob { jPair :: !(U.Vector Int), jCache :: !(IM.IntMap Info), jDone :: [Cancel], jEnd :: !Bool }

seqInit :: Cx -> U.Vector Float -> U.Vector Int -> SeqJob
seqInit cx h base = SeqJob base cache0 [] False
  where cache0 = IM.fromList [ (c, cellInfo cx h base c) | c <- [0 .. nCells cx - 1], base U.! c < 0, cDim (cell cx c) > 0 ]

-- k 回ぶん進める。limit より大きいパーシステンスのペアが最小になったら止める
seqAdvance :: Cx -> U.Vector Float -> Float -> Int -> SeqJob -> SeqJob
seqAdvance cx h limit = go
  where
    go :: Int -> SeqJob -> SeqJob
    go k j
      | k <= 0 || jEnd j = j
      | otherwise = case IM.foldl' (\m i -> maybe m (better m) (iBest i)) Nothing (jCache j) of
          Just s | sPers s <= limit ->
            let pair' = cancelPath (jPair j) (sPath s)
                changed = IS.fromList (sPath s)
                cache1 = IS.foldl' (flip IM.delete) (jCache j) changed
                cache2 = IM.mapWithKey (\c i -> if IS.disjoint (iTouched i) changed then i else cellInfo cx h pair' c) cache1
            in go (k - 1) (SeqJob pair' cache2 (s : jDone j) False)
          _ -> j { jEnd = True }

jobResult :: SeqJob -> [Cancel]
jobResult = reverse . jDone

buildSequence :: Cx -> U.Vector Float -> Float -> U.Vector Int -> [Cancel]
buildSequence cx h limit base = jobResult (seqAdvance cx h limit maxBound (seqInit cx h base))

-- ε 以下のキャンセルだけを最初からやり直す
applyEps :: Float -> U.Vector Int -> [Cancel] -> U.Vector Int
applyEps eps base sq = base U.// concatMap (cancelUpdates . sPath) (takeWhile ((<= eps) . sPers) sq)

-- パスを重なりのない線分に分ける（同じ線分を何度も描かない）
uniqueSegs :: U.Vector Point -> [[Int]] -> [(Point, Point)]
uniqueSegs pos paths = [ (pos U.! a, pos U.! b) | (a, b) <- S.toList (S.fromList (concatMap links paths)) ]
  where links p = [ (min a b, max a b) | (a, b) <- zip p (drop 1 p) ]

-- ピクセル画像を左上 (x, y)、幅 ww、高さ hh に引きのばして置く
stretch :: Float -> Float -> Float -> Float -> Int -> Int -> (Int -> Int -> (Word8, Word8, Word8)) -> Picture
stretch x y ww hh w h f =
  Translate (x + ww / 2) (-(y + hh / 2)) (Scale (ww / fromIntegral w) (hh / fromIntegral h) (pixelImage w h 1 f))

bytes :: Color -> (Word8, Word8, Word8)
bytes c = let (r, g, b, _) = rgbaOfColor c in (q r, q g, q b)
  where q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))
