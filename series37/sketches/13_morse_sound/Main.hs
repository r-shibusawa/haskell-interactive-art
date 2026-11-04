-- ============================================
-- 13 勾配の流れを聴く ─ V-パスを降りながら音階を鳴らす
-- 地形（高さ関数）の上でクリックした頂点から、矢印（頂点 → 辺 → 頂点 …）をたどって極小まで降りる
--   1 歩ごとに高さに応じた音（高い所 = 高い音）、鞍点のとなりを通ると「分岐音」、極小に着くと和音
-- クリック = その頂点から流す / A = 自動で次々に流す / N = 新しい地形 / 1〜3 = 音階 / ↑↓ = 速さ
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
import Numeric (showFFloat)

nx, ny :: Int
nx = 24             -- ★ 格子の大きさ
ny = 15

ox, oy, cs :: Float
ox = 30
oy = 110
cs = min ((scrW - 60) / fi nx) ((scrH - 170) / fi ny)

cx :: Cx
cx = makeGridComplex nx ny Plain (\_ _ -> True)

pos :: U.Vector Point
pos = gridPositions cx ox oy cs

-- ★ ペンタトニック・短調・半音階
scales :: [U.Vector Int]
scales = map U.fromList [[0, 2, 4, 7, 9], [0, 2, 3, 5, 7, 8, 10], [0 .. 11]]

data Walker = Walker { wkV :: !Int, wkTrail :: [Int], wkT :: !Int }

data World = World
  { wH      :: !(U.Vector Float)
  , wPair   :: !(U.Vector Int)
  , wWalker :: !(Maybe Walker)
  , wAuto   :: !Bool
  , wStep   :: !Int        -- 何フレームごとに 1 歩進むか
  , wScale  :: !Int
  , wTrails :: [[Int]]     -- 過去の流れ（8 本まで）
  }

empty :: World
empty = World U.empty (U.replicate (nCells cx) (-1)) Nothing False 9 1 []

main :: IO ()
main = runSketch (sketch "37-13 Morse Sound" empty)
  { skBack  = hsb 225 30 8
  , skStart = \_ -> newTerrain empty
  , skStep  = \env _ w -> stepWorld env w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

newTerrain :: World -> IO World
newTerrain w = do
  s <- newSeed
  let n = pnoise 2 0.4 s
      raw = heightFrom cx (\i j -> n (fi i * 0.13) (fi j * 0.13) + 0.0001 * (fi i * 3 + fi j))
      lo = U.minimum raw
      hi = U.maximum raw
      h = U.map (\v -> (v - lo) / max 1e-6 (hi - lo)) raw
  pure w { wH = h, wPair = gradientFromHeight cx h, wWalker = Nothing, wTrails = [] }

-- ★ 高さ → 周波数：高さ 0〜1 を 2 オクターブに割り当て、音階にそろえる
freqOf :: World -> Float -> Float
freqOf w height = 220 * 2 ** (fi semi / 12)
  where
    sc = case drop (wScale w - 1) scales of
      (x : _) -> x
      [] -> U.singleton 0
    l = U.length sc
    idx = floor (clampF 0 0.999 height * fi l * 2) :: Int
    semi = (idx `div` l) * 12 + sc U.! (idx `mod` l)

-- 周波数 freq（Hz）、大きさ vol（0〜1）、長さ dur（秒）、波形の音をひとつ
ping :: Float -> Float -> Float -> Wave -> IO ()
ping f = playTone (clampF 40 4000 f)

startWalker :: Int -> Int -> World -> IO World
startWalker frame v w = do
  ping (freqOf w (wH w U.! v)) 0.3 0.5 Triangle
  pure w { wWalker = Just (Walker v [v] frame) }

-- ★ 1 歩進む：頂点 → ペアの辺 → 反対側の頂点。臨界な頂点（極小）に着いたら和音
advance :: Int -> Walker -> World -> IO World
advance frame wk w
  | e < 0 = do
      let f = freqOf w (wH w U.! v)
      ping f 0.35 1.6 Triangle
      ping (f * 2 ** (4 / 12)) 0.25 1.6 Triangle
      ping (f * 2 ** (7 / 12)) 0.25 1.6 Triangle
      let ts = wTrails w ++ [wkTrail wk]
      pure w { wTrails = drop (length ts - 8) ts, wWalker = Nothing }
  | otherwise = do
      let u = other (cVerts (cell cx e))
          fu = freqOf w (wH w U.! u)
          nearSaddle = any (\q -> wPair w U.! q < 0) (cCofaces (cell cx u))
      if nearSaddle
        then ping (fu * 1.5) 0.2 0.25 Square >> ping (fu * 0.75) 0.2 0.25 Square
        else ping fu 0.28 0.45 Triangle
      pure w { wWalker = Just wk { wkV = u, wkTrail = wkTrail wk ++ [u], wkT = frame } }
  where
    v = wkV wk
    e = wPair w U.! v
    other [a, b] = if a == v then b else a
    other _ = v

stepWorld :: Env -> World -> IO World
stepWorld env w0 = do
  let frame = envFrame env
  w1 <- case wWalker w0 of
    Just wk | frame - wkT wk >= wStep w0 -> advance frame wk w0
    _ -> pure w0
  case wWalker w1 of
    Nothing | wAuto w1 && frame `mod` 50 == 0 -> do
      v <- randomRIO (0, (nx + 1) * (ny + 1) - 1)
      startWalker frame v w1
    _ -> pure w1

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $ Pictures $
  [ box (hsb (200 - v * 150) 45 (22 + v * 55)) (ox + fi i * cs) (oy + fi j * cs) cs cs
  | i <- [0 .. nx - 1], j <- [0 .. ny - 1]
  , let v = sum (map (wH w U.!) (cVerts (cell cx (fid cx i j)))) / 4 ]
  ++ drawGridComplex cx (wPair w) ox oy cs False True
  -- 過去の流れ（うすく）と今の流れ
  ++ [ polyL 3 (hsba 48 80 100 ((15 + fi k * 6) / 100)) (map (pos U.!) t) | (k, t) <- zip [0 :: Int ..] (wTrails w) ]
  ++ walkerPics
  ++ [ txt ox 40 20 white ("Click a vertex and flow down the arrows.  min " ++ show m0 ++ "  saddle " ++ show m1 ++ "  max " ++ show m2
         ++ maybe "" (\wk -> "   going down... " ++ show (length (wkTrail wk)) ++ " steps") (wWalker w))
     , txt ox 68 15 (gray 0.65) ("Click: flow  A: auto " ++ (if wAuto w then "ON" else "off") ++ "  N: new  1-3: scale " ++ scaleName
         ++ "  Up/Dn: speed " ++ show (wStep w) ++ "   high = high pitch  by a saddle = fork  min = chord")
     ]
  where
    (m0, m1, m2) = countCritical cx (wPair w)
    scaleName = concat (take 1 (drop (wScale w - 1) ["pentatonic", "minor", "chromatic"]))
    walkerPics = case wWalker w of
      Nothing -> []
      Just wk ->
        let p@(x, y) = pos U.! wkV wk
        in [ polyL 5 (hsb 48 90 100) (map (pos U.!) (wkTrail wk))
           , dotC (hsb 48 90 100) p (cs * 0.5)
           , Translate x (-(y - cs * 0.55)) (labelC 0 0 14 white (fmt 0 (freqOf w (wH w U.! wkV wk)) ++ " Hz")) ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseP env
        pick (b, bd) c = let d = dist2 m (pos U.! c) in if d < bd then (c, d) else (b, bd)
        (best, _) = foldl' pick (-1, cs * 0.7) [c | c <- [0 .. nCells cx - 1], cDim (cell cx c) == 0]
    in if best >= 0 then startWalker (envFrame env) best w else pure w
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "nN" -> newTerrain w
    | c `elem` "123" -> pure w { wScale = fromEnum c - fromEnum '0' }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wStep = max 2 (wStep w - 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wStep = min 30 (wStep w + 2) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 同じ極小に落ちる 2 つの頂点をクリックして、メロディの「終わり」が同じ和音になるのを聴こう
-- 2. 鞍点のすぐとなりの頂点からスタートすると、左右どちらの極小に落ちる？ 分岐音が鳴る場所を探そう
-- 3. freqOf を変えて「高さが低いほど高い音」にする。地形の印象はどう変わる？
-- 4. 極小に着いたら、そこの流域（09）の大きさに応じて和音の長さを変えてみよう

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

drawArrow :: Float -> Point -> Point -> Picture
drawArrow cs (x1, y1) (x2, y2) = Pictures [seg (max 1.5 (cs * 0.06)) c (x1, y1) (x2, y2), Color c (Polygon (map fl [(x2, y2), side (-0.5), side 0.5]))]
  where
    a = atan2 (y2 - y1) (x2 - x1)
    hd = min 8 (cs * 0.22)
    c = hsb 48 70 100
    side d = (x2 - hd * cos (a + d), y2 - hd * sin (a + d))

-- 格子複体を描く（左上 ox, oy・1 マス cs）。臨界：頂点 = 紫、辺 = ライム、面 = コーラル
--   showArrows で矢印も描く。onlyCrit なら臨界でない面は塗らない
drawGridComplex :: Cx -> U.Vector Int -> Float -> Float -> Float -> Bool -> Bool -> [Picture]
drawGridComplex cx pair ox oy cs showArrows onlyCrit = faces ++ edges ++ verts ++ (if showArrows then arrows else [])
  where
    nx = cxNx cx
    ny = cxNy cx
    gx i = ox + fromIntegral i * cs
    gy j = oy + fromIntegral j * cs
    crit c = pair U.! c < 0
    half = cs / 2
    faces = [ box col (gx i + 1) (gy j + 1) (cs - 2) (cs - 2)
            | i <- [0 .. nx - 1], j <- [0 .. ny - 1], let f = fid cx i j, f >= 0
            , col <- [hsba 12 80 95 0.75 | crit f] ++ [hsb 225 25 22 | not (crit f), not onlyCrit] ]
    edgeLine e a b | crit e = seg 5 (hsb 85 90 95) a b
                   | otherwise = seg 1.5 (gray 0.42) a b
    edges = concat [ [edgeLine e (gx i, gy j) (gx (i + 1), gy j) | let e = ehAt cx i j, e >= 0]
                     ++ [edgeLine e (gx i, gy j) (gx i, gy (j + 1)) | let e = evAt cx i j, e >= 0]
                   | i <- [0 .. nx], j <- [0 .. ny] ]
    verts = [ if crit v then dotC (hsb 272 70 100) (gx i, gy j) (cs * 0.34) else dotC (gray 0.72) (gx i, gy j) (max 3 (cs * 0.14))
            | i <- [0 .. nx], j <- [0 .. ny], let v = vid cx i j, v >= 0 ]
    arrows = concat [ arrowsAt i j (gx i) (gy j) | i <- [0 .. nx], j <- [0 .. ny] ]
    arrowsAt i j x y =
      let v = vid cx i j
          p = if v >= 0 then pair U.! v else -1
          e1 = ehAt cx i j
          e2 = evAt cx i j
          toFace e = e >= 0 && pair U.! e >= 0 && cDim (cell cx (pair U.! e)) == 2
          ar = drawArrow cs
      in [ ar (x, y) (x + half, y) | p >= 0, i < nx, e1 == p ]                               -- 頂点 → 辺
         ++ [ ar (x, y) (x - half, y) | p >= 0, i > 0, ehAt cx (i - 1) j == p ]
         ++ [ ar (x, y) (x, y + half) | p >= 0, j < ny, e2 == p ]
         ++ [ ar (x, y) (x, y - half) | p >= 0, j > 0, evAt cx i (j - 1) == p ]
         ++ [ ar (x + half, y) (x + half, y + half) | toFace e1, j < ny, fid cx i j == pair U.! e1 ]          -- 横の辺 → 面
         ++ [ ar (x + half, y) (x + half, y - half) | toFace e1, j > 0, fid cx i (j - 1) == pair U.! e1 ]
         ++ [ ar (x, y + half) (x + half, y + half) | toFace e2, i < nx, fid cx i j == pair U.! e2 ]          -- 縦の辺 → 面
         ++ [ ar (x, y + half) (x - half, y + half) | toFace e2, i > 0, fid cx (i - 1) j == pair U.! e2 ]
