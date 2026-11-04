-- ============================================
-- 06 モース不等式 ─ 臨界胞体の数はベッチ数より少なくできない
-- 高さ関数（ノイズ）から勾配場を作り、臨界胞体の数 m_k とベッチ数 b_k をバーグラフでくらべる
--   弱いモース不等式  m_k ≥ b_k　　　オイラーの関係  m0 − m1 + m2 = b0 − b1 + b2 = χ
--   強いモース不等式  m1 − m0 ≥ b1 − b0　（m0 が多ければ m1 もそのぶん多い：極小と鞍点はペアで余る）
-- 1〜3 = 円盤・円環・穴 2 つ / N = 新しい高さ関数 / ←→ = ノイズの細かさ / A = 矢印の表示
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
nx = 16             -- ★ 格子の大きさ
ny = 10

cs, ox, oy :: Float
cs = 48
ox = 50
oy = 110

data World = World
  { wCx    :: !Cx
  , wBetti :: !Betti
  , wShape :: !Int
  , wH     :: !(U.Vector Float)
  , wPair  :: !(U.Vector Int)
  , wHist  :: [(Int, Int, Int)]
  , wFreq  :: !Float
  , wArrow :: !Bool
  }

main :: IO ()
main = runSketch (sketch "37-06 Morse Inequalities" (world0 1 0.45 False))
  { skBack  = hsb 225 30 8
  , skStart = \_ -> newHeight (world0 1 0.45 False)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ★ 複体の形
world0 :: Int -> Float -> Bool -> World
world0 shapeNo fq ar = World cx (bettiNumbers cx) shapeNo U.empty (U.replicate (nCells cx) (-1)) [] fq ar
  where
    holes = case shapeNo of
      1 -> []
      2 -> [(6, 3, 4, 4)]
      _ -> [(3, 3, 3, 4), (10, 3, 3, 4)]
    inHole i j (x, y, w, h) = i >= x && i < x + w && j >= y && j < y + h
    cx = makeGridComplex nx ny Plain (\i j -> not (any (inHole i j) holes))

-- ★ 高さ関数 → 勾配場
newHeight :: World -> IO World
newHeight w = do
  s <- newSeed
  let cx = wCx w
      fq = wFreq w
      h = heightFrom cx (\i j -> noiseP s (fi i * fq) (fi j * fq) + 0.0001 * (fi i * 3 + fi j))
      pair = gradientFromHeight cx h
      m = countCritical cx pair
  pure w { wH = h, wPair = pair, wHist = drop (length (wHist w) + 1 - 12) (wHist w) ++ [m] }

drawBars :: Float -> Float -> Float -> Float -> [Int] -> [Int] -> [Picture]
drawBars x y w hh ms bs = concat (zipWith3 bar [0 :: Int ..] ms bs) ++ [seg 1 (gray 0.4) (x, y + hh) (x + w, y + hh)]
  where
    top = fromIntegral (maximum (4 : ms ++ bs)) :: Float
    bw = w / 3
    bar k m b =
      let bx = x + fi k * bw
          hm = fi m / top * hh
          hb = fi b / top * hh
          bwid = bw * 0.4 - 10
          mx = bx + 10
          nx' = bx + bw * 0.5
          hue = case k of { 0 -> 272; 1 -> 85; _ -> 12 }
          center cxp cy s col str = Translate cxp (-cy) (labelC 0 0 s col str)
      in [ box (hsb hue 70 95) mx (y + hh - hm) bwid hm
         , box (gray 0.7) nx' (y + hh - hb) bwid hb
         , center (mx + bwid / 2) (y + hh - hm - 14) 16 white (show m)
         , center (nx' + bwid / 2) (y + hh - hb - 14) 16 white (show b)
         , center (mx + bwid / 2) (y + hh + 16) 14 (gray 0.85) ("m" ++ show k)
         , center (nx' + bwid / 2) (y + hh + 16) 14 (gray 0.85) ("b" ++ show k)
         , center (bx + bw / 2) (y + hh + 38) 14 (gray 0.65) (concat (take 1 (drop k ["min/blob", "saddle/hole", "max/void"])))
         ]

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $ Pictures $
  -- 高さを面の色に
  [ box (hsb (200 - v * 150) 40 (25 + v * 45)) (ox + fi i * cs) (oy + fi j * cs) cs cs
  | i <- [0 .. nx - 1], j <- [0 .. ny - 1], let f = fid cx i j, f >= 0
  , let vs = cVerts (cell cx f), let v = sum (map (wH w U.!) vs) / fi (length vs) ]
  ++ drawGridComplex cx (wPair w) ox oy cs (wArrow w) True
  ++ drawBars gx (oy + 20) gw 240 [m0, m1, m2] [b0 b, b1 b, b2 b]
  ++ zipWith checkLine [0 :: Int ..] checks
  ++ [ txt gx (oy + 462 + fi k * 20) 14 (gray 0.65) l | (k, l) <- zip [0 :: Int ..] histLines ]
  ++ [ txt ox 44 20 white ("Shape: " ++ shapeName ++ "  chi = " ++ show (chi b) ++ "   critical m = " ++ showM m ++ "  Betti b = " ++ showM (b0 b, b1 b, b2 b))
     , txt ox 72 15 (gray 0.65) ("1-3: shape  N: new height  L/R: detail " ++ fmt 2 (wFreq w) ++ "  A: arrows " ++ (if wArrow w then "ON" else "off") ++ "   color = height (blue low, yellow high)")
     , txt ox (oy + fi ny * cs + 36) 14 (gray 0.55) "For any height function m_k never drops below b_k. An extra min always comes with an extra saddle (removed in 07)."
     ]
  where
    cx = wCx w
    b = wBetti w
    m@(m0, m1, m2) = countCritical cx (wPair w)
    gx = ox + fi nx * cs + 50
    gw = scrW - gx - 40
    shapeName = concat (take 1 (drop (wShape w - 1) ["disc", "annulus", "2 holes"]))
    alt = m0 - m1 + m2
    -- 不等式の判定
    checks =
      [ ("weak  m_k >= b_k", Just (m0 >= b0 b && m1 >= b1 b && m2 >= b2 b))
      , ("Euler m0-m1+m2 = " ++ show alt ++ " = chi = " ++ show (chi b), Just (alt == chi b))
      , ("strong m1-m0 = " ++ show (m1 - m0) ++ " >= b1-b0 = " ++ show (b1 b - b0 b), Just (m1 - m0 >= b1 b - b0 b))
      , ("Extra: min " ++ show (m0 - b0 b) ++ ", saddle " ++ show (m1 - b1 b) ++ ", max " ++ show (m2 - b2 b), Nothing)
      ]
    checkLine k (t, ok) = case ok of
      Nothing -> txt gx (oy + 336 + fi k * 28) 16 (gray 0.7) t
      Just True -> txt gx (oy + 336 + fi k * 28) 16 (hsb 85 80 100) ("OK " ++ t)
      Just False -> txt gx (oy + 336 + fi k * 28) 16 (hsb 0 80 100) ("NG " ++ t)
    -- 履歴（4 つずつ折り返す）
    histLines = "(m0,m1,m2) so far:" : chunks (map (\(a, c, d) -> "(" ++ show a ++ "," ++ show c ++ "," ++ show d ++ ")") (wHist w))
    chunks [] = []
    chunks l = unwords (take 4 l) : chunks (drop 4 l)

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "nN" -> newHeight w
    | c `elem` "aA" -> pure w { wArrow = not (wArrow w) }
    | c `elem` "123" -> newHeight (world0 (fromEnum c - fromEnum '0') (wFreq w) (wArrow w))
  EventKey (SpecialKey KeyRight) Down _ _ -> newHeight w { wFreq = min 1.2 (wFreq w + 0.1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> newHeight w { wFreq = max 0.15 (wFreq w - 0.1) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. N を 10 回押して、m0 − m1 + m2 がいつも χ になることを確かめよう（履歴を見る）
-- 2. ← で高さ関数をなめらかにすると m が b に近づく。m = b（完全モース関数）になるか？
-- 3. 円環（2）で m1 = 1 になるとき、その鞍点（ライムの辺）は穴のどこにある？
-- 4. 穴 2 つ（3）で「強い不等式」が等号になるのはどんなとき？

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

-- Union-Find：n 個の要素を組 (a, b) で結び、各要素の根を返す
components :: Int -> [(Int, Int)] -> U.Vector Int
components n links = U.create $ do
  p <- U.thaw (U.enumFromN 0 n)
  let find a = do
        q <- MU.read p a
        if q == a then pure a else do
          r <- MU.read p q
          MU.write p a r
          find r
      unite (a, b) = do
        x <- find a
        y <- find b
        if x /= y then MU.write p x y else pure ()
  mapM_ unite links
  mapM_ (\i -> find i >>= MU.write p i) [0 .. n - 1]
  pure p

-- ベッチ数：b0 = つながった塊の数（Union-Find）、b2 = 閉じた曲面の数、b1 = b0 + b2 − χ（Z2 係数）
data Betti = Betti { b0, b1, b2, chi, nV, nE, nF :: !Int }

bettiNumbers :: Cx -> Betti
bettiNumbers cx = Betti bb0 (bb0 + bb2 - x) bb2 x v e f
  where
    cs = [0 .. nCells cx - 1]
    ofDim d = [c | c <- cs, cDim (cell cx c) == d]
    v = length (ofDim 0)
    e = length (ofDim 1)
    f = length (ofDim 2)
    x = v - e + f
    links = [ (a, b) | c <- ofDim 1, [a, b] <- [cVerts (cell cx c)] ]
         ++ [ (a, b) | c <- ofDim 2, ed <- cFaces (cell cx c), [a, b] <- [cCofaces (cell cx ed)] ]   -- 辺を共有する面をつなぐ
    root = components (nCells cx) links
    bb0 = length [c | c <- ofDim 0, root U.! c == c]
    closed = IM.fromListWith (&&) [ (root U.! c, all (\ed -> cNcof (cell cx ed) == 2) (cFaces (cell cx c))) | c <- ofDim 2 ]
    bb2 = length (filter id (IM.elems closed))
