-- ============================================
-- 05 最適二分探索木：よく引く単語ほど根の近くに
-- キー k1…kn が引かれる確率 p と、間の「見つからない」確率 q から、探索コストの期待値が最小の木をつくる
-- e[i][j] = min over r ( e[i][r-1] + e[r+1][j] + w(i,j) )　─ 根 r をためし、左右の部分木は表から
-- スペース = 再生・停止 / → = 1 手 / 1 = お手本 / R = ランダムな確率 / ↑↓ = 速さ
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import qualified Data.Map.Strict as M
import Text.Printf (printf)

startP, startQ :: [Float]
startP = [0, 0.08, 0.20, 0.06, 0.14, 0.12]    -- ★ p[1..n]：キー ki を探す確率
startQ = [0.06, 0.04, 0.08, 0.05, 0.07, 0.10]  -- ★ q[0..n]：ki と ki+1 の間（見つからない）確率

startFrames :: Int
startFrames = 8

data World = World
  { wP       :: ![Float]
  , wQ       :: ![Float]
  , wE       :: !(M.Map (Int, Int) Float)
  , wWt      :: !(M.Map (Int, Int) Float)
  , wRoot    :: !(M.Map (Int, Int) Int)
  , wL       :: !Int
  , wI       :: !Int
  , wR       :: !Int
  , wBest    :: !Float
  , wFrames  :: !Int
  , wPlaying :: !Bool
  , wDone    :: !Bool
  , wTimer   :: !Int
  }

initial :: World
initial = restart (World startP startQ M.empty M.empty M.empty 1 1 1 infinity startFrames True False 0)

main :: IO ()
main = runSketch (sketch "12-05 Optimal binary search tree" initial)
  { skBack  = cBg
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> onEvent ev w
  }

infinity :: Float
infinity = 1e30

sizeN :: World -> Int
sizeN w = length (wP w) - 1

pAt, qAt :: World -> Int -> Float
pAt w = nth 0 (wP w)
qAt w = nth 0 (wQ w)

restart :: World -> World
restart w =
  let n = sizeN w
      base = M.fromList [((i, i - 1), qAt w (i - 1)) | i <- [1 .. n + 1]]
  in w { wE = base
       , wWt = M.insert (1, 1) (qAt w 0 + pAt w 1 + qAt w 1) base
       , wRoot = M.empty, wL = 1, wI = 1, wR = 1, wBest = infinity, wDone = False }

look :: M.Map (Int, Int) Float -> Int -> Int -> Float
look m a b = M.findWithDefault 0 (a, b) m

-- ★ 1 手：根を r にしたときのコストを調べる
advance :: World -> World
advance w
  | wDone w = w
  | r1 <= j = better { wR = r1 }
  | otherwise =
      let eNew = M.insert (i, j) (wBest better) (wE better)
          i1 = i + 1
          (l2, i2) = if i1 > n - l + 1 then (l + 1, 1) else (l, i1)
      in if l2 > n
           then better { wE = eNew, wBest = infinity, wI = i2, wL = l2, wR = r1, wDone = True }
           else let j2 = i2 + l2 - 1
                    wNew = M.insert (i2, j2) (look (wWt better) i2 (j2 - 1) + pAt w j2 + qAt w j2) (wWt better)
                in better { wE = eNew, wWt = wNew, wBest = infinity, wI = i2, wL = l2, wR = i2 }
  where
    n = sizeN w
    l = wL w
    i = wI w
    r = wR w
    j = i + l - 1
    t = look (wE w) i (r - 1) + look (wE w) (r + 1) j + look (wWt w) i j
    better = if t < wBest w - 1e-6
               then w { wBest = t, wRoot = M.insert (i, j) r (wRoot w) }
               else w
    r1 = r + 1

step :: World -> World
step w
  | wPlaying w && not (wDone w) =
      if wTimer w + 1 >= wFrames w then advance w { wTimer = 0 } else w { wTimer = wTimer w + 1 }
  | otherwise = w

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> toggle
  EventKey (MouseButton LeftButton) Down _ _ -> toggle
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (advance w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wFrames = max 1 (wFrames w - 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wFrames = min 40 (wFrames w + 2) }
  EventKey (Char '1') Down _ _ -> pure (restart w { wP = startP, wQ = startQ })
  EventKey (Char c) Down _ _ | c `elem` "rR" -> do
    k <- randomRIO (4, 7 :: Int)
    raw <- replicateM (2 * k + 1) $ do
      u <- randomRIO (0, 1 :: Float)
      if u < 0.3 then randomRIO (4, 10) else randomRIO (0.5, 3)
    let total = sum raw
        vals = map (/ total) raw
        ps = [v | (a, v) <- zip [0 :: Int ..] vals, odd a]
        qs = [v | (a, v) <- zip [0 :: Int ..] vals, even a]
    pure (restart w { wP = 0 : ps, wQ = qs }) { wPlaying = True }
  _ -> pure w
  where toggle = pure w { wPlaying = not (wPlaying w) }

fmt :: Maybe Float -> String
fmt = maybe "-" (fixN 2)

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env
  ( [ txt 26 cFg 32 24 "Optimal BST: minimize the expected search cost"
    , txt 15 cMuted 32 60 "Space: play/pause   Right: 1 step   1: sample   R: random   Up/Down: speed" ]
  ++ bars ++ table ++ right )
  where
    wd = envW env
    ht = envH env
    n = sizeN w
    -- 確率の棒
    bx = 32
    by = 96
    bw = min 46 (wd * 0.45 / i2f (2 * n + 1))
    bars = concat
      [ [ box (if isKey then cRasp else cCyan) (x + 4) (by + 80 - v * 300) (bw - 8) (v * 300)
        , txtC 14 cMuted (x + bw / 2) (by + 92) ((if isKey then "k" else "d") ++ show idx)
        , txtC 14 cMuted (x + bw / 2) (by + 110) (drop 1 (fixN 2 v)) ]
      | k <- [0 .. 2 * n]
      , let isKey = odd k
      , let idx = k `div` 2 + (if isKey then 1 else 0)
      , let v = if isKey then pAt w idx else qAt w idx
      , let x = bx + i2f k * bw ]
    -- e の表
    cell = minimum [58, (wd * 0.5 - 60) / i2f (n + 1), (ht - 356) / i2f (n + 1)]
    x0 = 60
    y0 = 276
    i' = wI w
    jj = i' + wL w - 1
    r' = wR w
    table =
      txt 15 cFg (x0 - 28) (y0 - 42) "e[i][j]: best expected cost of ki..kj (r = its root)"
      : concat
      [ [txtR 14 cMuted (x0 - 4) (y0 + i2f (i - 1) * cell + cell / 2) ("i" ++ show i)]
        ++ concat
        [ [txtC 14 cMuted (x + cell / 2) (y0 - 12) ("j" ++ show j) | i == 1]
          ++ [ cellBox (case v of
                          Nothing -> cBg
                          Just _ -> if j == i - 1 then rgb255 0x1f 0x2a 0x33 else cPanel)
                       (maybe cGrid id hl) (maybe 1 (const 3) hl) (x + 2) (y + 2) (cell - 4) (cell - 4) ]
          ++ concat
             [ txtC 14 cFg (x + cell / 2) (y + cell * 0.36) (fixN 2 e)
               : [txtC 14 cLime (x + cell / 2) (y + cell * 0.72) ("r=" ++ show rt) | Just rt <- [M.lookup (i, j) (wRoot w)]]
             | Just e <- [v] ]
        | j <- [i - 1 .. n]
        , let x = x0 + i2f j * cell
        , let y = y0 + i2f (i - 1) * cell
        , let v = M.lookup (i, j) (wE w)
        , let hl | wDone w = Nothing
                 | i == i' && j == jj = Just cAmber
                 | i == i' && j == r' - 1 = Just cCyan
                 | i == r' + 1 && j == jj = Just cRasp
                 | otherwise = Nothing ]
      | i <- [1 .. n + 1] ]
    -- 右：計算の途中か、完成した木
    tx = wd * 0.52
    tw = wd - tx - 30
    right = rbox cPanel tx 96 tw (ht - 130) : (if wDone w then finished else working)
    working =
      [ txt 16 cFg (tx + 18) 112 ("Length " ++ show (wL w) ++ ": try k" ++ show r' ++ " as the root of k" ++ show i' ++ "..k" ++ show jj)
      , txt 16 cCyan (tx + 18) 142 ("left  e[" ++ show i' ++ "][" ++ show (r' - 1) ++ "] = " ++ fmt (M.lookup (i', r' - 1) (wE w)))
      , txt 16 cRasp (tx + 18) 168 ("right e[" ++ show (r' + 1) ++ "][" ++ show jj ++ "] = " ++ fmt (M.lookup (r' + 1, jj) (wE w)))
      , txt 16 cAmber (tx + 18) 194 ("one level deeper: w(" ++ show i' ++ "," ++ show jj ++ ") = " ++ fmt (M.lookup (i', jj) (wWt w)))
      , txt 14 cMuted (tx + 18) 236 "Choosing a root pushes both subtrees down one level."
      , txt 14 cMuted (tx + 18) 256 "That extra cost is the sum of their probabilities, w." ]
    layout = build w 1 n 0 Nothing
    depthMax = maximum (0 : [oDepth o | o <- layout])
    slot = (tw - 40) / i2f (2 * n + 1)
    dy = min 80 ((ht - 330) / i2f (max 1 depthMax))
    pos ord depth = (tx + 20 + i2f ord * slot + slot / 2, 200 + i2f depth * dy)
    finished =
      [ txt 18 cFg (tx + 18) 112 ("Best tree: expected cost " ++ fixN 2 (look (wE w) 1 n))
      , txt 14 cMuted (tx + 18) 140 ("(balanced tree with the middle as root: " ++ fixN 2 (balancedCost w) ++ ")") ]
      ++ [ seg (rgb255 0x5a 0x4a 0x66) 2 (pos (oOrd o) (oDepth o)) (pos po pd) | o <- layout, Just (po, pd) <- [oParent o] ]
      ++ map drawItem layout
    drawItem o =
      let (x, y) = pos (oOrd o) (oDepth o)
      in if oKey o > 0
           then Pictures [circ cRasp x y (max 30 (min 44 (slot * 1.6))), txtC 14 cBg x y ("k" ++ show (oKey o))]
           else Pictures [rbox cCyan (x - 15) (y - 13) 30 26, txtC 14 cBg x y ("d" ++ show (oDummy o))]

-- 木の 1 つの部品。oKey が 0 なら「見つからない」葉 d
data Item = Item { oKey :: !Int, oDummy :: !Int, oDepth :: !Int, oOrd :: !Int, oParent :: !(Maybe (Int, Int)) }

-- 根の表 root から木をつくる。ord は左から何番目か（d0 k1 d1 k2 … の順）
build :: World -> Int -> Int -> Int -> Maybe (Int, Int) -> [Item]
build w i j depth parent
  | j <= i - 1 = [Item 0 j depth (2 * j) parent]
  | otherwise =
      let r = M.findWithDefault i (i, j) (wRoot w)
          ord = 2 * r - 1
          me = Just (ord, depth)
      in Item r 0 depth ord parent : build w i (r - 1) (depth + 1) me ++ build w (r + 1) j (depth + 1) me

balancedCost :: World -> Float
balancedCost w = cost 1 (sizeN w) 0
  where
    cost :: Int -> Int -> Int -> Float
    cost i j depth
      | j <= i - 1 = i2f (depth + 1) * qAt w j
      | otherwise =
          let r = (i + j) `div` 2
          in i2f (depth + 1) * pAt w r + cost i (r - 1) (depth + 1) + cost (r + 1) j (depth + 1)

-- ---------- やってみよう ----------
-- 1. startP の 1 つを大きくすると、そのキーが根に近づくのを見る
-- 2. startQ を全部 0 に近づけると、木の形はどう変わる？
-- 3. balancedCost と最適なコストの差が一番大きくなる確率を探す

-- ---------- 道具：色と、左上が原点の座標で描く関数（p5.js の座標のまま書ける） ----------

cBg, cPanel, cFg, cMuted, cRasp, cCyan, cAmber, cLime, cGrid :: Color
cBg    = rgb255 0x16 0x11 0x1c
cPanel = rgb255 0x23 0x1a 0x2b
cFg    = rgb255 0xf3 0xea 0xf0
cMuted = rgb255 0x9a 0x8a 0xa3
cRasp  = rgb255 0xff 0x6b 0x8b
cCyan  = rgb255 0x5e 0xe6 0xe0
cAmber = rgb255 0xff 0xc8 0x57
cLime  = rgb255 0xa3 0xe6 0x35
cGrid  = rgb255 0x3a 0x2f 0x44

-- | 画面全体を「左上が原点」の座標に移す（中の図形は y を下向きで渡す）
screen :: Env -> [Picture] -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Pictures

-- | 左上 (x, y)、幅 w、高さ h の塗りつぶした四角
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Polygon [(x, -y), (x + w, -y), (x + w, -y - h), (x, -y - h)])

-- | 角を落とした四角の頂点（p5.js の rect の角丸の代わり）
cornerPts :: Float -> Float -> Float -> Float -> [Point]
cornerPts x y w h =
  let r = min 5 (min (abs w) (abs h) / 3)
  in [ (x + r, -y), (x + w - r, -y), (x + w, -y - r), (x + w, -y - h + r)
     , (x + w - r, -y - h), (x + r, -y - h), (x, -y - h + r), (x, -y - r) ]

-- | 角を落とした塗りつぶしの四角
rbox :: Color -> Float -> Float -> Float -> Float -> Picture
rbox c x y w h = Color c (Polygon (cornerPts x y w h))

-- | 四角の枠線（太さ t）
outline :: Color -> Float -> Float -> Float -> Float -> Float -> Picture
outline c t x y w h = case cornerPts x y w h of
  ps@(p : _) | t <= 1.2  -> Color c (Line (ps ++ [p]))
             | otherwise -> Color c (thickPath t (ps ++ [p]))
  [] -> Blank

-- | 塗りと枠線のある表のマス
cellBox :: Color -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
cellBox f s t x y w h = Pictures [rbox f x y w h, outline s t x y w h]

-- | 線分（太さ t）
seg :: Color -> Float -> Point -> Point -> Picture
seg c t (x1, y1) (x2, y2)
  | t <= 1.2  = Color c (Line [(x1, -y1), (x2, -y2)])
  | otherwise = Color c (thickLine t (x1, -y1) (x2, -y2))

-- | 円（p5.js の circle と同じく d は直径）
circ :: Color -> Float -> Float -> Float -> Picture
circ c x y d = Color c (Translate x (-y) (disc (d / 2)))

-- | 文字：(x, y) を左上にして書く
txt :: Float -> Color -> Float -> Float -> String -> Picture
txt size c x y = label x (-y - size) size c

-- | 文字：(x, y) を中心にして書く
txtC :: Float -> Color -> Float -> Float -> String -> Picture
txtC size c x y s = label (x - fromIntegral (length s) * size * 0.4) (-y - size / 2) size c s

-- | 文字：(x, y) を右端の真ん中にして書く
txtR :: Float -> Color -> Float -> Float -> String -> Picture
txtR size c x y s = label (x - fromIntegral (length s) * size * 0.8) (-y - size / 2) size c s

-- | 小数 n 桁の文字列（p5.js の toFixed）
fixN :: Int -> Float -> String
fixN n = printf ("%." ++ show n ++ "f")

-- | リストの i 番目（範囲外なら d）
nth :: a -> [a] -> Int -> a
nth d xs i
  | i < 0 = d
  | otherwise = case drop i xs of
      (x : _) -> x
      [] -> d

-- | 整数を小数に
i2f :: Int -> Float
i2f = fromIntegral
