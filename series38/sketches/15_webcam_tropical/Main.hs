-- ============================================
-- 38-15 カメラ映像からトロピカル曲線（おまけ）
-- カメラ画像を縮小し、ニュートン多角形の格子点 (i, j) に対応する場所の明るさを係数 a_ij にする
--   暗いところ = 低い = その項が勝ちやすい。明るさが変わると曲線が動く（カメラの許可が必要。なければノイズ画像）
-- 操作：↑↓ = 次数 d / I = 明暗を反転 / V = 映像の表示切替 / R = 領域の色分け
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString as B
import Data.List (foldl', sortOn)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

vw, vh :: Int
vw = 64                    -- ★ カメラ画像の大きさ（小さいほど速い）
vh = 48

amp :: Double
amp = 5                    -- ★ 明るさ → 高さの幅

scale4 :: Float
scale4 = 6

data World = World
  { wCam :: !(Maybe Camera)
  , wReady :: !Bool                -- 本物のカメラか
  , wVideo :: !B.ByteString        -- 最新の映像（RGB）
  , wD :: !Int                     -- ★ 次数（2〜7）
  , wInvert, wShowVideo, wRegions :: !Bool
  , wTerms :: !(V.Vector Term) }

world0 :: World
world0 = World Nothing False B.empty 5 False True True (makeTerms 5)

main :: IO ()
main = runSketch (sketch "38-15 Tropical Curve from the Camera" world0)
  { skBack  = bgCol
  , skStart = \_ -> do
      cam <- startCamera vw vh
      pure world0 { wCam = Just cam }
  , skStep  = stepWorld
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

scOf, ox, oy :: Env -> Float
scOf env = min (envW env) (envH env) / 14
ox env = envW env * 0.5
oy env = envH env * 0.5

-- ★ 格子点 (i, j) → カメラ画像のどこを見るか（三角形を画像全体に広げる）
samplePos :: Int -> Term -> (Int, Int)
samplePos d t =
  let fd = fromIntegral d :: Float
      i = fromIntegral (tI t) :: Float
      j = fromIntegral (tJ t) :: Float
  in ( floor ((i + j * 0.5) / fd * fromIntegral (vw - 1))
     , floor ((1 - j / fd) * fromIntegral (vh - 1)) )

-- 映像の (x, y) の色（0〜255）
videoAt :: B.ByteString -> Int -> Int -> (Float, Float, Float)
videoAt f x y =
  let o = (y * vw + x) * 3
      g k = if k < B.length f then fromIntegral (B.index f k) else 0
  in (g o, g (o + 1), g (o + 2))

-- 明るさ → 高さ（カメラがなければノイズ）
stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = do
  (ready, video) <- case wCam w of
    Just cam -> do
      live <- camLive cam
      f <- if live then cameraFrame cam else pure B.empty
      pure (live, f)
    Nothing -> pure (False, B.empty)
  let frameT = fromIntegral (envFrame env) * 0.01 :: Float
      update t =
        let (px, py) = samplePos (wD w) t
            raw :: Float
            raw | ready = let (r, g, b) = videoAt video px py in (r + g + b) / 765
                | otherwise = noise3 (fromIntegral (tI t) * 0.5) (fromIntegral (tJ t) * 0.5) frameT
            b0 = realToFrac (if wInvert w then 1 - raw else raw) :: Double
            goal = b0 * amp + 0.1 * fromIntegral (tI t * tI t + tJ t * tJ t)
        in t { tA = tA t + (goal - tA t) * 0.2 }                 -- なめらかに追いかける
  pure w { wReady = ready, wVideo = video, wTerms = V.map update (wTerms w) }

-- 背景の映像（薄く）と領域の色分けを 1 枚の粗い画像に重ねる
backdrop :: Env -> World -> Picture
backdrop env w
  | not (wRegions w) && not useVideo = Blank
  | otherwise = pixelImage bw bh scale4 pixel
  where
    useVideo = wShowVideo w && wReady w
    bw = ceiling (envW env / scale4) :: Int
    bh = ceiling (envH env / scale4) :: Int
    ts = wTerms w
    n = V.length ts
    as = U.generate n (\k -> realToFrac (tA (ts V.! k))) :: U.Vector Float
    is = U.generate n (\k -> fromIntegral (tI (ts V.! k))) :: U.Vector Float
    js = U.generate n (\k -> fromIntegral (tJ (ts V.! k))) :: U.Vector Float
    sc = scOf env
    pixel qx py =
      let (br, bg, bb)
            | useVideo =
                let (r, g, b) = videoAt (wVideo w) (qx * vw `div` bw) (py * vh `div` bh)
                in (lerpF 11 r 0.35, lerpF 23 g 0.35, lerpF 26 b 0.35)
            | otherwise = (11, 23, 26)
      in if not (wRegions w) then (truncate br, truncate bg, truncate bb) else
         let y = (oy env - fromIntegral py * scale4) / sc
             x = (fromIntegral qx * scale4 - ox env) / sc
             go :: Int -> Float -> Float -> Int -> (Float, Float, Int)
             go !k !m1 !m2 !best
               | k >= n = (m1, m2, best)
               | v < m1 = go (k + 1) v m1 k
               | v < m2 = go (k + 1) m1 v best
               | otherwise = go (k + 1) m1 m2 best
               where v = U.unsafeIndex as k + U.unsafeIndex is k * x + U.unsafeIndex js k * y
             (a1, a2, b0) = go 0 (1 / 0) (1 / 0) 0
             ti = U.unsafeIndex is b0
             tj = U.unsafeIndex js b0
             sh = 0.5 + 0.5 * min 1 ((a2 - a1) / 0.6)
             mixA = 150 / 255 :: Float                           -- 領域の色の透明度
             q base c = truncate (lerpF base (c * sh) mixA)
         in (q br (120 + ti * 30), q bg (60 + tj * 35), q bb (200 - (ti + tj) * 20))

draw :: Env -> World -> Picture
draw env w = Pictures (
  [ backdrop env w ]
  -- 曲線
  ++ concat [ let (a, b) = edgeEnds cv e
              in [ seg env (5 + wf e * 3) (hsb 0 0 10) (us a) (us b)
                 , seg env (2 + wf e * 2) (hsb 35 80 100) (us a) (us b) ]
            | e <- cvEdges cv ]
  ++ [ let (x, y) = us (cellXY c) in dotP env x y 8 (gray 1) | c <- V.toList (cvCells cv) ]
  -- 右下：ニュートン多角形と、どこの明るさを見ているか
  ++ [ box env (nx - 20) (ny - fd * ns - 20) (fd * ns + 40) (fd * ns + 40) (rgba255 0 0 0 128) ]
  ++ [ dotP env (nx + fromIntegral (tI t) * ns) (ny - fromIntegral (tJ t) * ns) 14
         (hsb 0 0 (100 - min 100 (max 0 (realToFrac (tA t / amp) * 100))))
     | t <- V.toList (wTerms w) ]
  ++ thumbnail
  ++ [ box env 0 0 (envW env) 72 (rgba255 0 0 0 130)
     , txt env 20 30 16 (gray 1) ((if wReady w then "Camera" else "No camera -> noise") ++ "   Up/Down: degree d = " ++ show d ++ " (" ++ show (V.length (wTerms w)) ++ " terms)   I: invert " ++ (if wInvert w then "ON" else "off") ++ "   V: video   R: region colours")
     , txt env 20 56 14 (gray 0.85) "Lower right = Newton polygon (whiter = lower = stronger). Hold up a dark object: that term's region grows." ] )
  where
    d = wD w
    fd = fromIntegral d :: Float
    ns = 28
    nx = envW env - 40 - fd * 28
    ny = envH env - 40
    cv = tropCurve (wTerms w)
    wf e = fromIntegral (eW e) :: Float
    us (x, y) = (ox env + x * scOf env, oy env - y * scOf env)
    -- 左下：カメラ画像と、各格子点が見ている画素
    thumbnail = case (wReady w, wCam w) of
      (True, Just cam) ->
        let s = 1.5
            tx = 20
            ty = envH env - fromIntegral vh * s - 20
            (cx, cy) = pt env (tx + fromIntegral vw * s / 2) (ty + fromIntegral vh * s / 2)
        in at (cx, cy) (camImage cam (wVideo w) s)
           : [ let (px, py) = samplePos d t
                   (gx, gy) = pt env (tx + fromIntegral px * s + s / 2) (ty + fromIntegral py * s + s / 2)
               in Color (hsba 0 0 100 0.6) (rectLine gx gy s s)
             | t <- V.toList (wTerms w) ]
      _ -> []

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> setDegree (min 7 (wD w + 1))
  EventKey (SpecialKey KeyDown) Down _ _ -> setDegree (max 2 (wD w - 1))
  EventKey (Char ch) Down _ _
    | ch `elem` "iI" -> w { wInvert = not (wInvert w) }
    | ch `elem` "vV" -> w { wShowVideo = not (wShowVideo w) }
    | ch `elem` "rR" -> w { wRegions = not (wRegions w) }
  _ -> w
  where
    setDegree d = w { wD = d, wTerms = makeTerms d }

-- ---------- やってみよう ----------
-- 1. 手を画面の左下・右下・上に動かして、曲線のどの部分が動くか観察しよう（samplePos の対応）
-- 2. amp を大きくすると曲線は大きく動く。小さくするとほぼ 0.1(i²+j²) の「お椀」だけ = 規則正しい六角形模様
-- 3. 明るさの代わりに「赤の強さ − 青の強さ」を高さにする（色で動く曲線）
-- 4. 14 のように各領域の色を項ごとに決めて、カメラの色と混ぜてみよう


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）→ gloss の座標（中央が原点、y は上向き）
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js と同じ座標で返す
mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

-- 中央揃えの文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = let (gx, gy) = pt env x y in labelC gx gy s c str

-- 左上が (x, y) の塗りつぶした長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 線分（太さ wd）。太さ 1 以下は細い線で描く
seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env wd c (x1', y1') (x2', y2')
  | wd <= 1 = Color c (Line [a, b])
  | otherwise = Color c (thickLine wd a b)
  where a = pt env x1' y1'
        b = pt env x2' y2'

-- 塗りつぶした円（直径 d）
dotP :: Env -> Float -> Float -> Float -> Color -> Picture
dotP env x y d c = Color c (at (pt env x y) (disc (d / 2)))

-- 暗いふちのある円（直径 d）
dotO :: Env -> Float -> Float -> Float -> Color -> Picture
dotO env x y d c = Pictures [dotP env x y (d + 2) (hsb 0 0 10), dotP env x y (d - 2) c]

bgCol :: Color
bgCol = hsb 190 55 10

-- 小数 n 桁の文字列（p5.js の nf）
nf :: Int -> Float -> String
nf n' v = noMinusZero (showFFloat (Just n') v "")

nfd :: Int -> Double -> String
nfd n' v = noMinusZero (showFFloat (Just n') v "")

-- "-0.00" は "0.00" と書く
noMinusZero :: String -> String
noMinusZero str = if all (`elem` "-0.") str then dropWhile (== '-') str else str


-- ==================================================
-- ここから下は「トロピカル曲線の道具」
--   Term i j a … 単項 a ⊗ x^i ⊗ y^j（a = 係数 = 格子点 (i, j) の高さ）
--   F(x, y) = min (a + i x + j y)。tropCurve は曲線を「頂点と辺」で返す
--     Cell … 細分のセル（高さの下側凸包の面）⇔ 曲線の頂点 (x, y)。cIds は項番号
--     Edge … 細分の辺（項 p–q）⇔ 曲線の辺。eW = 格子長 = 重み
--       eC2 >= 0 なら頂点 eC1 → eC2 の線分。eC2 < 0 なら頂点 eC1 から方向 (eDx, eDy) の半直線
--   同じ平面の上にあるかを 1e-7 の精度で調べるので、高さと頂点は Double で持つ
-- ==================================================
data Term = Term { tI :: !Int, tJ :: !Int, tA :: !Double }

data Cell = Cell { cIds :: [Int], cX :: !Double, cY :: !Double }

data Edge = Edge
  { eP, eQ :: !Int      -- 細分の辺の両端（項番号）
  , eW :: !Int          -- 重み
  , eC1, eC2 :: !Int    -- 両側のセル（eC2 < 0 なら半直線）
  , eDx, eDy :: !Int }  -- 原始方向ベクトル

data Curve = Curve { cvCells :: V.Vector Cell, cvEdges :: [Edge] }

tropCurve :: V.Vector Term -> Curve
tropCurve ts = Curve cells edges
  where
    n' = V.length ts
    eps' = 1e-7 :: Double
    term k = ts V.! k
    fi :: Int -> Double
    fi = fromIntegral
    -- 3 点を通る平面 z = al + be*i + ga*j。すべての点がその上側なら「下側凸包の面」
    face p q r =
      let tp = term p
          tq = term q
          tr = term r
          di1 = tI tq - tI tp
          dj1 = tJ tq - tJ tp
          di2 = tI tr - tI tp
          dj2 = tJ tr - tJ tp
          det = di1 * dj2 - dj1 * di2
          da1 = tA tq - tA tp
          da2 = tA tr - tA tp
          be = (da1 * fi dj2 - fi dj1 * da2) / fi det
          ga = (fi di1 * da2 - fi di2 * da1) / fi det
          al = tA tp - be * fi (tI tp) - ga * fi (tJ tp)
          collect :: Int -> [Int] -> Maybe [Int]
          collect !k acc
            | k >= n' = Just (reverse acc)
            | dd < negate eps' = Nothing
            | dd < eps' = collect (k + 1) (k : acc)
            | otherwise = collect (k + 1) acc
            where t = term k
                  dd = tA t - (al + be * fi (tI t) + ga * fi (tJ t))
      in if det == 0 then Nothing else fmap (\ids -> (ids, negate be, negate ga)) (collect 0 [])
    faces = [f | p <- [0 .. n' - 1], q <- [p + 1 .. n' - 1], r <- [q + 1 .. n' - 1], Just f <- [face p q r]]
    -- 同じ面は 1 回だけ。面の傾き → 曲線の頂点
    cells = V.fromList (reverse (snd (foldl' addFace (Set.empty, []) faces)))
    addFace (seen, acc) (ids, x, y)
      | Set.member ids seen = (seen, acc)
      | length hull >= 3 = (seen', Cell hull x y : acc)
      | otherwise = (seen', acc)
      where seen' = Set.insert ids seen
            hull = hull2d ts ids
    -- セルのまわりの辺を集める。2 つのセルが共有する辺は有界な辺、1 つだけなら半直線
    (_, rawEdges, _) = V.ifoldl' addCell (Map.empty, IntMap.empty, 0 :: Int) cells
    addCell st ci c =
      let ids = cIds c
      in foldl' (addEdge ci) st (zip ids (drop 1 ids ++ take 1 ids))
    addEdge ci (mp, es, next) (p, q) =
      let key = (min p q, max p q)
      in case Map.lookup key mp of
           Just ei -> (mp, IntMap.adjust (\(a, b, c1, _) -> (a, b, c1, ci)) ei es, next)
           Nothing -> (Map.insert key next mp, IntMap.insert next (p, q, ci, -1 :: Int) es, next + 1)
    total = fromIntegral (max n' 1) :: Double
    ci0 = sum [fi (tI t) / total | t <- V.toList ts]
    cj0 = sum [fi (tJ t) / total | t <- V.toList ts]
    edges = map finish (IntMap.elems rawEdges)
    finish (p, q, c1, c2) =
      let tp = term p
          tq = term q
          di = tI tq - tI tp
          dj = tJ tq - tJ tp
          g = gcdInt (abs di) (abs dj)
          ux' = dj `quot` g                -- 細分の辺に垂直な原始ベクトル
          uy' = negate di `quot` g
          ca = cells V.! c1
          s | c2 >= 0 = let cb = cells V.! c2 in fi ux' * (cX cb - cX ca) + fi uy' * (cY cb - cY ca)
            | otherwise = fi ux' * (ci0 - fi (tI tp)) + fi uy' * (cj0 - fi (tJ tp))   -- 半直線はニュートン多角形の内向き（min 流儀）
          sg = if s < 0 then -1 else 1
      in Edge p q g c1 c2 (sg * ux') (sg * uy')

gcdInt :: Int -> Int -> Int
gcdInt a b = let g = gcd a b in if g == 0 then 1 else g

-- 格子点の集合の凸包（反時計回り。一直線上の点は除く）
hull2d :: V.Vector Term -> [Int] -> [Int]
hull2d ts ids
  | length pts < 3 = pts
  | otherwise = reverse (drop 1 (chain pts)) ++ reverse (drop 1 (chain (reverse pts)))
  where
    pts = sortOn (\k -> let t = ts V.! k in (tI t, tJ t)) ids
    turn o a b =
      let to = ts V.! o
          ta = ts V.! a
          tb = ts V.! b
      in (tI ta - tI to) * (tJ tb - tJ to) - (tJ ta - tJ to) * (tI tb - tI to)
    chain = foldl' push []
    push (b : a : rest) p | turn a b p <= 0 = push (a : rest) p
    push st p = p : st

-- 次数 d のすべての項 (i + j <= d) を高さ 0 でつくる
makeTerms :: Int -> V.Vector Term
makeTerms d = V.fromList [Term i j 0 | i <- [0 .. d], j <- [0 .. d - i]]

-- 頂点の座標
cellXY :: Cell -> Point
cellXY c = (realToFrac (cX c), realToFrac (cY c))

-- 辺の両端（半直線は長さ 100 で切る）
edgeEnds :: Curve -> Edge -> (Point, Point)
edgeEnds cv e =
  let a@(ax, ay) = cellXY (cvCells cv V.! eC1 e)
      b | eC2 e >= 0 = cellXY (cvCells cv V.! eC2 e)
        | otherwise = (ax + fromIntegral (eDx e) * 100, ay + fromIntegral (eDy e) * 100)
  in (a, b)
