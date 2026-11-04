-- ============================================
-- 38-07 アメーバと Maslov 脱量子化
-- 複素曲線 Σ t^{a_ij} z^i w^j = 0 の点 (z, w) を乱数で拾い、(log_t|z|, log_t|w|) に打つ = アメーバ
--   底 t（0 < t < 1）を小さくするとアメーバは細くなり、トロピカル曲線 min(a_ij + i x + j y) に近づく
--   （log_t をとる = 「掛け算 → 足し算」「足し算 → min」に化ける：Maslov の脱量子化）
-- 操作：↑↓ = 底 t / M = 直線 ⇄ 2 次曲線 / T = トロピカル曲線を重ねる / C = 描き直す
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', intercalate, sortOn)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Numeric (showFFloat)
import System.Random (StdGen, mkStdGen, newStdGen, randomR)

perFrame :: Int
perFrame = 1500            -- ★ 1 フレームに打つ点の数

rangeR :: Double
rangeR = 7                 -- ★ 座標の範囲 ±R

-- 点を貯める画像：横 ±7.02、縦 ±8.01 を 1 マス 0.06 単位（画面で 3 px）で数える
cellU :: Double
cellU = 0.06

bufW, bufH :: Int
bufW = 234
bufH = 267

data World = World
  { wT :: !Double          -- ★ 底 t（0.001 〜 0.9）。小さいほど細い
  , wKind :: !Int          -- 1 = 直線 1 + z + w，2 = 2 次曲線
  , wTrop :: !Bool
  , wTerms :: !(V.Vector Term)
  , wCount :: !Int
  , wBuf :: !(U.Vector Word8)   -- マスごとの点の数（40 で止める）
  , wGen :: !StdGen }

emptyBuf :: U.Vector Word8
emptyBuf = U.replicate (bufW * bufH) 0

-- ★ 係数 a_ij：複素曲線の係数は t^{a_ij}
termsOf :: Int -> V.Vector Term
termsOf 1 = V.fromList [Term 0 0 0, Term 1 0 0, Term 0 1 0]
termsOf _ = V.fromList [Term 0 0 1, Term 1 0 0, Term 2 0 1, Term 0 1 0, Term 1 1 0, Term 0 2 1]

clearBuf :: World -> World
clearBuf w = w { wBuf = emptyBuf, wCount = 0 }

world0 :: World
world0 = World 0.3 1 True (termsOf 1) 0 emptyBuf (mkStdGen 1)

main :: IO ()
main = runSketch (sketch "38-07 Amoeba and Dequantization" world0)
  { skBack  = bgCol
  , skStart = \_ -> do
      g <- newStdGen
      pure world0 { wGen = g }
  , skStep  = \_ _ w -> pure (sprinkle w)
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

scOf :: Env -> Float
scOf env = min (envW env) (envH env) / (2 * realToFrac rangeR + 2)

toX, toY :: Env -> Float -> Float
toX env x = envW env * 0.5 + x * scOf env
toY env y = envH env * 0.5 - y * scOf env

-- 複素数の道具
data C = C !Double !Double

cmul, cadd, cdiv :: C -> C -> C
cmul (C a b) (C c d) = C (a * c - b * d) (a * d + b * c)
cadd (C a b) (C c d) = C (a + c) (b + d)
cdiv (C a b) (C c d) = let m = c * c + d * d in C ((a * c + b * d) / m) ((b * c - a * d) / m)

cneg :: C -> C
cneg (C a b) = C (negate a) (negate b)

cabs :: C -> Double
cabs (C a b) = sqrt (a * a + b * b)

csqrt :: C -> C
csqrt p@(C a b) =
  let r = cabs p
      re = sqrt ((r + a) / 2)
      im = sqrt (max 0 ((r - a) / 2))
  in C re (if b < 0 then negate im else im)

-- ★ z を決めて w を解く：w について 2 次以下の多項式 c2 w² + c1 w + c0 = 0（係数は z の多項式）
solveW :: Double -> V.Vector Term -> C -> [C]
solveW t ts z
  | cabs c2 < 1e-300 = if cabs c1 < 1e-300 then [] else [cdiv (cneg c0) c1]
  | otherwise =
      let disc' = cadd (cmul c1 c1) (cmul (C (-4) 0) (cmul c2 c0))
          s = csqrt disc'
          two = cmul (C 2 0) c2
      in [cdiv (cadd (cneg c1) s) two, cdiv (cadd (cneg c1) (cneg s)) two]
  where
    zp :: Int -> C
    zp 0 = C 1 0
    zp 1 = z
    zp _ = cmul z z
    coefJ :: Int -> C
    coefJ j = V.foldl' (\acc tm -> if tJ tm == j then cadd acc (cmul (C (t ** tA tm) 0) (zp (tI tm))) else acc) (C 0 0) ts
    c0 = coefJ 0
    c1 = coefJ 1
    c2 = coefJ 2

-- 点を打つ（蓄積）。1 フレームぶんの点のマス番号を集めて、まとめて数える
sprinkle :: World -> World
sprinkle w =
  let lt = log (wT w)
      go :: Int -> StdGen -> [Int] -> Int -> ([Int], StdGen, Int)
      go 0 g acc !n = (acc, g, n)
      go k g acc !n =
        let (rx, g1) = randomR (0, 1 :: Double) g
            (rt, g2) = randomR (0, 1 :: Double) g1
            x = rx * 2 * rangeR - rangeR
            th = rt * pi * 2
            r = exp (x * lt)                 -- |z| = t^x
            z = C (r * cos th) (r * sin th)
            ys = [ y | s <- solveW (wT w) (wTerms w) z
                     , let aw = cabs s
                     , aw > 0, not (isInfinite aw)
                     , let y = log aw / lt   -- log_t|w|
                     , abs y <= rangeR + 1 ]
            acc' = foldl' (\as y -> cellIndex x y : as) acc ys
        in go (k - 1) g2 acc' (n + length ys)
      (hits, gen', added) = go perFrame (wGen w) [] 0
      buf' = U.accum (\v _ -> if v >= 40 then v else v + 1) (wBuf w) [(i, ()) | i <- hits]
  in w { wBuf = buf', wGen = gen', wCount = wCount w + added }

cellIndex :: Double -> Double -> Int
cellIndex x y =
  let cx = max 0 (min (bufW - 1) (floor ((x + 7.02) / cellU)))
      cy = max 0 (min (bufH - 1) (floor ((8.01 - y) / cellU)))
  in cy * bufW + cx

-- 点の数 → 色。1 点で 18 % ずつ色を重ねる（背景はマス 0 の色）
shadeR, shadeG, shadeB :: U.Vector Word8
shadeR = shade 11 255
shadeG = shade 23 204
shadeB = shade 26 102

shade :: Float -> Float -> U.Vector Word8
shade bg fg = U.generate 41 (\n -> round (lerpF bg fg (1 - 0.82 ^ n)))

amoebaPic :: Env -> World -> Picture
amoebaPic env w = pixelImage bufW bufH (realToFrac cellU * scOf env) $ \x y ->
  let n = fromIntegral (wBuf w U.! (y * bufW + x))
  in (shadeR U.! n, shadeG U.! n, shadeB U.! n)

draw :: Env -> World -> Picture
draw env w = Pictures (
  [ amoebaPic env w
  , seg env 1 (hsba 0 0 100 0.3) (toX env 0, 0) (toX env 0, envH env)
  , seg env 1 (hsba 0 0 100 0.3) (0, toY env 0) (envW env, toY env 0) ]
  ++ (if wTrop w then [ let (a, b) = edgeEnds cv e in seg env 2.5 (hsba 90 90 100 0.85) (us a) (us b) | e <- cvEdges cv ] else [])
  ++ [ txt env 20 28 16 (gray 1) ("Up/Down: base t = " ++ nfd 3 (wT w) ++ "   M: " ++ (if wKind w == 1 then "line 1 + z + w" else "conic t + z + t z^2 + w + z w + t w^2"))
     , txt env 20 50 16 (gray 1) ("T: tropical curve " ++ (if wTrop w then "ON" else "off") ++ "   C: redraw   Points " ++ show (wCount w))
     , txt env 20 74 14 (gray 0.65) "Across = log_t|z|, up = log_t|w|."
     , txt env 20 94 14 (gray 0.65) "|t^a z^i w^j| = t^(a + i x + j y)"
     , txt env 20 114 14 (gray 0.65) "As t -> 0 only the 2 smallest"
     , txt env 20 134 14 (gray 0.65) "exponents survive and cancel:"
     , txt env 20 154 14 (gray 0.65) "the min is attained twice."
     , txt env 20 (envH env - 84) 14 (gray 0.65) "t = 0.9: fat amoeba"
     , txt env 20 (envH env - 64) 14 (gray 0.65) "(width ~ 1/|log t|)"
     , txt env 20 (envH env - 44) 14 (gray 0.65) "t = 0.01: almost a polyline.  Dequantization: times -> plus, plus -> min"
     , txt env 20 (envH env - 24) 14 (gray 0.65) ("Green = tropical curve min(" ++ intercalate ", " (map termStr (V.toList (wTerms w))) ++ "), the amoeba's spine") ] )
  where
    cv = tropCurve (wTerms w)
    us (x, y) = (toX env x, toY env y)
    termStr t = show (round (tA t) :: Int) ++ part (tI t) "x" ++ part (tJ t) "y"
    part :: Int -> String -> String
    part k v | k == 0 = ""
             | k == 1 = " + " ++ v
             | otherwise = " + " ++ show k ++ v

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> clearBuf w { wT = min 0.9 (wT w * 1.5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> clearBuf w { wT = max 0.001 (wT w / 1.5) }
  EventKey (Char ch) Down _ _
    | ch `elem` "mM" -> let k = if wKind w == 1 then 2 else 1
                        in clearBuf w { wKind = k, wTerms = termsOf k }
    | ch `elem` "tT" -> w { wTrop = not (wTrop w) }
    | ch `elem` "cC" -> clearBuf w
  _ -> w

-- ---------- やってみよう ----------
-- 1. t = 0.9 のアメーバの「触手」は 3 本（直線）。方向はトロピカル直線の半直線と同じ？
-- 2. 2 次曲線のアメーバには穴（有界な補集合）がある？ ない？ 係数 a_ij を変えて（termsOf の数字）試そう
-- 3. 係数 a_00 を 3 にすると、トロピカル曲線の頂点はどこへ動く？ アメーバもついてくる？
-- 4. t > 1 にすると（log t > 0）min が max に変わる。onEvent の min 0.9 の上限を外して確かめよう


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
