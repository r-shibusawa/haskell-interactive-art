-- ============================================
-- 58-04 反射 ─ 鏡の法線ベクトル n で v ↦ −n v n⁻¹
-- 幾何代数では「鏡」は法線ベクトル 1 本。反射は掛け算 −n v n⁻¹ だけで計算できる（行列も角度もいらない）
-- 2 枚の鏡で続けて反射すると、鏡のなす角 α の 2 倍の回転：m(n v n⁻¹)m⁻¹ = (mn) v (mn)⁻¹。R = mn がローター
-- 操作：ドラッグ 鏡の法線（紫・水色の丸）、図形（黄色の丸）/ K 万華鏡（2 枚の鏡が生む像を全部）/ 2〜8 α = 180°/k にそろえる / R 元にもどす
-- ============================================
module Main (main) where

import Art
import Data.Bits (popCount, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (foldl', transpose)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Numeric (showEFloat, showFFloat)
import System.Environment (lookupEnv)

type P2 = (Double, Double)

g2 :: Alg
g2 = makeAlgebra [1, 1]

nA0, nB0, fig0 :: P2
nA0 = (1, 0.15)                  -- ★ 2 枚の鏡の法線（長さは何でもよい：n⁻¹ が割ってくれる）
nB0 = (0.55, 0.9)
fig0 = (2.2, 0.9)                -- ★ 図形の位置

unitPx :: Double
unitPx = 90

-- 「F」の形（向きがわかる）と、それを塗るための三角形（F はへこんだ形なので三角形に分けて塗る）
shape :: [P2]
shape = [(0, 0), (0.9, 0), (0.9, 0.22), (0.25, 0.22), (0.25, 0.45), (0.65, 0.45), (0.65, 0.65), (0.25, 0.65), (0.25, 1.1), (0, 1.1)]

shapeTris :: [(Int, Int, Int)]
shapeTris = [(0, 1, 2), (0, 2, 3), (0, 3, 8), (0, 8, 9), (4, 5, 6), (4, 6, 7)]

data World = World
  { wNA, wNB, wFig :: !P2
  , wDrag          :: !Int
  , wKaleido       :: !Bool
  , wTest          :: [(String, Bool)] }

vio, cya, sil, gold :: (Int, Int, Int)
vio = (180, 130, 255)
cya = (90, 220, 240)
sil = (215, 220, 230)
gold = (255, 210, 120)

main :: IO ()
main = runSketch (sketch "58-04 Reflections" world0)
  { skBack  = rgbC 14 14 18
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World nA0 nB0 fig0 (-1) False []

toPx :: Env -> P2 -> P2
toPx env (x, y) = (realToFrac (envW env) * 0.42 + x * unitPx, realToFrac (envH env) * 0.55 - y * unitPx)

v2 :: P2 -> MV
v2 (x, y) = vec g2 [x, y]

p2 :: MV -> P2
p2 m = (m U.! 1, m U.! 2)

shapeVecs :: World -> [MV]
shapeVecs w = let (fx, fy) = wFig w in [ v2 (fx + (px - 0.4) * 0.8, fy + (py - 0.5) * 0.8) | (px, py) <- shape ]

drawShape :: Env -> [MV] -> (Int, Int, Int) -> Int -> Float -> Picture
drawShape env vs col al sw = Pictures
  [ Color (colA col (al * 3 `div` 10)) (Pictures [ Polygon (map pt2 [getAt (0, 0) ps i, getAt (0, 0) ps j, getAt (0, 0) ps k]) | (i, j, k) <- shapeTris ])
  , Color (colA col al) (polyline sw True ps) ]
  where ps = map (toPx env . p2) vs

-- 鏡（法線 n に垂直な直線）と法線の矢印
drawMirror :: Env -> P2 -> (Int, Int, Int) -> [Picture]
drawMirror env (nx, ny) col =
  [ Color (colA col 200) (lineS 3 (toPx env (tx * far, ty * far)) (toPx env (-tx * far, -ty * far)))
  , arrowS (col3 col) 2 (toPx env (0, 0)) (toPx env (nx, ny))
  , Color (col3 col) (discS (toPx env (nx, ny)) 8) ]
  where l = hypot nx ny
        tx = -ny / l
        ty = nx / l
        far = 12

-- 鏡の法線から生まれる群（ベルソルの積）を幅優先で集める。上限 cap 個
generate :: [P2] -> Int -> ([MV], Bool)
generate normals cap = go [scalar g2 1] [[1, 0, 0, 1]] [scalar g2 1]
  where
    gens = map (unit g2 . v2) normals
    go list mats queue = case queue of
      (v : rest) | length list < cap ->
        let (list', mats', added, stop) = foldl' (tryGen v) (list, mats, [], False) gens
        in if stop then (list', null (rest ++ added)) else go list' mats' (rest ++ added)
      _ -> (list, null queue)
    tryGen v (list, mats, added, stop) g
      | stop = (list, mats, added, stop)
      | any (\q -> and (zipWith (\x y -> abs (x - y) < 1e-6) q m)) mats = (list, mats, added, False)   -- ±W は同じ変換
      | otherwise = let list' = list ++ [ww] in (list', mats ++ [m], added ++ [ww], length list' >= cap)
      where ww = gp g2 g v
            (e1x, e1y) = p2 (applyVersor g2 ww (blade g2 1 1))
            (e2x, e2y) = p2 (applyVersor g2 ww (blade g2 2 1))
            m = [e1x, e2x, e1y, e2y]

draw :: Env -> World -> Picture
draw env w = view env $
  [ drawShape env (map (applyVersor g2 vv) vs) (if isOdd g2 vv then cya else vio) 90 1.5 | Just (lst, _) <- [grp], vv <- drop 1 lst ]
  ++ drawMirror env (wNA w) vio ++ drawMirror env (wNB w) cya
  ++ (if not (wKaleido w)
        then [ drawShape env img1 vio 160 2, drawShape env img2 (255, 160, 220) 220 2.5
             -- 回転行列で 2α 回した図形（点線）→ 2 回反射とぴったり重なる
             , let pts = [ toPx env (t2 (matVec (rotMat2 (2 * alpha)) [fst (p2 v), snd (p2 v)])) | v <- vs ]
               in Color white (Pictures [ dashed 1 4 5 a b | (a, b) <- zip pts (drop 1 pts ++ take 1 pts) ]) ]
        else [])
  ++ [ drawShape env vs gold 255 2.5, Color (col3 gold) (discS (toPx env (wFig w)) 7) ]
  -- 文字
  ++ [ say ("drag: mirror normals, the shape   K: kaleidoscope " ++ (if wKaleido w then "ON" else "off") ++ "   2-8: alpha = 180/k deg   R: reset") 20 34 18 (col3 sil) ]
  ++ [ say s 20 (64 + 25 * fromIntegral k) 16 (col3 c) | (k, (s, c)) <- zip [0 :: Int ..] rows ]
  ++ [ drawSelfTest env (wTest w) ]
  where
    t2 xs = (getAt 0 xs 0, getAt 0 xs 1)
    n = v2 (wNA w)
    m = v2 (wNB w)
    vs = shapeVecs w
    (ax, ay) = wNA w
    (bx, by) = wNB w
    alpha = atan2 (ax * by - ay * bx) (ax * bx + ay * by)     -- n から m への角
    rotor = gp g2 m n
    ru = smul rotor (1 / norm g2 rotor)
    grp = if wKaleido w then Just (generate [wNA w, wNB w] 60) else Nothing
    img1 = map (reflect g2 n) vs
    img2 = map (reflect g2 m) img1
    deg x = x * 180 / pi
    rows =
      [ ("n = " ++ str g2 n 2 ++ "   m = " ++ str g2 m 2 ++ "   angle between the mirrors alpha = " ++ fmt 2 (deg alpha) ++ " deg", sil)
      , ("1st (violet): v -> -n v n^-1     2nd (pink): v -> -m(-n v n^-1)m^-1 = (mn) v (mn)^-1", vio)
      , ("rotor R = mn = " ++ str g2 rotor 3 ++ "  -> as a unit " ++ str g2 ru 3 ++ " = cos(alpha) - I sin(alpha)", (255, 160, 220))
      , ("rotation angle = 2 alpha = " ++ fmt 2 (deg (2 * alpha)) ++ " deg  (white dots = turned by 2 alpha with a matrix; it lies on pink)", sil) ]
      ++ case grp of
           Just (lst, closed) ->
             let k = pi / abs alpha
                 isK = abs (k - fromIntegral (round k :: Integer)) < 1e-6
             in [ ("kaleidoscope: " ++ show (length lst) ++ (if closed then " images (closed! dihedral group D" ++ show (length lst `div` 2) ++ ", violet = rotations, cyan = reflections)"
                                                              else "+ images -> not closed (unless alpha is 180/k deg the images keep growing)")
                  , if closed then gold else (255, 120, 120)) ]
                ++ [ ("hint: keys 2-8 set alpha to 180/k deg and the pattern closes", (150, 150, 150)) | not isK ]
           Nothing -> []

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = evP env pos
        pick (best, k) (i, p) = let (sx, sy) = toPx env p; d = hypot (mx - sx) (my - sy) in if d < best then (d, i) else (best, k)
    in w { wDrag = snd (foldl' pick (26, -1) (zip [0 ..] [wNA w, wNB w, wFig w])) }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion pos | wDrag w >= 0 ->
    let (mx, my) = evP env pos
        p = (clampv (-4) 4 ((mx - realToFrac (envW env) * 0.42) / unitPx), clampv (-4) 4 ((realToFrac (envH env) * 0.55 - my) / unitPx))
    in if wDrag w < 2 && uncurry hypot p < 0.3 then w           -- 法線が 0 になると鏡が決まらない
       else case wDrag w of
              0 -> w { wNA = p }
              1 -> w { wNB = p }
              _ -> w { wFig = p }
  EventKey (Char c) Down _ _
    | c `elem` "kK" -> w { wKaleido = not (wKaleido w) }
    | c >= '2' && c <= '8' ->
        let k = fromIntegral (fromEnum c - fromEnum '0')
            a = uncurry (flip atan2) (wNA w) + pi / k
            l = uncurry hypot (wNB w)
        in w { wNB = (l * cos a, l * sin a) }
    | c `elem` "rR" -> world0 { wTest = wTest w }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 7 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest = evalRng 4 $ do
  rs <- mapM (\_ -> do
          n <- do v <- randVec g2; r <- nextR; pure (smul v (1 + 3 * r))
          m <- do v <- randVec g2; r <- nextR; pure (smul v (0.5 + r))
          v <- randVec g2
          let [nx, ny] = vecOf g2 n
              vv@[vx, vy] = vecOf g2 v
              r = vecOf g2 (reflect g2 n v)
              nn = nx * nx + ny * ny
              d = (vx * nx + vy * ny) / nn
              ok1 = nearArr r [vx - 2 * d * nx, vy - 2 * d * ny] 1e-12     -- ハウスホルダーの式
              ok2 = nearM (reflect g2 n (reflect g2 n v)) v 1e-12 && near (lenv r) (lenv vv) 1e-12
              [mx, my] = vecOf g2 m
              al = atan2 (nx * my - ny * mx) (nx * mx + ny * my)
              twice = vecOf g2 (reflect g2 m (reflect g2 n v))
              viaR = vecOf g2 (applyVersor g2 (gp g2 m n) v)
              ok3 = nearArr twice (matVec (rotMat2 (2 * al)) vv) 1e-12 && nearArr viaR twice 1e-12
              ru = unit g2 (gp g2 m n)
              ok4 = nearM (gp g2 ru (rev g2 ru)) (scalar g2 1) 1e-12 && nearM (rev g2 (gp g2 m n)) (gp g2 n m) 1e-12
              perp = vec g2 [-ny, nx]
              ok5 = nearM (reflect g2 n perp) perp 1e-12 && nearM (reflect g2 n n) (smul n (-1)) 1e-12
          pure [ok1, ok2, ok3, ok4, ok5]) [1 .. 40 :: Int]
  let col k = all (\r -> getAt False r k) rs
      okK = and [ length (fst (generate [(1, 0), (cos (pi / fromIntegral k), sin (pi / fromIntegral k))] 100)) == 2 * k | k <- [2 .. 8 :: Int] ]
  pure
    [ ("-n v n^-1 = v - 2(v.n)n/|n|^2 (Householder formula; n need not be a unit)", col 0)
    , ("reflecting twice in the same mirror gives v back; lengths do not change", col 1)
    , ("2 reflections = rotation matrix of 2 alpha = (mn) v (mn)^-1", col 2)
    , ("R = mn made a unit has R R~ = 1, R~ = nm (the opposite rotation)", col 3)
    , ("vectors on the mirror stay, the normal becomes -n", col 4)
    , ("alpha = 180/k deg gives 2k images (dihedral group D_k, k = 2-8)", okK)
    , ("alpha = 1 rad (not 180/k deg) does not close even past 60 images", not (snd (generate [(1, 0), (cos 1, sin 1)] 60))) ]

-- ---------- やってみよう ----------
-- 1. 法線を回して、ピンクの図形（2 回反射）が白い点線（回転行列で 2α）といつも重なることを確かめよう
-- 2. 2 枚の鏡を平行に近づける（α → 0）と回転はどうなる？ 直角（α = 90°）なら？
-- 3. 紫の図形（1 回反射）は「F」が裏返っている。2 回反射すると裏返りはどうなる？
-- 4. K で万華鏡。3 を押すと像は何個？ 法線を少しずらすと何が起きる？
-- 5. n の長さを変えても反射の結果は変わらない。なぜ？（n⁻¹ = n/|n|²）

-- ==================================================
-- ここから下は「幾何代数の道具」（中身は読まなくても OK。消さないでね）
--   マルチベクトル = 2ⁿ 個の Double の配列（unboxed vector）。基底ブレードは「ビットマスク」で番号をつける
--   例（n = 3）：0 = 1、1 = e1、2 = e2、3 = e12、4 = e3、5 = e13、6 = e23、7 = e123（e31 = −e13）
--   幾何積：ブレード a と b を並べかえる回数（入れかえ 1 回で −1）× 同じ e_i が 2 つ並んだら e_i² を掛ける
--   計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
type MV = U.Vector Double

-- 代数：次元 n、基底の数 2ⁿ、各 e_i の 2 乗、次数、掛け算表（符号）、基底の名前
data Alg = Alg
  { aN      :: !Int
  , aNb     :: !Int
  , aMetric :: [Double]
  , aGrade  :: !(U.Vector Int)
  , aTbl    :: !(U.Vector Double)
  , aNames  :: [String] }

-- 基底 a のうしろに基底 b を並べ、標準の順（番号の小さい順）にするまでの入れかえ回数 → 奇数なら −1
reorderSign :: Int -> Int -> Double
reorderSign a0 b = go (a0 `shiftR` 1) 0
  where go 0 s = if odd s then -1 else 1
        go a s = go (a `shiftR` 1) (s + popCount (a .&. b))

-- 代数をつくる：metric = 各 e_i の 2 乗（+1 か −1）。[1,1,1] = G(3)、[-1,-1] = Cl(0,2)、[1,-1,-1,-1] = 時空の代数
makeAlgebra :: [Double] -> Alg
makeAlgebra metric = Alg n nb metric grade tbl names
  where
    n = length metric
    nb = 1 `shiftL` n
    grade = U.generate nb popCount
    tbl = U.generate (nb * nb) $ \k ->
      let (a, b) = k `divMod` nb
          c = a .&. b
      in foldl' (\s (i, m) -> if testBit c i then s * m else s) (reorderSign a b) (zip [0 ..] metric)   -- 同じ e_i どうし → e_i² = ±1
    names = [ if a == 0 then "1" else 'e' : concat [ show (i + 1) | i <- [0 .. n - 1], testBit a i ] | a <- [0 .. nb - 1] ]

-- 基底の名前を取りかえる
withNames :: [String] -> Alg -> Alg
withNames ns al = al { aNames = ns }

nameOf :: Alg -> Int -> String
nameOf al i = getAt "?" (aNames al) i

zeroM :: Alg -> MV
zeroM al = U.replicate (aNb al) 0

scalar :: Alg -> Double -> MV
scalar al s = zeroM al U.// [(0, s)]

-- 基底ブレード（係数 c）
blade :: Alg -> Int -> Double -> MV
blade al mask c = zeroM al U.// [(mask, c)]

-- ベクトル（足りない成分は 0）
vec :: Alg -> [Double] -> MV
vec al vs = zeroM al U.// [ (1 `shiftL` i, v) | (i, v) <- zip [0 .. aN al - 1] vs ]

vecOf :: Alg -> MV -> [Double]
vecOf al m = [ m U.! (1 `shiftL` i) | i <- [0 .. aN al - 1] ]

addM, subM :: MV -> MV -> MV
addM = U.zipWith (+)
subM = U.zipWith (-)

-- いくつも足す
addMs :: Alg -> [MV] -> MV
addMs al = foldl' addM (zeroM al)

smul :: MV -> Double -> MV
smul a s = U.map (* s) a

-- 積の共通部分：kind 0 = 幾何積、1 = 外積 ∧（共通の e_i がないときだけ）、2 = 左縮約 ⌋（a のブレードが b に含まれるときだけ）
prodK :: Alg -> Int -> MV -> MV -> MV
prodK al kind a b = U.accum (+) (zeroM al)
  [ (i `xor` j, U.unsafeIndex tbl (i * nb + j) * x * y)
  | i <- [0 .. nb - 1], let x = U.unsafeIndex a i, x /= 0
  , j <- [0 .. nb - 1], let y = U.unsafeIndex b j, y /= 0
  , kind /= 1 || i .&. j == 0
  , kind /= 2 || i .&. j == i ]
  where nb = aNb al
        tbl = aTbl al

gp, wedge, lc :: Alg -> MV -> MV -> MV
gp al = prodK al 0
wedge al = prodK al 1
lc al = prodK al 2

-- 3 つ以上の積（左から順に）
gps, wedges :: Alg -> [MV] -> MV
gps al ms = case ms of
  (m : rest) -> foldl' (gp al) m rest
  [] -> scalar al 1
wedges al ms = case ms of
  (m : rest) -> foldl' (wedge al) m rest
  [] -> scalar al 1

-- ベクトルどうしなら a·b
inner :: Alg -> MV -> MV -> MV
inner al a b = gradePart al (gp al a b) 0

-- スカラー部分
sp :: Alg -> MV -> MV -> Double
sp al a b = gp al a b U.! 0

gradePart :: Alg -> MV -> Int -> MV
gradePart al a k = U.imap (\i x -> if aGrade al U.! i == k then x else 0) a

gradeSize :: Alg -> MV -> Int -> Double
gradeSize al a k = sqrt (U.ifoldl' (\s i x -> if aGrade al U.! i == k then s + x * x else s) 0 a)

-- 逆順 ã（掛ける順を逆に）
rev :: Alg -> MV -> MV
rev al = U.imap (\i x -> let g = aGrade al U.! i in if odd ((g * (g - 1)) `div` 2) then -x else x)

-- 次数の反転（奇数次に −）
ginv :: Alg -> MV -> MV
ginv al = U.imap (\i x -> if odd (aGrade al U.! i) then -x else x)

normSq, norm :: Alg -> MV -> Double
normSq al a = gp al a (rev al a) U.! 0
norm al a = sqrt (abs (normSq al a))

unit :: Alg -> MV -> MV
unit al a = let n = norm al a in smul a (1 / (if n == 0 || isNaN n then 1 else n))

-- 逆元（ベクトル・ブレード・ベルソル用：a ã がスカラー）
inv :: Alg -> MV -> MV
inv al a = let r = rev al a in smul r (1 / (gp al a r U.! 0))

-- 擬スカラー（一番上のブレード）
pss :: Alg -> MV
pss al = blade al (aNb al - 1) 1

-- 双対 a* = a I⁻¹
dualOf, undual :: Alg -> MV -> MV
dualOf al a = gp al a (inv al (pss al))
undual al a = gp al a (pss al)

-- 交わり（向きと大きさは決めかた次第）
meet :: Alg -> MV -> MV -> MV
meet al a b = undual al (wedge al (dualOf al a) (dualOf al b))

-- R x R̃（回転）
sandwich :: Alg -> MV -> MV -> MV
sandwich al r x = gps al [r, x, rev al r]

-- −n x n⁻¹（法線 n の鏡で反射）
reflect :: Alg -> MV -> MV -> MV
reflect al nv x = smul (gps al [nv, x, inv al nv]) (-1)

isOdd :: Alg -> MV -> Bool
isOdd al v = o > e
  where (o, e) = U.ifoldl' (\(so, se) i x -> if odd (aGrade al U.! i) then (so + x * x, se) else (so, se + x * x)) (0, 0) v

-- ±V x V⁻¹（鏡 k 枚の積なら符号 (−1)^k）
applyVersor :: Alg -> MV -> MV -> MV
applyVersor al v x = smul (gps al [v, x, inv al v]) (if isOdd al v then -1 else 1)

-- 二重ベクトル B の指数関数 e^B：B² がスカラーなら閉じた式、そうでなければ級数（割って 2 乗をくり返す）
expB :: Alg -> MV -> Bool -> MV
expB al b forceSeries
  | not forceSeries && rest < 1e-12 =
      let s = U.head b2
          (c, k) | s < -1e-15 = let t = sqrt (-s) in (cos t, sin t / t)
                 | s > 1e-15 = let t = sqrt s in (cosh t, sinh t / t)
                 | otherwise = (1, 1)
          r = smul b k
      in r U.// [(0, U.head r + c)]
  | otherwise =
      let sz0 = U.foldl' (\m x -> max m (abs x)) 0 b
          halve (sz, q) = if sz > 0.25 && q < 40 then halve (sz / 2, q + 1) else q
          sq = halve (sz0, 0 :: Int)
          x = smul b (1 / (2 ^^ sq))
          series = snd (foldl' (\(term, sm) k -> let t' = smul (gp al term x) (1 / fromIntegral k) in (t', addM sm t')) (scalar al 1, scalar al 1) [1 .. 29 :: Int])
      in iterate (\m -> gp al m m) series !! sq
  where
    b2 = gp al b b
    rest = U.sum (U.map abs (U.drop 1 b2))

near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 2 つのマルチベクトルが同じか（tol の中）
nearM :: MV -> MV -> Double -> Bool
nearM a b tol = U.length a == U.length b && U.and (U.zipWith (\x y -> abs (x - y) <= tol) a b)

isZeroM :: MV -> Double -> Bool
isZeroM a tol = U.all (\x -> abs x <= tol) a

-- ランダムなマルチベクトル・ベクトル（成分は −1〜1）
randM, randVec :: Alg -> R MV
randM al = U.fromList <$> mapM (\_ -> (\r -> r * 2 - 1) <$> nextR) [1 .. aNb al]
randVec al = vec al <$> mapM (\_ -> (\r -> r * 2 - 1) <$> nextR) [1 .. aN al]

-- 文字にする：「1.20 + 0.50 e12」（記号は ASCII）
str :: Alg -> MV -> Int -> String
str al a d = if null s then "0" else s
  where
    s = foldl' piece "" (zip [0 ..] (U.toList a))
    piece acc (i, x)
      | abs x < 10 ^^ negate d / 2 = acc
      | otherwise =
          let v = fmt d (abs x)
              sg = if null acc then (if x < 0 then "-" else "") else (if x < 0 then " - " else " + ")
              body = if i > 0 then (if v == fmt d 1 then "" else v ++ " ") ++ nameOf al i else v
          in acc ++ sg ++ body

-- ---- 比べるための「ふつうの」計算（行列・外積・行列式）。ベクトルは [x, y, z] のリスト
dotv :: [Double] -> [Double] -> Double
dotv a b = foldl' (+) 0 (zipWith (*) a b)

crossv :: [Double] -> [Double] -> [Double]
crossv a b = [a1 * b2 - a2 * b1, a2 * b0 - a0 * b2, a0 * b1 - a1 * b0]
  where [a0, a1, a2] = take 3 (a ++ repeat 0)
        [b0, b1, b2] = take 3 (b ++ repeat 0)

lenv :: [Double] -> Double
lenv a = sqrt (dotv a a)

det3 :: [Double] -> [Double] -> [Double] -> Double
det3 a b c = dotv a (crossv b c)

matVec :: [[Double]] -> [Double] -> [Double]
matVec m v = map (\r -> foldl' (+) 0 (zipWith (*) r v)) m

matMul :: [[Double]] -> [[Double]] -> [[Double]]
matMul p q = [ [ foldl' (+) 0 (zipWith (*) r col) | col <- transpose q ] | r <- p ]

rotMat2 :: Double -> [[Double]]
rotMat2 t = [[cos t, -(sin t)], [sin t, cos t]]

-- 軸 ax（単位ベクトル）まわりに角 t 回す行列
rodrigues :: [Double] -> Double -> [[Double]]
rodrigues ax t =
  [ [c + x * x * k, x * y * k - z * s, x * z * k + y * s]
  , [y * x * k + z * s, c + y * y * k, y * z * k - x * s]
  , [z * x * k - y * s, z * y * k + x * s, c + z * z * k] ]
  where [x, y, z] = take 3 (ax ++ repeat 0)
        c = cos t
        s = sin t
        k = 1 - c

nearArr :: [Double] -> [Double] -> Double -> Bool
nearArr a b tol = length a == length b && and (zipWith (\x y -> abs (x - y) <= tol) a b)

-- ---- 種つきの乱数（自己テストで毎回同じ数を使う。p5.js 版の rnd と同じ mulberry32）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

nextRng :: Rng -> (Double, Rng)
nextRng (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に使う計算（状態を手で受けわたさなくてよいように）
newtype R a = R (Rng -> (a, Rng))

instance Functor R where fmap f (R g) = R (\s -> let (a, s') = g s in (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))
instance Monad R where R g >>= k = R (\s -> let (a, s1) = g s; R h = k a in h s1)

nextR :: R Double
nextR = R nextRng

runRng :: R a -> Rng -> (a, Rng)
runRng (R g) = g

evalRng :: Word32 -> R a -> a
evalRng sd m = fst (runRng m (seedRng sd))

-- ---- 数の道具
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- JavaScript の % と同じ（符号は割られる数と同じ）
jsMod :: Double -> Double -> Double
jsMod a m = a - m * fromIntegral (truncate (a / m) :: Integer)

-- Math.round と同じ（.5 は大きい方へ）
jsRound :: Double -> Double
jsRound v = if isNaN v || isInfinite v then 0 else fromIntegral (floor (v + 0.5) :: Integer)

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . clampv (-1e6) 1e6 . fin 0

hypot :: Double -> Double -> Double
hypot a b = sqrt (a * a + b * b)

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 1.2e-5 のような書き方（toExponential(1)）
fmtE :: Double -> String
fmtE v = showEFloat (Just 1) (fin 0 v) ""

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点、y は下向き）で
mouseP :: Env -> (Double, Double)
mouseP env = let (mx, my) = envMouse env in (realToFrac (mx + envW env / 2), realToFrac (envH env / 2 - my))

-- gloss のイベントの位置 → p5.js の座標
evP :: Env -> (Float, Float) -> (Double, Double)
evP env (mx, my) = (realToFrac (mx + envW env / 2), realToFrac (envH env / 2 - my))

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC r g b a = makeColorI r g b (max 0 (min 255 a))

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- [r, g, b] の色（透明度 a）
colA :: (Int, Int, Int) -> Int -> Color
colA (r, g, b) a = rgbaC r g b a

col3 :: (Int, Int, Int) -> Color
col3 (r, g, b) = rgbC r g b

-- 左下が (x, y) の文字
say :: String -> Double -> Double -> Float -> Color -> Picture
say s x y size c = Translate (f2 x) (f2 y) (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Double -> Double -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

textW :: String -> Float -> Double
textW s size = fromIntegral (length s) * realToFrac size * 0.72

pt2 :: (Double, Double) -> (Float, Float)
pt2 (x, y) = (f2 x, f2 y)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Double -> Double -> Double -> Double -> Picture
rectTL x y w h = Polygon [pt2 (x, y), pt2 (x + w, y), pt2 (x + w, y + h), pt2 (x, y + h)]

-- 線（太さ wt）
lineS :: Float -> (Double, Double) -> (Double, Double) -> Picture
lineS wt a b = if wt <= 1.2 then Line [pt2 a, pt2 b] else thickLine wt (pt2 a) (pt2 b)

-- 点線（dash：線の長さ、gap：すき間）
dashed :: Float -> Double -> Double -> (Double, Double) -> (Double, Double) -> Picture
dashed wt dash gap (x1, y1) (x2, y2) = Pictures
  [ lineS wt (x1 + ux * s, y1 + uy * s) (x1 + ux * min l (s + dash), y1 + uy * min l (s + dash)) | s <- takeWhile (< l) [0, dash + gap ..] ]
  where l = min 5000 (fin 0 (hypot (x2 - x1) (y2 - y1)))
        ux = if l > 0 then (x2 - x1) / l else 0
        uy = if l > 0 then (y2 - y1) / l else 0

-- 点が 2 つ以上あるときだけ折れ線を描く（closed なら閉じる）
polyline :: Float -> Bool -> [(Double, Double)] -> Picture
polyline wt closed pts = case pts of
  (p0 : _ : _) -> let ps = map pt2 (if closed then pts ++ [p0] else pts) in if wt <= 1.2 then Line ps else thickPath wt ps
  _ -> Blank

-- 閉じた形を塗る（へこんだ形でも大丈夫なように、重心からの三角形で塗る）
fillPoly :: [(Double, Double)] -> Picture
fillPoly pts = case pts of
  (_ : _ : _ : _) -> Pictures [ Polygon [pt2 c, pt2 a, pt2 b] | (a, b) <- zip pts (drop 1 pts ++ take 1 pts) ]
  _ -> Blank
  where n = fromIntegral (length pts)
        c = (sum (map fst pts) / n, sum (map snd pts) / n)

-- 最初の点から扇のように塗る（beginShape の扇形・星形の形）
fanFill :: [(Double, Double)] -> Picture
fanFill pts = case pts of
  (p0 : rest@(_ : _ : _)) -> Pictures [ Polygon [pt2 p0, pt2 a, pt2 b] | (a, b) <- zip rest (drop 1 rest) ]
  _ -> Blank

-- 円（中心・半径）の枠と塗り
circleS :: Float -> (Double, Double) -> Double -> Picture
circleS wt (x, y) r = Translate (f2 x) (f2 y) (if wt <= 1.2 then Circle (f2 r) else ThickCircle (f2 r) wt)

discS :: (Double, Double) -> Double -> Picture
discS (x, y) r = Translate (f2 x) (f2 y) (circleSolid (f2 r))

-- 矢印（画面の座標）：線の太さ sw
arrowS :: Color -> Float -> (Double, Double) -> (Double, Double) -> Picture
arrowS c sw (x1, y1) (x2, y2) = Color c $ Pictures $
  lineS sw (x1, y1) (x2, y2) :
  [ Polygon [pt2 (x2, y2), pt2 (x2 - h * cos (a - 0.4), y2 - h * sin (a - 0.4)), pt2 (x2 - h * cos (a + 0.4), y2 - h * sin (a + 0.4))]
  | hypot (x2 - x1) (y2 - y1) >= 2 ]
  where a = atan2 (y2 - y1) (x2 - x1)
        h = 8 + realToFrac sw * 2

-- ---- 自己テストの道具
-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (realToFrac (envW env) - 200) (realToFrac (envH env) - 12) 14 (if all snd rs then rgbC 150 220 200 else rgbC 255 100 110)
