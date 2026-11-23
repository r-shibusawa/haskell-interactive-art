-- ============================================
-- 58-12 共形幾何代数 ─ 平面の点を 4 次元の Cl(3,1)（16 成分）に持ち上げると、円も直線も「外積 1 つ」
-- e₊² = +1、e₋² = −1 から e∞ = e₋ + e₊、e0 = (e₋ − e₊)/2 を作る（どちらも 2 乗が 0）。点 x → up(x) = x + ½|x|² e∞ + e0
-- 3 点を通る円 = P1∧P2∧P3、2 点を通る直線 = P∧Q∧e∞（無限遠を通る円）。円と直線の交わり = 2 点の組（点対）
-- 操作：ドラッグ 白い点（円）・水色の点（直線）・黄色の点（4 点目）/ C 中心と半径の比較 / スペース 自動 / R 元にもどす
-- 画面の基底の名前は ASCII：e₊ → ep、e₋ → em（例：e12p = e1 e2 e₊）
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

-- e1, e2, e₊, e₋
cga :: Alg
cga = withNames [ if m == 0 then "1" else 'e' : concat [ getAt "" ["1", "2", "p", "m"] i | i <- [0 .. 3], testBit m i ] | m <- [0 .. 15 :: Int] ] (makeAlgebra [1, 1, 1, -1])

einf, eo :: MV
einf = addM (blade cga 4 1) (blade cga 8 1)
eo = smul (subM (blade cga 8 1) (blade cga 4 1)) 0.5

unitPx :: Double
unitPx = 90

data World = World
  { wCps    :: [P2]              -- 円を決める 3 点
  , wLps    :: [P2]              -- 直線を決める 2 点
  , wP4     :: !P2
  , wCmp    :: !Bool
  , wAuto   :: !Bool
  , wT      :: !Double
  , wDrag   :: !Int              -- 0〜2 円の点、3〜4 直線の点、5 4 点目、−1 なし
  , wTest   :: [(String, Bool)] }

vio, cya, sil, gold :: (Int, Int, Int)
vio = (180, 130, 255)
cya = (90, 220, 240)
sil = (215, 220, 230)
gold = (255, 210, 120)

main :: IO ()
main = runSketch (sketch "58-12 Conformal geometric algebra" world0)
  { skBack  = rgbC 14 14 18
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World [(-2, -0.5), (1.5, 1.6), (2, -1.2)]   -- ★ 円を決める 3 点
               [(-3.5, -2), (3.5, 0.8)]              -- ★ 直線を決める 2 点
               (0.2, 2.2) True False 0 (-1) []

step :: World -> World
step w
  | wAuto w = let t = wT w + 0.01
                  ls = wLps w
                  (ax, _) = getAt (0, 0) ls 0
                  (bx, _) = getAt (0, 0) ls 1
              in w { wT = t, wLps = [(ax, -2 + 1.6 * sin t), (bx, 0.8 + 1.6 * cos (t * 0.7))] }
  | otherwise = w

up :: P2 -> MV
up (x, y) = addMs cga [vec cga [x, y, 0, 0], smul einf (0.5 * (x * x + y * y)), eo]

-- 点の重みでわって平面へ
down :: MV -> P2
down p = let w = -(inner cga p einf U.! 0) in (p U.! 1 / w, p U.! 2 / w)

-- 円（3 次の元）→ 中心と半径。双対 σ = C* を −σ·e∞ = 1 にそろえると σ = up(c) − ½r²e∞、σ² = r²
data CircleInfo = IsLine P2 Double | IsCircle P2 Double

circleOf :: MV -> CircleInfo
circleOf c
  | not (abs w > 1e-9 * norm cga s) = IsLine (s U.! 1, s U.! 2) (s U.! 4 + s U.! 8)   -- 直線（中心が無限遠）
  | otherwise = let s' = smul s (1 / w) in IsCircle (s' U.! 1, s' U.! 2) (gp cga s' s' U.! 0)
  where s = dualOf cga c
        w = -(inner cga s einf U.! 0)

-- 点対 T（2 次の元）→ 2 つの点。T² < 0 なら交わらない
pairOf :: MV -> Maybe (P2, P2)
pairOf t
  | t2 < -1e-9 = Nothing
  | otherwise = Just (pick 1, pick (-1))
  where t2 = gp cga t t U.! 0
        rt = sqrt (max 0 t2)
        ei = lc cga einf t
        pick s = down (gp cga (addM t (scalar cga (s * rt))) ei)

-- くらべる：ふつうの解析幾何（外心の公式、直線と円の交点の 2 次方程式）
circumcenter :: P2 -> P2 -> P2 -> (Double, Double, Double)
circumcenter (a0, a1) (b0, b1) (c0, c1) = (ux, uy, hypot (a0 - ux) (a1 - uy))
  where d = 2 * (a0 * (b1 - c1) + b0 * (c1 - a1) + c0 * (a1 - b1))
        s (x, y) = x * x + y * y
        sa = s (a0, a1); sb = s (b0, b1); sc = s (c0, c1)
        ux = (sa * (b1 - c1) + sb * (c1 - a1) + sc * (a1 - b1)) / d
        uy = (sa * (c0 - b0) + sb * (a0 - c0) + sc * (b0 - a0)) / d

lineCircle :: P2 -> P2 -> Double -> Double -> Double -> Maybe (P2, P2)
lineCircle (p0, p1) (q0, q1) cx cy r
  | dd < 0 = Nothing
  | otherwise = Just (pt (-1), pt 1)
  where dx = q0 - p0; dy = q1 - p1; fx = p0 - cx; fy = p1 - cy
        a = dx * dx + dy * dy
        b = 2 * (fx * dx + fy * dy)
        c = fx * fx + fy * fy - r * r
        dd = b * b - 4 * a * c
        pt s = let t = (-b + s * sqrt dd) / (2 * a) in (p0 + t * dx, p1 + t * dy)

toPx :: Env -> P2 -> P2
toPx env (x, y) = (realToFrac (envW env) * 0.42 + x * unitPx, realToFrac (envH env) * 0.56 - y * unitPx)

p3 :: [P2] -> (P2, P2, P2)
p3 ps = (getAt (0, 0) ps 0, getAt (0, 0) ps 1, getAt (0, 0) ps 2)

showP :: Int -> P2 -> String
showP d (x, y) = "(" ++ fmt d x ++ ", " ++ fmt d y ++ ")"

draw :: Env -> World -> Picture
draw env w = view env $
  [ Color (grayC 30) (Pictures (concat [ [lineS 1 (tp (k, -6)) (tp (k, 6)), lineS 1 (tp (-8, k)) (tp (8, k))] | k <- [-8 .. 8] ])) ]
  -- 円（または 3 点が一直線なら直線）
  ++ (case ci of
        IsCircle c r2 | r2 > 0 && r2 < 400 -> [Color (col3 vio) (circleS 3 (tp c) (sqrt r2 * unitPx)), Color (col3 vio) (discS (tp c) 4.5)]
        IsLine _ _ -> let (c0x, c0y) = c0; (c1x, c1y) = c1; u = (c1x - c0x, c1y - c0y)
                      in [Color (col3 vio) (lineS 3 (tp (c0x - 9 * fst u, c0y - 9 * snd u)) (tp (c0x + 9 * fst u, c0y + 9 * snd u)))]
        _ -> [])
  -- 直線 P∧Q∧e∞
  ++ [ let (ax, ay) = l0; (bx, by) = l1; d = (bx - ax, by - ay)
       in Color (col3 cya) (lineS 2.5 (tp (ax - 9 * fst d, ay - 9 * snd d)) (tp (ax + 9 * fst d, ay + 9 * snd d))) ]
  -- 交わり（点対）
  ++ [ Color (col3 gold) (circleS 3 (tp p) 11) | Just (pa, pb) <- [pts], p <- [pa, pb] ]
  ++ [ Color white (discS (tp p) 7.5) | p <- wCps w ]
  ++ [ Color (col3 cya) (discS (tp p) 7.5) | p <- wLps w ]
  ++ [ Color (if dev < 0.02 then col3 gold else rgbC 140 140 150) (discS (tp (wP4 w)) 7.5) ]
  -- 文字
  ++ [ say ("drag: white (3 circle points), cyan (2 line points), yellow (4th point)   C compare " ++ onOff (wCmp w) ++ "   Space auto   R reset") 20 34 17 (col3 sil) ]
  ++ [ say s 20 (62 + 23 * fromIntegral k) 15 (col3 c) | (k, (s, c)) <- zip [0 :: Int ..] rows ]
  ++ [ say "3D version is Cl(4,1) (32 components): 4 points wedge to a sphere, sphere meet plane is a circle... same formulas" 20 (h - 40) 14 (grayC 150)
     , drawSelfTest env (wTest w) ]
  where
    tp = toPx env
    h = realToFrac (envH env) :: Double
    (c0, c1, c2) = p3 (wCps w)
    l0 = getAt (0, 0) (wLps w) 0
    l1 = getAt (0, 0) (wLps w) 1
    cir = wedges cga (map up [c0, c1, c2])
    lin = wedges cga [up l0, up l1, einf]
    ci = circleOf cir
    t = meet cga cir lin
    pts = case ci of
      IsLine _ _ -> Nothing
      _ -> pairOf t
    on4 = wedge cga (up (wP4 w)) cir
    dev = norm cga on4 / max 1e-9 (norm cga cir)
    (ccx, ccy, ccr) = circumcenter c0 c1 c2
    dcir = dualOf cga cir
    rows =
      [ ("up(x) = x + 1/2|x|^2 einf + e0     up(x).up(y) = -1/2|x - y|^2 (inner product = distance)     P^Q of 2 points = point pair", sil)
      , ("circle C = P1^P2^P3 = " ++ str cga cir 2, vio) ]
      ++ (case ci of
            IsLine _ _ -> [("3 points on a line: the dual sigma has sigma.einf = 0 -> centre at infinity = a line", (255, 120, 120))]
            IsCircle c r2 ->
              ("dual of C: sigma = " ++ str cga (smul dcir (-1 / (inner cga dcir einf U.! 0))) 3 ++ "  -> centre " ++ showP 4 c ++ ", radius sqrt(sigma^2) = " ++ fmt 4 (sqrt (max 0 r2)), vio)
              : [ ("circumcentre formula (analytic geometry): centre " ++ showP 4 (ccx, ccy) ++ ", radius " ++ fmt 4 ccr ++ "  -> the same", sil) | wCmp w ])
      ++ [ ("line L = P^Q^einf = " ++ str cga lin 2, cya) ]
      ++ (case ci of
            IsLine _ _ -> []
            _ -> case pts of
              Just (pa, pb) -> ("meet C v L = point pair -> " ++ showP 3 pa ++ " and " ++ showP 3 pb, gold)
                               : [ ("intersection from the quadratic equation: " ++ showP 3 qa ++ " and " ++ showP 3 qb, sil) | wCmp w, Just (qa, qb) <- [lineCircle l0 l1 ccx ccy ccr] ]
              Nothing -> [("point pair T with T^2 < 0 -> the circle and the line do not meet (imaginary pair)", (255, 120, 120))])
      ++ [ ("4th point: |P4^C| / |C| = " ++ fmt 4 dev ++ (if dev < 0.02 then "  -> on the same circle!" else "  (0 means on the same circle)"), if dev < 0.02 then gold else (150, 150, 150)) ]

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = evP env pos
        cands = zip [0 ..] (wCps w ++ wLps w ++ [wP4 w])
        pick (best, k) (i, p) = let (sx, sy) = toPx env p; d = hypot (mx - sx) (my - sy) in if d < best then (d, i) else (best, k)
    in w { wDrag = snd (foldl' pick (24, -1) cands) }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion pos | wDrag w >= 0 ->
    let (mx, my) = evP env pos
        p = (clampv (-7) 7 ((mx - realToFrac (envW env) * 0.42) / unitPx), clampv (-5) 5 ((realToFrac (envH env) * 0.56 - my) / unitPx))
        set i xs = [ if k == i then p else x | (k, x) <- zip [0 ..] xs ]
        d = wDrag w
    in if d < 3 then w { wCps = set d (wCps w) } else if d < 5 then w { wLps = set (d - 3) (wLps w) } else w { wP4 = p }
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> w { wCmp = not (wCmp w) }
    | c `elem` "rR" -> w { wCps = wCps world0, wLps = wLps world0, wP4 = wP4 world0, wAuto = False }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 8 項目・同じ許容誤差・同じ種）
selfTest :: [(String, Bool)]
selfTest = evalRng 12 $ do
  let rp = do a <- nextR; b <- nextR; pure (a * 6 - 3, b * 6 - 3)
  rs <- mapM (\_ -> do
          x <- rp; yv <- rp
          let xx = up x
              ok1 = isZeroM (gp cga xx xx) 1e-10 && near (inner cga xx (up yv) U.! 0) (-0.5 * ((fst x - fst yv) ^ (2 :: Int) + (snd x - snd yv) ^ (2 :: Int))) 1e-10
          a <- rp; b <- rp; c <- rp
          let cc@(ccx, ccy, ccr) = circumcenter a b c
              cabc = wedges cga [up a, up b, up c]
              ok2 = case circleOf cabc of
                IsCircle (cx, cy) r2 -> near cx ccx (1e-7 * max 1 (abs ccx)) && near cy ccy (1e-7 * max 1 (abs ccy)) && near (sqrt r2) ccr (1e-7 * max 1 ccr)
                _ -> False
          p <- rp; q <- rp
          let tt = meet cga cabc (wedges cga [up p, up q, einf])
              ok3 = case (pairOf tt, lineCircle p q ccx ccy ccr) of
                (Nothing, Nothing) -> True
                (Just (pa, pb), Just (qa, qb)) ->
                  let na u v = nearArr [fst u, snd u] [fst v, snd v] 1e-6
                  in (na pa qa && na pb qb) || (na pa qb && na pb qa)
                _ -> False
          th <- (* 6.28) <$> nextR
          let on = (ccx + ccr * cos th, ccy + ccr * sin th)
              ok4 = norm cga (wedge cga (up on) cabc) < 1e-7 * norm cga cabc
          pure (ok1, ok2, ok3, ok4, cc)) [1 .. 30 :: Int]
  let allOf f = and (map f rs)
      pp = pairOf (wedge cga (up (1, 2)) (up (-0.5, 0.3)))
      na u v = nearArr [fst u, snd u] [fst v, snd v] 1e-9
      c3 = makeAlgebra [1, 1, 1, 1, -1]
      ei = addM (blade c3 8 1) (blade c3 16 1)
      eo3 = smul (subM (blade c3 16 1) (blade c3 8 1)) 0.5
      up3 pv = addMs c3 [vec c3 (pv ++ [0, 0]), smul ei (0.5 * dotv pv pv), eo3]
      ctr = [1, -2, 0.5]
      rr = 2
      sph = wedges c3 [ up3 (zipWith (\x ui -> x + rr * ui) ctr u) | u <- [[1, 0, 0], [0, 1, 0], [0, 0, 1], [0.6, 0, 0.8]] ]
      sg0 = dualOf c3 sph
      sg = smul sg0 (-1 / (inner c3 sg0 ei U.! 0))
  pure
    [ ("einf^2 = 0, e0^2 = 0, e0.einf = -1", isZeroM (gp cga einf einf) 1e-9 && isZeroM (gp cga eo eo) 1e-9 && near (inner cga eo einf U.! 0) (-1) 1e-9)
    , ("up(x)^2 = 0 (a point has 'length 0'), up(x).up(y) = -1/2|x - y|^2", allOf (\(a, _, _, _, _) -> a))
    , ("circle from the wedge of 3 points: centre and radius = circumcentre formula (30 sets)", allOf (\(_, b, _, _, _) -> b))
    , ("point pair from circle meet line = roots of the quadratic (and both say 'none' when they miss)", allOf (\(_, _, c, _, _) -> c))
    , ("a point P on the circle has P^C = 0", allOf (\(_, _, _, d, _) -> d))
    , ("the wedge P^Q of 2 points is a point pair: extracting it gives the 2 points back"
      , case pp of
          Just (pa, pb) -> (na pa (1, 2) && na pb (-0.5, 0.3)) || (na pb (1, 2) && na pa (-0.5, 0.3))
          Nothing -> False)
    , ("3 points on a line -> a line (centre at infinity)", case circleOf (wedges cga [up (0, 0), up (1, 1), up (3, 3)]) of { IsLine _ _ -> True; _ -> False })
    , ("3D Cl(4,1) (32 components): sphere from the wedge of 4 points has centre (1, -2, 0.5) and radius 2"
      , nearArr [sg U.! 1, sg U.! 2, sg U.! 4] ctr 1e-9 && near (sqrt (gp c3 sg sg U.! 0)) rr 1e-9) ]

-- ---------- やってみよう ----------
-- 1. 白い 3 点を一直線に並べると、円はどうなる？ 画面の文字は？
-- 2. 水色の直線を円から離していくと、点対はどうなる？ ちょうど接するときは？
-- 3. 黄色の 4 点目を紫の円の上にのせて、|P4∧C| が 0 になることを確かめよう
-- 4. up(x)·up(y) = −½|x − y|² なので、内積だけで距離がわかる。2 点の距離を inner cga で計算してみよう
-- 5. 円と円の交わりも meet cga c1 c2 で点対になる。もう 1 つ円を足してみよう

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
