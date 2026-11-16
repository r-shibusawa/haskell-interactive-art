-- ============================================
-- 47-12 ザイフェルト曲面
-- どんな結び目も「向きのつけられる曲面のふち」になる。作り方（ザイフェルトの算法）：
--   ① ひもに向きをつける → ② 各交点を向きに沿って開く（入ってきたら、出ていく方へ）→ ばらばらの円（ザイフェルト円）
--   ③ 円に円板を張る（入れ子は高さをずらす）→ ④ 交点のところを、ねじれた帯でつなぐ
--   種数 g = (交点の数 − 円の数 − 成分の数 + 2) ÷ 2。結び目なら (n − s + 1) ÷ 2。g = 「穴の数」
-- 操作：スペース = 次の段階 / A = 自動 / ←→ = 結び目を選ぶ
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newListArray, readArray, writeArray)
import Data.Bits (popCount, shiftL, testBit)
import Data.List (find, foldl', mapAccumL, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import System.Environment (lookupEnv)
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as U

r0 :: Float
r0 = 24                       -- ★ 交点を開く半径（px）

kn :: [String]
kn = ["0_1", "3_1", "4_1", "5_1", "5_2", "6_1", "6_2", "6_3", "L2a1", "L5a1", "L6a4"]

phaseNames :: [String]
phaseNames =
  [ "1. Knot diagram (arrows = orientation)"
  , "2. Open each crossing along the orientation -> Seifert circles"
  , "3. Fill each circle with a disc"
  , "4. Join at the crossings with twisted bands -> surface done" ]

-- ザイフェルト円の色（色相だけ使う）
ccHues :: [Float]
ccHues = [38, 10, 205, 140, 290, 55, 180]

cc :: Int -> Color
cc k = case drop (k `mod` length ccHues) [hsb 38 50 95, hsb 10 70 100, hsb 205 55 100, hsb 140 50 85, hsb 290 40 100, hsb 55 70 100, hsb 180 60 90] of
  (c : _) -> c
  [] -> white

data World = World
  { wIdx    :: !Int
  , wPhase  :: !Int
  , wAuto   :: !Bool
  , wTimer  :: !Int
  , wCs     :: [[P3]]
  , wD      :: Diagram
  , wLoopOf :: M.Map Int Int     -- 辺 → どのザイフェルト円か
  , wNS     :: !Int              -- ザイフェルト円の数
  , wMu     :: !Int              -- 成分の数
  , wGenus  :: !Float
  , wDeg    :: !Int }            -- アレクサンダー多項式の次数

main :: IO ()
main = runSketch (sketch "47-12 Seifert surface" (rebuild (World 1 0 True 0 [] (diagramOf []) M.empty 1 1 0 0)))
  { skBack  = hsb 228 45 16
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = event
  }

step :: World -> World
step w
  | not (wAuto w) = w
  | wTimer w + 1 > 140 = w { wTimer = 0, wPhase = (wPhase w + 1) `mod` 4 }
  | otherwise = w { wTimer = wTimer w + 1 }

rebuild :: World -> World
rebuild w = w { wCs = cs, wD = d, wLoopOf = loopOf, wNS = nS, wMu = mu, wGenus = fromIntegral (n - nS - mu + 2) / 2
              , wDeg = length (alexander (dgPd d)) - 1 }
  where
    name = case drop (wIdx w) kn of { (x : _) -> x; [] -> "0_1" }
    cs = knotCurves name 240
    d = diagramOf cs
    n = length (dgPd d)
    -- ★ 向きに沿って開く：下から入った辺 i → 上の出ていく辺、上から入った辺 → 下の出ていく辺 k
    loopOf = classesOf (concatMap crossList (dgPd d))
               (concat [[(paIn (crPu c), paOut (crPo c)), (paIn (crPo c), paOut (crPu c))] | c <- dgCross d])
    nS = if n > 0 then (if M.null loopOf then 0 else 1 + maximum (M.elems loopOf)) else 1
    mu = if n > 0 then piNComp (pdInfo (dgPd d)) else 1

nameOf :: World -> String
nameOf w = case drop (wIdx w) kn of { (x : _) -> x; [] -> "0_1" }

draw :: Env -> World -> Picture
draw env w = view env $
  (if wPhase w == 0
     then drawDiagram scr (wD w) [hsb 38 40 95, hsb 10 75 100, hsb 215 50 100] 6 11
          : [arrowAt (pt c k) (pt c (k + 3)) | c <- scr, let m = length c, m > 0, k <- [0, 40 .. m - 1]]
     else drawSmoothed w scr)
  ++ [ txt 20 36 18 white ("Space: next step   A: auto " ++ (if wAuto w then "ON" else "off") ++ "   Left/Right: " ++ nm ++ " (" ++ kTitle (knotByName nm) ++ ")")
     , txt 20 76 22 (hsb 45 40 100) (case drop (wPhase w) phaseNames of { (s : _) -> s; [] -> "" })
     , txt x0 y0 20 white ("Crossings n = " ++ show n)
     , txt x0 (y0 + 34) 20 white ("Seifert circles s = " ++ show (wNS w))
     , txt x0 (y0 + 68) 20 white ("Components mu = " ++ show (wMu w))
     , txt x0 (y0 + 116) 22 (hsb 30 70 100) ("Genus g = (n - s - mu + 2)/2 = " ++ num (wGenus w)) ]
  ++ paragraphs
  where
    nm = nameOf w
    bigR = min (envW env * 0.27) (envH env * 0.38)
    scr = toScreen (wCs w) (envW env * 0.32) (envH env * 0.55) bigR
    pt c k = let v = U.fromList c; (x, y, _) = v U.! (k `mod` U.length v) in (x, y)
    x0 = envW env * 0.6
    y0 = 140
    n = length (dgPd (wD w))
    half = fromIntegral (wDeg w) / 2 :: Float
    paras = [ "Degree of the Alexander polynomial / 2 = " ++ num half
              ++ (if half == wGenus w then "  = g (this surface has the fewest holes)" else "  <= g") | take 1 nm /= "L" ]
         ++ [ "Euler characteristic chi = s - n = " ++ show (wNS w - n) ++ " (s discs - n bands). The surface has "
              ++ show (wMu w) ++ " boundary curve" ++ (if wMu w == 1 then "" else "s") ++ ", so chi = 2 - 2g - mu"
            , "For a minimal diagram of an alternating knot, this algorithm gives the genus of the knot (3_1, 4_1: g = 1; 5_1: g = 2)" ]
    paragraphs = concat (snd (mapAccumL (\y p -> let ls = wrapWords 44 p
                                                 in (y + fromIntegral (length ls) * 20 + 16, [txt x0 (y + fromIntegral q * 20) 15 (hsb 0 0 75) s | (q, s) <- zip [0 :: Int ..] ls]))
                                        (y0 + 160) paras))

-- 開いた図：辺を交点のまわりで切り、向きに沿ってつなぐ。段階 ③ で円板、④ で帯
drawSmoothed :: World -> [[P3]] -> [Picture]
drawSmoothed w scr =
  -- ③ 円板（入れ子のものほど明るく：半透明を重ねる）
  [ Color (hsba (hueOf k) 45 80 0.3) (Pictures (map Polygon (triangulate (thin ps)))) | wPhase w >= 2, (k, ps) <- M.toList loops ]
  -- ④ 帯：交点ごとに、開いた 2 本の弧を結ぶ「ちょうネクタイ」形の帯（色 = 交点の符号）
  ++ [ Color (if crSign c > 0 then hsba 10 70 100 0.75 else hsba 200 60 100 0.75) (Pictures (map Polygon (quadTris a e d b)))
     | wPhase w >= 3, (k, c) <- zip [0 ..] (dgCross dg)
     , let a = end k (paIn (crPu c)), let b = end k (paOut (crPo c)), let d = end k (paIn (crPo c)), let e = end k (paOut (crPu c)) ]
  -- 弧とつなぎ目（色 = どのザイフェルト円か）
  ++ [ Color (cc (M.findWithDefault 0 e (wLoopOf w))) (thickPath 5 ps) | (e, ps) <- M.toList polys ]
  ++ [ Color (cc (M.findWithDefault 0 u (wLoopOf w))) (thickPath 5 (bez a q b))
     | (k, c) <- zip [0 ..] (dgCross dg)
     , let xm = mid (end k (paIn (crPu c))) (end k (paOut (crPu c)))
     , (u, v) <- [(paIn (crPu c), paOut (crPo c)), (paIn (crPo c), paOut (crPu c))]
     , let a = end k u, let b = end k v, let q = mid (mid a b) xm ]
  where
    dg = wD w
    hueOf k = case drop (k `mod` length ccHues) ccHues of { (h : _) -> h; [] -> 0 }
    flat c = [(x, y) | (x, y, _) <- c]
    -- 辺ごとの点の列（polys）と、交点のまわりの端点（ends：(交点, 辺) → 点）
    (polys, ends) = foldl' addCurve (M.empty, M.empty) (zip scr (dgPass dg ++ repeat []))
    addCurve (pm, em) (c, ps)
      | null ps = (M.insert 0 (flat c) pm, em)
      | otherwise =
          let p = pathOf True (flat c)
              m = length ps
              nexts = drop 1 ps ++ take 1 ps
          in foldl' (\(pm', em') (q, pa, nx) ->
                       let a = plToLen p (paS pa)
                           b = plToLen p (paS nx) + (if q == m - 1 then plL p else 0)
                           pts = plPiece p (a + r0) (b - r0)
                       in ( M.insert (paOut pa) pts pm'
                          , M.insert (paK nx, paIn nx) (lastP pts) (M.insert (paK pa, paOut pa) (firstP pts) em') ))
                    (pm, em) (zip3 [0 ..] ps nexts)
    end k lab = M.findWithDefault (0, 0) (k, lab) ends
    -- ザイフェルト円をたどって多角形にする（辺 → 交点で開いた先の辺 → …）
    nextOf = M.fromList (concat [[(paIn (crPu c), paOut (crPo c)), (paIn (crPo c), paOut (crPu c))] | c <- dgCross dg])
    bridge = M.fromList (concat [ [ (paIn (crPu c), (end k (paIn (crPu c)), end k (paOut (crPo c))))
                                  , (paIn (crPo c), (end k (paIn (crPo c)), end k (paOut (crPu c)))) ]
                                | (k, c) <- zip [0 ..] (dgCross dg) ])
    loops
      | null (dgPd dg) = M.fromList [(0, M.findWithDefault [] 0 polys)]
      | otherwise = snd (foldl' trace (S.empty, M.empty) (M.keys polys))
    trace (seen, lm) e
      | S.member e seen = (seen, lm)
      | otherwise = let (seen', pts) = follow seen e (0 :: Int) []
                    in (seen', M.insert (M.findWithDefault 0 e (wLoopOf w)) pts lm)
    follow seen cur guard acc
      | S.member cur seen || guard >= 200 = (seen, concat (reverse acc))
      | otherwise = case M.lookup cur bridge of
          Nothing -> (S.insert cur seen, concat (reverse (M.findWithDefault [] cur polys : acc)))
          Just (a, b) -> follow (S.insert cur seen) (M.findWithDefault cur cur nextOf) (guard + 1) ([mid a b] : M.findWithDefault [] cur polys : acc)

-- 向きの矢印
arrowAt :: Point -> Point -> Picture
arrowAt (ax, ay) (bx, by) = Color (hsb 55 80 100) (Pictures [thickLine 3 (ax, ay) (tip (ang - 0.5)), thickLine 3 (ax, ay) (tip (ang + 0.5))])
  where
    ang = atan2 (by - ay) (bx - ax)
    tip t = (ax - 14 * cos t, ay - 14 * sin t)

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPhase = (wPhase w + 1) `mod` 4, wTimer = 0 }
  EventKey (Char c) Down _ _ | c `elem` "aA" -> w { wAuto = not (wAuto w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild w { wIdx = (wIdx w + 1) `mod` length kn }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild w { wIdx = (wIdx w + length kn - 1) `mod` length kn }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 三葉結び目：円 2 つ、帯 3 本 → g = (3 − 2 + 1)/2 = 1。曲面は穴 1 つのドーナツから円板を 1 枚とったもの
-- 2. 5_1 と 5_2 は交点の数が同じ 5 なのに種数がちがう。円の数を数えよう
-- 3. ホップ絡み目（L2a1）の曲面は？ ふちが 2 本の帯（アニュラス）
-- 4. 帯のねじれの向き（赤 = +、青 = −）を全部反対にすると、鏡像の結び目の曲面になる
-- 5. 帯の数 − 円板の数 = 1 − χ。紙とテープで三葉結び目のザイフェルト曲面を作ってみよう

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 文を単語の切れ目で折り返す（1 行 n 文字まで）
wrapWords :: Int -> String -> [String]
wrapWords n = reverse . foldl add [] . words
  where
    add [] t = [t]
    add (l : ls) t
      | length l + 1 + length t <= n = (l ++ " " ++ t) : ls
      | otherwise = t : l : ls

-- 整数なら整数で、そうでなければ小数 1 桁で
num :: Float -> String
num x = if x == fromIntegral (round x :: Int) then show (round x :: Int) else show (fromIntegral (round (x * 10) :: Int) / 10 :: Float)

mid :: Point -> Point -> Point
mid (ax, ay) (bx, by) = ((ax + bx) / 2, (ay + by) / 2)

firstP, lastP :: [Point] -> Point
firstP ps = case ps of { (p : _) -> p; [] -> (0, 0) }
lastP ps = firstP (reverse ps)

-- 2 次ベジエ曲線（a → b、制御点 q）を 9 点で
bez :: Point -> Point -> Point -> [Point]
bez (ax, ay) (qx, qy) (bx, by) =
  [ ((1 - t) * (1 - t) * ax + 2 * t * (1 - t) * qx + t * t * bx, (1 - t) * (1 - t) * ay + 2 * t * (1 - t) * qy + t * t * by)
  | i <- [0 .. 8 :: Int], let t = fromIntegral i / 8 ]

-- 点を間引く（塗るための多角形。90 点くらいまで）
thin :: [Point] -> [Point]
thin ps = let n = length ps; k = max 1 (n `div` 90) in [p | (i, p) <- zip [0 :: Int ..] ps, i `mod` k == 0]

-- gloss の Polygon は凸の形しか正しく塗れないので、多角形を三角形に分ける（耳を切り取る方法）
triangulate :: [Point] -> [[Point]]
triangulate ps0 = go (if area ps > 0 then ps else reverse ps) (0 :: Int)
  where
    ps = dedup ps0
    dedup xs = [p | (p, q) <- zip xs (drop 1 xs ++ take 1 xs), dist2 p q > 1e-6]
    area xs = sum [x1 * y2 - x2 * y1 | ((x1, y1), (x2, y2)) <- zip xs (drop 1 xs ++ take 1 xs)] / 2
    crossZ (ax, ay) (bx, by) (cx, cy) = (bx - ax) * (cy - by) - (by - ay) * (cx - bx)
    inside a b c p = crossZ a b p >= 0 && crossZ b c p >= 0 && crossZ c a p >= 0
    go xs guard
      | length xs < 3 = []
      | length xs == 3 = [xs]
      | guard > 4000 = fan xs
      | otherwise =
          let n = length xs
              v = U.fromList xs
              at' i = v U.! (i `mod` n)
              isEar i = let a = at' (i - 1 + n); b = at' i; c = at' (i + 1)
                        in crossZ a b c > 0 && not (any (inside a b c) [at' j | j <- [0 .. n - 1], j /= i, j /= (i + 1) `mod` n, j /= (i - 1 + n) `mod` n])
          in case filter isEar [0 .. n - 1] of
               (i : _) -> [at' (i - 1 + n), at' i, at' (i + 1)] : go [p | (j, p) <- zip [0 ..] xs, j /= i] (guard + 1)
               [] -> fan xs
    fan xs = case xs of
      (p : rest) -> [[p, b, c] | (b, c) <- zip rest (drop 1 rest)]
      [] -> []

-- 四角形 a-e-d-b を三角形に（ちょうネクタイ形のときは交わる点で 2 つに分ける）
quadTris :: Point -> Point -> Point -> Point -> [[Point]]
quadTris a e d b = case (segX a e d b, segX e d b a) of
  (Just p, _) -> [[a, p, b], [e, d, p]]
  (_, Just p) -> [[a, e, p], [p, d, b]]
  _ | side a d e * side a d b < 0 -> [[a, e, d], [a, d, b]]
    | otherwise -> [[a, e, b], [e, d, b]]
  where
    side (px, py) (qx, qy) (rx, ry) = (qx - px) * (ry - py) - (qy - py) * (rx - px)
    segX (x1, y1) (x2, y2) (x3, y3) (x4, y4) =
      let den = (x2 - x1) * (y4 - y3) - (y2 - y1) * (x4 - x3)
          t = ((x3 - x1) * (y4 - y3) - (y3 - y1) * (x4 - x3)) / den
          u = ((x3 - x1) * (y2 - y1) - (y3 - y1) * (x2 - x1)) / den
      in if abs den > 1e-9 && t > 0 && t < 1 && u > 0 && u < 1 then Just (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t) else Nothing

-- ==================================================
-- ここから下は「結び目の道具」（中身は読まなくても OK。消さないでね）
-- ==================================================
-- ---- 結び目の表と不変量の計算
-- PD 符号：交点ごとに (i, j, k, l)（辺の番号）。i = 下をくぐって入ってくる辺、そこから反時計回りに j, k, l。
--   下のひもは i → k、上のひもは j と l を結ぶ。番号は KnotTheory / Knot Atlas と同じ約束
-- ジョーンズ多項式の約束：A = t^(−1/4)。右手系三葉（交点の符号が全部 +）は V = t + t³ − t⁴、左手系は −t⁻⁴ + t⁻³ + t⁻¹
-- 計算はすべて整数（Integer）で正確に行う（小数を使わない）
type Cross = (Int, Int, Int, Int)
type PD = [Cross]

data Knot = Knot { kName :: String, kTitle :: String, kPd :: PD }

knots :: [Knot]
knots =
  [ Knot "0_1" "Unknot" []
  , Knot "3_1" "Trefoil" [(1, 4, 2, 5), (3, 6, 4, 1), (5, 2, 6, 3)]
  , Knot "4_1" "Figure-eight knot" [(4, 2, 5, 1), (8, 6, 1, 5), (6, 3, 7, 4), (2, 7, 3, 8)]
  , Knot "5_1" "Cinquefoil knot" [(1, 6, 2, 7), (3, 8, 4, 9), (5, 10, 6, 1), (7, 2, 8, 3), (9, 4, 10, 5)]
  , Knot "5_2" "Three-twist knot" [(1, 4, 2, 5), (3, 8, 4, 9), (5, 10, 6, 1), (9, 6, 10, 7), (7, 2, 8, 3)]
  , Knot "6_1" "Stevedore knot" [(1, 4, 2, 5), (7, 10, 8, 11), (3, 9, 4, 8), (9, 3, 10, 2), (5, 12, 6, 1), (11, 6, 12, 7)]
  , Knot "6_2" "6_2 knot" [(1, 4, 2, 5), (5, 10, 6, 11), (3, 9, 4, 8), (9, 3, 10, 2), (7, 12, 8, 1), (11, 6, 12, 7)]
  , Knot "6_3" "6_3 knot" [(4, 2, 5, 1), (8, 4, 9, 3), (12, 9, 1, 10), (10, 5, 11, 6), (6, 11, 7, 12), (2, 8, 3, 7)]
  , Knot "L2a1" "Hopf link" [(4, 1, 3, 2), (2, 3, 1, 4)]
  , Knot "L5a1" "Whitehead link" [(6, 1, 7, 2), (10, 7, 5, 8), (4, 5, 1, 6), (2, 10, 3, 9), (8, 4, 9, 3)]
  , Knot "L6a4" "Borromean rings" [(6, 1, 7, 2), (12, 8, 9, 7), (4, 12, 1, 11), (10, 5, 11, 6), (8, 4, 5, 3), (2, 9, 3, 10)]
  ]

-- 名前で表をさがす（見つからなければ自明な結び目）
knotByName :: String -> Knot
knotByName nm = fromMaybe (Knot nm "" []) (find ((== nm) . kName) knots)

-- 表の i 番目（範囲の外は端に寄せる）
knotAt :: Int -> Knot
knotAt i = case drop (max 0 (min (length knots - 1) i)) knots of
  (k : _) -> k
  [] -> Knot "0_1" "Unknot" []

mirrorPD :: PD -> PD   -- 鏡像：上下を入れかえる
mirrorPD = map (\(i, j, k, l) -> (i, l, k, j))

crossList :: Cross -> [Int]
crossList (i, j, k, l) = [i, j, k, l]

-- つながりの分類：pairs でつないだとき、各ラベルが何番目のかたまりか（番号は最小のラベルの小さい順）
classesOf :: [Int] -> [(Int, Int)] -> M.Map Int Int
classesOf labels pairs = snd (foldl' visit (0, M.empty) (S.toAscList (S.fromList labels)))
  where
    adj = M.fromListWith (++) (concat [[(a, [b]), (b, [a])] | (a, b) <- pairs])
    visit (nc, seen) a
      | M.member a seen = (nc, seen)
      | otherwise = (nc + 1, flood nc [a] seen)
    flood _ [] seen = seen
    flood c (x : xs) seen
      | M.member x seen = flood c xs seen
      | otherwise = flood c (M.findWithDefault [] x adj ++ xs) (M.insert x c seen)

-- 交点の符号と成分（輪）を調べる。上のひもの向きは「辺の両端の一方が入口、もう一方が出口」から決める
data PdInfo = PdInfo
  { piN :: !Int, piSigns :: [Int], piComp :: M.Map Int Int, piNComp :: !Int
  , piUnderComp :: [Int], piOverComp :: [Int] }

pdInfo :: PD -> PdInfo
pdInfo pd = PdInfo (length pd) signs comp nComp (map (\(i, _, _, _) -> cmp i) pd) (map (\(_, j, _, _) -> cmp j) pd)
  where
    xs = zip [0 :: Int ..] pd
    occ = M.fromListWith (flip (++)) [(a, [(c, p)]) | (c, x) <- xs, (p, a) <- zip [0 :: Int ..] (crossList x)]
    comp = classesOf (M.keys occ) (concat [[(i, k), (j, l)] | (i, j, k, l) <- pd])
    nComp = if M.null comp then 0 else 1 + maximum (M.elems comp)
    cmp a = M.findWithDefault 0 a comp
    -- head (c, p)：その位置で辺が交点へ入ってくるか（わかったものだけ入れる）
    h0 = M.fromList (concat [[((c, 0), True), ((c, 2), False)] | (c, _) <- xs])
    stepLabel h ps = case ps of
      (o1 : o2 : _) -> case (M.lookup o1 h, M.lookup o2 h) of
        (Just h1, Nothing) -> M.insert o2 (not h1) h
        (Nothing, Just h2) -> M.insert o1 (not h2) h
        _ -> h
      _ -> h
    stepCross h (c, _) = case (M.lookup (c, 1) h, M.lookup (c, 3) h) of
      (Just h1, Nothing) -> M.insert (c, 3) (not h1) h
      (Nothing, Just h3) -> M.insert (c, 1) (not h3) h
      _ -> h
    sweep h = let h' = foldl' stepCross (foldl' stepLabel h (M.elems occ)) xs
              in if M.size h' == M.size h then h else sweep h'
    hd = sweep h0
    signs = [if M.lookup (c, 1) hd == Just False then 1 else -1 | (c, _) <- xs]   -- 上が l → j なら +、j → l なら −

writhe :: PD -> Int
writhe = sum . piSigns . pdInfo

-- 絡み数：成分 a と b の間の交点の符号の和 ÷ 2（半整数もありうるので Float）
linkingNumber :: PD -> Int -> Int -> Float
linkingNumber pd a b = fromIntegral (sum [s | (u, o, s) <- zip3 (piUnderComp i) (piOverComp i) (piSigns i), (u == a && o == b) || (u == b && o == a)]) / 2
  where i = pdInfo pd

-- 弧（下をくぐるところで切れた線）：上のひも j と l は同じ弧
pdArcs :: PD -> (M.Map Int Int, Int)
pdArcs pd = (arcOf, if M.null arcOf then 0 else 1 + maximum (M.elems arcOf))
  where arcOf = classesOf (concatMap crossList pd) [(j, l) | (_, j, _, l) <- pd]

arcIx :: M.Map Int Int -> Int -> Int
arcIx m a = M.findWithDefault 0 a m

-- 彩色の行列：交点ごとに 2·上 − 下1 − 下2（= 0 mod n が彩色の条件）
colourMatrix :: PD -> ([[Integer]], Int, M.Map Int Int)
colourMatrix pd = ([row x | x <- pd], nArcs, arcOf)
  where
    (arcOf, nArcs) = pdArcs pd
    row (i, j, k, _) = [fromIntegral (2 * hit (arcIx arcOf j) a - hit (arcIx arcOf i) a - hit (arcIx arcOf k) a) | a <- [0 .. nArcs - 1]]
    hit p a = if p == a then 1 else 0 :: Int

-- 行列の (i, j) 成分（範囲の外は 0）
ix2 :: Num a => [[a]] -> Int -> Int -> a
ix2 m i j = case drop i m of
  (r : _) -> case drop j r of
    (x : _) -> x
    [] -> 0
  [] -> 0

swapAt :: Int -> Int -> [a] -> [a]
swapAt a b xs
  | a == b = xs
  | otherwise = case (drop a xs, drop b xs) of
      (xa : _, xb : _) -> [if q == a then xb else if q == b then xa else x | (q, x) <- zip [0 ..] xs]
      _ -> xs

-- 整数行列を対角化（スミス標準形の手前まで）。M·V = U⁻¹·D。V（列の操作）も記録する
diagonalize :: [[Integer]] -> Int -> ([Integer], [[Integer]])
diagonalize m0 nc = go 0 m0 v0 []
  where
    nr = length m0
    v0 = [[if i == j then 1 else 0 | j <- [0 .. nc - 1]] | i <- [0 .. nc - 1]]
    go t m v ds
      | t >= min nr nc = (map abs (reverse ds ++ replicate (nc - length ds) 0), v)
      | otherwise = let (m', v') = settle t m v in go (t + 1) m' v' (ix2 m' t t : ds)
    settle t m v =
      case [(i, j) | (i, r) <- drop t (zip [0 ..] m), (j, x) <- drop t (zip [0 ..] r), x /= 0] of
        [] -> (m, v)
        cands ->
          let (bi, bj) = foldl' (\b c -> if absAt c < absAt b then c else b) (head' cands) cands
              absAt (i, j) = abs (ix2 m i j)
              head' (c : _) = c
              head' [] = (t, t)
              m1 = map (swapAt t bj) (swapAt t bi m)
              v1 = map (swapAt t bj) v
              p = ix2 m1 t t
              rt = case drop t m1 of { (r : _) -> r; [] -> [] }
              -- 行の消去：行 i −= q·行 t（q = 割り算の商、0 の方向へ切り捨て）
              m2 = [ if i > t then let q = ix2 [r] 0 t `quot` p in if q == 0 then r else [if j >= t then x - q * y else x | (j, x, y) <- zip3 [0 :: Int ..] r rt] else r
                   | (i, r) <- zip [0 :: Int ..] m1 ]
              -- 列の消去：列 j −= q·列 t（M と V の両方）
              qs = [if j > t then ix2 m2 t j `quot` p else 0 | j <- [0 .. nc - 1]]
              colOps r = let rtv = ix2 [r] 0 t in [x - q * rtv | (x, q) <- zip r qs]
              m3 = map colOps m2
              v2 = map colOps v1
              clean = all (\i -> ix2 m3 i t == 0) [t + 1 .. nr - 1] && all (\j -> ix2 m3 t j == 0) [t + 1 .. nc - 1]
          in if clean then (m3, v2) else settle t m3 v2

-- フォックス n 彩色の数 = ∏ gcd(dᵢ, n)（dᵢ = 対角成分。n が素数でなくても正しい）
colourings :: PD -> Integer -> Integer
colourings [] n = n
colourings pd n = product [gcd x n | x <- fst (diagonalize m nArcs)]
  where (m, nArcs, _) = colourMatrix pd

-- k 番目の n 彩色（弧ごとの色のリスト）。0 ≤ k < colourings pd n
colouringAt :: PD -> Integer -> Integer -> [Integer]
colouringAt pd n k0 = [sum (zipWith (*) r ys) `mod` n | r <- v]
  where
    (m, nArcs, _) = colourMatrix pd
    (d, v) = diagonalize m nArcs
    ys = snd (mapAccumL (\k x -> let g = gcd x n in (k `div` g, (k `mod` g) * (n `div` g))) k0 d)

-- 行列式（結び目の行列式）= 対角成分のうち 0 でないものの積（階数が 弧の数 − 1 のとき）
determinant :: PD -> Integer
determinant [] = 1
determinant pd = if length nz == nArcs - 1 then product nz else 0
  where
    (m, nArcs, _) = colourMatrix pd
    nz = filter (/= 0) (fst (diagonalize m nArcs))

-- ---- 多項式（係数のリスト。添字 = t の次数）
type Poly = [Integer]

pTrim :: Poly -> Poly
pTrim a = case reverse (dropWhile (== 0) (reverse a)) of
  [] -> [0]
  r -> r

pAdd :: Poly -> Poly -> Integer -> Poly   -- a + s·b
pAdd a b s = pTrim (zipLong a b)
  where
    zipLong (x : xs) (y : ys) = x + s * y : zipLong xs ys
    zipLong xs [] = xs
    zipLong [] ys = map (s *) ys

pMul :: Poly -> Poly -> Poly
pMul a b = pTrim (foldl' (\acc (i, x) -> addAt i [x * y | y <- b] acc) (replicate (length a + length b - 1) 0) (zip [0 ..] a))
  where addAt i ys acc = take i acc ++ zipWith (+) (drop i acc) (ys ++ repeat 0)

pZero :: Poly -> Bool
pZero a = all (== 0) a

-- 割り切れる割り算（ベアイス法では必ず割り切れる）
pDiv :: Poly -> Poly -> Poly
pDiv a0 b0
  | pZero a || pZero b = [0]
  | otherwise = pTrim (go (length a - lb) a (replicate (max 1 (length a - lb + 1)) 0))
  where
    a = pTrim a0
    b = pTrim b0
    lb = length b
    bl = last b
    go i r q
      | i < 0 = q
      | otherwise =
          let c = ix2 [r] 0 (i + lb - 1) `quot` bl
              r' = [if j >= i && j < i + lb then x - c * ix2 [b] 0 (j - i) else x | (j, x) <- zip [0 ..] r]
          in go (i - 1) r' [if j == i then c else y | (j, y) <- zip [0 ..] q]

pEval :: Poly -> Integer -> Integer
pEval a t = sum (zipWith (*) a (iterate (* t) 1))

pEvalF :: Poly -> Float -> Float
pEvalF a t = sum (zipWith (\c e -> fromIntegral c * t ^^ e) a [0 :: Int ..])

-- 多項式の行列式（ベアイス法：分数を使わない消去法）
polyDet :: [[Poly]] -> Poly
polyDet [] = [1]
polyDet m0 = go 0 m0 [1] 1
  where
    n = length m0
    at2 m i j = case drop i m of { (r : _) -> case drop j r of { (x : _) -> x; [] -> [0] }; [] -> [0] }
    go k m prev sgn
      | k >= n - 1 = map (* sgn) (at2 m (n - 1) (n - 1))
      | otherwise =
          let (m1, sgn1, ok)
                | not (pZero (at2 m k k)) = (m, sgn, True)
                | otherwise = case [s | s <- [k + 1 .. n - 1], not (pZero (at2 m s k))] of
                    (s : _) -> (swapAt k s m, negate sgn, True)
                    [] -> (m, sgn, False)
              mkk = at2 m1 k k
              m2 = [ if i > k then [if j > k then pDiv (pAdd (pMul x mkk) (pMul (at2 m1 i k) (at2 m1 k j)) (-1)) prev else x | (j, x) <- zip [0 :: Int ..] r] else r
                   | (i, r) <- zip [0 :: Int ..] m1 ]
          in if ok then go (k + 1) m2 mkk sgn1 else [0]

-- アレクサンダー行列：上の弧に 1 − t、下の 2 本に t と −1（どちらに t が付くかは交点の符号で決まる）
alexanderMatrix :: PD -> [[Poly]]
alexanderMatrix pd = zipWith row pd (piSigns (pdInfo pd))
  where
    (arcOf, nArcs) = pdArcs pd
    row (i, j, k, _) sg =
      let puts = (arcIx arcOf j, [1, -1]) : (if sg > 0 then [(arcIx arcOf i, [0, 1]), (arcIx arcOf k, [-1])] else [(arcIx arcOf i, [-1]), (arcIx arcOf k, [0, 1])])
      in [foldl' (\p (a', q) -> if a' == a then pAdd p q 1 else p) [0] puts | a <- [0 .. nArcs - 1]]

-- アレクサンダー多項式：最後の行と列を消した小行列式。t^k 倍と符号をそろえる（最低次の係数が正）
alexander :: PD -> Poly
alexander [] = [1]
alexander pd = case dropWhile (== 0) p of
  [] -> [0]
  q@(c : _) -> if c < 0 then map negate q else q
  where
    mat = map dropLast (dropLast (alexanderMatrix pd))
    dropLast xs = take (length xs - 1) xs
    p = pTrim (polyDet mat)

-- ---- カウフマン括弧 → ジョーンズ多項式（ローラン多項式は Map 次数 係数 の形）
type Laurent = M.Map Int Integer

lAdd :: Laurent -> Laurent -> Integer -> Laurent   -- a + s·b
lAdd a b s = M.filter (/= 0) (M.unionWith (+) a (M.map (s *) b))

lMul :: Laurent -> Laurent -> Laurent
lMul a b = M.filter (/= 0) (M.fromListWith (+) [(e + f, x * y) | (e, x) <- M.toList a, (f, y) <- M.toList b])

-- 1 つの状態（bits の c 桁目 = 1 なら交点 c を B に開く）のループの数
stateLoops :: PD -> Int -> Int
stateLoops pd bits = loopCounter pd bits

-- ループを数える道具を作る（ラベルを 0, 1, 2, … に直して、状態ごとに配列で union-find）
loopCounter :: PD -> Int -> Int
loopCounter pd = \bits -> runST (count bits)
  where
    labels = S.toAscList (S.fromList (concatMap crossList pd))
    nL = length labels
    idx = M.fromList (zip labels [0 ..])
    li a = M.findWithDefault 0 a idx
    xs = [(c, li i, li j, li k, li l) | (c, (i, j, k, l)) <- zip [0 ..] pd]
    count :: Int -> ST s Int
    count bits = do
      par <- newListArray (0, max 0 (nL - 1)) [0 .. max 0 (nL - 1)] :: ST s (STUArray s Int Int)
      let root a = do
            p <- readArray par a
            if p == a then pure a else do
              r <- root p
              writeArray par a r
              pure r
          join a b = do
            ra <- root a
            rb <- root b
            when (ra /= rb) (writeArray par ra rb)
      forM_ xs $ \(c, i, j, k, l) ->
        if testBit bits c then join i l >> join j k    -- B：(i,l) と (j,k)
                          else join i j >> join k l    -- A：(i,j) と (k,l)
      rs <- mapM (\a -> (== a) <$> root a) [0 .. nL - 1]
      pure (length (filter id rs))

bracketMax :: Int
bracketMax = 14              -- ★ これより交点が多いと 2^n 状態が多すぎるので計算しない

-- 括弧 ⟨K⟩ = Σ A^(a−b) d^(ループ − 1)、d = −A² − A⁻²（Map A の次数 係数）
bracket :: PD -> Maybe Laurent
bracket [] = Just (M.singleton 0 1)
bracket pd
  | n > bracketMax = Nothing
  | otherwise = Just (M.foldlWithKey' add M.empty counts)
  where
    n = length pd
    loops = loopCounter pd
    -- 同じ（A の次数, ループの数）になる状態をまとめて数える
    counts = M.fromListWith (+) [((n - 2 * popCount s, loops s), 1 :: Integer) | s <- [0 .. shiftL 1 n - 1 :: Int]]
    d = M.fromList [(2, -1), (-2, -1)]
    dPow k = iterate (lMul d) (M.singleton 0 1) !! max 0 k
    add acc (e, l) cnt = lAdd acc (lMul (M.singleton e 1) (dPow (l - 1))) cnt

-- ジョーンズ多項式 V(t) = (−A³)^(−w) ⟨K⟩ を A = t^(−1/4) で。返り値は Map (2×(t の次数)) 係数（絡み目は半整数の次数になるので 2 倍で持つ）
type Jones = M.Map Int Integer

jones :: PD -> Maybe Jones
jones pd = case bracket pd of
  Nothing -> Nothing
  Just br ->
    let w = writhe pd
        f = lMul br (M.singleton (-3 * w) (if even w then 1 else -1))
    in Just (M.fromList [(negate (e `div` 2), c) | (e, c) <- M.toList f])

jonesMirror :: Jones -> Jones
jonesMirror = M.mapKeys negate

jonesEval :: Jones -> Float -> Float
jonesEval v t = sum [fromIntegral c * t ** (fromIntegral e / 2) | (e, c) <- M.toList v]

lEqual :: M.Map Int Integer -> M.Map Int Integer -> Bool
lEqual a b = M.filter (/= 0) a == M.filter (/= 0) b

mEqual :: Maybe Jones -> Maybe Jones -> Bool   -- 両方計算できて等しいか
mEqual (Just a) (Just b) = lEqual a b
mEqual _ _ = False

-- 多項式を文字列に（ASCII）。terms = [(次数の文字列, 係数)]（並んだ順に書く）
termsStr :: [(String, Integer)] -> String -> String
termsStr terms v = case foldl' add "" terms of
  "" -> "0"
  s -> s
  where
    add out (e, c)
      | c == 0 = out
      | otherwise =
          let mono | e == "0" = ""
                   | e == "1" = v
                   | '/' `elem` e = v ++ "^(" ++ e ++ ")"
                   | otherwise = v ++ "^" ++ e
              a = abs c
              body = if null mono then show a else (if a == 1 then "" else show a) ++ mono
          in if null out then (if c < 0 then "-" else "") ++ body else out ++ (if c < 0 then " - " else " + ") ++ body

jonesStr :: Maybe Jones -> String
jonesStr Nothing = "(too many crossings: skipped)"
jonesStr (Just v) = termsStr [(if even e then show (e `div` 2) else show e ++ "/2", c) | (e, c) <- M.toAscList v] "t"

alexStr :: Poly -> String
alexStr p = termsStr (reverse [(show i, c) | (i, c) <- zip [0 :: Int ..] p]) "t"

-- ---- 表から名前をさがす：行列式・アレクサンダー・（計算できれば）ジョーンズが全部一致するもの
data Hit = Hit { hName :: String, hTitle :: String, hHow :: String }
data Ident = Ident { idHits :: [Hit], idV :: Maybe Jones, idA :: String, idD :: Integer, idNc :: Int }

tableInv :: [(Knot, Int, Integer, String, Maybe Jones)]
tableInv = [(k, max 1 (piNComp (pdInfo (kPd k))), determinant (kPd k), alexStr (alexander (kPd k)), jones (kPd k)) | k <- knots]

identify :: PD -> Ident
identify pd = Ident hits v a d nc
  where
    v = jones pd
    a = alexStr (alexander pd)
    d = determinant pd
    nc = max 1 (piNComp (pdInfo pd))
    hits = [ h | (k, tnc, td, ta, tv) <- tableInv, tnc == nc, td == d, ta == a
               , Just h <- [match k tv] ]
    match k tv = case (v, tv) of
      (Nothing, _) -> Just (Hit (kName k) (kTitle k) " (V skipped)")
      (Just x, Just y)
        | lEqual x y -> Just (Hit (kName k) (kTitle k) "")
        | lEqual x (jonesMirror y) -> Just (Hit (kName k) (kTitle k) " (mirror)")
      _ -> Nothing

-- |V(−1)|² ：t^(1/2) = i を入れる（i の h 乗は 1, i, −1, −i をくり返す）。整数のまま 2 乗で返す
jonesAbsSqAtMinus1 :: Jones -> Integer
jonesAbsSqAtMinus1 v = re * re + im * im
  where
    part r = sum [c | (h, c) <- M.toList v, h `mod` 4 == r]
    re = part 0 - part 2
    im = part 1 - part 3

jonesAbsAtMinus1 :: Jones -> Float
jonesAbsAtMinus1 = sqrt . fromIntegral . jonesAbsSqAtMinus1

-- ---- 自己テスト：知られている値と、ここで計算した値を比べる
--   curvesCheck：形（3D 曲線）から作った図の検査（曲線の道具があるスケッチだけ。なければ Nothing）
selfTestWith :: Maybe (Knot -> Bool) -> [(String, Bool)]
selfTestWith curvesCheck =
  [ ("Unknot V = 1", lEqual' (j "0_1") [(0, 1)])
  , ("Twisted loop (R1, +/-) also V = 1", lEqual' (jones [(1, 1, 2, 2)]) [(0, 1)] && lEqual' (jones [(2, 1, 1, 2)]) [(0, 1)])
  , ("Right trefoil w = +3, V = t + t^3 - t^4", writhe r3 == 3 && lEqual' (jones r3) [(2, 1), (6, 1), (8, -1)])
  , ("Left trefoil w = -3, V = -t^-4 + t^-3 + t^-1", writhe l3 == -3 && lEqual' (jones l3) [(-8, -1), (-6, 1), (-2, 1)])
  , ("Figure-eight V = t^-2 - t^-1 + 1 - t + t^2", lEqual' (j "4_1") [(-4, 1), (-2, -1), (0, 1), (2, -1), (4, 1)])
  , ("5_1 V = -t^-7 + t^-6 - t^-5 + t^-4 + t^-2", lEqual' (j "5_1") [(-14, -1), (-12, 1), (-10, -1), (-8, 1), (-4, 1)])
  , ("6_3 V = -t^-3 + 2t^-2 - 2t^-1 + 3 - 2t + 2t^2 - t^3", lEqual' (j "6_3") [(-6, -1), (-4, 2), (-2, -2), (0, 3), (2, -2), (4, 2), (6, -1)])
  , ("Hopf link V = -t^(-1/2) - t^(-5/2), linking number +/-1", lEqual' (j "L2a1") [(-1, -1), (-5, -1)] && abs (linkingNumber (pdOf "L2a1") 0 1) == 1)
  , ("Whitehead: linking number 0, V differs from 2 unlinked rings", linkingNumber (pdOf "L5a1") 0 1 == 0 && lEqual' (j "L5a1") [(-7, 1), (-5, -2), (-3, 1), (-1, -2), (1, 1), (3, -1)])
  , ("Borromean: 3 rings, every pair has linking number 0", piNComp bi == 3 && all (\(a, b) -> linkingNumber bor a b == 0) [(0, 1), (0, 2), (1, 2)]
                                                              && lEqual' (j "L6a4") [(-6, -1), (-4, 3), (-2, -2), (0, 4), (2, -2), (4, 3), (6, -1)])
  , ("Alexander trefoil t^2 - t + 1", al "3_1" == "t^2 - t + 1")
  , ("Alexander figure-eight t^2 - 3t + 1", al "4_1" == "t^2 - 3t + 1")
  , ("Alexander 5_2 2t^2 - 3t + 2, 6_1 2t^2 - 5t + 2", al "5_2" == "2t^2 - 3t + 2" && al "6_1" == "2t^2 - 5t + 2")
  , ("Alexander 6_2 t^4 - 3t^3 + 3t^2 - 3t + 1, 6_3 t^4 - 3t^3 + 5t^2 - 3t + 1", al "6_2" == "t^4 - 3t^3 + 3t^2 - 3t + 1" && al "6_3" == "t^4 - 3t^3 + 5t^2 - 3t + 1")
  , ("Determinant 1, 3, 5, 5, 7, 9, 11, 13", all (\(k, v) -> determinant (pdOf k) == v) [("0_1", 1), ("3_1", 3), ("4_1", 5), ("5_1", 5), ("5_2", 7), ("6_1", 9), ("6_2", 11), ("6_3", 13)])
  , ("Trefoil: 9 3-colourings; figure-eight: 3 (not 3-colourable), 25 5-colourings", colourings l3 3 == 9 && colourings (pdOf "4_1") 3 == 3 && colourings (pdOf "4_1") 5 == 25)
  , ("Whole table: |Delta(-1)| = |V(-1)| = det (3 separate computations agree), knots have V(1) = 1", all tableOk knots) ]
  ++ [ ("Whole table: PD code from the 3D curve = PD code in the table (same V and writhe)", all f knots) | Just f <- [curvesCheck] ]
  ++ [ ("Mirror swaps t <-> 1/t (left and right trefoils differ)", mEqual (jones r3) (jonesMirror <$> jones l3) && not (mEqual (jones r3) (jones l3))) ]
  where
    pdOf = kPd . knotByName
    j = jones . pdOf
    al = alexStr . alexander . pdOf
    l3 = pdOf "3_1"
    r3 = mirrorPD l3
    bor = pdOf "L6a4"
    bi = pdInfo bor
    lEqual' mv xs = case mv of
      Just v -> lEqual v (M.fromList xs)
      Nothing -> False
    tableOk k =
      let d = determinant (kPd k)
      in case jones (kPd k) of
           Nothing -> False
           Just v -> abs (pEval (alexander (kPd k)) (-1)) == d && jonesAbsSqAtMinus1 v == d * d
                       && (take 1 (kName k) == "L" || sum (M.elems v) == 1)   -- V(1) = 係数の和

-- 結果を端末に出す（テスト中は 1 行のまとめと失敗した項目だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(w, ok) -> putStrLn ((if ok then "OK  " else "NG  ") ++ w)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "NG ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- ---- 曲線 → 結び目の図（交点を見つけて PD 符号にする）
-- curves：輪ごとの点の列 [(x, y, z), ...]（閉じた折れ線）。x 右・y 上・z 手前（大きいほうが上を通る）
type P3 = (Float, Float, Float)

segHit :: P3 -> P3 -> P3 -> P3 -> Maybe (Float, Float)
segHit (x1, y1, _) (x2, y2, _) (x3, y3, _) (x4, y4, _)
  | abs d < 1e-12 = Nothing
  | t >= 0 && t < 1 && u >= 0 && u < 1 = Just (t, u)
  | otherwise = Nothing
  where
    d = (x2 - x1) * (y4 - y3) - (y2 - y1) * (x4 - x3)
    t = ((x3 - x1) * (y4 - y3) - (y3 - y1) * (x4 - x3)) / d
    u = ((x3 - x1) * (y2 - y1) - (y3 - y1) * (x2 - x1)) / d

data Seg = Seg { sgCi, sgI :: !Int, sgA, sgB :: !P3, sgX0, sgX1, sgY0, sgY1 :: !Float }

-- 交点を通るところ：輪 eCi の、点の番号 + 割合 eS（eSeg は線分の番号）
data End = End { eCi :: !Int, eS :: !Float, eSeg :: !Int }
-- 通過点：輪にそって並べたときの入ってくる辺 paIn、出ていく辺 paOut
data Pass = Pass { paS :: !Float, paK :: !Int, paOver :: !Bool, paIn, paOut :: !Int }
data Crossing = Crossing
  { crX, crY :: !Float, crO, crU :: !End, crSign :: !Int
  , crDv, crDu :: !(Float, Float)        -- 上・下のひもの向き
  , crPo, crPu :: !Pass }                 -- 上・下の通過点
data Diagram = Diagram { dgPd :: PD, dgCross :: [Crossing], dgPass :: [[Pass]], dgEdges :: !Int }

diagramOf :: [[P3]] -> Diagram
diagramOf = diagramOfWith False Nothing

-- over (ci, i) (ci', i')：交点で 1 本目（輪 ci の線分 i）が上なら True を返す関数（Nothing なら z で決める）
diagramOfWith :: Bool -> Maybe ((Int, Int) -> (Int, Int) -> Bool) -> [[P3]] -> Diagram
diagramOfWith open over curves = Diagram pd crosses passes base
  where
    lens = map length curves
    segs = sortOn sgX0
      [ Seg ci i a b (min ax bx) (max ax bx) (min ay by) (max ay by)
      | (ci, c) <- zip [0 ..] curves, let m = length c, let v = U.fromList c
      , i <- [0 .. (if open then m - 2 else m - 1)]
      , let a@(ax, ay, _) = v U.! i, let b@(bx, by, _) = v U.! ((i + 1) `mod` m) ]
    sweep [] = []
    sweep (sa : rest) = [r | sb <- takeWhile (\s -> sgX0 s <= sgX1 sa) rest, Just r <- [hit sa sb]] ++ sweep rest
    hit sa sb
      | sgY0 sb > sgY1 sa || sgY1 sb < sgY0 sa = Nothing
      | sgCi sa == sgCi sb && (dd <= 1 || (not open && dd == mlen - 1)) = Nothing
      | otherwise = case segHit (sgA sa) (sgB sa) (sgA sb) (sgB sb) of
          Nothing -> Nothing
          Just (ra, rb) ->
            let z3 (_, _, z) = z
                zA = z3 (sgA sa) + (z3 (sgB sa) - z3 (sgA sa)) * ra
                zB = z3 (sgA sb) + (z3 (sgB sb) - z3 (sgA sb)) * rb
                aOver = case over of
                  Just f -> f (sgCi sa, sgI sa) (sgCi sb, sgI sb)
                  Nothing -> zA > zB
                (so, su, ro, ru) = if aOver then (sa, sb, ra, rb) else (sb, sa, rb, ra)
                vec s = let (x0, y0, _) = sgA s; (x1, y1, _) = sgB s in (x1 - x0, y1 - y0)
                dv@(dvx, dvy) = vec so
                du@(dux, duy) = vec su
                (axx, ayy, _) = sgA sa
                (bxx, byy, _) = sgB sa
            in Just ( axx + (bxx - axx) * ra, ayy + (byy - ayy) * ra
                    , End (sgCi so) (fromIntegral (sgI so) + ro) (sgI so), End (sgCi su) (fromIntegral (sgI su) + ru) (sgI su)
                    , if dvx * duy - dvy * dux > 0 then 1 else -1, dv, du )
      where
        dd = abs (sgI sa - sgI sb)
        mlen = case drop (sgCi sa) lens of { (m : _) -> m; [] -> 0 }
    raw = sweep segs
    -- 輪ごとに、通過点（交点を通るところ）を曲線の向きに並べて辺に番号をつける
    nCurves = length curves
    byCurve = IM.fromListWith (flip (++))
      (concat [[(eCi o, [(eS o, k, True)]), (eCi u, [(eS u, k, False)])] | (k, (_, _, o, u, _, _, _)) <- zip [0 ..] raw])
    (base, passes) = mapAccumL label 0 [0 .. nCurves - 1]
    label b ci =
      let ps = sortOn (\(s, _, _) -> s) (IM.findWithDefault [] ci byCurve)
          m = length ps
      in (b + m, [Pass s k ov (b + (if q == 0 then m else q)) (b + q + 1) | (q, (s, k, ov)) <- zip [0 ..] ps])
    passOf = M.fromList [((paK p, paOver p), p) | p <- concat passes]
    getP k ov = M.findWithDefault (Pass 0 k ov 0 0) (k, ov) passOf
    crosses = [Crossing x y o u sg dv du (getP k True) (getP k False) | (k, (x, y, o, u, sg, dv, du)) <- zip [0 ..] raw]
    pd = [ if crSign c > 0 then (i, paOut po, k, paIn po) else (i, paIn po, k, paOut po)   -- i から反時計回りに次の端が j
         | c <- crosses, let pu = crPu c, let po = crPo c, let i = paIn pu, let k = paOut pu ]

-- 折れ線の長さの道具：plAt 長さ で点、plToLen (点の番号 + 割合) で長さ、plPiece a b で長さ a〜b の部分の点の列
data PathL = PathL { plL :: !Float, plAt :: Float -> Point, plToLen :: Float -> Float, plPiece :: Float -> Float -> [Point] }

pathOf :: Bool -> [Point] -> PathL
pathOf closed ptsL = PathL total at toLen piece
  where
    pts = U.fromList ptsL
    n = U.length pts
    segN = if closed then n else n - 1
    pt i = pts U.! (i `mod` max 1 n)
    cum = U.scanl' (+) 0 (U.generate (max 0 segN) (\i -> let (ax, ay) = pt i; (bx, by) = pt (i + 1) in sqrt ((bx - ax) ^ (2 :: Int) + (by - ay) ^ (2 :: Int))))
    cu i = cum U.! max 0 (min segN i)
    total = cu segN
    at s0 =
      let s | closed = s0 - fromIntegral (floor (s0 / total) :: Int) * total
            | otherwise = max 0 (min total s0)
          search lo hi | lo >= hi = lo
                       | otherwise = let m = (lo + hi + 1) `div` 2 in if cu m <= s then search m hi else search lo (m - 1)
          lo = search 0 (segN - 1)
          (ax, ay) = pt lo
          (bx, by) = pt (lo + 1)
          dl = cu (lo + 1) - cu lo
          f = (s - cu lo) / (if dl == 0 then 1 else dl)
      in (ax + (bx - ax) * f, ay + (by - ay) * f)
    toLen s = let i = max 0 (min (floor s) (segN - 1)) in cu i + (cu (i + 1) - cu i) * (s - fromIntegral i)
    piece a b = [at a] ++ go 1 ++ [at b]
      where
        go k | k > 2 * segN = []
             | not closed && k > segN = []
             | otherwise =
                 let s = cu (k `mod` segN) + fromIntegral (k `div` segN) * total
                 in if s >= b then [] else (if s > a then (pt k :) else id) (go (k + 1))

-- 下を通るところにすき間をあけた部分の列（gapS：すき間の位置（点の番号 + 割合）、gap：すき間の半分の長さ）
--   返り値 (q, 点の列)：q 番目のすき間のあとの部分（＝ 1 本の弧。色を変えたいときに使う）
gappedPieces :: Bool -> [Point] -> [Float] -> Float -> [(Int, [Point])]
gappedPieces closed pts gapS gap
  | length pts < 2 || plL p <= 0 = []
  | null gaps = [(0, if closed then pts ++ take 1 pts else pts)]
  | not closed =
      let step (s0, acc) (q, c) = (max s0 (c + gap), if c - gap > s0 then (q - 1, plPiece p s0 (c - gap)) : acc else acc)
          (sEnd, ps) = foldl' step (0, []) (zip [0 ..] gaps)
      in reverse ps ++ [(length gaps - 1, plPiece p sEnd (plL p)) | sEnd < plL p]
  | otherwise =
      [ (q, plPiece p a b) | (q, c, nxt) <- zip3 [0 ..] gaps (drop 1 gaps ++ map (+ plL p) (take 1 gaps))
      , let a = c + gap, let b = nxt - gap, b > a ]
  where
    p = pathOf closed pts
    gaps = sortOn id (map (plToLen p) gapS)

-- すき間つきで 1 本描く（太さ w、色 col）
drawGapped :: Float -> Color -> Bool -> [Point] -> [Float] -> Float -> Picture
drawGapped w col closed pts gapS gap = Color col (Pictures [thickPath w ps | (_, ps) <- gappedPieces closed pts gapS gap])

-- 結び目の図を描く（scr：画面の点の曲線、D：diagramOf の結果、cols：輪ごとの色）
toScreen :: [[P3]] -> Float -> Float -> Float -> [[P3]]
toScreen cs cx cy r = map (map (\(x, y, z) -> (cx + x * r, cy - y * r, z))) cs

drawDiagram :: [[P3]] -> Diagram -> [Color] -> Float -> Float -> Picture
drawDiagram scr dg cols w gap = Pictures
  [ drawGapped w (colAt ci) True [(x, y) | (x, y, _) <- c] [eS (crU x) | x <- dgCross dg, eCi (crU x) == ci] gap
  | (ci, c) <- zip [0 ..] scr ]
  where colAt ci = case drop (ci `mod` max 1 (length cols)) cols of { (c : _) -> c; [] -> white }

-- 点を回転（p5.js の画面と同じ回し方：rotateX(−rx) のあと rotateY(ry)）。返り値も x 右・y 上・z 手前
rotView :: Float -> Float -> P3 -> P3
rotView rx ry (x, y, z) = (x1, -(y1 * cx + z1 * sx), -y1 * sx + z1 * cx)
  where
    cy = cos ry
    sy = sin ry
    cx = cos rx
    sx = sin rx
    x1 = x * cy + z * sy
    z1 = -x * sy + z * cy
    y1 = -y

rotPts :: [[P3]] -> Float -> Float -> [[P3]]
rotPts cs rx ry = map (map (rotView rx ry)) cs

-- ---- 結び目の形（3D の曲線）。式があるものは式、ないものはフーリエ級数の係数（x, y, z ごとに (cos, sin) × 次数）
-- どれも、真上（z 軸）から見た図が最小の交点数になるように選んである（自己テストで PD 符号と照合）
fourier :: [(String, [[[(Float, Float)]]])]
fourier =
  [ ("5_2", [[[(-0.15, -0.14), (-0.53, 0.46), (0.21, -0.27), (0.05, 0.33), (0.06, 0.14), (0.03, -0.04), (-0.07, 0.02)], [(-0.19, 0.23), (-0.63, -0.15), (0.15, 0.12), (-0.32, -0.17), (-0.16, 0), (0.11, 0.1), (-0.02, 0.07)], [(0.07, -0.07), (0.13, 0), (-0.12, -0.06), (0.02, -0.05), (0.2, 0.32), (0.21, 0.14), (-0.02, 0.07)]]])
  , ("6_1", [[[(-0.27, 0.05), (-0.73, 0.2), (0.31, -0.11), (-0.23, 0.14), (0, 0.05), (0.01, -0.02), (-0.02, 0.08)], [(0.1, 0.23), (-0.04, -0.33), (0.2, 0.27), (-0.03, -0.13), (0.21, 0.18), (0.22, 0.15), (-0.02, -0.03)], [(-0.02, -0.02), (0.02, -0.01), (0.07, 0.05), (0.05, 0.04), (0.14, 0.06), (-0.18, -0.15), (-0.19, -0.2)]]])
  , ("6_2", [[[(0.1, -0.15), (0.61, -0.41), (-0.06, 0.12), (0.27, -0.02), (0, -0.13), (-0.05, -0.05), (0.02, -0.03)], [(-0.13, 0.2), (-0.37, -0.19), (0.13, 0.32), (0.33, -0.23), (0.18, 0.04), (-0.02, 0.08), (0.04, -0.09)], [(-0.09, 0), (0.09, 0.04), (-0.12, -0.09), (-0.16, 0.05), (0.29, -0.17), (0.23, 0.26), (0.11, -0.05)]]])
  , ("6_3", [[[(0.16, -0.21), (0.64, -0.07), (-0.23, 0.14), (0.34, 0.03), (0.21, 0.01), (-0.02, 0.02), (-0.03, -0.03)], [(-0.15, -0.25), (-0.51, 0.4), (-0.09, -0.11), (0.36, 0.17), (-0.04, 0.03), (-0.03, 0.16), (0.08, 0.06)], [(0.05, 0.07), (0.09, -0.1), (-0.01, 0.04), (-0.12, -0.13), (0.07, 0.05), (0.11, 0.28), (0.15, 0.08)]]])
  , ("L5a1", [ [[(-0.03, 0.01), (0.52, 0.3), (0, 0.01), (0.06, -0.01), (0, 0), (0.02, 0.07)], [(-1, 0.3), (-0.02, -0.01), (-0.05, 0.32), (0, 0), (-0.07, 0), (0, 0)], [(0.33, -0.05), (0.01, 0.01), (0.14, 0.26), (0, 0), (-0.07, -0.15), (0, 0)]]
             , [[(-0.3, 0.84), (-0.03, -0.05), (0.04, -0.02), (0.03, 0.08), (-0.01, -0.01), (0.01, -0.03)], [(-1.04, -0.31), (-0.09, 0.21), (0.09, 0.13), (-0.12, 0.06), (0.01, 0.06), (-0.02, 0)], [(0.42, 0.13), (0.12, -0.15), (0.13, 0.2), (-0.11, 0.03), (-0.01, 0.03), (-0.05, -0.01)]] ])
  ]

-- 名前の結び目の曲線（輪ごとに n 点）。大きさは xy の半径の最大が 1 になるようにそろえる
knotCurves :: String -> Int -> [[P3]]
knotCurves name n = map (map norm) cs
  where
    samp f = [f (fromIntegral i / fromIntegral n * 2 * pi) | i <- [0 .. n - 1]]
    fs cf t = case [sum [a * cos (fromIntegral h * t) + b * sin (fromIntegral h * t) | (h, (a, b)) <- zip [1 :: Int ..] d] | d <- cf] of
      [x, y, z] -> (x, y, z)
      _ -> (0, 0, 0)
    cs = case name of
      "0_1" -> [samp (\t -> (cos t, sin t, 0))]
      "3_1" -> [samp (\t -> (sin t + 2 * sin (2 * t), cos t - 2 * cos (2 * t), -sin (3 * t)))]
      "4_1" -> [samp (\t -> ((2 + cos (2 * t)) * cos (3 * t), (2 + cos (2 * t)) * sin (3 * t), sin (4 * t)))]
      "5_1" -> [samp (\t -> ((2 + cos (5 * t)) * cos (2 * t), (2 + cos (5 * t)) * sin (2 * t), sin (5 * t)))]
      "L2a1" -> [samp (\t -> (cos t - 0.55, sin t, -0.5 * sin t)), samp (\t -> (cos t + 0.55, 0.5 * sin t, sin t))]
      "L6a4" -> [ samp (\t -> (0.62 * cos a + cos t, 0.62 * sin a + sin t, 0.6 * cos (3 * t)))
                | k <- [0 .. 2 :: Int], let a = pi / 2 + fromIntegral k * 2 * pi / 3 ]
      _ -> case lookup name fourier of
        Just cfs -> map (samp . fs) cfs
        Nothing -> [samp (\t -> (cos t, sin t, 0))]          -- 形がわからないものは輪で
    allP = concat cs
    np = fromIntegral (max 1 (length allP))
    mx = sum [x | (x, _, _) <- allP] / np
    my = sum [y | (_, y, _) <- allP] / np
    r = max 1e-6 (foldl' max 0 [sqrt ((x - mx) ^ (2 :: Int) + (y - my) ^ (2 :: Int)) | (x, y, _) <- allP])
    norm (x, y, z) = ((x - mx) / r, (y - my) / r, z / r)

-- 自己テストの 19 番目：表の全部で、形（3D 曲線）から作った図の PD 符号 = 表の PD 符号（V とねじれ数が一致）
curvesMatchTable :: Knot -> Bool
curvesMatchTable k = length p == length (kPd k) && writhe p == writhe (kPd k) && mEqual (jones p) (jones (kPd k))
  where p = dgPd (diagramOf (knotCurves (kName k) 240))

-- 曲線の道具があるスケッチの自己テスト（19 項目）
selfTest :: [(String, Bool)]
selfTest = selfTestWith (Just curvesMatchTable)
