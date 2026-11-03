-- ============================================
-- 08 ライデマイスター移動と三彩色性
-- 結び目の図を「弧」（下をくぐるところで切れた線）に分けて 3 色で塗る。ルール：どの交点でも 3 本の弧が「全部同じ色」か「全部ちがう色」
--   2 色以上使って塗れる ＝ 三彩色可能。図を動かしても（ライデマイスター移動）変わらないので、三葉結び目はほどけないと証明できる！
-- クリック = 弧の色を変える / 1 = 三葉結び目 / 2 = 8 の字結び目 / 3 = ねじれた輪（ほどける） / 4 = 五葉結び目 / 5 = (3,4) トーラス結び目
-- R = ライデマイスター移動 I（ねじれを足す・消す） / W = 移動 II（ひもを上に重ねる・戻す） / S = 塗り方を 1 つ見せる / C = 色を消す
-- ============================================
module Main (main) where

import Art
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as U

nS :: Int
nS = 320             -- ★ 曲線の点の数

width, height, sc, cxK, cyK :: Float
width = 1280
height = 800
sc = min width height * 0.11
cxK = width * 0.4
cyK = height * 0.55

-- 曲線の計算は p5.js（倍精度）と同じ交点が見つかるように Double で行う
type D3 = (Double, Double, Double)

-- | 交点：位置、上を通る位置、下を通る位置（点の番号、小数あり）
data Crossing = Crossing { cX, cY :: !Double, cOver, cUnder :: !Double }

data World = World
  { wKnot :: !Int
  , wKink, wKinkGoal, wFinger, wFingerGoal :: !Double
  , wPts :: !(U.Vector D3)
  , wCross :: ![Crossing]
  , wUnders :: ![Double]          -- 下をくぐる位置（小さい順）
  , wArcCol :: !(U.Vector Int)    -- 弧の色（−1 = まだ）
  , wNArcs :: !Int
  , wSolutions :: !(Maybe Int)    -- 正しい塗り方の数（Nothing = 多すぎて数えない）
  , wLastSig :: !(Int, Int)
  }

main :: IO ()
main = runSketch (sketch "27-08 Reidemeister Moves and Tricolorability" (rebuild world0))
  { skBack  = hsb 230 25 12
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = World 1 0 0 0 0 U.empty [] [] U.empty (-1) Nothing (-1, -1)

palette :: Int -> Color
palette k = case k of
  0 -> hsb 0 75 100
  1 -> hsb 130 70 90
  _ -> hsb 215 75 100

-- | ★ 結び目の式 (x, y, z)：z は高さ（交点でどちらが上か）
base :: Int -> Double -> D3
base knot t = case knot of
  1 -> (sin t + 2 * sin (2 * t), cos t - 2 * cos (2 * t), -sin (3 * t))
  2 -> ((2 + cos (2 * t)) * cos (3 * t), (2 + cos (2 * t)) * sin (3 * t), sin (4 * t))
  3 -> (2 * cos t, 1.5 * sin (2 * t), sin t)
  _ ->
    let pp = if knot == 4 then 2 else 3
        qq = if knot == 4 then 5 else 4
        r = 2 + cos (qq * t)
    in (r * cos (pp * t), r * sin (pp * t), -sin (qq * t))

-- | 図を動かす：ねじれ（移動 I）と指のように押し込む（移動 II）を足した曲線
buildCurve :: World -> U.Vector D3
buildCurve w =
  let n = fromIntegral nS :: Double
      kink = wKink w
      finger = wFinger w
      raw = U.generate nS (\i -> base (wKnot w) (fromIntegral i / n * 2 * pi))
      xs = U.map (\(x, _, _) -> x) raw
      ys = U.map (\(_, y, _) -> y) raw
      cx = U.sum xs / n
      cy = U.sum ys / n
      -- 移動 I の場所（i1）と移動 II の場所（いちばん外側の点 i2）
      radius i = let (x, y, _) = raw U.! i in sqrt ((x - cx) ^ (2 :: Int) + (y - cy) ^ (2 :: Int))
      i2 = U.foldl' (\b i -> if radius i > radius b then i else b) 0 (U.enumFromN 0 nS)
      w1 = floor (n * 0.05) :: Int
      i10 = floor (n * 0.12) :: Int
      i1 = if abs (i2 - i10) < w1 * 3 then (i2 + nS `div` 2) `mod` nS else i10
      (ax, ay, _) = raw U.! ((i1 - w1 + nS) `mod` nS)
      (bx, by, _) = raw U.! ((i1 + w1) `mod` nS)
      d0 = sqrt ((bx - ax) ^ (2 :: Int) + (by - ay) ^ (2 :: Int))
      d = if d0 == 0 then 0.1 else d0
      tx = (bx - ax) / d
      ty = (by - ay) / d
      (r1x, r1y, _) = raw U.! i1
      flipN = (r1x - cx) * (-ty) + (r1y - cy) * tx < 0     -- 外向きにループを出す
      nx = if flipN then ty else -ty
      ny = if flipN then -tx else tx
      (p2x, p2y, _) = raw U.! i2
      toCx = cx - p2x
      toCy = cy - p2y
      wrapI i c = fromIntegral (((i - c) + nS + nS `div` 2) `mod` nS) - n / 2 :: Double
      warp i (x, y, z) =
        let u = wrapI i i1 / fromIntegral w1
            (x1, y1, z1)
              | kink > 0 && abs u < 1 =
                  let s = sin (pi * u)
                      c = 1 + cos (pi * u)
                  in ( x + kink * d * (-1.6 * s * tx + 0.6 * c * nx)
                     , y + kink * d * (-1.6 * s * ty + 0.6 * c * ny)
                     , z + kink * 0.8 * u )
              | otherwise = (x, y, z)
            d2 = wrapI i i2
            g = exp (-(d2 * d2) / ((n * 0.035) ^ (2 :: Int)))
        in if finger > 0
             then (x1 + finger * g * toCx * 1.05, y1 + finger * g * toCy * 1.05, z1 + finger * g * 3)
             else (x1, y1, z1)
  in U.imap warp raw

-- | 交点をさがし、下をくぐる位置で区切って「弧」に分ける
findCrossings :: U.Vector D3 -> [Crossing]
findCrossings pts =
  [ Crossing (ax + (bx - ax) * t) (ay + (by - ay) * t)
             (if zi > zj then pi' else pj) (if zi > zj then pj else pi')
  | i <- [0 .. nS - 1]
  , let (ax, ay, az) = pts U.! i
  , let (bx, by, bz) = pts U.! ((i + 1) `mod` nS)
  , j <- [i + 2 .. nS - 1]
  , not (i == 0 && j == nS - 1)
  , let (cx, cy, cz) = pts U.! j
  , let (dx, dy, dz) = pts U.! ((j + 1) `mod` nS)
  , let den = (bx - ax) * (dy - cy) - (by - ay) * (dx - cx)
  , abs den >= 1e-12
  , let t = ((cx - ax) * (dy - cy) - (cy - ay) * (dx - cx)) / den
  , t >= 0, t < 1
  , let u = ((cx - ax) * (by - ay) - (cy - ay) * (bx - ax)) / den
  , u >= 0, u < 1
  , let zi = az + (bz - az) * t
  , let zj = cz + (dz - cz) * u
  , let pi' = fromIntegral i + t
  , let pj = fromIntegral j + u
  ]

-- | 曲線と弧を作り直す。弧の数が変わったら色を消し、塗り方の数を数え直す
rebuild :: World -> World
rebuild w0 =
  let pts = buildCurve w0
      crossings = findCrossings pts
      unders = sort (map cUnder crossings)
      m = max 1 (length unders)
      w1 = w0 { wPts = pts, wCross = crossings, wUnders = unders }
      w2 = if m /= wNArcs w0 then w1 { wNArcs = m, wArcCol = U.replicate m (-1) } else w1
      sig = (m, length crossings)
  in if sig /= wLastSig w2 then w2 { wLastSig = sig, wSolutions = countColorings w2 } else w2

-- | 位置 s（点の番号、小数あり）が何番目の弧か
arcOf :: [Double] -> Double -> Int
arcOf [] _ = 0
arcOf us s0 =
  let n = fromIntegral nS
      s = s0 - n * fromIntegral (floor (s0 / n) :: Int)
      k = length (takeWhile (< s) us)
  in k `mod` length us

crossingArcs :: [Double] -> Crossing -> (Int, Int, Int)
crossingArcs us c = (arcOf us (cOver c), arcOf us (cUnder c - 0.3), arcOf us (cUnder c + 0.3))

rule :: Int -> Int -> Int -> Bool
rule a b c = (a == b && b == c) || (a /= b && b /= c && a /= c)

---------------------------------------------------------------- 動き

stepWorld :: World -> World
stepWorld w =
  let toward x g v = x + signum (g - x) * min v (abs (g - x))
      k = toward (wKink w) (wKinkGoal w) 0.025
      f = toward (wFinger w) (wFingerGoal w) 0.02
  in if k /= wKink w || f /= wFinger w then rebuild w { wKink = k, wFinger = f } else w

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "12345" ->
        rebuild w { wKnot = fromEnum c - fromEnum '0', wKink = 0, wKinkGoal = 0, wFinger = 0, wFingerGoal = 0
                  , wLastSig = (-1, -1), wNArcs = -1 }
    | c `elem` "rR" -> w { wKinkGoal = if wKinkGoal w /= 0 then 0 else 1 }
    | c `elem` "wW" -> w { wFingerGoal = if wFingerGoal w /= 0 then 0 else 1 }
    | c `elem` "cC" -> w { wArcCol = U.map (const (-1)) (wArcCol w) }
    | c `elem` "sS" -> maybe w (\s -> w { wArcCol = s }) (findSolution w)
  EventKey (MouseButton LeftButton) Down _ (gx, gy) ->
    let dists = [ (d, i)
                | i <- [0 .. nS - 1]
                , let (x, y, _) = wPts w U.! i
                , let d = dist2 (scrX x, scrY y) (gx + width / 2, height / 2 - gy) ]
        (bd, best) = foldr (\a b -> if fst a <= fst b then a else b) (26, -1) dists
    in if bd < 26 && best >= 0
         then let a = arcOf (wUnders w) (fromIntegral best + 0.5)
                  v = fromMaybe (-1) (wArcCol w U.!? a)
              in w { wArcCol = wArcCol w U.// [(a, (v + 1) `mod` 3)] }
         else w
  _ -> w

scrX, scrY :: Double -> Float
scrX x = cxK + realToFrac x * sc
scrY y = cyK - realToFrac y * sc

---------------------------------------------------------------- 描画

drawWorld :: World -> Picture
drawWorld w =
  let us = wUnders w
      cols = wArcCol w
      colAt k = fromMaybe (-1) (cols U.!? k)
      n = fromIntegral nS :: Double
      isGap mid = any (\u -> abs (u - mid) < 1.6 || abs (u - mid + n) < 1.6 || abs (u - mid - n) < 1.6) us
      segPic i =
        let mid = fromIntegral i + 0.5
            (ax, ay, _) = wPts w U.! i
            (bx, by, _) = wPts w U.! ((i + 1) `mod` nS)
            k = colAt (arcOf us mid)
            c = if k >= 0 then palette k else hsb 0 0 70
        in if isGap mid then Blank
           else Color c (thickLine 7 (pt (scrX ax, scrY ay)) (pt (scrX bx, scrY by)))
      -- 交点のチェック
      checks = [ (c, colAt a, colAt b, colAt d) | c <- wCross w, let (a, b, d) = crossingArcs us c ]
      filled = [ (c, rule a b d) | (c, a, b, d) <- checks, a >= 0, b >= 0, d >= 0 ]
      okAll = length filled == length checks && all snd filled
      ringPic (c, ok) =
        at (pt (scrX (cX c), scrY (cY c))) (Color (hsb (if ok then 130 else 0) 80 100) (ring 15 3))
      allColored = U.all (>= 0) cols
      used = length (filter (\k -> U.any (== k) cols) [0, 1, 2])
      name = case wKnot w of
        1 -> "Trefoil knot"
        2 -> "Figure-eight knot"
        3 -> "Twisted loop (unknots)"
        4 -> "Cinquefoil knot"
        _ -> "(3,4) torus knot"
      x0 = width * 0.64
      solCount = wSolutions w
      tricolorable = maybe False (> 3) solCount
      resultText
        | okAll && used >= 2 = "Success! Tricolored (" ++ show used ++ " colors)"
        | okAll = "Rule OK, but only 1 color"
        | otherwise = "A crossing breaks the rule (red)"
  in Pictures $
       map segPic [0 .. nS - 1]
       ++ map ringPic filled
       ++ [ txt x0 150 26 (gray 1) name
          , txt x0 196 18 (gray 1) ("Crossings " ++ show (length (wCross w)) ++ "   Arcs " ++ show (max 1 (length us)))
          , txt x0 232 18 (gray 1) ("Valid 3-colorings: " ++ maybe "too many to count" show solCount)
          , txt x0 258 14 (gray 0.65) "Includes 3 one-color ones. Invariant!" ]
       ++ [ txt x0 310 16 (hsb (if okAll && used >= 2 then 130 else 0) 70 100) resultText
          | allColored && not (null (wCross w)) ]
       ++ [ txt x0 350 16 (hsb (if tricolorable then 130 else 30) 70 100)
              (if tricolorable then "-> Tricolorable: cannot be untied!" else "-> Not tricolorable (not a trefoil)")
          , txt 20 40 15 (gray 1) ("Click: arc color  1-5: knot  R: move I (kink)" ++ onOff (wKinkGoal w)
                                   ++ "  W: move II (overlap)" ++ onOff (wFingerGoal w) ++ "  S: a solution  C: clear")
          , txt 20 68 14 (gray 0.6) "Rule: at each crossing, the over arc and 2 under arcs are all the same or all different. Green ok, red bad."
          ]
  where onOff g = if g /= 0 then " ON" else ""

-- ---------- やってみよう ----------
-- 1. 三葉結び目を自分の手で 3 色に塗ろう。弧は何本？ 交点は何個？
-- 2. 8 の字結び目は何をしても 1 色しか成り立たない。塗り方の数が 3 のままなのを確かめよう
-- 3. R と W で図を変えても「塗り方の数」は変わらない（9 か 3）。これが「不変量」
-- 4. 五葉結び目は三彩色できない。でも 5 色版（五彩色）なら？ rule を「a + b ≡ 2c (mod 5)」に変えて試そう

-- ==================================================
-- ここから下は「塗り方を数える道具」（3 の弧の数乗 通りを全部ためす）
-- ==================================================

-- | 通し番号 m を 3 進法で読んだ i 桁目を、弧 i の色とする
digit :: Int -> Int -> Int
digit m i = (m `quot` (3 ^ i)) `rem` 3

validColoring :: [(Int, Int, Int)] -> Int -> Bool
validColoring triples m = all (\(a, b, d) -> rule (digit m a) (digit m b) (digit m d)) triples

triplesOf :: World -> [(Int, Int, Int)]
triplesOf w = map (crossingArcs (wUnders w)) (wCross w)

countColorings :: World -> Maybe Int
countColorings w
  | wNArcs w > 12 = Nothing
  | otherwise =
      let triples = triplesOf w
      in Just (length (filter (validColoring triples) [0 .. 3 ^ wNArcs w - 1]))

findSolution :: World -> Maybe (U.Vector Int)
findSolution w =
  let n = wNArcs w
      triples = triplesOf w
      total = 3 ^ min n 12 :: Int
      twoColors m = any (\i -> digit m i /= digit m 0) [1 .. n - 1]
  in case filter (\m -> twoColors m && validColoring triples m) [0 .. total - 1] of
       (m : _) -> Just (U.generate n (digit m))
       [] -> Nothing

pt :: (Float, Float) -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = let (gx, gy) = pt (x, y) in label gx gy s c str
