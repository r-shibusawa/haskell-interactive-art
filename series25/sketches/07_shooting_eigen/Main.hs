-- ============================================
-- 07 固有値を数値で求める（シューティング法）
-- 好きな V(x) をマウスで描くと、その中で許されるエネルギー（固有値）を探して ψn を描く
--   ψ'' = 2(V − E) ψ を左端から Numerov 法で積分 → 節（ψ = 0 の点）の数を数える → 二分法で E を絞りこむ
-- ドラッグ = V(x) を描く / 1 = 調和 / 2 = 箱 / 3 = 二重井戸 / 4 = 周期的な山（結晶） / 5 = 斜面 / C = 平らに / ↑↓ = 求める準位の数 / P = ψ ⇔ |ψ|²
-- ※ Numerov 法は 1 との小さな差を積み重ねるので、Float では桁が足りない。ここだけ Double で計算する（p5.js の数も Double）
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Numeric (showFFloat)

nN :: Int
nN = 400             -- ★ 格子点の数

xR :: Double
xR = 4               -- x の範囲 −4 〜 4（両端は無限に高い壁）

vMax :: Double
vMax = 10            -- 描ける V の最大

h :: Double
h = 2 * xR / fromIntegral (nN - 1)

type Field = U.Vector Double

data Level = Level { lvE :: !Double, lvPsi :: !Field }

data World = World
  { wV      :: !Field
  , wLevels :: ![Level]
  , wCount  :: !Int      -- ★ 求める準位の数
  , wProb   :: !Bool
  , wDirty  :: !Bool
  , wLastJ  :: !Int      -- ドラッグ中の前の点（なければ −1）
  , wIters  :: !Int      -- 積分した回数
  }

main :: IO ()
main = runSketch (sketch "25-07 Shooting Method" (preset 1 (World U.empty [] 5 False True (-1) 0)))
  { skBack  = hsb 30 25 10
  , skStep  = \env _ w -> pure (stepW env w)
  , skDraw  = drawW
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

xAt :: Int -> Double
xAt j = -xR + fromIntegral j * h

preset :: Int -> World -> World
preset k w = w { wV = U.generate nN (\j -> min (shape k (xAt j)) vMax), wDirty = True }

shape :: Int -> Double -> Double
shape k x = case k of
  1 -> 0.5 * x * x
  3 -> 0.35 * sqD (x * x - 4)
  4 -> 3 * (1 - cos (pi * 2 * x)) / 2 + 0.02
  5 -> 1.2 * (x + xR)
  _ -> 0

fAt :: Field -> Double -> Int -> Double
fAt v e j = 1 + (h * h / 12) * 2 * (e - v `U.unsafeIndex` j)

-- ★ Numerov 法で左端（ψ = 0）から右端まで積分し、「符号が変わった回数」を返す
--   ψ(j+1) = [ (12 − 10 f(j)) ψ(j) − f(j−1) ψ(j−1) ] / f(j+1)、 f = 1 + h²/12 · 2(E − V)
countNodes :: Field -> Double -> Int
countNodes v e = go 1 0 1e-6 0
  where
    go :: Int -> Double -> Double -> Int -> Int
    go !j !p0 !p1 !nodes
      | j >= nN - 1 = nodes
      | otherwise =
          let p2 = ((12 - 10 * fAt v e j) * p1 - fAt v e (j - 1) * p0) / fAt v e (j + 1)
              nodes' = if (p2 < 0 && p1 > 0) || (p2 > 0 && p1 < 0) then nodes + 1 else nodes
          in if abs p2 > 1e12                    -- 大きくなりすぎたら全体を縮める（形は同じ）
               then go (j + 1) (p1 * 1e-12) (p2 * 1e-12) nodes'
               else go (j + 1) p1 p2 nodes'

-- 同じ積分をして ψ を全部残す
shootKeep :: Field -> Double -> Field
shootKeep v e = runST $ do
  psi <- MV.replicate nN 0
  MV.unsafeWrite psi 1 1e-6
  integrate psi v e
  U.unsafeFreeze psi

integrate :: MV.STVector s Double -> Field -> Double -> ST s ()
integrate psi v e = go 1 0 1e-6
  where
    go !j !p0 !p1
      | j >= nN - 1 = pure ()
      | otherwise = do
          let p2 = ((12 - 10 * fAt v e j) * p1 - fAt v e (j - 1) * p0) / fAt v e (j + 1)
          if abs p2 > 1e12
            then do
              forM_ [0 .. j] (MV.unsafeModify psi (* 1e-12))
              MV.unsafeWrite psi (j + 1) (p2 * 1e-12)
              go (j + 1) (p1 * 1e-12) (p2 * 1e-12)
            else do
              MV.unsafeWrite psi (j + 1) p2
              go (j + 1) p1 p2

-- ★ n 番目の固有値：右端までの符号変化の数 = E より下にある固有値の数（スツルムの定理）
--   その数が n 以下 → E は低すぎ、n+1 以上 → 高すぎ。二分法で 50 回
findLevel :: Field -> Int -> (Level, Int)
findLevel v n =
  let lo0 = U.minimum v
      grow :: Double -> Int -> (Double, Int)
      grow hi c
        | countNodes v hi <= n && hi < 2000 = grow (lo0 + (hi - lo0) * 2) (c + 1)
        | otherwise = (hi, c + 1)
      (hi0, shots) = grow (lo0 + 2) 0
      bisect :: Int -> Double -> Double -> (Double, Double)
      bisect 0 lo hi = (lo, hi)
      bisect it lo hi =
        let mid = (lo + hi) / 2
        in if countNodes v mid <= n then bisect (it - 1) mid hi else bisect (it - 1) lo mid
      (lo1, hi1) = bisect 50 lo0 hi0
      e = (lo1 + hi1) / 2
  in (Level e (cleanPsi v e (shootKeep v e)), shots + 51)

-- 右の「入れない領域」（V > E）で |ψ| がまた大きくなる所は、E の誤差で指数関数的に伸びた分なので 0 にする
-- そのあと ψ を正規化（∫ψ² dx = 1）
cleanPsi :: Field -> Double -> Field -> Field
cleanPsi v e psi =
  let jt = lastInside (nN - 1)
      lastInside j | j > 0 && v U.! j > e = lastInside (j - 1)
                   | otherwise = j
      -- jt より右で |ψ| がいちばん小さい場所
      jm = fst (foldl (\(bj, bp) j -> let p = abs (psi U.! j) in if p < bp then (j, p) else (bj, bp))
                      (nN - 1, 1 / 0) [jt .. nN - 1])
      cut = U.imap (\j p -> if j >= jm then 0 else p) psi
      s = U.sum (U.map (\p -> p * p * h) cut)
  in if s > 0 then U.map (/ sqrt s) cut else cut

solve :: World -> World
solve w =
  let rs = map (findLevel (wV w)) [0 .. wCount w - 1]
  in w { wLevels = map fst rs, wIters = sum (map snd rs), wDirty = False }

-- マウスで V を描く（前の点との間も埋める）
stepW :: Env -> World -> World
stepW env w =
  let (mx, my) = mouseP env
      left = 60
      wd = envW env - 380
      base = envH env - 60
      eU = (envH env - 200) / realToFrac vMax
      drawing = envDown env && my > 100 && mx > left && mx < left + wd
      w1 | drawing =
             let j = max 0 (min (nN - 1) (round ((mx - left) / wd * fromIntegral (nN - 1))))
                 val = realToFrac (clampF 0 (realToFrac vMax) ((base - my) / eU)) :: Double
                 a = if wLastJ w < 0 then j else wLastJ w
                 spots = [min a j .. max a j] ++ [max 0 (j - 3) .. min (nN - 1) (j + 3)]
             in w { wV = wV w U.// [ (k, val) | k <- spots ], wLastJ = j, wDirty = True }
         | otherwise = w { wLastJ = -1 }
  in if wDirty w1 && not (envDown env) then solve w1 else w1

drawW :: Env -> World -> Picture
drawW env w =
  let width = envW env
      height = envH env
      left = 60
      wd = width - 380
      base = height - 60
      eU = (height - 200) / realToFrac vMax
      sx j = left + fromIntegral j / fromIntegral (nN - 1) * wd :: Float
      vTop j = base - realToFrac (wV w U.! j) * eU
      -- V(x)（凹んだ形も塗れるよう、細い台形に分ける）
      potPic = Color (hsb 0 0 28) $ Pictures
        [ Polygon (map (uncurry (pt env)) [(sx j, base), (sx (j + 1), base), (sx (j + 1), vTop (j + 1)), (sx j, vTop j)])
        | j <- [0 .. nN - 2], vTop j < base - 0.3 || vTop (j + 1) < base - 0.3 ]
      hueOf n = fromIntegral n * 55 + 10 :: Float
      -- 準位と ψn
      levelPic (n, lv) =
        let y = base - realToFrac (lvE lv) * eU
            val j = let p = realToFrac (lvPsi lv U.! j) :: Float in if wProb w then p * p * 30 else p * 40
        in if y < 90 then [] else
           [ seg env 1 (hsba (hueOf n) 70 100 0.4) (left, y) (left + wd, y)
           , curve env 2 (hsb (hueOf n) 70 100) [ (sx j, y - val j) | j <- [0, 2 .. nN - 1] ] ]
      levels = zip [0 :: Int ..] (wLevels w)
      -- 右：数値
      rx = width - 305
      rows = 152 + fromIntegral (wCount w) * 26 :: Float
  in Pictures $
       [ potPic
       , seg env 3 (hsb 0 0 70) (left, base) (left, 100)
       , seg env 3 (hsb 0 0 70) (left + wd, base) (left + wd, 100) ]
       ++ (if wDirty w then [] else concatMap levelPic levels)
       ++ [ txt env rx 120 16 white "Eigenvalues found" ]
       ++ (if wDirty w then [] else
            [ txt env rx (152 + fromIntegral n * 26) 16 (hsb (hueOf n) 70 100) ("E" ++ show n ++ " = " ++ nf 4 (realToFrac (lvE lv)))
            | (n, lv) <- levels ])
       ++ [ txt env rx (rows + 10) 14 (gray 0.6) "Harmonic (1) theory:"
          , txt env rx (rows + 30) 14 (gray 0.6) "En = n + 1/2"
          , txt env rx (rows + 54) 14 (gray 0.6) "Box (2) theory:"
          , txt env rx (rows + 74) 14 (gray 0.6) "En = (n+1)^2 pi^2/(2*8^2)"
          , txt env rx (rows + 94) 14 (gray 0.6) ("= " ++ nf 4 (pi * pi / 128) ++ " x (n+1)^2")
          , txt env rx (rows + 124) 14 (gray 0.6) ("Integrations: " ++ show (wIters w))
          , txt env 20 26 16 white ("Drag: draw V   1-5: examples   C: flat   Up/Down: levels " ++ show (wCount w) ++ "   P: " ++ (if wProb w then "|psi|^2" else "psi"))
          , txt env 20 48 14 (gray 0.6) "Level n has exactly n nodes. Only an E with psi = 0 at the right end is allowed,"
          , txt env 20 68 14 (gray 0.6) "so the energies come in separate steps."
          ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '5' -> preset (fromEnum c - fromEnum '0') w
    | toLower c == 'c' -> preset 2 w
    | toLower c == 'p' -> w { wProb = not (wProb w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wCount = min 10 (wCount w + 1), wDirty = True }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wCount = max 1 (wCount w - 1), wDirty = True }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1（調和）で E0〜E4 が 0.5, 1.5, 2.5… になっているか確かめよう。ずれは nN を増やすと小さくなる
-- 2. 3（二重井戸）で E0 と E1 がほとんど同じ値になる。この差がトンネルの速さ（03 の 5、13 の 3）
-- 3. 4（周期的な山）で準位を 10 にすると、近い準位がかたまって並ぶ = 結晶のエネルギーバンドのはじまり
-- 4. 箱の真ん中に細い壁を描いて、壁を高くしていくと準位がどう 2 つずつ組になるか観察しよう

-- ==================================================
-- ここから下は「道具」
-- ==================================================
sqD :: Double -> Double
sqD x = x * x

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = pt env x y in label gx gy size c s

seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c (x1, y1) (x2, y2) = Color c (thickLine w (pt env x1 y1) (pt env x2 y2))

curve :: Env -> Float -> Color -> [Point] -> Picture
curve env w c ps = Color c (thickPath w (map (uncurry (pt env)) ps))
