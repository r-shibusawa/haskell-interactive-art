-- ============================================
-- 08 M/G/1 とポラチェック＝ヒンチンの公式
-- サービス時間の分布 G が何でもいい窓口 1 つの待ち行列。平均はどれも 1 分なのに、ばらつきで待ち時間が変わる
--   Wq = ρ (1 + cs²) / (2(1 − ρ)) × E[S]　　cs² = 分散 / 平均²（ばらつきの大きさ）
--   4 種類の窓口に同じ客を流して比べる。リンドレーの式 W(次) = max(0, W + S − 到着の間隔) で 1 人ずつ計算
-- ←→ = ρ / ↑↓ = 1 フレームに計算する人数 / スペース = 一時停止 / R = 測り直す
-- ============================================
module Main (main) where

import Art
import Data.Array.Unboxed (UArray, accumArray, elems)
import Data.Foldable (toList)
import Data.List (foldl', mapAccumL)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import System.Random (StdGen, mkStdGen, newStdGen, random)
import Text.Printf (PrintfArg, printf)

-- 乱数の種を受け取り、サービス時間と次の種を返す
type Gen = StdGen -> (Double, StdGen)

data Desk = Desk
  { dName :: String
  , dCs2, dHue :: !Float
  , dGen :: Gen
  , dHist :: ![Int]          -- サービス時間の分布
  , dW, dS, dSumW :: !Double
  , dTrace :: !(Seq Float)   -- 最近の客の待ち時間
  }

data World = World
  { wRho :: !Float     -- ★ 利用率（平均サービス時間 1 なので λ = ρ）
  , wPerFrame :: !Int  -- ★ 1 フレームで計算する客の数
  , wPaused :: !Bool
  , wDesks :: ![Desk]
  , wCust :: !Int
  , wGen :: !StdGen
  }

main :: IO ()
main = runSketch (sketch "29-08 M/G/1 and Pollaczek-Khinchine" (World 0.8 400 False [] 0 (mkStdGen 1)))
  { skBack  = hsb 200 25 11
  , skStart = \_ -> setup
  , skStep  = \_ _ w -> pure (if wPaused w then w else runCustomers (wPerFrame w) w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

expG :: Double -> Gen
expG rate g = let (u, g1) = random g in (-log (1 - u) / rate, g1)

erlang4, hyper :: Gen
erlang4 g0 =
  let (a, g1) = expG 4 g0
      (b, g2) = expG 4 g1
      (c, g3) = expG 4 g2
      (d, g4) = expG 4 g3
  in (a + b + c + d, g4)
hyper g0 =   -- 超指数分布（cs² = 4）
  let p = 0.5 * (1 + sqrt (3 / 5))   -- 混ぜる割合
      (u, g1) = random g0 :: (Double, StdGen)
  in if u < p then expG (2 * p) g1 else expG (2 * (1 - p)) g1

setup :: IO World
setup = do
  g0 <- newStdGen
  let table =
        [ ("Constant (cs2 = 0)", 0, 330, \g -> (1, g))
        , ("Erlang-4 (cs2 = 0.25)", 0.25, 45, erlang4)
        , ("Exponential (cs2 = 1)", 1, 190, expG 1)
        , ("Hyperexp. (cs2 = 4)", 4, 0, hyper) ]
      build g (name, cs2, hue, gen) =
        let (g1, h) = histOf gen g in (g1, Desk name cs2 hue gen h 0 0 0 Seq.empty)
      (g2, ds) = mapAccumL build g0 table
  pure (resetSim (World 0.8 400 False ds 0 g2))

-- サービス時間 20000 個のヒストグラム（0〜4 分を 30 本に）
histOf :: Gen -> StdGen -> (StdGen, [Int])
histOf gen g0 =
  let go :: Int -> StdGen -> [Int] -> (StdGen, [Int])
      go 0 g acc = (g, acc)
      go n g acc =
        let (v, g1) = gen g
            b = floor (v / 4 * 30) :: Int
        in g1 `seq` go (n - 1) g1 (if b < 30 then b : acc else acc)
      (g2, bs) = go 20000 g0 []
      arr = accumArray (+) 0 (0, 29) [(b, 1) | b <- bs] :: UArray Int Int
  in (g2, elems arr)

resetSim :: World -> World
resetSim w =
  let (g1, ds) = mapAccumL (\g d -> let (s, g') = dGen d g in (g', d { dW = 0, dS = s, dSumW = 0, dTrace = Seq.empty })) (wGen w) (wDesks w)
  in w { wCust = 0, wDesks = ds, wGen = g1 }

theoryWq :: World -> Float -> Float
theoryWq w cs2 = wRho w * (1 + cs2) / (2 * (1 - wRho w))

runCustomers :: Int -> World -> World
runCustomers n w0 = go 0 (wGen w0) (wDesks w0)
  where
    rho = realToFrac (wRho w0) :: Double
    go :: Int -> StdGen -> [Desk] -> World
    go !k g ds
      | k >= n = w0 { wGen = g, wDesks = ds, wCust = wCust w0 + n }
      | otherwise =
          let (a, g1) = expG rho g                  -- 次の客までの間隔（4 つで共通）
              (g2, ds1) = mapAccumL (stepDesk k a) g1 ds
          in foldr seq () ds1 `seq` g2 `seq` go (k + 1) g2 ds1
    stepDesk k a g d =
      let w1 = max 0 (dW d + dS d - a)              -- ★ リンドレーの式
          (s, g1) = dGen d g
          tr | k `mod` 4 == 0 =
                 let t = dTrace d |> fl w1
                 in if Seq.length t > 300 then Seq.drop 1 t else t
             | otherwise = dTrace d
      in (g1, d { dW = w1, dS = s, dSumW = dSumW d + w1, dTrace = tr })

drawAll :: World -> Picture
drawAll w = Pictures
  ( concat (zipWith (drawColumn wmax) [0 ..] (wDesks w))
 ++ [ seg 1 ax (bx, by) (bx + bw * 4, by) ]
 ++ concat (zipWith bars [0 ..] (wDesks w))
 ++ [ txt bx (by - bh - 10) 14 (hsb 0 0 60) "Mean wait Wq: color = simulation, white line = P-K formula"
    , seg 1 ax (px, py) (px + pw, py), seg 1 ax (px, py) (px, py - ph)
    , seg 2 (hsb 45 90 100) (px, py - theoryWq w 0 / wmax * ph) (px + pw, py - min (theoryWq w 4 / wmax) 1 * ph) ]
 ++ [ dot (px + dCs2 d / 4 * pw) (py - min (mean d / wmax) 1 * ph) 14 (hsb (dHue d) 70 100) | d <- wDesks w ]
 ++ [ txt px (py - ph - 10) 14 (hsb 0 0 60) "x = cs2 (0 to 4), y = Wq: a straight line"
    , txt 20 40 18 (gray 1) ("rho = " ++ nf 2 (wRho w) ++ " (<- ->)  Customers " ++ show (wCust w) ++ " (" ++ show (wPerFrame w)
        ++ " per frame, Up/Down)  Space: pause  R: again")
    , txt 20 68 15 (hsb 0 0 60) "'Always 1 min' makes people wait half as long as exponential. With high variance, lines form behind the rare long job." ] )
  where
    wmax = theoryWq w 4 * 1.6
    ax = hsba 0 0 100 0.3
    -- 下左：棒グラフ（測定 vs 理論）
    bx = 60
    by = height - 60
    bh = height * 0.28
    bw = (width * 0.45) / 4
    mean d = fl (dSumW d) / fromIntegral (max (wCust w) 1)
    bars :: Float -> Desk -> [Picture]
    bars i d =
      let m = mean d
          t = theoryWq w (dCs2 d)
          h = min (m / wmax) 1 * bh
          h2 = min (t / wmax) 1 * bh
          cx = bx + i * bw + bw / 2
      in [ box (bx + i * bw + 10) (by - h) (bw - 20) h (hsb (dHue d) 60 90)
         , seg 2 (gray 1) (bx + i * bw + 4, by - h2) (bx + (i + 1) * bw - 4, by - h2)
         , txtC cx (by - h - 8) 14 (gray 1) (nf 2 m)
         , txtC cx (by + 20) 14 (hsb 0 0 60) ("Theory " ++ nf 2 t) ]
    -- 下右：Wq は cs² の 1 次関数
    px = width * 0.6
    py = height - 60
    pw = width * 0.34
    ph = height * 0.28

drawColumn :: Float -> Float -> Desk -> [Picture]
drawColumn wmax i d =
  txt x 112 15 (hsb (dHue d) 60 100) (dName d)
  : [ let h = fromIntegral c / pk * hh
      in box (x + b * cw / 30) (hy - h) (cw / 30 - 1) h (hsba (dHue d) 50 85 0.7)
    | (b, c) <- zip [0 ..] (dHist d) ]
 ++ [ txt x (hy + 18) 14 (hsb 0 0 60) "Service time, 0 to 4 min"
      -- 待ち時間の推移（最近の客）
    , seg 1 (hsba 0 0 100 0.25) (x, ty) (x + cw, ty)
    , path 1.2 (hsb (dHue d) 70 100) [ (x + j / 300 * cw, ty - min (v / (wmax * 2)) 1 * th) | (j, v) <- zip [0 ..] (toList (dTrace d)) ]
    , txt x (ty + 18) 14 (hsb 0 0 60) "Waits of recent customers" ]
  where
    colW = (width - 80) / 4
    x = 40 + i * colW
    cw = colW - 30
    hy = 230
    hh = 90
    pk = fromIntegral (maximum (1 : dHist d)) :: Float
    ty = height * 0.5
    th = 120

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> resetSim w { wRho = roundTo 100 (min 0.95 (wRho w + 0.05)) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetSim w { wRho = roundTo 100 (max 0.1 (wRho w - 0.05)) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wPerFrame = min 4000 (wPerFrame w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wPerFrame = max 1 (wPerFrame w `div` 2) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _ | c `elem` "rR" -> resetSim w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 4 つの窓口で Wq の比が 1 : 1.25 : 2 : 5 になっていることを確かめる（1 + cs² の比）
-- 2. wPerFrame を 1 にしてゆっくり見る。ばらつき大の窓口で長い待ちが「まとまって」起きるのはなぜ？
-- 3. ρ = 0.95 にすると、ばらつき大の窓口はなかなか理論値に落ち着かない。どれくらいの人数が必要？
-- 4. 現実の改善：サービス時間の平均を 10% 縮めるのと、ばらつきを半分にするのはどちらが効く？ 式で比べる

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Float -> Float -> Point
pt x y = (x - width / 2, height / 2 - y)

-- 左上の角が (x, y) の長方形
box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)

-- 中心 (x, y)、直径 d の円
dot :: Float -> Float -> Float -> Color -> Picture
dot x y d c = Color c (at (pt x y) (disc (d / 2)))

-- 太さ wd の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg wd c (x1, y1) (x2, y2) = Color c (thickLine wd (pt x1 y1) (pt x2 y2))

-- 太さ wd の折れ線
path :: Float -> Color -> [Point] -> Picture
path wd c ps = Color c (thickPath wd [pt x y | (x, y) <- ps])

-- 文字：(x, y) は左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 中央ぞろえの文字
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y s c str = txt (x - fromIntegral (length str) * s * 0.36) y s c str

-- p5 の nf(v, 1, d)：小数 d 桁の文字列
nf :: PrintfArg a => Int -> a -> String
nf d v = printf "%.*f" d v

-- シミュレーションの時刻は Double、描くときは Float
fl :: Double -> Float
fl = realToFrac

-- p5 の round(v * k) / k
roundTo :: Float -> Float -> Float
roundTo k v = fromIntegral (round (v * k) :: Int) / k
