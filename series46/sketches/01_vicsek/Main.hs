-- ============================================
-- 46-01 ヴィチェック模型 ─ ノイズに負けずに向きがそろうか
-- 粒子はみんな同じ速さで進み、半径 1 の中の仲間の「平均の向き」にそろえる。ただし毎回ノイズ η が入る
-- 秩序変数 φ = |平均の速度| / 速さ（0 = バラバラ、1 = 全員同じ向き）。η や密度を変えると φ が急に変わる（相転移）
-- 操作：←→ = ノイズ η / ↑↓ = 粒子の数（密度 ρ）/ S = η を自動で走査して φ(η) を描く / A = 全員同じ向きから / R = バラバラから
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Data.Bits (shiftL, shiftR, xor)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word (Word64)
import Numeric (showFFloat)

boxL :: Int
boxL = 20                   -- ★ 箱の大きさ（相互作用の半径 = 1 が単位）

v0 :: Float
v0 = 0.03                   -- ★ 1 ステップに進む距離

stepsPerFrame :: Int
stepsPerFrame = 3           -- ★ 1 フレームに何ステップ進めるか

data World = World
  { wN          :: !Int                  -- 粒子の数（ρ = N / L²）
  , wEta        :: !Float                -- ノイズの強さ η（0〜2π）
  , wXs, wYs    :: !(U.Vector Float)     -- 位置（0〜L）
  , wTh         :: !(U.Vector Float)     -- 向き
  , wSeed       :: !Seed                 -- 乱数の種
  , wPhiHist    :: ![Float]              -- φ の記録（新しい順、300 個まで）
  , wScan       :: ![(Float, Float, Float)]  -- φ(η) の点 (η, φ, ρ)
  , wSweeping   :: !Bool
  , wSweepEta   :: !Float
  , wSweepCount :: !Int
  , wSweepSum   :: !Float
  }

world0 :: World
world0 = World 400 2.0 U.empty U.empty U.empty 1 [] [] False 0 0 0

main :: IO ()
main = runSketch (sketch "46-01 Vicsek model" world0)
  { skBack  = hsb 258 45 12
  , skStart = \_ -> do
      s <- randomRIO (1, 2 ^ (62 :: Int))
      pure (resetParticles False world0 { wSeed = s })
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

-- 粒子を置き直す（aligned = 全員同じ向き）
resetParticles :: Bool -> World -> World
resetParticles aligned w = w { wXs = xs, wYs = ys, wTh = th, wSeed = nextSeed s, wPhiHist = [] }
  where
    n = wN w
    s = wSeed w
    lf = fromIntegral boxL
    xs = U.generate n (\i -> hashRand s (3 * i) * lf)
    ys = U.generate n (\i -> hashRand s (3 * i + 1) * lf)
    th = U.generate n (\i -> if aligned then 0.3 else hashRand s (3 * i + 2) * tau - pi)

-- ★ ヴィチェックの 1 ステップ：近くの仲間の平均の向き ＋ ノイズ、そして前に進む
vicsekStep :: World -> World
vicsekStep w = w { wXs = xs', wYs = ys', wTh = th', wSeed = nextSeed (nextSeed s) }
  where
    s = wSeed w
    xs = wXs w
    ys = wYs w
    th = wTh w
    n = U.length xs
    cos' = U.map cos th
    sin' = U.map sin th
    lf = fromIntegral boxL
    cellXY i = (floor (xs U.! i) `mod` boxL, floor (ys U.! i) `mod` boxL)
    grid = buildGrid (boxL * boxL) (U.generate n (\i -> let (cx, cy) = cellXY i in cx + cy * boxL))
    newTh i =
      let (cx, cy) = cellXY i
          xi = xs U.! i
          yi = ys U.! i
          cells = [ ((cx + gx + boxL) `mod` boxL) + ((cy + gy + boxL) `mod` boxL) * boxL | gy <- [-1, 0, 1], gx <- [-1, 0, 1] ]
          add (ax, ay) j =
            let dx0 = xs U.! j - xi
                dy0 = ys U.! j - yi
                dx = wrapD lf dx0   -- 周期境界（いちばん近い仲間の像）
                dy = wrapD lf dy0
            in if dx * dx + dy * dy <= 1 then (ax + cos' U.! j, ay + sin' U.! j) else (ax, ay)   -- 自分も含む
          (sx, sy) = foldl' (\acc c -> U.foldl' add acc (cellMembers grid c)) (0, 0) cells
      in atan2 sy sx + wEta w * (hashRand s i - 0.5)
    th' = U.generate n newTh
    s2 = nextSeed s
    move get dir i =
      let v = wrapF lf (get U.! i + v0 * dir (th' U.! i))
      in if v >= 0 && v < lf then v else hashRand s2 i * lf       -- 念のため（NaN や端ちょうど）
    xs' = U.generate n (move xs cos)
    ys' = U.generate n (move ys sin)

orderParam :: World -> Float
orderParam w
  | U.null (wTh w) = 0
  | otherwise = sqrt (sx * sx + sy * sy) / fromIntegral (U.length (wTh w))
  where
    sx = U.sum (U.map cos (wTh w))
    sy = U.sum (U.map sin (wTh w))

step :: Env -> World -> World
step env w0 = sweep w2
  where
    eR = if isDown env (SpecialKey KeyRight) then 0.02 else 0
    eL = if isDown env (SpecialKey KeyLeft) then -0.02 else 0
    w1 = w0 { wEta = clampF 0 6.28 (wEta w0 + eR + eL) }
    w1' = iterate vicsekStep w1 !! stepsPerFrame
    phi = orderParam w1'
    w2 = w1' { wPhiHist = take 300 (phi : wPhiHist w1') }
    -- ---- 自動走査：η を少しずつ上げ、落ち着かせてから φ の平均をとる
    sweep w
      | not (wSweeping w) = w
      | c >= 240 =
          let e' = wSweepEta w + 0.25
              w' = w { wScan = (wSweepEta w, sm / 120, rho w) : wScan w, wSweepEta = e', wSweepCount = 0, wSweepSum = 0 }
          in if e' > 5.01 then w' { wSweeping = False } else w' { wEta = e' }
      | otherwise = w { wSweepCount = c, wSweepSum = sm }
      where
        c = wSweepCount w + 1
        sm = if c > 120 then wSweepSum w + phi else wSweepSum w

rho :: World -> Float
rho w = fromIntegral (wN w) / fromIntegral (boxL * boxL)

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 箱の中の粒子（向きで色をつける。12 色に分けてまとめて描く）
  [ Color (hsbA 0 0 0 30) (rect (bx + bs / 2) (by + bs / 2) bs bs) ]
  ++ [ Color (hsb (fromIntegral b * 30) 70 100) (Pictures [ thickLine 2 (x, y) (x - cos t * 7, y - sin t * 7)
                                                            | (x, y, t) <- ps ])
     | (b, ps) <- buckets, not (null ps) ]
  ++ [ Color (hsbA 0 0 100 40) (Translate (bx + bs - 40) (by + 40) (Circle k))
     , txt (bx + bs - 64) (by + 46 + k) 14 (hsb 0 0 80) "radius 1" ]
  -- ---- 右：φ の時間変化と φ(η)
  ++ [ drawPlot (wPhiHist w) gx 120 gw 110 0 1 (hsb 45 80 100) ("phi over time (now " ++ fmt 3 phi ++ ")")
     , Color (hsbA 0 0 0 35) (rect (gx + gw / 2) (sy0 + sh / 2) gw sh) ]
  ++ [ seg 1 (hsb 0 0 50) (gx + fromIntegral e / 5 * gw, sy0 + sh) (gx + fromIntegral e / 5 * gw, sy0 + sh + 5) | e <- [0 .. 5 :: Int] ]
  ++ [ txt (gx + fromIntegral e / 5 * gw - 4) (sy0 + sh + 20) 14 (hsb 0 0 75) (show e) | e <- [0 .. 5 :: Int] ]
  ++ [ txt (gx + gw - 90) (sy0 + sh + 38) 14 (hsb 0 0 75) "noise eta ->"
     , txt gx (sy0 - 8) 15 (hsb 0 0 90) ("phi(eta): S to scan ("
         ++ (if wSweeping w then "scanning eta = " ++ fmt 2 (wSweepEta w) else "colour = density rho") ++ ")") ]
  ++ [ dot (hsb (wrapF 360 (r * 80)) 70 100) (gx + e / 5 * gw) (sy0 + sh - p * sh) 9 | (e, p, r) <- wScan w ]
  ++ [ dot (hsb 0 80 100) (gx + min (wEta w) 5 / 5 * gw) (sy0 + sh - phi * sh) 13
     , txt 20 36 18 (hsb 0 0 100) ("Left/Right: noise eta = " ++ fmt 2 (wEta w) ++ "   Up/Down: " ++ show (wN w)
                                   ++ " particles (rho = " ++ fmt 2 (rho w) ++ ")   S: scan eta   A: aligned   R: random")
     , txt 20 62 14 (hsb 0 0 70) "theta(t+1) = mean heading within radius 1 + eta * uniform(-1/2..1/2). Small eta / high density: phi -> 1" ]
  where
    bs = min (envH env - 120) (envW env * 0.55)
    bx = 20
    by = 90
    k = bs / fromIntegral boxL
    phi = orderParam w
    pts = [ (bx + x * k, by + y * k, t) | (x, y, t) <- U.toList (U.zip3 (wXs w) (wYs w) (wTh w)) ]
    bucketOf t = floor (wrapF tau t / tau * 12) `mod` 12 :: Int
    buckets = [ (b, [ p | p@(_, _, t) <- pts, bucketOf t == b ]) | b <- [0 .. 11] ]
    gx = bx + bs + 50
    gw = envW env - gx - 40
    sy0 = 300
    sh = max 120 (envH env - sy0 - 70)

event :: Event -> World -> World
event ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> resetParticles False w { wN = min 1600 (wN w + 100) }
  EventKey (SpecialKey KeyDown) Down _ _ -> resetParticles False w { wN = max 100 (wN w - 100) }
  EventKey (Char c) Down _ _
    | c `elem` "sS" -> resetParticles True w { wSweeping = not (wSweeping w), wSweepEta = 0, wEta = 0, wSweepCount = 0, wSweepSum = 0 }
    | c `elem` "aA" -> resetParticles True w
    | c `elem` "rR" -> resetParticles False w { wScan = [] }
  _ -> w

-- ---------- やってみよう ----------
-- 1. η = 0.3 と η = 4.5 で φ を比べよう。どこかの η で φ が急に下がる。そこが「相転移点」
-- 2. S で走査したあと ↑ で粒子を増やし（密度を上げ）、もう一度 S。曲線は右にずれる？左にずれる？
-- 3. η が小さいとき、R（バラバラ）から始めても、色（向き）の帯がだんだん 1 色になる様子を見よう
-- 4. v0 を 0.3 にすると？ 速いと仲間がよく入れ替わり、情報が遠くまで伝わる
-- 5. 00 のボイドとの違いは？ ヴィチェックは「整列」だけ。それでも集団の動きが生まれる

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = if w <= 1 then Color c (Line ps) else Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""

-- グラフ：記録 hist（新しい順、300 個まで）を左上 (x, y)、幅 w、高さ h の枠に lo〜hi で描く
drawPlot :: [Float] -> Float -> Float -> Float -> Float -> Float -> Float -> Color -> String -> Picture
drawPlot hist x y w h lo hi c title = Pictures
  [ Color (hsbA 0 0 0 35) (rect (x + w / 2) (y + h / 2) w h)
  , txt x (y - 8) 14 (hsb 0 0 85) title
  , if length hist < 2 then Blank
    else path 2 c [ (x + fromIntegral i / 299 * w, y + h - clampF 0 1 ((v - lo) / (hi - lo)) * h)
                  | (i, v) <- zip [0 :: Int ..] (reverse hist) ] ]

-- 乱数（xorshift64）。種を World に入れておけば skStep は純粋なまま
type Seed = Word64

nextSeed :: Seed -> Seed
nextSeed s0 = let s1 = s0 `xor` (s0 `shiftL` 13)
                  s2 = s1 `xor` (s1 `shiftR` 7)
              in s2 `xor` (s2 `shiftL` 17)

-- 種 s と番号 i から決まる 0〜1 の乱数（たくさんの粒子に一度に配るとき用）
hashRand :: Seed -> Int -> Float
hashRand s i =
  let z0 = s + fromIntegral i * 0x9E3779B97F4A7C15
      z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
      z3 = z2 `xor` (z2 `shiftR` 31)
  in fromIntegral (z3 `shiftR` 40) / 16777216

-- 空間ハッシュ：粒子 i の入るマス cells!i から「マスごとの粒子の並び」を作る（数えて並べる）
data Grid = Grid !(U.Vector Int) !(U.Vector Int)   -- マス c の粒子は order[start[c] .. start[c+1]-1]

buildGrid :: Int -> U.Vector Int -> Grid
buildGrid nc cells = Grid starts order
  where
    counts = U.accumulate (+) (U.replicate nc 0) (U.map (\c -> (c, 1)) cells)
    starts = U.scanl (+) 0 counts
    order = U.create $ do
      pos <- U.thaw (U.take nc starts)
      out <- M.new (U.length cells)
      forM_ [0 .. U.length cells - 1] $ \i -> do
        let c = cells U.! i
        p <- M.read pos c
        M.write out p i
        M.write pos c (p + 1)
      pure out

-- 周期境界での差：−L/2〜L/2 に入れる（Math.round を使うより速い）
wrapD :: Float -> Float -> Float
wrapD l d
  | d > l / 2 = d - l
  | d < -l / 2 = d + l
  | otherwise = d

cellMembers :: Grid -> Int -> U.Vector Int
cellMembers (Grid starts order) c = U.slice (starts U.! c) (starts U.! (c + 1) - starts U.! c) order
