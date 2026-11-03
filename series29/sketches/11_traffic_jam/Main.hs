-- ============================================
-- 11 交通流と渋滞 ─ ナーゲル＝シュレッケンベルク模型
-- 1 車線の環状道路をマス目に区切り、車は毎ステップ 4 つのルールで動く（セル・オートマトン）
--   ① 加速 v+1（最大 vmax）② 前の車にぶつからないよう減速　③ 確率 p でなんとなく減速　④ v マス進む
--   事故もボトルネックもないのに、ルール③だけで「自然渋滞（幽霊渋滞）」が生まれ、後ろへ伝わっていく
-- ↑↓ = 車の密度 / ←→ = でたらめ減速の確率 p / F = 基本図をまとめて描く / スペース = 一時停止 / R = 並べ直す
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Array (Array, listArray, (!))
import Data.Foldable (toList)
import Data.List (mapAccumL)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import System.Random (StdGen, mkStdGen, newStdGen, random)
import Text.Printf (PrintfArg, printf)

roadL, vmax, stepsPerFrame, stRows :: Int
roadL = 300          -- ★ 道路のマスの数
vmax = 5             -- ★ 最高速度（マス/ステップ）
stepsPerFrame = 1
stRows = 260         -- 時空図に残すステップ数

data Car = Car { cX, cV :: !Int }

data World = World
  { wDensity :: !Float     -- ★ 車の密度（台/マス）
  , wP :: !Float           -- ★ でたらめに減速する確率
  , wPaused :: !Bool
  , wCars :: ![Car]        -- 位置の順に並んでいる
  , wRow :: !Int
  , wSt :: !(U.Vector Word8)   -- 時空図：0 = 車なし、1 + v = 速度 v の車
  , wFd :: ![(Float, Float)]   -- 基本図の点（密度, 流量）
  , wSweep :: ![Float]         -- これから測る密度
  , wLive :: !(Seq Float)
  , wGen :: !StdGen
  }

main :: IO ()
main = runSketch (sketch "29-11 Traffic Jams" world0)
  { skBack  = hsb 100 15 10
  , skStart = \_ -> (\g -> resetRoad world0 { wGen = g }) <$> newStdGen
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = World 0.15 0.25 False [] 0 (U.replicate (roadL * stRows) 0) [] [] Seq.empty (mkStdGen 1)

placeCars :: Int -> [Car]
placeCars n = [ Car (i * roadL `div` n) 0 | i <- [0 .. n - 1] ]

resetRoad :: World -> World
resetRoad w =
  let n = max 1 (round (wDensity w * fromIntegral roadL))
  in w { wCars = placeCars n, wSt = U.replicate (roadL * stRows) 0, wRow = 0, wLive = Seq.empty }

-- ★ ナーゲル＝シュレッケンベルクの 4 つのルール（cars は位置の順に並んでいる）
nasch :: Float -> StdGen -> [Car] -> ([Car], Int, StdGen)
nasch p g0 cs =
  let n = length cs
      ahead = drop 1 cs ++ take 1 cs
      rule g (me, next) =
        let gap | n == 1 = roadL - 1
                | otherwise = (cX next - cX me - 1 + roadL) `mod` roadL
            v1 = min (min (cV me + 1) vmax) gap        -- ① 加速 ② ぶつからないように
            (u, g1) = random g :: (Float, StdGen)
        in if v1 > 0 && u < p                          -- ③ でたらめ減速
             then (g1, v1 - 1)
             else (if v1 > 0 then g1 else g, v1)
      (g2, vs) = mapAccumL rule g0 (zip cs ahead)
      moved = zipWith (\c v -> Car ((cX c + v) `mod` roadL) v) cs vs   -- ④ 進む
      total = sum vs
      -- 位置の順に並べ直す（環状なので 1 台だけ先頭に回ることがある）
      firstIdx = snd (minimum (zip (map cX moved) [0 :: Int ..]))
      (front, back) = splitAt firstIdx moved
  in (back ++ front, total, g2)

-- 密度 d の道路を別にシミュレーションして、基本図の点を 1 つ測る
measure :: Float -> Float -> StdGen -> (Float, Float, StdGen)
measure p d g0 =
  let n = round (d * fromIntegral roadL) :: Int
      run :: Int -> [Car] -> Int -> StdGen -> ([Car], Int, StdGen)
      run 0 cs m g = (cs, m, g)
      run k cs !m g = let (cs1, mv, g1) = nasch p g cs
                      in forceCars cs1 `seq` run (k - 1) cs1 (m + mv) g1
      (warm, _, g1) = run 150 (placeCars n) 0 g0
      (_, total, g2) = run 150 warm 0 g1
  in (fromIntegral n / fromIntegral roadL, fromIntegral total / 150 / fromIntegral roadL, g2)

forceCars :: [Car] -> ()
forceCars = foldr seq ()

stepWorld :: World -> World
stepWorld w0 = sweepOne (if wPaused w0 then w0 else iterate stepRoad w0 !! stepsPerFrame)

stepRoad :: World -> World
stepRoad w =
  let (cars, moved, g1) = nasch (wP w) (wGen w) (wCars w)
      live = wLive w |> (fromIntegral moved / fromIntegral roadL)
      row = wRow w
      -- 時空図に 1 行書く（横 = 道路の位置、縦 = 時間）
      clearRow = [ (row * roadL + x, 0) | x <- [0 .. roadL - 1] ]
      carsRow = [ (row * roadL + cX c, fromIntegral (cV c + 1)) | c <- cars ]
  in forceCars cars `seq`
     w { wCars = cars, wGen = g1, wLive = if Seq.length live > 200 then Seq.drop 1 live else live
       , wSt = wSt w U.// (clearRow ++ carsRow), wRow = (row + 1) `mod` stRows }

-- ★ いろいろな密度で別にシミュレーションして基本図の点を集める（1 フレームに 1 つの密度）
sweepOne :: World -> World
sweepOne w = case wSweep w of
  [] -> w
  (d : rest) ->
    let (dn, q, g1) = measure (wP w) d (wGen w)
    in dn `seq` q `seq` w { wFd = (dn, q) : wFd w, wSweep = rest, wGen = g1 }

avgV :: World -> Float
avgV w = fromIntegral (sum (map cV (wCars w))) / fromIntegral (max (length (wCars w)) 1)

-- 速度の色（赤 = 止まっている、緑 = 最高速度）
speedColor :: Int -> Color
speedColor v = hsb (fromIntegral v / fromIntegral vmax * 120) 90 100

speedRGB :: Array Int (Word8, Word8, Word8)
speedRGB = listArray (0, vmax + 1) ((20, 20, 20) : [ rgb8 (speedColor v) | v <- [0 .. vmax] ])
  where
    rgb8 c = let (r, g, b, _) = rgbaOfColor c in (q r, q g, q b)
    q x = round (x * 255)

drawAll :: World -> Picture
drawAll w = Pictures [drawRing w, drawSpaceTime w, drawDiagram w, hud]
  where
    hud = Pictures
      [ txt 20 40 18 (gray 1) ("Density " ++ nf 2 (wDensity w) ++ " (Up/Down)  Random braking p = " ++ nf 2 (wP w) ++ " (<- ->)  F: sweep  Space: pause  R: reset")
      , txt 20 68 15 (hsb 0 0 60) "Up to density 0.1 everyone runs at top speed. Above 0.15, one driver's braking chains backward and cars stop." ]

-- 環状道路
drawRing :: World -> Picture
drawRing w = Pictures
  ( hoop cx cy (r * 2) 22 (hsb 0 0 30)
  : [ let a = fromIntegral (cX c) / fromIntegral roadL * tau - pi / 2
      in Color (speedColor (cV c)) (at (pt (cx + r * cos a) (cy + r * sin a))
           (Rotate ((a + pi / 2) * 180 / pi) (rectangleSolid 8 12)))
    | c <- wCars w ]
 ++ [ txtC cx (cy - 6) 15 (gray 1) (show (length (wCars w)) ++ " cars")
    , txtC cx (cy + 18) 15 (gray 1) ("Mean speed " ++ nf 2 (avgV w))
    , txt (cx - r) (cy + r + 36) 14 (hsb 0 0 60) "Red = stopped, green = top speed" ] )
  where
    cx = width * 0.2
    cy = height * 0.52
    r = min (width * 0.16) (height * 0.32)

-- 時空図（上が古い、下が新しい）
drawSpaceTime :: World -> Picture
drawSpaceTime w = Pictures
  [ at (pt (sx + sw / 2) (sy + sh / 2))
      (Scale (sw / fromIntegral roadL) (sh / fromIntegral stRows)
        (pixelImage roadL stRows 1 (\x y ->
           speedRGB ! fromIntegral (st U.! (((row + y) `mod` stRows) * roadL + x)))))
  , txt sx (sy + sh + 18) 14 (hsb 0 0 60) "Space-time: x = road, down = newer. Red bands = jams, moving backward"
  ]
  where
    st = wSt w
    row = wRow w
    sx = width * 0.42
    sy = 110
    sw = width * 0.55
    sh = height * 0.42

-- 基本図（密度と流量）
drawDiagram :: World -> Picture
drawDiagram w = Pictures
  ( [ seg 1 ax (px, py) (px + pw, py), seg 1 ax (px, py) (px, py - ph) ]
 ++ [ dot (px + d * pw) (py - f / qmax * ph) 7 (hsba 190 60 100 0.8) | (d, f) <- wFd w ]
 ++ [ dot (px + dens * pw) (py - q / qmax * ph) 14 (hsb 45 90 100)
    , txt px (py - ph - 8) 14 (hsb 0 0 60) "x = density (0 to 1), y = flow. Yellow = now, cyan = F sweep"
    , txt rx (py - ph + 20) 15 (gray 1) ("Flow " ++ nf 3 q ++ "  Density " ++ nf 3 dens)
    , txt rx (py - ph + 46) 15 (gray 1) "Flow = density x mean speed"
    , txt rx (py - ph + 76) 14 (hsb 0 0 60) "Left slope: free flow"
    , txt rx (py - ph + 96) 14 (hsb 0 0 60) "Right slope: jammed flow"
    , txt rx (py - ph + 116) 14 (hsb 0 0 60) "Top: the most flow" ] )
  where
    ax = hsba 0 0 100 0.3
    sw = width * 0.55
    sh = height * 0.42
    px = width * 0.42
    py = height - 50
    pw = sw * 0.6
    ph = height - 110 - sh - 110
    rx = px + pw + 20
    qmax = 0.7
    live = toList (wLive w)
    q = sum live / fromIntegral (max (length live) 1)
    dens = fromIntegral (length (wCars w)) / fromIntegral roadL

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> resetRoad w { wDensity = roundTo 100 (min 0.9 (wDensity w + 0.03)) }
  EventKey (SpecialKey KeyDown) Down _ _ -> resetRoad w { wDensity = roundTo 100 (max 0.01 (wDensity w - 0.03)) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wP = roundTo 100 (min 0.8 (wP w + 0.05)) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wP = roundTo 100 (max 0 (wP w - 0.05)) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "fF" -> w { wFd = [], wSweep = [ 0.02 + 0.03 * fromIntegral k | k <- [0 .. 30 :: Int] ] }
    | c `elem` "rR" -> resetRoad w
  _ -> w

-- ---------- やってみよう ----------
-- 1. p = 0 にすると渋滞は消える？ 密度を 0.2 → 0.3 と上げたときの時空図を見る
-- 2. F で基本図を描き、流量が最大になる密度を読む。高速道路の「流入制限」はここに保つため
-- 3. ルール③で、止まっている車だけ減速確率を大きくする（スロースタート）と、渋滞が長く残るか試す
-- 4. 待ち行列として見る：渋滞の先頭は「窓口」、そこから出ていく率が流量。待ち行列の長さ = 渋滞の長さ

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Float -> Float -> Point
pt x y = (x - width / 2, height / 2 - y)

-- 中心 (x, y)、直径 d の円
dot :: Float -> Float -> Float -> Color -> Picture
dot x y d c = Color c (at (pt x y) (disc (d / 2)))

-- 中心 (x, y)、直径 d、太さ wd の輪
hoop :: Float -> Float -> Float -> Float -> Color -> Picture
hoop x y d wd c = Color c (at (pt x y) (ring (d / 2) wd))

-- 太さ wd の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg wd c (x1, y1) (x2, y2) = Color c (thickLine wd (pt x1 y1) (pt x2 y2))

-- 文字：(x, y) は左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 中央ぞろえの文字
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y s c str = txt (x - fromIntegral (length str) * s * 0.36) y s c str

-- p5 の nf(v, 1, d)：小数 d 桁の文字列
nf :: PrintfArg a => Int -> a -> String
nf d v = printf "%.*f" d v

-- p5 の round(v * k) / k
roundTo :: Float -> Float -> Float
roundTo k v = fromIntegral (round (v * k) :: Int) / k
