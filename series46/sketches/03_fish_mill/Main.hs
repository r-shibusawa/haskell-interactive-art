-- ============================================
-- 46-03 魚の渦（ミル）と隊列 ─ クーザンの 3 ゾーンモデル
-- 魚のまわりに 3 つの輪：近すぎたら離れる（反発 zor）→ その外で向きをそろえる（整列 zoo）→ さらに外の仲間に寄る（誘引 zoa）
-- 輪の幅 Δro（整列）と Δra（誘引）を変えるだけで「ばらばらの群れ」「ドーナツ形の渦」「ゆるい隊列」「きれいな隊列」に切り替わる
-- 操作：1〜4 = 型のプリセット / ←→ = 整列の幅 Δro / ↑↓ = 誘引の幅 Δra / R = 散らばった状態から / Z = 輪を表示
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftL, shiftR, xor)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)
import Numeric (showFFloat)

nFish :: Int
nFish = 200                  -- ★ 魚の数

zor, speedU, turnRate, fov, dtS, unitPx :: Float
zor = 1                      -- 反発ゾーンの半径（これが長さの単位）
speedU = 3                   -- ★ 速さ（単位 / 秒）
turnRate = 40                -- ★ 1 秒に曲がれる最大角度（度）
fov = 270                    -- ★ 見える角度（真後ろは見えない）
dtS = 0.1                    -- 1 ステップの時間
unitPx = 13                  -- 1 単位 = 何 px

stepsPerFrame :: Int
stepsPerFrame = 2

-- プリセット：(Δro, Δra, 名前)
presets :: [(Float, Float, String)]
presets = [(0, 14, "swarm"), (2, 13, "mill (torus)"), (6, 12, "loose parallel"), (13, 13, "tight parallel")]

data World = World
  { wX, wY      :: !(U.Vector Float)     -- 位置（単位）
  , wDx, wDy    :: !(U.Vector Float)     -- 向き（長さ 1）
  , wRo, wRa    :: !Float                -- 整列ゾーンの幅 Δro、誘引ゾーンの幅 Δra
  , wPreset     :: !String
  , wZones      :: !Bool
  , wPHist      :: ![Float]              -- 偏極度 p の記録（新しい順、300 個まで）
  , wMHist      :: ![Float]              -- 角運動量 m の記録
  , wSeed       :: !Seed
  }

world0 :: World
world0 = World U.empty U.empty U.empty U.empty 2 13 "mill (torus)" True [] [] 1

main :: IO ()
main = runSketch (sketch "46-03 Fish mill and schools" world0)
  { skBack  = hsb 222 55 14
  , skStart = \_ -> do
      s <- randomRIO (1, 2 ^ (62 :: Int))
      pure (scatter world0 { wSeed = s })
  , skStep  = \_ _ w -> pure (measure (iterate couzinStep w !! stepsPerFrame))
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

-- 半径 12 の円の中に散らばらせる
scatter :: World -> World
scatter w = w { wX = U.generate nFish (\i -> rad i * cos (ang i)), wY = U.generate nFish (\i -> rad i * sin (ang i))
              , wDx = U.generate nFish (cos . hd), wDy = U.generate nFish (sin . hd)
              , wPHist = [], wMHist = [], wSeed = nextSeed s }
  where
    s = wSeed w
    ang i = hashRand s (3 * i) * tau
    rad i = sqrt (hashRand s (3 * i + 1)) * 12
    hd i = hashRand s (3 * i + 2) * tau

-- 仲間を数えるときの合計（反発・整列・誘引）
data Acc = Acc !Float !Float !Int !Float !Float !Int !Float !Float !Int

-- ★ クーザンのルール：反発が最優先。反発する相手がいなければ 整列＋誘引
couzinStep :: World -> World
couzinStep w = w { wX = x', wY = y', wDx = U.map cos hs, wDy = U.map sin hs, wSeed = nextSeed s }
  where
    s = wSeed w
    xs = wX w
    ys = wY w
    dxs = wDx w
    dys = wDy w
    n = U.length xs
    rRo = zor + wRo w
    rRa = zor + wRo w + wRa w
    cosHalf = cos (fov / 2 * pi / 180)
    maxTurn = turnRate * pi / 180 * dtS
    heading i =
      let ax0 = xs U.! i
          ay0 = ys U.! i
          adx = dxs U.! i
          ady = dys U.! i
          look acc@(Acc rx ry nr ox oy no gx gy na) j
            | j == i = acc
            | d < 1e-6 || d >= rRa = acc
            | d < zor = Acc (rx - vx) (ry - vy) (nr + 1) ox oy no gx gy na                       -- 反発は全方向
            | vx * adx + vy * ady < cosHalf = acc                                                -- 真後ろは見えない
            | d < rRo = Acc rx ry nr (ox + dxs U.! j) (oy + dys U.! j) (no + 1) gx gy na          -- 整列
            | otherwise = Acc rx ry nr ox oy no (gx + vx) (gy + vy) (na + 1)                     -- 誘引
            where
              ex = xs U.! j - ax0
              ey = ys U.! j - ay0
              d = sqrt (ex * ex + ey * ey)
              vx = ex / d
              vy = ey / d
          Acc rx ry nr ox oy no gx gy na = foldl' look (Acc 0 0 0 0 0 0 0 0 0) [0 .. n - 1]
          unitOf a b = let m = sqrt (a * a + b * b) + 1e-9 in (a / m, b / m)
          (tx0, ty0)
            | nr > 0 = (rx, ry)
            | no == 0 && na == 0 = (adx, ady)
            | otherwise =
                let (o1, o2) = if no > 0 then unitOf (ox + adx) (oy + ady) else (0, 0)
                    (a1, a2) = if na > 0 then unitOf gx gy else (0, 0)
                in (o1 + a1, o2 + a2)
          tx = tx0 + (hashRand s (2 * i) - 0.5) * 0.1                                           -- 小さなゆらぎ
          ty = ty0 + (hashRand s (2 * i + 1) - 0.5) * 0.1
          -- 目標の向きへ、最大 maxTurn だけ回る
          cur = atan2 ady adx
          want = atan2 ty tx
          diff = atan2 (sin (want - cur)) (cos (want - cur))
      in cur + clampF (-maxTurn) maxTurn diff
    hs = U.generate n heading
    x' = U.imap (\i x -> x + cos (hs U.! i) * speedU * dtS) xs
    y' = U.imap (\i y -> y + sin (hs U.! i) * speedU * dtS) ys

-- 重心
centroid :: World -> (Float, Float)
centroid w = let n = max 1 (fromIntegral (U.length (wX w))) in (U.sum (wX w) / n, U.sum (wY w) / n)

-- ---- 測る：偏極度 p（向きのそろい）と 角運動量 m（重心のまわりを回るか）
polMom :: World -> (Float, Float)
polMom w = (sqrt (sx * sx + sy * sy) / n, abs mz / n)
  where
    n = max 1 (fromIntegral (U.length (wX w)))
    (cx, cy) = centroid w
    sx = U.sum (wDx w)
    sy = U.sum (wDy w)
    mz = U.sum (U.generate (U.length (wX w)) (\i ->
           let rx = wX w U.! i - cx
               ry = wY w U.! i - cy
               rl = sqrt (rx * rx + ry * ry) + 1e-9
           in (rx * wDy w U.! i - ry * wDx w U.! i) / rl))

measure :: World -> World
measure w = let (p, m) = polMom w in w { wPHist = take 300 (p : wPHist w), wMHist = take 300 (m : wMHist w) }

draw :: Env -> World -> Picture
draw env w = view env $
  -- 魚（カメラは群れの重心を追う）
  [ Color (hsb 185 60 100) (Pictures (zipWith3 fishPic (U.toList (wX w)) (U.toList (wY w)) (zip (U.toList (wDx w)) (U.toList (wDy w))))) ]
  ++ zones
  -- ---- 右：グラフと「型の地図」
  ++ [ drawPlot (wPHist w) gx 110 310 70 0 1 (hsb 50 80 100) ("Polarisation p = " ++ fmt 2 pol)
     , drawPlot (wMHist w) gx 220 310 70 0 1 (hsb 320 60 100) ("Angular momentum m = " ++ fmt 2 mom)
     , Color (hsbA 0 0 0 35) (rect (mx + mw / 2) (my + mh / 2) mw mh)
     , txt mx (my - 8) 14 (hsb 0 0 85) "Map of types (x = d_ro, y = d_ra)" ]
  ++ [ seg 1 (hsbA 0 0 100 15) (mx + b / 15 * mw, my) (mx + b / 15 * mw, my + mh) | b <- [1, 4, 10] ]   -- おおよその境目（Δra が大きいとき）
  ++ [ txt (mx + 3) (my + 24) 14 c "swarm", txt (mx + 2 / 15 * mw + 3) (my + 44) 14 c "mill"
     , txt (mx + 5 / 15 * mw) (my + 24) 14 c "loose", txt (mx + 10.5 / 15 * mw) (my + 44) 14 c "tight"
     , txt (mx + mw - 92) (my + mh - 8) 14 c "d_ro -> 15", txt (mx + 4) (my + mh - 8) 14 c "d_ra 0"
     , dot (hsb 0 80 100) (mx + wRo w / 15 * mw) (my + mh - wRa w / 15 * mh) 12
     , txt 20 36 16 (hsb 0 0 100) ("1-4: preset (" ++ wPreset w ++ ")   Left/Right: d_ro (align) = " ++ fmt 1 (wRo w)
                                   ++ "   Up/Down: d_ra (attract) = " ++ fmt 1 (wRa w) ++ "   R: scatter   Z: rings")
     , txt 20 60 14 (hsb 0 0 70) ("Type now (from p and m): " ++ kind
                                  ++ ".  Rings around fish 1: red = repulsion, yellow = alignment, green = attraction") ]
  where
    (pol, mom) = polMom w
    (cx, cy) = centroid w
    ox = envW env * 0.36 - cx * unitPx
    oy = envH env * 0.55 - cy * unitPx
    fishPic fx fy (dx, dy) =
      let x = ox + fx * unitPx
          y = oy + fy * unitPx
      in Polygon [(x + dx * 8, y + dy * 8), (x - dx * 5 - dy * 2.5, y - dy * 5 + dx * 2.5), (x - dx * 5 + dy * 2.5, y - dy * 5 - dx * 2.5)]
    zones
      | wZones w && not (U.null (wX w)) =
          let x = ox + U.head (wX w) * unitPx
              y = oy + U.head (wY w) * unitPx
          in [ Color (hsbA 0 80 100 80) (Translate x y (ring (zor * unitPx) 1.5))
             , Color (hsbA 50 80 100 60) (Translate x y (ring ((zor + wRo w) * unitPx) 1.5))
             , Color (hsbA 120 60 100 45) (Translate x y (ring ((zor + wRo w + wRa w) * unitPx) 1.5))
             , dot (hsb 0 0 100) x y 6 ]
      | otherwise = []
    gx = envW env - 340
    mx = gx
    my = 340
    mw = 310
    mh = 200
    c = hsb 0 0 60
    kind | pol > 0.65 = "parallel school"
         | mom > 0.45 = "mill"
         | otherwise = "swarm"

event :: Event -> World -> World
event ev w = case ev of
  EventKey (Char k) Down _ _
    | k `elem` "1234" -> case drop (fromEnum k - fromEnum '1') presets of
        ((ro, ra, nm) : _) -> scatter w { wRo = ro, wRa = ra, wPreset = nm }
        [] -> w
    | k `elem` "rR" -> scatter w
    | k `elem` "zZ" -> w { wZones = not (wZones w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wRo = min 15 (wRo w + 0.5), wPreset = "manual" }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wRo = max 0 (wRo w - 0.5), wPreset = "manual" }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wRa = min 15 (wRa w + 0.5), wPreset = "manual" }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wRa = max 0 (wRa w - 0.5), wPreset = "manual" }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 2（渦）で R を何回か押す。毎回ドーナツができる？ 右回り・左回りはどう決まる？
-- 2. 渦の状態から → で Δro を少しずつ広げる。どこで渦が「隊列」に変わる？ 戻すと渦に戻る？（ヒステリシス）
-- 3. fov を 360 にする（真後ろも見える）と、渦はできにくくなる。目の付き方が群れの形を決める
-- 4. Δra を 0 にすると誘引がなくなり、群れはバラバラに散っていく
-- 5. nFish を 50 と 400 で比べよう。数が多いほど渦は大きく、安定する？

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

