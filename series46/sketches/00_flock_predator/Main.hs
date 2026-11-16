-- ============================================
-- 46-00 ボイド＋捕食者 ─ 群れが割れる・渦になる
-- 3 つのルール（分離・整列・結合）に「捕食者から逃げる」を足す。捕食者が飛び込むと群れが割れ、まわりを回る渦（ベイトボール）ができる
-- 近傍を「半径 r 以内の全員」と「近い順に 7 羽（トポロジカル近傍）」で切り替えて、まとまり方を比べる
-- 操作：T = 近傍の切替 / P = 捕食者（自動 ⇄ マウス）/ ↑↓ = 半径 r / ←→ = 逃げはじめる距離 / スペース = 捕食者を休ませる / R = リセット
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, xor)
import Data.List (foldl', sortOn)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word (Word64)
import Numeric (showFFloat)

nBirds :: Int
nBirds = 400                 -- ★ 鳥（魚）の数

maxSpeed, maxForce :: Float
maxSpeed = 3.2               -- ★ 速さの上限
maxForce = 0.12              -- ★ 曲がる力の上限

wSep, wAli, wCoh, wFlee :: Float
wSep = 1.6                   -- ★ 4 つのルールの重み
wAli = 1.0
wCoh = 0.9
wFlee = 4.0

kNear :: Int
kNear = 7                    -- ★ トポロジカル近傍の数（ムクドリは 6〜7 羽）

predSpeed :: Float
predSpeed = 3.7              -- ★ 捕食者の速さ（鳥より少しだけ速い）

cellSize :: Float
cellSize = 60                -- 空間ハッシュのマスの大きさ（px）

-- 捕食者
data Pred = Pred { prX, prY, prVx, prVy :: !Float }

data World = World
  { wW, wH      :: !Float                -- 画面の大きさ（リセットしたときの値）
  , wPx, wPy    :: !(U.Vector Float)     -- 位置
  , wVx, wVy    :: !(U.Vector Float)     -- 速度
  , wPred       :: !Pred
  , wRadius     :: !Float                -- 近傍の半径 r（px）
  , wFear       :: !Float                -- 捕食者から逃げはじめる距離
  , wTopo       :: !Bool                 -- True = 近い順に kNear 羽
  , wAuto       :: !Bool                 -- 捕食者は自動か
  , wRest       :: !Bool                 -- 捕食者が休み中か
  , wCaught     :: !Int
  , wGroups     :: !Int                  -- 群れの数（5 羽以上）
  , wPhiHist    :: ![Float]              -- 記録（新しい順、300 個まで）
  , wGroupHist  :: ![Float]
  , wSeed       :: !Seed
  }

world0 :: World
world0 = World 1280 800 U.empty U.empty U.empty U.empty (Pred 60 60 0 0) 55 130 False True False 0 0 [] [] 1

main :: IO ()
main = runSketch (sketch "46-00 Boids and a predator" world0)
  { skBack  = hsb 258 45 12
  , skStart = \env -> do
      s <- randomRIO (1, 2 ^ (62 :: Int))
      pure (resetFlock env world0 { wSeed = s })
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \env ev w -> pure (event env ev w)
  }

resetFlock :: Env -> World -> World
resetFlock env w = w { wW = sw, wH = sh, wPx = U.generate nBirds (\i -> sw * 0.5 + (r (4 * i) - 0.5) * 400)
                     , wPy = U.generate nBirds (\i -> sh * 0.5 + (r (4 * i + 1) - 0.5) * 300)
                     , wVx = U.generate nBirds (\i -> cos (ang i) * 2), wVy = U.generate nBirds (\i -> sin (ang i) * 2)
                     , wPred = Pred 60 60 0 0, wCaught = 0, wPhiHist = [], wGroupHist = [], wSeed = nextSeed s }
  where
    s = wSeed w
    sw = envW env
    sh = envH env
    r = hashRand s
    ang i = r (4 * i + 2) * tau

-- 近傍の 1 羽：番号、相手への差 (dx, dy)、距離
data Nb = Nb !Int !Float !Float !Float

-- ★ 鳥 i が見る「仲間」のリストを返す（半径 r か、近い順に k 羽）
neighbours :: World -> Int -> Int -> Grid -> Int -> [Nb]
neighbours w cols rows grid i
  | wTopo w = take kNear (sortOn (\(Nb _ _ _ d) -> d) (let f = found 1 in if length f >= kNear then f else found 3))
  | otherwise = found (ceiling (wRadius w / cellSize))
  where
    xi = wPx w U.! i
    yi = wPy w U.! i
    cx = floor (xi / cellSize) :: Int
    cy = floor (yi / cellSize) :: Int
    rr = wRadius w * wRadius w
    found reach =
      [ Nb j dx dy (sqrt d2 + 0.01)
      | gy <- [-reach .. reach], gx <- [-reach .. reach]
      , let c = (cx + gx) `mod` cols + ((cy + gy) `mod` rows) * cols
      , j <- U.toList (cellMembers grid c), j /= i
      , let dx = wrapD (wW w) (wPx w U.! j - xi)
            dy = wrapD (wH w) (wPy w U.! j - yi)
            d2 = dx * dx + dy * dy
      , wTopo w || d2 < rr ]

-- 目標の向き (x, y) に最高速で向かうための力（maxForce で頭打ち）
steer :: Float -> Float -> Float -> Float -> (Float, Float)
steer x y vx vy
  | m < 1e-9 = (0, 0)
  | otherwise = (clampF (-maxForce) maxForce (x / m * maxSpeed - vx), clampF (-maxForce) maxForce (y / m * maxSpeed - vy))
  where m = sqrt (x * x + y * y)

step :: Env -> World -> World
step env w0 = measure (movePredator env (moveBirds w1))
  where
    up = if isDown env (SpecialKey KeyUp) then 1 else 0
    dn = if isDown env (SpecialKey KeyDown) then -1 else 0
    rt = if isDown env (SpecialKey KeyRight) then 2 else 0
    lt = if isDown env (SpecialKey KeyLeft) then -2 else 0
    w1 = w0 { wRadius = clampF 15 120 (wRadius w0 + up + dn), wFear = clampF 30 300 (wFear w0 + rt + lt) }

-- ---- 1 羽ずつ 4 つのルールで向きを決めて動かす
moveBirds :: World -> World
moveBirds w = w { wPx = px', wPy = py', wVx = vx', wVy = vy', wGroups = groupsOf n edges }
  where
    n = U.length (wPx w)
    sw = wW w
    sh = wH w
    cols = max 1 (ceiling (sw / cellSize))
    rows = max 1 (ceiling (sh / cellSize))
    cellOf i = (floor (wPx w U.! i / cellSize) `mod` cols) + (floor (wPy w U.! i / cellSize) `mod` rows) * cols
    grid = buildGrid (cols * rows) (U.generate n cellOf)
    nbs = map (neighbours w cols rows grid) [0 .. n - 1]
    Pred qx qy _ _ = wPred w
    force i nb =
      let vxi = wVx w U.! i
          vyi = wVy w U.! i
          r = wRadius w
          sepOf (Nb _ dx dy d) = if d < r * 0.45 || (wTopo w && d < 25) then (-dx / (d * d), -dy / (d * d)) else (0, 0)  -- 分離
          sumP = foldl' (\(a, b) (c, d) -> (a + c, b + d)) (0, 0)
          (sx, sy) = sumP (map sepOf nb)
          (alx, aly) = sumP [ (wVx w U.! j, wVy w U.! j) | Nb j _ _ _ <- nb ]                                         -- 整列
          (chx, chy) = sumP [ (dx, dy) | Nb _ dx dy _ <- nb ]                                                          -- 結合
          (f1x, f1y) = if null nb then (0, 0) else
            let (a, b) = steer sx sy vxi vyi
                (c, d) = steer alx aly vxi vyi
                (e, f) = steer chx chy vxi vyi
            in (a * wSep + c * wAli + e * wCoh, b * wSep + d * wAli + f * wCoh)
          ex = wrapD sw (wPx w U.! i - qx)
          ey = wrapD sh (wPy w U.! i - qy)
          ed = sqrt (ex * ex + ey * ey)
          (f2x, f2y) = if not (wRest w) && ed < wFear w                                                                 -- 逃げる
                         then let k = (1 - ed / wFear w) * wFlee
                                  (a, b) = steer ex ey vxi vyi
                              in (a * k, b * k)
                         else (0, 0)
      in (f1x + f2x, f1y + f2y)
    fs = U.fromList (zipWith force [0 ..] nbs)
    edges = [ (i, j) | (i, nb) <- zip [0 ..] nbs, Nb j _ _ d <- nb, d < 40 ]
    newV i =
      let (fx, fy) = fs U.! i
          vx1 = wVx w U.! i + fx
          vy1 = wVy w U.! i + fy
          sp = sqrt (vx1 * vx1 + vy1 * vy1)
          (vx2, vy2) = if sp > maxSpeed then (vx1 * maxSpeed / sp, vy1 * maxSpeed / sp) else (vx1, vy1)
      in if sp < 1 then (vx2 + vx2 / (sp + 1e-6) * 0.2, vy2 + vy2 / (sp + 1e-6) * 0.2) else (vx2, vy2)
    vs = U.generate n newV
    vx' = U.map fst vs
    vy' = U.map snd vs
    px' = U.imap (\i x -> wrapF sw (x + vx' U.! i)) (wPx w)
    py' = U.imap (\i y -> wrapF sh (y + vy' U.! i)) (wPy w)

-- 群れの数：距離 40 未満でつながった仲間の集まりのうち 5 羽以上のもの（素集合データ構造）
groupsOf :: Int -> [(Int, Int)] -> Int
groupsOf n edges = runST $ do
  uf <- U.thaw (U.generate n id)
  let find i = do
        p <- M.read uf i
        if p == i then pure i else do
          gp <- M.read uf p
          M.write uf i gp
          find gp
  forM_ edges $ \(a, b) -> do
    ra <- find a
    rb <- find b
    when (ra /= rb) (M.write uf ra rb)
  roots <- mapM find [0 .. n - 1]
  let sizes = U.accumulate (+) (U.replicate n (0 :: Int)) (U.fromList [ (r, 1) | r <- roots ])
  pure (U.length (U.filter (>= 5) sizes))

movePredator :: Env -> World -> World
movePredator env w = if wRest w then w { wPred = p' } else caught
  where
    Pred x y vx vy = wPred w
    sw = wW w
    sh = wH w
    n = U.length (wPx w)
    dd i = let a = wrapD sw (wPx w U.! i - x); b = wrapD sh (wPy w U.! i - y) in a * a + b * b
    (tx, ty)
      | wRest w = (x + vx * 10, y + vy * 10)
      | not (wAuto w) = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)
      | n == 0 = (x, y)
      | otherwise =                                 -- 自動：一番近い鳥をねらう
          let best = U.minIndexBy (\a b -> compare (dd a) (dd b)) (U.enumFromN 0 n)
              bi = best
          in (x + wrapD sw (wPx w U.! bi - x), y + wrapD sh (wPy w U.! bi - y))
    dx = tx - x
    dy = ty - y
    d = sqrt (dx * dx + dy * dy) + 1e-6
    vx1 = vx + dx / d * 0.25
    vy1 = vy + dy / d * 0.25
    sp = sqrt (vx1 * vx1 + vy1 * vy1)
    lim = if wRest w then 1.2 else predSpeed
    (vx2, vy2) = if sp > lim then (vx1 * lim / sp, vy1 * lim / sp) else (vx1, vy1)
    x' = wrapF sw (x + vx2)
    y' = wrapF sh (y + vy2)
    p' = Pred x' y' vx2 vy2
    -- 捕まったら遠くで生まれ直す
    hit i = let a = wrapD sw (wPx w U.! i - x'); b = wrapD sh (wPy w U.! i - y') in a * a + b * b < 81
    hits = U.generate n hit
    caught = w { wPred = p', wCaught = wCaught w + U.length (U.filter id hits)
               , wPx = U.imap (\i v -> if hits U.! i then wrapF sw (x' + sw / 2) else v) (wPx w)
               , wPy = U.imap (\i v -> if hits U.! i then wrapF sh (y' + sh / 2) else v) (wPy w) }

-- ---- 測る：偏り φ と 群れの数
phiOf :: World -> Float
phiOf w = sqrt (svx * svx + svy * svy) / max ssp 1e-6
  where
    svx = U.sum (wVx w)
    svy = U.sum (wVy w)
    ssp = U.sum (U.zipWith (\a b -> sqrt (a * a + b * b)) (wVx w) (wVy w))

measure :: World -> World
measure w = w { wPhiHist = take 300 (phiOf w : wPhiHist w), wGroupHist = take 300 (fromIntegral (wGroups w) : wGroupHist w) }

draw :: Env -> World -> Picture
draw env w = view env $
  -- 鳥（三角形）
  [ Color (hsb 42 70 100) (Pictures (zipWith bird [0 ..] (U.toList (wPx w)))) ]
  ++ [ Color (hsbA 0 80 100 (if wRest w then 20 else 45)) (Translate qx qy (Circle (wFear w)))
     , dot (hsb 0 85 (if wRest w then 50 else 100)) qx qy 18
     , drawPlot (wPhiHist w) (sw - 330) 100 300 70 0 1 (hsb 42 70 100) ("Alignment phi = " ++ fmt 2 (phiOf w))
     , drawPlot (wGroupHist w) (sw - 330) 210 300 70 0 20 (hsb 190 60 100) ("Groups (5+ birds) = " ++ show (wGroups w))
     , txt 20 36 16 (hsb 0 0 100) ("T: neighbours = " ++ (if wTopo w then "nearest " ++ show kNear ++ " (topological)" else "radius r " ++ show (round (wRadius w) :: Int) ++ " px")
                                   ++ "   P: predator " ++ (if wAuto w then "auto" else "mouse")
                                   ++ "   Left/Right: flee distance " ++ show (round (wFear w) :: Int)
                                   ++ "   Space: " ++ (if wRest w then "resting" else "hunting") ++ "   R: reset")
     , txt 20 60 14 (hsb 0 0 70) ("Caught " ++ show (wCaught w) ++ "   (separation, alignment, cohesion + flee)")
     , txt 20 80 14 (hsb 0 0 70) "Topological neighbours keep the flock together even when it thins out" ]
  where
    sw = envW env
    Pred qx qy _ _ = wPred w
    bird i x =
      let y = wPy w U.! i
          vx = wVx w U.! i
          vy = wVy w U.! i
          sp = sqrt (vx * vx + vy * vy) + 1e-6
          ux = vx / sp
          uy = vy / sp
      in Polygon [(x + ux * 7, y + uy * 7), (x - ux * 4 - uy * 3, y - uy * 4 + ux * 3), (x - ux * 4 + uy * 3, y - uy * 4 - ux * 3)]

event :: Env -> Event -> World -> World
event env ev w = case ev of
  EventKey (Char c) Down _ _
    | c `elem` "tT" -> w { wTopo = not (wTopo w) }
    | c `elem` "pP" -> w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> resetFlock env w
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wRest = not (wRest w) }
  EventResize _ -> resetFlock env w
  _ -> w

-- ---------- やってみよう ----------
-- 1. スペースで捕食者を休ませると、群れは 1 つにまとまり φ が 1 に近づく。狩りを再開すると群れの数と φ はどう変わる？
-- 2. T でトポロジカル近傍に。半径 r を小さく（↓）しても群れが崩れにくいのはなぜ？（密度が変わっても「7 羽」は変わらない）
-- 3. P でマウスを捕食者に。群れの真ん中にゆっくり入ると、穴が開いて周りを回る「渦」ができる
-- 4. wFlee を 0 にすると？ 10 にすると？ 捕まった数を 1 分間数えて比べよう
-- 5. predSpeed を maxSpeed（3.2）より遅くすると、捕食者はまず捕まえられない。群れは「逃げ切る」ための形

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
