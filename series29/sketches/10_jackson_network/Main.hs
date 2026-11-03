-- ============================================
-- 10 ネットワーク待ち行列（ジャクソン網）
-- 窓口 A〜D がつながった網。外から A に客が来て、終わると確率で次の窓口へ進むか、外へ出る（戻ってくることもある）
--   各窓口に流れこむ量 λ_i は「流れの式」λ = 外からの到着 + Σ λ_j × (j から i へ行く確率) で決まる
--   ジャクソンの定理：各窓口はそれぞれ M/M/1（L_i = ρ_i/(1−ρ_i)）として計算できる。ρ が最大の窓口 = ボトルネック
-- クリック / 1〜4 = 窓口を選ぶ / ↑↓ = その窓口の μ / ←→ = その窓口の行き先の割合 / [ ] = 外からの到着 γ / F = 早送り / R = 測り直す
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt)
import Data.List (foldl')
import Text.Printf (PrintfArg, printf)

-- 窓口：名前・位置（画面に対する割合）・サービス率・行き先 2 つ（-1 = 外へ）・1 つ目へ行く確率
data Node = Node
  { nName :: String
  , nPx, nPy :: !Float
  , nMu, nS :: !Float
  , nOut1, nOut2 :: !Int
  , nQ, nDone :: !Int
  , nNext, nArea :: !Double
  }

-- 移動中の客（見た目だけ）：from（-1 = 外）、to、進み具合
data Fly = Fly !Int !Int !Float

data World = World
  { wGamma :: !Float    -- ★ 外から A への到着率
  , wNodes :: ![Node]
  , wSel :: !Int
  , wFast :: !Bool
  , wT, wNextExt, wAreaTot :: !Double
  , wFly :: ![Fly]
  , wExits, wDropped :: !Int
  }

main :: IO ()
main = runSketch (sketch "29-10 Jackson Network" world0)
  { skBack  = hsb 185 25 11
  , skStart = \_ -> resetSim world0
  , skStep  = \_ _ w -> stepWorld w
  , skDraw  = \env w -> drawAll env w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World 0.5 nodes0 0 False 0 0 0 [] 0 0

-- ★ 窓口の設定
nodes0 :: [Node]
nodes0 =
  [ mk "A Desk" 0.15 0.45 1.6 1 2 0.6
  , mk "B Exam" 0.42 0.25 1.0 3 (-1) 0.7
  , mk "C Consult" 0.42 0.68 0.8 3 0 0.6
  , mk "D Pay" 0.7 0.45 1.5 (-1) 1 0.85 ]
  where mk name x y m o1 o2 s = Node name x y m s o1 o2 0 0 infinity 0

resetSim :: World -> IO World
resetSim w = do
  n <- expRand (gamma w)
  pure w { wT = 0, wNextExt = n, wFly = [], wExits = 0, wAreaTot = 0, wDropped = 0
         , wNodes = [ d { nQ = 0, nNext = infinity, nArea = 0, nDone = 0 } | d <- wNodes w ] }

gamma :: World -> Double
gamma = realToFrac . wGamma

nodeAt :: World -> Int -> Node
nodeAt w i = case drop i (wNodes w) of
  (n : _) -> n
  [] -> Node "" 0 0 1 0.5 (-1) (-1) 0 0 infinity 0   -- i はいつも 0〜3 なのでここには来ない

posOf :: Node -> Point
posOf n = (nPx n * width, 120 + nPy n * (height - 220))

-- ★ 流れの式を繰り返し代入で解く
solveFlows :: World -> [Float]
solveFlows w = iterateN (500 :: Int) [g, 0, 0, 0]
  where
    g = wGamma w
    iterateN 0 lam = lam
    iterateN k lam = let nw = once lam in foldr seq () nw `seq` iterateN (k - 1) nw
    once lam = foldl' addFlow [g, 0, 0, 0] (zip lam (wNodes w))
    addFlow acc (l, n) = addTo (nOut2 n) (l * (1 - nS n)) (addTo (nOut1 n) (l * nS n) acc)
    addTo to v acc | to < 0 = acc
                   | otherwise = [ if i == to then a + v else a | (i, a) <- zip [0 ..] acc ]

-- 窓口 i に客が着く
arriveAt :: Int -> World -> IO World
arriveAt i w = do
  let n = nodeAt w i
  if nQ n >= 400 then pure w { wDropped = wDropped w + 1 } else do
    s <- expRand (realToFrac (nMu n))
    let n1 = n { nQ = nQ n + 1, nNext = if nQ n == 0 then wT w + s else nNext n }
    pure w { wNodes = setAt i n1 (wNodes w) }

pushFly :: Fly -> World -> World
pushFly f w = if length (wFly w) < 80 then w { wFly = f : wFly w } else w

accumulate :: Double -> World -> World
accumulate tn w =
  let dt = tn - wT w
      tot = sum (map nQ (wNodes w))
  in w { wNodes = [ d { nArea = nArea d + fromIntegral (nQ d) * dt } | d <- wNodes w ]
       , wAreaTot = wAreaTot w + fromIntegral tot * dt, wT = tn }

advance :: Double -> World -> IO World
advance tEnd w
  | tn > tEnd = pure (accumulate tEnd w)
  | who < 0 = do                                  -- 外からの到着
      g <- expRand (gamma w)
      w1 <- arriveAt 0 moved
      advance tEnd (pushFly (Fly (-1) 0 0) w1 { wNextExt = tn + g })
  | otherwise = do                                -- サービス終了
      let n = nodeAt moved who
      s <- expRand (realToFrac (nMu n))
      u <- randomRIO (0, 1 :: Float)
      let n1 = n { nQ = nQ n - 1, nDone = nDone n + 1, nNext = if nQ n > 1 then tn + s else infinity }
          w1 = moved { wNodes = setAt who n1 (wNodes moved) }
          to = if u < nS n then nOut1 n else nOut2 n
      w2 <- if to < 0 then pure w1 { wExits = wExits w1 + 1 } else arriveAt to w1
      advance tEnd (pushFly (Fly who to 0) w2)
  where
    (tn, who) = foldl' (\(t, k) (i, n) -> if nNext n < t then (nNext n, i) else (t, k))
                       (wNextExt w, -1) (zip [0 ..] (wNodes w))
    moved = accumulate tn w

stepWorld :: World -> IO World
stepWorld w0 = do
  w1 <- advance (wT w0 + (if wFast w0 then 120 else 6) / 60) w0
  let adv (Fly a b f) = Fly a b (f + 0.05)
      alive (Fly _ _ f) = f < 1
  pure w1 { wFly = filter alive (map adv (wFly w1)) }

drawAll :: Env -> World -> Picture
drawAll env w = Pictures
  ( concatMap arrowsOf (zip [0 ..] (wNodes w))
 ++ [ drawArrow (20, snd ePos) ePos 1 0 "" ]
 ++ map drawFly (wFly w)
 ++ concat (zipWith3 drawNode [0 ..] (wNodes w) (zip lams rhos))
 ++ [ txt tx ty 16 (gray 1) "Throughput (exit rate)"
    , txt tx (ty + 22) 16 (gray 1) ("Measured " ++ nf 3 (fromIntegral (wExits w) / tSafe) ++ " (stable: = " ++ nf 2 (wGamma w) ++ ")")
    , txt tx (ty + 52) 16 (gray 1) "Mean people in the network L"
    , txt tx (ty + 74) 16 (gray 1) ("Measured " ++ nf 2 (wAreaTot w / tSafe) ++ "  Theory " ++ (if stable then nf 2 lTot else "inf"))
    , txt tx (ty + 104) 16 (gray 1) "Mean time in network W = L / gamma"
    , txt tx (ty + 126) 16 (gray 1) ("Theory " ++ (if stable then nf 2 (lTot / wGamma w) else "inf"))
    , txt tx (ty + 156) 16 (hsb 0 80 100) ("Bottleneck: " ++ nName (nodeAt w bott))
    ]
 ++ [ txt tx (ty + 178) 16 (hsb 0 80 100) "rho >= 1: the line keeps growing!" | not stable ]
 ++ [ txt tx (ty + 200) 14 (hsb 0 0 60) ("Overflowed and vanished: " ++ show (wDropped w)) | wDropped w > 0 ]
 ++ [ txt 20 40 18 (gray 1) ("Selected: " ++ nName (nodeAt w (wSel w)) ++ " (click / 1-4)  mu: Up/Down  Split: <- ->  gamma = " ++ nf 2 (wGamma w)
        ++ " ([ ])  F: " ++ (if wFast w then "ON" else "off") ++ "  R")
    , txt 20 68 15 (hsb 0 0 60) "The glowing desk is the bottleneck: speeding up the others barely helps. Return arrows (C->A, D->B) multiply the flow." ] )
  where
    lams = solveFlows w
    rhos = zipWith (\l n -> l / nMu n) lams (wNodes w)
    bott = fst (foldl' (\(bi, bv) (i, r) -> if r > bv then (i, r) else (bi, bv)) (0, -1) (zip [0 ..] rhos))
    ePos = posOf (nodeAt w 0)
    tSafe = max (wT w) 1e-9
    stable = all (< 1) rhos
    lTot = sum [ r / (1 - r) | r <- rhos, r < 1 ]
    tx = width * 0.7
    ty = height * 0.68
    -- 矢印（行き先と確率）
    arrowsOf (i, n) = [ one i n (nS n) (nOut1 n), one i n (1 - nS n) (nOut2 n) ]
    one :: Int -> Node -> Float -> Int -> Picture
    one i n p to =
      let a = posOf n
          b = target a to
          back = to >= 0 && (let m = nodeAt w to in nOut1 m == i || nOut2 m == i)
      in drawArrow a b p (if back then 30 else 0) (if to < 0 then "out" else "")
    target (ax, ay) to = if to < 0 then (width * 0.9, ay - 60) else posOf (nodeAt w to)
    drawFly (Fly a b f) =
      let pa = if a < 0 then (20, snd ePos) else posOf (nodeAt w a)
          pb = target pa b
      in dot (lerpF (fst pa) (fst pb) f) (lerpF (snd pa) (snd pb) f) 7 (hsb 45 80 100)
    -- 窓口
    drawNode :: Int -> Node -> (Float, Float) -> [Picture]
    drawNode i n (l, r) =
      let (x, y) = posOf n
          selected = i == wSel w
          lm = fl (nArea n) / fl tSafe
          lt = r / (1 - r)
      in [ dot x y 150 (hsba 0 80 100 ((25 + 15 * sin (fromIntegral (envFrame env) * 0.1)) / 100)) | i == bott ]
         ++ [ dot x y 112 (if r >= 1 then hsb 0 70 60 else hsb (200 - r * 160) 60 55)
            , hoop x y 112 (if selected then 4 else 2) (if selected then hsb 45 90 100 else hsba 0 0 100 0.5)
            , txtC x (y - 22) 14 (gray 1) (nName n)
            , txtC x (y - 5) 14 (gray 1) ("mu=" ++ nf 1 (nMu n))
            , txtC x (y + 12) 14 (gray 1) ("lam=" ++ nf 2 l)
            , txtC x (y + 29) 14 (gray 1) ("rho=" ++ nf 2 r) ]
         ++ [ dot (x - 50 + fromIntegral (k `mod` 10) * 11) (y + 62 + fromIntegral (k `div` 10) * 11) 8 (hsb 30 80 100) | k <- [0 .. min (nQ n) 30 - 1] ]
         ++ [ txt (x - 60) (y + 108) 14 (hsb 0 0 70) ("L meas " ++ nf 2 lm ++ " / theory " ++ (if r < 1 then nf 2 lt else "inf")) ]

drawArrow :: Point -> Point -> Float -> Float -> String -> Picture
drawArrow (ax, ay) (bx, by) p bend lab = Pictures
  ( seg (1 + p * 5) c (sx, sy) (ex, ey)
  : fillPoly c [ (ex + ux * 6, ey + uy * 6), (ex - ux * 10 - uy * 7, ey - uy * 10 + ux * 7), (ex - ux * 10 + uy * 7, ey - uy * 10 - ux * 7) ]
  : [ txt ((sx + ex) / 2 - uy * 14) ((sy + ey) / 2 + ux * 14) 14 (hsb 45 80 100) (nf 2 p ++ (if null lab then "" else " " ++ lab)) | p < 1 ] )
  where
    dx = bx - ax
    dy = by - ay
    d = max (sqrt (dx * dx + dy * dy)) 1
    ux = dx / d
    uy = dy / d
    sx = ax + ux * 52 - uy * bend * 0.3
    sy = ay + uy * 52 + ux * bend * 0.3
    ex = bx - ux * 56 - uy * bend * 0.3
    ey = by - uy * 56 + ux * bend * 0.3
    c = hsba 0 0 100 ((25 + p * 60) / 100)

-- 選んだ窓口を書きかえる
onSel :: (Node -> Node) -> World -> World
onSel f w = w { wNodes = setAt (wSel w) (f (nodeAt w (wSel w))) (wNodes w) }

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 pos
        hit = [ i | (i, n) <- zip [0 ..] (wNodes w), let (x, y) = posOf n, dist2 (mx, my) (x, y) < 50 ]
    in pure (case reverse hit of
               (i : _) -> w { wSel = i }
               [] -> w)
  EventKey (SpecialKey KeyUp) Down _ _ -> resetSim (onSel (\n -> n { nMu = roundTo 10 (min 5 (nMu n + 0.1)) }) w)
  EventKey (SpecialKey KeyDown) Down _ _ -> resetSim (onSel (\n -> n { nMu = roundTo 10 (max 0.2 (nMu n - 0.1)) }) w)
  EventKey (SpecialKey KeyRight) Down _ _ -> resetSim (onSel (\n -> n { nS = roundTo 100 (min 0.95 (nS n + 0.05)) }) w)
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetSim (onSel (\n -> n { nS = roundTo 100 (max 0.05 (nS n - 0.05)) }) w)
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> pure w { wSel = digitToInt c - 1 }
    | c == ']' -> resetSim w { wGamma = roundTo 100 (min 3 (wGamma w + 0.05)) }
    | c == '[' -> resetSim w { wGamma = roundTo 100 (max 0.05 (wGamma w - 0.05)) }
    | c `elem` "fF" -> pure w { wFast = not (wFast w) }
    | c `elem` "rR" -> resetSim w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. ボトルネックの μ だけを上げる（↑）。全体の W はどれだけ減る？ ボトルネックでない窓口を上げた場合と比べる
-- 2. C の戻り（C → A）の割合を増やす（C を選んで ←）。A の λ が γ より大きくなるのはなぜ？
-- 3. γ（]）を上げていき、最初に ρ = 1 を超える窓口を予想してから確かめる
-- 4. 窓口を 5 つに増やして、自分の大学の「履修登録の流れ」や「病院の流れ」をつくってみる

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Float -> Float -> Point
pt x y = (x - width / 2, height / 2 - y)

-- マウスの位置（gloss の座標）を p5 の座標に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- 中心 (x, y)、直径 d の円
dot :: Float -> Float -> Float -> Color -> Picture
dot x y d c = Color c (at (pt x y) (disc (d / 2)))

-- 中心 (x, y)、直径 d、太さ wd の輪
hoop :: Float -> Float -> Float -> Float -> Color -> Picture
hoop x y d wd c = Color c (at (pt x y) (ring (d / 2) wd))

-- 太さ wd の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg wd c (x1, y1) (x2, y2) = Color c (thickLine wd (pt x1 y1) (pt x2 y2))

-- 塗りつぶした多角形（凸形だけ）
fillPoly :: Color -> [Point] -> Picture
fillPoly c ps = Color c (Polygon [pt x y | (x, y) <- ps])

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

-- 平均 1/rate の指数分布の乱数（次の到着までの時間など）
expRand :: Double -> IO Double
expRand rate = do
  u <- randomRIO (0, 1 :: Double)
  pure (-log (max 1e-12 (1 - u)) / rate)

infinity :: Double
infinity = 1 / 0

-- リストの i 番目を置きかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if j == i then v else x | (j, x) <- zip [0 ..] xs]

-- p5 の round(v * k) / k
roundTo :: Float -> Float -> Float
roundTo k v = fromIntegral (round (v * k) :: Int) / k
