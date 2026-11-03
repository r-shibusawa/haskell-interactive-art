-- ============================================
-- 04 シンプレックス法：角から角へ、上り坂だけを歩く
-- スラック形の式を書きかえる「ピボット」をくり返すと、実行可能領域の頂点を 1 つずつ移動し、
-- 目的関数 z が増えなくなったところが最適解。
-- 原点がダメなときは補助 LP（x₀ を足す）で最初の頂点を探す（2 段階法の第 1 段階）
-- スペース：1 ステップ / A：自動 / R：最初から
-- 1：お手本  2：工場の例  3：原点が実行不能（補助 LP）  4：実行不能  5：非有界  6：ランダム
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM, when)
import Data.Char (digitToInt, toLower)
import Data.List (elemIndex)
import Numeric (showFFloat)

speed :: Int
speed = 50 -- ★ 自動再生の間隔（フレーム）

nVars :: Int
nVars = 2

type V2 = (Double, Double)

-- 線形計画問題
data LP = LP { lpC :: ![Double], lpA :: ![[Double]], lpB :: ![Double] }

-- お手本の問題
pre :: Int -> LP
pre k = case k of
  1 -> LP [2, 1] [[3, -1], [1, 1], [-1, 2]] [9, 7, 8]
  2 -> LP [3, 5] [[1, 0], [0, 2], [3, 2]] [4, 12, 18]
  3 -> LP [1, 2] [[1, 1], [-2, -1]] [6, -4]        -- x1 + x2 <= 6 かつ 2x1 + x2 >= 4
  4 -> LP [1, 1] [[1, 1], [-1, -1]] [2, -5]
  _ -> LP [1, 1] [[1, -1], [-1, 1]] [1, 4]

data Hi = Hi
  { hE    :: !(Maybe Int)  -- 入る変数
  , hL    :: !(Maybe Int)  -- 出る式
  , hDone :: !(Maybe Int)  -- ピボットした式
  , hBad  :: !Bool
  }

noHi :: Hi
noHi = Hi Nothing Nothing Nothing False

-- 表（タブロー）：各行は「基底変数 + Σ T[i][j]·x_j = 右辺」
data S = S
  { sV      :: !Int
  , sNames  :: ![String]
  , sT      :: ![[Double]]
  , sBasis  :: ![Int]
  , sZ      :: ![Double]
  , sZv     :: !Double
  , sMsg    :: !String
  , sHi     :: !Hi
  , sFin    :: !Bool
  , sPhase  :: !Int
  , sTarget :: !V2
  , sTrail  :: ![V2]   -- 新しいものが後ろ
  }

data World = World
  { wLP     :: !LP
  , wS      :: !S
  , wRest   :: [S]
  , wPos    :: !V2
  , wAuto   :: !Bool
  , wTimer  :: !Int
  , wPreset :: !Int
  }

main :: IO ()
main = runSketch (sketch "16-04 Simplex" (makeWorld 1 (pre 1)))
  { skBack  = bgC
  , skStep  = \_ _ w -> pure (stepW w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

load :: Int -> IO World
load 6 = do
  a <- replicateM 4 (replicateM 2 (fromIntegral <$> randomRIO (1, 5 :: Int)))
  b <- replicateM 4 (fromIntegral <$> randomRIO (10, 29 :: Int))
  c <- replicateM 2 (fromIntegral <$> randomRIO (1, 5 :: Int))
  pure (makeWorld 6 (LP c a b))
load k = pure (makeWorld k (pre k))

makeWorld :: Int -> LP -> World
makeWorld k lp =
  let s0 = (build lp False)
             { sMsg = "Space: one step. We start at the origin (x1 = x2 = 0, all slack = b)."
             , sTrail = [(0, 0)] }
  in World lp s0 (steps (simplex lp) s0) (cur s0) False 0 k

-- 表を作る
build :: LP -> Bool -> S
build lp aux =
  let m = length (lpA lp)
      v = nVars + m + (if aux then 1 else 0)
      names = ["x" ++ show (j + 1) | j <- [0 .. nVars + m - 1]] ++ ["x0" | aux]
      row i ar b =
        [ if j < nVars then idx ar j
          else if j == nVars + i then 1
          else if aux && j == v - 1 then -1
          else if j == v then b
          else 0
        | j <- [0 .. v] ]
      t = [row i ar b | (i, ar, b) <- zip3 [0 ..] (lpA lp) (lpB lp)]
      z | aux = [if j == v - 1 then -1 else 0 | j <- [0 .. v - 1]]
        | otherwise = [if j < nVars then idx (lpC lp) j else 0 | j <- [0 .. v - 1]]
  in S v names t [nVars + i | i <- [0 .. m - 1]] z 0 "" noHi False 2 (0, 0) []

-- 基底解の (x1, x2)
cur :: S -> V2
cur s =
  let v = sV s
      value j = sum [idx r v | (b, r) <- zip (sBasis s) (sT s), b == j]
  in (value 0, value 1)

-- ピボット：行 l の基底変数と変数 e を入れかえる
pivotS :: Int -> Int -> S -> S
pivotS l e s =
  let v = sV s
      rowL = let r = idxOr [] (sT s) l; pv = idx r e in map (/ pv) r
      rows = [ if i == l then rowL
               else let f = idx r e in if abs f > 1e-12 then zipWith (\x y -> x - f * y) r rowL else r
             | (i, r) <- zip [0 ..] (sT s) ]
      ce = idx (sZ s) e
  in s { sT = rows
       , sZ = zipWith (\x y -> x - ce * y) (sZ s) rowL
       , sZv = sZv s + ce * idx rowL v
       , sBasis = [if i == l then e else b | (i, b) <- zip [0 ..] (sBasis s)] }

moveTarget :: S -> S
moveTarget s = let p = cur s in s { sTarget = p, sTrail = sTrail s ++ [p] }

-- 1 回の単体法（z の係数が正の変数がなくなるまで）。戻り値 True なら非有界
simplexLoop :: Gen S Bool
simplexLoop = do
  s <- gets id
  let cand = [j | j <- [0 .. sV s - 1], j `notElem` sBasis s, idx (sZ s) j > 1e-9]
      e = foldl (\b j -> case b of
                    Nothing -> Just j
                    Just x | idx (sZ s) j > idx (sZ s) x + 1e-9 -> Just j
                    _ -> b) Nothing cand
  case e of
    Nothing -> pure False
    Just ev -> do
      let nameE = idxOr "?" (sNames s) ev
      modify (\t -> t { sHi = noHi { hE = Just ev }
                      , sMsg = nameE ++ " has the largest positive coefficient in z, so it enters. The more it grows, the more z grows." })
      yield
      let ratios = [ (idx r (sV s) / idx r ev, i) | (i, r) <- zip [0 ..] (sT s), idx r ev > 1e-9 ]
          (best, l) = foldl (\(b, li) (r, i) -> if r < b - 1e-9 then (r, i) else (b, li)) (1 / 0, -1) ratios
      if l < 0
        then do
          modify (\t -> t { sHi = noHi { hE = Just ev } })
          pure True
        else do
          let outN = idxOr "?" (sNames s) (idx (sBasis s) l)
          modify (\t -> t { sHi = noHi { hE = Just ev, hL = Just l }
                          , sMsg = "How far can " ++ nameE ++ " grow? Compare the limit of each row: " ++ outN
                                   ++ " reaches 0 first (it leaves). Limit " ++ fmt best })
          yield
          modify (\t -> let t2 = moveTarget (pivotS l ev t)
                        in t2 { sHi = noHi { hDone = Just l }
                              , sMsg = "Pivot: rewrite the row for " ++ nameE ++ " and substitute. " ++ nameE
                                       ++ " is basic, " ++ outN ++ " is 0 (nonbasic). z = " ++ fmt (sZv t2) })
          yield
          simplexLoop

simplex :: LP -> Gen S ()
simplex lp = do
  let minb = foldl min 1e18 (lpB lp)
      stage2 = do
        unb <- simplexLoop
        s <- gets id
        if unb
          then modify (\t -> t { sFin = True
                               , sMsg = maybe "" (idxOr "?" (sNames s)) (hE (sHi s))
                                        ++ " grows but no row reaches 0: it can grow forever. Unbounded (no maximum)." })
          else do
            let (x, y) = cur s
            modify (\t -> t { sHi = noHi, sFin = True
                            , sMsg = "All coefficients of z are <= 0: no direction raises z. Optimal! x = ("
                                     ++ fmt x ++ ", " ++ fmt y ++ "), z = " ++ fmt (sZv s) })
        yield
  if minb >= 0
    then stage2
    else do
      modify (\s -> let b = build lp True
                    in b { sPhase = 1, sTrail = sTrail s, sTarget = sTarget s, sHi = noHi
                         , sMsg = "At the origin some b < 0, so a slack is negative: infeasible. Build the auxiliary LP: minimize x0 (maximize -x0)." })
      yield
      let l = maybe 0 id (elemIndex minb (lpB lp))
      modify (\s -> let t = moveTarget (pivotS l (sV s - 1) s)
                    in t { sHi = noHi { hDone = Just l }
                         , sMsg = "Bring in x0 and swap with the most negative row " ++ idxOr "?" (sNames s) (nVars + l)
                                  ++ ": all right sides are now >= 0. The auxiliary LP can start." })
      yield
      _ <- simplexLoop
      zv <- gets sZv
      if zv < -1e-9
        then do
          modify (\s -> s { sHi = noHi { hBad = True }, sFin = True
                          , sMsg = "The auxiliary maximum is -x0 = " ++ fmt zv ++ " < 0: x0 cannot be 0, so the original LP is infeasible." })
          yield
        else do
          modify (leaveAux lp)
          yield
          stage2

-- x₀ の列を消して、もとの目的関数を今の非基底変数で書き直す
leaveAux :: LP -> S -> S
leaveAux lp s0 =
  let v0 = sV s0
      -- x₀ がまだ基底なら、ほかの変数と入れかえる
      s1 = case elemIndex (v0 - 1) (sBasis s0) of
        Nothing -> s0
        Just li ->
          let row = idxOr [] (sT s0) li
              cand = [j | j <- [0 .. v0 - 2], j `notElem` sBasis s0, abs (idx row j) > 1e-9]
          in case cand of
               (e : _) -> pivotS li e s0
               [] -> s0
      v = v0 - 1
      dropCol r = take (v0 - 1) r ++ drop v0 r
      rows = map dropCol (sT s1)
      z0 = [if j < nVars then idx (lpC lp) j else 0 | j <- [0 .. v - 1]]
      (zF, zvF) = foldl sub1 (z0, 0) (zip (sBasis s1) rows)
      sub1 (z, zv) (b, r)
        | b < v && abs (idx z b) > 1e-12 =
            let cb = idx z b
            in (zipWith (\x y -> x - cb * y) z r, zv + cb * idx r v)
        | otherwise = (z, zv)
  in s1 { sV = v, sNames = take v (sNames s1), sT = rows, sZ = zF, sZv = zvF, sPhase = 2, sHi = noHi
        , sMsg = "x0 = 0 is possible: a feasible corner was found. Drop x0, restore the original z and start the real run." }

advance :: World -> World
advance w
  | sFin (wS w) = w
  | otherwise = case wRest w of
      [] -> w { wS = (wS w) { sFin = True } }
      (s : r) -> w { wS = s, wRest = r }

stepW :: World -> World
stepW w0 =
  let running = wAuto w0 && not (sFin (wS w0))
      timer = if running then wTimer w0 + 1 else wTimer w0
      w1 = w0 { wTimer = timer }
      w2 = if running && timer `mod` speed == 0 then advance w1 else w1
      (px, py) = wPos w2
      (tx, ty) = sTarget (wS w2)
  in w2 { wPos = (px + (tx - px) * 0.08, py + (ty - py) * 0.08) }

drawW :: Env -> World -> Picture
drawW env w =
  let s = wS w
  in Pictures
       [ drawGeo env w
       , txt env 30 50 26 (g8 235) ("Simplex: corner to corner, uphill" ++ (if sPhase s == 1 then "  [auxiliary LP]" else ""))
       , txt env 30 72 15 (g8 150) ("Space: 1 step  A: auto" ++ (if wAuto w then " (on)" else "") ++ "  R: redo")
       , txt env 30 94 15 (g8 150) "1: sample  2: factory  3: bad origin  4: infeasible  5: unbounded  6: random"
       , drawSlack env w
       ]

drawGeo :: Env -> World -> Picture
drawGeo env w =
  let s = wS w
      lp = wLP w
      size = min (envW env * 0.42) (envH env - 150)
      ox = 40
      oy = 110
      rng = realToFrac (foldl max 12 (concat [[x * 1.2, y * 1.2] | (x, y) <- sTrail s])) :: Float
      toS (x, y) = (ox + realToFrac x / rng * size, oy + size - realToFrac y / rng * size)
      region = foldl (\p (a, b) -> clipHalf p (idx a 0) (idx a 1) b)
                     [(0, 0), (200, 0), (200, 200), (0, 200)] (zip (lpA lp) (lpB lp))
      stepK = if rng > 30 then 5 else 1 :: Float
      gridC = rgb255 45 38 62
      grid = [ let (x, _) = toS (realToFrac k, 0)
                   (_, y) = toS (0, realToFrac k)
               in Pictures [seg env gridC 1 (x, oy) (x, oy + size), seg env gridC 1 (ox, y) (ox + size, y)]
             | k <- takeWhile (<= rng) (iterate (+ stepK) 0) ]
      regionP
        | length region >= 3 =
            let q = map (pt env . toS) region
            in Pictures [Color (rgba255 78 225 190 60) (Polygon q), Color mint (thickPath 2 (q ++ take 1 q))]
        | otherwise = Blank
      trailP = Color amber (thickPath 3 (map (pt env . toS) (sTrail s)))
      (qx, qy) = toS (wPos w)
  in Pictures
       [ box env plotC ox oy size size
       , Pictures grid
       , regionP
       , trailP
       , dot env amber qx qy 22
       , dot env bgC qx qy 8
       , maskOut env bgC ox oy size size
       , para env ox (oy + size + 10) 14 (g8 200) size
           ("Green: feasible region  Yellow: current basic solution (x1, x2) and its path"
             ++ (if sPhase s == 1 then "\nDuring the auxiliary LP it comes in from outside the region" else ""))
       ]

drawSlack :: Env -> World -> Picture
drawSlack env w =
  let s = wS w
      hi = sHi s
      v = sV s
      px = envW env * 0.42 + 80
      pw = envW env - px - 30
      nb = [j | j <- [0 .. v - 1], j `notElem` sBasis s]
      name = idxOr "?" (sNames s)
      term sign c j = " " ++ (if sign c >= 0 then "+" else "-") ++ " " ++ fmt (abs c) ++ name j
      zs = (if sPhase s == 1 then "z' = " else "z = ") ++ fmt (sZv s)
             ++ concat [term id (idx (sZ s) j) j | j <- nb]
      y0 = 156
      rowPic (i, b, r) =
        let yy = y0 + 44 + fromIntegral (i :: Int) * 40
            bg | hL hi == Just i = rgba255 255 95 109 90
               | hDone hi == Just i = rgba255 78 225 190 60
               | otherwise = plotC
            st = name b ++ " = " ++ fmt (idx r v) ++ concat [term negate (idx r j) j | j <- nb]
            limitTxt = case (hE hi, hL hi) of
              (Just e, Just _) | idx r e > 1e-9 ->
                txtR env (px + pw - 16) (yy + 18) 15 amber ("limit " ++ fmt (idx r v / idx r e))
              _ -> Blank
        in Pictures [box env bg (px - 8) (yy - 6) pw 32, txtT env px yy 16 (g8 235) st, limitTxt]
      m = length (sT s)
      ym = y0 + 44 + fromIntegral m * 40 + 10
      msgC | sFin s && hBad hi = rgb255 255 110 110
           | sFin s = mint
           | otherwise = g8 240
  in Pictures
       [ txtT env px 120 18 (g8 235) "Slack form (left: basic, right: nonbasic)"
       , box env panelC (px - 8) (y0 - 6) pw 32
       , txtT env px y0 16 amber zs
       , case hE hi of
           Just e -> txtR env (px + pw - 16) (y0 + 18) 15 coral ("^ " ++ name e ++ " grows")
           Nothing -> Blank
       , Pictures (map rowPic (zip3 [0 ..] (sBasis s) (sT s)))
       , box env panelC (px - 8) ym pw 100
       , para env (px + 6) (ym + 10) 16 msgC (pw - 24) (sMsg s)
       , footer env px pw 14 (g8 150)
           "Basic solution: set the nonbasic variables on the right to 0. It is always a corner of the region.\nThere are many entering rules (here: largest coefficient). Bland's rule prevents cycling at one corner.\nWorst case is exponential, but it is very fast in practice. Polynomial methods exist too (ellipsoid, interior point)."
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (advance w)
  EventKey (Char c0) Down _ _ -> case toLower c0 of
    'a' -> pure w { wAuto = not (wAuto w) }
    'r' -> keep <$> load (wPreset w)
    c | c `elem` "123456" -> keep <$> load (digitToInt c)
    _ -> pure w
  _ -> pure w
  where keep w' = w' { wAuto = wAuto w, wTimer = wTimer w }

-- ---------- やってみよう ----------
-- 1. 2 の工場で、入る変数を「係数最大」ではなく「番号が小さい順」にすると歩く道は変わる？
-- 2. 3 の補助 LP で、黄色の点が領域の外から入ってくる様子を見る
-- 3. pre に自分の問題を書いて、03 で描いた図と答えが同じか確かめる

-- ---------- 道具 ----------
-- 範囲外ならかわりの値を返す添字
idxOr :: a -> [a] -> Int -> a
idxOr d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

idx :: Num a => [a] -> Int -> a
idx = idxOr 0

fmt :: Double -> String
fmt v
  | abs v < 1e-9 = "0"
  | abs (v - fromIntegral r) < 1e-9 = show r
  | otherwise = showFFloat (Just 2) v ""
  where r = floor (v + 0.5) :: Int

clipHalf :: [V2] -> Double -> Double -> Double -> [V2]
clipHalf ps u v b = concat (zipWith edge ps (drop 1 ps ++ take 1 ps))
  where
    edge p@(px, py) (qx, qy) =
      let fp = u * px + v * py - b
          fq = u * qx + v * qy - b
          keep = [p | fp <= 1e-9]
          cross
            | (fp < -1e-9 && fq > 1e-9) || (fp > 1e-9 && fq < -1e-9) =
                let t = fp / (fp - fq) in [(px + t * (qx - px), py + t * (qy - py))]
            | otherwise = []
      in keep ++ cross

-- (x, y, w, h) の外側を塗りつぶす（描いたものを枠で切り取る）
maskOut :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
maskOut env c x y w h =
  let far = 3000
  in Pictures
       [ box env c (x - far) (y - far) far (h + 2 * far)
       , box env c (x + w) (y - far) far (h + 2 * far)
       , box env c x (y - far) w far
       , box env c x (y + h) w far ]

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> (Float, Float) -> (Float, Float)
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> (Float, Float) -> (Float, Float) -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 長方形の枠線（太さ t）
frame :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
frame env c t x y w h = Pictures
  [ seg env c t (x, y) (x + w, y), seg env c t (x + w, y) (x + w, y + h)
  , seg env c t (x + w, y + h) (x, y + h), seg env c t (x, y + h) (x, y) ]

-- 直径 d の円（p5.js の circle）
dot :: Env -> Color -> Float -> Float -> Float -> Picture
dot env c x y d = Color c (at (pt env (x, y)) (disc (d / 2)))

-- 直径 d の円の輪郭（太さ t）
hoop :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
hoop env c t x y d = Color c (at (pt env (x, y)) (ring (d / 2) t))

-- 文字：txt は左下が (x, y)、txtT は左上が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtT, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtT env x y s c str = txt env x (y + s) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = txt env (x - fromIntegral (length str) * s * 0.78) y s c str

-- ---------- 道具：文字の折り返し（文字の幅はおよそ 0.85 × 大きさ） ----------
wrapText :: Float -> Float -> String -> [String]
wrapText s w = concatMap one . lines
  where
    limit = max 8 (floor (w / (s * 0.85))) :: Int
    one l = case words l of
      [] -> [""]
      (x : xs) -> go x xs
    go cur [] = [cur]
    go cur (x : xs)
      | length cur + 1 + length x <= limit = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs

lineH :: Float -> Float
lineH s = s * 1.35

-- 左上 (x, y) から幅 w で折り返して書く
para :: Env -> Float -> Float -> Float -> Color -> Float -> String -> Picture
para env x y s c w str = Pictures
  [ txtT env x (y + fromIntegral i * lineH s) s c l | (i, l) <- zip [0 :: Int ..] (wrapText s w str) ]

-- 折り返したときの高さ
paraH :: Float -> Float -> String -> Float
paraH s w str = fromIntegral (length (wrapText s w str)) * lineH s

-- 上から順に積む：(大きさ, 色, 下のすきま, 文字)
stack :: Env -> Float -> Float -> Float -> [(Float, Color, Float, String)] -> Picture
stack env x y0 w = Pictures . go y0
  where
    go _ [] = []
    go y ((s, c, gap, str) : rest) = para env x y s c w str : go (y + paraH s w str + gap) rest

-- 画面の下の端にそろえて書く
footer :: Env -> Float -> Float -> Float -> Color -> String -> Picture
footer env x w s c str = para env x (envH env - 14 - paraH s w str) s c w str

-- ---------- 道具：色 ----------
bgC, panelC, plotC, cellC, mint, coral, amber :: Color
bgC    = rgb255 18 14 26
panelC = rgb255 32 26 44
plotC  = rgb255 28 22 38
cellC  = rgb255 40 34 56
mint   = rgb255 78 225 190
coral  = rgb255 255 95 109
amber  = rgb255 255 200 90

-- 0〜255 の灰色
g8 :: Int -> Color
g8 v = rgb255 v v v

-- ---------- 道具：1 手ずつ止まる計算（p5.js の function* の代わり） ----------
-- yield のたびに状態を 1 つ流す計算。結果は「状態の遅延リスト」になり、
-- 画面側はリストを 1 つずつ取り出して進める。
newtype Gen s a = Gen { unGen :: s -> (a -> s -> [s]) -> [s] }

instance Functor (Gen s) where
  fmap f (Gen g) = Gen (\s k -> g s (k . f))

instance Applicative (Gen s) where
  pure x = Gen (\s k -> k x s)
  Gen f <*> Gen g = Gen (\s k -> f s (\h s' -> g s' (k . h)))

instance Monad (Gen s) where
  Gen g >>= f = Gen (\s k -> g s (\x s' -> unGen (f x) s' k))

yield :: Gen s ()
yield = Gen (\s k -> s : k () s)

gets :: (s -> a) -> Gen s a
gets f = Gen (\s k -> k (f s) s)

modify :: (s -> s) -> Gen s ()
modify f = Gen (\s k -> k () $! f s)

steps :: Gen s () -> s -> [s]
steps (Gen g) s0 = g s0 (\_ _ -> [])
