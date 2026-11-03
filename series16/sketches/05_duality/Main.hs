-- ============================================
-- 05 双対性：資源の「値段」から見たもう 1 つの LP
-- 主問題「材料の範囲で利益 c·x を最大に」には、双対問題「材料 1 単位の値段 y を、
-- どの商品も値段の合計 ≥ 利益 になるように決めて、材料全体の値段 b·y を最小に」がペアでついてくる。
-- 最適値は必ず一致する（強双対定理）。y は材料を 1 増やしたときの利益の増え方（影の価格）
-- 制約の線をドラッグ：材料の量 b を増減 / 矢印の先をドラッグ：利益 c / マウスを領域に：その計画の利益
-- 1：パン屋  2：お手本  3：ランダム
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (digitToInt)
import Data.List (intercalate)
import Numeric (showFFloat)

range :: Double
range = 10 -- ★ 画面に表示する範囲

type V2 = (Double, Double)
type Con = (Double, Double, Double)  -- a x1 + b x2 <= e

data Opt = Opt { oX, oY, oZ :: !Double, oUnb :: !Bool }

data Drag = DragObj | DragCon !Int

data World = World
  { wCons   :: ![Con]
  , wC      :: !V2
  , wLabels :: ![String]
  , wOpt    :: !(Maybe Opt)
  , wYs     :: ![Double]
  , wDrag   :: !(Maybe Drag)
  }

main :: IO ()
main = runSketch (sketch "16-05 Duality" (update (bakery (World [] (1, 1) [] Nothing [] Nothing))))
  { skBack  = bgC
  , skDraw  = drawW
  , skEvent = onEvent
  }

bakery, sample :: World -> World
bakery w = w { wCons = [(2, 1, 10), (1, 2, 8), (0, 1, 3.5)], wC = (3, 2), wLabels = ["Flour", "Butter", "Sugar"] }
sample w = w { wCons = [(3, -1, 9), (1, 1, 7), (-1, 2, 8)], wC = (2, 1), wLabels = ["(1)", "(2)", "(3)"] }

preset :: Int -> World -> IO World
preset k w = update <$> case k of
  1 -> pure (bakery w)
  2 -> pure (sample w)
  _ -> do
    cons <- replicateM 3 $ do
      a <- randomRIO (1, 4 :: Int)
      b <- randomRIO (1, 4 :: Int)
      e <- randomRIO (6, 15 :: Int)
      pure (fromIntegral a, fromIntegral b, fromIntegral e)
    c1 <- randomRIO (1, 4 :: Int)
    c2 <- randomRIO (1, 4 :: Int)
    pure w { wCons = cons, wC = (fromIntegral c1, fromIntegral c2), wLabels = ["Res A", "Res B", "Res C"] }

-- 2 変数の LP を「角を全部調べる」方法で解く
lpSolve :: [Con] -> V2 -> Maybe Opt
lpSolve cs (c1, c2) =
  let allC = cs ++ [(-1, 0, 0), (0, -1, 0), (1, 0, 1000), (0, 1, 1000)]
      numbered = zip [0 :: Int ..] allC
      corners =
        [ (x, y)
        | (i, (a, b, e)) <- numbered, (j, (p, q, f)) <- numbered, j > i
        , let det = a * q - b * p
        , abs det >= 1e-12
        , let x = (e * q - b * f) / det
        , let y = (a * f - e * p) / det
        , all (\(u, v, t) -> u * x + v * y <= t + 1e-7) allC ]
      pick best (x, y) =
        let z = c1 * x + c2 * y
        in case best of
             Just o | z <= oZ o + 1e-9 -> best
             _ -> Just (Opt x y z False)
      mark o = o { oUnb = oX o > 999 || oY o > 999 }
  in mark <$> foldl pick Nothing corners

update :: World -> World
update w =
  let opt = lpSolve (wCons w) (wC w)
      eps = 1e-4
      -- 材料 i を少しだけ増やしたら利益がどれだけ増えるか
      priceOf o i =
        let c2 = [if k == i then (a, b, e + eps) else (a, b, e) | (k, (a, b, e)) <- zip [0 ..] (wCons w)]
        in case lpSolve c2 (wC w) of
             Just o2 -> max 0 ((oZ o2 - oZ o) / eps)
             Nothing -> 0
      ys = case opt of
        Just o | not (oUnb o) -> map (priceOf o) [0 .. length (wCons w) - 1]
        _ -> []
  in w { wOpt = opt, wYs = ys }

regionS :: Env -> Float
regionS env = min (envW env * 0.5) (envH env - 150)

rx0, ry0 :: Float
rx0 = 40
ry0 = 110

toS :: Env -> V2 -> (Float, Float)
toS env (x, y) =
  let s = regionS env
  in (rx0 + realToFrac (x / range) * s, ry0 + s - realToFrac (y / range) * s)

toW :: Env -> (Float, Float) -> V2
toW env (x, y) =
  let s = regionS env
  in (realToFrac ((x - rx0) / s) * range, realToFrac ((ry0 + s - y) / s) * range)

drawW :: Env -> World -> Picture
drawW env w =
  let s = regionS env
      cons = wCons w
      ys = wYs w
      (c1, c2) = wC w
      yOf i = case drop i ys of
        (y : _) -> y
        [] -> 0
      region = foldl (\p (a, b, e) -> clipHalf p a b e) [(0, 0), (100, 0), (100, 100), (0, 100)] cons
      regionP
        | length region >= 3 =
            let q = map (pt env . toS env) region
            in Pictures [Color (rgba255 78 225 190 60) (Polygon q), Color mint (thickPath 2 (q ++ take 1 q))]
        | otherwise = Blank
      conLine (i, (a, b, e)) =
        let (p, q)
              | abs b > abs a = ((-5, (e + 5 * a) / b), (range + 5, (e - (range + 5) * a) / b))
              | otherwise = (((e + 5 * b) / a, -5), ((e - (range + 5) * b) / a, range + 5))
            on = yOf i > 1e-6
        in if abs a < 1e-12 && abs b < 1e-12 then Blank
           else seg env (rgba255 255 95 109 (if on then 255 else 110)) (if on then 3.5 else 2) (toS env p) (toS env q)
      -- 最適点で、c を「効いている制約の法線」の組み合わせに分解する
      decomposition = case wOpt w of
        Just o | not (oUnb o) ->
          let (ox, oy) = toS env (oX o, oY o)
              k = 130 / realToFrac (sqrt (c1 * c1 + c2 * c2)) :: Float
              walk _ [] = []
              walk (px, py) ((i, (a, b, _)) : rest)
                | yOf i > 1e-6 =
                    let qx = px + realToFrac (a * yOf i) * k
                        qy = py - realToFrac (b * yOf i) * k
                        sb = show (i + 1)
                    in Pictures [ seg env coral 3 (px, py) (qx, qy)
                                , dot env coral qx qy 8
                                , txt env ((px + qx) / 2 + 6) ((py + qy) / 2 + 14) 14 coral ("y" ++ sb ++ "*a" ++ sb) ]
                         : walk (qx, qy) rest
                | otherwise = walk (px, py) rest
              tipX = ox + realToFrac c1 * k
              tipY = oy - realToFrac c2 * k
          in Pictures
               [ Pictures (walk (ox, oy) (zip [0 ..] cons))
               , seg env amber 4 (ox, oy) (tipX, tipY)
               , txt env (tipX + 6) (tipY - 6) 14 amber "c"
               , dot env amber ox oy 20 ]
        _ -> Blank
      (mx, my) = mouseP env
      (mwx, mwy) = toW env (mx, my)
      mFeas = mwx >= 0 && mwy >= 0 && all (\(a, b, e) -> a * mwx + b * mwy <= e + 1e-9) cons
      mouseDot = if mFeas && mx < rx0 + s then dot env white mx my 10 else Blank
      (ox0, oy0) = toS env (0, 0)
      tip0 = (ox0 + realToFrac c1 * 25, oy0 - realToFrac c2 * 25)
      px = rx0 + s + 30
      pw = envW env - px - 30
      f = fmtV
      numbered = zip [0 :: Int ..] cons
      sb i = show (i + 1)
      primalY = 164
      primalL = [ txtT env (px + 16) (primalY + fromIntegral i * 22) 16 (g8 220)
                    (lb ++ ": " ++ f a ++ "x1 + " ++ f b ++ "x2 <= " ++ f e)
                | (i, (a, b, e)) <- numbered, let lb = nth (wLabels w) i ]
      y1 = primalY + 3 * 22 + 14
      dualMin = "Minimize  " ++ intercalate " + " [f e ++ "y" ++ sb i | (i, (_, _, e)) <- numbered]
      dual1 = "x1 price: " ++ intercalate " + " [f a ++ "y" ++ sb i | (i, (a, _, _)) <- numbered] ++ " >= " ++ f c1
      dual2 = "x2 price: " ++ intercalate " + " [f b ++ "y" ++ sb i | (i, (_, b, _)) <- numbered] ++ " >= " ++ f c2
      y2 = y1 + 28 + 24 + 22 + 22 + 36
      result = case wOpt w of
        Nothing -> txtT env px y2 18 (rgb255 255 110 110) "Infeasible"
        Just o
          | oUnb o -> txtT env px y2 18 amber "The primal is unbounded, so the dual is infeasible"
          | otherwise ->
              let dual = sum [e * yOf i | (i, (_, _, e)) <- numbered]
                  primalT = "Primal max c.x* = " ++ f (oZ o) ++ "  (x* = (" ++ f (oX o) ++ ", " ++ f (oY o) ++ "))"
                  dualT = "Dual min   b.y* = " ++ f dual ++ "  (y* = (" ++ intercalate ", " (map f ys) ++ "))"
                  yDual = y2 + paraH 15 pw primalT + 4
                  by = yDual + paraH 15 pw dualT + 14
                  bw = pw - 20
                  zmax = if oZ o * 1.6 /= 0 then oZ o * 1.6 else 1
                  xOf v = px + realToFrac (max 0 (min 1 (v / zmax))) * bw
                  zm = c1 * mwx + c2 * mwy
                  yTxt = by + 50
                  weakTxt = "Weak duality: any plan's profit <= any pricing's total. When the two meet, both are proven optimal."
              in Pictures
                   [ para env px y2 15 mint pw primalT
                   , para env px yDual 15 coral pw dualT
                   -- 弱双対性のものさし
                   , box env cellC px by bw 14
                   , box env mint px by (xOf (oZ o) - px) 14
                   , if mFeas
                       then Pictures [ dot env white (xOf zm) (by + 7) 14
                                     , txtT env px (by + 22) 14 (g8 220) ("Your plan: profit " ++ f zm) ]
                       else Blank
                   , seg env coral 3 (xOf dual, by - 8) (xOf dual, by + 22)
                   , para env px yTxt 14 (g8 200) pw weakTxt
                   , Pictures [ txtT env px (yTxt + paraH 14 pw weakTxt + 8 + fromIntegral i * 22) 14
                                  (if yOf i > 1e-6 then rgb255 255 150 160 else g8 140)
                                  (nth (wLabels w) i ++ ": y" ++ sb i ++ " = " ++ f (yOf i)
                                    ++ (if yOf i > 1e-6 then "  (used up: more = more profit)" else "  (left over: no gain)"))
                              | (i, _) <- numbered ]
                   ]
  in Pictures
       [ box env plotC rx0 ry0 s s
       , regionP
       , Pictures (map conLine numbered)
       , decomposition
       , mouseDot
       , maskOut env bgC rx0 ry0 s s
       , seg env amber 4 (ox0, oy0) tip0
       , uncurry (dot env amber) tip0 16
       , txt env 30 50 26 (g8 235) "Duality: another LP, seen from resource prices"
       , txt env 30 80 15 (g8 150) "Drag line: b  Drag arrow: c  Mouse on region: that plan  1: bakery  2: sample  3: random"
       , txtT env px 112 18 (g8 235) "Primal (decide the amounts x)"
       , txtT env (px + 16) 140 16 amber ("Maximize  " ++ f c1 ++ "x1 + " ++ f c2 ++ "x2")
       , Pictures primalL
       , txtT env px y1 18 (g8 235) "Dual (decide the prices y)"
       , txtT env (px + 16) (y1 + 28) 16 coral dualMin
       , txtT env (px + 16) (y1 + 52) 16 (g8 220) dual1
       , txtT env (px + 16) (y1 + 74) 16 (g8 220) dual2
       , txtT env (px + 16) (y1 + 96) 16 (g8 220) "y >= 0"
       , result
       , footer env px pw 14 (g8 150)
           "Left: at the optimum, arrow c is the sum of the arrows a_i of the used-up constraints, scaled by y_i. That is why y can be read from the last z row of the simplex method."
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseP env
        (c1, c2) = wC w
        (ox, oy) = toS env (0, 0)
        (wx, wy) = toW env m
        nearLine (_, (a, b, e)) = abs (a * wx + b * wy - e) / sqrt (a * a + b * b) < 0.3
    in pure $
         if dist2 m (ox + realToFrac c1 * 25, oy - realToFrac c2 * 25) < 14
           then w { wDrag = Just DragObj }
           else case filter nearLine (zip [0 ..] (wCons w)) of
                  ((i, _) : _) -> w { wDrag = Just (DragCon i) }
                  [] -> w
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = Nothing }
  EventMotion _ -> pure $ case wDrag w of
    Nothing -> w
    Just DragObj ->
      let (ox, oy) = toS env (0, 0)
          (mx, my) = mouseP env
          rd v = fromIntegral (max 0 (floor (v + 0.5) :: Int)) :: Double
          c = (rd ((mx - ox) / 25), rd ((oy - my) / 25))
      in update w { wC = if c == (0, 0) then (1, 0) else c }
    Just (DragCon i) ->
      let (wx, wy) = toW env (mouseP env)
          set (a, b, e) =
            let v = a * wx + b * wy
                e' = fromIntegral (floor (v * 2 + 0.5) :: Int) / 2
            in (a, b, if isNaN e' then e else e')
      in update w { wCons = [if k == i then set c else c | (k, c) <- zip [0 ..] (wCons w)] }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> preset (digitToInt c) w { wDrag = Nothing }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. パン屋でバターの線を 1 だけ外へ動かす。利益の増え方が y2 と同じになるか確かめる
-- 2. 砂糖を増やしても利益が増えない理由を、図で説明する（砂糖の線が最適点にふれていない）
-- 3. 利益の矢印を回して、どの材料の値段が 0 になるか観察する（相補性）

-- ---------- 道具 ----------
fmtV :: Double -> String
fmtV v
  | abs v < 1e-6 = "0"
  | abs (v - fromIntegral r) < 1e-6 = show r
  | otherwise = showFFloat (Just 2) v ""
  where r = floor (v + 0.5) :: Int

nth :: [String] -> Int -> String
nth xs i = case drop i xs of
  (x : _) -> x
  [] -> ""

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
