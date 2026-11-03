-- ============================================
-- 03 3 リンクアームで文字を描く
-- 「手先にこの線をなぞらせたい」→ 軌道上の点を順に目標にして IK（CCD）で追いかける
-- 右のグラフは 3 つの関節角の時間変化。手先はまっすぐ動いても関節はまっすぐ動かない
-- 1 = 円 / 2 = 四角 / 3 = 文字 N / 4 = 星 / ↑↓ = 速さ / スペース = 一時停止 / C = 描いた線を消す
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl')
import Text.Printf (printf)

lens :: [Float]
lens = [150, 120, 90]         -- ★ 3 本のリンクの長さ

hues :: [Float]
hues = [200, 270, 320]

data World = World
  { wQ      :: ![Float]       -- 関節角
  , wShape  :: !Int
  , wT      :: !Float         -- 軌道のパラメータ 0〜1
  , wSpeed  :: !Float         -- ★ 1 フレームで進む t
  , wPaused :: !Bool
  , wPen    :: ![Point]       -- 手先が実際に通った点（新しい順、1500 点まで）
  , wHist   :: ![[Float]]     -- 関節角の履歴（新しい順、400 フレーム分。1 要素 = 3 つの角）
  }

main :: IO ()
main = runSketch (sketch "21-03 Arm Draws a Path" world0)
  { skBack  = hsb 215 35 12
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = World [1.2, -0.6, -0.4] 1 0 0.002 False [] []

basePos, center :: Point
basePos = (width * 0.3, height * 0.55)
center = (fst basePos + 200, snd basePos - 140)

shapeName :: Int -> String
shapeName s = case s of { 1 -> "Circle"; 2 -> "Square"; 3 -> "Letter N"; _ -> "Star" }

-- ★ 軌道：t（0〜1）→ 目標の点
pathPoint :: Int -> Float -> Point
pathPoint shape tt = case shape of
  1 -> (cx + r * cos (tau * tt), cy + r * sin (tau * tt))
  2 -> polyline [(-r, -r), (r, -r), (r, r), (-r, r), (-r, -r)] tt
  3 -> polyline [(-r * 0.7, r), (-r * 0.7, -r), (r * 0.7, r), (r * 0.7, -r)] tt
  _ -> polyline [ (r * cos a, r * sin a) | i <- [0 .. 5 :: Int], let a = -pi / 2 + fromIntegral i * tau * 2 / 5 ] tt
  where
    (cx, cy) = center
    r = 90

-- 折れ線を等速でなぞる
polyline :: [Point] -> Float -> Point
polyline pts tt =
  let segs = zip pts (drop 1 pts)
      total = sum [dist2 a b | (a, b) <- segs]
      go _ [] = center
      go s ((a, b) : rest)
        | s <= d || null rest =
            let u = clampF 0 1 (s / d)
            in (fst center + lerpF (fst a) (fst b) u, snd center + lerpF (snd a) (snd b) u)
        | otherwise = go (s - d) rest
        where d = dist2 a b
  in go (tt * total) segs

joints :: [Float] -> [Point]
joints q = map fst (scanl next (basePos, 0) (zip lens q))
  where next ((x, y), a) (l, qi) =
          let a' = a + qi
          in ((x + l * cos a', y - l * sin a'), a')

handOf :: [Point] -> Point
handOf = foldl' (\_ p -> p) basePos

-- CCD を何回か回して手先を目標へ（1 フレームに 3 回で十分なめらか）
solveIK :: Point -> [Float] -> [Float]
solveIK (tx, ty) q0 = foldl' turn q0 (concat (replicate 3 [2, 1, 0]))
  where
    turn q i =
      let pts = joints q
          (jx, jy) = case drop i pts of { (p : _) -> p; [] -> basePos }
          (hx, hy) = handOf pts
          d = atan2 (-(ty - jy)) (tx - jx) - atan2 (-(hy - jy)) (hx - jx)
      in addAt i (atan2 (sin d) (cos d) * 0.8) q     -- ★ 0.8 = 少し控えめに回すと震えにくい

step :: World -> World
step w =
  let t0 = wT w + wSpeed w
      t | wPaused w = wT w
        | t0 >= 1 = t0 - 1
        | otherwise = t0
      q = solveIK (pathPoint (wShape w) t) (wQ w)
  in w { wT = t, wQ = q
       , wPen = take 1500 (handOf (joints q) : wPen w)
       , wHist = take 400 (q : wHist w) }

drawAll :: World -> Picture
drawAll w =
  let shape = wShape w
      pts = joints (wQ w)
      hand = handOf pts
      target = pathPoint shape (wT w)
      gx = width * 0.58
      gy = 120
      gw = width * 0.38
      gh = height - 240
      armLink (i, c, a, b) = Color (hsb c 60 95) (thickLine (12 - i * 2) a b)
      y0 i = gy + (fromIntegral i + 0.5) * gh / 3
      wrapA a = atan2 (sin a) (cos a)
      -- 関節 i の折れ線グラフ
      curve i = [ (gx + (fromIntegral k / 400) * gw, y0 i - (wrapA v / pi) * gh / 7)
                | (k, qs) <- zip [0 :: Int ..] (reverse (wHist w)), v <- take 1 (drop i qs) ]
      graph (i, c) = Pictures
        [ Color (hsb 0 0 40) (Line [(gx, y0 i), (gx + gw, y0 i)])
        , Color (hsb c 60 95) (thickPath 2 (curve i)) ]
      graphText (i, c, q) = txt (gx + 6) (y0 i - gh / 7 - 2) 14 (hsb c 60 95)
        ("q" ++ show (i + 1) ++ " = " ++ nf 0 (wrapA q * 180 / pi) ++ " deg")
  in Pictures
  [ view
      ( [ Color (hsbA 0 0 100 15) (Line [pathPoint shape (fromIntegral u * 0.005) | u <- [0 .. 200 :: Int]])   -- 目標の軌道
        , Color (hsbA 45 90 100 80) (thickPath 3 (wPen w)) ]     -- 描いた線
        ++ map armLink (zip4' [0 ..] hues pts (drop 1 pts))
        ++ [ Color (hsb 0 0 100) (Pictures (map (\p -> at p (disc 7)) (take 3 pts)))
           , Color (hsb 45 90 100) (at hand (disc 7))
           , Color (hsb 0 0 40) (boxAt (fst basePos - 30) (snd basePos) 60 14)
           , Color (hsb 120 90 100) (at target (ring 9 2))
           -- 関節角グラフ
           , Color (hsbA 0 0 100 6) (boxAt (gx - 10) (gy - 10) (gw + 20) (gh + 20)) ]
        ++ map graph (zip [0 :: Int ..] hues) )
  , Pictures (map graphText (zip3 [0 :: Int ..] hues (wQ w)))
  , txt 20 34 16 (hsb 0 0 100) ("Path: " ++ shapeName shape ++ " (1-4)   Speed " ++ nf 1 (wSpeed w * 1000) ++ " (Up/Down)   Space: "
                                ++ (if wPaused w then "resume" else "pause") ++ "   C: clear line   t = " ++ nf 2 (wT w))
  , txt 20 58 14 (hsb 0 0 60) "When the hand draws a straight line, the joint angles (right) still curve: joint space vs task space."
  , txt 20 78 14 (hsb 0 0 60) "Too fast and IK cannot keep up, so the line breaks."
  ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> w { wShape = fromEnum c - fromEnum '0', wT = 0, wPen = [] }
    | toLower c == 'c' -> w { wPen = [] }
  EventKey (SpecialKey k) Down _ _ -> case k of
    KeyUp -> w { wSpeed = min 0.02 (wSpeed w * 1.4) }
    KeyDown -> w { wSpeed = max 0.0003 (wSpeed w / 1.4) }
    KeySpace -> w { wPaused = not (wPaused w) }
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 四角の角で関節角のグラフが「折れる」のを探そう。手先の向きが急に変わる瞬間
-- 2. 速さを上げると描いた線が目標からずれる。どのくらいまで追いつける？（solveIK の回数を増やすと？）
-- 3. pathPoint に自分の文字や形を追加しよう（shape 5）。自分の名前の頭文字が描けるか
-- 4. center を基部から遠くへ動かすと届かなくなる。届かないときアームはどうふるまう？


-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は p5 の text と同じ左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 左上の角が (x, y) の長方形（p5 の rect）
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y bw bh = rect (x + bw / 2) (y + bh / 2) bw bh

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 小数点以下 d 桁の文字列（p5 の nf）
nf :: Int -> Float -> String
nf d = printf ("%." ++ show d ++ "f")

-- リストの i 番目に d を足す
addAt :: Int -> Float -> [Float] -> [Float]
addAt i d xs = [if k == i then x + d else x | (k, x) <- zip [0 ..] xs]

zip4' :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
zip4' _ _ _ _ = []
