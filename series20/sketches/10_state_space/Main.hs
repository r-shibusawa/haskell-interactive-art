-- ============================================
-- 10 状態空間と極配置
-- 位置と速度をまとめて「状態 x = [x1, x2]」にすると、システムは x' = A x + B u
--   フィードバック u = −K x をかけると閉ループは x' = (A − BK) x
--   好きな極を指定して K を逆算する = 極配置（アッカーマンの公式）
-- 1〜4 = 配置する極 / 0 = フィードバックなし / クリック = 初期状態（位相面）
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (printf)

kSpring, cDamp, r1, r2, dt :: Float
kSpring = 1.0   -- ★ バネ定数
cDamp = 0.2     -- ★ 摩擦（弱い減衰のバネ・マス系）
r1 = 3          -- 位相面の横の範囲
r2 = 4          -- 位相面の縦の範囲
dt = 1 / 60

histMax :: Int
histMax = 600

-- 2×2 行列：[[a, b], [c, d]] を (a, b, c, d) で表す
type M2 = (Float, Float, Float, Float)

matA :: M2
matA = (0, 1, -kSpring, -cDamp)

vecB :: Point
vecB = (0, 1)

-- ★ 配置したい極（名前、[(実部, 虚部), (実部, 虚部)]）
presets :: [(String, Maybe (Point, Point))]
presets =
  [ ("none (K = 0)", Nothing)
  , ("-1, -1 (slow)", Just ((-1, 0), (-1, 0)))
  , ("-3, -3 (fast)", Just ((-3, 0), (-3, 0)))
  , ("-1 +/- 2j (a bit shaky)", Just ((-1, 2), (-1, -2)))
  , ("-0.3 +/- 3j (very shaky)", Just ((-0.3, 3), (-0.3, -3)))
  ]

data World = World
  { wPreset :: !Int
  , wK1, wK2 :: !Float         -- フィードバックゲイン K = [k1, k2]
  , wX1, wX2, wU :: !Float
  , wTrail :: !(Seq Point)     -- 位相面の軌跡（最大 900 点）
  , h1, h2, hU :: !Hist
  }

-- ★ アッカーマン：K = [0 1] · [B AB]⁻¹ · (A² + α1 A + α0 I)
placePoles :: Int -> (Float, Float)
placePoles i = case drop i presets of
  ((_, Just ((re0, im0), (re1, im1))) : _) ->
    let a1 = negate (re0 + re1)            -- 望む特性多項式 s² + α1 s + α0
        a0 = re0 * re1 - im0 * im1
        (abx, aby) = m2vec matA vecB
        (bx, by) = vecB
        wc = (bx, abx, by, aby)
        phi = m2add (m2add (m2mul matA matA) (m2scale matA a1)) (m2scale (1, 0, 0, 1) a0)
        (_, _, c, d) = m2mul (m2inv wc) phi
    in (c, d)
  _ -> (0, 0)

setPreset :: Int -> World -> World
setPreset i w = let (a, b) = placePoles i in w { wPreset = i, wK1 = a, wK2 = b }

world0 :: World
world0 = setPreset 2 (World 2 0 0 2.5 0 0 Seq.empty Seq.empty Seq.empty Seq.empty)

main :: IO ()
main = runSketch (sketch "20-10 State Space" world0)
  { skBack  = hsb 230 30 10
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawAll
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

step :: World -> World
step w@World{..} =
  let u = clampF (-30) 30 (negate (wK1 * wX1 + wK2 * wX2))   -- ★ 状態フィードバック u = −K x
      (a, b, c, d) = matA
      (b0, b1) = vecB
      sub (x, y) _ =
        let d1 = a * x + b * y + b0 * u
            d2 = c * x + d * y + b1 * u
        in (x + d1 * dt / 4, y + d2 * dt / 4)
      (x10, x20) = foldl' sub (wX1, wX2) [1 .. 4 :: Int]
      x1 = clampF (-10) 10 x10
      x2 = clampF (-20) 20 x20
      tr = wTrail |> (x1, x2)
  in w { wX1 = x1, wX2 = x2, wU = u
       , wTrail = if Seq.length tr > 900 then Seq.drop 1 tr else tr
       , h1 = pushHist h1 x1, h2 = pushHist h2 x2, hU = pushHist hU u }

-- 位相面の枠
panel :: Env -> (Float, Float, Float, Float)
panel env = (envW env * 0.05, envH env * 0.16, envW env * 0.42, envH env * 0.7)

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures $
  [ -- 位相面（x1 横、x2 縦）
    box ppx ppy ppw pph (hsb 0 0 14)
  , seg 1 (hsb 0 0 30) (px 0, ppy) (px 0, ppy + pph)
  , seg 1 (hsb 0 0 30) (ppx, py 0) (ppx + ppw, py 0)
    -- ベクトル場（閉ループの x' の向き）
  , Color (hsb 200 40 60) (Pictures field)
    -- 軌跡（古いほど薄い）
  , Pictures trail
  , dot (px wX1) (py wX2) 14 (hsb 30 80 100)
  , txt (ppx + 8) (ppy + 20) 14 c70 "Phase plane: x1 = position, x2 = velocity"
  , txt (ppx + 8) (ppy + 38) 14 c70 "Click: initial state. Arrows: closed-loop flow"
    -- おもりの絵（上の帯）
  , seg 2 (hsb 0 0 40) (ppx, ry + 14) (ppx + ppw, ry + 14)
  , seg 2 (hsb 200 60 100) (px 0, ry - 14) (px 0, ry + 14)
  , box (px wX1 - 16) (ry - 12) 32 24 (hsb 30 80 100)
    -- 時間グラフ
  , drawPlot gx (ht * 0.16) gw (ht * 0.32) [ser h1 30 "Position x1", ser h2 200 "Velocity x2"] (-4) 4 "State"
  , drawPlot gx (ht * 0.53) gw (ht * 0.22) [ser hU 120 "u = -Kx"] (-15) 15 "Input"
  , txt 20 40 18 white ("Poles: " ++ presetName ++ " (0-4)   K = [" ++ nf 2 wK1 ++ ", " ++ nf 2 wK2 ++ "]   Closed-loop poles = " ++ polesTxt)
  , txt 20 68 15 c60 "x' = Ax + Bu, A = [[0, 1], [-k, -c]], B = [0, 1]. u = -Kx: k1 stiffens the spring, k2 adds friction."
  ]
  ++ zipWith (\i s -> txt gx (ht * 0.8 + i * 24) 15 c60 s) [0 ..]
     [ "Phase plane: stable poles pull into the origin; straight"
     , "for real poles, spiral for complex ones. Far poles are"
     , "faster but need a larger u."
     , "Pole placement: decide the character first, then compute"
     , "K. Press 1-4 and see how K changes." ]
  where
    wd = envW env
    ht = envH env
    (ppx, ppy, ppw, pph) = panel env
    px v = remap v (-r1) r1 ppx (ppx + ppw)
    py v = remap v (-r2) r2 (ppy + pph) ppy
    ry = ht * 0.11
    c60 = hsb 0 0 60
    c70 = hsb 0 0 70
    gx = wd * 0.52
    gw = wd * 0.44
    presetName = case drop wPreset presets of { ((s, _) : _) -> s; [] -> "" }
    (_, _, a10, a11) = matA
    field =
      [ Line [(px i, py j), (px i + d1 / l * 10, py j - d2 / l * 10)]
      | n <- [0 .. 12 :: Int], let i = -r1 + fromIntegral n * 0.5
      , m <- [0 .. 10 :: Int], let j = -r2 + fromIntegral m * 0.8
      , let uu = clampF (-30) 30 (negate (wK1 * i + wK2 * j))
            d1 = j
            d2 = a10 * i + a11 * j + uu
            l = sqrt (d1 * d1 + d2 * d2) + 1e-9 ]
    pts = toList wTrail
    nTrail = fromIntegral (length pts) :: Float
    trail = zipWith3 (\i (a, b) (c, d) -> seg 2 (hsba 30 80 100 (remap i 0 nTrail 0.1 1)) (px a, py b) (px c, py d))
              [1 ..] pts (drop 1 pts)
    -- 閉ループの極を確認（2×2 なら手計算：tr と det）
    (a, b, c, d) = matA
    (b0, b1) = vecB
    m00 = a - b0 * wK1
    m01 = b - b0 * wK2
    m10 = c - b1 * wK1
    m11 = d - b1 * wK2
    tr = m00 + m11
    det = m00 * m11 - m01 * m10
    dsc = tr * tr - 4 * det
    polesTxt | dsc >= 0 = nf 2 ((tr + sqrt dsc) / 2) ++ ", " ++ nf 2 ((tr - sqrt dsc) / 2)
             | otherwise = nf 2 (tr / 2) ++ " +/- " ++ nf 2 (sqrt (negate dsc) / 2) ++ "j"

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let (mx, my) = toP5 env p
        (ppx, ppy, ppw, pph) = panel env
    in if mx > ppx && mx < ppx + ppw && my > ppy && my < ppy + pph
         then w { wX1 = remap mx ppx (ppx + ppw) (-r1) r1, wX2 = remap my (ppy + pph) ppy (-r2) r2, wTrail = Seq.empty }
         else w
  EventKey (Char c) Down _ _ | c >= '0' && c <= '4' -> setPreset (fromEnum c - fromEnum '0') w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 0（フィードバックなし）で初期状態を置く。うずを巻いてゆっくり原点へ（極 −0.1 ± 1j）
-- 2. 2（−3, −3）と 4（−0.3 ± 3j）で同じ初期状態から。軌跡の形と u の大きさを比べる
-- 3. presets に自分の極を追加（例：−5 ± 5j）。K はどれだけ大きくなる？ u の上限 30 に当たる？
-- 4. 極を右半面（+0.5）に置いてみる → 原点から離れていく。K の符号はどうなっている？

-- ==================================================
-- ここから下は「道具」です
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を gloss の座標に直す
screen :: Env -> Picture -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- マウスの位置を p5.js の座標に直す
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字：(x, y) は文字の下端。txt = 左揃え、txtC = 中央揃え
txt, txtC :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = Translate x y (Scale 1 (-1) (label 0 0 s c str))
txtC x y s c str = txt (x - textW s str / 2) y s c str

-- 文字の幅のおよその値
textW :: Float -> String -> Float
textW s str = fromIntegral (length str) * s * 0.72

-- 左上 (x, y)、幅 w、高さ h の塗りつぶした長方形
box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2) (y + h / 2) w h)

-- 太さ wt の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg wt c a b = Color c (thickLine wt a b)

-- 太さ wt の枠
frameBox :: Float -> Color -> Float -> Float -> Float -> Float -> Picture
frameBox wt c x y w h = Color c (thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 直径 d の円
dot :: Float -> Float -> Float -> Color -> Picture
dot x y d c = Color c (at (x, y) (disc (d / 2)))

-- 小数点以下 d 桁の文字列（p5.js の nf）
nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

-- ---------- 2×2 行列 ----------
m2mul, m2add :: M2 -> M2 -> M2
m2mul (a, b, c, d) (e, f, g, h) = (a * e + b * g, a * f + b * h, c * e + d * g, c * f + d * h)
m2add (a, b, c, d) (e, f, g, h) = (a + e, b + f, c + g, d + h)

m2scale :: M2 -> Float -> M2
m2scale (a, b, c, d) s = (a * s, b * s, c * s, d * s)

m2vec :: M2 -> Point -> Point
m2vec (a, b, c, d) (x, y) = (a * x + b * y, c * x + d * y)

m2inv :: M2 -> M2
m2inv (a, b, c, d) = let e = a * d - b * c in (d / e, negate b / e, negate c / e, a / e)

-- 履歴：新しい値を足し、古い値を捨てる
type Hist = Seq Float

pushHist :: Hist -> Float -> Hist
pushHist h v =
  let v' = if isNaN v || isInfinite v then 0 else v
      h' = h |> v'
  in v' `seq` (if Seq.length h' > histMax then Seq.drop 1 h' else h')

-- グラフの線：データ、色、名前、太さ
data Series = Series !Hist !Color !String !Float

-- 色相 hue の線（彩度 70）
ser :: Hist -> Float -> String -> Series
ser h hue name = Series h (hsb hue 70 100) name 2

-- 彩度も指定する線
serS :: Hist -> Float -> Float -> String -> Series
serS h hue sat name = Series h (hsb hue sat 100) name 2

plotBg :: Color
plotBg = hsb 0 0 14

-- 時間プロット：枠 (x, y, w, h) に series を描く（縦の範囲 lo〜hi）
drawPlot :: Float -> Float -> Float -> Float -> [Series] -> Float -> Float -> String -> Picture
drawPlot x y w h series lo hi title = Pictures
  ( box x y w h plotBg
  : (if y0 > y && y0 < y + h then seg 1 (hsb 0 0 30) (x, y0) (x + w, y0) else Blank)
  : map curve series
  ++ [txt (x + 8) (y + 20) 14 (hsb 0 0 70) title]
  ++ legend (x + w - 8) series )
  where
    y0 = remap 0 lo hi (y + h) y
    px i = remap (fromIntegral i) 0 (fromIntegral histMax - 1) x (x + w)
    py v = remap (clampF lo hi v) lo hi (y + h) y
    curve (Series d c _ wt) =
      let pts = zipWith (\i v -> (px i, py v)) [0 :: Int ..] (toList d)
      in Color c (if wt <= 1 then Line pts else thickPath wt pts)
    legend _ [] = []
    legend lx (Series _ c name _ : rest) =
      let tw = textW 14 name
      in txt (lx - tw) (y + 20) 14 c name : legend (lx - tw - 14) rest
