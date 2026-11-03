-- ============================================
-- 11 倒立振子（台車で棒を立てる）
-- 手のひらでほうきを立てるのと同じ。倒れそうな向きに台車を動かして支える
--   状態 x = [台車の位置, 速度, 棒の角度, 角速度]（4 つ）
--   直立まわりで線形化 x' = Ax + Bu → 極配置で K を求め、u = −K x
-- クリック = 棒をつつく / 1〜3 = 配置する極 / 上下キー = ゲインの倍率 / R でやり直し
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (foldl', maximumBy, transpose)
import Data.Ord (comparing)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (printf)

-- 数値微分や逆行列は桁が必要なので、力学は Double で計算する
cartM, poleM, poleL, grav, fMax, dt :: Double
cartM = 1.0     -- ★ 台車の質量
poleM = 0.1     -- ★ 棒の質量
poleL = 0.5     -- ★ 棒の半分の長さ
grav = 9.8
fMax = 30       -- ★ モーターの最大の力
dt = 1 / 60

histMax :: Int
histMax = 600

-- ★ 配置する極（4 つ）
presets :: [(String, [Double])]
presets =
  [ ("slow -1.5, -2, -2.5, -3", [-1.5, -2, -2.5, -3])
  , ("standard -3, -3.5, -4, -4.5", [-3, -3.5, -4, -4.5])
  , ("strong -6, -7, -8, -9", [-6, -7, -8, -9])
  ]

data World = World
  { wPreset :: !Int
  , wGain   :: !Float      -- ★ 上下キーで K 全体を何倍にするか（1 以外だと設計とずれる）
  , wKf     :: ![Double]
  , wX, wXd, wTh, wThd :: !Double   -- 状態 [x, ẋ, θ, θ̇]
  , wF      :: !Double
  , wFallen :: !Bool
  , wPoke   :: !Int        -- つついた印を出すフレーム数
  , wPokeDir :: !Float
  , hTh, hX, hF :: !Hist
  }

world0 :: World
world0 = World
  { wPreset = 1, wGain = 1, wKf = design 1, wX = 0, wXd = 0, wTh = 0.1, wThd = 0, wF = 0
  , wFallen = False, wPoke = 0, wPokeDir = 0, hTh = Seq.empty, hX = Seq.empty, hF = Seq.empty }

main :: IO ()
main = runSketch (sketch "20-11 Cart Pole" world0)
  { skBack  = hsb 230 30 10
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawAll
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

-- 直立まわりで線形化して、極配置で K を求める
design :: Int -> [Double]
design i =
  let (a, b) = linearize cartPoleDeriv [0, 0, 0, 0] 0
      poles = case drop i presets of { ((_, ps) : _) -> ps; [] -> [-3, -3.5, -4, -4.5] }
  in ackermann a b poles

-- ★ 倒立振子の運動方程式（θ = 0 が直立、右に倒れると正）
cartPoleDeriv :: [Double] -> Double -> [Double]
cartPoleDeriv s u = case s of
  [_, xd, th, thd] ->
    let sn = sin th
        cs = cos th
        tmp = (u + poleM * poleL * thd * thd * sn) / (cartM + poleM)
        thdd = (grav * sn - cs * tmp) / (poleL * (4 / 3 - (poleM * cs * cs) / (cartM + poleM)))
        xdd = tmp - (poleM * poleL * thdd * cs) / (cartM + poleM)
    in [xd, xdd, thd, thdd]
  _ -> []

step :: World -> World
step w@World{..} =
  let -- ★ 状態フィードバック u = −K x（倒れたら制御をやめる）
      fb = negate (realToFrac wGain * sum (zipWith (*) wKf [wX, wXd, wTh, wThd]))
      f = if wFallen then 0 else max (-fMax) (min fMax fb)
      lim lo hi = max lo . min hi
      sub (x, xd, th, thd) _ = case cartPoleDeriv [x, xd, th, thd] f of
        [d0, d1, d2, d3] ->
          let x1 = x + d0 * dt / 4
              xd1 = xd + d1 * dt / 4
              th1 = th + d2 * dt / 4
              thd1 = thd + d3 * dt / 4
              hit = abs x1 > 4                         -- レールの端で跳ね返る
              x2 = if hit then lim (-4) 4 x1 else x1
              xd2 = if hit then xd1 * (-0.3) else xd1
          in (x2, lim (-30) 30 xd2, th1, lim (-40) 40 thd1)
        _ -> (x, xd, th, thd)
      (x, xd, th, thd) = foldl' sub (wX, wXd, wTh, wThd) [1 .. 4 :: Int]
      fl v = realToFrac v :: Float
  in w { wX = x, wXd = xd, wTh = th, wThd = thd, wF = f
       , wFallen = wFallen || abs th > 1
       , wPoke = max 0 (wPoke - 1)
       , hTh = pushHist hTh (fl th * 180 / pi), hX = pushHist hX (fl x), hF = pushHist hF (fl f) }

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures
  [ seg 4 (hsb 0 0 50) (rx0, railY + 20) (rx1, railY + 20)
  , box (cx - 35) (railY - 20) 70 40 (hsb 30 80 100)
  , dot (cx - 20) (railY + 16) 16 (hsb 0 0 30)
  , dot (cx + 20) (railY + 16) 16 (hsb 0 0 30)
  , seg 8 poleC (cx, railY - 20) (tipX, tipY)
  , dot tipX tipY 22 poleC
    -- 力の矢印
  , seg 5 (hsb 120 80 100) (cx, railY) (cx + f wF * 3, railY)
    -- つついた印
  , if wPoke > 0 then txtC tipX (tipY - 30) 28 (hsb 45 90 100) (if wPokeDir > 0 then "-> poke" else "poke <-") else Blank
  , txtC cx (railY + 50) 14 white ("theta = " ++ nf 1 (th * 180 / pi) ++ " deg   x = " ++ nf 2 (f wX))
  , if wFallen then txt (rx0 + 20) (ht * 0.25) 32 (hsb 0 80 100) "Fallen! R: restart" else Blank
    -- グラフ
  , drawPlot gx (ht * 0.16) gw (ht * 0.24) [ser hTh 200 "Angle theta [deg]"] (-30) 30 "Pole angle"
  , drawPlot gx (ht * 0.43) gw (ht * 0.2) [ser hX 30 "Cart position x"] (-4) 4 "Cart"
  , drawPlot gx (ht * 0.66) gw (ht * 0.2) [ser hF 120 "Force F"] (negate (f fMax)) (f fMax) "Control input"
  , txt 20 40 18 white ("Poles: " ++ presetName ++ " (1-3)   Gain x" ++ nf 1 wGain ++ " (Up/Down)   Click: poke   R: restart")
  , txt 20 68 15 c60 ("K = [" ++ kText ++ "]  u = -Kx. Pole falling right: move the cart right, under the pole.")
  , txt 20 90 15 c60 "Too small a gain cannot hold it; too large hits the force limit and rattles."
  , txt 20 (ht - 30) 15 c60 "Linearize, then 4th-order pole placement (Ackermann). Segway, rocket attitude, biped walking: same basic problem."
  ]
  where
    wd = envW env
    ht = envH env
    f = realToFrac :: Double -> Float
    th = f wTh
    railY = ht * 0.62
    rx0 = wd * 0.05
    rx1 = wd * 0.47
    xOf v = remap v (-4) 4 rx0 rx1
    l = (rx1 - rx0) / 8 * 2 * f poleL * 2
    cx = xOf (f wX)
    tipX = cx + l * sin th
    tipY = railY - 20 - l * cos th
    poleC = hsb (if wFallen then 0 else 200) 60 100
    gx = wd * 0.52
    gw = wd * 0.44
    c60 = hsb 0 0 60
    presetName = case drop wPreset presets of { ((s, _) : _) -> s; [] -> "" }
    kText = commaSep (map (\v -> nf 1 (f v * wGain)) wKf)
    commaSep [] = ""
    commaSep [s] = s
    commaSep (s : r) = s ++ ", " ++ commaSep r

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p
    | wFallen w -> resetAll w
    | otherwise ->
        let (mx, _) = toP5 env p
            cartX = remap (realToFrac (wX w)) (-4) 4 (envW env * 0.05) (envW env * 0.47)
            d = if mx > cartX then -1 else 1      -- 棒の右側をクリック → 左へ押す
        in w { wPokeDir = d, wThd = wThd w + realToFrac d * 1.2, wPoke = 30 }   -- ★ つつく強さ
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' ->
        let i = fromEnum c - fromEnum '1' in w { wPreset = i, wKf = design i }
    | toLower c == 'r' -> resetAll w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wGain = min (wGain w + 0.1) 3 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wGain = max (wGain w - 0.1) 0 }
  _ -> w

resetAll :: World -> World
resetAll w = w { wX = 0, wXd = 0, wTh = 0.1, wThd = 0, wFallen = False }

-- ---------- やってみよう ----------
-- 1. 倍率を 0.1 ずつ下げていく。どこで倒れる？（線形化モデルでは安定でも、力の上限と非線形で倒れる）
-- 2. 「強い」極で強くつつく。力が上限（±30）に張りつくと救えない
-- 3. 棒を長く（poleL = 1.0）すると支えやすい？ 短く（0.2）すると？ ほうきと鉛筆のちがい
-- 4. K の符号を見る。位置 x の係数が負（目標から離れる向きに一瞬動く）なのはなぜ？

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

-- ---------- 線形化と極配置（n 次元の行列はリストのリスト） ----------
type Mat = [[Double]]

-- 数値微分で x' = f(x, u) を x0, u0 のまわりで線形化 → A（n×n）, B（n）
linearize :: ([Double] -> Double -> [Double]) -> [Double] -> Double -> (Mat, [Double])
linearize f x0 u0 =
  let h = 1e-5
      bump j d = [ if i == j then v + d else v | (i, v) <- zip [0 :: Int ..] x0 ]
      diff fp fm = zipWith (\p m -> (p - m) / (2 * h)) fp fm
      cols = [ diff (f (bump j h) u0) (f (bump j (negate h)) u0) | j <- [0 .. length x0 - 1] ]
  in (transpose cols, diff (f x0 (u0 + h)) (f x0 (u0 - h)))

-- アッカーマンの公式：K = [0 … 0 1] · Wc⁻¹ · φ(A)、Wc = [B AB A²B …]、φ = 望む特性多項式
ackermann :: Mat -> [Double] -> [Double] -> [Double]
ackermann a b poles =
  let n = length a
      cols = take n (iterate (matVec a) b)
      wc = transpose cols
      alpha = foldl' (\acc p -> polyMulReal acc [negate p, 1]) [1] poles   -- (s − p1)(s − p2)…（昇べき）
      powers = iterate (`matMul` a) (matIdentity n)
      phi = foldl' matAdd (matZero n) (zipWith (\c m -> map (map (* c)) m) alpha powers)
  in case reverse (matMul (matInv wc) phi) of
       (lastRow : _) -> lastRow
       [] -> []

matZero, matIdentity :: Int -> Mat
matZero n = replicate n (replicate n 0)
matIdentity n = [ [ if i == j then 1 else 0 | j <- [1 .. n] ] | i <- [1 .. n] ]

matAdd, matMul :: Mat -> Mat -> Mat
matAdd = zipWith (zipWith (+))
matMul p q = let cols = transpose q in [ [ sum (zipWith (*) r c) | c <- cols ] | r <- p ]

matVec :: Mat -> [Double] -> [Double]
matVec p v = [ sum (zipWith (*) r v) | r <- p ]

polyMulReal :: [Double] -> [Double] -> [Double]
polyMulReal a b = foldr (\ai acc -> addL (map (* ai) b) (0 : acc)) [] a
  where
    addL (x : xs) (y : ys) = (x + y) : addL xs ys
    addL xs [] = xs
    addL [] ys = ys

-- 逆行列（ガウス・ジョルダン法）
matInv :: Mat -> Mat
matInv p =
  let n = length p
      aug = zipWith (++) p (matIdentity n)
      elim m col =
        let (done, rest) = splitAt col m
            piv = maximumBy (comparing (\(_, r) -> abs (r !! col))) (zip [0 :: Int ..] rest)
            (pi', pr) = piv
            others = [ r | (i, r) <- zip [0 ..] rest, i /= pi' ]
            d0 = pr !! col
            d = if d0 == 0 then 1e-12 else d0
            prN = map (/ d) pr
            clear r = let k = r !! col in zipWith (\a b -> a - k * b) r prN
        in map clear done ++ prN : map clear others
  in map (drop n) (foldl' elim aug [0 .. n - 1])

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
