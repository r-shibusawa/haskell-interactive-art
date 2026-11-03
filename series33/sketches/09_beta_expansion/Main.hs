-- ============================================
-- 09 ベータ展開 ─ φ 進法とツェッケンドルフ表現
-- 10 進法の「位」は 1, 10, 100 …。φ 進法では位が φ^k（…, 1/φ, 1, φ, φ², …）で、数字は 0 と 1 だけ
--   φ² = φ + 1 なので「011 = 100」。だから 1 が 2 つ続かない形にそろえられる（白銀比なら数字は 0〜2）
-- 下は整数をフィボナッチ数の和で表すツェッケンドルフ表現（となりどうしを使わない）
-- スペース = 自動カウント / ←→ = 数を 1 ずつ / ↑↓ = n（1 黄金・2 白銀・3 青銅）/ M = マウスで小数も
-- ============================================
module Main (main) where

import Art
import Data.List (dropWhileEnd, intercalate, mapAccumL)
import Numeric (showEFloat, showFFloat)

data World = World
  { wNum   :: !Int    -- 表す数
  , wN     :: !Int    -- ★ 底 β = φn
  , wAuto  :: !Bool
  , wMouse :: !Bool
  , wTimer :: !Int
  }

kLo, kHi :: Int
kLo = -10             -- ★ 表示する位の範囲
kHi = 11

initialWorld :: World
initialWorld = World 1 1 True False 0

main :: IO ()
main = runSketch (sketch "33-09 Beta Expansion" initialWorld)
  { skBack  = hsb 200 30 10
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

stepWorld :: World -> World
stepWorld w
  | wAuto w && not (wMouse w) =
      if wTimer w + 1 > 50
        then w { wTimer = 0, wNum = if wNum w >= 150 then 0 else wNum w + 1 }
        else w { wTimer = wTimer w + 1 }
  | otherwise = w

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (hud ++ cells ++ [point, expansion] ++ blocks ++ zeck)
  where
    n = wN w
    beta = metallic (fromIntegral n)
    (mx0, _) = envMouse env
    mouseX = clampF 0 (envW env) (mx0 + envW env / 2)
    x :: Double
    x | wMouse w = realToFrac (mouseX / envW env * 40)
      | otherwise = fromIntegral (wNum w)
    digits = betaDigits x beta
    xText = if wMouse w then nfD 4 x else show (wNum w)
    hud =
      [ txt env 20 28 15 white ("x = " ++ xText ++ " (L/R, Space auto " ++ onOff (wAuto w) ++ ", M mouse " ++ onOff (wMouse w) ++ ")   base beta = phi" ++ show n ++ " = " ++ nfD 4 beta ++ " (Up/Dn)")
      , txt env 20 58 14 (gray 0.75) ("Greedy: fill big places first. Digits are 0-" ++ show n ++ "; after a " ++ show n ++ " comes a 0, automatically.")
      ]
    -- 位のマス
    cols = kHi - kLo + 1
    cw = min 64 ((envW env - 80) / fromIntegral cols)
    x0 = 40
    y0 = 110
    cells = concat [ cell k d | (k, d) <- digits ]
    cell :: Int -> Int -> [Picture]
    cell k d =
      let i = kHi - k
          cx = x0 + fromIntegral i * cw + (if k < 0 then 14 else 0)
          mid = cx + (cw - 6) / 2
          fd = fromIntegral d :: Float
      in [ Color (if d > 0 then hsb (45 + (fd - 1) * 140) 80 100 else hsb 0 0 22) (box env cx y0 (cw - 6) (cw - 6))
         , txtC env mid (y0 + (cw - 6) / 2) (max 14 (cw * 0.45)) (if d > 0 then black else gray 0.6) (show d)
         , txtC env mid (y0 + cw + 6) 14 (gray 0.7) ("^" ++ show k)
         , txtC env mid (y0 + cw + 24) 14 (gray 0.7) (placeText (beta ^^ k))
         ]
    point = Color white (at (pt env (x0 + fromIntegral (kHi + 1) * cw + 4) (y0 + cw - 10)) (disc 3))    -- 小数点
    -- 文字列としての表現
    expansion = txt env 40 (y0 + cw + 64) 22 (hsb 45 60 100)
      ("(" ++ xText ++ ")_10 = (" ++ digitString digits ++ ")_phi" ++ show n)
    -- 数直線：位の長さのブロックを積み上げる
    ly = y0 + cw + 110
    lw = envW env - 80
    unit = lw / max 10 (realToFrac x * 1.1)
    flat = [ k | (k, d) <- digits, _ <- [1 .. d] ]
    (total, placed) = mapAccumL (\p k -> (p + beta ^^ k, (p, k))) 0 flat
    blocks =
      [ Color (withA 0.4 white) (Line [pt env 40 (ly + 30), pt env (40 + lw) (ly + 30)]) ]
      ++ concat
        [ let bl = realToFrac (beta ^^ k) * unit
              bx = 40 + realToFrac p * unit
              hue = wrapF 360 (fromIntegral (k * 40 + 400))
          in Color (hsba hue 60 95 0.85) (box env bx ly (max 1 (bl - 1)) 26)
             : [ txt env (bx + 4) (ly + 19) 14 (hsb 0 0 10) ("^" ++ show k) | bl > 34 ]
        | (p, k) <- placed ]
      ++ [ Color white (Polygon [ pt env tipX (ly - 4), pt env (tipX + 6) (ly - 14), pt env (tipX - 6) (ly - 14) ])
         , txt env 40 (ly + 52) 14 (gray 0.75)
             ("Block length = " ++ nfD 6 total ++ " (error " ++ showEFloat (Just 1) (abs (total - x)) "" ++ ")")
         ]
    tipX = 40 + realToFrac x * unit
    zeck = if wMouse w then [] else drawZeckendorf env n (wNum w) 40 (ly + 100)

-- ツェッケンドルフ（オストロフスキー）表現：整数を数列の和で
drawZeckendorf :: Env -> Int -> Int -> Float -> Float -> [Picture]
drawZeckendorf env n m x0 y0 =
  [ txt env x0 y0 15 white (title ++ ": " ++ show m ++ " = " ++ sumText) ]
  ++ concat (zipWith bulb [0 ..] (zip gs use))
  ++ [ txt env x0 (y0 + 100) 14 (gray 0.75) "Neighbor bulbs are never both on (n=1). Taking the biggest first guarantees this." ]
  where
    -- 数列 1, n, n²+1, …（n=1 なら 1, 2, 3, 5, 8, …）
    gsAll = grow [n, 1]
    grow acc@(a : b : _) | a < 400 = grow (n * a + b : acc)
    grow acc = reverse acc
    gs = if n == 1 then drop 1 gsAll else gsAll
    -- 大きい方から貪欲に取る
    use = reverse (snd (mapAccumL (\rest g -> (rest - (rest `div` g) * g, rest `div` g)) m (reverse gs)))
    title | n == 1 = "Zeckendorf (Fibonacci sum)"
          | n == 2 = "Pell number sum"
          | otherwise = "Sum of a(k+1) = " ++ show n ++ "a(k) + a(k-1)"
    terms = [ (if u > 1 then show u ++ "x" else "") ++ show g | (g, u) <- zip gs use, u > 0 ]
    sumText = if null terms then "0" else intercalate " + " (reverse terms)
    bw = min 70 ((envW env - 80) / fromIntegral (max 1 (length gs)))
    bulb :: Int -> (Int, Int) -> [Picture]
    bulb i (g, u) =
      let on = u > 0
          cx = x0 + fromIntegral i * bw + bw / 2
      in [ Color (if on then hsb 190 70 100 else hsb 0 0 20) (at (pt env cx (y0 + 40)) (disc (bw * 0.3)))
         , txtC env cx (y0 + 40) 14 (if on then black else gray 0.6) (show g)
         ]
         ++ [ txtC env cx (y0 + 74) 14 (hsb 50 90 100) ("x" ++ show u) | u > 1 ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wNum = min (wNum w + 1) 199 }
  EventKey (SpecialKey KeyLeft) Down _ _  -> w { wNum = max (wNum w - 1) 0 }
  EventKey (SpecialKey KeyUp) Down _ _    -> w { wN = min (wN w + 1) 4 }
  EventKey (SpecialKey KeyDown) Down _ _  -> w { wN = max (wN w - 1) 1 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  EventKey (Char c) Down _ _
    | c == 'm' || c == 'M' -> w { wMouse = not (wMouse w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. φ 進法で 1, 2, 3 を書くと？（1 = 1、2 = 10.01、3 = 100.01）。2 = φ + 1/φ² を確かめよう
-- 2. どの整数も「有限の桁」で書けるのは φ が特別な数（ピゾ数）だから。↑↓ で白銀比・青銅比も試そう
-- 3. M で x = 1/2 のような小数にすると、桁が kLo まで続く（終わらない）ことがある
-- 4. ツェッケンドルフ表現の電球で、数を 1 ずつ増やすと電球はどう繰り上がる？（011 → 100）

-- ---------- 道具 ----------
metallic :: Double -> Double
metallic k = (k + sqrt (k * k + 4)) / 2

-- x の β 進展開（貪欲法）。(k, β^k の位の数字) を大きい位から順に返す
betaDigits :: Double -> Double -> [(Int, Int)]
betaDigits x beta = snd (mapAccumL place x [kHi, kHi - 1 .. kLo])
  where
    place rest k =
      let p = beta ^^ k
          c = max 0 (min (floor beta) (floor (rest / p + 1e-9))) :: Int
          r = rest - fromIntegral c * p
      in (if r < 1e-9 then max 0 r else r, (k, c))

-- 012.340 のような文字列にして、前後のいらない 0 を取る
digitString :: [(Int, Int)] -> String
digitString ds =
  let intPart = concat [ show d | (k, d) <- ds, k >= 0 ]
      fracPart = concat [ show d | (k, d) <- ds, k < 0 ]
      i = case dropWhile (== '0') intPart of { "" -> "0"; s -> s }
      f = dropWhileEnd (== '0') fracPart
  in if null f then i else i ++ "." ++ f

-- 位の大きさ（マスに入るように短く書く）
placeText :: Double -> String
placeText v
  | v >= 10000 = showEFloat (Just 0) v ""
  | v >= 100 = show (round v :: Int)
  | v >= 1 = nfD 1 v
  | otherwise = drop 1 (nfD 3 v)       -- 0.618 → .618

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

nfD :: Int -> Double -> String
nfD d v = showFFloat (Just d) v ""

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

-- 中央揃えの文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = let (gx, gy) = pt env x y in labelC gx gy s c str

box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = Polygon [pt env x y, pt env (x + w) y, pt env (x + w) (y + h), pt env x (y + h)]
