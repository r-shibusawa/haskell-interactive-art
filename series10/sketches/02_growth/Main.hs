-- ============================================
-- 02 関数の増え方（漸近記法）
-- アルゴリズムの速さは「n が大きくなったとき、手間がどう増えるか」で比べる（O 記法・Θ 記法）
-- 左右キーで n の範囲を広げると、最初は小さかった 2^n や n^2 が、すぐに他を追い越す
-- ←→：横軸の範囲　1〜7：その関数を表示／非表示　マウスを横に動かす：その n での値を読む
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt)
import Data.List (intercalate)
import Numeric (showFFloat)

-- 値は 2^1000 のように大きいので、ここだけ Float でなく Double で計算する
funcs :: [(String, Double -> Double)]
funcs =
  [ ("1", const 1)
  , ("log2 n", \v -> logBase 2 (max v 1))
  , ("n", id)
  , ("n log2 n", \v -> v * logBase 2 (max v 1))
  , ("n^2", \v -> v * v)
  , ("n^3", \v -> v * v * v)
  , ("2^n", \v -> 2 ** v)
  ]

ranges :: [Double]
ranges = [8, 16, 32, 64, 128, 1024, 1e6]   -- ★ 横軸の最大値の候補

ops :: Double
ops = 1e9        -- ★ 1 秒に計算できる回数（いまのパソコンはだいたい 10^9 回）

data World = World
  { wRange :: !Int       -- ranges の何番目か
  , wOn    :: ![Bool]    -- 関数ごとの表示／非表示
  }

main :: IO ()
main = runSketch (sketch "10-02 Growth of Functions" (World 1 (map (const True) funcs)))
  { skBack  = hsb 220 35 9
  , skDraw  = drawW
  , skEvent = onEvent
  }

square :: Double -> Double
square v = v * v

drawW :: Env -> World -> Picture
drawW env w =
  let nMax = case drop (wRange w) ranges of
        (r : _) -> r
        [] -> 16
      l = 110
      r' = envW env - 380
      t = 130
      b = envH env - 80
      yMax = square nMax         -- 縦軸は n^2 の値に合わせる（はみ出したものは上に張りつく）
      axes = Pictures
        [ seg env (hsb 0 0 30) 1 (l, b) (r', b)
        , seg env (hsb 0 0 30) 1 (l, b) (l, t) ]
      tick :: Int -> Picture
      tick k =
        let q = fromIntegral k / 4 :: Float
            y = lerpF b t q
            lbl = fmt (yMax * fromIntegral k / 4)
        in Pictures
             [ seg env (hsb 0 0 18) 1 (l, y) (r', y)
             , txt env (l - 8 - fromIntegral (length lbl) * 10.5) (y + 7) 14 (hsb 0 0 55) lbl
             , txtC env (lerpF l r' q) (b + 18) 14 (hsb 0 0 55) (fmt (nMax * fromIntegral k / 4)) ]
      curve :: (Int, (String, Double -> Double), Bool) -> Picture
      curve (_, _, False) = Blank
      curve (i, (_, f), True) =
        let pointOf :: Int -> (Float, Float)
            pointOf px =
              let v = f (nMax * fromIntegral px / 400)
                  q = realToFrac (min v (yMax * 1.02) / yMax)
              in pt env (remap (fromIntegral px) 0 400 l r', lerpF b t q)
        in Color (hsb (fromIntegral i * 50) 70 100) (thickPath 3 (map pointOf [0 .. 400]))
      -- マウスの位置の n
      (mx0, _) = envMouse env
      mouseX = mx0 + envW env / 2
      mn = max 1 (min nMax (realToFrac (remap mouseX l r' 0 1) * nMax))
      mx = lerpF l r' (realToFrac (mn / nMax))
      dash y0 = seg env (hsb 0 0 60) 1 (mx, y0) (mx, min b (y0 + 4))
      -- 右の表：その n で、1 秒 10^9 回の計算機が何秒かかるか
      x0 = envW env - 360
      row :: (Int, (String, Double -> Double), Bool) -> Picture
      row (i, (name, f), on) =
        let v = f mn
            y = t + 34 + fromIntegral i * 30
            hue = hsb (fromIntegral i * 50) 70 (if on then 100 else 40)
            val = hsb 0 0 (if on then 90 else 40)
        in Pictures
             [ txt env x0 y 14 hue (show (i + 1) ++ "  " ++ name)
             , txt env (x0 + 115) y 14 val (fmt v)
             , txt env (x0 + 235) y 14 val (timeStr (v / ops)) ]
      table = zip3 [0 ..] funcs (wOn w)
      help = hsb 0 0 65
  in Pictures
       [ axes
       , Pictures (map tick [0 .. 4])
       , Pictures (map curve table)
       , Pictures (map dash [t, t + 10 .. b])
       , txt env x0 t 18 (hsb 0 0 100) ("n = " ++ fmt mn)
       , Pictures (map row table)
       , txt env x0 (t + 34 + 7 * 30 + 10) 14 (hsb 0 0 60) "Right column: time at 10^9 steps per second"
       , txt env 20 40 22 (hsb 0 0 100) ("Growth of functions   x axis: n = 0 to " ++ fmt nMax)
       , txt env 20 68 15 help "Left/Right: range   1-7: show/hide   Mouse: read n   (y axis fits n^2, larger values stick to the top)"
       , txt env 20 92 15 help "O(f): at most a constant times f   Theta: exactly   Omega: at least. Constants fade as n grows"
       ]

-- 数の表示
fmt :: Double -> String
fmt v
  | isInfinite v = "inf"
  | v >= 1e6 =
      let e = floor (logBase 10 v + 1e-9) :: Int   -- 1e-9 は計算誤差（log10 1e6 = 5.99…）の補正
      in fixed 1 (v / 10 ** fromIntegral e) ++ "x10^" ++ show e
  | v >= 100 = comma (round v)
  | otherwise =
      let q = fromIntegral (round (v * 10) :: Int) / 10 :: Double
      in if q == fromIntegral (round q :: Int) then show (round q :: Int) else fixed 1 q

-- 計算にかかる時間の表示
timeStr :: Double -> String
timeStr s
  | isInfinite s = "forever"
  | s < 1e-6 = fixed 1 (s * 1e9) ++ " ns"
  | s < 1e-3 = fixed 1 (s * 1e6) ++ " us"
  | s < 1 = fixed 1 (s * 1e3) ++ " ms"
  | s < 60 = fixed 1 s ++ " s"
  | s < 3600 = fixed 1 (s / 60) ++ " min"
  | s < 86400 = fixed 1 (s / 3600) ++ " hours"
  | s < 3.15e7 = fixed 1 (s / 86400) ++ " days"
  | otherwise =
      let y = s / 3.15e7
      in if y < 1e6
           then fixed 0 y ++ " years"
           else "~10^" ++ show (floor (logBase 10 y) :: Int) ++ " yr"

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wRange = min (length ranges - 1) (wRange w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wRange = max 0 (wRange w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "1234567" ->
        let i = digitToInt c - 1
        in pure w { wOn = [if k == i then not on else on | (k, on) <- zip [0 ..] (wOn w)] }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. n = 64 で 2^n は何年かかる？ 宇宙の年齢（約 1.4×10^10 年）と比べよう
-- 2. funcs に n! （階乗）や √n を追加してみる（wOn は funcs と同じ長さになる）
-- 3. 100n と n^2 はどこで逆転する？ 定数倍は「いつか」必ず負ける

-- ---------- 道具：小数と 3 けた区切り ----------
fixed :: Int -> Double -> String
fixed d v = showFFloat (Just d) v ""

comma :: Int -> String
comma v = intercalate "," (reverse (map reverse (chunks (reverse (show v)))))
  where
    chunks [] = []
    chunks s = let (a, b) = splitAt 3 s in a : chunks b

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> (Float, Float) -> (Float, Float)
pt env (x, y) = (gx env x, gy env y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> (Float, Float) -> (Float, Float) -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 文字：txt は左下が (x, y)、txtC は中心が (x, y)
txt, txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
