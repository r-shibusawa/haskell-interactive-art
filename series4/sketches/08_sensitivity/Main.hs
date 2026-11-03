-- ============================================
-- 08 初期値敏感性（バタフライ効果を数で見る）
-- 同じ式を、最初の値が 0.000001 だけちがう 2つで計算する
-- 規則的な r では差はずっと小さいまま。カオスの r では、差が毎回「倍」になって爆発する
-- マウス左右 = r / クリックで最初の値を変える
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

newtype World = World { wX0 :: Double }   -- 最初の値

eps :: Double
eps = 0.000001               -- ★ 最初の差（小さな差を追うので Double で計算する）

years :: Int
years = 60                   -- ★ 何年ぶん計算するか

main :: IO ()
main = runSketch (sketch "04-08 Sensitivity" (World 0.3))
  { skBack  = hsb 230 40 10
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

series :: Double -> Double -> [Double]
series r x0 = take years (iterate (\x -> r * x * (1 - x)) x0)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  ( [ drawSeries as 170, drawSeries bs 20, splitMark, logAxis, logCurve
    , txt env 20 40 18 white ("r = " ++ fmt 3 r ++ " (mouse X)   Start gap = 0.000001   Click: new start value")
    , txt env 20 70 14 (hsb 0 0 70)
        "Top: the two series.  Bottom: their gap (log scale).  Straight rise = same factor each year = chaos"
    ])
  where
    (mx, _) = mouseP5 env
    r = realToFrac (remapC mx 0 (envW env) 2.8 4.0) :: Double
    as = series r (wX0 w)
    bs = series r (wX0 w + eps)
    gaps = zipWith (\a b -> abs (a - b)) as bs
    left = 80
    right = envW env - 60
    xAt :: Int -> Float
    xAt i = remap (fromIntegral i) 0 (fromIntegral (years - 1)) left right
    -- ---- 上：2つの系列 ----
    top1 = 110
    bot1 = envH env * 0.5
    drawSeries xs hue = Color (hsb hue 70 100) $ thickPath 2
      [sc env (xAt i) (remap (toF x) 0 1 bot1 top1) | (i, x) <- zip [0 ..] xs]
    -- 2つが離れた最初の年を探す
    splitYear = case [i | (i, g) <- zip [0 :: Int ..] gaps, g > 0.01] of
      (i : _) -> Just i
      [] -> Nothing
    splitMark = case splitYear of
      Nothing -> Blank
      Just i ->
        let sx = xAt i
            msg = "They split here (year " ++ show i ++ ")"
            tx = if sx > envW env - 340 then sx - 330 else sx + 8     -- 右端では左に書く
        in Pictures
             [ Color (hsb 40 90 100) (thickLine 2 (sc env sx top1) (sc env sx bot1))
             , txt env tx (top1 + 20) 16 (hsb 40 90 100) msg ]
    -- ---- 下：差の大きさ（対数目盛）----
    top2 = envH env * 0.58
    bot2 = envH env - 60
    logAxis = Pictures
      ( Color (hsb 0 0 40) (thickLine 1 (sc env left bot2) (sc env right bot2))
      : [ Pictures
            [ Color (hsb 0 0 25) (thickLine 1 (sc env left y) (sc env right y))
            , txt env (left - 66) (y + 5) 14 (hsb 0 0 60) ("10^" ++ show e) ]
        | e <- [-6 .. 0 :: Int]
        , let y = remap (fromIntegral e) (-6) 0 bot2 top2 ] )
    logCurve = Color (hsb 40 90 100) $ thickPath 2.5
      [ sc env (xAt i) (remap (toF (logBase 10 (max g 1e-7))) (-6) 0 bot2 top2)
      | (i, g) <- zip [0 ..] gaps ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ (EventKey (MouseButton LeftButton) Down _ _) w = do
  x0 <- randomRIO (0.05, 0.95)
  pure w { wX0 = x0 }
onEvent _ _ w = pure w

-- ---------- やってみよう ----------
-- 1. r = 3.0（規則的）では、下のグラフは下に張りついたまま。r = 3.9 では右上へ一直線
-- 2. 最初の差 eps を 0.0000000001 にすると、別れるのは何年遅れる？（たった数年！）
-- 3. 下のグラフの傾きが「リアプノフ指数」。天気予報が2週間以上当たらない理由

-- ---------- 道具 ----------

toF :: Double -> Float
toF = realToFrac

-- p5.js 風の座標（左上が原点、y は下向き）を gloss の座標に直す
sc :: Env -> Float -> Float -> Point
sc env x y = (x - envW env / 2, envH env / 2 - y)

mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size col s = let (gx, gy) = sc env x y in label gx gy size col s

remapC :: Float -> Float -> Float -> Float -> Float -> Float
remapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)

fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) v ""
