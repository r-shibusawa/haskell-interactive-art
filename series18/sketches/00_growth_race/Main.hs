-- ============================================
-- 00 多項式時間 vs 指数時間
-- n を増やすと、計算回数 n, n^2, n^3, 2^n, n! はどれだけ増える？ 1 秒に 10 億回計算できるコンピュータで何秒かかる？
-- 「多項式時間（n の何乗か）で解ける」＝ 現実的に解ける。2^n や n! は n = 60 で宇宙の年齢を超える
-- 左右キー / マウスの横位置 で n を変える / スペースで n を自動で増やす / 1：最適化問題 2：判定問題（Yes/No）
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

data World = World
  { wN    :: !Int
  , wAuto :: !Bool
  , wMode :: !Int
  }

speedOps :: Double
speedOps = 1e9      -- ★ 1 秒に何回計算できるか（10 億回）

maxLog :: Float
maxLog = 120        -- 目盛りは 10^120 まで

data Func = Func { fName :: String, fOf :: Double -> Double, fHue :: Float }

-- n! は Float に入りきらないので、ここだけ Double で計算する
funcs :: [Func]
funcs =
  [ Func "n"       id                               120
  , Func "n log n" (\n -> n * logBase 2 (max n 2))  160
  , Func "n^2"     (\n -> n * n)                    190
  , Func "n^3"     (\n -> n * n * n)                220
  , Func "2^n"     (\n -> 2 ** n)                   30
  , Func "n!"      factorial                        0
  ]

factorial :: Double -> Double
factorial k = product [2 .. k]

main :: IO ()
main = runSketch (sketch "18-00 Polynomial vs Exponential" (World 10 False 1))
  { skBack  = hsb 230 30 10
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

stepW :: Env -> Float -> World -> IO World
stepW env _ w
  | wAuto w && envFrame env `mod` 12 == 0 =
      pure w { wN = if wN w >= 80 then 1 else wN w + 1 }
  | otherwise = pure w

drawW :: Env -> World -> Picture
drawW env w =
  let n = wN w
      left = 170
      right = envW env - 330
      top = 130
      rowH = min 70 ((envH env - 330) / fromIntegral (length funcs))
      bottom = top + rowH * fromIntegral (length funcs)
      row (i, f) =
        let v = fOf f (fromIntegral n)
            lg = if v <= 1 then 0 else realToFrac (logBase 10 v) :: Float
            y = top + fromIntegral (i :: Int) * rowH
            bw = remap (min lg maxLog) 0 maxLog 4 (right - left)
        in Pictures
             [ txtR env (left - 16) (y + rowH / 2) 18 (hsb 0 0 70) (fName f)
             , box env (hsb 0 0 20) left (y + 8) (right - left) (rowH - 16)
             , box env (hsb (fHue f) 75 95) left (y + 8) bw (rowH - 16)
             , txtM env (right + 16) (y + rowH / 2 - 11) 16 (hsb 0 0 100) (fmtCount v ++ " ops")
             , txtM env (right + 16) (y + rowH / 2 + 13) 15 (hsb (fHue f) 40 100) (fmtTime (v / speedOps))
             ]
      tick e =
        let x = remap e 0 maxLog (left + 4) right
        in Pictures
             [ seg env (hsb 0 0 35) 1 (x, top) (x, bottom)
             , txtC env x (bottom + 16) 14 (hsb 0 0 55) ("10^" ++ show (round e :: Int))
             ]
      by = bottom + 50
      explain
        | wMode w == 1 =
            [ txt env 20 (by + 16) 16 (hsb 50 60 100) "Optimization: \"What is the shortest tour?\" \"The smallest vertex cover?\""
            , txt env 20 (by + 42) 16 (hsb 0 0 80) "The answer is a number. NP-completeness theory first restates it as a decision: press 2"
            ]
        | otherwise =
            [ txt env 20 (by + 16) 16 (hsb 190 60 100) "Decision: \"Is there a tour of length <= k?\"  The answer is just Yes or No"
            , txt env 20 (by + 42) 16 (hsb 0 0 80) "Decision hard => optimization hard (knowing the optimum decides at once)."
            ]
  in Pictures
       ( map row (zip [0 ..] funcs)
      ++ map tick [0, 20 .. maxLog]
      ++ explain
      ++ [ txt env 20 36 16 (hsb 0 0 100) "Left/Right or mouse: change n    Space: auto    1: optimization    2: decision"
         , txt env 20 92 36 (hsb 50 80 100) ("n = " ++ show n)
         , txt env 20 (by + 74) 15 (hsb 0 0 60) "Log scale: one tick = 10^20 times. Age of the universe is about 4x10^17 seconds."
         , txt env 20 (by + 98) 15 (hsb 0 0 60) "Input size n counts bits: writing a number N takes n = log2 N bits."
         ] )

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wN = min 80 (wN w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wN = max 1 (wN w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wAuto = not (wAuto w) }
  EventKey (Char '1') Down _ _ -> pure w { wMode = 1 }
  EventKey (Char '2') Down _ _ -> pure w { wMode = 2 }
  EventMotion p ->   -- ドラッグ中もここに来る
    let (mx, my) = mouseP5 env p
    in if not (wAuto w) && my > 110
         then pure w { wN = max 1 (min 80 (floor (remap mx 0 (envW env) 1 81))) }
         else pure w
  _ -> pure w

fmtCount :: Double -> String
fmtCount v
  | isInfinite v = "inf"
  | v < 1e6 = show (round v :: Int)
  | otherwise =
      let e = floor (logBase 10 v + 1e-12) :: Int
      in fix 2 (v / 10 ^^ e) ++ "x10^" ++ show e

fmtTime :: Double -> String
fmtTime s
  | isInfinite s = "cannot compute"
  | s < 1e-6 = fix 1 (s * 1e9) ++ " ns"
  | s < 1e-3 = fix 1 (s * 1e6) ++ " us"
  | s < 1 = fix 1 (s * 1e3) ++ " ms"
  | s < 60 = fix 1 s ++ " s"
  | s < 3600 = fix 1 (s / 60) ++ " min"
  | s < 86400 = fix 1 (s / 3600) ++ " hours"
  | s < 3.15e7 = fix 1 (s / 86400) ++ " days"
  | y < 1e6 = show (round y :: Int) ++ " years"
  | otherwise =
      "10^" ++ show (floor (logBase 10 y) :: Int) ++ " yr" ++ (if s > 4e17 then " (> universe)" else "")
  where y = s / 3.15e7

-- ---------- やってみよう ----------
-- 1. n^3 が 1 秒を超える n と、2^n が 1 秒を超える n を比べよう
-- 2. speedOps を 1000 倍にしても 2^n で解ける n はいくつ増える？（たった 10）
-- 3. funcs に n^5 や 1.1^n を足してみよう。いつ逆転する？

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- gloss のマウス座標を p5.js の座標に直す
mouseP5 :: Env -> Point -> Point
mouseP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 文字：左端の下が (x, y)
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str

-- 文字：中心が (x, y)
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = labelC (gx env x) (gy env y) s c str

-- 文字：左端の上下中央が (x, y)
txtM :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtM env x y s c str = label (gx env x) (gy env y - s / 2) s c str

-- 文字：右端の上下中央が (x, y)
txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtR env x y s c str =
  label (gx env x - fromIntegral (length str) * s * 0.72) (gy env y - s / 2) s c str

-- ---------- 道具：その他 ----------
-- 小数点以下 d 桁で表示（p5.js の nf）
fix :: RealFloat a => Int -> a -> String
fix d v = showFFloat (Just d) v ""
