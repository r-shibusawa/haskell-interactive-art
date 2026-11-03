-- ============================================
-- 00 点の集まりに「形」はあるか？
-- データは点の集まり（点群）。人の目には円や 8 の字に見えるが、コンピュータにはただの座標の列
-- TDA（位相的データ解析）は「穴がいくつあるか」のように、形の本質を数で取り出す方法
-- 1〜5 で形を切りかえ / マウス上下 = ばらつき（ノイズ） / 左右 = 点の数 / クリックで作り直す
-- ============================================
module Main (main) where

import Art
import Data.List (intercalate)
import System.Random (mkStdGen, randomRs)

data World = World
  { wShape :: !Int   -- 形の番号（1〜5）
  , wSeed  :: !Int   -- 乱数の種
  }

main :: IO ()
main = runSketch (sketch "09-00 Point Cloud" (World 1 1))
  { skBack  = hsb 230 40 10
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

shapeName :: Int -> String
shapeName k = case k of
  1 -> "Circle (1 hole)"
  2 -> "2 circles (2 holes)"
  3 -> "Figure 8"
  4 -> "Blob (no hole)"
  _ -> "2 blobs"

-- ★ 形から点をサンプリングする
makePoints :: Env -> World -> Int -> Float -> [Point]
makePoints env w n noiseAmt = map sample (take n (randomChunks (wSeed w) 7))
  where
    cx = envW env / 2
    cy = envH env / 2 + 30
    bigR = min (envW env) (envH env) * 0.25
    sample (ua : us : ur : g1 : g2 : g3 : g4 : _) =
      let a = ua * tau
          s = if us < 0.5 then -1 else 1
          (x, y) = case wShape w of
            1 -> (cx + cos a * bigR, cy + sin a * bigR)
            2 -> (cx + s * bigR * 1.1 + cos a * bigR * 0.6, cy + sin a * bigR * 0.6)
            3 -> (cx + bigR * 1.2 * sin a, cy + bigR * 0.8 * sin a * cos a)
            4 -> let r = bigR * sqrt ur in (cx + cos a * r, cy + sin a * r)
            _ -> let r = bigR * 0.5 * sqrt ur in (cx + s * bigR + cos a * r, cy + sin a * r)
      in (x + gaussian g1 g2 * noiseAmt, y + gaussian g3 g4 * noiseAmt)
    sample _ = (cx, cy)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ Color (hsb 170 70 100) (Pictures [at (toG env x y) (disc 4.5) | (x, y) <- pts])
  , txt env 20 40 18 (hsb 0 0 100)
      ("Shape: " ++ shapeName (wShape w) ++ " (1-5)  Points " ++ show n ++ " (mouse X)  Spread " ++ show (round noiseAmt :: Int) ++ " (mouse Y)")
  , txt env 20 68 15 (hsb 0 0 60)
      ("The computer has only " ++ show n ++ " (x, y) pairs. How can it compute that there is a hole?  Click: new sample")
  -- 座標の列（データの正体）
  , txt env 20 (envH env - 20) 14 (hsb 0 0 45) ("Data: " ++ intercalate "  " (map showPoint (take 6 pts)) ++ "  ...")
  ]
  where
    (mx, my) = mouseXY env
    n = floor (mapC mx 0 (envW env) 20 200) :: Int
    noiseAmt = mapC my 0 (envH env) 40 0
    pts = makePoints env w n noiseAmt
    showPoint (x, y) = "(" ++ show (round x :: Int) ++ ", " ++ show (round y :: Int) ++ ")"

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '5' -> pure w { wShape = fromEnum c - fromEnum '0' }
  EventKey (MouseButton LeftButton) Down _ _ -> do
    s <- randomRIO (0, 9999)
    pure w { wSeed = s }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. ノイズを上げていくと、どこまで「円」に見える？ 自分の目の限界を知ろう
-- 2. 点を 20 個にすると、8 の字と 2 つの円の区別はつく？
-- 3. 自分の形を 6 として追加しよう（例：渦巻き、三角形の輪郭）。makePoints の case に足す

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 範囲からはみ出さない remap（p5.js の map(v, a, b, c, d, true)）
mapC :: Float -> Float -> Float -> Float -> Float -> Float
mapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)

-- 種から決まる 0〜1 の乱数を k 個ずつ区切った列（p5.js の randomSeed + random）。
-- 同じ種なら毎回同じ点群になる
randomChunks :: Int -> Int -> [[Float]]
randomChunks seed k = go (randomRs (0, 1) (mkStdGen seed))
  where go us = let (a, b) = splitAt k us in a : go b

-- 0〜1 の乱数 2 つから正規分布の乱数を作る（p5.js の randomGaussian）
gaussian :: Float -> Float -> Float
gaussian u v = sqrt (-2 * log (max 1e-6 u)) * cos (tau * v)
