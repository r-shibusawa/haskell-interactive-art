-- ============================================
-- 06 エピサイクル（フーリエ変換でお絵かき）
-- マウスで描いた線を、フーリエ変換で「回転する円の組み合わせ」に分解する
-- 円を何個も重ねて回すと、元の絵がそっくり描き直される
-- ドラッグで描く → 離すと再生 / マウス左右（再生中）= 使う円の数 / C で消す
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', sortOn)
import qualified Data.Vector.Unboxed as U

-- 回転する円 1 つぶん
data Epi = Epi
  { eFreq  :: !Float   -- 回る速さ（負なら逆回り）
  , eAmp   :: !Float   -- 半径
  , ePhase :: !Float   -- はじめの角度
  }

data Mode = Sketching | Playing deriving Eq

data World = World
  { wMode    :: !Mode
  , wDrawing :: ![Point]   -- 描いた点（描いている間は新しいものが先頭）
  , wEpis    :: ![Epi]     -- 分解結果（大きい円から順）
  , wTime    :: !Float
  , wPath    :: ![Point]   -- 再生で描いた点（新しいものが先頭）
  , wNum     :: !Int       -- 今使っている円の数
  , wArms    :: ![Point]   -- 中心から先端までの腕の節
  }

maxDrawing :: Int
maxDrawing = 3000          -- ★ 1 回に描ける点の数（ここまでで止まる）

pointsWanted :: Int
pointsWanted = 300         -- ★ 計算を軽くするために減らす目安の点の数

main :: IO ()
main = runSketch (sketch "05-06 Epicycles" (World Sketching [] [] 0 [] 0 []))
  { skBack  = hsb 220 40 10
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

step :: Env -> World -> World
step env w
  | wMode w == Sketching = w
  | otherwise =
      let total = length (wEpis w)
          (mx, _) = envMouse env
          num = floor (mapC (mx + envW env / 2) 0 (envW env) 1 (fromIntegral total)) :: Int
          -- ---- 円を重ねて、先端の位置を求める ----
          arms = scanl (addEpi (wTime w)) (0, 0) (take num (wEpis w))
          tip = foldl' (\_ p -> p) (0, 0) arms
          -- 軌跡は元の絵の点の数まで
          path = take (length (wDrawing w)) (tip : wPath w)
          time = wTime w + tau / fromIntegral (length (wDrawing w))
      in if time > tau
           then w { wNum = num, wArms = arms, wPath = [], wTime = 0 }
           else w { wNum = num, wArms = arms, wPath = path, wTime = time }

-- 1 つ前の節の位置に、円 1 つぶんの腕を足す
addEpi :: Float -> Point -> Epi -> Point
addEpi t (x, y) e =
  ( x + eAmp e * cos (eFreq e * t + ePhase e)
  , y + eAmp e * sin (eFreq e * t + ePhase e) )

-- ★ 2 次元の DFT：x を実部、y を虚部とみなして分解する
dft2D :: [Point] -> [Epi]
dft2D pts = map term [0 .. n - 1]
  where
    n = length pts
    fn = fromIntegral n :: Float
    xs = U.fromList (map fst pts)
    ys = U.fromList (map snd pts)
    term k =
      let phi i = tau * fromIntegral (k * i) / fn
          re = U.sum (U.imap (\i x -> x * cos (phi i)) xs) + U.sum (U.imap (\i y -> y * sin (phi i)) ys)
          im = U.sum (U.imap (\i y -> y * cos (phi i)) ys) - U.sum (U.imap (\i x -> x * sin (phi i)) xs)
          re' = re / fn
          im' = im / fn
          -- 負の周波数を正しく扱う（k > N/2 は負の周波数）
          freq = if 2 * k > n then k - n else k
      in Epi (fromIntegral freq) (sqrt (re' * re' + im' * im')) (atan2 im' re')

-- 指を離したら、絵を円に分解して再生を始める
finish :: World -> World
finish w
  | wMode w == Sketching && total > 10 =
      let stepN = max 1 (total `div` pointsWanted)
          pts = [p | (i, p) <- zip [0 ..] (reverse (wDrawing w)), i `mod` stepN == 0]
      in w { wMode = Playing
           , wDrawing = pts
           , wEpis = sortOn (negate . eAmp) (dft2D pts)   -- 大きい円から順に
           , wTime = 0
           , wPath = []
           , wArms = []
           }
  | otherwise = w
  where total = length (wDrawing w)

drawWorld :: Env -> World -> Picture
drawWorld env w = case wMode w of
  Sketching -> Pictures
    [ Color (hsb 170 80 100) (thickPath 3 (wDrawing w))
    , hud 18 "Drag to draw one continuous line. Let go and circles will redraw it"
    ]
  Playing -> Pictures
    [ Pictures (zipWith3 arm (wArms w) (drop 1 (wArms w)) (wEpis w))
    , Color (hsb 40 90 100) (thickPath 3 (wPath w))              -- 描いた軌跡
    , Color (hsba 170 60 100 0.25) (thickPath 2 (wDrawing w))    -- 元の絵（うすく）
    , hud 18 ("Circles " ++ show (wNum w) ++ " / " ++ show (length (wEpis w))
              ++ " (left/right)   C: draw again")
    ]
  where
    hud size = label (20 - envW env / 2) (envH env / 2 - 40) size (hsb 0 0 100)
    -- 円と腕
    arm p q e = Pictures
      [ Color (hsba 0 0 100 0.25) (at p (Circle (eAmp e)))
      , Color (hsba 0 0 100 0.6) (Line [p, q])
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | wMode w == Sketching -> w { wDrawing = [] }
  EventKey (MouseButton LeftButton) Up _ _ -> finish w
  EventMotion p
    | wMode w == Sketching && envDown env && length (wDrawing w) < maxDrawing -> w { wDrawing = p : wDrawing w }
  EventKey (Char 'c') Down _ _ -> clear w
  EventKey (Char 'C') Down _ _ -> clear w
  _ -> w

clear :: World -> World
clear w = w { wMode = Sketching, wDrawing = [], wPath = [], wArms = [], wEpis = [] }

-- ---------- やってみよう ----------
-- 1. 円 1 個、2 個、5 個、全部、と増やして、絵がだんだん正確になるのを見よう
-- 2. 大きい円 = 低い周波数（おおまかな形）、小さい円 = 高い周波数（細かいギザギザ）
-- 3. 星や文字を描いてみよう。角のある絵ほど円がたくさん必要

-- ---------- 道具 ----------

-- p5.js の map(v, a, b, c, d, true)：範囲からはみ出さない
mapC :: Float -> Float -> Float -> Float -> Float -> Float
mapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)
