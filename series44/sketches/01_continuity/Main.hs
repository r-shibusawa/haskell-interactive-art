-- ============================================
-- 44-01 連続の式　A1 v1 = A2 v2
-- 水はつぶれない（非圧縮）ので、管のどこでも「毎秒通る量」は同じ
--   流量 Q = 断面積 A × 速さ v = 一定　→　細い所ほど速い
-- ホースの先をつぶすと水が勢いよく飛ぶのと同じしくみ
-- 操作：管の中を上下にドラッグ = しぼる / 上下キー = 流量 Q / R = 元に戻す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Numeric (showFFloat)

q0 :: Float
q0 = 30000          -- ★ 流量（毎秒流れる量。2D なので [px²/秒]）

throat0 :: Float
throat0 = 0.45      -- ★ しぼり（いちばん細い所の太さ ÷ 元の太さ）

pipeH0 :: Float
pipeH0 = 150        -- ★ 管の太さの半分 [px]

nParts :: Int
nParts = 700        -- ★ 粒の数

gateX :: [Float]
gateX = [0.14, 0.5, 0.86]   -- 粒を数える 3 か所（画面の幅に対する割合）

-- 粒：x と、管の中の高さの割合 eta（-1〜1）
data Part = Part { qx :: !Float, qEta :: !Float }

data World = World
  { wQ      :: !Float
  , wThroat :: !Float
  , wParts  :: ![Part]
  , wCount  :: ![Int]      -- 3 か所を通った粒の数
  , wRate   :: ![Float]    -- 1 秒ごとの集計
  , wGateT  :: !Float
  }

main :: IO ()
main = runSketch (sketch "44-01 Continuity equation" (World q0 throat0 [] [0, 0, 0] [0, 0, 0] 0))
  { skBack  = hsb 215 60 10
  , skStart = \env -> do
      ps <- scatter env nParts
      pure (World q0 throat0 ps [0, 0, 0] [0, 0, 0] 0)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- 粒を「面積あたり同じ密度」でまく（広い所に多く、細い所に少なく）
scatter :: Env -> Int -> IO [Part]
scatter _ 0 = pure []
scatter env n = do
  x <- randomRIO (0, envW env)
  r <- randomRIO (0, 1)
  e <- randomRIO (-1, 1)
  if r < halfHeight env throat0 x / pipeH env
    then (Part x e :) <$> scatter env (n - 1)
    else scatter env n

pipeH :: Env -> Float
pipeH env = min pipeH0 (envH env * 0.2)

cy :: Env -> Float
cy env = envH env * 0.42

-- ★ 管の形：場所 x での「太さの半分」。まん中がなめらかに細くなる
halfHeight :: Env -> Float -> Float -> Float
halfHeight env th x =
  let s = clampF (-1) 1 ((x - envW env / 2) / (envW env * 0.22))
  in pipeH env * (1 - (1 - th) * 0.5 * (1 + cos (pi * s)))

-- ★ 連続の式：v = Q / A（A = 太さ = 2 × halfHeight）
speedAt :: Env -> World -> Float -> Float
speedAt env w x = wQ w / (2 * halfHeight env (wThroat w) x)

step :: Env -> Float -> World -> IO World
step env dt0 w0 = do
  let dt = min dt0 0.033
      (_, my) = mouseP env
      w = if envDown env && my > 90 && my < cy env + pipeH env + 40
            then w0 { wThroat = clampF 0.15 1 (abs (my - cy env) / pipeH env) }
            else w0
      wd = envW env
      gates = map (* wd) gateX
  -- 粒：x 方向に v(x) で進む（中点法）
  moved <- mapM (\(Part x e) -> do
      let v = speedAt env w x
          x' = x + speedAt env w (x + v * dt / 2) * dt
          crossed = [ if x < g && x' >= g then 1 else 0 | g <- gates ] :: [Int]
      if x' > wd
        then do e' <- randomRIO (-1, 1); pure (Part (x' - wd) e', crossed)
        else pure (Part x' e, crossed)) (wParts w)
  let counts = foldr (zipWith (+) . snd) (wCount w) moved
      gt = wGateT w + dt
      -- 通過した粒を数える（1 秒ごとに集計）
      (counts', rate', gt') = if gt >= 1 then ([0, 0, 0], map (\c -> fromIntegral c / gt) counts, 0)
                                         else (counts, wRate w, gt)
  pure w { wParts = map fst moved, wCount = counts', wRate = rate', wGateT = gt' }

draw :: Env -> World -> Picture
draw env w = view env $
  -- 管の壁
  [ path 5 (hsb 42 40 85) [ (x, c + sgn * (halfHeight env th x + 4)) | x <- xs ] | sgn <- [-1, 1] ]
  -- 粒
  ++ [ dot (hsb (remap (clampF 80 500 v) 80 500 200 10) 70 100) x (c + e * halfHeight env th x) 5
     | Part x e <- wParts w, let v = speedAt env w x ]
  -- 3 か所の断面：A・v・A×v を表示
  ++ concat
     [ [ Color (hsbA 0 0 100 60) (Line [(x, c - h), (x, c + h)])
       , Color (hsb 180 80 100) (arrowP 3 (x, c) (x + v * 0.25, c))
       , txtC x (c + ph + 40) 15 (hsb 0 0 100) ("A = " ++ f0 (2 * h))
       , txtC x (c + ph + 62) 15 (hsb 180 80 100) ("v = " ++ f0 v)
       , txtC x (c + ph + 84) 15 (hsb 42 80 100) ("A x v = " ++ f0 (2 * h * v))
       , txtC x (c + ph + 106) 15 (hsb 0 0 65) ("counted " ++ f0 r ++ " /s") ]
     | (gx, r) <- zip gateX (wRate w)
     , let x = gx * wd, let h = halfHeight env th x, let v = speedAt env w x ]
  -- グラフ：断面積 A(x) と速さ v(x)
  ++ [ Color (hsb 215 50 16) (Polygon [(0, gy - gh - 10), (wd, gy - gh - 10), (wd, gy + 20), (0, gy + 20)])
     , path 2 (hsb 0 0 80) [ (x, gy - gh * halfHeight env th x / ph * 0.5) | x <- xs ]
     , path 2 (hsb 180 80 100) [ (x, gy - gh * clampF 0 1 (speedAt env w x / 700)) | x <- xs ]
     , txt 20 (gy - gh + 4) 14 (hsb 0 0 80) "area A(x)"
     , txt 130 (gy - gh + 4) 14 (hsb 180 80 100) "speed v(x) = Q / A(x)"
     , txt 20 40 18 (hsb 0 0 100)
         ("Continuity  A1 v1 = A2 v2 = Q = " ++ f0 (wQ w) ++ " (Up/Down)   throat " ++ f0 (th * 100) ++ "% (drag up/down in the pipe)")
     , txt 20 68 15 (hsb 0 0 65)
         ("The narrow part is " ++ showFFloat (Just 2) (1 / th) "" ++ "x faster, but the particles counted per second are almost equal at all 3 gates.")
     ]
  where
    wd = envW env
    c = cy env
    ph = pipeH env
    th = wThroat w
    xs = [0, 8 .. wd] ++ [wd]
    gy = envH env * 0.94
    gh = envH env * 0.11

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wQ = min 80000 (wQ w * 1.2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wQ = max 8000 (wQ w / 1.2) }
  EventKey (Char k) Down _ _ | k `elem` "rR" -> w { wQ = q0, wThroat = throat0 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. しぼりを 50% にする。v は何倍？ 25% では？（A が半分 → v が 2 倍）
-- 2. 「数えた粒」が 3 か所でほぼ同じになることを確かめる。Q を変えるとどうなる？
-- 3. halfHeight を書きかえて、しぼりを 2 か所にしてみよう
-- 4. 3D の円管なら A = πr²。半径を半分にすると速さは何倍？（→ 4 倍）
-- 5. 川幅が広い所は流れがゆるやか。浅くなる所は？ 身近な例をさがそう

-- ---------- 道具 ----------
f0 :: Float -> String
f0 v = show (round v :: Int)

-- 矢印（p5 の座標で、矢じりは 2 本の線）
arrowP :: Float -> Point -> Point -> Picture
arrowP wdt (x1, y1) (x2, y2) =
  let a = atan2 (y2 - y1) (x2 - x1)
  in Pictures [ thickLine wdt (x1, y1) (x2, y2)
              , thickLine wdt (x2, y2) (x2 - 10 * cos (a - 0.5), y2 - 10 * sin (a - 0.5))
              , thickLine wdt (x2, y2) (x2 - 10 * cos (a + 0.5), y2 - 10 * sin (a + 0.5)) ]

-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃えの文字（y は文字の下の線）
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y size c s = txt (x - fromIntegral (length s) * size * 0.36) y size c s

hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

path :: Float -> Color -> [Point] -> Picture
path w c ps = Color c (Pictures (zipWith (thickLine w) ps (drop 1 ps)))

dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))
