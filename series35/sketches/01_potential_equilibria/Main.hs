-- ============================================
-- 01 ポテンシャルと平衡点
-- ボールが止まれる場所 = 地形 V(x; a) の傾きが 0 の所（平衡点、V'(x) = 0）
--   V'' > 0（谷の底）なら安定、V'' < 0（山の頂上）なら不安定
-- 右の図は「a を変えたとき平衡点がどこにあるか」。谷と山がぶつかって消える所 = 折り目（fold、サドル・ノード分岐）
-- マウス左右（または ←→）= a / 1〜3 = 地形の種類 / ドラッグ（左の図）= ボールを置く / R = ボールを戻す
-- ============================================
module Main (main) where

import Art
import Data.Maybe (mapMaybe)
import Numeric (showFFloat)

damping :: Float
damping = 3                -- ★ 摩擦

data World = World
  { wKind :: !Int          -- 地形の種類
  , wA    :: !Float        -- パラメータ
  , wX    :: !Float        -- ボールの位置
  , wV    :: !Float        -- ボールの速度
  }

world0 :: World
world0 = World 1 (-1) 0.5 0

main :: IO ()
main = runSketch (sketch "35-01 Potential and Equilibria" world0)
  { skBack  = hsb 230 30 12
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

kindName :: Int -> String
kindName 1 = "Fold  V = x^3/3 + a x"
kindName 2 = "Tilted valley  V = x^4/4 + a x^2/2 + 0.25 x"
kindName _ = "Two valleys  V = x^4/4 - x^2/2 + a x"

-- ★ 地形と、その傾き V'、曲がり具合 V''
vOf, dV, ddV :: Int -> Float -> Float -> Float
vOf 1 x a = x * x * x / 3 + a * x
vOf 2 x a = x ^ (4 :: Int) / 4 + a * x * x / 2 + 0.25 * x
vOf _ x a = x ^ (4 :: Int) / 4 - x * x / 2 + a * x
dV 1 x a = x * x + a
dV 2 x a = x * x * x + a * x + 0.25
dV _ x a = x * x * x - x + a
ddV 1 x _ = 2 * x
ddV 2 x a = 3 * x * x + a
ddV _ x _ = 3 * x * x - 1

-- V'(x; a) = 0 を a について解く（3 つとも a は 1 次式なので解ける）
aOf :: Int -> Float -> Maybe Float
aOf 1 x = Just (negate (x * x))
aOf 2 x = if abs x < 1e-3 then Nothing else Just (negate (x * x * x + 0.25) / x)
aOf _ x = Just (x - x * x * x)

stepWorld :: World -> World
stepWorld w = let (x, v) = go (6 :: Int) (wX w) (wV w) in w { wX = x, wV = v }
  where
    go 0 x v = (x, v)
    go k x v =
      let dt = 1 / 120
          v1 = v + (-8 * dV (wKind w) x (wA w) - damping * v) * dt
          x1 = x + v1 * dt
      in if x1 < -2 || x1 > 2 then go (k - 1) (clampF (-2) 2 x1) 0 else go (k - 1) x1 v1

roots :: Int -> Float -> [Float]
roots kind a = go (1 :: Int) (dV kind (-2) a)
  where
    go i prev
      | i > 400 = []
      | otherwise =
          let xx = -2 + 4 * fromIntegral i / 400
              cur = dV kind xx a
              rest = go (i + 1) cur
          in if prev * cur < 0 || cur == 0 then (xx - 0.01 * cur / (cur - prev + 1e-12)) : rest else rest

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      kind = wKind w
      a = wA w
      x = wX w
      gap = 40
      pw = (wd - 3 * gap) / 2
      py = 110
      ph = h - py - 40
      panel = hsb 230 25 18
      -- 左：地形 V(x)
      lx = gap
      toX xx = lx + (xx + 2) / 4 * pw
      toY vv = py + ph * 0.55 - vv * ph * 0.16
      clampY = clampF py (py + ph)
      curve = [ (toX xx, clampY (toY (vOf kind xx a))) | i <- [0 .. 160 :: Int], let xx = -2 + 4 * fromIntegral i / 160 ]
      eqs = roots kind a
      eqMark e =
        let yy = toY (vOf kind e a)
        in if yy < py || yy > py + ph then Blank
           else if ddV kind e a > 0 then Color (hsb 150 70 100) (at (toX e, yy) (disc 9))
           else at (toX e, yy) (Pictures [Color panel (disc 9), Color (hsb 0 70 100) (ring 9 3)])
      -- 右：平衡点の図（横 = a、縦 = x）
      rx = 2 * gap + pw
      toA aa = rx + (aa + 2) / 4 * pw
      toXr xx = py + ph / 2 - xx / 2 * (ph / 2 - 20)
      pts = [ if inRange then Just (toA aa, toXr xx, ddV kind xx aa > 0) else Nothing
            | i <- [0 .. 400 :: Int]
            , let xx = -2 + 4 * fromIntegral i / 400
                  mb = aOf kind xx
                  aa = maybe 0 id mb
                  inRange = maybe False (\v -> v > -2 && v < 2) mb ]
      branch = concat (zipWith3 seg [1 :: Int ..] pts (drop 1 pts))
      seg i (Just (x0, y0, _)) (Just (x1, y1, st))
        | st = [Color (hsb 150 70 100) (thickLine 4 (x0, y0) (x1, y1))]
        | i `mod` 4 < 2 = [Color (hsb 0 70 100) (thickLine 3 (x0, y0) (x1, y1))]
      seg _ _ _ = []
      -- 折り目の点（V' = 0 かつ V'' = 0）
      folds = mapMaybe foldAt [0 .. 399 :: Int]
      foldAt i =
        let x0 = -2 + 4 * (fromIntegral i + 0.37) / 400
            x1 = x0 + 0.01
        in case (aOf kind x0, aOf kind x1) of
             (Just a0, Just a1) | ddV kind x0 a0 * ddV kind x1 a1 < 0 && abs a0 < 2 ->
               Just (at (toA a0, toXr x0) (disc 7))
             _ -> Nothing
  in Pictures
    [ onP5 env (Pictures
        [ Color panel (box lx py pw ph)
        , Color (hsbA 0 0 100 15) (Line [(lx, toY 0), (lx + pw, toY 0)])
        , Color (hsb 150 50 80) (thickPath 4 curve)
        , Pictures (map eqMark eqs)
        , Color (hsb 45 90 100) (at (toX x, clampY (toY (vOf kind x a) - 12)) (disc 11))
        , Color panel (box rx py pw ph)
        , Color (hsbA 0 0 100 15) (Pictures [Line [(rx, toXr 0), (rx + pw, toXr 0)], Line [(toA 0, py), (toA 0, py + ph)]])
        , Pictures branch
        , Color (hsb 45 90 100) (Pictures folds)
        , Color (hsb 200 70 100) (thickLine 2 (toA a, py) (toA a, py + ph))
        , Color (hsb 45 90 100) (at (toA a, toXr x) (disc 8))
        ])
    , txt env (lx + 16) (py + 28) 14 (gray 0.8) (kindName kind)
    , txt env (lx + 16) (py + 52) 14 (gray 0.8) ("Dot: stable (V''>0)  Ring: unstable (V''<0)  Count: " ++ show (length eqs))
    , txt env (rx + 16) (py + 28) 14 (gray 0.8) "Equilibria: across = a, up = x"
    , txt env (rx + 16) (py + 52) 14 (gray 0.8) "Green stable, red dashed unstable, yellow fold"
    , txt env (rx + pw - 24) (toXr 0 - 8) 16 (gray 0.8) "a"
    , txt env (toA 0 + 8) (py + 76) 16 (gray 0.8) "x"
    , txt env 20 36 18 white ("a = " ++ nf 2 a ++ " (mouse on right / <- ->)   1-3: terrain   Drag: place ball   R: reset")
    , txt env 20 62 14 (gray 0.6) "Where the blue line crosses the curve = equilibria now. As a moves, a green valley and a red hill"
    , txt env 20 84 14 (gray 0.6) "meet at a yellow point and vanish. A ball resting there has nowhere to stay."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventMotion _
    | envDown env && mx < gap + pw && my > 90 -> w { wX = clampF (-2) 2 (remap mx gap (gap + pw) (-2) 2), wV = 0 }
    | my > 90 && mx > 2 * gap + pw -> w { wA = clampF (-2) 2 (remap mx (2 * gap + pw) (envW env - gap) (-2) 2) }
  EventKey (SpecialKey KeyLeft) Down _ _  -> w { wA = max (-2) (wA w - 0.05) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wA = min 2 (wA w + 0.05) }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> w { wKind = fromEnum c - 48, wX = 0.5, wV = 0 }
    | c `elem` "rR" -> w { wX = 0.5, wV = 0 }
  _ -> w
  where
    (mx, my) = mouseP5 env
    gap = 40
    pw = (envW env - 3 * gap) / 2

-- ---------- やってみよう ----------
-- 1. 地形 1 で a を −1 から +1 へ。a = 0 で谷と山がぶつかる。そのとき V''（曲がり具合）はいくつ？
-- 2. 地形 3 で、平衡点が 3 つある a の範囲を右の図から読みとろう（答え：約 −0.38 < a < 0.38）
-- 3. 地形 2 の右の図は折り目が 1 つだけ。0.25 を 0 にすると形がどう変わる？（ピッチフォーク分岐）
-- 4. 自分の地形 V を 1 つ追加してみよう：vOf, dV, ddV, aOf の 4 つに 1 行ずつ書き足す


-- ==================================================
-- ここから下は「道具」（座標の変換・色・数字）
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を画面に合わせる
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- p5.js の text(s, x, y) と同じ位置に文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

-- マウスの位置（左上が原点、y は下向き）
mouseP5 :: Env -> Point
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上が (x, y) の長方形（p5.js の rect）
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

-- 透明度を 0〜100 で書く HSB（p5.js と同じ）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

onOff :: Bool -> String
onOff b = if b then "ON" else "off"
