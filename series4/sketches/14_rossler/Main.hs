-- ============================================
-- 14 レスラー・アトラクター（おまけ）
-- ローレンツより単純な式で、らせんを描きながら「折り返す」カオス
-- 自動で回転 / マウス上下 = パラメータ c（周期 → カオスへ）/ 1〜3 で別の方程式
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

data V = V !Float !Float !Float

data World = World
  { wPos   :: !V
  , wTrail :: ![V]       -- 軌跡（新しいものが先頭）
  , wAngle :: !Float
  , wType  :: !Int       -- 方程式の種類（1〜3）
  }

stepsPerFrame :: Int
stepsPerFrame = 10       -- ★ 1フレームに進めるステップ数

trailMax :: Int
trailMax = 4000          -- ★ 軌跡の長さ

main :: IO ()
main = runSketch (sketch "04-14 Rossler Attractor" (World (V 1 1 1) [] 0 1))
  { skBack  = hsb 250 40 8
  , skStep  = \env _ w -> pure (step (cOf env) w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

cOf :: Env -> Float
cOf env = remapC (snd (mouseP5 env)) 0 (envH env) 8 2

startPos :: Int -> V
startPos 2 = V 1.1 1.1 (-0.01)   -- x = y = z だと対称なまま 1 点に落ちてしまうので、少しくずす
startPos 3 = V 0.1 0 0
startPos _ = V 1 1 1

-- 1 ステップ進める。戻り値は (dx, dy, dz, dt)
equations :: Int -> Float -> V -> (Float, Float, Float, Float)
equations 1 c (V x y z) =
  -- ★ レスラー方程式
  let a = 0.2; b = 0.2
  in (-y - z, x + a * y, b + z * (x - c), 0.01)
equations 2 _ (V x y z) =
  -- トーマスの循環対称アトラクター（sin だけ！）
  let bb = 0.19
  in (sin y - bb * x, sin z - bb * y, sin x - bb * z, 0.05)
equations _ _ (V x y z) =
  -- アイザワ・アトラクター
  let a = 0.95; b = 0.7; cc = 0.6; d = 3.5; e = 0.25; f = 0.1
  in ( (z - b) * x - d * y
     , d * x + (z - b) * y
     , cc + a * z - (z * z * z) / 3 - (x * x + y * y) * (1 + e * z) + f * z * x * x * x
     , 0.01 )

step :: Float -> World -> World
step c w = w
  { wAngle = wAngle w + 0.004
  , wPos = pos
  , wTrail = take trailMax (new ++ wTrail w) }
  where
    (pos, new) = go stepsPerFrame (wPos w) []
    go :: Int -> V -> [V] -> (V, [V])
    go 0 p acc = (p, acc)
    go n p acc =
      let p' = safe (advance p)
      in go (n - 1) p' (p' : acc)
    advance p@(V x y z) =
      let (dx, dy, dz, dt) = equations (wType w) c p
      in V (x + dx * dt) (y + dy * dt) (z + dz * dt)
    -- c を急に変えると飛び出すことがあるので、遠くへ行ったら最初の位置に戻す
    safe p@(V x y z)
      | isNaN (x + y + z) || abs x > 500 || abs y > 500 || abs z > 500 = startPos (wType w)
      | otherwise = p

typeName :: Int -> String
typeName 1 = "Rossler"
typeName 2 = "Thomas"
typeName _ = "Aizawa"

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  ( zipWith3 seg [0 :: Int ..] oldestFirst (drop 1 oldestFirst) ++
  [ txt env 20 40 16 white
      (typeName t ++ " attractor   "
        ++ (if t == 1 then "c = " ++ fmt 2 (cOf env) ++ " (mouse Y: small = periodic, large = chaos)   " else "")
        ++ "1-3: equations")
  ])
  where
    t = wType w
    oldestFirst = reverse (wTrail w)
    n = length oldestFirst
    zoom = case t of { 1 -> 22; 2 -> 90; _ -> 180 } :: Float
    zoff = case t of { 1 -> -10; 3 -> -0.5; _ -> 0 } :: Float
    angle = wAngle w
    -- 回転させて投影する（少し上から見る）
    toScreen (V px py pz) =
      let rx = px * cos angle - py * sin angle
          ry = px * sin angle + py * cos angle
          sy = ry * 0.4 - (pz + zoff) * 0.9
      in (rx * zoom, -sy * zoom)
    seg i a b@(V _ _ pz) =
      Color (hsba (pz * 10 + 200) 70 100 (remap (fromIntegral i) 0 (fromIntegral n) 5 100 / 100))
            (thickLine 1.5 (toScreen a) (toScreen b))

onEvent :: Env -> Event -> World -> IO World
onEvent _ (EventKey (Char c) Down _ _) w
  | c >= '1' && c <= '3' =
      let t = fromEnum c - fromEnum '0'
      in pure w { wType = t, wTrail = [], wPos = startPos t }
onEvent _ _ w = pure w

-- ---------- やってみよう ----------
-- 1. レスラーで c を 2 → 8 へ。1つの輪 → 2重 → 4重 → ぐちゃぐちゃ（周期倍分岐！）
-- 2. トーマス（2）は sin だけ。bb を 0.1 にすると？
-- 3. 3つの式をよく見ると、どれも「x, y, z がおたがいを変えあう」だけ

-- ---------- 道具 ----------

-- p5.js 風の座標（左上が原点、y は下向き）を gloss の座標に直す
sc :: Env -> Float -> Float -> Point
sc env x y = (x - envW env / 2, envH env / 2 - y)

mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size col s = let (gx, gy) = sc env x y in label gx gy size col s

remapC :: Float -> Float -> Float -> Float -> Float -> Float
remapC v a b c d = clampF (min c d) (max c d) (remap v a b c d)

fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
