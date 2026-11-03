-- ============================================
-- 08 一頂点の平坦折りシミュレーション（3D）
-- 4 本の折り目が集まる頂点を、紙を曲げずに（剛体のまま）少しずつ折る。動かせるのは 1 つの角度だけ（自由度 1）
--   川崎の条件を満たすと最後までぺたんと平らになる。満たさないと途中で止まる
-- ←→ = 折る量 / A = 自動 / 1〜3 = 角の組み合わせ / B = 折れ方（もう一方の枝）/ ドラッグ = 視点 / ホイール = 近づく
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Numeric (showFFloat)

presets :: [(String, [Int])]
presets =
  [ ("Miura-ori vertex", [70, 110, 110, 70])
  , ("General flat-foldable vertex", [50, 80, 130, 100])
  , ("Vertex that breaks Kawasaki's condition", [80, 100, 95, 85])
  ]                -- ★ 4 つの角（度）。θ1 + θ3 = θ2 + θ4 = 180 で平坦折り可能

rr :: Float
rr = 220           -- ★ 紙の半径（px）

-- 4 本の折り目の向き（長さ 1 の 3D ベクトル）。交わりの判定に細かい桁が要るので Double
data C4 = C4 !V !V !V !V

data World = World
  { wPreset :: !Int
  , wRho    :: !Double    -- 折り目 c0 の折り角（ラジアン）
  , wBranch :: !Double    -- 2 つの折れ方のどちらか
  , wAuto   :: !Bool
  , wDir    :: !Double
  , wC      :: !C4
  , wStuck  :: !Bool
  , wYaw    :: !Float     -- 視点
  , wPitch  :: !Float
  , wDist   :: !Float
  , wLast   :: !Point     -- ドラッグの 1 つ前のマウス位置
  }

main :: IO ()
main = runSketch (sketch "26-08 Single-Vertex Rigid Fold" initial)
  { skBack  = rgb255 30 28 36
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

initial :: World
initial = resetFold World
  { wPreset = 0, wRho = 0.02, wBranch = 1, wAuto = False, wDir = 1, wC = flat 0, wStuck = False
  , wYaw = 0, wPitch = atan2 420 520, wDist = sqrt (420 * 420 + 520 * 520), wLast = (0, 0) }

thetas :: Int -> [Double]
thetas k = map (\d -> fromIntegral d * pi / 180) (degs k)

degs :: Int -> [Int]
degs k = case drop k presets of
  (_, d) : _ -> d
  [] -> [90, 90, 90, 90]

-- 紙を広げた状態（念のための初期値）
flat :: Int -> C4
flat k = case thetas k of
  [a, b, c, _] -> C4 (V 1 0 0) (V (cos a) 0 (sin a)) (V (cos (a + b)) 0 (sin (a + b))) (V (cos (a + b + c)) 0 (sin (a + b + c)))
  _ -> C4 (V 1 0 0) (V 0 0 1) (V (-1) 0 0) (V 0 0 (-1))

resetFold :: World -> World
resetFold w = case solve (wPreset w) (wBranch w) Nothing 0.02 of
  Just c -> w { wRho = 0.02, wC = c }
  Nothing -> w { wRho = 0.02, wC = flat (wPreset w) }

-- ★ 折り目 c0 を ρ だけ折ったときの 4 本の折り目を求める
solve :: Int -> Double -> Maybe V -> Double -> Maybe C4
solve k branch prev r = case thetas k of
  [t0, t1, t2, t3] ->
    let c0 = V 1 0 0
        c1 = V (cos t0) 0 (sin t0)                       -- 面 0（c0〜c1）は床に固定
        c3 = rotAxis (V (cos (-t3)) 0 (sin (-t3))) c0 r  -- 面 3 を c0 のまわりに ρ 回す
        -- c2 は「c1 と θ2 の角」かつ「c3 と θ3 の角」をなす単位ベクトル：2 つの円錐の交わり
        g = dotv c1 c3
        ca = cos t1
        cb = cos t2
        den = 1 - g * g
        a = (ca - g * cb) / den
        b = (cb - g * ca) / den
        base = addv (scalev a c1) (scalev b c3)
        w = crossv c1 c3
        ww = dotv w w
        rest = 1 - dotv base base
        t = sqrt (max 0 rest / ww)
        s1 = addv base (scalev t w)
        s2 = addv base (scalev (-t) w)
        yOf (V _ y _) = y
        c2 = case prev of
          Just p -> if dotv s1 p >= dotv s2 p then s1 else s2     -- 前のフレームに近い方（連続に動かす）
          Nothing -> if yOf s1 * branch <= yOf s2 * branch then s1 else s2
    in if den < 1e-9 || rest < -1e-9    -- 交わらない → これ以上折れない
         then Nothing
         else Just (C4 c0 c1 c2 c3)
  _ -> Nothing

tryRho :: Double -> World -> (Bool, World)
tryRho r0 w =
  let r = max 0.02 (min (pi - 0.02) r0)
      C4 _ _ c2 _ = wC w
  in case solve (wPreset w) (wBranch w) (Just c2) r of
       Just c -> (True, w { wRho = r, wC = c, wStuck = False })
       Nothing -> (False, w { wStuck = True })

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 =
  let w1 = if isDown env (SpecialKey KeyRight) then snd (tryRho (wRho w0 + 0.012) w0) else w0
      w2 = if isDown env (SpecialKey KeyLeft) then snd (tryRho (wRho w1 - 0.012) w1) else w1
      w3 | wAuto w2 =
             let (ok, w') = tryRho (wRho w2 + 0.01 * wDir w2) w2
             in if not ok || wRho w' > pi - 0.03 || wRho w' <= 0.02 then w' { wDir = -(wDir w') } else w'
         | otherwise = w2
  in pure w3

-- 折り目 i（0〜3）の向き
cAt :: C4 -> Int -> V
cAt (C4 a b c d) i = case i `mod` 4 of { 0 -> a; 1 -> b; 2 -> c; _ -> d }

-- 折り目 i の折り角（左右の面の法線の角度）。符号 + = 谷、− = 山
foldAngle :: C4 -> Int -> Double
foldAngle cs i =
  let a = cAt cs (i + 3)
      b = cAt cs i
      c = cAt cs (i + 1)
      n1 = normv (crossv a b)
      n2 = normv (crossv b c)
      ang = acos (max (-1) (min 1 (dotv n1 n2)))
  in if dotv (crossv n1 n2) b >= 0 then ang else -ang

-- 視点：p5 の座標（y は下向き）を y 上向きにして回す
toView :: World -> V -> V3
toView w (V x y z) =
  rotX (wPitch w) (rotY (wYaw w) (V3 (realToFrac x) (realToFrac (-y)) (realToFrac z)))

-- 透視投影（画面での大きさは p5.js の標準カメラと同じ 800 / 距離）
persp :: World -> V3 -> Point
persp w v = let (x, y) = project (wDist w) v
                k = 800 / wDist w
            in (x * k, y * k)

zOf :: V3 -> Float
zOf (V3 _ _ z) = z

-- 光の当たり方：環境光 + 平行光（面の表裏は区別しない）
shade :: V -> (Float, Float, Float) -> Color
shade n (r, g, b) =
  let V lx ly lz = normv (V 0.3 1 (-0.5))
      V nx ny nz = normv n
      d = realToFrac (abs (lx * nx + ly * ny + lz * nz)) :: Float
      k = min 1 (120 / 255 + 0.75 * d)
  in makeColor (r * k) (g * k) (b * k) 1

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let cs = wC w
      th = thetas (wPreset w)
      paperA = (246 / 255, 240 / 255, 226 / 255)
      paperB = (226 / 255, 84 / 255, 72 / 255)
      -- 4 枚の板を細かいマスに分けて、奥から順に描く（画家のアルゴリズム）
      nA = 16 :: Int
      nR = 4 :: Int
      plate i t =
        let a = cAt cs i
            b = cAt cs (i + 1)
            col = shade (crossv a b) (if even i then paperA else paperB)
            rad j = realToFrac rr * fromIntegral j / fromIntegral nR :: Double
            sl k = slerp a b t (fromIntegral k / fromIntegral nA)
        in [ (sum (map zOf vs) / 4, Color col (Polygon (map (persp w) vs)))
           | k <- [0 .. nA - 1], j <- [0 .. nR - 1]
           , let p = sl k
           , let q = sl (k + 1)
           , let vs = map (toView w) [scalev (rad j) p, scalev (rad (j + 1)) p, scalev (rad (j + 1)) q, scalev (rad j) q] ]
      plates = concat (zipWith plate [0 ..] th)
      -- 折り目（山 = 赤、谷 = 青）
      creaseSegs i =
        let c = cAt cs i
            col = if foldAngle cs i >= 0 then rgb255 60 120 240 else rgb255 230 70 50
            r k = realToFrac rr * 1.02 * fromIntegral k / 8 :: Double
        in [ ((zOf v0 + zOf v1) / 2 + 3, Color col (thickLine 4 (persp w v0) (persp w v1)))
           | k <- [0 .. 7 :: Int]
           , let v0 = toView w (scalev (r k) c)
           , let v1 = toView w (scalev (r (k + 1)) c) ]
      scene = map snd (depthSort fst (plates ++ concatMap creaseSegs [0 .. 3]))
  in Pictures (scene ++ [drawHud env w])

drawHud :: Env -> World -> Picture
drawHud env w =
  let cs = wC w
      ds = degs (wPreset w)
      (name, _) = case drop (wPreset w) presets of { x : _ -> x; [] -> ("", []) }
      (k1, k2) = case ds of
        [a, b, c, d] -> (a + c, b + d)
        _ -> (0, 0)
      okCol = if k1 == 180 && k2 == 180 then rgb255 80 220 140 else rgb255 255 110 90
      foldLine i =
        let f = foldAngle cs i
        in txt env 20 (206 + fromIntegral i * 26) 16 (if f >= 0 then rgb255 110 160 255 else rgb255 255 110 90)
             ("Crease c" ++ show i ++ ": " ++ nfD 1 (abs f * 180 / pi) ++ " deg  " ++ (if f >= 0 then "valley" else "mountain"))
      -- 展開図（右下）
      ox = envW env - 170
      oy = envH env - 170
      r = 120
      angles = scanl (+) 0 (map realToFrac (thetas (wPreset w))) :: [Float]
      patt =
        [ Color (rgb255 246 240 226) (at (sc env (ox, oy)) (disc (r + 10))) ]
        ++ [ Color (if foldAngle cs i >= 0 then rgb255 60 120 240 else rgb255 230 70 50)
               (thickLine 3 (sc env (ox, oy)) (sc env (ox + cos a * r, oy + sin a * r)))
           | (i, a) <- zip [0 .. 3] angles ]
        ++ [ txt env (ox - 120) (oy + r + 30) 14 (gray 0.78) "Crease pattern (red = M, blue = V)" ]
  in Pictures $
       [ txt env 20 40 18 white ("Left/Right: fold   A: auto " ++ (if wAuto w then "ON" else "off") ++ "   1-3: vertex type   B: other branch   Drag: view")
       , txt env 20 68 15 (gray 0.67) "Paper does not bend: 4 rigid plates. Set one fold angle and the other 3 are decided (1 degree of freedom)."
       , txt env 20 110 22 (rgb255 255 190 40) (show (wPreset w + 1) ++ ". " ++ name)
       , txt env 20 138 17 (gray 0.86) ("Angles: " ++ concatMap (\d -> show d ++ " ") ds ++ "deg")
       , txt env 20 164 17 okCol ("th1 + th3 = " ++ show k1 ++ " deg,  th2 + th4 = " ++ show k2 ++ " deg")
       ]
       ++ map foldLine [0 .. 3]
       ++ [ if wStuck w
              then txt env 20 320 16 (rgb255 255 110 90) "Cannot fold further while staying rigid!"
              else txt env 20 320 16 (gray 0.78) ("rho = " ++ nfD 1 (wRho w * 180 / pi) ++ " deg (flat at 180 deg)") ]
       ++ patt

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ p -> w { wLast = p }
  EventKey (MouseButton WheelUp) Down _ _ -> w { wDist = max 300 (wDist w * 0.93) }
  EventKey (MouseButton WheelDown) Down _ _ -> w { wDist = min 2000 (wDist w * 1.07) }
  EventMotion p@(x, y) | envDown env ->
    let (lx, ly) = wLast w
    in w { wYaw = wYaw w + (x - lx) * 0.01
         , wPitch = clampF (-1.55) 1.55 (wPitch w - (y - ly) * 0.01)
         , wLast = p }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '3' -> resetFold w { wPreset = fromEnum c - fromEnum '1', wStuck = False }
    | toLower c == 'a' -> w { wAuto = not (wAuto w) }
    | toLower c == 'b' -> resetFold w { wBranch = -(wBranch w), wStuck = False }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1（ミウラ）と 2 で ρ = 180° まで折れるのに、3 は途中で止まる。止まる角度は？
-- 2. B で折れ方を変えると、山と谷の組み合わせが入れかわる。どちらも前川の定理（M − V = ±2）を満たしている？
-- 3. presets の角を自分で決めよう（例：[40, 90, 140, 90]）。川崎の条件を満たすようにして平らになるか確かめる
-- 4. 4 本の折り角の大きさを見比べよう。向かい合う折り目の角度はどうなっている？

-- ==================================================
-- ここから下は「3D ベクトルの道具」と画面の道具
-- ==================================================

data V = V !Double !Double !Double

dotv :: V -> V -> Double
dotv (V a b c) (V d e f) = a * d + b * e + c * f

addv :: V -> V -> V
addv (V a b c) (V d e f) = V (a + d) (b + e) (c + f)

scalev :: Double -> V -> V
scalev k (V a b c) = V (a * k) (b * k) (c * k)

crossv :: V -> V -> V
crossv (V a b c) (V d e f) = V (b * f - c * e) (c * d - a * f) (a * e - b * d)

normv :: V -> V
normv v = let l0 = sqrt (dotv v v)
              l = if l0 == 0 then 1 else l0
          in scalev (1 / l) v

-- 軸 k（長さ 1）のまわりに v を角 t 回す（ロドリゲスの公式）
rotAxis :: V -> V -> Double -> V
rotAxis v k t =
  let c = cos t
      s = sin t
  in addv (addv (scalev c v) (scalev s (crossv k v))) (scalev (dotv k v * (1 - c)) k)

-- a から b へ（間の角 th）の球面上の補間
slerp :: V -> V -> Double -> Double -> V
slerp a b th s =
  addv (scalev (sin ((1 - s) * th) / sin th) a) (scalev (sin (s * th) / sin th) b)

sc :: Env -> Point -> Point
sc env (x, y) = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sc env (x, y) in label a b size c s

nfD :: Int -> Double -> String
nfD d v = showFFloat (Just d) v ""
