-- ============================================
-- 08 スピンとブロッホ球（3D）
-- スピン 1/2（1 量子ビット）の状態 |ψ⟩ = α|0⟩ + β|1⟩ は、球の表面の 1 点（ブロッホベクトル）で表せる
--   ゲート = 球の回転。測定すると |0⟩（北極）か |1⟩（南極）に飛び移る。確率 = |α|²、|β|²
-- X Y Z H S T = ゲート / M = z で測定 / K = 1000 回測った統計 / B = 磁場（z 軸のまわりの歳差運動） / ←→ ↑↓ = y・x 軸まわりに少しずつ回す / R = |0⟩ に戻す / ドラッグ = 視点
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toUpper)
import Numeric (showFFloat)

animFrames :: Int
animFrames = 24         -- ★ ゲート 1 つを何フレームかけて回すか

larmor :: Float
larmor = 1.2            -- ★ 歳差運動の角速度

trailMax :: Int
trailMax = 300

-- α と β（複素数）
data Psi = Psi !Float !Float !Float !Float

-- アニメーション中のゲート
data Gate = Gate { gN :: !V3, gTh :: !Float, gLeft :: !Int, gName :: !Char }

data World = World
  { wPsi    :: !Psi
  , wQueue  :: ![Gate]
  , wField  :: !Bool
  , wTrail  :: ![V3]       -- ブロッホベクトルの跡（新しいものが先頭）
  , wC0     :: !Int        -- M で |0⟩ が出た回数
  , wC1     :: !Int
  , wStat   :: !(Maybe Int) -- K：1000 回中 |0⟩ が出た回数
  , wRotX   :: !Float      -- 視点
  , wRotY   :: !Float
  , wLast   :: !String
  , wPrev   :: !Point      -- 前のマウスの位置
  }

state0 :: Psi
state0 = Psi 1 0 0 0

main :: IO ()
main = runSketch (sketch "25-08 Bloch Sphere" (World state0 [] False [] 0 0 Nothing (-0.35) (-0.7) "-" (0, 0)))
  { skBack  = rgb255 16 18 34
  , skStep  = \env dt w -> pure (stepW env dt w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

-- ★ 回転のゲート U = cos(θ/2) I − i sin(θ/2) (n · σ) を状態にかける（σ = パウリ行列）
applyRot :: V3 -> Float -> Psi -> Psi
applyRot (V3 nx ny nz) th (Psi aR aI bR bI) =
  let c = cos (th / 2)
      s = sin (th / 2)
      -- (n·σ)|ψ⟩ の成分：上 = nz α + (nx − i ny) β、下 = (nx + i ny) α − nz β
      uR = nz * aR + nx * bR + ny * bI
      uI = nz * aI + nx * bI - ny * bR
      dR = nx * aR - ny * aI - nz * bR
      dI = nx * aI + ny * aR - nz * bI
      -- ψ' = c ψ − i s (n·σ)ψ　（−i (x + iy) = y − ix）
      aR' = c * aR + s * uI
      aI' = c * aI - s * uR
      bR' = c * bR + s * dI
      bI' = c * bI - s * dR
      n0 = sqrt (aR' * aR' + aI' * aI' + bR' * bR' + bI' * bI')   -- 誤差がたまらないよう正規化
      nrm = if n0 > 0 then n0 else 1
  in Psi (aR' / nrm) (aI' / nrm) (bR' / nrm) (bI' / nrm)

-- ブロッホベクトル (x, y, z) = (2 Re(α*β), 2 Im(α*β), |α|² − |β|²)
bloch :: Psi -> V3
bloch (Psi aR aI bR bI) =
  V3 (2 * (aR * bR + aI * bI)) (2 * (aR * bI - aI * bR)) (aR * aR + aI * aI - bR * bR - bI * bI)

-- ブロッホ座標 → 画面の 3D 座標（z が上、x が手前、y が右）
toScreen :: Float -> V3 -> V3
toScreen r (V3 x y z) = V3 (y * r) (z * r) (x * r)

gateOf :: Char -> Maybe Gate
gateOf name =
  let r2 = sqrt 0.5
      mk n th = Just (Gate n th animFrames name)
  in case name of
       'X' -> mk (V3 1 0 0) pi
       'Y' -> mk (V3 0 1 0) pi
       'Z' -> mk (V3 0 0 1) pi
       'H' -> mk (V3 r2 0 r2) pi
       'S' -> mk (V3 0 0 1) (pi / 2)
       'T' -> mk (V3 0 0 1) (pi / 4)
       _ -> Nothing

stepW :: Env -> Float -> World -> World
stepW env dt w =
  let -- ゲートを少しずつ回す
      (psi1, queue, lastG) = case wQueue w of
        [] -> (wPsi w, [], wLast w)
        (q : rest) ->
          let p = applyRot (gN q) (gTh q / fromIntegral animFrames) (wPsi w)
              left = gLeft q - 1
          in (p, if left <= 0 then rest else q { gLeft = left } : rest, [gName q])
      psi2 = if wField w then applyRot (V3 0 0 1) (larmor * dt) psi1 else psi1
      held k rot p = if isDown env (SpecialKey k) then rot p else p
      psi3 = held KeyLeft (applyRot (V3 0 1 0) (-0.03))
           . held KeyRight (applyRot (V3 0 1 0) 0.03)
           . held KeyUp (applyRot (V3 1 0 0) (-0.03))
           . held KeyDown (applyRot (V3 1 0 0) 0.03) $ psi2
  in w { wPsi = psi3, wQueue = queue, wLast = lastG
       , wTrail = take trailMax (bloch psi3 : wTrail w) }

-- 視点の回転（p5 の rotateX → rotateY と同じ順。y を上向きにした分、X の符号が逆になる）
viewOf :: World -> V3 -> V3
viewOf w = rotX (negate (wRotX w)) . rotY (wRotY w)

camD :: Env -> Float
camD env = envH env / 2 / tan (pi / 6)     -- p5 の標準のカメラの距離

drawW :: Env -> World -> Picture
drawW env w =
  let r = min (envW env) (envH env) * 0.3
      d = camD env
      view = viewOf w
      proj = project d
      depthOf (V3 _ _ z) = z
      -- 3D の線分 1 本 → (奥行き, 絵)
      seg3 wd c a b =
        let a' = view a
            b' = view b
        in ((depthOf a' + depthOf b') / 2, Color c (thickLine wd (proj a') (proj b')))
      -- 長い線は奥行きの前後が正しく出るよう短く分ける
      long3 wd c a b n =
        [ seg3 wd c (lerp3 a b (fromIntegral i / fromIntegral n)) (lerp3 a b (fromIntegral (i + 1) / fromIntegral n))
        | i <- [0 .. n - 1 :: Int] ]
      ball3 c v rad =
        let v'@(V3 _ _ z) = view v
            k = d / max 1 (d - z)
        in (z + rad, Color c (at (proj v') (disc (rad * k))))
      -- 球の骨組み：緯線（lat）と経線（lon）
      ringPts kind a =
        [ toScreen r (case kind of
            'a' -> V3 (cos a * cos u) (cos a * sin u) (sin a)
            _ -> V3 (cos a * sin u) (sin a * sin u) (cos u))
        | i <- [0 .. 64 :: Int], let u = fromIntegral i / 64 * tau ]
      ringC alpha = rgba255 200 210 255 alpha
      ring3 kind alpha a =
        let ps = ringPts kind (a * pi / 180)
        in zipWith (seg3 1 (ringC alpha)) ps (drop 1 ps)
      lats = concat [ ring3 'a' (if lat == 0 then 120 else 50) lat | lat <- [-60, -30, 0, 30, 60] ]
      lons = concat [ ring3 'o' 40 lon | lon <- [0, 30, 60, 90, 120, 150] ]
      big = r * 1.25
      axes = long3 3 (rgb255 255 110 110) (V3 0 0 (-big)) (V3 0 0 big) 20       -- x 赤
          ++ long3 3 (rgb255 110 230 140) (V3 (-big) 0 0) (V3 big 0 0) 20       -- y 緑
          ++ long3 3 (rgb255 120 170 255) (V3 0 (-big) 0) (V3 0 big 0) 20       -- z 青
      -- 北極 |0⟩ と南極 |1⟩
      poles = [ ball3 (rgb255 120 170 255) (V3 0 r 0) 7, ball3 (rgb255 60 90 160) (V3 0 (-r) 0) 7 ]
      -- 軌跡
      trailPts = map (toScreen r) (wTrail w)
      trail = zipWith (seg3 2 (rgba255 255 220 120 150)) trailPts (drop 1 trailPts)
      -- 状態ベクトル
      v = bloch (wPsi w)
      p@(V3 _ py _) = toScreen r v
      arrowItems = long3 5 (rgb255 255 230 120) (V3 0 0 0) p 8
      tip = ball3 (rgb255 255 230 120) p 12
      -- z 軸への影（測定で |0⟩ が出る確率 = (1 + z)/2）
      shadow = long3 1 (rgba255 255 230 120 90) p (V3 0 py 0) 4
      items = depthSort fst (lats ++ lons ++ axes ++ poles ++ trail ++ arrowItems ++ [tip] ++ shadow)
  in Pictures (map snd items ++ [hudPic env w v])

hudPic :: Env -> World -> V3 -> Picture
hudPic env w (V3 vx vy vz) =
  let Psi aR aI bR bI = wPsi w
      p0 = (1 + vz) / 2
      px = 20
      py = envH env - 250
      cplx a b = nf 3 a ++ (if b >= 0 then " + " else " - ") ++ nf 3 (abs b) ++ "i"
      yellow' = rgb255 255 230 120
      grayT = gray 0.67
      tot = wC0 w + wC1 w
      statPics = case wStat w of
        Nothing -> []
        Just c0 ->
          let f0 = fromIntegral c0 / 1000 :: Float
          in [ txt env px (py + 140) 14 white ("K: of 1000 runs |0> " ++ show c0 ++ ", |1> " ++ show (1000 - c0))
             , box env (rgb255 120 170 255) px (py + 150) (f0 * 360) 16
             , box env (rgb255 60 90 160) (px + f0 * 360) (py + 150) ((1 - f0) * 360) 16 ]
  in Pictures $
       [ txt env 20 26 16 white ("X Y Z H S T: gates (last " ++ wLast w ++ ")   M: measure   K: 1000-run stats   B: field " ++ (if wField w then "ON" else "off"))
       , txt env 20 48 16 white "Arrows: small turns   R: back to |0>   Drag: view"
       , txt env 20 70 14 grayT "Blue = z (up |0>, down |1>), red = x (front |+>), green = y (right |+i>)."
       , txt env 20 90 14 grayT "H takes |0> to |+> on the equator. S and T turn about z (phase)."
       , box env (rgba255 0 0 0 120) (px - 10) (py - 30) 440 270
       , txt env px py 16 yellow' ("alpha = " ++ cplx aR aI)
       , txt env px (py + 26) 16 yellow' ("beta = " ++ cplx bR bI)
       , txt env px (py + 56) 14 white ("Bloch (x, y, z) = (" ++ nf 2 vx ++ ", " ++ nf 2 vy ++ ", " ++ nf 2 vz ++ ")")
       , txt env px (py + 82) 14 white ("P(|0>) = " ++ nf 3 p0 ++ "   P(|1>) = " ++ nf 3 (1 - p0))
       , txt env px (py + 112) 14 white ("M counts: |0> " ++ show (wC0 w) ++ "   |1> " ++ show (wC1 w))
       ]
       ++ statPics
       ++ (if tot > 0
             then [ txt env px (py + 190) 14 grayT "Measuring makes the state jump to a"
                  , txt env px (py + 210) 14 grayT "pole (collapse)." ]
             else [ txt env px (py + 190) 14 grayT "Measuring destroys the state. K means: prepare"
                  , txt env px (py + 210) 14 grayT "1000 copies and measure each once." ])

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventMotion p@(mx, my) ->
    let (lx, ly) = wPrev w
        movedX = mx - lx
        movedY = -(my - ly)          -- p5 の movedY は下向きが正
    in pure $ if envDown env
         then w { wRotY = wRotY w + movedX * 0.01
                , wRotX = clampF (-1.5) 1.5 (wRotX w - movedY * 0.01)
                , wPrev = p }
         else w { wPrev = p }
  EventKey (MouseButton LeftButton) Down _ p -> pure w { wPrev = p }
  EventKey (Char c) Down _ _ -> onChar (toUpper c) w
  _ -> pure w

onChar :: Char -> World -> IO World
onChar k w
  | Just g <- gateOf k = pure w { wQueue = wQueue w ++ [g] }
  | k == 'M' = do
      let Psi aR aI _ _ = wPsi w
          p0 = aR * aR + aI * aI
      r <- randomRIO (0, 1)
      pure $ if r < p0
        then w { wPsi = Psi 1 0 0 0, wQueue = [], wC0 = wC0 w + 1 }
        else w { wPsi = Psi 0 0 1 0, wQueue = [], wC1 = wC1 w + 1 }
  | k == 'K' = do
      let Psi aR aI _ _ = wPsi w
          p0 = aR * aR + aI * aI
      rs <- replicateM 1000 (randomRIO (0, 1))
      pure w { wStat = Just (length (filter (< p0) rs)) }
  | k == 'B' = pure w { wField = not (wField w) }
  | k == 'R' = pure w { wPsi = state0, wQueue = [], wTrail = [], wC0 = 0, wC1 = 0, wStat = Nothing }
  | otherwise = pure w

-- ---------- やってみよう ----------
-- 1. H を 2 回押すと元に戻る。H → Z → H は何のゲートと同じ？（X）
-- 2. H で赤道へ → B で歳差運動。M で測ると |0⟩ と |1⟩ はいつでも半々。でも K の前に H を押すと？（位相で変わる）
-- 3. T を 8 回押すと 1 周。S は T 何回分？
-- 4. gateOf に好きな軸 (nx, ny, nz) を入れた自分のゲートを作り、キーに割り当てよう

-- ==================================================
-- ここから下は「道具」
-- ==================================================
lerp3 :: V3 -> V3 -> Float -> V3
lerp3 a b t = add3 a (scale3 t (sub3 b a))

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = pt env x y in label gx gy size c s

box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)
