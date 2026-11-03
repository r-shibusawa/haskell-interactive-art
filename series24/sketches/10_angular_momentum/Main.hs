-- ============================================
-- 10 回転と角運動量
-- フィギュアスケートのスピン：腕をたたむと速く回る。外から力のモーメント（トルク）がなければ角運動量 L = Iω は一定
--   慣性モーメント I = Σ m r²（重さが軸から遠いほど回しにくい）。腕をたたむと I が減る → ω が増える
-- スペース（押している間）= 腕をたたむ / T（押している間）= トルクを加える / R = 逆向きトルク / F = 氷の摩擦 / 0 = リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Text.Printf (printf)

iBody, mHand, rOut, rIn, torque, fric :: Float
iBody = 1.0     -- ★ 体（軸の近く）の慣性モーメント [kg·m²]
mHand = 4       -- ★ 片腕（＋手に持ったおもり）の質量 [kg]
rOut = 0.9      -- ★ 腕を広げたときの半径 [m]
rIn = 0.15      -- ★ 腕をたたんだときの半径 [m]
torque = 6      -- ★ T キーで加えるトルク [N·m]
fric = 0.3      -- ★ 摩擦トルクの係数

histLen :: Int
histLen = 600

data World = World
  { wR, wL, wPhi :: !Float
  , wFriction :: !Bool
  , wTau      :: !Float             -- いまのトルク
  , wPushing  :: !Bool              -- T か R を押している
  , hI, hW, hL, hK :: !(Seq.Seq Float)
  , wTrail    :: !(Seq.Seq (Point, Point))
  }

main :: IO ()
main = runSketch (sketch "24-10 Angular Momentum" world0)
  { skBack  = hsb 205 30 10
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = World 0.9 4 0 False 0 False Seq.empty Seq.empty Seq.empty Seq.empty Seq.empty

inertia :: Float -> Float
inertia rr = iBody + 2 * mHand * rr * rr

cx0, cy0, pxm :: Float
cx0 = width * 0.25
cy0 = height * 0.52
pxm = min width height * 0.28

stepWorld :: Env -> World -> World
stepWorld env w =
  let dt = 1 / 60
      held c = isDown env (Char c) || isDown env (Char (toEnum (fromEnum c - 32)))
      -- 腕の半径を目標へなめらかに近づける（スケーターが自分の筋肉で引き寄せる）
      target = if isDown env (SpecialKey KeySpace) then rIn else rOut
      r = wR w + (target - wR w) * 0.08
      -- ★ 外からのトルク τ だけが L を変える：dL/dt = τ
      push = held 't' || held 'r'
      tau0 = (if held 't' then torque else 0) - (if held 'r' then torque else 0)
      i = inertia r
      om0 = wL w / i
      tauAll = if wFriction w then tau0 - fric * om0 else tau0
      l = clampF (-60) 60 (wL w + tauAll * dt)
      om = l / i                       -- ★ ω = L / I
      phi = wrapF tau (wPhi w + om * dt)
      k = l * l / (2 * i)              -- 回転の運動エネルギー = L² / 2I
      hx = cos phi * r * pxm
      hy = sin phi * r * pxm
      tr = wTrail w Seq.|> ((cx0 + hx, cy0 + hy), (cx0 - hx, cy0 - hy))
  in w { wR = r, wL = l, wPhi = phi, wTau = tauAll, wPushing = push
       , hI = pushHist (hI w) i, hW = pushHist (hW w) om, hL = pushHist (hL w) l, hK = pushHist (hK w) k
       , wTrail = if Seq.length tr > 60 then Seq.drop 1 tr else tr }

drawAll :: World -> Picture
drawAll w = Pictures
  [ -- 上から見たスケーター
    view ( Color (hsb 0 0 25) (at (cx, cy) (ring (rOut * pxm) 1))
         : Color (hsb 0 0 25) (at (cx, cy) (ring (rIn * pxm) 1))
         : concat [ [ Color (hsba 30 70 100 (fromIntegral i * 0.012)) (at p (disc 1.5))
                    , Color (hsba 30 70 100 (fromIntegral i * 0.012)) (at q (disc 1.5)) ]
                  | (i, (p, q)) <- zip [0 :: Int ..] (toList (wTrail w)) ]
        ++ [ Color (hsb 20 30 90) (thickLine 12 (cx - hx, cy - hy) (cx + hx, cy + hy))          -- 腕
           , Color (hsb 220 30 70) (at (cx, cy) (disc 35))                                       -- 体（上から見た頭と肩）
           , Color (hsb 220 30 90) (Polygon [ (cx + 20 * cos a * cos phi - 30 * sin a * sin phi
                                              , cy + 20 * cos a * sin phi + 30 * sin a * cos phi)
                                            | a <- [0, 0.2 .. tau] ])
           , Color (hsb 0 0 20) (at (cx + 14 * cos phi, cy + 14 * sin phi) (disc 4))             -- 顔の向き
           , Color (hsb 30 80 100) (at (cx + hx, cy + hy) (disc 13))                             -- 手のおもり
           , Color (hsb 30 80 100) (at (cx - hx, cy - hy) (disc 13)) ]
        ++ torqueArc )
  , if showTorque
      then txt (cx - 70) (cy - rOut * pxm - 50) 16 (hsb 0 80 100) ("Torque = " ++ nf 1 (wTau w) ++ " N m")
      else Blank
    -- 角運動量ベクトル（上から見ると紙面の手前向き / 奥向き）
  , txt (width * 0.04) (height * 0.88) 15 (hsb 55 90 100)
      ("L = " ++ nf 2 l ++ (if l >= 0 then "  (out of the page)" else "  (into the page)"))
    -- 右のグラフ
  , drawPlot gx (height * 0.12) gw gh [Series (hI w) 200 "I"] 0 (inertia rOut * 1.1) "I = I_body + 2 m r^2  [kg m^2]"
  , drawPlot gx (height * 0.32) gw gh [Series (hW w) 30 "w"] (-25) 25 "w = L / I  [rad/s]"
  , drawPlot gx (height * 0.52) gw gh [Series (hL w) 55 "L"] (-20) 20 "L = I w  (flat when no torque)"
  , drawPlot gx (height * 0.72) gw gh [Series (hK w) 330 "K"] 0 150 "K = L^2 / 2I  [J] (grows when arms fold)"
  , txt 20 36 18 (gray 1) ("Space (hold): fold arms   T / R (hold): torque   F: friction " ++ (if wFriction w then "ON" else "OFF") ++ "   0: reset")
  , txt 20 66 14 (hsb 0 0 65) ("I = " ++ nf 2 i ++ "   w = " ++ nf 2 om ++ " rad/s (" ++ nf 0 (abs om / tau * 60) ++ " rpm)   L = " ++ nf 2 l ++ "   K = " ++ nf 1 (l * l / (2 * i))
                               ++ " J   Folding: I / " ++ nf 1 ratio ++ ", so w x " ++ nf 1 ratio)
  ]
  where
    cx = cx0
    cy = cy0
    phi = wPhi w
    l = wL w
    i = inertia (wR w)
    om = l / i
    hx = cos phi * wR w * pxm
    hy = sin phi * wR w * pxm
    ratio = inertia rOut / inertia rIn
    gx = width * 0.5
    gw = width * 0.46
    gh = height * 0.17
    showTorque = wTau w /= 0 && wPushing w
    -- トルクの矢印（回転方向の円弧）
    arcR = rOut * pxm + 30
    torqueArc
      | not showTorque = []
      | otherwise =
          let s = signum (wTau w)
              a0 = -pi / 2
          in [ Color (hsb 0 80 100) (thickPath 5 [(cx + arcR * cos a, cy + arcR * sin a) | a <- [a0 - 0.6, a0 - 0.5 .. a0 + 0.6]])
             , Color (hsb 0 80 100) (at (cx + cos (a0 + 0.6 * s) * arcR, cy + sin (a0 + 0.6 * s) * arcR) (disc 8)) ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _
    | toLower c == 'f' -> w { wFriction = not (wFriction w) }
    | c == '0' -> w { wL = 4, wR = rOut, wPhi = 0 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. スペースで腕をたたむと何倍速くなる？ I の比と同じになるか確かめる
-- 2. 腕をたたむと運動エネルギー K が増える。エネルギーはどこから来た？（腕を引き寄せる筋肉の仕事）
-- 3. T を押している間だけ L が増える。押していない間は、腕を動かしても L のグラフは水平
-- 4. mHand を 0 にすると（おもりなし）腕をたたんでもあまり変わらない。猫が空中で向きを変えるのも角運動量の工夫


-- ==================================================
-- 道具（グラフを描く）
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 と同じ座標（左上が原点、y は下向き）で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は左下の位置（p5 の座標）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtR x y s c str = txt (x - fromIntegral (length str) * s * 0.72) y s c str

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y w h = rect (x + w / 2) (y + h / 2) w h

nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

pushHist :: Seq.Seq Float -> Float -> Seq.Seq Float
pushHist s !v =
  let s' = s Seq.|> v
  in if Seq.length s' > histLen then Seq.drop 1 s' else s'

data Series = Series
  { sData :: !(Seq.Seq Float)
  , sHue  :: !Float
  , sName :: String
  }

-- 時間プロット：枠 (x, y, w, h) に series を描く（縦の範囲 lo〜hi）
drawPlot :: Float -> Float -> Float -> Float -> [Series] -> Float -> Float -> String -> Picture
drawPlot x0 y0 w h series lo hi title = Pictures
  ( view ( Color (hsb 0 0 14) (boxAt x0 y0 w h)
         : [Color (hsb 0 0 30) (Line [(x0, yz), (x0 + w, yz)]) | yz > y0 && yz < y0 + h]
        ++ map curve series )
  : txt (x0 + 8) (y0 + 20) 14 (hsb 0 0 70) title
  : legend (x0 + w - 8) series )
  where
    yz = remap 0 lo hi (y0 + h) y0
    curve s =
      let d = toList (sData s)
          off = histLen - length d
      in Color (hsb (sHue s) 70 100) (thickPath 2
           [ ( remap (fromIntegral (i + off)) 0 (fromIntegral histLen - 1) x0 (x0 + w)
             , remap (clampF lo hi v) lo hi (y0 + h) y0 )
           | (i, v) <- zip [0 :: Int ..] d, even i ])
    legend _ [] = []
    legend lx (s : rest) = txtR lx (y0 + 20) 14 (hsb (sHue s) 70 100) (sName s)
                         : legend (lx - fromIntegral (length (sName s)) * 14 * 0.72 - 14) rest
