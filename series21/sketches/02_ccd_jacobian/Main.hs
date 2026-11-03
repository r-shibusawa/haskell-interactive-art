-- ============================================
-- 02 多リンクの逆運動学 ─ CCD とヤコビアン転置
-- リンクが多いと式では解けない。だから「少しずつ近づける」反復法を使う
--   CCD：先端の関節から順に、手先が目標を向くように 1 関節ずつ回す
--   ヤコビアン転置：各関節を少し回したら手先がどう動くか（J）から、目標方向へ全関節を同時に回す
-- マウス = 目標 / 1 = CCD / 2 = ヤコビアン転置 / L = 関節の角度制限 / ↑↓ = リンク数 / スペース = 1 回ずつ進める（→ で 1 回）/ R = リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl')
import Text.Printf (printf)

linkLen, maxAngle, gain :: Float
linkLen = 70                     -- ★ 1 本の長さ
maxAngle = pi * 100 / 180        -- 関節制限（ラジアン）
gain = 0.0012                    -- ★ ヤコビアン転置のステップ幅

itersPerFrame :: Int
itersPerFrame = 1                -- ★ 1 フレームに何回反復するか

data World = World
  { wN      :: !Int       -- ★ リンク数（3〜8）
  , wQ      :: ![Float]   -- 各関節の角度（1 つ前のリンクからの相対角）
  , wMethod :: !Int       -- 1 = CCD, 2 = ヤコビアン転置
  , wLimit  :: !Bool      -- 関節制限 ±100°
  , wStep   :: !Bool      -- スペースで手動
  , wIter   :: !Int       -- 目標が動いてからの反復回数
  , wConv   :: !Bool      -- 収束したか
  , wHist   :: ![Float]   -- 誤差の履歴（新しい順、300 個まで）
  , wLast   :: !Point     -- 前のフレームの目標
  }

main :: IO ()
main = runSketch (sketch "21-02 CCD and Jacobian" world0)
  { skBack  = hsb 215 35 12
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = \env w -> drawAll (targetOf env) w
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

world0 :: World
world0 = resetChain (World 5 [] 1 False False 0 False [] (0, 0))

resetChain :: World -> World
resetChain w = w { wQ = 1.2 : replicate (wN w - 1) (-0.3), wIter = 0, wHist = [] }

basePos :: Point
basePos = (width * 0.4, height * 0.65)

-- マウスの位置が目標（まだ動かしていなければ台座の右上）
targetOf :: Env -> Point
targetOf env
  | envMouse env == (0, 0) = (fst basePos + 200, snd basePos - 150)
  | otherwise = toP5 (envMouse env)

-- 順運動学：各関節の位置（先頭 = 台座、最後 = 手先）
joints :: [Float] -> [Point]
joints q = map fst (scanl next (basePos, 0) q)
  where next ((x, y), a) qi =
          let a' = a + qi
          in ((x + linkLen * cos a', y - linkLen * sin a'), a')

handOf :: [Point] -> Point
handOf = foldl' (\_ p -> p) basePos

clampQ :: Bool -> [Float] -> [Float]
clampQ False q = q
clampQ True q = zipWith lim [0 :: Int ..] q
  where lim i v = if i >= 1 then clampF (-maxAngle) maxAngle v else v

-- ★ CCD：手先に近い関節から順に、「関節→手先」が「関節→目標」を向くように回す
ccdStep :: Bool -> Point -> [Float] -> [Float]
ccdStep lim (tx, ty) q0 = foldl' turn q0 [length q0 - 1, length q0 - 2 .. 0]
  where
    turn q i =
      let pts = joints q
          (jx, jy) = case drop i pts of { (p : _) -> p; [] -> basePos }
          (hx, hy) = handOf pts
          aH = atan2 (-(hy - jy)) (hx - jx)
          aT = atan2 (-(ty - jy)) (tx - jx)
          d = aT - aH
      in clampQ lim (addAt i (atan2 (sin d) (cos d)) q)

-- ★ ヤコビアン転置：Δq = k · Jᵀ · (目標 − 手先)
--   関節 i を少し回すと手先は「関節 i から手先へのベクトルを 90° 回したもの」の方向に動く
jacobianStep :: Bool -> Point -> [Float] -> [Float]
jacobianStep lim (tx, ty) q =
  let pts = joints q
      (hx, hy) = handOf pts
      ex = tx - hx
      ey = -(ty - hy)                   -- 誤差（y 上向き）
      dq (px, py) =
        let rx = hx - px
            ry = -(hy - py)
            jx = -ry                    -- ∂手先/∂q_i
            jy = rx
        in gain * (jx * ex + jy * ey)
  in clampQ lim (zipWith (+) q (map dq pts))

doIteration :: Point -> World -> World
doIteration t w =
  let f = if wMethod w == 1 then ccdStep else jacobianStep
  in w { wQ = f (wLimit w) t (wQ w), wIter = wIter w + 1 }

errOf :: Point -> [Float] -> Float
errOf t q = dist2 (handOf (joints q)) t

step :: Env -> World -> World
step env w0 =
  let t@(tx, ty) = targetOf env
      (lx, ly) = wLast w0
      -- 目標が動いたら数え直す
      w1 | abs (tx - lx) > 2 || abs (ty - ly) > 2 = w0 { wIter = 0, wHist = [], wLast = t, wConv = False }
         | otherwise = w0
      conv = errOf t (wQ w1) < 2
      w2 | not (wStep w1) && not conv = iterate (doIteration t) w1 !! itersPerFrame
         | otherwise = w1
      err = errOf t (wQ w2)
      hist = wHist w2
      hist' | length hist < 300 && (null hist || not conv) = err : hist
            | otherwise = hist
  in w2 { wConv = conv, wHist = hist' }

drawAll :: Point -> World -> Picture
drawAll t@(tx, ty) w =
  let n = wN w
      q = wQ w
      pts = joints q
      hand = handOf pts
      b@(bx, by) = basePos
      err = dist2 hand t
      gx = width - 380
      gy = height - 60
      gw = 340
      gh = 140
      graph = [ (gx + (fromIntegral i / 300) * gw, gy - clampF 0 1 (e / 300) * gh)
              | (i, e) <- zip [0 :: Int ..] (reverse (wHist w)) ]
      link (i, a, c) = Color (hsb (remap (fromIntegral i) 0 (fromIntegral n) 200 330) 60 95)
                             (thickLine (12 - fromIntegral i) a c)
      links = zip3 [0 :: Int ..] pts (drop 1 pts)
      -- 関節（制限にかかっている関節は赤い輪）
      limited i = wLimit w && i > 0 && i < n
                  && case drop i q of
                       (v : _) -> abs (abs v - maxAngle) < 0.01
                       [] -> False
      jointDot (i, p) = Pictures
        [ Color (if i == n then hsb 45 90 100 else hsb 0 0 100) (at p (disc (if i == n then 8 else 7)))
        , if limited i then Color (hsb 0 90 100) (at p (disc 11)) else Blank ]
  in Pictures
  [ view
      ( [ Color (hsbA 45 80 100 30) (at b (ring (fromIntegral n * linkLen) 1)) ]   -- 届く範囲
        ++ map link links
        ++ map jointDot (zip [0 :: Int ..] pts)
        ++ [ Color (hsb 0 0 40) (boxAt (bx - 30) by 60 14)
           -- 目標
           , Color (hsb (if wConv w then 120 else 0) 90 100)
               (Pictures [at t (ring 12 2), thickLine 2 hand t])
           -- 誤差のグラフ
           , Color (hsbA 0 0 100 8) (boxAt (gx - 10) (gy - gh - 10) (gw + 20) (gh + 20))
           , Color (hsb 170 70 100) (thickPath 2 graph)
           ] )
  , txt gx (gy - gh - 16) 14 (hsb 0 0 60) "Error (px), by iteration 0 to 300"
  , txt 20 32 16 (hsb 0 0 100) ("Method: " ++ (if wMethod w == 1 then "CCD (1)" else "Jacobian transpose (2)")
                                ++ "   Links " ++ show n ++ " (Up/Down)   L: limits " ++ (if wLimit w then "ON +-100 deg" else "off")
                                ++ "   Space: step mode " ++ onOff (wStep w))
  , txt 20 56 14 (hsb 0 0 100) ("Iterations " ++ show (wIter w) ++ "   Error " ++ nf 1 err ++ " px" ++ (if wConv w then "   Converged" else "")
                                ++ "   (in step mode, Right = one iteration)   R: reset")
  , txt 20 78 14 (hsb 0 0 60) "CCD arrives fast but the arm curls. Jacobian moves all joints gently but takes more steps. Red joint = at limit."
  ]

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (Char c) Down _ _ -> case toLower c of
    '1' -> w { wMethod = 1, wIter = 0, wHist = [] }
    '2' -> w { wMethod = 2, wIter = 0, wHist = [] }
    'l' -> let lim = not (wLimit w) in w { wLimit = lim, wQ = clampQ lim (wQ w) }
    'r' -> resetChain w
    _ -> w
  EventKey (SpecialKey k) Down _ _ -> case k of
    KeyUp -> resetChain w { wN = min 8 (wN w + 1) }
    KeyDown -> resetChain w { wN = max 3 (wN w - 1) }
    KeySpace -> w { wStep = not (wStep w) }
    KeyRight | wStep w -> doIteration (targetOf env) w
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 同じ目標に対して 1 と 2 で「収束までの回数」と「腕の形」を比べよう（グラフを見る）
-- 2. gain を 0.003 にすると速いが震える。0.0003 だと？ 安定と速さのトレードオフ
-- 3. L で関節制限をつけると届かない目標が出てくる。どんな場所？
-- 4. 腕を 8 本にして、スペース＋→ で 1 回ずつ進め、CCD の「先端から順に」を目で追う


-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- マウスの位置（gloss の座標：中央が原点、y は上向き）を p5 と同じ座標（左上が原点、y は下向き）に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は p5 の text と同じ左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 左上の角が (x, y) の長方形（p5 の rect）
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y bw bh = rect (x + bw / 2) (y + bh / 2) bw bh

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 小数点以下 d 桁の文字列（p5 の nf）
nf :: Int -> Float -> String
nf d = printf ("%." ++ show d ++ "f")

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- リストの i 番目に d を足す
addAt :: Int -> Float -> [Float] -> [Float]
addAt i d xs = [if k == i then x + d else x | (k, x) <- zip [0 ..] xs]
