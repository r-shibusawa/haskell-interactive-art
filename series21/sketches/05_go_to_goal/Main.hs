-- ============================================
-- 05 ゴールへ向かう ─ go-to-goal と pure pursuit
-- 「向きたい方向」と「いま向いている方向」の差を、回転速度にフィードバックする
--   1 go-to-goal：ω = Kp × (目標の方角 − θ)。ゴールごとにまっすぐ向かう
--   2 pure pursuit：経路の少し先（lookahead 距離 Ld）の点を追いかける。曲がり角がなめらかになる
-- クリック = 経由点を追加 / 1・2 = 方法 / ↑↓ = Kp / ←→ = Ld / C = 経由点を消す / R = ロボットを戻す
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl')
import Text.Printf (printf)

vMax :: Float
vMax = 3                  -- ★ 最大速度

data World = World
  { wX      :: !Float     -- ロボットの位置と向き
  , wY      :: !Float
  , wTh     :: !Float
  , wWays   :: ![Point]   -- 経由点
  , wMethod :: !Int
  , wKp     :: !Float     -- ★ 向きのゲイン
  , wLd     :: !Float     -- ★ pure pursuit の先読み距離
  , wCur    :: !Int       -- いま目指している経由点の番号
  , wTrail  :: ![Point]   -- 軌跡（新しい順、2000 点まで）
  , wLook   :: !(Maybe Point)   -- 先読み点
  , wV      :: !Float     -- 画面表示用の速度
  , wOm     :: !Float     -- 画面表示用の回転速度
  }

main :: IO ()
main = runSketch (sketch "21-05 Go to Goal" world0)
  { skBack  = hsb 215 35 12
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = resetRobot World
  { wX = 0, wY = 0, wTh = 0
  , wWays = [(width * 0.3, height * 0.3), (width * 0.7, height * 0.35), (width * 0.6, height * 0.75), (width * 0.25, height * 0.7)]
  , wMethod = 1, wKp = 0.08, wLd = 90, wCur = 0, wTrail = [], wLook = Nothing, wV = 0, wOm = 0 }

resetRobot :: World -> World
resetRobot w = w { wX = 120, wY = height - 120, wTh = 0, wCur = 0, wTrail = [] }

angDiff :: Float -> Float
angDiff a = atan2 (sin a) (cos a)

-- ★ 1：go-to-goal。目標の方角へ比例制御で向きを合わせる。戻り値は (v, ω, 次の番号)
goToGoal :: World -> (Float, Float, Int)
goToGoal w = case drop (wCur w) (wWays w) of
  [] -> (0, 0, wCur w)
  ((gx, gy) : _) ->
    let dx = gx - wX w
        dy = gy - wY w
        d = sqrt (dx * dx + dy * dy)
        e = angDiff (atan2 dy dx - wTh w)
        v = vMax * max 0 (cos e) * min 1 (d / 60)    -- 向きがずれている間は遅く、近づいたら減速
    in if d < 12 then (0, 0, wCur w + 1) else (v, wKp w * e, wCur w)

-- ★ 2：pure pursuit。経路（経由点をつないだ折れ線）上で、ロボットから Ld 離れた点を追う
--   戻り値は (v, ω, 次の番号, 先読み点)
purePursuit :: World -> (Float, Float, Int, Maybe Point)
purePursuit w = case wWays w of
  [] -> (0, 0, wCur w, wLook w)
  (w1 : _) ->
    let ways = wWays w
        n = length ways
        robot = (wX w, wY w)
        lastP = foldl' (\_ p -> p) w1 ways
        dEnd = dist2 lastP robot
        ld = wLd w
        wayAt i = case drop i ways of { (p : _) -> p; [] -> lastP }
        -- 経路を順にたどって、ロボットからの距離が Ld を越える最初の点を探す
        search i cur
          | i >= n = (cur, lastP)
          | otherwise =
              let a = if i == 0 then robot else wayAt (i - 1)
                  b = wayAt i
                  far = [ p | u <- [0 .. 50 :: Int]
                            , let k = fromIntegral u * 0.02
                            , let p = (lerpF (fst a) (fst b) k, lerpF (snd a) (snd b) k)
                            , dist2 p robot >= ld ]
              in case far of
                   (p : _) -> (cur, p)
                   [] ->
                     -- この区間を通りすぎたら次の区間へ
                     let cur' = if i == cur && dist2 b robot < ld then min (cur + 1) (n - 1) else cur
                     in search (i + 1) cur'
        (curNext, look@(lx, ly)) = search (wCur w) (wCur w)
        -- 曲率 κ = 2 sin α / Ld（α = 先読み点の方角とロボットの向きの差）
        alpha = angDiff (atan2 (ly - wY w) (lx - wX w) - wTh w)
        kappa = 2 * sin alpha / ld
        v = vMax * (if dEnd < 60 then dEnd / 60 else 1)
    in if dEnd < 10 then (0, 0, wCur w, Nothing) else (v, v * kappa, curNext, Just look)

step :: Env -> World -> World
step env w =
  let (v, om0, cur, look)
        | wMethod w == 1 = let (a, b, c) = goToGoal w in (a, b, c, wLook w)
        | otherwise = purePursuit w
      om = clampF (-0.15) 0.15 om0
      x = wX w + v * cos (wTh w)
      y = wY w + v * sin (wTh w)
      trail | v > 0.01 && even (envFrame env) = take 2000 ((x, y) : wTrail w)
            | otherwise = wTrail w
  in w { wX = x, wY = y, wTh = wTh w + om, wCur = cur, wLook = look, wTrail = trail, wV = v, wOm = om }

drawAll :: World -> Picture
drawAll w =
  let robot = (wX w, wY w)
      th = wTh w
      ways = zip [0 :: Int ..] (wWays w)
      wayDot (i, p) = Color (if i < wCur w then hsb 0 0 40 else hsb 120 80 100) (at p (disc 11))
      wayNum (i, (x, y)) = labelC (x - width / 2) (height / 2 - y) 14 (hsb 0 0 10) (show (i + 1))
      pursuit
        | wMethod w /= 2 = []
        | otherwise =
            Color (hsbA 320 60 100 50) (at robot (ring (wLd w) 1))
            : case wLook w of
                Just p -> [ Color (hsb 320 60 100) (at p (disc 5))
                          , Color (hsbA 320 60 100 70) (Line [robot, p]) ]
                Nothing -> []
  in Pictures
  [ view
      ( [ Color (hsbA 0 0 100 25) (Line (wWays w)) ]      -- 経路
        ++ map wayDot ways )
  , Pictures (map wayNum ways)
  , view
      ( [ Color (hsbA 45 90 100 80) (thickPath 2 (wTrail w)) ]   -- 軌跡
        ++ pursuit
        ++ [ Color (hsb 170 60 95) (bodyRect robot th (-20) (-15) 40 30)
           , Color (hsb 0 0 100) (bodyTri robot th (20, 0) (8, -8) (8, 8)) ] )
  , txt 20 34 16 (hsb 0 0 100) ("Method: " ++ (if wMethod w == 1 then "go-to-goal (1)" else "pure pursuit (2)") ++ "   Kp = " ++ nf 2 (wKp w)
                                ++ " (Up/Down)   Ld = " ++ nf 0 (wLd w) ++ " (L/R)   v = " ++ nf 1 (wV w) ++ "  w = " ++ nf 3 (wOm w))
  , txt 20 58 14 (hsb 0 0 100) ("Waypoints " ++ show (length (wWays w)) ++ " (click: add, C: clear)   R: back to start")
  , txt 20 78 14 (hsb 0 0 60) "1 stops at corners to turn. 2 chases a point ahead and rounds corners. Larger Ld cuts corners more."
  ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let m@(_, my) = toP5 pos
    in if my < 90 then w else w { wWays = wWays w ++ [m] }
  EventKey (Char c) Down _ _ -> case toLower c of
    '1' -> w { wMethod = 1, wCur = min (wCur w) n }
    '2' -> w { wMethod = 2, wCur = min (wCur w) (max 0 (n - 1)) }
    'c' -> w { wWays = [], wCur = 0, wLook = Nothing }
    'r' -> resetRobot w
    _ -> w
  EventKey (SpecialKey k) Down _ _ -> case k of
    KeyUp -> w { wKp = min 0.5 (wKp w * 1.3) }
    KeyDown -> w { wKp = max 0.01 (wKp w / 1.3) }
    KeyRight -> w { wLd = min 300 (wLd w + 15) }
    KeyLeft -> w { wLd = max 20 (wLd w - 15) }
    _ -> w
  _ -> w
  where n = length (wWays w)

-- ---------- やってみよう ----------
-- 1. Kp を大きくすると向きを合わせるのは速いが、ふらつく（振動）。ちょうどいい値を探そう
-- 2. Ld を 20 と 300 で比べる。小さいとジグザグ、大きいと角をショートカット
-- 3. 経由点を「く」の字に置いて、1 と 2 の軌跡の形を比べよう
-- 4. vMax を上げると曲がりきれなくなる。ω の上限（clampF）を変えるとどうなる？


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

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 小数点以下 d 桁の文字列（p5 の nf）
nf :: Int -> Float -> String
nf d = printf ("%." ++ show d ++ "f")

-- (px, py) に向き th で置いた機体の中の点を、画面の座標に変える（p5 の translate + rotate）
place :: Point -> Float -> Point -> Point
place (px, py) th (x, y) = (px + x * cos th - y * sin th, py + x * sin th + y * cos th)

-- 機体の中の長方形（左上 (x, y)、幅 bw、高さ bh）
bodyRect :: Point -> Float -> Float -> Float -> Float -> Float -> Picture
bodyRect p th x y bw bh = Polygon (map (place p th) [(x, y), (x + bw, y), (x + bw, y + bh), (x, y + bh)])

-- 機体の中の三角形
bodyTri :: Point -> Float -> Point -> Point -> Point -> Picture
bodyTri p th a b c = Polygon (map (place p th) [a, b, c])
