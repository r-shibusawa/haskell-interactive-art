-- ============================================
-- 13 群ロボット（swarm）
-- 1 台は単純でも、たくさん集まると「群れ」として形をつくる。中央の司令塔はなく、各ロボットは近くの仲間しか見ない
--   1 群れ（boids）：離れすぎない・近すぎない・向きをそろえる
--   2〜4 隊形：各ロボットに「席」を割り当てて向かう（円・一列・V 字）
--   5 合意（rendezvous）：近くの仲間の平均位置へ動く → 全員が 1 点に集まる
-- クリック = 集合点 / 1〜5 = 行動 / ↑↓ = 台数 / O = 障害物 ON/OFF / R = ばらまく
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

maxSpeed, viewR :: Float
maxSpeed = 2.4            -- ★ 最高速度
viewR = 90                -- ★ 仲間が見える距離

-- ロボット 1 台 = (x, y, vx, vy)
type Bot = (Float, Float, Float, Float)

data Obs = Obs !Float !Float !Float

data World = World
  { wBots   :: !(U.Vector Bot)
  , wN      :: !Int       -- ★ 台数
  , wMode   :: !Int
  , wCenter :: !Point     -- 集合点・隊形の中心
  , wUseObs :: !Bool
  }

main :: IO ()
main = runSketch (sketch "21-13 Swarm" world0)
  { skBack  = hsb 215 35 12
  , skStart = \_ -> scatter world0
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World U.empty 30 1 (width / 2, height / 2 + 40) True

obstacles :: [Obs]
obstacles = [Obs (width * 0.3) (height * 0.4) 50, Obs (width * 0.68) (height * 0.65) 65]

modeName :: Int -> String
modeName m = case m of
  1 -> "Flock (boids)"
  2 -> "Circle formation"
  3 -> "Line formation"
  4 -> "V formation"
  _ -> "Consensus (gather)"

scatter :: World -> IO World
scatter w = do
  bs <- replicateM (wN w) ((,,,) <$> randomRIO (0, width) <*> randomRIO (110, height)
                                 <*> randomRIO (-1, 1) <*> randomRIO (-1, 1))
  pure w { wBots = U.fromList bs }

-- ★ 隊形の「席」：i 番目のロボットの目標位置
slot :: World -> Int -> Point
slot w i =
  let (cx, cy) = wCenter w
      n = fromIntegral (wN w) :: Float
      fi = fromIntegral i :: Float
      k = fi - (n - 1) / 2
  in case wMode w of
       2 -> let a = tau * fi / n in (cx + 160 * cos a, cy + 160 * sin a)
       3 -> (cx + k * 22, cy)
       4 -> let s = if even i then 1 else -1
                m = fromIntegral ((i + 1) `div` 2)
            in (cx - m * 18, cy + s * m * 18)
       _ -> (cx, cy)

-- 群れの計算の途中経過（分離、整列、結合、見えている台数）
data Sum7 = Sum7 !Float !Float !Float !Float !Float !Float !Int

-- 群れ：分離・整列・結合（近くの仲間だけ見る）
flockForce :: World -> U.Vector Bot -> Int -> Bot -> Point
flockForce w bots i (bx, by, bvx, bvy) =
  let add acc@(Sum7 sx sy vx vy cx cy n) j (ox, oy, ovx, ovy)
        | j == i || d > viewR || d < 0.01 = acc
        | otherwise =
            let (sx', sy') | d < 30 = (sx - dx / d * (30 - d) / 30, sy - dy / d * (30 - d) / 30)   -- 分離
                           | otherwise = (sx, sy)
            in Sum7 sx' sy' (vx + ovx) (vy + ovy) (cx + ox) (cy + oy) (n + 1)                      -- 整列・結合
        where dx = ox - bx
              dy = oy - by
              d = sqrt (dx * dx + dy * dy)
      Sum7 sx sy vx vy cx cy n = U.ifoldl' add (Sum7 0 0 0 0 0 0 0) bots
      fn = fromIntegral n
      (mx, my) = wCenter w
      (ax0, ay0) | n > 0 = ( sx * 0.6 + (vx / fn - bvx) * 0.05 + (cx / fn - bx) * 0.004
                           , sy * 0.6 + (vy / fn - bvy) * 0.05 + (cy / fn - by) * 0.004 )
                 | otherwise = (0, 0)
  in (ax0 + (mx - bx) * 0.0006, ay0 + (my - by) * 0.0006)    -- ゆるく集合点へ

-- 合意：見える仲間の平均位置へ（自分も含む）
consensusForce :: U.Vector Bot -> Int -> Bot -> Point
consensusForce bots i (bx, by, _, _) =
  let add acc@(cx, cy, n) j (ox, oy, _, _)
        | j /= i && dist2 (ox, oy) (bx, by) < viewR * 1.6 = (cx + ox, cy + oy, n + 1)
        | otherwise = acc
      (sx, sy, cnt) = U.ifoldl' add (bx, by, 1 :: Int) bots
      fn = fromIntegral cnt
  in ((sx / fn - bx) * 0.05, (sy / fn - by) * 0.05)

-- 隊形：自分の席へ。ぶつからないように分離も少し
formationForce :: World -> U.Vector Bot -> Int -> Bot -> Point
formationForce w bots i (bx, by, _, _) =
  let (sx, sy) = slot w i
      add acc@(ax, ay) j (ox, oy, _, _)
        | j /= i && d < 14 && d > 0.01 = (ax - dx / d * 0.3, ay - dy / d * 0.3)
        | otherwise = acc
        where dx = ox - bx
              dy = oy - by
              d = sqrt (dx * dx + dy * dy)
  in U.ifoldl' add ((sx - bx) * 0.02, (sy - by) * 0.02) bots

-- 障害物から離れる
avoid :: World -> Bot -> Point
avoid w (bx, by, _, _)
  | not (wUseObs w) = (0, 0)
  | otherwise = foldl' add (0, 0) obstacles
  where
    add acc@(ax, ay) (Obs ox oy r)
      | d < r + 40 && d > 0.01 =
          let f = (r + 40 - d) / 40
          in (ax + dx / d * f * 0.5, ay + dy / d * f * 0.5)
      | otherwise = acc
      where dx = bx - ox
            dy = by - oy
            d = sqrt (dx * dx + dy * dy)

-- 1 台ずつ順に動かす（先に動いた仲間の新しい位置が、次のロボットに見える）
step :: World -> World
step w = w { wBots = foldl' moveBot (wBots w) [0 .. U.length (wBots w) - 1] }
  where
    moveBot bots i =
      let b@(bx, by, bvx, bvy) = bots U.! i
          (ax1, ay1) = case wMode w of
            1 -> flockForce w bots i b
            5 -> consensusForce bots i b
            _ -> formationForce w bots i b
          (ax2, ay2) = avoid w b
          vx0 = (bvx + ax1 + ax2) * 0.95          -- 慣性と摩擦
          vy0 = (bvy + ay1 + ay2) * 0.95
          sp = sqrt (vx0 * vx0 + vy0 * vy0)
          k = if sp > maxSpeed then maxSpeed / sp else 1
          vx1 = vx0 * k
          vy1 = vy0 * k
          x1 = bx + vx1
          y1 = by + vy1
          (x2, vx2) | x1 < 10 = (10, abs vx1)
                    | x1 > width - 10 = (width - 10, -abs vx1)
                    | otherwise = (x1, vx1)
          (y2, vy2) | y1 < 100 = (100, abs vy1)
                    | y1 > height - 10 = (height - 10, -abs vy1)
                    | otherwise = (y1, vy1)
      in bots U.// [(i, (x2, y2, vx2, vy2))]

drawAll :: World -> Picture
drawAll w =
  let bots = U.toList (wBots w)
      n = wN w
      fn = fromIntegral (max 1 (length bots)) :: Float
      mode = wMode w
      indexed = zip [0 :: Int ..] bots
      -- 仲間どうしの線（群れと合意）
      links | mode == 1 || mode == 5 =
                [ Line [(x1, y1), (x2, y2)]
                | (i, (x1, y1, _, _)) <- indexed
                , (j, (x2, y2, _, _)) <- indexed
                , j > i
                , dist2 (x1, y1) (x2, y2) < viewR ]
            | otherwise = []
      botPic (i, (x, y, vx, vy)) =
        let p = (x, y)
            th = atan2 vy vx
        in Pictures
           [ Color (hsb (remap (fromIntegral i) 0 (fromIntegral n) 170 330) 60 95) (bodyRect p th (-9) (-7) 18 14)
           , Color (hsb 0 0 100) (bodyTri p th (9, 0) (3, -4) (3, 4)) ]
      -- 群れの広がり（平均からの距離の平均）
      mx = sum [x | (x, _, _, _) <- bots] / fn
      my = sum [y | (_, y, _, _) <- bots] / fn
      spread = sum [dist2 (x, y) (mx, my) | (x, y, _, _) <- bots] / fn
  in Pictures
  [ view
      ( [ if wUseObs w then Color (hsb 0 60 70) (Pictures [at (x, y) (disc r) | Obs x y r <- obstacles]) else Blank ]
        ++ [ Color (hsbA 0 0 100 20) (Pictures [at (slot w i) (disc 4) | i <- [0 .. n - 1]]) | mode >= 2 && mode <= 4 ]   -- 席（隊形モード）
        ++ [ Color (hsbA 120 80 100 70) (at (wCenter w) (disc 7))
           , Color (hsbA 170 60 100 15) (Pictures links) ]
        ++ map botPic indexed )
  , txt 20 34 16 (hsb 0 0 100) ("Behavior: " ++ modeName mode ++ " (1-5)   " ++ show n ++ " bots (Up/Down)   Spread " ++ nf 0 spread ++ " px")
  , txt 20 58 14 (hsb 0 0 100) ("Click: meeting point   O: obstacles " ++ onOff (wUseObs w) ++ "   R: scatter")
  , txt 20 78 14 (hsb 0 0 60) ("Each bot sees only mates within " ++ nf 0 viewR ++ " px (faint lines), yet a shape emerges. Mode 5 is a consensus algorithm.")
  ]

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let m@(_, my) = toP5 pos
    in pure (if my > 90 then w { wCenter = m } else w)
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '5' -> pure w { wMode = fromEnum c - fromEnum '0' }
    | otherwise -> case toLower c of
        'o' -> pure w { wUseObs = not (wUseObs w) }
        'r' -> scatter w
        _ -> pure w
  EventKey (SpecialKey k) Down _ _ -> case k of
    KeyUp -> scatter w { wN = min 80 (wN w + 10) }
    KeyDown -> scatter w { wN = max 10 (wN w - 10) }
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. viewR を 30 にすると群れが分裂する。合意（5）は 1 点に集まれる？（グラフがつながっていないと平均が一致しない）
-- 2. 隊形の席を自分で設計しよう（ハート、文字、十字）。slot に mode 6 を追加
-- 3. 群れ（1）で分離を 0 にするとどうなる？ 整列を 0 にすると？ 3 つの力の役割
-- 4. 障害物の間を V 字隊形が通り抜けられる？ 通れないなら「隊形を一時的にくずす」ルールを考えよう


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

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- (px, py) に向き th で置いた機体の中の点を、画面の座標に変える（p5 の translate + rotate）
place :: Point -> Float -> Point -> Point
place (px, py) th (x, y) = (px + x * cos th - y * sin th, py + x * sin th + y * cos th)

-- 機体の中の長方形（左上 (x, y)、幅 bw、高さ bh）
bodyRect :: Point -> Float -> Float -> Float -> Float -> Float -> Picture
bodyRect p th x y bw bh = Polygon (map (place p th) [(x, y), (x + bw, y), (x + bw, y + bh), (x, y + bh)])

-- 機体の中の三角形
bodyTri :: Point -> Float -> Point -> Point -> Point -> Picture
bodyTri p th a b c = Polygon (map (place p th) [a, b, c])
