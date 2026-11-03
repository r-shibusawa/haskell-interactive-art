-- ============================================
-- 13 ボロノイを聞く
-- セル 1 つ 1 つが「鍵盤」。動くカーソルが新しいセルに入るたびに、そのセルの音が鳴る
--   音の高さ：1 = サイトの高さ（上ほど高い音）/ 2 = セルの面積（広いほど低い音）。サイトはゆっくり漂うので、メロディが変わり続ける
-- クリックで音をスタート（もう一度クリック = サイトを追加）/ M = マウスで弾く ⇄ 自動 / 1・2 = 音の決め方 / ↑↓ = カーソルの速さ / スペース = 漂う ON/OFF / R = やり直し
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM, when)
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

nSites :: Int
nSites = 14               -- ★ セルの数

penta :: U.Vector Int
penta = U.fromList [0, 2, 4, 7, 9]   -- ★ 音階（ペンタトニック：どう弾いてもきれいに聞こえる）

-- 動ける範囲
bx0, by0, bx1, by1 :: Float
bx0 = 0
by0 = 90
bx1 = width
by1 = height

data Site = Site { sX, sY, sVx, sVy, sHue, sGlow :: !Float }   -- sGlow = セルの光り具合

data World = World
  { wSites  :: ![Site]
  , wCells  :: ![[Point]]
  , wPen    :: !Point     -- カーソル
  , wCur    :: !Int       -- 今いるセル
  , wMouse  :: !Bool      -- マウスで弾く
  , wPitch  :: !Int       -- 音の決め方
  , wSpeed  :: !Float     -- ★ 自動カーソルの速さ
  , wTt     :: !Float
  , wDrift  :: !Bool
  , wTrail  :: ![Point]   -- カーソルの軌跡（60 点まで）
  , wAudio  :: !Bool      -- 音がスタートしたか
  }

main :: IO ()
main = runSketch (sketch "22-13 Voronoi Sound" world0)
  { skBack  = hsb 230 25 12
  , skStart = \_ -> reset world0
  , skStep  = \env _ w -> step env w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> onEvent ev w
  }

world0 :: World
world0 = World [] [] (0, 0) (-1) False 1 1 0 True [] False

addSite :: Float -> Float -> IO Site
addSite x y = do
  a <- randomRIO (0, tau)
  h <- randomRIO (0, 360)
  pure (Site x y (cos a * 0.4) (sin a * 0.4) h 0)

reset :: World -> IO World
reset w = do
  ss <- replicateM nSites $ do
    x <- randomRIO (bx0 + 40, bx1 - 40)
    y <- randomRIO (by0 + 40, by1 - 40)
    addSite x y
  pure w { wSites = ss, wCur = -1 }

-- ★ セルの音の高さ（Hz）
cellFreq :: Int -> Site -> [Point] -> Float
cellFreq pitch s c = 220 * 2 ** (fromIntegral semi / 12)
  where
    a = polyArea c / ((bx1 - bx0) * (by1 - by0))
    k0 | pitch == 1 = floor (remap (sY s) by1 by0 0 15)        -- 上ほど高い
       | otherwise = floor (clampF 0 15 (remap (log (a + 1e-6)) (log 0.2) (log 0.005) 0 15))  -- 広いほど低い
    k = max 0 (min 15 k0) :: Int
    semi = 12 * (k `div` 5) + penta U.! (k `mod` 5)

-- サイトが漂う
driftSite :: Site -> Site
driftSite s =
  let x = sX s + sVx s
      y = sY s + sVy s
      vx = if x < bx0 + 20 || x > bx1 - 20 then negate (sVx s) else sVx s
      vy = if y < by0 + 20 || y > by1 - 20 then negate (sVy s) else sVy s
  in s { sX = clampF (bx0 + 20) (bx1 - 20) x, sY = clampF (by0 + 20) (by1 - 20) y, sVx = vx, sVy = vy }

step :: Env -> World -> IO World
step env w0' = do
  let moved = if wDrift w0' then map driftSite (wSites w0') else wSites w0'
      points = map (\s -> (sX s, sY s)) moved
      cells = map (\p -> voronoiCell points p bx0 by0 bx1 by1) points
      (mx, my) = toP5 (envMouse env)
      tt = if wMouse w0' then wTt w0' else wTt w0' + 0.008 * wSpeed w0'
      -- カーソル：自動ならリサジュー曲線、M でマウス
      pen@(px, py)
        | wMouse w0' = (mx, clampF by0 by1 my)
        | otherwise = ( width / 2 + (width / 2 - 60) * sin (tt * 1.3)
                      , (by0 + by1) / 2 + ((by1 - by0) / 2 - 40) * sin (tt * 2.1 + 0.5) )
      -- 今いるセル = いちばん近いサイト。変わったら音を鳴らす
      pick (bi, bd) (i, (x, y)) =
        let d = (px - x) * (px - x) + (py - y) * (py - y)
        in if d < bd then (i, d) else (bi, bd)
      (best, _) = foldl' pick (-1 :: Int, 1 / 0 :: Float) (zip [0 ..] points)
      changed = best /= wCur w0' && best >= 0
      lit i s = s { sGlow = (if changed && i == best then 1 else sGlow s) * 0.94 }
      sites' = zipWith lit [0 ..] moved
  when (changed && wAudio w0') $
    case drop best (zip sites' cells) of
      ((s, c) : _) -> playTone (clampF 30 4000 (cellFreq (wPitch w0') s c)) 0.25 0.6 (if wPitch w0' == 1 then Triangle else Sine)
      [] -> pure ()
  pure w0'
    { wSites = sites', wCells = cells, wPen = pen, wTt = tt
    , wCur = if changed then best else wCur w0'
    , wTrail = drop (length (wTrail w0') + 1 - 60) (wTrail w0') ++ [pen] }

drawAll :: World -> Picture
drawAll w = Pictures
  [ view (zipWith cellPic (wSites w) (wCells w))
  , view [dotAt (gray 1) (sX s, sY s) 7 | s <- wSites w]
  , Pictures [ txt (sX s + 6) (sY s - 6) 14 (hsbA 0 0 100 60) (printf "%.0f" (cellFreq (wPitch w) s c) ++ "Hz")
             | (s, c) <- zip (wSites w) (wCells w) ]
  , view [ Color (hsbA 50 90 100 60) (thickPath 2 (wTrail w))
         , dotAt (hsb 50 90 100) (wPen w) 14
         , Color (hsbA 0 0 0 55) (boxAt 0 0 width 84) ]
  , txt 20 30 16 (gray 1) ((if wAudio w then "Playing" else "Click to start sound") ++ "  M: " ++ (if wMouse w then "mouse" else "auto")
      ++ "  1,2: pitch = " ++ (if wPitch w == 1 then "site height" else "cell area") ++ "  Up/Down: speed  Space: drift " ++ onOff (wDrift w) ++ "  Click: add  R: redo")
  , txt 20 52 14 (gray 0.8) "One note each time the yellow cursor crosses a boundary (equal distance from two sites)."
  , txt 20 74 14 (gray 0.8) "Small cells give fast phrases, large cells slow ones. The scale is pentatonic."
  ]
  where
    cellPic s c =
      let g = sGlow s
      in Pictures [ Color (hsb (sHue s) 50 (30 + 60 * g)) (Polygon c)
                  , Color (hsbA 0 0 100 (40 + 50 * g)) (outline (1.5 + 3 * g) c) ]

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> do
    let (x, y) = toP5 pos
    if wAudio w && y > by0 && length (wSites w) < 40
      then do s <- addSite x y
              pure w { wSites = wSites w ++ [s] }
      else pure w { wAudio = True }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 5 (wSpeed w + 0.5), wAudio = True }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 0.5 (wSpeed w - 0.5), wAudio = True }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wDrift = not (wDrift w), wAudio = True }
  EventKey (Char c) Down _ _ -> case toLower c of
    'm' -> pure w { wMouse = not (wMouse w), wAudio = True }
    '1' -> pure w { wPitch = 1, wAudio = True }
    '2' -> pure w { wPitch = 2, wAudio = True }
    'r' -> reset w { wAudio = True }
    _ -> pure w { wAudio = True }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. penta を [0, 2, 3, 7, 8]（和風の音階）や [0, 4, 7]（和音だけ）に変えてみよう
-- 2. カーソルを 2 つにして、2 声のメロディにしよう（2 つ目は別の音色で）
-- 3. 面積モード（2）で、06 のロイド緩和を少しずつかけると、セルの大きさがそろって同じ音ばかりになる？
-- 4. カーソルではなく「サイト同士が近づいたら」鳴らすと、どんな音楽になる？

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- マウスの位置（gloss の座標：中央が原点、y は上向き）を p5 と同じ座標に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は p5 の text と同じ左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y bw bh = rect (x + bw / 2) (y + bh / 2) bw bh

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 直径 d の円（p5 の circle と同じく直径で書く）
dotAt :: Color -> Point -> Float -> Picture
dotAt c p d = Color c (at p (disc (d / 2)))

-- 閉じた多角形の太い枠
outline :: Float -> [Point] -> Picture
outline t ps = thickPath t (ps ++ take 1 ps)

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- ==================================================
-- ボロノイ図の道具（ほかのスケッチにもそのままコピーして使える）
--   voronoiCell sites a x0 y0 x1 y1 … サイト a の領域（多角形）。長方形を垂直二等分線で切っていく（O(n²)）
-- ==================================================
voronoiCell :: [Point] -> Point -> Float -> Float -> Float -> Float -> [Point]
voronoiCell sites a@(ax, ay) x0 y0 x1 y1 = foldl' cut [(x0, y0), (x1, y0), (x1, y1), (x0, y1)] sites
  where
    -- a の側 ＝ (b − a)・p ≦ (|b|² − |a|²) / 2　（a と b の垂直二等分線で切る）
    cut poly b@(bx, by)
      | b == a || length poly <= 2 = poly
      | otherwise = clipHalfPlane poly (bx - ax) (by - ay) ((bx * bx + by * by - ax * ax - ay * ay) / 2)

-- nx·x + ny·y ≦ c の側だけ残す（Sutherland–Hodgman の 1 本分）
clipHalfPlane :: [Point] -> Float -> Float -> Float -> [Point]
clipHalfPlane poly nx ny c = concat (zipWith edge poly (drop 1 poly ++ take 1 poly))
  where
    side (x, y) = nx * x + ny * y - c
    edge p@(px, py) q@(qx, qy) =
      let dp = side p
          dq = side q
          t = dp / (dp - dq)
      in [p | dp <= 0] ++ [(px + (qx - px) * t, py + (qy - py) * t) | (dp < 0 && dq > 0) || (dp > 0 && dq < 0)]

-- 多角形の面積（Float の桁落ちを防ぐため、最初の頂点からの相対座標で計算する）
polyArea :: [Point] -> Float
polyArea [] = 0
polyArea poly@((ox, oy) : _) = abs (sum (zipWith cr poly (drop 1 poly ++ take 1 poly))) / 2
  where cr (px, py) (qx, qy) = (px - ox) * (qy - oy) - (qx - ox) * (py - oy)
