-- ============================================
-- 11 トルシェ・タイルとランダム・タイル
-- 1 種類のタイルを「向き」だけ変えて並べる。向きをランダムにすると、迷路や流れる曲線が現れる
--   トルシェ（1704 年、三角形）、スミス（1987 年、4 分の 1 円）、斜め線 1 本（10 PRINT の迷路）
-- 1〜4 = タイルの種類 / クリック = その 1 枚を回す / スペース = ランダム / R = 規則で並べる / A = 自動で回る / C = 塗り分け / ↑↓ = 大きさ / S = 保存
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import qualified Data.Vector.Unboxed as U

data World = World
  { wKind    :: !Int      -- ★ 1 = 4 分の 1 円、2 = 斜め線（迷路）、3 = 三角形、4 = 波線
  , wCell    :: !Float    -- ★ タイルの大きさ
  , wAuto    :: !Bool
  , wColored :: !Bool
  , wRule    :: !Int      -- 0 = ランダム、1〜4 = 規則
  , wCols, wRows :: !Int
  , wOri     :: !(U.Vector Int)     -- 向き（0〜3）
  , wTurn    :: !(U.Vector Float)   -- 回転アニメーションの残り（ラジアン）
  }

world0 :: World
world0 = World 1 60 False True 0 0 0 U.empty U.empty

main :: IO ()
main = runSketch (sketch "32-11 Truchet Tiles" world0)
  { skBack  = colB
  , skStart = \env -> layout env world0
  , skStep  = stepWorld
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- ★ 色
colA, colB, lineCol :: Color
colA = rgb255 245 235 215
colB = rgb255 40 60 100
lineCol = rgb255 230 90 70

layout :: Env -> World -> IO World
layout env w = do
  let cols = ceiling (envW env / wCell w)
      rows = ceiling ((envH env - 90) / wCell w)
  o <- U.replicateM (cols * rows) (randomRIO (0, 3))
  pure w { wCols = cols, wRows = rows, wOri = o, wTurn = U.replicate (cols * rows) 0 }

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  w <- if wAuto w0 && envFrame env `mod` 3 == 0 && U.length (wOri w0) > 0
         then do
           k <- randomRIO (0, U.length (wOri w0) - 1)
           pure (spinTile k w0)
         else pure w0
  -- 回転アニメーションをだんだん止める
  pure w { wTurn = U.map (\t -> let v = t * 0.82 in if abs v < 0.01 then 0 else v) (wTurn w) }

drawAll :: Env -> World -> Picture
drawAll env w =
  let cell = wCell w
      tiles = [ Translate (fromIntegral i * cell + cell / 2) (90 + fromIntegral j * cell + cell / 2)
                  (rotP (negate (wTurn w U.! k)) (drawTile w (wOri w U.! k) i j))
              | j <- [0 .. wRows w - 1], i <- [0 .. wCols w - 1], let k = j * wCols w + i ]
      nm = case wKind w of
        1 -> "Quarter circles (Smith)"
        2 -> "One diagonal (maze)"
        3 -> "Triangles (Truchet)"
        _ -> "Wavy lines"
      ruleName = case wRule w of
        0 -> "random"
        1 -> "checker"
        2 -> "columns"
        3 -> "concentric"
        _ -> "noise"
      onOff b = if b then "ON" else "OFF"
  in Pictures
    [ onP5 env (Pictures (tiles ++ [Color (rgba255 0 0 0 190) (box 0 0 (envW env) 90)]))
    , txt env 20 36 20 white (nm ++ " (1-4)   Layout: " ++ ruleName ++ " (Space / R)")
    , txt env 20 66 15 (gray 0.78)
        ("Click turns a tile.  A auto " ++ onOff (wAuto w) ++ "  C colors " ++ onOff (wColored w) ++ "  Lines meet at edge midpoints, so any layout connects")
    ]

-- ★ タイル 1 枚（中心が原点）
drawTile :: World -> Int -> Int -> Int -> Picture
drawTile w o i j =
  let cell = wCell w
      h = cell / 2
      colored = wColored w
      square c = Color c (rectangleSolid cell cell)
  in rotP (fromIntegral o * pi / 2) $ case wKind w of
    1 ->
      -- 2 つの角を中心に 4 分の 1 円。向き 0 と 2、1 と 3 は同じ形
      -- 塗り分け：角 (格子点) の偶奇で色を決めると、つながった領域が交互の色になる
      let isEven = even (i + j + o `mod` 2)
          cap = if colored && isEven then colA else colB
          band = if colored && not isEven then colA else colB
      in Pictures
        [ square band
        , Color cap (Pictures [Polygon (pie (-h) (-h) h 0 90), Polygon (pie h h h 180 270)])
        , Color lineCol (Pictures
            [ at (-h, -h) (ThickArc 0 90 h (cell * 0.08))
            , at (h, h) (ThickArc 180 270 h (cell * 0.08)) ])
        ]
    2 -> Pictures [square colB, Color colA (thickLine (cell * 0.12) (-h, -h) (h, h))]
    3 -> Pictures [square colA, Color (if colored then lineCol else colB) (Polygon [(-h, -h), (h, -h), (-h, h)])]
    _ ->
      -- 上の辺の真ん中 → 右の辺の真ん中、下の辺の真ん中 → 左の辺の真ん中を S 字で
      let segs = if cell < 30 then 3 else 8
      in Pictures
        [ square colB
        , Color colA (Pictures
            [ thickPath (cell * 0.1) (bezier segs (0, -h) (0, -h * 0.2) (h * 0.2, 0) (h, 0))
            , thickPath (cell * 0.1) (bezier segs (0, h) (0, h * 0.2) (-h * 0.2, 0) (-h, 0)) ])
        ]

-- ★ 並べ方の規則
arrange :: World -> IO World
arrange w = do
  let cols = wCols w
      rows = wRows w
      visit acc@(ori, turn) k = do
        let i = k `mod` cols
            j = k `div` cols
            halfC = fromIntegral cols / 2 :: Float
            halfR = fromIntegral rows / 2 :: Float
        o <- case wRule w of
          0 -> randomRIO (0, 3)
          1 -> pure ((i + j) `mod` 2)
          2 -> pure (i `mod` 4)
          3 -> let d = floor (max (abs (fromIntegral i - halfC)) (abs (fromIntegral j - halfR))) :: Int
                   side = (fromIntegral i < halfC) /= (fromIntegral j < halfR)
               in pure (d `mod` 2 + (if side then 2 else 0))
          _ -> pure (floor (noise2 (fromIntegral i * 0.15) (fromIntegral j * 0.15) * 8) `mod` 4)
        let cur = wOri w U.! k
        pure (if o /= cur
                then ((k, o) : ori, (k, wTurn w U.! k + pi / 2 * fromIntegral ((o - cur + 4) `mod` 4)) : turn)
                else acc)
  (ori, turn) <- foldM visit ([], []) [0 .. cols * rows - 1]
  pure w { wOri = wOri w U.// ori, wTurn = wTurn w U.// turn }

spinTile :: Int -> World -> World
spinTile k w =
  w { wOri = wOri w U.// [(k, (wOri w U.! k + 1) `mod` 4)]
    , wTurn = wTurn w U.// [(k, wTurn w U.! k + pi / 2)] }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ (px, py) ->
    let x = px + envW env / 2
        y = envH env / 2 - py
        i = floor (x / wCell w)
        j = floor ((y - 90) / wCell w)
    in pure (if i >= 0 && j >= 0 && i < wCols w && j < wRows w then spinTile (j * wCols w + i) w else w)
  EventKey (SpecialKey KeySpace) Down _ _ -> arrange w { wRule = 0 }
  EventKey (SpecialKey KeyUp) Down _ _ -> layout env w { wCell = min 160 (wCell w + 10) }
  EventKey (SpecialKey KeyDown) Down _ _ -> layout env w { wCell = max 16 (wCell w - 10) }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> pure w { wKind = fromEnum c - 48 }
    | c == 'r' || c == 'R' -> arrange w { wRule = wRule w `mod` 4 + 1 }
    | c == 'a' || c == 'A' -> pure w { wAuto = not (wAuto w) }
    | c == 'c' || c == 'C' -> pure w { wColored = not (wColored w) }
    | c == 's' || c == 'S' -> saveSvg "truchet" env (drawAll env w) >> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 2（斜め線）を小さく（↓）すると迷路になる。入口から出口まで行ける？
-- 2. 1 で C を ON。どう回しても 2 色で塗り分けられるのはなぜ？（格子点の偶奇がヒント）
-- 3. R の規則を自分で作ろう（arrange の wRule == 2 の行を、i * j `mod` 4 などに）
-- 4. A で自動回転にして、ゆっくり変わる「動く壁紙」を作ろう


-- ==================================================
-- ここから下は「道具」
-- ==================================================
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

-- p5.js の rotate(a) と同じ向き（y 下向きの座標の中で使う）。ラジアンで指定
rotP :: Float -> Picture -> Picture
rotP a = Rotate (negate a * 180 / pi)

-- 中心 (x, y)、半径 r の扇形の点（角度は度）
pie :: Float -> Float -> Float -> Float -> Float -> [Point]
pie x y r a0 a1 =
  (x, y) : [ (x + r * cos t, y + r * sin t) | i <- [0 .. 10 :: Int], let t = (a0 + (a1 - a0) * fromIntegral i / 10) * pi / 180 ]

-- 3 次ベジェ曲線の点
bezier :: Int -> Point -> Point -> Point -> Point -> [Point]
bezier n (ax, ay) (bx, by) (cx, cy) (dx, dy) =
  [ (f ax bx cx dx t, f ay by cy dy t) | i <- [0 .. n], let t = fromIntegral i / fromIntegral n ]
  where f a b c d t = let u = 1 - t in u * u * u * a + 3 * u * u * t * b + 3 * u * t * t * c + t * t * t * d
