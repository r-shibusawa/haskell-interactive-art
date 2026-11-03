-- ============================================
-- 07 グリッド上の A* 経路計画
-- 地図をマス目にして「スタートからゴールまでの最短の道」をグラフ探索で見つける
--   f = g（ここまでの実コスト）+ w × h（ゴールまでの見込み距離）。w = 0 ならダイクストラ、w = 1 で A*、w > 1 は速いが最短とは限らない
-- ドラッグ = 壁を描く / E = 消しゴム / S・G = マウスの位置にスタート・ゴール / スペース = 探索 / ↑↓ = w / D = ダイクストラと切りかえ / 8 = ななめ / C = 全部消す / R = ランダムな壁
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (delete, foldl')
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

cols, rows, nCells :: Int
cols = 44                 -- ★ マス目の数
rows = 26
nCells = cols * rows

cell, ox, oy :: Float
cell = min ((width - 40) / fromIntegral cols) ((height - 120) / fromIntegral rows)
ox = 20
oy = 100

stepsPerFrame :: Int
stepsPerFrame = 4         -- ★ 1 フレームに進める回数

data World = World
  { wWall    :: !(U.Vector Bool)
  , wStart   :: !Int
  , wGoal    :: !Int
  , wOpen    :: ![Int]                -- 候補（open）
  , wState   :: !(U.Vector Int)       -- 0 = 未訪問, 1 = 候補（open）, 2 = 調べた（closed）, 3 = 道
  , wG       :: !(U.Vector Float)     -- ここまでの実コスト
  , wF       :: !(U.Vector Float)     -- f = g + w × h
  , wFrom    :: !(U.Vector Int)       -- どのマスから来たか
  , wPath    :: ![Int]
  , wRunning :: !Bool
  , wDone    :: !Bool
  , wWeight  :: !Float                -- ★ ヒューリスティックの重み（0 = ダイクストラ）
  , wErase   :: !Bool
  , wDiag    :: !Bool                 -- ★ ななめ移動を許すか
  , wExpand  :: !Int
  }

main :: IO ()
main = runSketch (sketch "21-07 A* on a Grid" world0)
  { skBack  = hsb 215 35 12
  , skStart = \_ -> randomWalls world0
  , skStep  = \_ _ w -> pure (if wRunning w then iterate stepAStar w !! stepsPerFrame else w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \env ev w -> onEvent env ev w
  }

world0 :: World
world0 = World
  { wWall = U.replicate nCells False
  , wStart = 2 + 3 * cols, wGoal = (cols - 3) + (rows - 4) * cols
  , wOpen = [], wState = U.replicate nCells 0
  , wG = U.replicate nCells inf, wF = U.replicate nCells inf, wFrom = U.replicate nCells (-1)
  , wPath = [], wRunning = False, wDone = False, wWeight = 1, wErase = False, wDiag = False, wExpand = 0 }

inf :: Float
inf = 1 / 0

randomWalls :: World -> IO World
randomWalls w = do
  rs <- replicateM nCells (randomRIO (0, 1 :: Float))
  let wall = U.fromList (map (< 0.3) rs) U.// [(wStart w, False), (wGoal w, False)]
  pure (resetSearch w { wWall = wall })

resetSearch :: World -> World
resetSearch w = w { wOpen = [], wPath = [], wRunning = False, wDone = False, wExpand = 0, wState = U.replicate nCells 0 }

-- ★ 見込み距離（ヒューリスティック）：ななめなしならマンハッタン、ありならオクタイル距離
heuristic :: World -> Int -> Float
heuristic w i =
  let dx = fromIntegral (abs (i `mod` cols - wGoal w `mod` cols))
      dy = fromIntegral (abs (i `div` cols - wGoal w `div` cols))
  in if wDiag w then max dx dy + (sqrt 2 - 1) * min dx dy else dx + dy

startSearch :: World -> World
startSearch w0 =
  let w = resetSearch w0
      s = wStart w
  in w { wG = U.replicate nCells inf U.// [(s, 0)]
       , wF = U.replicate nCells inf U.// [(s, wWeight w * heuristic w s)]
       , wFrom = U.replicate nCells (-1)
       , wOpen = [s], wState = wState w U.// [(s, 1)], wRunning = True }

-- ★ A* の 1 ステップ：f が最小の候補を取り出して、隣を調べる
stepAStar :: World -> World
stepAStar w = case wOpen w of
  _ | not (wRunning w) -> w
  [] -> w { wRunning = False, wDone = True }
  (o : os) ->
    let fOf i = wF w U.! i
        cur = foldl' (\b k -> if fOf k < fOf b then k else b) o os
        w1 = w { wOpen = delete cur (wOpen w), wState = wState w U.// [(cur, 2)], wExpand = wExpand w + 1 }
    in if cur == wGoal w then finish cur w1 else foldl' (visit cur) w1 (moves w)

-- ゴールに着いたら、来た道を逆にたどる
finish :: Int -> World -> World
finish cur w =
  let back v | v < 0 = []
             | otherwise = v : back (wFrom w U.! v)
      path = reverse (take nCells (back cur))
  in w { wPath = path, wState = wState w U.// [(v, 3) | v <- path], wRunning = False, wDone = True }

moves :: World -> [(Int, Int, Float)]
moves w = [(1, 0, 1), (-1, 0, 1), (0, 1, 1), (0, -1, 1)]
  ++ (if wDiag w then [(1, 1, sqrt 2), (1, -1, sqrt 2), (-1, 1, sqrt 2), (-1, -1, sqrt 2)] else [])

-- 隣のマスを調べる
visit :: Int -> World -> (Int, Int, Float) -> World
visit cur w (dx, dy, c)
  | nx < 0 || ny < 0 || nx >= cols || ny >= rows = w
  | wWall w U.! nb || wState w U.! nb == 2 = w
  | dx /= 0 && dy /= 0 && (wWall w U.! (x + dx + y * cols) || wWall w U.! (x + (y + dy) * cols)) = w   -- 角をすり抜けない
  | g < wG w U.! nb =
      let isOpen = wState w U.! nb == 1
      in w { wG = wG w U.// [(nb, g)]
           , wF = wF w U.// [(nb, g + wWeight w * heuristic w nb)]
           , wFrom = wFrom w U.// [(nb, cur)]
           , wOpen = if isOpen then wOpen w else wOpen w ++ [nb]
           , wState = if isOpen then wState w else wState w U.// [(nb, 1)] }
  | otherwise = w
  where
    x = cur `mod` cols
    y = cur `div` cols
    nx = x + dx
    ny = y + dy
    nb = nx + ny * cols
    g = wG w U.! cur + c

cellAt :: Point -> Int
cellAt (mx, my) =
  let cx = floor ((mx - ox) / cell)
      cy = floor ((my - oy) / cell)
  in if cx < 0 || cy < 0 || cx >= cols || cy >= rows then -1 else cx + cy * cols

cellPos :: Int -> Point
cellPos i = (ox + fromIntegral (i `mod` cols) * cell, oy + fromIntegral (i `div` cols) * cell)

cellColor :: World -> Int -> Color
cellColor w i
  | wWall w U.! i = hsb 0 0 60
  | otherwise = case wState w U.! i of
      3 -> hsb 45 90 100
      2 -> hsb 200 60 55
      1 -> hsb 120 70 80
      _ -> hsb 0 0 18

drawAll :: World -> Picture
drawAll w =
  let square i = let (x, y) = cellPos i
                 in Color (cellColor w i) (boxAt (x + 0.5) (y + 0.5) (cell - 1) (cell - 1))
      mid i = let (x, y) = cellPos i in (x + cell / 2, y + cell / 2)
      sp = mid (wStart w)
      gp = mid (wGoal w)
      letter (x, y) s = labelC (x - width / 2) (height / 2 - y) (cell * 0.6) (hsb 0 0 10) s
      wt = wWeight w
      kind | wt == 0 = " = Dijkstra"
           | wt == 1 = " = A*"
           | otherwise = " = weighted A*"
      pathLen | null (wPath w) = "-"
              | otherwise = nf 1 (wG w U.! wGoal w)
  in Pictures
  [ view
      ( map square [0 .. nCells - 1]
        ++ [ Color (hsb 170 80 100) (at sp (disc (cell * 0.45)))
           , Color (hsb 0 90 100) (at gp (disc (cell * 0.45))) ] )
  , letter sp "S"
  , letter gp "G"
  , txt 20 26 16 (hsb 0 0 100) ("Space: search   w = " ++ nf 1 wt ++ " (Up/Down)" ++ kind ++ "   D: Dijkstra/A*   8: diagonal " ++ onOff (wDiag w))
  , txt 20 48 14 (hsb 0 0 100) ("Drag: walls   E: " ++ (if wErase w then "eraser" else "pen") ++ "   S, G: start, goal at mouse   C: clear   R: random")
  , txt 20 70 14 (hsb 0 0 60) ("Expanded (blue) " ++ show (wExpand w) ++ "   Open (green) " ++ show (length (wOpen w)) ++ "   Path length " ++ pathLen
                               ++ (if wDone w && null (wPath w) then "   No path!" else "")
                               ++ "   Higher w: less blue, path may not be shortest")
  ]

paint :: Point -> World -> World
paint m w
  | i < 0 || i == wStart w || i == wGoal w = w
  | wDone w || wRunning w = resetSearch w1
  | otherwise = w1
  where
    i = cellAt m
    w1 = w { wWall = wWall w U.// [(i, not (wErase w))] }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> pure (paint (toP5 pos) w)
  EventMotion pos | envDown env -> pure (paint (toP5 pos) w)
  EventKey (Char c) Down _ _ -> case toLower c of
    'e' -> pure w { wErase = not (wErase w) }
    'c' -> pure (resetSearch w { wWall = U.replicate nCells False })
    'r' -> randomWalls w
    'd' -> pure (research w { wWeight = if wWeight w == 0 then 1 else 0 })
    '8' -> pure (resetSearch w { wDiag = not (wDiag w) })
    's' -> pure (placeAt (\i -> w { wStart = i }))
    'g' -> pure (placeAt (\i -> w { wGoal = i }))
    _ -> pure w
  EventKey (SpecialKey k) Down _ _ -> case k of
    KeySpace -> pure (startSearch w)
    KeyUp -> pure (research w { wWeight = min 5 (wWeight w + 0.5) })
    KeyDown -> pure (research w { wWeight = max 0 (wWeight w - 0.5) })
    _ -> pure w
  _ -> pure w
  where
    research x = if wDone x then startSearch x else x
    placeAt f =
      let i = cellAt (toP5 (envMouse env))
      in if i >= 0 && not (wWall w U.! i) then resetSearch (f i) else w

-- ---------- やってみよう ----------
-- 1. 同じ地図で w = 0（ダイクストラ）と w = 1（A*）の「調べたマス」の数を比べよう。道の長さは同じ？
-- 2. w = 3 にすると道の長さが変わることがある。そうなる地図を描いてみよう
-- 3. ゴールを囲む壁を描いて「道がない」を出す。A* はいつあきらめる？
-- 4. ななめ移動 ON で道がどう変わるか。斜めのコストを 1 にする（ずる）とどうなる？


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

-- 小数点以下 d 桁の文字列（p5 の nf）
nf :: Int -> Float -> String
nf d = printf ("%." ++ show d ++ "f")

onOff :: Bool -> String
onOff b = if b then "ON" else "off"
