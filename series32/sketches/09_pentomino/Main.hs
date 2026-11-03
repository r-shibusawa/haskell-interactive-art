-- ============================================
-- 09 ポリオミノとパズル（ペントミノ）
-- 正方形 5 つをつないだ形は 12 種類（ペントミノ）。全部で 60 マス → 6×10 などの長方形にぴったり詰められる
--   コンピュータの解き方 = バックトラック：左の空きマスに入る形を順に試し、行き詰まったら 1 つ戻る
-- 1 = 6×10 2 = 5×12 3 = 4×15 4 = 3×20 / スペース = 一時停止 / ↑↓ = 速さ / N = 次の解 / R = 順番を変えて最初から / F = 一気に / S = 保存
-- ============================================
module Main (main) where

import Art
import Data.Bits (clearBit, setBit, testBit)
import Data.List (foldl', nub, sort)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Clock (getMonotonicTime)

-- 探索の 1 段：空きマス (fx, fy)、order の何番目か k、向きの番号 o
data Frame = Frame !Int !Int !Int !Int

data World = World
  { wBi     :: !Int              -- ★ 盤の番号
  , wW, wH  :: !Int
  , wSpeed  :: !Int
  , wPaused :: !Bool
  , wOrder  :: !(U.Vector Int)   -- 試す順番
  , wBoard  :: !(U.Vector Int)   -- 各マスのピース（−1 = 空き）
  , wUsed   :: !Int              -- 使ったピース（ビットで記録）
  , wStack  :: [Frame]           -- 先頭が最新
  , wNeedNew, wSolved, wFailed :: !Bool
  , wTries, wSols :: !Int
  , wTurbo  :: !Double           -- F：この時刻まで全力で探す
  }

boards :: [(Int, Int)]
boards = [(10, 6), (12, 5), (15, 4), (20, 3)]

speeds :: [Int]
speeds = [1, 3, 10, 40, 200, 1000]   -- ★ 1 フレームに試す手の数

world0 :: World
world0 = restart (World 0 10 6 2 False (U.enumFromN 0 12) U.empty 0 [] True False False 0 0 0)

main :: IO ()
main = runSketch (sketch "32-09 Pentomino Puzzle" world0)
  { skBack  = hsb 230 25 12
  , skStep  = \_ _ w -> stepWorld w
  , skDraw  = drawAll
  , skEvent = onEvent
  }

shapes :: [(String, [String])]
shapes =
  [ ("F", [".##", "##.", ".#."]), ("I", ["#####"]), ("L", ["####", "#..."]), ("N", ["###.", "..##"])
  , ("P", ["###", "##."]), ("T", ["###", ".#.", ".#."]), ("U", ["#.#", "###"]), ("V", ["#..", "#..", "###"])
  , ("W", ["#..", "##.", ".##"]), ("X", [".#.", "###", ".#."]), ("Y", ["####", ".#.."]), ("Z", ["##.", ".#.", ".##"]) ]

-- ORI ! p = 向き（回転・裏返し）のリスト。各向き = マスのずれ [(dx, dy), …]
ori :: V.Vector (V.Vector [(Int, Int)])
ori = V.fromList (map (V.fromList . orientations . snd) shapes)

-- ★ 12 色（となりのピースが見分けやすい順）
pieceCol :: Int -> Color
pieceCol i = case drop i [0, 200, 45, 280, 120, 25, 330, 170, 60, 230, 95, 300] of
  (h : _) -> hsb h (if odd i then 50 else 70) (if i `mod` 3 == 0 then 95 else 80)
  [] -> white

restart :: World -> World
restart w =
  let (bw, bh) = case drop (wBi w) boards of
        (b : _) -> b
        [] -> (10, 6)
  in w { wW = bw, wH = bh, wBoard = U.replicate (bw * bh) (-1), wUsed = 0, wStack = []
       , wNeedNew = True, wSolved = False, wFailed = False, wTries = 0, wSols = 0, wTurbo = 0 }

running :: World -> Bool
running w = not (wSolved w) && not (wFailed w)

stepWorld :: World -> IO World
stepWorld w
  | wTurbo w > 0 = do
      t <- getMonotonicTime
      if t > wTurbo w || not (running w) then pure w { wTurbo = 0 } else turbo (t + 0.02) w
  | wPaused w = pure w
  | otherwise = pure (steps (speedOf w) w)

speedOf :: World -> Int
speedOf w = case drop (wSpeed w) speeds of
  (s : _) -> s
  [] -> 1

steps :: Int -> World -> World
steps k w
  | k <= 0 || not (running w) = w
  | otherwise = steps (k - 1) (stepDFS w)

-- 画面が止まらないよう、1 フレームに 20 ミリ秒まで探す
turbo :: Double -> World -> IO World
turbo tEnd w
  | not (running w) = pure w
  | otherwise = do
      t <- getMonotonicTime
      if t > tEnd then pure w else turbo tEnd (steps 200 w)

-- ★ バックトラックの 1 手
stepDFS :: World -> World
stepDFS w0
  | wNeedNew w0 = case firstEmpty w0 of
      Nothing -> w0 { wSolved = True, wSols = wSols w0 + 1 }
      Just (fx, fy) -> tryTop w0 { wStack = Frame fx fy 0 (-1) : wStack w0, wNeedNew = False }
  | otherwise = tryTop w0

tryTop :: World -> World
tryTop w = case wStack w of
  [] -> w { wFailed = True }
  (fr : rest) ->
    let w1 = removeTop fr w                        -- 前に試したピースを外す
    in case advance w1 fr of
         Just fr'@(Frame _ _ k _) ->
           putPiece fr' w1 { wStack = fr' : rest, wNeedNew = True, wTries = wTries w1 + 1 }
         Nothing -> w1 { wStack = rest, wFailed = null rest }

-- 次に置けるピースと向きを探す（見つかれば Just）
advance :: World -> Frame -> Maybe Frame
advance w (Frame fx fy k0 o0) = goK k0 o0
  where
    goK k o
      | k >= 12 = Nothing
      | testBit (wUsed w) p = goK (k + 1) (-1)
      | otherwise = goO (o + 1)
      where
        p = wOrder w U.! k
        os = ori V.! p
        goO i
          | i >= V.length os = goK (k + 1) (-1)
          | fits w (os V.! i) fx fy = Just (Frame fx fy k i)
          | otherwise = goO (i + 1)

fits :: World -> [(Int, Int)] -> Int -> Int -> Bool
fits w o fx fy = all ok o
  where
    ok (dx, dy) =
      let x = fx + dx
          y = fy + dy
      in x >= 0 && y >= 0 && x < wW w && y < wH w && U.unsafeIndex (wBoard w) (x * wH w + y) < 0

putPiece :: Frame -> World -> World
putPiece (Frame fx fy k o) w =
  let p = wOrder w U.! k
      cells = [ ((fx + dx) * wH w + fy + dy, p) | (dx, dy) <- ori V.! p V.! o ]
  in w { wBoard = wBoard w U.// cells, wUsed = setBit (wUsed w) p }

removeTop :: Frame -> World -> World
removeTop (Frame fx fy k o) w
  | o < 0 = w
  | otherwise =
      let p = wOrder w U.! k
          cells = [ ((fx + dx) * wH w + fy + dy, -1) | (dx, dy) <- ori V.! p V.! o ]
      in w { wBoard = wBoard w U.// cells, wUsed = clearBit (wUsed w) p }

-- 左の列から順に、上から見て最初の空きマス
firstEmpty :: World -> Maybe (Int, Int)
firstEmpty w = fmap (\i -> (i `div` wH w, i `mod` wH w)) (U.findIndex (< 0) (wBoard w))

drawAll :: Env -> World -> Picture
drawAll env w =
  let ww = envW env
      h = envH env
      bw = wW w
      bh = wH w
      cs = min ((ww - 80) / fromIntegral bw) ((h - 260) / fromIntegral bh)
      ox = (ww - cs * fromIntegral bw) / 2
      oy = 130
      fx x = ox + fromIntegral x * cs
      fy y = oy + fromIntegral y * cs
      at' x y = wBoard w U.! (x * bh + y)
      cells = [ Pictures
                  [ Color (if p >= 0 then pieceCol p else hsb 230 15 22) (box (fx x) (fy y) cs cs)
                  , Color (hsb 230 20 25) (rectLine (fx x + cs / 2) (fy y + cs / 2) cs cs) ]
              | x <- [0 .. bw - 1], y <- [0 .. bh - 1], let p = at' x y ]
      -- 同じピースの境目は消して、ピースの形を見やすく
      borders = Color (hsb 230 30 8) (Pictures (concat
        [ (if x < bw - 1 && at' (x + 1) y /= p then [thickLine 4 (fx (x + 1), fy y) (fx (x + 1), fy (y + 1))] else [])
          ++ (if y < bh - 1 && at' x (y + 1) /= p then [thickLine 4 (fx x, fy (y + 1)) (fx (x + 1), fy (y + 1))] else [])
        | x <- [0 .. bw - 1], y <- [0 .. bh - 1], let p = at' x y ]))
      -- 12 種類のピース
      px i = 40 + fromIntegral i * ((ww - 80) / 12)
      py = h - 100
      c = 11
      samples = [ Color (if testBit (wUsed w) i then hsb 0 0 35 else pieceCol i)
                    (box (px i + fromIntegral dx * c) (py + fromIntegral dy * c) (c - 1) (c - 1))
                | (i, (_, rows)) <- zip [0 ..] shapes
                , (dy, row) <- zip [0 :: Int ..] rows
                , (dx, ch) <- zip [0 :: Int ..] row, ch == '#' ]
      names = [ txt env (px i) (py + 70) 14 (hsb 0 0 80) nm | (i, (nm, _)) <- zip [0 :: Int ..] shapes ]
      state | wSolved w = "Solved! (N: next)"
            | wFailed w = "No more solutions"
            | wPaused w = "Paused"
            | otherwise = "Searching..."
  in Pictures
    [ onP5 env (Pictures (cells ++ [borders] ++ samples))
    , Pictures names
    , txt env 20 36 20 white ("Fill " ++ show bh ++ " x " ++ show bw ++ " with the 12 pentominoes (1-4: board)")
    , txt env 20 62 15 (hsb 0 0 75) (state ++ "   tries " ++ show (wTries w) ++ "   solutions " ++ show (wSols w) ++ "   speed " ++ show (speedOf w) ++ " moves/frame (Up/Down)")
    , txt env 20 86 15 (hsb 0 0 75) "Fill the leftmost empty cell with an unused piece; if stuck, go back one (backtracking)"
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min (length speeds - 1) (wSpeed w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 0 (wSpeed w - 1) }
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> pure (restart w { wBi = fromEnum c - 49 })
    | c == 'n' || c == 'N' -> pure (if wSolved w then w { wSolved = False, wNeedNew = False } else w)
    | c == 'r' || c == 'R' -> do
        o <- shuffle (U.toList (wOrder w))
        pure (restart w { wOrder = U.fromList o })
    | c == 'f' || c == 'F' -> do
        t <- getMonotonicTime
        pure w { wTurbo = t + 2 }
    | c == 's' || c == 'S' -> saveSvg "pentomino" env (drawAll env w) >> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 6×10 は 2339 通り、3×20 はたった 2 通りの解がある。3×20 は手数がずっと多いのはなぜ？
-- 2. R で試す順番を変えると、見つかる解も手数も変わる。いちばん少ない手数は？
-- 3. N で次々に解を探そう。裏返しや回転で同じになる解は 4 つずつ見つかる
-- 4. shapes に形を足して（例：テトロミノ 5 種類 = 20 マス）、4×5 の盤を作ってみよう


-- ==================================================
-- ここから下は「道具」（座標、シャッフル、回転・裏返しで向きを全部作る）
-- ==================================================
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

shuffle :: [a] -> IO [a]
shuffle [] = pure []
shuffle l = do
  i <- randomRIO (0, length l - 1)
  case splitAt i l of
    (a, x : b) -> (x :) <$> shuffle (a ++ b)
    (a, []) -> pure a

orientations :: [String] -> [[(Int, Int)]]
orientations rows =
  let cells = [ (x, y) | (y, r) <- zip [0 ..] rows, (x, ch) <- zip [0 ..] r, ch == '#' ]
      turn = map (\(x, y) -> (y, negate x))
      flipX = map (\(x, y) -> (negate x, y))
      rots c = take 4 (drop 1 (iterate turn c))
      normal c =
        let mx = minimum' (map fst c)
            my = minimum' (map snd c)
        in sort [ (x - mx, y - my) | (x, y) <- c ]
      -- 「左の列のいちばん上のマス」が (0, 0) になるようにずらす
      anchor o = case o of
        ((ax, ay) : _) -> [ (x - ax, y - ay) | (x, y) <- o ]
        [] -> []
  in map anchor (nub (map normal (rots cells ++ rots (flipX cells))))
  where minimum' = foldl' min 1000
