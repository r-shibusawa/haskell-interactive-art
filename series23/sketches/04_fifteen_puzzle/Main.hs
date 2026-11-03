-- ============================================
-- 04 15 パズルと偶置換
-- 1 回スライドする = 空白とタイルの入れかえ（互換）1 回 → 置換の偶奇が変わり、空白の位置も 1 マス動く
--   「置換の偶奇」と「空白のゴールからの距離の偶奇」はいつも一致 → 14 と 15 だけ入れかえた状態は絶対に解けない
-- タイルをクリック or ←→↑↓ = スライド / スペース = シャッフル（正しい手だけ）/ W = 14 と 15 をこっそり入れかえ / 0 = リセット
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

boardN :: Int
boardN = 4               -- ★ 盤の大きさ（3 にすると 8 パズル）

data World = World
  { wBoard :: ![Int]     -- board[マス] = タイル番号（0 = 空白）
  , wMoves :: !Int
  , wSlide :: !(Maybe (Int, Int, Float))   -- アニメーション（タイル、元のマス、進み具合）
  , wCheat :: !Bool
  }

fresh :: World
fresh = World [(i + 1) `mod` (boardN * boardN) | i <- [0 .. boardN * boardN - 1]] 0 Nothing False

main :: IO ()
main = runSketch (sketch "23-04 Fifteen Puzzle" fresh)
  { skBack  = rgb255 30 34 44
  , skStep  = \_ _ w -> pure (stepSlide w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = onEvent
  }

stepSlide :: World -> World
stepSlide w = case wSlide w of
  Just (a, b, t) | t + 0.2 >= 1 -> w { wSlide = Nothing }
                 | otherwise -> w { wSlide = Just (a, b, t + 0.2) }
  Nothing -> w

cs, ox, oy :: Float
cs = min (width * 0.42) (height - 220) / fromIntegral boardN
ox = 60
oy = 120

blankAt :: [Int] -> Int
blankAt b = fromMaybe 0 (elemIndex 0 b)

drawWorld :: World -> Picture
drawWorld w = Pictures (frame : tiles ++ hud)
  where
    n = boardN
    fn = fromIntegral n :: Float
    frame = box (ox - 10) (oy - 10) (cs * fn + 20) (cs * fn + 20) (gray (20 / 255))
    cellX i = ox + fromIntegral (i `mod` n) * cs
    cellY i = oy + fromIntegral (i `div` n) * cs
    tiles = concat
      [ [ box (x + 4) (y + 4) (cs - 8) (cs - 8) col
        , txtC (x + cs / 2) (y + cs / 2) (cs * 0.4) (gray (30 / 255)) (show t) ]
      | (i, t) <- zip [0 ..] (wBoard w), t /= 0
      , let (x, y) = case wSlide w of
              Just (st, from, a) | st == t ->
                let e = a * a * (3 - 2 * a)
                in (lerpF (cellX from) (cellX i) e, lerpF (cellY from) (cellY i) e)
              _ -> (cellX i, cellY i)
            col | wCheat w && (t == n * n - 1 || t == n * n - 2) = rgb255 255 150 150
                | t - 1 == i = rgb255 250 220 140
                | otherwise = rgb255 240 240 245 ]
    -- 2 つの偶奇
    par = permParity (wBoard w)
    bi = blankAt (wBoard w)
    distB = (n - 1 - bi `mod` n) + (n - 1 - bi `div` n)   -- 空白からゴール（右下）までの距離
    solvable = par == distB `mod` 2
    px = ox + cs * fn + 60
    py = 130 :: Float
    blue' = rgb255 130 200 255
    parName v = if v /= (0 :: Int) then "odd" else "even"
    hud =
      [ txt px py 18 white ("Permutation parity (blank = tile " ++ show (n * n) ++ ")")
      , txt px (py + 50) 40 blue' (parName par)
      , txt px (py + 110) 18 white ("Blank's distance from bottom-right = " ++ show distB ++ " ->")
      , txt px (py + 160) 40 blue' (parName (distB `mod` 2))
      , txt px (py + 230) 30 (if solvable then rgb255 120 230 160 else rgb255 255 110 110)
          (if solvable then "Match -> solvable" else "Mismatch -> NEVER solvable!")
      , txt px (py + 280) 14 (gray 0.78) "1 move = 1 swap with blank: parity and distance both flip"
      , txt px (py + 306) 14 (gray 0.78) "-> Whether they match never changes (an invariant)"
      , txt px (py + 332) 14 (gray 0.78) "Goal = even + distance 0 = match. Mismatch can't get there"
      , txt px (py + 380) 16 (gray 0.78) ("Moves " ++ show (wMoves w) ++ (if solved (wBoard w) then "   Solved!" else ""))
      , txt 20 36 18 white "Click or arrows: slide   Space: shuffle   W: swap 14 and 15 (cheat!)   0: reset"
      , txt 20 62 15 (gray (170 / 255)) "Around 1880, a prize for solving the puzzle with 14-15 swapped went viral. Nobody could: permutation parity"
      ]

-- 置換の偶奇（空白は N*N 番のタイルとして扱う）
permParity :: [Int] -> Int
permParity b =
  let p = map (\t -> (if t == 0 then boardN * boardN else t) - 1) b
      len = length p
      go seen i cyc
        | i >= len = cyc
        | Set.member i seen = go seen (i + 1) cyc
        | otherwise = go (mark seen i) (i + 1) (cyc + 1)
      mark seen j | Set.member j seen = seen
                  | otherwise = mark (Set.insert j seen) (p !! j)
  in (len - go Set.empty 0 (0 :: Int)) `mod` 2

solved :: [Int] -> Bool
solved b = and (zipWith (\i t -> t == (i + 1) `mod` (boardN * boardN)) [0 ..] b)

-- マス i のタイルが空白のとなりなら動かす
tryMove :: Bool -> Int -> World -> World
tryMove animate i w
  | not adj = w
  | otherwise = w { wBoard = map swap' (wBoard w), wMoves = wMoves w + 1
                  , wSlide = if animate then Just (tile, i, 0) else wSlide w }
  where
    n = boardN
    b = blankAt (wBoard w)
    adj = abs (i `mod` n - b `mod` n) + abs (i `div` n - b `div` n) == 1
    tile = wBoard w !! i
    swap' t | t == 0 = tile
            | t == tile = 0
            | otherwise = t

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = toP5 (envMouse env)
        c = floor ((mx - ox) / cs) :: Int
        r = floor ((my - oy) / cs) :: Int
    in pure (if c >= 0 && c < n && r >= 0 && r < n then tryMove True (r * n + c) w else w)
  -- 矢印の向きにタイルが動く（= 空白は逆向き）
  EventKey (SpecialKey KeyLeft) Down _ _ | bc < n - 1 -> pure (tryMove True (b + 1) w)
  EventKey (SpecialKey KeyRight) Down _ _ | bc > 0 -> pure (tryMove True (b - 1) w)
  EventKey (SpecialKey KeyUp) Down _ _ | br < n - 1 -> pure (tryMove True (b + n) w)
  EventKey (SpecialKey KeyDown) Down _ _ | br > 0 -> pure (tryMove True (b - n) w)
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    w' <- foldM (\v _ -> randomMove v) w [1 .. 400 :: Int]
    pure w' { wMoves = 0 }
  EventKey (Char c) Down _ _ | c == 'w' || c == 'W' ->
    let sw t | t == n * n - 2 = n * n - 1
             | t == n * n - 1 = n * n - 2
             | otherwise = t
    in pure w { wBoard = map sw (wBoard w), wCheat = not (wCheat w) }
  EventKey (Char '0') Down _ _ -> pure fresh
  _ -> pure w
  where
    n = boardN
    b = blankAt (wBoard w)
    bc = b `mod` n
    br = b `div` n

-- 正しい手を 1 つランダムにえらぶ
randomMove :: World -> IO World
randomMove w = do
  let n = boardN
      bb = blankAt (wBoard w)
      nb = [ j | d <- [-1, 1, -n, n], let j = bb + d, j >= 0, j < n * n
           , abs d == n || j `div` n == bb `div` n ]
  k <- randomRIO (0, length nb - 1)
  pure (tryMove False (nb !! k) w)

-- ---------- やってみよう ----------
-- 1. W を押してから、14 と 15 以外がそろうところまで解いてみよう。最後の 2 枚はどうしても逆のまま
-- 2. W をもう 1 回押す（入れかえ 2 回 = 偶置換）とまた解ける。なぜ？
-- 3. boardN = 3（8 パズル）でも同じ。8 パズルの「解ける状態」は 9!/2 = 181,440 通り
-- 4. ルービックキューブにも同じ「偶奇の保存」がある（13）。辺 2 個だけの入れかえはできない

-- ---------- 道具 ----------
width, height :: Float
width = 1280
height = 800

-- gloss の座標 → p5 の座標（マウス用）
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y = labelC (x - width / 2) (height / 2 - y)

box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)
