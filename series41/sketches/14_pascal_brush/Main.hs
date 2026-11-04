-- ============================================
-- 41-14 パスカル・ブラシ
-- クリックした所から、二項係数 mod m の模様（03）が三角形に広がっていくスタンプ
-- 余りで色を塗る。m とマスの大きさで模様が変わる。ドラッグで連続スタンプ
-- 操作：クリック／ドラッグ = スタンプ / ↑↓ = m（2〜9）/ ←→ = マスの大きさ / F = 向きを回す / C = 消す / S = 保存
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Sequence as Seq

rowsMax :: Int
rowsMax = 40            -- ★ 何行まで広がるか

growth :: Float
growth = 1.2            -- ★ 1 フレームに進む行数

maxInk :: Int
maxInk = 7000           -- ★ 覚えておくマスの数（超えると古いものから消える）

data Stamp = Stamp
  { sX, sY    :: !Float
  , sM        :: !Int
  , sS        :: !Float
  , sRow      :: ![Int]
  , sDrawn    :: !Int
  , sProgress :: !Float
  , sHue      :: !Float
  , sAng      :: !Int     -- 向き（0: 下向き, 1: 左, 2: 上, 3: 右）
  }

data World = World
  { wM      :: !Int                 -- ★ 割る数
  , wCell   :: !Float               -- ★ マスの大きさ（px）
  , wStamps :: ![Stamp]             -- 広がっている最中のスタンプ
  , wInk    :: !(Seq.Seq Picture)   -- 描いたマス（古い順）
  , wAngle  :: !Int
  , wLast   :: !Point
  }

main :: IO ()
main = runSketch (sketch "41-14 Pascal brush" (World 3 6 [] Seq.empty 0 (-999, -999)))
  { skBack  = paper
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

paper :: Color
paper = hsb 230 30 8

addStamp :: Env -> Float -> Float -> World -> World
addStamp env x y w =
  w { wStamps = wStamps w ++
        [Stamp x y (wM w) (wCell w) [1] 0 0 (fromIntegral (envFrame env * 3 `mod` 360)) (wAngle w)]
    , wLast = (x, y) }

-- ★ 次の行：両端は 1、あとは (左上 + 右上) mod m
nextRow :: Int -> [Int] -> [Int]
nextRow md row = (1 `mod` md) : zipWith (\a b -> (a + b) `mod` md) row (drop 1 row) ++ [1 `mod` md]

-- 90 度ずつ回す（p5.js の rotate と同じ向き：y は下向き）
turn :: Int -> Float -> Float -> Point
turn a x y = case a `mod` 4 of
  0 -> (x, y)
  1 -> (-y, x)
  2 -> (-x, -y)
  _ -> (y, -x)

-- 行 sDrawn のマスの絵をつくる（向きに応じて回す）
rowPictures :: Env -> Stamp -> [Picture]
rowPictures env st =
  let r = sDrawn st
      fr = fromIntegral r :: Float
      s = sS st
      corner k (dx, dy) =
        let (tx, ty) = turn (sAng st) ((fromIntegral k - fr / 2) * s - s / 2 + dx) (fr * s + dy)
        in toG env (sX st + tx) (sY st + ty)
      square (k, v) =
        Color (hsb (wrapF 360 (sHue st + fromIntegral v * 360 / fromIntegral (sM st) * 0.5)) 70 (95 - fr * 0.8))
              (Polygon (map (corner (k :: Int)) [(0, 0), (s - 1, 0), (s - 1, s - 1), (0, s - 1)]))
  in [square kv | kv@(_, v) <- zip [0 ..] (sRow st), v /= 0]

-- スタンプを進める（新しい行のマスだけ絵をつくる → 速い）
growStamp :: Env -> (Seq.Seq Picture, [Stamp]) -> Stamp -> (Seq.Seq Picture, [Stamp])
growStamp env (ink, acc) st0 =
  let advance i st
        | sDrawn st < min rowsMax (floor (sProgress st)) =
            advance (foldl' (Seq.|>) i (rowPictures env st))
                    st { sRow = nextRow (sM st) (sRow st), sDrawn = sDrawn st + 1 }
        | otherwise = (i, st)
      (ink1, st1) = advance ink st0 { sProgress = sProgress st0 + growth }
  in (ink1, if sDrawn st1 < rowsMax then st1 : acc else acc)

step :: Env -> Float -> World -> IO World
step env _ w =
  let (ink, stamps) = foldl' (growStamp env) (wInk w, []) (wStamps w)
      extra = Seq.length ink - maxInk
  in pure w { wInk = if extra > 0 then Seq.drop extra ink else ink, wStamps = reverse stamps }

draw :: Env -> World -> Picture
draw env w =
  let (mx, my) = mouseP env
      len = fromIntegral rowsMax * wCell w
      corner (x, y) = let (tx, ty) = turn (wAngle w) x y in toG env (mx + tx) (my + ty)
  in Pictures
       [ Pictures (toList (wInk w))
       , txt env 20 40 16 (hsb 0 0 100)
           ("mod " ++ show (wM w) ++ " (Up/Down)   cell " ++ show (round (wCell w) :: Int) ++ "px (L/R)   F: direction   C: clear   S: save   growing: "
            ++ show (length (wStamps w)))
       , txt env 20 68 14 (hsb 0 0 60)
           "Click or drag to stamp. Remainder 0 stays empty. m = 2 gives Sierpinski; each prime m has its own fractal."
       -- カーソルに小さなプレビュー
       , Color (hsba 0 0 100 0.4) (poly (map corner [(0, 0), (-len / 2, len), (len / 2, len)]))
       ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env in pure (addStamp env mx my w)
  EventMotion _
    | envDown env ->
        -- ★ 一定の距離を動いたら次のスタンプ
        let p@(mx, my) = mouseP env
        in pure (if dist2 p (wLast w) > wCell w * 5 then addStamp env mx my w else w)
  EventKey (SpecialKey KeyUp) Down _ _ | wM w < 9 -> pure w { wM = wM w + 1 }
  EventKey (SpecialKey KeyDown) Down _ _ | wM w > 2 -> pure w { wM = wM w - 1 }
  EventKey (SpecialKey KeyRight) Down _ _ | wCell w < 14 -> pure w { wCell = wCell w + 1 }
  EventKey (SpecialKey KeyLeft) Down _ _ | wCell w > 2 -> pure w { wCell = wCell w - 1 }
  EventKey (Char c) Down _ _
    | c `elem` "fF" -> pure w { wAngle = (wAngle w + 1) `mod` 4 }
    | c `elem` "cC" -> pure w { wInk = Seq.empty, wStamps = [] }
    | c `elem` "sS" -> do
        saveSvg "pascal_brush" env (Pictures [Color paper (rect 0 0 (envW env) (envH env)), draw env w])
        pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. m = 2 と m = 3 と m = 5 で同じ場所にスタンプして、模様の「自己相似」のちがいを見よう
-- 2. rowsMax を 128 にしてマスを 2px にすると、大きなシェルピンスキーが描ける（m = 2 で 2^7 行。maxInk に注意）
-- 3. 色を「余り」ではなく「行番号」で決めると虹色の三角形に。rowPictures の色を変えよう
-- 4. 向き（F）を変えながら中心から 4 方向に押すと、雪の結晶のような形に。自由制作のベースにどうぞ

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js の座標で
mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s
