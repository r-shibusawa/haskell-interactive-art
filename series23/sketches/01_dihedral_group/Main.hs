-- ============================================
-- 01 二面体群 Dn
-- 正 n 角形の対称性は 2 つの操作 R（360°/n 回す）と F（たて軸で裏返す）の組み合わせで全部つくれる
--   2n 個の元を表にしたものがケイリー表。R·F と F·R は違う → 順番が大事（非可換）
-- R = 回す / F = 裏返す / ↑↓ = n（3〜6）/ 表のマスをクリック = その掛け算を実行 / A = 表を全部うめる / 0 = リセット
-- ============================================
module Main (main) where

import Art
import qualified Data.Set as Set

-- 元 (k, f) = r^k s^f（関数として「まず s^f、次に r^k」）
type El = (Int, Int)

data World = World
  { wN     :: !Int                    -- ★ 角の数
  , wG     :: !El                     -- 今の元
  , wWord  :: ![String]               -- 押したキーの列（新しい順）
  , wFill  :: !(Set.Set (Int, Int))   -- うまった表のマス
  , wAnim  :: !(Maybe (Float, Bool))  -- 見た目のアニメーション（進み具合、裏返しか）
  , wShown :: !El                     -- アニメーション開始時の元
  , wAuto  :: !Bool
  }

world0 :: World
world0 = World 3 (0, 0) [] Set.empty Nothing (0, 0) False

main :: IO ()
main = runSketch (sketch "23-01 Dihedral Group" world0)
  { skBack  = rgb255 244 242 250
  , skStep  = \env _ w -> pure (step (envFrame env) w)
  , skDraw  = \env w -> drawWorld (toP5 (envMouse env)) w
  , skEvent = \env ev w -> pure (onEvent (toP5 (envMouse env)) ev w)
  }

-- 元の番号 ↔ (k, f)：0..n-1 が回転、n..2n-1 が鏡映
el :: Int -> Int -> El
el n i = (i `mod` n, if i < n then 0 else 1)

idx :: Int -> El -> Int
idx n (k, f) = k + f * n

-- 「x をしてから y」= 関数の合成 y∘x（s が r を逆向きにする）
andThen :: Int -> El -> El -> El
andThen n (xk, xf) (yk, yf) = ((yk + (if yf /= 0 then -xk else xk)) `mod` n, (xf + yf) `mod` 2)

name :: El -> String
name (0, 0) = "e"
name (k, f) = (if k /= 0 then "r" ++ (if k > 1 then show k else "") else "") ++ (if f /= 0 then "s" else "")

opR, opF :: El
opR = (1, 0)
opF = (0, 1)

step :: Int -> World -> World
step frame w0 =
  let w1 = case wAnim w0 of
        Just (t, fl) | t >= 1 -> w0 { wAnim = Nothing }
                     | otherwise -> w0 { wAnim = Just (min 1 (t + 0.06), fl) }
        Nothing -> w0
  in if wAuto w1 && frame `mod` 4 == 0 then autoFill w1 else w1

-- 表を 1 マスずつうめる
autoFill :: World -> World
autoFill w =
  let nn = 2 * wN w
      todo = [(i, j) | i <- [0 .. nn - 1], j <- [0 .. nn - 1], not (Set.member (i, j) (wFill w))]
  in case todo of
       (c : _) -> w { wFill = Set.insert c (wFill w) }
       [] -> w { wAuto = False }

drawWorld :: Point -> World -> Picture
drawWorld mouse w = Pictures
  [ drawPolygon 281.6 384 192 w
  , txt 20 36 18 (gray 0.12) ("R: rotate   F: flip   Up/Down: n = " ++ show (wN w) ++ "   click a cell: do that product   A: fill table   0: reset")
  , txt 20 62 15 (gray 0.43) "Cell (row a, column b) = 'do a, then b'. Every row and column has each element once (Latin square)"
  , txt 30 704 18 (gray 0.12) ("Pressed: " ++ (if null shown then "(none)" else shown))
  , txt 30 744 30 (rgb255 120 60 200) ("= " ++ name (wG w))
  , txt 30 780 16 (gray 0.12) ("R*F = " ++ name (andThen n opR opF) ++ "    F*R = " ++ name (andThen n opF opR) ++ "   -> different! (non-commutative)")
  , drawTable mouse w
  ]
  where
    n = wN w
    shown = joinDots (reverse (take 16 (wWord w)))
    joinDots [] = ""
    joinDots [x] = x
    joinDots (x : r) = x ++ "*" ++ joinDots r

-- 番号つきの正 n 角形（アニメーションつき）
drawPolygon :: Float -> Float -> Float -> World -> Picture
drawPolygon cx cy r w = Pictures $
  [ Color (makeColorI 0 0 0 8) (Polygon (map (pt . base) ids))
  , Color (makeColorI 0 0 0 30) (thickPath 3 (map (pt . base) (ids ++ take 1 ids)))
  , dashed 2 (rgba255 220 60 90 120) (cx, cy - r * 1.25) (cx, cy + r * 1.25)   -- F の軸
  , Color (rgba255 150 130 240 120) (Polygon (map pt pts))
  , Color (rgb255 50 50 120) (thickPath 4 (map pt (pts ++ take 1 pts)))
  ] ++ concat
  [ [ discAt x y 20 (hsb (fromIntegral i * 360 / fromIntegral n) 70 90)
    , txtC x y 20 white (show (i + 1)) ]
  | (i, (x, y)) <- zip ids pts ]
  where
    n = wN w
    ids = [0 .. n - 1]
    (t, isFlip) = case wAnim w of
      Just (a, fl) -> (a * a * (3 - 2 * a), fl)
      Nothing -> (1, False)
    base i = let th = start i in (cx + r * cos th, cy + r * sin th)
    start :: Int -> Float
    start i = -pi / 2 + fromIntegral i * tau / fromIntegral n
    -- 頂点 i の角度：元 (k, f) は角度 θ を (f ? π - θ : θ) にしてから k 段回す
    pos (k, f) i =
      let th = if f /= 0 then pi - start i else start i
      in th + fromIntegral k * tau / fromIntegral n
    pts = map point1 ids
    point1 i =
      let a = pos (wShown w) i
          b = pos (wG w) i
          d = atan2 (sin (b - a)) (cos (b - a))     -- 近い向きに回す
      in if isFlip     -- 裏返しは x 座標だけ反転して見せる
           then (cx + r * lerpF (cos a) (cos b) t, cy + r * lerpF (sin a) (sin b) t)
           else (cx + r * cos (a + d * t), cy + r * sin (a + d * t))

-- 表の位置と 1 マスの大きさ
tableX, tableY :: Float
tableX = 588.8
tableY = 110

cellSize :: Int -> Float
cellSize nn = min ((width - tableX - 30) / fromIntegral (nn + 1)) ((height - 250) / fromIntegral (nn + 1))

-- マウスが乗っているマス
hoverCell :: Point -> Int -> Maybe (Int, Int)
hoverCell (mx, my) n =
  let nn = 2 * n
      cs = cellSize nn
      j = floor ((mx - tableX) / cs) - 1 :: Int
      i = floor ((my - tableY) / cs) - 1 :: Int
  in if i >= 0 && i < nn && j >= 0 && j < nn then Just (i, j) else Nothing

-- ケイリー表（行 a・列 b に「a をしてから b」）
drawTable :: Point -> World -> Picture
drawTable mouse w = Pictures (concat cells ++ hoverLine ++ [countLine])
  where
    n = wN w
    nn = 2 * n
    cs = cellSize nn
    hov = hoverCell mouse n
    fs = max 14 (cs * 0.36)
    bottom = tableY + fromIntegral (nn + 1) * cs
    cells =
      [ cell i j (tableX + fromIntegral (j + 1) * cs) (tableY + fromIntegral (i + 1) * cs)
      | i <- [-1 .. nn - 1], j <- [-1 .. nn - 1] ]
    cell i j x y
      | i < 0 && j < 0 = [txtC (x + cs / 2) (y + cs / 2) (max 14 (cs * 0.28)) (gray 0.12) "a\\b"]
      | i < 0 || j < 0 =
          let e = el n (if i < 0 then j else i)
          in [ box (x + 1) (y + 1) (cs - 2) (cs - 2) (if snd e /= 0 then rgb255 200 230 255 else rgb255 255 225 200)
             , txtC (x + cs / 2) (y + cs / 2) fs (gray 0.12) (name e) ]
      | otherwise =
          let p@(pk, pf) = andThen n (el n i) (el n j)
              on = Set.member (i, j) (wFill w)
              over = hov == Just (i, j)
              col | on = hsb (fromIntegral (if pf /= 0 then 200 + pk * 25 else 20 + pk * 25)) 45 95
                  | over = gray (225 / 255)
                  | otherwise = gray (235 / 255)
          in box (x + 0.5) (y + 0.5) (cs - 1) (cs - 1) col
             : [ txtC (x + cs / 2) (y + cs / 2) fs (gray 0.12) (name p) | on ]
    hoverLine = case hov of
      Just (i, j) ->
        [ txt tableX (bottom + 30) 18 (gray 0.12)
            (name (el n i) ++ " then " ++ name (el n j) ++ "  =  " ++ name (andThen n (el n i) (el n j))) ]
      Nothing -> []
    countLine = txt tableX (bottom + 56) 14 (gray 0.43)
      ("Filled " ++ show (Set.size (wFill w)) ++ " / " ++ show (nn * nn) ++ " (R or F fills that column in the current row)")

applyOp :: El -> String -> World -> World
applyOp op lab w =
  let n = wN w
  in w { wFill = Set.insert (idx n (wG w), idx n op) (wFill w)
       , wShown = wG w
       , wAnim = Just (0, op == opF)
       , wG = andThen n (wG w) op
       , wWord = lab : take 40 (wWord w) }

resetAll :: World -> World
resetAll w = w { wG = (0, 0), wShown = (0, 0), wWord = [], wFill = Set.empty, wAnim = Nothing }

onEvent :: Point -> Event -> World -> World
onEvent mouse ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> case hoverCell mouse n of
    Just (i, j) ->
      -- e から a を実行 → b を実行、の 2 段階を見せる
      let a = el n i
          b = el n j
      in w { wFill = Set.insert (i, j) (wFill w), wWord = [name b, name a]
           , wShown = a, wG = andThen n a b, wAnim = Just (0, False) }
    Nothing -> w
  EventKey (Char c) Down _ _
    | c == 'r' || c == 'R' -> applyOp opR "R" w
    | c == 'f' || c == 'F' -> applyOp opF "F" w
    | c == 'a' || c == 'A' -> w { wAuto = not (wAuto w) }
    | c == '0' -> resetAll w
  EventKey (SpecialKey KeyUp) Down _ _ -> resetAll w { wN = min 6 (n + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> resetAll w { wN = max 3 (n - 1) }
  _ -> w
  where n = wN w

-- ---------- やってみよう ----------
-- 1. R を n 回、F を 2 回 → e。R F R F は？（F R F = R⁻¹ になる）
-- 2. 表の対角線（a·a）を見て、e になる元を数えよう。鏡映はすべて「2 回で e」
-- 3. n = 4 の表で、左上 4×4（回転だけ）は閉じている = 部分群（→ 05）
-- 4. R·F と F·R の違いを、番号 1 の行き先で説明してみよう
-- 5. どの行・列にも全部の元が 1 回ずつ。なぜ？（ヒント：逆元があるから）

-- ---------- 道具 ----------
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Point -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

-- gloss の座標 → p5 の座標（マウス用）
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y = labelC (x - width / 2) (height / 2 - y)

box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)

discAt :: Float -> Float -> Float -> Color -> Picture
discAt x y r c = Color c (at (pt (x, y)) (disc r))

dashed :: Float -> Color -> Point -> Point -> Picture
dashed w c (x1, y1) (x2, y2) =
  let l = dist2 (x1, y1) (x2, y2)
      n = floor (l / 12) :: Int
      p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
  in Color c (Pictures
       [ thickLine w (pt (p (12 * fromIntegral i / l))) (pt (p (min 1 ((12 * fromIntegral i + 6) / l))))
       | i <- [0 .. n] ])
