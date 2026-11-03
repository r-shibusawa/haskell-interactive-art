-- ============================================
-- 09 基本群とループ
-- 穴（くぎ）のあいた平面に輪ゴムのループをかける。ループを切らずに 1 点まで縮められるか？
--   各穴のまわりを何回まわったか（回転数）と、穴の上の「見えない線」をどの順で横切ったか（言葉）で調べる ＝ 基本群
-- ドラッグ = ループを描く（離すと閉じる） / スペース = 輪ゴムを縮める / 1 = a / 2 = a a / 3 = a b / 4 = a b a⁻¹ b⁻¹（回転数 0 なのに縮まない！） / 5 = 縮むループ / C = 消す
-- ============================================
module Main (main) where

import Art
import Data.Char (isLower, toLower)
import Data.List (elemIndex, foldl')
import Data.Maybe (fromMaybe)

holeR, shrinkRate, spacing :: Float
holeR = 26           -- ★ 穴の半径
shrinkRate = 0.35    -- ★ 縮む強さ（なめらかにする量）
spacing = 6          -- ★ ループの点の間隔（px）

maxDraw :: Int
maxDraw = 3000       -- 描くループの点の数の上限

width, height :: Float
width = 1280
height = 800

type P = (Float, Float)

names :: [Char]
names = "abc"

hues :: [Float]
hues = [20, 200, 120]

holes :: [P]
holes =
  let cx = width * 0.42
      cy = height * 0.5
  in [(cx - 200, cy), (cx, cy - 30), (cx + 200, cy + 20)]

data World = World
  { wLoop :: ![P]               -- ループの点（描いている途中は新しい順）
  , wDrawing :: !Bool
  , wDrawN :: !Int
  , wShrinking, wCollapsed :: !Bool
  }

main :: IO ()
main = runSketch (sketch "27-09 Fundamental Group and Loops" (wordPreset "ab" (World [] False 0 False False)))
  { skBack  = hsb 190 30 12
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- | 言葉からループをつくる：下の基点 → 穴の下 → 穴のまわりを 1 周 → 基点へ戻る、をつなげる
wordPreset :: String -> World -> World
wordPreset word w =
  let bx = width * 0.42
      by = height * 0.86
      part (k, ch) =
        let i = fromMaybe 0 (elemIndex (toLower ch) names)
            sgn = if isLower ch then 1 else -1 :: Float
            (hx, hy) = holes `at'` i
            rr = 62 + fromIntegral (k :: Int) * 10
            off = fromIntegral k * 5
        in (bx + off, by) :
           [ (hx + rr * cos th + off * 0.3, hy + rr * sin th)
           | a <- [0 .. 64 :: Int], let th = pi / 2 - sgn * (fromIntegral a / 64) * tau ]
  in w { wLoop = resample (concatMap part (zip [0 ..] word)), wDrawing = False, wDrawN = 0, wShrinking = False, wCollapsed = False }

at' :: [P] -> Int -> P
at' xs i = case drop i xs of
  (x : _) -> x
  [] -> (0, 0)

-- | ★ 回転数：ループにそって「穴から見た向き」の変化を足す（反時計回り = +1）
winding :: [P] -> P -> Int
winding loop (hx, hy) =
  let delta (ax, ay) (bx, by) =
        let d0 = atan2 (by - hy) (bx - hx) - atan2 (ay - hy) (ax - hx)
            d1 = if d0 > pi then d0 - tau else d0
        in if d1 < -pi then d1 + tau else d1
      total = foldl' (+) 0 (zipWith delta loop (rotate1 loop))
  in round (-total / tau)

-- | ★ 言葉：各穴から真上に引いた線を横切るたびに文字を書く（右→左 = a、左→右 = a⁻¹）。となりの a a⁻¹ は消す
loopWord :: [P] -> [(Int, Int)]
loopWord loop =
  let crossAt (ax, ay) (bx, by) =
        [ (k, if bx < ax then 1 else -1)
        | (k, (hx, hy)) <- zip [0 ..] holes
        , (ax < hx) /= (bx < hx)
        , ay + (hx - ax) * (by - ay) / (bx - ax) < hy ]
      syms = concat (zipWith crossAt loop (rotate1 loop))
      push st s = case st of
        (t : more) | fst t == fst s && snd t == negate (snd s) -> more
        _ -> s : st
      out = reverse (foldl' push [] syms)
      -- 輪なので両端も消せる
      trim xs = case xs of
        (a : rest@(_ : _)) ->
          let l = lastOf rest
          in if fst a == fst l && snd a == negate (snd l) then trim (dropLast rest) else xs
        _ -> xs
  in trim out

---------------------------------------------------------------- 動き

stepWorld :: World -> World
stepWorld w
  | wShrinking w && not (wCollapsed w) && not (wDrawing w) && length (take 3 (wLoop w)) > 2 =
      iterate shrinkStep w !! 4
  | otherwise = w

-- | 輪ゴムの 1 ステップ：となりの点の真ん中へ寄せる → 穴の中に入ったら押し出す → 点の間隔をそろえる
shrinkStep :: World -> World
shrinkStep w
  | wCollapsed w = w
  | otherwise =
      let ps = wLoop w
          prevs = rotateBack ps
          nexts = rotate1 ps
          pull (ax, ay) (px, py) (bx, by) =
            (px + shrinkRate * ((ax + bx) / 2 - px), py + shrinkRate * ((ay + by) / 2 - py))
          pushOut p (hx, hy) =
            let (x, y) = p
                dx = x - hx
                dy = y - hy
                d = sqrt (dx * dx + dy * dy)
            in if d < holeR + 5
                 then let s = (holeR + 5) / (if d == 0 then 1 else d) in (hx + dx * s, hy + dy * s)
                 else p
          next = map (\p -> foldl' pushOut p holes) (zipWith3 pull prevs ps nexts)
          loop' = resample next
          len = sum (zipWith dist2 loop' (rotate1 loop'))
      in w { wLoop = loop', wCollapsed = len < 25 }

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let m@(_, my) = toP5 pos
    in if my > 90 then w { wLoop = [m], wDrawing = True, wDrawN = 1, wShrinking = False, wCollapsed = False } else w
  EventMotion pos | wDrawing w ->
    let m = toP5 pos
    in case wLoop w of
         (l : _) | dist2 m l > 5 && wDrawN w < maxDraw -> w { wLoop = m : wLoop w, wDrawN = wDrawN w + 1 }
         _ -> w
  EventKey (MouseButton LeftButton) Up _ _ | wDrawing w ->
    w { wDrawing = False, wDrawN = 0, wLoop = if wDrawN w > 5 then resample (reverse (wLoop w)) else [] }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wShrinking = not (wShrinking w), wCollapsed = False }
  EventKey (Char '1') Down _ _ -> wordPreset "a" w
  EventKey (Char '2') Down _ _ -> wordPreset "aa" w
  EventKey (Char '3') Down _ _ -> wordPreset "ab" w
  EventKey (Char '4') Down _ _ -> wordPreset "abAB" w
  EventKey (Char '5') Down _ _ -> wordPreset "aA" w
  EventKey (Char c) Down _ _ | c `elem` "cC" -> w { wLoop = [], wShrinking = False, wDrawing = False, wDrawN = 0 }
  _ -> w

---------------------------------------------------------------- 描画

drawWorld :: World -> Picture
drawWorld w =
  let loop = wLoop w
      hasLoop = length (take 3 loop) > 2
      idle = not (wDrawing w) && hasLoop
      ws = [if idle then winding loop h else 0 | h <- holes]
      -- 見えない線（穴の上）を点線で
      dash i (hx, hy) =
        let ys = takeWhile (> 90) [hy - fromIntegral k * 14 | k <- [0 :: Int ..]]
            c = hsba (hues `atF` i) 50 60 0.5
        in Color c (Pictures [thickLine 2 (pt (hx, y)) (pt (hx, max 90 (y - 6))) | y <- ys])
      loopPic
        | length (take 2 loop) < 2 = Blank
        | wDrawing w = Color (hsb 50 80 100) (thickPath 4 (map pt loop))
        | otherwise = Color (hsb 50 80 100) (thickPath 4 (map pt (loop ++ take 1 loop)))
      holePic (i, h@(hx, hy), wn) =
        let (gx, gy) = pt (hx, hy - 2)
            (lx, ly) = pt (hx, hy + holeR + 22)
        in Pictures
             [ at (pt h) (Color (hsb (hues `atF` i) 70 90) (disc holeR))
             , labelC gx gy 22 (hsb 0 0 10) [names `atC` i]
             , labelC lx ly 15 (gray 1) ("winding " ++ show wn) ]
      x0 = width * 0.68
      wordPic
        | not idle = Blank
        | otherwise =
            let word = loopWord loop
                wordStr = if null word then "1 (empty)" else unwords [ (names `atC` k) : (if e < 0 then "^-1" else "") | (k, e) <- word ]
                size = clampF 14 28 (390 / (0.8 * fromIntegral (max 1 (length wordStr))))
                shown = if length wordStr > 44 then take 41 wordStr ++ "..." else wordStr
                allZero = all (== 0) ws
                c65 = gray 0.65
            in Pictures $
                 [ txt x0 150 20 (gray 1) "Loop word:"
                 , txt x0 196 size (gray 1) shown
                 , txt x0 250 20 (hsb (if null word then 130 else 0) 70 100) (if null word then "Can shrink to a point" else "Cannot shrink") ]
                 ++ (if not (null word) && allZero
                       then [ txt x0 284 14 c65 "All windings are 0, still stuck!"
                            , txt x0 306 14 c65 "a b and b a are in different order"
                            , txt x0 328 14 c65 "(the group cares about order)" ]
                       else [ txt x0 284 14 c65 "Winding = number of a in the word"
                            , txt x0 306 14 c65 "          - number of a^-1" ])
                 ++ (if wCollapsed w then [txt x0 380 18 (hsb 130 70 100) "Shrunk to a point!"]
                     else if wShrinking w
                       then [ txt x0 380 16 (gray 1) "Shrinking..."
                            , txt x0 404 16 (gray 1) "(stops when caught on a hole)" ]
                       else [])
  in Pictures $
       zipWith dash [0 ..] holes
       ++ [loopPic]
       ++ map holePic (zip3 [0 ..] holes ws)
       ++ [ wordPic
          , txt 20 40 15 (gray 1) ("Drag: draw loop  Space: shrink " ++ (if wShrinking w then "ON" else "off")
                                   ++ "  1: a  2: a a  3: a b  4: a b a^-1 b^-1  5: shrinkable  C: clear")
          , txt 20 68 14 (gray 0.6) "Dashed = invisible line above a hole. Right to left = a, left to right = a^-1. Neighbors a a^-1 cancel."
          ]

atF :: [Float] -> Int -> Float
atF xs i = case drop i xs of
  (x : _) -> x
  [] -> 0

atC :: [Char] -> Int -> Char
atC xs i = case drop i xs of
  (x : _) -> x
  [] -> '?'

-- ---------- やってみよう ----------
-- 1. 4（a b a⁻¹ b⁻¹）を縮めると、どこに引っかかって止まる？ 回転数が 0 なのに縮まない理由を考えよう
-- 2. 穴 a のまわりを 2 回まわるループを自分で描いて、回転数と言葉を確かめよう
-- 3. 穴を 1 つにすると（holes を 1 個に）、ループは回転数だけで分類できる（基本群 ＝ 整数）
-- 4. 「額縁に絵をかけるひも」パズル：くぎ 2 本にかけて、どちらのくぎを抜いても落ちるかけ方の言葉は？（ヒント：4）

-- ==================================================
-- ここから下は「道具」（ループの点を等間隔にする、座標の変換）
-- ==================================================

resample :: [P] -> [P]
resample [] = []
resample ps =
  let segs = zipWith dist2 ps (rotate1 ps)
      l = foldl' (+) 0 segs
      m = max 8 (min 1500 (round (l / spacing))) :: Int
      targets = [fromIntegral k / fromIntegral m * l | k <- [0 .. m - 1]]
      -- targets は増えていくので、線分のリストを前から順に見ていけばよい
      go _ _ [] = []
      go acc rest@((a, b, s) : more) (t : ts)
        | not (null more) && acc + s < t = go (acc + s) more (t : ts)
        | otherwise =
            let u = if s > 0 then (t - acc) / s else 0
                (ax, ay) = a
                (bx, by) = b
            in (ax + (bx - ax) * u, ay + (by - ay) * u) : go acc rest ts
      go _ [] _ = []
  in go 0 (zip3 ps (rotate1 ps) segs) targets

rotate1 :: [a] -> [a]
rotate1 [] = []
rotate1 (x : xs) = xs ++ [x]

rotateBack :: [a] -> [a]
rotateBack [] = []
rotateBack xs = lastOf xs : dropLast xs

lastOf :: [a] -> a
lastOf = foldr1 (\_ b -> b)

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x : xs) = x : dropLast xs

pt :: P -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

toP5 :: Point -> P
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = let (gx, gy) = pt (x, y) in label gx gy s c str
