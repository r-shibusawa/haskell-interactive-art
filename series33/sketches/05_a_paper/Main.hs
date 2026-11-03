-- ============================================
-- 05 A 判の紙 ─ 半分に切っても同じ形（√2 の白銀比）
-- A0 は面積 1 m²、縦横 1 : √2。半分に切ると A1、また半分で A2 … 形はずっと同じ
--   r / 2 = 1 / r  ⇔  r² = 2  ⇔  r = √2（半分にして相似になる比は √2 だけ）
-- クリック / H = 半分に切る / B = B 判を重ねる / F = 比を自由に（←→）/ R = A0 に戻す
-- ============================================
module Main (main) where

import Art
import Data.List (mapAccumL)
import Numeric (showFFloat)

data World = World
  { wCuts  :: !Int     -- 何回切ったか
  , wAnim  :: !Float   -- 切るアニメーション（0→1）
  , wShowB :: !Bool
  , wFree  :: !Bool
  , wFreeR :: !Float   -- F モードの比
  }

maxCut :: Int
maxCut = 10            -- ★

initialWorld :: World
initialWorld = World 0 1 False False 1.6

main :: IO ()
main = runSketch (sketch "33-05 A Paper" initialWorld)
  { skBack  = hsb 40 10 14
  , skStep  = \_ _ w -> pure w { wAnim = min 1 (wAnim w + 0.04) }
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

x0, y0 :: Float
x0 = 40
y0 = 120

-- 長方形 (x, y, w, h)
type Rc = (Float, Float, Float, Float)

-- 残りの紙を半分に切る。戻り値は (新しい残り, 切り取った片)
halve :: Rc -> (Rc, Rc)
halve (x, y, w, h)
  | h >= w    = ((x, y + h / 2, w, h / 2), (x, y, w, h / 2))   -- 横に切る（長い辺を半分）
  | otherwise = ((x + w / 2, y, w / 2, h), (x, y, w / 2, h))   -- 縦に切る

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (pieces ++ restPic ++ bPaper ++ [outer] ++ hud ++ [drawPanel env w (x0 + bigW + 110) 130 r])
  where
    r = if wFree w then wFreeR w else sqrt 2
    -- 紙の大きさ（長辺 = 縦）
    bigH = envH env - 190
    bigW = bigH / r
    cuts = wCuts w
    prefix = if wFree w then "#" else "A"
    -- 切った紙を順に並べる：k 回目に切った片は「残り」の半分
    (rest, rects) = mapAccumL (\rc _ -> halve rc) (x0, y0, bigW, bigH) [1 .. cuts]
    pieces = concat (zipWith piece [0 ..] rects)
    piece :: Int -> Rc -> [Picture]
    piece k (px, py, pw, ph) =
      let t = if k == cuts - 1 then easeOut (wAnim w) else 1
          off = 6 * t      -- 切り取った片をすこし浮かせて影をつける
      in [ Color (hsba 0 0 0 0.3) (box env (px + off + 3) (py + off + 3) pw ph)
         , Color (hsb (wrapF 360 (fromIntegral (k * 33 + 190))) (30 + 20 * t) 95) (box env px py pw ph)
         , Color (hsb 0 0 30) (boxLine env px py pw ph)
         ] ++ paperName env (hsb 0 0 20) (prefix ++ show (k + 1)) px py pw ph
    restPic =
      let (x, y, rw, rh) = rest
      in [ Color white (box env x y rw rh)
         , Color (hsb 0 0 30) (boxLine env x y rw rh)
         ] ++ paperName env (hsb 0 0 30) (prefix ++ show cuts) x y rw rh
    -- B 判（日本の JIS）：面積が A 判の 1.5 倍 → 辺は √1.5 ≈ 1.22 倍。同じ √2 の形
    bPaper
      | wShowB w && not (wFree w) =
          [ Color (hsb 350 70 90) (dashedBox env x0 y0 (if odd k then bh else bw) (if odd k then bw else bh))
          | k <- [1 .. 4 :: Int]
          , let bh = sqrt 1.5 * bigH / sqrt 2 / sqrt 2 ^ (k - 1)
          , let bw = bh / sqrt 2 ]
      | otherwise = []
    outer = Color white (thickPath 2 (map (uncurry (pt env))
              [(x0, y0), (x0 + bigW, y0), (x0 + bigW, y0 + bigH), (x0, y0 + bigH), (x0, y0)]))
    hud =
      [ txt env 20 28 15 white ("Click / H: cut in half (" ++ show cuts ++ ")   B: B series " ++ onOff (wShowB w) ++ "   F: free ratio " ++ (if wFree w then "ON (L/R)" else "off") ++ "   R: back to A0")
      , txt env 20 58 14 (gray 0.75) "Every cut gives the same shape. A copier's A4 -> A3 '141%' is just sqrt2."
      ]

-- 紙の名前。片が小さすぎるときは書かない
paperName :: Env -> Color -> String -> Float -> Float -> Float -> Float -> [Picture]
paperName env c s x y w h
  | min w h < 18 = []
  | otherwise =
      let (gx, gy) = pt env (x + w / 2) (y + h / 2)
      in [ labelC gx gy (clampF 14 40 (min w h * 0.25)) c s ]

-- 右側：寸法と比の表
drawPanel :: Env -> World -> Float -> Float -> Float -> Picture
drawPanel env w px py r
  | wFree w = Pictures
      ( [ txt env px py 15 white ("First ratio 1 : " ++ nf 3 r) ]
        ++ zipWith freeRow [0 ..] (take 9 (iterate halveAB (1, r)))
        ++ [ txt env px (py + 250) 14 (gray 0.7) "The ratio swings between r and 2/r."
           , txt env px (py + 272) 14 (gray 0.7) "r = 2/r only for r = sqrt2 = 1.4142" ])
  | otherwise = Pictures
      ( [ txt env px py 15 white "A series (mm)", txt env (px + 300) py 15 white "B series (mm)" ]
        ++ concat [ [ txt env px (py + 28 + fromIntegral k * 22) 14 (if k == wCuts w then hsb 50 80 100 else gray 0.8) (sizeText 'A' k)
                    , txt env (px + 300) (py + 28 + fromIntegral k * 22) 14 (hsb 350 40 90) (sizeText 'B' k) ]
                  | k <- [0 .. 8] ]
        ++ [ txt env px yy 15 white ("A4 = 210 x 297 -> 297 / 210 = " ++ nf 4 (297 / 210))
           , txt env px (yy + 24) 15 white ("sqrt2 = " ++ nf 4 (sqrt 2))
           , txt env px (yy + 56) 14 (gray 0.7) "A0 has area 1 m^2, B0 has 1.5 m^2 (Japanese B series)."
           , txt env px (yy + 78) 14 (gray 0.7) "Remove one square from 1 : sqrt2 and 1 : (1 + sqrt2) remains:"
           , txt env px (yy + 100) 14 (gray 0.7) "the metallic 'silver ratio' (try F in sketch 01)."
           ])
  where
    yy = py + 240
    halveAB (a, b) = if a < b then (a, b / 2) else (a / 2, b)
    freeRow :: Int -> (Float, Float) -> Picture
    freeRow k (a, b) =
      let ratio = max a b / min a b
          same = abs (ratio - r) < 0.001
      in txt env px (py + 30 + fromIntegral k * 22) 14 (if same then hsb 120 60 100 else hsb 10 70 100)
           ("#" ++ show k ++ "  1 : " ++ nf 4 ratio ++ (if same then "  same shape" else "  different"))
    sizeText c k = let (s, l) = paper c k in c : show k ++ "  " ++ show s ++ " x " ++ show l

-- k 番目の紙の寸法 (短辺, 長辺)（mm）。長辺を半分に切り捨てたものが次の短辺
paper :: Char -> Int -> (Int, Int)
paper c k = iterate (\(s, l) -> (l `div` 2, s)) start !! max 0 k
  where start = if c == 'A' then (841, 1189) else (1030, 1456)

cut :: World -> World
cut w | wCuts w < maxCut = w { wCuts = wCuts w + 1, wAnim = 0 }
      | otherwise = w

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> cut w
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wFreeR = min 3 (wFreeR w + 0.05) }
  EventKey (SpecialKey KeyLeft) Down _ _  -> w { wFreeR = max 1.05 (wFreeR w - 0.05) }
  EventKey (Char c) Down _ _
    | c == 'h' || c == 'H' -> cut w
    | c == 'b' || c == 'B' -> w { wShowB = not (wShowB w) }
    | c == 'f' || c == 'F' -> w { wFree = not (wFree w) }
    | c == 'r' || c == 'R' -> w { wCuts = 0, wAnim = 1 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 表の A4 の寸法を、実際の A4 の紙を定規で測って確かめよう
-- 2. F で比を 1.6（黄金比の近く）にして切ってみよう。形はどう変わる？
-- 3. A4 2 枚 = A3、B5 と A4 の大きさの関係は？（JIS の B 判は面積 1.5 倍 → 辺は √1.5 ≈ 1.22 倍。国際規格の B 判は 2 の 4 乗根 ≈ 1.19 倍）
-- 4. 切った片の色を「大きさの順」に虹色にして、ポスターを作ってみよう

-- ---------- 道具 ----------
easeOut :: Float -> Float
easeOut t = 1 - (1 - t) * (1 - t)

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = Polygon [pt env x y, pt env (x + w) y, pt env (x + w) (y + h), pt env x (y + h)]

boxLine :: Env -> Float -> Float -> Float -> Float -> Picture
boxLine env x y w h =
  Line [pt env x y, pt env (x + w) y, pt env (x + w) (y + h), pt env x (y + h), pt env x y]

-- 点線の長方形
dashedBox :: Env -> Float -> Float -> Float -> Float -> Picture
dashedBox env x y w h = Pictures
  [ dashed (pt env x y) (pt env (x + w) y), dashed (pt env (x + w) y) (pt env (x + w) (y + h))
  , dashed (pt env (x + w) (y + h)) (pt env x (y + h)), dashed (pt env x (y + h)) (pt env x y) ]

dashed :: Point -> Point -> Picture
dashed a@(ax, ay) b@(bx, by) =
  let len = dist2 a b
      n = floor (len / 14) :: Int
      f t = (ax + (bx - ax) * t, ay + (by - ay) * t)
  in Pictures [ thickLine 2 (f (fromIntegral i * 14 / len)) (f ((fromIntegral i * 14 + 8) / len))
              | i <- [0 .. n], len > 0 ]
