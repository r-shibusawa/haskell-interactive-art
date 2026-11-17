-- ============================================
-- 48-09 周辺視ドリフト錯視（止まっている絵が回って見える）
-- 明るさの階段「黒 → 暗い灰 → 白 → 明るい灰」をくり返して並べるだけ。視線を動かしたり、まばたきすると
-- 目のはし（周辺視）で、黒 → 暗い灰 の向きに少しずつ動いて見える。並びを入れかえると止まる・逆に回る
-- 操作：1 = 黒→暗灰→白→明灰（動く）/ 2 = 黒→白→暗灰→明灰（止まる）/ 3 = 逆向き / L = 輪 ⇔ 横のしま
--   ←→ = 1 周の区切りの数 / ↑↓ = 輪の数 / T = 真実（動いていないことを画素で確かめる、明るさの並びを拡大）
-- 長く見つめて気分が悪くなったら目を離す
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import System.Environment (lookupEnv)

-- ★ 4 つの明るさ：黒 B・暗い灰 DG・白 W・明るい灰 LG
lumB, lumDG, lumW, lumLG :: Int
lumB = 0
lumDG = 90
lumW = 255
lumLG = 170

orders :: [[Int]]
orders = [ [lumB, lumDG, lumW, lumLG], [lumB, lumW, lumDG, lumLG], [lumLG, lumW, lumDG, lumB] ]

orderNames :: [String]
orderNames = ["black>dark>white>light (moves)", "black>white>dark>light (still)", "reversed (turns back)"]

bgV :: Int
bgV = 128                    -- 背景の灰色

data World = World
  { wOrder :: !Int             -- 並びの番号（0〜2）
  , wSeg   :: !Int             -- ★ 1 周（1 行）の区切りの数
  , wRings :: !Int             -- ★ 輪の数
  , wRows  :: !Bool            -- 横のしまにするか
  , wTruth :: !Bool
  , wScene :: [Shape]          -- 絵（キーを押したときだけ作り直す。動かすのは見ている人の目）
  , wPic   :: Picture
  , wIndex :: IM.IntMap [Shape] -- 画素を速く読むための索引
  , wLast  :: !(Maybe Int)     -- 前の確認のときの合計
  , wDiff  :: !Int
  , wTest  :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "48-09 Peripheral drift" (build env1 world0))
  { skBack  = g' bgV
  , skStart = \env -> do
      let w = build env world0
          rs = selfTest env w
      printSelfTest rs
      pure w { wTest = rs }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }
  where env1 = Env 1280 800 (0, 0) False 0 0 mempty

world0 :: World
world0 = World 0 20 6 False False [] Blank IM.empty Nothing 0 []

-- 中心と、絵の半径
geom :: Env -> (Float, Float, Float)
geom env = (fromIntegral (jsRound (envW env * 0.4)), fromIntegral (jsRound (envH env * 0.535)), min (envW env * 0.36) (envH env * 0.41))

-- k 番目の輪の内側・外側の半径
ringR :: Env -> World -> Int -> (Float, Float)
ringR env w k = (wd * (fromIntegral k + 0.6) + 3, wd * (fromIntegral k + 1.6) - 3)
  where (_, _, r) = geom env
        wd = r / (fromIntegral (wRings w) + 0.6)

-- 絵を作り直す
build :: Env -> World -> World
build env w = let sc = scene env w in w { wScene = sc, wPic = render sc, wIndex = indexShapes sc, wLast = Nothing }

-- ---- 図：輪（扇形の帯）または横のしま。最後に見つめる赤い点
scene :: Env -> World -> [Shape]
scene env w = (if wRows w then rowsPic else ringsPic) ++ [ Dot (255, 40, 40) cx cy 4 ]
  where
    (cx, cy, r) = geom env
    ord = getAt [] orders (wOrder w)
    seg = fromIntegral (wSeg w)
    ringsPic =
      [ Sector (g v) cx cy r0 r1 a0 a1
      | k <- [0 .. wRings w - 1]
      , let (r0, r1) = ringR env w k
            dir = if odd k then -1 else 1                -- となりの輪は逆向き
            rot = fromIntegral k * 0.37
      , s <- [0 .. wSeg w - 1], (q, v) <- zip [0 :: Int ..] ord
      , let a0 = rot + dir * tau * (fromIntegral s + fromIntegral q / 4) / seg
            a1 = rot + dir * tau * (fromIntegral s + fromIntegral (q + 1) / 4) / seg ]
    rowsPic =
      [ Box (g v) x y (bw / 4 + 0.6) (h - 6)
      | k <- [0 .. wRings w - 1]
      , let h = 2 * r / fromIntegral (wRings w)
            y = cy - r + fromIntegral k * h + 3
            right = even k
      , s <- [0 .. wSeg w - 1], (q, v) <- zip [0 :: Int ..] ord
      , let x = cx - r + fromIntegral s * bw + fromIntegral (if right then q else 3 - q) * (bw / 4) ]
    bw = 2 * r / seg

-- 絵の範囲の画素を間引いて合計（変化の確認用）
sampleSum :: Env -> IM.IntMap [Shape] -> Int
sampleSum env ix = sum [ v | y <- [cy - r, cy - r + 7 .. cy + r - 1], x <- [cx - r, cx - r + 7 .. cx + r - 1]
                           , let (v, _, _) = readPixI (g bgV) ix (jsRound x) (jsRound y) ]
  where (cx, cy, r) = geom env

-- 真実のとき：0.5 秒ごとに画素を読んで、前の確認と比べる
step :: Env -> Float -> World -> IO World
step env _ w
  | not (wTruth w) = pure w { wLast = Nothing }
  | envFrame env `mod` 30 == 0 || wLast w == Nothing =
      let s = sampleSum env (wIndex w)
      in pure w { wDiff = maybe 0 (\l -> abs (s - l)) (wLast w), wLast = Just s }
  | otherwise = pure w

draw :: Env -> World -> Picture
draw env w = view env $
  [ wPic w
  , Color (g' 235) (rect (envW env / 2) 35 (envW env) 70)
  , txt 20 30 18 (g' 20) ("1-3: " ++ getAt "" orderNames (wOrder w) ++ "   L: " ++ (if wRows w then "rows" else "rings")
                          ++ "   Left/Right: segments " ++ show (wSeg w) ++ "   Up/Down: rings " ++ show (wRings w) ++ "   T: truth")
  , txt 20 56 15 (rgb255 160 30 30) "Slowly look around the red dot, or blink. Look away if you feel unwell." ]
  ++ (if wTruth w then truthPanel else [])
  ++ [ Color (g' 235) (rect 85 (envH env - 22) 150 28)
     , txt 20 (envH env - 16) 14 (testColour (wTest w)) (selfTestLine (wTest w)) ]
  where
    px = envW env - 250
    truthPanel =
      [ Color (g' 235) (rect (px + 105) 220 250 260)
      , txt px 116 15 (g' 20) "Order of greys (1 segment)" ]
      ++ concat [ [ Color (g' v) (rect (px + fromIntegral q * 52 + 25) 175 50 90)
                  , Color (g' 60) (rectLine (px + fromIntegral q * 52 + 25) 175 50 90)
                  , txtC (px + fromIntegral q * 52 + 25) 244 15 (g' 20) (show v) ]
                | (q, v) <- zip [0 :: Int ..] (getAt [] orders (wOrder w)) ]
      ++ [ txt px 280 15 (g' 20) "Pixels read every 0.5 s,"
         , txt px 302 15 (g' 20) ("difference: " ++ show (wDiff w))
         , txt px 326 15 (if wDiff w == 0 then rgb255 30 130 60 else rgb255 200 40 40)
               (if wDiff w == 0 then "-> not one pixel moved" else "-> changed") ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "123" -> build env w { wOrder = fromEnum c - fromEnum '1' }
    | c `elem` "lL" -> build env w { wRows = not (wRows w) }
    | c `elem` "tT" -> w { wTruth = not (wTruth w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> build env w { wSeg = min 48 (wSeg w + 2) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> build env w { wSeg = max 6 (wSeg w - 2) }
  EventKey (SpecialKey KeyUp) Down _ _ -> build env w { wRings = min 10 (wRings w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> build env w { wRings = max 1 (wRings w - 1) }
  EventResize _ -> build env w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 1 と 2 を切りかえる。同じ 4 つの明るさなのに、並びだけで動いたり止まったりする
-- 2. 3（逆の並び）で回る向きが逆になる？
-- 3. lumDG（暗い灰）を 0（黒と同じ）にすると？ 階段が 3 段になると弱くなる？
-- 4. L で横のしまにすると、行ごとに左右に流れて見える
-- 5. 画面に近づく・離れる。区切りの大きさ（←→）で強さは変わる？

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃えの文字
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y size c s = txt (x - fromIntegral (length s) * size * 0.36) y size c s

-- 灰色（0〜255）
g :: Int -> RGB
g v = (v, v, v)

g' :: Int -> Color
g' v = rgb255 v v v

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- JavaScript の Math.round と同じ（0.5 は上へ）
jsRound :: Float -> Int
jsRound x = floor (x + 0.5)

-- ==================================================
-- ここから下は「錯視の道具」（画素を読む・自己テスト。中身は読まなくても OK。消さないでね）
-- ==================================================
-- ---- 図は「形のリスト」で作る。同じリストから、画面の絵（render）と「画素を読む」関数（readPix）の両方を作る
--   座標は p5.js と同じ（左上が原点、y は下向き）。色は 0〜255 の整数なので、ぴったり同じ値かどうかを比べられる
type RGB = (Int, Int, Int)

data Shape
  = Box !RGB !Float !Float !Float !Float                  -- 左上 (x, y)・幅・高さ の長方形
  | Dot !RGB !Float !Float !Float                         -- 中心 (x, y)・半径 の円
  | Sector !RGB !Float !Float !Float !Float !Float !Float -- 扇形の帯：中心 (x, y)・半径 r0〜r1・角度 a0〜a1

-- 形のリスト → 画面の絵
render :: [Shape] -> Picture
render = Pictures . map one
  where
    one sh = case sh of
      Box c x y w h -> Color (col c) (rect (x + w / 2) (y + h / 2) w h)
      Dot c x y r -> Color (col c) (at (x, y) (disc r))
      Sector c x y r0 r1 a0 a1 ->
        -- 細い四角形に分けて塗る（gloss の Polygon は凸の形だけ正しく塗れる）
        let n = max 1 (ceiling (abs (a1 - a0) * 180 / pi / 3)) :: Int
            ang i = a0 + (a1 - a0) * fromIntegral i / fromIntegral n
            pt r a = (x + r * cos a, y + r * sin a)
        in Color (col c) (Pictures [ Polygon [pt r0 (ang i), pt r1 (ang i), pt r1 (ang (i + 1)), pt r0 (ang (i + 1))] | i <- [0 .. n - 1] ])
    col (r, gg, b) = rgb255 r gg b

-- 形が点 p をおおっていれば、その色
covers :: Shape -> Point -> Maybe RGB
covers sh p@(px, py) = case sh of
  Box c x y w h -> hit c (px >= x && px < x + w && py >= y && py < y + h)
  Dot c x y r -> hit c (dist2 p (x, y) <= r)
  Sector c x y r0 r1 a0 a1 ->
    let dx = px - x
        dy = py - y
        d2 = dx * dx + dy * dy
    in if d2 < r0 * r0 || d2 > r1 * r1 then Nothing
       else hit c (wrapF tau (atan2 dy dx - min a0 a1) < abs (a1 - a0))
  where hit c ok = if ok then Just c else Nothing

-- 背景 bg の上に形を順に重ねたときの、点 p の色
colourAt :: RGB -> [Shape] -> Point -> RGB
colourAt bg shs p = foldl' (\d sh -> maybe d id (covers sh p)) bg shs

-- 画素 (x, y) の色（画素の真ん中を読む。p5.js の readPix）
readPix :: RGB -> [Shape] -> Int -> Int -> RGB
readPix bg shs x y = colourAt bg shs (fromIntegral x + 0.5, fromIntegral y + 0.5)

-- ---- 索引：画面を 16 px のます目に分け、ます目ごとに「そこにかかる形」を描く順に覚える
--   readPixI は、その点のます目の形だけを調べる（readPix と同じ答えで、ずっと速い）
cellOf :: Int -> Int -> Int
cellOf cx cy = cy * 4096 + cx

indexShapes :: [Shape] -> IM.IntMap [Shape]
indexShapes shs = IM.map reverse (foldl' add IM.empty shs)
  where
    add m sh =
      let (x0, y0, x1, y1) = bbox sh
          cs = [ cellOf i j | j <- [cl y0 .. cl y1], i <- [cl x0 .. cl x1] ]
      in foldl' (\mm c -> IM.insertWith (++) c [sh] mm) m cs
    cl v = max 0 (min 4095 (floor (v / 16)))

-- 形がはいる長方形（左, 上, 右, 下）
bbox :: Shape -> (Float, Float, Float, Float)
bbox sh = case sh of
  Box _ x y w h -> (x, y, x + w, y + h)
  Dot _ x y r -> (x - r, y - r, x + r, y + r)
  Sector _ x y r0 r1 a0 a1 ->
    let ps = [ (x + r * cos a, y + r * sin a) | r <- [r0, r1], i <- [0 .. 16 :: Int], let a = a0 + (a1 - a0) * fromIntegral i / 16 ]
    in (minimum (map fst ps) - 1, minimum (map snd ps) - 1, maximum (map fst ps) + 1, maximum (map snd ps) + 1)

readPixI :: RGB -> IM.IntMap [Shape] -> Int -> Int -> RGB
readPixI bg ix x y = readPix bg (IM.findWithDefault [] (cellOf (c x) (c y)) ix) x y
  where c v = max 0 (min 4095 (v `div` 16))

-- ---- 自己テスト：描いた輪の画素を読んで、明るさの並びと「動いていない」ことを確かめる
selfTest :: Env -> World -> [(String, Bool)]
selfTest env w =
  [ ("inner ring: pixel order " ++ arrows (readSeq sc0 0) ++ " = black>dark>white>light", readSeq sc0 0 == want)
  , ("next ring has the same order (angle runs backwards) " ++ arrows (readSeq sc0 1), readSeq sc0 1 == want)
  , ("drawn again, not a single pixel changes (still picture)", s1 == s2)
  , ("levels used: 0, 90, 170, 255 and background 128 all appear", all (`S.member` lv) [0, 90, 170, 255, 128])
  , ("order 2 (still) is black>white>dark>light: " ++ arrows (readSeq sc1 0), readSeq sc1 0 == getAt [] orders 1) ]
  where
    (cx, cy, r) = geom env
    w0 = w { wOrder = 0, wRows = False }
    sc0 = scene env w0
    ix0' = wIndex (build env (build env w0 { wOrder = 2 }) { wOrder = 0 })   -- もう一度、別に作った絵
    sc1 = scene env w0 { wOrder = 1 }
    want = getAt [] orders 0
    s1 = sampleSum env (indexShapes sc0)
    s2 = sampleSum env ix0'
    -- k 番目の輪の 1 区切り目の 4 つの明るさ（帯の真ん中）
    readSeq sc k =
      let (r0, r1) = ringR env w0 k
          rr = (r0 + r1) / 2
          dir = if odd k then -1 else 1
          rot = fromIntegral k * 0.37
      in [ v | q <- [0 .. 3 :: Int]
             , let a = rot + dir * tau * (fromIntegral q + 0.5) / 4 / fromIntegral (wSeg w0)
                   (v, _, _) = readPix (g bgV) sc (jsRound (cx + rr * cos a)) (jsRound (cy + rr * sin a)) ]
    lv = S.fromList [ a | y <- [cy - r, cy - r + 5 .. cy + r - 1], x <- [cx - r, cx - r + 5 .. cx + r - 1]
                        , let (a, b, c) = readPix (g bgV) sc0 (jsRound x) (jsRound y), a == b && b == c ]
    arrows = foldr1' (\a b -> a ++ ">" ++ b) . map show
    foldr1' _ [] = ""
    foldr1' f xs = foldr1 f xs

-- 結果を端末に出す（テスト中は 1 行のまとめと失敗した項目だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(s, ok) -> putStrLn ((if ok then "OK  " else "NG  ") ++ s)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if not (null rs) && all snd rs then "OK " else "NG ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

testColour :: [(String, Bool)] -> Color
testColour rs = if not (null rs) && all snd rs then rgb255 30 130 60 else rgb255 200 40 40
