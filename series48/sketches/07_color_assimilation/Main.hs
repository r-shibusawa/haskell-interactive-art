-- ============================================
-- 48-07 色の同化と対比（ムンカー型の原理を使ったオリジナルの図）
-- 左右の円はまったく同じ色。左は「黄のしま」、右は「青のしま」が円の上を横切っている
-- 細いしまでは、円の色がしまの色に近づいて見える（同化）。太いしまでは逆に遠ざかることも（対比）
-- 操作：1〜3 = 色の組 / ↑↓ = しまの太さ / マウスを動かす = スポイト（その画素の本当の RGB）
--   T またはマウスを押している間 = 真実を見る（しまを外して、円をそのまま見せる）
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import System.Environment (lookupEnv)

-- 色の組：しまの色 p・q と、円の色 c
data ColourSet = ColourSet { sP :: RGB, sQ :: RGB, sC :: RGB, sName :: String }

sets :: [ColourSet]
sets =                                            -- ★ 自分の色の組を足してみよう
  [ ColourSet (30, 70, 230) (250, 215, 30) (225, 70, 60) "red disc, blue/yellow stripes"
  , ColourSet (40, 170, 90) (200, 40, 170) (240, 160, 40) "orange disc, green/purple stripes"
  , ColourSet (0, 0, 0) (255, 255, 255) (128, 128, 128) "grey disc, black/white stripes" ]

back :: RGB
back = (235, 235, 235)

data World = World
  { wSet   :: !Int             -- 色の組の番号（0〜2）
  , wSH    :: !Int             -- ★ しまの太さ px
  , wTruth :: !Bool
  , wTest  :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "48-07 Colour assimilation" world0)
  { skBack  = col back
  , skStart = \env -> do
      let rs = selfTest env world0
      printSelfTest rs
      pure world0 { wTest = rs }
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 0 8 False []

setOf :: World -> ColourSet
setOf w = getAt (head' sets) sets (wSet w)
  where head' (s : _) = s
        head' [] = ColourSet (0, 0, 0) (255, 255, 255) (128, 128, 128) ""

-- 2 つのパネルの位置と、円の中心・半径
data Panels = Panels { pw, ph, py0, pxL, pxR, pr :: !Int }

panels :: Env -> Panels
panels env = Panels pw' ph' 90 (jsRound (envW env / 2 - fromIntegral pw' - 20)) (jsRound (envW env / 2 + 20)) (jsRound (fromIntegral ph' * 0.3))
  where
    pw' = jsRound (min (envW env * 0.42) ((envH env - 160) * 1.1))
    ph' = jsRound (min (envH env - 170) (fromIntegral pw'))

-- ---- 図：しま（両方の色）→ 同じ色の円 → 円の上に片方の色のしま（左は q、右は p）
scene :: Env -> World -> Bool -> [Shape]
scene env w truth = concat
  [ stripes w x0 (py0 p) (pw p) (ph p) Nothing
    ++ [ Dot (sC s) (fromIntegral (x0 + jsRound (fromIntegral (pw p) / 2))) (fromIntegral (py0 p + jsRound (fromIntegral (ph p) / 2))) (fromIntegral (pr p)) ]
    ++ (if truth then [] else stripes w x0 (py0 p) (pw p) (ph p) (Just (k == 1)))
  | (k, x0) <- [(0 :: Int, pxL p), (1, pxR p)] ]
  where
    p = panels env
    s = setOf w

-- しま模様：which = Nothing なら両方、Just True なら p のしまだけ、Just False なら q のしまだけ
stripes :: World -> Int -> Int -> Int -> Int -> Maybe Bool -> [Shape]
stripes w x0 y0 wd ht which =
  [ Box (if isP then sP s else sQ s) (fromIntegral x0) (fromIntegral y) (fromIntegral wd) (fromIntegral (min sh (y0 + ht - y)))
  | (k, y) <- zip [0 :: Int ..] [y0, y0 + sh .. y0 + ht - 1]
  , let isP = even k
  , maybe True (== isP) which ]
  where
    s = setOf w
    sh = max 1 (wSH w)

draw :: Env -> World -> Picture
draw env w = view env $
  [ render sc
  , txt 20 30 18 (g' 20) ("1-3: " ++ sName s ++ "   Up/Down: stripe width " ++ show (wSH w) ++ " px   Mouse: eyedropper   T: truth (no stripes)")
  , txt 20 56 15 (g' 70) ("Both discs are RGB " ++ showRGB (sC s) ++ ". Move the eyedropper over them to check.")
  , txt 20 (envH env - 16) 14 (testColour (wTest w)) (selfTestLine (wTest w)) ]
  ++ dropper
  where
    truth = wTruth w || envDown env
    sc = scene env w truth
    s = setOf w
    (gx, gy) = envMouse env
    mx = gx + envW env / 2
    my = envH env / 2 - gy
    -- スポイト：マウスの下の画素の色（形のリストから読む）を拡大して表示
    dropper
      | mx < 0 || my < 0 || mx >= envW env || my >= envH env = []
      | otherwise =
          let c@(r, gg, b) = readPix back sc (floor mx) (floor my)
              x = envW env - 230
              y = envH env - 110
          in [ Color black (Pictures [ Line [(mx - 10, my), (mx - 3, my)], Line [(mx + 3, my), (mx + 10, my)]
                                     , Line [(mx, my - 10), (mx, my - 3)], Line [(mx, my + 3), (mx, my + 10)] ])
             , Color white (rect (x + 100) (y + 35) 220 90)
             , Color (col c) (rect (x + 35) (y + 35) 70 70)
             , txt (x + 80) (y + 15) 15 (g' 20) "Eyedropper"
             , txt (x + 80) (y + 39) 15 (g' 20) ("R " ++ show r ++ "  G " ++ show gg)
             , txt (x + 80) (y + 61) 15 (g' 20) ("B " ++ show b) ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "123" -> w { wSet = fromEnum c - fromEnum '1' }
    | c `elem` "tT" -> w { wTruth = not (wTruth w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wSH = min 60 (wSH w + 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wSH = max 2 (wSH w - 2) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. しまの太さを 2 px・8 px・40 px にして、左右の円の見え方の差が大きいのはどれ？
-- 2. 3（灰色の円）でも同じことが起きる？ 色だけでなく明るさの同化
-- 3. 画面から離れて見ると（しまが細かく見えると）錯視は強くなる？
-- 4. sets に自分の色の組を足そう：円の色 c を、p と q の中間の色にすると効きやすい
-- 5. スポイトで円の上の 2 か所をなぞる。見た目の色と RGB の数字はちがう？

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

g' :: Int -> Color
g' v = rgb255 v v v

col :: RGB -> Color
col (r, gg, b) = rgb255 r gg b

showRGB :: RGB -> String
showRGB (r, gg, b) = "(" ++ show r ++ ", " ++ show gg ++ ", " ++ show b ++ ")"

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
  = Box !RGB !Float !Float !Float !Float          -- 左上 (x, y)・幅・高さ の長方形
  | Dot !RGB !Float !Float !Float                 -- 中心 (x, y)・半径 の円

-- 形のリスト → 画面の絵
render :: [Shape] -> Picture
render = Pictures . map one
  where
    one sh = case sh of
      Box c x y w h -> Color (col c) (rect (x + w / 2) (y + h / 2) w h)
      Dot c x y r -> Color (col c) (at (x, y) (disc r))

-- 形が点 p をおおっていれば、その色
covers :: Shape -> Point -> Maybe RGB
covers sh p@(px, py) = case sh of
  Box c x y w h -> hit c (px >= x && px < x + w && py >= y && py < y + h)
  Dot c x y r -> hit c (dist2 p (x, y) <= r)
  where hit c ok = if ok then Just c else Nothing

-- 背景 bg の上に形を順に重ねたときの、点 p の色
colourAt :: RGB -> [Shape] -> Point -> RGB
colourAt bg shs p = foldl' (\d sh -> maybe d id (covers sh p)) bg shs

-- 画素 (x, y) の色（画素の真ん中を読む。p5.js の readPix）
readPix :: RGB -> [Shape] -> Int -> Int -> RGB
readPix bg shs x y = colourAt bg shs (fromIntegral x + 0.5, fromIntegral y + 0.5)

-- ---- 自己テスト：3 つの色の組で、左右の円の見えている部分の画素を読み、同じ RGB かを確かめる
selfTest :: Env -> World -> [(String, Bool)]
selfTest env w = concat
  [ [ ("set " ++ show (k + 1) ++ ": left disc " ++ showRGB a ++ " = right disc " ++ showRGB b ++ " = c", a == sC s && b == sC s)
    , ("set " ++ show (k + 1) ++ ": stripes crossing the disc are q on the left, p on the right", a2 == sQ s && b2 == sP s) ]
  | (k, s) <- zip [0 :: Int ..] sets
  , let sc = scene env w { wSet = k } False
        a = readPix back sc xl (rowOf kP)        -- 左は p の行、右は q の行で円が見えている
        b = readPix back sc xr (rowOf kQ)
        a2 = readPix back sc xl (rowOf kQ)
        b2 = readPix back sc xr (rowOf kP) ]
  where
    p = panels env
    sh = wSH w
    cy = py0 p + jsRound (fromIntegral (ph p) / 2)
    kc = (cy - py0 p) `div` sh
    rowOf k = py0 p + k * sh + sh `div` 2       -- k 番目のしまの真ん中の行
    kP = if even kc then kc else kc + 1         -- 中心の近くの p のしま・q のしま
    kQ = kP + 1
    xl = pxL p + jsRound (fromIntegral (pw p) / 2)
    xr = pxR p + jsRound (fromIntegral (pw p) / 2)

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
