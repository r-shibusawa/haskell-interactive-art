-- ============================================
-- 06 活動選択問題：1 つの部屋に、重ならない予定をできるだけ多く入れる
-- 貪欲法：「いちばん早く終わる予定」を選び、それと重ならない中でまた一番早く終わるものを…
-- 早く終われば残り時間が一番多く残る → 一度選んだら後悔しない。ほかのルールと比べてみよう
-- スペース = 再生・停止 / → = 1 手 / 1〜4 = 選ぶルール / ドラッグ = 予定を追加 / R = ランダム / C = お手本
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (foldl', sortOn)
import Text.Printf (printf)

sampleActs :: [(Int, Int)]
sampleActs = zip [0, 1, 4, 2, 3, 6, 5, 8, 9, 11, 13] [3, 4, 6, 6, 8, 9, 10, 11, 13, 15, 16]  -- ★ (始まり, 終わり)。最適は 5 個

framesPerMove :: Int
framesPerMove = 30      -- ★ 1 手の間のフレーム数

data World = World
  { wActs    :: ![(Int, Int)]
  , wT       :: !Int           -- 時間の長さ
  , wRule    :: !Int
  , wOrder   :: ![Int]
  , wPos     :: !Int
  , wChosen  :: ![Int]
  , wLastF   :: !Int
  , wPlaying :: !Bool
  , wTimer   :: !Int
  , wDrag    :: !(Maybe Float) -- ドラッグを始めた x
  }

initial :: World
initial = restart (World sampleActs 17 1 [] 0 [] 0 True 0 Nothing)

main :: IO ()
main = runSketch (sketch "12-06 Activity selection" initial)
  { skBack  = cBg
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

actOf :: World -> Int -> (Int, Int)
actOf w = nth (0, 0) (wActs w)

-- ★ ルールごとに、予定を調べる順番を決める
restart :: World -> World
restart w = w { wOrder = order, wPos = 0, wChosen = [], wLastF = 0 }
  where
    acts = wActs w
    idx = [0 .. length acts - 1]
    conflicts k =
      let (s, f) = actOf w k
      in length [() | (j, (bs, bf)) <- zip idx acts, j /= k, bs < f, s < bf]
    order = case wRule w of
      1 -> sortOn (snd . actOf w) idx
      2 -> sortOn (fst . actOf w) idx
      3 -> sortOn (\k -> let (s, f) = actOf w k in f - s) idx
      _ -> sortOn conflicts idx

-- 1 手：次の予定を、これまで選んだものと重ならなければ選ぶ
advance :: World -> World
advance w = case drop (wPos w) (wOrder w) of
  [] -> w
  (k : _) ->
    let (s, f) = actOf w k
        ok = all (\c -> let (cs, cf) = actOf w c in cf <= s || f <= cs) (wChosen w)
    in if ok then w { wChosen = wChosen w ++ [k], wLastF = f, wPos = wPos w + 1 }
             else w { wPos = wPos w + 1 }

-- 正しい貪欲法で選べる数（比較用）
bestCount :: World -> Int
bestCount w = fst (foldl' pick (0, minBound) (sortOn snd (wActs w)))
  where pick (cnt, f) (s, e) | s >= f = (cnt + 1 :: Int, e)
                             | otherwise = (cnt, f)

step :: World -> World
step w
  | wPlaying w && wPos w < length (wOrder w) =
      if wTimer w + 1 >= framesPerMove then advance w { wTimer = 0 } else w { wTimer = wTimer w + 1 }
  | otherwise = w

timeLeft, timeRight :: Env -> Float
timeLeft _ = 60
timeRight env = envW env - 300

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (advance w)
  EventKey (Char c) Down _ _
    | c `elem` "1234" -> pure (restart w { wRule = fromEnum c - fromEnum '0' })
    | c `elem` "cC" -> pure (restart w { wActs = sampleActs, wT = 17 })
    | c `elem` "rR" -> do
        let t = 24
        acts <- replicateM 14 $ do
          s <- randomRIO (0, t - 3)
          d <- randomRIO (1, 6)
          pure (s, min t (s + d))
        pure (restart w { wActs = acts, wT = t }) { wPlaying = True }
  EventKey (MouseButton LeftButton) Down _ _
    | mx > timeLeft env && mx < timeRight env -> pure w { wDrag = Just mx }
  EventKey (MouseButton LeftButton) Up _ _ -> pure $ case wDrag w of
    Nothing -> w
    Just d ->
      let toTime x = remap x (timeLeft env) (timeRight env) 0 (i2f (wT w))
          lim = max 0 . min (wT w)
          a = lim (round (toTime (min d mx)))
          b = lim (round (toTime (max d mx)))
      in if b > a && length (wActs w) < 24
           then (restart w { wActs = wActs w ++ [(a, b)] }) { wDrag = Nothing }
           else w { wDrag = Nothing }
  _ -> pure w
  where (mx, _) = mouseP env

ruleName :: Int -> String
ruleName r = case r of
  1 -> "earliest finish (correct greedy)"
  2 -> "earliest start"
  3 -> "shortest first"
  _ -> "fewest overlaps"

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env
  ( [ txt 26 cFg 32 24 ("Activity selection: rule " ++ show (wRule w) ++ " - " ++ ruleName (wRule w))
    , txt 15 cMuted 32 60 "Space: play/pause   Right: 1 step   1-4: rule   Drag: add an activity   R: random   C: sample" ]
  ++ grid ++ rows ++ band ++ dragBar ++ panel )
  where
    wd = envW env
    ht = envH env
    x0 = timeLeft env
    x1 = timeRight env
    top = 120
    nActs = length (wActs w)
    rowH = min 36 ((ht - top - 120) / i2f (max 1 nActs))
    tx :: Int -> Float
    tx t = remap (i2f t) 0 (i2f (wT w)) x0 x1
    -- 時間軸
    grid = concat
      [ [ seg cGrid 1 (tx t, top - 10) (tx t, top + rowH * i2f nActs)
        , txtC 14 cMuted (tx t) (top - 22) (show t) ]
      | t <- [0 .. wT w] ]
    -- 予定（並べる順＝ルールの順）
    rows = concat
      [ [ rbox col (tx s) (y + 4) barW (rowH - 8) ]
        ++ [ if fits then txtL 14 (if isChosen || cur then cBg else cFg) (tx s + 6) (y + rowH / 2) name
                     else txtL 14 cMuted (tx f + 6) (y + rowH / 2) name ]
        ++ [seg cRasp 2 (tx s, y + rowH / 2) (tx f, y + rowH / 2) | looked && not isChosen]
      | (r, k) <- zip [0 ..] (wOrder w)
      , let (s, f) = actOf w k
      , let y = top + i2f r * rowH
      , let isChosen = k `elem` wChosen w
      , let looked = r < wPos w
      , let cur = r == wPos w
      , let col | isChosen = cLime
                | looked = rgb255 0x4a 0x3d 0x56
                | cur = cAmber
                | otherwise = rgb255 0x6b 0x5a 0x78
      , let barW = tx f - tx s
      , let name = "a" ++ show (k + 1) ++ " " ++ show s ++ "-" ++ show f
      , let fits = barW > i2f (length name) * 12 + 8 ]
    -- 選ばれた予定を 1 本の帯に
    by = top + rowH * i2f nActs + 20
    band =
      rbox cPanel x0 by (x1 - x0) 34
      : [rbox cLime (tx s + 1) (by + 4) (tx f - tx s - 2) 26 | k <- wChosen w, let (s, f) = actOf w k]
      ++ [seg cAmber 2 (tx (wLastF w), top - 10) (tx (wLastF w), by + 34) | wRule w == 1 && wPos w < length (wOrder w)]
    -- ドラッグ中
    (mx, _) = mouseP env
    dragBar = [rbox cCyan (min d mx) (by + 44) (abs (mx - d)) 20 | Just d <- [wDrag w]]
    -- 右パネル
    px = wd - 270
    note = if wRule w == 1
      then ["Finishing early leaves", "the most time left.", "It is always part of", "some best answer."]
      else ["Drag to build a case", "where this rule is", "not optimal."]
    panel =
      [ rbox cPanel px 110 240 260
      , txt 17 cFg (px + 16) 126 ("Chosen  " ++ show (length (wChosen w)))
      , txt 14 cMuted (px + 16) 156 ("Best possible  " ++ show (bestCount w))
      , txt 14 cMuted (px + 16) 182 ("Checked  " ++ show (wPos w) ++ " / " ++ show (length (wOrder w))) ]
      ++ [txt 14 cFg (px + 16) (220 + i2f l * 22) s | (l, s) <- zip [0 :: Int ..] note]

-- ---------- やってみよう ----------
-- 1. 「短い順」（3）が最適にならない予定を、ドラッグで作る
-- 2. 「重なりが少ない順」（4）でも失敗する例を探す（少し難しい）
-- 3. sampleActs を自分の 1 日の予定に書きかえる

-- ---------- 道具：色と、左上が原点の座標で描く関数（p5.js の座標のまま書ける） ----------

cBg, cPanel, cFg, cMuted, cRasp, cCyan, cAmber, cLime, cGrid :: Color
cBg    = rgb255 0x16 0x11 0x1c
cPanel = rgb255 0x23 0x1a 0x2b
cFg    = rgb255 0xf3 0xea 0xf0
cMuted = rgb255 0x9a 0x8a 0xa3
cRasp  = rgb255 0xff 0x6b 0x8b
cCyan  = rgb255 0x5e 0xe6 0xe0
cAmber = rgb255 0xff 0xc8 0x57
cLime  = rgb255 0xa3 0xe6 0x35
cGrid  = rgb255 0x3a 0x2f 0x44

-- | 画面全体を「左上が原点」の座標に移す（中の図形は y を下向きで渡す）
screen :: Env -> [Picture] -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Pictures

-- | マウスの位置を左上原点の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- | 角を落とした四角の頂点（p5.js の rect の角丸の代わり）
cornerPts :: Float -> Float -> Float -> Float -> [Point]
cornerPts x y w h =
  let r = min 5 (min (abs w) (abs h) / 3)
  in [ (x + r, -y), (x + w - r, -y), (x + w, -y - r), (x + w, -y - h + r)
     , (x + w - r, -y - h), (x + r, -y - h), (x, -y - h + r), (x, -y - r) ]

-- | 角を落とした塗りつぶしの四角
rbox :: Color -> Float -> Float -> Float -> Float -> Picture
rbox c x y w h = Color c (Polygon (cornerPts x y w h))

-- | 線分（太さ t）
seg :: Color -> Float -> Point -> Point -> Picture
seg c t (x1, y1) (x2, y2)
  | t <= 1.2  = Color c (Line [(x1, -y1), (x2, -y2)])
  | otherwise = Color c (thickLine t (x1, -y1) (x2, -y2))

-- | 文字：(x, y) を左上にして書く
txt :: Float -> Color -> Float -> Float -> String -> Picture
txt size c x y = label x (-y - size) size c

-- | 文字：(x, y) を左端の真ん中にして書く
txtL :: Float -> Color -> Float -> Float -> String -> Picture
txtL size c x y = label x (-y - size / 2) size c

-- | 文字：(x, y) を中心にして書く
txtC :: Float -> Color -> Float -> Float -> String -> Picture
txtC size c x y s = label (x - fromIntegral (length s) * size * 0.4) (-y - size / 2) size c s

-- | リストの i 番目（範囲外なら d）
nth :: a -> [a] -> Int -> a
nth d xs i
  | i < 0 = d
  | otherwise = case drop i xs of
      (x : _) -> x
      [] -> d

-- | 整数を小数に
i2f :: Int -> Float
i2f = fromIntegral
