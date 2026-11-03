-- ============================================
-- 09 締め切りと罰金：マトロイドと貪欲法
-- 1 時間で終わる仕事がいくつもあり、締め切りに遅れると罰金。罰金の合計を最小にする並べ方は？
-- 罰金の大きい順に「まだ全部締め切りに間に合うなら採用」。この「間に合う仕事の集合」はマトロイドになるので、貪欲法で最適
-- スペース = 再生・停止 / → = 1 手 / 1 = お手本 / R = ランダム / 仕事をクリック = 罰金 +10（Shift で -10）
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (sortOn)
import qualified Data.IntMap.Strict as IM
import Text.Printf (printf)

sampleTasks :: [(Int, Int)]
sampleTasks = zip [3, 1, 4, 1, 2, 3, 2] [60, 50, 45, 40, 30, 25, 15]   -- ★ (締め切り, 罰金)。a4・a6・a7 が遅刻組 → 罰金 80

framesPerMove :: Int
framesPerMove = 45      -- ★ 1 手の間のフレーム数

data World = World
  { wTasks    :: ![(Int, Int)]
  , wOrder    :: ![Int]
  , wPos      :: !Int
  , wAccepted :: ![Int]
  , wRejected :: ![Int]
  , wSlots    :: !(IM.IntMap Int)   -- 時間割：t 番目の枠に入る仕事
  , wPlaying  :: !Bool
  , wTimer    :: !Int
  , wMsg      :: !String
  }

initial :: World
initial = restart (World sampleTasks [] 0 [] [] IM.empty True 0 "")

main :: IO ()
main = runSketch (sketch "12-09 Deadlines and penalties" initial)
  { skBack  = cBg
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

task :: World -> Int -> (Int, Int)
task w = nth (1, 0) (wTasks w)

restart :: World -> World
restart w = w
  { wOrder = sortOn (negate . snd . task w) [0 .. length (wTasks w) - 1]
  , wPos = 0, wAccepted = [], wRejected = [], wMsg = "", wSlots = IM.empty }

-- ★ 独立性のチェック：締め切り t までの仕事の数 N_t(A) が t 以下なら、全部締め切りに間に合う
independent :: World -> [Int] -> Bool
independent w set =
  and [ length [k | k <- set, fst (task w k) <= t] <= t | t <- [1 .. length (wTasks w)] ]

advance :: World -> World
advance w = case drop (wPos w) (wOrder w) of
  [] -> w
  (k : _) ->
    let (_, pen) = task w k
        name = "a" ++ show (k + 1) ++ " (penalty " ++ show pen ++ ")"
        ok = independent w (wAccepted w ++ [k])
        acc = if ok then wAccepted w ++ [k] else wAccepted w
        rej = if ok then wRejected w else wRejected w ++ [k]
        -- 採用した仕事を締め切り順に並べる（これが早い者勝ちの時間割）。残りは後ろで遅刻
        early = sortOn (fst . task w) acc
    in w { wAccepted = acc, wRejected = rej, wPos = wPos w + 1
         , wSlots = IM.fromList (zip [1 ..] (early ++ rej))
         , wMsg = name ++ (if ok then " accepted: everyone is still on time"
                                 else " would make someone late, so it goes to the late group") }

step :: World -> World
step w
  | wPlaying w && wPos w < length (wOrder w) =
      if wTimer w + 1 >= framesPerMove then advance w { wTimer = 0 } else w { wTimer = wTimer w + 1 }
  | otherwise = w

cardW :: Env -> World -> Float
cardW env w = min 110 ((envW env - 64) / i2f (max 1 (length (wTasks w))))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPlaying = not (wPlaying w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (advance w)
  EventKey (Char '1') Down _ _ -> pure (restart w { wTasks = sampleTasks })
  EventKey (Char c) Down _ _ | c `elem` "rR" -> do
    n <- randomRIO (5, 9)
    ts <- replicateM n ((,) <$> randomRIO (1, n - 1) <*> ((* 10) <$> randomRIO (1, 9)))
    pure (restart w { wTasks = ts }) { wPlaying = True }
  EventKey (MouseButton LeftButton) Down m _ ->
    let (mx, my) = mouseP env
        cw = cardW env w
        hit = [k | (r, k) <- zip [0 :: Int ..] (wOrder w), let x = 32 + i2f r * cw, mx > x, mx < x + cw, my > 100, my < 180]
    in pure $ case hit of
         (k : _) ->
           let change (d, p) = (d, max 10 (p + (if shift m == Down then -10 else 10)))
               ts = [if i == k then change t else t | (i, t) <- zip [0 ..] (wTasks w)]
           in restart w { wTasks = ts }
         [] -> w { wPlaying = not (wPlaying w) }
  _ -> pure w

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env
  ( [ txt 26 cFg 32 24 "Unit tasks with deadlines: minimize the total penalty"
    , txt 15 cMuted 32 60 "Space: play/pause   Right: 1 step   1: sample   R: random   Click task: +10 (Shift -10)" ]
  ++ cards ++ slots ++ graph ++ panel )
  where
    wd = envW env
    ht = envH env
    n = length (wTasks w)
    -- 仕事のカード（罰金の大きい順）
    cw = cardW env w
    cards = concat
      [ [ rbox (case st of { 0 -> cLime; 1 -> cRasp; 2 -> cAmber; _ -> cPanel }) (x + 4) 100 (cw - 8) 80
        , txtC 16 ink (x + cw / 2) 116 ("a" ++ show (k + 1))
        , txtC 14 ink (x + cw / 2) 140 ("due " ++ show d)
        , txtC 14 ink (x + cw / 2) 162 ("pen " ++ show p) ]
      | (r, k) <- zip [0 :: Int ..] (wOrder w)
      , let x = 32 + i2f r * cw
      , let (d, p) = task w k
      , let st | k `elem` wAccepted w = 0 :: Int
               | k `elem` wRejected w = 1
               | r == wPos w = 2
               | otherwise = 3
      , let ink = if st == 3 then cFg else cBg ]
    -- 時間割
    sy = 230
    sw = min 110 (wd * 0.62 / i2f (max 1 n))
    slots =
      txt 16 cFg 32 (sy - 26) "Schedule: accepted tasks by deadline, the rest come late"
      : concat
      [ [ cellBox cPanel cGrid 1 (x + 2) sy (sw - 4) 70
        , txtC 14 cMuted (x + sw / 2) (sy + 84) (show (t - 1) ++ "-" ++ show t) ]
        ++ concat
           [ [ rbox (if late then cRasp else cLime) (x + 6) (sy + 6) (sw - 12) 58
             , txtC 15 cBg (x + sw / 2) (sy + 22) ("a" ++ show (k + 1))
             , txtC 14 cBg (x + sw / 2) (sy + 46) (if late then "-" ++ show p else "due " ++ show d) ]
           | Just k <- [IM.lookup t (wSlots w)]
           , let (d, p) = task w k
           , let late = t > d ]
      | t <- [1 .. n], let x = 32 + i2f (t - 1) * sw ]
    -- N_t(A) ≤ t のグラフ
    gx = 32
    gy = sy + 130
    gh = min 180 (ht - gy - 90)
    gw = sw * i2f n
    unitH = gh / i2f (max 1 n)
    colW = gw / i2f (max 1 n)
    graph =
      txt 16 cFg gx (gy - 26) "Independence check: accepted tasks due by t, N_t <= t"
      : concat
      [ [ rbox cGrid (x + 6) (gy + gh - i2f t * unitH) (colW - 12) (i2f t * unitH)
        , rbox (if cnt > t then cRasp else cCyan) (x + 14) (gy + gh - i2f cnt * unitH) (colW - 28) (i2f cnt * unitH)
        , txtC 14 cMuted (x + colW / 2) (gy + gh + 12) ("t=" ++ show t) ]
      | t <- [1 .. n]
      , let x = gx + i2f (t - 1) * colW
      , let cnt = length [k | k <- wAccepted w, fst (task w k) <= t] ]
      ++ [txt 14 cMuted gx (gy + gh + 28) "gray = limit t,  cyan = N_t"]
    -- 右パネル
    px = wd * 0.68
    pw = wd - px - 30
    total = sum [snd (task w k) | k <- wRejected w]
    matroid =
      [ "Two matroid properties", "1. Remove a task from an on-time set:"
      , "   it is still on time.", "2. A smaller set can borrow a task"
      , "   from a larger one (exchange).", "So greedy by weight is optimal." ]
    panel =
      [ rbox cPanel px 210 pw 310
      , txt 16 cFg (px + 16) 226 ("Total penalty  " ++ show total) ]
      ++ [txt 14 cMuted (px + 16) (256 + i2f l * 20) s | (l, s) <- zip [0 :: Int ..] (wrapText 30 (wMsg w))]
      ++ [txt 14 cFg (px + 16) (350 + i2f l * 24) s | (l, s) <- zip [0 :: Int ..] matroid]

-- ---------- やってみよう ----------
-- 1. カードをクリックして罰金を変え、遅刻組が入れかわるのを見る
-- 2. sampleTasks の締め切りを全部 1 にすると、どうなる？
-- 3. advance で「罰金の小さい順」に調べたら、合計はどれだけ増える？

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

-- | 四角の枠線（太さ t）
outline :: Color -> Float -> Float -> Float -> Float -> Float -> Picture
outline c t x y w h = case cornerPts x y w h of
  ps@(p : _) | t <= 1.2  -> Color c (Line (ps ++ [p]))
             | otherwise -> Color c (thickPath t (ps ++ [p]))
  [] -> Blank

-- | 塗りと枠線のある表のマス
cellBox :: Color -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
cellBox f s t x y w h = Pictures [rbox f x y w h, outline s t x y w h]

-- | 文字：(x, y) を左上にして書く
txt :: Float -> Color -> Float -> Float -> String -> Picture
txt size c x y = label x (-y - size) size c

-- | 文字：(x, y) を中心にして書く
txtC :: Float -> Color -> Float -> Float -> String -> Picture
txtC size c x y s = label (x - fromIntegral (length s) * size * 0.4) (-y - size / 2) size c s

-- | 長い文を 1 行 n 文字くらいで折り返す
wrapText :: Int -> String -> [String]
wrapText n = go "" . words
  where
    go cur [] = [cur | not (null cur)]
    go "" (x : xs) = go x xs
    go cur (x : xs)
      | length cur + 1 + length x <= n = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs

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
