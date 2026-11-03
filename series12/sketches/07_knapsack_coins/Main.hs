-- ============================================
-- 07 貪欲法が勝つとき・負けるとき：ナップサックと両替
-- 粉のように分けられる荷物（分数ナップサック）なら「重さあたりの価値が高い順」の貪欲法で最適
-- でも分けられない荷物（0-1 ナップサック）や、変わった硬貨での両替では貪欲法が失敗する → DP で解く
-- K = ナップサック / C = 両替 / スペース = 再生・停止 / → = 1 マス / 1〜3 = お手本 / ↑↓ = 容量・金額 / R = ランダム
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Array
import Data.List (foldl', intercalate, sort, sortOn)
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as Set
import Text.Printf (printf)

startItems :: [(Int, Int)]
startItems = [(1, 50), (2, 80), (3, 105)]   -- ★ 荷物（重さ kg, 価値 円）。容量 5 なら分数 200 円、0-1 貪欲 130 円、0-1 最適 185 円

startCap :: Int
startCap = 5                 -- ★ ナップサックの容量

startCoins :: [Int]
startCoins = [1, 3, 4]       -- ★ 硬貨の種類

startAmount :: Int
startAmount = 6              -- ★ 両替する金額

data World = World
  { wKnap    :: !Bool       -- True ならナップサック、False なら両替
  , wItems   :: ![(Int, Int)]
  , wCap     :: !Int
  , wCoins   :: ![Int]
  , wAmount  :: !Int
  , wK       :: !(Array (Int, Int) (Int, Bool))  -- K[i][c] と「入れたか」
  , wC       :: !(IM.IntMap (Int, Int))          -- C[a] と最後に使う硬貨
  , wFilled  :: !Float
  , wPlaying :: !Bool
  }

initial :: World
initial = restart (World True startItems startCap startCoins startAmount (knapTable [] 0) IM.empty 0 True)

main :: IO ()
main = runSketch (sketch "12-07 When greedy wins or loses" initial)
  { skBack  = cBg
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> onEvent ev w
  }

-- ★ 0-1 ナップサックの表
knapTable :: [(Int, Int)] -> Int -> Array (Int, Int) (Int, Bool)
knapTable items cap = t
  where
    n = length items
    t = array ((0, 0), (n, cap)) [((i, c), f i c) | i <- [0 .. n], c <- [0 .. cap]]
    f 0 _ = (0, False)
    f i c =
      let (wt, v) = nth (0, 0) items (i - 1)
          skip = fst (t ! (i - 1, c))
          use = fst (t ! (i - 1, c - wt)) + v
      in if wt <= c && use > skip then (use, True) else (skip, False)

noWay :: Int
noWay = maxBound `div` 2    -- JavaScript の Infinity の代わり（払えない）

-- ★ 両替の表：C[a] = 1 + min( C[a - 硬貨] )
coinTable :: [Int] -> Int -> IM.IntMap (Int, Int)
coinTable coins amount = foldl' fillOne (IM.singleton 0 (0, 0)) [1 .. amount]
  where
    fillOne m a = IM.insert a (foldl' (try m a) (noWay, 0) coins) m
    try m a (best, from) c
      | c <= a && fst (IM.findWithDefault (noWay, 0) (a - c) m) + 1 < best =
          (fst (IM.findWithDefault (noWay, 0) (a - c) m) + 1, c)
      | otherwise = (best, from)

restart :: World -> World
restart w
  | wKnap w = w { wFilled = 0, wK = knapTable (wItems w) (wCap w) }
  | otherwise = w { wFilled = 0, wC = coinTable (wCoins w) (wAmount w) }

step :: World -> World
step w
  | not (wPlaying w) = w
  | wKnap w =
      let total = i2f (length (wItems w) * (wCap w + 1))
      in w { wFilled = min (total + 1) (wFilled w + max 0.2 (total / 240)) }
  | otherwise = w { wFilled = min (i2f (wAmount w + 1)) (wFilled w + 0.12) }

onEvent :: Event -> World -> IO World
onEvent ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> toggle
  EventKey (MouseButton LeftButton) Down _ _ -> toggle
  EventKey (SpecialKey KeyRight) Down _ _ ->
    let lim = if wKnap w then i2f (length (wItems w) * (wCap w + 1) + 1) else i2f (wAmount w + 1)
    in pure w { wPlaying = False, wFilled = min lim (i2f (floor (wFilled w) + 1 :: Int)) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure . restart $
    if wKnap w then w { wCap = min 20 (wCap w + 1) } else w { wAmount = min 40 (wAmount w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure . restart $
    if wKnap w then w { wCap = max 1 (wCap w - 1) } else w { wAmount = max 1 (wAmount w - 1) }
  EventKey (Char c) Down _ _
    | c `elem` "kK" -> pure (restart w { wKnap = True })
    | c `elem` "cC" -> pure (restart w { wKnap = False })
    | c == '1' -> preset (if wKnap w then w { wItems = startItems, wCap = 5 } else w { wCoins = [1, 5, 10, 25], wAmount = 30 })  -- アメリカの硬貨：貪欲で最適
    | c == '2' -> preset (if wKnap w then w { wItems = [(5, 10), (4, 40), (6, 30), (3, 50)], wCap = 10 } else w { wCoins = [1, 3, 4], wAmount = 6 })  -- 貪欲 4+1+1、最適 3+3
    | c == '3' -> preset (if wKnap w then w { wItems = [(2, 3), (3, 4), (4, 5), (5, 6), (9, 10)], wCap = 13 } else w { wCoins = [1, 7, 10], wAmount = 14 })  -- 貪欲 10+1+1+1+1、最適 7+7
    | c `elem` "rR" ->
        if wKnap w
          then do
            k <- randomRIO (3, 6)
            items <- replicateM k ((,) <$> randomRIO (1, 6) <*> ((* 5) <$> randomRIO (2, 19)))
            cap <- randomRIO (6, 15)
            preset w { wItems = items, wCap = cap }
          else do
            cs <- pickCoins [1]
            a <- randomRIO (10, 29)
            preset w { wCoins = sort cs, wAmount = a }
  _ -> pure w
  where
    toggle = pure w { wPlaying = not (wPlaying w) }
    preset w1 = pure (restart w1) { wPlaying = True }
    pickCoins cs
      | length cs >= 3 = pure cs
      | otherwise = do
          c <- randomRIO (2, 12)
          pickCoins (if c `elem` cs then cs else cs ++ [c])

itemColor :: Int -> Color
itemColor k = nth cFg [cRasp, cCyan, cAmber, cLime, rgb255 0xc7 0x92 0xff, rgb255 0xff 0x9e 0x57] (k `mod` 6)

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env
  [ txt 15 cMuted 32 60 "K: knapsack   C: coins   Space: play/pause   Right: 1 cell   1-3: samples   Up/Down: size   R: random"
  , if wKnap w then drawKnapsack env w else drawCoins env w ]

---------------------------------------------------------------- ナップサック
drawKnapsack :: Env -> World -> Picture
drawKnapsack env w = Pictures
  ( txt 26 cFg 32 24 ("Knapsack: pack capacity " ++ show cap ++ " for the most value (yen)")
  : itemBoxes ++ headers ++ cells
  ++ [txt 14 cMuted x0 (if px < 480 then y0 + bh + 80 else y0 + i2f (n + 2) * cellH + 8) "K[i][c] = max( skip K[i-1][c] , take K[i-1][c-w] + v )"]
  ++ concat (zipWith bagPic [0 ..] bags) )
  where
    wd = envW env
    ht = envH env
    items = wItems w
    n = length items
    cap = wCap w
    total = n * (cap + 1)
    shown = floor (wFilled w) :: Int
    t = wK w
    done = shown >= total
    -- 荷物
    boxW (wt, _) = max 120 (40 + i2f wt * 16)
    itemXs = scanl (\x it -> x + boxW it + 10) 32 items
    itemBoxes = concat
      [ [ rbox (itemColor k) x 100 (boxW it) 46
        , txtC 14 cBg (x + boxW it / 2) 114 (show wt ++ "kg " ++ show v ++ "yen")
        , txtC 14 cBg (x + boxW it / 2) 133 (show (v `div` max 1 wt) ++ "/kg") ]
      | (k, it@(wt, v), x) <- zip3 [0 ..] items itemXs ]
    -- DP 表（数字が入るよう、横は 42 以上にする）
    cellH = minimum [54, wd * 0.6 / i2f (cap + 2), (ht - 300) / i2f (n + 2)]
    cellW = max 42 (min 54 (wd * 0.6 / i2f (cap + 2)))
    x0 = 32
    y0 = 180
    (ci, cc) | shown < total = (shown `div` (cap + 1) + 1, shown `mod` (cap + 1))
             | otherwise = (-1, -1)
    curW = fst (nth (0, 0) items (ci - 1))
    headers =
      [ txtC 14 (if c == cc then cAmber else cMuted) (x0 + i2f (c + 1) * cellW + cellW / 2) (y0 + cellH / 2) (show c) | c <- [0 .. cap] ]
      ++ [ txtC 14 (if i == ci then cAmber else cMuted) (x0 + cellW / 2) (y0 + i2f (i + 1) * cellH + cellH / 2) ("i=" ++ show i) | i <- [0 .. n] ]
    onPath = if done then Set.fromList (pathCells n cap) else Set.empty
    pathCells i c
      | i < 1 = []
      | otherwise = (i, c) : pathCells (i - 1) (if snd (t ! (i, c)) then c - fst (nth (0, 0) items (i - 1)) else c)
    cells = concat
      [ [ cellBox col (if cur then cAmber else if dep then cCyan else cGrid) (if cur || dep then 2.5 else 1)
                  (x + 1) (y + 1) (cellW - 2) (cellH - 2) ]
        ++ [txtC 14 cFg (x + cellW / 2) (y + cellH / 2) (show (fst (t ! (i, c)))) | vis || cur]
      | i <- [0 .. n], c <- [0 .. cap]
      , let x = x0 + i2f (c + 1) * cellW
      , let y = y0 + i2f (i + 1) * cellH
      , let idx = (i - 1) * (cap + 1) + c
      , let vis = i == 0 || idx < shown
      , let cur = i == ci && c == cc
      , let dep = ci > 0 && i == ci - 1 && (c == cc || c == cc - curW)
      , let col | Set.member (i, c) onPath = if snd (t ! (i, c)) then rgb255 0x5f 0x7f 0x1e else rgb255 0x4a 0x3d 0x56
                | vis = cPanel
                | otherwise = cBg ]
    -- 3 つの詰め方
    px = x0 + i2f (cap + 2) * cellW + 40
    pw = wd - px - 30
    ratio = sortOn (\k -> let (wt, v) = nth (1, 0) items k in negate (i2f v / i2f wt)) [0 .. n - 1]
    -- ★ 分数：入るだけ入れて、最後は分けて入れる
    (frac, _, fv) = foldl' fracStep ([], i2f cap, 0) ratio
    fracStep (acc, room, val) k =
      let (wt, v) = nth (1, 0) items k
          part = min 1 (room / i2f wt)
      in if part > 0 then (acc ++ [(k, part)], room - part * i2f wt, val + part * i2f v) else (acc, room, val)
    -- ★ 0-1 の貪欲：まるごと入るものだけ入れる
    (g01, _, gv) = foldl' greedyStep ([], cap, 0 :: Int) ratio
    greedyStep (acc, room, val) k =
      let (wt, v) = nth (1, 0) items k
      in if wt <= room then (acc ++ [(k, 1 :: Float)], room - wt, val + v) else (acc, room, val)
    dpTake i c
      | i < 1 = []
      | snd (t ! (i, c)) = (i - 1, 1 :: Float) : dpTake (i - 1) (c - fst (nth (0, 0) items (i - 1)))
      | otherwise = dpTake (i - 1) c
    best = fst (t ! (n, cap))
    bags =
      [ (("frac.", "greedy"), frac, show (round fv :: Int))
      , (("0-1", "greedy"), g01, show gv)
      , (("0-1", "DP"), if done then dpTake n cap else [], if done then show best else "?") ]
    bw = min 90 (pw / 3 - 20)
    bh = min 320 (ht - 300)
    unit = bh / i2f cap
    bagPic b ((l1, l2), content, value) =
      let bx = px + i2f b * (bw + 24)
          byy = 180
          hs = [(k, i2f (fst (nth (1, 0) items k)) * part * unit) | (k, part) <- content]
          tops = scanl (+) 0 (map snd hs)
      in [outline cFg 2 bx byy bw bh]
         ++ [ rbox (itemColor k) (bx + 3) (byy + bh - off - h + 2) (bw - 6) (h - 4) | ((k, h), off) <- zip hs tops ]
         ++ [ txtC 14 cFg (bx + bw / 2) (byy + bh + 18) l1
            , txtC 14 cFg (bx + bw / 2) (byy + bh + 38) l2
            , txtC 18 (if b == (1 :: Int) && done && gv < best then cRasp else cLime) (bx + bw / 2) (byy + bh + 62) value ]

---------------------------------------------------------------- 両替
drawCoins :: Env -> World -> Picture
drawCoins env w = Pictures
  ( txt 26 cFg 32 24 ("Coins: pay " ++ show amount ++ " with fewest of {" ++ intercalate ", " (map show coins) ++ "}")
  : concatMap cell [0 .. amount]
  ++ [txt 14 cMuted 32 (y0 + cw + 80) "C[a] = 1 + min( C[a - coin] ): try every coin as the last one"]
  ++ coinRow "Greedy (largest first)" greedy (if rest > 0 then cRasp else cAmber) (y0 + cw + 130)
       (if rest > 0 then "cannot pay!" else countText greedy)
  ++ coinRow "DP (fewest coins)" opt cLime (y0 + cw + 210) (countText opt) )
  where
    wd = envW env
    coins = wCoins w
    amount = wAmount w
    shown = floor (wFilled w) :: Int
    cw = min 64 ((wd - 64) / i2f (amount + 1))
    y0 = 150
    table a = IM.findWithDefault (noWay, 0) a (wC w)
    cell a =
      let x = 32 + i2f a * cw
          vis = a <= shown
          cur = a == shown && shown <= amount
          (cost, from) = table a
      in [ cellBox (if vis then cPanel else cBg) (if cur then cAmber else cGrid) (if cur then 3 else 1) (x + 2) y0 (cw - 4) (cw - 4)
         , txtC 14 cMuted (x + cw / 2) (y0 - 14) (show a) ]
         ++ [ txtC (max 14 (min 18 (cw * 0.35))) cFg (x + cw / 2) (y0 + cw / 2 - 2) (if cost >= noWay then "x" else show cost) | vis ]
         ++ (if cur && a > 0 then concatMap (arc a x from) [c | c <- coins, c <= a] else [])
    arc a x from c =
      let sx = 32 + i2f (a - c) * cw + cw / 2
          ex = x + cw / 2
          h = 30 + i2f c * 6
          isFrom = c == from
      in [ pathL (if isFrom then cLime else rgb255 0x6b 0x5a 0x78) (if isFrom then 3 else 1.5)
             (bezierPts (sx, y0 + cw) (sx, y0 + cw + h) (ex, y0 + cw + h) (ex, y0 + cw))
         , txtC 14 (if isFrom then cLime else cMuted) ((sx + ex) / 2) (y0 + cw + 36 + i2f c * 4.5) ("+" ++ show c) ]
    -- ★ 貪欲：大きい硬貨から払えるだけ払う
    (greedy, rest) = foldl' payWith ([], amount) (reverse (sort coins))
    payWith (acc, r) c = (acc ++ replicate (r `div` c) c, r `mod` c)
    opt | shown > amount && fst (table amount) < noWay = walk amount
        | otherwise = []
    walk a | a <= 0 = []
           | otherwise = let c = max 1 (snd (table a)) in c : walk (a - c)
    countText arr = if null arr then "" else show (length arr) ++ " coins"
    coinRow name arr col yy tailText =
      let dia c = 30 + i2f c * 2
          -- 隣の硬貨と重ならないように、半径の和だけ離して並べる
          xs = scanl (\x (a, b) -> x + (dia a + dia b) / 2 + 8) 300 (zip arr (drop 1 arr))
          fit = takeWhile (\(x, _) -> x < wd - 170) (zip xs arr)
          endX = case reverse fit of
            ((x, c) : _) -> x + dia c / 2 + 8
            [] -> 300
      in txtL 15 cFg 32 (yy + 26) name
         : concat [ [circ col x (yy + 26) (dia c), txtC 14 cBg x (yy + 26) (show c)] | (x, c) <- fit ]
         ++ [txtL 15 col (endX + 10) (yy + 26) tailText]

-- ---------- やってみよう ----------
-- 1. startCoins を [1, 6, 10] にして、貪欲法が負ける金額を探す
-- 2. startItems の価値を変えて、0-1 の貪欲法が勝つ例と負ける例を作る
-- 3. 両替で「1 円玉がない」硬貨にすると、払えない金額（x）が現れる

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

-- | 折れ線（太さ t）
pathL :: Color -> Float -> [Point] -> Picture
pathL c t ps = Color c (thickPath t [(x, -y) | (x, y) <- ps])

-- | 円（p5.js の circle と同じく d は直径）
circ :: Color -> Float -> Float -> Float -> Picture
circ c x y d = Color c (Translate x (-y) (disc (d / 2)))

-- | 3 次ベジェ曲線の点列（p5.js の bezier の代わり）
bezierPts :: Point -> Point -> Point -> Point -> [Point]
bezierPts (ax, ay) (bx, by) (cx, cy) (dx, dy) =
  [ (f ax bx cx dx t, f ay by cy dy t) | i <- [0 .. 20 :: Int], let t = fromIntegral i / 20 ]
  where f a b c d t = let u = 1 - t in u * u * u * a + 3 * u * u * t * b + 3 * u * t * t * c + t * t * t * d

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
