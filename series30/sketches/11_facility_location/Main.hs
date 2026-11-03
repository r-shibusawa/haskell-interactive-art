-- ============================================
-- 11 施設配置問題 ─ p メディアンと最大被覆
-- 町に p か所の拠点（倉庫・救急車の待機所・図書館）を置く。どこに置くとみんなが便利？
--   p メディアン：「人数 × 一番近い拠点までの距離」の合計を最小に ／ 最大被覆：半径 R 以内に入る人数を最大に
--   貪欲法で 1 か所ずつ足し、そのあと「入れかえ（スワップ）」で直す
-- クリック = 住宅地を足す / スペース = 貪欲 → 入れかえ / ↑↓ = 拠点の数 p / 1・2 = 目的 / ←→ = 半径 R / R = 新しい町 / C = 全部消す
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Numeric (showFFloat)
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (elemIndex, foldl')
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U

nDemand :: Int
nDemand = 120                          -- ★ 住宅地の数

data Phase = Idle | Greedy | Swap deriving Eq

data World = World
  { wXs, wYs, wWs :: !(U.Vector Float) -- 住宅地の位置と人数
  , wFac :: ![Int]                     -- 拠点（住宅地の番号）
  , wP :: !Int                         -- ★ 拠点の数
  , wObj :: !Int                       -- 1 = p メディアン, 2 = 最大被覆
  , wR :: !Float                       -- ★ 被覆の半径
  , wPhase :: !Phase
  , wSwapIdx, wNoImprove, wSwaps :: !Int
  , wObjLog :: !(Seq.Seq Float)        -- 目的関数の記録（300 個まで）
  }

mapX, mapY, mapW, mapH :: Float
mapX = 30
mapY = 100
mapW = 1280 * 0.62
mapH = 800 - 130

main :: IO ()
main = runSketch (sketch "30-11 Facility Location" (error "skStart makes the world"))
  { skBack  = hsb 225 30 12
  , skStart = \_ -> newTown (World U.empty U.empty U.empty [] 4 1 120 Idle 0 0 0 Seq.empty)
  , skStep  = \env _ w -> pure (stepWorld (envFrame env) w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

newTown :: World -> IO World
newTown w = do
  centers <- replicateM 5 ((,,) <$> randomRIO (80, mapW - 80) <*> randomRIO (80, mapH - 80) <*> randomRIO (40, 110))
  ps <- replicateM nDemand $ do
    k <- randomRIO (0, length centers - 1)
    let (cx, cy, s) = case drop k centers of
          c : _ -> c
          [] -> (mapW / 2, mapH / 2, 50 :: Float)
    zx <- gauss
    zy <- gauss
    pw <- randomRIO (1, 9 :: Int)
    pure ( clampF (mapX + 10) (mapX + mapW - 10) (mapX + cx + zx * s)
         , clampF (mapY + 10) (mapY + mapH - 10) (mapY + cy + zy * s)
         , fromIntegral pw )
  pure (resetFac w { wXs = U.fromList [x | (x, _, _) <- ps], wYs = U.fromList [y | (_, y, _) <- ps], wWs = U.fromList [p | (_, _, p) <- ps] })

resetFac :: World -> World
resetFac w = w { wFac = [], wPhase = Idle, wSwaps = 0, wObjLog = Seq.empty }

dst :: World -> Int -> Int -> Float
dst World{..} i j =
  let dx = U.unsafeIndex wXs i - U.unsafeIndex wXs j
      dy = U.unsafeIndex wYs i - U.unsafeIndex wYs j
  in sqrt (dx * dx + dy * dy)
{-# INLINE dst #-}

-- 一番近い拠点までの距離
nearDist :: World -> [Int] -> Int -> Float
nearDist w fs i = foldl' (\m f -> min m (dst w i f)) (1 / 0) fs

-- ★ 目的関数（小さいほど良いようにそろえる）
objective :: World -> [Int] -> Float
objective w@World{..} fs
  | null fs = 1 / 0
  | otherwise = foldl' add 0 [0 .. U.length wXs - 1]
  where
    add !s i =
      let m = nearDist w fs i
          pw = U.unsafeIndex wWs i
      in if wObj == 1 then s + pw * m            -- p メディアン：人数 × 距離
         else if m > wR then s + pw else s       -- 最大被覆：カバーされない人数

-- ★ 貪欲法：今の拠点に 1 か所足すなら、目的が一番良くなる場所
greedyAdd :: World -> World
greedyAdd w@World{..} =
  let pick (best, bv) c
        | c `elem` wFac = (best, bv)
        | otherwise =
            let v = objective w (wFac ++ [c])
            in if v < bv then (c, v) else (best, bv)
      (b, _) = foldl' pick (-1, 1 / 0) [0 .. U.length wXs - 1]
  in if b >= 0 then w { wFac = wFac ++ [b] } else w

-- ★ 入れかえ：拠点 k を別の場所に動かして良くなるなら動かす（1 回に拠点 1 つ分）
swapStep :: World -> (World, Bool)
swapStep w@World{..}
  | null wFac = (w, False)
  | otherwise =
      let k = wSwapIdx `mod` length wFac
          replaceAt c = [if i == k then c else f | (i, f) <- zip [0 ..] wFac]
          pick (best, bv) c
            | c `elem` wFac = (best, bv)
            | otherwise =
                let v = objective w (replaceAt c)
                in if v < bv - 1e-9 then (c, v) else (best, bv)
          (b, _) = foldl' pick (-1, objective w wFac) [0 .. U.length wXs - 1]
          noImp = if b >= 0 then 0 else wNoImprove + 1
          w1 | b >= 0 = w { wFac = replaceAt b, wSwaps = wSwaps + 1 }
             | otherwise = w
      in (w1 { wSwapIdx = wSwapIdx + 1, wNoImprove = noImp }, noImp < length wFac)

pushLog :: World -> World
pushLog w =
  let v = objective w (wFac w)
      l = wObjLog w Seq.|> v
  in w { wObjLog = if Seq.length l > 300 then Seq.drop 1 l else l }

stepWorld :: Int -> World -> World
stepWorld fc w = case wPhase w of
  Greedy | fc `mod` 8 == 0 ->
    let w1 = pushLog (greedyAdd w)
    in if length (wFac w1) >= wP w1 then w1 { wPhase = Swap, wSwapIdx = 0, wNoImprove = 0 } else w1
  Swap | fc `mod` 4 == 0 ->
    let (w1, more) = swapStep w
    in pushLog (if more then w1 else w1 { wPhase = Idle })
  _ -> w

facHue :: Int -> Float
facHue k = wrapF 360 (fromIntegral (k * 67 + 30))

drawAll :: Env -> World -> Picture
drawAll env w@World{..} = screen env $ Pictures [mapPic, numbers, graph, notes, hud]
  where
    wd = envW env
    n = U.length wXs
    px i = (wXs U.! i, wYs U.! i)
    hasFac = not (null wFac)

    -- 割り当て（一番近い拠点）
    nearest i = foldl' (\(m, b) f -> let d = dst w i f in if d < m then (d, f) else (m, b)) (1 / 0, -1) wFac
    near = map nearest [0 .. n - 1]
    total = U.sum wWs
    covered = sum [p | (p, (m, _)) <- zip (U.toList wWs) near, m <= wR]
    sumWD = sum [p * m | (p, (m, b)) <- zip (U.toList wWs) near, b >= 0]

    mapPic = Pictures $
      [ boxL 1 (hsba 0 0 100 0.15) mapX mapY mapW mapH ]
      ++ (if wObj == 2
            then concat [ let (x, y) = px f in [dot (hsba 190 60 90 0.12) x y (2 * wR), ringAt 1 (hsba 190 60 90 0.4) x y (2 * wR)] | f <- wFac ]
            else [])
      ++ concat [ homePic i p m b | (i, p, (m, b)) <- zip3 [0 ..] (U.toList wWs) near ]
      ++ concat [ facPic k f | (k, f) <- zip [0 ..] wFac ]
    homePic i p m b =
      let (x, y) = px i
          ok = m <= wR
          col | wObj == 2 && hasFac = if ok then hsb 140 60 90 else hsb 0 70 90
              | otherwise = gr 85
          k = maybe 0 id (elemIndex b wFac)
      in [ seg 1 (hsba (facHue k) 50 90 0.4) (x, y) (px b) | b >= 0 && wObj == 1 ]
         ++ [ dot col x y (3 + p * 1.2) ]
    facPic k f =
      let (x, y) = px f
      in [ box (hsb (facHue k) 70 100) (x - 10) (y - 10) 20 20
         , boxL 2 white (x - 10) (y - 10) 20 20
         , txtC x y 14 (gr 10) (show (k + 1)) ]

    -- 右：数字とグラフ
    rx = mapX + mapW + 40
    rw = wd - rx - 30
    stage = case wPhase of
      Idle -> "stopped"
      Greedy -> "greedy add"
      Swap -> "swap to improve"
    rows =
      [ "Sites " ++ show (length wFac) ++ " / p = " ++ show wP ++ "  Homes " ++ show n ++ " (" ++ nf 0 total ++ ")"
      , "People x distance  " ++ (if hasFac then nf 0 sumWD else "-")
      , "Mean distance  " ++ (if hasFac then nf 1 (sumWD / max 1 total) else "-")
      , "Within R " ++ nf 0 wR ++ ":  " ++ (if hasFac then nf 1 (100 * covered / max 1 total) ++ "%" else "-")
      , "Stage: " ++ stage ++ "  Swaps " ++ show wSwaps
      ]
    numbers = Pictures $
      txt rx 130 20 white (objName wObj)
      : [ txt rx (166 + i * 26) 14 (gr 85) s | (i, s) <- zip [0 ..] rows ]

    gy = 330
    gh = 160
    logL = toList wObjLog
    finite = filter (not . isInfinite) logL
    graph = Pictures $
      [ txt rx (gy - 10) 14 (gr 70) "Objective over time (lower = better)"
      , seg 1 (hsba 0 0 100 0.25) (rx, gy + gh) (rx + rw, gy + gh) ]
      ++ (if length logL > 1 && not (null finite)
            then
              let hi = maximum finite
                  lo = minimum finite
                  nL = fromIntegral (length logL - 1) :: Float
              in [ path 2 (hsb 50 90 100)
                     [ (rx + fromIntegral k / nL * rw, gy + gh - (v - lo) / max 1e-9 (hi - lo) * gh)
                     | (k, v) <- zip [0 :: Int ..] logL, not (isInfinite v) ] ]
            else [])
    notes = Pictures
      [ txt rx (gy + gh + 40 + i * 22) 14 (gr 60) s
      | (i, s) <- zip [0 ..]
          [ "p-median = average access (depot, school)"
          , "Max cover = \"within N min\" (ambulance)"
          , "Greedy tends to put site 1 in the middle"
          , "then swaps review the whole (Teitz-Bart)"
          , ""
          , "Size of a home circle = people" ] ]

    hud = Pictures
      [ txt 20 40 16 white ("Click: home  Space: greedy -> swap  U/D p " ++ show wP ++ "  1/2: goal " ++ objName wObj ++ "  L/R radius " ++ nf 0 wR ++ "  R: new  C: clear")
      , txt 20 68 14 (gr 60) "Squares = sites. p-median looks at line length x people; max cover at people inside circles."
      ]

objName :: Int -> String
objName m = if m == 1 then "p-median" else "Max cover"

-- キーとマウス
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
    in pure $ if mx > mapX && mx < mapX + mapW && my > mapY && my < mapY + mapH
         then w { wXs = U.snoc (wXs w) mx, wYs = U.snoc (wYs w) my, wWs = U.snoc (wWs w) 9 }
         else w
  EventKey (SpecialKey KeySpace) Down _ _
    | U.length (wXs w) > wP w -> pure w { wFac = [], wObjLog = Seq.empty, wSwaps = 0, wPhase = Greedy }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (resetFac w { wP = min 10 (wP w + 1) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (resetFac w { wP = max 1 (wP w - 1) })
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wR = min 400 (wR w + 10) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wR = max 20 (wR w - 10) }
  EventKey (Char c) Down _ _ -> case toLower c of
    '1' -> pure (resetFac w { wObj = 1 })
    '2' -> pure (resetFac w { wObj = 2 })
    'r' -> newTown w
    'c' -> pure (resetFac w { wXs = U.empty, wYs = U.empty, wWs = U.empty })
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. p = 1 の p メディアンの拠点はどこになる？ 人数の重心と比べよう（同じとは限らない）
-- 2. 同じ町・同じ p で 1（p メディアン）と 2（最大被覆）の配置を比べよう。どこが違う？
-- 3. 町の外れに人数 9 の住宅地をいくつかクリックすると、拠点は動く？ 最大被覆では？
-- 4. 「最も遠い人の距離を最小に」（p センター問題）を objective に足してみよう


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標で描くための変換
screen :: Env -> Picture -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2)

-- マウスの位置（左上が原点、y は下向き）
mouseP :: Env -> Point
mouseP = toP5 <*> envMouse

-- gloss の座標 → 左上が原点の座標
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字：(x, y) は左端のベースライン
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label x (negate y)

-- 文字：(x, y) が文字の中心
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y = labelC x (negate y)

-- 塗りつぶした長方形：左上 (x, y)、幅 w、高さ h
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (rect (x + w / 2) (negate (y + h / 2)) w h)

-- 枠だけの長方形（線の太さ t）
boxL :: Float -> Color -> Float -> Float -> Float -> Float -> Picture
boxL t c x y w h = loopPath t c [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- 線分（太さ t）
seg :: Float -> Color -> Point -> Point -> Picture
seg t c (x1, y1) (x2, y2) = Color c (thickLine t (x1, negate y1) (x2, negate y2))

-- 折れ線（太さ t）
path :: Float -> Color -> [Point] -> Picture
path t c ps = Color c (thickPath t [(x, negate y) | (x, y) <- ps])

-- 閉じた折れ線（太さ t）
loopPath :: Float -> Color -> [Point] -> Picture
loopPath t c ps = path t c (ps ++ take 1 ps)

-- 塗りつぶした円：中心 (x, y)、直径 d
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (at (x, negate y) (disc (d / 2)))

-- 輪：中心 (x, y)、直径 d、太さ t
ringAt :: Float -> Color -> Float -> Float -> Float -> Picture
ringAt t c x y d = Color c (at (x, negate y) (ring (d / 2) t))

-- 灰色（明るさ 0〜100）
gr :: Float -> Color
gr = hsb 0 0

-- 小数点以下 d 桁の文字列（p5.js の nf）
nf :: RealFloat a => Int -> a -> String
nf d x
  | d <= 0 = show (round x :: Int)
  | otherwise = showFFloat (Just d) x ""

-- 正規分布の乱数（ボックス＝ミュラー法、p5.js の randomGaussian）
gauss :: IO Float
gauss = do
  u <- randomRIO (1e-6, 1)
  v <- randomRIO (0, 1)
  pure (sqrt (-2 * log u) * cos (tau * v))

