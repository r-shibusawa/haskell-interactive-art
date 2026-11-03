-- ============================================
-- 12 配送計画（車両ルーティング）─ クラーク＝ライトのセービング法
-- 倉庫から何台かのトラックで客先に荷物を届ける。1 台が積める量には上限がある。走る距離の合計を最小にしたい
--   はじめは 1 軒ごとに往復。i と j を 1 台でまとめて回ると、距離が s = d(倉庫,i) + d(倉庫,j) − d(i,j) だけ節約（セービング）できる
--   節約の大きい順に、積載量を超えなければ 2 つのルートをつなぐ（1964）
-- スペース = 自動 ON/OFF / → = 1 つずつ / ↑↓ = トラックの積載量 / クリック = 客先を足す / R = 新しい客先 / N = 最初から
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Numeric (showFFloat)
import Control.Monad (replicateM)
import Data.Char (toLower)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import qualified Data.Vector as V

nCust :: Int
nCust = 30                             -- ★ 客先の数

-- | 客先：位置と荷物の量
data Cust = Cust { cX, cY :: !Float, cQ :: !Int }

-- | セービング：客 i、客 j、節約できる距離
data Saving = Saving { sI, sJ :: !Int, sS :: !Float }

-- | 最後に処理したセービング
data Last = Last { lI, lJ :: !Int, lOk :: !Bool, lWhy :: String }

data World = World
  { wCust :: !(V.Vector Cust)
  , wCap :: !Int                       -- ★ トラック 1 台の積載量
  , wRoutes :: !(IM.IntMap [Int])      -- ルート（客先の番号の並び）
  , wRouteOf :: !(IM.IntMap Int)       -- 客先がどのルートにいるか
  , wSavings :: !(V.Vector Saving)
  , wIdx :: !Int
  , wRunning :: !Bool
  , wLast :: !(Maybe Last)
  , wMerged :: !Int
  }

depot :: Point
depot = (1280 * 0.33, 800 * 0.55)

main :: IO ()
main = runSketch (sketch "30-12 Vehicle Routing" (error "skStart makes the world"))
  { skBack  = hsb 225 30 12
  , skStart = \_ -> newCustomers (World V.empty 40 IM.empty IM.empty V.empty 0 False Nothing 0)
  , skStep  = \_ _ w -> pure (if wRunning w then stepAuto (3 :: Int) w else w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

stepAuto :: Int -> World -> World
stepAuto 0 w = w
stepAuto k w =
  let w1 = stepSaving w
  in if wRunning w1 then stepAuto (k - 1) w1 else w1

newCustomers :: World -> IO World
newCustomers w = do
  cs <- replicateM nCust $ do
    a <- randomRIO (0, tau)
    r <- randomRIO (60, min (1280 * 0.3) (800 * 0.4))
    q <- randomRIO (1, 9)                                -- ★ 荷物の量 1〜9
    pure (Cust (fst depot + cos a * r) (snd depot + sin a * r * 0.85) q)
  pure (restart w { wCust = V.fromList cs })

dd :: Point -> Point -> Float
dd = dist2

custP :: Cust -> Point
custP c = (cX c, cY c)

loadOf :: World -> [Int] -> Int
loadOf w = sum . map (cQ . (wCust w V.!))

routeLen :: World -> [Int] -> Float
routeLen w r = case map (custP . (wCust w V.!)) r of
  [] -> 0
  ps -> let stops = depot : ps ++ [depot] in sum (zipWith dd stops (drop 1 stops))

totalLen :: World -> Float
totalLen w = sum (map (routeLen w) (IM.elems (wRoutes w)))

restart :: World -> World
restart w =
  let n = V.length (wCust w)
      is = [0 .. n - 1]
      pt = custP . (wCust w V.!)
      -- ★ セービング値を全部のペアで計算して、大きい順に並べる
      saves = [ Saving i j (dd depot (pt i) + dd depot (pt j) - dd (pt i) (pt j)) | i <- is, j <- [i + 1 .. n - 1] ]
  in w { wRoutes = IM.fromList [(i, [i]) | i <- is]                 -- 1 軒ずつ往復
       , wRouteOf = IM.fromList [(i, i) | i <- is]
       , wSavings = V.fromList (sortBy (comparing (negate . sS)) saves)
       , wIdx = 0, wLast = Nothing, wMerged = 0 }

-- ★ セービングを 1 つ処理する
stepSaving :: World -> World
stepSaving w@World{..}
  | wIdx >= V.length wSavings = w { wRunning = False }
  | otherwise =
      let Saving{..} = wSavings V.! wIdx
          ri = IM.findWithDefault 0 sI wRouteOf
          rj = IM.findWithDefault 0 sJ wRouteOf
          a = IM.findWithDefault [] ri wRoutes
          b = IM.findWithDefault [] rj wRoutes
          w1 = w { wIdx = wIdx + 1 }
          refuse why = w1 { wLast = Just (Last sI sJ False why) }
          isHead x r = take 1 r == [x]
          isTail x r = take 1 (reverse r) == [x]
          loadA = loadOf w a
          loadB = loadOf w b
      in if ri == rj then refuse "same route"
         else if not ((isHead sI a || isTail sI a) && (isHead sJ b || isTail sJ b)) then refuse "inside a route"
         else if loadA + loadB > wCap then refuse ("over capacity (" ++ show (loadA + loadB) ++ " > " ++ show wCap ++ ")")
         else
           -- i が A の最後、j が B の先頭 になるように向きをそろえてつなぐ
           let a' = if isHead sI a then reverse a else a
               b' = if isHead sJ b then b else reverse b
               merged = a' ++ b'
           in w1 { wRoutes = IM.insert rj [] (IM.insert ri merged wRoutes)
                 , wRouteOf = foldl' (\m c -> IM.insert c ri m) wRouteOf merged
                 , wLast = Just (Last sI sJ True "")
                 , wMerged = wMerged + 1 }

routeHue :: Int -> Float
routeHue k = wrapF 360 (fromIntegral (k * 47))

drawAll :: Env -> World -> Picture
drawAll env w@World{..} = screen env $ Pictures [routes, lastPic, custs, depotPic, panel, loads, hud]
  where
    wd = envW env
    used = [(k, r) | (k, r) <- IM.toList wRoutes, not (null r)]
    pt = custP . (wCust V.!)
    (dx, dy) = depot

    -- ルート
    routes = Pictures
      [ let single = length r == 1
        in path (if single then 1 else 2.5) (hsba (routeHue k) 60 95 (if single then 0.35 else 0.9)) (depot : map pt r ++ [depot])
      | (k, r) <- used ]
    lastPic = case wLast of
      Just Last{..} -> seg 5 (if lOk then hsb 140 90 100 else hsb 0 80 100) (pt lI) (pt lJ)
      Nothing -> Blank
    custs = Pictures $ concat
      [ [ dot (gr 95) (cX c) (cY c) (8 + fromIntegral (cQ c) * 1.6)
        , txtC (cX c) (cY c) 14 (gr 10) (show (cQ c)) ]
      | c <- V.toList wCust ]
    depotPic = Pictures
      [ box (hsb 50 90 100) (dx - 14) (dy - 14) 28 28
      , txtC dx dy 14 (gr 10) "D" ]

    -- 右：数字・セービングの表
    rx = wd * 0.66
    ry = 120
    l0 = V.sum (V.map (\c -> 2 * dd depot (custP c)) wCust)
    lNow = totalLen w
    ly = ry + 64
    panel = Pictures $
      [ txt rx ry 20 white ("Trucks " ++ show (length used) ++ "  Distance " ++ nf 0 lNow)
      , txt rx (ry + 26) 14 (gr 70) ("Start " ++ nf 0 l0 ++ ": saved " ++ nf 1 (100 * (1 - lNow / max 1 l0)) ++ "%  Capacity " ++ show wCap)
      , txt rx ly 14 (gr 85) "Savings (largest first)"
      ]
      ++ [ let sv = wSavings V.! idx
               col | idx == wIdx - 1 = if maybe False lOk wLast then hsb 140 70 100 else hsb 0 70 100
                   | idx < wIdx = gr 45
                   | otherwise = gr 80
           in txt rx (ly + 24 + fromIntegral k * 20) 14 col (show (idx + 1) ++ ".  C" ++ show (sI sv) ++ " - C" ++ show (sJ sv) ++ "   s = " ++ nf 1 (sS sv))
         | k <- [0 .. 13 :: Int], let idx = wIdx - 6 + k, idx >= 0, idx < V.length wSavings ]
      ++ [ case wLast of
             Just Last{..} -> txt rx (ly + 330) 15 (if lOk then hsb 140 70 100 else hsb 0 70 100) (if lOk then "OK: joined" else "No: " ++ lWhy)
             Nothing -> Blank ]

    -- 各トラックの積載
    by = ly + 360
    bw = (wd - rx - 30) / fromIntegral (max 1 (length used))
    loads = Pictures $
      txt rx by 14 (gr 70) "Load per truck (line = limit)"
      : [ let h = fromIntegral (loadOf w r) / fromIntegral wCap * 80
          in box (hsb (routeHue k) 60 90) (rx + i * bw) (by + 100 - h) (max 2 (bw - 2)) h
        | (i, (k, r)) <- zip [0 ..] used ]
      ++ [ seg 1 (hsba 0 0 100 0.6) (rx, by + 20) (wd - 30, by + 20) ]

    hud = Pictures
      [ txt 20 40 16 white ("Space: auto " ++ (if wRunning then "ON" else "off") ++ "   Right: one step   U/D capacity " ++ show wCap ++ "   Click: customer   R: new customers   N: restart")
      , txt 20 68 14 (gr 60) ("Number = load. Green line = joined, red = refused. Done " ++ show wIdx ++ " / " ++ show (V.length wSavings) ++ "  joined " ++ show wMerged
          ++ (if wIdx >= V.length wSavings then "  FINISHED" else ""))
      ]

-- キーとマウス
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
    in if mx < envW env * 0.64 && my > 90
         then do
           q <- randomRIO (1, 9)
           pure (restart w { wCust = V.snoc (wCust w) (Cust mx my q) })
         else pure w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wRunning = not (wRunning w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (stepSaving w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (restart w { wCap = min 200 (wCap w + 5) })
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (restart w { wCap = max 10 (wCap w - 5) })
  EventKey (Char c) Down _ _ -> case toLower c of
    'r' -> newCustomers w
    'n' -> pure (restart w) { wRunning = False }
    _ -> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 積載量を 200（無制限）にすると 1 台になる。それは巡回セールスマン問題（08）の答えとどれくらい近い？
-- 2. 積載量を 10 にするとトラックは何台？ 荷物の合計 ÷ 積載量 と比べよう（これより少なくはできない）
-- 3. 完成した各ルートに 08 の 2-opt をかけると、さらに短くなる？
-- 4. 「1 台の走行距離にも上限がある」（運転時間）制約を stepSaving に足してみよう


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

-- 線分（太さ t）
seg :: Float -> Color -> Point -> Point -> Picture
seg t c (x1, y1) (x2, y2) = Color c (thickLine t (x1, negate y1) (x2, negate y2))

-- 折れ線（太さ t）
path :: Float -> Color -> [Point] -> Picture
path t c ps = Color c (thickPath t [(x, negate y) | (x, y) <- ps])

-- 塗りつぶした円：中心 (x, y)、直径 d
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (at (x, negate y) (disc (d / 2)))

-- 灰色（明るさ 0〜100）
gr :: Float -> Color
gr = hsb 0 0

-- 小数点以下 d 桁の文字列（p5.js の nf）
nf :: RealFloat a => Int -> a -> String
nf d x
  | d <= 0 = show (round x :: Int)
  | otherwise = showFFloat (Just d) x ""

