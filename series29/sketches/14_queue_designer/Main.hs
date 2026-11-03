-- ============================================
-- 14 待ち行列デザイナー（自由制作のベース）
-- 「客がわく所（発生源）」「窓口（待ち行列 + サービス）」「出口」を置いて矢印でつなぎ、自分だけの待ち行列の網をつくる
--   客は矢印に沿って移動し、矢印が複数あるときはでたらめに 1 本を選ぶ。どこに行列ができるか見てみよう
-- 1・2・3 = 置く部品（発生源・窓口・出口）/ 空いた所をクリック = 置く / 部品からドラッグ = 矢印をつなぐ（もう一度で外す）
-- Shift + ドラッグ = 動かす / ↑↓ = 選んだ部品の速さ / X = 選んだ部品を消す / スペース = 一時停止 / S = 保存 / C = 全部消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Char (digitToInt)
import qualified Data.IntMap.Strict as IM
import Data.List (delete)
import Data.Sequence (Seq, ViewL(..), viewl, (|>))
import qualified Data.Sequence as Seq
import Text.Printf (PrintfArg, printf)

travelSpeed :: Float
travelSpeed = 3     -- ★ 客が矢印の上を進む速さ（px/フレーム）

maxTokens, maxQueue :: Int
maxTokens = 600     -- ★ 画面に出せる客の上限
maxQueue = 5000     -- 1 つの窓口に並べる上限（メモリを守る）

toolNames :: [String]
toolNames = ["", "Source", "Desk", "Exit"]

-- 部品（kind 1 = 発生源, 2 = 窓口, 3 = 出口）
data Node = Node
  { nKind :: !Int
  , nX, nY, nRate :: !Float
  , nOuts :: ![Int]
  , nQ :: !(Seq Double)         -- 並んでいる客の「生まれた時刻」
  , nCur :: !(Maybe Double)     -- サービス中の客の「生まれた時刻」
  , nRem, nNext :: !Double
  , nBusyAvg, nQAvg :: !Float
  , nDone :: !Int
  , nSumT :: !Double
  }

-- 矢印の上を動く客
data Token = Token { tFrom, tTo :: !Int, tF :: !Float, tBorn :: !Double }

data Drag = NoDrag | Linking | Moving deriving Eq

data World = World
  { wTool :: !Int
  , wNodes :: !(IM.IntMap Node)
  , wTokens :: ![Token]
  , wSel :: !(Maybe Int)
  , wDrag :: !Drag
  , wPaused :: !Bool
  , wT :: !Double
  , wNid :: !Int
  }

main :: IO ()
main = runSketch (sketch "29-14 Queue Designer" world0)
  { skBack  = hsb 300 15 11
  , skStart = \_ -> example world0
  , skStep  = \_ _ w -> if wPaused w then pure w else update (1 / 60) w
  , skDraw  = \env w -> drawAll env w
  , skEvent = \env ev w -> onEvent env ev w
  }

world0 :: World
world0 = World 1 IM.empty [] Nothing NoDrag False 0 0

addNode :: Int -> Float -> Float -> World -> IO (Int, World)
addNode kind x y w = do
  let rate | kind == 1 = 0.8
           | kind == 2 = 1.0
           | otherwise = 0
  next <- if kind == 1 then (wT w +) <$> expRand (realToFrac rate) else pure 0
  let k = wNid w
      n = Node kind x y rate [] Seq.empty Nothing 0 next 0 0 0 0
  pure (k, w { wNodes = IM.insert k n (wNodes w), wNid = k + 1 })

-- 最初から置いてある例
example :: World -> IO World
example w0 = do
  (s, w1) <- addNode 1 (width * 0.12) (height * 0.5) w0
  (a, w2) <- addNode 2 (width * 0.35) (height * 0.5) w1
  (b, w3) <- addNode 2 (width * 0.58) (height * 0.32) w2
  (c, w4) <- addNode 2 (width * 0.58) (height * 0.7) w3
  (e, w5) <- addNode 3 (width * 0.82) (height * 0.5) w4
  let set k f = IM.adjust f k
      nodes = set s (\n -> n { nOuts = [a] })
            . set a (\n -> n { nRate = 1.2, nOuts = [b, c] })
            . set b (\n -> n { nRate = 0.5, nOuts = [e] })
            . set c (\n -> n { nRate = 0.6, nOuts = [e] })
            $ wNodes w5
  pure w5 { wNodes = nodes, wSel = Just a }

nodeAt :: Point -> World -> Maybe Int
nodeAt (x, y) w = case [ k | (k, n) <- IM.toList (wNodes w), dist2 (x, y) (nX n, nY n) < 30 ] of
  (k : _) -> Just k
  [] -> Nothing

-- 客を次の部品へ送る（矢印がなければその場で消える）
sendFrom :: Int -> Double -> World -> IO World
sendFrom k born w = case IM.lookup k (wNodes w) of
  Just n | not (null (nOuts n)) -> do
    i <- randomRIO (0, length (nOuts n) - 1)
    pure $ case drop i (nOuts n) of
      (to : _) | length (wTokens w) < maxTokens -> w { wTokens = Token k to 0 born : wTokens w }
      _ -> w
  _ -> pure w

update :: Double -> World -> IO World
update dt w0 = do
  let w1 = w0 { wT = wT w0 + dt }
  w2 <- foldM (stepNode dt) w1 (IM.keys (wNodes w1))
  moveTokens w2

stepNode :: Double -> World -> Int -> IO World
stepNode dt w k = case IM.lookup k (wNodes w) of
  Just n | nKind n == 1 -> spawn n w
         | nKind n == 2 -> serve n w
  _ -> pure w
  where
    -- ★ ポアソン到着
    spawn n w1
      | nNext n <= wT w1 = do
          w2 <- sendFrom k (nNext n) w1
          g <- expRand (realToFrac (max (nRate n) 0.01))
          let n1 = n { nNext = nNext n + g }
          spawn n1 w2 { wNodes = IM.insert k n1 (wNodes w2) }
      | otherwise = pure w1
    serve n w1 = do
      -- サービス中なら残り時間を減らし、終わったら次へ送る
      (n1, w2) <- case nCur n of
        Just born | nRem n - dt <= 0 -> do
          w' <- sendFrom k born w1
          pure (n { nCur = Nothing, nRem = 0 }, w')
        Just _ -> pure (n { nRem = nRem n - dt }, w1)
        Nothing -> pure (n, w1)
      -- ★ 指数分布のサービス
      n2 <- case (nCur n1, viewl (nQ n1)) of
        (Nothing, born :< rest) -> do
          s <- expRand (realToFrac (max (nRate n1) 0.01))
          pure n1 { nCur = Just born, nQ = rest, nRem = s }
        _ -> pure n1
      let n3 = n2 { nBusyAvg = nBusyAvg n2 + ((if null (nCur n2) then 0 else 1) - nBusyAvg n2) * 0.003
                  , nQAvg = nQAvg n2 + (fromIntegral (Seq.length (nQ n2)) - nQAvg n2) * 0.003 }
      pure w2 { wNodes = IM.insert k n3 (wNodes w2) }

moveTokens :: World -> IO World
moveTokens w0 = do
  let tokens = wTokens w0
  w1 <- foldM arrive w0 { wTokens = [] } tokens
  pure w1
  where
    arrive w t = case (IM.lookup (tFrom t) (wNodes w), IM.lookup (tTo t) (wNodes w)) of
      (Just a, Just b) ->
        let d = max (dist2 (nX a, nY a) (nX b, nY b)) 1
            f = tF t + travelSpeed / d
        in if f < 1 then pure w { wTokens = t { tF = f } : wTokens w } else
           case nKind b of
             2 | Seq.length (nQ b) < maxQueue -> pure w { wNodes = IM.insert (tTo t) b { nQ = nQ b |> tBorn t } (wNodes w) }
               | otherwise -> pure w
             3 -> pure w { wNodes = IM.insert (tTo t) b { nDone = nDone b + 1, nSumT = nSumT b + wT w - tBorn t } (wNodes w) }
             _ -> sendFrom (tTo t) (tBorn t) w
      _ -> pure w

drawAll :: Env -> World -> Picture
drawAll env w = Pictures
  ( -- 矢印
    [ drawArrow n m | n <- nodes, to <- nOuts n, Just m <- [IM.lookup to (wNodes w)] ]
 ++ [ seg 2 (hsba 45 90 100 0.7) (nX n, nY n) (toP5 (envMouse env))
    | wDrag w == Linking, Just k <- [wSel w], Just n <- [IM.lookup k (wNodes w)] ]
    -- 客
 ++ [ dot (lerpF (nX a) (nX b) (tF t)) (lerpF (nY a) (nY b) (tF t)) 8 (hsb 30 80 100)
    | t <- wTokens w, Just a <- [IM.lookup (tFrom t) (wNodes w)], Just b <- [IM.lookup (tTo t) (wNodes w)] ]
    -- 部品
 ++ concatMap drawNode (IM.toList (wNodes w))
 ++ [ txt 20 30 16 (gray 1) ("Place: " ++ concat (take 1 (drop (wTool w) toolNames)) ++ " (1-3)  Click: place  Drag: link  Shift+drag: move  Up/Down: rate  X: delete")
    , txt 20 52 14 (gray 1) ("Space: " ++ (if wPaused w then "resume" else "pause") ++ "  S: save  C: clear all")
    , txt 20 74 14 (hsb 0 0 60) "Desk color: green = free, red = over 90% busy (the % in the box). Busy = incoming flow / mu. Merging crowds, splitting relieves." ] )
  where
    nodes = IM.elems (wNodes w)
    drawNode (k, n) =
      let selected = wSel w == Just k
          edge = if selected then hsb 45 90 100 else hsba 0 0 100 0.4
          wd = if selected then 4 else 2
          x = nX n
          y = nY n
      in case nKind n of
           1 -> [ dot x y 56 (hsb 190 60 60), hoop x y 56 wd edge
                , txtC x (y - 2) 14 (gray 1) "lam"
                , txtC x (y + 15) 14 (gray 1) (nf 1 (nRate n)) ]
           2 -> [ box (x - 30) (y - 26) 60 52 (if nBusyAvg n > 0.9 then hsb 0 70 70 else hsb (140 - nBusyAvg n * 120) 60 55)
                , path wd edge [(x - 30, y - 26), (x + 30, y - 26), (x + 30, y + 26), (x - 30, y + 26), (x - 30, y - 26)]
                , txtC x (y - 2) 14 (gray 1) ("mu=" ++ nf 1 (nRate n))
                , txtC x (y + 16) 14 (gray 1) (nf 0 (nBusyAvg n * 100) ++ "%") ]
                ++ [ dot (x - 30 + fromIntegral (i `mod` 8) * 8) (y + 36 + fromIntegral (i `div` 8) * 8) 6 (hsb 30 80 100)
                   | i <- [0 .. min (Seq.length (nQ n)) 24 - 1] ]
                ++ [ dot (x + 22) (y - 18) 9 (hsb 140 70 100) | Just _ <- [nCur n] ]
                ++ [ txt (x - 30) (y + 74) 14 (hsb 0 0 70) ("Mean line " ++ nf 1 (nQAvg n)) ]
           _ -> [ dot x y 52 (hsb 280 40 50), hoop x y 52 wd edge
                , txtC x (y + 7) 14 (gray 1) (show (nDone n)) ]
                ++ [ txt (x - 34) (y + 46) 14 (hsb 0 0 70) ("Mean stay " ++ nf 1 (nSumT n / fromIntegral (nDone n)) ++ " s") | nDone n > 0 ]

drawArrow :: Node -> Node -> Picture
drawArrow a b = Pictures
  ( seg 2 (hsba 0 0 100 0.5) (nX a + ux * 30, nY a + uy * 30) (ex, ey)
  : fillPoly (hsba 0 0 100 0.6) [ (ex, ey), (ex - ux * 12 - uy * 6, ey - uy * 12 + ux * 6), (ex - ux * 12 + uy * 6, ey - uy * 12 - ux * 6) ]
  : [ txt ((nX a + nX b) / 2 + uy * 12) ((nY a + nY b) / 2 - ux * 12) 14 (hsb 45 80 100) (nf 2 (1 / fromIntegral (length (nOuts a)) :: Float))
    | length (nOuts a) > 1 ] )
  where
    dx = nX b - nX a
    dy = nY b - nY a
    d = max (sqrt (dx * dx + dy * dy)) 1
    ux = dx / d
    uy = dy / d
    ex = nX b - ux * 34
    ey = nY b - uy * 34

mousePress :: Modifiers -> Point -> World -> IO World
mousePress mods pos w
  | my < 90 = pure w
  | otherwise = case nodeAt (mx, my) w of
      Just k -> pure w { wSel = Just k, wDrag = if shift mods == Down then Moving else Linking }
      Nothing -> do
        (k, w1) <- addNode (wTool w) mx my w
        pure w1 { wSel = Just k, wDrag = NoDrag }
  where (mx, my) = toP5 pos

mouseMove :: Point -> World -> World
mouseMove pos w = case (wDrag w, wSel w) of
  (Moving, Just k) ->
    let (mx, my) = toP5 pos
    in w { wNodes = IM.adjust (\n -> n { nX = mx, nY = max 100 my }) k (wNodes w) }
  _ -> w

mouseRelease :: Point -> World -> World
mouseRelease pos w = case (wDrag w, wSel w) of
  (Linking, Just s) -> done (link s)
  _ -> done (wNodes w)
  where
    done nodes = w { wNodes = nodes, wDrag = NoDrag }
    link s = case (IM.lookup s (wNodes w), nodeAt (toP5 pos) w >>= \m -> (,) m <$> IM.lookup m (wNodes w)) of
      (Just from, Just (m, to))
        | m /= s && nKind from /= 3 && nKind to /= 1 ->
            let outs | m `elem` nOuts from = delete m (nOuts from)   -- もう一度つなぐと外れる
                     | otherwise = nOuts from ++ [m]
            in IM.insert s from { nOuts = outs } (wNodes w)
      _ -> wNodes w

adjustSel :: (Float -> Float) -> World -> World
adjustSel f w = case wSel w of
  Just k -> w { wNodes = IM.adjust (\n -> n { nRate = roundTo 10 (f (nRate n)) }) k (wNodes w) }
  Nothing -> w

deleteSel :: World -> World
deleteSel w = case wSel w of
  Just k -> w { wNodes = IM.map (\n -> n { nOuts = filter (/= k) (nOuts n) }) (IM.delete k (wNodes w))
              , wTokens = filter (\t -> tFrom t /= k && tTo t /= k) (wTokens w)
              , wSel = Nothing }
  Nothing -> w

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down mods pos -> mousePress mods pos w
  EventKey (MouseButton LeftButton) Up _ pos -> pure (mouseRelease pos w)
  EventMotion pos -> pure (mouseMove pos w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (adjustSel (\r -> min 5 (r + 0.1)) w)
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (adjustSel (\r -> max 0.1 (r - 0.1)) w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> pure w { wTool = digitToInt c }
    | c `elem` "xX" -> pure (deleteSel w)
    | c `elem` "sS" -> saveSvg "queue_network" env (drawAll env w) >> pure w
    | c `elem` "cC" -> pure w { wNodes = IM.empty, wTokens = [], wSel = Nothing }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 発生源 1 つから窓口 3 つに分けて出口へ。λ をいくつにすると、どこかが赤くなる？（使用率 = 流れ ÷ μ）
-- 2. 窓口から前の窓口へ戻る矢印をつくる（やり直し・再検査）。流れが増えて混むのを確かめる（10 のジャクソン網）
-- 3. 発生源を 2 つにして 1 つの窓口へ合流させる。λ1 + λ2 が μ を超えると？
-- 4. 作品にする：空港・テーマパーク・学食など、身近な場所の流れを再現して名前をつける

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Float -> Float -> Point
pt x y = (x - width / 2, height / 2 - y)

-- マウスの位置（gloss の座標）を p5 の座標に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- 左上の角が (x, y) の長方形
box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)

-- 中心 (x, y)、直径 d の円
dot :: Float -> Float -> Float -> Color -> Picture
dot x y d c = Color c (at (pt x y) (disc (d / 2)))

-- 中心 (x, y)、直径 d、太さ wd の輪
hoop :: Float -> Float -> Float -> Float -> Color -> Picture
hoop x y d wd c = Color c (at (pt x y) (ring (d / 2) wd))

-- 太さ wd の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg wd c (x1, y1) (x2, y2) = Color c (thickLine wd (pt x1 y1) (pt x2 y2))

-- 太さ wd の折れ線
path :: Float -> Color -> [Point] -> Picture
path wd c ps = Color c (thickPath wd [pt x y | (x, y) <- ps])

-- 塗りつぶした多角形（凸形だけ）
fillPoly :: Color -> [Point] -> Picture
fillPoly c ps = Color c (Polygon [pt x y | (x, y) <- ps])

-- 文字：(x, y) は左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 中央ぞろえの文字
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y s c str = txt (x - fromIntegral (length str) * s * 0.36) y s c str

-- p5 の nf(v, 1, d)：小数 d 桁の文字列
nf :: PrintfArg a => Int -> a -> String
nf d v = printf "%.*f" d v

-- 平均 1/rate の指数分布の乱数（次の到着までの時間など）
expRand :: Double -> IO Double
expRand rate = do
  u <- randomRIO (0, 1 :: Double)
  pure (-log (max 1e-12 (1 - u)) / rate)

-- p5 の round(v * k) / k
roundTo :: Float -> Float -> Float
roundTo k v = fromIntegral (round (v * k) :: Int) / k
