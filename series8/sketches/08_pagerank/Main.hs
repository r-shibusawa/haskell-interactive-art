-- ============================================
-- 08 ページランク（Google の検索順位のもと）
-- 「たくさんリンクされているページは重要。重要なページからのリンクはもっと重要」
-- ランダムに動き回る「サーファー」が、よく滞在するページほど重要 ＝ ページランク
-- クリック → 別の点へドラッグでリンク（矢印） / スペースでサーファーを放つ / R でやり直し
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (chr)
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

data Surfer = Surfer { sAt, sTo :: !Int, sT :: !Float }

data World = World
  { wNodes    :: !(V.Vector Point)
  , wLinks    :: ![(Int, Int)]           -- (from, to)（向きあり）
  , wRank     :: !(U.Vector Float)
  , wVisits   :: !(U.Vector Int)
  , wSurfers  :: ![Surfer]
  , wDragFrom :: !Int
  }

pageCount :: Int
pageCount = 8

damping :: Float
damping = 0.85             -- ★ リンクをたどる確率（1−d でランダムジャンプ）

maxSurfers :: Int
maxSurfers = 500           -- 増えすぎないように上限

startLinks :: [(Int, Int)]
startLinks = [(0, 1), (1, 2), (2, 0), (3, 0), (4, 0), (5, 0), (6, 2), (7, 6), (2, 3)]

main :: IO ()
main = runSketch (sketch "08-08 PageRank" (reset env0Like))
  { skBack  = hsb 220 40 10
  , skStart = pure . reset
  , skStep  = stepWorld
  , skDraw  = drawWorld
  , skEvent = onEvent
  }
  where env0Like = Env 1280 800 (0, 0) False 0 0 mempty

reset :: Env -> World
reset env = World nodes startLinks (computeRank startLinks) (U.replicate pageCount 0) [] (-1)
  where
    nodes = V.generate pageCount $ \i ->
      let a = tau * fromIntegral i / fromIntegral pageCount
      in (envW env * 0.3 + cos a * 230, envH env * 0.55 + sin a * 230)

outLinks :: [(Int, Int)] -> Int -> [Int]
outLinks links i = [b | (a, b) <- links, a == i]

-- ★ ページランク：全ページ同じ値から始めて、「リンク先に自分の値を分配する」を何回もくり返す
computeRank :: [(Int, Int)] -> U.Vector Float
computeRank links = foldl' (\r _ -> iter r) (U.replicate pageCount (1 / nF)) [1 .. 50 :: Int]
  where
    nF = fromIntegral pageCount :: Float
    iter rank = U.accum (+) (U.replicate pageCount ((1 - damping) / nF)) (concatMap (share rank) [0 .. pageCount - 1])
    share rank i = case outLinks links i of
      [] -> [(j, damping * rank U.! i / nF) | j <- [0 .. pageCount - 1]]               -- 行き止まりは全員へ
      out -> [(b, damping * rank U.! i / fromIntegral (length out)) | b <- out]         -- リンク先に等分

-- サーファーを動かす
moveSurfer :: [(Int, Int)] -> Surfer -> IO (Surfer, [Int])
moveSurfer links s
  | t < 1 = pure (s { sT = t }, [])
  | otherwise = do
      let at' = sTo s
          out = outLinks links at'
      r <- randomRIO (0, 1 :: Float)
      next <- if r < 0.85 && not (null out)             -- ★ 85% でリンク、15% でジャンプ
                then choose out
                else randomRIO (0, pageCount - 1)
      pure (Surfer at' next 0, [at'])
  where t = sT s + 0.04

stepWorld :: Env -> Float -> World -> IO World
stepWorld _ _ w = do
  results <- mapM (moveSurfer (wLinks w)) (wSurfers w)
  let arrived = concatMap snd results
  pure w { wSurfers = map fst results
         , wVisits = U.accum (+) (wVisits w) [(a, 1) | a <- arrived] }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (map linkPic (wLinks w) ++ dragPic ++ nodePics ++ surferPics ++ bars ++ hud)
  where
    rank = wRank w
    maxR = U.foldl' max 1e-6 rank
    rankHue i = remap (rank U.! i) 0 maxR 200 40
    pos i = wNodes w V.! i
    gpos i = let (x, y) = pos i in toG env x y
    -- リンク（矢印）
    linkPic (a, b) =
      let (ax, ay) = pos a
          (bx, by) = pos b
          ang = atan2 (by - ay) (bx - ax)
          r = 18 + sqrt (rank U.! b) * 60
          ex = bx - cos ang * r
          ey = by - sin ang * r
          tip d = toG env (ex - cos (ang + d) * 14) (ey - sin (ang + d) * 14)
      in Color (hsb 0 0 55) (Pictures
           [ thickLine 2 (toG env ax ay) (toG env ex ey)
           , Polygon [toG env ex ey, tip (-0.4), tip 0.4] ])
    dragPic
      | wDragFrom w >= 0 = [Color (hsb 45 90 100) (thickLine 2 (gpos (wDragFrom w)) (envMouse env))]
      | otherwise = []
    -- ノード：大きさ = ページランク
    nodePics = concatMap nodePic [0 .. pageCount - 1]
    nodePic i =
      let (gx, gy) = gpos i
      in [ Color (hsb (rankHue i) 80 100) (at (gx, gy) (disc (15 + sqrt (rank U.! i) * 60)))
         , labelC gx (gy + 10) 14 (hsb 0 0 10) [pageName i]
         , labelC gx (gy - 10) 14 (hsb 0 0 10) (nf 2 (rank U.! i)) ]
    surferPics =
      [ let (ax, ay) = pos (sAt s)
            (bx, by) = pos (sTo s)
        in Color (hsb 45 90 100) (at (toG env (lerpF ax bx (sT s)) (lerpF ay by (sT s))) (disc 6))
      | s <- wSurfers w ]
    -- 右：訪問回数（サーファーの統計）とページランクの比較
    rx = envW env * 0.66
    rw = envW env - rx - 50
    totalV = max 1 (U.sum (wVisits w))
    bars = txt env rx (envH env * 0.22) 16 (hsb 0 0 100) "PageRank vs surfers' visit share"
         : concatMap bar [0 .. pageCount - 1]
    bar i =
      let y = envH env * 0.26 + fromIntegral i * 38
          share = fromIntegral (wVisits w U.! i) / fromIntegral totalV :: Float
      in [ txt env rx (y + 14) 14 (hsb 0 0 70) [pageName i]
         , Color (hsb (rankHue i) 80 100) (rectTL env (rx + 24) y (rank U.! i * rw * 1.6) 12)
         , Color (hsb 45 90 100) (rectTL env (rx + 24) (y + 15) (share * rw * 1.6) 12) ]
    hud =
      [ txt env 20 40 18 (hsb 0 0 100) ("Drag: add link   Space: release surfers (yellow), " ++ show (length (wSurfers w)) ++ " now   R: reset")
      , txt env 20 68 15 (hsb 0 0 60) "Big = high PageRank: linked from many pages, or from important ones (compare A and C)."
      ]

pageName :: Int -> Char
pageName i = chr (65 + i)

nearest :: World -> Point -> Int
nearest w m = maybe (-1) id (V.findIndex (\p -> dist2 m p < 35) (wNodes w))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | snd m > 80 -> pure w { wDragFrom = nearest w m }
    | otherwise -> pure w
  EventKey (MouseButton LeftButton) Up _ _ -> pure (release w)
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    new <- replicateM 5 (Surfer 0 <$> randomRIO (0, pageCount - 1) <*> pure 0)
    pure w { wSurfers = take maxSurfers (wSurfers w ++ new) }
  EventKey (Char c) Down _ _ | c `elem` "rR" -> pure (reset env)
  _ -> pure w
  where
    m = mouseXY env
    release wd =
      let from = wDragFrom wd
          to = nearest wd m
      in if from >= 0 && to >= 0 && to /= from && (from, to) `notElem` wLinks wd
           then let links = wLinks wd ++ [(from, to)]
                in wd { wLinks = links, wRank = computeRank links, wDragFrom = -1 }
           else wd { wDragFrom = -1 }

-- ---------- やってみよう ----------
-- 1. A は 4 本リンクされている。C は 2 本。でも C のほうが大きくなるのはなぜ？（A からリンクされているから）
-- 2. 2 点だけでリンクし合う「ずる」をしても、d = 0.85 のおかげで大きくなりすぎない
-- 3. サーファーをたくさん放って長く待つと、実測がページランクに近づく

-- ---------- 道具 ----------

-- リストからランダムに 1 つ選ぶ（空でないときだけ呼ぶ。空なら 0）
choose :: [Int] -> IO Int
choose [] = pure 0
choose xs = do
  k <- randomRIO (0, length xs - 1)
  pure (case drop k xs of
          (x : _) -> x
          [] -> 0)

-- 左上が (x, y) の長方形（p5.js の rect）
rectTL :: Env -> Float -> Float -> Float -> Float -> Picture
rectTL env x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in rect gx gy w h

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 小数を d 桁で文字にする（p5.js の nf）
nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
