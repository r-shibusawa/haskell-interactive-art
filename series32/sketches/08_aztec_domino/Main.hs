-- ============================================
-- 08 ドミノ・タイリングと北極円
-- 階段状のダイヤモンド（アステカ・ダイヤモンド）を 1×2 のドミノでランダムに埋める
--   「ドミノ・シャッフル」：ぶつかるペアを消す → 全部 1 マス動かす → 空いた 2×2 にランダムに 2 枚置く、をくり返す
--   大きくすると、四隅はレンガのように固まり、真ん中だけがランダム。その境目がほぼ円 = 北極円（arctic circle）
-- ↑↓ = 目標の大きさ / スペース = 作り直す / F = 一気に / A = 円を表示 / S = 保存
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import GHC.Clock (getMonotonicTime)

-- ドミノ (x, y, t)  t: 0 = 北, 1 = 南（横向き）, 2 = 東, 3 = 西（縦向き）
type Doms = U.Vector (Int, Int, Int)

data World = World
  { wTarget :: !Int       -- ★ ダイヤモンドの大きさ（次数 n）
  , wN      :: !Int       -- いまの大きさ
  , wDoms   :: !Doms
  , wCircle :: !Bool
  , wImg    :: Picture    -- 大きいときの画像（ドミノが変わったときだけ作る）
  , wImgK   :: !Float     -- 画像 1 画素が何マスぶんか
  , wTurbo  :: !Double    -- F：この時刻まで全力で育てる
  }

world0 :: World
world0 = World 80 0 U.empty True Blank 1 0

maxImage :: Int
maxImage = 282            -- 画像の 1 辺の上限（8 万画素まで）

main :: IO ()
main = runSketch (sketch "32-08 Aztec Diamond" world0)
  { skBack  = backCol
  , skStep  = \_ _ w -> stepWorld w
  , skDraw  = drawAll
  , skEvent = onEvent
  }

backCol :: Color
backCol = rgb255 18 20 28

-- ★ 色：北・南・東・西（動く向きで色分け）
colOf :: Int -> (Word8, Word8, Word8)
colOf t = case t of
  0 -> (250, 200, 60)
  1 -> (230, 80, 70)
  2 -> (80, 190, 120)
  _ -> (70, 130, 220)

toColor :: (Word8, Word8, Word8) -> Color
toColor (r, g, b) = rgb255 (fromIntegral r) (fromIntegral g) (fromIntegral b)

stepWorld :: World -> IO World
stepWorld w
  | wN w > wTarget w = pure w { wDoms = U.empty, wN = 0, wImg = Blank }
  | wN w >= wTarget w = pure w { wTurbo = 0 }
  | wTurbo w > 0 = do
      t <- getMonotonicTime
      if t > wTurbo w then pure w { wTurbo = 0 } else makeImage <$> growUntil (t + 0.02) w
  | otherwise = makeImage <$> grow w

-- 画面が止まらないよう、1 フレームに 20 ミリ秒まで育てる
growUntil :: Double -> World -> IO World
growUntil tEnd w
  | wN w >= wTarget w = pure w
  | otherwise = do
      t <- getMonotonicTime
      if t > tEnd then pure w else grow w >>= growUntil tEnd

-- ★ ドミノ・シャッフル（大きさ n → n + 1）
grow :: World -> IO World
grow w = do
  let n0 = wN w
      w0 = 2 * (n0 + 2)
      off = n0 + 2
      idx x y = (y + off) * w0 + (x + off)
      -- その左下マスにあるドミノの種類
      grid = U.replicate (w0 * w0) (-1 :: Int)
               U.// [ (idx x y, t) | (x, y, t) <- U.toList (wDoms w) ]
      at' x y = grid U.! idx x y
      -- 1. 向かい合ってぶつかるペアを消す（北の上に南、東の右に西）
      collides (x, y, t) = case t of
        0 -> at' x (y + 1) == 1
        1 -> at' x (y - 1) == 0
        2 -> at' (x + 1) y == 3
        _ -> at' (x - 1) y == 2
      keep0 = U.filter (not . collides) (wDoms w)
      -- 2. 全部 1 マス、自分の向きに動かす
      move (x, y, t) = case t of
        0 -> (x, y + 1, t)
        1 -> (x, y - 1, t)
        2 -> (x + 1, y, t)
        _ -> (x - 1, y, t)
      keep = U.map move keep0
      -- 3. 空いた 2×2 にランダムに 2 枚（横 2 枚 = 南と北、縦 2 枚 = 西と東）
      n = n0 + 1
      w2 = 2 * (n + 1)
      off2 = n + 1
      id2 x y = (y + off2) * w2 + (x + off2)
  occ <- UM.replicate (w2 * w2) (0 :: Word8)
  U.forM_ keep $ \(x, y, t) -> do
    UM.write occ (id2 x y) 1
    if t < 2 then UM.write occ (id2 (x + 1) y) 1 else UM.write occ (id2 x (y + 1)) 1
  let visit acc (x, y) = do
        o <- UM.read occ (id2 x y)
        if abs (fromIntegral x + 0.5) + abs (fromIntegral y + 0.5) > (fromIntegral n :: Float) || o /= 0
          then pure acc
          else do
            r <- randomRIO (0, 1) :: IO Float
            mapM_ (\i -> UM.write occ i 1) [id2 x y, id2 (x + 1) y, id2 x (y + 1), id2 (x + 1) (y + 1)]
            pure (if r < 0.5 then (x, y, 1) : (x, y + 1, 0) : acc
                  else (x, y, 3) : (x + 1, y, 2) : acc)
  added <- foldM visit [] [ (x, y) | y <- [-n .. n - 1], x <- [-n .. n - 1] ]
  pure w { wN = n, wDoms = keep U.++ U.fromList added }

-- 1 マス = 1 画素の画像にしてから拡大する（大きいときは間引く）
makeImage :: World -> World
makeImage w
  | wN w <= 40 = w { wImg = Blank }
  | otherwise =
      let n = wN w
          full = 2 * n
          cell x y = (n - 1 - y) * full + (x + n)
          marks (x, y, t)
            | t < 2 = [(cell x y, t), (cell (x + 1) y, t)]
            | otherwise = [(cell x y, t), (cell x (y + 1), t)]
          grid = U.replicate (full * full) (-1 :: Int) U.// concatMap marks (U.toList (wDoms w))
          m = min full maxImage
          step = fromIntegral full / fromIntegral m :: Float
          pick i = floor (fromIntegral i * step) :: Int
          pixel x y = case grid U.! (pick y * full + pick x) of
            t | t < 0 -> (18, 20, 28)
              | otherwise -> colOf t
      in w { wImg = pixelImage m m 1 pixel, wImgK = step }

drawAll :: Env -> World -> Picture
drawAll env w =
  let n = wN w
      cs = min (envW env) (envH env - 120) * 0.92 / (2 * fromIntegral (max n 1))
      cy = -30                   -- 中心を少し下げる
      gap = if n > 25 then 0.5 else 1
      domino (x, y, t)
        | t < 2 = Color (toColor (colOf t)) (rect ((fx + 1) * cs) ((fy + 0.5) * cs + cy) (2 * cs - 2 * gap) (cs - 2 * gap))
        | otherwise = Color (toColor (colOf t)) (rect ((fx + 0.5) * cs) ((fy + 1) * cs + cy) (cs - 2 * gap) (2 * cs - 2 * gap))
        where fx = fromIntegral x
              fy = fromIntegral y
      body | n <= 40 = Pictures (map domino (U.toList (wDoms w)))
           | otherwise = let k = wImgK w * cs in Translate 0 cy (Scale k k (wImg w))
      ringPic = if wCircle w && n > 0
        then at (0, cy) (Color white (ring (2 * fromIntegral n * cs / sqrt 2 / 2) 2))
        else Blank
  in Pictures
    [ body
    , ringPic
    , txt env 20 36 20 white ("Random domino tiling of an Aztec diamond   size " ++ show n ++ " / target " ++ show (wTarget w) ++ " (Up/Down)")
    , txt env 20 62 15 (gray 0.75) (show (U.length (wDoms w)) ++ " dominoes. Color = direction (yellow N, red S, green E, blue W). White circle = arctic circle (A)")
    , txt env 20 84 15 (gray 0.75) "Outside the circle it freezes into brickwork; only the inside is random like a liquid"
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wTarget = min 240 (wTarget w + 10) }
  EventKey (SpecialKey KeyDown) Down _ _ ->
    let t = max 1 (wTarget w - 10)
    in pure (if wN w > t then w { wTarget = t, wDoms = U.empty, wN = 0, wImg = Blank } else w { wTarget = t })
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wDoms = U.empty, wN = 0, wImg = Blank }
  EventKey (Char c) Down _ _
    | c == 'f' || c == 'F' -> do
        t <- getMonotonicTime
        pure w { wTurbo = t + 2 }
    | c == 'a' || c == 'A' -> pure w { wCircle = not (wCircle w) }
    | c == 's' || c == 'S' -> saveSvg "aztec" env (drawAll env w) >> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. wTarget を 4 にして、1 枚ずつ増えていく様子をスペースで何度も見よう
-- 2. 大きさ 200 で、円の外側に「レンガ」の 4 色の領域ができるのを確かめよう（なぜ固まる？）
-- 3. r < 0.5 を 0.8 にすると「かたより」ができる。円はどんな形にゆがむ？
-- 4. 大きさ n のダイヤモンドの埋め方は 2^(n(n+1)/2) 通り。n = 10 で何通り？


-- ==================================================
-- ここから下は「道具」
-- ==================================================
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c
