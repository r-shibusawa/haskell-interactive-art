-- ============================================
-- 10 木の高さとランダムな順番
-- 二分探索木の操作はすべて O(高さ)。同じ n 個のキーでも、入れる順番で高さが大きく変わる
-- ランダムな順番で入れた木の高さの期待値は O(log n)。小さい順に入れると高さ n（ただの 1 列）
-- 操作：1 = ランダムな順 / 2 = 小さい順 / 3 = ほぼ小さい順（少しだけ入れかえ）/ ↑↓ = n / スペース = 一時停止 / C = 記録を消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_)
import Data.Char (digitToInt, toLower)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)

firstN :: Int
firstN = 63          -- ★ キーの個数

msPerFrame :: Double
msPerFrame = 6       -- ★ 1 フレームで実験に使う時間（ミリ秒）

-- | ふつうの二分探索木（すべて正格なので、入れた時点で木が完成する）
data Tree = E | N !Int !Tree !Tree

data Sample = Sample { sTree :: !Tree, sHeight :: !Int }

data World = World
  { n      :: !Int
  , mode   :: !Int
  , paused :: !Bool
  , hist   :: !(IM.IntMap Int)   -- 高さ → 回数
  , trials :: !Int
  , sumH   :: !Int
  , sample :: !(Maybe Sample)
  }

initialWorld :: World
initialWorld = World firstN 1 False IM.empty 0 0 Nothing

-- | 入れる順番を作る
makeOrder :: Int -> Int -> IO (U.Vector Int)
makeOrder size md = do
  v <- U.thaw (U.enumFromN 1 size)
  case md of
    1 -> forM_ [size - 1, size - 2 .. 1] $ \i -> do   -- シャッフル
           j <- randomRIO (0, i)
           M.swap v i j
    3 -> forM_ [1 .. (size + 9) `div` 10] $ \_ -> do  -- n / 10 回だけ入れかえ
           i <- randomRIO (0, size - 1)
           j <- randomRIO (0, size - 1)
           M.swap v i j
    _ -> pure ()
  U.unsafeFreeze v

-- | キーを入れて、入った深さも返す
insertKey :: Int -> Int -> Tree -> (Tree, Int)
insertKey k !d E = (N k E E, d)
insertKey k !d (N x l r)
  | k < x = case insertKey k (d + 1) l of (l', h) -> (N x l' r, h)
  | otherwise = case insertKey k (d + 1) r of (r', h) -> (N x l r', h)

build :: U.Vector Int -> Sample
build order =
  let add (Sample t h) k = case insertKey k 1 t of (t', d) -> Sample t' (max h d)
  in U.foldl' add (Sample E 0) order

-- | 時間いっぱい実験をくり返す（少なくとも 1 回）
step :: Env -> Float -> World -> IO World
step env _ w0
  | paused w0 = pure w0
  | otherwise = do
      t0 <- getMonotonicTime
      let loop :: Int -> World -> IO World
          loop t w = do
            now <- getMonotonicTime
            if t > 0 && (now - t0) * 1000 >= msPerFrame then pure w
            else do
              order <- makeOrder (n w) (mode w)
              let r = build order
                  h = sHeight r
                  keep = t == 0 && (envFrame env `mod` 30 == 1 || isNothing' (sample w))
              loop (t + 1) w { hist = IM.insertWith (+) h 1 (hist w), trials = trials w + 1, sumH = sumH w + h
                             , sample = if keep then Just r else sample w }
      loop 0 w0

isNothing' :: Maybe a -> Bool
isNothing' = maybe True (const False)

-- | 中間順の番号と深さを振る
layTree :: Tree -> Int -> Int -> [(Int, Int, Int)] -> ([(Int, Int, Int)], Int)
layTree E _ i acc = (acc, i)
layTree (N k l r) d i acc =
  let (a1, i1) = layTree l (d + 1) i acc
  in layTree r (d + 1) (i1 + 1) ((k, i1, d) : a1)

drawSample :: Env -> Int -> Sample -> Picture
drawSample env size s =
  let (lay, _) = layTree (sTree s) 0 0 []
      pos = IM.fromList [(k, (i, d)) | (k, i, d) <- lay]
      maxD = maximum (0 : [d | (_, _, d) <- lay])
      wd = envW env * 0.55
      ht = envH env - 300
      gap = min 40 (ht / fi (maxD + 1))
      place k = let (i, d) = IM.findWithDefault (0, 0) k pos
                in (40 + ((fi i + 0.5) / fi size) * wd, 150 + fi d * gap)
      edges t = case t of
        E -> []
        N k l r -> [ (place k, place c) | c <- childKey l ++ childKey r ] ++ edges l ++ edges r
      childKey t = case t of
        E -> []
        N k _ _ -> [k]
      edgePic ((x1, y1), (x2, y2)) = Line [scr env x1 y1, scr env x2 y2]
  in Pictures
       [ Color (rgb255 90 150 170) (Pictures (map edgePic (edges (sTree s))))
       , Color (rgb255 120 220 240) (Pictures [at (scr env x y) (disc 3.5) | (k, _, _) <- lay, let (x, y) = place k])
       , txt env 40 128 15 (gray 0.78) ("Sample tree (height " ++ show (sHeight s) ++ ")")
       ]

draw :: Env -> World -> Picture
draw env w =
  let size = n w
      gx = envW env * 0.62
      gw = envW env * 0.34
      gy = envH env - 160
      gh = envH env * 0.45
      maxC = maximum (1 : IM.elems (hist w))
      showMax | mode w == 2 = size + 2
              | otherwise = min (size + 2) (maximum (30 : IM.keys (hist w)) + 3)
      bars = [ let x = gx + (fi h / fi showMax) * gw
                   hh = (fi c / fi maxC) * gh
               in box env (x - 3) (gy - hh) (max 3 (gw / fi showMax - 2)) hh 0 (rgb255 120 220 160)
             | (h, c) <- IM.toList (hist w) ]
      log2 v = logBase 2 (fi v) :: Float
      lx = gx + (log2 (size + 1) / fi showMax) * gw
      modeName = case mode w of
        1 -> "random"
        2 -> "sorted"
        _ -> "nearly sorted"
      note = case mode w of
        2 -> "Sorted input always goes to the far right, so height = n. Search becomes O(n)!"
        3 -> "A few swaps still leave it quite tall. Real data is often 'nearly sorted' like this"
        _ -> "Random order: height is about 2-3 times log2 n. But we rarely choose the order -> red-black trees"
  in Pictures
       ( [ txt env 30 24 26 (gray 0.94) "Insertion Order and Tree Height"
         , txt env 30 60 14 (rgb255 150 175 190) "1 = random  2 = sorted  3 = nearly sorted  Up/Down = n  Space = pause  C = clear the record"
         , txt env 30 82 17 (gray 1)
             ("n = " ++ show size ++ "   order: " ++ modeName ++ "   trials " ++ show (trials w) ++ "   average height "
              ++ (if trials w > 0 then fmt 2 (fi (sumH w) / fi (trials w)) else "-"))
         , txt env 30 104 15 (gray 0.86)
             ("log2 n = " ++ fmt 2 (log2 size) ++ "   minimum height ceil(log2(n+1)) = " ++ show (ceiling (log2 (size + 1)) :: Int))
         , maybe Blank (drawSample env size) (sample w)
         , seg env 1 (gray 0.31) gx gy (gx + gw) gy
         ]
         ++ bars ++
         [ seg env 2 (rgb255 255 200 100) lx gy lx (gy - gh)
         , txtC env lx (gy + 24) 14 (rgb255 255 200 100) "log2(n+1)"
         , txt env gx (gy - gh - 26) 15 (gray 0.78) ("Height distribution (x axis = height 0-" ++ show showMax ++ ")")
         , txt env 30 (envH env - 60) 16 (rgb255 255 225 140) note
         ] )

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey k Down _ _ -> pure (onKey (normKey k) w)
  _ -> pure w

onKey :: Key -> World -> World
onKey key w = case key of
  Char c | c >= '1' && c <= '3' -> resetStats w { mode = digitToInt c }
  SpecialKey KeyUp -> resetStats w { n = min 1023 (n w * 2 + 1) }
  SpecialKey KeyDown -> resetStats w { n = max 3 ((n w - 1) `div` 2) }
  SpecialKey KeySpace -> w { paused = not (paused w) }
  Char 'c' -> resetStats w
  _ -> w

resetStats :: World -> World
resetStats w = w { hist = IM.empty, trials = 0, sumH = 0, sample = Nothing }

main :: IO ()
main = runSketch (sketch "11-10 Insertion Order and Height" initialWorld)
  { skBack  = rgb255 14 18 22
  , skStep  = step
  , skDraw  = draw
  , skEvent = onEvent
  }

-- ---------- やってみよう ----------
-- 1. ↑ で n = 1023 にする。ランダムなら高さは 20〜25 くらい。小さい順だと 1023！
-- 2. 3（ほぼ小さい順）で入れかえの回数（n / 10）を変えてみよう
-- 3. 「どんな順番でも高さ O(log n)」を保証するのが 11・12 の赤黒木


-- ==================================================
-- ここから下は「画面の道具」です
-- p5.js と同じ「左上が原点、y は下向き」の座標で考えて、ここで gloss の座標に直します
-- ==================================================
fi :: Int -> Float
fi = fromIntegral

scr :: Env -> Float -> Float -> Point
scr env x y = (x - envW env / 2, envH env / 2 - y)

-- | 左上をそろえた文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y sz c s = let (px, py) = scr env x (y + sz) in label px py sz c s

-- | 中央をそろえた文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y sz c s = let (px, py) = scr env x y in labelC px py sz c s

-- | 太さのある線分
seg :: Env -> Float -> Color -> Float -> Float -> Float -> Float -> Picture
seg env sw c x1 y1 x2 y2 = Color c (thickLine sw (scr env x1 y1) (scr env x2 y2))

-- | 角の丸い長方形のかど（左上の x, y、幅、高さ、丸みの半径）
roundCorners :: Float -> Float -> Float -> Float -> Float -> [(Float, Float)]
roundCorners x y w h r0 =
  let r = max 0 (minimum [r0, w / 2, h / 2])
      arcAt cx cy a0 = [(cx + r * cos a, cy + r * sin a) | i <- [0 .. 3 :: Int], let a = a0 + fi i * pi / 6]
  in arcAt (x + w - r) (y + r) (-pi / 2) ++ arcAt (x + w - r) (y + h - r) 0
     ++ arcAt (x + r) (y + h - r) (pi / 2) ++ arcAt (x + r) (y + r) pi

-- | 塗りつぶした角丸の長方形（p5.js の rect と同じく左上で指定）
box :: Env -> Float -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h r c = Color c (Polygon [scr env px py | (px, py) <- roundCorners x y w h r])

-- | 小数 n 桁の文字列（p5.js の nf に相当）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""

-- | キーをそろえる：英字は小文字に。Enter・Backspace・スペースは環境によって来かたが違うので 1 つにまとめる
normKey :: Key -> Key
normKey k = case k of
  Char ' ' -> SpecialKey KeySpace
  Char '\r' -> SpecialKey KeyEnter
  Char '\n' -> SpecialKey KeyEnter
  Char '\b' -> SpecialKey KeyBackspace
  Char '\DEL' -> SpecialKey KeyBackspace
  SpecialKey KeyDelete -> SpecialKey KeyBackspace
  SpecialKey KeyPadEnter -> SpecialKey KeyEnter
  Char c -> Char (toLower c)
  _ -> k
