-- ============================================
-- 41-06 鳩の巣原理
-- n 個の巣に n+1 羽の鳩を入れると、どう入れても「2 羽以上入った巣」が必ずできる
-- 応用：n+1 個の整数を n で割った余りは n 種類しかない → 余りが同じペアがあり、その差は n の倍数
-- 操作：↑↓ = 巣の数 n（2〜9）/ スペース = ランダムに入れ直す / ドラッグ = 鳩を別の巣へ / M = 整数モード / N = 新しい数
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.List (foldl', intercalate)
import qualified Data.Map.Strict as Map

startN :: Int
startN = 5                 -- ★ 巣の数（鳩は n+1 羽）

data Pigeon = Pigeon
  { pHole :: !Int
  , pX, pY :: !Float
  , pHue :: !Float
  }

data World = World
  { wN        :: !Int
  , wPigeons  :: ![Pigeon]
  , wNumbers  :: !Bool       -- True で整数モード
  , wDragging :: !Int        -- ドラッグ中の鳩の番号（なければ -1）
  , wNums     :: ![Int]      -- 整数モードの数
  , wBounce   :: !Float
  }

main :: IO ()
main = runSketch (sketch "41-06 Pigeonhole principle" (World startN [] False (-1) [] 0))
  { skBack  = hsb 215 30 11
  , skStart = \env -> randomize env (World startN [] False (-1) [] 0) >>= newNumbers
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

-- 巣の長方形 (x, y, 幅, 高さ)
holeRect :: Env -> Int -> Int -> (Float, Float, Float, Float)
holeRect env n h =
  let fn = fromIntegral n
      w = min 150 ((envW env - 80) / fn - 14)
      x = envW env / 2 - (fn * (w + 14) - 14) / 2 + fromIntegral h * (w + 14)
  in (x, envH env * 0.42, w, 190)

randomize :: Env -> World -> IO World
randomize env w = do
  let n = wN w
  ps <- mapM (\i -> do
                h <- randomRIO (0, n - 1)
                x <- randomRIO (0, envW env)
                pure (Pigeon h x (-40) (fromIntegral i * 360 / fromIntegral (n + 1))))
             [0 .. n]
  pure w { wPigeons = ps, wDragging = -1 }

-- 1〜60 の中から、ちがう数を n+1 個選ぶ
newNumbers :: World -> IO World
newNumbers w = do
  xs <- go []
  pure w { wNums = xs }
  where
    go acc
      | length acc >= wN w + 1 = pure (reverse acc)
      | otherwise = do
          v <- randomRIO (1, 60 :: Int)
          go (if v `elem` acc then acc else v : acc)

-- 各巣の鳩の数（ドラッグ中の鳩は数えない）
holeCounts :: World -> Map.Map Int Int
holeCounts w = foldl' add (Map.fromList [(h, 0) | h <- [0 .. wN w - 1]]) (zip [0 ..] (wPigeons w))
  where add m (i, p) = if i == wDragging w then m else Map.insertWith (+) (pHole p) 1 m

step :: Env -> Float -> World -> IO World
step env _ w = pure w { wBounce = wBounce w + 0.08, wPigeons = reverse moved }
  where
    n = wN w
    (_, moved) = foldl' one (Map.empty, []) (zip [0 ..] (wPigeons w))
    one (cnt, acc) (i, p)
      | i == wDragging w =
          let (mx, my) = mouseP env in (cnt, p { pX = mx, pY = my } : acc)
      | otherwise =
          -- 巣の中の何番目かで目標位置を決め、目標へ近づく
          let slot = Map.findWithDefault 0 (pHole p) cnt :: Int
              (rx, ry, rw, rh) = holeRect env n (pHole p)
              tx = rx + 24 + fromIntegral (slot `mod` 3) * (rw - 48) / 2
              ty = ry + rh - 28 - fromIntegral (slot `div` 3) * 40
          in ( Map.insert (pHole p) (slot + 1) cnt
             , p { pX = pX p + (tx - pX p) * 0.12, pY = pY p + (ty - pY p) * 0.12 } : acc )

draw :: Env -> World -> Picture
draw env w = if wNumbers w then drawNumbers env w else drawPigeons env w

holeBox :: Env -> Bool -> (Float, Float, Float, Float) -> Picture
holeBox env crowded (x, y, rw, rh) =
  box env (if crowded then hsb 20 60 50 else hsb 220 20 30) x y rw rh

drawPigeons :: Env -> World -> Picture
drawPigeons env w =
  let n = wN w
      cnt = holeCounts w
      count h = Map.findWithDefault 0 h cnt
      hole h =
        let r@(x, y, rw, rh) = holeRect env n h
            crowded = count h >= 2
        in Pictures
             [ holeBox env crowded r
             , if crowded then Color (hsb 30 90 100) (frameBox env 3 (x - 3) (y - 3) (rw + 6) (rh + 6)) else Blank
             , txtC env (x + rw / 2) (y - 10) 14 (hsb 0 0 85) ("Nest " ++ show (h + 1) ++ ": " ++ show (count h)) ]
      bird (i, p) = drawBird env (pX p) (pY p + sin (wBounce w + fromIntegral i) * 2) (pHue p) (i == wDragging w)
      overs = length (filter (>= 2) (Map.elems cnt))
      maxC = foldl' max 0 (Map.elems cnt)
  in Pictures
       [ Pictures (map hole [0 .. n - 1])
       , Pictures (map bird (zip [0 :: Int ..] (wPigeons w)))
       , txt env 20 40 16 (hsb 0 0 100)
           ("Nests n = " ++ show n ++ " (Up/Down)   " ++ show (n + 1) ++ " pigeons   Space: reshuffle   Drag: move a bird   M: numbers")
       , txt env 20 68 14 (hsb 0 0 60)
           ("Crowded nests (red) can never reach 0. The fullest has " ++ show maxC
            ++ " birds (at least ceil(" ++ show (n + 1) ++ "/" ++ show n ++ ") = 2)")
       , txt env 20 110 20 (hsb 30 80 100) ("Nests with 2+ birds: " ++ show overs ++ "   -> always at least 1")
       ]

drawNumbers :: Env -> World -> Picture
drawNumbers env w =
  let n = wN w
      -- 余りごとの巣
      res h = [v | v <- wNums w, v `mod` n == h]
      hole h =
        let r@(x, y, rw, _) = holeRect env n h
            vals = res h
        in Pictures
             ( holeBox env (length vals >= 2) r
             : txtC env (x + rw / 2) (y - 10) 14 (hsb 0 0 85) ("Rem " ++ show h)
             : [ txtC env (x + rw / 2) (y + 36 + fromIntegral j * 32) 22 (hsb 0 0 100) (show v)
               | (j, v) <- zip [0 :: Int ..] vals ] )
      -- 差が n の倍数のペアを 1 つ見つける
      pairs = [(a, b) | h <- [0 .. n - 1], (a : b : _) <- [res h]]
      hg = envH env
  in Pictures
       [ Pictures (map hole [0 .. n - 1])
       , txt env 20 40 16 (hsb 0 0 100)
           ("n = " ++ show n ++ " (Up/Down)   numbers: " ++ intercalate ", " (map show (wNums w)) ++ "   N: new   M: pigeons")
       , txt env 20 68 14 (hsb 0 0 60)
           (show (length (wNums w)) ++ " integers divided by " ++ show n ++ " give only " ++ show n
            ++ " remainders -> a pair with the same remainder must exist")
       , case pairs of
           ((p, q) : _) ->
             let a = max p q
                 b = min p q
             in txt env 20 110 20 (hsb 30 80 100)
                  ("Pair " ++ show a ++ " and " ++ show b ++ ": difference " ++ show (a - b) ++ " = "
                   ++ show n ++ " x " ++ show ((a - b) `div` n) ++ " -> a multiple of " ++ show n ++ "!")
           [] -> Blank
       , txt env 20 (hg - 60) 14 (hsb 0 0 55)
           "More examples: 5 people and 12 birth months may not overlap. With 13 people, two must share a month."
       , txt env 20 (hg - 36) 14 (hsb 0 0 55)
           "Tokyo surely has two people with the same hair count (max 150 thousand hairs, 14 million people)."
       ]

drawBird :: Env -> Float -> Float -> Float -> Bool -> Picture
drawBird env x y hue lifted =
  at (toG env x y) $ Pictures
    [ if lifted then Color (hsba 0 0 100 0.25) (disc 22) else Blank
    , Color (hsb hue 60 95) (Pictures [Scale 15 11 (disc 1), Translate 12 8 (disc 7)])
    , Color (hsb 40 90 100) (Polygon [(18, 8), (26, 6), (18, 4)])
    , Color (hsb 0 0 10) (Translate 14 10 (disc 1.5))
    ]

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ | wN w < 9 -> randomize env w { wN = wN w + 1 } >>= newNumbers
  EventKey (SpecialKey KeyDown) Down _ _ | wN w > 2 -> randomize env w { wN = wN w - 1 } >>= newNumbers
  EventKey (SpecialKey KeySpace) Down _ _ -> randomize env w
  EventKey (Char c) Down _ _
    | c `elem` "mM" -> pure w { wNumbers = not (wNumbers w) }
    | c `elem` "nN" -> newNumbers w
  EventKey (MouseButton LeftButton) Down _ _ -> pure (press env w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure (release env w)
  _ -> pure w

-- 上に描いた鳩から順に調べてつかむ
press :: Env -> World -> World
press env w
  | wNumbers w = w
  | otherwise =
      let (mx, my) = mouseP env
          hits = [i | (i, p) <- zip [0 ..] (wPigeons w), (pX p - mx) ^ (2 :: Int) + (pY p - my) ^ (2 :: Int) < 18 * 18]
      in case reverse hits of
           (i : _) -> w { wDragging = i }
           [] -> w

-- 離した場所の巣へ入れる
release :: Env -> World -> World
release env w
  | wDragging w < 0 = w
  | otherwise =
      let (mx, my) = mouseP env
          inside h =
            let (x, y, rw, rh) = holeRect env (wN w) h
            in mx >= x - 10 && mx <= x + rw + 10 && my >= y - 40 && my <= y + rh + 40
          target p = foldl' (\cur h -> if inside h then h else cur) (pHole p) [0 .. wN w - 1]
      in w { wPigeons = [if i == wDragging w then p { pHole = target p } else p | (i, p) <- zip [0 ..] (wPigeons w)]
           , wDragging = -1 }

-- ---------- やってみよう ----------
-- 1. 鳩をドラッグして「どの巣も 1 羽以下」にしようとしてみる。できない理由を言葉で説明しよう
-- 2. 鳩を 2n+1 羽にすると「3 羽以上の巣」が必ずできる（一般化）。randomize の羽数を変えてみよう
-- 3. 整数モード：数を 1〜60 から 1〜1000 に広げても、ペアは必ず見つかる？
-- 4. 「5 個の格子点のうち 2 点の中点が格子点」も鳩の巣（座標の偶奇は 4 種類）。描いて確かめよう

-- ---------- 道具 ----------
-- 文字の幅のおおよその見積もり
textW :: Float -> String -> Float
textW size s = fromIntegral (length s) * size * 0.85

-- p5.js と同じ座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js の座標で
mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左上が (x, y) の長方形
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 太さ t の枠
frameBox :: Env -> Float -> Float -> Float -> Float -> Float -> Picture
frameBox env t x y w h =
  let a = toG env x y
      b = toG env (x + w) y
      c = toG env (x + w) (y + h)
      d = toG env x (y + h)
  in thickPath t [a, b, c, d, a]

-- 左下が (x, y) の文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 横は中央ぞろえ、y は文字の下
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = txt env (x - textW size s / 2) y size c s
