-- ============================================
-- 09 並列の行列かけ算（ブロックに分ける再帰版）
-- C = A × B を 4 つのブロックに分けると、8 つの小さなかけ算になる
-- 1：ふつうの 3 重ループ（1 人で n³ ステップ）
-- 2：4 つずつ 2 回に分けて並列（同じ C のブロックに同時に書かない）→ スパン Θ(n)
-- 3：8 つ全部を同時に（一時行列 T を使う）→ 最後に足し合わせ。スパン Θ(lg² n)、ただしメモリが増える
-- スペース = 再生・停止 / 1〜3 = 方式 / 上下 = n / マウスを C のマスに乗せる = そのマスの計算の順番 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Array (Array, accumArray, bounds, elems, (!))
import qualified Data.IntSet as IS
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

data EvKind = Plain | Mul | Add deriving Eq

-- 1 つの時間ステップに実行される計算 (i, j, k)
data Ev = Ev { eI :: !Int, eJ :: !Int, eK :: !Int, eKind :: !EvKind }

data World = World
  { wN :: !Int
  , wMode :: !Int
  , wA :: !(U.Vector Int)
  , wB :: !(U.Vector Int)
  , wC :: !(U.Vector Int)
  , wTerms :: !(U.Vector Int)
  , wEvents :: Array Int [Ev]
  , wT :: !Int
  , wPlaying :: !Bool
  , wTimer :: !Int
  }

main :: IO ()
main = runSketch (sketch "15-09 Parallel Matrix Multiply" (makeSchedule (World 8 2 z z z z noEvents 0 False 0)))
  { skBack  = hsb 30 25 9
  , skStart = \_ -> rebuild (makeSchedule (World 8 2 z z z z noEvents 0 False 0))  -- ★ 最初の n と方式
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }
  where z = U.replicate 64 0

noEvents :: Array Int [Ev]
noEvents = accumArray (flip (:)) [] (0, 0) []

numSteps :: World -> Int
numSteps w = let (a, b) = bounds (wEvents w) in b - a + 1

rebuild :: World -> IO World
rebuild w = do
  let n = wN w
      rnd = U.fromList <$> mapM (const (randomRIO (0, 3))) [1 .. n * n]
  a <- rnd
  b <- rnd
  pure (makeSchedule w { wA = a, wB = b })

makeSchedule :: World -> World
makeSchedule w =
  let n = wN w
      triple = [(i, j, k) | i <- [0 .. n - 1], j <- [0 .. n - 1], k <- [0 .. n - 1]]
      evs = case wMode w of
        1 -> [(t, Ev i j k Plain) | (t, (i, j, k)) <- zip [0 ..] triple]
        2 -> snd (recur 0 0 0 n 0 [])
        _ ->
          -- 8 つ同時：かけ算は全部 1 ステップ目、そのあと 2 つずつ足していく（lg n 段）
          let levels = round (logBase 2 (fromIntegral n :: Float)) :: Int
          in [(0, Ev i j k Mul) | (i, j, k) <- triple]
             ++ [(l, Ev i j (-1) Add) | l <- [1 .. levels], i <- [0 .. n - 1], j <- [0 .. n - 1]]
      lastT = foldl' (\m (t, _) -> max m t) 0 evs
      arr = accumArray (flip (:)) [] (0, lastT) evs
  in resetRun w { wEvents = arr }

-- 再帰：前半 4 つ（C11 += A11·B11 など）を並列 → 後半 4 つ（C11 += A12·B21 など）を並列
recur :: Int -> Int -> Int -> Int -> Int -> [(Int, Ev)] -> (Int, [(Int, Ev)])
recur r0 c0 k0 size time acc
  | size == 1 = (time + 1, (time, Ev r0 c0 k0 Plain) : acc)
  | otherwise =
      let h = size `div` 2
          quads = [(0, 0), (0, h), (h, 0), (h, h)]
          half k t0 ac = foldl'
            (\(e, a) (di, dj) -> let (e', a') = recur (r0 + di) (c0 + dj) k h t0 a in (max e e', a'))
            (t0, ac) quads
          (end1, acc1) = half k0 time acc
      in half (k0 + h) end1 acc1

resetRun :: World -> World
resetRun w =
  let n = wN w
      z = U.replicate (n * n) 0
  in w { wC = z, wTerms = z, wT = 0, wPlaying = False }

stepTime :: World -> World
stepTime w
  | wT w >= numSteps w = w { wPlaying = False }
  | otherwise =
      let n = wN w
          evs = wEvents w ! wT w
          ix e = eI e * n + eJ e
          a = wA w
          b = wB w
          dotProduct i j = sum [a U.! (i * n + k) * b U.! (k * n + j) | k <- [0 .. n - 1]]
      in if wMode w == 3
           then
             -- 3 は「段ごとに部分和が倍になる」と考える
             let terms' = U.accum (\t isMul -> if isMul then 1 else min n (t * 2)) (wTerms w) [(ix e, eKind e == Mul) | e <- evs]
                 ready = [ (ix e, dotProduct (eI e) (eJ e)) | e <- evs, eKind e == Add, terms' U.! ix e == n ]
             in w { wTerms = terms', wC = wC w U.// ready, wT = wT w + 1 }
           else
             w { wC = U.accum (+) (wC w) [(ix e, a U.! (eI e * n + eK e) * b U.! (eK e * n + eJ e)) | e <- evs]
               , wTerms = U.accum (+) (wTerms w) [(ix e, 1) | e <- evs]
               , wT = wT w + 1 }

stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wPlaying w =
      let n = wN w
          mode1 = wMode w == 1
          per = if mode1 then max 1 (n * n `div` 16) else 1
          t = wTimer w + 1
          w' = w { wTimer = t }
      in pure (if t `mod` (if mode1 then 2 else 20) == 0 then iterate stepTime w' !! per else w')
  | otherwise = pure w

cell :: Env -> Int -> Float
cell env n = let d = fromIntegral (2 * n) in minimum [46, (envH env - 260) / d, (envW env * 0.5) / d]

-- マスの中の数字の大きさ（文字は 14 以上）
cellText :: Float -> Float
cellText s = max 14 (s * 0.36)

drawW :: Env -> World -> Picture
drawW env w =
  let n = wN w
      s = cell env n
      nf' = fromIntegral n :: Float
      gx' = 70 + nf' * s + 20
      gy' = 120 + nf' * s + 20
      t = wT w
      nSteps = numSteps w
      cur = if t > 0 && t - 1 < nSteps then wEvents w ! (t - 1) else []
      curLen = length cur
      showHi = curLen <= n * n
      hiA = IS.fromList [eI e * n + eK e | showHi, e <- cur, eKind e /= Add]
      hiB = IS.fromList [eK e * n + eJ e | showHi, e <- cur, eKind e /= Add]
      -- C：色 = 何項まで足し終わったか
      cellC i j =
        let f = fromIntegral (wTerms w U.! (i * n + j)) / nf'
            x = gx' + fromIntegral j * s
            y = gy' + fromIntegral i * s
        in Pictures
             [ box env (hsb (30 + f * 20) (20 + 70 * f) (18 + 77 * f)) x y s s
             , wire env (hsb 30 20 25) x y s s
             -- n = 16 では 3 けたの数がマスに入らないので、マウスを乗せたときに下に出す
             , if f > 0 && s >= 24
                 then txtC env (x + s / 2) (y + s / 2) (cellText s) (lum (if f >= 1 then 10 else 90)) (show (wC w U.! (i * n + j)))
                 else Blank ]
      (mx, my) = mouseP env
      mj = floor ((mx - gx') / s) :: Int
      mi = floor ((my - gy') / s) :: Int
      overC = mi >= 0 && mi < n && mj >= 0 && mj < n
      info
        | overC && wMode w /= 3 =
            [ "C[" ++ show mi ++ "][" ++ show mj ++ "] = " ++ show (wC w U.! (mi * n + mj)) ++ "   order (k@time): "
                ++ unwords [ show (eK e) ++ "@" ++ show (time + 1)
                           | (time, evs) <- zip [0 :: Int ..] (elems (wEvents w)), e <- evs, eI e == mi, eJ e == mj ] ]
        | otherwise = []
      px = gx' + nf' * s + 40
      py = 140
      title = case wMode w of
        1 -> "1: triple loop (one person)"
        2 -> "2: 4 + 4 in parallel"
        _ -> "3: all 8 at once + add up"
      rows = ["n", "solo", "mode 2", "mode 3"]
             : [ [show m, big (m * m * m), show m, show ((lg + 1) * lg `div` 2 + lg + 1)]
               | m <- [8, 64, 1024 :: Int]
               , let lg = round (logBase 2 (fromIntegral m :: Float)) :: Int ]
      big v | v > 1000000 = printf "%.1e" (fromIntegral v :: Float)
            | otherwise = show v
      rowPics = [ txt env (px + fromIntegral c * 90) (py + 150 + fromIntegral q * 24) 15 (lum 70) v
                | (q, r) <- zip [0 :: Int ..] rows, (c, v) <- zip [0 :: Int ..] r ]
  in Pictures
       [ drawMat env n (wA w) 70 gy' s "A" (`IS.member` hiA) 200
       , drawMat env n (wB w) gx' 120 s "B" (`IS.member` hiB) 140
       , Pictures [cellC i j | i <- [0 .. n - 1], j <- [0 .. n - 1]]
       , txt env gx' (gy' - 8) 16 (hsb 45 70 100) "C = A x B"
       , txt env px py 22 (lum 100) title
       , txt env px (py + 40) 18 (lum 85) ("Time " ++ show t ++ " / " ++ show nSteps ++ " (= span)")
       , txt env px (py + 70) 18 (lum 85) ("Work (multiplications) n^3 = " ++ show (n * n * n))
       , txt env px (py + 100) 18 (lum 85) ("Running at once in this step: " ++ show curLen)
       , Pictures rowPics
       , txt env px (py + 260) 15 (lum 70) "(Rough span. Mode 3 is Theta(lg^2 n),"
       , txt env px (py + 282) 15 (lum 70) "but temp matrix T doubles the memory)"
       , Pictures [txt env 20 (envH env - 34) 14 (hsb 45 80 100) l | l <- info]
       , txt env 20 36 16 (lum 100)
           ("Space: " ++ (if wPlaying w then "stop" else "play") ++ "  1-3: mode  Up/Down: n = " ++ show n
             ++ "  R: restart  Hover over C: order of that cell")
       , txt env 20 62 14 (lum 65) "Adding to one C cell at once causes the race of sketch 08. Mode 2 puts such pairs in the first and second half"
       ]

drawMat :: Env -> Int -> U.Vector Int -> Float -> Float -> Float -> String -> (Int -> Bool) -> Float -> Picture
drawMat env n m x0 y0 s name hi hue = Pictures
  ( txt env x0 (y0 - 8) 16 (hsb hue 50 95) name
  : [ Pictures
        [ box env (if on then hsb 50 90 100 else hsb hue 30 (25 + fromIntegral v * 12)) x y s s
        , wire env (hsb 30 20 25) x y s s
        , txtC env (x + s / 2) (y + s / 2) (cellText s) (lum (if on then 10 else 90)) (show v) ]
    | i <- [0 .. n - 1], j <- [0 .. n - 1]
    , let ix = i * n + j
    , let v = m U.! ix
    , let on = hi ix
    , let x = x0 + fromIntegral j * s
    , let y = y0 + fromIntegral i * s ] )

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ ->
    let w1 = if wT w >= numSteps w then resetRun w else w
    in pure w1 { wPlaying = not (wPlaying w1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild w { wN = min 16 (wN w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild w { wN = max 2 (wN w `div` 2) }
  EventKey (Char c) Down _ _
    | c == '1' -> pure (makeSchedule w { wMode = 1 })
    | c == '2' -> pure (makeSchedule w { wMode = 2 })
    | c == '3' -> pure (makeSchedule w { wMode = 3 })
    | c `elem` "rR" -> pure (resetRun w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 方式 1 と 2 で、終わるまでの時刻（スパン）を比べる。n を 2 倍にすると、それぞれ何倍？
-- 2. 方式 2 で C のマスにマウスを乗せる。k の順番がバラバラでも、足す順番が変わるだけで答えは同じ
-- 3. ワーク（かけ算の数）はどの方式でも n³。並列にしても仕事の総量は減らない

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 左上 (x, y)、幅 w、高さ h の枠線
wire :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
wire env c x y w h = Color c (rectLine (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 文字：txt は左下が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = label (gx env x - fromIntegral (length str) * s * 0.72) (gy env y) s c str

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0
