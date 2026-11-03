-- ============================================
-- 09 頂点被覆の 2 倍近似（線を選んで両端を取る）
-- 最小の頂点被覆は NP 困難。でも「残っている線を 1 本選んで両端を両方取る」をくり返すだけで、最適の 2 倍以内が保証される
-- 理由：選んだ線どうしは端を共有しない。最適解もそれぞれの線の端を少なくとも 1 つ取るので、最適 ≥ 選んだ線の本数 = 近似 ÷ 2
-- スペース：1 手ずつ進める（もう一度押すと自動）/ O：最適解を表示 / R：ランダムなグラフ / 1〜3：お手本
-- 何もない所をクリック：点を追加 / 点から点へドラッグ：線を追加
-- ============================================
module Main (main) where

import Art
import Control.Monad (filterM, replicateM)
import Data.Bits (popCount, shiftL, testBit)
import Data.List (find)
import qualified Data.Set as Set
import Numeric (showFFloat)

maxN :: Int
maxN = 16           -- ★ 最適解の総当たりは 2^点の数

type Edge = (Int, Int)

data World = World
  { wNodes   :: [Point]
  , wEdges   :: [Edge]
  , wCover   :: [Int]
  , wPicked  :: [Edge]
  , wRemain  :: [Edge]
  , wAuto    :: !Bool
  , wTimer   :: !Int
  , wOpt     :: Maybe (Int, Int)   -- 最適解（点の集合のビット列、個数）
  , wShowOpt :: !Bool
  , wDrag    :: !Int
  }

main :: IO ()
main = runSketch (sketch "18-09 Vertex Cover 2-Approximation" emptyWorld)
  { skBack  = hsb 210 30 9
  , skStart = \env -> preset env 1 emptyWorld
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

emptyWorld :: World
emptyWorld = World [] [] [] [] [] False 0 Nothing False (-1)

preset :: Env -> Int -> World -> IO World
preset env k w =
  let cx = envW env / 2
      cy = envH env * 0.56
      r = min (envW env) (envH env) * 0.33
  in case k of
       1 -> -- 星：最適 1、近似 2
         pure $ resetAlg w
           { wNodes = (cx, cy) : [(cx + cos a * r, cy + sin a * r) | i <- [0 .. 7 :: Int], let a = tau * fromIntegral i / 8]
           , wEdges = [(0, i + 1) | i <- [0 .. 7]] }
       2 -> -- 離れた線 3 本：近似 6 点、最適 3 点でちょうど 2 倍
         pure $ resetAlg w
           { wNodes = [ (cx - r + fromIntegral (i `mod` 3) * r, cy + (if i < 3 then -r * 0.5 else r * 0.5)) | i <- [0 .. 5 :: Int] ]
           , wEdges = [(i, i + 3) | i <- [0 .. 2]] }
       _ -> randomGraph env 11 w

randomGraph :: Env -> Int -> World -> IO World
randomGraph env n w = do
  ps <- replicateM n $ do
    x <- randomRIO (120, envW env - 120)
    y <- randomRIO (170, envH env - 90)
    pure (x, y)
  let ip = zip [0 ..] ps
      limit = min (envW env) (envH env) * 0.32
  es <- filterM (const (chance 0.5)) [(i, j) | (i, a) <- ip, (j, b) <- ip, i < j, dist2 a b < limit]
  pure (resetAlg w { wNodes = ps, wEdges = es })

resetAlg :: World -> World
resetAlg w = w { wCover = [], wPicked = [], wRemain = wEdges w, wAuto = False, wOpt = Nothing, wShowOpt = False }

-- 1 手：残っている線を 1 本選び、両端を取る
algStep :: World -> IO (World, Bool)
algStep w = case wRemain w of
  [] -> pure (w, False)
  rs -> do
    i <- randomRIO (0, length rs - 1)              -- ★ 線の選び方を変えてみよう（例：0 番）
    let e@(a, b) = rs !! i
        cover = wCover w ++ [a, b]
        rest = [(p, q) | (p, q) <- rs, p `notElem` cover, q `notElem` cover]  -- 端が被覆に入った線を消す
        w1 = w { wPicked = wPicked w ++ [e], wCover = cover, wRemain = rest }
    pure (if null rest then computeOpt w1 else w1, True)

-- 総当たりで最小の頂点被覆を探す
computeOpt :: World -> World
computeOpt w =
  let n = length (wNodes w)
      es = wEdges w
      covers m = all (\(a, b) -> testBit m a || testBit m b) es
      better best m =
        let c = popCount m
        in case best of
             Just (_, bc) | c >= bc -> best
             _ -> if covers m then Just (m, c) else best
  in if n > maxN
       then w { wOpt = Nothing }
       else w { wOpt = foldl' better Nothing [0 .. (1 `shiftL` n) - 1 :: Int] }

stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wAuto w =
      let t = wTimer w + 1
          w1 = w { wTimer = t }
      in if t `mod` 30 == 0
           then do
             (w2, moved) <- algStep w1
             pure (if moved then w2 else w2 { wAuto = False })
           else pure w1
  | otherwise = pure w

posOf :: World -> Int -> Point
posOf w i = case drop i (wNodes w) of
  (p : _) -> p
  [] -> (0, 0)

drawW :: Env -> World -> Picture
drawW env w =
  let pickedSet = Set.fromList (wPicked w)
      aliveSet = Set.fromList (wRemain w)
      edge e@(a, b) =
        let isPick = Set.member e pickedSet
            alive = Set.member e aliveSet
            c | isPick = hsb 45 90 100
              | alive = hsb 0 0 80
              | otherwise = hsba 150 50 60 0.6
        in seg env c (if isPick then 7 else if alive then 2.5 else 2) (posOf w a) (posOf w b)
      dragLine
        | wDrag w >= 0 = seg env (hsb 0 0 100) 2 (posOf w (wDrag w)) (mouseP5 env (envMouse env))
        | otherwise = Blank
      node (i, p@(x, y)) =
        let inC = i `elem` wCover w
            inO = wShowOpt w && maybe False (\(m, _) -> testBit m i) (wOpt w)
        in Pictures
             [ if inO then dot env (hsb 280 70 100) p 52 else Blank
             , dot env (if inC then hsb 45 90 100 else hsb 210 20 55) p 34
             , txtC env x y 14 (hsb 0 0 10) (show i)
             ]
      optLine
        | null (wRemain w) && not (null (wEdges w)) = case wOpt w of
            Just (_, c) ->
              txt env 20 102 18 (hsb 280 60 100)
                ("Optimum: " ++ show c ++ " points   approx / opt = "
                  ++ fix 2 (fromIntegral (length (wCover w)) / fromIntegral (max 1 c) :: Float)
                  ++ " (always <= 2)" ++ (if wShowOpt w then "" else "   O: show"))
            Nothing ->
              txt env 20 102 18 (hsb 0 0 70) ("More than " ++ show maxN ++ " points: optimum not computed (2^n cases)")
        | otherwise = Blank
  in Pictures
       ( map edge (wEdges w)
      ++ [dragLine]
      ++ map node (zip [0 ..] (wNodes w))
      ++ [ txt env 20 34 15 (hsb 0 0 100) "Space: step (then auto)   O: optimum   R: random   1 star  2 pairs  3 random   Click: point   Drag: edge"
         , txt env 20 70 18 (hsb 45 85 100)
             ("Approx: picked edges " ++ show (length (wPicked w)) ++ " -> cover size " ++ show (length (wCover w))
               ++ " (edges left: " ++ show (length (wRemain w)) ++ ")")
         , optLine
         , txt env 20 (envH env - 22) 15 (hsb 0 0 60) "Yellow edges share no ends. OPT needs a point on each: OPT >= yellow, approx = 2 x yellow"
         ] )

hit :: World -> Point -> Int
hit w m = maybe (-1) fst (find (\(_, p) -> dist2 m p < 20) (zip [0 ..] (wNodes w)))

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let m@(_, my) = mouseP5 env p
        h = hit w m
    in pure $ if my < 110
         then w
         else if h < 0 && length (wNodes w) < 24
           then resetAlg w { wDrag = h, wNodes = wNodes w ++ [m] }
           else w { wDrag = h }
  EventKey (MouseButton LeftButton) Up _ p
    | wDrag w >= 0 ->
        let from = wDrag w
            to = hit w (mouseP5 env p)
            w1 = w { wDrag = -1 }
            e = (min from to, max from to)
            exists = any (\(a, b) -> (a, b) == e || (b, a) == e) (wEdges w)
        in pure $ if to >= 0 && to /= from && not exists
             then resetAlg w1 { wEdges = wEdges w ++ [e] }
             else w1
  EventKey (SpecialKey KeySpace) Down _ _
    | null (wRemain w) -> pure (resetAlg w)
    | not (null (wPicked w)) -> pure w { wAuto = not (wAuto w) }
    | otherwise -> fst <$> algStep w
  EventKey (Char c) Down _ _
    | c `elem` "oO" ->
        let w1 = case wOpt w of
              Nothing | null (wRemain w) -> computeOpt w
              _ -> w
        in pure w1 { wShowOpt = not (wShowOpt w1) }
    | c `elem` "rR" -> randomGraph env 11 w
    | c `elem` "123" -> preset env (fromEnum c - fromEnum '0') w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 星（1）で近似は 2、最適は 1。比がちょうど 2 になる
-- 2. ランダムなグラフで何度も試し、比の平均を記録しよう（2 よりかなり小さいことが多い）
-- 3. 「次数がいちばん大きい点を取る」貪欲法に変えると？（実は比が log n まで悪くなる例がある）

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- gloss のマウス座標を p5.js の座標に直す
mouseP5 :: Env -> Point -> Point
mouseP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 直径 d の円（p5.js の circle）
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

-- 文字：左端の下が (x, y)
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str

-- 文字：中心が (x, y)
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = labelC (gx env x) (gy env y) s c str

-- ---------- 道具：その他 ----------
-- 確率 p で True
chance :: Float -> IO Bool
chance p = (< p) <$> randomRIO (0, 1)

-- 小数点以下 d 桁で表示（p5.js の nf）
fix :: RealFloat a => Int -> a -> String
fix d v = showFFloat (Just d) v ""
