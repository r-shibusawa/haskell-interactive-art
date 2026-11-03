-- ============================================
-- 07 凸包：グレアム走査
-- 点の集まりをゴムひもで囲んだ形 = 凸包
-- ① いちばん下の点 p0 から見た角度の順に並べる
-- ② 順に積み（push）、「右に曲がる」ならスタックから降ろす（pop）
-- 各点は 1 回積まれ、高々 1 回降ろされるだけ → 並べ替えの O(n log n) が全体の手間
-- 操作：クリック = 点を追加 / ドラッグ = 動かす / スペース = 再生・停止 / → = 1 ステップ
--       R = ランダム / C = 全部消す / 1〜4 = お手本（ランダム・円・格子・かたまり）
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM)
import Data.List (elemIndex, sortBy)
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

nPoints :: Int
nPoints = 26            -- ★ ランダムに置く点の数

speed :: Int
speed = 14              -- ★ 何フレームに 1 ステップ

type Pts = U.Vector (Float, Float)

data S = S
  { sStack   :: ![Int]   -- スタック（先頭が一番上）
  , sSorted  :: ![Int]   -- 角度の順に並べた点
  , sP0      :: !Int
  , sDropped :: ![Int]   -- 同じ向きで捨てた点
  , sPushes  :: !Int
  , sPops    :: !Int
  , sCand    :: !Int     -- いま調べている点（-1 = なし）
  , sRight   :: !Bool    -- その点へ右に曲がるか
  , sMsg     :: String
  }

data World = World
  { wPts  :: !Pts
  , wAuto :: !Bool
  , wS    :: !S
  , wGen  :: [S]
  , wDrag :: !Int
  }

main :: IO ()
main = runSketch (sketch "17-07 Graham scan" initialWorld)
  { skBack  = cBg
  , skStart = \env -> preset env 1 initialWorld
  , skStep  = \env _ w -> pure (if wAuto w && envFrame env `mod` speed == 0 then advance w else w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

emptyS :: String -> S
emptyS = S [] [] (-1) [] 0 0 (-1) False

initialWorld :: World
initialWorld = World U.empty True (emptyS "") [] (-1)

preset :: Env -> Int -> World -> IO World
preset env k w = do
  let bigW = envW env - 300
      cx = fromIntegral (round (bigW / 2) :: Int)
      cy = fromIntegral (round (envH env / 2 + 30) :: Int)
      r = min bigW (envH env) * 0.36
      n = nPoints
      -- 格子の間隔は整数にする（小数の誤差で「一直線」が崩れないように）
      stepG = fromIntegral (2 * round (r * 0.35 / 2) :: Int)
      uni k' = randomRIO (-r, r) >>= \v -> pure (v * k')
  ps <- forM [0 .. n - 1] $ \i -> do
    let fi = fromIntegral i
    case k of
      2 -> let a = tau * fi / fromIntegral n in pure (cx + cos a * r, cy + sin a * r)   -- 全部が凸包に乗る
      3 -> pure ( cx + (fromIntegral (i `mod` 6) - 2.5) * stepG                        -- 一直線に並ぶ点
                , cy + (fromIntegral (i `div` 6) - 2) * stepG )
      4 | i < n - 5 -> do
            a <- gaussian
            b <- gaussian
            pure (cx + a * r * 0.25, cy + b * r * 0.25)
        | otherwise -> (\a b -> (cx + a, cy + b)) <$> uni 1.3 <*> uni 1
      _ -> (\a b -> (cx + a, cy + b)) <$> uni 1.2 <*> uni 1
  pure (restart w { wPts = U.fromList ps })

-- 画面は y が下向きなので、y を反転して「数学の向き」で計算する（+ なら左回り）
turn :: Point -> Point -> Point -> Float
turn (ox, oy) (ax, ay) (bx, by) = (ax - ox) * (oy - by) - (oy - ay) * (bx - ox)

angleFrom :: Point -> Point -> Float
angleFrom (ox, oy) (px, py) = atan2 (oy - py) (px - ox)

restart :: World -> World
restart w =
  let ok = U.length (wPts w) >= 3
      s0 = emptyS (if ok then "" else "Place at least 3 points (click)")
  in w { wS = s0, wGen = if ok then steps (graham (wPts w)) s0 else [] }

setMsg :: String -> Gen S ()
setMsg m = modify (\s -> s { sMsg = m })

-- 表示用の名前：p0 と、角度の順番 p1, p2, …
lab :: Int -> [Int] -> Int -> String
lab p0 sorted i
  | i == p0 = "p0"
  | otherwise = 'p' : show (maybe 0 (+ 1) (elemIndex i sorted))

-- ★ グレアム走査
graham :: Pts -> Gen S ()
graham pts = do
  let n = U.length pts
      at' i = pts U.! i
      lower a b = let (ax, ay) = at' a
                      (bx, by) = at' b
                  in ay > by || (ay == by && ax < bx)
      -- ① いちばん下（同じなら左）の点
      p0 = foldl (\b i -> if lower i b then i else b) 0 [1 .. n - 1]
      o = at' p0
      rest0 = filter (/= p0) [0 .. n - 1]
      -- ② 角度の順に並べる（同じ向きなら近い順）
      cmp a b
        | abs (turn o (at' a) (at' b)) < 1e-6 = comparing (dist2 o . at') a b
        | otherwise = comparing (angleFrom o . at') a b
      rest = sortBy cmp rest0
      -- 同じ向きなら遠い点だけ残す
      keep = [ (i, sameDir i nxt) | (i, nxt) <- zip rest (map Just (drop 1 rest) ++ [Nothing]) ]
      sameDir i (Just j) = abs (turn o (at' i) (at' j)) < 1e-6
      sameDir _ Nothing = False
      sorted = [i | (i, False) <- keep]
      dropped = [i | (i, True) <- keep]
      name = lab p0 sorted
      -- ③ スタックで走査
      scan i = do
        st <- gets sStack
        case st of
          (b : a : _) | turn (at' a) (at' b) (at' i) <= 0 -> do
            modify (\s -> s { sCand = i, sRight = True })
            setMsg (name i ++ " makes a right turn (a dent) -> pop the top, " ++ name b)
            yield
            modify (\s -> s { sStack = drop 1 (sStack s), sPops = sPops s + 1 })
            scan i
          _ -> do
            modify (\s -> s { sCand = i, sRight = False })
            setMsg (name i ++ " makes a left turn -> push")
            yield
            modify (\s -> s { sStack = i : sStack s, sPushes = sPushes s + 1 })
  modify (\s -> s { sP0 = p0 })
  setMsg "Step 1: find the lowest point p0"
  yield
  modify (\s -> s { sSorted = sorted, sDropped = dropped })
  setMsg "Step 2: sort by angle from p0 (same direction: keep the farthest)"
  yield
  case sorted of
    (a : b : more) -> do
      modify (\s -> s { sStack = [b, a, p0], sPushes = 3 })
      setMsg "Step 3: push p0 and the first 2 points"
      yield
      mapM_ scan more
      modify (\s -> s { sCand = -1 })
      modify (\s -> s { sMsg = "Done! " ++ show (length (sStack s)) ++ " hull vertices   push " ++ show (sPushes s)
                                 ++ " / pop " ++ show (sPops s) ++ " (each at most n)" })
      yield
    _ -> do
      modify (\s -> s { sStack = reverse (p0 : sorted) })
      setMsg "All points are collinear: the hull is a segment"
      yield

advance :: World -> World
advance w = case wGen w of
  [] -> w
  (s : r) -> w { wS = s, wGen = r }

drawW :: Env -> World -> Picture
drawW env w =
  let ww = envW env
      hh = envH env
      pts = wPts w
      s = wS w
      p0 = sP0 s
      stack = sStack s
      p i = pts U.! i
      genOn = not (null (wGen w))
      -- 角度の順を示す放射線
      rays
        | p0 >= 0 = Pictures
            [ Pictures [ Color (withA 0.09 white) (Line [pt env (p p0), pt env (x, y)])
                       , txt env (x + 7) (y - 18) 14 cDim ('p' : show (k + 1)) ]
            | (k, i) <- zip [0 :: Int ..] (sSorted s), let (x, y) = p i ]
        | otherwise = Blank
      -- スタック上の折れ線
      chain = map p (reverse stack)
      stackLine = case chain of
        [] -> Blank
        (c0 : _) -> path env cOrchid 3 (if genOn then chain else chain ++ [c0])
      -- 候補点への曲がり方
      candPart = case stack of
        (b : a : _) | sCand s >= 0 ->
          let c = p (sCand s)
              col = if sRight s then cBad else cMint
          in Pictures [fillPoly env (withA 0.3 col) [p a, p b, c], seg env col 3 (p b) c]
        _ -> Blank
      p0Label
        | p0 >= 0 = let (x, y) = p p0 in txt env (x + 8) (y + 4) 14 cGold "p0"
        | otherwise = Blank
      pointPic i =
        let onHull = i `elem` stack
            c | i == p0 = cGold
              | i `elem` sDropped s = rgb255 0x4a 0x40 0x60
              | onHull = cOrchid
              | otherwise = cFg
        in dot env c (p i) (if i == p0 || onHull then 13 else 9)
      -- スタックの表示
      px = ww - 270
      sh = min 30 ((hh - 200) / fromIntegral (max (length stack) 1))
      name = lab p0 (sSorted s)
      stackRow (r, i) =
        let y = 72 + fromIntegral (r :: Int) * sh
        in Pictures
             [ box env (if r == 0 then cOrchid else rgb255 0x4b 0x3a 0x6b) (px + 18) y 210 (sh - 4)
             , txtM env (px + 30) (y + sh / 2 - 2) (clampF 14 15 (sh * 0.6)) (if r == 0 then cBg else cFg) (name i) ]
  in Pictures
       [ rays
       , stackLine
       , candPart
       , p0Label
       , Pictures (map pointPic [0 .. U.length pts - 1])
       , box env cPanel px 20 250 (hh - 40)
       , txt env (px + 18) 36 17 cFg "Stack (top first)"
       , Pictures (map stackRow (zip [0 ..] stack))
       , txt env (px + 18) (hh - 90) 15 cFg ("n = " ++ show (U.length pts))
       , txt env (px + 18) (hh - 68) 15 cFg ("push " ++ show (sPushes s) ++ "  pop " ++ show (sPops s))
       , txt env 30 24 22 cFg "Convex hull: Graham scan"
       , txt env 30 56 14 cDim "Click add  Drag move  Space play/stop  Right 1 step  R random  C clear  1-4 examples"
       , txt env 30 84 17 cGold (sMsg s)
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | mx > envW env - 270 -> pure w
    | otherwise ->
        let near = [i | i <- [0 .. U.length (wPts w) - 1], dist2 m (wPts w U.! i) < 12]
        in pure w { wDrag = foldl (\_ i -> i) (-1) near }
  EventKey (MouseButton LeftButton) Up _ _
    | mx > envW env - 270 -> pure w { wDrag = -1 }
    | wDrag w < 0 && my > 110 -> pure (restart w { wPts = U.snoc (wPts w) m })
    | otherwise -> pure w { wDrag = -1 }
  EventMotion _
    | wDrag w >= 0 -> pure (restart w { wPts = wPts w U.// [(wDrag w, m)] })
  EventResize _ -> preset env 1 w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wAuto = not (wAuto w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (advance w)
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> preset env 1 w
    | c `elem` "cC" -> pure (restart w { wPts = U.empty, wDrag = -1 })
    | c `elem` "1234" -> preset env (fromEnum c - fromEnum '0') w
  _ -> pure w
  where m@(mx, my) = mouse env

-- ---------- やってみよう ----------
-- 1. お手本 2（円）で pop が 0 回になることを確かめる
-- 2. お手本 3（格子）で、同じ向きの点がどう捨てられるか見る
-- 3. scan の <= 0 を < 0 にすると、一直線の点はどうなる？

-- ---------- 道具：乱数 ----------
-- 正規分布の乱数（p5.js の randomGaussian）。ボックス＝ミュラー法
gaussian :: IO Float
gaussian = do
  u <- randomRIO (1e-6, 1)
  v <- randomRIO (0, 1)
  pure (sqrt (-2 * log u) * cos (tau * v))

-- ---------- 道具：ジェネレーター（function*）の代わり ----------
-- yield のたびに状態を 1 つ流す計算。結果は「状態の遅延リスト」になり、
-- World がその残りを持っていて、1 手進めるたびに先頭を 1 つ取る。
newtype Gen s a = Gen { unGen :: s -> (a -> s -> [s]) -> [s] }

instance Functor (Gen s) where
  fmap f (Gen g) = Gen (\s k -> g s (k . f))

instance Applicative (Gen s) where
  pure x = Gen (\s k -> k x s)
  Gen f <*> Gen g = Gen (\s k -> f s (\h s' -> g s' (k . h)))

instance Monad (Gen s) where
  Gen g >>= f = Gen (\s k -> g s (\x s' -> unGen (f x) s' k))

yield :: Gen s ()
yield = Gen (\s k -> s : k () s)

gets :: (s -> a) -> Gen s a
gets f = Gen (\s k -> k (f s) s)

modify :: (s -> s) -> Gen s ()
modify f = Gen (\s k -> k () $! f s)

steps :: Gen s () -> s -> [s]
steps (Gen g) s0 = g s0 (\_ _ -> [])

-- ---------- 道具：色 ----------
cBg, cPanel, cFg, cDim, cOrchid, cMint, cGold, cBad, cEdge, cGreen, cRed, cAmber, cBlue :: Color
cBg     = rgb255 0x14 0x10 0x1f
cPanel  = rgb255 0x22 0x1a 0x33
cFg     = rgb255 0xe9 0xe4 0xf5
cDim    = rgb255 0x8c 0x84 0xa3
cOrchid = rgb255 0xf0 0xa3 0xff
cMint   = rgb255 0x6f 0xe3 0xd0
cGold   = rgb255 0xff 0xcf 0x6b
cBad    = rgb255 0xff 0x6b 0x81
cEdge   = rgb255 0x3a 0x2f 0x52
cGreen  = rgb255 0x2f 0x5e 0x57
cRed    = rgb255 0x5e 0x2a 0x3a
cAmber  = rgb255 0x5a 0x4a 0x1f
cBlue   = rgb255 0x2f 0x4a 0x5e

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置を p5.js の座標で
mouse :: Env -> Point
mouse env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の枠
outline :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
outline env c t x y w h = Color c (thickPath t (map (pt env) [(x - t / 2, y), (x + w + t / 2, y), (x + w, y), (x + w, y + h), (x + w + t / 2, y + h), (x - t / 2, y + h), (x, y + h), (x, y)]))

-- 太さ t の線分（p5.js の line）と折れ線
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

path :: Env -> Color -> Float -> [Point] -> Picture
path env c t ps = Color c (thickPath t (map (pt env) ps))

-- 点線
dashed :: Env -> Color -> Float -> Float -> Point -> Point -> Picture
dashed env c t d a@(x1, y1) b@(x2, y2) =
  let l = dist2 a b
      k = floor (l / (2 * d)) :: Int
      f s = (x1 + (x2 - x1) * s / max l 1e-6, y1 + (y2 - y1) * s / max l 1e-6)
  in Pictures [seg env c t (f (2 * d * fromIntegral i)) (f (min l (2 * d * fromIntegral i + d))) | i <- [0 .. k]]

-- 直径 d の円（p5.js の circle）と、太さ t の輪
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

hoop :: Env -> Color -> Float -> Point -> Float -> Picture
hoop env c t p d = Color c (at (pt env p) (ring (d / 2) t))

-- 塗りつぶしの多角形（凸形のみ）
fillPoly :: Env -> Color -> [Point] -> Picture
fillPoly env c ps = Color c (Polygon (map (pt env) ps))

-- 文字：txt は左上が (x, y)、txtM は左端で上下中央、txtC は中心が (x, y)
txt, txtM, txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env (y + s)) s c str
txtM env x y s c str = label (gx env x) (gy env (y + s / 2)) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str

-- 小数点以下 d 桁（JavaScript の toFixed）
fix :: Int -> Float -> String
fix d v = showFFloat (Just d) v ""

