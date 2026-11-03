-- ============================================
-- 09 最近点対：分割統治法
-- いちばん近い 2 点を探す。全部の組を調べると n(n−1)/2 回
-- ① 縦線で左右半分に分け、それぞれで最近点対を探す（再帰）
-- ② 左右の答えの近い方を δ として、縦線から幅 δ の帯の中だけ「境目をまたぐ組」を調べる
--    帯の中では y の順に並べると、各点は次の 7 点までと比べれば十分 → O(n log n)
-- 操作：クリック = 点を追加 / スペース = 再生・停止 / → = 1 ステップ / ↑↓ = 速さ
--       R = ランダム / C = 全部消す / 1〜3 = お手本（ランダム・格子・かたまり）
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM)
import qualified Data.IntSet as IS
import Data.List (sortOn)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

nPoints :: Int
nPoints = 48            -- ★ ランダムに置く点の数

type Pts = U.Vector (Float, Float)

data Best = Best { bD :: !Float, bA :: !Int, bB :: !Int }

-- 再帰の枠
data Frame = Frame { fX0 :: !Float, fX1 :: !Float, fDepth :: !Int, fMid :: !(Maybe Float) }

data S = S
  { sFrames :: ![Frame]               -- 先頭がいちばん深い
  , sStrip  :: !(Maybe (Float, Float)) -- 帯（中心, δ）
  , sCmp    :: !(Maybe (Int, Int))
  , sBest   :: !(Maybe Best)
  , sComps  :: !Int
  , sMsg    :: String
  }

data World = World
  { wPts  :: !Pts
  , wPerF :: !Int        -- ★ 1 フレームに何ステップ
  , wAuto :: !Bool
  , wS    :: !S
  , wGen  :: [S]
  }

main :: IO ()
main = runSketch (sketch "17-09 Closest pair" initialWorld)
  { skBack  = cBg
  , skStart = \env -> preset env 1 initialWorld
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

emptyS :: String -> S
emptyS = S [] Nothing Nothing Nothing 0

initialWorld :: World
initialWorld = World U.empty 1 True (emptyS "") []

preset :: Env -> Int -> World -> IO World
preset env k w = do
  let bigW = envW env - 300
      hh = envH env
  ps <- forM [0 .. nPoints - 1] $ \i -> case k of
    2 -> do
      a <- randomRIO (-6, 6)
      b <- randomRIO (-6, 6)
      pure ( 80 + fromIntegral (i `mod` 8) * (bigW - 160) / 7 + a
           , 150 + fromIntegral (i `div` 8) * (hh - 220) / 5 + b )
    3 -> do
      a <- gaussian
      b <- gaussian
      pure (bigW * (0.25 + fromIntegral (i `mod` 3) * 0.25) + a * 50, hh * 0.55 + b * 80)
    _ -> (,) <$> randomRIO (60, bigW - 40) <*> randomRIO (130, hh - 50)
  pure (restart w { wPts = U.fromList ps })

restart :: World -> World
restart w =
  let pts = wPts w
      n = U.length pts
      ok = n >= 2
      idx = [0 .. n - 1]
      xs = sortOn (\i -> fst (pts U.! i)) idx
      ys = sortOn (\i -> snd (pts U.! i)) idx
      s0 = emptyS (if ok then "" else "Place at least 2 points")
      program = do
        r <- rec' pts xs ys 0
        modify (\s -> s { sBest = r, sCmp = Nothing, sStrip = Nothing
                        , sMsg = "Done! Closest distance " ++ maybe "-" (fix 1 . bD) r })
        yield
  in w { wS = s0, wGen = if ok then steps program s0 else [] }

better :: Maybe Best -> Maybe Best -> Maybe Best
better Nothing q = q
better p Nothing = p
better (Just p) (Just q) = Just (if bD p <= bD q then p else q)

setMsg :: String -> Gen S ()
setMsg m = modify (\s -> s { sMsg = m })

-- ★ 分割統治（yield のたびに 1 ステップ止まる）
rec' :: Pts -> [Int] -> [Int] -> Int -> Gen S (Maybe Best)
rec' pts xs ys depth = do
  let p i = pts U.! i
      px i = fst (p i)
      py i = snd (p i)
      d a b = dist2 (p a) (p b)
      count = length xs
      x0 = minimum (map px xs)
      x1 = maximum (map px xs)
      pushFrame m = modify (\s -> s { sFrames = Frame x0 x1 depth m : sFrames s })
      compareOne a b = modify (\s -> s { sComps = sComps s + 1, sCmp = Just (a, b) })
      -- 3 点以下は全部の組を調べる
      brute res [] = pure res
      brute res ((a, b) : more) = do
        compareOne a b
        setMsg ("Small part (" ++ show count ++ " points): check every pair")
        yield
        brute (better res (Just (Best (d a b) a b))) more
      -- 帯の中の点を y の順に見る
      stripLoop _ res [] = pure res
      stripLoop delta res (a : rest) = do
        res' <- stripInner delta res a (take 7 rest)
        stripLoop delta res' rest
      stripInner _ res _ [] = pure res
      stripInner delta res a (b : more)
        | py b - py a >= delta = pure res
        | otherwise = do
            compareOne a b
            let c = d a b
                closer = maybe True (\r -> c < bD r) res
            if closer then setMsg ("A closer pair across the border! " ++ fix 1 c) else pure ()
            yield
            stripInner delta (if closer then Just (Best c a b) else res) a more
      finish res = do
        modify (\s -> s { sBest = better (sBest s) res, sFrames = drop 1 (sFrames s) })
        pure res
  if count <= 3
    then do
      pushFrame Nothing
      res <- brute Nothing [ (a, b) | (i, a) <- zip [0 :: Int ..] xs, b <- drop (i + 1) xs ]
      finish res
    else do
      let m = count `div` 2
          xl = take m xs
          xr = drop m xs
          border k = case drop k xs of
            (v : _) -> px v
            [] -> 0
          midX = (border (m - 1) + border m) / 2
          leftSet = IS.fromList xl
          yl = filter (`IS.member` leftSet) ys
          yr = filter (not . (`IS.member` leftSet)) ys
      pushFrame (Just midX)
      setMsg ("Split " ++ show count ++ " points by a vertical line: " ++ show (length xl) ++ " and " ++ show (length xr))
      yield
      rl <- rec' pts xl yl (depth + 1)
      rr <- rec' pts xr yr (depth + 1)
      let res = better rl rr
          delta = maybe 1e9 bD res
          -- 帯の中の点（y の順）
          strip' = filter (\i -> abs (px i - midX) < delta) ys
      modify (\s -> s { sStrip = Just (midX, delta) })
      setMsg ("Smaller answer delta = " ++ fix 1 delta ++ ". Check only the 2*delta strip ("
                ++ show (length strip') ++ " points)")
      yield
      res' <- stripLoop delta res strip'
      modify (\s -> s { sStrip = Nothing })
      finish res'

advance :: World -> World
advance w = case wGen w of
  [] -> w
  (s : r) -> w { wS = s, wGen = r }

stepW :: Env -> Float -> World -> IO World
stepW env _ w
  -- 速さ 1 のときは 5 フレームに 1 ステップ
  | wAuto w && (wPerF w > 1 || envFrame env `mod` 5 == 0) = pure (iterate advance w !! wPerF w)
  | otherwise = pure w

drawW :: Env -> World -> Picture
drawW env w =
  let ww = envW env
      hh = envH env
      pts = wPts w
      s = wS w
      p i = pts U.! i
      n = U.length pts
      framesOut = reverse (sFrames s)    -- 外側から順に
      deepest = length framesOut - 1
      framePic (k, f) = Pictures
        [ box env (rgba255 240 163 255 (14 + fDepth f * 6)) (fX0 f - 8) 110 (fX1 f - fX0 f + 16) (hh - 130)
        , case fMid f of
            Just mx -> seg env cOrchid (if k == deepest then 2.5 else 1) (mx, 110) (mx, hh - 20)
            Nothing -> Blank ]
      stripPic = case sStrip s of
        Just (mx, delta) -> box env (rgba255 255 207 107 40) (mx - delta) 110 (delta * 2) (hh - 130)
        Nothing -> Blank
      cmpPic = case sCmp s of
        Just (a, b) -> seg env cGold 2 (p a) (p b)
        Nothing -> Blank
      bestPic = case sBest s of
        Just (Best d a b) -> Pictures [seg env cMint 4 (p a) (p b), hoop env cMint 1 (p a) (d * 2)]
        Nothing -> Blank
      isBest i = maybe False (\b -> i == bA b || i == bB b) (sBest s)
      isCmp i = maybe False (\(a, b) -> i == a || i == b) (sCmp s)
      pointPic i = dot env (if isBest i then cMint else if isCmp i then cGold else cFg) (p i) 9
      -- パネル
      px = ww - 270
      py = 110
      allPairs = n * (n - 1) `div` 2
      bw = 210 / fromIntegral (max allPairs 1)
  in Pictures
       [ Pictures (map framePic (zip [0 ..] framesOut))
       , stripPic
       , cmpPic
       , bestPic
       , Pictures (map pointPic [0 .. n - 1])
       , box env cPanel px py 250 240
       , txt env (px + 18) (py + 16) 17 cFg "Comparisons"
       , txt env (px + 18) (py + 50) 15 cFg ("Divide&conquer " ++ show (sComps s))
       , txt env (px + 18) (py + 72) 15 cFg ("All pairs " ++ show allPairs)
       , txt env (px + 18) (py + 116) 15 cFg ("Recursion depth " ++ show (length framesOut))
       , box env cMint (px + 18) (py + 170) (fromIntegral (sComps s) * bw) 12
       , box env (rgb255 0x4b 0x3a 0x6b) (px + 18) (py + 190) (fromIntegral allPairs * bw) 12
       , txt env 30 24 22 cFg "Closest pair: split, then check only the border"
       , txt env 30 56 14 cDim ("Click add  Space play/stop  Right 1 step  Up/Down speed (" ++ show (wPerF w)
           ++ ")  R random  C clear  1-3 examples")
       , txt env 30 84 17 cGold (sMsg s)
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | my > 110 && mx < envW env - 280 -> pure (restart w { wPts = U.snoc (wPts w) m })
  EventResize _ -> preset env 1 w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wAuto = not (wAuto w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (advance w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wPerF = min (wPerF w * 2) 64 }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wPerF = max (wPerF w `div` 2) 1 }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> preset env 1 w
    | c `elem` "cC" -> pure (restart w { wPts = U.empty })
    | c `elem` "123" -> preset env (fromEnum c - fromEnum '0') w
  _ -> pure w
  where m@(mx, my) = mouse env

-- ---------- やってみよう ----------
-- 1. お手本 2（格子）とかたまり（3）で、比べた回数を比べる
-- 2. 帯の幅を半分にすると、正しい答えが出なくなる例を探そう
-- 3. take 7 を take 2 にすると、まちがう場合はある？

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

