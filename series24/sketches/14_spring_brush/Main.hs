-- ============================================
-- 14 ばね質点ブラシ
-- マウスで描いた線が、おもり（質点）とばねの鎖になって重力で揺れる。床にぶつかると音が鳴る
--   自由制作のベース：ばねの硬さ・重力・色・音の決め方を変えて、自分だけの「動く絵筆」をつくろう
-- ドラッグ = 鎖を描く（点の近くを押すとつかむ）/ B = 描いた線を輪（ぷるぷる）にする / P = 始点を固定
-- G = 重力 ON/OFF / T = 残像 / S = 画像を保存 / C = 全部消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.IORef
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Maybe (isNothing)

gravityA, kSpring, damping, seg, bounce :: Float
gravityA = 900      -- ★ 重力 [px/s²]
kSpring = 20000     -- ★ ばねの硬さ
damping = 0.9995    -- ★ 速度の減衰（1 に近いほど長く揺れる）
seg = 14            -- ★ 鎖の点の間隔 [px]
bounce = 0.6        -- ★ 床の反発係数

maxP :: Int
maxP = 1400         -- ★ 点の最大数

ghostMax :: Int
ghostMax = 24       -- ★ 残像を何フレーム分持つか

-- 点は [x, y, px, py, x, y, px, py, ...] と 1 本の配列に並べる（ベルレ積分の「前の位置」が px, py）
data World = World
  { wSt      :: !(U.Vector Float)
  , wHue     :: !(U.Vector Float)
  , wPinned  :: !(U.Vector Bool)
  , wLen     :: !(U.Vector Int)     -- その点の鎖の長さ（描いている途中は 0）
  , wSpA, wSpB :: !(U.Vector Int)   -- ばねの両端
  , wRest, wK :: !(U.Vector Float)  -- ばねの自然長と硬さ
  , wChain   :: ![Int]              -- 描いている途中の鎖（新しい順）
  , wGrab    :: !Int
  , wGravity, wTrails, wPinFirst, wRing :: !Bool
  , wHueNow  :: !Float
  , wLastPing :: !Float
  , wGhosts  :: !(Seq.Seq (U.Vector Float))   -- 残像：過去のフレームの点
  }

main :: IO ()
main = runSketch (sketch "24-14 Spring Brush" sampleWorld)
  { skBack  = hsb 240 30 8
  , skStep  = stepWorld
  , skDraw  = \_ w -> drawAll w
  , skEvent = onEvent
  }

emptyWorld :: World
emptyWorld = World U.empty U.empty U.empty U.empty U.empty U.empty U.empty U.empty
                   [] (-1) True False True False 0 (-1) Seq.empty

-- 最初の見本：垂れ下がる鎖を 4 本
sampleWorld :: World
sampleWorld = foldl chainK emptyWorld [0 .. 3 :: Int]
  where
    chainK w k =
      let hue = fromIntegral k * 70
          x0 = width * (0.2 + fromIntegral k * 0.2)
          w1 = addPoint x0 120 hue True w
          base = pointCount w
          w2 = foldl (\q i ->
                 let fi = fromIntegral i
                     q1 = addPoint (x0 + fi * seg * 0.9) (120 + fi * 4) (hue + fi * 2) False q
                     p = base + i
                     q2 = addSpring (p - 1) p kSpring q1
                 in if i >= 2 then addSpring (p - 2) p (kSpring * 0.03) q2 else q2) w1 [1 .. 17 :: Int]
      in setLens [base .. base + 17] w2 { wHueNow = hue }

pointCount :: World -> Int
pointCount w = U.length (wSt w) `div` 4

posOf :: U.Vector Float -> Int -> Point
posOf st i = (U.unsafeIndex st (4 * i), U.unsafeIndex st (4 * i + 1))

addPoint :: Float -> Float -> Float -> Bool -> World -> World
addPoint x y hue pin w =
  w { wSt = wSt w U.++ U.fromList [x, y, x, y]
    , wHue = U.snoc (wHue w) hue, wPinned = U.snoc (wPinned w) pin, wLen = U.snoc (wLen w) 0 }

addSpring :: Int -> Int -> Float -> World -> World
addSpring a b k w =
  w { wSpA = U.snoc (wSpA w) a, wSpB = U.snoc (wSpB w) b
    , wRest = U.snoc (wRest w) (dist2 (posOf (wSt w) a) (posOf (wSt w) b))
    , wK = U.snoc (wK w) k }

setLens :: [Int] -> World -> World
setLens ids w = let n = length ids in w { wLen = wLen w U.// [(i, n) | i <- ids] }

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w = do
  let n = pointCount w
      nSp = U.length (wSpA w)
      sub = 8 :: Int
      h = 1 / 60 / fromIntegral sub :: Float
      floorY = height - 30
      grabbed = wGrab w
      grav = if wGravity w then gravityA else 0
  st <- U.thaw (wSt w)
  firstHit <- newIORef Nothing
  forM_ [1 .. sub] $ \_ -> do
    -- ★ ばねの力 F = −k (伸び)。両端の点に逆向きに
    forM_ [0 .. nSp - 1] $ \s -> do
      let a = U.unsafeIndex (wSpA w) s
          b = U.unsafeIndex (wSpB w) s
      ax <- M.unsafeRead st (4 * a)
      ay <- M.unsafeRead st (4 * a + 1)
      bx <- M.unsafeRead st (4 * b)
      by <- M.unsafeRead st (4 * b + 1)
      let dx = bx - ax
          dy = by - ay
          d0 = sqrt (dx * dx + dy * dy)
          d = if d0 == 0 then 1e-6 else d0
          f = U.unsafeIndex (wK w) s * (d - U.unsafeIndex (wRest w) s) / d * h * h
      when (not (U.unsafeIndex (wPinned w) a)) $ do
        M.unsafeWrite st (4 * a) (ax + dx * f * 0.5)
        M.unsafeWrite st (4 * a + 1) (ay + dy * f * 0.5)
      when (not (U.unsafeIndex (wPinned w) b)) $ do
        M.unsafeWrite st (4 * b) (bx - dx * f * 0.5)
        M.unsafeWrite st (4 * b + 1) (by - dy * f * 0.5)
    -- ベルレ積分：次の位置 = いま + (いま − 前) × 減衰 + 加速度 × h²
    forM_ [0 .. n - 1] $ \i -> do
      x <- M.unsafeRead st (4 * i)
      y <- M.unsafeRead st (4 * i + 1)
      if U.unsafeIndex (wPinned w) i || i == grabbed
        then do
          M.unsafeWrite st (4 * i + 2) x
          M.unsafeWrite st (4 * i + 3) y
        else do
          ppx <- M.unsafeRead st (4 * i + 2)
          ppy <- M.unsafeRead st (4 * i + 3)
          let vx = clampF (-40) 40 ((x - ppx) * damping)
              vy = clampF (-40) 40 ((y - ppy) * damping)
              x1 = x + vx
              y1 = y + vy + grav * h * h
          M.unsafeWrite st (4 * i + 2) x
          M.unsafeWrite st (4 * i + 3) y
          M.unsafeWrite st (4 * i) x1
          M.unsafeWrite st (4 * i + 1) y1
          -- 床と壁
          when (y1 > floorY) $ do
            let impact = vy / h       -- ぶつかる速さ [px/s]
            M.unsafeWrite st (4 * i + 1) floorY
            M.unsafeWrite st (4 * i + 3) (floorY + vy * bounce)
            when (impact > 250) $
              modifyIORef' firstHit (\m -> if isNothing m then Just (i, impact) else m)
          when (x1 < 0) $ do
            M.unsafeWrite st (4 * i) 0
            M.unsafeWrite st (4 * i + 2) (vx * bounce)
          when (x1 > width) $ do
            M.unsafeWrite st (4 * i) width
            M.unsafeWrite st (4 * i + 2) (width + vx * bounce)
  when (grabbed >= 0 && grabbed < n) $ do
    let (mx, my) = toP5 (envMouse env)
    M.unsafeWrite st (4 * grabbed) mx
    M.unsafeWrite st (4 * grabbed + 1) my
  st' <- U.unsafeFreeze st
  hit <- readIORef firstHit
  lastPing <- case hit of
    Just (i, impact) | envTime env - wLastPing w >= 0.04 || wLastPing w < 0 -> do
      ping (U.unsafeIndex (wLen w) i) impact
      pure (envTime env)
    _ -> pure (wLastPing w)
  let ghosts | wTrails w = let g = wGhosts w Seq.|> wSt w
                           in if Seq.length g > ghostMax then Seq.drop 1 g else g
             | otherwise = Seq.empty
  pure w { wSt = st', wLastPing = lastPing, wGhosts = ghosts }

-- ★ 衝突音：鎖が短いほど高い音、ぶつかる速さで音の大きさ
ping :: Int -> Float -> IO ()
ping len0 impact = do
  let len = if len0 > 0 then len0 else 10
      f = clampF 120 1600 (1600 / sqrt (fromIntegral len))
      scaleNotes = [0, 2, 4, 7, 9] :: [Int]     -- ★ ペンタトニック（ずれても気持ちいい音階）
      midi0 = round (12 * logBase 2 (f / 440) + 69) :: Int
      snap m = if (m `mod` 12) `elem` scaleNotes then m else snap (m - 1)
  playTone (midiFreq (snap midi0)) (clampF 0.05 0.4 (impact / 3000)) 0.5 Triangle

drawAll :: World -> Picture
drawAll w = Pictures
  [ view ( ghostPics
        ++ [ Color (hsba (U.unsafeIndex (wHue w) a) 70 (clampF 50 100 (60 + (stretch - 1) * 300)) 0.9) (thickLine 3 pa pb)
           | s <- [0 .. U.length (wSpA w) - 1]
           , let a = U.unsafeIndex (wSpA w) s
           , let b = U.unsafeIndex (wSpB w) s
           , let pa = posOf st a
           , let pb = posOf st b
           , let stretch = dist2 pa pb / max 1e-6 (U.unsafeIndex (wRest w) s) ]
        ++ [ Color (hsb (U.unsafeIndex (wHue w) i) (if pin then 20 else 60) 100) (at (posOf st i) (disc (if pin then 5 else 3)))
           | i <- [0 .. pointCount w - 1], let pin = U.unsafeIndex (wPinned w) i ]
        ++ [Color (hsb 0 0 30) (thickLine 2 (0, height - 30) (width, height - 30))] )
  , txt 20 36 17 (gray 1) ("Drag: draw or grab   B: ring " ++ onOff (wRing w) ++ "   P: pin first " ++ onOff (wPinFirst w) ++ "   G: gravity " ++ onOff (wGravity w) ++ "   T: trails   S: save   C: clear")
  , txt 20 66 14 (hsb 0 0 65) ("Points " ++ show (pointCount w) ++ " / " ++ show maxP ++ "   Springs " ++ show (U.length (wSpA w)) ++ "   Hitting the floor rings: short chains are higher, fast hits are louder")
  ]
  where
    st = wSt w
    -- 残像：古いフレームほど薄く。ばねが多いときは重くならないようにフレーム数を減らす
    nSp = U.length (wSpA w)
    budget = if nSp == 0 then 0 else min ghostMax (3400 `div` nSp)
    frames = zip [0 :: Int ..] (reverse (toList (wGhosts w)))
    ghostPics =
      [ Color (hsba (U.unsafeIndex (wHue w) a) 70 70 (0.9 * 0.88 ^ age)) (thickLine 3 (posOf g a) (posOf g b))
      | (age, g) <- take budget frames
      , s <- [0 .. nSp - 1]
      , let a = U.unsafeIndex (wSpA w) s
      , let b = U.unsafeIndex (wSpB w) s
      , 4 * a + 1 < U.length g, 4 * b + 1 < U.length g ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos
    | my < 90 -> pure w
    | g >= 0 -> pure w { wGrab = g }
    | pointCount w >= maxP -> pure w
    | otherwise ->
        let hue = wrapF 360 (wHueNow w + 47)
            w1 = addPoint mx my hue (wPinFirst w && not (wRing w)) w { wHueNow = hue }
        in pure w1 { wChain = [pointCount w] }
    where (mx, my) = toP5 pos
          g = nearest mx my 16 w
  EventMotion pos -> pure (extendChain (toP5 pos) w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure (finishChain w { wGrab = -1 })
  EventKey (Char c) Down _ _ -> case toLower c of
    'g' -> pure w { wGravity = not (wGravity w) }
    't' -> pure w { wTrails = not (wTrails w) }
    'p' -> pure w { wPinFirst = not (wPinFirst w) }
    'b' -> pure w { wRing = not (wRing w) }
    'c' -> pure emptyWorld { wGravity = wGravity w, wTrails = wTrails w, wPinFirst = wPinFirst w, wRing = wRing w, wHueNow = wHueNow w, wLastPing = wLastPing w }
    's' -> saveSvg "spring_brush" env (drawAll w) >> pure w
    _ -> pure w
  _ -> pure w

extendChain :: Point -> World -> World
extendChain m@(mx, my) w = case wChain w of
  (lastI : rest)
    | pointCount w < maxP && dist2 m (posOf (wSt w) lastI) >= seg ->
        let p = pointCount w
            chainLen = length (wChain w)
            w1 = addSpring lastI p kSpring (addPoint mx my (wrapF 360 (wHueNow w + fromIntegral chainLen * 2)) False w)
            w2 = case rest of
              (prev : _) -> addSpring prev p (kSpring * 0.03) w1   -- 1 つとばしのばね = 曲げにくさ
              [] -> w1
        in w2 { wChain = p : wChain w }
  _ -> w

finishChain :: World -> World
finishChain w = case wChain w of
  [] -> w
  rev ->
    let c = reverse rev
        n = length c
        half = n `div` 2
        -- 輪にする：最後と最初をつなぎ、向かい側どうしもつなぐ（ぷるぷるのゼリー）
        ringed
          | wRing w && n > 4 =
              let w1 = case (rev, c) of
                    (l : _, f : _) -> addSpring l f kSpring w
                    _ -> w
              in foldl (\q (a, b) -> addSpring a b (kSpring * 0.05) q) w1 (zip (take half c) (drop half c))
          | otherwise = w
    in setLens c ringed { wChain = [] }

-- 半径 r 以内でいちばん近い点の番号（なければ -1）
nearest :: Float -> Float -> Float -> World -> Int
nearest x y r w =
  fst (foldl (\best@(_, bd) i ->
                let (px, py) = posOf (wSt w) i
                    d = (px - x) * (px - x) + (py - y) * (py - y)
                in if d < bd then (i, d) else best)
             (-1, r * r) [0 .. pointCount w - 1])

-- ---------- やってみよう ----------
-- 1. P を OFF にして長い鎖を描き、床に落とす。ON にすると、カーテンのように垂れ下がる
-- 2. B で輪を描くと、ぷるぷるのゼリー。kSpring を小さくするとやわらかく、大きくすると硬くなる
-- 3. G で重力を消して、つかんで振り回す。宇宙のひも
-- 4. ping の音階（scaleNotes）や音色（Triangle）を変えて、落とすたびに曲になる作品に


-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- マウスの位置（gloss の座標）を p5 と同じ座標（左上が原点、y は下向き）に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は左下の位置（p5 の座標）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

onOff :: Bool -> String
onOff b = if b then "ON" else "OFF"
