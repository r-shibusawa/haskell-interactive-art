-- ============================================
-- 46-08 シロアリの柱づくり ─ スティグマジー（環境を介した協調）
-- シロアリは設計図を持たない。ランダムに歩き、「土の少ない所では拾い、土の多い所に置く」だけ
-- 置いた土からはにおい（セメントフェロモン）が広がり、土を運ぶシロアリはにおいの濃い方へ歩く → 小山が育って柱になり、柱どうしは適度に離れる
-- 操作：↑↓ = シロアリの数 / ←→ = 速さ / P = におい ON/OFF / ドラッグ = 土をまく / R = やり直し / スペース = 一時停止
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, xor)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word (Word64, Word8)
import Numeric (showFFloat)

gw, gh :: Int
gw = 150                         -- ★ 格子の大きさ（マスの数）
gh = 88

k1, k2, chem, evap, diffuse, soilRatio :: Float
k1 = 0.4                         -- ★ 拾う感度（Deneubourg の式の定数）
k2 = 0.3                         -- ★ 置く感度
chem = 6                         -- ★ においへの引かれやすさ
evap = 0.02                      -- ★ においの蒸発
diffuse = 0.2                    -- ★ においの広がり
soilRatio = 0.3                  -- ★ 最初に土があるマスの割合

pillarH :: Int
pillarH = 8                      -- ★ この高さ以上を「柱」と数える

dirs :: [(Int, Int)]
dirs = [(1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]

data World = World
  { wN          :: !Int                -- シロアリの数
  , wSteps      :: !Int                -- 1 フレームの歩数
  , wUsePher    :: !Bool
  , wPaused     :: !Bool
  , wHgt        :: !(U.Vector Int)     -- 土の高さ
  , wPher       :: !(U.Vector Float)   -- におい
  , wTx, wTy    :: !(U.Vector Int)     -- シロアリの位置
  , wTd         :: !(U.Vector Int)     -- 向き（0〜7）
  , wTc         :: !(U.Vector Bool)    -- 土を運んでいるか
  , wSeed       :: !Seed
  , wPillarHist :: ![Float]            -- 柱の本数の記録（新しい順）
  , wMaxHHist   :: ![Float]
  , wPillars    :: !Int
  , wMaxH       :: !Int
  , wPeaks      :: ![(Int, Int, Int)]  -- 柱のてっぺん (x, y, 高さ)
  }

world0 :: World
world0 = World 500 20 True False U.empty U.empty U.empty U.empty U.empty U.empty 1 [] [] 0 0 []

main :: IO ()
main = runSketch (sketch "46-08 Termite mound" world0)
  { skBack  = hsb 30 35 12
  , skStart = \_ -> do
      s <- randomRIO (1, 2 ^ (62 :: Int))
      pure (resetWorld world0 { wSeed = s })
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

resetWorld :: World -> World
resetWorld w = w { wHgt = U.generate (gw * gh) (\i -> if hashRand s1 i < soilRatio then 1 else 0)
                 , wPher = U.replicate (gw * gh) 0
                 , wTx = U.generate n (\i -> floor (hashRand s2 (3 * i) * fromIntegral gw))
                 , wTy = U.generate n (\i -> floor (hashRand s2 (3 * i + 1) * fromIntegral gh))
                 , wTd = U.generate n (\i -> floor (hashRand s2 (3 * i + 2) * 8))
                 , wTc = U.replicate n False
                 , wSeed = nextSeed s2, wPillarHist = [], wMaxHHist = [] }
  where
    n = wN w
    s1 = nextSeed (wSeed w)
    s2 = nextSeed s1

idx :: Int -> Int -> Int
idx x y = ((y + gh) `mod` gh) * gw + ((x + gw) `mod` gw)

-- ★ シロアリ全員を steps 歩進める（1 匹ずつ順番に。土とにおいは書きかえながら進む）
runTermites :: World -> World
runTermites w = runST $ do
  h <- U.thaw (wHgt w)
  p <- U.thaw (wPher w)
  tx <- U.thaw (wTx w)
  ty <- U.thaw (wTy w)
  td <- U.thaw (wTd w)
  tc <- U.thaw (wTc w)
  let n = U.length (wTx w)
      usePher = wUsePher w
      turnW t = case t of { 0 -> 8; 1 -> 4; 2 -> 1; 3 -> 0.3; _ -> 0.1 } :: Float
      dirsV = U.fromList dirs
      -- 向き k を選ぶ重み：だいたいまっすぐ。土を運んでいるときは、においの濃い方を選びやすい
      weight x y d carry k = do
        let turn = min ((k - d + 8) `mod` 8) ((d - k + 8) `mod` 8)
            (ddx, ddy) = dirsV U.! k
        if carry && usePher
          then do ph <- M.read p (idx (x + ddx) (y + ddy))
                  pure (turnW turn * (1 + chem * ph))
          else pure (turnW turn)
      -- まわり 3×3 の土の平均の高さ
      localSoil x y = do
        vs <- mapM (\(ddx, ddy) -> M.read h (idx (x + ddx) (y + ddy))) [(a, b) | b <- [-1, 0, 1], a <- [-1, 0, 1]]
        pure (fromIntegral (sum vs) / 9 :: Float)
      termiteStep s0 i = do
        x <- M.read tx i
        y <- M.read ty i
        d <- M.read td i
        carry <- M.read tc i
        ws <- mapM (weight x y d carry) [0 .. 7]
        let (r, s1) = rand01 s0
            pick rr k (wk : rest) | k < 7 && rr > wk = pick (rr - wk) (k + 1) rest
            pick _ k _ = k
            k = pick (r * sum ws) 0 ws
            (ddx, ddy) = dirsV U.! k
            x' = (x + ddx + gw) `mod` gw
            y' = (y + ddy + gh) `mod` gh
            c = idx x' y'
        M.write td i k
        M.write tx i x'
        M.write ty i y'
        -- 拾う・置く：f = まわりの土の量
        f <- localSoil x' y'
        hc <- M.read h c
        let (u, s2) = rand01 s1
        if not carry && hc > 0
          then do
            let pPick = (k1 / (k1 + f)) ^ (2 :: Int)          -- 土が少ない所ほど拾いやすい
            when (u < pPick) $ M.write h c (hc - 1) >> M.write tc i True
          else when carry $ do
            pc <- M.read p c
            let g = f + (if usePher then pc else 0)
                pDrop = (g / (k2 + g)) ^ (2 :: Int)          -- 土（とにおい）が多い所ほど置きやすい
            when (u < pDrop * 0.5) $ do
              M.write h c (min 60000 (hc + 1))
              M.write tc i False
              M.write p c (pc + 1)
        pure s2
  s' <- foldM (\s _ -> foldM termiteStep s [0 .. n - 1]) (wSeed w) [1 .. wSteps w]
  h' <- U.freeze h
  p' <- U.freeze p
  tx' <- U.freeze tx
  ty' <- U.freeze ty
  td' <- U.freeze td
  tc' <- U.freeze tc
  pure w { wHgt = h', wPher = p', wTx = tx', wTy = ty', wTd = td', wTc = tc', wSeed = s' }

-- においを広げて、少しずつ消す
updatePheromone :: U.Vector Float -> U.Vector Float
updatePheromone ph = U.generate (gw * gh) $ \i ->
  let x = i `mod` gw
      y = i `div` gw
      avg = (ph U.! idx (x + 1) y + ph U.! idx (x - 1) y + ph U.! idx x (y + 1) + ph U.! idx x (y - 1)) / 4
      v = ph U.! i
  in min 50 ((v + diffuse * (avg - v)) * (1 - evap))

-- 柱を数える：高さ pillarH 以上のマスのかたまりの数（つながったもの = 1 本）。てっぺんも返す
countPillars :: U.Vector Int -> [(Int, Int, Int)]
countPillars h = runST $ do
  seen <- M.replicate (gw * gh) False
  let flood [] top = pure top
      flood (c : stack) top = do
        let cx = c `mod` gw
            cy = c `div` gw
            top' = if h U.! c > h U.! top then c else top
        new <- fmap concat $ mapM (\(ddx, ddy) -> do
                 let j = idx (cx + ddx) (cy + ddy)
                 sj <- M.read seen j
                 if not sj && h U.! j >= pillarH then M.write seen j True >> pure [j] else pure []) dirs
        flood (new ++ stack) top'
      go i acc
        | i >= gw * gh = pure (reverse acc)
        | otherwise = do
            si <- M.read seen i
            if si || h U.! i < pillarH then go (i + 1) acc
              else do
                M.write seen i True
                top <- flood [i] i
                go (i + 1) ((top `mod` gw, top `div` gw, h U.! top) : acc)
  go 0 []

step :: Env -> World -> World
step env w0 = count w2
  where
    w1 = if wPaused w0 then w0
         else let w' = runTermites w0 in if wUsePher w' then w' { wPher = updatePheromone (wPher w') } else w'
    -- ドラッグで土をまく（まわり 5×5 に 6 回）
    w2 = case toGrid env (envMouse env) of
      Just (gx, gy) | envDown env ->
        let s = wSeed w1
            cells = [ idx (gx + floor (hashRand s (2 * k) * 5) - 2) (gy + floor (hashRand s (2 * k + 1) * 5) - 2) | k <- [0 .. 5 :: Int] ]
        in w1 { wHgt = U.accum (\v _ -> min 60000 (v + 1)) (wHgt w1) [ (c, ()) | c <- cells ], wSeed = nextSeed s }
      _ -> w1
    count w
      | envFrame env `mod` 15 /= 0 = w
      | otherwise =
          let peaks = countPillars (wHgt w)
              mh = U.maximum (wHgt w)
          in w { wPeaks = peaks, wPillars = length peaks, wMaxH = mh
               , wPillarHist = take 300 (fromIntegral (length peaks) : wPillarHist w)
               , wMaxHHist = take 300 (fromIntegral mh : wMaxHHist w) }

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 土の高さ（明るさ）とにおい（紫）を画像にする
  [ Translate (ax + aw / 2) (ay + ah / 2) (Scale 1 (-1) (pixelImage gw gh kk (\x y -> img U.! (y * gw + x))))
  , Color (hsbA 40 60 100 60) (Pictures [ Translate (ax + (fromIntegral px + 0.5) * kk) (ay + (fromIntegral py + 0.5) * kk)
                                                     (ring (kk * (1.5 + sqrt (fromIntegral ph)) / 2) 1.5)
                                        | (px, py, ph) <- wPeaks w ])
  -- ---- 右：数字とグラフ
  , drawPlot (wPillarHist w) gx 120 gwid 90 0 120 (hsb 30 80 100) ("Pillars (h >= " ++ show pillarH ++ ") = " ++ show (wPillars w))
  , drawPlot (wMaxHHist w) gx 250 gwid 90 0 100 (hsb 200 60 100) ("Tallest pillar = " ++ show (wMaxH w))
  , txt gx 380 14 (hsb 0 0 85) ("Carrying soil: " ++ show carrying ++ " / " ++ show (U.length (wTc w)))
  , txt gx 404 14 (hsb 0 0 85) "yellow = carrying, grey = empty"
  , txt gx 428 14 (hsb 0 0 85) "bright = tall soil, purple = scent"
  , txt gx 466 14 (hsb 0 0 65) "P(pick) = (k1 / (k1 + f))^2"
  , txt gx 490 14 (hsb 0 0 65) "P(drop) = (f / (k2 + f))^2"
  , txt gx 514 14 (hsb 0 0 65) "f = soil around (+ scent)"
  , txt 20 36 17 (hsb 0 0 100) ("Up/Down: " ++ show (wN w) ++ " termites   Left/Right: speed x" ++ show (wSteps w)
                                ++ "   P: scent " ++ (if wUsePher w then "ON" else "OFF") ++ "   drag: drop soil   R: reset   Space: "
                                ++ (if wPaused w then "paused" else "running"))
  , txt 20 60 14 (hsb 0 0 70) "No one sees the whole, yet pillars grow. Old work (piled soil) calls the next work = stigmergy" ]
  where
    (ax, ay, aw, ah) = area env
    kk = aw / fromIntegral gw
    gx = ax + aw + 30
    gwid = envW env - gx - 25
    carrying = U.length (U.filter id (wTc w))
    base = U.generate (gw * gh) $ \i ->
      let hh = min 1 (sqrt (fromIntegral (wHgt w U.! i) / 25))      -- 高いほど明るい（√ で低い山も見える）
          pp = min 1 (wPher w U.! i * 0.6)
      in (b8 (40 + 180 * hh + 70 * pp), b8 (30 + 130 * hh), b8 (30 + 60 * hh + 110 * pp))
    -- シロアリ：運び中 = 黄、手ぶら = うすい灰色（重なったら順に塗る）
    paint (r, g, b) carry
      | carry = (255, 220, 40)
      | otherwise = (avg8 r 160, avg8 g 160, avg8 b 170)
    img = U.accum paint base (U.toList (U.zip (U.zipWith (\x y -> y * gw + x) (wTx w) (wTy w)) (wTc w)))

event :: Event -> World -> World
event ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> resetWorld w { wN = min 1500 (wN w + 100) }
  EventKey (SpecialKey KeyDown) Down _ _ -> resetWorld w { wN = max 100 (wN w - 100) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSteps = min 60 (wSteps w + 5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wSteps = max 1 (wSteps w - 5) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "pP" -> w { wUsePher = not (wUsePher w) }
    | c `elem` "rR" -> resetWorld w
  _ -> w

-- ---------- やってみよう ----------
-- 1. P でにおいを OFF にすると？ 小山はできるが、柱のように高くはなりにくい。においが「ここに積め」の合図
-- 2. k2 を 0.05 にすると少しの土でも置く → 小さな山がたくさん。k2 = 1 だと？
-- 3. ドラッグで土をまいて「種」を作ると、そこに柱が育つ？ 柱と柱の間隔はにおいの広がり diffuse で変わる
-- 4. 柱の本数のグラフは、最初に増えて、あとで減る。小さい山が大きい山に吸収されるから（勝者総取り）
-- 5. runTermites は 1 匹ずつ順番に土を書きかえる（runST の中だけで配列を変える）。順番を変えると結果は変わる？

-- ---------- 道具 ----------
-- 格子を描く場所 (左, 上, 幅, 高さ)
area :: Env -> (Float, Float, Float, Float)
area env = let w = min (envW env * 0.7) ((envH env - 110) * fromIntegral gw / fromIntegral gh)
           in (20, 85, w, w * fromIntegral gh / fromIntegral gw)

-- 画面の位置（gloss の座標）→ 格子のマス
toGrid :: Env -> Point -> Maybe (Int, Int)
toGrid env (mx0, my0)
  | mx < ax || mx >= ax + aw || my < ay || my >= ay + ah = Nothing
  | otherwise = Just (floor ((mx - ax) / aw * fromIntegral gw), floor ((my - ay) / ah * fromIntegral gh))
  where
    (ax, ay, aw, ah) = area env
    mx = mx0 + envW env / 2
    my = envH env / 2 - my0

b8 :: Float -> Word8
b8 v = fromIntegral (max 0 (min 255 (round v :: Int)))

avg8 :: Word8 -> Int -> Word8
avg8 a b = fromIntegral ((fromIntegral a + b + 1) `div` 2 :: Int)

-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太さ w の折れ線
path :: Float -> Color -> [Point] -> Picture
path w c ps = if w <= 1 then Color c (Line ps) else Pictures (zipWith (seg w c) ps (drop 1 ps))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""

-- グラフ：記録 hist（新しい順、300 個まで）を左上 (x, y)、幅 w、高さ h の枠に lo〜hi で描く
drawPlot :: [Float] -> Float -> Float -> Float -> Float -> Float -> Float -> Color -> String -> Picture
drawPlot hist x y w h lo hi c title = Pictures
  [ Color (hsbA 0 0 0 35) (rect (x + w / 2) (y + h / 2) w h)
  , txt x (y - 8) 14 (hsb 0 0 85) title
  , if length hist < 2 then Blank
    else path 2 c [ (x + fromIntegral i / 299 * w, y + h - clampF 0 1 ((v - lo) / (hi - lo)) * h)
                  | (i, v) <- zip [0 :: Int ..] (reverse hist) ] ]

-- 乱数（xorshift64）。種を World に入れておけば skStep は純粋なまま
type Seed = Word64

nextSeed :: Seed -> Seed
nextSeed s0 = let s1 = s0 `xor` (s0 `shiftL` 13)
                  s2 = s1 `xor` (s1 `shiftR` 7)
              in s2 `xor` (s2 `shiftL` 17)

-- 種を 1 つ進めて 0〜1 の乱数を 1 つ取り出す（順番に使うとき用）
rand01 :: Seed -> (Float, Seed)
rand01 s = let s' = nextSeed s in (fromIntegral (s' `shiftR` 40) / 16777216, s')

-- 種 s と番号 i から決まる 0〜1 の乱数（たくさんの粒子に一度に配るとき用）
hashRand :: Seed -> Int -> Float
hashRand s i =
  let z0 = s + fromIntegral i * 0x9E3779B97F4A7C15
      z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
      z3 = z2 `xor` (z2 `shiftR` 31)
  in fromIntegral (z3 `shiftR` 40) / 16777216
