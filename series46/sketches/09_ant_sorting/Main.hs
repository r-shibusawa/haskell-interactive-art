-- ============================================
-- 46-09 アリの仕分け（Deneubourg のモデル）
-- 2 色（または 3 色）の卵がばらまかれている。アリは「まわりに同じ色が少ない卵は拾いやすく、同じ色が多い所には置きやすい」だけ
-- 拾う確率 = (k1 / (k1 + f))²、置く確率 = (f / (k2 + f))²、f = まわり 5×5 にある「同じ色」の割合。色ごとの塊に分かれていく
-- 操作：1 = 2 色 / 2 = 3 色 / ↑↓ = 速さ / ←→ = k1（拾いやすさ）/ R = やり直し / スペース = 一時停止
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
gw = 120                          -- ★ 格子の大きさ
gh = 72

nAnts, nItems :: Int
nAnts = 120                       -- ★ アリの数
nItems = 1600                     -- ★ 卵の数

k2 :: Float
k2 = 0.3                          -- ★ 置く感度

-- 卵の色（赤・青・黄）
pal :: Int -> (Word8, Word8, Word8)
pal c = case c of
  0 -> (255, 120, 80)
  1 -> (90, 180, 255)
  _ -> (250, 230, 90)

data World = World
  { wColors   :: !Int               -- 色の数
  , wK1       :: !Float             -- ★ 拾う感度 k1
  , wMoves    :: !Int               -- アリ 1 匹が 1 フレームに歩く歩数
  , wPaused   :: !Bool
  , wCells    :: !(U.Vector Int)    -- −1 = 空、0〜2 = 卵の色
  , wAx, wAy  :: !(U.Vector Int)    -- アリの位置
  , wLoad     :: !(U.Vector Int)    -- 運んでいる卵の色（−1 = 手ぶら）
  , wSeed     :: !Seed
  , wSameHist :: ![Float]           -- 同種率の記録（新しい順）
  , wSameNow  :: !Float
  }

world0 :: World
world0 = World 2 0.1 40 False U.empty U.empty U.empty U.empty 1 [] 0

main :: IO ()
main = runSketch (sketch "46-09 Ant sorting" world0)
  { skBack  = hsb 25 25 11
  , skStart = \_ -> do
      s <- randomRIO (1, 2 ^ (62 :: Int))
      pure (resetWorld world0 { wSeed = s })
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

-- 卵をばらまき、アリを置き直す
resetWorld :: World -> World
resetWorld w = w' { wSameHist = [], wSameNow = measureSame (wCells w') }
  where
    nc = wColors w
    (cells, s1) = runST $ do
      c <- M.replicate (gw * gh) (-1)
      let place s placed
            | placed >= nItems = pure s
            | otherwise = do
                let (r, s') = rand01 s
                    i = min (gw * gh - 1) (floor (r * fromIntegral (gw * gh)))
                v <- M.read c i
                if v < 0 then M.write c i (placed `mod` nc) >> place s' (placed + 1) else place s' placed
      s' <- place (wSeed w) 0
      v <- U.freeze c
      pure (v, s')
    w' = w { wCells = cells
           , wAx = U.generate nAnts (\i -> floor (hashRand s1 (2 * i) * fromIntegral gw))
           , wAy = U.generate nAnts (\i -> floor (hashRand s1 (2 * i + 1) * fromIntegral gh))
           , wLoad = U.replicate nAnts (-1), wSeed = nextSeed s1 }

idx :: Int -> Int -> Int
idx x y = ((y + gh) `mod` gh) * gw + ((x + gw) `mod` gw)

-- ★ アリ全員を moves 歩進める：歩く → 拾う or 置く（1 匹ずつ順番に格子を書きかえる）
runAnts :: World -> World
runAnts w = runST $ do
  cells <- U.thaw (wCells w)
  ax <- U.thaw (wAx w)
  ay <- U.thaw (wAy w)
  load <- U.thaw (wLoad w)
  let k1 = wK1 w
      -- f = まわり 5×5（自分以外の 24 マス）にある色 c の卵の割合
      fraction x y c = do
        let go [] n = pure n
            go ((dx, dy) : rest) n = do
              v <- M.read cells (idx (x + dx) (y + dy))
              go rest (if v == c then n + 1 else n)
        n <- go [ (dx, dy) | dy <- [-2 .. 2], dx <- [-2 .. 2], dx /= 0 || dy /= 0 ] (0 :: Int)
        pure (fromIntegral n / 24 :: Float)
      antMove s0 i = do
        x <- M.read ax i
        y <- M.read ay i
        let (r1, s1) = rand01 s0
            (r2, s2) = rand01 s1
            (u, s3) = rand01 s2
            x' = (x + floor (r1 * 3) - 1 + gw) `mod` gw
            y' = (y + floor (r2 * 3) - 1 + gh) `mod` gh
            c = idx x' y'
        M.write ax i x'
        M.write ay i y'
        l <- M.read load i
        v <- M.read cells c
        if l < 0 && v >= 0
          then do
            f <- fraction x' y' v
            when (u < (k1 / (k1 + f)) ^ (2 :: Int)) $ M.write load i v >> M.write cells c (-1)
          else when (l >= 0 && v < 0) $ do
            f <- fraction x' y' l
            when (u < (f / (k2 + f)) ^ (2 :: Int)) $ M.write cells c l >> M.write load i (-1)
        pure s3
  s' <- foldM (\s _ -> foldM antMove s [0 .. nAnts - 1]) (wSeed w) [1 .. wMoves w]
  cells' <- U.freeze cells
  ax' <- U.freeze ax
  ay' <- U.freeze ay
  load' <- U.freeze load
  pure w { wCells = cells', wAx = ax', wAy = ay', wLoad = load', wSeed = s' }

-- 仕分けの度合い：卵ごとに「となり 8 マスの卵のうち同じ色の割合」を平均
measureSame :: U.Vector Int -> Float
measureSame cells = if cnt > 0 then total / fromIntegral cnt else 0
  where
    one i =
      let c = cells U.! i
          x = i `mod` gw
          y = i `div` gw
          ns = [ d | dy <- [-1, 0, 1], dx <- [-1, 0, 1], dx /= 0 || dy /= 0, let d = cells U.! idx (x + dx) (y + dy), d >= 0 ]
          same = length (filter (== c) ns)
      in if c < 0 || null ns then Nothing else Just (fromIntegral same / fromIntegral (length ns) :: Float)
    rs = [ r | i <- [0 .. gw * gh - 1], Just r <- [one i] ]
    total = sum rs
    cnt = length rs

step :: Env -> World -> World
step env w0
  | envFrame env `mod` 10 == 0 = let s = measureSame (wCells w1) in w1 { wSameNow = s, wSameHist = take 300 (s : wSameHist w1) }
  | otherwise = w1
  where w1 = if wPaused w0 then w0 else runAnts w0

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 格子を画像にする（白 = アリ）
  [ Translate (20 + iw / 2) (85 + ih / 2) (Scale 1 (-1) (pixelImage gw gh (iw / fromIntegral gw) (\x y -> img U.! (y * gw + x))))
  -- ---- 右：数字とグラフ
  , drawPlot (wSameHist w) gx 120 gwid 120 (1 / nc - 0.05) 1 (hsb 30 80 100) ("Same-colour neighbours = " ++ fmt 1 (wSameNow w * 100) ++ " %")
  , txt gx 260 14 (hsb 0 0 70) ("random: about " ++ fmt 0 (100 / nc) ++ " % (" ++ show (wColors w) ++ " colours)")
  , txt gx 300 14 (hsb 0 0 85) (show nItems ++ " eggs (" ++ show (wColors w) ++ " colours), " ++ show nAnts ++ " ants")
  , txt gx 324 14 (hsb 0 0 85) ("Carrying: " ++ show carried ++ " ants (white = ant)")
  , txt gx 362 14 (hsb 0 0 65) "P(pick) = (k1 / (k1 + f))^2"
  , txt gx 386 14 (hsb 0 0 65) "P(drop) = (f / (k2 + f))^2"
  , txt gx 410 14 (hsb 0 0 65) "f = same colour share in 5x5"
  , txt gx 434 14 (hsb 0 0 65) ("k1 = " ++ fmt 2 k1 ++ ", k2 = " ++ fmt 2 k2)
  -- 拾う・置く確率のグラフ
  , Color (hsb 0 0 40) (rectLine (cx + cw / 2) (cy + ch / 2) cw ch)
  , path 2 (hsb 20 80 100) [ (cx + f * cw, cy + ch - (k1 / (k1 + f)) ^ (2 :: Int) * ch) | k <- [0 .. 40 :: Int], let f = fromIntegral k / 40 ]
  , path 2 (hsb 200 70 100) [ (cx + f * cw, cy + ch - (f / (k2 + f)) ^ (2 :: Int) * ch) | k <- [0 .. 40 :: Int], let f = fromIntegral k / 40 ]
  , txt (cx + 6) (cy + 20) 14 (hsb 20 80 100) "pick"
  , txt (cx + cw - 44) (cy + 34) 14 (hsb 200 70 100) "drop"
  , txt (cx + cw - 30) (cy + ch + 18) 14 (hsb 0 0 60) "f ->"
  , txt 20 36 17 (hsb 0 0 100) ("1: 2 colours   2: 3 colours   Up/Down: speed (" ++ show (wMoves w) ++ " steps/frame)   Left/Right: k1 = "
                                ++ fmt 2 k1 ++ "   R: reset   Space: " ++ (if wPaused w then "paused" else "running"))
  , txt 20 60 14 (hsb 0 0 70) "The ants do not mean to sort. Two probabilities (pick, drop) alone make same-colour clusters" ]
  where
    k1 = wK1 w
    nc = fromIntegral (wColors w) :: Float
    iw = min (envW env * 0.68) ((envH env - 110) * fromIntegral gw / fromIntegral gh)
    ih = iw * fromIntegral gh / fromIntegral gw
    gx = 20 + iw + 30
    gwid = envW env - gx - 25
    carried = U.length (U.filter (>= 0) (wLoad w))
    cx = gx
    cy = 470
    cw = min gwid 260
    ch = 110
    base = U.map (\c -> if c >= 0 then pal c else (38, 30, 26)) (wCells w)
    img = base U.// [ (y * gw + x, (255, 255, 255)) | (x, y) <- U.toList (U.zip (wAx w) (wAy w)) ]

event :: Event -> World -> World
event ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wMoves = min 320 (wMoves w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wMoves = max 1 (round (fromIntegral (wMoves w) / 2 :: Float)) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wK1 = min 1 (wK1 w + 0.05) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wK1 = max 0.01 (wK1 w - 0.05) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c == '1' -> resetWorld w { wColors = 2 }
    | c == '2' -> resetWorld w { wColors = 3 }
    | c `elem` "rR" -> resetWorld w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 同種率が 50 % → 何 % まで上がるか、時間とともに見よう。塊は時間がたつと数が減って大きくなる？
-- 2. k1 を 0.5 にすると、塊の中の卵も拾ってしまう。塊はどうなる？
-- 3. 2（3 色）にして同じことを。ランダムなら 33 % から始まる
-- 4. k2 を 0.05 にすると、どこにでも置いてしまう。仕分けは進む？
-- 5. 実際のアリは卵・幼虫・さなぎを大きさごとに別の場所に集める。ロボットで倉庫を仕分ける研究もある

-- ---------- 道具 ----------
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
