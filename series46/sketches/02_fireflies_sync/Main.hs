-- ============================================
-- 46-02 ホタルの同期 ─ パルス結合振動子（ミロロ–ストロガッツ型）
-- ホタルは自分のリズムで光る。近くの仲間が光るのを見ると、自分の「光りたさ」x が ε だけ押し上げられ、早めに光る
-- x = f(位相) は上に凸のカーブ。これだけで、バラバラだった光がだんだんそろう。秩序変数 r = |平均 e^{2πiφ}|
-- 操作：↑↓ = 結合の強さ ε / ←→ = 見える距離 / A = 全員が見える ⇄ 近くだけ / クリック = ランタンを光らせる / R = 位相をバラバラに
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Control.Monad.ST (runST)
import Data.Bits (shiftL, shiftR, xor)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word (Word64, Word8)
import Numeric (showFFloat)

nFire :: Int
nFire = 300                 -- ★ ホタルの数

period, jitter, curveB :: Float
period = 1.0                -- ★ 光る周期（秒）
jitter = 0.0                -- ★ 周期のばらつき（0 = 全員同じ、0.05 = ±5%）
curveB = 3.0                -- ★ x = f(φ) の曲がり具合（0 に近いと直線 → そろいにくい）

rasterW, rasterH :: Int
rasterW = 360               -- ラスター図の大きさ（横 = フレーム、縦 = ホタル）
rasterH = 150

data World = World
  { wPos     :: !(U.Vector (Float, Float))   -- 位置（p5.js と同じ座標：左上が原点）
  , wPh      :: !(U.Vector Float)            -- 位相 0〜1
  , wT       :: !(U.Vector Float)            -- 一匹ずつの周期
  , wGlow    :: !(U.Vector Float)            -- 光の残り（1 = 光った直後）
  , wNbr     :: !(V.Vector (U.Vector Int))   -- 見える仲間のリスト
  , wEps     :: !Float                       -- 結合の強さ ε
  , wSeeR    :: !Float                       -- 見える距離（px）
  , wAll     :: !Bool                        -- 全員が見えるか
  , wRHist   :: ![Float]                     -- r の記録（新しい順、300 個まで）
  , wRaster  :: !(U.Vector Word8)            -- ラスター図（1 = 光った）
  , wFlashes :: !Int                         -- このフレームに光った数
  , wSeed    :: !Seed
  }

main :: IO ()
main = runSketch (sketch "46-02 Firefly synchronisation" (makeWorld 1280 800 1))
  { skBack  = hsb 250 50 7
  , skStart = \env -> makeWorld (envW env) (envH env) <$> randomRIO (1, 2 ^ (62 :: Int))
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

makeWorld :: Float -> Float -> Seed -> World
makeWorld sw sh s = buildNeighbours w
  where
    pos = U.generate nFire (\i -> (lerpF 40 (sw * 0.62) (hashRand s (4 * i)), lerpF 110 (sh - 40) (hashRand s (4 * i + 1))))
    ph = U.generate nFire (\i -> hashRand s (4 * i + 2))
    ts = U.generate nFire (\i -> period * (1 + jitter * (hashRand s (4 * i + 3) * 2 - 1)))
    w = World pos ph ts (U.replicate nFire 0) V.empty 0.08 160 False [] (U.replicate (rasterW * rasterH) 0) 0 (nextSeed s)

-- 見える仲間のリストを作り直す
buildNeighbours :: World -> World
buildNeighbours w = w { wNbr = V.generate nFire nb }
  where
    pos = wPos w
    nb i = U.filter (\j -> j /= i && (wAll w || dist2 (pos U.! i) (pos U.! j) < wSeeR w)) (U.enumFromN 0 nFire)

-- ★ 光りたさ x と 位相 φ の関係（上に凸）と、その逆関数
fX :: Float -> Float
fX ph = log (1 + (exp curveB - 1) * ph) / curveB

fInv :: Float -> Float
fInv x = (exp (curveB * x) - 1) / (exp curveB - 1)

-- 1 フレーム進める。返り値は新しい位相と「光ったか」
fire :: World -> (U.Vector Float, U.Vector Bool)
fire w = runST $ do
  -- ---- 1) 全員の位相を進める
  ph <- U.thaw (U.zipWith (\p t -> p + (1 / 60) / t) (wPh w) (wT w))
  done <- M.replicate nFire False
  fired0 <- foldM (\acc i -> do p <- M.read ph i; pure (if p >= 1 then i : acc else acc)) [] [0 .. nFire - 1]
  -- ---- 2) 光ったホタルが仲間を押す。押されて 1 を超えた仲間もその場で光る（連鎖）
  let loop [] = pure ()
      loop (i : rest) = do
        d <- M.read done i
        if d then loop rest else do
          M.write done i True
          M.write ph i 0
          let nb = wNbr w V.! i
              push1 = wEps w / max 1 (sqrt (fromIntegral (U.length nb)) / 3)   -- 仲間が多いと 1 匹あたりの押す量を少し弱める
              pushJ acc j = do
                dj <- M.read done j
                if dj then pure acc else do
                  p <- M.read ph j
                  let x = fX p + push1
                  if x >= 1 then pure (j : acc) else M.write ph j (fInv x) >> pure acc
          new <- foldM pushJ rest (U.toList nb)
          loop new
  loop fired0
  (,) <$> U.freeze ph <*> U.freeze done

step :: Env -> World -> World
step env w = w { wPh = ph, wGlow = glow, wFlashes = U.length (U.filter id done)
               , wRHist = take 300 (r : wRHist w), wRaster = raster }
  where
    (ph, done) = fire w
    glow = U.zipWith (\g d -> if d then 1 else g * 0.86) (wGlow w) done
    -- ---- 3) 秩序変数 r
    sx = U.sum (U.map (\p -> cos (p * tau)) ph)
    sy = U.sum (U.map (\p -> sin (p * tau)) ph)
    r = sqrt (sx * sx + sy * sy) / fromIntegral nFire
    -- ---- ラスター図：1 列 = 1 フレーム、光ったホタルに点（列はぐるっと回して使う）
    col = envFrame env `mod` rasterW
    raster = wRaster w U.// ([ (y * rasterW + col, 0) | y <- [0 .. rasterH - 1] ]
                             ++ [ ((i * rasterH `div` nFire) * rasterW + col, 1) | i <- [0 .. nFire - 1], done U.! i ])

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- ホタルを描く（光った直後ほど大きく明るい）
  [ Pictures [ if g > 0.05 then dot (hsbA 65 80 100 (g * 30)) x y (10 + 30 * g) else Blank
             , dot (hsb 65 (60 + 40 * g) (25 + 75 * g)) x y (4 + 3 * g) ]
  | ((x, y), g) <- U.toList (U.zip (wPos w) (wGlow w)) ]
  ++ [ Color (hsbA 0 0 100 25) (Translate mx my (Circle (wSeeR w))) | not (wAll w), mx < envW env * 0.65 ]
  -- ---- ラスター図（古い → 新しいの順に左から並べる）
  ++ [ Translate (gx + gw / 2) (300 + fromIntegral rasterH / 2)
         (Scale (gw / fromIntegral rasterW) (-1) (pixelImage rasterW rasterH 1 rasterPx))
     , txt gx 292 14 (hsb 0 0 85) "Flash times (x = time, y = firefly number)"
     , drawPlot (wRHist w) gx 120 gw 110 0 1 (hsb 60 80 100) ("Sync r = " ++ fmt 3 r)
  -- ---- 光りたさのカーブ
     , Color (hsb 0 0 40) (rectLine (cx + cw / 2) (cy + ch / 2) cw ch)
     , path 2 (hsb 30 80 100) [ (cx + fromIntegral k / 40 * cw, cy + ch - fX (fromIntegral k / 40) * ch) | k <- [0 .. 40 :: Int] ]
     , txt (cx + 6) (cy + 18) 14 (hsb 0 0 80) "x = f(phase)"
     , txt (cx + cw - 70) (cy + ch + 18) 14 (hsb 0 0 80) "phase ->" ]
  ++ [ txt (cx + cw + 14) (cy + 30 + 20 * i) 14 (hsb 0 0 80) s
     | (i, s) <- zip [0 ..] ["A push raises x by eps.", "The curve is concave, so", "fireflies further along", "jump ahead more."] ]
  ++ [ txt 20 36 18 (hsb 0 0 100) ("Up/Down: coupling eps = " ++ fmt 3 (wEps w) ++ "   Left/Right: sight "
                                   ++ (if wAll w then "everyone" else show (round (wSeeR w) :: Int) ++ " px")
                                   ++ "   A: all-to-all " ++ (if wAll w then "ON" else "off") ++ "   Click: lantern   R: scramble")
     , txt 20 62 14 (hsb 0 0 70) ("Flashing now " ++ show (wFlashes w) ++ " / " ++ show nFire ++ ". The closer r is to 1, the more they flash together") ]
  where
    (mx, my) = toP5 env (envMouse env)
    gx = envW env * 0.66
    gw = envW env - gx - 30
    r = case wRHist w of
          (v : _) -> v
          [] -> 0
    col = envFrame env `mod` rasterW
    rasterPx x y = if wRaster w U.! (y * rasterW + (col + 1 + x) `mod` rasterW) == 1 then (255, 240, 120) else (0, 0, 0)
    cx = gx
    cy = 500
    cw = 170
    ch = 120

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  -- ランタン：クリックした場所の近くのホタルを ε × 3 押す
  EventKey (MouseButton LeftButton) Down _ p ->
    let m = toP5 env p
        push q ph = if dist2 q m < wSeeR w then let x = fX ph + wEps w * 3 in if x >= 1 then 0.999 else fInv x else ph
    in w { wPh = U.zipWith push (wPos w) (wPh w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wEps = min 0.4 (wEps w + 0.01) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wEps = max 0 (wEps w - 0.01) }
  EventKey (SpecialKey KeyRight) Down _ _ -> buildNeighbours w { wSeeR = min 600 (wSeeR w + 20) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> buildNeighbours w { wSeeR = max 40 (wSeeR w - 20) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> buildNeighbours w { wAll = not (wAll w) }
    | c `elem` "rR" -> w { wPh = U.generate nFire (hashRand (wSeed w)), wSeed = nextSeed (wSeed w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. ε = 0 にすると r はずっと低いまま。ε = 0.05、0.1、0.2 で r が 0.9 を超えるまでの時間を比べよう
-- 2. ←で見える距離を短くすると、場所ごとに別々のリズムの「波」ができる。ラスター図の斜めの線が波
-- 3. curveB を 0.01（ほぼ直線）にすると、押されても前後のずれが縮まらない。上に凸であることが同期の秘密
-- 4. jitter を 0.05 にして周期をばらつかせる。それでもそろう ε はいくつ？
-- 5. クリックでランタンを光らせ続けると、ホタルはランタンのリズムに引き込まれる？

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- gloss の座標（中央が原点、y は上向き）を p5.js の座標に直す
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

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

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

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

-- 種 s と番号 i から決まる 0〜1 の乱数（たくさんの粒子に一度に配るとき用）
hashRand :: Seed -> Int -> Float
hashRand s i =
  let z0 = s + fromIntegral i * 0x9E3779B97F4A7C15
      z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
      z3 = z2 `xor` (z2 `shiftR` 31)
  in fromIntegral (z3 `shiftR` 40) / 16777216
