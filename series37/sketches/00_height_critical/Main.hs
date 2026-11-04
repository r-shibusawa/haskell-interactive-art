-- ============================================
-- 00 高さ関数の臨界点 ─ 水位を上げると形が変わる
-- ノイズでつくった地形（高さ関数 h）の極小・鞍点・極大を自動で見つけて色分けする
--   マウス左右 = 水位 t。水位より低い所（劣位集合 {h ≤ t}）が水色に染まる
--   水面の形（塊の数 b0・島の数 b1）が変わるのは、水位が臨界点を通る瞬間だけ ─ これがモース理論の出発点
-- マウス左右 = 水位 / スペース = 自動で上げる / N = 新しい地形 / C = 臨界点の表示 / ↑↓ = 地形の細かさ
-- ============================================
module Main (main) where

import Art
import Art
import qualified Data.IntSet as IS
import Data.List (foldl', sortOn)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)
import Numeric (showFFloat)

cols, rows, nPix :: Int
cols = 128          -- ★ 地形の解像度
rows = 80
nPix = (cols + 1) * (rows + 1)

ox, oy, gW, gH :: Float
ox = 30
oy = 100
gW = scrW - 60
gH = scrH - 180

data Crit = Crit { crI, crJ :: !Int, crType :: !Int, crH :: !Float }   -- crType：0 極小、1 鞍点、2 極大

data World = World
  { wH       :: !(U.Vector Float)                   -- 高さ（0〜1）
  , wColor   :: !(U.Vector (Word8, Word8, Word8))   -- 等高線の帯の色
  , wCrit    :: [Crit]
  , wLevel   :: !Float
  , wAuto    :: !Bool
  , wShow    :: !Bool
  , wFreq    :: !Float      -- ★ ノイズの細かさ（↑↓）
  , wSeed    :: !Float
  , wB0, wB1 :: !Int
  , wCounted :: !Float      -- b0, b1 を数えたときの水位
  , wLast    :: !Int        -- 直前に通りすぎた臨界点の数（-1 = まだない）
  , wPassed  :: !Int
  , wFlash   :: !Float
  , wMoved   :: !Bool       -- マウスが動いたか
  }

world0 :: World
world0 = makeTerrain World
  { wH = U.empty, wColor = U.empty, wCrit = [], wLevel = 0.3, wAuto = False, wShow = True, wFreq = 2.2
  , wSeed = 1, wB0 = 0, wB1 = 0, wCounted = -1, wLast = -1, wPassed = 0, wFlash = 0, wMoved = False }

main :: IO ()
main = runSketch (sketch "37-00 Height Critical" world0)
  { skBack  = hsb 225 30 8
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- ★ 地形をつくる：ノイズを足し合わせて 0〜1 に正規化
makeTerrain :: World -> World
makeTerrain w = w { wH = hs, wColor = U.map bandColor hs, wCrit = findCritical hs, wCounted = -1, wLast = -1 }
  where
    s = wSeed w * 17.3
    fq = wFreq w
    n = pnoise 3 0.4 s
    raw = U.generate nPix $ \k ->
      let (j, i) = k `divMod` (cols + 1)
          u = fromIntegral i / fromIntegral cols
          v = fromIntegral j / fromIntegral rows
      in n (u * fq) (v * fq * 0.7) + 0.2 * n (u * fq * 3 + 9) (v * fq * 2 + 9)
    lo = U.minimum raw
    hi = U.maximum raw
    hs = U.map (\v -> (v - lo) / max 1e-6 (hi - lo)) raw

-- 等高線の帯
bandColor :: Float -> (Word8, Word8, Word8)
bandColor v = bytes (hsb (100 - band * 70) 40 (25 + band * 60))
  where band = fromIntegral (floor (v * 12) :: Int) / 12

-- ★ 臨界点を見つける：8 近傍とくらべて、全部高い = 極小、全部低い = 極大、まわりの上下が 4 回入れかわる = 鞍点
findCritical :: U.Vector Float -> [Crit]
findCritical hs = sortOn crH
  [ Crit i j t v
  | j <- [1 .. rows - 1], i <- [1 .. cols - 1]
  , let v = hAt i j
        signs = [hAt (i + a) (j + b) > v | (a, b) <- around]
        lower = length (filter not signs)
        changes = length (filter id (zipWith (/=) signs (drop 1 signs ++ take 1 signs)))
  , t <- [0 | lower == 0] ++ [2 | lower == 8] ++ [1 | lower /= 0, lower /= 8, changes >= 4] ]
  where
    hAt i j = hs U.! (i + j * (cols + 1))
    around = [(1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]

-- 劣位集合 {h ≤ t} の塊の数 b0 と島（穴）の数 b1 を数える（Union-Find）
countWater :: U.Vector Float -> Float -> (Int, Int)
countWater hs t = (IS.size water, IS.size (IS.difference land edgeLand))
  where
    wet k = hs U.! k <= t
    links = concat
      [ [(k, k + 1) | i < cols, wet (k + 1) == w] ++ [(k, k + cols + 1) | j < rows, wet (k + cols + 1) == w]
        ++ (if not w && i < cols && j < rows                      -- 陸は斜めもつなぐ（水を 4 近傍、陸を 8 近傍で見る）
              then [(k, k + cols + 2) | not (wet (k + cols + 2))] ++ [(k, k + cols) | not (wet (k + cols))]
              else [])
      | j <- [0 .. rows], i <- [0 .. cols], let k = i + j * (cols + 1), let w = wet k ]
    root = components nPix links
    keys = [0 .. nPix - 1]
    water = IS.fromList [root U.! k | k <- keys, wet k]
    land = IS.fromList [root U.! k | k <- keys, not (wet k)]
    onEdge k = let (j, i) = k `divMod` (cols + 1) in i == 0 || j == 0 || i == cols || j == rows
    edgeLand = IS.fromList [root U.! k | k <- keys, not (wet k), onEdge k]   -- 水に囲まれた陸 = 島 = 水の穴

stepWorld :: Env -> World -> World
stepWorld env w = w1 { wLast = passed, wPassed = passed, wFlash = flash * 0.9 }
  where
    (mx, my) = mouseP env
    level | wAuto w = wrapF 1.02 (wLevel w + 0.0015)
          | wMoved w && my > oy = clampF 0 1 ((mx - ox) / gW)
          | otherwise = wLevel w
    w0 = w { wLevel = level }
    w1 | level /= wCounted w =
           let (a, b) = countWater (wH w) level in w0 { wB0 = a, wB1 = b, wCounted = level }
       | otherwise = w0
    -- 通りすぎた臨界点を調べる（直前との差）
    passed = length (takeWhile ((<= level) . crH) (wCrit w))
    flash = if wLast w >= 0 && passed /= wLast w then 1 else wFlash w

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $ Pictures $
  [ stretch ox oy gW gH (cols + 1) (rows + 1) pixel ]
  ++ (if wShow w then map critMark (wCrit w) else [])
  ++ slider
  ++ hud
  where
    level = wLevel w
    pixel x y =
      let k = x + y * (cols + 1)
          (r, g, b) = wColor w U.! k
          mix c a = fromIntegral ((fromIntegral c * 65 + a * 190) `div` 255 :: Int)
      in if wH w U.! k <= level then (mix r 40, mix g 140, mix b 255) else (r, g, b)
    sx = gW / fromIntegral cols
    sy = gH / fromIntegral rows
    critMark c =
      let p@(x, y) = (ox + fromIntegral (crI c) * sx, oy + fromIntegral (crJ c) * sy)
          a = if crH c <= level then 0.5 else 1
      in case crType c of
           0 -> dotC (hsba 272 70 100 a) p 14
           2 -> dotC (hsba 12 85 100 a) p 14
           _ -> let col = hsba 85 90 100 a
                in Pictures [seg 3 col (x - 7, y - 7) (x + 7, y + 7), seg 3 col (x - 7, y + 7) (x + 7, y - 7)]
    -- 水位のスライダー
    slider =
      [ box (hsba 0 0 100 0.6) ox (scrH - 30) gW 6
      , box (hsb 205 80 100) ox (scrH - 30) (gW * level) 6
      , dotC white (ox + gW * level, scrH - 27) 14 ]
    count t = length (filter ((== t) . crType) (wCrit w))
    hud =
      [ txt ox 40 20 white ("Level t = " ++ fmt 3 level ++ "  water b0 = " ++ show (wB0 w) ++ "  islands b1 = " ++ show (wB1 w)
          ++ "   passed " ++ show (wPassed w) ++ " / " ++ show (length (wCrit w)))
      , txt ox 66 15 (gray 0.65) ("o min " ++ show (count 0) ++ " (purple)  x saddle " ++ show (count 1) ++ " (lime)  o max " ++ show (count 2)
          ++ " (coral)   Mouse: level  Space: auto " ++ onOff (wAuto w) ++ "  N: new  C: marks  Up/Dn: " ++ fmt 1 (wFreq w))
      , txt ox (scrH - 50) 14 (gray 0.6) "A min starts a blob; a saddle joins blobs or makes an island; a max sinks an island. Elsewhere nothing changes."
      ]
      ++ [ txt ox 92 18 (hsba 48 90 100 (min 1 (wFlash w))) "Critical point passed! The shape changed." | wFlash w > 0.05 ]

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventMotion _ -> w { wMoved = True }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAuto = not (wAuto w) }
  EventKey (Char c) Down _ _
    | c `elem` "nN" -> makeTerrain w { wSeed = wSeed w + 1 }
    | c `elem` "cC" -> w { wShow = not (wShow w) }
  EventKey (SpecialKey KeyUp) Down _ _ -> makeTerrain w { wFreq = min 6 (wFreq w + 0.4) }
  EventKey (SpecialKey KeyDown) Down _ _ -> makeTerrain w { wFreq = max 1 (wFreq w - 0.4) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 水位をゆっくり上げて、b0 が増える瞬間に紫の点（極小）が沈んでいるのを確かめよう
-- 2. 鞍点（×）を通るとき、b0 が減る場合と b1 が増える場合がある。どちらになるかは何で決まる？
-- 3. wFreq を大きくして臨界点を増やす。極小 − 鞍点 + 極大 はいつもいくつ？（モースの関係式）
-- 4. 8 近傍の判定だと「壊れた鞍点」が出ることがある。changes >= 6 の点（猿の鞍点）を探してみよう

-- ---------- 離散モース理論の道具 ----------

-- p5.js と同じ座標（左上が原点、y は下向き）で描くための小道具。screen で全体を画面に合わせる
scrW, scrH :: Float
scrW = 1280
scrH = 800

screen :: Env -> Picture -> Picture
screen env = Translate (-envW env / 2) (envH env / 2)

toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

mouseP :: Env -> Point
mouseP env = toP5 env (envMouse env)

fl :: Point -> Point
fl (x, y) = (x, -y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label x (-y)

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (thickLine w (fl a) (fl b))

-- 太さ w の折れ線
polyL :: Float -> Color -> [Point] -> Picture
polyL w c ps = Color c (thickPath w (map fl ps))

-- 左上 (x, y)、幅 w、高さ h の長方形
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (rect (x + w / 2) (-(y + h / 2)) w h)

-- 直径 d の円（p5.js の circle）
dotC :: Color -> Point -> Float -> Picture
dotC c p d = Color c (at (fl p) (disc (d / 2)))

-- 直径 d、線の太さ w の輪
ringC :: Color -> Point -> Float -> Float -> Picture
ringC c p d w = Color c (at (fl p) (ring (d / 2) w))

fmt :: Int -> Float -> String
fmt n x = showFFloat (Just n) x ""

fi :: Int -> Float
fi = fromIntegral

-- p5.js の noise に近いノイズ。oct = 重ねる回数、fall = 減衰（noiseDetail）、s = ずらす量（noiseSeed の代わり）
pnoise :: Int -> Float -> Float -> Float -> Float -> Float
pnoise oct fall s x y = go oct 1 fall 0
  where
    go :: Int -> Float -> Float -> Float -> Float
    go 0 _ _ acc = acc
    go k f a acc = go (k - 1) (f * 2) (a * fall) (acc + a * clampF 0 1 (0.5 + 2.2 * (noise3 (x * f + s) (y * f + s * 0.61) (0.5 + s * 0.37) - 0.5)))   -- 2.2 倍：p5.js の noise と同じくらいの幅にする

-- noiseDetail を変えていないときの noise
noiseP :: Float -> Float -> Float -> Float
noiseP = pnoise 4 0.5

newSeed :: IO Float
newSeed = randomRIO (0, 200)

-- Union-Find：n 個の要素を組 (a, b) で結び、各要素の根を返す
components :: Int -> [(Int, Int)] -> U.Vector Int
components n links = U.create $ do
  p <- U.thaw (U.enumFromN 0 n)
  let find a = do
        q <- MU.read p a
        if q == a then pure a else do
          r <- MU.read p q
          MU.write p a r
          find r
      unite (a, b) = do
        x <- find a
        y <- find b
        if x /= y then MU.write p x y else pure ()
  mapM_ unite links
  mapM_ (\i -> find i >>= MU.write p i) [0 .. n - 1]
  pure p

-- ピクセル画像を左上 (x, y)、幅 ww、高さ hh に引きのばして置く
stretch :: Float -> Float -> Float -> Float -> Int -> Int -> (Int -> Int -> (Word8, Word8, Word8)) -> Picture
stretch x y ww hh w h f =
  Translate (x + ww / 2) (-(y + hh / 2)) (Scale (ww / fromIntegral w) (hh / fromIntegral h) (pixelImage w h 1 f))

bytes :: Color -> (Word8, Word8, Word8)
bytes c = let (r, g, b, _) = rgbaOfColor c in (q r, q g, q b)
  where q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))
