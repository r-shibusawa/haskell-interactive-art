-- ============================================
-- 00 波と粒子：二重スリット
-- 電子は 1 個ずつ「点」としてスクリーンに当たる。でも当たる場所の確率は |ψ|²（2 つのスリットを通った波の干渉）
--   点がたまると縞模様（干渉縞）が浮かび上がる。どちらのスリットを通ったか見張る（W）と縞は消える
-- 1 = 両方開ける / 2 = 上だけ / 3 = 下だけ / W = 通り道を見張る検出器 / ↑↓ = 1 フレームの粒子数 / ←→ = スリットの間隔 / T = 理論の曲線 / C = 消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)

lambda0 :: Float
lambda0 = 7          -- ★ 波長（px）。大きくすると縞の間隔が広がる

slitW :: Float
slitW = 12           -- ★ スリットの幅 a（px）

binH :: Float
binH = 4             -- ヒストグラムの 1 段の高さ（px）

data Slit = Both | TopOnly | BottomOnly deriving (Eq, Show)

-- 飛んでいる途中の粒子（見た目用）
data Fly = Fly { fT :: !Float, fS :: !Float, fY :: !Float }

data World = World
  { wGap    :: !Float           -- ★ スリットの間隔 d（px）
  , wRate   :: !Int             -- ★ 1 フレームに飛ばす粒子の数
  , wOpen   :: !Slit
  , wWatch  :: !Bool            -- 通り道の検出器
  , wTheory :: !Bool
  , wBins   :: !(U.Vector Int)  -- ヒストグラム
  , wHits   :: !(U.Vector Float) -- 当たった点を描きためる画素（0〜1）
  , wTotal  :: !Int
  , wFlying :: ![Fly]
  }

-- 画面の配置（p5 と同じく左上が原点、y は下向き）
data Lay = Lay { srcX :: !Float, slitX :: !Float, scrX :: !Float, cy :: !Float, ymax :: !Float }

layOf :: Env -> Lay
layOf env = Lay (envW env * 0.08) (envW env * 0.3) (envW env * 0.68) (envH env * 0.55) (envH env * 0.38)

-- 点をためる画像の大きさ（スクリーンの幅 ±14 px と高さ）
hitsW :: Int
hitsW = 32

hitsH :: Lay -> Int
hitsH l = ceiling (2 * ymax l) + 6

main :: IO ()
main = runSketch (sketch "25-00 Double Slit" world0)
  { skBack  = bg
  , skStart = \env -> pure (reset (layOf env) world0)
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

world0 :: World
world0 = World 60 6 Both False True U.empty U.empty 0 []

-- layout() に相当：ためた点をすべて消す
reset :: Lay -> World -> World
reset l w = w
  { wBins = U.replicate (ceiling (2 * ymax l / binH)) 0
  , wHits = U.replicate (hitsW * hitsH l) 0
  , wTotal = 0
  , wFlying = []
  }

-- ★ スクリーン上の位置 y（中心から px）での確率 |ψ|²（フラウンホーファー近似）
--   1 本のスリットの波：ψ1 ∝ sinc(π a (y - s) / (λ L)) · e^{i k r1}
--   両方開いていれば ψ = ψ上 + ψ下 → |ψ|² に cos²（干渉）が出る
prob :: Lay -> World -> Float -> Float
prob l w y =
  let bigL = scrX l - slitX l
      gap = wGap w
      single s = let u = pi * slitW * (y - s) / (lambda0 * bigL)
                 in if abs u < 1e-6 then 1 else sin u / u
      phase = pi * gap * y / (lambda0 * bigL)    -- 2 つの道の位相差の半分
      a1 = single (-gap / 2)
      a2 = single (gap / 2)
      re = (a1 + a2) * cos phase                 -- 振幅を足してから 2 乗：|a1 e^{-iφ} + a2 e^{iφ}|² / 4
      im = (a2 - a1) * sin phase
  in case wOpen w of
       TopOnly -> a1 * a1
       BottomOnly -> a2 * a2
       Both | wWatch w -> 0.5 * (a1 * a1 + a2 * a2)   -- 見張ると干渉項が消える（確率を足すだけ）
            | otherwise -> (re * re + im * im) / 4

-- 確率 prob(y) に従って y を 1 つ選ぶ（棄却法：でたらめに選んで、確率に応じて採用）
sampleY :: Lay -> World -> IO Float
sampleY l w = go (300 :: Int)
  where
    go 0 = pure 0
    go n = do
      y <- randomRIO (-ymax l, ymax l)
      r <- randomRIO (0, 1)
      if r < prob l w y then pure y else go (n - 1)

-- 粒子を 1 個飛ばす：(y, スクリーン上の横ずれ, 新しく見せる粒子)
shootOne :: Lay -> World -> IO (Float, Float, Maybe Fly)
shootOne l w = do
  y <- sampleY l w
  rx <- randomRIO (-14, 14)
  r <- randomRIO (0, 1 :: Float)
  r2 <- randomRIO (0, 1 :: Float)
  let s = case wOpen w of
        TopOnly -> -1
        BottomOnly -> 1
        Both -> if r2 < 0.5 then -1 else 1
  pure (y, rx, if r < 0.3 then Just (Fly 0 s y) else Nothing)

stepW :: Env -> Float -> World -> IO World
stepW env _ w = do
  let l = layOf env
  shots <- replicateM (wRate w) (shootOne l w)
  let nb = U.length (wBins w)
      binsAdd = [ (bi, 1) | (y, _, _) <- shots, let bi = floor ((y + ymax l) / binH), bi >= 0, bi < nb ]
      hitsAdd = concat [ hitMarks l y rx | (y, rx, _) <- shots ]
      moved = [ f { fT = fT f + 0.03 } | f <- wFlying w, fT f + 0.03 < 1 ]
      flying = foldl' (\fs (_, _, m) -> case m of
                                           Just f | length fs < 40 -> f : fs
                                           _ -> fs) moved shots
  pure w { wBins = U.accum (+) (wBins w) binsAdd
         , wHits = U.accum (\c a -> 1 - (1 - c) * (1 - a)) (wHits w) hitsAdd
         , wTotal = wTotal w + wRate w
         , wFlying = flying }

-- 点 1 個（直径 2.2 px、不透明度 70%）が画素に与える濃さ
hitMarks :: Lay -> Float -> Float -> [(Int, Float)]
hitMarks l y rx =
  let ix = round rx + hitsW `div` 2
      iy = round (y + ymax l) + 3
      at' dx dy a = ((iy + dy) * hitsW + ix + dx, a)
  in [at' 0 0 0.7, at' 1 0 0.2, at' (-1) 0 0.2, at' 0 1 0.2, at' 0 (-1) 0.2]

drawW :: Env -> World -> Picture
drawW env w =
  let l = layOf env
      gap = wGap w
      gTop = cy l - gap / 2
      gBot = cy l + gap / 2
      wallT = cy l - ymax l - 20
      hx = scrX l + 40
      hw = envW env - hx - 30
      mx = fromIntegral (max 1 (U.maximum (U.cons 1 (wBins w)))) :: Float
      -- 描きためた点
      hh = hitsH l
      hits = pixelImage hitsW hh 1 (\x y -> shade (wHits w `U.unsafeIndex` (y * hitsW + x)))
      hitsPic = at (pt env (scrX l) (cy l + fromIntegral hh / 2 - 3 - ymax l)) hits
      ys = takeWhile (<= ymax l) (iterate (+ 2) (-ymax l))
      pm = max 1e-6 (foldl' (\m y -> max m (prob l w y)) 0 ys)
      theory = curve env 2 (hsb 190 80 100) [ (hx + prob l w y / pm * hw, cy l + y) | y <- ys ]
      flyPic f =
        let sy = cy l + fS f * gap / 2
            (x, y) | fT f < 0.5 = (lerpF (srcX l) (slitX l) (fT f * 2), lerpF (cy l) sy (fT f * 2))
                   | otherwise = (lerpF (slitX l) (scrX l) ((fT f - 0.5) * 2), lerpF sy (cy l + fY f) ((fT f - 0.5) * 2))
        in dotAt env (hsb (if wWatch w then 120 else 190) 70 100) x y 6
  in Pictures $
       [ hitsPic
       -- 源・壁・スクリーン
       , dotAt env (hsb 0 0 70) (srcX l) (cy l) 18
       , box env (hsb 0 0 45) (slitX l - 5) wallT 10 (gTop - slitW / 2 - wallT)
       , box env (hsb 0 0 45) (slitX l - 5) (gTop + slitW / 2) 10 (gBot - gTop - slitW)
       , box env (hsb 0 0 45) (slitX l - 5) (gBot + slitW / 2) 10 (cy l + ymax l + 20 - gBot - slitW / 2)
       ]
       ++ [ box env (hsb 0 70 70) (slitX l - 5) (gTop - slitW / 2) 10 slitW | wOpen w == BottomOnly ]   -- ふさいだスリット
       ++ [ box env (hsb 0 70 70) (slitX l - 5) (gBot - slitW / 2) 10 slitW | wOpen w == TopOnly ]
       ++ [ seg env 2 (hsb 0 0 60) (scrX l, cy l - ymax l) (scrX l, cy l + ymax l) ]
       ++ (if wWatch w then [ dotAt env (hsb 120 80 90) (slitX l + 22) gTop 14, dotAt env (hsb 120 80 90) (slitX l + 22) gBot 14 ] else [])
       ++ map flyPic (wFlying w)
       -- ヒストグラムと理論曲線
       ++ [ box env (hsba 55 70 95 0.8) hx (cy l - ymax l + fromIntegral i * binH) (fromIntegral b / mx * hw) (binH - 1)
          | (i, b) <- zip [0 :: Int ..] (U.toList (wBins w)), b > 0 ]
       ++ [ theory | wTheory w ]
       ++ [ txt env 20 26 16 white ("1: both  2: top  3: bottom (" ++ openName (wOpen w) ++ ")   W: detector " ++ (if wWatch w then "ON" else "off") ++ "   Up/Down: rate " ++ show (wRate w))
          , txt env 20 48 16 white ("Left/Right: gap d = " ++ show (round gap :: Int) ++ "   T: theory   C: clear   particles " ++ show (wTotal w))
          , txt env 20 70 14 (gray 0.6) "Each dot lands at random, yet thousands make fringes: the probability is a wave |psi|^2."
          , txt env 20 90 14 (gray 0.6) "Watching the path adds probabilities instead of amplitudes, so the fringes vanish."
          , txt env hx (cy l - ymax l - 12) 14 (hsb 190 80 100) "Blue line = |psi|^2 (theory)"
          ]

openName :: Slit -> String
openName Both = "both"
openName TopOnly = "top"
openName BottomOnly = "bottom"

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventResize _ -> changed w
  EventKey (Char c) Down _ _ -> case toLower c of
    '1' -> changed w { wOpen = Both }
    '2' -> changed w { wOpen = TopOnly }
    '3' -> changed w { wOpen = BottomOnly }
    'w' -> changed w { wWatch = not (wWatch w) }
    't' -> w { wTheory = not (wTheory w) }
    'c' -> changed w
    _ -> w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wRate = min 200 (wRate w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wRate = max 1 (wRate w `div` 2) }
  EventKey (SpecialKey KeyRight) Down _ _ -> changed w { wGap = min 160 (wGap w + 10) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> changed w { wGap = max 20 (wGap w - 10) }
  _ -> w
  where changed = reset (layOf env)

-- ---------- やってみよう ----------
-- 1. 粒子数を 1 にして、最初の 20 個が縞に見えるか観察。いつごろ縞だとわかる？
-- 2. 縞の間隔は λL/d。lambda0 を 2 倍、wGap を 2 倍にして確かめよう
-- 3. 2（上だけ）の模様と 3（下だけ）の模様を足したものと、1（両方）は同じ？ → 違う！
-- 4. W の検出器を「半分の確率でだけ見張る」ように prob を書き換えると、縞はどのくらい薄くなる？

-- ==================================================
-- ここから下は「道具」
-- ==================================================
bg :: Color
bg = hsb 230 30 10

-- 描きためた点の濃さ c（0〜1）を色にする（背景 → 黄色）
shade :: Float -> (Word8, Word8, Word8)
shade c =
  let (r0, g0, b0, _) = rgbaOfColor bg
      (r1, g1, b1, _) = rgbaOfColor (hsb 55 60 100)
      q a b = fromIntegral (round (255 * lerpF a b c) :: Int)
  in (q r0 r1, q g0 g1, q b0 b1)

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = pt env x y in label gx gy size c s

seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c (x1, y1) (x2, y2) = Color c (thickLine w (pt env x1 y1) (pt env x2 y2))

curve :: Env -> Float -> Color -> [Point] -> Picture
curve env w c ps = Color c (thickPath w (map (uncurry (pt env)) ps))

-- p5 の rect(x, y, w, h)（x, y は左上）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- p5 の circle(x, y, d)（d は直径）
dotAt :: Env -> Color -> Float -> Float -> Float -> Picture
dotAt env c x y d = Color c (at (pt env x y) (disc (d / 2)))
