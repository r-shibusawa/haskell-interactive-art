-- ============================================
-- 10 量子もつれとベルの不等式
-- 一重項状態 (|↑↓⟩ − |↓↑⟩)/√2 のペアを左右に飛ばし、角度 a, b の検出器でスピンを測る（結果は +1 か −1）
--   相関 E(a,b) = ⟨AB⟩ = −cos(a − b)。CHSH の S = |E(a,b) − E(a,b') + E(a',b) + E(a',b')| は
--   どんな「隠れた変数」の理論でも ≤ 2。でも量子力学では 2√2 ≈ 2.83 まで行く（実験でも確かめられた）
-- ←→ = A の角度 a / ↑↓ = B の角度 b / H = 量子 ⇔ 隠れた変数モデル / スペース = 角度を自動で一周 / C = 記録を消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, replicateM)
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

pairsPerFrame :: Int
pairsPerFrame = 60    -- ★ 1 フレームのペア数

-- 飛んでいるペア（見た目用）
data Flight = Flight { fT :: !Float, fA :: !Int, fB :: !Int }

data World = World
  { wA      :: !Float            -- ★ 検出器の角度（度）
  , wB      :: !Float
  , wHidden :: !Bool             -- 隠れた変数モデル
  , wSweep  :: !Bool
  , wSumAB  :: !Int              -- いまの角度での相関
  , wNAB    :: !Int
  , wCurveS :: !(U.Vector Int)   -- 角度の差ごとの測定値（0〜180° を 36 区間）
  , wCurveN :: !(U.Vector Int)
  , wChshS  :: ![Int]            -- CHSH の 4 つの組
  , wChshN  :: ![Int]
  , wFly    :: ![Flight]
  , wLanded :: !(Maybe (Int, Int))
  }

main :: IO ()
main = runSketch (sketch "25-10 Bell CHSH" (reset (World 0 45 False False 0 0 U.empty U.empty [] [] [] Nothing)))
  { skBack  = hsb 320 20 10
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

reset :: World -> World
reset w = w { wCurveS = U.replicate 37 0, wCurveN = U.replicate 37 0
            , wChshS = [0, 0, 0, 0], wChshN = [0, 0, 0, 0], wSumAB = 0, wNAB = 0 }

-- ★ 1 組のペアを測る：A と B の結果（±1）を返す
measure :: Bool -> Float -> Float -> IO (Int, Int)
measure hidden aDeg bDeg = do
  let ar = aDeg * pi / 180
      br = bDeg * pi / 180
  if not hidden
    then do
      -- 量子力学：A はでたらめに ±1。B が A と同じになる確率は sin²((a − b)/2)
      r1 <- randomRIO (0, 1 :: Float)
      r2 <- randomRIO (0, 1)
      let a = if r1 < 0.5 then 1 else -1
          b = if r2 < sq (sin ((ar - br) / 2)) then a else -a
      pure (a, b)
    else do
      -- 隠れた変数：ペアは生まれたときに方向 λ を持っていて、結果は λ と検出器の角度だけで決まる
      lam <- randomRIO (0, tau)
      pure (if cos (ar - lam) >= 0 then 1 else -1, if cos (br - lam) >= 0 then -1 else 1)

-- 角度の差 0〜180°
angleDiff :: Float -> Float -> Float
angleDiff a b = abs (wrapF 360 (a - b + 180) - 180)

chshSets :: [(Float, Float)]
chshSets = [(0, 45), (0, 135), (90, 45), (90, 135)]   -- a = 0°, a' = 90°, b = 45°, b' = 135°

stepW :: Env -> Float -> World -> IO World
stepW env _ w0 = do
  let w = if wSweep w0 then w0 { wB = wrapF 360 (wB w0 + 0.5) } else w0
      hidden = wHidden w
  pairs <- replicateM pairsPerFrame (measure hidden (wA w) (wB w))
  let total = sum (map (uncurry (*)) pairs)
      k = max 0 (min 36 (floor (angleDiff (wA w) (wB w) / 5 + 0.5)))
      addAt v i x = v U.// [(i, v U.! i + x)]
      firstPair = case pairs of
        (p : _) -> p
        [] -> (1, -1)
      newFly = [ Flight 0 (fst firstPair) (snd firstPair) | envFrame env `mod` 4 == 0 ]
      moved = [ f { fT = fT f + 0.04 } | f <- wFly w ] ++ newFly
      landed = case reverse (filter (\f -> fT f >= 1) moved) of
        (f : _) -> Just (fA f, fB f)
        [] -> Nothing
  sets <- mapM (\(a, b) -> foldM (\s _ -> do { (x, y) <- measure hidden a b; pure (s + x * y) }) 0 [1 .. 25 :: Int]) chshSets
  pure w { wSumAB = wSumAB w + total, wNAB = wNAB w + pairsPerFrame
         , wCurveS = addAt (wCurveS w) k total, wCurveN = addAt (wCurveN w) k pairsPerFrame
         , wChshS = zipWith (+) (wChshS w) sets, wChshN = map (+ 25) (wChshN w)
         , wFly = filter (\f -> fT f < 1.3) moved, wLanded = landed }

drawW :: Env -> World -> Picture
drawW env w =
  let width = envW env
      height = envH env
      es = zipWith (\s n -> if n > 0 then fromIntegral s / fromIntegral n else 0 :: Float) (wChshS w) (wChshN w)
      (e0, e1, e2, e3) = case es of
        [p, q, r, s] -> (p, q, r, s)
        _ -> (0, 0, 0, 0)
      bigS = abs (e0 - e1 + e2 + e3)
      -- 上：源と 2 つの検出器
      cx = width / 2
      cy = height * 0.27
      dxA = cx - width * 0.32
      dxB = cx + width * 0.32
      flightPic f | fT f >= 1 = []
                  | otherwise =
                      [ dotAt env (hsb 190 60 100) (lerpF cx dxA (fT f)) cy 9
                      , dotAt env (hsb 190 60 100) (lerpF cx dxB (fT f)) cy 9 ]
      detector x ang res nm =
        let r = ang * pi / 180
            ax = 38 * sin r
            ay = 38 * cos r
            col | res > 0 = hsb 120 70 80
                | res < 0 = hsb 0 70 80
                | otherwise = hsb 0 0 30
        in [ dotAt env col x cy 90
           , seg env 4 (hsb 55 90 100) (x - ax, cy + ay) (x + ax, cy - ay)
           , dotAt env (hsb 55 90 100) (x + ax) (cy - ay) 10
           , txtC env x (cy + 70) 16 white (nm ++ ": " ++ nf 0 ang ++ " deg  result " ++ (if res > 0 then "+1" else if res < 0 then "-1" else "")) ]
      (resA, resB) = maybe (0, 0) id (wLanded w) :: (Int, Int)
      -- 左下：E(θ) のグラフ
      gx = 70
      gy = height * 0.48
      gw = width * 0.5
      gh = height * 0.38
      ex d = gx + d / 180 * gw
      ey e = gy + gh / 2 - e * (gh / 2)
      ds = [ fromIntegral d | d <- [0, 2 .. 180 :: Int] ] :: [Float]
      measured = [ dotAt env white (ex (fromIntegral k * 5)) (ey (fromIntegral (wCurveS w U.! k) / fromIntegral n)) 7
                 | k <- [0 .. 36], let n = wCurveN w U.! k, n > 20 ]
      dNow = angleDiff (wA w) (wB w)
      -- 右下：CHSH
      px = gx + gw + 60
      py = gy + 20
      bw = width - px - 60
      good = hsb 120 70 90
      warn = hsb 30 80 90
      eNow = if wNAB w > 0 then fromIntegral (wSumAB w) / fromIntegral (wNAB w) else 0 :: Float
      eThe = if wHidden w then -1 + 2 * dNow / 180 else negate (cos (dNow * pi / 180))
  in Pictures $
       [ dotAt env (hsb 0 0 80) cx cy 26 ]
       ++ concatMap flightPic (wFly w)
       ++ detector dxA (wA w) resA "A"
       ++ detector dxB (wB w) resB "B"
       ++ [ frameBox env (hsb 0 0 35) gx gy gw gh
          , seg env 1 (hsb 0 0 35) (gx, gy + gh / 2) (gx + gw, gy + gh / 2)
          , curve env 2 (hsb 200 70 100) [ (ex d, ey (negate (cos (d * pi / 180)))) | d <- ds ]
          , curve env 2 (hsb 30 80 100) [ (ex d, ey (-1 + 2 * d / 180)) | d <- ds ] ]
       ++ measured
       ++ [ seg env 1 (hsba 55 90 100 0.6) (ex dNow, gy) (ex dNow, gy + gh)
          , txt env (gx + gw - 230) (gy + gh + 22) 14 (gray 0.7) "Angle difference a - b (0-180)"
          , txt env (gx - 60) (gy + 12) 14 (gray 0.7) "E=+1"
          , txt env (gx - 60) (gy + gh) 14 (gray 0.7) "E=-1"
          , txt env (gx + 10) (gy + 20) 14 (hsb 200 70 100) "Quantum: -cos(a - b)"
          , txt env (gx + 10) (gy + 40) 14 (hsb 30 80 100) "Hidden variable: straight line"
          , txt env (gx + 10) (gy + 60) 14 white "White dots = simulated results"
          , txt env px py 16 white "CHSH (a=0, a'=90, b=45, b'=135)"
          , txt env px (py + 32) 16 white ("E(a, b) = " ++ nf 3 e0)
          , txt env px (py + 56) 16 white ("E(a, b') = " ++ nf 3 e1)
          , txt env px (py + 80) 16 white ("E(a', b) = " ++ nf 3 e2)
          , txt env px (py + 104) 16 white ("E(a', b') = " ++ nf 3 e3)
          , txt env px (py + 150) 28 (if bigS > 2.05 then hsb 120 70 100 else hsb 30 80 100) ("S = " ++ nf 3 bigS)
          , txt env px (py + 178) 14 (gray 0.7) "Hidden-variable limit 2   Quantum limit 2.828"
          , box env (hsb 0 0 25) px (py + 195) bw 18
          , box env (if bigS > 2 then good else warn) px (py + 195) (min bigS 3 / 3 * bw) 18
          , seg env 1 white (px + 2 / 3 * bw, py + 188) (px + 2 / 3 * bw, py + 220)
          , seg env 1 (hsb 120 70 100) (px + 2.828 / 3 * bw, py + 188) (px + 2.828 / 3 * bw, py + 220)
          , txt env px (py + 246) 14 (gray 0.7) ("Now: E = " ++ nf 3 eNow ++ "  (theory " ++ nf 3 eThe ++ ")")
          , txt env 20 26 16 white ("Left/Right: a = " ++ nf 0 (wA w) ++ "   Up/Down: b = " ++ nf 0 (wB w) ++ "   H: " ++ (if wHidden w then "hidden variable" else "quantum") ++ "   Space: sweep b " ++ (if wSweep w then "ON" else "off") ++ "   C: clear")
          , txt env 20 48 14 (gray 0.6) "Same angle: always opposite (E = -1), which 'decided from the start' can explain."
          , txt env 20 68 14 (gray 0.6) "The difference shows at slanted angles: the quantum correlation is stronger."
          ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char c) Down _ _ -> case toLower c of
    'h' -> reset w { wHidden = not (wHidden w) }
    'c' -> reset w
    _ -> w
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wA = wrapF 360 (wA w + 345), wSumAB = 0, wNAB = 0 }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wA = wrapF 360 (wA w + 15), wSumAB = 0, wNAB = 0 }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wB = wrapF 360 (wB w + 15), wSumAB = 0, wNAB = 0 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wB = wrapF 360 (wB w + 345), wSumAB = 0, wNAB = 0 }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wSweep = not (wSweep w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. a = b にして E = −1 を確かめる。a − b = 90° では E = 0（無相関）
-- 2. H で隠れた変数モデルにすると、0° と 180° では量子と一致するのに 45° でずれる。S はいくつ？
-- 3. CHSH の角度を変えて（chshSets を書き換えて）、量子で S が最大になる角度を探そう
-- 4. 隠れた変数モデルの measure を自分で工夫して S > 2 にできるか挑戦（ベルの定理：局所的なら無理）

-- ==================================================
-- ここから下は「道具」
-- ==================================================
sq :: Float -> Float
sq x = x * x

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = pt env x y in label gx gy size c s

-- 中央揃えの文字（y はベースライン）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = txt env (x - fromIntegral (length s) * size * 0.36) y size c s

seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c (x1, y1) (x2, y2) = Color c (thickLine w (pt env x1 y1) (pt env x2 y2))

curve :: Env -> Float -> Color -> [Point] -> Picture
curve env w c ps = Color c (thickPath w (map (uncurry (pt env)) ps))

box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

frameBox :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
frameBox env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rectLine gx gy w h)

dotAt :: Env -> Color -> Float -> Float -> Float -> Picture
dotAt env c x y d = Color c (at (pt env x y) (disc (d / 2)))
