-- ============================================
-- 11 ページランクとペロン＝フロベニウス
-- ページランク x* は「グーグル行列 G をかけても変わらないベクトル」= G の不動点（固有値 1 の固有ベクトル）
--   G = α·P + (1 − α)/n（P：リンクをたどる、残り：ランダムにジャンプ）。G は 1-ノルムで α 倍に縮める → 02 のバナッハの定理
--   どんな初期値からでも G をかけ続ける（べき乗法）と x* に着き、誤差は毎回 α 倍以下
-- クリック → 別のページをクリック = リンクをつける・外す / ←→ = α（たどる割合）/ スペース = 止める・動かす / N = 1 回 / U = 1 ページに集めて再出発 / R = ランダムなリンク
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import Data.Char (chr, ord, toLower)
import Data.List (sortOn)
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

n :: Int
n = 8                        -- ★ ページの数

data World = World
  { wDamp :: !Double         -- ★ リンクをたどる確率（グーグルの元の論文は 0.85）
  , wL    :: !(U.Vector Int) -- L[i*n+j] = 1：i → j のリンク
  , wG    :: !(U.Vector Double)  -- グーグル行列（G[j*n+i]、列ごとに和 1）
  , wX    :: !(U.Vector Double)
  , wXs   :: !(U.Vector Double)
  , wHist :: ![Double]       -- 誤差の記録（古い順、60 個まで。1e-16 まで見るので Double）
  , wRun  :: !Bool
  , wSel  :: !Int
  }

main :: IO ()
main = do
  w0 <- randomLinks (World 0.85 U.empty U.empty U.empty U.empty [] True (-1))
  runSketch (sketch "31-11 PageRank and Perron" w0)
    { skBack  = hsb 230 30 11
    , skStep  = stepWorld
    , skDraw  = drawWorld
    , skEvent = onEvent
    }

randomLinks :: World -> IO World
randomLinks w = do
  ls <- mapM (\i -> do
                k <- randomRIO (1, 3 :: Int)
                js <- replicateM k (randomRIO (0, n - 1))
                pure [ (i * n + j, 1) | j <- js, j /= i ]) [0 .. n - 1]
  pure (rebuild True w { wL = U.replicate (n * n) 0 U.// concat ls })

-- ★ グーグル行列：リンク先へ等分（行き先のないページは全ページへ等分）＋ ランダムジャンプ
rebuild :: Bool -> World -> World
rebuild uniformStart w =
  let d = wDamp w
      nn = fromIntegral n :: Double
      outOf i = U.sum (U.slice (i * n) n (wL w))
      g = U.generate (n * n) $ \k ->
        let (j, i) = k `divMod` n       -- G[j][i]：i → j
            out = outOf i
            pji = if out == 0 then 1 / nn else fromIntegral (wL w U.! (i * n + j)) / fromIntegral out
        in d * pji + (1 - d) / nn
      uni = U.replicate n (1 / nn)
      -- 不動点 x*：十分たくさんかけて求めておく
      xs = nTimes 3000 (mul g) uni
      x = if uniformStart then uni else wX w
  in w { wG = g, wXs = xs, wX = x, wHist = [err xs x] }

mul :: U.Vector Double -> U.Vector Double -> U.Vector Double
mul g v = U.generate n $ \j -> sumRow j 0 0
  where
    sumRow :: Int -> Int -> Double -> Double
    sumRow j i !acc
      | i >= n = acc
      | otherwise = sumRow j (i + 1) (acc + g U.! (j * n + i) * v U.! i)

err :: U.Vector Double -> U.Vector Double -> Double
err xs v = U.sum (U.zipWith (\a b -> abs (a - b)) v xs)

stepOnce :: World -> World
stepOnce w =
  let x = mul (wG w) (wX w)
      h = wHist w ++ [err (wXs w) x]
  in w { wX = x, wHist = drop (length h - 60) h }

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w
  | wRun w && envFrame env `mod` 15 == 0 = pure (stepOnce w)
  | otherwise = pure w

-- ページの位置
posOf :: Env -> Int -> Point
posOf env i =
  let r = min (envW env * 0.18) (envH env * 0.3)
      cx = 60 + r + 30
      cy = 130 + r
      a = negate pi / 2 + fromIntegral i / fromIntegral n * tau
  in (cx + r * cos a, cy + r * sin a)

letter :: Int -> String
letter i = [chr (ord 'A' + i)]

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env $
  links
  ++ concatMap page [0 .. n - 1]
  -- 右上：グーグル行列のヒートマップ
  ++ [ txt hx (hy - 12) 14 (hsb 0 0 70) "Google matrix G"
     , txt hx (hy + fn * cs + 18) 14 (hsb 0 0 70) "column = from, row = to"
     ]
  ++ [ fillBox (hsb 45 80 (15 + 85 * min 1 (fl (wG w U.! (j * n + i)) * 1.6))) (hx + fromIntegral i * cs) (hy + fromIntegral j * cs) (cs - 1) (cs - 1)
     | j <- [0 .. n - 1], i <- [0 .. n - 1] ]
  -- 棒グラフ：x_k と x*
  ++ [ txt bx (hy - 12) 14 (hsb 0 0 70) "x_k (bars), x* (yellow)" ]
  ++ concat [ let x' = bx + fromIntegral i * (bw + 6)
                  hb = fl (wX w U.! i) * bh * 2.5
                  hs = fl (wXs w U.! i) * bh * 2.5
              in [ fillBox (hsb (200 + fromIntegral i * 18) 60 95) x' (hy + bh - hb) bw hb
                 , ln 2 (hsb 45 90 100) (x' - 2, hy + bh - hs) (x' + bw + 2, hy + bh - hs)
                 , txt (x' + bw / 2 - 5) (hy + bh + 18) 14 (hsb 0 0 80) (letter i) ]
            | i <- [0 .. n - 1] ]
  -- 下：誤差の対数グラフ
  ++ [ box (hsb 0 0 35) ex0 ey0 ew eh
     , Color (hsb 190 70 100) (dashed 6 6 1 [ (ex0 + fromIntegral k / 59 * ew, gyv (h0 * wDamp w ^ k)) | k <- [0 .. 59 :: Int] ])
     ]
  ++ [ spot (hsb 320 70 100) (ex0 + fromIntegral k / 59 * ew, gyv e) 6 | (k, e) <- zip [0 :: Int ..] (wHist w) ]
  ++ [ txt ex0 (ey0 - 10) 14 (hsb 0 0 70) "Error |x_k - x*|_1 (log scale). Dashed = alpha^k * first error (Banach)" ]
  ++ [ txt (ex0 - 48) (gyv (10 ** e) + 5) 14 (hsb 0 0 70) ("1e" ++ show (round e :: Int)) | e <- [0, -5, -10, -15 :: Double] ]
  -- 順位
  ++ [ txt 20 (hh - 50) 15 white ("PageRank order: " ++ unwords [ letter i ++ "(" ++ nf 3 (wXs w U.! i) ++ ")" | i <- order ])
     , txt 20 (hh - 22) 14 (hsb 0 0 60) "All entries of G > 0, so Perron-Frobenius: eigenvalue 1 has one positive eigenvector. |lambda_2| <= alpha sets the speed."
     , txt 20 34 15 white ("alpha = " ++ nf 2 (wDamp w) ++ " (Left/Right)   Space: " ++ (if wRun w then "pause" else "run") ++ "   N: one step   U: all on one page   R: random   click -> click: link")
     , txt 20 62 14 (hsb 0 0 60) "x_(k+1) = G x_k (power method): G is the fixed-point map itself. alpha near 1 follows links closely but converges slowly (slope = log10 alpha)."
     ]
  where
    hh = envH env
    fn = fromIntegral n :: Float
    fl :: Double -> Float
    fl = realToFrac
    hx = envW env * 0.47
    hy = 120
    cs = min 30 (envW env * 0.22 / fn)
    bx = hx + fn * cs + 60
    bw = min 26 ((envW env - bx - 30) / fn - 6)
    bh = fn * cs
    ex0 = hx
    ey0 = hy + bh + 60
    ew = envW env - hx - 40
    eh = hh - ey0 - 110
    gyv :: Double -> Float
    gyv e = remap (clampF (-16) 1 (fl (logBase 10 (max e 1e-16)))) 1 (-16) ey0 (ey0 + eh)
    h0 = case wHist w of { (e : _) -> e; [] -> 1 }
    diameter v = 14 + 120 * sqrt (fl v)
    order = sortOn (\i -> negate (wXs w U.! i)) [0 .. n - 1]
    -- リンク（矢印）
    links = concat
      [ [ ln 1.5 (hsba 0 0 70 0.6) a (ex, ey)
        , Color (hsba 0 0 70 0.8) (Polygon (map (rotP ang (ex, ey)) [(0, 0), (-12, -5), (-12, 5)])) ]
      | i <- [0 .. n - 1], j <- [0 .. n - 1], wL w U.! (i * n + j) == 1
      , let a@(ax, ay) = posOf env i
      , let (bx', by') = posOf env j
      , let ang = atan2 (by' - ay) (bx' - ax)
      , let rb = diameter (wX w U.! j)
      , let ex = bx' - cos ang * rb / 2
      , let ey = by' - sin ang * rb / 2 ]
    rotP ang (cx, cy) (dx, dy) = (cx + dx * cos ang - dy * sin ang, cy + dx * sin ang + dy * cos ang)
    -- ページ：大きさ = 今の x、輪 = 不動点 x*
    page i =
      let p@(px, py) = posOf env i
          d = diameter (wX w U.! i)
          ds = diameter (wXs w U.! i)
      in [ spot (hsba (200 + fromIntegral i * 18) 60 95 0.85) p d
         , hoop 2 (hsb 45 90 100) p ds ]
         ++ [ hoop 3 white p (d + 10) | i == wSel w ]
         ++ [ txt (px - 6) (py + 6) 16 black (letter i) ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseP env
        hits = [ i | i <- [0 .. n - 1], dist2 m (posOf env i) < 30 ]
    in pure $ case reverse hits of
         [] -> w { wSel = -1 }
         (hit : _)
           | wSel w < 0 -> w { wSel = hit }
           | hit /= wSel w ->
               let k = wSel w * n + hit
               in (rebuild False w { wL = wL w U.// [(k, 1 - wL w U.! k)] }) { wSel = -1 }
           | otherwise -> w { wSel = -1 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure (rebuild False w { wDamp = max 0 (roundCent (wDamp w - 0.05)) })
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (rebuild False w { wDamp = min 0.99 (roundCent (wDamp w + 0.05)) })
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wRun = not (wRun w) }
  EventKey (Char c) Down _ _
    | toLower c == 'n' -> pure (stepOnce w)
    | toLower c == 'u' -> do
        i <- randomRIO (0, n - 1)
        let x = U.generate n (\j -> if j == i then 1 else 0)
        pure w { wX = x, wHist = [err (wXs w) x] }
    | toLower c == 'r' -> randomLinks w
  _ -> pure w
  where roundCent v = fromIntegral (round (v * 100) :: Int) / 100

-- ---------- やってみよう ----------
-- 1. α = 0.5 と 0.95 で誤差のグラフの傾きを比べよう。15 桁合わせるのに何回ずつかかる？
-- 2. α = 0 にすると x* はどうなる？ α = 0.99 では？（どちらも G の不動点）
-- 3. どこからもリンクされないページを作ると、そのページのランクはいくつになる？ 式 (1 − α)/n と比べよう
-- 4. U で 1 ページに全部集めてから始めても、同じ x* に着くことを確かめよう（バナッハの定理の「どこからでも」）

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5 と同じ座標（左上が原点、y は下向き）で描くための枠
screen :: Env -> [Picture] -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 文字（y 軸を反転してあるので、文字だけ元の向きに戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = Translate x y (Scale 1 (-1) (label 0 0 s c str))

mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

ln :: Float -> Color -> Point -> Point -> Picture
ln w c a b = Color c (thickLine w a b)

-- 塗りつぶした円（d は直径）
spot :: Color -> Point -> Float -> Picture
spot c (x, y) d = Color c (Translate x y (disc (d / 2)))

-- 輪（d は直径、w は太さ）
hoop :: Float -> Color -> Point -> Float -> Picture
hoop w c (x, y) d = Color c (Translate x y (ring (d / 2) w))

-- 左上 (x, y)、幅 w、高さ h の枠
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし
fillBox :: Color -> Float -> Float -> Float -> Float -> Picture
fillBox c x y w h = Color c (rect (x + w / 2) (y + h / 2) w h)

nf :: RealFloat a => Int -> a -> String
nf d v = showFFloat (Just d) v ""

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ !x = x
nTimes k f !x = nTimes (k - 1) f (f x)

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> Float -> [Point] -> Picture
dashed on off w ps = Pictures (go True on ps)
  where
    go draw left (a : b : rest) =
      let l = dist2 a b
      in if l <= left
           then [thickLine w a b | draw] ++ go draw (left - l) (b : rest)
           else let t = left / l
                    m = (lerpF (fst a) (fst b) t, lerpF (snd a) (snd b) t)
                in [thickLine w a m | draw] ++ go (not draw) (if draw then off else on) (m : b : rest)
    go _ _ _ = []
