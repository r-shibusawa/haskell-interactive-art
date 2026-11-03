-- ============================================
-- 11 エレベーターを待つか、階段を使うか（オンラインアルゴリズム）
-- エレベーターがいつ来るかは分からない（m 分後）。階段なら B 分。待つほど損かもしれない
-- 未来を知っている人（OPT）のコスト = min(m, B)。分からないまま決める戦略のコストと比べる → 競合比
-- 「B 分だけ待って、来なければ階段」は、どんな m でも OPT の 2 倍以内（2-競合）。乱択ならさらに e/(e−1) ≈ 1.58 倍
-- マウスの左右 = エレベーターが来る時刻 m（意地悪な相手）/ 1〜4 = 戦略を強調 / スペース = ランダムな m で 1 回 / 上下 = B
-- ============================================
module Main (main) where

import Art
import Text.Printf (printf)

data World = World
  { wB :: !Float             -- ★ 階段にかかる時間 B
  , wFocus :: !Int
  , wSim :: Maybe (Float, Float)  -- ランダムな (m, w)
  }

names :: [String]
names = ["Stairs at once", "Wait forever", "Wait B, then stairs (fixed)", "Random wait (randomized)"]

hues :: [Float]
hues = [0, 200, 45, 140]

main :: IO ()
main = runSketch (sketch "15-11 Ski Rental" (World 10 3 Nothing))
  { skBack  = hsb 210 30 9
  , skDraw  = drawW
  , skEvent = onEvent
  }

e' :: Float
e' = exp 1

-- 戦略 k が、エレベーターが m 分後に来るときに払う時間
cost :: Float -> Int -> Float -> Float
cost b k m = case k of
  0 -> b
  1 -> m
  2 -> if m <= b then m else 2 * b      -- B 分待って来なければ、さらに B 分かけて階段
  _ -> expectedRandom b m

-- 乱択：待つ時間 w を密度 e^(w/B) / (B(e−1)) で選ぶ（0 ≤ w ≤ B）。期待コストを数値計算
expectedRandom :: Float -> Float -> Float
expectedRandom b m = go 0 0
  where
    n = 200 :: Int
    nf' = fromIntegral n :: Float
    go :: Int -> Float -> Float
    go q !s
      | q >= n = s
      | otherwise =
          let w = (fromIntegral q + 0.5) / nf' * b
              p = exp (w / b) / (b * (e' - 1)) * (b / nf')
          in go (q + 1) (s + p * (if m <= w then m else w + b))

opt :: Float -> Float -> Float
opt = min

drawW :: Env -> World -> Picture
drawW env w =
  let b = wB w
      gx' = 90
      gy' = 130
      gw = envW env * 0.5
      gh = envH env - 300
      h = envH env
      mMax = 3 * b
      (mouseX', _) = mouseP env
      m = clampF 0.01 mMax (remap mouseX' gx' (gx' + gw) 0 mMax)
      gridY r = gy' + gh - ((r - 0.5) / 3.5) * gh
      grid = [ Pictures
                 [ seg env (hsb 210 20 30) 1 (gx', gridY r) (gx' + gw, gridY r)
                 , txt env (gx' - 44) (gridY r + 5) 14 (lum 60) (show (round r :: Int) ++ "x") ]
             | r <- [1, 2, 3, 4] ]
      -- グラフ：横 = m、縦 = 競合比（コスト / OPT）
      curve k =
        let isF = wFocus w == k
            pts = [ (gx' + (mm / mMax) * gw, gy' + gh - ((ratio - 0.5) / 3.5) * gh)
                  | q <- [1 .. 160 :: Int]
                  , let mm = (fromIntegral q / 160) * mMax
                  , let ratio = min 4 (cost b k mm / opt mm b) ]
        in Color (hsba (hues !! k) 70 95 (if isF then 1 else 0.35))
             (thickPath (if isF then 4 else 2) (map (pt env) pts))
      xOf v = gx' + (v / mMax) * gw
      px = gx' + gw + 50
      py = 150
      worst = ["inf", "inf", "2.00x", f2 (e' / (e' - 1)) ++ "x"]
      strategy k =
        let c = cost b k m
            isF = wFocus w == k
            yk = py + 80 + fromIntegral k * 64
        in Pictures
             [ txt env px yk (if isF then 18 else 16) (hsba (hues !! k) 70 95 (if isF then 1 else 0.6)) (show (k + 1) ++ ". " ++ (names !! k))
             , txt env (px + 20) (yk + 24) 15 (lum (if isF then 95 else 65))
                 ("cost " ++ f1 c ++ " min  ratio " ++ f2 (c / opt m b) ++ "  worst " ++ (worst !! k)) ]
      simPic = case wSim w of
        Nothing -> Blank
        Just (sm, sw) -> Pictures
          [ txt env px (h - 134) 15 (lum 80) ("Random m = " ++ f1 sm ++ ", chosen wait w = " ++ f1 sw)
          , txt env px (h - 112) 15 (lum 80)
              (if sm <= sw then "-> caught the elevator (" ++ f1 sm ++ " min)" else "-> stairs (" ++ f1 (sw + b) ++ " min)") ]
  in Pictures
       [ Pictures grid
       , Pictures (map curve [0 .. 3])
       , seg env (hsba 0 0 100 0.5) 1 (xOf m, gy') (xOf m, gy' + gh)
       , seg env (lum 50) 1 (xOf b, gy' + gh) (xOf b, gy' + gh + 8)
       , txtC env (xOf b) (gy' + gh + 20) 14 (lum 70) "m = B"
       , txtC env (gx' + gw / 2) (gy' + gh + 44) 14 (lum 70) "Elevator arrival time m (minutes) ->"
       , txt env px py 20 (lum 100) ("m = " ++ f1 m ++ " min, stairs B = " ++ show (round b :: Int) ++ " min")
       , txt env px (py + 32) 16 (lum 75) ("One who knows the future (OPT): " ++ f1 (opt m b) ++ " min")
       , Pictures (map strategy [0 .. 3])
       , simPic
       , txt env 20 36 16 (lum 100)
           ("Mouse left/right: arrival m (adversary)  1-4: focus strategy  Space: one random trial  Up/Down: B = " ++ show (round b :: Int))
       , txt env 20 62 14 (lum 65) "Vertical = cost / OPT (competitive ratio). Online: decide now without the future. Ski rent-or-buy is the same problem"
       , txt env 20 (h - 28) 15 (hsb 45 80 100)
           "A fixed strategy is at best 2x (the adversary picks m just after you leave). Randomized: 1.58x, since w stays hidden"
       ]

sampleWait :: Float -> IO Float
sampleWait b = do
  -- 逆関数法：F(w) = (e^(w/B) − 1)/(e − 1)
  u <- randomRIO (0, 1)
  pure (b * log (1 + u * (e' - 1)))

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    m <- randomRIO (0, 3 * wB w)
    s <- sampleWait (wB w)
    pure w { wSim = Just (m, s) }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wB = min 30 (wB w + 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wB = max 2 (wB w - 2) }
  EventKey (Char c) Down _ _
    | c == '1' -> pure w { wFocus = 0 }
    | c == '2' -> pure w { wFocus = 1 }
    | c == '3' -> pure w { wFocus = 2 }
    | c == '4' -> pure w { wFocus = 3 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 戦略 3 の比が最大になる m はどこ？（m がちょうど B を少し超えたとき → 2 倍）
-- 2. 「B/2 分待つ」戦略を cost に追加して、最悪の比を調べる（B 分待つより悪くなる）
-- 3. スキー：レンタル 1 日 1、購入 B。「B 日レンタルしたら買う」が同じ 2-競合

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 文字：txt は左下が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = label (gx env x - fromIntegral (length str) * s * 0.72) (gy env y) s c str

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0

-- 小数の表示（p5.js の nf）
f1, f2 :: Float -> String
f1 = printf "%.1f"
f2 = printf "%.2f"
