-- ============================================
-- 11 乗算型重み更新法（重みつき多数決）
-- 8 人の「専門家」が毎日、明日の天気（晴れ / 雨）を予想する。誰が当たるかは最初わからない
-- アルゴリズム：重みの合計が大きい方の意見に従う。はずれた専門家の重みを (1 − ε) 倍にする
-- これだけで、自分のまちがいは「いちばん優秀な専門家のまちがい」の約 2 倍 + 定数におさまる
--   まちがい ≤ 2(1 + ε)·m* + 2 ln n / ε  （m* = 最優秀の専門家のまちがい数）
-- 操作：スペース = 再生・停止 / → = 1 日進める / ↑↓ = ε を変える / R = やり直し
--       1〜3 = お手本（名人が 1 人・名人が途中で交代・全員でたらめ）
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

n :: Int
n = 8

speed :: Int
speed = 6               -- ★ 何フレームに 1 日進めるか

names :: String
names = "ABCDEFGH"

-- 重みはとても小さくなるので Double で持つ
data World = World
  { wEps      :: !Double             -- ★ はずれたときに重みを減らす割合
  , wAcc      :: !(U.Vector Double)  -- 各専門家が当たる確率
  , wWeights  :: !(U.Vector Double)
  , wMistakes :: !(U.Vector Int)
  , wAlgMiss  :: !Int
  , wDay      :: !Int
  , wHistAlg  :: ![Float]            -- 履歴（新しい順、600 日分まで）
  , wHistBest :: ![Float]
  , wHistBnd  :: ![Float]
  , wPred     :: ![Int]              -- 今日の予想（1 = 雨、0 = 晴れ。空 = まだ）
  , wTruth    :: !Int
  , wAlg      :: !Int
  , wScenario :: !Int
  , wAuto     :: !Bool
  }

main :: IO ()
main = runSketch (sketch "17-11 Multiplicative weights" (reset 1 (World 0.25 U.empty U.empty U.empty 0 0 [] [] [] [] 0 0 1 True)))
  { skBack  = cBg
  , skStep  = \env _ w -> if wAuto w && envFrame env `mod` speed == 0 then oneDay w else pure w
  , skDraw  = drawW
  , skEvent = onEvent
  }

reset :: Int -> World -> World
reset k w =
  let base = U.fromList [0.55, 0.6, 0.5, 0.62, 0.58, 0.52, 0.9, 0.5]   -- G が名人
  in w { wScenario = k
       , wAcc = if k == 3 then U.map (const 0.5) base else base
       , wWeights = U.replicate n 1, wMistakes = U.replicate n 0
       , wAlgMiss = 0, wDay = 0, wHistAlg = [], wHistBest = [], wHistBnd = [], wPred = [] }

-- ★ 1 日ぶん
oneDay :: World -> IO World
oneDay w0 = do
  let w = if wScenario w0 == 2 && wDay w0 == 150
            then w0 { wAcc = wAcc w0 U.// [(6, 0.45), (1, 0.92)] }   -- 名人が交代する
            else w0
  r <- randomRIO (0, 1 :: Float)
  let truth = if r < 0.5 then 1 else 0 :: Int                        -- 1 = 雨、0 = 晴れ
  preds <- mapM (\a -> do
                    q <- randomRIO (0, 1 :: Double)
                    pure (if q < a then truth else 1 - truth))
                (U.toList (wAcc w))
  let pv = U.fromList preds
      eps = wEps w
      -- 重みつき多数決
      wRain = U.sum (U.zipWith (\p v -> if p == 1 then v else 0) pv (wWeights w))
      wSun = U.sum (U.zipWith (\p v -> if p == 0 then v else 0) pv (wWeights w))
      alg = if wRain >= wSun then 1 else 0 :: Int
      -- はずれた専門家の重みを (1 − ε) 倍
      ws1 = U.zipWith (\p v -> if p /= truth then v * (1 - eps) else v) pv (wWeights w)
      -- 小さくなりすぎないように（比は同じ）
      ws2 = if U.maximum ws1 < 1e-100 then U.map (* 1e100) ws1 else ws1
      miss = U.zipWith (\p c -> if p /= truth then c + 1 else c) pv (wMistakes w)
      best = U.minimum miss
      algMiss = wAlgMiss w + (if alg /= truth then 1 else 0)
      bound = 2 * (1 + eps) * fromIntegral best + 2 * log (fromIntegral n) / eps
  pure w { wWeights = ws2, wMistakes = miss, wAlgMiss = algMiss, wDay = wDay w + 1
         , wHistAlg = take 600 (fromIntegral algMiss : wHistAlg w)
         , wHistBest = take 600 (fromIntegral best : wHistBest w)
         , wHistBnd = take 600 (realToFrac bound : wHistBnd w)
         , wPred = preds, wTruth = truth, wAlg = alg }

weather :: Int -> String
weather v = if v == 1 then "rain" else "sun"

drawW :: Env -> World -> Picture
drawW env w =
  let ww = envW env
      hh = envH env
      total = U.sum (wWeights w)
      x0 = 30
      y0 = 100
      rowH = min 46 ((hh * 0.56 - 100) / fromIntegral n)
      barW = ww * 0.36
      predOf i = case drop i (wPred w) of
        (p : _) -> Just p
        [] -> Nothing
      row i =
        let y = y0 + fromIntegral i * rowH
            share = realToFrac (wWeights w U.! i / total) :: Float
            col = case predOf i of
              Just p | p /= wTruth w -> cBad
              _ -> cMint
        in Pictures
             [ box env cPanel (x0 + 60) y barW (rowH - 8)
             , box env col (x0 + 60) y (max 2 (share * barW)) (rowH - 8)
             , txtM env (x0 + 10) (y + rowH / 2 - 4) 18 cFg (take 1 (drop i names))
             , txtM env (x0 + 36) (y + rowH / 2 - 4) 14 cDim (maybe "" (\p -> if p == 1 then "R" else "S") (predOf i))
             , txtM env (x0 + 70 + barW) (y + rowH / 2 - 4) 14 cDim
                 (fix 1 (share * 100) ++ "%   wrong " ++ show (wMistakes w U.! i)
                   ++ "   (accuracy " ++ show (round (wAcc w U.! i * 100) :: Int) ++ "%)") ]
      has = not (null (wPred w))
      ry = y0 + fromIntegral n * rowH + 6
      -- まちがい数のグラフ
      gX = 30
      gY = ry + 36
      gW = ww - 60
      gH = hh - gY - 16
      latest xs = take 1 xs
      mx = maximum (1 : latest (wHistBnd w) ++ latest (wHistAlg w))
      points hist =
        [ (gX + 10 + fromIntegral i / 600 * (gW - 20), gY + gH - 10 - v / mx * (gH - 30))
        | (i, v) <- zip [0 :: Int ..] (reverse hist) ]
      plot col hist = path env col 2.5 (points hist)
      -- 点線：3 区間おきに描く
      plotDashed col hist =
        let ps = points hist
        in Pictures [ seg env col 2.5 a b | (i, a, b) <- zip3 [0 :: Int ..] ps (drop 1 ps), even (i `div` 3) ]
      best = U.minimum (wMistakes w)
  in Pictures
       [ txt env 30 24 22 cFg "Multiplicative weights: wrong experts lose their voice"
       , txt env 30 56 14 cDim ("Space play/stop   Right 1 day   Up/Down eps = " ++ fixD 2 (wEps w)
           ++ "   R reset   1 one expert   2 expert changes   3 all random")
       , Pictures (map row [0 .. n - 1])
       , txt env x0 ry 18 cFg ("Day " ++ show (wDay w) ++ "   truth: " ++ (if has then weather (wTruth w) else "-")
           ++ "   algorithm: " ++ (if has then weather (wAlg w) else "-"))
       , if has
           then txt env (x0 + 620) ry 18 (if wAlg w == wTruth w then cMint else cBad)
                  (if wAlg w == wTruth w then "Right" else "Wrong")
           else Blank
       , box env cPanel gX gY gW gH
       , plotDashed cGold (wHistBnd w)
       , plot cMint (wHistBest w)
       , plot cOrchid (wHistAlg w)
       , txt env (gX + 16) (gY + 10) 14 cOrchid ("Algorithm mistakes " ++ show (wAlgMiss w))
       , txt env (gX + 270) (gY + 10) 14 cMint ("Best expert m* = " ++ show best)
       , txt env (gX + 500) (gY + 10) 14 cGold ("Bound: 2(1+eps)m* + 2 ln n / eps = "
           ++ (case wHistBnd w of
                 (b : _) -> fix 1 b
                 [] -> "-"))
       ]

fixD :: Int -> Double -> String
fixD d v = showFFloat (Just d) v ""

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wAuto = not (wAuto w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> oneDay w
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wEps = min 0.9 (wEps w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wEps = max 0.05 (wEps w - 0.05) }
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> pure (reset (wScenario w) w)
    | c `elem` "123" -> pure (reset (fromEnum c - fromEnum '0') w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. お手本 2 で、名人が交代したあと重みが移るまでの日数を見る
-- 2. ε を大きくすると、交代には速く応じるが、ふだんのまちがいは増える？
-- 3. 全員でたらめ（3）でも保証は守られるか確かめよう

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

