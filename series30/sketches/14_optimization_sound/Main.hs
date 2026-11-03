-- ============================================
-- 14 最適化を聞く（自由制作のベース）
-- 焼きなまし法（09）で巡回路を短くしていく様子を音にする。道が短くなるほど音が高くなり、悪くなる手を受け入れるたびにカチッと鳴る
--   熱いうちはカチカチとにぎやか → 冷えると静かになって高い音に落ち着く。「最良」が更新されるとチーンと鳴る
-- クリック = 町を足す（最初のクリックで音が出る）/ スペース = やり直し / ↑↓ = 冷やす速さ / H = 温め直す / P = 音階にそろえる / M = 消音 / R = 新しい町
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Numeric (showFFloat)
import Data.Ord (comparing)
import Data.List (sortBy)
import Control.Monad (replicateM, when)
import Control.Monad.ST (ST, runST)
import Data.Char (toLower)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import System.Random (StdGen, newStdGen, randomR)

nCities0, itersPerFrame :: Int
nCities0 = 50                          -- ★ 町の数
itersPerFrame = 1500

t0 :: Double
t0 = 150                               -- ★ 最初の温度

notes :: [Int]
notes = [0, 2, 4, 7, 9]                -- ★ 音階（ペンタトニック）

droneEvery, tickEvery :: Int
droneEvery = 12                        -- ★ 連続音の代わりの短い音を何フレームごとに鳴らすか
tickEvery = 6                          -- ★ カチッを何フレームごとまで鳴らすか

data World = World
  { wCx, wCy :: !(U.Vector Double)     -- 町の位置
  , wFlash :: !(U.Vector Float)        -- 光り具合
  , wTour, wBestTour :: !(U.Vector Int)
  , wCool :: !Double                   -- ★ 冷やす速さ
  , wT, wCur, wBest, wStart :: !Double
  , wQuantize, wMuted, wAudio :: !Bool
  , wLenLog :: !(Seq.Seq Double)       -- 長さの記録（600 個まで）
  , wGen :: !StdGen
  }

mapX, mapY, mapW, mapH :: Float
mapX = 30
mapY = 100
mapW = 1280 - 60
mapH = 800 - 260

main :: IO ()
main = runSketch (sketch "30-14 Listening to Optimization" (error "skStart makes the world"))
  { skBack  = hsb 225 30 12
  , skStart = \_ -> newCities =<< (World U.empty U.empty U.empty U.empty U.empty 0.999996 0 0 0 0 True False False Seq.empty <$> newStdGen)
  , skStep  = stepWorld
  , skDraw  = drawAll
  , skEvent = onEvent
  }

newCities :: World -> IO World
newCities w = do
  xs <- replicateM nCities0 (randomRIO (20, mapW - 20))
  ys <- replicateM nCities0 (randomRIO (20, mapH - 20))
  restart w { wCx = U.fromList (map (realToFrac . (+ mapX)) xs), wCy = U.fromList (map (realToFrac . (+ mapY)) ys), wFlash = U.replicate nCities0 0 }

restart :: World -> IO World
restart w = do
  s <- shuffleIO [0 .. U.length (wCx w) - 1]
  let t = U.fromList s
      l = tourLen w t
  pure w { wTour = t, wBestTour = t, wCur = l, wBest = l, wStart = l, wT = t0, wLenLog = Seq.empty }

dd :: U.Vector Double -> U.Vector Double -> Int -> Int -> Double
dd xs ys a b =
  let dx = U.unsafeIndex xs a - U.unsafeIndex xs b
      dy = U.unsafeIndex ys a - U.unsafeIndex ys b
  in sqrt (dx * dx + dy * dy)
{-# INLINE dd #-}

tourLen :: World -> U.Vector Int -> Double
tourLen w t
  | U.null t = 0
  | otherwise = U.sum (U.zipWith (dd (wCx w) (wCy w)) t (U.snoc (U.drop 1 t) (U.head t)))

-- 焼きなましの途中の状態
data SA = SA
  { saGen :: !StdGen
  , saT, saCur :: !Double
  , saClicks, saClickCity :: !Int      -- 悪くなる手を受け入れた回数と、最後の町
  }

-- 焼きなまし法の 1 手（09 と同じ）
annealStep :: U.Vector Double -> U.Vector Double -> M.MVector s Int -> SA -> ST s SA
annealStep xs ys tour s@SA{..} = do
  let n = M.length tour
      (i0, g1) = randomR (0, n - 1) saGen
      (j0, g2) = randomR (0, n - 1) g1
      i = min i0 j0
      j = max i0 j0
  if j - i < 2 || (i == 0 && j == n - 1) then pure s { saGen = g2 } else do
    a <- M.unsafeRead tour i
    b <- M.unsafeRead tour (i + 1)
    c <- M.unsafeRead tour j
    e <- M.unsafeRead tour ((j + 1) `mod` n)
    let d = dd xs ys
        delta = d a c + d b e - d a b - d c e
        (u, g3) = randomR (0, 1 :: Double) g2
    if delta < 0 || (saT > 1e-9 && u < exp (negate delta / saT))
      then do
        reverseRange tour (i + 1) j
        pure s { saGen = g3, saCur = saCur + delta
               , saClicks = if delta > 0 then saClicks + 1 else saClicks
               , saClickCity = if delta > 0 then b else saClickCity }
      else pure s { saGen = g3 }

reverseRange :: M.MVector s Int -> Int -> Int -> ST s ()
reverseRange v !p !q
  | p >= q = pure ()
  | otherwise = M.unsafeSwap v p q >> reverseRange v (p + 1) (q - 1)

-- itersPerFrame 手をまとめて試す
runFrame :: World -> (U.Vector Int, SA)
runFrame World{..} = runST $ do
  mv <- U.thaw wTour
  let loop 0 s = pure s
      loop k s = do
        s1 <- annealStep wCx wCy mv s
        loop (k - 1) s1 { saT = saT s1 * wCool }
  r <- loop (itersPerFrame :: Int) (SA wGen wT wCur 0 (-1))
  t <- U.freeze mv
  pure (t, r)

-- ★ 長さ → 音の高さ：最初の長さで低い音、短くなるほど高い音
lenToFreq :: World -> Double -> Float
lenToFreq w l =
  let t = max 0 (min 0.8 (1 - l / max 1e-9 (wStart w))) / 0.8     -- 0（最初）〜 1（とても短い）
      semi = t * 36                                                 -- 3 オクターブ
      oct = floor (semi / 12) :: Int
      k = semi - fromIntegral oct * 12
      best = maximum (0 : [s | s <- notes, fromIntegral s <= k])
      semiQ = if wQuantize w then fromIntegral (oct * 12 + best) else semi
  in 110 * 2 ** (realToFrac semiQ / 12)

-- 1 フレーム分を進めて、音を鳴らす
stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  let fc = envFrame env
      big = U.length (wCx w0) >= 5
      (tour, r) = if big then runFrame w0 else (wTour w0, SA (wGen w0) (wT w0) (wCur w0) 0 (-1))
      tNow = if big && saT r < 0.01 then 0 else saT r
      w1 = w0 { wTour = tour, wGen = saGen r, wT = tNow, wCur = saCur r }
      newBest = big && saCur r < wBest w0 - 1e-6
      -- 最良を更新 → チーン
      bigGain = newBest && wBest w0 - saCur r > wStart w0 * 0.004
      w2 = if newBest then w1 { wBest = saCur r, wBestTour = tour } else w1
      soundOn = wAudio w2 && not (wMuted w2)
      clicking = saClicks r > 0 && fc `mod` tickEvery == 0
      flash0 = U.map (* 0.85) (wFlash w2)
      flash1 = if not (wMuted w2) && clicking && saClickCity r >= 0 then flash0 U.// [(saClickCity r, 1)] else flash0
      l = wLenLog w2 Seq.|> saCur r
  when (soundOn && bigGain) $ ping (lenToFreq w2 (saCur r) * 2) 0.12
  -- 音：連続音の高さ = 今の長さ、カチッ = 悪くなる手を受け入れた
  when (soundOn && fc `mod` droneEvery == 0) $
    playTone (roundHz (lenToFreq w2 (saCur r))) (quant (0.08 + 0.1 * min 1 (realToFrac (tNow / t0)))) 0.24 Triangle
  when (soundOn && clicking) $ tick (min 0.3 (0.05 + fromIntegral (saClicks r) * 0.004))
  pure w2 { wFlash = flash1, wLenLog = if Seq.length l > 600 then Seq.drop 1 l else l }

roundHz :: Float -> Float
roundHz = fromIntegral . (round :: Float -> Int)

-- 音量を 0.02 きざみにする（波形ファイルの数をおさえる）
quant :: Float -> Float
quant v = fromIntegral (round (v * 50) :: Int) / 50

-- カチッ（とても短い高い音。p5.js 版はノイズ）
tick :: Float -> IO ()
tick vol = playTone 2600 (quant vol) 0.02 Square

-- チーン（減衰するサイン波）
ping :: Float -> Float -> IO ()
ping freq vol = playTone (roundHz (clampF 40 6000 freq)) vol 0.8 Sine

drawAll :: Env -> World -> Picture
drawAll env w@World{..} = screen env $ Pictures [mapPic, lower, hud]
  where
    wd = envW env
    ht = envH env
    px i = (fl (wCx U.! i), fl (wCy U.! i))
    ratio = realToFrac (max 0 (min 0.8 (1 - wCur / max 1e-9 wStart))) :: Float
    hu = remap ratio 0 0.8 200 50

    -- 地図
    mapPic = Pictures $
      [ loopPath 1.5 (hsba 30 70 100 0.3) (map px (U.toList wTour))
      , loopPath 3 (hsb hu 80 100) (map px (U.toList wBestTour)) ]
      ++ [ dot white (fl x) (fl y) (8 + f * 16) | (x, y, f) <- zip3 (U.toList wCx) (U.toList wCy) (U.toList wFlash) ]

    -- 下：長さと温度
    gy = ht - 140
    gh = 100
    gw = wd - 360
    lower = Pictures
      [ seg 1 (hsba 0 0 100 0.25) (30, gy + gh) (30 + gw, gy + gh)
      , path 2 (hsb hu 80 100)
          [ (30 + fromIntegral k / 599 * gw, gy + gh - fl (max 0 (min 1 (l / max 1e-9 wStart))) * gh) | (k, l) <- zip [0 :: Int ..] (toList wLenLog) ]
      , txt (gw + 60) (gy + 20) 15 white ("Length " ++ nf 0 wCur ++ " (" ++ nf 1 (100 * wCur / max 1e-9 wStart) ++ "%)")
      , txt (gw + 60) (gy + 46) 15 white ("Temp T = " ++ nf 2 wT)
      , txt (gw + 60) (gy + 72) 15 white ("Tone " ++ nf 0 (lenToFreq w wCur) ++ " Hz" ++ (if wQuantize then " (scale)" else ""))
      , txt 30 (gy - 8) 14 (gr 60) "Length over time"
      ]

    hud = Pictures
      [ txt 20 40 15 white ("Click: city (sound on)  Space: restart  U/D cooling " ++ show wCool ++ "  H: reheat  P: scale " ++ onOff wQuantize ++ "  M: mute " ++ onOff wMuted ++ "  R: new")
      , txt 20 68 14 (gr 60) $ if wAudio
          then "Listen: clicks = trying bad moves, rising tone = shorter tour, ping = new record"
          else "Click or press a key first to turn the sound on"
      ]

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- キーとマウス
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseP env
    in if mx > mapX && mx < mapX + mapW && my > mapY && my < mapY + mapH
         then do
           let n = U.length (wCx w)
               t = U.snoc (wTour w) n
               w1 = w { wAudio = True, wCx = U.snoc (wCx w) (realToFrac mx), wCy = U.snoc (wCy w) (realToFrac my)
                      , wFlash = U.snoc (wFlash w) 1, wTour = t, wBestTour = t }
               l = tourLen w1 t
           when (not (wMuted w)) (ping 880 0.1)
           pure w1 { wCur = l, wBest = l }
         else pure w { wAudio = True }
  EventKey (SpecialKey KeySpace) Down _ _ -> restart w { wAudio = True }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wAudio = True, wCool = min 0.9999995 (round8 (wCool w + (1 - wCool w) * 0.5)) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wAudio = True, wCool = max 0.9999 (round8 (1 - (1 - wCool w) * 2)) }
  EventKey (SpecialKey _) Down _ _ -> pure w { wAudio = True }
  EventKey (Char c) Down _ _ ->
    let w1 = w { wAudio = True }
    in case toLower c of
         'h' -> pure w1 { wT = t0 * 0.4 }
         'p' -> pure w1 { wQuantize = not (wQuantize w) }
         'm' -> pure w1 { wMuted = not (wMuted w) }
         'r' -> newCities w1
         _ -> pure w1
  _ -> pure w

round8 :: Double -> Double
round8 v = fromIntegral (round (v * 1e8) :: Int) / 1e8

-- ---------- やってみよう ----------
-- 1. 音階（notes）を [0, 3, 5, 7, 10] や [0, 4, 7] に変えると、収束の「曲」の雰囲気はどう変わる？
-- 2. 町を円や文字の形にクリックして並べ、焼きなましで「なぞる」作品にしよう
-- 3. 温め直し（H）を一定時間ごとに自動で入れると、音楽のくり返しのようになる
-- 4. 09 の代わりに 10 の GA や 08 の 2-opt を音にするとどう聞こえる？ 何を音にするかが作品の中心


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標で描くための変換
screen :: Env -> Picture -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2)

-- マウスの位置（左上が原点、y は下向き）
mouseP :: Env -> Point
mouseP = toP5 <*> envMouse

-- gloss の座標 → 左上が原点の座標
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字：(x, y) は左端のベースライン
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label x (negate y)

-- 線分（太さ t）
seg :: Float -> Color -> Point -> Point -> Picture
seg t c (x1, y1) (x2, y2) = Color c (thickLine t (x1, negate y1) (x2, negate y2))

-- 折れ線（太さ t）
path :: Float -> Color -> [Point] -> Picture
path t c ps = Color c (thickPath t [(x, negate y) | (x, y) <- ps])

-- 閉じた折れ線（太さ t）
loopPath :: Float -> Color -> [Point] -> Picture
loopPath t c ps = path t c (ps ++ take 1 ps)

-- 塗りつぶした円：中心 (x, y)、直径 d
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (at (x, negate y) (disc (d / 2)))

-- 灰色（明るさ 0〜100）
gr :: Float -> Color
gr = hsb 0 0

-- 小数点以下 d 桁の文字列（p5.js の nf）
nf :: RealFloat a => Int -> a -> String
nf d x
  | d <= 0 = show (round x :: Int)
  | otherwise = showFFloat (Just d) x ""

-- Double → Float（計算は Double、描くのは Float）
fl :: Double -> Float
fl = realToFrac

-- 並びをランダムに混ぜる（p5.js の shuffle）
shuffleIO :: [a] -> IO [a]
shuffleIO xs = do
  keys <- mapM (const (randomRIO (0, 1 :: Float))) xs
  pure (map snd (sortBy (comparing fst) (zip keys xs)))

