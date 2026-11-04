-- ============================================
-- 38-13 max-plus のダイヤを聴く
-- 10 の周期スケジュール x(k+1) = A ⊗ x(k)（max-plus）をそのまま音にする：
--   各工程の開始時刻にその工程の音が鳴る。やがて周期 λ（トロピカル固有値）のリズムに落ち着く
--   自由制作のベース：工程の数・つなぎ方・音色を変えて「リズムマシン」にしよう
-- 操作：クリックで音をスタート / ノードをクリック = 所要時間を 0.25 ずつ変える（Shift で減らす）/ 1〜3 = 音色 / R = リセット / ↑↓ = テンポ
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.List (foldl', sortOn)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

tau0 :: U.Vector Double
tau0 = U.fromList [0.5, 1.0, 0.75, 0.5]                        -- ★ 各工程の所要時間（秒）

arcs :: [(Int, Int)]
arcs = [(0, 1), (1, 2), (2, 3), (3, 0), (1, 3), (3, 1), (0, 2)]   -- ★ j → i のつながり

notes :: U.Vector Float
notes = U.fromList [220, 330, 277.2, 440]                      -- ★ 各工程の音の高さ（Hz）

moveT :: Double
moveT = 0.25                                                   -- ★ 工程間の搬送時間（秒）

n :: Int
n = 4

negInf :: Double
negInf = negate (1 / 0)

data World = World
  { wTaus :: !(U.Vector Double)
  , wWave :: !Wave
  , wTempo :: !Double
  , wX :: !(U.Vector Double)      -- いまの x(k)
  , wPending :: [(Double, Int)]   -- まだ鳴っていないイベント（時刻の順）
  , wPlayed :: [(Double, Int)]    -- 鳴ったイベント
  , wT0 :: !Double                -- 開始した時刻
  , wNow :: !Double
  , wFlash :: !(U.Vector Float)
  , wAudio :: !Bool }             -- クリックで音をスタート

resetSchedule :: Double -> World -> World
resetSchedule t w = w
  { wX = U.replicate n 0, wPending = [(0, i) | i <- [0 .. n - 1]], wPlayed = [], wT0 = t, wNow = 0 }

world0 :: World
world0 = resetSchedule 0 (World tau0 Triangle 1 U.empty [] [] 0 0 (U.replicate n 0) False)

main :: IO ()
main = runSketch (sketch "38-13 Listening to Max-Plus Schedules" world0)
  { skBack  = bgCol
  , skStep  = \env _ w -> stepWorld env w
  , skDraw  = draw
  , skEvent = onEvent
  }

node :: Env -> Int -> Point
node env i =
  let a = 2 * pi * fromIntegral i / fromIntegral n - pi / 2
  in (envW env * 0.25 + 150 * cos a, envH env * 0.45 + 150 * sin a)

hueOf :: Int -> Float
hueOf i = fromIntegral ((i * 80 + 20) `mod` 360)

matrixA :: U.Vector Double -> U.Vector Double
matrixA ts = U.generate (n * n) f U.// [ (i * n + j, ts U.! j + moveT) | (j, i) <- arcs ]
  where f k = let (i, j) = k `divMod` n in if i == j then ts U.! i else negInf   -- 同じ工程は前の仕事が終わるまで待つ

at2 :: U.Vector Double -> Int -> Int -> Double
at2 a i j = a U.! (i * n + j)

-- ★ 1 回ぶん進める：x_i(k+1) = max_j (A_ij + x_j(k))
stepOnce :: World -> World
stepOnce w =
  let a = matrixA (wTaus w)
      x = wX w
      y = U.generate n $ \i ->
        let m = foldl' (\acc j -> max acc (at2 a i j + x U.! j)) negInf [0 .. n - 1]
        in if isInfinite m then x U.! i + 1 else m
      new = [ (y U.! i, i) | i <- [0 .. n - 1], y U.! i > x U.! i ]
  in w { wX = y, wPending = sortOn fst (wPending w ++ new) }

-- Karp のアルゴリズム（10 と同じ）
karp :: U.Vector Double -> Double
karp a = foldl' max negInf (map vCycle [0 .. n - 1])
  where
    ds = V.iterateN (n + 1) next (U.replicate n 0)
    next prev = U.generate n $ \v ->
      foldl' (\acc u -> if at2 a v u > negInf && prev U.! u > negInf then max acc (prev U.! u + at2 a v u) else acc) negInf [0 .. n - 1]
    dn = ds V.! n
    vCycle v
      | dn U.! v == negInf = negInf
      | otherwise = foldl' min (1 / 0)
          [ (dn U.! v - dk U.! v) / fromIntegral (n - k) | k <- [0 .. n - 1], let dk = ds V.! k, dk U.! v > negInf ]

-- 先の予定を用意しておき、時刻が来たら鳴らす
stepWorld :: Env -> World -> IO World
stepWorld env w0 = do
  let now = (realToFrac (envTime env) - wT0 w0) * wTempo w0
      prepare :: Int -> World -> World
      prepare g w
        | g < 50 && (null (wPending w) || fst (last (wPending w)) < now + 4) = prepare (g + 1) (stepOnce w)
        | otherwise = w
      w1 = prepare 0 w0 { wNow = now }
      (due, later) = span ((<= now) . fst) (wPending w1)
  w2 <- foldM (ring' now) w1 { wPending = later } due
  pure w2
    { wPlayed = filter ((> now - 8) . fst) (wPlayed w2)
    , wFlash = U.map (* 0.9) (wFlash w2) }
  where
    ring' _ w ev@(_, i) = do
      -- 短い音を 1 つ鳴らす（高さ、音量、長さ、波形）
      if wAudio w
        then playTone (notes U.! i) 0.25 (max 0.05 (realToFrac (wTaus w U.! i * 0.6 / wTempo w))) (wWave w)
        else pure ()
      pure w { wFlash = wFlash w U.// [(i, 1)], wPlayed = ev : wPlayed w }

draw :: Env -> World -> Picture
draw env w = Pictures (
  concatMap arcPic arcs
  ++ concatMap nodePic [0 .. n - 1]
  -- 流れるダイヤ：右 = いま
  ++ [ box env (tx - 10) (ty - 30) (tw + 20) (rowH * fromIntegral n + 40) (hsba 0 0 100 0.06) ]
  ++ concatMap rowPic [0 .. n - 1]
  ++ [ seg env 2 (hsb 90 90 100) (nowX, ty - 30) (nowX, ty + rowH * fromIntegral n + 10) ]
  -- 周期の目盛り
  ++ [ let x = nowX - fromIntegral m * realToFrac lam / 12 * tw
       in seg env 1 (hsba 330 70 100 0.4) (x, ty + rowH * fromIntegral n + 10) (x, ty + rowH * fromIntegral n + 20)
     | m <- [0 .. ceiling (8 / max lam 0.1) - 1 :: Int] ]
  ++ [ txt env tx (ty + rowH * fromIntegral n + 44) 14 (hsb 330 70 100) ("Pink ticks = period lambda = " ++ nfd 3 lam ++ " s (largest mean cycle)")
     , txt env 20 30 16 (gray 1) ((if wAudio w then "Sound is on" else "Click to start the sound") ++ "   Click a node: tau + 0.25 (Shift: -)   1-3: wave (" ++ waveName ++ ")")
     , txt env 20 52 16 (gray 1) ("Up/Down: tempo x" ++ nfd 2 (wTempo w) ++ "   R: reset")
     , txt env 20 76 14 (gray 0.65) "x_i(k+1) = max_j (tau_j + move + x_j(k)): start when all earlier work has arrived."
     , txt env 20 96 14 (gray 0.65) "Bars = running, faint frames = planned. Soon every process runs with the same period lambda."
     , txt env 20 (envH env - 24) 14 (gray 0.65) "Make your own: change notes, tau0, arcs. Add processes (n, tau0, notes). Change the wave." ] )
  where
    ts = wTaus w
    lam = karp (matrixA ts)
    now = wNow w
    waveName = case wWave w of
      Triangle -> "triangle"
      Square -> "square"
      _ -> "sine"
    arcPic (j, i) =
      let (ux, uy) = node env j
          (vx, vy) = node env i
          ang = atan2 (vy - uy) (vx - ux)
          o = if (i, j) `elem` arcs then 8 else 0
          ox = negate (sin ang) * o
          oy = cos ang * o
      in [ Color (hsba 0 0 100 0.35) (arrow 2 (pt env (ux + ox) (uy + oy)) (pt env (vx - 26 * cos ang + ox) (vy - 26 * sin ang + oy))) ]
    nodePic i =
      let (x, y) = node env i
          fl = wFlash w U.! i
      in [ dotP env x y (50 + fl * 60) (hsba (hueOf i) 70 100 (fl * 0.6))
         , dotO env x y 52 (hsb (hueOf i) 70 (60 + fl * 40))
         , txtC env x (y - 9) 14 (gray 0.1) (show i)
         , txtC env x (y + 10) 14 (gray 0.1) (nfd 2 (ts U.! i)) ]
    tx = envW env * 0.5
    tw = envW env - tx - 40
    ty = 160
    rowH = 50
    nowX = tx + tw * 8 / 12                  -- 緑の線 = いま、左 = 過去 8 秒、右 = これから 4 秒の予定
    sx t = nowX - realToFrac (now - t) / 12 * tw
    barW i = max 2 (realToFrac (ts U.! i) / 12 * tw)
    rowPic i =
      let y = ty + fromIntegral i * rowH
      in [ seg env 1 (hsba 0 0 100 0.2) (tx, y) (tx + tw, y)
         , txt env tx (y - 16) 14 (gray 0.6) ("Proc " ++ show i ++ " (" ++ show (notes U.! i) ++ " Hz)") ]
         ++ [ box env (sx t) (y - 12) (barW i) 24 (hsba (hueOf i) 70 100 0.8) | (t, h) <- wPlayed w, h == i ]
         ++ [ Color (hsba (hueOf i) 70 100 0.5) (rectLineP (sx t) (y - 12) (barW i) 24) | (t, h) <- wPending w, h == i, t + ts U.! i < now + 4 ]
    rectLineP x y bw bh = let (gx, gy) = pt env (x + bw / 2) (y + bh / 2) in rectLine gx gy bw bh

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down mods _ ->
    let m = mouseP env
        dv = if shift mods == Down then -0.25 else 0.25
        hit = [i | i <- [0 .. n - 1], dist2 m (node env i) < 28]
        change ts i = ts U.// [(i, max 0.25 (min 4 (ts U.! i + dv)))]
    in pure w { wAudio = True, wTaus = foldl' change (wTaus w) hit }
  EventKey (Char '1') Down _ _ -> pure w { wWave = Triangle }
  EventKey (Char '2') Down _ _ -> pure w { wWave = Square }
  EventKey (Char '3') Down _ _ -> pure w { wWave = Sine }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wTempo = min 4 (wTempo w * 1.25) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wTempo = max 0.25 (wTempo w / 1.25) }
  EventKey (Char ch) Down _ _ | ch `elem` "rR" ->
    pure (resetSchedule (realToFrac (envTime env)) w { wTaus = tau0 })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. どれか 1 つの τ を変えても周期が変わらないことがある。クリティカルでない工程だから。どの工程を変えると周期が変わる？
-- 2. notes を和音（ド・ミ・ソ・シ）にする。周期 λ を 1 秒ちょうどにするには τ をどう決める？
-- 3. arcs を変えて「2 つの独立したループ」を作る（たとえば 0⇄1 と 2⇄3）。周期は 2 つになる？（固有値が複数）
-- 4. playTone の波形や音量を工程ごとに変えて、ドラムのようなリズムを作ろう


-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）→ gloss の座標（中央が原点、y は上向き）
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置を p5.js と同じ座標で返す
mouseP :: Env -> Point
mouseP env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が (x, y)）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = pt env x y in label gx gy s c str

-- 中央揃えの文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = let (gx, gy) = pt env x y in labelC gx gy s c str

-- 左上が (x, y) の塗りつぶした長方形
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 線分（太さ wd）。太さ 1 以下は細い線で描く
seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env wd c (x1', y1') (x2', y2')
  | wd <= 1 = Color c (Line [a, b])
  | otherwise = Color c (thickLine wd a b)
  where a = pt env x1' y1'
        b = pt env x2' y2'

-- 塗りつぶした円（直径 d）
dotP :: Env -> Float -> Float -> Float -> Color -> Picture
dotP env x y d c = Color c (at (pt env x y) (disc (d / 2)))

-- 暗いふちのある円（直径 d）
dotO :: Env -> Float -> Float -> Float -> Color -> Picture
dotO env x y d c = Pictures [dotP env x y (d + 2) (hsb 0 0 10), dotP env x y (d - 2) c]

bgCol :: Color
bgCol = hsb 190 55 10

-- 小数 n 桁の文字列（p5.js の nf）
nf :: Int -> Float -> String
nf n' v = noMinusZero (showFFloat (Just n') v "")

nfd :: Int -> Double -> String
nfd n' v = noMinusZero (showFFloat (Just n') v "")

-- "-0.00" は "0.00" と書く
noMinusZero :: String -> String
noMinusZero str = if all (`elem` "-0.") str then dropWhile (== '-') str else str
