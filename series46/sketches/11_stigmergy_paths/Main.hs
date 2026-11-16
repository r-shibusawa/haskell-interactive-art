-- ============================================
-- 46-11 歩くほど道になる ─ アクティブウォーカーモデル（Helbing）
-- 芝生を横切る人は、目的地の方向に歩きながら、すでに踏まれて歩きやすい所（踏み跡）に少し引き寄せられる
-- 歩いた所は踏み固められ、だれも歩かない所は芝生がまた生えてくる。すると「みんなで少しずつ遠回りして共有する道」のネットワークができる
-- 操作：クリック = 入口を追加（最大 8）/ C = 入口を 4 つに戻す / ↑↓ = 踏み跡への引かれやすさ κ / ←→ = 芝生が戻る速さ / R = 芝生をリセット
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftL, shiftR, xor)
import Data.List (mapAccumL)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64, Word8)
import Numeric (showFFloat)

gw, gh :: Int
gw = 180                         -- ★ 芝生の格子（1 マス = 1 m とする）
gh = 110

nWalkers, stepsPerFrame :: Int
nWalkers = 140                   -- ★ 歩く人の数
stepsPerFrame = 2

trample, speedC :: Float
trample = 0.1                    -- ★ 1 歩で踏み固める強さ
speedC = 0.45                    -- ★ 1 ステップに歩くマス数

-- 歩く人：位置、目的地の入口、歩いた距離、出発点
data Walker = Walker { wkX, wkY :: !Float, wkTo :: !Int, wkWalked :: !Float, wkSx, wkSy :: !Float }

data World = World
  { wKappa   :: !Float               -- 踏み跡への引かれやすさ κ
  , wRegrow  :: !Float               -- 芝生が戻る時間（フレーム）
  , wG       :: !(U.Vector Float)    -- 踏み固め G（0〜1）
  , wV       :: !(U.Vector Float)    -- 見える踏み跡 V（G をぼかしたもの）
  , wWalkers :: ![Walker]
  , wGates   :: ![(Float, Float)]    -- 入口（格子の座標）
  , wTrips   :: ![Float]             -- 直近 60 回の遠回り率（新しい順）
  , wLenHist :: ![Float]             -- 道の面積の記録（新しい順）
  , wDetHist :: ![Float]             -- 遠回り率の記録（新しい順）
  , wNetLen  :: !Int
  , wDirect  :: !Float               -- 入口どうしを全部まっすぐ結んだ長さ
  , wSeed    :: !Seed
  }

world0 :: World
world0 = World 6 900 (U.replicate (gw * gh) 0) (U.replicate (gw * gh) 0) [] [] [] [] [] 0 0 1

main :: IO ()
main = runSketch (sketch "46-11 Paths made by walking" world0)
  { skBack  = hsb 95 30 12
  , skStart = \_ -> do
      s <- randomRIO (1, 2 ^ (62 :: Int))
      pure (defaultGates world0 { wSeed = s })
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

defaultGates :: World -> World
defaultGates w = resetWalkers w { wGates = [(6, 8), (g gw - 7, 14), (g gw - 10, g gh - 8), (12, g gh - 12)] }
  where g = fromIntegral

resetWalkers :: World -> World
resetWalkers w = w { wWalkers = ws, wSeed = s', wTrips = [], wLenHist = [], wDetHist = [], wDirect = direct }
  where
    gs = wGates w
    ng = length gs
    (s', ws) = mapAccumL (\s _ -> let (u, s1) = rand01 s
                                  in spawn gs (min (ng - 1) (floor (u * fromIntegral ng))) True s1) (wSeed w) [1 .. nWalkers]
    direct = sum [ dist2 a b | (i, a) <- zip [0 :: Int ..] gs, (j, b) <- zip [0 ..] gs, j > i ]

gateAt :: [(Float, Float)] -> Int -> (Float, Float)
gateAt gs i = case drop i gs of { (g : _) -> g; [] -> (0, 0) }

-- 入口 from から出発する人（scatter = 道の途中に置く）
spawn :: [(Float, Float)] -> Int -> Bool -> Seed -> (Seed, Walker)
spawn gs from scatter s0 = (s2, wk)
  where
    ng = length gs
    (u1, s1) = rand01 s0
    (u2, s2) = rand01 s1
    to0 = min (max 0 (ng - 2)) (floor (u1 * fromIntegral (ng - 1)))
    to = if ng < 2 then from else if to0 >= from then to0 + 1 else to0
    (fx, fy) = gateAt gs from
    (tx, ty) = gateAt gs to
    (x, y) = if scatter then (fx + (tx - fx) * u2, fy + (ty - fy) * u2) else (fx, fy)
    wk = Walker x y to 0 x y

idx :: Float -> Float -> Int
idx x y = (max 0 (min (gh - 1) (floor y))) * gw + max 0 (min (gw - 1) (floor x))

-- 見える踏み跡 V = 踏み固め G をぼかしたもの（遠くの道もぼんやり見える）。横と縦に半径 4 の平均
blurField :: U.Vector Float -> U.Vector Float
blurField g = vert
  where
    r = 4
    at' v x y = v U.! (max 0 (min (gh - 1) y) * gw + max 0 (min (gw - 1) x))
    horiz = U.generate (gw * gh) (\i -> let x = i `mod` gw; y = i `div` gw in sum [ at' g (x + d) y | d <- [-r .. r] ] / fromIntegral (2 * r + 1))
    vert = U.generate (gw * gh) (\i -> let x = i `mod` gw; y = i `div` gw in sum [ at' horiz x (y + d) | d <- [-r .. r] ] / fromIntegral (2 * r + 1))

-- ★ 1 人の 1 歩：目的地の向き ＋ κ × 踏み跡の坂（∇V）。返すのは (種, 遠回り率の記録, 踏んだマス) と新しい自分
walk :: World -> (Seed, [Float], [Int]) -> Walker -> ((Seed, [Float], [Int]), Walker)
walk w (s, trips, steps) wk
  | gd < 2 =                                                    -- 着いた：遠回り率を記録して、次の目的地へ
      let straight = sqrt ((x - wkSx wk) ^ (2 :: Int) + (y - wkSy wk) ^ (2 :: Int))
          trips' = if straight > 20 then take 60 (wkWalked wk / straight : trips) else trips
          (s', wk') = spawn (wGates w) (wkTo wk) False s
      in ((s', trips', steps), wk')
  | otherwise = ((s2, trips, idx x' y' : steps), wk { wkX = x', wkY = y', wkWalked = wkWalked wk + speedC })
  where
    x = wkX wk
    y = wkY wk
    (gtx, gty) = gateAt (wGates w) (wkTo wk)
    gx = gtx - x
    gy = gty - y
    gd = sqrt (gx * gx + gy * gy)
    v = wV w
    vx = (v U.! idx (x + 1) y - v U.! idx (x - 1) y) / 2
    vy = (v U.! idx x (y + 1) - v U.! idx x (y - 1)) / 2
    near = min 1 (gd / 10)                                      -- 目的地のすぐ近くでは、まっすぐ向かう
    tx0 = near * wKappa w * vx * 10
    ty0 = near * wKappa w * vy * 10
    tm = sqrt (tx0 * tx0 + ty0 * ty0)
    (tx, ty) = if tm > 0.85 then (tx0 * 0.85 / tm, ty0 * 0.85 / tm) else (tx0, ty0)   -- 踏み跡の引きは目的地の引きより弱く
    ddx = gx / gd + tx
    ddy = gy / gd + ty
    m = sqrt (ddx * ddx + ddy * ddy) + 1e-9
    (u1, s1) = rand01 s
    (u2, s2) = rand01 s1
    x' = clampF 0 (fromIntegral gw - 1) (x + ddx / m * speedC + (u1 - 0.5) * 0.15)
    y' = clampF 0 (fromIntegral gh - 1) (y + ddy / m * speedC + (u2 - 0.5) * 0.15)

step :: Env -> World -> World
step env w0 = record w3
  where
    w1 = if envFrame env `mod` 3 == 0 then w0 { wV = blurField (wG w0) } else w0
    oneStep w =
      let ((s, trips, steps), ws) = mapAccumL (walk w) (wSeed w, wTrips w, []) (wWalkers w)
          g' = U.accum (\gv _ -> gv + trample * (1 - gv)) (wG w) [ (i, ()) | i <- reverse steps ]   -- 踏み固める（1 まで）
      in w { wWalkers = ws, wSeed = s, wTrips = trips, wG = g' }
    w2 = iterate oneStep w1 !! stepsPerFrame
    decay = 1 / wRegrow w2
    g2 = U.map (\gv -> gv - gv * decay) (wG w2)                     -- 芝生が戻る
    w3 = w2 { wG = g2, wNetLen = U.length (U.filter (> 0.3) g2) }
    record w
      | envFrame env `mod` 10 /= 0 = w
      | otherwise = w { wLenHist = take 300 (fromIntegral (wNetLen w) : wLenHist w)
                      , wDetHist = take 300 (meanTrip w : wDetHist w) }

meanTrip :: World -> Float
meanTrip w = if null (wTrips w) then 1 else sum (wTrips w) / fromIntegral (length (wTrips w))

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 芝生（緑）と道（土色）
  [ Translate (ax + aw / 2) (ay + ah / 2) (Scale 1 (-1) (pixelImage gw gh kk pix))
  , Color (hsb 0 0 100) (Pictures [ Translate (ax + wkX wk * kk) (ay + wkY wk * kk) (disc 2) | wk <- wWalkers w ]) ]
  ++ concat [ [ dot (hsb 0 80 100) (ax + x * kk) (ay + y * kk) 18, txt (ax + x * kk - 5) (ay + y * kk + 6) 14 (hsb 0 0 100) [toEnum (65 + i)] ]
            | (i, (x, y)) <- zip [0 ..] (wGates w) ]
  -- ---- 右：数字とグラフ
  ++ [ drawPlot (wLenHist w) gx 120 gwid 90 0 (max (wDirect w * 5) 100) (hsb 30 70 100) ("Path area = " ++ show (wNetLen w) ++ " m^2")
     , drawPlot (wDetHist w) gx 250 gwid 90 1 1.5 (hsb 200 60 100) ("Detour ratio = " ++ fmt 3 det)
     , txt gx 380 14 (hsb 0 0 85) (show (length (wGates w)) ++ " gates joined straight: " ++ fmt 0 (wDirect w) ++ " m")
     , txt gx 404 14 (hsb 0 0 85) "Merged paths: less ground used,"
     , txt gx 428 14 (hsb 0 0 85) "but each walk detours a bit (>1)"
     , txt gx 466 14 (hsb 0 0 65) "heading = goal + kappa grad V"
     , txt gx 490 14 (hsb 0 0 65) ("walk: G += " ++ fmt 1 trample ++ " (1 - G)")
     , txt gx 514 14 (hsb 0 0 65) ("regrow: G *= 1 - 1/" ++ show (round (wRegrow w) :: Int))
     , txt 20 36 16 (hsb 0 0 100) ("Click: add gate   C: 4 gates   Up/Down: kappa = " ++ fmt 1 (wKappa w)
                                   ++ "   Left/Right: regrow time " ++ show (round (wRegrow w) :: Int) ++ "   R: reset grass")
     , txt 20 60 14 (hsb 0 0 70) "No one designs the paths. Footprints change how the next person walks = stigmergy (desire paths)" ]
  where
    (ax, ay, aw, ah) = area env
    kk = aw / fromIntegral gw
    gx = ax + aw + 30
    gwid = envW env - gx - 25
    det = case wDetHist w of { (d : _) -> d; [] -> 1 }
    g = wG w
    pix x y = let i = y * gw + x
                  v = min 1 (g U.! i * 1.6)
              in (b8 (60 + 140 * v), b8 (120 + 40 * v - (if i `mod` 7 == 0 then 10 else 0)), b8 (50 + 70 * v))

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ (mx0, my0)
    | mx >= ax && mx <= ax + aw && my >= ay && my <= ay + ah && length (wGates w) < 8 ->
        resetWalkers w { wGates = wGates w ++ [((mx - ax) / aw * fromIntegral gw, (my - ay) / ah * fromIntegral gh)] }
    where
      (ax, ay, aw, ah) = area env
      mx = mx0 + envW env / 2
      my = envH env / 2 - my0
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wKappa = min 20 (wKappa w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wKappa = max 0 (wKappa w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wRegrow = min 3000 (wRegrow w + 150) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wRegrow = max 150 (wRegrow w - 150) }
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> defaultGates w
    | c `elem` "rR" -> resetWalkers w { wG = U.replicate (gw * gh) 0, wV = U.replicate (gw * gh) 0 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 最初はまっすぐな線（入口どうしの直線）がたくさんできる。時間がたつと、何本かがまとまって Y 字の分かれ道になる？
-- 2. κ = 0 にすると、みんな直線で歩く（遠回り率 1.0）。道の総延長はどうなる？ κ = 15 だと？
-- 3. ← で芝生が早く戻るようにすると、あまり使われない道は消える。どの道が残る？
-- 4. 入口を三角形に 3 つだけ置く（C のあと R、クリック）と、真ん中で 3 本が合流する形（シュタイナー点）ができる？
-- 5. 自分の大学で、芝生に「けもの道」ができている所を探してみよう。設計した道と比べると？

-- ---------- 道具 ----------
-- 格子を描く場所 (左, 上, 幅, 高さ)
area :: Env -> (Float, Float, Float, Float)
area env = let w = min (envW env * 0.7) ((envH env - 110) * fromIntegral gw / fromIntegral gh)
           in (20, 85, w, w * fromIntegral gh / fromIntegral gw)

b8 :: Float -> Word8
b8 v = fromIntegral (max 0 (min 255 (round v :: Int)))

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

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

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
