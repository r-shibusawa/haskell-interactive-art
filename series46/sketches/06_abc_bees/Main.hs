-- ============================================
-- 46-06 人工蜂コロニー（ABC）─ 働き蜂・追従蜂・偵察蜂の 3 役
-- エサ場（花）1 つに働き蜂 1 匹。働き蜂は近くを少し探して、よければ移る。巣で待つ追従蜂は「良いエサ場ほど高い確率」で選んでそこを探す
-- limit 回続けて良くならないエサ場は放棄され、偵察蜂がランダムな新しい場所を探しに行く。花の大きさ = エサ場の質、× = 放棄した場所
-- 操作：1〜3 = 関数（Rastrigin・Schwefel・Ackley）/ ↑↓ = limit（あきらめるまでの回数）/ ←→ = 速さ / R = やり直し / スペース = 一時停止
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftL, shiftR, xor)
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)
import Numeric (showEFloat, showFFloat)

numFood :: Int
numFood = 20                 -- ★ エサ場の数（＝働き蜂の数＝追従蜂の数）

-- 関数：名前、範囲、式、最小値の場所
data Fn = Fn { fnName :: String, fnLo, fnHi :: !Double, fnF :: Double -> Double -> Double, fnOpt :: (Double, Double) }

funcs :: [Fn]
funcs =
  [ Fn "Rastrigin (many holes)" (-5.12) 5.12 (\x y -> 20 + x * x - 10 * cos (2 * pi * x) + y * y - 10 * cos (2 * pi * y)) (0, 0)
  , Fn "Schwefel (best hole in a corner)" (-500) 500 (\x y -> 837.9658 - x * sin (sqrt (abs x)) - y * sin (sqrt (abs y))) (420.9687, 420.9687)
  , Fn "Ackley (flat plateau, deep hole)" (-5) 5
       (\x y -> -20 * exp (-0.2 * sqrt (0.5 * (x * x + y * y))) - exp (0.5 * (cos (2 * pi * x) + cos (2 * pi * y))) + exp 1 + 20) (0, 0) ]

-- エサ場：場所、値、良くならなかった回数
data Food = Food { fx, fy, fv :: !Double, fTrial :: !Int }

data Ghost = Ghost !Double !Double !Float        -- 放棄した場所と残りの寿命
data Scout = Scout !Double !Double !Double !Double   -- 偵察蜂：放棄した場所 → 新しい場所

data World = World
  { wFnId     :: !Int
  , wField    :: !Picture         -- 等高線の絵（関数を変えたときだけ作り直す）
  , wLimit    :: !Int             -- あきらめるまでの回数
  , wFpc      :: !Int             -- 1 サイクルを何フレームで見せるか
  , wFoods    :: !(Seq.Seq Food)
  , wBestVal  :: !Double
  , wBestX, wBestY :: !Double
  , wCycle    :: !Int
  , wAbandon  :: !Int
  , wHist     :: ![Float]         -- log10 最良値の記録（新しい順、300 個まで）
  , wOnlook   :: ![Int]           -- このサイクルに追従蜂が選んだエサ場
  , wScouts   :: ![Scout]
  , wGhosts   :: ![Ghost]
  , wPaused   :: !Bool
  , wSeed     :: !Seed
  }

world0 :: World
world0 = World 1 Blank 30 12 Seq.empty inf 0 0 0 0 [] [] [] [] False 1

inf :: Double
inf = 1 / 0

main :: IO ()
main = runSketch (sketch "46-06 Artificial bee colony" world0)
  { skBack  = hsb 30 30 12
  , skStart = \_ -> do
      s <- randomRIO (1, 2 ^ (62 :: Int))
      pure (chooseFn 1 world0 { wSeed = s })
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

curFn :: World -> Fn
curFn w = case drop (wFnId w - 1) funcs of
  (f : _) -> f
  [] -> Fn "sphere" (-5) 5 (\x y -> x * x + y * y) (0, 0)

evalF :: Fn -> Double -> Double -> Double
evalF fn x y = let v = fnF fn x y in if isNaN v || isInfinite v then 1e9 else max v 0

fitness :: Double -> Double
fitness f = 1 / (1 + f)

-- 乱数を 1 つ取り出して種を進める
rnd :: Seed -> (Double, Seed)
rnd s = (realToFrac (hashRand s 0), nextSeed s)

randomFood :: Fn -> Seed -> (Food, Seed)
randomFood fn s0 = (Food x y (evalF fn x y) 0, s2)
  where
    (a, s1) = rnd s0
    (b, s2) = rnd s1
    x = fnLo fn + a * (fnHi fn - fnLo fn)
    y = fnLo fn + b * (fnHi fn - fnLo fn)

chooseFn :: Int -> World -> World
chooseFn i w = let w' = w { wFnId = i } in restart w' { wField = makeField (curFn w') }

restart :: World -> World
restart w = memorize w { wFoods = Seq.fromList fs, wSeed = s', wBestVal = inf, wCycle = 0, wAbandon = 0, wHist = []
                       , wOnlook = [], wScouts = [], wGhosts = [] }
  where
    (fs, s') = foldl' (\(acc, s) _ -> let (f, s1) = randomFood (curFn w) s in (acc ++ [f], s1)) ([], wSeed w) [1 .. numFood]

memorize :: World -> World
memorize w = foldl' (\v s -> if fv s < wBestVal v then v { wBestVal = fv s, wBestX = fx s, wBestY = fy s } else v) w (toList (wFoods w))

-- ★ エサ場 i の近くを探す：v = x ＋ φ (x − x_k)（ほかのエサ場 k との差を使う）。良ければ移る（貪欲）
tryNeighbour :: Fn -> Int -> (Seq.Seq Food, Seed) -> (Seq.Seq Food, Seed)
tryNeighbour fn i (foods, s0) = (Seq.update i new foods, s3)
  where
    (a, s1) = rnd s0
    (b, s2) = rnd s1
    (c, s3) = rnd s2
    k0 = min (numFood - 2) (floor (a * fromIntegral (numFood - 1)))
    k = if k0 >= i then k0 + 1 else k0
    sf = Seq.index foods i
    other = Seq.index foods k
    phi = b * 2 - 1
    (nx, ny) = if c < 0.5 then (clampD (fnLo fn) (fnHi fn) (fx sf + phi * (fx sf - fx other)), fy sf)
                          else (fx sf, clampD (fnLo fn) (fnHi fn) (fy sf + phi * (fy sf - fy other)))
    nv = evalF fn nx ny
    new = if nv < fv sf then Food nx ny nv 0 else sf { fTrial = fTrial sf + 1 }

-- ★ ABC の 1 サイクル：働き蜂 → 追従蜂 → 偵察蜂
abcCycle :: World -> World
abcCycle w = w4 { wCycle = wCycle w + 1, wHist = take 300 (realToFrac (logBase 10 (max (wBestVal w4) 1e-12)) : wHist w4) }
  where
    fn = curFn w
    st1 = foldl' (\acc i -> tryNeighbour fn i acc) (wFoods w, wSeed w) [0 .. numFood - 1]       -- 働き蜂
    fits = map (fitness . fv) (toList (fst st1))
    total = sum fits
    roulette r i (f : fs) | i < numFood - 1 && r > f = roulette (r - f) (i + 1) fs
    roulette _ i _ = i
    (st2, onl) = foldl' (\((fs, s), ol) _ ->                                                     -- 追従蜂：ルーレットで選ぶ
                           let (r, s1) = rnd s
                               i = roulette (r * total) 0 fits
                           in (tryNeighbour fn i (fs, s1), ol ++ [i]))
                        (st1, []) [1 .. numFood]
    w2 = memorize w { wFoods = fst st2, wSeed = snd st2, wOnlook = onl, wScouts = [] }
    foodsL = zip [0 :: Int ..] (toList (wFoods w2))
    worst = foldl' (\b (i, f) -> if fTrial f > wLimit w2 && maybe True (\(_, g) -> fTrial f > fTrial g) b then Just (i, f) else b) Nothing foodsL
    w4 = case worst of                                                                          -- 偵察蜂：1 サイクルに 1 匹まで
      Nothing -> w2
      Just (i, f) ->
        let (nf0, s') = randomFood fn (wSeed w2)
        in memorize w2 { wGhosts = Ghost (fx f) (fy f) 1 : wGhosts w2, wScouts = [Scout (fx f) (fy f) (fx nf0) (fy nf0)]
                       , wFoods = Seq.update i nf0 (wFoods w2), wAbandon = wAbandon w2 + 1, wSeed = s' }

step :: Env -> World -> World
step env w0 = w1 { wGhosts = [ Ghost x y (l - 0.01) | Ghost x y l <- wGhosts w1, l - 0.01 > 0 ] }
  where
    w1 = if not (wPaused w0) && envFrame env `mod` wFpc w0 == 0 then abcCycle w0 else w0

draw :: Env -> World -> Picture
draw env w = view env $
  [ Translate (ox + sz / 2) (oy + sz / 2) (Scale (sz / 220) (-(sz / 220)) (wField w))
  , seg 2 (hsb 120 70 100) (sx ox' - 8, sy oy') (sx ox' + 8, sy oy'), seg 2 (hsb 120 70 100) (sx ox', sy oy' - 8) (sx ox', sy oy' + 8) ]
  -- 放棄した場所（×）
  ++ [ Color (hsbA 0 0 100 (l * 80)) (Pictures [ thickLine 2 (x - 6, y - 6) (x + 6, y + 6), thickLine 2 (x - 6, y + 6) (x + 6, y - 6) ])
     | Ghost gx0 gy0 l <- wGhosts w, let x = sx gx0, let y = sy gy0 ]
  -- 花（エサ場）：大きさ = 質。色 = あきらめ度（trial / limit）
  ++ concat [ [ Color (hsbA (wrapF 360 (330 + 80 * min 1 (fromIntegral (fTrial f) / fromIntegral (wLimit w)))) 60 100 85)
                      (Pictures [ Translate (x + cos (fromIntegral p * tau / 5) * r * 0.45) (y + sin (fromIntegral p * tau / 5) * r * 0.45) (disc (r * 0.3)) | p <- [0 .. 4 :: Int] ])
              , dot (hsb 50 90 100) x y (r * 0.4) ]
            | f <- foods, let q = realToFrac (fitness (fv f) / fmax), let x = sx (fx f), let y = sy (fy f), let r = 8 + 18 * q ]
  -- 巣と蜂
  ++ [ Color (hsb 40 80 90) (rect hx (hy - 5) 44 18) ]
  ++ [ dot (hsb 50 90 100) (lerpF hx (sx (fx f)) e + sin (fromIntegral (envFrame env) * 0.7 + fromIntegral i) * 3) (lerpF hy (sy (fy f)) e) 5
     | i <- wOnlook w, f <- take 1 (drop i foods) ]
  ++ [ dot (hsb 190 80 100) (lerpF (sx a) (sx c) tCyc) (lerpF (sy b) (sy d) tCyc) 8 | Scout a b c d <- wScouts w ]
  ++ [ dot (hsb 0 90 100) (sx (wBestX w)) (sy (wBestY w)) 10 ]
  -- ---- 右：数字とグラフ
  ++ [ txt gx (110 + fromIntegral i * 26) 16 (gray 1) l | (i, l) <- zip [0 :: Int ..] info ]
  ++ [ drawPlot (wHist w) gx 280 gw 160 (-12) 3 (hsb 40 85 100) "Best value (vertical: log10 f)"
     , txt gx 480 14 (hsb 50 90 100) "yellow dots = onlookers (better flowers get more)"
     , txt gx 502 14 (hsb 190 80 100) "cyan = scouts (abandoned flower -> random place)"
     , txt gx 524 14 (hsb 0 0 80) "flower colour: pink = improved, yellow = near limit"
     , txt gx 546 14 (hsb 0 0 80) "one employed bee per flower (at the flower itself)"
     , txt 20 36 17 (gray 1) ("1-3: function   Up/Down: limit = " ++ show (wLimit w) ++ "   Left/Right: speed (" ++ show (wFpc w)
                              ++ " frames / cycle)   R: restart   Space: " ++ (if wPaused w then "paused" else "running"))
     , txt 20 60 14 (hsb 0 0 70) "Employed: search near own flower -> Onlookers: pick good flowers by chance -> Scouts: drop a flower after limit failures" ]
  where
    fn = curFn w
    (ox', oy') = fnOpt fn
    foods = toList (wFoods w)
    fmax = foldl' max 1e-12 (map (fitness . fv) foods)
    tCyc = fromIntegral (envFrame env `mod` wFpc w) / fromIntegral (wFpc w)
    e = if tCyc < 0.5 then tCyc * 2 else 2 - tCyc * 2
    sz = min (envH env - 110) (envW env * 0.58)
    ox = 30
    oy = 90
    sx x = ox + realToFrac ((x - fnLo fn) / (fnHi fn - fnLo fn)) * sz
    sy y = oy + sz - realToFrac ((y - fnLo fn) / (fnHi fn - fnLo fn)) * sz
    hx = ox + sz / 2
    hy = oy + sz + 2
    gx = ox + sz + 40
    gw = envW env - gx - 30
    info = [ "Function " ++ show (wFnId w) ++ ": " ++ fnName fn
           , "Cycle " ++ show (wCycle w) ++ " (" ++ show numFood ++ " flowers, limit = " ++ show (wLimit w) ++ ")"
           , "Best value = " ++ fmtV (wBestVal w) ++ " (true min 0 at green +)"
           , "Best place = (" ++ fmtD 3 (wBestX w) ++ ", " ++ fmtD 3 (wBestY w) ++ ")"
           , "Abandoned and scouted: " ++ show (wAbandon w) ++ " times" ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "123" -> chooseFn (fromEnum c - fromEnum '0') w
    | c `elem` "rR" -> restart w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wLimit = min 200 (wLimit w + 5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wLimit = max 5 (wLimit w - 5) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wFpc = max 1 (wFpc w - 2) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wFpc = min 40 (wFpc w + 2) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 2（Schwefel）は一番深い穴が右上の隅にある。ABC は見つけられる？ PSO（05）でも試して比べよう
-- 2. limit を 5 にすると偵察が増えて「広く浅く」、200 にすると偵察がほぼなくなり「狭く深く」。どちらが速く 0 に近づく？
-- 3. 追従蜂がどの花に集まるか見よう。良い花に集中しすぎると何が困る？
-- 4. numFood を 5 と 50 で比べる。同じサイクル数での最良値は？（数が多いと 1 サイクルの計算も多い）
-- 5. 本物のミツバチは 8 の字ダンスで良い花を教える（12 で詳しく）。ABC の「ルーレットで選ぶ」がダンスの代わり

-- ---------- 道具 ----------
-- 等高線の絵（220 × 220）：値の log をとって色にし、14 段ごとに暗い線を入れる
makeField :: Fn -> Picture
makeField fn = pixelImage nf nf 1 pix
  where
    nf = 220
    sp = fnHi fn - fnLo fn
    vals = U.generate (nf * nf) (\k -> let (j, i) = k `divMod` nf
                                           x = fnLo fn + (fromIntegral i + 0.5) / fromIntegral nf * sp
                                           y = fnHi fn - (fromIntegral j + 0.5) / fromIntegral nf * sp
                                       in realToFrac (log (1 + evalF fn x y)) :: Float)
    vmax = max 1e-12 (U.maximum vals)
    pix i j =
      let t = vals U.! (j * nf + i) / vmax
          band = if fract (t * 14) < 0.08 then 0.75 else 1
      in (q ((30 + 120 * t) * band), q ((60 + 70 * (1 - t)) * band), q ((40 + 40 * t) * band))
    q v = fromIntegral (max 0 (min 255 (round v :: Int)))

clampD :: Double -> Double -> Double -> Double
clampD lo hi = max lo . min hi

-- 小さい値は 1.23e-5 のように書く
fmtV :: Double -> String
fmtV v
  | isInfinite v = "-"
  | v < 1e-4 = showEFloat (Just 2) v ""
  | otherwise = fmtD 4 v

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

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmtD :: Int -> Double -> String
fmtD n v = showFFloat (Just n) v ""

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
