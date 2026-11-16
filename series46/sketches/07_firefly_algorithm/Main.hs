-- ============================================
-- 46-07 ホタルアルゴリズム ─ 明るい仲間に引き寄せられる
-- 関数の値が小さい場所にいるホタルほど明るい。暗いホタルは、自分より明るい全員に引き寄せられる
-- 引き寄せの強さ β = β0 · e^{−γ r²}。光は距離 r で弱まるので、γ が大きいと「近くの明るい仲間」にしか寄らない → 谷ごとに小さな群れができ、複数の答えを同時に見つける
-- 操作：1〜2 = 関数（Himmelblau・Rastrigin）/ ↑↓ = 吸収係数 γ / ←→ = ランダムさ α / R = やり直し / F = 速さ / スペース = 一時停止
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftL, shiftR, xor)
import Data.Foldable (toList)
import Data.List (elemIndex, foldl', sort)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)
import Numeric (showEFloat, showFFloat)

numF :: Int
numF = 40                    -- ★ ホタルの数

beta0 :: Double
beta0 = 1.0                  -- ★ 距離 0 での引き寄せの強さ

-- 関数：名前、範囲、式、最小値の場所
data Fn = Fn { fnName :: String, fnLo, fnHi :: !Double, fnF :: Double -> Double -> Double, fnOpt :: [(Double, Double)] }

funcs :: [Fn]
funcs =
  [ Fn "Himmelblau (4 valleys, all 0)" (-5) 5 (\x y -> (x * x + y - 11) ^ (2 :: Int) + (x + y * y - 7) ^ (2 :: Int))
       [(3, 2), (-2.805118, 3.131312), (-3.77931, -3.283186), (3.584428, -1.848126)]
  , Fn "Rastrigin (bottom at the centre)" (-5.12) 5.12
       (\x y -> 20 + x * x - 10 * cos (2 * pi * x) + y * y - 10 * cos (2 * pi * y)) [(0, 0)] ]

-- ホタル：場所と値
data Fly = Fly { qx, qy, qf :: !Double }

data World = World
  { wFnId    :: !Int
  , wField   :: !Picture         -- 等高線の絵（関数を変えたときだけ作り直す）
  , wGamma   :: !Double          -- 吸収係数 γ（大きいほど光が届かない）
  , wAlphaR  :: !Double          -- ランダムな動きの大きさ α（毎回 0.97 倍に小さくなる）
  , wAlphaNow :: !Double
  , wFpi     :: !Int             -- 何フレームに 1 回更新するか（F で切替）
  , wFlies   :: !(Seq.Seq Fly)
  , wBestVal :: !Double
  , wBestX, wBestY :: !Double
  , wIter    :: !Int
  , wFound   :: !Int             -- 見つけた谷底の数
  , wHist    :: ![Float]         -- log10 最良値の記録（新しい順、300 個まで）
  , wPaused  :: !Bool
  , wSeed    :: !Seed
  }

world0 :: World
world0 = World 1 Blank 1 0.2 0.2 4 Seq.empty inf 0 0 0 0 [] False 1

inf :: Double
inf = 1 / 0

main :: IO ()
main = runSketch (sketch "46-07 Firefly algorithm" world0)
  { skBack  = hsb 240 45 9
  , skStart = \_ -> do
      s <- randomRIO (1, 2 ^ (62 :: Int))
      pure (chooseFn 1 world0 { wSeed = s })
  , skStep  = \env _ w -> pure (if not (wPaused w) && envFrame env `mod` wFpi w == 0 then fireflyIteration w else w)
  , skDraw  = draw
  , skEvent = event
  }

curFn :: World -> Fn
curFn w = case drop (wFnId w - 1) funcs of
  (f : _) -> f
  [] -> Fn "sphere" (-5) 5 (\x y -> x * x + y * y) [(0, 0)]

evalF :: Fn -> Double -> Double -> Double
evalF fn x y = let v = fnF fn x y in if isNaN v || isInfinite v then 1e9 else v

-- 乱数を 1 つ取り出して種を進める
rnd :: Seed -> (Double, Seed)
rnd s = (realToFrac (hashRand s 0), nextSeed s)

chooseFn :: Int -> World -> World
chooseFn i w = let w' = w { wFnId = i } in restart w' { wField = makeField (curFn w') }

restart :: World -> World
restart w = memorize w { wFlies = Seq.fromList fs, wSeed = s', wBestVal = inf, wIter = 0, wHist = [], wAlphaNow = wAlphaR w }
  where
    fn = curFn w
    sp = fnHi fn - fnLo fn
    (fs, s') = foldl' (\(acc, s) _ -> let (a, s1) = rnd s
                                          (b, s2) = rnd s1
                                          x = fnLo fn + a * sp
                                          y = fnLo fn + b * sp
                                      in (acc ++ [Fly x y (evalF fn x y)], s2)) ([], wSeed w) [1 .. numF]

memorize :: World -> World
memorize w = foldl' (\v p -> if qf p < wBestVal v then v { wBestVal = qf p, wBestX = qx p, wBestY = qy p } else v) w (toList (wFlies w))

-- 谷底 o の近く（0.1 以内）に、値が 0.01 より小さいホタルがいるか
hit :: [Fly] -> (Double, Double) -> Bool
hit fs (ox, oy) = any (\p -> sqrt ((qx p - ox) ^ (2 :: Int) + (qy p - oy) ^ (2 :: Int)) < 0.1 && qf p < 0.01) fs

-- ★ ホタルアルゴリズムの 1 回分：i より明るい j があれば、そのたびに j の方へ動く（ヤンの元の形）。ランダムな一歩は 1 回に 1 度
fireflyIteration :: World -> World
fireflyIteration w = w2 { wFound = length (filter (hit (toList (wFlies w2))) (fnOpt fn)), wIter = wIter w + 1
                        , wHist = take 300 (realToFrac (logBase 10 (max (wBestVal w2) 1e-12)) : wHist w2) }
  where
    fn = curFn w
    sp = fnHi fn - fnLo fn
    an = wAlphaNow w
    cl = clampD (fnLo fn) (fnHi fn)
    (flies', seed') = foldl' moveOne (wFlies w, wSeed w) [0 .. numF - 1]
    moveOne (fs, s0) i =
      let p0 = Seq.index fs i
          -- 自分より明るい（値が小さい）相手だけに寄る。距離は箱の大きさ = 1 で測り、γ は「箱の 1/10」を 1 とした単位
          pullTo (x, y, pull) j =
            let q = Seq.index fs j
                dx = (qx q - x) / sp
                dy = (qy q - y) / sp
                b = beta0 * exp (-(wGamma w) * 100 * (dx * dx + dy * dy))
            in if j == i || qf q >= qf p0 then (x, y, pull) else (x + b * (qx q - x), y + b * (qy q - y), pull + b)
          (x1, y1, pull1) = foldl' pullTo (qx p0, qy p0, 0) [0 .. numF - 1]
          (a, s1) = rnd s0
          (b, s2) = rnd s1
          p'
            | pull1 > 0.02 =                    -- 引き寄せられたホタル：ランダムな一歩を足して移る
                let nx = cl (x1 + an * sp * (a - 0.5))
                    ny = cl (y1 + an * sp * (b - 0.5))
                in Fly nx ny (evalF fn nx ny)
            | otherwise =                       -- 近くに明るい仲間がいない＝その場のリーダー：小さくゆらいで、良ければ移る
                let nx = cl (x1 + an * sp * 0.2 * (a - 0.5))
                    ny = cl (y1 + an * sp * 0.2 * (b - 0.5))
                    nv = evalF fn nx ny
                in if nv < qf p0 then Fly nx ny nv else p0 { qx = x1, qy = y1 }
      in (Seq.update i p' fs, s2)
    w2 = memorize w { wFlies = flies', wSeed = seed', wAlphaNow = an * 0.97 }

draw :: Env -> World -> Picture
draw env w = view env $
  [ Translate (ox + sz / 2) (oy + sz / 2) (Scale (sz / 220) (-(sz / 220)) (wField w)) ]
  ++ [ Color (hsb (if hit flies o then 120 else 0) 70 100) (Translate (sx (fst o)) (sy (snd o)) (ring 13 2)) | o <- fnOpt fn ]
  -- ホタル：明るさ = 値の小ささ（順位）
  ++ concat [ [ dot (hsbA 60 80 100 (15 + 25 * g)) x y (10 + 26 * g), dot (hsb 60 (40 + 50 * g) (40 + 60 * g)) x y 6 ]
            | p <- flies, let rank = maybe 0 fromIntegral (elemIndex (qf p) sorted) / fromIntegral numF
            , let g = 1 - rank, let x = sx (qx p), let y = sy (qy p) ]
  -- 光の届く範囲（β が β0 の半分になる距離）
  ++ [ Color (hsbA 60 60 100 40) (Translate (sx (wBestX w)) (sy (wBestY w)) (Circle rHalf)) | rHalf < sz ]
  -- ---- 右：数字とグラフ
  ++ [ txt gx (110 + fromIntegral i * 26) 16 (gray 1) l | (i, l) <- zip [0 :: Int ..] info ]
  ++ [ drawPlot (wHist w) gx 280 gw 160 (-12) 3 (hsb 60 85 100) "Best value (vertical: log10 f)"
     , txt gx 480 14 (hsb 0 0 75) "x_i <- x_i + beta0 e^(-gamma r^2) (x_j - x_i)"
     , txt (gx + 96) 500 14 (hsb 0 0 75) "+ alpha (rand - 1/2)"
     , txt gx 528 14 (hsb 0 0 75) "small gamma: light reaches all -> one swarm (like PSO)"
     , txt gx 550 14 (hsb 0 0 75) "large gamma: sees only near mates -> swarm per valley"
     , txt 20 36 17 (gray 1) ("1-2: function   Up/Down: gamma = " ++ fmtD 3 (wGamma w) ++ "   Left/Right: alpha = "
                              ++ fmtD 2 (wAlphaR w) ++ "   R: restart   F: speed   Space: " ++ (if wPaused w then "paused" else "running"))
     , txt 20 60 14 (hsb 0 0 70) "Move toward brighter (lower) mates. Change gamma: find one valley, or all 4 at once?" ]
  where
    fn = curFn w
    flies = toList (wFlies w)
    sorted = sort (map qf flies)
    halfD = sqrt (log 2 / (wGamma w * 100))
    rHalf = realToFrac halfD * sz
    sz = min (envH env - 110) (envW env * 0.58)
    ox = 30
    oy = 90
    sx x = ox + realToFrac ((x - fnLo fn) / (fnHi fn - fnLo fn)) * sz
    sy y = oy + sz - realToFrac ((y - fnLo fn) / (fnHi fn - fnLo fn)) * sz
    gx = ox + sz + 40
    gw = envW env - gx - 30
    info = [ "Function " ++ show (wFnId w) ++ ": " ++ fnName fn
           , "Iteration " ++ show (wIter w) ++ " (" ++ show numF ++ " fireflies, alpha = " ++ fmtD 3 (wAlphaNow w) ++ ")"
           , "Best value = " ++ fmtV (wBestVal w) ++ " (true minimum 0)"
           , "Valleys found: " ++ show (wFound w) ++ " / " ++ show (length (fnOpt fn)) ++ " (green circles)"
           , "Light halves at " ++ fmtD 1 (halfD * 100) ++ " % of the box" ]

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c `elem` "12" -> chooseFn (fromEnum c - fromEnum '0') w
    | c `elem` "fF" -> w { wFpi = if wFpi w == 4 then 1 else 4 }
    | c `elem` "rR" -> restart w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wGamma = min 64 (wGamma w * 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wGamma = max 0.001 (wGamma w / 2) }
  EventKey (SpecialKey KeyRight) Down _ _ -> let a = min 1 (wAlphaR w + 0.05) in w { wAlphaR = a, wAlphaNow = a }
  EventKey (SpecialKey KeyLeft) Down _ _ -> let a = max 0 (wAlphaR w - 0.05) in w { wAlphaR = a, wAlphaNow = a }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. Himmelblau で γ = 0.001（↓ を何回か）にすると全員が 1 つの谷へ。γ = 1 だと 4 つの谷に分かれる？ 見つけた谷底の数を比べよう
-- 2. γ = 64 にすると光がほとんど届かず、ホタルは自分の近くしか見えない。ランダムに歩くだけに近くなる
-- 3. α = 0 にすると？ ランダムさがないと、最初に明るかったホタルの所で止まってしまう
-- 4. 2（Rastrigin）は穴が規則正しく並ぶ。γ が大きいとたくさんの穴に分かれて止まる。それでも中央は見つかる？
-- 5. 「複数の答えを同時に見つける」と何がうれしい？（第 1 候補がダメなときの予備、設計の選択肢）

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
          band = if fract (t * 14) < 0.08 then 0.7 else 1
      in (q ((20 + 60 * t) * band), q ((30 + 40 * t) * band), q ((60 + 90 * t) * band))
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
