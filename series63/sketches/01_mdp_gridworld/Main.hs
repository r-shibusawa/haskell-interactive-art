-- ============================================
-- 63-01 マルコフ決定過程とグリッドワールド ─ 状態・行動・報酬・割引率・すべりやすさ
-- マス = 状態 s、上下左右 = 行動 a。意図した向きに進むのは 1 − slip の確率、残りは左右の直角へすべる。
-- でたらめに動くエージェントの「割引つき報酬の合計」G = r1 + γ r2 + γ² r3 + … を集めると、その平均が状態の価値 V(S) になる
-- 操作：クリック マスを 床→壁→宝→穴 と切りかえ / ↑↓ 割引率 γ / ← → すべりやすさ / C 1 歩ごとの報酬 / F 速さ / スペース やり直し
-- マウスを乗せたマスの「行き先の確率」を表示
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad.ST (runST)
import Control.Exception (evaluate)
import Control.Monad (foldM, replicateM)
import Data.Bits (shiftR, xor, (.|.))
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word32)
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

mapRows :: [String]
mapRows =                              -- ★ 地図（. 床、# 壁、S スタート、G 宝 +1、P 落とし穴 −1）
  [ ".........."
  , ".....#...."
  , ".........."
  , "S........G"
  , ".PPPPPPPP."
  , ".........." ]

stepCosts :: [Double]
stepCosts = [0, -0.04, -0.1]           -- ★ 1 歩ごとの報酬の候補（C で切りかえ）

data World = World
  { wGW      :: !GW
  , wGamma   :: !Double                -- ★ 割引率 γ
  , wSlip    :: !Double                -- ★ すべりやすさ
  , wCostIdx :: !Int
  , wMdp     :: !Mdp
  , wStart   :: !Int
  , wVRand   :: !(U.Vector Double)     -- でたらめ方策の価値
  , wFast    :: !Bool
  , wPos     :: !Int                   -- エージェントのいるマス
  , wEpRet   :: !Double                -- このエピソードの G
  , wEpDisc  :: !Double                -- γ^t
  , wEpLen   :: !Int
  , wReturns :: !(Seq.Seq Double)      -- 集めた G（最新 3000 回）
  , wTrail   :: !(Seq.Seq Int)         -- 足あと（40 マス）
  , wRng     :: !Rng
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "63-01 MDP and gridworld" (setup []))
  { skBack  = bgC
  , skStart = \_ -> do
      let rs = selfTest
      printSelfTest rs
      pure (setup rs)
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

setup :: [(String, Bool)] -> World
setup rs = rebuild w0
  where gw = gridParse mapRows
        w0 = World gw 0.9 0.2 1 (buildMdp gw 0.2 (-0.04)) 0 (U.replicate 1 0) False 0 0 1 0 Seq.empty Seq.empty (seedRng 11) rs

-- 地図や γ・slip が変わったら作り直す
rebuild :: World -> World
rebuild w = newEpisode w { wMdp = m, wStart = st, wVRand = fst (evalRandomPolicy m (wGamma w) 1e-10), wReturns = Seq.empty }
  where m = buildMdp (wGW w) (wSlip w) (idx 0 stepCosts (wCostIdx w))
        st = maybe 0 id (U.elemIndex 'S' (gwCell (wGW w)))

newEpisode :: World -> World
newEpisode w = w { wPos = wStart w, wEpRet = 0, wEpDisc = 1, wEpLen = 0, wTrail = Seq.singleton (wStart w) }

-- ★ でたらめな方策（4 方向を 1/4 ずつ）の価値：V(s) = Σ_a ¼ Σ_s' p (r + γ V(s')) を収束するまでくり返す（その場で書きかえる）
evalRandomPolicy :: Mdp -> Double -> Double -> (U.Vector Double, Int)
evalRandomPolicy m gam tol = runST $ do
  v <- UM.replicate (mdpNS m) 0
  let qv s a = foldM (\q o -> do x <- UM.read v (trS2 o); pure (q + trP o * (trR o + gam * x))) 0 (trans m s a)
      sweep = foldM (\d s -> if isTerm m s then pure d else do
                        x <- foldM (\acc a -> do q <- qv s a; pure (acc + 0.25 * q)) 0 [0 .. 3]
                        old <- UM.read v s
                        UM.write v s x
                        pure (max d (abs (x - old)))) 0 [0 .. mdpNS m - 1]
      loop !n d | d >= tol && n < 20000 = do d' <- sweep; loop (n + 1) d'
                | otherwise = pure n
  n <- loop (0 :: Int) posInf
  v' <- U.freeze v
  pure (v', n)

-- ★ 環境の 1 歩：確率 P[s][a] にしたがって次のマスを 1 つ選ぶ
sampleStep :: Mdp -> Int -> Int -> R Tr
sampleStep m s a = do
  u <- rnd
  let outs = trans m s a
      go _ [o] = o
      go c (o : os) = let c' = c + trP o in if u < c' then o else go c' os
      go _ [] = Tr s 1 0
  pure (go 0 outs)

agentStep :: World -> World
agentStep w
  | isTerm (wMdp w) pos' || len' >= 300 = newEpisode w' { wReturns = pushCap 3000 ret' (wReturns w) }
  | otherwise = w'
  where
    ((_, o), g) = runWith (wRng w) (do u <- rnd; let a = floor (u * 4) in (,) a <$> sampleStep (wMdp w) (wPos w) a)
    ret' = wEpRet w + wEpDisc w * trR o
    pos' = trS2 o
    len' = wEpLen w + 1
    tr = wTrail w Seq.|> pos'
    w' = w { wRng = g, wEpRet = ret', wEpDisc = wEpDisc w * wGamma w, wEpLen = len', wPos = pos'
           , wTrail = if Seq.length tr > 40 then Seq.drop 1 tr else tr }

step :: Env -> Float -> World -> IO World
step env _ w
  | wFast w = sliced 5 (const True) agentStep w
  | envFrame env `mod` 6 == 0 = pure (agentStep w)
  | otherwise = pure w

-- マスの大きさと置き場所
gx0, gy0 :: Float
gx0 = 40
gy0 = 110

cellSize :: Env -> GW -> Float
cellSize env gw = minimum [64, (envW env * 0.55 - 60) / fromIntegral (gwW gw), (envH env - 260) / fromIntegral (gwH gw)]

-- r の表示（JS の数の書き方）
showR :: Double -> String
showR = prec3

draw :: Env -> World -> Picture
draw env w = view env $
  [ say ("MDP: gamma = " ++ fmtN 2 (wGamma w) ++ "   slip = " ++ fmtN 1 (wSlip w) ++ "   step reward = " ++ showR cost) 40 42 22 (grayC 240)
  , say ("Click floor->wall->goal->pit   Up/Down gamma   <- -> slip   C step reward   F speed (" ++ (if wFast w then "fast" else "normal") ++ ")   Space restart") 40 68 15 (rgbC 170 176 210)
  , drawCells gw gx0 gy0 cs False ]
  -- 価値（でたらめ方策）を小さく表示
  ++ [ let (x, y) = cellXY gw s gx0 gy0 cs in sayC (fmtN 2 (wVRand w U.! s)) (x + cs / 2) (y + cs - 4) 14 (rgbC 150 156 200)
     | s <- [0 .. mdpNS m - 1], not (isTerm m s) ]
  -- 足あと
  ++ [ Color (withA (140 / 255) cyanC) (polyline 3 [ let (x, y) = cellXY gw s gx0 gy0 cs in (x + cs / 2, y + cs / 2) | s <- toList (wTrail w) ])
     , let (ax, ay) = cellXY gw (wPos w) gx0 gy0 cs in Color cyanC (circleAt (ax + cs / 2) (ay + cs / 2) (cs * 0.45)) ]
  -- ---- マウスの下のマスの遷移確率
  ++ [ say ("This episode: " ++ show (wEpLen w) ++ " steps, G = " ++ fmtN 3 (wEpRet w)) px (py + 16) 16 (grayC 230)
     , say ("Mean of collected G (" ++ show (Seq.length (wReturns w)) ++ " runs) = " ++ (if Seq.null (wReturns w) then "?" else fmtN 3 (meanOf (toList (wReturns w))))) px (py + 42) 16 (grayC 230)
     , say ("Computed V(S) (random policy) = " ++ fmtN 3 (wVRand w U.! wStart w)) px (py + 68) 16 goldC ]
  ++ (if hs >= 0 && not (isTerm m hs)
        then [ say ("Cell (" ++ show (hs `mod` gwW gw) ++ ", " ++ show (hs `div` gwW gw) ++ ") P(s' | s, a), r = " ++ showR cost ++ " unless noted:") px (py + 112) 16 (grayC 230) ]
             ++ [ say (idx "" dirNames a ++ ": " ++ unwords [ "(" ++ show (trS2 o `mod` gwW gw) ++ "," ++ show (trS2 o `div` gwW gw) ++ ") " ++ fmtN 2 (trP o)
                                                             ++ (if trR o /= cost then " r=" ++ showR (trR o) else "") ++ " " | o <- trans m hs a ])
                      px (py + 138 + fromIntegral a * 22) 14 (rgbC 200 205 235) | a <- [0 .. 3] ]
             ++ [ let (hx, hy) = cellXY gw hs gx0 gy0 cs in Color goldC (frameTL 2 (hx + 2) (hy + 2) (cs - 4) (cs - 4)) ]
        else [ sayWrap "Hover a cell to see where the 4 actions lead and with what probability" px (py + 96) (envW env - px - 30) 14 (rgbC 150 156 190) ])
  -- ---- 集めた G のヒストグラム
  ++ [ plotAxes g "G (discounted return)" "episodes" [-1.5, -1, 0, 1, 1.5] [0, y1 g] ]
  ++ [ Color (withA (190 / 255) cyanC) (rectTL (gX g (-1.5 + fromIntegral i * 0.1) + 1) (gY g b) (pW g / 30 - 2) (gY g 0 - gY g b))
     | (i, b) <- zip [0 :: Int ..] (U.toList bins), b > 0 ]
  ++ [ Color goldC (lineW 2 (gX g vS, pY g) (gX g vS, pY g + pH g))
     , say "Gold line = computed V(S). With many runs the mean comes close to it" 40 (gy0 + fromIntegral (gwH gw) * cs + 34) 14 (rgbC 150 156 190)
     , drawSelfTest env (wTest w) ]
  where
    gw = wGW w
    m = wMdp w
    cs = cellSize env gw
    cost = idx 0 stepCosts (wCostIdx w)
    (mx, my) = mouseP env
    hs = cellAt gw gx0 gy0 cs mx my
    px = gx0 + fromIntegral (gwW gw) * cs + 30
    py = gy0 + 10
    bins = U.accum (+) (U.replicate 30 0) [ (floorI (clampv (fromIntegral (floorI ((r + 1.5) / 0.1))) 0 29), 1 :: Double) | r <- toList (wReturns w) ]
    g0 = G px (py + 290) (min 440 (envW env - px - 40)) 150 (-1.5) 1.5 0 1
    g = g0 { y1 = max 1 (U.maximum bins) }
    vS = wVRand w U.! wStart w

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let gw = wGW w
        (mx, my) = toP5 env pos
        s = cellAt gw gx0 gy0 (cellSize env gw) mx my
        order = ".#GP"
        c = cellOf gw s
        next = idx '.' order ((maybe (-1) id (lookup c (zip order [0 ..])) + 1) `mod` 4)
    in if s < 0 || c == 'S' then w else rebuild w { wGW = gw { gwCell = gwCell gw U.// [(s, next)] } }
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild w { wGamma = min 0.99 (jsRound ((wGamma w + 0.05) * 100) / 100) }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild w { wGamma = max 0 (jsRound ((wGamma w - 0.05) * 100) / 100) }
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild w { wSlip = min 0.8 (jsRound ((wSlip w + 0.1) * 10) / 10) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild w { wSlip = max 0 (jsRound ((wSlip w - 0.1) * 10) / 10) }
  EventKey (SpecialKey KeySpace) Down _ _ -> rebuild w
  EventKey (Char ch) Down _ _
    | ch `elem` "cC" -> rebuild w { wCostIdx = (wCostIdx w + 1) `mod` length stepCosts }
    | ch `elem` "fF" -> w { wFast = not (wFast w) }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目・同じ種・同じ乱数の順）
selfTest :: [(String, Bool)]
selfTest =
  [ ("transition probabilities add up to 1 and are >= 0 for every state and action (slip 0, 0.2, 0.5, 0.8)", okSum)
  , ("goal and pit (terminal states) stay put with reward 0", okTerm)
  , ("with slip 0 there is exactly one next cell (probability 1)", okDet)
  , ("sampled next-cell frequencies ~ P(s' | s, right) (20000 samples)", all (\o -> near (cnt (trS2 o) / 20000) (trP o) 0.015) (trans m s0 1))
  , ("random policy: mean G " ++ fmtN 3 mu ++ " ~ computed V(S) " ++ fmtN 3 vSt ++ " (within 3 standard errors)", abs (mu - vSt) < 3 * se + 1e-3)
  , ("values stay within 1/(1 - gamma) (rewards in -1..1), no NaN", U.all (\v -> abs v <= 1 / (1 - 0.9) && not (isNaN v || isInfinite v)) ev) ]
  where
    gwT = gridParse mapRows                -- テスト用の地図（画面の地図とは別）
    checks = [ (sl, s, a, buildMdp gwT sl (-0.04)) | sl <- [0, 0.2, 0.5, 0.8], let mm = buildMdp gwT sl (-0.04), s <- [0 .. mdpNS mm - 1], a <- [0 .. 3] ]
    okSum = and [ near (sum (map trP os)) 1 1e-12 && all ((>= 0) . trP) os | (_, s, a, mm) <- checks, let os = trans mm s a ]
    okTerm = and [ case trans mm s a of { [o] -> trS2 o == s && trR o == 0; _ -> False } | (_, s, a, mm) <- checks, isTerm mm s ]
    okDet = and [ length (trans mm s a) == 1 | (sl, s, a, mm) <- checks, sl == 0, not (isTerm mm s) ]
    -- サンプリングの頻度 = 確率
    m = buildMdp gwT 0.2 (-0.04)
    s0 = 2 * gwW gwT + 4
    samples = withSeed 3 (replicateM 20000 (trS2 <$> sampleStep m s0 1))
    counts = U.accum (+) (U.replicate (mdpNS m) (0 :: Int)) [ (s2, 1) | s2 <- samples ]
    cnt s2 = fromIntegral (counts U.! s2) :: Double
    -- でたらめ方策：集めた G の平均 ≈ 計算した V(S)
    ev = fst (evalRandomPolicy m 0.9 1e-10)
    st = maybe 0 id (U.elemIndex 'S' (gwCell gwT))
    vSt = ev U.! st
    gs = withSeed 4 (replicateM 3000 (episode st 0 1 (0 :: Int)))
    episode !s !gg !d !t
      | t >= 300 || isTerm m s = pure gg
      | otherwise = do
          u <- rnd
          o <- sampleStep m s (floor (u * 4))
          episode (trS2 o) (gg + d * trR o) (d * 0.9) (t + 1)
    mu = meanOf gs
    se = sqrt (meanOf (map (\x -> (x - mu) * (x - mu)) gs) / fromIntegral (length gs))

-- ---------- やってみよう ----------
-- 1. すべり 0 と 0.5 で、でたらめ方策の V(S) はどう変わる？
-- 2. γ を 0 にすると V(S) は？（すぐ次の報酬しか見ない）。γ = 0.99 では？
-- 3. 落とし穴をふやす・宝をスタートの近くに置くと、G のヒストグラムの形はどう変わる？
-- 4. stepCosts の −0.1 を選ぶと「早く終わるほうがよい」世界になる。V(S) は？
-- 5. でたらめではなく「いつも右」の方策にしたら、価値はどう計算する？（03 の方策評価）

-- ==================================================
-- ここから下は「強化学習の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。乱数は p5.js 版と同じ mulberry32 で、同じ順に引く
-- ==================================================
-- ---- 色と小さな道具
bgC, goldC, cyanC, pinkC, limeC, violetC :: Color
bgC = rgbC 14 16 34                     -- 背景（アーケードの紺）
goldC = rgbC 255 196 60
cyanC = rgbC 90 215 245
pinkC = rgbC 245 110 170
limeC = rgbC 160 230 110
violetC = rgbC 175 140 255

negInf, posInf :: Double
negInf = -1 / 0
posInf = 1 / 0

-- v を a〜b に（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

-- いちばん大きい値の番号（同点はランダムに 1 つ。乱数を 1 つ使う）
argmaxR :: [Double] -> R Int
argmaxR q = case reverse best of
    [] -> pure 0
    bs -> do u <- rnd; pure (idx 0 bs (floor (u * fromIntegral (length bs))))
  where
    (_, best) = foldl' step (negInf, []) (zip [0 ..] q)
    step (m, b) (i, v)
      | v > m + 1e-12 = (v, [i])
      | abs (v - m) <= 1e-12 = (m, i : b)
      | otherwise = (m, b)

-- 同点は若い番号
argmaxFirst :: [Double] -> Int
argmaxFirst q = fst (foldl' (\(b, bv) (i, v) -> if v > bv then (i, v) else (b, bv)) (0, idx 0 q 0) (zip [0 ..] q))

maxOf :: [Double] -> Double
maxOf = foldl' (\m v -> if v > m then v else m) negInf

-- ε の確率でランダム、ほかは貪欲
epsGreedy :: [Double] -> Double -> R Int
epsGreedy q eps = do
  u <- rnd
  if u < eps then (\v -> floor (v * fromIntegral (length q))) <$> rnd else argmaxR q

-- ボルツマン分布 exp(h/τ) / Σ（大きい数でもあふれないように最大値を引く）
softmaxArr :: [Double] -> Double -> [Double]
softmaxArr h tau = map (/ s) e
  where
    m = maxOf h
    t = if tau == 0 || isNaN tau then 1 else tau
    e = map (\v -> exp ((v - m) / t)) h
    s = foldl' (+) 0 e

sampleFrom :: [Double] -> R Int
sampleFrom p = do
  u <- rnd
  let go _ _ [] = length p - 1
      go i c (x : xs) = let c' = c + x in if u < c' then i else go (i + 1) c' xs
  pure (go 0 0 p)

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else foldl' (+) 0 a / fromIntegral (length a)

-- 小数点以下 d 桁（toFixed）。−0.00 は 0.00 に、NaN は ?
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let s = showFFloat (Just d) v ""
                in if all (`elem` "-0.") s && take 1 s == "-" then drop 1 s else s

-- リストの i 番目（なければ d）
idx :: a -> [a] -> Int -> a
idx d xs i = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d
-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

seedRng :: Word32 -> Rng
seedRng s = Rng (if s == 0 then 1 else s)

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態はすぐに計算する（正格）
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- 種 s で始めて結果だけ取り出す（seedRng(s) してから計算）
withSeed :: Word32 -> R a -> a
withSeed s m = fst (runR m (seedRng s))

-- 今の乱数の状態で計算して、(結果, 次の状態)
runWith :: Rng -> R a -> (a, Rng)
runWith s m = runR m s

rnd :: R Double
rnd = R (\(Rng s) -> case rndStep (Rng s) of (u, s') -> (u, s'))

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

randn :: R Double                             -- 標準正規分布（ボックス-ミュラー）
randn = do
  u0 <- rnd
  v <- rnd
  let u = 1 - u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))
-- ---- グラフの道具（左上が原点、y は下向き）：G 左 上 幅 高さ x0 x1 y0 y1
data G = G { pX, pY, pW, pH :: !Float, x0, x1, y0, y1 :: !Double }

gX :: G -> Double -> Float
gX g v = pX g + f2 (clampv ((v - x0 g) / (x1 g - x0 g)) (-0.02) 1.02) * pW g

gY :: G -> Double -> Float
gY g v = pY g + pH g - f2 (clampv ((v - y0 g) / (y1 g - y0 g)) (-0.02) 1.02) * pH g

-- 枠・目盛り・軸の名前
plotAxes :: G -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgbC 70 76 110) (frameTL 1 (pX g) (pY g) (pW g) (pH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (pY g + pH g + 18) 14 (rgbC 150 156 190) | v <- xt ]
  ++ [ sayR (prec3 v) (pX g - 5) (gY g v + 5) 14 (rgbC 150 156 190) | v <- yt ]
  ++ [ sayR xl (pX g + pW g) (pY g + pH g + 38) 14 (rgbC 205 210 235), say yl (pX g) (pY g - 8) 14 (rgbC 205 210 235) ]

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ wt）
polyline :: Float -> [(Float, Float)] -> Picture
polyline wt pts = case pts of
  (_ : _ : _) -> thickPath wt pts
  _ -> Blank

-- 直前 k 個の平均（ギザギザの学習曲線をならす）
smoothArr :: Seq.Seq Double -> Int -> Seq.Seq Double
smoothArr a k = Seq.fromList (go 0 0 (toList a))
  where
    arr = a
    go _ _ [] = []
    go !i !s (v : vs) = let s1 = s + v - (if i >= k then Seq.index arr (i - k) else 0)
                        in s1 / fromIntegral (min (i + 1) k) : go (i + 1) s1 vs

-- arr の i 番目を x = i + xOff に。長いときは間引いて 600 点まで（色 col、太さ sw）
drawCurve :: G -> Seq.Seq Double -> Color -> Float -> Double -> Picture
drawCurve g arr col sw xOff = Color col (polyline sw pts)
  where
    n = Seq.length arr
    stride = max 1 ((n + 599) `div` 600)
    pt i = (gX g (fromIntegral i + xOff), gY g (Seq.index arr i))
    pts = [ pt i | i <- [0, stride .. n - 1] ] ++ [ pt (n - 1) | n > 1, (n - 1) `mod` stride /= 0 ]

-- 色の見本と名前（items = [(色, 名前)]）
legend :: Float -> Float -> [(Color, String)] -> Picture
legend x y items = Pictures (concat [ [Color c (rectTL x (yy - 5) 16 10), say s (x + 22) (yy + 5) 14 (rgbC 220 225 245)]
                                    | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * fromIntegral k ])

-- 長さ cap までの列に追加（古いものから捨てる。メモリを増やし続けない）
pushCap :: Int -> Double -> Seq.Seq Double -> Seq.Seq Double
pushCap cap !v s = let s' = s Seq.|> v in if Seq.length s' > cap then Seq.drop 1 s' else s'
-- ---- グリッドワールド：文字の地図 '.' 床、'#' 壁、'S' スタート、'G' 宝（+1 で終わり）、'P' 落とし穴（−1 で終わり）
dirs :: [(Int, Int)]
dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)]       -- 行動 0 上、1 右、2 下、3 左

dirNames :: [String]
dirNames = ["up", "right", "down", "left"]

data GW = GW { gwW, gwH :: !Int, gwCell :: !(U.Vector Char) }

gridParse :: [String] -> GW
gridParse rows = GW (length (headOr "" rows)) (length rows) (U.fromList (concat rows))

cellOf :: GW -> Int -> Char
cellOf gw s = if s >= 0 && s < U.length (gwCell gw) then gwCell gw U.! s else '#'

-- 決まった向きに 1 マス。外や壁ならその場にとどまる
gridMove :: GW -> Int -> Int -> Int
gridMove gw s a =
  let (dx, dy) = idx (0, 0) dirs a
      x = s `mod` gwW gw + dx
      y = s `div` gwW gw + dy
  in if x < 0 || y < 0 || x >= gwW gw || y >= gwH gw || cellOf gw (y * gwW gw + x) == '#' then s else y * gwW gw + x

isTerminalCell :: Char -> Bool
isTerminalCell c = c == 'G' || c == 'P'

enterReward :: Char -> Double -> Double
enterReward c stepR = if c == 'G' then 1 else if c == 'P' then -1 else stepR

-- 遷移 (次の状態 s2, 確率 p, 報酬 r)
data Tr = Tr { trS2 :: !Int, trP :: !Double, trR :: !Double }

-- マルコフ決定過程：mP V.! s V.! a = [遷移]。意図した向きに 1 − slip、左右の直角の向きに slip/2 ずつ
data Mdp = Mdp { mdpNS :: !Int, mdpP :: !(V.Vector (V.Vector [Tr])), mdpTerm :: !(U.Vector Bool), mdpGW :: !GW }

buildMdp :: GW -> Double -> Double -> Mdp
buildMdp gw slip stepR = Mdp nS (V.generate nS row) term gw
  where
    nS = gwW gw * gwH gw
    term = U.generate nS (\s -> let c = cellOf gw s in isTerminalCell c || c == '#')
    row s = V.generate 4 (\a -> if term U.! s then [Tr s 1 0] else outs s a)   -- 終わりのマス（と壁）はそこに留まり、報酬 0
    outs s a = foldl' add [] [(a, 1 - slip), ((a + 1) `mod` 4, slip / 2), ((a + 3) `mod` 4, slip / 2)]
      where add list (d, p)
              | p <= 0 = list
              | otherwise = let s2 = gridMove gw s d
                            in if any ((== s2) . trS2) list then [ if trS2 o == s2 then o { trP = trP o + p } else o | o <- list ]
                               else list ++ [Tr s2 p (enterReward (cellOf gw s2) stepR)]

trans :: Mdp -> Int -> Int -> [Tr]
trans mdp s a = (mdpP mdp V.! s) V.! a

isTerm :: Mdp -> Int -> Bool
isTerm mdp s = mdpTerm mdp U.! s

qValue :: Mdp -> U.Vector Double -> Double -> Int -> Int -> Double
qValue mdp v gamma s a = foldl' (\q o -> q + trP o * (trR o + gamma * (v U.! trS2 o))) 0 (trans mdp s a)

-- 価値反復の 1 回：V(s) ← max_a Σ p (r + γ V(s'))。(新しい V, 最大の変化量)
viSweep :: Mdp -> U.Vector Double -> Double -> (U.Vector Double, Double)
viSweep mdp v gamma = (nv, U.maximum (U.cons 0 (U.zipWith (\a b -> abs (a - b)) nv v)))
  where nv = U.generate (mdpNS mdp) (\s -> if isTerm mdp s then v U.! s else maximum' [ qValue mdp v gamma s a | a <- [0 .. 3] ])
        maximum' = foldl' max negInf

-- (V, 回数, 毎回の変化量)
valueIteration :: Mdp -> Double -> Double -> Int -> (U.Vector Double, Int, [Double])
valueIteration mdp gamma tol maxIt = go (U.replicate (mdpNS mdp) 0) 0 posInf []
  where
    go v its d ds
      | d >= tol && its < maxIt = let (v', d') = viSweep mdp v gamma in go v' (its + 1) d' (d' : ds)
      | otherwise = (v, its, reverse ds)

-- 最適な行動の集合（同点は全部）
optimalActs :: Mdp -> U.Vector Double -> Double -> Int -> Double -> [Int]
optimalActs mdp v gamma s tol = [ a | (a, q) <- zip [0 ..] qs, q >= m - tol ]
  where qs = [ qValue mdp v gamma s a | a <- [0 .. 3] ]
        m = maxOf qs

-- 方策評価：V(s) ← Σ p (r + γ V(s')) を、変化が tol 未満になるまで（その場で書きかえる）。(V, 何回まわしたか)
evalPolicy :: Mdp -> U.Vector Int -> Double -> Double -> U.Vector Double -> (U.Vector Double, Int)
evalPolicy mdp pol gamma tol v0 = runST $ do
  v <- U.thaw v0
  let qv s a = foldM (\q o -> do x <- UM.read v (trS2 o); pure (q + trP o * (trR o + gamma * x))) 0 (trans mdp s a)
      sweep = foldM (\d s -> if isTerm mdp s then pure d else do
                        x <- qv s (pol U.! s)
                        old <- UM.read v s
                        UM.write v s x
                        pure (max d (abs (x - old)))) 0 [0 .. mdpNS mdp - 1]
      loop !n d | d >= tol && n < 100000 = do d' <- sweep; loop (n + 1) d'
                | otherwise = pure n
  n <- loop (0 :: Int) posInf
  v' <- U.freeze v
  pure (v', n)

-- マスの左上（p5 の座標）
cellXY :: GW -> Int -> Float -> Float -> Float -> (Float, Float)
cellXY gw s gx gy cs = (gx + fromIntegral (s `mod` gwW gw) * cs, gy + fromIntegral (s `div` gwW gw) * cs)

-- マスの中心から向き a に矢印（太さ wt）
drawArrowIn :: Float -> Float -> Int -> Float -> Float -> Picture
drawArrowIn cx cy a len wt = Pictures
  [ thickLine wt (cx - dx * len * 0.4, cy - dy * len * 0.4) (ex, ey)
  , Polygon [(ex + dx * 5, ey + dy * 5), (ex - dy * 5 - dx * 2, ey + dx * 5 - dy * 2), (ex + dy * 5 - dx * 2, ey - dx * 5 - dy * 2)] ]
  where
    (ix', iy') = idx (0, 0) dirs a
    dx = fromIntegral ix'
    dy = fromIntegral iy'
    ex = cx + dx * len
    ey = cy + dy * len

-- 壁・宝（G）・落とし穴（P）・スタート（S）を描く
drawCells :: GW -> Float -> Float -> Float -> Bool -> Picture
drawCells gw gx gy cs skipFloor = Pictures (concatMap one [0 .. gwW gw * gwH gw - 1])
  where
    ts = max 14 (cs * 0.4)
    one s =
      let c = cellOf gw s
          (x, y) = cellXY gw s gx gy cs
          bgc | c == '#' = rgbC 70 76 120
              | c == 'G' = goldC
              | c == 'P' = rgbC 120 30 70
              | otherwise = rgbC 26 30 60
          mid col str = [sayM str (x + cs / 2) (y + cs / 2) ts col]
      in if c == '.' && skipFloor then [] else
           [Color bgc (rectTL (x + 1) (y + 1) (cs - 2) (cs - 2))]
           ++ (if c == 'G' then mid (rgbC 40 30 0) "G" else [])
           ++ (if c == 'P' then mid (rgbC 255 150 190) "P" else [])
           ++ (if c == 'S' then mid cyanC "S" else [])

-- マウスの下のマス（なければ −1）
cellAt :: GW -> Float -> Float -> Float -> Float -> Float -> Int
cellAt gw gx gy cs mx my =
  let x = floor ((mx - gx) / cs)
      y = floor ((my - gy) / cs)
  in if x >= 0 && y >= 0 && x < gwW gw && y < gwH gw then y * gwW gw + x else -1

-- 決定的に動くときの、各マスからゴールまでの最短の歩数（届かなければ posInf）
bfsDist :: GW -> Int -> U.Vector Double
bfsDist gw goal = go (U.replicate n posInf U.// [(goal, 0)]) (Seq.singleton goal)
  where
    n = gwW gw * gwH gw
    go d q = case Seq.viewl q of
      Seq.EmptyL -> d
      s Seq.:< rest ->
        let step (dd, qq) a =
              let (ddx, ddy) = idx (0, 0) dirs a
                  x = s `mod` gwW gw - ddx
                  y = s `div` gwW gw - ddy
                  p = y * gwW gw + x            -- p から向き a で s に来られる？
              in if x < 0 || y < 0 || x >= gwW gw || y >= gwH gw || cellOf gw p == '#' || dd U.! p < posInf || isTerminalCell (cellOf gw p)
                   then (dd, qq)
                   else if gridMove gw p a == s then (dd U.// [(p, dd U.! s + 1)], qq Seq.|> p) else (dd, qq)
            (d', q') = foldl' step (d, rest) [0 .. 3]
        in go d' q'

-- 低い = 紫、0 = 紺、高い = 金
valueFill :: Double -> Double -> Double -> Color
valueFill v lo hi =
  let rng' = if hi - lo == 0 then 1 else hi - lo
      t = clampv ((v - lo) / rng') 0 1
  in if t < 0.5 then let u = t / 0.5 in rgbD (110 - 80 * u) (60 - 30 * u) (150 - 90 * u)
     else let u = (t - 0.5) / 0.5 in rgbD (30 + 220 * u) (30 + 160 * u) (60 - 20 * u)

rgbD :: Double -> Double -> Double -> Color
rgbD r g b = makeColor (f2 r / 255) (f2 g / 255) (f2 b / 255) 1
-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

rgbC :: Int -> Int -> Int -> Color
rgbC r g b = makeColorI r g b 255

rgbaC :: Int -> Int -> Int -> Int -> Color
rgbaC = makeColorI

grayC :: Int -> Color
grayC v = makeColorI v v v 255

-- 左下が (x, y) の文字（y は文字の下の線）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

textW :: String -> Float -> Float
textW s size = fromIntegral (length s) * size * 0.72

-- 幅 maxW で折り返す文字（左上 = (x, y)）
sayWrap :: String -> Float -> Float -> Float -> Float -> Color -> Picture
sayWrap s x y maxW size c = Pictures [ say l x (y + size + fromIntegral k * size * 1.3) size c | (k, l) <- zip [0 :: Int ..] (wrapText maxW size s) ]

wrapText :: Float -> Float -> String -> [String]
wrapText maxW size s = go (words s) ""
  where
    fits t = textW t size <= maxW
    go [] cur = [cur | not (null cur)]
    go (w : ws) "" = go ws w
    go (w : ws) cur = let t = cur ++ " " ++ w in if fits t then go ws t else cur : go (w : ws) ""

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が (x, y) の長方形の中にあるか
inBox :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
inBox (mx, my) (x, y, w, h) = mx > x && mx < x + w && my > y && my < y + h
-- ---- 1 フレームの計算を budget ミリ秒までにする（p5 の do { … } while (performance.now() − t0 < budget && more)）
--      step は必ず 1 回は動く。速さ F は budget を変える
sliced :: Double -> (w -> Bool) -> (w -> w) -> w -> IO w
sliced budget more step w0 = do
  t0 <- getMonotonicTime
  let go w = do
        w' <- evaluate (step w)
        t <- getMonotonicTime
        if (t - t0) * 1000 < budget && more w' then go w' else pure w'
  go w0
-- ---- 数の道具
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt = fmtN

-- 有効数字 n 桁に丸めた数（toPrecision）
roundSig :: Int -> Double -> Double
roundSig n v0'
  | v == 0 = 0
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    k = 10 ^^ (n - 1 - e) :: Double
                in fromIntegral (round (v * k) :: Integer) / k
  where v = fin 0 v0'

-- 有効数字 3 桁で書く（+v.toPrecision(3)）
prec3 :: Double -> String
prec3 v = let r = roundSig 3 v
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else showFFloat Nothing r ""

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Double
jsRound v = fromIntegral (jsRoundI v)

jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e9) 1e9)

-- リストの最初（なければ d）
headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d
-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- NaN も ∞ もない
noNaN :: [Double] -> Bool
noNaN = all (\v -> not (isNaN v || isInfinite v))

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 画面の右下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 200) (envH env - 12) 14 (if all snd rs then rgbC 120 225 200 else rgbC 255 90 120)
