-- ============================================
-- 64-14 アンサンブルの合唱 🔊 ─ 5 つのモデルが 5 人の歌い手。マウスの位置で、それぞれの予測（クラス）を音の高さで歌う
-- クラス 0 なら ド（C）の長三和音、クラス 1 なら ファ♯（F♯）の長三和音の音を、歌い手ごとの決まった位置（根音・3 度・5 度…）で歌う
--   5 人の意見がそろうと 1 つの長三和音（協和音）。割れると C と F♯ の和音が混ざる（不協和音。ペトルーシュカ和音とも呼ばれる）
--   いちばん低い音 = 多数決の答え。音量 = その歌い手の自信（確率が 0.5 から遠いほど大きい）
--   背景 = ソフト投票の地図、白い線 = 5 つのモデルの境界。境界の近くほど意見が割れる
--   音は Art.Sound の短い音を 0.4 秒ごとに鳴らし直す（p5.js 版は鳴りっぱなしの声の高さと音量を変える）
-- 操作：クリック 音を出す（もう一度で止める）/ A 自動でなぞる / 1〜5 歌い手を黙らせる / ← → データの形 / R データを作り直す
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (runST)
import Data.Bits (shiftR, xor, (.|.))
import Data.List (foldl', intercalate, sortBy, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word32, Word8)
import GHC.Clock (getMonotonicTime)
import System.Environment (lookupEnv)

nTrain :: Int
nTrain = 200                   -- ★ 点の数

flipP :: Double
flipP = 0.08                   -- ★ ラベルのノイズ

roots :: [Int]
roots = [48, 54]               -- ★ クラスの根音（C3 と F♯3）

singerIv :: [Int]
singerIv = [0, 4, 7, 12, 16]   -- ★ 歌い手ごとの音の位置（半音）

cells :: Int
cells = 70

beatFrames :: Int
beatFrames = 24                -- ★ 音を鳴らし直す間隔（フレーム、24 = 0.4 秒）

-- いまの位置での 5 人の答え
data Choir = Choir { chPs :: ![Double], chCs :: ![Int], chMaj :: !Int, chAgree :: !Int, chMidis :: ![Int] }

data World = World
  { wKind    :: !Int           -- ★ データの形（0〜5）
  , wSeedD   :: !Int
  , wTrain   :: !DataSet
  , wModels  :: ![Model]       -- 4 番目（ネット）は少しずつ学習
  , wMlpLeft :: !Int
  , wGrids   :: ![U.Vector Double]   -- モデルごとのマス目の確率
  , wImg     :: !Picture       -- ソフト投票の地図 + 境界
  , wSound   :: !Bool
  , wAuto    :: !Bool
  , wMute    :: ![Bool]
  , wPos     :: !(Double, Double)   -- 聞く位置
  , wChoir   :: !Choir
  , wAgree   :: ![Int]         -- これまでの一致の人数（新しい順、300 まで）
  , wTest    :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "64-14 Ensemble choir" world0)
  { skBack  = colOf bgCol
  , skStart = \_ -> do
      printSelfTest selfTest
      _ <- evaluate (sum (map U.sum (wGrids world0)))
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = restart (World 4 2 (DataSet V.empty U.empty) [] 0 [] Blank False True (replicate 5 False) (0.5, 0.5) (Choir [] [] 0 5 []) [] [])

restart :: World -> World
restart w =
  let tr = makeData2D (wKind w) nTrain flipP (wSeedD w)
      ms = [ fitBase 0 (dsX tr) (dsY tr), fitBase 1 (dsX tr) (dsY tr), fitBase 2 (dsX tr) (dsY tr)
           , MMlp (makeMLP 2 10 True 7), fitBase 4 (dsX tr) (dsY tr) ]   -- ネットは step の中で少しずつ学習
  in repaint w { wTrain = tr, wModels = ms, wMlpLeft = 300, wGrids = map gridOf ms }

gridOf :: Model -> U.Vector Double
gridOf m = U.generate (cells * cells) (\q -> probOf m (cellPt cells q))

-- 地図：ソフト投票の色 + 5 つのモデルの境界（白）。p5.js 版の 2 枚の画像を 1 枚に重ねて作る
repaint :: World -> World
repaint w = w { wImg = gridImage cells col }
  where
    gs = wGrids w
    pr q = [ if g U.! q > 0.5 then 1 else 0 :: Int | g <- gs ]
    col q = let s = foldl' (\a g -> a + g U.! q) 0 gs / fromIntegral (length gs)
                (r, g, b) = probCol s 0.7
                i = q `mod` cells
                p0 = pr q
                right = if i + 1 < cells then pr (q + 1) else p0
                down = if q + cells < cells * cells then pr (q + cells) else p0
                edge = length (filter id (zipWith3 (\a c d -> c /= a || d /= a) p0 right down))
                a = fromIntegral (min 255 (edge * 110)) / 255
            in (r + (255 - r) * a, g + (245 - g) * a, b + (220 - b) * a)

-- ★ 歌い手 k がクラス c のときの音（MIDI 番号）と、多数決の低音
singerMidi :: Int -> Int -> Int
singerMidi k c = headOr 48 (drop c roots) + headOr 0 (drop k singerIv)

bassMidi :: Int -> Int
bassMidi maj = headOr 48 (drop maj roots) - 12

choirAt :: [Model] -> Pt -> Choir
choirAt ms x =
  let ps = map (`probOf` x) ms
      cs = map (\p -> if p > 0.5 then 1 else 0) ps
      n1 = sum cs
  in Choir ps cs (if 2 * n1 > 5 then 1 else 0) (max n1 (5 - n1)) (zipWith singerMidi [0 ..] cs)

plotBox :: Env -> Box
plotBox env = let side = min (envH env - 170) (envW env * 0.48) in Box 40 100 side side

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  -- ネットを 1 フレームに 15 回ずつ学習（終わったら地図を塗り直す）
  let w1 = if wMlpLeft w0 <= 0 then w0 else case drop 3 (wModels w0) of
        (MMlp net : rest) ->
          let net' = mlpTrain net (V.map centerX (dsX (wTrain w0))) (U.map fromIntegral (dsY (wTrain w0))) 15 0.03 0
              ms = take 3 (wModels w0) ++ MMlp net' : rest
              w' = w0 { wModels = ms, wMlpLeft = wMlpLeft w0 - 15 }
          in if wMlpLeft w' <= 0 then repaint w' { wGrids = map gridOf' (zip [0 ..] ms) } else w'
          where gridOf' (k, m) = if k == (3 :: Int) then gridOf m else headOr U.empty (drop k (wGrids w0))
        _ -> w0 { wMlpLeft = 0 }
      -- 聞く位置：マウス（図の上）か、自動でなぞる 8 の字
      (mu, mv) = fromPx (plotBox env) (mouseP env)
      onMap = mu >= 0 && mu <= 1 && mv >= 0 && mv <= 1
      tt = fromIntegral (envFrame env) / 240 :: Double
      pos = if wAuto w1 && not onMap then (0.5 + 0.42 * sin (2 * pi * tt), 0.5 + 0.38 * sin (4 * pi * tt)) else (clampv mu 0 1, clampv mv 0 1)
      ch = choirAt (wModels w1) (uncurry pt2 pos)
  -- ---- 音（短い音を beatFrames ごとに鳴らし直す）
  when (wSound w1 && envFrame env `mod` beatFrames == 0) $ do
    let dur = fromIntegral beatFrames / 60 * 0.9
    forM_ (zip3 (chMidis ch) (chPs ch) (wMute w1)) $ \(md, p, mt) ->
      when (not mt) (playTone (realToFrac (midiHz (fromIntegral md))) (realToFrac (master * (0.06 + 0.14 * abs (2 * p - 1)))) dur Triangle)
    playTone (realToFrac (midiHz (fromIntegral (bassMidi (chMaj ch))))) (realToFrac (master * 0.22)) dur Sine
  pure w1 { wPos = pos, wChoir = ch, wAgree = take 300 (chAgree ch : wAgree w1) }

draw :: Env -> World -> Picture
draw env w = view env
  [ imageBox 40 100 side side cells cells (wImg w)
  , frameBox pb frameCol
  , drawPoints pb (dsX (wTrain w)) (dsY (wTrain w)) 5
  , Color white (Pictures [ ringAt ppx ppy 22 2, lineW 2 (ppx - 16, ppy) (ppx + 16, ppy), lineW 2 (ppx, ppy - 16) (ppx, ppy + 16) ])
  -- ---- 右：歌い手
  , say "5 singers (height = the note they sing)" rx (sy - 20) 16 (colOf ink)
  , Pictures [ Pictures [ Color (rgbC 70 50 56) (lineW 1 (rx, yOf md) (rx + rw, yOf md))
                        , sayRM ("C" ++ show (md `div` 12 - 1)) (rx - 4) (yOf md) 14 (rgbC 140 120 120) ]
             | md <- [36, 48, 60, 72] ]
  , Pictures [ Pictures [ Color (if mt then rgbC 70 60 60 else colOf (clsCol c)) (circleAt x (yOf md) (26 + 30 * f2 (abs (2 * p - 1))))
                        , sayM (if c == 1 then "F#" else "C") x (yOf md) 14 (grayC 20)
                        , sayC (show (k + 1) ++ " " ++ shortName k) x (sy + 336) 14 (colOf (baseCol k))
                        , sayC ("p = " ++ fmtNum 2 p) x (sy + 356) 14 (rgbC 200 185 170) ]
             | (k, md, c, p, mt) <- zip5 [0 :: Int ..] (chMidis ch) (chCs ch) (chPs ch) (wMute w)
             , let x = rx + 40 + fromIntegral k * ((rw - 120) / 4) ]
  , Color (colOf gold) (rectTL (rx + 10) (by - 8) (rw - 20) 16)
  , sayM ("bass = majority: class " ++ show (chMaj ch) ++ (if chMaj ch == 1 then " (F#)" else " (C)")) (rx + rw / 2) by 14 (grayC 20)
  -- ---- 一致のようす
  , say (if chAgree ch == 5 then "5 agree -> consonant (major triad)" else show (chAgree ch) ++ " vs " ++ show (5 - chAgree ch) ++ " -> dissonant (two chords mixed)")
        rx (sy + 396) 20 (colOf (if chAgree ch == 5 then teal else if chAgree ch == 4 then gold else wine))
  , plotAxes g "" "agreement so far (5 = all agree)" [] [3, 4, 5]
  , Color (colOf gold) (polyline 2 [ gPt g (fromIntegral k) (fromIntegral a) | (k, a) <- zip [0 :: Int ..] (reverse (wAgree w)) ])
  -- ---- 文字
  , say ("Ensemble choir   data: " ++ shapeName (wKind w) ++ (if wMlpLeft w > 0 then "   (training net...)" else "")
         ++ (if wSound w then "   sound ON" else "   sound OFF (click)")) 40 40 20 (colOf ink)
  , say ("Click sound on/off   A auto trace" ++ (if wAuto w then " (ON)" else " (OFF)") ++ "   1-5 mute singer   <- -> data shape   R new data") 40 66 15 (rgbC 170 155 145)
  , drawSelfTest env (wTest w) ]
  where
    pb = plotBox env
    side = bxW pb
    ch = wChoir w
    (ppx, ppy) = toPx pb (fst (wPos w)) (snd (wPos w))
    rx = 40 + side + 60
    rw = envW env - rx - 40
    sy = 150
    yOf md = sy + 300 - ((fromIntegral md - 30) / (72 - 30)) * 300
    by = yOf (bassMidi (chMaj ch))
    g = Graph rx (sy + 430) rw (max 40 (envH env - sy - 510)) 0 300 2.5 5.5
    shortName k = headOr "" (drop k ["k-NN", "logit", "NB", "net", "stump"])
    zip5 (a : as) (b : bs) (c : cs) (d : ds) (e : es) = (a, b, c, d, e) : zip5 as bs cs ds es
    zip5 _ _ _ _ _ = []

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w { wSound = not (wSound w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> restart w { wKind = (wKind w + 1) `mod` 6 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> restart w { wKind = (wKind w + 5) `mod` 6 }
  EventKey (Char c) Down _ _
    | c == 'a' || c == 'A' -> w { wAuto = not (wAuto w) }
    | c >= '1' && c <= '5' -> let k = fromEnum c - fromEnum '1' in w { wMute = [ if j == k then not m else m | (j, m) <- zip [0 ..] (wMute w) ] }
    | c == 'r' || c == 'R' -> restart w { wSeedD = wSeedD w + 1 }
  _ -> w

-- ---- 自己テスト
selfTest :: [(String, Bool)]
selfTest =
  [ ("全員一致なら 5 つの音は 1 つの長三和音の中、割れた 30 通りはどれも 1 つの長三和音・短三和音に入らない", okU && okS)
  , ("音の高さ：A4 = MIDI 69 = 440 Hz、1 オクターブ上で 2 倍、低音は根音の 1 オクターブ下"
    , midiHz 69 == 440 && near (midiHz 81 / midiHz 69) 2 1e-12 && bassMidi 0 == 36 && bassMidi 1 == 42)
  , ("三日月データで 4 つのモデルは 80% 以上、切り株も 65% 以上（" ++ intercalate ", " (map pctStr accs) ++ "）", all (>= 0.8) (take 4 accs) && lastOr 0 accs >= 0.65)
  , ("低音（多数決）= 5 つのハード投票の答え（300 点）。5 人なので一致の人数はいつも 3〜5", all fst chk && all snd chk)
  , ("音量の上限：MASTER = " ++ fmtNum 3 master ++ " ≤ 0.2、歌い手 1 人の音量 ≤ 0.2", master <= 0.2 && 0.06 + 0.14 <= 0.2 + (1e-12 :: Double)) ]
  where
    inTriad pcs = or [ all (`elem` tri) pcs | r <- [0 .. 11], t <- [[0, 4, 7], [0, 3, 7]], let tri = map (\v -> (v + r) `mod` 12) t ]
    okU = all (\c -> inTriad [ singerMidi k c `mod` 12 | k <- [0 .. 4] ]) [0, 1]
    okS = not (any (\mask -> inTriad [ singerMidi k ((mask `div` (2 ^ k)) `mod` 2) `mod` 12 | k <- [0 .. 4] ]) [1 .. 30 :: Int])
    tr = makeData2D 4 200 0.08 2
    te = makeData2D 4 500 0.08 99
    ms = [ fitBase k (dsX tr) (dsY tr) | k <- [0 .. 4] ]
    accs = [ accuracyOf (predOf m) (dsX te) (dsY te) | m <- ms ]
    xs = withSeed 5 (mapM (const (do a <- rnd; b <- rnd; pure (pt2 a b))) [1 .. 300 :: Int])
    chk = [ (chMaj c == hardVote ms x, chAgree c >= 3 && chAgree c <= 5) | x <- xs, let c = choirAt ms x ]

-- ---------- やってみよう ----------
-- 1. クリックで音を出し、マウスを境界の上でゆっくり動かそう。どこで音がにごる？
-- 2. 1〜5 で歌い手を 1 人ずつ黙らせると、にごりは減る？ どのモデルがいちばん「ちがう意見」を歌う？
-- 3. roots の 54（F♯）を 55（G）にすると？ 割れても協和音になってしまう組合せがある（自己テストも FAIL に）
-- 4. singerIv を [0, 7, 12, 19, 24]（根音と 5 度だけ）にすると、和音の響きはどう変わる？
-- 5. 自由制作：モデルの数を増やしたり、確率で音の大きさだけでなく音色を変えたりしてみよう

-- ==================================================
-- ここから下は「アンサンブル学習の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする。点 x = U.Vector Double（x1, x2, …）
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 小さな道具：はさむ・合計・平均・いちばん大きい番号・数の表示
type Pt = U.Vector Double                     -- 点（x1, x2, …）

pt2 :: Double -> Double -> Pt
pt2 u v = U.fromList [u, v]

-- データ：点 X とクラス y（0, 1）
data DataSet = DataSet { dsX :: !(V.Vector Pt), dsY :: !(U.Vector Int) }

xAt :: V.Vector Pt -> Int -> Int -> Double    -- X[i][f]
xAt xs i f = (xs V.! i) U.! f

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0

-- はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

sumL :: [Double] -> Double
sumL = foldl' (+) 0

sumU :: U.Vector Double -> Double
sumU = U.foldl' (+) 0

meanOf :: [Double] -> Double
meanOf a = if null a then 0 else sumL a / fromIntegral (length a)

-- ÷n の分散
varOf :: [Double] -> Double
varOf a = let m = meanOf a in if null a then 0 else foldl' (\s v -> s + (v - m) * (v - m)) 0 a / fromIntegral (length a)

-- いちばん大きい番号（同点なら小さい番号）
argMaxL :: [Double] -> Int
argMaxL [] = 0
argMaxL (a0 : as) = go 1 0 a0 as
  where go _ b _ [] = b
        go !i !b !bv (v : vs) = if v > bv then go (i + 1) i v vs else go (i + 1) b bv vs

-- いちばん小さい値の最初の番号（indexOf(Math.min(...))）
argMinL :: [Double] -> Int
argMinL = argMaxL . map negate

sigm :: Double -> Double
sigm z = 1 / (1 + exp (-(clampv z (-40) 40)))

fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- 数を短く書く（p5.js 版の fmtNum：toFixed(d) のあと余分な 0 を消す。0.000001 より小さいと 1e-7 の形）
fmtNum :: Int -> Double -> String
fmtNum d v
  | isNaN v || isInfinite v = "?"
  | n == 0 = "0"
  | otherwise = (if v < 0 then "-" else "") ++ body
  where
    n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
    ds = show n
    padded = replicate (d + 1 - length ds) '0' ++ ds
    (ip, fp) = splitAt (length padded - d) padded
    tiny = fromIntegral n / (10 ^^ d :: Double) < 1e-6
    sig = reverse (dropWhile (== '0') (reverse ds))
    ex = length ds - 1 - d
    body | tiny = take 1 sig ++ (if length sig > 1 then "." ++ drop 1 sig else "") ++ "e" ++ show ex
         | d > 0 = stripZeros (ip ++ "." ++ fp)
         | otherwise = ip
    stripZeros s = let t = reverse (dropWhile (== '0') (reverse s)) in if lastOr ' ' t == '.' then init t else t

pctStr :: Double -> String
pctStr v = fmtNum 1 (100 * v) ++ "%"

-- ---- 種つきの乱数（同じ種なら毎回同じ列。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：列を種 s から始め直す（0 なら 1）
seedRng :: Int -> R ()
seedRng s = R (\_ -> let w = fromIntegral s :: Word32 in ((), Rng (if w == 0 then 1 else w)))

rngOf :: Int -> Rng
rngOf s = snd (runR (seedRng s) (Rng 1))

-- 計算を実行して、結果と次の状態
evalR :: Rng -> R a -> (a, Rng)
evalR g m = runR m g

-- 種 s から始めて結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

rndGauss :: R Double                          -- 標準正規分布（ボックス-ミュラー）
rndGauss = do
  u0 <- rnd
  v <- rnd
  let u = max 1e-12 u0
  pure (sqrt (-2 * log u) * cos (2 * pi * v))

-- フィッシャー-イェーツのシャッフル（後ろから i と 0〜i のどこかを入れかえる）
shuffleU :: U.Unbox a => U.Vector a -> R (U.Vector a)
shuffleU v = do
  let n = U.length v
  js <- mapM (\i -> do u <- rnd; pure (i, floor (u * fromIntegral (i + 1)))) [n - 1, n - 2 .. 1]
  pure (U.modify (\mv -> mapM_ (\(i, j) -> MU.swap mv i j) js) v)

-- ブートストラップ：0〜n−1 を重複ありで n 個
bootIdx :: Int -> R (U.Vector Int)
bootIdx n = U.fromList <$> mapM (const (do u <- rnd; pure (floor (u * fromIntegral n)))) [1 .. n]

-- ---- 連立 1 次方程式 A w = b（部分ピボットつきのガウスの消去法）。ridge = 対角に足す小さな数（解けないのを防ぐ）
solveLin :: [[Double]] -> [Double] -> Double -> [Double]
solveLin a b ridge = [ if abs (r U.! i) < 1e-14 then 0 else (r U.! n) / (r U.! i) | (i, r) <- zip [0 ..] (V.toList final) ]
  where
    n = length b
    m0 = V.fromList [ U.fromList (zipWith (\k v -> if k == i then v + ridge else v) [0 ..] row ++ [bi]) | (i, row, bi) <- zip3 [0 :: Int ..] a b ]
    final = foldl' elim m0 [0 .. n - 1]
    elim m c =
      let p = foldl' (\pp r -> if abs ((m V.! r) U.! c) > abs ((m V.! pp) U.! c) then r else pp) c [c + 1 .. n - 1]
      in if abs ((m V.! p) U.! c) < 1e-14 then m else
         let ms = m V.// [(c, m V.! p), (p, m V.! c)]
             pr = ms V.! c
         in V.imap (\r row -> if r == c then row else
                      let f = (row U.! c) / (pr U.! c)
                      in if f /= 0 then U.imap (\k v -> if k >= c then v - f * (pr U.! k) else v) row else row) ms

-- ---- 合成データ（[0, 1]² の 2 次元、2 クラス、種つき）。kind = 0 かたまり / 1 斜め / 2 円 / 3 XOR / 4 三日月 / 5 しま
shapeNames :: [String]
shapeNames = ["blobs", "diagonal", "circle", "XOR", "moons", "stripes"]

shapeName :: Int -> String
shapeName k = headOr "?" (drop k shapeNames)

trueLabel :: Int -> Double -> Double -> Int
trueLabel kind u v = case kind of
  0 -> if sq (u - 0.32) + sq (v - 0.36) < sq (u - 0.68) + sq (v - 0.64) + 0.03 * sin (9 * u) then 0 else 1
  1 -> if u + 0.8 * v > 0.9 then 1 else 0
  2 -> if sq (u - 0.5) + sq (v - 0.5) < 0.09 then 1 else 0
  3 -> if (u > 0.5) /= (v > 0.5) then 1 else 0
  4 -> if v > 0.5 + 0.28 * cos (pi * (1.6 * u - 0.3)) - 0.18 then 1 else 0
  _ -> if sin (10 * u + 3 * v) > 0 then 1 else 0
  where sq a = a * a

-- n 個、flip = ラベルをわざと入れかえる割合（ノイズ）
makeData2DR :: Int -> Int -> Double -> Int -> R DataSet
makeData2DR kind n flipP seed = do
  seedRng seed
  ps <- mapM (const one) [1 .. n]
  pure (DataSet (V.fromList (map fst ps)) (U.fromList (map snd ps)))
  where one = do
          a <- rnd
          b <- rnd
          let u = 0.02 + 0.96 * a
              v = 0.02 + 0.96 * b
              c = trueLabel kind u v
          f <- rnd
          pure (pt2 u v, if f < flipP then 1 - c else c)

makeData2D :: Int -> Int -> Double -> Int -> DataSet
makeData2D kind n flipP seed = withSeed 1 (makeData2DR kind n flipP seed)

-- ---- k 近傍法：近い k 個のうちクラス 1 の割合 = 確率
data Knn = Knn { knX :: !(U.Vector Double), knY :: !(U.Vector Double), knC :: !(U.Vector Int), knK :: !Int }

fitKnn :: V.Vector Pt -> U.Vector Int -> Int -> Knn
fitKnn xs ys k = Knn (U.fromList [x U.! 0 | x <- V.toList xs]) (U.fromList [x U.! 1 | x <- V.toList xs]) ys k

-- 小さい順の列に挿入（並べ替えはしない。同じ距離なら先に入った点が前）
knnProb :: Knn -> Pt -> Double
knnProb m x = if k == 0 then 0.5 else fromIntegral cnt / fromIntegral k
  where
    n = U.length (knX m)
    k = min (knK m) n
    x0 = x U.! 0
    x1 = x U.! 1
    cnt = runST $ do
      bd <- MU.replicate (max 1 k) 0
      bc <- MU.replicate (max 1 k) (0 :: Int)
      let loop !i !len
            | i >= n = pure ()
            | otherwise = do
                let a = U.unsafeIndex (knX m) i - x0
                    b = U.unsafeIndex (knY m) i - x1
                    d = a * a + b * b
                lastD <- if len > 0 then MU.read bd (len - 1) else pure 0
                if len < k || d < lastD
                  then do
                    let shift q = if q > 0
                                    then do e <- MU.read bd (q - 1)
                                            if e > d then do MU.write bd q e
                                                             MU.read bc (q - 1) >>= MU.write bc q
                                                             shift (q - 1)
                                                     else pure q
                                    else pure q
                    q <- shift (if len < k then len else len - 1)
                    MU.write bd q d
                    MU.write bc q (U.unsafeIndex (knC m) i)
                    loop (i + 1) (min k (len + 1))
                  else loop (i + 1) len
      loop 0 0
      foldM (\s q -> (+ s) <$> MU.read bc q) 0 [0 .. k - 1]

-- ---- ロジスティック回帰（ニュートン法、L2 正則化 λ）。F = 特徴量ベクトルの列、戻り値 = 重み w
dotV :: U.Vector Double -> U.Vector Double -> Double
dotV a b = U.foldl' (+) 0 (U.zipWith (*) a b)

fitLogitVec :: [U.Vector Double] -> U.Vector Int -> Double -> Int -> U.Vector Double
fitLogitVec fs ys lam iters = go 0 (U.replicate d 0)
  where
    d = U.length (headOr U.empty fs)
    yl = U.toList ys
    go it w
      | it >= iters = w
      | otherwise =
          let ps = [ sigm (dotV w f) | f <- fs ]
              rs = zipWith (\p y -> p - fromIntegral y) ps yl
              ss = map (\p -> max 1e-6 (p * (1 - p))) ps
              g = [ foldl' (\acc (r, f) -> acc + r * (f U.! a)) 0 (zip rs fs) + lam * (w U.! a) | a <- [0 .. d - 1] ]
              h = [ [ foldl' (\acc (s, f) -> acc + s * (f U.! a) * (f U.! b)) 0 (zip ss fs) + (if a == b then lam else 0) | b <- [0 .. d - 1] ] | a <- [0 .. d - 1] ]
              dw = solveLin h g 0
              w' = U.fromList (zipWith (\wa da -> wa - clampv da (-20) 20) (U.toList w) dw)
              big = maxOr 0 (map abs dw)
          in if big < 1e-9 then w' else go (it + 1) w'

logitFeat :: Pt -> Bool -> U.Vector Double
logitFeat x quad = let u = x U.! 0 - 0.5
                       v = x U.! 1 - 0.5
                   in U.fromList (if quad then [1, u, v, u * u, v * v, u * v] else [1, u, v])

fitLogit :: V.Vector Pt -> U.Vector Int -> Bool -> Double -> U.Vector Double
fitLogit xs ys quad lam = fitLogitVec (map (`logitFeat` quad) (V.toList xs)) ys lam 25

-- ---- ガウスのナイーブベイズ：クラスごとに、特徴量ごとの平均と分散（特徴量どうしは独立と仮定）
data NB = NB { nbPrior :: ![Double], nbMu :: ![[Double]], nbVa :: ![[Double]] }

fitNB :: V.Vector Pt -> U.Vector Int -> NB
fitNB xs ys = NB [p | (p, _, _) <- per] [m | (_, m, _) <- per] [s | (_, _, s) <- per]
  where
    n = V.length xs
    per = [ let pts = [ x | (i, x) <- zip [0 ..] (V.toList xs), ys U.! i == c ]
                cols = [ map (U.! f) pts | f <- [0, 1] ]
            in ( fromIntegral (length pts + 1) / fromIntegral (n + 2)
               , [ if null col then 0.5 else meanOf col | col <- cols ]
               , [ max 1e-3 (varOf col) | col <- cols ] )
          | c <- [0, 1 :: Int] ]

nbProb :: NB -> Pt -> Double
nbProb m x = sigm (lg 1 - lg 0)
  where
    lg c = log (nbPrior m !! c) + foldl' (term c) 0 [0, 1]
    term c s f = let va = nbVa m !! c !! f
                     dd = x U.! f - nbMu m !! c !! f
                 in s - 0.5 * log (2 * pi * va) - dd * dd / (2 * va)

-- ---- 隠れ層 1 層のニューラルネット（tanh）。cls = True なら出力はシグモイド（交差エントロピー）、False なら直線（2 乗誤差 / 2）
-- パラメータは 1 列：W1（j 行 k 列）、b1、W2、b2 の順。学習はフルバッチの Adam。少しずつ呼べば画面は止まらない
data MLP = MLP { mlIn :: !Int, mlH :: !Int, mlCls :: !Bool, mlP :: !(U.Vector Double), mlT :: !Int
               , mlM :: !(U.Vector Double), mlV :: !(U.Vector Double) }

makeMLPR :: Int -> Int -> Bool -> Int -> R MLP
makeMLPR nIn nH cls seed = do
  seedRng seed
  let s1 = 1.5 / sqrt (fromIntegral nIn)
  rows <- mapM (const (do w1 <- mapM (const ((s1 *) <$> rndGauss)) [1 .. nIn]
                          b1 <- (0.5 *) <$> rndGauss
                          w2 <- (/ sqrt (fromIntegral nH)) <$> rndGauss
                          pure (w1, b1, w2))) [1 .. nH]
  let p = U.fromList (concat [w | (w, _, _) <- rows] ++ [b | (_, b, _) <- rows] ++ [w | (_, _, w) <- rows] ++ [0])
  pure (MLP nIn nH cls p 0 U.empty U.empty)

makeMLP :: Int -> Int -> Bool -> Int -> MLP
makeMLP nIn nH cls seed = withSeed 1 (makeMLPR nIn nH cls seed)

mlpOutP :: Int -> Int -> Bool -> U.Vector Double -> U.Vector Double -> Double
mlpOutP nIn nH cls p x =
  let o = foldl' (\acc j -> let z = foldl' (\zz k -> zz + (p U.! (j * nIn + k)) * (x U.! k)) (p U.! (nH * nIn + j)) [0 .. nIn - 1]
                            in acc + (p U.! (nH * nIn + nH + j)) * tanh z) (p U.! (nH * nIn + 2 * nH)) [0 .. nH - 1]
  in if cls then sigm o else o

mlpOut :: MLP -> U.Vector Double -> Double
mlpOut net = mlpOutP (mlIn net) (mlH net) (mlCls net) (mlP net)

mlpLoss :: MLP -> [U.Vector Double] -> [Double] -> Double
mlpLoss net xs ys =
  let l = foldl' (\s (x, y) -> let p = mlpOut net x
                               in s + (if mlCls net then -(if y /= 0 then log (max 1e-12 p) else log (max 1e-12 (1 - p))) else 0.5 * ((p - y) * (p - y)))) 0 (zip xs ys)
  in if null xs then 0 else l / fromIntegral (length xs)

-- 誤差逆伝播：平均の損失の勾配（パラメータと同じ並び）
mlpGradP :: MLP -> U.Vector Double -> V.Vector (U.Vector Double) -> U.Vector Double -> U.Vector Double
mlpGradP net p xs ys = runST $ do
  let nIn = mlIn net
      nH = mlH net
      nn = fromIntegral (max 1 (V.length xs)) :: Double
      oB1 = nH * nIn
      oW2 = oB1 + nH
      oB2 = oW2 + nH
  g <- MU.replicate (U.length p) 0
  h <- MU.replicate nH 0
  let loopI i | i >= V.length xs = pure ()
              | otherwise = do
                  let x = xs V.! i
                      fwd j !o | j >= nH = pure o
                               | otherwise = do
                                   let z = foldl' (\zz k -> zz + (p U.! (j * nIn + k)) * (x U.! k)) (p U.! (oB1 + j)) [0 .. nIn - 1]
                                       hj = tanh z
                                   MU.write h j hj
                                   fwd (j + 1) (o + (p U.! (oW2 + j)) * hj)
                  o <- fwd 0 (p U.! oB2)
                  let d = ((if mlCls net then sigm o else o) - ys U.! i) / nn
                  MU.modify g (+ d) oB2
                  let back j | j >= nH = pure ()
                             | otherwise = do
                                 hj <- MU.read h j
                                 MU.modify g (+ d * hj) (oW2 + j)
                                 let dz = d * (p U.! (oW2 + j)) * (1 - hj * hj)
                                 MU.modify g (+ dz) (oB1 + j)
                                 mapM_ (\k -> MU.modify g (+ dz * (x U.! k)) (j * nIn + k)) [0 .. nIn - 1]
                                 back (j + 1)
                  back 0
                  loopI (i + 1)
  loopI 0
  U.freeze g

-- Adam（β1 = 0.9, β2 = 0.999）、wd = 重みの減衰
mlpTrain :: MLP -> V.Vector (U.Vector Double) -> U.Vector Double -> Int -> Double -> Double -> MLP
mlpTrain net0 xs ys iters lr wd = go 0 (if U.null (mlM net0) then net0 { mlM = z, mlV = z } else net0)
  where
    z = U.replicate (U.length (mlP net0)) 0
    go it net
      | it >= iters = net
      | otherwise =
          let p = mlP net
              g = mlpGradP net p xs ys
              t = mlT net + 1
              b1t = 1 - 0.9 ** fromIntegral t
              b2t = 1 - 0.999 ** fromIntegral t
              gq = U.zipWith (\gv pv -> gv + wd * pv) g p
              m' = U.zipWith (\mv gv -> 0.9 * mv + 0.1 * gv) (mlM net) gq
              v' = U.zipWith (\vv gv -> 0.999 * vv + 0.001 * gv * gv) (mlV net) gq
              p' = U.zipWith3 (\pv mv vv -> clampv (pv - lr * (mv / b1t) / (sqrt (vv / b2t) + 1e-8)) (-50) 50) p m' v'
          in go (it + 1) net { mlP = p', mlT = t, mlM = m', mlV = v' }

-- ---- 切り株（深さ 1 の木）：特徴量 1 つ × しきい値 1 つ。重み w つき。左右の葉 = クラス 1 の重みの割合（確率）
data Stump = Stump { stFeat :: !Int, stThr :: !Double, stPL :: !Double, stPR :: !Double, stErr :: !Double }

fitStump :: V.Vector Pt -> U.Vector Int -> Maybe (U.Vector Double) -> Stump
fitStump xs ys mw = case best of
    Nothing -> Stump 0 2 ((w1 + 1) / (wT + 2)) 0.5 (min w1 (wT - w1))
    Just (f, thr, lw, l1, err) -> Stump f thr ((l1 + 0.5) / (lw + 1)) ((w1 - l1 + 0.5) / (wT - lw + 1)) err
  where
    n = V.length xs
    wOf i = maybe 1 (U.! i) mw
    (wT, w1) = foldl' (\(!a, !b) i -> let wi = wOf i in (a + wi, if ys U.! i /= 0 then b + wi else b)) (0, 0) [0 .. n - 1]
    best = foldl' scan Nothing [0, 1]
    scan b0 f =
      let ord = U.fromList (sortOn (\i -> xAt xs i f) [0 .. n - 1])
          step (!b, !lw, !l1) q =
            let i = ord U.! q
                wi = wOf i
                lw' = lw + wi
                l1' = if ys U.! i /= 0 then l1 + wi else l1
                v0 = xAt xs i f
                v1 = xAt xs (ord U.! (q + 1)) f
                err = min l1' (lw' - l1') + min (w1 - l1') (wT - lw' - (w1 - l1'))
                b' = if not (v1 > v0) then b else case b of
                       Just (_, _, _, _, e) | not (err < e - 1e-12) -> b
                       _ -> Just (f, (v0 + v1) / 2, lw', l1', err)
            in (b', lw', l1')
          (bb, _, _) = foldl' step (b0, 0, 0) [0 .. n - 2]
      in bb

stumpProb :: Stump -> Pt -> Double
stumpProb m x = if x U.! stFeat m <= stThr m then stPL m else stPR m

-- ---- 5 種類の「ちがうモデル」をまとめて扱う：fitBase 番号 X y → モデル、probOf モデル x → クラス 1 の確率
data Model = MKnn !Knn | MLogit !Bool !(U.Vector Double) | MNB !NB | MMlp !MLP | MStump !Stump

baseNames :: [String]
baseNames = ["k-NN", "logistic", "naive Bayes", "neural net", "stump"]

baseCol :: Int -> RGB                         -- ★ モデルの色
baseCol k = case k of
  0 -> (240, 120, 100)
  1 -> (236, 214, 160)
  2 -> (110, 165, 240)
  3 -> (190, 135, 235)
  _ -> (150, 210, 95)

centerX :: Pt -> U.Vector Double              -- ネットには [−1, 1] に直して渡す
centerX x = U.fromList [2 * x U.! 0 - 1, 2 * x U.! 1 - 1]

fitBase :: Int -> V.Vector Pt -> U.Vector Int -> Model
fitBase kind xs ys = case kind of
  0 -> MKnn (fitKnn xs ys 15)                 -- ★ k = 15
  1 -> MLogit False (fitLogit xs ys False 0.01)   -- 直線の境界
  2 -> MNB (fitNB xs ys)
  3 -> MMlp (mlpTrain (makeMLP 2 10 True 7) (V.map centerX xs) (U.map fromIntegral ys) 300 0.03 0)
  _ -> MStump (fitStump xs ys Nothing)

probOf :: Model -> Pt -> Double
probOf m x = case m of
  MKnn k -> knnProb k x
  MLogit q w -> sigm (dotV w (logitFeat x q))
  MNB nb -> nbProb nb x
  MMlp net -> mlpOut net (centerX x)
  MStump s -> stumpProb s x

predOf :: Model -> Pt -> Int
predOf m x = if probOf m x > 0.5 then 1 else 0

accuracyOf :: (Pt -> Int) -> V.Vector Pt -> U.Vector Int -> Double
accuracyOf fn xs ys = let n = V.length xs
                          ok = length [ () | i <- [0 .. n - 1], fn (xs V.! i) == ys U.! i ]
                      in if n == 0 then 0 else fromIntegral ok / fromIntegral n

-- 投票：hard = 多数決（同数ならソフトの確率で決める）、soft = 確率の平均
hardVote :: [Model] -> Pt -> Int
hardVote ms x = let v = sum (map (`predOf` x) ms)
                    n = length ms
                in if 2 * v > n then 1 else if 2 * v < n then 0 else if softVote ms x > 0.5 then 1 else 0

softVote :: [Model] -> Pt -> Double
softVote ms x = if null ms then 0.5 else foldl' (\s m -> s + probOf m x) 0 ms / fromIntegral (length ms)

-- ---- 描く道具：色（オーケストラ：えんじ色の黒、象牙色、金管の金、弦の青緑）、領域の絵、点
type RGB = (Double, Double, Double)

bgCol, ink, gold, teal, wine :: RGB
bgCol = (24, 11, 16)                          -- 背景（えんじ色の黒）
ink = (242, 233, 214)
gold = (226, 176, 74)
teal = (64, 186, 176)
wine = (196, 72, 96)

clsCol :: Int -> RGB                          -- ★ クラス 0（金）・クラス 1（青緑）の色
clsCol c = if c == 0 then (226, 170, 70) else (60, 182, 176)

colOf :: RGB -> Color
colOf (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) 255

colA :: Int -> RGB -> Color
colA a (r, g, b) = makeColorI (rnd8 r) (rnd8 g) (rnd8 b) a

rnd8 :: Double -> Int
rnd8 v = max 0 (min 255 (round (fin 0 v)))

-- クラス 1 の確率 → 背景と混ぜた色（k = 濃さ）
probCol :: Double -> Double -> RGB
probCol p k = let a = clampv k 0 1 * abs (2 * p - 1) + 0.12
                  (cr, cg, cb) = clsCol (if p > 0.5 then 1 else 0)
                  (br, bg, bb) = bgCol
              in (br + (cr - br) * a, bg + (cg - bg) * a, bb + (cb - bb) * a)

-- 幅 iw × 高さ ih の画像：全部の画素に fn u v → (r, g, b) を塗る（u は右向き、v は上向き、どちらも 0〜1）
paintImage :: Int -> Int -> (Double -> Double -> RGB) -> Picture
paintImage iw ih fn = pixelImage iw ih 1 (\px py ->
  let (r, g, b) = fn ((fromIntegral px + 0.5) / fromIntegral iw) (1 - (fromIntegral py + 0.5) / fromIntegral ih)
  in (w8 r, w8 g, w8 b))
  where w8 v = fromIntegral (rnd8 v) :: Word8

-- マス目の値（行 j = 上から、列 i = 左から）を色にした画像
gridImage :: Int -> (Int -> RGB) -> Picture
gridImage cells fn = pixelImage cells cells 1 (\i j -> let (r, g, b) = fn (i + cells * j) in (w8 r, w8 g, w8 b))
  where w8 v = fromIntegral (rnd8 v) :: Word8

-- マス目の中心の点（p5.js 版の [(i + 0.5)/CELLS, 1 − (j + 0.5)/CELLS]）
cellPt :: Int -> Int -> Pt
cellPt cells q = let (j, i) = q `quotRem` cells
                 in pt2 ((fromIntegral i + 0.5) / fromIntegral cells) (1 - (fromIntegral j + 0.5) / fromIntegral cells)

-- 画像を、左上 (x, y)・幅 w・高さ h の箱に引きのばして置く（画面の道具の座標で）
imageBox :: Float -> Float -> Float -> Float -> Int -> Int -> Picture -> Picture
imageBox x y w h iw ih pic = Translate (x + w / 2) (y + h / 2) (Scale (w / fromIntegral iw) (negate h / fromIntegral ih) pic)

data Box = Box { bxX :: !Float, bxY :: !Float, bxW :: !Float, bxH :: !Float }

toPx :: Box -> Double -> Double -> (Float, Float)   -- [0,1]² → 画面
toPx bx u v = (bxX bx + f2 u * bxW bx, bxY bx + (1 - f2 v) * bxH bx)

fromPx :: Box -> (Float, Float) -> (Double, Double)
fromPx bx (px, py) = (realToFrac ((px - bxX bx) / bxW bx), realToFrac (1 - (py - bxY bx) / bxH bx))

-- 点を描く（直径 r、ふちは黒）
drawPoints :: Box -> V.Vector Pt -> U.Vector Int -> Float -> Picture
drawPoints bx xs ys r = Pictures
  [ Pictures [Color (grayC 10) (circleAt px py (r + 2)), Color (colOf (clsCol (ys U.! i))) (circleAt px py r)]
  | i <- [0 .. V.length xs - 1]
  , let (px, py) = toPx bx (xAt xs i 0) (xAt xs i 1) ]

frameBox :: Box -> RGB -> Picture
frameBox bx c = Color (colOf c) (frameTL 1 (bxX bx) (bxY bx) (bxW bx) (bxH bx))

frameCol :: RGB                               -- 枠のふつうの色
frameCol = (110, 80, 85)

-- ---- グラフの道具：g = { x, y, w, h, x0, x1, y0, y1 }
data Graph = Graph { grX :: !Float, grY :: !Float, grW :: !Float, grH :: !Float, grX0 :: !Double, grX1 :: !Double, grY0 :: !Double, grY1 :: !Double }

gX :: Graph -> Double -> Float
gX g v = grX g + f2 (clampv ((v - grX0 g) / (grX1 g - grX0 g)) (-0.02) 1.02) * grW g

gY :: Graph -> Double -> Float
gY g v = grY g + grH g - f2 (clampv ((v - grY0 g) / (grY1 g - grY0 g)) (-0.02) 1.02) * grH g

gPt :: Graph -> Double -> Double -> (Float, Float)
gPt g a b = (gX g a, gY g b)

plotAxes :: Graph -> String -> String -> [Double] -> [Double] -> Picture
plotAxes g xl yl xt yt = Pictures $
  [ Color (rgbC 95 70 75) (frameTL 1 (grX g) (grY g) (grW g) (grH g)) ]
  ++ [ sayC (prec3 v) (gX g v) (grY g + grH g + 18) 14 (rgbC 175 160 150) | v <- xt ]
  ++ [ sayR (prec3 v) (grX g - 6) (gY g v + 6) 14 (rgbC 175 160 150) | v <- yt ]
  ++ [ sayR xl (grX g + grW g) (grY g + grH g + 40) 14 (rgbC 215 205 190), say yl (grX g) (grY g - 8) 14 (rgbC 215 205 190) ]

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1.2 then Line pts else thickPath w pts
  _ -> Blank

-- 点線（on ピクセル描いて off ピクセルあける）
dashed :: Float -> Float -> Float -> [(Float, Float)] -> Picture
dashed wd on off pts = Pictures (concat (zipWith seg pts (drop 1 pts)))
  where seg (x1, y1) (x2, y2) =
          let l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
              k = max 1 (ceiling (l / (on + off))) :: Int
              p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
              fr = on / (on + off)
          in [ lineW wd (p (fromIntegral i / fromIntegral k)) (p ((fromIntegral i + fr) / fromIntegral k)) | i <- [0 .. k - 1] ]

-- 凡例：items = [(色, 名前)]
legend :: Float -> Float -> [(RGB, String)] -> Picture
legend x y items = Pictures
  [ Pictures [Color (colOf c) (rectTL x (yy - 5) 14 10), say s (x + 22) (yy + 5) 14 (rgbC 220 210 195)]
  | (k, (c, s)) <- zip [0 :: Int ..] items, let yy = y + 20 * fromIntegral k ]

-- ---- 音の道具（Art.Sound）。音量の上限：全体の音量 master は 0.2 以下
master :: Double
master = 0.1                                  -- ★ 全体の音量（0.2 以下にしておく）

midiHz :: Double -> Double
midiHz m = 440 * 2 ** ((m - 69) / 12)

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

-- 右揃えで上下は中央（textAlign(RIGHT, CENTER)）、左揃えで上下は中央（textAlign(LEFT, CENTER)）
sayRM, sayLM :: String -> Float -> Float -> Float -> Color -> Picture
sayRM s x y size c = sayR s x (y + size * 0.4) size c
sayLM s x y size c = say s x (y + size * 0.4) size c

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
frameTL wt x y w h = if wt <= 1.2 then Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
                     else thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分（細いときは gloss の線）
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW wt a b = if wt <= 1.2 then Line [a, b] else thickLine wt a b

-- ---- 数の道具
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
          in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else fmtNum 6 r

-- 小数点以下 d 桁に丸めた数（+v.toFixed(d)）
roundTo :: Int -> Double -> Double
roundTo d v = fromIntegral (jsRoundI (v * 10 ^ d)) / 10 ^ d

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRoundI :: Double -> Int
jsRoundI v = floorI (v + 0.5)

floorI :: Double -> Int
floorI v = if isNaN v || isInfinite v then 0 else floor (clampv v (-1e9) 1e9)

-- JavaScript の toExponential(1)（例 6.7e-9）
toExp1 :: Double -> String
toExp1 v
  | isNaN v || isInfinite v = "?"
  | v == 0 = "0.0e+0"
  | otherwise =
      let a = toRational (abs v)
          e0 = floor (logBase 10 (abs v)) :: Int
          pick e = let n = floor (a / (10 ^^ (e - 1)) + 1 / 2) :: Integer in (n, e)
          (n1, e1) = let (n, e) = pick e0 in if n >= 100 then pick (e0 + 1) else if n < 10 then pick (e0 - 1) else (n, e)
          (n2, e2) = if n1 >= 100 then (n1 `div` 10, e1 + 1) else (n1, e1)
          ds = show n2
      in (if v < 0 then "-" else "") ++ take 1 ds ++ "." ++ drop 1 ds ++ "e" ++ (if e2 >= 0 then "+" else "-") ++ show (abs e2)

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

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

drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) (envW env - 220) (envH env - 12) 14 (if all snd rs then rgbC 140 215 170 else rgbC 255 90 110)
