-- ============================================
-- 47-14 結び目を演奏する 🔊
-- 1 = ジョーンズ多項式を演奏：項 c·tᵉ を、次数 e → 音の高さ、|c| → 音の大きさ・回数、符号 → 音色（+ はやわらかい音、− はかたい音）
-- 2 = 組みひもの語を演奏：σᵢ → i 番目の音（ひもの番号が大きいほど高い）、σᵢ⁻¹ → 1 オクターブ下。閉じた組みひもなので、くり返す
-- 結び目がちがえば曲がちがう。鏡像はジョーンズの次数が反対（t ↔ t⁻¹）になるので、メロディーが上下さかさまになる
-- 操作：まずクリック（音を出す合図）/ ←→ = 結び目 / 1・2 = 演奏のしかた / M = 鏡像 / ↑↓ = 速さ / スペース = 止める・鳴らす
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newListArray, readArray, writeArray)
import Data.Bits (popCount, shiftL, testBit)
import Data.List (find, foldl', mapAccumL, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import System.Environment (lookupEnv)
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as U

base :: Float
base = 261.63                -- ★ 基準の音（ド）

scaleSteps :: [Int]
scaleSteps = [0, 2, 4, 7, 9] -- ★ 音階（ペンタトニック：ド レ ミ ソ ラ）

-- 組みひもの語（閉包が表の結び目になるもの。下で不変量を計算して確かめる）
braids :: [(String, ([Int], Int))]
braids =
  [ ("0_1", ([], 1)), ("3_1", ([1, 1, 1], 2)), ("4_1", ([-1, 2, -1, 2], 3)), ("5_1", ([1, 1, 1, 1, 1], 2)), ("5_2", ([1, 1, 2, -1, 2, 1], 3))
  , ("6_1", ([-3, 2, -3, -1, 2, 1, 1], 4)), ("6_2", ([-2, 1, 1, 1, -2, 1], 3)), ("6_3", ([-2, -2, 1, 1, -2, 1], 3)), ("L2a1", ([1, 1], 2))
  , ("L6a4", ([1, -2, 1, -2, 1, -2], 3)) ]

-- 1 つの音：e = 2×(t の次数)、c = 係数（ジョーンズのとき）、g = σ の番号（組みひものとき）
data Note = Note { nE :: !Int, nC :: !Integer, nG :: !Int, nLabel :: String }

data World = World
  { wIdx     :: !Int
  , wMode    :: !Int
  , wMirror  :: !Bool
  , wPlaying :: !Bool
  , wBpm     :: !Float           -- ★ 速さ（1 分間の音の数）
  , wAudio   :: !Bool            -- クリックかキーで音を出しはじめる
  , wSeq     :: [Note]
  , wPos     :: !Int
  , wNextAt  :: !Float           -- 次の音を鳴らす時刻（秒）
  , wV       :: Jones
  , wCheckOk :: !Bool
  , wCheck   :: String
  }

main :: IO ()
main = runSketch (sketch "47-14 Knot music" (rebuild (World 1 1 False True 150 False [] 0 0 M.empty False "")))
  { skBack  = hsb 228 45 16
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

braidOf :: World -> ([Int], Int)
braidOf w = snd (getAt ("0_1", ([], 1)) braids (wIdx w))

nameOf :: World -> String
nameOf w = fst (getAt ("0_1", ([], 1)) braids (wIdx w))

rebuild :: World -> World
rebuild w = w { wV = v, wCheckOk = ok, wCheck = msg, wSeq = sq, wPos = 0 }
  where
    k = knotByName (nameOf w)
    pd = if wMirror w then mirrorPD (kPd k) else kPd k
    v = fromMaybe M.empty (jones pd)
    (w0, m) = braidOf w
    wd = if wMirror w then map negate w0 else w0
    -- 組みひもの閉包の不変量を計算して、表の結び目と同じか確かめる
    dw = diagramOf (braidCurves wd (max m 1) 15 1.6 0.55)
    vw = if null wd then Just (M.singleton 0 1) else jones (dgPd dw)
    (ok, msg) | mEqual vw (Just v) = (True, "OK: the closure has the same V")
              | mEqual vw (Just (jonesMirror v)) = (True, "OK: the closure has the same V (mirror)")
              | otherwise = (False, "NG: no match")
    sq | wMode w == 1 = [ Note e c 0 ("t^" ++ (if even e then show (e `div` 2) else show e ++ "/2"))
                        | (e, c) <- M.toAscList v, _ <- [1 .. abs c] ]
       | otherwise = case [Note 0 0 g ("s" ++ show (abs g) ++ (if g < 0 then "^-1" else "")) | g <- wd] of
           [] -> [Note 0 0 0 "(nothing)"]
           ns -> ns

-- ★ 音の高さ：音階の何番目か → 周波数
freqOf :: Int -> Float
freqOf st = base * 2 ** (fromIntegral o + fromIntegral (getAt 0 scaleSteps s) / 12)
  where
    n = length scaleSteps
    o = st `div` n
    s = st `mod` n

-- 1 音：周波数 f、長さ dur 秒、大きさ vol、音色 wave（高すぎる音は 4000 Hz まで）
playNoteK :: Float -> Float -> Float -> Wave -> IO ()
playNoteK f dur vol wave = when (not (isNaN f) && not (isInfinite f) && f > 0) (playTone (min 4000 f) (min 0.5 vol) dur wave)

step :: Env -> Float -> World -> IO World
step env _ w
  | wPlaying w && wAudio w && envTime env > wNextAt w && not (null (wSeq w)) = do
      let n = getAt (Note 0 0 0 "") (wSeq w) (wPos w `mod` length (wSeq w))
      if wMode w == 1
        then playNoteK (freqOf (nE n + 5)) 0.35 (0.12 + 0.05 * fromIntegral (abs (nC n))) (if nC n > 0 then Triangle else Square)
        else when (nG n /= 0) $
               playNoteK (freqOf (abs (nG n) * 2 + (if nG n < 0 then -5 else 0))) 0.3 0.18 (if nG n > 0 then Sine else Sawtooth)
      pure w { wPos = wPos w + 1, wNextAt = envTime env + 60 / wBpm w }
  | otherwise = pure w

draw :: Env -> World -> Picture
draw env w = view env $
  [ txt 20 36 17 white ("Click first   Left/Right: " ++ nameOf w ++ " (" ++ kTitle (knotByName (nameOf w)) ++ (if wMirror w then ", mirror" else "")
                        ++ ")   1: Jones   2: braid   M: mirror")
  , txt 20 62 15 (hsb 0 0 70) ("Up/Down: speed " ++ show (round (wBpm w) :: Int) ++ "   Space: " ++ (if wPlaying w then "stop" else "play")
                               ++ "      " ++ (if wAudio w then "Sound is on" else "Click the window to start the sound")) ]
  ++ [ txt 40 (110 + fromIntegral k * 28) 22 (hsb 45 40 100) s | (k, s) <- zip [0 :: Int ..] (wrapPoly 60 ("V(t) = " ++ jonesStr (Just v))) ]
  ++ (if wMode w == 1 then jonesBars else braidWord)
  ++ [ txt 40 (envH env - 40) 16 (if wCheckOk w then hsb 130 50 95 else hsb 0 80 100)
           ("Braid " ++ (if null w0 then "(none)" else unwords [(if g > 0 then "+" else "-") ++ show (abs g) | g <- w0]) ++ " closure computed: " ++ wCheck w) ]
  where
    v = wV w
    sq = wSeq w
    nSeq = length sq
    cur = (wPos w - 1 + nSeq) `mod` max 1 nSeq
    curNote = if nSeq > 0 then Just (getAt (Note 0 0 0 "") sq cur) else Nothing
    (w0, m) = braidOf w
    -- 係数の棒グラフ（横 = 次数、高さ = 係数）
    jonesBars =
      let es = M.keys v
          lo = minimum (0 : es)
          hi = maximum (0 : es)
          gx = 80
          gw = envW env - 160
          gy = envH env * 0.55
          xOf e = gx + fromIntegral (e - lo) / fromIntegral (max 1 (hi - lo)) * gw
          half = any odd es                 -- 絡み目は半整数の次数
          negMax = fromIntegral (maximum (0 : [negate c | c <- M.elems v]))   -- 下向きの棒の長さ（説明の文字を棒の下へ）
      in [ Color (hsb 0 0 50) (Line [(gx - 20, gy), (gx + gw + 20, gy)]) ]
         ++ [ txtC (xOf e) (if M.findWithDefault 0 e v < 0 then gy - 14 else gy + 18) 14 (hsb 0 0 60) (if even e then show (e `div` 2) else show e ++ "/2")
            | e <- [lo, lo + (if half then 1 else 2) .. hi] ]
         ++ [ Color (if c > 0 then hsb 38 50 (if on then 100 else 75) else hsb 10 75 (if on then 100 else 70))
                    (rectTL (xOf e - 18) (if c > 0 then gy - fromIntegral c * 60 else gy) 36 (fromIntegral (abs c) * 60))
            | (e, c) <- M.toList v, let on = fmap nE curNote == Just e ]
         ++ [ txt gx (gy + max 60 (60 * negMax + 24)) 14 (hsb 0 0 70) "x: degree of t (left = lower note). Up: coefficient + (soft tone). Down: - (hard tone)" ]
    -- 組みひもの語を左から右へ
    braidWord =
      let cw = min 80 ((envW env - 120) / fromIntegral (max 1 nSeq))
      in concat [ [ Color (if i == cur then hsb 55 80 100 else if nG n > 0 then hsb 38 45 85 else hsb 200 50 85) (at (x + cw / 2, y) (disc (if i == cur then 23 else 18)))
                  , txtC (x + cw / 2) y 15 (hsb 0 0 10) (if nG n == 0 then nLabel n else "s" ++ show (abs (nG n))) ]
                  ++ [ txt (x + cw / 2 + 14) (y - 16) 14 white "-1" | nG n < 0 ]
                | (i, n) <- zip [0 ..] sq
                , let x = 60 + fromIntegral i * cw, let y = envH env * 0.5 - fromIntegral (abs (nG n) - 1) * 50 ]
         ++ [ txt 60 (envH env * 0.5 + 110) 15 (hsb 0 0 70)
                  (show m ++ "-strand braid. Higher = higher note (s1 < s2 < s3). Inverse s^-1 = low note, buzzy tone") ]

event :: Env -> Event -> World -> IO World
event _ ev w0 = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w0 { wAudio = True }
  EventKey key Down _ _ -> keyed key w0 { wAudio = True }
  _ -> w0
  where
    nB = length braids
    keyed key w = case key of
      SpecialKey KeyRight -> rebuild w { wIdx = (wIdx w + 1) `mod` nB }
      SpecialKey KeyLeft -> rebuild w { wIdx = (wIdx w + nB - 1) `mod` nB }
      Char '1' -> rebuild w { wMode = 1 }
      Char '2' -> rebuild w { wMode = 2 }
      Char c | c `elem` "mM" -> rebuild w { wMirror = not (wMirror w) }
      SpecialKey KeyUp -> w { wBpm = min 480 (wBpm w + 30) }
      SpecialKey KeyDown -> w { wBpm = max 60 (wBpm w - 30) }
      SpecialKey KeySpace -> w { wPlaying = not (wPlaying w) }
      _ -> w

-- ---------- やってみよう ----------
-- 1. 3_1 を M で鏡像に。メロディーが上下さかさまになる？ 4_1 と 6_3 は鏡像でも同じ曲（両手型）
-- 2. scaleSteps を [0, 2, 4, 5, 7, 9, 11]（ドレミファソラシ）や [0, 3, 5, 7, 10]（ブルース風）に変えよう
-- 3. 組みひもモードで、5_1（σ₁ を 5 回）と 3_1（3 回）を聞きくらべよう
-- 4. braids に自分の語を足してみよう（08 で作った結び目）。左下の OK で、表の結び目と同じか確かめられる
-- 5. アレクサンダー多項式の係数で和音を作ってみよう（alexander pd は係数のリスト）。playChord が使える

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中心が (x, y) の文字
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y size c s = txt (x - fromIntegral (length s) * size * 0.36) (y + size / 2) size c s

-- 左上が (x, y) の塗りつぶした長方形（p5.js の rect）
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- 長い多項式を項の切れ目（" + " と " - "）で折り返す（1 行 n 文字まで）
wrapPoly :: Int -> String -> [String]
wrapPoly n s = reverse (map reverse (foldl add [] (terms s)))
  where
    terms str = case breakTerm str of
      (a, "") -> [a]
      (a, rest) -> a : terms rest
    breakTerm str = go "" str
      where go acc (' ' : o : ' ' : r) | o `elem` "+-" = (reverse acc, o : ' ' : r)
            go acc (ch : r) = go (ch : acc) r
            go acc [] = (reverse acc, "")
    add [] t = [reverse t]
    add (l : ls) t
      | length l + 1 + length t <= n = (reverse t ++ " " ++ l) : ls
      | otherwise = reverse t : l : ls

-- ==================================================
-- ここから下は「結び目の道具」（中身は読まなくても OK。消さないでね）
-- ==================================================
-- ---- 結び目の表と不変量の計算
-- PD 符号：交点ごとに (i, j, k, l)（辺の番号）。i = 下をくぐって入ってくる辺、そこから反時計回りに j, k, l。
--   下のひもは i → k、上のひもは j と l を結ぶ。番号は KnotTheory / Knot Atlas と同じ約束
-- ジョーンズ多項式の約束：A = t^(−1/4)。右手系三葉（交点の符号が全部 +）は V = t + t³ − t⁴、左手系は −t⁻⁴ + t⁻³ + t⁻¹
-- 計算はすべて整数（Integer）で正確に行う（小数を使わない）
type Cross = (Int, Int, Int, Int)
type PD = [Cross]

data Knot = Knot { kName :: String, kTitle :: String, kPd :: PD }

knots :: [Knot]
knots =
  [ Knot "0_1" "Unknot" []
  , Knot "3_1" "Trefoil" [(1, 4, 2, 5), (3, 6, 4, 1), (5, 2, 6, 3)]
  , Knot "4_1" "Figure-eight knot" [(4, 2, 5, 1), (8, 6, 1, 5), (6, 3, 7, 4), (2, 7, 3, 8)]
  , Knot "5_1" "Cinquefoil knot" [(1, 6, 2, 7), (3, 8, 4, 9), (5, 10, 6, 1), (7, 2, 8, 3), (9, 4, 10, 5)]
  , Knot "5_2" "Three-twist knot" [(1, 4, 2, 5), (3, 8, 4, 9), (5, 10, 6, 1), (9, 6, 10, 7), (7, 2, 8, 3)]
  , Knot "6_1" "Stevedore knot" [(1, 4, 2, 5), (7, 10, 8, 11), (3, 9, 4, 8), (9, 3, 10, 2), (5, 12, 6, 1), (11, 6, 12, 7)]
  , Knot "6_2" "6_2 knot" [(1, 4, 2, 5), (5, 10, 6, 11), (3, 9, 4, 8), (9, 3, 10, 2), (7, 12, 8, 1), (11, 6, 12, 7)]
  , Knot "6_3" "6_3 knot" [(4, 2, 5, 1), (8, 4, 9, 3), (12, 9, 1, 10), (10, 5, 11, 6), (6, 11, 7, 12), (2, 8, 3, 7)]
  , Knot "L2a1" "Hopf link" [(4, 1, 3, 2), (2, 3, 1, 4)]
  , Knot "L5a1" "Whitehead link" [(6, 1, 7, 2), (10, 7, 5, 8), (4, 5, 1, 6), (2, 10, 3, 9), (8, 4, 9, 3)]
  , Knot "L6a4" "Borromean rings" [(6, 1, 7, 2), (12, 8, 9, 7), (4, 12, 1, 11), (10, 5, 11, 6), (8, 4, 5, 3), (2, 9, 3, 10)]
  ]

-- 名前で表をさがす（見つからなければ自明な結び目）
knotByName :: String -> Knot
knotByName nm = fromMaybe (Knot nm "" []) (find ((== nm) . kName) knots)

-- 表の i 番目（範囲の外は端に寄せる）
knotAt :: Int -> Knot
knotAt i = case drop (max 0 (min (length knots - 1) i)) knots of
  (k : _) -> k
  [] -> Knot "0_1" "Unknot" []

mirrorPD :: PD -> PD   -- 鏡像：上下を入れかえる
mirrorPD = map (\(i, j, k, l) -> (i, l, k, j))

crossList :: Cross -> [Int]
crossList (i, j, k, l) = [i, j, k, l]

-- つながりの分類：pairs でつないだとき、各ラベルが何番目のかたまりか（番号は最小のラベルの小さい順）
classesOf :: [Int] -> [(Int, Int)] -> M.Map Int Int
classesOf labels pairs = snd (foldl' visit (0, M.empty) (S.toAscList (S.fromList labels)))
  where
    adj = M.fromListWith (++) (concat [[(a, [b]), (b, [a])] | (a, b) <- pairs])
    visit (nc, seen) a
      | M.member a seen = (nc, seen)
      | otherwise = (nc + 1, flood nc [a] seen)
    flood _ [] seen = seen
    flood c (x : xs) seen
      | M.member x seen = flood c xs seen
      | otherwise = flood c (M.findWithDefault [] x adj ++ xs) (M.insert x c seen)

-- 交点の符号と成分（輪）を調べる。上のひもの向きは「辺の両端の一方が入口、もう一方が出口」から決める
data PdInfo = PdInfo
  { piN :: !Int, piSigns :: [Int], piComp :: M.Map Int Int, piNComp :: !Int
  , piUnderComp :: [Int], piOverComp :: [Int] }

pdInfo :: PD -> PdInfo
pdInfo pd = PdInfo (length pd) signs comp nComp (map (\(i, _, _, _) -> cmp i) pd) (map (\(_, j, _, _) -> cmp j) pd)
  where
    xs = zip [0 :: Int ..] pd
    occ = M.fromListWith (flip (++)) [(a, [(c, p)]) | (c, x) <- xs, (p, a) <- zip [0 :: Int ..] (crossList x)]
    comp = classesOf (M.keys occ) (concat [[(i, k), (j, l)] | (i, j, k, l) <- pd])
    nComp = if M.null comp then 0 else 1 + maximum (M.elems comp)
    cmp a = M.findWithDefault 0 a comp
    -- head (c, p)：その位置で辺が交点へ入ってくるか（わかったものだけ入れる）
    h0 = M.fromList (concat [[((c, 0), True), ((c, 2), False)] | (c, _) <- xs])
    stepLabel h ps = case ps of
      (o1 : o2 : _) -> case (M.lookup o1 h, M.lookup o2 h) of
        (Just h1, Nothing) -> M.insert o2 (not h1) h
        (Nothing, Just h2) -> M.insert o1 (not h2) h
        _ -> h
      _ -> h
    stepCross h (c, _) = case (M.lookup (c, 1) h, M.lookup (c, 3) h) of
      (Just h1, Nothing) -> M.insert (c, 3) (not h1) h
      (Nothing, Just h3) -> M.insert (c, 1) (not h3) h
      _ -> h
    sweep h = let h' = foldl' stepCross (foldl' stepLabel h (M.elems occ)) xs
              in if M.size h' == M.size h then h else sweep h'
    hd = sweep h0
    signs = [if M.lookup (c, 1) hd == Just False then 1 else -1 | (c, _) <- xs]   -- 上が l → j なら +、j → l なら −

writhe :: PD -> Int
writhe = sum . piSigns . pdInfo

-- 絡み数：成分 a と b の間の交点の符号の和 ÷ 2（半整数もありうるので Float）
linkingNumber :: PD -> Int -> Int -> Float
linkingNumber pd a b = fromIntegral (sum [s | (u, o, s) <- zip3 (piUnderComp i) (piOverComp i) (piSigns i), (u == a && o == b) || (u == b && o == a)]) / 2
  where i = pdInfo pd

-- 弧（下をくぐるところで切れた線）：上のひも j と l は同じ弧
pdArcs :: PD -> (M.Map Int Int, Int)
pdArcs pd = (arcOf, if M.null arcOf then 0 else 1 + maximum (M.elems arcOf))
  where arcOf = classesOf (concatMap crossList pd) [(j, l) | (_, j, _, l) <- pd]

arcIx :: M.Map Int Int -> Int -> Int
arcIx m a = M.findWithDefault 0 a m

-- 彩色の行列：交点ごとに 2·上 − 下1 − 下2（= 0 mod n が彩色の条件）
colourMatrix :: PD -> ([[Integer]], Int, M.Map Int Int)
colourMatrix pd = ([row x | x <- pd], nArcs, arcOf)
  where
    (arcOf, nArcs) = pdArcs pd
    row (i, j, k, _) = [fromIntegral (2 * hit (arcIx arcOf j) a - hit (arcIx arcOf i) a - hit (arcIx arcOf k) a) | a <- [0 .. nArcs - 1]]
    hit p a = if p == a then 1 else 0 :: Int

-- 行列の (i, j) 成分（範囲の外は 0）
ix2 :: Num a => [[a]] -> Int -> Int -> a
ix2 m i j = case drop i m of
  (r : _) -> case drop j r of
    (x : _) -> x
    [] -> 0
  [] -> 0

swapAt :: Int -> Int -> [a] -> [a]
swapAt a b xs
  | a == b = xs
  | otherwise = case (drop a xs, drop b xs) of
      (xa : _, xb : _) -> [if q == a then xb else if q == b then xa else x | (q, x) <- zip [0 ..] xs]
      _ -> xs

-- 整数行列を対角化（スミス標準形の手前まで）。M·V = U⁻¹·D。V（列の操作）も記録する
diagonalize :: [[Integer]] -> Int -> ([Integer], [[Integer]])
diagonalize m0 nc = go 0 m0 v0 []
  where
    nr = length m0
    v0 = [[if i == j then 1 else 0 | j <- [0 .. nc - 1]] | i <- [0 .. nc - 1]]
    go t m v ds
      | t >= min nr nc = (map abs (reverse ds ++ replicate (nc - length ds) 0), v)
      | otherwise = let (m', v') = settle t m v in go (t + 1) m' v' (ix2 m' t t : ds)
    settle t m v =
      case [(i, j) | (i, r) <- drop t (zip [0 ..] m), (j, x) <- drop t (zip [0 ..] r), x /= 0] of
        [] -> (m, v)
        cands ->
          let (bi, bj) = foldl' (\b c -> if absAt c < absAt b then c else b) (head' cands) cands
              absAt (i, j) = abs (ix2 m i j)
              head' (c : _) = c
              head' [] = (t, t)
              m1 = map (swapAt t bj) (swapAt t bi m)
              v1 = map (swapAt t bj) v
              p = ix2 m1 t t
              rt = case drop t m1 of { (r : _) -> r; [] -> [] }
              -- 行の消去：行 i −= q·行 t（q = 割り算の商、0 の方向へ切り捨て）
              m2 = [ if i > t then let q = ix2 [r] 0 t `quot` p in if q == 0 then r else [if j >= t then x - q * y else x | (j, x, y) <- zip3 [0 :: Int ..] r rt] else r
                   | (i, r) <- zip [0 :: Int ..] m1 ]
              -- 列の消去：列 j −= q·列 t（M と V の両方）
              qs = [if j > t then ix2 m2 t j `quot` p else 0 | j <- [0 .. nc - 1]]
              colOps r = let rtv = ix2 [r] 0 t in [x - q * rtv | (x, q) <- zip r qs]
              m3 = map colOps m2
              v2 = map colOps v1
              clean = all (\i -> ix2 m3 i t == 0) [t + 1 .. nr - 1] && all (\j -> ix2 m3 t j == 0) [t + 1 .. nc - 1]
          in if clean then (m3, v2) else settle t m3 v2

-- フォックス n 彩色の数 = ∏ gcd(dᵢ, n)（dᵢ = 対角成分。n が素数でなくても正しい）
colourings :: PD -> Integer -> Integer
colourings [] n = n
colourings pd n = product [gcd x n | x <- fst (diagonalize m nArcs)]
  where (m, nArcs, _) = colourMatrix pd

-- k 番目の n 彩色（弧ごとの色のリスト）。0 ≤ k < colourings pd n
colouringAt :: PD -> Integer -> Integer -> [Integer]
colouringAt pd n k0 = [sum (zipWith (*) r ys) `mod` n | r <- v]
  where
    (m, nArcs, _) = colourMatrix pd
    (d, v) = diagonalize m nArcs
    ys = snd (mapAccumL (\k x -> let g = gcd x n in (k `div` g, (k `mod` g) * (n `div` g))) k0 d)

-- 行列式（結び目の行列式）= 対角成分のうち 0 でないものの積（階数が 弧の数 − 1 のとき）
determinant :: PD -> Integer
determinant [] = 1
determinant pd = if length nz == nArcs - 1 then product nz else 0
  where
    (m, nArcs, _) = colourMatrix pd
    nz = filter (/= 0) (fst (diagonalize m nArcs))

-- ---- 多項式（係数のリスト。添字 = t の次数）
type Poly = [Integer]

pTrim :: Poly -> Poly
pTrim a = case reverse (dropWhile (== 0) (reverse a)) of
  [] -> [0]
  r -> r

pAdd :: Poly -> Poly -> Integer -> Poly   -- a + s·b
pAdd a b s = pTrim (zipLong a b)
  where
    zipLong (x : xs) (y : ys) = x + s * y : zipLong xs ys
    zipLong xs [] = xs
    zipLong [] ys = map (s *) ys

pMul :: Poly -> Poly -> Poly
pMul a b = pTrim (foldl' (\acc (i, x) -> addAt i [x * y | y <- b] acc) (replicate (length a + length b - 1) 0) (zip [0 ..] a))
  where addAt i ys acc = take i acc ++ zipWith (+) (drop i acc) (ys ++ repeat 0)

pZero :: Poly -> Bool
pZero a = all (== 0) a

-- 割り切れる割り算（ベアイス法では必ず割り切れる）
pDiv :: Poly -> Poly -> Poly
pDiv a0 b0
  | pZero a || pZero b = [0]
  | otherwise = pTrim (go (length a - lb) a (replicate (max 1 (length a - lb + 1)) 0))
  where
    a = pTrim a0
    b = pTrim b0
    lb = length b
    bl = last b
    go i r q
      | i < 0 = q
      | otherwise =
          let c = ix2 [r] 0 (i + lb - 1) `quot` bl
              r' = [if j >= i && j < i + lb then x - c * ix2 [b] 0 (j - i) else x | (j, x) <- zip [0 ..] r]
          in go (i - 1) r' [if j == i then c else y | (j, y) <- zip [0 ..] q]

pEval :: Poly -> Integer -> Integer
pEval a t = sum (zipWith (*) a (iterate (* t) 1))

pEvalF :: Poly -> Float -> Float
pEvalF a t = sum (zipWith (\c e -> fromIntegral c * t ^^ e) a [0 :: Int ..])

-- 多項式の行列式（ベアイス法：分数を使わない消去法）
polyDet :: [[Poly]] -> Poly
polyDet [] = [1]
polyDet m0 = go 0 m0 [1] 1
  where
    n = length m0
    at2 m i j = case drop i m of { (r : _) -> case drop j r of { (x : _) -> x; [] -> [0] }; [] -> [0] }
    go k m prev sgn
      | k >= n - 1 = map (* sgn) (at2 m (n - 1) (n - 1))
      | otherwise =
          let (m1, sgn1, ok)
                | not (pZero (at2 m k k)) = (m, sgn, True)
                | otherwise = case [s | s <- [k + 1 .. n - 1], not (pZero (at2 m s k))] of
                    (s : _) -> (swapAt k s m, negate sgn, True)
                    [] -> (m, sgn, False)
              mkk = at2 m1 k k
              m2 = [ if i > k then [if j > k then pDiv (pAdd (pMul x mkk) (pMul (at2 m1 i k) (at2 m1 k j)) (-1)) prev else x | (j, x) <- zip [0 :: Int ..] r] else r
                   | (i, r) <- zip [0 :: Int ..] m1 ]
          in if ok then go (k + 1) m2 mkk sgn1 else [0]

-- アレクサンダー行列：上の弧に 1 − t、下の 2 本に t と −1（どちらに t が付くかは交点の符号で決まる）
alexanderMatrix :: PD -> [[Poly]]
alexanderMatrix pd = zipWith row pd (piSigns (pdInfo pd))
  where
    (arcOf, nArcs) = pdArcs pd
    row (i, j, k, _) sg =
      let puts = (arcIx arcOf j, [1, -1]) : (if sg > 0 then [(arcIx arcOf i, [0, 1]), (arcIx arcOf k, [-1])] else [(arcIx arcOf i, [-1]), (arcIx arcOf k, [0, 1])])
      in [foldl' (\p (a', q) -> if a' == a then pAdd p q 1 else p) [0] puts | a <- [0 .. nArcs - 1]]

-- アレクサンダー多項式：最後の行と列を消した小行列式。t^k 倍と符号をそろえる（最低次の係数が正）
alexander :: PD -> Poly
alexander [] = [1]
alexander pd = case dropWhile (== 0) p of
  [] -> [0]
  q@(c : _) -> if c < 0 then map negate q else q
  where
    mat = map dropLast (dropLast (alexanderMatrix pd))
    dropLast xs = take (length xs - 1) xs
    p = pTrim (polyDet mat)

-- ---- カウフマン括弧 → ジョーンズ多項式（ローラン多項式は Map 次数 係数 の形）
type Laurent = M.Map Int Integer

lAdd :: Laurent -> Laurent -> Integer -> Laurent   -- a + s·b
lAdd a b s = M.filter (/= 0) (M.unionWith (+) a (M.map (s *) b))

lMul :: Laurent -> Laurent -> Laurent
lMul a b = M.filter (/= 0) (M.fromListWith (+) [(e + f, x * y) | (e, x) <- M.toList a, (f, y) <- M.toList b])

-- 1 つの状態（bits の c 桁目 = 1 なら交点 c を B に開く）のループの数
stateLoops :: PD -> Int -> Int
stateLoops pd bits = loopCounter pd bits

-- ループを数える道具を作る（ラベルを 0, 1, 2, … に直して、状態ごとに配列で union-find）
loopCounter :: PD -> Int -> Int
loopCounter pd = \bits -> runST (count bits)
  where
    labels = S.toAscList (S.fromList (concatMap crossList pd))
    nL = length labels
    idx = M.fromList (zip labels [0 ..])
    li a = M.findWithDefault 0 a idx
    xs = [(c, li i, li j, li k, li l) | (c, (i, j, k, l)) <- zip [0 ..] pd]
    count :: Int -> ST s Int
    count bits = do
      par <- newListArray (0, max 0 (nL - 1)) [0 .. max 0 (nL - 1)] :: ST s (STUArray s Int Int)
      let root a = do
            p <- readArray par a
            if p == a then pure a else do
              r <- root p
              writeArray par a r
              pure r
          join a b = do
            ra <- root a
            rb <- root b
            when (ra /= rb) (writeArray par ra rb)
      forM_ xs $ \(c, i, j, k, l) ->
        if testBit bits c then join i l >> join j k    -- B：(i,l) と (j,k)
                          else join i j >> join k l    -- A：(i,j) と (k,l)
      rs <- mapM (\a -> (== a) <$> root a) [0 .. nL - 1]
      pure (length (filter id rs))

bracketMax :: Int
bracketMax = 14              -- ★ これより交点が多いと 2^n 状態が多すぎるので計算しない

-- 括弧 ⟨K⟩ = Σ A^(a−b) d^(ループ − 1)、d = −A² − A⁻²（Map A の次数 係数）
bracket :: PD -> Maybe Laurent
bracket [] = Just (M.singleton 0 1)
bracket pd
  | n > bracketMax = Nothing
  | otherwise = Just (M.foldlWithKey' add M.empty counts)
  where
    n = length pd
    loops = loopCounter pd
    -- 同じ（A の次数, ループの数）になる状態をまとめて数える
    counts = M.fromListWith (+) [((n - 2 * popCount s, loops s), 1 :: Integer) | s <- [0 .. shiftL 1 n - 1 :: Int]]
    d = M.fromList [(2, -1), (-2, -1)]
    dPow k = iterate (lMul d) (M.singleton 0 1) !! max 0 k
    add acc (e, l) cnt = lAdd acc (lMul (M.singleton e 1) (dPow (l - 1))) cnt

-- ジョーンズ多項式 V(t) = (−A³)^(−w) ⟨K⟩ を A = t^(−1/4) で。返り値は Map (2×(t の次数)) 係数（絡み目は半整数の次数になるので 2 倍で持つ）
type Jones = M.Map Int Integer

jones :: PD -> Maybe Jones
jones pd = case bracket pd of
  Nothing -> Nothing
  Just br ->
    let w = writhe pd
        f = lMul br (M.singleton (-3 * w) (if even w then 1 else -1))
    in Just (M.fromList [(negate (e `div` 2), c) | (e, c) <- M.toList f])

jonesMirror :: Jones -> Jones
jonesMirror = M.mapKeys negate

jonesEval :: Jones -> Float -> Float
jonesEval v t = sum [fromIntegral c * t ** (fromIntegral e / 2) | (e, c) <- M.toList v]

lEqual :: M.Map Int Integer -> M.Map Int Integer -> Bool
lEqual a b = M.filter (/= 0) a == M.filter (/= 0) b

mEqual :: Maybe Jones -> Maybe Jones -> Bool   -- 両方計算できて等しいか
mEqual (Just a) (Just b) = lEqual a b
mEqual _ _ = False

-- 多項式を文字列に（ASCII）。terms = [(次数の文字列, 係数)]（並んだ順に書く）
termsStr :: [(String, Integer)] -> String -> String
termsStr terms v = case foldl' add "" terms of
  "" -> "0"
  s -> s
  where
    add out (e, c)
      | c == 0 = out
      | otherwise =
          let mono | e == "0" = ""
                   | e == "1" = v
                   | '/' `elem` e = v ++ "^(" ++ e ++ ")"
                   | otherwise = v ++ "^" ++ e
              a = abs c
              body = if null mono then show a else (if a == 1 then "" else show a) ++ mono
          in if null out then (if c < 0 then "-" else "") ++ body else out ++ (if c < 0 then " - " else " + ") ++ body

jonesStr :: Maybe Jones -> String
jonesStr Nothing = "(too many crossings: skipped)"
jonesStr (Just v) = termsStr [(if even e then show (e `div` 2) else show e ++ "/2", c) | (e, c) <- M.toAscList v] "t"

alexStr :: Poly -> String
alexStr p = termsStr (reverse [(show i, c) | (i, c) <- zip [0 :: Int ..] p]) "t"

-- ---- 表から名前をさがす：行列式・アレクサンダー・（計算できれば）ジョーンズが全部一致するもの
data Hit = Hit { hName :: String, hTitle :: String, hHow :: String }
data Ident = Ident { idHits :: [Hit], idV :: Maybe Jones, idA :: String, idD :: Integer, idNc :: Int }

tableInv :: [(Knot, Int, Integer, String, Maybe Jones)]
tableInv = [(k, max 1 (piNComp (pdInfo (kPd k))), determinant (kPd k), alexStr (alexander (kPd k)), jones (kPd k)) | k <- knots]

identify :: PD -> Ident
identify pd = Ident hits v a d nc
  where
    v = jones pd
    a = alexStr (alexander pd)
    d = determinant pd
    nc = max 1 (piNComp (pdInfo pd))
    hits = [ h | (k, tnc, td, ta, tv) <- tableInv, tnc == nc, td == d, ta == a
               , Just h <- [match k tv] ]
    match k tv = case (v, tv) of
      (Nothing, _) -> Just (Hit (kName k) (kTitle k) " (V skipped)")
      (Just x, Just y)
        | lEqual x y -> Just (Hit (kName k) (kTitle k) "")
        | lEqual x (jonesMirror y) -> Just (Hit (kName k) (kTitle k) " (mirror)")
      _ -> Nothing

-- |V(−1)|² ：t^(1/2) = i を入れる（i の h 乗は 1, i, −1, −i をくり返す）。整数のまま 2 乗で返す
jonesAbsSqAtMinus1 :: Jones -> Integer
jonesAbsSqAtMinus1 v = re * re + im * im
  where
    part r = sum [c | (h, c) <- M.toList v, h `mod` 4 == r]
    re = part 0 - part 2
    im = part 1 - part 3

jonesAbsAtMinus1 :: Jones -> Float
jonesAbsAtMinus1 = sqrt . fromIntegral . jonesAbsSqAtMinus1

-- ---- 自己テスト：知られている値と、ここで計算した値を比べる
--   curvesCheck：形（3D 曲線）から作った図の検査（曲線の道具があるスケッチだけ。なければ Nothing）
selfTestWith :: Maybe (Knot -> Bool) -> [(String, Bool)]
selfTestWith curvesCheck =
  [ ("Unknot V = 1", lEqual' (j "0_1") [(0, 1)])
  , ("Twisted loop (R1, +/-) also V = 1", lEqual' (jones [(1, 1, 2, 2)]) [(0, 1)] && lEqual' (jones [(2, 1, 1, 2)]) [(0, 1)])
  , ("Right trefoil w = +3, V = t + t^3 - t^4", writhe r3 == 3 && lEqual' (jones r3) [(2, 1), (6, 1), (8, -1)])
  , ("Left trefoil w = -3, V = -t^-4 + t^-3 + t^-1", writhe l3 == -3 && lEqual' (jones l3) [(-8, -1), (-6, 1), (-2, 1)])
  , ("Figure-eight V = t^-2 - t^-1 + 1 - t + t^2", lEqual' (j "4_1") [(-4, 1), (-2, -1), (0, 1), (2, -1), (4, 1)])
  , ("5_1 V = -t^-7 + t^-6 - t^-5 + t^-4 + t^-2", lEqual' (j "5_1") [(-14, -1), (-12, 1), (-10, -1), (-8, 1), (-4, 1)])
  , ("6_3 V = -t^-3 + 2t^-2 - 2t^-1 + 3 - 2t + 2t^2 - t^3", lEqual' (j "6_3") [(-6, -1), (-4, 2), (-2, -2), (0, 3), (2, -2), (4, 2), (6, -1)])
  , ("Hopf link V = -t^(-1/2) - t^(-5/2), linking number +/-1", lEqual' (j "L2a1") [(-1, -1), (-5, -1)] && abs (linkingNumber (pdOf "L2a1") 0 1) == 1)
  , ("Whitehead: linking number 0, V differs from 2 unlinked rings", linkingNumber (pdOf "L5a1") 0 1 == 0 && lEqual' (j "L5a1") [(-7, 1), (-5, -2), (-3, 1), (-1, -2), (1, 1), (3, -1)])
  , ("Borromean: 3 rings, every pair has linking number 0", piNComp bi == 3 && all (\(a, b) -> linkingNumber bor a b == 0) [(0, 1), (0, 2), (1, 2)]
                                                              && lEqual' (j "L6a4") [(-6, -1), (-4, 3), (-2, -2), (0, 4), (2, -2), (4, 3), (6, -1)])
  , ("Alexander trefoil t^2 - t + 1", al "3_1" == "t^2 - t + 1")
  , ("Alexander figure-eight t^2 - 3t + 1", al "4_1" == "t^2 - 3t + 1")
  , ("Alexander 5_2 2t^2 - 3t + 2, 6_1 2t^2 - 5t + 2", al "5_2" == "2t^2 - 3t + 2" && al "6_1" == "2t^2 - 5t + 2")
  , ("Alexander 6_2 t^4 - 3t^3 + 3t^2 - 3t + 1, 6_3 t^4 - 3t^3 + 5t^2 - 3t + 1", al "6_2" == "t^4 - 3t^3 + 3t^2 - 3t + 1" && al "6_3" == "t^4 - 3t^3 + 5t^2 - 3t + 1")
  , ("Determinant 1, 3, 5, 5, 7, 9, 11, 13", all (\(k, v) -> determinant (pdOf k) == v) [("0_1", 1), ("3_1", 3), ("4_1", 5), ("5_1", 5), ("5_2", 7), ("6_1", 9), ("6_2", 11), ("6_3", 13)])
  , ("Trefoil: 9 3-colourings; figure-eight: 3 (not 3-colourable), 25 5-colourings", colourings l3 3 == 9 && colourings (pdOf "4_1") 3 == 3 && colourings (pdOf "4_1") 5 == 25)
  , ("Whole table: |Delta(-1)| = |V(-1)| = det (3 separate computations agree), knots have V(1) = 1", all tableOk knots) ]
  ++ [ ("Whole table: PD code from the 3D curve = PD code in the table (same V and writhe)", all f knots) | Just f <- [curvesCheck] ]
  ++ [ ("Mirror swaps t <-> 1/t (left and right trefoils differ)", mEqual (jones r3) (jonesMirror <$> jones l3) && not (mEqual (jones r3) (jones l3))) ]
  where
    pdOf = kPd . knotByName
    j = jones . pdOf
    al = alexStr . alexander . pdOf
    l3 = pdOf "3_1"
    r3 = mirrorPD l3
    bor = pdOf "L6a4"
    bi = pdInfo bor
    lEqual' mv xs = case mv of
      Just v -> lEqual v (M.fromList xs)
      Nothing -> False
    tableOk k =
      let d = determinant (kPd k)
      in case jones (kPd k) of
           Nothing -> False
           Just v -> abs (pEval (alexander (kPd k)) (-1)) == d && jonesAbsSqAtMinus1 v == d * d
                       && (take 1 (kName k) == "L" || sum (M.elems v) == 1)   -- V(1) = 係数の和

-- 結果を端末に出す（テスト中は 1 行のまとめと失敗した項目だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(w, ok) -> putStrLn ((if ok then "OK  " else "NG  ") ++ w)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "NG ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- ---- 曲線 → 結び目の図（交点を見つけて PD 符号にする）
-- curves：輪ごとの点の列 [(x, y, z), ...]（閉じた折れ線）。x 右・y 上・z 手前（大きいほうが上を通る）
type P3 = (Float, Float, Float)

segHit :: P3 -> P3 -> P3 -> P3 -> Maybe (Float, Float)
segHit (x1, y1, _) (x2, y2, _) (x3, y3, _) (x4, y4, _)
  | abs d < 1e-12 = Nothing
  | t >= 0 && t < 1 && u >= 0 && u < 1 = Just (t, u)
  | otherwise = Nothing
  where
    d = (x2 - x1) * (y4 - y3) - (y2 - y1) * (x4 - x3)
    t = ((x3 - x1) * (y4 - y3) - (y3 - y1) * (x4 - x3)) / d
    u = ((x3 - x1) * (y2 - y1) - (y3 - y1) * (x2 - x1)) / d

data Seg = Seg { sgCi, sgI :: !Int, sgA, sgB :: !P3, sgX0, sgX1, sgY0, sgY1 :: !Float }

-- 交点を通るところ：輪 eCi の、点の番号 + 割合 eS（eSeg は線分の番号）
data End = End { eCi :: !Int, eS :: !Float, eSeg :: !Int }
-- 通過点：輪にそって並べたときの入ってくる辺 paIn、出ていく辺 paOut
data Pass = Pass { paS :: !Float, paK :: !Int, paOver :: !Bool, paIn, paOut :: !Int }
data Crossing = Crossing
  { crX, crY :: !Float, crO, crU :: !End, crSign :: !Int
  , crDv, crDu :: !(Float, Float)        -- 上・下のひもの向き
  , crPo, crPu :: !Pass }                 -- 上・下の通過点
data Diagram = Diagram { dgPd :: PD, dgCross :: [Crossing], dgPass :: [[Pass]], dgEdges :: !Int }

diagramOf :: [[P3]] -> Diagram
diagramOf = diagramOfWith False Nothing

-- over (ci, i) (ci', i')：交点で 1 本目（輪 ci の線分 i）が上なら True を返す関数（Nothing なら z で決める）
diagramOfWith :: Bool -> Maybe ((Int, Int) -> (Int, Int) -> Bool) -> [[P3]] -> Diagram
diagramOfWith open over curves = Diagram pd crosses passes base
  where
    lens = map length curves
    segs = sortOn sgX0
      [ Seg ci i a b (min ax bx) (max ax bx) (min ay by) (max ay by)
      | (ci, c) <- zip [0 ..] curves, let m = length c, let v = U.fromList c
      , i <- [0 .. (if open then m - 2 else m - 1)]
      , let a@(ax, ay, _) = v U.! i, let b@(bx, by, _) = v U.! ((i + 1) `mod` m) ]
    sweep [] = []
    sweep (sa : rest) = [r | sb <- takeWhile (\s -> sgX0 s <= sgX1 sa) rest, Just r <- [hit sa sb]] ++ sweep rest
    hit sa sb
      | sgY0 sb > sgY1 sa || sgY1 sb < sgY0 sa = Nothing
      | sgCi sa == sgCi sb && (dd <= 1 || (not open && dd == mlen - 1)) = Nothing
      | otherwise = case segHit (sgA sa) (sgB sa) (sgA sb) (sgB sb) of
          Nothing -> Nothing
          Just (ra, rb) ->
            let z3 (_, _, z) = z
                zA = z3 (sgA sa) + (z3 (sgB sa) - z3 (sgA sa)) * ra
                zB = z3 (sgA sb) + (z3 (sgB sb) - z3 (sgA sb)) * rb
                aOver = case over of
                  Just f -> f (sgCi sa, sgI sa) (sgCi sb, sgI sb)
                  Nothing -> zA > zB
                (so, su, ro, ru) = if aOver then (sa, sb, ra, rb) else (sb, sa, rb, ra)
                vec s = let (x0, y0, _) = sgA s; (x1, y1, _) = sgB s in (x1 - x0, y1 - y0)
                dv@(dvx, dvy) = vec so
                du@(dux, duy) = vec su
                (axx, ayy, _) = sgA sa
                (bxx, byy, _) = sgB sa
            in Just ( axx + (bxx - axx) * ra, ayy + (byy - ayy) * ra
                    , End (sgCi so) (fromIntegral (sgI so) + ro) (sgI so), End (sgCi su) (fromIntegral (sgI su) + ru) (sgI su)
                    , if dvx * duy - dvy * dux > 0 then 1 else -1, dv, du )
      where
        dd = abs (sgI sa - sgI sb)
        mlen = case drop (sgCi sa) lens of { (m : _) -> m; [] -> 0 }
    raw = sweep segs
    -- 輪ごとに、通過点（交点を通るところ）を曲線の向きに並べて辺に番号をつける
    nCurves = length curves
    byCurve = IM.fromListWith (flip (++))
      (concat [[(eCi o, [(eS o, k, True)]), (eCi u, [(eS u, k, False)])] | (k, (_, _, o, u, _, _, _)) <- zip [0 ..] raw])
    (base, passes) = mapAccumL label 0 [0 .. nCurves - 1]
    label b ci =
      let ps = sortOn (\(s, _, _) -> s) (IM.findWithDefault [] ci byCurve)
          m = length ps
      in (b + m, [Pass s k ov (b + (if q == 0 then m else q)) (b + q + 1) | (q, (s, k, ov)) <- zip [0 ..] ps])
    passOf = M.fromList [((paK p, paOver p), p) | p <- concat passes]
    getP k ov = M.findWithDefault (Pass 0 k ov 0 0) (k, ov) passOf
    crosses = [Crossing x y o u sg dv du (getP k True) (getP k False) | (k, (x, y, o, u, sg, dv, du)) <- zip [0 ..] raw]
    pd = [ if crSign c > 0 then (i, paOut po, k, paIn po) else (i, paIn po, k, paOut po)   -- i から反時計回りに次の端が j
         | c <- crosses, let pu = crPu c, let po = crPo c, let i = paIn pu, let k = paOut pu ]

-- 折れ線の長さの道具：plAt 長さ で点、plToLen (点の番号 + 割合) で長さ、plPiece a b で長さ a〜b の部分の点の列
data PathL = PathL { plL :: !Float, plAt :: Float -> Point, plToLen :: Float -> Float, plPiece :: Float -> Float -> [Point] }

pathOf :: Bool -> [Point] -> PathL
pathOf closed ptsL = PathL total at toLen piece
  where
    pts = U.fromList ptsL
    n = U.length pts
    segN = if closed then n else n - 1
    pt i = pts U.! (i `mod` max 1 n)
    cum = U.scanl' (+) 0 (U.generate (max 0 segN) (\i -> let (ax, ay) = pt i; (bx, by) = pt (i + 1) in sqrt ((bx - ax) ^ (2 :: Int) + (by - ay) ^ (2 :: Int))))
    cu i = cum U.! max 0 (min segN i)
    total = cu segN
    at s0 =
      let s | closed = s0 - fromIntegral (floor (s0 / total) :: Int) * total
            | otherwise = max 0 (min total s0)
          search lo hi | lo >= hi = lo
                       | otherwise = let m = (lo + hi + 1) `div` 2 in if cu m <= s then search m hi else search lo (m - 1)
          lo = search 0 (segN - 1)
          (ax, ay) = pt lo
          (bx, by) = pt (lo + 1)
          dl = cu (lo + 1) - cu lo
          f = (s - cu lo) / (if dl == 0 then 1 else dl)
      in (ax + (bx - ax) * f, ay + (by - ay) * f)
    toLen s = let i = max 0 (min (floor s) (segN - 1)) in cu i + (cu (i + 1) - cu i) * (s - fromIntegral i)
    piece a b = [at a] ++ go 1 ++ [at b]
      where
        go k | k > 2 * segN = []
             | not closed && k > segN = []
             | otherwise =
                 let s = cu (k `mod` segN) + fromIntegral (k `div` segN) * total
                 in if s >= b then [] else (if s > a then (pt k :) else id) (go (k + 1))

-- 下を通るところにすき間をあけた部分の列（gapS：すき間の位置（点の番号 + 割合）、gap：すき間の半分の長さ）
--   返り値 (q, 点の列)：q 番目のすき間のあとの部分（＝ 1 本の弧。色を変えたいときに使う）
gappedPieces :: Bool -> [Point] -> [Float] -> Float -> [(Int, [Point])]
gappedPieces closed pts gapS gap
  | length pts < 2 || plL p <= 0 = []
  | null gaps = [(0, if closed then pts ++ take 1 pts else pts)]
  | not closed =
      let step (s0, acc) (q, c) = (max s0 (c + gap), if c - gap > s0 then (q - 1, plPiece p s0 (c - gap)) : acc else acc)
          (sEnd, ps) = foldl' step (0, []) (zip [0 ..] gaps)
      in reverse ps ++ [(length gaps - 1, plPiece p sEnd (plL p)) | sEnd < plL p]
  | otherwise =
      [ (q, plPiece p a b) | (q, c, nxt) <- zip3 [0 ..] gaps (drop 1 gaps ++ map (+ plL p) (take 1 gaps))
      , let a = c + gap, let b = nxt - gap, b > a ]
  where
    p = pathOf closed pts
    gaps = sortOn id (map (plToLen p) gapS)

-- すき間つきで 1 本描く（太さ w、色 col）
drawGapped :: Float -> Color -> Bool -> [Point] -> [Float] -> Float -> Picture
drawGapped w col closed pts gapS gap = Color col (Pictures [thickPath w ps | (_, ps) <- gappedPieces closed pts gapS gap])

-- 結び目の図を描く（scr：画面の点の曲線、D：diagramOf の結果、cols：輪ごとの色）
toScreen :: [[P3]] -> Float -> Float -> Float -> [[P3]]
toScreen cs cx cy r = map (map (\(x, y, z) -> (cx + x * r, cy - y * r, z))) cs

drawDiagram :: [[P3]] -> Diagram -> [Color] -> Float -> Float -> Picture
drawDiagram scr dg cols w gap = Pictures
  [ drawGapped w (colAt ci) True [(x, y) | (x, y, _) <- c] [eS (crU x) | x <- dgCross dg, eCi (crU x) == ci] gap
  | (ci, c) <- zip [0 ..] scr ]
  where colAt ci = case drop (ci `mod` max 1 (length cols)) cols of { (c : _) -> c; [] -> white }

-- 点を回転（p5.js の画面と同じ回し方：rotateX(−rx) のあと rotateY(ry)）。返り値も x 右・y 上・z 手前
rotView :: Float -> Float -> P3 -> P3
rotView rx ry (x, y, z) = (x1, -(y1 * cx + z1 * sx), -y1 * sx + z1 * cx)
  where
    cy = cos ry
    sy = sin ry
    cx = cos rx
    sx = sin rx
    x1 = x * cy + z * sy
    z1 = -x * sy + z * cy
    y1 = -y

rotPts :: [[P3]] -> Float -> Float -> [[P3]]
rotPts cs rx ry = map (map (rotView rx ry)) cs

-- ---- 組みひも → 閉じた曲線（円柱のまわりに m 本のひもを並べ、σᵢ で i 番目と i+1 番目を入れかえる）
-- word：[1, -2, 1, ...]（+i = σᵢ、−i = σᵢ⁻¹）。返り値：輪ごとの 3D 点の列（x, y が平面、z が高さ）
braidCurves :: [Int] -> Int -> Int -> Float -> Float -> [[P3]]
braidCurves word m perSlot bigR w = go 0 (replicate m False)
  where
    nL = max (length word) 1
    slots = take nL (word ++ repeat 0)
    -- 位置 s0 から出発したひもを 1 周追いかける：(着いた位置, 点の列)
    path s0 = let (end, ptss) = mapAccumL slot s0 (zip [0 ..] slots) in (end, concat ptss)
    slot pos (s, g) =
      let a = abs g - 1
          (mv, up) | g /= 0 && (pos == a || pos == a + 1) = let mv' = if pos == a then 1 else -1 in (mv', if (mv' > 0) == (g > 0) then 1 else -1)
                   | otherwise = (0, 0) :: (Int, Int)
          pts = [ (r * cos th, r * sin th, 0.35 * fromIntegral up * sin (pi * f))
                | k <- [0 .. perSlot - 1]
                , let f = fromIntegral k / fromIntegral perSlot
                , let e = (1 - cos (pi * f)) / 2
                , let r = bigR + w * (fromIntegral pos + fromIntegral mv * e)
                , let th = 2 * pi * (fromIntegral s + f) / fromIntegral nL ]
      in (pos + mv, pts)
    paths = IM.fromList [(s0, path s0) | s0 <- [0 .. m - 1]]
    go s0 used
      | s0 >= m = []
      | used !! s0 = go (s0 + 1) used
      | otherwise = let (c, used') = follow s0 used [] in c : go (s0 + 1) used'
    follow p used acc
      | p < 0 || p >= m || used !! p = (concat (reverse acc), used)
      | otherwise = let (end, pts) = IM.findWithDefault (p, []) p paths
                    in follow end [u || q == p | (q, u) <- zip [0 ..] used] (pts : acc)
