-- ============================================
-- 50-14 ファジィ作曲
-- マウスの位置 = 2 つの言語変数「明るさ」（左 暗い → 右 明るい）と「激しさ」（下 穏やか → 上 激しい）
--   9 個の規則（マムダニ型、min で切って max で集めて重心）で テンポ・音階・音量 を決めて演奏する
-- 操作：まずクリック（音が出る） / マウス = 気分 / スペース = 再生・停止 / ↑↓ = オクターブ
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Control.Monad (when)
import Data.List (foldl')
import Data.Maybe (fromMaybe, isJust)
import System.Environment (lookupEnv)
import Numeric (showFFloat)

lvNames, leNames :: [String]
lvNames = ["dark", "mid", "bright"]      -- 明るさ：暗い・ふつう・明るい
leNames = ["calm", "mid", "intense"]     -- 激しさ：穏やか・ふつう・激しい

-- ★ 規則の表 [明るさ][激しさ] → 後件の番号（0 小さい・1 中くらい・2 大きい）
tempoR, scaleR, volR :: [[Int]]
tempoR = [[0, 0, 1], [0, 1, 2], [1, 2, 2]]   -- 遅い・中くらい・速い
scaleR = [[0, 0, 1], [1, 1, 1], [2, 2, 1]]   -- 暗い音階・中間・明るい音階
volR   = [[0, 1, 2], [0, 1, 2], [1, 1, 2]]   -- 小・中・大

-- 音階（暗い → 明るい の順に 7 つの旋法）
modes :: [(String, [Int])]
modes =
  [ ("Locrian", [0, 1, 3, 5, 6, 8, 10]), ("Phrygian", [0, 1, 3, 5, 7, 8, 10])
  , ("Aeolian (minor)", [0, 2, 3, 5, 7, 8, 10]), ("Dorian", [0, 2, 3, 5, 7, 9, 10])
  , ("Mixolydian", [0, 2, 4, 5, 7, 9, 10]), ("Ionian (major)", [0, 2, 4, 5, 7, 9, 11])
  , ("Lydian", [0, 2, 4, 6, 7, 9, 11]) ]

-- 鳴った音（音階の段・音量・色相）
data Note = Note { nDeg :: !Int, nVol :: !Double, nHue :: !Float }

data World = World
  { wB       :: !Double     -- 明るさ 0〜1
  , wE       :: !Double     -- 激しさ 0〜1
  , wPlaying :: !Bool
  , wAudio   :: !Bool       -- クリックかキーで音を出しはじめる
  , wNextAt  :: !Float      -- 次の音を鳴らす時刻 [秒]
  , wDeg     :: !Int        -- 今の音階の段（0〜13）
  , wBeat    :: !Int
  , wNotes   :: [Note]      -- 最近の 48 音
  , wBase    :: !Double     -- ★ いちばん下の音の高さ [Hz]（最初は 220）
  , wTest    :: [(String, Bool)]
  }

main :: IO ()
main = runSketch (sketch "50-14 Fuzzy music" world0)
  { skBack  = hsb 250 18 14
  , skStart = \_ -> do
      printSelfTest selfTest
      pure world0 { wTest = selfTest }
  , skStep  = step
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 0.5 0.5 True False 0 7 0 [] 220 []

-- 入力 2 つは共通、出力ごとに 1 つのマムダニ系（テンポ・音階の明るさ・音量）
systems :: [Sys]
systems = [ mk "Tempo [BPM]" 50 180 ["slow", "mid", "fast"] tempoR
          , mk "Scale brightness" 0 6 ["dark", "mid", "bright"] scaleR
          , mk "Volume" 0 1 ["soft", "mid", "loud"] volR ]
  where
    ins = [Var "Brightness" "" 0 1 (evenTri 0 1 lvNames), Var "Intensity" "" 0 1 (evenTri 0 1 leNames)]
    mk nm lo hi names r = Sys ins (Var nm "" lo hi (evenTri lo hi names))
                              [ Rule [i, j] k | (i, row) <- zip [0 ..] r, (j, k) <- zip [0 ..] row ] False False 401

infer :: World -> [Res]
infer w = [ mamdani s [wB w, wE w] | s <- systems ]

-- パッドの位置と大きさ
padOf :: Env -> (Float, Float, Float)
padOf env = (40, 120, min (envW env * 0.36) (envH env - 220))

-- マウスがパッドの上なら気分を変え、時刻が来たら 1 音鳴らす（8 分音符ごと。音階の中をふらふら歩く）
step :: Env -> Float -> World -> IO World
step env _ w0 = do
  let (px, py, ps) = padOf env
      (mx, my) = p5Mouse env
      w = if mx > px && mx < px + ps && my > py && my < py + ps
            then w0 { wB = clampv 0 1 (realToFrac ((mx - px) / ps)), wE = clampv 0 1 (1 - realToFrac ((my - py) / ps)) }
            else w0
      rs = infer w
      tempo = resY (getAt dummyRes rs 0)
      modeI = modeIndex rs
      vol = resY (getAt dummyRes rs 2)
  if wPlaying w && wAudio w && envTime env >= wNextAt w
    then do
      let dt = 60 / tempo / 2
      r <- randomRIO (-2, 2 :: Int)
      let d = max 0 (min 13 (wDeg w + r))
          sc = snd (getAt ("", [0]) modes modeI)
          semi = 12 * (d `div` 7) + getAt 0 sc (d `mod` 7)
          e = wE w
      playNoteK (wBase w * 2 ** (fromIntegral semi / 12)) (dt * (1.6 - e)) (0.04 + 0.3 * vol) (if e > 0.6 then Sawtooth else Triangle)
      when (wBeat w `mod` 4 == 0) $ playNoteK (wBase w / 2) (dt * 3) (0.03 + 0.2 * vol) Sine
      let nt = Note d vol (20 + fromIntegral modeI * 30)
      pure w { wNextAt = envTime env + realToFrac dt, wDeg = d, wBeat = wBeat w + 1
             , wNotes = drop (length (wNotes w) + 1 - 48) (wNotes w ++ [nt]) }
    else pure w

-- 1 音：周波数 f [Hz]、長さ dur 秒（0.05〜3）、大きさ vol（0.5 まで）、音色 wave（高すぎる音は 4000 Hz まで）
playNoteK :: Double -> Double -> Double -> Wave -> IO ()
playNoteK fr dur vol wave =
  when (not (isNaN fr) && not (isInfinite fr) && fr > 0) $
    playTone (realToFrac (min 4000 fr)) (realToFrac (max 0.0002 (min 0.5 vol))) (realToFrac (max 0.05 (min 3 dur))) wave

modeIndex :: [Res] -> Int
modeIndex rs = jsRound (clampv 0 6 (resY (getAt dummyRes rs 1)))

dummyRes :: Res
dummyRes = Res [] [] [] [] [] 0 False

draw :: Env -> World -> Picture
draw env w = view env $
  -- パッド：9 つの規則のマス（色の濃さ = 発火の強さ）
  concat [ let k = i * 3 + j
               wk = realToFrac (getAt 0 ws k)
               x = px + fromIntegral i * cell
               y = py + fromIntegral (2 - j) * cell
               tc = hsba 0 0 100 ((50 + 50 * wk) / 100)
           in [ Color (hsba 20 45 100 ((8 + 70 * wk) / 100)) (rectTL x y cell cell)
              , Color (hsba 0 0 100 0.2) (rectLineTL x y cell cell)
              , txt (x + 8) (y + 20) 14 tc (getAt "" lvNames i ++ " AND " ++ getAt "" leNames j)
              , txt (x + 8) (y + 38) 14 tc ("-> " ++ getAt "" ["slow", "mid", "fast"] (at2 tempoR i j) ++ "/" ++ getAt "" ["dark", "mid", "bright"] (at2 scaleR i j)
                                            ++ "/" ++ getAt "" ["soft", "mid", "loud"] (at2 volR i j))
              , txt (x + 8) (y + cell - 12) 16 tc ("w = " ++ fmt 2 (getAt 0 ws k)) ]
         | i <- [0 .. 2], j <- [0 .. 2] ]
  ++ [ Color (hsb 175 60 80) (at (px + realToFrac (wB w) * ps, py + realToFrac (1 - wE w) * ps) (disc 9))
     , txt px (py + ps + 24) 14 (hsb 0 0 80) ("brightness " ++ fmt 2 (wB w) ++ ": " ++ unwords [ n ++ " " ++ fmt 2 d | (n, d) <- zip lvNames (getAt [] degs 0) ])
     , txt px (py + ps + 44) 14 (hsb 0 0 80) ("intensity  " ++ fmt 2 (wE w) ++ ": " ++ unwords [ n ++ " " ++ fmt 2 d | (n, d) <- zip leNames (getAt [] degs 1) ]) ]
  -- 鳴った音（右へ流れる。p5.js より 20 下げて、文字と重ならないように）
  ++ [ Color (hsba (nHue n) 45 100 ((30 + 70 * fromIntegral i / fromIntegral (length (wNotes w))) / 100))
         (rectTL (px + fromIntegral i / 48 * ps) (py + ps + 100 - fromIntegral (nDeg n) * 3) (ps / 52) (6 + realToFrac (nVol n) * 10))
     | (i, n) <- zip [0 :: Int ..] (wNotes w) ]
  -- 右：3 つの出力（切った後件を max で集めた形と、重心）
  ++ concat [ outPanel q s r | (q, (s, r)) <- zip [0 :: Int ..] (zip systems rs) ]
  ++ [ txt 20 36 17 (hsb 0 0 95) ((if wAudio w then (if wPlaying w then "Playing" else "Stopped") else "Click the window to start the sound")
                                   ++ "  |  mouse = mood (x brightness, y intensity)   Space: play/stop   Up/Down: octave " ++ show (round (wBase w) :: Int) ++ " Hz")
     , txt 20 62 15 (hsb 0 0 70) "w = min(mu brightness, mu intensity). Clip consequents at w, overlay by max, centroid = int x mu dx / int mu dx"
     , txt 20 (envH env - 20) 14 (if all snd (wTest w) then hsb 160 50 90 else hsb 0 80 100) (selfTestLine (wTest w)) ]
  where
    (px, py, ps) = padOf env
    cell = ps / 3
    rs = infer w
    r0 = getAt dummyRes rs 0
    ws = resW r0
    degs = resDeg r0
    modeI = modeIndex rs
    at2 tbl i j = getAt 0 (getAt [] tbl i) j
    ox = px + ps + 60
    ow = envW env - ox - 40
    oh = (envH env - 200) / 3 - 20
    outPanel q s r =
      let oy = 110 + fromIntegral q * (oh + 30)
          o = sOutput s
          xOf v = ox + realToFrac ((v - vLo o) / (vHi o - vLo o)) * ow
          yOf m = oy + oh - realToFrac m * (oh - 30)
          pts = [ (xOf x, yOf y) | (x, y) <- every 4 (zip (resOx r) (resAgg r)) ]
          val | q == 0 = fmt 1 (resY r) ++ " BPM"
              | q == 1 = fmt 2 (resY r) ++ " -> " ++ fst (getAt ("", []) modes modeI)
              | otherwise = fmt 3 (resY r)
      in [ Color (hsba 0 0 100 0.05) (rectTL (ox - 10) (oy - 6) (ow + 20) (oh + 14)) ]
         ++ [ Color (hsba 0 0 100 0.3) (thickPath 1.5 [ (xOf v, yOf (evalMF (fsMF fs) v)) | k <- [0 .. 60 :: Int], let v = vLo o + ((vHi o - vLo o) * fromIntegral k) / 60 ])
            | fs <- vSets o ]
         ++ [ Color (hsba 20 45 100 0.45) (Pictures [ Polygon [(x0, yOf 0), (x0, y0), (x1, y1), (x1, yOf 0)] | ((x0, y0), (x1, y1)) <- zip pts (drop 1 pts) ])
            , Color (hsb 175 60 80) (thickLine 3 (xOf (resY r), yOf 0) (xOf (resY r), yOf 1))
            , txt ox (oy + 14) 16 (hsb 0 0 95) (vName o ++ "   centroid = " ++ val)
            , txt ox (oy + 34) 14 (hsb 0 0 70) ("consequent strength  " ++ unwords [ fsName fs ++ " " ++ fmt 2 st | (fs, st) <- zip (vSets o) (resStr r) ]) ]

-- k 個おきに取り出す（最後の点は必ず入れる）
every :: Int -> [a] -> [a]
every k = go 0
  where
    go :: Int -> [a] -> [a]
    go _ [] = []
    go _ [z] = [z]
    go i (z : zs) = if i `mod` k == 0 then z : go (i + 1) zs else go (i + 1) zs

event :: Env -> Event -> World -> IO World
event _ ev w0 = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w0 { wAudio = True }
  EventKey key Down _ _ -> keyed key w0 { wAudio = True }
  _ -> w0
  where
    keyed key w = case key of
      SpecialKey KeySpace -> w { wPlaying = not (wPlaying w) }
      SpecialKey KeyUp -> w { wBase = min 880 (wBase w * 2) }
      SpecialKey KeyDown -> w { wBase = max 55 (wBase w / 2) }
      _ -> w

-- ---- 自己テスト：手で計算できる値と比べる
selfTest :: [(String, Bool)]
selfTest =
  [ ("at the center (mid, mid) exactly one rule fires, with strength 1", length (filter (> 0) (resW m)) == 1 && getAt 0 (resW m) 4 == 1)
  , ("then the tempo = centroid of the symmetric 'mid' = 115", nearTol 1e-6 (resY m) 115)
  , ("bottom-left (dark, calm): half triangle at the end -> centroid 50 + 65/3 = 71.67", nearTol 1e-6 (resY (run 0 0 0)) (50 + 65 / 3))
  , ("mid, intense -> volume 'loud' (half triangle 0.5..1) -> centroid 1 - 0.5/3 = 0.833", nearTol 1e-6 (resY (run 2 0.5 1)) (1 - 0.5 / 3))
  , ("bright, mid -> scale centroid 5 -> Ionian (major)", nearTol 1e-6 (resY (run 1 1 0.5)) 5 && take 6 (fst (getAt ("", []) modes 5)) == "Ionian")
  , ("brightness 0.25 (dark 0.5, mid 0.5): centroid of the clipped, overlaid shape = " ++ fmt 3 c, nearTol 1e-6 (resY (run 0 0.25 0.5)) c)
  , ("input memberships always sum to 1 (evenly spaced triangles)", okSum) ]
  where
    run q b e = mamdani (getAt (Sys [] (Var "" "" 0 1 []) [] False False 401) systems q) [b, e]
    m = run 0 0.5 0.5
    a1 = 97.5 * 0.5
    a2 = 0.5 * 32.5 * 0.5
    c = (a1 * 98.75 + a2 * (147.5 + 32.5 / 3)) / (a1 + a2)
    okSum = and [ nearTol 1e-12 (sum (getAt [] (resDeg (run 0 (fromIntegral k / 19) 0.3)) 0)) 1 | k <- [0 .. 19 :: Int] ]

-- ---------- やってみよう ----------
-- 1. 4 すみと真ん中にマウスを置いて、どの規則が発火しているか見よう。真ん中は 1 つだけ、マスの境目では 2〜4 つ
-- 2. tempoR の表を書きかえる（例：暗い・激しい を 2 = 速い に）。パッドの左上の音が変わる
-- 3. systems の mk の最後の False False を False True（積で縮める）にすると、重心は少し変わる。耳でわかる？
-- 4. modes に自分の音階（例 ペンタトニック [0, 2, 4, 7, 9, 12, 14]）を入れてみよう
-- 5. 規則を 5×5 に増やしてみよう（lvNames と leNames を 5 個に、表も 5×5 に）

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標（左上が原点、y は下向き）で
p5Mouse :: Env -> (Float, Float)
p5Mouse env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 左上が (x, y) の塗りつぶした長方形（p5.js の rect）
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠
rectLineTL :: Float -> Float -> Float -> Float -> Picture
rectLineTL x y w h = Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]

-- Double → Float（描くときだけ）
f :: Double -> Float
f = realToFrac

-- リストの i 番目（なければ d）
getAt :: a -> [a] -> Int -> a
getAt d xs i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> d

-- JavaScript の Math.round（.5 は大きいほうへ）
jsRound :: Double -> Int
jsRound v = floor (v + 0.5)

-- 小数点以下 n 桁で書く（p5.js の toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) v ""

-- ==================================================
-- ここから下は「ファジィの道具」（中身は読まなくても OK。消さないでね）
-- 計算はすべて Double（倍精度）で行う
-- ==================================================
-- ---- メンバーシップ関数（所属度 μ を 0〜1 で返す）
trimf :: Double -> Double -> Double -> Double -> Double      -- 三角：a で 0、b で 1、c で 0
trimf x a b c
  | x < a || x > c = 0
  | x == b = 1
  | x < b = if b > a then (x - a) / (b - a) else 1
  | otherwise = if c > b then (c - x) / (c - b) else 1

trapmf :: Double -> Double -> Double -> Double -> Double -> Double   -- 台形：b〜c が 1
trapmf x a b c d
  | x < a || x > d = 0
  | x >= b && x <= c = 1
  | x < b = (x - a) / (b - a)
  | otherwise = (d - x) / (d - c)

gaussmf :: Double -> Double -> Double -> Double
gaussmf x c s = exp (-((x - c) * (x - c)) / (2 * s * s))

sigmf :: Double -> Double -> Double -> Double
sigmf x a c = 1 / (1 + exp (-a * (x - c)))

bellmf :: Double -> Double -> Double -> Double -> Double
bellmf x a b c = 1 / (1 + abs ((x - c) / a) ** (2 * b))

-- メンバーシップ関数の形とパラメータ
data MF = Tri !Double !Double !Double | Trap !Double !Double !Double !Double
        | Gauss !Double !Double | Sig !Double !Double | Bell !Double !Double !Double

evalMF :: MF -> Double -> Double
evalMF mf x = case mf of
  Tri a b c -> trimf x a b c
  Trap a b c d -> trapmf x a b c d
  Gauss c s -> gaussmf x c s
  Sig a c -> sigmf x a c
  Bell a b c -> bellmf x a b c

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- ---- 非ファジィ化（xs, ys は同じ長さ。ys は折れ線の高さ）
-- 重心 = ∫x μ dx ÷ ∫μ dx（折れ線の区間ごとに正確に積分）。どの規則も発火しなければ Nothing
centroid :: [Double] -> [Double] -> Maybe Double
centroid xs ys = if a > 1e-12 then Just (m / a) else Nothing
  where
    ps = zip xs ys
    (a, m) = foldl' step (0, 0) (zip ps (drop 1 ps))
    step (!aa, !mm) ((x0, y0), (x1, y1)) =
      let h = x1 - x0
      in (aa + (h * (y0 + y1)) / 2, mm + (h * (x0 * (2 * y0 + y1) + x1 * (y0 + 2 * y1))) / 6)

areaOf :: [Double] -> [Double] -> Double
areaOf xs ys = foldl' (\acc ((x0, y0), (x1, y1)) -> acc + ((x1 - x0) * (y0 + y1)) / 2) 0 (zip ps (drop 1 ps))
  where ps = zip xs ys

-- ---- マムダニ型推論
-- 言語変数：名前・単位・範囲・ファジィ集合の列。規則：前件の集合の番号（入力ごと、−1 は使わない）と後件の番号
data FSet = FSet { fsName :: String, fsMF :: MF }
data Var = Var { vName :: String, vUnit :: String, vLo :: !Double, vHi :: !Double, vSets :: [FSet] }
data Rule = Rule { rIfs :: [Int], rThen :: !Int }
data Sys = Sys { sInputs :: [Var], sOutput :: Var, sRules :: [Rule]
               , sAndProd :: !Bool     -- 「かつ」：False = min、True = 積
               , sImpProd :: !Bool     -- 含意：False = 切る（min）、True = 縮める（積）
               , sN :: !Int }          -- 出力を何点で積分するか

-- 推論の途中経過を全部返す：所属度・規則の強さ・後件ごとの強さ・集約した形・重心
data Res = Res { resDeg :: [[Double]], resW :: [Double], resStr :: [Double]
               , resOx :: [Double], resAgg :: [Double], resY :: !Double, resFired :: !Bool }

mamdani :: Sys -> [Double] -> Res
mamdani sys xin = Res deg ws str ox agg (fromMaybe ((vLo o + vHi o) / 2) c) (isJust c)
  where
    deg = [ [evalMF (fsMF s) (clampv (vLo v) (vHi v) x) | s <- vSets v] | (v, x) <- zip (sInputs sys) xin ]
    ws = [ foldl' andF 1 (zip [0 ..] (rIfs r)) | r <- sRules sys ]
    andF acc (i, si)
      | si < 0 = acc
      | sAndProd sys = acc * d
      | otherwise = min acc d
      where d = getAt 0 (getAt [] deg i) si
    o = sOutput sys
    str = [ foldl' max 0 [wv | (r, wv) <- zip (sRules sys) ws, rThen r == k] | k <- [0 .. length (vSets o) - 1] ]   -- 同じ後件は max でまとめる
    n = sN sys
    ox = [ vLo o + ((vHi o - vLo o) * fromIntegral i) / fromIntegral (n - 1) | i <- [0 .. n - 1] ]
    live = [ (s, fsMF fs) | (s, fs) <- zip str (vSets o), s > 0 ]
    agg = [ foldl' (\a (s, mf) -> let m = evalMF mf x in max a (if sImpProd sys then s * m else min s m)) 0 live | x <- ox ]   -- 切る（min）か縮める（積）
    c = centroid ox agg

-- 等間隔の三角 MF を n 個（両端は肩）
evenTri :: Double -> Double -> [String] -> [FSet]
evenTri lo hi names = [ FSet nm (Tri (lo + fromIntegral (i - 1) * d) (lo + fromIntegral i * d) (lo + fromIntegral (i + 1) * d))
                      | (i, nm) <- zip [0 :: Int ..] names ]
  where d = (hi - lo) / fromIntegral (length names - 1)

-- ---- 自己テストの道具
near :: Double -> Double -> Bool
near a b = nearTol 1e-9 a b

nearTol :: Double -> Double -> Double -> Bool
nearTol tol a b = abs (a - b) <= tol

-- 値がないこともある数（Nothing は不合格）
nearM :: Double -> Maybe Double -> Double -> Bool
nearM tol ma b = maybe False (\a -> nearTol tol a b) ma

-- 結果を端末に出す（テスト中は 1 行のまとめと失敗した項目だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK  " else "NG  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "NG ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)
