-- ============================================
-- 46-10 道の分岐と最短路 ─ 二重橋の実験（Deneubourg・Goss）
-- 巣とエサの間に長さの違う 2 本の橋。アリは分かれ道で、フェロモンが濃い方を確率で選ぶ：p = (k + τ)^h / ((k + τ1)^h + (k + τ2)^h)
-- 短い橋を往復したアリは早く戻るので、短い方が先に濃くなる → さらに選ばれる。最後はほとんどが短い橋を使う
-- 操作：←→ = 長さの比 r（長い橋 / 短い橋）/ ↑↓ = アリの流れ / B = 20 回の実験をまとめて行う / R = やり直し / スペース = 一時停止
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftL, shiftR, xor)
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Data.Word (Word64)
import Numeric (showFFloat)

kC, hC, rhoE :: Float
kC = 20                      -- ★ 選ぶ式の定数 k（Deneubourg の実験に合う値）
hC = 2                       -- ★ 選ぶ式の定数 h
rhoE = 0.002                 -- ★ 1 ステップの蒸発率

stepsPerFrame :: Int
stepsPerFrame = 2

-- アリ 1 匹：どの橋か（0 = 短い、1 = 長い）、巣からの道のり、向き（+1 = エサへ、−1 = 巣へ）
data Ant = Ant !Int !Float !Float

-- 1 回の実験
data Sim = Sim
  { smAnts   :: ![Ant]
  , smTau0   :: !Float          -- 短い橋のフェロモン
  , smTau1   :: !Float          -- 長い橋のフェロモン
  , smAcc    :: !Float          -- 巣から出るアリの端数
  , smRecent :: !(Seq.Seq Int)  -- 直近 100 回の選択（1 = 短い橋、古い順）
  , smShortN :: !Int            -- その中で短い橋を選んだ回数
  , smT      :: !Int
  }

newSim :: Sim
newSim = Sim [] 0 0 0 Seq.empty 0 0

data World = World
  { wRatio   :: !Float                 -- 長さの比 r（1 = 同じ長さ）
  , wFlux    :: !Float                 -- 巣から出るアリの数（1 ステップあたり）
  , wPaused  :: !Bool
  , wSim     :: !Sim
  , wTheo    :: !(Float, Float)        -- 理論（平均場）の τ
  , wHist    :: ![(Float, Float)]      -- (実測, 理論) の記録（新しい順、450 個まで）
  , wBatch   :: !(Maybe (Int, Int))    -- まとめ実験：(終わった回数, 短い橋が勝った回数)
  , wResults :: ![(Float, Int)]        -- まとめ実験の結果 (r, 勝ち数)。新しい順
  , wSeed    :: !Seed
  }

world0 :: World
world0 = World 2.0 0.2 False newSim (0, 0) [] Nothing [] 1

main :: IO ()
main = runSketch (sketch "46-10 Double bridge" world0)
  { skBack  = hsb 275 35 12
  , skStart = \_ -> do
      s <- randomRIO (1, 2 ^ (62 :: Int))
      pure world0 { wSeed = s }
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = draw
  , skEvent = \_ ev w -> pure (event ev w)
  }

resetAll :: World -> World
resetAll w = w { wSim = newSim, wTheo = (0, 0), wHist = [] }

-- ★ 分かれ道で短い橋（0）を選ぶ確率
chooseShort :: Float -> Float -> Float
chooseShort t0 t1 = let a = (kC + t0) ** hC
                        b = (kC + t1) ** hC
                    in a / (a + b)

-- 橋の長さ（単位）。短い橋 = 100
lenU :: Float -> Int -> Float
lenU ratio b = if b == 0 then 100 else 100 * ratio

record :: Int -> Sim -> Sim
record b sm = case Seq.viewl r1 of
    old Seq.:< rest | Seq.length r1 > 100 -> sm { smRecent = rest, smShortN = n1 - old }
    _ -> sm { smRecent = r1, smShortN = n1 }
  where
    v = if b == 0 then 1 else 0
    r1 = smRecent sm Seq.|> v
    n1 = smShortN sm + v

shortShare :: Sim -> Float
shortShare sm = if Seq.null (smRecent sm) then 0.5 else fromIntegral (smShortN sm) / fromIntegral (Seq.length (smRecent sm))

-- ★ アリの 1 ステップ：生まれる → 歩く → 端に着いたらフェロモンを置き、エサ側なら帰りの橋を選ぶ
simStep :: Float -> Float -> (Sim, Seed) -> (Sim, Seed)
simStep ratio flux (sm0, s0) = (sm3 { smAnts = reverse kept, smTau0 = t0' * (1 - rhoE), smTau1 = t1' * (1 - rhoE), smT = smT sm0 + 1 }, s2)
  where
    -- 巣から生まれる
    born (sm, s, new)
      | smAcc sm >= 1 =
          let (u, s') = rand01 s
              b = if u < chooseShort (smTau0 sm) (smTau1 sm) then 0 else 1
          in born (record b sm { smAcc = smAcc sm - 1 }, s', Ant b 0 1 : new)
      | otherwise = (sm, s, new)
    (sm1, s1, newAnts) = born (sm0 { smAcc = smAcc sm0 + flux }, s0, [])
    moved = [ Ant b (d + dir) dir | Ant b d dir <- smAnts sm1 ++ reverse newAnts ]
    addTau b v sm = if b == 0 then sm { smTau0 = smTau0 sm + v } else sm { smTau1 = smTau1 sm + v }
    visit (sm, s, keep) a@(Ant b d dir)
      | dir > 0 && d >= l =                                  -- エサに着いた
          let sm' = addTau b (100 / l) sm                    -- 通った橋に置く（長い橋は薄く広がる）
              (u, s') = rand01 s
              b' = if u < chooseShort (smTau0 sm') (smTau1 sm') then 0 else 1
          in (record b' sm', s', Ant b' (lenU ratio b') (-1) : keep)
      | dir < 0 && d <= 0 = (addTau b (100 / l) sm, s, keep)   -- 巣に戻った（いなくなる）
      | otherwise = (sm, s, a : keep)
      where l = lenU ratio b
    (sm3, s2, kept) = foldl' visit (sm1, s1, []) moved
    t0' = smTau0 sm3
    t1' = smTau1 sm3

-- ★ 理論（平均場の式）：dτi/dt = 2 Φ pi (100 / Li) − ρ τi（往復で 2 回通る、遅れは無視）
theoryStep :: Float -> Float -> (Float, Float) -> (Float, Float)
theoryStep ratio flux (c0, c1) =
  let p = chooseShort c0 c1
  in ( c0 + 2 * flux * p * (100 / lenU ratio 0) - rhoE * c0
     , c1 + 2 * flux * (1 - p) * (100 / lenU ratio 1) - rhoE * c1 )

step :: World -> World
step w0 = batchStep (if wPaused w0 then w0 else iterate one w0 !! stepsPerFrame)
  where
    one w =
      let (sm, s) = simStep (wRatio w) (wFlux w) (wSim w, wSeed w)
          th = theoryStep (wRatio w) (wFlux w) (wTheo w)
          h = if smT sm `mod` 4 == 0 then take 450 ((shortShare sm, uncurry chooseShort th) : wHist w) else wHist w
      in w { wSim = sm, wSeed = s, wTheo = th, wHist = h }
    -- まとめ実験：1 フレームに 1 回分（3000 ステップ）
    batchStep w = case wBatch w of
      Just (done, wins) | done < 20 ->
        let (sm, s) = run (3000 :: Int) (newSim, wSeed w)
            run 0 st = st
            run n st = let st'@(a, b) = simStep (wRatio w) (wFlux w) st in a `seq` b `seq` run (n - 1) st'
            wins' = if shortShare sm > 0.5 then wins + 1 else wins
            res = if done + 1 == 20 then (wRatio w, wins') : wResults w else wResults w
        in sm `seq` w { wBatch = Just (done + 1, wins'), wResults = res, wSeed = s }
      _ -> w

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- 橋を描く（太さ = フェロモンの量）
  [ bridge b (if b == 0 then 45 else 300) (if b == 0 then smTau0 sm else smTau1 sm) | b <- [0, 1] ]
  ++ [ Color (hsb 0 0 100) (Pictures [ Translate x y (disc 2.5) | Ant b d _ <- smAnts sm, let (x, y) = pointAt (paths !! b) (d / lenU (wRatio w) b) ]) ]
  ++ [ dot (hsb 30 60 70) (fst n0) (snd n0) 46, dot (hsb 110 60 80) (fst n1) (snd n1) 46
     , txt (fst n0 - 16) (snd n0 + 6) 15 (hsb 0 0 100) "nest", txt (fst n1 - 16) (snd n1 + 6) 15 (hsb 0 0 100) "food"
     , txt ((fst n0 + fst n1) / 2 - 70) (cy + 110 + 34) 15 (hsb 45 70 100) "short bridge (length 1)"
     , txt ((fst n0 + fst n1) / 2 - 70) (envH env * 0.2) 15 (hsb 300 60 100) ("long bridge (length " ++ fmt 2 (wRatio w) ++ ")") ]
  -- ---- 右：短い橋を選ぶ割合（実測と理論）
  ++ [ Color (hsbA 0 0 0 35) (rect (gx + gw / 2) (gy + gh / 2) gw gh)
     , seg 1 (hsbA 0 0 100 20) (gx, gy + gh / 2) (gx + gw, gy + gh / 2) ]
  ++ (if length hist > 1
        then [ path 2 (hsb 45 80 100) [ (gx + fromIntegral i / 449 * gw, gy + gh - a * gh) | (i, (a, _)) <- zip [0 :: Int ..] hist ]
             , path 2 (hsb 180 60 100) [ (gx + fromIntegral i / 449 * gw, gy + gh - t * gh) | (i, (_, t)) <- zip [0 :: Int ..] hist ] ]
        else [])
  ++ [ txt gx (gy - 26) 14 (hsb 0 0 85) "Share choosing the short bridge"
     , txt gx (gy - 8) 14 (hsb 0 0 85) "(yellow = simulation, last 100; cyan = theory)"
     , txt (gx - 16) (gy + 8) 14 (hsb 0 0 60) "1", txt (gx - 30) (gy + gh / 2 + 5) 14 (hsb 0 0 60) "1/2", txt (gx - 16) (gy + gh) 14 (hsb 0 0 60) "0"
     , txt gx (gy + gh + 30) 15 (hsb 0 0 100) ("now: sim " ++ fmt 0 (shortShare sm * 100) ++ " %, theory " ++ fmt 0 (uncurry chooseShort (wTheo w) * 100) ++ " %")
     , txt gx (gy + gh + 56) 15 (hsb 0 0 100) ("tau: short " ++ fmt 0 (smTau0 sm) ++ ", long " ++ fmt 0 (smTau1 sm))
     , txt gx (gy + gh + 82) 15 (hsb 0 0 100) (show (length (smAnts sm)) ++ " ants on bridges, " ++ show (smT sm) ++ " steps")
     -- まとめ実験の結果
     , txt gx by 14 (hsb 0 0 85) "B: in 20 trials, how often did short win?" ]
  ++ [ txt gx (by + 24) 14 (hsb 50 80 100) ("running... " ++ show d ++ " / 20") | Just (d, _) <- [wBatch w], d < 20 ]
  ++ concat [ [ txt gx (by + 48 + fromIntegral i * 22) 14 (hsb 0 0 80) ("r = " ++ fmt 2 r ++ ": " ++ show wins ++ " / 20")
              , Color (hsb 45 80 100) (rect (gx + 185 + bw / 2) (by + 44 + fromIntegral i * 22) bw 12) ]
            | (i, (r, wins)) <- zip [0 :: Int .. 7] (wResults w), let bw = fromIntegral wins / 20 * (gw - 195) ]
  ++ [ txt 20 36 17 (hsb 0 0 100) ("Left/Right: ratio r = " ++ fmt 2 (wRatio w) ++ "   Up/Down: ant flow " ++ fmt 2 (wFlux w)
                                   ++ "   B: 20 trials   R: reset   Space: " ++ (if wPaused w then "paused" else "running"))
     , txt 20 60 14 (hsb 0 0 70) "p = (k + t_short)^2 / ((k + t_short)^2 + (k + t_long)^2). Even at r = 1 one side wins (symmetry breaking)" ]
  where
    sm = wSim w
    hist = reverse (wHist w)
    paths = buildPaths env (wRatio w)
    p0 = head' (paths !! 0)
    n0 = p0
    n1 = last' (paths !! 0)
    cy = envH env * 0.55
    tmax = maximum [smTau0 sm, smTau1 sm, 1]
    bg = hsb 275 35 12
    bridge b hue t =
      let frac = t / tmax
          col = mixC (30 / 100 + 60 / 100 * frac) (hsb hue 70 100) bg       -- 半透明の色を背景と混ぜておく（重なりで濃くならない）
          wd = 3 + 16 * frac
          ps = fst (paths !! b)
      in Color col (Pictures (thickPath wd ps : [ Translate x y (disc (wd / 2)) | (x, y) <- ps ]))
    gx = envW env * 0.66
    gw = envW env - gx - 30
    gy = 120
    gh = 170
    by = gy + gh + 130
    head' (ps, _) = case ps of { (p : _) -> p; [] -> (0, 0) }
    last' (ps, _) = if null ps then (0, 0) else last ps

event :: Event -> World -> World
event ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> resetAll w { wRatio = min 3 (wRatio w + 0.25) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> resetAll w { wRatio = max 1 (wRatio w - 0.25) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wFlux = min 1 (wFlux w + 0.05) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wFlux = max 0.05 (wFlux w - 0.05) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "bB" -> w { wBatch = Just (0, 0) }
    | c `elem` "rR" -> resetAll w
  _ -> w

-- ---------- やってみよう ----------
-- 1. r = 2 で R を何回か押す。短い橋がほぼ毎回勝つ？ B で 20 回まとめて数えよう
-- 2. ← で r = 1（同じ長さ）に。それでも片方に偏る。どちらに偏るかは毎回変わる（最初の偶然が増幅される）
-- 3. 理論（水色）は「平均」の動き。シミュレーション（黄）は数が少ないのでゆらぐ。↑ でアリを増やすと理論に近づく？
-- 4. rhoE を 0 にすると？ 一度偏ると戻れない。0.02 にすると忘れっぽく、偏りが弱くなる
-- 5. hC を 1 にすると「濃い方に比例して選ぶ」だけ。偏りは強くなる？弱くなる？

-- ---------- 道具 ----------
-- 橋の形（折れ線・長さ・位置）。折れ線の点と、始点からの道のりの表を組にする
type Bridge = ([Point], [Float])

polyLen :: [Point] -> Float
polyLen ps = sum (zipWith dist2 ps (drop 1 ps))

makeBranch :: Env -> Float -> Float -> [Point]
makeBranch env sgn wig =
  [(x0, cy), (x0 + 50, cy + off * 0.6), (x0 + 90, cy + off)]
  ++ [ (x0 + 90 + (x1 - x0 - 180) * k / 160, cy + off + sgn * wig * sin (k / 160 * pi * 10)) | i <- [0 .. 160 :: Int], let k = fromIntegral i ]
  ++ [(x1 - 50, cy + off * 0.6), (x1, cy)]
  where
    x0 = envW env * 0.08
    x1 = envW env * 0.58
    cy = envH env * 0.55
    off = 110 * sgn

-- 短い橋はまっすぐ、長い橋はくねらせて長さを r 倍にする（くねりの大きさを二分法で決める）
buildPaths :: Env -> Float -> [Bridge]
buildPaths env ratio = map withLen [short, long]
  where
    short = makeBranch env 1 0
    target = polyLen short * ratio
    bisect :: Int -> Float -> Float -> Float
    bisect 0 lo _ = lo
    bisect n lo hi = let mid = (lo + hi) / 2
                     in if polyLen (makeBranch env (-1) mid) < target then bisect (n - 1) mid hi else bisect (n - 1) lo mid
    long = makeBranch env (-1) (bisect 40 0 300)
    withLen ps = (ps, scanl (+) 0 (zipWith dist2 ps (drop 1 ps)))

-- 橋の上で、割合 u（0 = 巣、1 = エサ）の位置
pointAt :: Bridge -> Float -> Point
pointAt (ps, acc) u = go (zip3 ps (drop 1 ps) (zip acc (drop 1 acc)))
  where
    total = if null acc then 0 else last acc
    d = clampF 0 1 u * total
    go [] = case ps of { (p : _) -> p; [] -> (0, 0) }
    go [((ax, ay), (bx, by), (la, lb))] = lerp ax ay bx by la lb
    go (((ax, ay), (bx, by), (la, lb)) : rest) = if lb < d then go rest else lerp ax ay bx by la lb
    lerp ax ay bx by la lb = let t = (d - la) / max (lb - la) 1e-6 in (ax + (bx - ax) * t, ay + (by - ay) * t)

-- 色 c を透明度 a で背景 bg に重ねた色
mixC :: Float -> Color -> Color -> Color
mixC a c bg = let (r1, g1, b1, _) = rgbaOfColor c
                  (r2, g2, b2, _) = rgbaOfColor bg
              in makeColor (lerpF r2 r1 a) (lerpF g2 g1 a) (lerpF b2 b1 a) 1

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

-- 乱数（xorshift64）。種を World に入れておけば skStep は純粋なまま
type Seed = Word64

nextSeed :: Seed -> Seed
nextSeed s0 = let s1 = s0 `xor` (s0 `shiftL` 13)
                  s2 = s1 `xor` (s1 `shiftR` 7)
              in s2 `xor` (s2 `shiftL` 17)

-- 種を 1 つ進めて 0〜1 の乱数を 1 つ取り出す（順番に使うとき用）
rand01 :: Seed -> (Float, Seed)
rand01 s = let s' = nextSeed s in (fromIntegral (s' `shiftR` 40) / 16777216, s')
