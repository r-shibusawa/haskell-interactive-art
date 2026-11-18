{-# LANGUAGE BangPatterns #-}
-- ============================================
-- 54-12 アンテナからの電波 ─ 振動する電気双極子のまわりの電気力線
-- まん中の短いアンテナで ＋ と − が入れかわり続ける（ヘルツの双極子）。電場は式で正確にわかる（解析解）
-- 電気力線 = 関数 Ψ = sin²θ·(cos(kr − ωt)/(kr) + sin(kr − ωt)) の等高線。近くでは双極子の形、遠くでは輪が切りはなされて光の速さで飛んでいく
-- 操作：←→ = 波長（周波数）/ スペース = 止める / C = 色 / ↑↓ = 線の数 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Control.Monad (forM, when)
import Data.List (sort)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Numeric (showFFloat)
import System.Environment (lookupEnv)

cpx :: Double
cpx = 110                              -- ★ 画面の上での光の速さ [px/秒]（本物は 3×10⁸ m/s）

pxM :: Double
pxM = 0.01                             -- ★ 1 px = 1 cm として周波数を計算

cell :: Int
cell = 6                               -- 等高線の格子 [px]

data World = World
  { wLam    :: !Double                 -- 波長 [px]
  , wPhase  :: !Double                 -- ωt
  , wPaused :: !Bool
  , wColor  :: !Bool
  , wNLev   :: !Int                    -- 線の数
  , wGx     :: !Int                    -- 格子の数（横・たて）
  , wGy     :: !Int
  , wPsi    :: !(U.Vector Double)      -- 格子の上の Ψ
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "54-12 Radio waves from an antenna" world0)
  { skBack  = rgb255 14 18 26
  , skStart = \env -> do
      printSelfTest selfTest
      pure (computePsi env world0 { wTest = selfTest })
  , skStep  = \env dt w -> pure (step env dt w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 220 0 False True 6 0 0 U.empty []

-- アンテナの位置
center :: Env -> (Double, Double)
center env = (realToFrac (envW env) * 0.38, realToFrac (envH env) * 0.55)

step :: Env -> Float -> World -> World
step env dt w = computePsi env w1
  where
    w1 | wPaused w = w
       | otherwise = w { wPhase = wPhase w + (2 * pi * cpx / wLam w) * realToFrac (min dt 0.05) }

-- 格子の上で Ψ を計算する
computePsi :: Env -> World -> World
computePsi env w = w { wGx = gx, wGy = gy, wPsi = U.generate (gx * gy) f }
  where
    gx = ceiling (envW env * 0.76 / fromIntegral cell) + 1
    gy = ceiling ((envH env - 120) / fromIntegral cell) + 1
    (cx, cy) = center env
    k = (2 * pi) / wLam w
    f q = let (j, i) = q `divMod` gx
              x = fromIntegral (i * cell) - cx
              z = cy - fromIntegral (120 + j * cell)
              r = hyp x z
              th = atan2 (abs x) z
          in streamPsi (max (k * r) 0.25) th (wPhase w)

-- ---- 描く
draw :: Env -> World -> Picture
draw env w = view env $
  [ at (fromIntegral (-cell) / 2 + fromIntegral (gx * cell) / 2, 120 - fromIntegral cell / 2 + fromIntegral (gy * cell) / 2)
       (Scale 1 (-1) (pixelImage gx gy (fromIntegral cell) px))
  , Color (rgba255 240 245 255 210) (Pictures (contours psi gx gy levels)) ]
  -- 近い場と遠い場の境目 r = λ/2π
  ++ [ Color (rgba255 255 230 120 120) (dashed 5 6 [ (fcx + rr * cos a, fcy + rr * sin a) | t <- [0 .. 90 :: Int], let a = fromIntegral t / 90 * 2 * pi ])
  -- アンテナ（電荷 ± が入れかわる）
     , Color (gray 0.78) (thickLine 4 (fcx, fcy - 18) (fcx, fcy + 18))
     , Color (if q > 0 then red' else blue') (circleD fcx (fcy - 20) (6 + 10 * f2 (abs q)))
     , Color (if q > 0 then blue' else red') (circleD fcx (fcy + 20) (6 + 10 * f2 (abs q)))
     , drawPattern (envW env * 0.86) (envH env * 0.42) (min 130 (envW env * 0.1))
     , say ("Left/Right wavelength  Space pause (" ++ (if wPaused w then "paused" else "running") ++ ")  C color  Up/Down number of lines") 20 34 17 (gray 0.92)
     , say ("Wavelength = " ++ showJ (wLam w) ++ " px (if 1 px = 1 cm: " ++ fmt 2 (wLam w * pxM) ++ " m -> f = c/lambda = " ++ sci 3 (fReal / 1e6) ++ " MHz)") 20 60 15 (rgb255 255 230 120) ]
  ++ [ say s 20 (82 + 17 * fromIntegral i) 14 (gray 0.67) | (i, s) <- zip [0 :: Int ..] (wordWrap (envW env - 40) 14 note) ]
  ++ [ Color (rgba255 0 0 0 160) (rect 85 (envH env - 19) 150 26)
     , drawSelfTest env (wTest w) ]
  where
    gx = wGx w
    gy = wGy w
    psi = wPsi w
    (cx, cy) = center env
    fcx = f2 cx
    fcy = f2 cy
    rr = f2 (wLam w / (2 * pi))
    q = cos (wPhase w)
    red' = rgb255 240 90 70
    blue' = rgb255 70 150 250
    fReal = 3e8 / (wLam w * pxM)
    n = wNLev w
    levels = [ (fromIntegral (sg * l) / fromIntegral (n + 1)) * 1.4 | l <- [1 .. n], sg <- [1, -1] ]
    -- 色：|Ψ|
    px i j = let v = U.unsafeIndex psi (j * gx + i)
                 e = if wColor w then clampv 0 1.2 (abs v) / 1.2 else 0
             in (b8 (14 + (if v > 0 then 120 * e else 30 * e)), b8 (18 + 50 * e), b8 (30 + (if v < 0 then 150 * e else 30 * e)))
    note = "White lines = electric field lines (vertical cross-section, the antenna stands upright in the middle). Inside the dashed circle r = lambda/2pi is the "
           ++ "'near field' (dipole shape), outside is the 'far field' (radio waves). On screen the speed of light = " ++ showJ cpx ++ " px/s"

-- 等高線（電気力線）を折れ線で。格子の点 (i, j) は画面の (i·CELL, 120 + j·CELL)
contours :: U.Vector Double -> Int -> Int -> [Double] -> [Picture]
contours psi gx gy levels = [ Line [ (sx x, sy y) | (x, y) <- path ] | path <- marchingSquares psi gx gy levels ]
  where
    sx x = f2 (x * fromIntegral cell)
    sy y = 120 + f2 (y * fromIntegral cell)

-- 右：放射の強さの向き（遠くの E を式で計算して |E|² の時間平均）と sin²θ
drawPattern :: Float -> Float -> Float -> Picture
drawPattern x y r = Pictures
  [ Color (rgb255 70 80 100) (at (x, y) (Circle r))
  , Color (rgb255 70 80 100) (Line [(x, y - r - 10), (x, y + r + 10)])
  , Color (rgb255 255 200 90) (polyline 3 pts)
  , Color (rgb255 120 230 255) (dashed 4 4 pts2)
  , say "Radiation strength by direction" (x - r) (y - r - 30) 14 (gray 0.86)
  , say "orange: |E|^2 computed at kr = 200" (x - r) (y + r + 30) 14 (rgb255 255 200 90)
  , say "dashed: sin^2 theta" (x - r) (y + r + 50) 14 (rgb255 120 230 255)
  , say "(nothing along the antenna)" (x - r) (y + r + 68) 14 (rgb255 120 230 255) ]
  where
    angs = [ fromIntegral a * pi / 180 | a <- [0, 3 .. 360 :: Int] ]
    pts = [ (x + rr * f2 (sin th), y - rr * f2 (cos th)) | th <- angs, let rr = r * f2 (farIntensity (jsRem (abs th) (2 * pi))) ]
    pts2 = [ (x + rr * f2 (sin th), y - rr * f2 (cos th)) | th <- angs, let rr = r * f2 (sin th ** 2) ]

-- ---- キー
event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wLam = min 400 (wLam w + 20) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wLam = max 60 (wLam w - 20) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wNLev = min 12 (wNLev w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wNLev = max 2 (wNLev w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> w { wColor = not (wColor w) }
    | c `elem` "rR" -> w { wLam = 220, wNLev = 6, wPaused = False }
  _ -> w

-- ---- 自己テスト（p5.js 版と同じ 6 項目）
selfTest :: [(String, Bool)]
selfTest =
  [ ("the formula's E follows the contours of Psi (field lines): E.grad Psi ~ 0", worst < 1e-5)
  , ("far-away radiation strength ~ sin^2 theta (within 1%)", all (\t -> nearRel (farIntensity t) (sin t ** 2) 0.01) [0.3, 0.7, 1.2])
  , ("total power in all directions ~ integral of sin^2 theta dOmega = 8pi/3 (Larmor formula shape, within 1%)", nearRel pTot ((8 * pi) / 3) 0.01)
  , ("very close (kr << 1) it is an ordinary dipole: E_r/E_theta = 2 cot theta", nearRel (er0 / et0) (2 / tan 0.6) 1e-3)
  , ("far loops travel at the speed of light (d(kr) = d(omega t), within 1%)", nearRel (root 0.5 - root 0) 0.5 0.01)
  , ("c = f lambda: lambda = 2 m -> f = 150 MHz", near (3e8 / 2 / 1e6) 150 1e-9) ]
  where
    worst = foldl' max 0
      [ abs (er * dr + et * dth) / (hyp er et * hyp dr dth + 1e-300)
      | n <- [0 .. 39 :: Int]
      , let kr = 0.3 + fromIntegral n * 0.37
            th = 0.2 + jsRem (fromIntegral n * 0.61) 2.7
            wt = fromIntegral n * 0.9
            (er, et) = fieldE kr th wt
            h = 1e-6
            dr = (streamPsi (kr + h) th wt - streamPsi (kr - h) th wt) / (2 * h)
            dth = (streamPsi kr (th + h) wt - streamPsi kr (th - h) wt) / (2 * h) / kr ]
    nP = 2000 :: Int
    pTot = foldl' (\s i -> let t = (pi * (fromIntegral i + 0.5)) / fromIntegral nP
                               (_, _, c2, c3) = cplxE 500 t
                           in s + (c2 * c2 + c3 * c3) * 500 * 500 * 2 * pi * sin t * (pi / fromIntegral nP)) 0 [0 .. nP - 1]
    (er0, et0) = fieldE 0.01 0.6 0
    root wt = go (0 :: Int) 40 (40 + pi)
      where go i lo hi
              | i >= 60 = lo
              | otherwise = let m = (lo + hi) / 2
                            in if signum (streamPsi m (pi / 2) wt) == signum (streamPsi lo (pi / 2) wt) then go (i + 1) m hi else go (i + 1) lo m

-- ---------- やってみよう ----------
-- 1. 止めて（スペース）、電気力線の輪がアンテナから切りはなされる瞬間を探そう
-- 2. ←→ で波長を変えると、点線の円（λ/2π）と輪の間隔はどう変わる？
-- 3. アンテナの真上（たての向き）には電波が出ない。右の図で確かめよう。テレビのアンテナの向きとの関係は？
-- 4. FM ラジオ（80 MHz）の波長は何 m？ そのときの半波長アンテナの長さは？
-- 5. cpx を変えて、画面の上での光の速さを変えてみよう


-- ==================================================
-- ここから下は「電磁気の道具」（中身は読まなくても OK。消さないでね）
-- 計算は Double（倍精度）、描くときだけ Float にする
-- ==================================================
-- ---- 電気力線の関数 Ψ（kr、θ = アンテナの向きからの角度、wt = ωt）
streamPsi :: Double -> Double -> Double -> Double
streamPsi kr th wt = let a = kr - wt in sin th ** 2 * (cos a / kr + sin a)

-- ---- ヘルツの双極子の電場（係数 k³p/4πε0 を 1 にした値）：複素数で (Re E_r, Im E_r, Re E_θ, Im E_θ)（時刻 0）
-- E_r = 2cosθ (1/(kr)³ − i/(kr)²) e^{ikr}、E_θ = sinθ (1/(kr)³ − i/(kr)² − 1/(kr)) e^{ikr}
cplxE :: Double -> Double -> (Double, Double, Double, Double)
cplxE kr th = (2 * cos th * er0, 2 * cos th * er1, sin th * et0, sin th * et1)
  where
    c = cos kr
    s = sin kr
    a = 1 / kr ** 3
    b = 1 / kr ** 2
    d = 1 / kr
    mul re im = (re * c - im * s, re * s + im * c)
    (er0, er1) = mul a (-b)
    (et0, et1) = mul (a - d) (-b)

-- ---- 時刻 t の本物の電場 Re(E e^{−iωt}) = (E_r, E_θ)
fieldE :: Double -> Double -> Double -> (Double, Double)
fieldE kr th wt = (c0 * cw + c1 * sw, c2 * cw + c3 * sw)
  where
    (c0, c1, c2, c3) = cplxE kr th
    cw = cos wt
    sw = sin wt

-- ---- 遠く（kr = 200）での |E|² の時間平均を、θ = 90° を 1 として
farIntensity :: Double -> Double
farIntensity th = (c0 ** 2 + c1 ** 2 + c2 ** 2 + c3 ** 2) / (m2 ** 2 + m3 ** 2)
  where
    (c0, c1, c2, c3) = cplxE 200 th
    (_, _, m2, m3) = cplxE 200 (pi / 2)

-- ---- マーチング・スクエア：値 = level の線（格子の単位）を、つながった折れ線のリストで
--      ます目の 4 つの辺（上・右・下・左）のうち、両はしの値が level をまたぐ辺に線が通る（p5.js 版と同じ規則）。
--      p5.js 版はます目ごとの短い線分を描くが、ここでは辺を伝って線をたどり、長い折れ線にする（描く数を減らすため）
--      辺の番号：横の辺 (i, j)–(i+1, j) は 2(j·nx + i)、たての辺 (i, j)–(i, j+1) は 2(j·nx + i) + 1
marchingSquares :: U.Vector Double -> Int -> Int -> [Double] -> [[(Double, Double)]]
marchingSquares g gx gy levels0 = runST $ do
  seen <- MU.replicate (2 * gx * gy) False
  fmap concat $ forM [0 .. nl - 1] $ \li -> do
    let lev = U.unsafeIndex levs li
        val i j = U.unsafeIndex g (j * gx + i)
        -- 辺 e を level の線が通るか（両はしの「何本の高さより上か」で調べる）
        crosses e = U.unsafeIndex eLo e <= li && li < U.unsafeIndex eHi e
        -- 辺の上の点（直線で内そう）
        point e = let (i, j, t) = edgeIJ e
                      (i2, j2) = if t == 0 then (i + 1, j) else (i, j + 1)
                      a = val i j
                      b = val i2 j2
                      !u = (lev - a) / (b - a)
                      !px = fromIntegral i + u * fromIntegral (i2 - i)
                      !py = fromIntegral j + u * fromIntegral (j2 - j)
                  in (px, py)
        -- ます目 (i, j) に辺 e から入ったときの出口の辺（なければ -1）。4 本なら（上・右）（下・左）の組
        exitOf i j e =
          let eT = eH i j
              eR = eV (i + 1) j
              eB = eH i (j + 1)
              eL = eV i j
              cT = crosses eT
              cR = crosses eR
              cB = crosses eB
              cL = crosses eL
              n = fromEnum cT + fromEnum cR + fromEnum cB + fromEnum cL
          in if n == 4 then (if e == eT then eR else if e == eR then eT else if e == eB then eL else eB)
             else if n == 2 then (case [ x | (x, c) <- [(eT, cT), (eR, cR), (eB, cB), (eL, cL)], c, x /= e ] of
                                    (x : _) -> x
                                    [] -> -1)
             else -1
        -- 辺 e の向こう側のます目（ます目 (i, j) から来たとき）。なければ i = -1
        across e i j = let (ei, ej, t) = edgeIJ e
                       in if t == 0 then (if ej == j then (if ej > 0 then (ei, ej - 1) else (-1, 0)) else (if ej < gy - 1 then (ei, ej) else (-1, 0)))
                                    else (if ei == i then (if ei > 0 then (ei - 1, ej) else (-1, 0)) else (if ei < gx - 1 then (ei, ej) else (-1, 0)))
        -- ます目 (i, j) に辺 e から入って、線をたどる。たどった点を逆順に acc へ。(点, 出発点にもどったか)
        trace !e0 !i !j !e acc = do
          let e' = exitOf i j e
          if e' < 0 then pure (acc, False) else do
            v <- MU.unsafeRead seen e'
            if v then pure (if e' == e0 then (point e' : acc, True) else (acc, False)) else do
              MU.unsafeWrite seen e' True
              let !p = point e'
                  (i2, j2) = across e' i j
              if i2 < 0 then pure (p : acc, False) else trace e0 i2 j2 e' (p : acc)
        go !c acc
          | c >= nC = pure acc
          | not (crosses e) = go (c + 1) acc
          | otherwise = do
              v <- MU.unsafeRead seen e
              if v then go (c + 1) acc else do
                MU.unsafeWrite seen e True
                let (ei, ej, t) = edgeIJ e
                    -- 辺のとなりのます目：「先」と「手前」
                    (fi, fj, fok) = if t == 0 then (ei, ej, ej < gy - 1) else (ei, ej, ei < gx - 1)
                    (bi, bj, bok) = if t == 0 then (ei, ej - 1, ej > 0) else (ei - 1, ej, ei > 0)
                (fwdR, closed) <- if fok then trace e fi fj e [] else pure ([], False)
                (bwdR, _) <- if bok && not closed then trace e bi bj e [] else pure ([], False)
                go (c + 1) ((bwdR ++ [point e] ++ reverse fwdR) : acc)
          where e = U.unsafeIndex cand c
    MU.set seen False
    go 0 []
  where
    levs = sortedVec levels0
    nl = U.length levs
    nE = 2 * gx * gy
    nP = gx * gy
    nC = U.length cand
    eH i j = 2 * (j * gx + i)
    eV i j = 2 * (j * gx + i) + 1
    edgeIJ e = let (q, t) = e `quotRem` 2
                   (j, i) = q `quotRem` gx
               in (i, j, t)
    -- 格子の点ごとに「いくつの高さより上か」（band）。辺ごとに、両はしの band の小さいほう（eLo）・大きいほう（eHi）
    -- lo ≤ 高さの番号 < hi なら、その高さの線が辺を通る
    (eLo, eHi, cand) = runST $ do
      band <- MU.replicate nP (0 :: Int)
      loop 0 nP $ \k -> do
        let v = U.unsafeIndex g k
            cnt !l !c | l >= nl = c
                      | otherwise = cnt (l + 1) (if U.unsafeIndex levs l < v then c + 1 else c)
        MU.unsafeWrite band k (cnt 0 0)
      lo <- MU.replicate nE (0 :: Int)
      hi <- MU.replicate nE (0 :: Int)
      loop 0 gy $ \j -> loop 0 gx $ \i -> do
        ba <- MU.unsafeRead band (j * gx + i)
        when (i < gx - 1) $ do
          bb <- MU.unsafeRead band (j * gx + i + 1)
          MU.unsafeWrite lo (eH i j) (min ba bb)
          MU.unsafeWrite hi (eH i j) (max ba bb)
        when (j < gy - 1) $ do
          bb <- MU.unsafeRead band ((j + 1) * gx + i)
          MU.unsafeWrite lo (eV i j) (min ba bb)
          MU.unsafeWrite hi (eV i j) (max ba bb)
      a <- U.unsafeFreeze lo
      b <- U.unsafeFreeze hi
      -- どれかの線が通る辺だけの番号の一覧
      let cs = U.filter (\e -> U.unsafeIndex a e < U.unsafeIndex b e) (U.enumFromN 0 nE)
      pure (a, b, cs)

-- a 以上 b 未満の整数でくり返す
loop :: Int -> Int -> (Int -> ST s ()) -> ST s ()
loop a b f = go a
  where go !k | k >= b = pure ()
              | otherwise = f k >> go (k + 1)
{-# INLINE loop #-}

-- 小さい順に並べた配列（NOINLINE：配列として 1 回だけ作り、毎回リストからたどり直さないようにする）
sortedVec :: [Double] -> U.Vector Double
sortedVec xs = U.fromList (sort xs)
{-# NOINLINE sortedVec #-}

-- ---- 数の道具
hyp :: Double -> Double -> Double
hyp a b = sqrt (a * a + b * b)

clampv :: Double -> Double -> Double -> Double
clampv a b v = if v < a then a else if v > b then b else v

-- JavaScript の % （割られる数と同じ符号の余り）
jsRem :: Double -> Double -> Double
jsRem x y = x - y * fromIntegral (truncate (x / y) :: Integer)

b8 :: Double -> Word8
b8 v = fromIntegral (max 0 (min 255 (round v :: Int)))

-- NaN・Infinity なら d にもどす
fin :: Double -> Double -> Double
fin d v = if isNaN v || isInfinite v then d else v

f2 :: Double -> Float
f2 = realToFrac . fin 0

-- 小数点以下 n 桁で書く（toFixed）
fmt :: Int -> Double -> String
fmt n v = showFFloat (Just n) (fin 0 v) ""

-- 大きい数・小さい数を読みやすく：0.000032 → "3.20e-5"（有効数字 sig 桁）
sci :: Int -> Double -> String
sci sig v
  | isNaN v || isInfinite v = "-"
  | v == 0 = "0"
  | e >= -2 && e <= 3 = fmt (max 0 (sig - 1 - e)) v
  | otherwise = fmt (sig - 1) (v / 10 ^^ e) ++ "e" ++ show e
  where
    e0 = floor (logBase 10 (abs v)) :: Int
    e | 10 ^^ (e0 + 1) <= abs v = e0 + 1
      | 10 ^^ e0 > abs v = e0 - 1
      | otherwise = e0

-- JavaScript の数の文字列（整数なら小数点なし）
showJ :: Double -> String
showJ v | isNaN v || isInfinite v = "0"
        | v == fromIntegral (round v :: Integer) && abs v < 1e15 = show (round v :: Integer)
        | otherwise = showFFloat Nothing v ""

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 幅 maxW に収まるように単語の切れ目で折り返す
wordWrap :: Float -> Float -> String -> [String]
wordWrap maxW size s = reverse (go (words s) "" [])
  where
    go [] cur acc = if null cur then acc else cur : acc
    go (x : xs) cur acc
      | null cur = go xs x acc
      | fromIntegral (length cur + 1 + length x) * size * 0.62 <= maxW = go xs (cur ++ " " ++ x) acc
      | otherwise = go xs x (cur : acc)

-- 点が 2 つ以上あるときだけ折れ線を描く（太さ w）
polyline :: Float -> [Point] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> if w <= 1 then Line pts else thickPath w pts
  _ -> Blank

-- 点線（線 on、すきま off ピクセル）
dashed :: Float -> Float -> [Point] -> Picture
dashed on off ps = Pictures (go True on ps)
  where
    go _ _ [] = []
    go _ _ [_] = []
    go dr left (a@(x1, y1) : rest@(b@(x2, y2) : _))
      | l < 1e-6 = go dr left rest
      | l <= left = [ Line [a, b] | dr ] ++ go dr (left - l) rest
      | otherwise = let m = (x1 + (x2 - x1) * left / l, y1 + (y2 - y1) * left / l)
                    in [ Line [a, m] | dr ] ++ go (not dr) (if dr then off else on) (m : rest)
      where l = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

-- 円（直径 d、p5.js の circle と同じ）
circleD :: Float -> Float -> Float -> Picture
circleD x y d = at (x, y) (disc (d / 2))

-- ---- 自己テストの道具
near :: Double -> Double -> Double -> Bool
near a b tol = not (isNaN a || isInfinite a) && abs (a - b) <= tol

-- 相対誤差 rel 以内
nearRel :: Double -> Double -> Double -> Bool
nearRel a b rel = near a b (abs b * rel)

-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if not (null rs) && all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 画面の左下に小さく
drawSelfTest :: Env -> [(String, Bool)] -> Picture
drawSelfTest env rs = say (selfTestLine rs) 20 (envH env - 12) 14 (if not (null rs) && all snd rs then rgb255 120 220 160 else rgb255 255 100 90)
