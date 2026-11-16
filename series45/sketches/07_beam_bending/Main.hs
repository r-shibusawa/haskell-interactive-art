-- ============================================
-- 45-07 はりの曲げ
-- 荷重 → せん断力 V → 曲げモーメント M → たわみ y。順に積分していく（オイラー・ベルヌーイのはり理論：EI y'' = M）
-- 断面二次モーメント I = b h³ / 12。高さ h を 2 倍にすると、たわみは 1/8 になる
-- 操作：矢印をドラッグ = 荷重の位置と大きさ / はりをクリック = 荷重を足す / T = 単純支持 ⇄ 片持ち
--       ←→ = 幅 b / ↑↓ = 高さ h / E = 材料 / C = 荷重を 1 つに
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector.Unboxed as U
import Numeric (showFFloat)

beamLen :: Float
beamLen = 2.0                  -- ★ はりの長さ（m）

matList :: [(String, Float)]
matList = [("steel", 206e9), ("aluminium", 70e9), ("wood (cedar)", 7e9)]   -- ★ 材料とヤング率（Pa）

nb :: Int
nb = 400                       -- はりを分ける数

-- 荷重：位置 a（m）、大きさ p（N、下向きが正）
data Load = Load { lA :: !Float, lP :: !Float }

data World = World
  { wB, wH     :: !Float       -- ★ 断面の幅 b と高さ h（mm）
  , wSupport   :: !Int         -- 0 単純支持 / 1 片持ち
  , wMat       :: !Int
  , wLoads     :: ![Load]
  , wGrab      :: !Int
  }

main :: IO ()
main = runSketch (sketch "45-07 Beam bending" (World 30 50 0 0 [Load 1.2 1000] (-1)))
  { skBack  = hsb 212 45 13
  , skDraw  = draw
  , skEvent = event
  }

-- はりの絵の左端・幅・高さ位置
bx, by :: Float
bx = 100
by = 190

bw :: Env -> Float
bw env = envW env - 560

matOf :: World -> (String, Float)
matOf w = case drop (wMat w) matList of
  (m : _) -> m
  [] -> ("steel", 206e9)

data Sol = Sol { sV, sM, sY :: !(U.Vector Float), sRA, sRB, sMA, sEI :: !Float }

-- ★ 数値積分で V, M, たわみ角, たわみ を求める
solveBeam :: World -> Sol
solveBeam w = Sol v m y ra rb ma ei
  where
    dx = beamLen / fromIntegral nb
    ei = snd (matOf w) * ((wB w / 1000) * (wH w / 1000) ^ (3 :: Int)) / 12
    sumP = sum (map lP (wLoads w))
    sumPa = sum [lP q * lA q | q <- wLoads w]
    (ra, rb, ma) | wSupport w == 0 = let r2 = sumPa / beamLen in (sumP - r2, r2, 0)   -- 単純支持：モーメントのつり合い
                 | otherwise = (sumP, 0, negate sumPa)                               -- 片持ち：壁が力とモーメントを受ける
    shearAt x = ra - sum [lP q | q <- wLoads w, lA q < x]
    v = U.generate (nb + 1) (\i -> shearAt (fromIntegral i * dx + 1e-9))
    m = U.scanl (+) ma (U.generate nb (\i -> shearAt ((fromIntegral (i + 1) - 0.5) * dx) * dx))       -- M = ∫V dx
    slope = U.scanl (+) 0 (U.generate nb (\i -> (m U.! i + m U.! (i + 1)) / 2 / ei * dx))            -- θ = ∫M/EI dx
    y0 = U.scanl (+) 0 (U.generate nb (\i -> (slope U.! i + slope U.! (i + 1)) / 2 * dx))             -- y = ∫θ dx
    yEnd = U.last y0
    y | wSupport w == 0 = U.imap (\i yi -> yi - yEnd * fromIntegral i / fromIntegral nb) y0          -- 両端で y = 0 になるように
      | otherwise = y0

draw :: Env -> World -> Picture
draw env w = view env $
  -- ---- はりと支点
  supports
  ++ [ seg 1 (hsbA 0 0 50 50) (xOf 0, by) (xOf beamLen, by)          -- 変形前
     , thickPath' (clampF 4 22 (wH w / 5)) (hsb (if wMat w == 2 then 30 else 200) 50 95)
         [ (xOf (fromIntegral i * beamLen / fromIntegral nb), by - sY sol U.! i * pxPerM) | i <- [0, 4 .. nb] ] ]
  ++ concatMap loadArrow (wLoads w)
  -- 反力
  ++ [ txt (xOf 0 - 40) (by + 62) 14 (hsb 140 60 100) ("R = " ++ fmt 0 (sRA sol) ++ " N") ]
  ++ (if wSupport w == 0 then [txt (xOf beamLen - 40) (by + 62) 14 (hsb 140 60 100) ("R = " ++ fmt 0 (sRB sol) ++ " N")]
                         else [txt (xOf 0 - 40) (by + 80) 14 (hsb 140 60 100) ("M = " ++ fmt 0 (sMA sol) ++ " N.m")])
  -- ---- せん断力図（SFD）と曲げモーメント図（BMD）
  ++ concat [ diagram r | r <- [0, 1] ]
  -- ---- 数値
  ++ [ txt 20 40 18 (hsb 0 0 100) ("Drag arrow dot: load   Click beam: add load   T: " ++ (if wSupport w == 0 then "simply supported" else "cantilever")
                                    ++ "   L/R: b   Up/Down: h   E: " ++ fst (matOf w) ++ "   C: one load")
     , txt 20 68 15 (hsb 0 0 60) ("Load -> V -> M -> slope -> deflection: integrate 4 times. Deflection is magnified (real max "
                                  ++ fmt 2 (yMax * 1000) ++ " mm)")
     , Color (hsbA 0 0 100 8) (rect (ix - 16 + 205) (iy0 - 30 + 200) 410 400)
     , txt ix iy0 16 (hsb 0 0 100) "EI y'' = M(x)     dM/dx = V"
     , txt ix (iy0 + 28) 16 (hsb 0 0 100) "Second moment of area I = b h^3 / 12"
     , txt ix (iy0 + 56) 16 (hsb 35 90 100) ("b = " ++ fmt 0 (wB w) ++ " mm  h = " ++ fmt 0 (wH w) ++ " mm  I = " ++ fmt 1 (iSec / 1e4) ++ " cm^4")
     , txt ix (iy0 + 90) 16 (hsb 190 70 100) ("max deflection = " ++ fmt 2 (yMax * 1000) ++ " mm (x = " ++ fmt 2 (fromIntegral iy * beamLen / fromIntegral nb) ++ " m)")
     , txt ix (iy0 + 116) 16 (hsb 320 60 100) ("max bending stress M(h/2)/I = " ++ fmt 1 sigMax ++ " MPa") ]
  ++ check
  -- 断面の絵
  ++ [ Color (hsb (if wMat w == 2 then 30 else 200) 45 80) (rect cxs cys (wB w * cs) (wH w * cs))
     , Color (hsbA 0 0 100 70) (rectLine cxs cys (wB w * cs) (wH w * cs))
     , seg 1 (hsbA 0 0 100 50) (cxs - 80, cys) (cxs + 80, cys)          -- 中立軸
     , txt (cxs + 86) (cys + 4) 14 (hsb 0 0 60) "neutral axis", txt ix (cys + 4) 14 (hsb 0 0 60) "section" ]
  where
    sol = solveBeam w
    xOf x = bx + x / beamLen * bw env
    vMax = max 1 (U.maximum (U.map abs (sV sol)))
    mMax = max 1 (U.maximum (U.map abs (sM sol)))
    absY = U.map abs (sY sol)
    iy = U.maxIndex absY
    yMax = max 1e-9 (absY U.! iy)
    pxPerM = min 8000 (60 / yMax)          -- たわみの拡大（最大でも 60 px）
    yAtLoad a = sY sol U.! max 0 (min nb (round (a / beamLen * fromIntegral nb)))
    supports
      | wSupport w == 0 =
          [ Color (hsb 0 0 40) (Polygon [(xOf 0, by + 8), (xOf 0 - 14, by + 32), (xOf 0 + 14, by + 32)])
          , Color (hsb 0 0 40) (Polygon [(xOf beamLen, by + 8), (xOf beamLen - 14, by + 32), (xOf beamLen + 14, by + 32)])
          , dot (hsb 0 0 40) (xOf beamLen - 8) (by + 37) 9, dot (hsb 0 0 40) (xOf beamLen + 8) (by + 37) 9 ]
      | otherwise =
          Color (hsb 0 0 40) (rect (xOf 0 - 13) by 26 120)
          : [ seg 1 (hsb 0 0 60) (xOf 0 - 26, by + k + 12) (xOf 0, by + k) | k <- [-60, -48 .. 59] ]
    -- 荷重の矢印
    loadArrow q =
      let x = xOf (lA q)
          yb = by - yAtLoad (lA q) * pxPerM
          lenA = lP q / 12
          sg = if lP q >= 0 then 1 else -1
          tip = yb - 12 * sg
          tail' = tip - lenA - 10 * sg
      in [ seg 4 (hsb 0 80 100) (x, tail') (x, tip)
         , seg 4 (hsb 0 80 100) (x, tip) (x - 8, tip - 12 * sg), seg 4 (hsb 0 80 100) (x, tip) (x + 8, tip - 12 * sg)
         , dot (hsb 0 80 100) x tail' 14
         , txt (x + 12) (tail' + 4) 14 (hsb 0 0 100) (fmt 0 (lP q) ++ " N") ]
    diagram :: Int -> [Picture]
    diagram r =
      let y0 = by + 190 + fromIntegral r * 150
          amp = 52
          (name, arr, peak, hue, unit) = if r == 0 then ("Shear force V", sV sol, vMax, 190, " N") else ("Bending moment M", sM sol, mMax, 320, " N.m")
          sgn = if r == 1 then -1 else 1                 -- M は下向きを正（引張側）に描く
          pts = [ (xOf (fromIntegral i * beamLen / fromIntegral nb), y0 - arr U.! i / peak * amp * sgn) | i <- [0, 2 .. nb] ]
          outline = [(xOf 0, y0)] ++ pts ++ [(xOf beamLen, y0)]
      in [ Color (hsbA hue 60 90 30) (Pictures (zipWith (band y0) pts (drop 1 pts)))
         , Color (hsb hue 60 100) (Pictures (zipWith (thickLine 2) outline (drop 1 outline ++ take 1 outline)))
         , seg 1 (hsb 0 0 50) (xOf 0, y0) (xOf beamLen, y0)
         , txt (xOf 0) (y0 - amp - 12) 14 (hsb hue 40 100) (name ++ "   max " ++ fmt 0 peak ++ unit) ]
    iSec = wB w * wH w ^ (3 :: Int) / 12                  -- mm⁴
    sigMax = mMax * 1000 * (wH w / 2) / iSec               -- 曲げ応力 σ = M (h/2) / I（MPa）
    ix = envW env - 420
    iy0 = 130
    -- 公式との答え合わせ（荷重点のたわみ）
    check = case wLoads w of
      [q] ->
        let a = lA q
            b = beamLen - a
            formula = if wSupport w == 0 then lP q * a * a * b * b / (3 * sEI sol * beamLen) else lP q * a * a * a / (3 * sEI sol)
            numeric = negate (yAtLoad a)
        in [ txt ix (iy0 + 146) 14 (hsb 0 0 70) ("formula (at load): " ++ (if wSupport w == 0 then "P a^2 b^2 / (3 E I L)" else "P a^3 / (3 E I)")
                                                  ++ " = " ++ fmt 3 (formula * 1000) ++ " mm")
           , txt ix (iy0 + 166) 14 (hsb 0 0 70) ("numerical: " ++ fmt 3 (numeric * 1000) ++ " mm") ]
      _ -> []
    cs = 1.6
    cxs = ix + 190
    cys = iy0 + 280

-- 軸 y0 と 2 点のあいだを塗る台形（軸をまたぐときは 2 つの三角形に分ける）
band :: Float -> Point -> Point -> Picture
band y0 (x1, y1) (x2, y2)
  | (y1 - y0) * (y2 - y0) < 0 =
      let t = (y0 - y1) / (y2 - y1)
          xm = x1 + (x2 - x1) * t
      in Pictures [Polygon [(x1, y0), (x1, y1), (xm, y0)], Polygon [(xm, y0), (x2, y2), (x2, y0)]]
  | otherwise = Polygon [(x1, y0), (x1, y1), (x2, y2), (x2, y0)]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> press env w
  EventKey (MouseButton LeftButton) Up _ _ -> w { wGrab = -1 }
  EventMotion _ | envDown env && wGrab w >= 0 ->
    let (mx, my) = mouseP env
        q = Load (clampF 0.02 beamLen ((mx - bx) / bw env * beamLen)) (clampF (-1500) 1500 ((by - 22 - my) * 12))
    in w { wLoads = [if k == wGrab w then q else l | (k, l) <- zip [0 ..] (wLoads w)] }
  EventKey (Char k) Down _ _
    | k `elem` "tT" -> w { wSupport = 1 - wSupport w }
    | k `elem` "eE" -> w { wMat = (wMat w + 1) `mod` length matList }
    | k `elem` "cC" -> w { wLoads = [Load (beamLen * 0.6) 1000], wGrab = -1 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wB = clampF 10 80 (wB w - 5) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wB = clampF 10 80 (wB w + 5) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wH = clampF 10 100 (wH w + 5) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wH = clampF 10 100 (wH w - 5) }
  _ -> w

press :: Env -> World -> World
press env w
  | g >= 0 = w { wGrab = g }
  | mx > bx && mx < bx + bw env && abs (my - by) < 30 && length (wLoads w) < 5 =
      w { wLoads = wLoads w ++ [Load ((mx - bx) / bw env * beamLen) 600], wGrab = length (wLoads w) }
  | otherwise = w { wGrab = -1 }
  where
    (mx, my) = mouseP env
    -- p5.js と同じく、条件に合う最後の荷重をつかむ
    g = foldl (\acc (k, q) -> if abs (mx - (bx + lA q / beamLen * bw env)) < 20 && my < by + 80 && my > 90 then k else acc)
              (-1) (zip [0 ..] (wLoads w))

-- ---------- やってみよう ----------
-- 1. h を 50 → 100 にすると、たわみは 1/8、応力は 1/4。b を 2 倍にした場合と比べよう（板は立てて使う）
-- 2. 単純支持で荷重を中央に置くと最大。公式 PL³/48EI と一致するか電卓で確かめよう
-- 3. T で片持ちに。モーメント図は壁で最大になる。飛び込み台の付け根が太い理由
-- 4. 荷重を 2 つにして、中央部の せん断力を 0（モーメント一定＝純曲げ）にしよう。4 点曲げ試験の形
-- 5. b と h を入れ替えただけの断面（30×50 と 50×30）で、たわみは何倍ちがう？

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 太い折れ線（つなぎ目に円を置いて角を埋める）
thickPath' :: Float -> Color -> [Point] -> Picture
thickPath' w c ps = Color c (Pictures (zipWith (thickLine w) ps (drop 1 ps) ++ [at p (disc (w / 2)) | p <- ps]))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
