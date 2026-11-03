-- ============================================
-- 09 ナイキスト線図と安定余裕
-- 開ループ L(jω) を ω = 0 → ∞ で複素平面に描く。曲線が −1 の点を囲まなければ閉ループは安定
--   ゲイン余裕 GM：位相が −180° のとき、あと何倍ゲインを上げると −1 に届くか（dB）
--   位相余裕 PM：ゲインが 1 のとき、あと何度遅れると −1 に届くか（度）
-- 左右キー = K / 1〜2 でプラント / 右はステップ応答
-- ============================================
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Art
import Data.List (find, foldl', mapAccumL)
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

range :: Float
range = 2.5      -- ★ 複素平面の表示の範囲

-- L(jω) の 1 点
data Pt = Pt { pW, pRe, pIm, pMag, pPh :: !Double }

data World = World
  { wPlant  :: !Int
  , wK      :: !Double         -- ★ ループゲイン
  , wDen    :: ![Double]
  , wCurve  :: ![Pt]
  , wGM, wPM, wWGM, wWPM :: !Double   -- 余裕（なければ無限大）とそのときの ω（なければ 0）
  , wStep   :: !(U.Vector Float)
  , wStable :: !Bool
  }

plantName :: Int -> String
plantName 1 = "L = K/((s+1)(s+2)(s+3))"
plantName _ = "L = K/(s(s+1)(s+2))"

makePlant :: Int -> Double -> World
makePlant p k =
  let den | p == 1 = polyMul [1, 1] (polyMul [2, 1] [3, 1])
          | otherwise = polyMul [0, 1] (polyMul [1, 1] [2, 1])
  in updateK (World p k den [] 0 0 0 0 U.empty True)

updateK :: World -> World
updateK w@World{..} =
  let num = [wK]
      -- ★ ω を対数で動かして L(jω) を集める
      ws = [ 10 ** (fromIntegral i / 600 * 5 - 2) | i <- [0 .. 600 :: Int] ]
      point prev om =
        let l@(re, im) = evalTF num wDen om
            ph0 = atan2 im re * 180 / pi
            ph = case prev of
              Just q -> wrapNear q ph0
              Nothing -> if ph0 > 0 then ph0 - 360 else ph0
        in (Just ph, Pt om re im (cabs l) ph)
      (_, curve) = mapAccumL point Nothing ws
      pairs = zip curve (drop 1 curve)
      -- 位相 −180° を横切る ω → ゲイン余裕
      gmHit = find (\(a, b) -> pPh a > -180 && pPh b <= -180) pairs
      -- ゲイン 1 を横切る ω → 位相余裕
      pmHit = find (\(a, b) -> pMag a > 1 && pMag b <= 1) pairs
      inf = 1 / 0
      (wgm, gm) = case gmHit of
        Just (_, b) -> (pW b, -20 * logBase 10 (max (pMag b) 1e-12))
        Nothing -> (0, inf)
      (wpm, pm) = case pmHit of
        Just (_, b) -> (pW b, 180 + pPh b)
        Nothing -> (0, inf)
      charPoly = polyAdd wDen (polyScale num 1)
  in w { wCurve = foldr seq curve curve, wGM = gm, wPM = pm, wWGM = wgm, wWPM = wpm
       , wStable = gm > 0 && pm > 0, wStep = stepResponse num charPoly 10 }

wrapNear :: Double -> Double -> Double
wrapNear prev p
  | p - prev > 180 = wrapNear prev (p - 360)
  | p - prev < -180 = wrapNear prev (p + 360)
  | otherwise = p

main :: IO ()
main = runSketch (sketch "20-09 Nyquist Plot" (makePlant 1 10))
  { skBack  = hsb 230 30 10
  , skDraw  = drawAll
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

drawAll :: Env -> World -> Picture
drawAll env World{..} = screen env $ Pictures $
  [ box px py pw pH (hsb 0 0 14)
  , seg 1 (hsb 0 0 30) (nx 0, py) (nx 0, py + pH)
  , seg 1 (hsb 0 0 30) (px, ny 0) (px + pw, ny 0)
  , Color (hsb 0 0 40) (at (nx 0, ny 0) (Circle (nx 1 - nx 0)))   -- 単位円
    -- 曲線（ω > 0 を実線、ω < 0 を鏡写しの薄い線）
  , Pictures (map (uncurry (seg 1.5 (hsba 200 70 100 0.35))) (clipped (-1)))
  , Pictures (map (uncurry (seg 2.5 (hsb 200 70 100))) (clipped 1))
    -- 向きの印（ω が増える向きの途中）
  , case drop 240 wCurve of
      (m : _) | (a, b) <- screenPt 1 m, inPanel (a, b) == (a, b) -> dot a b 8 (hsb 200 70 100)
      _ -> Blank
    -- −1 の点
  , seg 3 red80 (nx (-1) - 8, ny 0 - 8) (nx (-1) + 8, ny 0 + 8)
  , seg 3 red80 (nx (-1) - 8, ny 0 + 8) (nx (-1) + 8, ny 0 - 8)
  , txt (nx (-1) - 10) (ny 0 + 24) 14 red80 "-1"
    -- 余裕の印
  , if wWGM > 0 then gmMark else Blank
  , if wWPM > 0 then pmMark else Blank
  , txt (px + 8) (py + 20) 14 (hsb 0 0 70) "Nyquist L(jw): solid w > 0, faint w < 0"
  , txt (px + 8) (py + 38) 14 (hsb 0 0 70) "Gray circle: |L| = 1"
    -- ステップ応答
  , drawStep gx gy gw gh wStep wStable (\p -> "Closed-loop step response (0-10 s), +/-" ++ p)
  , txt 20 40 18 white ("K = " ++ nf 2 (f wK) ++ " (L/R)   " ++ plantName wPlant ++ " (1-2)   " ++ (if wStable then "Stable" else "UNSTABLE!"))
  , txt 20 68 15 c60 ("GM = " ++ (if isInfinite wGM then "inf" else nf 1 (f wGM) ++ " dB (w = " ++ nf 2 (f wWGM) ++ ")")
        ++ "   PM = " ++ (if isInfinite wPM then "inf" else nf 1 (f wPM) ++ " deg (w = " ++ nf 2 (f wWPM) ++ ")")
        ++ "   Guide: GM > 6 dB, PM > 30-60 deg is safe")
  ]
  ++ zipWith (\i s -> txt gx (gy + gh + 30 + i * 24) 15 c60 s) [0 ..]
     [ "The smaller the margins, the more the step response"
     , "shakes. Margins are the room to survive model errors"
     , "(delay, gain error)."
     , "Encircling -1 means the closed loop is unstable when the"
     , "open loop is stable (Nyquist criterion). Same data as"
     , "the Bode plot in 07, on one picture." ]
  where
    wd = envW env
    ht = envH env
    px = wd * 0.05
    py = ht * 0.16
    pw = wd * 0.42
    pH = ht * 0.7
    nx re = remap re (-range) range px (px + pw)
    ny im = remap im (-range * (pH / pw)) (range * (pH / pw)) (py + pH) py
    f = realToFrac :: Double -> Float
    lim v = clampF (-range * 3) (range * 3) (f v)
    screenPt s c = (nx (lim (pRe c)), ny (s * lim (pIm c)))
    inPanel (a, b) = (clampF px (px + pw) a, clampF py (py + pH) b)
    -- 枠の外に出る部分は切り取って描く
    clipped s = let ps = map (screenPt s) wCurve
                in [ q | (a, b) <- zip ps (drop 1 ps), Just q <- [clipSeg (px, py, px + pw, py + pH) a b] ]
    red80 = hsb 0 80 100
    c60 = hsb 0 0 60
    gmMark = case find (\q -> pW q >= wWGM) wCurve of
      Just c ->
        let xg = clampF px (px + pw) (nx (f (pRe c)))
            yel = hsb 45 90 100
        in Pictures
             [ seg 3 yel (nx (-1), ny 0) (xg, ny 0)
             , dot xg (ny 0) 10 yel
             , txt (xg + 8) (ny 0 - 10) 14 yel "GM" ]
      Nothing -> Blank
    pmMark = case find (\q -> pW q >= wWPM) wCurve of
      Just c ->
        let (xp, yp) = inPanel (nx (f (pRe c)), ny (f (pIm c)))
            mag = hsb 300 60 100
        in Pictures
             [ seg 2 mag (nx 0, ny 0) (xp, yp)
             , dot xp yp 10 mag
             , txt (xp + 8) (yp + 18) 14 mag "PM" ]
      Nothing -> Blank
    gx = wd * 0.52
    gy = ht * 0.2
    gw = wd * 0.44
    gh = ht * 0.4

-- 線分を長方形で切り取る（リャン・バースキー法）
clipSeg :: (Float, Float, Float, Float) -> Point -> Point -> Maybe (Point, Point)
clipSeg (x0, y0, x1, y1) (ax, ay) (bx, by) =
  let dx = bx - ax
      dy = by - ay
      edges = [(-dx, ax - x0), (dx, x1 - ax), (-dy, ay - y0), (dy, y1 - ay)]
      go (t0, t1) (p, q)
        | p == 0 = if q < 0 then (1, 0) else (t0, t1)
        | p < 0 = (max t0 (q / p), t1)
        | otherwise = (t0, min t1 (q / p))
      (s, e) = foldl' go (0, 1) edges
  in if s > e then Nothing
     else Just ((ax + s * dx, ay + s * dy), (ax + e * dx, ay + e * dy))

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyRight) Down _ _ -> updateK w { wK = min (wK w * 1.15) 500 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> updateK w { wK = max (wK w / 1.15) 0.1 }
  EventKey (Char c) Down _ _ | c == '1' || c == '2' -> makePlant (fromEnum c - fromEnum '0') (wK w)
  _ -> w

-- ---------- やってみよう ----------
-- 1. K を上げて GM が 0 dB になる瞬間（K ≈ 60）、曲線が −1 を通る。ステップ応答はどうなっている？
-- 2. PM が 60° くらいになる K を探す。応答はゆれる？ 30° では？
-- 3. プラント 2（積分器）は ω → 0 で曲線が無限遠へ。K = 6 で限界になることを確かめる
-- 4. 自分で wDen に (0.1s + 1) をかける（小さな遅れを追加）。余裕はどれだけ減る？

-- ==================================================
-- ここから下は「道具」です
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を gloss の座標に直す
screen :: Env -> Picture -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- マウスの位置を p5.js の座標に直す
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字：(x, y) は文字の下端。txt = 左揃え、txtC = 中央揃え
txt, txtC :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = Translate x y (Scale 1 (-1) (label 0 0 s c str))
txtC x y s c str = txt (x - textW s str / 2) y s c str

-- 文字の幅のおよその値
textW :: Float -> String -> Float
textW s str = fromIntegral (length str) * s * 0.72

-- 左上 (x, y)、幅 w、高さ h の塗りつぶした長方形
box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2) (y + h / 2) w h)

-- 太さ wt の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg wt c a b = Color c (thickLine wt a b)

-- 太さ wt の枠
frameBox :: Float -> Color -> Float -> Float -> Float -> Float -> Picture
frameBox wt c x y w h = Color c (thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 直径 d の円
dot :: Float -> Float -> Float -> Color -> Picture
dot x y d c = Color c (at (x, y) (disc (d / 2)))

-- 小数点以下 d 桁の文字列（p5.js の nf）
nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

-- ---------- 複素数と多項式 ----------
-- 複素数は (実部, 虚部)。根の計算は桁が必要なので Double を使う
type Cx = (Double, Double)

cmul, cdiv, csub :: Cx -> Cx -> Cx
cmul (a, b) (c, d) = (a * c - b * d, a * d + b * c)
cdiv (a, b) (c, d) = let e = c * c + d * d + 1e-30 in ((a * c + b * d) / e, (b * c - a * d) / e)
csub (a, b) (c, d) = (a - c, b - d)

cabs :: Cx -> Double
cabs (a, b) = sqrt (a * a + b * b)

-- 多項式は昇べきの係数のリスト：c0 + c1 s + c2 s² …
polyMul :: [Double] -> [Double] -> [Double]
polyMul a b = foldr (\ai acc -> zipLong (map (* ai) b) (0 : acc)) [] a

zipLong :: [Double] -> [Double] -> [Double]
zipLong (x : xs) (y : ys) = (x + y) : zipLong xs ys
zipLong xs [] = xs
zipLong [] ys = ys

polyAdd :: [Double] -> [Double] -> [Double]
polyAdd = zipLong

polyScale :: [Double] -> Double -> [Double]
polyScale a k = map (* k) a

-- 複素数 s で評価（ホーナー法）
polyEvalC :: [Double] -> Cx -> Cx
polyEvalC c s = foldr (\ci acc -> let (re, im) = cmul acc s in (re + ci, im)) (0, 0) c

-- 伝達関数 G(jω) = num(jω) / den(jω)
evalTF :: [Double] -> [Double] -> Double -> Cx
evalTF num den w = cdiv (polyEvalC num (0, w)) (polyEvalC den (0, w))

-- ステップ応答：伝達関数 b(s)/a(s)（可制御正準形で積分、T 秒）
stepResponse :: [Double] -> [Double] -> Double -> U.Vector Float
stepResponse b a t =
  let n = length a - 1
      an = case reverse a of { (v : _) -> v; [] -> 1 }
      h = 0.002
      steps = round (t / h) :: Int
      every = max 1 (steps `div` 500)
      lim = max (-1e6) . min 1e6
      forceL xs = foldr seq xs xs
      loop !k zs acc
        | k >= steps = U.fromList (reverse acc)
        | otherwise =
            let top = (1 - sum (zipWith (*) a zs)) / an
                zs1 = zipWith (+) zs (map (* h) (drop 1 zs ++ [top]))
                y = sum (zipWith (*) b zs1)
                zs2 = forceL (map lim zs1)
                v = realToFrac (lim y) :: Float
            in if k `mod` every == 0 then v `seq` loop (k + 1) zs2 (v : acc)
               else loop (k + 1) zs2 acc
  in loop (0 :: Int) (replicate n 0) []

-- ステップ応答のグラフ（縦の範囲はデータに合わせて ±2〜±20）
drawStep :: Float -> Float -> Float -> Float -> U.Vector Float -> Bool -> (String -> String) -> Picture
drawStep gx gy gw gh resp stable title = Pictures
  [ box gx gy gw gh (hsb 0 0 14)
  , seg 1 (hsb 0 0 30) (gx, yOf 0) (gx + gw, yOf 0)
  , seg 1 (hsba 45 90 100 0.6) (gx, yOf 1) (gx + gw, yOf 1)
  , Color (hsb (if stable then 120 else 0) 80 100) (thickPath 2.5 pts)
  , txt (gx + 8) (gy + 20) 14 (hsb 0 0 70) (title (nf 1 peak)) ]
  where
    peak = min 20 (U.foldl' (\m v -> max m (abs v)) 2 resp)
    yOf v = remap v (-peak) peak (gy + gh) gy
    n = U.length resp
    pts = [ (remap (fromIntegral i) 0 (fromIntegral (max 1 (n - 1))) gx (gx + gw), yOf (clampF (-peak) peak v))
          | (i, v) <- zip [0 :: Int ..] (U.toList resp) ]
