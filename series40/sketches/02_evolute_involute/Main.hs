-- ============================================
-- 40-02 縮閉線（evolute）と伸開線（involute）
-- 縮閉線 = 曲線の各点の「曲率中心」（接触円の中心 p + N/κ）をつないだ曲線。法線たちの包絡線にもなる
-- 伸開線 = 曲線に巻きつけた糸をピンと張ったままほどくとき、糸の先が描く曲線 q(s) = p(s) − (s − s0) T(s)
--   有名な例：楕円の縮閉線はアステロイド、サイクロイドの伸開線はまたサイクロイド（ホイヘンスの振り子）
-- 操作：1〜5 = 曲線 / E = 縮閉線 / N = 法線の束 / I = 伸開線 / スペース = 一時停止 / ←→ = 糸をほどく速さ / R = 最初から
-- ============================================
module Main (main) where

import Art
import qualified Data.Vector as V
import Numeric (showFFloat)

speed0 :: Float
speed0 = 0.004            -- ★ 糸をほどく速さ

names :: [String]
names = ["Ellipse -> astroid", "Cycloid -> cycloid", "Circle -> involute spiral", "Parabola -> semicubical parabola", "Cardioid -> small cardioid"]

-- | 曲線の 1 点の情報
data Sample = Sample
  { sP, sT, sN :: !Point          -- 点、単位接線、左向きの単位法線
  , sKappa     :: !Float
  , sCenter    :: !(Maybe Point)  -- 曲率中心（まっすぐな所は無い）
  , sArc       :: !Float          -- ここまでの弧長
  }

data World = World
  { wCurve   :: !Int
  , wEvolute, wNormals, wInvolute, wPaused :: !Bool
  , wSpeed   :: !Float
  , wS       :: !Float
  , wInv     :: ![Point]            -- 伸開線の軌跡（新しい順）
  , wSamples :: !(V.Vector Sample)
  , wSC, wCx, wCy :: !Float         -- 画面への拡大率と中心
  }

main :: IO ()
main = runSketch (sketch "40-02 Evolute and involute" world0)
  { skBack  = hsb 260 25 10
  , skStart = \env -> pure (setCurve env 1 world0)
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 True True True False speed0 0 [] V.empty 1 0 0

-- ★ 曲線（数学の座標、y は上向き）
curveFn :: Int -> Double -> (Double, Double)
curveFn no t = case no of
  1 -> (2.2 * cos t, 1.2 * sin t)
  2 -> (t - sin t - pi, negate (1 - cos t) + 1)
  3 -> (cos t, sin t)
  4 -> (t, t * t - 1)
  _ -> let r = 1 - cos t in (r * cos t + 0.8, r * sin t)

tRange :: Int -> (Double, Double)
tRange no = if no == 4 then (-1.6, 1.6) else (0, 2 * pi)

setCurve :: Env -> Int -> World -> World
setCurve env no w =
  let (t0, t1) = tRange no
      n = 600 :: Int
      d = (t1 - t0) / fromIntegral n * 0.5
      raw i =
        let t = t0 + (t1 - t0) * fromIntegral i / fromIntegral n
            (px, py) = curveFn no t
            (ax, ay) = curveFn no (t - d)
            (bx, by) = curveFn no (t + d)
            x1 = (bx - ax) / (2 * d)
            y1 = (by - ay) / (2 * d)
            x2 = (bx - 2 * px + ax) / (d * d)
            y2 = (by - 2 * py + ay) / (d * d)
            sp0 = sqrt (x1 * x1 + y1 * y1)
            sp = if sp0 == 0 then 1e-9 else sp0
            kap = (x1 * y2 - y1 * x2) / (sp * sp * sp)
            (tx, ty) = (x1 / sp, y1 / sp)
            (nx, ny) = (negate ty, tx)                      -- 左向きの法線
        in (px, py, tx, ty, nx, ny, kap)
      f = realToFrac :: Double -> Float
      build (prev, acc) i =
        let (px, py, tx, ty, nx, ny, kap) = raw i
            arc = case prev of
              Nothing -> 0
              Just (qx, qy) -> acc + sqrt ((px - qx) * (px - qx) + (py - qy) * (py - qy))
            c = if abs kap > 1e-6 then Just (f (px + nx / kap), f (py + ny / kap)) else Nothing
        in ((Just (px, py), arc), Sample (f px, f py) (f tx, f ty) (f nx, f ny) (f kap) c (f arc))
      go _ [] = []
      go st (i : is) = let (st', s) = build st i in s : go st' is
  in w { wCurve = no
       , wSC = min (envW env) (envH env) * (if no == 2 then 0.14 else 0.16)
       , wCx = envW env * 0.5
       , wCy = envH env * 0.5 + (if no == 2 then 20 else 0)
       , wSamples = V.fromList (go (Nothing, 0) [0 .. n])
       , wS = 0, wInv = [] }

toScr :: World -> Point -> Point
toScr w (x, y) = (wCx w + x * wSC w, wCy w - y * wSC w)

curIndex :: World -> Int
curIndex w = let n = V.length (wSamples w) - 1 in max 0 (min n (floor (wS w * fromIntegral n)))

step :: Env -> World -> World
step env w0 =
  let s1 = if isDown env (SpecialKey KeyLeft) then max 0.0005 (wSpeed w0 * 0.97) else wSpeed w0
      s2 = if isDown env (SpecialKey KeyRight) then min 0.03 (s1 * 1.03) else s1
      w1 = w0 { wSpeed = s2 }
      w2 | wPaused w1 = w1
         | wS w1 + s2 > 1 = w1 { wS = 0, wInv = [] }
         | otherwise = w1 { wS = wS w1 + s2 }
  in case wSamples w2 V.!? curIndex w2 of
       Just q | wInvolute w2 && (not (wPaused w2) || null (wInv w2)) -> w2 { wInv = take 2500 (tipOf q : wInv w2) }
       _ -> w2

-- 糸の先：ほどけた糸の長さ = 弧長
tipOf :: Sample -> Point
tipOf q = let (px, py) = sP q
              (tx, ty) = sT q
              l = sArc q
          in (px - tx * l, py - ty * l)

draw :: Env -> World -> Picture
draw env w = case wSamples w V.!? curIndex w of
  Nothing -> Blank
  Just q -> Pictures (scene env w q ++ hud env w q)

scene :: Env -> World -> Sample -> [Picture]
scene env w q =
  let sc = toScr w
      samples = V.toList (wSamples w)
      pv = sc (sP q)
      -- 法線の束（包絡線として縮閉線が浮かび上がる）
      normals = [ seg2 env (hsba 30 80 100 0.18) 1 (sc (px - nx * 1.6, py - ny * 1.6)) (sc (px + nx * 1.6, py + ny * 1.6))
                | (i, s) <- zip [0 :: Int ..] samples, i `mod` 8 == 0, let (px, py) = sP s, let (nx, ny) = sN s ]
      curve = path2 env white 3 (map (sc . sP) samples)
      -- 縮閉線（曲率中心の軌跡）：遠くへ飛ぶ所で切る
      runs = splitRuns [ case sCenter s of { Just c | norm2 c <= 12 -> Just (sc c); _ -> Nothing } | s <- samples ]
      evolute = map (path2 env (hsb 30 90 100) 2.5) runs
      -- 現在の点・接触円・曲率中心
      osc = case sCenter q of
        Just c | norm2 c < 12 ->
          let cv = sc c
              r = abs (1 / sKappa q) * wSC w
          in [ ringAt env (hsba 30 90 100 0.5) r 1.5 cv
             , seg2 env (hsba 30 90 100 0.5) 1.5 pv cv
             , dotAt env (hsb 30 90 100) 4.5 cv ]
        _ -> []
      -- 伸開線（糸をほどく）
      inv = if not (wInvolute w) then [] else
        let tv = sc (tipOf q)
        in [ path2 env (hsb 185 80 100) 2.5 (map sc (wInv w))
           , seg2 env (hsba 0 0 100 0.7) 1.5 pv tv            -- ピンと張った糸（接線）
           , dotAt env (hsb 185 80 100) 5 tv ]
  in (if wNormals w then normals else []) ++ [curve] ++ (if wEvolute w then evolute else []) ++ osc ++ inv ++ [dotAt env white 5.5 pv]

-- Nothing の所で折れ線を切る
splitRuns :: [Maybe Point] -> [[Point]]
splitRuns = go []
  where
    go acc [] = flush acc []
    go acc (Just p : r) = go (p : acc) r
    go acc (Nothing : r) = flush acc (go [] r)
    flush acc rest = case acc of
      (_ : _ : _) -> reverse acc : rest
      _ -> rest

hud :: Env -> World -> Sample -> [Picture]
hud env w q =
  let onOff b = if b then "ON" else "off"
      nameNow = case drop (wCurve w - 1) names of { (s : _) -> s; [] -> "" }
      g = hsb 0 0 65
  in [ txt env 20 40 18 white ("1-5: curve (" ++ nameNow ++ ")   Space: " ++ (if wPaused w then "resume" else "pause") ++ "   <- ->: speed   R: restart")
     , txt env 20 64 16 white ("E: evolute " ++ onOff (wEvolute w) ++ "   N: normals " ++ onOff (wNormals w) ++ "   I: involute " ++ onOff (wInvolute w))
     , txt env 20 88 14 g "white = curve   orange = evolute (centers p + N/k, envelope of the normals)"
     , txt env 20 108 14 g ("cyan = involute (string tip q = p - s T)   string length = arc length s = " ++ nf 2 (sArc q))
     , txt env 20 128 14 g ("k = " ++ nf 3 (sKappa q) ++ "   radius of curvature 1/|k| = " ++ (if abs (sKappa q) > 1e-6 then nf 3 (abs (1 / sKappa q)) else "inf"))
     ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '5' -> setCurve env (fromEnum c - fromEnum '0') w
    | c == 'e' || c == 'E' -> w { wEvolute = not (wEvolute w) }
    | c == 'n' || c == 'N' -> w { wNormals = not (wNormals w) }
    | c == 'i' || c == 'I' -> w { wInvolute = not (wInvolute w), wInv = [] }
    | c == 'r' || c == 'R' -> w { wS = 0, wInv = [] }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPaused = not (wPaused w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 楕円（1）の縮閉線はアステロイド（4 つの尖点）。尖点は楕円のどの点に対応する？（κ が極値をとる点）
-- 2. サイクロイド（2）の伸開線が元と同じ形になることを確かめよう。これが「等時性の振り子」の仕組み
-- 3. 円（3）の縮閉線は 1 点（中心）。伸開線（らせん）は歯車の歯の形に使われている。なぜ都合がいい？
-- 4. curveFn に y = sin x などを足してみよう。縮閉線はどんな形？
-- 5. 法線の束 N だけで縮閉線が「見える」のはなぜ？（包絡線）

-- ==================================================
-- ここから下は「座標と文字の道具」です
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- | p5.js の座標（左上が原点、y は下向き）→ gloss の座標（中央が原点、y は上向き）
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- | gloss の座標 → p5.js の座標（マウスの位置を読むときに使う）
fromG :: Env -> Point -> Point
fromG env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- | p5.js の座標で文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str

-- | p5.js の座標で長方形を塗る（(x, y) は左上）
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (gx, gy) = toG env (x + w / 2, y + h / 2) in Color c (rect gx gy w h)

-- | p5.js の座標で線分を引く（w は太さ、1 以下なら細い線）
seg2 :: Env -> Color -> Float -> Point -> Point -> Picture
seg2 env c w a b
  | w <= 1 = Color c (Line [toG env a, toG env b])
  | otherwise = Color c (thickLine w (toG env a) (toG env b))

-- | p5.js の座標で折れ線を引く（色は 1 色）
path2 :: Env -> Color -> Float -> [Point] -> Picture
path2 env c w ps
  | w <= 1 = Color c (Line (map (toG env) ps))
  | otherwise = Color c (thickPath w (map (toG env) ps))

-- | p5.js の座標で点（塗りつぶした円）を打つ。r は半径
dotAt :: Env -> Color -> Float -> Point -> Picture
dotAt env c r p = at (toG env p) (Color c (disc r))

-- | p5.js の座標で輪を描く。r は半径、w は線の太さ
ringAt :: Env -> Color -> Float -> Float -> Point -> Picture
ringAt env c r w p
  | w <= 1 = at (toG env p) (Color c (Circle r))
  | otherwise = at (toG env p) (Color c (ring r w))

-- | 小数 n 桁の文字列（p5.js の nf に相当）
nf :: Int -> Float -> String
nf n x = showFFloat (Just n) x ""
