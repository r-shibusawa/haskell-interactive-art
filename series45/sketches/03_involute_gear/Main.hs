-- ============================================
-- 45-03 インボリュート歯車
-- 歯の形は「基礎円に巻いた糸をほどくときの糸の先の軌跡」（インボリュート曲線）
-- かみ合う点はいつも 2 つの基礎円の共通接線（作用線）の上を動く → 速度比が一定になる
-- 操作：←→ = 歯数 z1 / ↑↓ = 歯数 z2 / M・N = モジュール / A = 圧力角（14.5°・20°・25°）/ C = 円の表示 / スペース = 止める
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

paList :: [Float]
paList = [14.5, 20, 25]       -- ★ 圧力角（度）

turnSpeed :: Float
turnSpeed = 0.008             -- ★ 回る速さ

data World = World
  { wZ1, wZ2   :: !Int        -- ★ 歯数
  , wModule    :: !Float      -- ★ モジュール m（歯の大きさ。ピッチ円直径 = m × z）
  , wPaId      :: !Int
  , wRot1      :: !Float      -- 歯車 1 の回転角
  , wPlaying   :: !Bool
  , wCircles   :: !Bool
  , wOut1, wOut2 :: ![Point]  -- 歯車の輪郭（作り直すのはキーを押したときだけ）
  }

main :: IO ()
main = runSketch (sketch "45-03 Involute gears" (rebuild (World 14 24 16 1 0 True True [] [])))
  { skBack  = hsb 212 45 13
  , skStep  = \_ _ w -> pure (if wPlaying w then w { wRot1 = wRot1 w + turnSpeed } else w)
  , skDraw  = draw
  , skEvent = event
  }

paOf :: World -> Float
paOf w = case drop (wPaId w) paList of
  (d : _) -> d
  [] -> 20

rebuild :: World -> World
rebuild w = w { wOut1 = makeGearOutline (wZ1 w) (wModule w) pa 1 1.25, wOut2 = makeGearOutline (wZ2 w) (wModule w) pa 1 1.25 }
  where pa = paOf w * pi / 180

draw :: Env -> World -> Picture
draw env w = view env $
  [ Translate (envW env / 2 - (r2 - r1) * fit / 2) (envH env * 0.56) (Scale fit fit (Pictures gears)) ]
  -- ---- 数値
  ++ [ txt 20 40 18 (hsb 0 0 100) ("Left/Right: z1 = " ++ show z1 ++ "   Up/Down: z2 = " ++ show z2 ++ "   M/N: module " ++ fmt 0 m
                                   ++ "   A: pressure angle " ++ fmtPa ++ " deg   C: circles   Space: " ++ (if wPlaying w then "stop" else "run"))
     , txt 20 68 15 (hsb 0 0 60) "Red dots = teeth touching now. They always lie on the yellow line of action through the white pitch point -> smooth drive"
     , Color (hsbA 0 0 100 8) (rectTL (ix - 12) (iy - 26) 560 150)
     , txt ix iy 15 (hsb 0 0 100) ("Pitch circle d = m z: " ++ fmt 0 (m * fz1) ++ " / " ++ fmt 0 (m * fz2)
                                   ++ "   center distance a = m(z1 + z2)/2 = " ++ fmt 0 (m * (fz1 + fz2) / 2))
     , txt ix (iy + 24) 15 (hsb 140 60 100) ("Base circle d_b = d cos a: " ++ fmt 1 (2 * rb1) ++ " / " ++ fmt 1 (2 * rb2))
     , txt ix (iy + 48) 15 (hsb 35 90 100) ("Ratio i = z2 / z1 = " ++ fmt 3 (fz2 / fz1) ++ " (gear 2 turns " ++ fmt 3 (fz1 / fz2) ++ "x as fast, reversed)")
     , txt ix (iy + 72) 15 (hsb 0 (if eps < 1.2 then 80 else 0) 100)
         ("Contact ratio e = " ++ fmt 2 eps ++ " (" ++ show contacts ++ " pairs touching now; aim for 1.2+)")
     , txt ix (iy + 96) 15 (if under then hsb 0 80 100 else hsb 0 0 60)
         ("Min teeth without undercut 2 / sin^2 a = " ++ fmt 1 zMin ++ (if under then "  -> too few teeth (root undercut)" else "")) ]
  where
    z1 = wZ1 w
    z2 = wZ2 w
    fz1 = fromIntegral z1
    fz2 = fromIntegral z2
    m = wModule w
    pa = paOf w * pi / 180
    fmtPa = let d = paOf w in if d == fromIntegral (round d :: Int) then fmt 0 d else fmt 1 d
    r1 = m * fz1 / 2                       -- ピッチ円
    r2 = m * fz2 / 2
    rb1 = r1 * cos pa                      -- 基礎円
    rb2 = r2 * cos pa
    ra1 = r1 + m                           -- 歯先円
    ra2 = r2 + m
    -- 画面に収まるように全体を縮める
    fit = minimum [1, (envW env - 80) / (2 * (r1 + r2) + 4 * m), (envH env - 200) / (2 * max r1 r2 + 3 * m)]
    rot1 = wRot1 w
    -- ★ かみ合いの条件：歯車 1 の歯が歯車 2 の谷に入る。回転角は歯数の逆比で逆向き
    rot2 = pi + pi / fz2 - rot1 * fz1 / fz2
    c1 = -r1                               -- ピッチ点を原点に、中心は左右
    c2 = r2
    -- ---- 作用線：基礎円の共通接線。ピッチ点を通り、共通接線（縦線）から圧力角 α だけ傾く
    ux = sin pa
    uy = cos pa
    t1 = (c1 + rb1 * cos pa, -rb1 * sin pa)    -- 歯車 1 側の接点
    t2 = (c2 - rb2 * cos pa, rb2 * sin pa)     -- 歯車 2 側の接点
    -- 実際にかみ合う区間：相手の歯先円が作用線を切るところまで（接点より先ではかみ合えない）
    sEnd = min (-r1 * sin pa + sqrt (max (r1 * r1 * sin pa * sin pa + ra1 * ra1 - r1 * r1) 0)) (r2 * sin pa)
    sStart = max (-(-r2 * sin pa + sqrt (max (r2 * r2 * sin pa * sin pa + ra2 * ra2 - r2 * r2) 0))) (-r1 * sin pa)
    -- かみ合い点：作用線の上を、法線ピッチ pb = πm cos α ごとに並んで進む
    pb = pi * m * cos pa
    s0 = wrapF pb (rb1 * (rot1 + pi / (2 * fz1)))
    contactS = [s | k <- [-4 .. 4 :: Int], let s = s0 + fromIntegral k * pb, s >= sStart, s <= sEnd]
    contacts = length contactS
    eps = (sEnd - sStart) / pb                     -- かみ合い率
    zMin = 2 / (sin pa * sin pa)                   -- 切り下げが起きない最小歯数
    under = fromIntegral (min z1 z2) < zMin
    ix = 30
    iy = envH env - 150
    gears =
      [ drawOutline (hsb 200 55 70) (1.5 / fit) (wOut1 w) c1 0 rot1
      , drawOutline (hsb 28 60 80) (1.5 / fit) (wOut2 w) c2 0 rot2
      , dot (hsb 0 0 100) c1 0 (14 / fit), dot (hsb 0 0 100) c2 0 (14 / fit)
      -- 目印の歯
      , dot (hsb 0 0 100) (c1 + (r1 - m) * cos rot1) ((r1 - m) * sin rot1) (8 / fit)
      , dot (hsb 0 0 100) (c2 + (r2 - m) * cos rot2) ((r2 - m) * sin rot2) (8 / fit) ]
      ++ (if wCircles w
            then [ circ (1.2 / fit) (hsbA 0 0 100 45) c1 r1, circ (1.2 / fit) (hsbA 0 0 100 45) c2 r2         -- ピッチ円
                 , circ (1.2 / fit) (hsbA 140 70 100 70) c1 rb1, circ (1.2 / fit) (hsbA 140 70 100 70) c2 rb2 ] -- 基礎円
            else [])
      ++ [ seg (1.5 / fit) (hsbA 50 90 100 60) t1 t2
         , seg (1.5 / fit) (hsbA 50 90 100 60) (c1, 0) t1, seg (1.5 / fit) (hsbA 50 90 100 60) (c2, 0) t2
         , seg (4 / fit) (hsb 50 90 100) (sStart * ux, sStart * uy) (sEnd * ux, sEnd * uy) ]
      ++ [ dot (hsb 0 90 100) (s * ux) (s * uy) (12 / fit) | s <- contactS ]
      ++ [ dot (hsb 0 0 100) 0 0 (8 / fit) ]   -- ピッチ点

event :: Env -> Event -> World -> IO World
event _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyLeft) Down _ _ -> rebuild w { wZ1 = clampI 6 40 (wZ1 w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> rebuild w { wZ1 = clampI 6 40 (wZ1 w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> rebuild w { wZ2 = clampI 6 40 (wZ2 w - 1) }
  EventKey (SpecialKey KeyUp) Down _ _ -> rebuild w { wZ2 = clampI 6 40 (wZ2 w + 1) }
  EventKey (Char k) Down _ _
    | k `elem` "mM" -> rebuild w { wModule = clampF 6 30 (wModule w + 2) }
    | k `elem` "nN" -> rebuild w { wModule = clampF 6 30 (wModule w - 2) }
    | k `elem` "aA" -> rebuild w { wPaId = (wPaId w + 1) `mod` length paList }
    | k `elem` "cC" -> w { wCircles = not (wCircles w) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. スペースで止めて赤い点を数えよう。1 組 → 2 組 → 1 組 と入れ替わる。かみ合い率 ε はその平均
-- 2. A で圧力角を変える。25° は歯元が太く強いが ε は小さい。14.5° は？
-- 3. z1 を 6 まで減らすと歯元が細くなる（切り下げ）。17 枚（α = 20°）が境目と言われる理由を式で確かめよう
-- 4. モジュールを変えても速度比は変わらない。変わるのは何？（大きさと強さ）
-- 5. rebuild の makeGearOutline の ha（1）を 0.8 にすると「低歯」になる。ε はどう変わる？（ra1・ra2 の式も直す）

-- ==================================================
-- ここから下は「歯車の道具」です（インボリュート歯形の輪郭を作る）
-- ==================================================

-- 歯数 z、モジュール m、圧力角 pa（ラジアン）の歯車の輪郭を点の列で返す
--   歯 1 枚の中心が角度 0（+x 方向）を向く。ha・hf は歯先・歯元の高さ（モジュールの何倍か）
--   インボリュート：基礎円 rb に巻いた糸をほどくときの糸の先。ほどいた角 t の点は
--   半径 rb√(1 + t²)、角度 inv = t − atan t
makeGearOutline :: Int -> Float -> Float -> Float -> Float -> [Point]
makeGearOutline z m pa ha hf = concatMap tooth [0 .. z - 1]
  where
    fz = fromIntegral z
    r = m * fz / 2
    rb = r * cos pa
    ra = r + ha * m
    rf = max (r - hf * m) m
    half = pi / (2 * fz) + tan pa - pa                  -- 基礎円の上での歯の半角
    t0 = if rf > rb then sqrt ((rf / rb) * (rf / rb) - 1) else 0
    t1 = sqrt (max ((ra / rb) * (ra / rb) - 1) (t0 * t0))
    -- [半径, 歯の中心からの角度]。基礎円より内側はまっすぐ
    flank = [(rf, half) | rf < rb]
            ++ [ (rb * sqrt (1 + t * t), max (half - (t - atan t)) 0)
               | i <- [0 .. 8 :: Int], let t = t0 + (t1 - t0) * fromIntegral i / 8 ]
    tooth k =
      let c = fromIntegral k * 2 * pi / fz
      in (rf * cos (c - pi / fz), rf * sin (c - pi / fz))        -- 歯底の真ん中
         : [ (q * cos (c - a), q * sin (c - a)) | (q, a) <- flank ]
         ++ [ (q * cos (c + a), q * sin (c + a)) | (q, a) <- reverse flank ]

-- 輪郭を (x, y) に角度 rot で描く。歯車の形はへこみがあるので、中心からの扇を凸の部分に分けて塗り、白い縁を描く
drawOutline :: Color -> Float -> [Point] -> Float -> Float -> Float -> Picture
drawOutline col sw pts x y rot = Pictures
  [ Color col (convexFan (x, y) ps)
  , Color (hsbA 0 0 100 80) (thickPath sw (ps ++ take 1 ps)) ]
  where
    cr = cos rot
    sr = sin rot
    ps = [ (x + px * cr - py * sr, y + px * sr + py * cr) | (px, py) <- pts ]

-- 中心 o から見て一周する輪郭を塗る。gloss の Polygon は凸の形しか正しく塗れないので、
-- 中心と輪郭の点をつないだ扇を、凸のままでいられるところまでまとめて 1 枚ずつ塗る
convexFan :: Point -> [Point] -> Picture
convexFan _ [] = Blank
convexFan o (p0 : rest) = Pictures (go [p0] (rest ++ [p0]))
  where
    go grp [] = [Polygon (o : reverse grp)]
    go grp@(lastP : _) (q : qs)
      | convex (o : reverse (q : grp)) = go (q : grp) qs
      | otherwise = Polygon (o : reverse grp) : go [q, lastP] qs
    go [] _ = []
    convex ps =
      let n = length ps
          trip = zip3 ps (drop 1 ps ++ take 1 ps) (drop 2 ps ++ take 2 ps)
          cr = [ (bx - ax) * (cy - by) - (by - ay) * (cx - bx) | ((ax, ay), (bx, by), (cx, cy)) <- trip ]
      in n < 4 || all (>= -1e-3) cr || all (<= 1e-3) cr

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 左上が (x, y) の塗りつぶした長方形（p5.js の rect）
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (thickLine w a b)

-- 中心 (x, 0)、半径 r、太さ w の輪
circ :: Float -> Color -> Float -> Float -> Picture
circ w c x r = Color c (Translate x 0 (ThickCircle r w))

-- 中心 (x, y)、直径 d の塗りつぶした円
dot :: Color -> Float -> Float -> Float -> Picture
dot c x y d = Color c (Translate x y (disc (d / 2)))

clampI :: Int -> Int -> Int -> Int
clampI lo hi = max lo . min hi

-- 小数点以下 n 桁で書く（p5.js の nf）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""
