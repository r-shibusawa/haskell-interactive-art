-- ============================================
-- 05 エネルギー保存：ジェットコースター
-- レールの上をすべる玉。高さのエネルギー（位置エネルギー）と速さのエネルギー（運動エネルギー）が入れかわる
--   マウスでレールを描くと、左端から玉がスタートする。宙返りの頂上で v² ≥ gR なら落ちない
-- ドラッグ = レールを描く / 1 = 山と谷 / 2 = 宙返り / F = 摩擦 ON/OFF / スペース = スタートに戻す
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

pxPerM, gravity, mu, ds, groundY :: Float
pxPerM = 60         -- ★ 1 m = 60 px
gravity = 9.8       -- ★ 重力加速度 [m/s²]
mu = 0.15           -- ★ 摩擦（速さに比例する抵抗の係数 [1/s]）
ds = 4              -- 点の間隔 [px]
groundY = height * 0.88

histLen :: Int
histLen = 600

data World = World
  { wTrackX, wTrackY :: !(U.Vector Float)   -- 等間隔に並べたレールの点
  , wFriction :: !Bool
  , wDrawing  :: !Bool
  , wRaw      :: ![Point]                   -- 描いている途中の点（新しい順）
  , wS, wV    :: !Float                     -- レールに沿った位置 [px] と速さ [m/s]
  , wLoopTop  :: !Int
  , wLoopR    :: !Float
  , wVTop     :: !(Maybe Float)
  , hK, hU, hE :: !(Seq.Seq Float)
  }

main :: IO ()
main = runSketch (sketch "24-05 Roller Coaster" (preset 2 world0))
  { skBack  = hsb 260 30 11
  , skStep  = \_ _ w -> pure (stepWorld w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = World U.empty U.empty False False [] 0 0 (-1) 0 Nothing Seq.empty Seq.empty Seq.empty

-- 手で描いた点を、等間隔 ds に並べなおす（結果は順番どおり）
resample :: [Point] -> [Point]
resample [] = []
resample (p0 : ps) = reverse (foldl stepTo [p0] ps)
  where
    stepTo out b = case out of
      (a : _) -> walk out a b
      [] -> [b]
    walk out a b =
      let d = dist2 a b
      in if d >= ds
           then let t = ds / d
                    p = (fst a + (fst b - fst a) * t, snd a + (snd b - snd a) * t)
                in p `seq` walk (p : out) p b
           else out

preset :: Int -> World -> World
preset n w0'
  | n == 1 =
      let pts = [ (x0 + u * bigW, top + (groundY - top - 20) * (0.5 - 0.5 * cos (u * tau * 1.5)) * (1 - 0.35 * u))
                | i <- [0 .. 200 :: Int], let u = fromIntegral i / 200 ]
      in (setTrack pts w0') { wLoopTop = -1 }
  | otherwise =
      let -- 坂を下りて、半径 loopR の円を 1 周して、また進む
          loopR = min (bigW * 0.16) ((groundY - top) * 0.2)
          cx = x0 + bigW * 0.62
          bottom = groundY - 10
          a1 = [ (x0 + u * (cx - x0), top + (bottom - top) * (0.5 - 0.5 * cos (u * pi)))
               | i <- [0 .. 80 :: Int], let u = fromIntegral i / 80 ]
          a2 = [ (cx + loopR * cos a + (u * 6), bottom - loopR + loopR * sin a)
               | i <- [1 .. 120 :: Int], let u = fromIntegral i / 120, let a = pi / 2 - u * tau ]
          a3 = [ (cx + 6 + u * (x0 + bigW - cx - 6), bottom)
               | i <- [1 .. 40 :: Int], let u = fromIntegral i / 40 ]
          w = setTrack (a1 ++ a2 ++ a3) w0'
          ys = wTrackY w
          len = U.length ys
          from = floor (fromIntegral len * 0.45 :: Float) :: Int
          -- 宙返りの頂上（いちばん高い点のうち、半分より先にあるもの）
          best = U.ifoldl' (\b i y -> if i >= from && (b < 0 || y < U.unsafeIndex ys b) then i else b) (-1) ys
      in w { wLoopTop = best, wLoopR = loopR }
  where
    bigW = width * 0.46
    x0 = width * 0.03
    top = height * 0.22

setTrack :: [Point] -> World -> World
setTrack pts w =
  let t = resample pts
  in if length t < 3 then w else
     w { wTrackX = U.fromList (map fst t), wTrackY = U.fromList (map snd t)
       , wS = 0, wV = 0, wVTop = Nothing, hK = Seq.empty, hU = Seq.empty, hE = Seq.empty }

-- レールの位置 ss での (x, y, 接線 tx, ty)
trackAt :: World -> Float -> (Float, Float, Float, Float)
trackAt w ss =
  let n = U.length (wTrackX w)
      f = clampF 0 (fromIntegral n - 1.001) (ss / ds)
      i = floor f :: Int
      u = f - fromIntegral i
      ax = U.unsafeIndex (wTrackX w) i
      ay = U.unsafeIndex (wTrackY w) i
      bx = U.unsafeIndex (wTrackX w) (i + 1)
      by = U.unsafeIndex (wTrackY w) (i + 1)
  in (ax + (bx - ax) * u, ay + (by - ay) * u, (bx - ax) / ds, (by - ay) / ds)

stepWorld :: World -> World
stepWorld w0' =
  let n = U.length (wTrackX w0')
      bigL = fromIntegral (n - 1) * ds
      h = 1 / 60 / 20 :: Float
      -- ★ 接線方向の加速度 = g × (下向きの傾き)。画面は y が下向きなので ty > 0 が下り坂
      sub :: Int -> Float -> Float -> Maybe Float -> (Float, Float, Maybe Float)
      sub 0 !s !v vt = (s, v, vt)
      sub k !s !v vt =
        let (_, _, _, ty) = trackAt w0' s
            a = gravity * ty - (if wFriction w0' then mu * v else 0)
            v1 = clampF (-60) 60 (v + a * h)
            s1 = s + v1 * h * pxPerM
            (s2, v2) | s1 < 0 = (0, abs v1)
                     | s1 > bigL = (bigL, -abs v1)
                     | otherwise = (s1, v1)
            vt' = if wLoopTop w0' > 0 && abs (s2 / ds - fromIntegral (wLoopTop w0')) < 1 then Just (abs v2) else vt
        in sub (k - 1) s2 v2 vt'
      w1 | not (wDrawing w0') && n > 2 =
             let (s, v, vt) = sub (20 :: Int) (wS w0') (wV w0') (wVTop w0')
             in w0' { wS = s, wV = v, wVTop = vt }
         | otherwise = w0'
      (_, py, _, _) = trackAt w1 (wS w1)
      hgt = (groundY - py) / pxPerM
      k = 0.5 * wV w1 * wV w1
      u = gravity * hgt
  in w1 { hK = pushHist (hK w1) k, hU = pushHist (hU w1) u, hE = pushHist (hE w1) (k + u) }

drawAll :: World -> Picture
drawAll w = Pictures
  [ view ( Color (hsb 0 0 16) (boxAt 0 groundY (width * 0.5) (height - groundY))
         : Color (hsb 0 0 70) (thickPath 4 railPts)
         : if wDrawing w then [] else
             [ -- 玉と速度の矢印
               Color (hsb 130 80 100) (thickLine 3 (px, py - 14) (px + tx * wV w * 8, py - 14 + ty * wV w * 8))
             , Color (hsb 30 90 100) (at (px, py) (disc 12))
               -- スタートの高さ（摩擦なしならここまで戻ってくる）
             , Color (hsba 55 90 100 0.6) (dashPath 1 5 6 [(0, startY), (width * 0.48, startY)]) ] )
    -- 右：棒グラフと時間グラフ
  , view [Color (hsb 0 0 14) (boxAt gx by (gw * 0.35) (bh + 40))]
  , barV (gx + bs * 0.2) (by + bh + 20) bw2 bh (u / bmax) (hsb 200 70 100) "U"
  , barV (gx + bs * 1.2) (by + bh + 20) bw2 bh (k / bmax) (hsb 330 70 100) "K"
  , barV (gx + bs * 2.2) (by + bh + 20) bw2 bh ((k + u) / bmax) (hsb 55 90 100) "Total"
    -- 宙返りの判定
  , view [Color (hsb 0 0 14) (boxAt (gx + gw * 0.38) by (gw * 0.62) (bh + 40))]
  , t' 30 ("h = " ++ nf 2 hgt ++ " m   v = " ++ nf 2 (abs (wV w)) ++ " m/s")
  , t' 56 ("sqrt(2g(h0 - h)) gives " ++ nf 2 (sqrt (max 0 (2 * (e0 - u)))) ++ " m/s")
  , if wLoopTop w > 0 then loopInfo else Blank
  , txt tx' (by + 212) 14 (hsb 0 0 60) "U = mgh,  K = mv^2 / 2"
  , drawPlot gx (height * 0.56) gw (height * 0.32) [Series (hU w) 200 "U", Series (hK w) 330 "K", Series (hE w) 55 "K + U"] 0 bmax "Energy per kg [J/kg]"
  , txt 20 36 18 (gray 1) ("Drag: draw a track   1: hills   2: loop   Friction " ++ (if wFriction w then "ON" else "OFF") ++ " (F)   Space: to start")
  , txt 20 66 14 (hsb 0 0 65) "Without friction K + U stays constant: never above the start height (yellow dashes). Friction turns it into heat."
  ]
  where
    n = U.length (wTrackX w)
    railPts | wDrawing w = wRaw w
            | otherwise = let stride = max 1 (n `div` 800)
                          in [ (U.unsafeIndex (wTrackX w) i, U.unsafeIndex (wTrackY w) i)
                             | i <- [0, stride .. n - 1] ]
    (px, py, tx, ty) = trackAt w (wS w)
    hgt = (groundY - py) / pxPerM
    k = 0.5 * wV w * wV w
    u = gravity * hgt
    startY = if n > 0 then U.unsafeIndex (wTrackY w) 0 else groundY
    e0 = gravity * (groundY - startY) / pxPerM
    gx = width * 0.52
    gw = width * 0.44
    bmax = maximum [e0, k + u, 1] * 1.15
    bh = height * 0.3
    by = height * 0.14
    bs = gw * 0.35 / 3
    bw2 = bs * 0.6
    tx' = gx + gw * 0.38 + 16
    t' dy = txt tx' (by + dy) 14 (hsb 0 0 90)
    bigR = wLoopR w / pxPerM
    need = sqrt (gravity * bigR)
    loopInfo = Pictures
      [ t' 96 ("Loop radius R = " ++ nf 2 bigR ++ " m")
      , t' 122 ("Needed at top sqrt(gR) = " ++ nf 2 need ++ " m/s")
      , case wVTop w of
          Just vt | vt >= need -> txt tx' (by + 148) 14 (hsb 130 80 100) ("Top speed " ++ nf 2 vt ++ " m/s: stays on")
                  | otherwise -> txt tx' (by + 148) 14 (hsb 0 80 100) ("Top speed " ++ nf 2 vt ++ " m/s: would fall!")
          Nothing -> Blank
      , txt tx' (by + 180) 14 (hsb 0 0 60) "Need h0 >= 2.5R (no friction)"
      ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos
    | mx < width * 0.5 && my > 90 -> w { wDrawing = True, wRaw = [(mx, my)], wLoopTop = -1 }
    where (mx, my) = toP5 pos
  EventKey (MouseButton LeftButton) Up _ _
    | wDrawing w ->
        let w1 = w { wDrawing = False }
        in if length (wRaw w) > 3 then setTrack (reverse (wRaw w)) w1 else w1
  EventMotion pos | wDrawing w -> case wRaw w of
    (q : _) | dist2 q (mx, my) > 4 && length (wRaw w) < 3000 ->
      w { wRaw = (clampF 0 (width * 0.5) mx, clampF 90 groundY my) : wRaw w }
    _ -> w
    where (mx, my) = toP5 pos
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wS = 0, wV = 0, wVTop = Nothing }
  EventKey (Char '1') Down _ _ -> preset 1 w
  EventKey (Char '2') Down _ _ -> preset 2 w
  EventKey (Char c) Down _ _ | toLower c == 'f' -> w { wFriction = not (wFriction w) }
  _ -> w

-- ---------- やってみよう ----------
-- 1. スタートより高い山を描くと、玉は越えられずに戻ってくる（エネルギー保存）
-- 2. 宙返りの坂のスタートを低く描きなおして、頂上の速さが √(gR) を下回るのを確かめる
-- 3. 摩擦 ON にすると K + U がだんだん減る。減った分はどこへ行った？
-- 4. まっすぐな坂と、カーブした坂（最速降下線）、どちらが早く下に着く？ 描いて競争させてみよう


-- ==================================================
-- 道具（グラフを描く）
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- マウスの位置（gloss の座標）を p5 と同じ座標（左上が原点、y は下向き）に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は左下の位置（p5 の座標）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtC, txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y s c str = txt (x - fromIntegral (length str) * s * 0.36) y s c str
txtR x y s c str = txt (x - fromIntegral (length str) * s * 0.72) y s c str

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y w h = rect (x + w / 2) (y + h / 2) w h

nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

pushHist :: Seq.Seq Float -> Float -> Seq.Seq Float
pushHist s !v =
  let s' = s Seq.|> v
  in if Seq.length s' > histLen then Seq.drop 1 s' else s'

barV :: Float -> Float -> Float -> Float -> Float -> Color -> String -> Picture
barV x y w h f c name =
  let fh = h * clampF 0 1.2 f
  in Pictures [ view [Color c (boxAt x (y - fh) w fh)]
              , txtC (x + w / 2) (y + 16) 14 (hsb 0 0 80) name ]

-- 点線：線の長さ on、すきま off
dashPath :: Float -> Float -> Float -> [Point] -> Picture
dashPath w on off = Pictures . go True on
  where
    go draw left (a : b : rest)
      | l <= left = [thickLine w a b | draw] ++ go draw (left - l) (b : rest)
      | otherwise =
          let u = left / l
              m = (lerpF (fst a) (fst b) u, lerpF (snd a) (snd b) u)
          in [thickLine w a m | draw] ++ go (not draw) (if draw then off else on) (m : b : rest)
      where l = dist2 a b
    go _ _ _ = []

data Series = Series
  { sData :: !(Seq.Seq Float)
  , sHue  :: !Float
  , sName :: String
  }

-- 時間プロット：枠 (x, y, w, h) に series を描く（縦の範囲 lo〜hi）
drawPlot :: Float -> Float -> Float -> Float -> [Series] -> Float -> Float -> String -> Picture
drawPlot x0 y0 w h series lo hi title = Pictures
  ( view (Color (hsb 0 0 14) (boxAt x0 y0 w h) : map curve series)
  : txt (x0 + 8) (y0 + 20) 14 (hsb 0 0 70) title
  : legend (x0 + w - 8) series )
  where
    curve s =
      let d = toList (sData s)
          off = histLen - length d
      in Color (hsb (sHue s) 70 100) (thickPath 2
           [ ( remap (fromIntegral (i + off)) 0 (fromIntegral histLen - 1) x0 (x0 + w)
             , remap (clampF lo hi v) lo hi (y0 + h) y0 )
           | (i, v) <- zip [0 :: Int ..] d, even i ])
    legend _ [] = []
    legend lx (s : rest) = txtR lx (y0 + 20) 14 (hsb (sHue s) 70 100) (sName s)
                         : legend (lx - fromIntegral (length (sName s)) * 14 * 0.72 - 14) rest
