-- ============================================
-- 08 ケプラー運動と万有引力
-- 太陽の引力 F = GMm / r² だけで、惑星は楕円を描く（第 1 法則）
--   同じ時間に掃く面積は同じ（第 2 法則）、周期² ∝ 長半径³（第 3 法則）を確かめる
-- ドラッグ = 惑星を置いて投げる（押した所が位置、引っぱった向きと長さが速度）/ ↑↓ = 時間の速さ / C = 記録を消す
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Text.Printf (printf)

gm :: Float
gm = 4e6            -- ★ 万有引力定数 × 太陽の質量 [px³/s²]

sectorsN :: Int
sectorsN = 12       -- ★ 1 周を何個の扇形に分けるか

-- 惑星（太陽が原点）
data Planet = Planet { px, py, pvx, pvy :: !Float }

-- 扇形：通った点（間引いて持つ）と、掃いた面積
data Sector = Sector { secPts :: ![Point], secArea :: !Float }

-- 測った周期
data Rec = Rec { rA, rT, rE :: !Float }

-- 今の扇形を作っている途中の点の情報（新しい順）
data Cur = Cur { curPts :: ![Point], curArea :: !Float, curCount :: !Int }

data World = World
  { wPlanet  :: !(Maybe Planet)
  , wAim     :: !(Maybe Point)
  , wScale   :: !Float            -- ★ 時間の速さ
  , wSectors :: !(Seq.Seq Sector)
  , wCur     :: !Cur
  , wTSec, wSecDT :: !Float
  , wRecords :: ![Rec]
  , wAngAcc, wLastAng, wTOrbit :: !Float
  , wTrail   :: !(Seq.Seq Point)
  }

main :: IO ()
main = runSketch (sketch "24-08 Kepler Orbits" (launch 0 (-170) 175 0 world0))
  { skBack  = hsb 235 40 8
  , skStep  = \env _ w -> pure (stepWorld (envFrame env) w)
  , skDraw  = \env w -> drawAll (toP5 (envMouse env)) w
  , skEvent = \env ev w -> pure (onEvent (toP5 (envMouse env)) ev w)
  }

world0 :: World
world0 = World Nothing Nothing 1 Seq.empty (Cur [] 0 0) 0 1 [] 0 0 0 Seq.empty

sunX, sunY :: Float
sunX = width * 0.25
sunY = height * 0.52

launch :: Float -> Float -> Float -> Float -> World -> World
launch x y vx vy w =
  let p = Planet x y vx vy
      (_, _, _, _, t) = elements p
  in w { wPlanet = Just p
       , wSecDT = if isInfinite t then 1 else t / fromIntegral sectorsN
       , wSectors = Seq.empty, wCur = Cur [(x, y)] 0 1, wTSec = 0
       , wAngAcc = 0, wLastAng = atan2 y x, wTOrbit = 0, wTrail = Seq.empty }

-- 軌道要素：エネルギー E と角運動量 h から 長半径 a・離心率 e・周期 T
elements :: Planet -> (Float, Float, Float, Float, Float)
elements p =
  let r = sqrt (px p * px p + py p * py p)
      v2 = pvx p * pvx p + pvy p * pvy p
      en = v2 / 2 - gm / r
      h = px p * pvy p - py p * pvx p
      a = -gm / (2 * en)
      e = sqrt (max 0 (1 + 2 * en * h * h / (gm * gm)))
      t = if en < 0 then tau * sqrt (a * a * a / gm) else 1 / 0
  in (en, h, a, e, t)

accel :: Float -> Float -> (Float, Float)
accel x y =
  let r2 = x * x + y * y
      r = sqrt r2
      f = -gm / (r2 * r)
  in (f * x, f * y)

addPoint :: Point -> Cur -> Cur
addPoint q@(x2, y2) c = case curPts c of
  ((x1, y1) : _) -> c { curPts = q : curPts c, curArea = curArea c + (x1 * y2 - x2 * y1), curCount = curCount c + 1 }
  [] -> c { curPts = [q], curCount = 1 }

stepWorld :: Int -> World -> World
stepWorld frame w = case (wPlanet w, wAim w) of
  (Just p0, Nothing) ->
    let total = wScale w / 60
        -- ★ 速度ベルレ法（リープフロッグ）で細かく進める。太陽の近くでは自動で細かく
        loop :: Int -> Float -> Planet -> World -> (Maybe Planet, World)
        loop !guard !done !p !s
          | done >= total || guard >= 4000 = (Just p, s)
          | otherwise =
              let r = sqrt (px p * px p + py p * py p)
                  h = min (total - done) (0.0005 * (r / 100) ** 1.5)
                  (ax, ay) = accel (px p) (py p)
                  vx1 = pvx p + ax * h / 2
                  vy1 = pvy p + ay * h / 2
                  x1 = px p + vx1 * h
                  y1 = py p + vy1 * h
                  (ax2, ay2) = accel x1 y1
                  p1 = Planet x1 y1 (vx1 + ax2 * h / 2) (vy1 + ay2 * h / 2)
                  r1 = sqrt (x1 * x1 + y1 * y1)
                  s1 = afterStep h p1 s
              in if r1 < 14 || isNaN r1 || isInfinite r1
                   then (Nothing, s)        -- 太陽にぶつかった
                   else loop (guard + 1) (done + h) p1 s1
        (mp, w1) = loop 0 0 p0 w
    in case mp of
         Nothing -> w1 { wPlanet = Nothing }
         Just p ->
           let r = sqrt (px p * px p + py p * py p)
               cur' = if even frame && curCount (wCur w1) < 4000 then addPoint (px p, py p) (wCur w1) else wCur w1
               tr = wTrail w1 Seq.|> (px p, py p)
           in w1 { wPlanet = if r > 6000 then Nothing else Just p   -- 飛んでいった
                 , wCur = cur'
                 , wTrail = if Seq.length tr > 1500 then Seq.drop 1 tr else tr }
  _ -> w

-- 1 刻みごとの記録：扇形の区切りと、周期の測定
afterStep :: Float -> Planet -> World -> World
afterStep h p w =
  let tSec = wTSec w + h
      tOrbit = wTOrbit w + h
      here = (px p, py p)
      w1 | tSec >= wSecDT w =
             let c = addPoint here (wCur w)
                 sec = Sector (thin 60 (reverse (curPts c))) (abs (curArea c) / 2)
                 ss = wSectors w Seq.|> sec
             in w { wSectors = if Seq.length ss > sectorsN then Seq.drop 1 ss else ss
                  , wCur = Cur [here] 0 1, wTSec = tSec - wSecDT w }
         | otherwise = w { wTSec = tSec }
      -- 1 周したら周期を記録（角度の合計が 2π を超えたとき）
      ang = atan2 (py p) (px p)
      d0 = ang - wLastAng w
      d | d0 > pi = d0 - tau
        | d0 < -pi = d0 + tau
        | otherwise = d0
      acc = wAngAcc w + d
  in if abs acc >= tau
       then let (_, _, a, e, _) = elements p
                rs = wRecords w1 ++ [Rec a tOrbit e]
            in w1 { wRecords = if length rs > 8 then drop 1 rs else rs
                  , wAngAcc = acc - signum acc * tau, wLastAng = ang, wTOrbit = 0 }
       else w1 { wAngAcc = acc, wLastAng = ang, wTOrbit = tOrbit }

-- 点を n 個くらいまで間引く（最後の点は残す）
thin :: Int -> [a] -> [a]
thin n xs =
  let len = length xs
      k = max 1 (len `div` n)
      keep = [x | (i, x) <- zip [0 ..] xs, i `mod` k == 0 || i == len - 1]
  in keep

drawAll :: Point -> World -> Picture
drawAll mouse w = Pictures
  [ view [ Translate sunX sunY (Pictures
             ( zipWith sectorPic [0 :: Int ..] (toList (wSectors w))
            ++ maybe [] planetPics (wPlanet w)
            ++ [Color (hsb 45 90 100) (disc 14)] ))
         , case wAim w of
             Just a -> Pictures [ Color (hsb 130 80 100) (thickLine 3 a mouse)
                                , Color (hsb 190 70 100) (at a (disc 8)) ]
             Nothing -> Blank ]
  , drawPanel w
  , txt 20 36 18 (gray 1) ("Drag: throw a planet   Time x" ++ nf 2 (wScale w) ++ " (Up/Down)   C: clear records")
  , txt 20 66 14 (hsb 0 0 65) ("Each sector = area swept in the same time (1/" ++ show sectorsN ++ " period). Fast near the sun, slow far away: same area.")
  ]
  where
    -- 扇形（同じ時間に掃いた面積）。へこんだ形でも正しく塗れるように、三角形に分けて塗る
    sectorPic i s =
      let c = hsba (if odd i then 200 else 30) 60 80 0.35
          ps = secPts s
      in Color c (Pictures [Polygon [(0, 0), a, b] | (a, b) <- zip ps (drop 1 ps)])
    planetPics p =
      let (en, _, a, e, _) = elements p
          r = sqrt (px p * px p + py p * py p)
          vr = px p * pvx p + py p * pvy p
          v2 = pvx p * pvx p + pvy p * pvy p
          -- 離心率ベクトルから向きを求める
          ex = (v2 - gm / r) * px p / gm - vr * pvx p / gm
          ey = (v2 - gm / r) * py p / gm - vr * pvy p / gm
          om = atan2 ey ex
          b = a * sqrt (max 0 (1 - e * e))
          -- 予想される楕円（点線）
          ellipsePts = [ let qx = a * cos t - a * e
                             qy = b * sin t
                         in (qx * cos om - qy * sin om, qx * sin om + qy * cos om)
                       | t <- [0, 0.05 .. tau + 0.05] ]
      in [ if en < 0 then Color (hsb 0 0 60) (dashPath 1 5 7 ellipsePts) else Blank
         , Color (hsba 45 60 100 0.6) (thickPath 1.5 (every2 (toList (wTrail w))))
         , Color (hsb 190 70 100) (at (px p, py p) (disc 8))
         , Color (hsb 130 80 100) (thickLine 2 (px p, py p) (px p + pvx p * 0.3, py p + pvy p * 0.3)) ]

-- 右：面積の表と第 3 法則の表
drawPanel :: World -> Picture
drawPanel w = Pictures
  [ view ( Color (hsb 0 0 12) (boxAt gx gy gw (height * 0.34))
         : [ Color (hsb (if odd i then 200 else 30) 60 80)
               (boxAt (gx + 20 + fromIntegral i * bw) (gy + height * 0.3 - hh) (bw - 4) hh)
           | (i, s) <- zip [0 :: Int ..] secs
           , let hh = secArea s / amax * (height * 0.2) ] )
  , txt (gx + 14) (gy + 26) 15 (hsb 0 0 85) ("Second law: area of the last " ++ show (length secs) ++ " sectors")
  , case wPlanet w of
      Just p -> let (_, h, _, _, _) = elements p
                in txt (gx + 14) (gy + 48) 14 (hsb 0 0 70) ("Theory: (h/2) dt = " ++ nf 0 (abs h / 2 * wSecDT w) ++ " px^2  (h = r x v is constant)")
      Nothing -> Blank
  , view [Color (hsb 0 0 12) (boxAt gx ty gw (height * 0.4))]
  , txt (gx + 14) (ty + 26) 15 (hsb 0 0 85) "Third law: T^2 / a^3 is the same for all orbits (4 pi^2 / GM)"
  , Pictures (zipWith (\c s -> txt c (ty + 56) 14 (hsb 0 0 60) s) cols ["a [px]", "e", "T [s]", "T^2/a^3 (x1e-6)"])
  , Pictures (concat (zipWith recRow [0 ..] (wRecords w)))
  , txt (gx + 20) (ty + height * 0.4 - 36) 14 (hsb 55 90 100) ("Theory 4 pi^2 / GM = " ++ nf 4 (4 * pi * pi / gm * 1e6))
  , case wPlanet w of
      Just p ->
        let (en, _, a, e, t) = elements p
        in txt (gx + 20) (ty + height * 0.4 - 14) 14 (hsb 0 0 70)
             (if en < 0 then "Now: a = " ++ nf 0 a ++ ", e = " ++ nf 2 e ++ ", T = " ++ nf 2 t ++ " s"
              else "Energy >= 0: hyperbola, it never comes back")
      Nothing -> txt (gx + 20) (ty + height * 0.4 - 14) 14 (hsb 0 80 100) "No planet. Drag to throw one"
  ]
  where
    gx = width * 0.52
    gy = height * 0.12
    gw = width * 0.44
    ty = height * 0.5
    secs = toList (wSectors w)
    amax = foldl' max 1 (map secArea secs)
    bw = (gw - 40) / fromIntegral sectorsN
    cols = [gx + 20, gx + gw * 0.25, gx + gw * 0.45, gx + gw * 0.62]
    recRow :: Float -> Rec -> [Picture]
    recRow j r = zipWith (\c s -> txt c (ty + 82 + j * 24) 14 (hsb 0 0 90) s) cols
      [nf 0 (rA r), nf 2 (rE r), nf 2 (rT r), nf 4 (rT r * rT r / (rA r * rA r * rA r) * 1e6)]

onEvent :: Point -> Event -> World -> World
onEvent mouse ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos
    | mx < width * 0.5 && my > 90 -> w { wAim = Just (mx, my) }
    where (mx, my) = toP5 pos
  EventKey (MouseButton LeftButton) Up _ _ -> case wAim w of
    Just (ax, ay) ->
      let x = ax - sunX
          y = ay - sunY
          w1 = w { wAim = Nothing }
      in if x * x + y * y > 30 * 30
           then launch x y ((fst mouse - ax) * 1.5) ((snd mouse - ay) * 1.5) w1
           else w1
    Nothing -> w
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wScale = min (wScale w * 2) 16 }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wScale = max (wScale w / 2) 0.125 }
  EventKey (Char c) Down _ _ | toLower c == 'c' -> w { wRecords = [] }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 円になるように投げる（速さ v = √(GM/r)）。e ≈ 0 になる？ 少し速くすると楕円、√2 倍で放物線（脱出）
-- 2. 細長い楕円を投げて、扇形の形と面積の棒を見る。形は違うのに面積は同じ
-- 3. 大きさの違う軌道を 3 つ以上投げて、表の T²/a³ がそろうことを確かめる（第 3 法則）
-- 4. accel の力を 1/r² から 1/r³ や 1/r に変えると？ 楕円が閉じなくなる（閉じるのは 1/r² と r だけ）


-- ==================================================
-- 道具
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

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y w h = rect (x + w / 2) (y + h / 2) w h

nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

every2 :: [a] -> [a]
every2 (a : _ : c : rest) = a : every2 (c : rest)
every2 xs = xs

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
