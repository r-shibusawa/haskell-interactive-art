-- ============================================
-- 09 偏光とマリュスの法則
-- 光は横波。偏光板は「ある向きの振動」だけを通す
--   偏光板を 2 枚重ねたときの透過する強さ：  I = I0 cos²θ   （θ = 2 枚の向きの差）   ← マリュスの法則
--   90° で真っ暗。でも間に 45° の 3 枚目を入れると光が通る！（通すたびに向きが変わる）
-- 偏光板の上でドラッグ = 回す / 1〜3 = 偏光板を選ぶ、←→ = 選んだ板を回す / I = 3 枚目を入れる・抜く / A = 自動で回す / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.List (foldl')
import Numeric (showFFloat)

radiusP :: Float
radiusP = 78               -- ★ 偏光板の半径

data World = World
  { wTh0, wTh1, wTh2 :: !Float   -- ★ 偏光板の向き（度）：1 枚目、2 枚目、真ん中の 3 枚目
  , wThird  :: !Bool
  , wSel    :: !Int
  , wAuto   :: !Bool
  , wDrag   :: !Int              -- つかんでいる板（−1 = なし）
  }

world0 :: World
world0 = World 0 90 45 False 1 False (-1)

main :: IO ()
main = runSketch (sketch "36-09 Polarization and Malus's Law" world0)
  { skBack  = hsb 230 40 8
  , skStep  = \_ _ w -> pure (if wAuto w then w { wTh1 = fmod360 (wTh1 w + 0.6) } else w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

polOf :: World -> Int -> Float
polOf w 0 = wTh0 w
polOf w 1 = wTh1 w
polOf w _ = wTh2 w

setPol :: Int -> Float -> World -> World
setPol 0 v w = w { wTh0 = v }
setPol 1 v w = w { wTh1 = v }
setPol _ v w = w { wTh2 = v }

-- 画面上の偏光板の位置（順番どおり）：(板の番号, x, y)
stations :: Env -> World -> [(Int, Float, Float)]
stations env w =
  let y = envH env * 0.42
      wd = envW env
  in [(0, wd * 0.3, y)]
     ++ [(2, wd * 0.5, y) | wThird w]
     ++ [(1, wd * (if wThird w then 0.7 else 0.62), y)]

-- ★ 各段の光の強さと偏光の向きを計算（マリュスの法則を順に適用）
--    最初の Nothing は「偏光板の前：強さ 1、無偏光」
propagate :: Env -> World -> [(Float, Maybe Float)]
propagate env w =
  case stations env w of
    [] -> [(1, Nothing)]
    ((i0, _, _) : rest) ->
      let th0 = polOf w i0
          -- 1 枚目：無偏光の半分が通る
          stepOne (acc, i, th) (k, _, _) =
            let t2 = polOf w k
                dth = (t2 - th) * pi / 180
                i' = i * cos dth * cos dth
            in ((i', Just t2) : acc, i', t2)
          (out, _, _) = foldl' stepOne ([(0.5, Just th0), (1, Nothing)], 0.5, th0) rest
      in reverse out

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      st = stations env w
      out = propagate env w
      y = h * 0.42
      r = radiusP
      -- 光の帯（強さで明るさ、偏光の向きを小さな線で描く）
      xs = [60] ++ [ x | (_, x, _) <- st ] ++ [wd - 60]
      spans = zip3 xs (drop 1 xs) out
      lineAt x a len = thickLine 2 (x - len * cos a, y - len * sin a) (x + len * cos a, y + len * sin a)
      beam (x0, x1, (i0, th)) =
        let marks = [ x0 + 30 + 36 * fromIntegral k
                    | k <- [0 .. ceiling ((x1 - 20 - (x0 + 30)) / 36) - 1 :: Int] ]
            mark x = case th of
              Nothing -> Pictures [ lineAt x (a * pi / 180) 14 | a <- [0, 45, 90, 135] ]
              Just t -> lineAt x (t * pi / 180) 16
        in Pictures
          [ Color (hsbA 50 30 100 (10 + 70 * i0)) (box x0 (y - 26) (x1 - x0) 52)
          , Color (hsbA 50 60 100 (20 + 80 * i0)) (Pictures (map mark marks)) ]
      beamLabels = [ txt env ((x0 + x1) / 2 - 36) (y + 60) 16 white ("I = " ++ nf 3 i0)
                   | (x0, x1, (i0, _)) <- spans ]
      -- 偏光板
      plate (idx, sx, sy) =
        let th = polOf w idx * pi / 180
            sel = idx == wSel w
            stripe k =
              let off = k * 20
                  len = sqrt (max 0 (r * r - off * off))
              in Line [ (sx - len * cos th - off * sin th, sy - len * sin th + off * cos th)
                      , (sx + len * cos th - off * sin th, sy + len * sin th + off * cos th) ]
        in Pictures
          [ Color (hsbA 200 50 60 70) (at (sx, sy) (disc r))
          , Color (hsbA 200 20 100 60) (Pictures (map stripe [-3 .. 3]))
          , Color (if sel then hsb 45 90 100 else white) (at (sx, sy) (ring r (if sel then 4 else 2))) ]
      plateLabels = [ txt env (sx - 60) (sy - r - 14) 16 white
                        ("Plate " ++ show (idx + 1) ++ "  " ++ nf 0 (wrapF 360 (polOf w idx)) ++ " deg")
                    | (idx, sx, sy) <- st ]
      -- 透過率のグラフ：θ（2 枚の差）に対する cos²θ、現在の点
      gx = 80
      gy = h - 60
      gw = wd - 160
      gh = 150
      curve = [ (gx + th / 180 * gw, gy - cos (th * pi / 180) ^ (2 :: Int) * gh)
              | k <- [0 .. 90 :: Int], let th = fromIntegral k * 2 ]
      dth = wrapF 180 (wTh1 w - wTh0 w)
      lastI = foldl' (\_ (i0, _) -> i0) 0 out
      iNow = lastI / 0.5
      markX = gx + dth / 180 * gw
  in Pictures
    [ onP5 env (Pictures
        ( map beam spans ++ map plate st ++
        [ Color (hsbA 0 0 100 6) (box (gx - 10) (gy - gh - 10) (gw + 20) (gh + 20))
        , Color (hsb 170 60 100) (thickPath 2 curve)
        , Color (hsbA 0 0 100 40) (Line [(markX, gy), (markX, gy - gh)])
        , Color (hsb 45 90 100) (at (markX, gy - iNow * gh) (disc 6)) ]))
    , Pictures beamLabels
    , Pictures plateLabels
    , txt env gx (gy + 18) 14 (hsb 0 0 70) "0"
    , txt env (gx + gw / 2 - 10) (gy + 18) 14 (hsb 0 0 70) "90"
    , txt env (gx + gw - 30) (gy + 18) 14 (hsb 0 0 70) "180"
    , txt env (gx + 10) (gy - gh + 10) 14 (hsb 0 0 70)
        ("Cyan = cos^2 th (difference of plates 1 and 2)   Yellow = now " ++ nf 3 iNow
         ++ (if wThird w then " (plate 3 moves it off the curve)" else ""))
    , txt env 20 40 18 white
        ("1-3: select plate (now " ++ show (wSel w + 1) ++ ")  L/R: rotate  I: 3rd plate "
         ++ (if wThird w then "in" else "out") ++ "  A: auto " ++ onOff (wAuto w) ++ "  R: reset")
    , txt env 20 68 15 (hsb 0 0 60)
        "Drag a plate to turn it. Plate 1 halves the light, plate 2 multiplies by cos^2 th."
    , txt env 20 90 15 (hsb 0 0 60)
        "Set the two plates 90 deg apart to go dark, then press I to insert a 45 deg plate between them."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    foldl' (\v (i, sx, sy) -> if dist2 mouse (sx, sy) < 90 then v { wDrag = i, wSel = i } else v) w (stations env w)
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = -1 }
  EventMotion _ | wDrag w >= 0 && envDown env ->
    case [ (sx, sy) | (i, sx, sy) <- stations env w, i == wDrag w ] of
      ((sx, sy) : _) -> setPol (wDrag w) (atan2 (snd mouse - sy) (fst mouse - sx) * 180 / pi) w
      [] -> w
  EventKey (SpecialKey KeyLeft) Down _ _ -> setPol (wSel w) (polOf w (wSel w) - 3) w
  EventKey (SpecialKey KeyRight) Down _ _ -> setPol (wSel w) (polOf w (wSel w) + 3) w
  EventKey (Char c) Down _ _
    | c == '1' -> w { wSel = 0 }
    | c == '2' -> w { wSel = 1 }
    | c == '3' -> w { wThird = True, wSel = 2 }
    | c `elem` "iI" -> w { wThird = not (wThird w) }
    | c `elem` "aA" -> w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> w { wTh0 = 0, wTh1 = 90, wTh2 = 45, wThird = False, wAuto = False }
  _ -> w
  where mouse = mouseP5 env

-- ---------- やってみよう ----------
-- 1. 2 枚を 90° にして真っ暗にする。I で 3 枚目を入れて 45° に回すと、どれくらい光が戻る？（0.5 × cos²45° × cos²45° = 0.125）
-- 2. 3 枚目を 0°、90° にすると光は通らない。通るのは「間の角度」のときだけ。なぜ？
-- 3. 板を 10 枚にして 9° ずつ回したら、どれくらい通る？（propagate を改造して計算）
-- 4. 液晶ディスプレイは「偏光板 2 枚 + 向きを変える液晶」。スマホの画面を偏光サングラスごしに回して見てみよう


-- ==================================================
-- ここから下は「道具」（座標の変換・色・数字）
-- ==================================================
-- p5.js と同じ座標（左上が原点、y は下向き）で描いた絵を画面に合わせる
onP5 :: Env -> Picture -> Picture
onP5 env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1)

-- p5.js の text(s, x, y) と同じ位置に文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c = label (x - envW env / 2) (envH env / 2 - y) s c

-- マウスの位置（左上が原点、y は下向き）
mouseP5 :: Env -> Point
mouseP5 env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上が (x, y) の長方形（p5.js の rect）
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = rect (x + w / 2) (y + h / 2) w h

-- 透明度を 0〜100 で書く HSB（p5.js と同じ）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- 角度を 360 で割った余り
fmod360 :: Float -> Float
fmod360 x = x - 360 * fromIntegral (truncate (x / 360) :: Int)
