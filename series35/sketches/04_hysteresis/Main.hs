-- ============================================
-- 04 ヒステリシス
-- カスプの地形 V(x) = x⁴ + a·x² + b·x で a < 0 に固定し、b を行ったり来たりさせる
--   右へ動かすときは b = +bc でとび、左へ戻すときは b = −bc でとぶ。行きと帰りで道が違う → ループ（ヒステリシス）
--   bc = √(−8a³/27)。身近な例：磁石の磁化、サーモスタット（暖房のオン・オフ）、気分の切り替わり
-- ←→ = 往復の速さ / ↑↓ = a（谷の深さ） / M = マウスで b を動かす / スペース = 一時停止 / C = ループを消す
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Numeric (showFFloat)

amp :: Float
amp = 2.4                  -- ★ 往復の幅

data Rec = Rec !Float !Float !Bool      -- (b, x, 右向きか) の記録

data World = World
  { wA     :: !Float       -- ★ a < 0 で谷が 2 つ
  , wB     :: !Float
  , wX     :: !Float
  , wSpeed :: !Float       -- ★ 往復の速さ
  , wPhase :: !Float
  , wMouse :: !Bool
  , wPause :: !Bool
  , wDirUp :: !Bool
  , wPath  :: !(Seq Rec)
  , wUps   :: ![Float]     -- 行きでとんだ b（5 個まで）
  , wDowns :: ![Float]     -- 帰りでとんだ b（5 個まで）
  }

world0 :: World
world0 = World (-1.5) 0 (-0.9) 0.008 0 False False True Seq.empty [] []

main :: IO ()
main = runSketch (sketch "35-04 Hysteresis" world0)
  { skBack  = hsb 280 25 11
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = drawAll
  , skEvent = onEvent
  }

dV, vOf :: Float -> Float -> Float -> Float
dV a b x = 4 * x * x * x + 2 * a * x + b
vOf a b x = x ^ (4 :: Int) + a * x * x + b * x

bc :: Float -> Float
bc a = if a < 0 then sqrt (-8 * a * a * a / 27) else 0

stepWorld :: Env -> World -> World
stepWorld env w
  | wPause w = w { wDirUp = True }
  | otherwise =
      let (mx, _) = mouseP5 env
          phase = if wMouse w then wPhase w else wPhase w + wSpeed w
          b | wMouse w = clampF (-3) 3 (remap mx (envW env * 0.42) (envW env - 40) (-3) 3)
            | otherwise = amp * sin phase
          x = clampF (-1.8) 1.8 (descend (20 :: Int) (wX w))
          descend 0 v = v
          descend k v = descend (k - 1) (v - 0.004 * dV (wA w) b v)
          up = b >= wB w
          jump = abs (x - wX w) > 0.08
          keep5 l = drop (length l - 5) l
      in w { wPhase = phase, wB = b, wX = x, wDirUp = up
           , wPath = pushN 2000 (Rec b x up) (wPath w)
           , wUps = if jump && up then keep5 (wUps w ++ [b]) else wUps w
           , wDowns = if jump && not up then keep5 (wDowns w ++ [b]) else wDowns w }

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      a = wA w
      b = wB w
      x = wX w
      py = 110
      ph = h - py - 40
      panel = hsb 280 20 17
      warm = hsb 25 90 100
      cool = hsb 195 80 100
      -- 左：地形とボール
      lx = 30
      lw = wd * 0.38
      toX xx = lx + (xx + 1.8) / 3.6 * lw
      toY vv = py + ph * 0.45 - vv * ph * 0.12
      curve = [ (toX xx, clampF py (py + ph) (toY (vOf a b xx))) | i <- [0 .. 160 :: Int], let xx = -1.8 + 3.6 * fromIntegral i / 160 ]
      -- 右：(b, x) の図
      rx = lx + lw + 30
      rw = wd - rx - 30
      toB bb = rx + (bb + 3) / 6 * rw
      toXr xx = py + ph / 2 - xx / 1.6 * (ph / 2 - 20)
      -- 平衡点の S 字（b = −4x³ − 2a·x）。真ん中（不安定）は細く
      sCurve = [ Color (hsbA 0 0 100 (if stable then 35 else 20))
                   (thickLine (if stable then 8 else 2) (toB b0, toXr x0) (toB b1, toXr x1))
               | i <- [0 .. 319 :: Int]
               , let x0 = -1.6 + 3.2 * fromIntegral i / 320
                     x1 = x0 + 0.01
                     b0 = -4 * x0 * x0 * x0 - 2 * a * x0
                     b1 = -4 * x1 * x1 * x1 - 2 * a * x1
                     stable = 12 * x0 * x0 + 2 * a > 0
               , abs b0 <= 3, abs b1 <= 3 ]
      c = bc a
      recs = toList (wPath w)
      trail = zipWith (\(Rec b0 x0 _) (Rec b1 x1 up) ->
                  Color (withA 0.7 (if up then warm else cool)) (thickLine 3 (toB b0, toXr x0) (toB b1, toXr x1)))
                recs (drop 1 recs)
      showJ = intercalate ", " . map (nf 2)
  in Pictures
    [ onP5 env (Pictures
        [ Color panel (box lx py lw ph)
        , Color (hsb 300 35 45) (underCurve (py + ph) curve)
        , Color (if x > 0 then warm else cool) (at (toX x, clampF (py + 13) (py + ph) (toY (vOf a b x) - 13)) (disc 13))
        , Color panel (box rx py rw ph)
        , Color (hsbA 0 0 100 15) (Pictures [Line [(rx, toXr 0), (rx + rw, toXr 0)], Line [(toB 0, py), (toB 0, py + ph)]])
        , Pictures sCurve
        , Color (hsbA 0 80 100 60) (Pictures [ Line [(toB (s * c), py), (toB (s * c), py + ph)] | s <- [-1, 1] ])
        , Pictures trail
        , Color (hsb 45 90 100) (at (toB b, toXr x) (disc 8))
        ])
    , txt env (lx + 16) (py + 28) 14 (gray 0.85) ("b = " ++ nf 2 b ++ (if wDirUp w then "  ->" else "  <-"))
    , txt env (rx + 16) (py + 28) 14 (gray 0.85) ("Across b (cause), up x (state). Red: jump at b = +-" ++ nf 2 c)
    , txt env (rx + 16) (py + 50) 14 (gray 0.85) "White thick = valley, thin = hill."
    , txt env (rx + 16) (py + ph - 40) 14 warm ("Jumped going ->: " ++ showJ (wUps w))
    , txt env (rx + 16) (py + ph - 16) 14 cool ("Jumped going <-: " ++ showJ (wDowns w))
    , txt env 20 36 18 white ("a = " ++ nf 2 a ++ " (Up/Dn)  speed " ++ nf 0 (wSpeed w * 1000) ++ " (<- ->)  M: mouse "
                              ++ onOff (wMouse w) ++ "  Space: " ++ (if wPause w then "resume" else "pause") ++ "  C: clear")
    , txt env 20 62 14 (gray 0.6) "At the same b = 0 the state depends on where it came from: it has memory. As a nears 0"
    , txt env 20 84 14 (gray 0.6) "the loop gets thinner, and for a > 0 it is gone."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = pure $ case ev of
  EventKey (SpecialKey KeyUp) Down _ _    -> w { wA = min 0.5 (wA w + 0.1), wPath = Seq.empty }
  EventKey (SpecialKey KeyDown) Down _ _  -> w { wA = max (-2.5) (wA w - 0.1), wPath = Seq.empty }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wSpeed = min 0.05 (wSpeed w * 1.4) }
  EventKey (SpecialKey KeyLeft) Down _ _  -> w { wSpeed = max 0.001 (wSpeed w / 1.4) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wPause = not (wPause w) }
  EventKey (Char c) Down _ _
    | c `elem` "mM" -> w { wMouse = not (wMouse w) }
    | c `elem` "cC" -> w { wPath = Seq.empty, wUps = [], wDowns = [] }
  _ -> w

-- ---------- やってみよう ----------
-- 1. a = −0.5, −1.5, −2.5 でループの幅を測り、bc = √(−8a³/27) と比べよう
-- 2. amp を 1.0 にすると（bc より小さい往復）、ボールは一度もとばない。なぜ？
-- 3. 速さを最大にするとループが太る（遅れ）。本物の磁石でも速く動かすとループが広がる
-- 4. サーモスタット：b = 室温、x = 暖房オン/オフとして、室温を b で上下させるシミュレーションにしてみよう


-- ==================================================
-- ここから下は「道具」（座標の変換・塗りつぶし・記録・色・数字）
-- ==================================================
-- 曲線の下を塗る。gloss はへこんだ多角形を塗れないので、細い台形に分ける
underCurve :: Float -> [Point] -> Picture
underCurve bottom ps = Pictures (zipWith quad ps (drop 1 ps))
  where quad (x0, y0) (x1, y1) = Polygon [(x0, bottom), (x1, bottom), (x1, y1), (x0, y0)]

-- 長さ n までの記録に足す
pushN :: Int -> a -> Seq a -> Seq a
pushN n x s = let s' = s |> x in if Seq.length s' > n then Seq.drop 1 s' else s'

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
