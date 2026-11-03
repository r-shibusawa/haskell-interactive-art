-- ============================================
-- 14 カタストロフィー・ブラシ
-- 画面全体がコントロール平面 (a, b)：横 = b、縦 = a。マウスで描くと、カスプの系 V = x⁴ + a·x² + b·x の状態 x が線の色になる
--   上の谷（x > 0）= 暖色、下の谷（x < 0）= 寒色。折り目をこえて谷がとぶと、火花が散って「カン」と鳴る
--   同じ場所でも、どちらから来たかで色が違う（記憶をもつ絵筆）。自由制作のベース：色・太さ・音を変えよう
-- クリックで音をスタート / マウス = 描く / スペース = ペンの上げ下げ / H = カスプの線 / S = 保存 / C = 消す / 1〜3 = 色の組
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM, when)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Numeric (showFFloat)

aMin, aMax, bRange :: Float
aMin = -3                  -- ★ 画面に写すコントロールの範囲
aMax = 1.5
bRange = 3

maxStrokes :: Int
maxStrokes = 2500          -- ★ 覚えておく線の数（これをこえると古い線から消える）

toneEvery :: Int
toneEvery = 10             -- ★ 連続音の代わりに、何フレームごとに短い音を鳴らすか

data Spark = Spark !Float !Float !Float !Float !Float !Color   -- x y vx vy life 色

data World = World
  { wA      :: !Float
  , wB      :: !Float
  , wX      :: !Float
  , wArt    :: !(Seq Picture)   -- 描いた絵（線の記録）
  , wPen    :: !Bool
  , wCusp   :: !Bool
  , wPal    :: !Int
  , wSparks :: ![Spark]
  , wPrev   :: !(Maybe Point)
  , wJumps  :: !Int
  , wAudio  :: !Bool
  }

world0 :: World
world0 = World 0 0 1 Seq.empty True True 1 [] Nothing 0 False

main :: IO ()
main = runSketch (sketch "35-14 Catastrophe Brush" world0)
  { skBack  = hsb 240 30 8
  , skStep  = stepWorld
  , skDraw  = drawAll
  , skEvent = onEvent
  }

-- ★ 状態 x → 色（palette ごとに変える）。透明度は 0〜100
inkColor :: Int -> Float -> Float -> Color
inkColor pal xx alpha =
  let s = clampF (-1) 1 (xx / 1.6)
  in case pal of
       1 -> if s > 0 then hsbA (20 + 30 * s) 85 100 alpha else hsbA (190 - 30 * s) 75 100 alpha
       2 -> if s > 0 then hsbA 320 60 100 alpha else hsbA 150 70 90 alpha
       _ -> hsbA 0 0 (50 + 50 * s) alpha

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  let m@(mx, my) = mouseP5 env
      wd = envW env
      h = envH env
      -- マウス → コントロール
      b = clampF (negate bRange) bRange (remap mx 0 wd (negate bRange) bRange)
      a = clampF aMin aMax (remap my 0 h aMax aMin)
      descend :: Int -> Float -> Float
      descend 0 v = v
      descend k v = descend (k - 1) (v - 0.004 * (4 * v * v * v + 2 * a * v + b))
      x = clampF (-1.8) 1.8 (descend 20 (wX w0))
      jumped = abs (x - wX w0) > 0.06
      pal = wPal w0
  new <- if not jumped then pure [] else do
    when (wAudio w0) (playTone (if x > 0 then 660 else 330) 0.3 0.5 Triangle)
    replicateM 24 $ do
      th <- randomRIO (0, tau)
      sp <- randomRIO (1, 6)
      pure (Spark mx my (sp * cos th) (sp * sin th) 40 (inkColor pal x 100))
  -- 線を描く（太さ = 谷の深さ）
  let depth = clampF 0 30 (12 * x * x + 2 * a)
      wid = 2 + depth * 0.5
      stroke = case wPrev w0 of
        Just p | wPen w0 && p /= m && dist2 p m < 150 ->
          [ Color (inkColor pal x 70) (Pictures (thickLine wid p m : [at m (disc (wid / 2)) | wid >= 4])) ]
          ++ [ Color (inkColor pal x 40) (at m (disc 20)) | jumped ]
        _ -> []
      art = foldl (|>) (wArt w0) stroke
      art' = Seq.drop (Seq.length art - maxStrokes) art
      moveSpark (Spark sx sy vx vy l c) = Spark (sx + vx) (sy + vy) vx (vy + 0.1) (l - 1) c
      sparks = [ s | s@(Spark _ _ _ _ l _) <- map moveSpark (wSparks w0) ++ new, l > 0 ]
  -- 連続音の代わりに短い音を続けて鳴らす
  when (wAudio w0 && wPen w0 && envFrame env `mod` toneEvery == 0)
    (playTone (clampF 40 3000 (220 * 2 ** x)) 0.08 0.2 Sine)
  pure w0 { wA = a, wB = b, wX = x, wArt = art', wSparks = sparks, wPrev = Just m
          , wJumps = if jumped then wJumps w0 + 1 else wJumps w0 }

drawAll :: Env -> World -> Picture
drawAll env w =
  let wd = envW env
      h = envH env
      m = mouseP5 env
      cuspPts = [ (remap (8 * s * s * s) (negate bRange) bRange 0 wd, remap (-6 * s * s) aMax aMin 0 h)
                | i <- [-90 .. 90 :: Int], let s = 0.01 * fromIntegral i ]
  in Pictures
    [ onP5 env (Pictures
        [ Pictures (toList (wArt w))
        , if wCusp w then Color (hsbA 0 0 100 25) (thickPath 2 cuspPts) else Blank
        , Pictures [ Color (withA (l * 0.025) c) (at (sx, sy) (disc 2)) | Spark sx sy _ _ l c <- wSparks w ]
        , Color (inkColor (wPal w) (wX w) 100) (at m (ring 9 2))
        ])
    , txt env 20 36 16 white ((if wAudio w then "Sound on" else "Click for sound") ++ "   Space: pen " ++ (if wPen w then "down" else "up")
                              ++ "   H: cusp line   1-3: colors   S: save   C: clear")
    , txt env 20 60 14 (gray 0.8) ("jumps " ++ show (wJumps w) ++ "   (a, b) = (" ++ nf 2 (wA w) ++ ", " ++ nf 2 (wB w) ++ ")")
    , txt env 20 82 14 (gray 0.6) "Inside the white line there are 2 valleys. Crossing it, color changes at different places each way."
    , txt env 20 104 14 (gray 0.6) "Lower on screen (a < 0): deeper valleys, thicker lines."
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton _) Down _ _ -> pure w { wAudio = True }
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (on w { wPen = not (wPen w) })
  EventKey (SpecialKey _) Down _ _ -> pure (on w)
  EventKey (Char c) Down _ _
    | c `elem` "hH" -> pure (on w { wCusp = not (wCusp w) })
    | c `elem` "123" -> pure (on w { wPal = fromEnum c - 48 })
    | c `elem` "sS" -> saveSvg "catastrophe_brush" env (drawAll env w) >> pure (on w)
    | c `elem` "cC" -> pure (on w { wArt = Seq.empty, wJumps = 0 })
    | otherwise -> pure (on w)
  _ -> pure w
  where on v = v { wAudio = True }

-- ---------- やってみよう ----------
-- 1. カスプの内側で左右にジグザグに描こう。色の境目が「行き」と「帰り」でずれるのが見える？
-- 2. inkColor を変えて、自分の色の組（palette 4）をつくろう
-- 3. 線の太さを「谷の深さ」でなく「x の速さ」にすると、とんだ所だけ太くなる
-- 4. マウスの代わりに円や 8 の字を自動で描かせて、模様をつくろう（a, b を sin・cos で）


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
