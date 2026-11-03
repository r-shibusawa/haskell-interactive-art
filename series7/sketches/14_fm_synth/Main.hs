-- ============================================
-- 14 FM 音源（おまけ）
-- 1 つの波（モジュレータ）で別の波（キャリア）の周波数を揺らすと、倍音がどっと増える
-- 周波数の比が整数なら楽器らしい音、非整数なら金属的な音。1980 年代のシンセ（DX7）の方式
-- マウス左右 = 周波数の比 / 上下 = 変調の深さ / クリックで鳴らす / 1〜3 でお手本
-- ============================================
-- Art.Sound は決まった波形しか鳴らせない。FM の音は「キャリア ± n × モジュレータ」のサイン波の和で
-- その大きさはベッセル関数 J_n で決まるので、強いものから順にサイン波を同時に鳴らして FM の音を作る。
module Main (main) where

import Art
import Control.Monad (forM_)
import Data.List (sortOn)
import Text.Printf (printf)

-- 鳴らしている音
data Voice = Voice
  { vStart :: !Float, vF :: !Float, vR :: !Float, vIdx :: !Float, vDur :: !Float }

data World = World
  { wPhase :: !Float
  , wVoice :: !(Maybe Voice)   -- 最後に鳴らした音（一度も鳴らしていなければ Nothing）
  }

maxPartials :: Int
maxPartials = 7            -- ★ 同時に鳴らすサイン波の数（Art.Sound は同時に 8 音まで）

main :: IO ()
main = runSketch (sketch "07-14 FM Synth" (World 0 Nothing))
  { skBack  = hsb 260 40 10
  , skStep  = \_ _ w -> pure w { wPhase = wPhase w + 0.02 }
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 周波数の比：マウスの左右で 0.5〜8（0.5 きざみ）
ratioOf :: Env -> Float
ratioOf env =
  let v = clampF 0.5 8 (remap (fst (fromG env (envMouse env))) 0 (envW env) 0.5 8)
  in fromIntegral (floor (v * 2 + 0.5) :: Int) / 2

-- 変調の深さ：マウスの上下で 10〜0
indexOf :: Env -> Float
indexOf env = clampF 0 10 (remap (snd (fromG env (envMouse env))) 0 (envH env) 10 0)

-- ベッセル関数 J_n(x)：cos(nτ - x sin τ) の 1 周の平均
bessel :: Int -> Float -> Float
bessel n x = sum [cos (fromIntegral n * t - x * sin t) | i <- [0 .. 63 :: Int], let t = tau * fromIntegral i / 64] / 64

-- 側波帯の番号
sidebands :: [Int]
sidebands = [-10 .. 10]

-- 時間とともに変調が浅くなる（ベルらしさ）。t 秒後の変調指数
betaAt :: Voice -> Float -> Float
betaAt v t =
  let b0 = vIdx v * 0.5
      b1 = 1 / (vF v * vR v)
  in if b0 > b1 then b0 * (b1 / b0) ** (clampF 0 1 (t / vDur v)) else b0

-- ★ FM の音を鳴らす：強い側波帯から順にサイン波を鳴らす。高い側波帯ほど早く消える（変調が浅くなるため）
playFM :: Float -> Float -> Float -> Float -> IO ()
playFM f r idx dur = do
  let b0 = idx * 0.5
      b1 = 1 / (f * r)
      lastFor n
        | n == 0 || b0 <= b1 = dur
        | otherwise = clampF 0.15 dur (dur * log (b0 / (0.5 * fromIntegral (abs n))) / log (b0 / b1))
      parts = [ (abs (f + fromIntegral n * f * r), a, lastFor n)
              | n <- sidebands
              , let a = abs (bessel n b0)
              , a > 0.01 ]
      strongest = take maxPartials (sortOn (\(_, a, _) -> negate a) parts)
  forM_ strongest (\(hz, a, d) -> if hz < 20 then pure () else playTone hz (0.4 * a) d Sine)

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures ([axis, waveLine] ++ spectrum ++ hud)
  where
    wd = envW env
    ht = envH env
    left = 60
    right = wd - 60
    mid = ht * 0.45
    h = ht * 0.2
    ratio = ratioOf env
    index = indexOf env
    -- ---- 波形 ----
    axis = Color (hsb 0 0 25) (Line [toG env (left, mid), toG env (right, mid)])
    waveLine = Color (hsb 45 90 100) (thickPath 2.5
      [ toG env (x, mid - v * h)
      | x <- [left .. right]
      , let t = (x - left) / (right - left) * tau * 3
            -- ★ FM の式：キャリアの位相に、モジュレータを足す
            v = sin (t + index * sin (ratio * t + wPhase w)) ])
    -- ---- スペクトル（式から計算）----
    base = ht - 70
    sh = ht * 0.22
    xOfHz f = remap f 0 5000 left right
    spectrum = case wVoice w of
      Nothing -> []
      Just v ->
        let age = envTime env - vStart v
            b = betaAt v age
            gain = 0.4 * (0.0001 / 0.4) ** (age / vDur v)
            stems =
              [ Color (hsb 170 80 100) (thickLine 2 (toG env (xOfHz hz, base)) (toG env (xOfHz hz, base - bar * sh)))
              | age < vDur v
              , n <- sidebands
              , let hz = abs (vF v + fromIntegral n * vF v * vR v)
              , hz < 5000
              , let a = gain * abs (bessel n b)
                    db = 20 * logBase 10 (max 1e-6 a)
                    bar = clampF 0 1 ((db + 60) / 60) ]
        in Color (hsb 0 0 25) (Line [toG env (left, base), toG env (right, base)])
           : txt env left (base - sh - 10) 14 (hsb 0 0 60) "Spectrum, computed (0-5000 Hz): partials sit at carrier +- n x modulator"
           : stems
    hud =
      [ txt env 20 40 18 white
          ("Ratio " ++ printf "%.1f" ratio ++ " (mouse X)   Depth " ++ printf "%.1f" index
           ++ " (mouse Y)   Click: play   1: bell   2: e-piano   3: brass")
      , txt env 20 68 15 (hsb 0 0 60) "y = sin(2 pi f t + I x sin(2 pi ratio f t))"
      , txt env 20 90 15 (hsb 0 0 60) "Integer ratio (1, 2, 3) -> harmonic sound. Non-integer ratio (1.4, 3.5) -> metallic, bell-like sound."
      ]

-- 音を鳴らして、スペクトルの表示のために覚えておく
start :: Env -> Float -> Float -> Float -> Float -> World -> IO World
start env f r idx dur w = do
  playFM f r idx dur
  pure w { wVoice = Just (Voice (envTime env) f r idx dur) }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> start env 220 (ratioOf env) (indexOf env) 1.5 w
  EventKey (Char '1') Down _ _ -> start env 440 3.5 6 2.5 w
  EventKey (Char '2') Down _ _ -> start env 220 1 2 1.5 w
  EventKey (Char '3') Down _ _ -> start env 165 1 5 1.0 w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 比を 1 → 2 → 3 と整数で変えると倍音の間隔が変わる。1.5 や 2.5 にすると不協和な響き
-- 2. 深さ 0 は純粋なサイン波。上げるほど倍音が増えて明るく・ざらざらに
-- 3. maxPartials を 1 や 3 にすると、音色はどう変わる？

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- gloss の座標を p5.js の座標に変える
fromG :: Env -> Point -> Point
fromG env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 文字（左端と文字の下端を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str
