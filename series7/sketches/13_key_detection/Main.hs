-- ============================================
-- 13 キー（調）の推定
-- 曲の中でよく鳴る 12 音の割合（クロマの累積）を、24 の調の「お手本の形」と比べて、
-- いちばん似ている調を答える（クルムハンスル＝シュムクラーのアルゴリズム）
-- クリックでマイク ON（マイクが使えないときはデモの信号で動く）→ 曲を流す・歌う / R でリセット
-- ============================================
module Main (main) where

import Art
import Data.List (sortOn)
import qualified Data.Vector.Unboxed as U
import Text.Printf (printf)

data World = World
  { wMic    :: !(Maybe Mic)        -- まだ ON にしていなければ Nothing
  , wLive   :: !Bool               -- 本物のマイクの音が来ているか
  , wSpec   :: !(U.Vector Float)   -- なめらかにしたスペクトル
  , wAccum  :: !(U.Vector Float)   -- 12 音の累積
  , wScores :: ![(String, Float)]  -- 調と似ている度（よく似ている順）
  }

noteNames :: [String]
noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

-- ★ 長調・短調の「音の重み」お手本（心理実験から求められた値）
majorProfile, minorProfile :: [Float]
majorProfile = [6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88]
minorProfile = [6.33, 2.68, 3.52, 5.38, 2.6, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17]

binHz, smoothing :: Float
binHz = 22050 / 2 / 256    -- micSpectrum の 1 個ぶんの周波数
smoothing = 0.5            -- ★ スペクトルをなめらかにする割合

main :: IO ()
main = runSketch (sketch "07-13 Key Detection" (World Nothing False (U.replicate 256 0) (U.replicate 12 0) []))
  { skBack  = hsb 260 40 10
  , skStep  = \_ _ w -> stepWorld w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 相関係数（形がどれだけ似ているか、-1〜1）
correlation :: [Float] -> [Float] -> Float
correlation a b =
  let ma = sum a / 12
      mb = sum b / 12
      num = sum (zipWith (\x y -> (x - ma) * (y - mb)) a b)
      da = sum (map (\x -> (x - ma) * (x - ma)) a)
      db = sum (map (\y -> (y - mb) * (y - mb)) b)
  in num / sqrt (da * db + 1e-9)

-- お手本を root だけ回転させる
rotated :: [Float] -> Int -> [Float]
rotated prof root = [prof !! ((i - root) `mod` 12) | i <- [0 .. 11]]

-- ★ 24 の調それぞれと比べる：お手本を回転させて相関をとる
scoreAll :: [Float] -> [(String, Float)]
scoreAll accum = sortOn (negate . snd) (concat
  [ [ (name ++ " major", correlation accum (rotated majorProfile root))
    , (name ++ " minor", correlation accum (rotated minorProfile root)) ]
  | (root, name) <- zip [0 ..] noteNames ])

stepWorld :: World -> IO World
stepWorld w = case wMic w of
  Nothing -> pure w
  Just mic -> do
    live <- micLive mic
    raw <- micSpectrum mic
    let spec = U.zipWith (\o v -> smoothing * o + (1 - smoothing) * v) (wSpec w) (U.fromListN 256 (raw ++ repeat 0))
        add = [ (pc, b * b * 0.01)                  -- ★ 時間とともに積み上げる
              | (i, v) <- drop 2 (zip [0 :: Int ..] (U.toList spec))
              , let f = fromIntegral i * binHz
              , f >= 80 && f <= 2000
              , let pc = (round (69 + 12 * logBase 2 (f / 440)) :: Int) `mod` 12
                    b = toByte v / 255 ]
        accum = U.accum (+) (wAccum w) add
    pure w { wLive = live, wSpec = spec, wAccum = accum, wScores = scoreAll (U.toList accum) }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (bars ++ keys ++ hud)
  where
    wd = envW env
    ht = envH env
    -- ---- 左：累積クロマ ----
    top = U.maximum (wAccum w)
    m = if top > 0 then top else 1
    bx = 60
    bw = (wd * 0.5 - 80) / 12
    base = ht * 0.75
    bh = ht * 0.4
    bars = concat
      [ [ Color (hsb (fromIntegral (i * 30)) 70 90) (box env (bx + fi * bw) (base - h) (bw - 4) h)
        , txtC env (bx + fi * bw + bw / 2) (base + 20) 14 (hsb 0 0 70) name
        ]
      | (i, name) <- zip [0 :: Int ..] noteNames
      , let fi = fromIntegral i
            h = wAccum w U.! i / m * bh ]
      ++ [txt env bx (base - bh - 14) 14 (hsb 0 0 60) "How much each note sounded overall (accumulated chroma)"]
    -- ---- 右：上位の調 ----
    rx = wd * 0.58
    ry = ht * 0.35
    keys = case wScores w of
      [] -> []
      ((name, v) : rest) ->
        [ txt env rx ry 48 white name
        , txt env rx (ry + 36) 16 (hsb 0 0 60) ("Similarity " ++ printf "%.2f" v)
        ]
        ++ [ txt env rx (ry + 50 + fi * 34) 20 (hsb 0 0 (60 - fi * 6))
               (show (i + 1) ++ ". " ++ n ++ "  " ++ printf "%.2f" s)
           | (i, (n, s)) <- zip [1 :: Int .. 5] rest
           , let fi = fromIntegral i ]
    hud =
      [ txt env 20 40 18 white (case wMic w of
          Nothing -> "Click to turn the mic ON"
          Just _  -> "Play a song or sing: the key slowly settles.   R: reset")
      , txt env 20 68 15 (hsb 0 0 60) "Compare with the typical note shape of each of 24 keys and pick the closest, as music apps do."
      , case wMic w of
          Just _ | not (wLive w) -> txt env 20 94 15 (hsb 0 0 70) "No mic found: demo signal"
          _ -> Blank
      ]

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ | Nothing <- wMic w -> do
    mic <- startMic
    pure w { wMic = Just mic }
  EventKey (Char c) Down _ _ | c == 'r' || c == 'R' -> pure w { wAccum = U.replicate 12 0 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 「きらきら星」をハ長調（ドから）で歌う → C major と出るか
-- 2. 同じ曲を違う高さで歌う → 別の調が出る（移調）
-- 3. 長調と短調で迷うのはなぜ？（C major と A minor は同じ 7 音を使う：平行調）

-- ---------- 道具 ----------

-- 強さ（0〜1）を、p5.js の getByteFrequencyData と同じ -100〜-30 dB の目盛りで 0〜255 にする
toByte :: Float -> Float
toByte v =
  let magnitude = v * 40 / 512
      db = 20 * logBase 10 (max 1e-6 magnitude)
  in 255 * clampF 0 1 ((db + 100) / 70)

-- p5.js の座標（左上が原点、y は下向き）を gloss の座標に変える
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の長方形（p5.js の rect）
box :: Env -> Float -> Float -> Float -> Float -> Picture
box env x y w h = let (gx, gy) = toG env (x + w / 2, y + h / 2) in rect gx gy w h

-- 文字（左端と文字の下端を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str

-- 文字（中央揃え。y は文字の下端）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = let (gx, gy) = toG env (x, y) in labelC gx (gy + s / 2) s c str
