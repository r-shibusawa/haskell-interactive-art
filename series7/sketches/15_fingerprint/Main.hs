-- ============================================
-- 15 音の指紋（おまけ）── Shazam のしくみ
-- スペクトログラムの「とくに強い点（ピーク）」だけを取り出すと、曲ごとにちがう星座になる
-- ピークのペア（周波数①・周波数②・時間差）をハッシュにして、データベースと照合するのが音楽検索
-- クリックでマイク ON（マイクが使えないときはデモの信号で動く）
-- スペースで星座を保存 → もう一度流すと「一致したペア」を数える
-- ============================================
-- gloss は毎フレーム描きなおすので、スペクトログラムは 320 × 160 のバイト列に覚えておき、4 倍にして表示する。
module Main (main) where

import Art
import Data.List (foldl', tails)
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)

data Peak = Peak { pkT :: !Int, pkF :: !Int }    -- t = 列番号、f = 周波数ビン

-- 一致したペアをつなぐ線（列、ビン、列、ビン）
data Link = Link !Int !Int !Int !Int

type Hash = (Int, Int, Int)

data World = World
  { wMic     :: !(Maybe Mic)        -- まだ ON にしていなければ Nothing
  , wLive    :: !Bool               -- 本物のマイクの音が来ているか
  , wSpec    :: !(U.Vector Float)   -- なめらかにしたスペクトル
  , wLayer   :: !(U.Vector Word8)   -- スペクトログラム（0 = 背景、255 = ピーク）
  , wCol     :: !Int                -- いま書いている列
  , wPeaks   :: ![Peak]             -- 新しいものが先頭、最大 400
  , wLinks   :: ![Link]             -- 緑の線（最大 400）
  , wHashes  :: !(S.Set Hash)       -- 保存した星座
  , wMatches :: !Int
  }

cols, maxBin :: Int
cols = 320                 -- 列の数
maxBin = 160               -- 使う周波数ビンの数

cellPx :: Float
cellPx = 4                 -- 1 マスの大きさ（ピクセル）

threshold :: Float
threshold = 120            -- ★ ピークのしきい値（0〜255）

smoothing :: Float
smoothing = 0.3            -- ★ スペクトルをなめらかにする割合

main :: IO ()
main = runSketch (sketch "07-15 Audio Fingerprint"
  (World Nothing False (U.replicate 256 0) (U.replicate (cols * maxBin) 0) 0 [] [] S.empty 0))
  { skBack  = hsb 0 0 5
  , skStep  = \_ _ w -> stepWorld w
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

stepWorld :: World -> IO World
stepWorld w = case wMic w of
  Nothing -> pure w
  Just mic -> do
    live <- micLive mic
    raw <- micSpectrum mic
    let spec = U.zipWith (\o v -> smoothing * o + (1 - smoothing) * v) (wSpec w) (U.fromListN 256 (raw ++ repeat 0))
        bytes = U.map toByte spec
        col = wCol w
        -- ★ ピーク検出：まわりより十分大きい周波数ビンだけ残す
        found = filter (isPeak bytes) [3 .. maxBin - 4]
        (peaks, links, matches) = foldl' (addPeak (wHashes w) col) (wPeaks w, wLinks w, wMatches w) found
        next = (col + 1) `mod` cols
        clear1 = (next + 1) `mod` cols      -- カーソルの先を消す
        clear2 = (next + 2) `mod` cols
        touches c (Link a _ b _) = a == c || b == c
        writes =
          [ (cellIndex col i, fromIntegral (min 254 (round (bytes U.! i) :: Int))) | i <- [0 .. maxBin - 1] ]
          ++ [ (cellIndex col i, 255) | i <- found ]
          ++ [ (cellIndex c i, 0) | c <- [clear1, clear2], i <- [0 .. maxBin - 1] ]
    pure w
      { wLive = live
      , wSpec = spec
      , wLayer = wLayer w U.// writes
      , wCol = next
      , wPeaks = if length peaks > 400 then take 300 peaks else peaks
      , wLinks = take 400 (filter (\l -> not (touches clear1 l || touches clear2 l)) links)
      , wMatches = matches
      }

-- (列, ビン) → バイト列の位置（低い音が下）
cellIndex :: Int -> Int -> Int
cellIndex c i = (maxBin - 1 - i) * cols + c

isPeak :: U.Vector Float -> Int -> Bool
isPeak s i =
  let v = s U.! i
  in v >= threshold && and [s U.! (i + k) < v | k <- [-3, -2, -1, 1, 2, 3]]

-- ★ ハッシュ：周波数①、周波数②、時間差
hashOf :: Peak -> Int -> Int -> Hash
hashOf p f t = (pkF p, f, floor (fromIntegral (t - pkT p) / 2 + 0.5 :: Float))

-- ★ 直前のピークとペアにしてハッシュを調べる
addPeak :: S.Set Hash -> Int -> ([Peak], [Link], Int) -> Int -> ([Peak], [Link], Int)
addPeak hashes col (peaks, links, matches) i =
  let recent = take 5 peaks
      hits = filter (\p -> S.member (hashOf p i col) hashes) recent
  in ( Peak col i : peaks
     , map (\p -> Link (pkT p) (pkF p) col i) hits ++ links
     , matches + length hits )

-- 最近のピークのペアをデータベースに
buildHashes :: [Peak] -> S.Set Hash
buildHashes newestFirst = S.fromList
  [ hashOf a (pkF b) (pkT b)
  | (a : rest) <- tails (reverse newestFirst)
  , b <- take 5 rest ]

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures ([image] ++ links ++ hud)
  where
    -- 画像の中心（p5.js の座標で上から 110 のところから描く）
    imgH = fromIntegral maxBin * cellPx
    (_, imgY) = toG env (envW env / 2, 110 + imgH / 2)
    layer = wLayer w
    image = at (0, imgY) (pixelImage cols maxBin cellPx (\x y -> shade (layer U.! (y * cols + x))))
    -- マスの中心の座標
    pos :: Int -> Int -> Point
    pos c i = ( (fromIntegral c + 0.5 - fromIntegral cols / 2) * cellPx
              , imgY + (fromIntegral i + 0.5 - fromIntegral maxBin / 2) * cellPx )
    links = [ Color (hsb 120 90 100) (thickLine 2 (pos a b) (pos c d)) | Link a b c d <- wLinks w ]
    hud =
      [ txt env 20 40 18 white (case wMic w of
          Nothing -> "Click to turn the mic ON"
          Just _  -> "Yellow peaks are the fingerprint. Space: save (" ++ show (S.size (wHashes w)) ++ " pairs). Green lines = saved match")
      , txt env 20 68 15 (hsb 0 0 60) ("Matches " ++ show (wMatches w) ++ ". Play the same part of the same song and the matches increase.")
      , txt env 20 90 15 (hsb 0 0 60) (case wMic w of
          Just _ | not (wLive w) -> "No mic found: demo signal."
          _ -> "Peaks resist noise and volume changes, so even a phone recording can be matched.")
      ]

-- バイト列の値を色にする
shade :: Word8 -> (Word8, Word8, Word8)
shade v
  | v == 0 = (13, 13, 13)                      -- 背景
  | v == 255 = (255, 208, 26)                  -- ピーク（黄）
  | otherwise =
      let i = fromIntegral v :: Int
      in (shadeR U.! i, shadeG U.! i, shadeB U.! i)

-- 色の表（hsb 230 50 (v × 60)）
shadeR, shadeG, shadeB :: U.Vector Word8
shadeR = shadeTable (\(r, _, _) -> r)
shadeG = shadeTable (\(_, g, _) -> g)
shadeB = shadeTable (\(_, _, b) -> b)

shadeTable :: ((Float, Float, Float) -> Float) -> U.Vector Word8
shadeTable pick = U.generate 256 $ \i ->
  let (r, g, b, _) = rgbaOfColor (hsb 230 50 (fromIntegral i / 255 * 60))
  in fromIntegral (round (255 * pick (r, g, b)) :: Int)

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ | Nothing <- wMic w -> do
    mic <- startMic
    pure w { wMic = Just mic }
  EventKey (SpecialKey KeySpace) Down _ _ ->
    pure w { wHashes = buildHashes (wPeaks w), wMatches = 0 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 曲のサビを流してスペース → 同じサビをもう一度流すと緑の線が増える。別の曲だと？
-- 2. しきい値 threshold の 120 を変えると、ピークの数が変わる（多すぎると偶然の一致が増える）
-- 3. ペアにする「時間差」を含めるのはなぜ？（周波数だけだと別の曲でも一致してしまう）

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

-- 文字（左端と文字の下端を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str
