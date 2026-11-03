-- ============================================
-- 15 周波数ホッピング（おまけ）
-- 送信機が決まった順番で周波数を飛び回る。順番を知っている受信機だけが追いかけられる
-- 妨害電波（マウス）が 1 つの周波数を潰しても、ほとんどのデータは通る
-- マウス上下 = 妨害する周波数 / 1 = ホッピングなし / 2 = ホッピングあり
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

data Hop = Hop !Int !Bool     -- 使った周波数と、届いたか

data World = World
  { wHopping  :: !Bool
  , wTimeline :: ![Hop]       -- 古い順
  , wSent     :: !Int
  , wLost     :: !Int
  }

channels, seed, historyLen, framesPerHop :: Int
channels = 16                 -- ★ 周波数の数
seed = 7                      -- ★ 順番を決める種
historyLen = 60               -- 画面に残す数
framesPerHop = 8              -- ★ 何フレームごとに送るか

main :: IO ()
main = runSketch (sketch "06-15 Frequency Hopping" (World True [] 0 0))
  { skBack  = hsb 230 40 10
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = drawWorld
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- ★ ホッピング列：送信機と受信機が共有する「秘密の順番」（擬似乱数）
hopSequence :: Bool -> Int -> Int
hopSequence False _ = 5                                -- ホッピングなし：いつも同じ周波数
hopSequence True t = (t * 7 + seed) `mod` channels     -- 単純な擬似乱数

-- マウス上下で妨害する周波数を決める
jamOf :: Env -> Int
jamOf env =
  let my = snd (mouseXY env)
      top = 100
      bottom = envH env - 80
      top' = fromIntegral (channels - 1)
  in floor (clampF 0 top' (remap my top bottom top' 0))

step :: Env -> World -> World
step env w
  | envFrame env `mod` framesPerHop /= 0 = w
  | otherwise =
      let t = envFrame env `div` framesPerHop
          ch = hopSequence (wHopping w) t
          ok = ch /= jamOf env                 -- 妨害されている周波数なら失敗
          line' = wTimeline w ++ [Hop ch ok]
      in w { wTimeline = drop (length line' - historyLen) line'
           , wSent = wSent w + 1
           , wLost = wLost w + (if ok then 0 else 1) }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ -- ---- 周波数（縦）× 時間（横）----
    Pictures (map channelRow [0 .. channels - 1])
    -- パケット
  , Pictures (zipWith packet [0 ..] (wTimeline w))
    -- 妨害電波
  , roundBox env (right - 215) (jamY - 18) 205 22 3 (hsba 230 40 10 0.85)
  , txt env (right - 210) jamY 14 (hsb 0 90 100) "Jammer (mouse Y)"
  , txt env 20 40 18 (hsb 0 0 100)
      ((if wHopping w then "Hopping ON" else "No hopping (single frequency)")
       ++ "   1/2: switch   Sent " ++ show (wSent w) ++ "   Jammed " ++ show (wLost w)
       ++ " (" ++ nf 0 (100 * fromIntegral (wLost w) / fromIntegral (max 1 (wSent w))) ++ " %)")
  , txt env 20 68 15 (hsb 0 0 60) "Bluetooth hops 1600 times a second over 79 frequencies, dodging Wi-Fi and microwave ovens"
  ]
  where
    left = 100
    right = envW env - 60
    top = 100
    bottom = envH env - 80
    rowH = (bottom - top) / fromIntegral channels
    colW = (right - left) / fromIntegral historyLen
    jam = jamOf env
    jamY = bottom - fromIntegral (jam + 1) * rowH - 2
    channelRow c =
      let y = bottom - fromIntegral (c + 1) * rowH
          col = if c == jam then hsb 0 80 35 else hsb 230 30 14
          (gx, gy) = toG env ((left + right) / 2) (y + (rowH - 2) / 2)
      in Pictures
           [ Color col (rect gx gy (right - left) (rowH - 2))
           , txt env (left - 70) (y + rowH / 2 + 5) 14 (hsb 0 0 60) ("ch " ++ show c) ]
    packet :: Float -> Hop -> Picture
    packet i (Hop ch ok) =
      let x = left + i * colW
          y = bottom - fromIntegral (ch + 1) * rowH
      in roundBox env (x + 1) (y + 2) (colW - 2) (rowH - 6) 3 (if ok then hsb 120 80 100 else hsb 0 80 100)

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (Char '1') Down _ _ -> World False [] 0 0
  EventKey (Char '2') Down _ _ -> World True [] 0 0
  _ -> w

-- ---------- やってみよう ----------
-- 1. ホッピングなし（1）で、マウスを ch 5 に合わせる → 全部失敗。ホッピングあり（2）なら 1/16 だけ
-- 2. 妨害を 2 つにすると？（jamOf をもう 1 つ作る）
-- 3. hopSequence を自分の順番にして、「受信機が知らない順番」だと受け取れないことを確認

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左下が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx gy size c s

-- 角の丸い長方形（左上が x, y、角の半径 r）
roundBox :: Env -> Float -> Float -> Float -> Float -> Float -> Color -> Picture
roundBox env x y w h r c = Color c (Polygon (concat
  [ corner (x + w - r) (y + r) (-90), corner (x + w - r) (y + h - r) 0
  , corner (x + r) (y + h - r) 90, corner (x + r) (y + r) 180 ]))
  where
    corner cx cy a0 = [ toG env (cx + r * cos a) (cy + r * sin a)
                      | i <- [0 .. 4 :: Int], let a = (a0 + 22.5 * fromIntegral i) * pi / 180 ]

-- 小数を d 桁で文字にする（p5.js の nf）
nf :: Int -> Float -> String
nf d v = showFFloat (Just d) v ""
