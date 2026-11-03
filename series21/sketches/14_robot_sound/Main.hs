-- ============================================
-- 14 ロボットの音
-- アームの動きを音にする：関節の速さ → モーターの音の高さと大きさ、手先が壁にぶつかる → ノイズ風の音
--   2 リンクの IK（01）でマウスを追いかける。自由制作のベース：音の決め方・アームの形・障害物を変えよう
-- クリックで音をスタート / マウス = 目標 / A = 自動で動く / 1〜3 = モーターの音色 / E = ひじの向き / C = 障害物を消す / Shift + クリック = 障害物
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, replicateM, when)
import Data.Char (toLower)
import Text.Printf (printf)

l1, l2, smoothRate :: Float
l1 = 170                  -- ★ リンクの長さ
l2 = 130
smoothRate = 0.12         -- ★ 動きのなめらかさ（小さいほど重いモーター）

motorEvery :: Int
motorEvery = 6            -- ★ モーター音を何フレームごとに鳴らすか（連続音の代わりに短い音を並べる）

data Wall = Wall !Float !Float !Float         -- x, y, 半径
data Spark = Spark !Float !Float !Float !Float !Int   -- x, y, vx, vy, 残り

data World = World
  { wQ1     :: !Float
  , wQ2     :: !Float
  , wW1     :: !Float     -- 関節の角速度
  , wW2     :: !Float
  , wUp     :: !Bool
  , wAuto   :: !Bool
  , wWave   :: !Wave      -- ★ モーターの波形
  , wAudio  :: !Bool      -- 音をスタートしたか
  , wWalls  :: ![Wall]
  , wCool   :: !Int       -- 衝突音の待ち時間
  , wSparks :: ![Spark]
  , wTarget :: !Point
  }

main :: IO ()
main = runSketch (sketch "21-14 Robot Sound" world0)
  { skBack  = hsb 215 35 12
  , skStep  = \env _ w -> step env w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = World 0.8 0.9 0 0 True False Sawtooth False
  [Wall (width * 0.62) (height * 0.3) 40, Wall (width * 0.25) (height * 0.25) 35] 0 [] (0, 0)

basePos :: Point
basePos = (width * 0.42, height * 0.65)

inverse :: Point -> Bool -> (Float, Float)
inverse (tx, ty) up =
  let (bx, by) = basePos
      x = tx - bx
      y = by - ty
      r = clampF (abs (l1 - l2) + 0.01) (l1 + l2 - 0.01) (sqrt (x * x + y * y))
      c2 = clampF (-1) 1 ((r * r - l1 * l1 - l2 * l2) / (2 * l1 * l2))
      a2 = acos c2 * (if up then 1 else -1)
  in (atan2 y x - atan2 (l2 * sin a2) (l1 + l2 * cos a2), a2)

forward :: Float -> Float -> (Point, Point)
forward a1 a2 =
  let (bx, by) = basePos
      ex = bx + l1 * cos a1
      ey = by - l1 * sin a1
  in ((ex, ey), (ex + l2 * cos (a1 + a2), ey - l2 * sin (a1 + a2)))

wrapAng :: Float -> Float
wrapAng a = atan2 (sin a) (cos a)

approach :: Float -> Float -> Float
approach cur goal = cur + wrapAng (goal - cur) * smoothRate

step :: Env -> World -> IO World
step env w0 = do
  let (bx, by) = basePos
      t = fromIntegral (envFrame env) * 0.015
      target | wAuto w0 = (bx + 230 * cos t + 50 * cos (4 * t), by - 140 - 110 * sin (2 * t))
             | otherwise = toP5 (envMouse env)
      (a1, a2) = inverse target (wUp w0)
      q1 = approach (wQ1 w0) a1
      q2 = approach (wQ2 w0) a2
      om1 = wrapAng (q1 - wQ1 w0)     -- 関節の角速度
      om2 = wrapAng (q2 - wQ2 w0)
      (_, hand) = forward q1 q2
      w1 = w0 { wQ1 = q1, wQ2 = q2, wW1 = om1, wW2 = om2, wTarget = target
              , wCool = max 0 (wCool w0 - 1) }
  -- ★ 関節の速さ → モーター音（速いほど高く・大きく）
  when (envFrame env `mod` motorEvery == 0) $ do
    setMotor w1 (60 + abs om1 * 2500) (min 0.25 (abs om1 * 6))
    setMotor w1 (90 + abs om2 * 2500) (min 0.25 (abs om2 * 6))
  -- ★ 手先が障害物にぶつかったらノイズ
  w2 <- foldM (hitWall hand) w1 (wWalls w1)
  -- 火花
  pure w2 { wSparks = [ Spark (x + vx) (y + vy) vx vy (l - 1) | Spark x y vx vy l <- wSparks w2, l > 1 ] }

hitWall :: Point -> World -> Wall -> IO World
hitWall hand w (Wall x y r)
  | dist2 hand (x, y) < r + 8 && wCool w == 0 = do
      noiseBurst w (0.4 + min 0.6 ((abs (wW1 w) + abs (wW2 w)) * 10))
      new <- replicateM 12 $ do
        vx <- randomRIO (-4, 4)
        vy <- randomRIO (-4, 4)
        pure (Spark (fst hand) (snd hand) vx vy 20)
      pure w { wCool = 12, wSparks = wSparks w ++ new }
  | otherwise = pure w

drawAll :: World -> Picture
drawAll w =
  let (bx, by) = basePos
      b = basePos
      (e, h) = forward (wQ1 w) (wQ2 w)
      om1 = abs (wW1 w)
      om2 = abs (wW2 w)
      waveName = case wWave w of
        Sawtooth -> "sawtooth"
        Square -> "square"
        Triangle -> "triangle"
        Sine -> "sine"
      sparkPic (Spark x y _ _ l) = Color (hsbA 45 90 100 (fromIntegral l * 5)) (at (x, y) (disc 2.5))
  in Pictures
  [ view
      ( [ Color (hsb 0 60 70) (Pictures [at (x, y) (disc r) | Wall x y r <- wWalls w])
        , Color (hsb 200 60 95) (thickLine 14 b e)
        , Color (hsb 320 60 95) (thickLine 14 e h)
        , Color (hsb 0 0 100) (Pictures [at b (disc 11), at e (disc 9)])
        , Color (hsb 45 90 100) (at h (disc 8))
        , Color (hsb 0 0 40) (boxAt (bx - 30) by 60 14)
        -- 速さの輪（モーターの音を目で見る）
        , Color (hsbA 200 60 95 70) (at b (ring (15 + om1 * 300) 3))
        , Color (hsbA 320 60 95 70) (at e (ring (13 + om2 * 300) 3)) ]
        ++ map sparkPic (wSparks w)
        ++ [ Color (hsb 120 90 100) (at (wTarget w) (ring 10 2)) ] )
  , txt 20 34 16 (hsb 0 0 100) ((if wAudio w then "Sound on" else "Click to start sound") ++ "   Mouse: target   A: auto " ++ onOff (wAuto w)
                                ++ "   1-3: tone (" ++ waveName ++ ")   E: elbow")
  , txt 20 58 14 (hsb 0 0 100) ("Shift+click: obstacle   C: clear   |w1| = " ++ nf 2 (om1 * 60) ++ "  |w2| = " ++ nf 2 (om2 * 60) ++ " rad/s")
  , txt 20 78 14 (hsb 0 0 60) "Each joint has a motor sound (faster is higher). The hand hitting a red obstacle makes noise. Try other smoothRate values."
  ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down mods pos ->
    let (mx, my) = toP5 pos
        walls | shift mods == Down && my > 90 = wWalls w ++ [Wall mx my 30]
              | otherwise = wWalls w
    in w { wAudio = True, wWalls = walls }
  EventKey (Char c) Down _ _ -> case toLower c of
    'a' -> w { wAuto = not (wAuto w) }
    'e' -> w { wUp = not (wUp w) }
    '1' -> w { wWave = Sawtooth }
    '2' -> w { wWave = Square }
    '3' -> w { wWave = Triangle }
    'c' -> w { wWalls = [] }
    _ -> w
  _ -> w

-- ---------- やってみよう ----------
-- 1. 音の高さを「関節の角度」で決める（速さでなく）と、腕の形がメロディになる。どちらが「ロボットらしい」？
-- 2. 障害物に当たったときノイズではなく和音を鳴らす（playChord を使う）
-- 3. 腕を 3 リンク（03）にしてモーターを 3 つに。音が増えると何に聞こえる？
-- 4. 自動モードの軌道（cos, sin の係数）を変えてリズムをつくる。作品のベースに


-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- マウスの位置（gloss の座標：中央が原点、y は上向き）を p5 と同じ座標（左上が原点、y は下向き）に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は p5 の text と同じ左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 左上の角が (x, y) の長方形（p5 の rect）
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y bw bh = rect (x + bw / 2) (y + bh / 2) bw bh

-- 透明度を 0〜100 で書く HSB（p5 の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 小数点以下 d 桁の文字列（p5 の nf）
nf :: Int -> Float -> String
nf d = printf ("%." ++ show d ++ "f")

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- ここから下は「音を鳴らすための道具」（モーター音・ノイズ）

-- モーターの短い音を 1 つ鳴らす。周波数と音量を丸めて、作る音のファイルの数を抑える
setMotor :: World -> Float -> Float -> IO ()
setMotor w freq vol =
  when (wAudio w && v > 0.01) $
    playTone (10 * fromIntegral (round (clampF 30 2000 freq / 10) :: Int)) v (fromIntegral motorEvery / 60 * 1.3) (wWave w)
  where v = fromIntegral (round (clampF 0 0.3 vol * 20) :: Int) / 20

-- 短いノイズ風の音（衝突音）：周波数がそろわない 4 つの音を同時に鳴らす
noiseBurst :: World -> Float -> IO ()
noiseBurst w vol =
  when (wAudio w) $
    playChord [431, 977, 1693, 2719] (fromIntegral (round (clampF 0 1 vol * 4) :: Int) / 16) 0.15 Sawtooth
