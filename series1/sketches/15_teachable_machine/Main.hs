-- ============================================
-- 15 自分で学習させる画像分類（Teachable Machine の代わり）
-- カメラに見せて覚えさせたクラスを当てて、映像と音が変わる
--
-- 【使い方】
-- 1. 見せたいもの（グー など）をカメラに映して、1 キーを押し続ける → クラス 1 に学習
-- 2. 別のもの（パー など）を映して 2 キーを押し続ける → クラス 2 に学習（3, 4 キーも使える）
-- 3. カメラに映すと、いちばん近いクラスに切りかわる
-- C キーで学習をやり直す（カメラが使えないときはデモ映像）
-- ============================================
-- p5.js 版は Web 上の Teachable Machine のモデルを読みこむ。Haskell では読みこめないので、
-- 「クラスごとの平均の画像にいちばん近いものを選ぶ」小さな分類器をこの中で学習する。
module Main (main) where

import Art
import Control.Monad (forM, replicateM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.List (sortOn)
import qualified Data.Vector.Unboxed as U

-- 学習したクラス 1 つ
data Trained = Trained
  { cId    :: !Int               -- 何番のクラスか（0〜3）
  , cCount :: !Int               -- 覚えた画像の枚数
  , cMean  :: !(U.Vector Float)  -- 覚えた画像の平均
  }

data Particle = Particle
  { pX, pY, pVx, pVy, pSize, pLife :: !Float }

data World = World
  { wCam     :: !(Maybe Camera)
  , wFrame   :: !B.ByteString
  , wLive    :: !Bool
  , wClasses :: ![Trained]         -- 学習したクラス（番号順）
  , wResults :: ![(Int, Float)]    -- 分類の結果 (クラス番号, 確率)
  , wLabel   :: !(Maybe Int)       -- 今いちばん確率が高いクラス
  , wParts   :: ![Particle]
  , wBg      :: !(Float, Float, Float)   -- 今の背景の色（0〜1）
  , wTimer   :: !Float             -- 次の分類までの時間
  }

-- カメラ映像の大きさ
camW, camH :: Int
camW = 160
camH = 120

-- クラスごとの色（1 番目, 2 番目, 3 番目, 4 番目）
classColors :: [(Float, Float, Float)]
classColors = [hex 0xff 0x4f 0x8b, hex 0x2f 0xd6 0xc3, hex 0xff 0xcc 0x33, hex 0x7a 0x6c 0xff]  -- ★

-- クラスごとの音
classNotes :: [String]
classNotes = ["C4", "E4", "G4", "B4"]  -- ★

sharpness :: Float
sharpness = 30                  -- ★ 大きいほど確率が 0% か 100% に寄る

initialWorld :: World
initialWorld = World Nothing B.empty False [] [] Nothing [] (dark20, dark20, dark20) 0
  where dark20 = 20 / 255

main :: IO ()
main = runSketch (sketch "01-15 Teach the Camera" initialWorld)
  { skBack  = gray (20 / 255)
  , skStart = \_ -> do
      cam <- startCamera camW camH
      pure initialWorld { wCam = Just cam }
  , skStep  = step
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

step :: Env -> Float -> World -> IO World
step env dt w0 = do
  frame <- maybe (pure B.empty) cameraFrame (wCam w0)
  live <- maybe (pure False) camLive (wCam w0)
  let w1 = w0 { wFrame = frame, wLive = live, wParts = concatMap move (wParts w0)
              , wBg = mixBg (wBg w0) (targetBg w0) }
      timer = wTimer w0 - dt
  if timer > 0 then pure w1 { wTimer = timer } else do
    -- 0.1 秒ごとに、押されているキーのクラスを学習してから分類する
    let held = [ k | k <- [0 .. 3], isDown env (Char (toEnum (fromEnum '1' + k))) ]
        w2 = foldr (learn frame) w1 held
    classifyNow env w2 { wTimer = 0.1 }

-- 背景を、今のクラスの色にゆっくり近づける
targetBg :: World -> (Float, Float, Float)
targetBg w = case wLabel w of
  Just i -> colorOf i
  Nothing -> (20 / 255, 20 / 255, 20 / 255)

mixBg :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
mixBg (a, b, c) (x, y, z) = (lerpF a x 0.08, lerpF b y 0.08, lerpF c z 0.08)

move :: Particle -> [Particle]
move p
  | pLife p - 2 <= 0 = []
  | otherwise = [p { pX = pX p + pVx p, pY = pY p + pVy p, pLife = pLife p - 2 }]

-- 今の映像を、クラス i の平均に足しこむ
learn :: B.ByteString -> Int -> World -> World
learn frame i w
  | B.length frame < camW * camH * 3 = w
  | otherwise = w { wClasses = sortOn cId (updated : others) }
  where
    f = features frame
    others = filter ((/= i) . cId) (wClasses w)
    updated = case filter ((== i) . cId) (wClasses w) of
      (c : _) ->
        let n = fromIntegral (cCount c) :: Float
        in c { cCount = cCount c + 1, cMean = U.zipWith (\m v -> (m * n + v) / (n + 1)) (cMean c) f }
      [] -> Trained i 1 f

-- 分類して、確率が 0.8 を超えたクラスに切りかえる
classifyNow :: Env -> World -> IO World
classifyNow env w
  | length (wClasses w) < 2 = pure w { wResults = [(cId c, 1) | c <- wClasses w] }
  | B.length (wFrame w) < camW * camH * 3 = pure w
  | otherwise = do
      let f = features (wFrame w)
          results = classify f (wClasses w)
          best = foldr (\a b -> if snd a >= snd b then a else b) (-1, 0) results
          w' = w { wResults = results }
      if Just (fst best) /= wLabel w && snd best > 0.8
        then onLabelChange env (fst best) w' { wLabel = Just (fst best) }
        else pure w'

-- 平均の画像との近さから、確率をつくる
classify :: U.Vector Float -> [Trained] -> [(Int, Float)]
classify f cs =
  let dist c = sqrt (U.sum (U.zipWith (\a b -> (a - b) * (a - b)) f (cMean c)) / fromIntegral (U.length f))
      ds = map dist cs
      nearest = foldr min 1e9 ds                 -- いちばん近いものを基準にする
      scores = map (\d -> exp (negate sharpness * (d - nearest))) ds
      total = max 1e-9 (sum scores)
  in zipWith (\c s -> (cId c, s / total)) cs scores

-- ==================================================
-- ★ ここを改造しよう：クラスが変わった瞬間に呼ばれる
-- ==================================================
onLabelChange :: Env -> Int -> World -> IO World
onLabelChange env i w = do
  -- 音を鳴らす（クラスごとにちがう音）
  playNote (concat (take 1 (drop i classNotes))) 0.4 0.8 Triangle
  -- 粒を飛ばす
  new <- replicateM 80 $ do
    a <- randomRIO (0, tau)
    s <- randomRIO (2, 10)
    size <- randomRIO (4, 12)
    pure (Particle 0 (envH env * 0.08) (cos a * s) (sin a * s) size 100)
  pure w { wParts = take 800 (new ++ wParts w) }

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (bg : particles ++ [bigLabel] ++ bars ++ [preview, message, hint])
  where
    ww = envW env
    h = envH env
    (br, bgG, bb) = wBg w
    bg = Color (makeColor br bgG bb 1) (rectangleSolid ww h)
    -- ---- 粒（クラスが変わった瞬間に飛び出す）----
    particles =
      [ Color (makeColor 1 1 1 (pLife p * 2.5 / 255)) (at (pX p, pY p) (disc (pSize p / 2)))
      | p <- wParts w ]
    -- ---- 大きなラベル ----
    bigLabel = case wLabel w of
      Just i -> shadowC 0 (h * 0.08) (min ww h * 0.12) (className i)
      Nothing -> Blank
    -- ---- 各クラスの確率バー ----
    barW = min 500 (ww * 0.6)
    x0 = -barW / 2
    bars = concat (zipWith barOf [0 ..] (wResults w))
    barOf :: Int -> (Int, Float) -> [Picture]
    barOf k (i, prob) =
      let yc = h / 2 - h * 0.62 - fromIntegral k * 36 - 12
          name = className i ++ "  " ++ show (round (prob * 100) :: Int) ++ "%"
          (r, g, b) = colorOf i
      in [ Color (rgba255 0 0 0 80) (pill x0 yc barW 24)
         , Color (makeColor r g b 1) (pill x0 yc (barW * prob) 24)
         , label (x0 + 13) (yc - 9) 16 shade name
         , label (x0 + 12) (yc - 8) 16 white name ]
    -- ---- カメラ映像（右上に小さく）----
    preview = case wCam w of
      Just cam | B.length (wFrame w) >= camW * camH * 3 ->
        at (ww / 2 - 120, h / 2 - 95) (camImage cam (wFrame w) 1.25)
      _ -> Blank
    -- ---- メッセージ ----
    message = shadowC 0 (h / 2 - 34) 18 $ case length (wClasses w) of
      0 -> "Hold a key 1-4 to teach a class from the camera"
      1 -> "Now show something else and hold another key"
      _ -> "Show the camera. Hold 1-4 to add, C to reset"
    hint | wLive w = Blank
         | otherwise = shadowC (ww / 2 - 120) (h / 2 - 186) 14 "No camera: demo"

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> classifyNow env (learn (wFrame w) (fromEnum c - fromEnum '1') w)
    | c == 'c' || c == 'C' -> pure w { wClasses = [], wResults = [], wLabel = Nothing }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. onLabelChange を改造して、クラスごとにちがう動きをさせよう
-- 2. sharpness を変えると、確率バーの動きはどう変わる？
-- 3. classColors と classNotes を好きな色と音に変えよう

-- ---------- 道具 ----------

-- 明るい背景でも読めるように、文字に影をつける
shade :: Color
shade = rgba255 0 0 0 150

shadowC :: Float -> Float -> Float -> String -> Picture
shadowC x y size str = Pictures [labelC (x + 1) (y - 1) size shade str, labelC x y size white str]

className :: Int -> String
className i = "Class " ++ show (i + 1)

colorOf :: Int -> (Float, Float, Float)
colorOf i = case drop i classColors of
  (c : _) -> c
  [] -> (1, 1, 1)

hex :: Float -> Float -> Float -> (Float, Float, Float)
hex r g b = (r / 255, g / 255, b / 255)

-- 映像を 16 × 12 マスに縮めて、(R, G, B) の小数を並べたもの。これが分類の手がかりになる
features :: B.ByteString -> U.Vector Float
features f = U.generate (16 * 12 * 3) $ \n ->
  let (cellIndex, ch) = n `divMod` 3
      (cy, cx) = cellIndex `divMod` 16
      at' x y = fromIntegral (BU.unsafeIndex f ((y * camW + x) * 3 + ch)) / 255 :: Float
      total = sum [ at' (cx * 10 + i) (cy * 10 + j) | j <- [0, 3, 6, 9], i <- [0, 3, 6, 9] ]
  in total / 16

-- 両はしが丸いバー。左はし x0、中心の高さ yc、幅 w、高さ h
pill :: Float -> Float -> Float -> Float -> Picture
pill x0 yc w0 h = Polygon (cap (x0 + w - r) (-90) ++ cap (x0 + r) 90)
  where
    w = max h w0
    r = h / 2
    cap cx a0 = [ (cx + r * cos a, yc + r * sin a)
                | i <- [0 .. 8 :: Int], let a = (a0 + fromIntegral i * 22.5) * pi / 180 ]
