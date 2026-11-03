-- ============================================
-- 13 パーティクル・ライフ
-- 色のちがう粒どうしに「引き合う／反発する」の表（ルール）をランダムに決めるだけで、
-- 細胞のような、生き物のような構造が自然に生まれる
-- クリックで新しいルール / スペースで止める / S で保存
-- ============================================
module Main (main) where

import Art
import Control.Monad (replicateM)
import qualified Data.Vector.Unboxed as U

-- 粒たち（同じ番号が同じ粒。速く計算できるよう、値ごとに配列にする）
data Particles = Particles
  { pX  :: !(U.Vector Float)
  , pY  :: !(U.Vector Float)
  , pVx :: !(U.Vector Float)
  , pVy :: !(U.Vector Float)
  , pT  :: !(U.Vector Int)      -- 色の種類
  }

data World = World
  { wP      :: !Particles
  , wMatrix :: !(U.Vector Float)                       -- a * types + b 番目 = 色 a の粒が色 b の粒から受ける力（-1〜1）
  , wPast   :: ![(U.Vector Float, U.Vector Float)]     -- 残像のための前の位置
  , wPaused :: !Bool
  }

count, types :: Int
count = 400                -- ★ 粒の数（多いと重い）
types = 5                  -- ★ 色の種類

friction :: Float
friction = 0.85            -- ★ 摩擦

trailFrames :: Int
trailFrames = 2

trailFade :: Float
trailFade = 0.4            -- 残像の薄くなり方（p5.js の background の透明度 60 に相当）

main :: IO ()
main = runSketch (sketch "03-13 Particle Life" (World (Particles U.empty U.empty U.empty U.empty U.empty) U.empty [] False))
  { skBack  = hsb 0 0 5
  , skStart = \env -> do
      xs <- U.replicateM count (randomRIO (0, envW env))
      ys <- U.replicateM count (randomRIO (0, envH env))
      ts <- U.replicateM count (randomRIO (0, types - 1))
      let zeros = U.replicate count 0
      m <- newRules
      pure (World (Particles xs ys zeros zeros ts) m [] False)
  , skStep  = \env _ w -> pure (stepWorld env w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

newRules :: IO (U.Vector Float)
newRules = U.fromList <$> replicateM (types * types) (randomRIO (-1, 1))

stepWorld :: Env -> World -> World
stepWorld env w =
  let p = wP w
      past = take trailFrames ((pX p, pY p) : wPast w)
  in if wPaused w then w { wPast = past } else w { wP = moveAll env (wMatrix w) p, wPast = past }

moveAll :: Env -> U.Vector Float -> Particles -> Particles
moveAll env matrix p =
  let width' = envW env
      height' = envH env
      forces = U.generate count (forceOn width' height' matrix p)
      vx = U.zipWith (\v (fx, _) -> (v + fx * 0.3) * friction) (pVx p) forces
      vy = U.zipWith (\v (_, fy) -> (v + fy * 0.3) * friction) (pVy p) forces
  in p { pX = U.zipWith (\x v -> wrapF width' (x + v)) (pX p) vx
       , pY = U.zipWith (\y v -> wrapF height' (y + v)) (pY p) vy
       , pVx = vx, pVy = vy }

-- i 番目の粒が受ける力の合計
forceOn :: Float -> Float -> U.Vector Float -> Particles -> Int -> (Float, Float)
forceOn !width' !height' matrix p !i = go 0 0 0
  where
    !px = U.unsafeIndex (pX p) i
    !py = U.unsafeIndex (pY p) i
    !row = U.unsafeIndex (pT p) i * types
    go :: Int -> Float -> Float -> (Float, Float)
    go !j !fx !fy
      | j >= count = (fx, fy)
      | j == i = go (j + 1) fx fy
      | otherwise =
          -- 画面の端はつながっている（いちばん近い方向を使う）
          let dx0 = U.unsafeIndex (pX p) j - px
              dy0 = U.unsafeIndex (pY p) j - py
              dx | dx0 > width' / 2 = dx0 - width'
                 | dx0 < -width' / 2 = dx0 + width'
                 | otherwise = dx0
              dy | dy0 > height' / 2 = dy0 - height'
                 | dy0 < -height' / 2 = dy0 + height'
                 | otherwise = dy0
          in if dx > 80 || dx < -80 || dy > 80 || dy < -80    -- 遠い粒は計算しない（速くするため）
               then go (j + 1) fx fy
               else
                 let d = sqrt (dx * dx + dy * dy)
                 in if d > 0 && d < 80
                      then
                        let f | d < 20 = (d / 20 - 1) * 2     -- 近すぎると必ず反発（ぶつからない）
                              -- ★ 表の値で引力/反発
                              | otherwise = U.unsafeIndex matrix (row + U.unsafeIndex (pT p) j) * (1 - abs (2 * d - 100) / 60)
                        in go (j + 1) (fx + dx / d * f) (fy + dy / d * f)
                      else go (j + 1) fx fy

typeColor :: Int -> Float -> Color
typeColor t = hsba (fromIntegral (t * 360) / fromIntegral types) 70 100

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ p5Space env $ Pictures $
      [ dots (trailFade ^ age) xs ys | (age, (xs, ys)) <- reverse (zip [1 :: Int ..] (wPast w)) ]
      ++ [dots 1 (pX p) (pY p), ruleTable env (wMatrix w)]
  , hud env 20 15 white "Click: new rules   Space: pause   S: save"
  , hud env 40 15 white "Table: green = attract, red = repel (row color feels column color)"
  ]
  where
    p = wP w
    ts = pT p
    dots alpha xs ys = Pictures
      [ Color (typeColor t alpha) (Translate x y (disc 3.5)) | (x, y, t) <- U.toList (U.zip3 xs ys ts) ]

-- ルール表を右上に表示
ruleTable :: Env -> U.Vector Float -> Picture
ruleTable env matrix = Pictures $ concat
  [ [ Color (typeColor a 1) (box (ox + fa * s) (oy - s) (s - 2) (s - 2))
    , Color (typeColor a 1) (box (ox - s) (oy + fa * s) (s - 2) (s - 2)) ]
    ++ [ Color (if v > 0 then hsb 120 80 (40 + abs v * 60) else hsb 0 80 (40 + abs v * 60))
           (box (ox + fromIntegral b * s) (oy + fa * s) (s - 2) (s - 2))
       | b <- [0 .. types - 1], let v = matrix U.! (a * types + b) ]
  | a <- [0 .. types - 1], let fa = fromIntegral a ]
  where
    s = 22
    ox = envW env - fromIntegral types * s - 20
    oy = 50

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | snd (mouseP5 env) > 44 -> (\m -> w { wMatrix = m }) <$> newRules
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wPaused = not (wPaused w) }
  EventKey (Char c) Down _ _
    | c `elem` "sS" -> saveSvg "particle-life" env (drawWorld env w) >> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. クリックを何回もして、「細胞」「追いかけっこ」「結晶」みたいなルールを探そう
-- 2. types を 3 や 8 にすると？
-- 3. 摩擦 0.85 を 0.95 にすると、ぬるぬる動く

-- ---------- 道具 ----------

-- p5.js の座標（左上が原点、y は下向き）で描いた絵を、gloss の座標に置く
p5Space :: Env -> Picture -> Picture
p5Space env = Translate (-envW env / 2) (envH env / 2) . Scale 1 (-1)

-- マウスの位置を p5.js の座標で返す
mouseP5 :: Env -> Point
mouseP5 env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- p5.js の rect と同じ、左上の角 (x, y) と幅・高さで決める長方形
box :: Float -> Float -> Float -> Float -> Picture
box x y w h = Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- 左上から y の高さに文字を書く
hud :: Env -> Float -> Float -> Color -> String -> Picture
hud env y size = label (16 - envW env / 2) (envH env / 2 - y) size
