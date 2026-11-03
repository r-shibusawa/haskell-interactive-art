-- ============================================
-- 13 光の反射と屈折を音にする
-- 部屋の中に光の粒（光子）を放つ。壁や鏡で反射するたびに音が鳴り、反射の回数で音階が上がる
--   ガラスの板に入ると屈折して遅くなり（速さ c/n）、音色が変わる（三角波 → のこぎり波）
--   使っている式は 00・01 と同じ：反射 d' = d − 2(d·n)n、スネルの法則 n1 sinθ1 = n2 sinθ2
-- クリック = 光子を放つ（マウスの位置から中心へ向かって） / A = 自動で放つ / 1〜3 = 音階 / ↑↓ = 速さ / ←→ = ガラスの屈折率 / C = 全部消す
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, when)
import Numeric (showFFloat)

maxPhotons, maxBounces, trailMax :: Int
maxPhotons = 12            -- ★ 同時にある光子の数
maxBounces = 14            -- ★ この回数反射したら消える
trailMax = 90              -- 軌跡の長さ

-- ★ 音階：ペンタ、短調、長七
scaleOf :: Int -> [Int]
scaleOf 0 = [0, 2, 4, 7, 9]
scaleOf 1 = [0, 2, 3, 5, 7, 8, 10]
scaleOf _ = [0, 4, 7, 11]

scaleName :: Int -> String
scaleName 0 = "pentatonic"
scaleName 1 = "minor"
scaleName _ = "maj7 chord"

data Photon = Photon
  { pX, pY   :: !Float
  , pDx, pDy :: !Float
  , pBounces :: !Int
  , pTrail   :: ![Point]   -- 新しい順
  , pHue     :: !Float
  , pInside  :: !Bool
  , pLife    :: !Float
  , pFlash   :: !Int
  }

data World = World
  { wPhotons :: ![Photon]
  , wSpeed   :: !Float     -- ★ 光の速さ（px/フレーム）
  , wGlass   :: !Float     -- ★ ガラスの屈折率
  , wScale   :: !Int
  , wAuto    :: !Bool
  }

type Wall = (Point, Point)

-- 部屋の壁（4 辺）と斜めの鏡 2 枚
walls :: Env -> [Wall]
walls env =
  let wd = envW env
      h = envH env
      m = 70
  in [ ((m, 100), (wd - m, 100)), ((wd - m, 100), (wd - m, h - m)), ((wd - m, h - m), (m, h - m)), ((m, h - m), (m, 100))
     , ((wd * 0.62, 160), (wd * 0.8, 300)), ((wd * 0.2, h - 150), (wd * 0.38, h - 280)) ]

-- ガラスの板（x, y, 幅, 高さ）
glassRect :: Env -> (Float, Float, Float, Float)
glassRect env = (envW env * 0.42, envH env * 0.35, 160, 300)

inGlass :: Env -> Float -> Float -> Bool
inGlass env x y =
  let (gx, gy, gw, gh) = glassRect env
  in x > gx && x < gx + gw && y > gy && y < gy + gh

main :: IO ()
main = runSketch (sketch "36-13 Reflection and Refraction as Sound" (World [] 6 1.5 0 False))
  { skBack  = hsb 230 40 7
  , skStep  = stepWorld
  , skDraw  = drawAll
  , skEvent = onEvent
  }

emit :: Env -> Float -> Float -> Float -> Float -> World -> IO World
emit env x y dx dy w
  | len < 1e-6 = pure w
  | otherwise = do
      hue <- randomRIO (0, 360)
      let ps = wPhotons w
          kept = if length ps >= maxPhotons then drop 1 ps else ps
          new = Photon x y (dx / len) (dy / len) 0 [] hue (inGlass env x y) 1 0
      pure w { wPhotons = kept ++ [new] }
  where len = norm2 (dx, dy)

raySeg :: Float -> Float -> Float -> Float -> Wall -> Maybe Float
raySeg px py dx dy ((ax, ay), (bx, by))
  | abs den < 1e-9 = Nothing
  | t > 1e-3 && s >= 0 && s <= 1 = Just t
  | otherwise = Nothing
  where
    ex = bx - ax
    ey = by - ay
    den = dx * ey - dy * ex
    t = ((ax - px) * ey - (ay - py) * ex) / den
    s = ((ax - px) * dy - (ay - py) * dx) / den

snellRefract :: Float -> Float -> Float -> Float -> Float -> Float -> Maybe Point
snellRefract dx dy nx0 ny0 na nb =
  let c0 = negate (dx * nx0 + dy * ny0)
      flipN = c0 < 0
      nx = if flipN then negate nx0 else nx0
      ny = if flipN then negate ny0 else ny0
      c = abs c0
      r = na / nb
      k = 1 - r * r * (1 - c * c)
      f = r * c - sqrt k
  in if k < 0 then Nothing else Just (r * dx + f * nx, r * dy + f * ny)

-- ★ 光子を 1 歩進める。壁・鏡で反射、ガラスの境界で屈折
stepPhoton :: Env -> World -> Photon -> IO Photon
stepPhoton env w p =
  case [ (t, wl) | wl <- walls env, Just t <- [raySeg (pX p) (pY p) (pDx p) (pDy p) wl], t <= v ] of
    ((t, ((ax, ay), (bx, by))) : _) -> do
      let ex = bx - ax
          ey = by - ay
          len = max 1e-6 (norm2 (ex, ey))
          wnx = negate ey / len
          wny = ex / len
          dp = pDx p * wnx + pDy p * wny
          p' = p { pX = pX p + pDx p * t, pY = pY p + pDy p * t
                 , pDx = pDx p - 2 * dp * wnx, pDy = pDy p - 2 * dp * wny
                 , pBounces = pBounces p + 1 }
      playBounce w p'
    [] -> do
      -- ガラスの境界をまたいだ？
      let wasIn = pInside p
          nowIn = inGlass env nx ny
      if wasIn == nowIn
        then pure p { pX = nx, pY = ny }
        else do
          -- どの辺をまたいだかで法線を決める（縦の辺なら x 向き、横の辺なら y 向き）
          let (gx, _, gw, _) = glassRect env
              crossX = (pX p < gx) /= (nx < gx) || (pX p < gx + gw) /= (nx < gx + gw)
              bnx = if crossX then 1 else 0
              bny = if crossX then 0 else 1
              na = if wasIn then wGlass w else 1
              nb = if wasIn then 1 else wGlass w
          case snellRefract (pDx p) (pDy p) bnx bny na nb of
            Just (ddx, ddy) -> do
              playRefract nowIn
              pure p { pDx = ddx, pDy = ddy, pInside = nowIn, pX = nx, pY = ny }
            Nothing ->   -- 全反射
              playBounce w (if crossX then p { pDx = negate (pDx p), pBounces = pBounces p + 1 }
                                      else p { pDy = negate (pDy p), pBounces = pBounces p + 1 })
  where
    v = wSpeed w / (if pInside p then wGlass w else 1)
    nx = pX p + pDx p * v
    ny = pY p + pDy p * v

-- ★ 反射の回数 → 音の高さ（音階を上っていく）
playBounce :: World -> Photon -> IO Photon
playBounce w p = do
  let sc = scaleOf (wScale w)
      n = length sc
      degree = pBounces p - 1
      semi = sum (take 1 (drop (degree `mod` n) sc)) + 12 * (degree `div` n)
      freq = 220 * 2 ** (fromIntegral semi / 12) :: Float
  playTone (min 3000 freq) 0.18 0.35 (if pInside p then Sawtooth else Triangle)
  pure p { pFlash = 12 }

playRefract :: Bool -> IO ()
playRefract entering = playTone (if entering then 180 else 360) 0.12 0.5 Sine

stepWorld :: Env -> Float -> World -> IO World
stepWorld env _ w0 = do
  -- 自動で放つ
  w <- if wAuto w0 && envFrame env `mod` 40 == 0
         then do
           x <- randomRIO (120, envW env - 120)
           y <- randomRIO (140, envH env - 120)
           dx <- randomRIO (-1, 1)
           dy <- randomRIO (-1, 1)
           emit env x y dx dy w0
         else pure w0
  moved <- mapM (advance w) (wPhotons w)
  pure w { wPhotons = filter alive moved }
  where
    advance w p = do
      p1 <- foldM (\q _ -> stepPhoton env w q) p [1, 2 :: Int]
      pure p1
        { pTrail = take trailMax ((pX p1, pY p1) : pTrail p1)
        , pLife = if pBounces p1 >= maxBounces then pLife p1 - 0.02 else pLife p1
        , pFlash = if pFlash p1 > 0 then pFlash p1 - 1 else 0 }
    alive p = pLife p > 0 && pX p > -50 && pX p < envW env + 50 && pY p > -50 && pY p < envH env + 50

drawAll :: Env -> World -> Picture
drawAll env w =
  let (gx, gy, gw, gh) = glassRect env
      photon p =
        [ Color (hsbA (pHue p) 70 100 (50 * pLife p)) (thickPath 2 (pTrail p))
        , Color (hsbA (pHue p) 50 100 (100 * pLife p)) (at (pX p, pY p) (disc ((10 + fromIntegral (pFlash p)) / 2))) ]
      counters = [ txt env (pX p + 10) (pY p - 8) 14 (hsbA 0 0 100 (80 * pLife p)) (show (pBounces p)) | p <- wPhotons w ]
  in Pictures
    [ onP5 env (Pictures
        ( [ Color (hsbA 190 50 70 25) (box gx gy gw gh)
          , Color (hsbA 190 50 100 60) (thickPath 2 [(gx, gy), (gx + gw, gy), (gx + gw, gy + gh), (gx, gy + gh), (gx, gy)])
          , Color (hsb 0 0 80) (Pictures [ thickLine 4 a b | (a, b) <- walls env ]) ]
          ++ concatMap photon (wPhotons w) ))
    , Pictures counters
    , txt env 20 36 18 white
        ("Click: emit photon  A: auto " ++ onOff (wAuto w) ++ "  Scale " ++ scaleName (wScale w) ++ " (1-3)  Speed "
         ++ nf 0 (wSpeed w) ++ " (Up/Down)  Glass n = " ++ nf 2 (wGlass w) ++ " (L/R)  C: clear")
    , txt env 20 62 15 (hsb 0 0 60)
        "Each reflection climbs the scale (number = reflections). In glass it slows and the timbre changes."
    , txt env 20 84 15 (hsb 0 0 60) ("Photons fade after " ++ show maxBounces ++ " reflections.")
    ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> do
    let (mx, my) = mouseP5 env
    ox <- randomRIO (-80, 80)
    oy <- randomRIO (-80, 80)
    emit env mx my (envW env / 2 - mx + ox) (envH env / 2 - my + oy) w
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wSpeed = min 16 (wSpeed w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wSpeed = max 1 (wSpeed w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wGlass = min 2.5 (wGlass w + 0.1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wGlass = max 1 (wGlass w - 0.1) }
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "123" -> pure w { wScale = fromEnum c - 49 }
    | c `elem` "cC" -> pure w { wPhotons = [] }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 鏡の角に向けて放つと、短い間隔で反射して速いフレーズになる。部屋の形で曲が変わる
-- 2. ガラスの屈折率を 2.5 にすると、中で全反射して閉じ込められることがある（光ファイバー）
-- 3. 音の高さを「反射の回数」でなく「進んだ距離」や「入射角」で決めてみよう
-- 4. walls に鏡を足して、自分だけの「音の部屋」をつくろう。自由制作のベースに


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
