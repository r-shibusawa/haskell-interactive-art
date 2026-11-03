-- ============================================
-- 06 準同型と剰余群
-- φ(x) = x mod m で 12 時間の時計 Z12 を m 時間の時計 Zm に写す。足し算を保つ：φ(a + b) = φ(a) + φ(b)
--   0 に行く元の集まり = 核（kernel）K。K の剰余類を 1 点にまとめた Z12 / K は Zm と同じ（準同型定理）
-- ←→ = m（2〜6）/ 時計の数字を 2 つクリック = a + b を確かめる / スペース = 剰余類をまとめる・ばらす
-- ============================================
module Main (main) where

import Art
import Data.List (find, intercalate)

bigM :: Int
bigM = 12                -- ★ もとの時計

data World = World
  { wM      :: !Int      -- 写す先の時計
  , wMerge  :: !Float    -- 0 = ばらばら、1 = まとめる
  , wTarget :: !Float
  , wA      :: !Int      -- クリックした 2 つの数
  , wB      :: !Int
  }

main :: IO ()
main = runSketch (sketch "23-06 Homomorphism and Quotient" (World 4 0 0 (-1) (-1)))
  { skBack  = rgb255 250 248 242
  , skStep  = \_ _ w -> pure w { wMerge = wMerge w + (wTarget w - wMerge w) * 0.08 }
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \env ev w -> pure (onEvent (toP5 (envMouse env)) ev w)
  }

phi :: Int -> Int -> Int
phi m x = x `mod` m

isHom :: Int -> Bool     -- m が 12 を割り切るときだけ準同型
isHom m = bigM `mod` m == 0

-- 2 つの時計の中心と半径
r1, r2 :: Float
r1 = min (width * 0.15) (height * 0.24)
r2 = r1 * 0.75

c1, c2 :: Point
c1 = (width * 0.24, height * 0.48)
c2 = (width * 0.72, height * 0.48)

-- 左の時計の数字の位置。「まとめる」と同じ剰余類（同じ φ）の点が 1 か所に集まる
leftPos :: World -> Int -> Point
leftPos w x
  | isHom m && mg > 0.001 = (lerpF px qx mg, lerpF py qy mg)
  | otherwise = (px, py)
  where
    m = wM w
    mg = wMerge w
    (cx, cy) = c1
    (px, py) = clockPos c1 r1 x bigM
    b = -pi / 2 + fromIntegral (phi m x) * tau / fromIntegral m
    k = fromIntegral (x `div` m) :: Float
    cnt = fromIntegral (bigM `div` m) :: Float
    off = (k - (cnt - 1) / 2) * 8
    qx = cx + r1 * 0.75 * cos b + off
    qy = cy + r1 * 0.75 * sin b + off

clockPos :: Point -> Float -> Int -> Int -> Point
clockPos (cx, cy) r x mm =
  let a = -pi / 2 + fromIntegral x * tau / fromIntegral mm
  in (cx + r * cos a, cy + r * sin a)

drawWorld :: World -> Picture
drawWorld w = Pictures (maps ++ [circleAt c1 r1] ++ leftDots ++ [circleAt c2 r2] ++ rightDots ++ sums ++ hud)
  where
    m = wM w
    hom = isHom m
    ph = phi m
    hueOf v = fromIntegral v * 360 / fromIntegral m :: Float
    -- 写像の線（まとめるときは消す）
    maps =
      [ Color (hsba (hueOf (ph x)) 50 80 (0.35 * (1 - wMerge w)))
          (thickLine 2 (pt (leftPos w x)) (pt (clockPos c2 r2 (ph x) m)))
      | x <- [0 .. bigM - 1] ]
    circleAt c r = Color (makeColorI 0 0 0 30) (at (pt c) (ring r 2))
    -- 左の時計（Z12）
    leftDots = concat
      [ [ discAt px py (23 + (if inK then 2 else 1)) (if inK then gray (20 / 255) else white)
        , discAt px py (23 - (if inK then 2 else 1)) (hsb (hueOf (ph x)) 55 92)
        , txtC px py 18 (gray (20 / 255)) (show x) ]
      | x <- [0 .. bigM - 1], let (px, py) = leftPos w x, let inK = ph x == 0 ]
    -- 右の時計（Zm）
    rightDots = concat
      [ [ discAt px py 29 white
        , discAt px py 27 (hsb (hueOf y) 55 92)
        , txtC px py 22 (gray (20 / 255)) (show y) ]
      | y <- [0 .. m - 1], let (px, py) = clockPos c2 r2 y m ]
    -- クリックした a, b の足し算
    y0 = height - 110
    pa = wA w
    pb = wB w
    dark = gray (20 / 255)
    sums
      | pa >= 0 && pb >= 0 =
          let s = (pa + pb) `mod` bigM
              rhs = (ph pa + ph pb) `mod` m
              same = ph s == rhs
          in [ txt 30 y0 20 dark ("a + b = " ++ show pa ++ " + " ++ show pb ++ " = " ++ show s ++ " (mod 12) -> phi(a + b) = " ++ show (ph s))
             , txt 30 (y0 + 32) 20 dark ("phi(a) + phi(b) = " ++ show (ph pa) ++ " + " ++ show (ph pb) ++ " = " ++ show rhs ++ " (mod " ++ show m ++ ")")
             , txt 30 (y0 + 70) 24 (if same then rgb255 40 150 90 else rgb255 210 50 60)
                 (if same then "They match: OK" else "No match: X (not a homomorphism)") ]
      | pa >= 0 = [txt 30 y0 20 dark ("a = " ++ show pa ++ ". Click one more")]
      | otherwise = [txt 30 y0 16 (gray 0.43) "Click two numbers on the left clock to compare phi(a + b) with phi(a) + phi(b)"]
    -- 文字
    tx = width * 0.5 - 120
    py0 = 110 :: Float
    red' = rgb255 210 50 60
    ks = [x | x <- [0 .. bigM - 1], ph x == 0]
    hud =
      [ txt 20 36 18 dark ("Left/Right: m = " ++ show m ++ "   Click two numbers: test the sum   Space: merge kernel cosets (" ++ (if wTarget w > 0.5 then "ON" else "off") ++ ")")
      , txt 20 62 15 (gray 0.43) "Homomorphism = a map that keeps the operation. Black rim = kernel K = what phi sends to 0 (itself a subgroup)"
      , txt tx py0 20 dark ("phi : Z12 -> Z" ++ show m ++ ",  phi(x) = x mod " ++ show m)
      ] ++ (if hom
        then [ txt tx (py0 + 30) 16 (gray (60 / 255))
                 ("K = { " ++ intercalate ", " (map show ks) ++ " } (|K| = " ++ show (length ks) ++ ")   Z12 / K = Z" ++ show m
                  ++ " (12 = " ++ show (length ks) ++ " x " ++ show m ++ ")") ]
        else [ txt tx (py0 + 30) 15 red' (show m ++ " does not divide 12 -> 11 + 1 = 0, but phi(11) + phi(1) = " ++ show ((ph 11 + 1) `mod` m))
             , txt tx (py0 + 52) 15 red' "and phi(0) = 0: they differ, so this is not a homomorphism" ])

onEvent :: Point -> Event -> World -> World
onEvent mouse ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    case find (\x -> dist2 mouse (leftPos w x) < 23) [0 .. bigM - 1] of
      Just x | wA w < 0 || wB w >= 0 -> w { wA = x, wB = -1 }
             | otherwise -> w { wB = x }
      Nothing -> w
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wM = min 6 (wM w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wM = max 2 (wM w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wTarget = 1 - wTarget w }
  _ -> w

-- ---------- やってみよう ----------
-- 1. m = 3, 4, 6 で核の大きさは？ |核| × m = 12 になっている
-- 2. m = 5 を選んで 11 と 1 をクリック → 一致しない。12 の約数でないとダメな理由を考えよう
-- 3. スペースでまとめると、12 個の点が m 個の「かたまり」に。かたまりどうしの足し算 = Zm の足し算
-- 4. φ(x) = 2x mod 12 も準同型（Z12 → Z12）。核は？ 写る先（像）は？ phi を書きかえよう

-- ---------- 道具 ----------
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Point -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

-- gloss の座標 → p5 の座標（マウス用）
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y = labelC (x - width / 2) (height / 2 - y)

discAt :: Float -> Float -> Float -> Color -> Picture
discAt x y r c = Color c (at (pt (x, y)) (disc r))
