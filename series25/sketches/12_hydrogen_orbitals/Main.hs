-- ============================================
-- 12 水素原子の軌道
-- 電子の波動関数 ψnlm = Rnl(r) · Ylm(θ, φ)（動径 × 角度）。断面を切って |ψ|² を明るさ、ψ の符号を色で描く
--   エネルギーは n だけで決まる En = −13.6 eV / n²。n → 2 へ落ちるときに出る光がバルマー系列（目に見える 4 本の線）
-- ↑↓ = n（1〜4） / ←→ = l（0〜n−1） / M = m を切りかえ / P = 断面（xz 面 ⇔ xy 面） / Z = 拡大 / G = 明るさの強調
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Numeric (showFFloat)

gridN :: Int
gridN = 160           -- ★ 断面の画素数（大きいほどきれいだが重い）

data World = World
  { wN      :: !Int
  , wL      :: !Int
  , wM      :: !Int
  , wXZ     :: !Bool      -- 断面：True = xz 面、False = xy 面
  , wZoom   :: !Float
  , wGamma  :: !Float     -- ★ 明るさの強調（小さいほど薄い所が見える）
  , wImg    :: !Picture   -- n, l, m を変えたときだけ作り直す断面の画像
  , wRadial :: !(U.Vector Float)
  }

main :: IO ()
main = runSketch (sketch "25-12 Hydrogen Orbitals" (render (World 2 1 0 True 1 0.45 Blank U.empty)))
  { skBack  = hsb 240 30 6
  , skDraw  = drawW
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

-- 表示する範囲（ボーア半径 a0 = 1 の単位）
extent :: World -> Float
extent w = (2.4 * fromIntegral (wN w * wN w) + 5) / wZoom w

-- ★ 動径関数 Rnl(r) ∝ ρ^l e^{−ρ/2} L(n−l−1, 2l+1)(ρ)、ρ = 2r/n（L はラゲールの陪多項式、漸化式で計算）
rnl :: World -> Float -> Float
rnl w r =
  let n = wN w
      l = wL w
      rho = 2 * r / fromIntegral n
      k = n - l - 1
      al = fromIntegral (2 * l + 1) :: Float
      go :: Int -> Float -> Float -> Float
      go j l0 l1
        | j >= k = l1
        | otherwise =
            let fj = fromIntegral j
                l2 = ((2 * fj + 1 + al - rho) * l1 - (fj + al) * l0) / (fj + 1)
            in go (j + 1) l1 l2
      lk = if k == 0 then 1 else go 1 1 (1 + al - rho)
  in rho ^ l * exp (-rho / 2) * lk

-- ★ 角度の部分（実数の球面調和関数）∝ P_l^|m|(cos θ) × cos(mφ) か sin(|m|φ)
ylm :: World -> Float -> Float -> Float
ylm w ct ph =
  let l = wL w
      m = wM w
      am = abs m
      st = sqrt (max 0 (1 - ct * ct))
      pmm0 = foldl' (\p i -> p * negate (fromIntegral (2 * i - 1)) * st) 1 [1 .. am]      -- P_m^m
      legendre
        | l > am = up (am + 2) pmm0 (ct * fromIntegral (2 * am + 1) * pmm0)               -- P_{m+1}^m から
        | otherwise = pmm0
      up :: Int -> Float -> Float -> Float
      up ll a b
        | ll > l = b
        | otherwise =
            let p2 = (fromIntegral (2 * ll - 1) * ct * b - fromIntegral (ll + am - 1) * a) / fromIntegral (ll - am)
            in up (ll + 1) b p2
  in if m > 0 then legendre * cos (fromIntegral m * ph)
     else if m < 0 then legendre * sin (fromIntegral am * ph)
     else legendre

psi :: World -> Float -> Float -> Float -> Float
psi w x y z =
  let r = sqrt (x * x + y * y + z * z) + 1e-9
  in rnl w r * ylm w (z / r) (atan2 y x)

-- 断面の画像をつくる（n, l, m を変えたときだけ）
render :: World -> World
render w =
  let e = extent w
      g = fromIntegral gridN :: Float
      val k =
        let (j, i) = k `divMod` gridN
            u = ((fromIntegral i + 0.5) / g * 2 - 1) * e
            v = (1 - (fromIntegral j + 0.5) / g * 2) * e
        in if wXZ w then psi w u 0 v else psi w u v 0
      vals = U.generate (gridN * gridN) val
      mx = U.foldl' (\m p -> max m (p * p)) 1e-30 vals
      pixel i j =
        let p = vals `U.unsafeIndex` (j * gridN + i)
            b = (p * p / mx) ** wGamma w
            -- ψ > 0 はオレンジ、ψ < 0 は水色（符号 = 位相 0 か π）
            (cr, cg, cb) = if p >= 0 then (255, 150, 60) else (70, 180, 255)
        in (q (cr * b), q (cg * b), q (cb * b))
      -- 動径分布 P(r) = r² R²（半径 r の殻に電子がいる確率）
      raw = U.generate 301 (\i -> let r = fromIntegral i / 300 * e * 1.4
                                      bigR = rnl w r
                                  in r * r * bigR * bigR)
      rm = max 1e-30 (U.maximum raw)
  in w { wImg = pixelImage gridN gridN 1 pixel, wRadial = U.map (/ rm) raw }

q :: Float -> Word8
q v = fromIntegral (max 0 (min 255 (floor v :: Int)))

orbitalName :: World -> String
orbitalName w =
  let l = wL w
      m = wM w
      base = show (wN w) ++ ["spdf" !!! l]
      suffix = case (l, m) of
        (1, 0) -> "z"
        (1, 1) -> "x"
        (1, -1) -> "y"
        (2, 0) -> "z2"
        (2, 1) -> "xz"
        (2, -1) -> "yz"
        (2, 2) -> "x2-y2"
        (2, -2) -> "xy"
        (3, _) -> " (m = " ++ show m ++ ")"
        _ -> ""
  in base ++ suffix

-- 波長（nm）→ 色（目に見える範囲のだいたいの色）
waveColor :: Float -> Color
waveColor wl = hsb (clampF 0 270 (remap wl 400 700 270 0)) 90 100

balmer :: Int -> Float
balmer k = 1e9 / (1.097e7 * (0.25 - 1 / fromIntegral (k * k)))

drawW :: Env -> World -> Picture
drawW env w =
  let width = envW env
      height = envH env
      n = wN w
      l = wL w
      s = min (height - 160) (width * 0.5)
      ox = 40
      oy = 110
      imgPic = at (pt env (ox + s / 2) (oy + s / 2)) (Scale (s / fromIntegral gridN) (s / fromIntegral gridN) (wImg w))
      -- 右：動径分布
      px = ox + s + 50
      pw = width - px - 40
      py = oy + 10
      ph = height * 0.28
      radial = [ (px + fromIntegral i / 300 * pw, py + 10 + ph - v * ph) | (i, v) <- zip [0 :: Int ..] (U.toList (wRadial w)) ]
      -- エネルギー準位
      ey = py + ph + 80
      eh = height - ey - 170
      ew = pw * 0.45
      yOf e = ey + 20 + eh * (negate e / 13.6)      -- E = −13.6 が下、0 が上
      levelPic k =
        let e = -13.6 / fromIntegral (k * k) :: Float
            y = yOf e
            shift' = if k == 4 then 160 else 0
        in [ seg env (if k == n then 3 else 1) (if k == n then hsb 55 90 100 else hsb 0 0 60) (px, y) (px + ew, y) ]
           ++ [ txt env (px + ew + 8 + shift') (y + 5) 14 (gray 0.8) ("n=" ++ show k ++ " " ++ nf 2 e ++ "eV") | k <= 4 ]
      -- バルマー系列（n → 2）の矢印
      arrowPic k =
        let x = px + 20 + fromIntegral (k - 3) * 30
        in seg env (if k == n then 4 else 2) (waveColor (balmer k)) (x, yOf (-13.6 / fromIntegral (k * k))) (x, yOf (-3.4))
      -- スペクトル（380〜700 nm）
      sx = px
      sy = height - 80
      sw = pw
      sh = 36
      linePic k =
        let wl = balmer k
            x = sx + (wl - 380) / 320 * sw
        in [ seg env (if k == n then 6 else 3) (waveColor wl) (x, sy) (x, sy + sh)
           , txt env (x - 18) (sy + sh + (if k == 6 then 34 else 17)) 14 (gray 0.8) (nf 0 wl ++ " nm") ]
      planeText = if wXZ w then "xz plane (y = 0): across x, up z" else "xy plane (z = 0): across x, up y"
  in Pictures $
       [ imgPic
       , frameBox env (hsb 0 0 40) ox oy s s
       , dotAt env white (ox + s / 2) (oy + s / 2) 4       -- 原子核
       , txt env ox (oy + s + 22) 14 (gray 0.8) (planeText ++ "   width " ++ nf 0 (2 * extent w) ++ " a0")
       , txt env px py 16 white "Radial r^2 |R(r)|^2 (chance at radius r)"
       , frameBox env (hsb 0 0 35) px (py + 10) pw ph
       , curve env 2 (hsb 30 80 100) radial
       , txt env px (py + ph + 26) 14 (gray 0.6) "0"
       , txt env (px + pw - 90) (py + ph + 26) 14 (gray 0.6) ("r = " ++ nf 0 (extent w * 1.4) ++ " a0")
       , txt env px (py + ph + 46) 14 (gray 0.6) ("Nodes: radial " ++ show (n - l - 1) ++ " + angular " ++ show l ++ " = n - 1 = " ++ show (n - 1))
       , txt env px ey 16 white "Energy levels En = -13.6 / n^2 eV"
       ]
       ++ concatMap levelPic [1 .. 6 :: Int]
       ++ map arrowPic [3 .. 6 :: Int]
       ++ [ box env (hsb 0 0 12) sx sy sw sh ]
       ++ concatMap linePic [3 .. 6 :: Int]
       ++ [ txt env sx (sy - 8) 14 (gray 0.7) "Hydrogen Balmer series (n = 3, 4, 5, 6 -> 2)"
          , txt env 20 26 16 white ("Up/Down: n = " ++ show n ++ "   Left/Right: l = " ++ show l ++ " (" ++ ["spdf" !!! l] ++ ")   M: m = " ++ show (wM w) ++ "   -> " ++ orbitalName w ++ " orbital")
          , txt env 20 48 16 white ("P: plane " ++ (if wXZ w then "xz" else "xy") ++ "   Z: zoom x " ++ nf 1 (wZoom w) ++ "   G: gamma " ++ nf 2 (wGamma w))
          , txt env 20 70 14 (gray 0.6) "Brightness = |psi|^2 (where the electron is likely), color = sign of psi (orange +, blue -)."
          , txt env 20 90 14 (gray 0.6) "Orbitals with y (m < 0) are invisible in the xz plane: press P for the xy plane."
          ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyUp) Down _ _ -> changed w { wN = min 4 (wN w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _ -> changed w { wN = max 1 (wN w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> changed w { wL = wL w + 1 }
  EventKey (SpecialKey KeyLeft) Down _ _ -> changed w { wL = wL w - 1 }
  EventKey (Char c) Down _ _ -> case toLower c of
    'm' -> changed w { wM = if wM w + 1 > wL w then negate (wL w) else wM w + 1 }
    'p' -> changed w { wXZ = not (wXZ w) }
    'z' -> changed w { wZoom = if wZoom w == 1 then 2 else if wZoom w == 2 then 0.5 else 1 }
    'g' -> changed w { wGamma = if wGamma w == 0.45 then 0.25 else if wGamma w == 0.25 then 1 else 0.45 }
    _ -> w
  _ -> w
  where
    -- l と m を許される範囲に入れてから描き直す
    changed v =
      let l = max 0 (min (wN v - 1) (wL v))
          m = max (negate l) (min l (wM v))
      in render v { wL = l, wM = m }

-- ---------- やってみよう ----------
-- 1. 1s → 2s → 3s と n を上げると、動径分布の山はどこへ動く？ 山の数は？（節の数 n − l − 1）
-- 2. 2p の m = 0（z）と m = 1（x）を比べる。形は同じで向きだけが違う。m = −1（y）は P で xy 面に
-- 3. 3d の 5 つの m を順に見よう。化学で出てくる d 軌道の形（四つ葉）になっている？
-- 4. 3 → 2 の光（656 nm）は赤、4 → 2 は青緑。En の式から波長 λ = hc / ΔE を自分で計算してみよう

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- 範囲の外なら空白を返す（!! の安全な版）
(!!!) :: String -> Int -> Char
xs !!! i = case drop i xs of
  (x : _) | i >= 0 -> x
  _ -> ' '

nf :: Int -> Float -> String
nf d x = showFFloat (Just d) x ""

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Env -> Float -> Float -> Point
pt env x y = (x - envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = pt env x y in label gx gy size c s

seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c (x1, y1) (x2, y2) = Color c (thickLine w (pt env x1 y1) (pt env x2 y2))

curve :: Env -> Float -> Color -> [Point] -> Picture
curve env w c ps = Color c (thickPath w (map (uncurry (pt env)) ps))

box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

frameBox :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
frameBox env c x y w h = let (gx, gy) = pt env (x + w / 2) (y + h / 2) in Color c (rectLine gx gy w h)

dotAt :: Env -> Color -> Float -> Float -> Float -> Picture
dotAt env c x y d = Color c (at (pt env x y) (disc (d / 2)))
