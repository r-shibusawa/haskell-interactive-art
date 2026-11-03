-- ============================================
-- 15 カメラで行列を数える（おまけ）
-- カメラの前を横切った人（手・物）を「到着」として数え、到着率 λ をその場で推定する
--   前のフレームとの差（フレーム間差分）が、映像の中の黄色い線のまわりで急に増えたら 1 人と数える
--   集めた到着の間隔が指数分布、10 秒ごとの人数がポアソン分布に近いか確かめる（01 の答え合わせを本物のデータで）
-- クリック = 手動で 1 人追加（カメラがなくても遊べる）/ ←→ = 動きのしきい値 / ↑↓ = 線の位置 / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.Array.Unboxed (UArray, accumArray, (!))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Foldable (foldl', toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import Text.Printf (PrintfArg, printf)

vw, vh, bandHalf, binSec :: Int
vw = 160          -- ★ カメラ画像の大きさ（小さいほど速い）
vh = 120
bandHalf = 4      -- ★ 線のまわりの帯の半分の幅（画素）
binSec = 10       -- ★ 人数を数える区間（秒）

trigger, cooldown :: Float
trigger = 0.12    -- ★ 帯の中で動いた画素の割合がこれを超えたら到着
cooldown = 0.7    -- ★ 1 回数えたら、これだけ（秒）は数えない

data World = World
  { wCam :: !(Maybe Camera)
  , wLive :: !Bool
  , wFrame :: !B.ByteString          -- いまの映像
  , wPrev :: !(U.Vector Float)       -- 前のフレームの明るさ
  , wMoved :: !(U.Vector Bool)       -- 動いた画素
  , wDiffThr :: !Float               -- ★ 画素の明るさの差がこれ以上なら「動いた」
  , wLineX :: !Int
  , wArrivals :: !(Seq Float)
  , wStartT, wLastHit, wActivity, wNow :: !Float
  , wActRec :: !(Seq Float)
  }

main :: IO ()
main = runSketch (sketch "29-15 Counting with a Camera" world0)
  { skBack  = hsb 60 20 11
  , skStart = \_ -> (\c -> world0 { wCam = Just c }) <$> startCamera vw vh
  , skStep  = \env _ w -> stepWorld env w
  , skDraw  = \_ w -> drawAll w
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

world0 :: World
world0 = World Nothing False B.empty U.empty (U.replicate (vw * vh) False) 40 80 Seq.empty 0 (-10) 0 0 Seq.empty

-- ★ フレーム間差分：線のまわりの帯で動いた画素の割合
detect :: B.ByteString -> World -> (Float, U.Vector Float, U.Vector Bool)
detect frame w
  | B.length frame < vw * vh * 3 = (0, wPrev w, wMoved w)
  | otherwise =
      let px i = fromIntegral (BU.unsafeIndex frame i) :: Float
          gray1 = U.generate (vw * vh) (\i -> (px (i * 3) + px (i * 3 + 1) + px (i * 3 + 2)) / 3)
          prev = wPrev w
          moved | U.null prev = U.replicate (vw * vh) False
                | otherwise = U.zipWith (\a b -> abs (a - b) > wDiffThr w) gray1 prev
          inBand i = abs (i `mod` vw - wLineX w) <= bandHalf
          (nMoved, total) = U.ifoldl' (\(!m, !t) i v ->
            if inBand i then (if v then m + 1 else m, t + 1) else (m, t)) (0 :: Int, 0 :: Int) moved
      in (fromIntegral nMoved / fromIntegral (max total 1), gray1, moved)

stepWorld :: Env -> World -> IO World
stepWorld env w = do
  frame <- maybe (pure B.empty) cameraFrame (wCam w)
  live <- maybe (pure False) camLive (wCam w)
  let (activity, gray1, moved) = detect frame w
      t = envTime env - wStartT w
      hit = activity > trigger && t - wLastHit w > cooldown
      recs = wActRec w |> activity
  pure w { wFrame = frame, wLive = live, wPrev = gray1, wMoved = moved, wActivity = activity, wNow = t
         , wArrivals = if hit then wArrivals w |> t else wArrivals w
         , wLastHit = if hit then t else wLastHit w
         , wActRec = if Seq.length recs > 300 then Seq.drop 1 recs else recs }

vx, vy, s, vidW, vidH :: Float
vx = 30
vy = 100
s = min ((width * 0.42) / fromIntegral vw) ((height * 0.42) / fromIntegral vh)
vidW = fromIntegral vw * s
vidH = fromIntegral vh * s

drawAll :: World -> Picture
drawAll w = Pictures
  [ drawVideo w
  , drawEstimate w
  , txt 20 40 18 (gray 1) ((if wLive w then "Camera OK" else "No camera: demo video") ++ "  Click: add 1 by hand  Threshold " ++ nf 0 (wDiffThr w)
      ++ " (<- ->)  Line (Up/Down)  R: reset  Time " ++ nf 0 (wNow w) ++ " s")
  , txt 20 68 15 (hsb 0 0 60) "Cross at random times and gaps look exponential. Cross to a metronome and CV nears 0: that is not Poisson."
  ]

-- 映像（動いた画素はオレンジを重ねる）と線
drawVideo :: World -> Picture
drawVideo w = Pictures
  ( [ if B.length frame >= vw * vh * 3
        then at (pt (vx + vidW / 2) (vy + vidH / 2)) (pixelImage vw vh s pixel)
        else box vx vy vidW vidH (hsb 0 0 25) ]
 ++ [ seg (if flash then 5 else 2) (if flash then hsb 0 80 100 else hsb 55 90 100) (lx, vy) (lx, vy + vidH)
    , box (vx + fromIntegral (wLineX w - bandHalf) * s) vy (fromIntegral (bandHalf * 2) * s) vidH (hsba 55 90 100 0.15)
      -- 動きの量
    , seg 1 (hsba 0 0 100 0.3) (vx, ay) (vx + vidW, ay)
    , seg 1 (hsba 0 70 100 0.6) (vx, ay - trigger * 200) (vx + vidW, ay - trigger * 200)
    , path 1.5 (hsb 30 80 100) [ (vx + i / 300 * vidW, ay - min v 0.3 * 200) | (i, v) <- zip [0 ..] (toList (wActRec w)) ]
    , txt vx (ay + 20) 14 (hsb 0 0 60) "Motion near the line (above red = 1 arrival)" ] )
  where
    frame = wFrame w
    moved = wMoved w
    flash = wNow w - wLastHit w < 0.3
    lx = vx + fromIntegral (wLineX w) * s
    ay = vy + vidH + 70
    pixel x y =
      let i = y * vw + x
          o = i * 3
          r = BU.unsafeIndex frame o
          g = BU.unsafeIndex frame (o + 1)
          b = BU.unsafeIndex frame (o + 2)
          mix a c = fromIntegral ((fromIntegral a * 55 + c * 200) `div` 255 :: Int)
      in if moved U.! i then (mix r 255, mix g 120, mix b 40) else (r, g, b)

-- 推定
drawEstimate :: World -> Picture
drawEstimate w = Pictures
  ( [ txt rx ry 22 (gray 1) ("Arrivals counted  " ++ show n)
    , txt rx (ry + 30) 16 (gray 1) ("lambda ~ " ++ nf 1 lamAll ++ " /min (overall)  +-" ++ nf 1 (sqrt (fromIntegral n) / bigT * 60))
    , txt rx (ry + 56) 16 (gray 1) ("Last 30 s  " ++ nf 0 (fromIntegral recent * 2 :: Float) ++ " /min")
    , txt rx (ry + 82) 16 (gray 1) ("Mean gap " ++ nf 2 mg ++ " s  CV " ++ (if nGaps > 2 then nf 2 (sd / max mg 1e-6) else "-") ++ " (Poisson ~ 1, regular ~ 0)")
      -- 間隔のヒストグラム vs 指数分布
    , seg 1 ax (gx, gy) (gx + gw, gy) ]
 ++ [ let hh = dens i / peak * gh
      in box (gx + fromIntegral i * gw / fromIntegral nb + 1) (gy - hh) (gw / fromIntegral nb - 2) hh (hsba 190 60 90 0.7)
    | i <- [0 .. nb - 1] ]
 ++ [ path 2 (hsb 45 90 100) [ (gx + gw * fromIntegral i / 50, gy - lam * exp (negate lam * (xmax * fromIntegral i / 50)) / peak * gh) | i <- [0 .. 50 :: Int] ]
    | nGaps > 1 ]
 ++ [ txt gx (gy - gh - 6) 14 (hsb 0 0 60) ("Gaps (0 to " ++ nf 1 xmax ++ " s) and exponential (yellow)")
      -- 区間ごとの人数 vs ポアソン
    , seg 1 ax (px, py) (px + pw, py) ]
 ++ concat [ let f = fromIntegral (freq ! k) / fromIntegral (max nBin 1) :: Float
             in [ box (px + fromIntegral k * cw + 2) (py - f * ph * 2) (cw - 4) (f * ph * 2) (hsba 320 50 90 0.7)
                , dot (px + fromIntegral k * cw + cw / 2) (py - min p 0.5 * ph * 2) 8 (hsb 45 90 100)
                , txtC (px + fromIntegral k * cw + cw / 2) (py + 18) 14 (hsb 0 0 60) (show k) ]
           | (k, p) <- zip [0 .. kmax] pmf ]
 ++ [ txt px (py - ph - 6) 14 (hsb 0 0 60) ("Count per " ++ show binSec ++ " s (pink, " ++ show nBin ++ " bins) and Poisson (yellow)") ] )
  where
    ax = hsba 0 0 100 0.3
    arrs = toList (wArrivals w)
    t = wNow w
    n = Seq.length (wArrivals w)
    bigT = max t 1e-3
    lamAll = fromIntegral n / bigT * 60
    recent = length (filter (> t - 30) arrs)
    gaps = zipWith (-) (drop 1 arrs) arrs
    nGaps = length gaps
    mg = sum gaps / fromIntegral (max nGaps 1)
    sd = sqrt (foldl' (\a g -> a + (g - mg) * (g - mg)) 0 gaps / fromIntegral (max (nGaps - 1) 1))
    rx = width * 0.5
    ry = 120
    gx = rx
    gy = height * 0.62
    gw = width * 0.44
    gh = height * 0.2
    nb = 12 :: Int
    xmax = max 4 (mg * 4)
    bins :: UArray Int Int
    bins = accumArray (+) 0 (0, nb - 1) [ (max 0 (min (nb - 1) (floor (g / xmax * fromIntegral nb))), 1) | g <- gaps ]
    lam = 1 / max mg 1e-6
    bw = xmax / fromIntegral nb
    dens i = fromIntegral (bins ! i) / fromIntegral (max nGaps 1) / bw :: Float
    peak = maximum (lam * 1.2 : map dens [0 .. nb - 1])
    nBin = max 0 (floor (t / fromIntegral binSec)) :: Int
    cnt :: UArray Int Int
    cnt = accumArray (+) 0 (0, max 0 (nBin - 1)) [ (b, 1) | a <- arrs, let b = floor (a / fromIntegral binSec), b >= 0, b < nBin ]
    m = lamAll / 60 * fromIntegral binSec
    kmax = 10 :: Int
    freq :: UArray Int Int
    freq = accumArray (+) 0 (0, kmax) [ (min (cnt ! b) kmax, 1) | b <- [0 .. nBin - 1] ]
    px = rx
    py = height - 40
    pw = gw
    ph = height * 0.16
    cw = pw / fromIntegral (kmax + 1)
    pmf = take (kmax + 1) (scanl (\p k -> p * m / fromIntegral k) (exp (negate m)) [1 :: Int ..])

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> w { wArrivals = wArrivals w |> (envTime env - wStartT w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> w { wDiffThr = min 120 (wDiffThr w + 5) }
  EventKey (SpecialKey KeyLeft) Down _ _ -> w { wDiffThr = max 10 (wDiffThr w - 5) }
  EventKey (SpecialKey KeyUp) Down _ _ -> w { wLineX = min (vw - bandHalf - 1) (wLineX w + 8) }
  EventKey (SpecialKey KeyDown) Down _ _ -> w { wLineX = max bandHalf (wLineX w - 8) }
  EventKey (Char c) Down _ _ | c `elem` "rR" ->
    w { wArrivals = Seq.empty, wStartT = envTime env, wLastHit = -10 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 2 分間、教室のみんなが好きなときに手を横切らせる。λ と変動係数を記録する
-- 2. 手拍子に合わせて規則的に横切らせると、ヒストグラムの形はどう変わる？
-- 3. 推定した λ を 03 の M/M/1 に入れて「窓口が 1 分に何人さばければ、平均の行列が 2 人以下か」を計算する
-- 4. 線を 2 本にして、左→右と右→左を区別して数える（入店と退店）。店内人数 = 入 − 出 が数えられる

-- ==================================================
-- 道具
-- ==================================================
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）を gloss の座標に変える
pt :: Float -> Float -> Point
pt x y = (x - width / 2, height / 2 - y)

-- 左上の角が (x, y) の長方形
box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)

-- 中心 (x, y)、直径 d の円
dot :: Float -> Float -> Float -> Color -> Picture
dot x y d c = Color c (at (pt x y) (disc (d / 2)))

-- 太さ wd の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg wd c (x1, y1) (x2, y2) = Color c (thickLine wd (pt x1 y1) (pt x2 y2))

-- 太さ wd の折れ線
path :: Float -> Color -> [Point] -> Picture
path wd c ps = Color c (thickPath wd [pt x y | (x, y) <- ps])

-- 文字：(x, y) は左下の位置
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

-- 中央ぞろえの文字
txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y s c str = txt (x - fromIntegral (length str) * s * 0.36) y s c str

-- p5 の nf(v, 1, d)：小数 d 桁の文字列
nf :: PrintfArg a => Int -> a -> String
nf d v = printf "%.*f" d v
