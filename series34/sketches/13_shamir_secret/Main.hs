-- ============================================
-- 13 秘密分散（シャミアの方法）
-- 秘密 S を「k−1 次の多項式 f(x) の x = 0 の値」に隠し、f(1), f(2), …, f(n) を n 人に 1 つずつ配る（シェア）。
--   k 人集まれば点 k 個から多項式が 1 つに決まる（ラグランジュ補間）→ f(0) = S が分かる
--   k−1 人以下だと、どんな S を通る曲線も引けてしまう → 秘密について何も分からない
-- シェア（点）をクリック = 選ぶ・外す / ↑↓ = しきい値 k / ←→ = 配る人数 n / スペース = 新しい秘密 / A = 全部選ぶ / C = 全部外す
-- ※ 教育用：見やすさのため実数で描いている。実際は mod p（有限体）で計算して、点から何も漏れないようにする
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM, replicateM)
import Data.List (foldl')
import Text.Printf (printf)

data World = World
  { kNeed   :: !Int        -- ★ しきい値：何人集まれば復元できるか
  , nShares :: !Int        -- ★ 配る人数
  , secret  :: !Int
  , coef    :: ![Float]
  , picked  :: ![Bool]
  , ghosts  :: ![[Point]]  -- k 人未満のときに引ける「ありえる曲線」
  }

main :: IO ()
main = do
  w0 <- newSecret (World 3 6 0 [] [] [])
  runSketch (sketch "34-13 Shamir Secret Sharing" w0)
    { skBack  = rgb255 14 18 24
    , skDraw  = drawWorld
    , skEvent = onEvent
    }

newSecret :: World -> IO World
newSecret w = do
  s <- randomRIO (20, 79)                                   -- ★ 秘密の数
  -- 高次ほど小さく（画面に収めるため）
  cs <- forM [1 .. kNeed w - 1] $ \i -> do
    r <- randomRIO (-1, 1 :: Float)
    pure (r * 40 / fromIntegral (nShares w) ^^ (i - 1))
  makeGhosts w { secret = s, coef = fromIntegral s : cs
               , picked = True : replicate (nShares w - 1) False }

-- 本当の多項式
polyF :: World -> Float -> Float
polyF w x = foldr (\c y -> y * x + c) 0 (coef w)

selected :: World -> [Int]
selected w = [i | (i, True) <- zip [1 ..] (picked w)]

-- k 人未満のとき：選んだ点 + ランダムな秘密 (0, s) + 足りない分のランダムな点 で曲線をつくる
makeGhosts :: World -> IO World
makeGhosts w
  | length sel >= kNeed w = pure w { ghosts = [] }
  | otherwise = do
      gs <- forM [0 .. 13 :: Int] $ \gi -> do
        let base = (0, fromIntegral (secret w) + (fromIntegral gi - 6.5) * 9)  -- 秘密の候補
                 : [(fromIntegral x, polyF w (fromIntegral x)) | x <- sel]
            missing = kNeed w - length base
        extra <- forM [0 .. missing - 1] $ \e -> do
          let x = fromIntegral e + 0.5
          d <- randomRIO (-15, 15)
          pure (x, polyF w x + d)
        pure (base ++ extra)
      pure w { ghosts = gs }
  where sel = selected w

-- グラフの座標
data Plot = Plot { plX :: !Float, plY :: !Float, plW :: !Float, plH :: !Float, plMax :: !Float, plN :: !Int }

plotOf :: Env -> World -> Plot
plotOf env w =
  let n = nShares w
      ym = foldl' (\m i -> max m (abs (polyF w (fromIntegral (i :: Int) / 60 * (fromIntegral n + 0.5))) * 1.15)) 130 [0 .. 60]
  in Plot 40 90 (envW env * 0.64) (envH env - 150) ym n

sxOf, syOf :: Plot -> Float -> Float
sxOf p x = plX p + (x + 0.5) / (fromIntegral (plN p) + 1) * plW p
syOf p y = plY p + plH p / 2 - y * plH p / 2 / plMax p

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (header ++ axes ++ curves ++ shares ++ panel)
  where
    sw = envW env
    pl = plotOf env w
    sx = sxOf pl
    sy = syOf pl
    k = kNeed w
    n = nShares w
    amber = rgb255 255 200 90
    good = rgb255 120 230 150
    pink = rgb255 255 120 200
    blue' = rgb255 130 200 255
    header =
      [ txt env 24 34 16 (gray 0.94)
          ("Shamir secret sharing  click points  Up/Down k = " ++ show k ++ "  Left/Right n = " ++ show n ++ "  Space new secret  A all  C none")
      , txt env 24 56 14 (gray 0.6)
          ("Give shares to " ++ show n ++ " people; any " ++ show k ++ " can recover: a (" ++ show k ++ ", " ++ show n ++ ") threshold scheme | educational (real numbers for drawing)")
      ]
    -- 軸
    axisC = rgb255 60 70 90
    axes =
      [ Color axisC (thickLine 1 (toS env (plX pl, sy 0)) (toS env (plX pl + plW pl, sy 0)))
      , Color axisC (thickLine 1 (toS env (sx 0, plY pl)) (toS env (sx 0, plY pl + plH pl)))
      ]
      ++ [ txt env (sx (fromIntegral i) - 14) (sy 0 + 20) 14 (gray 0.55) ("x=" ++ show i) | i <- [1 .. n] ]
    sel = selected w
    -- k 人以上：選んだ点から補間した曲線
    ptsUsed = [ (fromIntegral x, polyF w (fromIntegral x)) | x <- take k sel ]
    recovered = if length sel >= k then Just (lagrange ptsUsed 0) else Nothing
    curves =
      -- k 人未満：ありえる曲線をたくさん描く
      (if length sel < k
         then concatMap (\g -> drawCurve env pl (lagrange g) (rgba255 255 200 90 60) 1.5) (ghosts w)
              ++ [ at (toS env (sx 0, sy y)) (Color (rgba255 255 200 90 120) (disc 4)) | g <- ghosts w, (_, y) <- take 1 g ]
         else [])
      -- 本当の多項式（薄く）
      ++ drawCurve env pl (polyF w) (rgba255 255 255 255 40) 1
      ++ (case recovered of
            Just _ -> drawCurve env pl (lagrange ptsUsed) good 3
            Nothing -> [])
      -- 秘密の位置
      ++ [ at (toS env (sx 0, sy (fromIntegral (secret w)))) (Color pink (disc (maybe 5 (const 11) recovered))) ]
    -- シェア
    shares = concat
      [ [ at (toS env (px, py)) (Color (if on then blue' else rgb255 90 100 120) (disc (if on then 11 else 8)))
        , txt env (px + 14) (py - 10) 14 (gray 0.86) ('P' : show i) ]
      | (i, on) <- zip [1 :: Int ..] (picked w)
      , let px = sx (fromIntegral i)
            py = sy (polyF w (fromIntegral i)) ]

    -- ---- 右のパネル
    px0 = sw * 0.68 + 20
    py0 = 110
    y2 = py0 + 40 + fromIntegral n * 24
    panel =
      txt env px0 py0 17 (gray 0.94) "Shares handed out"
      : concat
        [ [ at (toS env (px0 + 7, y - 6))
              (if on then Color blue' (disc 6) else Color (gray 0.55) (ring 5 1.5))
          , txt env (px0 + 22) y 15 (if on then blue' else gray 0.55)
              ("P" ++ show i ++ ": (" ++ show i ++ ", " ++ f2 (polyF w (fromIntegral i)) ++ ")") ]
        | (i, on) <- zip [1 :: Int ..] (picked w)
        , let y = py0 + 10 + fromIntegral i * 24 ]
      ++ [ txt env px0 y2 17 (gray 0.94) ("Gathered " ++ show (length sel) ++ " / need " ++ show k) ]
      ++ case recovered of
           Just v ->
             let xs = take k sel
                 m = fromIntegral (length xs) * 20
             in [ txt env px0 (y2 + 30) 14 (gray 0.67) "Lagrange: S = sum of y_i * L_i(0)" ]
                ++ [ txt env px0 (y2 + 54 + fromIntegral j * 20) 14 (gray 0.67)
                       ("  L" ++ show xj ++ "(0) = " ++ printf "%.3f" (basisAtZero xs xj))
                   | (j, xj) <- zip [0 :: Int ..] xs ]
                ++ [ txt env px0 (y2 + 70 + m) 20 good ("Recovered secret = " ++ f2 v)
                   , txt env px0 (y2 + 98 + m) 20 pink ("True secret = " ++ show (secret w)) ]
           Nothing ->
             [ txt env px0 (y2 + 34 + fromIntegral j * 22) 14 amber s
             | (j, s) <- zip [0 :: Int ..]
                 [ show (k - length sel) ++ " more needed."
                 , "Every yellow curve fits these points."
                 , "The value at x = 0 (secret candidate)"
                 , "could be anything -> nothing learned" ] ]

-- 選んだ人 xs のうち xj の重み L_xj(0)
basisAtZero :: [Int] -> Int -> Float
basisAtZero xs xj = product
  [ (0 - fromIntegral m) / (fromIntegral xj - fromIntegral m) | m <- xs, m /= xj ]

drawCurve :: Env -> Plot -> (Float -> Float) -> Color -> Float -> [Picture]
drawCurve env pl fn col wd =
  [ Color col (thickLine wd (toS env a) (toS env b))
  | (a, b) <- zip pts (drop 1 pts), inside a, inside b ]
  where
    n = fromIntegral (plN pl) :: Float
    pts = [ (sxOf pl x, syOf pl (fn x)) | i <- [0 .. 120 :: Int], let x = -0.4 + fromIntegral i / 120 * (n + 0.8) ]
    inside (_, y) = y > plY pl && y < plY pl + plH pl

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let pl = plotOf env w
        (mx0, my0) = envMouse env
        mx = mx0 + envW env / 2
        my = envH env / 2 - my0
        hit i = dist2 (mx, my) (sxOf pl (fromIntegral i), syOf pl (polyF w (fromIntegral i))) < 18
    in case filter hit [1 .. nShares w] of
         (i : _) -> makeGhosts w { picked = [if j == i then not p else p | (j, p) <- zip [1 ..] (picked w)] }
         [] -> pure w
  EventKey (SpecialKey KeyUp) Down _ _    -> newSecret w { kNeed = min (nShares w) (kNeed w + 1) }
  EventKey (SpecialKey KeyDown) Down _ _  -> newSecret w { kNeed = max 2 (kNeed w - 1) }
  EventKey (SpecialKey KeyRight) Down _ _ -> newSecret w { nShares = min 8 (nShares w + 1) }
  EventKey (SpecialKey KeyLeft) Down _ _  -> newSecret w { nShares = max (kNeed w) (nShares w - 1) }
  EventKey (SpecialKey KeySpace) Down _ _ -> newSecret w
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> makeGhosts w { picked = map (const True) (picked w) }
    | c `elem` "cC" -> makeGhosts w { picked = map (const False) (picked w) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. k = 2 なら直線。1 人の点を通る直線は無数にあるので秘密は分からない。2 人なら 1 本に決まる
-- 2. k = 3, n = 6 で、どの 3 人を選んでも同じ秘密が出るのを確かめよう
-- 3. 会社の金庫の暗証番号を「役員 5 人のうち 3 人」で開けられるようにするには k, n をいくつにする？
-- 4. 1 人がうそのシェアを出したら復元した秘密はどうなる？（4 人目のシェアで確かめる方法を考えよう）

-- ==================================================
-- ここから下は「道具」（ラグランジュ補間）
-- ==================================================
-- 点 pts = [(x, y), …] を全部通る多項式の、x での値
lagrange :: [Point] -> Float -> Float
lagrange pts x = sum
  [ yj * product [ (x - xm) / (xj - xm) | (m, (xm, _)) <- ipts, m /= j ]
  | (j, (xj, yj)) <- ipts ]
  where ipts = zip [0 :: Int ..] pts

f2 :: Float -> String
f2 = printf "%.2f"

-- p5.js と同じ「左上が原点・y は下向き」の座標を gloss の座標に直す
toS :: Env -> Point -> Point
toS e (x, y) = (x - envW e / 2, envH e / 2 - y)

-- 文字（(x, y) は左端の下）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt e x y s c str = let (a, b) = toS e (x, y) in label a b s c str
