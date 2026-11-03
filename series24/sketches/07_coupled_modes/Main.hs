-- ============================================
-- 07 ばねと連成振動・基準モード
-- ばねでつながったおもりは、ふつうは複雑に揺れる。でも「基準モード」の形で揺らすと、全員が同じ周期で揺れ続ける
--   どんな揺れも基準モードの足し算。1 個だけ揺らすとエネルギーが隣へ行ったり来たり（うなり）
-- ←→ = おもりの数（2〜5）/ 1〜5 = 基準モードで揺らす / B = 1 個目だけ揺らす / W = 弱い結合 / ドラッグ = おもりを引っぱる
-- ============================================
module Main (main) where

import Art
import Data.Array
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (foldl', sortOn)
import qualified Data.Sequence as Seq
import Text.Printf (printf)

kWall, kWeak, mass :: Float
kWall = 10      -- ★ 壁とつなぐばね [N/m]（おもりどうしのばねも、ふつうは同じ強さ）
kWeak = 1.5     -- ★ W を押したときのおもりどうしのばね
mass = 1        -- ★ おもりの質量 [kg]

hues :: [Float]
hues = [30, 200, 130, 320, 55]

histLen :: Int
histLen = 600

data World = World
  { wN      :: !Int             -- ★ おもりの数
  , wWeak   :: !Bool
  , wKc     :: !Float           -- おもりどうしのばね
  , wX, wV  :: ![Float]
  , wModes  :: ![[Float]]       -- 基準モードの形（モードごと）
  , wOmegas :: ![Float]
  , hX, hE  :: ![Seq.Seq Float]
  , wGrab   :: !Int
  }

main :: IO ()
main = runSketch (sketch "24-07 Coupled Modes" (excite 1 (rebuild world0)))
  { skBack  = hsb 190 30 10
  , skStep  = \env _ w -> pure (stepWorld (fst (toP5 (envMouse env))) w)
  , skDraw  = \_ w -> drawAll w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

world0 :: World
world0 = World 2 False kWall [] [] [] [] [] [] (-1)

-- 剛性行列 K（N×N）をつくり、固有ベクトル = 基準モードの形、固有値 = ω² を求める
rebuild :: World -> World
rebuild w =
  let n = wN w
      kc = if wWeak w then kWeak else kWall
      entry i j
        | i == j = (if i == 0 then kWall else kc) + (if i == n - 1 then kWall else kc)
        | abs (i - j) == 1 = -kc
        | otherwise = 0
      a = listArray ((0, 0), (n - 1, n - 1)) [entry i j / mass | i <- [0 .. n - 1], j <- [0 .. n - 1]]
      (vals, vecs) = jacobiEigen n a
      order = sortOn snd (zip [0 ..] vals)
  in w { wKc = kc
       , wOmegas = [sqrt (max v 0) | (_, v) <- order]
       , wModes = [[vecs ! (r, i) | r <- [0 .. n - 1]] | (i, _) <- order]
       , wX = replicate n 0, wV = replicate n 0
       , hX = replicate n Seq.empty, hE = replicate n Seq.empty }

excite :: Int -> World -> World
excite k w = case drop (k - 1) (wModes w) of
  (shape : _) | k >= 1 && k <= wN w ->
    let s = 0.6 / foldl' max 1e-6 (map abs shape)
    in w { wX = map (* s) shape, wV = map (const 0) shape }
  _ -> w

kick :: World -> World
kick w = w { wX = 0.8 : drop 1 (map (const 0) (wX w)) }

forces :: Float -> [Float] -> [Float]
forces kc xs =
  let n = length xs
      lefts = Nothing : map Just xs
      rights = map Just (drop 1 xs) ++ [Nothing]
      f l x r =
        let fl = maybe (-kWall * x) (\p -> -kc * (x - p)) l
            fr = maybe (-kWall * x) (\p -> -kc * (x - p)) r
        in (fl + fr) / mass
  in n `seq` zipWith3 f lefts xs rights

layout :: Int -> (Float, Float, Float, Float)
layout n = let spacing = width * 0.44 / fromIntegral (n + 1)
           in (spacing, height * 0.36, width * 0.03, spacing * 0.5)

stepWorld :: Float -> World -> World
stepWorld mx w =
  let g = wGrab w
      kc = wKc w
      sub = 10 :: Int
      h = 1 / 60 / fromIntegral sub
      -- ★ 速度ベルレ法（エネルギーがずれにくい）で 10 回に分けて進める
      one (xs, vs) =
        let a = forces kc xs
            v1 = zipWith (\v q -> v + q * h / 2) vs a
            x1 = [if i /= g then x + v * h else x | (i, x, v) <- zip3 [0 ..] xs v1]
            a2 = forces kc x1
            v2 = [if i == g then 0 else v + q * h / 2 | (i, v, q) <- zip3 [0 ..] v1 a2]
            !x2 = strictList (map (clampF (-1.5) 1.5) x1)
            !v3 = strictList v2
        in (x2, v3)
      loop :: Int -> ([Float], [Float]) -> ([Float], [Float])
      loop 0 s = s
      loop k s = let !s' = one s in loop (k - 1) s'
      (xs2, vs2) = loop sub (wX w, wV w)
      (spacing, _, wallL, sc) = layout (wN w)
      xs3 | g >= 0 = [ if i == g then clampF (-1.2) 1.2 ((mx - (wallL + spacing * (fromIntegral i + 1))) / sc) else x
                     | (i, x) <- zip [0 :: Int ..] xs2 ]
          | otherwise = xs2
      energies = zipWith (\x v -> 0.5 * mass * v * v + 0.25 * kWall * x * x) xs3 vs2   -- おもりのエネルギーの目安
  in w { wX = xs3, wV = vs2
       , hX = zipWith pushHist (hX w) xs3
       , hE = zipWith pushHist (hE w) energies }

drawAll :: World -> Picture
drawAll w = Pictures
  [ -- 壁・ばね・おもり
    view ( Color (hsb 0 0 50) (thickLine 6 (wallL, y0 - 60) (wallL, y0 + 60))
         : Color (hsb 0 0 50) (thickLine 6 (wallR, y0 - 60) (wallR, y0 + 60))
         : springs
        ++ [Color (hsb h 70 100) (boxAt (p - 22) (y0 - 22) 44 44) | (h, p) <- zip hues massPos] )
  , Pictures [txtC p (y0 + 6) 14 (hsb 0 0 10) (show i) | (i, p) <- zip [1 :: Int ..] massPos]
    -- 基準モードの形と、いまの揺れに含まれる量（モードへの射影）
  , txt wallL (by - 14) 15 (hsb 0 0 80) ("Normal modes (keys 1-" ++ show n ++ ") and how much of each is in the motion now")
  , Pictures (zipWith3 modePanel [0 ..] (wModes w) (wOmegas w))
    -- 右：変位とエネルギーのグラフ
  , drawPlot gx (height * 0.12) gw (height * 0.38) [Series d h ("x" ++ show i) | (i, d, h) <- zip3 [1 :: Int ..] (hX w) hues] (-1.3) 1.3 "Displacement x(t)"
  , drawPlot gx (height * 0.54) gw (height * 0.36) [Series d h ("E" ++ show i) | (i, d, h) <- zip3 [1 :: Int ..] (hE w) hues]
      0 (max 1e-3 (0.5 * mass * (topOmega * 0.8) * (topOmega * 0.8))) "Mass energy (to and fro = beats)"
  , txt 20 36 18 (gray 1) ("Masses " ++ show n ++ " (Left/Right)   1-" ++ show n ++ ": mode   B: first only   W: coupling " ++ (if wWeak w then "weak" else "normal") ++ "   Drag: pull")
  , txt 20 66 14 (hsb 0 0 65) "m x'' = -K x: eigenvectors = normal modes, each with its own w. Mixed they look complex. W + B: close w's make beats."
  ]
  where
    n = wN w
    (spacing, y0, wallL, sc) = layout n
    wallR = wallL + spacing * fromIntegral (n + 1)
    massPos = [wallL + spacing * (fromIntegral i + 1) + x * sc | (i, x) <- zip [0 :: Int ..] (wX w)]
    pos = wallL : massPos ++ [wallR]
    springs =
      [ spring (a + (if i > 0 then 22 else 0)) (b - (if i < n then 22 else 0)) y0
               (if i == 0 || i == n then 1 else if wWeak w then 0.4 else 1)
      | (i, a, b) <- zip3 [0 ..] pos (drop 1 pos) ]
    by = height * 0.58
    gx = width * 0.5
    gw = width * 0.46
    topOmega = foldl' max 0 (wOmegas w)
    modePanel :: Int -> [Float] -> Float -> Picture
    modePanel k shape om =
      let bw' = width * 0.44 / fromIntegral n
          bx = wallL + fromIntegral k * bw'
          bw = bw' - 12
          mid = by + 50
          qx i = remap (fromIntegral i) (-1) (fromIntegral n) bx (bx + bw)
          -- 射影：q = Σ mode·x、q' = Σ mode·v → 振幅 √(q² + (q'/ω)²)
          q = sum (zipWith (*) shape (wX w))
          qd = sum (zipWith (*) shape (wV w))
          amp = sqrt (q * q + (qd / max 1e-6 om) * (qd / max 1e-6 om))
      in Pictures
           [ view ( Color (hsb 0 0 14) (boxAt bx by bw 190)
                  : Color (hsb 0 0 40) (Line [(bx + 8, mid), (bx + bw - 8, mid)])
                  : concat [ [ Color (hsb hue 40 80) (Line [(qx i, mid), (qx i, mid - s * 60)])
                             , Color (hsb hue 70 100) (at (qx i, mid - s * 60) (disc 6)) ]
                           | (i, s, hue) <- zip3 [0 :: Int ..] shape hues ]
                 ++ [Color (hsb 55 90 100) (boxAt (bx + 8) (by + 116) (clampF 0 1.2 amp / 1.2 * (bw - 16)) 10)] )
           , txt (bx + 6) (by + 146) 14 (hsb 0 0 80) ("Mode " ++ show (k + 1))
           , txt (bx + 6) (by + 164) 14 (hsb 0 0 80) ("w = " ++ nf 2 om)
           , txt (bx + 6) (by + 182) 14 (hsb 0 0 80) ("T = " ++ nf 2 (tau / max 1e-6 om))
           ]

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let (mx, my) = toP5 pos
        (spacing, y0, wallL, sc) = layout (wN w)
        hits = [ i | (i, x) <- zip [0 ..] (wX w)
               , abs (mx - (wallL + spacing * (fromIntegral i + 1) + x * sc)) < 26
               , abs (my - y0) < 30 ]
    in if null hits then w else w { wGrab = last hits }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wGrab = -1 }
  EventKey (SpecialKey KeyRight) Down _ _ -> excite 1 (rebuild w { wN = min (wN w + 1) 5, wGrab = -1 })
  EventKey (SpecialKey KeyLeft) Down _ _ -> excite 1 (rebuild w { wN = max (wN w - 1) 2, wGrab = -1 })
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '5' -> excite (fromEnum c - fromEnum '0') w
    | toLower c == 'b' -> kick w { wV = map (const 0) (wV w) }
    | toLower c == 'w' -> kick (rebuild w { wWeak = not (wWeak w) })
  _ -> w

-- ---------- やってみよう ----------
-- 1. おもり 2 個でモード 1（同じ向き）とモード 2（逆向き）。周期の比は？ ω₂/ω₁ = √3 になる
-- 2. W（弱い結合）で B。エネルギーが 1 → 2 → 1 と移る。行き来する周期 = 2π / (ω₂ − ω₁)
-- 3. おもり 5 個でモード 1〜5。モード番号が上がるほど「節」（動かない点）が増える → 弦の倍音と同じ（13 へ）
-- 4. 自分でドラッグして揺らす。どんな揺れも黄色い棒（モードの大きさ）は時間がたっても変わらない


-- ==================================================
-- 道具：ヤコビ法（対称行列の固有値）・ばねの絵・グラフ
-- ==================================================
type Mat = Array (Int, Int) Float

-- 非対角成分を 1 つずつ回転で消していく。戻り値は（固有値、固有ベクトルを列に並べた行列）
jacobiEigen :: Int -> Mat -> ([Float], Mat)
jacobiEigen n a0 = finish (sweeps (50 :: Int) a0 identity)
  where
    idx = [0 .. n - 1]
    pairs = [(p, q) | p <- idx, q <- idx, q > p]
    identity = listArray ((0, 0), (n - 1, n - 1)) [if i == j then 1 else 0 | i <- idx, j <- idx] :: Mat
    sweeps 0 a v = (a, v)
    sweeps s a v
      | sum [(a ! pq) * (a ! pq) | pq <- pairs] < 1e-14 = (a, v)
      | otherwise = let (a', v') = foldl' rotate (a, v) pairs in sweeps (s - 1) a' v'
    rotate (a, v) (p, q)
      | abs (a ! (p, q)) < 1e-12 = (a, v)
      | otherwise =
          let th2 = (a ! (q, q) - a ! (p, p)) / (2 * a ! (p, q))
              sg = if th2 < 0 then -1 else 1
              t = sg / (abs th2 + sqrt (th2 * th2 + 1))
              c = 1 / sqrt (t * t + 1)
              s = t * c
              cols m = m // concat [ [((k, p), c * m ! (k, p) - s * m ! (k, q)), ((k, q), s * m ! (k, p) + c * m ! (k, q))] | k <- idx ]
              rows m = m // concat [ [((p, k), c * m ! (p, k) - s * m ! (q, k)), ((q, k), s * m ! (p, k) + c * m ! (q, k))] | k <- idx ]
          in (rows (cols a), cols v)
    -- 向きをそろえる（成分の合計を正に）
    finish (a, v) =
      let flips = [ if sum [v ! (i, j) | i <- idx] < -1e-5 then -1 else 1 | j <- idx ] :: [Float]
          signed = listArray ((0, 0), (n - 1, n - 1)) [ (v ! (i, j)) * (flips !! j) | i <- idx, j <- idx ] :: Mat
      in ([a ! (i, i) | i <- idx], signed)

spring :: Float -> Float -> Float -> Float -> Picture
spring xa xb y thick = Color (hsb 0 0 75) (thickPath (1.5 + thick)
  ( (xa, y)
  : [(lerpF xa xb (fromIntegral j / 12), y + (if odd j then -10 else 10)) | j <- [1 .. 11 :: Int]]
 ++ [(xb, y)] ))

width, height :: Float
width = 1280
height = 800

-- マウスの位置（gloss の座標）を p5 と同じ座標（左上が原点、y は下向き）に変える
toP5 :: Point -> Point
toP5 (x, y) = (x + width / 2, height / 2 - y)

-- p5 と同じ座標で描いた図形を画面に置く（文字は裏返るのでここに入れない）
view :: [Picture] -> Picture
view = Translate (-width / 2) (height / 2) . Scale 1 (-1) . Pictures

-- 文字：(x, y) は左下の位置（p5 の座標）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtC, txtR :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y s c str = txt (x - fromIntegral (length str) * s * 0.36) y s c str
txtR x y s c str = txt (x - fromIntegral (length str) * s * 0.72) y s c str

-- 左上の角が (x, y) の長方形
boxAt :: Float -> Float -> Float -> Float -> Picture
boxAt x y w h = rect (x + w / 2) (y + h / 2) w h

nf :: Int -> Float -> String
nf d v = printf "%.*f" d v

-- リストの中身まで計算を済ませる（遅延評価のたまりを防ぐ）
strictList :: [Float] -> [Float]
strictList xs = foldr seq xs xs

pushHist :: Seq.Seq Float -> Float -> Seq.Seq Float
pushHist s !v =
  let s' = s Seq.|> v
  in if Seq.length s' > histLen then Seq.drop 1 s' else s'

data Series = Series
  { sData :: !(Seq.Seq Float)
  , sHue  :: !Float
  , sName :: String
  }

-- 時間プロット：枠 (x, y, w, h) に series を描く（縦の範囲 lo〜hi）
drawPlot :: Float -> Float -> Float -> Float -> [Series] -> Float -> Float -> String -> Picture
drawPlot x0 y0 w h series lo hi title = Pictures
  ( view ( Color (hsb 0 0 14) (boxAt x0 y0 w h)
         : [Color (hsb 0 0 30) (Line [(x0, yz), (x0 + w, yz)]) | yz > y0 && yz < y0 + h]
        ++ map curve series )
  : txt (x0 + 8) (y0 + 20) 14 (hsb 0 0 70) title
  : legend (x0 + w - 8) series )
  where
    yz = remap 0 lo hi (y0 + h) y0
    curve s =
      let d = toList (sData s)
          off = histLen - length d
      in Color (hsb (sHue s) 70 100) (thickPath 2
           [ ( remap (fromIntegral (i + off)) 0 (fromIntegral histLen - 1) x0 (x0 + w)
             , remap (clampF lo hi v) lo hi (y0 + h) y0 )
           | (i, v) <- zip [0 :: Int ..] d, even i ])
    legend _ [] = []
    legend lx (s : rest) = txtR lx (y0 + 20) 14 (hsb (sHue s) 70 100) (sName s)
                         : legend (lx - fromIntegral (length (sName s)) * 14 * 0.72 - 14) rest
