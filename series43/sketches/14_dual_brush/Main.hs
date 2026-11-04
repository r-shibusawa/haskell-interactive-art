-- ============================================
-- 43-14 双対ブラシ
-- 左に描いた「点」の軌跡が、右では「直線」の束になって現れる（00 の双対変換をブラシに）
--   どちらのパネルに描いても、相手側に双対が描かれる。色は時間で変わり、線は重なって光る
--   M でモード：アフィン（点 (a,b) → 直線 y = ax − b）／ 極（点 p → 直線 ⟨p, x⟩ = 1）
-- 操作：ドラッグ = 描く / M = モード / ↑↓ = 線の濃さ / C = 全部消す / S = 保存
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

rangeR :: Float
rangeR = 3               -- ★ 数学座標の範囲 ±rangeR

maxInk :: Int
maxInk = 3500            -- ★ パネルごとに覚えておく線の数（超えると古いものから消える）

data World = World
  { wL     :: !(Seq Picture)  -- 左のパネルの絵（パネルの左上が原点）
  , wR     :: !(Seq Picture)  -- 右のパネルの絵
  , wMode  :: !Int            -- 0 = アフィン, 1 = 極
  , wAlpha :: !Float          -- ★ 直線の濃さ（薄いほど重なりがきれい）
  , wHue   :: !Float
  , wPrev  :: !Point          -- 前のマウス位置（pmouseX, pmouseY の代わり）
  }

main :: IO ()
main = runSketch (sketch "43-14 Dual brush" (World Seq.empty Seq.empty 0 14 0 (0, 0)))
  { skBack  = hsb 250 40 10
  , skDraw  = draw
  , skEvent = event
  }

panels :: Env -> (Panel, Panel)
panels env =
  let w = (envW env - 60) / 2
      h = envH env - 150
  in (Panel 20 100 w h, Panel (40 + w) 100 w h)

-- パネル内の画面座標 ↔ 数学座標（パネルの左上を原点とした px で扱う）
toM :: Panel -> Point -> Point
toM p (sx, sy) = ((sx - px p) / pw p * 2 * rangeR - rangeR, rangeR - (sy - py p) / ph p * 2 * rangeR)

toLocal :: Panel -> Float -> Float -> Point
toLocal p x y = ((x + rangeR) / (2 * rangeR) * pw p, (rangeR - y) / (2 * rangeR) * ph p)

-- ★ 点 p の双対直線の 2 端点（パネルの左上が原点）
dualLine :: Int -> Panel -> Point -> Maybe (Point, Point)
dualLine mode q (x, y)
  | mode == 0 =
      -- y = p.x · x − p.y
      Just (toLocal q (-rangeR) (x * (-rangeR) - y), toLocal q rangeR (x * rangeR - y))
  | d2 < 1e-4 = Nothing
  | otherwise =
      -- ⟨p, x⟩ = 1：足 p/|p|²、向き p に直交
      let fx = x / d2
          fy = y / d2
          tx = -y / sqrt d2
          ty = x / sqrt d2
      in Just (toLocal q (fx + tx * 20) (fy + ty * 20), toLocal q (fx - tx * 20) (fy - ty * 20))
  where d2 = x * x + y * y

-- 古い線を消して上限を守る
pushInk :: [Picture] -> Seq Picture -> Seq Picture
pushInk ps s =
  let s' = foldl' (|>) s ps
  in Seq.drop (Seq.length s' - maxInk) s'

draw :: Env -> World -> Picture
draw env w = view env $
  [ Translate (px l) (py l) (Pictures (toList (wL w)))
  , Translate (px r) (py r) (Pictures (toList (wR w)))
  , frameOf l, frameOf r ]
  -- 軸（薄く）
  ++ concat [ [ seg 1 axis (px p, py p + ph p / 2) (px p + pw p, py p + ph p / 2)
              , seg 1 axis (px p + pw p / 2, py p) (px p + pw p / 2, py p + ph p) ]
              ++ [ Color axis (Translate (px p + pw p / 2) (py p + ph p / 2) (Circle (pw p / rangeR / 2))) | wMode w == 1 ]
            | p <- [l, r] ]
  ++ preview
  ++ [ txt 20 32 18 (hsb 0 0 100) (modeName (wMode w) ++ " (M)   ink " ++ show (round (wAlpha w) :: Int) ++ " (Up/Down)   C: clear   S: save")
     , txt 20 58 14 (hsb 0 0 60) "Draw a curve on the left and its dual appears on the right as an envelope of lines. Try a circle, try a straight line"
     , txt 20 78 14 (hsb 0 0 60) "(the duals of points on a line pass through one point)." ]
  where
    (l, r) = panels env
    axis = hsbA 0 0 100 10
    frameOf p = box (hsb 0 0 35) (px p) (py p) (pw p) (ph p)
    m@(mx, my) = mouseP env
    onL = inside l m
    onR = inside r m
    -- マウスの双対をプレビュー（点線）
    preview
      | onL || onR =
          let (p0, q) = if onL then (l, r) else (r, l)
              (x, y) = toM p0 m
              -- 原点ちょうどのときも線が出るよう、p5.js 版と同じく小さな値にかえる
              (x', y') = if wMode w == 1 && x * x + y * y < 1e-4 then (0.01, 0) else (x, y)
              line0 = case dualLine (wMode w) q (x', y') of
                Just ((ax, ay), (bx, by)) ->
                  case clipSeg q (px q + ax, py q + ay) (px q + bx, py q + by) of
                    Just (c1, c2) -> dashPath 1 (hsbA 0 0 100 40) 4 6 [c1, c2]
                    Nothing -> Blank
                Nothing -> Blank
          in [ line0, circ 1 (hsbA 0 0 100 60) mx my 10 ]
      | otherwise = []

modeName :: Int -> String
modeName 0 = "Affine: point (a, b) <-> line y = ax - b"
modeName _ = "Polar: point p <-> line <p, x> = 1 (origin is the pole)"

-- ドラッグ：自分のパネルに軌跡、相手のパネルに双対の直線を足す
paint :: Env -> World -> World
paint env w
  | not onL && not onR = w
  | otherwise =
      let (p0, q) = if onL then (l, r) else (r, l)
          hue = wrapF 360 (wHue w + 0.8)
          pt = toM p0 m
          prevM = if inside p0 (wPrev w) then toM p0 (wPrev w) else pt
          (ax, ay) = uncurry (toLocal p0) pt
          (bx, by) = uncurry (toLocal p0) prevM
          trail = Color (hsbA hue 70 100 90) (thickLine 3 (ax, ay) (bx, by))
          -- 2 点の間を補間して直線を何本か引く（なめらかな包絡線になる）
          n = max 1 (min 6 (floor (dist2 m (wPrev w) / 3))) :: Int
          box0 = Panel 0 0 (pw q) (ph q)
          lineAt k =
            let t = fromIntegral k / fromIntegral n
                mid = (lerpF (fst prevM) (fst pt) t, lerpF (snd prevM) (snd pt) t)
            in case dualLine (wMode w) q mid of
                 Just (a, b) | Just (c1, c2) <- clipSeg box0 a b ->
                   [Color (hsbA hue 70 100 (wAlpha w)) (Line [c1, c2])]
                 _ -> []
          duals = concatMap lineAt [1 .. n]
      in if onL
           then w { wHue = hue, wL = pushInk [trail] (wL w), wR = pushInk duals (wR w) }
           else w { wHue = hue, wR = pushInk [trail] (wR w), wL = pushInk duals (wL w) }
  where
    (l, r) = panels env
    m = mouseP env
    onL = inside l m
    onR = inside r m

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventMotion _ ->
    let w' = if envDown env then paint env w else w
    in pure w' { wPrev = mouseP env }
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wPrev = mouseP env }
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wAlpha = min 60 (wAlpha w + 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wAlpha = max 2 (wAlpha w - 2) }
  EventKey (Char k) Down _ _
    | k `elem` "mM" -> pure w { wMode = 1 - wMode w }
    | k `elem` "cC" -> pure w { wL = Seq.empty, wR = Seq.empty }
    | k `elem` "sS" -> saveSvg "dual_brush" env (draw env w) >> pure w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 左にまっすぐな線を引く → 右の直線は 1 点に集まる（共点）。左に円 → 右に？（極モードでは円）
-- 2. 右に描くと左に直線が出る。双対の双対は元に戻るので、左右どちらで描いても同じ「言語」
-- 3. 線の濃さを下げて、ゆっくり大きく描く。包絡線がくっきりする条件は？
-- 4. 色を「位置」で決める（hue = p.x に応じて）、線の太さを「原点からの距離」で決める、などに改造

-- ---------- 道具 ----------
-- p5.js と同じ座標（左上が原点、y は下向き）で描く
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置を p5.js の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左下が (x, y) の文字（view の中で上下が逆にならないよう戻す）
txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y size c s = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 透明度を 0〜100 で書く HSB（p5.js の colorMode(HSB, 360, 100, 100, 100)）
hsbA :: Float -> Float -> Float -> Float -> Color
hsbA h s b a = hsba h s b (a / 100)

-- 左上が (x, y) の長方形の枠
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)])

-- 太さ w の線分
seg :: Float -> Color -> Point -> Point -> Picture
seg w c a b = Color c (if w <= 1 then Line [a, b] else thickLine w a b)

-- 点線の折れ線（on だけ描いて off だけ空ける）
dashPath :: Float -> Color -> Float -> Float -> [Point] -> Picture
dashPath w c on off pts = Pictures (walk on True pts)
  where
    walk left drawing (a : b : rest)
      | l <= left = [seg w c a b | drawing] ++ walk (left - l) drawing (b : rest)
      | otherwise = [seg w c a m | drawing] ++ walk (if drawing then off else on) (not drawing) (m : b : rest)
      where
        l = dist2 a b
        t = left / l
        m = (lerpF (fst a) (fst b) t, lerpF (snd a) (snd b) t)
    walk _ _ _ = []

-- 中心 (x, y)、直径 d の輪
circ :: Float -> Color -> Float -> Float -> Float -> Picture
circ w c x y d = Color c (Translate x y (if w <= 1 then Circle (d / 2) else ThickCircle (d / 2) w))

-- 画面の枠（左上の x, y、幅、高さ）
data Panel = Panel { px :: !Float, py :: !Float, pw :: !Float, ph :: !Float }

inside :: Panel -> Point -> Bool
inside p (x, y) = x >= px p && x <= px p + pw p && y >= py p && y <= py p + ph p

-- 線分を枠で切る（Liang–Barsky）
clipSeg :: Panel -> Point -> Point -> Maybe (Point, Point)
clipSeg p (x1, y1) (x2, y2) = go 0 1 tests
  where
    dx = x2 - x1
    dy = y2 - y1
    tests = [(-dx, x1 - px p), (dx, px p + pw p - x1), (-dy, y1 - py p), (dy, py p + ph p - y1)]
    go t0 t1 [] = Just ((x1 + t0 * dx, y1 + t0 * dy), (x1 + t1 * dx, y1 + t1 * dy))
    go t0 t1 ((a, q) : rest)
      | a == 0 = if q < 0 then Nothing else go t0 t1 rest
      | a < 0 = if r > t1 then Nothing else go (max t0 r) t1 rest
      | otherwise = if r < t0 then Nothing else go t0 (min t1 r) rest
      where r = q / a
