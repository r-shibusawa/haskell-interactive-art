-- ============================================
-- 40-14 フレネ・ブラシ ─ 接線と法線で描く筆
-- ドラッグした軌跡の各点で接線 T と法線 N と曲率 κ を計算し、N の方向にリボンや羽を描く
--   幅は曲率で変わる（急カーブで太く or 細く）、色は κ の符号（左曲がり = 暖色、右曲がり = 寒色）
--   κ は直前の 3 点から：κ = 4 × 三角形の面積 / (3 辺の長さの積)（メンガー曲率）
-- 操作：ドラッグ = 描く / 1〜4 = ブラシ / ↑↓ = 太さ / F = 急カーブで太く ↔ 細く / B = 紙の色 / C = クリア / S = 保存
-- ============================================
module Main (main) where

import Art
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Numeric (showFFloat)

baseW0 :: Float
baseW0 = 14               -- ★ 基本の太さ

smoothing :: Float
smoothing = 0.35          -- ★ 手ぶれを抑える（小さいほどなめらか）

kGain :: Float
kGain = 25                -- ★ 曲率が幅に効く強さ

maxInk :: Int
maxInk = 7000             -- ★ 紙に残す図形の数（超えると古いものから消える）

names :: [String]
names = ["Ribbon", "Feather", "Comb (T and N)", "Osculating circle"]

data World = World
  { wBrush :: !Int
  , wBaseW :: !Float
  , wFat, wDark :: !Bool
  , wInk :: !(Seq.Seq Picture)      -- 紙に描いた図形（古い順）
  , wSp :: !(Maybe Point)           -- なめらかにした筆先の位置
  , wTrail :: ![Point]              -- 直前の点（新しい順、最大 4 個）
  , wCursor :: ![Point]             -- 筆先の案内用のマウス履歴（新しい順、最大 3 個）
  , wLastK, wLastW :: !Float
  , wRibbonW :: !(Maybe Float)
  , wFeather :: !Int }

main :: IO ()
main = runSketch (sketch "40-14 Frenet brush" world0)
  { skBack  = hsb 230 20 8
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World 1 baseW0 True True Seq.empty Nothing [] [] 0 0 Nothing 0

-- | 接線・法線・曲率
data Fr = Fr { frK, frTx, frTy, frNx, frNy :: !Float }

-- 3 点 a, b, c から接線 T、左向きの法線 N、メンガー曲率 κ を求める
frenet2D :: Point -> Point -> Point -> Maybe Fr
frenet2D (ax, ay) (bx, by) (cx, cy) =
  let (abx, aby) = (bx - ax, by - ay)
      (bcx, bcy) = (cx - bx, cy - by)
      (acx, acy) = (cx - ax, cy - ay)
      cr = abx * bcy - aby * bcx                 -- 外積（符号 = 曲がる向き）
      la = norm2 (abx, aby)
      lb = norm2 (bcx, bcy)
      lc = norm2 (acx, acy)
      k = if la * lb * lc > 1e-9 then negate (2 * cr) / (la * lb * lc) else 0   -- p5 の座標は y 下向きなので符号を反転して左 = +
      tx = acx / lc
      ty = acy / lc
  in if lc < 1e-6 then Nothing else Just (Fr k tx ty ty (negate tx))

-- 紙に図形を足す（増えすぎたら古いものから消す）
addInk :: [Picture] -> World -> World
addInk ps w =
  let ink = foldl (Seq.|>) (wInk w) ps
      over = Seq.length ink - maxInk
  in w { wInk = if over > 0 then Seq.drop over ink else ink }

-- 直前の 3 点から T, N, κ を求めて、1 スタンプ描く
stamp :: Env -> World -> World
stamp env w = case wTrail w of
  (c : b : a : _) -> case frenet2D a b c of
    Just f -> stampAt env a b c f w
    Nothing -> w
  _ -> w

stampAt :: Env -> Point -> Point -> Point -> Fr -> World -> World
stampAt env _ b@(bx, by) c@(cx, cy) f w =
  let k = frK f
      (tx, ty, nx, ny) = (frTx f, frTy f, frNx f, frNy f)
      kk = min 1 (abs k * kGain)
      bw = wBaseW w
      wd = if wFat w then bw * (0.4 + 1.6 * kk) else bw * (1.6 - 1.4 * kk)
      hue = if k > 0 then 20 + 20 * kk else 200 + 40 * kk
      sat = 35 + 60 * kk
      fc = wFeather w
      w1 = w { wLastK = k, wLastW = wd }
      sgn = if k > 0 then 1 else -1
  in case wBrush w of
       1 -> -- リボン：前の点と今の点の ± N を四角でつなぐ
         let pw = maybe wd id (wRibbonW w)
             quad = Color (hsba hue sat 95 0.85) (Polygon (map (toG env)
                      [ (bx + nx * pw, by + ny * pw), (cx + nx * wd, cy + ny * wd)
                      , (cx - nx * wd, cy - ny * wd), (bx - nx * pw, by - ny * pw) ]))
         in addInk [quad] w1 { wRibbonW = Just wd }
       2 -> -- 羽：法線方向に短い線を左右交互に、少し後ろへ流す
         let side = fromIntegral (fc `mod` 2) * 2 - 1
             l = wd * 2.2
             tip = (cx + (nx * side - tx * 0.6) * l, cy + (ny * side - ty * 0.6) * l)
         in addInk [ seg2 env (hsba hue sat 100 0.7) 1.5 c tip
                   , dotAt env (hsba 0 0 100 0.6) 1.25 c ] w1 { wFeather = fc + 1 }
       3 -> -- くし：T（白）と N（色）の小さな標構
         let w2 = w1 { wFeather = fc + 1 }
         in if fc `mod` 3 /= 0 then w2 else
            addInk [ seg2 env (hsba 0 0 100 0.8) 2 c (cx + tx * wd * 1.2, cy + ty * wd * 1.2)
                   , seg2 env (hsba hue sat 100 0.9) 2 c (cx + nx * wd * sgn, cy + ny * wd * sgn)
                   , dotAt env white 2 c ] w2
       _ -> -- 接触円：半径 1/|κ| の薄い円を曲率中心に
         let r = min (1 / max (abs k) 1e-9) 400
             w2 = if r < 400 then w1 { wFeather = fc + 1 } else w1
             circ = [ ringAt env (hsba hue sat 100 0.18) r 1 (cx + nx * r * sgn, cy + ny * r * sgn) | r < 400 && even fc ]
         in addInk (circ ++ [dotAt env (hsba 0 0 100 0.9) 1.5 c]) w2

step :: Env -> World -> World
step env w0 =
  let m@(mx, my) = fromG env (envMouse env)
      w1 = w0 { wCursor = take 3 (m : wCursor w0) }
  in if envDown env && my > 90
       then let (sx0, sy0) = maybe m id (wSp w1)
                sp@(sx, sy) = (sx0 + (mx - sx0) * smoothing, sy0 + (my - sy0) * smoothing)
                w2 = w1 { wSp = Just sp }
                far = case wTrail w2 of
                  (l : _) -> dist2 (sx, sy) l > 2.5
                  [] -> True
            in if far then stamp env w2 { wTrail = take 4 (sp : wTrail w2) } else w2
       else w1

draw :: Env -> World -> Picture
draw env w = Pictures (paper ++ toList (wInk w) ++ guide ++ texts)
  where
    (mx, my) = fromG env (envMouse env)
    paper = [box env 0 0 (envW env) (envH env) (paperColor w)]
    v = if wDark w then 100 else 20
    -- 筆先の案内：押していないときも、マウスの動きから T と N と κ を計算して小さな標構を描く
    guide = case wCursor w of
      [c, b, a] | not (envDown env) -> case frenet2D a b c of
        Just f ->
          let sgn = if frK f > 0 then 1 else -1
          in [ seg2 env (hsba 0 0 v 0.7) 2 (mx, my) (mx + frTx f * 30, my + frTy f * 30)
             , seg2 env (hsba (if frK f > 0 then 25 else 200) 80 100 0.9) 2 (mx, my) (mx + frNx f * 30 * sgn, my + frNy f * 30 * sgn)
             , ringAt env (hsba 45 90 100 0.4) (wBaseW w) 2 (mx, my) ]
        Nothing -> []
      _ -> []
    c1 = hsb 0 0 (if wDark w then 100 else 15)
    c2 = hsb 0 0 (if wDark w then 65 else 40)
    nameNow = case drop (wBrush w - 1) names of { (s : _) -> s; [] -> "" }
    texts =
      [ txt env 20 34 16 c1 ("drag: draw   1-4: brush (" ++ nameNow ++ ")   Up/Down: width " ++ nf 0 (wBaseW w) ++ "   F: " ++ (if wFat w then "fat" else "thin") ++ " on curves")
      , txt env 20 56 16 c1 "B: paper color   C: clear   S: save"
      , txt env 20 78 14 c2 ("warm = left (k > 0)  cool = right (k < 0)  last k = " ++ nf 4 (wLastK w) ++ "  width = " ++ nf 1 (wLastW w) ++ " px")
      ]

paperColor :: World -> Color
paperColor w = if wDark w then hsb 230 20 8 else hsb 45 8 97

resetStroke :: World -> World
resetStroke w = w { wTrail = [], wSp = Nothing, wRibbonW = Nothing }

event :: Env -> Event -> World -> IO World
event env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (resetStroke w)
  EventKey (MouseButton LeftButton) Up _ _ -> pure (resetStroke w)
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '4' -> pure w { wBrush = fromEnum c - fromEnum '0' }
    | c == 'f' || c == 'F' -> pure w { wFat = not (wFat w) }
    | c == 'b' || c == 'B' -> pure w { wDark = not (wDark w), wInk = Seq.empty }
    | c == 'c' || c == 'C' -> pure w { wInk = Seq.empty }
    | c == 's' || c == 'S' -> saveSvg "frenet_brush" env (draw env w) >> pure w
  EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wBaseW = min 60 (wBaseW w + 2) }
  EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wBaseW = max 2 (wBaseW w - 2) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. ゆっくり大きな円を描くと一色、小さな渦を描くと濃い色になる。κ の大きさが色の濃さ
-- 2. 「の」の字を描くと左右どちらの色？ 逆向きに書くと？（κ の符号 = 回る向き）
-- 3. 羽（2）の長さに sin (envTime env) を掛けて揺らぐ筆にしよう。接触円（4）の透明度を κ で変えよう
-- 4. 13 の音と合体して、描きながら曲率の音を鳴らす「歌う筆」にしよう

-- ==================================================
-- ここから下は「座標と文字の道具」です
-- （中身は読まなくても OK。消さないでね）
-- ==================================================

-- | p5.js の座標（左上が原点、y は下向き）→ gloss の座標（中央が原点、y は上向き）
toG :: Env -> Point -> Point
toG env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- | gloss の座標 → p5.js の座標（マウスの位置を読むときに使う）
fromG :: Env -> Point -> Point
fromG env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- | p5.js の座標で文字を書く
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = let (gx, gy) = toG env (x, y) in label gx gy s c str

-- | p5.js の座標で長方形を塗る（(x, y) は左上）
box :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h c = let (gx, gy) = toG env (x + w / 2, y + h / 2) in Color c (rect gx gy w h)

-- | p5.js の座標で線分を引く（w は太さ、1 以下なら細い線）
seg2 :: Env -> Color -> Float -> Point -> Point -> Picture
seg2 env c w a b
  | w <= 1 = Color c (Line [toG env a, toG env b])
  | otherwise = Color c (thickLine w (toG env a) (toG env b))

-- | p5.js の座標で折れ線を引く（色は 1 色）
path2 :: Env -> Color -> Float -> [Point] -> Picture
path2 env c w ps
  | w <= 1 = Color c (Line (map (toG env) ps))
  | otherwise = Color c (thickPath w (map (toG env) ps))

-- | p5.js の座標で点（塗りつぶした円）を打つ。r は半径
dotAt :: Env -> Color -> Float -> Point -> Picture
dotAt env c r p = at (toG env p) (Color c (disc r))

-- | p5.js の座標で輪を描く。r は半径、w は線の太さ
ringAt :: Env -> Color -> Float -> Float -> Point -> Picture
ringAt env c r w p
  | w <= 1 = at (toG env p) (Color c (Circle r))
  | otherwise = at (toG env p) (Color c (ring r w))

-- | 小数 n 桁の文字列（p5.js の nf に相当）
nf :: Int -> Float -> String
nf n x = showFFloat (Just n) x ""
