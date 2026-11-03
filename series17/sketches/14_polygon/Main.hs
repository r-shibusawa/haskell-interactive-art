-- ============================================
-- 14 多角形の面積と内外判定
-- 面積：基準点 O と各辺でできる三角形の「符号つき面積」（外積 ÷ 2）を足すだけ（靴ひも公式）
--       左回りの辺は +（緑）、右回りの辺は −（赤）。はみ出た部分が打ち消し合う
-- 内外：マウスの点から右へ半直線をのばし、辺と交わる回数が奇数なら内側、偶数なら外側
-- 操作：クリック = 頂点を追加 / ドラッグ = 頂点や O を動かす / C = 全部消す
--       スペース = 三角形を 1 つずつ足すアニメーション⇄全部 / 1〜3 = お手本（星・くし形・うず巻き）
-- ============================================
module Main (main) where

import Art
import Numeric (showFFloat)

data Grab = NoGrab | GrabO | GrabV !Int deriving Eq

data World = World
  { wPoly :: ![Point]
  , wO    :: !Point    -- 面積を計算する基準点（どこに置いても答えは同じ）
  , wAnim :: !Bool
  , wDrag :: !Grab
  }

main :: IO ()
main = runSketch (sketch "17-14 Polygon area and inside test" (World [] (0, 0) True NoGrab))
  { skBack  = cBg
  , skStart = \env -> pure (preset env 1 (World [] (0, 0) True NoGrab))
  , skDraw  = drawW
  , skEvent = onEvent
  }

preset :: Env -> Int -> World -> World
preset env k w =
  let bigW = envW env - 300
      cx = bigW / 2
      cy = envH env * 0.56
      r = min bigW (envH env) * 0.33
      polar a q = (cx + cos a * q, cy + sin a * q)
      ps = case k of
        2 -> -- くし形（へこみが多い）
          zipWith (\x y -> (cx + x * r, cy + y * r))
            [-1, -1, -0.6, -0.6, -0.2, -0.2, 0.2, 0.2, 0.6, 0.6, 1, 1]
            [0.8, -0.8, -0.8, 0.3, 0.3, -0.8, -0.8, 0.3, 0.3, -0.8, -0.8, 0.8]
        3 -> [ polar (fi * 0.5) (r * (0.15 + fi * 0.033)) | i <- [0 .. 25 :: Int], let fi = fromIntegral i ]
        _ -> [ polar (-pi / 2 + fromIntegral i * tau / 10) (if odd i then r * 0.45 else r) | i <- [0 .. 9 :: Int] ]
  in w { wPoly = ps, wO = (cx - r * 1.2, cy + r * 0.9) }

-- 画面は y が下向きなので、y を反転して計算（+ = 左回り）
turn :: Point -> Point -> Point -> Float
turn (ox, oy) (ax, ay) (bx, by) = (ax - ox) * (oy - by) - (oy - ay) * (bx - ox)

-- 辺の一覧（最後の頂点から最初の頂点へ戻る辺も入れる）
edges :: [Point] -> [(Point, Point)]
edges ps = zip ps (drop 1 ps ++ take 1 ps)

drawW :: Env -> World -> Picture
drawW env w =
  let ww = envW env
      poly' = wPoly w
      n = length poly'
      o = wO w
      shownTri = if wAnim w then (envFrame env `div` 20) `mod` (n + 8) else n   -- 何枚目の三角形まで表示するか
      es = if n >= 3 then edges poly' else []
      shownEs = take shownTri es
      area (a, b) = turn o a b / 2
      sumShown = sum (map area shownEs)
      total = sum (map area es)
      -- 符号つき三角形
      triPic e@(a, b) =
        let c = if area e > 0 then cMint else cBad
        in Pictures [fillPoly env (withA 0.18 c) [o, a, b], Color c (Line [pt env o, pt env a])]
      -- 多角形
      outlinePic = path env cOrchid 3 (if n >= 3 then poly' ++ take 1 poly' else poly')
      vertexPic (i, p@(x, y)) = Pictures
        [ dot env cOrchid p 11, txt env (x + 7) (y - 21) 14 cDim (show (i :: Int)) ]
      (ox, oy) = o
      -- 内外判定（マウスから右へ）
      m@(mx, my) = mouse env
      insidePic
        | n >= 3 && mx < ww - 300 && my > 90 =
            let crossings =
                  [ x | ((ax, ay), (bx, by)) <- edges poly'
                      , (ay > my) /= (by > my)     -- 辺がマウスの高さをまたぐ
                      , let x = ax + (my - ay) * (bx - ax) / (by - ay)
                      , x > mx ]
                count = length crossings
                inside = odd count
            in Pictures
                 [ dashed env (if inside then cMint else cDim) 2 6 m (ww - 300, my)
                 , Pictures [dot env cGold (x, my) 10 | x <- crossings]
                 , dot env (if inside then cMint else cBad) m 14
                 , txt env (mx + 12) (my - 27) 15 (if inside then cMint else cBad)
                     (show count ++ " crossing(s) -> " ++ (if inside then "inside" else "outside")) ]
        | otherwise = Blank
      -- パネル
      px = ww - 280
      r0 v = show (round v :: Int)
      lines' =
        [ "Vertices " ++ show n
        , "Triangles " ++ show (min shownTri n) ++ " / " ++ show n
        , "Sum so far " ++ r0 sumShown
        , ""
        , "Area = |" ++ r0 total ++ "| = " ++ r0 (abs total)
        , "Direction: " ++ (if total > 0 then "ccw (+)" else if total < 0 then "cw (-)" else "-") ]
  in Pictures
       [ Pictures (map triPic shownEs)
       , outlinePic
       , Pictures (map vertexPic (zip [0 ..] poly'))
       , dot env cGold o 14
       , txt env (ox + 10) (oy + 4 - 14) 14 cFg "O"
       , insidePic
       , box env cPanel px 20 260 270
       , txt env (px + 18) 36 17 cFg "Shoelace formula"
       , Pictures [ txt env (px + 18) (70 + fromIntegral k * 22) 15 cFg l | (k, l) <- zip [0 :: Int ..] lines' ]
       , txt env (px + 18) 250 14 cDim "Moving O keeps the area"
       , txt env 30 24 22 cFg "Polygon area and inside test"
       , txt env 30 56 14 cDim "Click add vertex  Drag move (gold = O)  C clear  Space animate/all  1-3 examples"
       , if n < 3 then txt env 30 84 16 cBad "Click at least 3 vertices" else Blank
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | mx > envW env - 290 -> w
    | otherwise ->
        let onO = if dist2 m (wO w) < 14 then GrabO else NoGrab
            grab = foldl (\g (i, p) -> if dist2 m p < 12 then GrabV i else g) onO (zip [0 ..] (wPoly w))
        in if grab == NoGrab && my > 100
             then w { wDrag = NoGrab, wPoly = wPoly w ++ [m] }
             else w { wDrag = grab }
  EventKey (MouseButton LeftButton) Up _ _ -> w { wDrag = NoGrab }
  EventMotion _ -> case wDrag w of
    GrabO -> w { wO = m }
    GrabV i -> w { wPoly = [if k == i then m else p | (k, p) <- zip [0 ..] (wPoly w)] }
    NoGrab -> w
  EventResize _ -> preset env 1 w { wDrag = NoGrab }
  EventKey (SpecialKey KeySpace) Down _ _ -> w { wAnim = not (wAnim w) }
  EventKey (Char c) Down _ _
    | c `elem` "cC" -> w { wPoly = [], wDrag = NoGrab }
    | c `elem` "123" -> preset env (fromEnum c - fromEnum '0') w { wDrag = NoGrab }
  _ -> w
  where m@(mx, my) = mouse env

-- ---------- やってみよう ----------
-- 1. O をいろいろな場所へ動かして、面積が変わらないことを確かめる
-- 2. お手本 3（うず巻き）で、マウスを動かして交わる回数の変化を見る
-- 3. 頂点を逆回りにクリックすると、合計の符号はどうなる？

-- ---------- 道具：色 ----------
cBg, cPanel, cFg, cDim, cOrchid, cMint, cGold, cBad, cEdge, cGreen, cRed, cAmber, cBlue :: Color
cBg     = rgb255 0x14 0x10 0x1f
cPanel  = rgb255 0x22 0x1a 0x33
cFg     = rgb255 0xe9 0xe4 0xf5
cDim    = rgb255 0x8c 0x84 0xa3
cOrchid = rgb255 0xf0 0xa3 0xff
cMint   = rgb255 0x6f 0xe3 0xd0
cGold   = rgb255 0xff 0xcf 0x6b
cBad    = rgb255 0xff 0x6b 0x81
cEdge   = rgb255 0x3a 0x2f 0x52
cGreen  = rgb255 0x2f 0x5e 0x57
cRed    = rgb255 0x5e 0x2a 0x3a
cAmber  = rgb255 0x5a 0x4a 0x1f
cBlue   = rgb255 0x2f 0x4a 0x5e

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置を p5.js の座標で
mouse :: Env -> Point
mouse env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶし（p5.js の rect）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = Color c (rect (gx env (x + w / 2)) (gy env (y + h / 2)) w h)

-- 太さ t の枠
outline :: Env -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
outline env c t x y w h = Color c (thickPath t (map (pt env) [(x - t / 2, y), (x + w + t / 2, y), (x + w, y), (x + w, y + h), (x + w + t / 2, y + h), (x - t / 2, y + h), (x, y + h), (x, y)]))

-- 太さ t の線分（p5.js の line）と折れ線
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

path :: Env -> Color -> Float -> [Point] -> Picture
path env c t ps = Color c (thickPath t (map (pt env) ps))

-- 点線
dashed :: Env -> Color -> Float -> Float -> Point -> Point -> Picture
dashed env c t d a@(x1, y1) b@(x2, y2) =
  let l = dist2 a b
      k = floor (l / (2 * d)) :: Int
      f s = (x1 + (x2 - x1) * s / max l 1e-6, y1 + (y2 - y1) * s / max l 1e-6)
  in Pictures [seg env c t (f (2 * d * fromIntegral i)) (f (min l (2 * d * fromIntegral i + d))) | i <- [0 .. k]]

-- 直径 d の円（p5.js の circle）と、太さ t の輪
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

hoop :: Env -> Color -> Float -> Point -> Float -> Picture
hoop env c t p d = Color c (at (pt env p) (ring (d / 2) t))

-- 塗りつぶしの多角形（凸形のみ）
fillPoly :: Env -> Color -> [Point] -> Picture
fillPoly env c ps = Color c (Polygon (map (pt env) ps))

-- 文字：txt は左上が (x, y)、txtM は左端で上下中央、txtC は中心が (x, y)
txt, txtM, txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env (y + s)) s c str
txtM env x y s c str = label (gx env x) (gy env (y + s / 2)) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str

-- 小数点以下 d 桁（JavaScript の toFixed）
fix :: Int -> Float -> String
fix d v = showFFloat (Just d) v ""

