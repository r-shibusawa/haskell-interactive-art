-- ============================================
-- 00 対称性を見つける
-- 図形を動かしても「ぴったり重なる」動かし方 = 対称性。回転と鏡映（裏返し）がある
--   正 n 角形には回転 n 個 + 鏡映 n 個 = 2n 個の対称性がある（二面体群 Dn）
-- 1〜4 = 図形（正多角形・雪の結晶・風車・文字）/ ↑↓ = 角の数・文字 / R = 回す / F = 鏡に映す
-- ←→ = 鏡の軸をえらぶ / 0 = もとにもどす
-- ============================================
module Main (main) where

import Art

-- 2×2 行列 [a b; c d]
data M2 = M2 !Float !Float !Float !Float

-- アニメーション中の動き（回転か鏡映か）
data Anim = Anim
  { aRot :: !Bool     -- True なら回転、False なら鏡映
  , aT   :: !Float    -- 進み具合 0〜1
  , aK0  :: !Int      -- 動かす前の k
  , aF0  :: !Int      -- 動かす前の f
  , aPhi :: !Float    -- 鏡の軸の向き
  }

data World = World
  { wShape  :: !Int           -- 図形の番号 1〜4
  , wN      :: !Int           -- 角の数（回転の対称性の数）
  , wK      :: !Int           -- 今の対称性 = r^k s^f（r：回転、s：鏡映）
  , wF      :: !Int
  , wAxis   :: !Int           -- えらんでいる鏡の軸
  , wAnim   :: !(Maybe Anim)
  , wLetter :: !Int           -- 文字の番号
  }

-- 今の図形の「回転の数 n・鏡があるか・最初の鏡の向き」
data Info = Info !Int !Bool !Float

-- 文字：線分の列、回転の数 n、鏡があるか、鏡の向き（ラジアン）
data Letter = Letter String [(Float, Float, Float, Float)] !Int !Bool !Float

r0 :: Float
r0 = 210                 -- ★ 図形の大きさ

letters :: [Letter]      -- ★ 文字を足せる
letters =
  [ Letter "A" [(-1, 1, 0, -1), (0, -1, 1, 1), (-0.5, 0.2, 0.5, 0.2)] 1 True (pi / 2)
  , Letter "H" [(-1, -1, -1, 1), (1, -1, 1, 1), (-1, 0, 1, 0)] 2 True 0
  , Letter "N" [(-1, 1, -1, -1), (-1, -1, 1, 1), (1, 1, 1, -1)] 2 False 0
  , Letter "S" [(1, -1, -1, -1), (-1, -1, -1, 0), (-1, 0, 1, 0), (1, 0, 1, 1), (1, 1, -1, 1)] 2 False 0
  , Letter "Z" [(-1, -1, 1, -1), (1, -1, -1, 1), (-1, 1, 1, 1)] 2 False 0
  , Letter "X" [(-1, -1, 1, 1), (1, -1, -1, 1)] 2 True (pi / 4)
  , Letter "E" [(1, -1, -1, -1), (-1, -1, -1, 1), (-1, 1, 1, 1), (-1, 0, 0.6, 0)] 1 True 0
  , Letter "F" [(1, -1, -1, -1), (-1, -1, -1, 1), (-1, 0, 0.6, 0)] 1 False 0
  ]

main :: IO ()
main = runSketch (sketch "23-00 Finding Symmetry" (World 1 5 0 0 0 Nothing 0))
  { skBack  = rgb255 250 247 240
  , skStep  = \_ _ w -> pure (stepAnim w)
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

currentLetter :: World -> Letter
currentLetter w = letters !! (wLetter w `mod` length letters)

info :: World -> Info
info w = case wShape w of
  1 -> Info (wN w) True (-pi / 2)
  2 -> Info 6 True (-pi / 2)
  3 -> Info (wN w) False 0
  _ -> let Letter _ _ n m ph = currentLetter w in Info n m ph

stepAnim :: World -> World
stepAnim w = case wAnim w of
  Just a | aT a >= 1 -> w { wAnim = Nothing }
         | otherwise -> w { wAnim = Just a { aT = min 1 (aT a + 0.045) } }
  Nothing -> w

drawWorld :: World -> Picture
drawWorld w = Pictures
  ( drawShape w 512 440 (M2 1 0 0 1) True 1
  : drawShape w 512 440 current False 1
  : mirrors
  ++ [discAt 512 440 4 accent]
  ++ hud
  ++ panel )
  where
    i@(Info n mir phi) = info w
    (cx, cy) = (512, 440) :: (Float, Float)    -- 図形の中心（画面の 40%, 55%）
    -- 今の変換を行列で（回転 2πk/n、鏡映は軸 phi で）
    current = case wAnim w of
      Nothing -> elementMatrix (wK w) (wF w) i
      Just a ->
        let t = aT a
            e = t * t * (3 - 2 * t)
            tm | aRot a = rotM (e * tau / fromIntegral n)
               | otherwise = squashM (aPhi a) (1 - 2 * e)   -- 軸に垂直な方向につぶして裏返す
        in mulM tm (elementMatrix (aK0 a) (aF0 a) i)
    -- 鏡の軸
    mirrors
      | not mir = []
      | otherwise =
          [ let a = phi + fromIntegral j * pi / fromIntegral n
                on = j == wAxis w `mod` n
                d = r0 * 1.35
            in dashed (if on then 3 else 1) (if on then accent else makeColorI 0 0 0 40)
                 (cx - cos a * d, cy - sin a * d) (cx + cos a * d, cy + sin a * d)
          | j <- [0 .. n - 1] ]
    hud =
      [ txt 20 36 18 (gray 0.12) ("1-4: shape   Up/Down: " ++ (if wShape w == 4 then "letter" else "corners")
          ++ "   R: rotate   F: mirror   Left/Right: axis   0: reset")
      , txt 20 62 15 (gray 0.43) "If it lands exactly on the pale shadow, the move is a symmetry. Two symmetries in a row: again a symmetry (a group)"
      ]
    -- 右の説明と、対称性の一覧
    px = 921.6 :: Float
    py = 120 :: Float
    count = n * (if mir then 2 else 1)
    Letter lname _ _ _ _ = currentLetter w
    title = case wShape w of
      1 -> "Regular " ++ show (wN w) ++ "-gon"
      2 -> "Snowflake"
      3 -> show (wN w) ++ "-blade windmill"
      _ -> "Letter " ++ lname
    panel =
      [ txt px py 22 (gray 0.12) title
      , txt px (py + 32) 16 (gray 0.31) ("Rotations " ++ show n ++ (if mir then " + mirrors " ++ show n else " (no mirrors)"))
      , txt px (py + 90) 48 accent (show count)
      , txt px (py + 118) 15 (gray 0.31)
          ((if mir then "Dihedral group D" else "Cyclic group C") ++ show n ++ " (order " ++ show count ++ ")")
      ] ++ concat
      [ [ box (x - s / 2) (y - s / 2) s s (if kk == wK w && ff == wF w then rgb255 255 220 120 else rgb255 235 230 220)
        , drawShape w x y (elementMatrix kk ff i) False (s / (2.6 * r0))
        , txtC x (y + s / 2 + 10) 14 (gray 0.24) (elName kk ff) ]
      | c <- [0 .. count - 1]
      , let kk = c `mod` n
            ff = if c < n then 0 else 1
            x = px + fromIntegral (c `mod` 4) * (s + 10) + s / 2
            y = y0 + fromIntegral (c `div` 4) * (s + 26) + s / 2
      , y <= height - 30 ]
    s = 54 :: Float
    y0 = py + 150

accent :: Color
accent = rgb255 230 60 90

-- 図形を描く。m は 2×2 行列、sc は大きさの倍率
drawShape :: World -> Float -> Float -> M2 -> Bool -> Float -> Picture
drawShape w cx cy (M2 a b c d) ghost sc = case wShape w of
  1 -> polygonShape
  2 -> snowflake
  3 -> windmill
  _ -> letterShape
  where
    n = wN w
    tr (x, y) = pt (cx + sc * (a * x + b * y), cy + sc * (c * x + d * y))
    thick ww = max 1 (ww * sc)
    seg ww p q = thickLine (thick ww) (tr p) (tr q)
    vertexAt :: Int -> Point
    vertexAt i = let t = -pi / 2 + fromIntegral i * tau / fromIntegral n in (r0 * cos t, r0 * sin t)
    polygonShape = Pictures $
      [ Color (if ghost then makeColorI 0 0 0 10 else rgba255 120 160 240 160) (Polygon (map (tr . vertexAt) [0 .. n - 1]))
      , Color (if ghost then makeColorI 0 0 0 25 else rgb255 40 60 120)
          (Pictures [seg (if ghost then 3 else 4) (vertexAt i) (vertexAt ((i + 1) `mod` n)) | i <- [0 .. n - 1]])
      ] ++ (if ghost then [] else concat   -- 頂点に番号と色 → どの対称性か区別できる
      [ Color (hsb (fromIntegral i * 360 / fromIntegral n) 70 90) (at (tr p) (disc (17 * sc)))
        : [ let (x, y) = tr p in labelC x y 18 white (show (i + 1)) | sc == 1 ]   -- 数字は読める向きに
      | i <- [0 .. n - 1], let p = vertexAt i ])
    spin :: Float -> Point -> Point
    spin t (x, y) = (x * cos t - y * sin t, x * sin t + y * cos t)
    snowflake = Pictures $
      [ Color (if ghost then makeColorI 0 0 0 25 else rgb255 40 120 200) (Pictures (concat
          [ seg ww (spin t (0, 0)) (spin t (0, -r0))
            : concat [ [ seg ww (spin t (0, -r0 * u)) (spin t (l, -r0 * u - l))
                       , seg ww (spin t (0, -r0 * u)) (spin t (-l, -r0 * u - l)) ]
                     | u <- [0.35, 0.6, 0.82], let l = r0 * (0.36 - u * 0.25) ]
          | i <- [0 .. 5 :: Int], let t = fromIntegral i * tau / 6 ]))
      ] ++ [ Color accent (at (tr (r0 * 0.12, -r0 * 0.62)) (disc (9 * sc))) | not ghost ]  -- 目印（1 つだけ）
      where ww = if ghost then 4 else 6
    windmill = Pictures
      [ Color col (Pictures      -- へこんだ四角形なので三角形 2 枚に分ける
          [ Polygon (map (tr . spin t) [(0, 0), (r0 * 0.25, -r0 * 0.55), (0, -r0)])
          , Polygon (map (tr . spin t) [(0, 0), (0, -r0), (-r0 * 0.08, -r0 * 0.3)]) ])
      | i <- [0 .. n - 1]
      , let t = fromIntegral i * tau / fromIntegral n
            col | ghost = makeColorI 0 0 0 18
                | i == 0 = accent
                | otherwise = rgb255 60 160 120 ]
    letterShape =
      let Letter _ segs _ _ _ = currentLetter w
          ww = if ghost then 30 else 34
          k = r0 * 0.75
          piece (x1, y1, x2, y2) =
            [ seg ww (x1 * k, y1 * k) (x2 * k, y2 * k)      -- 端を丸くする
            , at (tr (x1 * k, y1 * k)) (disc (thick ww / 2))
            , at (tr (x2 * k, y2 * k)) (disc (thick ww / 2)) ]
          mark = case segs of
            ((x, y, _, _) : _) | not ghost -> [Color accent (at (tr (x * k, y * k)) (disc (6 * sc)))]  -- 目印
            _ -> []
      in Pictures (Color (if ghost then makeColorI 0 0 0 25 else rgb255 40 60 120) (Pictures (concatMap piece segs)) : mark)

-- r^k s^f の行列：まず鏡映（軸 phi）、それから回転
elementMatrix :: Int -> Int -> Info -> M2
elementMatrix kk ff (Info n _ phi)
  | ff == 0 = rm
  | otherwise = mulM rm (M2 c s s (-c))
  where rm = rotM (fromIntegral kk * tau / fromIntegral n)
        c = cos (2 * phi)
        s = sin (2 * phi)

rotM :: Float -> M2
rotM a = M2 (cos a) (-sin a) (sin a) (cos a)

-- 軸 phi に垂直な方向を s 倍（s = -1 で鏡映）
squashM :: Float -> Float -> M2
squashM phi s = mulM (mulM (M2 c (-sn) sn c) (M2 1 0 0 s)) (M2 c sn (-sn) c)
  where c = cos phi
        sn = sin phi

mulM :: M2 -> M2 -> M2
mulM (M2 a b c d) (M2 e f g h) = M2 (a * e + b * g) (a * f + b * h) (c * e + d * g) (c * f + d * h)

elName :: Int -> Int -> String
elName kk ff = (if kk == 0 && ff == 0 then "e" else "")
  ++ (if kk /= 0 then "r" ++ (if kk > 1 then show kk else "") else "")
  ++ (if ff /= 0 then "s" else "")

onEvent :: Event -> World -> World
onEvent (EventKey k Down _ _) w0 =
  case k of
    Char c | c >= '1' && c <= '4' ->
      keyRest k w0 { wShape = fromEnum c - fromEnum '0', wK = 0, wF = 0, wAxis = 0, wAnim = Nothing }
    _ -> keyRest k w0
onEvent _ w = w

keyRest :: Key -> World -> World
keyRest k w = case wAnim w of
  Just _ -> w          -- 動いている間は受けつけない
  Nothing -> case k of
    Char c | c == 'r' || c == 'R' ->
      w { wAnim = Just (Anim True 0 (wK w) (wF w) 0), wK = (wK w + 1) `mod` n }
    Char c | (c == 'f' || c == 'F') && mir ->
      -- 軸 j の鏡映 = r^j s。r^j s · r^k s^f = r^(j-k) s^(1+f)
      let j = wAxis w `mod` n
      in w { wAnim = Just (Anim False 0 (wK w) (wF w) (phi + fromIntegral j * pi / fromIntegral n))
           , wK = (j - wK w) `mod` n, wF = 1 - wF w }
    Char '0' -> w { wK = 0, wF = 0 }
    SpecialKey KeyLeft -> w { wAxis = (wAxis w + n - 1) `mod` n }
    SpecialKey KeyRight -> w { wAxis = (wAxis w + 1) `mod` n }
    SpecialKey KeyUp -> changeN 1
    SpecialKey KeyDown -> changeN (-1)
    _ -> w
  where
    Info n mir phi = info w
    changeN d
      | wShape w == 4 = w { wLetter = (wLetter w + d) `mod` length letters, wK = 0, wF = 0, wAxis = 0 }
      | otherwise = w { wN = max 2 (min 12 (wN w + d)), wK = 0, wF = 0, wAxis = 0 }

-- ---------- やってみよう ----------
-- 1. 正方形（n = 4）で R を 4 回 → もとどおり。F を 2 回 → もとどおり。対称性には必ず「逆」がある
-- 2. R のあと F と、F のあと R は同じ？ 番号の位置を比べよう（→ 01 で詳しく）
-- 3. 風車（3）には鏡映がない。なぜ？ 羽根を鏡に映すと向きが逆になるから
-- 4. 文字を ↑↓ で切りかえて、それぞれ何個の対称性があるか予想してから確かめよう
-- 5. letters に自分の文字（T, O, K…）を足そう。n と鏡の有無、向きを正しく決められるかな？

-- ---------- 道具 ----------
width, height :: Float
width = 1280
height = 800

-- p5 の座標（左上が原点、y は下向き）→ gloss の座標
pt :: Point -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y = label (x - width / 2) (height / 2 - y)

txtC :: Float -> Float -> Float -> Color -> String -> Picture
txtC x y = labelC (x - width / 2) (height / 2 - y)

-- 左上 (x, y)、幅 w、高さ h の塗りつぶした四角
box :: Float -> Float -> Float -> Float -> Color -> Picture
box x y w h c = Color c (rect (x + w / 2 - width / 2) (height / 2 - y - h / 2) w h)

discAt :: Float -> Float -> Float -> Color -> Picture
discAt x y r c = Color c (at (pt (x, y)) (disc r))

-- 点線（8 ピクセルごとに描く・あける）
dashed :: Float -> Color -> Point -> Point -> Picture
dashed w c (x1, y1) (x2, y2) =
  let l = dist2 (x1, y1) (x2, y2)
      n = floor (l / 16) :: Int
      p t = (x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
  in Color c (Pictures
       [ thickLine w (pt (p (16 * fromIntegral i / l))) (pt (p (min 1 ((16 * fromIntegral i + 8) / l))))
       | i <- [0 .. n] ])
