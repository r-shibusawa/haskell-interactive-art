-- ============================================
-- 00 折り線と折り目
-- 紙の上の点 P をつまんで点 Q に重ねると、折り線は「P と Q の垂直二等分線」になる
--   折る = 折り線について線対称に移す（鏡映）。何回でも重ねて折れて、重なった紙は層になる
-- ドラッグ = 点をつまんで重ねる（角や辺の中点に吸いつく）/ U = 広げて折り目を見る / Z = 1 つ戻す / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import Data.List (sortOn)

snapR :: Float
snapR = 18                        -- ★ 角や中点に吸いつく距離（px）

frontCol, backCol :: Color
frontCol = rgb255 246 240 226     -- ★ 紙のおもて（白）
backCol  = rgb255 226 84 72       -- ★ 紙のうら（色紙の赤）

maxFaces :: Int
maxFaces = 512                    -- ★ 面の数の上限（これ以上は折らない）

-- 紙の面：今の位置、もとの紙 → 今 の変換、裏返っているか、重なりの順
data Face = Face { fPts :: [Point], fM :: !Mat, fBack :: !Bool, fZ :: !Int }

-- 広げた紙の上の折り目（True = 山、False = 谷）
data Crease = Crease !Point !Point !Bool

data World = World
  { wFaces   :: [Face]
  , wCreases :: [Crease]
  , wHistory :: [([Face], [Crease])]   -- Z で戻すための記録
  , wGrab    :: !(Maybe Point)         -- つまんだ点 P
  , wUnfold  :: !Bool
  }

main :: IO ()
main = runSketch (sketch "26-00 Fold Line and Creases" (newPaper 1280 800))
  { skBack  = rgb255 34 32 40
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 紙の一辺と左上
paperRect :: Float -> Float -> (Float, Float, Float)
paperRect w h =
  let s = min w h * 0.6
  in (s, w / 2 - s / 2, h / 2 - s / 2 + 30)

newPaper :: Float -> Float -> World
newPaper w h =
  let (s, ox, oy) = paperRect w h
      sq = [(ox, oy), (ox + s, oy), (ox + s, oy + s), (ox, oy + s)]
  in World [Face sq ident False 0] [] [] Nothing False

-- ★ 折る：折り線 L について、点 P がある側の紙をぜんぶ鏡に映して上に重ねる
foldFaces :: [Face] -> Ln -> Point -> ([Face], [Crease])
foldFaces fs ln p =
  let side = if sdist p ln > 0 then 1 else -1
      maxZ = maximum (0 : map fZ fs)
      r = reflMat ln
      one f =
        let keep = clipHalf (fPts f) ln (-side)
            move = clipHalf (fPts f) ln side
            kept = [f { fPts = keep } | length keep >= 3]
            moved = [ Face (map (refl ln) move) (compose r (fM f)) (not (fBack f)) (2 * maxZ + 1 - fZ f)
                    | length move >= 3 ]
            -- 面が切られた所が新しい折り目。もとの紙の位置に戻して記録する
            -- おもてが見えている面を手前に折る → おもてから見て谷折り。裏が見えている面なら山折り
            cuts = case cutSeg (fPts f) ln of
              Just (a, b) | length move >= 3 && length keep >= 3 ->
                let inv = inverse (fM f) in [Crease (apply inv a) (apply inv b) (fBack f)]
              _ -> []
        in (kept ++ moved, cuts)
      rs = map one fs
  in (concatMap fst rs, concatMap snd rs)

drawWorld :: Env -> World -> Picture
drawWorld env w =
  let m = mouseXY env
      foldNow = case (wGrab w, wUnfold w) of
        (Just p, False) ->
          let q = snapPoint w m
          in if dist2 p q > 4 then Just (p, q, bisector p q) else Nothing
        _ -> Nothing
      shown = case foldNow of
        Just (p, _, ln) -> fst (foldFaces (wFaces w) ln p)
        Nothing -> wFaces w
      paper = if wUnfold w then drawUnfolded env w else drawFaces env shown
      guide = case foldNow of
        Nothing -> Blank
        Just (p, q, Ln (ax, ay) (ux, uy)) -> Pictures
          [ Color (rgb255 255 220 90) (dashed 2 [10, 8] (sc env (ax - ux * 2000, ay - uy * 2000)) (sc env (ax + ux * 2000, ay + uy * 2000)))
          , Color (rgba255 255 255 255 120) (thickLine 1 (sc env p) (sc env q))
          , Color (rgb255 90 200 255) (at (sc env p) (disc 7))
          , Color (rgb255 255 220 90) (at (sc env q) (disc 7))
          , txt env (fst p + 10) (snd p - 10) 15 white "P"
          , txt env (fst q + 10) (snd q - 10) 15 white "Q"
          ]
      -- 吸いつく点の目印
      marks = if wUnfold w then Blank else
        Color (rgba255 255 255 255 60) (Pictures [at (sc env t) (disc 3) | t <- snapTargets w])
      nFold = length (wHistory w)
      hud = Pictures
        [ txt env 20 40 18 white ("Drag: pick P, drop on Q   U: unfold " ++ onOff (wUnfold w) ++ "   Z: undo   R: reset   folds " ++ show nFold ++ "   layers " ++ show (length (wFaces w)))
        , txt env 20 68 15 (gray 0.67) "Fold line = perpendicular bisector of P and Q. Every point moves to its mirror image across it."
        , if wUnfold w then Pictures
            [ txt env 20 (envH env - 50) 15 (rgb255 255 120 100) "-.-. Mountain (M)"
            , txt env 230 (envH env - 50) 15 (rgb255 110 170 255) "- - - Valley (V)"
            , txt env 430 (envH env - 50) 15 (gray 0.78)
                ("M " ++ show (length [() | Crease _ _ True <- wCreases w]) ++ "   V " ++ show (length [() | Crease _ _ False <- wCreases w]) ++ "   (seen from the front)")
            ]
          else Blank
        ]
  in Pictures [paper, guide, marks, hud]

drawFaces :: Env -> [Face] -> Picture
drawFaces env fs = Pictures (concatMap one (sortOn fZ fs))
  where
    one f =
      let ps = map (sc env) (fPts f)
      in [ Color (if fBack f then backCol else frontCol) (Polygon ps)
         , Color (rgba255 60 40 40 150) (poly ps) ]

drawUnfolded :: Env -> World -> Picture
drawUnfolded env w =
  let (s, ox, oy) = paperRect (envW env) (envH env)
      (cx, cy) = sc env (ox + s / 2, oy + s / 2)
      one (Crease a b isM)
        | isM = Color (rgb255 230 80 60) (dashed 2.5 [14, 5, 3, 5] (sc env a) (sc env b))
        | otherwise = Color (rgb255 50 110 230) (dashed 2.5 [8, 6] (sc env a) (sc env b))
  in Pictures (Color frontCol (rect cx cy s s) : map one (wCreases w))

snapTargets :: World -> [Point]
snapTargets w = concat
  [ [a, ((ax + bx) / 2, (ay + by) / 2)]
  | f <- wFaces w, (a@(ax, ay), (bx, by)) <- edges (fPts f) ]

snapPoint :: World -> Point -> Point
snapPoint w m = fst (foldl pick (m, snapR) (snapTargets w))
  where pick (b, bd) t = let d = dist2 m t in if d < bd then (t, d) else (b, bd)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _
    | wUnfold w -> w
    | otherwise ->
        let m = mouseXY env
            p = snapPoint w m
            on = any (insidePoly p . fPts) (wFaces w) || p /= m
        in w { wGrab = if on then Just p else Nothing }
  EventKey (MouseButton LeftButton) Up _ _ -> case wGrab w of
    Nothing -> w
    Just p ->
      let q = snapPoint w (mouseXY env)
          (fs, cuts) = foldFaces (wFaces w) (bisector p q) p
      in if dist2 p q > 4 && length fs <= maxFaces
           then w { wGrab = Nothing, wHistory = (wFaces w, wCreases w) : wHistory w
                  , wFaces = fs, wCreases = wCreases w ++ cuts }
           else w { wGrab = Nothing }
  EventKey (Char c) Down _ _ -> case toLower c of
    'u' -> w { wUnfold = not (wUnfold w) }
    'z' -> case wHistory w of
      (fs, cs) : rest -> w { wFaces = fs, wCreases = cs, wHistory = rest }
      [] -> w
    'r' -> (newPaper (envW env) (envH env)) { wUnfold = wUnfold w }
    _ -> w
  EventResize _ -> newPaper (envW env) (envH env)
  _ -> w

-- ---------- やってみよう ----------
-- 1. 角を向かいの角に重ねる・辺を向かいの辺に重ねる。U で広げると折り目はどこにできた？
-- 2. 半分に 3 回折ってから U。折り目は何本？ 山と谷の数をくらべよう（06 前川の定理への伏線）
-- 3. 角を辺の中点に重ねると、辺が 2:1 に分かれる場所ができる（04 芳賀の定理）。探してみよう
-- 4. backCol を好きな色に。おもてとうらで違う色の「色紙」にすると、折り紙の模様がつくれる

-- ==================================================
-- ここから下は「折り紙の幾何の道具」（線対称・多角形の切り分け・変換）と画面の道具
-- ==================================================

-- 直線 L：通る点と向き（長さ 1）。座標は p5.js と同じ「左上が原点、y は下向き」
data Ln = Ln !Point !Point

bisector :: Point -> Point -> Ln   -- a と b の垂直二等分線
bisector (ax, ay) (bx, by) =
  let dx = bx - ax
      dy = by - ay
      l0 = sqrt (dx * dx + dy * dy)
      l = if l0 == 0 then 1 else l0
  in Ln ((ax + bx) / 2, (ay + by) / 2) (-dy / l, dx / l)

sdist :: Point -> Ln -> Float       -- 符号つき距離
sdist (qx, qy) (Ln (px, py) (ux, uy)) = (qx - px) * (-uy) + (qy - py) * ux

refl :: Ln -> Point -> Point
refl ln@(Ln _ (ux, uy)) q@(qx, qy) = let s = sdist q ln in (qx + 2 * s * uy, qy - 2 * s * ux)

edges :: [a] -> [(a, a)]
edges ps = zip ps (drop 1 ps ++ take 1 ps)

mixP :: Point -> Point -> Float -> Point
mixP (ax, ay) (bx, by) t = (ax + (bx - ax) * t, ay + (by - ay) * t)

-- 凸多角形を直線で切り、side 側（符号）を残す
clipHalf :: [Point] -> Ln -> Float -> [Point]
clipHalf pts ln side = concatMap one (edges pts)
  where
    one (a, b) =
      let sa = sdist a ln * side
          sb = sdist b ln * side
      in [a | sa >= -1e-3]
         ++ [mixP a b (sa / (sa - sb)) | (sa > 1e-3 && sb < -1e-3) || (sa < -1e-3 && sb > 1e-3)]

cutSeg :: [Point] -> Ln -> Maybe (Point, Point)   -- 多角形と直線が交わる 2 点
cutSeg pts ln =
  case concatMap one (edges pts) of
    r@(a : _ : _) -> Just (a, last r)
    _ -> Nothing
  where
    one (a, b) =
      let sa = sdist a ln
          sb = sdist b ln
      in if abs sa < 1e-3 then [a]
         else if sa * sb < 0 then [mixP a b (sa / (sa - sb))]
         else []

insidePoly :: Point -> [Point] -> Bool
insidePoly (px, py) pts = foldl flipIf False (edges pts)
  where
    flipIf c ((jx, jy), (ix, iy))
      | (iy > py) /= (jy > py) && px < (jx - ix) * (py - iy) / (jy - iy) + ix = not c
      | otherwise = c

-- 平面の変換 x' = a x + b y + e,  y' = c x + d y + f
data Mat = Mat !Float !Float !Float !Float !Float !Float

ident :: Mat
ident = Mat 1 0 0 1 0 0

apply :: Mat -> Point -> Point
apply (Mat a b c d e f) (x, y) = (a * x + b * y + e, c * x + d * y + f)

compose :: Mat -> Mat -> Mat       -- B をしてから A をする（合成 A∘B）
compose (Mat a b c d e f) (Mat a2 b2 c2 d2 e2 f2) =
  Mat (a * a2 + b * c2) (a * b2 + b * d2) (c * a2 + d * c2) (c * b2 + d * d2)
      (a * e2 + b * f2 + e) (c * e2 + d * f2 + f)

inverse :: Mat -> Mat
inverse (Mat a0 b0 c0 d0 e f) =
  let det = a0 * d0 - b0 * c0
      a = d0 / det
      b = -b0 / det
      c = -c0 / det
      d = a0 / det
  in Mat a b c d (-(a * e + b * f)) (-(c * e + d * f))

reflMat :: Ln -> Mat               -- 直線 L についての鏡映
reflMat (Ln (px, py) (ux, uy)) =
  let nx = -uy
      ny = ux
      a = 1 - 2 * nx * nx
      b = -2 * nx * ny
      d = 1 - 2 * ny * ny
  in Mat a b b d (px - (a * px + b * py)) (py - (b * px + d * py))

-- p5.js の座標（左上が原点、y は下向き）→ gloss の座標
sc :: Env -> Point -> Point
sc env (x, y) = (x - envW env / 2, envH env / 2 - y)

mouseXY :: Env -> Point
mouseXY env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sc env (x, y) in label a b size c s

onOff :: Bool -> String
onOff b = if b then "ON" else "off"

-- 破線：pat は「描く長さ、あける長さ、…」のくり返し
dashed :: Float -> [Float] -> Point -> Point -> Picture
dashed w pat a b =
  let l = dist2 a b
      go s (on : off : rest)
        | s >= l = []
        | otherwise = thickLine w (mixP a b (s / l)) (mixP a b (min l (s + on) / l)) : go (s + on + off) rest
      go _ _ = []
  in if l < 1e-6 then Blank else Pictures (go 0 (cycle pat))
