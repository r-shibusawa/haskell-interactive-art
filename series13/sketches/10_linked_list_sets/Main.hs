-- ============================================
-- 10 互いに素な集合（連結リスト版）─ 短い方を長い方につなぐ
-- 集合を連結リストで表し、どの要素も「自分の集合（代表）」へのポインタを持つ。FIND-SET は O(1)
-- UNION は片方のリストをもう片方の後ろにつなぎ、つないだ要素のポインタを全部書きかえる。短い方をつなぐ（重みつき合併）と速い
-- 要素を 2 つ順にクリック = UNION / スペース = ランダムに UNION / W = 重みつき合併 ON/OFF / B = 最悪の順番で合併 / R = やり直し
-- ============================================
module Main (main) where

import Art
import Data.List (sortOn)
import qualified Data.IntMap.Strict as IM

n :: Int
n = 16                -- ★ 要素の数

speed :: Float
speed = 0.25          -- ★ 1 回の UNION の間の秒数（B のとき）

data SetL = SetL { sId :: !Int, sList :: ![Int] }

data World = World
  { wSets     :: ![SetL]
  , wRep      :: !(IM.IntMap Int)     -- 要素 → 集合の番号
  , wWeighted :: !Bool
  , wPos      :: !(IM.IntMap Point)
  , wFlash    :: !(IM.IntMap Float)
  , wSelected :: !(Maybe Int)
  , wQueue    :: ![(Int, Int)]
  , wTimer    :: !Float
  , wUpdates  :: !Int
  , wUnions   :: !Int
  , wMsg      :: !String
  , wHist     :: ![(Bool, Int)]       -- 新しい順、6 個まで
  }

main :: IO ()
main = runSketch (sketch "13-10 Linked List Sets" (resetWorld (World [] IM.empty True IM.empty IM.empty Nothing [] 0 0 0 "" [])))
  { skBack  = hsb 100 25 9
  , skStep  = \env dt w -> pure (stepWorld env dt w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

resetWorld :: World -> World
resetWorld w = w
  { wSets = [SetL i [i] | i <- [0 .. n - 1]]        -- MAKE-SET
  , wRep = IM.fromList [(i, i) | i <- [0 .. n - 1]]
  , wQueue = [], wSelected = Nothing, wUpdates = 0, wUnions = 0, wFlash = IM.empty
  , wMsg = "MAKE-SET " ++ show n ++ " times: every element is its own set" }

setOf :: World -> Int -> Maybe SetL
setOf w sid = case filter ((== sid) . sId) (wSets w) of
  (s : _) -> Just s
  [] -> Nothing

-- UNION(x, y)：y の集合を x の集合の後ろにつなぐ（重みつきなら短い方を長い方へ）
union :: Int -> Int -> World -> World
union x y w =
  case (setOf w (IM.findWithDefault 0 x (wRep w)), setOf w (IM.findWithDefault 0 y (wRep w))) of
    (Just a0, Just b0)
      | sId a0 == sId b0 -> w { wMsg = show x ++ " and " ++ show y ++ " are already in the same set" }
      | otherwise ->
          let (a, b) = if wWeighted w && length (sList b0) > length (sList a0) then (b0, a0) else (a0, b0)
              la = length (sList a)
              lb = length (sList b)
              a' = a { sList = sList a ++ sList b }
          in w { wRep = foldr (\e -> IM.insert e (sId a)) (wRep w) (sList b)
               , wFlash = foldr (\e -> IM.insert e 1) (wFlash w) (sList b)
               , wUpdates = wUpdates w + lb
               , wUnions = wUnions w + 1
               , wMsg = "UNION: list of " ++ show lb ++ " goes after list of " ++ show la ++ ": rewrite " ++ show lb ++ " pointers"
               , wSets = [ if sId s == sId a then a' else s | s <- wSets w, sId s /= sId b ] }
    _ -> w

-- ---------- 配置 ----------
-- 上から順に並んだ行：(集合, 行の中心 y)
rowsOf :: Env -> World -> ([(SetL, Float)], Float)
rowsOf env w =
  let rows = sortOn (foldr min maxBound . sList) (wSets w)
      rowH = min 46 ((envH env - 260) / fromIntegral (max 1 (length rows)))
  in ([(s, 150 + fromIntegral r * rowH + rowH / 2) | (r, s) <- zip [0 :: Int ..] rows], rowH)

nodeWidth :: Env -> Float
nodeWidth env = min 50 ((envW env - 520) / fromIntegral n)

listX :: Float
listX = 190

stepWorld :: Env -> Float -> World -> World
stepWorld env dt w0 =
  let w1 = case wQueue w0 of
        [] -> w0
        ((a, b) : rest) ->
          let t = wTimer w0 + dt
          in if t > speed
               then
                 let w' = (union a b w0) { wQueue = rest, wTimer = 0 }
                 in if null rest then w' { wHist = take 6 ((wWeighted w', wUpdates w') : wHist w') } else w'
               else w0 { wTimer = t }
      (rows, _) = rowsOf env w1
      nw = nodeWidth env
      targets = IM.fromList
        [ (e, (listX + fromIntegral j * nw, y)) | (s, y) <- rows, (j, e) <- zip [0 :: Int ..] (sList s) ]
  in w1 { wPos = IM.mapWithKey (\e t -> lerpP 0.15 (IM.findWithDefault t e (wPos w1)) t) targets
        , wFlash = IM.map (* 0.95) (wFlash w1) }

-- ---------- 描画 ----------
drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (setPics ++ elemPics ++ panel)
  where
    h = envH env
    (rows, rowH) = rowsOf env w
    nw = nodeWidth env
    posOf e = IM.findWithDefault (0, 0) e (wPos w)
    hueOf s = fromIntegral ((s * 137) `mod` 360) :: Float
    rowY sid = case [y | (s, y) <- rows, sId s == sid] of
      (y : _) -> y
      [] -> 0
    -- 集合オブジェクトと矢印
    setPics = concat
      [ [ fillBox env (hsb hue 50 40) 30 (y - rowH * 0.38) 130 (rowH * 0.76)
        , txtC env 95 y 14 (hsb 0 0 100) ("Set " ++ show (sId s) ++ " (" ++ show (length (sList s)) ++ ")") ]
        ++ [ lineP env 1.5 (hsb hue 40 70) (160, y) (fx - nw * 0.35, fy) | (first : _) <- [sList s], let (fx, fy) = posOf first ]
        ++ concat
           [ [ lineP env 1.5 (hsb hue 40 70) (ax + nw * 0.35, ay) (bx - nw * 0.35, by)
             , triP env (hsb hue 40 70) (bx - nw * 0.35, by) (bx - nw * 0.35 - 6, by - 4) (bx - nw * 0.35 - 6, by + 4) ]
           | (a, b) <- zip (sList s) (drop 1 (sList s))
           , let (ax, ay) = posOf a
                 (bx, by) = posOf b ]
      | (s, y) <- rows, let hue = hueOf (sId s) ]
    (mx, my) = mouseXY env
    -- 要素
    elemPics = concat
      [ -- 代表へのポインタ：書きかえた直後と、マウスをのせた行だけ表示
        [ pathP env 1.5 (if hover then hsb hue 40 90 else hsba 0 0 100 fl)
            (bezierPts (px, py - rowH * 0.3) (px, py - rowH * 0.75) (95, sy - rowH * 0.75) (95, sy - rowH * 0.38))
        | fl > 0.05 || hover ]
        ++ [ fillBox env (if wSelected w == Just e then hsb 50 90 100 else if fl > 0.1 then hsb 0 0 100 else hsb hue 55 80) (px - nw * 0.35) (py - rowH * 0.32) (nw * 0.7) (rowH * 0.64)
           , txtC env px py 14 (hsb 0 0 10) (show e) ]
      | e <- [0 .. n - 1]
      , let (px, py) = posOf e
            sid = IM.findWithDefault 0 e (wRep w)
            hue = hueOf sid
            sy = rowY sid
            fl = IM.findWithDefault 0 e (wFlash w)
            hover = abs (my - sy) < rowH / 2 && mx < envW env - 320 ]
    px0 = envW env - 300
    panel =
      [ txt env 24 18 26 (hsb 0 0 100) "Disjoint sets: linked lists"
      , txt env 24 56 15 (hsb 0 0 80)
          ("Weighted union (shorter goes after longer): " ++ (if wWeighted w then "ON" else "OFF") ++ " (W)   sets " ++ show (length (wSets w))
           ++ "   UNIONs " ++ show (wUnions w) ++ "   pointer rewrites " ++ show (wUpdates w))
      , txt env 24 88 17 (hsb 100 80 100) (wMsg w)
      , txt env px0 150 15 (hsb 0 0 100) "B (worst order) results"
      ] ++
      [ txt env px0 (178 + fromIntegral j * 22) 14 (if wt then hsb 100 60 90 else hsb 0 60 90)
          ((if wt then "Weighted: " else "Unweighted: ") ++ show u ++ " rewrites")
      | (j, (wt, u)) <- zip [0 :: Int ..] (reverse (wHist w)) ] ++
      [ txt env px0 (330 + fromIntegral j * 20) 14 (hsb 0 0 70) s
      | (j, s) <- zip [0 :: Int ..]
          [ "Worst unweighted:"
          , "1+2+...+" ++ show (n - 1) ++ " = " ++ show (n * (n - 1) `div` 2) ++ " (n^2)"
          , "Weighted: each rewrite"
          , "at least doubles the set"
          , "of that element, so at"
          , "most lg n per element."
          , "Total O(n lg n), max " ++ show (round (fromIntegral n * logBase 2 (fromIntegral n :: Float)) :: Int) ] ] ++
      [ txt env 24 (h - 52) 14 (hsb 0 0 60) "Hover a row to see pointers to the representative. Click 2 elements = UNION / Space = random"
      , txt env 24 (h - 30) 14 (hsb 0 0 60) "W = weighted ON/OFF / B = worst order (slow without weights) / R = reset"
      ]

-- ---------- マウスとキー ----------
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let (mx, my) = mouseXY env
        nw = nodeWidth env
        hit = [ e | e <- [0 .. n - 1], let (px, py) = IM.findWithDefault (0, 0) e (wPos w)
                  , abs (mx - px) < nw * 0.4, abs (my - py) < 16 ]
    in pure $ case (hit, wSelected w) of
         (e : _, Nothing) -> w { wSelected = Just e }
         (e : _, Just s) -> (union s e w) { wSelected = Nothing }
         ([], _) -> w { wSelected = Nothing }
  EventKey (SpecialKey KeySpace) Down _ _
    | length (wSets w) > 1 -> do
        a <- pick (wSets w)
        b <- pick (wSets w)
        pure $ case (a, b) of
          (Just (SetL ia (x : _)), Just (SetL ib (y : _))) | ia /= ib -> union x y w
          _ -> w
    | otherwise -> pure w
  EventKey (Char c) Down _ _
    | c `elem` "wW" -> pure w { wWeighted = not (wWeighted w) }
    -- UNION(x2, x1), UNION(x3, x2), …：どんどん長くなるリストを 1 個の集合の後ろにつなぐ
    | c `elem` "bB" -> pure (resetWorld w) { wQueue = [(i + 1, i) | i <- [0 .. n - 2]] }
    | c `elem` "rR" -> pure (resetWorld w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. W で重みなしにして B を押そう。書きかえの回数は重みつきの何倍？
-- 2. n を 32 にすると、重みなしの最悪と重みつきの差はどう広がる？ 式と比べよう
-- 3. 重みつきで、ある要素のポインタが書きかえられる回数を数えよう。lg n を超える？

-- ---------- 道具 ----------

-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標に直す
toG :: Env -> Float -> Float -> Point
toG env x y = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点）
mouseXY :: Env -> Point
mouseXY env = let (mx, my) = envMouse env in (mx + envW env / 2, envH env / 2 - my)

-- 文字（左上が x, y）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (gx, gy) = toG env x y in label gx (gy - size) size c s

-- 中央揃えの文字（中心が x, y）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = let (gx, gy) = toG env x y in labelC gx gy size c s

-- 太さのある線分
lineP :: Env -> Float -> Color -> Point -> Point -> Picture
lineP env w c (x1, y1) (x2, y2) = Color c (thickLine w (toG env x1 y1) (toG env x2 y2))

-- 太さのある折れ線
pathP :: Env -> Float -> Color -> [Point] -> Picture
pathP env w c ps = Color c (thickPath w [toG env x y | (x, y) <- ps])

-- ベジェ曲線の上の点を並べる
bezierPts :: Point -> Point -> Point -> Point -> [Point]
bezierPts (ax, ay) (bx, by) (cx, cy) (dx, dy) =
  [ (f ax bx cx dx t, f ay by cy dy t) | i <- [0 .. 16 :: Int], let t = fromIntegral i / 16 ]
  where f a b c d t = let u = 1 - t in u * u * u * a + 3 * u * u * t * b + 3 * u * t * t * c + t * t * t * d

-- 塗りつぶした長方形（x, y は左上）
fillBox :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
fillBox env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 三角形
triP :: Env -> Color -> Point -> Point -> Point -> Picture
triP env c (a, b) (d, e) (f, g) = Color c (Polygon [toG env a b, toG env d e, toG env f g])

-- 2 点の間を t の割合で進む（p5.js の lerp を x, y に）
lerpP :: Float -> Point -> Point -> Point
lerpP t (a, b) (c, d) = (lerpF a c t, lerpF b d t)

-- 列の i 番目（なければ Nothing）
safeAt :: [a] -> Int -> Maybe a
safeAt xs i = if i < 0 then Nothing else case drop i xs of
  (x : _) -> Just x
  [] -> Nothing

-- 列からランダムに 1 つ選ぶ
pick :: [a] -> IO (Maybe a)
pick [] = pure Nothing
pick xs = do
  i <- randomRIO (0, length xs - 1)
  pure (safeAt xs i)
