-- ============================================
-- 06 掃引線で交差を見つける
-- 縦の線（掃引線）を左から右へ動かし、「いま線にかかっている線分」を上から順に並べておく
-- 新しく入った線分は「すぐ上」と「すぐ下」とだけ、抜けるときはその上下どうしを比べる
-- 全部の組（n(n−1)/2 通り）を調べなくても、交差があれば必ず見つかる → O(n log n)
-- 操作：端点をドラッグ / スペース = 再生・停止 / → = 次のイベント / R = ランダム
--       S = 最初の交差で止める⇄最後まで / 1〜3 = お手本
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM)
import Data.List (elemIndex, findIndex, intercalate, sortBy)
import Data.Ord (comparing)
import Numeric (showFFloat)

segCount :: Int
segCount = 8            -- ★ 線分の数

data Seg = Seg { sA :: !Point, sB :: !Point, sHue :: !Float }

data Ev = Ev { eX :: !Float, eY :: !Float, eLeft :: !Bool, eI :: !Int }

data World = World
  { wSegs    :: ![Seg]
  , wEvents  :: ![Ev]
  , wEv      :: !Int
  , wOnLine  :: ![Int]                 -- 掃引線にかかっている線分（上から順）
  , wSweepX  :: !Float
  , wTargetX :: !Float
  , wChecks  :: ![(Int, Int, Bool)]    -- 調べた組（新しい順）
  , wFound   :: ![(Int, Int)]
  , wStop1st :: !Bool
  , wAuto    :: !Bool
  , wWaiting :: !Int
  , wDrag    :: !(Maybe (Int, Bool))   -- つかんでいる端点（線分, a か）
  }

main :: IO ()
main = runSketch (sketch "17-06 Sweep line" initialWorld)
  { skBack  = hsb 260 45 10
  , skStart = \env -> preset env 1 initialWorld
  , skStep  = \_ _ w -> pure (stepW w)
  , skDraw  = drawW
  , skEvent = onEvent
  }

initialWorld :: World
initialWorld = World [] [] 0 [] 30 30 [] [] True True 0 Nothing

preset :: Env -> Int -> World -> IO World
preset env k w = do
  let bigW = envW env - 360
      hh = envH env
      n = segCount
  segs <- forM [0 .. n - 1] $ \i -> do
    x1 <- randomRIO (60, bigW * 0.7)
    dx <- randomRIO (120, bigW * 0.45)
    ya <- randomRIO (140, hh - 60)
    yb <- randomRIO (140, hh - 60)
    jitter <- randomRIO (-20, 20)
    let fi = fromIntegral i
        (y1, y2) = case k of
          2 -> let y = 140 + fi * (hh - 200) / fromIntegral n in (y, y + jitter)     -- 交わらない
          3 -> if odd i then (160, hh - 80) else (hh - 80, 160)                       -- たくさん交わる
          _ -> (ya, yb)
    pure (Seg (x1, y1) (min (x1 + dx) (bigW - 20), y2) (fi * 360 / fromIntegral n))
  pure (restart w { wSegs = segs, wDrag = Nothing })

restart :: World -> World
restart w =
  let norm s =
        let s1 = if fst (sA s) > fst (sB s) then s { sA = sB s, sB = sA s } else s
            (ax, _) = sA s1
            (bx, by) = sB s1
        in if abs (bx - ax) < 3 then s1 { sB = (ax + 3, by) } else s1    -- 縦の線分はなし
      segs = map norm (wSegs w)
      evs = concat [ [Ev ax ay True i, Ev bx by False i]
                   | (i, Seg (ax, ay) (bx, by) _) <- zip [0 ..] segs ]
      -- 同じ x なら左端が先、y の小さい方が先
      sorted = sortBy (comparing (\e -> (eX e, not (eLeft e), eY e))) evs
      first = case sorted of
        (e : _) -> eX e
        [] -> 30
  in w { wSegs = segs, wEvents = sorted, wEv = 0, wOnLine = [], wChecks = [], wFound = []
       , wSweepX = 30, wTargetX = first }

segAt :: World -> Int -> Seg
segAt w i = wSegs w !! i     -- i はいつも 0〜segCount−1

yAt :: Seg -> Float -> Float
yAt (Seg (ax, ay) (bx, by) _) x = ay + (by - ay) * (x - ax) / (bx - ax)

turn :: Point -> Point -> Point -> Float
turn (ox, oy) (ax, ay) (bx, by) = (ax - ox) * (by - oy) - (ay - oy) * (bx - ox)

onSeg :: Point -> Point -> Point -> Bool
onSeg (ax, ay) (bx, by) (cx, cy) =
  min ax bx <= cx && cx <= max ax bx && min ay by <= cy && cy <= max ay by

intersects :: World -> Int -> Int -> Bool
intersects w i j =
  let Seg p1 p2 _ = segAt w i
      Seg p3 p4 _ = segAt w j
      d1 = turn p3 p4 p1
      d2 = turn p3 p4 p2
      d3 = turn p1 p2 p3
      d4 = turn p1 p2 p4
  in (((d1 > 0 && d2 < 0) || (d1 < 0 && d2 > 0)) && ((d3 > 0 && d4 < 0) || (d3 < 0 && d4 > 0)))
     || (d1 == 0 && onSeg p3 p4 p1) || (d2 == 0 && onSeg p3 p4 p2)
     || (d3 == 0 && onSeg p1 p2 p3) || (d4 == 0 && onSeg p1 p2 p4)

-- 2 本を調べる（どちらかがなければ何もしない）
test :: Maybe Int -> Maybe Int -> World -> World
test (Just i) (Just j) w =
  let hit = intersects w i j
      known = any (\(a, b) -> (a == i && b == j) || (a == j && b == i)) (wFound w)
  in w { wChecks = (i, j, hit) : wChecks w
       , wFound = if hit && not known then wFound w ++ [(i, j)] else wFound w }
test _ _ w = w

nth :: [Int] -> Int -> Maybe Int
nth xs i
  | i < 0 = Nothing
  | otherwise = case drop i xs of
      (x : _) -> Just x
      [] -> Nothing

-- ★ イベントを 1 つ処理する
processEvent :: World -> World
processEvent w
  | wEv w >= length evs || (wStop1st w && not (null (wFound w))) = w
  | otherwise =
      case drop (wEv w) evs of
        [] -> w
        (e : rest) ->
          let w1 = w { wEv = wEv w + 1, wSweepX = eX e }
              ol = wOnLine w
              w2 | eLeft e =
                     -- INSERT：掃引線との交点の y で並ぶ位置を決める
                     let pos = maybe (length ol) id (findIndex (\k -> yAt (segAt w k) (eX e) > eY e) ol)
                         ol' = take pos ol ++ [eI e] ++ drop pos ol
                     in test (Just (eI e)) (nth ol' (pos + 1))        -- すぐ下と
                          (test (Just (eI e)) (nth ol' (pos - 1))     -- すぐ上と
                             w1 { wOnLine = ol' })
                 | otherwise = case elemIndex (eI e) ol of
                     Just pos ->
                       let w3 = test (nth ol (pos - 1)) (nth ol (pos + 1)) w1    -- 抜けたあと隣どうしになる 2 本
                       in w3 { wOnLine = take pos ol ++ drop (pos + 1) ol }
                     Nothing -> w1
          in case rest of
               (e' : _) -> w2 { wTargetX = eX e' }
               [] -> w2
  where evs = wEvents w

-- 掃引線を次のイベントまで動かす
stepW :: World -> World
stepW w
  | wAuto w && not (wStop1st w && not (null (wFound w))) && wEv w < length (wEvents w) =
      let x = min (wSweepX w + 4) (wTargetX w)
          w1 = w { wSweepX = x }
      in if x >= wTargetX w
           then if wWaiting w + 1 > 10
                  then processEvent w1 { wWaiting = 0 }
                  else w1 { wWaiting = wWaiting w + 1 }
           else w1
  | otherwise = w

drawW :: Env -> World -> Picture
drawW env w =
  let ww = envW env
      hh = envH env
      sx = wSweepX w
      segPic (i, s@(Seg a@(ax, ay) b hue)) =
        let active = i `elem` wOnLine w
            bad = any (\(p, q) -> p == i || q == i) (wFound w)
        in Pictures
             [ seg env (hsb hue (if bad then 90 else 55) (if active then 100 else 55)) (if active then 4 else 2) a b
             , dot env (hsb hue 55 100) a 9
             , dot env (hsb hue 55 100) b 9
             , labelR (ax - 8) ay (show (i :: Int)) ]
      labelR x y str = txtM env (x - fromIntegral (length str) * 11) y 14 (hsb 0 0 90) str
      -- 比べた組（最近のものを点線で）
      checkPic (i, j, hit) =
        let y1 = clampF 0 hh (yAt (segAt w i) sx)
            y2 = clampF 0 hh (yAt (segAt w j) sx)
        in dashed env (hsba (if hit then 350 else 45) 70 100 0.7) 2 4 (sx + 12, y1) (sx + 12, y2)
      -- 交点
      foundPic (i, j) =
        let Seg (x1, y1) (x2, y2) _ = segAt w i
            Seg (x3, y3) (x4, y4) _ = segAt w j
            den = (x2 - x1) * (y4 - y3) - (y2 - y1) * (x4 - x3)
            t = ((x3 - x1) * (y4 - y3) - (y3 - y1) * (x4 - x3)) / den
        in if abs den < 1e-9 then Blank
           else hoop env (hsb 45 80 100) 3 (x1 + t * (x2 - x1), y1 + t * (y2 - y1))
                  (24 + sin (fromIntegral (envFrame env) * 0.2) * 4)
      -- イベント点（x 軸上）
      evPic (k, e) = fillPoly env (if k < wEv w then hsb 0 0 40 else hsb 300 30 100)
        [(eX e, hh - 12), (eX e - 5, hh - 2), (eX e + 5, hh - 2)]
      -- パネル：掃引線の状態
      px = ww - 330
      rowPic (r, k) =
        let y = 76 + fromIntegral (r :: Int) * 36
            s = segAt w k
        in Pictures
             [ box env (hsb (sHue s) 55 90) (px + 18) y 34 28
             , txtC env (px + 35) (y + 14) 16 (hsb 0 0 10) (show k)
             , txtM env (px + 64) (y + 14) 16 (hsb 0 0 80) ("y = " ++ show (round (yAt s sx) :: Int)) ]
      by = hh - 230
      n = segCount
      found = wFound w
      foundLines
        | null found = ["No crossing yet"]
        | otherwise = take 3 (chunk 3 (map (\(i, j) -> show i ++ "x" ++ show j) found))
      foundPics = [ txt env (px + 18) (by + 90 + fromIntegral k * 22) 16
                      (if null found then hsb 0 0 95 else hsb 45 80 100) l
                  | (k, l) <- zip [0 :: Int ..] (if null found then foundLines else "Crossing found!" : foundLines) ]
      (note1, note2) = if wStop1st w
        then ("S: stop at the first crossing", "(just asks: is there one?)")
        else ("S: sweep to the end", "")
  in Pictures
       [ Pictures (map segPic (zip [0 ..] (wSegs w)))
       , Pictures (map checkPic (take 3 (wChecks w)))
       , Pictures (map foundPic found)
       , seg env (hsb 300 40 100) 2 (sx, 110) (sx, hh - 20)
       , Pictures (map evPic (zip [0 ..] (wEvents w)))
       , box env (hsb 260 35 18) px 20 310 (hh - 40)
       , txt env (px + 18) 36 17 (hsb 0 0 95) "Segments on the line"
       , Pictures (map rowPic (zip [0 ..] (wOnLine w)))
       , txt env (px + 18) by 17 (hsb 0 0 95) ("Event " ++ show (wEv w) ++ " / " ++ show (length (wEvents w)))
       , txt env (px + 18) (by + 28) 17 (hsb 0 0 95) ("Pairs checked: " ++ show (length (wChecks w)))
       , txt env (px + 18) (by + 56) 17 (hsb 0 0 60) ("All pairs: " ++ show (n * (n - 1) `div` 2))
       , Pictures foundPics
       , txt env (px + 18) (by + 168) 14 (hsb 0 0 60) note1
       , txt env (px + 18) (by + 188) 14 (hsb 0 0 60) note2
       , txt env 30 24 22 (hsb 0 0 95) "Sweep line: compare only neighbours"
       , txt env 30 56 14 (hsb 0 0 60) "Drag ends  Space play/stop  Right next event  R random  S stop mode  1-3 examples"
       ]

-- n 個ずつまとめて 1 行にする
chunk :: Int -> [String] -> [String]
chunk _ [] = []
chunk k xs = intercalate ", " (take k xs) : chunk k (drop k xs)

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouse env
        ends = concat [ [(i, True, a), (i, False, b)] | (i, Seg a b _) <- zip [0 ..] (wSegs w) ]
        near = [(i, isA) | (i, isA, p) <- ends, dist2 m p < 14]
    in pure w { wDrag = foldl (\_ d -> Just d) Nothing near }
  EventKey (MouseButton LeftButton) Up _ _ -> pure w { wDrag = Nothing }
  EventMotion _ | Just (i, isA) <- wDrag w ->
    let (mx, my) = mouse env
        p = (clampF 20 (envW env - 360) mx, clampF 110 (envH env - 20) my)
        put s = if isA then s { sA = p } else s { sB = p }
        segs = [if k == i then put s else s | (k, s) <- zip [0 ..] (wSegs w)]
        s' = segs !! i
        -- 左右が入れかわったら、つかんでいる端点の名前も入れかえる
        swapped = fst (sA s') > fst (sB s')
    in pure (restart w { wSegs = segs, wDrag = Just (i, if swapped then not isA else isA) })
  EventResize _ -> preset env 1 w
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wAuto = not (wAuto w) }
  EventKey (SpecialKey KeyRight) Down _ _ -> pure (processEvent w)
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> preset env 1 w
    | c `elem` "sS" -> pure (restart w { wStop1st = not (wStop1st w) })
    | c `elem` "123" -> preset env (fromEnum c - fromEnum '0') w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. お手本 2（交わらない）で、調べた組の数を全部の組の数と比べる
-- 2. S で最後まで掃引して、見つかる交差が全部ではないことを確かめる（順序が変わると見のがす）
-- 3. segCount を 16 にすると、調べる組の数はどれくらい増える？

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

