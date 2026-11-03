-- ============================================
-- 03 2 部グラフの最大マッチング（フローで解く）
-- 左 = 人、右 = 仕事。線 = 「その仕事ができる」。1 人 1 つ、1 つの仕事に 1 人で、なるべく多くペアを作る
-- s → 人 → 仕事 → t に容量 1 のパイプをつなぐと、最大フロー = 最大マッチング
-- 増加道は「マッチしていない線」と「マッチしている線（逆向き = ペアを解消）」を交互にたどる
-- スペース = 1 手 / A = 自動 / 左の点 → 右の点 とクリック = 線を足す・消す / R = ランダム / C = 線を全部消す
-- ============================================
module Main (main) where

import Art

people, jobs :: [String]
people = ["Person A", "Person B", "Person C", "Person D", "Person E", "Person F"]
jobs = ["Cooking", "Accounts", "Design", "Service", "Driving", "Translate"]

count' :: Int
count' = length people

edgeChance :: Float
edgeChance = 0.32            -- ★ ランダムに線を引く確率

-- 増加道の 1 本の線。sM = 今ペアの線（解消する）
data Step = Step { sI :: !Int, sJ :: !Int, sM :: !Bool }

data Phase = Find | Flip | Finished deriving Eq

data World = World
  { wAdj :: [[Int]]          -- wAdj !! i = 人 i ができる仕事
  , wMatchL :: [Int]
  , wMatchR :: [Int]
  , wPath :: Maybe [Step]
  , wPhase :: !Phase
  , wAuto :: !Bool
  , wTimer :: !Int
  , wPicked :: !Int
  , wCount :: !Int
  , wMessage :: String
  }

main :: IO ()
main = runSketch (sketch "15-03 Bipartite Matching" (resetMatch world0))
  { skBack  = hsb 255 30 9
  , skStart = \_ -> randomGraph world0
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

world0 :: World
world0 = World (map (const []) people) [] [] Nothing Find False 0 (-1) 0 ""

randomGraph :: World -> IO World
randomGraph w = do
  adj <- mapM (const (pickJobs)) people
  -- 「欲張りに選ぶと詰まる」形を少し混ぜる
  pure (resetMatch w { wAdj = [0, 1] : [0] : drop 2 adj })
  where
    pickJobs = do
      rs <- mapM (const (randomRIO (0, 1 :: Float))) jobs
      pure [j | (j, r) <- zip [0 ..] rs, r < edgeChance]

resetMatch :: World -> World
resetMatch w = w
  { wMatchL = map (const (-1)) people, wMatchR = map (const (-1)) jobs
  , wPath = Nothing, wPhase = Find, wCount = 0
  , wMessage = "Space: look for an augmenting path from a free person" }

lx, rx :: Env -> Int -> Float
lx env _ = envW env * 0.3
rx env _ = envW env * 0.7

ly, ry :: Env -> Int -> Float
ly env i = remap (fromIntegral i) 0 (fromIntegral (count' - 1)) (envH env * 0.2) (envH env * 0.85)
ry = ly

-- 増加道を幅優先で探す：左（未マッチ）→ 右 →（マッチの線で戻る）→ 左 → … → 右（未マッチ）
findAugmenting :: World -> Maybe [Step]
findAugmenting w =
  let mL = wMatchL w
      mR = wMatchR w
      free = [i | (i, m) <- zip [0 ..] mL, m < 0]
      go [] _ _ = Nothing
      go (i : q) prevR seenL = scan i (wAdj w !! i) q prevR seenL
      scan _ [] q prevR seenL = go q prevR seenL
      scan i (j : js) q prevR seenL
        | prevR !! j >= 0 || mL !! i == j = scan i js q prevR seenL
        | mR !! j < 0 = Just (cleanPath w j prevR')
        | k `elem` seenL = scan i js q prevR' seenL
        | otherwise = scan i js (q ++ [k]) prevR' (k : seenL)
        where
          prevR' = setAt j i prevR
          k = mR !! j
  in go free (map (const (-1)) jobs) free

-- 右の終点 j から、prevR とマッチをたどって交互道を作る
cleanPath :: World -> Int -> [Int] -> [Step]
cleanPath w j0 prevR = go j0 []
  where
    go j acc =
      let i = prevR !! j
          back = wMatchL w !! i
          acc1 = Step i j False : acc
      in if back < 0 then acc1 else go back (Step i back True : acc1)

advance :: World -> World
advance w = case wPhase w of
  Find -> case findAugmenting w of
    Nothing -> w { wPhase = Finished, wAuto = False
                 , wMessage = "No augmenting path -> maximum matching " ++ show (wCount w) ++ " pairs (= max flow)" }
    Just p -> w { wPath = Just p, wPhase = Flip
                , wMessage = "Augmenting path (yellow): dashed = unpair, solid = new pair. One more pair" }
  Flip ->
    let p = maybe [] id (wPath w)
        clear (l, r) s = if sM s then (setAt (sI s) (-1) l, setAt (sJ s) (-1) r) else (l, r)
        pair (l, r) s = if sM s then (l, r) else (setAt (sI s) (sJ s) l, setAt (sJ s) (sI s) r)
        (l1, r1) = foldl pair (foldl clear (wMatchL w, wMatchR w) p) p
        c = wCount w + 1
    in w { wMatchL = l1, wMatchR = r1, wCount = c, wPath = Nothing, wPhase = Find
         , wMessage = show c ++ " pairs. Find the next augmenting path" }
  Finished -> w

stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wAuto w =
      let t = wTimer w + 1
          w' = w { wTimer = t }
      in pure (if t `mod` 45 == 0 then advance w' else w')
  | otherwise = pure w

drawW :: Env -> World -> Picture
drawW env w =
  let sx = envW env * 0.1
      tx = envW env * 0.9
      cy = envH env * 0.52
      mL = wMatchL w
      mR = wMatchR w
      n = [0 .. count' - 1]
      lp i = (lx env i, ly env i)
      rp j = (rx env j, ry env j)
      sLines = [ seg env (hsb 140 50 (if on then 80 else 30)) (if on then 3 else 1) (sx, cy) (lp i)
               | i <- n, let on = mL !! i >= 0 ]
      tLines = [ seg env (hsb 20 50 (if on then 80 else 30)) (if on then 3 else 1) (rp j) (tx, cy)
               | j <- n, let on = mR !! j >= 0 ]
      links = [ seg env (if m then hsb 195 80 100 else hsb 255 15 45) (if m then 7 else 2) (lp i) (rp j)
              | i <- n, j <- wAdj w !! i, let m = mL !! i == j ]
      pathPic = [ (if sM s then dashed else seg) env (hsb 50 90 100) 5 (lp (sI s)) (rp (sJ s))
                | s <- maybe [] id (wPath w) ]
      leftNodes = [node env (lx env i) (ly env i) (people !! i) (mL !! i >= 0) (wPicked w == i) | i <- n]
      rightNodes = [node env (rx env j) (ry env j) (jobs !! j) (mR !! j >= 0) False | j <- n]
  in Pictures
       [ Pictures sLines, Pictures tLines, Pictures links, Pictures pathPic
       , dot env (hsb 140 70 85) (sx, cy) 44, dot env (hsb 20 75 95) (tx, cy) 44
       , txtC env sx cy 18 (lum 10) "s", txtC env tx cy 18 (lum 10) "t"
       , Pictures leftNodes, Pictures rightNodes
       , txt env 20 36 16 (lum 100) "Space: 1 step  A: auto  Click left then right: add/remove edge  R: random  C: clear all"
       , txt env 20 62 15 (lum 65) "Thin lines s->person, person->job, job->t all have capacity 1. Thick blue = matching (flow 1)"
       , txt env 20 (envH env - 26) 18 (hsb 45 80 100) (wMessage w)
       , txtR env (envW env - 30) 50 30 (lum 100) (show (wCount w) ++ " pairs")
       ]

node :: Env -> Float -> Float -> String -> Bool -> Bool -> Picture
node env x y s on sel =
  let c | sel = hsb 50 90 100
        | on = hsb 195 60 95
        | otherwise = hsb 255 10 85
  in Pictures [boxC env c x y 96 40, txtC env x y 14 (lum 10) s]

-- クリック：左の点を選び、右の点を押すと線を足す・消す
click :: Env -> World -> World
click env w =
  let (mx, my) = mouseP env
      n = [0 .. count' - 1]
      hitL = [i | i <- n, abs (mx - lx env i) < 50, abs (my - ly env i) < 22]
      hitR = [j | j <- n, abs (mx - rx env j) < 50, abs (my - ry env j) < 22, wPicked w >= 0]
  in case (hitL, hitR) of
       (i : _, _) -> w { wPicked = i }
       (_, j : _) ->
         let p = wPicked w
             old = wAdj w !! p
             new = if j `elem` old then filter (/= j) old else old ++ [j]
         in resetMatch w { wAdj = setAt p new (wAdj w), wPicked = -1 }
       _ -> w { wPicked = -1 }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure (click env w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure (advance w)
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "rR" -> randomGraph w
    | c `elem` "cC" -> pure (resetMatch w { wAdj = map (const []) people })
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. Person A→Cooking を先に選ぶと Person B が困る。増加道は「A さん、Accounts に移って」と自動で譲らせる
-- 2. 全員がペアになれない線の引き方を作る（ホールの定理：k 人ができる仕事が k 個未満だと無理）
-- 3. 1 回に 1 本ずつではなく、何本も同時に増やすのが 04 ホップクロフト＝カープ法

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 中心 (x, y)、幅 w、高さ h の塗りつぶし（rectMode(CENTER) の rect）
boxC :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
boxC env c x y w h = Color c (rect (gx env x) (gy env y) w h)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 点線（実線 10、すき間 8 をくり返す）
dashed :: Env -> Color -> Float -> Point -> Point -> Picture
dashed env c t (x1, y1) (x2, y2) =
  let l = dist2 (x1, y1) (x2, y2)
      at' d = let q = d / max 1e-6 l in (lerpF x1 x2 q, lerpF y1 y2 q)
  in Pictures [seg env c t (at' d) (at' (min l (d + 10))) | d <- takeWhile (< l) [0, 18 ..]]

-- 中心 (x, y)、直径 d の円（p5.js の circle）
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

-- 文字：txt は左下が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = label (gx env x - fromIntegral (length str) * s * 0.72) (gy env y) s c str

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0

-- リストの i 番目だけ置きかえる
setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]
