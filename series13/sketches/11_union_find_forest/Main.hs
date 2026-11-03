-- ============================================
-- 11 互いに素な集合の森 ─ ランクによる合併と経路圧縮
-- 集合を根つき木で表す。根が代表。FIND-SET は親をたどって根へ
-- 2 つの工夫：①低い木を高い木の下につなぐ（ランクによる合併）②たどった節点を根に直接つなぎ直す（経路圧縮）
-- 両方使うと m 回の操作が O(m α(n))。α はアッカーマン関数の逆関数で、現実には 4 以下 ─ ほぼ定数！
-- 節点クリック = FIND-SET / 節点から節点へドラッグ = UNION / スペース = ランダムに UNION / F = 一番深い節点を FIND / K = ランク ON/OFF / P = 経路圧縮 ON/OFF / C = 鎖を作る / R = やり直し
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', mapAccumL, maximumBy)
import Data.Ord (comparing)
import qualified Data.IntMap.Strict as IM

n :: Int
n = 20                -- ★ 要素の数

speed :: Float
speed = 0.35          -- ★ 1 コマの秒数

-- アニメーションの 1 コマ（p5.js 版の「あとで実行する関数」をデータにしたもの）
data Step
  = SFind !Int !String                 -- FIND-SET の計画を立てる
  | SHop [Int] !Int !Int !String       -- 道の途中を見せる（道、歩数、x、見出し）
  | SCompress [Int]                    -- 経路圧縮
  | SUnion !Int !Int                   -- UNION の計画を立てる
  | SUnion2 !Int !Int                  -- 2 つ目の FIND
  | SLink !Int !Int                    -- LINK

data World = World
  { wParent   :: !(IM.IntMap Int)
  , wRank     :: !(IM.IntMap Int)
  , wByRank   :: !Bool
  , wCompress :: !Bool
  , wPos      :: !(IM.IntMap Point)
  , wSteps    :: ![Step]
  , wTimer    :: !Float
  , wHiPath   :: ![Int]
  , wMsg      :: !String
  , wHops     :: !Int
  , wOps      :: !Int
  , wDrag     :: !(Maybe Int)
  }

main :: IO ()
main = runSketch (sketch "13-11 Union-Find Forest" (resetWorld (World IM.empty IM.empty True True IM.empty [] 0 [] "" 0 0 Nothing)))
  { skBack  = hsb 30 30 9
  , skStep  = \env dt w -> pure (stepWorld env dt w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

resetWorld :: World -> World
resetWorld w = w
  { wParent = IM.fromList [(i, i) | i <- [0 .. n - 1]]    -- MAKE-SET
  , wRank = IM.fromList [(i, 0) | i <- [0 .. n - 1]]
  , wSteps = [], wHops = 0, wOps = 0, wHiPath = []
  , wMsg = "MAKE-SET " ++ show n ++ " times: every element is its own root" }

parentOf :: World -> Int -> Int
parentOf w x = IM.findWithDefault x x (wParent w)

rootOf :: World -> Int -> Int
rootOf w x = let p = parentOf w x in if p == x then x else rootOf w p

depthOf :: World -> Int -> Int
depthOf w x = let p = parentOf w x in if p == x then 0 else 1 + depthOf w p

rankOf :: World -> Int -> Int
rankOf w x = IM.findWithDefault 0 x (wRank w)

-- x から根までの道
pathOf :: World -> Int -> [Int]
pathOf w x = let p = parentOf w x in if p == x then [x] else x : pathOf w p

-- FIND-SET をコマに分ける（実行する直前の木で道を調べる）
findSteps :: World -> Int -> String -> [Step]
findSteps w x lbl =
  let path = pathOf w x
  in [ SHop (take (i + 1) path) i x lbl | i <- [0 .. length path - 1] ]
     ++ [ SCompress path | wCompress w && length path > 2 ]

-- 1 コマ実行する。さらにコマが増えるときは、それを返す
runStep :: Step -> World -> (World, [Step])
runStep st w = case st of
  SFind x lbl -> (w, findSteps w x lbl)
  SHop sub i x lbl ->
    ( w { wHiPath = sub, wMsg = lbl ++ "FIND-SET(" ++ show x ++ "): follow parents (" ++ show i ++ " steps)"
        , wHops = wHops w + (if i > 0 then 1 else 0) }, [] )
  SCompress path ->
    let r = last path
    in ( w { wParent = foldl' (\m v -> IM.insert v r m) (wParent w) (init path), wHiPath = path
           , wMsg = "Path compression: link the " ++ show (length path - 1) ++ " visited nodes directly to root " ++ show r ++ " (1 step next time)" }, [] )
  SUnion a b -> (w, findSteps w a prep ++ [SUnion2 a b])
  SUnion2 a b -> (w, findSteps w b prep ++ [SLink a b])
  SLink a b -> (linkRoots a b w, [])
  where prep = "Before UNION: "

linkRoots :: Int -> Int -> World -> World
linkRoots a b w
  | x == y = w { wMsg = show a ++ " and " ++ show b ++ " are already in one set (root " ++ show x ++ ")" }
  | not (wByRank w) = hi (set x y) { wMsg = "LINK: root " ++ show x ++ " goes under root " ++ show y ++ " (ignoring ranks)" }
  | rx > ry = hi (set y x) { wMsg = "LINK: rank " ++ show ry ++ " < " ++ show rx ++ ": lower root " ++ show y ++ " goes under " ++ show x }
  | rx == ry =
      hi (set x y) { wRank = IM.insert y (ry + 1) (wRank w)
                   , wMsg = "LINK: same rank (" ++ show rx ++ "): " ++ show x ++ " goes under " ++ show y ++ ", rank of " ++ show y ++ " is now " ++ show (ry + 1) }
  | otherwise = hi (set x y) { wMsg = "LINK: rank " ++ show rx ++ " < " ++ show ry ++ ": lower root " ++ show x ++ " goes under " ++ show y }
  where
    x = rootOf w a
    y = rootOf w b
    rx = rankOf w x
    ry = rankOf w y
    set c p = w { wParent = IM.insert c p (wParent w) }
    hi v = v { wHiPath = [x, y] }

-- ---------- 配置（森）----------
data Layout = Layout { lTg :: IM.IntMap Point, lEdges :: [(Int, Int)], lU :: !Float, lRoots :: [Int], lMaxD :: !Int }

layoutOf :: Env -> World -> Layout
layoutOf env w = Layout (IM.fromList (concat placed)) (concatMap edgesOf roots) u roots maxD
  where
    all' = [0 .. n - 1]
    kidsMap = IM.fromListWith (flip (++)) [(parentOf w i, [i]) | i <- all', parentOf w i /= i]
    kids v = IM.findWithDefault [] v kidsMap
    roots = [i | i <- all', parentOf w i == i]
    leaves v = if null (kids v) then 1 else sum (map leaves (kids v)) :: Int
    total = sum [fromIntegral (leaves r) + 0.5 | r <- roots] :: Float
    maxD = foldl' max 0 (map (depthOf w) all')
    room = envW env - 360
    u = min 60 (room / max 1 total)
    lh = min 80 ((envH env - 330) / fromIntegral (max 1 maxD))
    x0 = 30 + (room - total * u) / 2
    (_, placed) = mapAccumL (\x r -> (x + (fromIntegral (leaves r) + 0.5) * u, snd (place r x 0))) x0 roots
    place v left d
      | null (kids v) = emit v (left + u / 2) d []
      | otherwise =
          let (_, res) = mapAccumL (\l c -> (l + fromIntegral (leaves c) * u, place c l (d + 1))) left (kids v)
          in emit v (midOf left (map fst res)) d (concatMap snd res)
    emit v cx d sub = (cx, (v, (cx, 190 + fromIntegral (d :: Int) * lh)) : sub)
    edgesOf v = [(v, c) | c <- kids v] ++ concatMap edgesOf (kids v)

-- ---------- 時間を進める ----------
stepWorld :: Env -> Float -> World -> World
stepWorld env dt w0 =
  let w1 = case wSteps w0 of
        [] -> w0
        (st : rest) ->
          let t = wTimer w0 + dt
          in if t > speed
               then
                 let (w', more) = runStep st w0 { wSteps = rest, wTimer = 0 }
                 in if null more then w' else w' { wSteps = more ++ rest, wTimer = speed }
               else w0 { wTimer = t }
      lay = layoutOf env w1
  in w1 { wPos = IM.mapWithKey (\v t -> lerpP 0.15 (IM.findWithDefault t v (wPos w1)) t) (lTg lay) }

-- ---------- 描画 ----------
drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (edgePics ++ dragPic ++ nodePics ++ ackPanel ++ panel)
  where
    lay = layoutOf env w
    posOf v = IM.findWithDefault (0, 0) v (wPos w)
    d = clampF 20 40 (lU lay * 0.7)
    hiP = wHiPath w
    mouse = mouseXY env
    edgePics = concat
      [ [ lineP env (if on then 3 else 1.6) col pp pc
        , triP env col (ex, ey)
            (ex - cos (a - 0.4) * 9, ey - sin (a - 0.4) * 9)
            (ex - cos (a + 0.4) * 9, ey - sin (a + 0.4) * 9) ]
      | (p, c) <- lEdges lay
      , let on = p `elem` hiP && c `elem` hiP
            col = if on then hsb 50 90 100 else hsb 30 20 50
            pp@(ppx, ppy) = posOf p
            pc@(pcx, pcy) = posOf c
            -- 親への矢印
            a = atan2 (ppy - pcy) (ppx - pcx)
            ex = ppx - cos a * d / 2
            ey = ppy - sin a * d / 2 ]
    dragPic = [ lineP env 2 (hsb 50 90 100) (posOf v) mouse | Just v <- [wDrag w] ]
    nodePics = concat
      [ [ discP env (if on then hsb 50 90 100 else hsb (fromIntegral ((r * 47) `mod` 360)) (if isRoot then 70 else 45) (if isRoot then 85 else 65)) p (d / 2) ]
        ++ [ ringP env (hsb 0 0 100) 2 p (d / 2) | dist2 mouse p < d / 2 ]
        ++ [ txtFit env (fst p) (snd p) (max 14 (d * 0.42)) (d - 2) (hsb 0 0 10) (show v) ]
        ++ [ txtC env (fst p) (snd p - d / 2 - 12) 14 (hsb 30 30 85) ("r" ++ show (rankOf w v)) | wByRank w && isRoot ]
      | v <- [0 .. n - 1]
      , let p = posOf v
            r = rootOf w v
            on = v `elem` hiP
            isRoot = parentOf w v == v ]
    -- アッカーマン関数とその逆関数
    px = envW env - 320
    ackPanel =
      [ fillBox env (hsb 30 15 15) (px - 12) 130 310 370
      , txt env px 142 16 (hsb 0 0 100) "Fast-growing A_k(1)" ] ++ concat
      [ [ txt env px (176 + j * 30) 14 (hsb 30 60 95) a
        , txt env (px + 70) (176 + j * 30) 14 (hsb 0 0 90) v
        , fillBox env (hsb 30 70 70) (px + 70) (194 + j * 30) bar 4 ]
      | (j, (a, v, bar)) <- zip [0 ..]
          [ ("A0(1)", "2", 10), ("A1(1)", "3", 15), ("A2(1)", "7", 35), ("A3(1)", "2047", 120)
          , ("A4(1)", ">> 10^80 (atoms!)", 220) ] ] ++
      [ txt env px (336 + fromIntegral j * 20) 14 (hsb 0 0 85) s
      | (j, s) <- zip [0 :: Int ..]
          [ "A_k(j) = apply A_(k-1)"
          , "j+1 times"
          , "alpha(n) = least k with"
          , "A_k(1) >= n"
          , "n <= 10^80: alpha <= 4"
          , ""
          , "Rank + compression:"
          , "m ops: O(m alpha(n))" ] ]
    panel =
      [ txt env 24 18 26 (hsb 0 0 100) "Disjoint-set forest"
      , txt env 24 56 15 (hsb 0 0 80)
          ("Union by rank: " ++ onOff (wByRank w) ++ " (K)   Path compression: " ++ onOff (wCompress w) ++ " (P)   sets " ++ show (length (lRoots lay))
           ++ "   tallest " ++ show (lMaxD lay) ++ "   parent hops " ++ show (wHops w))
      , txt env 24 88 17 (hsb 50 80 100) (wMsg w)
      , txt env 24 (envH env - 52) 14 (hsb 0 0 60) "Click = FIND-SET / Drag node to node = UNION / Space = random UNION / F = FIND the deepest node"
      , txt env 24 (envH env - 30) 14 (hsb 0 0 60) "K = rank / P = path compression / C = chain / R = reset   (r2 above a root = rank 2)"
      ]
    onOff b = if b then "ON" else "OFF"

nodeAt :: World -> Point -> Maybe Int
nodeAt w m = case [v | (v, p) <- IM.toList (wPos w), dist2 m p < 20] of
  (v : _) -> Just v
  [] -> Nothing

-- ---------- マウスとキー ----------
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wDrag = nodeAt w (mouseXY env) }
  EventKey (MouseButton LeftButton) Up _ _ ->
    pure $ case (wDrag w, nodeAt w (mouseXY env)) of
      (Just a, Just b)
        | a == b -> w { wDrag = Nothing, wOps = wOps w + 1, wSteps = wSteps w ++ [SFind a ""] }
        | otherwise -> (planUnion a b w) { wDrag = Nothing }
      _ -> w { wDrag = Nothing }
  EventKey (SpecialKey KeySpace) Down _ _ -> do
    a <- randomRIO (0, n - 1)
    b <- randomRIO (0, n - 1)
    pure (planUnion a b w)
  EventKey (Char c) Down _ _
    | c `elem` "fF" ->
        let deep = maximumBy (comparing (depthOf w)) [n - 1, n - 2 .. 0]
        in pure w { wOps = wOps w + 1, wSteps = wSteps w ++ [SFind deep ""] }
    | c `elem` "kK" -> pure w { wByRank = not (wByRank w) }
    | c `elem` "pP" -> pure w { wCompress = not (wCompress w) }
    -- 工夫なしだと 1 本の長い鎖になる並べ方
    | c `elem` "cC" -> pure (foldl' (\v i -> planUnion i (i + 1) v) (resetWorld w) [0 .. n - 2])
    | c `elem` "rR" -> pure (resetWorld w)
  _ -> pure w

planUnion :: Int -> Int -> World -> World
planUnion a b w = w { wOps = wOps w + 1, wSteps = wSteps w ++ [SUnion a b] }

-- ---------- やってみよう ----------
-- 1. K と P を両方 OFF にして C を押そう。木の高さはいくつになる？ F で FIND して歩数を見よう
-- 2. 片方だけ ON にすると高さはどう変わる？ 4 通りを表にしよう
-- 3. n を 40 にして、スペースをたくさん押そう。ランクはどこまで大きくなる？

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

-- 塗りつぶした長方形（x, y は左上）
fillBox :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
fillBox env c x y w h = let (gx, gy) = toG env (x + w / 2) (y + h / 2) in Color c (rect gx gy w h)

-- 塗りつぶした円（p は中心、r は半径）
discP :: Env -> Color -> Point -> Float -> Picture
discP env c (x, y) r = Color c (at (toG env x y) (disc (max 0.1 r)))

-- 円の輪郭
ringP :: Env -> Color -> Float -> Point -> Float -> Picture
ringP env c w (x, y) r = Color c (at (toG env x y) (ring (max 0.1 r) w))

-- 三角形
triP :: Env -> Color -> Point -> Point -> Point -> Picture
triP env c (a, b) (d, e) (f, g) = Color c (Polygon [toG env a b, toG env d e, toG env f g])

-- 2 点の間を t の割合で進む（p5.js の lerp を x, y に）
lerpP :: Float -> Point -> Point -> Point
lerpP t (a, b) (c, d) = (lerpF a c t, lerpF b d t)

-- 子の中心の真ん中（子がなければ d）
midOf :: Float -> [Float] -> Float
midOf d [] = d
midOf _ xs@(x : _) = (x + last xs) / 2

-- マスの中に入る文字（幅 maxW を超えるときは横に縮める）
txtFit :: Env -> Float -> Float -> Float -> Float -> Color -> String -> Picture
txtFit env x y size maxW c s =
  let (gx, gy) = toG env x y
      w = fromIntegral (length s) * size * 0.9
      k = min 1 (maxW / max 1 w)
  in Translate gx gy (Scale k 1 (labelC 0 0 size c s))
