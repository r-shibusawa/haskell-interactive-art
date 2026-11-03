-- ============================================
-- 15 タージャンのオフライン LCA ─ 深さ優先探索 × 互いに素な集合
-- 木の 2 点の「いちばん近い共通の祖先（LCA）」を、質問をまとめて 1 回の深さ優先探索で全部答える
-- 子を調べ終わるたびに子の集合を親の集合へ UNION し、その集合の ancestor を親にする。質問の相手がもう黒（調べ終わった）なら答え = ancestor[FIND-SET(相手)]
-- スペース = 1 歩 / A = 自動再生 / 2 つの節点を順にクリック = 質問を追加 / N = 新しい木 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', mapAccumL)
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence as Q

nn :: Int
nn = 15               -- ★ 節点の数

nq :: Int
nq = 5                -- ★ 最初の質問の数

speed :: Float
speed = 0.7           -- ★ 自動再生の秒数

-- アニメーションの 1 コマ（集合の状態をまるごと持つ）
data Frame = Frame
  { fU :: !Int, fMsg :: !String, fHiQ :: !Int
  , fParent :: !(IM.IntMap Int)      -- MAKE-SET の前は入っていない
  , fAnc    :: !(IM.IntMap Int)
  , fBlack  :: !(IM.IntMap Bool)
  , fAns    :: !(IM.IntMap Int)      -- 質問の番号 → 答え
  }

data World = World
  { wKids    :: !(IM.IntMap [Int])
  , wPar     :: !(IM.IntMap Int)
  , wQueries :: ![(Int, Int)]
  , wFrames  :: !(Q.Seq Frame)
  , wFi      :: !Int
  , wAuto    :: !Bool
  , wTimer   :: !Float
  , wPos     :: !(IM.IntMap Point)
  , wSel     :: !(Maybe Int)
  }

emptyFrame :: Frame
emptyFrame = Frame (-1) "" (-1) IM.empty IM.empty IM.empty IM.empty

initWorld :: World
initWorld = World IM.empty IM.empty [] Q.empty 0 False 0 IM.empty Nothing

main :: IO ()
main = runSketch (sketch "13-15 Tarjan Offline LCA" initWorld)
  { skBack  = hsb 210 30 9
  , skStart = \_ -> newTree initWorld
  , skStep  = \env dt w -> pure (stepWorld env dt w)
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- 親を前の節点からランダムに選ぶ（子は 3 人まで）
newTree :: World -> IO World
newTree w = do
  (kids, par) <- growTree 1 (IM.singleton 0 []) (IM.singleton 0 (-1))
  qs <- newQueries []
  pure (build w { wKids = kids, wPar = par, wQueries = qs })
  where
    growTree v kids par
      | v >= nn = pure (kids, par)
      | otherwise = do
          p <- choose kids v
          growTree (v + 1)
            (IM.insert v [] (IM.adjust (++ [v]) p kids))
            (IM.insert v p par)
    choose kids v = do
      p <- randomRIO (0, v - 1)
      if length (IM.findWithDefault [] p kids) >= 3 then choose kids v else pure p
    newQueries qs
      | length qs >= nq = pure qs
      | otherwise = do
          a <- randomRIO (0, nn - 1)
          b <- randomRIO (0, nn - 1)
          if a /= b && (a, b) `notElem` qs && (b, a) `notElem` qs
            then newQueries (qs ++ [(a, b)])
            else newQueries qs

-- LCA(u) の途中の状態
data B = B
  { bParent, bAnc :: !(IM.IntMap Int)
  , bBlack :: !(IM.IntMap Bool)
  , bAns   :: !(IM.IntMap Int)
  , bOut   :: ![Frame]
  }

snap :: Int -> String -> Int -> B -> B
snap u msg q b = b { bOut = Frame u msg q (bParent b) (bAnc b) (bBlack b) (bAns b) : bOut b }

findIn :: IM.IntMap Int -> Int -> Int
findIn par x = case IM.lookup x par of
  Just p | p /= x -> findIn par p
  _ -> x

-- LCA(u) を実行して、各段階をコマとして記録
build :: World -> World
build w =
  let kids v = IM.findWithDefault [] v (wKids w)
      queries = zip [0 :: Int ..] (wQueries w)
      lca b0 u =
        let b1 = snap u ("MAKE-SET(" ++ show u ++ "), ancestor = " ++ show u) (-1)
                   b0 { bParent = IM.insert u u (bParent b0), bAnc = IM.insert u u (bAnc b0) }
            b2 = foldl' (visitKid u) b1 (kids u)
            b3 = snap u ("Make " ++ show u ++ " black (its whole subtree is done)") (-1)
                   b2 { bBlack = IM.insert u True (bBlack b2) }
        in foldl' (ask u) b3 queries
      visitKid u b v =
        let b1 = lca b v
            ra = findIn (bParent b1) u
            rb = findIn (bParent b1) v
            par2 = IM.insert rb ra (bParent b1)        -- UNION(u, v)
            anc2 = IM.insert (findIn par2 u) u (bAnc b1)
        in snap u ("Child " ++ show v ++ " done: UNION(" ++ show u ++ ", " ++ show v ++ "), ancestor of the set = " ++ show u) (-1)
             b1 { bParent = par2, bAnc = anc2 }
      ask u b (qi, (a, c))
        | a /= u && c /= u = b
        | IM.findWithDefault False v (bBlack b) =
            let ans = IM.findWithDefault 0 (findIn (bParent b) v) (bAnc b)
                b1 = b { bAns = IM.insert qi ans (bAns b) }
            in snap u ("Query {" ++ show a ++ ", " ++ show c ++ "}: " ++ show v ++ " is black, LCA = ancestor[FIND-SET(" ++ show v ++ ")] = " ++ show ans) qi b1
        | otherwise =
            snap u ("Query {" ++ show a ++ ", " ++ show c ++ "}: " ++ show v ++ " is still white: answer when we visit " ++ show v) qi b
        where v = if a == u then c else a
      b0 = snap (-1) "Start the depth-first search: LCA(root)" (-1) (B IM.empty IM.empty IM.empty IM.empty [])
      bEnd = snap (-1) "All queries answered. O(n + |P|) set operations in all" (-1) (lca b0 0)
  in w { wFrames = Q.fromList (reverse (bOut bEnd)), wFi = 0 }

-- ---------- 配置 ----------
treeTargets :: Env -> World -> (IM.IntMap Point, Float)
treeTargets env w = (IM.fromList (snd (place 0 (30 + (room - total * u) / 2) 0)), u)
  where
    kids v = IM.findWithDefault [] v (wKids w)
    leaves v = if null (kids v) then 1 else sum (map leaves (kids v)) :: Int
    depthOf v = case IM.lookup v (wPar w) of
      Just p | p >= 0 -> 1 + depthOf p
      _ -> 0 :: Int
    room = envW env - 380
    total = fromIntegral (leaves 0) :: Float
    u = min 70 (room / total)
    maxD = foldl' max 0 (map depthOf [0 .. nn - 1])
    lh = min 90 ((envH env - 260) / fromIntegral (max 1 maxD))
    place v left d
      | null (kids v) = emit v (left + u / 2) d []
      | otherwise =
          let (_, res) = mapAccumL (\l c -> (l + fromIntegral (leaves c) * u, place c l (d + 1))) left (kids v)
          in emit v (midOf left (map fst res)) d (concatMap snd res)
    emit v cx d sub = (cx, (v, (cx, 160 + fromIntegral (d :: Int) * lh)) : sub)

-- ---------- 時間を進める ----------
stepWorld :: Env -> Float -> World -> World
stepWorld env dt w0 =
  let lastI = Q.length (wFrames w0) - 1
      w1 | wAuto w0 =
             let t = wTimer w0 + dt
             in if t > speed && wFi w0 < lastI then w0 { wTimer = 0, wFi = wFi w0 + 1 } else w0 { wTimer = t }
         | otherwise = w0
      (tg, _) = treeTargets env w1
  in w1 { wPos = IM.mapWithKey (\v t -> lerpP 0.15 (IM.findWithDefault t v (wPos w1)) t) tg }

-- ---------- 描画 ----------
drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures (edgePics ++ arcPics ++ nodePics ++ sidePanel ++ panel)
  where
    f = maybe emptyFrame id (Q.lookup (wFi w) (wFrames w))
    (_, u) = treeTargets env w
    posOf v = IM.findWithDefault (0, 0) v (wPos w)
    queries = zip [0 :: Int ..] (wQueries w)
    -- 辺
    edgePics =
      [ lineP env 2 (hsb 210 20 45) (posOf v) (posOf p)
      | v <- [1 .. nn - 1], Just p <- [IM.lookup v (wPar w)], p >= 0 ]
    -- 質問の弧
    arcPics =
      [ dashP env (if on then 3 else 2) col 6 6 (bezierPts a (mx, my) (mx, my) b)
      | (qi, (qa, qb)) <- queries
      , let a = posOf qa
            b = posOf qb
            on = fHiQ f == qi
            col | IM.member qi (fAns f) = hsba 50 85 100 0.8
                | on = hsb 0 0 100
                | otherwise = hsba 210 30 70 0.6
            mx = (fst a + fst b) / 2
            my = min (snd a) (snd b) - 60 - fromIntegral qi * 8 ]
    -- 節点
    d = clampF 22 40 (u * 0.6)
    answers = IM.elems (fAns f)
    nodePics = concat
      [ [ discP env (hsba hue 70 90 0.3) p (d * 0.9) | isAnc ]
        ++ [ discP env (if inSet then hsb hue 55 75 else hsb 210 10 30) p (d / 2) ]
        ++ [ ringP env edge sw p (d / 2) ]
        ++ [ txtC env (fst p) (snd p) (max 14 (d * 0.42)) (hsb 0 0 10) (show v) ]
        ++ [ txtC env (fst p) (snd p + d / 2 + 12) 14 (hsb 50 90 100) "LCA" | v `elem` answers ]
      | v <- [0 .. nn - 1]
      , let p = posOf v
            inSet = IM.member v (fParent f)
            rt = findIn (fParent f) v
            hue = fromIntegral ((rt * 53) `mod` 360) :: Float
            isAnc = inSet && IM.lookup rt (fAnc f) == Just v
            black = IM.findWithDefault False v (fBlack f)
            selected = wSel w == Just v
            cur = fU f == v
            edge | selected = hsb 0 0 100
                 | cur = hsb 50 90 100
                 | black = hsb 0 0 0
                 | otherwise = hsb 0 0 90
            sw | selected || cur || black = 4
               | otherwise = 1 ]
    -- パネル
    px = envW env - 330
    nQ = fromIntegral (length queries) :: Float
    sidePanel =
      [ txt env px 150 16 (hsb 0 0 100) "Queries P and answers" ] ++
      [ txt env px (180 + fromIntegral qi * 26) 15 (if fHiQ f == qi then hsb 50 90 100 else hsb 0 0 85)
          ("{" ++ show a ++ ", " ++ show b ++ "} -> " ++ maybe "?" (\v -> "LCA = " ++ show v) (IM.lookup qi (fAns f)))
      | (qi, (a, b)) <- queries ] ++
      [ txt env px (200 + nQ * 26 + fromIntegral j * 20) 14 (hsb 0 0 70) s
      | (j, s) <- zip [0 :: Int ..]
          [ "Color = set"
          , "Faint halo = set's ancestor"
          , "Black rim = done (BLACK)"
          , "Yellow rim = current node" ] ]
    panel =
      [ txt env 24 18 26 (hsb 0 0 100) "Tarjan's offline LCA"
      , txt env 24 56 15 (hsb 0 0 80) ("Frame " ++ show (wFi w + 1) ++ " / " ++ show (Q.length (wFrames w)))
      , txt env 24 86 16 (hsb 50 80 100) (fMsg f)
      , txt env 24 (envH env - 52) 14 (hsb 0 0 60)
          ("Space = 1 step / A = autoplay" ++ (if wAuto w then " (ON)" else "") ++ " / N = new tree / R = restart")
      , txt env 24 (envH env - 30) 14 (hsb 0 0 60) "Click 2 nodes in turn = add a query"
      ]

-- ---------- マウスとキー ----------
onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseXY env
    in pure $ case [v | v <- [0 .. nn - 1], Just p <- [IM.lookup v (wPos w)], dist2 m p < 20] of
         [] -> w { wSel = Nothing }
         (v : _) -> case wSel w of
           Nothing -> w { wSel = Just v }
           Just s
             | s == v -> w { wSel = Nothing }
             | otherwise ->
                 let qs = wQueries w ++ [(s, v)]
                 in (build w { wQueries = if length qs > 9 then drop 1 qs else qs }) { wSel = Nothing }
  EventKey (SpecialKey KeySpace) Down _ _ ->
    pure (if wFi w < Q.length (wFrames w) - 1 then w { wFi = wFi w + 1 } else w)
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c `elem` "nN" -> newTree w { wPos = IM.empty }
    | c `elem` "rR" -> pure w { wFi = 0 }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 質問の答えを自分で予想してから、スペースで確かめよう
-- 2. 同じ質問を {a, b} と {b, a} の順で足すと、答えが出る瞬間は変わる？
-- 3. nn を増やして、コマ数が n + |P| にほぼ比例することを確かめよう

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

-- 点線
dashP :: Env -> Float -> Color -> Float -> Float -> [Point] -> Picture
dashP env w c on off ps = Pictures (go on True ps)
  where
    go _ _ [] = []
    go _ _ [_] = []
    go left drawing (a : b : rest) =
      let l = dist2 a b
      in if l <= left
           then [lineP env w c a b | drawing] ++ go (left - l) drawing (b : rest)
           else let t = left / max 1e-6 l
                    m = (lerpF (fst a) (fst b) t, lerpF (snd a) (snd b) t)
                in [lineP env w c a m | drawing] ++ go (if drawing then off else on) (not drawing) (m : b : rest)

-- ベジェ曲線の上の点を並べる
bezierPts :: Point -> Point -> Point -> Point -> [Point]
bezierPts (ax, ay) (bx, by) (cx, cy) (dx, dy) =
  [ (f ax bx cx dx t, f ay by cy dy t) | i <- [0 .. 16 :: Int], let t = fromIntegral i / 16 ]
  where f a b c d t = let u = 1 - t in u * u * u * a + 3 * u * u * t * b + 3 * u * t * t * c + t * t * t * d

-- 塗りつぶした円（p は中心、r は半径）
discP :: Env -> Color -> Point -> Float -> Picture
discP env c (x, y) r = Color c (at (toG env x y) (disc (max 0.1 r)))

-- 円の輪郭
ringP :: Env -> Color -> Float -> Point -> Float -> Picture
ringP env c w (x, y) r = Color c (at (toG env x y) (ring (max 0.1 r) w))

-- 2 点の間を t の割合で進む（p5.js の lerp を x, y に）
lerpP :: Float -> Point -> Point -> Point
lerpP t (a, b) (c, d) = (lerpF a c t, lerpF b d t)

-- 子の中心の真ん中（子がなければ d）
midOf :: Float -> [Float] -> Float
midOf d [] = d
midOf _ xs@(x : _) = (x + last xs) / 2
