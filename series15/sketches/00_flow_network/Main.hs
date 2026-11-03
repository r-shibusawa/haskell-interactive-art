-- ============================================
-- 00 フローネットワーク
-- 線 = パイプ。数字は「流れている量 / 容量」。水は s（出発）から t（到着）へ流れる
-- 2 つのルールを守れば「フロー」：①容量を超えない ②途中の点では 入る量 = 出る量（フロー保存則）
-- 線をクリック = 流れを +1 / Shift + クリック = −1 / 1：お手本 / 2：出発点と到着点が複数 / S：超出発点・超到着点 / R：やり直し
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)

data Kind = Inner | Source | Sink | Super deriving Eq

-- 点の位置は画面の幅・高さに対する割合で持つ
data Node = Node { nName :: String, nFx :: !Float, nFy :: !Float, nKind :: !Kind }

data Edge = Edge { eU :: !Int, eV :: !Int, eC :: !Int, eF :: !Int, eSup :: !Bool }

-- 水の粒：どの線の上の、どこまで進んだか
data Particle = Particle { pE :: !Int, pT :: !Float }

data World = World
  { wNodes :: [Node]
  , wEdges :: [Edge]
  , wParts :: [Particle]
  , wSuper :: !Bool
  , wPreset :: !Int
  }

-- 容量 ∞ の代わり
inf :: Int
inf = maxBound

main :: IO ()
main = runSketch (sketch "15-00 Flow Network" (preset 1))
  { skBack  = hsb 222 45 9
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

edgesOf :: [(Int, Int, Int, Int)] -> [Edge]
edgesOf xs = [Edge u v c f False | (u, v, c, f) <- xs]

preset :: Int -> World
preset k
  | k == 1 = World
      -- お手本のネットワーク（値 15 のフローが流れている）
      [ Node "s" 0.12 0.55 Source
      , Node "a" 0.35 0.33 Inner, Node "b" 0.35 0.77 Inner
      , Node "c" 0.65 0.33 Inner, Node "d" 0.65 0.77 Inner
      , Node "t" 0.88 0.55 Sink ]
      (edgesOf [(0, 1, 10, 6), (0, 2, 12, 9), (1, 2, 3, 0), (1, 3, 9, 6), (1, 4, 4, 0), (2, 4, 11, 9), (4, 3, 5, 2), (3, 5, 15, 8), (4, 5, 9, 7)])
      [] False k
  | otherwise = World
      -- 工場が 3 つ（出発点）、倉庫が 2 つ（到着点）
      [ Node "s1" 0.14 0.3 Source, Node "s2" 0.14 0.55 Source, Node "s3" 0.14 0.8 Source
      , Node "a" 0.4 0.38 Inner, Node "b" 0.4 0.72 Inner, Node "c" 0.62 0.55 Inner
      , Node "t1" 0.84 0.4 Sink, Node "t2" 0.84 0.72 Sink ]
      (edgesOf [(0, 3, 6, 0), (1, 3, 4, 0), (1, 4, 5, 0), (2, 4, 7, 0), (3, 5, 6, 0), (4, 5, 5, 0), (3, 6, 4, 0), (5, 6, 5, 0), (5, 7, 6, 0), (4, 7, 4, 0)])
      [] False k

-- S キー：出発点を 1 つにまとめる「超出発点」、到着点をまとめる「超到着点」を足す（容量 ∞）
toggleSuper :: World -> World
toggleSuper w
  | wPreset w /= 2 = w
  | wSuper w = balanceSuper w
      { wSuper = False
      , wNodes = filter ((/= Super) . nKind) (wNodes w)
      , wEdges = filter (not . eSup) (wEdges w) }
  | otherwise =
      let ns = wNodes w
          s = length ns
          t = s + 1
          sups = concat
            [ [Edge s i inf 0 True | nKind n == Source] ++ [Edge i t inf 0 True | nKind n == Sink]
            | (i, n) <- zip [0 ..] ns ]
      in balanceSuper w
           { wSuper = True
           , wNodes = ns ++ [Node "s" 0.04 0.55 Super, Node "t" 0.96 0.56 Super]
           , wEdges = wEdges w ++ sups }

-- 超出発点・超到着点の線の流れは、つながる点の差し引きで自動的に決まる
balanceSuper :: World -> World
balanceSuper w = w { wEdges = map fix es }
  where
    es = wEdges w
    kindOf i = nKind (wNodes w !! i)
    fix e
      | not (eSup e) = e
      | otherwise =
          let i = if kindOf (eV e) == Source then eV e else eU e
              out = sum [eF d | d <- es, not (eSup d), eU d == i]
              inn = sum [eF d | d <- es, not (eSup d), eV d == i]
          in e { eF = max 0 (if kindOf i == Source then out - inn else inn - out) }

-- 点 i に入る量と出る量
balance :: [Edge] -> Int -> (Int, Int)
balance es i = (sum [eF e | e <- es, eV e == i], sum [eF e | e <- es, eU e == i])

flowValue :: World -> Int
flowValue w = sum
  [ out - inn
  | (i, n) <- zip [0 ..] (wNodes w), nKind n == Source
  , let (inn, out) = balance (wEdges w) i ]

stepW :: Env -> Float -> World -> IO World
stepW _ _ w = do
  -- 水の粒：流れの量に比例して生まれる（★ 0.04 を大きくすると粒が増える）
  born <- foldM spawn [] (zip [0 ..] (wEdges w))
  let moved = [p { pT = pT p + 0.012 } | p <- born ++ wParts w]
      n = length (wEdges w)
  pure w { wParts = [p | p <- moved, pT p < 1, pE p < n] }
  where
    spawn acc (i, e)
      | eF e <= 0 = pure acc
      | otherwise = do
          r <- randomRIO (0, 1 :: Float)
          pure (if r < min 0.9 (fromIntegral (min 1000 (eF e)) * 0.04) then Particle i 0 : acc else acc)

nodePos :: Env -> Node -> Point
nodePos env n = (envW env * nFx n, envH env * nFy n)

drawW :: Env -> World -> Picture
drawW env w =
  let es = wEdges w
      ns = wNodes w
      badOf i n = let (inn, out) = balance es i in nKind n == Inner && inn /= out
      ok = not (or (zipWith badOf [0 ..] ns)) && not (any (\e -> eF e > eC e) es)
      partPic p = case drop (pE p) es of
        (e : _) ->
          let (ax, ay) = nodePos env (ns !! eU e)
              (bx, by) = nodePos env (ns !! eV e)
          in dot env (hsba 195 70 100 0.8) (lerpF ax bx (pT p), lerpF ay by (pT p)) 6
        [] -> Blank
      nodePic (i, n) =
        let (x, y) = nodePos env n
            (inn, out) = balance es i
            inner = nKind n == Inner
            bad = badOf i n
            c | nKind n == Source || (nKind n == Super && nName n == "s") = hsb 140 70 80
              | nKind n == Sink || nKind n == Super = hsb 20 75 95
              | bad = hsb 0 80 90
              | otherwise = hsb 222 20 85
        in Pictures
             [ dot env c (x, y) 52
             , txtC env x y 17 (lum 10) (nName n)
             , if inner
                 then txtC env x (y + 42) 14 (if bad then hsb 0 70 100 else lum 75)
                        ("in " ++ show inn ++ " / out " ++ show out)
                 else Blank ]
      superHint
        | wPreset w == 2 = "  S: " ++ (if wSuper w then "remove" else "add") ++ " super s,t"
        | otherwise = ""
  in Pictures
       [ Pictures (map (pipe env ns) es)
       , Pictures (map partPic (wParts w))
       , Pictures (map nodePic (zip [0 ..] ns))
       , txt env 20 36 17 (lum 100) ("Click pipe = +1  Shift+click = -1  1: example  2: many sources/sinks" ++ superHint)
       , txt env 20 62 15 (lum 65) "Rule 1: flow <= capacity (else red)   Rule 2: at inner nodes, in = out (else red)"
       , txt env 20 (envH env - 30) 26 (if ok then hsb 140 60 95 else hsb 0 70 100)
           ((if ok then "Valid flow   " else "Rule broken   ") ++ "|f| = " ++ show (flowValue w))
       ]

-- パイプ：外側 = 容量、内側 = 流れ
pipe :: Env -> [Node] -> Edge -> Picture
pipe env ns e =
  let (ax, ay) = nodePos env (ns !! eU e)
      (bx, by) = nodePos env (ns !! eV e)
      ang = atan2 (by - ay) (bx - ax)
      p1 = (ax + cos ang * 28, ay + sin ang * 28)
      p2@(x2, y2) = (bx - cos ang * 30, by - sin ang * 30)
      isInf = eC e == inf
      over = eF e > eC e
      fF = fromIntegral (min 1000 (eF e)) :: Float
      cF = fromIntegral (eC e) :: Float
      full = if isInf then 0.3 else fF / max 1 cF
      mx = (fst p1 + x2) / 2 - sin ang * 18
      my = (snd p1 + y2) / 2 + cos ang * 18
      headC | over = hsb 0 80 100
            | full >= 1 = hsb 45 90 100
            | otherwise = lum 80
      textC | over = hsb 0 70 100
            | full >= 1 = hsb 45 90 100
            | otherwise = lum 95
  in Pictures
       [ seg env (hsb 222 15 35) (if isInf then 6 else 4 + cF * 0.7) p1 p2
       , if eF e > 0 then seg env (if over then hsb 0 80 100 else hsb 195 80 95) (2 + fF * 0.7) p1 p2 else Blank
       , tip env headC p2 ang 16 0.4
       , boxC env (hsba 222 40 15 0.85) mx my 62 24
       , txtC env mx my 15 textC (show (eF e) ++ " / " ++ (if isInf then "inf" else show (eC e)))
       ]

-- 点 p と線分 a-b の距離
segDist :: Point -> Point -> Point -> Float
segDist (px, py) (ax, ay) (bx, by) =
  let dx = bx - ax
      dy = by - ay
      l2 = max 1e-6 (dx * dx + dy * dy)
      t = clampF 0 1 (((px - ax) * dx + (py - ay) * dy) / l2)
  in dist2 (px, py) (ax + t * dx, ay + t * dy)

-- 16 ピクセル以内でいちばん近いパイプ（超出発点の線は選べない）
clickEdge :: Env -> Bool -> World -> World
clickEdge env sh w =
  let m = mouseP env
      cands = [ (segDist m (nodePos env (wNodes w !! eU e)) (nodePos env (wNodes w !! eV e)), i)
              | (i, e) <- zip [0 :: Int ..] (wEdges w), not (eSup e) ]
      best = foldl pick (16, -1) cands
      pick b@(bd, _) c@(d, _) = if d < bd then c else b
      bump (i, e)
        | i == snd best = e { eF = max 0 (eF e + (if sh then -1 else 1)) }
        | otherwise = e
  in if snd best < 0 then w else balanceSuper w { wEdges = map bump (zip [0 ..] (wEdges w)) }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down m _ -> clickEdge env (shift m == Down) w
  EventKey (Char c) Down _ _
    | c == '1' -> preset 1
    | c == '2' -> preset 2
    | c `elem` "sS" -> toggleSuper w
    | c `elem` "rR" -> preset (wPreset w)
  _ -> w

-- ---------- やってみよう ----------
-- 1. お手本で、フローの値を 15 から 21 まで増やせるか？（21 が最大）
-- 2. 2 で S を押す。出発点が何個あっても「s から t への 1 つの問題」にできる
-- 3. ルール ② を守ったまま、どこか 1 本だけ流れを増やすことはできる？（できない理由を考える）

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

-- 中心 (x, y)、直径 d の円（p5.js の circle）
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

-- 矢じり：先端 (x, y)、向き ang、長さ s、開き a
tip :: Env -> Color -> Point -> Float -> Float -> Float -> Picture
tip env c (x, y) ang s a = Color c (Polygon (map (pt env)
  [(x, y), (x - cos (ang - a) * s, y - sin (ang - a) * s), (x - cos (ang + a) * s, y - sin (ang + a) * s)]))

-- 文字：txt は左下が (x, y)、txtC は中心が (x, y)、txtR は右下が (x, y)
txt, txtC, txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
txtR env x y s c str = label (gx env x - fromIntegral (length str) * s * 0.72) (gy env y) s c str

-- 灰色（明るさ 0〜100）
lum :: Float -> Color
lum = hsb 0 0
