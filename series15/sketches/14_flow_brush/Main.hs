-- ============================================
-- 14 水路をデザインする（自由制作のベース・音つき）
-- 自分でネットワークを描くと、その場で最大フロー（エドモンズ＝カープ法）を計算して水を流す
-- 増加道が見つかるたびに、道の点を順に鳴らす（高い位置の点ほど高い音）。黄色に光る線が最小カット = ボトルネック
-- 何もないところをクリック = 点 / 点から点へドラッグ = パイプ / パイプにマウスを乗せて上下 = 容量
-- D = マウスの下の点・パイプを消す / スペース = 増加道をもう一度鳴らす / C = 全部消す / 1〜3 = お手本
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM, forM)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (findIndex, foldl')

data Node = Node { nX :: !Float, nY :: !Float, nFixed :: Maybe String }

-- eId：粒がどのパイプの上にいるかを覚えるための番号
data Edge = Edge { eId :: !Int, eU :: !Int, eV :: !Int, eC :: !Int, eF :: !Int }

data Particle = Particle { pId :: !Int, pT :: !Float }

data AugPath = AugPath { apNodes :: [Int], apAmount :: !Int }

data World = World
  { wNodes :: [Node]
  , wEdges :: [Edge]
  , wNextId :: !Int
  , wDragFrom :: !Int
  , wPress :: !Point
  , wPaths :: [AugPath]
  , wFlow :: !Int
  , wReach :: !IS.IntSet
  , wParts :: [Particle]
  , wReplay :: !(Int, Int, Int)  -- (何本目, 何番目の点, タイマー)
  , wDefaultCap :: !Int          -- ★ 新しいパイプの容量
  }

-- ★ 音階
scaleHz :: [Float]
scaleHz = [261.6, 293.7, 329.6, 392.0, 440.0, 523.3, 587.3, 659.3, 784.0, 880.0]

maxParticles :: Int
maxParticles = 3000          -- 水の粒の上限（メモリと描画を抑える）

main :: IO ()
main = runSketch (sketch "15-14 Flow Brush" world0)
  { skBack  = hsb 185 35 8
  , skStart = \env -> preset env 1 world0
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

world0 :: World
world0 = World [] [] 0 (-1) (0, 0) [] 0 IS.empty [] (0, 0, 0) 5

-- お手本
preset :: Env -> Int -> World -> IO World
preset env k w0 = do
  let width = envW env
      height = envH env
      base = [Node 80 (height / 2) (Just "s"), Node (width - 80) (height / 2) (Just "t")]
      plain x y = Node (width * x) (height * y) Nothing
  (extra, es) <- case k of
    1 -> pure
      ( [plain 0.3 0.3, plain 0.3 0.7, plain 0.6 0.35, plain 0.6 0.7]
      , [(0, 2, 8), (0, 3, 6), (2, 4, 5), (3, 2, 3), (3, 5, 6), (4, 5, 2), (4, 1, 7), (5, 1, 8)] )
    2 -> do
      -- 川の合流と分岐
      cols <- forM [1 .. 4 :: Int] $ \col -> forM [0 .. 2 :: Int] $ \r -> do
        dy <- randomRIO (-30, 30)
        pure (Node (width * (0.12 + fromIntegral col * 0.17)) (height * (0.25 + fromIntegral r * 0.25) + dy) Nothing)
      let idsOf col = [2 + (col - 1) * 3 + r | r <- [0 .. 2]]
          link (prev, acc) col = do
            let cur = idsOf col
            new <- forM [(p, q) | p <- prev, q <- cur] $ \(p, q) -> do
              x <- randomRIO (0, 1 :: Float)
              c <- randomRIO (2, 8)
              pure [(p, q, c) | x < 0.6 || length prev == 1]
            pure (cur, acc ++ concat new)
      (lastCol, inner) <- foldM link ([0], []) [1 .. 4]
      outs <- forM lastCol $ \p -> do
        c <- randomRIO (3, 8)
        pure (p, 1, c)
      pure (concat cols, inner ++ outs)
    _ -> pure
      -- 細い 1 本がボトルネック
      ( [plain 0.3 0.3, plain 0.3 0.7, plain 0.5 0.5, plain 0.7 0.3, plain 0.7 0.7]
      , [(0, 2, 9), (0, 3, 9), (2, 4, 9), (3, 4, 9), (4, 5, 2), (4, 6, 2), (5, 1, 9), (6, 1, 9)] )
  let edges = [Edge (wNextId w0 + i) u v c 0 | (i, (u, v, c)) <- zip [0 ..] es]
  pure (solve w0 { wNodes = base ++ extra, wEdges = edges, wNextId = wNextId w0 + length es, wParts = [] })

-- エドモンズ＝カープ法で最大フロー。増加道を記録しておく
solve :: World -> World
solve w = go (0 :: Int) [e { eF = 0 } | e <- wEdges w] [] 0
  where
    finish es paths val seen = w
      { wEdges = es, wPaths = reverse paths, wFlow = val, wReach = seen, wReplay = (0, 0, 0) }
    go guard es paths val =
      let (seen, prev) = bfs es
      in if guard >= 500 || not (IS.member 1 seen)
           then finish es paths val seen
           else
             let back v acc
                   | v == 0 = acc
                   | otherwise = case IM.lookup v prev of
                       Just a@(_, _, from, _) -> back from (a : acc)
                       Nothing -> acc
                 p = back 1 []
                 b = foldl' (\m (_, _, _, r) -> min m r) maxBound p
                 apply e0 (i, dir, _, _) =
                   [if q == i then e { eF = eF e + dir * b } else e | (q, e) <- zip [0 ..] e0]
                 es' = foldl' apply es p
                 toOf (i, dir, _, _) = case drop i es of
                   (e : _) -> if dir > 0 then eV e else eU e
                   [] -> 0
             in go (guard + 1) es' (AugPath (0 : map toOf p) b : paths) (val + b)

-- 幅優先探索：残余ネットワークで s から行ける点と、たどってきた線 (線の番号, 向き, 前の点, 残り)
bfs :: [Edge] -> (IS.IntSet, IM.IntMap (Int, Int, Int, Int))
bfs es = loop [0] (IS.singleton 0) IM.empty
  where
    indexed = zip [0 ..] es
    loop [] seen prev = (seen, prev)
    loop (v : q) seen prev =
      let (seen', prev', new) = foldl' (visit v) (seen, prev, []) indexed
      in loop (q ++ reverse new) seen' prev'
    visit v st0 (i, e) =
      let st1 = forward v st0 i e
      in backward v st1 i e
    forward v (s, p, n) i e
      | eU e == v && not (IS.member (eV e) s) && eC e - eF e > 0 =
          (IS.insert (eV e) s, IM.insert (eV e) (i, 1, v, eC e - eF e) p, eV e : n)
      | otherwise = (s, p, n)
    backward v (s, p, n) i e
      | eV e == v && not (IS.member (eU e) s) && eF e > 0 =
          (IS.insert (eU e) s, IM.insert (eU e) (i, -1, v, eF e) p, eU e : n)
      | otherwise = (s, p, n)

stepW :: Env -> Float -> World -> IO World
stepW env _ w = do
  -- 水の粒
  born <- foldM spawn [] (wEdges w)
  let live = IS.fromList (map eId (wEdges w))
      parts = take maxParticles
        [p { pT = pT p + 0.01 } | p <- born ++ wParts w, pT p + 0.01 < 1, IS.member (pId p) live]
  -- 増加道のリプレイ（音つき）
  replay <- stepReplay env w
  pure w { wParts = parts, wReplay = replay }
  where
    spawn acc e
      | eF e <= 0 = pure acc
      | otherwise = do
          r <- randomRIO (0, 1 :: Float)
          pure (if r < fromIntegral (eF e) * 0.05 then Particle (eId e) 0 : acc else acc)

stepReplay :: Env -> World -> IO (Int, Int, Int)
stepReplay env w =
  let (k, i, t) = wReplay w
  in case drop k (wPaths w) of
       [] -> pure (k, i, t)
       (p : _)
         | (t + 1) `mod` 8 /= 0 -> pure (k, i, t + 1)
         | otherwise -> do
             case drop i (apNodes p) of
               (ni : _) | ni < length (wNodes w) -> do
                 let nd = wNodes w !! ni
                     q = max 0 (min (length scaleHz - 1) (floor (remap (nY nd) (envH env) 0 0 (fromIntegral (length scaleHz)))))
                 playTone (scaleHz !! q) 0.25 0.3 Triangle
               _ -> pure ()
             pure (if i + 1 >= length (apNodes p) then (k + 1, 0, t + 1) else (k, i + 1, t + 1))

posOf :: World -> Int -> Point
posOf w i = case drop i (wNodes w) of
  (n : _) -> (nX n, nY n)
  [] -> (0, 0)

drawW :: Env -> World -> Picture
drawW env w =
  let ns = wNodes w
      reach i = IS.member i (wReach w)
      hov = hoverEdge env w
      -- 最小カット側をうっすら
      clouds = [dot env (hsba 140 60 50 0.12) (nX n, nY n) 140 | (i, n) <- zip [0 ..] ns, reach i]
      pulse = (60 + 40 * sin (fromIntegral (envFrame env) * 0.1)) / 100
      edgePic (i, e) =
        let (x1, y1) = posOf w (eU e)
            (x2, y2) = posOf w (eV e)
            ang = atan2 (y2 - y1) (x2 - x1)
            a@(ax, ay) = (x1 + cos ang * 22, y1 + sin ang * 22)
            b@(bx, by) = (x2 - cos ang * 24, y2 - sin ang * 24)
            cut = reach (eU e) && not (reach (eV e))
            isHov = hov == Just i
        in Pictures
             [ seg env (if cut then hsba 50 90 100 pulse else hsb 185 15 30) (3 + fromIntegral (eC e) * 1.5) a b
             , if eF e > 0 then seg env (hsb 195 80 100) (1 + fromIntegral (eF e) * 1.5) a b else Blank
             , tip env (if isHov then hsb 50 90 100 else lum 80) b ang 13 0.45
             , txtC env ((ax + bx) / 2 - sin ang * 18) ((ay + by) / 2 + cos ang * 18) 14
                 (if isHov then hsb 50 90 100 else lum 80) (show (eF e) ++ "/" ++ show (eC e))
             ]
      byId = IM.fromList [(eId e, e) | e <- wEdges w]
      partPic p = case IM.lookup (pId p) byId of
        Just e ->
          let (x1, y1) = posOf w (eU e)
              (x2, y2) = posOf w (eV e)
          in dot env (hsba 190 60 100 0.85) (lerpF x1 x2 (pT p), lerpF y1 y2 (pT p)) 5
        Nothing -> Blank
      (rk, ri, _) = wReplay w
      replayPic = case drop rk (wPaths w) of
        (p : _) -> Color (hsb 50 90 100) (thickPath 4 (map (pt env . posOf w) (take (ri + 1) (apNodes p))))
        [] -> Blank
      dragPic
        | wDragFrom w >= 0 = seg env (hsb 50 90 100) 2 (posOf w (wDragFrom w)) (mouseP env)
        | otherwise = Blank
      nodePic (i, n) =
        let c = case nFixed n of
              Just "s" -> hsb 140 70 85
              Just _ -> hsb 20 75 95
              Nothing -> hsb 185 15 85
        in Pictures
             [ dot env c (nX n, nY n) 40
             , txtC env (nX n) (nY n) 15 (lum 10) (maybe (show (i :: Int)) id (nFixed n)) ]
      h = envH env
  in Pictures
       [ Pictures clouds
       , Pictures (map edgePic (zip [0 ..] (wEdges w)))
       , Pictures (map partPic (wParts w))
       , replayPic
       , dragPic
       , Pictures (map nodePic (zip [0 ..] ns))
       , txt env 20 36 15 (lum 100) "Click: node  Drag: pipe  Hover pipe + Up/Down: capacity  D: delete  Space: replay paths  C: clear  1-3: examples"
       , txt env 20 62 15 (lum 65) ("Glowing yellow pipes = min cut (widen these or the total stays the same). New pipe capacity " ++ show (wDefaultCap w))
       , txt env 20 (h - 30) 28 (lum 100) ("Max flow " ++ show (wFlow w))
       , txt env 300 (h - 32) 16 (lum 70) (show (length (wPaths w)) ++ " augmenting paths")
       ]

nodeAt :: World -> Point -> Int
nodeAt w p = maybe (-1) id (findIndex (\n -> dist2 p (nX n, nY n) < 24) (wNodes w))

-- マウスの下のパイプの番号（12 ピクセル以内でいちばん近いもの）
hoverEdge :: Env -> World -> Maybe Int
hoverEdge env w =
  let (mx, my) = mouseP env
      distTo e =
        let (ax, ay) = posOf w (eU e)
            (bx, by) = posOf w (eV e)
            dx = bx - ax
            dy = by - ay
            t = clampF 0 1 (((mx - ax) * dx + (my - ay) * dy) / max 1e-6 (dx * dx + dy * dy))
        in dist2 (mx, my) (ax + t * dx, ay + t * dy)
      pick (bd, best) (i, e) =
        let d = distTo e
        in if d < bd then (d, Just i) else (bd, best)
  in snd (foldl' pick (12, Nothing) (zip [0 ..] (wEdges w)))

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if k == i then v else x | (k, x) <- zip [0 ..] xs]

mouseUp :: Env -> World -> World
mouseUp env w =
  let m@(mx, my) = mouseP env
      moved = dist2 m (wPress w) > 8
      from = wDragFrom w
      w1 = w { wDragFrom = -1 }
  in if from >= 0 && moved
       then
         let to = nodeAt w m
             exists = any (\e -> (eU e == from && eV e == to) || (eU e == to && eV e == from)) (wEdges w)
         in if to >= 0 && to /= from && to /= 0 && from /= 1 && not exists
              then solve w1 { wEdges = wEdges w ++ [Edge (wNextId w) from to (wDefaultCap w) 0], wNextId = wNextId w + 1 }
              else case drop from (wNodes w) of
                (n : _) | to < 0 && maybe True (const False) (nFixed n) ->
                  w1 { wNodes = setAt from n { nX = mx, nY = my } (wNodes w) }
                _ -> w1
       else if from < 0 && not moved && my > 80 && hoverEdge env w == Nothing
         then w1 { wNodes = wNodes w ++ [Node mx my Nothing] }
         else w1

-- 上下キー：マウスの下のパイプの容量か、新しいパイプの容量を変える
changeCap :: Env -> Int -> World -> World
changeCap env d w = case hoverEdge env w of
  Just i -> solve w { wEdges = [if q == i then e { eC = max 1 (min 20 (eC e + d)) } else e | (q, e) <- zip [0 ..] (wEdges w)] }
  Nothing -> w { wDefaultCap = max 1 (min 20 (wDefaultCap w + d)) }

deleteAtMouse :: Env -> World -> World
deleteAtMouse env w =
  let i = nodeAt w (mouseP env)
  in if i >= 2
       then
         -- 点を消すとパイプを作り直すので、流れていた粒も消える
         let kept = [e | e <- wEdges w, eU e /= i, eV e /= i]
             down v = if v > i then v - 1 else v
             es = [ e { eId = wNextId w + q, eU = down (eU e), eV = down (eV e) } | (q, e) <- zip [0 ..] kept ]
         in solve w { wNodes = [n | (q, n) <- zip [0 ..] (wNodes w), q /= i], wEdges = es, wNextId = wNextId w + length es }
       else case hoverEdge env w of
         Just q -> solve w { wEdges = [e | (r, e) <- zip [0 ..] (wEdges w), r /= q] }
         Nothing -> w

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = mouseP env in pure w { wDragFrom = nodeAt w m, wPress = m }
  EventKey (MouseButton LeftButton) Up _ _ -> pure (mouseUp env w)
  EventKey (SpecialKey KeyUp) Down _ _ -> pure (changeCap env 1 w)
  EventKey (SpecialKey KeyDown) Down _ _ -> pure (changeCap env (-1) w)
  EventKey (SpecialKey KeySpace) Down _ _ -> pure w { wReplay = (0, 0, 0) }
  EventKey (Char c) Down _ _
    | c `elem` "dD" -> pure (deleteAtMouse env w)
    | c `elem` "cC" -> pure (solve w { wNodes = take 2 (wNodes w), wEdges = [] })
    | c `elem` "123" -> preset env (fromEnum c - fromEnum '0') w
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 最大フローを 20 にするネットワークを、パイプの容量の合計がなるべく小さくなるように作る
-- 2. 黄色い最小カットのパイプを 1 本太くすると、最大フローはいくつ増える？ 別のパイプなら？
-- 3. 音階 scaleHz を変えたり、増加道の量 apAmount で音の長さを変えたりして「水路の曲」を作る

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

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
