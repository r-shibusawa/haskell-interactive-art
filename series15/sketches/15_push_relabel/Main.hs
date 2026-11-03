-- ============================================
-- 15 プッシュ再ラベル法（発展）
-- 道を探すのをやめて、水を「高いところから低いところへ」押し流す
-- はじめに s を高さ |V| に持ち上げ、s から出るパイプを満杯にする。各点にたまった水（超過）を
-- プッシュ：1 段低い隣へ押し出す / 再ラベル：押し出せないなら自分を持ち上げる（隣の最低 + 1）
-- 1：一般のプッシュ再ラベル（ランダムに選ぶ）O(V²E)　2：リラベル・トゥ・フロント（リスト順に排出）O(V³)
-- スペース = 1 操作 / A = 自動 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.List (delete, isPrefixOf)
import qualified Data.Vector.Unboxed as U

names :: [String]
names = ["s", "a", "b", "c", "d", "t"]

numV :: Int
numV = 6

-- ★ ネットワーク（出発、到着、容量）
network :: [(Int, Int, Int)]
network = [(0, 1, 10), (0, 2, 12), (1, 2, 3), (1, 3, 9), (1, 4, 4), (2, 4, 11), (4, 3, 5), (3, 5, 15), (4, 5, 9)]

data World = World
  { wMode :: !Int
  , wCap :: !(U.Vector Int)     -- u * 6 + v で引く
  , wF :: !(U.Vector Int)
  , wH :: !(U.Vector Int)       -- 高さ
  , wEx :: !(U.Vector Int)      -- 超過
  , wCur :: !(U.Vector Int)
  , wL :: [Int]
  , wUIdx :: !Int
  , wLog :: [String]
  , wAuto :: !Bool
  , wTimer :: !Int
  , wPushes :: !Int
  , wRelabels :: !Int
  , wShownH :: [Float]          -- 画面に見せる高さ（なめらかに動く）
  , wLastEdge :: Maybe (Int, Int)
  }

main :: IO ()
main = runSketch (sketch "15-15 Push-Relabel" (start 1 []))
  { skBack  = hsb 250 30 9
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

ix :: Int -> Int -> Int
ix u v = u * numV + v

start :: Int -> [Float] -> World
start mode shown =
  let cap = U.replicate (numV * numV) 0 U.// [(ix u v, c) | (u, v, c) <- network]
      out = [(v, cap U.! ix 0 v) | v <- [0 .. numV - 1], cap U.! ix 0 v > 0]
      -- 前フロー：s から出るパイプを満杯に
      f = U.replicate (numV * numV) 0 U.// concat [[(ix 0 v, c), (ix v 0, negate c)] | (v, c) <- out]
      ex = U.accum (+) (U.replicate numV 0) (concat [[(v, c), (0, negate c)] | (v, c) <- out])
      h = U.replicate numV 0 U.// [(0, numV)]
  in World mode cap f h ex (U.replicate numV 0) [1, 2, 3, 4] 0
       ["Init: s height = |V| = " ++ show numV ++ ", fill all pipes out of s"]
       False 0 0 0
       (if null shown then map fromIntegral (U.toList h) else shown)
       Nothing

neighbors :: World -> Int -> [Int]
neighbors w u = [v | v <- [0 .. numV - 1], v /= u, wCap w U.! ix u v > 0 || wCap w U.! ix v u > 0]

cf :: World -> Int -> Int -> Int
cf w u v = wCap w U.! ix u v - wF w U.! ix u v

say :: String -> World -> World
say s w = let l = wLog w ++ [s] in w { wLog = drop (length l - 8) l }

pushFlow :: Int -> Int -> World -> World
pushFlow u v w =
  let d = min (wEx w U.! u) (cf w u v)
  in say ("Push " ++ nm u ++ " -> " ++ nm v ++ ": " ++ show d ++ " units (height of " ++ nm u ++ " " ++ show (wH w U.! u) ++ " -> " ++ nm v ++ " " ++ show (wH w U.! v) ++ ")")
       w { wF = U.accum (+) (wF w) [(ix u v, d), (ix v u, negate d)]
         , wEx = U.accum (+) (wEx w) [(u, negate d), (v, d)]
         , wPushes = wPushes w + 1
         , wLastEdge = Just (u, v) }

relabel :: Int -> World -> World
relabel u w =
  let m = foldr min 1000000 [wH w U.! v | v <- neighbors w u, cf w u v > 0]
      h' = 1 + m
  in say ("Relabel " ++ nm u ++ ": lowest usable neighbor " ++ show m ++ " + 1 = " ++ show h')
       w { wH = wH w U.// [(u, h')], wRelabels = wRelabels w + 1, wLastEdge = Nothing }

nm :: Int -> String
nm i = names !! i

finish :: World -> World
finish w =
  let w1 = w { wAuto = False }
      isDone = case reverse (wLog w) of
        (l : _) -> "Done" `isPrefixOf` l
        [] -> False
  in if isDone then w1 else say ("Done! No node has excess -> max flow = " ++ show (wEx w U.! 5)) w1

operate :: World -> IO World
operate w
  | wMode w == 1 =
      case [u | u <- [1, 2, 3, 4], wEx w U.! u > 0] of
        [] -> pure (finish w)
        act -> do
          q <- randomRIO (0, length act - 1)
          let u = act !! q
          pure $ case [v | v <- neighbors w u, cf w u v > 0, wH w U.! u == wH w U.! v + 1] of
            (v : _) -> pushFlow u v w
            [] -> relabel u w
  | otherwise = pure (operateFront w)

-- リラベル・トゥ・フロント：リスト L を前から見て、u の超過がなくなるまで排出（DISCHARGE）
operateFront :: World -> World
operateFront w = case drop (wUIdx w) (wL w) of
  [] -> finish w
  (u : _)
    | wEx w U.! u <= 0 ->
        let i = wUIdx w + 1
            w1 = w { wUIdx = i }
        in if i >= length (wL w) then finish w1 else w1
    | otherwise ->
        let w1 = case drop (wCur w U.! u) (neighbors w u) of
              [] ->
                -- 持ち上げたら u をリストの先頭へ
                say ("  -> " ++ nm u ++ " moves to the front of the list")
                  (relabel u w) { wCur = wCur w U.// [(u, 0)], wL = u : delete u (wL w), wUIdx = 0 }
              (v : _)
                | cf w u v > 0 && wH w U.! u == wH w U.! v + 1 -> pushFlow u v w
                | otherwise -> w { wCur = wCur w U.// [(u, wCur w U.! u + 1)] }
        in if wEx w1 U.! u <= 0 then w1 { wUIdx = wUIdx w1 + 1 } else w1

stepW :: Env -> Float -> World -> IO World
stepW _ _ w = do
  let shown = [lerpF s (fromIntegral h) 0.12 | (s, h) <- zip (wShownH w) (U.toList (wH w))]
      t = wTimer w + 1
      w1 = w { wShownH = shown }
  if wAuto w
    then if t `mod` 25 == 0 then operate w1 { wTimer = t } else pure w1 { wTimer = t }
    else pure w1

-- 縦の位置 = 高さ
unitOf :: Env -> Float
unitOf env = (envH env - 260) / fromIntegral (2 * numV)

nodePos :: Env -> World -> Int -> Point
nodePos env w i =
  let xs = [0.1, 0.29, 0.37, 0.56, 0.64, 0.83]  -- 同じ高さでも重ならないよう、a と b、c と d を少し左右にずらす
      zs = [0, -1, 1, -1, 1, 0]
  in (envW env * (xs !! i), envH env - 90 - (wShownH w !! i) * unitOf env + (zs !! i) * 18)

drawW :: Env -> World -> Picture
drawW env w =
  let h = envH env
      wd = envW env
      unit = unitOf env
      idx = [0 .. numV - 1]
      -- 高さの目盛り
      grid k =
        let y = h - 90 - fromIntegral k * unit
        in Pictures
             [ seg env (hsb 250 20 (if k == numV then 40 else 20)) 1 (40, y) (wd - 340, y)
             , txtR env 36 (y + 7) 14 (lum 50) (show k) ]
      pipe (u, v) =
        let (x1, y1) = nodePos env w u
            (x2, y2) = nodePos env w v
            ang = atan2 (y2 - y1) (x2 - x1)
            a@(ax, ay) = (x1 + cos ang * 26, y1 + sin ang * 26)
            b@(bx, by) = (x2 - cos ang * 28, y2 - sin ang * 28)
            hot = wLastEdge w == Just (u, v) || wLastEdge w == Just (v, u)
            cap = wCap w U.! ix u v
            fl = wF w U.! ix u v
        in Pictures
             [ seg env (hsb 250 15 32) (3 + fromIntegral cap * 0.4) a b
             , if fl > 0 then seg env (if hot then hsb 50 90 100 else hsb 195 80 95) (1 + fromIntegral fl * 0.4) a b else Blank
             , tip env (lum 85) b ang 13 0.45
             , txtC env ((ax + bx) / 2 - sin ang * 16) ((ay + by) / 2 + cos ang * 16) 14 (lum 85)
                 (show (max 0 fl) ++ "/" ++ show cap)
             ]
      nodePic i =
        let p@(x, y) = nodePos env w i
            ex = wEx w U.! i
            isCur = wMode w == 2 && take 1 (drop (wUIdx w) (wL w)) == [i]
            c | i == 0 = hsb 140 70 85
              | i == 5 = hsb 20 75 95
              | otherwise = hsb 250 12 88
        in Pictures
             [ -- 超過 = 水のかたまり
               if ex > 0 && i /= 0 && i /= 5 then dot env (hsba 195 80 100 0.5) p (50 + fromIntegral ex * 3) else Blank
             , dot env c p 46
             , if isCur then Color (hsb 50 90 100) (at (pt env p) (ring 23 4)) else Blank
             , txtC env x y 15 (lum 10) (nm i)
             , txtC env x (y + 36) 14 (lum 85)
                 ("h=" ++ show (wH w U.! i) ++ (if i /= 0 then "  e=" ++ show ex else ""))
             ]
      px = wd - 320
      listPanel
        | wMode w == 2 =
            [ txt env px 204 16 (lum 80) "List L:" ]
            ++ [ txt env (px + 80 + fromIntegral k * 46) 204 16 (if k == wUIdx w then hsb 50 90 100 else lum 80) (nm u)
               | (k, u) <- zip [0 :: Int ..] (wL w) ]
            ++ [ txt env px 228 14 (lum 65)
                   (nm u ++ " neighbors: " ++ unwords [ if k == wCur w U.! u then "[" ++ nm v ++ "]" else nm v
                                                      | (k, v) <- zip [0 :: Int ..] (neighbors w u) ])
               | u <- take 1 (drop (wUIdx w) (wL w)) ]
        | otherwise = []
      -- 記録は 2 行に折り返す
      logPics = concat
        [ [ txt env px (276 + fromIntegral k * 40 + fromIntegral q * 17) 14 (lum 75) l
          | (q, l) <- zip [0 :: Int ..] (take 2 (wrapWords 27 s)) ]
        | (k, s) <- zip [0 :: Int ..] (wLog w) ]
  in Pictures
       [ Pictures (map grid [0 .. 2 * numV])
       , Pictures (map pipe [(u, v) | u <- idx, v <- idx, wCap w U.! ix u v > 0])
       , Pictures (map nodePic idx)
       , txt env px 110 20 (lum 100) (if wMode w == 1 then "1: generic push-relabel" else "2: relabel-to-front")
       , txt env px 140 16 (lum 80) ("Push " ++ show (wPushes w) ++ "  Relabel " ++ show (wRelabels w))
       , txt env px 166 16 (lum 80) ("Arrived at t: " ++ show (wEx w U.! 5))
       , Pictures listPanel
       , Pictures logPics
       , txt env 20 36 18 (lum 100) "Space: 1 operation  A: auto  1: generic  2: relabel-to-front  R: restart"
       , txt env 20 62 14 (lum 65) "Height h = vertical position. Water flows only to a neighbor exactly 1 lower. Stuck water returns to s (lifted above |V|)"
       ]

-- 空白で折り返す
wrapWords :: Int -> String -> [String]
wrapWords n = go "" . words
  where
    go cur [] = [cur | not (null cur)]
    go cur (x : xs)
      | null cur = go x xs
      | length cur + 1 + length x <= n = go (cur ++ " " ++ x) xs
      | otherwise = cur : go x xs

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ -> operate w
  EventKey (Char c) Down _ _
    | c `elem` "aA" -> pure w { wAuto = not (wAuto w) }
    | c == '1' -> pure (start 1 (wShownH w))
    | c == '2' -> pure (start 2 (wShownH w))
    | c `elem` "rR" -> pure (start (wMode w) (wShownH w))
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 1 と 2 で、プッシュと再ラベルの回数を比べる（どちらも最大フローは 21）
-- 2. 途中で水が s に押し戻されるのはどんなとき？（s の高さ 6 を超えた点から）
-- 3. 高さが 2|V| − 1 を超えないことが、計算が必ず終わる理由になっている

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

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
