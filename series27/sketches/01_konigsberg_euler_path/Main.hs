-- ============================================
-- 01 ケーニヒスベルクの橋と一筆書き
-- 陸地を「点」、橋を「線」にすると、距離や形を忘れて「つながり方」だけが残る（オイラー 1736 年 ＝ トポロジーの始まり）
--   一筆書きできる ⇔ つながっていて、奇数本の線が出ている点が 0 個（元の点に戻る）か 2 個（端から端へ）
-- クリック = 点を追加 / 点から点へドラッグ = 線（橋）を追加 / Shift+ドラッグ = 点を動かす / スペース = 一筆書きを探してアニメ
-- K = ケーニヒスベルク / 2 = 家（封筒） / 3 = 星 / 4 = 橋を 1 本増やしたケーニヒスベルク / D = 最後の線を消す / X = マウスの点を消す / C = 全部消す
-- ============================================
module Main (main) where

import Art
import Data.List (elemIndex, findIndex)
import Data.Maybe (fromMaybe)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

vr, animSpeed :: Float
vr = 24              -- ★ 点の半径
animSpeed = 0.025    -- ★ アニメの速さ

width, height :: Float
width = 1280
height = 800

type P = (Float, Float)

-- | 一筆書きの道：出発点と、(通る線の番号, 着く点) の列
data EPath = EPath !Int ![(Int, Int)]

data World = World
  { wVerts :: ![P]              -- 点
  , wEdges :: ![(Int, Int)]     -- 線（a, b）
  , wDragFrom, wMoving :: !Int  -- -1 = なし
  , wAnim :: !(Maybe EPath)
  , wAnimT :: !Float
  , wRiver :: !Bool
  , wNoPath :: !Bool            -- 道が見つからなかった
  }

main :: IO ()
main = runSketch (sketch "27-01 Konigsberg Bridges and Euler Paths" (preset 'K' world0))
  { skBack  = hsb 205 35 14
  , skStep  = \env _ w -> pure (stepWorld (toP5 (envMouse env)) w)
  , skDraw  = \env w -> drawWorld (toP5 (envMouse env)) w
  , skEvent = \env ev w -> pure (onEvent (toP5 (envMouse env)) ev w)
  }

world0 :: World
world0 = World [] [] (-1) (-1) Nothing 0 False False

-- | 例を読み込む（A = 北岸、B = 南岸、C = 中の島、D = 東の島）
preset :: Char -> World -> World
preset k w =
  let cx = width / 2
      cy = height / 2 + 40
      s = min width height * 0.32
      base = w { wAnim = Nothing, wRiver = False, wNoPath = False, wDragFrom = -1, wMoving = -1 }
      kEdges = [(0, 2), (0, 2), (1, 2), (1, 2), (0, 3), (1, 3), (2, 3)]
      kVerts = [(cx - s * 0.3, cy - s * 0.9), (cx - s * 0.3, cy + s * 0.9), (cx - s * 0.3, cy), (cx + s * 0.9, cy)]
  in case k of
       'K' -> base { wVerts = kVerts, wEdges = kEdges, wRiver = True }
       '4' -> base { wVerts = kVerts, wEdges = kEdges ++ [(0, 1)], wRiver = True }
       '2' -> base { wVerts = [ (cx - s * 0.6, cy + s * 0.7), (cx + s * 0.6, cy + s * 0.7), (cx + s * 0.6, cy - s * 0.3)
                              , (cx - s * 0.6, cy - s * 0.3), (cx, cy - s * 0.95) ]
                   , wEdges = [(0, 1), (1, 2), (2, 3), (3, 0), (0, 2), (1, 3), (3, 4), (4, 2)] }
       _   -> base { wVerts = [ (cx + s * cos a, cy + s * sin a)
                              | i <- [0 .. 4 :: Int], let a = -pi / 2 + fromIntegral i * tau / 5 ]
                   , wEdges = concat [[(i, (i + 1) `mod` 5), (i, (i + 2) `mod` 5)] | i <- [0 .. 4]] }

degreeList :: World -> [Int]
degreeList w =
  let m = IM.fromListWith (+) (concat [[(a, 1), (b, 1)] | (a, b) <- wEdges w])
  in [IM.findWithDefault 0 i m | i <- [0 .. length (wVerts w) - 1]]

-- | 線のある点がぜんぶ 1 つにつながっているか（幅優先探索）
connected :: World -> [Int] -> Bool
connected w d = case findIndex (> 0) d of
  Nothing -> True
  Just s ->
    let adj = IM.fromListWith (++) (concat [[(a, [b]), (b, [a])] | (a, b) <- wEdges w])
        bfs seen [] = seen
        bfs seen (v : q) =
          let ns = [n | n <- IM.findWithDefault [] v adj, not (IS.member n seen)]
              seen' = foldr IS.insert seen ns
          in bfs seen' (q ++ ns)
        seen1 = bfs (IS.singleton s) [s]
    in and [x == 0 || IS.member i seen1 | (i, x) <- zip [0 ..] d]

-- | いまの状態の判定文
resultText :: World -> String
resultText w =
  let d = degreeList w
      odd' = length (filter odd d)
      base | null (wEdges w) = "Draw some lines"
           | not (connected w d) = "Not connected -> cannot be drawn in one stroke"
           | odd' == 0 = "0 odd points -> one stroke works (returns to start)"
           | odd' == 2 = "2 odd points -> one stroke works (odd point to odd point)"
           | otherwise = show odd' ++ " odd points -> one stroke is impossible!"
  in if wNoPath w then "No path: " ++ base else base

-- | ★ ヒールホルツァーの方法：行けるだけ進み、行き止まりで戻りながら道をつなぐ
eulerPath :: World -> Maybe EPath
eulerPath w =
  let d = degreeList w
      odds = [i | (i, x) <- zip [0 ..] d, odd x]
      edges = zip [0 :: Int ..] (wEdges w)
      adj = IM.fromListWith (flip (++)) (concat [[(a, [i]), (b, [i])] | (i, (a, b)) <- edges])
      edgeMap = IM.fromList edges
      other v ei = case IM.lookup ei edgeMap of
        Just (a, b) -> if a == v then b else a
        Nothing -> v
      -- stack の要素 = (点, そこへ来た線)。rest = 各点のまだ見ていない線
      go [] _ _ out = out
      go st@((v, e) : below) used rest out =
        case dropWhile (`IS.member` used) (IM.findWithDefault [] v rest) of
          [] -> go below used (IM.insert v [] rest) (if e >= 0 then (e, v) : out else out)
          (ei : more) -> go ((other v ei, ei) : st) (IS.insert ei used) (IM.insert v more rest) out
      start = case odds of
        (o : _) -> o
        [] -> fromMaybe 0 (findIndex (> 0) d)
  in if null (wEdges w) || not (connected w d) || (length odds /= 0 && length odds /= 2)
       then Nothing
       else Just (EPath start (go [(start, -1)] IS.empty adj []))

---------------------------------------------------------------- 動き

stepWorld :: P -> World -> World
stepWorld m w =
  let w1 = if wMoving w >= 0 then w { wVerts = setAt (wMoving w) m (wVerts w) } else w
  in case wAnim w1 of
       Nothing -> w1
       Just (EPath _ steps) ->
         let t = wAnimT w1 + animSpeed
         in w1 { wAnimT = if t > fromIntegral (length steps) + 1.5 then 0 else t }

onEvent :: P -> Event -> World -> World
onEvent mouse ev w = case ev of
  EventKey (Char c) Down _ _ -> onChar mouse c w
  EventKey (SpecialKey KeySpace) Down _ _ ->
    let p = eulerPath w in w { wAnim = p, wAnimT = 0, wNoPath = maybe True (const False) p }
  EventKey (MouseButton LeftButton) Down mods pos ->
    let m@(_, my) = toP5 pos
        v = vertexAt w m
    in if v >= 0
         then (if shift mods == Down then w { wMoving = v } else w { wDragFrom = v })
         else if my > 90 && my < height - 90 then w { wVerts = wVerts w ++ [m], wNoPath = False } else w
  EventKey (MouseButton LeftButton) Up _ pos ->
    let u = vertexAt w (toP5 pos)
        w1 = if wDragFrom w >= 0 && u >= 0 && u /= wDragFrom w
               then w { wEdges = wEdges w ++ [(wDragFrom w, u)], wAnim = Nothing, wNoPath = False }
               else w
    in w1 { wDragFrom = -1, wMoving = -1 }
  _ -> w

onChar :: P -> Char -> World -> World
onChar mouse c w
  | c `elem` "kK" = preset 'K' w
  | c `elem` "234" = preset c w
  | c `elem` "dD", not (null (wEdges w)) = w { wEdges = init (wEdges w), wAnim = Nothing, wNoPath = False }
  | c `elem` "xX" =
      let v = vertexAt w mouse
          shiftI i = if i > v then i - 1 else i
      in if v < 0 then w else
           w { wEdges = [(shiftI a, shiftI b) | (a, b) <- wEdges w, a /= v, b /= v]
             , wVerts = [p | (i, p) <- zip [0 ..] (wVerts w), i /= v]
             , wAnim = Nothing, wNoPath = False, wDragFrom = -1, wMoving = -1 }
  | c `elem` "cC" = w { wVerts = [], wEdges = [], wAnim = Nothing, wRiver = False, wNoPath = False, wDragFrom = -1, wMoving = -1 }
  | otherwise = w

---------------------------------------------------------------- 描画

drawWorld :: P -> World -> Picture
drawWorld mouse w =
  let d = degreeList w
      nE = length (wEdges w)
      -- アニメ：通り終わった線と、いま通っている線
      (doneE, curE, curP) = case wAnim w of
        Just (EPath start steps) | not (null steps) ->
          let k = min (length steps - 1) (floor (wAnimT w))
              done = map fst (take k steps)
              (e, _) = stepAt k steps
              from = if k == 0 then start else snd (stepAt (k - 1) steps)
          in (done, e, Just (edgePt w e (min 1 (wAnimT w - fromIntegral k)) from))
        _ -> ([], -1, Nothing)
      edgePic i (a, _) =
        let done = i `elem` doneE
            col = hsb (if done || i == curE then 45 else 200) (if done then 90 else 30) (if done then 100 else 80)
            ps = [pt (edgePt w i (fromIntegral s * 0.05) a) | s <- [0 .. 20 :: Int]]
        in Color col (thickPath (if done then 7 else 4) ps)
      dragLine = if wDragFrom w >= 0
                   then Color (hsb 45 80 100) (thickLine 3 (pt (vAt w (wDragFrom w))) (pt mouse))
                   else Blank
      vertPic (p, x) =
        let isOdd = odd x
            g = at (pt p)
            (gx, gy) = pt p
        in Pictures
             [ g (Color (hsb 220 40 25) (disc vr))
             , g (Color (hsb (if isOdd then 0 else 140) 80 100) (ring vr (if isOdd then 5 else 3)))
             , labelC gx gy 18 (gray 1) (show x) ]
      oddN = length (filter odd d)
  in Pictures $
       [ if wRiver w then river else Blank ]
       ++ zipWith edgePic [0 ..] (wEdges w)
       ++ [dragLine]
       ++ map vertPic (zip (wVerts w) d)
       ++ [maybe Blank (\p -> at (pt p) (Color (hsb 45 90 100) (disc 11))) curP]
       ++ [ txt 20 40 16 (gray 1) "Click: point   Point->point drag: line   Shift+drag: move   Space: one-stroke path"
          , txt 20 62 16 (gray 1) "K 2 3 4: examples   D: remove line   X: remove point   C: clear"
          , txt 20 84 14 (gray 0.6) "Number = lines at the point (degree). Red ring = odd. Odd points always come in pairs."
          , txt 20 (height - 60) 22 (hsb (if oddN == 0 || oddN == 2 then 140 else 0) 70 100) (resultText w)
          , txt 20 (height - 28) 16 (gray 0.7)
              ("Points " ++ show (length d) ++ "   Lines " ++ show nE ++ "   Sum of degrees " ++ show (sum d) ++ " = lines x 2")
          ]

stepAt :: Int -> [(Int, Int)] -> (Int, Int)
stepAt k steps = fromMaybe (-1, 0) (nth k steps)

-- | 線 i の上の点（t = 0〜1、from 側から）。平行な線は横にずらして弓なりに
edgePt :: World -> Int -> Float -> Int -> P
edgePt w i t0 from = case nth i (wEdges w) of
  Nothing -> (0, 0)
  Just (ea, eb) ->
    let (ax, ay) = vAt w ea
        (bx, by) = vAt w eb
        t = if from == eb then 1 - t0 else t0
        key (a, b) = (min a b, max a b)
        same = [j | (j, f) <- zip [0 ..] (wEdges w), key f == key (ea, eb)]
        k = fromMaybe 0 (elemIndex i same)
        off0 = (fromIntegral k - fromIntegral (length same - 1) / 2) * 70
        off = if ea > eb then -off0 else off0
        mx = (ax + bx) / 2
        my = (ay + by) / 2
        l0 = dist2 (ax, ay) (bx, by)
        l = if l0 == 0 then 1 else l0
        cx = mx - (by - ay) / l * off
        cy = my + (bx - ax) / l * off
        u = 1 - t
    in (u * u * ax + 2 * u * t * cx + t * t * bx, u * u * ay + 2 * u * t * cy + t * t * by)

-- | マウスの下の点（あとから足した点を優先）
vertexAt :: World -> P -> Int
vertexAt w m =
  case [i | (i, v) <- reverse (zip [0 ..] (wVerts w)), dist2 v m < vr + 4] of
    (i : _) -> i
    [] -> -1

-- ---------- やってみよう ----------
-- 1. K で「全部の点が奇数」を確認。4 で橋を 1 本足すと一筆書きできる。どこからどこへ？
-- 2. 自分の名前の漢字（「田」「日」「中」…）を点と線で描いて、一筆書きできるか調べよう
-- 3. 線をどれか 1 本消して、一筆書きできるように「直す」には最低何本消せばいい？
-- 4. 奇数の点が 4 個のとき、何回に分けて書けば全部の線を通れる？（答え：奇数の点の数 ÷ 2）

-- ==================================================
-- ここから下は「道具」（座標の変換、リスト、川を描く）
-- ==================================================

pt :: P -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

toP5 :: Point -> P
toP5 (x, y) = (x + width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = let (gx, gy) = pt (x, y) in label gx gy s c str

nth :: Int -> [a] -> Maybe a
nth i xs = case drop i xs of
  (x : _) | i >= 0 -> Just x
  _ -> Nothing

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = [if j == i then v else x | (j, x) <- zip [0 ..] xs]

vAt :: World -> Int -> P
vAt w i = fromMaybe (0, 0) (nth i (wVerts w))

-- | ケーニヒスベルクの雰囲気を出す川
river :: Picture
river =
  let cx = width / 2
      cy = height / 2 + 40
      s = min width height * 0.32
      c = Color (hsba 200 60 45 0.6)
      path w ps = c (thickPath w (map pt ps))
  in Pictures
       [ path (s * 0.3) [(-40, cy), (cx - s * 1.1, cy)]                                   -- 西で 1 本に合流
       , path (s * 0.3) [(cx - s * 1.1, cy), (cx - s * 0.8, cy - s * 0.45), (width + 40, cy - s * 0.45)]   -- 北の流れ
       , path (s * 0.3) [(cx - s * 1.1, cy), (cx - s * 0.8, cy + s * 0.45), (width + 40, cy + s * 0.45)]   -- 南の流れ
       , path (s * 0.18) [(cx + s * 0.3, cy - s * 0.45), (cx + s * 0.3, cy + s * 0.45)]   -- 島（C）と東の陸（D）の間の水路
       ]
