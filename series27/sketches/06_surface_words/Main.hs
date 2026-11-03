-- ============================================
-- 06 曲面の分類と連結和
-- 閉じた曲面は、多角形の辺を 2 本ずつ貼り合わせてつくれる。辺の読み方を「言葉」で書く：a b a⁻¹ b⁻¹ はトーラス
--   言葉から 頂点 V、辺 E、面 F = 1 を数えて χ = V − E + F。向きがつけられるか と χ だけで曲面は全部分類できる
-- a〜h = 辺を追加（Shift で逆向き a⁻¹） / Backspace = 1 つ消す / 0 = 全部消す / 1〜6 = 例 / + = トーラスを連結和 / * = 射影平面を連結和
-- ============================================
module Main (main) where

import Art
import Data.Char (isLower, toLower)
import Data.List (find, foldl', intercalate, nub)
import qualified Data.IntMap.Strict as IM

maxLen :: Int
maxLen = 16          -- ★ 言葉の長さの上限

width, height :: Float
width = 1280
height = 800

-- | 言葉の 1 文字：文字と向き（1 か −1）
type Sym = (Char, Int)

-- | 調べた結果
data Info
  = NoInfo
  | BadInfo [(Char, Int)]       -- 2 回でない文字
  | Info { iV, iE, iChi :: !Int, iOrient :: !Bool, iName, iSub :: String, iK :: !Int, iClasses :: [Int] }

data World = World { wWord :: ![Sym], wInfo :: !Info }

main :: IO ()
main = runSketch (sketch "27-06 Classifying Surfaces and Connected Sums" (setWord "abAB"))
  { skBack  = bgCol
  , skDraw  = \_ w -> drawWorld w
  , skEvent = \_ ev w -> pure (onEvent ev w)
  }

bgCol :: Color
bgCol = hsb 275 30 13

letterHue :: Char -> Float
letterHue c = case c of
  'a' -> 20
  'b' -> 200
  'c' -> 120
  'd' -> 300
  'e' -> 50
  'f' -> 260
  'g' -> 160
  _ -> 0

mkWorld :: [Sym] -> World
mkWorld ws = World ws (analyze ws)

setWord :: String -> World
setWord s = mkWorld [(toLower c, if isLower c then 1 else -1) | c <- s]

unusedLetter :: [Sym] -> Maybe Char
unusedLetter ws = find (\l -> l `notElem` map fst ws) "abcdefgh"

-- | ★ 言葉を調べる：頂点の貼り合わせ（Union-Find）→ V、向き付け可能か、χ
analyze :: [Sym] -> Info
analyze [] = NoInfo
analyze ws =
  let n = length ws
      letters = nub (map fst ws)
      count l = length (filter ((== l) . fst) ws)
      bad = [(l, count l) | l <- letters, count l /= 2]
      idx = zip [0 ..] ws
      ends (i, (_, e)) = if e > 0 then (i, (i + 1) `mod` n) else ((i + 1) `mod` n, i)   -- その辺の「矢印の根元・先」の頂点
      union m (a, b) =
        let ra = IM.findWithDefault a a m
            rb = IM.findWithDefault b b m
        in if ra == rb then m else IM.map (\x -> if x == ra then rb else x) m
      pairOf l = [s | s@(_, (c, _)) <- idx, c == l]
      glue (m, ori) l = case pairOf l of
        [s1, s2] ->
          let (t1, h1) = ends s1
              (t2, h2) = ends s2
              -- 同じ向きで 2 回出てくる → メビウスの帯が入っている
              ori' = ori && snd (snd s1) /= snd (snd s2)
          in (union (union m (t1, t2)) (h1, h2), ori')
        _ -> (m, ori)
      (roots, orientable) = foldl' glue (IM.fromList [(i, i) | i <- [0 .. n - 1]], True) letters
      classes = [IM.findWithDefault i i roots | i <- [0 .. n - 1]]
      v = length (nub classes)
      e = n `div` 2
      chi = v - e + 1
  in if not (null bad) then BadInfo bad
     else if orientable
       then
         let g = (2 - chi) `div` 2
             name | g == 0 = "Sphere"
                  | g == 1 = "Torus"
                  | otherwise = "Torus with " ++ show g ++ " holes"
         in Info v e chi True name (if g > 1 then "= sum of " ++ show g ++ " tori" else "") g classes
       else
         let k = 2 - chi
             name | k == 1 = "Projective plane"
                  | k == 2 = "Klein bottle"
                  | otherwise = "Sum of " ++ show k ++ " proj. planes"
         in Info v e chi False name (if k == 2 then "= sum of 2 projective planes" else "") k classes

onEvent :: Event -> World -> World
onEvent ev w = case ev of
  EventKey (SpecialKey KeyBackspace) Down _ _ -> backspace
  EventKey (SpecialKey KeyDelete) Down _ _ -> backspace
  EventKey (Char c) Down _ _ -> onChar c
  _ -> w
  where
    ws = wWord w
    backspace = mkWorld (if null ws then [] else init ws)
    onChar c
      | c == '\b' || c == '\DEL' = backspace
      | c == '0' = mkWorld []
      | c `elem` "123456" = setWord (presetWord c)
      | c == '+' || c == ';' = case unusedLetter ws of
          Just a | length ws + 4 <= maxLen -> case unusedLetter (ws ++ [(a, 1)]) of
            Just b -> mkWorld (ws ++ [(a, 1), (b, 1), (a, -1), (b, -1)])
            Nothing -> w
          _ -> w
      | c == '*' || c == ':' = case unusedLetter ws of
          Just a | length ws + 2 <= maxLen -> mkWorld (ws ++ [(a, 1), (a, 1)])
          _ -> w
      | c `elem` "abcdefghABCDEFGH" && length ws < maxLen =
          mkWorld (ws ++ [(toLower c, if isLower c then 1 else -1)])
      | otherwise = w

presetWord :: Char -> String
presetWord c = case c of
  '1' -> "aA"
  '2' -> "abAB"
  '3' -> "aa"
  '4' -> "abaB"
  '5' -> "abABcdCD"
  _ -> "aabb"

symText :: Sym -> String
symText (l, e) = l : (if e < 0 then "^-1" else "")

---------------------------------------------------------------- 描画

drawWorld :: World -> Picture
drawWorld w =
  let ws = wWord w
      n = length ws
      cx = width * 0.3
      cy = height * 0.55
      rR = min (width * 0.24) (height * 0.32)
      fn = fromIntegral (max n 1) :: Float
      -- 多角形の頂点（左下から反時計回り）
      verts = [ (cx + rR * cos a, cy + rR * sin a)
              | i <- [0 .. n - 1], let a = pi / 2 + pi / fn + fromIntegral i * tau / fn ]
      face | n >= 3 = Color (hsb 275 20 30) (Polygon (map pt verts))
           | n == 2 = at (pt (cx, cy)) (Scale (rR / 2) rR (Color (hsb 275 20 30) (disc 1)))
           | otherwise = Blank
      edgePic (i, (a, b), w1) =
        let (ax, ay) = a
            (bx, by) = b
            (l, e) = w1
            h = letterHue l
            col = hsb h 80 100
            bend = if n == 2 then (if i == (0 :: Int) then 1 else -1) * rR * 0.5 else 0
            mx = (ax + bx) / 2
            my = (ay + by) / 2
            ln0 = dist2 a b
            ln = if ln0 == 0 then 1 else ln0
            nx = -(by - ay) / ln
            ny = (bx - ax) / ln
            qx = mx + nx * bend
            qy = my + ny * bend
            curve = [ ((1 - s) * (1 - s) * ax + 2 * s * (1 - s) * qx + s * s * bx, (1 - s) * (1 - s) * ay + 2 * s * (1 - s) * qy + s * s * by)
                    | k <- [0 .. 20 :: Int], let s = fromIntegral k * 0.05 ]
            -- 矢印（e = −1 なら逆向き）
            fe = fromIntegral e
            tx = (bx - ax) / ln * fe
            ty = (by - ay) / ln * fe
            arx = if n == 2 then (ax + 2 * qx + bx) / 4 else mx
            ary = if n == 2 then (ay + 2 * qy + by) / 4 else my
            barb s = thickLine 6 (pt (arx + tx * 12, ary + ty * 12))
                       (pt (arx + tx * 12 - (tx * 22 + s * ty * 14), ary + ty * 12 - (ty * 22 - s * tx * 14)))
            lx = if n == 2 then arx + (arx - cx) * 0.35 else mx + (mx - cx) * 0.18
            ly = if n == 2 then ary else my + (my - cy) * 0.18
            (gx, gy) = pt (lx, ly)
        in Pictures
             [ Color col (thickPath 6 (map pt curve))
             , Color col (Pictures [barb (-1), barb 1])
             , labelC gx gy 22 (hsb h 70 100) (symText w1) ]
      edges = [ (i, (a, b), s)
              | (i, a, b, s) <- zip4' [0 ..] verts (drop 1 verts ++ take 1 verts) ws ]
      -- 頂点（同じ色 = 貼り合わせると同じ 1 点）
      classOf i = case wInfo w of
        Info { iClasses = cs } -> Just (case drop i cs of
                                          (c : _) -> c
                                          [] -> 0)
        _ -> Nothing
      vertPic (i, v) =
        let col = case classOf i of
              Just c -> hsb (fromIntegral ((c * 67) `mod` 360)) 60 100
              Nothing -> gray 1
        in at (pt v) (Pictures [Color black (disc 12), Color col (disc 10)])
      x0 = width * 0.58
      y0 = height * 0.22
      -- 言葉（長いときは 2 行に分ける）
      syms = map symText ws
      (line1, line2) = splitAt 8 syms
      wordLines | null ws = ["Word: (empty)"]
                | null line2 = ["Word: " ++ unwords line1]
                | otherwise = ["Word: " ++ unwords line1, "      " ++ unwords line2]
      maxChars = maximum (map length wordLines)
      wordSize = clampF 14 28 (530 / (0.8 * fromIntegral maxChars))
      wordPic = case wordLines of
        [a, b] -> Pictures [txt x0 (y0 - 30) wordSize (gray 1) a, txt x0 y0 wordSize (gray 1) b]
        _ -> Pictures [txt x0 y0 wordSize (gray 1) l | l <- wordLines]
      result = case wInfo w of
        i@Info {} -> Pictures
          [ txt x0 (y0 + 60) 18 (gray 1) ("Vertices V = " ++ show (iV i) ++ " (points after gluing)")
          , txt x0 (y0 + 96) 18 (gray 1) ("Edges E = " ++ show (iE i) ++ "   Faces F = 1")
          , txt x0 (y0 + 140) 20 (hsb 50 80 100) ("chi = V - E + F = " ++ show (iChi i))
          , txt x0 (y0 + 180) 15 (hsb (if iOrient i then 140 else 0) 60 100)
              (if iOrient i then "Orientable (each x appears with x^-1)" else "Non-orientable (a letter repeats same way)")
          , txt x0 (y0 + 236) 22 (gray 1) ("-> " ++ iName i)
          , txt (x0 + 40) (y0 + 266) 16 (gray 0.7) (iSub i)
          , drawSurface i (x0 + 20) (y0 + 310) ]
        BadInfo bad -> Pictures
          [ txt x0 (y0 + 60) 16 (hsb 0 70 100) "Each letter must appear exactly twice"
          , txt x0 (y0 + 86) 16 (hsb 0 70 100) ("(now " ++ intercalate ", " [l : ": " ++ show c | (l, c) <- take 4 bad] ++ ")")
          , if length bad > 4
              then txt x0 (y0 + 112) 16 (hsb 0 70 100) ("(" ++ intercalate ", " [l : ": " ++ show c | (l, c) <- drop 4 bad] ++ ")")
              else Blank ]
        NoInfo -> Blank
  in Pictures $
       [face] ++ map edgePic edges ++ map vertPic (zip [0 ..] verts) ++
       [ wordPic, result
       , txt 20 40 16 (gray 1) "a-h: edge (Shift: inverse)  Backspace  0: clear  1-6: examples  +: add torus  *: add proj. plane"
       , txt 20 68 14 (gray 0.6) "1 sphere aA   2 torus abAB   3 proj. plane aa   4 Klein abaB   5 torus#torus   6 proj#proj.   # = join words"
       ]

-- | 分類結果の模式図：穴 g 個のドーナツ、または「×」印（クロスキャップ）k 個
drawSurface :: Info -> Float -> Float -> Picture
drawSurface i x y =
  let k = iK i
      ori = iOrient i
      h = 120
      -- 画面からはみ出さないように穴の間隔を決める
      gap = if k <= 1 then 120 else min 120 ((width - 20 - x - 160) / fromIntegral (k - 1))
      w = 160 + gap * fromIntegral (max 0 (k - 1))
      col = Color (hsb (if ori then 200 else 20) 50 70)
      bodyPic = Pictures
        [ col (let (gx, gy) = pt (x + w / 2, y + h / 2) in rect gx gy (w - h) h)
        , col (at (pt (x + h / 2, y + h / 2)) (disc (h / 2)))
        , col (at (pt (x + w - h / 2, y + h / 2)) (disc (h / 2))) ]
      holePic j =
        let hx = x + 80 + fromIntegral j * gap
            hy = y + h / 2
        in if ori
             then at (pt (hx, hy)) (Scale 30 13 (Color bgCol (disc 1)))
             else Pictures
                    [ at (pt (hx, hy)) (Color bgCol (disc (min 25 (gap / 2 - 2))))
                    , Color (gray 1) (thickLine 3 (pt (hx - 14, hy - 14)) (pt (hx + 14, hy + 14)))
                    , Color (gray 1) (thickLine 3 (pt (hx - 14, hy + 14)) (pt (hx + 14, hy - 14))) ]
      c60 = gray 0.6
  in Pictures $
       [bodyPic] ++ map holePic [0 .. k - 1] ++
       (if ori
          then [txt x (y + h + 30) 14 c60 ("Holes g = (2 - chi) / 2 = " ++ show k)]
          else [ txt x (y + h + 30) 14 c60 "Crosscaps (holes glued with a Mobius strip):"
               , txt x (y + h + 52) 14 c60 ("count = 2 - chi = " ++ show k) ])

-- ---------- やってみよう ----------
-- 1. トーラスに射影平面を連結和すると（2 のあと *）、射影平面 3 つと同じになる。χ で確かめよう
-- 2. a b c a⁻¹ b⁻¹ c⁻¹（六角形）は何の曲面？ 頂点の色が何色になるか予想してから打ってみよう
-- 3. a b a b は射影平面、a a b b はクラインの壺。どの文字を入れかえると変わる？
-- 4. 同じ曲面になる言葉を 2 つ見つけて、「言葉はちがっても曲面は同じ」を体験しよう

-- ==================================================
-- ここから下は「道具」（座標の変換など）
-- ==================================================

pt :: (Float, Float) -> Point
pt (x, y) = (x - width / 2, height / 2 - y)

txt :: Float -> Float -> Float -> Color -> String -> Picture
txt x y s c str = let (gx, gy) = pt (x, y) in label gx gy s c str

zip4' :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
zip4' _ _ _ _ = []
