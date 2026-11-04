-- ============================================
-- 42-03 独立集合の公理
-- 台集合 E = {a, b, c, d}（E キーで 5 要素）の部分集合をハッセ図に並べる（下 = 空集合、上 = 全体）
--   クリックで「独立」にしたい集合を指定すると、マトロイドの 3 つの公理を自動で検査する
--   (I1) 空集合は独立　(I2) 独立集合の部分集合は独立　(I3) 交換公理：|A| < |B| なら B の要素を 1 つ A に足して独立にできる
-- 操作：クリック = 独立／従属を切替 / 1〜5 = 見本（U2,4・グラフ・I2 違反・I3 違反・自由） / E = 4 要素 ⇄ 5 要素 / C = 空集合だけ
-- ============================================
module Main (main) where

import Art
import Data.Bits (clearBit, popCount, setBit, testBit, (.&.))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Maybe (isNothing, listToMaybe)
import Data.List (intercalate, sortOn)

nElem0 :: Int
nElem0 = 4               -- ★ 台集合の要素数（4 か 5）

setNames :: String
setNames = "abcde"

data World = World
  { wN     :: !Int
  , wIndep :: !IS.IntSet            -- 独立な部分集合（ビットで表す）
  , wBadI2 :: ![(Int, Int)]         -- (I2) 違反のペア (A, B)
  , wBadI3 :: !(Maybe (Int, Int))   -- (I3) 違反のペア (A, B)
  }

main :: IO ()
main = runSketch (sketch "42-03 Independence axioms" (preset 1 world0))
  { skBack  = hsb 200 30 9
  , skDraw  = draw
  , skEvent = event
  }

world0 :: World
world0 = World nElem0 IS.empty [] Nothing

setName :: Int -> Int -> String
setName n m = case [ c | (i, c) <- zip [0 ..] (take n setNames), testBit m i ] of
  "" -> "{}"
  s -> s

-- ハッセ図の位置：同じ大きさの集合を左から辞書順に並べる
layout :: Env -> Int -> IM.IntMap Point
layout env n =
  let total = 2 ^ n :: Int
      top = 110
      bottom = envH env - 60
      usable = envW env - 540
      level k = sortOn (setName n) [ m | m <- [0 .. total - 1], popCount m == k ]
      place k =
        let ms = level k
            cnt = length ms
            y = remap (fromIntegral k) 0 (fromIntegral n) bottom top
        in [ (m, (40 + usable * (if cnt == 1 then 0.5 else (fromIntegral i + 0.5) / fromIntegral cnt), y))
           | (i, m) <- zip [0 :: Int ..] ms ]
  in IM.fromList (concatMap place [0 .. n])

-- ★ 見本の独立集合族
preset :: Int -> World -> World
preset k w =
  let n = wN w
      test m =
        let c = popCount m
        in case k of
             1 -> c <= 2                               -- U2,n：2 個までなら何でも独立
             2 -> m .&. 7 /= 7 && c <= 3               -- グラフ：a,b,c が三角形、d（,e）はぶら下がり
             3 -> c <= 1 || m == 3 || m == 7           -- ab と abc は独立なのに ac, bc が従属 → (I2) 違反
             4 -> c <= 1 || m == 3 || m == 12          -- ab と cd だけ → {a} と cd で (I3) 違反
             _ -> True                                 -- 自由マトロイド
  in check w { wIndep = IS.fromList (filter test [0 .. 2 ^ n - 1]) }

-- ★ 3 つの公理を検査する
check :: World -> World
check w =
  let n = wN w
      ind = wIndep w
      isInd m = IS.member m ind
      sets = IS.toAscList ind
      bad2 = [ (a, clearBit a i) | a <- sets, i <- [0 .. n - 1], testBit a i, not (isInd (clearBit a i)) ]
      exchangeOk a b = or [ isInd (setBit a i) | i <- [0 .. n - 1], testBit b i, not (testBit a i) ]
      bad3 = listToMaybe [ (a, b) | a <- sets, b <- sets, popCount b > popCount a, not (exchangeOk a b) ]
  in w { wBadI2 = bad2, wBadI3 = bad3 }

draw :: Env -> World -> Picture
draw env w =
  let n = wN w
      total = 2 ^ n :: Int
      ind = wIndep w
      isInd m = IS.member m ind
      pos = layout env n
      posOf m = IM.findWithDefault (0, 0) m pos
      okI1 = isInd 0
      okI2 = null (wBadI2 w)
      okI3 = isNothing (wBadI3 w)
      isBad2 a b = (a, b) `elem` wBadI2 w
      lowerBad m = any ((== m) . snd) (wBadI2 w)
      inI3 m = case wBadI3 w of { Just (a, b) -> a == m || b == m; Nothing -> False }
      -- ハッセ図の線（1 要素ちがい）
      hasseLine m i =
        let up = setBit m i
            both = isInd m && isInd up
            bad = isBad2 up m
            c | bad = hsba 5 80 100 0.9
              | both = hsba 130 80 80 0.9
              | otherwise = withA 0.6 (gray 0.3)
        in seg env (if bad then 4 else if both then 3 else 1) c (posOf m) (posOf up)
      hasse = [ hasseLine m i | m <- [0 .. total - 1], i <- [0 .. n - 1], not (testBit m i) ]
      i3Line = case wBadI3 w of
        Just (a, b) -> dashSeg env 3 8 8 (hsb 35 90 100) (posOf a) (posOf b)
        Nothing -> Blank
      hover = nearest env w
      -- 集合
      setPic m =
        let p@(x, y) = posOf m
            on = isInd m
            bad = (m == 0 && not on) || lowerBad m || inI3 m
            border | bad = hsb (if inI3 m && not (lowerBad m) then 35 else 5) 90 100
                   | otherwise = gray 0.6
            bw = if bad then 4 else 1.5
            wd = 26 + 10 * fromIntegral (max 1 (popCount m))
            fillC = if on then hsb 130 70 75 else gray 0.22
        in Pictures $
             [ pill env (wd + 10) 40 white p | m == hover ] ++
             [ pill env (wd + 6) 36 (hsb 200 30 9) p | m == hover ] ++
             [ pill env (wd + bw) (30 + bw) border p
             , pill env wd 30 fillC p
             , txtC env x y 15 (gray (if on then 0.1 else 0.7)) (setName n m) ]
      -- 基の大きさ
      sets = IS.toAscList ind
      maxSize = foldl' max 0 (map popCount sets)
      maximal = [ m | m <- sets, not (or [ isInd (setBit m i) | i <- [0 .. n - 1], not (testBit m i) ]) ]
      bases = filter ((== maxSize) . popCount) maximal
      px = envW env - 460
      py = 120
      okC b = hsb (if b then 130 else 5) 70 100
      mark b = if b then "[ok] " else "[x] "
      allOk = okI1 && okI2 && okI3
      name = setName n
      i2Note = case wBadI2 w of
        ((a, b) : r) -> "  " ++ name a ++ " indep. but " ++ name b ++ " not (" ++ show (length r) ++ " more)"
        [] -> ""
      i3Note = case wBadI3 w of
        Just (a, b) -> "  A = " ++ name a ++ ", B = " ++ name b ++ ": no element fits"
        Nothing -> "  |A| < |B|: some b in B can join A"
  in Pictures $
       hasse ++ [i3Line] ++ map setPic [0 .. total - 1] ++
       [ txt env 20 40 16 white ("Click: indep/dep  1-5: U2,4 / graph / I2 bad / I3 bad / free  E: size " ++ show n ++ "  C: empty only")
       , txt env 20 66 14 (gray 0.6) "Green = independent. Red line = (I2) broken. Orange dashes = (I3) broken pair."
       , box env (withA 0.08 white) (px - 20) (py - 40) 470 (envH env - py + 20)
       , stack env px py $
           [ (24, hsb (if allOk then 130 else 5) 80 100, if allOk then "Matroid!" else "Not a matroid")
           , (16, okC okI1, mark okI1 ++ "(I1) empty set is independent")
           , (16, okC okI2, mark okI2 ++ "(I2) subsets are independent")
           , (14, gray 0.7, i2Note)
           , (16, okC okI3, mark okI3 ++ "(I3) exchange axiom")
           , (14, gray 0.7, i3Note)
           , (8, white, "")
           , (16, white, "Rank (max size) " ++ show maxSize ++ "  bases " ++ show (length bases))
           , (14, gray 0.7, "Maximal sets:")
           ] ++ map (\ms -> (14, gray 0.7, "  " ++ intercalate ", " (map name ms))) (chunks6 maximal) ++
           [ (14, gray 0.7, "")
           , (14, gray 0.7, if length maximal == length bases
                              then "Maximal = maximum: matroid-like"
                              else "Maximal sets differ in size: (I3) is broken")
           , (14, gray 0.7, "Independent sets: " ++ show (IS.size ind) ++ " / " ++ show total)
           ]
       ]

-- 6 個ずつに分ける（長い列を折り返す）
chunks6 :: [a] -> [[a]]
chunks6 [] = []
chunks6 xs = take 6 xs : chunks6 (drop 6 xs)

nearest :: Env -> World -> Int
nearest env w =
  let (mx, my) = mouseP env
      pos = layout env (wN w)
      hits = [ m | (m, (x, y)) <- IM.toAscList pos, abs (mx - x) < 30, abs (my - y) < 16 ]
  in pick (-1) hits 0

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ _ ->
    let m = nearest env w
        flipSet s = if IS.member m s then IS.delete m s else IS.insert m s
    in if m >= 0 then check w { wIndep = flipSet (wIndep w) } else w
  EventKey (Char c) Down _ _
    | c >= '1' && c <= '5' -> preset (fromEnum c - fromEnum '0') w
    | c `elem` "eE" -> preset 1 w { wN = if wN w == 4 then 5 else 4 }
    | c `elem` "cC" -> check w { wIndep = IS.singleton 0 }
  _ -> w

-- ---------- やってみよう ----------
-- 1. 見本 1（U2,4）から ab を従属にする。どの公理が壊れる？ ab と ac を両方従属にすると？
-- 2. 空集合だけ（C）から、公理を壊さずに 1 つずつ独立集合を増やして「自分のマトロイド」を作る
-- 3. 見本 4 は (I1)(I2) は OK なのに (I3) が壊れている。極大な独立集合の大きさがそろわないのを確認
-- 4. 01 のグラフで「どの辺集合が森か」を思い出して、見本 2 がどんなグラフか絵に描いてみよう

-- 丸い両端の長方形（中心 p、幅 w、高さ h）
pill :: Env -> Float -> Float -> Color -> Point -> Picture
pill env w h c p =
  let (x, y) = sp env p
      r = h / 2
      d = max 0 (w / 2 - r)
  in Color c (Pictures [rect x y (2 * d) h, at (x - d, y) (disc r), at (x + d, y) (disc r)])

-- ==================================================
-- ここから下は「道具」
-- ==================================================
-- p5.js と同じ「左上が原点、y は下向き」の座標を gloss の座標（中央が原点、y は上向き）へ
sp :: Env -> Point -> Point
sp env (x, y) = (x - envW env / 2, envH env / 2 - y)

-- マウスの位置（左上が原点の座標で）
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- 文字（左下を指定）
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y size c s = let (a, b) = sp env (x, y) in label a b size c s

-- 文字（中央を指定）
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y size c s = let (a, b) = sp env (x, y) in labelC a b size c s

-- 上から順に文字を並べる：(大きさ, 色, 文字)。空の文字は行間だけあける
stack :: Env -> Float -> Float -> [(Float, Color, String)] -> Picture
stack env x y0 rows = Pictures (go y0 rows)
  where go _ [] = []
        go y ((sz, c, s) : r) = txt env x y sz c s : go (y + sz * 1.5) r

-- 太さ w の線分
seg :: Env -> Float -> Color -> Point -> Point -> Picture
seg env w c p q = Color c (thickLine w (sp env p) (sp env q))

-- 破線（on 描いて off あける）
dashSeg :: Env -> Float -> Float -> Float -> Color -> Point -> Point -> Picture
dashSeg env w on off c p@(x1, y1) q@(x2, y2) =
  let l = dist2 p q
      n = floor (l / (on + off)) :: Int
      pt d = (x1 + (x2 - x1) * d / max 1e-6 l, y1 + (y2 - y1) * d / max 1e-6 l)
  in Pictures [ seg env w c (pt d) (pt (min l (d + on))) | k <- [0 .. n], let d = fromIntegral k * (on + off), d < l ]

-- 塗りつぶしの長方形（左上の角 (x, y)、幅 w、高さ h）
box :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
box env c x y w h = let (a, b) = sp env (x + w / 2, y + h / 2) in Color c (rect a b w h)

-- リストの i 番目（範囲外なら d を返す安全版）
pick :: a -> [a] -> Int -> a
pick d xs i
  | i < 0 = d
  | otherwise = case drop i xs of { (x : _) -> x; [] -> d }
