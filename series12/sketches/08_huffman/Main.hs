-- ============================================
-- 08 ハフマン符号：よく出る文字ほど短い 0/1 に
-- 出現回数の少ない 2 つを取り出して 1 つにまとめる、を繰り返すだけで最適な符号の木ができる
-- 取り出すのはいつも「いちばん小さい 2 つ」→ 優先度つきキュー（ヒープ）を使う貪欲法
-- キーボードで文を入力 / Enter = 自動で木をつくる / → = 1 回まとめる / Backspace = 1 文字消す / 下のボタンでお手本
-- ============================================
module Main (main) where

import Art
import Data.List (foldl', intercalate, nub, sortBy, sortOn)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M
import Text.Printf (printf)

startMessage :: String
startMessage = "abracadabra"   -- ★ 最初の文

framesPerMerge :: Int
framesPerMerge = 40            -- ★ まとめる間隔（フレーム）

-- 符号の木：葉は文字、節は 2 つの部分木
data Tree
  = Leaf !Char !Int !Int              -- 文字、回数、番号
  | Node Tree Tree !Int !Int !Int     -- 左、右、回数、番号、生まれたフレーム

data World = World
  { wMessage :: !String
  , wPreset  :: !(Maybe [(Char, Int)])
  , wForest  :: ![Tree]
  , wMerges  :: !Int
  , wAuto    :: !Bool
  , wTimer   :: !Int
  , wNextId  :: !Int
  }

main :: IO ()
main = runSketch (sketch "12-08 Huffman code" (rebuild (World startMessage Nothing [] 0 False 0 0)))
  { skBack  = cBg
  , skStep  = \env _ w -> pure (step env w)
  , skDraw  = drawWorld
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

freqOf :: Tree -> Int
freqOf (Leaf _ f _) = f
freqOf (Node _ _ f _ _) = f

idOf :: Tree -> Int
idOf (Leaf _ _ i) = i
idOf (Node _ _ _ i _) = i

-- 文字ごとの出現回数（最初に出てきた順）
freqTable :: World -> [(Char, Int)]
freqTable w = case wPreset w of
  Just p -> p
  Nothing -> [(c, length (filter (== c) (wMessage w))) | c <- nub (wMessage w)]

sortForest :: [Tree] -> [Tree]
sortForest = sortOn (\t -> (freqOf t, idOf t))

rebuild :: World -> World
rebuild w =
  let table = freqTable w
      leaves = [Leaf c f i | (i, (c, f)) <- zip [0 ..] table]
  in w { wForest = sortForest leaves, wNextId = length table, wMerges = 0, wAuto = False }

-- ★ いちばん小さい 2 つを取り出して（EXTRACT-MIN を 2 回）、まとめて戻す（INSERT）
mergeOnce :: Int -> World -> World
mergeOnce frame w = case wForest w of
  (x : y : rest) ->
    let node = Node x y (freqOf x + freqOf y) (wNextId w) frame
    in w { wForest = sortForest (rest ++ [node]), wNextId = wNextId w + 1, wMerges = wMerges w + 1 }
  _ -> w

leavesOf :: Tree -> Int
leavesOf (Leaf {}) = 1
leavesOf (Node l r _ _ _) = leavesOf l + leavesOf r

depthOf :: Tree -> Int
depthOf (Leaf {}) = 0
depthOf (Node l r _ _ _) = 1 + max (depthOf l) (depthOf r)

-- 木をたどって、左は 0、右は 1 を付ける
codes :: Tree -> M.Map Char String
codes = go ""
  where
    go prefix (Leaf c _ _) = M.singleton c (if null prefix then "0" else prefix)
    go prefix (Node l r _ _ _) = M.union (go (prefix ++ "0") l) (go (prefix ++ "1") r)

step :: Env -> World -> World
step env w
  | wAuto w && length (wForest w) > 1 =
      if wTimer w + 1 >= framesPerMerge then mergeOnce (envFrame env) w { wTimer = 0 } else w { wTimer = wTimer w + 1 }
  | otherwise = w

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case ev of
  EventKey (SpecialKey KeyEnter) Down _ _ -> enter
  EventKey (SpecialKey KeyRight) Down _ _ -> mergeOnce (envFrame env) w
  EventKey (SpecialKey KeyDelete) Down _ _ -> backspace
  EventKey (SpecialKey KeyBackspace) Down _ _ -> backspace
  EventKey (SpecialKey KeySpace) Down _ _ -> typed ' '
  EventKey (Char c) Down _ _
    | c == '\b' || c == '\DEL' -> backspace
    | c == '\r' -> enter
    | c >= ' ' && c <= '~' -> typed c
  EventKey (MouseButton LeftButton) Down _ _ ->
    case [act | (_, x, bw, act) <- buttons, mx > x, mx < x + bw, my > 146, my < 174] of
      (act : _) -> act w
      [] -> w
  _ -> w
  where
    (mx, my) = mouseP env
    enter = (if length (wForest w) <= 1 then rebuild w else w) { wAuto = True }
    backspace = rebuild w { wPreset = Nothing, wMessage = take (length (wMessage w) - 1) (wMessage w) }
    typed c | length (wMessage w) < 40 = rebuild w { wPreset = Nothing, wMessage = wMessage w ++ [c] }
            | otherwise = w

-- お手本のボタン（文字、左端、幅、動作）
buttons :: [(String, Float, Float, World -> World)]
buttons = go 32
  [ ("Frequency sample (a-f)", \w -> rebuild w { wPreset = Just [('a', 40), ('b', 8), ('c', 18), ('d', 6), ('e', 22), ('f', 6)] })
  , ("mississippi", \w -> rebuild w { wPreset = Nothing, wMessage = "mississippi river" })
  , ("Clear", \w -> rebuild w { wPreset = Nothing, wMessage = "" }) ]
  where
    go _ [] = []
    go x ((s, act) : rest) =
      let bw = i2f (length s) * 11.5 + 24
      in (s, x, bw, act) : go (x + bw + 10) rest

showCh :: Char -> String
showCh ' ' = "_"
showCh c = [c]

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env
  ( [ txt 26 cFg 32 24 "Huffman code: merge the two smallest, again and again"
    , txt 15 cMuted 32 60 "Type text   Enter: build automatically   Right: 1 merge   Backspace: delete"
    , rbox cPanel 32 92 (wd - 64) 44
    , txtL 22 cFg 46 114 (shownMsg ++ (if null (wPreset w) && envFrame env `mod` 40 < 20 then "|" else "")) ]
  ++ concat [ [rbox (rgb255 0x33 0x26 0x3d) x 146 bw 28, txtC 14 cFg (x + bw / 2) 160 s] | (s, x, bw, _) <- buttons ]
  ++ body )
  where
    wd = envW env
    ht = envH env
    forest = wForest w
    shownMsg = case wPreset w of
      Just p -> "(table) " ++ intercalate "  " [[k] ++ ":" ++ show v | (k, v) <- p]
      Nothing -> concatMap showCh (wMessage w)
    totalLeaves = sum (map leavesOf forest)
    body
      | totalLeaves == 0 = [txtC 18 cMuted (wd / 2) (ht / 2) "Type some characters"]
      | otherwise = trees ++ panel
    areaW = wd * 0.66 - 40
    unit = min 70 (areaW / i2f totalLeaves)
    maxD = maximum (0 : map depthOf forest)
    dy = min 70 ((ht - 330) / i2f (max 1 maxD))
    frame = envFrame env
    placed = scanl (\x t -> x + i2f (leavesOf t) * unit) 40 forest
    trees =
      [ snd (drawTree frame t x (x + i2f (leavesOf t) * unit) 230 dy (length forest > 1 && k < 2))
      | (k, t, x) <- zip3 [0 :: Int ..] forest placed ]
    -- 右パネル：符号表
    px = wd * 0.68
    pw = wd - px - 30
    panel = rbox cPanel px 200 pw (ht - 230) : (case forest of
      [only] -> finished only
      _ -> queue)
    queue =
      txt 16 cFg (px + 16) 214 "Queue (smallest first)"
      : [ txt 14 (if k < 2 then cAmber else cMuted) (px + 16) (244 + i2f k * 22)
            (show (freqOf t) ++ "  " ++ case t of
               Leaf c _ _ -> "'" ++ showCh c ++ "'"
               Node {} -> "(merged tree)")
        | (k, t) <- zip [0 :: Int ..] (take 12 forest) ]
      ++ [txt 14 cMuted (px + 16) (ht - 60) (show (length forest - 1) ++ " merges left")]
    finished only =
      let table = freqTable w
          cd = codes only
          code c = M.findWithDefault "0" c cd
          chars = sortBy (comparing (negate . snd)) table
          bits = sum [f * length (code c) | (c, f) <- chars]
          n = sum (map snd chars)
          perChar = max 1 (ceiling (logBase 2 (i2f (length chars)) :: Float)) :: Int
          fixed = perChar * n
          enc = unwords [code c | c <- wMessage w]
          encShort = if length enc > 106 then take 106 enc ++ "..." else enc
      in txt 16 cFg (px + 16) 214 "Finished code"
         : concat [ [ txt 14 cFg (px + 16) (244 + i2f k * 22) ("'" ++ showCh c ++ "' x" ++ show f)
                    , txt 14 cLime (px + 120) (244 + i2f k * 22) (code c) ]
                  | (k, (c, f)) <- zip [0 :: Int ..] (take 14 chars) ]
         ++ [ txt 15 cFg (px + 16) (ht - 110) ("Huffman: " ++ show bits ++ " bits")
            , txt 15 cMuted (px + 16) (ht - 86) ("Fixed (" ++ show perChar ++ " bits/char): " ++ show fixed ++ " bits")
            , txt 15 cAmber (px + 16) (ht - 62) (show (if fixed > 0 then round (100 * i2f bits / i2f fixed :: Float) else 0 :: Int) ++ "% of the length") ]
         ++ (case wPreset w of
               Nothing -> [ txt 14 cMuted 40 (ht - 66 + i2f l * 20) s | (l, s) <- zip [0 :: Int ..] (chunk 55 encShort) ]
               Just _ -> [])
    chunk k s = case splitAt k s of
      (a, []) -> [a]
      (a, b) -> a : chunk k b

-- 木を [x0, x1] の幅に描き、根の座標と絵を返す
drawTree :: Int -> Tree -> Float -> Float -> Float -> Float -> Bool -> ((Float, Float), Picture)
drawTree _ (Leaf c f _) x0 x1 y _ pick =
  let cx = (x0 + x1) / 2
      bw = clampF 18 40 (x1 - x0 - 2)
  in ( (cx, y)
     , Pictures [ rbox (if pick then cAmber else cRasp) (cx - bw / 2) (y - 20) bw 40
                , txtC 16 cBg cx (y - 9) (showCh c)
                , txtC 14 cBg cx (y + 11) (show f) ] )
drawTree frame (Node l r f _ born) x0 x1 y dy pick =
  let ll = i2f (leavesOf l)
      lr = i2f (leavesOf r)
      mid = x0 + (x1 - x0) * ll / (ll + lr)
      ((ax, ay), pa) = drawTree frame l x0 mid (y + dy) dy False
      ((bx, by), pb) = drawTree frame r mid x1 (y + dy) dy False
      cx = (ax + bx) / 2
      fresh = frame - born < 30
      edge = rgb255 0x6b 0x5a 0x78
  in ( (cx, y)
     , Pictures
         [ pa, pb
         , seg edge 2 (cx, y) (ax, ay - 20), seg edge 2 (cx, y) (bx, by - 20)
         , txtC 14 cCyan ((cx + ax) / 2 - 10) ((y + ay) / 2 - 8) "0"
         , txtC 14 cCyan ((cx + bx) / 2 + 10) ((y + by) / 2 - 8) "1"
         , circ (if pick then cAmber else if fresh then cLime else rgb255 0x4a 0x3d 0x56) cx y 38
         , txtC 14 (if pick || fresh then cBg else cFg) cx y (show f) ] )

-- ---------- やってみよう ----------
-- 1. 同じ文字ばかりの文と、全部ちがう文字の文で、何 % になるか比べる
-- 2. startMessage を自分の名前に変える
-- 3. mergeOnce で「いちばん大きい 2 つ」をまとめたら、符号はどれだけ長くなる？

-- ---------- 道具：色と、左上が原点の座標で描く関数（p5.js の座標のまま書ける） ----------

cBg, cPanel, cFg, cMuted, cRasp, cCyan, cAmber, cLime, cGrid :: Color
cBg    = rgb255 0x16 0x11 0x1c
cPanel = rgb255 0x23 0x1a 0x2b
cFg    = rgb255 0xf3 0xea 0xf0
cMuted = rgb255 0x9a 0x8a 0xa3
cRasp  = rgb255 0xff 0x6b 0x8b
cCyan  = rgb255 0x5e 0xe6 0xe0
cAmber = rgb255 0xff 0xc8 0x57
cLime  = rgb255 0xa3 0xe6 0x35
cGrid  = rgb255 0x3a 0x2f 0x44

-- | 画面全体を「左上が原点」の座標に移す（中の図形は y を下向きで渡す）
screen :: Env -> [Picture] -> Picture
screen env = Translate (negate (envW env) / 2) (envH env / 2) . Pictures

-- | マウスの位置を左上原点の座標で返す
mouseP :: Env -> Point
mouseP env = let (x, y) = envMouse env in (x + envW env / 2, envH env / 2 - y)

-- | 角を落とした四角の頂点（p5.js の rect の角丸の代わり）
cornerPts :: Float -> Float -> Float -> Float -> [Point]
cornerPts x y w h =
  let r = min 5 (min (abs w) (abs h) / 3)
  in [ (x + r, -y), (x + w - r, -y), (x + w, -y - r), (x + w, -y - h + r)
     , (x + w - r, -y - h), (x + r, -y - h), (x, -y - h + r), (x, -y - r) ]

-- | 角を落とした塗りつぶしの四角
rbox :: Color -> Float -> Float -> Float -> Float -> Picture
rbox c x y w h = Color c (Polygon (cornerPts x y w h))

-- | 線分（太さ t）
seg :: Color -> Float -> Point -> Point -> Picture
seg c t (x1, y1) (x2, y2)
  | t <= 1.2  = Color c (Line [(x1, -y1), (x2, -y2)])
  | otherwise = Color c (thickLine t (x1, -y1) (x2, -y2))

-- | 円（p5.js の circle と同じく d は直径）
circ :: Color -> Float -> Float -> Float -> Picture
circ c x y d = Color c (Translate x (-y) (disc (d / 2)))

-- | 文字：(x, y) を左上にして書く
txt :: Float -> Color -> Float -> Float -> String -> Picture
txt size c x y = label x (-y - size) size c

-- | 文字：(x, y) を左端の真ん中にして書く
txtL :: Float -> Color -> Float -> Float -> String -> Picture
txtL size c x y = label x (-y - size / 2) size c

-- | 文字：(x, y) を中心にして書く
txtC :: Float -> Color -> Float -> Float -> String -> Picture
txtC size c x y s = label (x - fromIntegral (length s) * size * 0.4) (-y - size / 2) size c s

-- | 整数を小数に
i2f :: Int -> Float
i2f = fromIntegral
