-- ============================================
-- 02 ポインタを配列で作る
-- ポインタのない言語でも、next・key・prev の 3 本の配列と「番号」で連結リストが作れる
-- 使っていないマスは「空きリスト（free list）」としてつないでおき、使うときに取り出し、いらなくなったら戻す
-- 操作：スペース（または数字 + Enter）= 先頭に挿入 / 列をクリック = その要素を削除 / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt, isDigit, toLower)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', intercalate)

memSize :: Int
memSize = 8        -- ★ 配列の大きさ（メモリの大きさ）

-- | 「どこも指さない」を -1 で表す
none :: Int
none = -1

data World = World
  { nextA  :: !(IM.IntMap Int)
  , keyA   :: !(IM.IntMap Int)   -- 入っていないマスは表にない（p5.js 版の null）
  , prevA  :: !(IM.IntMap Int)
  , listL  :: !Int
  , free   :: !Int
  , msg    :: String
  , buf    :: String
  , flashI :: !Int
  , flashT :: !Int
  }

initialWorld :: World
initialWorld = resetAll (World IM.empty IM.empty IM.empty none 0 "Space = insert, click a column = delete" "" (-1) 0)

resetAll :: World -> World
resetAll w =
  let blank' = w { nextA = IM.fromList [(i, if i + 1 < memSize then i + 1 else none) | i <- [0 .. memSize - 1]]
                 , keyA = IM.empty
                 , prevA = IM.fromList [(i, none) | i <- [0 .. memSize - 1]]
                 , listL = none, free = 0 }
  in foldl' (flip insertKey) blank' [3, 18, 11, 6]

nextOf, prevOf :: World -> Int -> Int
nextOf w i = IM.findWithDefault none i (nextA w)
prevOf w i = IM.findWithDefault none i (prevA w)

-- ---------- 空きリスト ----------
-- | 空きマスを 1 つ取り出す（なければ none）
allocateObject :: World -> (Int, World)
allocateObject w
  | free w == none = (none, w)     -- メモリ切れ
  | otherwise = let x = free w in (x, w { free = nextOf w x })

freeObject :: Int -> World -> World
freeObject x w = w { nextA = IM.insert x (free w) (nextA w), free = x
                   , prevA = IM.insert x none (prevA w), keyA = IM.delete x (keyA w) }

insertKey :: Int -> World -> World
insertKey k w0 =
  let (x, w) = allocateObject w0
  in if x == none then w { msg = "No free cell! (out of space)" }
     else
       let l = listL w
           p = if l /= none then IM.insert l x (prevA w) else prevA w
       in w { keyA = IM.insert x k (keyA w), nextA = IM.insert x l (nextA w)
            , prevA = IM.insert x none p
            , listL = x, flashI = x, flashT = 40
            , msg = "Took cell " ++ show x ++ " from the free list: key[" ++ show x ++ "] = " ++ show k
                    ++ ", next[" ++ show x ++ "] = " ++ showPtr l ++ ", L = " ++ show x }

deleteAt :: Int -> World -> World
deleteAt x w = case IM.lookup x (keyA w) of
  Nothing -> w
  Just k ->
    let p = prevOf w x
        n = nextOf w x
        w1 = if p /= none then w { nextA = IM.insert p n (nextA w) } else w { listL = n }
        w2 = if n /= none then w1 { prevA = IM.insert n p (prevA w1) } else w1
        w3 = freeObject x w2
    in w3 { msg = show k ++ " (cell " ++ show x ++ ") deleted and freed: back to the head of the free list (free = " ++ show (free w3) ++ ")" }

showPtr :: Int -> String
showPtr v = if v == none then "/" else show v

-- | i から next をたどる（多くても memSize 個）
chain :: World -> Int -> [Int]
chain w = go memSize
  where go :: Int -> Int -> [Int]
        go n i | i == none || n <= 0 = []
               | otherwise = i : go (n - 1) (nextOf w i)

-- | マスの配置：幅 cw、左端 x0、上端 y0
cellLayout :: Env -> (Float, Float, Float)
cellLayout env = (min 90 ((envW env - 220) / fi memSize), 160, envH env * 0.42)

step :: Env -> Float -> World -> IO World
step _ _ w = pure w { flashT = max 0 (flashT w - 1) }

draw :: Env -> World -> Picture
draw env w =
  let (cw, x0, y0) = cellLayout env
      ch = 46
      cx i = x0 + fi i * cw + cw / 2
      rows = [ ("next", \i -> Just (nextOf w i)), ("key", \i -> IM.lookup i (keyA w)), ("prev", \i -> Just (prevOf w i)) ]
      header = [txtC env (cx i) (y0 - 22) 14 (rgb255 140 140 180) (show i) | i <- [0 .. memSize - 1]]
      rowPic (r, (name, get)) = Pictures
        ( txtR env (x0 - 14) (y0 + fi r * ch + ch / 2 - 9) 17 (gray 0.78) name
        : [ let used = IM.member i (keyA w)
                fillC | i == flashI w && flashT w > 0 = rgb255 255 214 90
                      | used = rgb255 60 120 170
                      | otherwise = rgb255 36 34 52
                bx = x0 + fi i * cw + 2
                by = y0 + fi r * ch
            in Pictures
                 [ box env bx by (cw - 4) (ch - 4) 6 fillC
                 , boxEdge env bx by (cw - 4) (ch - 4) 6 1.5 (rgb255 70 70 110)
                 , case get i of
                     Just v -> txtC env (cx i) (by + ch / 2 - 2) 17 (gray (if used then 1 else 0.47)) (showPtr v)
                     Nothing -> Blank
                 ]
          | i <- [0 .. memSize - 1] ] )
      -- 矢印：リスト（青・上）と空きリスト（灰・下）
      arcs start y dir c =
        let ids = chain w start
        in zipWith (\a b -> arc3 env (cx a) (cx b) y dir c) ids (drop 1 ids)
      lLabel = if listL w == none then txtC env 60 (y0 - 92) 16 (rgb255 120 200 255) "L = /"
               else txtC env (cx (listL w)) (y0 - 92) 16 (rgb255 120 200 255) "L"
      fLabel = if free w == none then txtC env 60 (y0 + 3 * ch + 68) 16 (gray 0.67) "free = /"
               else txtC env (cx (free w)) (y0 + 3 * ch + 68) 16 (gray 0.67) "free"
      order = [maybe "?" show (IM.lookup i (keyA w)) | i <- chain w (listL w)]
      frees = map show (chain w (free w))
  in Pictures
       ( [ txt env 30 24 26 (gray 0.94) "Pointers Made of Arrays (3 arrays + a free list)"
         , txt env 30 60 14 (rgb255 160 160 200) "Space / number + Enter = insert  Click a column = delete  R = reset"
         , if null (buf w) then Blank else txt env 30 88 18 (gray 1) ("Input: " ++ buf w)
         ]
         ++ header ++ map rowPic (zip [0 :: Int ..] rows)
         ++ arcs (listL w) (y0 - 34) (-1) (rgb255 120 200 255)
         ++ arcs (free w) (y0 + 3 * ch + 8) 1 (rgb255 150 150 170)
         ++ [ lLabel, fLabel
            , txt env 30 (envH env - 120) 18 (gray 0.86) ("List order: " ++ (if null order then "(empty)" else intercalate " <-> " order))
            , txt env 30 (envH env - 92) 18 (gray 0.63) ("Free cells: " ++ (if null frees then "(none)" else intercalate " -> " frees))
            , Pictures (zipWith (\i s -> txt env 30 (envH env - 62 + i * 22) 16 (rgb255 255 225 140) s) [0 ..] (take 2 (wrapText 105 (msg w))))
            ] )

-- | 2 つのマスをつなぐ弧（dir = -1 で上、1 で下）
arc3 :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
arc3 env xa xb y dir c =
  let h = dir * (18 + abs (xb - xa) * 0.18)
  in Pictures
       [ bezier env 2 c (xa, y) (xa, y + h) (xb, y + h) (xb, y)
       , tri env c (xb, y) (xb - 5, y + dir * 9) (xb + 5, y + dir * 9)
       ]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> pure (onClick env (toP5 env pos) w)
  EventKey k Down _ _ -> onKey (normKey k) w
  _ -> pure w

onKey :: Key -> World -> IO World
onKey k w = case k of
  Char c | isDigit c -> pure w { buf = pushDigit 3 c (buf w) }
  SpecialKey KeyEnter | not (null (buf w)) -> pure (insertKey (readNum (buf w)) w) { buf = "" }
  SpecialKey KeySpace -> do
    v <- randomRIO (1, 99)
    pure (insertKey v w)
  Char 'r' -> pure (resetAll w) { msg = "Reset" }
  _ -> pure w

onClick :: Env -> (Float, Float) -> World -> World
onClick env (mx, my) w =
  let (cw, x0, y0) = cellLayout env
      i = floor ((mx - x0) / cw) :: Int
  in if i >= 0 && i < memSize && my > y0 - 10 && my < y0 + 140
       then if IM.member i (keyA w) then deleteAt i w
            else w { msg = "Cell " ++ show i ++ " is free (in the free list)" }
       else w

main :: IO ()
main = runSketch (sketch "11-02 Pointers Made of Arrays" initialWorld)
  { skBack  = rgb255 18 16 28
  , skStep  = step
  , skDraw  = draw
  , skEvent = onEvent
  }

-- ---------- やってみよう ----------
-- 1. 9 個目を入れると「空きがない」。memSize を 12 に増やしてみよう
-- 2. 削除したマスは空きリストの先頭へ。次の挿入はそのマスが再利用される（スタックと同じ動き）
-- 3. 1 本の配列に [key, next, prev] を 3 つずつ並べる「1 配列表現」も作れる。番号を 3 倍にするだけ


-- ==================================================
-- ここから下は「画面の道具」です
-- p5.js と同じ「左上が原点、y は下向き」の座標で考えて、ここで gloss の座標に直します
-- ==================================================
fi :: Int -> Float
fi = fromIntegral

scr :: Env -> Float -> Float -> Point
scr env x y = (x - envW env / 2, envH env / 2 - y)

-- | gloss の座標を p5.js と同じ座標へ
toP5 :: Env -> Point -> Point
toP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- | 左上をそろえた文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y sz c s = let (px, py) = scr env x (y + sz) in label px py sz c s

-- | 中央をそろえた文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y sz c s = let (px, py) = scr env x y in labelC px py sz c s

-- | 右上をそろえた文字
txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtR env x y sz c s = txt env (x - fi (length s) * sz * 0.72) y sz c s

-- | 角の丸い長方形のかど（左上の x, y、幅、高さ、丸みの半径）
roundCorners :: Float -> Float -> Float -> Float -> Float -> [(Float, Float)]
roundCorners x y w h r0 =
  let r = max 0 (minimum [r0, w / 2, h / 2])
      arcAt cx cy a0 = [(cx + r * cos a, cy + r * sin a) | i <- [0 .. 3 :: Int], let a = a0 + fi i * pi / 6]
  in arcAt (x + w - r) (y + r) (-pi / 2) ++ arcAt (x + w - r) (y + h - r) 0
     ++ arcAt (x + r) (y + h - r) (pi / 2) ++ arcAt (x + r) (y + r) pi

-- | 塗りつぶした角丸の長方形（p5.js の rect と同じく左上で指定）
box :: Env -> Float -> Float -> Float -> Float -> Float -> Color -> Picture
box env x y w h r c = Color c (Polygon [scr env px py | (px, py) <- roundCorners x y w h r])

-- | 角丸の長方形の枠
boxEdge :: Env -> Float -> Float -> Float -> Float -> Float -> Float -> Color -> Picture
boxEdge env x y w h r sw c =
  let ps = [scr env px py | (px, py) <- roundCorners x y w h r]
  in Color c (thickPath sw (ps ++ take 1 ps))

-- | 3 次ベジェ曲線の上の点
bezierPts :: Point -> Point -> Point -> Point -> [Point]
bezierPts (x0, y0) (x1, y1) (x2, y2) (x3, y3) =
  [ (b x0 x1 x2 x3 t, b y0 y1 y2 y3 t) | i <- [0 .. 20 :: Int], let t = fi i / 20 ]
  where b a0 a1 a2 a3 t = let u = 1 - t in u*u*u*a0 + 3*u*u*t*a1 + 3*u*t*t*a2 + t*t*t*a3

-- | 太さのある 3 次ベジェ曲線
bezier :: Env -> Float -> Color -> Point -> Point -> Point -> Point -> Picture
bezier env sw c p0 p1 p2 p3 =
  Color c (thickPath sw [scr env x y | (x, y) <- bezierPts p0 p1 p2 p3])

-- | 三角形（矢じりなど）
tri :: Env -> Color -> Point -> Point -> Point -> Picture
tri env c (a, b) (d, e) (f, g) = Color c (Polygon [scr env a b, scr env d e, scr env f g])

-- | 長い文を n 文字ごとに折り返す
wrapText :: Int -> String -> [String]
wrapText n s = go (words s) ""
  where
    go [] cur = [cur | not (null cur)]
    go (x : xs) "" = go xs x
    go (x : xs) cur
      | length cur + 1 + length x <= n = go xs (cur ++ " " ++ x)
      | otherwise = cur : go xs x

-- | キーをそろえる：英字は小文字に。Enter・Backspace・スペースは環境によって来かたが違うので 1 つにまとめる
normKey :: Key -> Key
normKey k = case k of
  Char ' ' -> SpecialKey KeySpace
  Char '\r' -> SpecialKey KeyEnter
  Char '\n' -> SpecialKey KeyEnter
  Char '\b' -> SpecialKey KeyBackspace
  Char '\DEL' -> SpecialKey KeyBackspace
  SpecialKey KeyDelete -> SpecialKey KeyBackspace
  SpecialKey KeyPadEnter -> SpecialKey KeyEnter
  Char c -> Char (toLower c)
  _ -> k

-- | 打ち込んだ数字の文字列を数に（空なら 0）
readNum :: String -> Int
readNum = foldl' (\acc c -> acc * 10 + digitToInt c) 0

-- | 入力欄：数字を 1 つ足す（lim 桁まで）
pushDigit :: Int -> Char -> String -> String
pushDigit lim c s = if length s < lim then s ++ [c] else s
