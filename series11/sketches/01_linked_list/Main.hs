-- ============================================
-- 01 双方向連結リストと番兵
-- 各要素が「次（next）」と「前（prev）」の 2 本の矢印を持つ。並び順は矢印で決まり、配列の番号はいらない
-- 番兵 nil（灰色）を置くと、先頭と末尾の特別あつかいがなくなり、コードが短くなる（輪になったリスト）
-- 操作：数字を打って Enter = 先頭に挿入 / S = 探索 / D = 削除 / 要素をクリック = 削除
--       スペース = ランダムな数を挿入 / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt, isDigit, toLower)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')

speed :: Int
speed = 18      -- ★ 探索の 1 歩にかけるフレーム数（小さいほど速い）

maxShow :: Int
maxShow = 12    -- ★ 画面に並べる最大の数

-- | リストの要素。ポインタの代わりに要素の番号（id）で next / prev を指す
data Cell = Cell { cKey :: !Int, cNext :: !Int, cPrev :: !Int }

data ScanMode = Search | Delete deriving (Eq)

data Scan = Scan { sKey :: !Int, sX :: !Int, sMode :: !ScanMode, sSteps :: !Int }

data World = World
  { cells  :: !(IM.IntMap Cell)
  , nextId :: !Int
  , buf    :: String            -- 打ち込み中の数字
  , msg    :: String
  , scan   :: !(Maybe Scan)     -- 探索中の状態
  , flash  :: !Int
  , flashT :: !Int
  }

-- | 番兵（sentinel）の番号
nilId :: Int
nilId = 0

initialWorld :: World
initialWorld = resetAll (World IM.empty 1 "" "Type a number + Enter to insert. S = search, D = delete" Nothing (-1) 0)

resetAll :: World -> World
resetAll w =
  let empty = w { cells = IM.singleton nilId (Cell 0 nilId nilId), nextId = 1, scan = Nothing }   -- 空のリスト：番兵が自分自身を指す
  in foldl' (\acc k -> fst (listInsert k acc)) empty [7, 23, 12, 5]

cell :: World -> Int -> Cell
cell w x = IM.findWithDefault (Cell 0 nilId nilId) x (cells w)

setNext, setPrev :: Int -> Int -> World -> World
setNext x v w = w { cells = IM.adjust (\c -> c { cNext = v }) x (cells w) }
setPrev x v w = w { cells = IM.adjust (\c -> c { cPrev = v }) x (cells w) }

-- ---------- リストの操作（どれも矢印のつなぎかえだけ）----------
-- | 先頭に入れる。新しい要素の番号も返す
listInsert :: Int -> World -> (World, Int)
listInsert k w =
  let x = nextId w
      first = cNext (cell w nilId)
      w1 = w { cells = IM.insert x (Cell k first nilId) (cells w), nextId = x + 1 }
  in (setNext nilId x (setPrev first x w1), x)

-- | x を取り除く（番兵のおかげで if がいらない）
listDelete :: Int -> World -> World
listDelete x w =
  let c = cell w x
  in setPrev (cNext c) (cPrev c) (setNext (cPrev c) (cNext c) w)

-- | 番兵の次から順にたどる
toArray :: World -> [Int]
toArray w = go (maxShow * 4 :: Int) (cNext (cell w nilId))
  where go n x | x == nilId || n <= 0 = []
               | otherwise = x : go (n - 1) (cNext (cell w x))

-- | もうつながっていない要素を片づける（JavaScript のごみ集めの代わり）。探索中は待つ
sweep :: World -> World
sweep w = case scan w of
  Just _ -> w
  Nothing -> w { cells = IM.fromList [(x, cell w x) | x <- nilId : toArray w] }

-- | 見た目の配置
layout :: Env -> Int -> (Float, Float)
layout env n = (min 110 ((envW env - 80) / fi n), envH env * 0.48)

step :: Env -> Float -> World -> IO World
step env _ w0 =
  let w1 = case scan w0 of
             Just s | envFrame env `mod` speed == 0 -> advanceScan s w0
             _ -> w0
  in pure w1 { flashT = max 0 (flashT w1 - 1) }

draw :: Env -> World -> Picture
draw env w =
  let items = toArray w
      allC = nilId : items
      n = length allC
      (cw, y) = layout env n
      posX i = 40 + cw * fi i + cw / 2 :: Float
      green' = rgb255 120 220 170
      orange' = rgb255 255 170 110
      lastI = n - 1
      firstX = posX (0 :: Int)
      links = [ Pictures
                  [ arrowTo env green' (posX i + 22) (y - 10) (posX (i + 1) - 22) (y - 10)
                  , arrowTo env orange' (posX (i + 1) - 22) (y + 10) (posX i + 22) (y + 10) ]
              | i <- [0 .. lastI - 1] ]
      lastX = posX lastI
      -- 末尾 → 番兵（輪をとじる）
      closing =
        [ bezier env 2 (withA 0.63 green') (lastX, y - 26) (lastX, y - 130) (firstX, y - 130) (firstX, y - 26)
        , bezier env 2 (withA 0.63 orange') (firstX, y + 26) (firstX, y + 130) (lastX, y + 130) (lastX, y + 26) ]
      element (i, x) =
        let px = posX i
            isScan = maybe False (\s -> sX s == x) (scan w)
            fillC | flash w == x && flashT w > 0 = rgb255 255 214 90
                  | x == nilId = rgb255 70 75 90
                  | otherwise = rgb255 40 110 150
        in Pictures
             [ box env (px - 24) (y - 26) 48 52 8 fillC
             , boxEdge env (px - 24) (y - 26) 48 52 8 (if isScan then 4 else 1.5) (if isScan then rgb255 255 214 90 else rgb255 80 90 120)
             , if x == nilId then txtC env px y 14 (gray 1) "nil"
               else txtC env px y 20 (gray 1) (show (cKey (cell w x)))
             ]
  in Pictures
       ( [ txt env 30 24 26 (gray 0.94) "Doubly Linked List with a Sentinel"
         , txt env 30 60 14 (rgb255 150 165 190) "Number + Enter = insert  S = search  D = delete  Click = delete  Space = random insert  R = reset"
         , txt env 30 88 20 (gray 1) ("Input: " ++ buf w ++ (if envFrame env `mod` 60 < 30 then "|" else ""))
         ]
         ++ links ++ closing ++ map element (zip [0 ..] allC) ++
         [ txt env 30 (y - 160) 14 green' "Green = next"
         , txt env 30 (y + 150) 14 orange' "Orange = prev"
         , txt env 30 (envH env - 70) 18 (rgb255 255 225 140) (msg w)
         ] )

startScan :: ScanMode -> World -> World
startScan mode w
  | null (buf w) = w { msg = "Type a number first" }
  | otherwise =
      let k = readNum (buf w)
      in w { scan = Just (Scan k (cNext (cell w nilId)) mode 1), buf = ""
           , msg = "Search(" ++ show k ++ "): compare one by one from the front..." }

advanceScan :: Scan -> World -> World
advanceScan s w
  | sX s == nilId =
      w { msg = show (sKey s) ++ " not found (" ++ show (sSteps s - 1) ++ " comparisons, reached the sentinel)", scan = Nothing }
  | cKey (cell w (sX s)) == sKey s =
      if sMode s == Delete
        then (listDelete (sX s) w)
               { msg = "Deleted " ++ show (sKey s) ++ ": just relink prev's next and next's prev (found at comparison " ++ show (sSteps s) ++ ")"
               , scan = Nothing }
        else w { msg = "Found " ++ show (sKey s) ++ " at comparison " ++ show (sSteps s) ++ "!"
               , flash = sX s, flashT = 60, scan = Nothing }
  | otherwise = w { scan = Just s { sX = cNext (cell w (sX s)), sSteps = sSteps s + 1 } }

insertNew :: Int -> String -> World -> World
insertNew k m w =
  let (w1, x) = listInsert k (sweep w)
  in w1 { flash = x, flashT = 40, msg = m }

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> pure (onClick env (toP5 env pos) w)
  EventKey k Down _ _ -> onKey (normKey k) w
  _ -> pure w

onKey :: Key -> World -> IO World
onKey k w = case k of
  Char c | isDigit c -> pure w { buf = pushDigit 3 c (buf w) }
  SpecialKey KeyBackspace -> pure w { buf = popDigit (buf w) }
  SpecialKey KeyEnter
    | null (buf w) -> pure w
    | length (toArray w) >= maxShow -> pure w { msg = "No more room on screen (delete with D first)", buf = "" }
    | otherwise ->
        let v = readNum (buf w)
        in pure (insertNew v ("Insert(" ++ buf w ++ "): at the front. Only 4 arrows change, so O(1)") w) { buf = "" }
  Char 's' -> pure (startScan Search w)
  Char 'd' -> pure (startScan Delete w)
  SpecialKey KeySpace
    | length (toArray w) >= maxShow -> pure w { msg = "Full" }
    | otherwise -> do
        v <- randomRIO (1, 99)
        pure (insertNew v ("Inserted " ++ show v ++ " at the front") w)
  Char 'r' -> pure (resetAll w) { msg = "Reset" }
  _ -> pure w

onClick :: Env -> (Float, Float) -> World -> World
onClick env (mx, my) w =
  let items = toArray w
      (cw, y) = layout env (length items + 1)
      hit (i, x) acc =
        let px = 40 + cw * fi i + cw / 2
        in if abs (mx - px) < 26 && abs (my - y) < 28
             then (listDelete x acc) { msg = "Deleted " ++ show (cKey (cell w x)) ++ " (O(1): no search needed when you know where it is)" }
             else acc
  in foldr hit w (zip [1 :: Int ..] items)

main :: IO ()
main = runSketch (sketch "11-01 Doubly Linked List" initialWorld)
  { skBack  = rgb255 14 20 28
  , skStep  = step
  , skDraw  = draw
  , skEvent = onEvent
  }

-- | 矢印（線と三角形）
arrowTo :: Env -> Color -> Float -> Float -> Float -> Float -> Picture
arrowTo env c x1 y1 x2 y2 =
  let a = atan2 (y2 - y1) (x2 - x1)
  in Pictures
       [ seg env 2 c x1 y1 x2 y2
       , tri env c (x2, y2) (x2 - cos (a - 0.4) * 9, y2 - sin (a - 0.4) * 9) (x2 - cos (a + 0.4) * 9, y2 - sin (a + 0.4) * 9)
       ]

-- ---------- やってみよう ----------
-- 1. 存在しない数を S で探すと、番兵まで全部たどる。要素が n 個なら n 回 → O(n)
-- 2. listDelete の中身を紙に書いて、番兵がないと先頭・末尾で何が困るか考える
-- 3. 末尾に入れる listAppend を作ってみよう（番兵の cPrev が末尾）


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

-- | 太さのある線分
seg :: Env -> Float -> Color -> Float -> Float -> Float -> Float -> Picture
seg env sw c x1 y1 x2 y2 = Color c (thickLine sw (scr env x1 y1) (scr env x2 y2))

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

-- | 入力欄：最後の 1 文字を消す
popDigit :: String -> String
popDigit s = take (length s - 1) s
