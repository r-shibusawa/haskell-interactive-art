-- ============================================
-- 04 直接アドレス表
-- キー k をそのまま配列の番号として使う：T[k] に入れる。探索・挿入・削除がすべて O(1)
-- でも、キーがとりうる範囲（全体集合 U）が大きいと、ほとんど空のムダな表になる → ハッシュ表へ
-- 操作：左の数字をクリック = 挿入/削除 / S → 数字 + Enter = 探索 / 1〜3 = U の大きさ / スペース = ランダム / C = 空にする
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Char (digitToInt, isDigit, toLower)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Numeric (showFFloat)

sizes :: [Int]
sizes = [10, 40, 400]   -- ★ 1〜3 キーで選べる全体集合 U の大きさ

names :: [String]
names = ["apple", "orange", "grape", "peach", "pear", "persimmon", "strawberry", "watermelon", "melon", "lemon", "kiwi", "banana"]

data World = World
  { universe  :: !Int
  , table     :: !(IM.IntMap String)   -- 表：T[k] = データ（入っていなければ表にない）
  , searching :: !Bool
  , buf       :: String
  , msg       :: String
  , look      :: !Int
  , lookT     :: !Int
  }

initialWorld :: World
initialWorld = World 10 IM.empty False "" "Click a number on the left to insert" (-1) 0

-- | 全体集合を作り直して、ランダムに 5 回入れる
resetU :: Int -> World -> IO World
resetU u w = foldM (\acc _ -> do
                       k <- randomRIO (0, u - 1)
                       pure (directInsert k acc))
                   w { universe = u, table = IM.empty }
                   [1 .. min 5 u]

-- ---------- 3 つの操作（探索・挿入・削除）----------
directSearch :: Int -> World -> Maybe String
directSearch k w = IM.lookup k (table w)

directInsert :: Int -> World -> World
directInsert k w =
  let name = case drop (k `mod` length names) names of
               (s : _) -> s
               [] -> ""
  in w { table = IM.insert k name (table w) }

directDelete :: Int -> World -> World
directDelete k w = w { table = IM.delete k (table w) }

-- | 左の図の配置：列の数、マスの大きさ
gridOf :: Env -> Int -> (Int, Float)
gridOf env u =
  let cols = ceiling (sqrt (fi u * 1.2) :: Float) :: Int
      rows = ceiling (fi u / fi cols :: Float) :: Int
  in (cols, minimum [52, envW env * 0.4 / fi cols, (envH env - 300) / fi rows])

step :: Env -> Float -> World -> IO World
step _ _ w = pure w { lookT = max 0 (lookT w - 1) }

draw :: Env -> World -> Picture
draw env w =
  let u = universe w
      (cols, cell) = gridOf env u
      x0 = 40
      y0 = 150
      (mx, my) = toP5 env (envMouse env)
      leftCell k =
        let x = x0 + fi (k `mod` cols) * cell
            y = y0 + fi (k `div` cols) * cell
            on = IM.member k (table w)
        in Pictures
             [ circ env (x + cell / 2) (y + cell / 2) (cell * 0.4) (if on then rgb255 255 140 120 else rgb255 55 48 62)
             , if cell >= 30   -- 小さすぎるマスには番号を書かない（読めないので）
                 then txtC env (x + cell / 2) (y + cell / 2) (max 14 (cell * 0.32)) (gray (if on then 0.08 else 0.6)) (show k)
                 else Blank
             ]
      tx = envW env * 0.52
      tw = envW env * 0.44
      tcols = if u <= 40 then 2 else 20 :: Int
      rowsN = ceiling (fi u / fi tcols :: Float) :: Int
      rh = min 40 ((envH env - 260) / fi rowsN)
      cw = tw / fi tcols
      tableCell k =
        let x = tx + fi (k `mod` tcols) * cw
            y = y0 + fi (k `div` tcols) * rh
            stored = IM.lookup k (table w)
            fillC | k == look w && lookT w > 0 = rgb255 255 214 90
                  | Just _ <- stored = rgb255 70 140 120
                  | otherwise = rgb255 35 30 40
        in if rh > 18 && cw > 60
             then Pictures
                    [ box env x y (cw - 2) (rh - 2) 4 fillC
                    , boxEdge env x y (cw - 2) (rh - 2) 4 1 (rgb255 70 60 80)
                    , txt env (x + 6) (y + rh / 2 - 8) 14 (gray 0.6) (show k)
                    , txt env (x + 40) (y + rh / 2 - 8) 14 (gray 1) (maybe "/" (\d -> show k ++ ": " ++ d) stored) ]
             else  -- マスが 400 個のときは角丸にせず、軽い四角で描く
               let (px, py) = scr env (x + (cw - 2) / 2) (y + (rh - 2) / 2)
               in Pictures [ Color fillC (rect px py (cw - 2) (rh - 2))
                           , Color (rgb255 70 60 80) (rectLine px py (cw - 2) (rh - 2)) ]
      used = IM.size (table w)
      ratio = fi used / fi u
      -- 番号が書けないときは、マウスの下のキーを見せる
      c = floor ((mx - x0) / cell) :: Int
      r = floor ((my - y0) / cell) :: Int
      hoverK = r * cols + c
      hoverTip = if cell < 30 && c >= 0 && c < cols && r >= 0 && hoverK < u
                   then txt env x0 (y0 + fi (ceiling (fi u / fi cols :: Float) :: Int) * cell + 8) 14 (gray 0.86) ("Under the mouse: key = " ++ show hoverK)
                   else Blank
  in Pictures
       ( [ txt env 30 24 26 (gray 0.94) "Direct-Address Table"
         , txt env 30 60 14 (rgb255 170 160 185) "Click a left number = insert/delete  S, number + Enter = search  1-3 = size of U (10/40/400)"
         , txt env 30 78 14 (rgb255 170 160 185) "Space = random  C = clear"
         , if searching w then txt env 30 96 16 (gray 1) ("Search: " ++ buf w ++ "|") else Blank
         , txt env x0 (y0 - 32) 17 (gray 0.78) ("Universe U (possible keys 0-" ++ show (u - 1) ++ ")")
         , txt env tx (y0 - 32) 17 (gray 0.78) "Table T (index = key)"
         , hoverTip
         ]
         ++ map leftCell [0 .. u - 1]
         ++ map tableCell [0 .. u - 1]
         ++ [ txt env 30 (envH env - 106) 16 (gray 0.86)
                ("Used " ++ show used ++ " / table size " ++ show u ++ " -> usage " ++ fmt 1 (100 * ratio) ++ "%")
            , box env 30 (envH env - 80) (envW env - 60) 12 6 (gray 0.31)
            , box env 30 (envH env - 80) ((envW env - 60) * ratio) 12 6 (rgb255 255 140 120)
            , txt env 30 (envH env - 56) 18 (rgb255 255 225 140) (msg w)
            ] )

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> pure (onClick env (toP5 env pos) w)
  EventKey k Down _ _
    | searching w -> pure (onSearchKey (normKey k) w)
    | otherwise -> onKey (normKey k) w
  _ -> pure w

onClick :: Env -> (Float, Float) -> World -> World
onClick env (mx, my) w =
  let u = universe w
      (cols, cell) = gridOf env u
      c = floor ((mx - 40) / cell) :: Int
      r = floor ((my - 150) / cell) :: Int
      k = r * cols + c
  in if c < 0 || c >= cols || r < 0 || k >= u then w
     else
       let w1 | IM.member k (table w) = (directDelete k w) { msg = "DELETE(" ++ show k ++ "): T[" ++ show k ++ "] = NIL, that is all. O(1)" }
              | otherwise = (directInsert k w) { msg = "INSERT(" ++ show k ++ "): just store in T[" ++ show k ++ "]. O(1)" }
       in w1 { look = k, lookT = 40 }

onSearchKey :: Key -> World -> World
onSearchKey key w = case key of
  Char c | isDigit c -> w { buf = pushDigit 3 c (buf w) }
  SpecialKey KeyBackspace -> w { buf = popDigit (buf w) }
  SpecialKey KeyEnter
    | null (buf w) -> w { searching = False }
    | otherwise ->
        let k = readNum (buf w)
            w1 = w { searching = False, buf = "" }
        in if k >= universe w then w1 { msg = show k ++ " is outside the universe U" }
           else w1 { look = k, lookT = 60
                   , msg = case directSearch k w of
                       Just d -> "SEARCH(" ++ show k ++ "): one look at T[" ++ show k ++ "] finds \"" ++ d ++ "\""
                       Nothing -> "SEARCH(" ++ show k ++ "): T[" ++ show k ++ "] is empty -> not there" }
  _ -> w

onKey :: Key -> World -> IO World
onKey key w = case key of
  Char c | c >= '1' && c <= '3' ->
    case drop (digitToInt c - 1) sizes of
      (u : _) -> do
        w1 <- resetU u w
        pure w1 { msg = "U = " ++ show u }
      [] -> pure w
  Char 's' -> pure w { searching = True, buf = "", msg = "Type the key to search, then Enter" }
  Char 'c' -> pure w { table = IM.empty, msg = "Cleared" }
  SpecialKey KeySpace -> do
    k <- randomRIO (0, universe w - 1)
    pure (directInsert k w) { msg = "Random insert" }
  _ -> pure w

main :: IO ()
main = runSketch (sketch "11-04 Direct-Address Table" initialWorld)
  { skBack  = rgb255 22 18 24
  , skStart = \_ -> resetU 10 initialWorld
  , skStep  = step
  , skDraw  = draw
  , skEvent = onEvent
  }

-- ---------- やってみよう ----------
-- 1. U = 400 にすると、5 個しか入っていないのに 400 マス必要。電話番号（10^10 通り）だったら？
-- 2. 同じキーが 2 回来たら上書きされる。直接アドレス表は「キーがすべて違う」ことが前提
-- 3. この問題を解くのが次の 05 ハッシュ表：大きな U を小さな m マスに「折りたたむ」


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

-- | 塗りつぶした円（中心と半径）
circ :: Env -> Float -> Float -> Float -> Color -> Picture
circ env x y r c = at (scr env x y) (Color c (disc r))

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

-- | 小数 n 桁の文字列（p5.js の nf に相当）
fmt :: Int -> Float -> String
fmt n v = showFFloat (Just n) v ""

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
