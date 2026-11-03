-- ============================================
-- 07 オープンアドレス法
-- リストを使わず、表のマスだけに入れる。ふさがっていたら「探査列」h(k,0), h(k,1), … の順に空きを探す
-- 線形探査：隣へ隣へ → 「固まり（クラスタ）」ができて遅くなる
-- 二次探査：1, 3, 6, 10 … と飛ぶ / ダブルハッシュ：キーごとに飛ぶ幅が違う → 固まりにくい
-- 削除：単に消すと探索がそこで止まってしまうので「削除済み」の印を置く。
--       線形探査なら、後ろの要素を詰め直して印を残さない方法もある
-- 操作：スペース = ランダムに挿入 / 数字 + Enter = 挿入 / 数字 + S = 探索 / マスをクリック = 削除
--       M = 探査の方法（線形 → 二次 → ダブルハッシュ）/ T = 削除の方法を切りかえ / C = 空に
-- ============================================
module Main (main) where

import Art
import Data.Char (digitToInt, isDigit, toLower)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Numeric (showFFloat)

m :: Int
m = 16         -- ★ マスの数（2 のべき乗にしておくと二次探査・ダブルハッシュが全マスを回れる）

speed :: Int
speed = 14     -- ★ 探査 1 回にかけるフレーム数

-- | マスの中身。表にないマスは空（NIL）
data Cell = Key !Int | Del deriving (Eq)

data OpKind = OpInsert | OpSearch deriving (Eq)

data Op = Op { oKind :: !OpKind, oK :: !Int, oI :: !Int, oPath :: [Int] }  -- oPath は新しいものが先頭

data World = World
  { table   :: !(IM.IntMap Cell)
  , probes  :: !(IM.IntMap Int)   -- キー → 何回の探査で入ったか
  , method  :: !Int
  , markDel :: !Bool
  , buf     :: String
  , op      :: !(Maybe Op)
  , msg     :: String
  }

initialWorld :: World
initialWorld = World IM.empty IM.empty 1 True "" Nothing "Space = random insert. M = change the probing method"

clearTable :: World -> World
clearTable w = w { table = IM.empty, probes = IM.empty, op = Nothing }

h1 :: Int -> Int
h1 k = k `mod` m

h2 :: Int -> Int
h2 k = 2 * ((k `div` m) `mod` (m `div` 2)) + 1   -- 奇数 → m（2 のべき乗）と互いに素

hp :: Int -> Int -> Int -> Int
hp method' k i = case method' of
  1 -> (h1 k + i) `mod` m                     -- 線形探査
  2 -> (h1 k + (i + i * i) `div` 2) `mod` m   -- 二次探査（c1 = c2 = 1/2）
  _ -> (h1 k + i * h2 k) `mod` m              -- ダブルハッシュ

startOp :: OpKind -> Int -> World -> World
startOp kind k w = case op w of
  Just _ -> w
  Nothing
    | kind == OpInsert && Key k `elem` IM.elems (table w) -> w { msg = show k ++ " is already in" }
    | otherwise ->
        w { op = Just (Op kind k 0 [])
          , msg = (if kind == OpInsert then "INSERT(" else "SEARCH(") ++ show k ++ "): start probing at h(" ++ show k ++ ", 0) = " ++ show (hp (method w) k 0) }

advanceOp :: Op -> World -> World
advanceOp o w =
  let q = hp (method w) (oK o) (oI o)
      o1 = o { oPath = q : oPath o }
      k = oK o
      tries = oI o + 1
      continue =
        let o2 = o1 { oI = tries }
        in if tries >= m
             then w { msg = if oKind o == OpInsert then "Table overflow" else "Not there (looked at every cell)", op = Nothing }
             else w { op = Just o2 }
  in case (oKind o, IM.lookup q (table w)) of
       (OpInsert, Just (Key _)) -> continue
       (OpInsert, _) ->
         w { table = IM.insert q (Key k) (table w), probes = IM.insert k tries (probes w)
           , msg = "INSERT(" ++ show k ++ "): placed in cell " ++ show q ++ " on probe " ++ show tries, op = Nothing }
       (OpSearch, Just (Key v)) | v == k ->
         w { msg = "SEARCH(" ++ show k ++ "): found in cell " ++ show q ++ " on probe " ++ show tries ++ "!", op = Nothing }
       (OpSearch, Nothing) ->
         w { msg = "SEARCH(" ++ show k ++ "): reached empty cell " ++ show q ++ " -> not there (" ++ show tries ++ " probes)", op = Nothing }
       _ -> continue

deleteAt :: Int -> World -> World
deleteAt q w = case IM.lookup q (table w) of
  Just (Key k)
    | markDel w || method w /= 1 ->
        w { table = IM.insert q Del (table w), probes = IM.delete k (probes w)
          , msg = show k ++ " deleted -> leave a DEL mark (search passes through, insert may reuse it)" }
    | otherwise -> shiftBack k q 0 w { probes = IM.delete k (probes w) }
  _ -> w

-- | 線形探査だけの削除法：印を置かずに、後ろの要素を前へ詰める
shiftBack :: Int -> Int -> Int -> World -> World
shiftBack k q moved w0 =
  let w = w0 { table = IM.delete q (table w0) }
      look q2 =
        let q3 = (q2 + 1) `mod` m
        in case IM.lookup q3 (table w) of
             Nothing -> Nothing
             Just (Key k2) | (q - h1 k2 + m) `mod` m < (q3 - h1 k2 + m) `mod` m -> Just (q3, k2)  -- k2 は q へ動かしても見つかる
             _ -> look q3
  in case look q of
       Nothing -> w { msg = show k ++ " deleted, and " ++ show moved ++ " later items moved forward (no mark)" }
       Just (q2, k2) ->
         shiftBack k q2 (moved + 1)
           w { table = IM.insert q (Key k2) (table w)
             , probes = IM.insert k2 ((q - h1 k2 + m) `mod` m + 1) (probes w) }

-- | マスの配置：幅 cw、左端 x0、上端 y
cellLayout :: Env -> (Float, Float, Float)
cellLayout env =
  let cw = min 72 ((envW env - 80) / fi m)
  in (cw, (envW env - cw * fi m) / 2, envH env * 0.42)

step :: Env -> Float -> World -> IO World
step env _ w = pure $ case op w of
  Just o | envFrame env `mod` speed == 0 -> advanceOp o w
  _ -> w

draw :: Env -> World -> Picture
draw env w =
  let (cw, x0, y) = cellLayout env
      cellOf i = IM.lookup (i `mod` m) (table w)
      mname = case method w of
        1 -> "Linear probing  h(k,i) = (k mod m + i) mod m"
        2 -> "Quadratic probing  h(k,i) = (k mod m + (i + i^2)/2) mod m"
        _ -> "Double hashing  h(k,i) = (k mod m + i*h2(k)) mod m"
      -- 固まり（連続してふさがっている所）
      clusterAt s
        | Just _ <- cellOf s, Nothing <- cellOf (s - 1 + m) =
            let len = length (takeWhile (\j -> maybe False (const True) (cellOf (s + j))) [0 .. m - 1])
            in [ box env (x0 + fi ((s + j) `mod` m) * cw + 2) (y + cw + 8) (cw - 4) 8 4 (rgba255 255 120 80 (min 220 (40 + len * 25)))
               | j <- [0 .. len - 1] ]
        | otherwise = []
      path = maybe [] oPath (op w)
      cell i =
        let x = x0 + fi i * cw
            v = cellOf i
            now = take 1 path == [i]
            onPath = i `elem` path
            fillC | onPath && not now = rgb255 120 100 60
                  | otherwise = case v of
                      Nothing -> rgb255 40 36 30
                      Just Del -> rgb255 80 60 60
                      Just (Key _) -> rgb255 200 150 70
        in Pictures
             [ box env (x + 2) y (cw - 4) cw 8 fillC
             , boxEdge env (x + 2) y (cw - 4) cw 8 (if now then 4 else 1.5) (if now then rgb255 255 214 90 else rgb255 90 80 70)
             , case v of
                 Nothing -> Blank
                 Just Del -> txtC env (x + cw / 2) (y + cw / 2) 14 (gray 0.9) "DEL"
                 Just (Key k) -> txtC env (x + cw / 2) (y + cw / 2) 18 (gray 0.08) (show k)
             , txtC env (x + cw / 2) (y - 14) 14 (gray 0.6) (show i)
             , case v of
                 Just (Key k) | IM.findWithDefault 0 k (probes w) > 1 ->
                   txtC env (x + cw / 2) (y + cw + 28) 14 (rgb255 255 160 120) (show (IM.findWithDefault 0 k (probes w)) ++ "x")
                 _ -> Blank
             ]
      -- 探査列の矢印
      opPics = case op w of
        Nothing -> []
        Just o ->
          let ps = reverse (oPath o)
              centre q = x0 + fi q * cw + cw / 2
              arcs = zipWith3 (\j a b ->
                        let xa = centre a
                            xb = centre b
                            top = y - 80 - fi j * 6
                        in bezier env 2 (rgb255 255 214 90) (xa, y - 26) (xa, top) (xb, top) (xb, y - 26))
                      [1 :: Int ..] ps (drop 1 ps)
          in arcs ++ [txtC env (envW env / 2) (y - 130) 16 (rgb255 255 214 90) ("k = " ++ show (oK o) ++ "   i = " ++ show (oI o))]
      n = length [() | Key _ <- IM.elems (table w)]
      alpha = fi n / fi m :: Float
      ps = IM.elems (probes w)
      avg = if null ps then 0 else fi (sum ps) / fi (length ps) :: Float
  in Pictures
       ( [ txt env 30 24 26 (gray 0.94) "Open Addressing"
         , txt env 30 60 14 (rgb255 190 180 160) "Space / number + Enter = insert  Number + S = search  Click = delete  M probing  T delete mode  C clear"
         , txt env 30 82 16 (gray 1) mname
         , txt env 30 104 16 (gray 1) ("Delete: " ++ (if markDel w || method w /= 1 then "DEL mark" else "move items back (linear only)") ++ "    Input: " ++ buf w)
         ]
         ++ concatMap clusterAt [0 .. m - 1]
         ++ map cell [0 .. m - 1]
         ++ opPics
         ++ [ txt env 30 (envH env - 134) 16 (gray 0.86)
                ("n = " ++ show n ++ "  alpha = " ++ fmt 2 alpha ++ "  successful search, avg probes (measured) " ++ fmt 2 avg
                 ++ "  theory " ++ (if alpha > 0 && alpha < 1 then fmt 2 (1 / alpha * log (1 / (1 - alpha))) else "-"))
            , txt env 30 (envH env - 112) 16 (gray 0.86)
                ("failed search, theory 1/(1-alpha) = " ++ (if alpha < 1 then fmt 2 (1 / (1 - alpha)) else "inf"))
            , txt env 30 (envH env - 84) 14 (rgb255 255 120 80) "Orange band = cluster. The longer, the darker"
            , txt env 30 (envH env - 58) 17 (rgb255 255 225 140) (msg w)
            ] )

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ pos -> pure (onClick env (toP5 env pos) w)
  EventKey k Down _ _ -> onKey (normKey k) w
  _ -> pure w

onClick :: Env -> (Float, Float) -> World -> World
onClick env (mx, my) w =
  let (cw, x0, y) = cellLayout env
      i = floor ((mx - x0) / cw) :: Int
      idle = maybe True (const False) (op w)
  in if i >= 0 && i < m && my > y && my < y + cw && idle then deleteAt i w else w

onKey :: Key -> World -> IO World
onKey key w = case key of
  Char c | isDigit c -> pure w { buf = pushDigit 3 c (buf w) }
  SpecialKey KeyBackspace -> pure w { buf = popDigit (buf w) }
  SpecialKey KeyEnter | not (null (buf w)) -> pure (startOp OpInsert (readNum (buf w)) w) { buf = "" }
  Char 's' | not (null (buf w)) -> pure (startOp OpSearch (readNum (buf w)) w) { buf = "" }
  SpecialKey KeySpace -> do
    k <- randomRIO (1, 199)
    pure (startOp OpInsert k w)
  Char 'c' -> pure (clearTable w) { msg = "Cleared" }
  Char 'm' | maybe True (const False) (op w) ->
    let next = method w `mod` 3 + 1
        name = case next of { 1 -> "Linear probing"; 2 -> "Quadratic probing"; _ -> "Double hashing" }
    in pure (clearTable w { method = next }) { msg = name ++ ", table cleared" }
  Char 't' ->
    let mark = not (markDel w)
    in pure w { markDel = mark, msg = if mark then "Delete = DEL mark" else "Delete = move items back (linear probing only)" }
  _ -> pure w

main :: IO ()
main = runSketch (sketch "11-07 Open Addressing" initialWorld)
  { skBack  = rgb255 20 18 14
  , skStep  = step
  , skDraw  = draw
  , skEvent = onEvent
  }

-- ---------- やってみよう ----------
-- 1. 線形探査で 12 個入れる。長いオレンジの帯ができ、探査回数が増える
-- 2. 同じことを M でダブルハッシュにして。帯が短く、平均探査数が小さい
-- 3. 印を置く方法で削除と挿入を繰り返すと「DEL」だらけに → 探索が遅くなる。T で詰め直しにすると？


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
