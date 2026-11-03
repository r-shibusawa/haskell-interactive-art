-- ============================================
-- 05 チェイン法のハッシュ表
-- キー k を h(k) 番のマスへ。同じマスに来たもの（衝突）は連結リストでつなぐ
-- 負荷率 α = n / m（1 マスあたりの平均の個数）。探索にかかる時間の平均は Θ(1 + α)
-- 操作：数字 + Enter = 挿入 / 数字 + S = 探索 / 数字 + D = 削除 / スペース = ランダムに 1 個 / A = 10 個
--       ↑↓ = マスの数 m（入れ直し）/ H = ハッシュ関数（割り算法 ⇄ かけ算法）/ C = 空に
-- ============================================
module Main (main) where

import Art
import Control.Monad (foldM)
import Data.Char (digitToInt, isDigit, toLower)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Numeric (showFFloat)

firstM :: Int
firstM = 9               -- ★ マスの数

keyMax :: Int
keyMax = 1000            -- ★ キーの範囲 0〜keyMax-1

constA :: Double
constA = (sqrt 5 - 1) / 2  -- ★ かけ算法の定数（Knuth のおすすめ 0.618…）

data Method = Division | Multiplication deriving (Eq)

data Mode = Search | Delete deriving (Eq)

data Scan = Scan { sKey :: !Int, sSlot :: !Int, sIdx :: !Int, sMode :: !Mode }

data World = World
  { m        :: !Int
  , method   :: !Method
  , table    :: !(IM.IntMap [Int])   -- T[i] = キーのリスト（先頭 = リストの先頭）
  , count    :: !Int
  , buf      :: String
  , msg      :: String
  , scan     :: !(Maybe Scan)
  , flashKey :: !Int
  , flashT   :: !Int
  }

initialWorld :: World
initialWorld = rebuild [] (World firstM Division IM.empty 0 "" "Space = random insert, number + Enter = insert your own" Nothing (-1) 0)

hashOf :: World -> Int -> Int
hashOf w k = case method w of
  Division -> k `mod` m w                                  -- 割り算法 h(k) = k mod m
  Multiplication ->                                         -- かけ算法 h(k) = ⌊m (kA mod 1)⌋
    let v = fromIntegral k * constA
    in floor (fromIntegral (m w) * (v - fromIntegral (floor v :: Int)))

slot :: World -> Int -> [Int]
slot w i = IM.findWithDefault [] i (table w)

chainedInsert :: Int -> World -> World
chainedInsert k w
  | k `elem` slot w h = w { msg = show k ++ " is already in" }
  | otherwise = w { table = IM.insert h (k : slot w h) (table w), count = count w + 1   -- リストの先頭に入れる → O(1)
                  , flashKey = k, flashT = 45
                  , msg = "INSERT(" ++ show k ++ "): to the front of list h(" ++ show k ++ ") = " ++ show h }
  where h = hashOf w k

rebuild :: [Int] -> World -> World
rebuild keys w =
  let add acc k = let h = hashOf w k in IM.insertWith (++) h [k] acc
  in w { table = foldl' add IM.empty keys, count = length keys }

allKeysH :: World -> [Int]
allKeysH w = concatMap (slot w) [0 .. m w - 1]

randomInserts :: Int -> World -> IO World
randomInserts n w = foldM (\acc _ -> do
                              k <- randomRIO (0, keyMax - 1)
                              pure (chainedInsert k acc)) w [1 .. n]

step :: Env -> Float -> World -> IO World
step env _ w0 =
  let w1 = w0 { flashT = max 0 (flashT w0 - 1) }
  in pure $ case scan w1 of
       Just s | envFrame env `mod` 16 == 0 -> advanceScan s w1
       _ -> w1

draw :: Env -> World -> Picture
draw env w =
  let y0 = 140
      rh = min 46 ((envH env - 290) / fi (m w))
      x0 = 90
      nw = 58
      maxFit = floor ((envW env - x0 - 120) / nw) :: Int
      row i =
        let y = y0 + fi i * rh
            list = slot w i
            cy = y + rh / 2 - 3
            item (j, k)
              | j > maxFit = Blank
              | j == maxFit = txt env (x0 + 60 + fi j * nw) (cy - 8) 14 (gray 0.78) ("... +" ++ show (length list - maxFit))
              | otherwise =
                  let x = x0 + 60 + fi j * nw
                      isScan = case scan w of
                        Just s -> sSlot s == i && sIdx s == j
                        Nothing -> False
                      fillC = if k == flashKey w && flashT w > 0 then rgb255 255 214 90 else rgb255 (keyHue k) 120 160
                  in Pictures
                       [ seg env 2 (rgb255 90 130 130) (if j == 0 then x0 + 46 else x - 12) cy x cy
                       , box env x (y + 2) (nw - 14) (rh - 10) 8 fillC
                       , boxEdge env x (y + 2) (nw - 14) (rh - 10) 8 (if isScan then 4 else 1) (if isScan then rgb255 255 214 90 else rgb255 80 120 120)
                       , txtC env (x + (nw - 14) / 2) cy 14 (gray 1) (show k)
                       ]
        in Pictures
             ( box env x0 y 46 (rh - 6) 6 (rgb255 30 50 55)
             : boxEdge env x0 y 46 (rh - 6) 6 1.5 (rgb255 60 90 90)
             : txtR env (x0 - 10) (cy - 8) 14 (rgb255 160 200 200) (show i)
             : map item (zip [0 ..] list) )
      longest = maximum (0 : map (length . slot w) [0 .. m w - 1])
      alpha = fi (count w) / fi (m w)
      bw = min 30 (envW env * 0.3 / fi (m w))
      bars = [ let l = fi (length (slot w i)) * 6
               in box env (envW env - 40 - fi (m w - i) * bw) (envH env - 120 - l) (bw - 3) l 0 (rgb255 80 160 160)
             | i <- [0 .. m w - 1] ]
      formula = case method w of
        Division -> "k mod " ++ show (m w) ++ " (division)"
        Multiplication -> "floor(" ++ show (m w) ++ " * (k * 0.618.. mod 1)) (multiplication)"
  in Pictures
       ( [ txt env 30 24 26 (gray 0.94) "Hash Table with Chaining"
         , txt env 30 60 14 (rgb255 150 180 180) "Number + Enter = insert  Number + S = search  Number + D = delete  Space / A = random"
         , txt env 30 78 14 (rgb255 150 180 180) "Up/Down = m  H = hash function  C = clear"
         , txt env 30 100 18 (gray 1) ("Input: " ++ buf w ++ (if envFrame env `mod` 60 < 30 then "|" else " ") ++ "    h(k) = " ++ formula)
         ]
         ++ map row [0 .. m w - 1]
         ++ bars
         ++ [ txt env 30 (envH env - 112) 16 (gray 0.86)
                ("n = " ++ show (count w) ++ "  m = " ++ show (m w) ++ "  load factor alpha = n/m = " ++ fmt 2 alpha ++ "  longest list = " ++ show longest)
            , txt env 30 (envH env - 90) 16 (gray 0.86) ("Failed search on average ~ 1 + alpha = " ++ fmt 2 (1 + alpha))
            , txt env 30 (envH env - 62) 18 (rgb255 255 225 140) (msg w)
            ] )

keyHue :: Int -> Int
keyHue k = (k * 37) `mod` 120 + 40

startScan :: Mode -> World -> World
startScan mode w
  | null (buf w) = w { msg = "Type a number first" }
  | otherwise =
      let k = readNum (buf w)
          h = hashOf w k
          text' = (if mode == Delete then "DELETE(" else "SEARCH(") ++ show k ++ "): look only at list h(" ++ show k ++ ") = " ++ show h ++ " from the front..."
      in if null (slot w h)
           then w { buf = "", msg = text' ++ " empty, so not there", scan = Nothing }
           else w { buf = "", msg = text', scan = Just (Scan k h 0 mode) }

advanceScan :: Scan -> World -> World
advanceScan s w =
  let list = slot w (sSlot s)
  in case drop (sIdx s) list of
       [] -> w { msg = show (sKey s) ++ " not found (saw all " ++ show (length list) ++ " in the list)", scan = Nothing }
       (x : rest)
         | x == sKey s ->
             if sMode s == Delete
               then w { table = IM.insert (sSlot s) (take (sIdx s) list ++ rest) (table w), count = count w - 1
                      , msg = "Deleted " ++ show (sKey s) ++ " (found at item " ++ show (sIdx s + 1) ++ ")", scan = Nothing }
               else w { msg = "Found " ++ show (sKey s) ++ " at item " ++ show (sIdx s + 1) ++ "! No other slot was touched"
                      , flashKey = sKey s, flashT = 60, scan = Nothing }
         | otherwise -> w { scan = Just s { sIdx = sIdx s + 1 } }

-- | m を変えて全部入れ直す（再ハッシュ）
resize :: Int -> World -> World
resize m' w =
  let keys = allKeysH w
  in (rebuild keys w { m = m' }) { msg = "m = " ++ show m' ++ ": everything inserted again (rehash)", scan = Nothing }

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey k Down _ _ -> onKey (normKey k) w
  _ -> pure w

onKey :: Key -> World -> IO World
onKey key w = case key of
  Char c | isDigit c -> pure w { buf = pushDigit 3 c (buf w) }
  SpecialKey KeyBackspace -> pure w { buf = popDigit (buf w) }
  SpecialKey KeyEnter | not (null (buf w)) -> pure (chainedInsert (readNum (buf w)) w) { buf = "" }
  Char 's' -> pure (startScan Search w)
  Char 'd' -> pure (startScan Delete w)
  SpecialKey KeySpace -> randomInserts 1 w
  Char 'a' -> randomInserts 10 w
  Char 'c' -> pure (rebuild [] w) { msg = "Cleared" }
  Char 'h' ->
    let keys = allKeysH w
        w1 = w { method = if method w == Division then Multiplication else Division }
    in pure (rebuild keys w1) { msg = "Changed the hash function and inserted everything again", scan = Nothing }
  SpecialKey KeyUp -> pure (resize (min 20 (m w + 1)) w)
  SpecialKey KeyDown -> pure (resize (max 1 (m w - 1)) w)
  _ -> pure w

main :: IO ()
main = runSketch (sketch "11-05 Hash Table with Chaining" initialWorld)
  { skBack  = rgb255 14 22 24
  , skStart = \_ -> randomInserts 12 initialWorld
  , skStep  = step
  , skDraw  = draw
  , skEvent = onEvent
  }

-- ---------- やってみよう ----------
-- 1. A で 50 個入れて α を 5 くらいにする。↑ で m を増やすとリストが短くなる
-- 2. m = 1 にすると？（ただの連結リスト。探索は O(n)）
-- 3. 10, 20, 30, … と 10 の倍数ばかり入れて m = 10 にすると？ H でかけ算法にすると？


-- ==================================================
-- ここから下は「画面の道具」です
-- p5.js と同じ「左上が原点、y は下向き」の座標で考えて、ここで gloss の座標に直します
-- ==================================================
fi :: Int -> Float
fi = fromIntegral

scr :: Env -> Float -> Float -> Point
scr env x y = (x - envW env / 2, envH env / 2 - y)

-- | 左上をそろえた文字
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y sz c s = let (px, py) = scr env x (y + sz) in label px py sz c s

-- | 中央をそろえた文字
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y sz c s = let (px, py) = scr env x y in labelC px py sz c s

-- | 右上をそろえた文字
txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtR env x y sz c s = txt env (x - fi (length s) * sz * 0.72) y sz c s

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
