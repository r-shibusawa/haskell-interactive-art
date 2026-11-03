-- ============================================
-- 00 スタックとキュー
-- スタック = 最後に入れたものが最初に出る（LIFO：お皿の山）
-- キュー   = 最初に入れたものが最初に出る（FIFO：レジの行列）
-- どちらも「配列 1 本 + 番号 1〜2 個」で作れる。キューは配列の端と端をつないだ「輪」として使う
-- 操作：P = プッシュ / O = ポップ / E = エンキュー / D = デキュー / スペース = 自動で動かす / R = リセット
-- ============================================
module Main (main) where

import Art
import Data.Char (toLower)
import qualified Data.IntMap.Strict as IM

cap :: Int
cap = 8            -- ★ 配列の大きさ（キューに入るのは cap-1 個まで：1 マス空けて「満杯」と「空」を区別する）

autoSpeed :: Int
autoSpeed = 40     -- ★ 自動モードで何フレームごとに操作するか

data World = World
  { sCells  :: !(IM.IntMap Int)  -- スタック：S[0..sTop-1] に入っている
  , sTop    :: !Int              -- 「次に入れる場所」＝今の個数
  , qCells  :: !(IM.IntMap Int)  -- キューの配列
  , qHead   :: !Int              -- Q[qHead] が先頭
  , qTail   :: !Int              -- Q[qTail] が次に入れる場所
  , counter :: !Int              -- 入れる値（1, 2, 3, …）
  , msg     :: String
  , flashS  :: !Int
  , flashQ  :: !Int
  , flashT  :: !Int
  , warn    :: String
  , auto    :: !Bool
  }

initialWorld :: World
initialWorld = resetAll (World IM.empty 0 IM.empty 0 0 1 "P/O for the stack, E/D for the queue. Space = auto" (-1) (-1) 0 "" False)

resetAll :: World -> World
resetAll w = w { sCells = IM.empty, sTop = 0, qCells = IM.empty, qHead = 0, qTail = 0, counter = 1, warn = "" }

-- ---------- スタック ----------
stackPush :: Int -> World -> World
stackPush x w
  | sTop w == cap = w { warn = "Stack overflow (full)!" }
  | otherwise =
      let t = sTop w + 1
      in w { sCells = IM.insert (sTop w) x (sCells w), flashS = sTop w, sTop = t, flashT = 30
           , msg = "PUSH(" ++ show x ++ "): put in S[" ++ show (t - 1) ++ "], top = " ++ show t }

stackPop :: World -> World
stackPop w
  | sTop w == 0 = w { warn = "Stack underflow (empty)!" }
  | otherwise =
      let t = sTop w - 1
          x = IM.findWithDefault 0 t (sCells w)
      in w { sTop = t, sCells = IM.delete t (sCells w), flashS = t, flashT = 30
           , msg = "POP() -> " ++ show x ++ " (the last one in), top = " ++ show t }

-- ---------- キュー（循環バッファ）----------
enqueue :: Int -> World -> World
enqueue x w
  | (qTail w + 1) `mod` cap == qHead w = w { warn = "Queue overflow (full)!" }
  | otherwise =
      let t = (qTail w + 1) `mod` cap   -- 端まで来たら 0 へ戻る
      in w { qCells = IM.insert (qTail w) x (qCells w), flashQ = qTail w, flashT = 30, qTail = t
           , msg = "ENQUEUE(" ++ show x ++ "): put in Q[" ++ show (qTail w) ++ "], tail = " ++ show t }

dequeue :: World -> World
dequeue w
  | qHead w == qTail w = w { warn = "Queue underflow (empty)!" }
  | otherwise =
      let h = qHead w
          x = IM.findWithDefault 0 h (qCells w)
          h' = (h + 1) `mod` cap
      in w { qCells = IM.delete h (qCells w), flashQ = h, flashT = 30, qHead = h'
           , msg = "DEQUEUE() -> " ++ show x ++ " (the first one in), head = " ++ show h' }

-- | 次の値を使う（満杯でも番号は進む）
nextValue :: (Int -> World -> World) -> World -> World
nextValue f w = f (counter w) w { counter = counter w + 1 }

step :: Env -> Float -> World -> IO World
step env _ w0 = do
  w1 <- if auto w0 && envFrame env `mod` autoSpeed == 0
          then do
            r <- randomRIO (0, 1 :: Float)
            pure $ if r < 0.28 then nextValue stackPush w0
                   else if r < 0.5 then stackPop w0
                   else if r < 0.78 then nextValue enqueue w0
                   else dequeue w0
          else pure w0
  pure w1 { flashT = max 0 (flashT w1 - 1) }

draw :: Env -> World -> Picture
draw env w = Pictures
  [ txt env 30 24 26 (gray 0.94) "Stacks and Queues"
  , txt env 30 60 14 (rgb255 160 170 200)
      ("P = push  O = pop  E = enqueue  D = dequeue  Space = auto " ++ (if auto w then "ON" else "OFF") ++ "  R = reset")
  , drawStack env w (envW env * 0.2) (envH env * 0.22)
  , drawQueue env w (envW env * 0.66) (envH env * 0.52)
  , txt env 30 (envH env - 80) 18 (rgb255 255 225 140) (msg w)
  , txt env 30 (envH env - 48) 18 (rgb255 255 110 110) (warn w)
  ]

cellEdge, emptyCell, yellow' :: Color
cellEdge = rgb255 70 80 110
emptyCell = rgb255 28 32 48
yellow' = rgb255 255 214 90

drawStack :: Env -> World -> Float -> Float -> Picture
drawStack env w cx y0 =
  let cw = 120
      ch = min 52 (envH env * 0.6 / fi cap)
      cell i =
        let y = y0 + fi (cap - 1 - i) * ch
            inside = i < sTop w
            fillC | i == flashS w && flashT w > 0 = yellow'
                  | inside = rgb255 80 200 160
                  | otherwise = emptyCell
        in Pictures
             [ box env (cx - cw / 2) y cw (ch - 4) 6 fillC
             , boxEdge env (cx - cw / 2) y cw (ch - 4) 6 1.5 cellEdge
             , case IM.lookup i (sCells w) of
                 Just v -> txtC env cx (y + ch / 2 - 2) 20 (gray (if inside then 0.04 else 0.35)) (show v)
                 Nothing -> Blank
             , txtR env (cx - cw / 2 - 10) (y + ch / 2 - 9) 14 (rgb255 110 120 150) (show i)
             ]
      ty = y0 + fi (cap - 1 - sTop w) * ch + ch / 2
  in Pictures
       ( txtC env cx (y0 - 20) 18 (gray 0.78) "Stack S (LIFO)"
       : txt env (cx + cw / 2 + 12) (clampF (y0 - 20) (y0 + fi cap * ch) ty - 8) 16 (rgb255 255 160 90) ("<- top = " ++ show (sTop w))
       : map cell [0 .. cap - 1] )

drawQueue :: Env -> World -> Float -> Float -> Picture
drawQueue env w cx cy =
  let r = min (envW env * 0.22) (envH env * 0.25)   -- ★ 輪の半径
      cnt = (qTail w - qHead w + cap) `mod` cap
      angle i = -pi / 2 + tau * fi i / fi cap
      cell i =
        let a = angle i
            x = cx + cos a * r
            y = cy + sin a * r
            inside = (i - qHead w + cap) `mod` cap < cnt
            fillC | i == flashQ w && flashT w > 0 = yellow'
                  | inside = rgb255 110 160 255
                  | otherwise = emptyCell
        in Pictures
             [ circ env x y 32 fillC
             , circEdge env x y 32 1.5 cellEdge
             , case IM.lookup i (qCells w) of
                 Just v -> txtC env x y 20 (gray (if inside then 0.04 else 0.35)) (show v)
                 Nothing -> Blank
             , txtC env (cx + cos a * (r - 52)) (cy + sin a * (r - 52)) 14 (rgb255 110 120 150) (show i)
             ]
      needle (idx, lab, c, off) =
        let a = angle idx
        in Pictures
             [ seg env 3 c (cx + cos a * (r + 34)) (cy + sin a * (r + 34)) (cx + cos a * (r + off - 12)) (cy + sin a * (r + off - 12))
             , txtC env (cx + cos a * (r + off + 6)) (cy + sin a * (r + off + 6)) 15 c lab
             ]
  in Pictures
       ( txtC env cx 96 18 (gray 0.78) "Queue Q (FIFO, an array bent into a ring)"
       : map cell [0 .. cap - 1]
       ++ map needle [(qHead w, "head", rgb255 255 160 90, 58), (qTail w, "tail", rgb255 160 255 140, 82)]
       ++ [txtC env cx cy 16 (gray 0.78) ("count " ++ show cnt ++ " / " ++ show (cap - 1))] )

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey k Down _ _ -> pure (onKey (normKey k) w { warn = "" })
  _ -> pure w

onKey :: Key -> World -> World
onKey k w = case k of
  Char 'p' -> nextValue stackPush w
  Char 'o' -> stackPop w
  Char 'e' -> nextValue enqueue w
  Char 'd' -> dequeue w
  SpecialKey KeySpace -> w { auto = not (auto w) }
  Char 'r' -> (resetAll w) { msg = "Reset" }
  _ -> w

main :: IO ()
main = runSketch (sketch "11-00 Stacks and Queues" initialWorld)
  { skBack  = rgb255 16 18 30
  , skStep  = step
  , skDraw  = draw
  , skEvent = onEvent
  }

-- ---------- やってみよう ----------
-- 1. P を 9 回押すとどうなる？　（cap = 8 なのでオーバーフロー）
-- 2. E を 3 回、D を 3 回、を繰り返すと head と tail がぐるぐる回る。配列の端で 0 に戻るのが「循環」
-- 3. キューは cap-1 個しか入らない。なぜ 1 マス空ける必要がある？（qHead == qTail が「空」の意味だから）


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

-- | 塗りつぶした円（中心と半径）
circ :: Env -> Float -> Float -> Float -> Color -> Picture
circ env x y r c = at (scr env x y) (Color c (disc r))

-- | 円の枠（中心、半径、太さ）
circEdge :: Env -> Float -> Float -> Float -> Float -> Color -> Picture
circEdge env x y r sw c = at (scr env x y) (Color c (ring r sw))

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
