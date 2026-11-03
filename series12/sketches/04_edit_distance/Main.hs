-- ============================================
-- 04 編集距離：単語を何回の操作で別の単語に変えられる？
-- 1 文字の「置換」「挿入」「削除」を 1 回と数え、X を Y に変える最小回数を求める
-- d[i][j] = min( d[i-1][j-1] + (同じ文字なら 0 / ちがえば 1), d[i-1][j] + 1 削除, d[i][j-1] + 1 挿入 )
-- 単語をクリックしてキーボードで書きかえ（Enter で決定）/ スペース = 再生・停止 / → = 1 マス / 1〜3 = お手本 / R = 最初から
-- ============================================
module Main (main) where

import Art
import Data.Array
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Text.Printf (printf)

startX, startY :: String
startX = "kitten"     -- ★ 変える前
startY = "sitting"    -- ★ 変えた後

costReplace :: Int
costReplace = 1       -- ★ 置換のコスト（2 にすると「削除＋挿入」と同じ → LCS と関係する）

data Op = Same | Replace | Insert | Delete deriving (Eq, Ord, Enum, Bounded)

-- 復元した操作の 1 手
data Step = Step { sO :: !Op, sI :: !Int, sJ :: !Int, sA :: !Char, sB :: !Char }

data World = World
  { wX       :: !String
  , wY       :: !String
  , wTab     :: !(Array (Int, Int) (Int, Op))
  , wOps     :: ![Step]
  , wFilled  :: !Float
  , wPlaying :: !Bool
  , wEdit    :: !(Maybe Char)   -- いま書きかえている単語（'X' か 'Y'）
  }

main :: IO ()
main = runSketch (sketch "12-04 Edit distance" (restart (World startX startY (editTable "" "") [] 0 True Nothing)))
  { skBack  = cBg
  , skStep  = \_ _ w -> pure (step w)
  , skDraw  = drawWorld
  , skEvent = \env ev w -> pure (onEvent env ev w)
  }

-- ★ 表を作る。3 つの候補のうちコストが一番小さいものを選ぶ
editTable :: String -> String -> Array (Int, Int) (Int, Op)
editTable xs ys = t
  where
    m = length xs
    n = length ys
    t = array ((0, 0), (m, n)) [((i, j), f i j) | i <- [0 .. m], j <- [0 .. n]]
    f 0 0 = (0, Same)
    f i 0 = (i, Delete)
    f 0 j = (j, Insert)
    f i j =
      let same = nth ' ' xs (i - 1) == nth ' ' ys (j - 1)
          cands = [ (fst (t ! (i - 1, j - 1)) + (if same then 0 else costReplace), if same then Same else Replace)
                  , (fst (t ! (i - 1, j)) + 1, Delete)
                  , (fst (t ! (i, j - 1)) + 1, Insert) ]
      in case sortOn fst cands of
           (best : _) -> best
           [] -> (0, Same)

-- 右下から操作をたどる
traceOps :: String -> String -> Array (Int, Int) (Int, Op) -> [Step]
traceOps xs ys t = reverse (go (length xs) (length ys))
  where
    go 0 0 = []
    go i j =
      let o = snd (t ! (i, j))
          a = if o == Insert then '-' else nth ' ' xs (i - 1)
          b = if o == Delete then '-' else nth ' ' ys (j - 1)
          next = case o of
            Delete -> go (i - 1) j
            Insert -> go i (j - 1)
            _ -> go (i - 1) (j - 1)
      in Step o i j a b : next

restart :: World -> World
restart w =
  let t = editTable (wX w) (wY w)
  in w { wTab = t, wOps = traceOps (wX w) (wY w) t, wFilled = 0 }

limitF :: World -> Float
limitF w = i2f (length (wX w) * length (wY w) + length (wOps w) + 1)

step :: World -> World
step w = case wEdit w of
  Nothing | wPlaying w ->
    w { wFilled = min (limitF w) (wFilled w + max 0.25 (i2f (length (wX w) * length (wY w)) / 300)) }
  _ -> w

onEvent :: Env -> Event -> World -> World
onEvent env ev w = case wEdit w of
  Just which -> case ev of
    EventKey (SpecialKey KeyEnter) Down _ _ -> restart w { wEdit = Nothing }
    EventKey (SpecialKey KeyDelete) Down _ _ -> backspace which
    EventKey (SpecialKey KeyBackspace) Down _ _ -> backspace which
    EventKey (Char c) Down _ _
      | c == '\b' || c == '\DEL' -> backspace which
      | c == '\r' -> restart w { wEdit = Nothing }
      | c > ' ' && c <= '~' -> typed which c
    EventKey (MouseButton LeftButton) Down _ _ -> click
    _ -> w
  Nothing -> case ev of
    EventKey (SpecialKey KeySpace) Down _ _ -> w { wPlaying = not (wPlaying w) }
    EventKey (SpecialKey KeyRight) Down _ _ ->
      w { wPlaying = False, wFilled = min (limitF w) (i2f (floor (wFilled w) + 1 :: Int)) }
    EventKey (Char '1') Down _ _ -> restart w { wX = "kitten", wY = "sitting" }
    EventKey (Char '2') Down _ _ -> restart w { wX = "program", wY = "paragraph" }
    EventKey (Char '3') Down _ _ -> restart w { wX = "sunday", wY = "saturday" }
    EventKey (Char c) Down _ _ | c `elem` "rR" -> w { wFilled = 0, wPlaying = True }
    EventKey (MouseButton LeftButton) Down _ _ -> click
    _ -> w
  where
    (mx, my) = mouseP env
    click
      | mx > 64 && mx < wordRight && abs (my - 112) < 20 = w { wEdit = Just 'X' }
      | mx > 64 && mx < wordRight && abs (my - 156) < 20 = w { wEdit = Just 'Y' }
      | otherwise = case wEdit w of
          Just _ -> restart w { wEdit = Nothing }
          Nothing -> w
    -- 書きかえのたびに表も作り直す（マスは Enter のあとで 1 つずつ埋まる）
    edit f which = restart (if which == 'X' then w { wX = f (wX w) } else w { wY = f (wY w) })
    backspace = edit (\s -> take (length s - 1) s)
    typed which c = edit (\s -> if length s < 14 then s ++ [c] else s) which

wordRight :: Float
wordRight = 444

opColor :: Op -> Color
opColor o = case o of
  Same -> cLime
  Replace -> cAmber
  Insert -> cCyan
  Delete -> cRasp

opName :: Op -> String
opName o = case o of
  Same -> "match"
  Replace -> "replace"
  Insert -> "insert"
  Delete -> "delete"

opLetter :: Op -> String
opLetter = take 1 . opName

drawWorld :: Env -> World -> Picture
drawWorld env w = screen env
  ( [ txt 26 cFg 32 24 "Edit distance (Levenshtein distance)"
    , txt 15 cMuted 32 60 "Click a word to edit, then Enter   Space: play/pause   Right: 1 cell   1-3: samples   R: restart" ]
  ++ wordBox "X" (wX w) 112 'X' ++ wordBox "Y" (wY w) 156 'Y'
  ++ headers ++ cells ++ panel )
  where
    xs = wX w
    ys = wY w
    m = length xs
    n = length ys
    total = m * n
    shown = floor (wFilled w) :: Int
    wordBox lab s yy which =
      let editing = wEdit w == Just which
          cursor = if editing && envFrame env `mod` 40 < 20 then "|" else ""
      in [ txtL 24 cMuted 32 yy lab
         , rbox (if editing then rgb255 0x3b 0x2a 0x44 else cPanel) 64 (yy - 20) (wordRight - 64) 40
         , txtL 24 cFg 76 yy (s ++ cursor) ]
    cell = minimum [54, envW env * 0.55 / i2f (n + 2), (envH env - 300) / i2f (m + 2)]
    x0 = 32
    y0 = 200
    (ci, cj) | shown < total = (shown `div` max 1 n + 1, shown `mod` max 1 n + 1)
             | otherwise = (-1, -1)
    fs = max 14 (min 18 (cell * 0.42))
    headers =
      [ txtC fs (if j == cj then cAmber else cMuted) (x0 + i2f (j + 1) * cell + cell / 2) (y0 + cell / 2)
          (if j == 0 then "-" else [nth ' ' ys (j - 1)])
      | j <- [0 .. n] ]
      ++ [ txtC fs (if i == ci then cAmber else cMuted) (x0 + cell / 2) (y0 + i2f (i + 1) * cell + cell / 2)
             (if i == 0 then "-" else [nth ' ' xs (i - 1)])
         | i <- [0 .. m] ]
    ps = shown - total
    nOps = length (wOps w)
    pathOn = M.fromList [ ((sI o, sJ o), sO o) | (k, o) <- zip [0 ..] (wOps w), k >= nOps - ps ]
    cells = concat
      [ [ cellBox col (if isCur then cAmber else if dep then cCyan else cGrid) (if isCur || dep then 2.5 else 1)
                  (x + 1) (y + 1) (cell - 2) (cell - 2) ]
        ++ [txtC 14 cFg (x + cell / 2) (y + cell / 2) (show (fst (wTab w ! (i, j)))) | visible || isCur]
      | i <- [0 .. m], j <- [0 .. n]
      , let x = x0 + i2f (j + 1) * cell
      , let y = y0 + i2f (i + 1) * cell
      , let idx = (i - 1) * n + (j - 1)
      , let visible = i == 0 || j == 0 || idx < shown
      , let isCur = i == ci && j == cj
      , let dep = shown < total && (i == ci - 1 || i == ci) && (j == cj - 1 || j == cj) && not isCur
      , let col = case M.lookup (i, j) pathOn of
              Just o -> mixC (opColor o) cBg 0.55
              Nothing -> if visible then cPanel else cBg ]
    px = x0 + i2f (n + 2) * cell + 30
    pw = envW env - px - 30
    panel =
      [ rbox cPanel px y0 pw 360
      , txt 18 cFg (px + 18) (y0 + 16) ("Edit distance = " ++ show (fst (wTab w ! (m, n)))) ]
      ++ (if shown >= total then alignment else [])
    k0 = max 0 (nOps - ps)
    bw = min 36 ((pw - 40) / i2f (max 1 nOps))
    alignment = concat
      [ [ rbox (if on then opColor (sO o) else cGrid) x (y0 + 60) (bw - 4) 110
        , txtC (max 14 (min 20 (bw * 0.6))) cBg (x + bw / 2 - 2) (y0 + 88) [sA o]
        , txtC (max 14 (min 20 (bw * 0.6))) cBg (x + bw / 2 - 2) (y0 + 142) [sB o] ]
        ++ [txtC 14 cBg (x + bw / 2 - 2) (y0 + 115) (opLetter (sO o)) | on]
      | (k, o) <- zip [0 ..] (wOps w)
      , let x = px + 18 + i2f k * bw
      , let on = k >= k0 ]
      ++ concat
      [ [ box (opColor op) (px + 18) (yy + 2) 14 14
        , txt 14 cFg (px + 40) yy (opLetter op ++ " = " ++ opName op ++ ": " ++ show (length (filter ((== op) . sO) (wOps w))) ++ " times") ]
      | (r, op) <- zip [0 :: Int ..] [minBound .. maxBound]
      , let yy = y0 + 200 + i2f r * 26 ]

-- ---------- やってみよう ----------
-- 1. costReplace を 2 にすると「置換」が消える。LCS との関係を考える
-- 2. 自分の名前と友だちの名前の距離を測る（単語をクリックして入力）
-- 3. 隣の文字の入れ替えも 1 回と数える候補を editTable に足してみる

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

-- | 左上 (x, y)、幅 w、高さ h の塗りつぶした四角
box :: Color -> Float -> Float -> Float -> Float -> Picture
box c x y w h = Color c (Polygon [(x, -y), (x + w, -y), (x + w, -y - h), (x, -y - h)])

-- | 角を落とした四角の頂点（p5.js の rect の角丸の代わり）
cornerPts :: Float -> Float -> Float -> Float -> [Point]
cornerPts x y w h =
  let r = min 5 (min (abs w) (abs h) / 3)
  in [ (x + r, -y), (x + w - r, -y), (x + w, -y - r), (x + w, -y - h + r)
     , (x + w - r, -y - h), (x + r, -y - h), (x, -y - h + r), (x, -y - r) ]

-- | 角を落とした塗りつぶしの四角
rbox :: Color -> Float -> Float -> Float -> Float -> Picture
rbox c x y w h = Color c (Polygon (cornerPts x y w h))

-- | 四角の枠線（太さ t）
outline :: Color -> Float -> Float -> Float -> Float -> Float -> Picture
outline c t x y w h = case cornerPts x y w h of
  ps@(p : _) | t <= 1.2  -> Color c (Line (ps ++ [p]))
             | otherwise -> Color c (thickPath t (ps ++ [p]))
  [] -> Blank

-- | 塗りと枠線のある表のマス
cellBox :: Color -> Color -> Float -> Float -> Float -> Float -> Float -> Picture
cellBox f s t x y w h = Pictures [rbox f x y w h, outline s t x y w h]

-- | 文字：(x, y) を左上にして書く
txt :: Float -> Color -> Float -> Float -> String -> Picture
txt size c x y = label x (-y - size) size c

-- | 文字：(x, y) を左端の真ん中にして書く
txtL :: Float -> Color -> Float -> Float -> String -> Picture
txtL size c x y = label x (-y - size / 2) size c

-- | 文字：(x, y) を中心にして書く
txtC :: Float -> Color -> Float -> Float -> String -> Picture
txtC size c x y s = label (x - fromIntegral (length s) * size * 0.4) (-y - size / 2) size c s

-- | 2 色を混ぜる（p5.js の lerpColor）
mixC :: Color -> Color -> Float -> Color
mixC a b t0 =
  let t = clampF 0 1 t0
      (r1, g1, b1, _) = rgbaOfColor a
      (r2, g2, b2, _) = rgbaOfColor b
  in makeColor (lerpF r1 r2 t) (lerpF g1 g2 t) (lerpF b1 b2 t) 1

-- | リストの i 番目（範囲外なら d）
nth :: a -> [a] -> Int -> a
nth d xs i
  | i < 0 = d
  | otherwise = case drop i xs of
      (x : _) -> x
      [] -> d

-- | 整数を小数に
i2f :: Int -> Float
i2f = fromIntegral
