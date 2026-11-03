-- ============================================
-- 04 どんな論理式も 3-CNF に変身（SAT から 3-CNF-SAT への還元）
-- (1) 式を木にする → (2) 木の各分かれ目に新しい変数 y をつけ「y <-> (子どうしの演算)」を並べる
-- (3) 各部品を真理値表から CNF（偽になる行を否定）に → (4) 文字が 3 個未満の節を p, q で水増しして、ちょうど 3 個に
-- 式の長さに比例する手間で変換できる（多項式時間の還元）。だから 3-CNF-SAT も NP 完全
-- 左右キー：段階を進める・戻す / 上下キー：(3) で見る部品を選ぶ / 1〜3：式を切り替え
-- ============================================
module Main (main) where

import Art
import Data.Bits (shiftL, shiftR, (.&.))
import Data.List (intercalate, isInfixOf, nub)

-- ★ 自分の式を足そう（x1〜x9、~ & | > = と括弧が使える。> は「ならば」、= は「同値」）
presets :: [String]
presets =
  [ "(~(x3>x1)|(x4=~x2))&x3"
  , "(x1&x2)|(~x1&x3)"
  , "~(x1|x2)>(x3=x1)"
  ]

data Op = ONot | OAnd | OOr | OImp | OIff deriving Eq

-- 構文木：葉は変数、分かれ目は演算子と y の名前
data T = V String | N Op String [T]

-- 部品 y <-> (…) と、その真理値表・CNF・3 文字にそろえた節
data Part = Part
  { pY      :: String
  , pDesc   :: String
  , pVars   :: [String]
  , pRows   :: [([Bool], Bool)]
  , pCnf    :: [[String]]
  , pPadded :: [[String]]
  }

data World = World
  { wTree  :: T
  , wParts :: [Part]
  , wStage :: !Int
  , wPick  :: !Int
  }

main :: IO ()
main = runSketch (sketch "18-04 Any Formula to 3-CNF" (load 0))
  { skBack  = hsb 200 35 9
  , skDraw  = drawW
  , skEvent = onEvent
  }

load :: Int -> World
load k =
  let src = concat (take 1 (drop k presets))
      (raw, _) = parseIff (tokenize src)
      (tree, _) = nameTree 1 raw
  in World tree (buildParts tree) 0 0

-- ---------- 構文解析（<-> < -> < | < & < ~ の順に強く結びつく）----------
tokenize :: String -> [String]
tokenize s = case s of
  [] -> []
  ('x' : d : r) -> ['x', d] : tokenize r
  (c : r)
    | c `elem` "~&|>=()" -> [c] : tokenize r
    | otherwise -> tokenize r

type Parser = [String] -> (T, [String])

parseIff :: Parser
parseIff ts = let (a, r) = parseImp ts in loop a r
  where
    loop a ("=" : r) = let (b, r') = parseImp r in loop (N OIff "" [a, b]) r'
    loop a r = (a, r)

parseImp :: Parser
parseImp ts = case parseOr ts of
  (a, ">" : r) -> let (b, r') = parseImp r in (N OImp "" [a, b], r')
  res -> res

parseOr :: Parser
parseOr ts = let (a, r) = parseAnd ts in loop a r
  where
    loop a ("|" : r) = let (b, r') = parseAnd r in loop (N OOr "" [a, b]) r'
    loop a r = (a, r)

parseAnd :: Parser
parseAnd ts = let (a, r) = parseNot ts in loop a r
  where
    loop a ("&" : r) = let (b, r') = parseNot r in loop (N OAnd "" [a, b]) r'
    loop a r = (a, r)

parseNot :: Parser
parseNot ts = case ts of
  ("~" : r) -> let (a, r') = parseNot r in (N ONot "" [a], r')
  ("(" : r) -> let (a, r') = parseIff r in (a, drop 1 r')
  (v : r) -> (V v, r)
  [] -> (V "?", [])

-- 分かれ目に上から順に y1, y2, … の名前をつける
nameTree :: Int -> T -> (T, Int)
nameTree k t = case t of
  V _ -> (t, k)
  N op _ kids ->
    let go c [] = ([], c)
        go c (x : xs) =
          let (x', c1) = nameTree c x
              (xs', c2) = go c1 xs
          in (x' : xs', c2)
        (kids', k') = go (k + 1) kids
    in (N op ("y" ++ show k) kids', k')

nm :: T -> String
nm (V v) = v
nm (N _ y _) = y

opName :: Op -> String
opName op = case op of
  ONot -> "~"
  OAnd -> "&"
  OOr -> "|"
  OImp -> "->"
  OIff -> "<->"

-- 元の式を読みやすく
showT :: T -> String
showT t = case t of
  V v -> v
  N ONot _ kids -> "~" ++ concatMap showT (take 1 kids)
  N op _ kids -> "(" ++ intercalate (" " ++ opName op ++ " ") (map showT kids) ++ ")"

applyOp :: Op -> Bool -> Bool -> Bool
applyOp op a b = case op of
  ONot -> not a
  OAnd -> a && b
  OOr -> a || b
  OImp -> not a || b
  OIff -> a == b

innerNodes :: T -> [T]
innerNodes t = case t of
  V _ -> []
  N _ _ kids -> t : concatMap innerNodes kids

buildParts :: T -> [Part]
buildParts tree = mkPart "y1" "y1" ["y1"] (\m -> get m "y1") : map fromNode (innerNodes tree)
  where
    get m v = maybe False id (lookup v m)
    fromNode (N op y kids) =
      let ks = map nm kids
          a = concat (take 1 ks)
          b = concat (take 1 (drop 1 ks))
          desc = y ++ " <-> " ++ (if op == ONot then "~" ++ a else "(" ++ a ++ " " ++ opName op ++ " " ++ b ++ ")")
      in mkPart y desc (nub (y : ks)) (\m -> get m y == applyOp op (get m a) (get m b))
    fromNode (V v) = mkPart v v [v] (\m -> get m v)

mkPart :: String -> String -> [String] -> ([(String, Bool)] -> Bool) -> Part
mkPart y desc vars f =
  let k = length vars
      row r =
        let bits = [(r `shiftR` (k - 1 - i)) .&. 1 == 0 | i <- [0 .. k - 1]]
        in (bits, f (zip vars bits))
      rows = map row [0 .. (1 `shiftL` k) - 1 :: Int]
      -- 偽の行を否定 → 節
      cnf = [zipWith (\v b -> if b then "~" ++ v else v) vars bits | (bits, ok) <- rows, not ok]
      pad cl = case cl of
        [_, _, _] -> [cl]
        [_, _] -> [cl ++ ["p"], cl ++ ["~p"]]
        _ -> [cl ++ ["p", "q"], cl ++ ["p", "~q"], cl ++ ["~p", "q"], cl ++ ["~p", "~q"]]
  in Part y desc vars rows cnf (concatMap pad cnf)

-- ---------- 描画 ----------
clauseStr :: [String] -> String
clauseStr c = "(" ++ intercalate " | " c ++ ")"

drawW :: Env -> World -> Picture
drawW env w =
  let stage = wStage w
      titles =
        [ "Step 0: the original formula"
        , "Step 1: make a parse tree"
        , "Step 2: one part y <-> (...) per branch"
        , "Step 3: truth table turns a part into CNF"
        , "Step 4: pad to 3 literals (3-CNF done)"
        ]
      px = envW env * 0.5
      py = 150
      head' = concat (take 1 (drop stage titles))
      common =
        [ txt env 20 34 16 (hsb 0 0 100) "Left/Right: step   Up/Down: choose a part   1-3: formula"
        , txt env 20 72 24 (hsb 45 80 100) head'
        , txt env 20 108 20 (hsb 0 0 90) ("phi = " ++ showT (wTree w))
        ]
      tree = if stage >= 1 then drawTree env w 30 150 (envW env * 0.45) (envH env - 220) else Blank
      body = case stage of
        0 -> drawStage0 env
        2 -> drawStage2 env w px py
        3 -> drawStage3 env w px py
        4 -> drawStage4 env w px py
        _ -> Blank
  in Pictures (tree : body : common)

drawStage0 :: Env -> Picture
drawStage0 env = Pictures
  [ txt env 20 170 18 (hsb 0 0 80) "Can some assignment of x make this true? Mixed shapes are hard to handle."
  , txt env 20 200 18 (hsb 0 0 80) "Right key: turn it into an AND of 3-literal clauses with the same answer."
  , txt env 20 240 18 (hsb 0 0 60) "Symbols: ~ not, & and, | or, -> implies, <-> iff"
  ]

drawStage2 :: Env -> World -> Float -> Float -> Picture
drawStage2 env w px py =
  let parts = wParts w
      row (i, p) =
        txt env px (py + 36 + fromIntegral (i :: Int) * 28) 18
          (if i == wPick w then hsb 45 80 100 else hsb 0 0 85) (pDesc p)
  in Pictures
       ( txt env px py 16 (hsb 0 0 100) "phi' = AND of these parts (same satisfiability)"
       : txt env px (py + 50 + fromIntegral (length parts) * 28) 15 (hsb 0 0 60) "At most 3 vars per part: at most 8 rows"
       : map row (zip [0 ..] parts) )

drawStage3 :: Env -> World -> Float -> Float -> Picture
drawStage3 env w px py = case take 1 (drop (wPick w) (wParts w)) of
  [] -> Blank
  (p : _) ->
    let colX i = px + fromIntegral (i :: Int) * 54
        nv = length (pVars p)
        valX = colX nv + 6
        header = [txt env (colX i) (py + 34) 16 (hsb 0 0 70) v | (i, v) <- zip [0 ..] (pVars p)]
        row (i, (bits, ok)) =
          let y = py + 60 + fromIntegral (i :: Int) * 24
              c = if ok then hsb 0 0 70 else hsb 0 75 100
          in Pictures
               ( txt env valX y 16 c (if ok then "1" else "0  -> negate into a clause")
               : [txt env (colX j) y 16 c (if b then "1" else "0") | (j, b) <- zip [0 ..] bits] )
        cy = py + 80 + fromIntegral (length (pRows p)) * 24
        maxChars = floor ((envW env - px - 20) / (15 * 0.8)) :: Int
        cnfLines = wrapClauses maxChars (map clauseStr (pCnf p))
        cnfRow (i, s) = txt env px (cy + fromIntegral (i :: Int) * 24) 15 (hsb 130 60 100) s
    in Pictures
         ( txt env px py 20 (hsb 45 80 100) (pDesc p)
         : txt env valX (py + 34) 16 (hsb 0 0 70) "value"
         : header
        ++ map row (zip [0 ..] (pRows p))
        ++ map cnfRow (zip [0 ..] cnfLines) )

-- 節を & でつなぎ、長くなったら折り返す
wrapClauses :: Int -> [String] -> [String]
wrapClauses maxChars = go ""
  where
    go cur [] = [cur | not (null cur)]
    go cur (c : r)
      | null cur = go c r
      | length cur + 3 + length c > maxChars = (cur ++ " &") : go c r
      | otherwise = go (cur ++ " & " ++ c) r

drawStage4 :: Env -> World -> Float -> Float -> Picture
drawStage4 env w px py =
  let allC = concatMap pPadded (wParts w)
      colW = (envW env - px - 20) / 2
      item (i, c) =
        let (row, col) = (i :: Int) `divMod` 2
            y = py + 34 + fromIntegral row * 20
            padded = any (\l -> "p" `isInfixOf` l || "q" `isInfixOf` l) c
        in if y > envH env - 40
             then Blank
             else txt env (px + fromIntegral col * colW) y 14
                    (if padded then hsb 280 50 100 else hsb 130 50 95) (clauseStr c)
  in Pictures
       ( txt env px py 16 (hsb 0 0 100) ("3-CNF: " ++ show (length allC) ++ " clauses (each exactly 3 literals)")
       : txt env 20 (envH env - 24) 15 (hsb 0 0 60) "Purple = padded with p, q (listed 2 or 4 ways, so p and q do not matter)"
       : map item (zip [0 ..] allC) )

-- 木の配置：葉を等間隔に並べ、分かれ目は子の真ん中に置く
data Placed = Placed Float Float T [Placed]

depthOf :: T -> Int
depthOf t = case t of
  V _ -> 0
  N _ _ kids -> 1 + maximum (0 : map depthOf kids)

leafCount :: T -> Int
leafCount t = case t of
  V _ -> 1
  N _ _ kids -> sum (map leafCount kids)

placeTree :: Float -> Float -> Float -> Float -> Int -> Int -> T -> Int -> (Placed, Int)
placeTree x0 w y0 lh total d t start =
  let y = y0 + fromIntegral d * lh + 20
  in case t of
       V _ -> (Placed (x0 + (fromIntegral start + 0.5) / fromIntegral total * w) y t [], start + 1)
       N _ _ kids ->
         let go s [] = ([], s)
             go s (k : ks) =
               let (p, s1) = placeTree x0 w y0 lh total (d + 1) k s
                   (ps, s2) = go s1 ks
               in (p : ps, s2)
             (placed, end) = go start kids
             xs = [x | Placed x _ _ _ <- placed]
         in (Placed (sum xs / fromIntegral (max 1 (length xs))) y t placed, end)

drawTree :: Env -> World -> Float -> Float -> Float -> Float -> Picture
drawTree env w x0 y0 tw th =
  let tree = wTree w
      maxD = depthOf tree
      lh = min 80 (th / fromIntegral (maxD + 1))
      (root, _) = placeTree x0 tw y0 lh (leafCount tree) 0 tree 0
      selY = case take 1 (drop (wPick w) (wParts w)) of
        (p : _) | wStage w == 3 -> pY p
        _ -> ""
      edges (Placed x y _ kids) =
        concat [seg env (hsb 0 0 45) 2 (x, y) (kx, ky) : edges k | k@(Placed kx ky _ _) <- kids]
      nodes (Placed x y t kids) =
        let (col, name, tag) = case t of
              V v -> (hsb 130 50 85, v, Blank)
              N op yName _ ->
                ( if yName == selY then hsb 45 90 100 else hsb 200 50 70
                , opName op
                , if wStage w >= 2 then txtC env (x + 30) (y - 16) 14 (hsb 45 80 100) yName else Blank )
        in dot env col (x, y) 40 : txtC env x y 14 (hsb 0 0 10) name : tag : concatMap nodes kids
  in Pictures (edges root ++ nodes root)

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w =
  let n = max 1 (length (wParts w))
  in case ev of
       EventKey (SpecialKey KeyRight) Down _ _ -> pure w { wStage = min 4 (wStage w + 1) }
       EventKey (SpecialKey KeyLeft) Down _ _ -> pure w { wStage = max 0 (wStage w - 1) }
       EventKey (SpecialKey KeyDown) Down _ _ -> pure w { wPick = (wPick w + 1) `mod` n }
       EventKey (SpecialKey KeyUp) Down _ _ -> pure w { wPick = (wPick w + n - 1) `mod` n }
       EventKey (Char c) Down _ _
         | c `elem` "123" -> pure (load (fromEnum c - fromEnum '1'))
       _ -> pure w

-- ---------- やってみよう ----------
-- 1. 式 1 は何個の節になった？ 元の式の長さとの比は？
-- 2. presets に自分の式を足そう（x1〜x9、~ & | > = と括弧が使える）
-- 3. 部品 y <-> ~a の真理値表は何行？ 偽の行はいくつ？

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 直径 d の円（p5.js の circle）
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

-- 文字：左端の下が (x, y)
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str

-- 文字：中心が (x, y)
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = labelC (gx env x) (gy env y) s c str
