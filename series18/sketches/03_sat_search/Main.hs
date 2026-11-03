-- ============================================
-- 03 SAT を探索木で解く
-- 論理式（CNF：(x1 | ~x2 | x3) & … の形）を真にする変数の割り当てはある？
-- 総当たりは 2^n 枚の葉を全部見る。途中で「もう偽の節がある」と気づいたら枝を切る（バックトラック）
-- さらに「残り 1 つで決まる節（単位節）」があれば迷わず進む（DPLL のアイデア）
-- スペース：探索開始・一時停止 / 1：総当たり 2：枝刈り 3：枝刈り＋単位節 / R：新しい式 / U：充足不能な式
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM, replicateM)
import Data.Bits (shiftR, (.&.))
import Data.List (sort, sortOn)
import Data.Maybe (isJust)

speedPerFrame :: Int
speedPerFrame = 2   -- ★ 1 フレームに進める手数

mClauses :: Int
mClauses = 14       -- ★ R で作る節の数（多いほど充足不能になりやすい）

data Status = Open | Sat | Conflict | Skip deriving Eq

data Visit = Visit { vPath :: [Bool], vStatus :: !Status }

-- ジェネレーターが進める状態
data S = S
  { sVisited :: [Visit]        -- 新しいものが先頭
  , sCurrent :: [Bool]         -- 今の割り当て（x1 から順に）
  , sResult  :: Maybe Bool     -- 探索の結果
  }

data World = World
  { wVars    :: !Int
  , wClauses :: [[Int]]        -- 例 [1, -2, 3] = (x1 | ~x2 | x3)
  , wMethod  :: !Int
  , wS       :: !S
  , wRest    :: [S]
  , wHasGen  :: !Bool
  , wRunning :: !Bool
  }

emptyS :: S
emptyS = S [] [] Nothing

main :: IO ()
main = runSketch (sketch "18-03 SAT Search Tree" (World 5 [] 2 emptyS [] False False))
  { skBack  = hsb 260 30 9
  , skStart = \_ -> randomFormula (World 5 [] 2 emptyS [] False False)
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

randomFormula :: World -> IO World
randomFormula w = do
  cls <- replicateM mClauses $ do
    vs <- shuffle [1, 2, 3, 4, 5]
    forM (sort (take 3 vs)) $ \v -> do
      pos <- chance 0.5
      pure (if pos then v else negate v)
  pure (restart w { wVars = 5, wClauses = cls })

unsatFormula :: World -> World
unsatFormula w =
  let allSigns = [[if (m `shiftR` i) .&. 1 == 1 then negate v else v | (i, v) <- zip [0 ..] [1, 2, 3]] | m <- [0 .. 7 :: Int]]
  in restart w { wVars = 4, wClauses = allSigns ++ [[4, -1, 2]] }

restart :: World -> World
restart w =
  let program = do
        r <- solve (wMethod w) (wVars w) (wClauses w) []
        modify (\s -> s { sResult = Just r })
        yield
  in w { wS = emptyS, wRest = steps program emptyS, wHasGen = True, wRunning = False }

-- 節の状態： 1 = 真, -1 = 偽（全部の文字が偽）, 0 = まだ決まらない
clauseState :: [Bool] -> [Int] -> Int
clauseState a cl
  | any isTrue cl = 1
  | any isOpen cl = 0
  | otherwise = -1
  where
    n = length a
    isOpen lit = abs lit > n
    isTrue lit = case drop (abs lit - 1) a of
      (v : _) -> (lit > 0) == v
      [] -> False

-- 次の変数が単位節で決まるか
forcedValue :: [[Int]] -> [Bool] -> Maybe Bool
forcedValue cls a =
  let next = length a + 1
      unit cl
        | clauseState a cl /= 0 = Nothing
        | otherwise = case filter (\l -> abs l > length a) cl of
            [l] | abs l == next -> Just (l > 0)
            _ -> Nothing
  in case [v | Just v <- map unit cls] of
       (v : _) -> Just v
       [] -> Nothing

visit :: [Bool] -> Status -> Gen S ()
visit p st = modify (\s -> s { sVisited = Visit p st : sVisited s })

solve :: Int -> Int -> [[Int]] -> [Bool] -> Gen S Bool
solve method nVars cls a = do
  modify (\s -> s { sCurrent = a })
  let states = map (clauseState a) cls
      conflict = (-1) `elem` states
  if length a == nVars
    then do
      let ok = all (== 1) states
      visit a (if ok then Sat else Conflict)
      yield
      pure ok
    else if conflict && method >= 2
      then visit a Conflict >> yield >> pure False
      else do
        visit a Open
        yield
        let forced = if method == 3 then forcedValue cls a else Nothing
            try [] = pure False
            try (v : vs)
              | isJust forced && forced /= Just v = visit (a ++ [v]) Skip >> try vs
              | otherwise = do
                  r <- solve method nVars cls (a ++ [v])
                  if r then pure True else try vs
        try [True, False]

advance :: World -> World
advance w = case wRest w of
  [] -> w { wRunning = False, wHasGen = False }
  (s : r)
    | isJust (sResult s) -> w { wS = s, wRest = [], wRunning = False, wHasGen = False }
    | otherwise -> w { wS = s, wRest = r }

stepW :: Env -> Float -> World -> IO World
stepW _ _ w
  | wRunning w = pure (iterate advance w !! speedPerFrame)
  | otherwise = pure w

litName :: Int -> String
litName l = (if l > 0 then "x" else "~x") ++ show (abs l)

clauseText :: [Int] -> String
clauseText cl = "(" ++ joinWith "|" (map litName cl) ++ ")"

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith sep (x : xs) = x ++ concatMap (sep ++) xs

-- 節を横に並べ、はみ出したら次の行へ
layoutClauses :: Float -> [String] -> ([(Float, Float)], Float)
layoutClauses width = go 20 70 []
  where
    go _ y acc [] = (reverse acc, y)
    go x y acc (s : r) =
      let w = fromIntegral (length s) * 15 * 0.8 + 16
          (x', y') = if x + w > width - 20 then (20, y + 26) else (x, y)
      in go (x' + w) y' ((x', y') : acc) r

drawW :: Env -> World -> Picture
drawW env w =
  let s = wS w
      current = sCurrent s
      visited = reverse (sVisited s)
      nVars = wVars w
      texts = map clauseText (wClauses w)
      (spots, yEnd) = layoutClauses (envW env) texts
      clauseLabel ((x, y), (cl, str)) =
        let st = clauseState current cl
            c | st == 1 = hsb 130 70 90
              | st == -1 = hsb 0 80 100
              | otherwise = hsb 0 0 75
        in txt env x y 15 c str
      top = yEnd + 50
      bottom = envH env - 90
      l0 = 40
      w0 = envW env - 80
      levelH = (bottom - top) / fromIntegral nVars
      place path =
        let idx = foldl (\i b -> i * 2 + (if b then 0 else 1)) (0 :: Int) path
            d = length path
        in (l0 + (fromIntegral idx + 0.5) / 2 ^ d * w0, top + fromIntegral d * levelH)
      edge v = case reverse (vPath v) of
        [] -> Blank
        (lastB : _) ->
          let a = place (take (length (vPath v) - 1) (vPath v))
              b = place (vPath v)
          in if vStatus v == Skip
               then seg env (hsb 0 0 25) 1 a b
               else seg env (if lastB then hsb 190 60 80 else hsb 30 60 80) 2 a b
      mark v =
        let p@(x, y) = place (vPath v)
            red = hsb 0 90 100
        in case vStatus v of
             Sat -> dot env (hsb 120 90 100) p 20
             Conflict -> Pictures
               [ seg env red 3 (x - 6, y - 6) (x + 6, y + 6)
               , seg env red 3 (x - 6, y + 6) (x + 6, y - 6)
               ]
             Skip -> dot env (hsb 0 0 30) p 6
             Open -> dot env (hsb 0 0 90) p 10
      levelLabel d =
        txtM env 4 (top + (fromIntegral d + 0.5) * levelH) 14 (hsb 0 0 60) ("set x" ++ show (d + 1))
      names = case wMethod w of
        1 -> "Brute force"
        2 -> "Pruning (backtracking)"
        _ -> "Pruning + unit clause (DPLL-like)"
      work = length (filter ((/= Skip) . vStatus) visited)
      status = case sResult s of
        Just True -> "Satisfiable! Found an assignment"
        Just False -> "Unsatisfiable (every assignment is false)"
        Nothing -> "Now: " ++ unwords (zipWith (\i b -> "x" ++ show (i :: Int) ++ "=" ++ (if b then "T" else "F")) [1 ..] current)
  in Pictures
       ( map clauseLabel (zip spots (zip (wClauses w) texts))
      ++ map edge visited
      ++ map mark visited
      ++ map levelLabel [0 .. nVars - 1]
      ++ [ txt env 20 34 16 (hsb 0 0 100) "Space: search   1: brute force   2: pruning   3: + unit clause   R: new   U: unsatisfiable"
         , txt env 20 (envH env - 52) 18 (hsb 50 80 100)
             (names ++ "   visited " ++ show work ++ " (brute force: " ++ show (2 ^ (nVars + 1) - 1 :: Int) ++ ")")
         , txt env 20 (envH env - 22) 18 (hsb 0 0 100) status
         , txtR env (envW env - 20) (envH env - 28) 14 (hsb 0 0 55) "Left = T  Right = F  X = false clause  Dot = solution"
         ] )

onEvent :: Env -> Event -> World -> IO World
onEvent _ ev w = case ev of
  EventKey (SpecialKey KeySpace) Down _ _ ->
    let w1 = if wHasGen w then w else restart w
    in pure w1 { wRunning = not (wRunning w1) }
  EventKey (Char c) Down _ _
    | c `elem` "123" -> pure (restart w { wMethod = fromEnum c - fromEnum '0' })
    | c `elem` "rR" -> randomFormula w
    | c `elem` "uU" -> pure (unsatFormula w)
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 同じ式で 1・2・3 を比べよう。訪れた節点はどれだけ減る？
-- 2. U（充足不能）では全部調べないと「ない」と言えない。何節点かかる？
-- 3. mClauses を 8 や 24 に。変数 5 個で節が何個をこえると充足不能が増える？（相転移：節/変数 = 約 4.27）

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

-- 文字：左端の上下中央が (x, y)
txtM :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtM env x y s c str = label (gx env x) (gy env y - s / 2) s c str

-- 文字：右端の上下中央が (x, y)
txtR :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtR env x y s c str =
  label (gx env x - fromIntegral (length str) * s * 0.72) (gy env y - s / 2) s c str

-- ---------- 道具：その他 ----------
-- リストをランダムに並べかえる（p5.js の shuffle）
shuffle :: [a] -> IO [a]
shuffle xs = do
  ks <- mapM (const (randomRIO (0, 1 :: Float))) xs
  pure (map snd (sortOn fst (zip ks xs)))

-- 確率 p で True
chance :: Float -> IO Bool
chance p = (< p) <$> randomRIO (0, 1)

-- ---------- 道具：ジェネレーター（function*）の代わり ----------
-- yield のたびに状態を 1 つ流す計算。結果は「状態の遅延リスト」になり、
-- World がその残りを持っていて、1 手進めるたびに先頭を 1 つ取る。
newtype Gen s a = Gen { unGen :: s -> (a -> s -> [s]) -> [s] }

instance Functor (Gen s) where
  fmap f (Gen g) = Gen (\s k -> g s (k . f))

instance Applicative (Gen s) where
  pure x = Gen (\s k -> k x s)
  Gen f <*> Gen g = Gen (\s k -> f s (\h s' -> g s' (k . h)))

instance Monad (Gen s) where
  Gen g >>= f = Gen (\s k -> g s (\x s' -> unGen (f x) s' k))

yield :: Gen s ()
yield = Gen (\s k -> s : k () s)

modify :: (s -> s) -> Gen s ()
modify f = Gen (\s k -> k () $! f s)

steps :: Gen s () -> s -> [s]
steps (Gen g) s0 = g s0 (\_ _ -> [])
