-- ============================================
-- 05 3-CNF-SAT → クリーク（還元）
-- 節が k 個の論理式を「グラフ」に変身させる。各節の 3 つの文字を点にし、
-- 「ちがう節」かつ「x と ~x のように矛盾しない」文字どうしを線で結ぶ
-- → 式が充足可能 ⇔ このグラフに大きさ k のクリーク（全員が互いにつながった集まり）がある
-- 点をクリックして各節から 1 つずつ選ぶ / スペース：クリークを自動で探す / 1：最初の例 R：ランダムな式
-- ============================================
module Main (main) where

import Art
import Control.Monad (forM, replicateM)
import Data.Array (Array, bounds, elems, listArray, (!))
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate, sort, sortOn)
import Data.Maybe (isJust)

-- ★ 最初の例（3 節）
firstFormula :: [[Int]]
firstFormula = [[1, 2, -3], [-1, -2, 3], [-1, 2, -3]]

data Vert = Vert { vC :: !Int, vLit :: !Int, vX :: !Float, vY :: !Float }

-- ジェネレーターが進める状態
data S = S
  { sChosen :: [Int]         -- 節ごとに選んだ点の番号（-1 = なし）
  , sTries  :: !Int
  , sFound  :: Maybe Bool    -- 探索の結果
  }

data World = World
  { wClauses :: [[Int]]
  , wVerts   :: Array Int Vert
  , wEdges   :: [(Int, Int)]
  , wS       :: !S
  , wRest    :: [S]
  , wHasGen  :: !Bool
  , wRunning :: !Bool
  }

main :: IO ()
main = runSketch (sketch "18-05 3-CNF-SAT to Clique" (setFormula env0' firstFormula))
  { skBack  = hsb 170 35 9
  , skStart = \env -> pure (setFormula (envW env, envH env) firstFormula)
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }
  where env0' = (1280, 800)

setFormula :: (Float, Float) -> [[Int]] -> World
setFormula (width, height) cls =
  let k = length cls
      cx = width / 2
      cy = height * 0.58
      r = min width height * 0.3
      vertsOf (c, cl) =
        let a = tau * fromIntegral c / fromIntegral k - pi / 2
        in [ Vert c lit (cx + cos b * r) (cy + sin b * r)
           | (j, lit) <- zip [0 :: Int ..] cl
           , let b = a + (fromIntegral j - 1) * 0.32 ]
      vs = concatMap vertsOf (zip [0 ..] cls)
      n = length vs
      arr = listArray (0, n - 1) vs
      es = [(i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1], okIn arr i j]
  in World cls arr es (S (replicate k (-1)) 0 Nothing) [] False False

randomFormula :: IO [[Int]]
randomFormula = replicateM 4 $ do
  vs <- shuffle [1, 2, 3, 4]
  forM (sort (take 3 vs)) $ \v -> do
    pos <- chance 0.5
    pure (if pos then v else negate v)

okIn :: Array Int Vert -> Int -> Int -> Bool
okIn arr i j =
  let a = arr ! i
      b = arr ! j
  in vC a /= vC b && vLit a /= negate (vLit b)

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

-- 節 0 から順に 1 つずつ選び、それまでの全員とつながっているかを確かめる
search :: Array Int Vert -> Int -> Int -> Gen S Bool
search arr k depth
  | depth == k = pure True
  | otherwise = try [0, 1, 2]
  where
    try [] = do
      modify (\s -> s { sChosen = setAt depth (-1) (sChosen s) })
      pure False
    try (j : js) = do
      let v = depth * 3 + j
      modify (\s -> s
        { sChosen = take depth (sChosen s) ++ [v] ++ replicate (k - depth - 1) (-1)
        , sTries = sTries s + 1 })
      yield
      chosen <- gets sChosen
      let good = all (\c -> okIn arr c v) (take depth chosen)
      r <- if good then search arr k (depth + 1) else pure False
      if r then pure True else try js

advance :: World -> World
advance w = case wRest w of
  [] -> w { wRunning = False, wHasGen = False }
  (s : r)
    | isJust (sFound s) -> w { wS = s, wRest = [], wRunning = False, wHasGen = False }
    | otherwise -> w { wS = s, wRest = r }

stepW :: Env -> Float -> World -> IO World
stepW env _ w
  | wRunning w && envFrame env `mod` 10 == 0 = pure (advance w)
  | otherwise = pure w

isClique :: World -> Bool
isClique w =
  let c = sChosen (wS w)
  in (-1) `notElem` c
       && and [okIn (wVerts w) a b | (i, a) <- zip [0 :: Int ..] c, (j, b) <- zip [0 ..] c, i < j]

litName :: Int -> String
litName l = (if l > 0 then "x" else "~x") ++ show (abs l)

clauseHue :: Int -> Float
clauseHue c = fromIntegral ((c * 80) `mod` 360)

drawW :: Env -> World -> Picture
drawW env w =
  let chosen = sChosen (wS w)
      verts = wVerts w
      clique = isClique w
      posV i = let v = verts ! i in (vX v, vY v)
      edge (a, b)
        | a `elem` chosen && b `elem` chosen = seg env (hsb 50 90 100) 4 (posV a) (posV b)
        | otherwise = seg env (hsba 0 0 100 0.18) 1.5 (posV a) (posV b)
      -- 選んだのにつながっていない組を赤い破線で
      badPairs =
        [ dashed env (hsb 0 90 100) 3 8 8 (posV i) (posV j)
        | (ca, i) <- zip [0 :: Int ..] chosen, (cb, j) <- zip [0 ..] chosen, ca < cb
        , i >= 0, j >= 0, not (okIn verts i j) ]
      node (i, v) =
        let on = i `elem` chosen
        in Pictures
             [ dot env (hsb (clauseHue (vC v)) (if on then 80 else 45) (if on then 100 else 60)) (vX v, vY v) (if on then 50 else 40)
             , txtC env (vX v) (vY v) 14 (hsb 0 0 10) (litName (vLit v))
             ]
      nClauses = length (wClauses w)
      cx = envW env / 2
      cy = envH env * 0.58
      r0 = min (envW env) (envH env) * 0.3
      clauseLabel c =
        let a = tau * fromIntegral c / fromIntegral nClauses - pi / 2
        in txtC env (cx + cos a * (r0 + 90)) (cy + sin a * (r0 + 48)) 15 (hsb (clauseHue c) 60 100) ("Clause " ++ show (c + 1))
      -- 式：色を節ごとに変えて横に並べる
      formula = go 20 (zip [0 ..] (wClauses w))
        where
          go _ [] = []
          go x ((c, cl) : rest) =
            let s = "(" ++ intercalate " | " (map litName cl) ++ ")" ++ (if null rest then "" else " & ")
            in txt env x 72 16 (hsb (clauseHue c) 60 100) s
                 : go (x + fromIntegral (length s) * 16 * 0.8) rest
      verdict
        | clique =
            let asg = IM.fromList [(abs l, l > 0) | i <- chosen, let l = vLit (verts ! i)]
                s = unwords ["x" ++ show v ++ "=" ++ (if b then "T" else "F") | (v, b) <- IM.toList asg]
            in txt env 20 (envH env - 16) 17 (hsb 120 80 100)
                 ("Clique of size " ++ show nClauses ++ "! Make the chosen literals true: all clauses true. " ++ s)
        | sFound (wS w) == Just False =
            txt env 20 (envH env - 16) 20 (hsb 0 80 100) "No clique -> the formula is unsatisfiable"
        | otherwise =
            txt env 20 (envH env - 16) 15 (hsb 0 0 60) "No edges inside a clause or between x and ~x. So a clique = one consistent true literal per clause."
      nVerts = snd (bounds verts) + 1
  in Pictures
       ( map edge (wEdges w)
      ++ badPairs
      ++ map node (zip [0 ..] (elems verts))
      ++ map clauseLabel [0 .. nClauses - 1]
      ++ formula
      ++ [ txt env 20 34 16 (hsb 0 0 100) "Click one point per clause   Space: auto search   1: first example   R: random (4 clauses)"
         , txt env 20 104 16 (hsb 0 0 75)
             (show nVerts ++ " points, " ++ show (length (wEdges w)) ++ " edges (about formula size squared: polynomial)   tried " ++ show (sTries (wS w)))
         , verdict
         ] )

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let m = mouseP5 env p
        hits = [(i, v) | (i, v) <- zip [0 ..] (elems (wVerts w)), dist2 m (vX v, vY v) < 24]
        toggle ch (i, v) =
          let c = vC v
              old = concat [[x] | (k, x) <- zip [0 ..] ch, k == c]
          in setAt c (if old == [i] then -1 else i) ch
    in if null hits
         then pure w
         else
           let s = wS w
           in pure w
                { wRunning = False, wHasGen = False, wRest = []
                , wS = s { sFound = Nothing, sChosen = foldl toggle (sChosen s) hits } }
  EventKey (SpecialKey KeySpace) Down _ _ ->
    let w1 | wHasGen w = w
           | otherwise =
               let k = length (wClauses w)
                   s0 = S (replicate k (-1)) 0 Nothing
                   program = do
                     r <- search (wVerts w) k 0
                     modify (\s -> s { sFound = Just r })
                     yield
               in w { wS = s0, wRest = steps program s0, wHasGen = True }
    in pure w1 { wRunning = not (wRunning w1) }
  EventKey (Char '1') Down _ _ -> pure (setFormula (envW env, envH env) firstFormula)
  EventKey (Char c) Down _ _
    | c `elem` "rR" -> setFormula (envW env, envH env) <$> randomFormula
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 手でクリークを見つけ、下に出る割り当てで式が本当に真になるか確かめよう
-- 2. 逆向き：式を真にする割り当てを先に考え、各節で真の文字を選ぶとクリークになる？
-- 3. 節の数を 5 にすると点と線はいくつ？（firstFormula を自分で書きかえる）

-- ---------- 道具：p5.js と同じ座標（左上が原点、y は下向き）で描く ----------
gx, gy :: Env -> Float -> Float
gx env x = x - envW env / 2
gy env y = envH env / 2 - y

pt :: Env -> Point -> Point
pt env (x, y) = (gx env x, gy env y)

-- gloss のマウス座標を p5.js の座標に直す
mouseP5 :: Env -> Point -> Point
mouseP5 env (x, y) = (x + envW env / 2, envH env / 2 - y)

-- 太さ t の線分（p5.js の line）
seg :: Env -> Color -> Float -> Point -> Point -> Picture
seg env c t a b = Color c (thickLine t (pt env a) (pt env b))

-- 破線（線の長さ on、すき間 off）
dashed :: Env -> Color -> Float -> Float -> Float -> Point -> Point -> Picture
dashed env c t on off a@(x1, y1) b@(x2, y2) =
  let l = dist2 a b
      p d = (x1 + (x2 - x1) * d / l, y1 + (y2 - y1) * d / l)
      starts = takeWhile (< l) (iterate (+ (on + off)) 0)
  in if l < 1 then Blank else Pictures [seg env c t (p d) (p (min l (d + on))) | d <- starts]

-- 直径 d の円（p5.js の circle）
dot :: Env -> Color -> Point -> Float -> Picture
dot env c p d = Color c (at (pt env p) (disc (d / 2)))

-- 文字：左端の下が (x, y)
txt :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txt env x y s c str = label (gx env x) (gy env y) s c str

-- 文字：中心が (x, y)
txtC :: Env -> Float -> Float -> Float -> Color -> String -> Picture
txtC env x y s c str = labelC (gx env x) (gy env y) s c str

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

gets :: (s -> a) -> Gen s a
gets f = Gen (\s k -> k (f s) s)

modify :: (s -> s) -> Gen s ()
modify f = Gen (\s k -> k () $! f s)

steps :: Gen s () -> s -> [s]
steps (Gen g) s0 = g s0 (\_ _ -> [])
