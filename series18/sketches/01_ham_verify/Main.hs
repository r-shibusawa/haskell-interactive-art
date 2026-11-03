-- ============================================
-- 01 答えを「確かめる」のはかんたん
-- ハミルトン閉路：すべての点をちょうど 1 回ずつ通って戻る道。見つけるのは大変、でも答え（証拠）を見せられたら確かめるのは一瞬
-- 「証拠をもらえば多項式時間で確かめられる」問題の集まりが NP
-- 点を順にクリックして証拠をつくる → V で検証 / スペースで総当たり探索（バックトラック）開始・停止 / C：証拠を消す
-- 1：十二面体（ハミルトンのゲーム）  2：ピーターセン・グラフ（閉路なし）  3：ランダム
-- ============================================
module Main (main) where

import Art
import Control.Monad (filterM, forM)
import qualified Data.IntMap.Strict as IM
import Data.List (elemIndex, find)
import Data.Maybe (isJust)
import Numeric (showFFloat)

searchSpeed :: Int
searchSpeed = 3     -- ★ 1 フレームに進める探索ステップ数

-- 検証の 1 ステップ
data Check = Check
  { cTxt  :: String
  , cOk   :: !Bool
  , cHl   :: [Int]               -- 光らせる点
  , cEdge :: Maybe (Int, Int)    -- 光らせる線
  }

-- 探索の状態（sPath と sIdx は新しいものが先頭）
data Search = Search
  { sPath  :: [Int]
  , sIdx   :: [Int]
  , sSteps :: !Int
  , sDone  :: !Bool
  , sFound :: !Bool
  }

data World = World
  { wPos       :: !(IM.IntMap Point)
  , wEdges     :: [(Int, Int)]
  , wNbrs      :: !(IM.IntMap [Int])
  , wCert      :: [Int]          -- 証拠（点の並び）
  , wChecks    :: [Check]
  , wCheckPos  :: !Int
  , wVerdict   :: String
  , wSearch    :: Maybe Search
  , wSearching :: !Bool
  }

emptyWorld :: World
emptyWorld = World IM.empty [] IM.empty [] [] (-1) "" Nothing False

main :: IO ()
main = runSketch (sketch "18-01 Verifying Is Easy" emptyWorld)
  { skBack  = hsb 250 30 10
  , skStart = \env -> preset env 1
  , skStep  = stepW
  , skDraw  = drawW
  , skEvent = onEvent
  }

preset :: Env -> Int -> IO World
preset env k = do
  let cx = envW env / 2
      cy = envH env * 0.56
      r = min (envW env) (envH env) * 0.36
      pOn rad i m off =
        let a = tau * fromIntegral (i :: Int) / fromIntegral (m :: Int) - pi / 2 + off
        in (cx + cos a * rad, cy + sin a * rad)
  (ps, es) <- case k of
    1 -> pure
      ( [pOn r i 5 0 | i <- [0 .. 4]]
        ++ [pOn (r * 0.66) j 10 0 | j <- [0 .. 9]]
        ++ [pOn (r * 0.33) i 5 (pi / 5) | i <- [0 .. 4]]
      , concat [[(i, (i + 1) `mod` 5), (i, 5 + 2 * i), (5 + 2 * i + 1, 15 + i), (15 + i, 15 + (i + 1) `mod` 5)] | i <- [0 .. 4]]
        ++ [(5 + j, 5 + (j + 1) `mod` 10) | j <- [0 .. 9]]
      )
    2 -> pure
      ( [pOn r i 5 0 | i <- [0 .. 4]] ++ [pOn (r * 0.48) i 5 0 | i <- [0 .. 4]]
      , concat [[(i, (i + 1) `mod` 5), (i, i + 5), (5 + i, 5 + (i + 2) `mod` 5)] | i <- [0 .. 4]]
      )
    _ -> do
      let m = 10
      qs <- forM [0 .. m - 1] $ \i -> do
        f <- randomRIO (0.55, 1)
        pure (pOn (r * f) i m 0)
      rs <- filterM (const (chance 0.3)) [(i, j) | i <- [0 .. m - 1], j <- [i + 1 .. m - 1]]
      pure (qs, rs)
  let nb = foldl' (\mp (a, b) -> IM.insertWith (flip (++)) b [a] (IM.insertWith (flip (++)) a [b] mp))
             (IM.fromList [(i, []) | i <- [0 .. length ps - 1]]) es
  pure emptyWorld { wPos = IM.fromList (zip [0 ..] ps), wEdges = es, wNbrs = nb }

nbrsOf :: World -> Int -> [Int]
nbrsOf w v = IM.findWithDefault [] v (wNbrs w)

adj :: World -> Int -> Int -> Bool
adj w a b = b `elem` nbrsOf w a

posOf :: World -> Int -> Point
posOf w i = IM.findWithDefault (0, 0) i (wPos w)

clearChecks :: World -> World
clearChecks w = w { wChecks = [], wCheckPos = -1, wVerdict = "" }

-- ---- 検証：証拠が本当にハミルトン閉路かを順番に確かめる（n 回くらいのチェックで終わる）----
buildChecks :: World -> World
buildChecks w =
  let n = IM.size (wPos w)
      cert = wCert w
      len = length cert
      next = drop 1 cert ++ take 1 cert
      go _ [] = []
      go seen ((a, b) : r) =
        Check ("point " ++ show a ++ " is new?") (a `notElem` seen) [a] Nothing
          : Check ("edge " ++ show a ++ " -> " ++ show b ++ " exists?") (adj w a b) [a, b] (Just (a, b))
          : go (a : seen) r
      first = Check ("count is " ++ show n ++ "? (now " ++ show len ++ ")") (len == n) [] Nothing
  in w { wChecks = first : go [] (zip cert next), wCheckPos = 0, wVerdict = "" }

checkTick :: World -> World
checkTick w =
  let pos = wCheckPos w
      total = length (wChecks w)
  in case currentCheck w of
       Nothing -> w
       Just c
         | not (cOk c) -> w { wVerdict = "NG: not a certificate", wCheckPos = total }
         | pos + 1 == total -> w { wVerdict = "OK: a Hamiltonian cycle!", wCheckPos = pos + 1 }
         | otherwise -> w { wCheckPos = pos + 1 }

currentCheck :: World -> Maybe Check
currentCheck w
  | wCheckPos w < 0 = Nothing
  | otherwise = case drop (wCheckPos w) (wChecks w) of
      (c : _) -> Just c
      [] -> Nothing

-- ---- 探索：バックトラックで閉路を探す（最悪 n! 通り）----
newSearch :: Search
newSearch = Search [0] [0] 0 False False

searchTick :: World -> World
searchTick w = case wSearch w of
  Just s0 | not (sDone s0), (v : _) <- sPath s0, (i0 : idxRest) <- sIdx s0 ->
    let s = s0 { sSteps = sSteps s0 + 1 }
        path = sPath s
        (skipped, after) = span (`elem` path) (drop i0 (nbrsOf w v))
    in if length path == IM.size (wPos w) && adj w v (last path)
         then w { wSearch = Just s { sDone = True, sFound = True }, wCert = reverse path }
         else case after of
           (u : _) ->
             w { wSearch = Just s { sIdx = 0 : (i0 + length skipped + 1) : idxRest, sPath = u : path } }
           [] ->
             let p' = drop 1 path
             in w { wSearch = Just s { sPath = p', sIdx = idxRest, sDone = null p' } }
  _ -> w

stepW :: Env -> Float -> World -> IO World
stepW env _ w0 =
  let w1 = if wSearching w0 && isJust (wSearch w0)
             then iterate searchTick w0 !! searchSpeed
             else w0
      w2 = if envFrame env `mod` 8 == 0 then checkTick w1 else w1
  in pure w2

drawW :: Env -> World -> Picture
drawW env w =
  let searching = wSearching w
      failed = case wSearch w of
        Just s -> not (sFound s) && sDone s
        Nothing -> False
      path
        | searching || failed = maybe [] (reverse . sPath) (wSearch w)
        | otherwise = wCert w
      cert = wCert w
      n = IM.size (wPos w)
      cur = currentCheck w
      pathHue = if searching then 30 else 160
      edgeLine (a, b) = seg env (hsb 0 0 35) 2 (posOf w a) (posOf w b)
      pathLine (a, b) = seg env (hsb pathHue 80 100) 6 (posOf w a) (posOf w b)
      closing = case cert of
        (c0 : _ : _)
          | not searching && length cert == n && adj w (last cert) c0 ->
              seg env (hsb 160 80 100) 6 (posOf w (last cert)) (posOf w c0)
        _ -> Blank
      curEdge = case cur of
        Just c | Just (a, b) <- cEdge c ->
          seg env (hsb (if cOk c then 120 else 0) 90 100) 10 (posOf w a) (posOf w b)
        _ -> Blank
      node i =
        let order = elemIndex i path
            base = case order of
              Just _ -> hsb pathHue 70 100
              Nothing -> hsb 220 20 60
            col = case cur of
              Just c | i `elem` cHl c -> hsb (if cOk c then 120 else 0) 90 100
              _ -> base
            p@(x, y) = posOf w i
            s = maybe (show i) (\o -> show (o + 1)) order
        in Pictures [dot env col p 34, txtC env x y 14 (hsb 0 0 10) s]
      verdict = wVerdict w
      isOk = take 1 verdict == "O"
      verdictLine
        | not (null verdict) =
            txt env 20 104 20 (hsb (if isOk then 120 else 0) 70 100)
              (verdict ++ " (" ++ show (min (wCheckPos w + (if isOk then 0 else 1)) (length (wChecks w))) ++ " checks)")
        | Just c <- cur = txt env 20 104 20 (hsb 50 70 100) ("Checking: " ++ cTxt c)
        | otherwise = Blank
      searchLine = case wSearch w of
        Just s ->
          let st | not (sDone s) = "searching..."
                 | sFound s = "found!"
                 | otherwise = "no cycle (all tried)"
          in txt env 20 136 20 (hsb 30 70 100) ("Brute force: " ++ st ++ "  steps " ++ commas (sSteps s))
        Nothing -> Blank
  in Pictures
       ( map edgeLine (wEdges w)
      ++ map pathLine (zip path (drop 1 path))
      ++ [closing, curEdge]
      ++ map node [0 .. n - 1]
      ++ [ txt env 20 36 15 (hsb 0 0 100) "Click points in order, V: verify   Space: search   C: clear   1 dodecahedron  2 Petersen  3 random"
         , txt env 20 64 16 (hsb 0 0 70)
             (show n ++ " points, " ++ show (length (wEdges w)) ++ " edges. At most " ++ show n ++ "! = " ++ fmt (factD n) ++ " orders")
         , verdictLine
         , searchLine
         , txt env 20 (envH env - 24) 15 (hsb 0 0 60) "Verifying takes about 2 x points checks: polynomial time. For searching, no fast way is known."
         ] )

-- 大きな階乗は Float に入りきらないので Double で
factD :: Int -> Double
factD k = product (map fromIntegral [2 .. k])

fmt :: Double -> String
fmt v
  | v < 1e6 = commas (round v)
  | otherwise =
      let e = floor (logBase 10 v + 1e-12) :: Int
      in fix 1 (v / 10 ^^ e) ++ "x10^" ++ show e

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (MouseButton LeftButton) Down _ p ->
    let m = mouseP5 env p
    in case find (\i -> dist2 m (posOf w i) < 20) (IM.keys (wPos w)) of
         Nothing -> pure w
         Just i ->
           let w1 = (clearChecks w) { wSearching = False, wSearch = Nothing }
               cert = wCert w1
           in pure w1 { wCert = if take 1 (reverse cert) == [i]   -- 同じ点をもう一度押すと 1 つ戻る
                                  then take (length cert - 1) cert
                                  else cert ++ [i] }
  EventKey (Char c) Down _ _
    | c `elem` "vV" -> pure (buildChecks w)
    | c `elem` "cC" -> pure (clearChecks w) { wCert = [], wSearch = Nothing, wSearching = False }
    | c `elem` "123" -> preset env (fromEnum c - fromEnum '0')
  EventKey (SpecialKey KeySpace) Down _ _ ->
    let finished = maybe True sDone (wSearch w)
        w1 = if finished
               then (clearChecks w) { wSearch = Just newSearch, wCert = [] }
               else w
    in pure w1 { wSearching = not (wSearching w1) }
  _ -> pure w

-- ---------- やってみよう ----------
-- 1. 十二面体で自分の手でハミルトン閉路を見つけよう（ハミルトンが 1857 年に売り出したパズル）
-- 2. わざと 1 点重複した証拠をつくって V。どこで NG になる？
-- 3. ピーターセン・グラフで総当たり。全部調べて「ない」と言い切るのに何手かかる？

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
-- 確率 p で True
chance :: Float -> IO Bool
chance p = (< p) <$> randomRIO (0, 1)

-- 小数点以下 d 桁で表示（p5.js の nf）
fix :: RealFloat a => Int -> a -> String
fix d v = showFFloat (Just d) v ""

-- 3 桁ごとにカンマを入れる（toLocaleString）
commas :: Int -> String
commas v
  | v < 0 = '-' : commas (negate v)
  | v < 1000 = show v
  | otherwise = commas (v `div` 1000) ++ "," ++ drop 1 (show (1000 + v `mod` 1000))
