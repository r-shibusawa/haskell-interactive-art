-- ============================================
-- 69-12 様相論理とクリプキモデル ─ 「必ず []」「かもしれない <>」を、可能世界（点）と到達関係（矢印）で考える
-- []p が世界 w で真 ⇔ w から行ける全部の世界で p が真。<>p が真 ⇔ 行ける世界のどこかで p が真（行き先がなければ []p はいつも真）。
-- 公理 T（[]p -> p）は「どの世界も自分に行ける（反射的）」フレームなら、p をどう決めても必ず真。4（[]p -> [][]p）は推移的なフレームで必ず真。
-- 操作：空いた所をクリック = 世界を足す（最初の 1 回は p5.js 版と同じく何もしない）/ 世界から世界へドラッグ = 矢印の切りかえ（同じ世界へ戻ると自分への矢印）
--       世界をクリック = 選ぶ → P / Q でその世界の p / q を切りかえ / Backspace 最後の世界を消す / ↑↓ 式 / 1〜4 お手本のフレーム
-- ============================================
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Art
import Data.Bits (shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.List (findIndex, foldl', sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.IO (hSetEncoding, stdout, utf8)

formulas :: [(String, String)]                -- ★ (式, 名前)（□ ◇ でも [] <> でも書ける。画面では [] <>）
formulas =
  [ ("□p", "necessarily p"), ("◇p", "possibly p"), ("□p → p", "T"), ("□p → □□p", "4"), ("p → □◇p", "B"), ("□p → ◇p", "D"), ("◇p → □◇p", "5")
  , ("□(p → q) → (□p → □q)", "K (every frame)"), ("◇p ↔ ¬□¬p", "<> vs []") ]

-- ★ 公理と、それが必ず成り立つフレームの性質（フレーム = 世界の数 n と矢印の表 r）
axioms :: [(String, String, String, Int -> Frame -> Bool)]
axioms =
  [ ("T", "□p → p", "reflexive", \n r -> allI n (\x -> rel r n x x))
  , ("4", "□p → □□p", "transitive", \n r -> allI n (\x -> allI n (\y -> allI n (\z -> not (rel r n x y && rel r n y z) || rel r n x z))))
  , ("B", "p → □◇p", "symmetric", \n r -> allI n (\x -> allI n (\y -> not (rel r n x y) || rel r n y x)))
  , ("D", "□p → ◇p", "serial", \n r -> allI n (\x -> anyI n (\y -> rel r n x y)))
  , ("5", "◇p → □◇p", "Euclidean", \n r -> allI n (\x -> allI n (\y -> allI n (\z -> not (rel r n x y && rel r n x z) || rel r n y z)))) ]

maxW :: Int
maxW = 6                                      -- ★ 世界の数の上限

wr :: Float
wr = 26                                       -- ★ 世界の丸の半径

-- フレームの矢印：ビット i*n + j が 1 なら i → j
type Frame = Int

rel :: Frame -> Int -> Int -> Int -> Bool
rel r n i j = testBit r (i * n + j)

allI, anyI :: Int -> (Int -> Bool) -> Bool
allI n f = all f [0 .. n - 1]
anyI n f = any f [0 .. n - 1]

-- 付値：変数 → 世界のビット（ビット w が 1 なら世界 w でその変数が真）
type Val = M.Map String Int

data Wld = Wld { wx :: !Float, wy :: !Float, wp :: !Bool, wq :: !Bool }

-- 計算しておく結果（変わったときだけ作り直す）
data Cache = Cache
  { cTruth :: [Bool]                          -- 選んだ式の、世界ごとの真偽
  , cAllT  :: [Bool]                          -- 式ごとに「全部の世界で真」か
  , cAx    :: [(Bool, Bool)] }                -- 公理ごとに (どの p でも真, フレームの性質)

data World = World
  { wWorlds :: [Wld]
  , wRel    :: [(Int, Int)]                   -- 矢印
  , wF      :: !Int                           -- 選んだ式
  , wSel    :: !Int                           -- 選んだ世界（-1 = なし）
  , wFocus  :: !Bool                          -- 最初のクリックがすんだか
  , wDrag   :: !Int                           -- ドラッグを始めた世界（-1 = なし）
  , wMoved  :: !Bool
  , wCache  :: Cache
  , wTest   :: [(String, Bool)] }

main :: IO ()
main = runSketch (sketch "69-12 Kripke Models" (start envT []))
  { skBack  = col bgc
  , skStart = \env -> do
      printSelfTest selfTest
      pure (start env selfTest)
  , skDraw  = draw
  , skEvent = event }

envT :: Env
envT = Env 1280 800 (0, 0) False 0 0 S.empty

start :: Env -> [(String, Bool)] -> World
start env t = example env 1 (World [] [] 2 0 False (-1) False (Cache [] [] []) t)

parsedF, axF :: [F]
parsedF = [ either (const FBot) id (parseFormulaM True s) | (s, _) <- formulas ]
axF = [ either (const FBot) id (parseFormulaM True s) | (_, s, _, _) <- axioms ]

-- ★ 様相の式の真偽：世界 w で、世界の数 n、到達関係 r、付値 v
evalModal :: F -> Int -> Int -> Frame -> Val -> Bool
evalModal f w n r v = case f of
  FVar nm -> testBit (M.findWithDefault 0 nm v) w
  FTop -> True
  FBot -> False
  FNot a -> not (ev a w)
  FBin OAnd a b -> ev a w && ev b w
  FBin OOr a b -> ev a w || ev b w
  FBin OImp a b -> not (ev a w) || ev b w
  FBin OIff a b -> ev a w == ev b w
  FBox a -> allI n (\u -> not (rel r n w u) || ev a u)
  FDia a -> anyI n (\u -> rel r n w u && ev a u)
  where ev g u = evalModal g u n r v

-- フレームで妥当：p, q をどう決めても、どの世界でも真
validOnFrame :: F -> Int -> Frame -> Bool
validOnFrame f n r =
  let vars = fmlVars f
      full = (1 `shiftL` n) - 1
      valOf k = M.fromList [ (v, (k `shiftR` (j * n)) .&. full) | (j, v) <- zip [0 ..] vars ]
  in and [ allI n (\w -> evalModal f w n r v) | k <- [0 .. (1 `shiftL` (n * length vars)) - 1 :: Int], let v = valOf k ]

frameOf :: World -> (Int, Frame, Val)
frameOf w =
  let ws = wWorlds w
      n = length ws
      r = foldl' (\m (i, j) -> if i < n && j < n then m .|. (1 `shiftL` (i * n + j)) else m) 0 (wRel w)
      bits f = foldl' (.|.) 0 [ 1 `shiftL` i | (i, x) <- zip [0 ..] ws, f x ]
  in (n, r, M.fromList [("p", bits wp), ("q", bits wq)])

refresh :: World -> World
refresh w =
  let (n, r, v) = frameOf w
      f = at' parsedF (wF w) FBot
  in w { wCache = Cache
           { cTruth = [ evalModal f i n r v | i <- [0 .. n - 1] ]
           , cAllT = [ n > 0 && allI n (\i -> evalModal g i n r v) | g <- parsedF ]
           , cAx = [ (n == 0 || validOnFrame a n r, test n r) | (a, (_, _, _, test)) <- zip axF axioms ] } }

-- お手本のフレーム
example :: Env -> Int -> World -> World
example env k w0 =
  let cx = envW env * 0.3
      cy = envH env * 0.52
      rr = min (envW env) (envH env) * 0.2
      ringW n = [ Wld (cx + rr * cos (-pi / 2 + i2f i * 2 * pi / i2f n)) (cy + rr * sin (-pi / 2 + i2f i * 2 * pi / i2f n)) False False | i <- [0 .. n - 1] ]
      setP is ws = [ if i `elem` is then x { wp = True } else x | (i, x) <- zip [0 ..] ws ]
      setQ is ws = [ if i `elem` is then x { wq = True } else x | (i, x) <- zip [0 ..] ws ]
      (ws', rel') = case k of
        1 -> (setP [1, 2] (ringW 3), [(0, 1), (1, 2), (0, 2)])                          -- 推移的だが反射的でない
        2 -> (setP [0, 1] (ringW 3), [(0, 0), (1, 1), (2, 2), (0, 1), (1, 2)])          -- 反射的だが推移的でない
        3 -> (setP [0] (ringW 3), [ (i, j) | i <- [0 .. 2], j <- [0 .. 2] ])            -- 全部つながる（S5）
        _ -> (setQ [1] (setP [0, 2] (ringW 4)), [(0, 1), (1, 2), (2, 3), (3, 0)])
  in refresh w0 { wWorlds = ws', wRel = rel', wSel = 0 }

draw :: Env -> World -> Picture
draw env w = view env ([hud] ++ arrows ++ dragLine ++ balls ++ notes ++ list ++ axPics ++ [drawSelfTest env (envW env - 180) (envH env - 14) (wTest w)])
  where
    wd = envW env
    ht = envH env
    ws = wWorlds w
    c = wCache w
    n = length ws
    hud = hudTitle "Kripke models: [] = every reachable world, <> = some reachable world"
            "Click: add / select a world   Drag world to world: toggle arrow   P Q: flip p q of the selected world\nUp/Down formula   1-4 example frames   Backspace: delete the last world"
    -- ---- 矢印と世界
    arrows = [ arrowW ws (wRel w) i j linec | (i, j) <- wRel w ]
    dragLine = case (wDrag w >= 0 && wMoved w, at' ws (wDrag w) (Wld 0 0 False False)) of
      (True, a) -> [Color (col sky) (lineW 2 (wx a, wy a) (mouseP env))]
      _ -> []
    balls = concat
      [ [ Color (col (if v then (70, 110, 70) else (120, 60, 66))) (circleAt (wx a) (wy a) (wr * 2))
        , Color (col (if i == wSel w then yel else ink)) (ringAt (wx a) (wy a) (wr * 2) (if i == wSel w then 4 else 2))
        , sayM ("w" ++ show i) (wx a) (wy a - 6) 16 (col ink)
        , sayM ((if wp a then "p" else "~p") ++ " " ++ (if wq a then "q" else "~q")) (wx a) (wy a + 11) 14 (col ink) ]
      | (i, a, v) <- zip3 [0 :: Int ..] ws (cTruth c ++ repeat False) ]
    notes =
      [ say ("World color: green = \"" ++ fmlStr (at' parsedF (wF w) FBot) ++ "\" is true, red = false (yellow ring = selected)") 30 (ht - 70) 14 (col dimc)
      , say "Many-valued and fuzzy logic (truth between 0 and 1): series 50" 30 (ht - 46) 14 (col dimc) ]
    -- ---- 右：式の一覧（全部の世界で真か）
    rx = wd * 0.56
    list = concat
      [ [ if k == wF w then Color (col linec) (rectTL (rx - 8) (y - 17) (wd - rx - 14) 23) else Blank
        , if allT then say "all true" rx y 14 (col grn) else Blank
        , say (fmlStr g) (rx + 78) y 14 (col ink)
        , say nm (rx + 340) y 14 (col dimc) ]
      | (k, g, (_, nm), allT) <- zip4' [0 :: Int ..] parsedF formulas (cAllT c ++ repeat False), let y = 104 + i2f k * 25 ]
    -- ---- 右下：公理とフレームの性質の対応（p の決め方を全部ためす）
    y0 = 104 + i2f (length formulas) * 25 + 26
    axPics =
      say "Valid on this frame (for every p)?  <=>  frame property" rx y0 14 (col yel)
      : concat [ [ say (nm ++ "  " ++ fmlStr a) rx y 14 (col ink)
                 , say (if vf then "valid" else "not valid") (rx + 175) y 14 (col (if vf then grn else pink))
                 , say (prop ++ (if pr then " OK" else " NO")) (rx + 280) y 14 (col (if pr then grn else pink))
                 , say (if vf == pr then "match" else "MISMATCH!") (rx + 450) y 14 (col (if vf == pr then dimc else pink)) ]
               | (k, a, (nm, _, prop, _), (vf, pr)) <- zip4' [0 :: Int ..] axF axioms (cAx c), let y = y0 + 28 + i2f k * 24 ]
      ++ [ say ("(" ++ show n ++ " worlds: 2^" ++ show n ++ " ways to choose p, all of them tried)") rx (y0 + 28 + i2f (length axioms) * 24) 14 (col dimc) ]

zip4' :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
zip4' _ _ _ _ = []

-- 矢印 i → j（自分への矢印は上の小さな輪。逆向きもあれば少しずらす）
arrowW :: [Wld] -> [(Int, Int)] -> Int -> Int -> RGB -> Picture
arrowW ws rels i j c =
  let z = Wld 0 0 False False
      a = at' ws i z
      b = at' ws j z
      dx = wx b - wx a
      dy = wy b - wy a
      l0 = hyp dx dy
      l = if l0 == 0 then 1 else l0
      ux = dx / l
      uy = dy / l
      off = if (j, i) `elem` rels then 6 else 0
      x0 = wx a + ux * wr - uy * off
      y0 = wy a + uy * wr + ux * off
      x1 = wx b - ux * (wr + 3) - uy * off
      y1 = wy b - uy * (wr + 3) + ux * off
  in Color (col c) $
       if i == j then ringAt (wx a) (wy a - wr - 12) 26 2
       else Pictures [ lineW 2 (x0, y0) (x1, y1)
                     , Polygon [(x1, y1), (x1 - ux * 11 - uy * 5, y1 - uy * 11 + ux * 5), (x1 - ux * 11 + uy * 5, y1 - uy * 11 - ux * 5)] ]

hitW :: [Wld] -> (Float, Float) -> Int
hitW ws (x, y) = headOr (-1) [ i | (i, a) <- reverse (zip [0 ..] ws), hyp (wx a - x) (wy a - y) < wr ]

event :: Env -> Event -> World -> IO World
event env ev w = pure $ case ev of
  EventKey (MouseButton LeftButton) Down _ pos ->
    let m@(mx, my) = toP5 env pos
        hit = hitW (wWorlds w) m
    in if not (wFocus w) then w { wFocus = True }
       else if mx > envW env * 0.54 then w
       else if hit < 0 && length (wWorlds w) < maxW && my > 80
         then refresh w { wDrag = -1, wMoved = False, wWorlds = wWorlds w ++ [Wld mx my False False], wSel = length (wWorlds w) }
         else w { wDrag = hit, wMoved = False }
  EventMotion pos | envDown env && wDrag w >= 0 ->
    let (mx, my) = toP5 env pos
        a = at' (wWorlds w) (wDrag w) (Wld 0 0 False False)
    in if hyp (mx - wx a) (my - wy a) > wr then w { wMoved = True } else w
  EventKey (MouseButton LeftButton) Up _ pos | wDrag w >= 0 ->
    let m@(mx, my) = toP5 env pos
        from = wDrag w
        to = hitW (wWorlds w) m
        w1 = w { wDrag = -1 }
    in if not (wMoved w) && to == from then w1 { wSel = to }
       else if to >= 0 then refresh w1 { wRel = case findIndex (== (from, to)) (wRel w) of
                                                  Just k -> removeAt k (wRel w)
                                                  Nothing -> wRel w ++ [(from, to)] }
       else if wMoved w then w1 { wWorlds = modifyAt from (\a -> a { wx = clampF 30 (envW env * 0.53) mx, wy = clampF 90 (envH env - 90) my }) (wWorlds w) }
       else w1
  EventKey k Down mods _ -> case typedKey k mods of
    TUp -> refresh w { wF = wrapI (wF w - 1) (length formulas) }
    TDown -> refresh w { wF = wrapI (wF w + 1) (length formulas) }
    TChar ch
      | ch >= '1' && ch <= '4' -> example env (fromEnum ch - fromEnum '0') w
      | isKey 'p' ch && sel -> refresh w { wWorlds = modifyAt (wSel w) (\a -> a { wp = not (wp a) }) (wWorlds w) }
      | isKey 'q' ch && sel -> refresh w { wWorlds = modifyAt (wSel w) (\a -> a { wq = not (wq a) }) (wWorlds w) }
    t | (t == TBack || t == TClear) && not (null (wWorlds w)) ->
      let kk = length (wWorlds w) - 1
      in refresh w { wWorlds = take kk (wWorlds w), wRel = filter (\(i, j) -> i /= kk && j /= kk) (wRel w), wSel = min (wSel w) (kk - 1) }
    _ -> w
  _ -> w
  where sel = wSel w >= 0 && wSel w < length (wWorlds w)

-- ---------- やってみよう ----------
-- 1. お手本 1（推移的、反射的でない）で T を選ぶ。どの世界が赤？ w0 に自分への矢印を足すと？
-- 2. お手本 2（反射的、推移的でない）で 4 が偽になる p の決め方を見つけよう（右下の表は「ある」と言っている）
-- 3. お手本 3（全部つながる）では T・4・B・D・5 が全部「必ず真」。これが知識の論理 S5（「知っている」の論理）
-- 4. [] を「いつも（これから先ずっと）」、<> を「いつか」と読むと時間の論理。矢印 = 時間が進む向き。どんなフレームが合う？
-- 5. formulas に足そう：[][]p -> []p、<><>p -> <>p。どんなフレームの性質と対応する？

-- ---- 自己テスト（p5.js 版と同じ項目。名前も同じ）
selfTest :: [(String, Bool)]
selfTest =
  [ ("式 " ++ show (length formulas) ++ " 個と公理 5 つが全部読める（□ ◇ も [] <> も）"
    , all ok [ s | (s, _) <- formulas ] && all ok [ s | (_, s, _, _) <- axioms ]
      && either (const False) (== at' axF 3 FTop) (parseFormulaM True "[]p -> <>p")) ]
  ++ [ ( nm ++ "（" ++ s ++ "）：世界 3 個までの全部のフレーム " ++ show (length frames) ++ " 個で「必ず真」⇔ " ++ propJa nm ++ "（成り立たないフレーム " ++ show fails ++ " 個）"
       , agree == length frames && fails > 0 )
     | (a, (nm, s, _, test)) <- zip axF axioms
     , let vs = [ (validOnFrame a n r, test n r) | (n, r) <- frames ]
     , let agree = length (filter (uncurry (==)) vs)
     , let fails = length (filter (not . fst) vs) ]
  ++ [ ("K と「◇p ↔ ¬□¬p」は、どのフレームでも p, q をどう決めても真（世界 2 個まで全部）"
       , and [ validOnFrame (at' parsedF 7 FBot) n r && validOnFrame (at' parsedF 8 FBot) n r | (n, r) <- frames, n <= 2 ])
     , ("行き先のない世界では □⊥ が真（全部の行き先で… の「全部」が空）、◇⊤ は偽"
       , evalModal (pf "[]0") 0 1 0 M.empty && not (evalModal (pf "<>1") 0 1 0 M.empty)) ]
  where
    ok s = either (const False) (const True) (parseFormulaM True s)
    pf s = either (const FBot) id (parseFormulaM True s)
    -- 世界 1〜3 個のフレームを全部（2 + 16 + 512 = 530 個）
    frames = [ (n, m) | n <- [1 .. 3], m <- [0 .. (1 `shiftL` (n * n)) - 1] ]
    propJa nm = case nm of
      "T" -> "反射的"
      "4" -> "推移的"
      "B" -> "対称的"
      "D" -> "行き止まりがない"
      _ -> "ユークリッド的"

-- ==================================================
-- ここから下は「論理の道具」（中身は読まなくても OK。消さないでね）
-- 座標は p5.js 版と同じ（左上が原点、y は下向き）。view で gloss の座標に直して描く
-- 画面の記号は ASCII：~ = ¬、& = ∧、| = ∨、-> = →、<-> = ↔、T = ⊤、_|_ = ⊥、[] = □、<> = ◇、\ = λ
-- 乱数は p5.js 版と同じ mulberry32 の 1 本の列。R の計算の中で rnd を呼んだ順に引く
-- ==================================================
-- ---- 色と小さな道具（黒板とチョーク）
type RGB = (Float, Float, Float)

bgc, ink, yel, sky, pink, grn, lilac, dimc, panelc, linec :: RGB
bgc = (40, 54, 47)                            -- 背景（黒板の灰緑）
ink = (238, 238, 228)                         -- 文字（白いチョーク）
yel = (246, 218, 120)
sky = (128, 186, 222)
pink = (238, 146, 150)
grn = (164, 214, 146)
lilac = (196, 170, 230)
dimc = (150, 166, 156)
panelc = (50, 66, 58)
linec = (92, 112, 102)

col :: RGB -> Color
col (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) 255

colA :: Float -> RGB -> Color                 -- 透明度 0〜255
colA a (r, g, b) = makeColorI (c8 r) (c8 g) (c8 b) (c8 a)

c8 :: Float -> Int
c8 v = if isNaN v then 0 else max 0 (min 255 (round v))

-- clampv(v, a, b)：はさむ（NaN なら a に戻す）
clampv :: Double -> Double -> Double -> Double
clampv v a b = if v < a then a else if v > b then b else if isNaN v then a else v

clampI :: Int -> Int -> Int -> Int
clampI v a b = max a (min b v)

i2f :: Int -> Float
i2f = fromIntegral

i2d :: Int -> Double
i2d = fromIntegral

f2 :: Double -> Float                         -- 描くときの Float（NaN は 0）
f2 v = if isNaN v || isInfinite v then 0 else realToFrac v

-- リストの i 番目（なければ d）
at' :: [a] -> Int -> a -> a
at' xs i d = if i < 0 then d else case drop i xs of
  (x : _) -> x
  [] -> d

headOr :: a -> [a] -> a
headOr d xs = case xs of
  (x : _) -> x
  [] -> d

lastOr :: a -> [a] -> a
lastOr d xs = if null xs then d else last xs

maxOr :: Ord a => a -> [a] -> a
maxOr d xs = if null xs then d else maximum xs

minOr :: Ord a => a -> [a] -> a
minOr d xs = if null xs then d else minimum xs

-- リストの i 番目を f で変える
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = [if k == i then f x else x | (k, x) <- zip [0 ..] xs]

setAt :: Int -> a -> [a] -> [a]
setAt i v = modifyAt i (const v)

-- i 番目を取りのぞく（splice(i, 1)）
removeAt :: Int -> [a] -> [a]
removeAt i xs = [x | (k, x) <- zip [0 ..] xs, k /= i]

-- (k + n) % n（負の数でも 0〜n−1）
wrapI :: Int -> Int -> Int
wrapI k n = if n <= 0 then 0 else k `mod` n

hyp :: Float -> Float -> Float                -- 長さ √(a² + b²)（Math.hypot）
hyp a b = sqrt (a * a + b * b)

nubOrd :: Ord a => [a] -> [a]                 -- 重なりを除く（最初の順を保つ。[...new Set(xs)]）
nubOrd = go S.empty
  where go _ [] = []
        go seen (x : xs) = if S.member x seen then go seen xs else x : go (S.insert x seen) xs

-- 厳密にリストを評価する（遅延評価の「つけ」をためない）
forceL :: [a] -> [a]
forceL xs = foldl' (flip seq) () xs `seq` xs

-- 長い文字列を n 文字で切る（p5.js 版の s.slice(0, n - 1) + '…'）
clip :: Int -> String -> String
clip n s = if length s > n then take (n - 1) s ++ "..." else s

-- 画面に出せない文字（ASCII 以外）を ? に
asciiOnly :: String -> String
asciiOnly = map (\c -> if c >= ' ' && c <= '~' then c else '?')

-- 文字の入力：allowed に入っている 1 文字だけ足す（長さの上限つき。p5.js 版の typeInto）
typeInto :: String -> Char -> String -> Int -> String
typeInto s ch allowed maxLen = if ch `elem` allowed && length s < maxLen then s ++ [ch] else s

dropLast :: [a] -> [a]                        -- s.slice(0, -1)
dropLast xs = if null xs then xs else init xs

-- キーの名前をそろえる（gloss では スペース・Enter・Backspace・Tab・Esc が SpecialKey で来る）
-- TClear = 全部消す（p5.js 版の Esc。gloss では Esc でウィンドウが閉じるので Shift + Backspace でも）
data TypedKey = TChar Char | TBack | TClear | TEnter | TTab | TUp | TDown | TLeft | TRight | TOther
  deriving (Eq, Show)

typedKey :: Key -> Modifiers -> TypedKey
typedKey k m = case k of
  Char '\b' -> back
  Char '\DEL' -> back
  Char '\t' -> TTab
  Char '\r' -> TEnter
  Char '\n' -> TEnter
  Char '\ESC' -> TClear
  Char c -> TChar c
  SpecialKey KeySpace -> TChar ' '
  SpecialKey KeyBackspace -> back
  SpecialKey KeyDelete -> back                -- Mac の delete キー
  SpecialKey KeyEnter -> TEnter
  SpecialKey KeyPadEnter -> TEnter
  SpecialKey KeyTab -> TTab
  SpecialKey KeyEsc -> TClear
  SpecialKey KeyUp -> TUp
  SpecialKey KeyDown -> TDown
  SpecialKey KeyLeft -> TLeft
  SpecialKey KeyRight -> TRight
  _ -> TOther
  where back = if shift m == Down then TClear else TBack

-- 文字（大文字・小文字のどちらでも）か
isKey :: Char -> Char -> Bool
isKey k c = c == k || c == toUpperC k

toUpperC :: Char -> Char
toUpperC c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

type PErr = (String, Int)                     -- (まちがいの説明, 何文字目)

-- まちがいの説明を 1 行に（p5.js 版の「（n 文字目あたり）」）
errLine :: PErr -> String
errLine (m, a) = m ++ " (near character " ++ show (a + 1) ++ ")"

-- 数の書き方：小数点以下 d 桁（toFixed と同じく、半分なら大きいほうへ）
fmtN :: Int -> Double -> String
fmtN d v
  | isNaN v = "?"
  | isInfinite v = if v > 0 then "inf" else "-inf"
  | otherwise = let n = floor (toRational (abs v) * 10 ^ d + 1 / 2) :: Integer
                    ds = show n
                    padded = replicate (d + 1 - length ds) '0' ++ ds
                    (ip, fp) = splitAt (length padded - d) padded
                    body = if d > 0 then ip ++ "." ++ fp else ip
                in if n == 0 || v > 0 then body else '-' : body

-- ---- 種つきの乱数（同じ種なら毎回同じ列 → 実験をくり返せる。p5.js 版の mulberry32 と同じ）
newtype Rng = Rng Word32

-- 0〜1 の一様乱数と、次の状態
rndStep :: Rng -> (Double, Rng)
rndStep (Rng s) =
  let a = s + 0x6d2b79f5
      t1 = (a `xor` (a `shiftR` 15)) * (a .|. 1)
      t2 = t1 `xor` (t1 + (t1 `xor` (t1 `shiftR` 7)) * (t1 .|. 61))
  in (fromIntegral (t2 `xor` (t2 `shiftR` 14)) / 4294967296, Rng a)

-- 乱数を順に引く計算（p5.js 版で rnd() を呼ぶ順をそのまま書ける）。状態 = いまの乱数の列
newtype R a = R { runR :: Rng -> (a, Rng) }

instance Functor R where
  fmap f (R g) = R (\s -> case g s of (a, !s') -> (f a, s'))
instance Applicative R where
  pure a = R (\s -> (a, s))
  R f <*> R g = R (\s -> case f s of (h, !s1) -> case g s1 of (a, !s2) -> (h a, s2))
instance Monad R where
  R g >>= k = R (\s -> case g s of (a, !s1) -> runR (k a) s1)

-- seedRng(s)：種 s の列（0 なら 1）
rngOf :: Int -> Rng
rngOf s = let w = fromIntegral s :: Word32 in Rng (if w == 0 then 1 else w)

-- 計算を種 s から実行して、結果だけ
withSeed :: Int -> R a -> a
withSeed s m = fst (runR m (rngOf s))

rnd :: R Double
rnd = R rndStep

rndInt :: Int -> Int -> R Int                 -- a〜b の整数
rndInt a b = do u <- rnd; pure (a + floor (u * fromIntegral (b - a + 1)))

rndPick :: [a] -> R a                         -- リストから 1 つ（空のリストには使わない）
rndPick xs = do
  u <- rnd
  let i = floor (u * fromIntegral (length xs))
  pure (case drop i xs of
          (x : _) -> x
          [] -> case xs of
            (x : _) -> x
            [] -> errorWithoutStackTrace "rndPick: empty list")

-- n 回くり返す（厳密に、1 回ずつ状態を進める）
loopR :: Int -> (a -> R a) -> a -> R a
loopR n f a0 = go n a0
  where go k !a = if k <= 0 then pure a else f a >>= go (k - 1)

-- ---- 論理式の道具：字句 → 構文木（再帰下降）→ 文字列・真理値
-- 構文木 = FVar 名前 / FTop ⊤ / FBot ⊥ / FNot a / FBin 演算子 a b（様相論理だけ FBox a □ と FDia a ◇）
-- 強さの順：~ [] <> ＞ & ＞ | ＞ ->（右から結合：p->q->r = p->(q->r)）＞ <->
-- ASCII で書く：~ ! = ¬、& ^ = ∧、| = ∨、-> > = →、<-> = = ↔、1 T = ⊤、0 _|_ = ⊥、[] = □、<> = ◇（記号のままでも読める）
data Op = OAnd | OOr | OImp | OIff deriving (Eq, Ord, Show)

data F = FVar String | FTop | FBot | FNot F | FBin Op F F | FBox F | FDia F
  deriving (Eq, Ord, Show)

fmlKeys :: String                             -- 入力できる文字
fmlKeys = ['a' .. 'z'] ++ ['0' .. '9'] ++ "~!&|^-<>=()[] T_"

fmlMaxVars :: Int
fmlMaxVars = 6                                -- ★ 変数の数の上限（真理値表は 2⁶ = 64 行まで）

opSym :: Op -> String                         -- 画面の記号（ASCII）
opSym o = case o of
  OAnd -> "&"
  OOr -> "|"
  OImp -> "->"
  OIff -> "<->"

fVar :: String -> F
fVar = FVar

fNot :: F -> F
fNot = FNot

fBin :: Op -> F -> F -> F
fBin = FBin

fKids :: F -> [F]                             -- 子（構文木の下の段）
fKids f = case f of
  FNot a -> [a]
  FBox a -> [a]
  FDia a -> [a]
  FBin _ a b -> [a, b]
  _ -> []

data Tk = TkVar String | TkNot | TkAnd | TkOr | TkImp | TkIff | TkL | TkR | TkTop | TkBot | TkBox | TkDia
  deriving (Eq, Show)

tkSym :: Tk -> String
tkSym t = case t of
  TkNot -> "~"
  TkAnd -> "&"
  TkOr -> "|"
  TkImp -> "->"
  TkIff -> "<->"
  TkL -> "("
  TkR -> ")"
  TkTop -> "T"
  TkBot -> "_|_"
  TkBox -> "[]"
  TkDia -> "<>"
  TkVar n -> n

lexFormula :: Bool -> String -> Either PErr [(Tk, Int)]
lexFormula modal src = go 0 src
  where
    go _ [] = Right []
    go i s@(c : rest)
      | c == ' ' = go (i + 1) rest
      | pre "<->" = tok TkIff 3
      | pre "->" = tok TkImp 2
      | pre "_|_" = tok TkBot 3
      | pre "[]" || pre "<>" = if modal then tok (if c == '[' then TkBox else TkDia) 2
                               else Left ("[] <> (box, diamond) only in modal logic (12)", i)
      | c >= 'a' && c <= 'z' =
          let tl = takeWhile (\d -> (d >= '0' && d <= '9') || d == '\'') rest
              k = 1 + length tl
          in ((TkVar (c : tl), i) :) <$> go (i + k) (drop (k - 1) rest)
      | Just t <- lookup c one =
          if (t == TkBox || t == TkDia) && not modal then Left ("box and diamond only in modal logic (12)", i)
          else tok t 1
      | c `elem` "-<[]_" = Left ("'" ++ [c] ++ "' alone is not a symbol (write -> <-> [] <>)", i)
      | otherwise = Left ("character '" ++ asciiOnly [c] ++ "' not allowed", i)
      where pre p = take (length p) s == p
            tok t k = ((t, i) :) <$> go (i + k) (drop k s)
    one = [ ('¬', TkNot), ('~', TkNot), ('!', TkNot), ('∧', TkAnd), ('&', TkAnd), ('^', TkAnd), ('∨', TkOr), ('|', TkOr)
          , ('→', TkImp), ('>', TkImp), ('↔', TkIff), ('=', TkIff), ('(', TkL), (')', TkR)
          , ('⊤', TkTop), ('1', TkTop), ('T', TkTop), ('⊥', TkBot), ('0', TkBot), ('□', TkBox), ('◇', TkDia) ]

-- 返り値 Right 式 / Left (説明, 何文字目)（例外は出さない）。modal = 様相論理の □ ◇ を使えるか
parseFormulaM :: Bool -> String -> Either PErr F
parseFormulaM modal src
  | length src > 160 = Left ("too long (160 characters max)", 160)
  | otherwise = do
      toks <- lexFormula modal src
      if null toks then Left ("empty formula", 0) else do
        (f, rest) <- pIff 0 toks
        case rest of
          ((t, a) : _) -> Left (leftover t, a)
          [] -> let nv = length (fmlVars f)
                in if nv > fmlMaxVars then Left (show nv ++ " variables (" ++ show fmlMaxVars ++ " max)", 0) else Right f
  where
    len = length src
    atOf ts = case ts of
      ((_, a) : _) -> a
      [] -> len
    failAt ts m = Left (m, atOf ts)
    leftover t = case t of
      TkR -> "extra ')'"
      TkVar _ -> needOp
      TkL -> needOp
      TkTop -> needOp
      TkBot -> needOp
      TkNot -> needOp
      _ -> "'" ++ tkSym t ++ "' in a strange place"
    needOp = "an operator (& | -> ...) is needed here"
    -- 2 項演算の 1 段：左結合（right なら右結合）
    binL sub op d ts = do
      (a, r) <- sub d ts
      loop a r
      where loop a r = case r of
              ((t, _) : r') | t == op -> do (b, r2) <- sub d r'; loop (FBin (tkOp op) a b) r2
              _ -> Right (a, r)
    binR sub op d ts = do
      (a, r) <- sub d ts
      case r of
        ((t, _) : r') | t == op -> do (b, r2) <- binR sub op d r'; Right (FBin (tkOp op) a b, r2)
        _ -> Right (a, r)
    tkOp t = case t of
      TkAnd -> OAnd
      TkOr -> OOr
      TkImp -> OImp
      _ -> OIff
    pIff = binL pImp TkIff
    pImp = binR pOr TkImp
    pOr = binL pAnd TkOr
    pAnd = binL pUn TkAnd
    pUn d ts
      | d + 1 > 80 = failAt ts "nesting too deep"
      | otherwise = case ts of
          [] -> failAt ts "the formula ends too early"
          ((t, _) : r) -> case t of
            TkNot -> do (a, r2) <- pUn (d + 1) r; Right (FNot a, r2)
            TkBox -> do (a, r2) <- pUn (d + 1) r; Right (FBox a, r2)
            TkDia -> do (a, r2) <- pUn (d + 1) r; Right (FDia a, r2)
            TkVar nm -> Right (FVar nm, r)
            TkTop -> Right (FTop, r)
            TkBot -> Right (FBot, r)
            TkL -> do
              (a, r2) <- pIff (d + 1) r
              case r2 of
                ((TkR, _) : r3) -> Right (a, r3)
                _ -> failAt r2 "')' missing"
            TkR -> failAt ts "nothing before ')'"
            _ -> failAt ts ("nothing before '" ++ tkSym t ++ "'")

parseFormula :: String -> Either PErr F
parseFormula = parseFormulaM False

fmlPrec :: F -> Int                           -- 2 項演算の強さ（2 項演算でなければ 0）
fmlPrec f = case f of
  FBin OIff _ _ -> 1
  FBin OImp _ _ -> 2
  FBin OOr _ _ -> 3
  FBin OAnd _ _ -> 4
  _ -> 0

-- 構文木 → 文字列（かっこは必要な所と、ちがう演算子が混ざる所に）
fmlStr :: F -> String
fmlStr f = case f of
  FVar n -> n
  FTop -> "T"
  FBot -> "_|_"
  FNot a -> "~" ++ un a
  FBox a -> "[]" ++ un a
  FDia a -> "<>" ++ un a
  FBin op a b ->
    let side c isLeft = case c of
          FBin op2 _ _ -> let assocOk = op2 == op && (if op == OImp then not isLeft else isLeft)
                          in if assocOk then fmlStr c else "(" ++ fmlStr c ++ ")"
          _ -> fmlStr c
    in side a True ++ " " ++ opSym op ++ " " ++ side b False
  where un a = if fmlPrec a > 0 then "(" ++ fmlStr a ++ ")" else fmlStr a

-- 出てくる変数（名前の順）
fmlVars :: F -> [String]
fmlVars f = S.toList (go f S.empty)
  where go g s = case g of
          FVar n -> S.insert n s
          _ -> foldr go s (fKids g)

type Asg = M.Map String Bool                  -- 割り当て（変数 → 真偽）

-- 真理値（□ ◇ は真理値表では評価しない → 偽。12 を見よ）
evalFml :: F -> Asg -> Bool
evalFml f e = case f of
  FVar n -> M.findWithDefault False n e
  FTop -> True
  FBot -> False
  FNot a -> not (evalFml a e)
  FBin OAnd a b -> evalFml a e && evalFml b e
  FBin OOr a b -> evalFml a e || evalFml b e
  FBin OImp a b -> not (evalFml a e) || evalFml b e
  FBin OIff a b -> evalFml a e == evalFml b e
  _ -> False

-- k 番目の割り当て（上の桁が最初の変数）
envOf :: [String] -> Int -> Asg
envOf vars k = M.fromList [ (v, testBit k (length vars - 1 - i)) | (i, v) <- zip [0 ..] vars ]

-- 真理値表の行 (割り当て, 値)
truthRowsV :: [String] -> F -> [(Asg, Bool)]
truthRowsV vars f = [ (e, evalFml f e) | k <- [0 .. 2 ^ length vars - 1 :: Int], let e = envOf vars k ]

truthRows :: F -> [(Asg, Bool)]
truthRows f = truthRowsV (fmlVars f) f

data Kind = Taut | Sat | Contra deriving (Eq, Show)

-- 恒真 / 充足可能（恒真ではない）/ 矛盾、真の行の数、行の数
classifyFml :: F -> (Kind, Int, Int)
classifyFml f =
  let rows = truthRows f
      n = length (filter snd rows)
      m = length rows
  in (if n == m then Taut else if n == 0 then Contra else Sat, n, m)

kindOf :: F -> Kind
kindOf f = let (k, _, _) = classifyFml f in k

kindName :: Kind -> String
kindName k = case k of
  Taut -> "TAUTOLOGY (valid)"
  Sat -> "SATISFIABLE (not valid)"
  Contra -> "CONTRADICTION (unsatisfiable)"

fmlEq :: F -> F -> Bool
fmlEq = (==)

fmlSize :: F -> Int
fmlSize f = 1 + sum (map fmlSize (fKids f))

-- 全部の割り当てで同じ値か（同値）
sameTable :: F -> F -> Bool
sameTable f g =
  let vars = S.toList (S.fromList (fmlVars f ++ fmlVars g))
  in and [ evalFml f e == evalFml g e | k <- [0 .. 2 ^ length vars - 1 :: Int], let e = envOf vars k ]

-- でたらめな式（自己テスト用、種つき乱数。p5.js 版と同じ順に引く）
randomFml :: Int -> [String] -> Bool -> R F
randomFml d vars consts = do
  r <- rnd
  if d <= 0 || r < 0.22 then do
    c <- if consts then (< 0.12) <$> rnd else pure False
    if c then do u <- rnd; pure (if u < 0.5 then FTop else FBot)
    else FVar <$> rndPick vars
  else if r < 0.38 then FNot <$> randomFml (d - 1) vars consts
  else do
    op <- rndPick [OAnd, OOr, OImp, OIff, OAnd, OOr]
    a <- randomFml (d - 1) vars consts
    b <- randomFml (d - 1) vars consts
    pure (FBin op a b)

fmlTreeLabel :: F -> String                   -- 木の丸の中の文字
fmlTreeLabel f = case f of
  FVar n -> n
  FTop -> "T"
  FBot -> "_|_"
  FNot _ -> "~"
  FBox _ -> "[]"
  FDia _ -> "<>"
  FBin op _ _ -> opSym op

-- ---- 画面の道具（p5.js と同じ座標：左上が原点、y は下向き）
view :: Env -> [Picture] -> Picture
view env = Translate (negate (envW env) / 2) (envH env / 2) . Scale 1 (-1) . Pictures

-- マウスの位置（p5.js の mouseX, mouseY）
mouseP :: Env -> (Float, Float)
mouseP env = toP5 env (envMouse env)

toP5 :: Env -> (Float, Float) -> (Float, Float)
toP5 env (mx, my) = (mx + envW env / 2, envH env / 2 - my)

-- 左下が (x, y) の文字（y は文字の下の線 = BASELINE）
say :: String -> Float -> Float -> Float -> Color -> Picture
say s x y size c = Translate x y (Scale 1 (-1) (label 0 0 size c s))

-- 中央揃え・右揃えの文字（y は文字の下の線）
sayC, sayR :: String -> Float -> Float -> Float -> Color -> Picture
sayC s x y size c = say s (x - textW s size / 2) y size c
sayR s x y size c = say s (x - textW s size) y size c

-- 上下も中央に（textAlign(CENTER, CENTER)）
sayM :: String -> Float -> Float -> Float -> Color -> Picture
sayM s x y size c = sayC s x (y + size * 0.4) size c

-- 左揃え・右揃えで上下は中央（textAlign(LEFT, CENTER)、(RIGHT, CENTER)）
sayLM, sayRM :: String -> Float -> Float -> Float -> Color -> Picture
sayLM s x y size c = say s x (y + size * 0.4) size c
sayRM s x y size c = sayR s x (y + size * 0.4) size c

textW :: String -> Float -> Float             -- 文字の幅（だいたい）
textW s size = fromIntegral (length s) * size * 0.72

-- 中心 (x, y)、直径 d の円（p5.js の circle）
circleAt :: Float -> Float -> Float -> Picture
circleAt x y d = Translate x y (disc (max 0 d / 2))

-- 直径 d、太さ wt の輪
ringAt :: Float -> Float -> Float -> Float -> Picture
ringAt x y d wt = Translate x y (ring (max 0 d / 2) wt)

-- 左上が (x, y) の塗りつぶした長方形
rectTL :: Float -> Float -> Float -> Float -> Picture
rectTL x y w h = rect (x + w / 2) (y + h / 2) w h

-- 左上が (x, y) の長方形の枠（太さ wt）
frameTL :: Float -> Float -> Float -> Float -> Float -> Picture
frameTL wt x y w h = thickPath wt [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y - wt / 2)]

-- 太さ wt の線分
lineW :: Float -> (Float, Float) -> (Float, Float) -> Picture
lineW = thickLine

-- 点が 2 つ以上あるときだけ折れ線を描く
polyline :: Float -> [(Float, Float)] -> Picture
polyline w pts = case pts of
  (_ : _ : _) -> thickPath w pts
  _ -> Blank

-- 点 (x, y) が長方形 (bx, by, bw, bh) の中か
inBox :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
inBox (mx, my) (bx, by, bw, bh) = mx > bx && mx < bx + bw && my > by && my < by + bh

-- 左上のタイトルと操作の説明（説明は \n で改行、行の間 19）
hudTitle :: String -> String -> Picture
hudTitle t s = Pictures (say t 30 38 22 (col ink) : [ say ln 30 (62 + 19 * i2f i) 14 (col dimc) | (i, ln) <- zip [0 :: Int ..] (lines s) ])

-- 黒板のパネル（p5.js 版の角の丸い長方形）
chalkPanel :: Float -> Float -> Float -> Float -> Picture
chalkPanel x y w h = Color (col panelc) (rectTL x y w h)

-- 入力欄：ラベル、文字列（点滅するカーソル）、まちがいの説明。active = False なら灰色でカーソルなし
drawInputLine :: Int -> Float -> Float -> String -> String -> String -> Bool -> Picture
drawInputLine frame x y lab s err active =
  let lx = x + textW lab 15 + 8
      blink = active && frame `mod` 60 < 30
  in Pictures
       [ say lab x y 15 (col (if active then yel else dimc))
       , say (asciiOnly s ++ (if blink then "_" else "")) lx (y + 1) 20 (col ink)
       , if null err then Blank else say ("! " ++ asciiOnly err) lx (y + 22) 14 (col pink) ]

-- ---- 自己テストの道具
-- 結果を端末に出す（テスト中は失敗した項目とまとめの 1 行だけ）
printSelfTest :: [(String, Bool)] -> IO ()
printSelfTest rs = do
  hSetEncoding stdout utf8
  quiet <- lookupEnv "ART_TEST"
  let shown = if quiet == Nothing then rs else filter (not . snd) rs
  mapM_ (\(nm, ok) -> putStrLn ((if ok then "OK    " else "FAIL  ") ++ nm)) shown
  putStrLn (selfTestLine rs)

selfTestLine :: [(String, Bool)] -> String
selfTestLine rs = "Self-test " ++ (if all snd rs then "OK " else "FAIL ") ++ show (length (filter snd rs)) ++ " / " ++ show (length rs)

-- 小さな表示（x, y は文字の左下。右端からはみ出さないように左へ寄せる）
drawSelfTest :: Env -> Float -> Float -> [(String, Bool)] -> Picture
drawSelfTest env x y rs = say (selfTestLine rs) (min x (envW env - 240)) y 14 (if all snd rs then col grn else col pink)
