-- ============================================
-- 03 L-システム（文字の置きかえで植物をつくる）
-- 文字列のルールをくり返し適用すると、植物の形が生まれる
-- 1〜4 キーで植物の種類 / ↑↓ で世代 / クリックでもう一度成長
-- ============================================
module Main (main) where

import Art
import qualified Data.ByteString.Char8 as BC
import Data.List (foldl')
import qualified Data.IntMap.Strict as IM

-- ルール集：どの文字を、何に置きかえるか
-- F = 前に進んで線を描く  + = 右に回る  - = 左に回る
-- [ = 今の場所を覚える   ] = 覚えた場所に戻る   X = 成長の目（描かない）
data Preset = Preset
  { pName  :: String
  , pAxiom :: String
  , pRules :: [(Char, String)]
  , pAngle :: Float    -- 度
  , pGen   :: Int
  , pLen   :: Float
  }

presets :: [Preset]   -- ★ 自分のルールを足してみよう
presets =
  [ Preset "Grass" "F" [('F', "FF+[+F-F-F]-[-F+F+F]")] 22.5 4 7
  , Preset "Twig" "X" [('X', "F[+X][-X]FX"), ('F', "FF")] 25.7 6 6
  , Preset "Tree" "X" [('X', "F-[[X]+X]+F[+FX]-X"), ('F', "FF")] 25 5 8
  , Preset "Fern" "F" [('F', "F[+F]F[-F]F")] 25.7 4 7
  ]

-- 描く線 1 本：文字列の何文字目か、絵
data Seg = Seg !Int Picture

data World = World
  { wCur   :: !Int     -- いま選んでいる植物
  , wGen   :: !Int     -- 世代（ルールを何回適用したか）
  , wChars :: !Int     -- 生成された文字列の長さ
  , wGrow  :: !Int     -- 成長アニメーション用（何文字目まで描くか）
  , wSegs  :: [Seg]    -- 画面に入る線（文字の順）
  , wAll   :: !Int     -- 間引く前の線の数
  }

maxLines :: Int
maxLines = 7400        -- ★ 同時に描く線の上限。超えたら枝分かれの深い細かい枝から省く

growSpeed :: Int
growSpeed = 60         -- ★ 1 フレームに進む文字数

main :: IO ()
main = runSketch (sketch "02-03 L-System" (generate 1280 800 2 (pGen (preset 2))))
  { skBack  = hsb 120 20 8
  , skStep  = \_ _ w -> pure w { wGrow = min (wGrow w + growSpeed) (wChars w) }  -- 少しずつ描く＝成長
  , skDraw  = drawWorld
  , skEvent = onEvent
  }

-- ルールを gen 回適用して文字列をつくり、カメの動きを線に直す
generate :: Float -> Float -> Int -> Int -> World
generate ww wh cur gen =
  let p = preset cur
      sentence = iterate (rewrite p) (BC.pack (pAxiom p)) !! gen
      (segs, total) = turtle ww wh p sentence
  in World cur gen (BC.length sentence) 0 segs total

-- ルールがある文字は置きかえ、ない文字はそのまま
rewrite :: Preset -> BC.ByteString -> BC.ByteString
rewrite p = BC.concatMap (\c -> maybe (BC.singleton c) BC.pack (lookup c (pRules p)))

drawWorld :: Env -> World -> Picture
drawWorld env w = Pictures
  [ at (0, -wh / 2 + 20) (Color (hsba 100 50 90 0.8) (Pictures shown))
  , label left (wh / 2 - 30) 16 white
      ( "Keys 1-4: plant (" ++ pName p ++ ")   Up/Down: generation (" ++ show (wGen w)
        ++ ")   Chars " ++ show (wChars w) )
  , label left (wh / 2 - 54) 14 (gray 0.7) ("Rules: " ++ showRules (pRules p))
  , if wAll w > length (wSegs w)
      then label left (wh / 2 - 78) 14 (gray 0.7)
             ("Drawing " ++ show (length (wSegs w)) ++ " of " ++ show (wAll w) ++ " lines: finest twigs hidden")
      else Blank
  ]
  where
    wh = envH env
    left = -envW env / 2 + 20
    p = preset (wCur w)
    shown = [pic | Seg _ pic <- takeWhile (\(Seg i _) -> i < wGrow w) (wSegs w)]

onEvent :: Env -> Event -> World -> IO World
onEvent env ev w = case ev of
  EventKey (Char c) Down _ _ | c >= '1' && c <= '4' ->
    let cur = fromEnum c - fromEnum '1' in pure (regen cur (pGen (preset cur)))
  EventKey (SpecialKey KeyUp) Down _ _ | wGen w < 7 -> pure (regen (wCur w) (wGen w + 1))
  EventKey (SpecialKey KeyDown) Down _ _ | wGen w > 0 -> pure (regen (wCur w) (wGen w - 1))
  EventKey (MouseButton LeftButton) Down _ _ -> pure w { wGrow = 0 }
  EventResize (a, b) -> pure (generate (fromIntegral a) (fromIntegral b) (wCur w) (wGen w))
  _ -> pure w
  where regen = generate (envW env) (envH env)

-- ---------- やってみよう ----------
-- 1. 世代を 0 から 1 つずつ上げて、文字列が長くなるのを見よう
-- 2. 角度を 90 にすると、植物が「回路」みたいになる
-- 3. 自分のルールを足そう  Preset "Mine" "F" [('F', "F+F-F")] 60 4 5

-- ---------- 道具 ----------

preset :: Int -> Preset
preset i = case drop i presets of
  (p : _) -> p
  [] -> Preset "?" "F" [] 25 0 7

-- カメの状態：位置、向き（真上が 0、右回りが正）、文字の番号、[ の深さ、覚えた場所、描いた線
data Turtle = Turtle !Float !Float !Float !Int !Int ![(Float, Float, Float)] ![Raw] !Int

data Raw = Raw !Int !Int !Float !Float !Float !Float

-- 文字列を「タートル（カメ）」の命令として読む。画面の外の線は覚えない
turtle :: Float -> Float -> Preset -> BC.ByteString -> ([Seg], Int)
turtle ww wh p sentence =
  let Turtle _ _ _ _ _ _ raw total = BC.foldl' step (Turtle 0 0 0 0 0 [] [] 0) sentence
      kept = limitLines (reverse raw)
  in (map toSeg kept, total)
  where
    a = pAngle p * pi / 180
    len = pLen p
    inside x y = abs x <= ww / 2 + 10 && y >= -30 && y <= wh + 10
    step (Turtle x y h i nest st out n) c = case c of
      'F' ->
        let x' = x + len * sin h
            y' = y + len * cos h
        in if inside x y || inside x' y'
             then Turtle x' y' h (i + 1) nest st (Raw i nest x y x' y' : out) (n + 1)
             else Turtle x' y' h (i + 1) nest st out n
      '+' -> Turtle x y (h + a) (i + 1) nest st out n
      '-' -> Turtle x y (h - a) (i + 1) nest st out n
      '[' -> Turtle x y h (i + 1) (nest + 1) ((x, y, h) : st) out n
      ']' -> case st of
        ((px, py, ph) : rest) -> Turtle px py ph (i + 1) (nest - 1) rest out n
        [] -> Turtle x y h (i + 1) nest st out n
      _ -> Turtle x y h (i + 1) nest st out n
    toSeg (Raw i _ x1 y1 x2 y2) = Seg i (thickLine 1.5 (x1, y1) (x2, y2))

-- 線が多すぎるときは、[ ] の入れ子が深い線（細かい枝）から省く
limitLines :: [Raw] -> [Raw]
limitLines raws
  | sum (IM.elems hist) <= maxLines = raws
  | otherwise = take maxLines [r | r@(Raw _ nest _ _ _ _) <- raws, nest <= limitNest]
  where
    hist = foldl' (\m (Raw _ nest _ _ _ _) -> IM.insertWith (+) nest (1 :: Int) m) IM.empty raws
    cumulative = drop 1 (scanl (\(_, s) (k, v) -> (k, s + v)) (0, 0) (IM.toAscList hist))
    limitNest = case takeWhile ((<= maxLines) . snd) cumulative of
      [] -> 0
      xs -> fst (last xs)

-- ルールを JSON 風の文字列に（p5.js の JSON.stringify）
showRules :: [(Char, String)] -> String
showRules rs = "{" ++ go rs ++ "}"
  where
    one (c, s) = show [c] ++ ":" ++ show s
    go [] = ""
    go [r] = one r
    go (r : more) = one r ++ "," ++ go more
