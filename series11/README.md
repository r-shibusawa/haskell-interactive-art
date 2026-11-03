# シリーズ11：基本データ構造 ─ Haskell コード集

スタック、キュー、連結リスト、ハッシュ表、二分探索木、赤黒木、拡張データ構造を、動くアニメーションで体験するコード集を Haskell（gloss）に移植しました。データを「しまう形」で速さが何倍も変わることを確かめられます。木のスケッチは途中経過を「コマ」として記録して再生するので、回転やぬりかえを 1 歩ずつ追えます。どのスケッチも `Main.hs` 1 ファイルで完結しています。

参考：Cormen ほか『Introduction to Algorithms』

p5.js 版： https://r-shibusawa.github.io/p5-interactive-art/series11/

## 使い方

```sh
cabal run s11-00
```

番号を変えると別のスケッチが動きます（`s11-07`、`s11-14` など）。

- 木のスケッチは `P` で一時停止、`←` `→` でコマ送りです。
- 数字を打ったあとに `Enter`（挿入）、`S`（探索）、`D`（削除）を押します。
- 🔊 音を使うスケッチは `afplay`（macOS）または `aplay`（Linux）で鳴らします。

## スケッチ一覧

| # | タイトル | 内容 | 操作 | 使うもの |
|---|---|---|---|---|
| 00 | [スタックとキュー](sketches/00_stack_queue/Main.hs) | LIFO と FIFO、循環バッファ | P O E D スペース R | |
| 01 | [双方向連結リストと番兵](sketches/01_linked_list/Main.hs) | 挿入・探索・削除 | 数字+Enter S D クリック スペース R | |
| 02 | [ポインタを配列で作る](sketches/02_array_pointers/Main.hs) | next/key/prev と空きリスト | スペース 数字+Enter クリック R | |
| 03 | [根つき木の表し方](sketches/03_rooted_trees/Main.hs) | 左子・右兄弟表現 | クリック 1 2 スペース R | |
| 04 | [直接アドレス表](sketches/04_direct_address/Main.hs) | キー = 番号。U が大きいと困る | クリック S 1〜3 スペース C | |
| 05 | [チェイン法のハッシュ表](sketches/05_hash_chaining/Main.hs) | 衝突をリストで、負荷率 α | 数字+Enter S D スペース A ↑↓ H C | |
| 06 | [よいハッシュ関数とは](sketches/06_hash_functions/Main.hs) | 割り算法・かけ算法・ユニバーサル | 1〜6 ↑↓ R スペース | |
| 07 | [オープンアドレス法](sketches/07_open_addressing/Main.hs) | 線形・二次・ダブルハッシュ、削除 | スペース 数字+Enter 数字+S クリック M T C | |
| 08 | [二分探索木：探す・なぞる](sketches/08_bst_basics/Main.hs) | 探索・最小・後者・3 つのなぞり方 | 数字+Enter 数字+S N X W クリック スペース R C P ← → | |
| 09 | [二分探索木の削除](sketches/09_bst_delete/Main.hs) | TRANSPLANT と 3 つの場合 | クリック 数字+D 数字+Enter スペース R P ← → | |
| 10 | [入れる順番と木の高さ](sketches/10_random_bst/Main.hs) | ランダムなら O(log n) | 1〜3 ↑↓ スペース C | |
| 11 | [赤黒木：挿入と回転](sketches/11_rb_insert/Main.hs) | ケース 1〜3 | 数字+Enter スペース A F R P ← → | |
| 12 | [赤黒木：削除](sketches/12_rb_delete/Main.hs) | ケース 1〜4 | クリック 数字+D 数字+Enter スペース R P ← → | |
| 13 | [順序統計木](sketches/13_order_statistic/Main.hs) | k 番目を探す（SELECT）/ 順位を求める（RANK） | 数字+S クリック 数字+Enter 数字+D スペース R P ← → | |
| 14 | [木の音楽](sketches/14_tree_music/Main.hs) | 探索の道のりを音に。**自由制作のベース** | クリック スペース W A C 数字+Enter | 🔊 |
| 15 | [区間木](sketches/15_interval_tree/Main.hs) | 重なる区間を探す | ドラッグ Q スペース F クリック C P ← → | |

## p5.js 版との違い

- 画面の文字は英語です（gloss のフォントは日本語を表示できません）。操作の説明が長いものは 2 行に分けています。07 の「削除済」は `DEL`、「3回」は `3x` と表示します。04 の果物の名前も英語です。
- 文字の大きさは 14 以上にそろえています。そのため 13・15 で節点がいっぱいのときは、節点の下の `size` / `max` の文字が p5.js 版より混み合います。
- 04：U = 400 のときはマスが小さく番号を書けないので、左の図にマウスを乗せると図の下にキーの番号を表示します。
- 00：キューの輪を少し小さくし、題名を輪の上に固定しました（`head` `tail` の文字が題名と重ならないように）。
- 10：1 フレームで実験に使う時間を 8 ミリ秒から 6 ミリ秒にしています（`msPerFrame`）。1 秒あたりの実験回数はそのぶん変わります。
- 14：音は短い音を OS のコマンドで 1 つずつ鳴らすので、p5.js 版より少し遅れて聞こえます。待っている音は 500 個までです。
