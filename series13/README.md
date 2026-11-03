# シリーズ13：高度なデータ構造 ─ Haskell コード集

B木、二項ヒープとフィボナッチヒープ、van Emde Boas 木、互いに素な集合。たくさんのデータを「速く」扱うための工夫がつまったデータ構造を、1 手ずつ動かして理解する 16 本を、Haskell（gloss）に移植したコード集です。どのスケッチも `Main.hs` 1 ファイルで完結していて、アルゴリズムの本体も、その途中経過をコマとして記録するしくみも、同じファイルの中にあります。

参考：Cormen ほか『Introduction to Algorithms』

p5.js 版： https://r-shibusawa.github.io/p5-interactive-art/series13/

## 使い方

```sh
cabal run s13-00
```

番号を変えると別のスケッチが動きます（`s13-07`、`s13-12` など）。

- 🔊 音を使うスケッチは `afplay`（macOS）または `aplay`（Linux）で鳴らします。
- 数字を打つスケッチは、数字キーのあと Enter で確定します。delete キーで 1 文字消せます。

## スケッチ一覧

| # | タイトル | 内容 | 操作 | 使うもの |
|---|---|---|---|---|
| 00 | [B木の探索](sketches/00_btree_search/Main.hs) | 最小次数 t、高さ、DISK-READ、2-3-4 木 = 赤黒木 | 数字+Enter スペース ←→ V R | |
| 01 | [B木への挿入](sketches/01_btree_insert/Main.hs) | 先回り分割 SPLIT-CHILD | 数字+Enter スペース A ←→ P N R | |
| 02 | [B木からの削除](sketches/02_btree_delete/Main.hs) | ケース 1・2a・2b・2c・3a・3b | キーをクリック 数字+Enter スペース I ←→ R | |
| 03 | [二項ヒープ](sketches/03_binomial_heap/Main.hs) | 2 進数の足し算と UNION | 数字+Enter スペース X U クリック R | |
| 04 | [フィボナッチヒープ](sketches/04_fib_heap/Main.hs) | INSERT・UNION・EXTRACT-MIN・CONSOLIDATE、ポテンシャル | 数字+Enter スペース X U P N R | |
| 05 | [キーを減らす](sketches/05_fib_decrease_key/Main.hs) | CUT・CASCADING-CUT・印・DELETE | クリック D スペース X R | |
| 06 | [なぜ「フィボナッチ」？](sketches/06_fib_degree_bound/Main.hs) | 次数の上限 F(k+2) ≥ φ^k、ヒープの比較表 | ←→ スペース H | |
| 07 | [ビットベクトルと要約](sketches/07_bit_vector_summary/Main.hs) | √u 個のかたまり、high・low・index | クリック 数字+Enter スペース ←→ R | |
| 08 | [proto-vEB](sketches/08_proto_veb/Main.hs) | 再帰的な要約と呼び出しの数 | 数字+Enter スペース 数直線クリック M R | |
| 09 | [van Emde Boas 木](sketches/09_veb_tree/Main.hs) | min/max の工夫で O(lg lg u) | クリック 数字+Enter スペース P ←→ R | |
| 10 | [互いに素な集合（連結リスト）](sketches/10_linked_list_sets/Main.hs) | 重みつき合併 | 2 つクリック スペース W B R | |
| 11 | [互いに素な集合の森](sketches/11_union_find_forest/Main.hs) | ランクによる合併・経路圧縮・α(n) | クリック ドラッグ スペース F K P C R | |
| 12 | [迷路をつくる](sketches/12_maze_kruskal/Main.hs) | ランダム版クラスカル法、合併を音に | スペース ↑↓ 1〜3 S M R | 🔊 |
| 13 | [しみこみブラシ](sketches/13_percolation_brush/Main.hs) | 連結成分とパーコレーション（**自由制作のベース**） | ドラッグ A ←→ S C R | 🔊 |
| 14 | [オフライン最小値問題](sketches/14_offline_minimum/Main.hs) | 先読みで EXTRACT-MIN に答える | スペース A N R | |
| 15 | [タージャンのオフライン LCA](sketches/15_tarjan_lca/Main.hs) | 深さ優先探索 × 互いに素な集合 | スペース A クリック N R | |

## p5.js 版との違い

- **全体**：
  - 画面の文字は英語です（gloss の文字は ASCII のみ）。長い説明は短くするか、2 行に分けています。添字や記号も ASCII にしています（`K₁` は `K1`、`Φ` は `Phi`、`√u` は `sqrt u`、`−∞` は `-inf`）。
  - 箱の角は丸くありません。
  - マスや節点に入りきらない番号は、大きさを変えずに横に縮めて描きます。
- **00〜05**：すでに見せ終わったコマは捨てます（どのスケッチもコマは先にしか進まないので、見た目は同じです）。
- **04**：1〜99 を全部使ったあとは、スペースと U で新しいキーを足しません（p5.js 版は同じキーの節点ができます）。
- **02**：ケース表の説明を短くしました。
- **06**：比較表の列名は `Bin`（2 分ヒープ）、`Binom`（二項ヒープ）、`Fib*`（フィボナッチヒープ）です。
- **07**：結果の表示を右端から少し左に寄せました（文字が大きくなった分）。
- **08**：小さい箱と呼び出しの記録では `proto-vEB(u)` を `P(u)` と略します。小さい箱の「要約」は `sum` です。
- **09**：
  - 幅のせまい箱は、`min` と `max` を縦に分けて書きます。u = 256 の一番せまい箱は名前を省き、上が min、下が max の数字だけです。
  - 長い呼び出しの記録は折り返して、次の行に送ります（p5.js 版は行が重なることがあります）。
- **10**：集合の箱を少し広げ（文字が入るように）、リストの開始位置を右にずらしました。
- **11**：根の上の `rank 2` は `r2` と略します（節点の間隔に入るように）。
- **12・13**：音を出すのにクリックは要りません。音は短い音を別々に再生するので、少し遅れて聞こえることがあります。
- **14**：FIND-SET は親をたどるだけで、経路半分化はしません（数が少ないので見た目も結果も同じです）。
