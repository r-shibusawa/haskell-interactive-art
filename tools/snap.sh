#!/bin/zsh
# 使い方: tools/snap.sh <シリーズ番号> <出力先> — テストモードの SVG を Chrome で PNG にする（見た目の確認用）
cd "$(dirname "$0")/.."
N=$1; D=$2; mkdir -p $D
ART_SVG_ROOT=$D tools/check.sh $N
G="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
for f in $D/s$N/*/*.svg(N); do
  "$G" --headless=new --disable-gpu --window-size=1280,800 --user-data-dir=$D/.chrome --screenshot=${f:r}.png "file://${f:A}" >/dev/null 2>&1
done
ls $D/s$N/*/*.png(N) | wc -l
