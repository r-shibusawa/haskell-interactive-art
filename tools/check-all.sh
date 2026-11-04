#!/bin/zsh
# 全シリーズを並列でテストする（使い方: tools/check-all.sh [並列数]）
cd "$(dirname "$0")/.."
ls -d series*/ | sed 's/series//; s#/##' | sort -n | xargs -P ${1:-5} -I{} sh -c 'tools/check.sh {} > .build/all-{}.log 2>&1; echo "{} exit=$?"'
