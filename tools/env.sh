# ツールの場所を変えたいときは tools/env.local に書く（例：PATH の設定）
[ -f "$(dirname "$0")/env.local" ] && source "$(dirname "$0")/env.local"
[ -f tools/env.local ] && source tools/env.local
true
