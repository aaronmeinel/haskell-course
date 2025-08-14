#!/usr/bin/env bash
set -euo pipefail

# Simple Elm watch script: rebuild on changes to frontend/src or frontend/public.
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
cd "$ROOT_DIR"

echo "[watch-elm] Watching frontend/src and frontend/public for changes..."

WATCH_PATHS=(frontend/src frontend/public)

build() {
  ./scripts/build-elm.sh || echo "[watch-elm] Build failed (will retry on next change)"
}

if command -v inotifywait >/dev/null 2>&1; then
  build
  while inotifywait -qq -r -e modify,create,delete,move "${WATCH_PATHS[@]}"; do
    echo "[watch-elm] Change detected, rebuilding..."
    build
  done
else
  echo "[watch-elm] inotifywait not found; falling back to 1s polling." >&2
  SNAP=""
  while true; do
    NEWSNAP=$(find "${WATCH_PATHS[@]}" -type f -printf '%P %T@\n' | sort)
    if [[ "$NEWSNAP" != "$SNAP" ]]; then
      echo "[watch-elm] Change detected (poll), rebuilding..."
      build
      SNAP="$NEWSNAP"
    fi
    sleep 1
  done
fi
