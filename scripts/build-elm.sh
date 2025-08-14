#!/usr/bin/env bash
set -euo pipefail

# Robust Elm build/bootstrap script (no npm). Installs Elm locally, initializes dependencies
# if missing or inconsistent, then builds optimized bundle to dist/.

ELM_VERSION=0.19.1
ROOT_DIR="$(pwd)"
ELM_BIN_DIR="$ROOT_DIR/.elm-bin"
ELM_BIN="$ELM_BIN_DIR/elm"
FRONTEND_DIR="$ROOT_DIR/frontend"
DIST_DIR="$ROOT_DIR/dist"
PUBLIC_DIR="$FRONTEND_DIR/public"
SENTINEL="$FRONTEND_DIR/.elm-deps-ok"

log(){ printf '[elm-build] %s\n' "$*"; }

install_elm() {
  if [[ -x "$ELM_BIN" ]]; then return 0; fi
  log "Installing Elm $ELM_VERSION locally..."
  mkdir -p "$ELM_BIN_DIR"
  local ARCH=$(uname -m)
  case "$ARCH" in
    x86_64|amd64) local URL="https://github.com/elm/compiler/releases/download/$ELM_VERSION/binary-for-linux-64-bit.gz" ;;
    *) echo "Unsupported architecture: $ARCH" >&2; exit 1 ;;
  esac
  local TMP=$(mktemp)
  curl -Ls "$URL" -o "$TMP"
  gunzip -c "$TMP" > "$ELM_BIN"
  chmod +x "$ELM_BIN"
  rm "$TMP"
  log "Elm installed at $ELM_BIN"
}

bootstrap_deps() {
  cd "$FRONTEND_DIR"
  if [[ -f elm.json && -f "$SENTINEL" ]]; then
    log "Dependencies sentinel present; skipping bootstrap."
    return 0
  fi
  log "Bootstrapping Elm dependencies..."
  rm -rf elm.json elm-stuff
  printf 'y\n' | "$ELM_BIN" init >/dev/null
  # Ensure src exists (elm init creates it if absent)
  mkdir -p src
  for pkg in elm/browser elm/json elm/http; do
    printf 'y\n' | "$ELM_BIN" install "$pkg" >/dev/null
  done
  touch "$SENTINEL"
  log "Dependencies resolved."
}

build() {
  mkdir -p "$DIST_DIR"
  # Copy static assets
  if [[ -d "$PUBLIC_DIR" ]]; then
    cp -r "$PUBLIC_DIR"/* "$DIST_DIR"/
  fi
  cd "$FRONTEND_DIR"
  "$ELM_BIN" make src/Main.elm --optimize --output "$DIST_DIR/elm.js"
  log "Build complete -> $DIST_DIR/elm.js"
}

main() {
  install_elm
  bootstrap_deps
  build
  log "Done."
}

main "$@"
