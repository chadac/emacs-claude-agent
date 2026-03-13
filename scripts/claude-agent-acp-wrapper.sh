#!/usr/bin/env bash
# Wrapper script for claude-agent-acp that ensures Node.js is available.
# This handles systems where node is provided via nix-shell or similar.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PACKAGE_DIR="$(dirname "$SCRIPT_DIR")"
ACP_BIN="$PACKAGE_DIR/vendor/claude-agent-acp/node_modules/.bin/claude-agent-acp"

if [ ! -x "$ACP_BIN" ]; then
    echo "Error: claude-agent-acp not found at $ACP_BIN" >&2
    echo "Run: npm install --prefix vendor/claude-agent-acp @zed-industries/claude-agent-acp" >&2
    exit 1
fi

# If node is on PATH, run directly
if command -v node &>/dev/null; then
    exec "$ACP_BIN" "$@"
fi

# Try nix-shell as fallback
if command -v nix-shell &>/dev/null; then
    exec nix-shell -p nodejs --run "\"$ACP_BIN\" $*"
fi

echo "Error: node not found. Install Node.js or use nix-shell." >&2
exit 1
