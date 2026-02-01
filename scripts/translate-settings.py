#!/usr/bin/env python3
"""Translate paths in .claude/settings.local.json for worktrees.

Usage: translate-settings.py <project-root> <worktree-path>

Reads .claude/settings.local.json from <project-root>, rewrites any
permission patterns containing <project-root> paths to use
<worktree-path> instead, and writes the result to
<worktree-path>/.claude/settings.local.json.

Patterns that don't contain filesystem paths (e.g. "Bash(git:*)" or
"mcp__emacs__eval(*)") are preserved unchanged.

If the source settings.local.json doesn't exist, this is a no-op.
"""
import json
import sys
from pathlib import Path


def translate_permission(pattern: str, project_root: str, worktree_path: str) -> str:
    """Translate a single permission pattern by replacing project-root with worktree-path.

    Only replaces paths that appear inside permission tool calls like:
      Edit(/home/user/main-repo/*)
      Read(//home/user/main-repo/**)

    Non-path patterns like "Bash(git:*)" or "mcp__emacs__eval(*)" are
    returned unchanged.
    """
    # Replace the project-root path wherever it appears in the pattern.
    # This handles patterns like:
    #   Edit(/home/user/main-repo/*)
    #   Read(//home/user/main-repo/**)
    #   Write(/home/user/main-repo/subdir/*)
    if project_root in pattern:
        return pattern.replace(project_root, worktree_path)
    return pattern


def translate_settings(project_root: str, worktree_path: str) -> bool:
    """Read settings from project_root, translate paths, write to worktree_path.

    Returns True if settings were translated and written, False otherwise.
    """
    # Normalize paths (remove trailing slashes for consistent matching)
    project_root = str(Path(project_root).resolve()).rstrip("/")
    worktree_path = str(Path(worktree_path).resolve()).rstrip("/")

    src = Path(project_root) / ".claude" / "settings.local.json"
    dst = Path(worktree_path) / ".claude" / "settings.local.json"

    if not src.exists():
        print(f"No settings.local.json at {src}, skipping", file=sys.stderr)
        return False

    with open(src) as f:
        settings = json.load(f)

    # Translate all permission lists
    permissions = settings.get("permissions", {})
    for key in ("allow", "deny", "ask"):
        if key in permissions and isinstance(permissions[key], list):
            permissions[key] = [
                translate_permission(p, project_root, worktree_path)
                for p in permissions[key]
            ]

    # Ensure destination directory exists
    dst.parent.mkdir(parents=True, exist_ok=True)

    with open(dst, "w") as f:
        json.dump(settings, f, indent=2)
        f.write("\n")

    print(f"Translated settings: {src} -> {dst}")
    return True


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(
            f"Usage: {sys.argv[0]} <project-root> <worktree-path>",
            file=sys.stderr,
        )
        sys.exit(1)

    project_root = sys.argv[1]
    worktree_path = sys.argv[2]

    success = translate_settings(project_root, worktree_path)
    sys.exit(0 if success else 1)
