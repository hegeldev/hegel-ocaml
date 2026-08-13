"""Pin a new libhegel release and leave a fully-formed commit on a local branch.

Resolves the target hegel-rust version (an explicit argument, else the latest
release), writes it into `lib/ffi/loader.ml`, regenerates the baked-in checksums
via the existing `scripts/update-checksums.py`, drops a `RELEASE.md` so merging
the bump PR cuts a hegel-ocaml release, and commits the result on the bump
branch. The commit is intentionally *not* pushed: the workflow then realigns
the ctypes FFI binding to the new release, amends the result into this commit,
and pushes (and opens/updates the PR) once.

Exposes `bumped`, `version`, and `branch` step outputs for the workflow.

Requires the GitHub CLI (`gh`) on PATH with `GH_TOKEN` set.
"""

import os
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent.parent
RUST_REPO = "hegeldev/hegel-rust"
# One branch per pinned version. A fixed, reused branch meant every release
# force-recreated it from main, clobbering any manual or agent work pushed to
# the open PR; a per-version branch keeps re-runs of the *same* version
# idempotent (the workflow's force-push) while never touching another
# version's work. The workflow closes superseded bot-only bump PRs after
# pushing.
BRANCH_PREFIX = "ci/bump-hegel-rust-"
LOADER = ROOT / "lib" / "ffi" / "loader.ml"
UPDATE_CHECKSUMS = ROOT / "scripts" / "update-checksums.py"
RELEASE_MD = ROOT / "RELEASE.md"

VERSION_RE = re.compile(r'^let version = "([^"]+)"', re.MULTILINE)


def git(*args: str) -> None:
    subprocess.run(["git", *args], check=True, cwd=ROOT)


def set_output(name: str, value: str) -> None:
    """Expose a step output to later workflow steps (no-op outside Actions)."""
    out = os.environ.get("GITHUB_OUTPUT")
    if not out:
        return
    with open(out, "a") as f:
        f.write(f"{name}={value}\n")


def get_pinned_version() -> str:
    m = VERSION_RE.search(LOADER.read_text(encoding="utf-8"))
    assert m is not None, "could not find `let version` in loader.ml"
    return m.group(1)


def resolve_latest() -> str:
    # `gh release view` with no tag resolves the latest release; strip the
    # leading `v` so it matches the `let version` form (e.g. "0.19.0").
    tag = subprocess.run(
        ["gh", "release", "view", "--repo", RUST_REPO,
         "--json", "tagName", "--jq", ".tagName"],
        check=True, capture_output=True, text=True,
    ).stdout.strip()
    return tag.lstrip("v")


def set_pinned_version(version: str) -> None:
    text = LOADER.read_text(encoding="utf-8")
    new_text, n = VERSION_RE.subn(f'let version = "{version}"', text, count=1)
    assert n == 1, "expected exactly one `let version` line in loader.ml"
    LOADER.write_text(new_text, encoding="utf-8")


def bump(requested: str) -> None:
    current = get_pinned_version()
    target = requested or resolve_latest()

    if target == current:
        print(f"Already pinned to v{current}; nothing to do.")
        set_output("bumped", "false")
        return

    # Pin the new version, then regenerate the checksums table.
    # update-checksums.py reads the version back out of loader.ml, so it must
    # be written first.
    set_pinned_version(target)
    subprocess.run([sys.executable, str(UPDATE_CHECKSUMS)], check=True, cwd=ROOT)

    current_url = f"https://github.com/{RUST_REPO}/releases/tag/v{current}"
    new_url = f"https://github.com/{RUST_REPO}/releases/tag/v{target}"

    RELEASE_MD.write_text(
        "RELEASE_TYPE: patch\n\n"
        f"This patch bumps our pinned libhegel ([hegel-rust]({RUST_REPO})) from "
        f"[{current}]({current_url}) to [{target}]({new_url}).\n",
        encoding="utf-8",
    )

    app_id = os.environ["HEGEL_RELEASE_APP_ID"]
    git("config", "user.name", "hegel-release[bot]")
    git("config", "user.email", f"{app_id}+hegel-release[bot]@users.noreply.github.com")

    # The per-version branch for this release. Commit locally only; the
    # workflow pushes the branch after folding in the FFI alignment.
    branch = BRANCH_PREFIX + target
    git("checkout", "-B", branch)
    git("add", str(LOADER), str(RELEASE_MD))
    git("commit", "-m", f"Bump pinned libhegel to {target}")

    set_output("bumped", "true")
    set_output("version", target)
    set_output("branch", branch)


if __name__ == "__main__":
    # An optional argument pins that exact version; with none we take the
    # latest. The repository_dispatch trigger passes client_payload.version.
    bump(sys.argv[1] if len(sys.argv) > 1 else "")
