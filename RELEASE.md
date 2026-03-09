RELEASE_TYPE: minor

Manage hegel binary via pinned .hegel/venv instead of relying on system installation.

- `hegel_version` is now a plain git commit hash for hegel-core, hard-coded in `session.ml`
- On first use, creates a project-local `.hegel/venv` virtualenv via `uv venv` and installs hegel into it via `uv pip install`
- A `.hegel/venv/hegel-version` file tracks the installed hash; subsequent runs skip installation if it matches `hegel_version`
- On version mismatch the venv is recreated and hegel is reinstalled
- `HEGEL_CMD` environment variable overrides to a specific binary path, skipping installation entirely
- The release script pins `hegel_version` to the current hegel-core main HEAD SHA at release time via `gh api`
