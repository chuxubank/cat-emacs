# Cat Emacs Agent Guide

## Scope

This file applies to the entire repository.

Cat Emacs is a personal, modular Emacs configuration, not an Emacs package.
Prefer direct configuration code and the existing local conventions over
package-project scaffolding or abstractions intended only for distribution.

Preserve unrelated user changes. The worktree may be dirty, including a
separate chezmoi worktree. Do not revert, stage, commit, apply, or rewrite
changes outside the requested scope.

## Architecture

- `early-init.el` defines platform flags, runtime directories, and startup
  instrumentation.
- `init.el` adds local Lisp to `load-path` and calls `cat-core-initialize`.
- `core/core.el` is the single core entry point. Keep its library list in
  explicit dependency order.
- `core/config.el` owns user/template configuration lookup and Custom loading.
- `core/module.el` reads the selected `cats` data and loads modules.
- `core/package/` owns package archives, manifest collection, the `:cat`
  keyword, bootstrap dependencies, synchronization, and upgrades.
- `modules/<group>/+<name>.el` contains optional feature configuration.
- `templates/` contains repository defaults. User overrides live under
  `$HOME/.config/cat-emacs/`.

Keep core initialization deterministic:

1. Load configuration and module primitives.
2. Load package-management implementation.
3. Collect package declarations while loading the selected modules.
4. Mark core and package state ready only after successful activation.

Do not add an alternative loader or an implicit core load path.

## Package Declarations

Enabled `cats` modules and their `use-package` forms are the only source of
managed package declarations.

- Declare archive packages with `use-package`.
- Declare VC packages with `use-package` and `:vc`.
- Use `:cat` for optional package-backed features.
- Enable or disable modules and Cat features in a `cats` file.
- Keep package-manager bootstrap dependencies private to `core/package/`.

Do not add or restore:

- static `package-selected-packages`, `package-vc-selected-packages`, or
  `package-pinned-packages` values;
- `cat-package-extra-*` escape hatches;
- interactive selected-package editing modules;
- package lists generated or maintained by chezmoi;
- a second CI- or Docker-only package declaration source.

The runtime manifest is derived state. It must remain in memory and must not be
written to `custom.el`. `package-selected-packages` must protect the union of
ELPA and VC roots from `package-autoremove`; bind it to ELPA-only roots only
for `package-install-selected-packages`.

Package collection, synchronization, and upgrades must restore manifest and
selection state on failure. Package operations must suppress both
`package--save-selected-packages` and Custom persistence while preserving
their in-memory updates.

Docker cache artifacts may contain a generated manifest, but it must be
generated from `cats` and `use-package` declarations during the build and
must never become a manually maintained source file.

## Configuration Style

- Keep lexical binding enabled in Emacs Lisp files.
- Follow nearby `use-package`, naming, and module patterns.
- Put shared lifecycle and package behavior in core; keep feature-specific
  settings in their module.
- Avoid shallow wrapper functions unless they enforce a real invariant or hide
  repeated lifecycle behavior.
- Do not add `declare-function` forms solely for byte-compiler warnings. This
  configuration is not byte-compiled as a package.
- Keep comments short and limited to non-obvious ordering or compatibility
  constraints.
- Prefer ASCII unless an existing file or user-facing value requires Unicode.

Do not edit generated or runtime state such as `elpa/`, `eln-cache/`, `.local/`,
`tree-sitter/`, or the active `custom.el`.

## Chezmoi

The chezmoi source is a separate worktree under
`$HOME/.local/share/chezmoi/`. When a task changes the managed user `cats` or
Custom workflow, inspect both the active file under `$HOME/.config/cat-emacs/`
and its chezmoi source.

Keep package declarations out of chezmoi data and templates. Do not run
`chezmoi apply` unless the user explicitly requests it. Check the chezmoi
worktree separately and preserve unrelated changes.

## Verification

This is a configuration project. Do not add ERT or other test files unless the
user explicitly changes this policy. Verify behavior with focused runtime and
static checks instead.

For Emacs Lisp edits:

1. Run `check-parens` on modified Lisp files.
2. Run `git diff --check`.
3. Perform a non-CI batch startup with `--debug-init`.
4. For core, module, package, or Docker changes, also perform a batch startup
   with `CI=true` and an empty temporary `XDG_CONFIG_HOME` so it uses
   `templates/cats`.

For package-management changes, also verify:

- the package manifest reaches `ready`;
- runtime selected packages equal the union of ELPA and VC roots;
- no VC root appears in `package--removable-packages`;
- simulated collection, sync, and upgrade failures restore prior state;
- simulated package selection saves do not create or modify a Custom file;
- rendered chezmoi Custom output contains no package declaration variables.

Do not run real package synchronization, upgrades, autoremove, Docker pushes,
or chezmoi apply merely to validate a refactor. These operations may download,
upgrade, or delete user state and require explicit task scope.
