# Compose Preview

Preview Jetpack Compose `@Preview` functions from Emacs using
[Paparazzi](https://github.com/cashapp/paparazzi).

The package runs Gradle asynchronously with a temporary init script. The script
injects Paparazzi into the current Android module, generates a temporary
parameterized JUnit test that scans `main` source-set `@Preview` composables,
runs the Paparazzi task, and opens the generated PNG snapshots in an Emacs
gallery buffer.

## Commands

- `M-x compose-preview-record`
  - runs `recordPaparazzi<Variant>` for the current Android module.
- `M-x compose-preview-verify`
  - runs `verifyPaparazzi<Variant>` for the current Android module.
- `C-u M-x compose-preview-record`
  - prompts for module and variant using android-mode's cached flavor data.
- `M-x compose-preview-set-variant`
  - changes the default variant using android-mode's variant list when present.
- `M-x compose-preview-open-results`
  - opens existing Paparazzi PNG outputs for the current module.
- `M-x compose-preview-gallery`
  - compatibility alias for `compose-preview-record`.

## Configuration

```elisp
(setq compose-preview-default-variant "debug"
      compose-preview-paparazzi-version "2.0.0-alpha02"
      compose-preview-image-width 420)
```

Set `compose-preview-disable-ksp2` to non-nil only for projects that still need
KSP1. Recent KSP versions fail configuration when `ksp.useKSP2=false` is passed.

`compose-preview-use-legacy-android-dsl` defaults to non-nil because Paparazzi
`2.0.0-alpha02` still needs AGP's legacy Android extension for resource tasks in
AGP 9 projects.

## Notes

- This is aimed at Android modules using Jetpack Compose.
- Projects with product flavors usually need a full variant name, for example
  `demoDebug`, because Paparazzi creates tasks like `recordPaparazziDemoDebug`.
  compose-preview reuses android-mode's flavor cache and selection helpers for
  these variants.
  If Gradle reports an ambiguous task such as `recordPaparazziDebug`, the Emacs
  command will offer the candidate variants and retry with the selected one.
- The generated test uses
  `AndroidComposablePreviewScanner`, `TestParameterInjector`, and
  `AndroidPreviewScreenshotIdBuilder` so each discovered preview becomes one
  Paparazzi snapshot.
- The scanner reads previews from the module's `main` source package tree, which
  matches the user-facing Android Studio preview model.
- Paparazzi `2.0.0-alpha02` configures successfully on an AGP 8.12 project in
  local testing. AGP 9 projects may need a newer Paparazzi version once one is
  available.
- Paparazzi's record task writes snapshots using Paparazzi's normal Gradle
  output locations, commonly under `src/test/snapshots` for recorded snapshots
  and `build/paparazzi` for failures.
