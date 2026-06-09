# Compose Preview

Android Studio-style Jetpack Compose `@Preview` browsing from Emacs.

Paparazzi is used as the rendering engine because it can render Compose without
an emulator. The Emacs experience is centered on refreshing and viewing previews,
not on running UI tests. Snapshot record/verify commands remain available as
secondary Paparazzi utilities.

## Commands

- `M-x compose-preview-refresh`
  - refreshes previews silently for the current Android module and opens the
    image gallery for the current Kotlin buffer's `@Preview` functions.
- `C-u M-x compose-preview-refresh`
  - prompts for module and variant using android-mode's cached flavor data.
- `M-x compose-preview-open-results`
  - opens generated preview PNGs. From a Kotlin buffer, it filters to that
    buffer's previews; elsewhere it falls back to the module gallery.
- `M-x compose-preview-set-variant`
  - changes the default variant using android-mode's variant list when present.
- `M-x compose-preview-record`
  - secondary snapshot command: records Paparazzi golden images.
- `M-x compose-preview-verify`
  - secondary snapshot command: verifies Paparazzi golden images.

## How It Works

`compose-preview-refresh` runs Gradle in the background with a temporary init
script. The script injects Paparazzi into the current Android module, generates a
temporary scanner-backed preview runner, runs `test<Variant>UnitTest`, and
opens the resulting PNGs in an Emacs gallery buffer. Build output stays in
`*compose-preview-log*` and is shown only when refresh fails.

The gallery is source-focused: when refresh is launched from a Kotlin file, it
shows only previews declared in that buffer and labels each section with the
preview display name rather than the Paparazzi PNG filename.
Preview metadata comes from `AndroidComposablePreviewScanner`, not Emacs-side
annotation parsing, so custom multipreview annotations such as `@DevicePreview`,
`@PreviewBackground`, and AndroidX templates like `@PreviewScreenSizes` follow
the same discovery path as Android Studio-style previews.
When invoked from a Kotlin buffer, the gallery does not fall back to module-wide
images; if the scanner manifest cannot attribute a preview to that source file,
the command reports that no current-buffer previews were found.

The generated runner uses `AndroidComposablePreviewScanner`,
`TestParameterInjector`, and `AndroidPreviewScreenshotIdBuilder`, so preview
discovery is closer to Android Studio than a hand-written regex. Refresh passes
the current Kotlin file to the runner, so it scans with the same scanner path but
renders only previews attributed to that file.

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

Refresh uses Gradle, Kotlin, KSP, and build caches by default. If a project hits
stale generated state while previewing, temporarily set
`compose-preview-force-clean-build` to non-nil to run a slower clean-style
preview build.

## Notes

- Projects with product flavors usually need a full variant name, for example
  `demoDebug`, because Paparazzi creates tasks like `recordPaparazziDemoDebug`.
  compose-preview reuses android-mode's flavor cache and selection helpers.
- The selected module and variant are cached per Gradle project, so refreshes
  from Kotlin buffers, the preview gallery, or the log buffer reuse the same
  target until you select another one with `C-u M-x compose-preview-refresh` or
  `M-x compose-preview-set-variant`.
- If Gradle reports an ambiguous task such as `recordPaparazziDebug`, the Emacs
  command offers the candidate variants and retries with the selected one.
- Refresh runs the generated preview test through the normal unit-test task and
  reads Paparazzi's HTML report images. It does not record golden snapshots.
- `compose-preview-record` still uses `recordPaparazzi<Variant>`, so recorded
  golden snapshots use Paparazzi's normal `src/test/snapshots` location.
