# Compose Preview

This Emacs package allows you to preview all of your Jetpack Compose `@Preview` functions in a single, scrollable desktop window.

This package is non-intrusive. It does **not** require you to copy any files into your project or modify any of your build files.

## Installation

1.  Add this directory to your Emacs `load-path`.
2.  Add `(require 'compose-preview)` to your `init.el`.

## Usage

1.  Open any file in your Android project.
2.  Run the command `M-x compose-preview-gallery`.

A new window will open, displaying all of your `@Preview` composables.