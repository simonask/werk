# Migrating to v0.2

Major behavioral changes in v0.2:

1. Werk no longer distinguishes between "workspace" and "out-dir" directories.
   Instead, all paths are relative to the workspace directory. The motivation
   for this change is to make it easier to implement manually initiated
   workflows that generate output files that are committed to source control,
   such as `bindgen` output and the like. But it means that Werkfiles must now
   explicitly put build artifacts in the desired output directory (`/target` for
   Rust projects etc.). This change also means that the string interpolation ops
   `<...:out-dir>` and `<...:workspace>` no longer work, and will cause errors.

2. Werk no longer implicitly uses `<root>/target` as the location for the cache
   when no other directory is configured in the Werkfile or on the command-line.

3. All recipes are now fully evaluated before any build commands are executed.
   This catches errors earlier, and allows for an internal "planning" step that
   can produce more interesting output (like a `.dot` dependency graph), but it
   also removes most chances of having recipes interfere with each other's
   evaluation.

`werk` will fatally error if it detects a v0.1-style Werkfile containing a
`default out-dir = ...` directive, or if the `output-dir` option is passed on
the command-line.

Steps to migrate:

1. Change all build recipes to place their outputs in the desired output
   directory (`/target` for Rust projects etc.).

2. Change `default out-dir = ...` directives in your Werkfile to `default
   cache-dir = ...` to place the `.werk-cache` in the same location.
