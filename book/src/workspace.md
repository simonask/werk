# Workspace

The workspace is the directory containing the `Werkfile`, minus any files and
directories mentioned by `.gitignore`.

When writing [build recipes](./build_recipes.md), the dependencies of a build
recipe may be references to files within the workspace, or they may be
referencing the output of another recipe, which will exist in the output
directory.

## Output directory

The output directory is where files produced by `werk` will be placed. The
default path is `$WORKSPACE/target` (same as Cargo), but this can be overridden
in two ways:

* From within the Werkfile: `default out-dir = ".."`
* From the command-like: `werk --output-dir=..`

If `werk` detects that an output directory is included in the workspace (i.e.,
it is not covered by `.gitignore`), it will emit a hard error.
