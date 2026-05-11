# Workspace

The workspace is the directory containing the `Werkfile`, minus any files and
directories mentioned by `.gitignore`.

When writing [build recipes](./build_recipes.md), the dependencies of a build
recipe may be references to files within the workspace, or they may be
referencing the output of another recipe, which will exist in the output
directory.

## Cache directory

The cache directory is where Werk stores its internal cache. It can be set in
two ways:

* From within the Werkfile: `default cache-dir = ".."`
* From the command-line: `werk --cache-dir=..`
