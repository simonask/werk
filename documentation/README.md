# Werk Documentation

Werk is a command runner and simplistic build system. It is intended to supplant
Make in projects that need a simple build tool adjacent to a main build system,
such as Cargo or NPM.

The motivating use case is an asset building pipeline for a video game, which
must perform a series of expensive steps to produce an asset archive that can be
hot-reloaded by a game engine.

Werk is both limited and very opinionated. It is not suited for all use cases,
and it can only replace more advanced solutions, like CMake or scons, in a
limited number of scenarios. However, it _is_ suited to work together with such
systems, and can be used to invoke them in a convenient way.

Examples of suitable use cases:

- Simple build processes for things like shaders, WASM modules, small C
  libraries, assets, etc.
- Command runner for "housekeeping" tasks, like running tests, publishing
  binaries, or downloading static file dependencies.
- Driving other build systems.

Examples of less suitable use cases, for now:

- Building complicated C++ libraries with many dependencies.
- Building complicated projects using advanced features like cross-compilation.
- Builds requiring detailed concurrency management. Werk potentially runs all
  independent recipes in parallel, and there is no way to limit parallelism
  outside of the `--jobs` parameter.
- Multiple outputs per recipe. Driving `bison` with Werk may require
  workarounds.
- Recursive workspaces.

## Design Overview

`werk` parses a file (`Werkfile` or `werk.toml`) containing "recipes" for files
and user-defined "workflow tasks" (i.e., commands that are often run but don't
necessarily produce any files).

Recipes contain information about which commands to run in order to produce a
desired output. When given the name of a file, `werk` tries to find a recipe
that claims to be able to produce that file, and runs its commands. If a recipe
has dependencies, `werk` only runs the recipe commands when the output file is
"outdated" with respect to its dependencies.

`werk` tries to be really clever about when to rerun commands, and is
knowledgeable about file modification times, but also things like the path to
any commands invoked in a recipe, any environment variables used in the recipe,
or changes in the results of glob patterns (like `*.txt`).

Werkfiles are written in a very simple custom language, but can also be written
in TOML with a similar feature set. The latter is primarily provided as a way to
more easily generate werkfiles programmatically (mostly because writing complex
expressions using TOML tables is cumbersome).

## Key differences from Make

- Truly cross-platform: `werk` treats Windows as a first-class target platform.
- `werk` has a clear separation between "input" files (workspace) and "output"
  files (output directory). While outputs can be used as inputs in other
  recipes, `werk` will never produce files alongside input files.
- Globbing "just works". `werk` tracks the result of glob operations between
  runs and detects that a file is outdated if one of its dependencies was
  removed since the last run.
- No shell requirement: `werk` does not execute commands in a shell, but
  performs its own `$PATH` lookup etc., meaning all `Werkfile`s work natively on
  all platforms without needing a POSIX emulation layer.
- No shell (2): This also means that common POSIX shell utilities are not
  available. Most operations usually delegated to the shell can be performed
  using built-in language features instead, but explicitly invoking a shell is
  also an option.
- Build recipes and tasks recipes are separate things: No need for `.PHONY`
  targets.
- Language semantics matching modern expectations: `werk` distinguished between
  lists and strings, meaning that file names can contain spaces.
- No implicit rules.
- Automatic documentation. `werk --list` prints the list of available recipes
  and configuration variables to the command line, along with any preceding
  comment.
- Dry-run mode: Running `werk --dry-run` does not run any commands, but still
  goes through the process of figuring out which commands would have been run.
- Better debugging: Running `werk --explain` explains why a recipe was run. Also
  works with `--dry-run`.
- Better error messages. `werk` tries very hard to be helpful when an error
  occurs.

## Key differences from Just

- `werk` can build files in addition to running commands.
- No shell requirement. See above.

## Example

The following is a werkfile expressing the "standard" way to build a C program,
but also demonstrating global variables, the built-in `which` operation, the
built-in `glob` operation, string interpolation, and `match`. It also
demonstrates support for implicitly generated depfiles.

```werk
let clang = which "clang"
let ld = clang
let source-files = glob "*.c"

build "%.o" {
    # Prerequisites are specified with the `from` statement.
    from "{%}.c"

    # The depfile for this recipe.
    depfile "{%}.c.d"

    # Run the compiler.
    run "{clang} -MD -MF <depfile> -o <out> <in>"
}

build "program{EXE_SUFFIX}" {
    # Determine the object files by "piping" the `source-files` variable
    # through a pattern-matching operation.
    from source-files | match { "%.c" => "{%}.o" }

    # Run the linker.
    run "{ld} -o <out> <in*>"
}

task build {
    # As part of this task, build this recipe.
    build "program{EXE_SUFFIX}"

    # Print a nice command at the end.
    info "Build successful!"
}

task run {
    let executable = "program{EXE_SUFFIX}"
    build executable

    # Invoke the executable using its native OS path.
    run "<executable>"
}
```

Example command line usage (here on Windows, where `EXE_SUFFIX` is `.exe`):

```sh
$ werk build
[ ok ] /foo.c
[ ok ] /main.c
[ ok ] /program.exe
[info] Built successful!
[ ok ] build
```

## Details

- [Language Reference](language.md)
- [Abstract Paths](paths.md)
- [Outdatedness / staleness](outdatedness.md)
- [Depfile support](depfiles.md)
- [Built-in variables](builtins.md)
