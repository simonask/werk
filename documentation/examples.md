# Examples

These are some examples of how to use Werk.

## Simple C or C++ project

The following is a Werkfile expressing the "standard" way to build a C program.
It also demonstrates global variables, the built-in `which` operation, the
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
