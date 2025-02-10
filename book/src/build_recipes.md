# Build recipes

Build recipes tell `werk` how to produce a file. They are the equivalent of Make
rules.

Build recipes are defined in terms of a [pattern](./language/patterns.md), which
may just be the literal name of a file, but it can also be Make-like patterns
with a substitution stem.

When a build recipe has one or more `run` statements, the recipe will execute
[recipe commands](./language/recipe_commands.md) when invoked in order to
produce the output file.

Build recipes may depend on each other. When multiple files depend on the same
recipe, that recipe is only executed exactly once (before any of its dependents
are built).

When a target is outdated, it and all of its dependents will be rebuilt. See the
[outdatedness](./outdatedness.md) chapter for the detailed rules governing when
targets are rebuilt.

Build recipes should always place their output in the output directory. This can
be achieved by using [path interpolation](./language/strings.md#paths)
(`"<...>"`) when passing files as arguments to external commands.

## Reference

This example builds an `.o` object file from a `.c` source file. See
[Patterns](./language/patterns.md) for more information about which patterns are
supported.

```werk
build "%.o" {
    # Define a local variable, here setting the name of the source file.
    let source-file = "%.c"

    # Define the dependencies of this recipe. May be a list or a single value.
    from source-file

    # Set the depfile for this recipe.
    depfile "{source-file:.c=.d}"

    # Disable forwarding the output of executed commands to the console.
    # Default is to capture (silence) in build recipes. Note that errors and warnings
    # from compilers are always forwarded.
    capture true

    # Set an environment variable for all child processes in this recipe.
    env "MY_VAR" = "value"

    # Remove an environment variable for all child processes in this recipe.
    env-remove "MY_VAR"

    # Run an external program to build the file.
    # out is the target file of the recipe, and in is the first dependency.
    run "clang -c -o <out> <in>"
}
```
