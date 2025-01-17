# Recipe commands

When recipes execute, they run a series of recipe commands, defined by the `run`
statement in the recipe body.

The most common recipe command is to execute an external program, like a
compiler, but for convenience several other operations are available,
corresponding to common filesystem operations.

Recipe commands always occur as part of a `run` statement in a recipe. Shell
commands are _not_ [expressions](./expressions.md), and do not produce an output
value in the language. Instead, `werk` forwards their output to the user when an
error occurs, or unconditionally if `capture false` is set in the recipe.

Recipe commands are the only way to cause `werk` to make modifications to the
system, and they will never execute in `--dry-run` mode.  (The thing that
`--dry-run` disables is recipe commands.)

A recipe command is one of the following:

- A string literal, representing a command-line invocation of an external
  program (like a compiler).
- One of the built-in recipe commands below.

## The `run` statement

In any recipe, the `run` statement takes one of these forms:

- `run "command"`: Invoke `command` as an external program directly.
- `run { ... }`: Run each recipe command inside the braces `{ ...
  }`, in order.
- `run [ ... ]`: Run each external program in order.

The following three `run` statements are all equivalent:

```werk
run "command"

run ["command"]

run {
    shell "command"
}
```

**Note:** Recipe commands are always _literal_, i.e. they cannot be built from
arbitrary [expressions](./expressions.md), only strings. However, string
interpolation can be used within `run` statements to build command-line
invocations from other bits.

## String interpolation in `run` statements

Commands executed in recipes, or as part of the `shell` expression, have
additional logic in order to make it intuitive to build valid commands that can
be understood by the OS, loosely following normal shell conventions.

Commands in `run` statements are always string literals, and cannot be stored in
variables.

Strings in `run` statements are evaluated according to the following additional
rules:

- A command consists of segments separated by whitespace.
- The first segment is always the absolute path to an executable, such as
  obtained by the [`which` expression](./expressons.md#which). If the first
  segment does not look like an absolute OS path (depending on the current
  platform), the `which` operation is still performed before executing the
  command.
- Any subsequent segment is treated as an argument to the executable, and will
  be passed as-is. That is to say, when passing paths to files within the
  workspace as arguments, they must be interpolated using native OS path
  resolution: `<var>`.
- When building a command using a string literal, the string is parsed with
  sensitivity to whitespace and double-quotes. Whitespace outside quotes is
  understood to mean a separator between arguments (and multiple whitespace
  characters are collapsed into a single separator). Quoted arguments are always
  passed literally as a single argument to the command.
- String interpolation _inside_ quotes is pasted according to normal string
  interpolation rules, and passed as a single argument to the command. Note that
  since commands are also string expressions, quotes must be escaped.
- String interpolation _outside_ quotes is treated as a single argument,
  _except_ if the string interpolation contains a [join
  operator](./strings.md#join-interpolation) that would separate the strings by
  a single space character (the default), in which case each string is passed as
  a separate argument.
- String interpolations that evaluate to strings containing quotes do not affect
  argument separation - the quotation characters are passed verbatim to the
  command. This is also the only way to pass a literal quote character as an
  argument to the command.

Note that shell piping syntax is not available, since the command is _not_ run
via a shell process such as `sh` or `pwsh.exe`.

Examples:

```werk
let compiler = "clang"
let input = "foo.c"
let output = "foo.o"
let cflags = ["-c", "-O0", "-g"]
run "{compiler} {cflags*} -o <output> <input>"

# Command that will be run:
# "c:\path\to\clang.exe" "-c" "-O0" "-g" "-o" "c:\workspace\output\foo.o" "c:\workspace\foo.c"
```

## Auto-`which`

When the first component of an external program invocation in a `run` statement
is a literal string, it is automatically [`which`ed](./expressions.md#which),
meaning that `werk` automatically looks up the actual path to the program (via
the `PATH` environment variable), and adds that path to [outdatedness
checks](../outdatedness.md) for the recipe containing the `run` statement.

Example:

```werk
run "clang -o <out> <in>"   # automatically resolves to (e.g.) /usr/bin/clang
```

The reason you might want to not rely on this, and use a global variable
instead, is that global variables can be overridden by the user on the command
line, providing a different compiler path.

Example:

```werk
# in global scope, overridable with `-Dcc=path` on the command-line
let cc = which "clang"

# in a recipe
run "{cc} -o <out> <in>"
```

## Built-in recipe commands

## `shell`

Invoke an external program. In `run` statements, this behaves the same as a
string literal.

**Note:** Within `run` statements, this behaves differently from [`shell`
expressions](./expressions.md#shell). The output of the command is not captured
as a string, but instead forwarded to the user, and there are no requirements
that the output is valid UTF-8.

Syntax:

```werk
shell <string-expr>
```

Example:

```werk
run {
    shell "clang -o <out> <in>"
}
```

## `write`

Write a string to a file in the output directory.

The destination path is an [abstract path](../paths.md) that will be resolved
relative to the output directory. This command will never overwrite any file
outside of the output directory.

If the destination path refers to a directory, this command fails.

Syntax:

```werk
write <expression> to <filename>
```

Example:

```werk
build "message.txt" {
    let message = "Hello, World!"
    run {
        write message to "<out>"
    }
}
```

## `copy`

Copy files to a file or directory path in the output directory.

The destination path is an [abstract path](../paths.md) that will be resolved
relative to the output directory. This command will never overwrite any file
outside of the output directory.

The source path may refer to files or directories in the output directory or in
the workspace.

**Note:** The source-path does _not_ automatically participate in [outdatedness
checks](../outdatedness.md) for the recipe. The dependency must be established
in some other way, like a `from` statement in a build recipe, or the result of a
[`glob` expression](./expressions.md#glob).

Syntax:

```werk
copy <source-path> to <destination-path>
```

Example:

```werk
build "b.txt" {
  from "a.txt"
  run {
    copy "a.txt" to "b.txt"
  }
}
```

## `delete`

Delete a file/directory or list of files/directories under the output directory,
if they exist.

If the file does not exist, this silently succeeds (same as `rm -rf`).

**Note:** This will never touch any file outside the output directory.

Syntax:

```werk
delete <paths>
```

Example:

```werk
task clean-objects {
  let files = glob "*.c" | map "{}.o"
  run {
    delete files
  }
}
```

## `info`

Print an informational message while running commands.

Example:

```werk
run {
    info "Build succeeded!"
}

# Shorthand in a recipe body:
info "Build succeeded!"
```

## `warn`

Print a warning while running commands.

Example:

```werk
run {
    warn "Caution!"
}

# Shorthand in a recipe body:
warn "Caution!"
```
