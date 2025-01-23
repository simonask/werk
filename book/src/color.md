# Color support

`werk` automatically detects whether or not it is running in a terminal, and
[respects conventional color support environment variables](./env.md).

Since `werk` captures the stdout/stderr of child processes, programs executed by
`werk` cannot detect that they are running in a terminal, so the only way to get
them to produce color output is to instruct them via environment variables or
command-line arguments.

`werk` automatically adjusts the color environment variables for child
processes such that child processes get a consistent idea of whether or not
color output should be enabled. For example, `CLICOLOR=1` and `NO_COLOR=1` will
never both be set for a child process.

Some programs do not respect these conventional environment variables, and must
manually be passed command-line arguments to enable color output. For example,
`clang` must be run with `-fcolor-diagnostics -fansi-escape-codes` to produce
color output when run through `werk` on all platforms. The [built-in global
variable `COLOR`](./language/builtins.md) can be used to conditionally pass such
arguments to compilers when `werk` itself has color output enabled.

## Example

```werk
let color-cflags = COLOR | match {
    "1" => ["-fcolor-diagnostics", "-fansi-escape-codes"]
    "%" => []
}
let cflags = ["-O0", "-g", color-cflags]
```

## Progress indicator

When `werk` detects that it is running in a terminal, and color is not disabled
through environment variables or command-line options, it will print and update
a progress indicator (spinner or progress bar, depending on settings) to the
terminal.

## Windows support

`werk` only supports ANSI colors and automatically attempts to set
`ENABLE_VIRTUAL_TERMINAL_PROCESSING` on Windows 10 and above. Legacy Windows
Console color is _not_ supported, so child processes should also be instructed
to emit ANSI color codes, such as passing `-fansi-escape-codes` to `clang`.
