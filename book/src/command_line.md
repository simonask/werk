# Command-line reference

Output of `werk --help`:

```plain
Usage: werk.exe [OPTIONS] [TARGET]

Arguments:
  [TARGET]
          The target to build

Options:
  -f, --file <FILE>
          The path to the Werkfile. Defaults to searching for `Werkfile` in the current
          working directory and its parents

  -l, --list
          List the available recipes

      --dry-run
          Dry run; do not execute any recipe commands. Note: Shell commands used
          in global variables are still executed!

      --print-commands
          Print recipe commands as they are executed. Implied by `--verbose`

      --print-fresh
          Print recipes that were up-to-date. Implied by `--verbose`

      --no-capture
          Forward the stdout of all executed commands to the terminal, even when
          successful. Stderr output is always forwarded. Implied by `--verbose`

      --explain
          For each outdated target, explain why it was outdated. Implied by `--verbose`

  -v, --verbose
          Shorthand for `--explain --print-commands --print-fresh --no-capture`

      --color <COLOR>
          [default: auto]

          Possible values:
          - auto:   Probe the current terminal and environment variables for color
                    support
          - always: Force color output, even if the command is not running in a
                    terminal
          - never:  Do not use color output

      --output-format <OUTPUT_FORMAT>
          [default: auto]

          Possible values:
          - ansi: Provide friendly user feedback assuming an ANSI terminal
          - log:  Emit the progress as log statements (assuming `WERK_LOG` is set
                  to a value)
          - json: Report progress as JSON to stdout. This also disables color output

  -j, --jobs <JOBS>
          Number of tasks to execute in parallel. Defaults to the number of CPU cores

      --workspace-dir <WORKSPACE_DIR>
          Override the workspace directory. Defaults to the directory containing
          Werkfile

      --output-dir <OUTPUT_DIR>
          Use the output directory instead of the default

  -D, --define <DEFINE>
          Override global variable. This takes the form `name=value`.

      --log [<LOG>]
          Enable debug logging to stdout.

          This takes a logging directive like `RUST_LOG`.

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version
```
