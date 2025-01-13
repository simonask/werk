# Language Reference

Werk supports the following builtin command line options.

## Builtin Commands 

### Target
`--target`
Prints the target to build.

### file
`--file`
The path to the werk file. Defaults to searching for `werk.toml` in the
working dir and its parents.

### list
`--list`
List the available targets.

### dry-run
`--dry-run`
Dry run; do not execute any recipe commands. Note: Shell commands used
in global variables are still executed!

### print-commands
`--print-commands`
Print recipe shell commands as they are executed. Implied by `--verbose`.

### print_fresh
`--print_fresh`
Print build targets that were not rebuilt because they were up-to-date.
Implied by `--verbose`.

### no-capture
`--no-capture`
Forward the `stdout` of all executed commands to the terminal, even when
successful. `stderr` output is always forwarded. Implied by `--verbose`.

### explain
`--explain`
For each outdated recipe, explain why it was outdated. Implied by `--verbose`.

### verbose
`--verbose`
Shorthand for `--explain --print-commands --print-fresh --no-capture`.
  
### color
`--color <string-option>`
Sets the application color mode. 

- `auto     #default`
  Probe the current terminal and environment variables for color support.
- `always`
  Force color output, even if the command is not running in a terminal.
- `never`
  Do not use color output.


### output-format
`--output-format <string-option>`
Terminal output mode.
- `auto    #default`
  Choose the best output format for the current terminal.
- `log`
  Emit the progress as log statements (assuming `WERK_LOG` is set to a value).
- `json`
  Report progress as JSON to stdout. This also disables color output.

### option
`--option <usize option>`
Number of tasks to execute in parallel. Defaults to the number of CPU cores.

### workspace-dir
`--workspace-dir`
Override the workspace directory. Defaults to the directory containing werk.toml.

### output-dir
`--output-dir <path-option>`
  Use the output directory instead of the default.

### define
`--define <string-option>`
Override variable in the werk.toml's `[global]` section. This takes the form `name=value`.


### log
`--log`
Enable debug logging to stdout.

The value is a logging directive (same syntax as the conventional
`RUST_LOG` environment variable), so it can be a log level like "info"
or "trace", or a module path like "werk_runner=debug". If passed without
a directive string, this enables logging at the "info" level for only
the `werk` runner.

