# Why not make?

GNU Make is an incredibly powerful tool that has proven its worth through \~50
years of reliable use. But its age is also showing, and its behavior is often
surprising to people who are used to more modern tools.

Put simply, it solves many hard problems that I don't have, and it doesn't solve
many of the easy problems I _do_ have.

The most glaring problem with Make is that it does not work well on Windows. It
can be made to work, but not portably - often projects will have specific
Makefiles for each platform, which must be maintained in isolation.

## Key differences from Make

- Truly cross-platform: `werk` treats Windows as a first-class target platform.
- `werk` has a clear separation between "input" files
  ([workspace](./workspace.md)) and "output" files (output directory). `werk`
  will never produce files alongside input files.
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
