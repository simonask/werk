# Watch for changes

`werk` can automatically watch for changes to the [workspace](./workspace.md)
and re-run a task or build recipe when a change occurs.

In any project, run `werk <target> --watch` to build a target and then wait for
changes to any files in the workspace to trigger a rebuild.

Only files in the workspace are watched for changes - not changes in the output
directory, or changes to files covered by `.gitignore`.

`werk` inserts a small delay between detecting a file and actually starting a
rebuild, to avoid "overreacting" when many changes occur, and also because some
filesystem notifications are actually delivered before the change is visible.
This delay can be customized using `--watch-delay=<ms>`.

`--watch` works together with other flags, like `--explain`, to provide detailed
information about the build.

## Example

Using [the C example](./examples/c.md), this will build and re-run the
executable for every change.

```shell
$ werk run --watch --explain
[ ok ] /my-program.exe
foo() returned: 123
[ ok ] run
[werk] Watching 4 files for changes, press Ctrl-C to stop
```

Making a change to any of the files involved in the build will then cause a
rebuild. Let's say a change was made to `foo.h`, which is included by other
files:

```shell
[0/1] rebuilding `/foo.o`
  Cause: `/foo.h` was modified
[0/1] rebuilding `/main.o`
  Cause: `/foo.h` was modified
[ ok ] /foo.o
[ ok ] /main.o
[0/1] rebuilding `my-program.exe`
  Cause: `/foo.o` was rebuilt
  Cause: `/main.o` was rebuilt
[ ok ] /my-program.exe
foo() returned: 123
[ ok ] run
[werk] Watching 4 files for changes, press Ctrl-C to stop
```
