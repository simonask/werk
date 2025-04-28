# Long-running tasks

Task recipes can spawn long-running processes controlled by `werk` using [the
`spawn` statement](./language/recipe_commands.md#the-spawn-statement). This is
useful for running a development server or other long-running processes that
need to be restarted when the source files change.

When a `spawn` statement has executed, `werk` will wait for the process to exit
before exiting itself. When `werk` receives a Ctrl-C signal, it will kill the
child process as well.

## Autowatch integration

When `--watch` is enabled, `werk` will automatically kill and restart any
spawned processes when a rebuild is triggered.

<div class="warning">
<strong>Note:</strong> Some programs, such as local webservers, implement their
own watching mechanism. Using these in conjunction with `--watch` may not be desirable,
because `werk` will unconditionally restart the process on any change.
</div>
