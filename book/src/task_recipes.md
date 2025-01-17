# Task recipes

Task recipes are "housekeeping tasks" or "workflows" that you may frequently
want to run. They have the same role as `.PHONY` targets (Make) and tasks in
`just`.

When a task recipe has one or more `run` statements, the recipe will execute
[recipe commands](./language/recipe_commands.md) when invoked.

Task recipes can depend on each other, and they can depend on build recipes. If
a task recipe participates in any [outdatedness check](./outdatedness.md), it
and all of its dependents is considered outdated.

A single task is only ever run once during a build (occupying a single node in
the dependency graph). In other words, if multiple recipes are being executed
that depend on the same task recipe, that recipe will be executed exactly once,
before any of the recipes that depend on it.

## Reference

```werk
task my-task {
    # Define a local variable, here indicating a build recipe to run.
    let my-program-target = "my-program"

    # Run tasks or build recipes before this task. May be a list or a single name.
    build "my-program"

    # Enable forwarding the output of executed commands to the console.
    capture false

    # Run an external program after building this task's dependencies.
    run "echo \"Hello!\""

    # Can also run a block of commands.
    run {
        "echo \"Hello!\""
        "some-other-command"
        info "my-task completed!"
    }
}
```
