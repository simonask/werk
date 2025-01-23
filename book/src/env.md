# Environment variables

All environment variables are forwarded to programs being run by `werk`, unless
a recipe overrides this behavior by adding/removing environment variables.

For details around color terminal output settings, see [Color terminal
support](./color.md).

## Environment variables read by Werk

- `WERK_LOG`: When set to a value, enables detailed logging. This takes a
  logging directive ([`RUST_LOG`
  conventions](https://docs.rs/tracing-subscriber/latest/tracing_subscriber/filter/struct.EnvFilter.html)).
  Overridden by the `--log` command-line option.
- `NO_COLOR`: Disable color output. Overridden by `--color=always`. Takes
  precedence over any other environment variables.
- `CLICOLOR`: When non-zero, enables color output. Overridden by
  `--color=always/never`, `NO_COLOR`, and `CLICOLOR_FORCE`.
- `CLICOLOR_FORCE`: When set, force-enable color output, same as
  `--color=always`. Overridden by `NO_COLOR` and `--color=never`.

## Environment variables set by Werk

- `CLICOLOR`, `CLICOLOR_FORCE`, `FORCE_COLOR`: These are set for programs
  executed by `werk` recipes when color output is enabled (running in a
  terminal, or color is enabled through environment variables, or
  `--color=always` is passed). Note that programs running through a [`shell`
  expression](./language/expressions.md#shell) never have color enabled.
- `NO_COLOR`: This is set for programs executed by `werk` when color output is
  _disabled_ (not running in a terminal, or color is disabled through
  environment variables, or `--color=never` is passed), and for all programs
  executed through a [`shell` expression](./language/expressions.md#shell)

## Modifying the environment in recipes

Recipes may add or remove environment variables for programs executed by that
recipe. Environment variables may be set or removed for a whole recipe, or
within a flow of [recipe commands](./language/recipe_commands.md).

Set or override an environment variable: `env "MY_VAR" = "..."`

Remove an environment variable, so it becomes unavailable to the child process:
`env-remove "MY_VAR"`.

Setting an environment variable in a recipe does _not_ impact the environment
variables seen by its dependencies or its dependents. Only processes executed by
that specific recipe will see modifications to the environment.

## Debugging

- `_WERK_ARTIFICIAL_DELAY`: Number of milliseconds to wait between executing
  recipe commands. This may be used while debugging `werk` itself, especially
  rendering to the CLI.
