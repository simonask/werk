# Build configuration

## Configure how werk runs

The behavior of the `werk` command can be configured in two ways:

1. Command-line arguments
2. `default` statements within the Werkfile

`default` statements take precedence over built-in defaults. Command-line
arguments take precedence over `default` statements.

Reference:

```werk
# Set the output directory, relative to the workspace root. Default is "target".
default out-dir = "output-directory"

# Set the default recipe to run when werk is run without arguments.
default target = "recipe-name"
```

## Customize your tasks and recipes

Build configuration variables in a Werkfile can be overridden from the
command-line using `-Dkey=value`, where `key` is the name of a `config key =
...` statement in the global scope, and `value` is a string value.

`config` statements work exactly like `let` statements, except that it is an
error if multiple identical `config` keys exist in the Werkfile. `let`
statements may shadow `config` statements and vice versa, but `config`
statements cannot shadow other `config` statements.

When a `config` variable is overridden from the command line, the value is
inserted during evaluation at the point where the `config` statement occurs, and
subsequent statements will see the value passed on the command-line.

Consider this Werkfile:

```werk
config greeting = "Hello"

task greet {
    info "{greeting}, World!"
}
```

Running this normally:

```sh
$ werk greet
[info] Hello, World!
[ ok ] greet
```

Override the greeting:

```sh
$ werk greet -Dgreeting=Goodbye
[info] Goodbye, World!
[ ok ] greet
```

A typical use case for this is to override the build "profile", i.e., whether to
build in debug or release mode.

Example using the [`match expression`](./language/operations.md#match) to
validate the argument:

```werk
config profile = "debug"
let cflags = profile | match {
    "debug" => ["-O0", "-g"]
    "release" => ["-O3"]
    "%" => error "unknown build profile '{profile}'"
}
```

Running this with default arguments:

```sh
$ werk --list
Config variables:
  profile = "debug"
```

Overriding the argument:

```sh
$ werk --list -Dprofile=release
Config variables:
  profile = "release"
  cflags  = ["-O3"]
```

Overriding the argument with an invalid value:

```sh
$ werk --list -Dprofile=wrong
Error: unknown build profile 'wrong'
```
