# Build configuration

## Configure how werk runs

The behavior of the `werk` command can be configured in two ways:

1. Command-line arguments
2. `config` statements within the Werkfile

`config` statements take precedence over built-in defaults. Command-line
arguments take precedence over `config` statements.

Reference:

```werk
# Set the output directory, relative to the workspace root. Default is "target".
default out-dir = "output-directory"

# Set the default recipe to run when werk is run without arguments.
default target = "recipe-name"
```

## Customize your tasks and recipes

Global variables in a Werkfile can be overridden from the command-line using
`-Dkey=value`, where `key` is the name of a `let key = ...` statement in the
global scope, and `value` is a string value.

Consider this Werkfile:

```werk
let greeting = "Hello"

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
let profile = "debug"
let cflags = profile | match {
    "debug" => ["-O0", "-g"]
    "release" => ["-O3"]
    "%" => error "unknown build profile '{profile}'"
}
```

Running this with default arguments:

```sh
$ werk --list
Global variables:
  profile = "debug"
  cflags  = ["-O0", "-g"]
```

Overriding the argument:

```sh
$ werk --list -Dprofile=release
Global variables:
  profile = "release"
  cflags  = ["-O3"]
```

Overriding the argument with an invalid value:

```sh
$ werk --list -Dprofile=wrong
Error: unknown build profile 'wrong'
```
