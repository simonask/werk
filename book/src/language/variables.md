# Variables

Defining a variable in Werk is a `let`-statement. Values are either strings or
lists (no numbers). Lists may contain other lists. All strings are valid UTF-8.

Syntax: `let identifier = expression`. The left-hand side of the `=` must be a
valid identifier (Unicode is supported), and the right-hand side is any
expression. All expressions produce a value.

```werk
let my-string = "value"

let my-list = ["a", "b", "c"]
```

All variables are immutable, but variable names may shadow local or global
variables with the same name.

```werk
let foo = "a"
let foo = "b"   # valid

let bar = foo   # == "b"
```

## Global variables are public

Variables defined at the global scope (i.e., outside of any recipe) are public,
and will appear in the output of `werk --list`. They can be overridden by
passing `-Dkey=value` on the command-line. Comments immediately preceding a
global variable will appear in the output as documentation for that variable.

```werk
# Set the build profile.
let profile = "debug"
```

```sh
$ werk --list
Global variables:
    profile = "debug" # Set the build profile.
```

## Local variables

Variables defined within a recipe are local to that recipe. Recipes cannot
change global variables, but they may shadow global variables in their local
scope by defining a variable with the same name as a global variable.
