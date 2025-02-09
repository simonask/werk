# Language Reference

Werkfiles are written in a very simple domain-specific language, optimized for
readability and debuggability. It is a Turing-_incomplete_ language: there are
no loops or functions.

- Comments begin with `#` and go until the end of the line. Comments may appear
  anywhere.
- Source files (Werkfile) consist of a sequence of statements.
- All statements begin with a keyword, like `let`, `run`, `task`, `build`, etc.
- Statements are separated by newlines and/or a semicolon.
- Some statements accept a `{ ... }` block containing other statements (`run`
  blocks, `task` and `build` recipes).
- Identifiers can contain any Unicode XID character, plus `-` (kebab-case is
  supported).
- Strings and patterns are always double-quoted.
- Lists are surrounded by `[ ... ]`, and elements are comma-separated.
- There are no functions or loops, but expressions can be "chained" or "piped"
  through [operations](./language/operations.md) using the `|` operator.
- All variables are immutable - there is no assignment operation.
- Local variables may shadow global variables or previously defined local
  variables in the same scope.

## Statements

### `let` statement

When appearing at the root scope, this defines a global variable available to
all recipes. When appearing in a recipe, this defines a local variable to that
recipe. Local variables may shadow global variables.

Syntax:

```werk
let <identifier> = <expression>
```

Example:

```werk
let message = "Hello, World!"
```

### `config` statement

Defines a global configuration variable available to all recipes, overridable
from the command-line with `-Dkey=value`.

`config` statements semantically work exactly like `let` statements, except that
it is an error if multiple `config` statements define the same variable. In
other words, `config` statements cannot shadow each other, but it _is_ valid for
`config` statements to shadow `let` variables, and `let` variables may shadow
`config` variables.

When a command-line override `-Dkey=value` is present, `value` is inserted into
the evaluation at the point where the `config` statement occurs, and the default
value expression is not evaluated.

Cannot appear in recipes.

Syntax:

```werk
config <identifier> = <default-value-expression>
```

Example:

```werk
config message = "Hello, World!"
```

### `default` statement

Sets project-level settings for the workspace, providing default values for
command-line arguments. May only appear in the global scope. Expressions are not
supported as values, and string interpolation does not happen, except for the
`target` key.

Syntax:

```werk
default <key> = <value>
```

Example:

```werk
# Set the output directory for the workspace.
default out-dir = "path/to/output/directory"

# Set the recipe to run when `werk` is run without arguments.
default target = "build"
```

### `task` statement

Define a "workflow task", invokable from the command-line. May only appear in
the global scope.

Syntax:

```werk
task <identifier> {
    # Set a local variable
    let <identifier> = <expression>

    # Build recipe(s)
    build <expression>

    # Run command(s)
    run <run-expression>

    # Print a message
    info <expression>
}
```

Example:

```werk
task build {
    let program = "my-program{EXE_SUFFIX}"
    build program
    info "Build succeeded!"
}
```

### `build` statement (at global scope)

Define a recipe for building a file. May only appear in the global scope.

Syntax:

```werk
build <pattern> {
    # Set a local variable
    let <identifier> = <expression>

    # Set the prerequisites for this recipe
    from <expression>

    # Set a depfile for this recipe
    depfile <expression>

    # Run command(s)
    run <run-expression>
}
```

Example:

```werk
build "%.o" {
    let source-file = "{%}.c"
    from source-file
    depfile "{%}.d"
    run "clang -c -MF <depfile> -o <out> <in>"
}
```

## Expressions

An expression is one of:

- String literal `"..."`, potentially containing interpolations.
- List literal `[...]`, comma-separated.
- A single identifier, referencing a variable in the local or global scope.
- An [operation](./language/operations.md) `<operation-name> <args>`.
- An expression chain (see [below](#expression-chaining--piping)).

## Expression chaining / "piping"

There are no functions in werk, but expressions can be chained together, such
that one expression works as the "input" to an operation. For example, an
expression that evaluates to a list can be passed to the `join ", "` operator to
produce a string where each element of the list is separated by a comma.

The chaining operator is `|`, as a nod to shell piping syntax. Expression chains
can be arbitrarily long.

Syntax:

```werk
<first> | <then> | <last>
```

Example:

```werk
let words = ["Hello", "World"]
let message = words | join ", "

# Prints "Hello, World!"
info message
```
