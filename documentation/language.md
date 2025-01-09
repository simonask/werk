# Language Reference

This is the language reference for Werk.

The rules for string interpolation also apply to TOML manifests.

## Basic syntax

- Comments begin with `#` and go until the end of the line. Comments may appear
  everywhere.
- All statements begin with a keyword, like `let`, `run`, `task`, `build`, etc.
- Statements are separated by newlines and/or a semicolon.
- Some statements accept a `{ ... }` block containing other statements (`run`,
  `task`, `build`).
- Identifiers can contain any Unicode XID character, plus `-` (kebab-case is
  supported).
- Strings and patterns are always double-quoted.
- Lists are surrounded by `[ ... ]`, and elements are comma-separated.
- There are no functions or loops, but expressions can be "chained" using the
  `=>` operator.

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

Sets project-level settings for the workspace. May only appear in the global
scope. Expressions are _not_ supported as values, and string interpolation does
not happen.

Syntax:

```werk
config <identifier> = <value>
```

Example:

```werk
# Set the output directory for the workspace.
config out-dir = "path/to/output/directory"

# Set the recipe to run when `werk` is run without arguments.
config default-target = "build"
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
- An operation `<operation-name> <args>` (see [built-in
  operations](#built-in-operations)).
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

### Chaining through a string expression

When a string literal appears in an expression chain, the string replaces the
left-hand side of the chaining expression. If the left-hand side of the chain
produces a list, the string expression recursively replaces all strings in the
list.

This is useful for applying [string interpolation
operations](#interpolation-operations) to all strings in a list, such as a regex
substitution, changing a file extension, or resolving native OS paths.

Each string in the left-hand expression is passed to the string expression as
the "implicit" value.

Example:

```werk
let source-files = ["foo.c", "main.c"]
let object-files = source-files | "<:.c=.o>" | join ", "

# Prints "c:\workspace\output\foo.o, c:\workspace\output.main.o"
info object-files
```

## Built-in operations

### `match` expression

Perform string [pattern-matching](#patterns-and-pattern-matching) and
substitution. The "input" to the match operation is whatever value is piped into
it, which is to say, `match` can only meaningfully appear as the right-hand side
of a chaining expression.

If the "input" value is a list, the pattern substitution is performed
recursively. If no patterns match the input, the string is passed through
unchanged. The catch-all pattern `"%"` can be used to provide a fallback,
potentially using an [`error`](#error-expression) operation to fail early.

Syntax:

```werk
match {
    <pattern> => <expression>
    <pattern> => <expression>
}
```

### `join` expression

Given a list of values, convert the list to a string (recursively), where each
element is separated by a separator.

The "input" to the operation is whatever value it receives via expression
piping.

Syntax:

```werk
join <separator>
```

Example:

```werk
let cflags = ["-O0", "-g"]
let arguments = cflags | join " "
```

### `split` expression

Given a string, convert it to a list by splitting it by some separator.

See also [`split-pattern`](#split-pattern-expression) and
[`lines`](#lines-expression).

### `split-pattern` expression

Given a string, convert it to a list by splitting it by some separator
identified by a pattern.

### `lines` expression

Given a string, convert it to a list by splitting it into separate lines.

This is similar to `split "\n"`, except it also handles CRLF line breaks.

### `which` expression

Look for a program name using the `PATH` environment variable, and produce the
absolute OS path to the program's executable as a string.

This expression fails if the executable could not be found, or if the path to
the executable cannot be represented as valid UTF-8.

### `glob` expression

Evaluate a glob pattern of workspace files and produce a list of matching files.
The pattern syntax is not a werk pattern, but any syntax accepted by the
`globset` crate, corresponding to the conventions of a POSIX shell.

Note that the glob pattern syntax collides with string interpolation syntax in
some cases, so the curly braces of a `{a,b}` pattern group must be escaped:
`\{a,b\}`.

### `shell` expression

Run a command during evaluation and produce the commands `stdout` as a string.

This expression fails if the command could not be run, or if it produced output
that is not valid UTF-8.

Note that executing a `shell` expression in a [build
recipe](#build-statement-at-global-scope) implicitly causes the resolved path to
the command to participate in the outdatedness of the recipe.

**Important:** `shell` expressions in the global scope cause the command to be
run even in `--dry-run` mode, as well as in `--list` mode. In general,
well-behaved werkfiles only use this expression to gather information about the
system or the workspace, such as obtaining the SHA1 of the git HEAD, querying
the current version from a `Cargo.toml` file, or similar non-destructive
operations.

### `read` expression

Read a file during evaluation and produce its contents as a string.

This expression fails if the file does not exist, or if its content is not valid
UTF-8.

Note that executing a `read` expression in a [build
recipe](#build-statement-at-global-scope) implicitly adds the file as a
dependency of the recipe - changing the file causes the recipe to become
outdated.

### `write` operation

Write a string to a file.

This can only be used in `run` statements in recipes.

Syntax:

```werk
write <expression>, <filename>
```

Example:

```werk
build "message.txt" {
    let message = "Hello, World!"
    run {
        write message, "<out>"
    }
}
```

### `info` expression

Print an informational message. This can appear within an expression chain to
introspect values along the way.

Syntax:

```werk
info <string-expression>
```

### `warn` expression

Print a warning. This can appear within an expression chain.

Syntax:

```werk
warn <string-expression>
```

### `error` expression

Unconditionally fail evaluation. If this is reached during evaluation in the
global scope, the workspace will fail to initialize. If this is reached in a
recipe scope, the recipe will fail to build.

In general, `error` expressions only meaningfully appear within `match`
expressions, but syntactically they can appear anywhere. For example, it may be
useful to do "printf-style debugging" of a werkfile by unconditionally failing
early.

Syntax:

```werk
error <string-expression>
```

Example:

```werk
let profile = "debug"
let cflags = profile | match {
    "debug" => "-O0"
    "release" => "-O3"
    "%" => error "Invalid profile: {profile}. Valid values are \"debug\" and \"release\"."
}
```

## Built-in variables and constants

See [built-in variables](builtins.md).

## Patterns and pattern matching

Patterns are strings containing special directives. They behave similarly to Make patterns.

Patterns can contain [string interpolations](#string-interpolation-syntax).
Interpolated string values are not interpreted as patterns, but will be matched
literally.

Special syntax in pattern strings:

- `%`: The "pattern stem". This matches any sequence of characters, which will
  be available to subsequent statements as `{%}`.
- `(a|b)`: Capture group matching either `a` or `b`.

When multiple patterns are participating in pattern matching (such as figuring
out which [build recipe](#build-statement-at-global-scope) to run, or in a
[`match` expression](#match-expression)), the "highest-quality" match is chosen.
Match quality is measured by the length of the stem: A match producing a shorter
stem is considered "better" than a match producing a longer stem.

**Important:** When multiple patterns match with equal quality, the pattern
matching operation is ambiguous, and a hard error is emitted. Often, capture
groups can be used to disambiguate two patterns by collapsing them into a single
pattern.

## String interpolation syntax

Strings literals "as expected", honoring normal character escape rules.

Additionally, strings can contain interpolations, i.e., inserting the value of
other expressions within the string. Interpolation is based on an identifier
(referencing a local or global variable), along with an optional sequence of
operations.

Interpolation has two "modes":

- `"{...}"` performs the interpolation literally - strings in the input are
  pasted verbatim in the string.
- `"<...>"` performs native OS path resolution before pasting the input, always
  producing an absolute native OS path, either in the workspace or the output
  directory. See [Abstract Paths](paths.md). This is handy when passing
  arguments to an external process. Note that some values will already be native
  OS paths, such as the result of a `which` expression, so those should not be
  interpolated using `<...>`, as it would apply path resolution twice.

Interpolation operations (i.e., operations affecting how the input is pasted)
appear after `:` within the interpolation block.

### Interpolation stem

Any interpolation block `{...}` or `<...>` must contain a "stem", which appears
before any interpolation operation (i.e., before a join operation or other
operations after `:`).

The stem is either:

- An identifier referencing a local or global variable. Example, reading the
  variable `var`: `"{var}"`
- Empty, referencing the "implied" value in a chaining operation, or the matched
  string in a `match` expression. Example, copying the implicit value: `"{}"`.

**Note:** When the interpolation stem refers to a list, and there is no join
operator, the first element of the list is produced. If the list is empty, the
interpolation block produces an empty string.

### Join interpolation

When interpolating a list, the `*` operator can be used to expand the list
(recursively), separated by a space. A different separator may also be provided:
`{input,*}` produces a string where each element of `input` is separated by a
comma.

When interpolating a string value, the join interpolation directive has no
effect.

Join operations happen _after_ any other interpolation operation has been
applied (recursively), and after native OS path resolution in `<...>`
interpolation blocks.

Example:

```werk
let letters = ["a", "b", "c"]
let string = "{letters,*}"

# Prints "a,b,c"
info string
```

Example using native OS path resolution (in this case, on Windows in a workspace
located in "c:\\workspace"):

```werk
let files = ["a.txt", "b.txt"]
let string = "<files*>"

# Prints "c:\workspace\a.txt c:\workspace\b.txt"
info string
```

### Interpolation operations

Interpolation operations appear after `:` in an interpolation block. Multiple
operations may appear in a single interpolation block separated by a comma, and
they are applied in order.

- `{...:.ext1=.ext2}` replaces file extension `.ext1` with `.ext2` (the period
  is mandatory).
- `{...:s/regex/replacement/}` replaces occurrences matching `regex` with
  `replacement`. The regex is passed verbatim to the `regex` crate, and the
  replacement string follows the normal conventions.

### String interpolation example

```werk
let input-files = ["foo.c", "main.c"]

info "{input-files ,*:.c=.o}"    # Prints "foo.o, main.o"
info "<input-files*:.c=.o>"      # Prints "c:\workspace\output\foo.o c:\workspace\output\main.o"
```

## Command interpolation in `run` statements

Commands executed in recipes, or as part of the `shell` expression, have
additional logic in order to make it intuitive to build valid commands that can
be understood by the OS, loosely following normal shell conventions.

Commands in `run` statements are always string literals, and cannot be stored in
variables.

Strings in `run` statements are evaluated according to the following additional
rules:

- A command consists of segments separated by whitespace.
- The first segment is always the absolute path to an executable, such as
  obtained by the [`which` expression](#which-expression). If the first segment
  does not look like an absolute OS path (depending on the current platform),
  the `which` operation is still performed before executing the command.
- Any subsequent segment is treated as an argument to the executable, and will
  be passed as-is. That is to say, when passing paths to files within the
  workspace as arguments, they must be interpolated using native OS path
  resolution: `<var>`.
- When building a command using a string literal, the string is parsed with
  sensitivity to whitespace and double-quotes. Whitespace outside quotes is
  understood to mean a separator between arguments (and multiple whitespace
  characters are collapsed into a single separator). Quoted arguments are always
  passed literally as a single argument to the command.
- String interpolation _inside_ quotes is pasted according to normal string
  interpolation rules, and passed as a single argument to the command. Note that
  since commands are also string expressions, quotes must be escaped.
- String interpolation _outside_ quotes is treated as a single argument,
  _except_ if the string interpolation contains a [join
  operator](#join-interpolation) that would separate the strings by a single
  space character (the default), in which case each string is passed as a
  separate argument.
- String interpolations that evaluate to strings containing quotes do not affect
  argument separation - the quotation characters are passed verbatim to the
  command. This is also the only way to pass a literal quote character as an
  argument to the command.

Note that shell piping syntax is not available, since the command is _not_ run
via a shell process such as `sh` or `pwsh.exe`.

Examples:

```werk
let compiler = "clang"
let input = "foo.c"
let output = "foo.o"
let cflags = ["-c", "-O0", "-g"]
run "{compiler} {cflags*} -o <output> <input>"

# Command that will be run:
# "c:\path\to\clang.exe" "-c" "-O0" "-g" "-o" "c:\workspace\output\foo.o" "c:\workspace\foo.c"
```

Note that `which` is run implicitly to obtain the path to `clang`.
