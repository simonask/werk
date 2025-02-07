# Expressions

Expressions occur in a number of places, most often the right-hand side of a
`let ... =` statement.

Every expression starts with a value:

- [String literal](./strings.md)
- List literal
- Any built-in expression that [queries the system](#querying-the-system),
  [workspace](../workspace.md) or runtime environment, like [`which`](#which) or
  [`env`](#env).
- Identifier, referencing a previously defined variable
- Any expression chain, surrounded by parentheses `( ... )`.

There are no functions or loops in Werk, but values can be transformed or
inspected by "piping" them through an [expression
chain](#expression-chaining--piping), imitating shell piping. Any value can be
piped through of the [built-in operators](#built-in-operators) to transfrom the
value in some way.

Abstract syntax:

```plain
atomic-expression = string-expr
                  | list-expr
                  | builtin-expr
                  | identifier
                  | '(' expression ')'
                  ;

expression = atomic-expression ('|' builtin-operator)*;

builtin-expr = 'which' string-expr
             | 'env' string-expr
             | ...
             ;

builtin-operator = 'match' match-body
                 | 'join' string-expr
                 | ...
                 ;

```

## Querying the system

The operators obtain a value from the system or runtime environment of the
`werk` process, and produce a value directly. They may initiate an expression
chain.

## `which`

Determine the native OS path of an executable, by way of the `PATH` environment
variable. If the program cannot be found, this expression causes an error to be
reported, and `werk` aborts.

The result of this expression participates in [outdatedness
checks](../outdatedness.md).

Syntax:

```werk
which <string-expr>
```

Example:

```werk
let cc = which "clang"     # e.g. "C:\Program Files\LLVM\bin\clang.EXE"
```

## `env`

Read environment variable. If the variable is not set, this evaluates to the
empty string.

The result of this expression participates in [outdatedness
checks](../outdatedness.md).

Syntax:

```werk
env <string-expr>
```

Example:

```werk
let rust-log = env "RUST_LOG"    # e.g. "trace"
```

## `glob`

Glob [workspace](../workspace.md) files. Given a standard globbing pattern
(e.g., `**/*.txt`), produces a list of all files in the workspace that match the
pattern. All names in the result are "absolute workspace paths", i.e. they start
with `/` and are relative to the workspace root.

This expression takes `.gitignore` into account, and will never return a path to
a file covered by `.gitignore`.

Note that standard glob pattern syntax collides with [string interpolation
syntax](./strings.md#string-interpolation), so capture groups must be escaped:
`*.\{frag,vert,comp\}`.

The result of this expression participates in [outdatedness
checks](../outdatedness.md).

Syntax:

```werk
glob <string-expr>
```

Example:

```werk
let source-files = glob "src/**/*.c"      # ["src/foo.c", "src/bar.c"]
```

## `shell`

Run a program during evaluation, producing its standard output. If the command
fails, evaluation fails and `werk` will abort. If the command produces output
that is not valid UTF-8, this expression also fails.

**Caution:** In the global scope, a `shell` expression will cause a program to
be run even in `--dry-run` mode. The intent is that this type of expression can
be used to query information about the project or system in a non-destructive
manner, such as obtaining the git `HEAD` or similar.

**Note:** Even though the expression is named "shell", the command is _not_
passed through the user's shell (like `sh`, `bash`, or PowerShell).

The result of this expression participates in [outdatedness
checks](../outdatedness.md).

Syntax:

```werk
shell <run-expr>
```

Example:

```werk
let sha1 = shell "git rev-parse --short HEAD"    # e.g., "0be81c6"
```

## `read`

Read a workspace file during evaluation, producing its contents as a string. If
the file contains invalid UTF-8, this expression fails, and `werk` will abort.

This will only read files in the workspace - never the output directory.

The result of this expression participates in [outdatedness
checks](../outdatedness.md).

Syntax:

```werk
read <string-expr>
```

Example:

```werk
let contents = read "my-file.txt"    # contents of "my-file.txt"
```

# Expression chaining / piping

There are no functions in werk, but expressions can be followed by a chain of
operators, transforming the value in some way. For example, an expression that
evaluates to a list can be passed to the [`join ", "`](#join) operator to
produce a string, where each element of the list is separated by a comma.

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

## Built-in operators

### `match`

Perform string [pattern-matching](patterns.md) and substitution. The "input" to
the match operation is whatever value is piped into it, which is to say, `match`
can only meaningfully appear as the right-hand side of a chaining expression.

If the "input" value is a list, the pattern substitution is performed
recursively. If no patterns match the input, the string is passed through
unchanged. The catch-all pattern `"%"` can be used to provide a fallback,
potentially using an [`error`](#error) operation to fail early.

Syntax:

```werk
match {
    <pattern> => <expression>
    <pattern> => <expression>
}
```

Example:

```werk
let source-file = "foo.c"
let object-file = source-file | match {
  "%.c" => "{%}.o"
  "%.cpp" => "{%}.o"
  "%" => "unsupported source file extension: {}"
}
```

### `join`

Given a list of values, convert the list to a string (recursively), where each
element is separated by a separator.

When given a string, returns the string unmodified.

The "input" to the operation is whatever value it receives via expression
piping.

Syntax:

```werk
join <separator>
```

Example:

```werk
let cflags = ["-O0", "-g"]
let arguments = cflags | join " "   # "-O0 -g"
```

### `split`

Given a string, convert it to a list by splitting it by some separator. The
separator is a pattern expression. If the separator is not present in the
string, returns a list with one entry containing the original string.

See also [`lines`](#lines).

Example:

```werk
let split = "Hello World" | split " "    # ["Hello", "World]
```

### `lines`

Given a string, convert it to a list by splitting it into separate lines.

This is similar to `split "\n"`, except it also handles CRLF line breaks.

Example:

```werk
let split = "a\r\nb\nc" | lines    # ["a", "b", "c"]
```

### `flatten`

Given a list containing other lists, return a flat list containing all strings
of the left-hand-side.

When given a string, returns a list with a single element containing that string
(equivalent to `[string]`).

May only appear on the right-hand-side of a piping expression.

Example:

```werk
let flattened = ["a", ["b", ["c"]]]   # ["a", "b", "c"]
```

### `filter`

Given a list, filter elements (recursively) through a pattern, keeping only the
elements that matched the pattern. This also flattens the list if it contains
other lists.

Always produces a list.

Example:

```werk
let filtered = ["a.c", "b.cpp"] | filter "%.cpp"   # ["b.cpp"]
```

### `filter-match`

Given a list, filter elements (recursively) through a pattern, and replace each
match with the right-hand side of the pattern match, keeping only the elements
that matched the pattern. This also flattens the list if it contains other
lists.

Always produces a list. When given a string, the string is filtered as if it was
the element of a list.

This is a combination of the [`filter`](#filter) and [`map`](#map) expressions,
or the [`match`](#match). Compared to `filter | map`, the difference
is that the mapping operation has access to pattern-match stem, capture groups,
etc., and not just the string that matched. Compared to `filter | match` or
`match | filter`, the difference is that the filter condition is that the
pattern failed to match.

Example:

```werk
let mapped = ["a.c", "b.cpp"]
           | filter-match "%.c" => "{%}.o"  # ["a.o"]
```

### `discard`

Inverse of [`filter`](#filter): Retains only elements that do _not_ match the
pattern(s).

Always produces a list.

Example:

```werk
let filtered = ["a.c", "b.cpp"] | detain "%.cpp"   # ["a.c"]
```

### `dedup`

Deduplicate strings in a list (recursively), preserving the original order. This
implies `flatten`.

When given a single string, returns the string unmodified.

Example:

```werk
let deduplicated = ["a", ["a"], "b", "a"] | dedup    # ["a", "b"]
```

### `map`

Given a list expression, pass each element through a string expression where the
"implied" value is the entry in the original list. Produces a list.

Given a string, evaluate the right-hand string expression once with the string
as the implied value. Produces a string.

Example:

```werk
let mapped = ["a", "b"] | map "hello {}"    # ["hello a", "hello b"]
let mapped = "a" | map "hello {}"           # "hello a"
```

### `info`

Print an informational message during evaluation. This can appear within an
expression chain to introspect values along the way.

Syntax:

```werk
info <string-expression>
```

### `warn`

Print a warning. This can appear within an expression chain.

Syntax:

```werk
warn <string-expression>
```

### `error`

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

### `assert-eq` expression

When this appear as part of an expression chain, fail evaluation if the two
sides are not equal.

Returns the value unchanged.

Syntax:

```werk
... | assert-eq <expr>
```

Example:

```werk
let input = ["a", "b"]
let result = input | map "{}.c" | assert-eq ["a.c", "b.c"]
```

### `assert-match` expression

When this appear as part of an expression chain, fail evaluation if the left
side does not match the pattern.

When the left hand side is a list, the pattern is evaluated for all strings in
the list (recursively).

Returns the value unchanged.

Syntax:

```werk
... | assert-match <pattern>
```

Example:

```werk
let input = ["a.c", "b.c"]
let result = input | assert-match "%.c"
```
