//PULL REQUEST LEGEND: 
All changes have an explanatory "//TODO:" Comment right above the changed text, after which follows an "EDIT:" with the suggested change and then a following "PREVIOUS:" with the original for ease of comparison. 
For situations where there is ambiguity where your developer input is needed to clarify the "TODO:" is instead a "TODO-DEV-INPUT:".
//TODO: Overall in the entire document every example comment that simply clarified the output of a piping sequence has been changed "<output-variable> Output = <piping-output>. Outside debatable example clarification I think this has the bonus upside of making the reader continiously more comfortable with the piping operations and syntax by softly reinforcing the order of operations and i/o for piping operations.  
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
- Strings and patterns are always double-quoted `"`. 
//TODO: No explicit mention of string interpolation in basic syntax, while the section appears at the end of document. Several examples become unclear until the reader has a solid understanding of the interpolation rules. My addition wording is probably awful and should be clarified. Also Consider moving the string interpolation section to be the next one after this one. 
ADDITION: - String sections beginning with the `<`(native OS Path resolution) or `{`(literal resolution) are string interpolations with different rules.
This can be used to apply regex expressions to string literals. See the following "String interpolation" section of this document.
- Lists are surrounded by `[ ... ]`, and elements are comma-separated.
// TODO: Amiguity about whether piped expressions resolve from left to right or right to left and more importantly the fact that lhand result is provided as input to rhand is not mentioned here is confusing. 
EDIT: - There are no functions or loops, but expressions can be "chained" or "piped"
  using the `|` operator. Chained expressions resolve from left to right.
  The "output" from the lefthand expression is provided as "input" to the right hand expression.
PREVIOUS: - There are no functions or loops, but expressions can be "chained" or "piped"
  using the `|` operator.
- All variables are immutable - there is no assignment operation.
- Local variables may shadow global variables or previously defined local
  variables in the same scope.

//TODO: The list of operations has been moved to be the final part of the document, with everything else(Built-in variables and constants, String interpolation syntax and Command interpolation in `run` statements) moved to follow directly after the basic syntax. This is in my personal opinion more intuitive and teaches the language better plus seems to generally be the formating standard for reference documentation. 
## Built-in variables and constants

See [built-in variables](builtins.md).

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

//TODO: Since the interpolation stem subsection references the : interpolation operator in passing moving this section to be before it makes more intuitive sense. 
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

### Interpolation stem

Any interpolation block `{...}` or `<...>` must contain a "stem", which appears
before any interpolation operation (i.e., before a join operation or other
operations after `:`).

The stem is either:

- An identifier referencing a local or global variable. Example, reading the
  variable `var`: `"{var}"`
- Empty, referencing the "implied" value in a chaining operation, or the matched
  string in a `match` expression. Example, copying the implicit value: `"{}"`.

//TODO-DEV-INPUT: What does "produced" mean in this context? Is it the final output produced(unchanged) from the interpolation or is it pasted verbatim into the string(and as such has regex mutations applied to it etc.?)? The part referencing the abscence of a join operator would lead me to expect the latter, but additional clarity is needed. 
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
//TODO:Adding additional lines to the example clarifies the syntax and behaviour for separators greatly, in my opinion.
```werk
EDIT:
let letters = ["a", "b", "c"]
let string = "{letters*}"
let stringComma = "{letters,*}"

# Prints "a b c"
info string
# Prints "a,b,c"
info stringComma
```

```werk
PREVIOUS:
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

## Language Operations

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
//TODO: Added explicit references to piped expression output in syntax example. 
Syntax:

```werk
EDIT: <first> => <output-first> | <output-first> => <then> => <output=then> | <output=then> => <last> 
```
```werk
PREVIOUS: <first> | <then> | <last> 
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

Perform string [pattern-matching](patterns.md) and substitution. The "input" to
the match operation is whatever value is piped into it, which is to say, `match`
can only meaningfully appear as the right-hand side of a chaining expression.

If the "input" value is a list, the pattern substitution is performed
recursively. If no patterns match the input, the string is passed through
unchanged. The catch-all pattern `"%"` can be used to provide a fallback,
potentially using an [`error`](#error-expression) operation to fail early.

Syntax:
//TODO: This might be a formating break from language reference standard, but I desire extra clarity in the example here. 
The fallback line in particular benefits from such in my opinion. 

```werk
EDIT:
let errorCode = "Fallback Error"
let input = [...,...] | match {
    <pattern> => <expression>
    <pattern> => <expression>
    "%" => error errorCode #Fallback + Error 
}
```
```werk
PREVIOUS:
match {
    <pattern> => <expression>
    <pattern> => <expression>
}
```

### `join` expression

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
let arguments = cflags | join " "   # join Output = "-O0 -g"
```

### `split` expression

Given a string, convert it to a list by splitting it by some separator. The
separator is a pattern expression. If the separator is not present in the
string, returns a list with one entry containing the original string.

See also [`lines`](#lines-expression).

Example:

```werk
let split = "Hello World" | split " "    # split Output = ["Hello", "World]
```

### `lines` expression

Given a string, convert it to a list by splitting it into separate lines.

This is similar to `split "\n"`, except it also handles CRLF line breaks.

### `flatten` expression

Given a list containing other lists, return a flat list containing all strings
of the left-hand-side.

When given a string, returns a list with a single element containing that string
(equivalent to `[string]`).

May only appear on the right-hand-side of a piping expression.

Example:
//TODO: Clarify example with full piping expression since other use is explicity forbidden
```werk
EDIT:
let flattened = ["a", ["b", ["c"]]] | flatten  # flattened Output = ["a", "b", "c"]
let flattenedSingleString = "a" | flatten  # flattenedSingleString Output = ["a"]
```
```werk
PREVIOUS:
let flattened = ["a", ["b", ["c"]]]   # ["a", "b", "c"]
```
### `filter` expression

Given a list, filter elements (recursively) through a pattern, keeping only the
elements that matched the pattern. This also flattens the list if it contains
other lists.

Always produces a list.

Example:

```werk
let filtered = ["a.c", "b.cpp"] | filter "%.cpp"   # filtered Output = ["b.cpp"]
```

### `filter-match` expression

Given a list, filter elements (recursively) through a pattern, and replace each
match with the right-hand side of the pattern match, keeping only the elements
that matched the pattern. This also flattens the list if it contains other
lists.

Always produces a list. When given a string, the string is filtered as if it was
the element of a list.

This is a combination of the [`filter`](#filter-expression) and
[`map`](#map-expression) expressions, or the [`match`](#match-expression).
Compared to `filter | map`, the difference is that since both operations happen in the same scope the mapping operation has
access to pattern-match stem, capture groups, etc., and not just the list of strings that
matched. Compared to `filter | match` or `match | filter`, the difference is
that the filter condition is that the pattern failed to match.

Example:

```werk
let mapped = ["a.c", "b.cpp"] | filter-match "%.c" => "{%}.o"  # mapped Output = ["a.o"]
```

### `discard` expression

Inverse of [`filter`](#filter-expression): Retains only elements that do _not_
match the pattern(s).

Always produces a list.

Example:
TODO: Wrong. Detain where there should be discard.
```werk
EDIT:
let filtered = ["a.c", "b.cpp"] | discard "%.cpp"   # filtered Output = ["a.c"]
```
```werk
PREVIOUS:
let filtered = ["a.c", "b.cpp"] | detain "%.cpp"   # ["a.c"]
```
### `map` expression

Given a list expression, pass each element through a string expression where the
"implied" value is the entry in the original list. Produces a list.

Given a string, evaluate the right-hand string expression once with the string
as the implied value. Produces a string.

Example:
TODO: Expressions and operations in the same codeblock are implied to exist in the same code context, consider using different variable names since a variable being defined and then immediatly shadowed on the next line is a meaningless operation and therefor confusing.
```werk
EDIT: 
let mapped = ["a", "b"] | map "hello {}"    # mapped Output = ["hello a", "hello b"]
let mappedString = "a" | map "hello {}"           # mappedString Output = "hello a"
```
```werk
PREVIOUS: 
let mapped = ["a", "b"] | map "hello {}"    # ["hello a", "hello b"]
let mapped = "a" | map "hello {}"           # "hello a"
```
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

//TODO: The expression fails with what result? Hard error? Soft error? console log? Clarify.
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
//TODO-DEV-INPUT: is this an append write(EDIT: assumes that it is)? 
EDIT: Write(append) a string to a file.
PREVIOUS: Write a string to a file.

This can only be used in `run` statements in recipes.

Syntax:

```werk
write <expression> to <filename>
```

Example:

```werk
build "message.txt" {
    let message = "Hello, World!"
    run {
        write message to "<out>"
    }
}
```

### `copy` operation
TODO-DEV-INPUT: Needs clarification on output file behaviour. Does the target file need to exist or will it be created if none exists, what is the behaviour if the target file does exist(assumed to be append). Clarify.
Copy a file to another.

This can only be used in `run` statements in recipes.

Syntax:

```werk
copy <source-path> to <destination-path>
```

Example:

```werk
build "b.txt" {
  from "a.txt"
  run {
    copy "a.txt" to "b.txt"
  }
}
```

### `delete` operation
//TODO-DEV-INPUT: Needs clarification on two things. 0. What is operation behaviour when target file does not exist or delete fails, & 1. an explicit statement about whether the is the delete operation limited to the output directory or can perform deletes on the entire working directory. 
Delete a file or list of files.

This can only be used in `run` statements in recipes.

Syntax:

```werk
delete <paths>
```

Example:

```werk
task clean-objects {
  let files = glob "*.c" | map "{}.o"
  run {
    delete files
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

Unconditionally fail evaluation that also prints a provided error message. If this is reached during evaluation in the
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
//TODO-DEV-INPUT: What does fail mean in this context? Error(is what I would assume based on the operation name)? Clarify. 
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
//TODO-DEV-INPUT: What does fail mean in this context? Error(is what I would assume based on the operation name)? Clarify. 
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

