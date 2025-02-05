# String Interpolation

Strings literals behave "as expected", honoring normal character escape rules.

Additionally, string literals can contain interpolations, i.e., inserting the
value of other expressions within the string. Interpolation is based on an
identifier (referencing a local or global variable), along with an optional
sequence of operations.

Interpolation has two "modes":

- `"{...}"` performs the interpolation literally - strings in the input are
  pasted verbatim in the string.
- `"<...>"` performs [native OS path resolution](../paths.md) before pasting the
  input, always producing an absolute native OS path, either in the workspace or
  the output directory. This is handy when passing arguments to an external
  process. Note that some values will already be native OS paths, such as the
  result of a `which` expression, so those should not be interpolated using
  `<...>`, as it would apply path resolution twice.

Interpolation operations (i.e., operations affecting how the input is pasted)
appear after `:` within the interpolation block.

## Interpolation stem

Any interpolation block `{...}` or `<...>` consists of a "stem", and optionally
a number of interpolation operations.

The stem is either:

- An identifier referencing a local or global variable. Example, reading the
  variable `var`: `"{var}"`
- Empty, referencing the "implied" value in a chaining operation, or the matched
  string in a `match` expression. Example, copying the implicit value: `"{}"`.

**Note:** When the interpolation stem refers to a list, and there is no join
operator, the first non-empty string (recursively, depth-first) inside the list
is interpolated. If the list is empty, the interpolation block produces an empty
string.

Example:

```werk
let list-of-lists = [[["a"], "b"], "c"]
let string = "{list-of-lists}"            # "a"
```

## Join interpolation

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

## Interpolation operations

Interpolation operations appear after `:` in an interpolation block. Multiple
operations may appear in a single interpolation block separated by a comma, and
they are applied in order.

- `{...:.ext1=.ext2}` replaces file extension `.ext1` with `.ext2` (the period
  is mandatory).
- `{...:s/regex/replacement/}` replaces occurrences matching `regex` with
  `replacement`. The regex is passed verbatim to the `regex` crate, and the
  replacement string follows the normal conventions.
- `{...:dir}`: When the stem refers to an [abstract path](../paths.md), produces
  the directory part of the path.
- `{...:filename}`: When the stem refers to an [abstract path](../paths.md),
  produces the file-without-directory part of the path.
- `{...:ext}`: When the stem refers to an [abstract path](../paths.md), produces
  the file extension (without the `.`) of the path.
- `<...:out-dir>`: Disambiguate [native path resolution](./path_resolution.md)
  to produce a path in the output directory. Does nothing in `{...}`
  interpolations.
- `<...:workspace>`: Disambiguate [native path resolution](./path_resolution.md)
  to produce a path in the workspace directory. Does nothing in `{...}`
  interpolations.

## String interpolation example

```werk
let input-files = ["foo.c", "main.c"]

info "{input-files ,*:.c=.o}"    # Prints "foo.o, main.o"
info "<input-files*:.c=.o>"      # Prints "c:\workspace\output\foo.o c:\workspace\output\main.o"
```
