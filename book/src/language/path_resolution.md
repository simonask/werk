# Path resolution

Werk supports translating [abstract paths](../paths.md) into native OS paths in
string interpolations, using the special `"<...>"` interpolation syntax.

Normal string interpolations `"{...}"` are always "verbatim" - the interpolation
is performed literally.

However, string interpolation with `<...>` performs extra logic to obtain a
native OS path whenever it occurs, and this logic is sensitive to the
surroundings of the interpolation, as well as the presence of build recipe
rules.

**Pathiness:** A string containing a `<...>` interpolation (i.e., containing a
native OS path) cannot be used in another `<...>` interpolation, as this would
create nonsensical OS paths. This is transitive, so a native OS path cannot be
"smuggled" through a normal `{...}` interpolation. However, certain operations
remove the "pathiness".

Consider the following Werkfile:

```werk
# c:\workspace
#    target\
#    dir\
#    foo.txt

default out-dir = "target"

let input = "foo.txt"
let output = "bar.txt"
let dir = "dir"

let input-path = "<input>"    # c:\workspace\foo.txt
let output-path = "<output>"  # c:\workspace\target\bar.txt
let output-filename = "{output-path:filename}"   # foo.txt

let output-path = "<output-path>"   # ERROR: Double path resolution
```

- `"<input>"` resolves to `c:\workspace\foo.txt`, because `foo.txt` exists in
  the workspace.
- `"<output>"` resolves to `c:\workspace\target\bar.txt`, because `bar.txt`
  does not exist in the workspace.
- `"<input:out-dir>"` resolves to `c:\workspace\target\foo.txt`, because it is
  explicitly requested.
- `"<output:workspace>"` resolves to `c:\workspace\bar.txt`, because it is
  explicitly requested, even though the file does not exist in the workspace.
- `"<dir>"` resolves to `c:\workspace\dir`, even though it is a directory.
- When an `<...>` interpolation would match a file in the workspace, but also
  matches a build recipe, `werk` fails with an error describing the ambiguity.
  The path can be disambiguated by using `:out-dir` or `:workspace` to
  disambiguate path resolution.
- Since they contain `<...>` interpolations, `input-path` and `output-path` are
  marked as "pathy", and those variables cannot be used in further `<...>`
  interpolations.
- However, the filename component of a path is _not_ "pathy", so
  `output-filename` may be used in other `<...>` interpolations.
