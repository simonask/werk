# Patterns and pattern-matching

Patterns are strings containing special directives. They behave similarly to
Make patterns.

Special syntax in pattern strings:

- `%`: The "pattern stem". This matches any sequence of characters, which will
  be available to subsequent statements as `{%}`.
- `(a|b)`: Capture group matching either `a` or `b`.

Patterns can contain [string interpolations](./strings.md#string-interpolation).
Interpolated string values are not interpreted as patterns, but will be matched
literally. For example, if an interpolated value contains `%`, it will only
match the string "%".

Example, given the pattern `%.(c|cpp)`:

- The string `"foo.c"` will match. The stem is `foo`, and capture group 0 is `c`.
- The string `"foo/bar/baz.cpp"` will match. The stem is `foo/bar/baz`, and
  capture group 0 is `cpp`.
- The string `"foo.h"` will not match, because none of the variants in the
  capture group apply.
- The string `"abc"` will not match, because the period is missing.

When multiple patterns are participating in pattern matching (such as figuring
out which [build recipe](language.md#build-statement-at-global-scope) to run, or
in a [`match` expression](language.md#match-expression)), the "highest-quality"
match is chosen. Match quality is measured by the length of the stem: A match
producing a shorter stem is considered "better" than a match producing a longer
stem.

- A pattern without a `%` stem is "more specific" than a pattern that has a
  stem.
- A pattern that matches the input with a shorter stem is "more specific" than
  a pattern that matches a longer stem.
- Capture groups do not affect the "specificity" of a pattern.

**Important:** When multiple patterns match with equal quality, the pattern
matching operation is ambiguous. In build recipes, this is a hard error. In
[`match` expressions](./operations.md#match), the first matching pattern will
win. Often, capture groups can be used to disambiguate two patterns by
collapsing them into a single pattern.

## Example

Given the patterns `%.c`, `%/a.c`, `foo/%/a.c`, `foo/bar/a.c`, this is how
matches will be chosen based on various inputs:

- `"bar/b.c"`: The pattern `%.c` will be chosen, because it does not match the
  other patterns. The stem is `"bar/b"`.
- `"foo/a.c"`: The pattern `%/a.c` will be chosen, because it produces the
  shortest stem `"a"`. The stem is `foo`.
- `"foo/foo/a.c"`: The pattern `foo/%/a.c` will be chosen over `%.c` and
  `%/a.c`, because it produces a shorter stem. The stem is `foo`.
- `"foo/bar/a.c"`: The pattern `foo/bar/a.c` will be chosen over `foo/%/a.c`,
  because the pattern is literal exact match without a stem.

**Conflicts:** It's possible to construct patterns that are different but match
the same input with the same "specificity". For example, both patterns
`foo/%/a.c` and `%/foo/a.c` match the input `"foo/foo/a.c"` equally. When such a
situation occurs, that's a hard error.
