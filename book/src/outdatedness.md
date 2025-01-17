# Outdatedness

`werk` has a richer idea of "outdatedness" or "staleness" than Make and similar
tools, enabling many cases that would traditionally require a full rebuild to be
selectively rebuilt instead.

This is made possible by placing a [`.werk-cache`](./werk_cache.md) file in the
project's [output directory](./workspace.md#output-directory) that tracks
outdatedness information between builds.

`werk` tracks outdatedness in extremely high detail. Any variable or expression
used in a build recipe may contribute to its outdatedness. This enables the
`--explain` option to provide very detailed information about why a build recipe
was executed, and it causes `werk` to be very accurate when deciding what to
build.

Outdatedness is always transitive. If a build recipe is outdated, all of its
dependents are also outdated. If a variable relies on another variable that is
determined to have changed, that variable is also determined to have changed,
and recipes relying on it will be outdated.

The following factors contribute to outdatedness:

- **File modification timestamps:** If a build recipe depends on a file that has
  a newer modification timestamp than a previously built output file, the file
  is considered outdated.

- **Glob results:** If a `glob` expression produces a new result between runs
  (i.e., a file is deleted that previously matched the pattern, or a new file is
  added matching the pattern), any recipe relying on the results of that glob
  expression will be outdated.

- **Program paths:** If the path to a program's executable changes between runs
  (i.e., the result of a `which` expression changed), any recipe relying on the
  results of that expression will be outdated. Note: `werk` does not currently
  take file modification timestamps of found programs into account, so updating
  your tools may still require a manual rebuild.

- **Environment variables:** If the value of an environment variable changed
  between runs, any recipe relying on the value will be outdated.

- **Recipes:** If the recipe to build a file changes in a way that would cause
  the file to be built in a different way, the file is considered outdated.
  Insignificant changes that are ignored in this check are `info` and `warn`
  statements and comments.

- **Global variables:** If the definition of a global variable changes in the
  Werkfile, all recipes that use that specific variable will be outdated. For
  example, changing the string value of a global variable will cause recipes
  relying on that variable to become outdated.

- **Command-line overrides:** If a `-Dkey=value` option is passed to `werk` to
  override a global variable, and it was not overridden with the same value in a
  previous run, all recipes depending on that variable will be considered
  outdated.

This means that a build recipe that has no input files can still become
outdated, because its outdatedness is determined by these factors.

Note that task recipes are always "outdated" (just like `.PHONY` targets), so a
build recipe that depends on a task recipe will always be outdated.

## Note about globals and recipes

The outdatedness of global variables and recipes is determined by their
definition in the Werkfile. This check is performed against the hash of the AST
of those statements - not the source code representation. This means that
modifying comments in or around those statements will not affect outdatedness.

In general, only changes that can affect the commands that are actually run by a
recipe are included in the outdatedness check, so things like modifying the
informational message of an `info` statement will not cause its surrounding
recipe to become outdated.
