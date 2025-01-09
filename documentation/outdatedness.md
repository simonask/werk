# Outdatedness

`werk` has a richer idea of "outdatedness" or "staleness" than Make and similar
tools, enabling many cases that would traditionally require a full rebuild to be
selectively rebuilt instead.

This is made possible by placing a `.werk-cache` file in the project's output
directory that tracks outdatedness information between builds.

`werk` tracks outdatedness in extremely high detail. Any variable used in a
build recipe may contribute to its outdatedness. For example, if a global
variable depends on some expression that participates in outdatedness (like
reading an environment variable), any expression or build recipe that uses the
variable (transitively) will also contribute to outdatedness.

Outdatedness is always transitive - if a dependency is outdated, all of its
dependents are also outdated and will be rebuilt.

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

All values stored in `.werk-cache` are hashed to avoid leaking environment
secrets.
