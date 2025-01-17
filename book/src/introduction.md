# Introduction

<div class="warning">
Werk is early alpha software. Use at your own risk. It may eat your
files, so run <code>git commit</code> before trying it out.
</div>

Werk is a command runner and build system. It is intended to replace Make in
projects that need a simple build tool adjacent to a main build system, such as
Cargo or NPM. It can also replace `just` in most cases.

The motivating use case is an asset building pipeline for a video game, which
must perform a series of expensive steps to produce an asset archive that can be
hot-reloaded by a game engine, but it can build anything, including C/C++
binaries, or integrate with external build systems, like Cargo.

Werk is [limited and opinionated](./features.md#limitations). It is not suited
for all use cases, and it can not replace more advanced solutions, like CMake or
scons. However, it _is_ suited to work together with such systems, and can be
used to invoke them in a convenient way. See the [Depfile
support](./depfile_support.md) chapter for more details.

`werk` tries to be really clever about when to rebuild files. In addition to
file modification times, it also takes things like the path to any commands
invoked in a recipe, any environment variables used in the recipe, or changes in
the results of glob patterns (like `*.txt`) into account when deciding whether
or not to rebuild a given file. See the [Outdatedness](./outdatedness.md)
chapter for more details.

`werk` also tries to be extremely helpful when diagnosing problems. The
command-line option `--explain` provides detailed information about why a given
target was rebuilt, without _excessive_ information. The command-line option
`--dry-run` allows evaluating the dependency graph without executing any
commands.

`werk` is religiously portable. It works _natively_ on all major platforms
(Linux, Windows, macOS), without any external dependencies - no `sh` required!

## Use cases

Examples of suitable use cases:

- Simple build processes for things like shaders, WASM modules, small C
  libraries, assets, etc.
- Command runner for "housekeeping" tasks, like running tests, publishing
  binaries, or downloading static file dependencies.
- Driving other build systems.

Examples of less suitable use cases:

- Building cross-platform C/C++ projects with system dependencies. There is no
  built-in way to discover "traditional" dependencies via `pkg-config`, `vcpkg`,
  or similar. Use CMake instead.
- Builds requiring detailed concurrency management. Werk assumes that all
  recipes that don't have an edge between them in the dependency graph can be
  run in parallel, and there is no way to limit parallelism outside of the
  `--jobs` parameter.
- Multiple outputs per recipe. Driving things like `bison` with Werk may require
  workarounds.
- Recursive workspaces.
