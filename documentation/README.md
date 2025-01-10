# Werk Documentation

- [Features](features.md)
- [Language Reference](language.md)
- [Abstract Paths](paths.md)
- [Outdatedness / staleness](outdatedness.md)
- [Depfile support](depfiles.md)
- [Built-in variables](builtins.md)
- [Examples](examples.md)

## Introduction

Werk is a command runner and simplistic build system. It is intended to supplant
Make in projects that need a simple build tool adjacent to a main build system,
such as Cargo or NPM.

The motivating use case is an asset building pipeline for a video game, which
must perform a series of expensive steps to produce an asset archive that can be
hot-reloaded by a game engine.

Werk is both limited and very opinionated. It is not suited for all use cases,
and it can only replace more advanced solutions, like CMake or scons, in a
limited number of scenarios. However, it _is_ suited to work together with such
systems, and can be used to invoke them in a convenient way.

`werk` tries to be really clever about when to rebuild files, and is
knowledgeable about file modification times, but also things like the path to
any commands invoked in a recipe, any environment variables used in the recipe,
or changes in the results of glob patterns (like `*.txt`).

Examples of suitable use cases:

- Simple build processes for things like shaders, WASM modules, small C
  libraries, assets, etc.
- Command runner for "housekeeping" tasks, like running tests, publishing
  binaries, or downloading static file dependencies.
- Driving other build systems.

Examples of less suitable use cases, for now:

- Building complicated C++ libraries with many dependencies.
- Building complicated projects using advanced features like cross-compilation.
- Builds requiring detailed concurrency management. Werk potentially runs all
  independent recipes in parallel, and there is no way to limit parallelism
  outside of the `--jobs` parameter.
- Multiple outputs per recipe. Driving `bison` with Werk may require
  workarounds.
- Recursive workspaces.
