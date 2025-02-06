# Example: Cargo + WASM + Assets

This example shows a complex use case, using parts of the other examples:

- Build a "main" binary for the host architecture using Cargo.
- Build a WASM plugin targeting `wasm32-wasip2` using Cargo.
- Compile shaders using `glslc`.
- Compress an `.tar.gz` "asset pack" containing compiled WASM modules, compiled
  shaders, and PNG images.

Due to the [outdatedness rules](../outdatedness.md) and depfile integration,
every rule accurately captures the actual dependencies of each step. For
example, changing a `.glsl` file included by one of the shaders will only cause
the relevant shaders to be rebuilt, and will cause `assets.tar.gz` to be
repackaged, but it will not cause WASM modules to be rebuilt. Similarly, due to
the glob patterns, adding a `.png` file to the project will cause
`assets.tar.gz` to be repackaged, but nothing else will be rebuilt.

Werkfile:

```werk
{{#include complex/Werkfile}}
```
