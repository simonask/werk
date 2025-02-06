# Example: GLSL shaders

This example shows how to build SPIR-V shaders using `glslc`. It also showcases
capture groups in pattern matching, where the same build rule is used for all
three types of shaders (fragment, vertex, compute).

Additionally, this also creates an "asset pack" containing all the shaders,
using `tar`.

Werkfile:

```werk
{{#include shaders/Werkfile}}
```
