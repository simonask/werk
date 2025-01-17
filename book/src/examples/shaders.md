# Example: GLSL shaders

This example shows how to build SPIR-V shaders using `glslc`. It also showcases
capture groups in pattern matching, where the same build rule is used for all
three types of shaders (fragment, vertex, compute).

Additionally, this also creates an "asset pack" containing all the shaders,
using `tar`.

Werkfile:

```werk
let glslc = which "glslc"
let tar = which "tar"

build "%.(frag|vert|comp).spv" {
    from "{%}.{0}"
    depfile "{%}.{0}.d"
    run "{glslc} -MD -MF <depfile> -o <out> <in>"
}

build "shaders.tar.gz" {
    # Note: Using "native" glob syntax.
    from glob "*.\{frag,vert,comp\}" | map "{}.spv"
    run "{tar} -zcf <out> <in*>"
}

task build-shaders {
    build "shaders.tar.gz"
}
```
