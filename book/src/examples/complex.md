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
let cargo = which "cargo"
let wasm-tuple = "wasm32-wasip2"

let profile = "debug"
let wasm-profile = "profile"

let cargo-profile = profile | match {
    "debug" => "dev"
    "%" => "{%}"
}

let cargo-wasm-profile = wasm-profile | match {
    "debug" => "dev"
    "%" => "{%}"
}

# Rule to build a WASM target with Cargo.
build "{wasm-tuple}/{wasm-profile}/%.wasm" {
    # Cargo uses dashes in package names and underscores in build artifacts, so
    # use a regex to replace it.
    let package-name = "{%:s/_/-/}"

    depfile "{%}.d"
    run "{cargo} build
        --target={wasm-tuple}
        --profile={cargo-wasm-profile}
        -p {package-name}"
}

# Rule to build a SPIR-V shader with glslc.
build "%.(frag|vert|comp).spv" {
    from "{%}.{0}"
    depfile "{%}.{0}.d"
    run "{glslc} -MD -MF <depfile> -o <out> <in>"
}

let wasm-targets = ["plugin1", "plugin2"]
                   | map "{wasm-tuple}/{wasm-profile}/{}.wasm"

build "assets.tar.gz" {
    from [
        wasm-targets,
        glob "assets/**/*.png",
        glob "shaders/**/*.\{frag,vert,comp\}" | map "{}.spv"
    ]

    run "tar -zcf <out> <in*>"
}

# Rule to build the main program.
build "{profile}/program{EXE_SUFFIX}" {
    depfile "{profile}/program.d"
    run "cargo -p program --profile={cargo-profile}"
}

# Task to build everything.
task build {
    build ["{profile}/program{EXE_SUFFIX}", "assets.tar.gz"]
}

# Task that just runs `cargo --clean`. This deletes `target/`, so also removes
# compiled shaders.
task clean {
    run "{cargo} --clean"
}
```
